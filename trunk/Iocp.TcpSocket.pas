unit Iocp.TcpSocket;

{关于客户端Socket池：
客户端使用ConnectEx创建的Socket连接，只有在服务端主动断开的情况，这个Socket才能被重用；
如果是由客户端发起的DisconnectEx断开，这个Socket不能被重用，所以客户端要主动断开连接的话
就直接用shutdown和closesocket就可以了。

在ConnectEx的时候，先从Socket池中查找已经由服务器断开的空闲Socket，如果有就重用，没有就新建

--这就是客户端的Socket重用机制

经过进一步测试发现，客户端的Socket重用相当不稳定，所以还是直接关闭Socket比较可靠

ZY. 2012.01.13

直接放弃Socket重用，因为会造成一些不稳定因素，对性能的提升又不是很大

ZY. 2012.04.19
}

// 如果开启这个开关，将禁用TCP/IP的Nagle算法
// 也就是不管发送的数据块是否能填满底层的缓冲，都直接发出去
// 这会一定程度降低发送效率，但是能提高响应速度
//{$define __TCP_NODELAY__}

// ** 下面两个0拷贝参数不要打开，经过实际测试发现打开后反而速度会严重下降
// ** 看来底层的缓存机制还是很高效的

// 发送缓存0拷贝，发送数据时直接使用程序设定的缓存，不用拷贝到Socket底层缓存
//{$define __TCP_SNDBUF_ZERO_COPY__}

// 接收缓存0拷贝，接收数据时直接使用程序设定的缓存，不用从Socket底层缓存拷贝
//{$define __TCP_RCVBUF_ZERO_COPY__}

// 启用超时检测时钟
{$define __TIME_OUT_TIMER__}

interface

uses
  Windows, Messages, Classes, SysUtils, SyncObjs, Math, Contnrs, System.Generics.Collections,
  Iocp.Winsock2, Iocp.Wship6, Iocp.ApiFix, Iocp.ThreadPool, Iocp.ReadWriteLocker, Iocp.MemoryPool,
  Iocp.ObjectPool, Iocp.Buffer, Iocp.Queue, Iocp.TimerQueue, Iocp.Logger, Iocp.Utils;

const
  SHUTDOWN_FLAG = ULONG_PTR(-1);

  MAX_FREE_HANDLE_DATA_BLOCKS = 512;
  MAX_FREE_IO_DATA_BLOCKS = MAX_FREE_HANDLE_DATA_BLOCKS * 2;
  INIT_ACCEPTEX_NUM = 1;
  NET_CACHE_SIZE = 4 * 1024; // 不要大于4K !!!!!
  FILE_CACHE_SIZE = 64 * 1024;

type
  TAddrUnion = record
    case Integer of
      0: (IPv4: TSockAddrIn);
      1: (IPv6: TSockAddrIn6);
  end;

  TAddrBuffer = record
    Addr: TAddrUnion;
    Extra: array [0..15] of Byte;
  end;

  TAcceptExBuffer = array[0..SizeOf(TAddrBuffer) * 2 - 1] of Byte;

  TPerIoBufUnion = record
    case Integer of
      0: (DataBuf: WSABUF);
      // 这个Buffer只用于AcceptEx保存终端地址数据，大小为2倍地址结构
      1: (AcceptExBuffer: TAcceptExBuffer);
  end;

  TIocpOperationType = (iotReadZero, iotRead, iotWrite, iotAccept, iotConnect);

  {
    *** 单IO数据结构
    每次IO操作都需要生成一个该结构传递给IOCP
  }
  PIocpPerIoData = ^TIocpPerIoData;
  TIocpPerIoData = record
    Overlapped: TWSAOverlapped;
    Buffer: TPerIoBufUnion;
    Operation: TIocpOperationType;
    ListenSocket, ClientSocket: TSocket;

    BytesTransfered: Cardinal;
  end;

  EIocpTcpException = class(Exception);

  TIocpTcpSocket = class;
  TIocpSocketConnection = class;

  {
     *** Socket 连接 ***
  }
  TIocpSocketConnection = class(TIocpObject)
  private
    FSocket: TSocket;
    FRemoteIP: string;
    FRemotePort: Word;

    FRefCount: Integer;
    FDisconnected: Integer;
    FFirstTick, FLastTick: DWORD;
    FTag: Pointer;
    FSndBufSize, FRcvBufSize: Integer;
    FRcvBuffer: Pointer;
    FPendingSend: Integer;
    FPendingRecv: Integer;
    FIsIPv6: Boolean;
    {$IFDEF __TIME_OUT_TIMER__}
    FTimer: TIocpTimerQueueTimer;
    FTimeout: DWORD;
    FLife: DWORD;
    // 连接超时检查
    procedure OnTimerCreate(Sender: TObject);
    procedure OnTimerExecute(Sender: TObject);
    procedure OnTimerDestroy(Sender: TObject);
    {$ENDIF}

    function GetRefCount: Integer;
    function GetIsClosed: Boolean;
    function GetOwner: TIocpTcpSocket;

    function InitSocket: Boolean;
    procedure UpdateTick;

    procedure IncPendingRecv;
    procedure DecPendingRecv;
    function PostReadZero: Boolean;
    function PostRead: Boolean;

    procedure IncPendingSend;
    procedure DecPendingSend;
    function PostWrite(const Buf: Pointer; Size: Integer): Boolean;

    procedure _TriggerClientRecvData(Buf: Pointer; Len: Integer);
    procedure _TriggerClientSentData(Buf: Pointer; Len: Integer);
    function _Send(Buf: Pointer; Size: Integer): Integer;
  protected
    procedure Initialize; override;
    procedure Finalize; override;

    {$IFDEF __TIME_OUT_TIMER__}
    procedure TriggerTimeout; virtual;
    procedure TriggerLifeout; virtual;
    {$ENDIF}

    function GetIsIdle: Boolean; virtual;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function AddRef: Integer;
    function Release: Boolean;
    procedure Disconnect;

    // 纯异步发送
    function Send(Buf: Pointer; Size: Integer): Integer; overload; virtual;
    function Send(const Buf; Size: Integer): Integer; overload;
    function Send(const Bytes: TBytes): Integer; overload;
    function Send(const s: RawByteString): Integer; overload;
    function Send(const s: string): Integer; overload;
    function Send(Stream: TStream): Integer; overload; virtual;

    property Owner: TIocpTcpSocket read GetOwner;
    property Socket: TSocket read FSocket;
    property RefCount: Integer read GetRefCount;
    property FirstTick: DWORD read FFirstTick;
    property LastTick: DWORD read FLastTick;

    property PeerIP: string read FRemoteIP;
    property PeerAddr: string read FRemoteIP;
    property PeerPort: Word read FRemotePort;
    property IsClosed: Boolean read GetIsClosed;
    property IsIdle: Boolean read GetIsIdle;
    property SndBufSize: Integer read FSndBufSize;
    property RcvBufSize: Integer read FRcvBufSize;
    property PendingSend: Integer read FPendingSend;
    property PendingRecv: Integer read FPendingRecv;
    property IsIPv6: Boolean read FIsIPv6;
    {$IFDEF __TIME_OUT_TIMER__}
    property Timeout: DWORD read FTimeout write FTimeout;
    property Life: DWORD read FLife write FLife;
    {$ENDIF}
    property Tag: Pointer read FTag write FTag;
  end;

  TIocpSocketConnectionClass = class of TIocpSocketConnection;

  {
    *** Socket连接列表 ***
  }
  TIocpSocketConnectionPair = TPair<TSocket, TIocpSocketConnection>;
  TIocpSocketConnectionDictionary = class(TDictionary<TSocket, TIocpSocketConnection>)
  private
    FOwner: TIocpTcpSocket;

    function GetItem(Socket: TSocket): TIocpSocketConnection;
    procedure SetItem(Socket: TSocket; const Value: TIocpSocketConnection);
  public
    constructor Create(AOwner: TIocpTcpSocket); virtual;

    procedure Assign(const Source: TIocpSocketConnectionDictionary);
    function Delete(Socket: TSocket): Boolean;

    property Item[Socket: TSocket]: TIocpSocketConnection read GetItem write SetItem; default;
  end;

  {
    *** IO处理线程 ***
    这是主要的数据收发线程,由IOCP线程池调度
  }
  TIocpIoThread = class(TThread)
  private
    FOwner: TIocpTcpSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(IocpSocket: TIocpTcpSocket); reintroduce;
  end;

  {
    *** Accept线程 ***
    用于在AcceptEx套接字不足时生成新的套接字
  }
  TIocpAcceptThread = class(TThread)
  private
    FOwner: TIocpTcpSocket;
    FListenSocket: TSocket;
    FInitAcceptNum: Integer;
    FAiFamily: Integer;
    FShutdownEvent: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(IocpSocket: TIocpTcpSocket; ListenSocket: TSocket; AiFamily, InitAcceptNum: Integer); reintroduce;

    procedure Quit;
    property ListenSocket: TSocket read FListenSocket;
  end;

  TIocpNotifyEvent = function(Sender: TObject; Client: TIocpSocketConnection): Boolean of object;
  TIocpDataEvent = function(Sender: TObject; Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean of object;

  {
    *** 主要的Socket实现类 ***
  }
  TIocpTcpSocket = class(TComponent)
  private
    FShutdown: Boolean;
    FIocpHandle: THandle;
    FIoThreadsNumber: Integer;
    FIoThreads: array of TIocpIoThread;
    FIoThreadHandles: array of THandle;
    FPendingRequest: Integer;
    FConnectionPool: TIocpObjectPool;
    FPerIoDataPool: TIocpMemoryPool;
    FConnectionList, FIdleConnectionList: TIocpSocketConnectionDictionary;
    FConnectionListLocker: TCriticalSection;
    FListenThreads: TList;
    FListenThreadsLocker: TCriticalSection;
    {$IFDEF __TIME_OUT_TIMER__}
    FTimerQueue: TIocpTimerQueue;
    FTimeout: DWORD;
    FClientLife: DWORD;
    {$ENDIF}
    FMaxClients: Integer;
    FSentBytes, FRecvBytes: Int64;
    FOnClientConnected: TIocpNotifyEvent;
    FOnClientSentData: TIocpDataEvent;
    FOnClientRecvData: TIocpDataEvent;
    FOnClientDisconnected: TIocpNotifyEvent;

    procedure ProcessRequest(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData; IoThread: TIocpIoThread); virtual;
    procedure ExtractAddrInfo(const Addr: PSockAddr; AddrLen: Integer; out IP: string; out Port: Word);
    function GetConnectionFreeMemory: Integer;
    function GetConnectionUsedMemory: Integer;
    function GetPerIoFreeMemory: Integer;
    function GetPerIoUsedMemory: Integer;
    function GetConnectionClass: TIocpSocketConnectionClass;
    procedure SetConnectionClass(const Value: TIocpSocketConnectionClass);
    function GetIoCacheFreeMemory: Integer;
    function GetIoCacheUsedMemory: Integer;

    function AllocConnection(Socket: TSocket): TIocpSocketConnection;
    procedure FreeConnection(Connection: TIocpSocketConnection);
    function AllocIoData(Socket: TSocket; Operation: TIocpOperationType): PIocpPerIoData;
    procedure FreeIoData(PerIoData: PIocpPerIoData);

    function AssociateSocketWithCompletionPort(Socket: TSocket; Connection: TIocpSocketConnection): Boolean;
    function PostNewAcceptEx(ListenSocket: TSocket; AiFamily: Integer): Boolean;

    procedure RequestAcceptComplete(PerIoData: PIocpPerIoData);
    procedure RequestConnectComplete(Connection: TIocpSocketConnection);
    procedure RequestReadZeroComplete(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData);
    procedure RequestReadComplete(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData);
    procedure RequestWriteComplete(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData);

    function _TriggerClientConnected(Client: TIocpSocketConnection): Boolean;
    function _TriggerClientDisconnected(Client: TIocpSocketConnection): Boolean;
    function _TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
    function _TriggerClientSentData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
  protected
    function ProcessMessage: Boolean;
    procedure MessagePump;

    procedure StartupWorkers; virtual;
    procedure ShutdownWorkers; virtual;

    // 重载下面几个方法可以实现在IO事件触发时做相应处理
    // 连接建立时触发
    function TriggerClientConnected(Client: TIocpSocketConnection): Boolean; virtual;

    // 连接断开时触发
    function TriggerClientDisconnected(Client: TIocpSocketConnection): Boolean; virtual;

    // 接收到数据时触发
    function TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; virtual;

    // 发送数据完成时触发
    // 这里Buf只有指针本身是可以安全使用的，它所指向的内存数据很有可能已经被释放了
    // 所以千万不要在这个事件中去尝试访问Buf所指向的数据
    function TriggerClientSentData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; IoThreadsNumber: Integer); reintroduce; overload;
    destructor Destroy; override;

    function Listen(const Host: string; Port: Word; InitAcceptNum: Integer): Boolean; overload;
    function Listen(Port: Word; InitAcceptNum: Integer): Boolean; overload;
    procedure StopListen(ListenSocket: TSocket);
    procedure CloseSocket(Socket: TSocket);
    function AsyncConnect(const RemoteAddr: string; RemotePort: Word; Tag: Pointer = nil): TSocket;
    function Connect(const RemoteAddr: string; RemotePort: Word; Tag: Pointer = nil; ConnectTimeout: DWORD = 10000): TIocpSocketConnection;
    procedure DisconnectAll;

    function LockConnectionList: TIocpSocketConnectionDictionary;
    procedure UnlockConnectionList;

    property ConnectionClass: TIocpSocketConnectionClass read GetConnectionClass write SetConnectionClass;
    property ConnectionList: TIocpSocketConnectionDictionary read FConnectionList;
    property ConnectionUsedMemory: Integer read GetConnectionUsedMemory;
    property ConnectionFreeMemory: Integer read GetConnectionFreeMemory;
    property PerIoDataPool: TIocpMemoryPool read FPerIoDataPool;
    property PerIoUsedMemory: Integer read GetPerIoUsedMemory;
    property PerIoFreeMemory: Integer read GetPerIoFreeMemory;
    property IoCacheUsedMemory: Integer read GetIoCacheUsedMemory;
    property IoCacheFreeMemory: Integer read GetIoCacheFreeMemory;
    property SentBytes: Int64 read FSentBytes;
    property RecvBytes: Int64 read FRecvBytes;
    property PendingRequest: Integer read FPendingRequest;
    {$IFDEF __TIME_OUT_TIMER__}
    property TimerQueue: TIocpTimerQueue read FTimerQueue;
    {$ENDIF}
  published
    {$IFDEF __TIME_OUT_TIMER__}
    property Timeout: DWORD read FTimeout write FTimeout default 0;
    property ClientLife: DWORD read FClientLife write FClientLife default 0;
    {$ENDIF}
    property MaxClients: Integer read FMaxClients write FMaxClients default 0;
    property OnClientConnected: TIocpNotifyEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TIocpNotifyEvent read FOnClientDisconnected write FOnClientDisconnected;
    property OnClientRecvData: TIocpDataEvent read FOnClientRecvData write FOnClientRecvData;
    property OnClientSentData: TIocpDataEvent read FOnClientSentData write FOnClientSentData;
  end;

  TIocpLineSocketConnection = class(TIocpSocketConnection)
  private
    FLineText: TIocpStringStream;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function Send(const s: RawByteString): Integer; reintroduce;

    property LineText: TIocpStringStream read FLineText;
  end;

  TIocpLineRecvEvent = procedure(Sender: TObject; Client: TIocpLineSocketConnection; Line: RawByteString) of object;
  TIocpLineSocket = class(TIocpTcpSocket)
  private
    FLineLimit: Integer;
    FLineEndTag: RawByteString;
    FOnRecvLine: TIocpLineRecvEvent;

    procedure SetLineEndTag(const Value: RawByteString);
  protected
    function TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; override;

    procedure ParseRecvData(Client: TIocpLineSocketConnection; Buf: Pointer; Len: Integer); virtual;

    // 重载这个方法，在里面处理接收到的文本行
    procedure DoOnRecvLine(Client: TIocpLineSocketConnection; Line: RawByteString); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    function Connect(const RemoteAddr: string; RemotePort: Word; Tag: Pointer = nil; ConnectTimeout: DWORD = 10000): TIocpLineSocketConnection;
  published
    property LineEndTag: RawByteString read FLineEndTag write SetLineEndTag;
    property LineLimit: Integer read FLineLimit write FLineLimit default 65536;
    property OnRecvLine: TIocpLineRecvEvent read FOnRecvLine write FOnRecvLine;
  end;

  TIocpLineServer = class(TIocpLineSocket)
  private
    FAddr: string;
    FPort: Word;
    FListened: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    function Start: Boolean;
    function Stop: Boolean;
  published
    property Addr: string read FAddr write FAddr;
    property Port: Word read FPort write FPort;
  end;

  TSimpleIocpTcpClient = class(TIocpTcpSocket)
  private
    FServerPort: Word;
    FServerAddr: string;
  public
    function AsyncConnect(Tag: Pointer = nil): TSocket;
    function Connect(Tag: Pointer = nil; ConnectTimeout: DWORD = 10000): TIocpSocketConnection;
  published
    property ServerAddr: string read FServerAddr write FServerAddr;
    property ServerPort: Word read FServerPort write FServerPort;
  end;

  TSimpleIocpTcpServer = class(TIocpTcpSocket)
  private
    FAddr: string;
    FPort: Word;
    FListened: Boolean;
    FInitAcceptNum: Integer;
    FStartTick: DWORD;
    FActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Start: Boolean;
    function Stop: Boolean;

    property StartTick: DWORD read FStartTick;
  published
    property Addr: string read FAddr write FAddr;
    property Port: Word read FPort write FPort;
    property InitAcceptNum: Integer read FInitAcceptNum write FInitAcceptNum default INIT_ACCEPTEX_NUM;
    property Active: Boolean read FActive write SetActive;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpTcpSocket, TSimpleIocpTcpClient, TSimpleIocpTcpServer, TIocpLineSocket, TIocpLineServer]);
end;

var
  IoCachePool, FileCachePool: TIocpMemoryPool;

{ TIocpSocketConnection }

constructor TIocpSocketConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FRcvBuffer := IoCachePool.GetMemory;
end;

destructor TIocpSocketConnection.Destroy;
begin
  IoCachePool.FreeMemory(FRcvBuffer);

  inherited Destroy;
end;

function TIocpSocketConnection.AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TIocpSocketConnection.Release: Boolean;
begin
  Result := (InterlockedDecrement(FRefCount) = 0);
  if not Result then Exit;

  Owner.CloseSocket(FSocket);
  Owner._TriggerClientDisconnected(Self);
  Owner.FreeConnection(Self);
end;

procedure TIocpSocketConnection.Disconnect;
begin
  if (InterlockedExchange(FDisconnected, 1) <> 0) then Exit;

  Release;
  {$IFDEF __TIME_OUT_TIMER__}
  FTimer.Release;
  {$ENDIF}
end;

procedure TIocpSocketConnection.DecPendingRecv;
begin
  InterlockedDecrement(FPendingRecv);
end;

procedure TIocpSocketConnection.DecPendingSend;
begin
  InterlockedDecrement(FPendingSend);
end;

procedure TIocpSocketConnection.Finalize;
begin
end;

function TIocpSocketConnection.GetIsClosed: Boolean;
begin
  Result := (InterlockedExchange(FDisconnected, FDisconnected) = 1);
end;

function TIocpSocketConnection.GetIsIdle: Boolean;
begin
  Result := not GetIsClosed and (FPendingSend = 0) and (FPendingRecv = 0)
end;

function TIocpSocketConnection.GetOwner: TIocpTcpSocket;
begin
  Result := TIocpTcpSocket(inherited Owner);
end;

function TIocpSocketConnection.GetRefCount: Integer;
begin
  Result := InterlockedExchange(FRefCount, FRefCount);
end;

function TIocpSocketConnection.InitSocket: Boolean;
var
{$IF defined(__TCP_SNDBUF_ZERO_COPY__) or defined(__TCP_RCVBUF_ZERO_COPY__)}
  BufSize: Integer;
{$IFEND}
{$IF not (defined(__TCP_SNDBUF_ZERO_COPY__) and defined(__TCP_RCVBUF_ZERO_COPY__))}
  OptLen: Integer;
{$IFEND}
{$IFDEF __TCP_NODELAY__}
  NagleValue: Byte;
{$ENDIF}
begin
  Result := False;

{$IFDEF __TCP_SNDBUF_ZERO_COPY__}
  BufSize := 0;
  if (setsockopt(FSocket, SOL_SOCKET, SO_SNDBUF,
    PAnsiChar(@BufSize), SizeOf(BufSize)) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.setsockopt.SO_SNDBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)], ltWarning);
    Exit;
  end;
  FSndBufSize := IoCachePool.BlockSize;
{$ELSE}
  OptLen := SizeOf(FSndBufSize);
  if (getsockopt(FSocket, SOL_SOCKET, SO_SNDBUF,
    PAnsiChar(@FSndBufSize), OptLen) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.getsockopt.SO_SNDBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)], ltWarning);
    Exit;
  end;
{$ENDIF}

{$IFDEF __TCP_RCVBUF_ZERO_COPY__}
  BufSize := 0;
  if (setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF,
    PAnsiChar(@BufSize), SizeOf(BufSize)) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.setsockopt.SO_RCVBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)], ltWarning);
    Exit;
  end;
  FRcvBufSize := IoCachePool.BlockSize;
{$ELSE}
  OptLen := SizeOf(FRcvBufSize);
  if (getsockopt(FSocket, SOL_SOCKET, SO_RCVBUF,
    PAnsiChar(@FRcvBufSize), OptLen) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.getsockopt.SO_RCVBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)], ltWarning);
    Exit;
  end;
{$ENDIF}

{$IFDEF __TCP_NODELAY__}
  NagleValue := 1;
  if (setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@NagleValue), SizeOf(Byte)) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.setsockopt.TCP_NODELAY ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)], ltWarning);
    Exit;
  end;
{$ENDIF}

  Result := True;
end;

procedure TIocpSocketConnection.IncPendingRecv;
begin
  InterlockedIncrement(FPendingRecv);
end;

procedure TIocpSocketConnection.IncPendingSend;
begin
  InterlockedIncrement(FPendingSend);
end;

procedure TIocpSocketConnection.Initialize;
begin
  FSocket := 0;
  FRefCount := 1; // 置初始引用计数 1
  FPendingSend := 0;
  FPendingRecv := 0;
  FDisconnected := 0;
  FLastTick := 0;
  FTag := nil;

//  ZeroMemory(@FRemoteAddr, SizeOf(TSockAddrIn));
  FRemoteIP := '';
  FRemotePort := 0;

  FFirstTick := GetTickCount;

  {$IFDEF __TIME_OUT_TIMER__}
  FTimeout := Owner.Timeout;
  FLife := Owner.ClientLife;
  FTimer := TIocpTimerQueueTimer.Create(Owner.FTimerQueue, 1000);
  FTimer.OnCreate := OnTimerCreate;
  FTimer.OnTimer := OnTimerExecute;
  FTimer.OnDestroy := OnTimerDestroy;
  {$ENDIF}
end;

function TIocpSocketConnection._Send(Buf: Pointer; Size: Integer): Integer;
var
  BlockSize: Integer;
begin
  if IsClosed then Exit(-1);

  Result := Size;

  // 在IocpHttpServer的实际测试中发现，当Server发送的块大于4K的时候
  // 浏览器收到的数据有可能会出现混乱，所以稳妥起见，这里将发送的内
  // 存块拆分成4K的小块发送
  while (Size > 0) do
  begin
    BlockSize := Min(IoCachePool.BlockSize, Size);
    if not PostWrite(Buf, BlockSize) then Exit(-2);
    Inc(PAnsiChar(Buf), BlockSize);
    Dec(Size, BlockSize);
  end;
end;

function TIocpSocketConnection.Send(Buf: Pointer; Size: Integer): Integer;
begin
  Result := _Send(Buf, Size);
end;

function TIocpSocketConnection.Send(const Buf; Size: Integer): Integer;
begin
  Result := Send(@Buf, Size);
end;

function TIocpSocketConnection.Send(const Bytes: TBytes): Integer;
begin
  Result := Send(Pointer(Bytes), Length(Bytes));
end;

function TIocpSocketConnection.Send(const s: RawByteString): Integer;
begin
  Result := Send(Pointer(s), Length(s));
end;

function TIocpSocketConnection.Send(const s: string): Integer;
begin
  Result := Send(Pointer(s), Length(s) * SizeOf(Char));
end;

function TIocpSocketConnection.Send(Stream: TStream): Integer;
var
  Buf: Pointer;
  BufSize, BlockSize: Integer;
begin
  BufSize := FileCachePool.BlockSize;
  Buf := FileCachePool.GetMemory;
  try
    Stream.Position := 0;
    while True do
    begin
      BlockSize := Stream.Read(Buf^, BufSize);
      if (BlockSize = 0) then Break;

      if (_Send(Buf, BlockSize) < 0) then Exit(-1);
    end;

    Result := Stream.Size;
  finally
    FileCachePool.FreeMemory(Buf);
  end;
end;

procedure TIocpSocketConnection._TriggerClientRecvData(Buf: Pointer; Len: Integer);
begin
end;

procedure TIocpSocketConnection._TriggerClientSentData(Buf: Pointer; Len: Integer);
begin
  IoCachePool.FreeMemory(Buf);
end;

{$IFDEF __TIME_OUT_TIMER__}
procedure TIocpSocketConnection.OnTimerCreate(Sender: TObject);
begin
  AddRef; // 为Timer增加连接引用计数
end;

procedure TIocpSocketConnection.OnTimerDestroy(Sender: TObject);
begin
  Release; // 对应Timer创建时的AddRef(procedure Initialize)
end;

procedure TIocpSocketConnection.OnTimerExecute(Sender: TObject);
begin
  try
    if IsClosed then
    begin
      FTimer.Release; // 连接断开时，将Timer也释放掉，以免Timer再次触发引发异常访问
      Exit;
    end;

    // 超时,断开连接
    if (FTimeout > 0) and (FLastTick > 0) and (CalcTickDiff(FLastTick, GetTickCount) > FTimeout) then
    begin
      TriggerTimeout;
      FTimer.Release; // 连接断开时，将Timer也释放掉，以免Timer再次触发引发异常访问
      Disconnect;
      Exit;
    end;

    // 超过生命期,断开连接
    if (FLife > 0) and (FFirstTick > 0) and (CalcTickDiff(FFirstTick, GetTickCount) > FLife) then
    begin
      TriggerLifeout;
      FTimer.Release; // 连接断开时，将Timer也释放掉，以免Timer再次触发引发异常访问
      Disconnect;
      Exit;
    end;
  except
  end;
end;

procedure TIocpSocketConnection.TriggerTimeout;
begin
end;

procedure TIocpSocketConnection.TriggerLifeout;
begin
end;
{$ENDIF}

function TIocpSocketConnection.PostReadZero: Boolean;
var
  PerIoData: PIocpPerIoData;
  Bytes, Flags: Cardinal;
  LastErr: Integer;
begin
  Result := False;
  if IsClosed then Exit;

  // 增加引用计数
  // 如果返回1则说明现在正在关闭连接
  if (AddRef = 1) then Exit;

  PerIoData := Owner.AllocIoData(FSocket, iotReadZero);
  PerIoData.Buffer.DataBuf.Buf := nil;
  PerIoData.Buffer.DataBuf.Len := 0;
  Flags := 0;
  Bytes := 0;
  if (WSARecv(PerIoData.ClientSocket, @PerIoData.Buffer.DataBuf, 1, Bytes, Flags, PWSAOverlapped(PerIoData), nil) = SOCKET_ERROR)
    and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    LastErr := WSAGetLastError;
    if (LastErr <> WSAECONNABORTED) and (LastErr <> WSAECONNRESET) then
      AppendLog('%s.Socket%d PostReadZero.WSARecv ERROR %d=%s', [ClassName, FSocket, LastErr, SysErrorMessage(LastErr)], ltWarning);
    Release; // 对应函数开头的 AddRef
    Disconnect; // 对应连接初始化时的 FRefCount := 1
    Owner.FreeIoData(PerIoData);
    Exit;
  end;

  Result := True;
end;

function TIocpSocketConnection.PostRead: Boolean;
var
  PerIoData: PIocpPerIoData;
  Bytes, Flags: Cardinal;
  LastErr: Integer;
begin
  Result := False;
  if IsClosed then Exit;

  // 增加引用计数
  // 如果返回1则说明现在正在关闭连接
  if (AddRef = 1) then Exit;

  IncPendingRecv;

  PerIoData := Owner.AllocIoData(FSocket, iotRead);
  PerIoData.Buffer.DataBuf.Buf := FRcvBuffer;
  PerIoData.Buffer.DataBuf.Len := IoCachePool.BlockSize;

  Flags := 0;
  Bytes := 0;
  if (WSARecv(PerIoData.ClientSocket, @PerIoData.Buffer.DataBuf, 1, Bytes, Flags, PWSAOverlapped(PerIoData), nil) = SOCKET_ERROR)
    and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    LastErr := WSAGetLastError;
    if (LastErr <> WSAECONNABORTED) and (LastErr <> WSAECONNRESET) then
      AppendLog('%s.Socket%d PostRead.WSARecv ERROR %d=%s', [ClassName, FSocket, LastErr, SysErrorMessage(LastErr)], ltWarning);
    DecPendingRecv;
    Release; // 对应函数开头的 AddRef
    Disconnect; // 对应连接初始化时的 FRefCount := 1
    Owner.FreeIoData(PerIoData);
    Exit;
  end;

  Result := True;
end;

function TIocpSocketConnection.PostWrite(const Buf: Pointer; Size: Integer): Boolean;
var
  PerIoData: PIocpPerIoData;
  Bytes: DWORD;
  LastErr: Integer;
  SndBuf: Pointer;
begin
  if IsClosed then Exit(False);

  // 增加引用计数
  // 如果返回1则说明现在正在关闭连接
  if (AddRef = 1) then Exit(False);

  IncPendingSend;

  SndBuf := IoCachePool.GetMemory;
  CopyMemory(SndBuf, Buf, Size);

  PerIoData := Owner.AllocIoData(FSocket, iotWrite);
  PerIoData.Buffer.DataBuf.Buf := SndBuf;
  PerIoData.Buffer.DataBuf.Len := Size;

  // WSAEFAULT(10014)
  // The lpBuffers, lpNumberOfBytesSent, lpOverlapped, lpCompletionRoutine parameter
  // is not totally contained in a valid part of the user address space.
  if (WSASend(FSocket, @PerIoData.Buffer.DataBuf, 1, Bytes, 0, PWSAOverlapped(PerIoData), nil) = SOCKET_ERROR)
    and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    LastErr := WSAGetLastError;
    if (LastErr <> WSAECONNABORTED) and (LastErr <> WSAECONNRESET) then
      AppendLog('%s.Socket%d PostWrite.WSASend error, ERR=%d,%s', [ClassName, FSocket, LastErr, SysErrorMessage(LastErr)], ltWarning);
    DecPendingSend;
    Release; // 对应函数开头的 AddRef
    Disconnect; // 对应连接初始化时的 FRefCount := 1
    Owner.FreeIoData(PerIoData);
    IoCachePool.FreeMemory(SndBuf);
    Exit(False);
  end;

  Result := True;
end;

procedure TIocpSocketConnection.UpdateTick;
begin
  FLastTick := GetTickCount;
end;

{ TIocpSocketConnectionDictionary }

constructor TIocpSocketConnectionDictionary.Create(AOwner: TIocpTcpSocket);
begin
  inherited Create;

  FOwner := AOwner;
end;

procedure TIocpSocketConnectionDictionary.Assign(const Source: TIocpSocketConnectionDictionary);
var
  Pair: TIocpSocketConnectionPair;
begin
  FOwner := Source.FOwner;
  Clear;
  for Pair in Source do
    AddOrSetValue(Pair.Key, Pair.Value);
end;

function TIocpSocketConnectionDictionary.GetItem(Socket: TSocket): TIocpSocketConnection;
begin
  if not TryGetValue(Socket, Result) then
    Result := nil;
end;

procedure TIocpSocketConnectionDictionary.SetItem(Socket: TSocket;
  const Value: TIocpSocketConnection);
begin
  AddOrSetValue(Socket, Value);
end;

function TIocpSocketConnectionDictionary.Delete(Socket: TSocket): Boolean;
begin
  Result := ContainsKey(Socket);
  if Result then
    Remove(Socket);
end;

{ TIocpIoThread }

constructor TIocpIoThread.Create(IocpSocket: TIocpTcpSocket);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FOwner := IocpSocket;

  Suspended := False;
end;

procedure TIocpIoThread.Execute;
const
  ERROR_ABANDONED_WAIT_0 = 735;
var
  IocpStatusOk: Boolean;
  BytesTransferred: Cardinal;
  Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData;
  LastErr: DWORD;
begin
//  AppendLog('%s.IoThread %d start', [FOwner.ClassName, ThreadID]);
  while not Terminated do
  try
    IocpStatusOk := Iocp.ApiFix.GetQueuedCompletionStatus(FOwner.FIocpHandle,
      BytesTransferred, ULONG_PTR(Connection), POverlapped(PerIoData), WSA_INFINITE);

    {
    (1) 如果I/O操作(WSASend() / WSARecv())成功完成,那么返回值为TRUE,并且 lpNumberOfBytes 为已传送的字节数.注意,已传送的字节数有可能小于你请求发送/接收的字节数.
    (2) 如果对方关闭了套接字,那么有两种情况
    (a) I/O操作已经完成了一部分,比如WSASend()请求发送1K字节,并且其中的512字节已经发送完成,则返回值为TRUE, lpNumberOfBytes 指向的值为512, lpOverlapped 有效.
    (b) I/O操作没有完成,那么返回值为FALSE, lpNumberOfBytes 指向的值为0, lpCompletionKey, lpOverlapped 有效.
    (3) 如果我们程序这方主动关闭了套接字,则和(2)的情况一样,没有区别.
    (4) 如果发生了其它错误,则返回值为FALSE,并且 lpCompletionKey, lpOverlapped = NULL,在这种情况下,应该调用 GetLastError() 查看错误信息,并且退出等待GetQueuedCompletionStatus()的循环.
    }

    if not IocpStatusOk then
    begin
      if (PerIoData = nil) and (Connection = nil) then
      begin
        LastErr := GetLastError;
        if (LastErr <> ERROR_ABANDONED_WAIT_0) then
          AppendLog('%s Io线程 %d 出错. ERR %d=%s', [FOwner.ClassName, ThreadID, LastErr, SysErrorMessage(LastErr)], ltError);
        Break;
      end;

      if (PerIoData <> nil) then
        FOwner.FreeIoData(PerIoData);

      if (Connection <> nil) then
      begin
        Connection.Release; // 对应PostRead/PostWrite中的AddRef
        Connection.Disconnect; // 对应连接创建时的FRefCount := 1
      end;

      Continue;
    end;

    if (BytesTransferred = 0) and (ULONG_PTR(PerIoData) = SHUTDOWN_FLAG) then Break;
    if (Connection = nil) and (PerIoData = nil) then Continue;

    PerIoData.BytesTransfered := BytesTransferred;
    FOwner.ProcessRequest(Connection, PerIoData, Self);
  except
    on e: Exception do
      AppendLog('TIocpIoThread.Execute, %s=%s', [e.ClassName, e.Message], ltException);
  end;

//  AppendLog('%s.IoThread %d exit', [FOwner.ClassName, ThreadID]);
end;

{ TIocpAcceptThread }

constructor TIocpAcceptThread.Create(IocpSocket: TIocpTcpSocket;
  ListenSocket: TSocket; AiFamily, InitAcceptNum: Integer);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FOwner := IocpSocket;
  FAiFamily := AiFamily;
  FInitAcceptNum := InitAcceptNum;
  FListenSocket := ListenSocket;
  FShutdownEvent := CreateEvent(nil, True, False, nil);

  Suspended := False;
end;

procedure TIocpAcceptThread.Execute;
var
  i: Integer;
  LastErr: Integer;
  AcceptEvents: array[0..1] of THandle;
  RetEvents: TWSANetworkEvents;
  dwRet: DWORD;
begin
//  AppendLog('%s.AcceptThread %d start', [FOwner.ClassName, ThreadID]);
  try
    for i := 1 to FInitAcceptNum do
      FOwner.PostNewAcceptEx(FListenSocket, FAiFamily);

    AcceptEvents[0] := WSACreateEvent; // 新建一个事件用于绑定 FD_ACCEPT
    AcceptEvents[1] := FShutdownEvent;
    try
      // 绑定监听端口的ACCEPT事件，当没有足够的Accept套接字时就会触发该事件
      WSAEventSelect(FListenSocket, AcceptEvents[0], FD_ACCEPT);

      while not Terminated do
      begin
        // 等待退出或者ACCEPT事件
        dwRet := WSAWaitForMultipleEvents(2, @AcceptEvents[0], False, INFINITE, True);

        // 收到退出事件通知
        if (dwRet = WSA_WAIT_EVENT_0 + 1) or (dwRet = WSA_WAIT_FAILED) or Terminated then Break;

        // 读取事件状态
        if (WSAEnumNetworkEvents(FListenSocket, AcceptEvents[0], @RetEvents) = SOCKET_ERROR) then
        begin
          LastErr := WSAGetLastError;
          AppendLog('%s.WSAEnumNetworkEvents失败, ERROR %d=%s', [ClassName, LastErr, SysErrorMessage(LastErr)], ltWarning);
          Break;
        end;

        // 如果ACCEPT事件触发，则投递新的Accept套接字
        // 每次投递 FInitAcceptNum 个
        if (RetEvents.lNetworkEvents and FD_ACCEPT = FD_ACCEPT) then
        begin
          if (RetEvents.iErrorCode[FD_ACCEPT_BIT] <> 0) then
          begin
            LastErr := WSAGetLastError;
            AppendLog('%s.WSAEnumNetworkEvents失败, ERROR %d=%s', [ClassName, LastErr, SysErrorMessage(LastErr)], ltWarning);
            Break;
          end;

          for i := 1 to FInitAcceptNum do
          begin
            if not FOwner.PostNewAcceptEx(FListenSocket, FAiFamily) then Break;
          end;
        end;
      end;
      FOwner.CloseSocket(FListenSocket);
    finally
      CloseHandle(AcceptEvents[0]);
      CloseHandle(AcceptEvents[1]);
    end;
  except
    on e: Exception do
      AppendLog('%s.Execute, %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
//  AppendLog('%s.AcceptThread %d exit', [FOwner.ClassName, ThreadID]);
end;

procedure TIocpAcceptThread.Quit;
begin
  SetEvent(FShutdownEvent);
end;

{ TIocpTcpSocket }

constructor TIocpTcpSocket.Create(AOwner: TComponent; IoThreadsNumber: Integer);
begin
  inherited Create(AOwner);

  FConnectionPool := TIocpObjectPool.Create(Self, TIocpSocketConnection, MAX_FREE_HANDLE_DATA_BLOCKS);
  FPerIoDataPool := TIocpMemoryPool.Create(SizeOf(TIocpPerIoData), MAX_FREE_IO_DATA_BLOCKS);
  FConnectionList := TIocpSocketConnectionDictionary.Create(Self);
  FConnectionListLocker := TCriticalSection.Create;
  FIdleConnectionList := TIocpSocketConnectionDictionary.Create(Self);

  FListenThreads := TList.Create;
  FListenThreadsLocker := TCriticalSection.Create;

  FIoThreadsNumber := IoThreadsNumber;
  FIocpHandle := 0;
  {$IFDEF __TIME_OUT_TIMER__}
  FTimeout := 0;
  FClientLife := 0;
  {$ENDIF}
  FMaxClients := 0;
  StartupWorkers;
end;

constructor TIocpTcpSocket.Create(AOwner: TComponent);
begin
  Create(AOwner, 0);
end;

destructor TIocpTcpSocket.Destroy;
begin
  ShutdownWorkers;

  FConnectionList.Free;
  FConnectionListLocker.Free;
  FIdleConnectionList.Free;
  FListenThreads.Free;
  FListenThreadsLocker.Free;
  FConnectionPool.Free;
  FPerIoDataPool.Clear;
  FPerIoDataPool.Release;

  inherited Destroy;
end;

function TIocpTcpSocket.PostNewAcceptEx(ListenSocket: TSocket; AiFamily: Integer): Boolean;
var
  PerIoData: PIocpPerIoData;
  ClientSocket: TSocket;
  Bytes: Cardinal;
  LastErr: Integer;
  Connection: TIocpSocketConnection;
begin
  Result := False;

  ClientSocket := WSASocket(AiFamily, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (ClientSocket = INVALID_SOCKET) then
  begin
    LastErr := WSAGetLastError;
    AppendLog('%s.PostNewAcceptEx.为AcceptEx创建Socket失败, ERR=%d,%s', [ClassName, LastErr, SysErrorMessage(LastErr)], ltWarning);
    Exit;
  end;

  // 生成新的连接对象
  Connection := AllocConnection(ClientSocket);
  Connection.FIsIPv6 := (AiFamily = AF_INET6);

  // 将连接放到空闲连接列表中
  // 在ShutdownWorks中才能完整释放Socket资源，否则会造成Socket句柄泄露
  try
    FConnectionListLocker.Enter;
    FIdleConnectionList[ClientSocket] := Connection;
  finally
    FConnectionListLocker.Leave;
  end;

  PerIoData := AllocIoData(ClientSocket, iotAccept);
  PerIoData.ListenSocket := ListenSocket;
  if (not AcceptEx(ListenSocket, ClientSocket, @PerIoData.Buffer.AcceptExBuffer, 0,
    SizeOf(TAddrBuffer), SizeOf(TAddrBuffer), Bytes, POverlapped(PerIoData))) then
  begin
    LastErr := WSAGetLastError;
    if (LastErr <> WSA_IO_PENDING) then
    begin
      AppendLog('%s.PostNewAcceptEx.调用AcceptEx失败(ListenSocket=%d, ClientSocket=%d), ERR=%d,%s', [ClassName, ListenSocket, ClientSocket, LastErr, SysErrorMessage(LastErr)], ltWarning);
      FreeIoData(PerIoData);
      Connection.Disconnect;
      Exit;
    end;
  end;

  Result := True;
end;

function TIocpTcpSocket.AllocConnection(Socket: TSocket): TIocpSocketConnection;
begin
  Result := TIocpSocketConnection(FConnectionPool.GetObject);

  Result.FSocket := Socket;
end;

function TIocpTcpSocket.AssociateSocketWithCompletionPort(Socket: TSocket;
  Connection: TIocpSocketConnection): Boolean;
begin
  Result := (Iocp.ApiFix.CreateIoCompletionPort(Socket, FIocpHandle, ULONG_PTR(Connection), 0) <> 0);
  if not Result then
    AppendLog(Format('绑定IOCP失败, IocpHandle=%d, Socket=%d, ERR=%d,%s', [FIocpHandle, Socket, GetLastError, SysErrorMessage(GetLastError)]), ltWarning);
end;

function TIocpTcpSocket.AllocIoData(Socket: TSocket;
  Operation: TIocpOperationType): PIocpPerIoData;
begin
  Result := FPerIoDataPool.GetMemory;
  Result.ClientSocket := Socket;
  Result.Operation := Operation;

  ZeroMemory(@Result.Overlapped, SizeOf(TWSAOverlapped));
end;

procedure TIocpTcpSocket.DisconnectAll;
var
  Client: TIocpSocketConnection;
begin
  try
    FConnectionListLocker.Enter;

    // X.Values.ToArray将连接列表复制一份是因为连接断开后会
    // 自动从对应的工作/空闲列表中删除，这会造成迭代返回异常数据
    for Client in FConnectionList.Values.ToArray do
      Client.Disconnect;

    for Client in FIdleConnectionList.Values.ToArray do
      Client.Disconnect;
  finally
    FConnectionListLocker.Leave;
  end;
end;

function TIocpTcpSocket.AsyncConnect(const RemoteAddr: string; RemotePort: Word; Tag: Pointer): TSocket;
var
  InAddrInfo: TAddrInfoW;
  POutAddrInfo: PAddrInfoW;
  BindAddrIPv4: TSockAddrIn;
  BindAddrIPv6: TSockAddrIn6;
  PBindAddr: PSOCKADDR;
  BindAddrSize: Integer;
  ClientSocket: TSocket;
  Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData;
  LastErr: Integer;
begin
  Result := INVALID_SOCKET;
  // 超过最大允许连接数
  if (FMaxClients > 0) and (FConnectionList.Count >= FMaxClients) then Exit;

  {
    64位程序中gethostbyname返回的数据结构的h_addr_list指针无效(貌似高4字节和低4字节顺序颠倒了)
    用getaddrinfo返回的数据不会有问题,而且可以兼顾IPv4和IPv6,只需要简单的修改就能让程序同时支持
    IPv4和IPv6了
  }
  FillChar(InAddrInfo, SizeOf(TAddrInfoW), 0);
  InAddrInfo.ai_family := AF_UNSPEC;
  InAddrInfo.ai_socktype := SOCK_STREAM;
  InAddrInfo.ai_protocol := IPPROTO_TCP;
  POutAddrInfo := nil;
  if (getaddrinfo(PWideChar(RemoteAddr), PWideChar(IntToStr(RemotePort)), @InAddrInfo, @POutAddrInfo) <> 0) then
  begin
    LastErr := WSAGetLastError;
    AppendLog('%s.AsyncConnect getaddrinfo失败, ERR=%d,%s', [ClassName, LastErr, SysErrorMessage(LastErr)], ltWarning);
    Exit;
  end;

  try
    ClientSocket := WSASocket(POutAddrInfo.ai_family, POutAddrInfo.ai_socktype,
      POutAddrInfo.ai_protocol, nil, 0, WSA_FLAG_OVERLAPPED);
    if (ClientSocket = INVALID_SOCKET) then Exit;

    if (POutAddrInfo.ai_family = AF_INET6) then
    begin
      BindAddrSize := SizeOf(BindAddrIPv6);
      ZeroMemory(@BindAddrIPv6, SizeOf(BindAddrIPv6));
      BindAddrIPv6.sin6_family := AF_INET6;
      BindAddrIPv6.sin6_addr := in6addr_any;
      BindAddrIPv6.sin6_port := 0;
      PBindAddr := @BindAddrIPv6;
    end else
    begin
      BindAddrSize := SizeOf(BindAddrIPv4);
      ZeroMemory(@BindAddrIPv4, SizeOf(BindAddrIPv4));
      BindAddrIPv4.sin_family := AF_INET;
      BindAddrIPv4.sin_addr.S_addr := INADDR_ANY;
      BindAddrIPv4.sin_port := 0;
      PBindAddr := @BindAddrIPv4;
    end;
    if (bind(ClientSocket, PBindAddr, BindAddrSize) = SOCKET_ERROR) then
    begin
      LastErr := WSAGetLastError;
      Iocp.Winsock2.CloseSocket(ClientSocket);
      AppendLog('%s.AsyncConnect绑定ConnectEx(%d)端口失败, ERR=%d,%s', [ClassName, ClientSocket, LastErr, SysErrorMessage(LastErr)], ltWarning);
      Exit;
    end;

    // 生成新的连接对象并绑定到IOCP
    Connection := AllocConnection(ClientSocket);
    Connection.Tag := Tag; // thanks Hezihang2012
    if not AssociateSocketWithCompletionPort(ClientSocket, Connection) then
    begin
      Iocp.Winsock2.CloseSocket(ClientSocket);
      FConnectionPool.FreeObject(Connection);
      Exit;
    end;

    if (Connection.AddRef = 1) then Exit;

    Connection.FIsIPv6 := (POutAddrInfo.ai_family = AF_INET6);
    ExtractAddrInfo(POutAddrInfo.ai_addr, POutAddrInfo.ai_addrlen, Connection.FRemoteIP, Connection.FRemotePort);
    PerIoData := AllocIoData(ClientSocket, iotConnect);
    if not ConnectEx(ClientSocket, POutAddrInfo.ai_addr, POutAddrInfo.ai_addrlen, nil, 0, PCardinal(0)^, PWSAOverlapped(PerIoData)) and
      (WSAGetLastError <> WSA_IO_PENDING) then
    begin
      LastErr := WSAGetLastError;
      AppendLog('%s.AsyncConnect.ConnectEx(%d)失败, ERR=%d,%s', [ClassName, ClientSocket, LastErr, SysErrorMessage(LastErr)], ltWarning);
      FreeIoData(PerIoData);
      Connection.Release;
      Connection.Disconnect;
      Exit;
    end;
  finally
    freeaddrinfo(POutAddrInfo);
  end;
  Result := ClientSocket;
end;

function TIocpTcpSocket.Connect(const RemoteAddr: string;
  RemotePort: Word; Tag: Pointer; ConnectTimeout: DWORD): TIocpSocketConnection;
var
  Socket: TSocket;
  DummyHandle: THandle;
  t: DWORD;
begin
  Result := nil;
  Socket := AsyncConnect(RemoteAddr, RemotePort, Tag);
  if (Socket = INVALID_SOCKET) then Exit;

  if (ConnectTimeout <= 0) or (ConnectTimeout > 10000) then
    ConnectTimeout := 10000;

  t := GetTickCount;
  DummyHandle := INVALID_HANDLE_VALUE;
  while True do
  begin
    FConnectionListLocker.Enter;
    try
      Result := FConnectionList[Socket];
      if (Result <> nil) then Exit;
    finally
      FConnectionListLocker.Leave;
    end;

    if (MsgWaitForMultipleObjects(0, DummyHandle, False, 100, QS_ALLINPUT) = WAIT_OBJECT_0) then
      MessagePump;
    if (CalcTickDiff(t, GetTickCount) >= ConnectTimeout) then Exit;
  end;
end;

procedure TIocpTcpSocket.ExtractAddrInfo(const Addr: PSockAddr; AddrLen: Integer; out IP: string; out Port: Word);
var
  ServInfo: string;
begin
  SetLength(IP, NI_MAXHOST);
  SetLength(ServInfo, NI_MAXSERV);
  getnameinfo(Addr, AddrLen, PWideChar(IP), NI_MAXHOST, PWideChar(ServInfo), NI_MAXSERV, NI_NUMERICHOST or NI_NUMERICSERV);
  SetLength(IP, StrLen(PWideChar(IP)));
  SetLength(ServInfo, StrLen(PWideChar(ServInfo)));
  Port := StrToInt(ServInfo);
end;

procedure TIocpTcpSocket.FreeConnection(Connection: TIocpSocketConnection);
begin
  try
    FConnectionListLocker.Enter;
    if not FConnectionList.Delete(Connection.FSocket) then
      FIdleConnectionList.Delete(Connection.FSocket);
    FConnectionPool.FreeObject(Connection);
  finally
    FConnectionListLocker.Leave;
  end;
end;

procedure TIocpTcpSocket.FreeIoData(PerIoData: PIocpPerIoData);
begin
  FPerIoDataPool.FreeMemory(Pointer(PerIoData));
end;

function TIocpTcpSocket.GetConnectionClass: TIocpSocketConnectionClass;
begin
  Result := TIocpSocketConnectionClass(FConnectionPool.ObjectClass);
end;

function TIocpTcpSocket.GetConnectionFreeMemory: Integer;
begin
  Result := FConnectionPool.FreeObjectsSize;
end;

function TIocpTcpSocket.GetConnectionUsedMemory: Integer;
begin
  Result := FConnectionPool.UsedObjectsSize;
end;

function TIocpTcpSocket.GetIoCacheFreeMemory: Integer;
begin
  Result := IoCachePool.FreeBlocksSize + FileCachePool.FreeBlocksSize;
end;

function TIocpTcpSocket.GetIoCacheUsedMemory: Integer;
begin
  Result := IoCachePool.UsedBlocksSize + FileCachePool.UsedBlocksSize;
end;

function TIocpTcpSocket.GetPerIoFreeMemory: Integer;
begin
  Result := FPerIoDataPool.FreeBlocksSize;
end;

function TIocpTcpSocket.GetPerIoUsedMemory: Integer;
begin
  Result := FPerIoDataPool.UsedBlocksSize;
end;

function TIocpTcpSocket.Listen(const Host: string; Port: Word; InitAcceptNum: Integer): Boolean;
const
  IPV6_V6ONLY = 27;
var
  PHost: PWideChar;
  ListenSocket: TSocket;
  InAddrInfo: TAddrInfoW;
  POutAddrInfo, Ptr: PAddrInfoW;
  LastErr: Integer;
begin
  Result := False;

  try
    // 如果传递了一个有效地址则监听该地址
    // 否则监听所有本地地址
    if (Host = '') then
      PHost := nil
    else
      PHost := PWideChar(Host);

    FillChar(InAddrInfo, SizeOf(TAddrInfoW), 0);
    InAddrInfo.ai_flags := AI_PASSIVE;
    InAddrInfo.ai_family := AF_UNSPEC;
    InAddrInfo.ai_socktype := SOCK_STREAM;
    InAddrInfo.ai_protocol := IPPROTO_TCP;
    if (getaddrinfo(PHost, PWideChar(IntToStr(Port)), @InAddrInfo, @POutAddrInfo) <> 0) then
    begin
      LastErr := WSAGetLastError;
      AppendLog('%s.getaddrinfo失败, ERR=%d,%s', [ClassName, LastErr, SysErrorMessage(LastErr)], ltWarning);
      Exit;
    end;

    try
      Ptr := POutAddrInfo;
      while (Ptr <> nil) do
      begin
        ListenSocket := WSASocket(Ptr.ai_family, Ptr.ai_socktype, Ptr.ai_protocol, nil, 0, WSA_FLAG_OVERLAPPED);
        if (ListenSocket = INVALID_SOCKET) then Exit;

//        no := 0;
//        setsockopt(ListenSocket, IPPROTO_IPV6, IPV6_V6ONLY, PAnsiChar(@no), sizeof(no));

        if (bind(ListenSocket, Ptr.ai_addr, Ptr.ai_addrlen) = SOCKET_ERROR) then
        begin
          LastErr := WSAGetLastError;
          Iocp.Winsock2.CloseSocket(ListenSocket);
          AppendLog('%s.绑定监听端口(%d)失败, ERR=%d,%s', [ClassName, Port, LastErr, SysErrorMessage(LastErr)], ltWarning);
          Exit;
        end;

        if (Iocp.Winsock2.listen(ListenSocket, SOMAXCONN) = SOCKET_ERROR) then
        begin
          LastErr := WSAGetLastError;
          Iocp.Winsock2.CloseSocket(ListenSocket);
          AppendLog('%s.启动监听端口(%d)失败, ERR=%d,%s', [ClassName, Port, LastErr, SysErrorMessage(LastErr)], ltWarning);
          Exit;
        end;

        if not AssociateSocketWithCompletionPort(ListenSocket, nil) then
        begin
          Iocp.Winsock2.CloseSocket(ListenSocket);
          AppendLog('%s.绑定监听端口(%d)到IOCP失败', [ClassName, Port], ltWarning);
          Exit;
        end;

        try
          FListenThreadsLocker.Enter;
          FListenThreads.Add(TIocpAcceptThread.Create(Self, ListenSocket, Ptr.ai_family, InitAcceptNum));
        finally
          FListenThreadsLocker.Leave;
        end;

        Ptr := Ptr.ai_next;
      end;
    finally
      freeaddrinfo(POutAddrInfo);
    end;

    Result := True;
  except
    on e: Exception do
      AppendLog('%s.Listen ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

function TIocpTcpSocket.Listen(Port: Word; InitAcceptNum: Integer): Boolean;
begin
  Result := Listen('', Port, InitAcceptNum);
end;

function TIocpTcpSocket.LockConnectionList: TIocpSocketConnectionDictionary;
begin
  Result := FConnectionList;
  FConnectionListLocker.Enter;
end;

procedure TIocpTcpSocket.UnlockConnectionList;
begin
  FConnectionListLocker.Leave;
end;

procedure TIocpTcpSocket.MessagePump;
begin
  while ProcessMessage do;
end;

function TIocpTcpSocket.ProcessMessage: Boolean;
var
  Msg: TMsg;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := True;
    if (Msg.Message = WM_QUIT) then
    begin
    end
    else
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
end;

procedure TIocpTcpSocket.ProcessRequest(Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData; IoThread: TIocpIoThread);
begin
  try
    try
      case PerIoData.Operation of
        iotAccept:   RequestAcceptComplete(PerIoData);
        iotConnect:  RequestConnectComplete(Connection);
        iotReadZero: RequestReadZeroComplete(Connection, PerIoData);
        iotRead:     RequestReadComplete(Connection, PerIoData);
        iotWrite:    RequestWriteComplete(Connection, PerIoData);
      end;
    finally
      FreeIoData(PerIoData);
    end;
  except
    on e: Exception do
      AppendLog('%s.ProcessRequest, ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestAcceptComplete(PerIoData: PIocpPerIoData);
var
  Connection: TIocpSocketConnection;
  LocalAddrLen, RemoteAddrLen: Integer;
  PLocalAddr, PRemoteAddr: PSockAddr;
begin
  if FShutdown then Exit;
  
  try
    // 将连接放到工作连接列表中
    try
      FConnectionListLocker.Enter;
      Connection := FIdleConnectionList[PerIoData.ClientSocket];
      // 如果之前该连接存在于空闲连接列表中则将它移到工作连接列表中
      if (Connection <> nil) then
      begin
        FConnectionList[PerIoData.ClientSocket] := Connection;
        FIdleConnectionList.Delete(Connection.FSocket);
      end else
      //** 理论上永远不会执行到这里来
      begin
        Connection := FConnectionList[PerIoData.ClientSocket];
        if (Connection = nil) then
        begin
          Connection := AllocConnection(PerIoData.ClientSocket);
          FConnectionList[PerIoData.ClientSocket] := Connection;
        end;
      end;

      // 将Socket邦定到IOCP
      if not AssociateSocketWithCompletionPort(PerIoData.ClientSocket, Connection) then
      begin
        AppendLog(
          'RequestAcceptComplete.AssociateSocketWithCompletionPort failed, Socket=%d',
          [PerIoData.ClientSocket],
          ltWarning);
        Connection.Release;
        Exit;
      end;
    finally
      FConnectionListLocker.Leave;
    end;

    Connection.UpdateTick;

    // 对于SO_UPDATE_ACCEPT_CONTEXT,最后一个参数optlen实际需要设定为SizeOf(PAnsiChar)
    // 这一点在MSDN的例子中都是错的！因为经过实际测试发现在64位程序中这里传递SizeOf(PerIoData.ListenSocket)
    // 的话会报错：10014,系统检测到在一个调用中尝试使用指针参数时的无效指针地址。
    // 也就是说这里的optlen实际上应该传递的是一个指针的长度(应该跟内存对齐有关系)
    if (setsockopt(PerIoData.ClientSocket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
      PAnsiChar(@PerIoData.ListenSocket), SizeOf(PAnsiChar)) = SOCKET_ERROR) then
    begin
      AppendLog(
        '%s.RequestAcceptComplete.setsockopt.SO_UPDATE_ACCEPT_CONTEXT, Socket=%d, ERROR %d=%s',
        [ClassName, PerIoData.ClientSocket, WSAGetLastError, SysErrorMessage(WSAGetLastError)],
        ltWarning);
      Connection.Disconnect;
      Exit;
    end;

    // 获取连接地址信息
    GetAcceptExSockaddrs(@PerIoData.Buffer.AcceptExBuffer[0], 0, SizeOf(TAddrBuffer),
      SizeOf(TAddrBuffer), PLocalAddr, LocalAddrLen,
      PRemoteAddr, RemoteAddrLen);

    if not Connection.InitSocket then
    begin
      Connection.Disconnect;
      Exit;
    end;

    // 解析地址信息
    ExtractAddrInfo(PRemoteAddr, RemoteAddrLen, Connection.FRemoteIP, Connection.FRemotePort);

    // 超过最大允许连接数，断开
    if (FMaxClients > 0) and (FConnectionList.Count > FMaxClients) then
    begin
      Connection.Disconnect;
      Exit;
    end;

    if not _TriggerClientConnected(Connection) then Exit;

    // 连接建立之后PostZero读取请求
    if not Connection.PostReadZero then Exit;
  except
    on e: Exception do
      AppendLog('%s.RequestAcceptComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestConnectComplete(Connection: TIocpSocketConnection);
begin
  if FShutdown then Exit;

  try
    try
      if not Connection.InitSocket then
      begin
        Connection.Disconnect;
        Exit;
      end;

      try
        FConnectionListLocker.Enter;
        FConnectionList[Connection.FSocket] := Connection;
      finally
        FConnectionListLocker.Leave;
      end;

      Connection.UpdateTick;

      if not _TriggerClientConnected(Connection) then Exit;

      // 连接建立之后PostZero读取请求
      if not Connection.PostReadZero then Exit;
    finally
      Connection.Release; // 对应 AsyncConnect 中的 AddRef;
    end;
  except
    on e: Exception do
      AppendLog('%s.RequestConnectComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestReadZeroComplete(
  Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData);
begin
  try
    try
      if (Connection.IsClosed) then Exit;

      Connection.UpdateTick;

      // 正式开始接收数据
      Connection.PostRead;
    finally
      Connection.Release; // 对应PostReadZero中的AddRef
    end;
  except
    on e: Exception do
      AppendLog('%s.RequestReadZeroComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestReadComplete(Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData);
begin
  try
    try
      Connection.DecPendingRecv;

      if (Connection.IsClosed) then Exit;

      if (PerIoData.BytesTransfered = 0) or (PerIoData.Buffer.DataBuf.buf = nil) then
      begin
        Connection.Disconnect;
        Exit;
      end;

      Connection.UpdateTick;

      // PerIoData.Buffer.DataBuf 就是已接收到的数据，PerIoData.BytesTransfered 是实际接收到的字节数
      PerIoData.Buffer.DataBuf.Len := PerIoData.BytesTransfered;

      try
        InterlockedIncrement(FPendingRequest);
        if not _TriggerClientRecvData(Connection, PerIoData.Buffer.DataBuf.buf, PerIoData.Buffer.DataBuf.len) then Exit;
      finally
        InterlockedDecrement(FPendingRequest);
      end;

      // 继续接收客户端数据
      Connection.PostReadZero;
    finally
      Connection.Release; // 对应PostRead中的AddRef
    end;
  except
    on e: Exception do
      AppendLog('%s.RequestReadComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestWriteComplete(Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData);
begin
  try
    try
      Connection.DecPendingSend;

      if (Connection.IsClosed) then Exit;

      if (PerIoData.BytesTransfered = 0) or (PerIoData.Buffer.DataBuf.buf = nil) then
      begin
        Connection.Disconnect;
        Exit;
      end;

      Connection.UpdateTick;

      PerIoData.Buffer.DataBuf.Len := PerIoData.BytesTransfered;
      _TriggerClientSentData(Connection, PerIoData.Buffer.DataBuf.Buf, PerIoData.Buffer.DataBuf.Len);
    finally
      Connection.Release; // 对应PostWrite中的AddRef
    end;
  except
    on e: Exception do
      AppendLog('%s.RequestWriteComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.SetConnectionClass(
  const Value: TIocpSocketConnectionClass);
begin
  FConnectionPool.ObjectClass := Value;
end;

procedure TIocpTcpSocket.CloseSocket(Socket: TSocket);
//var
//	lingerStruct: TLinger;
begin
{	lingerStruct.l_onoff := 1;
 lingerStruct.l_linger := 0;
 setsockopt(Socket, SOL_SOCKET, SO_LINGER, PAnsiChar(@lingerStruct), SizeOf(lingerStruct));
//  CancelIo(Socket);}

  Iocp.Winsock2.shutdown(Socket, SD_BOTH);
  Iocp.Winsock2.closesocket(Socket);
end;

procedure TIocpTcpSocket.StartupWorkers;
var
  si: TSystemInfo;
  NumberOfThreads, i: Integer;
begin
  if (FIocpHandle <> 0) then Exit;

  FPendingRequest := 0;

  // 计算IO线程数
  if (FIoThreadsNumber <= 0) then
  begin
    GetSystemInfo(si);
    NumberOfThreads := si.dwNumberOfProcessors * 2;
  end
  else
    NumberOfThreads := Min(FIoThreadsNumber, 64);

  // 创建完成端口
  // NumberOfConcurrentThreads = 0 表示每个CPU保持一个并发线程
  FIocpHandle := Iocp.ApiFix.CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  if (FIocpHandle = INVALID_HANDLE_VALUE) then
    raise Exception.CreateFmt('%s.StartupWorkers创建IOCP对象失败', [ClassName]);

  // 创建IO线程
  SetLength(FIoThreads, NumberOfThreads);
  SetLength(FIoThreadHandles, NumberOfThreads);
  for i := 0 to NumberOfThreads - 1 do
  begin
    FIoThreads[i] := TIocpIoThread.Create(Self);
    FIoThreadHandles[i] := FIoThreads[i].Handle;
  end;

  {$IFDEF __TIME_OUT_TIMER__}
  // 创建时钟队列
  FTimerQueue := TIocpTimerQueue.Create;
  {$ENDIF}

  FSentBytes := 0;
  FRecvBytes := 0;
  FShutdown := False;
end;

procedure TIocpTcpSocket.ShutdownWorkers;
var
  i: Integer;
  LTick, LTimeout: LongWord;
begin
  FShutdown := True;
  if (FIocpHandle = 0) then Exit;

  // 断开所有连接
  DisconnectAll;

  {$IFDEF __TIME_OUT_TIMER__}
  // 释放时钟队列
  FTimerQueue.Release;
  {$ENDIF}

  {$IFDEF __TIME_OUT_TIMER__}
  if (FTimeout > 0) and (FTimeout < 5000) then
    LTimeout := FTimeout
  else
  {$ENDIF}
    LTimeout := 5000;

  // 这里必须加上Sleep，以保证所有断开连接的命令比后面退出线程的命令先进入IOCP队列
  // 否则可能会出现连接还没全部释放，线程就被终止了，造成资源泄漏
  LTick := GetTickCount;
  while ((FConnectionList.Count > 0) or (FIdleConnectionList.Count > 0)) do
  begin
    SleepEx(10, True);
    if (CalcTickDiff(LTick, GetTickCount) > LTimeout) then Break;
  end;

  // 关闭监听线程
  FListenThreadsLocker.Enter;
  try
    for i := 0 to FListenThreads.Count - 1 do
      TIocpAcceptThread(FListenThreads[i]).Quit;
    FListenThreads.Clear;
  finally
    FListenThreadsLocker.Leave;
  end;

  // 关闭IO线程
  for i := Low(FIoThreads) to High(FIoThreads) do
  begin
    Iocp.ApiFix.PostQueuedCompletionStatus(FIocpHandle, 0, 0, POverlapped(SHUTDOWN_FLAG));
    SleepEx(10, True);
  end;

  // 等待IO线程结束
  WaitForMultipleObjects(Length(FIoThreadHandles), Pointer(FIoThreadHandles), True, LTimeout);
  SetLength(FIoThreads, 0);
  SetLength(FIoThreadHandles, 0);

  // 关闭完成端口
  CloseHandle(FIocpHandle);
  FIocpHandle := 0;

  FConnectionList.Clear;
  FIdleConnectionList.Clear;
  FConnectionPool.Clear;
  FPerIoDataPool.Clear;

//  AppendLog('%s.shutdown compelte, ConnCount=%d, IdleConnCount=%d',
//    [ClassName, FConnectionList.Count, FIdleConnectionList.Count]);
end;

procedure TIocpTcpSocket.StopListen(ListenSocket: TSocket);
var
  i: Integer;
begin
  FListenThreadsLocker.Enter;
  try
    for i := 0 to FListenThreads.Count - 1 do
    begin
      if (ListenSocket = TIocpAcceptThread(FListenThreads[i]).ListenSocket) then
      begin
        TIocpAcceptThread(FListenThreads[i]).Quit;
        FListenThreads.Delete(i);
        Break;
      end;
    end;
  finally
    FListenThreadsLocker.Leave;
  end;
end;

function TIocpTcpSocket.TriggerClientConnected(
  Client: TIocpSocketConnection): Boolean;
begin
  if Assigned(FOnClientConnected) then
    Result := FOnClientConnected(Self, Client)
  else
    Result := True;
end;

function TIocpTcpSocket.TriggerClientDisconnected(
  Client: TIocpSocketConnection): Boolean;
begin
  if Assigned(FOnClientDisconnected) then
    Result := FOnClientDisconnected(Self, Client)
  else
    Result := True;
end;

function TIocpTcpSocket.TriggerClientRecvData(
  Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
begin
  if Assigned(FOnClientRecvData) then
    Result := FOnClientRecvData(Self, Client, Buf, Len)
  else
    Result := True;
end;

function TIocpTcpSocket.TriggerClientSentData(
  Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
begin
  if Assigned(FOnClientSentData) then
    Result := FOnClientSentData(Self, Client, Buf, Len)
  else
    Result := True;
end;

function TIocpTcpSocket._TriggerClientConnected(Client: TIocpSocketConnection): Boolean;
begin
  Result := TriggerClientConnected(Client);
end;

function TIocpTcpSocket._TriggerClientDisconnected(Client: TIocpSocketConnection): Boolean;
begin
  Result := TriggerClientDisconnected(Client);
end;

function TIocpTcpSocket._TriggerClientRecvData(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer): Boolean;
begin
  TInterlocked.Add(FRecvBytes, Len);
  Client._TriggerClientRecvData(Buf, Len);
  Result := TriggerClientRecvData(Client, Buf, Len);
end;

function TIocpTcpSocket._TriggerClientSentData(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer): Boolean;
begin
  TInterlocked.Add(FSentBytes, Len);
  Result := TriggerClientSentData(Client, Buf, Len);
  Client._TriggerClientSentData(Buf, Len);
end;

{ TIocpLineSocketConnection }

constructor TIocpLineSocketConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FLineText := TIocpStringStream.Create('');
end;

destructor TIocpLineSocketConnection.Destroy;
begin
  FLineText.Free;

  inherited Destroy;
end;

function TIocpLineSocketConnection.Send(const s: RawByteString): Integer;
begin
  Result := inherited Send(s + TIocpLineSocket(Owner).LineEndTag);
end;

{ TIocpLineSocket }

function TIocpLineSocket.Connect(const RemoteAddr: string; RemotePort: Word;
  Tag: Pointer; ConnectTimeout: DWORD): TIocpLineSocketConnection;
begin
  Result := TIocpLineSocketConnection(inherited Connect(RemoteAddr, RemotePort, Tag, ConnectTimeout));
end;

constructor TIocpLineSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpLineSocketConnection;
  FLineEndTag := #13#10;
  FLineLimit := 65536;
end;

procedure TIocpLineSocket.DoOnRecvLine(Client: TIocpLineSocketConnection;
  Line: RawByteString);
begin
  if Assigned(FOnRecvLine) then
    FOnRecvLine(Self, Client, Line);
end;

procedure TIocpLineSocket.ParseRecvData(Client: TIocpLineSocketConnection;
  Buf: Pointer; Len: Integer);
var
  pch: PAnsiChar;
  Ch: AnsiChar;
  TagLen: Integer;
begin
  pch := Buf;
  TagLen := Length(FLineEndTag);
  while (Len > 0) do
  begin
    Ch := pch^;

    // 发现换行符
    if (TagLen > 0) and (Len >= TagLen) and (StrLIComp(pch, PAnsiChar(FLineEndTag), TagLen) = 0) then
    begin
      if (Client.LineText.Size > 0) then
      begin
        DoOnRecvLine(Client, Client.LineText.DataString);
        Client.LineText.Clear;
      end;
      Dec(Len, TagLen);
      Inc(pch, TagLen);
      Continue;
    end;

    Client.LineText.Write(Ch, 1);

    // 超出最大单行尺寸
    if (FLineLimit > 0) and (Client.LineText.Size >= FLineLimit) then
    begin
      DoOnRecvLine(Client, Client.LineText.DataString);
      Client.LineText.Clear;
    end;

    Dec(Len, SizeOf(Ch));
    Inc(pch);
  end;
end;

procedure TIocpLineSocket.SetLineEndTag(const Value: RawByteString);
begin
  if (Value <> '') then
    FLineEndTag := Value
  else
    FLineEndTag := #13#10;
end;

function TIocpLineSocket.TriggerClientRecvData(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer): Boolean;
begin
  ParseRecvData(TIocpLineSocketConnection(Client), Buf, Len);
  Result := True;
end;

{ TIocpLineServer }

constructor TIocpLineServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FListened := False;
end;

function TIocpLineServer.Start: Boolean;
begin
  if FListened then Exit(True);

  StartupWorkers;
  FListened := inherited Listen(FAddr, FPort, 1);
  Result := FListened;
end;

function TIocpLineServer.Stop: Boolean;
begin
  if not FListened then Exit(True);

  ShutdownWorkers;
  FListened := False;
  Result := True;
end;

{ TSimpleIocpTcpClient }

function TSimpleIocpTcpClient.AsyncConnect(Tag: Pointer): TSocket;
begin
  Result := inherited AsyncConnect(FServerAddr, FServerPort, Tag);
end;

function TSimpleIocpTcpClient.Connect(Tag: Pointer;
  ConnectTimeout: DWORD): TIocpSocketConnection;
begin
  Result := inherited Connect(FServerAddr, FServerPort, Tag, ConnectTimeout);
end;

{ TSimpleIocpTcpServer }

constructor TSimpleIocpTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListened := False;

  FAddr := '';
  FInitAcceptNum := INIT_ACCEPTEX_NUM;
  FStartTick := 0;
end;

destructor TSimpleIocpTcpServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TSimpleIocpTcpServer.SetActive(const Value: Boolean);
begin
  if (FActive = Value) then Exit;
  
  FActive := Value;
  if FActive then
    Start
  else
    Stop;
end;

function TSimpleIocpTcpServer.Start: Boolean;
begin
  if FListened then Exit(True);

  StartupWorkers;
  FListened := inherited Listen(FAddr, FPort, FInitAcceptNum);
  Result := FListened;
  if Result then
    FStartTick := GetTickCount;
end;

function TSimpleIocpTcpServer.Stop: Boolean;
begin
  if not FListened then Exit(True);

  ShutdownWorkers;
  FListened := False;
  Result := True;
  FStartTick := 0;
end;

initialization
  Iocp.Winsock2.InitializeWinSock;
  Iocp.Wship6.InitLibrary;

  IoCachePool := TIocpMemoryPool.Create(NET_CACHE_SIZE, MAX_FREE_IO_DATA_BLOCKS);
  FileCachePool := TIocpMemoryPool.Create(FILE_CACHE_SIZE, MAX_FREE_HANDLE_DATA_BLOCKS);

finalization
  IoCachePool.Clear;
  IoCachePool.Release;
  FileCachePool.Clear;
  FileCachePool.Release;

  Iocp.Winsock2.UninitializeWinSock;

end.

