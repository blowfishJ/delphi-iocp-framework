unit IocpTcpSocket;

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

// 智能发送队列，当待发送的数据不超过发送缓冲区大小，并且队列中没有未发完的数据时
// 直接发送数据，不进入队列
{$DEFINE __SMART_SND_QUEUE__}

interface

uses
  Windows, Messages, Classes, SysUtils, SyncObjs, Math, Contnrs,
  JwaWinsock2, JwaWS2tcpip, JwaMSWSock, WinsockEx{, cDataStructs},
  IocpApiFix, IocpThreadPool, IocpReadWriteLocker, IocpMemoryPool,
  IocpObjectPool, IocpBuffer, IocpQueue, IocpTimerQueue, IocpLogger,
  VaniFunc;

const
  SHUTDOWN_FLAG = ULONG_PTR(-1);

  MAX_FREE_HANDLE_DATA_BLOCKS = 512;
  MAX_FREE_IO_DATA_BLOCKS = MAX_FREE_HANDLE_DATA_BLOCKS * 2;
  INIT_ACCEPTEX_NUM = 64;
  NET_CACHE_SIZE = 4 * 1024;
  FILE_CACHE_SIZE = 64 * 1024;

type
  EIocpTcpException = class(Exception);

  TSimpleIocpTcpServer = class;
  TIocpTcpSocket = class;
  TIocpSocketConnection = class;

  TIocpOperationType = (iotReadZero, iotRead, iotWrite, iotAccept, iotConnect);

  {
    *** 发送队列数据块结构 ***
    用于保存被拆分后的内存块
  }
  PIocpIoBlock = ^TIocpIoBlock;
  TIocpIoBlock = record
    Buf: Pointer;
    Size: Integer;
  end;

  {
    *** 发送队列 ***
    将待发送内存块根据内存缓冲池块大小拆分后,保存到队列中
  }
  TIocpSendQueue = class
  private
    FLocker: TCriticalSection;
    FIocpQueue: TIocpPointerQueue;
    FOwner: TIocpSocketConnection;

    function GetCount: Integer;
    function UnsafePushBuffer(Buf: Pointer; Size: Integer): Boolean;
    function UnsafePopBuffer(out Buf: Pointer; out Size: Integer): Boolean;
  protected
    function Push(p: Pointer): Boolean; virtual;
    function Pop(out p: Pointer): Boolean; virtual;
  public
    constructor Create(Owner: TIocpSocketConnection); virtual;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function PushBuffer(Buf: Pointer; Size: Integer): Boolean;
    function PopBuffer(out Buf: Pointer; out Size: Integer): Boolean;
    procedure Clear;

    property Count: Integer read GetCount;
  end;

  {
     *** Socket 连接 ***
  }
  TIocpSocketConnection = class(TIocpObject)
  private
    FSocket: TSocket;
    FRemoteAddr: TSockAddr;
    FRemoteIP: AnsiString;
    FRemotePort: Word;

    FLocker: TCriticalSection;
    FRefCount: Integer;
    FDisconnected: Integer;
    FLastTick: DWORD;
    FTag: Pointer;
    FSndBufSize, FRcvBufSize: Integer;
    FRcvBuffer: array[0..NET_CACHE_SIZE - 1] of Byte;
    FPendingSendCount: Integer;
    FPendingRecvCount: Integer;
    FSndQueue: TIocpSendQueue;
    FTimer: TIocpTimerQueueTimer;
    FTimeout: DWORD;

    function GetRefCount: Integer;
    function GetIsClosed: Boolean;
    function GetOwner: TIocpTcpSocket;

    function InitSocket: Boolean;
    procedure ResetBuffer;
    procedure UpdateTick;

    procedure IncPendingRecvCount;
    procedure DecPendingRecvCount;
    function PostReadZero: Boolean;
    function PostRead: Boolean;

    procedure IncPendingSendCount;
    procedure DecPendingSendCount;
    function PostWrite(const Buf: Pointer; Size: Integer): Boolean;
    function SendFromQueue: Boolean;

    // 连接超时检查
    procedure OnTimer(Sender: TObject);
  protected
    procedure Initialize; override;
    procedure Finalize; override;

    function TriggerReadComplete(Buf: Pointer; Len: Integer): Boolean; virtual;
    function TriggerWriteComplete(Buf: Pointer; Len: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function AddRef: Integer;
    function Release: Boolean;
    procedure Disconnect;

    // 纯异步发送
    function Send(const Buf: Pointer; Size: Integer): Integer; overload;
    function Send(const s: string): Integer; overload;
{$IF COMPILERVERSION >= 20}
    function Send(const s: AnsiString): Integer; overload;
{$IFEND}
    function Send(Stream: TStream): Integer; overload;

    property Owner: TIocpTcpSocket read GetOwner;
    property Socket: TSocket read FSocket;
    property RefCount: Integer read GetRefCount;
    property LastTick: DWORD read FLastTick;
    property Tag: Pointer read FTag write FTag;

    property PeerIP: AnsiString read FRemoteIP;
    property PeerAddr: AnsiString read FRemoteIP;
    property PeerPort: Word read FRemotePort;
    property IsClosed: Boolean read GetIsClosed;
    property SndBufSize: Integer read FSndBufSize;
    property RcvBufSize: Integer read FRcvBufSize;
    property PendingSendCount: Integer read FPendingSendCount;
    property PendingRecvCount: Integer read FPendingRecvCount;
    property Timeout: DWORD read FTimeout write FTimeout;
  end;

  TIocpSocketConnectionClass = class of TIocpSocketConnection;

  TPerIoBufUnion = record
    case Integer of
      0: (DataBuf: WSABUF);
      // 这个Buffer只用于AcceptEx保存终端地址数据，大小为2倍地址结构
      1: (AcceptExBuffer: array[0..(SizeOf(TSockAddrIn) + 16) * 2 - 1] of Byte);
  end;

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

  {
    *** Socket连接列表 ***
  }
  TIocpSocketConnectionList = class
  private
    FOwner: TIocpTcpSocket;
    FConnectionList: TSparseObjectArray;

    function GetItem(Socket: TSocket): TIocpSocketConnection;
    procedure SetItem(Socket: TSocket; const Value: TIocpSocketConnection);
    function GetCount: Integer;
  public
    constructor Create(AOwner: TIocpTcpSocket); virtual;
    destructor Destroy; override;

    procedure Assign(const Source: TIocpSocketConnectionList);
    procedure Clear;
    function Delete(Socket: TSocket): Boolean;
    function FindFirst(var Socket: TSocket; var Value: TIocpSocketConnection): Boolean;
    function FindNext(var Socket: TSocket; var Value: TIocpSocketConnection): Boolean;

    property Item[Socket: TSocket]: TIocpSocketConnection read GetItem write SetItem; default;
    property Count: Integer read GetCount;
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
    FShutdownEvent: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(IocpSocket: TIocpTcpSocket; ListenSocket: TSocket; InitAcceptNum: Integer); reintroduce;

    procedure Quit;
    property ListenSocket: TSocket read FListenSocket;
  end;

  TIocpTcpSocketWorkItem = class(TWorkItem)
  private
    Client: TIocpSocketConnection;
    Buf: Pointer;
    Len: Integer;
  public
    constructor Create(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer);
    destructor Destroy; override;
  end;

  TIocpTcpSocketThreadPool = class(TThreadsPool)
  private
    IocpTcpSocket: TIocpTcpSocket;
    ThreadsNumber: Integer;
  protected
    procedure DoProcessRequest(aDataObj: TWorkItem; aThread: TProcessorThread); override;
  public
    constructor Create(IocpTcpSocket: TIocpTcpSocket; ThreadsNumber: Integer); reintroduce;
  end;

  {
    *** 主要的Socket实现类 ***
  }
  TIocpTcpSocket = class(TComponent)
  private
    FIocpHandle: THandle;
    FIoThreadsNumber: Integer;
    FIoThreads: array of TIocpIoThread;
    FIoThreadHandles: array of THandle;
    FLogicThreadsPool: TIocpTcpSocketThreadPool;
    FLogicThreadsNumber: Integer;
    FConnectionPool: TIocpObjectPool;
    FPerIoDataPool: TIocpMemoryPool;
    FConnectionList, FIdleConnectionList: TIocpSocketConnectionList;
    FConnectionListLocker: TCriticalSection;
    FListenThreads: TList;
    FListenThreadsLocker: TCriticalSection;
    FTimerQueue: TIocpTimerQueue;
    FTimeout: DWORD;

    procedure ProcessRequest(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData; aThread: TIocpIoThread); virtual;
    procedure ExtractAddrInfo(const Addr: TSockAddr; out IP: AnsiString; out Port: Word);
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
    function PostNewAcceptEx(ListenSocket: TSocket): Boolean;
  protected
    function ProcessMessage: Boolean;
    procedure MessagePump;

    procedure StartupWorkers;
    procedure ShutdownWorkers;

    procedure RequestAcceptComplete(PerIoData: PIocpPerIoData); virtual;
    procedure RequestConnectComplete(Connection: TIocpSocketConnection); virtual;
    procedure RequestReadZeroComplete(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData); virtual;
    procedure RequestReadComplete(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData); virtual;
    procedure RequestWriteComplete(Connection: TIocpSocketConnection; PerIoData: PIocpPerIoData); virtual;

    // 连接建立时触发
    function TriggerConnectComplete(Connection: TIocpSocketConnection): Boolean;

    // 连接断开时触发
    function TriggerDisconnectComplete(Connection: TIocpSocketConnection): Boolean;

    // 接收到数据时触发，该事件将在逻辑线程池中执行，
    // Connection 由引用计数保护，无需担心在逻辑处理过程中会被其它线程释放；
    // buf是从接受的数据完整复制的，可以安全使用，并且在逻辑线程处理完之后会自动释放
    function TriggerReadComplete(Connection: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;

    // 发送数据完成时触发
    function TriggerWriteComplete(Connection: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;

    // 重载下面几个方法可以实现在IO事件触发时做相应处理
    function TriggerClientConnected(Client: TIocpSocketConnection): Boolean; virtual;
    function TriggerClientDisconnected(Client: TIocpSocketConnection): Boolean; virtual;
    function TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; virtual;
    function TriggerClientSentData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; IoThreadsNumber: Integer); reintroduce; overload;
    destructor Destroy; override;

    function Listen(const Host: AnsiString; Port: Word; InitAcceptNum: Integer): TSocket; overload;
    function Listen(Port: Word; Concurrent: Integer): TSocket; overload;
    procedure StopListen(ListenSocket: TSocket);
    procedure CloseSocket(Socket: TSocket);
    function AsyncConnect(const RemoteAddr: AnsiString; RemotePort: Word): TSocket;
    function Connect(const RemoteAddr: AnsiString; RemotePort: Word; ConnectTimeout: DWORD = 10000): TIocpSocketConnection;
    procedure DisconnectAll;
    function IsClient(SomeThing: TObject): Boolean;

    function LockConnectionList: TIocpSocketConnectionList;
    procedure UnlockConnectionList;

    property ConnectionClass: TIocpSocketConnectionClass read GetConnectionClass write SetConnectionClass;
    property ConnectionList: TIocpSocketConnectionList read FConnectionList;
    property ConnectionUsedMemory: Integer read GetConnectionUsedMemory;
    property ConnectionFreeMemory: Integer read GetConnectionFreeMemory;
    property PerIoDataPool: TIocpMemoryPool read FPerIoDataPool;
    property PerIoUsedMemory: Integer read GetPerIoUsedMemory;
    property PerIoFreeMemory: Integer read GetPerIoFreeMemory;
    property IoCacheUsedMemory: Integer read GetIoCacheUsedMemory;
    property IoCacheFreeMemory: Integer read GetIoCacheFreeMemory;
    property Timeout: DWORD read FTimeout write FTimeout default 10000;
  end;

  TIocpLineSocketConnection = class(TIocpSocketConnection)
  private
    FLineText: TIocpStringStream;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function Send(const s: AnsiString): Integer; reintroduce;

    property LineText: TIocpStringStream read FLineText;
  end;

  TIocpLineSocket = class(TIocpTcpSocket)
  private
    FLineLimit: Integer;
    FLineEndTag: AnsiString;
    procedure SetLineEndTag(const Value: AnsiString);
  protected
    function TriggerClientRecvData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; override;

    procedure ParseRecvData(Client: TIocpLineSocketConnection; Buf: Pointer; Len: Integer); virtual;

    // 重载这个方法，在里面处理接收到的文本行
    procedure DoOnRecvLine(Client: TIocpLineSocketConnection; Line: AnsiString); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    function Connect(const RemoteAddr: AnsiString; RemotePort: Word; ConnectTimeout: DWORD = 10000): TIocpLineSocketConnection;
  published
    property LineEndTag: AnsiString read FLineEndTag write SetLineEndTag;
    property LineLimit: Integer read FLineLimit write FLineLimit default 655636;
  end;

  TSimpleIocpSocketConnection = class(TIocpSocketConnection)
  private
    FRequestDoneEvent: THandle;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function IsIdle: Boolean;
  end;

  TSimpleIocpTcpClient = class(TIocpTcpSocket)
  private
    FRemoteAddr: AnsiString;
    FRemotePort: Word;

    procedure SetRemoteAddr(const Value: AnsiString);
    procedure SetRemotePort(const Value: Word);
  protected
    function TriggerClientSentData(Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean; override;
  protected
    function GetIdleConnectionAndLock: TSimpleIocpSocketConnection;
  public
    constructor Create(AOwner: TComponent); override;

    function Send(Buf: Pointer; Size: Integer): Integer; overload;
    function Send(const s: AnsiString): Integer; overload;

    property RemoteAddr: AnsiString read FRemoteAddr write SetRemoteAddr;
    property RemotePort: Word read FRemotePort write SetRemotePort;
  end;

  TSimpleIocpTcpServer = class(TIocpTcpSocket)
  private
    FAddr: AnsiString;
    FPort: Word;
    FListenSocket: TSocket;
    FInitAcceptNum: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Start: Boolean;
    function Stop: Boolean;
  published
    property Addr: AnsiString read FAddr write FAddr;
    property Port: Word read FPort write FPort;
    property InitAcceptNum: Integer read FInitAcceptNum write FInitAcceptNum default INIT_ACCEPTEX_NUM;
  end;

implementation

var
  IoCachePool, FileCachePool: TIocpMemoryPool;

{ TIocpSendQueue }

procedure TIocpSendQueue.Clear;
var
  Buf: Pointer;
  Size: Integer;
begin
  try
    Lock;
    while UnsafePopBuffer(Buf, Size) do
      IoCachePool.FreeMemory(Buf);
  finally
    Unlock;
  end;
end;

constructor TIocpSendQueue.Create(Owner: TIocpSocketConnection);
begin
  FLocker := TCriticalSection.Create;
  FIocpQueue := TIocpPointerQueue.Create;
  FOwner := Owner;
end;

destructor TIocpSendQueue.Destroy;
begin
  Clear;
  FIocpQueue.Free;
  FLocker.Free;

  inherited Destroy;
end;

function TIocpSendQueue.GetCount: Integer;
begin
  Result := FIocpQueue.Count;
end;

procedure TIocpSendQueue.Lock;
begin
  FLocker.Enter;
end;

function TIocpSendQueue.Pop(out p: Pointer): Boolean;
begin
  Result := FIocpQueue.Pop(p);
end;

function TIocpSendQueue.PopBuffer(out Buf: Pointer; out Size: Integer): Boolean;
begin
  try
    Lock;
    Result := UnsafePopBuffer(Buf, Size);
  finally
    Unlock;
  end;
end;

function TIocpSendQueue.Push(p: Pointer): Boolean;
begin
  Result := FIocpQueue.Push(p);
end;

function TIocpSendQueue.PushBuffer(Buf: Pointer; Size: Integer): Boolean;
begin
  try
    Lock;
    Result := UnsafePushBuffer(Buf, Size);
  finally
    Unlock;
  end;
end;

procedure TIocpSendQueue.Unlock;
begin
  FLocker.Leave;
end;

function TIocpSendQueue.UnsafePopBuffer(out Buf: Pointer;
  out Size: Integer): Boolean;
var
  PBlock: PIocpIoBlock;
begin
  if Pop(Pointer(PBlock)) then
  begin
    Buf := PBlock.Buf;
    Size := PBlock.Size;
    System.Dispose(PBlock);
    Result := (Buf <> nil);
  end
  else
  begin
    Buf := nil;
    Size := -1;
    Result := False;
  end;
end;

function TIocpSendQueue.UnsafePushBuffer(Buf: Pointer; Size: Integer): Boolean;
var
  BlockSize: Integer;
  PBlock: PIocpIoBlock;
begin
  if (Buf = nil) or (Size <= 0) then
  begin
    Result := False;
    Exit;
  end;
  while (Size > 0) do
  begin
    BlockSize := Min(Size, IoCachePool.BlockSize);
    New(PBlock);
    PBlock.Buf := IoCachePool.GetMemory;
    Move(Buf^, PBlock.Buf^, BlockSize);
    PBlock.Size := BlockSize;
    if not Push(PBlock) then Break;

    Dec(Size, BlockSize);
    Inc(PByte(Buf), BlockSize);
  end;
  Result := True;
end;

{ TIocpSocketConnection }

function TIocpSocketConnection.AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

constructor TIocpSocketConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FLocker := TCriticalSection.Create;
  FSndQueue := TIocpSendQueue.Create(Self);
end;

function TIocpSocketConnection.Release: Boolean;
begin
  Result := (InterlockedDecrement(FRefCount) = 0);
  if not Result then Exit;

  // 如果Socket还没关闭，则需要关闭，否则会造成句柄泄露
  // 这种情况发生在远端主动断开连接
  if (InterlockedExchange(FDisconnected, 1) = 0) then
    Owner.CloseSocket(FSocket);

  Owner.TriggerDisconnectComplete(Self);
  Owner.FreeConnection(Self);
end;

procedure TIocpSocketConnection.DecPendingRecvCount;
begin
  InterlockedDecrement(FPendingRecvCount);
end;

procedure TIocpSocketConnection.DecPendingSendCount;
begin
  InterlockedDecrement(FPendingSendCount);
end;

destructor TIocpSocketConnection.Destroy;
begin
  FreeAndNil(FSndQueue);
  FLocker.Free;

  inherited Destroy;
end;

procedure TIocpSocketConnection.Disconnect;
begin
  if (InterlockedExchange(FDisconnected, 1) <> 0) then Exit;

  Owner.CloseSocket(FSocket);
  Release;
end;

procedure TIocpSocketConnection.Finalize;
begin
  FTimer.Release;
end;

function TIocpSocketConnection.GetIsClosed: Boolean;
begin
  Result := (InterlockedExchange(FDisconnected, FDisconnected) = 1);
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
    AppendLog('%s.InitSocket.setsockopt.SO_SNDBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    Exit;
  end;
  FSndBufSize := IoCachePool.BlockSize;
{$ELSE}
  OptLen := SizeOf(FSndBufSize);
  if (getsockopt(FSocket, SOL_SOCKET, SO_SNDBUF,
    PAnsiChar(@FSndBufSize), OptLen) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.getsockopt.SO_SNDBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    Exit;
  end;
{$ENDIF}

{$IFDEF __TCP_RCVBUF_ZERO_COPY__}
  BufSize := 0;
  if (setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF,
    PAnsiChar(@BufSize), SizeOf(BufSize)) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.setsockopt.SO_RCVBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    Exit;
  end;
  FRcvBufSize := Length(FRcvBuffer);
{$ELSE}
  OptLen := SizeOf(FRcvBufSize);
  if (getsockopt(FSocket, SOL_SOCKET, SO_RCVBUF,
    PAnsiChar(@FRcvBufSize), OptLen) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.getsockopt.SO_RCVBUF ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    Exit;
  end;
{$ENDIF}

{$IFDEF __TCP_NODELAY__}
  NagleValue := 1;
  if (setsockopt(FSocket, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@NagleValue), SizeOf(Byte)) = SOCKET_ERROR) then
  begin
    AppendLog('%s.InitSocket.setsockopt.TCP_NODELAY ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    Exit;
  end;
{$ENDIF}

  Result := True;
end;

procedure TIocpSocketConnection.IncPendingRecvCount;
begin
  InterlockedIncrement(FPendingRecvCount);
end;

procedure TIocpSocketConnection.IncPendingSendCount;
begin
  InterlockedIncrement(FPendingSendCount);
end;

procedure TIocpSocketConnection.Initialize;
begin
  FSocket := 0;
  FRefCount := 1; // 置初始引用计数 1
  FPendingSendCount := 0;
  FPendingRecvCount := 0;
  FDisconnected := 0;
  FLastTick := 0;
  FTag := nil;
  FTimeout := 0;

  ZeroMemory(@FRemoteAddr, SizeOf(TSockAddrIn));
  FRemoteIP := '';
  FRemotePort := 0;

  ResetBuffer;

  FTimer := TIocpTimerQueueTimer.Create(Owner.FTimerQueue, 1000);
  FTimer.OnTimer := OnTimer;
end;

function TIocpSocketConnection.Send(const Buf: Pointer; Size: Integer): Integer;
begin
  Result := -1;
  if IsClosed then Exit;

  try
    Lock;
{$IFDEF __SMART_SND_QUEUE__}
    if (FPendingSendCount = 0) and (Size <= FSndBufSize) then
    begin
      if not PostWrite(Buf, Size) then Exit;
    end
    else
    begin
      if not FSndQueue.PushBuffer(Buf, Size) then Exit;
      if not SendFromQueue then Exit;
    end;
{$ELSE}
    if not FSndQueue.PushBuffer(Buf, Size) then Exit;
    if not SendFromQueue then Exit;
{$ENDIF}

    Result := Size;
  finally
    Unlock;
  end;
end;

{$IF COMPILERVERSION >= 20}

function TIocpSocketConnection.Send(const s: string): Integer;
var
  RawData: RawByteString;
begin
  RawData := RawByteString(s);
  Result := Send(@RawData[1], Length(RawData) * SizeOf(AnsiChar));
end;

function TIocpSocketConnection.Send(const s: AnsiString): Integer;
begin
  Result := Send(@s[1], Length(s) * SizeOf(AnsiChar));
end;

{$ELSE}

function TIocpSocketConnection.Send(const s: string): Integer;
begin
  Result := Send(@s[1], Length(s) * SizeOf(AnsiChar));
end;

{$IFEND}

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

      if (Send(Buf, BlockSize) < 0) then
      begin
        Result := -1;
        Exit;
      end;
    end;

    Result := Stream.Size;
  finally
    FileCachePool.FreeMemory(Buf);
  end;
end;

function TIocpSocketConnection.SendFromQueue: Boolean;
var
  SndBuf: Pointer;
  SndSize: Integer;
begin
  if FSndQueue.PopBuffer(SndBuf, SndSize) and (SndBuf <> nil) then
  begin
    Result := PostWrite(SndBuf, SndSize);
    IoCachePool.FreeMemory(SndBuf);
  end
  else
  // Send中Push之后很有可能马上就被发送线程Pop走了，所以在Send中调用SendFromQueue时
  // 很有可能队列中已经没有数据了，这种情况应该返回True
    Result := True;
end;

function TIocpSocketConnection.TriggerReadComplete(Buf: Pointer; Len: Integer): Boolean;
begin
  Result := True;
end;

function TIocpSocketConnection.TriggerWriteComplete(Buf: Pointer;
  Len: Integer): Boolean;
begin
  Result := SendFromQueue;
end;

procedure TIocpSocketConnection.Lock;
begin
  FLocker.Enter;
end;

procedure TIocpSocketConnection.OnTimer(Sender: TObject);
begin
// *****
// 这里Connection的同步还是会有问题，OnTimer实际上是在时钟队列的线程中执行的
// 在使用5000个连接不停连接/断开的过程中，客户端还是有可能触发异常
// 还得再考虑考虑，这里暂时屏蔽

// 解决方法，初步设想：
// Connection.Initialize中创建Timer的时候Connection.AddRef
// 释放Timer的时候Connection.Release
// 这样就能保证Timer执行过程中Connection一直有效
// 但是这里出现了另一个问题，那就是Timer的释放实际上是在Connection.Finalize中才会触发
// 而Connection.Finalize只有在计数器减到0时才会触发，这岂不就造成死锁了，还得考虑考虑，
// 不过这个思路已经离成功不远了，加油~
  try
    // 超时没有数据传输,断开连接
//    if (FTimeout > 0) and (FLastTick > 0) and (CalcTickDiff(FLastTick, GetTickCount) > FTimeout) then
//      Disconnect;
  except
  end;
end;

function TIocpSocketConnection.PostReadZero: Boolean;
var
  PerIoData: PIocpPerIoData;
  Bytes, Flags: Cardinal;
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
    AppendLog('%s.Socket%d PostReadZero.WSARecv ERROR %d=%s', [ClassName, FSocket, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
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
begin
  Result := False;
  if IsClosed then Exit;

  // 增加引用计数
  // 如果返回1则说明现在正在关闭连接
  if (AddRef = 1) then Exit;

  IncPendingRecvCount;

  PerIoData := Owner.AllocIoData(FSocket, iotRead);
  PerIoData.Buffer.DataBuf.Buf := @FRcvBuffer[0];
  PerIoData.Buffer.DataBuf.Len := Length(FRcvBuffer);
  Flags := 0;
  Bytes := 0;
  if (WSARecv(PerIoData.ClientSocket, @PerIoData.Buffer.DataBuf, 1, Bytes, Flags, PWSAOverlapped(PerIoData), nil) = SOCKET_ERROR)
    and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    AppendLog('%s.Socket%d PostRead.WSARecv ERROR %d=%s', [ClassName, FSocket, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    DecPendingRecvCount;
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
begin
  Result := False;
  if IsClosed then Exit;

  // 增加引用计数
  // 如果返回1则说明现在正在关闭连接
  if (AddRef = 1) then Exit;
  IncPendingSendCount;

  PerIoData := Owner.AllocIoData(FSocket, iotWrite);
  PerIoData.Buffer.DataBuf.Buf := Buf;
  PerIoData.Buffer.DataBuf.Len := Size;

  // WSAEFAULT(10014)
  // The lpBuffers, lpNumberOfBytesSent, lpOverlapped, lpCompletionRoutine parameter
  // is not totally contained in a valid part of the user address space.
  if (WSASend(FSocket, @PerIoData.Buffer.DataBuf, 1, Bytes, 0, PWSAOverlapped(PerIoData), nil) = SOCKET_ERROR)
    and (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    AppendLog('%s.Socket%d PostWrite.WSASend error, ERR=%d,%s', [ClassName, FSocket, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
    DecPendingSendCount;
    Release; // 对应函数开头的 AddRef
    Disconnect; // 对应连接初始化时的 FRefCount := 1
    Owner.FreeIoData(PerIoData);
    Exit;
  end;

  Result := True;
end;

procedure TIocpSocketConnection.ResetBuffer;
begin
  if Assigned(FSndQueue) then
    FSndQueue.Clear;
end;

procedure TIocpSocketConnection.Unlock;
begin
  FLocker.Leave;
end;

procedure TIocpSocketConnection.UpdateTick;
begin
  FLastTick := GetTickCount;
end;

{ TIocpSocketConnectionList }

constructor TIocpSocketConnectionList.Create(AOwner: TIocpTcpSocket);
begin
  FOwner := AOwner;
  FConnectionList := TSparseObjectArray.Create(False);
end;

function TIocpSocketConnectionList.Delete(Socket: TSocket): Boolean;
var
  Value: TIocpSocketConnection;
begin
  if FConnectionList.LocateItem(Socket, TObject(Value)) and (Value <> nil) then
  begin
    FConnectionList.Delete(Socket);
    Result := True;
  end
  else
    Result := False;
end;

destructor TIocpSocketConnectionList.Destroy;
begin
  FConnectionList.Free;

  inherited Destroy;
end;

procedure TIocpSocketConnectionList.Assign(const Source: TIocpSocketConnectionList);
begin
  FOwner := Source.FOwner;
  FConnectionList.Assign(Source.FConnectionList);
end;

procedure TIocpSocketConnectionList.Clear;
begin
  FConnectionList.Clear;
end;

function TIocpSocketConnectionList.FindFirst(var Socket: TSocket;
  var Value: TIocpSocketConnection): Boolean;
begin
  Result := FConnectionList.FindFirst(Integer(Socket), TObject(Value));
end;

function TIocpSocketConnectionList.FindNext(var Socket: TSocket;
  var Value: TIocpSocketConnection): Boolean;
begin
  Result := FConnectionList.FindNext(Integer(Socket), TObject(Value));
end;

function TIocpSocketConnectionList.GetCount: Integer;
begin
  Result := FConnectionList.Count;
end;

function TIocpSocketConnectionList.GetItem(Socket: TSocket): TIocpSocketConnection;
begin
  FConnectionList.LocateItem(Socket, TObject(Result));
end;

procedure TIocpSocketConnectionList.SetItem(Socket: TSocket;
  const Value: TIocpSocketConnection);
begin
  FConnectionList[Socket] := Value;
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
var
  IocpStatusOk: Boolean;
  BytesTransferred: Cardinal;
  Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData;
begin
  AppendLog('%s.Thread %d start', [FOwner.ClassName, ThreadID]);
  while not Terminated do
    try
      IocpStatusOk := IocpApiFix.GetQueuedCompletionStatus(FOwner.FIocpHandle,
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
          AppendLog('%s 线程ID %d 出错. ERR %d=%s', [FOwner.ClassName, ThreadID, GetLastError, SysErrorMessage(GetLastError)]);
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

      if (BytesTransferred = 0) and (ULONG_PTR(PerIoData) = SHUTDOWN_FLAG) then
      begin
        AppendLog('%s.Thread %d 收到退出标记', [FOwner.ClassName, GetCurrentThreadID]);
        Break;
      end;

      if (Connection = nil) and (PerIoData = nil) then Continue;

      PerIoData.BytesTransfered := BytesTransferred;
      FOwner.ProcessRequest(Connection, PerIoData, Self);
    except
      on e: Exception do
        AppendLog('TIocpIoThread.Execute, %s=%s', [e.ClassName, e.Message], ltException);
    end;

  AppendLog('%s.Thread %d exit', [FOwner.ClassName, ThreadID]);
end;

{ TIocpAcceptThread }

constructor TIocpAcceptThread.Create(IocpSocket: TIocpTcpSocket;
  ListenSocket: TSocket; InitAcceptNum: Integer);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FOwner := IocpSocket;
  FInitAcceptNum := InitAcceptNum;
  FListenSocket := ListenSocket;
  FShutdownEvent := CreateEvent(nil, True, False, nil);

  Suspended := False;
end;

procedure TIocpAcceptThread.Execute;
var
  i: Integer;
  LastError: Integer;
  AcceptEvents: array[0..1] of THandle;
  RetEvents: TWSANetworkEvents;
  dwRet: DWORD;
begin
  try
    for i := 1 to FInitAcceptNum do
      FOwner.PostNewAcceptEx(FListenSocket);

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
          LastError := WSAGetLastError;
          AppendLog('%s.WSAEnumNetworkEvents失败, ERROR %d=%s', [ClassName, LastError, SysErrorMessage(LastError)]);
          Break;
        end;

        // 如果ACCEPT事件触发，则投递新的Accept套接字
        // 每次投递32个
        if (RetEvents.lNetworkEvents and FD_ACCEPT = FD_ACCEPT) then
        begin
          if (RetEvents.iErrorCode[FD_ACCEPT_BIT] <> 0) then
          begin
            LastError := WSAGetLastError;
            AppendLog('%s.WSAEnumNetworkEvents失败, ERROR %d=%s', [ClassName, LastError, SysErrorMessage(LastError)]);
            Break;
          end;

          for i := 1 to 32 do
          begin
            if not FOwner.PostNewAcceptEx(FListenSocket) then Break;
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
end;

procedure TIocpAcceptThread.Quit;
begin
  SetEvent(FShutdownEvent);
end;

{ TIocpTcpSocketWorkItem }

constructor TIocpTcpSocketWorkItem.Create(Client: TIocpSocketConnection;
  Buf: Pointer; Len: Integer);
begin
  Self.Client := Client;

  if (Buf <> nil) and (Len > 0) then
  begin
    GetMem(Self.Buf, Len);
    Self.Len := Len;
    CopyMemory(Self.Buf, Buf, Len);
  end;
end;

destructor TIocpTcpSocketWorkItem.Destroy;
begin
  if (Buf <> nil) then
    FreeMem(Buf);

  inherited Destroy;
end;

{ TIocpTcpSocketThreadPool }

constructor TIocpTcpSocketThreadPool.Create(IocpTcpSocket: TIocpTcpSocket;
  ThreadsNumber: Integer);
begin
  inherited Create(ThreadsNumber, True);

  Self.IocpTcpSocket := IocpTcpSocket;
  Self.ThreadsNumber := ThreadsNumber;

  Startup;
end;

procedure TIocpTcpSocketThreadPool.DoProcessRequest(aDataObj: TWorkItem;
  aThread: TProcessorThread);
var
  WorkItem: TIocpTcpSocketWorkItem;
  Client: TIocpSocketConnection;
begin
  WorkItem := TIocpTcpSocketWorkItem(aDataObj);
  if not Assigned(WorkItem) then Exit;

  Client := WorkItem.Client;
  try
    IocpTcpSocket.TriggerReadComplete(Client, WorkItem.Buf, WorkItem.Len);
  except
    on e: Exception do
      AppendLog('%s.DoProcessRequest, %s=%s', [ClassName, e.ClassName, e.Message]);
  end;
end;

{ TIocpTcpSocket }

constructor TIocpTcpSocket.Create(AOwner: TComponent; IoThreadsNumber: Integer);
begin
  inherited Create(AOwner);

  FConnectionPool := TIocpObjectPool.Create(Self, TIocpSocketConnection, MAX_FREE_HANDLE_DATA_BLOCKS);
  FPerIoDataPool := TIocpMemoryPool.Create(SizeOf(TIocpPerIoData), MAX_FREE_IO_DATA_BLOCKS);
  FConnectionList := TIocpSocketConnectionList.Create(Self);
  FConnectionListLocker := TCriticalSection.Create;
  FIdleConnectionList := TIocpSocketConnectionList.Create(Self);

  FListenThreads := TList.Create;
  FListenThreadsLocker := TCriticalSection.Create;

  FIoThreadsNumber := IoThreadsNumber;
  FIocpHandle := 0;
  FTimeout := 10000;
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
  FPerIoDataPool.Free;

  inherited Destroy;
end;

function TIocpTcpSocket.Connect(const RemoteAddr: AnsiString;
  RemotePort: Word; ConnectTimeout: DWORD): TIocpSocketConnection;
var
  Socket: TSocket;
  DummyHandle: THandle;
  t: DWORD;
begin
  Result := nil;
  Socket := AsyncConnect(RemoteAddr, RemotePort);
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

function TIocpTcpSocket.PostNewAcceptEx(ListenSocket: TSocket): Boolean;
var
  PerIoData: PIocpPerIoData;
  ClientSocket: TSocket;
  Bytes: Cardinal;
  LastError: Integer;
  Connection: TIocpSocketConnection;
begin
  Result := False;

  ClientSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (ClientSocket = INVALID_SOCKET) then
  begin
    LastError := WSAGetLastError;
    AppendLog('%s.PostNewAcceptEx.为AcceptEx创建Socket失败, ERR=%d,%s', [ClassName, LastError, SysErrorMessage(LastError)]);
    Exit;
  end;

  // 生成新的连接对象并绑定到IOCP
  Connection := AllocConnection(ClientSocket);
  if not AssociateSocketWithCompletionPort(ClientSocket, Connection) then
  begin
    JwaWinsock2.CloseSocket(ClientSocket);
    FConnectionPool.FreeObject(Connection);
    Exit;
  end;

  if (Connection.AddRef = 1) then Exit;

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
  if (not AcceptEx(ListenSocket, ClientSocket, @PerIoData.Buffer.AcceptExBuffer[0], 0,
    SizeOf(TSockAddrIn) + 16, SizeOf(TSockAddrIn) + 16, Bytes, POverlapped(PerIoData))) then
  begin
    LastError := WSAGetLastError;
    if (LastError <> WSA_IO_PENDING) then
    begin
      AppendLog('%s.PostNewAcceptEx.调用AcceptEx失败(ListenSocket=%d, ClientSocket=%d), ERR=%d,%s', [ClassName, ListenSocket, ClientSocket, LastError, SysErrorMessage(LastError)]);
      FreeIoData(PerIoData);
      Connection.Release;
      Connection.Disconnect;
      Exit;
    end;
  end;

  Result := True;
end;

function TIocpTcpSocket.AllocConnection(Socket: TSocket): TIocpSocketConnection;
begin
  Result := TIocpSocketConnection(FConnectionPool.GetObject);

  Result.Timeout := FTimeout;
  Result.FSocket := Socket;
end;

function TIocpTcpSocket.AssociateSocketWithCompletionPort(Socket: TSocket;
  Connection: TIocpSocketConnection): Boolean;
begin
  Result := (IocpApiFix.CreateIoCompletionPort(Socket, FIocpHandle, ULONG_PTR(Connection), 0) <> 0);
  if not Result then
    AppendLog(Format('绑定IOCP失败,Socket=%d, ERR=%d,%s', [Socket, GetLastError, SysErrorMessage(GetLastError)]));
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
  Socket: TSocket;
  ConnList: TIocpSocketConnectionList;
  Connection: TIocpSocketConnection;
begin
  ConnList := TIocpSocketConnectionList.Create(Self);

  FConnectionListLocker.Enter;
  try
    // 将连接列表复制一份是因为连接断开后会自动从对应的工作/空闲列表中删除，
    // 这会造成FindNext失败，复制的列表不会受影响
    ConnList.Assign(FConnectionList);
    if ConnList.FindFirst(Socket, Connection) then
      repeat
        Connection.Disconnect;
      until not ConnList.FindNext(Socket, Connection);

    ConnList.Assign(FIdleConnectionList);
    if ConnList.FindFirst(Socket, Connection) then
      repeat
        Connection.Release; // 空闲连接尚未触发完成事件中的Release，这里需要做Release
        Connection.Disconnect;
      until not ConnList.FindNext(Socket, Connection);
  finally
    FConnectionListLocker.Leave;
    ConnList.Free;
  end;
end;

function TIocpTcpSocket.AsyncConnect(const RemoteAddr: AnsiString; RemotePort: Word): TSocket;
var
  InAddrInfo: TAddrInfo;
  POutAddrInfo: PAddrInfo;
  Addr, tmpAddr: TSockAddr;
  ClientSocket: TSocket;
  Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData;
  LastError: Integer;
begin
  Result := INVALID_SOCKET;

  {
    64位程序中gethostbyname返回的数据结构的h_addr_list指针无效(貌似高4字节和低4字节顺序颠倒了)
    用getaddrinfo返回的数据不会有问题,而且可以兼顾IPv4和IPv6,只需要简单的修改就能让程序同时支持
    IPv4和IPv6了
  }
  FillChar(InAddrInfo, SizeOf(TAddrInfo), 0);
  if (getaddrinfo(PAnsiChar(RemoteAddr), nil, @InAddrInfo, POutAddrInfo) <> 0) then
  begin
    LastError := WSAGetLastError;
    AppendLog('%s.AsyncConnect getaddrinfo失败, ERR=%d,%s', [ClassName, LastError, SysErrorMessage(LastError)]);
    Exit;
  end;

  ZeroMemory(@Addr, SizeOf(TSockAddr));
  Addr.sin_family := AF_INET;
  Addr.sin_addr.S_addr := POutAddrInfo.ai_addr.sin_addr.S_addr;
  Addr.sin_port := htons(RemotePort);

  freeaddrinfo(POutAddrInfo);

  ClientSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (ClientSocket = INVALID_SOCKET) then Exit;

  ZeroMemory(@tmpAddr, SizeOf(tmpAddr));
  tmpAddr.sin_family := AF_INET;
  tmpAddr.sin_addr.S_addr := INADDR_ANY;
  tmpAddr.sin_port := 0;
  if (bind(ClientSocket, @tmpAddr, SizeOf(tmpAddr)) = SOCKET_ERROR) then
  begin
    LastError := WSAGetLastError;
    JwaWinsock2.CloseSocket(ClientSocket);
    AppendLog('%s.AsyncConnect绑定ConnectEx(%d)端口失败, ERR=%d,%s', [ClassName, ClientSocket, LastError, SysErrorMessage(LastError)]);
    Exit;
  end;

  // 生成新的连接对象并绑定到IOCP
  Connection := AllocConnection(ClientSocket);
  if not AssociateSocketWithCompletionPort(ClientSocket, Connection) then
  begin
    JwaWinsock2.CloseSocket(ClientSocket);
    FConnectionPool.FreeObject(Connection);
    Exit;
  end;

  if (Connection.AddRef = 1) then Exit;

  Connection.FRemoteAddr := Addr;
  ExtractAddrInfo(Connection.FRemoteAddr, Connection.FRemoteIP, Connection.FRemotePort);
  PerIoData := AllocIoData(ClientSocket, iotConnect);
  if not ConnectEx(ClientSocket, @Addr, SizeOf(TSockAddr), nil, 0, PCardinal(0)^, PWSAOverlapped(PerIoData)) and
    (WSAGetLastError <> WSA_IO_PENDING) then
  begin
    LastError := WSAGetLastError;
    AppendLog('%s.AsyncConnect.ConnectEx(%d)失败, ERR=%d,%s', [ClassName, ClientSocket, LastError, SysErrorMessage(LastError)]);
    FreeIoData(PerIoData);
    Connection.Release;
    Connection.Disconnect;
    Exit;
  end;

  Result := ClientSocket;
end;

function TIocpTcpSocket.IsClient(SomeThing: TObject): Boolean;
var
  Socket: TSocket;
  Client: TIocpSocketConnection;
begin
  FConnectionListLocker.Enter;
  try
    if FConnectionList.FindFirst(Socket, Client) then
    begin
      repeat
        if (Client = SomeThing) then
        begin
          Result := True;
          Exit;
        end;
      until not FConnectionList.FindNext(Socket, Client);
    end;
    Result := False;
  finally
    FConnectionListLocker.Leave;
  end;
end;

procedure TIocpTcpSocket.ExtractAddrInfo(const Addr: TSockAddr;
  out IP: AnsiString; out Port: Word);
begin
  IP := inet_ntoa(Addr.sin_addr);
  Port := ntohs(Addr.sin_port);
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

function TIocpTcpSocket.Listen(const Host: AnsiString; Port: Word; InitAcceptNum: Integer): TSocket;
var
  ListenSocket: TSocket;
  InAddrInfo: TAddrInfo;
  POutAddrInfo: PAddrInfo;
  ListenAddr: u_long;
  ServerAddr: TSockAddrIn;
  LastError: Integer;
begin
  Result := INVALID_SOCKET;

  // 如果传递了一个有效地址则监听该地址
  // 否则监听所有本地地址
  ListenAddr := htonl(INADDR_ANY);
  if (Host <> '') then
  begin
    FillChar(InAddrInfo, SizeOf(TAddrInfo), 0);
    if (getaddrinfo(PAnsiChar(Host), nil, @InAddrInfo, POutAddrInfo) = 0) then
    begin
      ListenAddr := POutAddrInfo.ai_addr.sin_addr.S_addr;
      freeaddrinfo(POutAddrInfo);
    end;
  end;

  ListenSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (ListenSocket = INVALID_SOCKET) then Exit;

  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_addr.S_addr := ListenAddr;
  ServerAddr.sin_port := htons(Port);
  if (bind(ListenSocket, PSockaddr(@ServerAddr), SizeOf(ServerAddr)) = SOCKET_ERROR) then
  begin
    LastError := WSAGetLastError;
    JwaWinsock2.CloseSocket(ListenSocket);
    AppendLog('%s.绑定监听端口(%d)失败, ERR=%d,%s', [ClassName, Port, LastError, SysErrorMessage(LastError)]);
    Exit;
  end;

  if (JwaWinsock2.Listen(ListenSocket, SOMAXCONN) = SOCKET_ERROR) then
  begin
    LastError := WSAGetLastError;
    JwaWinsock2.CloseSocket(ListenSocket);
    AppendLog('%s.启动监听端口(%d)失败, ERR=%d,%s', [ClassName, Port, LastError, SysErrorMessage(LastError)]);
    Exit;
  end;

  try
    if not AssociateSocketWithCompletionPort(ListenSocket, nil) then
    begin
      JwaWinsock2.CloseSocket(ListenSocket);
      AppendLog('%s.绑定监听端口(%d)到IOCP失败', [ClassName, Port]);
      Exit;
    end;

    try
      FListenThreadsLocker.Enter;
      FListenThreads.Add(TIocpAcceptThread.Create(Self, ListenSocket, InitAcceptNum));
    finally
      FListenThreadsLocker.Leave;
    end;

    Result := ListenSocket;
  except
    on e: Exception do
      AppendLog('%s.Listen ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

function TIocpTcpSocket.Listen(Port: Word; Concurrent: Integer): TSocket;
begin
  Result := Listen('', Port, Concurrent);
end;

function TIocpTcpSocket.LockConnectionList: TIocpSocketConnectionList;
begin
  Result := FConnectionList;
  FConnectionListLocker.Enter;
end;

procedure TIocpTcpSocket.MessagePump;
begin
  while ProcessMessage do
    ;
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
  PerIoData: PIocpPerIoData; aThread: TIocpIoThread);
begin
  try
    try
      case PerIoData.Operation of
        iotAccept: RequestAcceptComplete(PerIoData);
        iotConnect: RequestConnectComplete(Connection);
        iotReadZero: RequestReadZeroComplete(Connection, PerIoData);
        iotRead: RequestReadComplete(Connection, PerIoData);
        iotWrite: RequestWriteComplete(Connection, PerIoData);
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
  PLocalAddr, PRemoteAddr: PSockaddr;
begin
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
      end
      else
      begin
        Connection := FConnectionList[PerIoData.ClientSocket];
        if (Connection = nil) then Exit;
      end;
    finally
      FConnectionListLocker.Leave;
    end;

    try
      Connection.UpdateTick;

      // 对于SO_UPDATE_ACCEPT_CONTEXT,最后一个参数optlen实际需要设定为SizeOf(PAnsiChar)
      // 这一点在MSDN的例子中都是错的！因为经过实际测试发现在64位程序中这里传递SizeOf(PerIoData.ListenSocket)
      // 的话会报错：10014,系统检测到在一个调用中尝试使用指针参数时的无效指针地址。
      // 也就是说这里的optlen实际上应该传递的是一个指针的长度
      if (setsockopt(PerIoData.ClientSocket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
  //      PAnsiChar(@(PerIoData.ListenSocket)), SizeOf(PerIoData.ListenSocket)) = SOCKET_ERROR) then
        PAnsiChar(@PerIoData.ListenSocket), SizeOf(PAnsiChar)) = SOCKET_ERROR) then
      begin
        AppendLog('%s.RequestAcceptComplete.setsockopt.SO_UPDATE_ACCEPT_CONTEXT ERROR %d=%s', [ClassName, WSAGetLastError, SysErrorMessage(WSAGetLastError)]);
        Connection.Disconnect;
        Exit;
      end;

      LocalAddrLen := SizeOf(TSockAddr);
      RemoteAddrLen := SizeOf(TSockAddr);
      GetAcceptExSockaddrs(@PerIoData.Buffer.AcceptExBuffer[0], 0, SizeOf(TSockAddrIn) + 16,
        SizeOf(TSockAddrIn) + 16, PLocalAddr, LocalAddrLen,
        PRemoteAddr, RemoteAddrLen);

      if not Connection.InitSocket then
      begin
        Connection.Disconnect;
        Exit;
      end;

      Connection.FRemoteAddr := PRemoteAddr^;
      ExtractAddrInfo(Connection.FRemoteAddr, Connection.FRemoteIP, Connection.FRemotePort);

      if not TriggerConnectComplete(Connection) then Exit;

      // 连接建立之后PostZero读取请求
      if not Connection.PostReadZero then Exit;
    finally
      Connection.Release; // 对应 PostNewAcceptEx 中的 AddRef;
    end;
  except
    on e: Exception do
      AppendLog('%s.RequestAcceptComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestConnectComplete(Connection: TIocpSocketConnection);
begin
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

      if not TriggerConnectComplete(Connection) then Exit;

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
      Connection.UpdateTick;

      // 正式开始接收数据
      Connection.PostRead;
    finally
      Connection.Release; // 对应PostReadZero中的AddRef
    end;
  except
    on e: Exception do
      AppendLog('%s.RequestReadComplete ERROR %s=%s', [ClassName, e.ClassName, e.Message], ltException);
  end;
end;

procedure TIocpTcpSocket.RequestReadComplete(Connection: TIocpSocketConnection;
  PerIoData: PIocpPerIoData);
begin
  try
    try
      Connection.DecPendingRecvCount;

      if (PerIoData.BytesTransfered = 0) then
      begin
        Connection.Disconnect;
        Exit;
      end;

      Connection.UpdateTick;

      // PerIoData.Buffer.DataBuf 就是已接收到的数据，PerIoData.BytesTransfered 是实际接收到的字节数
      PerIoData.Buffer.DataBuf.Len := PerIoData.BytesTransfered;
//      if not TriggerReadComplete(Connection, PerIoData.Buffer.DataBuf.buf, PerIoData.Buffer.DataBuf.len) then Exit;

      if (Connection.IsClosed) or (Connection.AddRef = 1) then Exit;
      FLogicThreadsPool.AddRequest(TIocpTcpSocketWorkItem.Create(Connection, PerIoData.Buffer.DataBuf.Buf, PerIoData.Buffer.DataBuf.Len));

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
    Connection.DecPendingSendCount;

    try
      if (PerIoData.BytesTransfered = 0) then
      begin
        Connection.Disconnect;
        Exit;
      end;

      Connection.UpdateTick;

      PerIoData.Buffer.DataBuf.Len := PerIoData.BytesTransfered;
      TriggerWriteComplete(Connection, PerIoData.Buffer.DataBuf.Buf, PerIoData.Buffer.DataBuf.Len);
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

  JwaWinsock2.shutdown(Socket, SD_BOTH);
  JwaWinsock2.CloseSocket(Socket);
end;

procedure TIocpTcpSocket.StartupWorkers;
var
  si: TSystemInfo;
  NumberOfThreads, i: Integer;
begin
  if (FIocpHandle <> 0) then Exit;

  // 启动逻辑线程池
  FLogicThreadsPool := TIocpTcpSocketThreadPool.Create(Self, FLogicThreadsNumber);
  FLogicThreadsPool.Startup;

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
  FIocpHandle := IocpApiFix.CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  if (FIocpHandle = INVALID_HANDLE_VALUE) then
    raise Exception.CreateFmt('%s.StartupWorkers创建IOCP对象失败', [ClassName]);

  // 创建IO线程
  SetLength(FIoThreads, NumberOfThreads);
  SetLength(FIoThreadHandles, Length(FIoThreads));
  for i := 0 to High(FIoThreads) do
  begin
    FIoThreads[i] := TIocpIoThread.Create(Self);
    FIoThreadHandles[i] := FIoThreads[i].Handle;
  end;

  // 创建时钟队列
  FTimerQueue := TIocpTimerQueue.Create;
end;

procedure TIocpTcpSocket.ShutdownWorkers;
var
  i: Integer;
begin
  if (FIocpHandle = 0) then Exit;

  AppendLog('%s.shutdown 1', [ClassName]);
  // 断开所有连接
  DisconnectAll;

  // 关闭逻辑线程池
  FLogicThreadsPool.shutdown;
  FLogicThreadsPool.Free;

  // 这里必须加上Sleep，以保证所有断开连接的命令比后面退出线程的命令先进入IOCP队列
  // 否则可能会出现连接还没全部释放，线程就被终止了，造成资源泄漏
  while ((FConnectionList.Count > 0) or (FIdleConnectionList.Count > 0)) do
    Sleep(10);

  AppendLog('%s.shutdown 2', [ClassName]);
  // 关闭监听线程
  FListenThreadsLocker.Enter;
  try
    for i := 0 to FListenThreads.Count - 1 do
      TIocpAcceptThread(FListenThreads[i]).Quit;
    FListenThreads.Clear;
  finally
    FListenThreadsLocker.Leave;
  end;

  AppendLog('%s.shutdown 3', [ClassName]);
  // 关闭IO线程
  for i := Low(FIoThreads) to High(FIoThreads) do
    IocpApiFix.PostQueuedCompletionStatus(FIocpHandle, 0, 0, POverlapped(SHUTDOWN_FLAG));

  // 等待IO线程结束
  WaitForMultipleObjects(Length(FIoThreadHandles), Pointer(FIoThreadHandles), True, INFINITE);
  SetLength(FIoThreads, 0);
  SetLength(FIoThreadHandles, 0);

  // 关闭完成端口
  CloseHandle(FIocpHandle);
  FIocpHandle := 0;

  AppendLog('%s.shutdown 4, ConnCount=%d, IdleConnCount=%d',
    [ClassName, FConnectionList.Count, FIdleConnectionList.Count]);

  AppendLog('%s.shutdown 5', [ClassName]);
  FConnectionPool.Clear;
  FPerIoDataPool.Clear;

  // 释放时钟队列
  FTimerQueue.Free;

  AppendLog('%s.shutdown compelte', [ClassName]);
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
  Result := True;
end;

function TIocpTcpSocket.TriggerClientDisconnected(
  Client: TIocpSocketConnection): Boolean;
begin
  Result := True;
end;

function TIocpTcpSocket.TriggerClientRecvData(
  Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
begin
  Result := True;
end;

function TIocpTcpSocket.TriggerClientSentData(
  Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
begin
  Result := True;
end;

function TIocpTcpSocket.TriggerConnectComplete(Connection: TIocpSocketConnection): Boolean;
begin
  Result := TriggerClientConnected(Connection);
end;

function TIocpTcpSocket.TriggerDisconnectComplete(Connection: TIocpSocketConnection): Boolean;
begin
  Result := TriggerClientDisconnected(Connection);
end;

function TIocpTcpSocket.TriggerReadComplete(Connection: TIocpSocketConnection;
  Buf: Pointer; Len: Integer): Boolean;
begin
  try
    Result := Connection.TriggerReadComplete(Buf, Len);
    if Result then
      Result := TriggerClientRecvData(Connection, Buf, Len);
  finally
    Connection.Release; // 对应 RequestReadComplete 中添加线程池任务时的 AddRef
  end;
end;

function TIocpTcpSocket.TriggerWriteComplete(Connection: TIocpSocketConnection;
  Buf: Pointer; Len: Integer): Boolean;
begin
  Result := Connection.TriggerWriteComplete(Buf, Len);
  if Result then
    Result := TriggerClientSentData(Connection, Buf, Len);
end;

procedure TIocpTcpSocket.UnlockConnectionList;
begin
  FConnectionListLocker.Leave;
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

function TIocpLineSocketConnection.Send(const s: AnsiString): Integer;
begin
  Result := inherited Send(s + TIocpLineSocket(Owner).LineEndTag);
end;

{ TIocpLineSocket }

function TIocpLineSocket.Connect(const RemoteAddr: AnsiString; RemotePort: Word;
  ConnectTimeout: DWORD): TIocpLineSocketConnection;
begin
  Result := TIocpLineSocketConnection(inherited Connect(RemoteAddr, RemotePort, ConnectTimeout));
end;

constructor TIocpLineSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ConnectionClass := TIocpLineSocketConnection;
  FLineEndTag := #13#10;
  FLineLimit := 65536;
end;

procedure TIocpLineSocket.DoOnRecvLine(Client: TIocpLineSocketConnection;
  Line: AnsiString);
begin

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
        Client.LineText.Size := 0;
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
      Client.LineText.Size := 0;
    end;

    Dec(Len, SizeOf(Ch));
    Inc(pch);
  end;
end;

procedure TIocpLineSocket.SetLineEndTag(const Value: AnsiString);
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

{ TSimpleIocpSocketConnection }

constructor TSimpleIocpSocketConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FRequestDoneEvent := CreateEvent(nil, True, True, nil);
end;

destructor TSimpleIocpSocketConnection.Destroy;
begin
  SetEvent(FRequestDoneEvent);
  CloseHandle(FRequestDoneEvent);
  inherited Destroy;
end;

function TSimpleIocpSocketConnection.IsIdle: Boolean;
begin
  Result := (WaitForSingleObject(FRequestDoneEvent, 0) = WAIT_OBJECT_0) and not IsClosed;
end;

{ TSimpleIocpTcpClient }

constructor TSimpleIocpTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, 1);

  ConnectionClass := TSimpleIocpSocketConnection;
end;

function TSimpleIocpTcpClient.GetIdleConnectionAndLock: TSimpleIocpSocketConnection;
var
  Socket: TSocket;
  ClientList: TIocpSocketConnectionList;
  Client: TSimpleIocpSocketConnection;
begin
  ClientList := LockConnectionList;
  try
    if ClientList.FindFirst(Socket, TIocpSocketConnection(Client)) then
    begin
      repeat
        if (Client.IsIdle) then
        begin
          ResetEvent(Client.FRequestDoneEvent);
          Result := Client;
          Exit;
        end;
      until not ClientList.FindNext(Socket, TIocpSocketConnection(Client));
    end;
  finally
    UnlockConnectionList;
  end;

  Result := TSimpleIocpSocketConnection(Connect(FRemoteAddr, FRemotePort));
  if (Result <> nil) then
    ResetEvent(Result.FRequestDoneEvent);
end;

function TSimpleIocpTcpClient.Send(Buf: Pointer; Size: Integer): Integer;
var
  Client: TSimpleIocpSocketConnection;
begin
  Client := GetIdleConnectionAndLock;
  if (Client <> nil) then
    Result := Client.Send(Buf, Size)
  else
    Result := -1;
end;

function TSimpleIocpTcpClient.Send(const s: AnsiString): Integer;
begin
  Result := Send(@s[1], Length(s));
end;

procedure TSimpleIocpTcpClient.SetRemoteAddr(const Value: AnsiString);
begin
  FRemoteAddr := Value;
end;

procedure TSimpleIocpTcpClient.SetRemotePort(const Value: Word);
begin
  FRemotePort := Value;
end;

function TSimpleIocpTcpClient.TriggerClientSentData(
  Client: TIocpSocketConnection; Buf: Pointer; Len: Integer): Boolean;
begin
  with TSimpleIocpSocketConnection(Client) do
  begin
    SetEvent(FRequestDoneEvent);
  end;
  Result := True;
end;

{ TSimpleIocpTcpServer }

constructor TSimpleIocpTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListenSocket := INVALID_SOCKET;

  FAddr := '';
  FInitAcceptNum := INIT_ACCEPTEX_NUM;
end;

destructor TSimpleIocpTcpServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function TSimpleIocpTcpServer.Start: Boolean;
begin
  if (FListenSocket <> INVALID_SOCKET) then
  begin
    Result := True;
    Exit;
  end;

  StartupWorkers;
  FListenSocket := inherited Listen(FAddr, FPort, FInitAcceptNum);
  Result := (FListenSocket <> INVALID_SOCKET);
end;

function TSimpleIocpTcpServer.Stop: Boolean;
begin
  if (FListenSocket = INVALID_SOCKET) then
  begin
    Result := True;
    Exit;
  end;

  ShutdownWorkers;
  FListenSocket := INVALID_SOCKET;
  Result := True;
end;

initialization
  IoCachePool := TIocpMemoryPool.Create(NET_CACHE_SIZE, MAX_FREE_IO_DATA_BLOCKS);
  FileCachePool := TIocpMemoryPool.Create(FILE_CACHE_SIZE, MAX_FREE_HANDLE_DATA_BLOCKS);

finalization
  IoCachePool.Free;
  FileCachePool.Free;

end.

