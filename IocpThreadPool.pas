unit IocpThreadPool;

{*
  利用IOCP实现的线程池，效率远高于普通线程池
  根据Synopse中的线程池代码改编
 *}

interface

uses
  Windows, Classes, SysUtils, Math, IocpApiFix;

const
  SHUTDOWN_FLAG = ULONG_PTR(-1);

type
  TThreadsPool = class;
  TProcessorThread = class;

  // 储存请求数据的基本类
  TThreadRequest = class(TObject)
  protected
    // 线程池工作函数
    // 继承这个方法填写自己的线程代码
    procedure Execute(Thread: TProcessorThread); virtual;
  end;

  // 工作线程
  TProcessorThread = class(TThread)
  private
    FPool: TThreadsPool;
    FTag: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(Pool: TThreadsPool); reintroduce; virtual;

    property Tag: Pointer read FTag write FTag;
  end;

  TProcessorThreadArray = array of TProcessorThread;

  // 线程池
  TThreadsPool = class
  protected
    FIocpHandle: THandle;
    FNumberOfThreads: Integer;
    FThreads: TProcessorThreadArray;
    FThreadHandles: array of THandle;
    FPendingRequest: Integer;

    // 线程启动事件
    procedure DoThreadStart(Thread: TProcessorThread); virtual;

    // 线程结束事件
    procedure DoThreadExit(Thread: TProcessorThread); virtual;
  public
    // NumberOfThreads=线程数，如果<=0则自动根据CPU核心数计算最佳线程数
    constructor Create(NumberOfThreads: Integer = 0; Suspend: Boolean = False);
    destructor Destroy; override;

    function AddRequest(Request: TThreadRequest): Boolean; virtual;
    procedure Startup;
    procedure Shutdown;

    property Threads: TProcessorThreadArray read FThreads;
    property PendingRequest: Integer read FPendingRequest;
  end;

implementation

{ TThreadRequest }

procedure TThreadRequest.Execute(Thread: TProcessorThread);
begin
end;

{ TProcessorThread }

constructor TProcessorThread.Create(Pool: TThreadsPool);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FPool := Pool;

  FTag := nil;
  Suspended := False;
end;

procedure TProcessorThread.Execute;
var
  Bytes: DWORD;
  Request: TThreadRequest;
  CompKey: ULONG_PTR;
begin
  FPool.DoThreadStart(Self);
  while not Terminated and IocpApiFix.GetQueuedCompletionStatus(FPool.FIocpHandle, Bytes, CompKey, POverlapped(Request), INFINITE) do
  try
    // 不是有效的请求，忽略
    if (CompKey <> ULONG_PTR(FPool)) then Continue;

    // 收到线程退出标志，跳出循环
    if (ULONG_PTR(Request) = SHUTDOWN_FLAG) then Break;

    if (Request <> nil) then
    try
      Request.Execute(Self);
    finally
      InterlockedDecrement(FPool.FPendingRequest);
      Request.Free;
    end;
  except
  end;
  FPool.DoThreadExit(Self);
end;

{ TThreadsPool }

// NumberOfThreads 线程数，如果设定为0，则自动根据CPU核心数计算最佳线程数
constructor TThreadsPool.Create(NumberOfThreads: Integer; Suspend: Boolean);
begin
  FNumberOfThreads := NumberOfThreads;
  FIocpHandle := 0;

  if not Suspend then
    Startup;
end;

destructor TThreadsPool.Destroy;
begin
  Shutdown;
  inherited Destroy;
end;

procedure TThreadsPool.DoThreadStart(Thread: TProcessorThread);
begin
end;

procedure TThreadsPool.DoThreadExit(Thread: TProcessorThread);
begin
end;

function TThreadsPool.AddRequest(Request: TThreadRequest): Boolean;
begin
  Result := False;
  if (FIocpHandle = 0) then Exit;

  InterlockedIncrement(FPendingRequest);
  Result := IocpApiFix.PostQueuedCompletionStatus(FIocpHandle, 0, ULONG_PTR(Self), POverlapped(Request));
  if not Result then
    InterlockedDecrement(FPendingRequest);
end;

procedure TThreadsPool.Startup;
var
  NumberOfThreads, i: Integer;
  si: TSystemInfo;
begin
  if (FIocpHandle <> 0) then Exit;

  if (FNumberOfThreads <= 0) then
  begin
    GetSystemInfo(si);
    NumberOfThreads := si.dwNumberOfProcessors * 2 + 2;
  end else
    NumberOfThreads := Min(FNumberOfThreads, 64); // maximum count for WaitForMultipleObjects()

  // 创建完成端口
  // NumberOfConcurrentThreads = 0 表示每个CPU保持一个并发线程
  // 经实际测试发现：并发线程数如果只保持每个CPU一个，在IOCP大并发请求的处理中会出现待处理请求大量堆积
  // 从而出现内存的大量消耗，将并发数设置为上面计算出来的NumberOfThreads能保证逻辑请求尽可能快的被响应
  // 这样内存消耗几乎不会出现什么大的波动（收发速度会略微降低，不过这点牺牲是完全值得的）
  FIocpHandle := IocpApiFix.CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, NumberOfThreads);
  if (FIocpHandle = INVALID_HANDLE_VALUE) then
    raise Exception.Create('IocpThreadPool创建IOCP对象失败');

  // 创建所有工作线程
  Setlength(FThreads, NumberOfThreads);
  SetLength(FThreadHandles, NumberOfThreads);
  for i := 0 to NumberOfThreads - 1 do
  begin
    FThreads[i] := TProcessorThread.Create(Self);
    FThreadHandles[i] := FThreads[i].Handle;
  end;
end;

procedure TThreadsPool.Shutdown;
var
  i: Integer;
begin
  if (FIocpHandle = 0) then Exit;

  // 给所有工作线程发送退出命令
  for i := 0 to High(FThreads) do
    IocpApiFix.PostQueuedCompletionStatus(FIocpHandle, 0, ULONG_PTR(Self), POverLapped(SHUTDOWN_FLAG));

  // 等待工作线程结束
  WaitForMultipleObjects(Length(FThreadHandles), Pointer(FThreadHandles), True, INFINITE);

  // 关闭完成端口句柄
  CloseHandle(FIocpHandle);
  FIocpHandle := 0;

  SetLength(FThreads, 0);
  SetLength(FThreadHandles, 0);
end;

end.
