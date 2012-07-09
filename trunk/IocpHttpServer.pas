unit IocpHttpServer;

{$define __LOGIC_THREAD_POOL__}

interface

uses
  Windows, Messages, Classes, SysUtils, StrUtils, SyncObjs, Math, IOUtils, IdWinsock2,
  IocpTcpSocket, IocpThreadPool, IocpHttpUtils, IocpBuffer, IocpLogger;

const
  IOCP_HTTP_SERVER_VERSION  = 'IocpHttpServer/1.0';
  IOCP_HTTP_INIT_ACCEPT_NUM = 64;

type
  TIocpHttpServer = class;

  TIocpHttpConnectionState = (hcRequest, hcPostData, hcDone);
  TIocpHttpConnection = class(TIocpSocketConnection)
  private
    FHttpState: TIocpHttpConnectionState;
    FResponseSize, FResponseSent: Integer;
//    function ParseRequestDataZeroCopy: Boolean;
  protected
    FRawRequestText: TIocpStringStream;
    FMethod, FPath, FParams, FPathAndParams, FVersion: string;
    FHttpVerNum: Integer;
    FKeepAlive: Boolean;

    FRequestCmdLine: string;
    FRequestHeader: TStringList;
    FRequestContentType: string;
    FRequestHasContentLength: Boolean;
    FRequestContentLength: Int64;
    FRequestAccept: string;
    FRequestReferer: string;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestAuth: string;
    FRequestCookies: string;
    FRequestHost: string;
    FRequestHostName: string;
    FRequestHostPort: string;
    FRequestConnection: string;
    FXForwardedFor: string;
    FRequestPostData: TIocpStringStream;

    FAcceptPostData: Boolean;
    FPostDataSize: Integer;

    // 如果要更进一步解析HTTP请求头的数据，可以修改这个函数
    function ParseRequestData: Boolean; virtual;
    function MakeHeader(const Status, ContType, Header: string; ContSize: Integer): string;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    // 所有AnswerXXXX的基础函数
    function AnswerBuf(const Header: string; Buf: Pointer; Size: Integer): Boolean; overload;
    function AnswerStream(const Header: string; Stream: TStream): Boolean;

    // 以字节数组方式返回请求
    function AnswerBytes(const Status, ContType, Header: string; Data: TBytes): Boolean; overload;
    function AnswerBytes(const Header: string; Data: TBytes): Boolean; overload;

    // 以字符串方式返回请求
    function AnswerHTML(const Status, ContType, Header: string; HTML: RawByteString): Boolean; overload;
    function AnswerHTML(const Status, ContType, Header, HTML: string): Boolean; overload;
    function AnswerHTML(const Header: string; HTML: RawByteString): Boolean; overload;
    function AnswerHTML(const Header, HTML: string): Boolean; overload;

    // 以文件内容方式返回请求
    function AnswerDocument(const FileName: string): Boolean;

    // 返回错误信息
    procedure Answer400;
    procedure Answer401;
    procedure Answer403;
    procedure Answer404;
    procedure Answer501;
    procedure Answer503;
    procedure Answer506;
    function AnswerError(ErrCode: Integer): Boolean;

    procedure Reset;

    property RawRequestText: TIocpStringStream read FRawRequestText;
    property RequestCmdLine: string read FRequestCmdLine;
    property Method: string read FMethod;
    property Path: string read FPath;
    property Params: string read FParams;
    property PathAndParams: string read FPathAndParams;
    property Version: string read FVersion;
    property KeepAlive: Boolean read FKeepAlive;

    property RequestHeader: TStringList read FRequestHeader;
    property RequestContentType: string read FRequestContentType;
    property RequestHasContentLength: Boolean read FRequestHasContentLength;
    property RequestContentLength: Int64 read FRequestContentLength;
    property RequestAccept: string read FRequestAccept;
    property RequestReferer: string read FRequestReferer;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestUserAgent: string read FRequestUserAgent;
    property RequestAuth: string read FRequestAuth;
    property RequestCookies: string read FRequestCookies;
    property RequestHost: string read FRequestHost;
    property RequestHostName: string read FRequestHostName;
    property RequestHostPort: string read FRequestHostPort;
    property RequestConnection: string read FRequestConnection;
    property XForwardedFor: string read FXForwardedFor;

    property RequestPostData: TIocpStringStream read FRequestPostData;
  end;

  {
    *** Iocp逻辑(业务处理)请求对象 ***
  }
  TIocpHttpRequest = class(TIocpThreadRequest)
  private
    Client: TIocpHttpConnection;
  protected
    procedure Execute(Thread: TProcessorThread); override;
  public
    constructor Create(Client: TIocpHttpConnection);
  end;

  TIocpHttpAcceptPostDataEvent = function(Sender: TObject; DataSize: Int64): Boolean of object;
  TIocpHttpRequestEvent = procedure(Sender: TObject; Client: TIocpHttpConnection) of object;
  TIocpHttpServer = class(TSimpleIocpTcpServer)
  private
    {$ifdef __LOGIC_THREAD_POOL__}
    FJobThreadPool: TIocpThreadPool;
    {$endif}
    FRootDir: string;
    FAcceptPostData: TIocpHttpAcceptPostDataEvent;
    FOnRequest: TIocpHttpRequestEvent;

    function IsValidHttpRequest(buf: PAnsiChar; len: Integer): Boolean;
    procedure ParseRecvData(Client: TIocpHttpConnection; buf: Pointer; len: Integer);
    function GetRootDir: string;
  protected
    {$ifdef __LOGIC_THREAD_POOL__}
    procedure StartupWorkers; override;
    procedure ShutdownWorkers; override;
    {$endif}

    function TriggerClientRecvData(Client: TIocpSocketConnection; buf: Pointer; len: Integer): Boolean; override;
    function TriggerClientSentData(Client: TIocpSocketConnection; buf: Pointer; len: Integer): Boolean; override;
  protected
    function TriggerAcceptPostData(DataSize: Int64): Boolean; virtual;

    // DoOnRequest 将会在线程池中被调用，可以在这个函数里处理用户自定义的回执数据
    procedure DoOnRequest(Client: TIocpHttpConnection); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InitAcceptNum default IOCP_HTTP_INIT_ACCEPT_NUM;
    property RootDir: string read GetRootDir write FRootDir;
    property AcceptPostData: TIocpHttpAcceptPostDataEvent read FAcceptPostData write FAcceptPostData;
    property OnRequest: TIocpHttpRequestEvent read FOnRequest write FOnRequest;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Iocp', [TIocpHttpServer]);
end;

{ TIocpHttpConnection }

constructor TIocpHttpConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FRawRequestText := TIocpStringStream.Create('');
  FRequestHeader := TStringList.Create;
  FRequestPostData := TIocpStringStream.Create('');

  Reset;
end;

destructor TIocpHttpConnection.Destroy;
begin
  FRawRequestText.Free;
  FRequestHeader.Free;
  FRequestPostData.Free;

  inherited Destroy;
end;

function TIocpHttpConnection.MakeHeader(const Status, ContType,
  Header: string; ContSize: Integer): string;
begin
  Result := '';

  if (Status = '') then
    Result := Result + FVersion + ' 200 OK' + #13#10
  else
    Result := Result + FVersion + Status + #13#10;

  if (ContType = '') then
    Result := Result + 'Content-Type: text/html' + #13#10
  else
    Result := Result + 'Content-Type: ' + ContType + #13#10;


  if (ContSize > 0) then
    Result := Result + 'Content-Length: ' + IntToStr(ContSize) + #13#10;
//    Result := Result + 'Cache-Control: no-cache'#13#10;

  if FKeepAlive then
    Result := Result + 'Connection: keep-alive'#13#10
  else
    Result := Result + 'Connection: close'#13#10;

  if (IOCP_HTTP_SERVER_VERSION <> '') then
    Result := Result + 'Server: ' + IOCP_HTTP_SERVER_VERSION + #13#10;

  if (Header <> '') then
    Result := Result + FixHeader(Header)
  else
    Result := Result + #13#10;
end;

// todo: 减少内存复制，提升性能
// 直接从 FRawRequestText.DataString 进行解析
// 解析起来可能比较费劲，但是对性能提升会有很大帮助
// ** 实际测试发现，Delphi内置的Copy函数效率远高于我自己实现的
// ** CopyRawString，所以没必要去做更进一步的优化了，就保持这个
// ** 函数现在的样子，效率已经很高了
function TIocpHttpConnection.ParseRequestData: Boolean;
var
  RequestLine: string;
  I, J, SpacePos: Integer;
begin
  FRequestHeader.Text := string(FRawRequestText.DataString);
  if (FRequestHeader.Count = 0) then Exit(False);

  // GET /test?v=abc HTTP/1.1
  FRequestCmdLine := FRequestHeader[0];
  FRequestHeader.Delete(0);

  I := 1;
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求方法(GET, POST, PUT, HEAD...)
  FMethod := UpperCase(Copy(FRequestCmdLine, 1, I - 1));
  Inc(I);
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求参数及路径
  FPathAndParams := URLDecode(Copy(FRequestCmdLine, J, I - J));
  // 请求路径
  FPath := FPathAndParams;
  // 解析参数
  J := Pos('?', FPath);
  if (J <= 0) then
    FParams := ''
  else begin
    FParams := Copy(FPath, J + 1, Length(FPath));
    FPath   := Copy(FPath, 1, J - 1);
  end;
  Inc(I);
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(FRequestCmdLine)) and (FRequestCmdLine[I] <> ' ') do
    Inc(I);
  // 请求的HTTP版本
  FVersion := Trim(UpperCase(Copy(FRequestCmdLine, J, I - J)));
  if (FVersion = '') then
    FVersion := 'HTTP/1.0';
  if (FVersion = 'HTTP/1.0') then
    FHttpVerNum := 10
  else
    FHttpVerNum := 11;
  FKeepAlive := (FHttpVerNum = 11);

  FRequestHasContentLength := False;
  FRequestContentLength := 0;
  for RequestLine in FRequestHeader do
  begin
    if (RequestLine = '') then Continue;

    SpacePos := Pos(' ', RequestLine) + 1;

    if StrLIComp(@RequestLine[1], 'Content-Type:', 13) = 0 then
      FRequestContentType := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Content-Length:', 15) = 0 then
    begin
      FRequestHasContentLength := TRUE;
      FRequestContentLength := StrToInt64Def(Copy(RequestLine, SpacePos, Length(RequestLine)), -1);
    end
    else if StrLIComp(@RequestLine[1], 'Accept:', 7) = 0 then
      FRequestAccept:= Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Referer:', 8) = 0 then
      FRequestReferer := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Accept-Language:', 16) = 0 then
      FRequestAcceptLanguage := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Accept-Encoding:', 16) = 0 then
      FRequestAcceptEncoding := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'User-Agent:', 11) = 0 then
      FRequestUserAgent := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Authorization:', 14) = 0 then
      FRequestAuth := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Cookie:', 7) = 0 then
      FRequestCookies := Copy(RequestLine, SpacePos, Length(RequestLine))
    else if StrLIComp(@RequestLine[1], 'Host:', 5) = 0 then
    begin
      FRequestHost := Copy(RequestLine, SpacePos, Length(RequestLine));
      J := Pos(':', FRequestHost);
      if J > 0 then
      begin
        FRequestHostName := Copy(FRequestHost, 1, J - 1);
        FRequestHostPort := Copy(FRequestHost, J + 1, 100);
      end else
      begin
        FRequestHostName := FRequestHost;
        FRequestHostPort := IntToStr(TIocpHttpServer(Owner).Port);
      end;
    end
    else if StrLIComp(@RequestLine[1], 'Connection:', 11) = 0 then
    begin
      FRequestConnection := Copy(RequestLine, SpacePos, Length(RequestLine));
      // HTTP/1.0 默认KeepAlive=False，只有显示指定了Connection: keep-alive才认为KeepAlive=True
      // HTTP/1.1 默认KeepAlive=True，只有显示指定了Connection: close才认为KeepAlive=False
      if FHttpVerNum = 10 then
        FKeepAlive := SameText(FRequestConnection, 'keep-alive')
      else if SameText(FRequestConnection, 'close') then
        FKeepAlive := False;
    end
    else if StrLIComp(@RequestLine[1], 'X-Forwarded-For:', 16) = 0 then
      FXForwardedFor := Copy(RequestLine, SpacePos, Length(RequestLine));
  end;

  Result := True;
end;

// 实际测试发现，效率反而不如上面的ParseRequestData，暂时保留不使用
{function TIocpHttpConnection.ParseRequestDataZeroCopy: Boolean;
var
  I, J, SpacePos: Integer;
  PRawData, PEnd, PLine, PValue: PAnsiChar;
  LineSize, ValueSize: Integer;

  function ExtractLine(out Line: PAnsiChar): Integer;
  begin
    Result := 0;
    while (PRawData^ in [#0, #10, #13]) do Inc(PRawData);
    Line := PRawData;
    while (PRawData < PEnd) and not (PRawData^ in [#0, #10, #13]) do
    begin
      Inc(Result);
      Inc(PRawData);
    end;
  end;

  function CopyRawString(P: PAnsiChar; Size: Integer): string;
  var
    i: Integer;
  begin
    i := 1;
    SetLength(Result, Size);
    while (i <= Size) do
    begin
      Result[i] := Char(P^);
      Inc(P);
      Inc(i);
    end;
  end;

begin
  PRawData := Pointer(FRawRequestText.DataString);
  PEnd := PRawData + FRawRequestText.Size;
  LineSize := ExtractLine(PLine);
  if (LineSize = 0) then Exit(False);

  I := 0;
  while (I < LineSize) and (PLine[I] <> ' ') do Inc(I);
  // 请求方法(GET, POST, PUT, HEAD...)
  FMethod := UpperCase(CopyRawString(PLine, I));
  Inc(I);
  while (I < LineSize) and (PLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I < LineSize) and (PLine[I] <> ' ') do
    Inc(I);
  // 请求参数及路径
  FPathAndParams := URLDecode(CopyRawString(PLine + J, I - J));
  // 请求路径
  FPath := FPathAndParams;
  // 解析参数
  J := Pos('?', FPath);
  if (J <= 0) then
    FParams := ''
  else begin
    FParams := Copy(FPath, J + 1, Length(FPath));
    FPath   := Copy(FPath, 1, J - 1);
  end;
  Inc(I);
  while (I < LineSize) and (PLine[I] = ' ') do
    Inc(I);
  J := I;
  while (I < LineSize) and (PLine[I] <> ' ') do
    Inc(I);
  // 请求的HTTP版本
  FVersion := Trim(UpperCase(CopyRawString(PLine + J, I - J)));
  if (FVersion = '') then
    FVersion := 'HTTP/1.0';
  if (FVersion = 'HTTP/1.0') then
    FHttpVerNum := 10
  else
    FHttpVerNum := 11;
  FKeepAlive := (FHttpVerNum = 11);

  FRequestHasContentLength := False;
  FRequestContentLength := 0;
  while True do
  begin
    LineSize := ExtractLine(PLine);
    if (LineSize = 0) then Break;

    SpacePos := 0;
    while (SpacePos < LineSize) and (PLine[SpacePos] <> ' ') do Inc(SpacePos);
    Inc(SpacePos);
    ValueSize := LineSize - SpacePos;
    PValue := PLine + SpacePos;

    if StrLIComp(PLine, 'content-type:', 13) = 0 then
      FRequestContentType := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'content-length:', 15) = 0 then
    begin
      FRequestHasContentLength := TRUE;
      FRequestContentLength := StrToInt64Def(CopyRawString(PValue, ValueSize), -1);
    end
    else if StrLIComp(PLine, 'Accept:', 7) = 0 then
      FRequestAccept:= CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'Referer:', 8) = 0 then
      FRequestReferer := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'Accept-Language:', 16) = 0 then
      FRequestAcceptLanguage := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'Accept-Encoding:', 16) = 0 then
      FRequestAcceptEncoding := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'User-Agent:', 11) = 0 then
      FRequestUserAgent := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'Authorization:', 14) = 0 then
      FRequestAuth := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'Cookie:', 7) = 0 then
      FRequestCookies := CopyRawString(PValue, ValueSize)
    else if StrLIComp(PLine, 'Host:', 5) = 0 then
    begin
      FRequestHost := CopyRawString(PValue, ValueSize);
      J := Pos(':', FRequestHost);
      if J > 0 then
      begin
        FRequestHostName := Copy(FRequestHost, 1, J - 1);
        FRequestHostPort := Copy(FRequestHost, J + 1, 100);
      end else
      begin
        FRequestHostName := FRequestHost;
        FRequestHostPort := IntToStr(TIocpHttpServer(Owner).Port);
      end;
    end
    else if StrLIComp(PLine, 'Connection:', 11) = 0 then
    begin
      FRequestConnection := CopyRawString(PValue, ValueSize);
      // HTTP/1.0 默认KeepAlive=False，只有显示指定了Connection: keep-alive才认为KeepAlive=True
      // HTTP/1.1 默认KeepAlive=True，只有显示指定了Connection: close才认为KeepAlive=False
      if FHttpVerNum = 10 then
        FKeepAlive := SameText(FRequestConnection, 'keep-alive')
      else if SameText(FRequestConnection, 'close') then
        FKeepAlive := False;
    end
    else if StrLIComp(PLine, 'X-Forwarded-For:', 16) = 0 then
      FXForwardedFor := CopyRawString(PValue, ValueSize);
  end;

  Result := True;
end;}

procedure TIocpHttpConnection.Reset;
begin
  FRawRequestText.Size := 0;
  FRequestPostData.Size := 0;

  FHttpState := hcRequest;
  FResponseSize := 0;
  FResponseSent := 0;

  FMethod := '';
  FPath := '';
  FParams := '';
  FPathAndParams := '';
  FVersion := '';
  FHttpVerNum := 0;
  FKeepAlive := False;

  FRequestCmdLine := '';
  FRequestHeader.Clear;
  FRequestContentType := '';
  FRequestHasContentLength := False;
  FRequestContentLength := 0;
  FRequestAccept := '';;
  FRequestReferer := '';
  FRequestAcceptLanguage := '';
  FRequestAcceptEncoding := '';
  FRequestUserAgent := '';
  FRequestAuth := '';
  FRequestCookies := '';
  FRequestHost := '';
  FRequestHostName := '';
  FRequestHostPort := '';
  FRequestConnection := '';
  FXForwardedFor := '';

  FAcceptPostData := False;
  FPostDataSize := 0;
end;

function TIocpHttpConnection.AnswerBuf(const Header: string;
  Buf: Pointer; Size: Integer): Boolean;
var
  FixedHeader: RawByteString;
  Len{, BlockSize}: Integer;
begin
  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if (Header <> '') then
  begin
    FixedHeader := RawByteString(FixHeader(Header));
    Len := Length(FixedHeader);
    Result := (Send(FixedHeader) = Len);
    if not Result then Exit;
    FResponseSize := Len + Size;
  end else
  begin
    if (Buf = nil) or (Size <= 0) then Exit(False);
    FResponseSize := Size;
  end;

  try
(*    while (Size > 0) do
    begin
      BlockSize := Min(4096, Size);
      if (Send(Buf, BlockSize) <> BlockSize) then Exit(False);
      Inc(PAnsiChar(Buf), BlockSize);
      Dec(Size, BlockSize);
    end;
    Result := True;*)

    Result := (Send(Buf, Size) = Size);
  except
    on e: Exception do
    begin
      Result := False;
      AppendLog('TIocpHttpConnection.AnswerBuf: %s=%s', [e.ClassName, e.Message], ltException);
    end;
  end;
end;

function TIocpHttpConnection.AnswerStream(const Header: string;
  Stream: TStream): Boolean;
var
  FixedHeader: RawByteString;
  Len: Integer;
begin
  // FResponseSize必须准确指定发送的数据包大小
  // 用于在发送完之后(Owner.TriggerClientSentData)断开客户端连接
  if (Header <> '') then
  begin
    FixedHeader := RawByteString(FixHeader(Header));
    Len := Length(FixedHeader);
    Result := (Send(FixedHeader) = Len);
    if not Result then Exit;
    FResponseSize := Len + Stream.Size;
  end else
  begin
    if not Assigned(Stream) or (Stream.Size <= 0) then Exit(False);
    FResponseSize := Stream.Size;
  end;

  try
    Result := (Send(Stream) = Stream.Size);
  except
    on e: Exception do
    begin
      Result := False;
      AppendLog('TIocpHttpConnection.AnswerStream: %s=%s', [e.ClassName, e.Message], ltException);
    end;
  end;
end;

function TIocpHttpConnection.AnswerBytes(const Status, ContType, Header: string;
  Data: TBytes): Boolean;
var
  Size: Integer;
begin
  Size := Length(Data);
  Result := AnswerBuf(MakeHeader(Status, ContType, Header, Size), Pointer(Data), Size);
end;

function TIocpHttpConnection.AnswerBytes(const Header: string;
  Data: TBytes): Boolean;
begin
  Result := AnswerBuf(Header, Pointer(Data), Length(Data));
end;

function TIocpHttpConnection.AnswerHTML(const Status, ContType, Header: string;
  HTML: RawByteString): Boolean;
var
  Size: Integer;
begin
  Size := Length(HTML);
  Result := AnswerBuf(MakeHeader(Status, ContType, Header, Size), Pointer(HTML), Size);
end;

function TIocpHttpConnection.AnswerHTML(const Status, ContType, Header, HTML: string): Boolean;
begin
  Result := AnswerHTML(Status, ContType, Header, RawByteString(HTML));
end;

function TIocpHttpConnection.AnswerHTML(const Header: string;
  HTML: RawByteString): Boolean;
begin
  Result := AnswerBuf(Header, Pointer(HTML), Length(HTML));
end;

function TIocpHttpConnection.AnswerHTML(const Header, HTML: string): Boolean;
begin
  Result := AnswerHTML(Header, RawByteString(HTML));
end;

function TIocpHttpConnection.AnswerDocument(const FileName: string): Boolean;
var
  Header: string;
  Stream: TFileStream;
begin
  Result := False;
  try
    Stream := Classes.TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    AnswerError(404);
    Exit;
  end;

  try
    Header :=
      FVersion + ' 200 OK' + #13#10 +
      'Content-Type: file' + #13#10 +
      'Content-Length: ' + IntToStr(Stream.Size) + #13#10 +
      'Date: ' + RFC1123_Date(Now) + #13#10;

    Result := AnswerStream(Header, Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIocpHttpConnection.Answer400;
begin
  AnswerHTML('400 Bad Request', 'text/plain', '', '400 Bad Request');
end;

procedure TIocpHttpConnection.Answer401;
begin
  AnswerHTML('401 Access Denied', 'text/plain', '', '401 Access Denied');
end;

procedure TIocpHttpConnection.Answer403;
begin
  AnswerHTML('403 Forbidden', 'text/plain', '', '403 Forbidden');
end;

procedure TIocpHttpConnection.Answer404;
begin
  AnswerHTML('404 Not Found', 'text/plain', '', '404 Not Found');
end;

procedure TIocpHttpConnection.Answer501;
begin
  AnswerHTML('501 Unimplemented', 'text/plain', '', '501 Unimplemented');
end;

procedure TIocpHttpConnection.Answer503;
begin
  AnswerHTML('503 Server Unavailable', 'text/plain', '', '503 Server Unavailable');
end;

procedure TIocpHttpConnection.Answer506;
begin
  AnswerHTML('506 Server Unavailable', 'text/plain', '', '506 Request Failed');
end;

function TIocpHttpConnection.AnswerError(ErrCode: Integer): Boolean;
begin
  Result := True;

  case ErrCode of
    400: Answer400;
    401: Answer401;
    403: Answer403;
    404: Answer404;
    501: Answer501;
    503: Answer503;
    506: Answer506;
  else
    AnswerHTML(
      Format('%d System Error', [ErrCode]),
      'text/plain', '',
      Format('%d System Error', [ErrCode]));
  end;
end;

{ TIocpHttpRequest }

constructor TIocpHttpRequest.Create(Client: TIocpHttpConnection);
begin
  Self.Client := Client;
end;

procedure TIocpHttpRequest.Execute(Thread: TProcessorThread);
begin
  TIocpHttpServer(Client.Owner).DoOnRequest(Client);
  Client.Release;
end;

{ TIocpHttpServer }

constructor TIocpHttpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  InitAcceptNum := IOCP_HTTP_INIT_ACCEPT_NUM;
  ConnectionClass := TIocpHttpConnection;
end;

procedure TIocpHttpServer.DoOnRequest(Client: TIocpHttpConnection);
var
  RealPath, RequestPath: string;
begin
  if Assigned(FOnRequest) then
  begin
    FOnRequest(Self, Client);
    Exit;
  end;

  // 在这里响应客户端的请求
  if (Client.Method = 'GET') then
  begin
    RequestPath := StringReplace(Client.Path, '//', '/', [rfReplaceAll]);
    RealPath := ExpandFileName(RootDir) + UnixPathToDosPath(Copy(RequestPath, 2, Length(RequestPath)));
    if (IsDirectory(RealPath)) then
    begin
      if not Client.AnswerHTML('', '', '', RawByteString(BuildDirList(RealPath, RequestPath))) then
      begin
        Client.Answer404;
      end;
    end else
    begin
      if not FileExists(RealPath) or not Client.AnswerDocument(RealPath) then
      begin
        Client.Answer404;
      end;
    end;
  end else
  begin
    Client.AnswerHTML('', '', '', 'Hello World!');
  end;
end;

function TIocpHttpServer.GetRootDir: string;
begin
  if (FRootDir = '') then
    FRootDir := TDirectory.GetCurrentDirectory;
  if (FRootDir[Length(FRootDir)] <> TPath.DirectorySeparatorChar) then
    FRootDir := FRootDir + TPath.DirectorySeparatorChar;
  Result := FRootDir;
end;

function TIocpHttpServer.IsValidHttpRequest(buf: PAnsiChar;
  len: Integer): Boolean;
begin
  // HTTP 1.1 支持8种请求
  Result := (len > 7) and
    ((StrLIComp(buf, 'GET', 3) = 0) or
    (StrLIComp(buf, 'POST', 4) = 0) or
    (StrLIComp(buf, 'PUT', 3) = 0) or
    (StrLIComp(buf, 'HEAD', 4) = 0) or
    (StrLIComp(buf, 'OPTIONS', 7) = 0) or
    (StrLIComp(buf, 'DELETE', 6) = 0) or
    (StrLIComp(buf, 'TRACE', 5) = 0) or
    (StrLIComp(buf, 'CONNECT', 7) = 0));
end;

procedure TIocpHttpServer.ParseRecvData(Client: TIocpHttpConnection;
  buf: Pointer; len: Integer);
var
  pch: PAnsiChar;
  ch: AnsiChar;
  CR, LF: Integer;
begin
  // 在这里解析客户端浏览器发送过来的请求数据
  if (Client.FHttpState = hcDone) then
    Client.Reset;

  // 如果不是有效的Http请求直接断开
  if (Client.RawRequestText.Size = 0) and not IsValidHttpRequest(buf, len) then
  begin
    Client.Disconnect;
    Exit;
  end;

  pch := buf;
  CR := 0;
  LF := 0;
  while (len > 0) and (Client.FHttpState <> hcDone) do
  begin
    ch := pch^;

    if (ch = #13) then
      Inc(CR)
    else if (ch = #10) then
      Inc(LF)
    else
    begin
      CR := 0;
      LF := 0;
    end;

    if (Client.FHttpState = hcRequest) then
    begin
      Client.RawRequestText.Write(ch, 1);
      if (CR = 2) and (LF = 2) then
      begin
        if not Client.ParseRequestData then
        begin
          Client.Disconnect;
          Exit;
        end;
{        if not Client.ParseRequestDataZeroCopy then
        begin
          Client.Disconnect;
          Exit;
        end;}
        if SameText(Client.Method, 'POST') or
          SameText(Client.Method, 'PUT') then
        begin
          // 无效的Post请求直接断开
          if (Client.FRequestContentLength <= 0) then
          begin
            Client.Disconnect;
            Exit;
          end;
          Client.FRequestPostData.Size := 0;
          Client.FPostDataSize := 0;
          Client.FHttpState := hcPostData;
          Client.FAcceptPostData := TriggerAcceptPostData(Client.FRequestContentLength);
        end else
          Client.FHttpState := hcDone;
      end;

      Dec(len);
      Inc(pch);
    end else
    if (Client.FHttpState = hcPostData) then
    begin
      Inc(Client.FPostDataSize, len);
      if Client.FAcceptPostData then
        Client.FRequestPostData.Write(pch^, len);

      if (Client.FPostDataSize >= Client.FRequestContentLength) then
        Client.FHttpState := hcDone;

      // Post数据直接剩余部分整段处理，到这里就已经全部处理完了，直接跳出循环
      Break;
    end;
  end;

  // 在解析完请求数据之后再调用线程池
  if (Client.FHttpState = hcDone) then
  begin
    {$ifdef __LOGIC_THREAD_POOL__}
    if (Client.AddRef = 1) then Exit;
    FJobThreadPool.AddRequest(TIocpHttpRequest.Create(Client));
    {$else}
    DoOnRequest(Client);
    {$endif}
  end;
end;

{$ifdef __LOGIC_THREAD_POOL__}
procedure TIocpHttpServer.StartupWorkers;
begin
  if not Assigned(FJobThreadPool) then
    FJobThreadPool := TIocpThreadPool.Create;

  inherited StartupWorkers;
end;

procedure TIocpHttpServer.ShutdownWorkers;
begin
  inherited ShutdownWorkers;

  if Assigned(FJobThreadPool) then
  begin
    FJobThreadPool.Shutdown;
    FreeAndNil(FJobThreadPool);
  end;
end;
{$endif}

function TIocpHttpServer.TriggerAcceptPostData(DataSize: Int64): Boolean;
begin
  if Assigned(FAcceptPostData) then
    Result := FAcceptPostData(Self, DataSize)
  else
    Result := True;
end;

function TIocpHttpServer.TriggerClientRecvData(Client: TIocpSocketConnection;
  buf: Pointer; len: Integer): Boolean;
begin
  ParseRecvData(TIocpHttpConnection(Client), buf, len);
  Result := True;
end;

function TIocpHttpServer.TriggerClientSentData(Client: TIocpSocketConnection;
  buf: Pointer; len: Integer): Boolean;
begin
  with TIocpHttpConnection(Client) do
  begin
    // 如果客户端是HTTP/1.0的请求，回复完数据之后需要断开连接
    // Apache提供的ab测试程序就是使用的HTTP/1.0，如果这里不断开，ab无法正常测试，
    // 会一直等到超时退出
    Inc(FResponseSent, len);
    if not KeepAlive and (FResponseSize > 0) and (FResponseSent >= FResponseSize) then
      Disconnect;
  end;

  Result := True;
end;

end.
