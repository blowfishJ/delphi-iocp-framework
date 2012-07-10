unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  IocpTcpSocket, IocpTcpServer, IocpTcpClient, IocpFileServer, IocpFileClient, IocpHttpServer,
  IocpHttpUtils,
  VaniTCPDefine, Spin, StdCtrls, uGlobalVars, IocpUtils, OverbyteIcsHttpSrv, OverbyteIcsLibrary,
  IocpHttpClient, IocpLogger, IocpThreadPool, IocpHttpTunnel;

const
  WM_LOG = WM_USER + 100;

type
  TTestServer = class(TVaniFileServer)
  protected
    procedure DoOnRequest(Client: TVaniServerClientSocket; Request, Response: TVaniTCPPack); override;
  end;

  TTestClient = class(TVaniFileClient)
  protected
    procedure DoOnResponse(Client: TVaniClientSocket; Response: TVaniTCPPack); override;

    procedure TriggerClientConnected(Client: TIocpSocketConnection); override;
    procedure TriggerClientDisconnected(Client: TIocpSocketConnection); override;
  end;

  THttpDirEntry = class
    Visible   : Boolean;    { TRUE if the entry is to be shown in list  }
    Name      : string;
    SizeLow   : Cardinal;
    SizeHigh  : Cardinal;
    Year      : Integer;
    Month     : Integer;
    Day       : Integer;
    Hour      : Integer;
    Min       : Integer;
    Sec       : Integer;
    VolumeID  : Boolean;
    Directory : Boolean;
    ReadOnly  : Boolean;
    SysFile   : Boolean;
    Hidden    : Boolean;   { File is hidden, not the same as Visible !  }
  end;

  TTestHttpServer = class(TIocpHttpServer)
  protected
    procedure TriggerBeforePostData(DataSize: Int64; var AcceptPostData: Boolean); override;
    procedure DoOnRequest(Client: TIocpHttpConnection; WorkThread: TProcessorThread); override;
    function  FormatDirEntry(const Path: string; F: THttpDirEntry): string; virtual;
    function  BuildDirList(const RootDir, Path: string): string; virtual;
  public
    destructor Destroy; override;
  end;

  TGetFileThread = class(TThread)
  private
    FRemoteFile: string;
    FMsg: string;
  protected
    procedure Execute; override;
    procedure ReportMsg;
  public
    constructor Create(const RemoteFile: string); reintroduce;
  end;

  TPutFileThread = class(TThread)
  private
    FLocalFile: string;
    FMsg: string;
  protected
    procedure Execute; override;
    procedure ReportMsg;
  public
    constructor Create(const LocalFile: string); reintroduce;
  end;

  TTestLineSocket = class(TIocpLineSocket)
  protected
    procedure DoOnRecvLine(Client: TIocpLineSocketConnection; Line: string; WorkThread: TProcessorThread); override;
  end;

  TTestHttpTunnel = class(TIocpHttpTunnel)
  protected
    procedure TriggerBeforeTransmit(Client: TIocpHttpTunnelConnection;
      out Transmit: Boolean; out ServerAddr: string; out ServerPort: Word); override;
  end;

  TfmTestIocp = class(TForm)
    Memo1: TMemo;
    edtServerPort: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    edtServerIP: TEdit;
    edtSendPackSize: TSpinEdit;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    edtURL: TEdit;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    edtWeb: TEdit;
    Button12: TButton;
    Button13: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
  private
    FServer: TTestServer;
    FClient: TTestClient;
    FHttpServer: TTestHttpServer;
    FHttpClient: TIocpHttpClient;
    FLineSocket: TTestLineSocket;
    FHttpTunnel: TTestHttpTunnel;

    procedure WMLog(var Msg: TMessage); message WM_LOG;
  public
    procedure Log(const s: string);
  end;

var
  fmTestIocp: TfmTestIocp;

implementation

{$R *.dfm}

procedure TfmTestIocp.Button10Click(Sender: TObject);
var
  Conn: TIocpLineSocketConnection;
begin
  Conn := FLineSocket.Connect('127.0.0.1', 9981);
  Conn.Send('test 12345 zxy'#13#10'222222222'#13#10#13#10#13#10);
end;

procedure TfmTestIocp.Button11Click(Sender: TObject);
begin
  FHttpClient.DisconnectAll;
  FHttpClient.ServerAddr := edtWeb.Text;
end;

procedure TfmTestIocp.Button12Click(Sender: TObject);
var
  p: TIocpHttpServer;
begin
  p := nil;
  FreeAndNil(p);
  ShowMessage('ok');
end;

type
  TTest = class
  private
    FStuff: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

  public
    aaa: Integer;
    bbb: string;
    ccc: Boolean;
  end;

procedure TfmTestIocp.Button13Click(Sender: TObject);
var
  p: TTest;
  Ofs: Integer;
begin
  p := nil;
  Ofs := Integer(@p.bbb);
  ShowMessageFmt('%d', [Ofs]);
end;

procedure TfmTestIocp.Button1Click(Sender: TObject);
begin
  FClient.ServerIP := edtServerIP.Text;
  FClient.ServerPort := StrToInt(edtServerPort.Text);
end;

procedure TfmTestIocp.Button2Click(Sender: TObject);
begin
  FHttpServer.Start;
  FHttpTunnel.Start;
  FServer.Start;
  FLineSocket.Listen(9981, 10);

  Log('Server start success.')
end;

procedure TfmTestIocp.Button3Click(Sender: TObject);
begin
  FHttpServer.Stop;
  FHttpTunnel.Stop;
  FServer.Stop;
  FLineSocket.ShutdownWorkers;


  Log('Server stop success.')
end;

procedure TfmTestIocp.Button4Click(Sender: TObject);
var
  i, n: Integer;
  Req: TVaniTCPPack;
  RemoteFile: string;
begin
  Req := TVaniTCPPack.Create('test_cmd');
  try
    for i := 1 to edtSendPackSize.Value do
    begin
      Req.Cmd := 'test_cmd ' + IntToStr(i);
      Req.Params['p1'] := RandRange(1, 99999);
      Req.Params['p2'] := True;
      Req.Params['p3'] := 'Hello ' + RandomStr(5, rtStr);
      Req.Params['p4'] := Now;
      n := FClient.AsyncRequest(Req);
      //RemoteFile := GetTempFileName('upload\', 'Server', '.rar');
      //n := FClient.PutFile('Server.rar', RemoteFile);
      Log(Format('%s sent %d bytes', [Req.Cmd, n]));
    end;
  finally
    Req.Free;
  end;
end;

procedure TfmTestIocp.Button5Click(Sender: TObject);
begin
  FClient.DisconnectAll;
  Log('Disconnect.');
end;

procedure TfmTestIocp.Button6Click(Sender: TObject);
var
  i: Integer;
begin
  with TOpenDialog.Create(nil) do
  try
    if Execute then
    begin
      for i := 1 to edtSendPackSize.Value do
        TPutFileThread.Create(FileName);
    end;
  finally
    Free;
  end;
end;

procedure TfmTestIocp.Button7Click(Sender: TObject);
var
  i: Integer;
begin
  DeleteFileOrDir(gAppPath + 'download\*.*');
  for i := 1 to edtSendPackSize.Value do
    TGetFileThread.Create('server.rar');
end;

procedure TfmTestIocp.FormCreate(Sender: TObject);
begin
  ShowConsoleLog(True);
  
  FHttpServer := TTestHttpServer.Create(nil);
  FHttpServer.Port := 80;

  FHttpTunnel := TTestHttpTunnel.Create(nil);
  FHttpTunnel.Port := 8864;  
  FServer := TTestServer.Create(nil);
  FServer.Port := 8877;


  FClient := TTestClient.Create(nil);
  FClient.ServerIP := '127.0.0.1';
  FClient.ServerPort := 8877;

  FHttpClient := TIocpHttpClient.Create(nil);
  FHttpClient.ServerAddr := 'www.vanisoft.net';
  FHttpClient.ServerPort := 80;

  FLineSocket := TTestLineSocket.Create(nil);
  FLineSocket.LineLimit := 256;
end;

procedure TfmTestIocp.FormDestroy(Sender: TObject);
begin
  FHttpServer.Free;
  FHttpTunnel.Free;
  FClient.Free;
  FServer.Free;
  FHttpClient.Free;
  FLineSocket.Free;
end;

procedure TfmTestIocp.Log(const s: string);
var
  ps: PString;
begin
  New(ps);
  ps^ := s;

  PostMessage(Handle, WM_LOG, Integer(ps), 0);
end;

procedure TfmTestIocp.WMLog(var Msg: TMessage);
var
  ps: PString;
begin
  ps := PString(Msg.WParam);

  if (ps <> nil) then
  begin
    Memo1.Lines.Add(ps^);
    Dispose(ps);
  end;
end;

{ TTestServer }
{
procedure TTestServer.TriggerReadComplete(PerHandleData: PIocpPerHandleData;
  PerIoData: PIocpPerIoData);
var
  s: PString;
begin
  inherited;

  New(s);
  SetLength(s^, PerIoData.DataBuf.len);
  CopyMemory(@s^[1], PerIoData.DataBuf.buf, PerIoData.DataBuf.len);

  PostMessage(fmTestIocp.Handle, WM_LOG, Integer(s), 0);
  Send(PerIoData.ClientSocket, 'Hello');
end;
 }
{ TTestClient }
{
procedure TTestClient.TriggerConnectComplete;
begin
  fmTestIocp.Log(Format('connected %s:%d', [RemoteIP, RemotePort]));
end;

procedure TTestClient.TriggerDisconnectComplete;
begin
  fmTestIocp.Log(Format('disconnected %s:%d', [RemoteIP, RemotePort]));
end;

procedure TTestClient.TriggerReadComplete(Buf: Pointer; Size: Integer);
var
  s: string;
begin
  SetLength(s, Size);
  CopyMemory(@s[1], Buf, Size);

  fmTestIocp.Log(s);
end;   }

{ TTestServer }

procedure TTestServer.DoOnRequest(Client: TVaniServerClientSocket; Request,
  Response: TVaniTCPPack);
begin
  inherited DoOnRequest(Client, Request, Response);
  Response.Params['ReqPackSize'] := Request.PackSize;
end;

{ TTestClient }

procedure TTestClient.DoOnResponse(Client: TVaniClientSocket;
  Response: TVaniTCPPack);
var
  Msg: string;
begin
//  Msg := Format('Client %d Recv: %s=%s', [Client.Socket, Response.Cmd, ParamsToStr(Response.Params)]);
//  fmTestIocp.Log(Msg);
end;

procedure TTestClient.TriggerClientConnected(Client: TIocpSocketConnection);
begin
  fmTestIocp.Log(Format('client connected %s:%d', [Client.PeerIP, Client.PeerPort]));
end;

procedure TTestClient.TriggerClientDisconnected(Client: TIocpSocketConnection);
begin
  try
    fmTestIocp.Log(Format('client disconnected %s:%d', [Client.PeerIP, Client.PeerPort]));
  except
  end;
end;

{ TGetFileThread }

constructor TGetFileThread.Create(const RemoteFile: string);
begin
  inherited Create(True);
  FRemoteFile := RemoteFile;
  FreeOnTerminate := True;

  Resume;
end;

procedure TGetFileThread.Execute;
var
  Err: Integer;
  LocalFile: string;
begin
  LocalFile := GetTempFileName(gAppPath + 'download\', '', '.rar');
  Err := fmTestIocp.FClient.GetFile(FRemoteFile, LocalFile);
  FMsg := IntToStr(Err);
  ReportMsg;
end;

procedure TGetFileThread.ReportMsg;
begin
  fmTestIocp.Log(FMsg);
end;

{ TPutFileThread }

constructor TPutFileThread.Create(const LocalFile: string);
begin
  inherited Create(True);
  FLocalFile := LocalFile;
  FreeOnTerminate := True;

  Resume;
end;

procedure TPutFileThread.Execute;
var
  Err: Integer;
  RemoteFile: string;
begin
  RemoteFile := GetTempFileName('upload\', '', '.rar');
  Err := fmTestIocp.FClient.PutFile(FLocalFile, RemoteFile);
  FMsg := IntToStr(Err);
  ReportMsg;
end;

procedure TPutFileThread.ReportMsg;
begin
  fmTestIocp.Log(FMsg);
end;

{ TTestHttpServer }

function TTestHttpServer.BuildDirList(const RootDir, Path: string): string;
var
  Status     : Integer;
  F          : TSearchRec;
  ParentDir  : String;
  DirList    : TStringList;
  FileList   : TStringList;
  Data       : THttpDirEntry;
  I          : Integer;
  Total      : Cardinal;
  TotalBytes : Int64;
  Document: string;
begin
    Document := AbsolutisePath(RootDir +
                                    URLDecode(UnixPathToDosPath(Path)));
    DirList := TStringList.Create;
    FileList := TStringList.Create;
    Status  := _FindFirst(Document + '\*.*', faAnyFile, F);
    while Status = 0 do
    begin
      if (F.Name <> '.') and (F.Name <> '..') then
      begin
        Data           := THttpDirEntry.Create;
        Data.Visible   := TRUE;
        Data.Name      := F.Name;
        Data.SizeLow   := F.Size;
        Data.SizeHigh  := 0;
        Data.Day       := (HIWORD(F.Time) and $1F);
        Data.Month     := ((HIWORD(F.Time) shr 5) and $0F);
        Data.Year      := ((HIWORD(F.Time) shr 9) and $3F) + 1980;
        Data.Sec       := ((F.Time and $1F) shl 1);
        Data.Min       := ((F.Time shr 5) and $3F);
        Data.Hour      := ((F.Time shr 11) and $1F);
        Data.VolumeID  := FALSE; //((F.Attr and faVolumeID)  <> 0);
        Data.Directory := ((F.Attr and faDirectory) <> 0);
        Data.ReadOnly  := ((F.Attr and faReadOnly)  <> 0);
        Data.SysFile   := ((F.Attr and faSysFile)   <> 0);
        Data.Hidden    := ((F.Attr and faHidden)    <> 0);

        if ((F.Attr and faDirectory) <> 0) then
          DirList.AddObject(Data.Name, Data)
        else
          FileList.AddObject(Data.Name, Data);
      end;

      Status  := _FindNext(F);
    end;
    _FindClose(F);
    DirList.Sort;
    FileList.Sort;

    Result :=
      '<HTML>' + #13#10 +
      '<HEAD>' + #13#10 +
      '' + #13#10 +
      '<STYLE TYPE="text/css">' + #13#10 +
      '.dirline { font-family: arial; color: black; font-style: normal; }' + #13#10 +
      '</STYLE>' + #13#10 +
      '<TITLE>Directory List</TITLE>' + #13#10 +
      //'<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' + #13#10 +
      '</HEAD>' + #13#10 +
      '<BODY><P>Directory of ' +
    TextToHtmlText(DosPathToUnixPath(AbsolutisePath(UnixPathToDosPath(UrlDecode(Path))))) +
                       ':</P>' + #13#10 +
              '<TABLE CLASS="dirline">' + #13#10;
    if Path = '/' then
        ParentDir := ''
    else if Path[Length(Path)] = '/' then
        ParentDir := DosPathToUnixPath(_ExtractFilePath(UnixPathToDosPath(Copy(Path, 1, Length(Path) - 1))))
    else
        ParentDir := DosPathToUnixPath(_ExtractFilePath(UnixPathToDosPath(Path)));
    if (ParentDir <> '') and (ParentDir <> '/') then
        SetLength(ParentDir, Length(ParentDir) - 1);
    if ParentDir <> '' then
        Result  := Result + '<TR><TD><A HREF="' + ParentDir +
                         '">[To Parent Directrory]</A></TD></TR>';

    TotalBytes := 0;
    Total      := DirList.Count + FileList.Count;
    if Total <= 0 then
        Result := Result +'<TR><TD>No file</TD></TR>'
    else begin
        for I := 0 to DirList.Count - 1 do begin
            Data   := THttpDirEntry(DirList.Objects[I]);
            Result := Result + '<TR>' + FormatDirEntry(Path, Data) + '</TR>' + #13#10;
            DirList.Objects[I].Free;
        end;
        DirList.Free;

        for I := 0 to FileList.Count - 1 do begin
            Data       := THttpDirEntry(FileList.Objects[I]);
            Result     := Result + '<TR>' + FormatDirEntry(Path, Data) +
                                   '</TR>' + #13#10;
            TotalBytes := TotalBytes + Cardinal(Data.SizeLow);
            FileList.Objects[I].Free;
        end;
        FileList.Free;
        Result := Result + '<TR><TD COLSPAN="8">Total: ' +
                           _IntToStr(Total)      + ' file(s), ' +
                           _IntToStr(TotalBytes) + ' byte(s)</TD></TR>';
    end;

    Result := Result + '</TABLE></BODY></HTML>' + #13#10;
end;

destructor TTestHttpServer.Destroy;
var
  i: Integer;
  Obj: TObject;
begin
  for i := 0 to High(ThreadPool.Threads) do
  begin
    Obj := TObject(ThreadPool.Threads[i].Tag);
    if (Obj <> nil) then
      Obj.Free;
  end;
   
  inherited Destroy;
end;

procedure TTestHttpServer.DoOnRequest(Client: TIocpHttpConnection; WorkThread: TProcessorThread);
var
  RootDir, Document: string;
begin
  // 在这里响应客户端的请求
  if (Client.Method = 'GET') then
  begin
    RootDir := 'd:\';
    Document := AbsolutisePath(RootDir + URLDecode(UnixPathToDosPath(Client.Path)));
    if (IsDirectory(Document)) then
    begin
      if not Client.AnswerHTML('', '', '', BuildDirList(RootDir, Client.Path)) then
        AppendLog('AnswerHTML error %s', [Client.Path]);
    end else
    begin
      if not Client.AnswerDocument(Document) then
        AppendLog('AnswerDocument error %s', [Client.Path]);
    end;
  end;
end;

{procedure TTestHttpServer.DoOnRequest(Client: TIocpHttpConnection; WorkThread: TProcessorThread);
var
  AnswerContent: string;
begin
  // 在这里响应客户端的请求
  if SameText(Client.Path, '/test.jpg') then
  begin
    Client.AnswerDocument('e:\1.jpg');
    Exit;
  end;
  if SameText(Client.Path, '/test.zip') then
  begin
    Client.AnswerDocument('e:\temp\Google+快速上手指南.zip');
    Exit;
  end;
  AnswerContent := Format(
    'Hello World!'#13#10 +
    'Request method: %s'#13#10 +
    'Request path: %s'#13#10 +
    'Request params: %s'#13#10 +
    'Request header:'#13#10#13#10'%s'#13#10 +
    'Request data: %s',
    [Client.Method, Client.Path, Client.Params,
     Client.RequestHeader.Text,
     Client.RequestPostData.DataString]
    );
  Client.AnswerHTML('', 'text/plain', '', AnswerContent);
  Client.Reset;
end;}

{procedure TTestHttpServer.DoOnRequest(Client: TIocpHttpConnection; WorkThread: TProcessorThread);
var
  RecvHeader, RecvBody: string;
begin
AppendLog('%s:%d %s %s', [Client.PeerIP, Client.PeerPort, Client.Method, Client.PathAndParams]);
  SetHeader(Client.RequestHeader, 'Host', fmTestIocp.FHttpClient.ServerAddr);
  if (fmTestIocp.FHttpClient.Request(Client.Method, Client.PathAndParams, Client.RequestHeader.Text, Client.RequestPostData.DataString, RecvHeader, RecvBody)) then
  begin
    if not Client.AnswerHTML(RecvHeader, RecvBody) then
      AppendLog('AnswerHTML failed!!!');
  end;
end;}

function TTestHttpServer.FormatDirEntry(const Path: string; F: THttpDirEntry): string;
var
    Attr             : String;
    Link             : String;
    SizeString       : String;
const
    StrMonth : array [1..12] of String =
        ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    if F.VolumeID or
       (F.Name = '.') or
       (F.Name = '..') then begin
        { Ignore hidden files, volume ID, current and parent dir entries }
        Result := '';
        Exit;
    end;

    Attr := '-rw--';
    if F.Directory then begin
        Attr[1] := 'd';
        SizeString := '';
    end
    else
        SizeString := _IntToStr(F.SizeLow);

    if F.ReadOnly then
        Attr[3] := '-';

    if F.SysFile then
        Attr[4] := 's';

    if F.Hidden then
        Attr[5] := 'h';

{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}

    if Path = '/' then
        Link := '/' + UrlEncode(F.Name)
    else if Path[Length(Path)] = '/' then
        Link := Path + UrlEncode(F.Name)
    else
        Link := Path + '/' + UrlEncode(F.Name);

    Result := '<TD>' + Attr + '</TD>' +
              '<TD ALIGN="right">' + SizeString + '</TD>' +
              '<TD WIDTH="10"></TD>' +
              '<TD>' + _Format('%s %2.2d, %4.4d', [StrMonth[F.Month], F.Day, F.Year]) + '</TD>' +
              '<TD WIDTH="10"></TD>' +
              '<TD>' + _Format('%2.2d:%2.2d:%2.2d',  [F.Hour, F.Min, F.Sec])   + '</TD>' +
              '<TD WIDTH="10"></TD>' +
              '<TD><A HREF="' + Link + '">' +
              TextToHtmlText(F.Name) + '</A></TD>' + #13#10;
end;

procedure TTestHttpServer.TriggerBeforePostData(DataSize: Int64;
  var AcceptPostData: Boolean);
begin
  AcceptPostData := True;
end;

{ TTestLineSocket }

procedure TTestLineSocket.DoOnRecvLine(Client: TIocpLineSocketConnection;
  Line: string; WorkThread: TProcessorThread);
begin
  fmTestIocp.Log(Format('recv %s:%d - %s', [Client.PeerIP, Client.PeerPort, Line]));
end;

{ TTestHttpTunnel }

procedure TTestHttpTunnel.TriggerBeforeTransmit(Client: TIocpHttpTunnelConnection;
  out Transmit: Boolean; out ServerAddr: string; out ServerPort: Word);
begin
  Transmit := not SameText(Client.Path, '/test');

  if not Transmit then
  begin
    Client.AnswerHTML('', '', '', 'Hello World!!!');
  end else
  begin
    ServerAddr := 'www.vanisoft.net';
    ServerPort := 80;
  end;
end;

{ TTest }

constructor TTest.Create;
begin

end;

destructor TTest.Destroy;
begin

  inherited;
end;

end.
