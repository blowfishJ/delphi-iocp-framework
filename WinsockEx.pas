unit WinsockEx;

interface

uses
  Windows, Classes, SysUtils, JwaWinsock2;

const
  WSAID_ACCEPTEX: TGUID = (D1:$b5367df1;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_CONNECTEX: TGUID = (D1:$25a207b9;D2:$ddf3;D3:$4660;D4:($8e,$e9,$76,$e5,$8c,$74,$06,$3e));
  WSAID_DISCONNECTEX: TGUID = (D1:$7fda2e11;D2:$8630;D3:$436f;D4:($a0,$31,$f5,$36,$a6,$ee,$c1,$57));
  WSAID_GETACCEPTEXSOCKADDRS: TGUID = (D1:$b5367df2;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));

  SO_UPDATE_CONNECT_CONTEXT = $7010;
  
type
  EWinsockExError = class(Exception);

  EWinsockExStubError = class(EWinsockExError)
  protected
    FWin32Error: DWORD;
    FWin32ErrorMessage: string;
    FTitle: string;
  public
    constructor Build(AWin32Error: DWORD; const ATitle: string; AArgs: array of const);
    property Win32Error: DWORD read FWin32Error;
    property Win32ErrorMessage: string read FWin32ErrorMessage;
    property Title: string read FTitle;
  end;

  LPFN_ACCEPTEX = function(sListenSocket, sAcceptSocket: TSocket;
    lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength,
    dwRemoteAddressLength: DWORD; var lpdwBytesReceived: DWORD;
    lpOverlapped: POverlapped): BOOL; stdcall;
  LPFN_GETACCEPTEXSOCKADDRS = procedure(lpOutputBuffer: Pointer;
    dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
    var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer;
    var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); stdcall;
  LPFN_CONNECTEX = function(const s: TSocket; const name: PSOCKADDR; const namelen: Integer; lpSendBuffer: Pointer; dwSendDataLength: DWORD; var lpdwBytesSent: DWORD; lpOverlapped: LPWSAOVERLAPPED): BOOL; stdcall;
  LPFN_DISCONNECTEX = function(const hSocket: TSocket; AOverlapped: Pointer; const dwFlags: DWORD; const dwReserved: DWORD): BOOL; stdcall;

var
  AcceptEx : LPFN_ACCEPTEX = nil;
  GetAcceptExSockaddrs : LPFN_GETACCEPTEXSOCKADDRS = nil;
  ConnectEx : LPFN_CONNECTEX = nil;
  DisconnectEx : LPFN_DISCONNECTEX = nil;

implementation

{ EWinsockExStubError }

constructor EWinsockExStubError.Build(AWin32Error: DWORD; const ATitle: string;
  AArgs: array of const);
begin
  FTitle := Format(ATitle, AArgs);
  FWin32Error := AWin32Error;
  if AWin32Error = 0 then begin
    inherited Create(FTitle);
  end else
  begin
    FWin32ErrorMessage := IntToStr(AWin32Error) + ':' + SysUtils.SysErrorMessage(AWin32Error);
    inherited Create(FTitle + ': ' + FWin32ErrorMessage);    {Do not Localize}
  end;
end;

function FixupStubEx(hSocket: TSocket; const AName: string; const AGuid: TGUID): Pointer;
var
  LStatus: LongInt;
  LBytesSend: DWORD;
begin
  LStatus := WSAIoctl(hSocket, SIO_GET_EXTENSION_FUNCTION_POINTER, @AGuid, SizeOf(TGUID),
    @Result, sizeof(Pointer), @LBytesSend, nil, nil);
  if LStatus <> 0 then begin
    raise EWinsockExStubError.Build(WSAGetLastError, '调用Winsock2函数失败 Socket=%d, ERR=%s', [hSocket, AName]);
  end;
end;

function Stub_AcceptEx(sListenSocket, sAcceptSocket: TSocket;
  lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength,
  dwRemoteAddressLength: DWORD; var lpdwBytesReceived: DWORD;
  lpOverlapped: POverlapped): BOOL; stdcall;
begin
  {RLebeau - loading GetAcceptExSockaddrs() first in case it fails}
  @GetAcceptExSockaddrs := FixupStubEx(sListenSocket, 'GetAcceptExSockaddrs', WSAID_GETACCEPTEXSOCKADDRS); {Do not localize}
  @AcceptEx := FixupStubEx(sListenSocket, 'AcceptEx', WSAID_ACCEPTEX); {Do not localize}
  Result := AcceptEx(sListenSocket, sAcceptSocket, lpOutputBuffer, dwReceiveDataLength,
    dwLocalAddressLength, dwRemoteAddressLength, lpdwBytesReceived, lpOverlapped);
end;

function Stub_ConnectEx(const s : TSocket; const name: PSockAddr; const namelen: Integer; lpSendBuffer : Pointer;
  dwSendDataLength : DWORD; var lpdwBytesSent : DWORD; lpOverlapped : LPWSAOVERLAPPED) : BOOL;  stdcall;
begin
  @ConnectEx := FixupStubEx(s, 'ConnectEx', WSAID_CONNECTEX); {Do not localize}
  Result := ConnectEx(s, name, namelen, lpSendBuffer, dwSendDataLength, lpdwBytesSent, lpOverlapped);
end;

function Stub_DisconnectEx(const s : TSocket; AOverlapped: Pointer; const dwFlags : DWord; const dwReserved : DWORD) : BOOL;  stdcall;
begin
  @DisconnectEx := FixupStubEx(s, 'DisconnectEx', WSAID_DISCONNECTEX); {Do not localize}
  Result := DisconnectEx(s, AOverlapped, dwFlags, dwReserved);
end;

function MakeWord(a, b: Byte): Word;
begin
  Result := A shl 8 or B;
end;

procedure WinsockExStartup;
var
  WSData: TWSAData;
begin
  ZeroMemory(@WSData, sizeof(WSData));
  if (WSAStartup(MakeWord(2, 2), WSData) <> 0) then
    raise EWinsockExError.Create('初始化Winsock库失败.' + SysErrorMessage(WSAGetLastError));
  if (WSData.wVersion < 2) then
  begin
    WSACleanup;
    raise EWinsockExError.CreateFmt('Winsock版本号过低，最低要求Winsock2.0，系统中的版本为：%d', [WSData.wVersion]);
  end;
end;

procedure WinsockExCleanup;
begin
  WSACleanup;
end;

procedure InitializeWinsockEx;
begin
  AcceptEx     := Stub_AcceptEx;
  ConnectEx    := Stub_ConnectEx;
  DisconnectEx := Stub_DisconnectEx;
end;

initialization
  WinsockExStartup;
  InitializeWinsockEx;

finalization
  WinsockExCleanup;

end.
