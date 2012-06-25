program TestIocp;

uses
  Forms,
  uMain in 'uMain.pas' {fmTestIocp},
  IocpTcpServer in '..\IOCP\IocpTcpServer.pas',
  IocpTcpClient in '..\IOCP\IocpTcpClient.pas',
  IocpFileClient in '..\IOCP\IocpFileClient.pas',
  IocpFileConst in '..\IOCP\IocpFileConst.pas',
  IocpFileServer in '..\IOCP\IocpFileServer.pas',
  IocpHttpServer in '..\IOCP\IocpHttpServer.pas',
  IocpHttpClient in '..\IOCP\IocpHttpClient.pas',
  IocpHttpUtils in '..\IOCP\IocpHttpUtils.pas',
  IocpHttpTunnel in '..\IOCP\IocpHttpTunnel.pas',
  IocpReadWriteLocker in '..\IOCP\IocpReadWriteLocker.pas',
  IocpTcpSocket in '..\IOCP\IocpTcpSocket.pas',
  IocpMemoryPool in '..\IOCP\IocpMemoryPool.pas',
  IocpBuffer in '..\IOCP\IocpBuffer.pas',
  IocpApiFix in '..\IOCP\IocpApiFix.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmTestIocp, fmTestIocp);
  Application.Run;
end.
