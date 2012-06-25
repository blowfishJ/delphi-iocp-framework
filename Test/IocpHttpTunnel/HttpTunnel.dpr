program HttpTunnel;

uses
  Vcl.Forms,
  IocpHttpTunnelMain in 'IocpHttpTunnelMain.pas' {fmIocpHttpTunnel},
  uIocpHttpTunnelConfig in 'uIocpHttpTunnelConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmIocpHttpTunnel, fmIocpHttpTunnel);
  Application.Run;
end.
