unit IocpDsServerMethods;

interface

uses
  Datasnap.DSServer;

type
  TServerMethodsDs = class(TDSServerModule)
  public
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
  end;

implementation

uses
  System.StrUtils;

{$R *.dfm}

function TServerMethodsDs.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethodsDs.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.
