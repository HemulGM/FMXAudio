program Project21;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit14 in 'Unit14.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
