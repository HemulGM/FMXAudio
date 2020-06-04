program VCLSample;

uses
  Vcl.Forms,
  VCLSampleMain in 'VCLSampleMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
