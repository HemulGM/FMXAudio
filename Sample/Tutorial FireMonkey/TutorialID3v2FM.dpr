program TutorialID3v2FM;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1: TForm3D},
  ID3v1Library in '..\ID3v1Library.pas',
  ID3v2Library in '..\ID3v2Library.pas',
  BufferedStream in '..\BufferedStream.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
