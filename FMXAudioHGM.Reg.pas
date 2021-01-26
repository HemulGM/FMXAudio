unit FMXAudioHGM.Reg;

interface

uses
  System.Classes;

procedure Register;

implementation

uses
  FMX.BassComponents;

procedure Register;
begin
  RegisterComponents('Bass Audio HGM', [TFMXPlayer]);
  RegisterComponents('Bass Audio HGM', [TBassRecorder]);
end;

end.
