unit FMXAudioHGM.Reg;

interface

uses
  System.Classes;

procedure Register;

implementation

uses
  FMX.Player;

procedure Register;
begin
  RegisterComponents('Bass Audio HGM', [TFMXPlayer]);
end;

end.
