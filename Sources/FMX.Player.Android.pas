unit FMX.Player.Android;

interface

uses
  FMX.BASS, FMX.Player.Shared;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
    function InitBass(Handle: Pointer): Boolean; override;
  end;

implementation

{ TFMXPlatformPlayer }

function TFMXPlatformPlayer.InitBass(Handle: Pointer): Boolean;
begin
  Result := BASS_Init(Device, Freq, Flags, Handle, nil);
end;

end.

