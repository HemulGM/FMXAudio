unit FMX.Player.Windows;

interface

uses
  FMX.Player.Shared, FMX.Platform.Win, FMX.BASS;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
    function InitBass(Handle: Pointer): Boolean; override;
  end;

implementation

{ TFMXPlatformPlayer }

function TFMXPlatformPlayer.InitBass(Handle: Pointer): Boolean;
begin
  Result := BASS_Init(Device, Freq, Flags, WindowHandleToPlatform(Handle).Wnd, nil);
end;

end.

