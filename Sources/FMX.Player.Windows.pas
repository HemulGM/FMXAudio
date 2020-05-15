unit FMX.Player.Windows;

interface

uses
  FMX.Player.Shared, FMX.Types, FMX.Forms, FMX.Platform, FMX.Platform.Win, FMX.BASS, FMX.BASS.AAC;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
  public
    procedure SetVolume(const AValue: Single); override;
    function GetVolume: Single; override;
    function InitBass(Handle: TWindowHandle): Boolean; override;
  end;

implementation

{ TFMXPlatformPlayer }

function TFMXPlatformPlayer.GetVolume: Single;
begin
  Result := BASS_GetVolume;
end;

function TFMXPlatformPlayer.InitBass(Handle: TWindowHandle): Boolean;
begin
  Result := BASS_Init(-1, 44100, 0, WindowHandleToPlatform(Handle).Wnd, nil);
end;

procedure TFMXPlatformPlayer.SetVolume(const AValue: Single);
begin
  BASS_SetVolume(AValue);
end;

end.

