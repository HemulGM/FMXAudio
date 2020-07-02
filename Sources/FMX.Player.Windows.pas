unit FMX.Player.Windows;

interface

{$IFDEF MSWINDOWS}
uses
  FMX.Player.Shared, FMX.Platform.Win, FMX.BASS, FMX.BASS.AAC;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
  protected
    function InitBass(Handle: Pointer): Boolean; override;
  end;
{$ENDIF}

implementation

{ TFMXPlatformPlayer }

{$IFDEF MSWINDOWS}

function TFMXPlatformPlayer.InitBass(Handle: Pointer): Boolean;
begin
  Result := BASS_Init(Device, Freq, Flags, WindowHandleToPlatform(Handle).Wnd, nil);
end;

{$ENDIF}
end.

