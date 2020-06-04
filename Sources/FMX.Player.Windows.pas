unit FMX.Player.Windows;

interface

uses
  FMX.Player.Shared, FMX.Platform.Win, FMX.BASS, FMX.BASS.AAC;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
  protected
    function FInitBass(Handle: Cardinal): Boolean;
    function InitBass(Handle: Pointer): Boolean; override;
  public
    function Init(Handle: Cardinal): Boolean; overload;
  end;

implementation

{ TFMXPlatformPlayer }

function TFMXPlatformPlayer.FInitBass(Handle: Cardinal): Boolean;
begin
  Result := BASS_Init(Device, Freq, Flags, Handle, nil);
end;

function TFMXPlatformPlayer.Init(Handle: Cardinal): Boolean;
begin
  Result := False;
  if BASS_Available then
  begin
    if FInitBass(Handle) then
    begin
      BASS_PluginLoad(PChar(BASS_AAC_Lib), 0 or BASS_UNICODE);
      BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
      BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
      Result := True;
    end;
  end;
  FIsInit := Result;
end;

function TFMXPlatformPlayer.InitBass(Handle: Pointer): Boolean;
begin
  Result := BASS_Init(Device, Freq, Flags, WindowHandleToPlatform(Handle).Wnd, nil);
end;

end.

