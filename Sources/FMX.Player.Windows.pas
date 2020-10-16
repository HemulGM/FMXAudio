unit FMX.Player.Windows;

interface

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows, System.Classes, FMX.Forms, FMX.Player.Shared, FMX.Platform.Win,
  FMX.BASS, FMX.BASS.AAC;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
  protected
    function InitBass(Handle: Pointer = nil): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
{$ENDIF}

implementation

{ TFMXPlatformPlayer }

{$IFDEF MSWINDOWS}

constructor TFMXPlatformPlayer.Create(AOwner: TComponent);
begin
  inherited;
end;

function TFMXPlatformPlayer.InitBass(Handle: Pointer): Boolean;
var
  WinHWND: HWND;
begin
  if Handle = nil then
    WinHWND := 0 //WindowHandleToPlatform(Application.MainForm.Handle).Wnd
  else
    WinHWND := WindowHandleToPlatform(Handle).Wnd;

  Result := BASS_Init(Device, Freq, Flags, WinHWND, nil);
end;

{$ENDIF}
end.

