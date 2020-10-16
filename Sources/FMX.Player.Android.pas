unit FMX.Player.Android;

interface

{$IFDEF ANDROID}
uses
  System.Classes, FMX.Forms, FMX.BASS, FMX.Player.Shared;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
  protected
    function InitBass(Handle: Pointer = nil): Boolean; override;
  public
    constructor Create(AOwner: TComponent);
  end;

{$ENDIF}

implementation

{ TFMXPlatformPlayer }

{$IFDEF ANDROID}
constructor TFMXPlatformPlayer.Create(AOwner: TComponent);
begin
  inherited;
end;

function TFMXPlatformPlayer.InitBass(Handle: Pointer): Boolean;
begin
  //if Handle = nil then
  //  Handle := Application.MainForm;
  Result := BASS_Init(Device, Freq, Flags, Handle, nil);
end;
{$ENDIF}

end.

