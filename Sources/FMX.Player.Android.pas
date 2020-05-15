unit FMX.Player.Android;

interface

uses
  FMX.BASS, System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types, FMX.Player.Shared,
  FMX.Platform.Android, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.Helpers,
  Androidapi.JNI.App, FMX.Platform, FMX.PhoneDialer, FMX.BASS.AAC;

type
  TFMXPlatformPlayer = class(TFMXCustomPlayer)
  private
    FPhoneDialerService: IFMXPhoneDialerService;
    FPauseOnIncomingCalls: Boolean;
    procedure DetectIsCallStateChanged(const ACallID: string; const ACallState: TCallState);
  public
    procedure SetVolume(const AValue: Single); override;
    function GetVolume: Single; override;
    function Init(Handle: TWindowHandle): Boolean; override;
    function InitBass(Handle: TWindowHandle): Boolean; override;
  end;

implementation

{ TFMXPlatformPlayer }

procedure TFMXPlatformPlayer.DetectIsCallStateChanged(const ACallID: string; const ACallState: TCallState);
begin
  case ACallState of    //TCallState.None:
		//TCallState.Connected:
    TCallState.Incoming:
      begin
        if FPauseOnIncomingCalls then
        begin
          Pause;
        end;
      end;
		//TCallState.Dialing:
		//TCallState.Disconnected:
  end;
end;

function TFMXPlatformPlayer.GetVolume: Single;
var
  AudioManager: JAudioManager;
begin
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  Result := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  Result := Result / AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
end;

procedure TFMXPlatformPlayer.SetVolume(const AValue: Single);
var
  AudioManager: JAudioManager;
begin
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  AudioManager.SetStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, Round(AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC)
    * AValue), 0);
end;

function TFMXPlatformPlayer.InitBass(Handle: TWindowHandle): Boolean;
begin
  Result := BASS_Init(-1, 44100, 0, Handle, nil);
end;

function TFMXPlatformPlayer.Init(Handle: TWindowHandle): Boolean;
begin
  Result := inherited;
  if Result then
  begin
    Result := False;
    TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, IInterface(FPhoneDialerService));
    if Assigned(FPhoneDialerService) then
    begin
      FPhoneDialerService.OnCallStateChanged := DetectIsCallStateChanged;
      Result := True;
    end;
  end;
end;

end.

