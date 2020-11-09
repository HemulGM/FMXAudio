unit FMX.Player;

interface

uses
  {$IFDEF ANDROID}
  FMX.PhoneDialer,
  {$ENDIF}
  FMX.BASS, FMX.BASS.AAC, System.Classes;

type
  TFFTData = array[0..512] of Single;

  TPlayerState = (psNone, psStop, psPlay, psPause, psOpening, psError);

  TPlayerPlayKind = (pkFile, pkStream);

  TPlayAsyncResult = reference to procedure(const Success: Boolean);

  TFMXCustomPlayer = class abstract(TComponent)
  protected
    FActiveChannel: HSTREAM;
    FAutoInit: Boolean;
    FIsInit: Boolean;
  private
    {$IFDEF ANDROID}
    FPhoneDialerService: IFMXPhoneDialerService;
    procedure DetectIsCallStateChanged(const ACallID: string; const ACallState: TCallState);
    {$ENDIF}
  private
    FAsync: Boolean;
    FAutoplay: Boolean;
    FDevice: LongInt;
    FFileName: string;
    FFlags: Cardinal;
    FFreq: Cardinal;
    FKeepPlayChannel: Boolean;
    FLastErrorCode: Integer;
    FOnChangeState: TNotifyEvent;
    FOnEnd: TNotifyEvent;
    FPauseOnIncomingCalls: Boolean;
    FPlayerState: TPlayerState;
    FPlayKind: TPlayerPlayKind;
    FPlaySync: HSYNC;
    FStreamURL: string;
    FVolumeChannel: Single;
    function GetBufferring: Int64;
    function GetBufferringPercent: Extended;
    function GetIsActiveChannel: Boolean;
    function GetIsOpening: Boolean;
    function GetIsPause: Boolean;
    function GetIsPlay: Boolean;
    function GetPosition: Int64;
    function GetPositionByte: Int64;
    function GetPositionPercent: Extended;
    function GetPositionTime: string;
    function GetPositionTimeLeft: string;
    function GetSizeAsBuffer: Int64;
    function GetSizeByte: Int64;
    function GetSystemVolume: Single;
    function GetVersion: string;
    procedure DoChangeState;
    procedure DoOnEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
    procedure DoPlayerState(const Value: TPlayerState);
    procedure FUpdateChannelVolume;
    procedure SetAsync(const Value: Boolean);
    procedure SetAutoInit(const Value: Boolean);
    procedure SetAutoplay(const Value: Boolean);
    procedure SetDevice(const Value: LongInt);
    procedure SetFlags(const Value: Cardinal);
    procedure SetFreq(const Value: Cardinal);
    procedure SetKeepPlayChannel(const Value: Boolean);
    procedure SetOnChangeState(const Value: TNotifyEvent);
    procedure SetOnEnd(const Value: TNotifyEvent);
    procedure SetPauseOnIncomingCalls(Value: Boolean);
    procedure SetPlayerState(const Value: TPlayerState);
    procedure SetPosition(const Value: Int64);
    procedure SetPositionByte(const Value: Int64);
    procedure SetPositionPercent(const Value: Extended);
    procedure SetSystemVolume(const AValue: Single);
    procedure SetVolumeChannel(const Value: Single);
    procedure UnloadChannel;
  protected
    procedure SetFileName(const Value: string); virtual;
    procedure SetStreamURL(AUrl: string); virtual;
    property IsActiveChannel: Boolean read GetIsActiveChannel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Methods
    function GetData(var FFTData: TFFTData): Boolean;
    function GetLibPath: string; virtual;
    function GetSize: Int64; virtual;
    function GetTimeFromPercent(Value: Extended): string; virtual;
    /// <summary>
    /// Use Handle (for android, fmx) or WindowHandle (windows, fmx/vcl) or nothing
    /// </summary>
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; overload; virtual;
    function Play: Boolean; virtual;
    function Resume: Boolean; virtual;
    procedure Pause; virtual;
    procedure PlayAsync(ResultMethod: TPlayAsyncResult = nil); virtual;
    procedure Stop; virtual;
    //Props
    property Async: Boolean read FAsync write SetAsync;
    property AutoInit: Boolean read FAutoInit write SetAutoInit;
    property Autoplay: Boolean read FAutoplay write SetAutoplay;
    property Bufferring: Int64 read GetBufferring;
    property BufferringPercent: Extended read GetBufferringPercent;
    property Device: LongInt read FDevice write SetDevice;
    property FileName: string read FFileName write SetFileName;
    property Flags: Cardinal read FFlags write SetFlags;
    property Freq: Cardinal read FFreq write SetFreq;
    property IsInit: Boolean read FIsInit;
    property IsOpening: Boolean read GetIsOpening;
    property IsPause: Boolean read GetIsPause;
    property IsPlay: Boolean read GetIsPlay;
    property KeepPlayChannel: Boolean read FKeepPlayChannel write SetKeepPlayChannel;
    property LastErrorCode: Integer read FLastErrorCode;
    property PauseOnIncomingCalls: Boolean read FPauseOnIncomingCalls write SetPauseOnIncomingCalls;
    property PlayKind: TPlayerPlayKind read FPlayKind;
    property Position: Int64 read GetPosition write SetPosition;
    property PositionByte: Int64 read GetPositionByte write SetPositionByte;
    property PositionPercent: Extended read GetPositionPercent write SetPositionPercent;
    property PositionTime: string read GetPositionTime;
    property PositionTimeLeft: string read GetPositionTimeLeft;
    property Size: Int64 read GetSize;
    property SizeAsBuffer: Int64 read GetSizeAsBuffer;
    property SizeByte: Int64 read GetSizeByte;
    property State: TPlayerState read FPlayerState write SetPlayerState;
    property StreamURL: string read FStreamURL write SetStreamURL;
    property SystemVolume: Single read GetSystemVolume write SetSystemVolume;
    property Version: string read GetVersion;
    property VolumeChannel: Single read FVolumeChannel write SetVolumeChannel;
    //Events
    property OnChangeState: TNotifyEvent read FOnChangeState write SetOnChangeState;
    property OnEnd: TNotifyEvent read FOnEnd write SetOnEnd;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm)]
  TFMXPlayer = class(TFMXCustomPlayer)
  public
    property Bufferring;
    property BufferringPercent;
    property IsInit;
    property IsOpening;
    property IsPause;
    property IsPlay;
    property LastErrorCode;
    property Position;
    property PositionByte;
    property PositionPercent;
    property PositionTime;
    property PositionTimeLeft;
    property Size;
    property SizeAsBuffer;
    property SizeByte;
    property State;
    property VolumeChannel;
  published
    //Props
    property Async default False;
    property AutoInit default False;
    property Autoplay default False;
    property Device default -1;
    property FileName;
    property Flags default 0;
    property Freq default 44100;
    property KeepPlayChannel default False;
    property PauseOnIncomingCalls default False;
    property StreamURL;
    property Version;
    //Events
    property OnChangeState;
    property OnEnd;
  end;

var
  Player: TFMXCustomPlayer;

implementation

uses
  FMX.Platform,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF ANDROID}
  FMX.Platform.Android, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Media, Androidapi.JNI.Provider, Androidapi.Helpers,
  Androidapi.JNI.App,
  {$ENDIF}
  System.Math, System.SysUtils;

procedure FSync(handle: HSYNC; channel, data: Cardinal; user: Pointer);
begin
  if Assigned(Player) then
    Player.DoOnEnd(handle, channel, data, user);
end;

{ TFMXCustomPlayer }

{$IFDEF ANDROID}
procedure TFMXCustomPlayer.DetectIsCallStateChanged(const ACallID: string; const ACallState: TCallState);
begin
  case ACallState of
    //TCallState.None:
		//TCallState.Connected:
		//TCallState.Dialing:
		//TCallState.Disconnected:
    TCallState.Incoming:
      begin
        if FPauseOnIncomingCalls then
        begin
          Pause;
        end;
      end;
  end;
end;
{$ENDIF}

constructor TFMXCustomPlayer.Create(AOwner: TComponent);
begin
  inherited;
  Player := Self;
  FKeepPlayChannel := False;
  FDevice := -1;
  FFreq := 44100;
  FFlags := 0;
  FActiveChannel := 0;
  FVolumeChannel := 100;
  FPlayerState := TPlayerState.psNone;
end;

function TFMXCustomPlayer.GetSystemVolume: Single;
{$IFDEF ANDROID}
var
  AudioManager: JAudioManager;
{$ENDIF}
begin
{$IFDEF ANDROID}
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  Result := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  Result := Result / AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := BASS_GetVolume;
{$ENDIF}
end;

procedure TFMXCustomPlayer.SetSystemVolume(const AValue: Single);
{$IFDEF ANDROID}
var
  AudioManager: JAudioManager;
{$ENDIF}
begin
{$IFDEF ANDROID}
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  AudioManager.SetStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, Round(AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC)
    * AValue), 0);
{$ENDIF}
{$IFDEF MSWINDOWS}
  BASS_SetVolume(AValue);
{$ENDIF}
end;

procedure TFMXCustomPlayer.DoOnEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
begin
  DoPlayerState(TPlayerState.psStop);
  if Assigned(FOnEnd) then
    TThread.ForceQueue(nil,
      procedure
      begin
        FOnEnd(Self);
      end);
end;

procedure TFMXCustomPlayer.DoPlayerState(const Value: TPlayerState);
begin
  SetPlayerState(Value);
  DoChangeState;
end;

procedure TFMXCustomPlayer.FUpdateChannelVolume;
begin
  if IsActiveChannel then
  begin
    BASS_ChannelSetAttribute(FActiveChannel, BASS_ATTRIB_VOL, FVolumeChannel / 100);
  end;
end;

function TFMXCustomPlayer.Play: Boolean;
begin
  Result := False;
  try
    if FIsInit and (not IsOpening) then
    begin
      DoPlayerState(TPlayerState.psOpening);
      if not FKeepPlayChannel then
        UnloadChannel;
      case FPlayKind of
        pkFile:
          begin
            FActiveChannel := BASS_StreamCreateFile(False, PChar(FFileName), 0, 0, BASS_UNICODE);
          end;
        pkStream:
          begin
            FActiveChannel := BASS_StreamCreateURL(PChar(FStreamURL), 0, BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or
              BASS_UNICODE or BASS_MP3_SETPOS, nil, nil);
          end;
      end;

      if IsActiveChannel then
      begin
        FUpdateChannelVolume;
        if BASS_ChannelPlay(FActiveChannel, False) then
        begin
          FPlaySync := BASS_ChannelSetSync(FActiveChannel, BASS_SYNC_END, 0, @FSync, nil);
          DoPlayerState(TPlayerState.psPlay);
          Result := True;
        end;
      end
      else
      begin
        DoPlayerState(TPlayerState.psError);
        FLastErrorCode := Bass_ErrorGetCode;
      end;
    end;
  finally
    if IsOpening or (not Result) then
      DoPlayerState(TPlayerState.psError);
  end;
end;

procedure TFMXCustomPlayer.PlayAsync(ResultMethod: TPlayAsyncResult);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      Success: Boolean;
    begin
      try
        Success := Play;
        if Assigned(ResultMethod) then
          TThread.Synchronize(nil,
            procedure
            begin
              ResultMethod(Success);
            end);
      except
      end;
    end).Start;
end;

procedure TFMXCustomPlayer.UnloadChannel;
begin
  if IsActiveChannel then
  begin
    BASS_ChannelRemoveSync(FActiveChannel, FPlaySync);
    BASS_StreamFree(FActiveChannel);
  end;
end;

procedure TFMXCustomPlayer.Pause;
begin
  if IsActiveChannel then
  begin
    BASS_ChannelPause(FActiveChannel);
    DoPlayerState(TPlayerState.psPause);
  end;
end;

procedure TFMXCustomPlayer.SetPauseOnIncomingCalls(Value: Boolean);
begin
  FPauseOnIncomingCalls := Value;
end;

procedure TFMXCustomPlayer.SetPlayerState(const Value: TPlayerState);
begin
  FPlayerState := Value;
end;

procedure TFMXCustomPlayer.SetPosition(const Value: Int64);
begin
  if IsActiveChannel then
    BASS_ChannelSetPosition(FActiveChannel, BASS_ChannelSeconds2Bytes(FActiveChannel, Value), BASS_POS_BYTE);
end;

procedure TFMXCustomPlayer.SetPositionByte(const Value: Int64);
begin
  if IsActiveChannel then
    BASS_ChannelSetPosition(FActiveChannel, Value, BASS_POS_BYTE);
end;

procedure TFMXCustomPlayer.SetPositionPercent(const Value: Extended);
begin
  SetPosition(Round((GetSize / 100) * Value));
end;

procedure TFMXCustomPlayer.SetStreamURL(AUrl: string);
begin
  if AUrl.IsEmpty then
    Exit;
  FStreamURL := AUrl;
  FPlayKind := TPlayerPlayKind.pkStream;

  if not (csDesigning in ComponentState) then
    if FAutoplay then
      if FAsync then
        PlayAsync(
          procedure(const Success: Boolean)
          begin
            if not Success then
            begin
              Sleep(100);
              PlayAsync;
            end;
          end)
      else if not Play then
      begin
        Sleep(100);
        Play;
      end;
end;

procedure TFMXCustomPlayer.SetAsync(const Value: Boolean);
begin
  FAsync := Value;
end;

procedure TFMXCustomPlayer.SetAutoInit(const Value: Boolean);
begin
  FAutoInit := Value;
  if not (csDesigning in ComponentState) then
    if FAutoInit then
      Init;
end;

procedure TFMXCustomPlayer.SetAutoplay(const Value: Boolean);
begin
  FAutoplay := Value;
end;

procedure TFMXCustomPlayer.SetDevice(const Value: LongInt);
begin
  FDevice := Value;
end;

procedure TFMXCustomPlayer.SetFileName(const Value: string);
begin
  FFileName := Value;
  FPlayKind := TPlayerPlayKind.pkFile;

  if not (csDesigning in ComponentState) then
    if FAutoplay then
      if FAsync then
        PlayAsync
      else
        Play;
end;

procedure TFMXCustomPlayer.SetFlags(const Value: Cardinal);
begin
  FFlags := Value;
end;

procedure TFMXCustomPlayer.SetFreq(const Value: Cardinal);
begin
  FFreq := Value;
end;

procedure TFMXCustomPlayer.SetKeepPlayChannel(const Value: Boolean);
begin
  FKeepPlayChannel := Value;
end;

procedure TFMXCustomPlayer.SetVolumeChannel(const Value: Single);
begin
  FVolumeChannel := Value;
  FUpdateChannelVolume;
end;

procedure TFMXCustomPlayer.Stop;
begin
  if IsActiveChannel then
    BASS_ChannelStop(FActiveChannel);
  DoPlayerState(TPlayerState.psStop);
end;

function TFMXCustomPlayer.GetBufferring: Int64;
begin
  if not FIsInit then
    Exit(0);
  Result := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_BUFFER);
end;

function TFMXCustomPlayer.GetBufferringPercent: Extended;
begin
  if not FIsInit then
    Exit(0);
  if (SizeAsBuffer < 0) or (Bufferring < 0) then
    Exit(0);
  Result := Min(Max(0, (100 / SizeAsBuffer) * Bufferring), 100);
end;

function TFMXCustomPlayer.GetLibPath: string;
begin
  Result := BASS_FOLDER + bassdll;
end;

function TFMXCustomPlayer.GetPosition: Int64;
begin
  if IsActiveChannel then
    Result := Trunc(BASS_ChannelBytes2Seconds(FActiveChannel, BASS_ChannelGetPosition(FActiveChannel, BASS_POS_BYTE)))
  else
    Result := 0;
end;

function TFMXCustomPlayer.GetData(var FFTData: TFFTData): Boolean;
begin
  Result := False;
  if not FIsInit then
    Exit;
  if BASS_ChannelIsActive(FActiveChannel) <> BASS_ACTIVE_PLAYING then
    Exit;
  BASS_ChannelGetData(FActiveChannel, @FFTData, BASS_DATA_FFT512);
  Result := True;
end;

function TFMXCustomPlayer.GetIsActiveChannel: Boolean;
begin
  Result := FActiveChannel <> 0;
end;

function TFMXCustomPlayer.GetIsOpening: Boolean;
begin
  Result := FPlayerState = TPlayerState.psOpening;
end;

function TFMXCustomPlayer.GetIsPause: Boolean;
begin
  Result := FPlayerState = TPlayerState.psPause;
end;

function TFMXCustomPlayer.GetIsPlay: Boolean;
begin
  Result := FPlayerState = TPlayerState.psPlay;
end;

function TFMXCustomPlayer.GetPositionByte: Int64;
begin
  if IsActiveChannel then
    Result := BASS_ChannelGetPosition(FActiveChannel, BASS_POS_BYTE)
  else
    Result := 0;
end;

function TFMXCustomPlayer.GetPositionPercent: Extended;
begin
  Result := Min(Max(0, (100 / SizeByte) * PositionByte), 100);
end;

function TFMXCustomPlayer.GetPositionTime: string;
var
  M, S: Integer;
begin
  S := Position;
  M := S div 60;
  S := S mod 60;
  Result := Format('%d:%.2d', [M, S]);
end;

function TFMXCustomPlayer.GetPositionTimeLeft: string;
var
  M, S: Integer;
begin
  S := Position - Size;
  M := S div 60;
  S := S mod 60;
  Result := Format('-%d:%.2d', [Abs(M), Abs(S)]);
end;

function TFMXCustomPlayer.GetTimeFromPercent(Value: Extended): string;
var
  M, S: Integer;
begin
  S := Round(Size * (Value / 100));
  M := S div 60;
  S := S mod 60;
  Result := Format('%d:%.2d', [M, S]);
end;

function TFMXCustomPlayer.GetVersion: string;
begin
  Result := BASSVERSIONTEXT;
end;

function TFMXCustomPlayer.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
begin
  Result := False;
  if BASS_Available then
  begin
    {$IFDEF MSWINDOWS}
    if BASS_Init(Device, Freq, Flags, HWND, nil) then
    {$ENDIF}
    {$IFDEF ANDROID}
      if BASS_Init(Device, Freq, Flags, Handle, nil) then
    {$ENDIF}
      begin
        BASS_PluginLoad(PChar(BASS_AAC_Lib), 0 or BASS_UNICODE);
        BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
        BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
        Result := True;
      end;
  end;
  FIsInit := Result;
  {$IFDEF ANDROID}
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
  {$ENDIF}
end;

function TFMXCustomPlayer.GetSize: Int64;
begin
  if IsActiveChannel then
    Result := Trunc(BASS_ChannelBytes2Seconds(FActiveChannel, BASS_ChannelGetLength(FActiveChannel, BASS_POS_BYTE)))
  else
    Result := -1;
end;

destructor TFMXCustomPlayer.Destroy;
begin
  UnloadChannel;
  inherited;
end;

function TFMXCustomPlayer.GetSizeAsBuffer: Int64;
begin
  Result := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_END);
end;

function TFMXCustomPlayer.GetSizeByte: Int64;
begin
  if IsActiveChannel then
    Result := BASS_ChannelGetLength(FActiveChannel, BASS_POS_BYTE)
  else
    Result := 0;
end;

procedure TFMXCustomPlayer.DoChangeState;
begin
  if Assigned(FOnChangeState) then
    TThread.ForceQueue(nil,
      procedure
      begin
        FOnChangeState(Self);
      end);
end;

function TFMXCustomPlayer.Resume: Boolean;
begin
  if IsActiveChannel and BASS_ChannelPlay(FActiveChannel, False) then
  begin
    DoPlayerState(TPlayerState.psPlay);
    Result := True;
  end
  else
  begin
    DoPlayerState(TPlayerState.psError);
    Result := False;
  end;
end;

procedure TFMXCustomPlayer.SetOnChangeState(const Value: TNotifyEvent);
begin
  FOnChangeState := Value;
end;

procedure TFMXCustomPlayer.SetOnEnd(const Value: TNotifyEvent);
begin
  FOnEnd := Value;
end;

end.

