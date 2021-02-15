unit FMX.Player;

interface

uses
  {$IFDEF ANDROID}
  FMX.PhoneDialer,
  {$ENDIF}
  FMX.BASS.Classes, FMX.Types, FMX.BASS, FMX.BASS.AAC, FMX.BASS.Plugins, System.Classes;

type
  TFFTData = array[0..512] of Single;

  TPlayerState = (psNone, psStop, psPlay, psPause, psOpening, psError);

  TPlayerPlayKind = (pkFile, pkStream);

  TPlayAsyncResult = reference to procedure(const Success: Boolean);

  TOnChangePosition = procedure(Sender: TObject; const Time: Int64) of object;

  TFMXCustomPlayer = class abstract(TCustomBassComponent)
  protected
    FActiveChannel: HSTREAM;
  private
    {$IFDEF ANDROID}
    FPhoneDialerService: IFMXPhoneDialerService;
    procedure DetectIsCallStateChanged(const ACallID: string; const ACallState: TCallState);
    {$ENDIF}
  private
    FAsync: Boolean;
    FAutoPlay: Boolean;
    FFileName: string;
    FKeepPlayChannel: Boolean;
    FOnChangeState: TNotifyEvent;
    FOnEnd: TNotifyEvent;
    FPauseOnIncomingCalls: Boolean;
    FPlayerState: TPlayerState;
    FPlayKind: TPlayerPlayKind;
    FPlaySyncEnd: HSYNC;
    FStreamURL: string;
    FVolumeChannel: Single;
    FStarting: Boolean;
    FTimer: TTimer;
    FOnChangePosition: TOnChangePosition;
    FPositionInterval: Integer;
    FAutoFree: Boolean;
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
    procedure DoChangeState;
    procedure DoOnEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
    procedure DoPlayerState(const Value: TPlayerState);
    procedure FUpdateChannelVolume;
    procedure SetAsync(const Value: Boolean);
    procedure SetAutoPlay(const Value: Boolean);
    procedure SetKeepPlayChannel(const Value: Boolean);
    procedure SetOnChangeState(const Value: TNotifyEvent);
    procedure SetOnEnd(const Value: TNotifyEvent);
    procedure SetPauseOnIncomingCalls(Value: Boolean);
    procedure SetPlayerState(const Value: TPlayerState);
    procedure SetPosition(const Value: Int64);
    procedure SetPositionByte(const Value: Int64);
    procedure SetPositionPercent(const Value: Extended);
    procedure SetVolumeChannel(const Value: Single);
    procedure SetOnChangePosition(const Value: TOnChangePosition);
    procedure SetPositionInterval(const Value: Integer);
    procedure SetAutoFree(const Value: Boolean);
  protected
    procedure FOnTimer(Sender: TObject);
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
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; override;
    function Play: Boolean; virtual;
    function Resume: Boolean; virtual;
    procedure Pause; virtual;
    procedure PlayAsync(ResultMethod: TPlayAsyncResult = nil); virtual;
    procedure Stop; virtual;
    procedure SwitchPlay;
    procedure UnloadChannel;
    //Props
    property Async: Boolean read FAsync write SetAsync;
    property AutoPlay: Boolean read FAutoPlay write SetAutoPlay;
    property AutoFree: Boolean read FAutoFree write SetAutoFree;
    property Bufferring: Int64 read GetBufferring;
    property BufferringPercent: Extended read GetBufferringPercent;
    property FileName: string read FFileName write SetFileName;
    property IsOpening: Boolean read GetIsOpening;
    property IsPause: Boolean read GetIsPause;
    property IsPlay: Boolean read GetIsPlay;
    property KeepPlayChannel: Boolean read FKeepPlayChannel write SetKeepPlayChannel;
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
    property VolumeChannel: Single read FVolumeChannel write SetVolumeChannel;
    property PositionInterval: Integer read FPositionInterval write SetPositionInterval;
    //Events
    property OnChangeState: TNotifyEvent read FOnChangeState write SetOnChangeState;
    property OnEnd: TNotifyEvent read FOnEnd write SetOnEnd;
    property OnChangePosition: TOnChangePosition read FOnChangePosition write SetOnChangePosition;
  end;

var
  Player: TFMXCustomPlayer;

implementation

uses
  FMX.platform, System.Math, System.SysUtils;

procedure FSyncEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
begin
  if Assigned(Player) then
    Player.DoOnEnd(handle, channel, data, user);
end;

{ TFMXCustomPlayer }

{$IFDEF ANDROID}
procedure TFMXCustomPlayer.DetectIsCallStateChanged(const ACallID: string; const ACallState: TCallState);
begin
  case ACallState of    //TCallState.None:
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
  FPositionInterval := 1000;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := FPositionInterval;
  FTimer.OnTimer := FOnTimer;
  FStarting := False;
  FAutoFree := True;
  FKeepPlayChannel := False;
  FActiveChannel := 0;
  FVolumeChannel := 100;
  FPlayerState := TPlayerState.psNone;
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

procedure TFMXCustomPlayer.FOnTimer(Sender: TObject);
begin
  if IsPlay then
  begin
    if Assigned(FOnChangePosition) then
      FOnChangePosition(Self, GetPosition);
  end;
end;

procedure TFMXCustomPlayer.FUpdateChannelVolume;
begin
  if csDesigning in ComponentState then
    Exit;
  if IsActiveChannel then
  begin
    BASS_ChannelSetAttribute(FActiveChannel, BASS_ATTRIB_VOL, FVolumeChannel / 100);
  end;
end;

function TFMXCustomPlayer.Play: Boolean;
begin
  Result := False;
  try
    if BassLibrary.IsInit and (not IsOpening) then
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
          FPlaySyncEnd := BASS_ChannelSetSync(FActiveChannel, BASS_SYNC_END, 0, @FSyncEnd, nil);
          DoPlayerState(TPlayerState.psPlay);
          Result := True;
        end;
      end
      else
      begin
        DoPlayerState(TPlayerState.psError);
        BassLibrary.LastErrorCode := Bass_ErrorGetCode;
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
      while FStarting do
        Sleep(100);
      FStarting := True;
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
      FStarting := False;
    end).Start;
end;

procedure TFMXCustomPlayer.UnloadChannel;
begin
  if IsActiveChannel then
  begin
    BASS_ChannelRemoveSync(FActiveChannel, FPlaySyncEnd);
    BASS_StreamFree(FActiveChannel);
    FActiveChannel := 0;
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

procedure TFMXCustomPlayer.SetPositionInterval(const Value: Integer);
begin
  FPositionInterval := Value;
  FTimer.Interval := FPositionInterval;
end;

procedure TFMXCustomPlayer.SetPositionPercent(const Value: Extended);
begin
  SetPosition(Round((GetSize / 100) * Value));
end;

procedure TFMXCustomPlayer.SetStreamURL(AUrl: string);
begin
  if csDesigning in ComponentState then
  begin
    FPlayKind := TPlayerPlayKind.pkStream;
    FStreamURL := AUrl;
    Exit;
  end;

  if IsOpening then
    Exit;

  if AUrl.IsEmpty then
  begin
    FStreamURL := AUrl;
    Stop;
    Exit;
  end;

  if FStreamURL = AUrl then
  begin
    if (FAutoPlay and (not IsPlay)) then
      Play;
    Exit;
  end;

  FPlayKind := TPlayerPlayKind.pkStream;
  FStreamURL := AUrl;

  if FAutoPlay then
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

procedure TFMXCustomPlayer.SetAutoFree(const Value: Boolean);
begin
  FAutoFree := Value;
end;

procedure TFMXCustomPlayer.SetAutoPlay(const Value: Boolean);
begin
  FAutoPlay := Value;
end;

procedure TFMXCustomPlayer.SetFileName(const Value: string);
begin
  FFileName := Value;
  FPlayKind := TPlayerPlayKind.pkFile;

  if not (csDesigning in ComponentState) then
    if FAutoPlay then
      if FAsync then
        PlayAsync
      else
        Play;
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

procedure TFMXCustomPlayer.SwitchPlay;
begin
  if IsPlay then
    Pause
  else if IsPause then
    Resume
  else
    Play;
end;

function TFMXCustomPlayer.GetBufferring: Int64;
begin
  if not BassLibrary.IsInit then
    Exit(0);
  Result := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_BUFFER);
end;

function TFMXCustomPlayer.GetBufferringPercent: Extended;
begin
  if not BassLibrary.IsInit then
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
  if not BassLibrary.IsInit then
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

function TFMXCustomPlayer.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
begin
  Result := BassLibrary.IsInit or inherited Init(Handle, HWND);
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
  if not (csDesigning in ComponentState) then
  begin
    UnloadChannel;
  end;
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
  FTimer.Enabled := FPlayerState in [TPlayerState.psPlay, TPlayerState.psOpening];
  if IsActiveChannel and (FPlayerState in [psStop, psError]) and FAutoFree then
    UnloadChannel;
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

procedure TFMXCustomPlayer.SetOnChangePosition(const Value: TOnChangePosition);
begin
  FOnChangePosition := Value;
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

