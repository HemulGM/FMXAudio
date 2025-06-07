unit FMX.Player;

interface

uses
  {$IFDEF ANDROID}
  FMX.PhoneDialer,
  {$ENDIF}
  FMX.BASS.Classes, System.Types, FMX.Types, FMX.BASS, System.Classes,
  System.Net.HttpClient;

type
  TFFTData = array[0..512] of Single;

  TPlayerState = (psNone, psStop, psPlay, psPause, psOpening, psError, psEnd);

  TPlayerPlayKind = (pkFile, pkStreamUrl, pkStream);

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
    FFileName: string;
    FOnChangeState: TNotifyEvent;
    FOnEnd: TNotifyEvent;
    FPauseOnIncomingCalls: Boolean;
    FPlayerState: TPlayerState;
    FPlayKind: TPlayerPlayKind;
    FPlaySyncEnd: HSYNC;
    FStreamURL: string;
    FVolumeChannel: Single;
    FStarting: Boolean;
    FInQueue: Boolean;
    FTimer: TTimer;
    FOnChangePosition: TOnChangePosition;
    FAutoFree: Boolean;
    FFXHandles: TArray<HFX>;
    FFXValues: TArray<Single>;
    FEQFrequencies: TArray<Single>;
    FHeaders: string;
    FStream: TStream;
    FAutoFreeStream: Boolean;
    function GetBufferring: Int64;
    function GetBufferringPercent: Extended;
    function GetIsActiveChannel: Boolean;
    function GetIsOpening: Boolean;
    function GetIsPause: Boolean;
    function GetIsPlay: Boolean;
    function GetPositionSec: Int64;
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
    procedure SetOnChangeState(const Value: TNotifyEvent);
    procedure SetOnEnd(const Value: TNotifyEvent);
    procedure SetPauseOnIncomingCalls(Value: Boolean);
    procedure SetPlayerState(const Value: TPlayerState);
    procedure SetPositionSec(const Value: Int64);
    procedure SetPositionByte(const Value: Int64);
    procedure SetPositionPercent(const Value: Extended);
    procedure SetVolumeChannel(const Value: Single);
    procedure SetOnChangePosition(const Value: TOnChangePosition);
    procedure SetPositionUpdateInterval(const Value: Integer);
    procedure SetAutoFree(const Value: Boolean);
    function GetFXValue(const Index: Integer): Single;
    procedure SetFXValue(const Index: Integer; const Value: Single);
    procedure UpdateFX;
    function GetEQFrequencies(const Index: Integer): Single;
    procedure SetEQFrequencies(const Index: Integer; const Value: Single);
    procedure CreateFX;
    procedure SetHeaders(const Value: string);
    procedure SetAutoFreeStream(const Value: Boolean);
    function GetPositionUpdateInterval: Integer;
    function GetDurationTime: string;
  protected
    FBandWidth: Single;
    procedure FOnTimer(Sender: TObject);
    procedure SetFileName(const Value: string); virtual;
    procedure SetStreamURL(const Value: string); virtual;
    procedure SetStream(const Value: TStream); virtual;
    property IsActiveChannel: Boolean read GetIsActiveChannel;
    function Play: Boolean; virtual;
  public
    //fx
    procedure SetFXBands(BandCount: integer);
    property FXValues[const Index: Integer]: Single read GetFXValue write SetFXValue;
    property EQFrequencies[const Index: Integer]: Single read GetEQFrequencies write SetEQFrequencies;
    procedure ResetFX;
    function GetDuration: Int64; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //Methods
    function GetWaveData(var FFTData: TFFTData): Boolean;
    function GetLibPath: string; virtual;
    function GetTimeFromPercent(Value: Extended): string; virtual;
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; override;
    function Resume: Boolean; virtual;
    procedure Pause; virtual;
    procedure PlayAsync(ResultMethod: TPlayAsyncResult = nil); virtual;
    procedure Stop; virtual;
    procedure SwitchPlay;
    procedure UnloadChannel;
    procedure QuickPlayResource(const ResourceName: string);
    procedure QuickPlayFile(const FileName: string);
    //Props
    property AutoFree: Boolean read FAutoFree write SetAutoFree;
    property AutoFreeStream: Boolean read FAutoFreeStream write SetAutoFreeStream;
    property Bufferring: Int64 read GetBufferring;
    property BufferringPercent: Extended read GetBufferringPercent;
    property FileName: string read FFileName write SetFileName;
    property IsOpening: Boolean read GetIsOpening;
    property IsPause: Boolean read GetIsPause;
    property IsPlay: Boolean read GetIsPlay;
    property PauseOnIncomingCalls: Boolean read FPauseOnIncomingCalls write SetPauseOnIncomingCalls;
    property PlayKind: TPlayerPlayKind read FPlayKind;
    property PositionSec: Int64 read GetPositionSec write SetPositionSec;
    property PositionByte: Int64 read GetPositionByte write SetPositionByte;
    property PositionPercent: Extended read GetPositionPercent write SetPositionPercent;
    property PositionTime: string read GetPositionTime;
    property PositionTimeLeft: string read GetPositionTimeLeft;
    property DurationTime: string read GetDurationTime;
    property Duration: Int64 read GetDuration;
    property SizeAsBuffer: Int64 read GetSizeAsBuffer;
    property SizeByte: Int64 read GetSizeByte;
    property State: TPlayerState read FPlayerState write SetPlayerState;
    property StreamURL: string read FStreamURL write SetStreamURL;
    property Stream: TStream read FStream write SetStream;
    property VolumeChannel: Single read FVolumeChannel write SetVolumeChannel;
    property PositionUpdateInterval: Integer read GetPositionUpdateInterval write SetPositionUpdateInterval;
    property Headers: string read FHeaders write SetHeaders;
    //Events
    property OnChangeState: TNotifyEvent read FOnChangeState write SetOnChangeState;
    property OnEnd: TNotifyEvent read FOnEnd write SetOnEnd;
    property OnChangePosition: TOnChangePosition read FOnChangePosition write SetOnChangePosition;
  end;

implementation

uses
  FMX.platform, System.Math, System.SysUtils;

procedure FSyncEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer); stdcall;
begin
  TFMXCustomPlayer(user).DoOnEnd(handle, channel, data, user);
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

procedure TFMXCustomPlayer.SetFXBands(BandCount: integer);
begin
  SetLength(FFXHandles, BandCount);
  SetLength(FFXValues, BandCount);
  SetLength(FEQFrequencies, BandCount);
end;

procedure TFMXCustomPlayer.SetFXValue(const Index: Integer; const Value: Single);
begin
  FFXValues[Index] := Value;
  UpdateFX;
end;

procedure TFMXCustomPlayer.SetHeaders(const Value: string);
begin
  FHeaders := Value;
end;

constructor TFMXCustomPlayer.Create(AOwner: TComponent);
begin
  inherited;
  FBandWidth := 18;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 500;
  FTimer.OnTimer := FOnTimer;
  FStarting := False;
  FInQueue := False;
  FAutoFree := True;
  FActiveChannel := 0;
  FVolumeChannel := 100;
  FPlayerState := TPlayerState.psNone;
end;

procedure TFMXCustomPlayer.DoOnEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
begin
  DoPlayerState(TPlayerState.psEnd);
  if Assigned(FOnEnd) then
    TThread.Queue(nil,
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
      FOnChangePosition(Self, GetPositionSec);
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

procedure TFMXCustomPlayer.UpdateFX;
begin
  if (not IsActiveChannel) or (Length(FFXHandles) <= 0) then
    Exit;
  for var i := 0 to High(FFXValues) do
  begin
    var EQ: BASS_DX8_PARAMEQ;
    EQ.fGain := FFXValues[i];
    EQ.fBandwidth := FBandWidth;
    EQ.fCenter := FEQFrequencies[i];
    BASS_FXSetParameters(FFXHandles[i], @EQ);
  end;
end;

procedure TFMXCustomPlayer.CreateFX;
begin
  if (not IsActiveChannel) or (Length(FFXHandles) <= 0) then
    Exit;
  for var i := 0 to High(FFXHandles) do
    FFXHandles[i] := BASS_ChannelSetFX(FActiveChannel, BASS_FX_DX8_PARAMEQ, 0);
  UpdateFX;
end;

function StreamReadCallback(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
begin
  var Player := TFMXCustomPlayer(user);
  if Assigned(Player.FStream) then
  begin
    try
      Result := Player.FStream.Read(buffer^, length)
    except
      Player.Stop;
      Result := 0;
    end;
  end
  else
    Result := 0;
end;

function StreamSeekCallback(offset: QWORD; user: Pointer): BOOL; stdcall;
begin
  var Player := TFMXCustomPlayer(user);
  if Assigned(Player.FStream) then
  begin
    try
      Player.FStream.Seek(offset, soBeginning);
      Result := True;
    except
      Player.Stop;
      Result := False;
    end;
  end
  else
    Result := False;
end;

function StreamLengthCallback(user: Pointer): QWORD; stdcall;
begin
  var Player := TFMXCustomPlayer(user);
  try
    Result := Player.FStream.Size;
  except
    Player.Stop;
    Result := 0;
  end;
end;

procedure StreamCloseCallback(user: Pointer); stdcall;
begin
  var Player := TFMXCustomPlayer(user);
  if Player.FAutoFreeStream then
    Player.FStream.Free;
  Player.FStream := nil;
end;

procedure StatusProc(buffer: Pointer; length: Cardinal; user: Pointer); stdcall;
begin       {
  var str: AnsiString;
  SetString(str, PAnsiChar(buffer), length);}
end;

function TFMXCustomPlayer.Play: Boolean;
begin
  Result := False;
  try
    UnloadChannel;
    case FPlayKind of
      pkFile:
        begin
          FActiveChannel := BASS_StreamCreateFile(False, PChar(FFileName), 0, 0, BASS_UNICODE);
        end;
      pkStreamUrl:
        begin
          var Url: string := FStreamURL;
          if not FHeaders.IsEmpty then
            Url := Url + #13#10 + FHeaders;
          FActiveChannel := BASS_StreamCreateURL(PChar(Url), 0,
              BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or BASS_UNICODE or BASS_STREAM_PRESCAN, StatusProc, Self);
        end;
      pkStream:
        begin
          var FileProcs: BASS_FILEPROCS;
          FileProcs.close := StreamCloseCallback;
          FileProcs.read := StreamReadCallback;
          FileProcs.seek := StreamSeekCallback;
          FileProcs.length := StreamLengthCallback;
          FActiveChannel := BASS_StreamCreateFileUser(STREAMFILE_NOBUFFER,
              BASS_STREAM_STATUS or BASS_STREAM_AUTOFREE or BASS_UNICODE or BASS_STREAM_PRESCAN, FileProcs, Self);
        end;
    end;

    if not IsActiveChannel then
      Exit;

    CreateFX;
    FUpdateChannelVolume;
    if BASS_ChannelPlay(FActiveChannel, False) then
    begin
      FPlaySyncEnd := BASS_ChannelSetSync(FActiveChannel, BASS_SYNC_END, 0, @FSyncEnd, Self);
      Result := True;
    end;
  finally
    if not Result then
      BassLibrary.LastErrorCode := Bass_ErrorGetCode;
  end;
end;

procedure TFMXCustomPlayer.PlayAsync(ResultMethod: TPlayAsyncResult);
begin
  if not BassLibrary.IsInit then
    Exit;
  // State
  DoPlayerState(TPlayerState.psOpening);
  // Play
  if FInQueue then
    Exit;
  TaskRun(Self,
    procedure(Holder: IComponentHolder)
    begin
      while FStarting do
      begin
        FInQueue := True;
        Sleep(100);
      end;
      FInQueue := False;
      FStarting := True;
      var Success := False;
      try
        Success := Play;
        if FInQueue then
          Exit;
        if Success then
          DoPlayerState(TPlayerState.psPlay)
        else
          DoPlayerState(TPlayerState.psError);
      finally
        FStarting := False;
        if not FInQueue then
          Queue(
            procedure
            begin
              if Assigned(ResultMethod) then
                ResultMethod(Success);
            end);
      end;
    end);
end;

procedure TFMXCustomPlayer.QuickPlayFile(const FileName: string);
begin
  if not BassLibrary.IsInit then
    Exit;
  var Ch := BASS_StreamCreateFile(False, PChar(FileName), 0, 0, BASS_UNICODE or BASS_STREAM_AUTOFREE);
  BASS_ChannelPlay(Ch, False);
end;

procedure TFMXCustomPlayer.QuickPlayResource(const ResourceName: string);
begin
  if not BassLibrary.IsInit then
    Exit;
  var Res := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    var Ch := BASS_StreamCreateFile(True, Res.Memory, 0, Res.Size, BASS_STREAM_AUTOFREE);
    BASS_ChannelPlay(Ch, False);
  finally
    Res.Free;
  end;
end;

procedure TFMXCustomPlayer.UnloadChannel;
begin
  if not IsActiveChannel then
    Exit;
  for var i := Low(FFXHandles) to High(FFXHandles) do
    BASS_ChannelRemoveFX(FActiveChannel, FFXHandles[i]);
  BASS_ChannelRemoveSync(FActiveChannel, FPlaySyncEnd);
  BASS_StreamFree(FActiveChannel);
  FActiveChannel := 0;
end;

procedure TFMXCustomPlayer.Pause;
begin
  if not IsActiveChannel then
    Exit;
  BASS_ChannelPause(FActiveChannel);
  DoPlayerState(TPlayerState.psPause);
end;

procedure TFMXCustomPlayer.SetPauseOnIncomingCalls(Value: Boolean);
begin
  FPauseOnIncomingCalls := Value;
end;

procedure TFMXCustomPlayer.SetPlayerState(const Value: TPlayerState);
begin
  FPlayerState := Value;
end;

procedure TFMXCustomPlayer.SetPositionSec(const Value: Int64);
begin
  if not IsActiveChannel then
    Exit;
  BASS_ChannelSetPosition(FActiveChannel, BASS_ChannelSeconds2Bytes(FActiveChannel, Value), BASS_POS_BYTE);
end;

procedure TFMXCustomPlayer.SetPositionByte(const Value: Int64);
begin
  if not IsActiveChannel then
    Exit;
  BASS_ChannelSetPosition(FActiveChannel, Value, BASS_POS_BYTE);
end;

procedure TFMXCustomPlayer.SetPositionUpdateInterval(const Value: Integer);
begin
  FTimer.Interval := Value;
end;

procedure TFMXCustomPlayer.SetPositionPercent(const Value: Extended);
begin
  SetPositionSec(Round((GetDuration / 100) * Value));
end;

procedure TFMXCustomPlayer.SetStream(const Value: TStream);
begin
  FPlayKind := TPlayerPlayKind.pkStream;
  FStream := Value;
end;

procedure TFMXCustomPlayer.SetStreamURL(const Value: string);
begin
  FPlayKind := TPlayerPlayKind.pkStreamUrl;
  FStreamURL := Value;
end;

procedure TFMXCustomPlayer.SetAutoFree(const Value: Boolean);
begin
  FAutoFree := Value;
end;

procedure TFMXCustomPlayer.SetAutoFreeStream(const Value: Boolean);
begin
  FAutoFreeStream := Value;
end;

procedure TFMXCustomPlayer.SetEQFrequencies(const Index: Integer; const Value: Single);
begin
  FEQFrequencies[Index] := Value;
end;

procedure TFMXCustomPlayer.SetFileName(const Value: string);
begin
  FPlayKind := TPlayerPlayKind.pkFile;
  FFileName := Value;
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
  Result := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_DOWNLOAD);
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

function TFMXCustomPlayer.GetPositionSec: Int64;
begin
  if IsActiveChannel then
    Result := Trunc(BASS_ChannelBytes2Seconds(FActiveChannel, BASS_ChannelGetPosition(FActiveChannel, BASS_POS_BYTE)))
  else
    Result := 0;
end;

function TFMXCustomPlayer.GetWaveData(var FFTData: TFFTData): Boolean;
begin
  Result := False;
  if not BassLibrary.IsInit then
    Exit;
  if BASS_ChannelIsActive(FActiveChannel) <> BASS_ACTIVE_PLAYING then
    Exit;
  BASS_ChannelGetData(FActiveChannel, @FFTData, BASS_DATA_FFT512);
  Result := True;
end;

function TFMXCustomPlayer.GetEQFrequencies(const Index: Integer): Single;
begin
  Result := FEQFrequencies[Index];
end;

function TFMXCustomPlayer.GetFXValue(const Index: Integer): Single;
begin
  Result := FFXValues[Index];
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
  S := GetPositionSec;
  M := S div 60;
  S := S mod 60;
  Result := Format('%d:%.2d', [M, S]);
end;

function TFMXCustomPlayer.GetPositionTimeLeft: string;
var
  M, S: Integer;
begin
  S := GetPositionSec - GetDuration;
  M := S div 60;
  S := S mod 60;
  Result := Format('-%d:%.2d', [Abs(M), Abs(S)]);
end;

function TFMXCustomPlayer.GetPositionUpdateInterval: Integer;
begin
  Result := FTimer.Interval;
end;

function TFMXCustomPlayer.GetTimeFromPercent(Value: Extended): string;
var
  M, S: Integer;
begin
  S := Round(GetDuration * (Value / 100));
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

function TFMXCustomPlayer.GetDurationTime: string;
var
  M, S: Integer;
begin
  S := GetDuration;
  M := S div 60;
  S := S mod 60;
  Result := Format('%d:%.2d', [M, S]);
end;

function TFMXCustomPlayer.GetDuration: Int64;
begin
  if IsActiveChannel then
    Result := Trunc(BASS_ChannelBytes2Seconds(FActiveChannel, BASS_ChannelGetLength(FActiveChannel, BASS_POS_BYTE)))
  else
    Result := 0;
end;

destructor TFMXCustomPlayer.Destroy;
begin
  if not (csDesigning in ComponentState) then
    UnloadChannel;
  inherited;
end;

function TFMXCustomPlayer.GetSizeAsBuffer: Int64;
begin
  if IsActiveChannel then
    Result := BASS_StreamGetFilePosition(FActiveChannel, BASS_FILEPOS_END)
  else
    Result := 0;
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
  if IsActiveChannel and (FPlayerState in [psStop, psError, psEnd]) and FAutoFree then
    UnloadChannel;
  TThread.Queue(nil,
    procedure
    begin
      FTimer.Enabled := FPlayerState in [TPlayerState.psPlay, TPlayerState.psOpening];
      FOnTimer(FTimer);
      if Assigned(FOnChangeState) then
        FOnChangeState(Self);
    end);
end;

procedure TFMXCustomPlayer.ResetFX;
begin
  if (not IsActiveChannel) or (length(FFXHandles) <= 0) then
    Exit;
  for var i := 0 to High(FFXHandles) do
  begin
    var EQ: BASS_DX8_PARAMEQ;
    EQ.fGain := 0;
    EQ.fBandwidth := FBandWidth;
    EQ.fCenter := FEQFrequencies[i];
    BASS_FXSetParameters(FFXHandles[i], @EQ);
  end;
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
    BassLibrary.LastErrorCode := Bass_ErrorGetCode;
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

