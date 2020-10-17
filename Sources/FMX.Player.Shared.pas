unit FMX.Player.Shared;

interface

uses
  FMX.BASS, FMX.BASS.AAC, System.Classes;

type
  TFFTData = array[0..512] of Single;

  TPlayerState = (psNone, psStop, psPlay, psPause, psOpening, psError);

  TPlayerPlayKind = (pkFile, pkStream);

  TFMXCustomPlayer = class abstract(TComponent)
  protected
    FIsInit: Boolean;
    FAutoInit: Boolean;
  private
    FPauseOnIncomingCalls: Boolean;
    FVolumeChannel: Single;
    FOnChangeState: TNotifyEvent;
    FOnEnd: TNotifyEvent;
    FStreamURL: string;
    FPlaySync: HSYNC;
    FLastErrorCode: Integer;
    FPlayerState: TPlayerState;
    FPlayKind: TPlayerPlayKind;
    FFileName: string;
    FKeepPlayChannel: Boolean;
    FFreq: Cardinal;
    FDevice: LongInt;
    FFlags: Cardinal;
    FAutoplay: Boolean;
    FAsync: Boolean;
    function GetIsActiveChannel: Boolean;
    function GetIsPlay: Boolean;
    function GetPosition: Int64;
    function GetBufferring: Int64;
    function GetBufferringPercent: Extended;
    function GetPositionByte: Int64;
    function GetPositionPercent: Extended;
    function GetPositionTime: string;
    function GetPositionTimeLeft: string;
    function GetSizeAsBuffer: Int64;
    function GetSizeByte: Int64;
    procedure DoChangeState;
    procedure DoOnEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
    procedure SetPosition(const Value: Int64);
    procedure SetOnChangeState(const Value: TNotifyEvent);
    procedure SetOnEnd(const Value: TNotifyEvent);
    procedure SetPositionByte(const Value: Int64);
    procedure SetPositionPercent(const Value: Extended);
    procedure SetVolumeChannel(const Value: Single);
    procedure SetPauseOnIncomingCalls(Value: Boolean);
    procedure FUpdateChannelVolume;
    procedure SetPlayerState(const Value: TPlayerState);
    procedure DoPlayerState(const Value: TPlayerState);
    procedure UnloadChannel;
    function GetIsPause: Boolean;
    function GetIsOpening: Boolean;
    function GetVersion: string;
    procedure SetKeepPlayChannel(const Value: Boolean);
    procedure SetDevice(const Value: LongInt);
    procedure SetFlags(const Value: Cardinal);
    procedure SetFreq(const Value: Cardinal);
    procedure SetAutoplay(const Value: Boolean);
    procedure SetAutoInit(const Value: Boolean);
    procedure SetAsync(const Value: Boolean);
  protected
    FActiveChannel: HSTREAM;
    function InitBass(Handle: Pointer): Boolean; virtual;
    property IsActiveChannel: Boolean read GetIsActiveChannel;
    procedure SetStreamURL(AUrl: string); virtual;
    procedure SetFileName(const Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    function GetTimeFromPercent(Value: Extended): string;
    function GetLibPath: string;
    function GetData(var FFTData: TFFTData): Boolean;
    function Init(Handle: Pointer = nil): Boolean; overload; virtual;
    function GetSize: Int64; virtual;
    function Play: Boolean; virtual;
    procedure PlayAsync;
    function Resume: Boolean; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
    //
    property LastErrorCode: Integer read FLastErrorCode;
    property Position: Int64 read GetPosition write SetPosition;
    property PositionByte: Int64 read GetPositionByte write SetPositionByte;
    property PositionPercent: Extended read GetPositionPercent write SetPositionPercent;
    property Size: Int64 read GetSize;
    property IsPlay: Boolean read GetIsPlay;
    property IsPause: Boolean read GetIsPause;
    property IsInit: Boolean read FIsInit;
    property SizeByte: Int64 read GetSizeByte;
    property SizeAsBuffer: Int64 read GetSizeAsBuffer;
    property Bufferring: Int64 read GetBufferring;
    property BufferringPercent: Extended read GetBufferringPercent;
    property PositionTime: string read GetPositionTime;
    property PositionTimeLeft: string read GetPositionTimeLeft;
    property IsOpening: Boolean read GetIsOpening;
    property VolumeChannel: Single read FVolumeChannel write SetVolumeChannel;
    property PauseOnIncomingCalls: Boolean read FPauseOnIncomingCalls write SetPauseOnIncomingCalls;
    property OnEnd: TNotifyEvent read FOnEnd write SetOnEnd;
    property OnChangeState: TNotifyEvent read FOnChangeState write SetOnChangeState;
    property State: TPlayerState read FPlayerState write SetPlayerState;
    property PlayKind: TPlayerPlayKind read FPlayKind;
    property Version: string read GetVersion;
    property StreamURL: string read FStreamURL write SetStreamURL;
    property FileName: string read FFileName write SetFileName;
    property KeepPlayChannel: Boolean read FKeepPlayChannel write SetKeepPlayChannel default False;
    property Device: LongInt read FDevice write SetDevice default -1;
    property Freq: Cardinal read FFreq write SetFreq default 44100;
    property Flags: Cardinal read FFlags write SetFlags default 0;
    property Autoplay: Boolean read FAutoplay write SetAutoplay default False;
    property AutoInit: Boolean read FAutoInit write SetAutoInit default False;
    property Async: Boolean read FAsync write SetAsync default False;
  end;

var
  Player: TFMXCustomPlayer;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, FMX.Platform.Win,
  {$ENDIF}
  FMX.Forms, System.Math, System.SysUtils;

procedure FSync(handle: HSYNC; channel, data: Cardinal; user: Pointer);
begin
  if Assigned(Player) then
    Player.DoOnEnd(handle, channel, data, user);
end;

{ TFMXCustomPlayer }

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

procedure TFMXCustomPlayer.DoOnEnd(handle: HSYNC; channel, data: Cardinal; user: Pointer);
begin
  FPlayerState := TPlayerState.psStop;
  if Assigned(FOnEnd) then
  begin
    TThread.ForceQueue(nil,
      procedure
      begin
        FOnEnd(Self);
      end);
  end;
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
  if not FIsInit then
    Exit;

  if IsOpening then
    Exit;

  DoPlayerState(TPlayerState.psOpening);
  if not FKeepPlayChannel then
    UnloadChannel;
  try
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
  finally
    if IsOpening or (not Result) then
      DoPlayerState(TPlayerState.psError);
  end;
end;

procedure TFMXCustomPlayer.PlayAsync;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Play;
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
    DoPlayerState(TPlayerState.psPause);
    BASS_ChannelPause(FActiveChannel);
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
        TThread.CreateAnonymousThread(
          procedure
          begin
            if not Play then
            begin
              Sleep(100);
              Play;
            end;
          end).Start
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
        TThread.CreateAnonymousThread(
          procedure
          begin
            Play;
          end).Start
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
  DoPlayerState(TPlayerState.psStop);
  if IsActiveChannel then
    BASS_ChannelStop(FActiveChannel);
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

function TFMXCustomPlayer.Init(Handle: Pointer): Boolean;
begin
  Result := False;
  if BASS_Available then
  begin
    if InitBass(Handle) then
    begin
      BASS_PluginLoad(PChar(BASS_AAC_Lib), 0 or BASS_UNICODE);
      BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
      BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
      Result := True;
    end;
  end;
  FIsInit := Result;
end;

function TFMXCustomPlayer.InitBass(Handle: Pointer): Boolean;
{$IFDEF MSWINDOWS}
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

{$IFDEF ANDROID}
begin
  //if Handle = nil then
  //  Handle := Application.MainForm;
  Result := BASS_Init(Device, Freq, Flags, Handle, nil);
end;
{$ENDIF}

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

