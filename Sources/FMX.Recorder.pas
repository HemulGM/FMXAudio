unit FMX.Recorder;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.BASS.Classes, FMX.BASS, System.Generics.Collections;

type
  TInputDevice = record
    Index: Integer;
    Name: string;
    Level: Single;
    IsActive: Boolean;
  end;

  TOnBassRecording = procedure(Sender: TObject; Channel: DWORD; const CurrentTime: Cardinal) of object;

  TCustomBassRecorder = class abstract(TCustomBassComponent)
    type
      TWaveHeader = packed record
        RIFF: array[0..3] of UTF8Char;
        Length: DWORD;
        WAVE: array[0..3] of UTF8Char;
        WaveFormat: array[0..3] of UTF8Char;
        HeaderLength: DWORD;
        Format: Word;
        NumChannels: Word;
        SamplesPerSecond: DWORD;
        BytesPerSec: DWORD;
        BlockAlign: Word;
        BitsPerSample: Word;
        Data: array[0..3] of UTF8Char;
        DataLen: DWORD;
      end;
    class var
      InternalBaseRecorder: TCustomBassRecorder;
  private
    FRecordChannel: DWORD;
    FIsRecording: Boolean;
    FCurrentTime: Cardinal;
    FStream: TStream;
    FOnRecording: TOnBassRecording;
    FUseDefaultDevice: Boolean;
    FFreq: Cardinal;
    FAutoInit: Boolean;
    FDevice: LongInt;
    FFlags: Cardinal;
    FIsInit: Boolean;
    FChannels: Integer;
    function FDoOnRecording(hChannel: HRECORD; Buffer: Pointer; Size: DWord; User: Pointer): Boolean;
    procedure SetOnRecording(const Value: TOnBassRecording);
    procedure SetAutoInit(const Value: Boolean);
    procedure SetDevice(const Value: LongInt);
    procedure SetFlags(const Value: Cardinal);
    procedure SetFreq(const Value: Cardinal);
    procedure SetUseDefaultDevice(const Value: Boolean);
    procedure SetChannels(const Value: Integer);
  public
    function GetInputDevices: TList<TInputDevice>;
    //function Init(const Frequency: Cardinal; Device: Integer = -1): Boolean;
    procedure Start(const Stream: TStream; Device: Integer = -1);
    procedure Stop(const FreeStream: Boolean = True);
    property IsInit: Boolean read FIsInit;
    property AutoInit: Boolean read FAutoInit write SetAutoInit;
    property Device: LongInt read FDevice write SetDevice;
    property Flags: Cardinal read FFlags write SetFlags;
    property Freq: Cardinal read FFreq write SetFreq;
    property Channels: Integer read FChannels write SetChannels;
    property UseDefaultDevice: Boolean read FUseDefaultDevice write SetUseDefaultDevice;
    property IsRecording: Boolean read FIsRecording;
    property CurrentTime: Cardinal read FCurrentTime;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnRecording: TOnBassRecording read FOnRecording write SetOnRecording;
  end;

implementation

function RecordingCallback(hChannel: HRECORD; Buffer: Pointer; Size: DWord; User: Pointer): Boolean; stdcall;
begin
  Result := False;
  if Assigned(TCustomBassRecorder.InternalBaseRecorder) then
  begin
    Result := TCustomBassRecorder.InternalBaseRecorder.FDoOnRecording(hChannel, Buffer, Size, User);
  end;
end;

{ TCustomBassRecorder }

constructor TCustomBassRecorder.Create(AOwner: TComponent);
begin
  inherited;
  FCurrentTime := 0;
  InternalBaseRecorder := Self;

  FUseDefaultDevice := True;
  FChannels := 2;
  FDevice := -1;
  FFreq := 44100;
  FFlags := 0;
end;

destructor TCustomBassRecorder.Destroy;
begin
  if FIsRecording then
    Stop;
  inherited;
end;

function TCustomBassRecorder.FDoOnRecording(hChannel: HRECORD; Buffer: Pointer; Size: DWord; User: Pointer): Boolean;
var
  Position: integer;
begin
  FStream.Write(Buffer^, Size);
  Position := BASS_ChannelGetPosition(hChannel, 0);
  if (Position > 0) then
  begin
    FCurrentTime := Trunc(BASS_ChannelBytes2Seconds(hChannel, Position));
  end;
  try
    if Assigned(FOnRecording) then
      FOnRecording(Self, hChannel, CurrentTime);
  except
    //
  end;
  Result := True;
end;

function TCustomBassRecorder.GetInputDevices: TList<TInputDevice>;
var
  i: Integer;
  Level: Single;
  Item: TInputDevice;
  BassResult: Integer;
begin
  Result := TList<TInputDevice>.Create;
  i := 0;
  repeat
    BassResult := BASS_RecordGetInput(i, Level);
    if (BassResult and BASS_INPUT_TYPE_MASK) = BASS_INPUT_TYPE_MIC then
    begin
      Item.Index := i;
      Item.Name := string(BASS_RecordGetInputName(i));
      Item.Level := Level;
      Item.IsActive := (BassResult and BASS_INPUT_OFF) = 0;
      Result.Add(Item);
    end;
    Inc(i);
  until BassResult = -1;
end;

function TCustomBassRecorder.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
begin
  Result := False;
  if BASS_Available then
  begin
    if FUseDefaultDevice then
      BASS_SetConfig(BASS_CONFIG_DEV_DEFAULT, 1);
    {$IFDEF MSWINDOWS}
    if BASS_Init(Device, Freq, Flags, HWND, nil) then
    {$ENDIF}
    {$IFDEF ANDROID}
      if BASS_Init(Device, Freq, Flags, Handle, nil) then
    {$ENDIF}
      begin
        //FPlugins.Load;
        Result := BASS_RecordInit(Device);
      end;
  end;
  FIsInit := Result;
end;

procedure TCustomBassRecorder.SetAutoInit(const Value: Boolean);
begin
  FAutoInit := Value;
  if not (csDesigning in ComponentState) then
    if FAutoInit then
      Init;
end;

procedure TCustomBassRecorder.SetChannels(const Value: Integer);
begin
  FChannels := Value;
end;

procedure TCustomBassRecorder.SetDevice(const Value: LongInt);
begin
  FDevice := Value;
end;

procedure TCustomBassRecorder.SetFlags(const Value: Cardinal);
begin
  FFlags := Value;
end;

procedure TCustomBassRecorder.SetFreq(const Value: Cardinal);
begin
  FFreq := Value;
end;

procedure TCustomBassRecorder.SetOnRecording(const Value: TOnBassRecording);
begin
  FOnRecording := Value;
end;

procedure TCustomBassRecorder.SetUseDefaultDevice(const Value: Boolean);
begin
  FUseDefaultDevice := Value;
end;

procedure TCustomBassRecorder.Start(const Stream: TStream; Device: Integer);
const
  PCMHeader = 1;
var
  WaveHdr: TWaveHeader;
begin
  FStream := Stream;
  //FCurrentTime := 0;
  BASS_RecordInit(Device);
  FStream.Position := 0;
  with WaveHdr do
  begin
    RIFF := 'RIFF';
    Length := SizeOf(TWaveHeader);
    WAVE := 'WAVE';
    WaveFormat := 'fmt ';
    HeaderLength := 16;
    Format := PCMHeader;
    //
    NumChannels := Channels;
    SamplesPerSecond := Freq;
    BitsPerSample := 16; //8,16,24,32
    //
    BytesPerSec := NumChannels * SamplesPerSecond * (BitsPerSample div 8);
    BlockAlign := NumChannels * (BitsPerSample div 8);
    Data := 'data';
    DataLen := 0;
  end;
  FStream.Write(WaveHdr, SizeOf(TWaveHeader));
  FRecordChannel := BASS_RecordStart(Freq, Channels, 0, @RecordingCallback, nil);
end;

procedure TCustomBassRecorder.Stop(const FreeStream: Boolean);
var
  i: int64;
begin
  FCurrentTime := 0;
  BASS_ChannelStop(FRecordChannel);
  FStream.Position := 4;
  i := FStream.Size - 8;
  FStream.Write(i, 4);
  i := i - $24;
  FStream.Position := 40;
  FStream.Write(i, 4);
  if FreeStream then
    FStream.Free;
end;

end.

