unit FMX.Recorder;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.BASS, System.Generics.Collections;

type
  TInputDevice = record
    Index: Integer;
    Name: string;
    Level: Single;
    IsActive: Boolean;
  end;

  TOnBassRecording = procedure(Sender: TObject; Channel: DWORD; const CurrentTime: Cardinal) of object;

  TCustomBassRecorder = class abstract(TComponent)
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
    function FDoOnRecording(hChannel: HRECORD; Buffer: Pointer; Size: DWord; User: Pointer): Boolean;
    procedure SetOnRecording(const Value: TOnBassRecording);
  public
    function GetInputDevices: TList<TInputDevice>;
    function Init(const Frequency: Cardinal; Device: Integer = -1): Boolean;
    procedure Start(const Stream: TStream; Device: Integer = -1);
    procedure Stop(const FreeStream: Boolean = True);
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

function TCustomBassRecorder.Init(const Frequency: Cardinal; Device: Integer): Boolean;
begin
  Result := BASS_Init(Device, Frequency, BASS_DEVICE_LATENCY, 0, nil);
  BASS_RecordInit(Device);
end;

procedure TCustomBassRecorder.SetOnRecording(const Value: TOnBassRecording);
begin
  FOnRecording := Value;
end;

procedure TCustomBassRecorder.Start(const Stream: TStream; Device: Integer);
const
  PCMHeader = 1;
  Freq = 44100;
  Channels = 2;
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

