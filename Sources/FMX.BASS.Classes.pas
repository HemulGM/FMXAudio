unit FMX.BASS.Classes;

interface

uses
  FMX.Types, FMX.BASS, FMX.BASS.Plugins, System.Classes;

type
  TBassLibrary = class(TPersistent)
  private
    FPlugins: TFMXPlayerPlugins;
    procedure SetPlugins(const Value: TFMXPlayerPlugins);
  protected
    FIsInit: Boolean;
    FFlags: Cardinal;
    FFreq: Cardinal;
    FDevice: LongInt;
    FLastErrorCode: Integer;
    FUseDefaultDevice: Boolean;
    function GetSystemVolume: Single; virtual;
    procedure SetUseDefaultDevice(const Value: Boolean); virtual;
    function GetVersion: string; virtual;
    procedure SetDevice(const Value: LongInt); virtual;
    procedure SetFlags(const Value: Cardinal); virtual;
    procedure SetFreq(const Value: Cardinal); virtual;
    procedure SetSystemVolume(const AValue: Single); virtual;
  public
    procedure Uninit; virtual;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    /// <summary>
    /// Use Handle (for android, fmx) or WindowHandle (windows, fmx/vcl) or nothing
    /// </summary>
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; virtual;
    property IsInit: Boolean read FIsInit;
    property SystemVolume: Single read GetSystemVolume write SetSystemVolume;
    property LastErrorCode: Integer read FLastErrorCode write FLastErrorCode;
  published
    property Device: LongInt read FDevice write SetDevice default -1;
    property Flags: Cardinal read FFlags write SetFlags default 0;
    property Freq: Cardinal read FFreq write SetFreq default 44100;
    property UseDefaultDevice: Boolean read FUseDefaultDevice write SetUseDefaultDevice default True;
    property Plugins: TFMXPlayerPlugins read FPlugins write SetPlugins;
    property Version: string read GetVersion;
  end;

  TCustomBassComponent = class abstract(TComponent)
    class var
      FBassLibrary: TBassLibrary;
  private
    FAutoInit: Boolean;
    FBassLibraryInst: TBassLibrary;
    procedure SetAutoInit(const Value: Boolean);
    procedure SetBassLibraryInst(const Value: TBassLibrary);
  public
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; virtual;
    constructor Create(AOwner: TComponent); override;
    property BassLibrary: TBassLibrary read FBassLibraryInst write SetBassLibraryInst;
    property AutoInit: Boolean read FAutoInit write SetAutoInit;
  end;

implementation

{ TBassLibrary }

destructor TBassLibrary.Destroy;
begin
  //if not (csDesigning in ComponentState) then
  begin
    FPlugins.Unload;
    Uninit;
  end;
  FPlugins.Free;
  inherited;
end;

function TBassLibrary.GetSystemVolume: Single;
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

function TBassLibrary.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
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
        FPlugins.Load;
        BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
        BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
        Result := True;
      end;
  end;
  FIsInit := Result;
end;

constructor TBassLibrary.Create(AOwner: TComponent);
begin
  //inherited C;
  FUseDefaultDevice := True;
  FPlugins := TFMXPlayerPlugins.Create;
  FDevice := -1;
  FFreq := 44100;
  FFlags := 0;
end;

procedure TBassLibrary.SetDevice(const Value: LongInt);
begin
  FDevice := Value;
end;

procedure TBassLibrary.SetFlags(const Value: Cardinal);
begin
  FFlags := Value;
end;

procedure TBassLibrary.SetFreq(const Value: Cardinal);
begin
  FFreq := Value;
end;

procedure TBassLibrary.SetPlugins(const Value: TFMXPlayerPlugins);
begin
  FPlugins := Value;
end;

procedure TBassLibrary.Uninit;
begin
  if BASS_Available and FIsInit then
    BASS_Free;
end;

function TBassLibrary.GetVersion: string;
begin
  Result := BASSVERSIONTEXT;
end;

procedure TBassLibrary.SetSystemVolume(const AValue: Single);
{$IFDEF ANDROID}
var
  AudioManager: JAudioManager;
{$ENDIF}
begin       {
  if csDesigning in ComponentState then
    Exit;   }
{$IFDEF ANDROID}
  AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  AudioManager.SetStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, Round(AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC)
    * AValue), 0);
{$ENDIF}
{$IFDEF MSWINDOWS}
  BASS_SetVolume(AValue);
{$ENDIF}
end;

procedure TBassLibrary.SetUseDefaultDevice(const Value: Boolean);
begin
  FUseDefaultDevice := Value;
end;

{ TCustomBassComponent }

constructor TCustomBassComponent.Create(AOwner: TComponent);
begin
  inherited;
  if not Assigned(FBassLibrary) then
    FBassLibrary := TBassLibrary.Create(nil);
  FBassLibraryInst := FBassLibrary;
end;

function TCustomBassComponent.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
begin
  Result := BassLibrary.Init(Handle, HWND);
end;

procedure TCustomBassComponent.SetAutoInit(const Value: Boolean);
begin
  FAutoInit := Value;
  if not (csDesigning in ComponentState) then
    if FAutoInit then
      Init;
end;

procedure TCustomBassComponent.SetBassLibraryInst(const Value: TBassLibrary);
begin
  FBassLibraryInst := Value;
end;

initialization

finalization
  if Assigned(TCustomBassComponent.FBassLibrary) then
    TCustomBassComponent.FBassLibrary.Free;

end.

