unit FMX.BASS.Classes;

interface

uses
  System.SysUtils, FMX.Types, FMX.BASS, System.Classes;

type
  THolder = class(TComponent)
  private
    FHold: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure HoldComponent(AComponent: TComponent);
    function IsLive: Boolean;
  end;

  IComponentHolder = interface
    procedure HoldComponent(AComponent: TComponent);
    function IsLive: Boolean;
  end;

  TComponentHolder = class(TInterfacedObject, IComponentHolder)
  private
    FHolder: THolder;
  public
    procedure HoldComponent(AComponent: TComponent);
    function IsLive: Boolean;
    constructor Create(AComponent: TComponent = nil);
    destructor Destroy; override;
  end;

  TBassLibrary = class(TPersistent)
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
    constructor Create(AOwner: TComponent); virtual;
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
    constructor Create(AOwner: TComponent); override;
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; virtual;
    property BassLibrary: TBassLibrary read FBassLibraryInst write SetBassLibraryInst;
    property AutoInit: Boolean read FAutoInit write SetAutoInit;
  end;

procedure TaskRun(const Owner: TComponent; Proc: TProc<IComponentHolder>);

procedure Queue(Proc: TThreadProcedure);

procedure Sync(Proc: TThreadProcedure);

procedure ForceQueue(Proc: TThreadProcedure);

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF ANDROID}
  FMX.Platform.Android, Androidapi.JNI.Os, Androidapi.JNI.Net,
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Media,
  Androidapi.JNI.Provider, Androidapi.Helpers, Androidapi.JNI.App,
  {$ENDIF}
   System.Threading;

{ TBassLibrary }

destructor TBassLibrary.Destroy;
begin
  Uninit;
  inherited;
end;

function TBassLibrary.GetSystemVolume: Single;
begin
  {$IFDEF ANDROID}
  var AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  Result := AudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  Result := Result / AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  {$ELSE}
  Result := BASS_GetVolume;
  {$ENDIF}
end;

function TBassLibrary.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
begin
  Result := False;
  if BASS_Available then
  begin
    {$IFDEF MSWINDOWS}
    if BASS_Init(Device, Freq, Flags, HWND, nil) then
    {$ENDIF}
    {$IFDEF POSIX}
      if BASS_Init(Device, Freq, Flags, Handle, nil) then
    {$ENDIF}
      begin
        BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);
        BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0);
        Result := True;
      end;
    BASS_PluginLoad('bass_ssl.dll', 0);
    if FUseDefaultDevice then
      BASS_SetConfig(BASS_CONFIG_DEV_DEFAULT, 1);
    //BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 1);
  end;
  FIsInit := Result;
end;

constructor TBassLibrary.Create(AOwner: TComponent);
begin
  inherited Create;
  FUseDefaultDevice := True;
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
begin
  {$IFDEF ANDROID}
  var AudioManager := TJAudioManager.Wrap(MainActivity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE));
  AudioManager.SetStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC, Round(AudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC) * AValue), 0);
  {$ELSE}
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

procedure TaskRun(const Owner: TComponent; Proc: TProc<IComponentHolder>);
var
  ObjectHold: IComponentHolder;
begin
  ObjectHold := TComponentHolder.Create(Owner);
  TTask.Run(
    procedure
    begin
      try
        Proc(ObjectHold);
      finally
        TThread.ForceQueue(nil,
          procedure
          begin
            ObjectHold := nil;
          end);
      end;
    end);
end;

procedure Queue(Proc: TThreadProcedure);
begin
  TThread.Queue(nil, Proc);
end;

procedure ForceQueue(Proc: TThreadProcedure);
begin
  TThread.ForceQueue(nil, Proc);
end;

procedure Sync(Proc: TThreadProcedure);
begin
  TThread.Synchronize(nil, Proc);
end;

{ THolder }

procedure THolder.HoldComponent(AComponent: TComponent);
begin
  FHold := AComponent;
  AComponent.FreeNotification(Self);
end;

function THolder.IsLive: Boolean;
begin
  Result := Assigned(FHold);
end;

procedure THolder.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FHold then
      FHold := nil;
end;

{ TComponentHolder }

constructor TComponentHolder.Create(AComponent: TComponent);
begin
  inherited Create;
  FHolder := THolder.Create(nil);
  FHolder.HoldComponent(AComponent);
end;

destructor TComponentHolder.Destroy;
begin
  FHolder.Free;
  inherited;
end;

procedure TComponentHolder.HoldComponent(AComponent: TComponent);
begin
  FHolder.HoldComponent(AComponent);
end;

function TComponentHolder.IsLive: Boolean;
begin
  Result := FHolder.IsLive;
end;

initialization

finalization
  if Assigned(TCustomBassComponent.FBassLibrary) then
    TCustomBassComponent.FBassLibrary.Free;

end.

