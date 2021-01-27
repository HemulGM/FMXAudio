unit FMX.BASS.Classes;

interface

uses
  FMX.Types, FMX.BASS, System.Classes;

type
  TCustomBassComponent = class(TComponent)
  protected
    FLastErrorCode: Integer;
    FUseDefaultDevice: Boolean;
    procedure SetUseDefaultDevice(const Value: Boolean); virtual;
    function GetVersion: string; virtual;
  public
    /// <summary>
    /// Use Handle (for android, fmx) or WindowHandle (windows, fmx/vcl) or nothing
    /// </summary>
    function Init(Handle: Pointer = nil; HWND: NativeUInt = 0): Boolean; virtual;
    property UseDefaultDevice: Boolean read FUseDefaultDevice write SetUseDefaultDevice;
    property Version: string read GetVersion;
    property LastErrorCode: Integer read FLastErrorCode;
  end;

implementation

{ TCustomBassComponent }

function TCustomBassComponent.Init(Handle: Pointer; HWND: NativeUInt): Boolean;
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

function TCustomBassComponent.GetVersion: string;
begin
  Result := BASSVERSIONTEXT;
end;

end.

