unit FMX.BASS.Plugins;

interface

uses
  FMX.BASS, FMX.BASS.AAC, System.Classes, System.Generics.Collections;

type
  TFMXPlayerPlugins = class(TPersistent)
  private
    FHPlugins: TList<HPLUGIN>;
    FAAC: Boolean;
  public
    procedure Load;
    procedure Unload; overload;
    procedure Unload(PluginHandle: HPLUGIN); overload;
    function LoadPlugin(const FileName: string): HPLUGIN;
    constructor Create;
    destructor Destroy; override;
  published
    property AAC: Boolean read FAAC write FAAC default False;
  end;

implementation

{ TFMXPlayerPlugins }

constructor TFMXPlayerPlugins.Create;
begin
  FHPlugins := TList<HPLUGIN>.Create;
end;

destructor TFMXPlayerPlugins.Destroy;
begin
  FHPlugins.DisposeOf;
  inherited;
end;

procedure TFMXPlayerPlugins.Load;
begin
  if not BASS_Available then
    Exit;
  Unload;
  if AAC then
    LoadPlugin(BASS_AAC_Lib);
end;

procedure TFMXPlayerPlugins.Unload;
var
  Plugin: HPLUGIN;
begin
  if not BASS_Available then
    Exit;
  for Plugin in FHPlugins do
    BASS_PluginFree(Plugin);
  FHPlugins.Clear;
end;

function TFMXPlayerPlugins.LoadPlugin(const FileName: string): HPLUGIN;
begin
  Result := BASS_PluginLoad(PChar(FileName), 0 or BASS_UNICODE);
  if Result <> 0 then
    FHPlugins.Add(Result);
end;

procedure TFMXPlayerPlugins.Unload(PluginHandle: HPLUGIN);
var
  Id: Integer;
begin
  if not BASS_Available then
    Exit;
  BASS_PluginFree(PluginHandle);
  Id := FHPlugins.IndexOf(PluginHandle);
  if Id >= 0 then
    FHPlugins.Delete(Id);
end;

end.

