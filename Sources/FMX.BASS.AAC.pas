unit FMX.BASS.AAC;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  SysUtils,
{$ENDIF}
  FMX.BASS;

const
  // Additional BASS_SetConfig options
  BASS_CONFIG_MP4_VIDEO = $10700; // play the audio from MP4 videos
  BASS_CONFIG_AAC_MP4 = $10701; // support MP4 in BASS_AAC_StreamCreateXXX functions (no need for BASS_MP4_StreamCreateXXX)

  // Additional tags available from BASS_StreamGetTags
  BASS_TAG_MP4 = 7; // MP4/iTunes metadata

  BASS_AAC_STEREO = $400000; // downmatrix to stereo

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_AAC = $10b00; // AAC
  BASS_CTYPE_STREAM_MP4 = $10b01; // MP4

const
{$IFDEF MSWINDOWS}
  bassAccDll = 'bass_aac.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassAccDll = 'libbass_aac.so';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
  bassAccDll = 'libbass_aac.so';
  {$ELSE}
  bassAccDll = 'libbass_aac.dylib';
  {$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  bassAccDll = 'libbass_aac.so';
{$ENDIF}

var
  FBassAccDLL: THandle;
  BASS_AAC_StreamCreateURL: function(URL: Pointer; offset: Cardinal; flags: Cardinal; proc: DOWNLOADPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};
  BASS_AAC_StreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: Cardinal; flags: Cardinal): HSTREAM; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};
  BASS_AAC_StreamCreateFileUser: function(system, flags: Cardinal; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};
  BASS_MP4_StreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: QWORD; flags: Cardinal): HSTREAM; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};
  BASS_MP4_StreamCreateFileUser: function(system, flags: Cardinal; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};

procedure LoadDynamicBassAACDLL;

procedure UnloadDynamicBassAACDLL;

function BASS_AAC_Folder: string;

function BASS_AAC_Lib: string;

implementation

{$IFNDEF MSWINDOWS}
uses
  System.IOUtils;
{$ENDIF}

function BASS_AAC_Folder: string;
begin
  {$IFDEF MSWINDOWS}
  Result := '';
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(system.IOUtils.TPath.GetLibraryPath);
  {$ENDIF}
end;

function BASS_AAC_Lib: string;
begin
  Result := BASS_AAC_Folder + bassAccDll;
end;

procedure LoadDynamicBassAACDLL;
begin
  FBassAccDLL := LoadLibrary(PChar(BASS_AAC_Lib));
  if FBassAccDLL = 0 then
    Exit;

  BASS_AAC_StreamCreateFile := GetProcAddress(FBassAccDLL, 'BASS_AAC_StreamCreateFile');
  BASS_AAC_StreamCreateURL := GetProcAddress(FBassAccDLL, 'BASS_AAC_StreamCreateURL');
  BASS_AAC_StreamCreateFileUser := GetProcAddress(FBassAccDLL, 'BASS_AAC_StreamCreateFileUser');
  BASS_MP4_StreamCreateFile := GetProcAddress(FBassAccDLL, 'BASS_MP4_StreamCreateFile');
  BASS_MP4_StreamCreateFileUser := GetProcAddress(FBassAccDLL, 'BASS_MP4_StreamCreateFileUser');
end;

procedure UnloadDynamicBassAACDLL;
begin
  FreeLibrary(FBassAccDLL);
end;
           {
initialization
  LoadDynamicBassAACDLL;

finalization
  UnloadDynamicBassAACDLL;    }

end.

