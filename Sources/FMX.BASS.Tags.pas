{
  Tags.dll written by Wraith, 2k5-2k6
  Delphi Wrapper written by Chris Troesken

  Dynamic_Tags.pas written by Wishmaster
  Updated by Wishmaster 02/11/2018

}

unit FMX.BASS.Tags;

interface

uses
  Windows;

const
{$IFDEF MSWINDOWS}
  tagsdll = 'tags.dll';
{$ENDIF}
{$IFDEF LINUX}
  tagsdll = 'libtags.so';
{$ENDIF}
{$IFDEF MACOS}
  tagsdll = 'libtags.dylib';
{$ENDIF}



var
   TAGS_GetVersion:function(): DWORD;  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
   TAGS_SetUTF8:function(enable: BOOL): BOOL;  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
   TAGS_Read:function(handle: DWORD; const fmt: PAnsiChar): PAnsiChar;  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
   TAGS_ReadEx:function(handle: DWORD; const fmt: PAnsiChar; tagtype: DWORD; codepage: LongInt): PAnsiChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
   TAGS_GetLastErrorDesc:function: PAnsiChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};



var
   BASSTAGS_Handle : Thandle = 0;

   function Load_BASSTAGSDLL(const dllfilename : string) : boolean;
   procedure Unload_BASSTAGSDLL;

implementation

function Load_BASSTAGSDLL(const dllfilename : string) : boolean;
{$IFDEF MSWINDOWS}
	var  oldmode : integer;
{$ENDIF}
begin
  if BASSTAGS_Handle <> 0 then // is it already there ?
   Result := true
  else
   begin {go & load the dll}
   {$IFDEF MSWINDOWS}
     oldmode := SetErrorMode($8001);
   {$ENDIF}

   {$IFDEF UNICODE}
     BASSTAGS_Handle:= LoadLibraryW(PWideChar(dllfilename));
   {$ELSE}
     BASSTAGS_Handle:= LoadLibrary(PChar(dllfilename));
   {$ENDIF}

   {$IFDEF MSWINDOWS}
    SetErrorMode(oldmode);
   {$ENDIF}

    if BASSTAGS_Handle <> 0 then
     begin
      @TAGS_GetVersion:= GetProcAddress(BASSTAGS_Handle, PChar('TAGS_GetVersion'));
      @TAGS_SetUTF8:= GetProcAddress(BASSTAGS_Handle, PChar('TAGS_SetUTF8'));
      @TAGS_Read:= GetProcAddress(BASSTAGS_Handle, PChar('TAGS_Read'));
      @TAGS_ReadEx:= GetProcAddress(BASSTAGS_Handle, PChar('TAGS_ReadEx'));
      @TAGS_GetLastErrorDesc:= GetProcAddress(BASSTAGS_Handle, PChar('TAGS_GetLastErrorDesc'));


  if (@TAGS_GetVersion = nil) or
     (@TAGS_SetUTF8 = nil) or
     (@TAGS_Read = nil) or
     (@TAGS_ReadEx = nil) or
     (@TAGS_GetLastErrorDesc = nil)
      then
     begin
      FreeLibrary(BASSTAGS_Handle);
      BASSTAGS_Handle := 0;
     end;
    end;
   result := (BASSTAGS_Handle <> 0);
 end;
end;


procedure Unload_BASSTAGSDLL;
begin
  if BASSTAGS_Handle <> 0 then
  FreeLibrary(BASSTAGS_Handle);
  BASSTAGS_Handle := 0;
end;



end.

