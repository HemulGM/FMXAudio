//********************************************************************************************************************************
//*                                                                                                                              *
//*     ID3v1 Library 2.1.94.214 © 3delite 2010-2025                                                                             *
//*     See ID3v2 Library ReadMe.txt for details.                                                                                *
//*                                                                                                                              *
//* Licenses available for this component:                                                                                       *
//* Freeware License: €25                                                                                                        *
//* Shareware License: €50                                                                                                       *
//* Commercial License: €250                                                                                                     *
//*                                                                                                                              *
//* 	https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/register.html#id3registration                             *
//*                                                                                                                              *
//* Home page:                                                                                                                   *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html                                         *
//*                                                                                                                              *
//* This component is also available as a part of Tags Library:                                                                  *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/TagsLibrary.html                                          *
//*                                                                                                                              *
//* There is an APEv2 Library available at:                                                                                      *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/APEv2Library.html                                         *
//*                                                                                                                              *
//* and an MP4 Tag Library available at:                                                                                         *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MP4TagLibrary.html                                        *
//*                                                                                                                              *
//* and an Ogg Vorbis and Opus Tag Library available at:                                                                         *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/OpusTagLibrary.html                                       *
//*                                                                                                                              *
//* and a Flac Tag Library available at:                                                                                         *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/FlacTagLibrary.html                                       *
//*                                                                                                                              *
//* and a WMA Tag Library available at:                                                                                          *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WMATagLibrary.html                                        *
//*                                                                                                                              *
//* and a WAV Tag Library available at:                                                                                          *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WAVTagLibrary.html                                        *
//*                                                                                                                              *
//* an MKV Tag Library available at:                                                                                             *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MKVTagLibrary.html                                        *
//*                                                                                                                              *
//* For other Delphi components see the home page:                                                                               *
//*                                                                                                                              *
//*     https://www.3delite.hu/                                                                                                  *
//*                                                                                                                              *
//* If you have any questions or enquiries please mail: 3delite@3delite.hu                                                       *
//*                                                                                                                              *
//* Good coding! :)                                                                                                              *
//* 3delite                                                                                                                      *
//********************************************************************************************************************************

unit ID3v1Library;

{$MINENUMSIZE 4}

{$IFDEF MSWINDOWS}
    {.$DEFINE CHARSETDETECTOR} //* Only Win32, copy 'chsdet.dll' beside the .exe
{$ENDIF}

{$IFDEF FPC}
    {$MODE DELPHIUNICODE}{$H+}
    {$HINTS OFF}
    {$WARNINGS OFF}
{$ELSE}
    {$IF CompilerVersion >= 24}
        {$LEGACYIFEND ON}
    {$IFEND}
{$ENDIF}

interface

Uses
    SysUtils,
    Classes,
    Generics.Collections,
    {$IFDEF FPC}
        {$IFDEF MSWINDOWS}
        Windows,
        {$ENDIF}
    {$ENDIF}
    Math;

Const

    ID3V1LIBRARY_VERSION = $020194214;
    STR_ID3V1LIBRARY_VERSION = '2.1.94.214';
    STR_ID3V1LIBRARY_VERSION_NAME = 'ID3v1 Library ' + STR_ID3V1LIBRARY_VERSION;
    STR_ID3V1LIBRARY_HOMEPAGE = 'https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/ID3v2Library.html';

    //* Options
    ID3V1LIBRARY_DEFAULT_TRIM_TEXT = True;
    ID3V1LIBRARY_DEFAULT_TREAT_UNKNOWN_GENRES_AS_OTHER = False;
    ID3V1LIBRARY_DEFAULT_RETURN_ANSI_TEXT = True;
    ID3V1LIBRARY_DEFAULT_SET_ANSI_TEXT = True;
    ID3V1LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME = False;

Const
    ID3V1TAGSIZE = 128;
    {$IFDEF NEXTGEN}
    StrLow = 0;
    {$ELSE}
    StrLow = 1;
    {$ENDIF}

var
    ID3V1TAGID: Array[0..2] of Byte;
    ID3LYRICSTAGIDSTART: Array[1..11] of Byte;
    ID3LYRICSTAGIDEND: Array[1..9] of Byte;
    INDFieldID: Array [1..8] of Byte;

Const
    ID3V1LIBRARY_SUCCESS                = 0;
    ID3V1LIBRARY_ERROR                  = $FFFF;
    ID3V1LIBRARY_ERROR_OPENING_FILE     = 3;
    ID3V1LIBRARY_ERROR_READING_FILE     = 4;
    ID3V1LIBRARY_ERROR_WRITING_FILE     = 5;
    ID3V1LIBRARY_ERROR_FILE_READ_ONLY   = 6;

Const
    ID3v1TLOFEANNONE = 0;
    ID3v1TLOFEANDEPROTECT = 1;
    ID3v1TLOFEANDEPROTECTANDRESTORE = 2;

type
    TID3v1TLOpenFileExceptionCallback = procedure (FileName: String; AException: Exception; var Action: Integer);

type
    TID3v1TagData = packed record
        Identifier: Array [0..2] of Byte;
        Title: Array [0..29] of Byte;
        Artist: Array [0..29] of Byte;
        Album: Array [0..29] of Byte;
        Year: Array [0..3] of Byte;
        Comment: Array [0..29] of Byte;
        Genre: Byte;
    end;

type
    TID3LyricsTagIDStart = Array [1..11] of Byte;
    TID3LyricsTagIDEnd = Array [1..9] of Byte;
    TID3LyricsTagSize = Array [1..6] of Byte;
    TID3LyricsFieldSize = Array [1..5] of Byte;

Const
    ID3Genres: Array[0..191] of String = (
        { The following genres are defined in ID3v1 }
        'Blues',
        'Classic Rock',
        'Country',
        'Dance',
        'Disco',
        'Funk',
        'Grunge',
        'Hip-Hop',
        'Jazz',
        'Metal',
        'New Age',
        'Oldies',
        'Other',     { <= 12 Default }
        'Pop',
        'R&B',
        'Rap',
        'Reggae',
        'Rock',
        'Techno',
        'Industrial',
        'Alternative',
        'Ska',
        'Death Metal',
        'Pranks',
        'Soundtrack',
        'Euro-Techno',
        'Ambient',
        'Trip-Hop',
        'Vocal',
        'Jazz+Funk',
        'Fusion',
        'Trance',
        'Classical',
        'Instrumental',
        'Acid',
        'House',
        'Game',
        'Sound Clip',
        'Gospel',
        'Noise',
        'AlternRock',
        'Bass',
        'Soul',
        'Punk',
        'Space',
        'Meditative',
        'Instrumental Pop',
        'Instrumental Rock',
        'Ethnic',
        'Gothic',
        'Darkwave',
        'Techno-Industrial',
        'Electronic',
        'Pop-Folk',
        'Eurodance',
        'Dream',
        'Southern Rock',
        'Comedy',
        'Cult',
        'Gangsta',
        'Top 40',
        'Christian Rap',
        'Pop/Funk',
        'Jungle',
        'Native American',
        'Cabaret',
        'New Wave',
        'Psychedelic', // = 'Psychadelic' in ID3 docs, 'Psychedelic' in winamp.
        'Rave',
        'Showtunes',
        'Trailer',
        'Lo-Fi',
        'Tribal',
        'Acid Punk',
        'Acid Jazz',
        'Polka',
        'Retro',
        'Musical',
        'Rock & Roll',
        'Hard Rock',
        { The following genres are Winamp extensions }
        'Folk',
        'Folk-Rock',
        'National Folk',
        'Swing',
        'Fast Fusion',
        'Bebob',
        'Latin',
        'Revival',
        'Celtic',
        'Bluegrass',
        'Avantgarde',
        'Gothic Rock',
        'Progressive Rock',
        'Psychedelic Rock',
        'Symphonic Rock',
        'Slow Rock',
        'Big Band',
        'Chorus',
        'Easy Listening',
        'Acoustic',
        'Humour',
        'Speech',
        'Chanson',
        'Opera',
        'Chamber Music',
        'Sonata',
        'Symphony',
        'Booty Bass',
        'Primus',
        'Porn Groove',
        'Satire',
        'Slow Jam',
        'Club',
        'Tango',
        'Samba',
        'Folklore',
        'Ballad',
        'Power Ballad',
        'Rhythmic Soul',
        'Freestyle',
        'Duet',
        'Punk Rock',
        'Drum Solo',
        'A capella', // A Capella
        'Euro-House',
        'Dance Hall',
        { winamp ?? genres }
        'Goa',
        'Drum & Bass',
        'Club-House',
        'Hardcore',
        'Terror',
        'Indie',
        'BritPop',
        'Negerpunk',
        'Polsk Punk',
        'Beat',
        'Christian Gangsta Rap',
        'Heavy Metal',
        'Black Metal',
        'Crossover',
        'Contemporary Christian',
        'Christian Rock',
        { winamp 1.91 genres }
        'Merengue',
        'Salsa',
        'Trash Metal',
        { winamp 1.92 genres }
        'Anime',
        'JPop',
        'SynthPop',
        {Winamp 5.6}
        'Abstract',
        'Art Rock',
        'Baroque',
        'Bhangra',
        'Big Beat',
        'Breakbeat',
        'Chillout',
        'Downtempo',
        'Dub',
        'EBM',
        'Eclectic',
        'Electro',
        'Electroclash',
        'Emo',
        'Experimental',
        'Garage',
        'Global',
        'IDM',
        'Illbient',
        'Industro-Goth',
        'Jam Band',
        'Krautrock',
        'Leftfield',
        'Lounge',
        'Math Rock',
        'New Romantic',
        'Nu-Breakz',
        'Post-Punk',
        'Post-Rock',
        'Psytrance',
        'Shoegaze',
        'Space Rock',
        'Trop Rock',
        'World Music',
        'Neoclassical',
        'Audiobook',
        'Audio Theatre',
        'Neue Deutsche Welle',
        'Podcast',
        'Indie Rock',
        'G-Funk',
        'Dubstep',
        'Garage Rock',
        'Psybient'
    );

type
    TID3LyricsFrameID = Array [0..2] of Byte;

type
    TID3v1Tag = class;

    TID3LyricsFrame = class
        Parent: TID3v1Tag;
        ID: String;
        Data: String;
        function Index: Integer;
    end;

    TID3v1Tag = class
    private
        FLyricsTagSize: Cardinal;
        FFileSize: Int64;
    public
        SourceFileName: String;
        Loaded: Boolean;
        Revision1: Boolean;
        Title: String;
        Artist: String;
        Album: String;
        Year: String;
        Comment: String;
        Track: Byte;
        TrackString: String;
        Genre: String;
        LyricsFrames: TList<TID3LyricsFrame>;
        LyricsHasTimeStamp: Boolean;
        InhibitTracksForRandomSelection: Boolean;
        TrimText: Boolean;
        TreatUnknownGenresAsOther: Boolean;
        ReturnANSIText: Boolean;
        SetANSIText: Boolean;
        PreserveFileDateTime: Boolean;
        Constructor Create;
        Destructor Destroy; override;
        function LoadFromFile(FileName: String): Integer;
        function LoadFromStream(TagStream: TStream): Integer;
        function LoadFromMemory(MemoryAddress: Pointer): Integer;
        function SaveToFile(FileName: String; WriteLyricsTag: Boolean = False): Integer;
        function SaveToStream(var TagStream: TStream; WriteLyricsTag: Boolean = False): Integer;
        function LoadLyricsTag(TagStream: TStream): Integer;
        function LyricsTagSaveToStream(TagStream: TStream): Integer;
        procedure Clear;
        function AddLyricsFrame(ID: String): TID3LyricsFrame;
        function SetLyricsFrame(ID: String; Value: String): TID3LyricsFrame;
        function DeleteLyricsFrame(Index: Integer): Boolean; overload;
        function DeleteLyricsFrame(ID: String): Boolean; overload;
        function GetLyrics: String;
        procedure SetLyrics(Text: String);
        function FindLyricsFrame(ID: String): TID3LyricsFrame;
        property Lyrics: String read GetLyrics write SetLyrics;
        function Assign(Source: TID3v1Tag): Boolean;
        property LyricsSize: Cardinal read FLyricsTagSize;
        property FileSize: Int64 read FFileSize;
    end;

    procedure StringToPAnsiChar(const Source: String; Dest: PByte; const MaxLength: Integer); inline;
    procedure AnsiStringToPAnsiChar(const Source: String; Dest: PByte; const MaxLength: Integer);{$IF DEFINED(FPC) OR (RTLVersion >= 23)} inline;{$IFEND}
    function PAnsiCharToString(P: PByte; MaxLength: Integer): String; inline;
    function PAnsiCharToAnsiString(P: PByte; MaxLength: Integer): String;{$IF DEFINED(FPC) OR (RTLVersion >= 23)} inline;{$IFEND}

    function ID3GenreDataToString(GenreIndex: Byte): String;
    function ID3GenreDataToStringOther(GenreIndex: Byte): String;
    function ID3GenreStringToData(Genre: String): Byte;

    function RemoveID3v1TagFromFile(FileName: String; PreserveFileDateTime: Boolean = ID3V1LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME): Integer;
    function RemoveID3v1TagFromStream(Stream: TStream): Integer;

    function ID3v1TagErrorCode2String(ErrorCode: Integer): String;

var
    ID3v1LibraryDefaultTrimText: Boolean = ID3V1LIBRARY_DEFAULT_TRIM_TEXT;
    ID3v1LibraryTreatUnknownGenresAsOther: Boolean = ID3V1LIBRARY_DEFAULT_TREAT_UNKNOWN_GENRES_AS_OTHER;
    ID3v1LibraryDefaultReturnANSIText: Boolean = ID3V1LIBRARY_DEFAULT_RETURN_ANSI_TEXT;
    ID3v1LibraryDefaultSetANSIText: Boolean = ID3V1LIBRARY_DEFAULT_SET_ANSI_TEXT;
    ID3v1LibraryDefaultPreserveFileDateTime: Boolean = ID3V1LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME;
    ID3v1LibraryOpenFileExceptionCallback: TID3v1TLOpenFileExceptionCallback;

implementation

{$IFDEF CHARSETDETECTOR}
Uses
    Windows,
    chsd_dll_intf;
{$ENDIF}

{$IF NOT DEFINED(FPC) AND (RTLVersion < 23)}
type
    TEncodingHelper = class helper for TEncoding
    public
        class function ANSI: TEncoding; static;
    end;

class function TEncodingHelper.ANSI: TEncoding;
begin
    Result := Default;
end;
{$IFEND}

{$IFDEF FPC}
procedure FileSetReadOnly(FileName: String; ReadOnly: Boolean);
begin
    if ReadOnly then begin
        FileSetAttr(FileName, FileGetAttr(FileName) OR faReadOnly);
    end else begin
        FileSetAttr(FileName, FileGetAttr(FileName) AND (NOT faReadOnly));
    end;
end;
{$ENDIF}

type
    TStreamHelper = class helper for TStream
    public
        function Read2(var Buffer; Count: Longint): Longint;
    end;

function TStreamHelper.Read2(var Buffer; Count: Longint): Longint;
begin
    FillChar(Buffer, Count, 0);
    Result := Read(Buffer, Count);
end;

procedure StringToPAnsiChar(const Source: String; Dest: PByte; const MaxLength: Integer);
var
    i: Integer;
begin
    for i := StrLow to StrLow + Min(MaxLength, Length(Source)) - 1 do begin
        Dest^ := Byte(Source[i]);
        Inc(Dest);
    end;
end;

procedure AnsiStringToPAnsiChar(const Source: String; Dest: PByte; const MaxLength: Integer);
var
    i: Integer;
    Bytes: TBytes;
begin
    {$IFDEF FPC}
        {$IFDEF MSWINDOWS}
        Bytes := TEncoding.GetEncoding(GetACP).GetBytes(Source);
        {$ELSE}
        Bytes := TEncoding.ANSI.GetBytes(Source);
        {$ENDIF}
    {$ELSE}
    Bytes := TEncoding.ANSI.GetBytes(Source);
    {$ENDIF}
    for i := 0 to Min(MaxLength, Length(Bytes)) - 1 do begin
        Dest^ := Byte(Bytes[i]);
        Inc(Dest);
    end;
end;

function PAnsiCharToString(P: PByte; MaxLength: Integer): String;
var
    Data: Byte;
    Counter: Integer;
begin
    SetLength(Result, MaxLength);
    Counter := 0;
    repeat
        Data := P^;
        Result[StrLow + Counter] := Char(Data);
        Inc(P);
        Inc(Counter);
    until (Counter >= MaxLength)
    OR (Data = 0);
    if Data = 0 then begin
        SetLength(Result, Counter - 1);
    end else begin
        SetLength(Result, Counter);
    end;
end;

function PAnsiCharToAnsiString(P: PByte; MaxLength: Integer): String;
var
    Data: Byte;
    Bytes: TBytes;
    Counter: Integer;
    {$IFDEF CHARSETDETECTOR}
    WideString: String;
    CharsetInfo: rCharsetInfo;
    OutputLength: Integer;
    {$ENDIF}
begin
    SetLength(Bytes, MaxLength);
    Counter := 0;
    repeat
        Data := P^;
        Bytes[Counter] := Data;
        Inc(P);
        Inc(Counter);
    until (Counter >= MaxLength)
    OR (Data = 0);
    if Data = 0 then begin
        SetLength(Bytes, Counter - 1);
    end else begin
        SetLength(Bytes, Counter);
    end;
    {$IFDEF CHARSETDETECTOR}
    csd_Reset;
    csd_HandleData(@Bytes[0], Length(Bytes));
    if not csd_Done then begin
        csd_DataEnd;
    end;
    CharsetInfo := csd_GetDetectedCharset;
    if (CharsetInfo.CodePage > 0)
    AND Windows.IsValidCodePage(CharsetInfo.CodePage)
    then begin
        OutputLength := MultiByteToWideChar(CharsetInfo.CodePage, 0, @Bytes[0], Length(Bytes), nil, 0);
        SetLength(WideString, OutputLength);
        if MultiByteToWideChar(CharsetInfo.CodePage, 0, @Bytes[0], Length(Bytes), PWideChar(WideString), OutputLength) <> 0 then begin
            Result := WideString;
        end;
    end else begin
        Result := TEncoding.ANSI.GetString(Bytes);
    end;
    {$ELSE}
        {$IFDEF FPC}
            {$IFDEF MSWINDOWS}
            Result := TEncoding.GetEncoding(GetACP).GetString(Bytes);
            {$ELSE}
            Result := TEncoding.ANSI.GetString(Bytes);
            {$ENDIF}
        {$ELSE}
        Result := TEncoding.ANSI.GetString(Bytes);
        {$ENDIF}
    {$ENDIF}
end;

Constructor TID3v1Tag.Create;
begin
    Inherited;
    TrimText := ID3v1LibraryDefaultTrimText;
    TreatUnknownGenresAsOther := ID3v1LibraryTreatUnknownGenresAsOther;
    ReturnANSIText := ID3v1LibraryDefaultReturnANSIText;
    SetANSIText := ID3v1LibraryDefaultSetANSIText;
    PreserveFileDateTime := ID3v1LibraryDefaultPreserveFileDateTime;
    LyricsFrames := TList<TID3LyricsFrame>.Create;
    Clear;
end;

function TID3v1Tag.DeleteLyricsFrame(Index: Integer): Boolean;
begin
    Result := False;
    if (Index > LyricsFrames.Count - 1)
    OR (Index < 0)
    then begin
        Exit;
    end;
    LyricsFrames[Index].Free;
    LyricsFrames.Delete(Index);
    Result := True;
end;

function TID3v1Tag.DeleteLyricsFrame(ID: String): Boolean;
var
    i: Integer;
begin
    Result := False;
    for i := 0 to LyricsFrames.Count - 1 do begin
        if LyricsFrames[i].ID = ID then begin
            LyricsFrames[i].Free;
            LyricsFrames.Delete(i);
            Result := True;
            Exit;
        end;
    end;
end;

Destructor TID3v1Tag.Destroy;
begin
    Clear;
    LyricsFrames.Free;
    Inherited;
end;

function TID3v1Tag.FindLyricsFrame(ID: String): TID3LyricsFrame;
var
    i: Integer;
begin
    Result := nil;
    for i := 0 to LyricsFrames.Count - 1 do begin
        if LyricsFrames[i].ID = ID then begin
            Result := LyricsFrames[i];
            Exit;
        end;
    end;
end;

function TID3v1Tag.AddLyricsFrame(ID: String): TID3LyricsFrame;
var
    LyricsFrame: TID3LyricsFrame;
begin
    Result := nil;
    try
        LyricsFrame := TID3LyricsFrame.Create;
        LyricsFrame.ID := ID;
        LyricsFrame.Parent := Self;
        LyricsFrames.Add(LyricsFrame);
        Result := LyricsFrame;
    except
        //*
    end;
end;

function TID3v1Tag.Assign(Source: TID3v1Tag): Boolean;
var
    i: Integer;
begin
    Clear;
    SourceFileName := Source.SourceFileName;
    Loaded := Source.Loaded;
    Revision1 := Source.Revision1;
    Title := Source.Title;
    Artist := Source.Artist;
    Album := Source.Album;
    Year := Source.Year;
    Comment := Source.Comment;
    Track := Source.Track;
    Genre := Source.Genre;
    for i := 0 to Source.LyricsFrames.Count - 1 do begin
        AddLyricsFrame(Source.LyricsFrames[i].ID).Data := Source.LyricsFrames[i].Data;
    end;
    LyricsHasTimeStamp := Source.LyricsHasTimeStamp;
    InhibitTracksForRandomSelection := Source.InhibitTracksForRandomSelection;
    Result := True;
end;

procedure TID3v1Tag.Clear;
var
    i: Integer;
begin
    SourceFileName := '';
    Loaded := False;
    Revision1 := False;
    Title := '';
    Artist := '';
    Album := '';
    Year := '';
    Comment := '';
    Genre := '';
    Track := 0;
    TrackString := '';
    for i := LyricsFrames.Count - 1 downto 0 do begin
        LyricsFrames[i].Free;
    end;
    LyricsFrames.Clear;
    FLyricsTagSize := 0;
    FFileSize := 0;;
end;

function TID3v1Tag.LoadFromFile(FileName: String): Integer;
var
    FileStream: TFileStream;
begin
    try
        Clear;
        Loaded := False;
        if NOT FileExists(FileName) then begin
            Result := ID3V1LIBRARY_ERROR_OPENING_FILE;
            Exit;
        end;
        FileStream := TFileStream.Create(FileName, fmOpenRead OR fmShareDenyWrite);
        try
            //FileStream.Seek(-ID3V1TAGSIZE, soEnd);
            Result := LoadFromStream(FileStream);
            if Result = ID3V1LIBRARY_SUCCESS then begin
                Self.SourceFileName := FileName;
            end;
            //LoadLyricsTag(FileStream);
        finally
            FreeAndNil(FileStream);
        end;
    except
        Result := ID3V1LIBRARY_ERROR;
    end;
end;

function TID3v1Tag.LoadFromStream(TagStream: TStream): Integer;
var
    TagData: TID3v1TagData;
begin
    Result := ID3V1LIBRARY_ERROR;
    Loaded := False;
    Clear;
    try
        FFileSize := TagStream.Size;
        if TagStream.Size > ID3V1TAGSIZE then begin
            TagStream.Seek(- ID3V1TAGSIZE, soEnd);
        end else begin
            TagStream.Seek(0, soBeginning);
        end;
        TagStream.Read2(TagData, ID3V1TAGSIZE);
        if (TagData.Identifier[0] <> ID3V1TAGID[0])
        OR (TagData.Identifier[1] <> ID3V1TAGID[1])
        OR (TagData.Identifier[2] <> ID3V1TAGID[2])
        then begin
            Exit;
        end;
        if ReturnANSIText then begin
            Title := PAnsiCharToAnsiString(@TagData.Title, 30);
            Artist := PAnsiCharToAnsiString(@TagData.Artist, 30);
            Album := PAnsiCharToAnsiString(@TagData.Album, 30);
            Year := PAnsiCharToAnsiString(@TagData.Year, 4);
            Comment := PAnsiCharToAnsiString(@TagData.Comment, 30);
        end else begin
            Title := PAnsiCharToString(@TagData.Title, 30);
            Artist := PAnsiCharToString(@TagData.Artist, 30);
            Album := PAnsiCharToString(@TagData.Album, 30);
            Year := PAnsiCharToString(@TagData.Year, 4);
            Comment := PAnsiCharToString(@TagData.Comment, 30);
        end;
        if TreatUnknownGenresAsOther then begin
            Genre := ID3GenreDataToStringOther(TagData.Genre);
        end else begin
            Genre := ID3GenreDataToString(TagData.Genre);
        end;
        if TrimText then begin
            Title := Trim(Title);
            Artist := Trim(Artist);
            Album := Trim(Album);
            Year := Trim(Year);
            Comment := Trim(Comment);
            Genre := Trim(Genre);
        end;
        if TagData.Comment[28] = 0 then begin
            Track := Byte(TagData.Comment[29]);
            TrackString := IntToStr(Track);
            Revision1 := True;
        end else begin
            Track := 0;
            TrackString := '';
            Revision1 := False;
        end;
        if TagStream.Size > ID3V1TAGSIZE then begin
            LoadLyricsTag(TagStream);
        end;
        Loaded := True;
        Result := ID3V1LIBRARY_SUCCESS;
    except
        Clear;
        Result := ID3V1LIBRARY_ERROR_READING_FILE;
    end;
end;

function TID3v1Tag.LoadFromMemory(MemoryAddress: Pointer): Integer;
var
    DataStream: TMemoryStream;
begin
    Result := ID3V1LIBRARY_ERROR;
    if MemoryAddress <> nil then begin
        DataStream := TMemoryStream.Create;
        try
            DataStream.Write(MemoryAddress^, ID3V1TAGSIZE);
            DataStream.Seek(0, soBeginning);
            Result := LoadFromStream(DataStream);
        finally
            FreeAndNil(DataStream);
        end;
    end;
end;

function TID3v1Tag.LoadLyricsTag(TagStream: TStream): Integer;
var
    LyricsTagIDStart: TID3LyricsTagIDStart;
    LyricsTagIDEnd: TID3LyricsTagIDEnd;
    LyricsTagSize: TID3LyricsTagSize;
    //TagSize: Cardinal;
    FieldID: TID3LyricsFrameID;
    FieldIDString: String;
    LyricsFieldSize: TID3LyricsFieldSize;
    FieldSize: Cardinal;
    LyricsFrame: TID3LyricsFrame;
    i: Integer;
    Data: Byte;
begin
    Result := ID3V1LIBRARY_ERROR;
    TagStream.Seek(- (ID3V1TAGSIZE + 9), soEnd);
    TagStream.Read2(LyricsTagIDEnd, SizeOf(TID3LyricsTagIDEnd));
    if (LyricsTagIDEnd[1] = ID3LYRICSTAGIDEND[1])
    AND (LyricsTagIDEnd[2] = ID3LYRICSTAGIDEND[2])
    AND (LyricsTagIDEnd[3] = ID3LYRICSTAGIDEND[3])
    AND (LyricsTagIDEnd[4] = ID3LYRICSTAGIDEND[4])
    AND (LyricsTagIDEnd[5] = ID3LYRICSTAGIDEND[5])
    AND (LyricsTagIDEnd[6] = ID3LYRICSTAGIDEND[6])
    AND (LyricsTagIDEnd[7] = ID3LYRICSTAGIDEND[7])
    AND (LyricsTagIDEnd[8] = ID3LYRICSTAGIDEND[8])
    AND (LyricsTagIDEnd[9] = ID3LYRICSTAGIDEND[9])
    then begin
        TagStream.Seek(- (ID3V1TAGSIZE + 9 + 6), soEnd);
        TagStream.Read2(LyricsTagSize, SizeOf(TID3LyricsTagSize));
        FLyricsTagSize := StrToInt(Char(LyricsTagSize[1])
             + Char(LyricsTagSize[2])
             + Char(LyricsTagSize[3])
             + Char(LyricsTagSize[4])
             + Char(LyricsTagSize[5])
             + Char(LyricsTagSize[6])
             ) + 6 + 9;
        TagStream.Seek(- (ID3V1TAGSIZE {+ 9 + 6} + FLyricsTagSize), soEnd);
        TagStream.Read2(LyricsTagIDStart, SizeOf(TID3LyricsTagIDStart));
        if (LyricsTagIDStart[1] = ID3LYRICSTAGIDSTART[1])
        AND (LyricsTagIDStart[2] = ID3LYRICSTAGIDSTART[2])
        AND (LyricsTagIDStart[3] = ID3LYRICSTAGIDSTART[3])
        AND (LyricsTagIDStart[4] = ID3LYRICSTAGIDSTART[4])
        AND (LyricsTagIDStart[5] = ID3LYRICSTAGIDSTART[5])
        AND (LyricsTagIDStart[6] = ID3LYRICSTAGIDSTART[6])
        AND (LyricsTagIDStart[7] = ID3LYRICSTAGIDSTART[7])
        AND (LyricsTagIDStart[8] = ID3LYRICSTAGIDSTART[8])
        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
        then begin
            repeat
                TagStream.Read2(FieldID, SizeOf(TID3LyricsFrameID));
                TagStream.Read2(LyricsFieldSize, SizeOf(TID3LyricsFieldSize));
                FieldSize := StrToInt(Char(LyricsFieldSize[1])
                     + Char(LyricsFieldSize[2])
                     + Char(LyricsFieldSize[3])
                     + Char(LyricsFieldSize[4])
                     + Char(LyricsFieldSize[5])
                     );
                FieldIDString := Char(FieldID[0]) + Char(FieldID[1]) + Char(FieldID[2]);
                LyricsFrame := AddLyricsFrame(FieldIDString);
                for i := 0 to FieldSize - 1 do begin
                    TagStream.Read2(Data, 1);
                    LyricsFrame.Data := LyricsFrame.Data + Char(Data);
                end;
                if FieldIDString = 'IND' then begin
                    LyricsHasTimeStamp := Copy(LyricsFrame.Data, 2, 1) = '1';
                    InhibitTracksForRandomSelection := Copy(LyricsFrame.Data, 3, 1) = '1';
                end;
                if FieldIDString = 'EAL' then begin
                    if (Album = '')
                    OR (Pos(Album, LyricsFrame.Data) > 0)
                    then begin
                        Album := LyricsFrame.Data;
                    end;
                end;
                if FieldIDString = 'EAR' then begin
                    if (Artist = '')
                    OR (Pos(Artist, LyricsFrame.Data) > 0)
                    then begin
                        Artist := LyricsFrame.Data;
                    end;
                end;
                if FieldIDString = 'ETT' then begin
                    if (Title = '')
                    OR (Pos(Title, LyricsFrame.Data) > 0)
                    then begin
                        Title := LyricsFrame.Data;
                    end;
                end;
                Result := ID3V1LIBRARY_SUCCESS;
            until TagStream.Position >= TagStream.Size - (ID3V1TAGSIZE + 9 + 6);
        end;
    end;
end;

function TID3v1Tag.LyricsTagSaveToStream(TagStream: TStream): Integer;

    function NumberToFieldSize(Value: Cardinal): TID3LyricsFieldSize;
    var
        Text: String;
    begin
        Text := IntToStr(Value);
        while Length(Text) < 5 do begin
            Text := '0' + Text;
        end;
        AnsiStringToPAnsiChar(Text, @Result, 5);
    end;

    function NumberToTagSize(Value: Cardinal): TID3LyricsTagSize;
    var
        Text: String;
    begin
        Text := IntToStr(Value);
        while Length(Text) < 6 do begin
            Text := '0' + Text;
        end;
        AnsiStringToPAnsiChar(Text, @Result, 6);
    end;

var
    LyricsTagIDStart: TID3LyricsTagIDStart;
    LyricsTagIDEnd: TID3LyricsTagIDEnd;
    LyricsTagSize: TID3LyricsTagSize;
    TagSize: Integer;
    LyricsFieldSize: TID3LyricsFieldSize;
    i: Integer;
    Data: Byte;
    LyricsFrameID: TID3LyricsFrameID;
    Bytes: TBytes;
begin
    try
        TagStream.Seek(- 9, soEnd);
        TagStream.Read2(LyricsTagIDEnd, SizeOf(TID3LyricsTagIDEnd));
        if (LyricsTagIDEnd[1] = ID3LYRICSTAGIDEND[1])
        AND (LyricsTagIDEnd[2] = ID3LYRICSTAGIDEND[2])
        AND (LyricsTagIDEnd[3] = ID3LYRICSTAGIDEND[3])
        AND (LyricsTagIDEnd[4] = ID3LYRICSTAGIDEND[4])
        AND (LyricsTagIDEnd[5] = ID3LYRICSTAGIDEND[5])
        AND (LyricsTagIDEnd[6] = ID3LYRICSTAGIDEND[6])
        AND (LyricsTagIDEnd[7] = ID3LYRICSTAGIDEND[7])
        AND (LyricsTagIDEnd[8] = ID3LYRICSTAGIDEND[8])
        AND (LyricsTagIDEnd[9] = ID3LYRICSTAGIDEND[9])
        then begin
            TagStream.Seek(- (9 + 6), soEnd);
            TagStream.Read2(LyricsTagSize, SizeOf(TID3LyricsTagSize));
            TagSize := StrToInt(Char(LyricsTagSize[1])
                 + Char(LyricsTagSize[2])
                 + Char(LyricsTagSize[3])
                 + Char(LyricsTagSize[4])
                 + Char(LyricsTagSize[5])
                 + Char(LyricsTagSize[6])
                 );
            TagStream.Seek(- (9 + 6 + TagSize), soEnd);
            TagStream.Read2(LyricsTagIDStart, SizeOf(TID3LyricsTagIDStart));
            if (LyricsTagIDStart[1] = ID3LYRICSTAGIDSTART[1])
            AND (LyricsTagIDStart[2] = ID3LYRICSTAGIDSTART[2])
            AND (LyricsTagIDStart[3] = ID3LYRICSTAGIDSTART[3])
            AND (LyricsTagIDStart[4] = ID3LYRICSTAGIDSTART[4])
            AND (LyricsTagIDStart[5] = ID3LYRICSTAGIDSTART[5])
            AND (LyricsTagIDStart[6] = ID3LYRICSTAGIDSTART[6])
            AND (LyricsTagIDStart[7] = ID3LYRICSTAGIDSTART[7])
            AND (LyricsTagIDStart[8] = ID3LYRICSTAGIDSTART[8])
            AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
            AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
            AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
            then begin
                TagStream.Size := TagStream.Position - SizeOf(TID3LyricsTagIDStart);
            end;
        end;
        TagStream.Seek(0, soEnd);
        TagSize := 0;
        AnsiStringToPAnsiChar('LYRICSBEGIN', @LyricsTagIDStart, Length(ID3LYRICSTAGIDSTART));
        TagSize := TagSize + TagStream.Write(LyricsTagIDStart, Length(LyricsTagIDStart));
        TagSize := TagSize + TagStream.Write(INDFieldID[1], 8);
        if FindLyricsFrame('LYR') <> nil then begin
            Data := Ord('1');
        end else begin
            Data := Ord('0');
        end;
        TagSize := TagSize + TagStream.Write(Data, 1);
        if LyricsHasTimeStamp then begin
            Data := Ord('1');
        end else begin
            Data := Ord('0');
        end;
        TagSize := TagSize + TagStream.Write(Data, 1);
        if InhibitTracksForRandomSelection then begin
            Data := Ord('1');
        end else begin
            Data := Ord('0');
        end;
        TagSize := TagSize + TagStream.Write(Data, 1);
        for i := 0 to LyricsFrames.Count - 1 do begin
            if Length(LyricsFrames[i].Data) = 0 then begin
                Continue;
            end;
            if LyricsFrames[i].ID = 'IND' then begin
                Continue;
            end;
            AnsiStringToPAnsiChar(LyricsFrames[i].ID, @LyricsFrameID, SizeOf(LyricsFrameID));
            TagSize := TagSize + TagStream.Write(LyricsFrameID[0], 3);
            if LyricsFrames[i].ID = 'EAL' then begin
                SetLength(Bytes, 0);
                Bytes := TEncoding.UTF8.GetBytes(Album);
                LyricsFieldSize := NumberToFieldSize(Length(Bytes));
                TagSize := TagSize + TagStream.Write(LyricsFieldSize, 5);
                TagSize := TagSize + TagStream.Write(Bytes[0], Length(Bytes));
                Continue;
            end;
            if LyricsFrames[i].ID = 'EAR' then begin
                SetLength(Bytes, 0);
                Bytes := TEncoding.UTF8.GetBytes(Artist);
                LyricsFieldSize := NumberToFieldSize(Length(Bytes));
                TagSize := TagSize + TagStream.Write(LyricsFieldSize, 5);
                TagSize := TagSize + TagStream.Write(Bytes[0], Length(Bytes));
                Continue;
            end;
            if LyricsFrames[i].ID = 'ETT' then begin
                SetLength(Bytes, 0);
                Bytes := TEncoding.UTF8.GetBytes(Title);
                LyricsFieldSize := NumberToFieldSize(Length(Bytes));
                TagSize := TagSize + TagStream.Write(LyricsFieldSize, 5);
                TagSize := TagSize + TagStream.Write(Bytes[0], Length(Bytes));
                Continue;
            end;
            SetLength(Bytes, 0);
            Bytes := TEncoding.UTF8.GetBytes(LyricsFrames[i].Data);
            LyricsFieldSize := NumberToFieldSize(Length(Bytes));
            TagSize := TagSize + TagStream.Write(LyricsFieldSize, 5);
            TagSize := TagSize + TagStream.Write(Bytes[0], Length(Bytes));
        end;
        LyricsTagSize := NumberToTagSize(TagSize);
        TagStream.Write(LyricsTagSize, 6);
        AnsiStringToPAnsiChar('LYRICS200', @LyricsTagIDEnd, Length(ID3LYRICSTAGIDEND));
        TagStream.Write(LyricsTagIDEnd, Length(LyricsTagIDEnd));
        Result := ID3V1LIBRARY_SUCCESS;
    except
        Result := ID3V1LIBRARY_ERROR;
    end;
end;

function TID3v1Tag.GetLyrics: String;
var
    i: Integer;
begin
    Result := '';
    for i := 0 to LyricsFrames.Count - 1 do begin
        if LyricsFrames[i].ID = 'LYR' then begin
            Result := LyricsFrames[i].Data;
        end;
    end;
end;

procedure TID3v1Tag.SetLyrics(Text: String);
var
    i: Integer;
begin
    for i := 0 to LyricsFrames.Count - 1 do begin
        if LyricsFrames[i].ID = 'LYR' then begin
            if Text <> '' then begin
                LyricsFrames[i].Data := Text;
            end else begin
                DeleteLyricsFrame(i);
            end;
            Exit;
        end;
    end;
    if Text <> '' then begin
        AddLyricsFrame('LYR').Data := Text;
    end;
end;

function TID3v1Tag.SetLyricsFrame(ID: String; Value: String): TID3LyricsFrame;
begin
    Result := FindLyricsFrame(ID);
    if NOT Assigned(Result) then begin
        Result := AddLyricsFrame(ID);
    end;
    Result.Data := Value;
end;

function TID3v1Tag.SaveToFile(FileName: String; WriteLyricsTag: Boolean = False): Integer;
var
    FileDateTime: TDateTime;
    Action: Integer;
    FileStream: TStream;
begin
    Action := ID3v1TLOFEANNONE;
    try
        if PreserveFileDateTime then begin
            FileAge(FileName, FileDateTime);
        end;

        if FileIsReadOnly(FileName) then begin
            if Assigned(ID3v1LibraryOpenFileExceptionCallback) then begin
                ID3v1LibraryOpenFileExceptionCallback(FileName, nil, Action);
                case Action of
                    ID3v1TLOFEANNONE: begin
                        Result := ID3V1LIBRARY_ERROR_FILE_READ_ONLY;
                        Exit;
                    end;
                    ID3v1TLOFEANDEPROTECT, ID3v1TLOFEANDEPROTECTANDRESTORE: begin
                        FileSetReadOnly(FileName, False);
                    end;
                end;
            end else begin
                Result := ID3V1LIBRARY_ERROR_FILE_READ_ONLY;
                Exit;
            end;
        end;

        if WriteLyricsTag then begin
            RemoveID3v1TagFromFile(FileName, False);
        end;
        if FileExists(FileName) then begin
            FileStream := TFileStream.Create(FileName, fmOpenReadWrite OR fmShareDenyWrite);
        end else begin
            FileStream := TFileStream.Create(FileName, fmCreate OR fmShareDenyWrite);
        end;
    except
        Result := ID3V1LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        if WriteLyricsTag then begin
            LyricsTagSaveToStream(FileStream);
        end;
        FileStream.Seek(-ID3V1TAGSIZE, soEnd);
        Result := SaveToStream(FileStream);
        if Result = ID3V1LIBRARY_SUCCESS then begin
            Self.SourceFileName := FileName;
        end;
    finally
        FreeAndNil(FileStream);
        if PreserveFileDateTime then begin
            FileSetDate(FileName, DateTimeToFileDate(FileDateTime));
        end;
        if Action = ID3v1TLOFEANDEPROTECTANDRESTORE then begin
            FileSetReadOnly(FileName, True);
        end;
    end;
end;

function TID3v1Tag.SaveToStream(var TagStream: TStream; WriteLyricsTag: Boolean = False): Integer;
var
    TagData: TID3v1TagData;
begin
    try
        if TagStream.Size > ID3V1TAGSIZE then begin
            TagStream.Seek(- ID3V1TAGSIZE, soEnd);
        end else begin
            TagStream.Seek(0, soBeginning);
        end;
        TagStream.Read2(TagData, ID3V1TAGSIZE);
    except
        Result := ID3V1LIBRARY_ERROR_READING_FILE;
        Exit;
    end;
    try

        if WriteLyricsTag then begin
           RemoveID3v1TagFromStream(TagStream);
           TagStream.Seek(0, soEnd);
           LyricsTagSaveToStream(TagStream);
           TagStream.Seek(0, soEnd);
        end else begin
            if (TagData.Identifier[0] = ID3V1TAGID[0])
            AND (TagData.Identifier[1] = ID3V1TAGID[1])
            AND (TagData.Identifier[2] = ID3V1TAGID[2])
            then begin
                TagStream.Seek(- ID3V1TAGSIZE, soCurrent);
            end else begin
                TagStream.Seek(0, soEnd);
            end;
        end;
        FillChar(TagData, SizeOf(TID3v1TagData), #0);
        Move(ID3V1TAGID[0], TagData.Identifier[0], 3);
        if SetANSIText then begin
            AnsiStringToPAnsiChar(Title, @TagData.Title, 30);
            AnsiStringToPAnsiChar(Artist, @TagData.Artist, 30);
            AnsiStringToPAnsiChar(Album, @TagData.Album, 30);
            AnsiStringToPAnsiChar(Year, @TagData.Year, 4);
            AnsiStringToPAnsiChar(Comment, @TagData.Comment, 30);
        end else begin
            StringToPAnsiChar(Title, @TagData.Title, 30);
            StringToPAnsiChar(Artist, @TagData.Artist, 30);
            StringToPAnsiChar(Album, @TagData.Album, 30);
            StringToPAnsiChar(Year, @TagData.Year, 4);
            StringToPAnsiChar(Comment, @TagData.Comment, 30);
        end;
        TagData.Genre := ID3GenreStringToData(Genre);
        if TagData.Comment[28] = 0 then begin
            TagData.Comment[29] := Byte(Track);
        end;
        TagStream.Write(TagData, ID3V1TAGSIZE);
        Result := ID3V1LIBRARY_SUCCESS;
    except
        Result := ID3V1LIBRARY_ERROR_WRITING_FILE;
    end;
end;

function ID3GenreDataToString(GenreIndex: Byte): String;
begin
    if GenreIndex <= High(ID3Genres) then begin
        Result := ID3Genres[GenreIndex];
    end else if GenreIndex = 255 then begin
        Result := '';
    end else begin
        Result := IntToStr(GenreIndex);
    end;
end;

function ID3GenreDataToStringOther(GenreIndex: Byte): String;
begin
    if GenreIndex <= High(ID3Genres) then begin
        Result := ID3Genres[GenreIndex];
    end else begin
        Result := ID3Genres[12];
    end;
end;

function ID3GenreStringToData(Genre: String): Byte;
var
    i: Integer;
    GenreString: String;
    GenreIndex: Integer;
begin
    //* Number (unknown genre)
    GenreIndex := StrToIntDef(Genre, - 1);
    if GenreIndex > - 1 then begin
        Result := Byte(GenreIndex);
        Exit;
    end;
    //* Look-up genre string, 'Other' if not found
    Result := 12;
    GenreString := Genre;
    if GenreString = 'Psychadelic' then begin
        GenreString := 'Psychedelic';
    end;
    for i := 0 to Length(ID3Genres) - 1 do begin
        if GenreString = ID3Genres[i] then begin
            Result := i;
            Break;
        end;
    end;
end;

function RemoveID3v1TagFromFile(FileName: String; PreserveFileDateTime: Boolean = ID3V1LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME): Integer;
var
    FileDateTime: TDateTime;
    Action: Integer;
    AudioFile: TFileStream;
    DataByte: Byte;
    LyricsTagIDStart: TID3LyricsTagIDStart;
    LyricsTagIDEnd: TID3LyricsTagIDEnd;
    LyricsTagSize: TID3LyricsTagSize;
    TagSize: Integer;
begin
    Result := ID3V1LIBRARY_ERROR;
    if NOT FileExists(FileName) then begin
        Exit;
    end;
    Action := ID3v1TLOFEANNONE;
    try
        if PreserveFileDateTime then begin
            FileAge(FileName, FileDateTime);
        end;
        if FileIsReadOnly(FileName) then begin
            if Assigned(ID3v1LibraryOpenFileExceptionCallback) then begin
                ID3v1LibraryOpenFileExceptionCallback(FileName, nil, Action);
                case Action of
                    ID3v1TLOFEANNONE: begin
                        Result := ID3V1LIBRARY_ERROR_FILE_READ_ONLY;
                        Exit;
                    end;
                    ID3v1TLOFEANDEPROTECT, ID3v1TLOFEANDEPROTECTANDRESTORE: begin
                        FileSetReadOnly(FileName, False);
                    end;
                end;
            end else begin
                Result := ID3V1LIBRARY_ERROR_FILE_READ_ONLY;
                Exit;
            end;
        end;
        try
            AudioFile := TFileStream.Create(FileName, fmOpenReadWrite OR fmShareDenyWrite);
        except
            Result := ID3V1LIBRARY_ERROR_OPENING_FILE;
            Exit;
        end;
        if AudioFile.Size < ID3V1TAGSIZE then begin
            Exit;
        end;
        AudioFile.Seek(- ID3V1TAGSIZE, soEnd);
        AudioFile.Read2(DataByte, 1);
        if DataByte = Ord('T') then begin
            AudioFile.Read2(DataByte, 1);
            if DataByte = Ord('A') then begin
                AudioFile.Read2(DataByte, 1);
                if DataByte = Ord('G') then begin
                    AudioFile.Seek(- (ID3V1TAGSIZE + 9), soEnd);
                    AudioFile.Read2(LyricsTagIDEnd, SizeOf(TID3LyricsTagIDEnd));
                    if (LyricsTagIDEnd[1] = ID3LYRICSTAGIDEND[1])
                    AND (LyricsTagIDEnd[2] = ID3LYRICSTAGIDEND[2])
                    AND (LyricsTagIDEnd[3] = ID3LYRICSTAGIDEND[3])
                    AND (LyricsTagIDEnd[4] = ID3LYRICSTAGIDEND[4])
                    AND (LyricsTagIDEnd[5] = ID3LYRICSTAGIDEND[5])
                    AND (LyricsTagIDEnd[6] = ID3LYRICSTAGIDEND[6])
                    AND (LyricsTagIDEnd[7] = ID3LYRICSTAGIDEND[7])
                    AND (LyricsTagIDEnd[8] = ID3LYRICSTAGIDEND[8])
                    AND (LyricsTagIDEnd[9] = ID3LYRICSTAGIDEND[9])
                    then begin
                        AudioFile.Seek(- (ID3V1TAGSIZE + 9 + 6), soEnd);
                        AudioFile.Read2(LyricsTagSize, SizeOf(TID3LyricsTagSize));
                        TagSize := StrToInt(Char(LyricsTagSize[1])
                             + Char(LyricsTagSize[2])
                             + Char(LyricsTagSize[3])
                             + Char(LyricsTagSize[4])
                             + Char(LyricsTagSize[5])
                             + Char(LyricsTagSize[6])
                             );
                        AudioFile.Seek(- (ID3V1TAGSIZE + 9 + 6 + TagSize), soEnd);
                        AudioFile.Read2(LyricsTagIDStart, SizeOf(TID3LyricsTagIDStart));
                        if (LyricsTagIDStart[1] = ID3LYRICSTAGIDSTART[1])
                        AND (LyricsTagIDStart[2] = ID3LYRICSTAGIDSTART[2])
                        AND (LyricsTagIDStart[3] = ID3LYRICSTAGIDSTART[3])
                        AND (LyricsTagIDStart[4] = ID3LYRICSTAGIDSTART[4])
                        AND (LyricsTagIDStart[5] = ID3LYRICSTAGIDSTART[5])
                        AND (LyricsTagIDStart[6] = ID3LYRICSTAGIDSTART[6])
                        AND (LyricsTagIDStart[7] = ID3LYRICSTAGIDSTART[7])
                        AND (LyricsTagIDStart[8] = ID3LYRICSTAGIDSTART[8])
                        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
                        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
                        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
                        then begin
                            AudioFile.Seek(- (ID3V1TAGSIZE + 9 + 6 + TagSize), soEnd);
                            AudioFile.Size := AudioFile.Position;
                        end;
                    end else begin
                        AudioFile.Seek(- ID3V1TAGSIZE, soEnd);
                        AudioFile.Size := AudioFile.Position;
                    end;
                    Result := ID3V1LIBRARY_SUCCESS;
                end;
            end;
        end;
        if AudioFile <> nil then begin
            FreeAndNil(AudioFile);
        end;
        if PreserveFileDateTime then begin
            FileSetDate(FileName, DateTimeToFileDate(FileDateTime));
        end;
        if Action = ID3v1TLOFEANDEPROTECTANDRESTORE then begin
            FileSetReadOnly(FileName, True);
        end;
    except
        Result := ID3V1LIBRARY_ERROR;
    end;
end;

function RemoveID3v1TagFromStream(Stream: TStream): Integer;
var
    DataByte: Byte;
    LyricsTagIDStart: TID3LyricsTagIDStart;
    LyricsTagIDEnd: TID3LyricsTagIDEnd;
    LyricsTagSize: TID3LyricsTagSize;
    TagSize: Integer;
begin
    Result := ID3V1LIBRARY_ERROR;
    try
        if Stream.Size < ID3V1TAGSIZE then begin
            Exit;
        end;
        Stream.Seek(- ID3V1TAGSIZE, soEnd);
        Stream.Read2(DataByte, 1);
        if DataByte = Ord('T') then begin
            Stream.Read2(DataByte, 1);
            if DataByte = Ord('A') then begin
                Stream.Read2(DataByte, 1);
                if DataByte = Ord('G') then begin
                    Stream.Seek(- (ID3V1TAGSIZE + 9), soEnd);
                    Stream.Read2(LyricsTagIDEnd, SizeOf(TID3LyricsTagIDEnd));
                    if (LyricsTagIDEnd[1] = ID3LYRICSTAGIDEND[1])
                    AND (LyricsTagIDEnd[2] = ID3LYRICSTAGIDEND[2])
                    AND (LyricsTagIDEnd[3] = ID3LYRICSTAGIDEND[3])
                    AND (LyricsTagIDEnd[4] = ID3LYRICSTAGIDEND[4])
                    AND (LyricsTagIDEnd[5] = ID3LYRICSTAGIDEND[5])
                    AND (LyricsTagIDEnd[6] = ID3LYRICSTAGIDEND[6])
                    AND (LyricsTagIDEnd[7] = ID3LYRICSTAGIDEND[7])
                    AND (LyricsTagIDEnd[8] = ID3LYRICSTAGIDEND[8])
                    AND (LyricsTagIDEnd[9] = ID3LYRICSTAGIDEND[9])
                    then begin
                        Stream.Seek(- (ID3V1TAGSIZE + 9 + 6), soEnd);
                        Stream.Read2(LyricsTagSize, SizeOf(TID3LyricsTagSize));
                        TagSize := StrToInt(Char(LyricsTagSize[1])
                             + Char(LyricsTagSize[2])
                             + Char(LyricsTagSize[3])
                             + Char(LyricsTagSize[4])
                             + Char(LyricsTagSize[5])
                             + Char(LyricsTagSize[6])
                             );
                        Stream.Seek(- (ID3V1TAGSIZE + 9 + 6 + TagSize), soEnd);
                        Stream.Read2(LyricsTagIDStart, SizeOf(TID3LyricsTagIDStart));
                        if (LyricsTagIDStart[1] = ID3LYRICSTAGIDSTART[1])
                        AND (LyricsTagIDStart[2] = ID3LYRICSTAGIDSTART[2])
                        AND (LyricsTagIDStart[3] = ID3LYRICSTAGIDSTART[3])
                        AND (LyricsTagIDStart[4] = ID3LYRICSTAGIDSTART[4])
                        AND (LyricsTagIDStart[5] = ID3LYRICSTAGIDSTART[5])
                        AND (LyricsTagIDStart[6] = ID3LYRICSTAGIDSTART[6])
                        AND (LyricsTagIDStart[7] = ID3LYRICSTAGIDSTART[7])
                        AND (LyricsTagIDStart[8] = ID3LYRICSTAGIDSTART[8])
                        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
                        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
                        AND (LyricsTagIDStart[9] = ID3LYRICSTAGIDSTART[9])
                        then begin
                            Stream.Seek(- (ID3V1TAGSIZE + 9 + 6 + TagSize), soEnd);
                            Stream.Size := Stream.Position;
                        end;
                    end else begin
                        Stream.Seek(- ID3V1TAGSIZE, soEnd);
                        Stream.Size := Stream.Position;
                    end;
                    Result := ID3V1LIBRARY_SUCCESS;
                end;
            end;
        end;
    except
        Result := ID3V1LIBRARY_ERROR;
    end;
end;

function ID3v1TagErrorCode2String(ErrorCode: Integer): String;
begin
    Result := 'Unknown error code.';
    case ErrorCode of
        ID3V1LIBRARY_SUCCESS: Result := 'Success.';
        ID3V1LIBRARY_ERROR: Result := 'Unknown error occured.';
        ID3V1LIBRARY_ERROR_OPENING_FILE: Result := 'Error opening file.';
        ID3V1LIBRARY_ERROR_READING_FILE: Result := 'Error reading file.';
        ID3V1LIBRARY_ERROR_WRITING_FILE: Result := 'Error writing file.';
        ID3V1LIBRARY_ERROR_FILE_READ_ONLY: Result := 'Error writing file, file is read only.';
    end;
end;

{ TID3LyricsFrame }

function TID3LyricsFrame.Index: Integer;
begin
    Result := Parent.LyricsFrames.IndexOf(Self);
end;

Initialization

    ID3V1TAGID[0] := Ord('T');
    ID3V1TAGID[1] := Ord('A');
    ID3V1TAGID[2] := Ord('G');

    ID3LYRICSTAGIDSTART[1] := Ord('L');
    ID3LYRICSTAGIDSTART[2] := Ord('Y');
    ID3LYRICSTAGIDSTART[3] := Ord('R');
    ID3LYRICSTAGIDSTART[4] := Ord('I');
    ID3LYRICSTAGIDSTART[5] := Ord('C');
    ID3LYRICSTAGIDSTART[6] := Ord('S');
    ID3LYRICSTAGIDSTART[7] := Ord('B');
    ID3LYRICSTAGIDSTART[8] := Ord('E');
    ID3LYRICSTAGIDSTART[9] := Ord('G');
    ID3LYRICSTAGIDSTART[10] := Ord('I');
    ID3LYRICSTAGIDSTART[11] := Ord('N');


    ID3LYRICSTAGIDEND[1] := Ord('L');
    ID3LYRICSTAGIDEND[2] := Ord('Y');
    ID3LYRICSTAGIDEND[3] := Ord('R');
    ID3LYRICSTAGIDEND[4] := Ord('I');
    ID3LYRICSTAGIDEND[5] := Ord('C');
    ID3LYRICSTAGIDEND[6] := Ord('S');
    ID3LYRICSTAGIDEND[7] := Ord('2');
    ID3LYRICSTAGIDEND[8] := Ord('0');
    ID3LYRICSTAGIDEND[9] := Ord('0');

    INDFieldID[1] := Ord('I');
    INDFieldID[2] := Ord('N');
    INDFieldID[3] := Ord('D');
    INDFieldID[4] := Ord('0');
    INDFieldID[5] := Ord('0');
    INDFieldID[6] := Ord('0');
    INDFieldID[7] := Ord('0');
    INDFieldID[8] := Ord('3');

end.



