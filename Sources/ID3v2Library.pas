//********************************************************************************************************************************
//*                                                                                                                              *
//*     ID3v2 Library 2.1.94.214 © 3delite 2010-2025                                                                             *
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

{.$DEFINE STRANGE_TAG}

{$IFOPT R+}
    {$DEFINE RANGECHECKSAREON}
{$ENDIF}

{$MINENUMSIZE 4}

{$IFDEF MSWINDOWS}
    {.$DEFINE CHARSETDETECTOR} //* Only Win32, copy 'chsdet.dll' beside the .exe
{$ENDIF}

{$IFDEF FPC}
    {$MODE DELPHIUNICODE}{$H+}
    {$HINTS OFF}
    {$WARNINGS OFF}
{$ENDIF}

unit ID3v2Library;

interface

Uses
    SysUtils,
    Classes,
    Generics.Collections;

Const
    ID3V2LIBRARY_VERSION = $020194214;
    STR_ID3V2LIBRARY_VERSION = '2.1.94.214';
    STR_ID3V2LIBRARY_VERSION_NAME = 'ID3v2 Library ' + STR_ID3V2LIBRARY_VERSION;
    STR_ID3V2LIBRARY_HOMEPAGE = 'https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/ID3v2Library.html';        

Const
    ID3V2LIBRARY_DEFAULT_PARSE_AUDIO_ATTRIBUTES = True;
    ID3V2LIBRARY_DEFAULT_TRIM_TEXT = True;
    ID3V2LIBRARY_DEFAULT_BEGINNING_SEARCH_LENGTH = 0;
    ID3V2LIBRARY_DEFAULT_RETURN_ANSI_TEXT = False;
    ID3V2LIBRARY_DEFAULT_MPEG_SEARCH_LENGTH = 4096; //* Increasing this value helps for corrupted MPEG files, but increases false MPEG sync detection
    ID3V2LIBRARY_DEFAULT_AUTO_REMOVE_UNSYNCHRONISATION = True;
    ID3V2LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME = False;

Const
    ID3V2LIBRARY_FRAMES_GROWBY = 1024;

type
    DWord = Cardinal;
    PBytes = ^TBytes;

type
    TID3v2ID = Array [0..2] of Byte;
    TFrameID = Array [0..3] of Byte;
    TLanguageID = Array [0..2] of Byte;
    TRIFFID = Array [0..3] of Byte;
    TRIFFChunkID = Array [0..3] of Byte;
    TAIFFID = Array [0..3] of Byte;
    TAIFFChunkID = Array [0..3] of Byte;

Const
    ID3V2LIBRARY_SUCCESS                        = 0;
    ID3V2LIBRARY_ERROR                          = $FFFF;
    ID3V2LIBRARY_ERROR_NO_TAG_FOUND             = 1;
    ID3V2LIBRARY_ERROR_EMPTY_TAG                = 2;
    ID3V2LIBRARY_ERROR_EMPTY_FRAMES             = 3;
    ID3V2LIBRARY_ERROR_OPENING_FILE             = 4;
    ID3V2LIBRARY_ERROR_READING_FILE             = 5;
    ID3V2LIBRARY_ERROR_WRITING_FILE             = 6;
    ID3V2LIBRARY_ERROR_CORRUPT                  = 7;
    ID3V2LIBRARY_ERROR_NOT_SUPPORTED_VERSION    = 8;
    ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT     = 9;
    ID3V2LIBRARY_ERROR_DOESNT_FIT               = 10;
    ID3V2LIBRARY_ERROR_NEED_EXCLUSIVE_ACCESS    = 11;
    ID3V2LIBRARY_ERROR_FILE_READ_ONLY           = 12;

const
    ID3V2LIBRARY_DEFAULT_PADDING_SIZE    = 4096;

const
    ID3V2LIBRARY_SESC_ID    = $55555555;
    ID3V2LIBRARY_SESC_VERSION2: Byte = $02;

Const
    ID3v2TLOFEANNONE = 0;
    ID3v2TLOFEANDEPROTECT = 1;
    ID3v2TLOFEANDEPROTECTANDRESTORE = 2;

type
    TID3v2TLOpenFileExceptionCallback = procedure (FileName: String; AException: Exception; var Action: Integer);

type
    TExtendedBytes = Array [0..9] of Byte;

type
    TMPEGVersion = (tmpegvUnknown, tmpegv1, tmpegv2, tmpegv25);

    TMPEGLayer = (tmpeglUnknown, tmpegl1, tmpegl2, tmpegl3);

    TMPEGChannelMode = (tmpegcmUnknown, tmpegcmMono, tmpegcmDualChannel, tmpegcmJointStereo, tmpegcmStereo);

    TMPEGModeExtension = (tmpegmeUnknown, tmpegmeNone, tmpegmeIntensity, tmpegmeMS, tmpegmeIntensityMS);

    TMPEGEmphasis = (tmpegeUnknown, tmpegeNo, tmpege5015, tmpegeCCITJ17);

    TMPEGHeader = record
        Position: Int64;                    //* Position in bytes
        Header: Cardinal;                   //* The Headers bytes
        FrameSize: Integer;                 //* frames length
        Version: TMPEGVersion;              //* MPEG Version
        Layer: TMPEGLayer;                  //* MPEG Layer
        CRC: Boolean;                       //* Frame has CRC
        BitRate: DWord;                     //* frames bitrate
        SampleRate: DWord;                  //* frames sample rate
        Padding: Boolean;                   //* frame is padded
        _Private: Boolean;                  //* frames private bit is set
        ChannelMode: TMPEGChannelMode;      //* frames channel mode
        ModeExtension: TMPEGModeExtension;  //* Joint stereo only
        Copyrighted: Boolean;               //* frames Copyright bit is set
        Original: Boolean;                  //* frames Original bit is set
        Emphasis: TMPEGEmphasis;            //* frames emphasis mode
        VBR: Boolean;                       //* Stream is probably VBR
        FrameCount: Int64;                  //* Total number of MPEG frames (by header)
        Quality: DWord;                     //* MPEG quality
        Bytes: Int64;                       //* Total bytes
        HasInfo: Boolean;
        HasXing: Boolean;
        HasVBRI: Boolean;
        EncoderShortVersionString: String;  //* Decoded
        InfoTagRevision: Byte;              //* Decoded
        VBRMethod: Byte;                    //* Decoded
        LowpassFilterValue: Integer;        //* Decoded
        PeakSignalSmplitude: Single;
        RadioReplayGain: Word;
        AudiophileReplayGain: Word;
        EncodingFlags: Byte;                //* Decoded
        ATHType: Byte;                      //* Decoded
        ABROrMinimalBitrate: Byte;
        EncoderDelay: Integer;              //* Decoded
        EncoderPadding: Integer;            //* Decoded
        Misc: Byte;
        MP3Gain: Byte;
        PresetAndSurroundInfo: Word;
        MusicLength: Cardinal;
        MusicCRC: Word;
        CRC16OfInfoTag: Word;
    end;

type
    TWaveds64 = record
        ds64Size: DWORD;
        RIFFSizeLow: DWORD;
        RIFFSizeHigh: DWORD;
        DataSizeLow: DWORD;
        DataSizeHigh: DWORD;
        SampleCountLow: DWORD;
        SampleCountHigh: DWORD;
        TableLength: DWORD;
    end;

type
    TID3v2ExtendedHeader3 = class
        Size: Dword;
        CodedSize: Cardinal;
        Data: TMemoryStream;
        Flags: Word;
        CRCPresent: Boolean;
        Constructor Create;
        Destructor Destroy; override;
        procedure DecodeExtendedHeaderSize;
        procedure DecodeExtendedHeaderFlags;
    end;

type
    TID3v2ExtendedHeader4TagSizeRestriction = (
        NoMoreThan128FramesAnd1MBTotalTagSize,
        NoMoreThan64FramesAnd128KBTotalTagSize,
        NoMoreThan32FramesAnd40KBTotalTagSize,
        NoMoreThan32FramesAnd4KBTotalTagSize
    );

type
    TID3v2ExtendedHeader4TextEncodingRestriction = (
        NoTextEncodingRestrictions,
        OnlyEncodedWithISO88591OrUTF8
    );

type
    TID3v2ExtendedHeader4TextFieldsSizeRestriction = (
        NoTextFieldsSizeRestrictions,
        NoStringIsLongerThan1024Characters,
        NoStringIsLongerThan128Characters,
        NoStringIsLongerThan30Characters
    );

type
    TID3v2ExtendedHeader4ImageEncodingRestriction = (
        NoImageEncodingRestrictions,
        ImagesAreEncodedOnlyWithPNGOrJPEG
    );

type
    TID3v2ExtendedHeader4ImageSizeRestriction = (
        NoImageSizeRestrictions,
        AllImagesAre256x256PixelsOrSmaller,
        AllImagesAre64x64PixelsOrSmaller,
        AllImagesAreExactly64x64PixelsUnlessRequiredOtherwise
    );

type
    TID3v2ExtendedHeader4 = class
        Size: DWord;
        CodedSize: Cardinal;
        FlagBytes: Byte;
        Flags: Byte;
        ExtendedFlagsDataSize: Cardinal;
        ExtendedFlagsData: Array of Byte;
        TagIsAnUpdate: Boolean;
        CRCPresent: Boolean;
        TagRestrictions: Boolean;
        TagRestrictionsData: TID3v2ExtendedHeader4TagSizeRestriction;
        TextEncodingRestrictions: TID3v2ExtendedHeader4TextEncodingRestriction;
        TextFieldsSizeRestriction: TID3v2ExtendedHeader4TextFieldsSizeRestriction;
        ImageEncodingRestriction: TID3v2ExtendedHeader4ImageEncodingRestriction;
        ImageSizeRestriction: TID3v2ExtendedHeader4ImageSizeRestriction;
        Constructor Create;
        Destructor Destroy; override;
        procedure DecodeExtendedHeaderSize;
        procedure DecodeExtendedHeaderFlags;
        procedure CalculateExtendedFlagsDataSize;
        procedure DecodeExtendedHeaderFlagData;
    end;

type
    TID3v2SampleCache = Array of Byte;

type
    TID3v2FrameError = (id3v2feInvalidFrameID, id3v2feInvalidTextEncoding, id3v2feInvalidFlags, id3v2feNoContent, id3v2feContentToLarge, id3v2feSizeSpecifiedToLarge);
    TID3v2FrameErrors = set of TID3v2FrameError;

type
    TDSFChannelType = (dsfctUnknown, dsfctMono, dsfctStereo, dsfct3Channels, dsfctQuad, dsfct4Channels, dsfct5Channels, dsfct51Channels);

type
    TDSFInfo = class
    private
        function GetBitRate: Integer;
    public
        FormatVersion: Cardinal;
        FormatID: Cardinal;
        ChannelType: TDSFChannelType;
        ChannelNumber: Cardinal;
        SamplingFrequency: Cardinal;
        BitsPerSample: Cardinal;
        SampleCount: UInt64;
        BlockSizePerChannel: Cardinal;
        PlayTime: Double;
        procedure Clear;
        procedure Assign(DSFInfo: TDSFInfo);
        property BitRate: Integer read GetBitRate;
    end;

type
    TDFFInfo = class
    public
        FormatVersion: String;
        SampleRate: Cardinal;
        ChannelNumber: Word;
        CompressionName: String;
        SampleCount: UInt64;
        PlayTime: Double;
        BitRate: Cardinal;
        SoundDataLength: UInt64;
        DSTFramesCount: Cardinal;
        DSTFramesRate: Word;
        Ratio: Double;
        procedure ResetData;
        constructor Create;
        destructor Destroy; override;
        function LoadFromFile(const FileName: string): Boolean;
        function LoadFromStream(Stream: TStream): Boolean;
	end;

type
    TWaveHeader = record
        ident1: TRIFFID;                    // Must be "RIFF"
        len: DWORD;                         // Remaining length after this header
    end;

type
    TWaveFmt = record
        //ident2: TWAVIdent;                // Must be "WAVE"
        //ident3: TWAVIdent;                // Must be "fmt "
        fmtSize: DWord;                     // Reserved 4 bytes
        FormatTag: Word;                    // format type
        Channels: Word;                     // number of channels (i.e. mono, stereo, etc.)
        SamplesPerSec: DWord;               // sample rate
        AvgBytesPerSec: DWord;              // for buffer estimation
        BlockAlign: Word;                   // block size of data
        BitsPerSample: Word;                // number of bits per sample of mono data
        //* WAVE_FORMAT_EXTENSIBLE
        cbSize: Word;	                    // Size of the extension: 22
        ValidBitsPerSample: Word;	        // at most 8 *  M
        ChannelMask: DWord;	                // Speaker position mask: 0
        SubFormat: Array[0..15] of Byte;    // 16
    end;

type
    TAIFFInformation = record
        Channels: Word;
        SampleFrames: DWord;
        SampleSize: Word;
        SampleRate: Double;
        CompressionID: String;  // http://en.wikipedia.org/wiki/Audio_Interchange_File_Format
        Compression: String;
    end;

type
    TSourceFileType = (sftUnknown, sftMPEG, sftWAVE, sftRF64, sftAIFF, sftDSF, sftDFF);

type
    TID3v2TextEncoding = (teUnicode, teUTF8, teANSI);

type
    TID3v2SaveVersion = (sfid3v2NoChange, sfid3v23, sfid3v24);

type
    TID3v2Tag = class;

    TID3v2Frame = class;

    TID3v2Frames = class
    private
        function GetFrameCount: Int64;
    public
        Root: TID3v2Tag;
        Frames: TList<TID3v2Frame>;
        Constructor Create;
        Destructor Destroy; override;
        procedure Clear;
        procedure DeleteAllFrames;
        function LoadFrame(Stream: TStream): Boolean;
        procedure LoadFrameData(Stream: TStream; FrameIndex: Integer);
        function AddFrame(FrameID: TFrameID): Integer; overload;
        function AddFrame(const FrameID: String): Integer; overload;
        function AddFrameEx(const FrameID: String): TID3v2Frame; overload;
        function Convertv2Tov3(FrameIndex: Integer): Boolean;
        function Convertv2PICtoAPIC(FrameIndex: Integer): Boolean;
        function WriteAllFrames(Stream: TStream): Integer;
        function FrameExists(FrameID: TFrameID): Integer; overload;
        function FrameExists(const FrameID: String): Integer; overload;
        function FrameTypeCount(FrameID: TFrameID): Integer; overload;
        function FrameTypeCount(const FrameID: String): Integer; overload;
        function CoverArtCount: Integer;
        function FullFrameSize(FrameIndex: Cardinal): Cardinal;
        function DeleteFrame(FrameIndex: Integer): Boolean; overload;
        function DeleteFrame(FrameID: TFrameID): Boolean; overload;
        function DeleteFrame(const FrameID: String): Boolean; overload;
        procedure SetAllFrameDataStreams;
        procedure DeleteAllCoverArts;
        procedure AssignFrames(Source: TID3v2Frames);
        function GetText(FrameIndex: Integer; ReturnNativeText: Boolean = False): String; overload;
        function GetText(const FrameID: String; ReturnNativeText: Boolean = False): String; overload;
        function SetText(FrameIndex: Integer; const Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetText(const FrameID: String; const Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function GetTextMultiple(FrameIndex: Integer; List: TStrings): Boolean; overload;
        function GetTextMultiple(FrameID: String; List: TStrings): Boolean; overload;
        function SetTextMultiple(FrameIndex: Integer; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetTextMultiple(FrameID: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetRawText(FrameIndex: Integer; const Text: String): Boolean; overload;
        function SetRawText(const FrameID: String; const Text: String): Boolean; overload;
        function GetContent(FrameIndex: Integer; var LanguageID: TLanguageID; var Description: String): String; overload;
        function GetContent(FrameID: String; var LanguageID: TLanguageID; var Description: String): String; overload;
        function SetContent(FrameIndex: Integer; Content: String; LanguageID: TLanguageID; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetContent(FrameID: String; Content: String; LanguageID: TLanguageID; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function GetComment(FrameIndex: Integer; var LanguageID: TLanguageID; var Description: String): String; overload;
        function GetComment(FrameID: String; var LanguageID: TLanguageID; var Description: String): String; overload;
        function FindCommentByDescription(Description: String; var LanguageID: TLanguageID; var Comment: String): Integer;
        function SetComment(FrameIndex: Integer; Comment: String; LanguageID: TLanguageID; Description: String): Boolean; overload;
        function SetComment(FrameID: String; Comment: String; LanguageID: TLanguageID; Description: String): Boolean; overload;
        function SetCommentByDescription(Description: String; LanguageID: TLanguageID; Comment: String): Boolean;
        function GetLyrics(FrameIndex: Integer; var LanguageID: TLanguageID; var Description: String): String; overload;
        function GetLyrics(FrameID: String; var LanguageID: TLanguageID; var Description: String): String; overload;
        function SetLyrics(FrameIndex: Integer; Lyrics: String; LanguageID: TLanguageID; Description: String): Boolean; overload;
        function SetLyrics(FrameID: String; Lyrics: String; LanguageID: TLanguageID; Description: String): Boolean; overload;
        function GetCoverPictureStream(FrameIndex: Integer; PictureStream: TStream; var MIMEType: String; var Description: String; var CoverType: Integer): Boolean; overload;
        function GetCoverPictureStream(FrameID: String; PictureStream: TStream; var MIMEType: String; var Description: String; var CoverType: Integer): Boolean; overload;
        function GetCoverPictureInfo(FrameIndex: Integer; var MIMEType: String; var Description: String; var CoverType: Integer; Bytes: PBytes = nil): Boolean; overload;
        function GetCoverPictureInfo(FrameID: String; var MIMEType: String; var Description: String; var CoverType: Integer; Bytes: PBytes = nil): Boolean; overload;
        function GetCoverPictureInfoPointer(FrameIndex: Integer; var Data: Pointer; var DataSize: Int64; var MIMEType: Pointer; var Description: Pointer; var Encoding: Integer; var CoverType: Integer): Boolean;
        function SetCoverPictureFromStream(FrameIndex: Integer; Description: String; PictureStream: TStream; MIMEType: String; CoverType: Integer): Boolean; overload;
        function SetCoverPictureFromStream(FrameID: String; Description: String; PictureStream: TStream; MIMEType: String; CoverType: Integer): Boolean; overload;
        function SetCoverPictureFromFile(FrameIndex: Integer; Description: String; PictureFileName: String; MIMEType: String; CoverType: Integer): Boolean; overload;
        function SetCoverPictureFromFile(FrameID: String; Description: String; PictureFileName: String; MIMEType: String; CoverType: Integer): Boolean; overload;
        function GetFrameIndexOfCoverArtIndex(CoverArtIndex: Integer): Integer;
        function GetURL(FrameIndex: Integer): String; overload;
        function GetURL(FrameID: String): String; overload;
        function SetURL(FrameIndex: Integer; URL: String): Boolean; overload;
        function SetURL(FrameID: String; URL: String): Boolean; overload;
        function GetUserDefinedURLLink(FrameIndex: Integer; var Description: String): String; overload;
        function GetUserDefinedURLLink(FrameID: String; var Description: String): String; overload;
        function FindUserDefinedURLLinkByDescription(Description: String; var URL: String): Integer;
        function SetUserDefinedURLLink(FrameIndex: Integer; URL: String; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetUserDefinedURLLink(FrameID: String; URL: String; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetUserDefinedURLLinkByDescription(Description: String; URL: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
        function GetTime(FrameIndex: Integer): TDateTime; overload;
        function GetTime(FrameID: String): TDateTime; overload;
        function SetTime(FrameIndex: Integer; DateTime: TDateTime): Boolean; overload;
        function SetTime(FrameID: String; DateTime: TDateTime): Boolean; overload;
        function GetSEBR(FrameIndex: Integer): {$IFDEF WIN64}Double{$ELSE}Extended{$ENDIF}; overload;
        function GetSEBR(FrameID: String): {$IFDEF WIN64}Double{$ELSE}Extended{$ENDIF}; overload;
        function GetSEBRString(FrameIndex: Integer): String;
        function SetSEBR(FrameIndex: Integer; BitRateStr: String): Boolean; overload;
        function SetSEBR(FrameID: String; BitRateStr: String): Boolean; overload;
        {$IFNDEF WIN64}
        function SetSEBR(FrameIndex: Integer; BitRateValue: Extended): Boolean; overload;
        function SetSEBR(FrameID: String; BitRateValue: Extended): Boolean; overload;
        {$ENDIF}
        function GetSampleCache(FrameIndex: Integer; ForceDecompression: Boolean; var Version: Byte; var Channels: Integer): TID3v2SampleCache;
        function SetSampleCache(FrameIndex: Integer; SESC: TID3v2SampleCache; Channels: Integer): Boolean;
        function GetSEFC(FrameIndex: Integer): Int64;
        function SetSEFC(FrameIndex: Integer; SEFC: Int64): Boolean;
        function SetAlbumColors(FrameIndex: Integer; TitleColor, TextColor: Cardinal): Boolean; overload;
        function SetAlbumColors(FrameID: String; TitleColor, TextColor: Cardinal): Boolean; overload;
        function GetAlbumColors(FrameIndex: Integer; var TitleColor, TextColor: Cardinal): Boolean; overload;
        function GetAlbumColors(FrameID: String; var TitleColor, TextColor: Cardinal): Boolean; overload;
        function SetTLEN(FrameIndex: Integer; TLEN: Integer): Boolean; overload;
        function SetTLEN(FrameID: String; TLEN: Integer): Boolean; overload;
        function GetPlayCount(FrameIndex: Integer): Cardinal; overload;
        function GetPlayCount(FrameID: String): Cardinal; overload;
        function SetPlayCount(FrameIndex: Integer; PlayCount: Cardinal): Boolean; overload;
        function SetPlayCount(FrameID: String; PlayCount: Cardinal): Boolean; overload;
        function FindCustomFrame(FrameID: String; Description: String): Integer;
        function GetUserDefinedTextInformation(FrameIndex: Integer; var Description: String): String; overload;
        function GetUserDefinedTextInformation(const Description: String; CaseSensitive: Boolean = False): String; overload;
        function SetUserDefinedTextInformation(FrameIndex: Integer; Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetUserDefinedTextInformation(FrameID: String; Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetUserDefinedTextInformationMultiple(FrameIndex: Integer; Description: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetUserDefinedTextInformationMultiple(FrameID: String; Description: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function GetUserDefinedTextInformationMultiple(FrameIndex: Integer; var Description: String; List: TStrings): Boolean; overload;
        function GetUserDefinedTextInformationMultiple(FrameID: String; var Description: String; List: TStrings): Boolean; overload;
        function GetPopularimeter(FrameIndex: Integer; var Email: String; var Rating: Byte; var PlayCounter: Cardinal): Boolean;
        function FindPopularimeter(Email: String; var Rating: Byte; var PlayCounter: Cardinal): Integer;
        function SetPopularimeterByEmail(Email: String; Rating: Byte; PlayCounter: Cardinal = 0): Boolean;
        function SetPopularimeter(FrameIndex: Integer; Email: String; Rating: Byte; PlayCounter: Cardinal): Boolean;
        function FindTXXXByDescription(Description: String; var Text: String): Integer; overload;
        function FindTXXXByDescriptionMultiple(Description: String; List: TStrings): Integer; overload;
        function SetTXXXByDescription(Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
        function SetTXXXByDescriptionMultiple(Description: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
        function SetTXXX(Index: Integer; Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
        function GetListFrame(FrameID: String; List: TStrings): Boolean; overload;
        function GetListFrame(FrameIndex: Integer; List: TStrings): Boolean; overload;
        function SetListFrame(FrameID: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function SetListFrame(Index: Integer; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean; overload;
        function GetListFrameText(FrameIndex: Integer): String;
        function GetUFID(FrameIndex: Integer; var OwnerIdentifier: String): String; overload;
        function GetUFID(FrameID: String; var OwnerIdentifier: String): String; overload;
        function GetUFIDBytes(FrameIndex: Integer; var OwnerIdentifier: String): TBytes;
        function FindUFIDByOwnerIdentifier(OwnerIdentifier: String; var Identifier: String): Integer;
        function SetUFID(FrameIndex: Integer; OwnerIdentifier: String; Identifier: String): Boolean; overload;
        function SetUFID(FrameID: String; OwnerIdentifier: String; Identifier: String): Boolean; overload;
        function SetUFIDByOwnerIdentifier(OwnerIdentifier: String; Identifier: String): Boolean;
        function GetBytes(FrameIndex: Integer): TBytes;
        function GetAENC(FrameIndex: Integer; var OwnerIdentifier: String; var PreviewStart: Word; var PreviewLength: Word): TBytes; overload;
        function GetAENC(FrameID: String; var OwnerIdentifier: String; var PreviewStart: Word; var PreviewLength: Word): TBytes; overload;
        function SetAENC(FrameIndex: Integer; OwnerIdentifier: String; PreviewStart: Word; PreviewLength: Word; EncryptionInfo: TBytes): Boolean; overload;
        function SetAENC(FrameID: String; OwnerIdentifier: String; PreviewStart: Word; PreviewLength: Word; EncryptionInfo: TBytes): Boolean; overload;
        function GetGEOB(FrameIndex: Integer; var MIMEType: String; var EncapsulatedObjectFilename: String; var ContentDescription: String): TBytes; overload;
        function GetGEOB(FrameID: String; var MIMEType: String; var EncapsulatedObjectFilename: String; var ContentDescription: String): TBytes; overload;
        function SetGEOB(FrameIndex: Integer; MIMEType: String; EncapsulatedObjectFilename: String; ContentDescription: String; EncapsulatedObject: TBytes): Boolean; overload;
        function SetGEOB(FrameID: String; MIMEType: String; EncapsulatedObjectFilename: String; ContentDescription: String; EncapsulatedObject: TBytes): Boolean; overload;
        function GetOWNE(FrameIndex: Integer; var PricePaid: String; var DateOfPurchase: String; var Seller: String): Boolean; overload;
        function GetOWNE(FrameID: String; var PricePaid: String; var DateOfPurchase: String; var Seller: String): Boolean; overload;
        function SetOWNE(FrameIndex: Integer; PricePaid: String; DateOfPurchase: String; Seller: String): Boolean; overload;
        function SetOWNE(FrameID: String; PricePaid: String; DateOfPurchase: String; Seller: String): Boolean; overload;
        function GetOWNE(FrameIndex: Integer; var PricePaid: String; var DateOfPurchase: TDate; var Seller: String): Boolean; overload;
        function GetOWNE(FrameID: String; var PricePaid: String; var DateOfPurchase: TDate; var Seller: String): Boolean; overload;
        function SetOWNE(FrameIndex: Integer; PricePaid: String; DateOfPurchase: TDate; Seller: String): Boolean; overload;
        function SetOWNE(FrameID: String; PricePaid: String; DateOfPurchase: TDate; Seller: String): Boolean; overload;
        function GetUSER(FrameIndex: Integer; var LanguageID: TLanguageID): String; overload;
        function GetUSER(FrameID: String; var LanguageID: TLanguageID): String; overload;
        function SetUSER(FrameIndex: Integer; LanguageID: TLanguageID; TermsOfUse: String): Boolean; overload;
        function SetUSER(FrameID: String; LanguageID: TLanguageID; TermsOfUse: String): Boolean; overload;
        function GetiXML(FrameIndex: Integer): String; overload;
        function GetiXML(FrameID: String): String; overload;
        function SetiXML(FrameIndex: Integer; XMLText: String): Boolean; overload;
        function SetiXML(FrameID: String; XMLText: String): Boolean; overload;
        function GetXMP(FrameIndex: Integer): String; overload;
        function GetXMP(FrameID: String): String; overload;
        function SetXMP(FrameIndex: Integer; XMPText: String): Boolean; overload;
        function SetXMP(FrameID: String; XMPText: String): Boolean; overload;
        function FindChapterElementID(ChapterElementID: String): Integer; overload;
        function CheckValidFrame(FrameIndex: Integer): TID3v2FrameErrors;
        property FrameCount: Int64 read GetFrameCount;
    end;

    TID3v2Frame = class
    private
        FWasUnsynchronised: Boolean;
        function GetIndex: Integer;
    public
        ID: TFrameID;
        Parent: TID3v2Frames;
        Size: Cardinal;
        Stream: TMemoryStream;
        Flags: Word;
        TagAlterPreservation: Boolean;
        FileAlterPreservation: Boolean;
        ReadOnly: Boolean;
        Compressed: Boolean;
        Encrypted: Boolean;
        GroupingIdentity: Boolean;
        Unsynchronised: Boolean;
        DataLengthIndicator: Boolean;
        GroupIdentifier: Byte;
        EncryptionMethod: Byte;
        DataLengthIndicatorValue: Cardinal;
        AlreadyParsed: Boolean;
        Constructor Create(_Parent: TID3v2Frames);
        Destructor Destroy; override;
        procedure DecodeFlags3;
        procedure EncodeFlags3;
        procedure DecodeFlags4;
        procedure EncodeFlags4;
        function Compress: Boolean;
        function DeCompress: Boolean;
        function RemoveUnsynchronisation: Boolean;
        function ApplyUnsynchronisation: Boolean;
        function Assign(ID3v2Frame: TID3v2Frame): Boolean;
        procedure Delete;
        procedure Clear;
        function IsiXML: Boolean;
        function IsXMP: Boolean;
        property WasUnsynchronised: Boolean read FWasUnsynchronised;
        property Index: Integer read GetIndex;
    end;

    TID3v2FrameWithSubFrames = class (TID3v2Frame)
        SubFrames: TID3v2Frames;
        Constructor Create(_Parent: TID3v2Frames);
        Destructor Destroy; override;
    end;

    TID3v2FrameCHAP = class (TID3v2FrameWithSubFrames)
        ElementID: String;
        StartTime: Cardinal;
        EndTime: Cardinal;
        StartOffset: Cardinal;
        EndOffset: Cardinal;
        procedure Parse;
        procedure SetData;
    end;

    TID3v2FrameCTOC = class (TID3v2FrameWithSubFrames)
        ElementID: String;
        CTOCFlags: Byte;
        ChildElementIDs: Array of String;
        procedure Parse;
        procedure SetData;
        procedure SetFlagBits(TopLevelBit: Boolean; OrderedBit: Boolean);
        function TopLevelBit: Boolean;
        function OrderedBit: Boolean;
        function DeleteChapter(ElementID: String): Boolean;
        function AddChapter(ElementID: String): Boolean;
    end;

    TID3v2Tag = class (TID3v2Frames)
    private
        CodedSize: Cardinal;
        FPosition: Int64;
        FSourceFileType: TSourceFileType;
        FPlayTime: Double;
        {$IFDEF STRANGE_TAG}
        FStrangeTag: Boolean;
        {$ENDIF}
        FFileSize: Int64;
        //FError: Boolean;
        FWasUnsynchronised: Boolean;
        FSampleCount: Int64;
        FBitRate: Integer;
        procedure DecodeFlags;
        procedure EncodeFlags;
        procedure DecodeSize;
        procedure EncodeSize;
        function ReadExtendedHeader(Stream: TStream): Boolean;
        //function WriteExtendedHeader(TagStream: TStream): Boolean;
        //function RemoveUnsynchronisationOnExtendedHeaderSize: Boolean;
        //function ApplyUnsynchronisationOnExtendedHeaderSize: Boolean;
        function RemoveUnsynchronisationOnExtendedHeaderData: Boolean;
        //function ApplyUnsynchronisationOnExtendedHeaderData: Boolean;
        function WriteAllHeaders(Stream: TStream): Integer;
        function SaveDSF(Stream: TStream; WriteTagTotalSize: Cardinal): Integer;
        function SaveDFF(Stream: TStream; WriteTagTotalSize: Cardinal): Integer;
        function LoadDSFInfo(Stream: TStream): Boolean;
        function CheckMPEG(MPEGStream: TStream): Boolean;
        function MPEGValidHeader(MPEGStream: TStream): Integer;
        //function MPEGProcessHeader(MPEGStream: TStream): TMPEGHeader;
        function GetMPEGHeaderInformation(Stream: TStream): Boolean;
        procedure LoadWAVEAttributes(Stream: TStream);
        function GetWAVEInformation(Stream: TStream): TWaveFmt;
        function GetAIFFInformation(Stream: TStream): TAIFFInformation;
        function GetPlayTime: Double;
        function GetSampleCount: Int64;
    public
        SourceFileName: String;
        Loaded: Boolean;
        Flags: Byte;
        Unsynchronised: Boolean;
        Compressed: Boolean;
        ExtendedHeader: Boolean;
        Experimental: Boolean;
        FooterPresent: Boolean;
        ExtendedHeader3: TID3v2ExtendedHeader3;
        ExtendedHeader4: TID3v2ExtendedHeader4;
        PaddingSize: Cardinal;
        PaddingToWrite: Cardinal;
        MPEGInfo: TMPEGHeader;
        WAVInfo: TWaveFmt;
        AIFFInfo: TAIFFInformation;
        DSFInfo: TDSFInfo;
        DFFInfo: TDFFInfo;
        ParseAudioAttributes: Boolean;
        BeginningSearchLength: Cardinal;
        MPEGSearchLength: Integer;
        AutoRemoveUnsynchronisation: Boolean;
        UseTLENForPlaytime: Boolean;
        APEv2TagSize: Integer;
        MajorVersion: Byte;
        MinorVersion: Byte;
        FError: Boolean;
        Size: Cardinal;
        ParseCoverArts: Boolean;
        TrimText: Boolean;
        APICTextEncodingToWrite: TID3v2TextEncoding;
        PreserveFileDateTime: Boolean;
        WriteVersion: TID3v2SaveVersion;
        Constructor Create;
        Destructor Destroy; override;
        function LoadFromFile(FileName: String; Buffered: Boolean = True): Integer;
        function LoadFromStream(Stream: TStream; Buffered: Boolean = True): Integer;
        function LoadFromMemory(MemoryAddress: Pointer): Integer;
        function SaveToFile(FileName: String; Buffered: Boolean = True): Integer;
        function SaveToStream(Stream: TStream; Buffered: Boolean = True): Integer;
        function LoadTagFromStream(Stream: TStream): Integer;
        function SaveTagToStream(Stream: TStream; PaddingSizeToWrite: Integer = 0): Integer;
        procedure Clear;
        procedure Assign(ID3v2Tag: TID3v2Tag);
        function RemoveUnsynchronisationOnAllFrames: Boolean;
        function ApplyUnsynchronisationOnAllFrames: Boolean;
        function DeCompressAllFrames: Boolean;
        function CalculateTotalFramesSize: Integer;
        function CalculateTagSize(_PaddingSize: Cardinal): Cardinal;
        function CalculateTagCRC32: Cardinal;
        function GetFramePlayTime: Double;
        procedure ReSetAlreadyParsedState;
        function RewriteCoverArts: Boolean;
        property Position: Int64 read FPosition;
        property SourceFileType: TSourceFileType read FSourceFileType;
        property PlayTime: Double read GetPlayTime;
        property FileSize: Int64 read FFileSize;
        property Error: Boolean read FError;
        property WasUnsynchronised: Boolean read FWasUnsynchronised;
        property SampleCount: Int64 read GetSampleCount;
        property BitRate: Integer read FBitRate;
    end;

type
    TID3v2FrameType = (ftUnknown, ftText, ftTextWithDescription, ftTextWithDescriptionAndLangugageID, ftTextList, ftURL, ftUserDefinedURL, ftPlayCount, ftBinary, ftCTOC, ftCHAP, ftPRIV);

// The constants here are for the CRC-32 generator
// polynomial, as defined in the Microsoft
// Systems Journal, March 1995, pp. 107-108
Const
    CRC32Table: array[0..255] of DWORD =
    ($00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

    procedure UnSyncSafe(var Source; const SourceSize: Integer; var Dest: Cardinal);
    procedure SyncSafe(Source: Cardinal; var Dest; const DestSize: Integer);

    function ReverseBytes32(Value: Cardinal): Cardinal; overload;
    function ReverseBytes16(AWord: Word): Word; register;

    function RemoveUnsynchronisationScheme(Source, Dest: TStream; BytesToRead: Integer): Boolean;
    function ApplyUnsynchronisationScheme(Source, Dest: TStream; BytesToRead: Integer): Boolean;

    function RemoveUnsynchronisationOnStream(Stream: TMemoryStream): Boolean;
    function ApplyUnsynchronisationOnStream(Stream: TMemoryStream): Boolean;

    function ID3v2DecodeDateOnly(const Str: String; out Error: Boolean): TDate; overload;
    function ID3v2DecodeDateOnly(const Str: String): TDate; overload;

    function ID3v2DecodeDayMonth(const Str: String): String;

    function ID3v2DecodeGenre(const Str: String): String;

    function ID3v2DecodeList(const Str: String): String;

    function ID3v2EncodeTime(DateTime: TDateTime): String;
    function ID3v2DecodeTime(ID3v2DateTime: String; out Parts: Integer): TDateTime; overload;
    function ID3v2DecodeTime(ID3v2DateTime: String): TDateTime; overload;
    function ID3v2DecodeTimeToNumbers(ID3v2DateTime: String; var Year, Month, Day, Hour, Minute, Second: Integer): Boolean;

    function ID3v2DecodeTimeOnly(const Str: String; out Error: Boolean): TTime; overload;
    function ID3v2DecodeTimeOnly(const Str: String): TTime; overload;

    function ValidID3v2FrameID(FrameID: TFrameID): Boolean;
    function ValidID3v2FrameID2(FrameID: TFrameID): Boolean;
    function LanguageIDtoString(LanguageId : TLanguageID): String;
    procedure AnsiStringToPAnsiChar(const Source: String; out Dest: TFrameID);
    procedure StringToLanguageID(const Source: String; out Dest: TLanguageID);

    procedure ClearFrameID(out Dest: TFrameID); //inline;
    procedure ClearLanguageID(out Dest: TLanguageID); //inline;

    function APICType2Str(PictureType: Integer): String;
    function APICTypeStr2No(PictureType: String): Integer;

    function ID3v2ValidTag(Stream: TStream): Boolean;
    function CheckRIFF(Stream: TStream): Boolean;
    function SeekRIFF(Stream: TStream): Integer;
    function CheckAIFF(Stream: TStream): Boolean;
    function SeekAIFF(Stream: TStream): Integer;
    function CheckRF64(Stream: TStream): Boolean;
    function SeekRF64(Stream: TStream): Integer;
    function CheckDSF(Stream: TStream): Boolean;
    function SeekDSF(Stream: TStream): Integer;
    function CheckDFF(Stream: TStream): Boolean;
    function SeekDFF(Stream: TStream): Integer;
    function ValidRIFF(Stream: TStream): Boolean;
    function ValidRF64(Stream: TStream): Boolean;
    function ValidDSF(Stream: TStream): Boolean;
    function ValidDFF(Stream: TStream): Boolean;

    function RemoveID3v2TagFromFile(FileName: String; PreserveFileDateTime: Boolean = ID3V2LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME): Integer;
    function RemoveID3v2TagFromStream(Stream: TStream): Integer;

    procedure CalcCRC32(P: Pointer; ByteCount: DWORD; var CRCValue: DWORD);
    function CalculateStreamCRC32(Stream: TStream; var CRCvalue: DWORD): Boolean;

    function RIFFCreateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PaddingToWrite: Cardinal): Integer;
    function RIFFUpdateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PreviousTagSize: Cardinal; PaddingToWrite: Cardinal): Integer;

    function AIFFCreateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PaddingToWrite: Cardinal): Integer;
    function AIFFUpdateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PreviousTagSize: Cardinal; PaddingToWrite: Cardinal): Integer;

    function RF64CreateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PaddingToWrite: Cardinal): Integer;
    function RF64UpdateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PreviousTagSize: Cardinal; PaddingToWrite: Cardinal): Integer;

    function WritePadding(Stream: TStream; PaddingSize: Integer): Integer;

    function RemoveRIFFID3v2FromFile(FileName: String): Integer;
    function RemoveRIFFID3v2FromStream(Stream: TStream): Integer;
    function RemoveAIFFID3v2FromFile(FileName: String): Integer;
    function RemoveAIFFID3v2FromStream(Stream: TStream): Integer;
    function RemoveRF64ID3v2FromFile(FileName: String): Integer;
    function RemoveRF64ID3v2FromStream(Stream: TStream): Integer;
    function RemoveDSFID3v2FromFile(FileName: String): Integer;
    function RemoveDSFID3v2FromStream(Stream: TStream): Integer;
    function RemoveDFFID3v2FromFile(FileName: String): Integer;
    function RemoveDFFID3v2FromStream(Stream: TStream): Integer;
    function ID3v2TagErrorCode2String(ErrorCode: Integer): String;

    function MakeInt64(LowDWord, HiDWord: DWord): Int64;
    function LowDWordOfInt64(Value: Int64): Cardinal;
    function HighDWordOfInt64(Value: Int64): Cardinal;

    function GetID3v2FrameType(FrameID: TFrameID): TID3v2FrameType;

    procedure ConvertString2FrameID(StringFrameID: String; out FrameID: TFrameID);
    function ConvertFrameID2String(FrameID: TFrameID): String;
    function IsSameFrameID(FrameID1: TFrameID; FrameID2: TFrameID): Boolean; overload;
    function IsSameFrameID(FrameID: TFrameID; StringFrameID: String): Boolean; overload;

    function GetID3v2Size(PMemory: Pointer): Cardinal; overload;
    function GetID3v2Size(Stream: TStream): Cardinal; overload;

    function ID3v2SearchTag(Stream: TStream; SearchLength: Cardinal): Boolean;

    function GetAPEv2TagSize(CheckStream: TStream): Integer;

    function MPEGProcessHeader(MPEGStream: TStream): TMPEGHeader;

var
    ID3v2ID: TID3v2ID;
    RIFFID: TRIFFID;
    RF64ID: TRIFFID;
    RIFFWAVEID: TRIFFChunkID;
    RIFFID3v2ID: TRIFFChunkID;
    RIFFID3v2ID2: TRIFFChunkID;
    AIFFID: TAIFFID;
    AIFFChunkID: TAIFFChunkID;
    AIFCChunkID: TAIFFChunkID;
    AIFFID3v2ID: TAIFFChunkID;
    DSFID: TRIFFID;
    DSFfmt_ID: TRIFFID;
    DFFID: TRIFFID;
    DFFType: TRIFFID;
    DFFID3v2ID: TAIFFChunkID;

    ID3v2LibraryDefaultParseAudioAttributes: Boolean = ID3V2LIBRARY_DEFAULT_PARSE_AUDIO_ATTRIBUTES;
    ID3v2LibraryDefaultTrimText: Boolean = ID3V2LIBRARY_DEFAULT_TRIM_TEXT;
    ID3v2LibraryDefaultBeginningSearchLength: Cardinal = ID3V2LIBRARY_DEFAULT_BEGINNING_SEARCH_LENGTH;
    ID3v2LibraryDefaultReturnANSIText: Boolean = ID3V2LIBRARY_DEFAULT_RETURN_ANSI_TEXT;
    ID3v2LibraryDefaultMPEGSearchLength: Integer = ID3V2LIBRARY_DEFAULT_MPEG_SEARCH_LENGTH;
    ID3v2LibraryDefaultAutoRemoveUnsynchronisation: Boolean = ID3V2LIBRARY_DEFAULT_AUTO_REMOVE_UNSYNCHRONISATION;
    ID3v2LibraryDefaultAPICTextEncodingToWrite: TID3v2TextEncoding;
    ID3v2LibraryDefaultPreserveFileDateTime: Boolean = ID3V2LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME;
    ID3v2LibraryDefaultWriteVersion: TID3v2SaveVersion;

    ID3v2LibraryOpenFileExceptionCallback: TID3v2TLOpenFileExceptionCallback;

implementation

Uses
    {$IFDEF POSIX}
    Posix.UniStd,
    Posix.StdIO,
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Windows,
    {$ENDIF}
    {$IFDEF FPC}
    ZStream,
    {$ELSE}
        {$IFDEF WIN64}
        uTExtendedX87,
        {$ENDIF}
    ZLib,
    {$ENDIF}
    {$IF DEFINED(FPC) AND NOT DEFINED(MSWINDOWS)}
    BaseUnix,
    {$IFEND}
    ID3v1Library,
    DateUtils,
    Math,
    {$IFDEF NEXTGEN}
    System.Character,
    {$ENDIF}
    {$IFDEF CHARSETDETECTOR}
    chsd_dll_intf,
    {$ENDIF}
    BufferedStream;

{$IFDEF FPC}
function StrToUIntDef(Value: String; Default: QWord): Cardinal;
begin
     Result := StrToQWordDef(Value, Default);
end;

function SameText(const s1: WideString; const s2: WideString):Boolean;
begin
     Result := WideSameText(s1, s2);
end;

procedure FileSetReadOnly(FileName: String; ReadOnly: Boolean);
begin
    if ReadOnly then begin
        FileSetAttr(FileName, FileGetAttr(FileName) OR faReadOnly);
    end else begin
        FileSetAttr(FileName, FileGetAttr(FileName) AND (NOT faReadOnly));
    end;
end;
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

{$IF DEFINED(FPC) AND NOT DEFINED(MSWINDOWS)}
type
    TTime = System.TTime;

function GetLastError: Integer;
begin
Result := fpGetErrNo
end;
{$IFEND}

type
    TStreamHelper = class helper for TStream
    public
        function Read2(var Buffer; Count: Longint): Longint;
        function ReadBOM(ADefault: Boolean = False): Boolean;
        function ReadEncoding: Byte;
        function ReadText(Encoding: Byte = 0; BigEndian: PBoolean = nil; NilFound: PBoolean = nil): String;
    end;

function TStreamHelper.Read2(var Buffer; Count: Longint): Longint;
begin
    FillChar(Buffer, Count, 0);
    Result := Read(Buffer, Count);
end;

function TStreamHelper.ReadBOM(ADefault: Boolean = False): Boolean;
var
    DataWord: Word;
    OldPosition: Int64;
begin
    OldPosition := Position;
    Read2(DataWord, 2);
    if DataWord = $FEFF then
        Result := False
    else if DataWord = $FFFE then
        Result := True
    else begin
        Result := ADefault;
        Position := OldPosition;
    end;
end;

function TStreamHelper.ReadEncoding: Byte;
var
    OldPosition: Int64;
begin
    OldPosition := Position;
    Read2(Result, 1);
    if Result > 3 then begin
        Result := 0;
        Position := OldPosition;
    end;
end;

function TStreamHelper.ReadText(Encoding: Byte = 0; BigEndian: PBoolean = nil; NilFound: PBoolean = nil): String;
var
    DataByte: Byte;
    DataWord: Word;
    Bytes: TBytes;
    CharSize: Integer;
    i: Integer;
    LBigEndian: Boolean;
    ShouldBeBigEndian: Boolean;
    StreamStartPosition: Int64;
    StringLength: Integer;
    {$IFDEF CHARSETDETECTOR}
    WideString: String;
    CharsetInfo: rCharsetInfo;
    OutputLength: Integer;
    {$ENDIF}
begin
    Result := '';
    ShouldBeBigEndian := False;
    if Assigned(BigEndian) then begin
        ShouldBeBigEndian := Boolean(BigEndian^);
    end;
    LBigEndian := (BigEndian <> nil) and ShouldBeBigEndian;
    case Encoding of
        1: begin
            LBigEndian := ReadBOM(LBigEndian);
            if BigEndian <> nil then
                Boolean(BigEndian^) := LBigEndian;
        end;
    end;
    if NilFound <> nil then
        Boolean(NilFound^) := False;
    SetLength(Bytes, 0);
    DataWord := 0;
    case Encoding of
        1, 2: CharSize := 2;
        else CharSize := 1;
    end;
    //* Scan and determine string's length
    StringLength := 0;
    StreamStartPosition := Position;
    case CharSize of
        1: begin
            repeat
                Read2(DataByte, 1);
            until (DataByte = 0)
            OR (Position >= Size);
            if DataByte = 0 then begin
                StringLength := Position - StreamStartPosition - 1;
                if NilFound <> nil then begin
                    Boolean(NilFound^) := True;
                end;
            end else begin
                StringLength := Position - StreamStartPosition;
            end;
        end;
        2: begin
            repeat
                Read2(DataWord, 2);
            until (DataWord = 0)
            OR (Position >= Size);
            if DataWord = 0 then begin
                StringLength := Position - StreamStartPosition - 2;
                if NilFound <> nil then begin
                    Boolean(NilFound^) := True;
                end;
            end else begin
                StringLength := Position - StreamStartPosition;
            end;
        end;
    end;
    //* No string
    if StringLength <= 0 then begin
        Exit;
    end;
    //* Load the string
    Seek(StreamStartPosition, soBeginning);
    SetLength(Bytes, StringLength);
    Read2(Bytes[0], StringLength);
    //* Skip terminating 0
    Seek(CharSize, soCurrent);
    //* Convert the raw string bytes to string
    case Encoding of
        0: begin
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
                SetLength(Result, Length(Bytes));
                for i := 0 to Length(Bytes) - 1 do begin
                    Result[StrLow + i] := Char(Bytes[i]);
                end;
            end;
            {$ELSE}
            if ID3v2LibraryDefaultReturnANSIText then begin
                Result := TEncoding.ANSI.GetString(Bytes);
            end else begin
                SetLength(Result, Length(Bytes));
                for i := 0 to Length(Bytes) - 1 do begin
                    Result[StrLow + i] := Char(Bytes[i]);
                end;
            end;
            {$ENDIF}
        end;
        1: begin
            if LBigEndian then begin
                Result := TEncoding.BigEndianUnicode.GetString(Bytes)
            end else begin
                Result := TEncoding.Unicode.GetString(Bytes);
            end;
        end;
        2: Result := TEncoding.BigEndianUnicode.GetString(Bytes);
        3: Result := TEncoding.UTF8.GetString(Bytes);
        else Result := TEncoding.ANSI.GetString(Bytes);
    end;
end;

function ID3v2DecodeDateOnly(const Str: String; out Error: Boolean): TDate;
var
    Year: Word;
    Month: Word;
    Day: Word;
    StrYear: String;
    StrMonth: String;
    StrDay: String;
    i: Integer;
begin
    //* yyyyMMdd
    Error := False;
    StrYear := Copy(Str, 1, 4);
    StrMonth := Copy(Str, 5, 2);
    StrDay := Copy(Str, 7, 2);
    if TryStrToInt(StrYear, i) and (i >= 1) and (i <= 9999) then
        Year := i
    else begin
        Year := 2000;
        Error := True;
    end;
    if TryStrToInt(StrMonth, i) and (i >= 1) and (i <= 12) then
        Month := i
    else begin
        Month := 1;
        Error := True;
    end;
    if TryStrToInt(StrDay, i) and (i >= 1) and (i <= MonthDays[IsLeapYear(Year)][Month]) then
        Day := i
    else begin
        Day := 1;
        Error := True;
    end;
    Result := EncodeDate(Year, Month, Day);
end;

function ID3v2DecodeDateOnly(const Str: String): TDate;
var
    Error: Boolean;
begin
    Result := ID3v2DecodeDateOnly(Str, Error);
end;

function MakeInt64(LowDWord, HiDWord: DWord): Int64;
begin
    Result := LowDWord OR (Int64(HiDWord) SHL 32);
end;

function LowDWordOfInt64(Value: Int64): Cardinal;
begin
    Result := (Value SHL 32) SHR 32;
end;

function HighDWordOfInt64(Value: Int64): Cardinal;
begin
    Result := Value SHR 32;
end;

function ReverseBytes64(const aVal: Int64): Int64; overload; //inline;
begin
    Int64Rec(Result).Bytes[0] := Int64Rec(aVal).Bytes[7];
    Int64Rec(Result).Bytes[1] := Int64Rec(aVal).Bytes[6];
    Int64Rec(Result).Bytes[2] := Int64Rec(aVal).Bytes[5];
    Int64Rec(Result).Bytes[3] := Int64Rec(aVal).Bytes[4];
    Int64Rec(Result).Bytes[4] := Int64Rec(aVal).Bytes[3];
    Int64Rec(Result).Bytes[5] := Int64Rec(aVal).Bytes[2];
    Int64Rec(Result).Bytes[6] := Int64Rec(aVal).Bytes[1];
    Int64Rec(Result).Bytes[7] := Int64Rec(aVal).Bytes[0];
end;

function ReverseBytes64(const aVal: UInt64): UInt64; overload; //inline;
begin
    Result := UInt64(ReverseBytes64(Int64(aVal)));
end;

procedure TDFFInfo.ResetData;
begin
    FormatVersion := '';
    SampleRate := 0;
    ChannelNumber := 0;
    CompressionName := '';
    SampleCount := 0;
    PlayTime := 0.0;
    BitRate := 0;
    SoundDataLength := 0;
    DSTFramesCount := 0;
    DSTFramesRate := 0;
    Ratio := 0.0;
end;

constructor TDFFInfo.Create;
begin
    inherited;
    ResetData;
end;

destructor TDFFInfo.Destroy;
begin
    inherited;
end;

function TDFFInfo.LoadFromFile(const FileName: string): Boolean;
var
    Source: TFileStream;
begin
    Result := False;
    try
        Source := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    except
        Exit;
    end;
    try
        Result := LoadFromStream(Source);
    finally
        Source.Free;
    end;
end;

function TDFFInfo.LoadFromStream(Stream: TStream): Boolean;
var
  ID3v2Size: Cardinal;
  MagicBytes: Array [0..3] of Byte;
  MemoryStream: TMemoryStream;
  StreamReadBytesLength, MemoryStreamReadBytesLength, StreamBackupPosition, MemoryStreamBackupPosition: Int64;
  N, I: Integer;
begin
  Result := False;
  StreamBackupPosition := 0;
  MemoryStream := TMemoryStream.Create;
  try
    Stream.Seek(0, soBeginning);
    ID3v2Size := GetID3v2Size(Stream);
    Stream.Seek(ID3v2Size, soBeginning);
    Stream.Read2(MagicBytes[0], SizeOf(MagicBytes));
    if (MagicBytes[0] = ORD('F')) and
       (MagicBytes[1] = ORD('R')) and
       (MagicBytes[2] = ORD('M')) and
       (MagicBytes[3] = ORD('8')) then
    begin
      Stream.Seek(Int64(ID3v2Size) + 12, soBeginning);
      Stream.Read2(MagicBytes[0], SizeOf(MagicBytes));
      if (MagicBytes[0] = ORD('D')) and
         (MagicBytes[1] = ORD('S')) and
         (MagicBytes[2] = ORD('D')) and
         (MagicBytes[3] = ORD(' ')) then
      begin
        //媒体文件类型
        Result := True;
        for N := 1 to 8 do
        begin
          Stream.Read2(MagicBytes[0], SizeOf(MagicBytes));
          //For 'FVER' Form (Required)
          if (MagicBytes[0] = ORD('F')) and
             (MagicBytes[1] = ORD('V')) and
             (MagicBytes[2] = ORD('E')) and
             (MagicBytes[3] = ORD('R')) then
          begin
            Stream.Seek(8, soCurrent);
            Stream.Read2(MagicBytes[0], SizeOf(MagicBytes));                //Pointer Skip 'FVER' Form Now
            if (MagicBytes[0] = $01) and (MagicBytes[1] = $00) then
              FormatVersion := '1.0'
            else if (MagicBytes[0] = $01) and (MagicBytes[1] = $03) then
              FormatVersion := '1.3'
            else if (MagicBytes[0] = $01) and (MagicBytes[1] = $04) then
              FormatVersion := '1.4'
            else if (MagicBytes[0] = $01) and (MagicBytes[1] = $05) then
              FormatVersion := '1.5'
            else
              FormatVersion := '';
          end
          //For 'PROP' Form (Required)
          else if (MagicBytes[0] = ORD('P')) and
                  (MagicBytes[1] = ORD('R')) and
                  (MagicBytes[2] = ORD('O')) and
                  (MagicBytes[3] = ORD('P')) then
          begin
            Stream.Read2(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);
            StreamBackupPosition := Stream.Position;
            MemoryStream.CopyFrom(Stream, StreamReadBytesLength);
            MemoryStream.Seek(0, soBeginning);
            MemoryStream.Read2(MagicBytes[0], SizeOf(MagicBytes));
            if (MagicBytes[0] = ORD('S')) and
               (MagicBytes[1] = ORD('N')) and
               (MagicBytes[2] = ORD('D')) and
               (MagicBytes[3] = ORD(' ')) then
            begin
              for I := 1 to 5 do
              begin
                MemoryStream.Read2(MagicBytes[0], SizeOf(MagicBytes));
                if (MagicBytes[0] = ORD('F')) and
                   (MagicBytes[1] = ORD('S')) and
                   (MagicBytes[2] = ORD(' ')) and
                   (MagicBytes[3] = ORD(' ')) then
                begin
                  MemoryStream.Seek(8, soCurrent);
                  MemoryStream.Read2(SampleRate, 4);
                  SampleRate := ReverseBytes32(SampleRate);
                end
                else if (MagicBytes[0] = ORD('C')) and
                        (MagicBytes[1] = ORD('H')) and
                        (MagicBytes[2] = ORD('N')) and
                        (MagicBytes[3] = ORD('L')) then
                begin
                  MemoryStream.Read2(MemoryStreamReadBytesLength, 8);
                  MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);
                  MemoryStreamBackupPosition := MemoryStream.Position;
                  MemoryStream.Read2(ChannelNumber, 2);
                  ChannelNumber := ReverseBytes16(ChannelNumber);
                  MemoryStream.Seek(MemoryStreamBackupPosition, soBeginning);
                  MemoryStream.Seek(MemoryStreamReadBytesLength, soCurrent);
                end
                else if (MagicBytes[0] = ORD('C')) and
                        (MagicBytes[1] = ORD('M')) and
                        (MagicBytes[2] = ORD('P')) and
                        (MagicBytes[3] = ORD('R')) then
                begin
                  MemoryStream.Read2(MemoryStreamReadBytesLength, 8);
                  MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);
                  MemoryStreamBackupPosition := MemoryStream.Position;
                  MemoryStream.Read2(MagicBytes[0], SizeOf(MagicBytes));
                  if (MagicBytes[0] = ORD('D')) and
                     (MagicBytes[1] = ORD('S')) and
                     (MagicBytes[2] = ORD('D')) and
                     (MagicBytes[3] = ORD(' ')) then
                  begin
                    CompressionName := 'Not Compressed';
                  end
                  else if (MagicBytes[0] = ORD('D')) and
                          (MagicBytes[1] = ORD('S')) and
                          (MagicBytes[2] = ORD('T')) and
                          (MagicBytes[3] = ORD(' ')) then
                  begin
                    CompressionName := 'DST Encoded';
                  end;
                  MemoryStream.Seek(MemoryStreamBackupPosition, soBeginning);
                  MemoryStream.Seek(MemoryStreamReadBytesLength, soCurrent);
                end
                else if (MagicBytes[0] = ORD('A')) and
                        (MagicBytes[1] = ORD('B')) and
                        (MagicBytes[2] = ORD('S')) and
                        (MagicBytes[3] = ORD('S')) then
                begin
                  MemoryStream.Read2(MemoryStreamReadBytesLength, 8);
                  MemoryStreamReadBytesLength := ReverseBytes64(MemoryStreamReadBytesLength);
                  MemoryStream.Seek(MemoryStreamReadBytesLength, soCurrent);
                end
                else if (MagicBytes[0] = ORD('L')) and
                        (MagicBytes[1] = ORD('S')) and
                        (MagicBytes[2] = ORD('C')) and
                        (MagicBytes[3] = ORD('O')) then
                begin
                  MemoryStream.Seek(4, soCurrent);
                end
                else
                  MemoryStream.Seek(-4, soCurrent);  //Pointer Return Positon Before Read MagicBytes
              end;
            end;
            Stream.Seek(StreamBackupPosition, soBeginning);
            Stream.Seek(StreamReadBytesLength, soCurrent);   //Pointer Skip 'PROP' Form Now
          end
          //For 'DSD ' or 'DST ' Form (Required)
          else if ((MagicBytes[0] = ORD('D')) and
                   (MagicBytes[1] = ORD('S')) and
                   (MagicBytes[2] = ORD('D')) and
                   (MagicBytes[3] = ORD(' '))) or
                  ((MagicBytes[0] = ORD('D')) and
                   (MagicBytes[1] = ORD('S')) and
                   (MagicBytes[2] = ORD('T')) and
                   (MagicBytes[3] = ORD(' '))) then
          begin
            if (MagicBytes[0] = ORD('D')) and
               (MagicBytes[1] = ORD('S')) and
               (MagicBytes[2] = ORD('D')) and
               (MagicBytes[3] = ORD(' ')) then
            begin
              Stream.Read2(StreamReadBytesLength, 8);
              StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);
              StreamBackupPosition := Stream.Position;
              SoundDataLength := StreamReadBytesLength;
              SampleCount := Trunc(SoundDataLength * 2 * ChannelNumber / SampleRate) * SampleRate;
              PlayTime := SampleCount / SampleRate;
              BitRate := Round((((SampleCount * ChannelNumber * 1) / 8) / PlayTime) / 125);
              Ratio := SoundDataLength / (SampleCount * (ChannelNumber * 1 / 8) + (SoundDataLength - SampleCount / 2 / ChannelNumber));
            end
            else if (MagicBytes[0] = ORD('D')) and
                    (MagicBytes[1] = ORD('S')) and
                    (MagicBytes[2] = ORD('T')) and
                    (MagicBytes[3] = ORD(' ')) then
            begin
              Stream.Read2(StreamReadBytesLength, 8);
              StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);
              StreamBackupPosition := Stream.Position;
              SoundDataLength := StreamReadBytesLength;
              Stream.Read2(MagicBytes[0], SizeOf(MagicBytes));
              if (MagicBytes[0] = ORD('F')) and
                 (MagicBytes[1] = ORD('R')) and
                 (MagicBytes[2] = ORD('T')) and
                 (MagicBytes[3] = ORD('E')) then
              begin
                Stream.Seek(8, soCurrent);
                Stream.Read2(DSTFramesCount, 4);
                DSTFramesCount := ReverseBytes32(DSTFramesCount);
                Stream.Read2(DSTFramesRate, 2);
                DSTFramesRate := ReverseBytes16(DSTFramesRate);
                PlayTime := DSTFramesCount / DSTFramesRate;
                SampleCount := Round(PlayTime * SampleRate);
                BitRate := Round((((SampleCount * ChannelNumber * 1) / 8) / PlayTime) / 125);
                Ratio := SoundDataLength / (SampleCount * (ChannelNumber * 1 / 8));
              end;
            end;
            Stream.Seek(StreamBackupPosition, soBeginning);
            Stream.Seek(StreamReadBytesLength, soCurrent);   //Pointer Skip 'DSD ' or 'DST ' Form Now
          end
          //For 'DSTI' Form (Optional)
          else if (MagicBytes[0] = ORD('D')) and
                  (MagicBytes[1] = ORD('S')) and
                  (MagicBytes[2] = ORD('T')) and
                  (MagicBytes[3] = ORD('I')) then
          begin
            Stream.Read2(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'DSTI' Form Size
            Stream.Seek(StreamReadBytesLength, soCurrent);  //Pointer Skip 'DSTI' Form Now
          end
          //For 'COMT' Form (Optional)
          else if (MagicBytes[0] = ORD('C')) and
                  (MagicBytes[1] = ORD('O')) and
                  (MagicBytes[2] = ORD('M')) and
                  (MagicBytes[3] = ORD('T')) then
          begin
            Stream.Read2(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'COMT' Form Size
            Stream.Seek(StreamReadBytesLength, soCurrent);  //Pointer Skip 'COMT' Form Now
          end
          //For 'DIIN' Form (Optional)
          else if (MagicBytes[0] = ORD('D')) and
                  (MagicBytes[1] = ORD('I')) and
                  (MagicBytes[2] = ORD('I')) and
                  (MagicBytes[3] = ORD('N')) then
          begin
            Stream.Read2(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'DIIN' Form Size
            Stream.Seek(StreamReadBytesLength, soCurrent);  //Pointer Skip 'DIIN' Form Now
          end
          //For 'MANF' Form (Optional)
          else if (MagicBytes[0] = ORD('M')) and
                  (MagicBytes[1] = ORD('A')) and
                  (MagicBytes[2] = ORD('N')) and
                  (MagicBytes[3] = ORD('F')) then
          begin
            Stream.Read2(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'MANF' Form Size
            Stream.Seek(StreamReadBytesLength, soCurrent);  //Pointer Skip 'MANF' Form Now
          end
          //For 'ID3 ' Form (Optional)
          else if (MagicBytes[0] = ORD('I')) and
                  (MagicBytes[1] = ORD('D')) and
                  (MagicBytes[2] = ORD('3')) and
                  (MagicBytes[3] = ORD(' ')) then
          begin
            Stream.Read2(StreamReadBytesLength, 8);
            StreamReadBytesLength := ReverseBytes64(StreamReadBytesLength);  //'ID3 ' Form Size
            Stream.Seek(StreamReadBytesLength, soCurrent);  //Pointer Skip 'ID3 ' Form Now
          end
          else
            Stream.Seek(-4, soCurrent);  //Pointer Return Positon Before Read MagicBytes
        end;
      end;
    end;
  finally
    MemoryStream.Free;
  end;
end;

Constructor TID3v2ExtendedHeader3.Create;
begin
    inherited;
    Flags := 0;
    Size := 0;
    //SizeData := TMemoryStream.Create;
    Data := TMemoryStream.Create;
end;

Destructor TID3v2ExtendedHeader3.Destroy;
begin
    //FreeAndNil(SizeData);
    FreeAndNil(Data);
    inherited;
end;

procedure TID3v2ExtendedHeader3.DecodeExtendedHeaderSize;
begin
    UnSyncSafe(CodedSize, 4, Size);
end;

procedure TID3v2ExtendedHeader3.DecodeExtendedHeaderFlags;
var
    Bit: Byte;
begin
    Flags := ReverseBytes16(Flags);
    Bit := Flags SHR 15;
    CRCPresent := Boolean(Bit);
end;

Constructor TID3v2ExtendedHeader4.Create;
begin
    inherited;
    TagIsAnUpdate := False;
    CRCPresent := False;
    TagRestrictions := False;
    TagRestrictionsData := NoMoreThan128FramesAnd1MBTotalTagSize;
    TextEncodingRestrictions := NoTextEncodingRestrictions;
    TextFieldsSizeRestriction := NoTextFieldsSizeRestrictions;
    ImageEncodingRestriction := NoImageEncodingRestrictions;
    ImageSizeRestriction := NoImageSizeRestrictions;
end;

Destructor TID3v2ExtendedHeader4.Destroy;
begin
    inherited;
end;

procedure TID3v2ExtendedHeader4.DecodeExtendedHeaderSize;
begin
    UnSyncSafe(CodedSize, 4, Size);
end;

procedure TID3v2ExtendedHeader4.DecodeExtendedHeaderFlags;
var
    Bit: Byte;
begin
    {$RANGECHECKS OFF}
    Bit := Flags SHL 1;
    Bit := Bit SHR 7;
    TagIsAnUpdate := Boolean(Bit);
    Bit := Flags SHL 2;
    Bit := Bit SHR 7;
    CRCPresent := Boolean(Bit);
    Bit := Flags SHL 3;
    Bit := Bit SHR 7;
    TagRestrictions := Boolean(Bit);
    {$IFDEF RANGECHECKSAREON}
        {$RANGECHECKS ON}
    {$ENDIF}
end;

procedure TID3v2ExtendedHeader4.CalculateExtendedFlagsDataSize;
begin
    ExtendedFlagsDataSize := 0;
    if TagIsAnUpdate then begin
        //* No flag data
    end;
    if CRCPresent then begin
        ExtendedFlagsDataSize := ExtendedFlagsDataSize + 5;
    end;
    if TagRestrictions then begin
        ExtendedFlagsDataSize := ExtendedFlagsDataSize + 1;
    end;
end;

procedure TID3v2ExtendedHeader4.DecodeExtendedHeaderFlagData;
begin
    //* Not yet implemented
end;

Constructor TID3v2Frame.Create(_Parent: TID3v2Frames);
begin
    inherited Create;
    Parent := _Parent;
    //ID := '';
    Flags := 0;
    TagAlterPreservation := False;
    FileAlterPreservation := False;
    ReadOnly := False;
    Compressed := False;
    Encrypted := False;
    GroupingIdentity := False;
    DataLengthIndicator := False;
    Stream := TMemoryStream.Create;
    DataLengthIndicatorValue := 0;
    GroupIdentifier := 0;
    EncryptionMethod := 0;
end;

Destructor TID3v2Frame.Destroy;
begin
    FreeAndNil(Stream);
    Inherited;
end;

procedure TID3v2Frame.DecodeFlags3;
var
    Bit: Word;
begin
    {$RANGECHECKS OFF}
    Bit := Flags SHR 15;
    TagAlterPreservation := Boolean(Bit);
    Bit := Flags SHL 1;
    Bit := Bit SHR 15;
    FileAlterPreservation := Boolean(Bit);
    Bit := Flags SHL 2;
    Bit := Bit SHR 15;
    ReadOnly := Boolean(Bit);
    Bit := Flags SHL 8;
    Bit := Bit SHR 15;
    Compressed := Boolean(Bit);
    Bit := Flags SHL 9;
    Bit := Bit SHR 15;
    Encrypted := Boolean(Bit);
    Bit := Flags SHL 10;
    Bit := Bit SHR 15;
    GroupingIdentity := Boolean(Bit);
    {$IFDEF RANGECHECKSAREON}
        {$RANGECHECKS ON}
    {$ENDIF}
end;

procedure TID3v2Frame.EncodeFlags3;
var
    EncodedFlags: Word;
    Bit: Word;
begin
    EncodedFlags := 0;
    if TagAlterPreservation then begin
        Bit := 1 SHL 7;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if FileAlterPreservation then begin
        Bit := 1 SHL 6;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if ReadOnly then begin
        Bit := 1 SHL 5;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if Compressed then begin
        Bit := 1 SHL 15;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if Encrypted then begin
        Bit := 1 SHL 14;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if GroupingIdentity then begin
        Bit := 1 SHL 13;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    Flags := EncodedFlags;
end;

procedure TID3v2Frame.DecodeFlags4;
var
    Bit: Word;
begin
    {$RANGECHECKS OFF}
    Bit := Flags SHR 14;
    TagAlterPreservation := Boolean(Bit);
    Bit := Flags SHL 1;
    Bit := Bit SHR 14;
    FileAlterPreservation := Boolean(Bit);
    Bit := Flags SHL 2;
    Bit := Bit SHR 14;
    ReadOnly := Boolean(Bit);
    Bit := Flags SHL 9;
    Bit := Bit SHR 15;
    GroupingIdentity := Boolean(Bit);
    Bit := Flags SHL 12;
    Bit := Bit SHR 15;
    Compressed := Boolean(Bit);
    Bit := Flags SHL 13;
    Bit := Bit SHR 15;
    Encrypted := Boolean(Bit);
    Bit := Flags SHL 14;
    Bit := Bit SHR 15;
    Unsynchronised := Unsynchronised OR Boolean(Bit);
    Bit := Flags SHL 15;
    Bit := Bit SHR 15;
    DataLengthIndicator := Boolean(Bit);
    {$IFDEF RANGECHECKSAREON}
        {$RANGECHECKS ON}
    {$ENDIF}
end;

procedure TID3v2Frame.EncodeFlags4;
var
    EncodedFlags: Word;
    Bit: Word;
begin
    EncodedFlags := 0;
    if TagAlterPreservation then begin
        Bit := 1 SHL 14;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if FileAlterPreservation then begin
        Bit := 1 SHL 13;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if ReadOnly then begin
        Bit := 1 SHL 12;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if GroupingIdentity then begin
        Bit := 1 SHL 6;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if Compressed then begin
        Bit := 1 SHL 3;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if Encrypted then begin
        Bit := 1 SHL 2;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if Unsynchronised then begin
        Bit := 1 SHL 1;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if DataLengthIndicator then begin
        Bit := 1;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    Flags := EncodedFlags;
end;

(*
function TID3v2Frame.FullFrameSize: Cardinal;
begin
    Result := 0;
    if Parent.Root.MajorVersion = 2 then begin
        Result := Stream.Size;
    end;
    if Parent.Root.MajorVersion = 3 then begin
        Result := Stream.Size;
        if Compressed
        OR DataLengthIndicator
        then begin
            Result := Result + 4;
        end;
        if Encrypted then begin
            Result := Result + 1;
        end;
        if GroupingIdentity then begin
            Result := Result + 1;
        end;
    end;
    if Parent.Root.MajorVersion > 3 then begin
        Result := Stream.Size;
        if GroupingIdentity then begin
            Result := Result + 1;
        end;
        if Encrypted then begin
            Result := Result + 1;
        end;
        if DataLengthIndicator then begin
            Result := Result + 4;
        end;
    end;
end;
*)

function TID3v2Frame.GetIndex: Integer;
begin
    Result := Self.Parent.Frames.IndexOf(Self);
end;

function TID3v2Frame.Compress: Boolean;
var
    CompressionStream: {$IFDEF FPC}TcompressionStream{$ELSE}TZCompressionStream{$ENDIF};
    CompressedStream: TMemoryStream;
    UnCompressedSize: Int64;
begin
    Result := False;
    if Stream.Size = 0 then begin
        Exit;
    end;
    CompressionStream := nil;
    CompressedStream := nil;
    try
        try
            CompressedStream := TMemoryStream.Create;
            //* TZCompressionStream constructor has changed
            {$IFDEF FPC}
            CompressionStream := Tcompressionstream.Create(clmax, CompressedStream);
            {$ELSE}
            {$IF CompilerVersion >= 22}
            CompressionStream := TZCompressionStream.Create(clMax, CompressedStream);
            {$ELSE}
            CompressionStream := TZCompressionStream.Create(CompressedStream, zcMax);
            {$IFEND}
            {$IFEND}
            Stream.Seek(0, soBeginning);
            CompressionStream.CopyFrom(Stream, Stream.Size);
            //* Needed to flush the buffer
            {$IFDEF NEXTGEN}
            CompressionStream.DisposeOf;
            CompressionStream := nil;
            {$ELSE}
            FreeAndNil(CompressionStream);
            {$ENDIF}
            if CompressedStream.Size > 0 then begin
                UnCompressedSize := Stream.Size;
                //Stream.Clear;
                DataLengthIndicatorValue := UnCompressedSize;
                CompressedStream.Seek(0, soBeginning);
                //Stream.CopyFrom(CompressedStream, CompressedStream.Size);

                {$IFDEF NEXTGEN}
                Stream.DisposeOf;
                Stream := nil;
                {$ELSE}
                FreeAndNil(Stream);
                {$ENDIF}

                Stream := CompressedStream;
                CompressedStream := nil;

                Compressed := True;
                DataLengthIndicator := True;
                Result := True;
            end;
        except
            //*
        end;
    finally
        if Assigned(CompressionStream) then begin
            FreeAndNil(CompressionStream);
        end;
        if Assigned(CompressedStream) then begin
            FreeAndNil(CompressedStream);
        end;
    end;
end;

function TID3v2Frame.DeCompress: Boolean;
var
    DeCompressionStream: {$IFDEF FPC}Tdecompressionstream{$ELSE}TZDeCompressionstream{$ENDIF};
    UnCompressedStream: TMemoryStream;
begin
    Result := False;
    if Stream.Size <= 4 then begin
        Exit;
    end;
    DeCompressionStream := nil;
    UnCompressedStream := nil;
    try
        try
            UnCompressedStream := TMemoryStream.Create;
            Stream.Seek(0, soBeginning);
            {$IFDEF FPC}
            DeCompressionStream := Tdecompressionstream.Create(Stream);
            {$ELSE}
            DeCompressionStream := TZDeCompressionStream.Create(Stream);
            {$IFEND}
            DeCompressionStream.Seek(0, soBeginning);
            UnCompressedStream.CopyFrom(DeCompressionStream, 0);
            //Stream.Clear;
            //Stream.CopyFrom(UnCompressedStream, 0);
            Stream.Seek(0, soBeginning);

            {$IFDEF NEXTGEN}
            Stream.DisposeOf;
            Stream := nil;
            {$ELSE}
            FreeAndNil(Stream);
            {$ENDIF}

            Stream := UnCompressedStream;
            UnCompressedStream := nil;

            Compressed := False;
            DataLengthIndicator := False;
            Result := True;
        except
            //*
        end;
    finally
        if Assigned(DeCompressionStream) then begin
            FreeAndNil(DeCompressionStream);
        end;
        if Assigned(UnCompressedStream) then begin
            FreeAndNil(UnCompressedStream);
        end;
    end;
end;

procedure TID3v2Frame.Delete;
begin
    if Assigned(Parent) then begin
        Parent.DeleteFrame(Index);
    end;
end;

function TID3v2Frame.RemoveUnsynchronisation: Boolean;
begin
    FWasUnsynchronised := Unsynchronised;
    Result := RemoveUnsynchronisationOnStream(Stream);
    if Result then begin
        Unsynchronised := False;
    end;
end;

function TID3v2Frame.ApplyUnsynchronisation: Boolean;
begin
    Result := ApplyUnsynchronisationOnStream(Stream);
    if Result then begin
        Unsynchronised := True;
    end;
end;

function TID3v2Frame.Assign(ID3v2Frame: TID3v2Frame): Boolean;
begin
    Result := False;
    Clear;
    if ID3v2Frame <> nil then begin
        ID := ID3v2Frame.ID;
        Size := ID3v2Frame.Size;
        Flags := ID3v2Frame.Flags;
        TagAlterPreservation := ID3v2Frame.TagAlterPreservation;
        FileAlterPreservation := ID3v2Frame.FileAlterPreservation;
        ReadOnly := ID3v2Frame.ReadOnly;
        Compressed := ID3v2Frame.Compressed;
        Encrypted := ID3v2Frame.Encrypted;
        GroupingIdentity := ID3v2Frame.GroupingIdentity;
        Unsynchronised := ID3v2Frame.Unsynchronised;
        DataLengthIndicator := ID3v2Frame.DataLengthIndicator;
        GroupIdentifier := ID3v2Frame.GroupIdentifier;
        EncryptionMethod := ID3v2Frame.EncryptionMethod;
        ID3v2Frame.Stream.Seek(0, soBeginning);
        Stream.CopyFrom(ID3v2Frame.Stream, ID3v2Frame.Stream.Size);
        ID3v2Frame.Stream.Seek(0, soBeginning);
        if (Self is TID3v2FrameWithSubFrames)
        AND (ID3v2Frame is TID3v2FrameWithSubFrames)
        then begin
            (Self as TID3v2FrameWithSubFrames).SubFrames.AssignFrames((ID3v2Frame as TID3v2FrameWithSubFrames).SubFrames);
        end;
    end;
end;

procedure TID3v2Frame.Clear;
begin
    ClearFrameID(ID);
    Size := 0;
    Flags := 0;
    TagAlterPreservation := False;
    FileAlterPreservation := False;
    ReadOnly := False;
    Compressed := False;
    Encrypted := False;
    GroupingIdentity := False;
    Unsynchronised := False;
    FWasUnsynchronised := False;
    DataLengthIndicator := False;
    GroupIdentifier := 0;
    EncryptionMethod := 0;
    Stream.Clear;
end;

Constructor TID3v2Tag.Create;
begin
    Inherited;
    Root := Self;
    ExtendedHeader3 := TID3v2ExtendedHeader3.Create;
    ExtendedHeader4 := TID3v2ExtendedHeader4.Create;
    DSFInfo := TDSFInfo.Create;
    DFFInfo := TDFFInfo.Create;
    Clear;
    ParseAudioAttributes := ID3v2LibraryDefaultParseAudioAttributes;
    TrimText := ID3v2LibraryDefaultTrimText;
    BeginningSearchLength := ID3v2LibraryDefaultBeginningSearchLength;
    MPEGSearchLength := ID3v2LibraryDefaultMPEGSearchLength;
    AutoRemoveUnsynchronisation := ID3v2LibraryDefaultAutoRemoveUnsynchronisation;
    ParseCoverArts := True;
    APICTextEncodingToWrite := ID3v2LibraryDefaultAPICTextEncodingToWrite;
    PreserveFileDateTime := ID3v2LibraryDefaultPreserveFileDateTime;
    WriteVersion := ID3v2LibraryDefaultWriteVersion;
end;

Destructor TID3v2Tag.Destroy;
begin
    Clear;
    FreeAndNil(ExtendedHeader3);
    FreeAndNil(ExtendedHeader4);
    FreeAndNil(DSFInfo);
    FreeAndNil(DFFInfo);
    Inherited;
end;

procedure TID3v2Frames.DeleteAllCoverArts;
var
    i: Integer;
begin
    for i := FrameCount - 1 downto 0 do begin
        if IsSameFrameID(Frames[i].ID, 'APIC') then begin
            DeleteFrame(i);
        end;
    end;
end;

function TID3v2Tag.LoadTagFromStream(Stream: TStream): Integer;
var
    ValidFrameLoaded: Boolean;
begin
    Result := ID3V2LIBRARY_ERROR;
    if NOT ID3v2ValidTag(Stream) then begin
        Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
        Exit;
    end;
    try
        Stream.Read2(MajorVersion, 1);
        Stream.Read2(MinorVersion, 1);
    except
        Exit;
    end;
    if (MajorVersion > 4)
    OR (MajorVersion < 2)
    then begin
        Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_VERSION;
        Exit;
    end;
    try
        Stream.Read2(Flags, 1);
        DecodeFlags;
    except
        Exit;
    end;
    try
        Stream.Read2(CodedSize, 4);
        DecodeSize;
    except
        Exit;
    end;
    FPosition := Stream.Position - 10;
    if ExtendedHeader then begin
        //Showmessage('Extended header found!');
        ReadExtendedHeader(Stream);
    end;
    repeat
        ValidFrameLoaded := LoadFrame(Stream);
        //* TODO seek back 3 bytes for compatibility for corrupt tags and try again
    until NOT ValidFrameLoaded
    OR (Stream.Position >= FPosition + Self.Size);
    Result := ID3V2LIBRARY_SUCCESS;
    Loaded := True;
end;

function TID3v2Tag.LoadFromStream(Stream: TStream; Buffered: Boolean = True): Integer;
var
    PreviousPosition: Int64;
    LStream: TStream;
begin
    Loaded := False;
    if Buffered then
        LStream := TBufferedStream.Create(Stream)
    else
        LStream := Stream;
    //Result := ID3V2LIBRARY_ERROR;
    try
        Clear;
        FFileSize := LStream.Size;
        //APEv2TagSize := GetAPEv2TagSize(LStream);
        PreviousPosition := LStream.Position;
        if NOT ID3v2ValidTag(LStream) then begin
            LStream.Seek(PreviousPosition, soBeginning);
            //* WAV
            if CheckRIFF(LStream) then begin
                FSourceFileType := sftWAVE;
                if ParseAudioAttributes then begin
                    LoadWAVEAttributes(LStream);
                end;
                if SeekRIFF(LStream) = 0 then begin
                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                    Exit;
                end;
            end else begin
                //* WAV64
                LStream.Seek(PreviousPosition, soBeginning);
                if CheckRF64(LStream) then begin
                    FSourceFileType := sftRF64;
                    if ParseAudioAttributes then begin
                        LoadWAVEAttributes(LStream);
                    end;
                    if SeekRF64(LStream) = 0 then begin
                        Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                        Exit;
                    end;
                end else begin
                    //* AIFF
                    LStream.Seek(PreviousPosition, soBeginning);
                    if CheckAIFF(LStream) then begin
                        FSourceFileType := sftAIFF;
                        if ParseAudioAttributes then begin
                            AIFFInfo := GetAIFFInformation(LStream);
                        end;
                        if SeekAIFF(LStream) = 0 then begin
                            Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                            Exit;
                        end;
                    end else begin
                        //* DSF
                        LStream.Seek(PreviousPosition, soBeginning);
                        if CheckDSF(LStream) then begin
                            FSourceFileType := sftDSF;
                            if ParseAudioAttributes then begin
                                LoadDSFInfo(LStream);
                            end;
                            if SeekDSF(LStream) = 0 then begin
                                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                                Exit;
                            end;
                        end else begin
                            //* DFF
                            LStream.Seek(PreviousPosition, soBeginning);
                            if CheckDFF(LStream) then begin
                                FSourceFileType := sftDFF;
                                if ParseAudioAttributes then begin
                                    DFFInfo.LoadFromStream(LStream);
                                end;
                                if SeekDFF(LStream) = 0 then begin
                                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                                    Exit;
                                end;
                            end else if BeginningSearchLength > 0 then begin
                                LStream.Seek(PreviousPosition, soBeginning);
                                if NOT ID3v2SearchTag(LStream, BeginningSearchLength) then begin
                                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                                    Exit;
                                end;
                            end else begin
                                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                                Exit;
                            end;
                        end;
                    end;
                end;
            end;
            if NOT ID3v2ValidTag(LStream) then begin
                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                Exit;
            end;
        end;
        LStream.Seek(- 3, soCurrent);
        Result := LoadTagFromStream(LStream);
    finally
        FWasUnsynchronised := Self.Unsynchronised;
        if AutoRemoveUnsynchronisation then begin
            RemoveUnsynchronisationOnAllFrames;
            Self.Unsynchronised := False;
        end;
        //* Check if source is an MPEG
        LStream.Seek(Self.Size, soBeginning);
        if FSourceFileType = sftUnknown then begin
            if CheckMPEG(LStream) then begin
                FSourceFileType := sftMPEG;
                if ParseAudioAttributes then begin
                    APEv2TagSize := GetAPEv2TagSize(LStream);
                    MPEGInfo := MPEGProcessHeader(LStream);
                    GetMPEGHeaderInformation(LStream);
                    if MPEGInfo.VBR then begin
                        if PlayTime <> 0 then begin
                            FBitRate := Trunc((LStream.Size - MPEGInfo.Position - APEv2TagSize) / (125 * PlayTime ) + 0.5); // bitrate (Kbps)
                        end else begin
                            FBitRate := MPEGInfo.BitRate;
                        end;
                    end else begin
                        FBitRate := MPEGInfo.BitRate;
                    end;
                end;
            end;
        end;
        if Buffered then
            FreeAndNil(LStream);
    end;
end;

function TID3v2Tag.LoadFromFile(FileName: String; Buffered: Boolean = True): Integer;
var
    FileStream: TFileStream;
begin
    Clear;
    Loaded := False;
    if NOT FileExists(FileName) then begin
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        FileStream := TFileStream.Create(FileName, fmOpenRead OR fmShareDenyWrite);
    except
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        Result := LoadFromStream(FileStream, Buffered);
        if Result < ID3V2LIBRARY_ERROR_OPENING_FILE
        //OR (Result = ID3V2LIBRARY_ERROR_NOT_SUPPORTED_VERSION)
        then begin
            Self.SourceFileName := FileName;
        end;
    finally
        FreeAndNil(FileStream);
    end;
end;

function TID3v2Tag.LoadFromMemory(MemoryAddress: Pointer): Integer;
var
    ID3v2Size: Cardinal;
    DataStream: TMemoryStream;
begin
    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
    if MemoryAddress <> nil then begin
        ID3v2Size := GetID3v2Size(MemoryAddress);
        if ID3v2Size > 0 then begin
            DataStream := TMemoryStream.Create;
            try
                DataStream.Write(MemoryAddress^, ID3v2Size);
                DataStream.Seek(0, soBeginning);
                Result := LoadFromStream(DataStream, False);
            finally
                FreeAndNil(DataStream);
            end;
        end;
    end;
end;

procedure TID3v2Tag.DecodeFlags;
var
    Bit: Byte;
begin
    {$RANGECHECKS OFF}
    if MajorVersion = 2 then begin
        Bit := Flags SHR 7;
        Unsynchronised := Boolean(Bit);
        Bit := Flags SHL 1;
        Bit := Bit SHR 7;
        Compressed := Boolean(Bit);
    end else begin
        Bit := Flags SHR 7;
        Unsynchronised := Boolean(Bit);
        Bit := Flags SHL 1;
        Bit := Bit SHR 7;
        ExtendedHeader := Boolean(Bit);
        Bit := Flags SHL 2;
        Bit := Bit SHR 7;
        Experimental := Boolean(Bit);
        Bit := Flags SHL 3;
        Bit := Bit SHR 7;
        FooterPresent := Boolean(Bit);
    end;
    {$IFDEF RANGECHECKSAREON}
        {$RANGECHECKS ON}
    {$ENDIF}
end;

procedure TID3v2Tag.EncodeFlags;
var
    EncodedFlags: Byte;
    Bit: Byte;
begin
    EncodedFlags := 0;
    if Unsynchronised then begin
        Bit := 1 SHL 7;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if ExtendedHeader then begin
        //* Extended header writing is not supported
        //Bit := 1 SHL 6;
        //EncodedFlags := EncodedFlags OR Bit;
    end;
    if Experimental then begin
        Bit := 1 SHL 5;
        EncodedFlags := EncodedFlags OR Bit;
    end;
    if FooterPresent then begin
        //* Footer writing is not supported
        //Bit := 1 SHL 6;
        //EncodedFlags := EncodedFlags OR Bit;
    end;
    Flags := EncodedFlags;
end;

procedure TID3v2Tag.DecodeSize;
begin
    UnSyncSafe(CodedSize, 4, Size);
    Size := Size + 10;
end;

function TID3v2Tag.ReadExtendedHeader(Stream: TStream): Boolean;
var
    ExtendedFrameID: TFrameID;
begin
    try
        Stream.Read2(ExtendedFrameID[0], 4);
        //* Support for bad Tags that report an extended header but don't have one
        if NOT ValidID3v2FrameID(ExtendedFrameID) then begin
            Stream.Seek(-4, soCurrent);
            //* v3
            if MajorVersion = 3 then begin
                with ExtendedHeader3 do begin
                    //* If extended header is unsynchronised needed to remove it
                    //SizeData.CopyFrom(TagStream, 4);
                    //if Unsynchronised then begin
                    //    RemoveUnsynchronisationOnExtendedHeaderSize;
                    //end;
                    //SizeData.Seek(0, soBeginning);
                    //SizeData.Read2(CodedExtendedHeaderSize3, 4);
                    //SizeData.Seek(0, soBeginning);
                    Stream.Read2(CodedSize, 4);
                    DecodeExtendedHeaderSize;
                    //* Read extended header flags
                    Stream.Read2(ExtendedHeader3.Flags, 2);
                    DecodeExtendedHeaderFlags;
                    Data.CopyFrom(Stream, Size - 2);
                    if Unsynchronised then begin
                        RemoveUnsynchronisationOnExtendedHeaderData;
                    end;
                end;
            end;
            //* v4
            if MajorVersion = 4 then begin
                with ExtendedHeader4 do begin
                    Stream.Read2(CodedSize, 4);
                    DecodeExtendedHeaderSize;
                    Stream.Read2(FlagBytes, 1);
                    Stream.Read2(Flags, 1);
                    DecodeExtendedHeaderFlags;
                    CalculateExtendedFlagsDataSize;
                    SetLength(ExtendedFlagsData, ExtendedFlagsDataSize);
                    Stream.Read2(ExtendedFlagsData[0], ExtendedFlagsDataSize);
                    DecodeExtendedHeaderFlagData;
                end;
            end;
        end else begin
            ExtendedHeader := False;
            Stream.Seek(- 4, soCurrent);
        end;
        Result := True;
    except
        Result := False;
    end;
end;

procedure UnSyncSafe(var Source; const SourceSize: Integer; var Dest: Cardinal);
type
    TBytes = array [0..MaxInt - 1] of Byte;
var
    I: Byte;
begin
    { Test : Source = $01 $80 -> Dest = 255
             Source = $02 $00 -> Dest = 256
             Source = $02 $01 -> Dest = 257 etc.
    }
    Dest := 0;
    for I := 0 to SourceSize - 1 do begin
        Dest := Dest shl 7;
        Dest := Dest or (TBytes(Source)[I] and $7F); // $7F = %01111111
    end;
end;

procedure SyncSafe(Source: Cardinal; var Dest; const DestSize: Integer);
type
    TBytes = array [0..MaxInt - 1] of Byte;
var
    I: Byte;
begin
    { Test : Source = 255 -> Dest = $01 $80
             Source = 256 -> Dest = $02 $00
             Source = 257 -> Dest = $02 $01 etc.
    }
    for I := DestSize - 1 downto 0 do begin
        TBytes(Dest)[I] := Source and $7F; // $7F = %01111111
        Source := Source shr 7;
    end;
end;

{$IFNDEF FPC}
{$IFDEF WIN64}
procedure ReverseExtended(Source: TExtendedX87; var Dest: TExtendedX87);
begin
    Dest.AsBytes[0] := Source.AsBytes[9];
    Dest.AsBytes[1] := Source.AsBytes[8];
    Dest.AsBytes[2] := Source.AsBytes[7];
    Dest.AsBytes[3] := Source.AsBytes[6];
    Dest.AsBytes[4] := Source.AsBytes[5];
    Dest.AsBytes[5] := Source.AsBytes[4];
    Dest.AsBytes[6] := Source.AsBytes[3];
    Dest.AsBytes[7] := Source.AsBytes[2];
    Dest.AsBytes[8] := Source.AsBytes[1];
    Dest.AsBytes[9] := Source.AsBytes[0];
end;
{$ELSE}
procedure ReverseExtended(Source: TExtendedBytes; var Dest: TExtendedBytes);
begin
    Dest[0] := Source[9];
    Dest[1] := Source[8];
    Dest[2] := Source[7];
    Dest[3] := Source[6];
    Dest[4] := Source[5];
    Dest[5] := Source[4];
    Dest[6] := Source[3];
    Dest[7] := Source[2];
    Dest[8] := Source[1];
    Dest[9] := Source[0];
end;
{$ENDIF}
{$ENDIF}

function TID3v2Frames.LoadFrame(Stream: TStream): Boolean;
var
    FrameID: TFrameID;
    FrameIndex: Integer;
    ValidFrame: Boolean;
begin
    Result := False;
    ClearFrameID(FrameID);
    try
        if Root.MajorVersion = 2 then begin
            Stream.Read2(FrameID[0], 3);
            ValidFrame := ValidID3v2FrameID2(FrameID);
        end else begin
            Stream.Read2(FrameID[0], 4);
            ValidFrame := ValidID3v2FrameID(FrameID);
        end;
        {$IFDEF STRANGE_TAG}
        if (Self.MajorVersion > 2) and not ValidID3v2FrameID(FrameID) then begin
            FrameID[3] := 0;
            Stream.Seek(- 1, soCurrent);
            FStrangeTag := True;
        end;
        //* Workaround for buggy DataLengthIndicator
        if NOT ValidFrame then begin
            Stream.Read2(FrameID[0], 4);
            ValidFrame := ValidID3v2FrameID(FrameID);
        end;
        {$ENDIF}
        if ValidFrame then begin
            FrameIndex := AddFrame(FrameID);
            if FrameIndex > - 1 then begin
                LoadFrameData(Stream, FrameIndex);
                Result := not Root.FError;
            end;
        end;
    except

    end;
end;

procedure TID3v2Frames.LoadFrameData(Stream: TStream; FrameIndex: Integer);
var
    FrameSize: DWord;
    FrameFlags: Word;
    DataLengthIndicatorValueCoded: Cardinal;
begin
    try
        {$IFDEF STRANGE_TAG}
        if FStrangeTag and (Frames[FrameIndex].ID[3] = 0) then begin
            FrameSize := 0;
            Stream.Seek(4, soCurrent);
            Stream.Read2(FrameSize, 3);
            Frames[FrameIndex].Size := FrameSize;
            if Frames[FrameIndex].Size > Self.Size then begin
                FError := True;
            end
            else if Frames[FrameIndex].Size > 0 then begin
                Frames[FrameIndex].Unsynchronised := Unsynchronised;
                Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
            end;
            Convertv2Tov3(FrameIndex);
            Exit;
        end;
        {$ENDIF}
        if Root.MajorVersion = 2 then begin
            FrameSize := 0;
            Stream.Read2(FrameSize, 3);
            Frames[FrameIndex].Size := ReverseBytes32(FrameSize SHL 8);
            if Frames[FrameIndex].Size > Root.Size then begin
                Root.FError := True;
            end
            else if Frames[FrameIndex].Size > 0 then begin
                Frames[FrameIndex].Unsynchronised := Root.Unsynchronised;

                if Root.ParseCoverArts then begin
                    Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
                end else begin
                    if IsSameFrameID(Frames[FrameIndex].ID, 'PIC') then begin
                        Stream.Seek(Frames[FrameIndex].Size, soCurrent);
                    end else begin
                        Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
                    end;
                end;

            end;
            Convertv2Tov3(FrameIndex);
        end;
        if Root.MajorVersion = 3 then begin
            Stream.Read2(FrameSize, 4);
            Frames[FrameIndex].Size := ReverseBytes32(FrameSize);
            Stream.Read2(FrameFlags, 2);
            Frames[FrameIndex].Flags := ReverseBytes16(FrameFlags);
            if Frames[FrameIndex].Size > Root.Size then begin
                Root.FError := True;
            end
            else if Frames[FrameIndex].Size > 0 then begin
                Frames[FrameIndex].DecodeFlags3;
                Frames[FrameIndex].Unsynchronised := Root.Unsynchronised;
                if Frames[FrameIndex].Compressed then begin
                    Stream.Read2(DataLengthIndicatorValueCoded, 4);
                    UnSyncSafe(DataLengthIndicatorValueCoded, 4, Frames[FrameIndex].DataLengthIndicatorValue);
                    Frames[FrameIndex].DataLengthIndicator := True;
                    Frames[FrameIndex].Size := Frames[FrameIndex].Size - 4;
                end;
                if Frames[FrameIndex].Encrypted then begin
                    Stream.Read2(Frames[FrameIndex].EncryptionMethod, 1);
                    Frames[FrameIndex].Size := Frames[FrameIndex].Size - 1;
                end;
                if Frames[FrameIndex].GroupingIdentity then begin
                    Stream.Read2(Frames[FrameIndex].GroupIdentifier, 1);
                    Frames[FrameIndex].Size := Frames[FrameIndex].Size - 1;
                end;
                if Root.ParseCoverArts then begin
                    Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
                end else begin
                    if IsSameFrameID(Frames[FrameIndex].ID, 'APIC') then begin
                        Stream.Seek(Frames[FrameIndex].Size, soCurrent);
                    end else begin
                        Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
                    end;
                end;
            end;
        end;
        if Root.MajorVersion > 3 then begin
            Stream.Read2(FrameSize, 4);
            UnSyncSafe(FrameSize, 4, Frames[FrameIndex].Size);
            Stream.Read2(FrameFlags, 2);
            Frames[FrameIndex].Flags := ReverseBytes16(FrameFlags);
            if Frames[FrameIndex].Size > Root.Size then begin
                Root.FError := True;
            end
            else if Frames[FrameIndex].Size > 0 then begin
                Frames[FrameIndex].DecodeFlags4;
                if Frames[FrameIndex].GroupingIdentity then begin
                    Stream.Read2(Frames[FrameIndex].GroupIdentifier, 1);
                    Frames[FrameIndex].Size := Frames[FrameIndex].Size - 1;
                end;
                if Frames[FrameIndex].Encrypted then begin
                    Stream.Read2(Frames[FrameIndex].EncryptionMethod, 1);
                    Frames[FrameIndex].Size := Frames[FrameIndex].Size - 1;
                end;
                if Frames[FrameIndex].DataLengthIndicator then begin
                    Stream.Read2(DataLengthIndicatorValueCoded, 4);
                    UnSyncSafe(DataLengthIndicatorValueCoded, 4, Frames[FrameIndex].DataLengthIndicatorValue);
                    Frames[FrameIndex].Size := Frames[FrameIndex].Size - 4;
                end;
                if Root.ParseCoverArts then begin
                    Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
                end else begin
                    if IsSameFrameID(Frames[FrameIndex].ID, 'APIC') then begin
                        Stream.Seek(Frames[FrameIndex].Size, soCurrent);
                    end else begin
                        Frames[FrameIndex].Stream.CopyFrom(Stream, Frames[FrameIndex].Size);
                    end;
                end;
            end;
        end;
        //* Load CHAP and CTOC frames
        if Frames[FrameIndex] is TID3v2FrameCHAP then begin
            (Frames[FrameIndex] as TID3v2FrameCHAP).Parse;
        end;
        if Frames[FrameIndex] is TID3v2FrameCTOC then begin
            (Frames[FrameIndex] as TID3v2FrameCTOC).Parse;
        end;
    except
        //*
    end;
end;

function TID3v2Frames.AddFrame(FrameID: TFrameID): Integer;
var
    NewFrame: TID3v2Frame;
begin
    if Frames.Count = Frames.Capacity then begin
        Frames.Capacity := Frames.Capacity + ID3V2LIBRARY_FRAMES_GROWBY;
    end;
    if IsSameFrameID(FrameID, 'CHAP') then begin
        NewFrame := TID3v2FrameCHAP.Create(Self);
        (NewFrame as TID3v2FrameCHAP).SubFrames.Root := Root;
    end else if IsSameFrameID(FrameID, 'CTOC') then begin
        NewFrame := TID3v2FrameCTOC.Create(Self);
        (NewFrame as TID3v2FrameCTOC).SubFrames.Root := Root;
    end else begin
        NewFrame := TID3v2Frame.Create(Self);
    end;
    NewFrame.ID := FrameID;
    Result := Frames.Add(NewFrame);
end;

function TID3v2Frames.AddFrame(const FrameID: String): Integer;
var
    ID: TFrameID;
begin
    AnsiStringToPAnsiChar(FrameID, ID);
    Result := AddFrame(ID);
end;

function TID3v2Frames.AddFrameEx(const FrameID: String): TID3v2Frame;
var
    ID: TFrameID;
begin
    AnsiStringToPAnsiChar(FrameID, ID);
    Result := Frames[AddFrame(ID)];
end;

procedure TID3v2Frames.AssignFrames(Source: TID3v2Frames);
var
    i: Integer;
    NewFrameIndex: Integer;
begin
    Self.Clear;
    for i := 0 to Source.Frames.Count - 1 do begin
        NewFrameIndex := Self.AddFrame(Source.Frames[i].ID);
        Self.Frames[NewFrameIndex].Assign(Source.Frames[i]);
    end;
end;

function TID3v2Frames.DeleteFrame(FrameIndex: Integer): Boolean;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Free;
    Frames.Delete(FrameIndex);
    Result := True;
end;

function TID3v2Frames.DeleteFrame(FrameID: TFrameID): Boolean;
var
    Index: Integer;
begin
    Result := False;
    Index := FrameExists(FrameID);
    if (Index >= FrameCount)
    OR (Index < 0)
    then begin
        Exit;
    end;
    Result := DeleteFrame(Index);
end;

function TID3v2Frames.DeleteFrame(const FrameID: String): Boolean;
var
    ID: TFrameID;
begin
    AnsiStringToPAnsiChar(FrameID, ID);
    Result := DeleteFrame(ID);
end;

function TID3v2Frames.Convertv2PICtoAPIC(FrameIndex: Integer): Boolean;
var
    StrMIMEType: String;
    DataByte: Byte;
    Encoding: Byte;
    MIMEType: String;
    Description: String;
    CoverType: Byte;
    PictureStream: TStream;
    i: Integer;
begin
    Result := False;
    MIMEType := '';
    Description := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);

        PictureStream := TMemoryStream.Create;

        try
            //* Get encoding
            Encoding := Frames[FrameIndex].Stream.ReadEncoding;

            //* Get MIME type
            StrMIMEType := '';
            for i := 0 to 2 do begin
                Frames[FrameIndex].Stream.Read2(DataByte, 1);
                if DataByte <> 0 then begin
                    StrMIMEType := StrMIMEType + Char(DataByte);
                end;
            end;

            //* Get picture type
            Frames[FrameIndex].Stream.Read2(DataByte, 1);
            CoverType := DataByte;

            //* Get description
            Description := Frames[FrameIndex].Stream.ReadText(Encoding);

            //* Get binary picture data
            PictureStream.Seek(0, soBeginning);
            try
                PictureStream.CopyFrom(Frames[FrameIndex].Stream, Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position);
                PictureStream.Seek(0, soBeginning);
            except

            end;

            //* Set results
            MIMEType := StrMIMEType;

            MIMEType := WideUpperCase(MIMEType);
            if MIMEType = 'JPG' then begin
                MIMEType := 'image/jpeg';
            end;
            if MIMEType = 'PNG' then begin
                MIMEType := 'image/png';
            end;
            if MIMEType = 'GIF' then begin
                MIMEType := 'image/gif';
            end;
            if MIMEType = 'BMP' then begin
                MIMEType := 'image/bmp';
            end;

             Result := SetCoverPictureFromStream(FrameIndex, Description, PictureStream, MIMEType, CoverType);

        finally
            FreeAndNil(PictureStream);
        end;

    except
        //*
    end;
end;

function TID3v2Frames.Convertv2Tov3(FrameIndex: Integer): Boolean;
var
    V2FrameID: String;
begin
    Result := False;
    V2FrameID := Char(Frames[FrameIndex].ID[0]) + Char(Frames[FrameIndex].ID[1]) + Char(Frames[FrameIndex].ID[2]);
    if V2FrameID = 'PIC' then begin
        ConvertString2FrameID('APIC', Frames[FrameIndex].ID);
        Convertv2PICtoAPIC(FrameIndex);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'BUF' then begin
        ConvertString2FrameID('RBUF', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'CNT' then begin
        ConvertString2FrameID('PCNT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'COM' then begin
        ConvertString2FrameID('COMM', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'CRA' then begin
        ConvertString2FrameID('AENC', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'ETC' then begin
        ConvertString2FrameID('ETCO', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'EQU' then begin
        ConvertString2FrameID('EQUA', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'GEO' then begin
        ConvertString2FrameID('GEOB', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'GP1' then begin
        ConvertString2FrameID('GRP1', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'IPL' then begin
        ConvertString2FrameID('TIPL', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'LNK' then begin
        ConvertString2FrameID('LINK', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'MCI' then begin
        ConvertString2FrameID('MCDI', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'MLL' then begin
        ConvertString2FrameID('MLLT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'MVI' then begin
        ConvertString2FrameID('MVIN', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'MVN' then begin
        ConvertString2FrameID('MVNM', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'POP' then begin
        ConvertString2FrameID('POPM', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'REV' then begin
        ConvertString2FrameID('RVRB', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'RVA' then begin
        ConvertString2FrameID('RVAD', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'SLT' then begin
        ConvertString2FrameID('SYLT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'STC' then begin
        ConvertString2FrameID('SYTC', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TAL' then begin
        ConvertString2FrameID('TALB', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TBP' then begin
        ConvertString2FrameID('TBPM', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TCM' then begin
        ConvertString2FrameID('TCOM', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TCO' then begin
        ConvertString2FrameID('TCON', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TCP' then begin
        ConvertString2FrameID('TCMP', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TCR' then begin
        ConvertString2FrameID('TCOP', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TDA' then begin
        ConvertString2FrameID('TDAT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TDY' then begin
        ConvertString2FrameID('TDLY', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TEN' then begin
        ConvertString2FrameID('TENC', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TFT' then begin
        ConvertString2FrameID('TFLT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TIM' then begin
        ConvertString2FrameID('TIME', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TKE' then begin
        ConvertString2FrameID('TKEY', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TLA' then begin
        ConvertString2FrameID('TLAN', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TLE' then begin
        ConvertString2FrameID('TLEN', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TMT' then begin
        ConvertString2FrameID('TMED', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TOA' then begin
        ConvertString2FrameID('TOPE', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TOF' then begin
        ConvertString2FrameID('TOFN', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TOL' then begin
        ConvertString2FrameID('TOLY', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TOR' then begin
        ConvertString2FrameID('TORY', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TOT' then begin
        ConvertString2FrameID('TOAL', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TP1' then begin
        ConvertString2FrameID('TPE1', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TP2' then begin
        ConvertString2FrameID('TPE2', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TP3' then begin
        ConvertString2FrameID('TPE3', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TP4' then begin
        ConvertString2FrameID('TPE4', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TPA' then begin
        ConvertString2FrameID('TPOS', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TPB' then begin
        ConvertString2FrameID('TPUB', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TRC' then begin
        ConvertString2FrameID('TSRC', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TRD' then begin
        ConvertString2FrameID('TRDA', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TRK' then begin
        ConvertString2FrameID('TRCK', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TRN' then begin
        ConvertString2FrameID('TRSN', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TRO' then begin
        ConvertString2FrameID('TRSO', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TS2' then begin
        ConvertString2FrameID('TSO2', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TSA' then begin
        ConvertString2FrameID('TSOA', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TSC' then begin
        ConvertString2FrameID('TSOC', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TSI' then begin
        ConvertString2FrameID('TSIZ', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TSP' then begin
        ConvertString2FrameID('TSOP', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TSS' then begin
        ConvertString2FrameID('TSSE', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TST' then begin
        ConvertString2FrameID('TSOT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TT1' then begin
        ConvertString2FrameID('TIT1', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TT2' then begin
        ConvertString2FrameID('TIT2', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TT3' then begin
        ConvertString2FrameID('TIT3', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TXT' then begin
        ConvertString2FrameID('TEXT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TXX' then begin
        ConvertString2FrameID('TXXX', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'TYE' then begin
        ConvertString2FrameID('TYER', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'UFI' then begin
        ConvertString2FrameID('UFID', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'ULT' then begin
        ConvertString2FrameID('USLT', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WAF' then begin
        ConvertString2FrameID('WOAF', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WAR' then begin
        ConvertString2FrameID('WOAR', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WAS' then begin
        ConvertString2FrameID('WOAS', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WCM' then begin
        ConvertString2FrameID('WCOM', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WCP' then begin
        ConvertString2FrameID('WCOP', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WPB' then begin
        ConvertString2FrameID('WPUB', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
    if V2FrameID = 'WXX' then begin
        ConvertString2FrameID('WXXX', Frames[FrameIndex].ID);
        Result := True;
        Exit;
    end;
end;

function TID3v2Frames.FrameExists(FrameID: TFrameID): Integer;
var
    i: Integer;
begin
    Result := - 1;
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            Result := i;
            Break;
        end;
    end;
end;

function TID3v2Frames.FrameExists(const FrameID: String): Integer;
var
    i: Integer;
    TempFrameID: TFrameID;
begin
    Result := - 1;
    ConvertString2FrameID(FrameID, TempFrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(TempFrameID, Frames[i].ID) then begin
            Result := i;
            Break;
        end;
    end;
end;

function TID3v2Frames.FrameTypeCount(FrameID: TFrameID): Integer;
var
    i: Integer;
begin
    Result := 0;
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            Inc(Result);
        end;
    end;
end;

function TID3v2Frames.FrameTypeCount(const FrameID: String): Integer;
var
    ID: TFrameID;
begin
    ConvertString2FrameID(FrameID, ID);
    Result := FrameTypeCount(ID);
end;

function TID3v2Frames.CoverArtCount: Integer;
var
    i: Integer;
begin
    Result := 0;
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, 'APIC') then begin
            Inc(Result);
        end;
    end;
end;

function TID3v2Tag.SaveTagToStream(Stream: TStream; PaddingSizeToWrite: Integer = 0): Integer;
begin
    try
        if MajorVersion = 2 then begin
            MajorVersion := 3;
        end;
        if (MajorVersion < 3)
        OR (MajorVersion > 4)
        then begin
            Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_VERSION;
            Exit;
        end;
        //SetAllFrameDataStreams;
        PaddingSize := PaddingSizeToWrite;
        EncodeSize;
        EncodeFlags;
        //* EncodeExtendedHeader;
        Result := WriteAllHeaders(Stream);
        if Result <> ID3V2LIBRARY_SUCCESS then begin
            Exit;
        end;
        Result := WriteAllFrames(Stream);
        if Result <> ID3V2LIBRARY_SUCCESS then begin
            Exit;
        end;
        if PaddingSizeToWrite > 0 then begin
            Result := WritePadding(Stream, PaddingSize);
        end;
        if Result <> ID3V2LIBRARY_SUCCESS then begin
            Exit;
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function TID3v2Tag.SaveToFile(FileName: String; Buffered: Boolean = True): Integer;
var
    FileDateTime: TDateTime;
    FileStream: TFileStream;
    TagStream: TStream;
    NewTagStream: TStream;
    TagSizeInExistingStream: Cardinal;
    TagCodedSizeInExistingStream: Cardinal;
    WriteTagTotalSize: Cardinal;
    //NeedToCopyExistingStream: Boolean;
    PaddingNeededToWrite: Integer;
    NewFile: Boolean;
    ExclusiveAccess: Boolean;
    BufferedStream: TBufferedStream;
    Action: Integer;

    function CheckTag: Boolean;
    var
        PreviousPosition: Int64;
    begin
        PreviousPosition := TagStream.Position;
        if ID3v2ValidTag(TagStream) then begin
            //* Skip version data and flags
            TagStream.Seek(3, soCurrent);
            TagStream.Read2(TagCodedSizeInExistingStream, 4);
            UnSyncSafe(TagCodedSizeInExistingStream, 4, TagSizeInExistingStream);
            //* Add header size to size
            TagSizeInExistingStream := TagSizeInExistingStream + 10;
            if (WriteTagTotalSize > TagSizeInExistingStream)
            OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
            then begin
                //NeedToCopyExistingStream := True;
                NewFile := True;
            end;
            TagStream.Seek(PreviousPosition, soBeginning);
            Result := True;
        end else begin
            Result := False;
        end;
    end;

begin
    TagStream := nil;
    FileStream := nil;
    BufferedStream := nil;
    NewTagStream := nil;
    NewFile := False;
    Action := ID3v2TLOFEANNONE;
    try
        try
            if FrameCount = 0 then begin
                Result := ID3V2LIBRARY_ERROR_EMPTY_TAG;
                Exit;
            end;
            if MajorVersion = 2 then begin
                MajorVersion := 3;
            end;

            case WriteVersion of
                //sfid3v2NoChange //* No change
                sfid3v23: MajorVersion := 3;
                sfid3v24: MajorVersion := 4;
            end;

            if CalculateTotalFramesSize = 0 then begin
                Result := ID3V2LIBRARY_ERROR_EMPTY_FRAMES;
                Exit;
            end;
            if PreserveFileDateTime then begin
                FileAge(FileName, FileDateTime);
            end;

            if FileIsReadOnly(FileName) then begin
                if Assigned(ID3v2LibraryOpenFileExceptionCallback) then begin
                    ID3v2LibraryOpenFileExceptionCallback(FileName, nil, Action);
                    case Action of
                        ID3v2TLOFEANNONE: begin
                            Result := ID3V2LIBRARY_ERROR_FILE_READ_ONLY;
                            Exit;
                        end;
                        ID3v2TLOFEANDEPROTECT, ID3v2TLOFEANDEPROTECTANDRESTORE: begin
                            FileSetReadOnly(FileName, False);
                        end;
                    end;
                end else begin
                    Result := ID3V2LIBRARY_ERROR_FILE_READ_ONLY;
                    Exit;
                end;
            end;

            if NOT FileExists(FileName) then begin
                FileStream := TFileStream.Create(FileName, fmCreate OR fmShareDenyWrite);
                ExclusiveAccess := True;
            end else begin
                try
                    FileStream := TFileStream.Create(FileName, fmOpenReadWrite OR fmShareExclusive);
                    ExclusiveAccess := True;
                    FreeAndNil(FileStream);
                except
                    ExclusiveAccess := False;
                end;
                try
                    FileStream := TFileStream.Create(FileName, fmOpenReadWrite OR fmShareDenyWrite);
                except
                    Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
                    Exit;
                end;
            end;
            //* Assign the needed TStream to process
            if Buffered then begin
                BufferedStream := TBufferedStream.Create(FileStream);
                TagStream := BufferedStream;
            end else begin
                TagStream := FileStream;
            end;
            //NeedToCopyExistingStream := False;
            //SetAllFrameDataStreams;
            WriteTagTotalSize := CalculateTagSize(0);
            try
                if CheckRIFF(TagStream) then begin
                    if NOT ValidRIFF(TagStream) then begin
                        Result := ID3V2LIBRARY_ERROR_CORRUPT;
                        Exit;
                    end;
                    if SeekRIFF(TagStream) > 0 then begin
                        if CheckTag then begin
                            if (WriteTagTotalSize > TagSizeInExistingStream)
                            OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
                            then begin
                                TagStream.Seek(0, soBeginning);
                                //* Update size datas
                                Result := RIFFUpdateID3v2(FileName, TagStream, WriteTagTotalSize, TagSizeInExistingStream, PaddingToWrite);
                                if Result = ID3V2LIBRARY_SUCCESS then begin
                                    Result := SaveTagToStream(TagStream, PaddingToWrite);
                                end;
                                Exit;
                            end else begin
                                PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                                //* Just write it
                                Result := SaveTagToStream(TagStream, PaddingNeededToWrite);
                                Exit;
                            end;
                        //* Need to create new Tag
                        end else begin
                            TagStream.Seek(0, soBeginning);
                            Result := RIFFCreateID3v2(FileName, TagStream, WriteTagTotalSize, PaddingToWrite);
                            if Result = ID3V2LIBRARY_SUCCESS then begin
                                Result := SaveTagToStream(TagStream, PaddingToWrite);
                            end;
                            Exit;
                        end;
                    //* Need to create new Tag
                    end else begin
                        TagStream.Seek(0, soBeginning);
                        Result := RIFFCreateID3v2(FileName, TagStream, WriteTagTotalSize, PaddingToWrite);
                        if Result = ID3V2LIBRARY_SUCCESS then begin
                            Result := SaveTagToStream(TagStream, PaddingToWrite);
                        end;
                        Exit;
                    end;
                end else begin
                    TagStream.Seek(0, soBeginning);
                    if CheckAIFF(TagStream) then begin
                        if SeekAIFF(TagStream) > 0 then begin
                            if CheckTag then begin
                                if (WriteTagTotalSize > TagSizeInExistingStream)
                                OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
                                then begin
                                    TagStream.Seek(0, soBeginning);
                                    //* Update size datas
                                    Result := AIFFUpdateID3v2(FileName, TagStream, WriteTagTotalSize, TagSizeInExistingStream, PaddingToWrite);
                                    if Result = ID3V2LIBRARY_SUCCESS then begin
                                        Result := SaveTagToStream(TagStream, PaddingToWrite);
                                    end;
                                    Exit;
                                end else begin
                                    PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                                    //* Just write it
                                    Result := SaveTagToStream(TagStream, PaddingNeededToWrite);
                                    Exit;
                                end;
                            //* Need to create new Tag
                            end else begin
                                TagStream.Seek(0, soBeginning);
                                Result := AIFFCreateID3v2(FileName, TagStream, WriteTagTotalSize, PaddingToWrite);
                                if Result = ID3V2LIBRARY_SUCCESS then begin
                                    Result := SaveTagToStream(TagStream, PaddingToWrite);
                                end;
                                Exit;
                            end;
                        end else begin
                            TagStream.Seek(0, soBeginning);
                            Result := AIFFCreateID3v2(FileName, TagStream, WriteTagTotalSize, PaddingToWrite);
                            if Result = ID3V2LIBRARY_SUCCESS then begin
                                Result := SaveTagToStream(TagStream, PaddingToWrite);
                            end;
                            Exit;
                        end;
                    end else begin
                        TagStream.Seek(0, soBeginning);
                        if CheckRF64(TagStream) then begin
                            if NOT ValidRF64(TagStream) then begin
                                Result := ID3V2LIBRARY_ERROR_CORRUPT;
                                Exit;
                            end;
                            if SeekRF64(TagStream) > 0 then begin
                                if CheckTag then begin
                                    if (WriteTagTotalSize > TagSizeInExistingStream)
                                    OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
                                    then begin
                                        TagStream.Seek(0, soBeginning);
                                        //* Update size datas
                                        Result := RF64UpdateID3v2(FileName, TagStream, WriteTagTotalSize, TagSizeInExistingStream, PaddingToWrite);
                                        if Result = ID3V2LIBRARY_SUCCESS then begin
                                            Result := SaveTagToStream(TagStream, PaddingToWrite);
                                        end;
                                        Exit;
                                    end else begin
                                        PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                                        //* Just write it
                                        Result := SaveTagToStream(TagStream, PaddingNeededToWrite);
                                        Exit;
                                    end;
                                //* Need to create new Tag
                                end else begin
                                    TagStream.Seek(0, soBeginning);
                                    Result := RF64CreateID3v2(FileName, TagStream, WriteTagTotalSize, PaddingToWrite);
                                    if Result = ID3V2LIBRARY_SUCCESS then begin
                                        Result := SaveTagToStream(TagStream, PaddingToWrite);
                                    end;
                                    Exit;
                                end;
                            end else begin
                                TagStream.Seek(0, soBeginning);
                                Result := RF64CreateID3v2(FileName, TagStream, WriteTagTotalSize, PaddingToWrite);
                                if Result = ID3V2LIBRARY_SUCCESS then begin
                                    Result := SaveTagToStream(TagStream, PaddingToWrite);
                                end;
                                Exit;
                            end;
                        end else begin
                            //* DSF
                            TagStream.Seek(0, soBeginning);
                            if CheckDSF(TagStream) then begin
                                if NOT ValidDSF(TagStream) then begin
                                    Result := ID3V2LIBRARY_ERROR_CORRUPT;
                                    Exit;
                                end;
                                Result := SaveDSF(TagStream, WriteTagTotalSize);
                                Exit;
                            end else begin
                                //* DFF
                                TagStream.Seek(0, soBeginning);
                                if CheckDFF(TagStream) then begin
                                    if NOT ValidDFF(TagStream) then begin
                                        Result := ID3V2LIBRARY_ERROR_CORRUPT;
                                        Exit;
                                    end;
                                    Result := SaveDFF(TagStream, WriteTagTotalSize);
                                    Exit;
                                end else begin
                                    //* Normal file (MP3) - tag at start
                                    TagStream.Seek(0, soBeginning);
                                    if NOT CheckTag then begin
                                        TagSizeInExistingStream := 0;
                                        //NeedToCopyExistingStream := True;
                                        NewFile := True;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            except
                Result := ID3V2LIBRARY_ERROR_READING_FILE;
                Exit;
            end;

            if (TagSizeInExistingStream = 0)
            OR NewFile
            then begin
                PaddingNeededToWrite := PaddingToWrite;
            end else begin
                //* Calculate padding here
                PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                if PaddingNeededToWrite < 0 then begin
                    PaddingNeededToWrite := PaddingToWrite;
                end;
            end;

            if NewFile then begin
                if NOT ExclusiveAccess then begin
                    Result := ID3V2LIBRARY_ERROR_NEED_EXCLUSIVE_ACCESS;
                    Exit;
                end;
                {$IFDEF NEXTGEN}
                if Assigned(BufferedStream) then begin
                    BufferedStream.DisposeOf;
                    BufferedStream := nil;
                end;                
                {$ELSE}
                if Assigned(BufferedStream) then begin
                    FreeAndNil(BufferedStream);
                end;
                {$ENDIF}
                NewTagStream := TFileStream.Create(FileName + '.tmp', fmCreate OR fmShareExclusive);
                if Buffered then begin
		            BufferedStream := TBufferedStream.Create(NewTagStream);
		            TagStream := BufferedStream;
		        end else begin
		            TagStream := NewTagStream;
		        end;
                try
                    Result := SaveTagToStream(TagStream, PaddingNeededToWrite);
		            {$IFDEF NEXTGEN}
		            if Assigned(BufferedStream) then begin
			            BufferedStream.DisposeOf;
			            BufferedStream := nil;
		            end;
		            {$ELSE}
		            if Assigned(BufferedStream) then begin
			            FreeAndNil(BufferedStream);
		            end;
		            {$ENDIF}
                    FileStream.Seek(TagSizeInExistingStream, soBeginning);
                    NewTagStream.Seek(0, soEnd);
                    NewTagStream.CopyFrom(FileStream, FileStream.Size - TagSizeInExistingStream);
                    {$IFDEF NEXTGEN}
                    if Assigned(FileStream) then begin
                        FileStream.DisposeOf;
                        FileStream := nil;
                    end;
                    if Assigned(NewTagStream) then begin
                        NewTagStream.DisposeOf;
                        NewTagStream := nil;
                    end;
                    {$ELSE}
                    if Assigned(FileStream) then begin
                        FreeAndNil(FileStream);
                    end;
                    if Assigned(NewTagStream) then begin
                        FreeAndNil(NewTagStream);
                    end;
                    {$ENDIF}
                    if SysUtils.DeleteFile(FileName) then begin
                        if RenameFile(FileName + '.tmp', FileName) then begin
                            Result := ID3V2LIBRARY_SUCCESS;
                            Exit;
                        end;
                    end else begin
                        SysUtils.DeleteFile(FileName + '.tmp');
                        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                    end;
                except
                    Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                    Exit;
                end;
            end else begin
                try
                    Result := SaveTagToStream(TagStream, PaddingNeededToWrite);
                except
                    Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                    Exit;
                end;
            end;
        finally
            if Assigned(BufferedStream) then begin
                FreeAndNil(BufferedStream);
            end;
            if Assigned(FileStream) then begin
                FreeAndNil(FileStream);
            end;
            if Assigned(NewTagStream) then begin
                FreeAndNil(NewTagStream);
            end;
            if PreserveFileDateTime then begin
                FileSetDate(FileName, DateTimeToFileDate(FileDateTime));
            end;
            if Action = ID3v2TLOFEANDEPROTECTANDRESTORE then begin
                FileSetReadOnly(FileName, True);
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function TID3v2Tag.SaveToStream(Stream: TStream; Buffered: Boolean = True): Integer;
var
    NewTagStream: TStream;
    TagSizeInExistingStream: Cardinal;
    TagCodedSizeInExistingStream: Cardinal;
    WriteTagTotalSize: Cardinal;
    //NeedToCopyExistingStream: Boolean;
    PaddingNeededToWrite: Integer;
    NewFile: Boolean;
    ExclusiveAccess: Boolean;
    //FileName: String;
    LStream: TStream;

    function CheckTag: Boolean;
    var
        PreviousPosition: Int64;
    begin
        PreviousPosition := LStream.Position;
        if ID3v2ValidTag(LStream) then begin
            //* Skip version data and flags
            LStream.Seek(3, soCurrent);
            LStream.Read2(TagCodedSizeInExistingStream, 4);
            UnSyncSafe(TagCodedSizeInExistingStream, 4, TagSizeInExistingStream);
            //* Add header size to size
            TagSizeInExistingStream := TagSizeInExistingStream + 10;
            if (WriteTagTotalSize > TagSizeInExistingStream)
            OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
            then begin
                //NeedToCopyExistingStream := True;
                NewFile := True;
            end;
            LStream.Seek(PreviousPosition, soBeginning);
            Result := True;
        end else begin
            Result := False;
        end;
    end;

begin
    NewTagStream := nil;
    NewFile := False;
    ExclusiveAccess := True;
    //FileName := '';
    try
        if Buffered then
            LStream := TBufferedStream.Create(Stream)
        else
            LStream := Stream;
        try
            if FrameCount = 0 then begin
                Result := ID3V2LIBRARY_ERROR_EMPTY_TAG;
                Exit;
            end;
            if MajorVersion = 2 then begin
                MajorVersion := 3;
            end;

            case WriteVersion of
                //sfid3v2NoChange //* No change
                sfid3v23: MajorVersion := 3;
                sfid3v24: MajorVersion := 4;
            end;

            if CalculateTotalFramesSize = 0 then begin
                Result := ID3V2LIBRARY_ERROR_EMPTY_FRAMES;
                Exit;
            end;
            LStream.Seek(0, soBeginning);
            //NeedToCopyExistingStream := False;
            WriteTagTotalSize := CalculateTagSize(0);
            try
                if CheckRIFF(LStream) then begin
                    if NOT ValidRIFF(LStream) then begin
                        Result := ID3V2LIBRARY_ERROR_CORRUPT;
                        Exit;
                    end;
                    if SeekRIFF(LStream) > 0 then begin
                        if CheckTag then begin
                            if (WriteTagTotalSize > TagSizeInExistingStream)
                            OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
                            then begin
                                LStream.Seek(0, soBeginning);
                                //* Update size datas
                                Result := RIFFUpdateID3v2('', LStream, WriteTagTotalSize, TagSizeInExistingStream, PaddingToWrite);
                                if Result = ID3V2LIBRARY_SUCCESS then begin
                                    Result := SaveTagToStream(LStream, PaddingToWrite);
                                end;
                                Exit;
                            end else begin
                                PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                                //* Just write it
                                Result := SaveTagToStream(LStream, PaddingNeededToWrite);
                                Exit;
                            end;
                        //* Need to create new Tag
                        end else begin
                            LStream.Seek(0, soBeginning);
                            Result := RIFFCreateID3v2('', LStream, WriteTagTotalSize, PaddingToWrite);
                            if Result = ID3V2LIBRARY_SUCCESS then begin
                                Result := SaveTagToStream(LStream, PaddingToWrite);
                            end;
                            Exit;
                        end;
                    //* Need to create new Tag
                    end else begin
                        LStream.Seek(0, soBeginning);
                        Result := RIFFCreateID3v2('', LStream, WriteTagTotalSize, PaddingToWrite);
                        if Result = ID3V2LIBRARY_SUCCESS then begin
                            Result := SaveTagToStream(LStream, PaddingToWrite);
                        end;
                        Exit;
                    end;
                end else begin
                    LStream.Seek(0, soBeginning);
                    if CheckAIFF(LStream) then begin
                        if SeekAIFF(LStream) > 0 then begin
                            if CheckTag then begin
                                if (WriteTagTotalSize > TagSizeInExistingStream)
                                OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
                                then begin
                                    LStream.Seek(0, soBeginning);
                                    //* Update size datas
                                    Result := AIFFUpdateID3v2('', LStream, WriteTagTotalSize, TagSizeInExistingStream, PaddingToWrite);
                                    if Result = ID3V2LIBRARY_SUCCESS then begin
                                        Result := SaveTagToStream(LStream, PaddingToWrite);
                                    end;
                                    Exit;
                                end else begin
                                    PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                                    //* Just write it
                                    Result := SaveTagToStream(LStream, PaddingNeededToWrite);
                                    Exit;
                                end;
                            //* Need to create new Tag
                            end else begin
                                LStream.Seek(0, soBeginning);
                                Result := AIFFCreateID3v2('', LStream, WriteTagTotalSize, PaddingToWrite);
                                if Result = ID3V2LIBRARY_SUCCESS then begin
                                    Result := SaveTagToStream(LStream, PaddingToWrite);
                                end;
                                Exit;
                            end;
                        end else begin
                            LStream.Seek(0, soBeginning);
                            Result := AIFFCreateID3v2('', LStream, WriteTagTotalSize, PaddingToWrite);
                            if Result = ID3V2LIBRARY_SUCCESS then begin
                                Result := SaveTagToStream(LStream, PaddingToWrite);
                            end;
                            Exit;
                        end;
                    end else begin
                        LStream.Seek(0, soBeginning);
                        if CheckRF64(LStream) then begin
                            if NOT ValidRF64(LStream) then begin
                                Result := ID3V2LIBRARY_ERROR_CORRUPT;
                                Exit;
                            end;
                            if SeekRF64(LStream) > 0 then begin
                                if CheckTag then begin
                                    if (WriteTagTotalSize > TagSizeInExistingStream)
                                    OR (TagSizeInExistingStream - WriteTagTotalSize > PaddingToWrite)
                                    then begin
                                        LStream.Seek(0, soBeginning);
                                        //* Update size datas
                                        Result := RF64UpdateID3v2('', LStream, WriteTagTotalSize, TagSizeInExistingStream, PaddingToWrite);
                                        if Result = ID3V2LIBRARY_SUCCESS then begin
                                            Result := SaveTagToStream(LStream, PaddingToWrite);
                                        end;
                                        Exit;
                                    end else begin
                                        PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                                        //* Just write it
                                        Result := SaveTagToStream(LStream, PaddingNeededToWrite);
                                        Exit;
                                    end;
                                //* Need to create new Tag
                                end else begin
                                    LStream.Seek(0, soBeginning);
                                    Result := RF64CreateID3v2('', LStream, WriteTagTotalSize, PaddingToWrite);
                                    if Result = ID3V2LIBRARY_SUCCESS then begin
                                        Result := SaveTagToStream(LStream, PaddingToWrite);
                                    end;
                                    Exit;
                                end;
                            end else begin
                                LStream.Seek(0, soBeginning);
                                Result := RF64CreateID3v2('', LStream, WriteTagTotalSize, PaddingToWrite);
                                if Result = ID3V2LIBRARY_SUCCESS then begin
                                    Result := SaveTagToStream(LStream, PaddingToWrite);
                                end;
                                Exit;
                            end;
                        end else begin
                            LStream.Seek(0, soBeginning);
                            if CheckDSF(LStream) then begin
                                if NOT ValidDSF(LStream) then begin
                                    Result := ID3V2LIBRARY_ERROR_CORRUPT;
                                    Exit;
                                end;
                                Result := SaveDSF(LStream, WriteTagTotalSize);
                                Exit;
                            end else begin
                                LStream.Seek(0, soBeginning);
                                if CheckDFF(LStream) then begin
                                    if NOT ValidDFF(LStream) then begin
                                        Result := ID3V2LIBRARY_ERROR_CORRUPT;
                                        Exit;
                                    end;
                                    Result := SaveDFF(LStream, WriteTagTotalSize);
                                    Exit;
                                end else begin
                                    //* Normal file (MP3) - tag at start
                                    LStream.Seek(0, soBeginning);
                                    if NOT CheckTag then begin
                                        TagSizeInExistingStream := 0;
                                        //NeedToCopyExistingStream := True;
                                        NewFile := True;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            except
                Result := ID3V2LIBRARY_ERROR_READING_FILE;
                Exit;
            end;

            if (TagSizeInExistingStream = 0)
            OR NewFile
            then begin
                PaddingNeededToWrite := PaddingToWrite;
            end else begin
                //* Calculate padding here
                PaddingNeededToWrite := TagSizeInExistingStream - WriteTagTotalSize;
                if PaddingNeededToWrite < 0 then begin
                    PaddingNeededToWrite := PaddingToWrite;
                end;
            end;

            if NewFile then begin
                if NOT ExclusiveAccess then begin
                    Result := ID3V2LIBRARY_ERROR_NEED_EXCLUSIVE_ACCESS;
                    Exit;
                end;
                NewTagStream := TMemoryStream.Create;
                try
                    SaveTagToStream(NewTagStream, PaddingNeededToWrite);
                    LStream.Seek(TagSizeInExistingStream, soBeginning);
                    NewTagStream.CopyFrom(LStream, LStream.Size - TagSizeInExistingStream);
                    LStream.Size := 0;
                    LStream.Seek(0, soBeginning);
                    LStream.CopyFrom(NewTagStream, 0);
                    Result := ID3V2LIBRARY_SUCCESS;
                except
                    Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                    Exit;
                end;
            end else begin
                try
                    Result := SaveTagToStream(LStream, PaddingNeededToWrite);
                except
                    Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                    Exit;
                end;
            end;
        finally
            LStream.Seek(0, soBeginning);
            if Assigned(NewTagStream) then begin
                FreeAndNil(NewTagStream);
            end;
            if Buffered then
                FreeAndNil(LStream);
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function TID3v2Frames.GetText(const FrameID: String; ReturnNativeText: Boolean = False): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetText(Index, ReturnNativeText);
end;

function TID3v2Frames.GetText(FrameIndex: Integer; ReturnNativeText: Boolean = False): String;
var
    Encoding: Byte;
    BigEndian: Boolean;
    NilFound: Boolean;
begin
    Result := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        BigEndian := False;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        if ReturnNativeText and (Encoding = 3) then
            Encoding := 4;
        //* Get the content
        while True do begin
            Result := Result + Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian, @NilFound);
            if (NOT NilFound)
            OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size)
            then begin
                Break;
            end;
            Result := Result + #13#10;
        end;
        if Root.TrimText then begin
            Result := Trim(Result);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.SetText(const FrameID: String; const Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetText(Index, Text, TextEncoding);
end;

function TID3v2Frames.GetTextMultiple(FrameIndex: Integer; List: TStrings): Boolean;
begin
    List.Clear;
    List.Text := GetText(FrameIndex);
    Result := List.Text <> '';
end;


function TID3v2Frames.GetTextMultiple(FrameID: String; List: TStrings): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    List.Clear;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetTextMultiple(Index, List);
end;

function TID3v2Frames.SetTextMultiple(FrameIndex: Integer; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
    i: Integer;
    Text: String;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    case TextEncoding of
        teUnicode: begin
            try
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                for i := 0 to List.Count - 1 do begin
                    Text := List[i];
                    Frames[FrameIndex].Stream.Write(PWideChar(Text)^, (Length(Text) + 1) * 2);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            try
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                for i := 0 to List.Count - 1 do begin
                    Text := List[i];
                    Bytes := TEncoding.UTF8.GetBytes(Text);
                    Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[FrameIndex].Stream.Write(Data, 1);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            try
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                for i := 0 to List.Count - 1 do begin
                    Text := List[i];
                    Bytes := TEncoding.ANSI.GetBytes(Text);
                    Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[FrameIndex].Stream.Write(Data, 1);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;
end;

function TID3v2Frames.SetTextMultiple(FrameID: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetTextMultiple(Index, List, TextEncoding);
end;

function TID3v2Frames.SetText(FrameIndex: Integer; const Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    case TextEncoding of
        teUnicode: begin
            try
                Frames[FrameIndex].Stream.Clear;
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(PWideChar(Text)^, (Length(Text) + 1) * 2);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            Bytes := TEncoding.UTF8.GetBytes(Text);
            try
                Frames[FrameIndex].Stream.Clear;
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            Bytes := TEncoding.ANSI.GetBytes(Text);
            try
                Frames[FrameIndex].Stream.Clear;
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;

end;

function TID3v2Frames.SetRawText(const FrameID: String; const Text: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetRawText(Index, Text);
end;

function TID3v2Frames.SetRawText(FrameIndex: Integer; const Text: String): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Bytes := TEncoding.ANSI.GetBytes(Text);
    try
        Frames[FrameIndex].Stream.Clear;
        Data := $00;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
        Data := $00;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetComment(FrameID: String; var LanguageID: TLanguageID; var Description: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    ClearLanguageID(LanguageID);
    Description := '';
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetComment(Index, LanguageID, Description);
end;

function TID3v2Frames.FindCommentByDescription(Description: String; var LanguageID: TLanguageID; var Comment: String): Integer;
var
    FrameID: TFrameID;
    i: Integer;
    GetDescription: String;
    GetLanguageID: TLanguageID;
    GetContent: String;
begin
    Result := - 1;
    AnsiStringToPAnsiChar('COMM', FrameID);
    ClearLanguageID(LanguageID);
    Comment := '';
    ClearLanguageID(GetLanguageID);
    GetDescription := '';
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            GetContent := GetComment(i, GetLanguageID, GetDescription);
            if WideUpperCase(GetDescription) = WideUpperCase(Description) then begin
                LanguageID := GetLanguageID;
                Comment := GetContent;
                Result := i;
                Break;
            end;
        end;
    end;
end;

function TID3v2Frames.SetCommentByDescription(Description: String; LanguageID: TLanguageID; Comment: String): Boolean;
var
    Index: Integer;
    FrameID: TFrameID;
    i: Integer;
    GetDescription: String;
    GetLanguageID: TLanguageID;
    //GetContent: String;
begin
    Index := - 1;
    AnsiStringToPAnsiChar('COMM', FrameID);
    ClearLanguageID(GetLanguageID);
    GetDescription := '';
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            {GetContent :=} GetComment(i, GetLanguageID, GetDescription);
            if WideUpperCase(GetDescription) = WideUpperCase(Description) then begin
                Index := i;
                Break;
            end;
        end;
    end;
    if Index = - 1 then begin
        Index := AddFrame('COMM');
    end;
    Result := SetComment(Index, Comment, LanguageID, Description);
end;

function TID3v2Frames.GetComment(FrameIndex: Integer; var LanguageID: TLanguageID; var Description: String): String;
begin
    Result := GetContent(FrameIndex, LanguageID, Description);
end;

function TID3v2Frames.GetContent(FrameID: String; var LanguageID: TLanguageID; var Description: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    ClearLanguageID(LanguageID);
    Description := '';
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetComment(Index, LanguageID, Description);
end;

function TID3v2Frames.GetContent(FrameIndex: Integer; var LanguageID: TLanguageID; var Description: String): String;
var
    Encoding: Byte;
    BigEndian: Boolean;
begin
    Result := '';
    ClearLanguageID(LanguageID);
    Description := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        BigEndian := False;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get language ID
        Frames[FrameIndex].Stream.Read2(LanguageID[0], 3);
        //* Get description
        Description := Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian);
        //* Get the content
        Result := Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian);
    except
        //*
    end;
end;

function TID3v2Frames.SetComment(FrameID: String; Comment: String; LanguageID: TLanguageID; Description: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetComment(Index, Comment, LanguageID, Description);
end;

function TID3v2Frames.SetComment(FrameIndex: Integer; Comment: String; LanguageID: TLanguageID; Description: String): Boolean;
begin
    Result := SetContent(FrameIndex, Comment, LanguageID, Description);
end;

function TID3v2Frames.SetGEOB(FrameID, MIMEType, EncapsulatedObjectFilename, ContentDescription: String; EncapsulatedObject: TBytes): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetGEOB(Index, MIMEType, EncapsulatedObjectFilename, ContentDescription, EncapsulatedObject);
end;

function TID3v2Frames.SetOWNE(FrameIndex: Integer; PricePaid: String; DateOfPurchase: TDate; Seller: String): Boolean;
var
    DateOfPurchaseStr: String;
    DateOfPurchaseYear: String;
    DateOfPurchaseMonth: String;
    DateOfPurchaseDay: String;
begin
    DateOfPurchaseYear := IntToStr(YearOf(DateOfPurchase));
    DateOfPurchaseMonth := IntToStr(MonthOf(DateOfPurchase));
    DateOfPurchaseDay := IntToStr(DayOf(DateOfPurchase));
    DateOfPurchaseStr := DateOfPurchaseYear;
    if Length(DateOfPurchaseMonth) = 1 then begin
        DateOfPurchaseStr := DateOfPurchaseStr + '0' + DateOfPurchaseMonth;
    end else begin
        DateOfPurchaseStr := DateOfPurchaseStr + DateOfPurchaseMonth;
    end;
    if Length(DateOfPurchaseDay) = 1 then begin
        DateOfPurchaseStr := DateOfPurchaseStr + '0' + DateOfPurchaseDay;
    end else begin
        DateOfPurchaseStr := DateOfPurchaseStr + DateOfPurchaseDay;
    end;
    Result := SetOWNE(FrameIndex, PricePaid, DateOfPurchaseStr, Seller);
end;

function TID3v2Frames.SetOWNE(FrameID, PricePaid: String; DateOfPurchase: TDate; Seller: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetOWNE(Index, PricePaid, DateOfPurchase, Seller);
end;

function TID3v2Frames.SetOWNE(FrameIndex: Integer; PricePaid, DateOfPurchase, Seller: String): Boolean;
var
    DataByte: Byte;
    PricePaidBytes: TBytes;
    DateOfPurchaseBytes: TBytes;
    SellerBytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        //* Encoding (unicode)
        DataByte := $01;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* PricePaid
        PricePaidBytes := TEncoding.ANSI.GetBytes(PricePaid);
        Frames[FrameIndex].Stream.Write(PricePaidBytes[0], Length(PricePaidBytes));
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* DateOfPurchase
        DateOfPurchaseBytes := TEncoding.ANSI.GetBytes(DateOfPurchase);
        Frames[FrameIndex].Stream.Write(DateOfPurchaseBytes[0], Length(DateOfPurchaseBytes));
        //* Seller
        SellerBytes := TEncoding.Unicode.GetBytes(Seller);
        Frames[FrameIndex].Stream.Write(SellerBytes[0], Length(SellerBytes));
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetOWNE(FrameID, PricePaid, DateOfPurchase, Seller: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetOWNE(Index, PricePaid, DateOfPurchase, Seller);
end;

function TID3v2Frames.SetGEOB(FrameIndex: Integer; MIMEType, EncapsulatedObjectFilename, ContentDescription: String; EncapsulatedObject: TBytes): Boolean;
var
    DataByte: Byte;
    DataWord: Word;
    MIMETypeBytes: TBytes;
    FilenameBytes: TBytes;
    ContentDescriptionBytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        //* Encoding (unicode)
        DataByte := $01;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* MIMEType
        MIMETypeBytes := TEncoding.ANSI.GetBytes(MIMEType);
        Frames[FrameIndex].Stream.Write(MIMETypeBytes[0], Length(MIMETypeBytes));
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* Filename
        FilenameBytes := TEncoding.Unicode.GetBytes(EncapsulatedObjectFilename);
        Frames[FrameIndex].Stream.Write(FilenameBytes[0], Length(FilenameBytes));
        DataWord := $0000;
        Frames[FrameIndex].Stream.Write(DataWord, 2);
        //* ContentDescription
        ContentDescriptionBytes := TEncoding.Unicode.GetBytes(ContentDescription);
        Frames[FrameIndex].Stream.Write(ContentDescriptionBytes[0], Length(ContentDescriptionBytes));
        DataWord := $0000;
        Frames[FrameIndex].Stream.Write(DataWord, 2);
        //* EncapsulatedObject
        Frames[FrameIndex].Stream.Write(EncapsulatedObject[0], Length(EncapsulatedObject));
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetContent(FrameID: String; Content: String; LanguageID: TLanguageID; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetContent(Index, Content, LanguageID, Description, TextEncoding);
end;

function TID3v2Frames.SetContent(FrameIndex: Integer; Content: String; LanguageID: TLanguageID; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    case TextEncoding of
        teUnicode: begin
            try
                //* Set unicode flag
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the language
                Frames[FrameIndex].Stream.Write(LanguageID[0], 3);
                //* Set the description
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(PWideChar(Description)^, (Length(Description) + 1) * 2);
                //* Write the content with BOM
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(PWideChar(Content)^, (Length(Content) + 1) * 2);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            try
                //* Set UTF8 flag
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the language
                Frames[FrameIndex].Stream.Write(LanguageID[0], 3);
                //* Set the description
                Bytes := TEncoding.UTF8.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the content
                SetLength(Bytes, 0);
                Bytes := TEncoding.UTF8.GetBytes(Content);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            try
                //* Set ANSI flag
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the language
                Frames[FrameIndex].Stream.Write(LanguageID[0], 3);
                //* Set the description
                Bytes := TEncoding.ANSI.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the content with
                SetLength(Bytes, 0);
                Bytes := TEncoding.ANSI.GetBytes(Content);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;
end;

function TID3v2Frames.GetLyrics(FrameID: String; var LanguageID: TLanguageID; var Description: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    ClearLanguageID(LanguageID);
    Description := '';
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetLyrics(Index, LanguageID, Description);
end;

function TID3v2Frames.GetLyrics(FrameIndex: Integer; var LanguageID: TLanguageID; var Description: String): String;
begin
    Result := GetContent(FrameIndex, LanguageID, Description);
end;

function TID3v2Frames.SetLyrics(FrameID: String; Lyrics: String; LanguageID: TLanguageID; Description: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetContent(Index, Lyrics, LanguageID, Description);
end;

function TID3v2Frames.SetLyrics(FrameIndex: Integer; Lyrics: String; LanguageID: TLanguageID; Description: String): Boolean;
begin
    Result := SetContent(FrameIndex, Lyrics, LanguageID, Description);
end;

function TID3v2Frames.GetCoverPictureStream(FrameID: String; PictureStream: TStream; var MIMEType: String; var Description: String; var CoverType: Integer): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    MIMEType := '';
    Description := '';
    CoverType := 0;
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetCoverPictureStream(Index, PictureStream, MIMEType, Description, CoverType);
end;

function TID3v2Frames.GetCoverPictureStream(FrameIndex: Integer; PictureStream: TStream; var MIMEType: String; var Description: String; var CoverType: Integer): Boolean;
var
    DataByte: Byte;
    Encoding: Byte;
begin
    Result := False;
    MIMEType := '';
    Description := '';
    CoverType := 0;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get MIME type
        MIMEType := Frames[FrameIndex].Stream.ReadText;
        //* Get picture type
        Frames[FrameIndex].Stream.Read2(DataByte, 1);
        CoverType := DataByte;
        //* Get description
        Description := Frames[FrameIndex].Stream.ReadText(Encoding);
        //* Get binary picture data
        PictureStream.Seek(0, soBeginning);
        PictureStream.CopyFrom(Frames[FrameIndex].Stream, Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position);
        PictureStream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetCoverPictureInfo(FrameID: String; var MIMEType: String; var Description: String; var CoverType: Integer; Bytes: PBytes = nil): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    MIMEType := '';
    Description := '';
    CoverType := 0;
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetCoverPictureInfo(Index, MIMEType, Description, CoverType, Bytes);
end;

function TID3v2Frames.GetCoverPictureInfo(FrameIndex: Integer; var MIMEType: String; var Description: String; var CoverType: Integer; Bytes: PBytes = nil): Boolean;
var
    DataByte: Byte;
    Encoding: Byte;
    PictureDataSize: Int64;
begin
    Result := False;
    MIMEType := '';
    Description := '';
    CoverType := 0;
    if Bytes <> nil then
        Bytes^ := nil;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get MIME type
        MIMEType := Frames[FrameIndex].Stream.ReadText;
        //* Get picture type
        Frames[FrameIndex].Stream.Read2(DataByte, 1);
        CoverType := DataByte;
        //* Get description
        Description := Frames[FrameIndex].Stream.ReadText(Encoding);
        //* Get binary picture data
        if Bytes <> nil then begin
            PictureDataSize := Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position;
            if PictureDataSize > 0 then begin
                SetLength(Bytes^, PictureDataSize);
                Frames[FrameIndex].Stream.Read2(Bytes^[0], PictureDataSize);
            end;
        end;
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetCoverPictureInfoPointer(FrameIndex: Integer; var Data: Pointer; var DataSize: Int64; var MIMEType: Pointer; var Description: Pointer; var Encoding: Integer; var CoverType: Integer): Boolean;
var
    DataByte: Byte;
    DataWord: Word;
begin
    Result := False;
    CoverType := 0;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get MIME type
        MIMEType := Pointer(NativeUInt(Frames[FrameIndex].Stream.Memory) + UInt64(Frames[FrameIndex].Stream.Position));
        repeat
            Frames[FrameIndex].Stream.Read2(DataByte, 1);
        until (DataByte = 0)
        OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size);
        //* Get picture type
        Frames[FrameIndex].Stream.Read2(DataByte, 1);
        CoverType := DataByte;
        //* Get description
        case Encoding of
            //* ISO-8859-1
            0: begin
                Description := Pointer(NativeUInt(Frames[FrameIndex].Stream.Memory) + UInt64(Frames[FrameIndex].Stream.Position));
                repeat
                    Frames[FrameIndex].Stream.Read2(DataByte, 1);
                until (DataByte = 0)
                OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size);
            end;
            //* UTF-16
            1: begin
                Frames[FrameIndex].Stream.ReadBOM;
                Description := Pointer(NativeUInt(Frames[FrameIndex].Stream.Memory) + UInt64(Frames[FrameIndex].Stream.Position));
                repeat
                    Frames[FrameIndex].Stream.Read2(DataWord, 2);
                until (DataWord = 0)
                OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size);
            end;
            //* UTF-16BE
            2: begin
                Description := Pointer(NativeUInt(Frames[FrameIndex].Stream.Memory) + UInt64(Frames[FrameIndex].Stream.Position));
                repeat
                    Frames[FrameIndex].Stream.Read2(DataWord, 2);
                until (DataWord = 0)
                OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size);
            end;
            //* UTF-8
            3: begin
                Description := Pointer(NativeUInt(Frames[FrameIndex].Stream.Memory) + UInt64(Frames[FrameIndex].Stream.Position));
                repeat
                    Frames[FrameIndex].Stream.Read2(DataByte, 1);
                until (DataByte = 0)
                OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size);
            end;
        end;
        //* Get binary picture data
        Data := Pointer(NativeUInt(Frames[FrameIndex].Stream.Memory) + UInt64(Frames[FrameIndex].Stream.Position));
        DataSize := Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position;
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetGEOB(FrameID: String; var MIMEType, EncapsulatedObjectFilename, ContentDescription: String): TBytes;
var
    Index: Integer;
    ID: TFrameID;
begin
    SetLength(Result, 0);
    MIMEType := '';
    EncapsulatedObjectFilename := '';
    ContentDescription := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetGEOB(Index, MIMEType, EncapsulatedObjectFilename, ContentDescription);
end;

function TID3v2Frames.GetGEOB(FrameIndex: Integer; var MIMEType, EncapsulatedObjectFilename, ContentDescription: String): TBytes;
var
    Encoding: Byte;
    BigEndian: Boolean;
    DataSize: Int64;
begin
    SetLength(Result, 0);
    MIMEType := '';
    EncapsulatedObjectFilename := '';
    ContentDescription := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        BigEndian := False;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get MIME type
        MIMEType := Frames[FrameIndex].Stream.ReadText;
        //* Get filename
        EncapsulatedObjectFilename := Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian);
        //* Get content description
        ContentDescription := Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian);
        //* Get the content
        DataSize := Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position;
        if DataSize > 0 then begin
            SetLength(Result, DataSize);
            Frames[FrameIndex].Stream.Read2(Result[0], DataSize);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.SetCoverPictureFromStream(FrameID: String; Description: String; PictureStream: TStream; MIMEType: String; CoverType: Integer): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetCoverPictureFromStream(Index, Description, PictureStream, MIMEType, CoverType);
end;

function TID3v2Frames.SetCoverPictureFromStream(FrameIndex: Integer; Description: String; PictureStream: TStream; MIMEType: String; CoverType: Integer): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        case Root.APICTextEncodingToWrite of
            teUnicode: begin
                ///* Set data is unicode
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the MIME type
                Bytes := TEncoding.ANSI.GetBytes(MIMEType);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                ///* Set picture type
                Data := Byte(CoverType);
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the description with BOM
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(PWideChar(Description)^, (Length(Description) + 1) * 2);
            end;
            teUTF8: begin
                ///* Set data is UTF8
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the MIME type
                Bytes := TEncoding.ANSI.GetBytes(MIMEType);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                ///* Set picture type
                Data := Byte(CoverType);
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the description with BOM
                Bytes := TEncoding.UTF8.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
            end;
            teANSI: begin
                ///* Set data is ANSI
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the MIME type
                Bytes := TEncoding.ANSI.GetBytes(MIMEType);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                ///* Set picture type
                Data := Byte(CoverType);
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the description with BOM
                Bytes := TEncoding.ANSI.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
            end;
        end;
        //* Set picture data
        PictureStream.Seek(0, soBeginning);
        Frames[FrameIndex].Stream.CopyFrom(PictureStream, PictureStream.Size);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetCoverPictureFromFile(FrameID: String; Description: String; PictureFileName: String; MIMEType: String; CoverType: Integer): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetCoverPictureFromFile(Index, Description, PictureFileName, MIMEType, CoverType);
end;


function TID3v2Frames.SetCoverPictureFromFile(FrameIndex: Integer; Description: String; PictureFileName: String; MIMEType: String; CoverType: Integer): Boolean;
var
    PictureStream: TFileStream;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        PictureStream := nil;
        try
            PictureStream := TFileStream.Create(PictureFileName, fmOpenRead);
            Result := SetCoverPictureFromStream(FrameIndex, Description, PictureStream, MIMEType, CoverType);
        finally
            if Assigned(PictureStream) then begin
                FreeAndNil(PictureStream);
            end;
        end;
    except
        //*
    end;
end;

function ReverseBytes32(Value: Cardinal): Cardinal;
begin
    Result := (Value SHR 24) OR (Value SHL 24) OR ((Value AND $00FF0000) SHR 8) OR ((Value AND $0000FF00) SHL 8);
end;

function RemoveUnsynchronisationScheme(Source, Dest: TStream; BytesToRead: Integer): Boolean;
const
  MaxBufSize = $F000;
var
  LastWasFF: Boolean;
  BytesRead: Integer;
  SourcePtr, DestPtr: Integer;
  SourceBuf, DestBuf: array[0..MaxBufSize - 1] of Byte;
begin

  { Replace $FF 00 with $FF }

  LastWasFF := False;
  while BytesToRead > 0 do
  begin
    { Read at max CBufferSize bytes from the stream }
    BytesRead := Source.Read2(SourceBuf[0], Min(MaxBufSize, BytesToRead));
    //if BytesRead = 0 then
    //  ID3Error(RsECouldNotReadData);

    Dec(BytesToRead, BytesRead);

    DestPtr := 0;
    SourcePtr := 0;

    while SourcePtr < BytesRead do
    begin
      { If previous was $FF and current is $00 then skip.. }
      if not LastWasFF or (SourceBuf[SourcePtr] <> $00) then
      begin
        { ..otherwise copy }
        DestBuf[DestPtr] := SourceBuf[SourcePtr];
        Inc(DestPtr);
      end;

      LastWasFF := SourceBuf[SourcePtr] = $FF;
      Inc(SourcePtr);
    end;
    Dest.Write(DestBuf[0], DestPtr);
  end;
    Result := True;
end;

function ApplyUnsynchronisationScheme(Source, Dest: TStream; BytesToRead: Integer): Boolean;
const
  MaxBufSize = $F000;
var
  LastWasFF: Boolean;
  BytesRead: Integer;
  SourcePtr, DestPtr: Integer;
  SourceBuf, DestBuf: PByte;
begin
  { Replace $FF 00         with  $FF 00 00
    Replace $FF %111xxxxx  with  $FF 00 %111xxxxx (%11100000 = $E0 = 224 }

  GetMem(SourceBuf, Min(MaxBufSize div 2, BytesToRead));
  GetMem(DestBuf, 2 * Min(MaxBufSize div 2, BytesToRead));
  try
    LastWasFF := False;
    while BytesToRead > 0 do
    begin
      { Read at max CBufferSize div 2 bytes from the stream }
      BytesRead := Source.Read2(SourceBuf^, Min(MaxBufSize div 2, BytesToRead));
      //if BytesRead = 0 then
      //  ID3Error(RsECouldNotReadData);

      Dec(BytesToRead, BytesRead);

      DestPtr := 0;
      SourcePtr := 0;

      while SourcePtr < BytesRead do
      begin
        { If previous was $FF and current is $00 or >=$E0 then add space.. }
        if LastWasFF and
          ((SourceBuf[SourcePtr] = $00) or (Byte(SourceBuf[SourcePtr]) and $E0 > 0)) then
        begin
          DestBuf[DestPtr] := $00;
          Inc(DestPtr);
        end;

        { Copy }
        DestBuf[DestPtr] := SourceBuf[SourcePtr];
        Inc(DestPtr);

        LastWasFF := SourceBuf[SourcePtr] = $FF;
        Inc(SourcePtr);
      end;
      Dest.Write(DestBuf^, DestPtr);
    end;
  finally
    FreeMem(SourceBuf);
    FreeMem(DestBuf);
  end;
  Result := True;
end;

function TID3v2Frames.GetURL(FrameID: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetURL(Index);
end;

function TID3v2Frames.GetUSER(FrameIndex: Integer; var LanguageID: TLanguageID): String;
var
    Encoding: Byte;
begin
    Result := '';
    ClearLanguageID(LanguageID);
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get language ID
        Frames[FrameIndex].Stream.Read2(LanguageID[0], 3);
        //* Get the content
        Result := Frames[FrameIndex].Stream.ReadText(Encoding);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.GetUSER(FrameID: String; var LanguageID: TLanguageID): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    ClearLanguageID(LanguageID);
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetUSER(Index, LanguageID);
end;

function TID3v2Frame.IsiXML: Boolean;
var
    iXMLID: TFrameID;
begin
    Result := False;
    try
        if Stream.Size = 0 then begin
            Exit;
        end;
        Stream.Seek(0, soBeginning);
        Stream.Read(iXMLID[0], 4);
        Stream.Seek(0, soBeginning);
        Result := IsSameFrameID(iXMLID, 'iXML');
    except
        //*
    end;
end;

function TID3v2Frame.IsXMP: Boolean;
var
    XMPID: TLanguageID;
begin
    Result := False;
    try
        if Stream.Size = 0 then begin
            Exit;
        end;
        Stream.Seek(0, soBeginning);
        Stream.Read(XMPID[0], 3);
        Stream.Seek(0, soBeginning);
        Result := LanguageIDtoString(XMPID) = 'XMP';
    except
        //*
    end;
end;

function TID3v2Frames.GetiXML(FrameIndex: Integer): String;
begin
    Result := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(5, soBeginning);
        //* Get the content
        Result := Frames[FrameIndex].Stream.ReadText(3);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.GetiXML(FrameID: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetiXML(Index);
end;

function TID3v2Frames.GetXMP(FrameIndex: Integer): String;
begin
    Result := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(4, soBeginning);
        //* Get the content
        Result := Frames[FrameIndex].Stream.ReadText(0);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.GetXMP(FrameID: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetXMP(Index);
end;

function TID3v2Frames.SetiXML(FrameIndex: Integer; XMLText: String): Boolean;
var
    iXMLID: TFrameID;
    DataByte: Byte;
    XMLBytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        AnsiStringToPAnsiChar('iXML', iXMLID);
        //* 'iXML'
        Frames[FrameIndex].Stream.Write(iXMLID[0], 4);
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* The content
        XMLBytes := TEncoding.UTF8.GetBytes(XMLText);
        Frames[FrameIndex].Stream.Write(XMLBytes[0], Length(XMLBytes));
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetiXML(FrameID: String; XMLText: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := SetiXML(Index, XMLText);
end;

function TID3v2Frames.SetXMP(FrameIndex: Integer; XMPText: String): Boolean;
var
    XMPID: TLanguageID;
    DataByte: Byte;
    XMPBytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        StringToLanguageID('XMP', XMPID);
        //* 'XMP'
        Frames[FrameIndex].Stream.Write(XMPID[0], 3);
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* The content
        XMPBytes := TEncoding.ANSI.GetBytes(XMPText);
        Frames[FrameIndex].Stream.Write(XMPBytes[0], Length(XMPBytes));
        //* $00$00
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetXMP(FrameID: String; XMPText: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := SetXMP(Index, XMPText);
end;

function TID3v2Frames.GetUserDefinedURLLink(FrameID: String; var Description: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    Description := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetUserDefinedURLLink(Index, Description);
end;

function TID3v2Frames.FindUserDefinedURLLinkByDescription(Description: String; var URL: String): Integer;
var
    FrameID: TFrameID;
    i: Integer;
    GotDescription: String;
    GotURL: String;
begin
    Result := - 1;
    URL := '';
    ConvertString2FrameID('WXXX', FrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, FrameID) then begin
            GotURL := GetUserDefinedURLLink(i, GotDescription);
            if GotDescription = Description then begin
                Result := i;
                URL := GotURL;
                Break;
            end;
        end;
    end;
end;

function TID3v2Frames.SetUserDefinedURLLinkByDescription(Description: String; URL: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    FrameID: TFrameID;
    i: Integer;
    GotDescription: String;
    //GotURL: String;
    Index: Integer;
begin
    Index := - 1;
    ConvertString2FrameID('WXXX', FrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, FrameID) then begin
            {GotURL :=} GetUserDefinedURLLink(i, GotDescription);
            if GotDescription = Description then begin
                Index := i;
                Break;
            end;
        end;
    end;
    if Index = - 1 then begin
        Index := AddFrame(FrameID);
    end;
    Result := SetUserDefinedURLLink(Index, URL, Description, TextEncoding);
end;

function TID3v2Frames.GetUserDefinedURLLink(FrameIndex: Integer; var Description: String): String;
var
    Encoding: Byte;
begin
    Result := '';
    Description := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get description
        Description := Frames[FrameIndex].Stream.ReadText(Encoding);
        //* Get the URL
        Result := Frames[FrameIndex].Stream.ReadText;
    except
        //*
    end;
end;

function TID3v2Frames.SetUserDefinedURLLink(FrameID: String; URL: String; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetUserDefinedURLLink(Index, URL, Description, TextEncoding);
end;

function TID3v2Frames.CheckValidFrame(FrameIndex: Integer): TID3v2FrameErrors;
var
    Encoding: Byte;
begin
    Result := [];
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    case Root.MajorVersion of
        2: begin
            //* Check frame sizes
            if Frames[FrameIndex].Stream.Size > 16777215 {2^24} then begin
                Result := Result + [id3v2feContentToLarge];
            end;
            if Frames[FrameIndex].Size > 16777215 {2^24} then begin
                Result := Result + [id3v2feSizeSpecifiedToLarge];
            end;
        end;
    //* v3 and v4
    else begin
            //* Check if frame ID is valid
            if NOT ValidID3v2FrameID(Frames[FrameIndex].ID) then begin
                Result := Result + [id3v2feInvalidFrameID];
            end;
            //* Check if frame flags bits are invalid
            case Root.MajorVersion of
                2: begin
                    //* ???
                end;
                3: begin
                    if (Frames[FrameIndex].Flags AND $1F1F) > 0 then begin
                        Result := Result + [id3v2feInvalidFlags];
                    end;
                end;
                4: begin
                    if (Frames[FrameIndex].Flags AND $8FB0) > 0 then begin
                        Result := Result + [id3v2feInvalidFlags];
                    end;
                end;
            end;
            //* Check frame sizes
            if Frames[FrameIndex].Stream.Size > 268435455 {2^28} then begin
                Result := Result + [id3v2feContentToLarge];
            end;
            if Frames[FrameIndex].Size > 268435455 {2^28} then begin
                Result := Result + [id3v2feSizeSpecifiedToLarge];
            end;
        end;
    end;
    //* Check text frame encoding validity
    if Frames[FrameIndex].ID[0] = Ord('T') then begin
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Frames[FrameIndex].Stream.Read2(Encoding, 1);
        if Encoding > 3 then begin
            Result := Result + [id3v2feInvalidTextEncoding];
        end;
    end;
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Result := Result + [id3v2feNoContent];
    end;
end;

function TID3v2Frames.SetUserDefinedURLLink(FrameIndex: Integer; URL: String; Description: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
    i: Integer;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    case TextEncoding of
        teUnicode: begin
            try
                //* Set unicode flag
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Write(PWideChar(Description)^, (Length(Description) + 1) * 2);
                //* Write the URL
                for i := StrLow to StrLow + Length(URL) - 1 do begin
                    Data := Ord(URL[i]);
                    Frames[FrameIndex].Stream.Write(Data, 1);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            try
                //* Set UTF8 flag
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Bytes := TEncoding.UTF8.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the URL
                SetLength(Bytes, 0);
                Bytes := TEncoding.UTF8.GetBytes(URL);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            try
                //* Set ANSI flag
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Bytes := TEncoding.ANSI.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the URL
                SetLength(Bytes, 0);
                Bytes := TEncoding.ANSI.GetBytes(URL);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;
end;

function TID3v2Frames.GetURL(FrameIndex: Integer): String;
begin
    Result := GetText(FrameIndex);
end;

function TID3v2Frames.SetURL(FrameID: String; URL: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetURL(Index, URL);
end;

function TID3v2Frames.SetUSER(FrameIndex: Integer; LanguageID: TLanguageID; TermsOfUse: String): Boolean;
var
    DataByte: Byte;
    TermsOfUseBytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        //* Encoding (unicode)
        DataByte := $01;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* Language
        Frames[FrameIndex].Stream.Write(LanguageID[0], 3);
        //* Terms of use
        TermsOfUseBytes := TEncoding.Unicode.GetBytes(TermsOfUse);
        Frames[FrameIndex].Stream.Write(TermsOfUseBytes[0], Length(TermsOfUseBytes));
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetUSER(FrameID: String; LanguageID: TLanguageID; TermsOfUse: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetUSER(Index, LanguageID, TermsOfUse);
end;

function TID3v2Frames.SetURL(FrameIndex: Integer; URL: String): Boolean;
var
    Data: Byte;
    i: Integer;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        //* Write the URL
        for i := StrLow to StrLow + Length(URL) - 1 do begin
            Data := Ord(URL[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function ID3v2DecodeGenre(const Str: String): String;
var
    i: Integer;
    j: Integer;
    Start: Integer;
    Stop: Integer;
    S: String;
begin
    Result := '';
    i := StrLow;
    Start := i;
    Stop := StrLow + Length(Str) - 1;
    while (i < Stop) and (Str[i] = '(') do begin
        Inc(i);
        case Str[i] of
            '(': begin
                Start := i;
                Break;
            end;
            '0'..'9': begin
                repeat
                    Inc(i);
                until (i > Stop) or not {$IFDEF NEXTGEN}Str[i].IsInArray(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']){$ELSE}CharInSet(Str[i], ['0'..'9']){$ENDIF};
                if (i > Stop) or (Str[i] <> ')') or
                   not TryStrToInt(Copy(Str, Start - StrLow + 2, i - Start - 1), j) or
                   (j > 255) then
                    Break;
                S := ID3GenreDataToString(j);
            end;
            'R': begin
                Inc(i);
                if (i > Stop) or (Str[i] <> 'X') then
                    Break;
                Inc(i);
                if (i > Stop) or (Str[i] <> ')') then
                    Break;
                S := 'Remix';
            end;
            'C': begin
                Inc(i);
                if (i > Stop) or (Str[i] <> 'R') then
                    Break;
                Inc(i);
                if (i > Stop) or (Str[i] <> ')') then
                    Break;
                S := 'Cover';
            end;
            ')': begin
                S := '';
            end;
            else
                Break;
        end;
        if S <> '' then begin
            if Result <> '' then
                Result := Result + ' ';
            Result := Result + S;
        end;
        Inc(i);
        Start := i;
    end;
    S := Trim(Copy(Str, Start - StrLow + 1, MaxInt));
    if (S = '') or (S = Result) then
        Exit;
    if Result <> '' then
        Result := Result + ' ';
    Result := Result + S;
end;

function ID3v2DecodeDayMonth(const Str: String): String;
var
    Month: Integer;
    Day: Integer;
    StrMonth: String;
    StrDay: String;
begin
    //* ddMM
    Result := Str;
    StrDay := Copy(Str, 1, 2);
    StrMonth := Copy(Str, 3, 2);
    if not TryStrToInt(StrMonth, Month) or (Month <= 0) or (Month > 12) then
        Exit;
    if not TryStrToInt(StrDay, Day) or (Day <= 0) or (Day > MonthDays[True][Month]) then
        Exit;
    Result := IntToStr(Day) + ', ' + IntToStr(Month);
end;

function ID3v2DecodeList(const Str: String): String;
begin
    Result := StringReplace(Str, '/', #13#10, [rfReplaceAll]);
end;

function ID3v2EncodeTime(DateTime: TDateTime): String;
var
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    MSec: Word;
    StrYear: String;
    StrMonth: String;
    StrDay: String;
    StrHour: String;
    StrMinute: String;
    StrSecond: String;
begin
    DecodeTime(DateTime, Hour, Minute, Second, MSec);
    DecodeDate(DateTime, Year, Month, Day);
    StrYear := IntToStr(Year);
    if Length(StrYear) = 1 then begin
        StrYear := '0' + StrYear;
    end;
    StrMonth := IntToStr(Month);
    if Length(StrMonth) = 1 then begin
        StrMonth := '0' + StrMonth;
    end;
    StrDay := IntToStr(Day);
    if Length(StrDay) = 1 then begin
        StrDay := '0' + StrDay;
    end;
    StrHour := IntToStr(Hour);
    if Length(StrHour) = 1 then begin
        StrHour := '0' + StrHour;
    end;
    StrMinute := IntToStr(Minute);
    if Length(StrMinute) = 1 then begin
        StrMinute := '0' + StrMinute;
    end;
    StrSecond := IntToStr(Second);
    if Length(StrSecond) = 1 then begin
        StrSecond := '0' + StrSecond;
    end;
    //* yyyy-MM-ddTHH:mm:ss
    Result := StrYear + '-' + StrMonth + '-' + StrDay + 'T' + StrHour + ':' + StrMinute + ':' + StrSecond;
end;

function ID3v2DecodeTime(ID3v2DateTime: String; out Parts: Integer): TDateTime;
var
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    MSec: Word;
    StrYear: String;
    StrMonth: String;
    StrDay: String;
    StrHour: String;
    StrMinute: String;
    StrSecond: String;
    Date: TDateTime;
    Time: TDateTime;
    i: Integer;
begin
    //* yyyy-MM-ddTHH:mm:ss
    Parts := 0;
    StrYear := Copy(ID3v2DateTime, 1, 4);
    StrMonth := Copy(ID3v2DateTime, 6, 2);
    StrDay := Copy(ID3v2DateTime, 9, 2);
    StrHour := Copy(ID3v2DateTime, 12, 2);
    StrMinute := Copy(ID3v2DateTime, 15, 2);
    StrSecond := Copy(ID3v2DateTime, 18, 2);
    if TryStrToInt(StrYear, i) and (i >= 1) and (i <= 9999) then begin
        Year := i;
        Parts := 1;
    end
    else
        Year := 2000;
    if TryStrToInt(StrMonth, i) and (i >= 1) and (i <= 12) then begin
        Month := i;
        if Parts = 1 then
            Parts := 2;
    end
    else
        Month := 1;
    if TryStrToInt(StrDay, i) and (i >= 1) and (i <= MonthDays[IsLeapYear(Year)][Month]) then begin
        Day := i;
        if Parts = 2 then
            Parts := 3;
    end
    else
        Day := 1;
    if TryStrToInt(StrHour, i) and (i >= 0) and (i < HoursPerDay) then begin
        Hour := i;
        if Parts = 3 then
            Parts := 4;
    end
    else
        Hour := 0;
    if TryStrToInt(StrMinute, i) and (i >= 0) and (i < MinsPerHour) then begin
        Minute := i;
        if Parts = 4 then
            Parts := 5;
    end
    else
        Minute := 0;
    if TryStrToInt(StrSecond, i) and (i >= 0) and (i < SecsPerMin) then begin
        Second := i;
        if Parts = 5 then
            Parts := 6;
    end
    else
        Second := 0;
    MSec := 0;
    Time := EncodeTime(Hour, Minute, Second, MSec);
    Date := EncodeDate(Year, Month, Day);
    Result := Date + Time;
end;

function ID3v2DecodeTime(ID3v2DateTime: String): TDateTime;
var
    Parts: Integer;
begin
    Result := ID3v2DecodeTime(ID3v2DateTime, Parts);
end;

function ID3v2DecodeTimeToNumbers(ID3v2DateTime: String; var Year, Month, Day, Hour, Minute, Second: Integer): Boolean;
var
    StrYear: String;
    StrMonth: String;
    StrDay: String;
    StrHour: String;
    StrMinute: String;
    StrSecond: String;
begin
    //* yyyy-MM-ddTHH:mm:ss
    StrYear := Copy(ID3v2DateTime, 1, 4);
    StrMonth := Copy(ID3v2DateTime, 6, 2);
    StrDay := Copy(ID3v2DateTime, 9, 2);
    StrHour := Copy(ID3v2DateTime, 12, 2);
    StrMinute := Copy(ID3v2DateTime, 15, 2);
    StrSecond := Copy(ID3v2DateTime, 18, 2);
    Year := StrToIntDef(StrYear, 0);
    Month := StrToIntDef(StrMonth, 0);
    Day := StrToIntDef(StrDay, 0);
    Hour := StrToIntDef(StrHour, - 1);
    Minute := StrToIntDef(StrMinute, - 1);
    Second := StrToIntDef(StrSecond, - 1);
    Result := True;
end;

function ID3v2DecodeTimeOnly(const Str: String; out Error: Boolean): TTime;
var
    Hour: Word;
    Minute: Word;
    StrHour: String;
    StrMinute: String;
    i: Integer;
begin
    //* HH:mm
    Error := False;
    StrHour := Copy(Str, 1, 2);
    i := StrLow + 2;
    if (i <= Length(Str)) and (Str[i] = ':') then
        Inc(i);
    StrMinute := Copy(Str, i - StrLow + 1, 2);
    if TryStrToInt(StrHour, i) and (i >= 0) and (i < HoursPerDay) then
        Hour := i
    else begin
        Hour := 0;
        Error := True;
    end;
    if TryStrToInt(StrMinute, i) and (i >= 0) and (i < MinsPerHour) then
        Minute := i
    else begin
        Minute := 0;
        Error := True;
    end;
    Result := EncodeTime(Hour, Minute, 0, 0);
end;

function ID3v2DecodeTimeOnly(const Str: String): TTime;
var
    Error: Boolean;
begin
    Result := ID3v2DecodeTimeOnly(Str, Error);
end;

function TID3v2Frames.GetTime(FrameID: String): TDateTime;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := 0;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetTime(Index);
end;

function TID3v2Frames.GetTime(FrameIndex: Integer): TDateTime;
var
    Parts: Integer;
begin
    Parts := 0;
    Result := ID3v2DecodeTime(GetText(FrameIndex), Parts);
end;

function TID3v2Frames.SetTime(FrameID: String; DateTime: TDateTime): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetTime(Index, DateTime);
end;

function TID3v2Frames.SetTime(FrameIndex: Integer; DateTime: TDateTime): Boolean;
var
    TDRCDateTime: String;
    Data: Byte;
    Bytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        TDRCDateTime := ID3v2EncodeTime(DateTime);
        Bytes := TEncoding.ANSI.GetBytes(TDRCDateTime);
        Data := 0;
        Frames[FrameIndex].Stream.Write(Data, 1);
        //* Set the date time
        Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
        Data := 0;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Tag.CalculateTagSize(_PaddingSize: Cardinal): Cardinal;
var
    TotalTagSize: Cardinal;
    i: Integer;
begin
    //* TODO: Ext header size
    TotalTagSize := 10{ + ExtendedHeaderSize3};
    (*
    if MajorVersion = 3 then begin
        for i := 0 to FrameCount - 1 do begin
            if (Frames[i].Stream.Size = 0)
            OR (NOT ValidID3v2FrameID(Frames[i].ID))
            then begin
                Continue;
            end;
            TotalTagSize := TotalTagSize + Frames[i].Stream.Size + 10;
            if Frames[i].DataLengthIndicator
            OR Frames[i].Compressed
            then begin
                TotalTagSize := TotalTagSize + 4;
            end;
        end;
    end;
    if MajorVersion > 3 then begin
        for i := 0 to FrameCount - 1 do begin
            if (Frames[i].Stream.Size = 0)
            OR (NOT ValidID3v2FrameID(Frames[i].ID))
            then begin
                Continue;
            end;
            TotalTagSize := TotalTagSize + 10;
            TotalTagSize := TotalTagSize + Frames[i].Stream.Size;
            if Frames[i].GroupingIdentity then begin
                TotalTagSize := TotalTagSize + 1;
            end;
            if Frames[i].Encrypted then begin
                TotalTagSize := TotalTagSize + 1;
            end;
            if Frames[i].DataLengthIndicator then begin
                TotalTagSize := TotalTagSize + 4;
            end;
        end;
    end;
    *)
    for i := 0 to FrameCount - 1 do begin
        TotalTagSize := TotalTagSize + FullFrameSize(i) + 10;
    end;
    TotalTagSize := TotalTagSize + _PaddingSize;
    Result := TotalTagSize;
end;

function TID3v2Tag.CalculateTotalFramesSize: Integer;
var
    TotalFramesSize: Integer;
    i: Integer;
begin
    TotalFramesSize := 0;
    for i := 0 to FrameCount - 1 do begin
        if ValidID3v2FrameID(Frames[i].ID) then begin
            TotalFramesSize := TotalFramesSize + Frames[i].Stream.Size;
        end;
    end;
    Result := TotalFramesSize;
end;

function TID3v2Tag.CheckMPEG(MPEGStream: TStream): Boolean;
var
    i: Integer;
    Data: Byte;
    MPEGFrameSize: Integer;
    FirstMPEGFramePosition: Int64;
begin
    Result := False;
    i := 0;
    Data := 0;
    repeat
        MPEGStream.Read2(Data, 1);
        if Data = $FF then begin
            MPEGStream.Read2(Data, 1);
            if (Data = $F9)
            OR (Data = $FA)
            OR (Data = $FB)
            OR (Data = $FC)
            OR (Data = $FD)
            OR (Data = $F2)
            OR (Data = $F3)
            OR (Data = $E3)
            then begin
                MPEGStream.Seek(- 2, soCurrent);
                MPEGFrameSize := MPEGValidHeader(MPEGStream);
                if MPEGFrameSize > 0 then begin
                    FirstMPEGFramePosition := MPEGStream.Position;
                    MPEGStream.Seek(MPEGFrameSize, soCurrent);
                    MPEGFrameSize := MPEGValidHeader(MPEGStream);
                    MPEGStream.Seek(FirstMPEGFramePosition, soBeginning);
                    if MPEGFrameSize > 0 then begin
                        Result := True;
                        Exit;
                    end;
                end;
                MPEGStream.Seek(2, soCurrent);
            end;
        end;
        Inc(i);
    until i > MPEGSearchLength;
end;

function TID3v2Frames.FullFrameSize(FrameIndex: Cardinal): Cardinal;
    {
var
    i: Integer;
    }
begin
    Result := 0;
    if Root.MajorVersion = 2 then begin
        Result := Frames[FrameIndex].Stream.Size;
    end;
    if Root.MajorVersion = 3 then begin
        Result := Frames[FrameIndex].Stream.Size;
        if Frames[FrameIndex].Compressed
        OR Frames[FrameIndex].DataLengthIndicator
        then begin
            Result := Result + 4;
        end;
        if Frames[FrameIndex].Encrypted then begin
            Result := Result + 1;
        end;
        if Frames[FrameIndex].GroupingIdentity then begin
            Result := Result + 1;
        end;
    end;
    if Root.MajorVersion > 3 then begin
        Result := Frames[FrameIndex].Stream.Size;
        if Frames[FrameIndex].GroupingIdentity then begin
            Result := Result + 1;
        end;
        if Frames[FrameIndex].Encrypted then begin
            Result := Result + 1;
        end;
        if Frames[FrameIndex].DataLengthIndicator then begin
            Result := Result + 4;
        end;
    end;

    (*
    //* CHAP and CTOC frames
    if Frames[FrameIndex] is TID3v2FrameCHAP then begin
        for i := 0 to (Frames[FrameIndex] as TID3v2FrameCHAP).SubFrames.Frames.Count - 1 do begin
            Result := Result + (Frames[FrameIndex] as TID3v2FrameCHAP).SubFrames.FullFrameSize(i);
        end;
    end;
    if Frames[FrameIndex] is TID3v2FrameCTOC then begin
        for i := 0 to (Frames[FrameIndex] as TID3v2FrameCTOC).SubFrames.Frames.Count - 1 do begin
            Result := Result + (Frames[FrameIndex] as TID3v2FrameCTOC).SubFrames.FullFrameSize(i);
        end;
    end;
    *)

end;

procedure TID3v2Tag.Clear;
begin
    Inherited;
    SourceFileName := '';
    Loaded := False;
    MajorVersion := 3;
    MinorVersion := 0;
    Flags := 0;
    Unsynchronised := False;
    FWasUnsynchronised := False;
    Compressed := False;
    ExtendedHeader := False;
    Experimental := False;
    Size := 0;
    CodedSize := 0;
    PaddingSize := 0;
    PaddingToWrite := ID3V2LIBRARY_DEFAULT_PADDING_SIZE;
    FPosition := 0;
    if Assigned(ExtendedHeader3) then begin
        FreeAndNil(ExtendedHeader3);
    end;
    ExtendedHeader3 := TID3v2ExtendedHeader3.Create;
    if Assigned(ExtendedHeader4) then begin
        FreeAndNil(ExtendedHeader4);
    end;
    ExtendedHeader4 := TID3v2ExtendedHeader4.Create;
    MPEGInfo.Position := 0;
    MPEGInfo.Header := 0;
    MPEGInfo.FrameSize := 0;
    MPEGInfo.Version := tmpegvUnknown;
    MPEGInfo.Layer := tmpeglUnknown;
    MPEGInfo.CRC := False;
    MPEGInfo.BitRate := 0;
    MPEGInfo.SampleRate := 0;
    MPEGInfo.Padding := False;
    MPEGInfo._Private := False;
    MPEGInfo.ChannelMode := tmpegcmUnknown;
    MPEGInfo.ModeExtension := tmpegmeUnknown;
    MPEGInfo.Copyrighted := False;
    MPEGInfo.Original := False;
    MPEGInfo.Emphasis := tmpegeUnknown;
    MPEGInfo.VBR := False;
    MPEGInfo.FrameCount := 0;
    MPEGInfo.Quality := 0;
    MPEGInfo.Bytes := 0;
    MPEGInfo.HasXing := False;
    MPEGInfo.HasInfo := False;
    MPEGInfo.HasVBRI := False;
    MPEGInfo.EncoderShortVersionString := '';
    MPEGInfo.InfoTagRevision := 0;
    MPEGInfo.VBRMethod := 0;
    MPEGInfo.LowpassFilterValue := 0;
    MPEGInfo.PeakSignalSmplitude := 0;
    MPEGInfo.RadioReplayGain := 0;
    MPEGInfo.AudiophileReplayGain := 0;
    MPEGInfo.EncodingFlags := 0;
    MPEGInfo.ATHType := 0;
    MPEGInfo.ABROrMinimalBitrate := 0;
    MPEGInfo.EncoderDelay := 0;
    MPEGInfo.EncoderPadding := 0;
    MPEGInfo.Misc := 0;
    MPEGInfo.MP3Gain := 0;
    MPEGInfo.PresetAndSurroundInfo := 0;
    MPEGInfo.MusicLength := 0;
    MPEGInfo.MusicCRC := 0;
    MPEGInfo.CRC16OfInfoTag := 0;
    FillChar(Self.WAVInfo, SizeOf(TWaveFmt), 0);
    DSFInfo.Clear;
    DFFInfo.ResetData;
    AIFFInfo.Channels := 0;
    AIFFInfo.SampleFrames := 0;
    AIFFInfo.SampleSize := 0;
    AIFFInfo.SampleRate := 0;
    AIFFInfo.CompressionID := '';
    AIFFInfo.Compression := '';
    FSourceFileType := sftUnknown;
    FPlayTime := 0;
    FSampleCount := 0;
    FBitRate := 0;
    {$IFDEF STRANGE_TAG}
    FStrangeTag := False;
    {$ENDIF}
    FError := False;
    APEv2TagSize := 0;
end;

function TID3v2Frames.WriteAllFrames(Stream: TStream): Integer;
var
    i: Integer;
    UnCodedSize: Cardinal;
    CodedSize: Cardinal;
    ReversedFlags: Word;
    DataLengthIndicatorValueSyncSafe: Cardinal;
begin
    try
        for i := 0 to FrameCount - 1 do begin
            if (NOT ValidID3v2FrameID(Frames[i].ID))
            OR (Frames[i].Stream.Size = 0)
            then begin
                Continue;
            end;
            Stream.Write(Frames[i].ID, 4);
            UnCodedSize := FullFrameSize(i);
            if Root.MajorVersion = 3 then begin
                CodedSize := ReverseBytes32(UnCodedSize);
                Stream.Write(CodedSize, 4);
                Frames[i].EncodeFlags3;
                Stream.Write(Frames[i].Flags, 2);
                SyncSafe(Frames[i].DataLengthIndicatorValue, DataLengthIndicatorValueSyncSafe, 4);
                if Frames[i].Compressed
                OR Frames[i].DataLengthIndicator
                then begin
                    Stream.Write(DataLengthIndicatorValueSyncSafe, 4);
                end;
                if Frames[i].Encrypted then begin
                    Stream.Write(Frames[i].EncryptionMethod, 1);
                end;
                if Frames[i].GroupingIdentity then begin
                    Stream.Write(Frames[i].GroupIdentifier, 1);
                end;
            end;
            if Root.MajorVersion = 4 then begin
                UnCodedSize := FullFrameSize(i);
                SyncSafe(UnCodedSize, CodedSize, 4);
                Stream.Write(CodedSize, 4);
                Frames[i].EncodeFlags4;
                ReversedFlags := ReverseBytes16(Frames[i].Flags);
                Stream.Write(ReversedFlags, 2);
                if Frames[i].GroupingIdentity then begin
                    Stream.Write(Frames[i].GroupIdentifier, 1);
                end;
                if Frames[i].Encrypted then begin
                    Stream.Write(Frames[i].EncryptionMethod, 1);
                end;
                SyncSafe(Frames[i].DataLengthIndicatorValue, DataLengthIndicatorValueSyncSafe, 4);
                if Frames[i].DataLengthIndicator then begin
                    Stream.Write(DataLengthIndicatorValueSyncSafe, 4);
                end;
            end;
            Stream.CopyFrom(Frames[i].Stream, 0);
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
    end;
end;

function TID3v2Tag.WriteAllHeaders(Stream: TStream): Integer;
begin
    try
        Stream.Write(ID3v2ID, 3);
        Stream.Write(MajorVersion, 1);
        Stream.Write(MinorVersion, 1);
        if MajorVersion = 3 then begin
            Stream.Write(Flags, 1);
            Stream.Write(CodedSize, 4);
        end;
        if MajorVersion = 4 then begin
            Stream.Write(Flags, 1);
            Stream.Write(CodedSize, 4);
        end;
        if ExtendedHeader then begin
            //* TODO
            if MajorVersion = 3 then begin

            end;
            if MajorVersion >= 4 then begin

            end;
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
    end;
end;

function WritePadding(Stream: TStream; PaddingSize: Integer): Integer;
var
    Data: TBytes;
begin
    try
        SetLength(Data, PaddingSize);
        Stream.Write(Data[0], Length(Data));
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
    end;
end;

function LanguageIDtoString(LanguageId : TLanguageID): String;
var
    i: integer;
begin
    Result := '';
    for i := low(TLanguageID) to high(TLanguageID) do begin
        if LanguageId[i] <> 0 then begin
            Result := Result + Char(LanguageId[i]);
        end;
    end;
end;

procedure TID3v2Tag.EncodeSize;
var
    UnCodedSize: Cardinal;
begin
    UnCodedSize := CalculateTagSize(PaddingSize) - 10;
    SyncSafe(UnCodedSize, CodedSize, 4);
end;

{
function TID3v2Tag.RemoveUnsynchronisationOnExtendedHeaderSize: Boolean;
begin
    //Result := RemoveUnsynchronisationOnStream(ExtendedHeader3.SizeData);
    Result := False;
end;

function TID3v2Tag.ApplyUnsynchronisationOnExtendedHeaderSize: Boolean;
begin
    //Result := ApplyUnsynchronisationOnStream(ExtendedHeader3.SizeData);
    Result := False;
end;
}

function TID3v2Tag.RemoveUnsynchronisationOnExtendedHeaderData: Boolean;
begin
    Result := RemoveUnsynchronisationOnStream(ExtendedHeader3.Data);
end;

procedure TID3v2Tag.ReSetAlreadyParsedState;
var
    i: Integer;
begin
    for i := 0 to Frames.Count - 1 do begin
        Frames[i].AlreadyParsed := False;
    end;
end;

function TID3v2Tag.RewriteCoverArts: Boolean;
var
    i: Integer;
    PictureStream: TStream;
    MIMEType: String;
    Description: String;
    CoverType: Integer;
begin
    Result := False;
    for i := FrameCount - 1 downto 0 do begin
        if IsSameFrameID(Frames[i].ID, 'APIC') then begin
            PictureStream := TMemoryStream.Create;
            try
                if GetCoverPictureStream(i, PictureStream, MIMEType, Description, CoverType) then begin
                    SetCoverPictureFromStream(i, Description, PictureStream, MIMEType, CoverType);
                end;
            finally
                FreeAndNil(PictureStream);
            end;
            Result := True;
        end;
    end;
end;

{
function TID3v2Tag.ApplyUnsynchronisationOnExtendedHeaderData: Boolean;
begin
    Result := ApplyUnsynchronisationOnStream(ExtendedHeader3.Data);
end;
}
function RemoveUnsynchronisationOnStream(Stream: TMemoryStream): Boolean;
var
    UnUnsyncronisedStream: TMemoryStream;
    Success: Boolean;
begin
    Result := False;
    UnUnsyncronisedStream := nil;
    try
        UnUnsyncronisedStream := TMemoryStream.Create;
        Stream.Seek(0, soBeginning);
        Success := RemoveUnsynchronisationScheme(Stream, UnUnsyncronisedStream, Stream.Size);
        if Success then begin
            Stream.Clear;
            UnUnsyncronisedStream.Seek(0, soBeginning);
            Stream.CopyFrom(UnUnsyncronisedStream, 0);
            Result := True;
        end;
    finally
        if Assigned(UnUnsyncronisedStream) then begin
            FreeAndNil(UnUnsyncronisedStream);
        end;
    end;
end;

function ApplyUnsynchronisationOnStream(Stream: TMemoryStream): Boolean;
var
    UnsyncronisedStream: TMemoryStream;
    Success: Boolean;
begin
    Result := False;
    UnsyncronisedStream := nil;
    try
        UnsyncronisedStream := TMemoryStream.Create;
        Stream.Seek(0, soBeginning);
        Success := ApplyUnsynchronisationScheme(Stream, UnsyncronisedStream, Stream.Size);
        if Success then begin
            Stream.Clear;
            UnsyncronisedStream.Seek(0, soBeginning);
            Stream.CopyFrom(UnsyncronisedStream, 0);
            Result := True;
        end;
    finally
        if Assigned(UnsyncronisedStream) then begin
            FreeAndNil(UnsyncronisedStream);
        end;
    end;
end;

function TID3v2Frames.GetSEBR(FrameID: String): {$IFDEF WIN64}Double{$ELSE}Extended{$ENDIF};
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := 0;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetSEBR(Index);
end;

function TID3v2Frames.GetSEBR(FrameIndex: Integer): {$IFDEF WIN64}Double{$ELSE}Extended{$ENDIF};
var
    {$IFNDEF WIN64}
    SEBR: Extended;
    {$ENDIF}
    SEBRStr: String;
begin
    Result := 0;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    {$IFDEF WIN64}
    SEBRStr := GetSEBRString(FrameIndex);
    if Copy(SEBRStr, 1, 1) = '~' then begin
        Result := StrToFloatDef(Copy(SEBRStr, 2, Length(SEBRStr)), 0);
    end;
    {$ELSE}
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Seek(0, soBeginning);
    try
        SEBR := 0;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Frames[FrameIndex].Stream.Read2(SEBR, 10);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := SEBR;
    except
        //*
    end;
    if SEBR = 0 then begin
        SEBRStr := GetSEBRString(FrameIndex);
        if Copy(SEBRStr, 1, 1) = '~' then begin
            Result := StrToFloatDef(Copy(SEBRStr, 2, Length(SEBRStr)), 0);
        end;
    end;
    {$ENDIF}
end;

function TID3v2Frames.GetSEBRString(FrameIndex: Integer): String;
var
    SEBR: String;
    Data: Byte;
begin
    Result := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Seek(10, soBeginning);
    try
        SEBR := '';
        Data := 0;
        repeat
            Frames[FrameIndex].Stream.Read2(Data, 1);
            if Data <> 0 then begin
                SEBR := SEBR + Char(Data);
            end;
        until Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := SEBR;
    except
        //*
    end;
end;

function TID3v2Frames.SetSEBR(FrameID: String; BitRateStr: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetSEBR(Index, BitRateStr);
end;

function TID3v2Frames.SetSEBR(FrameIndex: Integer; BitRateStr: String): Boolean;
var
    Data: Byte;
    i: Integer;
    {$IFNDEF WIN64}
    SEBR: Extended;
    {$ENDIF}
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    try
        {$IFDEF WIN64}
        Data := 0;
        for i := 0 to 9 do begin
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        {$ELSE}
        if Copy(BitRateStr, 1, 1) = '~' then begin
            SEBR := StrToFloatDef(Copy(BitRateStr, 2, Length(BitRateStr)), 0);
        end;
        Frames[FrameIndex].Stream.Write(SEBR, 10);
        {$ENDIF}
        for i := StrLow to StrLow + Length(BitRateStr) - 2 do begin
            Data := Ord(BitRateStr[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Result := True;
    except
        //*
    end;
end;

{$IFNDEF WIN64}

function TID3v2Frames.SetSEBR(FrameID: String; BitRateValue: Extended): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetSEBR(Index, BitRateValue);
end;

function TID3v2Frames.SetSEBR(FrameIndex: Integer; BitRateValue: Extended): Boolean;
var
    StrSEBR: String;
    i: Integer;
    Data: Byte;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    try
        Frames[FrameIndex].Stream.Write(BitRateValue, 10);
        StrSEBR := FloatToStr(BitRateValue);
        for i := StrLow to StrLow + Length(StrSEBR) - 1 do begin
            Data := Ord(StrSEBR[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Result := True;
    except
        //*
    end;
end;

{$ENDIF}

function TID3v2Frames.GetSampleCache(FrameIndex: Integer; ForceDecompression: Boolean; var Version: Byte; var Channels: Integer): TID3v2SampleCache;
var
    ID: Integer;
    SESCHeaderSize: Cardinal;
    ReportedChannels: Integer;
    DataVersion: Byte;
    SeekPosition: Integer;
begin
    SetLength(Result, 0);
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Exit;
    end;
    Version := 1;
    Channels := 2;
    if Frames[FrameIndex].Unsynchronised then begin
        Frames[FrameIndex].RemoveUnsynchronisation;
    end;
    if Frames[FrameIndex].Compressed
    OR ForceDecompression
    then begin
        Frames[FrameIndex].DeCompress;
    end;
    Frames[FrameIndex].Stream.Seek(0, soBeginning);
    try
        Frames[FrameIndex].Stream.Read2(ID, 4);
        if ID = ID3V2LIBRARY_SESC_ID then begin
            Frames[FrameIndex].Stream.Read2(DataVersion, 1);
            Frames[FrameIndex].Stream.Read2(SESCHeaderSize, 4);
            Version := DataVersion;
            if DataVersion = ID3V2LIBRARY_SESC_VERSION2 then begin
                if SESCHeaderSize >= 4 then begin
                    Frames[FrameIndex].Stream.Read2(ReportedChannels, 4);
                    SeekPosition := SESCHeaderSize - 4;
                    Frames[FrameIndex].Stream.Seek(SeekPosition, soCurrent);
                end;
            end;
        end else begin
            Frames[FrameIndex].Stream.Seek(-4, soCurrent);
        end;
        SetLength(Result, Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position);
        Frames[FrameIndex].Stream.Read2(Pointer(Result)^, Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position);
    except
        //*
    end;
    Frames[FrameIndex].Stream.Seek(0, soBeginning);
end;

function TID3v2Frames.SetSampleCache(FrameIndex: Integer; SESC: TID3v2SampleCache; Channels: Integer): Boolean;
var
    SESCHeaderSize: Cardinal;
    SESCID: Integer;
    DataVersion: Byte;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        SESCID := ID3V2LIBRARY_SESC_ID;
        Frames[FrameIndex].Stream.Write(SESCID, 4);
        DataVersion := ID3V2LIBRARY_SESC_VERSION2;
        Frames[FrameIndex].Stream.Write(DataVersion, 1);
        SESCHeaderSize := 4;
        Frames[FrameIndex].Stream.Write(SESCHeaderSize, 4);
        Frames[FrameIndex].Stream.Write(Channels, 4);
        Frames[FrameIndex].Stream.Write(Pointer(SESC)^, Length(SESC));
        Frames[FrameIndex].Compress;
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetSEFC(FrameIndex: Integer): Int64;
var
    SEFC: String;
    Data: Byte;
    DataWord: Byte;
begin
    Result := - 1;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Seek(0, soBeginning);
    try
        Frames[FrameIndex].Stream.Read2(Data, 1);
        if Data = $00 then begin
            repeat
                Frames[FrameIndex].Stream.Read2(Data, 1);
                SEFC := SEFC + Char(Data);
            until Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size;
            Result := StrToIntDef(SEFC, 0);
        end;
        if Data = $01 then begin
            repeat
                Frames[FrameIndex].Stream.Read2(DataWord, 2);
                SEFC := SEFC + Char(DataWord);
            until Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size;
            Result := StrToIntDef(SEFC, 0);
        end;
    except
        //*
    end;
    Frames[FrameIndex].Stream.Seek(0, soBeginning);
end;

function TID3v2Frames.SetSEFC(FrameIndex: Integer; SEFC: Int64): Boolean;
var
    StrSEFC: String;
    Data: Byte;
    i: Integer;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        StrSEFC := IntToStr(SEFC);
        Data := $00;
        Frames[FrameIndex].Stream.Write(Data, 1);
        for i := StrLow to StrLow + Length(StrSEFC) - 1 do begin
            Data := Ord(StrSEFC[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Result := True;
    except
        //*
    end;
end;

procedure AnsiStringToPAnsiChar(const Source: String; out Dest: TFrameID);
var
    SourceLength: Integer;
begin
    SourceLength := Length(Source);
    if SourceLength > 0 then begin
        Dest[0] := Ord(Source[StrLow]);
    end else begin
        Dest[0] := $0;
    end;
    if SourceLength > 1 then begin
        Dest[1] := Ord(Source[StrLow + 1]);
    end else begin
        Dest[1] := $0;
    end;
    if SourceLength > 2 then begin
        Dest[2] := Ord(Source[StrLow + 2]);
    end else begin
        Dest[2] := $0;
    end;
    if SourceLength > 3 then begin
        Dest[3] := Ord(Source[StrLow + 3]);
    end else begin
        Dest[3] := $0;
    end;
end;

procedure StringToLanguageID(const Source: String; out Dest: TLanguageID);
begin
    Dest[0] := $20;
    Dest[1] := $20;
    Dest[2] := $20;
    if Length(Source) > 0 then begin
        Dest[0] := Byte(Source[StrLow]);
    end;
    if Length(Source) > 1 then begin
        Dest[1] := Byte(Source[StrLow + 1]);
    end;
    if Length(Source) > 2 then begin
        Dest[2] := Byte(Source[StrLow + 2]);
    end;
end;

procedure ClearFrameID(out Dest: TFrameID);
begin
     Dest[0] := $0;
     Dest[1] := $0;
     Dest[2] := $0;
     Dest[3] := $0;
end;

procedure ClearLanguageID(out Dest: TLanguageID);
begin
    Dest[0] := $20;
    Dest[1] := $20;
    Dest[2] := $20;
end;

function TID3v2Frames.SetAlbumColors(FrameIndex: Integer; TitleColor, TextColor: Cardinal): Boolean;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        Frames[FrameIndex].Stream.Write(TitleColor, 4);
        Frames[FrameIndex].Stream.Write(TextColor, 4);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetAENC(FrameIndex: Integer; OwnerIdentifier: String; PreviewStart, PreviewLength: Word; EncryptionInfo: TBytes): Boolean;
var
    OwnerIdentifierBytes: TBytes;
    DataByte: Byte;
    DataWord: Word;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        //* OwnerIdentifier
        OwnerIdentifierBytes := TEncoding.ANSI.GetBytes(OwnerIdentifier);
        Frames[FrameIndex].Stream.Write(OwnerIdentifierBytes[0], Length(OwnerIdentifierBytes));
        DataByte := $00;
        Frames[FrameIndex].Stream.Write(DataByte, 1);
        //* Preview
        DataWord := ReverseBytes16(PreviewStart);
        Frames[FrameIndex].Stream.Write(DataWord, 2);
        DataWord := ReverseBytes16(PreviewLength);
        Frames[FrameIndex].Stream.Write(DataWord, 2);
        //* EncryptionInfo
        Frames[FrameIndex].Stream.Write(EncryptionInfo[0], Length(EncryptionInfo));
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetAENC(FrameID: String; OwnerIdentifier: String; PreviewStart, PreviewLength: Word; EncryptionInfo: TBytes): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetAENC(Index, OwnerIdentifier, PreviewStart, PreviewLength, EncryptionInfo);
end;

function TID3v2Frames.SetAlbumColors(FrameID: String; TitleColor, TextColor: Cardinal): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetAlbumColors(Index, TitleColor, TextColor);
end;

procedure TID3v2Frames.SetAllFrameDataStreams;
var
    i: Integer;
begin
    for i := 0 to Frames.Count - 1 do begin
        //* Set CHAP and CTOC frame datas
        if Frames[i] is TID3v2FrameCHAP then begin
            (Frames[i] as TID3v2FrameCHAP).SubFrames.SetAllFrameDataStreams;
            (Frames[i] as TID3v2FrameCHAP).SetData;

        end;
        if Frames[i] is TID3v2FrameCTOC then begin
            (Frames[i] as TID3v2FrameCTOC).SubFrames.SetAllFrameDataStreams;
            (Frames[i] as TID3v2FrameCTOC).SetData;
        end;
    end;
end;

function TID3v2Frames.GetAlbumColors(FrameID: String; var TitleColor, TextColor: Cardinal): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := GetAlbumColors(Index, TitleColor, TextColor);
end;

function TID3v2Frames.GetAlbumColors(FrameIndex: Integer; var TitleColor, TextColor: Cardinal): Boolean;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Frames[FrameIndex].Stream.Read2(TitleColor, 4);
        Frames[FrameIndex].Stream.Read2(TextColor, 4);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.SetTLEN(FrameID: String; TLEN: Integer): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetTLEN(Index, TLEN);
end;

function TID3v2Frames.SetTLEN(FrameIndex: Integer; TLEN: Integer): Boolean;
var
    TLENString: String;
    i: Integer;
    Data: Byte;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        Data := 0;
        Frames[FrameIndex].Stream.Write(Data, 1);
        TLENString := IntToStr(TLEN);
        for i := StrLow to StrLow + Length(TLENString) - 1 do begin
            Data := Ord(TLENString[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetPlayCount(FrameID: String): Cardinal;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := 0;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := GetPlayCount(Index);
end;

function TID3v2Tag.GetPlayTime: Double;
var
    TLEN: String;
    ID3v1Tag: TID3v1Tag;
    ID3v1Size: Integer;
    SamplesPerFrame: Integer;
begin
    if FPlayTime = 0 then begin
        //* First check if there's a 'TLEN' frame
        if UseTLENForPlaytime then begin
            TLEN := GetText('TLEN');
            if TLEN <> '' then begin
                FPlayTime := StrToIntDef(TLEN, 0) / 1000;
            end;
        end;
        if FPlayTime = 0 then begin
            if SourceFileType = sftMPEG then begin
                //* We have MPEG frame count
                if MPEGInfo.FrameCount > 0 then begin
                    SamplesPerFrame := 0;
                    case MPEGInfo.Version of
                        tmpegv1: begin
                            case MPEGInfo.Layer of
                                tmpegl1: begin
                                    SamplesPerFrame := 384;
                                end;
                                tmpegl2, tmpegl3: begin
                                    SamplesPerFrame := 1152;
                                end;
                            end;
                        end;
                        tmpegv2, tmpegv25: begin
                            case MPEGInfo.Layer of
                                tmpegl1: begin
                                    SamplesPerFrame := 384;
                                end;
                                tmpegl2: begin
                                    SamplesPerFrame := 1152;
                                end;
                                tmpegl3: begin
                                    SamplesPerFrame := 576;
                                end;
                            end;
                        end;
                    end;
                    FPlayTime := (MPEGInfo.FrameCount * SamplesPerFrame - MPEGInfo.EncoderDelay - MPEGInfo.EncoderPadding) / MPEGInfo.SampleRate;
                //* Try to guess from file size and bit rate
                end else begin
                    ID3v1Size := 0;
                    ID3v1Tag := TID3v1Tag.Create;
                    try
                        ID3v1Tag.LoadFromFile(Self.SourceFileName);
                        if ID3v1Tag.Loaded then begin
                            ID3v1Size := 128 + ID3v1Tag.LyricsSize;
                        end;
                    finally
                        FreeAndNil(ID3v1Tag);
                    end;
                    if Self.Bitrate > 0 then begin
                        FPlayTime := ((Self.FileSize - MPEGInfo.Position - ID3v1Size - APEv2TagSize) / Self.Bitrate * 8) / 1000;
                    end else begin
                        FPlayTime := 0;
                    end;
                end;
            end;
        end;
    end;
    Result := FPlayTime;
end;

function TID3v2Frames.GetFrameCount: Int64;
begin
    Result := Frames.Count;
end;

function TID3v2Frames.GetFrameIndexOfCoverArtIndex(CoverArtIndex: Integer): Integer;
var
    i: Integer;
    Counter: Integer;
begin
    Result := - 1;
    Counter := 0;
    for i := 0 to Frames.Count - 1 do begin
        if IsSameFrameID(Frames[i].ID, 'APIC') then begin
            if Counter = CoverArtIndex then begin
                Result := i;
                Break;
            end;
            Inc(Counter);
        end;
    end;
end;

function TID3v2Tag.GetFramePlayTime: Double;
var
    SamplesPerFrame: Integer;
begin
    Result := 0;
    if SourceFileType = sftMPEG then begin
        SamplesPerFrame := 0;
        case MPEGInfo.Version of
            tmpegv1: begin
                case MPEGInfo.Layer of
                    tmpegl1: begin
                        SamplesPerFrame := 384;
                    end;
                    tmpegl2, tmpegl3: begin
                        SamplesPerFrame := 1152;
                    end;
                end;
            end;
            tmpegv2, tmpegv25: begin
                case MPEGInfo.Layer of
                    tmpegl1: begin
                        SamplesPerFrame := 384;
                    end;
                    tmpegl2: begin
                        SamplesPerFrame := 1152;
                    end;
                    tmpegl3: begin
                        SamplesPerFrame := 576;
                    end;
                end;
            end;
        end;
        Result := SamplesPerFrame / MPEGInfo.SampleRate;
        if Result = 0 then begin
            Result := 0.026;
        end;
    end;
end;

function TID3v2Frames.GetPlayCount(FrameIndex: Integer): Cardinal;
var
    DataByte: Byte;
    i: Integer;
    Value: Cardinal;
begin
    Result := 0;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    if Frames[FrameIndex].Stream.Size = 0 then begin
        Exit;
    end;
    try
        try
            Value := 0;
            Frames[FrameIndex].Stream.Seek(0, soBeginning);
            for i := 0 to Frames[FrameIndex].Stream.Size - 1 do begin
                Value := Value SHL 8;
                Frames[FrameIndex].Stream.Read2(DataByte, 1);
                Value := Value + DataByte;
            end;
            Result := Value;
        except
            Result := High(Cardinal);
        end;
    except
        //*
    end;
end;

function TID3v2Frames.SetPlayCount(FrameID: String; PlayCount: Cardinal): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetPlayCount(Index, PlayCount);
end;

function TID3v2Frames.SetPlayCount(FrameIndex: Integer; PlayCount: Cardinal): Boolean;
var
    Data: Byte;
    Value: Cardinal;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        Value := PlayCount SHR 24;
        Data := Value;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Value := PlayCount SHL 8;
        Value := Value SHR 24;
        Data := Value;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Value := PlayCount SHL 16;
        Value := Value SHR 24;
        Data := Value;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Value := PlayCount SHL 24;
        Value := Value SHR 24;
        Data := Value;
        Frames[FrameIndex].Stream.Write(Data, 1);
        Result := True;
    except
        //*
    end;
end;

function ReverseBytes16(AWord: Word): Word; register;
    {
asm
    xchg al,ah
    }
begin
    Result := Swap(AWord);
end;

function TID3v2Tag.RemoveUnsynchronisationOnAllFrames: Boolean;
var
    i: Integer;
begin
    Result := False;
    try
        if MajorVersion = 3 then begin
            if Unsynchronised then begin
                for i := 0 to Frames.Count - 1 do begin
                    Frames[i].RemoveUnsynchronisation;
                end;
                Unsynchronised := False;
            end;
        end;
        if MajorVersion = 4 then begin
            for i := 0 to Frames.Count - 1 do begin
                if Frames[i].Unsynchronised then begin
                    Frames[i].RemoveUnsynchronisation;
                end;
            end;
            Unsynchronised := False;
        end;
        Result := True;
    except
        //*
    end;
end;

function TID3v2Tag.ApplyUnsynchronisationOnAllFrames: Boolean;
var
    i: Integer;
begin
    Result := False;
    try
        if MajorVersion = 3 then begin
            for i := 0 to Frames.Count - 1 do begin
                Frames[i].ApplyUnsynchronisation;
            end;
            Unsynchronised := True;
        end;
        if MajorVersion = 4 then begin
            for i := 0 to Frames.Count - 1 do begin
                if NOT Frames[i].Unsynchronised then begin
                    Frames[i].ApplyUnsynchronisation;
                end;
            end;
            Unsynchronised := True;
        end;
        Result := True;
    except
        //*
    end;
end;

function TID3v2Tag.DeCompressAllFrames: Boolean;
var
    i: Integer;
begin
    Result := False;
    try
        for i := 0 to Frames.Count - 1 do begin
            if Frames[i].Compressed then begin
                Frames[i].DeCompress;
            end;
        end;
        Result := True;
    except
        //*
    end;
end;

function APICType2Str(PictureType: Integer): String;
begin
    Result := 'Other';
    if PictureType = $00 then begin
        Result := 'Other';
        Exit;
    end;
    if PictureType = $01 then begin
        Result := '32x32 pixels "file icon" (PNG only)';
        Exit;
    end;
    if PictureType = $02 then begin
        Result := 'Other file icon';
        Exit;
    end;
    if PictureType = $03 then begin
        Result := 'Cover (front)';
        Exit;
    end;
    if PictureType = $04 then begin
        Result := 'Cover (back)';
        Exit;
    end;
    if PictureType = $05 then begin
        Result := 'Leaflet page';
        Exit;
    end;
    if PictureType = $06 then begin
        Result := 'Media (e.g. label side of CD)';
        Exit;
    end;
    if PictureType = $07 then begin
        Result := 'Lead artist/lead performer/soloist';
        Exit;
    end;
    if PictureType = $08 then begin
        Result := 'Artist/performer';
        Exit;
    end;
    if PictureType = $09 then begin
        Result := 'Conductor';
        Exit;
    end;
    if PictureType = $0A then begin
        Result := 'Band/orchestra';
        Exit;
    end;
    if PictureType = $0B then begin
        Result := 'Composer';
    end;
    if PictureType = $0C then begin
        Result := 'Lyricist/text writer';
        Exit;
    end;
    if PictureType = $0D then begin
        Result := 'Recording location';
        Exit;
    end;
    if PictureType = $0E then begin
        Result := 'During recording';
        Exit;
    end;
    if PictureType = $0F then begin
        Result := 'During performance';
        Exit;
    end;
    if PictureType = $10 then begin
        Result := 'Movie/video screen capture';
        Exit;
    end;
    if PictureType = $11 then begin
        Result := 'A bright coloured fish';
        Exit;
    end;
    if PictureType = $12 then begin
        Result := 'Illustration';
        Exit;
    end;
    if PictureType = $13 then begin
        Result := 'Band/artist logotype';
        Exit;
    end;
    if PictureType = $14 then begin
        Result := 'Publisher/studio logotype';
        Exit;
    end;
end;

function APICTypeStr2No(PictureType: String): Integer;
begin
    Result := $00;
    if PictureType = 'Other' then begin
        Result := $00;
        Exit;
    end;
    if PictureType = '32x32 pixels ''file icon'' (PNG only)' then begin
        Result := $01;
        Exit;
    end;
    if PictureType = 'Other file icon' then begin
        Result := $02;
        Exit;
    end;
    if PictureType = 'Cover (front)' then begin
        Result := $03;
        Exit;
    end;
    if PictureType = 'Cover (back)' then begin
        Result := $04;
        Exit;
    end;
    if PictureType = 'Leaflet page' then begin
        Result := $05;
        Exit;
    end;
    if PictureType = 'Media (e.g. label side of CD)' then begin
        Result := $06;
        Exit;
    end;
    if PictureType = 'Lead artist/lead performer/soloist' then begin
        Result := $07;
        Exit;
    end;
    if PictureType = 'Artist/performer' then begin
        Result := $08;
        Exit;
    end;
    if PictureType = 'Conductor' then begin
        Result := $09;
        Exit;
    end;
    if PictureType = 'Band/Orchestra' then begin
        Result := $0A;
        Exit;
    end;
    if PictureType = 'Composer' then begin
        Result := $0B;
    end;
    if PictureType = 'Lyricist/text writer' then begin
        Result := $0C;
        Exit;
    end;
    if PictureType = 'Recording Location' then begin
        Result := $0D;
        Exit;
    end;
    if PictureType = 'During recording' then begin
        Result := $0E;
        Exit;
    end;
    if PictureType = 'During performance' then begin
        Result := $0F;
        Exit;
    end;
    if PictureType = 'Movie/video screen capture' then begin
        Result := $10;
        Exit;
    end;
    if PictureType = 'A bright coloured fish' then begin
        Result := $11;
        Exit;
    end;
    if PictureType = 'Illustration' then begin
        Result := $12;
        Exit;
    end;
    if PictureType = 'Band/artist logotype' then begin
        Result := $13;
        Exit;
    end;
    if PictureType = 'Publisher/Studio logotype' then begin
        Result := $14;
        Exit;
    end;
end;

function RemoveID3v2TagFromFile(FileName: String; PreserveFileDateTime: Boolean = ID3V2LIBRARY_DEFAULT_PRESERVE_FILE_DATETIME): Integer;
var
    FileDateTime: TDateTime;
    AudioFileName: String;
    AudioFile: TFileStream;
    OutputFileName: String;
    OutputFile: TFileStream;
    ID3v2Size: Integer;
    TagCodedSizeInExistingStream: Cardinal;
    TagSizeInExistingStream: Cardinal;
    Action: Integer;
begin
    //Result := ID3V2LIBRARY_ERROR;
    AudioFile := nil;
    if NOT FileExists(FileName) then begin
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    ID3v2Size := 0;
    Action := ID3v2TLOFEANNONE;
    try
        try
            Result := ID3V2LIBRARY_ERROR_EMPTY_TAG;
            AudioFileName := FileName;
            try
                if PreserveFileDateTime then begin
                    FileAge(FileName, FileDateTime);
                end;

                if FileIsReadOnly(FileName) then begin
                    if Assigned(ID3v2LibraryOpenFileExceptionCallback) then begin
                        ID3v2LibraryOpenFileExceptionCallback(FileName, nil, Action);
                        case Action of
                            ID3v2TLOFEANNONE: begin
                                Result := ID3V2LIBRARY_ERROR_FILE_READ_ONLY;
                                Exit;
                            end;
                            ID3v2TLOFEANDEPROTECT, ID3v2TLOFEANDEPROTECTANDRESTORE: begin
                                FileSetReadOnly(FileName, False);
                            end;
                        end;
                    end else begin
                        Result := ID3V2LIBRARY_ERROR_FILE_READ_ONLY;
                        Exit;
                    end;
                end;

                try
                    AudioFile := TFileStream.Create(AudioFileName, fmOpenRead);
                except
                    Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
                    Exit;
                end;
                if ID3v2ValidTag(AudioFile) then begin
                    //* Skip version data and flags
                    AudioFile.Seek(3, soCurrent);
                    AudioFile.Read2(TagCodedSizeInExistingStream, 4);
                    UnSyncSafe(TagCodedSizeInExistingStream, 4, TagSizeInExistingStream);
                    //* Add header size to size
                    ID3v2Size := TagSizeInExistingStream + 10;
                end else begin
                    AudioFile.Seek(0, soBeginning);
                    if CheckRIFF(AudioFile) then begin
                        if SeekRIFF(AudioFile) > 0 then begin
                            FreeAndNil(AudioFile);
                            Result := RemoveRIFFID3v2FromFile(FileName);
                            Exit;
                        end else begin
                            Exit;
                        end;
                    end else begin
                        AudioFile.Seek(0, soBeginning);
                        if CheckRF64(AudioFile) then begin
                            if SeekRF64(AudioFile) > 0 then begin
                                FreeAndNil(AudioFile);
                                Result := RemoveRF64ID3v2FromFile(FileName);
                                Exit;
                            end else begin
                                Exit;
                            end;
                        end else begin
                            AudioFile.Seek(0, soBeginning);
                            if CheckAIFF(AudioFile) then begin
                                if SeekAIFF(AudioFile) > 0 then begin
                                    FreeAndNil(AudioFile);
                                    Result := RemoveAIFFID3v2FromFile(FileName);
                                    Exit;
                                end else begin
                                    Exit;
                                end;
                            end else begin
                                AudioFile.Seek(0, soBeginning);
                                if CheckDSF(AudioFile) then begin
                                    if SeekDSF(AudioFile) > 0 then begin
                                        FreeAndNil(AudioFile);
                                        Result := RemoveDSFID3v2FromFile(FileName);
                                        Exit;
                                    end else begin
                                        Exit;
                                    end;
                                end else begin
                                    AudioFile.Seek(0, soBeginning);
                                    if CheckDFF(AudioFile) then begin
                                        if SeekDFF(AudioFile) > 0 then begin
                                            FreeAndNil(AudioFile);
                                            Result := RemoveDFFID3v2FromFile(FileName);
                                            Exit;
                                        end else begin
                                            Exit;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            finally
                if Assigned(AudioFile) then begin
                    FreeAndNil(AudioFile);
                end;
            end;
            //ID3v2Size := Size + 10;
            if ID3v2Size > 0 then begin
                try
                    AudioFile := TFileStream.Create(AudioFileName, fmOpenRead);
                except
                    Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
                    Exit;
                end;
                try
                    OutputFileName := ChangeFileExt(AudioFileName, '.tmp');
                    try
                        OutputFile := TFileStream.Create(OutputFileName, fmCreate OR fmOpenReadWrite);
                    except
                        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
                        Exit;
                    end;
                    try
                        AudioFile.Seek(ID3v2Size, soBeginning);
                        OutputFile.CopyFrom(AudioFile, AudioFile.Size - ID3v2Size);
                    finally
                        {$IFDEF NEXTGEN}
                        OutputFile.DisposeOf;
                        OutputFile := nil;
                        {$ELSE}
                        FreeAndNil(OutputFile);
                        {$ENDIF}
                    end;
                finally
                    {$IFDEF NEXTGEN}
                    AudioFile.DisposeOf;
                    AudioFile := nil;
                    {$ELSE}
                    FreeAndNil(AudioFile);
                    {$ENDIF}
                end;
                if NOT SysUtils.DeleteFile(AudioFileName) then begin
                    Result := GetLastError;
                    SysUtils.DeleteFile(OutputFileName);
                end else begin
                    RenameFile(OutputFileName, AudioFileName);
                    Result := ID3V2LIBRARY_SUCCESS;
                end;
            end;
        except
            Result := ID3V2LIBRARY_ERROR;
        end;
    finally
        if PreserveFileDateTime then begin
            FileSetDate(FileName, DateTimeToFileDate(FileDateTime));
        end;
        if Action = ID3v2TLOFEANDEPROTECTANDRESTORE then begin
            FileSetReadOnly(FileName, True);
        end;
    end;
end;

function RemoveID3v2TagFromStream(Stream: TStream): Integer;
var
    TempStream: TStream;
    ID3v2Size: Integer;
    TagCodedSizeInExistingStream: Cardinal;
    TagSizeInExistingStream: Cardinal;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    if Stream.Size = 0 then begin
        Exit;
    end;
    ID3v2Size := 0;
    try
        Result := ID3V2LIBRARY_ERROR_EMPTY_TAG;
        Stream.Seek(0, soBeginning);
        if ID3v2ValidTag(Stream) then begin
            //* Skip version data and flags
            Stream.Seek(3, soCurrent);
            Stream.Read2(TagCodedSizeInExistingStream, 4);
            UnSyncSafe(TagCodedSizeInExistingStream, 4, TagSizeInExistingStream);
            //* Add header size to size
            ID3v2Size := TagSizeInExistingStream + 10;
        end else begin
            Stream.Seek(0, soBeginning);
            if CheckRIFF(Stream) then begin
                if SeekRIFF(Stream) > 0 then begin
                    Result := RemoveRIFFID3v2FromStream(Stream);
                    Exit;
                end else begin
                    Exit;
                end;
            end else begin
                Stream.Seek(0, soBeginning);
                if CheckRF64(Stream) then begin
                    if SeekRF64(Stream) > 0 then begin
                        Result := RemoveRF64ID3v2FromStream(Stream);
                        Exit;
                    end else begin
                        Exit;
                    end;
                end else begin
                    Stream.Seek(0, soBeginning);
                    if CheckAIFF(Stream) then begin
                        if SeekAIFF(Stream) > 0 then begin
                            Result := RemoveAIFFID3v2FromStream(Stream);
                            Exit;
                        end else begin
                            Exit;
                        end;
                    end else begin
                        Stream.Seek(0, soBeginning);
                        if CheckDSF(Stream) then begin
                            if SeekDSF(Stream) > 0 then begin
                                Result := RemoveDSFID3v2FromStream(Stream);
                                Exit;
                            end else begin
                                Exit;
                            end;
                        end else begin
                            Stream.Seek(0, soBeginning);
                            if CheckDFF(Stream) then begin
                                if SeekDFF(Stream) > 0 then begin
                                    Result := RemoveDFFID3v2FromStream(Stream);
                                    Exit;
                                end else begin
                                    Exit;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
        end;
        if ID3v2Size > 0 then begin
            TempStream := TMemoryStream.Create;
            try
                Stream.Seek(ID3v2Size, soBeginning);
                try
                    TempStream.CopyFrom(Stream, Stream.Size - ID3v2Size);
                except
                    Exit;
                end;
                Stream.Size := 0;
                Stream.CopyFrom(TempStream, 0);
                Stream.Seek(0, soBeginning);
                Result := ID3V2LIBRARY_SUCCESS;
            finally
                FreeAndNil(TempStream);
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function ID3v2ValidTag(Stream: TStream): Boolean;
var
    Identification: TID3v2ID;
begin
    Result := False;
    try
        Stream.Read2(Identification[0], 3);
        if (Identification[0] = ID3v2ID[0])
        AND (Identification[1] = ID3v2ID[1])
        AND (Identification[2] = ID3v2ID[2])
        then begin
            Result := True;
        end;
    except
        //*
    end;
end;

function CheckRIFF(Stream: TStream): Boolean;
var
    Identification: TRIFFID;
begin
    Result := False;
    try
        Stream.Read2(Identification[0], 4);
        if (Identification[0] = RIFFID[0])
        AND (Identification[1] = RIFFID[1])
        AND (Identification[2] = RIFFID[2])
        AND (Identification[3] = RIFFID[3])
        then begin
            Result := True;
        end;
    except
        Result := False;
    end;
end;

function SeekRIFF(Stream: TStream): Integer;
var
    RIFFChunkSize: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Read2(RIFFChunkSize, 4);
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (
                (ChunkID[0] = RIFFID3v2ID[0])
                AND (ChunkID[1] = RIFFID3v2ID[1])
                AND (ChunkID[2] = RIFFID3v2ID[2])
                AND (ChunkID[3] = RIFFID3v2ID[3])
                )
                OR
                (
                (ChunkID[0] = RIFFID3v2ID2[0])
                AND (ChunkID[1] = RIFFID3v2ID2[1])
                AND (ChunkID[2] = RIFFID3v2ID2[2])
                AND (ChunkID[3] = RIFFID3v2ID2[3])
                )
                then begin
                    Result := ChunkSize;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        end;
    except
        Result := 0;
    end;
end;

function CheckAIFF(Stream: TStream): Boolean;
var
    Identification: TAIFFID;
begin
    Result := False;
    try
        Stream.Read2(Identification[0], 4);
        if (Identification[0] = AIFFID[0])
        AND (Identification[1] = AIFFID[1])
        AND (Identification[2] = AIFFID[2])
        AND (Identification[3] = AIFFID[3])
        then begin
            Result := True;
        end;
    except
        Result := False;
    end;
end;

function SeekAIFF(Stream: TStream): Integer;
var
    AIFFChunkSize: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Read2(AIFFChunkSize, 4);
        AIFFChunkSize := ReverseBytes32(AIFFChunkSize);
        Stream.Read2(ChunkID, 4);
        if ((ChunkID[0] = AIFFChunkID[0])
        AND (ChunkID[1] = AIFFChunkID[1])
        AND (ChunkID[2] = AIFFChunkID[2])
        AND (ChunkID[3] = AIFFChunkID[3]))
        OR ((ChunkID[0] = AIFCChunkID[0])
        AND (ChunkID[1] = AIFCChunkID[1])
        AND (ChunkID[2] = AIFCChunkID[2])
        AND (ChunkID[3] = AIFCChunkID[3]))
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                ChunkSize := ReverseBytes32(ChunkSize);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (ChunkID[0] = AIFFID3v2ID[0])
                AND (ChunkID[1] = AIFFID3v2ID[1])
                AND (ChunkID[2] = AIFFID3v2ID[2])
                AND (ChunkID[3] = AIFFID3v2ID[3])
                then begin
                    Result := ChunkSize;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        end;
    except
        Result := 0;
    end;
end;

function CheckRF64(Stream: TStream): Boolean;
var
    Identification: TRIFFID;
begin
    Result := False;
    try
        Stream.Read2(Identification[0], 4);
        if (Identification[0] = RF64ID[0])
        AND (Identification[1] = RF64ID[1])
        AND (Identification[2] = RF64ID[2])
        AND (Identification[3] = RF64ID[3])
        then begin
            Result := True;
        end;
    except
        Result := False;
    end;
end;

function SeekRF64(Stream: TStream): Integer;
var
    RIFFChunkSize: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    ds64DataSize: Int64;
    Waveds64: TWaveds64;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Read2(RIFFChunkSize, 4);
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                if (ChunkID[0] = Ord('d'))
                AND (ChunkID[1] = Ord('s'))
                AND (ChunkID[2] = Ord('6'))
                AND (ChunkID[3] = Ord('4'))
                then begin
                    Stream.Read2(Waveds64, SizeOf(TWaveds64));
                    Stream.Seek(Waveds64.ds64Size - SizeOf(TWaveds64) + 4 {table?}, soCurrent);
                    Continue;
                end;
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (
                (ChunkID[0] = RIFFID3v2ID[0])
                AND (ChunkID[1] = RIFFID3v2ID[1])
                AND (ChunkID[2] = RIFFID3v2ID[2])
                AND (ChunkID[3] = RIFFID3v2ID[3])
                )
                OR
                (
                (ChunkID[0] = RIFFID3v2ID2[0])
                AND (ChunkID[1] = RIFFID3v2ID2[1])
                AND (ChunkID[2] = RIFFID3v2ID2[2])
                AND (ChunkID[3] = RIFFID3v2ID2[3])
                )
                then begin
                    Result := ChunkSize;
                    Exit;
                end else begin
                    if ((ChunkID[0] = Ord('d'))
                    AND (ChunkID[1] = Ord('a'))
                    AND (ChunkID[2] = Ord('t'))
                    AND (ChunkID[3] = Ord('a')))
                    AND (ChunkSize = $FFFFFFFF)
                    then begin
                        ds64DataSize := MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh);
                        Stream.Seek(ds64DataSize, soCurrent);
                    end else begin
                        Stream.Seek(ChunkSize, soCurrent);
                    end;
                end;
            end;
        end;
    except
        Result := 0;
    end;
end;

function CheckDSF(Stream: TStream): Boolean;
var
    Identification: TRIFFID;
begin
    Result := False;
    try
        Stream.Read2(Identification[0], 4);
        if (Identification[0] = DSFID[0])
        AND (Identification[1] = DSFID[1])
        AND (Identification[2] = DSFID[2])
        AND (Identification[3] = DSFID[3])
        then begin
            Result := True;
        end;
    except
        Result := False;
    end;
end;

function SeekDSF(Stream: TStream): Integer;
var
    ID3v2Pointer: UInt64;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Seek(16, soCurrent);
        Stream.Read2(ID3v2Pointer, 8);
        //ID3v2Pointer := ReverseBytes64(ID3v2Pointer);
        if ID3v2Pointer > 0 then begin
            Stream.Seek(ID3v2Pointer, soBeginning);
            Result := GetID3v2Size(Stream);
        end;
    except
        Result := 0;
    end;
end;

function TID3v2Tag.LoadDSFInfo(Stream: TStream): Boolean;
var
    PreviousPosition: Int64;
    ChunkSize: UInt64;
    ChunkID: TRIFFID;
    ChannelType: Cardinal;
begin
    Result := False;
    PreviousPosition := Stream.Position;
    try
        Stream.Seek(4, soBeginning);
        Stream.Read2(ChunkSize, 8);
        if Odd(ChunkSize) then begin
            Inc(ChunkSize);
        end;
        Stream.Seek(ChunkSize - 12, soCurrent);
        Stream.Read2(ChunkID[0], 4);
        if (ChunkID[0] = DSFfmt_ID[0])
        AND (ChunkID[1] = DSFfmt_ID[1])
        AND (ChunkID[2] = DSFfmt_ID[2])
        AND (ChunkID[3] = DSFfmt_ID[3])
        then begin
            Stream.Seek(8, soCurrent);
            Stream.Read2(DSFInfo.FormatVersion, 4);
            Stream.Read2(DSFInfo.FormatID, 4);
            Stream.Read2(ChannelType, 4);
            case ChannelType of
                1: DSFInfo.ChannelType := dsfctMono;
                2: DSFInfo.ChannelType := dsfctStereo;
                3: DSFInfo.ChannelType := dsfct3Channels;
                4: DSFInfo.ChannelType := dsfctQuad;
                5: DSFInfo.ChannelType := dsfct4Channels;
                6: DSFInfo.ChannelType := dsfct5Channels;
                7: DSFInfo.ChannelType := dsfct51Channels;
            else
                DSFInfo.ChannelType := dsfctUnknown;
            end;
            Stream.Read2(DSFInfo.ChannelNumber, 4);
            Stream.Read2(DSFInfo.SamplingFrequency, 4);
            Stream.Read2(DSFInfo.BitsPerSample, 4);
            Stream.Read2(DSFInfo.SampleCount, 8);
            Stream.Read2(DSFInfo.BlockSizePerChannel, 4);
            //* Calculate playtime
            DSFInfo.PlayTime := DSFInfo.SampleCount / DSFInfo.SamplingFrequency;
            //* Set attrbiutes
            FPlayTime := DSFInfo.PlayTime;
            FSampleCount := DSFInfo.SampleCount;
            FBitRate := DSFInfo.GetBitRate;
            Result := True;
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function CheckDFF(Stream: TStream): Boolean;
var
    Identification: TRIFFID;
begin
    Result := False;
    try
        Stream.Read2(Identification[0], 4);
        if (Identification[0] = DFFID[0])
        AND (Identification[1] = DFFID[1])
        AND (Identification[2] = DFFID[2])
        AND (Identification[3] = DFFID[3])
        then begin
            Stream.Seek(8, soCurrent);
            Stream.Read2(Identification[0], 4);
            if (Identification[0] = DFFType[0])
            AND (Identification[1] = DFFType[1])
            AND (Identification[2] = DFFType[2])
            AND (Identification[3] = DFFType[3])
            then begin
                Result := True;
            end;
        end;
    except
        Result := False;
    end;
end;

function SeekDFF(Stream: TStream): Integer;
var
    FRM8ChunkSize: UInt64;
    Identification: TRIFFID;
    ChunkSize: UInt64;
begin
    Result := 0;
    try
        //* Find ID3v2
        Stream.Seek(4, soBeginning);
        Stream.Read2(FRM8ChunkSize, 8);
        if Odd(FRM8ChunkSize) then begin
            Inc(FRM8ChunkSize);
        end;
        FRM8ChunkSize := ReverseBytes64(FRM8ChunkSize);
        Stream.Seek(4, soCurrent);
        repeat
            Stream.Read2(Identification[0], 4);
            Stream.Read2(ChunkSize, 8);
            ChunkSize := ReverseBytes64(ChunkSize);
            if Odd(ChunkSize) then begin
                Inc(ChunkSize);
            end;
            if (Identification[0] = DFFID3v2ID[0])
            AND (Identification[1] = DFFID3v2ID[1])
            AND (Identification[2] = DFFID3v2ID[2])
            AND (Identification[3] = DFFID3v2ID[3])
            then begin
                Result := ChunkSize;
                Break;
            end else begin
                if Odd(ChunkSize) then begin
                    Stream.Seek(ChunkSize + 1, soCurrent);
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        until Stream.Position >= FRM8ChunkSize + 12;
    except
        Result := 0;
    end;
end;

  // Use CalcCRC32 as a procedure so CRCValue can be passed in but
  // also returned. This allows multiple calls to CalcCRC32 for
  // the "same" CRC-32 calculation.
procedure CalcCRC32(P: Pointer; ByteCount: DWORD; var CRCValue: DWORD);
  // The following is a little cryptic (but executes very quickly).
  // The algorithm is as follows:
  // 1. exclusive-or the input byte with the low-order byte of
  // the CRC register to get an INDEX
  // 2. shift the CRC register eight bits to the right
  // 3. exclusive-or the CRC register with the contents of Table[INDEX]
  // 4. repeat steps 1 through 3 for all bytes
var
    i: DWORD;
    q: ^BYTE;
begin
    q := p;
    for i := 0 to ByteCount - 1 do begin
        CRCvalue := (CRCvalue SHR 8) XOR CRC32Table[q^ XOR (CRCvalue AND $000000FF)];
        Inc(q)
    end;
end;

function CalculateStreamCRC32(Stream: TStream; var CRCvalue: DWORD): Boolean;
var
    MemoryStream: TMemoryStream;
begin
    Result := False;
    CRCValue := $FFFFFFFF;
    MemoryStream := TMemoryStream(Stream);
    try
        MemoryStream.Seek(0, soBeginning);
        if MemoryStream.Size > 0 then begin
            CalcCRC32(MemoryStream.Memory, MemoryStream.Size, CRCvalue);
            Result := True;
        end;
    except
        Result := False;
    end;
    CRCvalue := NOT CRCvalue;
end;

function TID3v2Tag.CalculateTagCRC32: Cardinal;
var
    CRC32: Cardinal;
    TagsStream: TStream;
    WriteError: Integer;
    ReUnsynchronise: Boolean;
begin
    Result := 0;
    ReUnsynchronise := Unsynchronised;
    TagsStream := TMemoryStream.Create;
    try
        if ReUnsynchronise then begin
            RemoveUnsynchronisationOnAllFrames;
        end;
        //SetAllFrameDataStreams;
        WriteError := WriteAllFrames(TagsStream);
        if WriteError <> ID3V2LIBRARY_SUCCESS then begin
            Exit;
        end;
        CalculateStreamCRC32(TagsStream, CRC32);
        Result := CRC32;
    finally
        FreeAndNil(TagsStream);
        if ReUnsynchronise then begin
            ApplyUnsynchronisationOnAllFrames;
        end;
    end;
end;

function RIFFCreateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PaddingToWrite: Cardinal): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TotalSize: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        Stream.Seek(4, soCurrent);
        Stream.Read2(RIFFChunkSize, 4);
        Stream.Seek(- 4, soCurrent);
        TotalSize := Int64(RIFFChunkSize) + Int64(WriteTagTotalSize) + Int64(PaddingToWrite) + 8;
        if Odd(TotalSize) then begin
            Inc(TotalSize);
        end;
        if TotalSize > $FFFFFFFF then begin
            Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
            Exit;
        end;
        RIFFChunkSizeNew := TotalSize;
        Stream.Write(RIFFChunkSizeNew, 4);
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            while Stream.Position + 8 < RIFFChunkSize do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                Stream.Seek(ChunkSize, soCurrent);
            end;
            if Stream.Position < Stream.Size then begin
                PreviousPosition := Stream.Position;
                if FileName <> '' then begin
                    try
                        TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                    except
                        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                        Exit;
                    end;
                end else begin
                    TempStream := TMemoryStream.Create;
                end;
                TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                Stream.Seek(PreviousPosition, soBeginning);
            end;
            Stream.Write(RIFFID3v2ID[0], 4);
            ChunkSize := WriteTagTotalSize + PaddingToWrite;
            if Odd(ChunkSize) then begin
                Inc(ChunkSize);
            end;
            Stream.Write(ChunkSize, 4);
            PreviousPosition := Stream.Position;
            WritePadding(Stream, ChunkSize);
            if Assigned(TempStream) then begin
                TempStream.Seek(0, soBeginning);
                Stream.CopyFrom(TempStream, TempStream.Size);
                {$IFDEF NEXTGEN}
                TempStream.DisposeOf;
                TempStream := nil;
                {$ELSE}
                FreeAndNil(TempStream);
                {$ENDIF}
                SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
            end;
            Stream.Seek(PreviousPosition, soBeginning);
            Result := ID3V2LIBRARY_SUCCESS;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RIFFUpdateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PreviousTagSize: Cardinal; PaddingToWrite: Cardinal): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TotalSize: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        Stream.Seek(4, soCurrent);
        Stream.Read2(RIFFChunkSize, 4);
        Stream.Seek(- 4, soCurrent);
        TotalSize := RIFFChunkSize - PreviousTagSize + WriteTagTotalSize + PaddingToWrite;
        if Odd(TotalSize) then begin
            Inc(TotalSize);
        end;
        if TotalSize > $FFFFFFFF then begin
            Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
            Exit;
        end;
        RIFFChunkSizeNew := TotalSize;
        Stream.Write(RIFFChunkSizeNew, 4);
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (
                (ChunkID[0] = RIFFID3v2ID[0])
                AND (ChunkID[1] = RIFFID3v2ID[1])
                AND (ChunkID[2] = RIFFID3v2ID[2])
                AND (ChunkID[3] = RIFFID3v2ID[3])
                )
                OR
                (
                (ChunkID[0] = RIFFID3v2ID2[0])
                AND (ChunkID[1] = RIFFID3v2ID2[1])
                AND (ChunkID[2] = RIFFID3v2ID2[2])
                AND (ChunkID[3] = RIFFID3v2ID2[3])
                )
                then begin
                    Stream.Seek(- 4, soCurrent);
                    PreviousPosition := Stream.Position;
                    Stream.Seek(Int64(ChunkSize) + 4, soCurrent);
                    if Stream.Position < Stream.Size then begin
                        if FileName <> '' then begin
                            try
                                TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                            except
                                Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                                Exit;
                            end;
                        end else begin
                            TempStream := TMemoryStream.Create;
                        end;
                        TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                    end;
                    Stream.Seek(PreviousPosition, soBeginning);
                    ChunkSize := ChunkSize - PreviousTagSize + WriteTagTotalSize + PaddingToWrite;
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    Stream.Write(ChunkSize, 4);
                    WritePadding(Stream, ChunkSize);
                    if Assigned(TempStream) then begin
                        TempStream.Seek(0, soBeginning);
                        Stream.CopyFrom(TempStream, TempStream.Size);
                        {$IFDEF NEXTGEN}
                        TempStream.DisposeOf;
                        TempStream := nil;
                        {$ELSE}
                        FreeAndNil(TempStream);
                        {$ENDIF}
                        SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
                    end;
                    Stream.Size := Stream.Position;
                    Stream.Seek(PreviousPosition + 4, soBeginning);
                    Result := ID3V2LIBRARY_SUCCESS;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function AIFFCreateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PaddingToWrite: Cardinal): Integer;
var
    AIFFChunkSize: DWord;
    AIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    ChunkSizeNew: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    ZeroByte: Byte;
    TotalSize: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        Stream.Seek(4, soCurrent);
        Stream.Read2(AIFFChunkSize, 4);
        AIFFChunkSize := ReverseBytes32(AIFFChunkSize);
        Stream.Seek(- 4, soCurrent);
        TotalSize := Int64(AIFFChunkSize) + Int64(WriteTagTotalSize) + Int64(PaddingToWrite) + 8;
        if Odd(TotalSize) then begin
            Inc(TotalSize);
        end;
        if TotalSize > $FFFFFFFF then begin
            Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
            Exit;
        end;
        AIFFChunkSizeNew := TotalSize;
        AIFFChunkSizeNew := ReverseBytes32(AIFFChunkSizeNew);
        Stream.Write(AIFFChunkSizeNew, 4);
        Stream.Read2(ChunkID, 4);
        if ((ChunkID[0] = AIFFChunkID[0])
        AND (ChunkID[1] = AIFFChunkID[1])
        AND (ChunkID[2] = AIFFChunkID[2])
        AND (ChunkID[3] = AIFFChunkID[3]))
        OR ((ChunkID[0] = AIFCChunkID[0])
        AND (ChunkID[1] = AIFCChunkID[1])
        AND (ChunkID[2] = AIFCChunkID[2])
        AND (ChunkID[3] = AIFCChunkID[3]))
        then begin
            while (Stream.Position + 8 < AIFFChunkSize)
            AND (Stream.Position + 8 < Stream.Size)
            do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                ChunkSize := ReverseBytes32(ChunkSize);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                Stream.Seek(ChunkSize, soCurrent);
            end;
            if Stream.Position < Stream.Size then begin
                PreviousPosition := Stream.Position;
                if FileName <> '' then begin
                    try
                        TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                    except
                        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                        Exit;
                    end;
                end else begin
                    TempStream := TMemoryStream.Create;
                end;
                TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                Stream.Seek(PreviousPosition, soBeginning);
            end;
            if Odd(Stream.Position) then begin
                ZeroByte := 0;
                Stream.Write(ZeroByte, 1);
            end;
            Stream.Write(AIFFID3v2ID[0], 4);
            ChunkSize := WriteTagTotalSize + PaddingToWrite;
            if Odd(ChunkSize) then begin
                Inc(ChunkSize);
            end;
            ChunkSizeNew := ReverseBytes32(ChunkSize);
            Stream.Write(ChunkSizeNew, 4);
            PreviousPosition := Stream.Position;
            WritePadding(Stream, ChunkSize);
            if Assigned(TempStream) then begin
                TempStream.Seek(0, soBeginning);
                Stream.CopyFrom(TempStream, TempStream.Size);
                {$IFDEF NEXTGEN}
                TempStream.DisposeOf;
                TempStream := nil;
                {$ELSE}
                FreeAndNil(TempStream);
                {$ENDIF}
                SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
            end;
            Stream.Seek(PreviousPosition, soBeginning);
            Result := ID3V2LIBRARY_SUCCESS;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function AIFFUpdateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PreviousTagSize: Cardinal; PaddingToWrite: Cardinal): Integer;
var
    AIFFChunkSize: DWord;
    AIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    ChunkSizeNew: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TotalSize: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        Stream.Seek(4, soCurrent);
        Stream.Read2(AIFFChunkSize, 4);
        AIFFChunkSize := ReverseBytes32(AIFFChunkSize);
        Stream.Seek(- 4, soCurrent);
        TotalSize := AIFFChunkSize - PreviousTagSize + WriteTagTotalSize + PaddingToWrite;
        if Odd(TotalSize) then begin
            Inc(TotalSize);
        end;
        if TotalSize > $FFFFFFFF then begin
            Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
            Exit;
        end;
        AIFFChunkSizeNew := TotalSize;
        AIFFChunkSizeNew := ReverseBytes32(AIFFChunkSizeNew);
        Stream.Write(AIFFChunkSizeNew, 4);
        Stream.Read2(ChunkID, 4);
        if ((ChunkID[0] = AIFFChunkID[0])
        AND (ChunkID[1] = AIFFChunkID[1])
        AND (ChunkID[2] = AIFFChunkID[2])
        AND (ChunkID[3] = AIFFChunkID[3]))
        OR ((ChunkID[0] = AIFCChunkID[0])
        AND (ChunkID[1] = AIFCChunkID[1])
        AND (ChunkID[2] = AIFCChunkID[2])
        AND (ChunkID[3] = AIFCChunkID[3]))
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                ChunkSize := ReverseBytes32(ChunkSize);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (ChunkID[0] = AIFFID3v2ID[0])
                AND (ChunkID[1] = AIFFID3v2ID[1])
                AND (ChunkID[2] = AIFFID3v2ID[2])
                AND (ChunkID[3] = AIFFID3v2ID[3])
                then begin
                    Stream.Seek(- 4, soCurrent);
                    PreviousPosition := Stream.Position;
                    Stream.Seek(Int64(ChunkSize) + 4, soCurrent);
                    if Stream.Position < Stream.Size then begin
                        if FileName <> '' then begin
                            try
                                TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                            except
                                Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                                Exit;
                            end;
                        end else begin
                            TempStream := TMemoryStream.Create;
                        end;
                        TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                    end;
                    Stream.Seek(PreviousPosition, soBeginning);
                    ChunkSize := ChunkSize - PreviousTagSize + WriteTagTotalSize + PaddingToWrite;
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    ChunkSizeNew := ReverseBytes32(ChunkSize);
                    Stream.Write(ChunkSizeNew, 4);
                    WritePadding(Stream, ChunkSize);
                    if Assigned(TempStream) then begin
                        TempStream.Seek(0, soBeginning);
                        Stream.CopyFrom(TempStream, TempStream.Size);
                        {$IFDEF NEXTGEN}
                        TempStream.DisposeOf;
                        TempStream := nil;
                        {$ELSE}
                        FreeAndNil(TempStream);
                        {$ENDIF}
                        SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
                    end;
                    Stream.Size := Stream.Position;
                    Stream.Seek(PreviousPosition + 4, soBeginning);
                    Result := ID3V2LIBRARY_SUCCESS;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RF64CreateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PaddingToWrite: Cardinal): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TotalSize: Int64;
    Waveds64: TWaveds64;
    Data: DWord;
    RF64Size: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    RF64Size := 0;
    try
        Stream.Seek(4, soCurrent);
        Stream.Read2(RIFFChunkSize, 4);
        if RIFFChunkSize = $FFFFFFFF then begin
            Stream.Read2(ChunkID, 4);
            if (ChunkID[0] <> Ord('W'))
            OR (ChunkID[1] <> Ord('A'))
            OR (ChunkID[2] <> Ord('V'))
            OR (ChunkID[3] <> Ord('E'))
            then begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            Stream.Read2(ChunkID, 4);
            if (ChunkID[0] = Ord('d'))
            AND (ChunkID[1] = Ord('s'))
            AND (ChunkID[2] = Ord('6'))
            AND (ChunkID[3] = Ord('4'))
            then begin
                Stream.Read2(Waveds64, SizeOf(TWaveds64));
                RF64Size := MakeInt64(Waveds64.RIFFSizeLow, Waveds64.RIFFSizeHigh);
                TotalSize := RF64Size + WriteTagTotalSize + PaddingToWrite + 8;
                if Odd(TotalSize) then begin
                    Inc(TotalSize);
                end;
                //* Set new RF64 size
                Stream.Position := 20;
                Data := LowDWordOfInt64(TotalSize);
                Stream.write(Data, 4);
                Data := HighDWordOfInt64(TotalSize);
                Stream.write(Data, 4);
                Stream.Seek(8, soBeginning);
            end;
        end else begin
            RF64Size := RIFFChunkSize;
            Stream.Seek(- 4, soCurrent);
            TotalSize := Int64(RIFFChunkSize) + Int64(WriteTagTotalSize) + Int64(PaddingToWrite) + 8;
            if Odd(TotalSize) then begin
                Inc(TotalSize);
            end;
            if TotalSize > $FFFFFFFF then begin
                Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
                Exit;
            end;
            RIFFChunkSizeNew := TotalSize;
            Stream.Write(RIFFChunkSizeNew, 4);
        end;
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            while Stream.Position < RF64Size + 8 do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (ChunkID[0] = Ord('d'))
                AND (ChunkID[1] = Ord('a'))
                AND (ChunkID[2] = Ord('t'))
                AND (ChunkID[3] = Ord('a'))
                AND (ChunkSize = $FFFFFFFF)
                then begin
                    Stream.Seek(MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh), soCurrent);
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
            if Stream.Position < Stream.Size then begin
                PreviousPosition := Stream.Position;
                if FileName <> '' then begin
                    try
                        TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                    except
                        Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                        Exit;
                    end;
                end else begin
                    TempStream := TMemoryStream.Create;
                end;
                TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                Stream.Seek(PreviousPosition, soBeginning);
            end;
            Stream.Write(RIFFID3v2ID[0], 4);
            ChunkSize := WriteTagTotalSize + PaddingToWrite;
            if Odd(ChunkSize) then begin
                Inc(ChunkSize);
            end;
            Stream.Write(ChunkSize, 4);
            PreviousPosition := Stream.Position;
            WritePadding(Stream, ChunkSize);
            if Assigned(TempStream) then begin
                TempStream.Seek(0, soBeginning);
                Stream.CopyFrom(TempStream, TempStream.Size);
                {$IFDEF NEXTGEN}
                TempStream.DisposeOf;
                TempStream := nil;
                {$ELSE}
                FreeAndNil(TempStream);
                {$ENDIF}
                SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
            end;
            Stream.Seek(PreviousPosition, soBeginning);
            Result := ID3V2LIBRARY_SUCCESS;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RF64UpdateID3v2(FileName: String; Stream: TStream; WriteTagTotalSize: Cardinal; PreviousTagSize: Cardinal; PaddingToWrite: Cardinal): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TotalSize: Int64;
    Waveds64: TWaveds64;
    RF64Size: Int64;
    Data: DWord;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    RF64Size := 0;
    try
        Stream.Seek(4, soCurrent);
        Stream.Read2(RIFFChunkSize, 4);
        if RIFFChunkSize = $FFFFFFFF then begin
            Stream.Read2(ChunkID, 4);
            if (ChunkID[0] <> Ord('W'))
            OR (ChunkID[1] <> Ord('A'))
            OR (ChunkID[2] <> Ord('V'))
            OR (ChunkID[3] <> Ord('E'))
            then begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            Stream.Read2(ChunkID, 4);
            if (ChunkID[0] = Ord('d'))
            AND (ChunkID[1] = Ord('s'))
            AND (ChunkID[2] = Ord('6'))
            AND (ChunkID[3] = Ord('4'))
            then begin
                Stream.Read2(Waveds64, SizeOf(TWaveds64));
                RF64Size := MakeInt64(Waveds64.RIFFSizeLow, Waveds64.RIFFSizeHigh);
                TotalSize := RF64Size - PreviousTagSize + WriteTagTotalSize + PaddingToWrite + 8;
                if Odd(TotalSize) then begin
                    Inc(TotalSize);
                end;
                //* Set new RF64 size
                Stream.Position := 20;
                Data := LowDWordOfInt64(TotalSize);
                Stream.write(Data, 4);
                Data := HighDWordOfInt64(TotalSize);
                Stream.write(Data, 4);
                Stream.Seek(8, soBeginning);
            end;
        end else begin
            //RF64Size := RIFFChunkSize;
            Stream.Seek(- 4, soCurrent);
            TotalSize := RIFFChunkSize - PreviousTagSize + WriteTagTotalSize + PaddingToWrite + 8;
            if Odd(TotalSize) then begin
                Inc(TotalSize);
            end;
            if TotalSize > $FFFFFFFF then begin
                Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
                Exit;
            end;
            RIFFChunkSizeNew := TotalSize;
            Stream.Write(RIFFChunkSizeNew, 4);
        end;
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            ChunkSize := 0;
            while Stream.Position < RF64Size + 8 do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (
                (ChunkID[0] = RIFFID3v2ID[0])
                AND (ChunkID[1] = RIFFID3v2ID[1])
                AND (ChunkID[2] = RIFFID3v2ID[2])
                AND (ChunkID[3] = RIFFID3v2ID[3])
                )
                OR
                (
                (ChunkID[0] = RIFFID3v2ID2[0])
                AND (ChunkID[1] = RIFFID3v2ID2[1])
                AND (ChunkID[2] = RIFFID3v2ID2[2])
                AND (ChunkID[3] = RIFFID3v2ID2[3])
                )
                then begin
                    Stream.Seek(- 4, soCurrent);
                    PreviousPosition := Stream.Position;
                    Stream.Seek(Int64(ChunkSize) + 4, soCurrent);
                    if Stream.Position < Stream.Size then begin
                        if FileName <> '' then begin
                            try
                                TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                            except
                                Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                                Exit;
                            end;
                        end else begin
                            TempStream := TMemoryStream.Create;
                        end;
                        TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                    end;
                    Stream.Seek(PreviousPosition, soBeginning);
                    ChunkSize := ChunkSize - PreviousTagSize + WriteTagTotalSize + PaddingToWrite;
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    Stream.Write(ChunkSize, 4);
                    WritePadding(Stream, ChunkSize);
                    if Assigned(TempStream) then begin
                        TempStream.Seek(0, soBeginning);
                        Stream.CopyFrom(TempStream, TempStream.Size);
                        {$IFDEF NEXTGEN}
                        TempStream.DisposeOf;
                        TempStream := nil;
                        {$ELSE}
                        FreeAndNil(TempStream);
                        {$ENDIF}
                        SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
                    end;
                    Stream.Size := Stream.Position;
                    Stream.Seek(PreviousPosition + 4, soBeginning);
                    Result := ID3V2LIBRARY_SUCCESS;
                    Exit;
                end else begin
                    if (ChunkID[0] = Ord('d'))
                    AND (ChunkID[1] = Ord('a'))
                    AND (ChunkID[2] = Ord('t'))
                    AND (ChunkID[3] = Ord('a'))
                    AND (ChunkSize = $FFFFFFFF)
                    then begin
                        Stream.Seek(MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh), soCurrent);
                    end else begin
                        Stream.Seek(ChunkSize, soCurrent);
                    end;
                end;
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveRIFFID3v2FromFile(FileName: String): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TFileStream;
    TagStream: TFileStream;
    TagSize: DWord;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        TagStream := TFileStream.Create(FileName, fmOpenReadWrite);
    except
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        try
            if CheckRIFF(TagStream) then begin
                TagSize := SeekRIFF(TagStream);
                if TagSize = 0 then begin
                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                    Exit;
                end;
            end else begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            TagStream.Seek(4, soBeginning);
            TagStream.Read2(RIFFChunkSize, 4);
            TagStream.Seek(- 4, soCurrent);
            RIFFChunkSizeNew := RIFFChunkSize - TagSize - 8;
            TagStream.Write(RIFFChunkSizeNew, 4);
            TagStream.Read2(ChunkID, 4);
            if (ChunkID[0] = RIFFWAVEID[0])
            AND (ChunkID[1] = RIFFWAVEID[1])
            AND (ChunkID[2] = RIFFWAVEID[2])
            AND (ChunkID[3] = RIFFWAVEID[3])
            then begin
                ChunkSize := 0;
                while TagStream.Position + 8 < TagStream.Size do begin
                    TagStream.Read2(ChunkID, 4);
                    TagStream.Read2(ChunkSize, 4);
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    if (
                    (ChunkID[0] = RIFFID3v2ID[0])
                    AND (ChunkID[1] = RIFFID3v2ID[1])
                    AND (ChunkID[2] = RIFFID3v2ID[2])
                    AND (ChunkID[3] = RIFFID3v2ID[3])
                    )
                    OR
                    (
                    (ChunkID[0] = RIFFID3v2ID2[0])
                    AND (ChunkID[1] = RIFFID3v2ID2[1])
                    AND (ChunkID[2] = RIFFID3v2ID2[2])
                    AND (ChunkID[3] = RIFFID3v2ID2[3])
                    )
                    then begin
                        TagStream.Seek(- 8, soCurrent);
                        PreviousPosition := TagStream.Position;
                        TagStream.Seek(Int64(ChunkSize) + 8, soCurrent);
                        if TagStream.Position + 8 + ChunkSize < TagStream.Size then begin
                            try
                                TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                            except
                                Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                                Exit;
                            end;
                            TempStream.CopyFrom(TagStream, TagStream.Size - TagStream.Position);
                        end;
                        TagStream.Seek(PreviousPosition, soBeginning);
                        THandleStream(TagStream).Size := TagStream.Position;
                        if Assigned(TempStream) then begin
                            TempStream.Seek(0, soBeginning);
                            TagStream.CopyFrom(TempStream, TempStream.Size);
                            {$IFDEF NEXTGEN}
                            TempStream.DisposeOf;
                            TempStream := nil;
                            {$ELSE}
                            FreeAndNil(TempStream);
                            {$ENDIF}
                            SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
                        end;
                        Result := ID3V2LIBRARY_SUCCESS;
                        Exit;
                    end else begin
                        TagStream.Seek(ChunkSize, soCurrent);
                    end;
                end;
            end;
        finally
            if Assigned(TagStream) then begin
                FreeAndNil(TagStream);
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveRIFFID3v2FromStream(Stream: TStream): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TagSize: DWord;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    if Stream.Size = 0 then begin
        Exit;
    end;
    try
        Stream.Seek(0, soBeginning);
        if CheckRIFF(Stream) then begin
            TagSize := SeekRIFF(Stream);
            if TagSize = 0 then begin
                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                Exit;
            end;
        end else begin
            Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
            Exit;
        end;
        Stream.Seek(4, soBeginning);
        Stream.Read2(RIFFChunkSize, 4);
        Stream.Seek(- 4, soCurrent);
        RIFFChunkSizeNew := RIFFChunkSize - TagSize - 8;
        Stream.Write(RIFFChunkSizeNew, 4);
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (
                (ChunkID[0] = RIFFID3v2ID[0])
                AND (ChunkID[1] = RIFFID3v2ID[1])
                AND (ChunkID[2] = RIFFID3v2ID[2])
                AND (ChunkID[3] = RIFFID3v2ID[3])
                )
                OR
                (
                (ChunkID[0] = RIFFID3v2ID2[0])
                AND (ChunkID[1] = RIFFID3v2ID2[1])
                AND (ChunkID[2] = RIFFID3v2ID2[2])
                AND (ChunkID[3] = RIFFID3v2ID2[3])
                )
                then begin
                    Stream.Seek(- 8, soCurrent);
                    PreviousPosition := Stream.Position;
                    Stream.Seek(Int64(ChunkSize) + 8, soCurrent);
                    if Stream.Position + 8 + ChunkSize < Stream.Size then begin
                        TempStream := TMemoryStream.Create;
                        try
                            TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                        except
                            FreeAndNil(TempStream);
                            Exit;
                        end;
                    end;
                    Stream.Seek(PreviousPosition, soBeginning);
                    Stream.Size := Stream.Position;
                    if Assigned(TempStream) then begin
                        TempStream.Seek(0, soBeginning);
                        Stream.CopyFrom(TempStream, TempStream.Size);
                        FreeAndNil(TempStream);
                    end;
                    Result := ID3V2LIBRARY_SUCCESS;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveAIFFID3v2FromFile(FileName: String): Integer;
var
    AIFFChunkSize: DWord;
    AIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TFileStream;
    TagStream: TFileStream;
    TagSize: DWord;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        TagStream := TFileStream.Create(FileName, fmOpenReadWrite);
    except
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        try
            if CheckAIFF(TagStream) then begin
                TagSize := SeekAIFF(TagStream);
                if TagSize = 0 then begin
                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                    Exit;
                end;
            end else begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            TagStream.Seek(4, soBeginning);
            TagStream.Read2(AIFFChunkSize, 4);
            AIFFChunkSize := ReverseBytes32(AIFFChunkSize);
            TagStream.Seek(- 4, soCurrent);
            AIFFChunkSizeNew := AIFFChunkSize - TagSize - 8;
            AIFFChunkSizeNew := ReverseBytes32(AIFFChunkSizeNew);
            TagStream.Write(AIFFChunkSizeNew, 4);
            TagStream.Read2(ChunkID, 4);
            if ((ChunkID[0] = AIFFChunkID[0])
            AND (ChunkID[1] = AIFFChunkID[1])
            AND (ChunkID[2] = AIFFChunkID[2])
            AND (ChunkID[3] = AIFFChunkID[3]))
            OR ((ChunkID[0] = AIFCChunkID[0])
            AND (ChunkID[1] = AIFCChunkID[1])
            AND (ChunkID[2] = AIFCChunkID[2])
            AND (ChunkID[3] = AIFCChunkID[3]))
            then begin
                ChunkSize := 0;
                while TagStream.Position + 8 < TagStream.Size do begin
                    TagStream.Read2(ChunkID, 4);
                    TagStream.Read2(ChunkSize, 4);
                    ChunkSize := ReverseBytes32(ChunkSize);
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    if (ChunkID[0] = AIFFID3v2ID[0])
                    AND (ChunkID[1] = AIFFID3v2ID[1])
                    AND (ChunkID[2] = AIFFID3v2ID[2])
                    AND (ChunkID[3] = AIFFID3v2ID[3])
                    then begin
                        TagStream.Seek(- 8, soCurrent);
                        PreviousPosition := TagStream.Position;
                        TagStream.Seek(Int64(ChunkSize) + 8, soCurrent);
                        if TagStream.Position + 8 + ChunkSize < TagStream.Size then begin
                            try
                                TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                            except
                                Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                                Exit;
                            end;
                            TempStream.CopyFrom(TagStream, TagStream.Size - TagStream.Position);
                        end;
                        TagStream.Seek(PreviousPosition, soBeginning);
                        THandleStream(TagStream).Size := TagStream.Position;
                        if Assigned(TempStream) then begin
                            TempStream.Seek(0, soBeginning);
                            TagStream.CopyFrom(TempStream, TempStream.Size);
                            {$IFDEF NEXTGEN}
                            TempStream.DisposeOf;
                            TempStream := nil;
                            {$ELSE}
                            FreeAndNil(TempStream);
                            {$ENDIF}
                            SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
                        end;
                        Result := ID3V2LIBRARY_SUCCESS;
                        Exit;
                    end else begin
                        TagStream.Seek(ChunkSize, soCurrent);
                    end;
                end;
            end;
        finally
            if Assigned(TagStream) then begin
                FreeAndNil(TagStream);
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveAIFFID3v2FromStream(Stream: TStream): Integer;
var
    AIFFChunkSize: DWord;
    AIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TagSize: DWord;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    if Stream.Size = 0 then begin
        Exit;
    end;
    try
        Stream.Seek(0, soBeginning);
        if CheckAIFF(Stream) then begin
            TagSize := SeekAIFF(Stream);
            if TagSize = 0 then begin
                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                Exit;
            end;
        end else begin
            Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
            Exit;
        end;
        Stream.Seek(4, soBeginning);
        Stream.Read2(AIFFChunkSize, 4);
        AIFFChunkSize := ReverseBytes32(AIFFChunkSize);
        Stream.Seek(- 4, soCurrent);
        AIFFChunkSizeNew := AIFFChunkSize - TagSize - 8;
        AIFFChunkSizeNew := ReverseBytes32(AIFFChunkSizeNew);
        Stream.Write(AIFFChunkSizeNew, 4);
        Stream.Read2(ChunkID, 4);
        if ((ChunkID[0] = AIFFChunkID[0])
        AND (ChunkID[1] = AIFFChunkID[1])
        AND (ChunkID[2] = AIFFChunkID[2])
        AND (ChunkID[3] = AIFFChunkID[3]))
        OR ((ChunkID[0] = AIFCChunkID[0])
        AND (ChunkID[1] = AIFCChunkID[1])
        AND (ChunkID[2] = AIFCChunkID[2])
        AND (ChunkID[3] = AIFCChunkID[3]))
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                ChunkSize := ReverseBytes32(ChunkSize);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (ChunkID[0] = AIFFID3v2ID[0])
                AND (ChunkID[1] = AIFFID3v2ID[1])
                AND (ChunkID[2] = AIFFID3v2ID[2])
                AND (ChunkID[3] = AIFFID3v2ID[3])
                then begin
                    Stream.Seek(- 8, soCurrent);
                    PreviousPosition := Stream.Position;
                    Stream.Seek(Int64(ChunkSize) + 8, soCurrent);
                    if Stream.Position + 8 + ChunkSize < Stream.Size then begin
                        TempStream := TMemoryStream.Create;
                        try
                            TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                        except
                            FreeAndNil(TempStream);
                            Exit;
                        end;
                    end;
                    Stream.Seek(PreviousPosition, soBeginning);
                    Stream.Size := Stream.Position;
                    if Assigned(TempStream) then begin
                        TempStream.Seek(0, soBeginning);
                        Stream.CopyFrom(TempStream, TempStream.Size);
                        FreeAndNil(TempStream);
                    end;
                    Result := ID3V2LIBRARY_SUCCESS;
                    Exit;
                end else begin
                    Stream.Seek(ChunkSize, soCurrent);
                end;
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveRF64ID3v2FromFile(FileName: String): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TFileStream;
    TagStream: TFileStream;
    TagSize: DWord;
    Waveds64: TWaveds64;
    RF64Size: Int64;
    Data: DWord;
    TotalSize: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    try
        TagStream := TFileStream.Create(FileName, fmOpenReadWrite);
    except
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        try
            if CheckRF64(TagStream) then begin
                TagSize := SeekRF64(TagStream);
                if TagSize = 0 then begin
                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                    Exit;
                end;
            end else begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            TagStream.Seek(4, soBeginning);
            TagStream.Read2(RIFFChunkSize, 4);
            if RIFFChunkSize = $FFFFFFFF then begin
                TagStream.Read2(ChunkID, 4);
                if (ChunkID[0] <> Ord('W'))
                OR (ChunkID[1] <> Ord('A'))
                OR (ChunkID[2] <> Ord('V'))
                OR (ChunkID[3] <> Ord('E'))
                then begin
                    Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                    Exit;
                end;
                TagStream.Read2(ChunkID, 4);
                if (ChunkID[0] = Ord('d'))
                AND (ChunkID[1] = Ord('s'))
                AND (ChunkID[2] = Ord('6'))
                AND (ChunkID[3] = Ord('4'))
                then begin
                    TagStream.Read2(Waveds64, SizeOf(TWaveds64));
                    RF64Size := MakeInt64(Waveds64.RIFFSizeLow, Waveds64.RIFFSizeHigh);
                    TotalSize := RF64Size - TagSize - 8;
                    if Odd(TotalSize) then begin
                        Inc(TotalSize);
                    end;
                    //* Set new RF64 size
                    TagStream.Position := 20;
                    Data := LowDWordOfInt64(TotalSize);
                    TagStream.write(Data, 4);
                    Data := HighDWordOfInt64(TotalSize);
                    TagStream.write(Data, 4);
                    TagStream.Seek(8, soBeginning);
                end;
            end else begin
                RF64Size := RIFFChunkSize;
                TagStream.Seek(- 4, soCurrent);
                TotalSize := RF64Size - TagSize - 8;
                //* Should not happen
                {
                if Odd(TotalSize) then begin
                    Inc(TotalSize);
                end;
                }
                if TotalSize > $FFFFFFFF then begin
                    Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
                    Exit;
                end;
                RIFFChunkSizeNew := TotalSize;
                TagStream.Write(RIFFChunkSizeNew, 4);
            end;
            TagStream.Read2(ChunkID, 4);
            if (ChunkID[0] = RIFFWAVEID[0])
            AND (ChunkID[1] = RIFFWAVEID[1])
            AND (ChunkID[2] = RIFFWAVEID[2])
            AND (ChunkID[3] = RIFFWAVEID[3])
            then begin
                ChunkSize := 0;
                while TagStream.Position + 8 < TagStream.Size do begin
                    TagStream.Read2(ChunkID, 4);
                    TagStream.Read2(ChunkSize, 4);
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    if (
                    (ChunkID[0] = RIFFID3v2ID[0])
                    AND (ChunkID[1] = RIFFID3v2ID[1])
                    AND (ChunkID[2] = RIFFID3v2ID[2])
                    AND (ChunkID[3] = RIFFID3v2ID[3])
                    )
                    OR
                    (
                    (ChunkID[0] = RIFFID3v2ID2[0])
                    AND (ChunkID[1] = RIFFID3v2ID2[1])
                    AND (ChunkID[2] = RIFFID3v2ID2[2])
                    AND (ChunkID[3] = RIFFID3v2ID2[3])
                    )
                    then begin
                        TagStream.Seek(- 8, soCurrent);
                        PreviousPosition := TagStream.Position;
                        TagStream.Seek(Int64(ChunkSize) + 8, soCurrent);
                        if TagStream.Position + 8 + ChunkSize < TagStream.Size then begin
                            try
                                TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                            except
                                Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                                Exit;
                            end;
                            TempStream.CopyFrom(TagStream, TagStream.Size - TagStream.Position);
                        end;
                        TagStream.Seek(PreviousPosition, soBeginning);
                        THandleStream(TagStream).Size := TagStream.Position;
                        if Assigned(TempStream) then begin
                            TempStream.Seek(0, soBeginning);
                            TagStream.CopyFrom(TempStream, TempStream.Size);
                            {$IFDEF NEXTGEN}
                            TempStream.DisposeOf;
                            TempStream := nil;
                            {$ELSE}
                            FreeAndNil(TempStream);
                            {$ENDIF}
                            SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
                        end;
                        Result := ID3V2LIBRARY_SUCCESS;
                        Exit;
                    end else begin
                        if (ChunkID[0] = Ord('d'))
                        AND (ChunkID[1] = Ord('a'))
                        AND (ChunkID[2] = Ord('t'))
                        AND (ChunkID[3] = Ord('a'))
                        AND (ChunkSize = $FFFFFFFF)
                        then begin
                            TagStream.Seek(MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh), soCurrent);
                        end else begin
                            TagStream.Seek(ChunkSize, soCurrent);
                        end;
                    end;
                end;
            end;
        finally
            if Assigned(TagStream) then begin
                FreeAndNil(TagStream);
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveRF64ID3v2FromStream(Stream: TStream): Integer;
var
    RIFFChunkSize: DWord;
    RIFFChunkSizeNew: DWord;
    ChunkID: TFrameID;
    ChunkSize: DWord;
    PreviousPosition: Int64;
    TempStream: TStream;
    TagSize: DWord;
    Waveds64: TWaveds64;
    RF64Size: Int64;
    Data: DWord;
    TotalSize: Int64;
begin
    Result := ID3V2LIBRARY_ERROR;
    TempStream := nil;
    if Stream.Size = 0 then begin
        Exit;
    end;

    try
        Stream.Seek(0, soBeginning);
        if CheckRF64(Stream) then begin
            TagSize := SeekRF64(Stream);
            if TagSize = 0 then begin
                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                Exit;
            end;
        end else begin
            Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
            Exit;
        end;
        Stream.Seek(4, soBeginning);
        Stream.Read2(RIFFChunkSize, 4);
        if RIFFChunkSize = $FFFFFFFF then begin
            Stream.Read2(ChunkID, 4);
            if (ChunkID[0] <> Ord('W'))
            OR (ChunkID[1] <> Ord('A'))
            OR (ChunkID[2] <> Ord('V'))
            OR (ChunkID[3] <> Ord('E'))
            then begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            Stream.Read2(ChunkID, 4);
            if (ChunkID[0] = Ord('d'))
            AND (ChunkID[1] = Ord('s'))
            AND (ChunkID[2] = Ord('6'))
            AND (ChunkID[3] = Ord('4'))
            then begin
                Stream.Read2(Waveds64, SizeOf(TWaveds64));
                RF64Size := MakeInt64(Waveds64.RIFFSizeLow, Waveds64.RIFFSizeHigh);
                TotalSize := RF64Size - TagSize - 8;
                if Odd(TotalSize) then begin
                    Inc(TotalSize);
                end;
                //* Set new RF64 size
                Stream.Position := 20;
                Data := LowDWordOfInt64(TotalSize);
                Stream.write(Data, 4);
                Data := HighDWordOfInt64(TotalSize);
                Stream.write(Data, 4);
                Stream.Seek(8, soBeginning);
            end;
        end else begin
            RF64Size := RIFFChunkSize;
            Stream.Seek(- 4, soCurrent);
            TotalSize := RF64Size - TagSize - 8;
            //* Should not happen
            {
            if Odd(TotalSize) then begin
                Inc(TotalSize);
            end;
            }
            if TotalSize > $FFFFFFFF then begin
                Result := ID3V2LIBRARY_ERROR_DOESNT_FIT;
                Exit;
            end;
            RIFFChunkSizeNew := TotalSize;
            Stream.Write(RIFFChunkSizeNew, 4);
        end;
        Stream.Read2(ChunkID, 4);
        if (ChunkID[0] = RIFFWAVEID[0])
        AND (ChunkID[1] = RIFFWAVEID[1])
        AND (ChunkID[2] = RIFFWAVEID[2])
        AND (ChunkID[3] = RIFFWAVEID[3])
        then begin
            ChunkSize := 0;
            while Stream.Position + 8 < Stream.Size do begin
                Stream.Read2(ChunkID, 4);
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                if (
                (ChunkID[0] = RIFFID3v2ID[0])
                AND (ChunkID[1] = RIFFID3v2ID[1])
                AND (ChunkID[2] = RIFFID3v2ID[2])
                AND (ChunkID[3] = RIFFID3v2ID[3])
                )
                OR
                (
                (ChunkID[0] = RIFFID3v2ID2[0])
                AND (ChunkID[1] = RIFFID3v2ID2[1])
                AND (ChunkID[2] = RIFFID3v2ID2[2])
                AND (ChunkID[3] = RIFFID3v2ID2[3])
                )
                then begin
                    Stream.Seek(- 8, soCurrent);
                    PreviousPosition := Stream.Position;
                    Stream.Seek(Int64(ChunkSize) + 8, soCurrent);
                    if Stream.Position + 8 + ChunkSize < Stream.Size then begin
                        TempStream := TMemoryStream.Create;
                        try
                            TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
                        except
                            FreeAndNil(TempStream);
                            Exit;
                        end;
                    end;
                    Stream.Seek(PreviousPosition, soBeginning);
                    THandleStream(Stream).Size := Stream.Position;
                    if Assigned(TempStream) then begin
                        TempStream.Seek(0, soBeginning);
                        Stream.CopyFrom(TempStream, TempStream.Size);
                        FreeAndNil(TempStream);
                    end;
                    Result := ID3V2LIBRARY_SUCCESS;
                    Exit;
                end else begin
                    if (ChunkID[0] = Ord('d'))
                    AND (ChunkID[1] = Ord('a'))
                    AND (ChunkID[2] = Ord('t'))
                    AND (ChunkID[3] = Ord('a'))
                    AND (ChunkSize = $FFFFFFFF)
                    then begin
                        Stream.Seek(MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh), soCurrent);
                    end else begin
                        Stream.Seek(ChunkSize, soCurrent);
                    end;
                end;
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveDSFID3v2FromFile(FileName: String): Integer;
var
    TempStream: TFileStream;
    TagStream: TFileStream;
    TagSize: DWord;
    ID3v2Pointer: UInt64;
    DSFFileSize: UInt64;
    Zero64: UInt64;
begin
    Zero64 := 0;
    TempStream := nil;
    try
        TagStream := TFileStream.Create(FileName, fmOpenReadWrite);
    except
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        try
            if CheckDSF(TagStream) then begin
                TagSize := SeekDSF(TagStream);
                if TagSize = 0 then begin
                    Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                    Exit;
                end;
            end else begin
                Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
                Exit;
            end;
            //* Update DSF file size
            TagStream.Seek(12, soBeginning);
            TagStream.Read2(DSFFileSize, 8);
            Dec(DSFFileSize, TagSize);
            TagStream.Seek(- 8, soCurrent);
            TagStream.Write(DSFFileSize, 8);
            //* Update ID3v2 pointer
            TagStream.Read2(ID3v2Pointer, 8);
            TagStream.Seek(- 8, soCurrent);
            TagStream.Write(Zero64, 8);
            //* Check if there is tail data
            TagStream.Seek(ID3v2Pointer, soBeginning);
            if TagStream.Position + TagSize < TagStream.Size then begin
                try
                    TempStream := TFileStream.Create(ChangeFileExt(FileName, '.tmp'), fmCreate);
                except
                    Result := ID3V2LIBRARY_ERROR_WRITING_FILE;
                    Exit;
                end;
                TagStream.Seek(ID3v2Pointer + TagSize, soBeginning);
                TempStream.CopyFrom(TagStream, TagStream.Size - TagStream.Position);
            end;
            //* Truncate file at ID3v2 pointer
            TagStream.Seek(ID3v2Pointer, soBeginning);
            TagStream.Size := TagStream.Position;
            //* Copy remaining data if have it
            if Assigned(TempStream) then begin
                TempStream.Seek(0, soBeginning);
                TagStream.CopyFrom(TempStream, TempStream.Size);
                {$IFDEF NEXTGEN}
                TempStream.DisposeOf;
                TempStream := nil;
                {$ELSE}
                FreeAndNil(TempStream);
                {$ENDIF}
                SysUtils.DeleteFile(ChangeFileExt(FileName, '.tmp'));
            end;
            Result := ID3V2LIBRARY_SUCCESS;
        finally
            if Assigned(TagStream) then begin
                FreeAndNil(TagStream);
            end;
        end;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveDSFID3v2FromStream(Stream: TStream): Integer;
var
    TempStream: TStream;
    TagSize: DWord;
    ID3v2Pointer: UInt64;
    DSFFileSize: UInt64;
    Zero64: UInt64;
begin
    Result := ID3V2LIBRARY_ERROR;
    Zero64 := 0;
    TempStream := nil;
    if Stream.Size = 0 then begin
        Exit;
    end;
    try
        Stream.Seek(0, soBeginning);
        if CheckDSF(Stream) then begin
            TagSize := SeekDSF(Stream);
            if TagSize = 0 then begin
                Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
                Exit;
            end;
        end else begin
            Result := ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT;
            Exit;
        end;
        //* Update DSF file size
        Stream.Seek(12, soBeginning);
        Stream.Read2(DSFFileSize, 8);
        Dec(DSFFileSize, TagSize);
        Stream.Seek(- 8, soCurrent);
        Stream.Write(DSFFileSize, 8);
        //* Update ID3v2 pointer
        Stream.Read2(ID3v2Pointer, 8);
        Stream.Seek(- 8, soCurrent);
        Stream.Write(Zero64, 8);
        //* Check if there is tail data
        Stream.Seek(ID3v2Pointer, soBeginning);
        if Stream.Position + TagSize < Stream.Size then begin
            TempStream := TMemoryStream.Create;
            try
                Stream.Seek(ID3v2Pointer + TagSize, soBeginning);
                TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
            except
                FreeAndNil(TempStream);
                Exit;
            end;
        end;
        //* Truncate file at ID3v2 pointer
        Stream.Seek(ID3v2Pointer, soBeginning);
        Stream.Size := Stream.Position;
        //* Copy remaining data if have it
        if Assigned(TempStream) then begin
            TempStream.Seek(0, soBeginning);
            Stream.CopyFrom(TempStream, TempStream.Size);
            FreeAndNil(TempStream);
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function RemoveDFFID3v2FromFile(FileName: String): Integer;
var
    Stream: TStream;
begin
    try
        Stream := TFileStream.Create(FileName, fmOpenReadWrite);
    except
        Result := ID3V2LIBRARY_ERROR_OPENING_FILE;
        Exit;
    end;
    try
        Result := RemoveDFFID3v2FromStream(Stream);
    finally
        FreeAndNil(Stream);
    end;
end;

function RemoveDFFID3v2FromStream(Stream: TStream): Integer;
var
    TempStream: TMemoryStream;
    ExistingTagSize: DWord;
    ExistingTagSizePadded: DWord;
    DFFFRM8Size: UInt64;
    ID3v2ChunkDataPosition: UInt64;
begin
    TempStream := nil;
    try
        ExistingTagSize := SeekDFF(Stream);
        if ExistingTagSize = 0 then begin
            Result := ID3V2LIBRARY_ERROR_NO_TAG_FOUND;
            Exit;
        end;
        ExistingTagSizePadded := ExistingTagSize;
        if Odd(ExistingTagSizePadded) then begin
            Inc(ExistingTagSizePadded);
        end;
        ID3v2ChunkDataPosition := Stream.Position;
        //* Check if there is tail data
        if ID3v2ChunkDataPosition + ExistingTagSizePadded < Stream.Size then begin
            TempStream := TMemoryStream.Create;
            Stream.Seek(ID3v2ChunkDataPosition + ExistingTagSizePadded, soBeginning);
            TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
        end;
        //* Update DFF FRM8 chunk size
        Stream.Seek(4, soBeginning);
        Stream.Read2(DFFFRM8Size, 8);
        DFFFRM8Size := ReverseBytes64(DFFFRM8Size);
        Dec(DFFFRM8Size, UInt64(ExistingTagSizePadded) + 12);
        Stream.Seek(- 8, soCurrent);
        DFFFRM8Size := ReverseBytes64(DFFFRM8Size);
        Stream.Write(DFFFRM8Size, 8);
        Stream.Seek(ID3v2ChunkDataPosition - 12, soBeginning);
        //* Set end of file
        Stream.Size := Stream.Position;
        //* Copy remaining data if have it
        if Assigned(TempStream) then begin
            TempStream.Seek(0, soBeginning);
            Stream.CopyFrom(TempStream, TempStream.Size);
            FreeAndNil(TempStream);
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function TID3v2Frames.FindChapterElementID(ChapterElementID: String): Integer;
var
    i: Integer;
begin
    Result := - 1;
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, 'CHAP') then begin
            if Frames[i] is TID3v2FrameCHAP then begin
                if (Frames[i] as TID3v2FrameCHAP).ElementID = ChapterElementID then begin
                    Result := i;
                    Break;
                end;
            end;
        end;
    end;
end;

function TID3v2Frames.FindCustomFrame(FrameID: String; Description: String): Integer;
var
    ID: TFrameID;
    FrameDescription: String;
    i: Integer;
begin
    Result := -1;
    AnsiStringToPAnsiChar(FrameID, ID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(ID, Frames[i].ID) then begin
            GetUserDefinedTextInformation(i, FrameDescription);
            if FrameDescription = Description then begin
                Result := i;
                Break;
            end;
        end;
    end;
end;

function TID3v2Frames.GetUserDefinedTextInformation(FrameIndex: Integer; var Description: String): String;
var
    Encoding: Byte;
    BigEndian: Boolean;
    NilFound: Boolean;
begin
    Result := '';
    Description := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        BigEndian := False;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get description
        Description := Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian);
        //* Get the content
        while True do begin
            Result := Result + Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian, @NilFound);
            if (NOT NilFound)
            OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size)
            then begin
                Break;
            end;
            Result := Result + #13#10;
        end;
        if Root.TrimText then begin
            Result := Trim(Result);
        end;
    except
        //*
    end;
end;

function TID3v2Frames.GetUserDefinedTextInformationMultiple(FrameIndex: Integer; var Description: String; List: TStrings): Boolean;
var
    Text: String;
begin
    List.Clear;
    Text := GetUserDefinedTextInformation(FrameIndex, Description);
    List.Text := Text;
    Result := List.Text <> '';
end;

function TID3v2Frames.GetUserDefinedTextInformation(const Description: String; CaseSensitive: Boolean = False): String;
var
    ID: TFrameID;
    FrameDescription: String;
    i: Integer;
    Value: String;
begin
    Result := '';
    AnsiStringToPAnsiChar('TXXX', ID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(ID, Frames[i].ID) then begin
            Value := GetUserDefinedTextInformation(i, FrameDescription);
            if CaseSensitive then begin
                if FrameDescription = Description then begin
                    Result := Value;
                    Break;
                end;
            end else begin
                if SameText(FrameDescription, Description) then begin
                    Result := Value;
                    Break;
                end;
            end;
        end;
    end;
end;

function TID3v2Frames.GetUserDefinedTextInformationMultiple(FrameID: String; var Description: String; List: TStrings): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    List.Clear;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetUserDefinedTextInformationMultiple(Index, Description, List);
end;

function TID3v2Frames.SetUserDefinedTextInformation(FrameID: String; Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetUserDefinedTextInformation(Index, Description, Text, TextEncoding);
end;

function TID3v2Frames.SetUserDefinedTextInformation(FrameIndex: Integer; Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    case TextEncoding of
        teUnicode: begin
            try
                //* Set unicode flag
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* BOM
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Frames[FrameIndex].Stream.Write(PWideChar(Description)^, (Length(Description) + 1) * 2);
                //* BOM
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the user defined text
                Frames[FrameIndex].Stream.Write(PWideChar(Text)^, (Length(Text) + 1) * 2);
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            try
                //* Set ANSI flag
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Bytes := TEncoding.UTF8.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the user defined text
                Bytes := TEncoding.UTF8.GetBytes(Text);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            try
                //* Set ANSI flag
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Bytes := TEncoding.ANSI.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $00;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the user defined text
                Bytes := TEncoding.ANSI.GetBytes(Text);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;
end;

function TID3v2Frames.SetUserDefinedTextInformationMultiple(FrameIndex: Integer; Description: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
    Text: String;
    i: Integer;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    Frames[FrameIndex].Stream.Clear;
    case TextEncoding of
        teUnicode: begin
            try
                //* Set unicode flag
                Data := $01;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* BOM
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Frames[FrameIndex].Stream.Write(PWideChar(Description)^, (Length(Description) + 1) * 2);
                //* BOM
                Data := $FF;
                Frames[FrameIndex].Stream.Write(Data, 1);
                Data := $FE;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the user defined text
                for i := 0 to List.Count - 1  do begin
                    Text := List[i];
                    Frames[FrameIndex].Stream.Write(PWideChar(Text)^, (Length(Text) + 1) * 2);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            try
                Frames[FrameIndex].Stream.Clear;
                //* Set UTF-8 flag
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Bytes := TEncoding.UTF8.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $0;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the user defined text
                for i := 0 to List.Count - 1  do begin
                    Text := List[i];
                    Bytes := TEncoding.UTF8.GetBytes(Text);
                    Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[FrameIndex].Stream.Write(Data, 1);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            try
                Frames[FrameIndex].Stream.Clear;
                //* Set UTF-8 flag
                Data := $03;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Set the description
                Bytes := TEncoding.UTF8.GetBytes(Description);
                Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                Data := $0;
                Frames[FrameIndex].Stream.Write(Data, 1);
                //* Write the user defined text
                for i := 0 to List.Count - 1  do begin
                    Text := List[i];
                    Bytes := TEncoding.UTF8.GetBytes(Text);
                    Frames[FrameIndex].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[FrameIndex].Stream.Write(Data, 1);
                end;
                Frames[FrameIndex].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;
end;

function TID3v2Frames.SetUserDefinedTextInformationMultiple(FrameID: String; Description: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetUserDefinedTextInformationMultiple(Index, Description, List, TextEncoding);
end;

function TID3v2Frames.GetPopularimeter(FrameIndex: Integer; var Email: String; var Rating: Byte; var PlayCounter: Cardinal): Boolean;
var
    DataByte: Byte;
begin
    Result := False;
    Email := '';
    Rating := 0;
    PlayCounter := 0;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get e-mail
        Email := Frames[FrameIndex].Stream.ReadText;
        //* Get rating
        Frames[FrameIndex].Stream.Read2(DataByte, 1);
        Rating := DataByte;
        //* Get playcount
        try
            while Frames[FrameIndex].Stream.Position < Frames[FrameIndex].Stream.Size do begin
                PlayCounter := PlayCounter SHL 8;
                Frames[FrameIndex].Stream.Read2(DataByte, 1);
                PlayCounter := PlayCounter + DataByte;
            end;
            Result := True;
        except
            PlayCounter := High(Cardinal);
        end;
    except
        //*
    end;
end;


function TID3v2Frames.FindPopularimeter(Email: String; var Rating: Byte; var PlayCounter: Cardinal): Integer;
var
    i: Integer;
    FrameEmail: String;
    FrameID: TFrameID;
begin
    Result := - 1;
    Rating := 0;
    PlayCounter := 0;
    ConvertString2FrameID('POPM', FrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, FrameID) then begin
            if GetPopularimeter(i, FrameEmail, Rating, PlayCounter) then begin
                if FrameEmail = Email then begin
                    Result := i;
                    Break;
                end else begin
                    Rating := 0;
                    PlayCounter := 0;
                end;
            end;
        end;
    end;
end;

function TID3v2Frames.SetPopularimeterByEmail(Email: String; Rating: Byte; PlayCounter: Cardinal = 0): Boolean;
var
    i: Integer;
    GetEmail: String;
    GetRating: Byte;
    GetPlayCounter: Cardinal;
    Index: Integer;
    FrameID: TFrameID;
begin
    Index := - 1;
    ConvertString2FrameID('POPM', FrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, FrameID) then begin
            if GetPopularimeter(i, GetEmail, GetRating, GetPlayCounter) then begin
                if GetEmail = Email then begin
                    Index := i;
                    Break;
                end;
            end;
        end;
    end;
    if Index = - 1 then begin
        Index := AddFrame('POPM');
    end;
    Result := SetPopularimeter(Index, Email, Rating, PlayCounter);
end;

function TID3v2Frames.SetPopularimeter(FrameIndex: Integer; Email: String; Rating: Byte; PlayCounter: Cardinal): Boolean;
var
    Data: Byte;
    Value: Cardinal;
    i: Integer;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Write e-mail
        for i := StrLow to StrLow + Length(Email) - 1 do begin
            Data := Ord(Email[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Data := 0;
        Frames[FrameIndex].Stream.Write(Data, 1);
        //* Write rating
        Frames[FrameIndex].Stream.Write(Rating, 1);
        //* Write playcount
        if PlayCounter > 0 then begin
            Value := PlayCounter SHR 24;
            Data := Value;
            Frames[FrameIndex].Stream.Write(Data, 1);
            Value := PlayCounter SHL 8;
            Value := Value SHR 24;
            Data := Value;
            Frames[FrameIndex].Stream.Write(Data, 1);
            Value := PlayCounter SHL 16;
            Value := Value SHR 24;
            Data := Value;
            Frames[FrameIndex].Stream.Write(Data, 1);
            Value := PlayCounter SHL 24;
            Value := Value SHR 24;
            Data := Value;
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.FindTXXXByDescription(Description: String; var Text: String): Integer;
var
    FrameID: TFrameID;
    i: Integer;
    GetDescription: String;
    GetLanguageID: TLanguageID;
    GetContent: String;
begin
    Result := - 1;
    Text := '';
    ConvertString2FrameID('TXXX', FrameID);
    ClearLanguageID(GetLanguageID);
    GetDescription := '';
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            GetContent := GetUserDefinedTextInformation(i, GetDescription);
            if WideUpperCase(GetDescription) = WideUpperCase(Description) then begin
                Text := GetContent;
                Result := i;
                Break;
            end;
        end;
    end;
end;

function TID3v2Frames.FindTXXXByDescriptionMultiple(Description: String; List: TStrings): Integer;
var
    FrameID: TFrameID;
    i: Integer;
    GetDescription: String;
    GetLanguageID: TLanguageID;
    GetContent: String;
begin
    Result := - 1;
    List.Clear;
    ConvertString2FrameID('TXXX', FrameID);
    ClearLanguageID(GetLanguageID);
    GetDescription := '';
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            GetContent := GetUserDefinedTextInformation(i, GetDescription);
            if WideUpperCase(GetDescription) = WideUpperCase(Description) then begin
                List.Text := GetContent;
                Result := i;
                Break;
            end;
        end;
    end;
end;

function TID3v2Frames.SetTXXXByDescription(Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    FrameID: TFrameID;
    i: Integer;
    GetDescription: String;
    GetLanguageID: TLanguageID;
    //GetContent: String;
begin
    Index := - 1;
    ConvertString2FrameID('TXXX', FrameID);
    ClearLanguageID(GetLanguageID);
    GetDescription := '';
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            {GetContent :=} GetUserDefinedTextInformation(i, GetDescription);
            if WideUpperCase(GetDescription) = WideUpperCase(Description) then begin
                Index := i;
                Break;
            end;
        end;
    end;
    if Index = - 1 then begin
        Index := AddFrame('TXXX');
    end;
    Result := SetTXXX(Index, Description, Text, TextEncoding);
end;

function TID3v2Frames.SetTXXXByDescriptionMultiple(Description: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    FrameID: TFrameID;
    i: Integer;
    GetDescription: String;
    GetLanguageID: TLanguageID;
    //GetContent: String;
begin
    Index := - 1;
    ConvertString2FrameID('TXXX', FrameID);
    ClearLanguageID(GetLanguageID);
    GetDescription := '';
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(FrameID, Frames[i].ID) then begin
            {GetContent :=} GetUserDefinedTextInformation(i, GetDescription);
            if WideUpperCase(GetDescription) = WideUpperCase(Description) then begin
                Index := i;
                Break;
            end;
        end;
    end;
    if Index = - 1 then begin
        Index := AddFrame('TXXX');
    end;
    Result := SetUserDefinedTextInformationMultiple(Index, Description, List, TextEncoding);
end;

function TID3v2Frames.SetTXXX(Index: Integer; Description: String; Text: String; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
begin
    Result := False;
    if (Index >= FrameCount)
    OR (Index < 0)
    then begin
        Exit;
    end;
    Result := SetUserDefinedTextInformation(Index, Description, Text, TextEncoding);
end;

function TID3v2Frames.GetListFrame(FrameID: String; List: TStrings): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    List.Clear;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetListFrame(Index, List);
end;

function TID3v2Frames.GetListFrame(FrameIndex: Integer; List: TStrings): Boolean;
begin
    List.Clear;
    List.Text := GetListFrameText(FrameIndex);
    Result := List.Text <> '';
end;

function TID3v2Frames.SetListFrame(FrameID: String; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(FrameID);
    end;
    Result := SetListFrame(Index, List, TextEncoding);
end;

function TID3v2Frames.SetListFrame(Index: Integer; List: TStrings; TextEncoding: TID3v2TextEncoding = teUnicode): Boolean;
var
    Data: Byte;
    Bytes: TBytes;
    i: Integer;
    Name: String;
    Value: String;
begin
    Result := False;
    if (Index >= FrameCount)
    OR (Index < 0)
    then begin
        Exit;
    end;
    Frames[Index].Stream.Clear;
    case TextEncoding of
        teUnicode: begin
            try
                Data := $01;
                Frames[Index].Stream.Write(Data, 1);
                Data := $FF;
                Frames[Index].Stream.Write(Data, 1);
                Data := $FE;
                Frames[Index].Stream.Write(Data, 1);
                for i := 0 to List.Count - 1 do begin
                    Name := List.Names[i];
                    Value := List.ValueFromIndex[i];
                    Frames[Index].Stream.Write(PWideChar(Name)^, (Length(Name) + 1) * 2);
                    Frames[Index].Stream.Write(PWideChar(Value)^, (Length(Value) + 1) * 2);
                end;
                Frames[Index].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teUTF8: begin
            try
                Data := $03;
                Frames[Index].Stream.Write(Data, 1);
                for i := 0 to List.Count - 1 do begin
                    Name := List.Names[i];
                    Value := List.ValueFromIndex[i];
                    Bytes := TEncoding.UTF8.GetBytes(Name);
                    Frames[Index].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[Index].Stream.Write(Data, 1);
                    Bytes := TEncoding.UTF8.GetBytes(Value);
                    Frames[Index].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[Index].Stream.Write(Data, 1);
                end;
                Frames[Index].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
        teANSI: begin
            try
                Data := $00;
                Frames[Index].Stream.Write(Data, 1);
                for i := 0 to List.Count - 1 do begin
                    Name := List.Names[i];
                    Value := List.ValueFromIndex[i];
                    Bytes := TEncoding.ANSI.GetBytes(Name);
                    Frames[Index].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[Index].Stream.Write(Data, 1);
                    Bytes := TEncoding.ANSI.GetBytes(Value);
                    Frames[Index].Stream.Write(Bytes[0], Length(Bytes));
                    Data := $0;
                    Frames[Index].Stream.Write(Data, 1);
                end;
                Frames[Index].Stream.Seek(0, soBeginning);
                Result := True;
            except
                //*
            end;
        end;
    end;
end;

function TID3v2Frames.GetListFrameText(FrameIndex: Integer): String;
var
    Encoding: Byte;
    BigEndian: Boolean;
    NilFound: Boolean;
    i: Integer;
begin
    Result := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        BigEndian := False;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get the content
        i := 0;
        while True do begin
            Result := Result + Frames[FrameIndex].Stream.ReadText(Encoding, @BigEndian, @NilFound);
            if (NOT NilFound)
            OR (Frames[FrameIndex].Stream.Position >= Frames[FrameIndex].Stream.Size)
            then begin
                Break;
            end;
            if Odd(i) then
                Result := Result + #13#10
            else
                Result := Result + '=';
            Inc(i);
        end;
        if Root.TrimText then begin
            Result := Trim(Result);
        end;
    except
        //*
    end;
end;

procedure TID3v2Tag.Assign(ID3v2Tag: TID3v2Tag);
var
    i: Integer;
    Index: Integer;
begin
    Clear;
    SourceFileName := ID3v2Tag.SourceFileName;
    Loaded := ID3v2Tag.Loaded;
    MajorVersion := ID3v2Tag.MajorVersion;
    MinorVersion := ID3v2Tag.MinorVersion;
    Flags := ID3v2Tag.Flags;
    Unsynchronised := ID3v2Tag.Unsynchronised;
    ExtendedHeader := ID3v2Tag.ExtendedHeader;
    Experimental := ID3v2Tag.Experimental;
    FooterPresent := ID3v2Tag.FooterPresent;
    Size := ID3v2Tag.Size;
    PaddingSize := ID3v2Tag.PaddingSize;
    PaddingToWrite := ID3v2Tag.PaddingToWrite;
    FSourceFileType := ID3v2Tag.SourceFileType;
    for i := 0 to ID3v2Tag.FrameCount - 1 do begin
        Index := AddFrame(ID3v2Tag.Frames[i].ID);
        Frames[Index].Assign(ID3v2Tag.Frames[i]);
    end;
    WAVInfo := ID3v2Tag.WAVInfo;
    DSFInfo.Assign(ID3v2Tag.DSFInfo);
    FPlayTime := ID3v2Tag.PlayTime;
    FSampleCount := ID3v2Tag.SampleCount;
    FBitRate := ID3v2Tag.BitRate;
end;

function TID3v2Frames.GetUFID(FrameID: String; var OwnerIdentifier: String): String;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := '';
    OwnerIdentifier := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetUFID(Index, OwnerIdentifier);
end;

function TID3v2Frames.FindUFIDByOwnerIdentifier(OwnerIdentifier: String; var Identifier: String): Integer;
var
    FrameID: TFrameID;
    i: Integer;
    GetOwnerIdentifier: String;
    GetIdentifier: String;
begin
    Result := - 1;
    Identifier := '';
    ConvertString2FrameID('UFID', FrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, FrameID) then begin
            GetIdentifier := GetUFID(i, GetOwnerIdentifier);
            if WideUpperCase(GetOwnerIdentifier) = WideUpperCase(OwnerIdentifier) then begin
                Result := i;
                Identifier := GetIdentifier;
                Break;
            end;
        end;
    end;
end;

function TID3v2Frames.SetUFIDByOwnerIdentifier(OwnerIdentifier: String; Identifier: String): Boolean;
var
    FrameID: TFrameID;
    i: Integer;
    GetOwnerIdentifier: String;
    //GetIdentifier: String;
    Index: Integer;
begin
    Index := - 1;
    ConvertString2FrameID('UFID', FrameID);
    for i := 0 to FrameCount - 1 do begin
        if IsSameFrameID(Frames[i].ID, FrameID) then begin
            {GetIdentifier :=} GetUFID(i, GetOwnerIdentifier);
            if WideUpperCase(GetOwnerIdentifier) = WideUpperCase(OwnerIdentifier) then begin
                Index := i;
                Break;
            end;
        end;
    end;
    if Index = - 1 then begin
        Index := AddFrame(FrameID);
    end;
    Result := SetUFID(Index, OwnerIdentifier, Identifier);
end;

function TID3v2Frames.GetUFID(FrameIndex: Integer; var OwnerIdentifier: String): String;
var
    i: Integer;
    Bytes: TBytes;
begin
    Result := '';
    Bytes := GetUFIDBytes(FrameIndex, OwnerIdentifier);
    for i := 0 to Length(Bytes) - 1 do
        if Bytes[i] <> 0 then
            Result := Result + Char(Bytes[i]);
end;

function TID3v2Frames.GetUFIDBytes(FrameIndex: Integer; var OwnerIdentifier: String): TBytes;
var
    DataSize: Int64;
begin
    Result := nil;
    OwnerIdentifier := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get owner identifier
        OwnerIdentifier := Frames[FrameIndex].Stream.ReadText;
        //* Get the content
        DataSize := Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position;
        if DataSize > 0 then begin
            SetLength(Result, DataSize);
            Frames[FrameIndex].Stream.Read2(Result[0], DataSize);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.SetUFID(FrameID: String; OwnerIdentifier: String; Identifier: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Index := AddFrame(ID);
        if Index < 0 then begin
            Exit;
        end;
    end;
    Result := SetUFID(Index, OwnerIdentifier, Identifier);
end;

function TID3v2Frames.SetUFID(FrameIndex: Integer; OwnerIdentifier: String; Identifier: String): Boolean;
var
    Data: Byte;
    i: Integer;
begin
    Result := False;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        Frames[FrameIndex].Stream.Clear;
        //* Write the Owner Identifier
        for i := StrLow to StrLow + Length(OwnerIdentifier) - 1 do begin
            Data := Ord(OwnerIdentifier[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Data := $00;
        Frames[FrameIndex].Stream.Write(Data, 1);
        //* Write the Identifier
        for i := StrLow to StrLow + Length(Identifier) - 1 do begin
            Data := Ord(Identifier[i]);
            Frames[FrameIndex].Stream.Write(Data, 1);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function ID3v2TagErrorCode2String(ErrorCode: Integer): String;
begin
    Result := 'Unknown error code.';
    case ErrorCode of
        ID3V2LIBRARY_SUCCESS: Result := 'Success.';
        ID3V2LIBRARY_ERROR: Result := 'Unknown error occured.';
        ID3V2LIBRARY_ERROR_NO_TAG_FOUND: Result := 'No ID3v2 tag found.';
        ID3V2LIBRARY_ERROR_EMPTY_TAG: Result := 'ID3v2 tag is empty.';
        ID3V2LIBRARY_ERROR_EMPTY_FRAMES: Result := 'ID3v2 tag contains only empty frames.';
        ID3V2LIBRARY_ERROR_OPENING_FILE: Result := 'Error opening file.';
        ID3V2LIBRARY_ERROR_READING_FILE: Result := 'Error reading file.';
        ID3V2LIBRARY_ERROR_WRITING_FILE: Result := 'Error writing file.';
        ID3V2LIBRARY_ERROR_CORRUPT: Result := 'Error: corrupt file.';
        ID3V2LIBRARY_ERROR_DOESNT_FIT: Result := 'Error: ID3v2 tag doesn''t fit into the file.';
        ID3V2LIBRARY_ERROR_NOT_SUPPORTED_VERSION: Result := 'Error: not supported ID3v2 version.';
        ID3V2LIBRARY_ERROR_NOT_SUPPORTED_FORMAT: Result := 'Error not supported file format.';
        ID3V2LIBRARY_ERROR_NEED_EXCLUSIVE_ACCESS: Result := 'Error: file is locked. Need exclusive access to write ID3v2 tag to this file.';
        ID3V2LIBRARY_ERROR_FILE_READ_ONLY: Result := 'Error writing file, file is read only.';
    end;
end;

function ValidID3v2FrameID(FrameID: TFrameID): Boolean;
var
    FrameIDChar: Char;
    i: Integer;
begin
    Result := True;
    for i := 0 to 3 do begin
        FrameIDChar := Char(FrameID[i]);
        //if NOT (FrameIDChar in ['A'..'Z'] + ['0'..'9']) then begin
        if NOT (((Ord(FrameIDChar) >= Ord('A')) AND (Ord(FrameIDChar) <= Ord('Z')))
        OR ((Ord(FrameIDChar) >= Ord('0')) AND (Ord(FrameIDChar) <= Ord('9'))))
        then begin
            Result := False;
            Break;
        end;
    end;
end;

function ValidID3v2FrameID2(FrameID: TFrameID): Boolean;
var
    FrameIDChar: Char;
    i: Integer;
begin
    Result := True;
    for i := 0 to 2 do begin
        FrameIDChar := Char(FrameID[i]);
        //if NOT (FrameIDChar in ['A'..'Z'] + ['0'..'9']) then begin
        if NOT (((Ord(FrameIDChar) >= Ord('A')) AND (Ord(FrameIDChar) <= Ord('Z')))
        OR ((Ord(FrameIDChar) >= Ord('0')) AND (Ord(FrameIDChar) <= Ord('9'))))
        then begin
            Result := False;
            Break;
        end;
    end;
end;

function GetID3v2FrameType(FrameID: TFrameID): TID3v2FrameType;
var
    TestForFrameID: TFrameID;
begin
    Result := ftBinary;

    if Char(FrameID[0]) = 'T' then begin
        Result := ftText;
    end;

    ConvertString2FrameID('TXXX', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID)
    //* TODO: all specified frames
    then begin
        Result := ftTextWithDescription;
    end;

    ConvertString2FrameID('CTOC', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftCTOC;
    end;

    ConvertString2FrameID('CHAP', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftCHAP;
    end;

    ConvertString2FrameID('COMM', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftTextWithDescriptionAndLangugageID;
    end;

    ConvertString2FrameID('USLT', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftTextWithDescriptionAndLangugageID;
    end;

    ConvertString2FrameID('TIPL', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftTextList;
    end;

    ConvertString2FrameID('TMCL', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftTextList;
    end;

    if Char(FrameID[0]) = 'W' then begin
        Result := ftURL;
    end;

    ConvertString2FrameID('WXXX', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftUserDefinedURL;
    end;

    ConvertString2FrameID('SESC', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftBinary;
    end;
    ConvertString2FrameID('SEBR', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftBinary;
    end;
    ConvertString2FrameID('SEFC', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftBinary;
    end;
    ConvertString2FrameID('GEOB', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftBinary;
    end;
    ConvertString2FrameID('APIC', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftBinary;
    end;
    ConvertString2FrameID('PCNT', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftPlayCount;
    end;
    ConvertString2FrameID('PRIV', TestForFrameID);
    if IsSameFrameID(FrameID, TestForFrameID) then begin
        Result := ftPRIV;
    end;
end;

procedure ConvertString2FrameID(StringFrameID: String; out FrameID: TFrameID);
begin
    FrameID[0] := Ord(StringFrameID[StrLow]);
    FrameID[1] := Ord(StringFrameID[StrLow + 1]);
    FrameID[2] := Ord(StringFrameID[StrLow + 2]);
    FrameID[3] := Ord(StringFrameID[StrLow + 3]);
end;

function ConvertFrameID2String(FrameID: TFrameID): String;
begin
    Result := Char(FrameID[0]) + Char(FrameID[1]) + Char(FrameID[2]);
    if FrameID[3] > 0 then
        Result := Result + Char(FrameID[3]);
end;

function IsSameFrameID(FrameID1: TFrameID; FrameID2: TFrameID): Boolean;
begin
    if (FrameID1[0] = FrameID2[0])
    AND (FrameID1[1] = FrameID2[1])
    AND (FrameID1[2] = FrameID2[2])
    AND (FrameID1[3] = FrameID2[3])
    then begin
        Result := True;
    end else begin
        Result := False;
    end;
end;

function IsSameFrameID(FrameID: TFrameID; StringFrameID: String): Boolean;
begin
    if (FrameID[0] = Ord(StringFrameID[StrLow]))
    AND (FrameID[1] = Ord(StringFrameID[StrLow + 1]))
    AND (FrameID[2] = Ord(StringFrameID[StrLow + 2]))
    AND (FrameID[3] = Ord(StringFrameID[StrLow + 3]))
    then begin
        Result := True;
    end else begin
        Result := False;
    end;
end;

function GetID3v2Size(PMemory: Pointer): Cardinal; overload;
type
    TID3v2Header = packed record
        ID: array [1..3] of Byte;
        Version: Byte;
        Revision: Byte;
        Flags: Byte;
        Size: Cardinal;
    end;
var
    Header: TID3v2Header;
begin
    // Get ID3v2 tag size (if exists)
    Result := 0;
    Move(PMemory^, Header, SizeOf(TID3v2Header));
    if (Header.ID[1] = Ord('I'))
    AND (Header.ID[2] = Ord('D'))
    AND (Header.ID[3] = Ord('3'))
    then begin
        UnSyncSafe(Header.Size, 4, Result);
        Inc(Result, 10);
    end;
end;

function GetID3v2Size(Stream: TStream): Cardinal; overload;
type
    TID3v2Header = packed record
        ID: array [1..3] of Byte;
        Version: Byte;
        Revision: Byte;
        Flags: Byte;
        Size: Cardinal;
    end;
var
    PreviousPosition: Int64;
    Header: TID3v2Header;
begin
    // Get ID3v2 tag size (if exists)
    Result := 0;
    PreviousPosition := Stream.Position;
    try
        Stream.Read2(Pointer(@Header)^, SizeOf(TID3v2Header));
        if (Header.ID[1] = Ord('I'))
        AND (Header.ID[2] = Ord('D'))
        AND (Header.ID[3] = Ord('3'))
        then begin
            UnSyncSafe(Header.Size, 4, Result);
            Inc(Result, 10);
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function ValidRIFF(Stream: TStream): Boolean;
var
    PreviousPosition: Int64;
    Identification: TRIFFID;
    RIFFChunkSize: DWord;
    ChunkID: TRIFFChunkID;
    ChunkSize: DWord;
begin
    Result := False;
    PreviousPosition := Stream.Position;
    try
        try
            Stream.Seek(0, soBeginning);
            Stream.Read2(Identification[0], 4);
            if (Identification[0] = RIFFID[0])
            AND (Identification[1] = RIFFID[1])
            AND (Identification[2] = RIFFID[2])
            AND (Identification[3] = RIFFID[3])
            then begin
                Stream.Read2(RIFFChunkSize, 4);
                Stream.Read2(ChunkID, 4);
                if (ChunkID[0] = RIFFWAVEID[0])
                AND (ChunkID[1] = RIFFWAVEID[1])
                AND (ChunkID[2] = RIFFWAVEID[2])
                AND (ChunkID[3] = RIFFWAVEID[3])
                then begin
                    Result := True;
                    ChunkSize := 0;
                    while Stream.Position < Stream.Size do begin
                        if Stream.Position >= Int64(RIFFChunkSize) + 8 then begin
                            Exit;
                        end;
                        Stream.Read2(ChunkID, 4);
                        Stream.Read2(ChunkSize, 4);
                        if Odd(ChunkSize) then begin
                            Inc(ChunkSize);
                        end;
                        if Stream.Position + ChunkSize > Stream.Size then begin
                            Result := False;
                            Exit;
                        end;
                        Stream.Seek(ChunkSize, soCurrent);
                    end;
                end;
            end;
        except
            Result := False;
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function ValidRF64(Stream: TStream): Boolean;
var
    PreviousPosition: Int64;
    Identification: TRIFFID;
    RIFFChunkSize: DWord;
    ChunkID: TRIFFChunkID;
    ChunkSize: DWord;
    Waveds64: TWaveds64;
    //RF64Size: Int64;
begin
    Result := False;
    //RF64Size := 0;
    PreviousPosition := Stream.Position;
    try
        try
            Stream.Seek(0, soBeginning);
            Stream.Read2(Identification[0], 4);
            if (Identification[0] = RF64ID[0])
            AND (Identification[1] = RF64ID[1])
            AND (Identification[2] = RF64ID[2])
            AND (Identification[3] = RF64ID[3])
            then begin
                Stream.Read2(RIFFChunkSize, 4);
                //RF64Size := RIFFChunkSize;
                Stream.Read2(ChunkID, 4);
                if (ChunkID[0] = RIFFWAVEID[0])
                AND (ChunkID[1] = RIFFWAVEID[1])
                AND (ChunkID[2] = RIFFWAVEID[2])
                AND (ChunkID[3] = RIFFWAVEID[3])
                AND (RIFFChunkSize = $FFFFFFFF)
                then begin
                    Stream.Read2(ChunkID, 4);
                    if (ChunkID[0] = Ord('d'))
                    AND (ChunkID[1] = Ord('s'))
                    AND (ChunkID[2] = Ord('6'))
                    AND (ChunkID[3] = Ord('4'))
                    then begin
                        Stream.Read2(Waveds64, SizeOf(TWaveds64));
                        //RF64Size := MakeInt64(Waveds64.RIFFSizeLow, Waveds64.RIFFSizeHigh);
                        Stream.Seek(Waveds64.ds64Size - SizeOf(TWaveds64) + 4 {table?}, soCurrent);
                    end;
                    Result := True;
                    ChunkSize := 0;
                    while Stream.Position < Stream.Size do begin
                        //if TagStream.Position >= RF64Size then begin
                        //    Exit;
                        //end;
                        Stream.Read2(ChunkID, 4);
                        Stream.Read2(ChunkSize, 4);
                        if Odd(ChunkSize) then begin
                            Inc(ChunkSize);
                        end;
                        if ((ChunkID[0] = Ord('d'))
                        AND (ChunkID[1] = Ord('a'))
                        AND (ChunkID[2] = Ord('t'))
                        AND (ChunkID[3] = Ord('a')))
                        AND (ChunkSize = $FFFFFFFF)
                        then begin
                            if Stream.Position + MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh) > Stream.Size then begin
                                Result := False;
                                Exit;
                            end;
                            Stream.Seek(MakeInt64(Waveds64.DataSizeLow, Waveds64.DataSizeHigh), soCurrent);
                        end else begin
                            if Stream.Position + ChunkSize > Stream.Size then begin
                                Result := False;
                                Exit;
                            end;
                            Stream.Seek(ChunkSize, soCurrent);
                        end;
                    end;
                end;
            end;
        except
            Result := False;
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function ValidDSF(Stream: TStream): Boolean;
var
    PreviousPosition: Int64;
    Identification: TRIFFID;
    DSFSize: UInt64;
begin
    Result := False;
    PreviousPosition := Stream.Position;
    try
        try
            Stream.Seek(0, soBeginning);
            Stream.Read2(Identification[0], 4);
            if (Identification[0] = DSFID[0])
            AND (Identification[1] = DSFID[1])
            AND (Identification[2] = DSFID[2])
            AND (Identification[3] = DSFID[3])
            then begin
                //Result := True;
                Stream.Seek(8, soCurrent);
                Stream.Read2(DSFSize, 8);
                Result := DSFSize <= Stream.Size;
            end;
        except
            Result := False;
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function TID3v2Tag.SaveDSF(Stream: TStream; WriteTagTotalSize: Cardinal): Integer;
var
    TempStream: TMemoryStream;
    TagSize: DWord;
    ID3v2Pointer: UInt64;
    DSFFileSize: UInt64;
begin
    TempStream := nil;
    try
        TagSize := SeekDSF(Stream);
        //* Update DSF file size
        Stream.Seek(12, soBeginning);
        Stream.Read2(DSFFileSize, 8);
        Dec(DSFFileSize, TagSize);
        Inc(DSFFileSize, WriteTagTotalSize);
        Stream.Seek(- 8, soCurrent);
        Stream.Write(DSFFileSize, 8);
        //* Update ID3v2 pointer
        Stream.Read2(ID3v2Pointer, 8);
        if ID3v2Pointer = 0 then begin
            ID3v2Pointer := DSFFileSize - WriteTagTotalSize;
            Stream.Seek(- 8, soCurrent);
            Stream.Write(ID3v2Pointer, 8);
        end;
        //* Check if there is tail data
        Stream.Seek(ID3v2Pointer, soBeginning);
        if Stream.Position + TagSize < Stream.Size then begin
            TempStream := TMemoryStream.Create;
            Stream.Seek(ID3v2Pointer + TagSize, soBeginning);
            TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
        end;
        //* Save ID3v2 tag at ID3v2 pointer
        Stream.Seek(ID3v2Pointer, soBeginning);
        //* Save the ID3v2 tag
        SaveTagToStream(Stream, 0);
        //* Set end of file
        Stream.Size := Stream.Position;
        //* Copy remaining data if have it
        if Assigned(TempStream) then begin
            TempStream.Seek(0, soBeginning);
            Stream.CopyFrom(TempStream, TempStream.Size);
            FreeAndNil(TempStream);
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

function ValidDFF(Stream: TStream): Boolean;
var
    PreviousPosition: Int64;
    Identification: TRIFFID;
    DFFSize: UInt64;
begin
    Result := False;
    PreviousPosition := Stream.Position;
    try
        try
            Stream.Seek(0, soBeginning);
            Stream.Read2(Identification[0], 4);
            if (Identification[0] = DFFID[0])
            AND (Identification[1] = DFFID[1])
            AND (Identification[2] = DFFID[2])
            AND (Identification[3] = DFFID[3])
            then begin
                Stream.Read2(DFFSize, 8);
                DFFSize := ReverseBytes64(DFFSize);
                Result := DFFSize + 12 <= Stream.Size;
            end;
        except
            Result := False;
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function TID3v2Tag.SaveDFF(Stream: TStream; WriteTagTotalSize: Cardinal): Integer;
var
    TempStream: TMemoryStream;
    ExistingTagSize: DWord;
    ExistingTagSizePadded: DWord;
    DFFFRM8Size: UInt64;
    ID3v2ChunkDataPosition: UInt64;
    ID3v2ChunkSize: UInt64;
    ZeroByte: Byte;
    Padded: Boolean;
begin
    ZeroByte := 0;
    TempStream := nil;
    try
        ExistingTagSize := SeekDFF(Stream);
        ExistingTagSizePadded := ExistingTagSize;
        if Odd(ExistingTagSizePadded) then begin
            Inc(ExistingTagSizePadded);
        end;
        ID3v2ChunkDataPosition := Stream.Position;
        //* Check if there is tail data
        if ID3v2ChunkDataPosition + ExistingTagSizePadded < Stream.Size then begin
            TempStream := TMemoryStream.Create;
            Stream.Seek(ID3v2ChunkDataPosition + ExistingTagSizePadded, soBeginning);
            TempStream.CopyFrom(Stream, Stream.Size - Stream.Position);
        end;
        //* Update DFF FRM8 chunk size
        Stream.Seek(4, soBeginning);
        Stream.Read2(DFFFRM8Size, 8);
        DFFFRM8Size := ReverseBytes64(DFFFRM8Size);
        Padded := Odd(WriteTagTotalSize);
        if ExistingTagSize = 0 then begin
            Inc(DFFFRM8Size, 12);
        end;
        Dec(DFFFRM8Size, ExistingTagSizePadded);
        if Padded then begin
            Inc(DFFFRM8Size, UInt64(WriteTagTotalSize) + 1);
        end else begin
            Inc(DFFFRM8Size, WriteTagTotalSize);
        end;
        Stream.Seek(- 8, soCurrent);
        DFFFRM8Size := ReverseBytes64(DFFFRM8Size);
        Stream.Write(DFFFRM8Size, 8);
        if ExistingTagSize = 0 then begin
            //* ID3v2ChunkDataPosition = at new chunk
            Stream.Seek(ID3v2ChunkDataPosition, soBeginning);
            Stream.Write(DFFID3v2ID[0], 4);
        end else begin
            //* ID3v2ChunkDataPosition = 'ID3 ' chunk data
            Stream.Seek(ID3v2ChunkDataPosition - 8, soBeginning);
        end;
        ID3v2ChunkSize := WriteTagTotalSize;
        ID3v2ChunkSize := ReverseBytes64(ID3v2ChunkSize);
        Stream.Write(ID3v2ChunkSize, 8);
        //* Save the ID3v2 tag
        SaveTagToStream(Stream);
        //* Pad for even
        if Padded then begin
            Stream.Write(ZeroByte, 1);
        end;
        //* Set end of file
        Stream.Size := Stream.Position;
        //* Copy remaining data if have it
        if Assigned(TempStream) then begin
            TempStream.Seek(0, soBeginning);
            Stream.CopyFrom(TempStream, TempStream.Size);
            FreeAndNil(TempStream);
        end;
        Result := ID3V2LIBRARY_SUCCESS;
    except
        Result := ID3V2LIBRARY_ERROR;
    end;
end;

{ TDSFInfo }

procedure TDSFInfo.Assign(DSFInfo: TDSFInfo);
begin
    FormatVersion := DSFInfo.FormatVersion;
    FormatID := DSFInfo.FormatID;
    ChannelType := DSFInfo.ChannelType;
    ChannelNumber := DSFInfo.ChannelNumber;
    SamplingFrequency := DSFInfo.SamplingFrequency;
    BitsPerSample := DSFInfo.BitsPerSample;
    SampleCount := DSFInfo.SampleCount;
    BlockSizePerChannel := DSFInfo.BlockSizePerChannel;
    PlayTime := DSFInfo.PlayTime;
end;

procedure TDSFInfo.Clear;
begin
    FormatVersion := 0;
    FormatID := 0;
    ChannelType := dsfctUnknown;
    ChannelNumber := 0;
    SamplingFrequency := 0;
    BitsPerSample := 0;
    SampleCount := 0;
    BlockSizePerChannel := 0;
    PlayTime := 0;
end;

function TDSFInfo.GetBitRate: Integer;
begin
    Result := Round((((SampleCount * ChannelNumber * BitsPerSample) / 8) / PlayTime) / 125);
end;

function TID3v2Tag.GetWAVEInformation(Stream: TStream): TWaveFmt;
var
    SourceHeader: TWaveHeader;
    ChunkIdent: TRIFFChunkID;
    ChunkSize: DWord;
    SourceISRF64: Boolean;
    Sourceds64: TWaveds64;
    PreviousPosition: Int64;
    DataSize32: DWord;
begin
    PreviousPosition := Stream.Position;
    try
        Stream.Seek(0, soBeginning);
        //* Check if WAV or RF64
        Stream.Read2(SourceHeader, SizeOf(TWaveHeader));
        //* Check if RF64
        SourceISRF64 := (SourceHeader.ident1[0] = RF64ID[0])
            AND (SourceHeader.ident1[1] = RF64ID[1])
            AND (SourceHeader.ident1[2] = RF64ID[2])
            AND (SourceHeader.ident1[3] = RF64ID[3])
        ;
        //* Check WAVE
        Stream.Read2(ChunkIdent, 4);
        if (ChunkIdent[0] <> RIFFWAVEID[0])
        OR (ChunkIdent[1] <> RIFFWAVEID[1])
        OR (ChunkIdent[2] <> RIFFWAVEID[2])
        OR (ChunkIdent[3] <> RIFFWAVEID[3])
        then begin
            Exit;
        end;
        //* If RF64 then there's a ds64 chunk
        if SourceISRF64 then begin
            Stream.Read2(ChunkIdent, 4);
            if (ChunkIdent[0] = Ord('d'))
            AND (ChunkIdent[1] = Ord('s'))
            AND (ChunkIdent[2] = Ord('6'))
            AND (ChunkIdent[3] = Ord('4'))
            then begin
                Stream.Read2(Sourceds64, SizeOf(TWaveds64));
                //DataSize := MakeInt64(Sourceds64.DataSizeLow, Sourceds64.DataSizeHigh);
                FSampleCount := MakeInt64(Sourceds64.SampleCountLow, Sourceds64.SampleCountHigh);
                Stream.Seek(Sourceds64.ds64Size - SizeOf(TWaveds64) + 4, soCurrent);
            end;
        end;
        //* Search for 'fmt '
        repeat
            Stream.Read2(ChunkIdent, 4);
            if (ChunkIdent[0] <> Ord('f'))
            OR (ChunkIdent[1] <> Ord('m'))
            OR (ChunkIdent[2] <> Ord('t'))
            OR (ChunkIdent[3] <> Ord(' '))
            then begin
                //* Not it, go to next chunk
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                Stream.Seek(ChunkSize, soCurrent);
            end;
        until ((ChunkIdent[0] = Ord('f'))
            AND (ChunkIdent[1] = Ord('m'))
            AND (ChunkIdent[2] = Ord('t'))
            AND (ChunkIdent[3] = Ord(' ')))
        OR (Stream.Position >= Stream.Size);
        //* We are at 'fmt ' position, read the content
        Stream.Read2(Result, SizeOf(TWaveFmt));
        //* If this is not a WAVE_FORMAT_EXTENSIBLE chunk clear this part
        if Result.FormatTag <> $FFFE then begin
            Result.cbSize := 0;
            Result.ValidBitsPerSample := 0;
            Result.ChannelMask := 0;
            FillChar(Result.SubFormat[0], SizeOf(Result.SubFormat), 0);
            Stream.Seek(- 24, soCurrent);
        end;
        //* Search for the 'data' chunk
        repeat
            Stream.Read2(ChunkIdent, 4);
            if (ChunkIdent[0] <> Ord('d'))
            OR (ChunkIdent[1] <> Ord('a'))
            OR (ChunkIdent[2] <> Ord('t'))
            OR (ChunkIdent[3] <> Ord('a'))
            then begin
                //* Not it, go to next chunk
                Stream.Read2(ChunkSize, 4);
                if Odd(ChunkSize) then begin
                    Inc(ChunkSize);
                end;
                Stream.Seek(ChunkSize, soCurrent);
            end;
        until ((ChunkIdent[0] = Ord('d'))
            AND (ChunkIdent[1] = Ord('a'))
            AND (ChunkIdent[2] = Ord('t'))
            AND (ChunkIdent[3] = Ord('a')))
        OR (Stream.Position >= Stream.Size);
        //* We are at 'data' position, read the size
        Stream.Read2(DataSize32, 4);
        //* If ordinary WAV use the chunk data size as the 'data size'
        if NOT SourceISRF64 then begin
            FSampleCount := DataSize32 div Result.BlockAlign;
        end;
        //* Calculate play time
        FPlayTime := SampleCount / Result.SamplesPerSec;
        //* Calculate bit rate
        FBitRate := Result.AvgBytesPerSec div 125;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

procedure TID3v2Tag.LoadWAVEAttributes(Stream: TStream);
begin
    FillChar(Self.WAVInfo, SizeOf(TWaveFmt), 0);
    Self.WAVInfo := GetWAVEInformation(Stream);
end;

function TID3v2Tag.MPEGValidHeader(MPEGStream: TStream): Integer;
var
    Data: Byte;
    Header: Longword;
    TmpHdr: Longword;
    Padding: Byte;
    PreviousPosition: Int64;
    MPEGHeader: TMPEGHeader;
begin
    //Result := 0;
    FillChar(MPEGHeader, SizeOf(TMPEGHeader), 0);
    PreviousPosition := MPEGStream.Position;
    try
        MPEGHeader.Position := PreviousPosition;
        MPEGStream.Read2(Data, 1);

        if Data = $FF then begin
            Header := Data;
            MPEGStream.Read2(Data, 1);
            if (Data = $F9)
            OR (Data = $FA)
            OR (Data = $FB)
            OR (Data = $FC)
            OR (Data = $FD)
            OR (Data = $F2)
            OR (Data = $F3)
            OR (Data = $E3)
            then begin
                Header := (Header SHL 8) OR Data;
                MPEGStream.Read2(Data, 1);
                Header := (Header SHL 8) OR Data;
                MPEGStream.Read2(Data, 1);
                Header := (Header SHL 8) OR Data;
            end else begin
                Exit;
            end;
        end else begin
            Exit;
        end;

        MPEGHeader.Header := Header;

        TmpHdr := ((Header shl 11) shr 30);
        case TmpHdr of
            $0: MPEGHeader.Version := tmpegv25;
            $1: MPEGHeader.Version := tmpegvUnknown;           // Reserved
            $2: MPEGHeader.Version := tmpegv2;
            $3: MPEGHeader.Version := tmpegv1;
        end;
        TmpHdr := ((Header shl 13) shr 30);
        case TmpHdr of
            $0: MPEGHeader.Layer := tmpeglUnknown;             // Reserved
            $1: MPEGHeader.Layer := tmpegl3;
            $2: MPEGHeader.Layer := tmpegl2;
            $3: MPEGHeader.Layer := tmpegl1;
        end;
        TmpHdr := ((Header shl 16) shr 28);
        if MPEGHeader.Version = tmpegv1 then begin
            if MPEGHeader.Layer = tmpegl3 then begin
                case TmpHdr of
                    $0: MPEGHeader.BitRate := 65535;           // Free bitrate
                    $1: MPEGHeader.BitRate := 32;
                    $2: MPEGHeader.BitRate := 40;
                    $3: MPEGHeader.BitRate := 48;
                    $4: MPEGHeader.BitRate := 56;
                    $5: MPEGHeader.BitRate := 64;
                    $6: MPEGHeader.BitRate := 80;
                    $7: MPEGHeader.BitRate := 96;
                    $8: MPEGHeader.BitRate := 112;
                    $9: MPEGHeader.BitRate := 128;
                    $A: MPEGHeader.BitRate := 160;
                    $B: MPEGHeader.BitRate := 192;
                    $C: MPEGHeader.BitRate := 224;
                    $D: MPEGHeader.BitRate := 256;
                    $E: MPEGHeader.BitRate := 320;
                    $F: MPEGHeader.BitRate := 0;               // Bad bitrate
                end;
            end;
            if MPEGHeader.Layer = tmpegl2 then begin
                case TmpHdr of
                    $0: MPEGHeader.BitRate := 65535;           // Free bitrate
                    $1: MPEGHeader.BitRate := 32;
                    $2: MPEGHeader.BitRate := 48;
                    $3: MPEGHeader.BitRate := 56;
                    $4: MPEGHeader.BitRate := 64;
                    $5: MPEGHeader.BitRate := 80;
                    $6: MPEGHeader.BitRate := 96;
                    $7: MPEGHeader.BitRate := 112;
                    $8: MPEGHeader.BitRate := 128;
                    $9: MPEGHeader.BitRate := 160;
                    $A: MPEGHeader.BitRate := 192;
                    $B: MPEGHeader.BitRate := 224;
                    $C: MPEGHeader.BitRate := 256;
                    $D: MPEGHeader.BitRate := 320;
                    $E: MPEGHeader.BitRate := 384;
                    $F: MPEGHeader.BitRate := 0;               // Bad bitrate
                end;
            end;
            if MPEGHeader.Layer = tmpegl1 then begin
                case TmpHdr of
                    $0: MPEGHeader.BitRate := 65535;           // Free bitrate
                    $1: MPEGHeader.BitRate := 32;
                    $2: MPEGHeader.BitRate := 64;
                    $3: MPEGHeader.BitRate := 96;
                    $4: MPEGHeader.BitRate := 128;
                    $5: MPEGHeader.BitRate := 160;
                    $6: MPEGHeader.BitRate := 192;
                    $7: MPEGHeader.BitRate := 224;
                    $8: MPEGHeader.BitRate := 256;
                    $9: MPEGHeader.BitRate := 288;
                    $A: MPEGHeader.BitRate := 320;
                    $B: MPEGHeader.BitRate := 352;
                    $C: MPEGHeader.BitRate := 384;
                    $D: MPEGHeader.BitRate := 416;
                    $E: MPEGHeader.BitRate := 448;
                    $F: MPEGHeader.BitRate := 0;               // Bad bitrate
                end;
            end;
            TmpHdr := ((Header shl 20) shr 30);
            case TmpHdr of
                $0: MPEGHeader.SampleRate := 44100;
                $1: MPEGHeader.SampleRate := 48000;
                $2: MPEGHeader.SampleRate := 32000;
                $3: MPEGHeader.SampleRate := 0;                // Reserved
            end;
            TmpHdr := ((Header shl 22) shr 31);
            case TmpHdr of
                $0: MPEGHeader.Padding := False;
                $1: MPEGHeader.Padding := True;
            end;
        end;
        if (MPEGHeader.Version = tmpegv2) OR (MPEGHeader.Version = tmpegv25) then begin
            if (MPEGHeader.Layer = tmpegl3) OR (MPEGHeader.Layer = tmpegl2) then begin
                case TmpHdr of
                    $0: MPEGHeader.BitRate := 65535;            // Free bitrate
                    $1: MPEGHeader.BitRate := 8;
                    $2: MPEGHeader.BitRate := 16;
                    $3: MPEGHeader.BitRate := 24;
                    $4: MPEGHeader.BitRate := 32;
                    $5: MPEGHeader.BitRate := 40;
                    $6: MPEGHeader.BitRate := 48;
                    $7: MPEGHeader.BitRate := 56;
                    $8: MPEGHeader.BitRate := 64;
                    $9: MPEGHeader.BitRate := 80;
                    $A: MPEGHeader.BitRate := 96;
                    $B: MPEGHeader.BitRate := 112;
                    $C: MPEGHeader.BitRate := 128;
                    $D: MPEGHeader.BitRate := 144;
                    $E: MPEGHeader.BitRate := 160;
                    $F: MPEGHeader.BitRate := 0;                // Bad bitrate
                end;
            end;
            if MPEGHeader.Layer = tmpegl1 then begin
                case TmpHdr of
                    $0: MPEGHeader.BitRate := 65535;            // Free bitrate
                    $1: MPEGHeader.BitRate := 32;
                    $2: MPEGHeader.BitRate := 48;
                    $3: MPEGHeader.BitRate := 56;
                    $4: MPEGHeader.BitRate := 64;
                    $5: MPEGHeader.BitRate := 80;
                    $6: MPEGHeader.BitRate := 96;
                    $7: MPEGHeader.BitRate := 112;
                    $8: MPEGHeader.BitRate := 128;
                    $9: MPEGHeader.BitRate := 144;
                    $A: MPEGHeader.BitRate := 160;
                    $B: MPEGHeader.BitRate := 176;
                    $C: MPEGHeader.BitRate := 192;
                    $D: MPEGHeader.BitRate := 224;
                    $E: MPEGHeader.BitRate := 256;
                    $F: MPEGHeader.BitRate := 0;                // Bad bitrate
                end;
            end;
            TmpHdr := ((Header shl 20) shr 30);
            if (MPEGHeader.Version = tmpegv2) then begin
                case TmpHdr of
                    $0: MPEGHeader.SampleRate := 22050;
                    $1: MPEGHeader.SampleRate := 24000;
                    $2: MPEGHeader.SampleRate := 16000;
                    $3: MPEGHeader.SampleRate := 0;             // Reserved
                end;
            end;
            if (MPEGHeader.Version = tmpegv25) then begin
                case TmpHdr of
                    $0: MPEGHeader.SampleRate := 32000;
                    $1: MPEGHeader.SampleRate := 16000;
                    $2: MPEGHeader.SampleRate := 8000;
                    $3: MPEGHeader.SampleRate := 0;             // Reserved
                end;
            end;
            TmpHdr := ((Header shl 22) shr 31);
            case TmpHdr of
                $0: MPEGHeader.Padding := False;
                $1: MPEGHeader.Padding := True;
            end;
        end;
        TmpHdr := ((Header shl 30) shr 30);
        case TmpHdr of
            $0: MPEGHeader.Emphasis := tmpegeNo;
            $1: MPEGHeader.Emphasis := tmpege5015;
            $2: MPEGHeader.Emphasis := tmpegeUnknown;
            $3: MPEGHeader.Emphasis := tmpegeCCITJ17;
        end;
        //(*
        if MPEGHeader.Padding
            then Padding := 1
            else Padding := 0;
        try
            if (MPEGHeader.Version = tmpegv1) then begin
                if MPEGHeader.SampleRate <> 0 then begin
                    if MPEGHeader.Layer = tmpegl1
                        then MPEGHeader.FrameSize := Trunc(24000 * (MPEGHeader.BitRate / MPEGHeader.SampleRate) + Padding);
                    if (MPEGHeader.Layer = tmpegl2)
                    OR (MPEGHeader.Layer = tmpegl3)
                        then MPEGHeader.FrameSize := Trunc(144000 * (MPEGHeader.BitRate / MPEGHeader.SampleRate ) + Padding);
                end else MPEGHeader.FrameSize := 0;
            end;
            if (MPEGHeader.Version = tmpegv2)
            OR (MPEGHeader.Version = tmpegv25)
            then begin
                if MPEGHeader.SampleRate <> 0 then begin
                    if MPEGHeader.Layer = tmpegl1
                        then MPEGHeader.FrameSize := Trunc(24000 * (MPEGHeader.BitRate / MPEGHeader.SampleRate) + Padding);
                    if (MPEGHeader.Layer = tmpegl2)
                    OR (MPEGHeader.Layer = tmpegl3)
                        then MPEGHeader.FrameSize := Trunc(72000 * (MPEGHeader.BitRate / MPEGHeader.SampleRate) + Padding);
                end else MPEGHeader.FrameSize := 0;
            end;
        except
            //* Devide by zero possible
        end;
        //*)
    finally
        MPEGStream.Seek(PreviousPosition, soBeginning);
        //* Check valid
        if (MPEGHeader.Version = tmpegvUnknown)
        OR (MPEGHeader.Layer = tmpeglUnknown)
        OR (MPEGHeader.BitRate = 0)
        OR (MPEGHeader.SampleRate = 0)
        then begin
            Result := 0;
        end else begin
            Result := MPEGHeader.FrameSize;
        end;
    end;
end;

function TID3v2Tag.GetSampleCount: Int64;
var
    SamplesPerFrame: Integer;
begin
    Result := 0;
    if FSampleCount <> 0 then begin
        Result := FSampleCount;
    end else begin
        if SourceFileType = sftMPEG then begin
            if MPEGInfo.FrameCount > 0 then begin
                SamplesPerFrame := 0;
                case MPEGInfo.Version of
                    tmpegv1: begin
                        case MPEGInfo.Layer of
                            tmpegl1: begin
                                SamplesPerFrame := 384;
                            end;
                            tmpegl2, tmpegl3: begin
                                SamplesPerFrame := 1152;
                            end;
                        end;
                    end;
                    tmpegv2, tmpegv25: begin
                        case MPEGInfo.Layer of
                            tmpegl1: begin
                                SamplesPerFrame := 384;
                            end;
                            tmpegl2: begin
                                SamplesPerFrame := 1152;
                            end;
                            tmpegl3: begin
                                SamplesPerFrame := 576;
                            end;
                        end;
                    end;
                end;
                FSampleCount := MPEGInfo.FrameCount * SamplesPerFrame - MPEGInfo.EncoderDelay - MPEGInfo.EncoderPadding;
                Result := FSampleCount;
            end;
        end;
    end;
end;

function TID3v2Frames.GetBytes(FrameIndex: Integer): TBytes;
var
    DataSize: Int64;
begin
    SetLength(Result, 0);
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get the content
        DataSize := Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position;
        if DataSize > 0 then begin
            SetLength(Result, DataSize);
            Frames[FrameIndex].Stream.Read2(Result[0], DataSize);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.GetAENC(FrameIndex: Integer; var OwnerIdentifier: String; var PreviewStart, PreviewLength: Word): TBytes;
var
    DataWord: Word;
    DataSize: Int64;
begin
    SetLength(Result, 0);
    OwnerIdentifier := '';
    PreviewStart := 0;
    PreviewLength := 0;
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get owner identifier
        OwnerIdentifier := Frames[FrameIndex].Stream.ReadText;
        //* Get preview start
        Frames[FrameIndex].Stream.Read2(DataWord, 2);
        PreviewStart := ReverseBytes16(DataWord);
        //* Get preview length
        Frames[FrameIndex].Stream.Read2(DataWord, 2);
        PreviewLength := ReverseBytes16(DataWord);
        //* Get the content
        DataSize := Frames[FrameIndex].Stream.Size - Frames[FrameIndex].Stream.Position;
        if DataSize > 0 then begin
            SetLength(Result, DataSize);
            Frames[FrameIndex].Stream.Read2(Result[0], DataSize);
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
    except
        //*
    end;
end;

function TID3v2Frames.GetAENC(FrameID: String; var OwnerIdentifier: String; var PreviewStart, PreviewLength: Word): TBytes;
var
    Index: Integer;
    ID: TFrameID;
begin
    SetLength(Result, 0);
    OwnerIdentifier := '';
    PreviewStart := 0;
    PreviewLength := 0;
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetAENC(Index, OwnerIdentifier, PreviewStart, PreviewLength);
end;

function TID3v2Tag.GetAIFFInformation(Stream: TStream): TAIFFInformation;
var
    ChunkSize: DWord;
    PreviousPosition: Int64;
    ChunkID: TAIFFChunkID;
    AIFFSize: DWord;
    DataByte: Byte;
    DataWord: Word;
    DataDWord: DWord;
    {$IFNDEF FPC}
        {$IFDEF WIN64}
        DataExtended: TExtendedX87;
        {$ELSE}
        DataExtended: Extended;
        {$ENDIF}
    {$ELSE}
    DataExtended: Extended;
    {$ENDIF}
    AIFC: Boolean;
    i: Integer;
    DataSize: Int64;
    ChunksStartPosition: Int64;
    Bytes: TBytes;
begin
    DataSize := 0;
    PreviousPosition := Stream.Position;
    try
        Stream.Seek(0, soBeginning);
        //* Check if AIFF/AIFC
        Stream.Read2(ChunkID, SizeOf(TAIFFChunkID));
        if ((ChunkID[0] = AIFFID[0])
        AND (ChunkID[1] = AIFFID[1])
        AND (ChunkID[2] = AIFFID[2])
        AND (ChunkID[3] = AIFFID[3]))
        then begin
            //* AIFF container size
            Stream.Read2(AIFFSize, 4);
            AIFFSize := ReverseBytes32(AIFFSize);
            //* Check if AIFC
            Stream.Read2(ChunkID, SizeOf(TAIFFChunkID));
            if ((ChunkID[0] = AIFCChunkID[0])
            AND (ChunkID[1] = AIFCChunkID[1])
            AND (ChunkID[2] = AIFCChunkID[2])
            AND (ChunkID[3] = AIFCChunkID[3]))
            then begin
                AIFC := True;
            end else begin
                AIFC := False;
            end;
            //* If AIFF or AIFC continue
            if ((ChunkID[0] = AIFFChunkID[0])
            AND (ChunkID[1] = AIFFChunkID[1])
            AND (ChunkID[2] = AIFFChunkID[2])
            AND (ChunkID[3] = AIFFChunkID[3]))
            OR ((ChunkID[0] = AIFCChunkID[0])
            AND (ChunkID[1] = AIFCChunkID[1])
            AND (ChunkID[2] = AIFCChunkID[2])
            AND (ChunkID[3] = AIFCChunkID[3]))
            then begin
                //* Store chunks root position
                ChunksStartPosition := Stream.Position;
                //* Search for the COMM chunk
                repeat
                    Stream.Read2(ChunkID, 4);
                    Stream.Read2(ChunkSize, 4);
                    ChunkSize := ReverseBytes32(ChunkSize);
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    if (ChunkID[0] <> Ord('C'))
                    OR (ChunkID[1] <> Ord('O'))
                    OR (ChunkID[2] <> Ord('M'))
                    OR (ChunkID[3] <> Ord('M'))
                    then begin
                        //* Go to next chunk
                        Stream.Seek(ChunkSize, soCurrent);
                    end else begin
                        //* Read in COMM content
                        Stream.Read2(DataWord, 2);
                        Result.Channels := ReverseBytes16(DataWord);
                        Stream.Read2(DataDWord, 4);
                        Result.SampleFrames := ReverseBytes32(DataDWord);
                        Stream.Read2(DataWord, 2);
                        Result.SampleSize := ReverseBytes16(DataWord);
                        Stream.Read2(DataExtended, 10);
                        {$IFDEF FPC}
                            //* Not supported
                            //ReverseExtended(DataExtended, DataExtended);
                        {$ELSE}
                            {$IFDEF WIN64}
                            ReverseExtended(DataExtended, DataExtended);
                            {$ELSE}
                            //ReverseExtended(TExtendedBytes(DataExtended), TExtendedBytes(DataExtended));
                            //* Not supported
                            DataExtended := 0;
                            {$ENDIF}
                        {$ENDIF}
                        Result.SampleRate := DataExtended;
                        //* If AIFC we have more content
                        if AIFC then begin
                            //* Read compression ID (4 chars)
                            Result.CompressionID := '';
                            for i := 1 to 4 do begin
                                Stream.Read2(DataByte, 1);
                                Result.CompressionID := Result.CompressionID + Char(DataByte);
                            end;
                            //* Read compression description (string)
                            Stream.Read2(DataByte, 1);
                            SetLength(Bytes, DataByte);
                            Stream.Read2(Bytes[0], DataByte);
                            Result.Compression := TEncoding.ANSI.GetString(Bytes);
                        end;
                    end;
                until ((ChunkID[0] = Ord('C'))
                    AND (ChunkID[1] = Ord('O'))
                    AND (ChunkID[2] = Ord('M'))
                    AND (ChunkID[3] = Ord('M')))
                OR (Stream.Position >= AIFFSize);
                //* Go to root position to search for SSND
                Stream.Seek(ChunksStartPosition, soBeginning);
                //* Search for SSND
                repeat
                    Stream.Read2(ChunkID, 4);
                    Stream.Read2(ChunkSize, 4);
                    ChunkSize := ReverseBytes32(ChunkSize);
                    if Odd(ChunkSize) then begin
                        Inc(ChunkSize);
                    end;
                    if (ChunkID[0] <> Ord('S'))
                    OR (ChunkID[1] <> Ord('S'))
                    OR (ChunkID[2] <> Ord('N'))
                    OR (ChunkID[3] <> Ord('D'))
                    then begin
                        //* Go to next chunk
                        Stream.Seek(ChunkSize, soCurrent);
                    end else begin
                        //* We have the audio data size
                        DataSize := ChunkSize;
                    end;
                until ((ChunkID[0] = Ord('S'))
                    AND (ChunkID[1] = Ord('S'))
                    AND (ChunkID[2] = Ord('N'))
                    AND (ChunkID[3] = Ord('D')))
                OR (Stream.Position >= AIFFSize);
            end;
        end;
        //* Calculate play time
        if Result.SampleRate <> 0 then begin
            FPlayTime := Result.SampleFrames / Result.SampleRate;
        end else begin
            FPlayTime := 0;
        end;
        //* Calculate bit rate
        if FPlayTime <> 0 then begin
            FBitRate := Trunc(DataSize / (125 * FPlayTime ) + 0.5); // bitrate (Kbps)
        end else begin
            FBitRate := 0;
        end;
    finally
        Stream.Seek(PreviousPosition, soBeginning);
    end;
end;

function TID3v2Tag.GetMPEGHeaderInformation(Stream: TStream): Boolean;
Const
    //* "Xing"
    VBR_ID_1 = $58;
    VBR_ID_2 = $69;
    VBR_ID_3 = $6E;
    VBR_ID_4 = $67;
    //* "Info"
    CBR_ID_1 = $49;
    CBR_ID_2 = $6E;
    CBR_ID_3 = $66;
    CBR_ID_4 = $6F;
    //* "VBRI"
    VBRI_ID_1 = Ord('V');
    VBRI_ID_2 = Ord('B');
    VBRI_ID_3 = Ord('R');
    VBRI_ID_4 = Ord('I');
var
    PreviousPosition: Int64;
    DataByte: Byte;
    DataWord: Word;
    DataDWord: DWord;
    HeaderFlags: DWord;
    Offset: Int64;
    i: Integer;
begin
    //* Clear
    Result := False;
    MPEGInfo.VBR := False;
    MPEGInfo.FrameCount := 0;
    MPEGInfo.Quality := 0;
    MPEGInfo.Bytes := 0;
    //* Calculate info offset
    Offset := 0;
    case MPEGInfo.Version of
        //tmpegvUnknown: Exit;
        tmpegv1: begin
            case MPEGInfo.ChannelMode of
                //tmpegcmUnknown: Exit;
                tmpegcmMono: Offset := 17;
                tmpegcmDualChannel, tmpegcmJointStereo, tmpegcmStereo: Offset := 32;
            end;
        end;
        tmpegv2, tmpegv25: begin
            case MPEGInfo.ChannelMode of
                //tmpegcmUnknown: Exit;
                tmpegcmMono: Offset := 9;
                tmpegcmDualChannel, tmpegcmJointStereo, tmpegcmStereo: Offset := 17;
            end;
        end;
    end;
    //* Store current position in stream
    PreviousPosition := Stream.Position;
    try
        //* Check 'Xing'
        try
            Stream.Seek(4 + Offset, soCurrent);
            Stream.Read2(DataByte, 1);
            if DataByte = VBR_ID_1 then begin
                Stream.Read2(DataByte, 1);
                if DataByte = VBR_ID_2 then begin
                    Stream.Read2(DataByte, 1);
                    if DataByte = VBR_ID_3 then begin
                        Stream.Read2(DataByte, 1);
                        if DataByte = VBR_ID_4 then begin
                            MPEGInfo.HasXing := True;
                            MPEGInfo.VBR := True;
                            Stream.Read2(HeaderFlags, 4);
                            HeaderFlags := ReverseBytes32(HeaderFlags);
                            //* Total frames
                            if (HeaderFlags AND $1) > 0 then begin
                                Stream.Read2(DataDWord, 4);
                                DataDWord := ReverseBytes32(DataDWord);
                                MPEGInfo.FrameCount := DataDWord;
                            end;
                            //* Bytes
                            if (HeaderFlags AND $2) > 0 then begin
                                Stream.Read2(DataDWord, 4);
                                DataDWord := ReverseBytes32(DataDWord);
                                MPEGInfo.Bytes := DataDWord;
                            end;
                            //* TOC
                            if (HeaderFlags AND $4) > 0 then begin
                                Stream.Seek(100, soCurrent);
                            end;
                            //* Quality
                            if (HeaderFlags AND $8) > 0 then begin
                                Stream.Read2(DataDWord, 4);
                                DataDWord := ReverseBytes32(DataDWord);
                                MPEGInfo.Quality := DataDWord;
                            end;
                            //* EncoderShortVersionString
                            for i := 1 to 9 do begin
                                Stream.Read2(DataByte, 1);
                                MPEGInfo.EncoderShortVersionString := MPEGInfo.EncoderShortVersionString + Char(DataByte);
                            end;
                            //* InfoTagRevision & VBRMethod
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.InfoTagRevision := DataByte SHR 4;
                            MPEGInfo.VBRMethod := DataByte AND $F;
                            //* LowpassFilterValue
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.LowpassFilterValue := DataByte * 100;
                            //* PeakSignalSmplitude
                            Stream.Read2(MPEGInfo.PeakSignalSmplitude, 4);
                            //* RadioReplayGain
                            Stream.Read2(MPEGInfo.RadioReplayGain, 2);
                            //* AudiophileReplayGain
                            Stream.Read2(MPEGInfo.AudiophileReplayGain, 2);
                            //* EncodingFlags & ATHType
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncodingFlags := DataByte SHR 4;
                            MPEGInfo.ATHType := DataByte AND $F;
                            //* ABROrMinimalBitrate
                            Stream.Read2(MPEGInfo.ABROrMinimalBitrate, 1);
                            //* Delay & padding
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncoderDelay := DataByte SHL 4;
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncoderDelay := MPEGInfo.EncoderDelay OR DataByte SHR 4;
                            MPEGInfo.EncoderPadding := DataByte SHL 8;
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncoderPadding := MPEGInfo.EncoderPadding OR DataByte;
                            //* Misc
                            Stream.Read2(MPEGInfo.Misc, 1);
                            //* MP3Gain
                            Stream.Read2(MPEGInfo.MP3Gain, 1);
                            //* PresetAndSurroundInfo
                            Stream.Read2(MPEGInfo.PresetAndSurroundInfo, 2);
                            //* MusicLength
                            Stream.Read2(MPEGInfo.MusicLength, 4);
                            //* MusicCRC
                            Stream.Read2(MPEGInfo.MusicCRC, 2);
                            //* CRC16OfInfoTag
                            Stream.Read2(MPEGInfo.CRC16OfInfoTag, 2);
                            Result := True;
                            Exit;
                        end;
                    end;
                end;
            end;
        finally
            Stream.Seek(PreviousPosition, soBeginning)
        end;
        //* Check 'Info'
        try
            Stream.Seek(4 + Offset, soCurrent);
            Stream.Read2(DataByte, 1);
            if DataByte = CBR_ID_1 then begin
                Stream.Read2(DataByte, 1);
                if DataByte = CBR_ID_2 then begin
                    Stream.Read2(DataByte, 1);
                    if DataByte = CBR_ID_3 then begin
                        Stream.Read2(DataByte, 1);
                        if DataByte = CBR_ID_4 then begin
                            MPEGInfo.HasInfo := True;
                            MPEGInfo.VBR := False;
                            Stream.Read2(HeaderFlags, 4);
                            HeaderFlags := ReverseBytes32(HeaderFlags);
                            //* Total frames
                            if (HeaderFlags AND $1) > 0 then begin
                                Stream.Read2(DataDWord, 4);
                                DataDWord := ReverseBytes32(DataDWord);
                                MPEGInfo.FrameCount := DataDWord;
                            end;
                            //* Bytes
                            if (HeaderFlags AND $2) > 0 then begin
                                Stream.Read2(DataDWord, 4);
                                DataDWord := ReverseBytes32(DataDWord);
                                MPEGInfo.Bytes := DataDWord;
                            end;
                            //* TOC
                            if (HeaderFlags AND $4) > 0 then begin
                                Stream.Seek(100, soCurrent);
                            end;
                            //* Quality
                            if (HeaderFlags AND $8) > 0 then begin
                                Stream.Read2(DataDWord, 4);
                                DataDWord := ReverseBytes32(DataDWord);
                                MPEGInfo.Quality := DataDWord;
                            end;
                            //* EncoderShortVersionString
                            for i := 1 to 9 do begin
                                Stream.Read2(DataByte, 1);
                                MPEGInfo.EncoderShortVersionString := MPEGInfo.EncoderShortVersionString + Char(DataByte);
                            end;
                            //* InfoTagRevision & VBRMethod
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.InfoTagRevision := DataByte SHR 4;
                            MPEGInfo.VBRMethod := DataByte AND $F;
                            //* LowpassFilterValue
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.LowpassFilterValue := DataByte * 100;
                            //* PeakSignalSmplitude
                            Stream.Read2(MPEGInfo.PeakSignalSmplitude, 4);
                            //* RadioReplayGain
                            Stream.Read2(MPEGInfo.RadioReplayGain, 2);
                            //* AudiophileReplayGain
                            Stream.Read2(MPEGInfo.AudiophileReplayGain, 2);
                            //* EncodingFlags & ATHType
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncodingFlags := DataByte SHR 4;
                            MPEGInfo.ATHType := DataByte AND $F;
                            //* ABROrMinimalBitrate
                            Stream.Read2(MPEGInfo.ABROrMinimalBitrate, 1);
                            //* Delay & padding
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncoderDelay := DataByte SHL 4;
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncoderDelay := MPEGInfo.EncoderDelay OR DataByte SHR 4;
                            MPEGInfo.EncoderPadding := DataByte SHL 8;
                            Stream.Read2(DataByte, 1);
                            MPEGInfo.EncoderPadding := MPEGInfo.EncoderPadding OR DataByte;
                            //* Misc
                            Stream.Read2(MPEGInfo.Misc, 1);
                            //* MP3Gain
                            Stream.Read2(MPEGInfo.MP3Gain, 1);
                            //* PresetAndSurroundInfo
                            Stream.Read2(MPEGInfo.PresetAndSurroundInfo, 2);
                            //* MusicLength
                            Stream.Read2(MPEGInfo.MusicLength, 4);
                            //* MusicCRC
                            Stream.Read2(MPEGInfo.MusicCRC, 2);
                            //* CRC16OfInfoTag
                            Stream.Read2(MPEGInfo.CRC16OfInfoTag, 2);
                            Result := True;
                            Exit;
                        end;
                    end;
                end;
            end;
        finally
            Stream.Seek(PreviousPosition, soBeginning)
        end;
        //* Check 'VBRI'
        try
            Stream.Seek(4 + 32, soCurrent);
            Stream.Read2(DataByte, 1);
            if DataByte = VBRI_ID_1 then begin
                Stream.Read2(DataByte, 1);
                if DataByte = VBRI_ID_2 then begin
                    Stream.Read2(DataByte, 1);
                    if DataByte = VBRI_ID_3 then begin
                        Stream.Read2(DataByte, 1);
                        if DataByte = VBRI_ID_4 then begin
                            MPEGInfo.HasVBRI := True;
                            MPEGInfo.VBR := True;
                            //* Skip 'Version ID' and 'Delay'
                            Stream.Seek(4, soCurrent);
                            //* Quality
                            Stream.Read2(DataWord, 2);
                            DataWord := ReverseBytes16(DataWord);
                            MPEGInfo.Quality := DataDWord;
                            //* Bytes
                            Stream.Read2(DataDWord, 4);
                            DataDWord := ReverseBytes32(DataDWord);
                            MPEGInfo.Bytes := DataDWord;
                            //* Total frames
                            Stream.Read2(DataDWord, 4);
                            DataDWord := ReverseBytes32(DataDWord);
                            MPEGInfo.FrameCount := DataDWord;
                            Result := True;
                            Exit;
                        end;
                    end;
                end;
            end;
        finally
            Stream.Seek(PreviousPosition, soBeginning)
        end;
    except
        Result := False;
    end;
end;

function TID3v2Frames.GetOWNE(FrameIndex: Integer; var PricePaid, DateOfPurchase, Seller: String): Boolean;
var
    Encoding: Byte;
    i: Integer;
    DataByte: Byte;
begin
    Result := False;
    PricePaid := '';
    DateOfPurchase := '';
    Seller := '';
    if (FrameIndex >= FrameCount)
    OR (FrameIndex < 0)
    then begin
        Exit;
    end;
    try
        if Frames[FrameIndex].Stream.Size = 0 then begin
            Exit;
        end;
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        //* Get encoding
        Encoding := Frames[FrameIndex].Stream.ReadEncoding;
        //* Get price paid
        PricePaid := Frames[FrameIndex].Stream.ReadText;
        //* Get date of purchase
        for i := 1 to 8 do begin
            Frames[FrameIndex].Stream.Read2(DataByte, 1);
            DateOfPurchase := DateOfPurchase + Char(DataByte);
        end;
        //* Get seller
        Seller := Frames[FrameIndex].Stream.ReadText(Encoding);
        Frames[FrameIndex].Stream.Seek(0, soBeginning);
        Result := True;
    except
        //*
    end;
end;

function TID3v2Frames.GetOWNE(FrameID: String; var PricePaid, DateOfPurchase, Seller: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    PricePaid := '';
    DateOfPurchase := '';
    Seller := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetOWNE(Index, PricePaid, DateOfPurchase, Seller);
end;

function TID3v2Frames.GetOWNE(FrameIndex: Integer; var PricePaid: String; var DateOfPurchase: TDate; var Seller: String): Boolean;
var
    DateOfPurchaseStr: String;
    DecodeError: Boolean;
begin
    DateOfPurchase := 0;
    Result := GetOWNE(FrameIndex, PricePaid, DateOfPurchaseStr, Seller);
    if Result then begin
        DecodeError := False;
        DateOfPurchase := ID3v2DecodeDateOnly(DateOfPurchaseStr, DecodeError);
    end;
end;

function TID3v2Frames.GetOWNE(FrameID: String; var PricePaid: String; var DateOfPurchase: TDate; var Seller: String): Boolean;
var
    Index: Integer;
    ID: TFrameID;
begin
    Result := False;
    PricePaid := '';
    DateOfPurchase := 0;
    Seller := '';
    AnsiStringToPAnsiChar(FrameID, ID);
    Index := FrameExists(ID);
    if Index < 0 then begin
        Exit;
    end;
    Result := GetOWNE(Index, PricePaid, DateOfPurchase, Seller);
end;

function ID3v2SearchTag(Stream: TStream; SearchLength: Cardinal): Boolean;
var
    StartPosition: Int64;
    SearchStream: TMemoryStream;
    i: Integer;
begin
    Result := False;
    SearchStream := TMemoryStream.Create;
    try
        StartPosition := Stream.Position;
        SearchStream.CopyFrom(Stream, Min(SearchLength, Stream.Size - Stream.Position));
        for i := 0 to SearchStream.Size - 1 do begin
            SearchStream.Seek(i, soBeginning);
            if ID3v2ValidTag(SearchStream) then begin
                Stream.Seek(StartPosition + i, soBeginning);
                Result := True;
                Exit;
            end;
        end;
        //* If not found seek to original position
        Stream.Seek(StartPosition, soBeginning);
    finally
        FreeAndNil(SearchStream);
    end;
end;

function GetAPEv2TagSize(CheckStream: TStream): Integer;
type
    TAPEv2Header = packed record
        Version: Cardinal;
        TagSize: Cardinal;
        ItemCount: Cardinal;
        Flags: Cardinal;
        Reserved: Int64;
    end;

    function APEv2ValidTag(Stream: TStream): Boolean;
    var
        Identification: Array [1..8] of Byte;
        APEv2ID: Array [1..8] of Byte;
    begin
        Result := False;
        try
            APEv2ID[1] := Ord('A');
            APEv2ID[2] := Ord('P');
            APEv2ID[3] := Ord('E');
            APEv2ID[4] := Ord('T');
            APEv2ID[5] := Ord('A');
            APEv2ID[6] := Ord('G');
            APEv2ID[7] := Ord('E');
            APEv2ID[8] := Ord('X');
            FillChar(Identification, SizeOf(Identification), 0);
            Stream.Read(Identification[1], 8);
            if (Identification[1] = APEv2ID[1])
            AND (Identification[2] = APEv2ID[2])
            AND (Identification[3] = APEv2ID[3])
            AND (Identification[4] = APEv2ID[4])
            AND (Identification[5] = APEv2ID[5])
            AND (Identification[6] = APEv2ID[6])
            AND (Identification[7] = APEv2ID[7])
            AND (Identification[8] = APEv2ID[8])
            then begin
                Result := True;
            end;
        except
            //*
        end;
    end;

var
    ID3V1TAGIDRead: Array[0..2] of Byte;
    APEv2Header: TAPEv2Header;
    PreviousPosition: Int64;
begin
    Result := 0;
    PreviousPosition := CheckStream.Position;
    try
        try
            CheckStream.Seek(- 128, soEnd);
            CheckStream.Read(ID3V1TAGIDRead, 3);
            if (ID3V1TAGIDRead[0] = ID3V1TAGID[0])
            AND (ID3V1TAGIDRead[1] = ID3V1TAGID[1])
            AND (ID3V1TAGIDRead[2] = ID3V1TAGID[2])
            then begin
                CheckStream.Seek(- 32 - 128, soEnd);
            end else begin
                CheckStream.Seek(- 32, soEnd);
            end;
            if NOT APEv2ValidTag(CheckStream) then begin
                Exit;
            end;
            if CheckStream.Read(APEv2Header, SizeOf(TAPEv2Header)) <> SizeOf(TAPEv2Header) then begin
                Exit;
            end;
            Result := APEv2Header.TagSize;
        except
            //*
        end;
    finally
        CheckStream.Seek(PreviousPosition, soBeginning);
    end;
end;


function MPEGProcessHeader(MPEGStream: TStream): TMPEGHeader;
var
    Data: Byte;
    Header: Longword;
    TmpHdr: Longword;
    Padding: Byte;
    PreviousPosition: Int64;
begin
    FillChar(Result, SizeOf(TMPEGHeader), 0);
    PreviousPosition := MPEGStream.Position;
    try
        Result.Position := MPEGStream.Position;
        MPEGStream.Read2(Data, 1);
        Header := Data;
        MPEGStream.Read2(Data, 1);
        Header := (Header SHL 8) OR Data;
        MPEGStream.Read2(Data, 1);
        Header := (Header SHL 8) OR Data;
        MPEGStream.Read2(Data, 1);
        Header := (Header SHL 8) OR Data;

        Result.Header := Header;

        TmpHdr := ((Header shl 11) shr 30);
        case TmpHdr of
            $0: Result.Version := tmpegv25;
            $1: Result.Version := tmpegvUnknown;           // Reserved
            $2: Result.Version := tmpegv2;
            $3: Result.Version := tmpegv1;
        end;
        TmpHdr := ((Header shl 13) shr 30);
        case TmpHdr of
            $0: Result.Layer := tmpeglUnknown;             // Reserved
            $1: Result.Layer := tmpegl3;
            $2: Result.Layer := tmpegl2;
            $3: Result.Layer := tmpegl1;
        end;
        TmpHdr := ((Header shl 15) shr 31);
        case TmpHdr of
            $0: Result.CRC := True;
            $1: Result.CRC := False;
        end;
        TmpHdr := ((Header shl 16) shr 28);
        if Result.Version = tmpegv1 then begin
            if Result.Layer = tmpegl3 then begin
                case TmpHdr of
                    $0: Result.BitRate := 65535;           // Free bitrate
                    $1: Result.BitRate := 32;
                    $2: Result.BitRate := 40;
                    $3: Result.BitRate := 48;
                    $4: Result.BitRate := 56;
                    $5: Result.BitRate := 64;
                    $6: Result.BitRate := 80;
                    $7: Result.BitRate := 96;
                    $8: Result.BitRate := 112;
                    $9: Result.BitRate := 128;
                    $A: Result.BitRate := 160;
                    $B: Result.BitRate := 192;
                    $C: Result.BitRate := 224;
                    $D: Result.BitRate := 256;
                    $E: Result.BitRate := 320;
                    $F: Result.BitRate := 0;               // Bad bitrate
                end;
            end;
            if Result.Layer = tmpegl2 then begin
                case TmpHdr of
                    $0: Result.BitRate := 65535;           // Free bitrate
                    $1: Result.BitRate := 32;
                    $2: Result.BitRate := 48;
                    $3: Result.BitRate := 56;
                    $4: Result.BitRate := 64;
                    $5: Result.BitRate := 80;
                    $6: Result.BitRate := 96;
                    $7: Result.BitRate := 112;
                    $8: Result.BitRate := 128;
                    $9: Result.BitRate := 160;
                    $A: Result.BitRate := 192;
                    $B: Result.BitRate := 224;
                    $C: Result.BitRate := 256;
                    $D: Result.BitRate := 320;
                    $E: Result.BitRate := 384;
                    $F: Result.BitRate := 0;               // Bad bitrate
                end;
            end;
            if Result.Layer = tmpegl1 then begin
                case TmpHdr of
                    $0: Result.BitRate := 65535;           // Free bitrate
                    $1: Result.BitRate := 32;
                    $2: Result.BitRate := 64;
                    $3: Result.BitRate := 96;
                    $4: Result.BitRate := 128;
                    $5: Result.BitRate := 160;
                    $6: Result.BitRate := 192;
                    $7: Result.BitRate := 224;
                    $8: Result.BitRate := 256;
                    $9: Result.BitRate := 288;
                    $A: Result.BitRate := 320;
                    $B: Result.BitRate := 352;
                    $C: Result.BitRate := 384;
                    $D: Result.BitRate := 416;
                    $E: Result.BitRate := 448;
                    $F: Result.BitRate := 0;               // Bad bitrate
                end;
            end;
            TmpHdr := ((Header shl 20) shr 30);
            case TmpHdr of
                $0: Result.SampleRate := 44100;
                $1: Result.SampleRate := 48000;
                $2: Result.SampleRate := 32000;
                $3: Result.SampleRate := 0;                // Reserved
            end;
        end;
        if (Result.Version = tmpegv2) OR (Result.Version = tmpegv25) then begin
            if (Result.Layer = tmpegl3) OR (Result.Layer = tmpegl2) then begin
                case TmpHdr of
                    $0: Result.BitRate := 65535;            // Free bitrate
                    $1: Result.BitRate := 8;
                    $2: Result.BitRate := 16;
                    $3: Result.BitRate := 24;
                    $4: Result.BitRate := 32;
                    $5: Result.BitRate := 40;
                    $6: Result.BitRate := 48;
                    $7: Result.BitRate := 56;
                    $8: Result.BitRate := 64;
                    $9: Result.BitRate := 80;
                    $A: Result.BitRate := 96;
                    $B: Result.BitRate := 112;
                    $C: Result.BitRate := 128;
                    $D: Result.BitRate := 144;
                    $E: Result.BitRate := 160;
                    $F: Result.BitRate := 0;                // Bad bitrate
                end;
            end;
            if Result.Layer = tmpegl1 then begin
                case TmpHdr of
                    $0: Result.BitRate := 65535;            // Free bitrate
                    $1: Result.BitRate := 32;
                    $2: Result.BitRate := 48;
                    $3: Result.BitRate := 56;
                    $4: Result.BitRate := 64;
                    $5: Result.BitRate := 80;
                    $6: Result.BitRate := 96;
                    $7: Result.BitRate := 112;
                    $8: Result.BitRate := 128;
                    $9: Result.BitRate := 144;
                    $A: Result.BitRate := 160;
                    $B: Result.BitRate := 176;
                    $C: Result.BitRate := 192;
                    $D: Result.BitRate := 224;
                    $E: Result.BitRate := 256;
                    $F: Result.BitRate := 0;                // Bad bitrate
                end;
            end;
            TmpHdr := ((Header shl 20) shr 30);
            if (Result.Version = tmpegv2) then begin
                case TmpHdr of
                    $0: Result.SampleRate := 22050;
                    $1: Result.SampleRate := 24000;
                    $2: Result.SampleRate := 16000;
                    $3: Result.SampleRate := 0;             // Reserved
                end;
            end;
            if (Result.Version = tmpegv25) then begin
                case TmpHdr of
                    $0: Result.SampleRate := 32000;
                    $1: Result.SampleRate := 16000;
                    $2: Result.SampleRate := 8000;
                    $3: Result.SampleRate := 0;             // Reserved
                end;
            end;
        end;
        TmpHdr := ((Header shl 22) shr 31);
        case TmpHdr of
            $0: Result.Padding := False;
            $1: Result.Padding := True;
        end;
        TmpHdr := ((Header shl 23) shr 31);
        case TmpHdr of
            $0: Result._Private := False;
            $1: Result._Private := True;
        end;
        TmpHdr := ((Header shl 24) shr 30);
        case TmpHdr of
            $0: begin
                Result.ChannelMode := tmpegcmStereo;
                Result.ModeExtension := tmpegmeNone;
            end;
            $1: begin
                Result.ChannelMode := tmpegcmJointStereo;
                TmpHdr := ((Header shl 26) shr 30);
                case TmpHdr of
                    $0: Result.ModeExtension := tmpegmeNone;
                    $1: Result.ModeExtension := tmpegmeIntensity;
                    $2: Result.ModeExtension := tmpegmeMS;
                    $3: Result.ModeExtension := tmpegmeIntensityMS;
                end;
            end;
            $2: begin
                Result.ChannelMode := tmpegcmDualChannel;
                Result.ModeExtension := tmpegmeNone;
            end;
            $3: begin
                Result.ChannelMode := tmpegcmMono;
                Result.ModeExtension := tmpegmeNone;
            end;
        end;
        TmpHdr := ((Header shl 28) shr 31);
        case TmpHdr of
            $0: Result.Copyrighted := False;
            $1: Result.Copyrighted := True;
        end;
        TmpHdr := ((Header shl 29) shr 31);
        case TmpHdr of
            $0: Result.Original := False;
            $1: Result.Original := True;
        end;
        TmpHdr := ((Header shl 30) shr 30);
        case TmpHdr of
            $0: Result.Emphasis := tmpegeNo;
            $1: Result.Emphasis := tmpege5015;
            $2: Result.Emphasis := tmpegeUnknown;
            $3: Result.Emphasis := tmpegeCCITJ17;
        end;
        if Result.Padding
            then Padding := 1
            else Padding := 0;
        try
            if (Result.Version = tmpegv1) then begin
                if Result.SampleRate <> 0 then begin
                    if Result.Layer = tmpegl1
                        then Result.FrameSize := Trunc(24000 * (Result.BitRate / Result.SampleRate) + Padding);
                    if (Result.Layer = tmpegl2)
                    OR (Result.Layer = tmpegl3)
                        then Result.FrameSize := Trunc(144000 * (Result.BitRate / Result.SampleRate ) + Padding);
                end else Result.FrameSize := 0;
            end;
            if (Result.Version = tmpegv2)
            OR (Result.Version = tmpegv25)
            then begin
                if Result.SampleRate <> 0 then begin
                    if Result.Layer = tmpegl1
                        then Result.FrameSize := Trunc(24000 * (Result.BitRate / Result.SampleRate) + Padding);
                    if (Result.Layer = tmpegl2)
                    OR (Result.Layer = tmpegl3)
                        then Result.FrameSize := Trunc(72000 * (Result.BitRate / Result.SampleRate) + Padding);
                end else Result.FrameSize := 0;
            end;
        except
            //* Devide by zero possible
        end;
    finally
        MPEGStream.Seek(PreviousPosition, soBeginning);
    end;
end;

{ TID3v2Frames }

procedure TID3v2Frames.Clear;
begin
    DeleteAllFrames;
end;

constructor TID3v2Frames.Create;
begin
    inherited;
    Frames := TList<TID3v2Frame>.Create;
    //Unsynchronised := False;
end;

procedure TID3v2Frames.DeleteAllFrames;
var
    i: Integer;
begin
    for i := 0 to Frames.Count - 1 do begin
        Frames[i].Free;
    end;
    Frames.Clear;
    Frames.Capacity := ID3V2LIBRARY_FRAMES_GROWBY;
end;

destructor TID3v2Frames.Destroy;
begin
    Clear;
    FreeAndNil(Frames);
    inherited;
end;

{ TID3v2FrameWithSubFrames }

constructor TID3v2FrameWithSubFrames.Create(_Parent: TID3v2Frames);
begin
    inherited;
    SubFrames := TID3v2Frames.Create;
end;

destructor TID3v2FrameWithSubFrames.Destroy;
begin
    FreeAndNil(SubFrames);
    inherited;
end;

{ TID3v2FrameCHAP }

procedure TID3v2FrameCHAP.Parse;
var
    DWordData: Cardinal;
    ValidFrameLoaded: Boolean;
begin
    Stream.Seek(0, soBeginning);
    ElementID := Stream.ReadText;
    Stream.Read(DWordData, 4);
    StartTime := ReverseBytes32(DWordData);
    Stream.Read(DWordData, 4);
    EndTime := ReverseBytes32(DWordData);
    Stream.Read(DWordData, 4);
    StartOffset := ReverseBytes32(DWordData);
    Stream.Read(DWordData, 4);
    EndOffset := ReverseBytes32(DWordData);
    //* Sub-frames loading here
    if Stream.Position < Stream.Size then begin
        repeat
            ValidFrameLoaded := SubFrames.LoadFrame(Stream);
        until NOT ValidFrameLoaded
        OR (Stream.Position >= Stream.Size);
    end;
    Stream.Seek(0, soBeginning);
end;

procedure TID3v2FrameCHAP.SetData;
var
    Bytes: TBytes;
    ByteData: Byte;
    DWordData: Cardinal;
begin
    Stream.Clear;
    Bytes := TEncoding.ANSI.GetBytes(ElementID);
    Stream.Write(Bytes[0], Length(Bytes));
    ByteData := 0;
    Stream.Write(ByteData, 1);
    DWordData := ReverseBytes32(StartTime);
    Stream.Write(DWordData, 4);
    DWordData := ReverseBytes32(EndTime);
    Stream.Write(DWordData, 4);
    DWordData := ReverseBytes32(StartOffset);
    Stream.Write(DWordData, 4);
    DWordData := ReverseBytes32(EndOffset);
    Stream.Write(DWordData, 4);
    SubFrames.WriteAllFrames(Stream);
    Stream.Seek(0, soBeginning);
end;

{ TID3v2FrameCTOC }

procedure TID3v2FrameCTOC.Parse;
var
    i: Integer;
    ByteData: Byte;
    ValidFrameLoaded: Boolean;
begin
    Stream.Seek(0, soBeginning);
    ElementID := Stream.ReadText;
    Stream.Read(ByteData, 1);
    CTOCFlags := ByteData;
    Stream.Read(ByteData, 1);
    SetLength(ChildElementIDs, ByteData);
    for i := 0 to ByteData - 1 do begin
        ChildElementIDs[i] := Stream.ReadText;
    end;
    //* Sub-frames loading here
    if Stream.Position < Stream.Size then begin
        repeat
            ValidFrameLoaded := SubFrames.LoadFrame(Stream);
        until NOT ValidFrameLoaded
        OR (Stream.Position >= Stream.Size);
    end;
    Stream.Seek(0, soBeginning);
end;

procedure TID3v2FrameCTOC.SetData;
var
    i: Integer;
    Bytes: TBytes;
    ByteData: Byte;
begin
    Stream.Clear;
    Bytes := TEncoding.ANSI.GetBytes(ElementID);
    Stream.Write(Bytes[0], Length(Bytes));
    ByteData := 0;
    Stream.Write(ByteData, 1);
    Stream.Write(CTOCFlags, 1);
    ByteData := Length(ChildElementIDs);
    Stream.Write(ByteData, 1);
    for i := Low(ChildElementIDs) to High(ChildElementIDs) do begin
        Bytes := TEncoding.ANSI.GetBytes(ChildElementIDs[i]);
        Stream.Write(Bytes[0], Length(Bytes));
        ByteData := 0;
        Stream.Write(ByteData, 1);
    end;
    SubFrames.WriteAllFrames(Stream);
    Stream.Seek(0, soBeginning);
end;

procedure TID3v2FrameCTOC.SetFlagBits(TopLevelBit, OrderedBit: Boolean);
var
    Bit: Byte;
begin
    CTOCFlags := 0;
    if TopLevelBit then begin
        Bit := 1 SHL 1;
        CTOCFlags := CTOCFlags OR Bit;
    end;
    if OrderedBit then begin
        Bit := 1;
        CTOCFlags := CTOCFlags OR Bit;
    end;
end;

function TID3v2FrameCTOC.AddChapter(ElementID: String): Boolean;
begin
    SetLength(ChildElementIDs, Length(ChildElementIDs) + 1);
    ChildElementIDs[High(ChildElementIDs)] := ElementID;
    Result := True;
end;

function TID3v2FrameCTOC.DeleteChapter(ElementID: String): Boolean;
var
    i: Integer;
begin
    Result := False;
    for i := High(ChildElementIDs) downto Low(ChildElementIDs) do begin
        if ElementID = ChildElementIDs[i] then begin
            System.Delete(ChildElementIDs, i, 1);
            Result := True;
        end;
    end;
end;

function TID3v2FrameCTOC.OrderedBit: Boolean;
var
    Bit: Byte;
begin
    {$RANGECHECKS OFF}
    Bit := CTOCFlags SHL 7;
    Bit := Bit SHR 7;
    Result := Boolean(Bit);
    {$IFDEF RANGECHECKSAREON}
        {$RANGECHECKS ON}
    {$ENDIF}
end;

function TID3v2FrameCTOC.TopLevelBit: Boolean;
var
    Bit: Byte;
begin
    {$RANGECHECKS OFF}
    Bit := CTOCFlags SHL 6;
    Bit := Bit SHR 7;
    Result := Boolean(Bit);
    {$IFDEF RANGECHECKSAREON}
        {$RANGECHECKS ON}
    {$ENDIF}
end;

Initialization

    ID3v2ID[0] := Ord('I');
    ID3v2ID[1] := Ord('D');
    ID3v2ID[2] := Ord('3');

    RIFFID[0] := Ord('R');
    RIFFID[1] := Ord('I');
    RIFFID[2] := Ord('F');
    RIFFID[3] := Ord('F');

    RF64ID[0] := Ord('R');
    RF64ID[1] := Ord('F');
    RF64ID[2] := Ord('6');
    RF64ID[3] := Ord('4');

    RIFFWAVEID[0] := Ord('W');
    RIFFWAVEID[1] := Ord('A');
    RIFFWAVEID[2] := Ord('V');
    RIFFWAVEID[3] := Ord('E');

    RIFFID3v2ID[0] := Ord('i');
    RIFFID3v2ID[1] := Ord('d');
    RIFFID3v2ID[2] := Ord('3');
    RIFFID3v2ID[3] := Ord(' ');

    RIFFID3v2ID2[0] := Ord('I');
    RIFFID3v2ID2[1] := Ord('D');
    RIFFID3v2ID2[2] := Ord('3');
    RIFFID3v2ID2[3] := Ord('2');

    AIFFID[0] := Ord('F');
    AIFFID[1] := Ord('O');
    AIFFID[2] := Ord('R');
    AIFFID[3] := Ord('M');

    AIFFChunkID[0] := Ord('A');
    AIFFChunkID[1] := Ord('I');
    AIFFChunkID[2] := Ord('F');
    AIFFChunkID[3] := Ord('F');

    AIFCChunkID[0] := Ord('A');
    AIFCChunkID[1] := Ord('I');
    AIFCChunkID[2] := Ord('F');
    AIFCChunkID[3] := Ord('C');

    AIFFID3v2ID[0] := Ord('I');
    AIFFID3v2ID[1] := Ord('D');
    AIFFID3v2ID[2] := Ord('3');
    AIFFID3v2ID[3] := Ord(' ');

    DSFID[0] := Ord('D');
    DSFID[1] := Ord('S');
    DSFID[2] := Ord('D');
    DSFID[3] := Ord(' ');

    DSFfmt_ID[0] := Ord('f');
    DSFfmt_ID[1] := Ord('m');
    DSFfmt_ID[2] := Ord('t');
    DSFfmt_ID[3] := Ord(' ');

    DFFID[0] := Ord('F');
    DFFID[1] := Ord('R');
    DFFID[2] := Ord('M');
    DFFID[3] := Ord('8');

    DFFType[0] := Ord('D');
    DFFType[1] := Ord('S');
    DFFType[2] := Ord('D');
    DFFType[3] := Ord(' ');

    DFFID3v2ID[0] := Ord('I');
    DFFID3v2ID[1] := Ord('D');
    DFFID3v2ID[2] := Ord('3');
    DFFID3v2ID[3] := Ord(' ');

end.
