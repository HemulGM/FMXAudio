{
  BASS 2.4 Delphi unit
  Copyright (c) 1999-2013 Un4seen Developments Ltd.

  See the BASS.CHM file for more detailed documentation

  How to install
  --------------
  Copy BASS.PAS to the \LIB subdirectory of your Delphi path or your project dir

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

unit FMX.BASS;

interface

uses
{$IFDEF MSWINDOWS}
  Windows
{$ELSE}
  SysUtils
{$ENDIF};

type
{$IFDEF NEXTGEN}
  PByteChar = MarshaledAString;

  TByteChar = Byte;
{$ELSE}
  PByteChar = PAnsiChar;

  TByteChar = AnsiChar;
{$ENDIF}

const
  BASSVERSION = $204; // API version
  BASSVERSIONTEXT = '2.4';

  // Use these to test for error from functions that return a Cardinal or QWORD
  DW_ERROR = LongWord(-1); // -1 (Cardinal)
  QW_ERROR = Int64(-1); // -1 (QWORD)

  // Error codes returned by BASS_ErrorGetCode()
  BASS_OK = 0; // all is OK
  BASS_ERROR_MEM = 1; // memory error
  BASS_ERROR_FILEOPEN = 2; // can't open the file
  BASS_ERROR_DRIVER = 3; // can't find a free sound driver
  BASS_ERROR_BUFLOST = 4; // the sample buffer was lost
  BASS_ERROR_HANDLE = 5; // invalid handle
  BASS_ERROR_FORMAT = 6; // unsupported sample format
  BASS_ERROR_POSITION = 7; // invalid position
  BASS_ERROR_INIT = 8; // BASS_Init has not been successfully called
  BASS_ERROR_START = 9; // BASS_Start has not been successfully called
  BASS_ERROR_ALREADY = 14; // already initialized/paused/whatever
  BASS_ERROR_NOCHAN = 18; // can't get a free channel
  BASS_ERROR_ILLTYPE = 19; // an illegal type was specified
  BASS_ERROR_ILLPARAM = 20; // an illegal parameter was specified
  BASS_ERROR_NO3D = 21; // no 3D support
  BASS_ERROR_NOEAX = 22; // no EAX support
  BASS_ERROR_DEVICE = 23; // illegal device number
  BASS_ERROR_NOPLAY = 24; // not playing
  BASS_ERROR_FREQ = 25; // illegal sample rate
  BASS_ERROR_NOTFILE = 27; // the stream is not a file stream
  BASS_ERROR_NOHW = 29; // no hardware voices available
  BASS_ERROR_EMPTY = 31; // the MOD music has no sequence data
  BASS_ERROR_NONET = 32; // no internet connection could be opened
  BASS_ERROR_CREATE = 33; // couldn't create the file
  BASS_ERROR_NOFX = 34; // effects are not enabled
  BASS_ERROR_NOTAVAIL = 37; // requested data is not available
  BASS_ERROR_DECODE = 38; // the channel is a "decoding channel"
  BASS_ERROR_DX = 39; // a sufficient DirectX version is not installed
  BASS_ERROR_TIMEOUT = 40; // connection timedout
  BASS_ERROR_FILEFORM = 41; // unsupported file format
  BASS_ERROR_SPEAKER = 42; // unavailable speaker
  BASS_ERROR_VERSION = 43; // invalid BASS version (used by add-ons)
  BASS_ERROR_CODEC = 44; // codec is not available/supported
  BASS_ERROR_ENDED = 45; // the channel/file has ended
  BASS_ERROR_BUSY = 46; // the device is busy
  BASS_ERROR_UNKNOWN = -1; // some other mystery problem

  // BASS_SetConfig options
  BASS_CONFIG_BUFFER = 0;
  BASS_CONFIG_UPDATEPERIOD = 1;
  BASS_CONFIG_GVOL_SAMPLE = 4;
  BASS_CONFIG_GVOL_STREAM = 5;
  BASS_CONFIG_GVOL_MUSIC = 6;
  BASS_CONFIG_CURVE_VOL = 7;
  BASS_CONFIG_CURVE_PAN = 8;
  BASS_CONFIG_FLOATDSP = 9;
  BASS_CONFIG_3DALGORITHM = 10;
  BASS_CONFIG_NET_TIMEOUT = 11;
  BASS_CONFIG_NET_BUFFER = 12;
  BASS_CONFIG_PAUSE_NOPLAY = 13;
  BASS_CONFIG_NET_PREBUF = 15;
  BASS_CONFIG_NET_PASSIVE = 18;
  BASS_CONFIG_REC_BUFFER = 19;
  BASS_CONFIG_NET_PLAYLIST = 21;
  BASS_CONFIG_MUSIC_VIRTUAL = 22;
  BASS_CONFIG_VERIFY = 23;
  BASS_CONFIG_UPDATETHREADS = 24;
  BASS_CONFIG_DEV_BUFFER = 27;
  BASS_CONFIG_VISTA_TRUEPOS = 30;
  BASS_CONFIG_IOS_MIXAUDIO = 34;
  BASS_CONFIG_DEV_DEFAULT = 36;
  BASS_CONFIG_NET_READTIMEOUT = 37;
  BASS_CONFIG_VISTA_SPEAKERS = 38;
  BASS_CONFIG_IOS_SPEAKER = 39;
  BASS_CONFIG_HANDLES = 41;
  BASS_CONFIG_UNICODE = 42;
  BASS_CONFIG_SRC = 43;
  BASS_CONFIG_SRC_SAMPLE = 44;
  BASS_CONFIG_ASYNCFILE_BUFFER = 45;
  BASS_CONFIG_OGG_PRESCAN = 47;

  // BASS_SetConfigPtr options
  BASS_CONFIG_NET_AGENT = 16;
  BASS_CONFIG_NET_PROXY = 17;

  // BASS_Init flags
  BASS_DEVICE_8BITS = 1; // 8 bit resolution, else 16 bit
  BASS_DEVICE_MONO = 2; // mono, else stereo
  BASS_DEVICE_3D = 4; // enable 3D functionality
  BASS_DEVICE_LATENCY = $100; // calculate device latency (BASS_INFO struct)
  BASS_DEVICE_CPSPEAKERS = $400; // detect speakers via Windows control panel
  BASS_DEVICE_SPEAKERS = $800; // force enabling of speaker assignment
  BASS_DEVICE_NOSPEAKER = $1000; // ignore speaker arrangement
  BASS_DEVICE_DMIX = $2000; // use ALSA "dmix" plugin
  BASS_DEVICE_FREQ = $4000; // set device sample rate

  // DirectSound interfaces (for use with BASS_GetDSoundObject)
  BASS_OBJECT_DS = 1; // IDirectSound
  BASS_OBJECT_DS3DL = 2; // IDirectSound3DListener

  // BASS_DEVICEINFO flags
  BASS_DEVICE_ENABLED = 1;
  BASS_DEVICE_DEFAULT = 2;
  BASS_DEVICE_INIT = 4;

  // BASS_INFO flags (from DSOUND.H)
  DSCAPS_CONTINUOUSRATE = $00000010;
  // supports all sample rates between min/maxrate
  DSCAPS_EMULDRIVER = $00000020;
  // device does NOT have hardware DirectSound support
  DSCAPS_CERTIFIED = $00000040; // device driver has been certified by Microsoft
  DSCAPS_SECONDARYMONO = $00000100; // mono
  DSCAPS_SECONDARYSTEREO = $00000200; // stereo
  DSCAPS_SECONDARY8BIT = $00000400; // 8 bit
  DSCAPS_SECONDARY16BIT = $00000800; // 16 bit

  // BASS_RECORDINFO flags (from DSOUND.H)
  DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER;
  // device does NOT have hardware DirectSound recording support
  DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED;
  // device driver has been certified by Microsoft

  // defines for formats field of BASS_RECORDINFO (from MMSYSTEM.H)
  WAVE_FORMAT_1M08 = $00000001; // 11.025 kHz, Mono,   8-bit
  WAVE_FORMAT_1S08 = $00000002; // 11.025 kHz, Stereo, 8-bit
  WAVE_FORMAT_1M16 = $00000004; // 11.025 kHz, Mono,   16-bit
  WAVE_FORMAT_1S16 = $00000008; // 11.025 kHz, Stereo, 16-bit
  WAVE_FORMAT_2M08 = $00000010; // 22.05  kHz, Mono,   8-bit
  WAVE_FORMAT_2S08 = $00000020; // 22.05  kHz, Stereo, 8-bit
  WAVE_FORMAT_2M16 = $00000040; // 22.05  kHz, Mono,   16-bit
  WAVE_FORMAT_2S16 = $00000080; // 22.05  kHz, Stereo, 16-bit
  WAVE_FORMAT_4M08 = $00000100; // 44.1   kHz, Mono,   8-bit
  WAVE_FORMAT_4S08 = $00000200; // 44.1   kHz, Stereo, 8-bit
  WAVE_FORMAT_4M16 = $00000400; // 44.1   kHz, Mono,   16-bit
  WAVE_FORMAT_4S16 = $00000800; // 44.1   kHz, Stereo, 16-bit

  BASS_SAMPLE_8BITS = 1; // 8 bit
  BASS_SAMPLE_FLOAT = 256; // 32-bit floating-point
  BASS_SAMPLE_MONO = 2; // mono
  BASS_SAMPLE_LOOP = 4; // looped
  BASS_SAMPLE_3D = 8; // 3D functionality
  BASS_SAMPLE_SOFTWARE = 16; // not using hardware mixing
  BASS_SAMPLE_MUTEMAX = 32; // mute at max distance (3D only)
  BASS_SAMPLE_VAM = 64; // DX7 voice allocation & management
  BASS_SAMPLE_FX = 128; // old implementation of DX8 effects
  BASS_SAMPLE_OVER_VOL = $10000; // override lowest volume
  BASS_SAMPLE_OVER_POS = $20000; // override longest playing
  BASS_SAMPLE_OVER_DIST = $30000; // override furthest from listener (3D only)

  BASS_STREAM_PRESCAN = $20000; // enable pin-point seeking/length (MP3/MP2/MP1)

  BASS_MP3_SETPOS = BASS_STREAM_PRESCAN;

  BASS_STREAM_AUTOFREE = $40000;
  // automatically free the stream when it stop/ends
  BASS_STREAM_RESTRATE = $80000;
  // restrict the download rate of internet file streams
  BASS_STREAM_BLOCK = $100000;
  // download/play internet file stream in small blocks
  BASS_STREAM_DECODE = $200000;
  // don't play the stream, only decode (BASS_ChannelGetData)
  BASS_STREAM_STATUS = $800000;
  // give server status info (HTTP/ICY tags) in DOWNLOADPROC

  BASS_MUSIC_FLOAT = BASS_SAMPLE_FLOAT;
  BASS_MUSIC_MONO = BASS_SAMPLE_MONO;
  BASS_MUSIC_LOOP = BASS_SAMPLE_LOOP;
  BASS_MUSIC_3D = BASS_SAMPLE_3D;
  BASS_MUSIC_FX = BASS_SAMPLE_FX;
  BASS_MUSIC_AUTOFREE = BASS_STREAM_AUTOFREE;
  BASS_MUSIC_DECODE = BASS_STREAM_DECODE;
  BASS_MUSIC_PRESCAN = BASS_STREAM_PRESCAN; // calculate playback length
  BASS_MUSIC_CALCLEN = BASS_MUSIC_PRESCAN;
  BASS_MUSIC_RAMP = $200; // normal ramping
  BASS_MUSIC_RAMPS = $400; // sensitive ramping
  BASS_MUSIC_SURROUND = $800; // surround sound
  BASS_MUSIC_SURROUND2 = $1000; // surround sound (mode 2)
  BASS_MUSIC_FT2MOD = $2000; // play .MOD as FastTracker 2 does
  BASS_MUSIC_PT1MOD = $4000; // play .MOD as ProTracker 1 does
  BASS_MUSIC_NONINTER = $10000; // non-interpolated sample mixing
  BASS_MUSIC_SINCINTER = $800000; // sinc interpolated sample mixing
  BASS_MUSIC_POSRESET = $8000; // stop all notes when moving position
  BASS_MUSIC_POSRESETEX = $400000;
  // stop all notes and reset bmp/etc when moving position
  BASS_MUSIC_STOPBACK = $80000; // stop the music on a backwards jump effect
  BASS_MUSIC_NOSAMPLE = $100000; // don't load the samples

  // Speaker assignment flags
  BASS_SPEAKER_FRONT = $1000000; // front speakers
  BASS_SPEAKER_REAR = $2000000; // rear/side speakers
  BASS_SPEAKER_CENLFE = $3000000; // center & LFE speakers (5.1)
  BASS_SPEAKER_REAR2 = $4000000; // rear center speakers (7.1)
  BASS_SPEAKER_LEFT = $10000000; // modifier: left
  BASS_SPEAKER_RIGHT = $20000000; // modifier: right
  BASS_SPEAKER_FRONTLEFT = BASS_SPEAKER_FRONT or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_FRONTRIGHT = BASS_SPEAKER_FRONT or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REARLEFT = BASS_SPEAKER_REAR or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REARRIGHT = BASS_SPEAKER_REAR or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_CENTER = BASS_SPEAKER_CENLFE or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_LFE = BASS_SPEAKER_CENLFE or BASS_SPEAKER_RIGHT;
  BASS_SPEAKER_REAR2LEFT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_LEFT;
  BASS_SPEAKER_REAR2RIGHT = BASS_SPEAKER_REAR2 or BASS_SPEAKER_RIGHT;
  BASS_ASYNCFILE = $40000000;
  BASS_UNICODE = $80000000;
  BASS_RECORD_PAUSE = $8000; // start recording paused

  // DX7 voice allocation & management flags
  BASS_VAM_HARDWARE = 1;
  BASS_VAM_SOFTWARE = 2;
  BASS_VAM_TERM_TIME = 4;
  BASS_VAM_TERM_DIST = 8;
  BASS_VAM_TERM_PRIO = 16;

  // BASS_CHANNELINFO types
  BASS_CTYPE_SAMPLE = 1;
  BASS_CTYPE_RECORD = 2;
  BASS_CTYPE_STREAM = $10000;
  BASS_CTYPE_STREAM_OGG = $10002;
  BASS_CTYPE_STREAM_MP1 = $10003;
  BASS_CTYPE_STREAM_MP2 = $10004;
  BASS_CTYPE_STREAM_MP3 = $10005;
  BASS_CTYPE_STREAM_AIFF = $10006;
  BASS_CTYPE_STREAM_WAV = $40000; // WAVE flag, LOWORD=codec
  BASS_CTYPE_STREAM_WAV_PCM = $50001;
  BASS_CTYPE_STREAM_WAV_FLOAT = $50003;
  BASS_CTYPE_MUSIC_MOD = $20000;
  BASS_CTYPE_MUSIC_MTM = $20001;
  BASS_CTYPE_MUSIC_S3M = $20002;
  BASS_CTYPE_MUSIC_XM = $20003;
  BASS_CTYPE_MUSIC_IT = $20004;
  BASS_CTYPE_MUSIC_MO3 = $00100; // MO3 flag

  // 3D channel modes
  BASS_3DMODE_NORMAL = 0; // normal 3D processing
  BASS_3DMODE_RELATIVE = 1; // position is relative to the listener
  BASS_3DMODE_OFF = 2; // no 3D processing

  // software 3D mixing algorithms (used with BASS_CONFIG_3DALGORITHM)
  BASS_3DALG_DEFAULT = 0;
  BASS_3DALG_OFF = 1;
  BASS_3DALG_FULL = 2;
  BASS_3DALG_LIGHT = 3;

  // EAX environments, use with BASS_SetEAXParameters
  EAX_ENVIRONMENT_GENERIC = 0;
  EAX_ENVIRONMENT_PADDEDCELL = 1;
  EAX_ENVIRONMENT_ROOM = 2;
  EAX_ENVIRONMENT_BATHROOM = 3;
  EAX_ENVIRONMENT_LIVINGROOM = 4;
  EAX_ENVIRONMENT_STONEROOM = 5;
  EAX_ENVIRONMENT_AUDITORIUM = 6;
  EAX_ENVIRONMENT_CONCERTHALL = 7;
  EAX_ENVIRONMENT_CAVE = 8;
  EAX_ENVIRONMENT_ARENA = 9;
  EAX_ENVIRONMENT_HANGAR = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY = 11;
  EAX_ENVIRONMENT_HALLWAY = 12;
  EAX_ENVIRONMENT_STONECORRIDOR = 13;
  EAX_ENVIRONMENT_ALLEY = 14;
  EAX_ENVIRONMENT_FOREST = 15;
  EAX_ENVIRONMENT_CITY = 16;
  EAX_ENVIRONMENT_MOUNTAINS = 17;
  EAX_ENVIRONMENT_QUARRY = 18;
  EAX_ENVIRONMENT_PLAIN = 19;
  EAX_ENVIRONMENT_PARKINGLOT = 20;
  EAX_ENVIRONMENT_SEWERPIPE = 21;
  EAX_ENVIRONMENT_UNDERWATER = 22;
  EAX_ENVIRONMENT_DRUGGED = 23;
  EAX_ENVIRONMENT_DIZZY = 24;
  EAX_ENVIRONMENT_PSYCHOTIC = 25;
  // total number of environments
  EAX_ENVIRONMENT_COUNT = 26;
  BASS_STREAMPROC_END = $80000000; // end of user stream flag

  // BASS_StreamCreateFileUser file systems
  STREAMFILE_NOBUFFER = 0;
  STREAMFILE_BUFFER = 1;
  STREAMFILE_BUFFERPUSH = 2;

  // BASS_StreamPutFileData options
  BASS_FILEDATA_END = 0; // end & close the file

  // BASS_StreamGetFilePosition modes
  BASS_FILEPOS_CURRENT = 0;
  BASS_FILEPOS_DECODE = BASS_FILEPOS_CURRENT;
  BASS_FILEPOS_DOWNLOAD = 1;
  BASS_FILEPOS_END = 2;
  BASS_FILEPOS_START = 3;
  BASS_FILEPOS_CONNECTED = 4;
  BASS_FILEPOS_BUFFER = 5;

  // BASS_ChannelSetSync types
  BASS_SYNC_POS = 0;
  BASS_SYNC_END = 2;
  BASS_SYNC_META = 4;
  BASS_SYNC_SLIDE = 5;
  BASS_SYNC_STALL = 6;
  BASS_SYNC_DOWNLOAD = 7;
  BASS_SYNC_FREE = 8;
  BASS_SYNC_SETPOS = 11;
  BASS_SYNC_MUSICPOS = 10;
  BASS_SYNC_MUSICINST = 1;
  BASS_SYNC_MUSICFX = 3;
  BASS_SYNC_OGG_CHANGE = 12;
  BASS_SYNC_MIXTIME = $40000000; // FLAG: sync at mixtime, else at playtime
  BASS_SYNC_ONETIME = $80000000; // FLAG: sync only once, else continuously

  // BASS_ChannelIsActive return values
  BASS_ACTIVE_STOPPED = 0;
  BASS_ACTIVE_PLAYING = 1;
  BASS_ACTIVE_STALLED = 2;
  BASS_ACTIVE_PAUSED = 3;

  // Channel attributes
  BASS_ATTRIB_FREQ = 1;
  BASS_ATTRIB_VOL = 2;
  BASS_ATTRIB_PAN = 3;
  BASS_ATTRIB_EAXMIX = 4;
  BASS_ATTRIB_NOBUFFER = 5;
  BASS_ATTRIB_CPU = 7;
  BASS_ATTRIB_SRC = 8;
  BASS_ATTRIB_MUSIC_AMPLIFY = $100;
  BASS_ATTRIB_MUSIC_PANSEP = $101;
  BASS_ATTRIB_MUSIC_PSCALER = $102;
  BASS_ATTRIB_MUSIC_BPM = $103;
  BASS_ATTRIB_MUSIC_SPEED = $104;
  BASS_ATTRIB_MUSIC_VOL_GLOBAL = $105;
  BASS_ATTRIB_MUSIC_VOL_CHAN = $200; // + channel #
  BASS_ATTRIB_MUSIC_VOL_INST = $300; // + instrument #

  // BASS_ChannelGetData flags
  BASS_DATA_AVAILABLE = 0; // query how much data is buffered
  BASS_DATA_FLOAT = $40000000; // flag: return floating-point sample data
  BASS_DATA_FFT256 = $80000000; // 256 sample FFT
  BASS_DATA_FFT512 = $80000001; // 512 FFT
  BASS_DATA_FFT1024 = $80000002; // 1024 FFT
  BASS_DATA_FFT2048 = $80000003; // 2048 FFT
  BASS_DATA_FFT4096 = $80000004; // 4096 FFT
  BASS_DATA_FFT8192 = $80000005; // 8192 FFT
  BASS_DATA_FFT16384 = $80000006; // 16384 FFT
  BASS_DATA_FFT_INDIVIDUAL = $10;
  // FFT flag: FFT for each channel, else all combined
  BASS_DATA_FFT_NOWINDOW = $20; // FFT flag: no Hanning window
  BASS_DATA_FFT_REMOVEDC = $40; // FFT flag: pre-remove DC bias
  BASS_DATA_FFT_COMPLEX = $80; // FFT flag: return complex data

  // BASS_ChannelGetTags types : what's returned
  BASS_TAG_ID3 = 0; // ID3v1 tags : TAG_ID3 structure
  BASS_TAG_ID3V2 = 1; // ID3v2 tags : variable length block
  BASS_TAG_OGG = 2; // OGG comments : series of null-terminated UTF-8 strings
  BASS_TAG_HTTP = 3; // HTTP headers : series of null-terminated ANSI strings
  BASS_TAG_ICY = 4; // ICY headers : series of null-terminated ANSI strings
  BASS_TAG_META = 5; // ICY metadata : ANSI string
  BASS_TAG_APE = 6; // APEv2 tags : series of null-terminated UTF-8 strings
  BASS_TAG_MP4 = 7;
  // MP4/iTunes metadata : series of null-terminated UTF-8 strings
  BASS_TAG_VENDOR = 9; // OGG encoder : UTF-8 string
  BASS_TAG_LYRICS3 = 10; // Lyric3v2 tag : ASCII string
  BASS_TAG_CA_CODEC = 11; // CoreAudio codec info : TAG_CA_CODEC structure
  BASS_TAG_MF = 13;
  // Media Foundation tags : series of null-terminated UTF-8 strings
  BASS_TAG_WAVEFORMAT = 14; // WAVE format : WAVEFORMATEEX structure
  BASS_TAG_RIFF_INFO = $100;
  // RIFF "INFO" tags : series of null-terminated ANSI strings
  BASS_TAG_RIFF_BEXT = $101; // RIFF/BWF "bext" tags : TAG_BEXT structure
  BASS_TAG_RIFF_CART = $102; // RIFF/BWF "cart" tags : TAG_CART structure
  BASS_TAG_RIFF_DISP = $103; // RIFF "DISP" text tag : ANSI string
  BASS_TAG_APE_BINARY = $1000;
  // + index #, binary APEv2 tag : TAG_APE_BINARY structure
  BASS_TAG_MUSIC_NAME = $10000; // MOD music name : ANSI string
  BASS_TAG_MUSIC_MESSAGE = $10001; // MOD message : ANSI string
  BASS_TAG_MUSIC_ORDERS = $10002;
  // MOD order list : BYTE array of pattern numbers
  BASS_TAG_MUSIC_INST = $10100;
  // + instrument #, MOD instrument name : ANSI string
  BASS_TAG_MUSIC_SAMPLE = $10300; // + sample #, MOD sample name : ANSI string

  // BASS_ChannelGetLength/GetPosition/SetPosition modes
  BASS_POS_BYTE = 0; // byte position
  BASS_POS_MUSIC_ORDER = 1; // order.row position, MAKELONG(order,row)
  BASS_POS_OGG = 3; // OGG bitstream number
  BASS_POS_DECODE = $10000000; // flag: get the decoding (not playing) position
  BASS_POS_DECODETO = $20000000;
  // flag: decode to the position instead of seeking

  // BASS_RecordSetInput flags
  BASS_INPUT_OFF = $10000;
  BASS_INPUT_ON = $20000;
  BASS_INPUT_TYPE_MASK = $FF000000;
  BASS_INPUT_TYPE_UNDEF = $00000000;
  BASS_INPUT_TYPE_DIGITAL = $01000000;
  BASS_INPUT_TYPE_LINE = $02000000;
  BASS_INPUT_TYPE_MIC = $03000000;
  BASS_INPUT_TYPE_SYNTH = $04000000;
  BASS_INPUT_TYPE_CD = $05000000;
  BASS_INPUT_TYPE_PHONE = $06000000;
  BASS_INPUT_TYPE_SPEAKER = $07000000;
  BASS_INPUT_TYPE_WAVE = $08000000;
  BASS_INPUT_TYPE_AUX = $09000000;
  BASS_INPUT_TYPE_ANALOG = $0A000000;
  BASS_FX_DX8_CHORUS = 0;
  BASS_FX_DX8_COMPRESSOR = 1;
  BASS_FX_DX8_DISTORTION = 2;
  BASS_FX_DX8_ECHO = 3;
  BASS_FX_DX8_FLANGER = 4;
  BASS_FX_DX8_GARGLE = 5;
  BASS_FX_DX8_I3DL2REVERB = 6;
  BASS_FX_DX8_PARAMEQ = 7;
  BASS_FX_DX8_REVERB = 8;
  BASS_DX8_PHASE_NEG_180 = 0;
  BASS_DX8_PHASE_NEG_90 = 1;
  BASS_DX8_PHASE_ZERO = 2;
  BASS_DX8_PHASE_90 = 3;
  BASS_DX8_PHASE_180 = 4;

type
  Cardinal = LongWord;

  BOOL = LongBool;

  FLOAT = Single;

  QWORD = Int64;

  HMUSIC = Cardinal; // MOD music handle

  HSAMPLE = Cardinal; // sample handle

  HCHANNEL = Cardinal; // playing sample's channel handle

  HSTREAM = Cardinal; // sample stream handle

  HRECORD = Cardinal; // recording handle

  HSYNC = Cardinal; // synchronizer handle

  HDSP = Cardinal; // DSP handle

  HFX = Cardinal; // DX8 effect handle

  HPLUGIN = Cardinal; // Plugin handle

  // Device info structure

  BASS_DEVICEINFO = record
    name: PByteChar; // description
    driver: PByteChar; // driver
    flags: Cardinal;
  end;

  BASS_INFO = record
    flags: Cardinal; // device capabilities (DSCAPS_xxx flags)
    hwsize: Cardinal; // size of total device hardware memory
    hwfree: Cardinal; // size of free device hardware memory
    freesam: Cardinal; // number of free sample slots in the hardware
    free3d: Cardinal; // number of free 3D sample slots in the hardware
    minrate: Cardinal; // min sample rate supported by the hardware
    maxrate: Cardinal; // max sample rate supported by the hardware
    eax: BOOL;
    // device supports EAX? (always FALSE if BASS_DEVICE_3D was not used)
    minbuf: Cardinal;
    // recommended minimum buffer length in ms (requires BASS_DEVICE_LATENCY)
    dsver: Cardinal; // DirectSound version
    latency: Cardinal;
    // delay (in ms) before start of playback (requires BASS_DEVICE_LATENCY)
    initflags: Cardinal; // BASS_Init "flags" parameter
    speakers: Cardinal; // number of speakers available
    freq: Cardinal; // current output rate
  end;

  // Recording device info structure
  BASS_RECORDINFO = record
    flags: Cardinal; // device capabilities (DSCCAPS_xxx flags)
    formats: Cardinal; // supported standard formats (WAVE_FORMAT_xxx flags)
    inputs: Cardinal; // number of inputs
    singlein: BOOL; // only 1 input can be set at a time
    freq: Cardinal; // current input rate
  end;

  // Sample info structure
  BASS_SAMPLE = record
    freq: Cardinal; // default playback rate
    volume: FLOAT; // default volume (0-100)
    pan: FLOAT; // default pan (-100=left, 0=middle, 100=right)
    flags: Cardinal; // BASS_SAMPLE_xxx flags
    length: Cardinal; // length (in samples, not bytes)
    max: Cardinal; // maximum simultaneous playbacks
    origres: Cardinal; // original resolution
    chans: Cardinal; // number of channels
    mingap: Cardinal; // minimum gap (ms) between creating channels
    mode3d: Cardinal; // BASS_3DMODE_xxx mode
    mindist: FLOAT; // minimum distance
    maxdist: FLOAT; // maximum distance
    iangle: Cardinal; // angle of inside projection cone
    oangle: Cardinal; // angle of outside projection cone
    outvol: FLOAT; // delta-volume outside the projection cone
    vam: Cardinal; // voice allocation/management flags (BASS_VAM_xxx)
    priority: Cardinal; // priority (0=lowest, $ffffffff=highest)
  end;

  // Channel info structure
  BASS_CHANNELINFO = record
    freq: Cardinal; // default playback rate
    chans: Cardinal; // channels
    flags: Cardinal; // BASS_SAMPLE/STREAM/MUSIC/SPEAKER flags
    ctype: Cardinal; // type of channel
    origres: Cardinal; // original resolution
    plugin: HPLUGIN; // plugin
    sample: HSAMPLE; // sample
    {$IFDEF CPUX64}
    padding: Cardinal;
    {$ENDIF}
    filename: PChar; // filename
  end;

  BASS_PLUGINFORM = record
    ctype: Cardinal; // channel type
    {$IFDEF CPUX64}
    padding: Cardinal;
    {$ENDIF}
    name: PByteChar; // format description
    exts: PByteChar; // file extension filter (*.ext1;*.ext2;etc...)
  end;

  PBASS_PLUGINFORMS = ^TBASS_PLUGINFORMS;

  TBASS_PLUGINFORMS = array[0..maxInt div sizeOf(BASS_PLUGINFORM) - 1] of BASS_PLUGINFORM;

  BASS_PLUGININFO = record
    version: Cardinal; // version (same form as BASS_GetVersion)
    formatc: Cardinal; // number of formats
    formats: PBASS_PLUGINFORMS; // the array of formats
  end;

  PBASS_PLUGININFO = ^BASS_PLUGININFO;

  // 3D vector (for 3D positions/velocities/orientations)
  BASS_3DVECTOR = record
    x: FLOAT; // +=right, -=left
    y: FLOAT; // +=up, -=down
    z: FLOAT; // +=front, -=behind
  end;

  // User file stream callback functions
  FILECLOSEPROC = procedure(user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};

  FILELENPROC = function(user: Pointer): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};

  FILEREADPROC = function(buffer: Pointer; length: Cardinal; user: Pointer): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};

  FILESEEKPROC = function(offset: QWORD; user: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};

  BASS_FILEPROCS = record
    close: FILECLOSEPROC;
    length: FILELENPROC;
    read: FILEREADPROC;
    seek: FILESEEKPROC;
  end;

  // ID3v1 tag structure
  TAG_ID3 = record
    id: array[0..2] of TByteChar;
    title: array[0..29] of TByteChar;
    artist: array[0..29] of TByteChar;
    album: array[0..29] of TByteChar;
    year: array[0..3] of TByteChar;
    comment: array[0..29] of TByteChar;
    genre: Byte;
  end;

  // Binary APEv2 tag structure
  TAG_APE_BINARY = record
    key: PByteChar;
    data: PByteChar;
    length: Cardinal;
  end;

  // BWF "bext" tag structure
  TAG_BEXT = packed record
    Description: array[0..255] of TByteChar; // description
    Originator: array[0..31] of TByteChar; // name of the originator
    OriginatorReference: array[0..31] of TByteChar;
    // reference of the originator
    OriginationDate: array[0..9] of TByteChar;
    // date of creation (yyyy-mm-dd)
    OriginationTime: array[0..7] of TByteChar; // time of creation (hh-mm-ss)
    TimeReference: QWORD; // first sample count since midnight (little-endian)
    version: Word; // BWF version (little-endian)
    UMID: array[0..63] of Byte; // SMPTE UMID
    Reserved: array[0..189] of Byte;
    CodingHistory: array of TByteChar; // history
  end;

  BASS_DX8_CHORUS = record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    fFrequency: FLOAT;
    lWaveform: Cardinal; // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: Cardinal;    // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_COMPRESSOR = record
    fGain: FLOAT;
    fAttack: FLOAT;
    fRelease: FLOAT;
    fThreshold: FLOAT;
    fRatio: FLOAT;
    fPredelay: FLOAT;
  end;

  BASS_DX8_DISTORTION = record
    fGain: FLOAT;
    fEdge: FLOAT;
    fPostEQCenterFrequency: FLOAT;
    fPostEQBandwidth: FLOAT;
    fPreLowpassCutoff: FLOAT;
  end;

  BASS_DX8_ECHO = record
    fWetDryMix: FLOAT;
    fFeedback: FLOAT;
    fLeftDelay: FLOAT;
    fRightDelay: FLOAT;
    lPanDelay: BOOL;
  end;

  BASS_DX8_FLANGER = record
    fWetDryMix: FLOAT;
    fDepth: FLOAT;
    fFeedback: FLOAT;
    fFrequency: FLOAT;
    lWaveform: Cardinal; // 0=triangle, 1=sine
    fDelay: FLOAT;
    lPhase: Cardinal;    // BASS_DX8_PHASE_xxx
  end;

  BASS_DX8_GARGLE = record
    dwRateHz: Cardinal;    // Rate of modulation in hz
    dwWaveShape: Cardinal; // 0=triangle, 1=square
  end;

  BASS_DX8_I3DL2REVERB = record
    lRoom: LongInt;             // [-10000, 0]      default: -1000 mB
    lRoomHF: LongInt;           // [-10000, 0]      default: 0 mB
    flRoomRolloffFactor: FLOAT; // [0.0, 10.0]      default: 0.0
    flDecayTime: FLOAT;         // [0.1, 20.0]      default: 1.49s
    flDecayHFRatio: FLOAT;      // [0.1, 2.0]       default: 0.83
    lReflections: LongInt;      // [-10000, 1000]   default: -2602 mB
    flReflectionsDelay: FLOAT;  // [0.0, 0.3]       default: 0.007 s
    lReverb: LongInt;           // [-10000, 2000]   default: 200 mB
    flReverbDelay: FLOAT;       // [0.0, 0.1]       default: 0.011 s
    flDiffusion: FLOAT;         // [0.0, 100.0]     default: 100.0 %
    flDensity: FLOAT;           // [0.0, 100.0]     default: 100.0 %
    flHFReference: FLOAT;       // [20.0, 20000.0]  default: 5000.0 Hz
  end;

  BASS_DX8_PARAMEQ = record
    fCenter: FLOAT;
    fBandwidth: FLOAT;
    fGain: FLOAT;
  end;

  BASS_DX8_REVERB = record
    fInGain: FLOAT;          // [-96.0,0.0]            default: 0.0 dB
    fReverbMix: FLOAT;       // [-96.0,0.0]            default: 0.0 db
    fReverbTime: FLOAT;      // [0.001,3000.0]         default: 1000.0 ms
    fHighFreqRTRatio: FLOAT; // [0.001,0.999]          default: 0.001
  end;

  // callback function types
  STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: Cardinal; user: Pointer): Cardinal; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF};
  {
    User stream callback function. NOTE: A stream function should obviously be as
    quick as possible, other streams (and MOD musics) can't be mixed until
    it's finished.
    handle : The stream that needs writing
    buffer : Buffer to write the samples in
    length : Number of bytes to write
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Number of bytes written. Set the BASS_STREAMPROC_END flag to end
             the stream.
 }

const
  // special STREAMPROCs
  STREAMPROC_DUMMY = Pointer(0); // "dummy" stream
  STREAMPROC_PUSH = Pointer(-1); // push stream

type
  DOWNLOADPROC = procedure(buffer: Pointer; length: Cardinal; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  {
    Internet stream download callback function.
    buffer : Buffer containing the downloaded data... NULL=end of download
    length : Number of bytes in the buffer
    user   : The 'user' parameter value given when calling BASS_StreamCreateURL
 }

  SYNCPROC = procedure(handle: HSYNC; channel, data: Cardinal; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
 }

  DSPPROC = procedure(handle: HDSP; channel: Cardinal; buffer: Pointer; length: Cardinal; user: Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
 }

  RECORDPROC = function(handle: HRECORD; buffer: Pointer; length: Cardinal; user: Pointer): BOOL; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF};
  {
    Recording callback function.
    handle : The recording handle
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
 }

const
{$IFDEF MSWINDOWS}
  bassdll = 'bass.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassdll = 'libbass.so';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    bassdll = 'libbass.so';
  {$ELSE}
    bassdll = 'libbass.dylib';
  {$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  bassdll = 'libbass.so';
{$ENDIF}


var
  {$IFDEF MSWINDOWS}
  BASS_Init: function(device: LongInt; freq, flags: Cardinal; win: HWND; clsid: PGUID): BOOL; stdcall;
  BASS_GetDSoundObject: function(obj: Cardinal): Pointer; stdcall;
  BASS_SetEAXParameters: function(env: LongInt; vol, decay, damp: FLOAT): BOOL; stdcall;
  BASS_GetEAXParameters: function(var env: Cardinal; var vol, decay, damp: FLOAT): BOOL; stdcall;
  {$ELSE}
  BASS_Init: function(device: LongInt; freq, flags: Cardinal; win: Pointer; clsid: Pointer): BOOL; cdecl;
  {$ENDIF}
  BASS_SetConfig: function(option, value: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetConfig: function(option: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SetConfigPtr: function(option: Cardinal; value: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetConfigPtr: function(option: Cardinal): Pointer; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetVersion: function(): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ErrorGetCode: function(): LongInt; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetDeviceInfo: function(device: Cardinal; var info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SetDevice: function(device: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetDevice: function(): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Free: function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetInfo: function(var info: BASS_INFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Update: function(length: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetCPU: function(): FLOAT; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Start: function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Stop: function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Pause: function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SetVolume: function(volume: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_GetVolume: function(): FLOAT; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_PluginLoad: function(filename: PChar; flags: Cardinal): HPLUGIN; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_PluginFree: function(handle: HPLUGIN): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_PluginGetInfo: function(handle: HPLUGIN): PBASS_PLUGININFO; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Set3DFactors: function(distf, rollf, doppf: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Get3DFactors: function(var distf, rollf, doppf: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Set3DPosition: function(var pos, vel, front, top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Get3DPosition: function(var pos, vel, front, top: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_Apply3D: procedure(); {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_MusicLoad: function(mem: BOOL; f: Pointer; offset: QWORD; length, flags, freq: Cardinal): HMUSIC; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_MusicFree: function(handle: HMUSIC): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleLoad: function(mem: BOOL; f: Pointer; offset: QWORD; length, max, flags: Cardinal): HSAMPLE; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleCreate: function(length, freq, chans, max, flags: Cardinal): HSAMPLE; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleFree: function(handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleSetData: function(handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleGetData: function(handle: HSAMPLE; buffer: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleGetInfo: function(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleSetInfo: function(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleGetChannel: function(handle: HSAMPLE; onlynew: BOOL): HCHANNEL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleGetChannels: function(handle: HSAMPLE; channels: Pointer): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_SampleStop: function(handle: HSAMPLE): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamCreate: function(freq, chans, flags: Cardinal; proc: STREAMPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamCreateFile: function(mem: BOOL; f: Pointer; offset, length: QWORD; flags: Cardinal): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamCreateURL: function(url: Pointer; offset: Cardinal; flags: Cardinal; proc: DOWNLOADPROC; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamCreateFileUser: function(system, flags: Cardinal; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamFree: function(handle: HSTREAM): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamGetFilePosition: function(handle: HSTREAM; mode: Cardinal): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamPutData: function(handle: HSTREAM; buffer: Pointer; length: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_StreamPutFileData: function(handle: HSTREAM; buffer: Pointer; length: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordGetDeviceInfo: function(device: Cardinal; var info: BASS_DEVICEINFO): BOOL; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};
  BASS_RecordInit: function(device: LongInt): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordSetDevice: function(device: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordGetDevice: function(): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordFree: function(): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordGetInfo: function(var info: BASS_RECORDINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordGetInputName: function(input: LongInt): PByteChar; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordSetInput: function(input: LongInt; flags: Cardinal; volume: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordGetInput: function(input: LongInt; var volume: FLOAT): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_RecordStart: function(freq, chans, flags: Cardinal; proc: RECORDPROC; user: Pointer): HRECORD; {$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelBytes2Seconds: function(handle: Cardinal; pos: QWORD): Double; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSeconds2Bytes: function(handle: Cardinal; pos: Double): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetDevice: function(handle: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetDevice: function(handle, device: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelIsActive: function(handle: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetInfo: function(handle: Cardinal; var info: BASS_CHANNELINFO): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetTags: function(handle: HSTREAM; tags: Cardinal): PByteChar; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelFlags: function(handle, flags, mask: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelUpdate: function(handle, length: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelLock: function(handle: Cardinal; lock: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelPlay: function(handle: Cardinal; restart: BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelStop: function(handle: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelPause: function(handle: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetAttribute: function(handle, attrib: Cardinal; value: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetAttribute: function(handle, attrib: Cardinal; var value: FLOAT): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSlideAttribute: function(handle, attrib: Cardinal; value: FLOAT; time: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelIsSliding: function(handle, attrib: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSet3DAttributes: function(handle: Cardinal; mode: LongInt; min, max: FLOAT; iangle, oangle, outvol: LongInt): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGet3DAttributes: function(handle: Cardinal; var mode: Cardinal; var min, max: FLOAT; var iangle, oangle, outvol: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSet3DPosition: function(handle: Cardinal; var pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGet3DPosition: function(handle: Cardinal; var pos, orient, vel: BASS_3DVECTOR): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetLength: function(handle, mode: Cardinal): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetPosition: function(handle: Cardinal; pos: QWORD; mode: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall {$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetPosition: function(handle, mode: Cardinal): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetLevel: function(handle: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelGetData: function(handle: Cardinal; buffer: Pointer; length: Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetSync: function(handle: Cardinal; type_: Cardinal; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelRemoveSync: function(handle: Cardinal; sync: HSYNC): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetDSP: function(handle: Cardinal; proc: DSPPROC; user: Pointer; priority: LongInt): HDSP; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelRemoveDSP: function(handle: Cardinal; dsp: HDSP): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetLink: function(handle, chan: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelRemoveLink: function(handle, chan: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelSetFX: function(handle, type_: Cardinal; priority: LongInt): HFX; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_ChannelRemoveFX: function(handle: Cardinal; fx: HFX): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_FXSetParameters: function(handle: HFX; par: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_FXGetParameters: function(handle: HFX; par: Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};
  BASS_FXReset: function(handle: HFX): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE} cdecl{$ENDIF};

var
  FBassDLL: THandle;

function BASS_Folder: string;

function BASS_Lib: string;

function BASS_Available: Boolean;

function BASS_SPEAKER_N(n: Cardinal): Cardinal;

{$IFDEF MSWINDOWS}
function BASS_SetEAXPreset(env: LongInt): BOOL;
{$ENDIF}

implementation

{$IFNDEF MSWINDOWS}
uses
  System.IOUtils;
{$ENDIF}

function BASS_SPEAKER_N(n: Cardinal): Cardinal;
begin
  Result := n shl 24;
end;

function BASS_Folder: string;
begin
  {$IFDEF MSWINDOWS}
  Result := '';
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath);
  {$ENDIF}
end;

function BASS_Available: Boolean;
begin
  Result := FBassDll <> 0;
end;

function BASS_Lib: string;
begin
  Result := BASS_FOLDER + bassdll;
end;

procedure LoadBassDLL;
begin
  FBassDLL := LoadLibrary(PChar(BASS_Lib));
  if FBassDLL = 0 then
    Exit;

  BASS_SetConfig := GetProcAddress(FBassDLL, 'BASS_SetConfig');
  BASS_GetConfig := GetProcAddress(FBassDLL, 'BASS_GetConfig');
  BASS_SetConfigPtr := GetProcAddress(FBassDLL, 'BASS_SetConfigPtr');
  BASS_GetConfigPtr := GetProcAddress(FBassDLL, 'BASS_GetConfigPtr');
  BASS_GetVersion := GetProcAddress(FBassDLL, 'BASS_GetVersion');
  BASS_ErrorGetCode := GetProcAddress(FBassDLL, 'BASS_ErrorGetCode');
  BASS_GetDeviceInfo := GetProcAddress(FBassDLL, 'BASS_GetDeviceInfo');
  BASS_Init := GetProcAddress(FBassDLL, 'BASS_Init');
  BASS_SetDevice := GetProcAddress(FBassDLL, 'BASS_SetDevice');
  BASS_GetDevice := GetProcAddress(FBassDLL, 'BASS_GetDevice');
  BASS_Free := GetProcAddress(FBassDLL, 'BASS_Free');
{$IFDEF MSWINDOWS}
  BASS_GetDSoundObject := GetProcAddress(FBassDLL, 'BASS_GetDSoundObject');
{$ENDIF}
  BASS_GetInfo := GetProcAddress(FBassDLL, 'BASS_GetInfo');
  BASS_Update := GetProcAddress(FBassDLL, 'BASS_Update');
  BASS_GetCPU := GetProcAddress(FBassDLL, 'BASS_GetCPU');
  BASS_Start := GetProcAddress(FBassDLL, 'BASS_Start');
  BASS_Stop := GetProcAddress(FBassDLL, 'BASS_Stop');
  BASS_Pause := GetProcAddress(FBassDLL, 'BASS_Pause');
  BASS_SetVolume := GetProcAddress(FBassDLL, 'BASS_SetVolume');
  BASS_GetVolume := GetProcAddress(FBassDLL, 'BASS_GetVolume');

  BASS_PluginLoad := GetProcAddress(FBassDLL, 'BASS_PluginLoad');
  BASS_PluginFree := GetProcAddress(FBassDLL, 'BASS_PluginFree');
  BASS_PluginGetInfo := GetProcAddress(FBassDLL, 'BASS_PluginGetInfo');
  BASS_Set3DFactors := GetProcAddress(FBassDLL, 'BASS_Set3DFactors');
  BASS_Get3DFactors := GetProcAddress(FBassDLL, 'BASS_Get3DFactors');
  BASS_Set3DPosition := GetProcAddress(FBassDLL, 'BASS_Set3DPosition');
  BASS_Get3DPosition := GetProcAddress(FBassDLL, 'BASS_Get3DPosition');
  BASS_Apply3D := GetProcAddress(FBassDLL, 'BASS_Apply3D');
{$IFDEF MSWINDOWS}
  BASS_SetEAXParameters := GetProcAddress(FBassDLL, 'BASS_SetEAXParameters');
  BASS_GetEAXParameters := GetProcAddress(FBassDLL, 'BASS_GetEAXParameters');
{$ENDIF}
  BASS_MusicLoad := GetProcAddress(FBassDLL, 'BASS_MusicLoad');
  BASS_MusicFree := GetProcAddress(FBassDLL, 'BASS_MusicFree');
  BASS_SampleLoad := GetProcAddress(FBassDLL, 'BASS_SampleLoad');
  BASS_SampleCreate := GetProcAddress(FBassDLL, 'BASS_SampleCreate');
  BASS_SampleFree := GetProcAddress(FBassDLL, 'BASS_SampleFree');
  BASS_SampleSetData := GetProcAddress(FBassDLL, 'BASS_SampleSetData');
  BASS_SampleGetData := GetProcAddress(FBassDLL, 'BASS_SampleGetData');
  BASS_SampleGetInfo := GetProcAddress(FBassDLL, 'BASS_SampleGetInfo');
  BASS_SampleSetInfo := GetProcAddress(FBassDLL, 'BASS_SampleSetInfo');
  BASS_SampleGetChannel := GetProcAddress(FBassDLL, 'BASS_SampleGetChannel');
  BASS_SampleGetChannels := GetProcAddress(FBassDLL, 'BASS_SampleGetChannels');
  BASS_SampleStop := GetProcAddress(FBassDLL, 'BASS_SampleStop');
  BASS_StreamCreate := GetProcAddress(FBassDLL, 'BASS_StreamCreate');
  BASS_StreamCreateFile := GetProcAddress(FBassDLL, 'BASS_StreamCreateFile');
  BASS_StreamCreateURL := GetProcAddress(FBassDLL, 'BASS_StreamCreateURL');
  BASS_StreamCreateFileUser := GetProcAddress(FBassDLL, 'BASS_StreamCreateFileUser');
  BASS_StreamFree := GetProcAddress(FBassDLL, 'BASS_StreamFree');
  BASS_StreamGetFilePosition := GetProcAddress(FBassDLL, 'BASS_StreamGetFilePosition');
  BASS_StreamPutData := GetProcAddress(FBassDLL, 'BASS_StreamPutData');
  BASS_StreamPutFileData := GetProcAddress(FBassDLL, 'BASS_StreamPutFileData');
  BASS_RecordGetDeviceInfo := GetProcAddress(FBassDLL, 'BASS_RecordGetDeviceInfo');
  BASS_RecordInit := GetProcAddress(FBassDLL, 'BASS_RecordInit');
  BASS_RecordSetDevice := GetProcAddress(FBassDLL, 'BASS_RecordSetDevice');
  BASS_RecordGetDevice := GetProcAddress(FBassDLL, 'BASS_RecordGetDevice');
  BASS_RecordFree := GetProcAddress(FBassDLL, 'BASS_RecordFree');
  BASS_RecordGetInfo := GetProcAddress(FBassDLL, 'BASS_RecordGetInfo');
  BASS_RecordGetInputName := GetProcAddress(FBassDLL, 'BASS_RecordGetInputName');
  BASS_RecordSetInput := GetProcAddress(FBassDLL, 'BASS_RecordSetInput');
  BASS_RecordGetInput := GetProcAddress(FBassDLL, 'BASS_RecordGetInput');
  BASS_RecordStart := GetProcAddress(FBassDLL, 'BASS_RecordStart');
  BASS_ChannelBytes2Seconds := GetProcAddress(FBassDLL, 'BASS_ChannelBytes2Seconds');
  BASS_ChannelSeconds2Bytes := GetProcAddress(FBassDLL, 'BASS_ChannelSeconds2Bytes');
  BASS_ChannelGetDevice := GetProcAddress(FBassDLL, 'BASS_ChannelGetDevice');
  BASS_ChannelSetDevice := GetProcAddress(FBassDLL, 'BASS_ChannelSetDevice');
  BASS_ChannelIsActive := GetProcAddress(FBassDLL, 'BASS_ChannelIsActive');
  BASS_ChannelGetInfo := GetProcAddress(FBassDLL, 'BASS_ChannelGetInfo');
  BASS_ChannelGetTags := GetProcAddress(FBassDLL, 'BASS_ChannelGetTags');
  BASS_ChannelFlags := GetProcAddress(FBassDLL, 'BASS_ChannelFlags');
  BASS_ChannelUpdate := GetProcAddress(FBassDLL, 'BASS_ChannelUpdate');
  BASS_ChannelLock := GetProcAddress(FBassDLL, 'BASS_ChannelLock');
  BASS_ChannelPlay := GetProcAddress(FBassDLL, 'BASS_ChannelPlay');
  BASS_ChannelStop := GetProcAddress(FBassDLL, 'BASS_ChannelStop');
  BASS_ChannelPause := GetProcAddress(FBassDLL, 'BASS_ChannelPause');
  BASS_ChannelSetAttribute := GetProcAddress(FBassDLL, 'BASS_ChannelSetAttribute');
  BASS_ChannelGetAttribute := GetProcAddress(FBassDLL, 'BASS_ChannelGetAttribute');
  BASS_ChannelSlideAttribute := GetProcAddress(FBassDLL, 'BASS_ChannelSlideAttribute');
  BASS_ChannelIsSliding := GetProcAddress(FBassDLL, 'BASS_ChannelIsSliding');
  BASS_ChannelSet3DAttributes := GetProcAddress(FBassDLL, 'BASS_ChannelSet3DAttributes');
  BASS_ChannelGet3DAttributes := GetProcAddress(FBassDLL, 'BASS_ChannelGet3DAttributes');
  BASS_ChannelSet3DPosition := GetProcAddress(FBassDLL, 'BASS_ChannelSet3DPosition');
  BASS_ChannelGet3DPosition := GetProcAddress(FBassDLL, 'BASS_ChannelGet3DPosition');
  BASS_ChannelGetLength := GetProcAddress(FBassDLL, 'BASS_ChannelGetLength');
  BASS_ChannelSetPosition := GetProcAddress(FBassDLL, 'BASS_ChannelSetPosition');
  BASS_ChannelGetPosition := GetProcAddress(FBassDLL, 'BASS_ChannelGetPosition');
  BASS_ChannelGetLevel := GetProcAddress(FBassDLL, 'BASS_ChannelGetLevel');
  BASS_ChannelGetData := GetProcAddress(FBassDLL, 'BASS_ChannelGetData');
  BASS_ChannelSetSync := GetProcAddress(FBassDLL, 'BASS_ChannelSetSync');
  BASS_ChannelRemoveSync := GetProcAddress(FBassDLL, 'BASS_ChannelRemoveSync');
  BASS_ChannelSetDSP := GetProcAddress(FBassDLL, 'BASS_ChannelSetDSP');
  BASS_ChannelSetLink := GetProcAddress(FBassDLL, 'BASS_ChannelSetLink');
  BASS_ChannelRemoveLink := GetProcAddress(FBassDLL, 'BASS_ChannelRemoveLink');
  BASS_ChannelSetFX := GetProcAddress(FBassDLL, 'BASS_ChannelSetFX');
  BASS_ChannelRemoveFX := GetProcAddress(FBassDLL, 'BASS_ChannelRemoveFX');
  BASS_FXSetParameters := GetProcAddress(FBassDLL, 'BASS_FXSetParameters');
  BASS_FXGetParameters := GetProcAddress(FBassDLL, 'BASS_FXGetParameters');
  BASS_FXReset := GetProcAddress(FBassDLL, 'BASS_FXReset');
end;

procedure UnloadBassDLL;
begin
  BASS_Free;
  FreeLibrary(FBassDLL);
end;

{$IFDEF MSWINDOWS}
function BASS_SetEAXPreset(env: LongInt): Bool;
begin
  case (env) of
    EAX_ENVIRONMENT_GENERIC:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_GENERIC, 0.5, 1.493, 0.5);
    EAX_ENVIRONMENT_PADDEDCELL:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PADDEDCELL, 0.25, 0.1, 0);
    EAX_ENVIRONMENT_ROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ROOM, 0.417, 0.4, 0.666);
    EAX_ENVIRONMENT_BATHROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_BATHROOM, 0.653, 1.499, 0.166);
    EAX_ENVIRONMENT_LIVINGROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_LIVINGROOM, 0.208, 0.478, 0);
    EAX_ENVIRONMENT_STONEROOM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONEROOM, 0.5, 2.309, 0.888);
    EAX_ENVIRONMENT_AUDITORIUM:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_AUDITORIUM, 0.403, 4.279, 0.5);
    EAX_ENVIRONMENT_CONCERTHALL:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CONCERTHALL, 0.5, 3.961, 0.5);
    EAX_ENVIRONMENT_CAVE:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CAVE, 0.5, 2.886, 1.304);
    EAX_ENVIRONMENT_ARENA:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ARENA, 0.361, 7.284, 0.332);
    EAX_ENVIRONMENT_HANGAR:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HANGAR, 0.5, 10.0, 0.3);
    EAX_ENVIRONMENT_CARPETEDHALLWAY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259, 2.0);
    EAX_ENVIRONMENT_HALLWAY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_HALLWAY, 0.361, 1.493, 0);
    EAX_ENVIRONMENT_STONECORRIDOR:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697, 0.638);
    EAX_ENVIRONMENT_ALLEY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_ALLEY, 0.25, 1.752, 0.776);
    EAX_ENVIRONMENT_FOREST:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_FOREST, 0.111, 3.145, 0.472);
    EAX_ENVIRONMENT_CITY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_CITY, 0.111, 2.767, 0.224);
    EAX_ENVIRONMENT_MOUNTAINS:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_MOUNTAINS, 0.194, 7.841, 0.472);
    EAX_ENVIRONMENT_QUARRY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_QUARRY, 1, 1.499, 0.5);
    EAX_ENVIRONMENT_PLAIN:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PLAIN, 0.097, 2.767, 0.224);
    EAX_ENVIRONMENT_PARKINGLOT:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PARKINGLOT, 0.208, 1.652, 1.5);
    EAX_ENVIRONMENT_SEWERPIPE:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_SEWERPIPE, 0.652, 2.886, 0.25);
    EAX_ENVIRONMENT_UNDERWATER:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_UNDERWATER, 1, 1.499, 0);
    EAX_ENVIRONMENT_DRUGGED:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DRUGGED, 0.875, 8.392, 1.388);
    EAX_ENVIRONMENT_DIZZY:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_DIZZY, 0.139, 17.234, 0.666);
    EAX_ENVIRONMENT_PSYCHOTIC:
      Result := BASS_SetEAXParameters(EAX_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563, 0.806);
  else
    Result := False;
  end;
end;
{$ENDIF}

initialization
  LoadBassDLL;

finalization
  UnloadBassDLL;

end.

