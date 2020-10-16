unit FMX.Player;

interface

uses
  FMX.Player.Shared,
  {$IFDEF ANDROID}
  FMX.Player.Android,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FMX.Player.Windows,
  {$ENDIF}
  System.Classes, FMX.BASS;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm)]
  TFMXPlayer = class(TFMXPlatformPlayer)
  public
    property LastErrorCode;
    //
    property IsPlay;
    property IsPause;
    property IsInit;
    property IsOpening;
    property State;
    //
    property Position;
    property PositionByte;
    property PositionPercent;
    property PositionTime;
    property PositionTimeLeft;
    property Size;
    property SizeByte;
    property SizeAsBuffer;
    property Bufferring;
    property BufferringPercent;
    //
    property VolumeChannel;
  published
    property Async;
    property Autoplay;
    property AutoInit;
    property StreamURL;
    property FileName;
    property Version;
    property PauseOnIncomingCalls default False;
    property OnEnd;
    property OnChangeState;
    property KeepPlayChannel;
    property Device;
    property Freq;
    property Flags;
  end;

implementation

end.

