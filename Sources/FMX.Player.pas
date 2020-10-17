unit FMX.Player;

interface

uses
  FMX.Player.Shared, System.Classes, FMX.BASS;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm)]
  TFMXPlayer = class(TFMXCustomPlayer)
  public
    property Bufferring;
    property BufferringPercent;
    property IsInit;
    property IsOpening;
    property IsPause;
    property IsPlay;
    property LastErrorCode;
    property Position;
    property PositionByte;
    property PositionPercent;
    property PositionTime;
    property PositionTimeLeft;
    property Size;
    property SizeAsBuffer;
    property SizeByte;
    property State;
    property VolumeChannel;
  published
    //Props
    property Async default False;
    property AutoInit default False;
    property Autoplay default False;
    property Device default -1;
    property FileName;
    property Flags default 0;
    property Freq default 44100;
    property KeepPlayChannel default False;
    property PauseOnIncomingCalls default False;
    property StreamURL;
    property Version;
    //Events
    property OnChangeState;
    property OnEnd;
  end;

implementation

end.

