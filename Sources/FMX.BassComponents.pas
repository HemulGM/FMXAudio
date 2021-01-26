unit FMX.BassComponents;

interface

uses
  FMX.Player, FMX.Recorder, System.Classes;

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
    property Plugins;
    property PositionInterval default 1000;
    property UseDefaultDevice default True;
    //Events
    property OnChangeState;
    property OnChangePosition;
    property OnEnd;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm)]
  TBassRecorder = class(TCustomBassRecorder)
  published
    property OnRecording;
  end;

implementation

end.

