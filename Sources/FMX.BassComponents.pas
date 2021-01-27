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
    property IsOpening;
    property IsPause;
    property IsPlay;
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
    property BassLibrary;
    property FileName;
    property KeepPlayChannel default False;
    property PauseOnIncomingCalls default False;
    property StreamURL;
    property PositionInterval default 1000;
    //Events
    property OnChangeState;
    property OnChangePosition;
    property OnEnd;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm)]
  TBassRecorder = class(TCustomBassRecorder)
  published
    property AutoInit default False;
    property BassLibrary;
    property Channels default 2;
    property OnRecording;
  end;

implementation

end.

