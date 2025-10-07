unit FMX.BassComponents;

interface

uses
  FMX.Player, FMX.Recorder, System.Classes;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroidArm32 or pidAndroidArm64)]
  TFMXPlayer = class(TFMXCustomPlayer)
  public
    property Bufferring;
    property BufferringPercent;
    property IsOpening;
    property IsPause;
    property IsPlay;
    property PositionByte;
    property PositionPercent;
    property PositionTime;
    property PositionTimeLeft;
    property SizeAsBuffer;
    property SizeByte;
    property State;
    property VolumeChannel;
  published //Props
    property AutoInit default False;
    property AutoFree default True;
    property BassLibrary;
    property FileName;
    property PauseOnIncomingCalls default False;
    property StreamURL;
  published //Events
    property OnChangeState;
    property OnChangePosition;
    property OnEnd;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroidArm32 or pidAndroidArm64)]
  TBassRecorder = class(TCustomBassRecorder)
  published
    property AutoInit default False;
    property BassLibrary;
    property Channels default 2;
    property OnRecording;
  end;

implementation

end.

