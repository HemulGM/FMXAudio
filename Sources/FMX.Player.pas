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
  System.Classes, FMX.BASS, FMX.Types, FMX.Forms;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidAndroid32Arm or pidAndroid64Arm)]
  TFMXPlayer = class(TFMXPlatformPlayer)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property Volume;
    property VolumeChannel;
  published
    property Version;
    property PauseOnIncomingCalls default False;
    property OnEnd;
    property OnChangeState;
  end;

implementation

{ TFMXPlayer }

constructor TFMXPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFMXPlayer.Destroy;
begin
  inherited;
end;

end.

