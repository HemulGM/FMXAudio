unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Player, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    FMXPlayer1: TFMXPlayer;
    Edit1: TEdit;
    ButtonPlay: TButton;
    ButtonAsyncPlay: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonAsyncPlayClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  FMX.Platform.Win;

{$R *.fmx}

procedure TForm1.ButtonAsyncPlayClick(Sender: TObject);
begin
  FMXPlayer1.StreamURL := Edit1.Text;
  //Start playing async
  FMXPlayer1.PlayAsync(
    procedure(const Success: Boolean)
    begin
      if not Success then
        ShowMessage('not ok ' + FMXPlayer1.LastErrorCode.ToString);
    end);
end;

procedure TForm1.ButtonPlayClick(Sender: TObject);
begin
  //For file open use
  //FMXPlayer1.FileName := Edit1.Text;
  //For stream open use
  FMXPlayer1.StreamURL := Edit1.Text;
  //Start playing sync
  if not FMXPlayer1.Play then
    ShowMessage('not ok ' + FMXPlayer1.LastErrorCode.ToString);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not FMXPlayer1.Init(Handle, WindowHandleToPlatform(Handle).Wnd) then
    ShowMessage('Error ' + FMXPlayer1.LastErrorCode.ToString);
end;

end.

