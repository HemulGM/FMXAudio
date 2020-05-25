unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Player, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Player.Shared,
  FMX.Player.Windows;

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

{$R *.fmx}

procedure TForm1.ButtonAsyncPlayClick(Sender: TObject);
begin
  FMXPlayer1.StreamURL := Edit1.Text;
  //Start playing async
  TThread.CreateAnonymousThread(
    procedure
    begin
      if not FMXPlayer1.Play then
      begin
        TThread.ForceQueue(nil,
          procedure
          begin
            ShowMessage('not ok ' + FMXPlayer1.LastErrorCode.ToString);
          end);
      end;
    end).Start;
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
  if not FMXPlayer1.Init(Handle) then
    ShowMessage('Error ' + FMXPlayer1.LastErrorCode.ToString);
end;

end.

