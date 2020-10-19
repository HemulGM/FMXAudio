unit VCLSampleMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, FMX.Player, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    FMXPlayer1: TFMXPlayer;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  FMXPlayer1.StreamURL := Edit1.Text;
  FMXPlayer1.Play;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  FMXPlayer1.StreamURL := Edit1.Text;
  FMXPlayer1.PlayAsync;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  if not FMXPlayer1.Init(nil, Handle) then
    ShowMessage('Error');
end;

end.

