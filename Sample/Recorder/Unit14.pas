unit Unit14;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.BassComponents, FMX.Recorder, FMX.Controls.Presentation, FMX.StdCtrls,
  System.Generics.Collections, FMX.Edit, FMX.ListBox, FMX.BASS.Classes, FMX.Player;

type
  TForm14 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditFile: TEdit;
    ButtonOpenFile: TEditButton;
    SaveDialog: TSaveDialog;
    ComboBoxChannels: TComboBox;
    LabelTime: TLabel;
    BassRecorder: TBassRecorder;
    FMXPlayer1: TFMXPlayer;
    ButtonPlay: TButton;
    ButtonStopPlay: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenFileClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure BassRecorderRecording(Sender: TObject; Channel: Cardinal; const CurrentTime: Cardinal);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopPlayClick(Sender: TObject);
  private
    procedure FillChannelList(List: TList<TInputDevice>);
  public
    { Public declarations }
  end;

var
  Form14: TForm14;

implementation

uses
  System.IOUtils;

{$R *.fmx}

procedure TForm14.BassRecorderRecording(Sender: TObject; Channel: Cardinal; const CurrentTime: Cardinal);
begin
  LabelTime.Text := Format(' %d:%.2d', [CurrentTime div 60, CurrentTime mod 60]);
end;

procedure TForm14.ButtonPlayClick(Sender: TObject);
begin
  FMXPlayer1.FileName := EditFile.Text;
  FMXPlayer1.Play;
end;

procedure TForm14.ButtonStopPlayClick(Sender: TObject);
begin
  FMXPlayer1.Stop;
  FMXPlayer1.UnloadChannel;
end;

procedure TForm14.ButtonOpenFileClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    EditFile.Text := SaveDialog.FileName;
end;

procedure TForm14.ButtonStartClick(Sender: TObject);
var
  FS: TFileStream;
begin
  {$IFDEF ANDROID}
  FS := TFileStream.Create(TPath.Combine(TPath.GetSharedDocumentsPath, EditFile.Text), fmCreate or fmOpenWrite);
  ShowMessage(TPath.Combine(TPath.GetSharedDocumentsPath, EditFile.Text));
  {$ELSE}
  FS := TFileStream.Create(EditFile.Text, fmCreate or fmOpenWrite);
  {$ENDIF}
  BassRecorder.Start(FS);
end;

procedure TForm14.ButtonStopClick(Sender: TObject);
begin
  BassRecorder.Stop;
end;

procedure TForm14.FillChannelList(List: TList<TInputDevice>);
var
  Item: TListBoxItem;
  Device: TInputDevice;
  ActiveItem: Integer;
begin
  ActiveItem := -1;
  ComboBoxChannels.BeginUpdate;
  try
    ComboBoxChannels.Clear;
    for Device in List do
    begin
      Item := TListBoxItem.Create(ComboBoxChannels);
      Item.Text := Device.Name;
      ComboBoxChannels.AddObject(Item);
      if Device.IsActive then
        ActiveItem := Item.Index;
    end;
    ComboBoxChannels.ItemIndex := ActiveItem;
  finally
    ComboBoxChannels.EndUpdate;
    List.Free;
  end;
end;

procedure TForm14.FormCreate(Sender: TObject);
begin
  if not BassRecorder.BassLibrary.IsInit then
    ShowMessage('Проблемы с инициализацией BASS')
  else
    FillChannelList(BassRecorder.GetInputDevices);
end;

end.

