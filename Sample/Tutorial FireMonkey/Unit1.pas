//********************************************************************************************************************************
//*                                                                                                                              *
//*     ID3v2 Library 2.0 © 3delite 2010-2022                                                                                    *
//*     See ID3v2 Library 2.0 ReadMe.txt for details.                                                                            *
//*                                                                                                                              *
//* Licenses available for this component:                                                                                       *
//* Freeware License: €25                                                                                                        *
//*     http://www.shareit.com/product.html?productid=300953211                                                                  *
//* Shareware License: €50                                                                                                       *
//*     http://www.shareit.com/product.html?productid=300294127                                                                  *
//* Commercial License: €250                                                                                                     *
//* 	http://www.shareit.com/product.html?productid=300184612                                                                  *
//*                                                                                                                              *
//* Home page:                                                                                                                   *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/id3v2library.html                                         *
//*                                                                                                                              *
//* This component is also available as a part of Tags Library:                                                                  *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/TagsLibrary.html                                          *
//*                                                                                                                              *
//* There is an APEv2 Library available at:                                                                                      *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/APEv2Library.html                                         *
//*                                                                                                                              *
//* and an MP4 Tag Library available at:                                                                                         *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MP4TagLibrary.html                                        *
//*                                                                                                                              *
//* and an Ogg Vorbis and Opus Tag Library available at:                                                                         *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/OpusTagLibrary.html                                       *
//*                                                                                                                              *
//* and a Flac Tag Library available at:                                                                                         *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/FlacTagLibrary.html                                       *
//*                                                                                                                              *
//* and a WMA Tag Library available at:                                                                                          *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WMATagLibrary.html                                        *
//*                                                                                                                              *
//* and a WAV Tag Library available at:                                                                                          *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/WAVTagLibrary.html                                        *
//*                                                                                                                              *
//* an MKV Tag Library available at:                                                                                             *
//*                                                                                                                              *
//*     https://www.3delite.hu/Object%20Pascal%20Developer%20Resources/MKVTagLibrary.html                                        *
//*                                                                                                                              *
//* For other Delphi components see the home page:                                                                               *
//*                                                                                                                              *
//*     https://www.3delite.hu/                                                                                                  *
//*                                                                                                                              *
//* If you have any questions or enquiries please mail: 3delite@3delite.hu                                                       *
//*                                                                                                                              *
//* Good coding! :)                                                                                                              *
//* 3delite                                                                                                                      *
//********************************************************************************************************************************

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Forms3D, FMX.Dialogs, FMX.Types3D, FMX.Layers3D,
  FMX.Edit, FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.TabControl, ID3v1Library, ID3v2Library, System.Math.Vectors, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Controls.Presentation, FMX.Controls3D, FMX.Memo.Types;

type
  TForm1 = class(TForm3D)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Layer3D1: TLayer3D;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    ComboBox2: TComboBox;
    TabItem2: TTabItem;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    GroupBox4: TGroupBox;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Edit15: TEdit;
    Edit16: TEdit;
    GroupBox5: TGroupBox;
    Edit17: TEdit;
    Edit18: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    Memo1: TMemo;
    GroupBox6: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    Edit19: TEdit;
    Edit20: TEdit;
    Memo2: TMemo;
    GroupBox7: TGroupBox;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    ImageControl1: TImageControl;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Edit21: TEdit;
    Button16: TButton;
    Button17: TButton;
    GroupBox8: TGroupBox;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    ComboBox1: TComboBox;
    Memo3: TMemo;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label45: TLabel;
    LabelPlaytime: TLabel;
    Label47: TLabel;
    LabelBitRate: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ClearDatav1;
    procedure ClearDatav2;
    function LoadAPIC(Index: Integer): Boolean;
    procedure SetFrameAttributes;
    procedure SetFrameCommandButtons;
    procedure PopulateGenres(ComboBox: TComboBox);
  end;

var
    Form1: TForm1;
    ID3v1Tag: TID3v1Tag = nil;
    ID3v2Tag: TID3v2Tag = nil;
    CurrentAPICIndex: Integer = - 1;

implementation

{$R *.fmx}

Uses
    FMX.Ani,
    FMX.DialogService;

function mSec2Time(mSec: Int64): String;
var
  tHours: String;
  tMinutes: String;
  tSecs: String;
  tmSecs: String;
  Seconds: Int64;
begin
    if mSec = 4294967295
        then mSec := 0;
    Seconds := mSec div 1000;
    tHours := IntToStr(Seconds div 3600);
    if Length(tHours) = 1
        then tHours := '0' + tHours;
    tMinutes := IntToStr(((Seconds - (StrToInt(tHours) * 3600)) div 60));
    if Length(tMinutes) = 1
        then tMinutes := '0' + tMinutes;
    tSecs := IntToStr(Seconds - (StrToInt(tHours) * 3600 + StrToInt(tMinutes) * 60));
    if Length(tSecs) = 1
        then tSecs := '0' + tSecs;
    tmSecs := IntToStr(mSec - ((StrToInt(tHours) * 3600 + StrToInt(tMinutes) * 60) + StrToInt(tSecs)) * 1000);
    case Length(tmSecs) of
        1: tmSecs := '00' + tmSecs;
        2: tmSecs := '0' + tmSecs;
    end;
    tmSecs := Copy(tmSecs, 1, 2);
    Result := tHours + ':' + tMinutes + ':' + tSecs + '.' + tmSecs;
end;

procedure TForm1.Button10Click(Sender: TObject);
var
    PictureType: Integer;
    PictureStream: TStream;
    Success: Boolean;
    MIMEType: String;
    Description: String;
    FileName: String;
begin
    if SaveDialog1.Execute then begin
        FileName := SaveDialog1.FileName;
        PictureStream := TMemoryStream.Create;
        try
            Success := ID3v2Tag.GetCoverPictureStream(ID3v2Tag.FrameExists('APIC'), PictureStream, MIMEType, Description, PictureType);
            if NOT Success then begin
                Exit;
            end;
            //* Set file format
            if MIMEType = 'image/jpeg' then begin
                FileName := ChangeFileExt(FileName, '.jpg');
            end;
            if MIMEType = 'image/png' then begin
                FileName := ChangeFileExt(FileName, '.png');
            end;
            if MIMEType = 'image/gif' then begin
                FileName := ChangeFileExt(FileName, '.gif');
            end;
            if MIMEType = 'image/bmp' then begin
                FileName := ChangeFileExt(FileName, '.bmp');
            end;
            //* Save the stream to file
            TMemoryStream(PictureStream).SaveToFile(FileName);
        finally
            PictureStream.Free;
        end;
    end;
end;

procedure TForm1.Button11Click(Sender: TObject);
var
    Fext: String;
    MIMEType: String;
    FrameIndex: Integer;
    Description: String;
    PictureType: Integer;
begin
    if OpenDialog1.Execute then begin
        //* Decide MIME type according to file extension
        Fext := UpperCase(ExtractFileExt(OpenDialog1.FileName));
        if (Fext = '.JPG')
        OR (Fext = '.JPEG')
        then begin
            MIMEType := 'image/jpeg';
        end;
        if (Fext = '.PNG')
        then begin
            MIMEType := 'image/png';
        end;
        if (Fext = '.BMP')
        then begin
            MIMEType := 'image/bmp';
        end;
        if (Fext = '.GIF')
        then begin
            MIMEType := 'image/gif';
        end;
        //* Image's short description
        Description := Edit21.Text;
        //* Image's type, here front cover ($03), see http://www.id3.org/id3v2.4.0-frames (APIC) on all the image types
        PictureType := $03;
        //* Create a new APIC frame
        FrameIndex := ID3v2Tag.AddFrame('APIC');
        //* Set it's data
        ID3v2Tag.SetCoverPictureFromFile(FrameIndex, Description, OpenDialog1.FileName, MIMEType, PictureType);
    end;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
    ID3v2Tag.Frames[ComboBox1.ItemIndex].Compress;
    ComboBox1Change(Sender);
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
    ID3v2Tag.Frames[ComboBox1.ItemIndex].DeCompress;
    ComboBox1Change(Sender);
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
    ID3v2Tag.Frames[ComboBox1.ItemIndex].ApplyUnsynchronisation;
    ComboBox1Change(Sender);
end;

procedure TForm1.Button15Click(Sender: TObject);
begin
    ID3v2Tag.Frames[ComboBox1.ItemIndex].RemoveUnsynchronisation;
    ComboBox1Change(Sender);
end;

procedure TForm1.Button16Click(Sender: TObject);
var
    i: Integer;
begin
    //* Calculate previous APIC index and load it
    for i := ID3v2Tag.FrameCount - 1 downto 0 do begin
        if IsSameFrameID(ID3v2Tag.Frames[i].ID, 'APIC') then begin
            if i < CurrentAPICIndex then begin
                LoadAPIC(i);
                Break;
            end;
        end;
    end;
end;

procedure TForm1.Button17Click(Sender: TObject);
var
    i: Integer;
begin
    //* Calculate next APIC index and load it
    for i := 0 to ID3v2Tag.FrameCount - 1 do begin
        if IsSameFrameID(ID3v2Tag.Frames[i].ID, 'APIC') then begin
            if i > CurrentAPICIndex then begin
                LoadAPIC(i);
                Break;
            end;
        end;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    if OpenDialog1.Execute
        then Edit1.Text := OpenDialog1.FileName;
end;

procedure TForm1.ClearDatav1;
begin
    //* ID3v1
    Edit2.Text := '';
    Edit3.Text := '';
    Edit4.Text := '';
    Edit5.Text := '';
    Edit6.Text := '';
    ComboBox2.ItemIndex := - 1;
    Edit7.Text := '';
end;

procedure TForm1.ClearDatav2;
begin
    //* ID3v2
    Edit9.Text := '';
    Edit10.Text := '';
    Edit12.Text := '';
    Edit13.Text := '';
    Edit14.Text := '';
    Edit15.Text := '';
    Edit16.Text := '';
    Edit17.Text := '';
    Edit18.Text := '';
    Memo1.Text := '';
    Memo2.Text := '';
    Edit19.Text := '';
    Edit20.Text := '';
    Edit21.Text := '';
    ImageControl1.Bitmap.Clear($FFFFFFFF);
    CurrentAPICIndex := -1;
    Label10.Text := 'unknown';
    Label35.Text := 'unknown';
    Label36.Text := 'unknown';
    Label37.Text := 'unknown';
    Label38.Text := 'unknown';
    Label39.Text := 'unknown';
    Label40.Text := 'unknown';
    Label41.Text := 'unknown';
    Label42.Text := 'unknown';
    Label43.Text := 'unknown';
    ComboBox1.Items.Clear;
    CheckBox1.IsChecked := False;
    CheckBox2.IsChecked := False;
    CheckBox3.IsChecked := False;
    CheckBox4.IsChecked := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    //* Rotation effect
    FMX.Ani.TAnimator.AnimateFloat(Layer3D1, 'RotationAngle.Y', 360, 2, TAnimationType.InOut, TInterpolationType.Back);
    FMX.Ani.TAnimator.AnimateFloat(Layer3D1, 'Position.Z', 500, 1);
    FMX.Ani.TAnimator.AnimateFloatDelay(Layer3D1, 'Position.Z', 0, 1, 1);

    ClearDatav1;
    ID3v1Tag.LoadFromFile(Edit1.Text);
    Edit2.Text := ID3v1Tag.Title;
    Edit3.Text := ID3v1Tag.Artist;
    Edit4.Text := ID3v1Tag.Album;
    Edit5.Text := ID3v1Tag.Year;
    Edit6.Text := ID3v1Tag.Comment;
    Edit7.Text := IntToStr(ID3v1Tag.Track);
    ComboBox2.ItemIndex := ComboBox2.Items.IndexOf(ID3v1Tag.Genre);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
    ErrorCode: Integer;
begin
    ID3v1Tag.Title := Edit2.Text;
    ID3v1Tag.Artist := Edit3.Text;
    ID3v1Tag.Album := Edit4.Text;
    ID3v1Tag.Year := Edit5.Text;
    ID3v1Tag.Comment := Edit6.Text;
    ID3v1Tag.Track := StrToIntDef(Edit7.Text, 0);
    ID3v1Tag.Genre := ComboBox2.Items[ComboBox2.ItemIndex];
    ErrorCode := ID3v1Tag.SaveToFile(Edit1.Text);
    if ErrorCode <> ID3V1LIBRARY_SUCCESS then begin
        Showmessage('Error saving ID3v1 tag, error code: ' + IntToStr(ErrorCode));
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
    Error: Integer;
begin
    Error := RemoveID3v1TagFromFile(Edit1.Text);
    if Error <> ID3V1LIBRARY_SUCCESS then begin
        TDialogService.ShowMessage('Error while removing ID3v1 tag, error code: ' + IntToStr(Error));
        Exit;
    end;
    ID3v1Tag.Clear;
    ClearDatav1;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
    i: Integer;
    LanguageID: TLanguageID;
    Description: String;
    RecordingDateTime: TDateTime;
    Error: Integer;
begin
    //* Rotation effect
    FMX.Ani.TAnimator.AnimateFloat(Layer3D1, 'RotationAngle.Y', 360, 2, TAnimationType.InOut, TInterpolationType.Back);
    FMX.Ani.TAnimator.AnimateFloat(Layer3D1, 'Position.Z', 500, 1);
    FMX.Ani.TAnimator.AnimateFloatDelay(Layer3D1, 'Position.Z', 0, 1, 1);

    //* Clear all the UI
    ClearDatav2;

    //* Load the ID3v2 Tag
    Error := ID3v2Tag.LoadFromFile(Edit1.Text);

    if Error <> ID3V2LIBRARY_SUCCESS then begin
        TDialogService.ShowMessage('Error loading ID3v2 tag, error code: ' + IntToStr(Error) + #13#10 + ID3v2TagErrorCode2String(Error));
    end;

    //* Get Tag version
    if Error <> ID3V2LIBRARY_SUCCESS then begin
        Label10.Text := IntToStr(ID3v2Tag.MajorVersion) + '.' + IntToStr(ID3v2Tag.MinorVersion);
        Exit;
    end else begin
        Label10.Text := IntToStr(ID3v2Tag.MajorVersion) + '.' + IntToStr(ID3v2Tag.MinorVersion);
    end;

    //* The ID3v2 tag was unsynchronised
    CheckBox1.IsChecked := ID3v2Tag.WasUnsynchronised;

    //* To parse the tag properly deunsynchronisation is needed if applied
    //* Note that in 'All Frames' view all frames will appear as not unsynchronised because of this!
    //ID3v2Tag.RemoveUnsynchronisationOnAllFrames;
    //* RemoveUnsynchronisationOnAllFrames() is now called automatically by default.
    //* Set ID3v2Tag.AutoRemoveUnsynchronisation to False to avoid this, or there is a global variable (that has effect before creating an TID3v2Tag instance) 'ID3v2LibraryDefaultAutoRemoveUnsynchronisation' True by default.


    //* Always call 'DeCompressAllFrames' after 'RemoveUnsynchronisationOnAllFrames'
    //* Note that in 'All Frames' view all frames will appear as not compressed because of this!
    ID3v2Tag.DeCompressAllFrames;

    //* Extended header exists in tag
    CheckBox2.IsChecked := ID3v2Tag.ExtendedHeader;

    //* Tag is in experimental stage
    CheckBox3.IsChecked := ID3v2Tag.Experimental;

    //* Extended header CRC32 exists in extended header
    if ID3v2Tag.MajorVersion = 3 then begin
        CheckBox4.IsChecked := ID3v2Tag.ExtendedHeader3.CRCPresent;
    end;

    //* Playtime
    LabelPlayTime.Text := mSec2Time(Trunc(ID3v2Tag.PlayTime * 1000));

    //* Bit rate
    if ID3v2Tag.MPEGInfo.VBR then begin
        LabelBitRate.Text := 'VBR';
    end else begin
        LabelBitRate.Text := IntToStr(ID3v2Tag.BitRate) + ' kbps';
    end;

    //* Get Title
    Edit9.Text := ID3v2Tag.GetText('TIT2');

    //* Get Artist
    Edit10.Text := ID3v2Tag.GetText('TPE1');

    //* Get Album
    Edit11.Text := ID3v2Tag.GetText('TALB');

    //* Get Year
    Edit12.Text := ID3v2Tag.GetText('TYER');

    //* Get recording time
    RecordingDateTime := ID3v2Tag.GetTime('TDRC');
    if RecordingDateTime <> 0 then begin
        Edit15.Text := DateTimeToStr(RecordingDateTime);
    end;

    //* Get track no.
    Edit16.Text := ID3v2Tag.GetText('TRCK');

    //* Get Genre
    Edit13.Text := ID3v2DecodeGenre(ID3v2Tag.GetText('TCON'));

    //* Get URL (WXXX is most common)
    Edit14.Text := ID3v2Tag.GetUserDefinedURLLink('WXXX', Description);
    //* For other URL types use GetUnicodeURL()

    //* Get Comment
    Memo2.Text := ID3v2Tag.GetComment('COMM', LanguageID, Description);
    Edit19.Text := LanguageIDtoString(LanguageID);
    Edit20.Text := Description;

    //* Lyrics
    Memo1.Text := ID3v2Tag.GetLyrics('USLT', LanguageID, Description);
    Edit17.Text := LanguageIDtoString(LanguageID);
    Edit18.Text := Description;

    //* Load first cover picture
    LoadAPIC(0);

    //* Get all the frame ID's
    //* See 'http://www.id3.org/id3v2.4.0-frames' for all the frame types and their description
    ComboBox1.Clear;
    for i := 0 to ID3v2Tag.FrameCount - 1 do begin
        ComboBox1.Items.Add(ConvertFrameID2String(ID3v2Tag.Frames[i].ID));
    end;
    ComboBox1.ItemIndex := 0;
    //* Display frame data
    ComboBox1Change(Self);

end;

procedure TForm1.Button6Click(Sender: TObject);
var
    ErrorCode: Integer;
    LanguageID: TLanguageID;
begin
    //* Set Title
    ID3v2Tag.SetText('TIT2', Edit9.Text);

    //* Set Artist
    ID3v2Tag.SetText('TPE1', Edit10.Text);

    //* Set Album
    ID3v2Tag.SetText('TALB', Edit11.Text);

    //* Set Year
    ID3v2Tag.SetText('TYER', Edit12.Text);

    //* Set Genre
    ID3v2Tag.SetText('TCON', Edit13.Text);

    //* Set track no.
    ID3v2Tag.SetText('TRCK', Edit16.Text);

    //* Set URL (WXXX is most common)
    ID3v2Tag.SetUserDefinedURLLink('WXXX', Edit14.Text, 'Description');
    //* For other URL types use SetUnicodeURL()

    //* Set Comment
    if Memo2.Text <> '' then begin
        //* Language ID is 3 bytes long ANSI string
        StringToLanguageID(Edit19.Text, LanguageID);
        ID3v2Tag.SetComment('COMM', Memo2.Text, LanguageID, Edit20.Text);
    end;

    //* Set lyrics
    if Memo1.Text <> '' then begin
        //* Language ID is 3 bytes long ANSI string we convert it here
        StringToLanguageID(Edit17.Text, LanguageID);
        ID3v2Tag.SetLyrics('USLT', Memo3.Text, LanguageID, Edit20.Text);
    end;

    if CheckBox1.IsChecked then begin
        ID3v2Tag.ApplyUnsynchronisationOnAllFrames;
    end;

    //* Save the ID3v2 Tag
    ErrorCode := ID3v2Tag.SaveToFile(Edit1.Text);

    if ErrorCode <> ID3V2LIBRARY_SUCCESS then begin
        TDialogService.ShowMessage('Error saving ID3v2 tag, error code: ' + IntToStr(ErrorCode) + #13#10 + ID3v2TagErrorCode2String(ErrorCode));
    end;

end;

procedure TForm1.Button7Click(Sender: TObject);
var
    ErrorCode: Integer;
begin
    ErrorCode := RemoveID3v2TagFromFile(Edit1.Text);
    if ErrorCode <> ID3V2LIBRARY_SUCCESS then begin
        TDialogService.ShowMessage('Error while removing ID3v2 tag, error code: ' + IntToStr(ErrorCode) + #13#10 + ID3v2TagErrorCode2String(ErrorCode));
        Exit;
    end;
    ID3v2Tag.Clear;
    ClearDatav2;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
    LoadAPIC(0);
end;

procedure TForm1.Button9Click(Sender: TObject);
var
    Success: Boolean;
begin
    Success := ID3v2Tag.DeleteFrame(CurrentAPICIndex);
    Showmessage(BoolToStr(Success, True));
    Button16Click(nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    //* Create both Tag classes
    ID3v1Tag := TID3v1Tag.Create;
    ID3v2Tag := TID3v2Tag.Create;

    PopulateGenres(ComboBox2);
end;

procedure TForm1.PopulateGenres(ComboBox: TComboBox);
var
    i: Integer;
begin
    for i := 0 to Length(ID3Genres) - 1 do begin
        ComboBox.Items.Add(ID3Genres[i]);
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    //* Free the class
    if ID3v1Tag <> nil
        then FreeAndNil(ID3v1Tag);
    //* Free the class
    if ID3v2Tag <> nil
        then FreeAndNil(ID3v2Tag);
end;

function TForm1.LoadAPIC(Index: Integer): Boolean;
var
    PictureType: Integer;
    PictureStream: TStream;
    Success: Boolean;
    MIMEType: String;
    Description: String;
    i: Integer;
begin
    Result := False;
    try
        PictureStream := TMemoryStream.Create;
        try
            if Index = 0 then begin
                Index := ID3v2Tag.FrameExists('APIC');
            end;
            if Index < 0 then begin
                Exit;
            end;

            Success := ID3v2Tag.GetCoverPictureStream(Index, PictureStream, MIMEType, Description, PictureType);
            //* No APIC picture found, exit
            if (PictureStream.Size = 0)
            OR (NOT Success)
            then begin
                Exit;
            end;

            //* Get first APIC index
            if Index = 0 then begin
                for i := 0 to ID3v2Tag.FrameCount - 1 do begin
                    if IsSameFrameID(ID3v2Tag.Frames[i].ID, 'APIC') then begin
                        CurrentAPICIndex := i;
                        Break;
                    end;
                end;
            //* Store the current APIC index as Index is frame index
            end else begin
                CurrentAPICIndex := Index;
            end;

            //* Display description
            Edit21.Text := Description;
            //* Display type
            Label24.Text := APICType2Str(PictureType);
            //* Do what you want with PictureStream here
            //* We load the APIC picture into a TImageControl
            //* If JPG
            {
            if (MIMEType = 'image/jpeg')
            OR (MIMEType = 'image/jpg')
            then begin
            }
               {
                PictureStream.Seek(0, soFromBeginning);
                TempBitmap := TBitmap.Create(0, 0);
                Filter := DefaultBitmapCodecClass.Create;
                try
                    Filter.LoadFromStream(PictureStream, TempBitmap);
                    ImageControl1.Bitmap.Assign(TempBitmap);
                finally
                    Filter.Free;
                    TempBitmap.Free;
                end;
               }

               ImageControl1.Bitmap.LoadFromStream(PictureStream);

            {end;}

            //* The APIC stream format can be decided from MIMEType for example:
            //* If BMP
            {
            if MIMEType = 'image/bmp' then begin
                PictureStream.Seek(0, soFromBeginning);
                ImageControl1.Bitmap.LoadFromStream(PictureStream);
            end;
            }
            //* To load other formats a third party component is needed, for example GraphicEx or Free Image Library (for Windows)
            Result := True;
        finally
            PictureStream.Free;
        end;
    except
        //*
    end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
type
    StreamPointer = array of Byte;
var
    tempString, asciiString, rowNo : String;
    Count, Remainder, LineCount, DataSize: Cardinal;
    tempByte: Byte;
begin
    //Screen.Cursor := crAppStart;
    Memo3.Text := '';
    SetFrameAttributes;
    SetFrameCommandButtons;
    DataSize := ID3v2Tag.Frames[ComboBox1.ItemIndex].Stream.Size;
    Count := 0;
    ID3v2Tag.Frames[ComboBox1.ItemIndex].Stream.Seek(0, soBeginning);
    Memo3.Lines.BeginUpdate;
    //* Function to display frame data as hex and string
    while Count < DataSize do begin
        tempString := '';
        asciiString := '';
        if DataSize - Count >= 16
            then Remainder := 16
            else Remainder := DataSize - Count;
        for LineCount := 0 to Remainder - 1 do begin
            ID3v2Tag.Frames[ComboBox1.ItemIndex].Stream.Read(tempByte, 1);
            tempString := tempString + inttohex(tempByte, 2) + ' ';
            if (tempByte > 31)
            and (tempByte < 255)
                then asciiString := asciiString + chr(tempByte)
                else asciiString := asciiString + chr(95);
        end;
        LineCount := Remainder - 1;
        while LineCount < 15 do begin
            tempString := tempString + '   ';
            Inc(LineCount);
        end;
        rowNo := IntToStr(Count);
        case Length(rowNo) of
            1: rowNo := '000000' + rowNo;
            2: rowNo := '00000' + rowNo;
            3: rowNo := '0000' + rowNo;
            4: rowNo := '000' + rowNo;
            5: rowNo := '00' + rowNo;
            6: rowNo := '0' + rowNo;
        end;
        rowNo := rowNo + ': ';
        Memo3.Lines.Append(rowNo + tempString + ' ' + asciiString);
        Inc(Count, Remainder);
    end;
    Memo3.Lines.EndUpdate;
    //Screen.Cursor := crDefault;
end;

procedure TForm1.SetFrameAttributes;
begin
    Label35.Text := IntToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].Stream.Size);
    Label36.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].TagAlterPreservation, True);
    Label37.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].FileAlterPreservation, True);
    Label38.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].ReadOnly, True);
    Label39.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].GroupingIdentity, True);
    Label40.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].Compressed, True);
    Label41.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].Encrypted, True);
    Label42.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].Unsynchronised, True);
    Label43.Text := BoolToStr(ID3v2Tag.Frames[ComboBox1.ItemIndex].DataLengthIndicator, True);
end;

procedure TForm1.SetFrameCommandButtons;
begin
    if ID3v2Tag.Frames[ComboBox1.ItemIndex].Compressed then begin
        Button12.Enabled := False;
        Button13.Enabled := True;
    end else begin
        Button12.Enabled := True;
        Button13.Enabled := False;
    end;
    if ID3v2Tag.Frames[ComboBox1.ItemIndex].Unsynchronised then begin
        Button14.Enabled := False;
        Button15.Enabled := True;
    end else begin
        Button14.Enabled := True;
        Button15.Enabled := False;
    end;
end;

end.
