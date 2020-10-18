object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 251
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 265
    Height = 21
    TabOrder = 0
    Text = 'https://icecast-newradio.cdnvideo.ru/newradio3'
  end
  object Button1: TButton
    Left = 8
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Play'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Play Async'
    TabOrder = 2
    OnClick = Button2Click
  end
  object FMXPlayer1: TFMXPlayer
    Left = 112
    Top = 104
  end
end
