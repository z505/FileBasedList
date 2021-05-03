object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 419
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 220
    Top = 350
    Width = 99
    Height = 16
    Caption = 'Compare the two'
  end
  object Memo1: TMemo
    Left = 18
    Top = 12
    Width = 403
    Height = 307
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 326
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Fast'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 326
    Top = 359
    Width = 75
    Height = 25
    Caption = 'Slow'
    TabOrder = 2
    OnClick = Button2Click
  end
end
