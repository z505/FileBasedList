object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 279
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object mStatus: TMemo
    Left = 16
    Top = 12
    Width = 441
    Height = 191
    Lines.Strings = (
      'Status:')
    TabOrder = 0
  end
  object bAddThousands: TButton
    Left = 280
    Top = 209
    Width = 177
    Height = 25
    Caption = 'Add 100,000 Integers'
    TabOrder = 1
    OnClick = bAddThousandsClick
  end
  object bAddMillion: TButton
    Left = 280
    Top = 236
    Width = 177
    Height = 25
    Caption = 'Add 1,000,000 Integers'
    TabOrder = 2
    OnClick = bAddMillionClick
  end
end
