object CurvatureForm: TCurvatureForm
  Left = 714
  Top = 469
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Vertical Earth Curvature'
  ClientHeight = 157
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 120
    Top = 72
    Width = 7
    Height = 13
    Caption = 'K'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 105
    Height = 105
    Caption = 'Algorithm'
    Items.Strings = (
      'None'
      'TM5-441'
      'Radio'
      'Yoeli')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object OKBtn: TBitBtn
    Left = 24
    Top = 122
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 109
    Top = 122
    Width = 77
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit1: TEdit
    Left = 136
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 3
  end
end
