object GridDiffForm: TGridDiffForm
  Left = 533
  Top = 363
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Grid Differences '
  ClientHeight = 115
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object CheckBox1: TCheckBox
    Left = 24
    Top = 23
    Width = 113
    Height = 17
    Caption = 'Histograms'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 46
    Width = 137
    Height = 17
    Caption = 'Difference map'
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 80
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 107
    Top = 80
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox3: TCheckBox
    Left = 24
    Top = 0
    Width = 97
    Height = 17
    Caption = 'Scatter plot'
    TabOrder = 4
  end
end
