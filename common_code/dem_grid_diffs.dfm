object GridDiffForm: TGridDiffForm
  Left = 533
  Top = 363
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Grid Differences '
  ClientHeight = 167
  ClientWidth = 267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 96
    Width = 80
    Height = 13
    Caption = 'Level to highlight'
  end
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
    Top = 125
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
    Left = 109
    Top = 125
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
  object CheckBox4: TCheckBox
    Left = 24
    Top = 69
    Width = 217
    Height = 17
    Caption = 'Highlight significant change on map'
    TabOrder = 5
    OnClick = CheckBox4Click
  end
  object Edit1: TEdit
    Left = 167
    Top = 92
    Width = 74
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
  end
end
