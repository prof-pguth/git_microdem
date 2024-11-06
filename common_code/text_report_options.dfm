object ReportOptionsForm: TReportOptionsForm
  Left = 44
  Top = 39
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Text/CSV Report Options'
  ClientHeight = 192
  ClientWidth = 208
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 122
    Width = 51
    Height = 13
    Caption = 'Thin factor'
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 146
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 121
    Height = 105
    Caption = 'Field separators'
    ItemIndex = 1
    Items.Strings = (
      'Space'
      'Comma (,)'
      'Tab'
      'Pipe (|)')
    TabOrder = 1
  end
  object HelpBtn: TBitBtn
    Left = 93
    Top = 146
    Width = 76
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit1: TEdit
    Left = 80
    Top = 119
    Width = 49
    Height = 21
    TabOrder = 3
    Text = '1'
    OnChange = Edit1Change
  end
  object UpDown1: TUpDown
    Left = 152
    Top = 115
    Width = 17
    Height = 25
    TabOrder = 4
    OnClick = UpDown1Click
  end
end
