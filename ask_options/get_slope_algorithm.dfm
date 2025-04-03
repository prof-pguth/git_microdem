object GetSlopeAlgorithm: TGetSlopeAlgorithm
  Left = 363
  Top = 242
  ActiveControl = OKBtn
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Slope Map Options'
  ClientHeight = 252
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label3: TLabel
    Left = 124
    Top = 10
    Width = 5
    Height = 13
  end
  object Label6: TLabel
    Left = 135
    Top = 15
    Width = 39
    Height = 13
    Caption = 'Label6'
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 212
    Width = 77
    Height = 27
    Caption = '&OK'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 100
    Top = 212
    Width = 77
    Height = 27
    Caption = '&Cancel'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 183
    Top = 212
    Width = 76
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object Button11: TButton
    Left = 2
    Top = 10
    Width = 116
    Height = 25
    Caption = 'Slope Algorithm'
    TabOrder = 3
    OnClick = Button11Click
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 41
    Width = 212
    Height = 50
    Caption = 'LSQ order'
    Columns = 4
    Items.Strings = (
      '1'
      '2'
      '3'
      '4')
    TabOrder = 4
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 97
    Width = 212
    Height = 50
    Caption = 'LSQ window size'
    Columns = 4
    Items.Strings = (
      '3x3'
      '5x5'
      '7x7'
      '9x9')
    TabOrder = 5
    OnClick = RadioGroup3Click
  end
  object RadioGroup5: TRadioGroup
    Left = 8
    Top = 153
    Width = 212
    Height = 40
    Caption = 'Use windws points'
    Columns = 3
    Items.Strings = (
      'All'
      'Edge'
      'Queen'#39's')
    TabOrder = 6
    OnClick = RadioGroup5Click
  end
end
