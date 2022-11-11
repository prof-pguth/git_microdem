object NetOptFm: TNetOptFm
  Left = 263
  Top = 115
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Stereo Net Options'
  ClientHeight = 259
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 489
    Height = 224
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 45
    Top = 238
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 248
    Top = 238
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 352
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Legend'
    TabOrder = 2
  end
  object RadioGroup1: TRadioGroup
    Left = 247
    Top = 88
    Width = 78
    Height = 97
    Caption = 'Grid size'
    ItemIndex = 0
    Items.Strings = (
      '30'#176
      '15'#176
      '10'#176
      '5'#176)
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 152
    Top = 88
    Width = 89
    Height = 97
    Caption = 'Grid'
    Items.Strings = (
      'Polar'
      'Equatorial'
      'Azimuths'
      'None')
    TabOrder = 4
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 352
    Top = 71
    Width = 113
    Height = 115
    Caption = 'Contouring colors'
    Items.Strings = (
      'Spectrum'
      'Rainbow'
      'Terrain'
      'Gray Scale'
      'Contrast B&&W'
      'Gray Dither')
    TabOrder = 5
  end
  object RadioGroup4: TRadioGroup
    Left = 25
    Top = 88
    Width = 121
    Height = 113
    Caption = 'Net size'
    Items.Strings = (
      'Tiny'
      'Small'
      'Medium'
      'Large'
      'Moderately large'
      'Very large')
    TabOrder = 6
    OnClick = RadioGroup4Click
  end
  object CheckBox2: TCheckBox
    Left = 352
    Top = 32
    Width = 97
    Height = 17
    Caption = 'North tick'
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 352
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Center tick'
    TabOrder = 8
    OnClick = CheckBox3Click
  end
  object RadioGroup5: TRadioGroup
    Left = 200
    Top = 18
    Width = 113
    Height = 64
    Caption = 'Hemisphere'
    Items.Strings = (
      'Upper'
      'Lower')
    TabOrder = 9
    OnClick = RadioGroup5Click
  end
  object RadioGroup6: TRadioGroup
    Left = 25
    Top = 24
    Width = 153
    Height = 58
    Caption = 'Net used'
    Items.Strings = (
      'Schmidt (equal area)'
      'Wulff (equal angle)')
    TabOrder = 10
    OnClick = RadioGroup6Click
  end
  object BitBtn1: TBitBtn
    Left = 352
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Grid'
    TabOrder = 11
    OnClick = BitBtn1Click
  end
  object CancelBtn: TBitBtn
    Left = 141
    Top = 238
    Width = 78
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 12
    IsControl = True
  end
end
