object KML_over_opts: TKML_over_opts
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'KML map overlay options'
  ClientHeight = 454
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 129
    Top = 414
    Width = 77
    Height = 27
    Caption = 'OK'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    Margin = 2
    ModalResult = 1
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 225
    Top = 414
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
  object RadioGroup2: TRadioGroup
    Left = 0
    Top = 94
    Width = 305
    Height = 49
    Caption = 'KML export'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Map && Overlays'
      'Map only'
      'Overlays only')
    TabOrder = 2
    OnClick = RadioGroup2Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 144
    Width = 305
    Height = 81
    TabOrder = 3
    object Label1: TLabel
      Left = 225
      Top = 58
      Width = 38
      Height = 13
      Caption = 'Opaque'
    end
    object Transparent: TLabel
      Left = 91
      Top = 58
      Width = 59
      Height = 13
      Caption = 'Transparent'
    end
    object TrackBar1: TTrackBar
      Left = 91
      Top = 28
      Width = 182
      Height = 32
      Max = 255
      Frequency = 25
      Position = 255
      TabOrder = 0
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 8
      Width = 65
      Height = 57
      Caption = 'Format'
      ItemIndex = 0
      Items.Strings = (
        'GIF'
        'PNG')
      TabOrder = 1
      OnClick = RadioGroup1Click
    end
    object CheckBox1: TCheckBox
      Left = 91
      Top = 5
      Width = 97
      Height = 17
      Caption = 'Transparent GIF'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox3: TCheckBox
      Left = 194
      Top = 5
      Width = 108
      Height = 17
      Caption = 'Transparent PNG'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 237
    Width = 164
    Height = 17
    Caption = 'Separate map layers'
    TabOrder = 4
  end
  object AllMapsCheckBox3: TCheckBox
    Left = 8
    Top = 260
    Width = 97
    Height = 17
    Caption = 'All maps'
    TabOrder = 5
  end
  object CheckBox4: TCheckBox
    Left = 152
    Top = 237
    Width = 97
    Height = 17
    Caption = 'Zip KML into KMZ'
    TabOrder = 6
  end
  object CheckBox6: TCheckBox
    Left = 8
    Top = 329
    Width = 105
    Height = 17
    Caption = 'Create world files'
    TabOrder = 7
  end
  object CheckBox7: TCheckBox
    Left = 8
    Top = 375
    Width = 115
    Height = 17
    Caption = 'Open Google Earth'
    TabOrder = 8
  end
  object CheckBox8: TCheckBox
    Left = 8
    Top = 283
    Width = 134
    Height = 17
    Caption = 'Individual viewsheds'
    TabOrder = 9
  end
  object Edit8: TEdit
    Left = 103
    Top = 35
    Width = 199
    Height = 21
    Enabled = False
    TabOrder = 10
  end
  object BitBtn3: TBitBtn
    Left = 10
    Top = 31
    Width = 87
    Height = 25
    Caption = 'Upper left logo'
    TabOrder = 11
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 63
    Width = 87
    Height = 25
    Caption = 'Lower left logo'
    TabOrder = 12
    OnClick = BitBtn4Click
  end
  object Edit9: TEdit
    Left = 103
    Top = 62
    Width = 199
    Height = 21
    Enabled = False
    TabOrder = 13
  end
  object CheckBox5: TCheckBox
    Left = 152
    Top = 260
    Width = 97
    Height = 17
    Caption = 'Debug KML file'
    TabOrder = 14
  end
  object CheckBox9: TCheckBox
    Left = 144
    Top = 345
    Width = 134
    Height = 17
    Caption = 'Allow time animations'
    TabOrder = 15
  end
  object CheckBox10: TCheckBox
    Left = 144
    Top = 368
    Width = 158
    Height = 17
    Caption = 'Default database  options'
    TabOrder = 16
  end
  object CheckBox11: TCheckBox
    Left = 152
    Top = 306
    Width = 97
    Height = 17
    Caption = 'Clean up HTML'
    TabOrder = 17
  end
  object CheckBox12: TCheckBox
    Left = 152
    Top = 283
    Width = 97
    Height = 17
    Caption = 'DB tables in KML'
    TabOrder = 18
  end
  object CheckBox13: TCheckBox
    Left = 8
    Top = 306
    Width = 97
    Height = 17
    Caption = 'Label graticule'
    TabOrder = 19
  end
  object CheckBox14: TCheckBox
    Left = 144
    Top = 391
    Width = 97
    Height = 17
    Caption = 'Top level folder'
    TabOrder = 20
  end
end
