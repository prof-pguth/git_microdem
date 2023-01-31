object TMapOptsForm: TTMapOptsForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Advanced  map colors and shading '
  ClientHeight = 339
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object RedrawSpeedButton12: TSpeedButton
    Left = 22
    Top = 297
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 113
    Height = 121
    Caption = 'DEM color merge'
    Items.Strings = (
      'None'
      'Elevation'
      'Slope'
      'Reflectance'
      'Aspect')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object HelpBtn: TBitBtn
    Left = 153
    Top = 297
    Width = 62
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 73
    Top = 297
    Width = 57
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
    TabOrder = 2
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 151
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Reflectance'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 197
    Width = 75
    Height = 25
    Caption = 'Slope'
    TabOrder = 4
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 232
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Elevation'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 89
    Top = 197
    Width = 41
    Height = 25
    Caption = 'Max'
    TabOrder = 6
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 149
    Width = 75
    Height = 25
    Caption = 'Merge colors'
    TabOrder = 7
    OnClick = BitBtn5Click
  end
  object GroupBox1: TGroupBox
    Left = 155
    Top = 92
    Width = 190
    Height = 41
    Caption = 'World outlines'
    TabOrder = 8
    object CheckBox4: TCheckBox
      Left = 11
      Top = 16
      Width = 72
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 0
    end
    object CheckBox5: TCheckBox
      Left = 89
      Top = 16
      Width = 66
      Height = 17
      Caption = 'Subdue'
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 155
    Top = 12
    Width = 190
    Height = 74
    Caption = 'Base map'
    TabOrder = 9
    object CheckBox3: TCheckBox
      Left = 86
      Top = 16
      Width = 66
      Height = 17
      Caption = 'Subdue'
      TabOrder = 0
    end
    object Checkbox1: TCheckBox
      Left = 11
      Top = 16
      Width = 69
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 1
    end
    object CheckBox2: TCheckBox
      Left = 42
      Top = 33
      Width = 97
      Height = 17
      Caption = 'Include overlays'
      TabOrder = 2
    end
    object CheckBox6: TCheckBox
      Left = 44
      Top = 54
      Width = 97
      Height = 17
      Caption = 'Include grids'
      TabOrder = 3
    end
  end
  object GroupBox3: TGroupBox
    Left = 155
    Top = 139
    Width = 190
    Height = 47
    Caption = 'OpenStreetMap'
    TabOrder = 10
    object CheckBox7: TCheckBox
      Left = 13
      Top = 21
      Width = 86
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 0
    end
    object CheckBox8: TCheckBox
      Left = 105
      Top = 22
      Width = 66
      Height = 17
      Caption = 'Subdue'
      TabOrder = 1
    end
  end
  object GroupBox4: TGroupBox
    Left = 155
    Top = 192
    Width = 190
    Height = 43
    Caption = 'Tiger'
    TabOrder = 11
    object CheckBox9: TCheckBox
      Left = 13
      Top = 15
      Width = 76
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 0
    end
    object CheckBox10: TCheckBox
      Left = 105
      Top = 13
      Width = 66
      Height = 17
      Caption = 'Subdue'
      TabOrder = 1
    end
  end
  object CheckBox11: TCheckBox
    Left = 8
    Top = 241
    Width = 273
    Height = 17
    Caption = 'Elevation from separate DEM'
    TabOrder = 12
    OnClick = CheckBox11Click
  end
end
