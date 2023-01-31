object DRGAnaglyphForm: TDRGAnaglyphForm
  Left = 451
  Top = 126
  BorderIcons = []
  Caption = 'Stereo Map Options'
  ClientHeight = 294
  ClientWidth = 286
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
  object Label3: TLabel
    Left = 134
    Top = 87
    Width = 62
    Height = 13
    Caption = 'Max displace'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 18
    Top = 249
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
  object OKBtn: TBitBtn
    Left = 78
    Top = 249
    Width = 57
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 136
    Top = 8
    Width = 59
    Height = 25
    Caption = 'Shading'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object HelpBtn: TBitBtn
    Left = 141
    Top = 249
    Width = 59
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox5: TCheckBox
    Left = 18
    Top = 202
    Width = 97
    Height = 17
    Caption = 'Hide map grids'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox5Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 87
    Width = 106
    Height = 86
    Caption = 'Stereo mode'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Anaglyph'
      'Stereo pair')
    TabOrder = 4
    OnClick = RadioGroup1Click
  end
  object Edit2: TEdit
    Left = 202
    Top = 84
    Width = 36
    Height = 21
    TabOrder = 5
    Text = '30'
    OnChange = Edit2Change
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 8
    Width = 107
    Height = 73
    Caption = 'Shading'
    Items.Strings = (
      'None'
      'Reflectance'
      'Intensity')
    TabOrder = 6
    OnClick = RadioGroup2Click
  end
  object CheckBox2: TCheckBox
    Left = 18
    Top = 179
    Width = 236
    Height = 17
    Caption = 'Elevation from separate DEM'
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 133
    Top = 128
    Width = 121
    Height = 17
    Caption = 'EW anaglpyph shift'
    TabOrder = 8
    OnClick = CheckBox3Click
  end
end
