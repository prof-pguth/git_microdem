object TrendPick: TTrendPick
  Left = 86
  Top = 487
  BorderIcons = []
  Caption = 'Trend Surface Options'
  ClientHeight = 246
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDesigned
  OnCreate = FormCreate
  TextHeight = 13
  object RedrawSpeedButton12: TSpeedButton
    Left = 68
    Top = 216
    Width = 25
    Height = 22
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
  object Label1: TLabel
    Left = 32
    Top = 160
    Width = 31
    Height = 13
    Caption = 'Higher'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 65
    Height = 145
    Caption = 'Order'
    ItemIndex = 0
    Items.Strings = (
      '1 '
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object CheckBox1: TCheckBox
    Left = 99
    Top = 123
    Width = 201
    Height = 17
    Caption = 'Graph of trend surface versus DEM'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 99
    Top = 31
    Width = 161
    Height = 17
    Caption = 'Map of deviations'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object OKBtn: TBitBtn
    Left = 116
    Top = 211
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object CheckBox3: TCheckBox
    Left = 99
    Top = 100
    Width = 161
    Height = 17
    Caption = 'Histogram of deviations'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 99
    Top = 77
    Width = 97
    Height = 17
    Caption = 'Text results'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = CheckBox4Click
  end
  object HelpBtn: TBitBtn
    Left = 199
    Top = 211
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox5: TCheckBox
    Left = 99
    Top = 54
    Width = 145
    Height = 17
    Caption = 'Trend surface over voids'
    TabOrder = 7
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 99
    Top = 8
    Width = 118
    Height = 17
    Caption = 'Map trend surface'
    TabOrder = 8
    OnClick = CheckBox6Click
  end
  object CheckBox7: TCheckBox
    Left = 99
    Top = 188
    Width = 97
    Height = 17
    Caption = 'Open maps'
    TabOrder = 9
    OnClick = CheckBox7Click
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 184
    Width = 85
    Height = 25
    Caption = 'Outcrop'
    TabOrder = 10
    OnClick = BitBtn1Click
  end
  object Edit1: TEdit
    Left = 79
    Top = 157
    Width = 66
    Height = 21
    TabOrder = 11
    Text = ' '
    OnChange = Edit1Change
  end
end
