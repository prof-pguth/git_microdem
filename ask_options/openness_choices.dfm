object OpenOptForm: TOpenOptForm
  Left = 0
  Top = 0
  Caption = 'Openness options'
  ClientHeight = 279
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 152
    Width = 91
    Height = 15
    Caption = 'Radial length (m)'
  end
  object Label2: TLabel
    Left = 24
    Top = 176
    Width = 109
    Height = 15
    Caption = 'Radial length (pixels)'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 24
    Top = 246
    Width = 73
    Height = 25
    Hint = 'Force redraw'
    Caption = 'Redraw'
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
  object Label3: TLabel
    Left = 26
    Top = 210
    Width = 107
    Height = 15
    Caption = 'Start readials at pixel'
  end
  object RadioGroup1: TRadioGroup
    Left = 145
    Top = 24
    Width = 121
    Height = 89
    Caption = 'Radials in terms of '
    Items.Strings = (
      'Meters'
      'Pixels')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object GroupBox1: TGroupBox
    Left = 304
    Top = 24
    Width = 81
    Height = 161
    Caption = 'Direction'
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 16
      Top = 16
      Width = 45
      Height = 17
      Caption = 'N'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 32
      Width = 45
      Height = 17
      Caption = 'NE'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 48
      Width = 45
      Height = 17
      Caption = 'E'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 64
      Width = 45
      Height = 17
      Caption = 'SE'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 80
      Width = 45
      Height = 17
      Caption = 'S'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBox6: TCheckBox
      Left = 16
      Top = 96
      Width = 45
      Height = 17
      Caption = 'SW'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBox7: TCheckBox
      Left = 16
      Top = 112
      Width = 40
      Height = 17
      Caption = 'W'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 128
      Width = 45
      Height = 17
      Caption = 'NW'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
  object Edit1: TEdit
    Left = 139
    Top = 149
    Width = 62
    Height = 23
    TabOrder = 2
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 139
    Top = 178
    Width = 62
    Height = 23
    TabOrder = 3
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object GroupBox2: TGroupBox
    Left = 12
    Top = 24
    Width = 121
    Height = 105
    Caption = 'Compute Openness'
    TabOrder = 4
    object CheckBox9: TCheckBox
      Left = 21
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Upward'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox10: TCheckBox
      Left = 21
      Top = 47
      Width = 97
      Height = 17
      Caption = 'Downward'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox11: TCheckBox
      Left = 21
      Top = 70
      Width = 97
      Height = 17
      Caption = 'Difefrence'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object Edit3: TEdit
    Left = 139
    Top = 207
    Width = 62
    Height = 23
    TabOrder = 5
    Text = 'Edit3'
    OnChange = Edit3Change
  end
  object HelpBtn: TBitBtn
    Left = 126
    Top = 244
    Width = 48
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
end
