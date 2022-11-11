object FieldPicker: TFieldPicker
  Left = 451
  Top = 319
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Pick fields from data base'
  ClientHeight = 178
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 3
    Height = 13
    Caption = ' '
  end
  object Label2: TLabel
    Left = 176
    Top = 8
    Width = 3
    Height = 13
    Caption = ' '
  end
  object Label3: TLabel
    Left = 320
    Top = 8
    Width = 3
    Height = 13
    Caption = ' '
  end
  object Label5: TLabel
    Left = 8
    Top = 77
    Width = 87
    Height = 13
    Caption = 'String field to color'
  end
  object Label6: TLabel
    Left = 168
    Top = 77
    Width = 99
    Height = 13
    Caption = 'Numeric field to color'
  end
  object Label7: TLabel
    Left = 320
    Top = 77
    Width = 58
    Height = 13
    Caption = 'Field for size'
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 24
    Width = 129
    Height = 21
    TabOrder = 0
  end
  object ComboBox2: TComboBox
    Left = 168
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object ComboBox3: TComboBox
    Left = 320
    Top = 24
    Width = 113
    Height = 21
    TabOrder = 2
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 140
    Width = 73
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = BitBtn4Click
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 329
    Top = 51
    Width = 113
    Height = 17
    Caption = 'Reverse Z values'
    TabOrder = 4
  end
  object ComboBox4: TComboBox
    Left = 8
    Top = 96
    Width = 129
    Height = 21
    TabOrder = 5
  end
  object ComboBox5: TComboBox
    Left = 162
    Top = 96
    Width = 129
    Height = 21
    TabOrder = 6
  end
  object ComboBox6: TComboBox
    Left = 313
    Top = 96
    Width = 129
    Height = 21
    TabOrder = 7
  end
  object CheckBox2: TCheckBox
    Left = 208
    Top = 145
    Width = 97
    Height = 17
    Caption = 'Flip histogram'
    TabOrder = 8
    OnClick = CheckBox2Click
  end
end
