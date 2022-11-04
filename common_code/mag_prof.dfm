object PickMagProfVars: TPickMagProfVars
  Left = 634
  Top = 347
  Caption = 'Magnetic anomaly variables'
  ClientHeight = 194
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 13
  object Label1: TLabel
    Left = 88
    Top = 8
    Width = 3
    Height = 13
    Caption = 'I'
  end
  object Label2: TLabel
    Left = 88
    Top = 32
    Width = 27
    Height = 13
    Caption = 'Alpha'
  end
  object Label3: TLabel
    Left = 88
    Top = 56
    Width = 68
    Height = 13
    Caption = 'Block intensity'
  end
  object Label4: TLabel
    Left = 294
    Top = 11
    Width = 132
    Height = 13
    Caption = 'Half spreading rate (km/Ma)'
  end
  object Label5: TLabel
    Left = 87
    Top = 110
    Width = 82
    Height = 13
    Caption = 'Age left side (Ma)'
  end
  object Label6: TLabel
    Left = 87
    Top = 134
    Width = 88
    Height = 13
    Caption = 'Age right side (Ma)'
  end
  object Label7: TLabel
    Left = 294
    Top = 43
    Width = 91
    Height = 13
    Caption = 'Ridge location (km)'
    Enabled = False
  end
  object Label8: TLabel
    Left = 216
    Top = 112
    Width = 32
    Height = 13
    Caption = 'Label8'
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 8
    Top = 32
    Width = 73
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 8
    Top = 56
    Width = 73
    Height = 21
    TabOrder = 2
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 161
    Width = 129
    Height = 25
    Caption = 'Modify Model Parameters'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 83
    Width = 81
    Height = 17
    Caption = 'Time scale'
    TabOrder = 4
  end
  object Edit4: TEdit
    Left = 215
    Top = 8
    Width = 73
    Height = 21
    Enabled = False
    TabOrder = 5
    Text = '50'
  end
  object BitBtn3: TBitBtn
    Left = 143
    Top = 161
    Width = 77
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = BitBtn3Click
    IsControl = True
  end
  object Edit5: TEdit
    Left = 8
    Top = 107
    Width = 73
    Height = 21
    TabOrder = 7
    OnChange = Edit5Change
  end
  object Edit6: TEdit
    Left = 8
    Top = 134
    Width = 73
    Height = 21
    TabOrder = 8
    OnChange = Edit6Change
  end
  object Edit7: TEdit
    Left = 216
    Top = 40
    Width = 72
    Height = 21
    Enabled = False
    TabOrder = 9
    Text = 'Edit7'
  end
  object CheckBox2: TCheckBox
    Left = 111
    Top = 83
    Width = 97
    Height = 17
    Caption = 'Anomaly names'
    TabOrder = 10
  end
  object RadioGroup1: TRadioGroup
    Left = 264
    Top = 144
    Width = 97
    Height = 40
    Caption = 'Line width'
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      '1'
      '2')
    TabOrder = 11
    OnClick = RadioGroup1Click
  end
end
