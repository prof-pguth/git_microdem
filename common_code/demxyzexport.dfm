object XYZformatform: TXYZformatform
  Left = 643
  Top = 356
  BorderStyle = bsDialog
  Caption = 'ASCII export options'
  ClientHeight = 275
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 166
    Width = 24
    Height = 13
    Caption = 'UTM'
  end
  object Label2: TLabel
    Left = 8
    Top = 190
    Width = 40
    Height = 13
    Caption = 'Lat/long'
  end
  object Label3: TLabel
    Left = 9
    Top = 214
    Width = 41
    Height = 13
    Caption = 'Z values'
  end
  object Label4: TLabel
    Left = 56
    Top = 144
    Width = 43
    Height = 13
    Caption = 'Decimals'
  end
  object Thin: TLabel
    Left = 152
    Top = 176
    Width = 21
    Height = 13
    Caption = 'Thin'
  end
  object Label5: TLabel
    Left = 144
    Top = 200
    Width = 50
    Height = 13
    Caption = 'UTM zone'
  end
  object Label6: TLabel
    Left = 192
    Top = 224
    Width = 32
    Height = 13
    Caption = 'Label6'
  end
  object RadioGroup1: TRadioGroup
    Left = 128
    Top = 8
    Width = 137
    Height = 105
    Caption = 'Z format'
    Items.Strings = (
      'default'
      'Meters (integer)'
      'Meters (float)'
      'Decimeters (integer)')
    TabOrder = 0
  end
  object OKBtn: TBitBtn
    Left = 128
    Top = 249
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 8
    Width = 105
    Height = 130
    Caption = 'XY Format'
    ItemIndex = 0
    Items.Strings = (
      'UTM '
      'Lat/Long '
      'Long/Lat'
      'MGRS '
      'Generate'
      'Recenter XYZ')
    TabOrder = 2
    OnClick = RadioGroup2Click
  end
  object Edit1: TEdit
    Left = 56
    Top = 163
    Width = 49
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object Edit2: TEdit
    Left = 56
    Top = 187
    Width = 49
    Height = 21
    TabOrder = 4
    Text = '6'
  end
  object Edit3: TEdit
    Left = 56
    Top = 214
    Width = 49
    Height = 21
    TabOrder = 5
    Text = '2'
  end
  object HelpBtn: TBitBtn
    Left = 211
    Top = 249
    Width = 76
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 144
    Top = 143
    Width = 97
    Height = 17
    Caption = 'Header row'
    TabOrder = 7
  end
  object Edit4: TEdit
    Left = 183
    Top = 173
    Width = 41
    Height = 21
    TabOrder = 8
    Text = '1'
  end
  object Edit5: TEdit
    Left = 200
    Top = 200
    Width = 65
    Height = 21
    TabOrder = 9
    Text = 'Edit5'
    OnChange = Edit5Change
  end
end
