object SpeedDistanceForm: TSpeedDistanceForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Speed/heading options'
  ClientHeight = 203
  ClientWidth = 188
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object CheckBox1: TCheckBox
    Left = 16
    Top = 16
    Width = 113
    Height = 17
    Caption = 'Compute azimuths'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 39
    Width = 129
    Height = 17
    Caption = 'Compute speeds'
    TabOrder = 1
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 62
    Width = 161
    Height = 17
    Caption = 'Compute interval distances'
    TabOrder = 2
  end
  object CheckBox4: TCheckBox
    Left = 16
    Top = 85
    Width = 169
    Height = 17
    Caption = 'Compute cumulative distances'
    TabOrder = 3
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 128
    Width = 137
    Height = 41
    Caption = 'Units'
    Columns = 2
    Items.Strings = (
      'meters'
      'km')
    TabOrder = 4
  end
  object OKBtn: TBitBtn
    Left = 36
    Top = 175
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = OKBtnClick
    IsControl = True
  end
  object CheckBox5: TCheckBox
    Left = 16
    Top = 105
    Width = 145
    Height = 17
    Caption = 'Compute 3D Distances'
    TabOrder = 6
  end
end
