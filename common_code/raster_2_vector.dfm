object rast_2_vect_f: Trast_2_vect_f
  Left = 0
  Top = 0
  Caption = 'Raster to vector options'
  ClientHeight = 188
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 123
    Height = 13
    Caption = 'Suppress speckles (def 2)'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 95
    Height = 13
    Caption = 'Black level (def 0.5)'
  end
  object Label3: TLabel
    Left = 16
    Top = 64
    Width = 117
    Height = 13
    Caption = 'Corner threshold (def 1)'
  end
  object Edit1: TEdit
    Left = 145
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Invert image'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object BitBtn1: TBitBtn
    Left = 96
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object Edit2: TEdit
    Left = 144
    Top = 32
    Width = 74
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object RadioGroup1: TRadioGroup
    Left = 200
    Top = 88
    Width = 97
    Height = 81
    Caption = 'Border'
    ItemIndex = 1
    Items.Strings = (
      'None'
      'White'
      'Black')
    TabOrder = 4
  end
  object Edit3: TEdit
    Left = 145
    Top = 61
    Width = 73
    Height = 21
    TabOrder = 5
    Text = 'Edit3'
    OnChange = Edit3Change
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 112
    Width = 97
    Height = 17
    Caption = 'Grayscale bitmap'
    TabOrder = 6
  end
end
