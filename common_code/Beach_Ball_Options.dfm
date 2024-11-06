object BeachBallForm: TBeachBallForm
  Left = 574
  Top = 381
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Earthquake Focal Mechanisms'
  ClientHeight = 350
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 239
    Top = 48
    Width = 39
    Height = 13
    Caption = 'Smallest'
  end
  object Label2: TLabel
    Left = 239
    Top = 72
    Width = 38
    Height = 13
    Caption = 'Largest '
  end
  object Label3: TLabel
    Left = 238
    Top = 21
    Width = 11
    Height = 13
    Caption = 'All'
  end
  object Label4: TLabel
    Left = 119
    Top = 277
    Width = 60
    Height = 13
    Caption = 'Pixel size (m)'
  end
  object Label5: TLabel
    Left = 263
    Top = 8
    Width = 33
    Height = 13
    Caption = 'Radius'
  end
  object Label6: TLabel
    Left = 192
    Top = 91
    Width = 17
    Height = 13
    Caption = 'Min'
  end
  object Label7: TLabel
    Left = 266
    Top = 91
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label8: TLabel
    Left = 304
    Top = 155
    Width = 64
    Height = 13
    Caption = 'M for coloring'
  end
  object Label9: TLabel
    Left = 302
    Top = 113
    Width = 55
    Height = 13
    Caption = 'M for radius'
  end
  object Label10: TLabel
    Left = 304
    Top = 200
    Width = 84
    Height = 13
    Caption = 'Depth for coloring'
  end
  object Label11: TLabel
    Left = 296
    Top = 277
    Width = 38
    Height = 13
    Caption = 'Label11'
  end
  object Label12: TLabel
    Left = 238
    Top = 317
    Width = 60
    Height = 13
    Caption = 'Max to show'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 145
    Height = 89
    Caption = 'Size'
    Items.Strings = (
      'Constant'
      'Magnitude (Mb)'
      'Magnitude (Ms)')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object Edit1: TEdit
    Left = 159
    Top = 16
    Width = 73
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 159
    Top = 40
    Width = 73
    Height = 21
    TabOrder = 2
  end
  object Edit3: TEdit
    Left = 159
    Top = 64
    Width = 73
    Height = 21
    TabOrder = 3
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 311
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = OKBtnClick
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 119
    Top = 313
    Width = 77
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = BitBtn3Click
    IsControl = True
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 232
    Width = 300
    Height = 39
    Caption = 'Map plots'
    Columns = 3
    Items.Strings = (
      'Color symbols'
      'Beach balls'
      'Scale switch')
    TabOrder = 6
    OnClick = RadioGroup2Click
  end
  object Edit4: TEdit
    Left = 202
    Top = 277
    Width = 67
    Height = 21
    TabOrder = 7
    Text = 'Edit4'
  end
  object Edit5: TEdit
    Left = 176
    Top = 110
    Width = 49
    Height = 21
    TabOrder = 8
  end
  object Edit6: TEdit
    Left = 247
    Top = 110
    Width = 49
    Height = 21
    TabOrder = 9
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 113
    Width = 145
    Height = 105
    Caption = 'Color'
    Items.Strings = (
      'Constant'
      'Magnitude (Mb)'
      'Magnitude (Ms)'
      'Depth')
    TabOrder = 10
    OnClick = RadioGroup3Click
  end
  object Edit7: TEdit
    Left = 176
    Top = 152
    Width = 49
    Height = 21
    TabOrder = 11
  end
  object Edit8: TEdit
    Left = 249
    Top = 152
    Width = 49
    Height = 21
    TabOrder = 12
  end
  object Edit11: TEdit
    Left = 176
    Top = 197
    Width = 49
    Height = 21
    TabOrder = 13
  end
  object Edit12: TEdit
    Left = 247
    Top = 197
    Width = 49
    Height = 21
    TabOrder = 14
  end
  object Edit9: TEdit
    Left = 315
    Top = 314
    Width = 73
    Height = 21
    TabOrder = 15
    Text = 'Edit9'
  end
end
