object CompareDSM_DTMform: TCompareDSM_DTMform
  Left = 0
  Top = 0
  Caption = 'DSM DTM compare'
  ClientHeight = 642
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 15
  object Label1: TLabel
    Left = 469
    Top = 614
    Width = 146
    Height = 15
    Caption = 'Max in graph legend entries'
  end
  object Label2: TLabel
    Left = 16
    Top = 136
    Width = 70
    Height = 15
    Caption = 'Comparisons'
  end
  object Label3: TLabel
    Left = 16
    Top = 328
    Width = 83
    Height = 15
    Caption = 'Multiple criteria'
  end
  object Label4: TLabel
    Left = 8
    Top = 546
    Width = 79
    Height = 15
    Caption = 'Single criterion'
  end
  object BitBtn1: TBitBtn
    Left = 170
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 88
    Width = 97
    Height = 25
    Caption = 'Load HRDEMs'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 89
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 24
    Width = 75
    Height = 25
    Caption = 'First'
    TabOrder = 3
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 251
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Last'
    TabOrder = 4
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 120
    Top = 88
    Width = 97
    Height = 25
    Caption = 'Load 1" DEMs'
    TabOrder = 5
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 223
    Top = 88
    Width = 97
    Height = 25
    Caption = 'SlopeMaps'
    TabOrder = 6
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 183
    Top = 400
    Width = 250
    Height = 25
    Caption = 'Grid for median or mean, multiple criteria'
    TabOrder = 7
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 183
    Top = 431
    Width = 250
    Height = 25
    Caption = 'Grid, all tiles for single criterion'
    TabOrder = 8
    OnClick = BitBtn9Click
  end
  object RadioGroup2: TRadioGroup
    Left = 344
    Top = 24
    Width = 271
    Height = 65
    Caption = 'Multiple tiles representation'
    Items.Strings = (
      'Mean'
      'Median')
    TabOrder = 9
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 342
    Top = 95
    Width = 273
    Height = 81
    Caption = 'Mulitple graph scaling'
    Items.Strings = (
      'Common (emphasize comparing graphs)'
      'Different (emphasize comparing each graph)')
    TabOrder = 10
    OnClick = RadioGroup3Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 156
    Width = 169
    Height = 138
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object Memo2: TMemo
    Left = 8
    Top = 344
    Width = 145
    Height = 161
    ScrollBars = ssVertical
    TabOrder = 12
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 567
    Width = 145
    Height = 23
    TabOrder = 13
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
  end
  object GroupBox3: TGroupBox
    Left = 344
    Top = 216
    Width = 161
    Height = 92
    Caption = 'Mixed Filters for Grids'
    TabOrder = 14
    object ComboBox6: TComboBox
      Left = 11
      Top = 26
      Width = 126
      Height = 23
      TabOrder = 0
      OnChange = ComboBox6Change
    end
    object ComboBox7: TComboBox
      Left = 11
      Top = 55
      Width = 126
      Height = 23
      TabOrder = 1
      OnChange = ComboBox7Change
    end
  end
  object BitBtn10: TBitBtn
    Left = 183
    Top = 369
    Width = 234
    Height = 25
    Caption = 'Grid multiple pair comparisons'
    TabOrder = 15
    OnClick = BitBtn10Click
  end
  object BitBtn11: TBitBtn
    Left = 469
    Top = 580
    Width = 238
    Height = 25
    Caption = 'BitBtn11'
    TabOrder = 16
    OnClick = BitBtn11Click
  end
  object BitBtn38: TBitBtn
    Left = 480
    Top = 443
    Width = 81
    Height = 29
    Caption = 'Save defaults'
    TabOrder = 17
    OnClick = BitBtn38Click
  end
  object BitBtn12: TBitBtn
    Left = 480
    Top = 412
    Width = 81
    Height = 25
    Caption = 'Close images'
    TabOrder = 18
    OnClick = BitBtn12Click
  end
  object BitBtn13: TBitBtn
    Left = 480
    Top = 381
    Width = 81
    Height = 25
    Caption = 'Close graphs'
    TabOrder = 19
    OnClick = BitBtn13Click
  end
  object Edit1: TEdit
    Left = 632
    Top = 611
    Width = 86
    Height = 23
    TabOrder = 20
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object BitBtn14: TBitBtn
    Left = 183
    Top = 241
    Width = 155
    Height = 25
    Caption = 'Multiple terrain scatterplots'
    TabOrder = 21
    OnClick = BitBtn14Click
  end
  object Memo3: TMemo
    Left = 632
    Top = 216
    Width = 121
    Height = 185
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 22
  end
  object BitBtn15: TBitBtn
    Left = 183
    Top = 462
    Width = 250
    Height = 25
    Caption = 'All  tiles grid, slope vs DSM spacing'
    TabOrder = 23
    OnClick = BitBtn15Click
  end
  object BitBtn16: TBitBtn
    Left = 183
    Top = 493
    Width = 250
    Height = 25
    Caption = 'All  tiles grid, slope vs DTM spacing'
    TabOrder = 24
    OnClick = BitBtn16Click
  end
  object BitBtn18: TBitBtn
    Left = 63
    Top = 511
    Width = 75
    Height = 25
    Caption = 'Reload'
    TabOrder = 25
    OnClick = BitBtn18Click
  end
  object BitBtn17: TBitBtn
    Left = 183
    Top = 283
    Width = 155
    Height = 25
    Caption = 'Single terrain scatterplot'
    TabOrder = 26
    OnClick = BitBtn17Click
  end
  object BitBtn19: TBitBtn
    Left = 78
    Top = 300
    Width = 75
    Height = 25
    Caption = 'Reload'
    TabOrder = 27
    OnClick = BitBtn19Click
  end
  object BitBtn20: TBitBtn
    Left = 184
    Top = 528
    Width = 249
    Height = 25
    Caption = 'All tiles grid, criterion vs DEM spacing'
    TabOrder = 28
    OnClick = BitBtn20Click
  end
end
