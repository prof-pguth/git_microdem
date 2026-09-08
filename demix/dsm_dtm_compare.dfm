object CompareDSM_DTMform: TCompareDSM_DTMform
  Left = 0
  Top = 0
  Caption = 'DSM DTM compare'
  ClientHeight = 729
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 326
    Top = 689
    Width = 146
    Height = 15
    Caption = 'Max in graph legend entries'
  end
  object Label2: TLabel
    Left = 16
    Top = 75
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
    Left = 20
    Top = 614
    Width = 79
    Height = 15
    Caption = 'Single criterion'
  end
  object BitBtn8: TBitBtn
    Left = 186
    Top = 376
    Width = 250
    Height = 25
    Caption = 'Grid for median or mean, multiple criteria'
    TabOrder = 0
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 182
    Top = 635
    Width = 250
    Height = 25
    Caption = 'Grid, all tiles for 1 criterion'
    TabOrder = 1
    OnClick = BitBtn9Click
  end
  object RadioGroup2: TRadioGroup
    Left = 416
    Top = 88
    Width = 335
    Height = 43
    Caption = 'Multiple tiles representation'
    Columns = 2
    Items.Strings = (
      'Mean'
      'Median')
    TabOrder = 2
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 416
    Top = 8
    Width = 335
    Height = 74
    Caption = 'Mulitple graph scaling'
    Items.Strings = (
      'All Common (emphasize comparing graphs)'
      'Each row common'
      'Every graph different (emphasize comparing each graph)')
    TabOrder = 3
    OnClick = RadioGroup3Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 95
    Width = 169
    Height = 138
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Memo2: TMemo
    Left = 8
    Top = 344
    Width = 145
    Height = 161
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object ComboBox1: TComboBox
    Left = 20
    Top = 635
    Width = 145
    Height = 23
    TabOrder = 6
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
  end
  object GroupBox3: TGroupBox
    Left = 197
    Top = 39
    Width = 213
    Height = 92
    Caption = 'Mixed Filters for Grids'
    TabOrder = 7
    object ComboBox6: TComboBox
      Left = 11
      Top = 26
      Width = 166
      Height = 23
      TabOrder = 0
      OnChange = ComboBox6Change
    end
    object ComboBox7: TComboBox
      Left = 11
      Top = 55
      Width = 166
      Height = 23
      TabOrder = 1
      OnChange = ComboBox7Change
    end
  end
  object BitBtn10: TBitBtn
    Left = 186
    Top = 345
    Width = 234
    Height = 25
    Caption = 'Grid multiple pair comparisons, 1 criterion'
    TabOrder = 8
    OnClick = BitBtn10Click
  end
  object BitBtn11: TBitBtn
    Left = 197
    Top = 8
    Width = 213
    Height = 25
    Caption = 'BitBtn11'
    TabOrder = 9
    OnClick = BitBtn11Click
  end
  object BitBtn38: TBitBtn
    Left = 216
    Top = 683
    Width = 81
    Height = 29
    Caption = 'Save defaults'
    TabOrder = 10
    OnClick = BitBtn38Click
  end
  object BitBtn12: TBitBtn
    Left = 112
    Top = 685
    Width = 81
    Height = 25
    Caption = 'Close images'
    TabOrder = 11
    OnClick = BitBtn12Click
  end
  object BitBtn13: TBitBtn
    Left = 8
    Top = 685
    Width = 81
    Height = 25
    Caption = 'Close graphs'
    TabOrder = 12
    OnClick = BitBtn13Click
  end
  object Edit1: TEdit
    Left = 478
    Top = 686
    Width = 86
    Height = 23
    TabOrder = 13
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object BitBtn14: TBitBtn
    Left = 183
    Top = 241
    Width = 155
    Height = 25
    Caption = 'Multiple terrain scatterplots'
    TabOrder = 14
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
    TabOrder = 15
  end
  object BitBtn15: TBitBtn
    Left = 183
    Top = 446
    Width = 250
    Height = 25
    Caption = 'All  tiles grid, slope vs DSM spacing'
    TabOrder = 16
    OnClick = BitBtn15Click
  end
  object BitBtn16: TBitBtn
    Left = 186
    Top = 477
    Width = 250
    Height = 25
    Caption = 'All  tiles grid, slope vs DTM spacing'
    TabOrder = 17
    OnClick = BitBtn16Click
  end
  object BitBtn18: TBitBtn
    Left = 63
    Top = 511
    Width = 75
    Height = 25
    Caption = 'Reload'
    TabOrder = 18
    OnClick = BitBtn18Click
  end
  object BitBtn17: TBitBtn
    Left = 183
    Top = 283
    Width = 155
    Height = 25
    Caption = 'Single terrain scatterplot'
    TabOrder = 19
    OnClick = BitBtn17Click
  end
  object BitBtn19: TBitBtn
    Left = 78
    Top = 239
    Width = 75
    Height = 25
    Caption = 'Reload'
    TabOrder = 20
    OnClick = BitBtn19Click
  end
  object BitBtn20: TBitBtn
    Left = 183
    Top = 555
    Width = 249
    Height = 25
    Caption = 'All tiles grid, criterion vs DEM spacing'
    TabOrder = 21
    OnClick = BitBtn20Click
  end
  object BitBtn2: TBitBtn
    Left = 185
    Top = 524
    Width = 250
    Height = 25
    Caption = 'All  tiles grid, slope vs GDEM spacing'
    TabOrder = 22
    OnClick = BitBtn2Click
  end
  object BitBtn6: TBitBtn
    Left = 186
    Top = 604
    Width = 250
    Height = 25
    Caption = 'Grid for median or mean, 1 criterion'
    TabOrder = 23
    OnClick = BitBtn6Click
  end
end
