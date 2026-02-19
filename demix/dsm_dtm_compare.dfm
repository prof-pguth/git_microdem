object CompareDSM_DTMform: TCompareDSM_DTMform
  Left = 0
  Top = 0
  Caption = 'DSM DTM compare'
  ClientHeight = 545
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Menu = MainMenu1
  TextHeight = 15
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
  object RadioGroup1: TRadioGroup
    Left = 360
    Top = 24
    Width = 185
    Height = 105
    Caption = 'DSM-DTM LSP difference'
    ItemIndex = 1
    Items.Strings = (
      'ELVD_FUV'
      'SLPD_FUV'
      'RRID_FUV')
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object BitBtn3: TBitBtn
    Left = 89
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 24
    Width = 75
    Height = 25
    Caption = 'First'
    TabOrder = 4
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 251
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Last'
    TabOrder = 5
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 120
    Top = 88
    Width = 97
    Height = 25
    Caption = 'Load 1" DEMs'
    TabOrder = 6
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 223
    Top = 88
    Width = 97
    Height = 25
    Caption = 'SlopeMaps'
    TabOrder = 7
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 159
    Top = 176
    Width = 250
    Height = 25
    Caption = 'Grid for median or mean for all criteria'
    TabOrder = 8
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 352
    Top = 216
    Width = 145
    Height = 25
    Caption = 'All tiles'
    TabOrder = 9
    OnClick = BitBtn9Click
  end
  object RadioGroup2: TRadioGroup
    Left = 312
    Top = 272
    Width = 185
    Height = 81
    Caption = 'Multiple tiles representation'
    Items.Strings = (
      'Mean'
      'Median')
    TabOrder = 10
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 312
    Top = 368
    Width = 233
    Height = 81
    Caption = 'Mulitple graph scaling'
    Items.Strings = (
      'Common (emphasize comparing graphs)'
      'Different (emphasize comparing each graph)')
    TabOrder = 11
    OnClick = RadioGroup3Click
  end
  object Memo1: TMemo
    Left = 32
    Top = 344
    Width = 121
    Height = 177
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 12
  end
  object BitBtn10: TBitBtn
    Left = 159
    Top = 135
    Width = 250
    Height = 25
    Caption = 'Gird for all DEM pairs for one criterion'
    TabOrder = 13
    OnClick = BitBtn10Click
  end
  object Memo2: TMemo
    Left = 32
    Top = 136
    Width = 121
    Height = 185
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssVertical
    TabOrder = 14
  end
  object ComboBox1: TComboBox
    Left = 208
    Top = 480
    Width = 145
    Height = 23
    TabOrder = 15
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
  end
  object MainMenu1: TMainMenu
    Left = 304
    Top = 280
  end
end
