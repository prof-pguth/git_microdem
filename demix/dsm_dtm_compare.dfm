object CompareDSM_DTMform: TCompareDSM_DTMform
  Left = 0
  Top = 0
  Caption = 'DSM DTM compare'
  ClientHeight = 547
  ClientWidth = 624
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
    Left = 40
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 192
    Top = 168
    Width = 97
    Height = 25
    Caption = 'Load HRDEMs'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 296
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
    Left = 40
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 48
    Top = 199
    Width = 75
    Height = 25
    Caption = 'First'
    TabOrder = 4
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 48
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Last'
    TabOrder = 5
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 192
    Top = 199
    Width = 97
    Height = 25
    Caption = 'Load 1" DEMs'
    TabOrder = 6
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 192
    Top = 256
    Width = 97
    Height = 25
    Caption = 'SlopeMaps'
    TabOrder = 7
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 352
    Top = 176
    Width = 145
    Height = 25
    Caption = 'Two factor FUV grid'
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
    Left = 40
    Top = 344
    Width = 113
    Height = 177
    Lines.Strings = (
      'Memo1')
    TabOrder = 12
  end
  object MainMenu1: TMainMenu
    Left = 304
    Top = 280
  end
end
