object CompareDSM_DTMform: TCompareDSM_DTMform
  Left = 0
  Top = 0
  Caption = 'DSM DTM compare'
  ClientHeight = 642
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
  object Label1: TLabel
    Left = 208
    Top = 576
    Width = 146
    Height = 15
    Caption = 'Max in graph legend entries'
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
    Left = 159
    Top = 392
    Width = 250
    Height = 25
    Caption = 'Grid for median or mean, multiple criteria'
    TabOrder = 7
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 199
    Top = 535
    Width = 250
    Height = 25
    Caption = 'All tiles for single criterion'
    TabOrder = 8
    OnClick = BitBtn9Click
  end
  object RadioGroup2: TRadioGroup
    Left = 344
    Top = 24
    Width = 185
    Height = 81
    Caption = 'Multiple tiles representation'
    Items.Strings = (
      'Mean'
      'Median')
    TabOrder = 9
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 344
    Top = 111
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
    Top = 119
    Width = 169
    Height = 175
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object Memo2: TMemo
    Left = 32
    Top = 320
    Width = 121
    Height = 185
    Lines.Strings = (
      'Memo2')
    ScrollBars = ssVertical
    TabOrder = 12
  end
  object ComboBox1: TComboBox
    Left = 32
    Top = 536
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
    Caption = 'Mixed Filters'
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
    Left = 199
    Top = 185
    Width = 139
    Height = 25
    Caption = 'Compare multiple pairs'
    TabOrder = 15
    OnClick = BitBtn10Click
  end
  object BitBtn11: TBitBtn
    Left = 355
    Top = 328
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
    Left = 371
    Top = 573
    Width = 86
    Height = 23
    TabOrder = 20
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object MainMenu1: TMainMenu
    Left = 568
    Top = 16
  end
end
