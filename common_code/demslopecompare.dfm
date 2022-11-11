object SlopeCompareOptions: TSlopeCompareOptions
  Left = 327
  Top = 276
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Slope Compare Options'
  ClientHeight = 236
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 112
    Top = 13
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 112
    Top = 44
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 83
    Height = 25
    Caption = 'Slope method 1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 39
    Width = 83
    Height = 25
    Caption = 'Slope method 2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 88
    Width = 129
    Height = 65
    Caption = 'Difference map'
    ItemIndex = 0
    Items.Strings = (
      'Absolute difference'
      'Signed value')
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 160
    Width = 97
    Height = 17
    Caption = 'Difference graph'
    TabOrder = 3
  end
  object BitBtn1: TBitBtn
    Left = 40
    Top = 212
    Width = 73
    Height = 25
    Caption = 'OK'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      555555555555555555555555555555555555555555FF55555555555559055555
      55555555577FF5555555555599905555555555557777F5555555555599905555
      555555557777FF5555555559999905555555555777777F555555559999990555
      5555557777777FF5555557990599905555555777757777F55555790555599055
      55557775555777FF5555555555599905555555555557777F5555555555559905
      555555555555777FF5555555555559905555555555555777FF55555555555579
      05555555555555777FF5555555555557905555555555555777FF555555555555
      5990555555555555577755555555555555555555555555555555}
    NumGlyphs = 2
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 183
    Width = 161
    Height = 17
    Caption = 'Method statistical summary'
    TabOrder = 5
  end
end
