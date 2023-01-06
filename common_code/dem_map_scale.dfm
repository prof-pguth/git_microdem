object MapScaledForm: TMapScaledForm
  Left = 532
  Top = 369
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Scaled map'
  ClientHeight = 190
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  ShowHint = True
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 40
    Width = 42
    Height = 13
    Caption = 'Scale, 1:'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 21
    Height = 13
    Caption = 'DPI:'
  end
  object Label3: TLabel
    Left = 16
    Top = 80
    Width = 87
    Height = 13
    Caption = 'Map width (pixels):'
  end
  object Label4: TLabel
    Left = 16
    Top = 104
    Width = 87
    Height = 13
    Caption = 'Map width (pixels):'
  end
  object Label5: TLabel
    Left = 24
    Top = 128
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object SpeedButton1: TSpeedButton
    Left = 16
    Top = 8
    Width = 23
    Height = 22
    Hint = 'Enter map center point'
    Caption = '+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object Label6: TLabel
    Left = 56
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Label6'
  end
  object SpeedButton2: TSpeedButton
    Left = 208
    Top = 32
    Width = 23
    Height = 22
    Hint = 'Current map center'
    Caption = '+'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton2Click
  end
  object ComboBox1: TComboBox
    Left = 64
    Top = 32
    Width = 129
    Height = 21
    AutoComplete = False
    TabOrder = 0
    Text = '50,000'
    OnChange = ComboBox1Change
    Items.Strings = (
      '24,000'
      '25,000'
      '50,000'
      '100,000'
      '250,000'
      '500,000'
      '1,000,000')
  end
  object Edit1: TEdit
    Left = 72
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '100'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 128
    Top = 80
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '600'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 128
    Top = 104
    Width = 65
    Height = 21
    TabOrder = 3
    Text = '500'
    OnChange = Edit3Change
  end
  object HelpBtn: TBitBtn
    Left = 155
    Top = 155
    Width = 76
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = HelpBtnClick
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 80
    Top = 155
    Width = 73
    Height = 27
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
    TabOrder = 5
    OnClick = BitBtn1Click
  end
end
