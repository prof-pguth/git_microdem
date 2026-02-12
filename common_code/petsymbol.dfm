object PickSymbolForm: TPickSymbolForm
  Left = 693
  Top = 409
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Plotting Symbol Selection'
  ClientHeight = 169
  ClientWidth = 203
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefault
  OnCreate = FormCreate
  TextHeight = 13
  object Image1: TImage
    Left = 16
    Top = 24
    Width = 40
    Height = 40
  end
  object Symbol: TLabel
    Left = 110
    Top = 39
    Width = 34
    Height = 13
    Caption = 'Symbol'
  end
  object Size: TLabel
    Left = 94
    Top = 69
    Width = 20
    Height = 13
    Caption = 'Size'
  end
  object SpeedButton1: TSpeedButton
    Left = 86
    Top = 39
    Width = 18
    Height = 18
    Caption = '>'
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 62
    Top = 39
    Width = 18
    Height = 18
    Caption = '<'
    OnClick = SpeedButton2Click
  end
  object Button1: TButton
    Left = 72
    Top = 8
    Width = 33
    Height = 25
    Caption = 'Color'
    TabOrder = 0
    OnClick = Button1Click
  end
  object UpDown2: TUpDown
    Left = 72
    Top = 69
    Width = 16
    Height = 24
    Min = 1
    Max = 12
    Position = 1
    TabOrder = 1
    OnClick = UpDown2Click
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 128
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 103
    Top = 128
    Width = 77
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = HelpBtnClick
    IsControl = True
  end
end
