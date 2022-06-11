object NEOutlineForm: TNEOutlineForm
  Left = 431
  Top = 624
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Smart Natural Earth Outlines'
  ClientHeight = 209
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 209
    Top = 35
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 209
    Top = 60
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 209
    Top = 87
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 216
    Top = 136
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object Label4: TLabel
    Left = 216
    Top = 110
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 22
    Top = 8
    Width = 123
    Height = 13
    Caption = 'Pixel size (m) to stop using'
  end
  object OKBtn: TBitBtn
    Left = 216
    Top = 173
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object Edit2: TEdit
    Left = 17
    Top = 24
    Width = 81
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 17
    Top = 51
    Width = 81
    Height = 21
    TabOrder = 2
    OnChange = Edit3Change
  end
  object BitBtn5: TBitBtn
    Left = 299
    Top = 173
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = BitBtn5Click
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 105
    Top = 24
    Width = 74
    Height = 25
    Caption = 'Small scale'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object BitBtn8: TBitBtn
    Left = 104
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Medium scale'
    TabOrder = 5
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 104
    Top = 82
    Width = 75
    Height = 25
    Caption = 'Large scale'
    TabOrder = 6
    OnClick = BitBtn9Click
  end
  object GroupBox1: TGroupBox
    Left = 17
    Top = 136
    Width = 136
    Height = 64
    Caption = 'Automatically show on'
    TabOrder = 7
    object CheckBox1: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'DEM Maps'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Imagery'
      TabOrder = 1
    end
  end
  object Edit1: TEdit
    Left = 17
    Top = 86
    Width = 81
    Height = 21
    TabOrder = 8
    OnChange = Edit1Change
  end
  object CheckBox3: TCheckBox
    Left = 33
    Top = 113
    Width = 160
    Height = 17
    Caption = 'Variable scale selection'
    TabOrder = 9
    OnClick = CheckBox3Click
  end
  object BitBtn2: TBitBtn
    Left = 267
    Top = 136
    Width = 109
    Height = 25
    Caption = 'Force NE download'
    TabOrder = 10
    OnClick = BitBtn2Click
  end
  object BitBtn16: TBitBtn
    Left = 185
    Top = 30
    Width = 18
    Height = 18
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADA00000000000000000FF0FEF4FEF4FEF00FF0EFE4EFE4EFE00FF044444444
      44400FF0EFE4EFE4EFE00000FEF4FEF4FEF00FF04444444444400FF0FEF4FEF4
      FEF00FF0EFE4EFE4EFE000000000000000000FF0FFFFF0FFFFF00FF0FFFFF0FF
      FFF00000000000000000DADADADADADADADAADADADADADADADAD}
    TabOrder = 11
    OnClick = BitBtn16Click
  end
  object BitBtn3: TBitBtn
    Left = 185
    Top = 54
    Width = 18
    Height = 18
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADA00000000000000000FF0FEF4FEF4FEF00FF0EFE4EFE4EFE00FF044444444
      44400FF0EFE4EFE4EFE00000FEF4FEF4FEF00FF04444444444400FF0FEF4FEF4
      FEF00FF0EFE4EFE4EFE000000000000000000FF0FFFFF0FFFFF00FF0FFFFF0FF
      FFF00000000000000000DADADADADADADADAADADADADADADADAD}
    TabOrder = 12
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 185
    Top = 82
    Width = 18
    Height = 18
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADA00000000000000000FF0FEF4FEF4FEF00FF0EFE4EFE4EFE00FF044444444
      44400FF0EFE4EFE4EFE00000FEF4FEF4FEF00FF04444444444400FF0FEF4FEF4
      FEF00FF0EFE4EFE4EFE000000000000000000FF0FFFFF0FFFFF00FF0FFFFF0FF
      FFF00000000000000000DADADADADADADADAADADADADADADADAD}
    TabOrder = 13
    OnClick = BitBtn4Click
  end
end
