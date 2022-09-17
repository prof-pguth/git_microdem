object PrintPreviewForm: TPrintPreviewForm
  Left = 402
  Top = 257
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Map Print Preview'
  ClientHeight = 422
  ClientWidth = 519
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 305
    Height = 305
    Stretch = True
  end
  object Image2: TImage
    Left = 0
    Top = 0
    Width = 105
    Height = 105
  end
  object Label1: TLabel
    Left = 395
    Top = 32
    Width = 35
    Height = 13
    Caption = ' Label1'
  end
  object Label2: TLabel
    Left = 400
    Top = 48
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object BitBtn1: TBitBtn
    Left = 392
    Top = 0
    Width = 105
    Height = 25
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn4: TBitBtn
    Left = 392
    Top = 245
    Width = 105
    Height = 27
    Caption = 'OK (Close)'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object BitBtn2: TBitBtn
    Left = 392
    Top = 88
    Width = 105
    Height = 25
    Hint = 'Print using Windows Driver'
    Caption = 'Print to scale'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      00033FFFFFFFFFFFFFFF0888888888888880777777777777777F088888888888
      8880777777777777777F0000000000000000FFFFFFFFFFFFFFFF0F8F8F8F8F8F
      8F80777777777777777F08F8F8F8F8F8F9F0777777777777777F0F8F8F8F8F8F
      8F807777777777777F7F0000000000000000777777777777777F3330FFFFFFFF
      03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
      03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
      33333337F3FF7F3733333330F08F0F0333333337F7737F7333333330FFFF0033
      33333337FFFF7733333333300000033333333337777773333333}
    NumGlyphs = 2
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object HelpBtn: TBitBtn
    Left = 393
    Top = 218
    Width = 104
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = HelpBtnClick
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 392
    Top = 64
    Width = 105
    Height = 25
    Caption = 'Printer setup'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333300333
      33333333333773FF333333333330F0033333333333373773FF333333330FFFF0
      03333333337F333773FF3333330FFFFFF003333333733FF33773333330FF00FF
      FF80333337F3773F3337333330FFFF0FFFF03FFFF7FFF3733F3700000000FFFF
      0FF0777777773FF373370000000000FFFFF07FFFFFF377FFF3370CCCCC000000
      FF037777773337773F7300CCC000003300307F77733337F37737000C00000033
      33307F373333F7F333370000007B703333307FFFF337F7F33337099900BBB033
      33307777F37777FF33370999007B700333037777F3373773FF73099900000030
      00337777FFFFF7F7773300000000003333337777777777333333}
    NumGlyphs = 2
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object BitBtn5: TBitBtn
    Left = 392
    Top = 112
    Width = 105
    Height = 25
    Hint = 'Print to large HP plotters'
    Caption = 'HP RTL Print '
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      00033FFFFFFFFFFFFFFF0888888888888880777777777777777F088888888888
      8880777777777777777F0000000000000000FFFFFFFFFFFFFFFF0F8F8F8F8F8F
      8F80777777777777777F08F8F8F8F8F8F9F0777777777777777F0F8F8F8F8F8F
      8F807777777777777F7F0000000000000000777777777777777F3330FFFFFFFF
      03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
      03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
      33333337F3FF7F3733333330F08F0F0333333337F7737F7333333330FFFF0033
      33333337FFFF7733333333300000033333333337777773333333}
    NumGlyphs = 2
    TabOrder = 5
  end
  object RadioGroup1: TRadioGroup
    Left = 400
    Top = 168
    Width = 97
    Height = 49
    ItemIndex = 1
    Items.Strings = (
      'Landscape '
      'Portrait')
    TabOrder = 6
  end
  object BitBtn6: TBitBtn
    Left = 392
    Top = 136
    Width = 105
    Height = 25
    Hint = 'Print to large HP plotters'
    Caption = 'Quick Print '
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
      00033FFFFFFFFFFFFFFF0888888888888880777777777777777F088888888888
      8880777777777777777F0000000000000000FFFFFFFFFFFFFFFF0F8F8F8F8F8F
      8F80777777777777777F08F8F8F8F8F8F9F0777777777777777F0F8F8F8F8F8F
      8F807777777777777F7F0000000000000000777777777777777F3330FFFFFFFF
      03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
      03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
      33333337F3FF7F3733333330F08F0F0333333337F7737F7333333330FFFF0033
      33333337FFFF7733333333300000033333333337777773333333}
    NumGlyphs = 2
    TabOrder = 7
  end
  object PrintDialog1: TPrintDialog
    Left = 320
    Top = 280
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 408
    Top = 280
  end
end
