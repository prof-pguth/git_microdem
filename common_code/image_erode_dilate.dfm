object DilateErodeForm: TDilateErodeForm
  Left = 0
  Top = 0
  Caption = 'Image erode/dilate'
  ClientHeight = 188
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object RedrawSpeedButton12: TSpeedButton
    Left = 255
    Top = 111
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
  object Label1: TLabel
    Left = 298
    Top = 15
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label2: TLabel
    Left = 224
    Top = 15
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object SpeedButton22: TSpeedButton
    Left = 162
    Top = 8
    Width = 25
    Height = 25
    Hint = 'No zoom (1:1 view)'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33033333333333333F8F3333333333333000333333333333F888333333333333
      000333333333333F888333333333333000333333333333F88833333333333300
      033333333FFF3F888333333800083B803333333F8883F8883333330FFFFF00B3
      3333338833388883333330FFFFFFF033333338F3333338F333338FFFF0FFFF83
      333338333333383F33330FFFF0FFFF0333338F333333338F33330FFFF0FFFF03
      33338F333333338F33330FFFF0FFFF03333383F33333338333338FFF00FFFF83
      333338F3333338F3333330FFF0FFF0333333383FF333F8333333330FFFFF0333
      333333883FF88333333333380008333333333333888333333333}
    NumGlyphs = 2
    OnClick = SpeedButton22Click
  end
  object SaveSpeedButton: TSpeedButton
    Left = 224
    Top = 111
    Width = 25
    Height = 25
    Hint = 'Save image'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
      7700333333337777777733333333008088003333333377F73377333333330088
      88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
      000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
      FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
      99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
      99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
      99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
      93337FFFF7737777733300000033333333337777773333333333}
    NumGlyphs = 2
    OnClick = SaveSpeedButtonClick
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Make image negative'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object DilateButton: TBitBtn
    Left = 8
    Top = 48
    Width = 137
    Height = 25
    Caption = 'Dilate image'
    TabOrder = 1
    OnClick = DilateButtonClick
  end
  object ErodeButton: TBitBtn
    Left = 8
    Top = 88
    Width = 137
    Height = 25
    Caption = 'Erode image'
    TabOrder = 2
    OnClick = ErodeButtonClick
  end
  object RadioGroup1: TRadioGroup
    Left = 190
    Top = 39
    Width = 185
    Height = 66
    Caption = 'Color options'
    ItemIndex = 0
    Items.Strings = (
      'Image white, background black'
      'Image black, background white')
    TabOrder = 3
  end
  object OKBtn: TBitBtn
    Left = 24
    Top = 150
    Width = 77
    Height = 27
    Caption = 'OK'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    Margin = 2
    ModalResult = 1
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 107
    Top = 151
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 190
    Top = 150
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    IsControl = True
  end
  object Edit1: TEdit
    Left = 259
    Top = 12
    Width = 33
    Height = 21
    TabOrder = 7
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 320
    Top = 12
    Width = 33
    Height = 21
    TabOrder = 8
    Text = 'Edit1'
    OnChange = Edit2Change
  end
end
