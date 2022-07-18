object CoordConverter: TCoordConverter
  Left = 282
  Top = 154
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Coordinate Converter'
  ClientHeight = 461
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBB
    BBBBB00000999999990001111111BBBBBBBBBBBBB00999999990001111110000
    BBBBBBBBBB00999999990011111133300BBBBBBBBBB009999998888888813333
    0000BBBBBBBB00999998FFFFFF813333333000BBBBBBB0099998F6666F813333
    3333300BBBBBBB009998F6666F810333333333000BBBBBB00098F6666F810000
    0033333300BBBBBBB008F6666F80AAAAA0033333300BBBBBBB08F6666F80AAAA
    AA03333333000BBBBBB8F6666F89AAAAAA003333333300BBBBB8F6666F898888
    88888888888888888888F6666F898FFFFFFFFFFFFFFFFFFFFFFFF6666F898EEE
    EEEEEEEEEEEEEEEEEEEEE6666F898EEEEEEEEEEEEEEEEEEEEEEEF6666F898FFF
    FFFFFFFFFFFFFFFFFFFFF6666F8988888888888888888888888FF6666F802222
    000AAAAAAAA00333333FF6666F8B22222200AAAAAAAA0033333FF6666F8B2222
    22200AAAAAAAA003333FF6666F8B2222222200AAAAAAAA03333FF666F88B0222
    22222000AAAAAA00333FF666F8BB00022222222000AAAAA0033FF666F8BBCC00
    02222222200AAAAA00FF6666F8BBCCCC002222222200AAAAA0FF6666F80BCCCC
    C00022222220AAAAAAFF6666F80B00CCCCC0002222200AAAAAFF666FF800400C
    CCCCC022222200AAAAFF666F88334400CCCCC00222222000AAFF666F83334440
    00CCCC002222222000FFFFFF83334444400CCCC00022222220AAAA0033330000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 31
    Top = 203
    Width = 39
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 104
    Top = 203
    Width = 39
    Height = 13
    Caption = 'Label2'
  end
  object Label6: TLabel
    Left = 175
    Top = 203
    Width = 39
    Height = 13
    Caption = 'Label6'
  end
  object Label14: TLabel
    Left = 52
    Top = 30
    Width = 46
    Height = 13
    Caption = 'Label14'
  end
  object Label13: TLabel
    Left = 104
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Label13'
  end
  object Label15: TLabel
    Left = 319
    Top = 34
    Width = 46
    Height = 13
    Caption = 'Label15'
  end
  object Label17: TLabel
    Left = 373
    Top = 13
    Width = 46
    Height = 13
    Caption = 'Label17'
  end
  object OKBtn: TBitBtn
    Left = 56
    Top = 422
    Width = 77
    Height = 27
    Caption = '&OK'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 280
    Top = 422
    Width = 76
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object GroupBox1: TGroupBox
    Left = 321
    Top = 53
    Width = 280
    Height = 116
    Caption = 'Output Datum Coordinates'
    TabOrder = 2
    object Label7: TLabel
      Left = 18
      Top = 21
      Width = 39
      Height = 13
      Caption = 'Label7'
    end
    object Label8: TLabel
      Left = 18
      Top = 40
      Width = 39
      Height = 13
      Caption = 'Label8'
    end
    object Label9: TLabel
      Left = 18
      Top = 59
      Width = 39
      Height = 13
      Caption = 'Label9'
    end
    object Label10: TLabel
      Left = 19
      Top = 78
      Width = 46
      Height = 13
      Caption = 'Label10'
    end
    object Label11: TLabel
      Left = 16
      Top = 97
      Width = 46
      Height = 13
      Caption = 'Label11'
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 53
    Width = 297
    Height = 116
    Caption = 'Input Datum Coordinates'
    TabOrder = 3
    object Label3: TLabel
      Left = 8
      Top = 21
      Width = 39
      Height = 13
      Caption = 'Label3'
    end
    object Label4: TLabel
      Left = 8
      Top = 39
      Width = 39
      Height = 13
      Caption = 'Label4'
    end
    object Label5: TLabel
      Left = 8
      Top = 58
      Width = 39
      Height = 13
      Caption = 'Label5'
    end
    object Label12: TLabel
      Left = 8
      Top = 77
      Width = 46
      Height = 13
      Caption = 'Label12'
    end
    object Label16: TLabel
      Left = 8
      Top = 96
      Width = 46
      Height = 13
      Caption = 'Label16'
    end
  end
  object Button1: TButton
    Left = 55
    Top = 230
    Width = 65
    Height = 25
    Caption = 'Convert'
    Enabled = False
    TabOrder = 4
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 286
    Width = 608
    Height = 130
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object Button2: TButton
    Left = 197
    Top = 230
    Width = 65
    Height = 25
    Caption = 'Clear'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 268
    Top = 230
    Width = 65
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = Button4Click
  end
  object CheckBox1: TCheckBox
    Left = 424
    Top = 203
    Width = 97
    Height = 17
    Caption = 'Lat/Long'
    TabOrder = 8
  end
  object CheckBox2: TCheckBox
    Left = 424
    Top = 247
    Width = 97
    Height = 17
    Caption = 'UTM'
    TabOrder = 9
  end
  object Button6: TButton
    Left = 126
    Top = 230
    Width = 65
    Height = 25
    Caption = 'Overlap'
    Enabled = False
    TabOrder = 10
    OnClick = Button6Click
  end
  object CheckBox4: TCheckBox
    Left = 424
    Top = 187
    Width = 153
    Height = 17
    Caption = '40 km grid overlap'
    TabOrder = 11
  end
  object CheckBox5: TCheckBox
    Left = 447
    Top = 224
    Width = 97
    Height = 17
    Caption = '3 formats'
    TabOrder = 12
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 8
    Width = 35
    Height = 29
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5555555555FFFFF555555555544C4C5555555555F777775FF5555554C444C444
      5555555775FF55775F55554C4334444445555575577F55557FF554C4C334C4C4
      335557F5577FF55577F554CCC3334444335557555777F555775FCCCCC333CCC4
      C4457F55F777F555557F4CC33333CCC444C57F577777F5F5557FC4333333C3C4
      CCC57F777777F7FF557F4CC33333333C4C457F577777777F557FCCC33CC4333C
      C4C575F7755F777FF5755CCCCC3333334C5557F5FF777777F7F554C333333333
      CC55575777777777F755553333CC3C33C555557777557577755555533CC4C4CC
      5555555775FFFF77555555555C4CCC5555555555577777555555}
    NumGlyphs = 2
    TabOrder = 13
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 50
    Top = 3
    Width = 48
    Height = 25
    Caption = 'Datum'
    TabOrder = 14
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 319
    Top = 8
    Width = 48
    Height = 25
    Caption = 'Datum'
    TabOrder = 15
    OnClick = BitBtn3Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.TXT'
    Filter = 'Text Files|*.TXT'
    Left = 504
    Top = 424
  end
end
