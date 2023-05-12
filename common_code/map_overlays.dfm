object MapOverlayForm: TMapOverlayForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Map Overlays'
  ClientHeight = 611
  ClientWidth = 238
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 1
    Top = 0
    Width = 254
    Height = 112
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 205
      Top = 3
      Width = 35
      Height = 22
      Caption = '+SHP'
      OnClick = SpeedButton1Click
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'TIGER'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 23
      Width = 97
      Height = 17
      Caption = 'Contours'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 80
      Top = 8
      Width = 41
      Height = 17
      Caption = 'Grids'
      TabOrder = 2
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 80
      Top = 23
      Width = 57
      Height = 17
      Caption = 'PLSS'
      TabOrder = 3
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 127
      Top = 8
      Width = 72
      Height = 17
      Caption = 'Gazetteer'
      TabOrder = 4
      OnClick = CheckBox5Click
    end
    object CheckBox7: TCheckBox
      Left = 9
      Top = 40
      Width = 112
      Height = 17
      Caption = 'SRTM water bodies'
      TabOrder = 5
      OnClick = CheckBox7Click
    end
    object CheckBox8: TCheckBox
      Left = 127
      Top = 57
      Width = 97
      Height = 17
      Caption = 'Carto DBs'
      TabOrder = 6
      OnClick = CheckBox8Click
    end
    object CheckBox9: TCheckBox
      Left = 127
      Top = 23
      Width = 97
      Height = 17
      Caption = 'US outlines'
      TabOrder = 7
      OnClick = CheckBox9Click
    end
    object CheckBox10: TCheckBox
      Left = 127
      Top = 40
      Width = 97
      Height = 17
      Caption = 'World outlines'
      TabOrder = 8
      OnClick = CheckBox10Click
    end
    object CheckBox13: TCheckBox
      Left = 127
      Top = 90
      Width = 97
      Height = 17
      Caption = 'Sensors'
      TabOrder = 9
      OnClick = CheckBox13Click
    end
    object CheckBox6: TCheckBox
      Left = 127
      Top = 74
      Width = 52
      Height = 17
      Caption = 'Tissot'
      TabOrder = 10
      OnClick = CheckBox6Click
    end
    object CheckBox14: TCheckBox
      Left = 8
      Top = 90
      Width = 113
      Height = 17
      Caption = 'Second DEM/Grid'
      TabOrder = 11
      OnClick = CheckBox14Click
    end
    object CheckBox15: TCheckBox
      Left = 9
      Top = 63
      Width = 97
      Height = 17
      Caption = 'OSM'
      TabOrder = 12
      OnClick = CheckBox15Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 113
    Width = 220
    Height = 28
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'TIGER Options'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn6: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn6Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 147
    Width = 220
    Height = 28
    TabOrder = 2
    object BitBtn2: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'Grid'
      Glyph.Data = {
        7E000000424D7E000000000000003E0000002800000010000000100000000100
        010000000000400000000000000000000000020000000200000000000000FFFF
        FF00EF7B0000EF7B000000000000EF7B0000EF7B0000EF7B0000EF7B00000000
        0000EF7B0000EF7B0000EF7B0000EF7B000000000000EF7B0000EF7B0000EF7B
        0000}
      TabOrder = 0
      OnClick = BitBtn2Click
    end
    object BitBtn7: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn7Click
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 181
    Width = 220
    Height = 28
    TabOrder = 3
    object BitBtn3: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'Contours'
      Glyph.Data = {
        8E000000424D8E000000000000003E0000002800000014000000140000000100
        010000000000500000000000000000000000020000000000000000000000FFFF
        FF00FFFFF000FFE77000CFF33000E0F99000FE3DD000FF9CF000FFC630008FF7
        B000E7FB9000F039D000FF9CF000BFCE7000C3EF3000F9E7B000FCF79000FE3B
        D00087B9D000F78CD000F3EED000FFFFF000}
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object BitBtn8: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn8Click
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 215
    Width = 220
    Height = 28
    TabOrder = 4
    object BitBtn4: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'Databases'
      TabOrder = 0
      OnClick = BitBtn4Click
    end
    object BitBtn9: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn9Click
    end
  end
  object Panel6: TPanel
    Left = 1
    Top = 249
    Width = 220
    Height = 28
    TabOrder = 5
    object BitBtn5: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'Gazetteer'
      TabOrder = 0
      OnClick = BitBtn5Click
    end
    object BitBtn10: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn10Click
    end
  end
  object Panel7: TPanel
    Left = 0
    Top = 570
    Width = 238
    Height = 41
    Align = alBottom
    TabOrder = 6
    ExplicitTop = 594
    ExplicitWidth = 254
    object OKBtn: TBitBtn
      Left = 3
      Top = 6
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
      TabOrder = 0
      OnClick = OKBtnClick
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 86
      Top = 6
      Width = 77
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn31: TBitBtn
      Left = 169
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Redraw'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 2
      OnClick = BitBtn31Click
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 283
    Width = 220
    Height = 28
    TabOrder = 7
    object BitBtn11: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 0
      OnClick = BitBtn11Click
    end
    object BitBtn16: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'PLSS'
      TabOrder = 1
      OnClick = BitBtn16Click
    end
  end
  object Panel10: TPanel
    Left = 0
    Top = 351
    Width = 220
    Height = 28
    TabOrder = 8
    object BitBtn14: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'SRTM water'
      TabOrder = 0
      OnClick = BitBtn14Click
    end
    object BitBtn15: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn15Click
    end
  end
  object Panel11: TPanel
    Left = 0
    Top = 382
    Width = 220
    Height = 28
    TabOrder = 9
    object BitBtn17: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'Carto DBs'
      TabOrder = 0
      OnClick = BitBtn17Click
    end
    object BitBtn18: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn18Click
    end
  end
  object Panel13: TPanel
    Left = 0
    Top = 416
    Width = 220
    Height = 28
    TabOrder = 10
    object BitBtn21: TBitBtn
      Left = 24
      Top = 0
      Width = 120
      Height = 25
      Caption = 'World outlines'
      TabOrder = 0
      OnClick = BitBtn21Click
    end
    object BitBtn22: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn22Click
    end
  end
  object Panel14: TPanel
    Left = 1
    Top = 450
    Width = 220
    Height = 28
    TabOrder = 11
    object BitBtn23: TBitBtn
      Left = 23
      Top = 0
      Width = 120
      Height = 25
      Caption = 'US outlines'
      TabOrder = 0
      OnClick = BitBtn23Click
    end
    object BitBtn24: TBitBtn
      Left = 149
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn24Click
    end
  end
  object Panel12: TPanel
    Left = 0
    Top = 518
    Width = 220
    Height = 28
    TabOrder = 12
    object BitBtn19: TBitBtn
      Left = 2
      Top = 0
      Width = 72
      Height = 25
      Caption = 'Sensor'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00AAAAAAAAAA99
        99AAAAAAAAAAAA3333339AA3AAAA33AAAA3393AAA3A3A33333AA39AA3A3A3A99
        333339A3A3A3A33A9933393A3A3A3A333AA3393A3A33A3A3333A3393A3A3933A
        33333393A3933933A33333933A3933933A3333A33A39A33A33A333A33A33933A
        333A333A33A33A33A333333A33A33A333A33333A333A33A33333}
      TabOrder = 0
      OnClick = BitBtn19Click
    end
    object BitBtn20: TBitBtn
      Left = 186
      Top = 0
      Width = 25
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn20Click
    end
    object TrackBar1: TTrackBar
      Left = 80
      Top = 0
      Width = 100
      Height = 25
      Max = 100
      Frequency = 10
      TabOrder = 2
      OnChange = TrackBar1Change
    end
  end
  object Panel9: TPanel
    Left = 1
    Top = 484
    Width = 220
    Height = 28
    TabOrder = 13
    object BitBtn12: TBitBtn
      Left = 24
      Top = 3
      Width = 120
      Height = 25
      Caption = 'Tissot'
      TabOrder = 0
      OnClick = BitBtn12Click
    end
    object BitBtn13: TBitBtn
      Left = 150
      Top = 0
      Width = 60
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn13Click
    end
  end
  object Panel17: TPanel
    Left = 1
    Top = 317
    Width = 220
    Height = 28
    TabOrder = 14
    object BitBtn29: TBitBtn
      Left = 2
      Top = 0
      Width = 72
      Height = 25
      Caption = '2d DEM/Grid'
      TabOrder = 0
    end
    object BitBtn30: TBitBtn
      Left = 186
      Top = 0
      Width = 25
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn30Click
    end
    object TrackBar2: TTrackBar
      Left = 80
      Top = -3
      Width = 100
      Height = 25
      Max = 100
      Frequency = 25
      TabOrder = 2
      OnChange = TrackBar1Change
    end
  end
  object Panel18: TPanel
    Left = 0
    Top = 552
    Width = 220
    Height = 28
    TabOrder = 15
    object BitBtn32: TBitBtn
      Left = 2
      Top = 0
      Width = 72
      Height = 25
      Caption = 'Vectors'
      TabOrder = 0
    end
    object BitBtn33: TBitBtn
      Left = 186
      Top = 0
      Width = 25
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn30Click
    end
    object TrackBar3: TTrackBar
      Left = 80
      Top = 0
      Width = 100
      Height = 25
      Max = 100
      Frequency = 25
      TabOrder = 2
      OnChange = TrackBar1Change
    end
  end
  object Panel19: TPanel
    Left = 2
    Top = 586
    Width = 220
    Height = 28
    TabOrder = 16
    object BitBtn34: TBitBtn
      Left = -2
      Top = 3
      Width = 72
      Height = 25
      Caption = 'OSM'
      TabOrder = 0
      OnClick = BitBtn34Click
    end
    object BitBtn35: TBitBtn
      Left = 182
      Top = 1
      Width = 25
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003C3333339333
        337437FFF3337F3333F73CCC33339333344437773F337F33377733CCC3339337
        4447337F73FF7F3F337F33CCCCC3934444433373F7737F773373333CCCCC9444
        44733337F337773337F3333CCCCC9444443333373F337F3337333333CCCC9444
        473333337F337F337F333333CCCC94444333333373F37F33733333333CCC9444
        7333333337F37F37F33333333CCC944433333333373F7F373333333333CC9447
        33333333337F7F7F3333333333CC94433333333333737F7333333333333C9473
        33333333333737F333333333333C943333333333333737333333333333339733
        3333333333337F33333333333333933333333333333373333333}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn35Click
    end
    object TrackBar4: TTrackBar
      Left = 76
      Top = 1
      Width = 100
      Height = 25
      Max = 100
      Frequency = 25
      TabOrder = 2
      OnChange = TrackBar4Change
    end
  end
end
