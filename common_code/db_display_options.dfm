object db_display_opts: Tdb_display_opts
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Database display options'
  ClientHeight = 541
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object BottomPanel: TPanel
    Left = 0
    Top = 500
    Width = 345
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 491
    ExplicitWidth = 339
    object SpeedButton16: TSpeedButton
      Left = 285
      Top = 8
      Width = 31
      Height = 25
      Caption = 'SHP'
      OnClick = SpeedButton16Click
    end
    object OKBtn: TBitBtn
      Left = 3
      Top = 6
      Width = 52
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
      Left = 61
      Top = 6
      Width = 58
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
      Left = 129
      Top = 8
      Width = 64
      Height = 25
      Caption = 'Check all'
      TabOrder = 2
      OnClick = BitBtn31Click
    end
    object BitBtn32: TBitBtn
      Left = 199
      Top = 8
      Width = 80
      Height = 25
      Caption = 'Uncheck all'
      TabOrder = 3
      OnClick = BitBtn32Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 279
    Height = 25
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton1Click
    end
    object BitBtn1: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn1Click
      OnMouseDown = BitBtn1MouseDown
    end
    object CheckBox1: TCheckBox
      Left = 206
      Top = 8
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object BitBtn16: TBitBtn
      Left = 253
      Top = 2
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
      TabOrder = 2
      OnClick = BitBtn16Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 31
    Width = 279
    Height = 25
    TabOrder = 2
    object SpeedButton2: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton2Click
    end
    object BitBtn2: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn2Click
      OnMouseDown = BitBtn2MouseDown
    end
    object CheckBox2: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object BitBtn17: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn17Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 62
    Width = 279
    Height = 25
    TabOrder = 3
    object SpeedButton3: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton3Click
    end
    object BitBtn3: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn3Click
      OnMouseDown = BitBtn3MouseDown
    end
    object CheckBox3: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox3Click
    end
    object BitBtn18: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn18Click
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 93
    Width = 279
    Height = 25
    TabOrder = 4
    object SpeedButton4: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton4Click
    end
    object BitBtn4: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn4Click
      OnMouseDown = BitBtn4MouseDown
    end
    object CheckBox4: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox4Click
    end
    object BitBtn19: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn19Click
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 125
    Width = 279
    Height = 25
    TabOrder = 5
    object SpeedButton5: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton5Click
    end
    object BitBtn5: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn5Click
      OnMouseDown = BitBtn5MouseDown
    end
    object CheckBox5: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox5Click
    end
    object BitBtn20: TBitBtn
      Left = 253
      Top = 4
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
      TabOrder = 2
      OnClick = BitBtn20Click
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 153
    Width = 279
    Height = 25
    TabOrder = 6
    object SpeedButton6: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton6Click
    end
    object BitBtn6: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn6Click
    end
    object CheckBox6: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox6Click
    end
    object BitBtn21: TBitBtn
      Left = 253
      Top = 3
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
      TabOrder = 2
      OnClick = BitBtn21Click
    end
  end
  object Panel7: TPanel
    Left = 0
    Top = 183
    Width = 279
    Height = 25
    TabOrder = 7
    object SpeedButton7: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton7Click
    end
    object BitBtn7: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn7Click
    end
    object CheckBox7: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox7Click
    end
    object BitBtn22: TBitBtn
      Left = 253
      Top = 1
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
      TabOrder = 2
      OnClick = BitBtn22Click
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 214
    Width = 279
    Height = 25
    TabOrder = 8
    object SpeedButton8: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton8Click
    end
    object BitBtn8: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn8Click
    end
    object CheckBox8: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox8Click
    end
    object BitBtn23: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn23Click
    end
  end
  object Panel9: TPanel
    Left = 3
    Top = 253
    Width = 276
    Height = 25
    TabOrder = 9
    object SpeedButton9: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton9Click
    end
    object BitBtn9: TBitBtn
      Left = -3
      Top = 0
      Width = 203
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn9Click
    end
    object CheckBox9: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox9Click
    end
    object BitBtn24: TBitBtn
      Left = 253
      Top = 0
      Width = 18
      Height = 25
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADA00000000000000000FF0FEF4FEF4FEF00FF0EFE4EFE4EFE00FF044444444
        44400FF0EFE4EFE4EFE00000FEF4FEF4FEF00FF04444444444400FF0FEF4FEF4
        FEF00FF0EFE4EFE4EFE000000000000000000FF0FFFFF0FFFFF00FF0FFFFF0FF
        FFF00000000000000000DADADADADADADADAADADADADADADADAD}
      TabOrder = 2
      OnClick = BitBtn24Click
    end
  end
  object Panel11: TPanel
    Left = 3
    Top = 315
    Width = 276
    Height = 25
    TabOrder = 10
    object SpeedButton11: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton11Click
    end
    object BitBtn11: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn11Click
    end
    object CheckBox11: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox11Click
    end
    object BitBtn26: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn26Click
    end
  end
  object Panel10: TPanel
    Left = 3
    Top = 284
    Width = 276
    Height = 25
    TabOrder = 11
    object SpeedButton10: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton10Click
    end
    object BitBtn10: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn10Click
    end
    object CheckBox10: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox10Click
    end
    object BitBtn25: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn25Click
    end
  end
  object Panel12: TPanel
    Left = 3
    Top = 346
    Width = 276
    Height = 25
    TabOrder = 12
    object SpeedButton12: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton12Click
    end
    object BitBtn12: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn12Click
    end
    object CheckBox12: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox12Click
    end
    object BitBtn27: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn27Click
    end
  end
  object Panel13: TPanel
    Left = 3
    Top = 377
    Width = 276
    Height = 25
    TabOrder = 13
    object SpeedButton13: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton13Click
    end
    object BitBtn13: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn13Click
    end
    object CheckBox13: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox13Click
    end
    object BitBtn28: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn28Click
    end
  end
  object Panel14: TPanel
    Left = 3
    Top = 419
    Width = 276
    Height = 25
    TabOrder = 14
    object SpeedButton14: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton14Click
    end
    object BitBtn14: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn14Click
    end
    object CheckBox14: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox14Click
    end
    object BitBtn29: TBitBtn
      Left = 253
      Top = -1
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
      TabOrder = 2
      OnClick = BitBtn29Click
    end
  end
  object Panel15: TPanel
    Left = 3
    Top = 450
    Width = 276
    Height = 25
    TabOrder = 15
    object SpeedButton15: TSpeedButton
      Left = 224
      Top = 4
      Width = 23
      Height = 18
      Caption = '^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButton15Click
    end
    object BitBtn15: TBitBtn
      Left = 0
      Top = 0
      Width = 200
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 0
      OnClick = BitBtn15Click
    end
    object CheckBox15: TCheckBox
      Left = 206
      Top = 4
      Width = 12
      Height = 17
      TabOrder = 1
      OnClick = CheckBox15Click
    end
    object BitBtn30: TBitBtn
      Left = 253
      Top = 0
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
      TabOrder = 2
      OnClick = BitBtn30Click
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 152
    Top = 488
    object Filter1: TMenuItem
      Caption = 'Filter'
      OnClick = Filter1Click
    end
    object CodebyDBfield1: TMenuItem
      Caption = 'Code by DB field'
      OnClick = CodebyDBfield1Click
    end
  end
end
