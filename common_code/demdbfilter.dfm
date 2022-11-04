object dbFilterCreation: TdbFilterCreation
  Left = 146
  Top = 207
  BorderIcons = []
  Caption = 'Data Base Filter'
  ClientHeight = 367
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 264
    Width = 533
    Height = 103
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 255
    ExplicitWidth = 527
    object BitBtn8: TBitBtn
      Left = 120
      Top = 38
      Width = 89
      Height = 27
      Caption = 'Plot'
      TabOrder = 0
      OnClick = BitBtn8Click
    end
    object BitBtn1: TBitBtn
      Left = 15
      Top = 9
      Width = 89
      Height = 25
      Caption = 'Apply Filter'
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
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object BitBtn3: TBitBtn
      Left = 120
      Top = 7
      Width = 89
      Height = 25
      Caption = 'Cancel'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333333333333333333FFF33FF333FFF339993370733
        999333777FF37FF377733339993000399933333777F777F77733333399970799
        93333333777F7377733333333999399933333333377737773333333333990993
        3333333333737F73333333333331013333333333333777FF3333333333910193
        333333333337773FF3333333399000993333333337377737FF33333399900099
        93333333773777377FF333399930003999333337773777F777FF339993370733
        9993337773337333777333333333333333333333333333333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      TabOrder = 2
      OnClick = BitBtn3Click
    end
    object HelpBtn: TBitBtn
      Left = 232
      Top = 7
      Width = 77
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 3
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn10: TBitBtn
      Left = 320
      Top = 40
      Width = 73
      Height = 25
      Hint = 'Add filter to layer file'
      Caption = '+ LYR'
      TabOrder = 4
      OnClick = BitBtn10Click
    end
    object CheckBox2: TCheckBox
      Left = 408
      Top = 2
      Width = 97
      Height = 17
      Caption = 'Clear layer'
      TabOrder = 5
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 408
      Top = 25
      Width = 121
      Height = 17
      Caption = 'Redraw on close'
      TabOrder = 6
      OnClick = CheckBox3Click
    end
    object CheckBox8: TCheckBox
      Left = 408
      Top = 56
      Width = 115
      Height = 17
      Caption = 'Set USE on close'
      TabOrder = 7
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 533
    Height = 264
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 527
    ExplicitHeight = 255
    object TabSheet1: TTabSheet
      Caption = 'Main filter'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 525
        Height = 90
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 519
        object Label2: TLabel
          Left = 208
          Top = 20
          Width = 3
          Height = 13
        end
        object Label1: TLabel
          Left = 217
          Top = 5
          Width = 73
          Height = 24
          Caption = 'DB field'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit1: TEdit
          Left = 3
          Top = 35
          Width = 113
          Height = 21
          Enabled = False
          TabOrder = 0
        end
        object BitBtn2: TBitBtn
          Left = 3
          Top = 63
          Width = 113
          Height = 25
          Caption = 'Add condition'
          Glyph.Data = {
            66010000424D6601000000000000760000002800000014000000140000000100
            040000000000F000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFAAAA
            FFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000FFFF
            FFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF
            0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000AAAAAAAAAAAA
            AAAAAAAA0000AAAAAAAAAAAAAAAAAAAA0000AAAAAAAAAAAAAAAAAAAA0000AAAA
            AAAAAAAAAAAAAAAA0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF
            0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAA
            FFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000FFFFFFFFAAAAFFFFFFFF0000FFFF
            FFFFAAAAFFFFFFFF0000}
          TabOrder = 1
          OnClick = BitBtn2Click
        end
        object ComboBox3: TComboBox
          Left = 131
          Top = 35
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '<'
          Items.Strings = (
            '<'
            '<=')
        end
        object ComboBox1: TComboBox
          Left = 178
          Top = 35
          Width = 137
          Height = 21
          TabOrder = 3
          OnChange = ComboBox1Change
        end
        object Button1: TButton
          Left = 267
          Top = 62
          Width = 49
          Height = 25
          Caption = 'All fields'
          TabOrder = 4
          OnClick = Button1Click
        end
        object ComboBox2: TComboBox
          Left = 331
          Top = 35
          Width = 57
          Height = 21
          TabOrder = 5
          Text = '<'
          OnChange = ComboBox2Change
          Items.Strings = (
            '<'
            '<='
            '='
            '<>')
        end
        object Edit2: TEdit
          Left = 395
          Top = 35
          Width = 105
          Height = 21
          TabOrder = 6
          Text = ' '
        end
        object BitBtn5: TBitBtn
          Left = 386
          Top = 62
          Width = 89
          Height = 25
          Caption = 'Query field'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            333333333333333FF3FF3333333333CC30003333333333773777333333333C33
            3000333FF33337F33777339933333C3333333377F33337F3333F339933333C33
            33003377333337F33377333333333C333300333F333337F33377339333333C33
            3333337FF3333733333F33993333C33333003377FF33733333773339933C3333
            330033377FF73F33337733339933C33333333FF377F373F3333F993399333C33
            330077F377F337F33377993399333C33330077FF773337F33377399993333C33
            33333777733337F333FF333333333C33300033333333373FF7773333333333CC
            3000333333333377377733333333333333333333333333333333}
          NumGlyphs = 2
          TabOrder = 7
          OnClick = BitBtn5Click
        end
        object BitBtn12: TBitBtn
          Left = 122
          Top = 65
          Width = 59
          Height = 25
          Caption = 'Quick'
          TabOrder = 8
          OnClick = BitBtn12Click
        end
        object BitBtn7: TBitBtn
          Left = 481
          Top = 62
          Width = 42
          Height = 25
          Caption = 'OR'
          TabOrder = 9
          OnClick = BitBtn7Click
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 90
        Width = 525
        Height = 88
        Align = alTop
        TabOrder = 1
        WordWrap = False
        OnChange = Memo1Change
        ExplicitWidth = 519
      end
      object CheckBox1: TCheckBox
        Left = 3
        Top = 200
        Width = 97
        Height = 17
        Caption = 'Case insensitive'
        TabOrder = 2
      end
      object BitBtn4: TBitBtn
        Left = 136
        Top = 200
        Width = 75
        Height = 25
        Caption = 'Clear'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00500005000555
          555557777F777555F55500000000555055557777777755F75555005500055055
          555577F5777F57555555005550055555555577FF577F5FF55555500550050055
          5555577FF77577FF555555005050110555555577F757777FF555555505099910
          555555FF75777777FF555005550999910555577F5F77777775F5500505509990
          3055577F75F77777575F55005055090B030555775755777575755555555550B0
          B03055555F555757575755550555550B0B335555755555757555555555555550
          BBB35555F55555575F555550555555550BBB55575555555575F5555555555555
          50BB555555555555575F555555555555550B5555555555555575}
        NumGlyphs = 2
        TabOrder = 3
        OnClick = BitBtn4Click
      end
      object BitBtn11: TBitBtn
        Left = 246
        Top = 200
        Width = 75
        Height = 25
        Caption = 'USE='#39'Y'#39
        TabOrder = 4
        OnClick = BitBtn11Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Geographic filter'
      ImageIndex = 1
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 525
        Height = 86
        Align = alTop
        Lines.Strings = (
          '')
        ScrollBars = ssHorizontal
        TabOrder = 0
        WordWrap = False
      end
      object BitBtn6: TBitBtn
        Left = 0
        Top = 92
        Width = 75
        Height = 25
        Caption = 'Clear'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00500005000555
          555557777F777555F55500000000555055557777777755F75555005500055055
          555577F5777F57555555005550055555555577FF577F5FF55555500550050055
          5555577FF77577FF555555005050110555555577F757777FF555555505099910
          555555FF75777777FF555005550999910555577F5F77777775F5500505509990
          3055577F75F77777575F55005055090B030555775755777575755555555550B0
          B03055555F555757575755550555550B0B335555755555757555555555555550
          BBB35555F55555575F555550555555550BBB55575555555575F5555555555555
          50BB555555555555575F555555555555550B5555555555555575}
        NumGlyphs = 2
        TabOrder = 1
        OnClick = BitBtn6Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Time filter'
      ImageIndex = 2
      object GroupBox1: TGroupBox
        Left = 0
        Top = 39
        Width = 229
        Height = 89
        Caption = 'Month'
        TabOrder = 0
        object CheckBox4: TCheckBox
          Left = 8
          Top = 15
          Width = 57
          Height = 17
          Caption = 'Jan'
          TabOrder = 0
          OnClick = CheckBox4Click
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 32
          Width = 57
          Height = 17
          Caption = 'Feb'
          TabOrder = 1
          OnClick = CheckBox5Click
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 48
          Width = 57
          Height = 17
          Caption = 'Mar'
          TabOrder = 2
          OnClick = CheckBox6Click
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 66
          Width = 57
          Height = 17
          Caption = 'Apr'
          TabOrder = 3
          OnClick = CheckBox7Click
        end
        object CheckBox12: TCheckBox
          Left = 64
          Top = 50
          Width = 57
          Height = 17
          Caption = 'Jul'
          TabOrder = 4
          OnClick = CheckBox12Click
        end
        object CheckBox13: TCheckBox
          Left = 64
          Top = 34
          Width = 57
          Height = 17
          Caption = 'Jun'
          TabOrder = 5
          OnClick = CheckBox13Click
        end
        object CheckBox14: TCheckBox
          Left = 64
          Top = 17
          Width = 57
          Height = 17
          Caption = 'May'
          TabOrder = 6
          OnClick = CheckBox14Click
        end
        object CheckBox15: TCheckBox
          Left = 64
          Top = 68
          Width = 57
          Height = 17
          Caption = 'Aug'
          TabOrder = 7
          OnClick = CheckBox15Click
        end
        object CheckBox20: TCheckBox
          Left = 116
          Top = 49
          Width = 57
          Height = 17
          Caption = 'Nov'
          TabOrder = 8
          OnClick = CheckBox20Click
        end
        object CheckBox21: TCheckBox
          Left = 116
          Top = 33
          Width = 57
          Height = 17
          Caption = 'Oct'
          TabOrder = 9
          OnClick = CheckBox21Click
        end
        object CheckBox22: TCheckBox
          Left = 116
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Sep'
          TabOrder = 10
          OnClick = CheckBox22Click
        end
        object CheckBox23: TCheckBox
          Left = 116
          Top = 67
          Width = 57
          Height = 17
          Caption = 'Dec'
          TabOrder = 11
          OnClick = CheckBox23Click
        end
      end
      object Memo3: TMemo
        Left = 0
        Top = 134
        Width = 519
        Height = 89
        TabOrder = 1
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 3
        Width = 438
        Height = 34
        TabOrder = 2
        object Label3: TLabel
          Left = 205
          Top = 3
          Width = 56
          Height = 24
          Caption = 'YEAR'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Edit4: TEdit
          Left = 330
          Top = 3
          Width = 105
          Height = 21
          TabOrder = 0
          Text = ' '
        end
        object ComboBox5: TComboBox
          Left = 267
          Top = 3
          Width = 57
          Height = 21
          TabOrder = 1
          Text = '<'
          OnChange = ComboBox2Change
          Items.Strings = (
            '<'
            '<='
            '='
            '<>')
        end
        object ComboBox4: TComboBox
          Left = 158
          Top = 3
          Width = 41
          Height = 21
          TabOrder = 2
          Text = '<'
          Items.Strings = (
            '<'
            '<=')
        end
        object Edit3: TEdit
          Left = 16
          Top = 3
          Width = 113
          Height = 21
          Enabled = False
          TabOrder = 3
        end
      end
      object BitBtn13: TBitBtn
        Left = 264
        Top = 56
        Width = 97
        Height = 25
        Caption = 'Clear time filter'
        TabOrder = 3
        OnClick = BitBtn13Click
      end
    end
  end
end
