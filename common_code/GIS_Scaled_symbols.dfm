object gis_scaled_form: Tgis_scaled_form
  Left = 463
  Top = 411
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Scaled Symbols'
  ClientHeight = 2271
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 262
    Width = 288
    Height = 188
    TabOrder = 0
    object Label30: TLabel
      Left = 177
      Top = 7
      Width = 36
      Height = 13
      Caption = 'Opacity'
    end
    object PlotScaledSymbolsButton: TBitBtn
      Left = 8
      Top = 152
      Width = 71
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
      TabOrder = 0
      OnClick = PlotScaledSymbolsButtonClick
    end
    object BitBtn2: TBitBtn
      Left = 166
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object Lengend: TBitBtn
      Left = 5
      Top = 121
      Width = 57
      Height = 25
      Caption = 'Legend'
      TabOrder = 2
      OnClick = LengendClick
    end
    object HelpBtn: TBitBtn
      Left = 85
      Top = 152
      Width = 75
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 3
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn8: TBitBtn
      Left = 68
      Top = 121
      Width = 37
      Height = 25
      Caption = 'Filter'
      TabOrder = 4
      OnClick = BitBtn8Click
    end
    object TrackBar1: TTrackBar
      Left = 134
      Top = 26
      Width = 150
      Height = 26
      Max = 100
      Frequency = 10
      Position = 58
      TabOrder = 5
      OnChange = TrackBar1Change
    end
    object CheckBox2: TCheckBox
      Left = 5
      Top = 7
      Width = 107
      Height = 17
      Caption = 'Immediate redraws'
      TabOrder = 6
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 6
      Top = 30
      Width = 66
      Height = 17
      Caption = 'Labels'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = CheckBox3Click
    end
    object BitBtn24: TBitBtn
      Left = 111
      Top = 90
      Width = 38
      Height = 25
      TabOrder = 8
      OnClick = BitBtn24Click
    end
    object CheckBox10: TCheckBox
      Left = 8
      Top = 90
      Width = 97
      Height = 17
      Caption = 'Outline polygons'
      TabOrder = 9
      OnClick = CheckBox10Click
    end
    object BitBtn17: TBitBtn
      Left = 116
      Top = 121
      Width = 99
      Height = 25
      Caption = 'New display mode'
      TabOrder = 10
      OnClick = BitBtn17Click
    end
    object CheckBox9: TCheckBox
      Left = 181
      Top = 58
      Width = 97
      Height = 17
      Caption = 'Monthly filter'
      TabOrder = 11
      OnClick = CheckBox9Click
    end
    object CheckBox14: TCheckBox
      Left = 181
      Top = 75
      Width = 97
      Height = 17
      Caption = 'Connect pts'
      TabOrder = 12
      OnClick = CheckBox14Click
    end
    object CheckBox15: TCheckBox
      Left = 6
      Top = 49
      Width = 97
      Height = 17
      Caption = 'Grayscale'
      TabOrder = 13
      OnClick = CheckBox15Click
    end
    object CheckBox16: TCheckBox
      Left = 6
      Top = 67
      Width = 97
      Height = 17
      Caption = 'Subdue'
      TabOrder = 14
      OnClick = CheckBox16Click
    end
    object BitBtn20: TBitBtn
      Left = 221
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Defaults'
      TabOrder = 15
      OnClick = BitBtn20Click
    end
    object CheckBox17: TCheckBox
      Left = 181
      Top = 98
      Width = 97
      Height = 17
      Caption = 'Quick filter'
      TabOrder = 16
      OnClick = CheckBox17Click
    end
    object BitBtn27: TBitBtn
      Left = 221
      Top = 121
      Width = 61
      Height = 25
      Caption = 'All DBs'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 17
      OnClick = BitBtn27Click
    end
  end
  object Panel3: TPanel
    Left = 2
    Top = 81
    Width = 294
    Height = 120
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Top = 47
      Width = 76
      Height = 13
      Caption = 'Max symbol size'
    end
    object Label2: TLabel
      Left = 152
      Top = 57
      Width = 49
      Height = 13
      Caption = 'Max value'
    end
    object Label4: TLabel
      Left = 156
      Top = 38
      Width = 3
      Height = 13
    end
    object Label31: TLabel
      Left = 16
      Top = 80
      Width = 73
      Height = 13
      Caption = 'Min symbol size'
    end
    object Label32: TLabel
      Left = 158
      Top = 88
      Width = 46
      Height = 13
      Caption = 'Min value'
    end
    object Edit2: TEdit
      Left = 207
      Top = 56
      Width = 66
      Height = 21
      TabOrder = 0
      OnChange = Edit2Change
    end
    object Edit1: TEdit
      Left = 96
      Top = 47
      Width = 41
      Height = 21
      TabOrder = 1
      Text = '15'
      OnChange = Edit1Change
    end
    object ComboBox1: TComboBox
      Left = 152
      Top = 14
      Width = 137
      Height = 21
      TabOrder = 2
      OnChange = ComboBox1Change
    end
    object RadioGroup3: TRadioGroup
      Left = 7
      Top = 6
      Width = 139
      Height = 35
      Caption = 'Size from'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'DB field'
        'Constant')
      TabOrder = 3
      OnClick = RadioGroup3Click
    end
    object Edit22: TEdit
      Left = 96
      Top = 80
      Width = 41
      Height = 21
      TabOrder = 4
      Text = 'Edit22'
      OnChange = Edit22Change
    end
    object Edit24: TEdit
      Left = 211
      Top = 83
      Width = 64
      Height = 21
      TabOrder = 5
      Text = 'Edit24'
      OnChange = Edit24Change
    end
  end
  object Panel4: TPanel
    Left = 8
    Top = 0
    Width = 288
    Height = 89
    TabOrder = 2
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 0
      Width = 113
      Height = 81
      Caption = 'Plot'
      ItemIndex = 0
      Items.Strings = (
        'Squares'
        'Circles'
        'True type symbol')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object BitBtn3: TBitBtn
      Left = 127
      Top = 7
      Width = 65
      Height = 41
      TabOrder = 1
      OnClick = BitBtn3Click
    end
  end
  object Panel6: TPanel
    Left = 8
    Top = 456
    Width = 288
    Height = 74
    TabOrder = 3
    object Label7: TLabel
      Left = 260
      Top = 16
      Width = 14
      Height = 13
      Caption = 'pts'
    end
    object Label29: TLabel
      Left = 16
      Top = 38
      Width = 94
      Height = 13
      Caption = 'Segment separation'
    end
    object BitBtn9: TBitBtn
      Left = 9
      Top = 7
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = BitBtn9Click
    end
    object ArrowCheckBox: TCheckBox
      Left = 104
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Arrows every'
      TabOrder = 1
      OnClick = ArrowCheckBoxClick
    end
    object Edit4: TEdit
      Left = 198
      Top = 8
      Width = 56
      Height = 21
      TabOrder = 2
      OnChange = Edit4Change
    end
    object ListBox3: TListBox
      Left = 24
      Top = 0
      Width = 65
      Height = 1
      ItemHeight = 13
      TabOrder = 3
    end
    object ComboBox3: TComboBox
      Left = 116
      Top = 35
      Width = 137
      Height = 21
      TabOrder = 4
      OnChange = ComboBox3Change
    end
  end
  object Panel7: TPanel
    Left = 8
    Top = 536
    Width = 288
    Height = 130
    TabOrder = 4
    object Label8: TLabel
      Left = 174
      Top = 5
      Width = 80
      Height = 13
      Caption = 'Secondary Label'
    end
    object Label16: TLabel
      Left = 161
      Top = 105
      Width = 64
      Height = 13
      Caption = 'Max decimals'
    end
    object Label17: TLabel
      Left = 19
      Top = 5
      Width = 104
      Height = 13
      Caption = 'Primary label from field'
    end
    object Label23: TLabel
      Left = 16
      Top = 112
      Width = 51
      Height = 13
      Caption = 'Skip factor'
    end
    object Label33: TLabel
      Left = 161
      Top = 82
      Width = 36
      Height = 13
      Caption = 'x Offset'
    end
    object FontButton: TBitBtn
      Left = 15
      Top = 51
      Width = 97
      Height = 25
      Caption = 'Font'
      TabOrder = 0
      OnClick = FontButtonClick
    end
    object ComboBox4: TComboBox
      Left = 8
      Top = 24
      Width = 138
      Height = 21
      TabOrder = 1
      OnChange = ComboBox4Change
    end
    object ComboBox5: TComboBox
      Left = 152
      Top = 24
      Width = 129
      Height = 21
      TabOrder = 2
      OnChange = ComboBox5Change
    end
    object Font2Button: TBitBtn
      Left = 161
      Top = 51
      Width = 97
      Height = 25
      Caption = 'Font'
      TabOrder = 3
      OnClick = Font2ButtonClick
    end
    object CheckBox4: TCheckBox
      Left = 15
      Top = 82
      Width = 140
      Height = 17
      Caption = 'Avoid label overprints'
      TabOrder = 4
      OnClick = CheckBox4Click
    end
    object Edit8: TEdit
      Left = 231
      Top = 107
      Width = 38
      Height = 21
      TabOrder = 5
      OnChange = Edit8Change
    end
    object Edit16: TEdit
      Left = 88
      Top = 104
      Width = 67
      Height = 21
      TabOrder = 6
      OnChange = Edit16Change
    end
    object Edit23: TEdit
      Left = 203
      Top = 78
      Width = 54
      Height = 21
      TabOrder = 7
      Text = 'Edit23'
      OnChange = Edit23Change
    end
  end
  object Panel8: TPanel
    Left = 8
    Top = 672
    Width = 288
    Height = 200
    TabOrder = 5
    object Label10: TLabel
      Left = 97
      Top = 94
      Width = 58
      Height = 13
      Caption = 'Line multiple'
    end
    object Label11: TLabel
      Left = 201
      Top = 14
      Width = 21
      Height = 13
      Caption = 'Thin'
    end
    object Label9: TLabel
      Left = 23
      Top = 48
      Width = 50
      Height = 13
      Caption = 'Magnitude'
    end
    object Label12: TLabel
      Left = 173
      Top = 48
      Width = 42
      Height = 13
      Caption = 'Direction'
    end
    object Label14: TLabel
      Left = 114
      Top = 171
      Width = 57
      Height = 13
      Caption = 'Min spacing'
    end
    object BitBtn11: TBitBtn
      Left = 9
      Top = 94
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = BitBtn11Click
    end
    object Edit5: TEdit
      Left = 161
      Top = 94
      Width = 43
      Height = 21
      TabOrder = 1
      OnChange = Edit5Change
    end
    object Edit6: TEdit
      Left = 228
      Top = 14
      Width = 47
      Height = 21
      TabOrder = 2
      OnChange = Edit6Change
    end
    object CheckBox6: TCheckBox
      Left = 8
      Top = 148
      Width = 97
      Height = 17
      Caption = 'Arrowhead'
      TabOrder = 3
      OnClick = CheckBox6Click
    end
    object CheckBox7: TCheckBox
      Left = 104
      Top = 148
      Width = 137
      Height = 17
      Caption = 'Reverse arrows (wind)'
      TabOrder = 4
      OnClick = CheckBox7Click
    end
    object ComboBox6: TComboBox
      Left = 5
      Top = 67
      Width = 131
      Height = 21
      TabOrder = 5
      OnClick = ComboBox6Click
    end
    object ComboBox7: TComboBox
      Left = 142
      Top = 67
      Width = 122
      Height = 21
      TabOrder = 6
      OnClick = ComboBox7Click
    end
    object RadioGroup6: TRadioGroup
      Left = 23
      Top = 5
      Width = 171
      Height = 37
      Columns = 2
      Items.Strings = (
        'Polar'
        'Components')
      TabOrder = 7
      OnClick = RadioGroup6Click
    end
    object CheckBox5: TCheckBox
      Left = 64
      Top = 125
      Width = 122
      Height = 17
      Caption = 'Color by max speed'
      TabOrder = 8
      OnClick = CheckBox5Click
    end
    object Edit17: TEdit
      Left = 184
      Top = 121
      Width = 42
      Height = 21
      TabOrder = 9
      OnChange = Edit17Change
    end
    object CheckBox11: TCheckBox
      Left = 11
      Top = 171
      Width = 97
      Height = 17
      Caption = 'Autospace'
      TabOrder = 10
      OnClick = CheckBox11Click
    end
    object Edit18: TEdit
      Left = 195
      Top = 171
      Width = 69
      Height = 21
      TabOrder = 11
      Text = 'Edit18'
      OnChange = Edit18Change
    end
  end
  object Panel10: TPanel
    Left = 8
    Top = 878
    Width = 288
    Height = 113
    TabOrder = 6
    object Label15: TLabel
      Left = 8
      Top = 89
      Width = 18
      Height = 13
      Caption = 'n>='
    end
    object ComboBox10: TComboBox
      Left = 7
      Top = 8
      Width = 130
      Height = 21
      TabOrder = 0
      OnChange = ComboBox10Change
    end
    object ListBox2: TListBox
      Left = 166
      Top = 1
      Width = 121
      Height = 111
      Align = alRight
      ItemHeight = 13
      TabOrder = 1
      OnClick = ListBox2Click
    end
    object FontButton2: TBitBtn
      Left = 8
      Top = 35
      Width = 49
      Height = 25
      Caption = 'Font'
      TabOrder = 2
      OnClick = FontButton2Click
    end
    object BitBtn10: TBitBtn
      Left = 100
      Top = 86
      Width = 27
      Height = 25
      Caption = '<'
      TabOrder = 3
      OnClick = BitBtn10Click
    end
    object BitBtn12: TBitBtn
      Left = 133
      Top = 86
      Width = 27
      Height = 25
      Caption = '>'
      TabOrder = 4
      OnClick = BitBtn12Click
    end
    object CheckBox8: TCheckBox
      Left = 7
      Top = 66
      Width = 130
      Height = 17
      Caption = 'Connect sequence'
      TabOrder = 5
    end
    object BitBtn13: TBitBtn
      Left = 63
      Top = 37
      Width = 28
      Height = 23
      TabOrder = 6
      OnClick = BitBtn13Click
    end
    object BitBtn14: TBitBtn
      Left = 97
      Top = 35
      Width = 56
      Height = 25
      Caption = 'Movie'
      TabOrder = 7
      OnClick = BitBtn14Click
    end
    object Edit7: TEdit
      Left = 32
      Top = 89
      Width = 34
      Height = 21
      TabOrder = 8
      Text = '1'
      OnChange = Edit7Change
    end
  end
  object Panel11: TPanel
    Left = 8
    Top = 1006
    Width = 288
    Height = 49
    TabOrder = 7
    object BitBtn16: TBitBtn
      Left = 191
      Top = 8
      Width = 81
      Height = 25
      Caption = 'New base map'
      TabOrder = 0
    end
    object RadioGroup5: TRadioGroup
      Left = 8
      Top = 2
      Width = 177
      Height = 34
      Caption = 'Redraw'
      Columns = 2
      Items.Strings = (
        'No records'
        'All records')
      TabOrder = 1
      OnClick = RadioGroup5Click
    end
  end
  object Panel13: TPanel
    Left = 9
    Top = 1061
    Width = 287
    Height = 113
    TabOrder = 8
    object Label18: TLabel
      Left = 7
      Top = 8
      Width = 20
      Height = 13
      Caption = 'Red'
    end
    object Label19: TLabel
      Left = 8
      Top = 29
      Width = 29
      Height = 13
      Caption = 'Green'
    end
    object Label20: TLabel
      Left = 7
      Top = 56
      Width = 21
      Height = 13
      Caption = 'Blue'
    end
    object ComboBox14: TComboBox
      Left = 43
      Top = 4
      Width = 111
      Height = 21
      TabOrder = 0
      OnChange = ComboBox14Change
    end
    object ComboBox15: TComboBox
      Left = 43
      Top = 29
      Width = 111
      Height = 21
      TabOrder = 1
      OnChange = ComboBox15Change
    end
    object ComboBox16: TComboBox
      Left = 43
      Top = 56
      Width = 111
      Height = 21
      TabOrder = 2
      OnChange = ComboBox16Change
    end
    object Edit9: TEdit
      Left = 153
      Top = 5
      Width = 59
      Height = 21
      TabOrder = 3
      OnChange = Edit9Change
    end
    object Edit10: TEdit
      Left = 218
      Top = 5
      Width = 59
      Height = 21
      TabOrder = 4
      OnChange = Edit10Change
    end
    object Edit11: TEdit
      Left = 153
      Top = 29
      Width = 59
      Height = 21
      TabOrder = 5
      OnChange = Edit11Change
    end
    object Edit12: TEdit
      Left = 218
      Top = 32
      Width = 59
      Height = 21
      TabOrder = 6
      OnChange = Edit12Change
    end
    object Edit13: TEdit
      Left = 153
      Top = 56
      Width = 59
      Height = 21
      TabOrder = 7
      OnChange = Edit13Change
    end
    object Edit14: TEdit
      Left = 218
      Top = 56
      Width = 59
      Height = 21
      TabOrder = 8
      OnChange = Edit14Change
    end
    object BitBtn19: TBitBtn
      Left = 103
      Top = 83
      Width = 59
      Height = 25
      Caption = 'Legend'
      TabOrder = 9
      OnClick = BitBtn19Click
    end
  end
  object Panel14Legend: TPanel
    Left = 0
    Top = 1180
    Width = 295
    Height = 73
    TabOrder = 9
    object Image1: TImage
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 287
      Height = 65
      Align = alClient
      ExplicitLeft = -5
      ExplicitTop = 16
    end
  end
  object Panel15: TPanel
    Left = 3
    Top = 1259
    Width = 296
    Height = 66
    TabOrder = 10
    object TrackBar2: TTrackBar
      Left = 1
      Top = 8
      Width = 150
      Height = 45
      Max = 2000
      Frequency = 100
      Position = 300
      TabOrder = 0
    end
    object BitBtn15: TBitBtn
      Left = 176
      Top = 8
      Width = 57
      Height = 25
      Caption = '>'
      TabOrder = 1
      OnClick = BitBtn15Click
    end
  end
  object Panel23: TPanel
    Left = 0
    Top = 1331
    Width = 296
    Height = 107
    TabOrder = 11
    object Label24: TLabel
      Left = 3
      Top = 61
      Width = 17
      Height = 13
      Caption = 'Min'
    end
    object Label25: TLabel
      Left = 95
      Top = 61
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object RadioGroup8: TRadioGroup
      Left = 4
      Top = 7
      Width = 282
      Height = 48
      Caption = 'Month'
      Columns = 6
      Items.Strings = (
        'Jan'
        'Feb'
        'Mar'
        'Apr'
        'May'
        'Jun'
        'Jul'
        'Aug'
        'Sep'
        'Oct'
        'Nov'
        'Dec')
      TabOrder = 0
      OnClick = RadioGroup8Click
    end
    object BitBtn21: TBitBtn
      Left = 11
      Top = 80
      Width = 26
      Height = 20
      Caption = '<'
      TabOrder = 1
      OnClick = BitBtn21Click
    end
    object BitBtn22: TBitBtn
      Left = 43
      Top = 80
      Width = 27
      Height = 20
      Caption = '>'
      TabOrder = 2
      OnClick = BitBtn22Click
    end
    object BitBtn25: TBitBtn
      Left = 88
      Top = 80
      Width = 75
      Height = 20
      Caption = 'Clear'
      TabOrder = 3
      OnClick = BitBtn25Click
    end
    object CheckBox12: TCheckBox
      Left = 169
      Top = 84
      Width = 80
      Height = 17
      Caption = 'Label month'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBox12Click
    end
    object CheckBox13: TCheckBox
      Left = 255
      Top = 85
      Width = 97
      Height = 17
      Caption = 'n='
      TabOrder = 5
    end
    object Edit20: TEdit
      Left = 30
      Top = 56
      Width = 44
      Height = 21
      TabOrder = 6
      OnChange = Edit20Change
    end
    object Edit21: TEdit
      Left = 121
      Top = 57
      Width = 44
      Height = 21
      TabOrder = 7
      OnChange = Edit21Change
    end
    object CheckBox1: TCheckBox
      Left = 171
      Top = 61
      Width = 90
      Height = 17
      Caption = 'Apply all DBs'
      TabOrder = 8
      OnClick = CheckBox1Click
    end
    object BitBtn6: TBitBtn
      Left = 255
      Top = 61
      Width = 27
      Height = 25
      Hint = 'Save alpha blend movie'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
        033333FFFF77777773F330000077777770333777773FFFFFF733077777000000
        03337F3F3F777777733F0797A770003333007F737337773F3377077777778803
        30807F333333337FF73707888887880007707F3FFFF333777F37070000878807
        07807F777733337F7F3707888887880808807F333333337F7F37077777778800
        08807F333FFF337773F7088800088803308073FF777FFF733737300008000033
        33003777737777333377333080333333333333F7373333333333300803333333
        33333773733333333333088033333333333373F7F33333333333308033333333
        3333373733333333333333033333333333333373333333333333}
      NumGlyphs = 2
      TabOrder = 9
      OnClick = BitBtn6Click
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 1444
    Width = 293
    Height = 41
    TabOrder = 12
    object Label21: TLabel
      Left = 11
      Top = 7
      Width = 69
      Height = 13
      Caption = 'Zero tolerance'
    end
    object Edit15: TEdit
      Left = 86
      Top = 7
      Width = 43
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = Edit15Change
    end
    object ComboBox2: TComboBox
      Left = 164
      Top = 8
      Width = 116
      Height = 21
      TabOrder = 1
      Text = 'ComboBox2'
      OnChange = ComboBox2Change
    end
  end
  object Panel9: TPanel
    Left = 3
    Top = 1491
    Width = 294
    Height = 150
    TabOrder = 13
    object Label3: TLabel
      Left = 166
      Top = 41
      Width = 31
      Height = 13
      Caption = 'lable 3'
    end
    object Label5: TLabel
      Left = 164
      Top = 60
      Width = 31
      Height = 13
      Caption = 'label 5'
    end
    object Label27: TLabel
      Left = 14
      Top = 5
      Width = 116
      Height = 13
      Caption = 'Numeric field for coloring'
    end
    object NumbersComboBox2: TComboBox
      Left = 149
      Top = 14
      Width = 139
      Height = 21
      AutoDropDown = True
      TabOrder = 0
      OnChange = NumbersComboBox2Change
    end
    object BitBtn18: TBitBtn
      Left = 53
      Top = 55
      Width = 58
      Height = 25
      Caption = 'Histogram'
      TabOrder = 1
      OnClick = BitBtn18Click
    end
    object BitBtn5: TBitBtn
      Left = 3
      Top = 24
      Width = 140
      Height = 25
      Caption = 'Color'
      TabOrder = 2
      OnClick = BitBtn5Click
    end
    object RadioGroup4: TRadioGroup
      Left = 12
      Top = 86
      Width = 280
      Height = 56
      Caption = 'Color Scaling'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Constant'
        'DB fiield linear'
        'DB field quantile'
        'DB field log')
      TabOrder = 3
      OnClick = RadioGroup4Click
    end
  end
  object Panel12Icons: TPanel
    Left = 6
    Top = 1647
    Width = 297
    Height = 41
    TabOrder = 14
    object Label22: TLabel
      Left = 10
      Top = 10
      Width = 74
      Height = 13
      Caption = 'Icon scaling (%)'
    end
    object Edit3: TEdit
      Left = 90
      Top = 10
      Width = 39
      Height = 21
      TabOrder = 0
      OnChange = Edit3Change
    end
    object BitBtn1: TBitBtn
      Left = 176
      Top = 2
      Width = 94
      Height = 33
      Caption = 'Icon'
      TabOrder = 1
      OnClick = BitBtn1Click
    end
  end
  object PanelDBASdefault: TPanel
    Left = 6
    Top = 1694
    Width = 297
    Height = 59
    TabOrder = 15
    object SpeedButton1: TSpeedButton
      Left = 176
      Top = 15
      Width = 18
      Height = 18
      Caption = '>'
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 152
      Top = 15
      Width = 18
      Height = 18
      Caption = '<'
      OnClick = SpeedButton2Click
    end
    object Size: TLabel
      Left = 269
      Top = 16
      Width = 20
      Height = 13
      Caption = 'Size'
    end
    object Label28: TLabel
      Left = 200
      Top = 16
      Width = 34
      Height = 13
      Caption = 'Symbol'
    end
    object Button1: TButton
      Left = 98
      Top = 16
      Width = 48
      Height = 25
      Caption = 'Color'
      TabOrder = 0
      OnClick = Button1Click
    end
    object BitBtnPoint: TBitBtn
      Left = 7
      Top = 0
      Width = 74
      Height = 49
      TabOrder = 1
    end
    object BitBtn7: TBitBtn
      Left = 240
      Top = 0
      Width = 23
      Height = 20
      Caption = #8593
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = BitBtn7Click
    end
    object BitBtn26: TBitBtn
      Left = 240
      Top = 26
      Width = 23
      Height = 20
      Caption = #8595
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = BitBtn26Click
    end
  end
  object PanelPolygon: TPanel
    Left = 2
    Top = 1790
    Width = 296
    Height = 41
    TabOrder = 16
    object BitBtn23: TBitBtn
      Left = 4
      Top = 0
      Width = 81
      Height = 34
      Caption = 'Area'
      TabOrder = 0
      OnClick = BitBtn23Click
    end
  end
  object PanelLine: TPanel
    Left = 2
    Top = 1837
    Width = 291
    Height = 41
    TabOrder = 17
    object BitBtn4: TBitBtn
      Left = 5
      Top = 0
      Width = 90
      Height = 33
      Caption = 'Line'
      TabOrder = 0
      OnClick = BitBtn4Click
    end
  end
  object PanelColorByString: TPanel
    Left = 2
    Top = 215
    Width = 294
    Height = 48
    TabOrder = 18
    object Label6: TLabel
      Left = 167
      Top = 28
      Width = 32
      Height = 13
      Caption = 'Label6'
    end
    object Label26: TLabel
      Left = 11
      Top = 0
      Width = 104
      Height = 13
      Caption = 'String field for coloring'
    end
    object StringsComboBox2: TComboBox
      Left = 6
      Top = 20
      Width = 139
      Height = 21
      TabOrder = 0
      OnChange = StringsComboBox2Change
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 1900
    Width = 296
    Height = 42
    TabOrder = 19
    object Label13: TLabel
      Left = 7
      Top = 0
      Width = 74
      Height = 13
      Caption = 'Legend caption'
    end
    object Edit19: TEdit
      Left = 96
      Top = 11
      Width = 86
      Height = 21
      TabOrder = 0
      OnChange = Edit19Change
    end
  end
  object Panel12: TPanel
    Left = 0
    Top = 1948
    Width = 303
    Height = 157
    TabOrder = 20
    object qf2ComboBox9: TComboBox
      Left = 88
      Top = 48
      Width = 115
      Height = 21
      TabOrder = 0
    end
    object qfBitBtn4: TBitBtn
      Left = 209
      Top = 56
      Width = 17
      Height = 15
      Caption = 'V'
      TabOrder = 1
      OnClick = qfBitBtn4Click
    end
    object qfBitBtn3: TBitBtn
      Left = 209
      Top = 43
      Width = 17
      Height = 15
      Caption = '^'
      TabOrder = 2
      OnClick = qfBitBtn3Click
    end
    object qfBitBtn2: TBitBtn
      Left = 212
      Top = 22
      Width = 17
      Height = 15
      Caption = 'V'
      TabOrder = 3
      OnClick = qfBitBtn2Click
    end
    object qfBitBtn1: TBitBtn
      Left = 212
      Top = 9
      Width = 17
      Height = 15
      Caption = '^'
      TabOrder = 4
      OnClick = qfBitBtn1Click
    end
    object qf1ComboBox10: TComboBox
      Left = 89
      Top = 11
      Width = 117
      Height = 21
      TabOrder = 5
      OnChange = qf1ComboBox10Change
    end
    object qfCheckBox3: TCheckBox
      Left = 11
      Top = 128
      Width = 74
      Height = 17
      Caption = 'Show n='
      TabOrder = 6
    end
    object qfRadioGroup1: TRadioGroup
      Left = 194
      Top = 119
      Width = 92
      Height = 35
      Caption = 'Filter'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'AND'
        'OR')
      TabOrder = 7
    end
    object CheckBox18: TCheckBox
      Left = 246
      Top = 48
      Width = 57
      Height = 17
      Caption = 'Field 2'
      TabOrder = 8
    end
    object Panel14: TPanel
      Left = 0
      Top = 0
      Width = 303
      Height = 157
      TabOrder = 9
      object qflabel2: TLabel
        Left = 15
        Top = 50
        Width = 41
        Height = 13
        Caption = 'qfLabel8'
      end
      object qflabel1: TLabel
        Left = 17
        Top = 16
        Width = 41
        Height = 13
        Caption = 'qfLabel7'
      end
      object qflabel3: TLabel
        Left = 17
        Top = 79
        Width = 41
        Height = 13
        Caption = 'qfLabel8'
      end
      object qfcombo2: TComboBox
        Left = 89
        Top = 43
        Width = 115
        Height = 21
        TabOrder = 0
        OnChange = qfcombo2Change
      end
      object BitBtn28: TBitBtn
        Left = 210
        Top = 56
        Width = 17
        Height = 15
        Caption = 'V'
        TabOrder = 1
      end
      object BitBtn29: TBitBtn
        Left = 212
        Top = 43
        Width = 17
        Height = 15
        Caption = '^'
        TabOrder = 2
      end
      object BitBtn30: TBitBtn
        Left = 212
        Top = 22
        Width = 17
        Height = 15
        Caption = 'V'
        TabOrder = 3
        OnClick = qfBitBtn2Click
      end
      object BitBtn31: TBitBtn
        Left = 212
        Top = 8
        Width = 17
        Height = 16
        Caption = '^'
        TabOrder = 4
        OnClick = qfBitBtn1Click
      end
      object qfcombo1: TComboBox
        Left = 89
        Top = 16
        Width = 117
        Height = 21
        TabOrder = 5
        OnChange = qfcombo1Change
      end
      object CheckBox19: TCheckBox
        Left = 11
        Top = 128
        Width = 74
        Height = 17
        Caption = 'Show n='
        TabOrder = 6
      end
      object CheckBox20: TCheckBox
        Left = 91
        Top = 128
        Width = 74
        Height = 17
        Caption = 'Show filter'
        TabOrder = 7
      end
      object RadioGroup2: TRadioGroup
        Left = 171
        Top = 111
        Width = 115
        Height = 35
        Caption = 'Filter'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'AND'
          'OR')
        TabOrder = 8
      end
      object qfcheck2: TCheckBox
        Left = 246
        Top = 48
        Width = 57
        Height = 17
        Caption = 'Field 2'
        TabOrder = 9
        OnClick = qfcheck2Click
      end
      object qfcheck3: TCheckBox
        Left = 246
        Top = 71
        Width = 57
        Height = 17
        Caption = 'Field 3'
        TabOrder = 10
        OnClick = qfcheck3Click
      end
      object qfBitBtn5: TBitBtn
        Left = 210
        Top = 77
        Width = 17
        Height = 15
        Caption = '^'
        TabOrder = 11
        OnClick = qfBitBtn5Click
      end
      object qfBitBtn6: TBitBtn
        Left = 210
        Top = 90
        Width = 17
        Height = 15
        Caption = 'V'
        TabOrder = 12
        OnClick = qfBitBtn6Click
      end
      object qfcombo3: TComboBox
        Left = 89
        Top = 77
        Width = 115
        Height = 21
        TabOrder = 13
        OnChange = qfcombo3Change
      end
      object qfcheck1: TCheckBox
        Left = 246
        Top = 25
        Width = 57
        Height = 17
        Caption = 'Field 1'
        TabOrder = 14
        OnClick = qfcheck1Click
      end
    end
  end
  object ColorDialog1: TColorDialog
    Left = 16
    Top = 2160
  end
end
