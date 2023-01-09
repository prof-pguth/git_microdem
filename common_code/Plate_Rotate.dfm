object PickRotationForm: TPickRotationForm
  Left = 1935
  Top = 178
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Plate rotations'
  ClientHeight = 967
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefaultSizeOnly
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 321
    Width = 260
    Height = 76
    Align = alTop
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 266
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 584
    Width = 260
    Height = 106
    Align = alTop
    Caption = 'Spreading Rates'
    TabOrder = 1
    ExplicitWidth = 266
    object Label13: TLabel
      Left = 136
      Top = 16
      Width = 103
      Height = 13
      Caption = 'Current rotation model'
    end
    object Label14: TLabel
      Left = 136
      Top = 40
      Width = 33
      Height = 13
      Caption = 'Plate 1'
    end
    object Label15: TLabel
      Left = 136
      Top = 64
      Width = 33
      Height = 13
      Caption = 'Plate 2'
    end
    object Label16: TLabel
      Left = 136
      Top = 88
      Width = 33
      Height = 13
      Caption = 'Plate 3'
    end
    object Plate1ComboBox: TComboBox
      Left = 9
      Top = 37
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = Plate1ComboBoxChange
    end
    object Plate2ComboBox: TComboBox
      Left = 9
      Top = 58
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object ModelComboBox: TComboBox
      Left = 9
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 2
      OnChange = ModelComboBoxChange
    end
    object Plate3ComboBox: TComboBox
      Left = 9
      Top = 77
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object BitBtn7: TBitBtn
      Left = 188
      Top = 75
      Width = 51
      Height = 25
      Caption = 'Vectors'
      TabOrder = 4
      OnClick = BitBtn7Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 905
    Width = 260
    Height = 41
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 266
    object BitBtn12: TBitBtn
      Left = 118
      Top = 3
      Width = 48
      Height = 25
      Caption = 'Motion '
      TabOrder = 0
      OnClick = BitBtn12Click
    end
    object HelpBtn: TBitBtn
      Left = 191
      Top = 3
      Width = 59
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 254
    Height = 155
    Align = alTop
    Alignment = taRightJustify
    TabOrder = 3
    ExplicitWidth = 260
    object Label12: TLabel
      Left = 7
      Top = 120
      Width = 73
      Height = 13
      Caption = 'Velocity vector '
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 4
      Width = 129
      Height = 110
      Caption = 'Option'
      Items.Strings = (
        'Current vectors'
        'Past total poles'
        'Arbitrary rotation'
        'Two plate motions'
        'Triple junction')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object TrackBar1: TTrackBar
      Left = 86
      Top = 120
      Width = 80
      Height = 32
      Max = 25
      Min = 1
      Frequency = 5
      Position = 3
      TabOrder = 1
      OnChange = TrackBar1Change
    end
    object CheckBox4: TCheckBox
      Left = 153
      Top = 21
      Width = 97
      Height = 17
      Caption = 'Label velocities'
      TabOrder = 2
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 153
      Top = 36
      Width = 104
      Height = 17
      Caption = 'Resultant vector'
      TabOrder = 3
      OnClick = CheckBox5Click
    end
    object CheckBox1: TCheckBox
      Left = 153
      Top = 5
      Width = 97
      Height = 17
      Caption = 'Velocity diagram'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object CheckBox8: TCheckBox
      Left = 153
      Top = 67
      Width = 97
      Height = 17
      Caption = 'Velocity scalebar'
      TabOrder = 5
      OnClick = CheckBox8Click
    end
    object CheckBox9: TCheckBox
      Left = 153
      Top = 82
      Width = 97
      Height = 17
      Caption = 'Plate numbers'
      TabOrder = 6
      OnClick = CheckBox9Click
    end
    object CheckBox10: TCheckBox
      Left = 153
      Top = 52
      Width = 97
      Height = 17
      Caption = 'Plate vectors'
      TabOrder = 7
      OnClick = CheckBox10Click
    end
    object BitBtn2: TBitBtn
      Left = 172
      Top = 127
      Width = 75
      Height = 25
      Caption = 'Line'
      TabOrder = 8
      OnClick = BitBtn2Click
    end
    object BitBtn13: TBitBtn
      Left = 172
      Top = 105
      Width = 70
      Height = 20
      Caption = 'Font'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333FFF33FFFFF33333300033000
        00333337773377777333333330333300033333337FF33777F333333330733300
        0333333377FFF777F33333333700000073333333777777773333333333033000
        3333333337FF777F333333333307300033333333377F777F3333333333703007
        33333333377F7773333333333330000333333333337777F33333333333300003
        33333333337777F3333333333337007333333333337777333333333333330033
        3333333333377333333333333333033333333333333733333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      TabOrder = 9
      OnClick = BitBtn13Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 161
    Width = 260
    Height = 160
    Align = alTop
    Alignment = taLeftJustify
    TabOrder = 4
    ExplicitWidth = 266
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 92
      Height = 13
      Caption = 'Pole of rotation, Lat'
    end
    object Label2: TLabel
      Left = 80
      Top = 32
      Width = 24
      Height = 13
      Caption = 'Long'
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 82
      Height = 13
      Caption = 'Rotation angle ('#176')'
    end
    object Label7: TLabel
      Left = 8
      Top = 88
      Width = 94
      Height = 13
      Caption = 'Rotation rate ('#176'/Ma)'
    end
    object Label8: TLabel
      Left = 8
      Top = 104
      Width = 105
      Height = 13
      Caption = 'Rotation duration (Ma)'
    end
    object Label4: TLabel
      Left = 176
      Top = 8
      Width = 30
      Height = 13
      Caption = 'N+, S-'
    end
    object Label5: TLabel
      Left = 176
      Top = 32
      Width = 33
      Height = 13
      Caption = 'E+, W-'
    end
    object Label11: TLabel
      Left = 175
      Top = 59
      Width = 64
      Height = 13
      Caption = 'CCW +, CW -'
    end
    object Edit5: TEdit
      Left = 128
      Top = 104
      Width = 41
      Height = 21
      TabOrder = 0
      Text = '5'
      OnChange = Edit5Change
    end
    object Edit4: TEdit
      Left = 112
      Top = 80
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '0.5'
    end
    object Edit3: TEdit
      Left = 112
      Top = 56
      Width = 57
      Height = 21
      TabOrder = 2
      Text = '30'
      OnChange = Edit3Change
    end
    object Edit2: TEdit
      Left = 112
      Top = 32
      Width = 57
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = Edit2Change
    end
    object Edit1: TEdit
      Left = 112
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 4
      Text = '90'
      OnChange = Edit1Change
    end
    object TrackBar2: TTrackBar
      Left = 175
      Top = 83
      Width = 81
      Height = 45
      LineSize = 5
      Max = 360
      Frequency = 30
      TabOrder = 5
      OnChange = TrackBar2Change
    end
    object BitBtn8: TBitBtn
      Left = 132
      Top = 129
      Width = 65
      Height = 25
      Caption = 'Show pole'
      TabOrder = 6
      OnClick = BitBtn8Click
    end
    object BitBtn10: TBitBtn
      Left = 16
      Top = 129
      Width = 65
      Height = 25
      Caption = 'Pick pole'
      TabOrder = 7
      OnClick = BitBtn10Click
    end
    object BitBtn1: TBitBtn
      Left = 80
      Top = 129
      Width = 52
      Height = 25
      Caption = 'Rotate'
      TabOrder = 8
      OnClick = BitBtn1Click
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 823
    Width = 254
    Height = 79
    Align = alTop
    Caption = 'Map'
    TabOrder = 5
    ExplicitWidth = 260
    object BitBtn4: TBitBtn
      Left = 167
      Top = 12
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
      TabOrder = 0
      OnClick = BitBtn4Click
    end
    object CheckBox2: TCheckBox
      Left = 23
      Top = 56
      Width = 106
      Height = 17
      Caption = 'Continental crust'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 142
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Plate boundaries'
      TabOrder = 2
      OnClick = CheckBox3Click
    end
    object CheckBox6: TCheckBox
      Left = 142
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Subdue base'
      TabOrder = 3
      OnClick = CheckBox6Click
    end
    object CheckBox7: TCheckBox
      Left = 23
      Top = 36
      Width = 97
      Height = 17
      Caption = 'Map outlines'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CheckBox7Click
    end
  end
  object Panel4: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 400
    Width = 254
    Height = 181
    Align = alTop
    Alignment = taRightJustify
    Color = clWhite
    ParentBackground = False
    TabOrder = 6
    ExplicitWidth = 260
    object ComboBox6: TComboBox
      Left = 142
      Top = 13
      Width = 88
      Height = 21
      ItemIndex = 0
      TabOrder = 0
      Text = 'Ridge'
      OnChange = ComboBox6Change
      Items.Strings = (
        'Ridge'
        'Trench'
        'Transform fault'
        '')
    end
    object ComboBox7: TComboBox
      Left = 135
      Top = 67
      Width = 95
      Height = 21
      ItemIndex = 0
      TabOrder = 1
      Text = 'Ridge'
      OnChange = ComboBox7Change
      Items.Strings = (
        'Ridge'
        'Trench'
        'Transform fault'
        '')
    end
    object ComboBox8: TComboBox
      Left = 137
      Top = 118
      Width = 93
      Height = 21
      ItemIndex = 0
      TabOrder = 2
      Text = 'Ridge'
      OnChange = ComboBox8Change
      Items.Strings = (
        'Ridge'
        'Trench'
        'Transform fault'
        '')
    end
    object ComboBox9: TComboBox
      Left = 77
      Top = 40
      Width = 89
      Height = 21
      Enabled = False
      ItemIndex = 0
      TabOrder = 3
      Text = 'P1 subducts'
      Items.Strings = (
        'P1 subducts'
        'P2 subducts'
        '')
    end
    object ComboBox10: TComboBox
      Left = 74
      Top = 94
      Width = 89
      Height = 21
      Enabled = False
      TabOrder = 4
      Text = 'P1 subducts'
      Items.Strings = (
        'P1 subducts'
        'P3 subducts'
        '')
    end
    object ComboBox11: TComboBox
      Left = 77
      Top = 145
      Width = 89
      Height = 21
      Enabled = False
      ItemIndex = 0
      TabOrder = 5
      Text = 'P2 subducts'
      Items.Strings = (
        'P2 subducts'
        'P3 subducts'
        '')
    end
    object BitBtn14: TBitBtn
      Left = 7
      Top = 145
      Width = 64
      Height = 25
      Caption = 'TJ motion'
      Enabled = False
      TabOrder = 6
      OnClick = BitBtn14Click
    end
    object Edit6: TEdit
      Left = 172
      Top = 40
      Width = 67
      Height = 21
      Enabled = False
      TabOrder = 7
      Text = '90'
    end
    object Edit7: TEdit
      Left = 169
      Top = 94
      Width = 67
      Height = 21
      Enabled = False
      TabOrder = 8
      Text = '45'
    end
    object Edit8: TEdit
      Left = 172
      Top = 145
      Width = 67
      Height = 21
      Enabled = False
      TabOrder = 9
      Text = '135'
    end
    object BitBtn16: TBitBtn
      Left = 6
      Top = 12
      Width = 130
      Height = 22
      Caption = 'P1-P2 boundary'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = BitBtn16Click
    end
    object BitBtn17: TBitBtn
      Left = 2
      Top = 66
      Width = 127
      Height = 22
      Caption = 'P12-P3 boundary'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnClick = BitBtn17Click
    end
    object BitBtn18: TBitBtn
      Left = 2
      Top = 117
      Width = 129
      Height = 22
      Caption = 'P2-P3 boundary'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = BitBtn18Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 690
    Width = 260
    Height = 130
    Align = alTop
    Caption = 'Total pole reconstruction'
    TabOrder = 7
    ExplicitWidth = 266
    object Label6: TLabel
      Left = 130
      Top = 79
      Width = 47
      Height = 13
      Caption = 'Time (Ma)'
    end
    object Label9: TLabel
      Left = 132
      Top = 47
      Width = 45
      Height = 13
      Caption = 'Continent'
    end
    object Label10: TLabel
      Left = 132
      Top = 22
      Width = 83
      Height = 13
      Caption = 'Total poles model'
    end
    object TotalPolesComboBox: TComboBox
      Left = 5
      Top = 20
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = TotalPolesComboBoxChange
    end
    object TimeComboBox: TComboBox
      Left = 3
      Top = 74
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object ContinentComboBox: TComboBox
      Left = 5
      Top = 47
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object BitBtn9: TBitBtn
      Left = 138
      Top = 98
      Width = 49
      Height = 25
      Caption = 'Movie'
      TabOrder = 3
      OnClick = BitBtn9Click
    end
    object BitBtn6: TBitBtn
      Left = 75
      Top = 98
      Width = 57
      Height = 25
      Caption = 'Time'
      TabOrder = 4
      OnClick = BitBtn6Click
    end
    object BitBtn5: TBitBtn
      Left = 27
      Top = 98
      Width = 49
      Height = 25
      Caption = 'Track'
      TabOrder = 5
      OnClick = BitBtn5Click
    end
  end
end
