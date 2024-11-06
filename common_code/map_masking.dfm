object MapMaskForm: TMapMaskForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Map Masking'
  ClientHeight = 658
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label6: TLabel
    Left = 152
    Top = 16
    Width = 3
    Height = 13
  end
  object Label7: TLabel
    Left = 152
    Top = 45
    Width = 3
    Height = 13
  end
  object Label9: TLabel
    Left = 222
    Top = 93
    Width = 3
    Height = 13
  end
  object Label10: TLabel
    Left = 222
    Top = 112
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 40
    Top = 176
    Width = 3
    Height = 13
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 304
    Width = 293
    Height = 249
    TabOrder = 0
    object Label11: TLabel
      Left = 77
      Top = 188
      Width = 37
      Height = 13
      Caption = 'Label11'
    end
    object Label12: TLabel
      Left = 230
      Top = 70
      Width = 49
      Height = 13
      Caption = 'Buffer (m)'
    end
    object Label3: TLabel
      Left = 77
      Top = 216
      Width = 31
      Height = 13
      Caption = 'Label3'
    end
    object RedrawSpeedButton12: TSpeedButton
      Left = 147
      Top = 13
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
    object BitBtn1: TBitBtn
      Left = 8
      Top = 188
      Width = 63
      Height = 25
      Caption = 'Directory'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object RadioGroup3: TRadioGroup
      Left = 2
      Top = 76
      Width = 222
      Height = 34
      Caption = 'Point Mask--leave in grid/color'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Near points'
        'Away from points')
      TabOrder = 1
      OnClick = RadioGroup3Click
    end
    object RadioGroup4: TRadioGroup
      Left = 2
      Top = 116
      Width = 222
      Height = 30
      Caption = 'Line Mask--leave in grid/color'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Near lines'
        'Away from lines')
      TabOrder = 2
      OnClick = RadioGroup4Click
    end
    object RadioGroup5: TRadioGroup
      Left = 2
      Top = 152
      Width = 222
      Height = 30
      Caption = 'Area Mask--leave in grid/color'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Inside areas'
        'Outside areas')
      TabOrder = 3
      OnClick = RadioGroup5Click
    end
    object Edit7: TEdit
      Left = 230
      Top = 89
      Width = 40
      Height = 21
      TabOrder = 4
      Text = '250'
    end
    object Edit8: TEdit
      Left = 230
      Top = 125
      Width = 40
      Height = 21
      TabOrder = 5
      Text = '250'
    end
    object Edit9: TEdit
      Left = 230
      Top = 157
      Width = 40
      Height = 21
      TabOrder = 6
      Text = '250'
    end
    object BitBtn8: TBitBtn
      Left = 194
      Top = 15
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 7
      OnClick = BitBtn8Click
    end
    object CheckBox3: TCheckBox
      Left = 18
      Top = 5
      Width = 123
      Height = 17
      Caption = 'Shapefile masking'
      TabOrder = 8
    end
    object BitBtn16: TBitBtn
      Left = 10
      Top = 218
      Width = 61
      Height = 25
      Caption = 'File'
      TabOrder = 9
      OnClick = BitBtn16Click
    end
    object CheckBox12: TCheckBox
      Left = 18
      Top = 24
      Width = 114
      Height = 17
      Caption = 'Symbols plus buffer'
      TabOrder = 10
      OnClick = CheckBox12Click
    end
    object CheckBox13: TCheckBox
      Left = 18
      Top = 47
      Width = 97
      Height = 17
      Caption = 'Show buffers'
      TabOrder = 11
      OnClick = CheckBox13Click
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 293
    Height = 41
    TabOrder = 1
    object BitBtn3: TBitBtn
      Left = 194
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object BitBtn2: TBitBtn
      Left = 95
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Options'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 3
      Width = 81
      Height = 17
      Caption = 'Ridge mask'
      TabOrder = 2
    end
    object CheckBox10: TCheckBox
      Left = 22
      Top = 22
      Width = 67
      Height = 17
      Caption = 'Exclude'
      TabOrder = 3
      OnClick = CheckBox10Click
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 55
    Width = 293
    Height = 42
    TabOrder = 2
    object Label8: TLabel
      Left = 155
      Top = 9
      Width = 8
      Height = 13
      Caption = 'm'
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 2
      Width = 98
      Height = 17
      Caption = 'TIGER roads'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object BitBtn4: TBitBtn
      Left = 235
      Top = 8
      Width = 45
      Height = 25
      Caption = 'Preview'
      TabOrder = 1
      OnClick = BitBtn4Click
    end
    object BitBtn6: TBitBtn
      Left = 179
      Top = 7
      Width = 50
      Height = 25
      Caption = 'Options'
      TabOrder = 2
      OnClick = BitBtn6Click
    end
    object CheckBox9: TCheckBox
      Left = 37
      Top = 17
      Width = 69
      Height = 17
      Caption = 'Exclude'
      TabOrder = 3
      OnClick = CheckBox9Click
    end
    object Edit3: TEdit
      Left = 112
      Top = 9
      Width = 37
      Height = 21
      TabOrder = 4
      Text = 'Edit1'
    end
  end
  object Panel4: TPanel
    Left = 8
    Top = 150
    Width = 293
    Height = 41
    TabOrder = 3
    object Label5: TLabel
      Left = 137
      Top = 12
      Width = 11
      Height = 13
      Caption = '%'
    end
    object CheckBox4: TCheckBox
      Left = 10
      Top = 11
      Width = 97
      Height = 17
      Caption = 'Slope mask'
      TabOrder = 0
      OnClick = CheckBox4Click
    end
    object Edit5: TEdit
      Left = 90
      Top = 7
      Width = 41
      Height = 21
      TabOrder = 1
    end
    object BitBtn5: TBitBtn
      Left = 193
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 2
      OnClick = BitBtn5Click
    end
  end
  object Panel6: TPanel
    Left = 8
    Top = 255
    Width = 293
    Height = 48
    TabOrder = 4
    object Label2: TLabel
      Left = 163
      Top = 8
      Width = 8
      Height = 13
      Caption = 'm'
    end
    object BitBtn15: TBitBtn
      Left = 194
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 0
      OnClick = BitBtn15Click
    end
    object CheckBox7: TCheckBox
      Left = 8
      Top = 7
      Width = 98
      Height = 17
      Caption = 'TIGER streams'
      TabOrder = 1
      OnClick = CheckBox7Click
    end
    object CheckBox8: TCheckBox
      Left = 31
      Top = 26
      Width = 75
      Height = 17
      Caption = 'Exclude'
      TabOrder = 2
      OnClick = CheckBox8Click
    end
    object Edit1: TEdit
      Left = 120
      Top = 8
      Width = 37
      Height = 21
      TabOrder = 3
      Text = 'Edit1'
    end
  end
  object Panel5: TPanel
    Left = 8
    Top = 197
    Width = 293
    Height = 52
    TabOrder = 5
    object BitBtn13: TBitBtn
      Left = 193
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Preview'
      TabOrder = 0
      OnClick = BitBtn13Click
    end
    object BitBtn14: TBitBtn
      Left = 112
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Options'
      TabOrder = 1
      OnClick = BitBtn14Click
    end
    object CheckBox5: TCheckBox
      Left = 8
      Top = 8
      Width = 98
      Height = 17
      Caption = 'Terrain catetory'
      TabOrder = 2
    end
    object CheckBox11: TCheckBox
      Left = 31
      Top = 26
      Width = 75
      Height = 17
      Caption = 'Exclude'
      TabOrder = 3
      OnClick = CheckBox11Click
    end
  end
  object Panel7: TPanel
    Left = 8
    Top = 569
    Width = 292
    Height = 65
    TabOrder = 6
    object Label4: TLabel
      Left = 143
      Top = 5
      Width = 37
      Height = 13
      Caption = 'Opacity'
    end
    object BitBtn9: TBitBtn
      Left = 15
      Top = 3
      Width = 122
      Height = 25
      Caption = 'Combined previews'
      TabOrder = 0
      OnClick = BitBtn9Click
    end
    object Edit2: TEdit
      Left = 186
      Top = 4
      Width = 26
      Height = 21
      TabOrder = 1
      Text = '50'
    end
    object BitBtn17: TBitBtn
      Left = 218
      Top = 3
      Width = 68
      Height = 25
      Caption = 'Mask'
      TabOrder = 2
      OnClick = BitBtn17Click
    end
    object BitBtn12: TBitBtn
      Left = 199
      Top = 31
      Width = 75
      Height = 25
      Caption = 'Create mask'
      TabOrder = 3
      OnClick = BitBtn12Click
    end
    object BitBtn11: TBitBtn
      Left = 118
      Top = 31
      Width = 75
      Height = 25
      Caption = 'Undo Mask'
      TabOrder = 4
      OnClick = BitBtn11Click
    end
    object BitBtn10: TBitBtn
      Left = 22
      Top = 31
      Width = 75
      Height = 25
      Caption = 'Mask DEM'
      TabOrder = 5
      OnClick = BitBtn10Click
    end
  end
  object Panel8: TPanel
    Left = 8
    Top = 626
    Width = 292
    Height = 41
    TabOrder = 7
    object CancelBtn: TBitBtn
      Left = 94
      Top = 6
      Width = 77
      Height = 27
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = CancelBtnClick
      IsControl = True
    end
    object OKBtn: TBitBtn
      Left = 11
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
      TabOrder = 1
      OnClick = OKBtnClick
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 177
      Top = 6
      Width = 77
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 2
      OnClick = HelpBtnClick
      IsControl = True
    end
  end
end
