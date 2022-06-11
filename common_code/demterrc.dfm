object GetTerrC: TGetTerrC
  Left = 632
  Top = 247
  BorderStyle = bsDialog
  Caption = 'Terrain Category Parameters'
  ClientHeight = 858
  ClientWidth = 324
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 261
    Width = 303
    Height = 91
    Caption = 'Aspect'
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 67
      Top = 16
      Width = 45
      Height = 17
      Caption = 'N'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 118
      Top = 16
      Width = 45
      Height = 17
      Caption = 'NE'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 120
      Top = 39
      Width = 45
      Height = 17
      Caption = 'E'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 120
      Top = 62
      Width = 45
      Height = 17
      Caption = 'SE'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox5: TCheckBox
      Left = 69
      Top = 62
      Width = 45
      Height = 17
      Caption = 'S'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBox6: TCheckBox
      Left = 18
      Top = 62
      Width = 45
      Height = 17
      Caption = 'SW'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBox7: TCheckBox
      Left = 16
      Top = 39
      Width = 40
      Height = 17
      Caption = 'W'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 16
      Width = 45
      Height = 17
      Caption = 'NW'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object CheckBox9: TCheckBox
      Left = 203
      Top = 62
      Width = 50
      Height = 17
      Caption = 'Any'
      TabOrder = 8
      OnClick = CheckBox9Click
    end
    object CheckBox11: TCheckBox
      Left = 205
      Top = 16
      Width = 49
      Height = 17
      Caption = 'Pit'
      TabOrder = 9
    end
    object CheckBox12: TCheckBox
      Left = 205
      Top = 39
      Width = 45
      Height = 17
      Caption = 'Flat'
      TabOrder = 10
    end
  end
  object Panel1: TPanel
    Left = -12
    Top = 0
    Width = 315
    Height = 57
    TabOrder = 1
    object CheckBox15: TCheckBox
      Left = 17
      Top = 31
      Width = 97
      Height = 17
      Caption = 'Slope'
      TabOrder = 0
      OnClick = CheckBox15Click
    end
    object CheckBox13: TCheckBox
      Left = 17
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Elevation'
      TabOrder = 1
      OnClick = CheckBox13Click
    end
    object CheckBox16: TCheckBox
      Left = 105
      Top = 8
      Width = 107
      Height = 17
      Caption = 'Aspect (simple)'
      TabOrder = 2
      OnClick = CheckBox16Click
    end
    object CheckBox14: TCheckBox
      Left = 218
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Relief'
      TabOrder = 3
      OnClick = CheckBox14Click
    end
    object CheckBox17: TCheckBox
      Left = 104
      Top = 32
      Width = 115
      Height = 17
      Caption = 'Aspect (detailed)'
      TabOrder = 4
      OnClick = CheckBox17Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 63
    Width = 303
    Height = 98
    Align = alCustom
    TabOrder = 2
    object Label2: TLabel
      Left = 99
      Top = 0
      Width = 24
      Height = 13
      Caption = 'Max'
    end
    object Label1: TLabel
      Left = 7
      Top = 5
      Width = 21
      Height = 13
      Caption = 'Min'
    end
    object Label3: TLabel
      Left = 163
      Top = 22
      Width = 47
      Height = 13
      Caption = 'Elev (m)'
    end
    object Label8: TLabel
      Left = 169
      Top = 46
      Width = 34
      Height = 13
      Caption = '(Feet)'
    end
    object Label6: TLabel
      Left = 169
      Top = 65
      Width = 58
      Height = 13
      Caption = 'Percentile'
    end
    object Label10: TLabel
      Left = 98
      Top = 46
      Width = 46
      Height = 13
      Caption = 'Label10'
    end
    object Label9: TLabel
      Left = 7
      Top = 46
      Width = 39
      Height = 13
      Caption = 'Label9'
    end
    object Label16: TLabel
      Left = 5
      Top = 65
      Width = 46
      Height = 13
      Caption = 'Label16'
    end
    object Label17: TLabel
      Left = 98
      Top = 65
      Width = 46
      Height = 13
      Caption = 'Label17'
    end
    object Edit2: TEdit
      Left = 85
      Top = 19
      Width = 72
      Height = 21
      TabOrder = 0
      OnChange = Edit2Change
    end
    object Edit1: TEdit
      Left = 7
      Top = 19
      Width = 72
      Height = 21
      TabOrder = 1
      OnChange = Edit1Change
    end
  end
  object Panel3: TPanel
    Left = 1
    Top = 167
    Width = 302
    Height = 95
    TabOrder = 3
    object Label4: TLabel
      Left = 6
      Top = 31
      Width = 54
      Height = 13
      Caption = 'Slope (%)'
    end
    object Label5: TLabel
      Left = 42
      Top = 58
      Width = 14
      Height = 13
      Caption = '('#176')'
    end
    object Label12: TLabel
      Left = 183
      Top = 5
      Width = 24
      Height = 13
      Caption = 'Max'
    end
    object Label13: TLabel
      Left = 92
      Top = 5
      Width = 21
      Height = 13
      Caption = 'Min'
    end
    object Edit3: TEdit
      Left = 75
      Top = 24
      Width = 72
      Height = 21
      TabOrder = 0
      OnChange = Edit3Change
    end
    object Edit8: TEdit
      Left = 75
      Top = 50
      Width = 72
      Height = 21
      TabOrder = 1
      OnChange = Edit8Change
    end
    object Edit4: TEdit
      Left = 153
      Top = 24
      Width = 73
      Height = 21
      TabOrder = 2
      OnChange = Edit4Change
    end
    object Edit9: TEdit
      Left = 153
      Top = 50
      Width = 73
      Height = 21
      TabOrder = 3
      OnChange = Edit9Change
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 358
    Width = 303
    Height = 97
    TabOrder = 4
    object Label7: TLabel
      Left = 18
      Top = 37
      Width = 55
      Height = 13
      Caption = 'Relief (m)'
    end
    object Label11: TLabel
      Left = 58
      Top = 69
      Width = 61
      Height = 13
      Caption = 'Radius (m)'
    end
    object Label14: TLabel
      Left = 92
      Top = 13
      Width = 21
      Height = 13
      Caption = 'Min'
    end
    object Label15: TLabel
      Left = 183
      Top = 13
      Width = 24
      Height = 13
      Caption = 'Max'
    end
    object Edit5: TEdit
      Left = 79
      Top = 32
      Width = 72
      Height = 21
      TabOrder = 0
    end
    object Edit7: TEdit
      Left = 125
      Top = 69
      Width = 72
      Height = 21
      TabOrder = 1
    end
    object Edit6: TEdit
      Left = 157
      Top = 32
      Width = 72
      Height = 21
      TabOrder = 2
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 578
    Width = 303
    Height = 98
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object Panel5: TPanel
    Left = 0
    Top = 682
    Width = 303
    Height = 161
    TabOrder = 6
    object BitBtn3: TBitBtn
      Left = 85
      Top = 32
      Width = 78
      Height = 25
      Caption = 'Points'
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object BitBtn1: TBitBtn
      Left = 1
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Color'
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object CheckBox10: TCheckBox
      Left = 129
      Top = 9
      Width = 81
      Height = 17
      Caption = 'IHS merge'
      TabOrder = 2
      OnClick = CheckBox10Click
    end
    object OKBtn: TBitBtn
      Left = -1
      Top = 125
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
      TabOrder = 3
      OnClick = OKBtnClick
      IsControl = True
    end
    object BitBtn2: TBitBtn
      Left = 1
      Top = 63
      Width = 75
      Height = 25
      Caption = 'Plot'
      TabOrder = 4
      OnClick = BitBtn2Click
    end
    object CancelBtn: TBitBtn
      Left = 82
      Top = 125
      Width = 87
      Height = 27
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 5
      OnClick = CancelBtnClick
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 175
      Top = 126
      Width = 77
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 6
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn4: TBitBtn
      Left = 169
      Top = 32
      Width = 84
      Height = 25
      Caption = 'Add cat'
      TabOrder = 7
      OnClick = BitBtn4Click
    end
    object BitBtn7: TBitBtn
      Left = 88
      Top = 63
      Width = 75
      Height = 25
      Caption = 'Export grid'
      TabOrder = 8
      OnClick = BitBtn7Click
    end
    object BitBtn8: TBitBtn
      Left = -2
      Top = 94
      Width = 78
      Height = 25
      Caption = 'Mask  cat in'
      TabOrder = 9
      OnClick = BitBtn8Click
    end
    object BitBtn9: TBitBtn
      Left = 82
      Top = 95
      Width = 87
      Height = 25
      Caption = 'Mask  cat out'
      TabOrder = 10
      OnClick = BitBtn9Click
    end
    object BitBtn10: TBitBtn
      Left = 169
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Export DB'
      TabOrder = 11
      OnClick = BitBtn10Click
    end
    object CheckBox18: TCheckBox
      Left = 26
      Top = 9
      Width = 97
      Height = 17
      Caption = 'Legend'
      TabOrder = 12
      OnClick = CheckBox18Click
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 461
    Width = 303
    Height = 140
    TabOrder = 7
    object Label18: TLabel
      Left = 15
      Top = 40
      Width = 60
      Height = 13
      Caption = 'Min dip dir'
    end
    object Label19: TLabel
      Left = 15
      Top = 64
      Width = 63
      Height = 13
      Caption = 'Max dip dir'
    end
    object Label20: TLabel
      Left = 15
      Top = 86
      Width = 72
      Height = 13
      Caption = 'Min slope ('#176')'
    end
    object Label21: TLabel
      Left = 17
      Top = 113
      Width = 75
      Height = 13
      Caption = 'Max slope ('#176')'
    end
    object Edit10: TEdit
      Left = 96
      Top = 32
      Width = 52
      Height = 21
      TabOrder = 0
    end
    object Edit11: TEdit
      Left = 95
      Top = 59
      Width = 53
      Height = 21
      TabOrder = 1
    end
    object Edit12: TEdit
      Left = 184
      Top = 32
      Width = 51
      Height = 21
      TabOrder = 2
    end
    object Edit13: TEdit
      Left = 182
      Top = 61
      Width = 53
      Height = 21
      TabOrder = 3
    end
    object BitBtn5: TBitBtn
      Left = 82
      Top = 1
      Width = 66
      Height = 25
      Caption = 'FP1'
      TabOrder = 4
      OnClick = BitBtn5Click
    end
    object BitBtn6: TBitBtn
      Left = 175
      Top = 1
      Width = 67
      Height = 25
      Caption = 'FP2'
      TabOrder = 5
      OnClick = BitBtn6Click
    end
    object Edit14: TEdit
      Left = 95
      Top = 86
      Width = 53
      Height = 21
      TabOrder = 6
      Text = '0'
    end
    object Edit15: TEdit
      Left = 182
      Top = 88
      Width = 53
      Height = 21
      TabOrder = 7
      Text = '0'
    end
    object Edit16: TEdit
      Left = 98
      Top = 113
      Width = 50
      Height = 21
      TabOrder = 8
    end
    object Edit17: TEdit
      Left = 182
      Top = 115
      Width = 53
      Height = 21
      TabOrder = 9
    end
  end
end
