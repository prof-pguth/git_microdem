object LOSOption: TLOSOption
  Left = 600
  Top = 350
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Line Of Sight Options'
  ClientHeight = 400
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object SaveSpeedButton: TSpeedButton
    Left = 411
    Top = 347
    Width = 27
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
  object OKBtn: TBitBtn
    Left = 9
    Top = 346
    Width = 56
    Height = 27
    Caption = '&OK'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 71
    Top = 347
    Width = 77
    Height = 27
    Caption = '&Cancel'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 154
    Top = 347
    Width = 63
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object BitBtn11: TBitBtn
    Left = 304
    Top = 347
    Width = 64
    Height = 25
    Caption = 'Defaults'
    TabOrder = 3
    OnClick = BitBtn11Click
  end
  object BitBtn17: TBitBtn
    Left = 223
    Top = 347
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
    TabOrder = 4
    OnClick = BitBtn17Click
  end
  object BitBtn18: TBitBtn
    Left = 444
    Top = 347
    Width = 105
    Height = 25
    Caption = 'Force recalc'
    TabOrder = 5
    OnClick = BitBtn18Click
  end
  object BitBtn22: TBitBtn
    Left = 374
    Top = 347
    Width = 31
    Height = 25
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
      F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
      F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
      F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
    TabOrder = 6
    OnClick = BitBtn22Click
  end
  object CheckBox3: TCheckBox
    Left = 555
    Top = 347
    Width = 97
    Height = 17
    Caption = 'Blockage'
    TabOrder = 7
    OnClick = CheckBox3Click
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 631
    Height = 341
    ActivePage = TabSheet2
    Align = alTop
    TabOrder = 8
    ExplicitWidth = 625
    object TabSheet1: TTabSheet
      Caption = 'Profile'
      object Label13: TLabel
        Left = 20
        Top = 34
        Width = 38
        Height = 13
        Caption = 'Label13'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label19: TLabel
        Left = 21
        Top = 53
        Width = 38
        Height = 13
        Caption = 'Label19'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label14: TLabel
        Left = 21
        Top = 99
        Width = 38
        Height = 13
        Caption = 'Label14'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label20: TLabel
        Left = 20
        Top = 118
        Width = 38
        Height = 13
        Caption = 'Label20'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 102
        Top = 10
        Width = 95
        Height = 13
        Caption = ' Observer up (m)'
      end
      object Label3: TLabel
        Left = 102
        Top = 70
        Width = 77
        Height = 13
        Caption = 'Target up (m)'
      end
      object Label15: TLabel
        Left = 11
        Top = 143
        Width = 61
        Height = 13
        Caption = 'Length (m)'
      end
      object Label16: TLabel
        Left = 12
        Top = 167
        Width = 62
        Height = 13
        Caption = 'Azimuth ('#176')'
      end
      object BitBtn12: TBitBtn
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Observer'
        TabOrder = 0
      end
      object BitBtn13: TBitBtn
        Left = 8
        Top = 68
        Width = 75
        Height = 25
        Caption = 'Target'
        TabOrder = 1
      end
      object Edit2: TEdit
        Left = 203
        Top = 8
        Width = 46
        Height = 21
        TabOrder = 2
        OnChange = Edit2Change
      end
      object Edit3: TEdit
        Left = 204
        Top = 67
        Width = 46
        Height = 21
        TabOrder = 3
        OnChange = Edit3Change
      end
      object Edit12: TEdit
        Left = 80
        Top = 140
        Width = 121
        Height = 21
        Enabled = False
        TabOrder = 4
        OnChange = Edit12Change
      end
      object Edit13: TEdit
        Left = 80
        Top = 167
        Width = 121
        Height = 21
        Enabled = False
        TabOrder = 5
        OnChange = Edit13Change
      end
      object GroupBox7: TGroupBox
        Left = 12
        Top = 194
        Width = 145
        Height = 103
        Caption = 'Profile'
        TabOrder = 6
        object Label6: TLabel
          Left = 3
          Top = 23
          Width = 76
          Height = 13
          Caption = 'High elev (m)'
        end
        object Label9: TLabel
          Left = 6
          Top = 50
          Width = 73
          Height = 13
          Caption = 'Low elev (m)'
        end
        object Label5: TLabel
          Left = 3
          Top = 69
          Width = 55
          Height = 13
          Caption = 'Vert exag'
        end
        object Edit8: TEdit
          Left = 85
          Top = 42
          Width = 57
          Height = 21
          TabOrder = 0
        end
        object Edit1: TEdit
          Left = 85
          Top = 15
          Width = 57
          Height = 21
          TabOrder = 1
        end
        object Edit5: TEdit
          Left = 85
          Top = 69
          Width = 57
          Height = 21
          TabOrder = 2
        end
      end
      object GroupBox5: TGroupBox
        Left = 273
        Top = 3
        Width = 185
        Height = 44
        Caption = 'DEM profile'
        TabOrder = 7
        object CheckBox7: TCheckBox
          Left = 15
          Top = 13
          Width = 54
          Height = 17
          Caption = 'Show'
          TabOrder = 0
          OnClick = CheckBox7Click
        end
        object BitBtn3: TBitBtn
          Left = 91
          Top = 14
          Width = 75
          Height = 25
          Caption = 'Topo'
          TabOrder = 1
          OnClick = BitBtn3Click
        end
      end
      object CheckBox23: TCheckBox
        Left = 283
        Top = 262
        Width = 137
        Height = 17
        Caption = 'Label end points'
        TabOrder = 8
        OnClick = CheckBox23Click
      end
      object GroupBox10: TGroupBox
        Left = 273
        Top = 53
        Width = 185
        Height = 96
        Caption = 'Horizontal ticks'
        TabOrder = 9
        object CheckBox19: TCheckBox
          Left = 16
          Top = 39
          Width = 97
          Height = 17
          Caption = 'Force spacing'
          TabOrder = 0
          OnClick = CheckBox19Click
        end
        object Edit15: TEdit
          Left = 56
          Top = 62
          Width = 73
          Height = 21
          TabOrder = 1
          OnChange = Edit15Change
        end
        object CheckBox20: TCheckBox
          Left = 80
          Top = 48
          Width = 1
          Height = 17
          Caption = 'CheckBox20'
          TabOrder = 2
        end
        object CheckBox21: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Use km'
          TabOrder = 3
          OnClick = CheckBox21Click
        end
      end
      object GroupBox11: TGroupBox
        Left = 273
        Top = 155
        Width = 185
        Height = 105
        Caption = 'Horizontal Zoom'
        TabOrder = 10
        object Label17: TLabel
          Left = 16
          Top = 56
          Width = 44
          Height = 13
          Caption = 'Left (m)'
        end
        object Label18: TLabel
          Left = 16
          Top = 80
          Width = 52
          Height = 13
          Caption = 'Right (m)'
        end
        object CheckBox24: TCheckBox
          Left = 16
          Top = 19
          Width = 97
          Height = 17
          Caption = 'Zoom in'
          TabOrder = 0
          OnClick = CheckBox24Click
        end
        object Edit16: TEdit
          Left = 74
          Top = 53
          Width = 73
          Height = 21
          TabOrder = 1
          Text = 'Edit16'
          OnChange = Edit16Change
        end
        object Edit17: TEdit
          Left = 74
          Top = 80
          Width = 73
          Height = 21
          TabOrder = 2
          Text = 'Edit17'
          OnChange = Edit17Change
        end
      end
      object CheckBox10: TCheckBox
        Left = 283
        Top = 285
        Width = 164
        Height = 17
        Caption = 'Show slope protractor'
        TabOrder = 11
      end
      object Font: TBitBtn
        Left = 488
        Top = 212
        Width = 75
        Height = 25
        Caption = 'Font'
        TabOrder = 12
        OnClick = FontClick
      end
      object CheckBox22: TCheckBox
        Left = 472
        Top = 264
        Width = 97
        Height = 17
        Caption = 'Sea level line'
        TabOrder = 13
        OnClick = CheckBox22Click
      end
      object CheckBox25: TCheckBox
        Left = 472
        Top = 288
        Width = 153
        Height = 17
        Caption = 'Elevation cross lines'
        TabOrder = 14
        OnClick = CheckBox25Click
      end
      object CheckBox27: TCheckBox
        Left = 472
        Top = 243
        Width = 129
        Height = 17
        Caption = 'Left side z labels'
        TabOrder = 15
        OnClick = CheckBox27Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'LOS'
      ImageIndex = 1
      object Label7: TLabel
        Left = -1
        Top = 200
        Width = 164
        Height = 13
        Caption = 'Closest bocking distance (m)'
      end
      object Label1: TLabel
        Left = 80
        Top = 242
        Width = 39
        Height = 13
        Caption = 'Label1'
      end
      object GroupBox8: TGroupBox
        Left = 3
        Top = 3
        Width = 196
        Height = 90
        Caption = 'Color visible/masked'
        TabOrder = 0
        object CheckBox1: TCheckBox
          Left = 16
          Top = 20
          Width = 91
          Height = 14
          Caption = 'Color Visible'
          TabOrder = 0
          OnClick = CheckBox1Click
        end
        object RadioGroup1: TRadioGroup
          Left = 3
          Top = 45
          Width = 113
          Height = 41
          HelpType = htKeyword
          Caption = 'Show by'
          Columns = 2
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Items.Strings = (
            'Pts'
            'Lines')
          ParentFont = False
          TabOrder = 1
        end
        object BitBtn9: TBitBtn
          Left = 122
          Top = 14
          Width = 66
          Height = 25
          Caption = 'Vis'
          TabOrder = 2
          OnClick = BitBtn9Click
        end
        object BitBtn10: TBitBtn
          Left = 122
          Top = 45
          Width = 66
          Height = 25
          Caption = 'Mask'
          TabOrder = 3
          OnClick = BitBtn10Click
        end
      end
      object GroupBox6: TGroupBox
        Left = 205
        Top = 49
        Width = 201
        Height = 71
        Caption = 'Observer pitch line'
        TabOrder = 1
        object Label10: TLabel
          Left = 3
          Top = 48
          Width = 38
          Height = 13
          Caption = 'Min ('#176')'
        end
        object Label11: TLabel
          Left = 88
          Top = 48
          Width = 41
          Height = 13
          Caption = 'Max ('#176')'
        end
        object CheckBox8: TCheckBox
          Left = 14
          Top = 17
          Width = 67
          Height = 17
          Caption = 'Show'
          TabOrder = 0
          OnClick = CheckBox8Click
        end
        object BitBtn8: TBitBtn
          Left = 87
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Pitch'
          TabOrder = 1
          OnClick = BitBtn8Click
        end
        object Edit9: TEdit
          Left = 135
          Top = 47
          Width = 38
          Height = 21
          TabOrder = 2
          Text = 'Edit9'
        end
        object Edit10: TEdit
          Left = 47
          Top = 47
          Width = 38
          Height = 21
          TabOrder = 3
        end
      end
      object GroupBox4: TGroupBox
        Left = 412
        Top = 99
        Width = 181
        Height = 71
        Caption = 'Fresnel zones'
        TabOrder = 2
        object Label4: TLabel
          Left = 79
          Top = 17
          Width = 24
          Height = 13
          Caption = 'Mhz'
        end
        object Edit4: TEdit
          Left = 109
          Top = 17
          Width = 52
          Height = 21
          TabOrder = 0
          OnChange = Edit4Change
        end
        object CheckBox4: TCheckBox
          Left = 14
          Top = 12
          Width = 59
          Height = 20
          Caption = 'Draw'
          TabOrder = 1
          OnClick = CheckBox4Click
        end
        object BitBtn6: TBitBtn
          Left = 3
          Top = 43
          Width = 87
          Height = 25
          Caption = 'Zone 1'
          TabOrder = 2
          OnClick = BitBtn6Click
        end
        object BitBtn7: TBitBtn
          Left = 96
          Top = 44
          Width = 75
          Height = 25
          Caption = 'Zone 2'
          TabOrder = 3
          OnClick = BitBtn7Click
        end
      end
      object GroupBox3: TGroupBox
        Left = 412
        Top = 16
        Width = 165
        Height = 50
        Caption = 'Masked air space'
        TabOrder = 3
        object BitBtn5: TBitBtn
          Left = 62
          Top = 22
          Width = 91
          Height = 25
          Caption = 'Masked'
          TabOrder = 0
          OnClick = BitBtn5Click
        end
        object CheckBox9: TCheckBox
          Left = 10
          Top = 19
          Width = 46
          Height = 17
          Caption = 'Show'
          TabOrder = 1
          OnClick = CheckBox9Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 213
        Top = 3
        Width = 193
        Height = 40
        Caption = 'Line of sight'
        TabOrder = 4
        object BitBtn4: TBitBtn
          Left = 87
          Top = 13
          Width = 75
          Height = 25
          Caption = 'LOS'
          TabOrder = 0
          OnClick = BitBtn4Click
        end
        object CheckBox2: TCheckBox
          Left = 10
          Top = 15
          Width = 55
          Height = 13
          Caption = 'Draw '
          TabOrder = 1
          OnClick = CheckBox2Click
        end
      end
      object Edit6: TEdit
        Left = 169
        Top = 192
        Width = 57
        Height = 21
        TabOrder = 5
        OnChange = Edit6Change
      end
      object CheckBox6: TCheckBox
        Left = 3
        Top = 145
        Width = 179
        Height = 17
        Caption = 'Missing data blocks LOS'
        TabOrder = 6
        OnClick = CheckBox6Click
      end
      object CheckBox11: TCheckBox
        Left = 10
        Top = 122
        Width = 179
        Height = 17
        Caption = 'Observer masking circle'
        TabOrder = 7
      end
      object CheckBox12: TCheckBox
        Left = 10
        Top = 177
        Width = 143
        Height = 17
        Caption = 'Show LOS data base'
        TabOrder = 8
      end
      object CheckBox17: TCheckBox
        Left = 10
        Top = 99
        Width = 109
        Height = 17
        Caption = 'Grazing angles'
        TabOrder = 9
      end
      object Button1: TButton
        Left = -1
        Top = 237
        Width = 75
        Height = 25
        Caption = 'Curvature'
        TabOrder = 10
        OnClick = Button1Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Point cloud/vegetation'
      ImageIndex = 2
      object CheckBox26: TCheckBox
        Left = 248
        Top = 201
        Width = 170
        Height = 17
        Caption = 'Vegetation effects (voxels)'
        TabOrder = 0
        OnClick = CheckBox26Click
      end
      object CheckBox18: TCheckBox
        Left = 247
        Top = 178
        Width = 154
        Height = 17
        Caption = 'Vegetation effects (grid)'
        TabOrder = 1
        OnClick = CheckBox18Click
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 3
        Width = 201
        Height = 307
        Caption = 'Point cloud options'
        TabOrder = 2
        object Label8: TLabel
          Left = 3
          Top = 146
          Width = 102
          Height = 13
          Caption = 'Lateral  buffer (m)'
        end
        object Label12: TLabel
          Left = 3
          Top = 173
          Width = 108
          Height = 13
          Caption = 'Slice thickness (m)'
        end
        object CheckBox13: TCheckBox
          Left = 16
          Top = 15
          Width = 166
          Height = 17
          Caption = 'Show floor/ceiling points'
          Enabled = False
          TabOrder = 0
          OnClick = CheckBox13Click
        end
        object CheckBox14: TCheckBox
          Left = 16
          Top = 31
          Width = 166
          Height = 17
          Caption = 'Show floor/ceiling lines'
          Enabled = False
          TabOrder = 1
          OnClick = CheckBox14Click
        end
        object BitBtn1: TBitBtn
          Left = 3
          Top = 54
          Width = 78
          Height = 25
          Caption = 'Ceiling'
          Enabled = False
          TabOrder = 2
          OnClick = BitBtn1Click
        end
        object BitBtn2: TBitBtn
          Left = 87
          Top = 54
          Width = 82
          Height = 25
          Caption = 'Floor'
          Enabled = False
          TabOrder = 3
          OnClick = BitBtn2Click
        end
        object Edit7: TEdit
          Left = 117
          Top = 146
          Width = 50
          Height = 21
          TabOrder = 4
          OnChange = Edit7Change
        end
        object Edit11: TEdit
          Left = 117
          Top = 171
          Width = 50
          Height = 21
          TabOrder = 5
          OnChange = Edit11Change
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 85
          Width = 97
          Height = 17
          Caption = 'Floor (m)'
          Enabled = False
          TabOrder = 6
          OnClick = CheckBox5Click
        end
        object Edit14: TEdit
          Left = 111
          Top = 85
          Width = 49
          Height = 21
          Enabled = False
          TabOrder = 7
        end
        object CheckBox15: TCheckBox
          Left = 14
          Top = 108
          Width = 152
          Height = 17
          Caption = 'Color cloud masking'
          Enabled = False
          TabOrder = 8
          OnClick = CheckBox15Click
        end
        object BitBtn15: TBitBtn
          Left = 95
          Top = 239
          Width = 87
          Height = 25
          Caption = 'LAS options'
          TabOrder = 9
          OnClick = BitBtn15Click
        end
        object BitBtn16: TBitBtn
          Left = 14
          Top = 239
          Width = 75
          Height = 25
          Caption = 'Load'
          Glyph.Data = {
            8A010000424D8A01000000000000760000002800000017000000170000000100
            0400000000001401000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFF0FFFFFF33FF33FFF33FF33FF0FF33FF33FF33FFF33FF33FF0FF33
            FFFFFFFFFFFFFFFFFFF0FFFFFFF33FFF33FFF33FFFF0FFF33FF33FFF33FFF33F
            FFF0FFF33FFFFFFFFFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFF0FFFFFFAAFAAF
            FFFFFFFFFFF0FFFFFFAAFAAFFFFFFFFFFFF0FFFFFFFFFFFFFFAAFFFFFFF0FFFF
            FAAFFAAFFFAAFFFFFFF0FFFFFAAFFAAFFFFFFFAAFFF0FFFFFFFFFFFFAAFFFFAA
            FFF0FFFFFFFFAAFFAAFFFFFFFFF0FFFFFAAFAAFFFFFAAFFFFFF0FFFFFAAFFFAA
            FFFAAFFFFFF0FFFFFFFFFFAAFFFFFFFFAAF0FFFFAAFFFFFFFFFFFFFFAAF0FFFF
            AAFAAFFFAAFFAAFFFFF0FFFFFFFAAFFFAAFFAAFFFFF0FFFFFFFFFFFFFFFFFFFF
            FFF0FFFFFFFFFFFFFFFFFFFFFFF0}
          TabOrder = 10
          OnClick = BitBtn16Click
        end
        object RadioGroup3: TRadioGroup
          Left = 32
          Top = 270
          Width = 81
          Height = 32
          Caption = 'Cloud'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            '1'
            '2')
          TabOrder = 11
          OnClick = RadioGroup3Click
        end
        object RadioGroup4: TRadioGroup
          Left = 3
          Top = 200
          Width = 198
          Height = 33
          Caption = 'Show'
          Columns = 3
          Items.Strings = (
            'None'
            'Points'
            'Density')
          TabOrder = 12
          OnClick = RadioGroup4Click
        end
      end
      object RadioGroup2: TRadioGroup
        Left = 235
        Top = 224
        Width = 122
        Height = 34
        Caption = 'Veg density voxels'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2')
        TabOrder = 3
        OnClick = RadioGroup2Click
      end
      object RadioGroup7: TRadioGroup
        Left = 363
        Top = 224
        Width = 77
        Height = 34
        Caption = 'Veg grid'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2')
        TabOrder = 4
        OnClick = RadioGroup7Click
      end
      object GroupBox9: TGroupBox
        Left = 240
        Top = 19
        Width = 185
        Height = 65
        Caption = 'OpenGL'
        TabOrder = 5
        object CheckBox16: TCheckBox
          Left = 16
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Show LOS'
          TabOrder = 0
        end
        object BitBtn14: TBitBtn
          Left = 23
          Top = 35
          Width = 138
          Height = 25
          Caption = 'OpenGL LOS'
          TabOrder = 1
          OnClick = BitBtn14Click
        end
        object BitBtn19: TBitBtn
          Left = 104
          Top = 5
          Width = 65
          Height = 24
          Enabled = False
          Glyph.Data = {
            8A000000424D8A000000000000003E0000002800000020000000130000000100
            0100000000004C000000C40E0000C40E0000020000000000000000000000FFFF
            FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE003FFFF00707FFC0DFF9FF81DF
            FF7F02555400075855C30715D583074645F3077FFCD300FFFE1383FFFFFFC0FF
            F9FFF00207FFFE003FFFFFFFFFFF}
          TabOrder = 2
          OnClick = BitBtn19Click
        end
      end
      object RadioGroup6: TRadioGroup
        Left = 240
        Top = 90
        Width = 178
        Height = 38
        Caption = 'Vegetation Density '
        Columns = 3
        Items.Strings = (
          'None'
          'Cloud'
          'Voxels')
        TabOrder = 6
        OnClick = RadioGroup6Click
      end
      object RadioGroup5: TRadioGroup
        Left = 240
        Top = 134
        Width = 178
        Height = 38
        Caption = 'Density along LOS line'
        Columns = 3
        Items.Strings = (
          'None'
          'Cloud'
          'Voxels')
        TabOrder = 7
        OnClick = RadioGroup5Click
      end
    end
  end
end
