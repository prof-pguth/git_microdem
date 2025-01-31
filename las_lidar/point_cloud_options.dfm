object pt_cloud_opts_fm: Tpt_cloud_opts_fm
  Left = 0
  Top = 0
  Caption = 'Point Cloud Options'
  ClientHeight = 452
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label25: TLabel
    Left = 152
    Top = 16
    Width = 37
    Height = 13
    Caption = 'Label25'
  end
  object Label30: TLabel
    Left = 224
    Top = 200
    Width = 37
    Height = 13
    Caption = 'Label30'
  end
  object Panel1: TPanel
    Left = 0
    Top = 411
    Width = 581
    Height = 41
    Align = alBottom
    TabOrder = 0
    object CancelBtn: TBitBtn
      Left = 290
      Top = 6
      Width = 77
      Height = 25
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = CancelBtnClick
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 373
      Top = 6
      Width = 77
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn8: TBitBtn
      Left = 109
      Top = 6
      Width = 105
      Height = 25
      Caption = 'Point cloud'
      Enabled = False
      Glyph.Data = {
        8A000000424D8A000000000000003E0000002800000020000000130000000100
        0100000000004C000000C40E0000C40E0000020000000000000000000000FFFF
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE003FFFF00707FFC0DFF9FF81DF
        FF7F02555400075855C30715D583074645F3077FFCD300FFFE1383FFFFFFC0FF
        F9FFF00207FFFE003FFFFFFFFFFF}
      TabOrder = 2
      OnClick = BitBtn8Click
    end
    object BitBtn6: TBitBtn
      Left = 4
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Force redraw'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 3
      OnClick = BitBtn6Click
    end
    object BitBtn21: TBitBtn
      Left = 220
      Top = 6
      Width = 49
      Height = 25
      Caption = 'Slice'
      TabOrder = 4
      OnClick = BitBtn21Click
    end
    object BitBtn63: TBitBtn
      Left = 472
      Top = 6
      Width = 89
      Height = 25
      Caption = 'Save defaults'
      TabOrder = 5
      OnClick = BitBtn63Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 581
    Height = 242
    ActivePage = TabSheet8
    Align = alClient
    TabOrder = 1
    object General: TTabSheet
      Caption = 'General'
      object Label6: TLabel
        Left = 16
        Top = 3
        Width = 79
        Height = 13
        Caption = 'OpenGL thinning'
      end
      object Label7: TLabel
        Left = 33
        Top = 34
        Width = 62
        Height = 13
        Caption = 'Slice thinning'
      end
      object Label10: TLabel
        Left = 3
        Top = 57
        Width = 108
        Height = 13
        Caption = 'KML vertical offset (m)'
      end
      object Edit8: TEdit
        Left = 116
        Top = 3
        Width = 43
        Height = 21
        TabOrder = 0
        OnChange = Edit8Change
      end
      object Edit9: TEdit
        Left = 117
        Top = 30
        Width = 42
        Height = 21
        TabOrder = 1
        OnChange = Edit9Change
      end
      object Edit10: TEdit
        Left = 117
        Top = 57
        Width = 42
        Height = 21
        TabOrder = 2
        OnChange = Edit10Change
      end
      object RadioGroup2: TRadioGroup
        Left = 3
        Top = 84
        Width = 135
        Height = 73
        Caption = 'Bad point filter'
        Items.Strings = (
          'None'
          'Noise only'
          'Overlap and noise')
        TabOrder = 3
        OnClick = RadioGroup2Click
      end
      object CheckBox28: TCheckBox
        Left = 3
        Top = 176
        Width = 230
        Height = 17
        Caption = 'Always assume LAS 1.4 classifications'
        TabOrder = 4
        OnClick = CheckBox28Click
      end
      object CheckBox24: TCheckBox
        Left = 199
        Top = 51
        Width = 200
        Height = 17
        Caption = 'Immediate map after opening'
        TabOrder = 5
        OnClick = CheckBox24Click
      end
      object CheckBox6: TCheckBox
        Left = 199
        Top = 5
        Width = 164
        Height = 17
        Caption = 'Only open tiles on map'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = CheckBox6Click
      end
      object CheckBox16: TCheckBox
        Left = 199
        Top = 28
        Width = 231
        Height = 17
        Caption = 'Immediate zoom to cloud on opening files'
        TabOrder = 7
        OnClick = CheckBox16Click
      end
      object CheckBox30: TCheckBox
        Left = 199
        Top = 74
        Width = 164
        Height = 17
        Caption = 'Automatic lidar redraws'
        TabOrder = 8
        OnClick = CheckBox30Click
      end
      object CheckBox31: TCheckBox
        Left = 199
        Top = 97
        Width = 231
        Height = 17
        Caption = 'Show point density on mean grid creation'
        TabOrder = 9
        OnClick = CheckBox31Click
      end
      object CheckBox21: TCheckBox
        Left = 199
        Top = 120
        Width = 97
        Height = 17
        Caption = 'Log output'
        TabOrder = 10
        OnClick = CheckBox21Click
      end
      object CheckBox29: TCheckBox
        Left = 199
        Top = 143
        Width = 97
        Height = 17
        Caption = 'Lat/long slices'
        TabOrder = 11
        OnClick = CheckBox29Click
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 573
        Height = 214
        Align = alClient
        TabOrder = 0
        object Label1: TLabel
          Left = 296
          Top = 16
          Width = 3
          Height = 13
        end
        object Label8: TLabel
          Left = 64
          Top = 59
          Width = 31
          Height = 13
          Caption = 'Label8'
        end
        object Label9: TLabel
          Left = 64
          Top = 90
          Width = 31
          Height = 13
          Caption = 'Label9'
        end
        object Label11: TLabel
          Left = 65
          Top = 123
          Width = 37
          Height = 13
          Caption = 'Label11'
        end
        object Label12: TLabel
          Left = 65
          Top = 155
          Width = 37
          Height = 13
          Caption = 'Label12'
        end
        object Label23: TLabel
          Left = 64
          Top = 184
          Width = 37
          Height = 13
          Caption = 'Label23'
        end
        object BitBtn5: TBitBtn
          Left = 4
          Top = 54
          Width = 54
          Height = 25
          Caption = 'Add #1'
          TabOrder = 0
          OnClick = BitBtn5Click
        end
        object BitBtn13: TBitBtn
          Left = 167
          Top = 6
          Width = 57
          Height = 25
          Caption = 'Metadata'
          TabOrder = 1
          OnClick = BitBtn13Click
        end
        object BitBtn18: TBitBtn
          Left = 230
          Top = 6
          Width = 41
          Height = 25
          Caption = 'Outline'
          TabOrder = 2
          OnClick = BitBtn18Click
        end
        object CheckBox7: TCheckBox
          Left = 336
          Top = 0
          Width = 97
          Height = 17
          Caption = 'Label'
          TabOrder = 3
          OnClick = CheckBox7Click
        end
        object BitBtn47: TBitBtn
          Left = 277
          Top = 6
          Width = 36
          Height = 25
          Caption = 'Zoom'
          TabOrder = 4
          OnClick = BitBtn47Click
        end
        object BitBtn44: TBitBtn
          Left = 5
          Top = 147
          Width = 54
          Height = 25
          Caption = 'Add #4'
          TabOrder = 5
          OnClick = BitBtn44Click
        end
        object BitBtn43: TBitBtn
          Left = 4
          Top = 116
          Width = 54
          Height = 25
          Caption = 'Add #3'
          TabOrder = 6
          OnClick = BitBtn43Click
        end
        object BitBtn29: TBitBtn
          Left = 4
          Top = 85
          Width = 54
          Height = 25
          Caption = 'Add #2'
          TabOrder = 7
          OnClick = BitBtn29Click
        end
        object RadioGroup6: TRadioGroup
          Left = 3
          Top = 0
          Width = 103
          Height = 33
          Caption = 'Open'
          Columns = 2
          DragMode = dmAutomatic
          Items.Strings = (
            'Dir'
            'Files')
          TabOrder = 8
          OnClick = RadioGroup6Click
        end
        object CheckBox14: TCheckBox
          Left = 336
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Outlines'
          TabOrder = 9
          OnClick = CheckBox14Click
        end
        object BitBtn28: TBitBtn
          Left = 4
          Top = 177
          Width = 54
          Height = 25
          Caption = 'Add #5'
          TabOrder = 10
          OnClick = BitBtn28Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Tools'
      ImageIndex = 3
      object ZcritLabel: TLabel
        Left = 212
        Top = 18
        Width = 40
        Height = 13
        Caption = 'Zcrit (m)'
      end
      object Edit2: TEdit
        Left = 258
        Top = 18
        Width = 43
        Height = 21
        TabOrder = 0
        Text = '1'
      end
      object RadioGroup3: TRadioGroup
        Left = 210
        Top = 45
        Width = 96
        Height = 51
        Caption = 'DEM Z'
        ItemIndex = 0
        Items.Strings = (
          'Interpolate'
          'Floor/ceiling')
        TabOrder = 1
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 3
        Width = 97
        Height = 166
        Caption = 'Point based'
        TabOrder = 2
        object BitBtn4: TBitBtn
          Left = 3
          Top = 16
          Width = 94
          Height = 20
          Caption = 'Extract map--LAS'
          TabOrder = 0
          OnClick = BitBtn4Click
        end
        object BitBtn15: TBitBtn
          Left = 5
          Top = 117
          Width = 89
          Height = 20
          Caption = 'Pts above DEM'
          TabOrder = 1
          OnClick = BitBtn14Click
        end
        object BitBtn14: TBitBtn
          Left = 5
          Top = 143
          Width = 89
          Height = 20
          Caption = 'Pts below DEM'
          TabOrder = 2
          OnClick = BitBtn14Click
        end
        object BitBtn19: TBitBtn
          Left = 5
          Top = 42
          Width = 89
          Height = 20
          Caption = 'Extract masked'
          TabOrder = 3
          OnClick = BitBtn19Click
        end
        object BitBtn48: TBitBtn
          Left = 3
          Top = 95
          Width = 94
          Height = 20
          Caption = 'ExtractGeoJSON'
          TabOrder = 4
          OnClick = BitBtn4Click
        end
        object BitBtn49: TBitBtn
          Left = 3
          Top = 68
          Width = 91
          Height = 21
          Caption = 'Extract valid grid'
          TabOrder = 5
          OnClick = BitBtn49Click
        end
      end
      object BitBtn52: TBitBtn
        Left = 103
        Top = 17
        Width = 75
        Height = 25
        Caption = 'Thin LAS'
        TabOrder = 3
        OnClick = BitBtn52Click
      end
      object BitBtn53: TBitBtn
        Left = 104
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Translate LAS'
        TabOrder = 4
        OnClick = BitBtn53Click
      end
      object BitBtn54: TBitBtn
        Left = 103
        Top = 79
        Width = 75
        Height = 25
        Caption = 'Scale up LAS'
        TabOrder = 5
        OnClick = BitBtn54Click
      end
      object BitBtn55: TBitBtn
        Left = 103
        Top = 110
        Width = 75
        Height = 25
        Caption = 'RGB filter LAS'
        TabOrder = 6
        OnClick = BitBtn55Click
      end
      object Las: TBitBtn
        Left = 360
        Top = 56
        Width = 75
        Height = 25
        Caption = 'lasview'
        TabOrder = 7
        OnClick = LasClick
      end
      object BitBtn61: TBitBtn
        Left = 210
        Top = 128
        Width = 106
        Height = 25
        Caption = 'Copy tiles on map'
        TabOrder = 8
        OnClick = BitBtn61Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Stats'
      ImageIndex = 4
      object BitBtn17: TBitBtn
        Left = 10
        Top = 3
        Width = 97
        Height = 25
        Caption = 'Histogram/counts'
        TabOrder = 0
        OnClick = BitBtn17Click
      end
      object BitBtn11: TBitBtn
        Left = 113
        Top = 34
        Width = 97
        Height = 25
        Caption = 'Zero classification'
        TabOrder = 1
        OnClick = BitBtn11Click
      end
      object BitBtn39: TBitBtn
        Left = 113
        Top = 96
        Width = 98
        Height = 25
        Caption = 'Split by Class'
        TabOrder = 2
        OnClick = BitBtn39Click
      end
      object BitBtn35: TBitBtn
        Left = 9
        Top = 96
        Width = 98
        Height = 27
        Caption = 'Subset/tile'
        TabOrder = 3
        OnClick = BitBtn35Click
      end
      object BitBtn42: TBitBtn
        Left = 113
        Top = 65
        Width = 98
        Height = 25
        Caption = 'Zero fields'
        TabOrder = 4
        OnClick = BitBtn42Click
      end
      object BitBtn40: TBitBtn
        Left = 10
        Top = 34
        Width = 97
        Height = 25
        Caption = 'RGB palette'
        TabOrder = 5
        OnClick = BitBtn40Click
      end
      object BitBtn34: TBitBtn
        Left = 10
        Top = 65
        Width = 97
        Height = 25
        Caption = 'TIN'
        TabOrder = 6
        OnClick = BitBtn34Click
      end
      object BitBtn25: TBitBtn
        Left = 113
        Top = 3
        Width = 97
        Height = 25
        Caption = 'Elev Hist'
        TabOrder = 7
        OnClick = BitBtn25Click
      end
      object BitBtn24: TBitBtn
        Left = 248
        Top = 3
        Width = 89
        Height = 25
        Caption = 'Cell statistics'
        TabOrder = 8
        OnClick = BitBtn24Click
      end
      object BitBtn60: TBitBtn
        Left = 248
        Top = 96
        Width = 89
        Height = 25
        Caption = 'Recompute'
        TabOrder = 9
        OnClick = BitBtn60Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Draw'
      ImageIndex = 5
      object Label5: TLabel
        Left = 233
        Top = 60
        Width = 24
        Height = 13
        Caption = 'Min z'
      end
      object Label4: TLabel
        Left = 228
        Top = 35
        Width = 28
        Height = 13
        Caption = 'Max z'
      end
      object Label3: TLabel
        Left = 3
        Top = 7
        Width = 47
        Height = 13
        Caption = 'Thin, map'
      end
      object Thin: TLabel
        Left = 2
        Top = 59
        Width = 73
        Height = 13
        Caption = 'Thin coefficient'
      end
      object Label17: TLabel
        Left = 283
        Top = 6
        Width = 37
        Height = 13
        Caption = 'Opacity'
      end
      object Label2: TLabel
        Left = 255
        Top = 144
        Width = 43
        Height = 13
        Caption = 'Max RGB'
      end
      object Label21: TLabel
        Left = 255
        Top = 163
        Width = 39
        Height = 13
        Caption = 'Min RGB'
      end
      object Label34: TLabel
        Left = 241
        Top = 96
        Width = 64
        Height = 13
        Caption = 'Max intensity'
      end
      object Label35: TLabel
        Left = 241
        Top = 120
        Width = 60
        Height = 13
        Caption = 'Min intensity'
      end
      object Edit6: TEdit
        Left = 263
        Top = 59
        Width = 65
        Height = 21
        TabOrder = 0
        Text = '9999'
      end
      object Edit5: TEdit
        Left = 262
        Top = 32
        Width = 66
        Height = 21
        TabOrder = 1
        Text = '-9999'
      end
      object Edit3: TEdit
        Left = 56
        Top = 4
        Width = 31
        Height = 21
        TabOrder = 2
        Text = '1'
        OnChange = Edit3Change
      end
      object CheckBox8: TCheckBox
        Left = 3
        Top = 31
        Width = 97
        Height = 17
        Caption = 'LAS Auto Thin'
        TabOrder = 3
        OnClick = CheckBox8Click
      end
      object Edit11: TEdit
        Left = 81
        Top = 54
        Width = 58
        Height = 21
        TabOrder = 4
        Text = '1'
        OnChange = Edit11Change
      end
      object BitBtn31: TBitBtn
        Left = 334
        Top = 30
        Width = 65
        Height = 25
        Caption = 'Cloud Z'
        TabOrder = 5
        OnClick = BitBtn31Click
      end
      object CheckBox3: TCheckBox
        Left = 172
        Top = 3
        Width = 105
        Height = 17
        Caption = 'IHS color merge'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = CheckBox3Click
      end
      object BitBtn56: TBitBtn
        Left = 334
        Top = 59
        Width = 65
        Height = 25
        Caption = 'DEM Z'
        TabOrder = 7
        OnClick = BitBtn56Click
      end
      object CheckBox19: TCheckBox
        Left = 3
        Top = 81
        Width = 97
        Height = 17
        Caption = 'LAS legend'
        TabOrder = 8
        OnClick = CheckBox19Click
      end
      object Edit18: TEdit
        Left = 326
        Top = 3
        Width = 65
        Height = 21
        TabOrder = 9
        OnChange = Edit18Change
      end
      object Edit4: TEdit
        Left = 319
        Top = 168
        Width = 80
        Height = 21
        TabOrder = 10
        Text = '0'
        OnChange = Edit4Change
      end
      object Edit23: TEdit
        Left = 319
        Top = 144
        Width = 80
        Height = 21
        TabOrder = 11
        Text = '65535'
        OnChange = Edit23Change
      end
      object CheckBox25: TCheckBox
        Left = 2
        Top = 104
        Width = 97
        Height = 17
        Caption = 'Scale 1%--99%'
        TabOrder = 12
      end
      object BitBtn7: TBitBtn
        Left = 8
        Top = 144
        Width = 113
        Height = 25
        Caption = 'Overlay grid outlines'
        TabOrder = 13
        OnClick = BitBtn7Click
      end
      object Edit33: TEdit
        Left = 319
        Top = 90
        Width = 80
        Height = 21
        TabOrder = 14
        Text = '32000'
        OnChange = Edit33Change
      end
      object Edit34: TEdit
        Left = 319
        Top = 117
        Width = 78
        Height = 21
        TabOrder = 15
        Text = '0'
        OnChange = Edit34Change
      end
      object BitBtn62: TBitBtn
        Left = 405
        Top = 100
        Width = 75
        Height = 25
        Caption = 'Intensity Dist'
        TabOrder = 16
        OnClick = BitBtn62Click
      end
    end
    object TabSheet6: TTabSheet
      Caption = '1 pass grids'
      ImageIndex = 6
      object BitBtn30: TBitBtn
        Left = 3
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Elevation grids'
        TabOrder = 0
        OnClick = BitBtn30Click
      end
      object BitBtn51: TBitBtn
        Left = 3
        Top = 34
        Width = 129
        Height = 25
        Caption = 'Point count/density grids'
        TabOrder = 1
        OnClick = BitBtn51Click
      end
      object BitBtn57: TBitBtn
        Left = 3
        Top = 65
        Width = 129
        Height = 25
        Caption = 'Other grids'
        TabOrder = 2
        OnClick = BitBtn57Click
      end
      object RadioGroup4: TRadioGroup
        Left = 5
        Top = 127
        Width = 133
        Height = 74
        Caption = 'DTM from ground points'
        Columns = 2
        ItemIndex = 2
        Items.Strings = (
          'Max '
          'Mean'
          'Min'
          'Nearest'
          'All')
        TabOrder = 3
        OnClick = RadioGroup4Click
      end
      object BitBtn1: TBitBtn
        Left = 6
        Top = 96
        Width = 129
        Height = 25
        Caption = 'DTM (external programs)'
        TabOrder = 4
        OnClick = BitBtn1Click
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Multi pass grids'
      ImageIndex = 8
      object Label32: TLabel
        Left = 16
        Top = 32
        Width = 91
        Height = 13
        Caption = 'Min points required'
      end
      object GroupBox2: TGroupBox
        Left = 191
        Top = 3
        Width = 258
        Height = 198
        Caption = 'Grid creation parameters'
        TabOrder = 0
        object Label28: TLabel
          Left = 18
          Top = 180
          Width = 37
          Height = 13
          Caption = 'Label28'
        end
        object Label27: TLabel
          Left = 27
          Top = 127
          Width = 76
          Height = 13
          Caption = 'Block size (cells)'
        end
        object Label29: TLabel
          Left = 112
          Top = 15
          Width = 47
          Height = 13
          Caption = 'Percentile'
        end
        object Label26: TLabel
          Left = 15
          Top = 153
          Width = 81
          Height = 13
          Caption = 'Max points in cell'
        end
        object Edit29: TEdit
          Left = 110
          Top = 150
          Width = 59
          Height = 21
          TabOrder = 0
          Text = 'Edit29'
          OnChange = Edit29Change
        end
        object Edit28: TEdit
          Left = 109
          Top = 123
          Width = 60
          Height = 21
          TabOrder = 1
          Text = 'Edit28'
          OnChange = Edit28Change
        end
        object Edit27: TEdit
          Left = 109
          Top = 95
          Width = 44
          Height = 21
          TabOrder = 2
          Text = '50'
          OnChange = Edit27Change
        end
        object BitBtn41: TBitBtn
          Left = 3
          Top = 96
          Width = 100
          Height = 25
          Caption = 'DTM ground returns'
          TabOrder = 3
          OnClick = BitBtn41Click
        end
        object BitBtn38: TBitBtn
          Left = 3
          Top = 65
          Width = 100
          Height = 25
          Caption = 'Grid middle cloud'
          TabOrder = 4
          OnClick = BitBtn38Click
        end
        object Edit26: TEdit
          Left = 109
          Top = 68
          Width = 44
          Height = 21
          TabOrder = 5
          Text = '50'
          OnChange = Edit26Change
        end
        object Edit22: TEdit
          Left = 109
          Top = 34
          Width = 44
          Height = 21
          TabOrder = 6
          Text = '50'
          OnChange = Edit22Change
        end
        object BitBtn26: TBitBtn
          Left = 3
          Top = 34
          Width = 100
          Height = 25
          Caption = 'DSM first returns'
          TabOrder = 7
          OnClick = BitBtn26Click
        end
        object CheckBox2: TCheckBox
          Left = 184
          Top = 14
          Width = 97
          Height = 17
          Caption = 'Use noise'
          TabOrder = 8
          OnClick = CheckBox2Click
        end
        object CheckBox26: TCheckBox
          Left = 184
          Top = 37
          Width = 97
          Height = 17
          Caption = 'Use overlap'
          TabOrder = 9
          OnClick = CheckBox26Click
        end
      end
      object BitBtn50: TBitBtn
        Left = 24
        Top = 102
        Width = 89
        Height = 25
        Caption = 'Multiple grids'
        TabOrder = 1
        OnClick = BitBtn50Click
      end
      object Edit31: TEdit
        Left = 113
        Top = 29
        Width = 57
        Height = 21
        TabOrder = 2
        Text = 'Edit31'
        OnChange = Edit31Change
      end
      object CheckBox32: TCheckBox
        Left = 16
        Top = 56
        Width = 116
        Height = 17
        Caption = 'Auto thin by block'
        TabOrder = 3
        OnClick = CheckBox32Click
      end
      object CheckBox33: TCheckBox
        Left = 16
        Top = 79
        Width = 97
        Height = 17
        Caption = 'Density grid'
        TabOrder = 4
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Grid params'
      ImageIndex = 9
      object Label22: TLabel
        Left = 140
        Top = 125
        Width = 96
        Height = 13
        Caption = 'Max  gap (grid cells)'
      end
      object Label31: TLabel
        Left = 155
        Top = 100
        Width = 86
        Height = 13
        Caption = 'Proj meters (X+Y)'
      end
      object Label24: TLabel
        Left = 3
        Top = 70
        Width = 72
        Height = 13
        Caption = 'Lat grid size (")'
      end
      object Label20: TLabel
        Left = 148
        Top = 70
        Width = 80
        Height = 13
        Caption = 'Long grid size (")'
      end
      object Label18: TLabel
        Left = 279
        Top = 11
        Width = 77
        Height = 13
        Caption = 'Force UTM zone'
      end
      object Label16: TLabel
        Left = 3
        Top = 43
        Width = 65
        Height = 13
        Caption = 'XGrid size (m)'
      end
      object Label19: TLabel
        Left = 328
        Top = 35
        Width = 3
        Height = 13
      end
      object Label33: TLabel
        Left = 153
        Top = 45
        Width = 65
        Height = 13
        Caption = 'YGrid size (m)'
      end
      object Label36: TLabel
        Left = 248
        Top = 192
        Width = 48
        Height = 13
        Caption = 'not saved'
      end
      object RadioGroup5: TRadioGroup
        Left = 3
        Top = 154
        Width = 103
        Height = 40
        Caption = 'Pixel Is'
        Columns = 2
        Items.Strings = (
          'Area'
          'Point')
        TabOrder = 0
        OnClick = RadioGroup5Click
      end
      object Edit24: TEdit
        Left = 247
        Top = 124
        Width = 54
        Height = 21
        TabOrder = 1
        OnChange = Edit24Change
      end
      object CheckBox9: TCheckBox
        Left = 35
        Top = 128
        Width = 99
        Height = 17
        Caption = 'Auto fill holes'
        TabOrder = 2
        OnClick = CheckBox9Click
      end
      object Edit30: TEdit
        Left = 247
        Top = 97
        Width = 54
        Height = 21
        TabOrder = 3
        OnChange = Edit30Change
      end
      object BitBtn45: TBitBtn
        Left = 3
        Top = 94
        Width = 143
        Height = 25
        Caption = 'wkt'
        TabOrder = 4
        OnClick = BitBtn45Click
      end
      object Edit25: TEdit
        Left = 247
        Top = 70
        Width = 54
        Height = 21
        TabOrder = 5
        OnChange = Edit25Change
      end
      object Edit21: TEdit
        Left = 81
        Top = 67
        Width = 61
        Height = 21
        TabOrder = 6
        OnChange = Edit21Change
      end
      object Edit17: TEdit
        Left = 81
        Top = 43
        Width = 61
        Height = 21
        TabOrder = 7
        OnChange = Edit17Change
      end
      object RadioGroup7: TRadioGroup
        Left = 3
        Top = 0
        Width = 270
        Height = 37
        Caption = 'Lidar grid projection'
        Columns = 3
        Items.Strings = (
          'UTM'
          'Lat/Long'
          'Other WKT')
        TabOrder = 8
        OnClick = RadioGroup7Click
      end
      object Edit19: TEdit
        Left = 369
        Top = 8
        Width = 47
        Height = 21
        TabOrder = 9
        OnChange = Edit19Change
      end
      object BitBtn58: TBitBtn
        Left = 299
        Top = 151
        Width = 147
        Height = 25
        Caption = 'Create blank grid'
        TabOrder = 10
        OnClick = BitBtn58Click
      end
      object Edit32: TEdit
        Left = 245
        Top = 43
        Width = 56
        Height = 21
        TabOrder = 11
        OnChange = Edit32Change
      end
      object BitBtn59: TBitBtn
        Left = 138
        Top = 151
        Width = 119
        Height = 25
        Caption = 'Close and create grid'
        TabOrder = 12
        OnClick = BitBtn59Click
      end
      object CheckBox34: TCheckBox
        Left = 328
        Top = 54
        Width = 153
        Height = 17
        Caption = 'Force equal  X + Y spacing'
        TabOrder = 13
        OnClick = CheckBox34Click
      end
      object RadioGroup8: TRadioGroup
        Left = 328
        Top = 77
        Width = 88
        Height = 61
        Caption = 'Lat hemi'
        Items.Strings = (
          'North'
          'South')
        TabOrder = 14
        OnClick = RadioGroup8Click
      end
      object BitBtn65: TBitBtn
        Left = 139
        Top = 182
        Width = 89
        Height = 25
        Caption = 'Auto save dir'
        TabOrder = 15
        OnClick = BitBtn65Click
      end
    end
    object Filters: TTabSheet
      Caption = 'Filters'
      ImageIndex = 7
      object CheckBox11: TCheckBox
        Left = 17
        Top = 0
        Width = 113
        Height = 17
        Caption = 'Enable LAS filters'
        TabOrder = 0
        OnClick = CheckBox11Click
      end
      object GroupBox6: TGroupBox
        Left = 0
        Top = 23
        Width = 313
        Height = 178
        Caption = 'Filter properties'
        TabOrder = 1
        object Edit1: TEdit
          Left = 111
          Top = 13
          Width = 33
          Height = 21
          Enabled = False
          TabOrder = 0
          Text = '1'
          OnChange = Edit1Change
        end
        object Edit7: TEdit
          Left = 111
          Top = 40
          Width = 34
          Height = 21
          TabOrder = 1
          Text = '1'
          OnChange = Edit7Change
        end
        object Edit15: TEdit
          Left = 111
          Top = 65
          Width = 33
          Height = 21
          TabOrder = 2
          OnChange = Edit15Change
        end
        object CheckBox13: TCheckBox
          Left = 16
          Top = 138
          Width = 114
          Height = 17
          Caption = 'Last returns only'
          TabOrder = 3
          OnClick = CheckBox13Click
        end
        object CheckBox10: TCheckBox
          Left = 15
          Top = 115
          Width = 97
          Height = 17
          Caption = 'Air returns only'
          TabOrder = 4
          OnClick = CheckBox10Click
        end
        object CheckBox12: TCheckBox
          Left = 15
          Top = 92
          Width = 129
          Height = 17
          Caption = 'First returns only'
          TabOrder = 5
          OnClick = CheckBox12Click
        end
        object CheckBox95: TCheckBox
          Left = 168
          Top = 112
          Width = 136
          Height = 17
          Caption = 'Ignore low points (noise)'
          TabOrder = 6
          OnClick = CheckBox95Click
        end
        object CheckBox5: TCheckBox
          Left = 15
          Top = 60
          Width = 90
          Height = 17
          Caption = 'Scan angle'
          TabOrder = 7
          OnClick = CheckBox5Click
        end
        object CheckBox1: TCheckBox
          Left = 16
          Top = 14
          Width = 89
          Height = 17
          Caption = ' LAS category'
          TabOrder = 8
          OnClick = CheckBox1Click
        end
        object CheckBox4: TCheckBox
          Left = 16
          Top = 37
          Width = 89
          Height = 17
          Caption = 'Return number'
          TabOrder = 9
          OnClick = CheckBox4Click
        end
        object CheckBox15: TCheckBox
          Left = 168
          Top = 12
          Width = 97
          Height = 17
          Caption = 'Point ID (Titan)'
          TabOrder = 10
          OnClick = CheckBox15Click
        end
        object Edit16: TEdit
          Left = 271
          Top = 10
          Width = 33
          Height = 21
          TabOrder = 11
          Text = '1'
          OnChange = Edit16Change
        end
        object CheckBox17: TCheckBox
          Left = 168
          Top = 89
          Width = 129
          Height = 17
          Caption = 'Ignore overlap points'
          TabOrder = 12
          OnClick = CheckBox17Click
        end
        object CheckBox20: TCheckBox
          Left = 168
          Top = 35
          Width = 97
          Height = 17
          Caption = 'User data record'
          TabOrder = 13
          OnClick = CheckBox20Click
        end
        object Edit20: TEdit
          Left = 271
          Top = 37
          Width = 33
          Height = 21
          TabOrder = 14
          Text = '1'
          OnChange = Edit20Change
        end
        object CheckBox22: TCheckBox
          Left = 168
          Top = 135
          Width = 113
          Height = 17
          Caption = 'Ground class only'
          TabOrder = 15
          OnClick = CheckBox22Click
        end
        object CheckBox23: TCheckBox
          Left = 168
          Top = 158
          Width = 145
          Height = 17
          Caption = 'Non ground, single returns'
          TabOrder = 16
          OnClick = CheckBox23Click
        end
        object CheckBox27: TCheckBox
          Left = 168
          Top = 66
          Width = 97
          Height = 17
          Caption = 'Ignore high noise'
          TabOrder = 17
          OnClick = CheckBox27Click
        end
        object CheckBox99: TCheckBox
          Left = 16
          Top = 160
          Width = 128
          Height = 17
          Caption = 'Single returns only'
          TabOrder = 18
          OnClick = CheckBox99Click
        end
      end
      object GroupBox3: TGroupBox
        Left = 319
        Top = 23
        Width = 136
        Height = 169
        Caption = 'Elevation checks'
        TabOrder = 2
        object Label13: TLabel
          Left = 24
          Top = 56
          Width = 48
          Height = 13
          Caption = 'High z (m)'
        end
        object Label14: TLabel
          Left = 24
          Top = 80
          Width = 49
          Height = 13
          Caption = 'Low  z (m)'
        end
        object Label15: TLabel
          Left = 8
          Top = 102
          Width = 64
          Height = 13
          Caption = 'Max CHM (m)'
        end
        object Edit13: TEdit
          Left = 79
          Top = 75
          Width = 48
          Height = 21
          TabOrder = 0
          OnChange = Edit13Change
        end
        object Edit12: TEdit
          Left = 78
          Top = 48
          Width = 49
          Height = 21
          TabOrder = 1
          OnChange = Edit12Change
        end
        object Edit14: TEdit
          Left = 78
          Top = 102
          Width = 42
          Height = 21
          TabOrder = 2
          OnChange = Edit14Change
        end
        object CheckBox18: TCheckBox
          Left = 9
          Top = 25
          Width = 65
          Height = 17
          Caption = 'Use'
          TabOrder = 3
          OnClick = CheckBox18Click
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Class'
      ImageIndex = 7
      object BitBtn37: TBitBtn
        Left = 18
        Top = 3
        Width = 97
        Height = 25
        Caption = 'Classify (lastools)'
        TabOrder = 0
        OnClick = BitBtn37Click
      end
      object BitBtn46: TBitBtn
        Left = 18
        Top = 34
        Width = 97
        Height = 25
        Caption = 'Classify (mcc-lidar)'
        TabOrder = 1
        OnClick = BitBtn46Click
      end
      object BitBtn2: TBitBtn
        Left = 18
        Top = 97
        Width = 97
        Height = 25
        Caption = 'Classify (Whitebox)'
        TabOrder = 2
        OnClick = BitBtn2Click
      end
      object BitBtn3: TBitBtn
        Left = 121
        Top = 97
        Width = 97
        Height = 25
        Caption = 'WB Seg Classify'
        TabOrder = 3
        OnClick = BitBtn3Click
      end
      object BitBtn9: TBitBtn
        Left = 18
        Top = 66
        Width = 97
        Height = 25
        Caption = 'Classify (Fusion)'
        TabOrder = 4
        OnClick = BitBtn9Click
      end
      object BitBtn12: TBitBtn
        Left = 18
        Top = 128
        Width = 97
        Height = 25
        Caption = 'Classify PDAL pmf'
        TabOrder = 5
        OnClick = BitBtn12Click
      end
      object BitBtn10: TBitBtn
        Left = 121
        Top = 128
        Width = 97
        Height = 25
        Caption = 'Classify PDAL smrf'
        TabOrder = 6
        OnClick = BitBtn10Click
      end
      object BitBtn16: TBitBtn
        Left = 18
        Top = 159
        Width = 97
        Height = 25
        Caption = 'Classify (all)'
        TabOrder = 7
        OnClick = BitBtn16Click
      end
      object BitBtn22: TBitBtn
        Left = 341
        Top = 3
        Width = 105
        Height = 25
        Caption = 'LAStools settings'
        TabOrder = 8
        OnClick = BitBtn22Click
      end
      object BitBtn23: TBitBtn
        Left = 338
        Top = 34
        Width = 105
        Height = 25
        Caption = 'MCC  settings'
        TabOrder = 9
        OnClick = BitBtn23Click
      end
      object BitBtn33: TBitBtn
        Left = 185
        Top = 3
        Width = 109
        Height = 25
        Caption = 'Denoise (Whitebox)'
        TabOrder = 10
        OnClick = BitBtn33Click
      end
      object BitBtn36: TBitBtn
        Left = 338
        Top = 65
        Width = 105
        Height = 25
        Caption = 'Whitebox  settings'
        TabOrder = 11
        OnClick = BitBtn36Click
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 242
    Width = 581
    Height = 169
    Align = alBottom
    TabOrder = 2
    object RadioGroup1: TRadioGroup
      Left = 10
      Top = 2
      Width = 212
      Height = 135
      Caption = 'LAS Lidar color coding'
      Columns = 2
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object GroupBox7: TGroupBox
      Left = 228
      Top = 2
      Width = 222
      Height = 130
      Caption = 'Active point clouds'
      TabOrder = 1
      object CheckBoxPC1: TCheckBox
        Left = 29
        Top = 19
        Width = 206
        Height = 17
        Caption = 'CheckBoxPC1'
        TabOrder = 0
        OnClick = CheckBoxPC1Click
      end
      object CheckBoxPC2: TCheckBox
        Left = 29
        Top = 42
        Width = 216
        Height = 17
        Caption = 'CheckBoxPC2'
        TabOrder = 1
        Visible = False
        OnClick = CheckBoxPC2Click
      end
      object CheckBoxPC3: TCheckBox
        Left = 29
        Top = 65
        Width = 206
        Height = 17
        Caption = 'CheckBoxPC3'
        TabOrder = 2
        Visible = False
        OnClick = CheckBoxPC3Click
      end
      object CheckBoxPC4: TCheckBox
        Left = 29
        Top = 88
        Width = 206
        Height = 17
        Caption = 'CheckBoxPC4'
        TabOrder = 3
        Visible = False
        OnClick = CheckBoxPC4Click
      end
      object CheckBoxPC5: TCheckBox
        Left = 29
        Top = 109
        Width = 177
        Height = 17
        Caption = 'CheckBoxPC5'
        TabOrder = 4
        Visible = False
        OnClick = CheckBoxPC5Click
      end
      object SymbolPC4: TBitBtn
        Left = 7
        Top = 87
        Width = 16
        Height = 16
        TabOrder = 5
        OnClick = SymbolPC4Click
      end
      object SymbolPC3: TBitBtn
        Left = 7
        Top = 62
        Width = 16
        Height = 16
        TabOrder = 6
        OnClick = SymbolPC3Click
      end
      object SymbolPC2: TBitBtn
        Left = 7
        Top = 40
        Width = 16
        Height = 16
        TabOrder = 7
        OnClick = SymbolPC2Click
      end
      object SymbolPC1: TBitBtn
        Left = 7
        Top = 18
        Width = 16
        Height = 16
        TabOrder = 8
        OnClick = SymbolPC1Click
      end
      object SymbolPC5: TBitBtn
        Left = 7
        Top = 109
        Width = 16
        Height = 16
        TabOrder = 9
        OnClick = SymbolPC5Click
      end
    end
    object BitBtn27: TBitBtn
      Left = 142
      Top = 138
      Width = 55
      Height = 25
      Caption = 'Class cats'
      TabOrder = 2
      OnClick = BitBtn27Click
    end
    object BitBtn20: TBitBtn
      Left = 91
      Top = 138
      Width = 45
      Height = 25
      Caption = '3 views'
      TabOrder = 3
      OnClick = BitBtn20Click
    end
    object BitBtn32: TBitBtn
      Left = 361
      Top = 138
      Width = 89
      Height = 25
      Caption = 'Clear all clouds'
      TabOrder = 4
      OnClick = BitBtn32Click
    end
    object BitBtn64: TBitBtn
      Left = 264
      Top = 138
      Width = 82
      Height = 25
      Caption = 'Pick all clouds'
      TabOrder = 5
      OnClick = BitBtn64Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 145
    Top = 278
    object DSMNVSDTM1: TMenuItem
      Caption = 'DSM + NVS + DTM'
      OnClick = DSMNVSDTM1Click
    end
    object Ceilingfloor1: TMenuItem
      Caption = 'DSM + NVS'
      OnClick = Ceilingfloor1Click
    end
    object Ceiling1: TMenuItem
      Caption = 'DSM'
      OnClick = Ceiling1Click
    end
    object Floor1: TMenuItem
      Caption = 'NVS'
      OnClick = Floor1Click
    end
    object DTMfromgroundclassifiedpoints1: TMenuItem
      Caption = 'DTM from ground classified points'
      OnClick = DTMfromgroundclassifiedpoints1Click
    end
    object DTMrangescalesfromgroundpoints1: TMenuItem
      Caption = 'DTM range scales from ground points'
      OnClick = DTMrangescalesfromgroundpoints1Click
    end
    object ArcsecondDTMDSMforrangeofscales1: TMenuItem
      Caption = 'Arc second DTM/DSM for range of scales'
      OnClick = ArcsecondDTMDSMforrangeofscales1Click
    end
    object DTMfromlowestreturn1: TMenuItem
      Caption = 'DTM from lowest return'
      OnClick = DTMfromlowestreturn1Click
    end
    object Meanstandarddeviation1: TMenuItem
      Caption = 'Mean all returns'
      OnClick = Meanstandarddeviation1Click
    end
    object Meanfirstreturns1: TMenuItem
      Caption = 'Mean first returns'
      OnClick = Meanfirstreturns1Click
    end
    object DistanceabovebelowDEM1: TMenuItem
      Caption = 'Distance above/below DEM'
      OnClick = DistanceabovebelowDEM1Click
    end
    object Lowpointsxyz1: TMenuItem
      Caption = 'Low ground points x-y-z'
      OnClick = Lowpointsxyz1Click
    end
    object Lowpointxyz1: TMenuItem
      Caption = 'Low point x-y-z'
      OnClick = Lowpointxyz1Click
    end
  end
  object PopupMenu3: TPopupMenu
    Left = 27
    Top = 263
    object Pointcount1: TMenuItem
      Caption = 'All returns'
      OnClick = Pointcount1Click
    end
    object GroundoverDTM1: TMenuItem
      Caption = 'Ground classified returns'
      OnClick = GroundoverDTM1Click
    end
    object Firstreturns1: TMenuItem
      Caption = 'First returns'
      OnClick = Firstreturns1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object hreekeydensities1: TMenuItem
      Caption = 'Three key densities'
      OnClick = hreekeydensities1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Secondreturns1: TMenuItem
      Caption = 'Second returns'
      OnClick = Secondreturns1Click
    end
    object Singlereturns1: TMenuItem
      Caption = 'Single returns'
      OnClick = Singlereturns1Click
    end
    object Nonlastreturns1: TMenuItem
      Caption = 'Non last returns (air)'
      OnClick = Nonlastreturns1Click
    end
    object Overlappoints1: TMenuItem
      Caption = 'Overlap points'
      OnClick = Overlappoints1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Byclass1: TMenuItem
      Caption = 'Density by class'
      OnClick = Byclass1Click
    end
  end
  object PopupMenu4: TPopupMenu
    AutoPopup = False
    Left = 154
    Top = 338
    object Returnintensity1: TMenuItem
      Caption = 'Multiple return intensity grids'
      OnClick = Returnintensity1Click
    end
    object Maxintenstiy1: TMenuItem
      Caption = 'Max intensity'
      OnClick = Maxintenstiy1Click
    end
    object Minintensity1: TMenuItem
      Caption = 'Min intensity'
      OnClick = Minintensity1Click
    end
    object Classification1: TMenuItem
      Caption = 'Classification'
      OnClick = Classification1Click
    end
    object Scanangle1: TMenuItem
      Caption = 'Scan angle'
      OnClick = Scanangle1Click
    end
    object RGB1: TMenuItem
      Caption = 'RGB'
      OnClick = RGB1Click
    end
    object PointsourceID1: TMenuItem
      Caption = 'Point source ID (word)'
      OnClick = PointsourceID1Click
    end
    object Userdata1: TMenuItem
      Caption = 'User data (byte)'
      OnClick = Userdata1Click
    end
    object VegetationVoxcels1: TMenuItem
      Caption = 'Vegetation Voxcels'
      OnClick = VegetationVoxcels1Click
    end
    object Returndensityvoxcels1: TMenuItem
      Caption = 'Return density voxcels'
      OnClick = Returndensityvoxcels1Click
    end
    object Blankgrid1: TMenuItem
      Caption = 'Blank grid'
      OnClick = Blankgrid1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Everything1: TMenuItem
      Caption = 'Everything'
      OnClick = Everything1Click
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 62
    Top = 331
    object MICRODEM1: TMenuItem
      Caption = 'MICRODEM DTM'
      OnClick = MICRODEM1Click
    end
    object FusionTIN1: TMenuItem
      Caption = 'Fusion TIN DTM'
      OnClick = FusionTIN1Click
    end
    object WBIDW1: TMenuItem
      Caption = 'WB IDW DTM'
      OnClick = WBIDW1Click
    end
    object WBnearestneighbor1: TMenuItem
      Caption = 'WB nearest neighbor DTM'
      OnClick = WBnearestneighbor1Click
    end
    object blast2dem1: TMenuItem
      Caption = 'blast2dem DTM'
      OnClick = blast2dem1Click
    end
  end
end
