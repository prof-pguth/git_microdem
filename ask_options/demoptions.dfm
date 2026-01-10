inherited OptionsForm: TOptionsForm
  Left = 434
  Top = 230
  BorderStyle = bsSizeable
  Caption = ''
  ClientHeight = 563
  ClientWidth = 793
  Position = poDefaultPosOnly
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  ExplicitWidth = 809
  ExplicitHeight = 602
  TextHeight = 15
  inherited Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 793
    Height = 505
    Align = alClient
    ExplicitLeft = 0
    ExplicitTop = 1
    ExplicitWidth = 535
    ExplicitHeight = 529
  end
  inherited OKBtn: TButton
    Left = 8
    Top = 432
    Width = 49
    Visible = False
    OnClick = OKBtnClick
    ExplicitLeft = 8
    ExplicitTop = 432
    ExplicitWidth = 49
  end
  inherited CancelBtn: TButton
    Left = 63
    Top = 432
    Width = 61
    Visible = False
    ExplicitLeft = 63
    ExplicitTop = 432
    ExplicitWidth = 61
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 793
    Height = 505
    ActivePage = TabSheet11
    Align = alClient
    MultiLine = True
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Program'
      object RadioGroup7: TRadioGroup
        Left = 200
        Top = 3
        Width = 353
        Height = 134
        Caption = 'Menus'
        Columns = 2
        Items.Strings = (
          'Regular GIS'
          'Geology'
          'Physical geography'
          'Remote sensing'
          'DragonPlot')
        TabOrder = 0
        OnClick = RadioGroup7Click
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 3
        Width = 129
        Height = 17
        Caption = 'Show main tool bar'
        TabOrder = 1
      end
      object CheckBox120: TCheckBox
        Left = 16
        Top = 49
        Width = 97
        Height = 17
        Caption = 'Show menus'
        TabOrder = 2
      end
      object BitBtn9: TBitBtn
        Left = 200
        Top = 185
        Width = 75
        Height = 25
        Caption = 'Status bar'
        TabOrder = 3
        OnClick = BitBtn9Click
      end
      object CheckBox3: TCheckBox
        Left = 15
        Top = 26
        Width = 129
        Height = 17
        Caption = 'Show map toolbar'
        TabOrder = 4
      end
      object RadioGroup26: TRadioGroup
        Left = 15
        Top = 95
        Width = 157
        Height = 234
        Caption = 'Auto open'
        Items.Strings = (
          'Nothing'
          'Last project'
          'Last DEM'
          'Last image'
          'Last hyperspectral'
          'Last multigrid'
          'Last point cloud'
          'Last DB/shapefile'
          'Blank vector map'
          'Last lidar multi')
        TabOrder = 5
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Directories'
      ImageIndex = 2
      object StringGrid1: TStringGrid
        Left = 0
        Top = 0
        Width = 785
        Height = 455
        Align = alClient
        ColCount = 2
        ScrollBars = ssVertical
        TabOrder = 0
        OnSelectCell = StringGrid1SelectCell
        ColWidths = (
          64
          64)
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Maps'
      ImageIndex = 3
      object Label8: TLabel
        Left = 312
        Top = 13
        Width = 34
        Height = 15
        Caption = 'Label8'
      end
      object Label34: TLabel
        Left = 215
        Top = 40
        Width = 71
        Height = 15
        Caption = 'Max map size'
      end
      object Label17: TLabel
        Left = 219
        Top = 66
        Width = 135
        Height = 15
        Caption = 'Map memory check (MB)'
      end
      object Label11: TLabel
        Left = 128
        Top = 224
        Width = 3
        Height = 15
      end
      object Label4: TLabel
        Left = 240
        Top = 120
        Width = 137
        Height = 15
        Caption = 'Blow up extra margin (m) '
      end
      object TLabel
        Left = 16
        Top = 312
        Width = 3
        Height = 15
      end
      object Label22: TLabel
        Left = 176
        Top = 284
        Width = 78
        Height = 15
        Caption = 'Label decimals'
      end
      object Label18: TLabel
        Left = 272
        Top = 376
        Width = 211
        Height = 15
        Caption = 'Max distortion tolerated for scalebar (%)'
      end
      object RadioGroup6: TRadioGroup
        Left = 16
        Top = 3
        Width = 185
        Height = 105
        Caption = 'Default display'
        Items.Strings = (
          'Color reflectance'
          'Grayscale reflectance'
          'Elevation'
          'Contour'
          'Reversed grayscale')
        TabOrder = 0
      end
      object BitBtn1: TBitBtn
        Left = 3
        Top = 111
        Width = 104
        Height = 25
        Caption = 'Missing data'
        TabOrder = 1
        OnClick = BitBtn1Click
      end
      object Button9: TButton
        Left = 5
        Top = 215
        Width = 75
        Height = 25
        Caption = 'Contour maps'
        TabOrder = 2
        OnClick = Button9Click
      end
      object Button5: TButton
        Left = 207
        Top = 8
        Width = 99
        Height = 25
        Caption = 'Default size'
        TabOrder = 3
        OnClick = Button5Click
      end
      object Edit15: TEdit
        Left = 376
        Top = 32
        Width = 73
        Height = 23
        TabOrder = 4
      end
      object Edit18: TEdit
        Left = 391
        Top = 63
        Width = 49
        Height = 23
        TabOrder = 5
      end
      object CheckBox59: TCheckBox
        Left = 190
        Top = 152
        Width = 156
        Height = 17
        Caption = 'Gray scale merges'
        TabOrder = 6
      end
      object CheckBox51: TCheckBox
        Left = 368
        Top = 152
        Width = 97
        Height = 17
        Caption = 'Pan buttons'
        TabOrder = 7
      end
      object RadioGroup24: TRadioGroup
        Left = 409
        Top = 175
        Width = 176
        Height = 41
        Caption = 'Pan overlap'
        Columns = 3
        Items.Strings = (
          '3/4'
          '1/2'
          '1/4')
        TabOrder = 8
      end
      object CheckBox49: TCheckBox
        Left = 23
        Top = 192
        Width = 161
        Height = 17
        Caption = 'Preserve map aspect ratio'
        TabOrder = 9
      end
      object BitBtn13: TBitBtn
        Left = 86
        Top = 215
        Width = 75
        Height = 25
        Caption = 'Aspect maps'
        TabOrder = 10
        OnClick = BitBtn13Click
      end
      object CheckBox123: TCheckBox
        Left = 16
        Top = 250
        Width = 339
        Height = 17
        Caption = 'Automatically rescale colors for resized elevation maps'
        TabOrder = 11
      end
      object BitBtn16: TBitBtn
        Left = 260
        Top = 215
        Width = 105
        Height = 25
        Caption = 'Key locations'
        TabOrder = 12
        OnClick = BitBtn16Click
      end
      object CheckBox124: TCheckBox
        Left = 266
        Top = 349
        Width = 209
        Height = 17
        Caption = 'Allow scale bars, small scale maps'
        TabOrder = 13
      end
      object GroupBox11: TGroupBox
        Left = 425
        Top = 222
        Width = 160
        Height = 91
        Caption = 'Blow up size ('#186')'
        TabOrder = 14
        object Label39: TLabel
          Left = 15
          Top = 28
          Width = 19
          Height = 15
          Caption = ' Lat'
        end
        object Label40: TLabel
          Left = 10
          Top = 47
          Width = 27
          Height = 15
          Caption = 'Long'
        end
        object Edit21: TEdit
          Left = 63
          Top = 20
          Width = 58
          Height = 23
          TabOrder = 0
        end
        object Edit22: TEdit
          Left = 63
          Top = 47
          Width = 57
          Height = 23
          TabOrder = 1
        end
      end
      object CheckBox52: TCheckBox
        Left = 16
        Top = 287
        Width = 154
        Height = 17
        Caption = 'No DEM interpolations'
        TabOrder = 15
      end
      object Edit2: TEdit
        Left = 425
        Top = 118
        Width = 40
        Height = 23
        TabOrder = 16
      end
      object CheckBox43: TCheckBox
        Left = 190
        Top = 168
        Width = 156
        Height = 17
        Caption = 'Transparent icons'
        TabOrder = 17
      end
      object Edit24: TEdit
        Left = 281
        Top = 273
        Width = 51
        Height = 23
        TabOrder = 18
      end
      object CheckBox97: TCheckBox
        Left = 23
        Top = 152
        Width = 161
        Height = 17
        Caption = 'Box around quick exports'
        TabOrder = 19
      end
      object RadioGroup32: TRadioGroup
        Left = 483
        Top = 3
        Width = 97
        Height = 105
        Caption = 'Clipboard'
        Items.Strings = (
          'Quick'
          'Detailed'
          'Ask')
        TabOrder = 20
      end
      object BitBtn36: TBitBtn
        Left = 113
        Top = 111
        Width = 96
        Height = 25
        Caption = 'Blank map'
        TabOrder = 21
        OnClick = BitBtn36Click
      end
      object CheckBox160: TCheckBox
        Left = 266
        Top = 326
        Width = 314
        Height = 17
        Caption = 'Grayscale reflectance map with overlay'
        TabOrder = 22
      end
      object Edit4: TEdit
        Left = 547
        Top = 373
        Width = 48
        Height = 23
        TabOrder = 23
        Text = 'Edit4'
      end
      object CheckBox89: TCheckBox
        Left = 266
        Top = 397
        Width = 218
        Height = 17
        Caption = 'Map name below composite maps'
        TabOrder = 24
      end
      object CheckBox80: TCheckBox
        Left = 16
        Top = 310
        Width = 225
        Height = 17
        Caption = 'Elevation percentile color limits'
        TabOrder = 25
      end
      object BitBtn20: TBitBtn
        Left = 179
        Top = 215
        Width = 75
        Height = 25
        Caption = 'Slope Maps'
        TabOrder = 26
        OnClick = BitBtn20Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Vector maps'
      ImageIndex = 4
      object Button2: TButton
        Left = 220
        Top = 114
        Width = 65
        Height = 25
        Caption = 'TIGER '
        TabOrder = 0
        OnClick = Button2Click
      end
      object Button15: TButton
        Left = 291
        Top = 114
        Width = 82
        Height = 25
        Caption = 'Gazetteer'
        TabOrder = 1
        OnClick = Button15Click
      end
      object Button21: TButton
        Left = 379
        Top = 114
        Width = 49
        Height = 25
        Caption = 'PLSS'
        TabOrder = 2
        OnClick = Button21Click
      end
      object BitBtn14: TBitBtn
        Left = 126
        Top = 114
        Width = 88
        Height = 25
        Caption = 'US Outlines'
        TabOrder = 3
        OnClick = BitBtn14Click
      end
      object CheckBox101: TCheckBox
        Left = 304
        Top = 243
        Width = 182
        Height = 17
        Caption = 'Rotating earth outlines'
        TabOrder = 4
      end
      object GroupBox8: TGroupBox
        Left = 8
        Top = 3
        Width = 243
        Height = 105
        Caption = 'Shift Mercator to UTM'
        TabOrder = 5
        object Label13: TLabel
          Left = 16
          Top = 40
          Width = 89
          Height = 15
          Caption = 'Long size to shift'
        end
        object CheckBox66: TCheckBox
          Left = 24
          Top = 17
          Width = 199
          Height = 17
          Caption = 'Auto shift Mercator to UTM'
          TabOrder = 0
        end
        object Edit6: TEdit
          Left = 150
          Top = 39
          Width = 56
          Height = 23
          TabOrder = 1
        end
        object CheckBox108: TCheckBox
          Left = 24
          Top = 73
          Width = 199
          Height = 17
          Caption = 'Must be in single zone'
          TabOrder = 2
        end
      end
      object RadioGroup19: TRadioGroup
        Left = 3
        Top = 159
        Width = 270
        Height = 130
        Caption = 'Default projection'
        Items.Strings = (
          'Mercator (world)'
          'Mercator (CONUS)'
          'Albers equal area conic (CONUS)'
          'Lambert conformal conic (CONUS)'
          'Specified MICRODEM PRJ file')
        TabOrder = 6
      end
      object CheckBox39: TCheckBox
        Left = 304
        Top = 176
        Width = 161
        Height = 17
        Caption = 'World outline global DEMs'
        TabOrder = 7
      end
      object CheckBox41: TCheckBox
        Left = 304
        Top = 199
        Width = 161
        Height = 17
        Caption = 'World outline global imagery'
        TabOrder = 8
      end
      object BitBtn31: TBitBtn
        Left = 8
        Top = 114
        Width = 112
        Height = 25
        Caption = 'World outlines'
        TabOrder = 9
        OnClick = BitBtn31Click
      end
      object GroupBox6: TGroupBox
        Left = 271
        Top = 3
        Width = 130
        Height = 105
        Caption = 'GADM'
        TabOrder = 10
        object BitBtn34: TBitBtn
          Left = 16
          Top = 31
          Width = 97
          Height = 25
          Caption = 'Country'
          TabOrder = 0
          OnClick = BitBtn34Click
        end
        object BitBtn35: TBitBtn
          Left = 16
          Top = 62
          Width = 97
          Height = 25
          Caption = 'Province'
          TabOrder = 1
          OnClick = BitBtn35Click
        end
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Web'
      ImageIndex = 5
      object Label14: TLabel
        Left = 32
        Top = 288
        Width = 80
        Height = 15
        Caption = 'Google API key'
      end
      object GroupBox7: TGroupBox
        Left = 24
        Top = 39
        Width = 193
        Height = 77
        Caption = 'Grid server'
        TabOrder = 0
        object Label31: TLabel
          Left = 17
          Top = 16
          Width = 21
          Height = 15
          Caption = 'URL'
        end
        object Label32: TLabel
          Left = 16
          Top = 43
          Width = 22
          Height = 15
          Caption = 'Port'
        end
        object Edit13: TEdit
          Left = 56
          Top = 16
          Width = 121
          Height = 23
          TabOrder = 0
        end
        object Edit14: TEdit
          Left = 56
          Top = 43
          Width = 121
          Height = 23
          TabOrder = 1
        end
      end
      object CheckBox88: TCheckBox
        Left = 24
        Top = 16
        Width = 217
        Height = 17
        Caption = 'Enable grid network computing'
        TabOrder = 1
        OnClick = CheckBox88Click
      end
      object CheckBox83: TCheckBox
        Left = 24
        Top = 136
        Width = 217
        Height = 17
        Caption = 'Clear KML directory on closing program'
        TabOrder = 2
      end
      object BitBtn18: TBitBtn
        Left = 24
        Top = 159
        Width = 75
        Height = 25
        Caption = 'KML options'
        TabOrder = 3
        OnClick = BitBtn18Click
      end
      object CheckBox48: TCheckBox
        Left = 24
        Top = 190
        Width = 193
        Height = 17
        Caption = 'Skip check for program updates'
        TabOrder = 4
      end
      object CheckBox86: TCheckBox
        Left = 24
        Top = 215
        Width = 209
        Height = 17
        Caption = 'Backup EXE before web update'
        TabOrder = 5
      end
      object Edit1: TEdit
        Left = 128
        Top = 288
        Width = 361
        Height = 23
        TabOrder = 6
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Geology/Geography'
      ImageIndex = 6
      object Label38: TLabel
        Left = 136
        Top = 104
        Width = 3
        Height = 15
      end
      object BitBtn8: TBitBtn
        Left = 16
        Top = 56
        Width = 185
        Height = 32
        Caption = 'Net'
        Glyph.Data = {
          46020000424D460200000000000076000000280000001D0000001D0000000100
          040000000000D001000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFF0000000FFFFFFFFFFF000FFFFFFFF000F
          F000FF000FFFFFFFF000FFFFFFF0FFFF0F0F0FFFF0FFFFFFF000FFFFFF0F0FFF
          0F0F0FFF0F0FFFFFF000FFFFF0000000000000000000FFFFF000FFFF0FF0FFF0
          FF0FF0FFF0FF0FFFF000FFF0FF0FFF0FFF0FF0FFF0FFF0FFF000FF0FFF0FFF0F
          FF0FFF0FFF0FFF0FF000FF0000000000000000000000000FF000FF0FF0FFF0FF
          FF0FFF0FFFF0FF0FF000F0FFF0FF0FFFFF0FFFF0FFF0FFF0F000F0FF0FFF0FFF
          FF0FFFF0FFFF0FF0F000F0FF0FFF0FFFFF0FFFF0FFFF0FF0F000F00000000000
          0000000000000000F000F0FF0FFF0FFFFF0FFFF0FFFF0FF0F000F0FFF0FFF0FF
          FF0FFF0FFFF0FFF0F000F0FFF0FFF0FFFF0FFF0FFFF0FFF0F000FF0FF0FFF0FF
          FF0FFF0FFFF0FF0FF000FF0000000000000000000000000FF000FF0FFF0FFF0F
          FF0FF0FFFF0FFF0FF000FFF0FFF0FFF0FF0FF0FFF0FFF0FFF000FFFF0FF0FFF0
          FF0FF0FF0FFF0FFFF000FFFFF0000000000000000000FFFFF000FFFFFF0F0FFF
          0F0F0FF0FF0FFFFFF000FFFFFFF0F0FF0F0F0FF0F0FFFFFFF000FFFFFFFF000F
          F000FF000FFFFFFFF000FFFFFFFFFFF0000000FFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 0
        OnClick = BitBtn8Click
      end
      object CheckBox68: TCheckBox
        Left = 16
        Top = 207
        Width = 329
        Height = 17
        Caption = 'Complex magnetic anomaly profile models'
        TabOrder = 1
      end
      object BitBtn7: TBitBtn
        Left = 16
        Top = 16
        Width = 185
        Height = 32
        Caption = 'Stratcol'
        Glyph.Data = {
          76020000424D7602000000000000760000002800000020000000200000000100
          0400000000000002000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
          0000000000000AAAAAA00999999999999999999999000A000000099909999990
          9999990999900AA0000009990999999099999909999000AA0000090000099000
          009900000990000AA0000999099999909999990999900000AA00099909999990
          99999909990000000AA00999999999999999999999000AAAAAA0000000000000
          0000000000000800000003333333333333333333300000000000033333333333
          3333333330000000000003300333003330033300300000000000033003330033
          3003330030000000000003333333333333333333300000000000000000000000
          00000000000008AAAA000DDDDDDDDDDDDDDDDDDDD0000AA00AA00DDDDDDDDDDD
          DDDDDDDDDD000A0000A000DDDD000DDD000DDD000D000A0000000DDDDDDDDDDD
          DDDDDDDDDD00AAAA00000DDDDDDDDDDDDDDDDDDDDD008A0000000DD000DDD000
          DDD000DDDD008A0000A00DDDDDDDDDDDDDDDDDDDD0000AA00AA00DDDDDDDDDDD
          DDDDDDDDD00008AAAA00000000000000000000000000000000000C0CCCCC0CCC
          CCC0CCCC0000000000000C0CCCCC0CCCCCC0CCCC0000000000000C0CCCCC0CCC
          CCC0CCCCC00000000000000000000000000000000000000000000CCCC0CCCCCC
          0CCCCCC0CC00000000000CCCC0CCCCCC0CCCCCC0CC00000000000CCCC0CCCCCC
          0CCCCCC0CC000000000000000000000000000000000000000000}
        TabOrder = 2
        OnClick = BitBtn7Click
      end
      object Koppen: TBitBtn
        Left = 16
        Top = 136
        Width = 185
        Height = 33
        Caption = 'Koppen/Daylight'
        Glyph.Data = {
          76020000424D7602000000000000760000002800000020000000200000000100
          0400000000000002000000000000000000001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
          000000000000000000000CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00CCCCCCCCCCC
          CBBBBCCCCCCCCCCCCCC00CCBBBBBCCCCCBBBBCCCCCCCCBBBBBC00CCBBBBBCCCC
          CBBBBCCCCCCCBBBBBBC00CCBBBBBBCCCCBBBBCCCCCCBBBBBBBC00CCBBBBBBCCC
          CBBBBCCCCCBBBBBBBBC00CCCBBBBBBCCCBBBBCCCCBBBBBBBBCC00CCCBBBBBBCC
          CBBBBCCCBBBBBBBBCCC00CCCCBBBBBCCCCCCCCCCBBBBBBBCCCC00CCCCBBBBBCC
          BBBBBBBCBBBBBBCCCCC00CCCCCBBBBCBBBBBBBBBCCBBBCCCCCC00CCCCCCCCCBB
          BBBBBBBBBCCBCCCCCCC00CCCCCCCCBBBBBBBBBBBBBCCCCCCCCC00CCCCCCCBBBB
          BBBBBBBBBBBCCCCCCCC0BBBBBBBCBBBBBBBBBBBBBBBCBBBBBBC0BBBBBBBCBBBB
          BBBBBBBBBBBCBBBBBBC0BBBBBBBCBBBBBBBBBBBBBBBCBBBBBBC0BBBBBBBCBBBB
          BBBBBBBBBBBCBBBBBBC00CCCCCCCBBBBBBBBBBBBBBBCCCCCCCC00CCCCCCCCBBB
          BBBBBBBBBBCCCCCCCCC00CCCCCCCCCBBBBBBBBBBBCCCCCCCCCC00CCCCCCCCCCB
          BBBBBBBBCCCCCCCCCCC00CCCCBBBBBCCBBBBBBBCCCBBBBBCCCC00CCCBBBBBBCC
          CCCCCCCCCCBBBBBBCCC00CCBBBBBBBCCCCCCCCCCCCBBBBBBBCC00CBBBBBBBBCC
          CBBBBCCCCCBBBBBBBBC00CBBBBBBBCCCCBBBBCCCCCCBBBBBBBB00CBBBBBBCCCC
          CBBBBCCCCCCCBBBBBBB00CBBBBBCCCCCCBBBBCCCCCCCCBBBBBB00CBBBBCCCCCC
          CBBBBCCCCCCCCCBBBBB00000000000000BBBB0000000000BBBB0}
        TabOrder = 3
        OnClick = KoppenClick
      end
      object BitBtn2: TBitBtn
        Left = 16
        Top = 94
        Width = 185
        Height = 36
        Caption = 'Focal mech'
        Glyph.Data = {
          96010000424D9601000000000000760000002800000018000000180000000100
          0400000000002001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFF0000000000FFFFFFFFFFFFF000000000000FFFFFFFFFF
          0000FFFFFFFF0000FFFFFFF0000FFFFFFFFFF0000FFFFFF000FFFFFFFFFFFFF0
          0FFFFF00000000FFFFFFFFFF00FFFF00000000000FFFFFFF00FFF00F00000000
          000FFFFFF00FF00F0000000000000FFFF00FF00FF0000000000000FFF00FF00F
          F00000000000000FF00FF00FFF0000000000000FF00FF00FFF00000000000000
          F00FF00FFFF0000000000000F00FF00FFFFF000000000000000FFF00FFFFF000
          0000000000FFFF00FFFFFFF000000FFF00FFFFF00FFFFFFFFFFFFFF00FFFFFF0
          000FFFFFFFFFF0000FFFFFFF0000FFFFFFFF0000FFFFFFFFFF000000000000FF
          FFFFFFFFFFF0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        TabOrder = 4
        OnClick = BitBtn2Click
      end
      object CheckBox76: TCheckBox
        Left = 16
        Top = 230
        Width = 297
        Height = 17
        Caption = 'Ternary percentages must sum to 100%'
        TabOrder = 5
      end
      object BitBtn21: TBitBtn
        Left = 16
        Top = 272
        Width = 177
        Height = 25
        Caption = 'Get geology data'
        Enabled = False
        TabOrder = 6
        OnClick = BitBtn21Click
      end
      object BitBtn33: TBitBtn
        Left = 16
        Top = 303
        Width = 177
        Height = 25
        Caption = 'Get geography data'
        Enabled = False
        TabOrder = 7
        OnClick = BitBtn33Click
      end
      object CheckBox53: TCheckBox
        Left = 280
        Top = 96
        Width = 265
        Height = 17
        Caption = 'Geography databases to RAM'
        TabOrder = 8
      end
      object CheckBox57: TCheckBox
        Left = 280
        Top = 119
        Width = 216
        Height = 17
        Caption = 'Geology databases to RAM'
        TabOrder = 9
      end
      object CheckBox46: TCheckBox
        Left = 280
        Top = 160
        Width = 265
        Height = 17
        Caption = 'Show old climate stations DB'
        TabOrder = 10
      end
      object BitBtn22: TBitBtn
        Left = 16
        Top = 334
        Width = 177
        Height = 25
        Caption = 'Get ETOPO1/Blue Marble'
        Enabled = False
        TabOrder = 11
        OnClick = BitBtn22Click
      end
      object CheckBox93: TCheckBox
        Left = 280
        Top = 56
        Width = 216
        Height = 17
        Caption = 'Custom sidescan palette'
        TabOrder = 12
      end
      object BitBtn42: TBitBtn
        Left = 16
        Top = 400
        Width = 177
        Height = 25
        Caption = 'Get Natural Earth Vectors'
        Enabled = False
        TabOrder = 13
        OnClick = BitBtn42Click
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Import/Export'
      ImageIndex = 7
      object Label30: TLabel
        Left = 24
        Top = 314
        Width = 3
        Height = 15
      end
      object CheckBox26: TCheckBox
        Left = 24
        Top = 3
        Width = 137
        Height = 17
        Caption = 'Wrap ETOPO5'
        TabOrder = 0
      end
      object Button22: TButton
        Left = 24
        Top = 283
        Width = 177
        Height = 25
        Caption = 'Default import datum'
        TabOrder = 1
        OnClick = Button22Click
      end
      object CheckBox20: TCheckBox
        Left = 552
        Top = 47
        Width = 257
        Height = 17
        Caption = 'Missing values set to sea level'
        TabOrder = 2
      end
      object CheckBox56: TCheckBox
        Left = 24
        Top = 171
        Width = 265
        Height = 17
        Caption = 'Prompt to save new grids when created'
        TabOrder = 3
      end
      object CheckBox98: TCheckBox
        Left = 279
        Top = 191
        Width = 210
        Height = 17
        Caption = 'Assume -99 missing'
        TabOrder = 4
      end
      object CheckBox99: TCheckBox
        Left = 279
        Top = 208
        Width = 145
        Height = 17
        Caption = 'Assume -999 missing'
        TabOrder = 5
      end
      object CheckBox100: TCheckBox
        Left = 279
        Top = 231
        Width = 189
        Height = 17
        Caption = 'Assume -9999 missing'
        TabOrder = 6
      end
      object CheckBox91: TCheckBox
        Left = 279
        Top = 168
        Width = 244
        Height = 17
        Caption = 'Assume negative values missing'
        TabOrder = 7
      end
      object CheckBox130: TCheckBox
        Left = 24
        Top = 102
        Width = 153
        Height = 17
        Caption = 'Verify all world files'
        TabOrder = 8
      end
      object CheckBox122: TCheckBox
        Left = 24
        Top = 125
        Width = 225
        Height = 17
        Caption = 'Open vegetation grid map'
        TabOrder = 9
      end
      object CheckBox55: TCheckBox
        Left = 279
        Top = 247
        Width = 163
        Height = 17
        Caption = 'Assume -99999 missing'
        TabOrder = 10
      end
      object CheckBox64: TCheckBox
        Left = 279
        Top = 263
        Width = 145
        Height = 17
        Caption = 'Assume -999999 missing'
        TabOrder = 11
      end
      object RadioGroup25: TRadioGroup
        Left = 42
        Top = 26
        Width = 199
        Height = 42
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          '1.5 times'
          '2 times')
        TabOrder = 12
      end
      object GroupBox12: TGroupBox
        Left = 257
        Top = 326
        Width = 227
        Height = 115
        Caption = 'GeoJSON'
        TabOrder = 13
        object Label28: TLabel
          Left = 16
          Top = 21
          Width = 98
          Height = 15
          Caption = 'Points--z decimals'
        end
        object Label45: TLabel
          Left = 16
          Top = 43
          Width = 98
          Height = 15
          Caption = 'Grids--xy decimals'
        end
        object Label46: TLabel
          Left = 15
          Top = 72
          Width = 92
          Height = 15
          Caption = 'Grids--z decimals'
        end
        object Edit27: TEdit
          Left = 145
          Top = 14
          Width = 56
          Height = 23
          TabOrder = 0
        end
        object Edit34: TEdit
          Left = 145
          Top = 48
          Width = 56
          Height = 23
          TabOrder = 1
        end
        object Edit35: TEdit
          Left = 145
          Top = 82
          Width = 56
          Height = 23
          TabOrder = 2
        end
      end
      object CheckBox23: TCheckBox
        Left = 24
        Top = 148
        Width = 177
        Height = 17
        Caption = 'GDAL for Geotiff export'
        TabOrder = 14
      end
      object GDAL: TBitBtn
        Left = 24
        Top = 333
        Width = 75
        Height = 25
        Caption = 'GDAL'
        TabOrder = 15
        OnClick = GDALClick
      end
      object Button6: TButton
        Left = 24
        Top = 368
        Width = 75
        Height = 25
        Caption = 'Nav Opts'
        TabOrder = 16
        OnClick = Button6Click
      end
      object CheckBox36: TCheckBox
        Left = 32
        Top = 400
        Width = 136
        Height = 17
        Caption = 'Nav with FIT loading'
        TabOrder = 17
      end
      object CheckBox79: TCheckBox
        Left = 536
        Top = 231
        Width = 209
        Height = 17
        Caption = 'Delete FIT files after import'
        TabOrder = 18
      end
      object CheckBox54: TCheckBox
        Left = 279
        Top = 280
        Width = 148
        Height = 17
        Caption = 'Assume -32767 missing'
        TabOrder = 19
      end
      object BitBtn29: TBitBtn
        Left = 105
        Top = 333
        Width = 112
        Height = 25
        Caption = 'Output grids'
        TabOrder = 20
        OnClick = BitBtn29Click
      end
      object CheckBox300: TCheckBox
        Left = 552
        Top = 24
        Width = 273
        Height = 17
        Caption = 'Sea level to missing on import'
        TabOrder = 21
      end
      object GroupBox16: TGroupBox
        Left = 544
        Top = 102
        Width = 249
        Height = 105
        Caption = 'Hole filling'
        TabOrder = 22
        object Label24: TLabel
          Left = 29
          Top = 69
          Width = 75
          Height = 15
          Caption = 'Max gap to fill'
        end
        object Edit8: TEdit
          Left = 143
          Top = 66
          Width = 65
          Height = 23
          TabOrder = 0
        end
        object CheckBox33: TCheckBox
          Left = 3
          Top = 20
          Width = 222
          Height = 17
          Caption = 'Auto fill holes on DEM merge'
          TabOrder = 1
        end
        object CheckBox127: TCheckBox
          Left = 3
          Top = 43
          Width = 214
          Height = 17
          Caption = 'Neighbors only in 2 directions'
          TabOrder = 2
        end
      end
      object CheckBox21: TCheckBox
        Left = 536
        Top = 280
        Width = 289
        Height = 17
        Caption = 'Delete metadada Geotiff DEM files'
        TabOrder = 23
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Views'
      ImageIndex = 8
      object Label3: TLabel
        Left = 11
        Top = 37
        Width = 123
        Height = 15
        Caption = 'Live fly extra delay (ms)'
      end
      object Button12: TButton
        Left = 3
        Top = 3
        Width = 117
        Height = 25
        Caption = 'Weapons Fans'
        TabOrder = 0
        OnClick = Button12Click
      end
      object BitBtn6: TBitBtn
        Left = 126
        Top = 3
        Width = 83
        Height = 25
        Caption = 'Ambush'
        TabOrder = 1
        OnClick = BitBtn6Click
      end
      object Edit17: TEdit
        Left = 173
        Top = 34
        Width = 54
        Height = 23
        TabOrder = 2
        Text = ' '
      end
      object Button18: TButton
        Left = 215
        Top = 3
        Width = 147
        Height = 25
        Caption = 'Vert Earth curvature'
        TabOrder = 3
        OnClick = Button18Click
      end
      object BitBtn23: TBitBtn
        Left = 492
        Top = 3
        Width = 149
        Height = 25
        Caption = 'IHS overlay colors'
        TabOrder = 4
      end
      object BitBtn26: TBitBtn
        Left = 368
        Top = 3
        Width = 118
        Height = 25
        Caption = 'LOS Algorithm'
        TabOrder = 5
        OnClick = BitBtn26Click
      end
      object RadioGroup12: TRadioGroup
        Left = 3
        Top = 208
        Width = 164
        Height = 129
        Caption = 'Movies'
        Items.Strings = (
          ' BMP'
          'JPEG'
          'GeoTiff'
          'BMP w/ world'
          'JPEG w/ world')
        TabOrder = 6
      end
      object RadioGroup20: TRadioGroup
        Left = 3
        Top = 106
        Width = 184
        Height = 89
        Caption = 'Map with vegetation'
        Items.Strings = (
          'DTM'
          'HAG (Vegetation)'
          'DSM')
        TabOrder = 7
      end
      object GroupBox10: TGroupBox
        Left = 273
        Top = 34
        Width = 280
        Height = 247
        Caption = 'OpenGL 3D viewer'
        TabOrder = 8
        object Label19: TLabel
          Left = 11
          Top = 20
          Width = 105
          Height = 15
          Caption = 'Max initial triangles,'
        end
        object Label42: TLabel
          Left = 16
          Top = 48
          Width = 58
          Height = 15
          Caption = 'Max points'
        end
        object Label43: TLabel
          Left = 16
          Top = 72
          Width = 76
          Height = 15
          Caption = 'Point thinning'
        end
        object Edit7: TEdit
          Left = 150
          Top = 17
          Width = 89
          Height = 23
          TabOrder = 0
          Text = ' '
        end
        object CheckBox131: TCheckBox
          Left = 16
          Top = 128
          Width = 241
          Height = 17
          Caption = 'OpenGL no vertical exaggeration'
          TabOrder = 1
        end
        object CheckBox85: TCheckBox
          Left = 17
          Top = 105
          Width = 184
          Height = 17
          Caption = 'OpenGL movies'
          TabOrder = 2
        end
        object Edit31: TEdit
          Left = 150
          Top = 44
          Width = 89
          Height = 23
          TabOrder = 3
        end
        object Edit32: TEdit
          Left = 150
          Top = 70
          Width = 89
          Height = 23
          TabOrder = 4
        end
        object CheckBox19: TCheckBox
          Left = 16
          Top = 151
          Width = 161
          Height = 17
          Caption = 'New drapes'
          TabOrder = 5
        end
        object CheckBox50: TCheckBox
          Left = 16
          Top = 176
          Width = 193
          Height = 17
          Caption = 'New OpenGL viewer'
          TabOrder = 6
        end
        object CheckBox145: TCheckBox
          Left = 16
          Top = 200
          Width = 185
          Height = 17
          Caption = 'OpenGL clean overlays'
          TabOrder = 7
        end
        object CheckBox167: TCheckBox
          Left = 16
          Top = 223
          Width = 113
          Height = 17
          Caption = 'New 3D viewer'
          TabOrder = 8
        end
      end
      object RadioGroup34: TRadioGroup
        Left = 3
        Top = 58
        Width = 164
        Height = 41
        Caption = 'Frame line width'
        Columns = 3
        DragMode = dmAutomatic
        Items.Strings = (
          '1'
          '2'
          '3')
        TabOrder = 9
      end
    end
    object TabSheet10: TTabSheet
      Caption = 'Hardware'
      ImageIndex = 9
      object Label33: TLabel
        Left = 11
        Top = 4
        Width = 100
        Height = 15
        Caption = 'Max Active threads'
      end
      object Label21: TLabel
        Left = 21
        Top = 169
        Width = 66
        Height = 15
        Caption = 'JPEG Quality'
      end
      object Label48: TLabel
        Left = 22
        Top = 199
        Width = 97
        Height = 15
        Caption = 'Update delay (sec)'
      end
      object Label6: TLabel
        Left = 280
        Top = 6
        Width = 34
        Height = 15
        Caption = 'Label6'
      end
      object CheckBox31: TCheckBox
        Left = 21
        Top = 77
        Width = 193
        Height = 17
        Caption = 'Debug log operational'
        TabOrder = 0
      end
      object Edit23: TEdit
        Left = 143
        Top = 3
        Width = 65
        Height = 23
        TabOrder = 1
      end
      object BitBtn27: TBitBtn
        Left = 3
        Top = 23
        Width = 75
        Height = 25
        Caption = 'Digitizing'
        TabOrder = 2
        OnClick = BitBtn27Click
      end
      object Edit11: TEdit
        Left = 152
        Top = 169
        Width = 48
        Height = 23
        TabOrder = 3
      end
      object CheckBox73: TCheckBox
        Left = 22
        Top = 100
        Width = 229
        Height = 17
        Caption = 'Save transparent GIF'
        TabOrder = 4
      end
      object Edit37: TEdit
        Left = 152
        Top = 203
        Width = 49
        Height = 23
        TabOrder = 5
      end
      object CheckBox157: TCheckBox
        Left = 23
        Top = 123
        Width = 199
        Height = 17
        Caption = 'Show WinExec window'
        TabOrder = 6
      end
      object CheckBox166: TCheckBox
        Left = 21
        Top = 264
        Width = 332
        Height = 17
        Caption = 'Stay awake (for GPS, bulk processing)'
        TabOrder = 7
      end
      object CheckBox18: TCheckBox
        Left = 21
        Top = 146
        Width = 147
        Height = 17
        Caption = 'Log DOS commands'
        TabOrder = 8
      end
    end
    object TabSheet11: TTabSheet
      Caption = 'Analysis'
      ImageIndex = 10
      object Label36: TLabel
        Left = 97
        Top = 164
        Width = 65
        Height = 15
        Caption = 'Graph width'
      end
      object Label7: TLabel
        Left = 40
        Top = 128
        Width = 34
        Height = 15
        Caption = 'Label7'
      end
      object Label2: TLabel
        Left = 32
        Top = 248
        Width = 42
        Height = 15
        Caption = 'Clip (%)'
      end
      object Label23: TLabel
        Left = 416
        Top = 120
        Width = 40
        Height = 15
        Caption = 'Label23'
      end
      object Label47: TLabel
        Left = 143
        Top = 386
        Width = 40
        Height = 15
        Caption = 'Label47'
      end
      object Label49: TLabel
        Left = 143
        Top = 424
        Width = 40
        Height = 15
        Caption = 'Label49'
      end
      object UpDown1: TUpDown
        Left = 16
        Top = 120
        Width = 16
        Height = 24
        Min = 1
        Max = 15
        Position = 1
        TabOrder = 0
        OnClick = UpDown1Click
      end
      object CheckBox16: TCheckBox
        Left = 8
        Top = 8
        Width = 259
        Height = 17
        Caption = 'Use sea level elevations for statistics'
        TabOrder = 1
      end
      object CheckBox107: TCheckBox
        Left = 273
        Top = 3
        Width = 257
        Height = 17
        Caption = 'Terrain category percentages'
        TabOrder = 2
      end
      object Graph: TBitBtn
        Left = 16
        Top = 159
        Width = 75
        Height = 25
        Caption = 'Graph'
        TabOrder = 3
        OnClick = GraphClick
      end
      object CheckBox125: TCheckBox
        Left = 273
        Top = 26
        Width = 234
        Height = 17
        Caption = 'Confirm openness directions'
        TabOrder = 4
      end
      object CheckBox8: TCheckBox
        Left = 16
        Top = 216
        Width = 235
        Height = 17
        Caption = 'Clip tails for histograms'
        TabOrder = 5
      end
      object Edit5: TEdit
        Left = 100
        Top = 245
        Width = 78
        Height = 23
        TabOrder = 6
        Text = 'Edit5'
      end
      object GroupBox2: TGroupBox
        Left = 273
        Top = 143
        Width = 272
        Height = 106
        Caption = 'Openness'
        TabOrder = 7
        object Label26: TLabel
          Left = 176
          Top = 24
          Width = 58
          Height = 15
          Caption = 'Height (m)'
        end
        object RadioGroup9: TRadioGroup
          Left = 16
          Top = 16
          Width = 129
          Height = 87
          Caption = 'Location'
          Items.Strings = (
            'On ground'
            'Antenna'
            'Flying')
          TabOrder = 0
        end
        object Edit26: TEdit
          Left = 176
          Top = 50
          Width = 81
          Height = 23
          TabOrder = 1
          Text = 'Edit26'
        end
      end
      object BitBtn19: TBitBtn
        Left = 264
        Top = 72
        Width = 195
        Height = 25
        Caption = 'Dune height/wavelength'
        TabOrder = 8
        OnClick = BitBtn19Click
      end
      object Button3: TButton
        Left = 264
        Top = 112
        Width = 137
        Height = 25
        Caption = 'Terrain blouw up'
        TabOrder = 9
        OnClick = Button3Click
      end
      object CheckBox149: TCheckBox
        Left = 264
        Top = 265
        Width = 369
        Height = 17
        Caption = 'Cumulative freq on normal prob axis'
        TabOrder = 10
      end
      object CheckBox150: TCheckBox
        Left = 264
        Top = 288
        Width = 243
        Height = 17
        Caption = 'Quantile ranges (not 1 std dev)'
        TabOrder = 11
      end
      object RadioGroup30: TRadioGroup
        Left = 3
        Top = 279
        Width = 152
        Height = 49
        Caption = 'SSO trend quadrants'
        Columns = 3
        Items.Strings = (
          '1'
          '2'
          '4')
        TabOrder = 12
      end
      object CheckBox152: TCheckBox
        Left = 264
        Top = 311
        Width = 266
        Height = 17
        Caption = 'Geomorph grids full coverage'
        TabOrder = 13
      end
      object CheckBox155: TCheckBox
        Left = 16
        Top = 193
        Width = 177
        Height = 17
        Caption = 'Deliberate histograms'
        TabOrder = 14
      end
      object BitBtn30: TBitBtn
        Left = 264
        Top = 334
        Width = 121
        Height = 25
        Caption = 'Least cost path'
        TabOrder = 15
        OnClick = BitBtn30Click
      end
      object GroupBox18: TGroupBox
        Left = 584
        Top = 8
        Width = 193
        Height = 238
        Caption = 'DEMIX'
        TabOrder = 16
        object DTfilllabel: TLabel
          Left = 10
          Top = 21
          Width = 103
          Height = 15
          Caption = 'Tile fill required (%)'
        end
        object Edit12: TEdit
          Left = 128
          Top = 18
          Width = 49
          Height = 23
          TabOrder = 0
          Text = 'Edit12'
        end
        object CheckBox5: TCheckBox
          Left = 16
          Top = 47
          Width = 137
          Height = 17
          Caption = 'Do CHMs'
          TabOrder = 1
        end
        object CheckBox154: TCheckBox
          Left = 16
          Top = 70
          Width = 169
          Height = 17
          Caption = 'Do air or dirt maps'
          TabOrder = 2
        end
        object CheckBox158: TCheckBox
          Left = 16
          Top = 93
          Width = 185
          Height = 17
          Caption = 'Do elevation differences'
          TabOrder = 3
        end
        object CheckBox162: TCheckBox
          Left = 16
          Top = 116
          Width = 169
          Height = 17
          Caption = 'Do slope differences'
          TabOrder = 4
        end
        object CheckBox169: TCheckBox
          Left = 16
          Top = 139
          Width = 193
          Height = 17
          Caption = 'Do roughness differences'
          TabOrder = 5
        end
        object CheckBox180: TCheckBox
          Left = 16
          Top = 162
          Width = 193
          Height = 17
          Caption = 'Do half second DEMs'
          TabOrder = 6
        end
        object CheckBox25: TCheckBox
          Left = 16
          Top = 185
          Width = 209
          Height = 17
          Caption = 'Elevation parmeter graphs'
          TabOrder = 7
        end
        object CheckBox181: TCheckBox
          Left = 16
          Top = 208
          Width = 161
          Height = 17
          Caption = 'Composite images'
          TabOrder = 8
        end
      end
      object CheckBox161: TCheckBox
        Left = 584
        Top = 386
        Width = 153
        Height = 17
        Caption = 'Process loops forward'
        TabOrder = 17
      end
      object BitBtn39: TBitBtn
        Left = 10
        Top = 382
        Width = 127
        Height = 25
        Caption = 'Curvature algorithm'
        TabOrder = 18
        OnClick = BitBtn39Click
      end
      object BitBtn43: TBitBtn
        Left = 10
        Top = 417
        Width = 127
        Height = 25
        Caption = 'Slope algorithm'
        TabOrder = 19
        OnClick = BitBtn43Click
      end
    end
    object TabSheet13: TTabSheet
      Caption = 'Units'
      ImageIndex = 12
      object RadioGroup3: TRadioGroup
        Left = 3
        Top = 119
        Width = 161
        Height = 90
        Caption = 'Lat/Long'
        Items.Strings = (
          'Decimal degrees'
          'Decimal minutes'
          'Decimal seconds')
        TabOrder = 0
      end
      object RadioGroup2: TRadioGroup
        Left = 8
        Top = 8
        Width = 161
        Height = 105
        Caption = 'Locations'
        Items.Strings = (
          'Lat/Long'
          'MGRS'
          'Short MGRS'
          ' UTM')
        TabOrder = 1
      end
      object RadioGroup11: TRadioGroup
        Left = 184
        Top = 8
        Width = 145
        Height = 73
        Caption = 'Elevations'
        Items.Strings = (
          'Meters'
          'Feet')
        TabOrder = 2
      end
      object RadioGroup14: TRadioGroup
        Left = 400
        Top = 8
        Width = 193
        Height = 65
        Caption = 'Azimuth angle'
        Items.Strings = (
          'Decimal degrees'
          'Degrees and minutes')
        TabOrder = 3
      end
      object RadioGroup22: TRadioGroup
        Left = 400
        Top = 79
        Width = 193
        Height = 65
        Caption = 'Pitch angle'
        Items.Strings = (
          'Decimal degrees'
          'Degrees and minutes')
        TabOrder = 4
      end
      object RadioGroup23: TRadioGroup
        Left = 184
        Top = 87
        Width = 145
        Height = 97
        Caption = 'Speed'
        Items.Strings = (
          'meters/sec'
          'km/hr'
          'miles/hr'
          'knots')
        TabOrder = 5
      end
      object RadioGroup10: TRadioGroup
        Left = 184
        Top = 190
        Width = 145
        Height = 81
        Caption = 'Distances'
        Items.Strings = (
          'Metric'
          'English'
          'Nautical miles')
        TabOrder = 6
      end
    end
    object TabSheet14: TTabSheet
      Caption = 'Coords'
      ImageIndex = 13
      object RadioGroup5: TRadioGroup
        Left = 234
        Top = 88
        Width = 167
        Height = 81
        Caption = 'Verify'
        Items.Strings = (
          'Graphical blowup'
          'Keyboard entry')
        TabOrder = 0
      end
      object RadioGroup4: TRadioGroup
        Left = 3
        Top = 88
        Width = 206
        Height = 81
        Caption = 'Verify Graphical Selections'
        Items.Strings = (
          'None'
          'Reasonable'
          'All')
        TabOrder = 1
      end
      object CheckBox30: TCheckBox
        Left = 3
        Top = 65
        Width = 257
        Height = 17
        Caption = 'MGRS and Lat/Long when roaming'
        TabOrder = 2
      end
      object CheckBox27: TCheckBox
        Left = 3
        Top = 42
        Width = 225
        Height = 17
        Caption = 'Both datums when roaming'
        TabOrder = 3
      end
      object CheckBox2: TCheckBox
        Left = 294
        Top = 42
        Width = 243
        Height = 17
        Caption = 'Satellite image coordinates'
        TabOrder = 4
      end
      object CheckBox1: TCheckBox
        Left = 3
        Top = 19
        Width = 257
        Height = 17
        Caption = 'Dual elevations (feet and meters)'
        TabOrder = 5
      end
      object BitBtn25: TBitBtn
        Left = 250
        Top = 184
        Width = 106
        Height = 25
        Caption = 'Roam cursor'
        TabOrder = 6
        OnClick = BitBtn25Click
      end
      object CheckBox133: TCheckBox
        Left = 294
        Top = 65
        Width = 211
        Height = 17
        Caption = 'DEM grid coordinates'
        TabOrder = 7
      end
      object CheckBox110: TCheckBox
        Left = 3
        Top = 187
        Width = 206
        Height = 17
        Caption = 'Show roam on all maps'
        TabOrder = 8
      end
      object CheckBox6: TCheckBox
        Left = 294
        Top = 19
        Width = 243
        Height = 17
        Caption = 'All z in status bar while roaming'
        TabOrder = 9
      end
      object CheckBox146: TCheckBox
        Left = 592
        Top = 24
        Width = 217
        Height = 17
        Caption = 'Show projected coordinates'
        TabOrder = 10
      end
    end
    object TabSheet15: TTabSheet
      Caption = 'Database'
      ImageIndex = 14
      object Label15: TLabel
        Left = 48
        Top = 279
        Width = 120
        Height = 15
        Caption = 'Number of mask fields'
      end
      object Label37: TLabel
        Left = 319
        Top = 85
        Width = 124
        Height = 15
        Caption = 'Min integer field length'
      end
      object Label41: TLabel
        Left = 328
        Top = 424
        Width = 208
        Height = 15
        Caption = 'Min VAT percentage to show in legends'
      end
      object CheckBox69: TCheckBox
        Left = 16
        Top = 371
        Width = 153
        Height = 17
        Caption = 'Save DB status'
        TabOrder = 0
      end
      object CheckBox10: TCheckBox
        Left = 16
        Top = 81
        Width = 257
        Height = 17
        Caption = 'Auto enhance shape files'
        TabOrder = 1
      end
      object CheckBox61: TCheckBox
        Left = 16
        Top = 96
        Width = 265
        Height = 17
        Caption = 'Show second database toolbar'
        TabOrder = 2
      end
      object CheckBox62: TCheckBox
        Left = 16
        Top = 112
        Width = 257
        Height = 17
        Caption = 'Show boundaries of colored regions'
        TabOrder = 3
      end
      object BitBtn24: TBitBtn
        Left = 16
        Top = 191
        Width = 116
        Height = 25
        Caption = 'Areas'
        TabOrder = 4
      end
      object RadioGroup18: TRadioGroup
        Left = 319
        Top = 0
        Width = 178
        Height = 72
        Caption = 'GIS Edits'
        Items.Strings = (
          'Never allowed'
          'Option to allow'
          'Always allowed')
        TabOrder = 5
      end
      object CheckBox96: TCheckBox
        Left = 16
        Top = 16
        Width = 201
        Height = 17
        Caption = 'Avoid text overwriting '
        TabOrder = 6
      end
      object CheckBox17: TCheckBox
        Left = 16
        Top = 32
        Width = 169
        Height = 17
        Caption = 'Plot on all maps'
        TabOrder = 7
      end
      object CheckBox109: TCheckBox
        Left = 16
        Top = 255
        Width = 234
        Height = 17
        Caption = 'Add MASK field when merging shape files'
        TabOrder = 8
      end
      object Edit10: TEdit
        Left = 204
        Top = 278
        Width = 65
        Height = 23
        TabOrder = 9
      end
      object OutlineButton: TBitBtn
        Left = 16
        Top = 224
        Width = 116
        Height = 25
        Caption = 'Outline selections'
        TabOrder = 10
        OnClick = OutlineButtonClick
      end
      object CheckBox29: TCheckBox
        Left = 16
        Top = 168
        Width = 208
        Height = 17
        Caption = 'Autoassign Label field'
        TabOrder = 11
      end
      object BitBtn15: TBitBtn
        Left = 138
        Top = 227
        Width = 113
        Height = 25
        Caption = 'Default points'
        TabOrder = 12
        OnClick = BitBtn15Click
      end
      object CheckBox11: TCheckBox
        Left = 312
        Top = 40
        Width = 1
        Height = 25
        Caption = 'CheckBox11'
        TabOrder = 13
      end
      object CheckBox84: TCheckBox
        Left = 320
        Top = 245
        Width = 241
        Height = 17
        Caption = 'Quick shape file definitions'
        TabOrder = 14
      end
      object CheckBox129: TCheckBox
        Left = 320
        Top = 268
        Width = 241
        Height = 17
        Caption = 'Multiple filters for DB graphs'
        TabOrder = 15
      end
      object CheckBox12: TCheckBox
        Left = 319
        Top = 291
        Width = 297
        Height = 17
        Caption = 'Start STAT graphs with reversed y-axis'
        TabOrder = 16
      end
      object CheckBox22: TCheckBox
        Left = 320
        Top = 198
        Width = 153
        Height = 17
        Caption = 'Edit in DB grid'
        TabOrder = 17
      end
      object CheckBox42: TCheckBox
        Left = 16
        Top = 152
        Width = 234
        Height = 17
        Caption = 'Modal record displays'
        TabOrder = 18
      end
      object CheckBox82: TCheckBox
        Left = 320
        Top = 150
        Width = 213
        Height = 17
        Caption = 'Minimize table on opening'
        TabOrder = 19
      end
      object Edit29: TEdit
        Left = 495
        Top = 82
        Width = 46
        Height = 23
        TabOrder = 20
      end
      object CheckBox115: TCheckBox
        Left = 16
        Top = 65
        Width = 137
        Height = 17
        Caption = 'Create 3D shapefiles'
        TabOrder = 21
      end
      object CheckBox117: TCheckBox
        Left = 16
        Top = 48
        Width = 208
        Height = 17
        Caption = 'Save intermdiate DBFs'
        TabOrder = 22
      end
      object CheckBox136: TCheckBox
        Left = 320
        Top = 128
        Width = 273
        Height = 17
        Caption = 'Use map pixel size plotting rules'
        TabOrder = 23
      end
      object CheckBox153: TCheckBox
        Left = 16
        Top = 328
        Width = 234
        Height = 17
        Caption = 'Include open grids in DB ID'
        TabOrder = 24
      end
      object CheckBox118: TCheckBox
        Left = 320
        Top = 104
        Width = 118
        Height = 17
        Caption = 'Show stats new fields'
        TabOrder = 25
      end
      object CheckBox159: TCheckBox
        Left = 320
        Top = 221
        Width = 241
        Height = 17
        Caption = 'Filter to map extent on opening'
        TabOrder = 26
      end
      object CheckBox38: TCheckBox
        Left = 16
        Top = 348
        Width = 234
        Height = 17
        Caption = 'Duplicate input lines allowed'
        TabOrder = 27
      end
      object CheckBox95: TCheckBox
        Left = 16
        Top = 307
        Width = 249
        Height = 17
        Caption = 'Always show coordinate edits'
        TabOrder = 28
      end
      object CheckBox165: TCheckBox
        Left = 16
        Top = 394
        Width = 153
        Height = 17
        Caption = 'Save DB Filter'
        TabOrder = 29
      end
      object GroupBox13: TGroupBox
        Left = 311
        Top = 314
        Width = 202
        Height = 97
        Caption = 'Speed up operations'
        TabOrder = 30
        object CheckBox37: TCheckBox
          Left = 17
          Top = 40
          Width = 216
          Height = 17
          Caption = 'Linked DBs moved to RAM'
          TabOrder = 0
        end
        object CheckBox121: TCheckBox
          Left = 16
          Top = 17
          Width = 201
          Height = 17
          Caption = 'Track database ranges'
          TabOrder = 1
        end
        object CheckBox35: TCheckBox
          Left = 16
          Top = 63
          Width = 233
          Height = 17
          Caption = 'Allow DBs to RAM'
          TabOrder = 2
        end
      end
      object CheckBox60: TCheckBox
        Left = 16
        Top = 419
        Width = 234
        Height = 17
        Caption = 'Record display toolbar on top'
        TabOrder = 31
      end
      object CheckBox81: TCheckBox
        Left = 319
        Top = 175
        Width = 178
        Height = 17
        Caption = 'Confirm DB edits'
        TabOrder = 32
      end
      object Edit36: TEdit
        Left = 544
        Top = 424
        Width = 61
        Height = 23
        TabOrder = 33
        Text = 'Edit36'
      end
    end
    object TabSheet16: TTabSheet
      Caption = 'Imagery'
      ImageIndex = 15
      object CheckBox40: TCheckBox
        Left = 24
        Top = 161
        Width = 161
        Height = 17
        Caption = 'Average zoomed out imagery'
        TabOrder = 0
      end
      object CheckBox45: TCheckBox
        Left = 24
        Top = 184
        Width = 153
        Height = 17
        Caption = 'Ignore Zero in histograms'
        TabOrder = 1
      end
      object CheckBox139: TCheckBox
        Left = 24
        Top = 221
        Width = 205
        Height = 17
        Caption = 'Multibands to grids'
        TabOrder = 2
      end
      object CheckBox140: TCheckBox
        Left = 24
        Top = 240
        Width = 229
        Height = 17
        Caption = 'Multibands true color'
        TabOrder = 3
      end
      object CheckBox141: TCheckBox
        Left = 24
        Top = 263
        Width = 293
        Height = 17
        Caption = 'Multibands normalize spectra'
        TabOrder = 4
      end
      object CheckBox143: TCheckBox
        Left = 24
        Top = 286
        Width = 245
        Height = 17
        Caption = 'Bands by wavelength'
        TabOrder = 5
      end
      object RadioGroup8: TRadioGroup
        Left = 300
        Top = 3
        Width = 245
        Height = 126
        Caption = 'Band computations'
        Items.Strings = (
          'DN'
          'Radiance'
          'Reflectance'
          'Reflectance w/sun elevation')
        TabOrder = 6
      end
      object CheckBox70: TCheckBox
        Left = 24
        Top = 120
        Width = 209
        Height = 17
        Caption = 'Icon open imagery via directories'
        TabOrder = 7
      end
      object CheckBox102: TCheckBox
        Left = 24
        Top = 97
        Width = 270
        Height = 17
        Caption = 'Delete JP2 after import to Geotiff'
        TabOrder = 8
      end
      object GroupBox17: TGroupBox
        Left = 300
        Top = 145
        Width = 381
        Height = 240
        Caption = 'ACOLITE'
        TabOrder = 9
        object Label10: TLabel
          Left = 16
          Top = 118
          Width = 65
          Height = 15
          Caption = 'L2W params'
        end
        object RadioGroup15: TRadioGroup
          Left = 16
          Top = 187
          Width = 305
          Height = 41
          Caption = 'Sentinal-2 output resolution (m)'
          Columns = 3
          Items.Strings = (
            '10'
            '20'
            '60')
          TabOrder = 0
        end
        object Edit9: TEdit
          Left = 35
          Top = 144
          Width = 309
          Height = 23
          TabOrder = 1
        end
        object CheckBox144: TCheckBox
          Left = 16
          Top = 92
          Width = 161
          Height = 17
          Caption = 'Delete miscellaneous'
          TabOrder = 2
        end
        object CheckBox128: TCheckBox
          Left = 16
          Top = 69
          Width = 97
          Height = 17
          Caption = 'Delete nc'
          TabOrder = 3
        end
        object CheckBox92: TCheckBox
          Left = 16
          Top = 23
          Width = 177
          Height = 17
          Caption = 'Delete rhos'
          TabOrder = 4
        end
        object CheckBox116: TCheckBox
          Left = 16
          Top = 46
          Width = 97
          Height = 17
          Caption = 'Delete rhot'
          TabOrder = 5
        end
      end
    end
    object TabSheet12: TTabSheet
      Caption = 'Indexes'
      ImageIndex = 16
      object Label20: TLabel
        Left = 184
        Top = 88
        Width = 40
        Height = 15
        Caption = 'Label20'
      end
      object CheckBox34: TCheckBox
        Left = 19
        Top = 27
        Width = 246
        Height = 17
        Caption = 'Automatically close index maps'
        TabOrder = 0
      end
      object BitBtn40: TBitBtn
        Left = 24
        Top = 80
        Width = 129
        Height = 25
        Caption = 'Map libary'
        TabOrder = 1
        OnClick = BitBtn40Click
      end
      object BitBtn41: TBitBtn
        Left = 24
        Top = 128
        Width = 129
        Height = 25
        Caption = 'Library settings'
        TabOrder = 2
        OnClick = BitBtn41Click
      end
    end
    object TabSheet18: TTabSheet
      Caption = 'Datum'
      ImageIndex = 17
      object Label1: TLabel
        Left = 112
        Top = 16
        Width = 34
        Height = 15
        Caption = 'Label1'
      end
      object Label5: TLabel
        Left = 136
        Top = 84
        Width = 34
        Height = 15
        Caption = 'Label5'
      end
      object Button1: TButton
        Left = 24
        Top = 8
        Width = 75
        Height = 25
        Caption = 'UTM zone'
        TabOrder = 0
        OnClick = Button1Click
      end
      object RadioGroup16: TRadioGroup
        Left = 24
        Top = 40
        Width = 113
        Height = 33
        Caption = 'Hemisphere'
        Columns = 2
        Items.Strings = (
          'North'
          'South')
        TabOrder = 1
      end
      object RadioGroup17: TRadioGroup
        Left = 136
        Top = 40
        Width = 105
        Height = 33
        Caption = ' '
        Columns = 2
        Items.Strings = (
          'West '
          'East')
        TabOrder = 2
      end
      object Button4: TButton
        Left = 24
        Top = 79
        Width = 89
        Height = 25
        Caption = 'Primary datum'
        TabOrder = 3
        OnClick = Button4Click
      end
      object BitBtn28: TBitBtn
        Left = 24
        Top = 144
        Width = 75
        Height = 25
        Caption = 'Geoid file'
        TabOrder = 4
        OnClick = BitBtn28Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'LAS'
      ImageIndex = 16
      object Label9: TLabel
        Left = 9
        Top = 112
        Width = 127
        Height = 15
        Caption = 'Profile extract width (m)'
      end
      object Label16: TLabel
        Left = 4
        Top = 142
        Width = 103
        Height = 15
        Caption = 'Max pts to memory'
      end
      object Edit19: TEdit
        Left = 175
        Top = 109
        Width = 49
        Height = 23
        TabOrder = 0
      end
      object BitBtn12: TBitBtn
        Left = 10
        Top = 215
        Width = 110
        Height = 33
        Caption = 'LAS Colors'
        TabOrder = 1
        OnClick = BitBtn12Click
      end
      object RadioGroup29: TRadioGroup
        Left = 3
        Top = 166
        Width = 166
        Height = 43
        Caption = 'Slice symbol size'
        Columns = 4
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2'
          '3'
          '4')
        TabOrder = 2
      end
      object Edit20: TEdit
        Left = 158
        Top = 138
        Width = 76
        Height = 23
        TabOrder = 3
      end
      object RadioGroup35: TRadioGroup
        Left = 328
        Top = 3
        Width = 145
        Height = 105
        Caption = 'Point density'
        Items.Strings = (
          'Max'
          'Mean'
          'Min')
        TabOrder = 4
      end
      object CheckBox164: TCheckBox
        Left = 152
        Top = 42
        Width = 170
        Height = 17
        Caption = 'LAS RGB color filter'
        TabOrder = 5
      end
      object GroupBox14: TGroupBox
        Left = 3
        Top = 3
        Width = 137
        Height = 92
        Caption = 'Load to memory'
        TabOrder = 6
        object CheckBox163: TCheckBox
          Left = 3
          Top = 62
          Width = 172
          Height = 17
          Caption = 'RGB'
          TabOrder = 0
        end
        object CheckBox47: TCheckBox
          Left = 3
          Top = 39
          Width = 185
          Height = 17
          Caption = 'Return number'
          TabOrder = 1
        end
        object CheckBox14: TCheckBox
          Left = 3
          Top = 16
          Width = 172
          Height = 17
          Caption = 'Classification'
          TabOrder = 2
        end
      end
      object RadioGroup1: TRadioGroup
        Left = 296
        Top = 136
        Width = 177
        Height = 105
        Caption = 'LVIS waveform graphs'
        Items.Strings = (
          'Histogram'
          'Cumulative'
          'Both')
        TabOrder = 7
      end
    end
    object TabSheet17: TTabSheet
      Caption = 'Grids'
      ImageIndex = 17
      object Label44: TLabel
        Left = 286
        Top = 40
        Width = 100
        Height = 15
        Caption = 'Min/max tolerance'
      end
      object CheckBox77: TCheckBox
        Left = 286
        Top = 17
        Width = 251
        Height = 17
        Caption = 'Any NoData means  NoData'
        TabOrder = 0
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 17
        Width = 238
        Height = 248
        Caption = 'Multi grid statistics--create grids'
        TabOrder = 1
        object CheckBox63: TCheckBox
          Left = 34
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Median'
          TabOrder = 0
        end
        object CheckBox75: TCheckBox
          Left = 33
          Top = 152
          Width = 125
          Height = 17
          Caption = 'Envelope thickness'
          TabOrder = 1
        end
        object CheckBox74: TCheckBox
          Left = 34
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Ceiling'
          TabOrder = 2
        end
        object CheckBox71: TCheckBox
          Left = 34
          Top = 87
          Width = 97
          Height = 17
          Caption = 'Floor'
          TabOrder = 3
        end
        object CheckBox67: TCheckBox
          Left = 34
          Top = 133
          Width = 97
          Height = 17
          Caption = 'Num  Pts'
          TabOrder = 4
        end
        object CheckBox65: TCheckBox
          Left = 34
          Top = 110
          Width = 97
          Height = 17
          Caption = 'Std Dev'
          TabOrder = 5
        end
        object CheckBox58: TCheckBox
          Left = 34
          Top = 41
          Width = 97
          Height = 17
          Caption = 'Mean'
          TabOrder = 6
        end
        object CheckBox147: TCheckBox
          Left = 34
          Top = 175
          Width = 131
          Height = 17
          Caption = 'Grid with min'
          TabOrder = 7
        end
        object CheckBox148: TCheckBox
          Left = 34
          Top = 198
          Width = 124
          Height = 17
          Caption = 'Grid with max'
          TabOrder = 8
        end
      end
      object Edit33: TEdit
        Left = 440
        Top = 40
        Width = 121
        Height = 23
        TabOrder = 2
      end
      object CheckBox13: TCheckBox
        Left = 286
        Top = 104
        Width = 169
        Height = 17
        Caption = 'Filter grids to edge'
        TabOrder = 3
      end
    end
    object TabSheet20: TTabSheet
      Caption = 'Vegetation'
      ImageIndex = 19
      object Label25: TLabel
        Left = 16
        Top = 120
        Width = 164
        Height = 15
        Caption = 'Max density for vertical profiles'
      end
      object CheckBox24: TCheckBox
        Left = 16
        Top = 24
        Width = 193
        Height = 17
        Caption = 'Auto load veg grid'
        TabOrder = 0
      end
      object CheckBox78: TCheckBox
        Left = 16
        Top = 48
        Width = 217
        Height = 17
        Caption = 'Auto load veg density grids'
        TabOrder = 1
      end
      object CheckBox87: TCheckBox
        Left = 16
        Top = 72
        Width = 217
        Height = 17
        Caption = 'Nine point averge for density profiles'
        TabOrder = 2
      end
      object Edit25: TEdit
        Left = 230
        Top = 117
        Width = 50
        Height = 23
        TabOrder = 3
        OnChange = Edit25Change
      end
      object GroupBox3: TGroupBox
        Left = 300
        Top = 26
        Width = 221
        Height = 114
        Caption = 'OpenGL Vegetation Density'
        TabOrder = 4
        object Label29: TLabel
          Left = 14
          Top = 87
          Width = 86
          Height = 15
          Caption = 'Location+/- (m)'
        end
        object CheckBox90: TCheckBox
          Left = 8
          Top = 16
          Width = 169
          Height = 17
          Caption = 'Show/export ground'
          TabOrder = 0
        end
        object CheckBox94: TCheckBox
          Left = 8
          Top = 63
          Width = 161
          Height = 17
          Caption = 'Randomize points'
          TabOrder = 1
        end
        object Edit28: TEdit
          Left = 122
          Top = 83
          Width = 61
          Height = 23
          TabOrder = 2
        end
        object CheckBox104: TCheckBox
          Left = 8
          Top = 39
          Width = 161
          Height = 17
          Caption = 'Show/export buildings'
          TabOrder = 3
        end
      end
    end
    object TabSheet21: TTabSheet
      Caption = 'Menus'
      ImageIndex = 20
      object GroupBox4: TGroupBox
        Left = 467
        Top = 18
        Width = 102
        Height = 97
        Caption = 'Oceanography'
        TabOrder = 0
        object CheckBox105: TCheckBox
          Left = 3
          Top = 25
          Width = 97
          Height = 17
          Caption = 'General'
          TabOrder = 0
        end
        object CheckBox111: TCheckBox
          Left = 3
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Sidescan'
          TabOrder = 1
        end
        object CheckBox112: TCheckBox
          Left = 3
          Top = 71
          Width = 97
          Height = 17
          Caption = 'Chirps'
          TabOrder = 2
        end
      end
      object GroupBox5: TGroupBox
        Left = 8
        Top = 8
        Width = 185
        Height = 84
        Caption = 'Data sets'
        TabOrder = 1
        object CheckBox113: TCheckBox
          Left = 11
          Top = 33
          Width = 169
          Height = 17
          Caption = 'Global topography'
          TabOrder = 0
        end
        object CheckBox114: TCheckBox
          Left = 11
          Top = 56
          Width = 166
          Height = 17
          Caption = 'Blue marble imagery'
          TabOrder = 1
        end
      end
      object GroupBox9: TGroupBox
        Left = 3
        Top = 112
        Width = 185
        Height = 217
        Caption = 'Geology'
        TabOrder = 2
        object CheckBox119: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Geology options'
          TabOrder = 0
        end
        object CheckBox126: TCheckBox
          Left = 16
          Top = 74
          Width = 97
          Height = 17
          Caption = 'Stereo net'
          TabOrder = 1
        end
        object CheckBox132: TCheckBox
          Left = 16
          Top = 97
          Width = 97
          Height = 17
          Caption = 'Stratcol'
          TabOrder = 2
        end
        object CheckBox134: TCheckBox
          Left = 16
          Top = 120
          Width = 97
          Height = 17
          Caption = 'Ternary'
          TabOrder = 3
        end
        object CheckBox135: TCheckBox
          Left = 16
          Top = 143
          Width = 97
          Height = 17
          Caption = 'Marine geology'
          TabOrder = 4
        end
        object CheckBox15: TCheckBox
          Left = 16
          Top = 166
          Width = 129
          Height = 17
          Caption = 'Plate rotations'
          TabOrder = 5
        end
        object CheckBox137: TCheckBox
          Left = 15
          Top = 189
          Width = 97
          Height = 17
          Caption = 'Sieve'
          TabOrder = 6
        end
        object CheckBox138: TCheckBox
          Left = 16
          Top = 47
          Width = 149
          Height = 17
          Caption = 'Geomorphometry'
          TabOrder = 7
        end
      end
      object CheckBox142: TCheckBox
        Left = 224
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Show labs'
        TabOrder = 3
        Visible = False
      end
      object CheckBox151: TCheckBox
        Left = 224
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Show views'
        TabOrder = 4
      end
      object CheckBox156: TCheckBox
        Left = 224
        Top = 104
        Width = 201
        Height = 17
        Caption = 'Advanced  DB options'
        TabOrder = 5
      end
      object CheckBox9: TCheckBox
        Left = 224
        Top = 136
        Width = 97
        Height = 17
        Caption = 'SHP button'
        TabOrder = 6
      end
      object CheckBox32: TCheckBox
        Left = 224
        Top = 160
        Width = 97
        Height = 17
        Caption = 'DB button'
        TabOrder = 7
      end
      object CheckBox72: TCheckBox
        Left = 224
        Top = 184
        Width = 97
        Height = 17
        Caption = 'Map library'
        TabOrder = 8
      end
      object CheckBox28: TCheckBox
        Left = 224
        Top = 232
        Width = 97
        Height = 17
        Caption = 'PLSS'
        TabOrder = 9
      end
      object CheckBox103: TCheckBox
        Left = 224
        Top = 255
        Width = 124
        Height = 17
        Caption = 'Point clouds'
        TabOrder = 10
      end
      object CheckBox44: TCheckBox
        Left = 224
        Top = 278
        Width = 233
        Height = 17
        Caption = 'Experimental slice options'
        TabOrder = 11
      end
      object CheckBox106: TCheckBox
        Left = 224
        Top = 18
        Width = 158
        Height = 17
        Caption = 'DEMIX wine contest'
        TabOrder = 12
      end
      object BitBtn38: TBitBtn
        Left = 528
        Top = 180
        Width = 121
        Height = 25
        Caption = 'Graph colors'
        TabOrder = 13
        OnClick = BitBtn38Click
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 505
    Width = 793
    Height = 58
    Align = alBottom
    TabOrder = 3
    object BitBtn3: TBitBtn
      Left = 178
      Top = 6
      Width = 77
      Height = 25
      Caption = 'Help'
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn32: TBitBtn
      Left = 261
      Top = 6
      Width = 99
      Height = 25
      Caption = 'Save Copy'
      TabOrder = 1
      OnClick = BitBtn32Click
    end
    object BitBtn10: TBitBtn
      Left = 12
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
      TabOrder = 2
      OnClick = OKBtnClick
      IsControl = True
    end
    object BitBtn17: TBitBtn
      Left = 95
      Top = 7
      Width = 77
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      Margin = 2
      ModalResult = 2
      NumGlyphs = 2
      ParentFont = False
      Spacing = -1
      TabOrder = 3
      OnClick = BitBtn17Click
      IsControl = True
    end
    object BitBtn4: TBitBtn
      Left = 261
      Top = 32
      Width = 99
      Height = 25
      Caption = 'Import copy'
      TabOrder = 4
      OnClick = BitBtn4Click
    end
    object BitBtn11: TBitBtn
      Left = 366
      Top = 6
      Width = 131
      Height = 25
      Caption = 'Restore defaults'
      TabOrder = 5
      OnClick = BitBtn11Click
    end
    object BitBtn5: TBitBtn
      Left = 366
      Top = 32
      Width = 131
      Height = 25
      Caption = 'Recreate INI file'
      TabOrder = 6
      OnClick = BitBtn5Click
    end
    object BitBtn37: TBitBtn
      Left = 513
      Top = 6
      Width = 96
      Height = 25
      Caption = 'View INI file'
      TabOrder = 7
      OnClick = BitBtn37Click
    end
  end
  object ColorDialog1: TColorDialog
    Left = 736
    Top = 456
  end
end
