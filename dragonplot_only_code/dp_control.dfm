object DragonPlotForm: TDragonPlotForm
  Left = 766
  Top = 222
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Fire Tower Information'
  ClientHeight = 539
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 499
    Width = 412
    Height = 40
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 500
    ExplicitWidth = 416
    object BitBtn3: TBitBtn
      Left = 130
      Top = 6
      Width = 46
      Height = 25
      Caption = 'Archive'
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object BitBtn10: TBitBtn
      Left = 339
      Top = 6
      Width = 55
      Height = 25
      Caption = 'Close DP'
      TabOrder = 1
      OnClick = BitBtn10Click
    end
    object BitBtn6: TBitBtn
      Left = 295
      Top = 6
      Width = 38
      Height = 25
      Caption = 'Clear'
      TabOrder = 2
      OnClick = BitBtn6Click
    end
    object BitBtn11: TBitBtn
      Left = 180
      Top = 6
      Width = 33
      Height = 25
      Caption = 'KML'
      Enabled = False
      TabOrder = 3
      OnClick = BitBtn11Click
    end
    object BitBtn1: TBitBtn
      Left = 400
      Top = 6
      Width = 24
      Height = 25
      Caption = 'Inst'
      TabOrder = 4
      Visible = False
      OnClick = BitBtn1Click
    end
    object PrimaryTowerComboBox7: TComboBox
      Left = 4
      Top = 6
      Width = 120
      Height = 21
      TabOrder = 5
      OnChange = PrimaryTowerComboBox7Change
    end
    object BitBtn36: TBitBtn
      Left = 219
      Top = 6
      Width = 70
      Height = 25
      Caption = 'Google Map'
      Enabled = False
      TabOrder = 6
      OnClick = BitBtn36Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 20
    Width = 428
    Height = 153
    ActivePage = TabSheet9
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Single'
      object Label3: TLabel
        Left = -1
        Top = 32
        Width = 37
        Height = 13
        Caption = 'Azimuth'
      end
      object Label9: TLabel
        Left = 10
        Top = 67
        Width = 24
        Height = 13
        Caption = 'Pitch'
      end
      object Label19: TLabel
        Left = 18
        Top = 51
        Width = 17
        Height = 13
        Caption = '('#176'T)'
      end
      object GroupBox1: TGroupBox
        Left = 42
        Top = 3
        Width = 129
        Height = 89
        Caption = 'Primary tower'
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Top = 16
          Width = 40
          Height = 13
          Caption = 'Degrees'
        end
        object Label2: TLabel
          Left = 76
          Top = 16
          Width = 37
          Height = 13
          Caption = 'Minutes'
        end
        object Edit3: TEdit
          Left = 15
          Top = 31
          Width = 41
          Height = 21
          TabOrder = 0
        end
        object Edit1: TEdit
          Left = 72
          Top = 31
          Width = 41
          Height = 21
          TabOrder = 1
        end
        object Edit2: TEdit
          Left = 72
          Top = 56
          Width = 41
          Height = 21
          TabOrder = 3
        end
        object Edit9: TEdit
          Left = 16
          Top = 56
          Width = 41
          Height = 21
          TabOrder = 2
        end
      end
      object BitBtn2: TBitBtn
        Left = 177
        Top = 57
        Width = 48
        Height = 25
        Caption = 'Plot'
        Enabled = False
        TabOrder = 1
        OnClick = BitBtn2Click
      end
      object CheckBox1: TCheckBox
        Left = 224
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Show LOS'
        TabOrder = 2
        OnClick = CheckBox1Click
      end
      object BitBtn5: TBitBtn
        Left = 303
        Top = 57
        Width = 64
        Height = 25
        Caption = 'Time'
        Enabled = False
        TabOrder = 3
        Visible = False
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Cross'
      ImageIndex = 6
      object Label22: TLabel
        Left = 8
        Top = 64
        Width = 37
        Height = 13
        Caption = 'Azimuth'
      end
      object Label23: TLabel
        Left = 16
        Top = 80
        Width = 17
        Height = 13
        Caption = '('#176'T)'
      end
      object SecondaryTowerComboBox2: TComboBox
        Left = 160
        Top = 6
        Width = 129
        Height = 21
        TabOrder = 0
        OnChange = SecondaryTowerComboBox2Change
      end
      object GroupBox2: TGroupBox
        Left = 160
        Top = 33
        Width = 105
        Height = 72
        Caption = 'Tower 2'
        Enabled = False
        TabOrder = 2
        object Label17: TLabel
          Left = 8
          Top = 16
          Width = 40
          Height = 13
          Caption = 'Degrees'
        end
        object Label18: TLabel
          Left = 60
          Top = 16
          Width = 37
          Height = 13
          Caption = 'Minutes'
        end
        object Edit14: TEdit
          Left = 9
          Top = 35
          Width = 41
          Height = 21
          Enabled = False
          TabOrder = 0
        end
        object Edit15: TEdit
          Left = 56
          Top = 35
          Width = 41
          Height = 21
          Enabled = False
          TabOrder = 1
        end
      end
      object GroupBox3: TGroupBox
        Left = 49
        Top = 33
        Width = 105
        Height = 72
        Caption = 'Primary Tower'
        TabOrder = 1
        object Label20: TLabel
          Left = 8
          Top = 16
          Width = 40
          Height = 13
          Caption = 'Degrees'
        end
        object Label21: TLabel
          Left = 60
          Top = 16
          Width = 37
          Height = 13
          Caption = 'Minutes'
        end
        object Edit16: TEdit
          Left = 9
          Top = 35
          Width = 41
          Height = 21
          TabOrder = 0
        end
        object Edit17: TEdit
          Left = 56
          Top = 35
          Width = 41
          Height = 21
          TabOrder = 1
        end
      end
      object CrossShotBitBtn8: TBitBtn
        Left = 271
        Top = 80
        Width = 51
        Height = 25
        Caption = 'Plot'
        TabOrder = 3
        OnClick = CrossShotBitBtn8Click
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Locate'
      ImageIndex = 7
      object Label24: TLabel
        Left = 126
        Top = 8
        Width = 3
        Height = 13
      end
      object Label26: TLabel
        Left = 128
        Top = 32
        Width = 3
        Height = 13
      end
      object BitBtn12: TBitBtn
        Left = 15
        Top = 0
        Width = 105
        Height = 25
        Caption = 'PLSS'
        TabOrder = 0
        OnClick = BitBtn12Click
      end
      object BitBtn13: TBitBtn
        Left = 113
        Top = 62
        Width = 75
        Height = 25
        Caption = 'Plot'
        Enabled = False
        TabOrder = 1
        OnClick = BitBtn13Click
      end
      object BitBtn14: TBitBtn
        Left = 15
        Top = 31
        Width = 105
        Height = 25
        Caption = 'Lat/Long (WGS84)'
        TabOrder = 2
        OnClick = BitBtn14Click
      end
      object BitBtn4: TBitBtn
        Left = 15
        Top = 97
        Width = 146
        Height = 25
        Caption = 'Bearing/distance from tower'
        TabOrder = 3
        OnClick = BitBtn4Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Maps'
      ImageIndex = 3
      object GroupBox5: TGroupBox
        Left = 0
        Top = 3
        Width = 281
        Height = 122
        Caption = 'Target map'
        TabOrder = 0
        object Label16: TLabel
          Left = 116
          Top = 61
          Width = 45
          Height = 13
          Caption = 'Long size'
        end
        object Label15: TLabel
          Left = 116
          Top = 15
          Width = 36
          Height = 13
          Caption = 'Lat size'
        end
        object RadioGroup2: TRadioGroup
          Left = 20
          Top = 16
          Width = 90
          Height = 58
          Caption = 'Source'
          Items.Strings = (
            'DEM/Tiger'
            'GeoTiff DRG')
          TabOrder = 0
          OnClick = RadioGroup2Click
        end
        object CheckBox3: TCheckBox
          Left = 3
          Top = 80
          Width = 97
          Height = 17
          Caption = 'Terrain shadows'
          TabOrder = 1
          OnClick = CheckBox3Click
        end
        object RadioGroup3: TRadioGroup
          Left = 188
          Top = 8
          Width = 82
          Height = 89
          Caption = 'Map size'
          Items.Strings = (
            'Zoom In'
            'Medium'
            'Zoom out'
            'Custom')
          TabOrder = 2
          OnClick = RadioGroup3Click
        end
        object Edit13: TEdit
          Left = 125
          Top = 80
          Width = 57
          Height = 21
          TabOrder = 3
          Text = '0.0075'
          OnChange = Edit13Change
        end
        object Edit12: TEdit
          Left = 125
          Top = 34
          Width = 57
          Height = 21
          TabOrder = 4
          Text = '0.05'
          OnChange = Edit12Change
        end
      end
      object BitBtn9: TBitBtn
        Left = 287
        Top = 3
        Width = 130
        Height = 25
        Caption = 'Redraw regional map'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
          FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
          CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
          FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
        TabOrder = 1
        OnClick = BitBtn9Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'View'
      ImageIndex = 1
      object Label12: TLabel
        Left = 185
        Top = 59
        Width = 29
        Height = 13
        Caption = 'Depth'
      end
      object PanoramaSpeedButton: TSpeedButton
        Left = 51
        Top = 3
        Width = 42
        Height = 42
        Hint = 'Draw tower panorama'
        Glyph.Data = {
          76020000424D7602000000000000760000002800000020000000200000000100
          0400000000000002000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
          8888880800888888088888888088888888880000888088808888888888088888
          8800880888880808888888888808888888088808888880088888888888808888
          8088880888888088888888888880888008888808888888088888088888880808
          8888880888888800088880888888008888888800808888088000880888800888
          8888880888088088888888008808808888888008888080888888888008888808
          8888080888880088888888808888880888808808888880888880088808888880
          8808880888888008888008880888888800888808888880808808808880888888
          8088880888880888080800888088888990888808998808888008800888088889
          9988889999880888880880800808880899988999988088888800808880008088
          8999999888808888880888088880999999999999999998888088880889999999
          999999999999999980888880999999989999999EE09999999088888999988809
          99880999EEE0088999880099988888999880EE999EEEE0889998EE9908888899
          880EEEE99EEEEE080990EE990008888800EEEEEEEEEEEEE0099EEE999EE00000
          0EEEEEEEEEEEEEEE999EEEE999EEEEEEEEEEEEEEEEEEEEE999EEEEEE99999EEE
          EEEEEEEEEEEE99999EEEEEEEE99999999999999999999999EEEEEEEEEEEE9999
          9999999999999EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
        OnClick = PanoramaSpeedButtonClick
      end
      object Pitch: TLabel
        Left = 94
        Top = 61
        Width = 24
        Height = 13
        Caption = 'Pitch'
      end
      object Edit11: TEdit
        Left = 220
        Top = 56
        Width = 65
        Height = 21
        Enabled = False
        TabOrder = 0
      end
      object Edit23: TEdit
        Left = 128
        Top = 58
        Width = 42
        Height = 21
        TabOrder = 1
        OnChange = Edit23Change
      end
      object BitBtn7: TBitBtn
        Left = 3
        Top = 3
        Width = 42
        Height = 42
        Glyph.Data = {
          76020000424D7602000000000000760000002800000020000000200000000100
          0400000000000002000000000000000000001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF555555555555
          5555555555555555555555555555555555555555555555555555555555555555
          5555555555555555555555222222222222222222222222222255552222222222
          2222222222222222225555222222222222222222222222222255552222222222
          2222222222222222225555222222222222222222222222222255552222222222
          2222222222222222225555222222222222222222222222222255552222222222
          222222222222222222555522222222222222222222222222225555222222AAAA
          AAAA2222222222222255552222AAAAAAAAAA22222222AAAAA2A555222AAAAFFF
          FFAAAA2222AAAAAAAA5555AAAAAFFFFFFFFAAAA222AAAFFFFAA555AAAAAFFFFF
          FFFFAAAAAAAAFFFFFFAAAAAFFFFFFFFFFFFFFAAAAAAFFFFFFFFF999999999999
          99999999999999999999FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000FF
          F000FFFF0000FFFFFFFFFFFFF0FFFFFF0FFF0FF0FFFF0FFFFFFFFFFFF0FFFFF0
          FFFFF0FFFFFF0FFFFFFFFFFFF0FFFFF0FFFFF0FFFF00FFFFFFFFFFFFF0FFFFF0
          FFFFF0FF00FFFFFFFFFFFFFFF0FFFFF0FFFFF0F0FFFFFFFFFFFFFFFFF0FFFFFF
          0FFF0FF0FFFF0FFFFFFFFFFFF0FFFFFFF000FFFF0000FFFFFFFF}
        TabOrder = 2
        OnClick = BitBtn7Click
      end
      object BitBtn16: TBitBtn
        Left = 209
        Top = 3
        Width = 42
        Height = 42
        Hint = 'Draw tower viewshed'
        Glyph.Data = {
          4E010000424D4E01000000000000760000002800000014000000120000000100
          040000000000D800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFF0000F0FFFFAAAAAAFFFFFFFF0000FF000000AAAAAAFFFFFF0000FF00
          0000000000AFFFFF0000FF000000000000000FFF0000FFAA0AA00000000000FF
          0000FAAAA0AAAAAAA0000FFF0000AAAAAA0AAAAAA00AAFFF0000AAAAAA0AAAA0
          0AAAAFFF0000AAAAAAA0A00AAAAAAAFF0000AAAAAAAA0AAAAAAAAAFF0000AAAA
          AAAAAAAAAAAAAAFF0000FAAAAAAAAAAAAAAAAFFF0000FAAAAAAAAAAAAAAAAFFF
          0000FFAAAAAAAAAAAAAAFFFF0000FFFAAAAAAAAAAAAFFFFF0000FFFFAAAAAAAA
          AAFFFFFF0000FFFFFFAAAAAAFFFFFFFF0000}
        TabOrder = 3
        OnClick = BitBtn16Click
      end
      object BitBtn20: TBitBtn
        Left = 257
        Top = 3
        Width = 42
        Height = 42
        Hint = 'How masked?'
        Caption = '?'
        Glyph.Data = {
          4E010000424D4E01000000000000760000002800000014000000120000000100
          040000000000D800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFF0000F0FFFFAAAAAAFFFFFFFF0000FF000000AAAAAAFFFFFF0000FF00
          0000000000AFFFFF0000FF000000000000000FFF0000FFAA0AA00000000000FF
          0000FAAAA0AAAAAAA0000FFF0000AAAAAA0AAAAAA00AAFFF0000AAAAAA0AAAA0
          0AAAAFFF0000AAAAAAA0A00AAAAAAAFF0000AAAAAAAA0AAAAAAAAAFF0000AAAA
          AAAAAAAAAAAAAAFF0000FAAAAAAAAAAAAAAAAFFF0000FAAAAAAAAAAAAAAAAFFF
          0000FFAAAAAAAAAAAAAAFFFF0000FFFAAAAAAAAAAAAFFFFF0000FFFFAAAAAAAA
          AAFFFFFF0000FFFFFFAAAAAAFFFFFFFF0000}
        TabOrder = 4
        OnClick = BitBtn20Click
      end
      object BitBtn22: TBitBtn
        Left = 14
        Top = 56
        Width = 57
        Height = 25
        Caption = 'Horizon'
        TabOrder = 5
        OnClick = BitBtn22Click
      end
      object BitBtn30: TBitBtn
        Left = 99
        Top = 3
        Width = 104
        Height = 42
        Hint = 'Draw tower viewshed'
        Caption = 'Composite'
        Glyph.Data = {
          4E010000424D4E01000000000000760000002800000014000000120000000100
          040000000000D800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFF0000F0FFFFAAAAAAFFFFFFFF0000FF000000AAAAAAFFFFFF0000FF00
          0000000000AFFFFF0000FF000000000000000FFF0000FFAA0AA00000000000FF
          0000FAAAA0AAAAAAA0000FFF0000AAAAAA0AAAAAA00AAFFF0000AAAAAA0AAAA0
          0AAAAFFF0000AAAAAAA0A00AAAAAAAFF0000AAAAAAAA0AAAAAAAAAFF0000AAAA
          AAAAAAAAAAAAAAFF0000FAAAAAAAAAAAAAAAAFFF0000FAAAAAAAAAAAAAAAAFFF
          0000FFAAAAAAAAAAAAAAFFFF0000FFFAAAAAAAAAAAAFFFFF0000FFFFAAAAAAAA
          AAFFFFFF0000FFFFFFAAAAAAFFFFFFFF0000}
        TabOrder = 6
        OnClick = BitBtn30Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Accuracy'
      ImageIndex = 2
      object Label6: TLabel
        Left = 24
        Top = 24
        Width = 37
        Height = 13
        Caption = 'Azimuth'
      end
      object Label7: TLabel
        Left = 32
        Top = 48
        Width = 24
        Height = 13
        Caption = 'Pitch'
      end
      object Label8: TLabel
        Left = 80
        Top = 0
        Width = 40
        Height = 13
        Caption = 'Degrees'
      end
      object Edit6: TEdit
        Left = 80
        Top = 16
        Width = 49
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = Edit6Change
      end
      object Edit8: TEdit
        Left = 80
        Top = 40
        Width = 49
        Height = 21
        TabOrder = 1
        Text = '0'
        OnChange = Edit8Change
      end
      object CheckBox2: TCheckBox
        Left = 23
        Top = 96
        Width = 97
        Height = 17
        Caption = 'Plot accuracies'
        TabOrder = 2
        OnClick = CheckBox2Click
      end
      object BitBtn27: TBitBtn
        Left = 11
        Top = 65
        Width = 121
        Height = 25
        Caption = 'Accuracy Symbol'
        TabOrder = 3
        OnClick = BitBtn27Click
      end
      object GroupBox7: TGroupBox
        Left = 184
        Top = 3
        Width = 145
        Height = 88
        Caption = 'Pitch limits'
        TabOrder = 4
        object Label14: TLabel
          Left = 88
          Top = 26
          Width = 47
          Height = 13
          Caption = 'Max Pitch'
        end
        object Label13: TLabel
          Left = 87
          Top = 56
          Width = 44
          Height = 13
          Caption = 'Min Pitch'
        end
        object Edit19: TEdit
          Left = 16
          Top = 50
          Width = 65
          Height = 21
          TabOrder = 0
          OnChange = Edit19Change
        end
        object Edit20: TEdit
          Left = 16
          Top = 23
          Width = 65
          Height = 21
          TabOrder = 1
          OnChange = Edit20Change
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Options'
      ImageIndex = 7
      object BitBtn23: TBitBtn
        Left = 151
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Delete temporary towers'
        TabOrder = 0
        OnClick = BitBtn23Click
      end
      object BitBtn24: TBitBtn
        Left = 3
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Add temporary tower'
        TabOrder = 1
        OnClick = BitBtn24Click
      end
      object BitBtn25: TBitBtn
        Left = 3
        Top = 34
        Width = 75
        Height = 25
        Caption = 'Visible'
        TabOrder = 2
        OnClick = BitBtn25Click
      end
      object BitBtn26: TBitBtn
        Left = 84
        Top = 34
        Width = 75
        Height = 25
        Caption = 'Masked'
        TabOrder = 3
        OnClick = BitBtn26Click
      end
      object RestoreConfigurationButton: TBitBtn
        Left = 3
        Top = 65
        Width = 121
        Height = 25
        Caption = 'Restore configuration'
        TabOrder = 4
        OnClick = RestoreConfigurationButtonClick
      end
      object BitBtn28: TBitBtn
        Left = 165
        Top = 34
        Width = 75
        Height = 25
        Caption = 'Towers'
        TabOrder = 5
        OnClick = BitBtn28Click
      end
      object BitBtn29: TBitBtn
        Left = 246
        Top = 34
        Width = 83
        Height = 25
        Caption = 'Shot results'
        TabOrder = 6
        OnClick = BitBtn29Click
      end
      object BitBtn35: TBitBtn
        Left = 335
        Top = 34
        Width = 66
        Height = 25
        Caption = 'Line'
        TabOrder = 7
        OnClick = BitBtn35Click
      end
      object CheckBox4: TCheckBox
        Left = 352
        Top = 73
        Width = 97
        Height = 17
        Caption = 'Admin'
        TabOrder = 8
        OnClick = CheckBox4Click
      end
      object AdminOptionsButton: TButton
        Left = 130
        Top = 65
        Width = 86
        Height = 25
        Caption = 'Admin options'
        TabOrder = 9
        OnClick = Button1Click
      end
      object Button1: TButton
        Left = 231
        Top = 65
        Width = 75
        Height = 25
        Caption = 'Other options'
        TabOrder = 10
        OnClick = Button1Click
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Lightning'
      ImageIndex = 8
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 420
        Height = 382
        Align = alTop
        TabOrder = 0
        object BitBtn18: TBitBtn
          Left = 155
          Top = 57
          Width = 68
          Height = 25
          Caption = 'Polarity'
          Enabled = False
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
          OnClick = BitBtn18Click
        end
        object BitBtn21: TBitBtn
          Left = 9
          Top = 88
          Width = 68
          Height = 25
          Caption = '+ Strikes'
          TabOrder = 1
          OnClick = BitBtn21Click
        end
        object BitBtn15: TBitBtn
          Left = 83
          Top = 88
          Width = 58
          Height = 25
          Caption = '- Strikes'
          TabOrder = 2
          OnClick = BitBtn15Click
        end
        object BitBtn17: TBitBtn
          Left = 12
          Top = 15
          Width = 75
          Height = 25
          Caption = 'Import file'
          TabOrder = 3
          OnClick = BitBtn17Click
        end
        object BitBtn8: TBitBtn
          Left = 93
          Top = 15
          Width = 75
          Height = 25
          Caption = 'Clear'
          Enabled = False
          TabOrder = 4
          OnClick = BitBtn8Click
        end
        object BitBtn31: TBitBtn
          Left = 229
          Top = 57
          Width = 68
          Height = 25
          Caption = 'Visiblity'
          Enabled = False
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
            FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
            CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
            FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
          TabOrder = 5
          OnClick = BitBtn31Click
        end
        object BitBtn32: TBitBtn
          Left = 153
          Top = 88
          Width = 68
          Height = 25
          Caption = 'Visible'
          TabOrder = 6
          OnClick = BitBtn32Click
        end
        object BitBtn33: TBitBtn
          Left = 227
          Top = 88
          Width = 70
          Height = 25
          Caption = 'Masked'
          TabOrder = 7
          OnClick = BitBtn33Click
        end
        object BitBtn34: TBitBtn
          Left = 184
          Top = 15
          Width = 106
          Height = 25
          Caption = 'View strike location'
          Enabled = False
          TabOrder = 8
          OnClick = BitBtn34Click
        end
        object BitBtn19: TBitBtn
          Left = 296
          Top = 15
          Width = 75
          Height = 25
          Caption = 'Lightning'
          TabOrder = 9
          OnClick = BitBtn19Click
        end
      end
    end
    object Tabsheet6: TTabSheet
      Caption = 'Access'
      ImageIndex = 8
      object GroupBox4: TGroupBox
        Left = 3
        Top = 3
        Width = 129
        Height = 105
        Caption = 'Accessibility map'
        TabOrder = 0
        object Suze: TLabel
          Left = 3
          Top = 43
          Width = 37
          Height = 13
          Caption = 'Size (m)'
        end
        object Label34: TLabel
          Left = 3
          Top = 71
          Width = 52
          Height = 13
          Caption = 'Blowup (%)'
        end
        object GroupBox6: TGroupBox
          Left = 128
          Top = 24
          Width = 57
          Height = 1
          Caption = 'GroupBox6'
          TabOrder = 0
        end
        object CheckBox99: TCheckBox
          Left = 3
          Top = 20
          Width = 137
          Height = 17
          Caption = 'Fire accessiblity map'
          TabOrder = 1
          OnClick = CheckBox99Click
        end
        object Edit25: TEdit
          Left = 64
          Top = 36
          Width = 41
          Height = 21
          TabOrder = 2
          Text = 'Edit25'
          OnChange = Edit25Change
        end
        object Edit26: TEdit
          Left = 61
          Top = 63
          Width = 44
          Height = 21
          TabOrder = 3
          Text = 'Edit26'
          OnChange = Edit26Change
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 412
    Height = 24
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 426
  end
  object Memo1: TMemo
    Left = 2
    Top = 175
    Width = 424
    Height = 335
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Lines.Strings = (
      'Notes:')
    ParentFont = False
    TabOrder = 3
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 128
    Top = 264
  end
  object AdminPopupMenu1: TPopupMenu
    Left = 304
    Top = 240
    object ChangeregionalDEM1: TMenuItem
      Caption = 'Change regional DEM'
      OnClick = ChangeRegionalDEM1Click
    end
    object Changeimagemap1: TMenuItem
      Caption = 'Change image map'
      OnClick = Changeimagemap1Click
    end
    object ChangeTowers1: TMenuItem
      Caption = 'Change Towers'
      OnClick = ChangeTowers1Click
    end
    object Instancestoload1: TMenuItem
      Caption = 'Instances to load'
      OnClick = Instancestoload1Click
    end
    object KMLdelaysec1: TMenuItem
      Caption = 'KML delay (sec)'
      OnClick = KMLdelaysec1Click
    end
    object OpenGoogleEarth1: TMenuItem
      Caption = 'Open GoogleEarth'
      OnClick = OpenGoogleEarth1Click
    end
    object PrepPLSS1: TMenuItem
      Caption = 'Prep PLSS'
      OnClick = PrepPLSS1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 320
    Top = 328
    object DP11: TMenuItem
      Caption = 'DP-1'
      OnClick = DP11Click
    end
    object DP21: TMenuItem
      Caption = 'DP-2'
      OnClick = DP21Click
    end
    object DP31: TMenuItem
      Caption = 'DP-3'
      OnClick = DP31Click
    end
    object DP41: TMenuItem
      Caption = 'DP-4'
      OnClick = DP41Click
    end
    object DP51: TMenuItem
      Caption = 'DP-5'
      OnClick = DP51Click
    end
    object DP61: TMenuItem
      Caption = 'DP-6'
      OnClick = DP61Click
    end
  end
end
