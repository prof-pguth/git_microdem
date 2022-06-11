object PhotoRegForm: TPhotoRegForm
  Left = 766
  Top = 222
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Fire Tower Information'
  ClientHeight = 531
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 414
    Height = 161
    ActivePage = TabSheet4
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Single'
      object Label3: TLabel
        Left = 0
        Top = 64
        Width = 37
        Height = 13
        Caption = 'Azimuth'
      end
      object Label9: TLabel
        Left = 8
        Top = 96
        Width = 24
        Height = 13
        Caption = 'Pitch'
      end
      object Label19: TLabel
        Left = 16
        Top = 80
        Width = 17
        Height = 13
        Caption = '('#176'T)'
      end
      object Label30: TLabel
        Left = 184
        Top = 40
        Width = 74
        Height = 13
        Caption = 'Distance (miles)'
      end
      object GroupBox1: TGroupBox
        Left = 40
        Top = 32
        Width = 129
        Height = 89
        Caption = 'Tower 1'
        TabOrder = 1
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
      object ComboBox1: TComboBox
        Left = 8
        Top = 8
        Width = 145
        Height = 21
        TabOrder = 0
      end
      object BitBtn2: TBitBtn
        Left = 177
        Top = 86
        Width = 57
        Height = 25
        Caption = 'Plot'
        Enabled = False
        TabOrder = 2
        OnClick = BitBtn2Click
      end
      object Edit21: TEdit
        Left = 264
        Top = 40
        Width = 73
        Height = 21
        TabOrder = 4
      end
      object BitBtn4: TBitBtn
        Left = 240
        Top = 86
        Width = 75
        Height = 25
        Caption = 'Bearings'
        TabOrder = 3
        OnClick = BitBtn4Click
      end
      object CheckBox1: TCheckBox
        Left = 224
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Show LOS'
        TabOrder = 5
        OnClick = CheckBox1Click
      end
      object BitBtn5: TBitBtn
        Left = 325
        Top = 86
        Width = 75
        Height = 25
        Caption = 'Time'
        Enabled = False
        TabOrder = 6
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
      object ComboBox2: TComboBox
        Left = 160
        Top = 8
        Width = 129
        Height = 21
        TabOrder = 1
        OnChange = ComboBox2Change
      end
      object GroupBox2: TGroupBox
        Left = 160
        Top = 32
        Width = 105
        Height = 89
        Caption = 'Tower 2'
        Enabled = False
        TabOrder = 3
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
      object ComboBox3: TComboBox
        Left = 0
        Top = 8
        Width = 145
        Height = 21
        TabOrder = 0
        OnChange = ComboBox3Change
      end
      object GroupBox3: TGroupBox
        Left = 48
        Top = 32
        Width = 105
        Height = 89
        Caption = 'Tower 1'
        TabOrder = 2
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
        TabOrder = 4
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
      object Label29: TLabel
        Left = 21
        Top = 75
        Width = 30
        Height = 13
        Caption = 'Tower'
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
        Left = 312
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Plot'
        Enabled = False
        TabOrder = 1
        OnClick = BitBtn13Click
      end
      object ComboBox4: TComboBox
        Left = 64
        Top = 75
        Width = 145
        Height = 21
        TabOrder = 2
        OnChange = ComboBox4Change
      end
      object BitBtn14: TBitBtn
        Left = 15
        Top = 31
        Width = 105
        Height = 25
        Caption = 'Lat/Long (WGS84)'
        TabOrder = 3
        OnClick = BitBtn14Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Maps'
      ImageIndex = 3
      object GroupBox4: TGroupBox
        Left = 0
        Top = 0
        Width = 105
        Height = 133
        Caption = 'Regional Map'
        TabOrder = 0
        object Label10: TLabel
          Left = 7
          Top = 16
          Width = 3
          Height = 13
          Caption = ' '
        end
        object BitBtn9: TBitBtn
          Left = 3
          Top = 20
          Width = 75
          Height = 25
          Caption = 'Redraw map'
          TabOrder = 0
          OnClick = BitBtn9Click
        end
      end
      object GroupBox5: TGroupBox
        Left = 104
        Top = 0
        Width = 273
        Height = 133
        Caption = 'Target map'
        TabOrder = 1
        object Label16: TLabel
          Left = 103
          Top = 59
          Width = 45
          Height = 13
          Caption = 'Long size'
        end
        object Label15: TLabel
          Left = 103
          Top = 13
          Width = 36
          Height = 13
          Caption = 'Lat size'
        end
        object Label33: TLabel
          Left = 143
          Top = 111
          Width = 56
          Height = 13
          Caption = 'Aspect ratio'
        end
        object Edit13: TEdit
          Left = 112
          Top = 78
          Width = 57
          Height = 21
          TabOrder = 0
          Text = '0.0075'
          OnChange = Edit13Change
        end
        object Edit12: TEdit
          Left = 112
          Top = 32
          Width = 57
          Height = 21
          TabOrder = 1
          Text = '0.05'
          OnChange = Edit12Change
        end
        object RadioGroup3: TRadioGroup
          Left = 175
          Top = 14
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
        object Edit24: TEdit
          Left = 216
          Top = 109
          Width = 41
          Height = 21
          TabOrder = 3
          OnChange = Edit24Change
        end
        object RadioGroup2: TRadioGroup
          Left = 7
          Top = 16
          Width = 90
          Height = 58
          Caption = 'Source'
          Items.Strings = (
            'DEM/Tiger'
            'GeoTiff')
          TabOrder = 4
          OnClick = RadioGroup2Click
        end
        object CheckBox3: TCheckBox
          Left = 7
          Top = 86
          Width = 97
          Height = 17
          Caption = 'Terrain shadows'
          TabOrder = 5
          OnClick = CheckBox3Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'View'
      ImageIndex = 1
      object Label5: TLabel
        Left = 4
        Top = 32
        Width = 29
        Height = 13
        Caption = 'HFOV'
      end
      object Label4: TLabel
        Left = 5
        Top = 8
        Width = 28
        Height = 13
        Caption = 'VFOV'
      end
      object Label12: TLabel
        Left = 8
        Top = 64
        Width = 29
        Height = 13
        Caption = 'Depth'
      end
      object PanoramaSpeedButton: TSpeedButton
        Left = 267
        Top = 35
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
        Left = 272
        Top = 93
        Width = 24
        Height = 13
        Caption = 'Pitch'
      end
      object Edit5: TEdit
        Left = 40
        Top = 32
        Width = 65
        Height = 21
        Enabled = False
        TabOrder = 0
        Text = '4'
        OnChange = Edit5Change
      end
      object Edit4: TEdit
        Left = 40
        Top = 8
        Width = 65
        Height = 21
        Enabled = False
        TabOrder = 1
        Text = '3'
        OnChange = Edit4Change
      end
      object Edit11: TEdit
        Left = 40
        Top = 56
        Width = 65
        Height = 21
        Enabled = False
        TabOrder = 2
      end
      object RadioGroup1: TRadioGroup
        Left = 120
        Top = 0
        Width = 97
        Height = 81
        Caption = 'View size'
        Items.Strings = (
          'Zoom In'
          'Medium'
          'Zoom out'
          'Custom')
        TabOrder = 3
        OnClick = RadioGroup1Click
      end
      object ComboBox5: TComboBox
        Left = 223
        Top = 8
        Width = 129
        Height = 21
        TabOrder = 4
        Text = 'ComboBox5'
        OnChange = ComboBox5Change
      end
      object Edit23: TEdit
        Left = 306
        Top = 90
        Width = 42
        Height = 21
        TabOrder = 5
      end
      object BitBtn7: TBitBtn
        Left = 223
        Top = 35
        Width = 38
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
        TabOrder = 6
        OnClick = BitBtn7Click
      end
      object BitBtn16: TBitBtn
        Left = 315
        Top = 35
        Width = 37
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
        TabOrder = 7
        OnClick = BitBtn16Click
      end
      object BitBtn20: TBitBtn
        Left = 358
        Top = 35
        Width = 45
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
        TabOrder = 8
        Visible = False
        OnClick = BitBtn20Click
      end
      object BitBtn22: TBitBtn
        Left = 192
        Top = 88
        Width = 57
        Height = 25
        Caption = 'Horizon'
        TabOrder = 9
        OnClick = BitBtn22Click
      end
      object BitBtn30: TBitBtn
        Left = 19
        Top = 83
        Width = 86
        Height = 30
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
        TabOrder = 10
        OnClick = BitBtn30Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Accuracy'
      ImageIndex = 2
      object Label6: TLabel
        Left = 16
        Top = 32
        Width = 37
        Height = 13
        Caption = 'Azimuth'
      end
      object Label7: TLabel
        Left = 24
        Top = 56
        Width = 24
        Height = 13
        Caption = 'Pitch'
      end
      object Label8: TLabel
        Left = 72
        Top = 8
        Width = 40
        Height = 13
        Caption = 'Degrees'
      end
      object Label13: TLabel
        Left = 232
        Top = 32
        Width = 47
        Height = 13
        Caption = 'Min Ptitch'
      end
      object Label14: TLabel
        Left = 232
        Top = 5
        Width = 47
        Height = 13
        Caption = 'Max Pitch'
      end
      object Edit6: TEdit
        Left = 72
        Top = 24
        Width = 49
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object Edit8: TEdit
        Left = 72
        Top = 48
        Width = 49
        Height = 21
        TabOrder = 1
        Text = '0'
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Plot accuracies'
        TabOrder = 2
        OnClick = CheckBox2Click
      end
      object Edit19: TEdit
        Left = 296
        Top = 32
        Width = 65
        Height = 21
        TabOrder = 3
        OnChange = Edit19Change
      end
      object Edit20: TEdit
        Left = 296
        Top = 5
        Width = 65
        Height = 21
        TabOrder = 4
        OnChange = Edit20Change
      end
      object BitBtn27: TBitBtn
        Left = 232
        Top = 88
        Width = 121
        Height = 25
        Caption = 'Accuracy Symbol'
        TabOrder = 5
        OnClick = BitBtn27Click
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Test'
      ImageIndex = 5
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 193
        Height = 120
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object SetSightButton: TBitBtn
        Left = 216
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Set sighting'
        TabOrder = 1
      end
      object BitBtn6: TBitBtn
        Left = 216
        Top = 56
        Width = 75
        Height = 25
        Caption = 'New DEM'
        TabOrder = 2
        OnClick = BitBtn6Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Options'
      ImageIndex = 7
      object Suze: TLabel
        Left = 198
        Top = 26
        Width = 37
        Height = 13
        Caption = 'Size (m)'
      end
      object Label34: TLabel
        Left = 198
        Top = 46
        Width = 52
        Height = 13
        Caption = 'Blowup (%)'
      end
      object BitBtn23: TBitBtn
        Left = 16
        Top = 34
        Width = 129
        Height = 25
        Caption = 'Delete temporary towers'
        TabOrder = 0
        OnClick = BitBtn23Click
      end
      object BitBtn24: TBitBtn
        Left = 16
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Add temporary tower'
        TabOrder = 1
        OnClick = BitBtn24Click
      end
      object BitBtn25: TBitBtn
        Left = 3
        Top = 73
        Width = 75
        Height = 25
        Caption = 'Visible'
        TabOrder = 2
        OnClick = BitBtn25Click
      end
      object BitBtn26: TBitBtn
        Left = 84
        Top = 73
        Width = 75
        Height = 25
        Caption = 'Masked'
        TabOrder = 3
        OnClick = BitBtn26Click
      end
      object Resotr: TBitBtn
        Left = 3
        Top = 104
        Width = 121
        Height = 25
        Caption = 'Restore configuration'
        TabOrder = 4
      end
      object BitBtn28: TBitBtn
        Left = 165
        Top = 73
        Width = 75
        Height = 25
        Caption = 'Towers'
        TabOrder = 5
        OnClick = BitBtn28Click
      end
      object BitBtn29: TBitBtn
        Left = 246
        Top = 73
        Width = 94
        Height = 25
        Caption = 'Shot results'
        TabOrder = 6
        OnClick = BitBtn29Click
      end
      object CheckBox99: TCheckBox
        Left = 200
        Top = 3
        Width = 137
        Height = 17
        Caption = 'Fire accessiblity map'
        TabOrder = 7
        OnClick = CheckBox99Click
      end
      object Edit25: TEdit
        Left = 256
        Top = 19
        Width = 65
        Height = 21
        TabOrder = 8
        Text = 'Edit25'
        OnChange = Edit25Change
      end
      object Edit26: TEdit
        Left = 256
        Top = 46
        Width = 65
        Height = 21
        TabOrder = 9
        Text = 'Edit26'
        OnChange = Edit26Change
      end
      object CheckBox10: TCheckBox
        Left = 264
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Show UTM'
        TabOrder = 10
        OnClick = CheckBox10Click
      end
      object BitBtn35: TBitBtn
        Left = 343
        Top = 73
        Width = 60
        Height = 25
        Caption = 'Line'
        TabOrder = 11
        OnClick = BitBtn35Click
      end
    end
    object TabSheet9: TTabSheet
      Caption = 'Lightning'
      ImageIndex = 8
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 406
        Height = 382
        Align = alTop
        TabOrder = 0
        object SpeedButton1: TSpeedButton
          Left = 85
          Top = 63
          Width = 50
          Height = 25
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Glyph.Data = {
            66010000424D6601000000000000760000002800000014000000140000000100
            040000000000F000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFF0000F888888888999988888F0000F88888888B999988888F0000F888
            88888BB99888888F0000F8888888BB899888888F0000F888888BB8880088888F
            0000F88888BB88800088888F0000F8888BB888000008888F0000F888BB888800
            0008888F0000F88BB88888800008888F0000F88BBB8888800008888F0000F888
            8BB888880000888F0000F88888BB88880088888F0000F888888BB8888888888F
            0000F888888BB8888888888F0000F88888BB88888888888F0000F8888BB88888
            8888888F0000F888BB8888888888888F0000F88BB88888888888888F0000FFFB
            BFFFFFFFFFFFFFFF0000}
          ParentFont = False
          OnClick = SpeedButton1Click
        end
        object BitBtn18: TBitBtn
          Left = 149
          Top = 1
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
        object ComboBox6: TComboBox
          Left = 6
          Top = 5
          Width = 129
          Height = 21
          Enabled = False
          TabOrder = 1
          OnChange = ComboBox6Change
        end
        object BitBtn21: TBitBtn
          Left = 3
          Top = 32
          Width = 68
          Height = 25
          Caption = '+ Strikes'
          TabOrder = 2
          OnClick = BitBtn21Click
        end
        object BitBtn15: TBitBtn
          Left = 77
          Top = 32
          Width = 58
          Height = 25
          Caption = '- Strikes'
          TabOrder = 3
          OnClick = BitBtn15Click
        end
        object BitBtn19: TBitBtn
          Left = 4
          Top = 94
          Width = 75
          Height = 25
          Caption = 'Enter strike'
          Enabled = False
          TabOrder = 4
          OnClick = BitBtn19Click
        end
        object BitBtn17: TBitBtn
          Left = 4
          Top = 63
          Width = 75
          Height = 25
          Caption = 'Import file'
          TabOrder = 5
          OnClick = BitBtn17Click
        end
        object BitBtn8: TBitBtn
          Left = 85
          Top = 94
          Width = 75
          Height = 25
          Caption = 'Clear'
          Enabled = False
          TabOrder = 6
          OnClick = BitBtn8Click
        end
        object BitBtn31: TBitBtn
          Left = 223
          Top = 3
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
          TabOrder = 7
          OnClick = BitBtn31Click
        end
        object BitBtn32: TBitBtn
          Left = 147
          Top = 32
          Width = 68
          Height = 25
          Caption = 'Visible'
          TabOrder = 8
          OnClick = BitBtn32Click
        end
        object BitBtn33: TBitBtn
          Left = 221
          Top = 32
          Width = 61
          Height = 25
          Caption = 'Masked'
          TabOrder = 9
          OnClick = BitBtn33Click
        end
      end
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 161
    Width = 414
    Height = 340
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 501
    Width = 414
    Height = 30
    Align = alBottom
    TabOrder = 2
    object Label28: TLabel
      Left = 144
      Top = 8
      Width = 48
      Height = 13
      Caption = 'Incident #'
    end
    object BitBtn10: TBitBtn
      Left = 319
      Top = 3
      Width = 91
      Height = 25
      Caption = 'Close DragonPlot'
      TabOrder = 0
      OnClick = BitBtn10Click
    end
    object BitBtn3: TBitBtn
      Left = 8
      Top = 3
      Width = 89
      Height = 25
      Caption = 'Review archive'
      TabOrder = 1
      OnClick = BitBtn3Click
    end
    object BitBtn11: TBitBtn
      Left = 103
      Top = 3
      Width = 33
      Height = 25
      Caption = 'KML'
      Enabled = False
      TabOrder = 2
      OnClick = BitBtn11Click
    end
    object Edit27: TEdit
      Left = 198
      Top = 3
      Width = 47
      Height = 21
      TabOrder = 3
      OnChange = Edit27Change
    end
    object BitBtn1: TBitBtn
      Left = 251
      Top = 3
      Width = 62
      Height = 25
      Caption = 'Save'
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
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
      TabOrder = 4
      OnClick = BitBtn1Click
    end
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
end
