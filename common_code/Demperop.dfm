object PerspOptions: TPerspOptions
  Left = 597
  Top = 152
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Perspective Options'
  ClientHeight = 561
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 8
    Top = 504
    Width = 77
    Height = 27
    Caption = 'Ok'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 91
    Top = 504
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 174
    Top = 504
    Width = 77
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
  object Button2: TButton
    Left = 160
    Top = 464
    Width = 89
    Height = 25
    Caption = 'Reset defaults'
    TabOrder = 3
    OnClick = Button2Click
  end
  object CheckBox20: TCheckBox
    Left = 288
    Top = 512
    Width = 177
    Height = 17
    Caption = 'Save as program defaults'
    TabOrder = 4
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 464
    Width = 137
    Height = 21
    TabOrder = 5
    Text = ' '
    OnChange = ComboBox1Change
    Items.Strings = (
      'Fishnet (regular)'
      'Fishnet (Chromadepth)'
      'Reflectance'
      'Draped'
      'None (ridges only)')
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 449
    ActivePage = TabSheet7
    TabOrder = 6
    object TabSheet1: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 20
        Top = 20
        Width = 169
        Height = 13
        Caption = 'Distance between profiles (m)'
      end
      object Label8: TLabel
        Left = 24
        Top = 96
        Width = 152
        Height = 13
        Caption = 'Point spacing on radial (m)'
      end
      object Label3: TLabel
        Left = 20
        Top = 72
        Width = 151
        Height = 13
        Caption = 'Distance to first profile (m)'
      end
      object Label2: TLabel
        Left = 20
        Top = 46
        Width = 104
        Height = 13
        Caption = 'Mesh spacing (m):'
      end
      object Edit2: TEdit
        Left = 202
        Top = 46
        Width = 98
        Height = 21
        TabOrder = 0
        Text = ' '
      end
      object Edit1: TEdit
        Left = 202
        Top = 20
        Width = 98
        Height = 21
        TabOrder = 1
        Text = ' '
      end
      object Edit3: TEdit
        Left = 202
        Top = 72
        Width = 98
        Height = 21
        TabOrder = 2
        Text = ' '
      end
      object Edit8: TEdit
        Left = 202
        Top = 96
        Width = 98
        Height = 21
        TabOrder = 3
        Text = ' '
      end
      object ComboBox2: TComboBox
        Left = 312
        Top = 40
        Width = 137
        Height = 21
        TabOrder = 4
        Text = ' '
        OnChange = ComboBox2Change
        Items.Strings = (
          'No curvature'
          'TM5-441 curvature'
          'Radio Line Of Sight'
          'Yoeli curvature')
      end
      object Button1: TButton
        Left = 260
        Top = 225
        Width = 89
        Height = 25
        Caption = 'Reflectance'
        TabOrder = 5
        OnClick = Button1Click
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 296
        Width = 97
        Height = 17
        Caption = 'Show views '
        TabOrder = 6
      end
      object CheckBox5: TCheckBox
        Left = 16
        Top = 360
        Width = 137
        Height = 17
        Caption = 'Flying danger colors'
        TabOrder = 7
      end
      object CheckBox16: TCheckBox
        Left = 16
        Top = 376
        Width = 163
        Height = 17
        Caption = 'Viewshed fan on map'
        TabOrder = 8
      end
      object GroupBox1: TGroupBox
        Left = 12
        Top = 217
        Width = 225
        Height = 73
        Caption = 'Ridge crests'
        TabOrder = 9
        object Label14: TLabel
          Left = 15
          Top = 39
          Width = 83
          Height = 13
          Caption = 'Separation (m)'
        end
        object BitBtn1: TBitBtn
          Left = 110
          Top = 8
          Width = 35
          Height = 25
          TabOrder = 0
          OnClick = BitBtn1Click
        end
        object CheckBox6: TCheckBox
          Left = 16
          Top = 16
          Width = 81
          Height = 17
          Caption = 'Outline'
          TabOrder = 1
          OnClick = CheckBox6Click
        end
        object Edit14: TEdit
          Left = 104
          Top = 39
          Width = 81
          Height = 21
          TabOrder = 2
        end
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 120
        Width = 353
        Height = 91
        Caption = 'Sky && Clouds'
        TabOrder = 10
        object Label24: TLabel
          Left = 233
          Top = 12
          Width = 5
          Height = 13
        end
        object M: TLabel
          Left = 24
          Top = 72
          Width = 109
          Height = 13
          Caption = 'Min sky to show ('#176')'
        end
        object CheckBox52: TCheckBox
          Left = 16
          Top = 40
          Width = 153
          Height = 17
          Caption = 'Custom perspective sky'
          TabOrder = 0
          OnClick = CheckBox52Click
        end
        object CheckBox18: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Clouds'
          TabOrder = 1
          OnClick = CheckBox18Click
        end
        object BitBtn2: TBitBtn
          Left = 176
          Top = 8
          Width = 49
          Height = 25
          Caption = 'File'
          TabOrder = 2
          OnClick = BitBtn2Click
        end
        object BitBtn4: TBitBtn
          Left = 175
          Top = 32
          Width = 50
          Height = 25
          TabOrder = 3
          OnClick = BitBtn4Click
        end
        object Edit35: TEdit
          Left = 156
          Top = 63
          Width = 69
          Height = 21
          TabOrder = 4
        end
      end
      object BitBtn5: TBitBtn
        Left = 355
        Top = 225
        Width = 75
        Height = 25
        Caption = 'Viewshed'
        TabOrder = 11
        OnClick = BitBtn5Click
      end
      object CheckBox21: TCheckBox
        Left = 16
        Top = 399
        Width = 162
        Height = 17
        Caption = 'Show location sensitivity'
        TabOrder = 12
      end
      object BitBtn12: TBitBtn
        Left = -640
        Top = 928
        Width = 75
        Height = 25
        Caption = 'BitBtn12'
        TabOrder = 13
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 312
        Width = 97
        Height = 17
        Caption = 'VE allowed'
        TabOrder = 14
        OnClick = CheckBox2Click
      end
      object GroupBox5: TGroupBox
        Left = 192
        Top = 292
        Width = 185
        Height = 126
        Caption = 'Viewport labels'
        TabOrder = 15
        object Label40: TLabel
          Left = 16
          Top = 82
          Width = 82
          Height = 13
          Caption = 'HFOV spacing'
        end
        object Label41: TLabel
          Left = 16
          Top = 101
          Width = 81
          Height = 13
          Caption = 'VFOV spacing'
        end
        object BitBtn13: TBitBtn
          Left = 14
          Top = 51
          Width = 75
          Height = 25
          Caption = 'Font'
          TabOrder = 0
          OnClick = BitBtn13Click
        end
        object CheckBox1: TCheckBox
          Left = 16
          Top = 13
          Width = 113
          Height = 17
          Caption = 'Label viewport'
          TabOrder = 1
        end
        object CheckBox4: TCheckBox
          Left = 16
          Top = 28
          Width = 97
          Height = 17
          Caption = 'Title viewport'
          TabOrder = 2
        end
        object Edit46: TEdit
          Left = 104
          Top = 80
          Width = 41
          Height = 21
          TabOrder = 3
          Text = 'Edit46'
        end
        object Edit47: TEdit
          Left = 104
          Top = 102
          Width = 42
          Height = 21
          TabOrder = 4
          Text = 'Edit47'
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Viewport'
      ImageIndex = 1
      object Label15: TLabel
        Left = 96
        Top = 54
        Width = 47
        Height = 13
        Caption = 'Pitch ('#176')'
      end
      object Label17: TLabel
        Left = 96
        Top = 84
        Width = 62
        Height = 13
        Caption = 'Azimuth ('#176')'
      end
      object Label19: TLabel
        Left = 45
        Top = 272
        Width = 121
        Height = 13
        Caption = 'Vertical exaggeration'
        Enabled = False
      end
      object Label20: TLabel
        Left = 77
        Top = 168
        Width = 79
        Height = 13
        Caption = 'Width (Pixels)'
      end
      object Label21: TLabel
        Left = 77
        Top = 192
        Width = 83
        Height = 13
        Caption = 'Height (Pixels)'
      end
      object Label6: TLabel
        Left = 18
        Top = 211
        Width = 148
        Height = 13
        Caption = 'Horizontal field of view ('#176')'
      end
      object Label18: TLabel
        Left = 26
        Top = 244
        Width = 134
        Height = 13
        Caption = 'Vertical field of view ('#176')'
      end
      object Label27: TLabel
        Left = 173
        Top = 144
        Width = 68
        Height = 13
        Caption = 'Perspective'
      end
      object Label28: TLabel
        Left = 269
        Top = 144
        Width = 57
        Height = 13
        Caption = 'Panorama'
      end
      object Label7: TLabel
        Left = 3
        Top = 307
        Width = 155
        Height = 13
        Caption = 'Observer above ground (m)'
      end
      object Label10: TLabel
        Left = 49
        Top = 111
        Width = 111
        Height = 13
        Caption = 'Viewport depth (m) '
      end
      object Label9: TLabel
        Left = 6
        Top = 27
        Width = 157
        Height = 13
        Caption = 'Observer Elevation (m ASL)'
      end
      object Label34: TLabel
        Left = 357
        Top = 144
        Width = 34
        Height = 13
        Caption = 'Flying'
      end
      object Senso: TLabel
        Left = 296
        Top = 54
        Width = 72
        Height = 13
        Caption = 'Sensor pitch'
      end
      object Label38: TLabel
        Left = 344
        Top = 78
        Width = 24
        Height = 13
        Caption = 'Max'
      end
      object Label39: TLabel
        Left = 344
        Top = 102
        Width = 22
        Height = 13
        Caption = 'MIn'
      end
      object Edit15: TEdit
        Left = 184
        Top = 54
        Width = 60
        Height = 21
        TabOrder = 0
        OnChange = Edit15Change
      end
      object Edit17: TEdit
        Left = 184
        Top = 81
        Width = 60
        Height = 21
        TabOrder = 1
        OnChange = Edit17Change
      end
      object Edit19: TEdit
        Left = 181
        Top = 272
        Width = 49
        Height = 21
        Enabled = False
        TabOrder = 2
      end
      object Edit20: TEdit
        Left = 181
        Top = 160
        Width = 49
        Height = 21
        TabOrder = 3
        OnChange = Edit20Change
      end
      object Edit21: TEdit
        Left = 181
        Top = 184
        Width = 49
        Height = 21
        TabOrder = 4
        OnChange = Edit21Change
      end
      object Edit6: TEdit
        Left = 181
        Top = 214
        Width = 49
        Height = 21
        TabOrder = 5
        Text = ' '
        OnChange = Edit6Change
      end
      object Edit13: TEdit
        Left = 181
        Top = 241
        Width = 49
        Height = 21
        TabOrder = 6
        OnChange = Edit13Change
      end
      object Edit26: TEdit
        Left = 269
        Top = 184
        Width = 49
        Height = 21
        TabOrder = 7
        OnChange = Edit21Change
      end
      object Edit27: TEdit
        Left = 269
        Top = 160
        Width = 49
        Height = 21
        TabOrder = 8
        OnChange = Edit20Change
      end
      object Edit28: TEdit
        Left = 269
        Top = 216
        Width = 49
        Height = 21
        TabOrder = 9
        OnChange = Edit28Change
      end
      object Edit29: TEdit
        Left = 269
        Top = 270
        Width = 49
        Height = 21
        Enabled = False
        TabOrder = 10
      end
      object Edit12: TEdit
        Left = 185
        Top = 108
        Width = 59
        Height = 21
        TabOrder = 11
        Text = ' '
        OnChange = Edit12Change
      end
      object Edit7: TEdit
        Left = 200
        Top = 299
        Width = 98
        Height = 21
        TabOrder = 12
        Text = ' '
        OnChange = Edit7Change
      end
      object Edit9: TEdit
        Left = 184
        Top = 27
        Width = 97
        Height = 21
        TabOrder = 13
        Text = ' '
        OnChange = Edit9Change
      end
      object RadioGroup1: TRadioGroup
        Left = 32
        Top = 360
        Width = 161
        Height = 49
        Caption = 'Observer elevation'
        Items.Strings = (
          'Nap of the earth'
          'Constant elevation')
        TabOrder = 14
        OnClick = RadioGroup1Click
      end
      object Edit25: TEdit
        Left = 349
        Top = 184
        Width = 42
        Height = 21
        TabOrder = 15
        OnChange = Edit25Change
      end
      object Edit24: TEdit
        Left = 349
        Top = 157
        Width = 42
        Height = 21
        TabOrder = 16
        OnChange = Edit24Change
      end
      object Edit30: TEdit
        Left = 349
        Top = 272
        Width = 42
        Height = 21
        Enabled = False
        TabOrder = 17
      end
      object Edit37: TEdit
        Left = 269
        Top = 243
        Width = 49
        Height = 21
        TabOrder = 18
        OnChange = Edit37Change
      end
      object Edit36: TEdit
        Left = 349
        Top = 211
        Width = 42
        Height = 21
        TabOrder = 19
        OnChange = Edit25Change
      end
      object Edit38: TEdit
        Left = 349
        Top = 238
        Width = 42
        Height = 21
        TabOrder = 20
        OnChange = Edit25Change
      end
      object Edit43: TEdit
        Left = 374
        Top = 99
        Width = 63
        Height = 21
        Enabled = False
        TabOrder = 21
      end
      object Edit44: TEdit
        Left = 374
        Top = 72
        Width = 63
        Height = 21
        Enabled = False
        TabOrder = 22
      end
      object Edit45: TEdit
        Left = 349
        Top = 299
        Width = 42
        Height = 21
        TabOrder = 23
        Text = 'Edit45'
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Viewer'
      ImageIndex = 6
      object Label31: TLabel
        Left = 32
        Top = 24
        Width = 46
        Height = 13
        Caption = 'Label31'
      end
      object Panel1: TPanel
        Left = 12
        Top = 56
        Width = 185
        Height = 105
        TabOrder = 0
        object Label32: TLabel
          Left = 16
          Top = 8
          Width = 52
          Height = 13
          Caption = 'Direction'
        end
        object Label33: TLabel
          Left = 16
          Top = 40
          Width = 72
          Height = 13
          Caption = 'Distance (m)'
        end
        object Edit33: TEdit
          Left = 104
          Top = 8
          Width = 65
          Height = 21
          TabOrder = 0
          OnChange = Edit33Change
        end
        object Edit34: TEdit
          Left = 104
          Top = 32
          Width = 65
          Height = 21
          TabOrder = 1
          OnChange = Edit34Change
        end
        object BitBtn6: TBitBtn
          Left = 80
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Move'
          Enabled = False
          TabOrder = 2
          OnClick = BitBtn6Click
        end
      end
      object BitBtn7: TBitBtn
        Left = 24
        Top = 184
        Width = 161
        Height = 25
        Caption = 'Enter new location'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          5555555555FFFFF555555555544C4C5555555555F777775FF5555554C444C444
          5555555775FF55775F55554C4334444445555575577F55557FF554C4C334C4C4
          335557F5577FF55577F554CCC3334444335557555777F555775FCCCCC333CCC4
          C4457F55F777F555557F4CC33333CCC444C57F577777F5F5557FC4333333C3C4
          CCC57F777777F7FF557F4CC33333333C4C457F577777777F557FCCC33CC4333C
          C4C575F7755F777FF5755CCCCC3333334C5557F5FF777777F7F554C333333333
          CC55575777777777F755553333CC3C33C555557777557577755555533CC4C4CC
          5555555775FFFF77555555555C4CCC5555555555577777555555}
        NumGlyphs = 2
        TabOrder = 1
        OnClick = BitBtn7Click
      end
      object BitBtn10: TBitBtn
        Left = 24
        Top = 215
        Width = 161
        Height = 25
        Caption = 'Redraw view outline'
        TabOrder = 2
        OnClick = BitBtn10Click
      end
      object CheckBox19: TCheckBox
        Left = 87
        Top = 246
        Width = 145
        Height = 17
        Caption = 'Redraw map first'
        TabOrder = 3
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Drape'
      ImageIndex = 2
      object Label12: TLabel
        Left = 192
        Top = 24
        Width = 101
        Height = 13
        Caption = 'Shift view (pixels)'
      end
      object Label13: TLabel
        Left = 192
        Top = 40
        Width = 112
        Height = 13
        Caption = 'View separation (m)'
      end
      object Label26: TLabel
        Left = 15
        Top = 251
        Width = 150
        Height = 13
        Caption = 'Max size drapes, y (pixels)'
      end
      object Label25: TLabel
        Left = 15
        Top = 224
        Width = 150
        Height = 13
        Caption = 'Max size drapes, x (pixels)'
      end
      object Edit11: TEdit
        Left = 312
        Top = 40
        Width = 57
        Height = 21
        TabOrder = 0
        Text = ' '
      end
      object Edit10: TEdit
        Left = 312
        Top = 16
        Width = 57
        Height = 21
        TabOrder = 1
        Text = ' '
      end
      object RadioGroup2: TRadioGroup
        Left = 3
        Top = 3
        Width = 161
        Height = 105
        Caption = 'Stereo Mode'
        Items.Strings = (
          'No stereo'
          'Anaglyph, displace'
          'Anaglyph, two scenes'
          'Stereo pair')
        TabOrder = 2
        OnClick = RadioGroup2Click
      end
      object checkBox99: TCheckBox
        Left = 208
        Top = 67
        Width = 141
        Height = 17
        Caption = 'Converging views'
        TabOrder = 3
      end
      object Edit31: TEdit
        Left = 183
        Top = 221
        Width = 121
        Height = 21
        TabOrder = 4
        Text = ' '
      end
      object Edit32: TEdit
        Left = 183
        Top = 248
        Width = 121
        Height = 21
        TabOrder = 5
        Text = ' '
      end
      object CheckBox10: TCheckBox
        Left = 7
        Top = 160
        Width = 209
        Height = 17
        Caption = 'Drape map without redrawing'
        TabOrder = 6
      end
      object CheckBox9: TCheckBox
        Left = 7
        Top = 128
        Width = 129
        Height = 17
        Caption = 'Show grid on drapes'
        TabOrder = 7
      end
      object CheckBox8: TCheckBox
        Left = 7
        Top = 144
        Width = 177
        Height = 17
        Caption = 'Show overlays on drapes'
        TabOrder = 8
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Flight'
      ImageIndex = 4
      object Label11: TLabel
        Left = 24
        Top = 88
        Width = 123
        Height = 13
        Caption = 'Movie name (4 chars)'
      end
      object Label16: TLabel
        Left = 24
        Top = 70
        Width = 119
        Height = 13
        Caption = 'Frame separation (m)'
      end
      object Label22: TLabel
        Left = 64
        Top = 256
        Width = 32
        Height = 13
        Caption = 'FOV1'
      end
      object Label23: TLabel
        Left = 64
        Top = 280
        Width = 32
        Height = 13
        Caption = 'FOV2'
      end
      object Edit16: TEdit
        Left = 160
        Top = 64
        Width = 73
        Height = 21
        TabOrder = 0
      end
      object Edit18: TEdit
        Left = 160
        Top = 88
        Width = 73
        Height = 21
        TabOrder = 1
      end
      object CheckBox11: TCheckBox
        Left = 24
        Top = 120
        Width = 137
        Height = 17
        Caption = 'Show flight map'
        TabOrder = 2
      end
      object CheckBox12: TCheckBox
        Left = 24
        Top = 136
        Width = 161
        Height = 17
        Caption = 'Side by side windows'
        TabOrder = 3
      end
      object CheckBox13: TCheckBox
        Left = 24
        Top = 152
        Width = 185
        Height = 17
        Caption = 'Default new movie names'
        TabOrder = 4
      end
      object CheckBox14: TCheckBox
        Left = 24
        Top = 216
        Width = 129
        Height = 17
        Caption = 'Dual fields of view'
        TabOrder = 5
      end
      object CheckBox15: TCheckBox
        Left = 24
        Top = 232
        Width = 113
        Height = 17
        Caption = 'Dual drape maps'
        TabOrder = 6
      end
      object Edit22: TEdit
        Left = 104
        Top = 256
        Width = 41
        Height = 21
        TabOrder = 7
      end
      object Edit23: TEdit
        Left = 104
        Top = 280
        Width = 41
        Height = 21
        TabOrder = 8
      end
      object BitBtn11: TBitBtn
        Left = 32
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Route'
        TabOrder = 9
        OnClick = BitBtn11Click
      end
      object GroupBox3: TGroupBox
        Left = 215
        Top = 152
        Width = 213
        Height = 101
        Caption = 'Cross track profiles'
        TabOrder = 10
        object Label4: TLabel
          Left = 24
          Top = 40
          Width = 59
          Height = 13
          Caption = 'Height (m)'
        end
        object Label5: TLabel
          Left = 24
          Top = 64
          Width = 77
          Height = 13
          Caption = 'Side view (m)'
        end
        object Edit4: TEdit
          Left = 120
          Top = 39
          Width = 76
          Height = 21
          TabOrder = 0
        end
        object Edit5: TEdit
          Left = 120
          Top = 67
          Width = 77
          Height = 21
          TabOrder = 1
        end
        object CheckBox7: TCheckBox
          Left = 17
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Show'
          TabOrder = 2
          OnClick = CheckBox7Click
        end
      end
      object GroupBox4: TGroupBox
        Left = 215
        Top = 280
        Width = 213
        Height = 101
        Caption = 'Along track profiles'
        TabOrder = 11
        object Label30: TLabel
          Left = 24
          Top = 40
          Width = 89
          Height = 13
          Caption = 'Number profiles'
        end
        object Label35: TLabel
          Left = 24
          Top = 64
          Width = 121
          Height = 13
          Caption = 'Profile separation (m)'
        end
        object Edit39: TEdit
          Left = 151
          Top = 37
          Width = 42
          Height = 21
          TabOrder = 0
        end
        object Edit40: TEdit
          Left = 151
          Top = 64
          Width = 45
          Height = 21
          TabOrder = 1
        end
        object CheckBox22: TCheckBox
          Left = 26
          Top = 17
          Width = 97
          Height = 17
          Caption = 'Show'
          TabOrder = 2
          OnClick = CheckBox22Click
        end
      end
      object CheckBox27: TCheckBox
        Left = 24
        Top = 176
        Width = 161
        Height = 17
        Caption = 'Live flying movie'
        TabOrder = 12
      end
      object CheckBox28: TCheckBox
        Left = 24
        Top = 193
        Width = 161
        Height = 17
        Caption = 'Save live flying route'
        TabOrder = 13
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Image'
      ImageIndex = 5
      object Label29: TLabel
        Left = 24
        Top = 336
        Width = 373
        Height = 20
        Caption = 'Options on this tab experimental--use carefully'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object CheckBox17: TCheckBox
        Left = 16
        Top = 24
        Width = 209
        Height = 17
        Caption = 'Vary radial spacing with range'
        TabOrder = 0
      end
      object StringGrid1: TStringGrid
        Left = 32
        Top = 64
        Width = 145
        Height = 161
        ColCount = 2
        FixedCols = 0
        RowCount = 6
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 1
        ColWidths = (
          64
          64)
        RowHeights = (
          24
          24
          24
          24
          24
          24)
      end
      object BitBtn3: TBitBtn
        Left = 24
        Top = 256
        Width = 89
        Height = 25
        Caption = 'ChangeDEM'
        TabOrder = 2
        OnClick = BitBtn3Click
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Gazetteer'
      ImageIndex = 7
      object Label36: TLabel
        Left = 56
        Top = 80
        Width = 104
        Height = 13
        Caption = 'Shift distance  (m)'
      end
      object Label37: TLabel
        Left = 24
        Top = 128
        Width = 133
        Height = 13
        Caption = 'Observer on peaks (m) '
      end
      object CheckBox23: TCheckBox
        Left = 24
        Top = 16
        Width = 289
        Height = 17
        Caption = 'Automatically label perspective and panorama'
        TabOrder = 0
      end
      object CheckBox24: TCheckBox
        Left = 24
        Top = 48
        Width = 209
        Height = 17
        Caption = 'Shift gazetteer peaks'
        TabOrder = 1
      end
      object Edit41: TEdit
        Left = 184
        Top = 77
        Width = 61
        Height = 21
        TabOrder = 2
        Text = 'Edit41'
      end
      object Edit42: TEdit
        Left = 180
        Top = 125
        Width = 65
        Height = 21
        TabOrder = 3
        Text = 'Edit42'
      end
      object CheckBox25: TCheckBox
        Left = 16
        Top = 176
        Width = 217
        Height = 17
        Caption = 'Mark peaks on perspective'
        TabOrder = 4
      end
      object CheckBox26: TCheckBox
        Left = 16
        Top = 208
        Width = 149
        Height = 17
        Caption = 'Mark peaks on map'
        TabOrder = 5
      end
    end
  end
  object BitBtn8: TBitBtn
    Left = 272
    Top = 464
    Width = 81
    Height = 25
    Caption = 'Save to file'
    TabOrder = 7
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 376
    Top = 462
    Width = 89
    Height = 25
    Caption = 'Read from file'
    TabOrder = 8
    OnClick = BitBtn9Click
  end
end
