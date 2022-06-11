object BlockOpsForm: TBlockOpsForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Geomorph Block Settings'
  ClientHeight = 549
  ClientWidth = 993
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
  object Label11: TLabel
    Left = 16
    Top = 23
    Width = 42
    Height = 13
    Caption = 'Sampling'
  end
  object Label15: TLabel
    Left = 360
    Top = 232
    Width = 37
    Height = 13
    Caption = 'Label15'
  end
  object Label5: TLabel
    Left = 14
    Top = 397
    Width = 89
    Height = 13
    Caption = 'Other box size (m)'
  end
  object HelpBtn: TBitBtn
    Left = 108
    Top = 516
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 516
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
  object CheckBox9: TCheckBox
    Left = 16
    Top = 0
    Width = 97
    Height = 17
    Caption = 'Entire DEM'
    TabOrder = 2
    OnClick = CheckBox9Click
  end
  object Edit7: TEdit
    Left = 64
    Top = 23
    Width = 65
    Height = 21
    TabOrder = 3
  end
  object PageControl1: TPageControl
    Left = 200
    Top = -2
    Width = 697
    Height = 455
    ActivePage = tabsheet2
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'Create database'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 155
        Height = 257
        Caption = 'DB Parameter groups'
        Color = clInfoBk
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        object CheckBox1: TCheckBox
          Left = 8
          Top = 63
          Width = 97
          Height = 17
          Caption = 'Slope'
          TabOrder = 0
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 86
          Width = 97
          Height = 17
          Caption = 'Fabric'
          TabOrder = 1
        end
        object CheckBox3: TCheckBox
          Left = 8
          Top = 109
          Width = 97
          Height = 17
          Caption = 'Profile curvature'
          TabOrder = 2
        end
        object CheckBox4: TCheckBox
          Left = 8
          Top = 132
          Width = 97
          Height = 17
          Caption = 'Plan curvature'
          TabOrder = 3
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 155
          Width = 97
          Height = 17
          Caption = 'Gamma'
          TabOrder = 4
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 178
          Width = 97
          Height = 17
          Caption = 'Fractal'
          TabOrder = 5
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 218
          Width = 136
          Height = 17
          Caption = 'Wavelength/height'
          Enabled = False
          TabOrder = 6
        end
        object CheckBox10: TCheckBox
          Left = 8
          Top = 17
          Width = 97
          Height = 17
          Caption = 'Basic elevation'
          TabOrder = 7
        end
        object CheckBox11: TCheckBox
          Left = 8
          Top = 40
          Width = 121
          Height = 17
          Caption = 'Advanced elevation'
          TabOrder = 8
        end
        object CheckBox12: TCheckBox
          Left = 8
          Top = 236
          Width = 113
          Height = 17
          Caption = 'Missing and holes'
          TabOrder = 9
        end
        object CheckBox13: TCheckBox
          Left = 8
          Top = 200
          Width = 97
          Height = 17
          Caption = 'Openness'
          TabOrder = 10
        end
      end
    end
    object tabsheet2: TTabSheet
      Caption = 'Create grids'
      ImageIndex = 1
      object GroupBox4: TGroupBox
        Left = 0
        Top = 3
        Width = 150
        Height = 97
        Caption = 'Relief 1'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        object CheckBox21: TCheckBox
          Left = 16
          Top = 15
          Width = 97
          Height = 17
          Caption = 'Relief'
          Color = clGradientActiveCaption
          ParentColor = False
          TabOrder = 0
        end
        object CheckBox22: TCheckBox
          Left = 16
          Top = 32
          Width = 110
          Height = 17
          Caption = 'Average elevation'
          TabOrder = 1
        end
        object CheckBox23: TCheckBox
          Left = 16
          Top = 48
          Width = 110
          Height = 17
          Caption = 'Elevation std dev'
          TabOrder = 2
        end
        object CheckBox24: TCheckBox
          Left = 16
          Top = 64
          Width = 118
          Height = 17
          Caption = 'REL (% pts lower)'
          TabOrder = 3
        end
        object CheckBox51: TCheckBox
          Left = 16
          Top = 80
          Width = 97
          Height = 17
          Caption = 'TPI'
          TabOrder = 4
        end
      end
      object GroupBox7: TGroupBox
        Left = 183
        Top = 4
        Width = 139
        Height = 200
        Caption = 'Slope/Aspect'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
        object CheckBox33: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Slope (%)'
          TabOrder = 0
        end
        object CheckBox34: TCheckBox
          Left = 16
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Slope (deg)'
          TabOrder = 1
        end
        object CheckBox35: TCheckBox
          Left = 16
          Top = 46
          Width = 97
          Height = 17
          Caption = 'Sine slope'
          TabOrder = 2
        end
        object CheckBox36: TCheckBox
          Left = 16
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Log tan slope'
          TabOrder = 3
        end
        object CheckBox37: TCheckBox
          Left = 16
          Top = 96
          Width = 97
          Height = 17
          Caption = 'Sqrt sin slope'
          TabOrder = 4
        end
        object CheckBox38: TCheckBox
          Left = 16
          Top = 112
          Width = 97
          Height = 17
          Caption = 'Aspect'
          TabOrder = 5
        end
        object CheckBox39: TCheckBox
          Left = 16
          Top = 128
          Width = 97
          Height = 17
          Caption = 'NS aspect'
          TabOrder = 6
        end
        object CheckBox40: TCheckBox
          Left = 16
          Top = 143
          Width = 97
          Height = 17
          Caption = 'EW aspect'
          TabOrder = 7
        end
        object CheckBox42: TCheckBox
          Left = 15
          Top = 161
          Width = 97
          Height = 17
          Caption = 'NS Slope'
          TabOrder = 8
        end
        object CheckBox43: TCheckBox
          Left = 16
          Top = 80
          Width = 90
          Height = 17
          Caption = 'Ln tan slope'
          TabOrder = 9
        end
        object CheckBox53: TCheckBox
          Left = 16
          Top = 180
          Width = 97
          Height = 17
          Caption = 'EW Slope'
          TabOrder = 10
        end
      end
      object GroupBox6: TGroupBox
        Left = 510
        Top = 3
        Width = 139
        Height = 218
        Caption = 'Terrain fabric/SSO'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 2
        object Label10: TLabel
          Left = 12
          Top = 124
          Width = 58
          Height = 13
          Caption = 'Box size (m)'
        end
        object Label8: TLabel
          Left = 12
          Top = 150
          Width = 63
          Height = 13
          Caption = 'Map thinniing'
        end
        object CheckBox29: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = 'S1S2 (flatness)'
          TabOrder = 0
        end
        object CheckBox30: TCheckBox
          Left = 16
          Top = 32
          Width = 113
          Height = 17
          Caption = 'S2S3 (organization)'
          TabOrder = 1
        end
        object CheckBox31: TCheckBox
          Left = 16
          Top = 47
          Width = 121
          Height = 17
          Caption = 'Fabric direction 90'
          TabOrder = 2
        end
        object CheckBox32: TCheckBox
          Left = 16
          Top = 97
          Width = 97
          Height = 17
          Caption = 'Roughness'
          TabOrder = 3
        end
        object CheckBox44: TCheckBox
          Left = 16
          Top = 64
          Width = 121
          Height = 17
          Caption = 'Fabric direction 180'
          TabOrder = 4
        end
        object CheckBox45: TCheckBox
          Left = 16
          Top = 80
          Width = 121
          Height = 17
          Caption = 'Fabric direction 360'
          TabOrder = 5
        end
        object Edit3: TEdit
          Left = 87
          Top = 120
          Width = 49
          Height = 21
          TabOrder = 6
        end
        object BitBtn3: TBitBtn
          Left = 3
          Top = 176
          Width = 126
          Height = 25
          Caption = 'Test box size'
          TabOrder = 7
          OnClick = BitBtn3Click
        end
        object Edit8: TEdit
          Left = 89
          Top = 147
          Width = 47
          Height = 21
          TabOrder = 8
        end
      end
      object GroupBox8: TGroupBox
        Left = 328
        Top = 227
        Width = 228
        Height = 191
        Caption = 'Moments'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 3
        object Label12: TLabel
          Left = 17
          Top = 113
          Width = 58
          Height = 13
          Caption = 'Box size (m)'
        end
        object Label13: TLabel
          Left = 21
          Top = 136
          Width = 61
          Height = 13
          Caption = 'Map thinning'
        end
        object CheckBox47: TCheckBox
          Left = 11
          Top = 17
          Width = 97
          Height = 17
          Caption = 'Elevation'
          TabOrder = 0
        end
        object CheckBox48: TCheckBox
          Left = 11
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Slope'
          TabOrder = 1
        end
        object CheckBox49: TCheckBox
          Left = 11
          Top = 63
          Width = 97
          Height = 17
          Caption = 'Plan curvature'
          TabOrder = 2
        end
        object CheckBox50: TCheckBox
          Left = 11
          Top = 86
          Width = 97
          Height = 17
          Caption = 'Profile curvature'
          TabOrder = 3
        end
        object Edit9: TEdit
          Left = 86
          Top = 109
          Width = 49
          Height = 21
          TabOrder = 4
        end
        object Edit10: TEdit
          Left = 88
          Top = 136
          Width = 47
          Height = 21
          TabOrder = 5
        end
        object BitBtn6: TBitBtn
          Left = 3
          Top = 163
          Width = 126
          Height = 25
          Caption = 'Test box size'
          TabOrder = 6
          OnClick = BitBtn6Click
        end
        object CheckBox8: TCheckBox
          Left = 128
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Mean'
          TabOrder = 7
        end
        object CheckBox55: TCheckBox
          Left = 128
          Top = 31
          Width = 97
          Height = 17
          Caption = 'Std Dev'
          TabOrder = 8
        end
        object CheckBox56: TCheckBox
          Left = 128
          Top = 56
          Width = 97
          Height = 17
          Caption = 'Skewness'
          TabOrder = 9
        end
        object CheckBox57: TCheckBox
          Left = 128
          Top = 79
          Width = 97
          Height = 17
          Caption = 'Kurtosis'
          TabOrder = 10
        end
      end
      object GroupBox3: TGroupBox
        Left = 183
        Top = 210
        Width = 137
        Height = 114
        Caption = 'Curvature'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 4
        object CheckBox17: TCheckBox
          Left = 24
          Top = 36
          Width = 97
          Height = 17
          Caption = 'Plan'
          TabOrder = 0
        end
        object CheckBox18: TCheckBox
          Left = 24
          Top = 55
          Width = 97
          Height = 17
          Caption = 'Minimum'
          TabOrder = 1
        end
        object CheckBox19: TCheckBox
          Left = 24
          Top = 72
          Width = 97
          Height = 17
          Caption = 'Maximum'
          TabOrder = 2
        end
        object CheckBox20: TCheckBox
          Left = 24
          Top = 90
          Width = 97
          Height = 17
          Caption = 'Cross'
          TabOrder = 3
        end
        object CheckBox16: TCheckBox
          Left = 24
          Top = 18
          Width = 97
          Height = 17
          Caption = 'Profile'
          TabOrder = 4
        end
      end
      object GroupBox2: TGroupBox
        Left = 349
        Top = 3
        Width = 140
        Height = 163
        Caption = 'Openness'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 5
        object Label9: TLabel
          Left = 8
          Top = 79
          Width = 58
          Height = 13
          Caption = 'Box size (m)'
        end
        object Label1: TLabel
          Left = 8
          Top = 106
          Width = 61
          Height = 13
          Caption = 'Map thinning'
        end
        object CheckBox14: TCheckBox
          Left = 21
          Top = 22
          Width = 97
          Height = 16
          Caption = 'Upward'
          TabOrder = 0
        end
        object CheckBox15: TCheckBox
          Left = 21
          Top = 40
          Width = 97
          Height = 17
          Caption = 'Downward'
          TabOrder = 1
        end
        object Edit2: TEdit
          Left = 76
          Top = 76
          Width = 49
          Height = 21
          TabOrder = 2
          OnChange = Edit1Change
        end
        object Edit1: TEdit
          Left = 79
          Top = 103
          Width = 47
          Height = 21
          TabOrder = 3
        end
        object BitBtn5: TBitBtn
          Left = 3
          Top = 130
          Width = 126
          Height = 25
          Caption = 'Test box size'
          TabOrder = 4
          OnClick = BitBtn5Click
        end
        object CheckBox54: TCheckBox
          Left = 20
          Top = 58
          Width = 97
          Height = 17
          Caption = 'Difference'
          TabOrder = 5
        end
      end
      object CheckBox46: TCheckBox
        Left = 29
        Top = 359
        Width = 97
        Height = 17
        Caption = 'Toggle all/none'
        TabOrder = 6
        OnClick = CheckBox46Click
      end
      object CheckBox52: TCheckBox
        Left = 29
        Top = 336
        Width = 97
        Height = 17
        Caption = 'Auto save grids'
        TabOrder = 7
        OnClick = CheckBox52Click
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 106
        Width = 150
        Height = 115
        Caption = 'Relief 2'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 8
        object CheckBox25: TCheckBox
          Left = 16
          Top = 12
          Width = 97
          Height = 17
          Caption = 'Relief'
          TabOrder = 0
        end
        object CheckBox26: TCheckBox
          Left = 16
          Top = 30
          Width = 97
          Height = 17
          Caption = 'Summit'
          Color = clSkyBlue
          ParentColor = False
          TabOrder = 1
        end
        object CheckBox27: TCheckBox
          Left = 16
          Top = 46
          Width = 97
          Height = 17
          Caption = 'Base level'
          TabOrder = 2
        end
        object CheckBox28: TCheckBox
          Left = 16
          Top = 64
          Width = 97
          Height = 17
          Caption = 'Geophysical'
          TabOrder = 3
        end
        object CheckBox41: TCheckBox
          Left = 16
          Top = 83
          Width = 97
          Height = 17
          Caption = 'Dropoff'
          TabOrder = 4
        end
        object CheckBox58: TCheckBox
          Left = 16
          Top = 98
          Width = 115
          Height = 17
          Caption = 'Elevation relief ratio'
          TabOrder = 5
        end
      end
      object BitBtn4: TBitBtn
        Left = 11
        Top = 390
        Width = 123
        Height = 25
        Caption = 'Create all maps'
        TabOrder = 9
        OnClick = BitBtn4Click
      end
      object GroupBox10: TGroupBox
        Left = 0
        Top = 227
        Width = 134
        Height = 110
        Caption = 'Both relief groupings'
        Color = clGradientInactiveCaption
        ParentBackground = False
        ParentColor = False
        TabOrder = 10
        object Label18: TLabel
          Left = 3
          Top = 23
          Width = 58
          Height = 13
          Caption = 'Box size (m)'
        end
        object Label4: TLabel
          Left = 3
          Top = 57
          Width = 61
          Height = 13
          Caption = 'Map thinning'
        end
        object Edit13: TEdit
          Left = 67
          Top = 20
          Width = 49
          Height = 21
          TabOrder = 0
        end
        object Edit11: TEdit
          Left = 69
          Top = 49
          Width = 47
          Height = 21
          TabOrder = 1
        end
        object BitBtn7: TBitBtn
          Left = 3
          Top = 78
          Width = 126
          Height = 25
          Caption = 'Test box size'
          TabOrder = 2
          OnClick = BitBtn7Click
        end
      end
    end
  end
  object GroupBox9: TGroupBox
    Left = 8
    Top = 236
    Width = 165
    Height = 110
    Caption = 'Minimum points for region stats'
    TabOrder = 5
    object Label6: TLabel
      Left = 24
      Top = 81
      Width = 44
      Height = 13
      Caption = 'Elevation'
    end
    object Label16: TLabel
      Left = 27
      Top = 31
      Width = 20
      Height = 13
      Caption = 'SSO'
    end
    object Edit12: TEdit
      Left = 73
      Top = 28
      Width = 49
      Height = 21
      TabOrder = 0
      OnChange = Edit12Change
    end
    object Edit5: TEdit
      Left = 73
      Top = 86
      Width = 49
      Height = 21
      TabOrder = 1
    end
  end
  object GroupBox12: TGroupBox
    Left = 320
    Top = 462
    Width = 153
    Height = 55
    Caption = 'Polygon/area statistics'
    TabOrder = 6
    object Label3: TLabel
      Left = 15
      Top = 28
      Width = 49
      Height = 13
      Caption = 'Buffer (m)'
    end
    object Edit6: TEdit
      Left = 70
      Top = 28
      Width = 63
      Height = 21
      TabOrder = 0
      Text = 'Edit6'
      OnChange = Edit6Change
    end
  end
  object GroupBox13: TGroupBox
    Left = 497
    Top = 459
    Width = 162
    Height = 65
    Caption = 'DEM series options'
    TabOrder = 7
    object Label7: TLabel
      Left = 89
      Top = 47
      Width = 31
      Height = 13
      Caption = 'Label7'
    end
    object Label2: TLabel
      Left = 89
      Top = 17
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object BitBtn2: TBitBtn
      Left = 8
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Output path'
      Enabled = False
      TabOrder = 0
      OnClick = BitBtn2Click
    end
    object BitBtn1: TBitBtn
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'DEM Location'
      Enabled = False
      TabOrder = 1
      OnClick = BitBtn1Click
    end
  end
  object Edit4: TEdit
    Left = 109
    Top = 394
    Width = 49
    Height = 21
    TabOrder = 8
    OnChange = Edit4Change
  end
end
