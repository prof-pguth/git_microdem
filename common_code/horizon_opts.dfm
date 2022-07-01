object HorizonOptions: THorizonOptions
  Left = 192
  Top = 173
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Horizon Options'
  ClientHeight = 433
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 8
    Top = 352
    Width = 83
    Height = 25
    Caption = 'Horizon'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object OKBtn: TBitBtn
    Left = 38
    Top = 401
    Width = 83
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
  object CheckBox5: TCheckBox
    Left = 24
    Top = 329
    Width = 113
    Height = 17
    Caption = 'Verify time zones'
    TabOrder = 2
  end
  object CheckBox6: TCheckBox
    Left = 24
    Top = 311
    Width = 97
    Height = 17
    Caption = 'Solar path map'
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 277
    Top = 215
    Width = 185
    Height = 123
    Caption = 'Geostationary satellite pointing'
    TabOrder = 4
    object Label9: TLabel
      Left = 25
      Top = 42
      Width = 87
      Height = 13
      Caption = 'Azimuth to satellite'
    end
    object Label10: TLabel
      Left = 10
      Top = 15
      Width = 94
      Height = 13
      Caption = 'Elevation to satellite'
    end
    object Label11: TLabel
      Left = 17
      Top = 69
      Width = 95
      Height = 13
      Caption = 'Map sampling factor'
    end
    object CheckBox7: TCheckBox
      Left = 17
      Top = 96
      Width = 129
      Height = 17
      Caption = 'Show masked regions'
      TabOrder = 0
    end
    object Edit11: TEdit
      Left = 118
      Top = 69
      Width = 48
      Height = 21
      TabOrder = 1
    end
    object Edit10: TEdit
      Left = 118
      Top = 15
      Width = 48
      Height = 21
      TabOrder = 2
    end
    object Edit9: TEdit
      Left = 118
      Top = 42
      Width = 48
      Height = 21
      TabOrder = 3
    end
  end
  object HelpBtn: TBitBtn
    Left = 147
    Top = 401
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = HelpBtnClick
    IsControl = True
  end
  object GroupBox2: TGroupBox
    Left = 277
    Top = 8
    Width = 181
    Height = 161
    Caption = 'Sunlight computations'
    TabOrder = 6
    object Label8: TLabel
      Left = 36
      Top = 139
      Width = 79
      Height = 13
      Caption = 'Single Julian day'
    end
    object Label7: TLabel
      Left = 7
      Top = 120
      Width = 108
      Height = 13
      Caption = 'Sunlight precision (min)'
    end
    object Label6: TLabel
      Left = 26
      Top = 46
      Width = 79
      Height = 13
      Caption = 'Last day sunlight'
    end
    object Label5: TLabel
      Left = 27
      Top = 27
      Width = 78
      Height = 13
      Caption = 'First day sunlight'
    end
    object Label12: TLabel
      Left = 32
      Top = 76
      Width = 66
      Height = 13
      Caption = 'Interval (days)'
    end
    object Edit6: TEdit
      Left = 111
      Top = 46
      Width = 57
      Height = 21
      TabOrder = 0
    end
    object Edit8: TEdit
      Left = 121
      Top = 137
      Width = 57
      Height = 21
      TabOrder = 1
    end
    object Edit7: TEdit
      Left = 121
      Top = 112
      Width = 57
      Height = 21
      TabOrder = 2
    end
    object Edit5: TEdit
      Left = 111
      Top = 19
      Width = 57
      Height = 21
      TabOrder = 3
    end
    object Edit12: TEdit
      Left = 112
      Top = 73
      Width = 57
      Height = 21
      TabOrder = 4
      Text = 'Edit12'
    end
  end
  object BitBtn2: TBitBtn
    Left = 478
    Top = 163
    Width = 107
    Height = 25
    Caption = 'Draw graphs'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 208
    Top = 352
    Width = 92
    Height = 25
    Caption = 'Stereonet options'
    TabOrder = 8
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 472
    Top = 70
    Width = 113
    Height = 25
    Caption = 'Horizon blocking'
    TabOrder = 9
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 472
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Sunlight maps'
    TabOrder = 10
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 472
    Top = 39
    Width = 113
    Height = 25
    Caption = 'Single sunlight map'
    TabOrder = 11
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 476
    Top = 101
    Width = 109
    Height = 25
    Caption = 'Single day table'
    TabOrder = 12
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 476
    Top = 132
    Width = 105
    Height = 25
    Caption = 'Annual table'
    TabOrder = 13
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 97
    Top = 352
    Width = 105
    Height = 25
    Caption = 'Grid options'
    TabOrder = 14
    OnClick = BitBtn9Click
  end
  object BitBtn11: TBitBtn
    Left = 478
    Top = 194
    Width = 105
    Height = 25
    Caption = 'Geostationary satellite'
    TabOrder = 15
    OnClick = BitBtn11Click
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 134
    Caption = 'Horizon calculation settings'
    TabOrder = 16
    object Label3: TLabel
      Left = 88
      Top = 94
      Width = 128
      Height = 13
      Caption = 'Angular precision (degrees)'
    end
    object Label2: TLabel
      Left = 88
      Top = 70
      Width = 172
      Height = 13
      Caption = 'Radial precision (times data spacing)'
    end
    object Label4: TLabel
      Left = 88
      Top = 43
      Width = 132
      Height = 13
      Caption = 'Observer above ground (m) '
    end
    object Label1: TLabel
      Left = 88
      Top = 16
      Width = 76
      Height = 13
      Caption = 'Max Horizon (m)'
    end
    object Edit4: TEdit
      Left = 16
      Top = 43
      Width = 57
      Height = 21
      TabOrder = 0
      OnChange = Edit4Change
    end
    object Edit3: TEdit
      Left = 16
      Top = 94
      Width = 57
      Height = 21
      TabOrder = 1
      OnChange = Edit3Change
    end
    object Edit2: TEdit
      Left = 16
      Top = 70
      Width = 57
      Height = 21
      TabOrder = 2
      OnChange = Edit2Change
    end
    object Edit1: TEdit
      Left = 18
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 3
      OnChange = Edit1Change
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 145
    Width = 249
    Height = 144
    Caption = 'Sunlight graphs'
    TabOrder = 17
    object CheckBox8: TCheckBox
      Left = 173
      Top = 76
      Width = 68
      Height = 17
      Caption = 'Solstices'
      TabOrder = 0
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 119
      Width = 97
      Height = 17
      Caption = 'Invert Skyline'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 96
      Width = 158
      Height = 17
      Caption = 'Circular horizon map'
      TabOrder = 2
      OnClick = CheckBox4Click
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 73
      Width = 137
      Height = 17
      Caption = 'Graph horizon vert angle'
      TabOrder = 3
      OnClick = CheckBox3Click
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 50
      Width = 129
      Height = 17
      Caption = 'Graph horizon distance'
      TabOrder = 4
      OnClick = CheckBox2Click
    end
    object CheckBox9: TCheckBox
      Left = 19
      Top = 27
      Width = 129
      Height = 17
      Caption = 'Day Light Duration'
      TabOrder = 5
    end
  end
end
