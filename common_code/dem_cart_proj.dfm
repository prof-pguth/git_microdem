object ProjectionDemForm: TProjectionDemForm
  Left = 388
  Top = 175
  Caption = 'Map Projections'
  ClientHeight = 562
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label5: TLabel
    Left = 111
    Top = 512
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object Label3: TLabel
    Left = 278
    Top = 246
    Width = 50
    Height = 13
    Caption = 'UTM zone'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 194
    Height = 129
    Caption = 'Planar projections'
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Orthographic (T)'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Stereographic (T)'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 48
      Width = 183
      Height = 17
      Caption = 'Lambert Azimuthal Equal Area (T)'
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox10: TCheckBox
      Left = 8
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Gnomonic'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
    object CheckBox15: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Hammer'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object CheckBox19: TCheckBox
      Left = 8
      Top = 96
      Width = 183
      Height = 17
      Caption = 'Polar stereographic (ellipse)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = CheckBox1Click
    end
  end
  object BitBtn1: TBitBtn
    Left = 216
    Top = 533
    Width = 112
    Height = 25
    Caption = 'Open map'
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
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 144
    Width = 185
    Height = 153
    Caption = 'Cylindrical projections'
    TabOrder = 2
    object CheckBox4: TCheckBox
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      Caption = 'Mercator (spherical) (T)'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox5: TCheckBox
      Left = 8
      Top = 31
      Width = 145
      Height = 17
      Caption = 'EquiDistant Cylindrical'
      Enabled = False
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object CheckBox6: TCheckBox
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Miller cylindrical '
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox9: TCheckBox
      Left = 8
      Top = 80
      Width = 97
      Height = 17
      Caption = 'Cassini'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
    object CheckBox14: TCheckBox
      Left = 8
      Top = 65
      Width = 174
      Height = 17
      Caption = 'Cylindrical equal area'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object CheckBox17: TCheckBox
      Left = 8
      Top = 96
      Width = 177
      Height = 17
      Caption = 'Mercator (ellipsoidal) (T)'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      State = cbChecked
      TabOrder = 5
      OnClick = CheckBox1Click
    end
    object CheckBox18: TCheckBox
      Left = 8
      Top = 112
      Width = 137
      Height = 17
      Caption = 'UTM (ellipsoidal) (T)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = CheckBox1Click
    end
    object CheckBox27: TCheckBox
      Left = 8
      Top = 128
      Width = 97
      Height = 17
      Caption = 'Web Mercator (T) '
      TabOrder = 7
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 303
    Width = 194
    Height = 57
    Caption = 'Conical projections (ellipsoidal)'
    TabOrder = 3
    object CheckBox7: TCheckBox
      Left = 8
      Top = 15
      Width = 183
      Height = 17
      Caption = 'Lambert conformal conic (T)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox8: TCheckBox
      Left = 8
      Top = 37
      Width = 183
      Height = 17
      Caption = 'Albers equal area conical (T)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = CheckBox1Click
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 366
    Width = 185
    Height = 57
    Caption = 'Pseudo-cylindrical projections'
    TabOrder = 4
    object CheckBox11: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Molleweide (T)'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox12: TCheckBox
      Left = 8
      Top = 37
      Width = 97
      Height = 17
      Caption = 'Sinusoidal (T)'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 429
    Width = 185
    Height = 60
    Caption = 'Miscellaneous projections'
    TabOrder = 5
    object CheckBox13: TCheckBox
      Left = 8
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Van Der Grinten'
      Enabled = False
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox22: TCheckBox
      Left = 8
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Equal earth'
      TabOrder = 1
      OnClick = CheckBox22Click
    end
  end
  object CheckBox20: TCheckBox
    Left = 214
    Top = 429
    Width = 121
    Height = 17
    Caption = 'Open multiple maps'
    TabOrder = 6
    OnClick = CheckBox20Click
  end
  object Panel1: TPanel
    Left = 208
    Top = 8
    Width = 121
    Height = 162
    TabOrder = 7
    object Label2: TLabel
      Left = 27
      Top = 8
      Width = 70
      Height = 13
      Caption = 'Central latitude'
    end
    object Label1: TLabel
      Left = 18
      Top = 48
      Width = 79
      Height = 13
      Caption = 'Central longitude'
    end
    object Edit2: TEdit
      Left = 16
      Top = 24
      Width = 81
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object Edit1: TEdit
      Left = 16
      Top = 67
      Width = 81
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object CheckBox21: TCheckBox
      Left = 8
      Top = 136
      Width = 105
      Height = 17
      Caption = 'Override defaults'
      TabOrder = 2
      OnClick = CheckBox21Click
    end
  end
  object HelpBtn: TBitBtn
    Left = 334
    Top = 532
    Width = 108
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 8
    OnClick = HelpBtnClick
    IsControl = True
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 507
    Width = 97
    Height = 25
    Caption = 'User Projection'
    TabOrder = 9
    OnClick = BitBtn2Click
  end
  object BitBtn4: TBitBtn
    Left = 335
    Top = 93
    Width = 75
    Height = 25
    Caption = 'Outlines'
    TabOrder = 10
  end
  object GroupBox6: TGroupBox
    Left = 335
    Top = 8
    Width = 131
    Height = 74
    TabOrder = 11
    object BitBtn3: TBitBtn
      Left = 24
      Top = 38
      Width = 75
      Height = 25
      Caption = 'Tissot options'
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object CheckBox16: TCheckBox
      Left = 10
      Top = 15
      Width = 113
      Height = 17
      Caption = 'Tissot Indicatrix (T)'
      TabOrder = 1
      OnClick = CheckBox16Click
    end
  end
  object CheckBox24: TCheckBox
    Left = 345
    Top = 124
    Width = 97
    Height = 17
    Caption = 'Grayscale'
    TabOrder = 12
    OnClick = CheckBox24Click
  end
  object CheckBox23: TCheckBox
    Left = 345
    Top = 147
    Width = 97
    Height = 17
    Caption = 'Subdue'
    TabOrder = 13
    OnClick = CheckBox23Click
  end
  object CheckBox28: TCheckBox
    Left = 214
    Top = 452
    Width = 124
    Height = 17
    Caption = 'Close form on opening'
    TabOrder = 14
    OnClick = CheckBox28Click
  end
  object ComboBox2: TComboBox
    Left = 209
    Top = 207
    Width = 129
    Height = 21
    Enabled = False
    TabOrder = 15
    Text = '0   Lambert     '
    Items.Strings = (
      '0  Lambert     '
      '30  Behrmann '
      '37.06667 Smyth     '
      '37.4  Trystan-Edwards'
      '37.5  Hobo-Dyer   '
      '45  Gall-Peters '
      '50  Balthasart   '
      '55.65  Tobler    ')
  end
  object Edit3: TEdit
    Left = 216
    Top = 243
    Width = 56
    Height = 21
    TabOrder = 16
    OnChange = Edit3Change
  end
end
