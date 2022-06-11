object EditHeader: TEditHeader
  Left = 587
  Top = 137
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 490
  ClientWidth = 420
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBB
    BBBBB00000999999990001111111BBBBBBBBBBBBB00999999990001111110000
    BBBBBBBBBB00999999990011111133300BBBBBBBBBB009999998888888813333
    0000BBBBBBBB00999998FFFFFF813333333000BBBBBBB0099998F6666F813333
    3333300BBBBBBB009998F6666F810333333333000BBBBBB00098F6666F810000
    0033333300BBBBBBB008F6666F80AAAAA0033333300BBBBBBB08F6666F80AAAA
    AA03333333000BBBBBB8F6666F89AAAAAA003333333300BBBBB8F6666F898888
    88888888888888888888F6666F898FFFFFFFFFFFFFFFFFFFFFFFF6666F898EEE
    EEEEEEEEEEEEEEEEEEEEE6666F898EEEEEEEEEEEEEEEEEEEEEEEF6666F898FFF
    FFFFFFFFFFFFFFFFFFFFF6666F8988888888888888888888888FF6666F802222
    000AAAAAAAA00333333FF6666F8B22222200AAAAAAAA0033333FF6666F8B2222
    22200AAAAAAAA003333FF6666F8B2222222200AAAAAAAA03333FF666F88B0222
    22222000AAAAAA00333FF666F8BB00022222222000AAAAA0033FF666F8BBCC00
    02222222200AAAAA00FF6666F8BBCCCC002222222200AAAAA0FF6666F80BCCCC
    C00022222220AAAAAAFF6666F80B00CCCCC0002222200AAAAAFF666FF800400C
    CCCCC022222200AAAAFF666F88334400CCCCC00222222000AAFF666F83334440
    00CCCC002222222000FFFFFF83334444400CCCC00022222220AAAA0033330000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = True
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 24
    Top = 136
    Width = 59
    Height = 13
    Caption = 'UTM zone'
  end
  object Label6: TLabel
    Left = 159
    Top = 112
    Width = 8
    Height = 13
    Caption = 'L'
    Visible = False
  end
  object Label8: TLabel
    Left = 176
    Top = 136
    Width = 29
    Height = 13
    Caption = 'Cols:'
  end
  object Label9: TLabel
    Left = 288
    Top = 136
    Width = 36
    Height = 13
    Caption = 'Rows:'
  end
  object Label13: TLabel
    Left = 232
    Top = 16
    Width = 143
    Height = 13
    Caption = 'Digitizing Datum for DEM'
  end
  object Label10: TLabel
    Left = 304
    Top = 80
    Width = 5
    Height = 13
  end
  object Label11: TLabel
    Left = 32
    Top = 160
    Width = 46
    Height = 13
    Caption = 'Label11'
  end
  object Edit5: TEdit
    Left = 96
    Top = 136
    Width = 49
    Height = 21
    TabOrder = 0
    Text = ' '
    OnChange = Edit5Change
  end
  object Edit6: TEdit
    Left = 208
    Top = 136
    Width = 57
    Height = 21
    TabOrder = 1
    Text = ' '
    OnChange = Edit6Change
  end
  object Edit7: TEdit
    Left = 328
    Top = 136
    Width = 65
    Height = 21
    TabOrder = 2
    Text = ' '
    OnChange = Edit7Change
  end
  object ComboBox2: TComboBox
    Left = 232
    Top = 40
    Width = 137
    Height = 21
    TabOrder = 3
    Text = ' '
    OnChange = ComboBox2Change
    Items.Strings = (
      'WGS72'
      'WGS84'
      'NAD27'
      'NAD83'
      'Spherical'
      'Local'
      'Rectangular'
      'Lambert Azimuthal Spherical'
      'Puerto Rico'
      'Sinusopidal Ellispoidal'
      'UK OS'
      'Defined'
      'Mars'
      'Venus'
      'SPCS')
  end
  object RadioGroup1: TRadioGroup
    Left = 144
    Top = 16
    Width = 81
    Height = 65
    Caption = 'Hemisphere'
    Items.Strings = (
      'North'
      'South')
    TabOrder = 4
  end
  object GroupBox2: TGroupBox
    Left = 24
    Top = 304
    Width = 353
    Height = 65
    Caption = 'SW corner'
    TabOrder = 5
    object Label7: TLabel
      Left = 64
      Top = 16
      Width = 39
      Height = 13
      Caption = 'Label7'
    end
    object xcoord: TLabel
      Left = 64
      Top = 40
      Width = 7
      Height = 13
      Caption = 'x'
    end
    object Label3: TLabel
      Left = 224
      Top = 40
      Width = 11
      Height = 13
      Caption = 'y '
    end
    object Edit3: TEdit
      Left = 88
      Top = 40
      Width = 97
      Height = 21
      TabOrder = 0
      Text = ' '
      OnChange = Edit3Change
    end
    object Edit4: TEdit
      Left = 248
      Top = 40
      Width = 97
      Height = 21
      TabOrder = 1
      Text = ' '
      OnChange = Edit4Change
    end
    object BitBtn2: TBitBtn
      Left = 16
      Top = 16
      Width = 33
      Height = 33
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
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
      TabOrder = 2
      OnClick = BitBtn2Click
    end
  end
  object GroupBox3: TGroupBox
    Left = 24
    Top = 176
    Width = 353
    Height = 49
    Caption = 'XY Units and spacing'
    TabOrder = 6
    object Label4: TLabel
      Left = 112
      Top = 16
      Width = 13
      Height = 13
      Caption = 'X:'
    end
    object Label15: TLabel
      Left = 232
      Top = 16
      Width = 13
      Height = 13
      Caption = 'Y:'
    end
    object ComboBox4: TComboBox
      Left = 9
      Top = 16
      Width = 97
      Height = 21
      TabOrder = 0
      Text = ' '
      OnChange = ComboBox4Change
    end
    object Edit1: TEdit
      Left = 136
      Top = 16
      Width = 81
      Height = 21
      TabOrder = 1
      Text = ' '
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 256
      Top = 16
      Width = 81
      Height = 21
      TabOrder = 2
      Text = ' '
      OnChange = Edit2Change
    end
  end
  object GroupBox4: TGroupBox
    Left = 24
    Top = 232
    Width = 353
    Height = 65
    Caption = 'Z Units'
    TabOrder = 7
    object Label1: TLabel
      Left = 200
      Top = 40
      Width = 21
      Height = 13
      Caption = 'Min'
    end
    object Label2: TLabel
      Left = 72
      Top = 40
      Width = 24
      Height = 13
      Caption = 'Max'
    end
    object ComboBox3: TComboBox
      Left = 3
      Top = 13
      Width = 201
      Height = 21
      TabOrder = 0
      Text = ' '
      OnChange = ComboBox3Change
    end
    object Edit8: TEdit
      Left = 112
      Top = 40
      Width = 73
      Height = 21
      TabOrder = 1
      Text = ' '
    end
    object Edit9: TEdit
      Left = 232
      Top = 40
      Width = 73
      Height = 21
      TabOrder = 2
      Text = ' '
    end
    object BitBtn3: TBitBtn
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Auto Check'
      TabOrder = 3
      OnClick = BitBtn3Click
    end
  end
  object RadioGroup2: TRadioGroup
    Left = 16
    Top = 16
    Width = 121
    Height = 65
    Caption = 'DEM type'
    Items.Strings = (
      'Lat/long based'
      'UTM based')
    TabOrder = 8
  end
  object BitBtn1: TBitBtn
    Left = 232
    Top = 72
    Width = 65
    Height = 25
    Caption = 'Projection'
    TabOrder = 9
    OnClick = BitBtn1Click
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 387
    Width = 393
    Height = 42
    Caption = 'Z data type'
    Columns = 4
    Enabled = False
    Items.Strings = (
      'Small integer'
      'Floating point'
      'Byte'
      'Word')
    TabOrder = 10
  end
  object BitBtn5: TBitBtn
    Left = 16
    Top = 105
    Width = 137
    Height = 25
    Caption = 'Local horizontal datum'
    TabOrder = 11
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 98
    Top = 445
    Width = 75
    Height = 25
    Caption = 'OK'
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
    TabOrder = 12
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 179
    Top = 445
    Width = 75
    Height = 25
    Caption = 'Cancel'
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
    NumGlyphs = 2
    TabOrder = 13
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 260
    Top = 444
    Width = 75
    Height = 25
    Caption = 'Help'
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333336633
      3333333333333FF3333333330000333333364463333333333333388F33333333
      00003333333E66433333333333338F38F3333333000033333333E66333333333
      33338FF8F3333333000033333333333333333333333338833333333300003333
      3333446333333333333333FF3333333300003333333666433333333333333888
      F333333300003333333E66433333333333338F38F333333300003333333E6664
      3333333333338F38F3333333000033333333E6664333333333338F338F333333
      0000333333333E6664333333333338F338F3333300003333344333E666433333
      333F338F338F3333000033336664333E664333333388F338F338F33300003333
      E66644466643333338F38FFF8338F333000033333E6666666663333338F33888
      3338F3330000333333EE666666333333338FF33333383333000033333333EEEE
      E333333333388FFFFF8333330000333333333333333333333333388888333333
      0000}
    NumGlyphs = 2
    TabOrder = 14
    OnClick = BitBtn8Click
  end
end
