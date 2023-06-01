object ElevationRangeForm: TElevationRangeForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Elevation Range For Coloring'
  ClientHeight = 316
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 159
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label2: TLabel
    Left = 17
    Top = 191
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label3: TLabel
    Left = 57
    Top = 140
    Width = 26
    Height = 13
    Caption = 'Value'
  end
  object Label4: TLabel
    Left = 120
    Top = 140
    Width = 47
    Height = 13
    Caption = 'Percentile'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 158
    Top = 273
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object OKBtn: TBitBtn
    Left = 17
    Top = 273
    Width = 57
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
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 80
    Top = 273
    Width = 65
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 122
    Height = 126
    Caption = 'z Range'
    ItemIndex = 4
    Items.Strings = (
      'DEM full range'
      'Map area range'
      'Topography only'
      'Bathymetry only'
      'Specified'
      'Percentiles')
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object Edit1: TEdit
    Left = 43
    Top = 159
    Width = 71
    Height = 21
    TabOrder = 3
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 43
    Top = 186
    Width = 71
    Height = 21
    TabOrder = 4
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 120
    Top = 159
    Width = 63
    Height = 21
    TabOrder = 5
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 120
    Top = 186
    Width = 63
    Height = 21
    TabOrder = 6
    OnChange = Edit4Change
  end
  object BitBtn1: TBitBtn
    Left = 158
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Highs'
    TabOrder = 7
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 158
    Top = 86
    Width = 75
    Height = 25
    Caption = 'Lows'
    TabOrder = 8
    OnClick = BitBtn2Click
  end
  object CheckBox1: TCheckBox
    Left = 158
    Top = 117
    Width = 97
    Height = 17
    Caption = 'Clipped colors'
    TabOrder = 9
    OnClick = CheckBox1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 228
    Width = 287
    Height = 39
    Caption = 'Stretching'
    Columns = 3
    Items.Strings = (
      'None'
      'Percentiles'
      'Std Dev')
    TabOrder = 10
    OnClick = RadioGroup2Click
  end
  object BitBtn3: TBitBtn
    Left = 200
    Top = 197
    Width = 94
    Height = 25
    Caption = 'Mask to Z range'
    TabOrder = 11
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 158
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Missing'
    TabOrder = 12
    OnClick = BitBtn4Click
  end
end
