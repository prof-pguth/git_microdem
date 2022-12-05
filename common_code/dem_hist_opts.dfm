object HistOptForm: THistOptForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DEM Histogram Options'
  ClientHeight = 275
  ClientWidth = 231
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 218
    Width = 35
    Height = 13
    Caption = 'Bin size'
  end
  object HelpBtn: TBitBtn
    Left = 105
    Top = 258
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
  object BitBtn1: TBitBtn
    Left = 24
    Top = 260
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
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 44
    Width = 121
    Height = 17
    Caption = 'Regular histogram'
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 60
    Width = 153
    Height = 17
    Caption = 'Cumulative histogram'
    TabOrder = 3
  end
  object CheckBox3: TCheckBox
    Left = 24
    Top = 76
    Width = 153
    Height = 17
    Caption = 'Normal probability '
    TabOrder = 4
  end
  object CheckBox4: TCheckBox
    Left = 24
    Top = 92
    Width = 97
    Height = 17
    Caption = 'Strahler'
    TabOrder = 5
  end
  object CheckBox5: TCheckBox
    Left = 24
    Top = 108
    Width = 97
    Height = 17
    Caption = 'Text'
    TabOrder = 6
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 176
    Width = 233
    Height = 33
    Caption = 'Axis'
    Columns = 2
    Items.Strings = (
      'Normalized '
      'Arithmetic')
    TabOrder = 7
  end
  object Edit1: TEdit
    Left = 88
    Top = 215
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'Edit1'
    OnChange = Edit1Change
  end
end
