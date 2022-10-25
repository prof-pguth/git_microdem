object CartMovieOptsForm: TCartMovieOptsForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Rotation Movie Options'
  ClientHeight = 179
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 121
    Height = 105
    Caption = 'Frame step'
    ItemIndex = 2
    Items.Strings = (
      '10'#176' (36 frames)'
      '15'#176' (24 frames)'
      '20'#176' (18 frames)'
      '30'#176' (12 frames)')
    TabOrder = 0
  end
  object RadioGroup2: TRadioGroup
    Left = 160
    Top = 8
    Width = 113
    Height = 105
    Caption = 'Map size'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Large'
      'Medium'
      'Small')
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 119
    Width = 225
    Height = 17
    Caption = 'Overlay political boundaries and rivers'
    TabOrder = 2
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 162
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
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 166
    Top = 162
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 139
    Width = 121
    Height = 17
    Caption = 'Manually set colors'
    TabOrder = 5
  end
end
