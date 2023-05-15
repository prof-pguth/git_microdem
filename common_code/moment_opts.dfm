object MomentOptsForm: TMomentOptsForm
  Left = 0
  Top = 0
  Caption = 'Moment distribution options'
  ClientHeight = 230
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object CheckBox2: TCheckBox
    Left = 8
    Top = 24
    Width = 153
    Height = 17
    Caption = 'Elevation moments'
    TabOrder = 0
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Slope moments'
    TabOrder = 1
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 56
    Width = 153
    Height = 17
    Caption = 'Plan curvature moments'
    TabOrder = 2
  end
  object CheckBox5: TCheckBox
    Left = 8
    Top = 72
    Width = 169
    Height = 17
    Caption = 'Profile curvature moments'
    TabOrder = 3
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 187
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
    TabOrder = 4
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 91
    Top = 187
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
  object CheckBox6: TCheckBox
    Left = 8
    Top = 129
    Width = 97
    Height = 17
    Caption = 'Graphs'
    TabOrder = 6
  end
  object RadioGroup1: TRadioGroup
    Left = 167
    Top = 40
    Width = 117
    Height = 65
    Caption = 'Histgrams'
    Items.Strings = (
      'Numerical count'
      'Standardized')
    TabOrder = 7
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 95
    Width = 129
    Height = 17
    Caption = 'Roughness moments'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object CheckBox7: TCheckBox
    Left = 8
    Top = 152
    Width = 169
    Height = 17
    Caption = 'Long version statistics'
    TabOrder = 9
  end
end
