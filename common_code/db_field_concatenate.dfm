object db_concatenate: Tdb_concatenate
  Left = 0
  Top = 0
  Caption = 'Concatenate fields'
  ClientHeight = 93
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 131
    Top = 24
    Width = 16
    Height = 23
    Caption = '='
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 160
    Top = 24
    Width = 49
    Height = 21
    TabOrder = 0
  end
  object ComboBox1: TComboBox
    Left = 216
    Top = 24
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 368
    Top = 24
    Width = 49
    Height = 21
    TabOrder = 2
  end
  object ComboBox2: TComboBox
    Left = 423
    Top = 24
    Width = 145
    Height = 21
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 782
    Top = 24
    Width = 51
    Height = 21
    TabOrder = 4
  end
  object BitBtn2: TBitBtn
    Left = 159
    Top = 62
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
    TabOrder = 5
    OnClick = BitBtn2Click
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 242
    Top = 62
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit4: TEdit
    Left = 8
    Top = 24
    Width = 105
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 12
    TabOrder = 7
    Text = 'NEW_FIELD'
  end
  object ComboBox3: TComboBox
    Left = 631
    Top = 24
    Width = 145
    Height = 21
    TabOrder = 8
  end
  object Edit5: TEdit
    Left = 574
    Top = 24
    Width = 51
    Height = 21
    TabOrder = 9
  end
end
