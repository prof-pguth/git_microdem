object dbjoinform2: Tdbjoinform2
  Left = 0
  Top = 0
  ClientHeight = 341
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 42
    Height = 13
    Caption = 'Join field'
  end
  object Label3: TLabel
    Left = 27
    Top = 127
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 16
    Top = 171
    Width = 42
    Height = 13
    Caption = 'Join field'
  end
  object Label1: TLabel
    Left = 8
    Top = 5
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 24
    Top = 144
    Width = 3
    Height = 13
  end
  object Label6: TLabel
    Left = 48
    Top = 56
    Width = 3
    Height = 13
  end
  object Label7: TLabel
    Left = 45
    Top = 190
    Width = 3
    Height = 13
  end
  object Label8: TLabel
    Left = 80
    Top = 280
    Width = 190
    Height = 16
    Caption = 'Duplicate records in Join Field'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Label9: TLabel
    Left = 48
    Top = 80
    Width = 3
    Height = 13
  end
  object Label10: TLabel
    Left = 48
    Top = 208
    Width = 3
    Height = 13
  end
  object ComboBox1: TComboBox
    Left = 56
    Top = 21
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object Memo1: TMemo
    Left = 208
    Top = 8
    Width = 137
    Height = 89
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Join table'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object ComboBox2: TComboBox
    Left = 64
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 3
    OnChange = ComboBox2Change
  end
  object Memo2: TMemo
    Left = 208
    Top = 152
    Width = 137
    Height = 89
    Lines.Strings = (
      '')
    TabOrder = 4
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 306
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
    OnClick = OKBtnClick
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 99
    Top = 306
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = CancelBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 182
    Top = 306
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 7
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 240
    Width = 152
    Height = 17
    Caption = 'Allow multiple join matches'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 263
    Width = 177
    Height = 17
    Caption = 'Clear leading 0'#39's in join fields'
    TabOrder = 9
    OnClick = CheckBox2Click
  end
end
