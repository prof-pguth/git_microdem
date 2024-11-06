object Koppen_opt_f: TKoppen_opt_f
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Geography Options'
  ClientHeight = 232
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 106
    Top = 96
    Width = 31
    Height = 13
    Caption = 'Height'
  end
  object Label2: TLabel
    Left = 11
    Top = 96
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object Label3: TLabel
    Left = 8
    Top = 161
    Width = 90
    Height = 13
    Caption = 'UTC offset (hours)'
  end
  object Ft: TLabel
    Left = 40
    Top = 128
    Width = 43
    Height = 13
    Caption = 'Font size'
  end
  object Label4: TLabel
    Left = 168
    Top = 24
    Width = 49
    Height = 13
    Caption = 'Max Temp'
  end
  object Label5: TLabel
    Left = 168
    Top = 48
    Width = 52
    Height = 13
    Caption = 'Max Precip'
  end
  object Edit1: TEdit
    Left = 143
    Top = 93
    Width = 52
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 51
    Top = 96
    Width = 49
    Height = 21
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 191
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
    TabOrder = 2
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 91
    Top = 191
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = HelpBtnClick
    IsControl = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 9
    Width = 129
    Height = 81
    Caption = 'Koppen climographs'
    TabOrder = 4
    object CheckBox1: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Show lat/long'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 38
      Width = 97
      Height = 17
      Caption = 'Temp/Precip'
      TabOrder = 1
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 61
      Width = 97
      Height = 17
      Caption = 'Show elevation'
      TabOrder = 2
    end
  end
  object Edit3: TEdit
    Left = 104
    Top = 158
    Width = 48
    Height = 21
    TabOrder = 5
  end
  object CheckBox7: TCheckBox
    Left = 176
    Top = 160
    Width = 118
    Height = 17
    Caption = 'Time zone from long'
    TabOrder = 6
  end
  object Edit4: TEdit
    Left = 104
    Top = 128
    Width = 48
    Height = 21
    TabOrder = 7
  end
  object Edit5: TEdit
    Left = 226
    Top = 21
    Width = 57
    Height = 21
    TabOrder = 8
    Text = 'Edit5'
  end
  object Edit6: TEdit
    Left = 226
    Top = 48
    Width = 57
    Height = 21
    TabOrder = 9
    Text = 'Edit6'
  end
end
