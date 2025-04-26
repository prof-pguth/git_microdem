object GetDir8: TGetDir8
  Left = 632
  Top = 247
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Directions to use'
  ClientHeight = 206
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 260
    Height = 193
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 206
    Width = 77
    Height = 27
    Caption = '&OK'
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
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 100
    Top = 207
    Width = 77
    Height = 27
    Caption = '&Cancel'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 183
    Top = 207
    Width = 77
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object GroupBox1: TGroupBox
    Left = 48
    Top = 16
    Width = 81
    Height = 161
    Caption = 'Direction'
    TabOrder = 3
    object CheckBox1: TCheckBox
      Left = 16
      Top = 16
      Width = 45
      Height = 17
      Caption = 'N'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 32
      Width = 45
      Height = 17
      Caption = 'NE'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 48
      Width = 45
      Height = 17
      Caption = 'E'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 64
      Width = 45
      Height = 17
      Caption = 'SE'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 80
      Width = 45
      Height = 17
      Caption = 'S'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBox6: TCheckBox
      Left = 16
      Top = 96
      Width = 45
      Height = 17
      Caption = 'SW'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBox7: TCheckBox
      Left = 16
      Top = 112
      Width = 40
      Height = 17
      Caption = 'W'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 128
      Width = 45
      Height = 17
      Caption = 'NW'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
end
