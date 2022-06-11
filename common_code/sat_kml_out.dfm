object KML_overlay_setup: TKML_overlay_setup
  Left = 0
  Top = 0
  Caption = 'KML Overlay setup'
  ClientHeight = 218
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 224
    Top = 8
    Width = 150
    Height = 150
    Stretch = True
  end
  object Label1: TLabel
    Left = 16
    Top = 72
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 16
    Top = 91
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 16
    Top = 110
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 8
    Top = 24
    Width = 89
    Height = 13
    Caption = 'Image blowup (%)'
  end
  object Label5: TLabel
    Left = 24
    Top = 43
    Width = 61
    Height = 13
    Caption = 'Max tile size '
  end
  object Edit1: TEdit
    Left = 103
    Top = 16
    Width = 57
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 103
    Top = 43
    Width = 57
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object HelpBtn: TBitBtn
    Left = 179
    Top = 183
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 13
    Top = 183
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
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 129
    Width = 97
    Height = 17
    Caption = 'Zip KML to KMZ'
    TabOrder = 4
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 152
    Width = 193
    Height = 17
    Caption = 'Output in Windows Temp directory'
    TabOrder = 5
  end
  object CancelBtn: TBitBtn
    Left = 96
    Top = 183
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
end
