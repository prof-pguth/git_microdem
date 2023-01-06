object DEMDigitOptions: TDEMDigitOptions
  Left = 2007
  Top = 272
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Digitizing Options'
  ClientHeight = 156
  ClientWidth = 198
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label25: TLabel
    Left = 16
    Top = 87
    Width = 113
    Height = 13
    Caption = 'Digitizing pixel tolerance'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 137
    Height = 73
    Caption = 'Digitize'
    Items.Strings = (
      'Stream mode'
      'Point mode')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 143
    Top = 87
    Width = 57
    Height = 21
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 137
    Width = 89
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
    Left = 123
    Top = 137
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 106
    Width = 89
    Height = 25
    Caption = 'Line'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
end
