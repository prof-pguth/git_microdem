object GazOptsForm: TGazOptsForm
  Left = 28
  Top = 166
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Gazetteer Label Options'
  ClientHeight = 143
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 47
    Top = 62
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object BitBtn8: TBitBtn
    Left = 8
    Top = 55
    Width = 33
    Height = 25
    Caption = 'File'
    TabOrder = 0
    OnClick = BitBtn8Click
  end
  object HelpBtn: TBitBtn
    Left = 89
    Top = 117
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Features: TBitBtn
    Left = 8
    Top = 86
    Width = 75
    Height = 25
    Caption = 'Features'
    TabOrder = 2
    OnClick = FeaturesClick
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 129
    Height = 17
    Caption = 'Avoid text overwriting'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 117
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
  object BitBtn1: TBitBtn
    Left = 89
    Top = 86
    Width = 75
    Height = 25
    Caption = 'Edit symbols'
    TabOrder = 5
    OnClick = BitBtn1Click
  end
end
