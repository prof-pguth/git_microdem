object MapAlgebraForm: TMapAlgebraForm
  Left = 0
  Top = 0
  Caption = 'Map Algebra '
  ClientHeight = 287
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 24
    Width = 137
    Height = 217
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object HelpBtn: TBitBtn
    Left = 247
    Top = 214
    Width = 58
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 189
    Top = 214
    Width = 52
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
  object BitBtn1: TBitBtn
    Left = 189
    Top = 24
    Width = 75
    Height = 25
    Caption = '+'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 264
    Width = 627
    Height = 21
    TabOrder = 4
  end
  object CheckBox1: TCheckBox
    Left = 189
    Top = 176
    Width = 252
    Height = 17
    Caption = 'Missing data in any grid makes result missing'
    TabOrder = 5
  end
end
