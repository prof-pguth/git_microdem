object ThumbnailForm: TThumbnailForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Thumbnail options'
  ClientHeight = 182
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 24
    Width = 148
    Height = 13
    Caption = 'Thumbnai image height (pixels)'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 113
    Height = 105
    Caption = 'Output file format'
    ItemIndex = 1
    Items.Strings = (
      '.bmp'
      '.jpg'
      '.gif'
      '.png')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 315
    Top = 21
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '75'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 119
    Width = 49
    Height = 25
    Caption = 'Dir'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object Edit2: TEdit
    Left = 80
    Top = 121
    Width = 290
    Height = 21
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 163
    Top = 64
    Width = 121
    Height = 17
    Caption = 'Create HTML form'
    TabOrder = 4
  end
  object HelpBtn: TBitBtn
    Left = 91
    Top = 150
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
  object OKBtn: TBitBtn
    Left = 8
    Top = 150
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
    TabOrder = 6
    OnClick = OKBtnClick
    IsControl = True
  end
  object CheckBox2: TCheckBox
    Left = 163
    Top = 88
    Width = 121
    Height = 17
    Caption = 'Table with file names'
    TabOrder = 7
  end
end
