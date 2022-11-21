object sunrisepicker: Tsunrisepicker
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Sunrise options'
  ClientHeight = 346
  ClientWidth = 170
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 200
    Width = 19
    Height = 13
    Caption = 'Day'
  end
  object Label2: TLabel
    Left = 16
    Top = 224
    Width = 30
    Height = 13
    Caption = 'Month'
  end
  object Label3: TLabel
    Left = 16
    Top = 248
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Label4: TLabel
    Left = 16
    Top = 280
    Width = 86
    Height = 13
    Caption = 'Difference to UTC'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 137
    Height = 105
    Caption = 'Sunlight'
    ItemIndex = 0
    Items.Strings = (
      'Sunrise/sunset'
      'Civil twilight'
      'Nautical twilight'
      'Astronomical twilight')
    TabOrder = 0
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 119
    Width = 137
    Height = 57
    Caption = 'Time of day'
    ItemIndex = 0
    Items.Strings = (
      'Morning'
      'Evening')
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 72
    Top = 200
    Width = 81
    Height = 21
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 72
    Top = 224
    Width = 81
    Height = 21
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 72
    Top = 248
    Width = 81
    Height = 21
    TabOrder = 4
  end
  object Edit4: TEdit
    Left = 111
    Top = 275
    Width = 41
    Height = 21
    TabOrder = 5
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 318
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
  object HelpBtn: TBitBtn
    Left = 91
    Top = 320
    Width = 77
    Height = 27
    Enabled = False
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 7
    IsControl = True
  end
end
