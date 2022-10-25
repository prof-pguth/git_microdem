object MaskOptsForm: TMaskOptsForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Masking Options'
  ClientHeight = 220
  ClientWidth = 222
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
    Top = 16
    Width = 70
    Height = 13
    Caption = 'Buffer size (m)'
  end
  object Edit1: TEdit
    Left = 112
    Top = 16
    Width = 65
    Height = 21
    TabOrder = 0
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 43
    Width = 185
    Height = 57
    Caption = 'Buffer options--remove'
    Items.Strings = (
      'Points near features'
      'Points away from features')
    TabOrder = 1
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 221
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
    Left = 100
    Top = 221
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
  object CheckBox1: TCheckBox
    Left = 16
    Top = 106
    Width = 97
    Height = 17
    Caption = 'All TIGER roads'
    TabOrder = 4
  end
  object BitBtn1: TBitBtn
    Left = 119
    Top = 109
    Width = 82
    Height = 25
    Caption = 'TIGER options'
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 185
    Width = 97
    Height = 17
    Caption = 'Show mask'
    TabOrder = 6
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 140
    Width = 145
    Height = 17
    Caption = 'Treat lines as polygons'
    TabOrder = 7
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 16
    Top = 163
    Width = 201
    Height = 17
    Caption = 'Treat polygons as lines (perimeter)'
    TabOrder = 8
    OnClick = CheckBox4Click
  end
end
