object leg_opts_form: Tleg_opts_form
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 258
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 56
    Top = 152
    Width = 37
    Height = 13
    Caption = 'Bar size'
  end
  object Label2: TLabel
    Left = 56
    Top = 176
    Width = 39
    Height = 13
    Caption = 'Tick size'
  end
  object Label3: TLabel
    Left = 56
    Top = 200
    Width = 46
    Height = 13
    Caption = 'x location'
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 24
    Width = 257
    Height = 57
    Caption = 'Location'
    Columns = 3
    Items.Strings = (
      'NW corner'
      'SW corner'
      'N center'
      'S center'
      'NE corner'
      'SE corner')
    TabOrder = 0
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 223
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
    TabOrder = 1
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 105
    Top = 223
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
  object CheckBox1: TCheckBox
    Left = 16
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Include on maps'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 16
    Top = 87
    Width = 85
    Height = 57
    Caption = 'Orientation'
    Items.Strings = (
      'Vertical'
      'Horizontal')
    TabOrder = 4
  end
  object BitBtn1: TBitBtn
    Left = 198
    Top = 87
    Width = 51
    Height = 25
    Caption = 'Font'
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object RadioGroup3: TRadioGroup
    Left = 107
    Top = 87
    Width = 75
    Height = 57
    Caption = 'Size'
    Items.Strings = (
      'Small'
      'Medium')
    TabOrder = 6
  end
  object Edit1: TEdit
    Left = 128
    Top = 152
    Width = 54
    Height = 21
    TabOrder = 7
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 128
    Top = 176
    Width = 54
    Height = 21
    TabOrder = 8
    Text = 'Edit2'
  end
  object Edit3: TEdit
    Left = 128
    Top = 196
    Width = 54
    Height = 21
    TabOrder = 9
    Text = 'Edit3'
  end
end
