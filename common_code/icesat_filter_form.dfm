object Icesat_filter: TIcesat_filter
  Left = 0
  Top = 0
  Caption = 'Icesat filter'
  ClientHeight = 430
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 296
    Width = 107
    Height = 13
    Caption = 'Box size (arc seconds)'
  end
  object Label2: TLabel
    Left = 61
    Top = 328
    Width = 52
    Height = 13
    Caption = 'Min cell pts'
  end
  object CheckBox1: TCheckBox
    Left = 18
    Top = 8
    Width = 169
    Height = 17
    Caption = 'Filter ICEsat 2 data'
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 121
    Top = 293
    Width = 66
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 105
    Height = 177
    Caption = 'Use beam'
    TabOrder = 2
    object CheckBox2: TCheckBox
      Left = 10
      Top = 24
      Width = 97
      Height = 17
      Caption = 'GT1R = 1'
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 10
      Top = 47
      Width = 97
      Height = 17
      Caption = 'GT1L = 2'
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Left = 10
      Top = 70
      Width = 97
      Height = 17
      Caption = 'GT2R = 3'
      TabOrder = 2
    end
    object CheckBox5: TCheckBox
      Left = 10
      Top = 93
      Width = 97
      Height = 17
      Caption = 'GT2L = 4'
      TabOrder = 3
    end
    object CheckBox6: TCheckBox
      Left = 10
      Top = 116
      Width = 97
      Height = 17
      Caption = 'GT3R = 5'
      TabOrder = 4
    end
    object CheckBox7: TCheckBox
      Left = 10
      Top = 139
      Width = 97
      Height = 17
      Caption = 'GT3L = 6'
      TabOrder = 5
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 121
    Top = 40
    Width = 185
    Height = 33
    Caption = 'GT1R min confidence'
    Columns = 5
    Items.Strings = (
      '0 '
      '1'
      '2'
      '3'
      '4')
    TabOrder = 3
  end
  object RadioGroup2: TRadioGroup
    Left = 121
    Top = 79
    Width = 185
    Height = 33
    Caption = 'GT1L min confidence'
    Columns = 5
    Items.Strings = (
      '0 '
      '1'
      '2'
      '3'
      '4')
    TabOrder = 4
  end
  object RadioGroup3: TRadioGroup
    Left = 121
    Top = 118
    Width = 185
    Height = 33
    Caption = 'GT2R min confidence'
    Columns = 5
    Items.Strings = (
      '0 '
      '1'
      '2'
      '3'
      '4')
    TabOrder = 5
  end
  object RadioGroup4: TRadioGroup
    Left = 121
    Top = 157
    Width = 185
    Height = 33
    Caption = 'GT2L min confidence'
    Columns = 5
    Items.Strings = (
      '0 '
      '1'
      '2'
      '3'
      '4')
    TabOrder = 6
  end
  object RadioGroup5: TRadioGroup
    Left = 121
    Top = 196
    Width = 185
    Height = 33
    Caption = 'GT3R min confidence'
    Columns = 5
    Items.Strings = (
      '0 '
      '1'
      '2'
      '3'
      '4')
    TabOrder = 7
  end
  object RadioGroup6: TRadioGroup
    Left = 121
    Top = 235
    Width = 185
    Height = 33
    Caption = 'GT3L min confidence'
    Columns = 5
    Items.Strings = (
      '0 '
      '1'
      '2'
      '3'
      '4')
    TabOrder = 8
  end
  object OKBtn: TBitBtn
    Left = 17
    Top = 395
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
    TabOrder = 9
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 100
    Top = 395
    Width = 77
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 10
    IsControl = True
  end
  object Edit2: TEdit
    Left = 119
    Top = 325
    Width = 68
    Height = 21
    TabOrder = 11
    Text = 'Edit2'
  end
end
