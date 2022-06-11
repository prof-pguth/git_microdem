object NewFieldForm: TNewFieldForm
  Left = 920
  Top = 145
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Set Data Base Fields'
  ClientHeight = 235
  ClientWidth = 214
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 18
    Top = 20
    Width = 53
    Height = 13
    Caption = '&Field Name'
  end
  object Label1: TLabel
    Left = 16
    Top = 136
    Width = 58
    Height = 13
    Caption = 'Field Length'
  end
  object Label3: TLabel
    Left = 16
    Top = 160
    Width = 43
    Height = 13
    Caption = 'Decimals'
  end
  object Edit1: TEdit
    Left = 88
    Top = 21
    Width = 89
    Height = 21
    CharCase = ecUpperCase
    TabOrder = 0
    Text = 'NEW_FIELD'
  end
  object Edit2: TEdit
    Left = 88
    Top = 133
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '24'
  end
  object Edit3: TEdit
    Left = 88
    Top = 160
    Width = 89
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object RadioGroup1: TRadioGroup
    Left = 18
    Top = 48
    Width = 87
    Height = 65
    Caption = 'Field type'
    ItemIndex = 0
    Items.Strings = (
      'String'
      'Integer'
      'Float')
    TabOrder = 3
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 200
    Width = 56
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
  object CancelBtn: TBitBtn
    Left = 70
    Top = 200
    Width = 65
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = CancelBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 141
    Top = 200
    Width = 58
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
end
