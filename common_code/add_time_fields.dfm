object TimeFieldsForm: TTimeFieldsForm
  Left = 0
  Top = 0
  BorderIcons = [biMinimize]
  BorderStyle = bsDialog
  Caption = 'Add KML Time Fields'
  ClientHeight = 262
  ClientWidth = 219
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 41
    Top = 250
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
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 137
    Top = 250
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 16
    Width = 177
    Height = 145
    Caption = 'Time intervals'
    ItemIndex = 0
    Items.Strings = (
      'Month and years'
      'Months in single year'
      'Years'
      'Days'
      'Full time')
    TabOrder = 2
  end
  object RadioGroup2: TRadioGroup
    Left = 24
    Top = 176
    Width = 121
    Height = 56
    Caption = 'Time zone'
    ItemIndex = 0
    Items.Strings = (
      'UTC=GMT=Zulu'
      'Specify')
    TabOrder = 3
    OnClick = RadioGroup2Click
  end
  object Edit1: TEdit
    Left = 151
    Top = 192
    Width = 50
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = '-5'
  end
end
