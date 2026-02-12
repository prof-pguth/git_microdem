object GridMaskOptForm: TGridMaskOptForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Grid mask options'
  ClientHeight = 252
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label3: TLabel
    Left = 31
    Top = 79
    Width = 59
    Height = 13
    Caption = 'Opacity (%)'
  end
  object Label4: TLabel
    Left = 31
    Top = 115
    Width = 50
    Height = 13
    Caption = 'Mask code'
  end
  object BitBtn1: TBitBtn
    Left = 31
    Top = 48
    Width = 106
    Height = 25
    Caption = 'Mask color'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object CheckBox1: TCheckBox
    Left = 31
    Top = 25
    Width = 170
    Height = 17
    Caption = 'Number of matches (not code)'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object Edit1: TEdit
    Left = 96
    Top = 79
    Width = 32
    Height = 21
    TabOrder = 2
    Text = 'Edit3'
  end
  object Edit4: TEdit
    Left = 96
    Top = 112
    Width = 41
    Height = 21
    TabOrder = 3
    Text = '1'
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 139
    Width = 199
    Height = 49
    Caption = 'Mask map'
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      'Separate'
      'Overlaid'
      'Both')
    TabOrder = 4
    OnClick = RadioGroup3Click
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 214
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
    TabOrder = 5
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 130
    Top = 213
    Width = 77
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
