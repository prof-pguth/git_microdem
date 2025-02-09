object RegOptsForm: TRegOptsForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Optima in region options'
  ClientHeight = 265
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  TextHeight = 13
  object Grid: TLabel
    Left = 8
    Top = 93
    Width = 164
    Height = 13
    Caption = 'Region size, Grid posts per column'
  end
  object Label2: TLabel
    Left = 8
    Top = 112
    Width = 149
    Height = 13
    Caption = 'Region size, Grid posts per row'
    Enabled = False
  end
  object Label3: TLabel
    Left = 8
    Top = 145
    Width = 103
    Height = 13
    Caption = 'Points in earch region'
  end
  object Label6: TLabel
    Left = 152
    Top = 16
    Width = 3
    Height = 13
  end
  object Label7: TLabel
    Left = 152
    Top = 45
    Width = 3
    Height = 13
  end
  object Label8: TLabel
    Left = 143
    Top = 69
    Width = 69
    Height = 13
    Caption = 'Box radius (m)'
  end
  object Label9: TLabel
    Left = 222
    Top = 93
    Width = 3
    Height = 13
  end
  object Label10: TLabel
    Left = 222
    Top = 112
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 40
    Top = 176
    Width = 3
    Height = 13
  end
  object Edit1: TEdit
    Left = 175
    Top = 88
    Width = 41
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 175
    Top = 115
    Width = 41
    Height = 21
    Enabled = False
    TabOrder = 1
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 117
    Top = 142
    Width = 41
    Height = 21
    TabOrder = 2
    OnChange = Edit3Change
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 220
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
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 95
    Top = 220
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = HelpBtnClick
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 129
    Height = 79
    Caption = 'Area to use'
    ItemIndex = 0
    Items.Strings = (
      'Entire DEM'
      'Current map area'
      'Box around point')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object Edit6: TEdit
    Left = 227
    Top = 66
    Width = 49
    Height = 21
    TabOrder = 6
  end
  object CheckBox5: TCheckBox
    Left = 204
    Top = 142
    Width = 97
    Height = 17
    Caption = 'Same'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = CheckBox5Click
  end
end
