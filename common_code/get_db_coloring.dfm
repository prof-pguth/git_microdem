object DBColorForm: TDBColorForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DBColorForm'
  ClientHeight = 313
  ClientWidth = 314
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
    Left = 8
    Top = 52
    Width = 45
    Height = 13
    Caption = 'Min value'
  end
  object Label2: TLabel
    Left = 8
    Top = 25
    Width = 49
    Height = 13
    Caption = 'Max value'
  end
  object Image1: TImage
    Left = 8
    Top = 179
    Width = 297
    Height = 81
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 199
    Top = 287
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object Edit1: TEdit
    Left = 80
    Top = 49
    Width = 81
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 80
    Top = 22
    Width = 81
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object RadioGroup1: TRadioGroup
    Left = 167
    Top = 8
    Width = 129
    Height = 105
    Caption = 'Color scheme'
    Items.Strings = (
      'Gray scale'
      'Rainbow colors'
      'Terrain colors'
      'Spectrum colors'
      'Chloropleth colors')
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object OKBtn: TBitBtn
    Left = 7
    Top = 266
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
    Left = 103
    Top = 266
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
  object ComboBox1: TComboBox
    Left = 8
    Top = 119
    Width = 202
    Height = 21
    TabOrder = 5
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
  end
  object BitBtn1: TBitBtn
    Left = 216
    Top = 119
    Width = 33
    Height = 25
    Caption = '<'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 255
    Top = 119
    Width = 33
    Height = 25
    Caption = '>'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object CheckBox1: TCheckBox
    Left = 199
    Top = 264
    Width = 106
    Height = 17
    Caption = 'Immediate redraw'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object BitBtn3: TBitBtn
    Left = 40
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Verify range'
    TabOrder = 9
    OnClick = BitBtn3Click
  end
  object CheckBox2: TCheckBox
    Left = 64
    Top = 146
    Width = 97
    Height = 17
    Caption = 'Reverse colors'
    TabOrder = 10
    OnClick = CheckBox2Click
  end
end
