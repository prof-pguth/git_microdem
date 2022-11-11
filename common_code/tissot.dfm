object TissotOpts: TTissotOpts
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Tissot Indicatrix Options'
  ClientHeight = 248
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 62
    Width = 54
    Height = 13
    Caption = 'Lat spacing'
  end
  object Label2: TLabel
    Left = 16
    Top = 94
    Width = 62
    Height = 13
    Caption = 'Long spacing'
  end
  object Label3: TLabel
    Left = 8
    Top = 120
    Width = 81
    Height = 13
    Caption = 'Diameter (pixels)'
  end
  object Label4: TLabel
    Left = 192
    Top = 70
    Width = 61
    Height = 13
    Caption = 'Pixel spacing'
  end
  object Label5: TLabel
    Left = 173
    Top = 179
    Width = 61
    Height = 13
    Caption = 'h&&k decimals'
  end
  object SpeedButton1: TSpeedButton
    Left = 264
    Top = 215
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
    OnClick = SpeedButton1Click
  end
  object HelpBtn: TBitBtn
    Left = 112
    Top = 213
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 213
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
  object Edit1: TEdit
    Left = 112
    Top = 67
    Width = 65
    Height = 21
    TabOrder = 2
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 112
    Top = 94
    Width = 65
    Height = 21
    TabOrder = 3
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 104
    Top = 121
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 148
    Width = 70
    Height = 25
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object Edit4: TEdit
    Left = 264
    Top = 67
    Width = 57
    Height = 21
    TabOrder = 6
    OnChange = Edit4Change
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 313
    Height = 41
    Caption = 'Indicatrix spacing'
    Columns = 2
    Items.Strings = (
      'By lat/long graticule'
      'By screen pixels')
    TabOrder = 7
    OnClick = RadioGroup1Click
  end
  object CheckBox2: TCheckBox
    Left = 40
    Top = 179
    Width = 97
    Height = 17
    Caption = 'Show h and k'
    TabOrder = 8
  end
  object Edit5: TEdit
    Left = 241
    Top = 174
    Width = 31
    Height = 21
    TabOrder = 9
  end
  object CheckBox1: TCheckBox
    Left = 168
    Top = 152
    Width = 137
    Height = 17
    Caption = 'Simple cylindrical spacing'
    TabOrder = 10
  end
end
