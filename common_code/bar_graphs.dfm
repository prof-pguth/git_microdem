object bargraphform: Tbargraphform
  Left = 0
  Top = 0
  ClientHeight = 214
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RedrawSpeedButton12: TSpeedButton
    Left = 8
    Top = 8
    Width = 82
    Height = 25
    Hint = 'Force redraw'
    Caption = 'Bar graph'
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
  object Label1: TLabel
    Left = 16
    Top = 88
    Width = 49
    Height = 13
    Caption = 'Bar height'
  end
  object Label2: TLabel
    Left = 16
    Top = 120
    Width = 79
    Height = 13
    Caption = 'Group separator'
  end
  object Label3: TLabel
    Left = 24
    Top = 160
    Width = 3
    Height = 13
  end
  object SpeedButton1: TSpeedButton
    Left = 264
    Top = 8
    Width = 105
    Height = 25
    Hint = 'Force redraw'
    Caption = 'COLOR field'
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
  object SpeedButton2: TSpeedButton
    Left = 264
    Top = 63
    Width = 105
    Height = 25
    Hint = 'Force redraw'
    Caption = 'SYMBOLS in DB'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = SpeedButton2Click
  end
  object Edit1: TEdit
    Left = 104
    Top = 85
    Width = 57
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 104
    Top = 120
    Width = 57
    Height = 21
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 24
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Form height'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object ComboBox1: TComboBox
    Left = 96
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 3
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
  end
  object BitBtn2: TBitBtn
    Left = 128
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Legend'
    TabOrder = 4
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 224
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 152
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Over_under'
    TabOrder = 6
    OnClick = BitBtn4Click
  end
  object CheckBox1: TCheckBox
    Left = 320
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Log-log'
    TabOrder = 7
  end
end
