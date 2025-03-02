object CorrelationMatrixOptionsForm: TCorrelationMatrixOptionsForm
  Left = 0
  Top = 0
  Caption = 'Correlation matrix options'
  ClientHeight = 329
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 15
  object RedrawSpeedButton12: TSpeedButton
    Left = 281
    Top = 159
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
  object Label1: TLabel
    Left = 32
    Top = 256
    Width = 47
    Height = 15
    Caption = 'UL string'
  end
  object Label2: TLabel
    Left = 32
    Top = 296
    Width = 58
    Height = 15
    Caption = 'Perfect R'
  end
  object RadioGroup1: TRadioGroup
    Left = 40
    Top = 40
    Width = 137
    Height = 81
    Caption = 'Color palette'
    Items.Strings = (
      'Red to green'
      'Blue to orange')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 208
    Top = 40
    Width = 161
    Height = 81
    Caption = 'Color bins'
    Items.Strings = (
      'Equal numbers per bin'
      'Equal size per bin')
    TabOrder = 1
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 40
    Top = 143
    Width = 137
    Height = 81
    Caption = 'Decimals for legend'
    Columns = 3
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
    TabOrder = 2
    OnClick = RadioGroup3Click
  end
  object Edit1: TEdit
    Left = 96
    Top = 253
    Width = 321
    Height = 23
    TabOrder = 3
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 96
    Top = 288
    Width = 121
    Height = 23
    TabOrder = 4
    Text = '0.9999'
    OnChange = Edit2Change
  end
end
