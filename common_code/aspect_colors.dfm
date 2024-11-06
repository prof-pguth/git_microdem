object AspectMapColors: TAspectMapColors
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Aspect Map Colors'
  ClientHeight = 308
  ClientWidth = 223
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object RedrawSpeedButton12: TSpeedButton
    Left = 120
    Top = 239
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
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 169
    Height = 225
    Caption = 'Color options'
    Items.Strings = (
      '8 cardinal directions'
      'Terrain color scale'
      'Spectrum colors'
      'Rainbow colors'
      'CET-C1'
      'CET-C2'
      'CET-C6'
      'CET-C7')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object OKBtn: TButton
    Left = 20
    Top = 239
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKBtnClick
  end
end
