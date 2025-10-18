object PickDiagNormForm: TPickDiagNormForm
  Left = 0
  Top = 0
  Caption = 'Diagonals and Geographic Grids'
  ClientHeight = 326
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 32
    Width = 185
    Height = 233
    Caption = 'Spacing normalization'
    Items.Strings = (
      '   nmNone;'
      '   nmEastWest '
      '   nmNorthSouth '
      '   nmAvgSpace;'
      '   nmBilinearDiagonals'
      '   nmInterpolateDiagonal;'
      '   nmInterpolate45'
      '   nm30m')
    TabOrder = 0
  end
  object OKBtn: TBitBtn
    Left = 56
    Top = 284
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
end
