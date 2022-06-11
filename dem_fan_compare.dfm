object FanCompareForm: TFanCompareForm
  Left = 380
  Top = 290
  Caption = 'Fan Algorithm Comparison'
  ClientHeight = 273
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 429
    Height = 200
    Align = alClient
    ColCount = 1
    FixedCols = 0
    TabOrder = 0
    OnClick = StringGrid1Click
    OnMouseMove = StringGrid1MouseMove
    ColWidths = (
      425)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object Panel1: TPanel
    Left = 0
    Top = 200
    Width = 429
    Height = 73
    Align = alBottom
    TabOrder = 1
    object RadioGroup1: TRadioGroup
      Left = 16
      Top = 8
      Width = 113
      Height = 57
      Caption = 'Fans to use'
      Columns = 3
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object OKBtn: TBitBtn
      Left = 176
      Top = 22
      Width = 77
      Height = 27
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = OKBtnClick
      IsControl = True
    end
  end
end
