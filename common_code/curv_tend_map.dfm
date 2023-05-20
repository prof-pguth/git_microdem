object CurvMapForm: TCurvMapForm
  Left = 0
  Top = 0
  Caption = 'Curvature Categories'
  ClientHeight = 299
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 32
    Width = 111
    Height = 13
    Caption = 'Curvature flatness limit'
  end
  object Image1: TImage
    Left = 8
    Top = 116
    Width = 500
    Height = 200
  end
  object Edit1: TEdit
    Left = 141
    Top = 29
    Width = 68
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object OKBtn: TBitBtn
    Left = 111
    Top = 83
    Width = 73
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 190
    Top = 84
    Width = 77
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 83
    Width = 89
    Height = 27
    Caption = 'Draw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object TrackBar1: TTrackBar
    Left = 344
    Top = 32
    Width = 150
    Height = 45
    Max = 100
    Frequency = 10
    Position = 50
    TabOrder = 4
  end
  object RadioGroup1: TRadioGroup
    Left = 230
    Top = 16
    Width = 99
    Height = 61
    Caption = 'Color scheme'
    ItemIndex = 0
    Items.Strings = (
      'Opacity'
      'IHS merge')
    TabOrder = 5
  end
end
