object grid_posting_options: Tgrid_posting_options
  Left = 398
  Top = 338
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DEM Grid Options'
  ClientHeight = 187
  ClientWidth = 177
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 145
    Height = 81
    Caption = 'Drawing selection'
    ItemIndex = 0
    Items.Strings = (
      'Draw single time'
      'Always redraw on map'
      'Remove')
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 95
    Width = 129
    Height = 17
    Caption = 'Label point elevations'
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 96
    Top = 118
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object OKBtn: TBitBtn
    Left = 60
    Top = 152
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 118
    Width = 66
    Height = 25
    Caption = '123.001'
    TabOrder = 4
    OnClick = BitBtn2Click
  end
end
