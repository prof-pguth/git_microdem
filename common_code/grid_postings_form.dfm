object grid_posting_options: Tgrid_posting_options
  Left = 398
  Top = 338
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'DEM Grid Options'
  ClientHeight = 218
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object RedrawSpeedButton12: TSpeedButton
    Left = 177
    Top = 185
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
    Left = 16
    Top = 120
    Width = 66
    Height = 13
    Caption = 'Label offset, x'
  end
  object Label2: TLabel
    Left = 143
    Top = 120
    Width = 5
    Height = 13
    Caption = 'y'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 177
    Height = 81
    Caption = 'Drawing selection'
    ItemIndex = 0
    Items.Strings = (
      'Draw single time'
      'Always redraw on map'
      'Remove')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 95
    Width = 129
    Height = 17
    Caption = 'Label point elevations'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object BitBtn1: TBitBtn
    Left = 96
    Top = 154
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object OKBtn: TBitBtn
    Left = 77
    Top = 185
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
    Top = 154
    Width = 66
    Height = 25
    Caption = '123.001'
    TabOrder = 4
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 8
    Top = 185
    Width = 46
    Height = 25
    Caption = 'Font'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 96
    Top = 120
    Width = 41
    Height = 21
    TabOrder = 6
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 161
    Top = 120
    Width = 41
    Height = 21
    TabOrder = 7
    OnChange = Edit2Change
  end
end
