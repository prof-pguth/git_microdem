object ContourOptions: TContourOptions
  Left = 172
  Top = 142
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Contour Map Options'
  ClientHeight = 382
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 249
    Height = 335
    Shape = bsFrame
    IsControl = True
  end
  object Label6: TLabel
    Left = 20
    Top = 20
    Width = 92
    Height = 13
    Caption = 'Contour Interval'
  end
  object Label9: TLabel
    Left = 168
    Top = 240
    Width = 39
    Height = 13
    Caption = 'Label9'
  end
  object Label1: TLabel
    Left = 56
    Top = 240
    Width = 107
    Height = 13
    Caption = 'Contour line width:'
  end
  object Label2: TLabel
    Left = 56
    Top = 296
    Width = 121
    Height = 13
    Caption = 'Index contour width: '
  end
  object Label3: TLabel
    Left = 184
    Top = 296
    Width = 39
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 200
    Top = 24
    Width = 5
    Height = 13
  end
  object Label5: TLabel
    Left = 213
    Top = 259
    Width = 22
    Height = 13
    Caption = '100'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 270
    Top = 349
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
  object OKBtn: TBitBtn
    Left = 10
    Top = 349
    Width = 55
    Height = 27
    Caption = '&OK'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 71
    Top = 349
    Width = 66
    Height = 27
    Caption = '&Cancel'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 143
    Top = 349
    Width = 58
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit1: TEdit
    Left = 134
    Top = 21
    Width = 60
    Height = 21
    TabOrder = 3
    OnChange = Edit1Change
  end
  object UpDown1: TUpDown
    Left = 32
    Top = 232
    Width = 16
    Height = 24
    Max = 5
    TabOrder = 4
    OnClick = UpDown1Click
  end
  object BitBtn1: TBitBtn
    Left = 144
    Top = 48
    Width = 100
    Height = 25
    Caption = 'Regular'
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 144
    Top = 112
    Width = 100
    Height = 25
    Caption = 'Index'
    TabOrder = 6
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 144
    Top = 80
    Width = 100
    Height = 25
    Caption = 'High index'
    TabOrder = 7
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 144
    Top = 144
    Width = 100
    Height = 25
    Caption = 'Low index'
    TabOrder = 8
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 144
    Top = 176
    Width = 100
    Height = 25
    Caption = 'Zero'
    TabOrder = 9
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 24
    Top = 176
    Width = 100
    Height = 25
    Caption = 'Overlay'
    TabOrder = 10
    OnClick = BitBtn6Click
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 48
    Width = 113
    Height = 105
    Caption = 'Contour colors'
    Items.Strings = (
      'Specified'
      'Single'
      'ChromaDepth'
      'Terrain ')
    TabOrder = 11
    OnClick = RadioGroup1Click
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 264
    Width = 113
    Height = 17
    Caption = 'Label contours'
    TabOrder = 12
    OnClick = CheckBox1Click
  end
  object UpDown2: TUpDown
    Left = 34
    Top = 288
    Width = 16
    Height = 24
    Max = 8
    TabOrder = 13
    OnClick = UpDown2Click
  end
  object BitBtn7: TBitBtn
    Left = 143
    Top = 259
    Width = 58
    Height = 25
    Caption = 'Font'
    TabOrder = 14
    OnClick = BitBtn7Click
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 318
    Width = 211
    Height = 17
    Caption = 'Export contour lines in shape file'
    Enabled = False
    TabOrder = 15
    OnClick = CheckBox2Click
  end
  object BitBtn8: TBitBtn
    Left = 143
    Top = 209
    Width = 75
    Height = 25
    Caption = 'All'
    TabOrder = 16
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 208
    Top = 349
    Width = 56
    Height = 27
    Caption = 'Defaults'
    TabOrder = 17
    OnClick = BitBtn9Click
  end
  object ColorDialog1: TColorDialog
    Left = 285
    Top = 77
  end
end
