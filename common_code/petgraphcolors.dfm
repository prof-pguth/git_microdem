object graphcolorsform: Tgraphcolorsform
  Left = 461
  Top = 306
  Caption = 'Graph Symbols and Lines'
  ClientHeight = 191
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 221
    Top = 162
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 304
    Top = 162
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'BitBtn4'
    TabOrder = 2
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 39
    Width = 121
    Height = 25
    Caption = 'BitBtn4'
    TabOrder = 3
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 8
    Top = 70
    Width = 121
    Height = 25
    Caption = 'BitBtn4'
    TabOrder = 4
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 8
    Top = 101
    Width = 121
    Height = 25
    Caption = 'BitBtn4'
    TabOrder = 5
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 8
    Top = 132
    Width = 121
    Height = 25
    Caption = 'BitBtn4'
    TabOrder = 6
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 8
    Top = 163
    Width = 121
    Height = 25
    Caption = 'BitBtn4'
    TabOrder = 7
    OnClick = BitBtn9Click
  end
  object Panel1: TPanel
    Left = 153
    Top = 8
    Width = 145
    Height = 106
    TabOrder = 8
    object CheckBox3: TCheckBox
      Left = 8
      Top = 8
      Width = 57
      Height = 17
      Caption = 'Points'
      TabOrder = 0
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 8
      Top = 39
      Width = 50
      Height = 17
      Caption = 'Lines'
      TabOrder = 1
      OnClick = CheckBox4Click
    end
    object SymBitBtn: TBitBtn
      Left = 64
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 2
      OnClick = SymBitBtnClick
    end
    object LineBitBtn: TBitBtn
      Left = 64
      Top = 39
      Width = 75
      Height = 25
      TabOrder = 3
    end
    object BitBtn10: TBitBtn
      Left = 24
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Done'
      TabOrder = 4
      OnClick = BitBtn10Click
    end
  end
  object PlotScaledSymbolsButton: TBitBtn
    Left = 144
    Top = 161
    Width = 71
    Height = 28
    Caption = 'Redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    TabOrder = 9
    OnClick = PlotScaledSymbolsButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 217
    Top = 120
    Width = 128
    Height = 17
    Caption = 'Point symbols all series'
    TabOrder = 10
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 217
    Top = 138
    Width = 97
    Height = 17
    Caption = 'Lines all series'
    TabOrder = 11
    OnClick = CheckBox2Click
  end
end
