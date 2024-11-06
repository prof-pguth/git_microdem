object InputArrays: TInputArrays
  Left = 263
  Top = 115
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Contour Boundaries'
  ClientHeight = 290
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 297
    Height = 257
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 48
    Top = 268
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 140
    Top = 268
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 224
    Top = 268
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object StringGrid1: TStringGrid
    Left = 40
    Top = 16
    Width = 225
    Height = 241
    ColCount = 2
    DefaultColWidth = 100
    RowCount = 20
    TabOrder = 3
    OnClick = StringGrid1Click
    OnDblClick = StringGrid1DblClick
    OnMouseMove = StringGrid1MouseMove
    ColWidths = (
      100
      100)
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
end
