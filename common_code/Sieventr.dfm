object SieveEntryForm: TSieveEntryForm
  Left = 263
  Top = 115
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Sieve Analysis Data Entry'
  ClientHeight = 368
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 409
    Height = 257
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 48
    Top = 332
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
    Top = 332
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
    Top = 332
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
    Width = 345
    Height = 241
    ColCount = 4
    DefaultColWidth = 75
    RowCount = 20
    TabOrder = 3
    OnClick = StringGrid1Click
    OnDblClick = StringGrid1DblClick
    OnMouseMove = StringGrid1MouseMove
    ColWidths = (
      75
      75
      75
      75)
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
  object CheckBox1: TCheckBox
    Left = 104
    Top = 288
    Width = 113
    Height = 17
    Caption = 'Change sieves'
    TabOrder = 4
  end
end
