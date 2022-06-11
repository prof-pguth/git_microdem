object PointRoamForm: TPointRoamForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Point Roam '
  ClientHeight = 297
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 224
    Top = 32
    Width = 44
    Height = 13
    Caption = 'Elevation'
  end
  object Label2: TLabel
    Left = 216
    Top = 128
    Width = 26
    Height = 13
    Caption = 'Slope'
  end
  object Label3: TLabel
    Left = 224
    Top = 216
    Width = 33
    Height = 13
    Caption = 'Aspect'
  end
  object Label4: TLabel
    Left = 24
    Top = 269
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 8
    Width = 201
    Height = 81
    ColCount = 3
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    TabOrder = 0
    ColWidths = (
      64
      64
      64)
    RowHeights = (
      24
      24
      24)
  end
  object StringGrid2: TStringGrid
    Left = 8
    Top = 95
    Width = 201
    Height = 81
    ColCount = 3
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    TabOrder = 1
    ColWidths = (
      64
      64
      64)
    RowHeights = (
      24
      24
      24)
  end
  object StringGrid3: TStringGrid
    Left = 8
    Top = 182
    Width = 201
    Height = 81
    ColCount = 3
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    TabOrder = 2
    ColWidths = (
      64
      64
      64)
    RowHeights = (
      24
      24
      24)
  end
end
