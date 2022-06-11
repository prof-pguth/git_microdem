object MakeSatHeader: TMakeSatHeader
  Left = 337
  Top = 107
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Make Satellite Header'
  ClientHeight = 324
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 184
    Width = 23
    Height = 13
    Caption = 'Cols:'
  end
  object Label2: TLabel
    Left = 16
    Top = 208
    Width = 30
    Height = 13
    Caption = 'Rows:'
  end
  object Label3: TLabel
    Left = 16
    Top = 232
    Width = 33
    Height = 13
    Caption = 'Bands:'
  end
  object Label4: TLabel
    Left = 16
    Top = 160
    Width = 34
    Height = 13
    Caption = 'Scene:'
  end
  object Label5: TLabel
    Left = 16
    Top = 136
    Width = 19
    Height = 13
    Caption = 'File:'
  end
  object Memo1: TMemo
    Left = 16
    Top = 16
    Width = 737
    Height = 89
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 56
    Top = 184
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object Edit2: TEdit
    Left = 56
    Top = 208
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object Edit3: TEdit
    Left = 56
    Top = 232
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '1'
    OnChange = Edit3Change
  end
  object StringGrid1: TStringGrid
    Left = 216
    Top = 128
    Width = 537
    Height = 161
    ColCount = 3
    DefaultColWidth = 160
    RowCount = 2
    TabOrder = 4
    OnClick = StringGrid1Click
    OnMouseMove = StringGrid1MouseMove
    ColWidths = (
      160
      160
      160)
    RowHeights = (
      24
      24)
  end
  object Edit4: TEdit
    Left = 56
    Top = 160
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'Scene Name'
  end
  object BitBtn1: TBitBtn
    Left = 768
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object OKBtn: TBitBtn
    Left = 32
    Top = 268
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 7
    OnClick = OKBtnClick
    IsControl = True
  end
  object Edit5: TEdit
    Left = 56
    Top = 136
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'FileName'
  end
end
