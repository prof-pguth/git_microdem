object CSVFileImportForm: TCSVFileImportForm
  Left = 467
  Top = 244
  Caption = 'CSV file import'
  ClientHeight = 461
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 504
    Height = 445
    Align = alClient
    FixedCols = 0
    TabOrder = 0
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 445
    Width = 642
    Height = 16
    Align = alBottom
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 504
    Top = 0
    Width = 138
    Height = 445
    Align = alRight
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 14
      Top = 56
      Width = 75
      Height = 25
      Caption = 'Write'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 14
      Top = 118
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 14
      Top = 87
      Width = 75
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 2
      OnClick = BitBtn3Click
      IsControl = True
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 129
      Height = 17
      Caption = 'Check missing values'
      TabOrder = 3
    end
  end
end
