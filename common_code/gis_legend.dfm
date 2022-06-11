object GIS_leg_form: TGIS_leg_form
  Left = 0
  Top = 0
  Caption = 'PetDrawGridForm'
  ClientHeight = 269
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DrawGrid1: TDrawGrid
    Left = 0
    Top = 0
    Width = 150
    Height = 269
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ColCount = 2
    FixedCols = 0
    ScrollBars = ssNone
    TabOrder = 0
    OnClick = DrawGrid1Click
    OnDrawCell = DrawGrid1DrawCell
    OnMouseMove = DrawGrid1MouseMove
    ColWidths = (
      64
      64)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object Panel1: TPanel
    Left = 150
    Top = 0
    Width = 257
    Height = 269
    Align = alRight
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      Left = 128
      Top = 119
      Width = 25
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333FFF33FFFFF33333300033000
        00333337773377777333333330333300033333337FF33777F333333330733300
        0333333377FFF777F33333333700000073333333777777773333333333033000
        3333333337FF777F333333333307300033333333377F777F3333333333703007
        33333333377F7773333333333330000333333333337777F33333333333300003
        33333333337777F3333333333337007333333333337777333333333333330033
        3333333333377333333333333333033333333333333733333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = SpeedButton1Click
    end
    object ClipboardSpeedButton: TSpeedButton
      Left = 97
      Top = 119
      Width = 25
      Height = 25
      Hint = 'Copy to clipboard'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFF0000000000FFFFF0FFFFFFFFFF0FFFF0FFFFFFFFFF0FFFF0F9FFFFFFF
        F0FFFF0FF9FFFFFFF0FF9999999FFFFFF0FF99999999FFFFF0FF99999999FFFF
        F0FF9999999FFFFFF0FFFF0FF9FFFFFFF0FFFF0F9FFFFFFFF0FFFF0FFFDDDDFF
        F0FFFFF000DDDD000FFFFFFFFDDFFDDFFFFFFFFFFFDDDDFFFFFF}
      OnClick = ClipboardSpeedButtonClick
    end
    object StringGrid1: TStringGrid
      Left = 16
      Top = 8
      Width = 209
      Height = 105
      ColCount = 2
      DefaultColWidth = 100
      RowCount = 4
      ScrollBars = ssNone
      TabOrder = 0
      OnClick = StringGrid1Click
      OnMouseMove = StringGrid1MouseMove
      ColWidths = (
        100
        100)
      RowHeights = (
        24
        24
        24
        24)
    end
    object Quick: TBitBtn
      Left = 16
      Top = 119
      Width = 75
      Height = 25
      Caption = 'Quick select'
      TabOrder = 1
      OnClick = QuickClick
    end
    object RadioGroup2: TRadioGroup
      Left = 16
      Top = 150
      Width = 121
      Height = 49
      Caption = 'Grid line width'
      Columns = 3
      ItemIndex = 1
      Items.Strings = (
        '0'
        '1'
        '2')
      TabOrder = 2
      OnClick = RadioGroup2Click
    end
    object Edit1: TEdit
      Left = 16
      Top = 205
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'Symbol'
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 16
      Top = 232
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'Description'
      OnChange = Edit2Change
    end
    object RadioGroup1: TRadioGroup
      Left = 143
      Top = 150
      Width = 99
      Height = 49
      Caption = 'Fill type'
      ItemIndex = 0
      Items.Strings = (
        'Standard'
        'Zipatone')
      TabOrder = 5
    end
    object BitBtn6: TBitBtn
      Left = 159
      Top = 119
      Width = 75
      Height = 25
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
      TabOrder = 6
      OnClick = BitBtn6Click
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 480
    Top = 264
  end
end
