object PickFillForm: TPickFillForm
  Left = 313
  Top = 281
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Pick Fill '
  ClientHeight = 223
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 137
    Height = 209
    OnDblClick = Image1DblClick
    OnMouseMove = Image1MouseMove
  end
  object Image2: TImage
    Left = 168
    Top = 8
    Width = 137
    Height = 100
  end
  object Label1: TLabel
    Left = 275
    Top = 126
    Width = 59
    Height = 13
    Caption = 'Border width'
    OnClick = BitBtn1Click
  end
  object Label2: TLabel
    Left = 275
    Top = 145
    Width = 3
    Height = 13
  end
  object BitBtn1: TBitBtn
    Left = 168
    Top = 121
    Width = 75
    Height = 25
    Caption = 'Outline'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 168
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Fill'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object OKBtn: TBitBtn
    Left = 249
    Top = 182
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = OKBtnClick
    IsControl = True
  end
  object SpinButton1: TSpinButton
    Left = 249
    Top = 126
    Width = 20
    Height = 29
    DownGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03DE03DE03D
      E03D0000E03DE03DE03DE03D4B02E03DE03DE03D000000000000E03DE03DE03D
      4B02E03DE03D00000000000000000000E03DE03D7002E03D0000000000000000
      000000000000E03DBF81E03DE03DE03DE03DE03DE03DE03DE03DE03DBC81}
    TabOrder = 3
    UpGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03D00000000
      00000000000000000000E03D4B02E03DE03D00000000000000000000E03DE03D
      4B02E03DE03DE03D000000000000E03DE03DE03D7002E03DE03DE03DE03D0000
      E03DE03DE03DE03DBC81E03DE03DE03DE03DE03DE03DE03DE03DE03D0000}
    OnDownClick = SpinButton1DownClick
    OnUpClick = SpinButton1UpClick
  end
  object Button1: TButton
    Left = 168
    Top = 183
    Width = 75
    Height = 25
    Caption = 'Fill && border'
    TabOrder = 4
    OnClick = Button1Click
  end
  object ColorDialog1: TColorDialog
    Left = 128
    Top = 96
  end
end
