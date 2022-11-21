object lineparamsform: Tlineparamsform
  Left = 678
  Top = 491
  Caption = 'Line Parameters'
  ClientHeight = 97
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnActivate = FormActivate
  OnCreate = FormCreate
  TextHeight = 13
  object Image1: TImage
    Left = 24
    Top = 8
    Width = 120
    Height = 65
  end
  object Width: TLabel
    Left = 160
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object Button1: TButton
    Left = 160
    Top = 72
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SpinButton1: TSpinButton
    Left = 200
    Top = 8
    Width = 20
    Height = 25
    DownGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0100E03DE03DE03D
      E03D0000E03DE03DE03DE03D2B02E03DE03DE03D000000000000E03DE03DE03D
      6600E03DE03D00000000000000000000E03DE03D8B04E03D0000000000000000
      000000000000E03D0000E03DE03DE03DE03DE03DE03DE03DE03DE03D0200}
    TabOrder = 1
    UpGlyph.Data = {
      BA000000424DBA00000000000000420000002800000009000000060000000100
      1000030000007800000000000000000000000000000000000000007C0000E003
      00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D3503E03D00000000
      00000000000000000000E03DBD02E03DE03D00000000000000000000E03DE03D
      0400E03DE03DE03D000000000000E03DE03DE03DCF01E03DE03DE03DE03D0000
      E03DE03DE03DE03DC081E03DE03DE03DE03DE03DE03DE03DE03DE03D0000}
    OnDownClick = SpinButton1DownClick
    OnUpClick = SpinButton1UpClick
  end
  object BitBtn1: TBitBtn
    Left = 160
    Top = 40
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object ColorDialog1: TColorDialog
    Left = 8
    Top = 80
  end
end
