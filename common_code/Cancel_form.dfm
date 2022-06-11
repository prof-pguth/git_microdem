object CheckCancel: TCheckCancel
  Left = 692
  Top = 638
  BorderIcons = []
  ClientHeight = 79
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 56
    Width = 3
    Height = 13
  end
  object BitBtn1: TBitBtn
    Left = 48
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
end
