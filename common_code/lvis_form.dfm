object lvis_form1: Tlvis_form1
  Left = 0
  Top = 0
  Caption = 'LVIS'
  ClientHeight = 224
  ClientWidth = 396
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
  object BitBtn1: TBitBtn
    Left = 32
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Canopy'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 32
    Top = 167
    Width = 75
    Height = 25
    Caption = 'Ground'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 168
    Top = 98
    Width = 75
    Height = 25
    Caption = 'Zoom'
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 168
    Top = 129
    Width = 75
    Height = 25
    Caption = 'Graph'
    TabOrder = 3
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 168
    Top = 160
    Width = 75
    Height = 25
    Caption = '+'
    TabOrder = 4
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 168
    Top = 192
    Width = 75
    Height = 25
    Caption = '-'
    TabOrder = 5
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 32
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Pulses'
    TabOrder = 6
    OnClick = BitBtn7Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 396
    Height = 74
    Align = alTop
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object BitBtn8: TBitBtn
    Left = 280
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Slicer'
    TabOrder = 8
    OnClick = BitBtn8Click
  end
end
