object PLSSConvertForm: TPLSSConvertForm
  Left = 344
  Top = 229
  BorderIcons = [biSystemMenu]
  Caption = 'PLSS Coordinate Converter'
  ClientHeight = 269
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 93
    Width = 550
    Height = 176
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 566
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 8
    Width = 161
    Height = 25
    Caption = 'PLSS to Lat/Long'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 32
    Width = 161
    Height = 25
    Caption = 'Lat/Long to PLSS'
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object OKBtn: TBitBtn
    Left = 426
    Top = 61
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 8
    Top = 63
    Width = 161
    Height = 25
    Caption = 'UTM/Lat-Long/Datum shift'
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object BitBtn5: TBitBtn
    Left = 345
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 5
    OnClick = BitBtn5Click
  end
end
