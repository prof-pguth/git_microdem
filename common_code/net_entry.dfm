object NetEntryForm: TNetEntryForm
  Left = 0
  Top = 0
  Caption = 'Stereo net entry'
  ClientHeight = 229
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 65
    Height = 13
    Caption = 'Dip and strike'
  end
  object Edit1: TEdit
    Left = 96
    Top = 24
    Width = 89
    Height = 21
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 224
    Top = 16
    Width = 107
    Height = 25
    Caption = 'Great circle'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 224
    Top = 48
    Width = 107
    Height = 25
    Caption = 'Pole'
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 224
    Top = 79
    Width = 107
    Height = 25
    Caption = 'Great circle and Pole'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 64
    Top = 64
    Width = 97
    Height = 25
    Caption = 'Great circle'
    TabOrder = 4
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 224
    Top = 168
    Width = 107
    Height = 25
    Caption = 'Line'
    TabOrder = 5
    OnClick = BitBtn5Click
  end
end
