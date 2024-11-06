object FlightControlSensitivity: TFlightControlSensitivity
  Left = 109
  Top = 564
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Flight Control Sensitivity'
  ClientHeight = 129
  ClientWidth = 190
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Speed sensitivity'
  end
  object Label2: TLabel
    Left = 15
    Top = 35
    Width = 88
    Height = 13
    Caption = 'Heading sensitivity'
  end
  object Label3: TLabel
    Left = 20
    Top = 62
    Width = 83
    Height = 13
    Caption = 'Altitude sensitivity'
  end
  object Edit1: TEdit
    Left = 109
    Top = 8
    Width = 73
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 109
    Top = 35
    Width = 73
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 109
    Top = 62
    Width = 73
    Height = 21
    TabOrder = 2
  end
  object OKBtn: TBitBtn
    Left = 26
    Top = 89
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    IsControl = True
  end
end
