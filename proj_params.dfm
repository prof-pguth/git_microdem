object ProjParamForm: TProjParamForm
  Left = 686
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Projection Parameters'
  ClientHeight = 146
  ClientWidth = 192
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 62
    Height = 13
    Caption = 'False easting'
  end
  object Label2: TLabel
    Left = 16
    Top = 80
    Width = 66
    Height = 13
    Caption = 'False northing'
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 8
    Width = 153
    Height = 33
    Caption = 'Units'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Meters'
      'Feet')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 96
    Top = 56
    Width = 65
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object Edit2: TEdit
    Left = 96
    Top = 80
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '0'
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 112
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
end
