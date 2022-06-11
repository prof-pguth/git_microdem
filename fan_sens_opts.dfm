object fan_sens_form: Tfan_sens_form
  Left = 603
  Top = 281
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Fan Sensitivity Options'
  ClientHeight = 227
  ClientWidth = 292
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
    Left = 8
    Top = 16
    Width = 114
    Height = 13
    Caption = 'Radial spacing, degrees'
  end
  object Label2: TLabel
    Left = 152
    Top = 16
    Width = 133
    Height = 13
    Caption = 'Spacing along radial, meters'
  end
  object Memo1: TMemo
    Left = 152
    Top = 35
    Width = 121
    Height = 137
    Lines.Strings = (
      '1'
      '5'
      '10'
      '15'
      '30'
      '60')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 35
    Width = 121
    Height = 137
    Lines.Strings = (
      '0.5'
      '1'
      '2.5'
      '5')
    TabOrder = 1
  end
  object HelpBtn: TBitBtn
    Left = 176
    Top = 190
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 32
    Top = 190
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
