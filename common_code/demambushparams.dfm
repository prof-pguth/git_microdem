object PickAmbushParams: TPickAmbushParams
  Left = 517
  Top = 169
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'T'
  ClientHeight = 235
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 16
    Top = 40
    Width = 39
    Height = 13
    Caption = 'Label5'
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 200
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 95
    Top = 200
    Width = 78
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 175
    Top = 200
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
  object Button2: TButton
    Left = 16
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Fan algorithm'
    TabOrder = 3
    OnClick = Button2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 64
    Width = 129
    Height = 65
    Caption = 'Show'
    ItemIndex = 1
    Items.Strings = (
      'Masked'
      'Visible'
      'Masked && visible')
    TabOrder = 4
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 144
    Width = 201
    Height = 17
    Caption = 'Percentage of route covered'
    TabOrder = 5
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 160
    Width = 177
    Height = 17
    Caption = 'Show route on overlays'
    TabOrder = 6
  end
  object BitBtn1: TBitBtn
    Left = 160
    Top = 72
    Width = 113
    Height = 25
    Caption = 'All coverage'
    TabOrder = 7
  end
  object BitBtn4: TBitBtn
    Left = 160
    Top = 96
    Width = 113
    Height = 25
    Caption = 'Fan Visible'
    TabOrder = 8
    OnClick = BitBtn4Click
  end
end
