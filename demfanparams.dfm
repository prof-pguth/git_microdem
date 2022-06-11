object PickFanParams: TPickFanParams
  Left = 1398
  Top = 148
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Fan Drawing Options'
  ClientHeight = 296
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 40
    Width = 39
    Height = 13
    Caption = 'Label1'
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 261
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
    Left = 99
    Top = 261
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
    Left = 183
    Top = 261
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
  object Button1: TButton
    Left = 16
    Top = 136
    Width = 121
    Height = 25
    Caption = 'O--Range circles'
    TabOrder = 3
    OnClick = Button1Click
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 168
    Width = 121
    Height = 25
    Caption = 'Visible'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 64
    Width = 129
    Height = 65
    Caption = 'Show'
    ItemIndex = 1
    Items.Strings = (
      'Masked'
      'Visible'
      'Masked && Visible')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object CheckBox2: TCheckBox
    Left = 144
    Top = 176
    Width = 153
    Height = 17
    Caption = 'Symbol at fan location'
    TabOrder = 6
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = 'LOS and fan algorithm'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 16
    Top = 200
    Width = 121
    Height = 25
    Caption = 'Masked'
    TabOrder = 8
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 176
    Top = 200
    Width = 113
    Height = 25
    Caption = 'Observer'
    TabOrder = 9
    OnClick = BitBtn4Click
  end
  object CheckBox4: TCheckBox
    Left = 152
    Top = 80
    Width = 209
    Height = 17
    Caption = 'Display fan vis/masked bitmaps'
    TabOrder = 10
  end
  object BitBtn5: TBitBtn
    Left = 16
    Top = 230
    Width = 121
    Height = 25
    Caption = 'Mixed'
    TabOrder = 11
    OnClick = BitBtn5Click
  end
  object CheckBox3: TCheckBox
    Left = 143
    Top = 231
    Width = 186
    Height = 17
    Caption = 'Differentiate mixed pixels'
    TabOrder = 12
    OnClick = CheckBox3Click
  end
  object RadioGroup2: TRadioGroup
    Left = 328
    Top = 128
    Width = 89
    Height = 65
    Caption = 'Fan format'
    Items.Strings = (
      'PNG'
      'BMP'
      'GIF')
    TabOrder = 13
    OnClick = RadioGroup2Click
  end
end
