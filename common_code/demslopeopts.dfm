object SlopeOptForm: TSlopeOptForm
  Left = 363
  Top = 242
  ActiveControl = OKBtn
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Slope Map Options'
  ClientHeight = 329
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 124
    Top = 10
    Width = 5
    Height = 13
  end
  object Image1: TImage
    Left = 216
    Top = 141
    Width = 100
    Height = 100
  end
  object Label6: TLabel
    Left = 135
    Top = 15
    Width = 39
    Height = 13
    Caption = 'Label6'
  end
  object Label25: TLabel
    Left = 26
    Top = 41
    Width = 142
    Height = 13
    Caption = 'Slope region size (pixels)'
  end
  object Label1: TLabel
    Left = 24
    Top = 64
    Width = 79
    Height = 13
    Caption = 'Max slope (%)'
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 294
    Width = 77
    Height = 27
    Caption = '&OK'
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 100
    Top = 294
    Width = 77
    Height = 27
    Caption = '&Cancel'
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 183
    Top = 294
    Width = 76
    Height = 27
    Caption = 'Help'
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 88
    Width = 169
    Height = 169
    Caption = 'Colors'
    Items.Strings = (
      'Standard Categories'
      'Trafficability Categories'
      'Gray Scale'
      'Gray scale (reversed)'
      'Rainbow '
      'Pastel '
      'Go/NoGo ')
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 216
    Top = 88
    Width = 89
    Height = 25
    Caption = 'Slope colors'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object Button11: TButton
    Left = 8
    Top = 10
    Width = 116
    Height = 25
    Caption = 'Slope Algorithm'
    TabOrder = 5
    OnClick = Button11Click
  end
  object Edit1: TEdit
    Left = 183
    Top = 37
    Width = 81
    Height = 21
    TabOrder = 6
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 184
    Top = 64
    Width = 80
    Height = 21
    TabOrder = 7
    OnChange = Edit2Change
  end
  object CheckBox1: TCheckBox
    Left = 56
    Top = 264
    Width = 161
    Height = 17
    Caption = 'Immediate redraws'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
end
