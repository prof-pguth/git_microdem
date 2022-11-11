object USOutlineForm: TUSOutlineForm
  Left = 431
  Top = 624
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Smart US Outlines'
  ClientHeight = 200
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label4: TLabel
    Left = 56
    Top = 8
    Width = 50
    Height = 13
    Caption = 'Fenneman'
  end
  object Label1: TLabel
    Left = 312
    Top = 40
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 311
    Top = 65
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 312
    Top = 104
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label5: TLabel
    Left = 312
    Top = 128
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object OKBtn: TBitBtn
    Left = 216
    Top = 173
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object Edit2: TEdit
    Left = 159
    Top = 34
    Width = 81
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 159
    Top = 58
    Width = 81
    Height = 21
    TabOrder = 2
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 32
    Width = 35
    Height = 25
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 8
    Top = 56
    Width = 35
    Height = 25
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 1
    Width = 35
    Height = 25
    TabOrder = 5
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 299
    Top = 173
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = BitBtn5Click
    IsControl = True
  end
  object CheckBox5: TCheckBox
    Left = 56
    Top = 40
    Width = 97
    Height = 17
    Caption = 'County outlines'
    TabOrder = 7
  end
  object CheckBox6: TCheckBox
    Left = 56
    Top = 64
    Width = 97
    Height = 17
    Caption = 'State outlines'
    TabOrder = 8
  end
  object CheckBox7: TCheckBox
    Left = 56
    Top = 87
    Width = 97
    Height = 17
    Caption = 'Highways'
    TabOrder = 9
  end
  object CheckBox8: TCheckBox
    Left = 56
    Top = 111
    Width = 97
    Height = 17
    Caption = 'Rivers'
    TabOrder = 10
  end
  object BitBtn6: TBitBtn
    Left = 8
    Top = 80
    Width = 35
    Height = 25
    TabOrder = 11
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 8
    Top = 105
    Width = 35
    Height = 25
    TabOrder = 12
    OnClick = BitBtn7Click
  end
  object BitBtn1: TBitBtn
    Left = 247
    Top = 34
    Width = 59
    Height = 25
    Caption = 'County'
    TabOrder = 13
    OnClick = BitBtn1Click
  end
  object BitBtn8: TBitBtn
    Left = 246
    Top = 61
    Width = 59
    Height = 25
    Caption = 'State'
    TabOrder = 14
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 246
    Top = 92
    Width = 57
    Height = 25
    Caption = 'Highway'
    TabOrder = 15
    OnClick = BitBtn9Click
  end
  object BitBtn10: TBitBtn
    Left = 248
    Top = 120
    Width = 55
    Height = 25
    Caption = 'River'
    TabOrder = 16
    OnClick = BitBtn10Click
  end
  object GroupBox1: TGroupBox
    Left = 17
    Top = 136
    Width = 136
    Height = 65
    Caption = 'Automatically show on'
    TabOrder = 17
    object CheckBox1: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'DEM Maps'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Imagery'
      TabOrder = 1
    end
  end
end
