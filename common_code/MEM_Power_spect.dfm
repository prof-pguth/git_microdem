object MemForm: TMemForm
  Left = 797
  Top = 202
  Caption = 'MEM Power spectrum options'
  ClientHeight = 182
  ClientWidth = 221
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 13
  object Profiles: TLabel
    Left = 64
    Top = 16
    Width = 139
    Height = 13
    Caption = 'Number Profiles (odd number)'
  end
  object Label2: TLabel
    Left = 63
    Top = 46
    Width = 147
    Height = 13
    Caption = 'Number Poles for MEM method'
  end
  object Label1: TLabel
    Left = 63
    Top = 75
    Width = 70
    Height = 13
    Caption = 'Low frequency'
  end
  object Label3: TLabel
    Left = 63
    Top = 99
    Width = 72
    Height = 13
    Caption = 'High frequency'
  end
  object Edit1: TEdit
    Left = 8
    Top = 16
    Width = 49
    Height = 21
    TabOrder = 0
    Text = '11'
  end
  object Edit3: TEdit
    Left = 8
    Top = 43
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '30'
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 150
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 126
    Top = 150
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = BitBtn3Click
    IsControl = True
  end
  object Edit2: TEdit
    Left = 8
    Top = 72
    Width = 49
    Height = 21
    TabOrder = 4
  end
  object Edit4: TEdit
    Left = 8
    Top = 99
    Width = 49
    Height = 21
    TabOrder = 5
  end
  object CheckBox1: TCheckBox
    Left = 32
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Log/log plot'
    TabOrder = 6
  end
end
