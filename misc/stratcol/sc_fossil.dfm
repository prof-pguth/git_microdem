object FossilRangeOptions: TFossilRangeOptions
  Left = 838
  Top = 194
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Fossil Range Options'
  ClientHeight = 211
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 300
    Height = 161
    Shape = bsFrame
    IsControl = True
  end
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 39
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 24
    Top = 56
    Width = 78
    Height = 13
    Caption = 'Ranges wide:'
  end
  object Label3: TLabel
    Left = 24
    Top = 96
    Width = 98
    Height = 13
    Caption = 'Starting location:'
  end
  object Label4: TLabel
    Left = 24
    Top = 128
    Width = 93
    Height = 13
    Caption = 'Ending location:'
  end
  object OKBtn: TBitBtn
    Left = 64
    Top = 180
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
    Left = 148
    Top = 180
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 232
    Top = 180
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
  object Edit1: TEdit
    Left = 144
    Top = 56
    Width = 73
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 144
    Top = 96
    Width = 73
    Height = 21
    TabOrder = 4
    Text = 'Edit2'
  end
  object Edit3: TEdit
    Left = 144
    Top = 128
    Width = 73
    Height = 21
    TabOrder = 5
    Text = 'Edit3'
    OnChange = Edit3Change
  end
end
