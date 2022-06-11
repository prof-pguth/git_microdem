object Collabf: TCollabf
  Left = 537
  Top = 376
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Edit Time Unit'
  ClientHeight = 165
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 257
    Height = 105
    Shape = bsFrame
    IsControl = True
  end
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 27
    Height = 13
    Caption = 'Top:'
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 33
    Height = 13
    Caption = 'Name'
  end
  object Label3: TLabel
    Left = 24
    Top = 80
    Width = 33
    Height = 13
    Caption = 'Base:'
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 124
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
    Left = 108
    Top = 124
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
    Left = 208
    Top = 124
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
  object TopEdit: TEdit
    Left = 120
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'TopEdit'
  end
  object TextEdit: TEdit
    Left = 120
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'TextEdit'
  end
  object BaseEdit: TEdit
    Left = 120
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'BaseEdit'
  end
end
