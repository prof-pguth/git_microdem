object PETMARCommonForm: TPETMARCommonForm
  Left = 492
  Top = 256
  ActiveControl = Edit1
  BorderIcons = []
  Caption = 'G'
  ClientHeight = 179
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'System'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  TextHeight = 16
  object Edit1: TEdit
    Left = 0
    Top = 41
    Width = 484
    Height = 28
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 353
  end
  object OKBtn: TBitBtn
    Left = 24
    Top = 76
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = OKBtnClick
    IsControl = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 484
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
    ExplicitWidth = 353
  end
  object OpenDialog1: TOpenDialog
    Options = [ofEnableSizing]
    Left = 126
    Top = 114
  end
  object SaveDialog1: TSaveDialog
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 224
    Top = 114
  end
  object ColorDialog1: TColorDialog
    Left = 26
    Top = 114
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 416
    Top = 122
  end
end
