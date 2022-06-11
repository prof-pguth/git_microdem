object PETMARCommonForm: TPETMARCommonForm
  Left = 492
  Top = 256
  ActiveControl = Edit1
  BorderIcons = []
  Caption = 'G'
  ClientHeight = 122
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 20
  object Edit1: TEdit
    Left = 0
    Top = 41
    Width = 353
    Height = 28
    Align = alTop
    TabOrder = 0
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
    Width = 353
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    Options = [ofEnableSizing]
    Left = 184
    Top = 72
  end
  object PrintDialog1: TPrintDialog
    Left = 136
    Top = 72
  end
  object BMPSaveDialog1: TSaveDialog
    Title = 'Save Image As'
    Left = 272
    Top = 72
  end
  object SaveDialog1: TSaveDialog
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 224
    Top = 72
  end
  object ColorDialog1: TColorDialog
    Left = 104
    Top = 72
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 320
    Top = 72
  end
end
