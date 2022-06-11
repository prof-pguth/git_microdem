object mess_cont_form: Tmess_cont_form
  Left = 579
  Top = 345
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 281
  ClientWidth = 470
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 240
    Width = 470
    Height = 41
    Align = alBottom
    TabOrder = 0
    object OKBtn: TBitBtn
      Left = 56
      Top = 6
      Width = 77
      Height = 27
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = OKBtnClick
      IsControl = True
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 470
    Height = 240
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
end
