object PetList: TPetList
  Left = 574
  Top = 310
  BorderIcons = []
  Caption = 'D'
  ClientHeight = 306
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 507
    Height = 41
    Align = alBottom
    TabOrder = 0
    object CancelBtn: TBitBtn
      Left = 100
      Top = 5
      Width = 77
      Height = 27
      Enabled = False
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = CancelBtnClick
      IsControl = True
    end
    object OKBtn: TBitBtn
      Left = 16
      Top = 5
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
    object HelpBtn: TBitBtn
      Left = 184
      Top = 5
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
    object BitBtn1: TBitBtn
      Left = 296
      Top = 6
      Width = 97
      Height = 25
      Caption = 'Load test file'
      TabOrder = 3
      Visible = False
      OnClick = BitBtn1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 507
    Height = 265
    Align = alClient
    TabOrder = 1
    object ListBox1: TListBox
      Left = 1
      Top = 42
      Width = 505
      Height = 222
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
      OnDblClick = ListBox1DblClick
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 505
      Height = 41
      Align = alTop
      TabOrder = 1
    end
  end
end
