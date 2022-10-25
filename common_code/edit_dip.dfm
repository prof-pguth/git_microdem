object GetDipStrike: TGetDipStrike
  Left = 268
  Top = 239
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Structural Geology Overlay Options'
  ClientHeight = 226
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 225
    Height = 195
    Shape = bsFrame
    IsControl = True
  end
  object Label1: TLabel
    Left = 16
    Top = 136
    Width = 59
    Height = 13
    Caption = 'Dip/Strike'
  end
  object Label2: TLabel
    Left = 16
    Top = 160
    Width = 32
    Height = 13
    Caption = 'Note:'
  end
  object OKBtn: TBitBtn
    Left = 4
    Top = 209
    Width = 65
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 72
    Top = 209
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
    Left = 156
    Top = 209
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
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 16
    Width = 185
    Height = 114
    Caption = 'Plot symbols as'
    ItemIndex = 0
    Items.Strings = (
      'Sedimentary bedding'
      'Joints'
      'Foliation'
      'Fault plane')
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object Edit1: TEdit
    Left = 80
    Top = 136
    Width = 137
    Height = 21
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 80
    Top = 160
    Width = 137
    Height = 21
    TabOrder = 5
  end
end
