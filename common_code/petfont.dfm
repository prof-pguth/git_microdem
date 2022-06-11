object FontDlg: TFontDlg
  Left = 486
  Top = 328
  ActiveControl = Edit2
  BorderIcons = [biHelp]
  BorderStyle = bsDialog
  Caption = 'Font Overlay'
  ClientHeight = 193
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 12
    Top = 9
    Width = 102
    Height = 13
    Caption = 'Rotation Angle ('#176')'
  end
  object Label4: TLabel
    Left = 20
    Top = 85
    Width = 45
    Height = 13
    Caption = 'X coord'
  end
  object Label7: TLabel
    Left = 184
    Top = 85
    Width = 13
    Height = 13
    Caption = 'Y '
  end
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 26
    Height = 13
    Caption = 'Text'
  end
  object OKBtn: TBitBtn
    Left = 16
    Top = 164
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
    Top = 164
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = CancelBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 200
    Top = 164
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object Edit1: TEdit
    Left = 133
    Top = 9
    Width = 72
    Height = 21
    TabOrder = 3
    OnChange = Edit1Change
  end
  object Edit3: TEdit
    Left = 89
    Top = 82
    Width = 72
    Height = 21
    TabOrder = 4
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 203
    Top = 82
    Width = 72
    Height = 21
    TabOrder = 5
    OnChange = Edit5Change
  end
  object BitBtn1: TBitBtn
    Left = 192
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Font'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333FFF33FFFFF33333300033000
      00333337773377777333333330333300033333337FF33777F333333330733300
      0333333377FFF777F33333333700000073333333777777773333333333033000
      3333333337FF777F333333333307300033333333377F777F3333333333703007
      33333333377F7773333333333330000333333333337777F33333333333300003
      33333333337777F3333333333337007333333333337777333333333333330033
      3333333333377333333333333333033333333333333733333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object Edit2: TEdit
    Left = 64
    Top = 40
    Width = 185
    Height = 21
    TabOrder = 7
    Text = ' '
    OnChange = Edit2Change
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 128
    Width = 137
    Height = 17
    Caption = 'Clear Background'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 256
    Top = 8
  end
end
