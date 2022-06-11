object FullUnitEntryDlg: TFullUnitEntryDlg
  Left = 780
  Top = 241
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Next Stratigraphic Unit'
  ClientHeight = 383
  ClientWidth = 673
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poScreenCenter
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 8
    Width = 242
    Height = 145
    Shape = bsFrame
    IsControl = True
  end
  object ShortLabel: TLabel
    Left = 16
    Top = 24
    Width = 71
    Height = 13
    Caption = 'Short Name:'
  end
  object Longlabel: TLabel
    Left = 16
    Top = 48
    Width = 69
    Height = 13
    Caption = 'Long Name:'
  end
  object ThickLabel: TLabel
    Left = 16
    Top = 72
    Width = 84
    Height = 13
    Caption = 'Thickness (m):'
  end
  object BaseLabel: TLabel
    Left = 16
    Top = 120
    Width = 62
    Height = 13
    Caption = 'Base (Ma):'
  end
  object TopAgeLabel: TLabel
    Left = 16
    Top = 96
    Width = 56
    Height = 13
    Caption = 'Top (Ma):'
  end
  object Image1: TImage
    Left = 32
    Top = 264
    Width = 150
    Height = 80
    AutoSize = True
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 349
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
    Left = 113
    Top = 349
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
    Left = 216
    Top = 349
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
  object ShortNameEdit: TEdit
    Left = 112
    Top = 24
    Width = 113
    Height = 21
    TabOrder = 3
  end
  object LongNameEdit: TEdit
    Left = 112
    Top = 48
    Width = 113
    Height = 21
    TabOrder = 4
  end
  object ThicknessEdit: TEdit
    Left = 113
    Top = 72
    Width = 112
    Height = 21
    TabOrder = 5
    OnChange = ThicknessEditChange
  end
  object BaseAgeEdit: TEdit
    Left = 112
    Top = 120
    Width = 113
    Height = 21
    TabOrder = 6
    OnChange = BaseAgeEditChange
  end
  object BasePanel1: TPanel
    Left = 464
    Top = 200
    Width = 201
    Height = 175
    TabOrder = 7
    object BaseLabell: TLabel
      Left = 8
      Top = 8
      Width = 60
      Height = 13
      Caption = 'Unit Base:'
    end
    object WhatIsBase: TLabel
      Left = 72
      Top = 8
      Width = 5
      Height = 13
    end
    object BaseWidthLabel: TLabel
      Left = 0
      Top = 144
      Width = 34
      Height = 13
      Caption = 'Width'
    end
    object BaseDepthLabel: TLabel
      Left = 96
      Top = 144
      Width = 35
      Height = 13
      Caption = 'Depth'
    end
    object BaseListBox: TListBox
      Left = 32
      Top = 24
      Width = 137
      Height = 114
      ItemHeight = 13
      Items.Strings = (
        'Normal conformable'
        'Unconformity'
        'Channel'
        'Transgressive'
        'Regressive'
        'Questionable'
        'Probable'
        'Jagged')
      TabOrder = 0
      OnClick = BaseListBoxClick
    end
    object BaseWidthEdit: TEdit
      Left = 40
      Top = 144
      Width = 41
      Height = 21
      TabOrder = 1
      Text = '5'
      OnChange = BaseWidthEditChange
    end
    object BaseDepthEdit: TEdit
      Left = 136
      Top = 144
      Width = 41
      Height = 21
      TabOrder = 2
      Text = '5'
      OnChange = BaseDepthEditChange
    end
  end
  object Panel1: TPanel
    Left = 464
    Top = 8
    Width = 201
    Height = 169
    TabOrder = 8
    object TopLabel: TLabel
      Left = 8
      Top = 8
      Width = 50
      Height = 13
      Caption = 'Unit top:'
    end
    object WhatIsTop: TLabel
      Left = 64
      Top = 8
      Width = 5
      Height = 13
    end
    object TopWidthLabel: TLabel
      Left = 8
      Top = 139
      Width = 34
      Height = 13
      Caption = 'Width'
    end
    object TopDepthLabel: TLabel
      Left = 96
      Top = 139
      Width = 39
      Height = 13
      Caption = 'Depth:'
    end
    object TopListBox: TListBox
      Left = 24
      Top = 24
      Width = 145
      Height = 109
      ItemHeight = 13
      Items.Strings = (
        'Normal Conformable'
        'Unconformity'
        'Channel'
        'Transgressive'
        'Regressive'
        'Questionable'
        'Probable'
        'Jagged')
      TabOrder = 0
      OnClick = TopListBoxClick
    end
    object TopWidthEdit: TEdit
      Left = 48
      Top = 139
      Width = 41
      Height = 21
      TabOrder = 1
      Text = '5'
      OnChange = TopWidthEditChange
    end
    object TopDepthEdit: TEdit
      Left = 136
      Top = 139
      Width = 41
      Height = 21
      TabOrder = 2
      Text = '5'
      OnChange = TopDepthEditChange
    end
  end
  object LithPanel: TPanel
    Left = 248
    Top = 8
    Width = 201
    Height = 297
    TabOrder = 9
    object LithLabel: TLabel
      Left = 8
      Top = 8
      Width = 56
      Height = 13
      Caption = 'Lithology:'
    end
    object LithListBox: TListBox
      Left = 16
      Top = 24
      Width = 153
      Height = 257
      ItemHeight = 13
      TabOrder = 0
      OnClick = LithListBoxClick
    end
    object SpinButton1: TSpinButton
      Left = 176
      Top = 48
      Width = 20
      Height = 25
      DownGlyph.Data = {
        BA000000424DBA00000000000000420000002800000009000000060000000100
        1000030000007800000000000000000000000000000000000000007C0000E003
        00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03DB904E03DE03DE03D
        E03D0000E03DE03DE03DE03DB904E03DE03DE03D000000000000E03DE03DE03D
        F303E03DE03D00000000000000000000E03DE03D00A0E03D0000000000000000
        000000000000E03D0500E03DE03DE03DE03DE03DE03DE03DE03DE03D1001}
      TabOrder = 1
      UpGlyph.Data = {
        BA000000424DBA00000000000000420000002800000009000000060000000100
        1000030000007800000000000000000000000000000000000000007C0000E003
        00001F000000E03DE03DE03DE03DE03DE03DE03DE03DE03D0300E03D00000000
        00000000000000000000E03DF303E03DE03D00000000000000000000E03DE03D
        F303E03DE03DE03D000000000000E03DE03DE03D5704E03DE03DE03DE03D0000
        E03DE03DE03DE03DBF81E03DE03DE03DE03DE03DE03DE03DE03DE03DBF81}
      OnDownClick = SpinButton1DownClick
      OnUpClick = SpinButton1UpClick
    end
    object ComboBox1: TComboBox
      Left = 16
      Top = 24
      Width = 145
      Height = 21
      TabOrder = 2
      Text = 'ComboBox1'
      Visible = False
    end
  end
  object TopAgeEdit: TEdit
    Left = 113
    Top = 96
    Width = 112
    Height = 21
    TabOrder = 10
    OnChange = TopAgeEditChange
  end
  object ResistancePanel: TPanel
    Left = 8
    Top = 160
    Width = 185
    Height = 97
    Caption = ' '
    TabOrder = 11
    object RadioGroup1: TRadioGroup
      Left = 0
      Top = 0
      Width = 185
      Height = 97
      Caption = 'Resistance'
      Items.Strings = (
        'Very recessive'
        'Recessive'
        'Moderate'
        'Resistance'
        'Very Resistant')
      TabOrder = 0
    end
  end
end
