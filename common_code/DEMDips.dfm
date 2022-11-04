object StructureOptions: TStructureOptions
  Left = 263
  Top = 115
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Structural Geology Overlay Options'
  ClientHeight = 212
  ClientWidth = 313
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
    Width = 281
    Height = 153
    Shape = bsFrame
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 29
    Top = 177
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
    Left = 112
    Top = 177
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
    Left = 204
    Top = 177
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
  object LabelValueCheckBox: TCheckBox
    Left = 26
    Top = 135
    Width = 98
    Height = 14
    Caption = 'Label Values'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 184
    Top = 136
    Width = 97
    Height = 17
    Caption = 'Stereo Net'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 16
    Width = 153
    Height = 105
    Caption = 'Plot symbols as'
    ItemIndex = 0
    Items.Strings = (
      'Sedimentary bedding'
      'Joints'
      'Foliation'
      'Fault plane')
    TabOrder = 5
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 200
    Top = 64
    Width = 65
    Height = 25
    Caption = 'Net'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 200
    Top = 24
    Width = 65
    Height = 25
    Caption = 'Map'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
end
