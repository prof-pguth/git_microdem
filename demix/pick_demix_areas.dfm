object PickAreasForm: TPickAreasForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Pick DEMIX Areas'
  ClientHeight = 290
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 15
  object BitBtn1: TBitBtn
    Left = 32
    Top = 24
    Width = 137
    Height = 25
    Caption = 'All areas'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 32
    Top = 64
    Width = 137
    Height = 25
    Caption = 'Pick areas'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 32
    Top = 104
    Width = 137
    Height = 25
    Caption = 'Load from file'
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object CancelBtn: TBitBtn
    Left = 32
    Top = 248
    Width = 66
    Height = 27
    Enabled = False
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    IsControl = True
  end
  object Memo1: TMemo
    Left = 216
    Top = 8
    Width = 185
    Height = 225
    TabOrder = 4
  end
  object BitBtn4: TBitBtn
    Left = 32
    Top = 143
    Width = 137
    Height = 25
    Caption = 'Load from memo'
    TabOrder = 5
    OnClick = BitBtn4Click
  end
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 184
    Width = 153
    Height = 49
    Caption = 'Alphabetical 1/3 of areas'
    Columns = 3
    Items.Strings = (
      'First '
      'Mid'
      'Last')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
end
