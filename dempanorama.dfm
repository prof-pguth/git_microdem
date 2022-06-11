inherited PanoramaOps: TPanoramaOps
  Left = 428
  Top = 292
  Caption = 'Panorama Options'
  ClientHeight = 196
  ClientWidth = 319
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  ExplicitWidth = 337
  ExplicitHeight = 243
  PixelsPerInch = 96
  TextHeight = 20
  inherited Bevel1: TBevel
    Width = 209
    Height = 185
    ExplicitWidth = 209
    ExplicitHeight = 185
  end
  object Label1: TLabel [1]
    Left = 24
    Top = 80
    Width = 88
    Height = 20
    Caption = 'Start azimuth'
  end
  object Label2: TLabel [2]
    Left = 24
    Top = 104
    Width = 95
    Height = 20
    Caption = 'Panorama size'
  end
  object Label3: TLabel [3]
    Left = 24
    Top = 128
    Width = 135
    Height = 20
    Caption = 'Increment per frame'
  end
  inherited OKBtn: TButton
    Left = 236
    ExplicitLeft = 236
  end
  inherited CancelBtn: TButton
    Left = 236
    Top = 37
    ExplicitLeft = 236
    ExplicitTop = 37
  end
  object HelpBtn: TButton
    Left = 236
    Top = 75
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 16
    Width = 137
    Height = 57
    Caption = 'Rotate'
    ItemIndex = 0
    Items.Strings = (
      'Clockwise'
      'Counterclockwise')
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 136
    Top = 80
    Width = 57
    Height = 28
    TabOrder = 4
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 136
    Top = 104
    Width = 57
    Height = 28
    TabOrder = 5
    Text = 'Edit2'
  end
  object Edit3: TEdit
    Left = 136
    Top = 128
    Width = 57
    Height = 28
    TabOrder = 6
    Text = 'Edit3'
  end
  object ComboBox1: TComboBox
    Left = 24
    Top = 160
    Width = 145
    Height = 28
    TabOrder = 7
    Text = 'ComboBox1'
    OnChange = ComboBox1Change
    Items.Strings = (
      'Regular Fishnet'
      'Chromadepth Fishnet'
      'Reflectance'
      'Draped')
  end
end
