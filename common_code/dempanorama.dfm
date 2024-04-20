inherited PanoramaOps: TPanoramaOps
  Left = 428
  Top = 292
  Caption = 'Panorama Options'
  ClientHeight = 201
  ClientWidth = 400
  Position = poDefaultSizeOnly
  StyleElements = [seFont, seClient, seBorder]
  OnCreate = FormCreate
  ExplicitWidth = 416
  ExplicitHeight = 240
  TextHeight = 15
  inherited Bevel1: TBevel
    Width = 273
    Height = 185
    ExplicitWidth = 273
    ExplicitHeight = 185
  end
  object Label1: TLabel [1]
    Left = 24
    Top = 80
    Width = 70
    Height = 15
    Caption = 'Start azimuth'
  end
  object Label2: TLabel [2]
    Left = 24
    Top = 104
    Width = 76
    Height = 15
    Caption = 'Panorama size'
  end
  object Label3: TLabel [3]
    Left = 24
    Top = 128
    Width = 108
    Height = 15
    Caption = 'Increment per frame'
  end
  inherited OKBtn: TButton
    Left = 316
    Top = 12
    ExplicitLeft = 316
    ExplicitTop = 12
  end
  inherited CancelBtn: TButton
    Left = 316
    Top = 41
    ExplicitLeft = 316
    ExplicitTop = 41
  end
  object HelpBtn: TButton
    Left = 316
    Top = 79
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
    Left = 173
    Top = 82
    Width = 57
    Height = 23
    TabOrder = 4
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 173
    Top = 106
    Width = 57
    Height = 23
    TabOrder = 5
    Text = 'Edit2'
  end
  object Edit3: TEdit
    Left = 173
    Top = 130
    Width = 57
    Height = 23
    TabOrder = 6
    Text = 'Edit3'
  end
  object ComboBox1: TComboBox
    Left = 24
    Top = 160
    Width = 145
    Height = 23
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
