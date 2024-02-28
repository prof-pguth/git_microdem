object eval_scores_graph_form: Teval_scores_graph_form
  Left = 0
  Top = 0
  Caption = 'DEMIX evaluations/scores graphs'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Visible = True
  OnCreate = FormCreate
  TextHeight = 15
  object RadioGroup1: TRadioGroup
    Left = 368
    Top = 96
    Width = 185
    Height = 105
    Caption = 'Graph on x-axis'
    ItemIndex = 0
    Items.Strings = (
      'Evaluations'
      'Scores (opinions)')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 56
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Graph'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 368
    Top = 224
    Width = 185
    Height = 105
    Caption = 'Graph on y axis'
    ItemIndex = 1
    Items.Strings = (
      'Test areas'
      'DEMIX tiles')
    TabOrder = 2
    OnClick = RadioGroup2Click
  end
  object RadioGroup3: TRadioGroup
    Left = 56
    Top = 80
    Width = 185
    Height = 105
    Caption = 'Sort Y axis by'
    ItemIndex = 0
    Items.Strings = (
      'Area/Tile names'
      'Average slope'
      'Average roughness'
      'Relief')
    TabOrder = 3
    OnClick = RadioGroup3Click
  end
  object CheckBox1: TCheckBox
    Left = 376
    Top = 360
    Width = 177
    Height = 17
    Caption = 'Large combined graph'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object BitBtn2: TBitBtn
    Left = 64
    Top = 256
    Width = 121
    Height = 25
    Caption = 'Add stats to DB'
    TabOrder = 5
    OnClick = BitBtn2Click
  end
  object CheckBox2: TCheckBox
    Left = 376
    Top = 384
    Width = 177
    Height = 17
    Caption = 'Panels by test DEM'
    TabOrder = 6
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 376
    Top = 407
    Width = 145
    Height = 17
    Caption = 'Movie by test DEM '
    TabOrder = 7
    OnClick = CheckBox3Click
  end
  object BitBtn3: TBitBtn
    Left = 424
    Top = 16
    Width = 161
    Height = 25
    Caption = 'Cloise graphs and images'
    TabOrder = 8
    OnClick = BitBtn3Click
  end
end
