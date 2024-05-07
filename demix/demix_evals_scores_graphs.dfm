object eval_scores_graph_form: Teval_scores_graph_form
  Left = 0
  Top = 0
  ClientHeight = 470
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Visible = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 376
    Top = 68
    Width = 86
    Height = 15
    Caption = 'Legend font size'
  end
  object Label2: TLabel
    Left = 208
    Top = 335
    Width = 63
    Height = 15
    Caption = 'Graph y size'
  end
  object Label3: TLabel
    Left = 208
    Top = 309
    Width = 63
    Height = 15
    Caption = 'Graph x size'
  end
  object RadioGroup1: TRadioGroup
    Left = 368
    Top = 96
    Width = 185
    Height = 89
    Caption = 'Graph on x-axis'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Evaluations'
      'Scores (opinions)')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Graph by best evaluation'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 368
    Top = 191
    Width = 185
    Height = 90
    Caption = 'Graph on y axis'
    Enabled = False
    ItemIndex = 1
    Items.Strings = (
      'Test areas'
      'DEMIX tiles')
    TabOrder = 2
    OnClick = RadioGroup2Click
  end
  object CheckBox1: TCheckBox
    Left = 376
    Top = 360
    Width = 217
    Height = 17
    Caption = 'Large combined graph all criteria'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 376
    Top = 384
    Width = 177
    Height = 17
    Caption = 'Panels by test DEM'
    TabOrder = 4
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 376
    Top = 407
    Width = 145
    Height = 17
    Caption = 'Movie by test DEM '
    TabOrder = 5
    OnClick = CheckBox3Click
  end
  object BitBtn3: TBitBtn
    Left = 432
    Top = 8
    Width = 161
    Height = 25
    Caption = 'Close graphs and images'
    TabOrder = 6
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 468
    Top = 67
    Width = 69
    Height = 23
    TabOrder = 7
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 288
    Top = 327
    Width = 65
    Height = 23
    TabOrder = 8
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 288
    Top = 306
    Width = 65
    Height = 23
    TabOrder = 9
    Text = 'Edit1'
    OnChange = Edit3Change
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 296
    Width = 161
    Height = 25
    Caption = 'Average multiple criteria'
    TabOrder = 10
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 24
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Scatterplot'
    TabOrder = 11
    OnClick = BitBtn5Click
  end
  object RadioGroup4: TRadioGroup
    Left = 207
    Top = 8
    Width = 130
    Height = 209
    Caption = 'Scatter plot SSIM/FUV'
    Items.Strings = (
      'ELEV'
      'HILL'
      'SLOPE'
      'RUFF'
      'RRI'
      'TPI'
      'ACCUM'
      'WETIN'
      'HAND'
      'LS')
    TabOrder = 12
    OnClick = RadioGroup4Click
  end
  object BitBtn6: TBitBtn
    Left = 24
    Top = 337
    Width = 75
    Height = 25
    Caption = 'Histograms'
    TabOrder = 13
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 8
    Top = 132
    Width = 145
    Height = 25
    Caption = 'Graph by average slope'
    TabOrder = 14
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 8
    Top = 163
    Width = 145
    Height = 25
    Caption = 'Graph by avg roughness'
    TabOrder = 15
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 8
    Top = 101
    Width = 145
    Height = 25
    Caption = 'Graph by relief'
    TabOrder = 16
    OnClick = BitBtn9Click
  end
  object BitBtn10: TBitBtn
    Left = 8
    Top = 234
    Width = 209
    Height = 25
    Caption = 'Winning perentages versus COP'
    TabOrder = 17
    OnClick = BitBtn10Click
  end
  object RadioGroup5: TRadioGroup
    Left = 368
    Top = 296
    Width = 185
    Height = 41
    Caption = 'Symbol size'
    Columns = 3
    Items.Strings = (
      '3'
      '4'
      '5')
    TabOrder = 18
    OnClick = RadioGroup5Click
  end
  object BitBtn11: TBitBtn
    Left = 8
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Graph by tile barren %'
    TabOrder = 19
    OnClick = BitBtn11Click
  end
  object BitBtn12: TBitBtn
    Left = 8
    Top = 265
    Width = 209
    Height = 25
    Caption = 'Whisker plots by parameter/criterion'
    TabOrder = 20
    OnClick = BitBtn12Click
  end
  object BitBtn13: TBitBtn
    Left = 192
    Top = 380
    Width = 137
    Height = 25
    Caption = 'Best by slope/barren'
    TabOrder = 21
    OnClick = BitBtn13Click
  end
  object CheckBox4: TCheckBox
    Left = 376
    Top = 440
    Width = 161
    Height = 17
    Caption = 'Graph retired DEMs'
    TabOrder = 22
    OnClick = CheckBox4Click
  end
  object BitBtn14: TBitBtn
    Left = 8
    Top = 70
    Width = 145
    Height = 25
    Caption = 'Graph by tile forest %'
    TabOrder = 23
    OnClick = BitBtn14Click
  end
  object BitBtn15: TBitBtn
    Left = 24
    Top = 440
    Width = 145
    Height = 25
    Caption = 'Best eval by criterion'
    TabOrder = 24
    OnClick = BitBtn15Click
  end
  object BitBtn16: TBitBtn
    Left = 192
    Top = 440
    Width = 145
    Height = 25
    Caption = 'Best eval colored by slope'
    TabOrder = 25
    OnClick = BitBtn16Click
  end
  object BitBtn2: TBitBtn
    Left = 432
    Top = 40
    Width = 161
    Height = 25
    Caption = 'Merge graph panels'
    TabOrder = 26
    OnClick = BitBtn2Click
  end
end
