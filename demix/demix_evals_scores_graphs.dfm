object eval_scores_graph_form: Teval_scores_graph_form
  Left = 0
  Top = 0
  ClientHeight = 470
  ClientWidth = 1071
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
    Left = 528
    Top = 339
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
  object Label4: TLabel
    Left = 680
    Top = 300
    Width = 107
    Height = 15
    Caption = 'Complex graph bins'
  end
  object Label5: TLabel
    Left = 904
    Top = 40
    Width = 81
    Height = 15
    Caption = 'Graph columns'
  end
  object RadioGroup1: TRadioGroup
    Left = 416
    Top = 96
    Width = 137
    Height = 73
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
    Left = 416
    Top = 175
    Width = 137
    Height = 74
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
    Left = 620
    Top = 338
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
    Left = 583
    Top = 81
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
    Left = 184
    Top = 409
    Width = 145
    Height = 25
    Caption = 'Cop wins,  slope/barren'
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
    Left = 568
    Top = 437
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
  object Memo1: TMemo
    Left = 184
    Top = 8
    Width = 121
    Height = 220
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 27
  end
  object BitBtn18: TBitBtn
    Left = 311
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Pick'
    TabOrder = 28
    OnClick = BitBtn18Click
  end
  object BitBtn19: TBitBtn
    Left = 320
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Load file'
    TabOrder = 29
    OnClick = BitBtn19Click
  end
  object BitBtn20: TBitBtn
    Left = 320
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Save to file'
    TabOrder = 30
    OnClick = BitBtn20Click
  end
  object BitBtn21: TBitBtn
    Left = 320
    Top = 144
    Width = 75
    Height = 25
    Caption = 'All in DB'
    TabOrder = 31
    OnClick = BitBtn21Click
  end
  object BitBtn17: TBitBtn
    Left = 24
    Top = 409
    Width = 145
    Height = 25
    Caption = 'Best eval filtered by slope'
    TabOrder = 32
    OnClick = BitBtn17Click
  end
  object BitBtn22: TBitBtn
    Left = 128
    Top = 368
    Width = 193
    Height = 25
    Caption = 'Win vs Cop, filtered by slope'
    TabOrder = 33
    OnClick = BitBtn22Click
  end
  object BitBtn23: TBitBtn
    Left = 607
    Top = 39
    Width = 90
    Height = 25
    Caption = 'Merge graphs'
    TabOrder = 34
    OnClick = BitBtn23Click
  end
  object BitBtn24: TBitBtn
    Left = 744
    Top = 370
    Width = 185
    Height = 25
    Caption = 'Best eval, by slope/barren'
    TabOrder = 35
    OnClick = BitBtn24Click
  end
  object Edit4: TEdit
    Left = 808
    Top = 306
    Width = 65
    Height = 23
    TabOrder = 36
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object RadioGroup3: TRadioGroup
    Left = 744
    Top = 32
    Width = 113
    Height = 65
    Caption = 'Symbols'
    ItemIndex = 0
    Items.Strings = (
      'Squares'
      'Circles')
    TabOrder = 37
    OnClick = RadioGroup3Click
  end
  object RadioGroup6: TRadioGroup
    Left = 744
    Top = 112
    Width = 113
    Height = 65
    Caption = '2 Geomorph Plots'
    ItemIndex = 0
    Items.Strings = (
      'Pie charts'
      'All points')
    TabOrder = 38
    OnClick = RadioGroup6Click
  end
  object BitBtn25: TBitBtn
    Left = 744
    Top = 432
    Width = 185
    Height = 25
    Caption = 'COP FUV by slope/barren'
    TabOrder = 39
    OnClick = BitBtn25Click
  end
  object BitBtn26: TBitBtn
    Left = 744
    Top = 401
    Width = 185
    Height = 25
    Caption = 'ALOS FUV by slope/barren'
    TabOrder = 40
    OnClick = BitBtn26Click
  end
  object Edit5: TEdit
    Left = 991
    Top = 37
    Width = 42
    Height = 23
    TabOrder = 41
    Text = 'Edit5'
    OnChange = Edit5Change
  end
  object BitBtn27: TBitBtn
    Left = 920
    Top = 72
    Width = 75
    Height = 25
    Caption = 'New DB'
    TabOrder = 42
    OnClick = BitBtn27Click
  end
  object RadioGroup7: TRadioGroup
    Left = 744
    Top = 184
    Width = 113
    Height = 75
    Caption = 'Group Won/Lost'
    ItemIndex = 0
    Items.Strings = (
      'By test DEM'
      'By criteriion')
    TabOrder = 43
    OnClick = RadioGroup7Click
  end
  object Memo2: TMemo
    Left = 887
    Top = 133
    Width = 114
    Height = 89
    Lines.Strings = (
      '(None)'
      'AVG_SLOPE>30'
      'AVG_SLOPE>5'
      'BARREN_PC>40')
    TabOrder = 44
  end
  object BitBtn28: TBitBtn
    Left = 912
    Top = 232
    Width = 121
    Height = 25
    Caption = 'Best evzl with filters'
    TabOrder = 45
    OnClick = BitBtn28Click
  end
end
