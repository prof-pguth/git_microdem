object eval_scores_graph_form: Teval_scores_graph_form
  Left = 0
  Top = 0
  Caption = 'Add tolerances to table'
  ClientHeight = 672
  ClientWidth = 1497
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
    Left = 1177
    Top = 591
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
    Left = 382
    Top = 477
    Width = 107
    Height = 15
    Caption = 'Complex graph bins'
  end
  object Label5: TLabel
    Left = 424
    Top = 503
    Width = 81
    Height = 15
    Caption = 'Graph columns'
  end
  object Label6: TLabel
    Left = 1176
    Top = 564
    Width = 79
    Height = 15
    Caption = 'Graph font size'
  end
  object RadioGroup2: TRadioGroup
    Left = 1320
    Top = 265
    Width = 153
    Height = 74
    Caption = 'Average multiple criteria'
    ItemIndex = 1
    Items.Strings = (
      'Test areas'
      'DEMIX tiles')
    TabOrder = 0
    OnClick = RadioGroup2Click
  end
  object CheckBox1: TCheckBox
    Left = 376
    Top = 360
    Width = 217
    Height = 17
    Caption = 'Large combined graph all criteria'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 376
    Top = 384
    Width = 177
    Height = 17
    Caption = 'Panels by test DEM'
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 376
    Top = 407
    Width = 145
    Height = 17
    Caption = 'Movie by test DEM '
    TabOrder = 3
    OnClick = CheckBox3Click
  end
  object BitBtn3: TBitBtn
    Left = 776
    Top = 633
    Width = 161
    Height = 25
    Caption = 'Close graphs and images'
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 1269
    Top = 590
    Width = 69
    Height = 23
    TabOrder = 5
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 288
    Top = 327
    Width = 65
    Height = 23
    TabOrder = 6
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 288
    Top = 306
    Width = 65
    Height = 23
    TabOrder = 7
    Text = 'Edit1'
    OnChange = Edit3Change
  end
  object BitBtn5: TBitBtn
    Left = 24
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Scatterplot'
    TabOrder = 8
    OnClick = BitBtn5Click
  end
  object RadioGroup4: TRadioGroup
    Left = 711
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
    TabOrder = 9
    OnClick = RadioGroup4Click
  end
  object BitBtn6: TBitBtn
    Left = 24
    Top = 337
    Width = 75
    Height = 25
    Caption = 'Histograms'
    TabOrder = 10
    OnClick = BitBtn6Click
  end
  object RadioGroup5: TRadioGroup
    Left = 368
    Top = 296
    Width = 201
    Height = 41
    Caption = 'Symbol size'
    Columns = 5
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5')
    TabOrder = 11
    OnClick = RadioGroup5Click
  end
  object BitBtn12: TBitBtn
    Left = 335
    Top = 194
    Width = 209
    Height = 25
    Caption = 'Whisker plots by parameter/criterion'
    TabOrder = 12
    OnClick = BitBtn12Click
  end
  object CheckBox4: TCheckBox
    Left = 376
    Top = 440
    Width = 161
    Height = 17
    Caption = 'Graph retired DEMs'
    TabOrder = 13
    OnClick = CheckBox4Click
  end
  object BitBtn16: TBitBtn
    Left = 744
    Top = 560
    Width = 159
    Height = 25
    Caption = 'Best eval colored by slope'
    TabOrder = 14
    OnClick = BitBtn16Click
  end
  object BitBtn2: TBitBtn
    Left = 511
    Top = 633
    Width = 138
    Height = 25
    Caption = 'Merge graph panels'
    TabOrder = 15
    OnClick = BitBtn2Click
  end
  object BitBtn17: TBitBtn
    Left = 644
    Top = 493
    Width = 269
    Height = 25
    Caption = 'Best eval graph for each criterion by filters'
    TabOrder = 16
    OnClick = BitBtn17Click
  end
  object BitBtn23: TBitBtn
    Left = 671
    Top = 633
    Width = 90
    Height = 25
    Caption = 'Merge graphs'
    TabOrder = 17
    OnClick = BitBtn23Click
  end
  object Edit4: TEdit
    Left = 504
    Top = 474
    Width = 65
    Height = 23
    TabOrder = 18
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object Edit5: TEdit
    Left = 511
    Top = 500
    Width = 42
    Height = 23
    TabOrder = 19
    Text = 'Edit5'
    OnChange = Edit5Change
  end
  object BitBtn27: TBitBtn
    Left = 1344
    Top = 343
    Width = 75
    Height = 25
    Caption = 'New DB'
    TabOrder = 20
    OnClick = BitBtn27Click
  end
  object RadioGroup7: TRadioGroup
    Left = 960
    Top = 566
    Width = 113
    Height = 75
    Caption = 'Group Won/Lost'
    ItemIndex = 0
    Items.Strings = (
      'By test DEM'
      'By criteriion')
    TabOrder = 21
    OnClick = RadioGroup7Click
  end
  object BitBtn28: TBitBtn
    Left = 8
    Top = 194
    Width = 159
    Height = 25
    Caption = 'Best eval graph per criterion'
    TabOrder = 22
    OnClick = BitBtn28Click
  end
  object BitBtn29: TBitBtn
    Left = 8
    Top = 232
    Width = 169
    Height = 25
    Caption = 'Comparison DEM wins w/ filters'
    TabOrder = 23
    OnClick = BitBtn29Click
  end
  object BitBtn30: TBitBtn
    Left = 1439
    Top = 352
    Width = 18
    Height = 18
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADA00000000000000000FF0FEF4FEF4FEF00FF0EFE4EFE4EFE00FF044444444
      44400FF0EFE4EFE4EFE00000FEF4FEF4FEF00FF04444444444400FF0FEF4FEF4
      FEF00FF0EFE4EFE4EFE000000000000000000FF0FFFFF0FFFFF00FF0FFFFF0FF
      FFF00000000000000000DADADADADADADADAADADADADADADADAD}
    TabOrder = 24
    OnClick = BitBtn30Click
  end
  object GroupBox1: TGroupBox
    Left = 1088
    Top = 16
    Width = 217
    Height = 257
    Caption = 'Criteria'
    TabOrder = 25
    object Memo1: TMemo
      Left = 3
      Top = 21
      Width = 121
      Height = 220
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object BitBtn21: TBitBtn
      Left = 130
      Top = 127
      Width = 75
      Height = 25
      Caption = 'All in DB'
      TabOrder = 1
      OnClick = BitBtn21Click
    end
    object BitBtn20: TBitBtn
      Left = 130
      Top = 96
      Width = 75
      Height = 25
      Caption = 'Save to file'
      TabOrder = 2
      OnClick = BitBtn20Click
    end
    object BitBtn19: TBitBtn
      Left = 130
      Top = 65
      Width = 75
      Height = 25
      Caption = 'Load file'
      TabOrder = 3
      OnClick = BitBtn19Click
    end
    object BitBtn18: TBitBtn
      Left = 130
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Pick'
      TabOrder = 4
      OnClick = BitBtn18Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 891
    Top = 16
    Width = 185
    Height = 231
    Caption = 'Test DEMs'
    TabOrder = 26
    object Memo3: TMemo
      Left = 3
      Top = 24
      Width = 102
      Height = 193
      Lines.Strings = (
        'COP'
        'TANDEM'
        'ALOS'
        'NASA'
        'ASTER'
        'FABDEM'
        'COAST'
        'DILUV'
        'DELTA')
      TabOrder = 0
    end
    object BitBtn36: TBitBtn
      Left = 131
      Top = 144
      Width = 51
      Height = 25
      Caption = 'All in DB'
      TabOrder = 1
      OnClick = BitBtn36Click
    end
  end
  object BitBtn33: TBitBtn
    Left = 344
    Top = 225
    Width = 209
    Height = 25
    Caption = 'Whister plots by slope categories'
    TabOrder = 27
    OnClick = BitBtn33Click
  end
  object BitBtn34: TBitBtn
    Left = 344
    Top = 256
    Width = 209
    Height = 25
    Caption = 'Whisker plots by clusters'
    TabOrder = 28
    OnClick = BitBtn34Click
  end
  object BitBtn35: TBitBtn
    Left = 384
    Top = 535
    Width = 185
    Height = 25
    Caption = 'Cluster maps'
    TabOrder = 29
    OnClick = BitBtn35Click
  end
  object Memo4: TMemo
    Left = 32
    Top = 499
    Width = 225
    Height = 89
    Lines.Strings = (
      'CLUSTER <=3'
      'CLUSTER >=4 AND CLUSTER <=6'
      'CLUSTER >=7')
    TabOrder = 30
  end
  object ComboBox1: TComboBox
    Left = 199
    Top = 233
    Width = 88
    Height = 23
    TabOrder = 31
    Text = 'COP'
  end
  object Edit6: TEdit
    Left = 1269
    Top = 561
    Width = 84
    Height = 23
    TabOrder = 32
    Text = 'Edit6'
    OnChange = Edit6Change
  end
  object RadioGroup8: TRadioGroup
    Left = 184
    Top = 16
    Width = 145
    Height = 172
    Caption = 'Average ranks'
    Items.Strings = (
      'Selected filters'
      'Slope intervals'
      'Roughness intervals'
      'Forest percentages'
      'Barren percentages'
      'Multiple panels')
    TabOrder = 33
    OnClick = RadioGroup8Click
  end
  object RadioGroup9: TRadioGroup
    Left = 8
    Top = 16
    Width = 145
    Height = 172
    Caption = 'Average evaluation'
    Items.Strings = (
      'Selected filters'
      'Slope intervals'
      'Roughness intervals'
      'Forest percentages'
      'Barren percentages'
      'Multiple panels')
    TabOrder = 34
    OnClick = RadioGroup9Click
  end
  object BitBtn22: TBitBtn
    Left = 1320
    Top = 498
    Width = 105
    Height = 25
    Caption = 'Legend'
    TabOrder = 35
    OnClick = BitBtn22Click
  end
  object RadioGroup1: TRadioGroup
    Left = 344
    Top = 16
    Width = 177
    Height = 172
    Caption = 'Graph, Y axis sort by'
    Items.Strings = (
      'Best evaluation'
      'Tile average slope'
      'Tile average roughness'
      'Tile relief'
      'Tile percent forested'
      'Tile percent barren')
    TabOrder = 36
    OnClick = RadioGroup1Click
  end
  object GroupBox3: TGroupBox
    Left = 1328
    Top = 8
    Width = 129
    Height = 193
    Caption = 'Filters'
    TabOrder = 37
    object Memo2: TMemo
      Left = 3
      Top = 29
      Width = 114
      Height = 119
      Lines.Strings = (
        'AVG_SLOPE<5'
        '(None)'
        'AVG_SLOPE>5'
        'AVG_SLOPE>30'
        'AVG_SLOPE>55'
        'BARREN_PC>40')
      TabOrder = 0
    end
  end
  object GroupBox4: TGroupBox
    Left = 1136
    Top = 345
    Width = 185
    Height = 96
    Caption = 'Multiple elevation range DBs'
    TabOrder = 38
    object BitBtn4: TBitBtn
      Left = 14
      Top = 24
      Width = 83
      Height = 25
      Caption = 'Load 4 DBs'
      TabOrder = 0
      OnClick = BitBtn4Click
    end
    object BitBtn10: TBitBtn
      Left = 16
      Top = 55
      Width = 89
      Height = 25
      Caption = 'Multiple wins'
      TabOrder = 1
      OnClick = BitBtn10Click
    end
  end
  object GroupBox5: TGroupBox
    Left = 644
    Top = 253
    Width = 259
    Height = 169
    Caption = 'GroupBox5'
    TabOrder = 39
    object Label7: TLabel
      Left = 120
      Top = 32
      Width = 30
      Height = 15
      Caption = 'X axis'
    end
    object Label8: TLabel
      Left = 120
      Top = 64
      Width = 30
      Height = 15
      Caption = 'Y axis'
    end
    object Label9: TLabel
      Left = 119
      Top = 85
      Width = 28
      Height = 15
      Caption = 'Value'
    end
    object ComboBox2: TComboBox
      Left = 16
      Top = 24
      Width = 97
      Height = 23
      TabOrder = 0
      Text = 'BARREN_PC'
    end
    object ComboBox3: TComboBox
      Left = 16
      Top = 51
      Width = 97
      Height = 23
      TabOrder = 1
      Text = 'AVG_SLOPE'
    end
    object ComboBox4: TComboBox
      Left = 16
      Top = 80
      Width = 97
      Height = 23
      TabOrder = 2
      Text = 'BEST_EVAL'
    end
    object BitBtn1: TBitBtn
      Left = 168
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Graphs'
      TabOrder = 3
      OnClick = BitBtn1Click
    end
    object RadioGroup3: TRadioGroup
      Left = 17
      Top = 106
      Width = 89
      Height = 60
      Caption = 'Symbols'
      ItemIndex = 0
      Items.Strings = (
        'Squares'
        'Circles')
      TabOrder = 4
      OnClick = RadioGroup3Click
    end
    object RadioGroup6: TRadioGroup
      Left = 128
      Top = 101
      Width = 99
      Height = 65
      Caption = '2 Geomorph Plots'
      ItemIndex = 0
      Items.Strings = (
        'Pie charts'
        'All points')
      TabOrder = 5
      OnClick = RadioGroup6Click
    end
  end
end
