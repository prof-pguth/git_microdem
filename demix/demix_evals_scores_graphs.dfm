object eval_scores_graph_form: Teval_scores_graph_form
  Left = 0
  Top = 0
  Caption = 'Add tolerances to table'
  ClientHeight = 645
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
  object Label6: TLabel
    Left = 1176
    Top = 564
    Width = 79
    Height = 15
    Caption = 'Graph font size'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Graph by best evaluation'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 368
    Top = 112
    Width = 153
    Height = 74
    Caption = 'Average multiple criteria'
    ItemIndex = 1
    Items.Strings = (
      'Test areas'
      'DEMIX tiles')
    TabOrder = 1
    OnClick = RadioGroup2Click
  end
  object CheckBox1: TCheckBox
    Left = 376
    Top = 360
    Width = 217
    Height = 17
    Caption = 'Large combined graph all criteria'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 376
    Top = 384
    Width = 177
    Height = 17
    Caption = 'Panels by test DEM'
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 376
    Top = 407
    Width = 145
    Height = 17
    Caption = 'Movie by test DEM '
    TabOrder = 4
    OnClick = CheckBox3Click
  end
  object BitBtn3: TBitBtn
    Left = 600
    Top = 8
    Width = 161
    Height = 25
    Caption = 'Close graphs and images'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 1269
    Top = 590
    Width = 69
    Height = 23
    TabOrder = 6
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 288
    Top = 327
    Width = 65
    Height = 23
    TabOrder = 7
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 288
    Top = 306
    Width = 65
    Height = 23
    TabOrder = 8
    Text = 'Edit1'
    OnChange = Edit3Change
  end
  object BitBtn5: TBitBtn
    Left = 24
    Top = 368
    Width = 75
    Height = 25
    Caption = 'Scatterplot'
    TabOrder = 9
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
    TabOrder = 10
    OnClick = RadioGroup4Click
  end
  object BitBtn6: TBitBtn
    Left = 24
    Top = 337
    Width = 75
    Height = 25
    Caption = 'Histograms'
    TabOrder = 11
    OnClick = BitBtn6Click
  end
  object BitBtn7: TBitBtn
    Left = 8
    Top = 132
    Width = 145
    Height = 25
    Caption = 'Graph by average slope'
    TabOrder = 12
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 8
    Top = 163
    Width = 145
    Height = 25
    Caption = 'Graph by avg roughness'
    TabOrder = 13
    OnClick = BitBtn8Click
  end
  object BitBtn9: TBitBtn
    Left = 8
    Top = 101
    Width = 145
    Height = 25
    Caption = 'Graph by relief'
    TabOrder = 14
    OnClick = BitBtn9Click
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
    TabOrder = 15
    OnClick = RadioGroup5Click
  end
  object BitBtn11: TBitBtn
    Left = 8
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Graph by tile barren %'
    TabOrder = 16
    OnClick = BitBtn11Click
  end
  object BitBtn12: TBitBtn
    Left = 840
    Top = 498
    Width = 209
    Height = 25
    Caption = 'Whisker plots by parameter/criterion'
    TabOrder = 17
    OnClick = BitBtn12Click
  end
  object BitBtn13: TBitBtn
    Left = 128
    Top = 399
    Width = 193
    Height = 25
    Caption = 'Cop wins,  slope/barren'
    TabOrder = 18
    OnClick = BitBtn13Click
  end
  object CheckBox4: TCheckBox
    Left = 376
    Top = 440
    Width = 161
    Height = 17
    Caption = 'Graph retired DEMs'
    TabOrder = 19
    OnClick = CheckBox4Click
  end
  object BitBtn14: TBitBtn
    Left = 8
    Top = 70
    Width = 145
    Height = 25
    Caption = 'Graph by tile forest %'
    TabOrder = 20
    OnClick = BitBtn14Click
  end
  object BitBtn15: TBitBtn
    Left = 24
    Top = 440
    Width = 145
    Height = 25
    Caption = 'Best eval by criterion'
    TabOrder = 21
  end
  object BitBtn16: TBitBtn
    Left = 568
    Top = 437
    Width = 145
    Height = 25
    Caption = 'Best eval colored by slope'
    TabOrder = 22
    OnClick = BitBtn16Click
  end
  object BitBtn2: TBitBtn
    Left = 335
    Top = 8
    Width = 138
    Height = 25
    Caption = 'Merge graph panels'
    TabOrder = 23
    OnClick = BitBtn2Click
  end
  object BitBtn17: TBitBtn
    Left = 904
    Top = 265
    Width = 159
    Height = 25
    Caption = 'Best eval graph by filter'
    TabOrder = 24
    OnClick = BitBtn17Click
  end
  object BitBtn23: TBitBtn
    Left = 495
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Merge graphs'
    TabOrder = 25
    OnClick = BitBtn23Click
  end
  object BitBtn24: TBitBtn
    Left = 744
    Top = 370
    Width = 185
    Height = 25
    Caption = 'Best eval, by slope/barren'
    TabOrder = 26
    OnClick = BitBtn24Click
  end
  object Edit4: TEdit
    Left = 808
    Top = 306
    Width = 65
    Height = 23
    TabOrder = 27
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
    TabOrder = 28
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
    TabOrder = 29
    OnClick = RadioGroup6Click
  end
  object BitBtn25: TBitBtn
    Left = 744
    Top = 432
    Width = 185
    Height = 25
    Caption = 'COP FUV by slope/barren'
    TabOrder = 30
    OnClick = BitBtn25Click
  end
  object BitBtn26: TBitBtn
    Left = 744
    Top = 401
    Width = 185
    Height = 25
    Caption = 'ALOS FUV by slope/barren'
    TabOrder = 31
    OnClick = BitBtn26Click
  end
  object Edit5: TEdit
    Left = 991
    Top = 37
    Width = 42
    Height = 23
    TabOrder = 32
    Text = 'Edit5'
    OnChange = Edit5Change
  end
  object BitBtn27: TBitBtn
    Left = 920
    Top = 72
    Width = 75
    Height = 25
    Caption = 'New DB'
    TabOrder = 33
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
    TabOrder = 34
    OnClick = RadioGroup7Click
  end
  object Memo2: TMemo
    Left = 895
    Top = 103
    Width = 114
    Height = 119
    Lines.Strings = (
      'AVG_SLOPE<5'
      '(None)'
      'AVG_SLOPE>5'
      'AVG_SLOPE>30'
      'AVG_SLOPE>55'
      'BARREN_PC>40')
    TabOrder = 35
  end
  object BitBtn28: TBitBtn
    Left = 904
    Top = 233
    Width = 159
    Height = 25
    Caption = 'Best eval graph per criterion'
    TabOrder = 36
    OnClick = BitBtn28Click
  end
  object BitBtn29: TBitBtn
    Left = 8
    Top = 232
    Width = 169
    Height = 25
    Caption = 'Comparison DEM wins w/ filters'
    TabOrder = 37
    OnClick = BitBtn29Click
  end
  object BitBtn30: TBitBtn
    Left = 1015
    Top = 81
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
    TabOrder = 38
    OnClick = BitBtn30Click
  end
  object BitBtn31: TBitBtn
    Left = 400
    Top = 537
    Width = 185
    Height = 25
    Caption = 'Average ranks with filters'
    TabOrder = 39
    OnClick = BitBtn31Click
  end
  object BitBtn32: TBitBtn
    Left = 944
    Top = 359
    Width = 105
    Height = 25
    Caption = 'Wins with filters'
    Enabled = False
    TabOrder = 40
    OnClick = BitBtn32Click
  end
  object GroupBox1: TGroupBox
    Left = 1088
    Top = 16
    Width = 217
    Height = 257
    Caption = 'Criteria'
    TabOrder = 41
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
    Left = 1091
    Top = 306
    Width = 185
    Height = 231
    Caption = 'Test DEMs'
    TabOrder = 42
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
    Left = 840
    Top = 528
    Width = 209
    Height = 25
    Caption = 'Whister plots by slope categories'
    TabOrder = 43
    OnClick = BitBtn33Click
  end
  object BitBtn34: TBitBtn
    Left = 840
    Top = 560
    Width = 209
    Height = 25
    Caption = 'Whisker plots by clusters'
    TabOrder = 44
    OnClick = BitBtn34Click
  end
  object BitBtn35: TBitBtn
    Left = 400
    Top = 480
    Width = 185
    Height = 25
    Caption = 'Cluster maps'
    TabOrder = 45
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
    TabOrder = 46
  end
  object ComboBox1: TComboBox
    Left = 199
    Top = 233
    Width = 88
    Height = 23
    TabOrder = 47
    Text = 'COP'
  end
  object Edit6: TEdit
    Left = 1269
    Top = 561
    Width = 84
    Height = 23
    TabOrder = 48
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
    TabOrder = 49
    OnClick = RadioGroup8Click
  end
  object RadioGroup9: TRadioGroup
    Left = 1301
    Top = 309
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
    TabOrder = 50
    OnClick = RadioGroup9Click
  end
  object BitBtn4: TBitBtn
    Left = 1368
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Load 4 DBs'
    TabOrder = 51
    OnClick = BitBtn4Click
  end
  object BitBtn10: TBitBtn
    Left = 1368
    Top = 81
    Width = 89
    Height = 25
    Caption = 'Multiple wins'
    TabOrder = 52
    OnClick = BitBtn10Click
  end
  object BitBtn22: TBitBtn
    Left = 1320
    Top = 498
    Width = 105
    Height = 25
    Caption = 'Legend'
    TabOrder = 53
    OnClick = BitBtn22Click
  end
end
