object eval_scores_graph_form: Teval_scores_graph_form
  Left = 0
  Top = 0
  Caption = 'DEMIX DB graphs'
  ClientHeight = 907
  ClientWidth = 1614
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
    Left = 383
    Top = 558
    Width = 86
    Height = 15
    Caption = 'Legend font size'
  end
  object Label2: TLabel
    Left = 384
    Top = 412
    Width = 63
    Height = 15
    Caption = 'Graph y size'
  end
  object Label3: TLabel
    Left = 384
    Top = 386
    Width = 62
    Height = 15
    Caption = 'Graph x size'
  end
  object Label4: TLabel
    Left = 382
    Top = 477
    Width = 106
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
    Left = 382
    Top = 531
    Width = 79
    Height = 15
    Caption = 'Graph font size'
  end
  object RadioGroup2: TRadioGroup
    Left = 1064
    Top = 8
    Width = 153
    Height = 106
    Caption = 'Average multiple criteria '
    Items.Strings = (
      'Test areas (avg scores)'
      'DEMIX tiles'
      'Test areas (avg evals)'
      'Test areas (each eval)')
    TabOrder = 0
    OnClick = RadioGroup2Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 76
    Width = 217
    Height = 17
    Caption = 'Large combined graph all criteria'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 9
    Top = 99
    Width = 177
    Height = 17
    Caption = 'Panels by test DEM'
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 9
    Top = 122
    Width = 145
    Height = 17
    Caption = 'Movie by test DEM '
    TabOrder = 3
    OnClick = CheckBox3Click
  end
  object BitBtn3: TBitBtn
    Left = 280
    Top = 598
    Width = 161
    Height = 25
    Caption = 'Close graphs and images'
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object Edit1: TEdit
    Left = 475
    Top = 557
    Width = 69
    Height = 23
    TabOrder = 5
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 464
    Top = 404
    Width = 65
    Height = 23
    TabOrder = 6
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 464
    Top = 383
    Width = 65
    Height = 23
    TabOrder = 7
    Text = 'Edit1'
    OnChange = Edit3Change
  end
  object BitBtn5: TBitBtn
    Left = 1176
    Top = 719
    Width = 75
    Height = 25
    Caption = 'Scatterplot'
    TabOrder = 8
    OnClick = BitBtn5Click
  end
  object RadioGroup4: TRadioGroup
    Left = 1064
    Top = 135
    Width = 130
    Height = 209
    Caption = 'Scatter plot SSIM/FUV'
    Enabled = False
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
    Left = 1176
    Top = 688
    Width = 75
    Height = 25
    Caption = 'Histograms'
    TabOrder = 10
    OnClick = BitBtn6Click
  end
  object RadioGroup5: TRadioGroup
    Left = 782
    Top = 520
    Width = 193
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
    Left = 599
    Top = 711
    Width = 209
    Height = 25
    Caption = 'Whisker plots by parameter/criterion'
    TabOrder = 12
    OnClick = BitBtn12Click
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 598
    Width = 138
    Height = 25
    Caption = 'Merge graph panels'
    TabOrder = 13
    OnClick = BitBtn2Click
  end
  object BitBtn23: TBitBtn
    Left = 167
    Top = 598
    Width = 90
    Height = 25
    Caption = 'Merge graphs'
    TabOrder = 14
    OnClick = BitBtn23Click
  end
  object Edit4: TEdit
    Left = 504
    Top = 474
    Width = 49
    Height = 23
    TabOrder = 15
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object Edit5: TEdit
    Left = 511
    Top = 500
    Width = 42
    Height = 23
    TabOrder = 16
    Text = 'Edit5'
    OnChange = Edit5Change
  end
  object BitBtn27: TBitBtn
    Left = 134
    Top = 633
    Width = 75
    Height = 25
    Caption = 'New DB'
    TabOrder = 17
    OnClick = BitBtn27Click
  end
  object RadioGroup7: TRadioGroup
    Left = 1410
    Top = 147
    Width = 113
    Height = 75
    Caption = 'Group Won/Lost'
    ItemIndex = 0
    Items.Strings = (
      'By test DEM'
      'By criterion')
    TabOrder = 18
    OnClick = RadioGroup7Click
  end
  object BitBtn30: TBitBtn
    Left = 229
    Top = 642
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
    TabOrder = 19
    OnClick = BitBtn30Click
  end
  object GroupBox1: TGroupBox
    Left = 588
    Top = 8
    Width = 217
    Height = 231
    Caption = 'Criteria'
    TabOrder = 20
    object Memo1: TMemo
      Left = 3
      Top = 21
      Width = 121
      Height = 193
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
    object BitBtn9: TBitBtn
      Left = 130
      Top = 158
      Width = 75
      Height = 25
      Caption = 'Ranked order'
      TabOrder = 5
      OnClick = BitBtn9Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 384
    Top = 8
    Width = 185
    Height = 231
    Caption = 'Test DEMs'
    TabOrder = 21
    object Memo3: TMemo
      Left = 3
      Top = 20
      Width = 102
      Height = 193
      TabOrder = 0
    end
    object BitBtn11: TBitBtn
      Left = 111
      Top = 96
      Width = 75
      Height = 25
      Caption = 'Pick'
      TabOrder = 1
      OnClick = BitBtn11Click
    end
    object BitBtn42: TBitBtn
      Left = 112
      Top = 136
      Width = 75
      Height = 25
      Caption = 'Load file'
      TabOrder = 2
      OnClick = BitBtn42Click
    end
  end
  object BitBtn33: TBitBtn
    Left = 595
    Top = 680
    Width = 209
    Height = 25
    Caption = 'Whisker plots by slope categories'
    TabOrder = 22
    OnClick = BitBtn33Click
  end
  object BitBtn34: TBitBtn
    Left = 595
    Top = 649
    Width = 209
    Height = 25
    Caption = 'Whisker plots by clusters'
    TabOrder = 23
    OnClick = BitBtn34Click
  end
  object BitBtn35: TBitBtn
    Left = 595
    Top = 618
    Width = 185
    Height = 25
    Caption = 'Cluster maps'
    TabOrder = 24
    OnClick = BitBtn35Click
  end
  object Memo4: TMemo
    Left = 1233
    Top = 415
    Width = 225
    Height = 89
    Lines.Strings = (
      'CLUSTER <=3'
      'CLUSTER >=4 AND CLUSTER <=6'
      'CLUSTER >=7')
    TabOrder = 25
  end
  object Edit6: TEdit
    Left = 475
    Top = 528
    Width = 69
    Height = 23
    TabOrder = 26
    Text = 'Edit6'
    OnChange = Edit6Change
  end
  object RadioGroup9: TRadioGroup
    Left = 1336
    Top = 557
    Width = 187
    Height = 122
    Caption = 'Filtered evaluation'
    Items.Strings = (
      'Selected mixed filters'
      'Land type filters'
      'Panels multiple criteria')
    TabOrder = 27
    OnClick = RadioGroup9Click
  end
  object BitBtn22: TBitBtn
    Left = 464
    Top = 598
    Width = 105
    Height = 25
    Caption = 'DEM Legend'
    TabOrder = 28
    OnClick = BitBtn22Click
  end
  object GroupBox3: TGroupBox
    Left = 827
    Top = 8
    Width = 161
    Height = 92
    Caption = 'Mixed Filters'
    TabOrder = 29
    object ComboBox6: TComboBox
      Left = 3
      Top = 26
      Width = 126
      Height = 23
      TabOrder = 0
      Text = 'filters_avg_slope'
      OnChange = ComboBox6Change
      Items.Strings = (
        'filters_avg_slope'
        'filters_forest_pc'
        'filters_barren_pc')
    end
    object ComboBox7: TComboBox
      Left = 3
      Top = 55
      Width = 126
      Height = 23
      TabOrder = 1
      Text = 'filters_barren_pc'
      OnChange = ComboBox7Change
      Items.Strings = (
        'filters_avg_slope'
        'filters_forest_pc'
        'filters_barren_pc')
    end
  end
  object GroupBox4: TGroupBox
    Left = 1396
    Top = 8
    Width = 185
    Height = 122
    Caption = 'Multiple elevation range DBs'
    TabOrder = 30
    object BitBtn4: TBitBtn
      Left = 14
      Top = 24
      Width = 107
      Height = 25
      Caption = 'Load 4 DBs'
      TabOrder = 0
      OnClick = BitBtn4Click
    end
    object BitBtn10: TBitBtn
      Left = 16
      Top = 55
      Width = 113
      Height = 25
      Caption = 'Multiple wins'
      TabOrder = 1
      OnClick = BitBtn10Click
    end
    object BitBtn8: TBitBtn
      Left = 16
      Top = 88
      Width = 113
      Height = 25
      Caption = 'Criteria performance'
      TabOrder = 2
      OnClick = BitBtn8Click
    end
  end
  object GroupBox5: TGroupBox
    Left = 579
    Top = 287
    Width = 406
    Height = 183
    Caption = 'Pie charts for two land types'
    TabOrder = 31
    object Label7: TLabel
      Left = 120
      Top = 32
      Width = 29
      Height = 15
      Caption = 'X axis'
    end
    object Label8: TLabel
      Left = 119
      Top = 59
      Width = 29
      Height = 15
      Caption = 'Y axis'
    end
    object Label9: TLabel
      Left = 120
      Top = 88
      Width = 34
      Height = 15
      Caption = 'DEM 1'
    end
    object Label10: TLabel
      Left = 119
      Top = 120
      Width = 34
      Height = 15
      Caption = 'DEM 2'
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
    object BitBtn1: TBitBtn
      Left = 16
      Top = 141
      Width = 97
      Height = 25
      Caption = 'Pie Graphs'
      TabOrder = 2
      OnClick = BitBtn1Click
    end
    object RadioGroup3: TRadioGroup
      Left = 233
      Top = 13
      Width = 152
      Height = 60
      Caption = 'Symbols'
      ItemIndex = 0
      Items.Strings = (
        'Squares'
        'Circles')
      TabOrder = 3
      OnClick = RadioGroup3Click
    end
    object RadioGroup10: TRadioGroup
      Left = 233
      Top = 79
      Width = 152
      Height = 90
      Caption = 'Comparison'
      ItemIndex = 1
      Items.Strings = (
        'Compare two DEMs'
        'Compare all DEMs'
        'Best evaluation')
      TabOrder = 4
    end
    object ComboBox4: TComboBox
      Left = 16
      Top = 80
      Width = 97
      Height = 23
      TabOrder = 5
      Text = 'ComboBox4'
    end
    object ComboBox5: TComboBox
      Left = 16
      Top = 112
      Width = 97
      Height = 23
      TabOrder = 6
      Text = 'ComboBox5'
    end
  end
  object BitBtn38: TBitBtn
    Left = 8
    Top = 629
    Width = 113
    Height = 29
    Caption = 'Save defaults'
    TabOrder = 32
    OnClick = BitBtn38Click
  end
  object BitBtn13: TBitBtn
    Left = 159
    Top = 287
    Width = 193
    Height = 25
    Caption = 'Winning percentages (2 DEM lists)'
    TabOrder = 33
    OnClick = BitBtn13Click
  end
  object CheckBox5: TCheckBox
    Left = 8
    Top = 8
    Width = 153
    Height = 17
    Caption = 'FUV expand scale'
    TabOrder = 34
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 8
    Top = 31
    Width = 169
    Height = 17
    Caption = 'Use median for statistics'
    TabOrder = 35
    OnClick = CheckBox6Click
  end
  object BitBtn31: TBitBtn
    Left = 595
    Top = 773
    Width = 193
    Height = 25
    Caption = 'Scatter plots, DEMs/criteria'
    TabOrder = 36
    OnClick = BitBtn31Click
  end
  object GroupBox6: TGroupBox
    Left = 358
    Top = 252
    Width = 163
    Height = 109
    Caption = 'Pie chart one mixed filter'
    TabOrder = 37
    object CheckBox9: TCheckBox
      Left = 3
      Top = 47
      Width = 177
      Height = 17
      Caption = 'Unrestricted/restricted'
      TabOrder = 0
    end
    object CheckBox7: TCheckBox
      Left = 3
      Top = 24
      Width = 97
      Height = 17
      Caption = 'US/Non-US'
      TabOrder = 1
    end
    object BitBtn36: TBitBtn
      Left = 19
      Top = 70
      Width = 75
      Height = 25
      Caption = 'Winning pies'
      TabOrder = 2
      OnClick = BitBtn36Click
    end
  end
  object CheckBox15: TCheckBox
    Left = 8
    Top = 53
    Width = 169
    Height = 17
    Caption = 'Average multiple criteria'
    TabOrder = 38
    OnClick = CheckBox15Click
  end
  object RadioGroup6: TRadioGroup
    Left = 780
    Top = 476
    Width = 193
    Height = 38
    Caption = 'Series symbols'
    Columns = 3
    Items.Strings = (
      'Points'
      'Lines'
      'Both')
    TabOrder = 39
    OnClick = RadioGroup6Click
  end
  object GroupBox7: TGroupBox
    Left = 8
    Top = 228
    Width = 309
    Height = 53
    Caption = 'Commpare one DEM to all others'
    TabOrder = 40
    object BitBtn29: TBitBtn
      Left = 8
      Top = 17
      Width = 186
      Height = 25
      Caption = 'Comparison DEM wins w/ filters'
      TabOrder = 0
      OnClick = BitBtn29Click
    end
    object ComboBox1: TComboBox
      Left = 218
      Top = 19
      Width = 88
      Height = 23
      TabOrder = 1
      Text = 'COP'
    end
  end
  object BitBtn37: TBitBtn
    Left = 8
    Top = 340
    Width = 130
    Height = 30
    Caption = 'Box plot evals (1 filter)'
    TabOrder = 41
    OnClick = BitBtn37Click
  end
  object BitBtn40: TBitBtn
    Left = 8
    Top = 376
    Width = 130
    Height = 30
    Caption = 'Box plot evals (2 filters)'
    TabOrder = 42
    OnClick = BitBtn40Click
  end
  object GroupBox8: TGroupBox
    Left = 830
    Top = 106
    Width = 158
    Height = 151
    Caption = 'Standard landscape filters'
    TabOrder = 43
    object CheckBox12: TCheckBox
      Left = 16
      Top = 122
      Width = 129
      Height = 17
      Caption = 'Forest percentage'
      TabOrder = 0
      OnClick = CheckBox12Click
    end
    object CheckBox13: TCheckBox
      Left = 16
      Top = 79
      Width = 145
      Height = 17
      Caption = 'Urban percentage'
      TabOrder = 1
      OnClick = CheckBox13Click
    end
    object CheckBox11: TCheckBox
      Left = 16
      Top = 102
      Width = 129
      Height = 17
      Caption = 'Barren percentage'
      TabOrder = 2
      OnClick = CheckBox11Click
    end
    object CheckBox10: TCheckBox
      Left = 16
      Top = 56
      Width = 129
      Height = 17
      Caption = 'Average roughness'
      TabOrder = 3
      OnClick = CheckBox10Click
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 33
      Width = 97
      Height = 17
      Caption = 'Average slope'
      TabOrder = 4
      OnClick = CheckBox8Click
    end
  end
  object BitBtn41: TBitBtn
    Left = 600
    Top = 520
    Width = 127
    Height = 25
    Caption = 'Criteria/DEM summary'
    TabOrder = 44
    OnClick = BitBtn41Click
  end
  object RadioGroup12: TRadioGroup
    Left = 782
    Top = 567
    Width = 191
    Height = 44
    Caption = 'Line width'
    Columns = 5
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5')
    TabOrder = 45
    OnClick = RadioGroup12Click
  end
  object GroupBox9: TGroupBox
    Left = 8
    Top = 688
    Width = 169
    Height = 135
    Caption = 'Graphs of filters versus FUV'
    TabOrder = 46
    object BitBtn39: TBitBtn
      Left = 16
      Top = 85
      Width = 130
      Height = 25
      Caption = 'Panels multiple criteria'
      TabOrder = 0
      OnClick = BitBtn39Click
    end
    object BitBtn7: TBitBtn
      Left = 16
      Top = 23
      Width = 130
      Height = 25
      Caption = 'Single mixed filter'
      TabOrder = 1
      OnClick = BitBtn7Click
    end
    object BitBtn25: TBitBtn
      Left = 16
      Top = 54
      Width = 130
      Height = 25
      Caption = 'Standard landscape filters'
      TabOrder = 2
      OnClick = BitBtn25Click
    end
  end
  object GroupBox10: TGroupBox
    Left = 260
    Top = 688
    Width = 229
    Height = 145
    Caption = 'Graphs of FUV versus criteria'
    TabOrder = 47
    object BitBtn26: TBitBtn
      Left = 20
      Top = 110
      Width = 193
      Height = 25
      Caption = 'Criteria FUV, just DB filter'
      TabOrder = 0
      OnClick = BitBtn26Click
    end
    object BitBtn24: TBitBtn
      Left = 20
      Top = 79
      Width = 193
      Height = 25
      Caption = 'Criteria FUV, 2 mixed filter grid'
      TabOrder = 1
      OnClick = BitBtn24Click
    end
    object BitBtn15: TBitBtn
      Left = 20
      Top = 48
      Width = 193
      Height = 25
      Caption = 'Criteria FUV, 1 mixed filter'
      TabOrder = 2
      OnClick = BitBtn15Click
    end
    object BitBtn14: TBitBtn
      Left = 20
      Top = 17
      Width = 193
      Height = 25
      Caption = 'Criteria FUV, standard filters'
      TabOrder = 3
      OnClick = BitBtn14Click
    end
  end
  object GroupBox11: TGroupBox
    Left = 167
    Top = 322
    Width = 185
    Height = 84
    Caption = 'Graph wth Y axis sort on'
    TabOrder = 48
    object ComboBox8: TComboBox
      Left = 16
      Top = 24
      Width = 145
      Height = 23
      TabOrder = 0
      Text = 'Tile average slope'
      Items.Strings = (
        'Best evaluation'
        'Tile average slope'
        'Tile average roughness'
        'Tile relief'
        'Tile percent barren'
        'Tile percent forest'
        'Tile latitude'
        'Best evaluation, slope colors')
    end
    object BitBtn32: TBitBtn
      Left = 48
      Top = 53
      Width = 75
      Height = 25
      Caption = 'Graphs'
      TabOrder = 1
      OnClick = BitBtn32Click
    end
  end
  object GroupBox12: TGroupBox
    Left = 8
    Top = 423
    Width = 235
    Height = 150
    Caption = 'Evaluation percentile verus best eval FUV'
    TabOrder = 49
    object BitBtn17: TBitBtn
      Left = 27
      Top = 85
      Width = 170
      Height = 25
      Caption = 'Best eval graph criterion, filters'
      TabOrder = 0
      OnClick = BitBtn17Click
    end
    object BitBtn28: TBitBtn
      Left = 24
      Top = 23
      Width = 170
      Height = 25
      Caption = 'Best eval graph per criterion'
      TabOrder = 1
      OnClick = BitBtn28Click
    end
    object BitBtn16: TBitBtn
      Left = 27
      Top = 54
      Width = 169
      Height = 25
      Caption = 'DEMs graph per criterion'
      TabOrder = 2
      OnClick = BitBtn16Click
    end
    object BitBtn43: TBitBtn
      Left = 27
      Top = 116
      Width = 170
      Height = 25
      Caption = 'Compare two tables'
      TabOrder = 3
      OnClick = BitBtn43Click
    end
  end
  object BitBtn44: TBitBtn
    Left = 592
    Top = 800
    Width = 196
    Height = 25
    Caption = 'GEDTM filtering'
    TabOrder = 50
    OnClick = BitBtn44Click
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 145
    Width = 153
    Height = 17
    Caption = 'Pie diagrams show n='
    TabOrder = 51
    OnClick = CheckBox4Click
  end
end
