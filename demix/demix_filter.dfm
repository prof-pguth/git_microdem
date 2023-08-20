object DemixFilterForm: TDemixFilterForm
  Left = 0
  Top = 0
  Caption = 'Demix Filter Pick Graphs'
  ClientHeight = 483
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 787
    Height = 483
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 783
    ExplicitHeight = 482
    object TabSheet1: TTabSheet
      Caption = 'New options'
      object BitBtn5: TBitBtn
        Left = 502
        Top = 138
        Width = 155
        Height = 25
        Caption = 'Load area DEMs'
        TabOrder = 0
        OnClick = BitBtn5Click
      end
      object CheckBox3: TCheckBox
        Left = 256
        Top = 146
        Width = 97
        Height = 17
        Caption = 'Test DEMs'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object LoadOneSecRefCheckBox: TCheckBox
        Left = 143
        Top = 146
        Width = 97
        Height = 17
        Caption = 'Reference 1"'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 146
        Width = 121
        Height = 17
        Caption = 'Source EGM2008'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object ComboBox4: TComboBox
        Left = 16
        Top = 117
        Width = 145
        Height = 23
        TabOrder = 4
        Text = 'ComboBox4'
      end
      object BitBtn4: TBitBtn
        Left = 287
        Top = 45
        Width = 155
        Height = 25
        Caption = 'Distribution histogram'
        TabOrder = 5
        OnClick = BitBtn4Click
      end
      object ComboBox3: TComboBox
        Left = 328
        Top = 16
        Width = 145
        Height = 23
        ItemIndex = 0
        TabOrder = 6
        Text = 'dtm'
        Items.Strings = (
          'dtm'
          'dsm')
      end
      object ComboBox2: TComboBox
        Left = 168
        Top = 16
        Width = 145
        Height = 23
        ItemIndex = 0
        TabOrder = 7
        Text = 'elev'
        Items.Strings = (
          'elev'
          'slope'
          'ruff')
      end
      object ComboBox1: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 23
        TabOrder = 8
        Text = 'ComboBox1'
        OnChange = ComboBox1Change
      end
      object BitBtn7: TBitBtn
        Left = 224
        Top = 377
        Width = 154
        Height = 25
        Caption = 'Elevation difference maps'
        TabOrder = 9
        OnClick = BitBtn7Click
      end
      object BitBtn8: TBitBtn
        Left = 361
        Top = 320
        Width = 154
        Height = 25
        Caption = 'Close DEMs'
        TabOrder = 10
        OnClick = BitBtn8Click
      end
      object BitBtn9: TBitBtn
        Left = 360
        Top = 264
        Width = 154
        Height = 25
        Caption = 'Redraw difference maps'
        TabOrder = 11
        OnClick = BitBtn9Click
      end
      object BitBtn10: TBitBtn
        Left = 224
        Top = 408
        Width = 154
        Height = 25
        Caption = 'All elev diff maps'
        TabOrder = 12
        OnClick = BitBtn10Click
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 169
        Width = 97
        Height = 17
        Caption = 'Ref DEMs v1'
        TabOrder = 13
      end
      object BitBtn11: TBitBtn
        Left = 531
        Top = 208
        Width = 129
        Height = 25
        Caption = 'v1 Elev Diff Maps'
        TabOrder = 14
        OnClick = BitBtn11Click
      end
      object BitBtn12: TBitBtn
        Left = 359
        Top = 176
        Width = 154
        Height = 25
        Caption = 'Peak topographic profiles'
        TabOrder = 15
        OnClick = BitBtn12Click
      end
      object BitBtn13: TBitBtn
        Left = 531
        Top = 177
        Width = 129
        Height = 25
        Caption = 'All  peaks profiles'
        TabOrder = 16
        OnClick = BitBtn13Click
      end
      object CheckBox5: TCheckBox
        Left = 143
        Top = 169
        Width = 97
        Height = 17
        Caption = 'Reference 0.5"'
        TabOrder = 17
      end
      object CheckBox6: TCheckBox
        Left = 256
        Top = 168
        Width = 97
        Height = 17
        Caption = 'COP && ALOS'
        TabOrder = 18
      end
      object BitBtn14: TBitBtn
        Left = 531
        Top = 264
        Width = 150
        Height = 25
        Caption = 'COP/ALOS diff maps'
        TabOrder = 19
        OnClick = BitBtn14Click
      end
      object RadioGroup1: TRadioGroup
        Left = 16
        Top = 352
        Width = 177
        Height = 81
        Caption = 'COP/ALOS compare'
        ItemIndex = 0
        Items.Strings = (
          '4 categories'
          '9 categories'
          'Both')
        TabOrder = 20
      end
      object BitBtn15: TBitBtn
        Left = 448
        Top = 45
        Width = 97
        Height = 25
        Caption = '3 distributions'
        TabOrder = 21
        OnClick = BitBtn15Click
      end
      object BitBtn16: TBitBtn
        Left = 551
        Top = 45
        Width = 90
        Height = 25
        Caption = '6 distribuitons'
        TabOrder = 22
        OnClick = BitBtn16Click
      end
      object BitBtn17: TBitBtn
        Left = 623
        Top = 346
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
        TabOrder = 23
        OnClick = BitBtn17Click
      end
      object BitBtn18: TBitBtn
        Left = 16
        Top = 45
        Width = 146
        Height = 25
        Caption = 'Load tiles from list'
        TabOrder = 24
        OnClick = BitBtn18Click
      end
      object BitBtn19: TBitBtn
        Left = 520
        Top = 80
        Width = 75
        Height = 25
        Caption = 'All tiles'
        TabOrder = 25
        OnClick = BitBtn19Click
      end
      object BitBtn20: TBitBtn
        Left = 384
        Top = 377
        Width = 131
        Height = 25
        Caption = 'Slope difference maps'
        TabOrder = 26
        OnClick = BitBtn20Click
      end
      object BitBtn21: TBitBtn
        Left = 521
        Top = 377
        Width = 131
        Height = 25
        Caption = ' Ruff difference maps'
        TabOrder = 27
        OnClick = BitBtn21Click
      end
      object BitBtn22: TBitBtn
        Left = 384
        Top = 408
        Width = 131
        Height = 25
        Caption = 'All slope diff maps'
        TabOrder = 28
        OnClick = BitBtn22Click
      end
      object BitBtn23: TBitBtn
        Left = 521
        Top = 408
        Width = 131
        Height = 25
        Caption = 'All ruff diff maps'
        TabOrder = 29
        OnClick = BitBtn23Click
      end
      object BitBtn24: TBitBtn
        Left = 16
        Top = 76
        Width = 146
        Height = 25
        Caption = 'Load tiles from db filter'
        TabOrder = 30
        OnClick = BitBtn24Click
      end
      object BitBtn25: TBitBtn
        Left = 531
        Top = 295
        Width = 150
        Height = 25
        Caption = 'COP/ALOS summary stats'
        TabOrder = 31
        OnClick = BitBtn25Click
      end
      object BitBtn26: TBitBtn
        Left = 542
        Top = 342
        Width = 75
        Height = 25
        Caption = 'DB stats'
        TabOrder = 32
        OnClick = BitBtn26Click
      end
      object BitBtn27: TBitBtn
        Left = 658
        Top = 377
        Width = 49
        Height = 25
        Caption = 'All'
        TabOrder = 33
        OnClick = BitBtn27Click
      end
      object BitBtn28: TBitBtn
        Left = 687
        Top = 264
        Width = 51
        Height = 25
        Caption = 'All'
        TabOrder = 34
        OnClick = BitBtn28Click
      end
      object GroupBox7: TGroupBox
        Left = 16
        Top = 192
        Width = 177
        Height = 145
        Caption = 'Tolerances'
        TabOrder = 35
        object Label6: TLabel
          Left = 18
          Top = 82
          Width = 79
          Height = 15
          Caption = 'Roughness (%)'
        end
        object Label5: TLabel
          Left = 16
          Top = 53
          Width = 50
          Height = 15
          Caption = 'Slope (%)'
        end
        object Label4: TLabel
          Left = 16
          Top = 21
          Width = 73
          Height = 15
          Caption = 'Elevation  (m)'
        end
        object Edit5: TEdit
          Left = 109
          Top = 79
          Width = 56
          Height = 23
          TabOrder = 0
          Text = 'Edit5'
          OnChange = Edit5Change
        end
        object Edit4: TEdit
          Left = 108
          Top = 50
          Width = 57
          Height = 23
          TabOrder = 1
          Text = 'Edit4'
          OnChange = Edit4Change
        end
        object Edit3: TEdit
          Left = 108
          Top = 21
          Width = 57
          Height = 23
          TabOrder = 2
          Text = 'Edit3'
          OnChange = Edit3Change
        end
        object BitBtn29: TBitBtn
          Left = 64
          Top = 112
          Width = 91
          Height = 25
          Caption = 'Restore defaults'
          TabOrder = 3
          OnClick = BitBtn29Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Compare v1 v2'
      ImageIndex = 1
      object ComboBox5: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 23
        TabOrder = 0
        Text = 'ComboBox5'
        OnChange = ComboBox5Change
      end
      object ComboBox6: TComboBox
        Left = 168
        Top = 16
        Width = 145
        Height = 23
        TabOrder = 1
        Text = 'ComboBox6'
        OnChange = ComboBox6Change
      end
      object ComboBox7: TComboBox
        Left = 336
        Top = 16
        Width = 145
        Height = 23
        ItemIndex = 0
        TabOrder = 2
        Text = 'DTM'
        OnChange = ComboBox7Change
        Items.Strings = (
          'DTM'
          'DSM')
      end
      object StringGrid1: TStringGrid
        Left = 3
        Top = 80
        Width = 478
        Height = 169
        ColCount = 7
        RowCount = 4
        TabOrder = 3
      end
      object BitBtn6: TBitBtn
        Left = 208
        Top = 48
        Width = 105
        Height = 25
        Caption = 'Compare'
        TabOrder = 4
        OnClick = BitBtn6Click
      end
      object ComboBox8: TComboBox
        Left = 504
        Top = 16
        Width = 145
        Height = 23
        TabOrder = 5
        Text = 'ALL'
        OnChange = ComboBox8Change
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Multiple panel graphs'
      ImageIndex = 2
      object Label2: TLabel
        Left = 8
        Top = 429
        Width = 63
        Height = 15
        Caption = 'Graph y size'
      end
      object Label1: TLabel
        Left = 8
        Top = 400
        Width = 63
        Height = 15
        Caption = 'Graph x size'
      end
      object GroupBox3: TGroupBox
        Left = 222
        Top = 8
        Width = 105
        Height = 161
        Caption = 'Land type'
        TabOrder = 0
        object Memo3: TMemo
          Left = 3
          Top = 24
          Width = 89
          Height = 129
          Lines.Strings = (
            'ALL'
            'BARREN'
            'CLIFF'
            'FLAT'
            'FOREST'
            'GENTLE'
            'STEEP'
            'URBAN')
          TabOrder = 0
        end
      end
      object GroupBox5: TGroupBox
        Left = 111
        Top = 8
        Width = 105
        Height = 153
        Caption = 'Candidate DEM'
        TabOrder = 1
        object Memo5: TMemo
          Left = 3
          Top = 24
          Width = 89
          Height = 129
          Lines.Strings = (
            'ALOS'
            'ASTER'
            'COP'
            'FABDEM'
            'NASA'
            'SRTM')
          TabOrder = 0
        end
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 97
        Height = 153
        Caption = 'DEM type'
        TabOrder = 2
        object Memo1: TMemo
          Left = 3
          Top = 24
          Width = 86
          Height = 129
          Lines.Strings = (
            'DSM'
            'DTM')
          TabOrder = 0
        end
      end
      object GroupBox6: TGroupBox
        Left = 604
        Top = 8
        Width = 153
        Height = 191
        Caption = 'Areas'
        TabOrder = 3
        object Memo6: TMemo
          Left = 3
          Top = 24
          Width = 142
          Height = 129
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object BitBtn3: TBitBtn
          Left = 11
          Top = 156
          Width = 75
          Height = 25
          Caption = 'Load'
          TabOrder = 1
          OnClick = BitBtn3Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 477
        Top = 8
        Width = 121
        Height = 191
        Caption = 'DEMIX tile'
        TabOrder = 4
        object Memo2: TMemo
          Left = 3
          Top = 21
          Width = 110
          Height = 129
          Lines.Strings = (
            'N28XW018B'
            'N59TE009G'
            'N36XW003D')
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object Load: TBitBtn
          Left = 3
          Top = 156
          Width = 75
          Height = 25
          Caption = 'Load'
          TabOrder = 1
          OnClick = LoadClick
        end
      end
      object GroupBox4: TGroupBox
        Left = 342
        Top = 8
        Width = 129
        Height = 191
        Caption = 'Criteria'
        TabOrder = 5
        object Memo4: TMemo
          Left = 3
          Top = 16
          Width = 116
          Height = 137
          Lines.Strings = (
            'ELVD_AVD'
            'ELVD_STD'
            'ELVD_MAE'
            'ELVD_RMSE'
            'ELVD_LE90'
            'SLPD_AVD'
            'SLPD_STD'
            'SLPD_MAE'
            'SLPD_RMSE'
            'SLPD_LE90'
            'RUFD_AVD'
            'RUFD_STD'
            'RUFD_MAE'
            'RUFD_RMSE'
            'RUFD_LE90')
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object BitBtn2: TBitBtn
          Left = 0
          Top = 158
          Width = 75
          Height = 25
          Caption = 'Load'
          TabOrder = 1
          OnClick = BitBtn2Click
        end
      end
      object Edit2: TEdit
        Left = 88
        Top = 426
        Width = 65
        Height = 23
        TabOrder = 6
        Text = 'Edit2'
        OnChange = Edit2Change
      end
      object Edit1: TEdit
        Left = 88
        Top = 397
        Width = 65
        Height = 23
        TabOrder = 7
        Text = 'Edit1'
        OnChange = Edit1Change
      end
      object BitBtn1: TBitBtn
        Left = 177
        Top = 397
        Width = 150
        Height = 25
        Caption = 'Multiple Criteria per Tile'
        TabOrder = 8
        OnClick = BitBtn1Click
      end
      object GroupBox8: TGroupBox
        Left = 16
        Top = 192
        Width = 89
        Height = 145
        Caption = 'Tile stats'
        TabOrder = 9
        object Memo7: TMemo
          Left = 0
          Top = 29
          Width = 86
          Height = 113
          Lines.Strings = (
            'RELIEF'
            'AVG_ELEV'
            'AVG_SLOPE'
            'AVG_ROUGH'
            'FOREST_PC'
            'URBAN_PC'
            'BARREN_PC')
          TabOrder = 0
        end
      end
      object RadioGroup2: TRadioGroup
        Left = 177
        Top = 224
        Width = 496
        Height = 110
        Caption = 'Best DEMs'
        Items.Strings = (
          'Sort by tile parameters, DEM type, graph for each criterion'
          
            'Sort by tile parameters, graph by DEM type and Land Type, merged' +
            ' all 15 criteria'
          
            'Sort by tile paramaters, each criterion, and graph by DEM type a' +
            'nd Land type'
          'Sort by tile parameters, graph by criterion and DEM type')
        TabOrder = 10
        OnClick = RadioGroup2Click
      end
    end
    object Settings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 3
      object CheckBox2: TCheckBox
        Left = 40
        Top = 24
        Width = 209
        Height = 17
        Caption = 'Two DEM difference maps'
        TabOrder = 0
        OnClick = CheckBox2Click
      end
      object CheckBox7: TCheckBox
        Left = 40
        Top = 47
        Width = 161
        Height = 17
        Caption = 'Two DEM best maps'
        TabOrder = 1
        OnClick = CheckBox7Click
      end
      object CheckBox8: TCheckBox
        Left = 40
        Top = 72
        Width = 209
        Height = 17
        Caption = 'Two differnce category maps'
        TabOrder = 2
        OnClick = CheckBox8Click
      end
      object threedembestrgm_checkbox: TCheckBox
        Left = 40
        Top = 118
        Width = 161
        Height = 17
        Caption = 'Three DEM best RGB'
        TabOrder = 3
        OnClick = threedembestrgm_checkboxClick
      end
      object CheckBox9: TCheckBox
        Left = 40
        Top = 95
        Width = 217
        Height = 17
        Caption = 'COP-FABDEM difference map'
        TabOrder = 4
        OnClick = CheckBox9Click
      end
      object CheckBox10: TCheckBox
        Left = 40
        Top = 141
        Width = 209
        Height = 17
        Caption = 'Three DEM best separates'
        TabOrder = 5
        OnClick = CheckBox10Click
      end
    end
  end
end
