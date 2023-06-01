object DemixFilterForm: TDemixFilterForm
  Left = 0
  Top = 0
  Caption = 'Demix Filter Pick Graphs'
  ClientHeight = 409
  ClientWidth = 671
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 671
    Height = 409
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 667
    ExplicitHeight = 408
    object TabSheet1: TTabSheet
      Caption = 'New options'
      object Label4: TLabel
        Left = 152
        Top = 232
        Width = 114
        Height = 15
        Caption = 'Highlight change (m)'
      end
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
        Left = 360
        Top = 176
        Width = 154
        Height = 25
        Caption = 'Elevation difference maps'
        TabOrder = 9
        OnClick = BitBtn7Click
      end
      object BitBtn8: TBitBtn
        Left = 360
        Top = 312
        Width = 154
        Height = 25
        Caption = 'Close DEMs'
        TabOrder = 10
        OnClick = BitBtn8Click
      end
      object Edit3: TEdit
        Left = 272
        Top = 229
        Width = 57
        Height = 23
        TabOrder = 11
        Text = 'Edit3'
        OnChange = Edit3Change
      end
      object BitBtn9: TBitBtn
        Left = 360
        Top = 264
        Width = 154
        Height = 25
        Caption = 'Redraw difference maps'
        TabOrder = 12
        OnClick = BitBtn9Click
      end
      object BitBtn10: TBitBtn
        Left = 528
        Top = 176
        Width = 129
        Height = 25
        Caption = 'All elev diff maps'
        TabOrder = 13
        OnClick = BitBtn10Click
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 169
        Width = 97
        Height = 17
        Caption = 'Ref DEMs v1'
        TabOrder = 14
      end
      object BitBtn11: TBitBtn
        Left = 531
        Top = 239
        Width = 129
        Height = 25
        Caption = 'v1 Elev Diff Maps'
        TabOrder = 15
        OnClick = BitBtn11Click
      end
      object BitBtn12: TBitBtn
        Left = 360
        Top = 208
        Width = 154
        Height = 25
        Caption = 'Peak topographic profiles'
        TabOrder = 16
        OnClick = BitBtn12Click
      end
      object BitBtn13: TBitBtn
        Left = 531
        Top = 270
        Width = 129
        Height = 25
        Caption = 'All  peaks profiles'
        TabOrder = 17
        OnClick = BitBtn13Click
      end
      object CheckBox5: TCheckBox
        Left = 143
        Top = 169
        Width = 97
        Height = 17
        Caption = 'Reference 0.5"'
        TabOrder = 18
      end
      object CheckBox6: TCheckBox
        Left = 256
        Top = 168
        Width = 97
        Height = 17
        Caption = 'COP && ALOS'
        TabOrder = 19
      end
      object BitBtn14: TBitBtn
        Left = 531
        Top = 208
        Width = 129
        Height = 25
        Caption = 'COP/ALOS diff maps'
        TabOrder = 20
        OnClick = BitBtn14Click
      end
      object RadioGroup1: TRadioGroup
        Left = 24
        Top = 264
        Width = 185
        Height = 105
        Caption = 'COP/ALOS compare'
        ItemIndex = 0
        Items.Strings = (
          '4 categories'
          '9 categories')
        TabOrder = 21
      end
      object BitBtn15: TBitBtn
        Left = 448
        Top = 45
        Width = 97
        Height = 25
        Caption = '3 distributions'
        TabOrder = 22
        OnClick = BitBtn15Click
      end
      object BitBtn16: TBitBtn
        Left = 551
        Top = 45
        Width = 90
        Height = 25
        Caption = '6 distribuitons'
        TabOrder = 23
        OnClick = BitBtn16Click
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
      Caption = 'Deprecated options'
      ImageIndex = 2
      object Label2: TLabel
        Left = 352
        Top = 157
        Width = 63
        Height = 15
        Caption = 'Graph y size'
      end
      object Label1: TLabel
        Left = 352
        Top = 128
        Width = 63
        Height = 15
        Caption = 'Graph x size'
      end
      object Label3: TLabel
        Left = 368
        Top = 48
        Width = 139
        Height = 15
        Caption = 'DEM type, Tiles, Land Type'
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
        Left = 8
        Top = 175
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
        Left = 167
        Top = 175
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
        Left = 294
        Top = 175
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
            'ELVD_MEAN'
            'ELVD_MED'
            'ELVD_AVD'
            'ELVD_STD'
            'ELVD_MAE'
            'ELVD_RMSE'
            'ELVD_LE90'
            'SLPD_MEAN'
            'SLPD_MED'
            'SLPD_AVD'
            'SLPD_STD'
            'SLPD_MAE'
            'SLPD_RMSE'
            'SLPD_LE90'
            'RUFD_MEAN'
            'RUFD_MED'
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
        Left = 432
        Top = 154
        Width = 65
        Height = 23
        TabOrder = 6
        Text = 'Edit2'
        OnChange = Edit2Change
      end
      object Edit1: TEdit
        Left = 432
        Top = 125
        Width = 65
        Height = 23
        TabOrder = 7
        Text = 'Edit1'
        OnChange = Edit1Change
      end
      object BitBtn1: TBitBtn
        Left = 347
        Top = 16
        Width = 150
        Height = 25
        Caption = 'Multiple Criteria per Tile'
        TabOrder = 8
        OnClick = BitBtn1Click
      end
    end
  end
end
