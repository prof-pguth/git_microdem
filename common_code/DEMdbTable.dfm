object dbtablef: Tdbtablef
  Left = 62
  Top = 306
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 1073
  ClientWidth = 1328
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsMDIChild
  Position = poDefault
  ShowHint = True
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 20
  object StatusBar1: TStatusBar
    Left = 0
    Top = 1054
    Width = 1328
    Height = 19
    Panels = <
      item
        Width = 250
      end>
    ExplicitTop = 1063
    ExplicitWidth = 1334
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 1328
    Height = 32
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 1334
    object BitBtn1: TBitBtn
      Left = 34
      Top = 1
      Width = 68
      Height = 25
      Hint = 'Set point symbol and color'
      Caption = 'Points'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn4: TBitBtn
      Left = 102
      Top = 1
      Width = 34
      Height = 25
      Hint = 'Set filter conditions'
      Caption = 'Filter'
      TabOrder = 1
      OnClick = BitBtn4Click
    end
    object Button1: TButton
      Left = 135
      Top = 1
      Width = 40
      Height = 25
      Hint = 'Show all records in table'
      Caption = 'All recs'
      TabOrder = 2
      OnClick = Button1Click
    end
    object BitBtn5: TBitBtn
      Left = 175
      Top = 1
      Width = 33
      Height = 25
      Hint = 'Plot on the map'
      Caption = 'Plot'
      TabOrder = 3
      OnClick = BitBtn5Click
    end
    object BitBtn8: TBitBtn
      Left = 214
      Top = 1
      Width = 40
      Height = 25
      Hint = 'Calculate statistics or plot graphs'
      Caption = 'Stats'
      TabOrder = 4
      OnClick = BitBtn8Click
    end
    object BitBtn7: TBitBtn
      Left = 250
      Top = 1
      Width = 60
      Height = 25
      Hint = 'Restrict the data table geographically'
      Caption = 'Map Query'
      TabOrder = 5
      OnClick = BitBtn7Click
    end
    object Button5: TButton
      Left = 309
      Top = 1
      Width = 28
      Height = 25
      Hint = 'Identify records on map, just this DB'
      Caption = 'ID'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = Button5Click
    end
    object BitBtn12: TBitBtn
      Left = 343
      Top = 1
      Width = 49
      Height = 25
      Hint = 'Create a report using current table display'
      Caption = 'Report'
      TabOrder = 7
      OnClick = BitBtn12Click
    end
    object Button4: TButton
      Left = 387
      Top = 1
      Width = 34
      Height = 25
      Hint = 'Edit data'
      Caption = 'Edit'
      TabOrder = 8
      OnClick = Button4Click
    end
    object BitBtn9: TBitBtn
      Left = 424
      Top = 1
      Width = 34
      Height = 25
      Hint = 'Hide some columns in table'
      Caption = 'Hide'
      TabOrder = 9
      OnClick = BitBtn9Click
    end
    object HelpBtn: TBitBtn
      Left = 464
      Top = 1
      Width = 49
      Height = 25
      Caption = 'Help'
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 10
      OnClick = HelpBtnClick
      IsControl = True
    end
    object CheckBox1: TCheckBox
      Left = 528
      Top = 9
      Width = 41
      Height = 17
      Caption = 'Edit'
      TabOrder = 11
      OnClick = CheckBox1Click
    end
    object BitBtn17: TBitBtn
      Left = 575
      Top = 1
      Width = 33
      Height = 25
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33339FFFFFF9FFFFFF9FF9FFFFF9FFFFF9FFFF9FFFF9FFFF9FFFFFF9FFF9FFF9
        FFFFFFF9FFF9FFF9FFFFFFFF9FF9FF9FFFFFFFFFF9F9F9FFFFFFFFFFFF999FFF
        FFFFFFFFFF888FFFFFFFCCCCC88888CCCCCCFFFFF88888FFFFFFFFFFFFF8FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      TabOrder = 12
      OnClick = BitBtn17Click
    end
    object BitBtn23: TBitBtn
      Left = 614
      Top = 1
      Width = 26
      Height = 25
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 13
      Visible = False
      OnClick = BitBtn23Click
    end
    object BitBtn28: TBitBtn
      Left = -5
      Top = 1
      Width = 33
      Height = 25
      Hint = 'Layer plotting status'
      Caption = #8730
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 14
      OnClick = BitBtn28Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 73
    Width = 1328
    Height = 41
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 1334
    object Label2: TLabel
      Left = 12
      Top = 12
      Width = 31
      Height = 20
      Caption = 'Find:'
    end
    object Edit1: TEdit
      Left = 56
      Top = 7
      Width = 145
      Height = 28
      TabOrder = 0
      OnChange = Edit1Change
    end
    object BitBtn11: TBitBtn
      Left = 207
      Top = 10
      Width = 102
      Height = 25
      Caption = 'Plot && Label'
      TabOrder = 1
      OnClick = BitBtn11Click
    end
    object BitBtn22: TBitBtn
      Left = 447
      Top = 10
      Width = 81
      Height = 25
      Caption = 'Gaz Opts'
      TabOrder = 2
      OnClick = BitBtn22Click
    end
    object Features: TCheckBox
      Left = 315
      Top = 6
      Width = 126
      Height = 17
      Caption = 'Label Features'
      TabOrder = 3
      OnClick = FeaturesClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1328
    Height = 41
    Align = alTop
    TabOrder = 4
    ExplicitWidth = 1334
    object LOSButton: TSpeedButton
      Left = 40
      Top = 6
      Width = 32
      Height = 32
      Hint = 'Line of Sight Profile'
      Glyph.Data = {
        76020000424D7602000000000000760000002800000020000000200000000100
        0400000000000002000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF555555555555
        5555555555555555555555555555555555555555555555555555555555555555
        5555555555555555555555222222222222222222222222222255552222222222
        2222222222222222225555222222222222222222222222222255552222222222
        2222222222222222225555222222222222222222222222222255552222222222
        2222222222222222225555222222222222222222222222222255552222222222
        222222222222222222555522222222222222222222222222225555222222AAAA
        AAAA2222222222222255552222AAAAAAAAAA22222222AAAAA2A555222AAAAFFF
        FFAAAA2222AAAAAAAA5555AAAAAFFFFFFFFAAAA222AAAFFFFAA555AAAAAFFFFF
        FFFFAAAAAAAAFFFFFFAAAAAFFFFFFFFFFFFFFAAAAAAFFFFFFFFF999999999999
        99999999999999999999FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000FF
        F000FFFF0000FFFFFFFFFFFFF0FFFFFF0FFF0FF0FFFF0FFFFFFFFFFFF0FFFFF0
        FFFFF0FFFFFF0FFFFFFFFFFFF0FFFFF0FFFFF0FFFF00FFFFFFFFFFFFF0FFFFF0
        FFFFF0FF00FFFFFFFFFFFFFFF0FFFFF0FFFFF0F0FFFFFFFFFFFFFFFFF0FFFFFF
        0FFF0FF0FFFF0FFFFFFFFFFFF0FFFFFFF000FFFF0000FFFFFFFF}
      OnClick = LOSButtonClick
    end
    object PanoramaSpeedButton: TSpeedButton
      Left = 78
      Top = 6
      Width = 32
      Height = 32
      Hint = 'Panorama'
      Glyph.Data = {
        76020000424D7602000000000000760000002800000020000000200000000100
        0400000000000002000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888880800888888088888888088888888880000888088808888888888088888
        8800880888880808888888888808888888088808888880088888888888808888
        8088880888888088888888888880888008888808888888088888088888880808
        8888880888888800088880888888008888888800808888088000880888800888
        8888880888088088888888008808808888888008888080888888888008888808
        8888080888880088888888808888880888808808888880888880088808888880
        8808880888888008888008880888888800888808888880808808808880888888
        8088880888880888080800888088888990888808998808888008800888088889
        9988889999880888880880800808880899988999988088888800808880008088
        8999999888808888880888088880999999999999999998888088880889999999
        999999999999999980888880999999989999999EE09999999088888999988809
        99880999EEE0088999880099988888999880EE999EEEE0889998EE9908888899
        880EEEE99EEEEE080990EE990008888800EEEEEEEEEEEEE0099EEE999EE00000
        0EEEEEEEEEEEEEEE999EEEE999EEEEEEEEEEEEEEEEEEEEE999EEEEEE99999EEE
        EEEEEEEEEEEE99999EEEEEEEE99999999999999999999999EEEEEEEEEEEE9999
        9999999999999EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE}
      OnClick = PanoramaSpeedButtonClick
    end
    object PerspectiveButton: TSpeedButton
      Left = 116
      Top = 6
      Width = 32
      Height = 32
      Hint = 'Perspective view'
      Glyph.Data = {
        76020000424D7602000000000000760000002800000020000000200000000100
        0400000000000002000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777770700777777077777777077777777770000777077707777777777077777
        7700770777770707777777777707777777077707777770077777777777707777
        7077770777777077777777777770777007777707777777077777077777770707
        7777770777777700077770777777007777777700707777077000770777700777
        7777770777077077777777007707707777777007777070777777777007777707
        7777070777770077777777707777770777707707777770777770077707777770
        7707770777777007777007770777777700777707777770707707707770777777
        7077770777770777070700777077777700777707777707777007700777077770
        7077770777770777770770700707770777077707777077777700707770007077
        7707770777707777770777077770077777707707777077777077770777777077
        77707700070777777077777077777707777000EEE00777777077777077777707
        77770EE0EEE0077770770077077777707770EEE0EEEEE0770777EE0007777770
        770EEEE0EEEEEE070770EEEE0007777700EEEEE0E00EEEE0000EEEEEEEE00000
        0EEEEEE00EE0EEEEEEEEEEEEEEEEEEEEEEEEEEE0EEE0EEEEEEEEEEEEEEEEEEEE
        EEEEEEE0EEE0EEEEEEEEEEEEEEEEEEEEEEEEEEE0EEE0EEEEEEEEEEEEEEEEEEEE
        EEEEEEE00EE0EEEEEEEEEEEEEEEEEEEEEEEEEEE0E00EEEEEEEEE}
      OnClick = PerspectiveButtonClick
    end
    object BitBtn6: TBitBtn
      Left = 288
      Top = 6
      Width = 49
      Height = 25
      Hint = 'Compute multiple coverage'
      Caption = 'Multiple'
      TabOrder = 0
      OnClick = BitBtn6Click
    end
    object BitBtn10: TBitBtn
      Left = 343
      Top = 6
      Width = 66
      Height = 25
      Caption = 'Pick sensors'
      TabOrder = 1
      OnClick = BitBtn10Click
    end
    object BitBtn14: TBitBtn
      Left = 230
      Top = 6
      Width = 56
      Height = 25
      Hint = 'Make composite fans'
      Caption = 'Composite'
      TabOrder = 2
      OnClick = BitBtn14Click
    end
    object BitBtn15: TBitBtn
      Left = 154
      Top = 6
      Width = 32
      Height = 32
      Hint = 'Required antenna heights'
      Glyph.Data = {
        36020000424D360200000000000076000000280000001C0000001C0000000100
        040000000000C001000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFF77000007F0000FFFFFFFFFFFF
        FFFFF87078F7707F0000FFFFFFFFFFFFFFFFF070000078FF0000FFFFFFFFFFFF
        FFFF0007000000FF0000FFFFFFFFFFFFFFFF7008000007FF0000FFFFFFFFFFFF
        878FF008000007FF0000FFFFFFFFFFF8F7007807000008FF0000FFFFFFFFFF8F
        FF700070007708FF0000FFFFFFFFFF7FFFF700070787FFFF0000FFFFFFFFFF7F
        FFFF00007707FFFF0000FFFFFFFFFF38FFFF80000F07FFFF0000F88FFFFFFF78
        FFFFF6000788FFFF0000F8FFF8FFFF74FFFFF800008FFFFF0000FFFFF7FF8F80
        F0FFFF00007FFFFF0000FF8FF7F88FF807FFFF70007FFFFF0000F87FF7F87F80
        0FFFFF80007FFFFF0000F8FFF3FF7F0778FFFFF0007FFFFF0000FF0FF77F7870
        8788FFF7007FFFFF0000FF07FF0FF7F887FFFFF7008FFFFF0000FF80FF77FF7F
        F77FFFF800FFFFFF0000FFF08FF08FFFFF07FFF807FFFFFF0000FFF80FFF78FF
        FFF08FF80FFFFFFF0000FFFF77FFF8FFFFFF38F7FFFFFFFF0000FFFFF08FFFFF
        FFFFF88FFFFFFFFF0000FFFFFF78FFFFFFFFFFFFFFFFFFFF0000FFFFFFF8FFFF
        FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
      TabOrder = 3
      OnClick = BitBtn15Click
    end
    object BitBtn16: TBitBtn
      Left = 192
      Top = 6
      Width = 32
      Height = 32
      Hint = 'Highlight single sensor'
      Caption = 'HL'
      TabOrder = 4
      OnClick = BitBtn16Click
    end
    object BitBtn18: TBitBtn
      Left = 2
      Top = 6
      Width = 32
      Height = 32
      Hint = 'Compute fan coverage'
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000014000000120000000100
        040000000000D800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFF0000F0FFFFAAAAAAFFFFFFFF0000FF000000AAAAAAFFFFFF0000FF00
        0000000000AFFFFF0000FF000000000000000FFF0000FFAA0AA00000000000FF
        0000FAAAA0AAAAAAA0000FFF0000AAAAAA0AAAAAA00AAFFF0000AAAAAA0AAAA0
        0AAAAFFF0000AAAAAAA0A00AAAAAAAFF0000AAAAAAAA0AAAAAAAAAFF0000AAAA
        AAAAAAAAAAAAAAFF0000FAAAAAAAAAAAAAAAAFFF0000FAAAAAAAAAAAAAAAAFFF
        0000FFAAAAAAAAAAAAAAFFFF0000FFFAAAAAAAAAAAAFFFFF0000FFFFAAAAAAAA
        AAFFFFFF0000FFFFFFAAAAAAFFFFFFFF0000}
      TabOrder = 5
      OnClick = BitBtn18Click
    end
    object BitBtn19: TBitBtn
      Left = 569
      Top = 6
      Width = 33
      Height = 27
      Hint = 'Save fan locations'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      TabOrder = 6
      OnClick = BitBtn19Click
    end
    object BitBtn20: TBitBtn
      Left = 608
      Top = 6
      Width = 40
      Height = 25
      Hint = 'Clear fans'
      Caption = 'Clear'
      TabOrder = 7
      OnClick = BitBtn20Click
    end
    object BitBtn21: TBitBtn
      Left = 415
      Top = 6
      Width = 45
      Height = 25
      Caption = 'Recolor '
      TabOrder = 8
      OnClick = BitBtn21Click
    end
    object TrackBar1: TTrackBar
      Left = 466
      Top = 6
      Width = 100
      Height = 25
      Max = 100
      Frequency = 10
      TabOrder = 9
      OnChange = TrackBar1Change
    end
    object HLCheckbox: TCheckBox
      Left = 654
      Top = 6
      Width = 35
      Height = 17
      Caption = 'HL'
      TabOrder = 10
    end
    object BitBtn2: TBitBtn
      Left = 696
      Top = 8
      Width = 33
      Height = 25
      Caption = 'KML'
      TabOrder = 11
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 735
      Top = 6
      Width = 75
      Height = 25
      Hint = 'Reset fan parameters'
      Caption = 'Params'
      TabOrder = 12
      OnClick = BitBtn3Click
    end
  end
  object Panel7: TPanel
    Left = 0
    Top = 114
    Width = 1328
    Height = 41
    Align = alTop
    TabOrder = 3
    ExplicitWidth = 1334
    object RadioGroup2: TRadioGroup
      Left = 20
      Top = 0
      Width = 517
      Height = 35
      Caption = 'Action on clicking'
      Columns = 5
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Symbol'
        'Name'
        'Filter'
        'Toggle plot')
      TabOrder = 0
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 155
    Width = 1328
    Height = 899
    Align = alClient
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -15
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    OnCellClick = DBGrid1CellClick
    OnDrawColumnCell = DBGrid1DrawColumnCell
    OnDblClick = DBGrid1DblClick
    OnTitleClick = DBGrid1TitleClick
  end
  object StatsPopupMenu1: TPopupMenu
    Left = 216
    Top = 608
    object Countuniquevalues1: TMenuItem
      Caption = 'Count unique values'
      OnClick = Countuniquevalues1Click
    end
    object Listuniquevalues1: TMenuItem
      Caption = 'List unique values'
      OnClick = Listuniquevalues1Click
    end
    object Listnonnumericvalues1: TMenuItem
      Caption = 'List non-numeric values'
      OnClick = Listnonnumericvalues1Click
    end
    object Histogrambyuniquevalues1: TMenuItem
      Caption = 'Frequency table'
      object Singlefield1: TMenuItem
        Caption = 'Single field'
        OnClick = Singlefield1Click
      end
      object Twofields1: TMenuItem
        Caption = 'Two fields'
        OnClick = Twofields1Click
      end
    end
    object Longeststring1: TMenuItem
      Caption = 'Longest string'
      OnClick = Longeststring1Click
    end
    object Sum2: TMenuItem
      Caption = 'Sum'
      object Sumforonefield1: TMenuItem
        Caption = 'Sum for one field'
        OnClick = Sumforonefield1Click
      end
      object Allrecordsmatchingsinglefield1: TMenuItem
        Caption = 'All records matching single field'
        OnClick = Allrecordsmatchingsinglefield1Click
      end
    end
    object Momentstatistics1: TMenuItem
      Caption = 'Statistics/Moments'
      object Singlefield2: TMenuItem
        Caption = 'Single field'
        OnClick = Singlefield2Click
      end
      object Selectfields1: TMenuItem
        Caption = 'Select fields'
        OnClick = Selectfields1Click
      end
      object Allfields2: TMenuItem
        Caption = 'All fields'
        OnClick = Allfields2Click
      end
      object MeanstdallDBs1: TMenuItem
        Caption = 'Mean/std all DB'#39's'
        OnClick = MeanstdallDBs1Click
      end
      object N43: TMenuItem
        Caption = '-'
      end
      object Fieldpercentiles2: TMenuItem
        Caption = 'Field percentiles'
        OnClick = Fieldpercentiles2Click
      end
    end
    object Statisticsgroupedbyonefield1: TMenuItem
      Caption = 'Statistics grouped by one field'
      OnClick = Statisticsgroupedbyonefield1Click
    end
    object Fieldcorrelations1: TMenuItem
      Caption = 'Field correlations matrix'
      OnClick = Fieldcorrelations1Click
    end
    object Principalcomponents1: TMenuItem
      Caption = 'Principal components'
      OnClick = Principalcomponents1Click
    end
    object Heatmap1: TMenuItem
      Caption = 'Heat map'
      OnClick = Heatmap1Click
    end
    object Cluster2: TMenuItem
      Caption = 'Cluster'
      object Cluster1: TMenuItem
        Caption = 'Create cluster'
        OnClick = Cluster1Click
      end
      object Clusterfrequency1: TMenuItem
        Caption = 'Cluster frequency'
        OnClick = Clusterfrequency1Click
      end
    end
    object SupervisedClassification1: TMenuItem
      Caption = 'Supervised classification'
      object Reflectancespectrasingleclass1: TMenuItem
        Caption = 'Reflectance spectra, pick classes'
        OnClick = Reflectancespectrasingleclass1Click
      end
      object Averagestandarddeviation1: TMenuItem
        Caption = 'Create avg/std dev/ranges'
        OnClick = AverageStandarddeviation1Click
      end
      object Creategriddistancetoclasscentroid1: TMenuItem
        Caption = 'Create grid, distance to class centroid'
        OnClick = Creategriddistancetoclasscentroid1Click
      end
      object Creategridnumberofbandswithclassbox1: TMenuItem
        Caption = 'Create grid, number of bands within class box'
        OnClick = Creategridnumberofbandswithclassbox1Click
      end
      object Compareclassifications1: TMenuItem
        Caption = 'Compare classifications'
        OnClick = Compareclassifications1Click
      end
      object Gridmaps1: TMenuItem
        Caption = 'Grid maps'
        OnClick = Gridmaps1Click
      end
      object Gridstatistics1: TMenuItem
        Caption = 'Grid statistics'
        OnClick = Gridstatistics1Click
      end
      object Gridcumulativedistributions1: TMenuItem
        Caption = 'Grid cumulative distributions'
        OnClick = Gridcumulativedistributions1Click
      end
      object Parametercumulativedistribution1: TMenuItem
        Caption = 'Parameter cumulative distribution'
        OnClick = Parametercumulativedistribution1Click
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object Extractclassfiles1: TMenuItem
        Caption = 'Extract class files'
        OnClick = Extractclassfiles1Click
      end
      object Openallclassfiles1: TMenuItem
        Caption = 'Open all class files'
        OnClick = Openallclassfiles1Click
      end
      object Wavelengthmeanallclasses1: TMenuItem
        Caption = 'Wavelength/mean, all classes'
        Enabled = False
        OnClick = Wavelengthmeanallclasses1Click
      end
      object WavelengthStdDevallclasses1: TMenuItem
        Caption = 'Wavelength/StdDev, all classes'
        Enabled = False
        OnClick = WavelengthStdDevallclasses1Click
      end
      object ClassBanddiscrimination1: TMenuItem
        Caption = 'Class/Band discrimination'
        Enabled = False
        OnClick = ClassBanddiscrimination1Click
      end
      object N28: TMenuItem
        Caption = '-'
      end
      object Candidatestoremove1: TMenuItem
        Caption = 'Candidates to remove'
        object Pointreflectancespectra1: TMenuItem
          Caption = 'Point reflectance spectra, raw'
          OnClick = Pointreflectancespectra1Click
        end
        object Pointreflectancespectranormalized1: TMenuItem
          Caption = 'Point reflectance spectra, normalized'
          OnClick = Pointreflectancespectranormalized1Click
        end
      end
    end
    object Centr1: TMenuItem
      Caption = 'Centroid separations'
      OnClick = Centr1Click
    end
    object Findneighbors1: TMenuItem
      Caption = 'Find neighbors'
      OnClick = Findneighbors1Click
    end
    object Arearecordstatistics1: TMenuItem
      Caption = 'Area record statistics'
      object Multipartrecords1: TMenuItem
        Caption = 'Multi-part records'
        OnClick = Multipartrecords1Click
      end
      object Recordswithholes1: TMenuItem
        Caption = 'Records with holes'
        OnClick = Recordswithholes1Click
      end
    end
    object Fieldcooccurrencematrix1: TMenuItem
      Caption = 'Field co-occurrence matrix'
      object Symmetric1: TMenuItem
        Caption = 'Symmetric (string)'
        OnClick = Symmetric1Click
      end
      object Asymmetric1: TMenuItem
        Caption = 'Asymmetric (string)'
        OnClick = Asymmetric1Click
      end
      object Integer1: TMenuItem
        Caption = 'Integer'
        OnClick = Integer1Click
      end
    end
    object Earthquakefocalmechanisms2: TMenuItem
      Caption = 'Earthquake focal mechanisms'
      object Focalmechanisms1: TMenuItem
        Caption = 'Focal mechanisms, all on single image'
        OnClick = Focalmechanisms1Click
      end
      object Bylatitude1: TMenuItem
        Caption = 'Graph focal depth  by latitude'
        OnClick = Bylatitude1Click
      end
      object Bylongitude1: TMenuItem
        Caption = 'Graph focal depth by longitude'
        OnClick = Bylongitude1Click
      end
      object ByLatitude2: TMenuItem
        Caption = 'Graph focal depth by latitude (km units)'
        OnClick = ByLatitude2Click
      end
      object byLongitude2: TMenuItem
        Caption = 'Graph focal depth by longitude (km units)'
        OnClick = byLongitude2Click
      end
      object Earthquakefocalmechanisms3D1: TMenuItem
        Caption = 'Focal mechanisms 3D'
        OnClick = Earthquakefocalmechanisms3D1Click
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object Focalplanesonstereonet1: TMenuItem
        Caption = 'Focal planes on stereonet'
        OnClick = Focalplanesonstereonet1Click
      end
      object Polestofocalplanes1: TMenuItem
        Caption = 'Poles to focal planes'
        OnClick = Polestofocalplanes1Click
      end
      object Dipdirectionsforfocalplanes1: TMenuItem
        Caption = 'Dip directions for focal planes'
        OnClick = Dipdirectionsforfocalplanes1Click
      end
      object N30: TMenuItem
        Caption = '-'
      end
      object Rosediagramstrikes1: TMenuItem
        Caption = 'Rose diagram, focal plane strikes'
        OnClick = Rosediagramstrikes1Click
      end
      object Rosediagramdipdirections1: TMenuItem
        Caption = 'Rose diagram, focal plane dip directions'
        OnClick = Rosediagramdipdirections1Click
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object Insertdipdirectionsdipandstrike1: TMenuItem
        Caption = 'Insert focal plane dip directions, dip and strike'
        OnClick = Insertdipdirectionsdipandstrike1Click
      end
      object Legned1: TMenuItem
        Caption = 'Legend'
        OnClick = Legned1Click
      end
      object N34: TMenuItem
        Caption = '-'
      end
      object QuickKMLexport2: TMenuItem
        Caption = 'Quick KML export'
        OnClick = QuickKMLexport2Click
      end
      object DeliberateKMLexport1: TMenuItem
        Caption = 'Deliberate KML export'
      end
    end
    object Structuralgeology1: TMenuItem
      Caption = 'Structural geology'
      object hreepointproblem1: TMenuItem
        Caption = 'Three point problem'
        OnClick = hreepointproblem1Click
      end
      object StereoNetPoles1: TMenuItem
        Caption = 'Stereo net, poles'
        OnClick = StereoNetPoles1Click
      end
      object Stereonetdipdirections1: TMenuItem
        Caption = 'Stereo net, dip directions'
        OnClick = Stereonetdipdirections1Click
      end
      object Stereonetgreatcircles1: TMenuItem
        Caption = 'Stereo net, great circles'
        OnClick = Stereonetgreatcircles1Click
      end
    end
    object Geomorphometryatlas1: TMenuItem
      Caption = 'Geomorphometry atlas'
      OnClick = Geomorphometryatlas1Click
    end
    object Geomorphometrystats1: TMenuItem
      Caption = 'Geomorphometry stats (each polygon)'
      OnClick = Geomorphometrystats1Click
    end
    object Geomrophometrystaseachpointneighborhood1: TMenuItem
      Caption = 'Geomorphometry stats (each point neighborhood)'
      OnClick = Geomrophometrystaseachpointneighborhood1Click
    end
    object Koppenlatitudestats1: TMenuItem
      Caption = 'Koppen latitude stats'
      OnClick = Koppenlatitudestats1Click
    end
    object N38: TMenuItem
      Caption = '-'
    end
    object Histogram2: TMenuItem
      Caption = 'Histogram'
      object Histogram1: TMenuItem
        Caption = '1 series'
        OnClick = Histogram1Click
      end
      object N2series1: TMenuItem
        Caption = '2 series'
        OnClick = N2series1Click
      end
      object N3series1: TMenuItem
        Caption = '3 series'
        OnClick = N3series1Click
      end
      object Cumulative1: TMenuItem
        Caption = 'Cumulative'
        OnClick = Cumulative1Click
      end
      object Bycategory1: TMenuItem
        Caption = 'By category'
        OnClick = Bycategory1Click
      end
      object Allfields1: TMenuItem
        Caption = 'Select multiple fields'
        OnClick = Allfields1Click
      end
      object AllDBs1: TMenuItem
        Caption = 'All DB'#39's, single graph'
        OnClick = AllDBs1Click
      end
      object AllDBsmultiplegraphs1: TMenuItem
        Caption = 'All DB'#39's multiple graphs'
        OnClick = AllDBsmultiplegraphs1Click
      end
    end
    object N2dgraphs1: TMenuItem
      Caption = '2d graphs'
      object N2Dgraph1: TMenuItem
        Caption = '2D graph, simple (points)'
        OnClick = N2Dgraph1Click
      end
      object N2Dgraphsimplelines1: TMenuItem
        Caption = '2D graph, simple (lines)'
        OnClick = N2Dgraphsimplelines1Click
      end
      object N2Dgraph2series1: TMenuItem
        Caption = '2D graph, 2 series'
        OnClick = N2Dgraph2series1Click
      end
      object N2Dgraph2yaxes1: TMenuItem
        Caption = '2D graph, 2 y-axes'
        OnClick = N2Dgraph2yaxes1Click
      end
      object N2DgraphCOLORfield1: TMenuItem
        Caption = '2D graph, COLOR field'
        OnClick = N2DgraphCOLORfield1Click
      end
      object N2Dgraphcolorcoded1: TMenuItem
        Caption = '2D graph, color code (numeric)'
        OnClick = N2Dgraphcolorcoded1Click
      end
      object N2Dgraphcolorcodetext1: TMenuItem
        Caption = '2D graph, color code (text/integer)'
        OnClick = N2Dgraphcolorcodetext1Click
      end
      object N2Dgraphallopendatabases1: TMenuItem
        Caption = '2D graph, all open databases (points)'
        OnClick = N2Dgraphallopendatabases1Click
      end
      object N2Dgraphallopendatabaseslines1: TMenuItem
        Caption = '2D graph, all open databases (lines)'
        OnClick = N2Dgraphallopendatabaseslines1Click
      end
    end
    object N3Dgraph1: TMenuItem
      Caption = '3D graph'
      object Noverticalexaggeration1: TMenuItem
        Caption = 'No vertical exaggeration'
        OnClick = Noverticalexaggeration1Click
      end
      object Verticalexagerration1: TMenuItem
        Caption = 'Vertical exagerration'
        OnClick = Verticalexagerration1Click
      end
    end
    object N3Dslicer1: TMenuItem
      Caption = '3D slicer'
      OnClick = N3Dslicer1Click
    end
    object Boxplot1: TMenuItem
      Caption = 'Box plot'
      OnClick = Boxplot1Click
    end
    object Bargraph1: TMenuItem
      Caption = 'Bar graph'
      OnClick = Bargraph1Click
    end
    object Stickstadpoleplot1: TMenuItem
      Caption = 'Sticks/tadpole plot'
      OnClick = Stickstadpoleplot1Click
    end
    object Rosediagram4: TMenuItem
      Caption = 'Rose diagram'
      object Rosediagram1: TMenuItem
        Caption = 'Rose diagram, 0-360'
        OnClick = Rosediagram1Click
      end
      object Rosediagram01801: TMenuItem
        Caption = 'Rose diagram, 0-180'
        OnClick = Rosediagram01801Click
      end
    end
    object Ternarydiagram1: TMenuItem
      Caption = 'Ternary diagram'
      OnClick = Ternarydiagram1Click
    end
    object Linelength1: TMenuItem
      Caption = 'Line length'
      OnClick = Linelength1Click
    end
    object Linelengthsforonefield1: TMenuItem
      Caption = 'Line lengths for one field'
      OnClick = Linelengthsforonefield1Click
    end
    object LOStopoprofile1: TMenuItem
      Caption = 'LOS/topo profile'
      OnClick = LOStopoprofile1Click
    end
    object ProfileallDEMs1: TMenuItem
      Caption = 'Profile all DEMs'
      OnClick = ProfileallDEMs1Click
    end
    object Terrainprofiles1: TMenuItem
      Caption = 'Terrain profiles'
      object Allprofiles1: TMenuItem
        Caption = 'All profiles normalized'
        OnClick = Allprofiles1Click
      end
      object Allnormalizedprofiles1: TMenuItem
        Caption = 'All normalized profiles (density)'
        OnClick = Allnormalizedprofiles1Click
      end
      object Eachprofileseparately1: TMenuItem
        Caption = 'Each profile separately'
        OnClick = Eachprofileseparately1Click
      end
      object Cyclethroughterrainprofiles1: TMenuItem
        Caption = 'Cycle through terrain profiles'
        OnClick = Cyclethroughterrainprofiles1Click
      end
    end
    object PlotallXYFiles1: TMenuItem
      Caption = 'Plot all XY Files'
      OnClick = PlotallXYFiles1Click
    end
    object ICESat21: TMenuItem
      Caption = 'ICESat-2'
      object Latprofiles1: TMenuItem
        Caption = 'Lat profiles, ground and canopy'
        OnClick = Latprofiles1Click
      end
      object Latprofiles2: TMenuItem
        Caption = 'Long profiles, ground and canopy'
        OnClick = Latprofiles2Click
      end
      object N46: TMenuItem
        Caption = '-'
      end
      object Lattimecolors1: TMenuItem
        Caption = 'Lat profiles, colors from date'
        OnClick = Lattimecolors1Click
      end
      object N44: TMenuItem
        Caption = '-'
      end
      object N45: TMenuItem
        Caption = 'Clean up ICESat-2 files'
        OnClick = N45Click
      end
    end
    object LVIS1: TMenuItem
      Caption = 'LVIS'
      object Lidarwaveforms1: TMenuItem
        Caption = 'Lidar waveforms'
        OnClick = Lidarwaveforms1Click
      end
      object LVISslices1: TMenuItem
        Caption = 'LVIS slices (ZG/ZT)'
        OnClick = LVISslices1Click
      end
      object LVISslicescanopy1: TMenuItem
        Caption = 'LVIS slices canopy'
        OnClick = LVISslicescanopy1Click
      end
    end
    object PointcloudstoanalyzeglobalDEMs1: TMenuItem
      Caption = 'Lidar to analyze global DEMs'
      Visible = False
      object AddDEMdata1: TMenuItem
        Caption = 'Add DEM elev/slope/fraction'
        OnClick = AddDEMdata1Click
      end
      object Distributionhistograms1: TMenuItem
        Caption = 'Canopy distribution histograms'
        object GlobalDEMsandpointclouds1: TMenuItem
          Caption = 'All DBs'
          OnClick = GlobalDEMsandpointclouds1Click
        end
        object DistributionhistogramsthisDB1: TMenuItem
          Caption = 'This DB'
          OnClick = DistributionhistogramsthisDB1Click
        end
        object hisDBbycanopyheight1: TMenuItem
          Caption = 'This DB, by canopy height'
          OnClick = hisDBbycanopyheight1Click
        end
        object hisDBByslope1: TMenuItem
          Caption = 'This DB, by slope'
          OnClick = hisDBByslope1Click
        end
        object hisDBbylandcover1: TMenuItem
          Caption = 'This DB, by land cover'
          OnClick = hisDBbylandcover1Click
        end
        object hisDBall3groupings1: TMenuItem
          Caption = 'This DB, all 3 groupings'
          OnClick = hisDBall3groupings1Click
        end
      end
      object Dirtandairshots1: TMenuItem
        Caption = 'Canopy dirt and air shots'
        object Distributionsummary1: TMenuItem
          Caption = 'All DBs'
          OnClick = Distributionsummary1Click
        end
        object DistributionsummarythisDB1: TMenuItem
          Caption = 'This DB'
          OnClick = DistributionsummarythisDB1Click
        end
        object hisDBbylandcovercats1: TMenuItem
          Caption = 'This DB,  land cover cats'
          OnClick = hisDBbylandcovercats1Click
        end
        object hisDBslopecats1: TMenuItem
          Caption = 'This DB, slope cats'
          OnClick = hisDBslopecats1Click
        end
        object hisDBcanopyheights1: TMenuItem
          Caption = 'This DB, canopy height'
          OnClick = hisDBcanopyheights1Click
        end
        object hisDBall3groups1: TMenuItem
          Caption = 'This DB, all 3 groups'
          OnClick = hisDBall3groups1Click
        end
      end
      object Elevationslopeplots1: TMenuItem
        Caption = 'Elevation slope plots'
        OnClick = Elevationslopeplots1Click
      end
      object ICEsat5graphs1: TMenuItem
        Caption = 'DEMs within point cloud range'
        object LATprofile2: TMenuItem
          Caption = 'LAT profile current filter'
          OnClick = LATprofile2Click
        end
        object LONGprofile2: TMenuItem
          Caption = 'LONG profile current filter'
          OnClick = LONGprofile2Click
        end
      end
      object Cloudsummaries2: TMenuItem
        Caption = 'Cloud summaries'
        object Cloudsummaries1: TMenuItem
          Caption = 'All DBs'
          OnClick = Cloudsummaries1Click
        end
        object hisDBbylandcovercategory1: TMenuItem
          Caption = 'This DB, land cover cats'
          OnClick = hisDBbylandcovercategory1Click
        end
      end
      object Landcoversummary1: TMenuItem
        Caption = 'Land cover summary'
        OnClick = Landcoversummary1Click
      end
      object ICESat2filecleanup1: TMenuItem
        Caption = 'ICESat-2 file cleanup'
        OnClick = ICESat2filecleanup1Click
      end
      object ICESat2canopyaddDEMdata1: TMenuItem
        Caption = 'ICESat-2 canopy add DEM data'
        OnClick = ICESat2canopyaddDEMdata1Click
      end
      object ICESat2Beamsplitter1: TMenuItem
        Caption = 'ICESat-2 Beam splitter'
        OnClick = ICESat2Beamsplitter1Click
      end
    end
    object N39: TMenuItem
      Caption = '-'
    end
    object LoadMSTfiles1: TMenuItem
      Caption = 'Load MST files'
      OnClick = LoadMSTfiles1Click
    end
    object Featurestatistics1: TMenuItem
      Caption = 'Feature statistics'
      OnClick = Featurestatistics1Click
    end
    object rendsurface1: TMenuItem
      Caption = 'Trend surface'
      OnClick = rendsurface1Click
    end
    object Vectoraverageinbox1: TMenuItem
      Caption = 'Vector average in box'
      OnClick = Vectoraverageinbox1Click
    end
    object Graphwithranges1: TMenuItem
      Caption = 'Graph with ranges'
      OnClick = Graphwithranges1Click
    end
    object Monthlyanalysis1: TMenuItem
      Caption = 'Monthly analysis'
      object Rosediagram2: TMenuItem
        Caption = 'Rose diagram'
        OnClick = Rosediagram2Click
      end
    end
    object Timeseries1: TMenuItem
      Caption = 'Time series'
      object FFT1: TMenuItem
        Caption = 'FFT'
        OnClick = FFT1Click
      end
      object Autocorrelation1: TMenuItem
        Caption = 'Autocorrelation'
        OnClick = Autocorrelation1Click
      end
      object Crosscorrelation1: TMenuItem
        Caption = 'Cross correlation'
        OnClick = Crosscorrelation1Click
      end
      object Fitfouriercurve1: TMenuItem
        Caption = 'Fit fourier curve'
        OnClick = Fitfouriercurve1Click
      end
      object Plot: TMenuItem
        Caption = 'Plot (xy)'
        OnClick = PlotClick
      end
      object Plot1series1: TMenuItem
        Caption = 'Plot (1 series)'
        OnClick = Plot1series1Click
      end
      object Plotforsubsamples1: TMenuItem
        Caption = 'Plot for subsamples'
        OnClick = Plotforsubsamples1Click
      end
      object Linearinterpolateacrossgaps1: TMenuItem
        Caption = 'Linear interpolate across gaps'
        OnClick = Linearinterpolateacrossgaps1Click
      end
    end
    object Multiplelinearregression1: TMenuItem
      Caption = 'Multiple linear regression'
      OnClick = Multiplelinearregression1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Linkdatabase1: TMenuItem
      Caption = 'Link/join data base'
      object Setjoin1: TMenuItem
        Caption = 'Set join'
        OnClick = Setjoin1Click
      end
      object Showjoin1: TMenuItem
        Caption = 'Show join'
        OnClick = Showjoin1Click
      end
      object Clearjoin1: TMenuItem
        Caption = 'Clear join'
        OnClick = Clearjoin1Click
      end
      object Resetjoin1: TMenuItem
        Caption = 'Reset join'
        OnClick = Resetjoin1Click
      end
    end
    object Quickfilters2: TMenuItem
      Caption = 'Quick filters'
      OnClick = Quickfilters2Click
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object Oceanography2: TMenuItem
      Caption = 'Oceanography'
      object Driftmodel1: TMenuItem
        Caption = 'Drift model'
        OnClick = Driftmodel1Click
      end
      object Tidepredictions1: TMenuItem
        Caption = 'Tide predictions'
        OnClick = Tidepredictions1Click
      end
    end
    object Redistrict1: TMenuItem
      Caption = 'Redistrict'
      OnClick = Redistrict1Click
    end
    object Multiplegraphmatrix1: TMenuItem
      Caption = 'Multiple graph matrix'
      OnClick = Multiplegraphmatrix1Click
    end
    object DEMIX1: TMenuItem
      Caption = 'DEMIX'
      object Filteroutsignedcriteriameanandmedian1: TMenuItem
        Caption = 'Filter out signed criteria (mean and median)'
        OnClick = Filteroutsignedcriteriameanandmedian1Click
      end
      object FilterforDEMIXtiles1: TMenuItem
        Caption = 'Filter for DEMIX tiles'
        OnClick = FilterforDEMIXtiles1Click
      end
      object ExtractDEMIXtiles1: TMenuItem
        Caption = 'Extract DEMIX tile DEMs'
        OnClick = ExtractDEMIXtiles1Click
      end
      object DEMIXtilesummary1: TMenuItem
        Caption = 'DEMIX tile summary'
        OnClick = DEMIXtilesummary1Click
      end
      object N1degreetilestocoverrecordsintable1: TMenuItem
        Caption = '1 degree tiles to cover records in table'
        OnClick = N1degreetilestocoverrecordsintable1Click
      end
      object Graphmeanmedianbyterraincategory1: TMenuItem
        Caption = 'Graphs with various filters (experimental)'
        object Allcriteriavalues1: TMenuItem
          Caption = 'Single tile, all criteria, values'
          OnClick = Allcriteriavalues1Click
        end
        object PercentageofcriteriawhereDEMisbest1: TMenuItem
          Caption = 'Percentage of criteria where DEM is best'
          Enabled = False
          OnClick = PercentageofcriteriawhereDEMisbest1Click
        end
        object Averageranksbyarea1: TMenuItem
          Caption = 'Average ranks by area'
          OnClick = Averageranksbyarea1Click
        end
      end
      object Graphavereagescoresbyterraincategories1: TMenuItem
        Caption = 'Graph avereage scores by terrain categories'
        OnClick = Graphavereagescoresbyterraincategories1Click
      end
      object FriedmanTest1: TMenuItem
        Caption = 'Friedman Test'
        OnClick = FriedmanTest1Click
      end
      object Sumscores1: TMenuItem
        Caption = 'Sum scores based on active filters'
        OnClick = Sumscores1Click
      end
      object N47: TMenuItem
        Caption = '-'
      end
      object Graphfilters1: TMenuItem
        Caption = 'Graph filters'
        OnClick = Graphfilters1Click
      end
      object PickParam1: TMenuItem
        Caption = 'Absolute differences from reference DEM by tile'
        OnClick = PickParam1Click
      end
      object NormalizeddifferencesfromreferenceDEM1: TMenuItem
        Caption = 'Normalized differences from reference DEM by tile'
        OnClick = NormalizeddifferencesfromreferenceDEM1Click
      end
      object RankDEMs1: TMenuItem
        Caption = 'Rank DEMs and find best by criterion and tile'
        OnClick = RankDEMs1Click
      end
      object COPoALOS1: TMenuItem
        Caption = 'Winning percentages and shootouts'
        OnClick = COPoALOS1Click
      end
      object BestDEMpertilebycriteria1: TMenuItem
        Caption = 'Graphs best DEM per tile, by criteria (sort by area)'
        OnClick = BestDEMpertilebycriteria1Click
      end
      object GraphsbestDEMpertilebycriteriasortbytilecharacteristics1: TMenuItem
        Caption = 
          'Graphs best DEM per tile, by criteria (sort by tile characterist' +
          'ics)'
        OnClick = GraphsbestDEMpertilebycriteriasortbytilecharacteristics1Click
      end
      object BestDEMbycategory1: TMenuItem
        Caption = 'Graph best DEM (average score) for criteria and filters '
        OnClick = BestDEMbycategory1Click
      end
      object N7Elevationdifferencecriteria1: TMenuItem
        Caption = 'Graph difference values by DEMIX tile'
        OnClick = N7Elevationdifferencecriteria1Click
      end
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object Elevationsatbenchmarks1: TMenuItem
      Caption = 'Elevations at bench marks'
    end
    object raceroute1: TMenuItem
      Caption = 'Trace route'
      OnClick = raceroute1Click
    end
  end
  object ColorDialog1: TColorDialog
    Left = 160
    Top = 312
  end
  object PlotPopupMenu2: TPopupMenu
    Left = 352
    Top = 608
    object Colorcodebynumericfield1: TMenuItem
      Caption = 'Color code by DB numeric field'
      OnClick = Colorcodebynumericfield1Click
    end
    object ColorcodebyDBstringfield1: TMenuItem
      Caption = 'Color code by DB string field'
      OnClick = ColorcodebyDBstringfield1Click
    end
    object ColorfieldinDB1: TMenuItem
      Caption = 'Color field in DB'
      OnClick = ColorfieldinDB1Click
    end
    object ColorfieldinjoinedDB1: TMenuItem
      Caption = 'Color field in joined DB'
      OnClick = ColorfieldinjoinedDB1Click
    end
    object Proportionalsquares1: TMenuItem
      Caption = 'Proportional symbols/Icons'
      OnClick = Proportionalsquares1Click
    end
    object Defaultssymbols1: TMenuItem
      Caption = 'Defaults symbols'
      OnClick = Defaultssymbols1Click
    end
    object Iconfield1: TMenuItem
      Caption = 'Icon field'
      OnClick = Iconfield1Click
    end
    object Singleicon1: TMenuItem
      Caption = 'Single icon'
      OnClick = Singleicon1Click
    end
    object ZipATonefield1: TMenuItem
      Caption = 'ZipATone field'
      OnClick = ZipATonefield1Click
    end
    object SingleZipatone1: TMenuItem
      Caption = 'Single Zipatone'
      OnClick = SingleZipatone1Click
    end
    object Vectordata1: TMenuItem
      Caption = 'Vector data'
      OnClick = Vectordata1Click
    end
    object woorthreefieldRGB1: TMenuItem
      Caption = 'Two or three field RGB'
      OnClick = woorthreefieldRGB1Click
    end
    object Positivenegative1: TMenuItem
      Caption = 'Positive/negative'
      OnClick = Positivenegative1Click
    end
    object Earthquakemechanisms2: TMenuItem
      Caption = 'Earthquake mechanisms'
      object Earthquakemechanisms1: TMenuItem
        Caption = 'Color code'
        OnClick = Earthquakemechanisms1Click
      end
      object Beachballs1: TMenuItem
        Caption = 'Beach balls'
        OnClick = Beachballs1Click
      end
    end
    object Climatestations1: TMenuItem
      Caption = 'Climate stations'
      object Koppenclassification1: TMenuItem
        Caption = 'Koppen classification'
        OnClick = Koppenclassification1Click
      end
      object Monthlytemperatures1: TMenuItem
        Caption = 'Monthly temperatures'
        OnClick = Monthlytemperatures1Click
      end
      object Monthlyprecipitation1: TMenuItem
        Caption = 'Monthly precipitation'
        OnClick = Monthlyprecipitation1Click
      end
    end
    object PointsymbolsinDB1: TMenuItem
      Caption = 'Point symbols in DB'
      OnClick = PointsymbolsinDB1Click
    end
    object Terrainfabric1: TMenuItem
      Caption = 'Terrain fabric'
      OnClick = Terrainfabric1Click
    end
    object Dipandstrikes1: TMenuItem
      Caption = 'Dip and strikes'
      OnClick = Dipandstrikes1Click
    end
    object Pointseparation1: TMenuItem
      Caption = 'Point separation'
      OnClick = PointSeparation1Click
    end
    object Connectsequentialpoints1: TMenuItem
      Caption = 'Connect sequential points'
      OnClick = Connectsequentialpoints1Click
    end
    object N26: TMenuItem
      Caption = '-'
    end
    object Plotfanlocations2: TMenuItem
      Caption = 'Plot fan locations'
      OnClick = Plotfanlocations2Click
    end
    object Labelfans1: TMenuItem
      Caption = 'Label fans'
      OnClick = Labelfans1Click
    end
    object Plotcoveragecircles1: TMenuItem
      Caption = 'Plot coverage circles'
      OnClick = Plotcoveragecircles1Click
    end
    object Layersymbology1: TMenuItem
      Caption = 'Edit layer symbology'
      OnClick = Layersymbology1Click
    end
    object Editgazetteersymbology1: TMenuItem
      Caption = 'Edit gazetteer symbology'
      OnClick = Editgazetteersymbology1Click
    end
    object AllDBsbynumericfield1: TMenuItem
      Caption = 'All DBs by numeric field'
      OnClick = AllDBsbynumericfield1Click
    end
    object ChangeTIGERsymbology1: TMenuItem
      Caption = 'Change TIGER symbology'
      OnClick = ChangeTIGERsymbology1Click
    end
    object Zvalues1: TMenuItem
      Caption = 'Z values'
      OnClick = Zvalues1Click
    end
    object Showarearecords1: TMenuItem
      Caption = 'Record centroids'
      OnClick = Showarearecords1Click
    end
    object Mask2: TMenuItem
      Caption = 'Buffers'
      OnClick = Mask2Click
    end
    object Restrictbymapscale1: TMenuItem
      Caption = 'Restrict by map scale'
      OnClick = Restrictbymapscale1Click
    end
    object ZoomtoDBcoverage1: TMenuItem
      Caption = 'Zoom to DB coverage'
      OnClick = ZoomtoDBcoverage1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Quickfiltering2: TMenuItem
      Caption = 'Quick filtering'
      OnClick = Quickfiltering2Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Label9: TMenuItem
      Caption = 'Label'
      object Label1: TMenuItem
        Caption = 'Label all records'
        OnClick = Label1Click
      end
      object Labeleverynthrecord1: TMenuItem
        Caption = 'Label every nth record'
        OnClick = Labeleverynthrecord1Click
      end
      object Labelselectedrecords1: TMenuItem
        Caption = 'Label selected records'
        OnClick = Labelselectedrecords1Click
      end
    end
    object Legend1: TMenuItem
      Caption = 'Legend'
      OnClick = Legend1Click
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Changelatlongfields1: TMenuItem
      Caption = 'Change lat/long fields'
      OnClick = Changelatlongfields1Click
    end
    object Highlightcolor1: TMenuItem
      Caption = 'Highlight color'
      OnClick = Highlightcolor1Click
    end
    object Font1: TMenuItem
      Caption = 'Font'
      OnClick = Font1Click
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object Downhilluphillsegments1: TMenuItem
      Caption = 'Downhill/uphill segments'
      OnClick = Downhilluphillsegments1Click
    end
    object Creategrid1: TMenuItem
      Caption = 'Create grid'
      OnClick = Creategrid1Click
    end
    object Photolocations1: TMenuItem
      Caption = 'Photo locations'
      OnClick = Photolocations1Click
    end
    object Tincontour1: TMenuItem
      Caption = 'Tin contour'
      OnClick = Tincontour1Click
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object Animatefield1: TMenuItem
      Caption = 'Animate field'
      OnClick = Animatefield1Click
    end
    object Timesequence1: TMenuItem
      Caption = 'Time sequence'
    end
    object Treatasregulardatabase1: TMenuItem
      Caption = 'Treat as regular database'
      OnClick = Treatasregulardatabase1Click
    end
  end
  object ReportPopupMenu4: TPopupMenu
    Left = 168
    Top = 512
    object Text1: TMenuItem
      Caption = 'Export text (CSV)'
      OnClick = Text1Click
    end
    object HTML2: TMenuItem
      Caption = 'Export HTML'
      object HTML1: TMenuItem
        Caption = 'HTML (single table)'
        OnClick = HTML1Click
      end
      object HTMLtableperrecord1: TMenuItem
        Caption = 'HTML (table per record)'
        OnClick = HTMLtableperrecord1Click
      end
    end
    object DBFfile1: TMenuItem
      Caption = 'Export DBF file'
      OnClick = DBFfile1Click
    end
    object DataDBFonlynogeometry1: TMenuItem
      Caption = 'Export data DBF only (no geometry)'
      OnClick = DataDBFonlynogeometry1Click
    end
    object KML1: TMenuItem
      Caption = 'Export quick KML/Google Earth'
      OnClick = KML1Click
    end
    object Otherdatabaseformats1: TMenuItem
      Caption = 'Export other database formats'
      object Exporttextdeliberate1: TMenuItem
        Caption = 'Export text (deliberate)'
        OnClick = Exporttextdeliberate1Click
      end
      object DeliberateKMLGoogleEarthexport1: TMenuItem
        Caption = 'Deliberate KML/Google Earth export'
        OnClick = DeliberateKMLGoogleEarthexport1Click
      end
      object N42: TMenuItem
        Caption = '-'
      end
      object CDSformat1: TMenuItem
        Caption = 'CDS format'
        OnClick = CDSformat1Click
      end
      object XML1: TMenuItem
        Caption = 'XML'
        OnClick = XML1Click
      end
      object SQLliteDB1: TMenuItem
        Caption = 'SQLite DB'
        OnClick = SQLliteDB1Click
      end
      object DeliberateCSVtxt1: TMenuItem
        Caption = 'Deliberate CSV/txt'
        OnClick = DeliberateCSVtxt1Click
      end
    end
    object Earthquakefocalmechanisms1: TMenuItem
      Caption = 'Earthquake focal mechanisms'
      object QuickKMLexport1: TMenuItem
        Caption = 'Quick KML export'
        OnClick = QuickKMLexport1Click
      end
      object KMLexportdeliberate1: TMenuItem
        Caption = 'KML export, deliberate'
      end
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object dbfStruct: TMenuItem
      Caption = 'Data base structure'
      OnClick = dbfStructClick
    end
    object Shapefilemetadata1: TMenuItem
      Caption = 'Shapefile metadata'
      OnClick = Shapefilemetadata1Click
    end
    object Coordinatestotextfile1: TMenuItem
      Caption = 'Coordinates to text file'
      OnClick = Coordinatestotextfile1Click
    end
    object Colorarray1: TMenuItem
      Caption = 'Color array'
      OnClick = Colorarray1Click
    end
    object N14: TMenuItem
      Caption = '-'
    end
    object Exportpointstoregistrationtable1: TMenuItem
      Caption = 'Export points to registration table'
    end
    object ArcGISviewshedsensors1: TMenuItem
      Caption = 'ArcGIS viewshed sensors'
      OnClick = ArcGISviewshedsensors1Click
    end
    object Createpointshapefile1: TMenuItem
      Caption = 'Create point shapefile'
      OnClick = Createpointshapefile1Click
    end
    object CreatepointSHXindex1: TMenuItem
      Caption = 'Create point SHX index'
      OnClick = CreatepointSHXindex1Click
    end
    object CreateXYZpointshapefile1: TMenuItem
      Caption = 'Create XYZ point shapefile'
      OnClick = CreateXYZpointshapefile1Click
    end
    object Creategrid3: TMenuItem
      Caption = 'Create grid'
      OnClick = Creategrid3Click
    end
    object Createnormalilzeddatabase1: TMenuItem
      Caption = 'Create normalilzed database'
      OnClick = Createnormalilzeddatabase1Click
    end
    object Createcostsurface1: TMenuItem
      Caption = 'Create cost surface from TIGER file'
    end
    object Exportlatlongz1: TMenuItem
      Caption = 'Export lat/long/z to text file'
      OnClick = Exportlatlongz1Click
    end
    object ExportXYZtriples1: TMenuItem
      Caption = 'Export XYZ triples to table'
      OnClick = ExportXYZtriples1Click
    end
    object Thindatabase2: TMenuItem
      Caption = 'Thin database'
      OnClick = Thindatabase2Click
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object Exportsortedtable1: TMenuItem
      Caption = 'Export sorted table'
      object Ascending1: TMenuItem
        Caption = 'Ascending'
        OnClick = Ascending1Click
      end
      object Descending1: TMenuItem
        Caption = 'Descending'
        OnClick = Descending1Click
      end
    end
    object Shapefilesubset1: TMenuItem
      Caption = 'Export shape file'
      object Currentfilter1: TMenuItem
        Caption = 'Current filter'
        OnClick = Currentfilter1Click
      end
      object Exportshapefilecliptomapextent1: TMenuItem
        Caption = 'Clip to map extent (OGR)'
        OnClick = Exportshapefilecliptomapextent1Click
      end
      object RewriteinsubdirOGR1: TMenuItem
        Caption = 'Rewrite in subdir (OGR)'
        OnClick = RewriteinsubdirOGR1Click
      end
    end
    object Copyshapefile1: TMenuItem
      Caption = 'Copy shapefile'
      OnClick = Copyshapefile1Click
    end
    object Createlineshapefilefrompoints1: TMenuItem
      Caption = 'Create line/area shapefile from points'
      object Lineshapefile1: TMenuItem
        Caption = 'Line shapefile'
        OnClick = Lineshapefile1Click
      end
      object Areashapefile1: TMenuItem
        Caption = 'Area shapefile'
        OnClick = Areashapefile1Click
      end
    end
    object Createpointsfile1: TMenuItem
      Caption = 'Create points file'
      object Createpointsinpolygons1: TMenuItem
        Caption = 'In polygons'
        OnClick = Createpointsinpolygons1Click
      end
      object Createpointsalonglines1: TMenuItem
        Caption = 'Along lines (by number)'
        OnClick = Createpointsalonglines1Click
      end
      object Createpointsalonglinesbyseparation1: TMenuItem
        Caption = 'Along lines (by separation)'
        OnClick = Createpointsalonglinesbyseparation1Click
      end
      object AllpointsinlinewithXYZ1: TMenuItem
        Caption = 'All points in line, with XYZ'
        OnClick = AllpointsinlinewithXYZ1Click
      end
    end
    object Create1: TMenuItem
      Caption = 'Create shapefile with outline all databases'
      OnClick = Create1Click
    end
    object CreateDBwithcornersandcenterofeveryrecord1: TMenuItem
      Caption = 'Create DB with corners and center of every record'
      OnClick = CreateDBwithcornersandcenterofeveryrecord1Click
    end
    object Createshapefilewithboundingboxforeachrecord1: TMenuItem
      Caption = 'Create KML with bounding box for each record'
      OnClick = Createshapefilewithboundingboxforeachrecord1Click
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object Zipshapefile2: TMenuItem
      Caption = 'Zip shapefile'
      object Zipshapefile1: TMenuItem
        Caption = 'Just shapefile'
        OnClick = Zipshapefile1Click
      end
      object Includedebuglog1: TMenuItem
        Caption = 'Include debug log'
        OnClick = Includedebuglog1Click
      end
    end
    object N35: TMenuItem
      Caption = '-'
    end
    object Copydatafiles2: TMenuItem
      Caption = 'Copy data files'
      OnClick = Copydatafiles2Click
    end
    object AddDBsfieldrange1: TMenuItem
      Caption = 'All DB'#39's, field range'
      OnClick = AddDBsfieldrange1Click
    end
    object Movie1: TMenuItem
      Caption = 'Movie'
      OnClick = Movie1Click
    end
    object Legend2: TMenuItem
      Caption = 'Legend'
      OnClick = Legend2Click
    end
    object Colorbarchart1: TMenuItem
      Caption = 'Color bar chart'
      OnClick = Colorbarchart1Click
    end
    object Fonts1: TMenuItem
      Caption = 'Fonts'
      object Gridfonts1: TMenuItem
        Caption = 'Grid fonts'
        object Gridfont1: TMenuItem
          Caption = 'Record text'
          OnClick = Gridfont1Click
        end
        object Gridtitlefont1: TMenuItem
          Caption = 'Field titles'
          OnClick = Gridtitlefont1Click
        end
        object Both1: TMenuItem
          Caption = 'Both'
          OnClick = Both1Click
        end
      end
      object Legendfont1: TMenuItem
        Caption = 'Legend font'
        OnClick = Legendfont1Click
      end
    end
    object Hideunusedfields1: TMenuItem
      Caption = 'Hide unused fields'
      OnClick = Hideunusedfields1Click
    end
    object ShowDBsettings1: TMenuItem
      Caption = 'Show DB settings'
      OnClick = ShowDBsettings1Click
    end
    object ClearDBsettings1: TMenuItem
      Caption = 'Clear DB settings'
      OnClick = ClearDBsettings1Click
    end
    object Clearfieldranges1: TMenuItem
      Caption = 'Clear field  ranges'
      OnClick = Clearfieldranges1Click
    end
  end
  object QueryPopupMenu5: TPopupMenu
    Left = 168
    Top = 440
    object Selectionregion1: TMenuItem
      Bitmap.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        70777777777777070F07777777777700FFF077777777770FFF0777777777770F
        F077000700070000000707777777770777770777777777077777777777777777
        7777077777777707777707777777770777770777777777077777777777777777
        7777077777777707777707777777770777770007000700077777}
      Caption = 'Rectangular region'
      OnClick = Selectionregion1Click
    end
    object Selectirregularregion1: TMenuItem
      Caption = 'Irregular region'
      OnClick = Selectirregularregion1Click
    end
    object Selectradiusaboutpoint1: TMenuItem
      Caption = 'Radius about point'
      OnClick = Selectradiusaboutpoint1Click
    end
    object Currentmaparea1: TMenuItem
      Caption = 'Current map area'
      OnClick = Currentmaparea1Click
    end
    object Cleargeographicfilter1: TMenuItem
      Caption = 'Clear geographic filter'
      OnClick = Cleargeographicfilter1Click
    end
    object Pickpointsfortimesequentialseries1: TMenuItem
      Caption = 'Pick points for time/sequential series'
      OnClick = Pickpointsfortimesequentialseries1Click
    end
    object NearTIGERroads1: TMenuItem
      Caption = 'Near TIGER roads'
      OnClick = NearTIGERroads1Click
    end
    object AwayfromTIGERroads1: TMenuItem
      Caption = 'Away from TIGER roads'
      OnClick = AwayfromTIGERroads1Click
    end
    object FindAddress1: TMenuItem
      Caption = 'Find Address'
      OnClick = FindAddress1Click
    end
  end
  object GridCellPopupMenu6: TPopupMenu
    Left = 280
    Top = 440
    object Columnoperations1: TMenuItem
      Caption = 'Column operations'
      OnClick = Columnoperations1Click
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object RecordDisplay1: TMenuItem
      Caption = 'Record Display'
      OnClick = RecordDisplay1Click
    end
    object Editrecord1: TMenuItem
      Caption = 'Edit record'
      object Recordedit1: TMenuItem
        Caption = 'Edit record'
        OnClick = Recordedit1Click
      end
      object Editpointlocation1: TMenuItem
        Caption = 'Edit point location (graphical)'
        OnClick = Editpointlocation1Click
      end
      object AddXYbox1: TMenuItem
        Caption = 'Add XY box'
        OnClick = AddXYbox1Click
      end
      object Keyboardnewpointlocation1: TMenuItem
        Caption = 'Keyboard new point location'
        OnClick = Keyboardnewpointlocation1Click
      end
      object Editpointsymbol1: TMenuItem
        Caption = 'Edit point symbol'
        OnClick = Editpointsymbol1Click
      end
      object Editlineweightandcolor1: TMenuItem
        Caption = 'Edit line weight and color'
        OnClick = Editlineweightandcolor1Click
      end
      object Editfillpattern1: TMenuItem
        Caption = 'Edit fill pattern'
        OnClick = Editfillpattern1Click
      end
      object Editfont1: TMenuItem
        Caption = 'Edit font'
        OnClick = Editfont1Click
      end
      object EditIcon1: TMenuItem
        Caption = 'Edit Icon'
        OnClick = EditIcon1Click
      end
      object Fanproperties1: TMenuItem
        Caption = 'Edit fan properties'
        OnClick = Fanproperties1Click
      end
      object Recolorfan1: TMenuItem
        Caption = 'Edit fan color'
        OnClick = Recolorfan1Click
      end
      object Editfilename1: TMenuItem
        Caption = 'Edit filename'
        OnClick = Editfilename1Click
      end
      object Colorpoint1: TMenuItem
        Caption = 'Color record'
        OnClick = Colorpoint1Click
      end
      object Associateimage1: TMenuItem
        Caption = 'Associate image'
        OnClick = Associateimage1Click
      end
    end
    object Map1: TMenuItem
      Caption = 'Map'
      object Highlightfan1: TMenuItem
        Caption = 'Highlight fan'
        OnClick = Highlightfan1Click
      end
      object Recentermaponrecord1: TMenuItem
        Caption = 'Recenter map on record'
        OnClick = Recentermaponrecord1Click
      end
      object Zoommaptorecord1: TMenuItem
        Caption = 'Zoom map to record'
        OnClick = Zoommaptorecord1Click
      end
      object Highlightrecordonmap1: TMenuItem
        Caption = 'Highlight record on map'
        OnClick = Highlightrecordonmap1Click
      end
      object Showneighbors1: TMenuItem
        Caption = 'Show neighbors'
        OnClick = Showneighbors1Click
      end
      object Forceredrawofmap1: TMenuItem
        Caption = 'Force redraw of map'
        OnClick = Forceredrawofmap1Click
      end
    end
    object Show1: TMenuItem
      Caption = 'Show'
      object Recordcoordinates1: TMenuItem
        Caption = 'Record coordinates'
        OnClick = Recordcoordinates1Click
      end
      object RecordcoordinatesCSV1: TMenuItem
        Caption = 'Record coordinates (CSV)'
        OnClick = RecordcoordinatesCSV1Click
      end
      object Recordscreencoordinates1: TMenuItem
        Caption = 'Record screen coordinates'
        OnClick = Recordscreencoordinates1Click
      end
      object Recordboundingbox1: TMenuItem
        Caption = 'Record bounding box'
        OnClick = Recordboundingbox1Click
      end
    end
    object Calculate1: TMenuItem
      Caption = 'Calculate'
      object Linelength2: TMenuItem
        Caption = 'Calculate line length'
        OnClick = Linelength2Click
      end
      object Calculatearea1: TMenuItem
        Caption = 'Calculate area'
        OnClick = Calculatearea1Click
      end
      object Calculateperimeter1: TMenuItem
        Caption = 'Calculate perimeter'
        OnClick = Calculateperimeter1Click
      end
      object CalculateVolume1: TMenuItem
        Caption = 'Calculate volume'
        OnClick = CalculateVolume1Click
      end
      object Calculatecentroid1: TMenuItem
        Caption = 'Calculate centroid'
        OnClick = Calculatecentroid1Click
      end
      object Distancetootherrecords1: TMenuItem
        Caption = 'Distance to other records'
        OnClick = Distancetootherrecords1Click
      end
      object Requiredantennaheight1: TMenuItem
        Caption = 'Required antenna height'
        OnClick = Requiredantennaheight1Click
      end
    end
    object Views1: TMenuItem
      Caption = 'Views'
      object Outlinecameraview1: TMenuItem
        Caption = 'Outline camera view'
        OnClick = Outlinecameraview1Click
      end
      object PanoramaView1: TMenuItem
        Caption = 'Panorama View'
        OnClick = PanoramaView1Click
      end
      object Perspectiveview1: TMenuItem
        Caption = 'Perspective view'
        OnClick = Perspectiveview1Click
      end
      object DEMTerrainprofile1: TMenuItem
        Caption = 'DEM Terrain profile'
        OnClick = DEMTerrainprofile1Click
      end
      object LOSterrainprofile1: TMenuItem
        Caption = 'LOS/ terrain profile'
        OnClick = LOSterrainprofile1Click
      end
      object Normalizedbasinprofile1: TMenuItem
        Caption = 'Normalized basin profile'
        OnClick = Normalizedbasinprofile1Click
      end
      object CompareshapefileandDEMprofiles1: TMenuItem
        Caption = 'Compare shape file and DEM profiles'
        OnClick = CompareshapefileandDEMprofiles1Click
      end
      object N3Dshapefileprofile1: TMenuItem
        Caption = '3D shapefile profile (Distance)'
        OnClick = N3Dshapefileprofile1Click
      end
      object N3DshapefileprofileLongitude1: TMenuItem
        Caption = '3D shapefile profile (Longitude)'
        OnClick = N3DshapefileprofileLongitude1Click
      end
      object N3dshapefileprofileLatitude1: TMenuItem
        Caption = '3d shapefile profile (Latitude)'
        OnClick = N3dshapefileprofileLatitude1Click
      end
      object DEMTerrainblowups1: TMenuItem
        Caption = 'DEM Terrain blowups'
        OnClick = DEMTerrainblowups1Click
      end
      object Horizonblocking1: TMenuItem
        Caption = 'Horizon blocking'
        OnClick = Horizonblocking1Click
      end
      object Sunabovethehorizon1: TMenuItem
        Caption = 'Sun above the horizon'
        OnClick = Sunabovethehorizon1Click
      end
    end
    object Lidarwaveform1: TMenuItem
      Caption = 'LVIS waveform'
      OnClick = Lidarwaveform1Click
    end
    object Insertnewrecordatdistancebearing1: TMenuItem
      Caption = 'Insert new record at distance/bearing'
      OnClick = Insertnewrecordatdistancebearing1Click
    end
    object Movie2: TMenuItem
      Caption = 'Movie'
      OnClick = Movie2Click
    end
    object WWW1: TMenuItem
      Caption = 'WWW'
      OnClick = WWW1Click
    end
    object Deleterecord1: TMenuItem
      Caption = 'Delete record'
    end
    object Markanddeleteallrecordspriortothisone1: TMenuItem
      Caption = 'Select all records prior to this one'
      OnClick = Markanddeleteallrecordspriortothisone1Click
    end
    object Selectallrecordsafterthisone1: TMenuItem
      Caption = 'Select all records after this one'
      OnClick = Selectallrecordsafterthisone1Click
    end
    object MaskDEMgrid1: TMenuItem
      Caption = 'Mask DEM/grid'
      OnClick = MaskDEMgrid1Click
    end
    object Rangecircles1: TMenuItem
      Caption = 'Range circles'
      OnClick = Rangecircles1Click
    end
    object Export1: TMenuItem
      Caption = 'Export'
      object Exportlinetopointdatabase1: TMenuItem
        Caption = 'Export line/polygon to point database'
        OnClick = Exportlinetopointdatabase1Click
      end
      object RecordtoKML1: TMenuItem
        Caption = 'Record to KML'
        OnClick = RecordtoKML1Click
      end
    end
    object CreateDEM1: TMenuItem
      Caption = 'Create DEM'
      OnClick = CreateDEM1Click
    end
    object Markholes1: TMenuItem
      Caption = 'Mark holes'
      OnClick = Markholes1Click
    end
    object Load2: TMenuItem
      Caption = 'Load'
      object Loadfrommaplibrary1: TMenuItem
        Caption = 'Load from map library'
        OnClick = Loadfrommaplibrary1Click
      end
      object Loadsidescanimage1: TMenuItem
        Caption = 'Load sidescan image'
        OnClick = Loadsidescanimage1Click
      end
      object LoadTMscene1: TMenuItem
        Caption = 'Load TM scene'
        OnClick = LoadTMscene1Click
      end
      object GDALsubsettomatchthisrecord1: TMenuItem
        Caption = 'GDAL subset to match this record'
        OnClick = GDALsubsettomatchthisrecord1Click
      end
    end
    object Datastatistics1: TMenuItem
      Caption = 'Data statistics'
      object Stationtimeseries1: TMenuItem
        Caption = 'Station time series'
        OnClick = Stationtimeseries1Click
      end
      object Stratigraphiccolumn1: TMenuItem
        Caption = 'Stratigraphic column'
        OnClick = Stratigraphiccolumn1Click
      end
      object PlotXYFile1: TMenuItem
        Caption = 'Plot XY File'
        OnClick = PlotXYFile1Click
      end
    end
    object Pointinrecord1: TMenuItem
      Caption = 'Point in record'
      OnClick = Pointinrecord1Click
    end
    object Featuregeomorphometry1: TMenuItem
      Caption = 'Feature geomorphometry'
      object SSOandaspectdiagrams1: TMenuItem
        Caption = 'SSO and aspect diagrams'
      end
      object Pointslopebyalgorithm1: TMenuItem
        Caption = 'Point slope by algorithm'
        OnClick = Pointslopebyalgorithm1Click
      end
    end
    object NLCDcategories1: TMenuItem
      Caption = 'NLCD categories'
    end
    object Monthlywinds1: TMenuItem
      Caption = 'Monthly winds'
      OnClick = Monthlywinds1Click
    end
    object Ridgecountaroundpoint1: TMenuItem
      Caption = 'Ridge count around point'
      OnClick = Ridgecountaroundpoint1Click
    end
    object Accumulatedcostsurface1: TMenuItem
      Caption = 'Accumulated cost surface'
      OnClick = Accumulatedcostsurface1Click
    end
    object PointcloudandglobalDEMs1: TMenuItem
      Caption = 'Point cloud and global DEMs'
      object Pointcloudstatistics1: TMenuItem
        Caption = 'Point cloud statistics and canopy density'
        Visible = False
        OnClick = Pointcloudstatistics1Click
      end
      object Pointcloudpoints1: TMenuItem
        Caption = 'Point cloud points to DBF'
        OnClick = Pointcloudpoints1Click
      end
      object LATprofile1: TMenuItem
        Caption = 'LAT profile'
        OnClick = LATprofile1Click
      end
      object LONGprofile1: TMenuItem
        Caption = 'LONG profile'
        OnClick = LONGprofile1Click
      end
      object Bothprofiles1: TMenuItem
        Caption = 'Both profiles'
        OnClick = Bothprofiles1Click
      end
    end
    object Pastecoordinatesfromclipboard1: TMenuItem
      Caption = 'Paste coordinates from clipboard'
      OnClick = Pastecoordinatesfromclipboard1Click
    end
    object CopyCoordinates: TMenuItem
      Caption = 'Copy record coordinates to clipboad'
      OnClick = CopyCoordinatesClick
    end
  end
  object EditPopupMenu8: TPopupMenu
    Left = 440
    Top = 512
    object Color1: TMenuItem
      Caption = 'Color'
      object Insertcolorfield1: TMenuItem
        Caption = 'Insert color field'
        OnClick = Insertcolorfield1Click
      end
      object Insertpointsymbol1: TMenuItem
        Caption = 'Insert point symbol'
        OnClick = Insertpointsymbol1Click
      end
      object Insertlinecolorwidthfields1: TMenuItem
        Caption = 'Insert line color/width fields'
        OnClick = Insertlinecolorwidthfields1Click
      end
      object Insertareacolorsymbology1: TMenuItem
        Caption = 'Insert area color/symbology'
        OnClick = Insertareacolorsymbology1Click
      end
      object ColorfromRGBfloatfields1: TMenuItem
        Caption = 'Color from RGB float fields'
        OnClick = ColorfromRGBfloatfields1Click
      end
      object ColorfromRGBintegerfields1: TMenuItem
        Caption = 'Color from RGB integer fields'
        OnClick = ColorfromRGBintegerfields1Click
      end
      object Coloralllinesegments1: TMenuItem
        Caption = 'Color all line segments'
        OnClick = Coloralllinesegments1Click
      end
      object Coloralluncoloredlinesegments1: TMenuItem
        Caption = 'Color all uncolored line segments'
        OnClick = Coloralluncoloredlinesegments1Click
      end
      object Colorallrecords1: TMenuItem
        Caption = 'Color all records'
        OnClick = Colorallrecords1Click
      end
      object Coloruncoloredrecords1: TMenuItem
        Caption = 'Color uncolored records'
        OnClick = Coloruncoloredrecords1Click
      end
      object Colorbasedonfield1: TMenuItem
        Caption = 'Color based on field'
        OnClick = Colorbasedonfield1Click
      end
      object Colorbasedonjoinedtable1: TMenuItem
        Caption = 'Color based on joined table'
        OnClick = Colorbasedonjoinedtable1Click
      end
      object ColorfromKoppencategory1: TMenuItem
        Caption = 'Color from Koppen category'
        OnClick = ColorfromKoppencategory1Click
      end
      object Fillpatternallrecords1: TMenuItem
        Caption = 'Fill pattern all records'
        OnClick = Fillpatternallrecords1Click
      end
      object Fillpatternalluncoloredrecords1: TMenuItem
        Caption = 'Fill pattern all uncolored records'
        OnClick = Fillpatternalluncoloredrecords1Click
      end
      object Assignuniquecolors1: TMenuItem
        Caption = 'Assign unique colors'
        OnClick = Assignuniquecolors1Click
      end
      object Recolorallfans1: TMenuItem
        Caption = 'Recolor all fans'
        OnClick = Recolorallfans1Click
      end
    end
    object Date1: TMenuItem
      Caption = 'Date/time'
      object Inserttimeanimationfields1: TMenuItem
        Caption = 'Insert time animation fields'
        OnClick = Inserttimeanimationfields1Click
      end
      object Insertdatefield1: TMenuItem
        Caption = 'Insert date field'
        OnClick = Insertdatefield1Click
      end
      object InsertYEARMONTHDATE1: TMenuItem
        Caption = 'Insert YEAR/MONTH/DAY'
        OnClick = InsertYEARMONTHDATE1Click
      end
      object InsertJuliandate1: TMenuItem
        Caption = 'Insert Julian date (annual)'
        OnClick = InsertJuliandate1Click
      end
      object SplitdatefieldYYYYMMDD1: TMenuItem
        Caption = 'Split date field (YYYY/MM/DD)'
        OnClick = SplitdatefieldYYYYMMDD1Click
      end
      object Splitdatefield1: TMenuItem
        Caption = 'Split date field (MM/DD/YYYY or MM/DD/YY)'
        OnClick = Splitdatefield1Click
      end
      object Splitdatefield2: TMenuItem
        Caption = 'Split date field (YYYYMMDD)'
        OnClick = Splitdatefield2Click
      end
      object SplittimestringHHMMSS1: TMenuItem
        Caption = 'Split time string (HH:MM:SS)'
        OnClick = SplittimestringHHMMSS1Click
      end
      object SplittimefieldHRMN1: TMenuItem
        Caption = 'Split time field (HRMN)'
        OnClick = SplittimefieldHRMN1Click
      end
      object TimeFieldAddSnakeyes: TMenuItem
        Caption = 'Time field (HHMMSS) to HH:MM:SS'
        OnClick = TimeFieldAddSnakeyesClick
      end
      object TimefieldHHMMSStohours1: TMenuItem
        Caption = 'Time field (HHMMSS or HH:MM:SS) to dec hours'
        OnClick = TimefieldHHMMSStohours1Click
      end
      object Timefieldshourminsectodechours1: TMenuItem
        Caption = 'Time fields (hour/min/sec) to dec hours'
        OnClick = Timefieldshourminsectodechours1Click
      end
      object Timefieldstodecdays1: TMenuItem
        Caption = 'Time fields (day/hour/min/sec) to dec days'
        OnClick = Timefieldstodecdays1Click
      end
      object TimefieldstodecJuliandays1: TMenuItem
        Caption = 'Time fields to dec Julian days (annual)'
        OnClick = TimefieldstodecJuliandays1Click
      end
      object Timefieldstodecyears1: TMenuItem
        Caption = 'Time fields to dec years'
        OnClick = Timefieldstodecyears1Click
      end
      object DechourstoHHMMSS1: TMenuItem
        Caption = 'Dec hours to HH:MM:SS'
        OnClick = DechourstoHHMMSS1Click
      end
      object MilitarytimetoHHMMSS1: TMenuItem
        Caption = 'Military time to HH:MM:SS'
        OnClick = MilitarytimetoHHMMSS1Click
      end
      object DayofweekfromYRMonDay1: TMenuItem
        Caption = 'Day of week from Year/Month/Day'
        OnClick = DayofweekfromYRMonDay1Click
      end
      object Dayssincefullmoon1: TMenuItem
        Caption = 'Days since full moon'
        OnClick = Dayssincefullmoon1Click
      end
      object Formatdatefield1: TMenuItem
        Caption = 'Format date field'
        object MMDDYYYY1: TMenuItem
          Caption = 'MM/DD/YYYY'
          OnClick = MMDDYYYY1Click
        end
        object MDYYYY1: TMenuItem
          Caption = 'M/D/YYYY'
          OnClick = MDYYYY1Click
        end
        object YYYYMMDD1: TMenuItem
          Caption = 'YYYY/MM/DD'
          OnClick = YYYYMMDD1Click
        end
      end
    end
    object Icons1: TMenuItem
      Caption = 'Icons'
      object Addicons1: TMenuItem
        Caption = 'Add icons'
        OnClick = Addicons1Click
      end
      object Createuniticons1: TMenuItem
        Caption = 'Create unit icons'
        OnClick = Createuniticons1Click
      end
      object Koppenicons1: TMenuItem
        Caption = 'Koppen icons'
        OnClick = Koppenicons1Click
      end
      object Iconsfromthumbnails1: TMenuItem
        Caption = 'Icons from thumbnails'
        OnClick = Iconsfromthumbnails1Click
      end
      object Inserticonsfromfilterfile1: TMenuItem
        Caption = 'Insert icons from filter file'
        OnClick = Inserticonsfromfilterfile1Click
      end
    end
    object Font2: TMenuItem
      Caption = 'Font'
      object Addfontdefinition1: TMenuItem
        Caption = 'Add font definition'
        OnClick = Addfontdefinition1Click
      end
      object Fillfontfield1: TMenuItem
        Caption = 'Fill font field'
        OnClick = Fillfontfield1Click
      end
    end
    object Mapcoordinates1: TMenuItem
      Caption = 'Map coordinates'
      object AddLatlong1: TMenuItem
        Caption = 'Add Lat/long'
        OnClick = AddLatlong1Click
      end
      object AddUTMcoordfields1: TMenuItem
        Caption = 'Add UTM'
        OnClick = AddUTMcoordfields1Click
      end
      object AddUTMfromoffsets1: TMenuItem
        Caption = 'Add UTM from offsets'
        OnClick = AddUTMfromoffsets1Click
      end
      object AddMGRS1: TMenuItem
        Caption = 'Add MGRS'
        OnClick = AddMGRS1Click
      end
      object AddMercator1: TMenuItem
        Caption = 'Add Mercator'
        OnClick = AddMercator1Click
      end
      object Addprojectedcoordinates1: TMenuItem
        Caption = 'Add projected coordinates'
        OnClick = Addprojectedcoordinates1Click
      end
      object Adddecimalsecondsstring1: TMenuItem
        Caption = 'Add decimal seconds string'
        OnClick = Adddecimalsecondsstring1Click
      end
      object Adddecimalminutesstring1: TMenuItem
        Caption = 'Add decimal minutes string'
        OnClick = Adddecimalminutesstring1Click
      end
      object Adddecimaldegreesstring1: TMenuItem
        Caption = 'Add decimal degrees string'
        OnClick = Adddecimaldegreesstring1Click
      end
      object Fix360longs1: TMenuItem
        Caption = 'Fix 360 longs'
        OnClick = Fix360longs1Click
      end
      object Addlatlongfromshpfile1: TMenuItem
        Caption = 'Add lat/long from shp file'
        OnClick = Addlatlongfromshpfile1Click
      end
      object Addlatlongfromlinkeddatabase1: TMenuItem
        Caption = 'Add lat/long from joined database'
        OnClick = Addlatlongfromlinkeddatabase1Click
      end
      object Addboundingbox1: TMenuItem
        Caption = 'Add bounding box'
        OnClick = Addboundingbox1Click
      end
      object AddXYZfromshpfile1: TMenuItem
        Caption = 'Add XYZ from shp file'
        OnClick = AddXYZfromshpfile1Click
      end
      object AddXYZfromjoinedtable1: TMenuItem
        Caption = 'Add XYZ from joined table'
        OnClick = AddXYZfromjoinedtable1Click
      end
      object ConvertDDMMSSHstringtoLatLong1: TMenuItem
        Caption = 'Convert DD:MM.M H string to Lat/Long'
        OnClick = ConvertDDMMSSHstringtoLatLong1Click
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object ConvertUTMtolatlong1: TMenuItem
        Caption = 'Convert UTM to lat/long'
        OnClick = ConvertUTMtolatlong1Click
      end
      object PutpointsonDEMgridlocations1: TMenuItem
        Caption = 'Put points on DEM grid locations'
        OnClick = PutpointsonDEMgridlocations1Click
      end
      object Datumshift1: TMenuItem
        Caption = 'Datum shift (reproject lat/long coordinates)'
        OnClick = Datumshift1Click
      end
      object Verticaldatumshift1: TMenuItem
        Caption = 'Vertical datum shift'
        OnClick = Verticaldatumshift1Click
      end
      object N33: TMenuItem
        Caption = '-'
      end
      object Randomizeduplicatepoints1: TMenuItem
        Caption = 'Randomize duplicate points'
        OnClick = Randomizeduplicatepoints1Click
      end
      object N37: TMenuItem
        Caption = '-'
      end
      object N36: TMenuItem
        Caption = 'Repair SHP and SHX bounding boxes'
        OnClick = N36Click
      end
    end
    object Fillviewshedfields1: TMenuItem
      Caption = 'Fill viewshed fields'
      OnClick = Fillviewshedfields1Click
    end
    object N19: TMenuItem
      Caption = '-'
    end
    object Insertrecord1: TMenuItem
      Caption = 'Insert record'
      OnClick = Insertrecord1Click
    end
    object InsertNewRecClipboard: TMenuItem
      Caption = 'Insert record from clipboard'
      OnClick = InsertNewRecClipboardClick
    end
    object Mergedatabases1: TMenuItem
      Caption = 'Merge point databases'
      object Mergedatabase1: TMenuItem
        Caption = 'Pick files'
        OnClick = Mergedatabase1Click
      end
    end
    object Graphicallymovepoints1: TMenuItem
      Caption = 'Graphically move points'
      OnClick = Graphicallymovepoints1Click
    end
    object Graphicalpickandeditdatabase1: TMenuItem
      Caption = 'Graphical pick and edit record'
      OnClick = Graphicalpickandeditdatabase1Click
    end
    object Grpahicallyinsertpoint1: TMenuItem
      Caption = 'Graphically insert point'
      OnClick = Grpahicallyinsertpoint1Click
    end
    object Graphicallydelete1: TMenuItem
      Caption = 'Graphically delete'
      object Singlepoints2: TMenuItem
        Caption = 'Single points'
        OnClick = Singlepoints2Click
      end
      object Singlepoints1: TMenuItem
        Caption = 'All points in box'
        OnClick = Singlepoints1Click
      end
      object AllpointsinboxallopenDBs1: TMenuItem
        Caption = 'All points in box, all open DBs'
        OnClick = AllpointsinboxallopenDBs1Click
      end
      object Allpointsinpolygon1: TMenuItem
        Caption = 'All points in polygon'
        OnClick = Allpointsinpolygon1Click
      end
      object AllpointsinpolygonallopenDBs1: TMenuItem
        Caption = 'All points in polygon, all open DBs'
        OnClick = AllpointsinpolygonallopenDBs1Click
      end
    end
    object Deletecurrentrecord1: TMenuItem
      Caption = 'Delete current record'
      OnClick = Deletecurrentrecord1Click
    end
    object Deleteallrecords1: TMenuItem
      Caption = 'Delete all records'
      OnClick = Deleteallrecords1Click
    end
    object Thindatabase1: TMenuItem
      Caption = 'Thin database'
      OnClick = Thindatabase1Click
    end
    object Mask1: TMenuItem
      Caption = 'Mask'
      object Maskdatabasewithshapefile1: TMenuItem
        Caption = 'Mask database with shapefile'
        OnClick = Maskdatabasewithshapefile1Click
      end
      object Mask: TMenuItem
        Caption = 'Mask database with DEM/grid'
        OnClick = MaskClick
      end
      object Maskdatabasewithgazetteer1: TMenuItem
        Caption = 'Mask DEM/grid with gazetteer'
        OnClick = Maskdatabasewithgazetteer1Click
      end
      object MaskDEMfromshapefile1: TMenuItem
        Caption = 'Mask DEM/grid from shapefile'
        OnClick = MaskDEMfromshapefile1Click
      end
      object InsertMASKfield1: TMenuItem
        Caption = 'Insert MASK field'
        OnClick = InsertMASKfield1Click
      end
      object Findpointsinareas1: TMenuItem
        Caption = 'Find points in areas/polygons'
        OnClick = Findpointsinareas1Click
      end
    end
    object Copydatafiles1: TMenuItem
      Caption = 'Copy data files'
    end
    object Flag1: TMenuItem
      Caption = 'Flag'
      object Addfromsubstring1: TMenuItem
        Caption = 'Add Y/N from sub-string'
        OnClick = Addfromsubstring1Click
      end
      object Addcountfromsubstring1: TMenuItem
        Caption = 'Add count from substring'
        OnClick = Addcountfromsubstring1Click
      end
    end
    object EvaluateXYProfiles1: TMenuItem
      Caption = 'Evaluate XY Profiles'
      OnClick = EvaluateXYProfiles1Click
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object Editrecordsingrid1: TMenuItem
      Caption = 'Edit records in grid'
      OnClick = Editrecordsingrid1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Creategrid4: TMenuItem
      Caption = 'Create grid'
      OnClick = Creategrid4Click
    end
    object DEM1: TMenuItem
      Caption = 'DEM/grid'
      object AddelevationfromDEM1: TMenuItem
        Caption = 'Add interpolated elevation from DEM'
        OnClick = AddelevationfromDEM1Click
      end
      object AddnearestelevationfromDEM2: TMenuItem
        Caption = 'Add nearest elevation from DEM'
        object AddnearestelevationfromDEM1: TMenuItem
          Caption = 'Float'
          OnClick = AddnearestelevationfromDEM1Click
        end
        object Integer2: TMenuItem
          Caption = 'Integer'
          OnClick = Integer2Click
        end
      end
      object AddslopefromDEM1: TMenuItem
        Caption = 'Add slope/aspect from DEM'
        OnClick = AddslopefromDEM1Click
      end
      object AddelevationdifferencefromDEM1: TMenuItem
        Caption = 'Add elevation difference from DEM'
        OnClick = AddelevationdifferencefromDEM1Click
      end
      object AddelevationfromDEMseries1: TMenuItem
        Caption = 'Add elevation from DEM series'
        OnClick = AddelevationfromDEMseries1Click
      end
      object FindrecordsonDEM1: TMenuItem
        Caption = 'Find records on DEM'
        OnClick = FindrecordsonDEM1Click
      end
      object Pickgridandaddnearestvalue1: TMenuItem
        Caption = 'Pick grid and add nearest value'
        OnClick = Pickgridandaddnearestvalue1Click
      end
      object PlacevaluesinDEM1: TMenuItem
        Caption = 'Place values in DEM'
        OnClick = PlacevaluesinDEM1Click
      end
      object AddEGMfields1: TMenuItem
        Caption = 'Add EGM fields from grid'
        OnClick = AddEGMfields1Click
      end
      object AddEGMfieldsfromalgorithms1: TMenuItem
        Caption = 'Add EGM fields from algorithms'
        OnClick = AddEGMfieldsfromalgorithms1Click
      end
      object Computeintervisibilityfrompoint1: TMenuItem
        Caption = 'Compute intervisibility from point'
        OnClick = Computeintervisibilityfrompoint1Click
      end
      object N32: TMenuItem
        Caption = '-'
      end
      object AllDBsaddinterpolatedelevation1: TMenuItem
        Caption = 'All DB'#39's, add interpolated elevation'
        OnClick = AllDBsaddinterpolatedelevation1Click
      end
      object Allover1: TMenuItem
        Caption = 'All  open grids, all interpolated elevation'
        OnClick = Allover1Click
      end
      object Allopengridselevationdifference1: TMenuItem
        Caption = 'All open grids, elevation difference'
        OnClick = Allopengridselevationdifference1Click
      end
    end
    object Satelliteaddreflectance1: TMenuItem
      Caption = 'Satellite, add reflectance'
      OnClick = Satelliteaddreflectance1Click
    end
    object Networkends1: TMenuItem
      Caption = 'Stream network topology/Strahler order'
    end
    object Fillindrainagebasin1: TMenuItem
      Caption = 'Fill in drainage basin'
    end
    object AddNLCDcategory1: TMenuItem
      Caption = 'Add NLCD category'
    end
    object Findneighborsinseconddatabase1: TMenuItem
      Caption = 'Find neighbors in second database'
      OnClick = Findneighborsinseconddatabase1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Checklinkeddatabase1: TMenuItem
      Caption = 'Check joined database'
    end
    object f1: TMenuItem
      Caption = 'Fill fields from joined database'
      OnClick = f1Click
    end
    object AddfieldfromopenDB1: TMenuItem
      Caption = 'Add field from open DB'
      OnClick = AddfieldfromopenDB1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Geocode1: TMenuItem
      Caption = 'Geocode'
      object Geocodeaddresses1: TMenuItem
        Caption = 'Address to lat/long'
        OnClick = Geocodeaddresses1Click
      end
      object Geocodelatlong1: TMenuItem
        Caption = 'Lat/long to address'
        OnClick = Geocodelatlong1Click
      end
    end
    object Insertnewfields1: TMenuItem
      Caption = 'Insert new fields'
      object Insertfield1: TMenuItem
        Caption = 'Insert field'
        OnClick = Insertfield1Click
      end
      object Concatenatestringfields1: TMenuItem
        Caption = 'Concatenate fields'
        OnClick = Concatenatestringfields1Click
      end
      object Splitefield1: TMenuItem
        Caption = 'Split field with separator'
        object Keepafte1: TMenuItem
          Caption = 'Keep after'
          OnClick = Keepafte1Click
        end
        object Keepbefore1: TMenuItem
          Caption = 'Keep before'
          OnClick = Keepbefore1Click
        end
        object Dividetwofields1: TMenuItem
          Caption = 'Divide two fields'
          OnClick = Dividetwofields1Click
        end
        object Dividethreefields1: TMenuItem
          Caption = 'Divide three fields'
          OnClick = Dividethreefields1Click
        end
      end
      object Copyfieldtostringfield1: TMenuItem
        Caption = 'Copy string field'
        OnClick = Copyfieldtostringfield1Click
      end
      object Predefiendfields1: TMenuItem
        Caption = 'Predefiend fields'
        object Viewshedfields1: TMenuItem
          Caption = 'Insert viewshed fields'
          OnClick = Viewshedfields1Click
        end
        object Insertimagefield1: TMenuItem
          Caption = 'Insert IMAGE field'
          OnClick = Insertimagefield1Click
        end
        object InsertWWWfield1: TMenuItem
          Caption = 'Insert WWW field'
          OnClick = InsertWWWfield1Click
        end
        object InsertCLASSfield1: TMenuItem
          Caption = 'Insert CLASS field'
          OnClick = InsertCLASSfield1Click
        end
        object InsertNAMEfield1: TMenuItem
          Caption = 'Insert NAME field'
          OnClick = InsertNAMEfield1Click
        end
        object InsertDATELABEL1: TMenuItem
          Caption = 'Insert DATE_LABEL'
          OnClick = InsertDATELABEL1Click
        end
        object InsertNPTSfield1: TMenuItem
          Caption = 'Insert NPTS field'
          OnClick = InsertNPTSfield1Click
        end
        object Latlongfields1: TMenuItem
          Caption = 'Lat/long fields'
          OnClick = Latlongfields1Click
        end
      end
      object Addintegercodefield1: TMenuItem
        Caption = 'Add integer code field'
        OnClick = Addintegercodefield1Click
      end
      object Recordnumber1: TMenuItem
        Caption = 'Record number'
        OnClick = Recordnumber1Click
      end
      object XYZ1: TMenuItem
        Caption = 'X-Y-Z'
        OnClick = XYZ1Click
      end
      object EGM20081: TMenuItem
        Caption = 'EGM 2008'
        OnClick = EGM20081Click
      end
      object EGM961: TMenuItem
        Caption = 'EGM96'
        OnClick = EGM961Click
      end
    end
    object Stringfields1: TMenuItem
      Caption = 'String field operations'
      object rimblanksinstringfield1: TMenuItem
        Caption = 'Trim blanks (one field)'
        OnClick = rimblanksinstringfield1Click
      end
      object Trimblanksallstringfields1: TMenuItem
        Caption = 'Trim blanks (all string fields)'
        OnClick = Trimblanksallstringfields1Click
      end
      object rimlengthallstringfields1: TMenuItem
        Caption = 'Trim length all string fields'
        OnClick = rimlengthallstringfields1Click
      end
      object Addleadingzeros1: TMenuItem
        Caption = 'Add leading zeros'
        OnClick = Addleadingzeros1Click
      end
      object Removeleadingzeros1: TMenuItem
        Caption = 'Remove leading zeros'
        OnClick = Removeleadingzeros1Click
      end
      object Removenonnumericentries1: TMenuItem
        Caption = 'Remove non numeric entries'
        OnClick = Removenonnumericentries1Click
      end
      object FixExceldates1: TMenuItem
        Caption = 'Fix Excel dates'
        OnClick = FixExceldates1Click
      end
      object Translatefromtable1: TMenuItem
        Caption = 'Translate from table'
        OnClick = Translatefromtable1Click
      end
      object AddrecordIDfield1: TMenuItem
        Caption = 'Add record ID field'
        OnClick = AddrecordIDfield1Click
      end
      object Extractfiename1: TMenuItem
        Caption = 'Extract file name'
        OnClick = Extractfiename1Click
      end
      object Countrecordswithsubstring1: TMenuItem
        Caption = 'Count records with substring'
        OnClick = Countrecordswithsubstring1Click
      end
      object Addmaskfieldfromsubstringinmultiplefields1: TMenuItem
        Caption = 'Add mask field from substring in multiple fields'
        OnClick = Addmaskfieldfromsubstringinmultiplefields1Click
      end
    end
    object Fieldarithmetic1: TMenuItem
      Caption = 'Field arithmetic'
      object Copynumericfield1: TMenuItem
        Caption = 'Copy numeric field'
        OnClick = Copynumericfield1Click
      end
      object Sumtwofields1: TMenuItem
        Caption = 'Sum two fields'
        OnClick = Sumtwofields1Click
      end
      object Differencetwofields1: TMenuItem
        Caption = 'Difference two fields'
        OnClick = Differencetwofields1Click
      end
      object Multiplytwofields1: TMenuItem
        Caption = 'Multiply two fields'
        OnClick = Multiplytwofields1Click
      end
      object Quotienttwofields1: TMenuItem
        Caption = 'Quotient two fields (ratio)'
        OnClick = Quotienttwofields1Click
      end
      object Percentfield1: TMenuItem
        Caption = 'Percent field'
        OnClick = Percentfield1Click
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object Addconstanttofield1: TMenuItem
        Caption = 'Add constant to field'
        OnClick = Addconstanttofield1Click
      end
      object Mutliple1: TMenuItem
        Caption = 'Mutliply field by constant'
        OnClick = Mutliple1Click
      end
      object Dividefieldbyconstant1: TMenuItem
        Caption = 'Divide field by constant'
        OnClick = Dividefieldbyconstant1Click
      end
      object Fixcompassangles1: TMenuItem
        Caption = 'Fix compass angles (0-360)'
        OnClick = Fixcompassangles1Click
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object Normalizefield1: TMenuItem
        Caption = 'Normalize fields'
        OnClick = Normalizefield1Click
      end
      object Logoffield1: TMenuItem
        Caption = 'Log of field'
        OnClick = Logoffield1Click
      end
      object Sinoffield1: TMenuItem
        Caption = 'Sin of field'
        OnClick = Sinoffield1Click
      end
      object Cosoffield1: TMenuItem
        Caption = 'Cos of field'
        OnClick = Cosoffield1Click
      end
      object Averageofneighbors2: TMenuItem
        Caption = 'Average of neighbors'
        object IgnoreMissingData1: TMenuItem
          Caption = 'Ignore missing data'
          OnClick = IgnoreMissingData1Click
        end
        object MissingData0: TMenuItem
          Caption = 'Missing data is zero'
          OnClick = MissingData0Click
        end
      end
      object Sumofneighbors1: TMenuItem
        Caption = 'Sum of neighbors'
        OnClick = Sumofneighbors1Click
      end
      object Meanfilterfilter1: TMenuItem
        Caption = 'Mean filter field'
        OnClick = Meanfilterfilter1Click
      end
      object Medianfilterfield1: TMenuItem
        Caption = 'Median filter field'
        OnClick = Medianfilterfield1Click
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object Zstatistics1: TMenuItem
        Caption = 'Z statistics'
        OnClick = Zstatistics1Click
      end
      object Removemissingdata2: TMenuItem
        Caption = 'Remove missing data/infinity'
        object Removemissingdata1: TMenuItem
          Caption = 'Pick field '
          OnClick = Removemissingdata1Click
        end
        object Allfields3: TMenuItem
          Caption = 'All fields'
          OnClick = Allfields3Click
        end
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object Oceanography1: TMenuItem
        Caption = 'Oceanography'
        object Sigmatee1: TMenuItem
          Caption = 'Sigma tee'
          OnClick = Sigmatee1Click
        end
        object Sigmatheta1: TMenuItem
          Caption = 'Sigma theta'
          OnClick = Sigmatheta1Click
        end
        object Soundvelocity1: TMenuItem
          Caption = 'Sound velocity'
          OnClick = Soundvelocity1Click
        end
      end
    end
    object Multiplefieldstatistics1: TMenuItem
      Caption = 'Multiple field statistics'
      object Sum3: TMenuItem
        Caption = 'Sum'
        OnClick = Sum3Click
      end
      object Mean1: TMenuItem
        Caption = 'Mean'
        OnClick = Mean1Click
      end
      object Median1: TMenuItem
        Caption = 'Median'
        OnClick = Median1Click
      end
      object Minimum1: TMenuItem
        Caption = 'Minimum'
        OnClick = Minimum1Click
      end
      object Maximum1: TMenuItem
        Caption = 'Maximum'
        OnClick = Maximum1Click
      end
    end
    object Fieldgeometry1: TMenuItem
      Caption = 'Field geometry'
      object Areaofeachrecord1: TMenuItem
        Caption = 'Area each record (km'#178')'
        OnClick = Areaofeachrecord1Click
      end
      object Areaofeachrecordm1: TMenuItem
        Caption = 'Area each record (m'#178')'
        OnClick = Areaofeachrecordm1Click
      end
      object ALANDtoALANDKM21: TMenuItem
        Caption = 'ALAND to LAND_KM2'
        OnClick = ALANDtoALANDKM21Click
      end
      object AWATERtoWATERKM21: TMenuItem
        Caption = 'AWATER to WATER_KM2'
        OnClick = AWATERtoWATERKM21Click
      end
      object Perimeterofeachrecord1: TMenuItem
        Caption = 'Perimeter of each record'
        OnClick = Perimeterofeachrecord1Click
      end
      object Centroidofeachrecord1: TMenuItem
        Caption = 'Centroid of each record'
        OnClick = Centroidofeachrecord1Click
      end
      object Polygonshape1: TMenuItem
        Caption = 'Polygon shape'
        object Compactness1: TMenuItem
          Caption = 'Compactness (Polsby-Popper)'
          OnClick = Compactness1Click
        end
        object Meanwidth1: TMenuItem
          Caption = 'Mean width'
          OnClick = Meanwidth1Click
        end
        object Shapenumber1: TMenuItem
          Caption = 'Shape number'
          OnClick = Shapenumber1Click
        end
        object Schwartzberg1: TMenuItem
          Caption = 'Schwartzberg'
          OnClick = Schwartzberg1Click
        end
        object Perimetersquaredoverarea1: TMenuItem
          Caption = 'Perimeter squared over area'
          OnClick = Perimetersquaredoverarea1Click
        end
      end
      object Lengthofeachrecord1: TMenuItem
        Caption = 'Length of each record'
        OnClick = Lengthofeachrecord1Click
      end
      object Elevationchangeofeachrecord1: TMenuItem
        Caption = 'Elevation change of each record'
        OnClick = Elevationchangeofeachrecord1Click
      end
      object Directionofeachrecord1: TMenuItem
        Caption = 'Direction of each record'
        OnClick = Directionofeachrecord1Click
      end
      object Distancebetweentwopointsinrecord1: TMenuItem
        Caption = 'Distance between two points in record'
        OnClick = Distancebetweentwopointsinrecord1Click
      end
      object Distancetolinepolygon1: TMenuItem
        Caption = 'Distance to line/polygon'
        OnClick = Distancetolinepolygon1Click
      end
      object ClosestPointonLinePolygon1: TMenuItem
        Caption = 'Closest Point on Line/Polygon'
        OnClick = ClosestPointonLinePolygon1Click
      end
      object Distancefrompoint1: TMenuItem
        Caption = 'Distance from point'
        OnClick = Distancefrompoint1Click
      end
      object Distancetopointsintable1: TMenuItem
        Caption = 'Distance/aziumth to all points in second table'
        OnClick = Distancetopointsintable1Click
      end
      object Distancealgorithmcomparison1: TMenuItem
        Caption = 'Distance algorithm comparison'
        OnClick = Distancealgorithmcomparison1Click
      end
      object Distancetonearestneighbor1: TMenuItem
        Caption = 'Distance to nearest neighbor'
        OnClick = Distancetonearestneighbor1Click
      end
      object Distanceoffmap1: TMenuItem
        Caption = 'Distance off map'
        OnClick = Distanceoffmap1Click
      end
      object Gradient1: TMenuItem
        Caption = 'Gradient'
        OnClick = Gradient1Click
      end
      object Pointinarea1: TMenuItem
        Caption = 'Point in area/polygon'
        OnClick = Pointinarea1Click
      end
      object Sinuosity1: TMenuItem
        Caption = 'Sinuosity'
        OnClick = Sinuosity1Click
      end
      object Distanceazimuthfromxycomponents1: TMenuItem
        Caption = 'Distance/azimuth from x/y components'
        OnClick = Distanceazimuthfromxycomponents1Click
      end
      object Adjustzvaluesbasedonx1: TMenuItem
        Caption = 'Adjust z values based on x'
        OnClick = Adjustzvaluesbasedonx1Click
      end
      object Recordpoints1: TMenuItem
        Caption = 'Number points each record'
        OnClick = Recordpoints1Click
      end
      object Firstandlastpoints1: TMenuItem
        Caption = 'First and last points each record'
        OnClick = Firstandlastpoints1Click
      end
      object Shiftpointrecords1: TMenuItem
        Caption = 'Shift point records'
        OnClick = Shiftpointrecords1Click
      end
    end
    object Navigation1: TMenuItem
      Caption = 'Navigation'
      object Addazimuthtotravelpath1: TMenuItem
        Caption = 'Add travel path (azimuth, distance, speed)'
        OnClick = Addazimuthtotravelpath1Click
      end
      object NewdbfwithspeedpathbyID1: TMenuItem
        Caption = 'New DBF with speed/path by ID'
        OnClick = NewdbfwithspeedpathbyID1Click
      end
      object AddDISTANCEAZIMUTHfields1: TMenuItem
        Caption = 'Add DISTANCE/AZIMUTH fields'
        OnClick = AddDISTANCEAZIMUTHfields1Click
      end
      object Getazimuthfrommagneticheading1: TMenuItem
        Caption = 'Get azimuth from magnetic heading'
        OnClick = Getazimuthfrommagneticheading1Click
      end
      object Addspeedfromxyoruvcomponents1: TMenuItem
        Caption = 'Add speed from x/y or u/v components'
        OnClick = Addspeedfromxyoruvcomponents1Click
      end
      object Deadreckoning1: TMenuItem
        Caption = 'Dead reckoning, forward'
        OnClick = Deadreckoning1Click
      end
      object Deadreckoningbackward1: TMenuItem
        Caption = 'Dead reckoning, backward'
        OnClick = Deadreckoningbackward1Click
      end
      object Removeduplicatepositions1: TMenuItem
        Caption = 'Remove duplicate positions'
        OnClick = Removeduplicatepositions1Click
      end
      object Filltrackvoids1: TMenuItem
        Caption = 'Fill track voids'
        OnClick = Filltrackvoids1Click
      end
    end
    object Renamefieldsfromreferencetable1: TMenuItem
      Caption = 'Rename fields from reference table'
      OnClick = Renamefieldsfromreferencetable1Click
    end
    object Fillfieldforallrecords1: TMenuItem
      Caption = 'Fill field for all records'
      OnClick = Fillfieldforallrecords1Click
    end
    object Deleteunusedfields1: TMenuItem
      Caption = 'Delete unused fields'
      OnClick = Deleteunusedfields1Click
    end
    object USEfield1: TMenuItem
      Caption = 'USE field'
      object MarkallY1: TMenuItem
        Caption = 'Mark all "Y"'
        OnClick = MarkallY1Click
      end
      object MarkallN1: TMenuItem
        Caption = 'Mark all "N"'
        OnClick = MarkallN1Click
      end
      object ogglerecords1: TMenuItem
        Caption = 'Toggle records'
        OnClick = ogglerecords1Click
      end
      object CurrentflterYrestN1: TMenuItem
        Caption = 'Current flter "Y", rest "N"'
        OnClick = CurrentflterYrestN1Click
      end
    end
    object Changefieldsused1: TMenuItem
      Caption = 'Change fields used'
      OnClick = Changefieldsused1Click
    end
    object CreategeographicPRJfile1: TMenuItem
      Caption = 'Create geographic PRJ  file'
      OnClick = CreategeographicPRJfile1Click
    end
    object BackupDB1: TMenuItem
      Caption = 'Backup DB'
      OnClick = BackupDB1Click
    end
    object RestorebackupversionofDB1: TMenuItem
      Caption = 'Restore backup version of DB'
      OnClick = RestorebackupversionofDB1Click
    end
    object Octree1: TMenuItem
      Caption = 'Octree'
      OnClick = Octree1Click
    end
    object RedclassifyLoppen1: TMenuItem
      Caption = 'Reclassify Koppen'
      OnClick = RedclassifyLoppen1Click
    end
    object Reclassifyfocalmechanisms1: TMenuItem
      Caption = 'Reclassify focal mechanisms'
      OnClick = Reclassifyfocalmechanisms1Click
    end
    object N25: TMenuItem
      Caption = '-'
    end
    object Trainingclassavailable1: TMenuItem
      Caption = 'Training class available'
      OnClick = Trainingclassavailable1Click
    end
    object Clearalldatabasesettings1: TMenuItem
      Caption = 'Clear all database settings'
      OnClick = Clearalldatabasesettings1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 40
    Top = 384
  end
  object PopupMenu9: TPopupMenu
    Left = 536
    Top = 384
    object Plotfanlocations1: TMenuItem
      Caption = 'Plot fan locations'
      OnClick = Plotfanlocations1Click
    end
    object Labelsensors1: TMenuItem
      Caption = 'Label sensors'
      OnClick = Labelsensors1Click
    end
    object Quickfilters1: TMenuItem
      Caption = 'Quick filters'
      OnClick = Quickfilters1Click
    end
    object Enablealloptions1: TMenuItem
      Caption = 'Enable all options'
      OnClick = Enablealloptions1Click
    end
  end
  object FieldTitlePopupMenu7: TPopupMenu
    Left = 552
    Top = 512
    object Plotwithcolorsfromthisfield1: TMenuItem
      Caption = 'Plot with colors from this field'
      OnClick = Plotwithcolorsfromthisfield1Click
    end
    object Fieldstatistics1: TMenuItem
      Caption = 'Field statistics'
      OnClick = Fieldstatistics1Click
    end
    object Historgram1: TMenuItem
      Caption = 'Histogram'
      OnClick = Historgram1Click
    end
    object Rosediagram3: TMenuItem
      Caption = 'Rose diagram'
      OnClick = Rosediagram3Click
    end
    object Sum1: TMenuItem
      Caption = 'Sum'
      OnClick = Sum1Click
    end
    object RMSE1: TMenuItem
      Caption = 'RMSE'
      OnClick = RMSE1Click
    end
    object Longeststring2: TMenuItem
      Caption = 'Longest string'
      OnClick = Longeststring2Click
    end
    object Listuniquevalues2: TMenuItem
      Caption = 'List unique values'
      OnClick = Listuniquevalues2Click
    end
    object Frequencytable1: TMenuItem
      Caption = 'Frequency table'
      OnClick = Frequencytable1Click
    end
    object Countofrecordswithsubstring1: TMenuItem
      Caption = 'Count of records with substring'
      OnClick = Countofrecordswithsubstring1Click
    end
    object Sortfield1: TMenuItem
      Caption = 'Sort field values'
      OnClick = Sortfield1Click
    end
    object Fieldpercentiles1: TMenuItem
      Caption = 'Field percentiles'
      OnClick = Fieldpercentiles1Click
    end
    object EditField1: TMenuItem
      Caption = 'Edit field'
      object Searchandreplace1: TMenuItem
        Caption = 'Search and replace'
        OnClick = Searchandreplace1Click
      end
      object FillField1: TMenuItem
        Caption = 'Fill Field'
        OnClick = FillField1Click
      end
      object Clearfield1: TMenuItem
        Caption = 'Clear field'
        OnClick = Clearfield1Click
      end
      object N40: TMenuItem
        Caption = '-'
      end
      object Colorallrecords2: TMenuItem
        Caption = 'Color all records, single color'
        OnClick = Colorallrecords2Click
      end
      object Colorallrecordsterrainscale1: TMenuItem
        Caption = 'Color all records, terrain scale'
        OnClick = Colorallrecordsterrainscale1Click
      end
      object Colorallrecordsspectrumscale1: TMenuItem
        Caption = 'Color all records, spectrum scale'
        OnClick = Colorallrecordsspectrumscale1Click
      end
      object Colorallrecordsrainbowscale1: TMenuItem
        Caption = 'Color all records, rainbow scale'
        OnClick = Colorallrecordsrainbowscale1Click
      end
      object N41: TMenuItem
        Caption = '-'
      end
      object Changefieldtype1: TMenuItem
        Caption = 'Change field type'
        OnClick = Changefieldtype1Click
      end
      object Renamefield1: TMenuItem
        Caption = 'Rename field'
        OnClick = Renamefield1Click
      end
      object Deletefield1: TMenuItem
        Caption = 'Delete field'
        OnClick = Deletefield1Click
      end
      object Increasefieldlength1: TMenuItem
        Caption = 'Change field length'
        OnClick = Increasefieldlength1Click
      end
      object rimfield1: TMenuItem
        Caption = 'Trim field length'
        OnClick = Trimfield1Click
      end
      object Limitdecimalplaces1: TMenuItem
        Caption = 'Limit decimal places'
        OnClick = Limitdecimalplaces1Click
      end
      object Fuibn1: TMenuItem
        Caption = 'Find non-numeric values'
        OnClick = Fuibn1Click
      end
      object Stringmanipulation1: TMenuItem
        Caption = 'String manipulation'
        object Removelowercasecharacters1: TMenuItem
          Caption = 'Remove lower case characters'
          OnClick = Removelowercasecharacters1Click
        end
        object Removenumericalcharacters1: TMenuItem
          Caption = 'Remove numerical characters'
          OnClick = Removenumericalcharacters1Click
        end
        object Removeinitialcharacter1: TMenuItem
          Caption = 'Remove initial character'
          OnClick = Removeinitialcharacter1Click
        end
        object RemovefinalCharacter: TMenuItem
          Caption = 'Remove final character'
          OnClick = RemovefinalCharacterClick
        end
        object Removeifsubstringpresent1: TMenuItem
          Caption = 'Remove entire string if substring present'
          OnClick = Removeifsubstringpresent1Click
        end
        object Removesubstringandfollowingcharacters1: TMenuItem
          Caption = 'Remove substring and following characters'
          OnClick = Removesubstringandfollowingcharacters1Click
        end
        object Retainfirstncharacters1: TMenuItem
          Caption = 'Retain first n characters'
          OnClick = Retainfirstncharacters1Click
        end
        object Alphabetize1: TMenuItem
          Caption = 'Alphabetize'
          OnClick = Alphabetize1Click
        end
        object Trimblanks1: TMenuItem
          Caption = 'Trim blanks'
          OnClick = Trimblanks1Click
        end
        object ranslatefromtable1: TMenuItem
          Caption = 'Translate from table'
          OnClick = ranslatefromtable1Click
        end
        object Addfileextension1: TMenuItem
          Caption = 'Add file extension'
          OnClick = Addfileextension1Click
        end
        object Extractfilename1: TMenuItem
          Caption = 'Extract file name'
          OnClick = Extractfilename1Click
        end
        object Flipbinnames1: TMenuItem
          Caption = 'Flip bin names'
          OnClick = Flipbinnames1Click
        end
      end
    end
    object Hide1: TMenuItem
      Caption = 'Hide'
      OnClick = Hide1Click
    end
    object Find1: TMenuItem
      Caption = 'Find'
      OnClick = Find1Click
    end
    object Maskfieldbystring1: TMenuItem
      Caption = 'Mask field by sub-string'
      OnClick = Maskfieldbystring1Click
    end
    object Sort1: TMenuItem
      Caption = 'Sort table (ascending)'
      OnClick = Sort1Click
    end
    object Sorttabledescending1: TMenuItem
      Caption = 'Sort table (descending)'
      OnClick = Sorttabledescending1Click
    end
    object Clusterlegend1: TMenuItem
      Caption = 'Cluster legend'
      OnClick = Clusterlegend1Click
    end
    object Quickfilter1: TMenuItem
      Caption = 'Quick filter'
      OnClick = Quickfilter1Click
    end
    object KMLexportfilteredbyvalues1: TMenuItem
      Caption = 'KML export filtered by values'
      OnClick = KMLexportfilteredbyvalues1Click
    end
    object Updaterecordnumbers1: TMenuItem
      Caption = 'Update record numbers'
      OnClick = Updaterecordnumbers1Click
    end
  end
  object PopupMenu10: TPopupMenu
    Left = 640
    Top = 288
    object Picklineonmap1: TMenuItem
      Caption = 'Pick line on map'
      OnClick = Picklineonmap1Click
    end
    object Picklinefromfile1: TMenuItem
      Caption = 'Pick line from file'
      OnClick = Picklinefromfile1Click
    end
    object Selectlinestodisplay1: TMenuItem
      Caption = 'Select lines to display'
      OnClick = Selectlinestodisplay1Click
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object Indexnewlines1: TMenuItem
      Caption = 'Index new lines'
      OnClick = Indexnewlines1Click
    end
  end
  object ReplaceDialog1: TReplaceDialog
    Options = [frDown, frHideMatchCase, frHideWholeWord, frHideUpDown, frDisableUpDown, frDisableWholeWord]
    OnFind = ReplaceDialog1Find
    OnReplace = ReplaceDialog1Replace
    Left = 328
    Top = 312
  end
  object FindDialog1: TFindDialog
    OnFind = FindDialog1Find
    Left = 224
    Top = 312
  end
  object PopupMenu3: TPopupMenu
    Left = 512
    Top = 288
    object ValuesfromDB1: TMenuItem
      Caption = 'Values from DB'
      OnClick = ValuesfromDB1Click
    end
    object RadiusfromDB1: TMenuItem
      Caption = 'Radius from DB'
      OnClick = RadiusfromDB1Click
    end
    object Pointsinbox1: TMenuItem
      Caption = 'Points in box'
      OnClick = Pointsinbox1Click
    end
    object Specifycode1: TMenuItem
      Caption = 'Specify code'
    end
    object Pointdensitymatchmaparea1: TMenuItem
      Caption = 'Point density (match map area)'
      OnClick = Pointdensitymatchmaparea1Click
    end
  end
end
