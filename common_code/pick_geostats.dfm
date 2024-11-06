object PickGeoStat: TPickGeoStat
  Left = 0
  Top = 0
  Caption = 'Geomorphometry statstistics'
  ClientHeight = 381
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label3: TLabel
    Left = 128
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object Panel1: TPanel
    Left = 0
    Top = 304
    Width = 524
    Height = 77
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 256
      Top = 17
      Width = 67
      Height = 13
      Caption = 'Stat sampling '
    end
    object Edit1: TEdit
      Left = 329
      Top = 18
      Width = 56
      Height = 21
      TabOrder = 0
      Text = ' '
      OnChange = Edit1Change
    end
    object Radiogroup1: TRadioGroup
      Left = 8
      Top = 6
      Width = 129
      Height = 67
      Caption = 'Analysis region'
      Items.Strings = (
        'Entire DEM'
        'Current map area')
      TabOrder = 1
      OnClick = Radiogroup1Click
    end
    object CheckBox2: TCheckBox
      Left = 143
      Top = 43
      Width = 97
      Height = 17
      Caption = 'All open DEMs'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object BitBtn10: TBitBtn
      Left = 432
      Top = 12
      Width = 77
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 3
      OnClick = BitBtn10Click
      IsControl = True
    end
    object Button15: TButton
      Left = 143
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Single DEM'
      TabOrder = 4
      OnClick = Button15Click
    end
    object BitBtn23: TBitBtn
      Left = 248
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Pick DEMs'
      TabOrder = 5
      OnClick = BitBtn23Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 339
    Height = 304
    ActivePage = TabSheet1
    Align = alClient
    MultiLine = True
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Basic'
      object Button6: TButton
        Left = 131
        Top = 7
        Width = 41
        Height = 25
        Caption = 'Opts'
        TabOrder = 0
        OnClick = Button6Click
      end
      object Button5: TButton
        Left = 3
        Top = 7
        Width = 122
        Height = 25
        Caption = 'Elevation/Slope/Aspect'
        TabOrder = 1
        OnClick = Button5Click
      end
      object BitBtn6: TBitBtn
        Left = 3
        Top = 104
        Width = 169
        Height = 25
        Caption = 'Aspect distribution by slope'
        TabOrder = 2
        OnClick = BitBtn6Click
      end
      object BitBtn12: TBitBtn
        Left = 3
        Top = 38
        Width = 122
        Height = 25
        Caption = 'Elevation histograms'
        TabOrder = 3
        OnClick = BitBtn12Click
      end
      object Button7: TButton
        Left = 131
        Top = 38
        Width = 43
        Height = 25
        Caption = 'Opts'
        TabOrder = 4
        OnClick = Button7Click
      end
      object Differen: TBitBtn
        Left = 3
        Top = 135
        Width = 122
        Height = 25
        Caption = 'Moment Report'
        TabOrder = 5
        OnClick = DifferenClick
      end
      object Button12: TButton
        Left = 131
        Top = 135
        Width = 41
        Height = 25
        Caption = 'Opts'
        TabOrder = 6
        OnClick = Button12Click
      end
      object Button14: TButton
        Left = 3
        Top = 69
        Width = 171
        Height = 25
        Caption = 'Quick Elev Histogram'
        TabOrder = 7
        OnClick = Button14Click
      end
      object TBitBtn
        Left = 180
        Top = 69
        Width = 125
        Height = 25
        Caption = 'Multiple histograms'
        TabOrder = 8
        OnClick = BitBtn29Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'SSO'
      ImageIndex = 1
      object Label4: TLabel
        Left = 184
        Top = 16
        Width = 96
        Height = 13
        Caption = 'Max concenrtration '
      end
      object BitBtn14: TBitBtn
        Left = 0
        Top = 38
        Width = 169
        Height = 25
        Caption = 'SSO diagram (downhill direction)'
        TabOrder = 0
        OnClick = BitBtn8Click
      end
      object BitBtn15: TBitBtn
        Left = 0
        Top = 69
        Width = 169
        Height = 25
        Caption = 'SSO distribution by slope'
        Enabled = False
        TabOrder = 1
        OnClick = BitBtn15Click
      end
      object BitBtn8: TBitBtn
        Left = 0
        Top = 7
        Width = 169
        Height = 25
        Caption = 'SSO diagram (normals)'
        TabOrder = 2
        OnClick = BitBtn8Click
      end
      object BitBtn13: TBitBtn
        Left = 0
        Top = 131
        Width = 169
        Height = 25
        Caption = 'Fabric wavelength/height'
        TabOrder = 3
        OnClick = BitBtn13Click
      end
      object Edit2: TEdit
        Left = 286
        Top = 9
        Width = 33
        Height = 21
        TabOrder = 4
        Text = 'Edit2'
        OnChange = Edit2Change
      end
      object BitBtn18: TBitBtn
        Left = 208
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Net options'
        TabOrder = 5
        OnClick = BitBtn18Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 2
      object BitBtn11: TBitBtn
        Left = 3
        Top = 65
        Width = 169
        Height = 25
        Caption = 'Point classification'
        TabOrder = 0
        OnClick = BitBtn11Click
      end
      object Ge: TBitBtn
        Left = 3
        Top = 3
        Width = 169
        Height = 25
        Caption = 'Geomorphometrics by block'
        TabOrder = 1
        OnClick = GeClick
      end
      object BitBtn9: TBitBtn
        Left = 3
        Top = 34
        Width = 169
        Height = 25
        Caption = 'Graph parameter by region size'
        TabOrder = 2
        OnClick = BitBtn9Click
      end
      object BitBtn20: TBitBtn
        Left = 3
        Top = 162
        Width = 169
        Height = 25
        Caption = 'Roughness maps'
        TabOrder = 3
        OnClick = BitBtn20Click
      end
      object StringGrid1: TStringGrid
        Left = 213
        Top = 10
        Width = 68
        Height = 159
        FixedCols = 0
        RowCount = 6
        ScrollBars = ssNone
        TabOrder = 4
        RowHeights = (
          23
          24
          24
          24
          24
          24)
      end
      object BitBtn22: TBitBtn
        Left = 3
        Top = 96
        Width = 169
        Height = 25
        Caption = 'TRI/TPI '
        TabOrder = 5
        OnClick = BitBtn22Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Fractal'
      ImageIndex = 3
      object Button8: TButton
        Left = 143
        Top = 101
        Width = 41
        Height = 25
        Caption = 'Opts'
        TabOrder = 0
        OnClick = Button8Click
      end
      object BitBtn4: TBitBtn
        Left = 16
        Top = 101
        Width = 121
        Height = 25
        Caption = 'MEM Power Spectrum'
        TabOrder = 1
        OnClick = BitBtn4Click
      end
      object BitBtn3: TBitBtn
        Left = 16
        Top = 70
        Width = 169
        Height = 25
        Caption = 'Variogram-semivariogram'
        TabOrder = 2
        OnClick = BitBtn3Click
      end
      object BitBtn2: TBitBtn
        Left = 16
        Top = 39
        Width = 169
        Height = 25
        Caption = 'Fast Fourier Transform'
        TabOrder = 3
        OnClick = BitBtn2Click
      end
      object BitBtn1: TBitBtn
        Left = 16
        Top = 8
        Width = 169
        Height = 25
        Caption = 'Triangular prism fractal'
        TabOrder = 4
        OnClick = BitBtn1Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Surfaces'
      ImageIndex = 4
      object Button1: TButton
        Left = 3
        Top = 34
        Width = 169
        Height = 25
        Caption = 'Compare trend surfaces'
        TabOrder = 0
        OnClick = Button1Click
      end
      object BitBtn16: TBitBtn
        Left = 3
        Top = 3
        Width = 169
        Height = 25
        Caption = 'Trend surface'
        TabOrder = 1
        OnClick = BitBtn16Click
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Algorithm'
      ImageIndex = 5
      object BitBtn7: TBitBtn
        Left = 3
        Top = 96
        Width = 211
        Height = 25
        Caption = 'Compare Aspect algorithms'
        TabOrder = 0
        OnClick = BitBtn7Click
      end
      object Button13: TButton
        Left = 3
        Top = 127
        Width = 211
        Height = 25
        Caption = 'Compare samplng for moments'
        TabOrder = 1
        OnClick = Button13Click
      end
      object Button9: TButton
        Left = 3
        Top = 34
        Width = 211
        Height = 25
        Caption = 'Slope region size'
        TabOrder = 2
        OnClick = Button9Click
      end
      object BitBtn5: TBitBtn
        Left = 3
        Top = 3
        Width = 211
        Height = 25
        Caption = 'Diff map 2 Slope algorithms'
        TabOrder = 3
        OnClick = BitBtn5Click
      end
      object BitBtn17: TBitBtn
        Left = 3
        Top = 65
        Width = 211
        Height = 25
        Caption = 'Compare slope algorithms histograms'
        TabOrder = 4
        OnClick = BitBtn17Click
      end
      object BitBtn19: TBitBtn
        Left = 3
        Top = 158
        Width = 211
        Height = 25
        Caption = 'Compare slope algorithms maps'
        TabOrder = 5
        OnClick = BitBtn19Click
      end
      object BitBtn21: TBitBtn
        Left = 3
        Top = 189
        Width = 211
        Height = 25
        Caption = 'Compare slope by program'
        TabOrder = 6
        OnClick = BitBtn21Click
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Defaults'
      ImageIndex = 6
      object Label2: TLabel
        Left = 160
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Label2'
      end
      object Label5: TLabel
        Left = 24
        Top = 168
        Width = 154
        Height = 13
        Caption = 'Elevation histogram bin size (m) '
      end
      object Label6: TLabel
        Left = 24
        Top = 192
        Width = 136
        Height = 13
        Caption = 'Slope histogram bin size (%)'
      end
      object Button11: TButton
        Left = 16
        Top = 120
        Width = 129
        Height = 25
        Caption = 'Reset defaults'
        TabOrder = 0
        OnClick = Button11Click
      end
      object Button2: TButton
        Left = 16
        Top = 31
        Width = 129
        Height = 25
        Caption = 'Slope categories'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button10: TButton
        Left = 16
        Top = 0
        Width = 129
        Height = 25
        Caption = 'Slope algorithm'
        TabOrder = 2
        OnClick = Button10Click
      end
      object CheckBox1: TCheckBox
        Left = 48
        Top = 62
        Width = 137
        Height = 17
        Caption = 'Quick slope spacing'
        TabOrder = 3
        OnClick = CheckBox1Click
      end
      object CheckBox3: TCheckBox
        Left = 48
        Top = 88
        Width = 166
        Height = 17
        Caption = 'Aspect relative true north'
        TabOrder = 4
        OnClick = CheckBox3Click
      end
      object Edit3: TEdit
        Left = 184
        Top = 165
        Width = 73
        Height = 21
        TabOrder = 5
        Text = 'Edit3'
        OnChange = Edit3Change
      end
      object Edit4: TEdit
        Left = 184
        Top = 192
        Width = 73
        Height = 21
        TabOrder = 6
        Text = 'Edit4'
        OnChange = Edit4Change
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Maps'
      ImageIndex = 7
      object BitBtn24: TBitBtn
        Left = 16
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Aspect'
        TabOrder = 0
        OnClick = BitBtn24Click
      end
      object BitBtn25: TBitBtn
        Left = 16
        Top = 55
        Width = 75
        Height = 25
        Caption = 'TRIK'
        TabOrder = 1
        OnClick = BitBtn25Click
      end
      object BitBtn26: TBitBtn
        Left = 16
        Top = 86
        Width = 75
        Height = 25
        Caption = 'Slope'
        TabOrder = 2
        OnClick = BitBtn26Click
      end
      object BitBtn27: TBitBtn
        Left = 16
        Top = 120
        Width = 75
        Height = 25
        Caption = 'Ruff-slope std'
        Enabled = False
        TabOrder = 3
      end
    end
  end
  object Memo1: TMemo
    Left = 339
    Top = 0
    Width = 185
    Height = 304
    Align = alRight
    TabOrder = 2
  end
end
