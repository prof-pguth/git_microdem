object ImageRegForm: TImageRegForm
  Left = 428
  Top = 442
  Caption = 'Registration Points'
  ClientHeight = 392
  ClientWidth = 664
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 322
    Width = 664
    Height = 70
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 72
      Top = 41
      Width = 32
      Height = 13
      Caption = 'Label1'
    end
    object BitBtn1: TBitBtn
      Left = 5
      Top = 6
      Width = 52
      Height = 25
      Caption = 'Image Pt'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 495
      Top = 4
      Width = 158
      Height = 65
      Caption = 'Register'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'x and y'
        'x UTM only'
        'y UTM only'
        'Lat only'
        'Long only')
      TabOrder = 1
    end
    object BitBtn6: TBitBtn
      Left = 8
      Top = 37
      Width = 49
      Height = 25
      Caption = 'Datum'
      TabOrder = 2
      OnClick = BitBtn6Click
    end
    object BitBtn5: TBitBtn
      Left = 160
      Top = 6
      Width = 42
      Height = 25
      Caption = 'Coords'
      TabOrder = 3
      OnClick = BitBtn5Click
    end
    object BitBtn3: TBitBtn
      Left = 298
      Top = 6
      Width = 58
      Height = 25
      Caption = 'Next Point'
      TabOrder = 4
      OnClick = BitBtn3Click
    end
    object BitBtn2: TBitBtn
      Left = 59
      Top = 6
      Width = 45
      Height = 25
      Caption = 'Map Pt'
      TabOrder = 5
      OnClick = BitBtn2Click
    end
    object BitBtn14: TBitBtn
      Left = 238
      Top = 6
      Width = 26
      Height = 25
      Caption = 'Gaz'
      TabOrder = 6
      OnClick = BitBtn14Click
    end
    object BitBtn18: TBitBtn
      Left = 270
      Top = 6
      Width = 26
      Height = 25
      Caption = 'Tri'
      TabOrder = 7
      OnClick = BitBtn18Click
    end
    object BitBtn19: TBitBtn
      Left = 205
      Top = 6
      Width = 33
      Height = 25
      Caption = 'XY'
      TabOrder = 8
      OnClick = BitBtn19Click
    end
    object BitBtn20: TBitBtn
      Left = 104
      Top = 6
      Width = 53
      Height = 25
      Caption = 'Graph Pt'
      TabOrder = 9
      OnClick = BitBtn20Click
    end
    object CheckBox2: TCheckBox
      Left = 400
      Top = 48
      Width = 89
      Height = 17
      Caption = 'Rapid cycle'
      TabOrder = 10
      OnClick = CheckBox2Click
    end
  end
  object Panel2: TPanel
    Left = 567
    Top = 0
    Width = 97
    Height = 322
    Align = alRight
    TabOrder = 1
    object BitBtn11: TBitBtn
      Left = 6
      Top = 0
      Width = 81
      Height = 25
      Caption = 'Reg File'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = BitBtn11Click
    end
    object HelpBtn: TBitBtn
      Left = 6
      Top = 200
      Width = 73
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn12: TBitBtn
      Left = 6
      Top = 176
      Width = 75
      Height = 25
      Caption = 'Clear Pnts'
      TabOrder = 2
      OnClick = BitBtn12Click
    end
    object BitBtn13: TBitBtn
      Left = 6
      Top = 151
      Width = 75
      Height = 25
      Caption = 'KML Pt Import'
      TabOrder = 3
      OnClick = BitBtn13Click
    end
    object CheckBox1: TCheckBox
      Left = 6
      Top = 49
      Width = 97
      Height = 17
      Caption = 'Label reg pts'
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object BitBtn9: TBitBtn
      Left = 6
      Top = 89
      Width = 35
      Height = 25
      Caption = 'Font'
      TabOrder = 5
      OnClick = BitBtn9Click
    end
    object BitBtn10: TBitBtn
      Left = 47
      Top = 89
      Width = 30
      Height = 25
      TabOrder = 6
      OnClick = BitBtn10Click
    end
    object BitBtn16: TBitBtn
      Left = 6
      Top = 233
      Width = 73
      Height = 25
      Caption = 'Quad map'
      TabOrder = 7
      OnClick = BitBtn16Click
    end
    object Edit1: TEdit
      Left = 16
      Top = 290
      Width = 57
      Height = 21
      TabOrder = 8
      Text = '0.05'
    end
    object BitBtn17: TBitBtn
      Left = 6
      Top = 264
      Width = 75
      Height = 25
      Caption = 'Stretch'
      TabOrder = 9
      OnClick = BitBtn17Click
    end
    object CheckBox3: TCheckBox
      Left = 6
      Top = 31
      Width = 97
      Height = 17
      Caption = 'Plot reg points'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 6
      Top = 66
      Width = 97
      Height = 17
      Caption = 'Plot triangles'
      TabOrder = 11
      OnClick = CheckBox4Click
    end
    object BitBtn7: TBitBtn
      Left = 47
      Top = 120
      Width = 34
      Height = 25
      TabOrder = 12
      OnClick = BitBtn7Click
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 567
    Height = 322
    Align = alClient
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = DBGrid1CellClick
  end
  object DataSource1: TDataSource
    Left = 40
    Top = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 144
    Top = 120
    object Deletecontrolpoint1: TMenuItem
      Caption = 'Delete control point'
      OnClick = Deletecontrolpoint1Click
    end
    object Movecontrolpointtoend1: TMenuItem
      Caption = 'Move control point to end '
      OnClick = Movecontrolpointtoend1Click
    end
    object Insertlatlongfromclipboard1: TMenuItem
      Caption = 'Insert lat/long from clipboard'
      OnClick = Insertlatlongfromclipboard1Click
    end
    object Insertimagecoordinatesfromclipboard1: TMenuItem
      Caption = 'Insert image coordinates from clipboard'
      OnClick = Insertimagecoordinatesfromclipboard1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 240
    Top = 96
    object TINmapping1: TMenuItem
      Caption = 'TIN mapping'
      OnClick = TINmapping1Click
    end
    object GDALtranslate1: TMenuItem
      Caption = 'GDAL_translate to Lat/Long'
      OnClick = GDALtranslate1Click
    end
    object GDALtranslastetoUTM1: TMenuItem
      Caption = 'GDAL translaste to UTM'
      OnClick = GDALtranslastetoUTM1Click
    end
    object Simpleregistrations1: TMenuItem
      Caption = 'Simple registrations'
      object UTMworldfile1: TMenuItem
        Caption = 'UTM world file'
        OnClick = UTMworldfile1Click
      end
      object Geographicworldfile1: TMenuItem
        Caption = 'Geographic world file'
        OnClick = Geographicworldfile1Click
      end
      object Rotationregistrationfile1: TMenuItem
        Caption = 'Rotation registration file'
        OnClick = Rotationregistrationfile1Click
      end
      object XYRotation1: TMenuItem
        Caption = 'XY Rotation'
        OnClick = XYRotation1Click
      end
      object Findoptimalrotationregistration1: TMenuItem
        Caption = 'Find optimal rotation registration'
        OnClick = Findoptimalrotationregistration1Click
      end
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ShiftprimeMeridian1: TMenuItem
      Caption = 'Shift prime Meridian'
      object Ferro1: TMenuItem
        Caption = 'Arbitrary'
      end
      object Ferro2: TMenuItem
        Caption = 'Ferro'
        OnClick = Ferro2Click
      end
    end
    object l1: TMenuItem
      Caption = 'Lat long graphs'
      OnClick = l1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Reprojectmap2: TMenuItem
      Caption = 'Reproject map'
      object ExportmaptoGoogleEarth1: TMenuItem
        Caption = 'Export map to Google Earth'
        OnClick = ExportmaptoGoogleEarth1Click
      end
      object Reprojectmap1: TMenuItem
        Caption = 'Reproject map, Geotiff'
        OnClick = Reprojectmap1Click
      end
      object KMLandGeotiff1: TMenuItem
        Caption = 'KML and Geotiff'
        OnClick = KMLandGeotiff1Click
      end
    end
  end
end
