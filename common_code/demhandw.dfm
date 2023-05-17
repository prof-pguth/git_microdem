object DemHandForm: TDemHandForm
  Left = 245
  Top = 244
  BorderIcons = [biSystemMenu]
  Caption = 'Data Manipulation'
  ClientHeight = 522
  ClientWidth = 890
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 16
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 890
    Height = 503
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    Visible = False
    ExplicitWidth = 926
    ExplicitHeight = 512
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 503
    Width = 890
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 250
      end>
    ExplicitTop = 512
    ExplicitWidth = 926
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 24
    object File1: TMenuItem
      Caption = '&File'
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Import1: TMenuItem
      Caption = '&Import'
      object DEMS2: TMenuItem
        Caption = 'Binary DEMs/grids'
        object BinaryrawDEM1: TMenuItem
          Caption = 'Binary raw DEM (16 bit)'
        end
        object BinaryrawDEM32bit1: TMenuItem
          Caption = 'Binary raw DEM (32 bit)'
        end
      end
      object ASCII1: TMenuItem
        Caption = 'ASCII DEMs/grids'
        object ASCIIXYZ1: TMenuItem
          Caption = 'ASCII XYZ'
        end
      end
      object Satellite1: TMenuItem
        Caption = '&Satellite'
        object BMPRGBcolor1: TMenuItem
          Caption = 'BMP (RGB color)'
        end
        object BMP1: TMenuItem
          Caption = 'BMP (monochrome)'
        end
        object Createheader1: TMenuItem
          Caption = 'Create header'
        end
        object MrSIDtotiff1: TMenuItem
          Caption = 'MrSID to tiff'
          OnClick = MrSIDtotiff1Click
        end
        object Resizeimages1: TMenuItem
          Caption = 'Resize images'
          OnClick = Resizeimages1Click
        end
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object USGSgazetteerdatabase1: TMenuItem
        Caption = 'USGS gazetteer database'
        OnClick = USGSgazetteerdatabase1Click
      end
      object NIMAgazetteerdatabase1: TMenuItem
        Caption = 'NGA gazetteer database'
        OnClick = NIMAgazetteerdatabase1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Harvardearthquakecentroids1: TMenuItem
        Caption = 'Global CMT Catalog'
        object Maindatabase1: TMenuItem
          Caption = 'Main database'
          OnClick = Maindatabase1Click
        end
        object Separatefile1: TMenuItem
          Caption = 'Separate file'
          OnClick = Separatefile1Click
        end
      end
      object Ducknearshoresurveys2: TMenuItem
        Caption = 'Duck nearshore surveys'
        object Ducknearshoresurveys1: TMenuItem
          Caption = 'Daily 3D to DBF'
          OnClick = Ducknearshoresurveys1Click
        end
        object DBFtoprofileshapefiles1: TMenuItem
          Caption = 'DBF to profile shapefiles'
        end
      end
      object ISOGravity1: TMenuItem
        Caption = 'ISO gravity stations'
        OnClick = ISOGravity1Click
      end
      object Plateboundaryfile1: TMenuItem
        Caption = 'Plate boundary file'
        OnClick = Plateboundaryfile1Click
      end
      object Cloudcompare1: TMenuItem
        Caption = 'Cloud compare ASCII'
        object CloudCompareASCIIfile1: TMenuItem
          Caption = 'Cloud to cloud distance'
        end
        object RANSAC1: TMenuItem
          Caption = 'RANSAC'
          OnClick = RANSAC1Click
        end
      end
      object XYADBfile1: TMenuItem
        Caption = 'XYADB file'
        OnClick = XYADBfile1Click
      end
      object Monthlytimeseries1: TMenuItem
        Caption = 'Monthly time series'
        OnClick = Monthlytimeseries1Click
      end
      object ICOADSLMRF1: TMenuItem
        Caption = 'ICOADS LMRF'
        OnClick = ICOADSLMRF1Click
      end
      object NCEPNCARReanalysiswinds1: TMenuItem
        Caption = 'NCEP/NCAR Reanalysis winds'
        OnClick = NCEPNCARReanalysiswinds1Click
      end
      object ExtractXYZfromVRML1: TMenuItem
        Caption = 'Extract XYZ from VRML'
      end
      object ENVIHDRIMG1: TMenuItem
        Caption = 'ENVI HDR/IMG'
        OnClick = ENVIHDRIMG1Click
      end
      object Icesat1: TMenuItem
        Caption = 'Icesat-2 to LAS'
        OnClick = Icesat1Click
      end
      object SOESTtidetimeseries1: TMenuItem
        Caption = 'SOEST tide time series'
        OnClick = SOESTtidetimeseries1Click
      end
      object Cardfileimport1: TMenuItem
        Caption = 'Card file import'
        OnClick = Cardfileimport1Click
      end
      object SF3Censusdata1: TMenuItem
        Caption = 'SF3 Census data'
        OnClick = SF3Censusdata1Click
      end
      object ExtractXYZfromDXFfile1: TMenuItem
        Caption = 'Extract XYZ from DXF file'
        OnClick = ExtractXYZfromDXFfile1Click
      end
      object ASCIIXYZto3Dshape1: TMenuItem
        Caption = 'ASCII XYZ to 3D shape'
      end
      object xview2json1: TMenuItem
        Caption = 'xview2 json to shapefile'
        OnClick = xview2json1Click
      end
      object LineKML1: TMenuItem
        Caption = 'Line KML to shapefile'
        OnClick = LineKML1Click
      end
    end
    object Export1: TMenuItem
      Caption = '&Export'
      object SINfiletoshapefile1: TMenuItem
        Caption = 'SIN file to shapefile'
        OnClick = SINfiletoshapefile1Click
      end
    end
    object Subset1: TMenuItem
      Caption = '&Subset'
      object DEMs1: TMenuItem
        Caption = 'DEMs/grids'
        OnClick = DEMs1Click
      end
      object MaskDEMs1: TMenuItem
        Caption = 'Mask DEMs/grids'
        OnClick = MaskDEMs1Click
      end
      object LASfilesbysize1: TMenuItem
        Caption = 'LAS files by size'
        OnClick = LASfilesbysize1Click
      end
    end
    object Resample1: TMenuItem
      Caption = '&Resample'
      object Shapefile1: TMenuItem
        Caption = 'Shapefile'
        object ArbitrarytoLatLong1: TMenuItem
          Caption = 'Arbitrary to Lat/Long'
          OnClick = ArbitrarytoLatLong1Click
        end
        object Datumshift1: TMenuItem
          Caption = 'Datum shift'
          OnClick = Datumshift1Click
        end
        object Specifyshift1: TMenuItem
          Caption = 'Specify shift'
          OnClick = Specifyshift1Click
        end
        object N13: TMenuItem
          Caption = '-'
        end
        object AddLineEndPoints: TMenuItem
          Caption = 'Add line endpoints to DBF file'
          OnClick = AddLineEndPointsClick
        end
        object AddXYZtoDBFfile1: TMenuItem
          Caption = 'Add XYZ to DBF file'
          OnClick = AddXYZtoDBFfile1Click
        end
        object RepairSHPandSHXboundingboxes1: TMenuItem
          Caption = 'Repair SHP and SHX bounding boxes'
          OnClick = RepairSHPandSHXboundingboxes1Click
        end
        object CreateSHXfilesforDBFfiles1: TMenuItem
          Caption = 'Create SHX files for DBF files'
          OnClick = CreateSHXfilesforDBFfiles1Click
        end
        object CreateDBFfile1: TMenuItem
          Caption = 'Create DBF file'
          OnClick = CreateDBFfile1Click
        end
        object N14: TMenuItem
          Caption = '-'
        end
        object Rename1: TMenuItem
          Caption = 'Rename'
          OnClick = Rename1Click
        end
      end
      object Database2: TMenuItem
        Caption = 'Database'
        object IGERredistricting1: TMenuItem
          Caption = 'TIGER redistricting'
          OnClick = IGERredistricting1Click
        end
        object ShiftXYZcoordinates1: TMenuItem
          Caption = 'Shift XYZ coordinates'
          OnClick = ShiftXYZcoordinates1Click
        end
        object Repairheaders1: TMenuItem
          Caption = 'Repair headers'
          OnClick = Repairheaders1Click
        end
        object RenameSF3fields1: TMenuItem
          Caption = 'Rename  fields'
          OnClick = RenameSF3fields1Click
        end
        object Addfilenameasfield1: TMenuItem
          Caption = 'Add file name as field'
          OnClick = Addfilenameasfield1Click
        end
      end
      object GDAL1: TMenuItem
        Caption = 'GDAL rasters'
        object Untilegeotiffs1: TMenuItem
          Caption = 'GDAL convert to Geotiff'
          OnClick = Untilegeotiffs1Click
        end
        object GDALmergeGeotiff1: TMenuItem
          Caption = 'GDAL merge Geotiff'
          OnClick = GDALmergeGeotiff1Click
        end
        object GDALbandextraction1: TMenuItem
          Caption = 'GDAL band extraction'
          OnClick = GDALbandextraction1Click
        end
        object GDALreprojectimagetoUTMNAD831: TMenuItem
          Caption = 'GDAL translate Geotiff to UTM, NAD83 or WGS84'
          OnClick = GDALreprojectimagetoUTMNAD831Click
        end
        object GDALassignandwarptoUTM1: TMenuItem
          Caption = 'GDAL assign EPSG and warp to UTM'
          OnClick = GDALassignandwarptoUTM1Click
        end
        object GDALwarpGeotifftoUTMNAD831: TMenuItem
          Caption = 'GDAL warp Geotiff to UTM, NAD83 or WGS84'
          OnClick = GDALwarpGeotifftoUTMNAD831Click
        end
        object GDALwarpGeotifftoadjacentUTMzone1: TMenuItem
          Caption = 'GDAL warp Geotiff to adjacent UTM zone'
          OnClick = GDALwarpGeotifftoadjacentUTMzone1Click
        end
        object GDALwarpGeotifftoGeoWGS841: TMenuItem
          Caption = 'GDAL warp Geotiff to Geo/WGS84'
          OnClick = GDALwarpGeotifftoGeoWGS841Click
        end
        object GDALwarpSentinel11: TMenuItem
          Caption = 'GDAL warp Sentinel-1'
          OnClick = GDALwarpSentinel11Click
        end
        object GDALcreatemultibandTIFF1: TMenuItem
          Caption = 'GDAL create multiband TIFF '
          OnClick = GDALcreatemultibandTIFF1Click
        end
        object ConvertDEMstoGeotiffGDAL1: TMenuItem
          Caption = 'Convert DEMs to Geotiff (GDAL)'
          OnClick = ConvertDEMstoGeotiffGDAL1Click
        end
        object N1: TMenuItem
          Caption = '-'
        end
      end
      object GDALOGRvectors1: TMenuItem
        Caption = 'GDAL/OGR vectors'
        object GDALGeodatabasetoshapefile1: TMenuItem
          Caption = 'GDAL Geodatabase to shapefile'
          OnClick = GDALGeodatabasetoshapefile1Click
        end
        object GDALreprojectshapefile1: TMenuItem
          Caption = 'GDAL reproject shapefile to lat/long'
          OnClick = GDALreprojectshapefile1Click
        end
        object OGRmergeshapefiles1: TMenuItem
          Caption = 'OGR merge shapefiles'
          OnClick = OGRmergeshapefiles1Click
        end
        object OGRDBFtoSQLite1: TMenuItem
          Caption = 'OGR DBF to SQLite'
          OnClick = OGRDBFtoSQLite1Click
        end
        object OGRDXFtoshapefile1: TMenuItem
          Caption = 'OGR DXF/GPX/OSM/GKPG/GeoJSON to shapefile'
          OnClick = OGRDXFtoshapefile1Click
        end
        object OGRshapefilestoGKPG1: TMenuItem
          Caption = 'OGR shapefiles to GKPG'
          OnClick = OGRshapefilestoGKPG1Click
        end
      end
      object XYZshifttoEGM2008withVDATUMresults1: TMenuItem
        Caption = 'XYZ shift to EGM2008 with VDATUM results'
        OnClick = XYZshifttoEGM2008withVDATUMresults1Click
      end
      object VerticaldatumshiftoverwriteDEMgrid1: TMenuItem
        Caption = 'Vertical datum shift, overwrite DEM/grid'
        OnClick = VerticaldatumshiftoverwriteDEMgrid1Click
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object DEMformats1: TMenuItem
        Caption = 'DEM format conversions'
        object IMGdirectorytoMDDEM1: TMenuItem
          Caption = 'IMG files'
          OnClick = IMGdirectorytoMDDEM1Click
        end
        object BILdirectorytoMDDEM1: TMenuItem
          Caption = 'BIL files'
          OnClick = BILdirectorytoMDDEM1Click
        end
        object IFfiles1: TMenuItem
          Caption = 'TIF files'
          OnClick = IFfiles1Click
        end
        object FLTfiles1: TMenuItem
          Caption = 'FLT files'
          OnClick = FLTfiles1Click
        end
        object ADFdirectory1: TMenuItem
          Caption = 'ADF directories'
          OnClick = ADFdirectory1Click
        end
        object ASCfiles1: TMenuItem
          Caption = 'ASC files'
          OnClick = ASCfiles1Click
        end
        object XTfiles1: TMenuItem
          Caption = 'TXT files'
          OnClick = XTfiles1Click
        end
        object ConverttoGeotiff1: TMenuItem
          Caption = 'Convert DEMs to Geotiff (GDAL)'
          OnClick = ConverttoGeotiff1Click
        end
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object LASfilesMD1: TMenuItem
        Caption = 'LAS, MICRODEM native'
        object DefinedtoUTM1: TMenuItem
          Caption = 'Defined to UTM'
          OnClick = DefinedtoUTM1Click
        end
        object ChangeUTMzone1: TMenuItem
          Caption = 'Change UTM zone'
          OnClick = ChangeUTMzone1Click
        end
        object IrishgridtoUTM1: TMenuItem
          Caption = 'Irish grid to UTM'
          OnClick = IrishgridtoUTM1Click
        end
        object UKOStoUTM1: TMenuItem
          Caption = 'UK OS to UTM'
          OnClick = UKOStoUTM1Click
        end
        object zshift1: TMenuItem
          Caption = 'z shift, feet to meters'
          OnClick = zshift1Click
        end
        object LAStoXYZ1: TMenuItem
          Caption = 'LAS to....'
          object XYZ1: TMenuItem
            Caption = 'XYZ'
            OnClick = XYZ1Click
          end
          object XYZRGB1: TMenuItem
            Caption = 'XYZ, RGB'
            OnClick = XYZRGB1Click
          end
          object XYZClassIntensity1: TMenuItem
            Caption = 'XYZ, Class, Intensity'
            OnClick = XYZClassIntensity1Click
          end
          object GeoJSONP1: TMenuItem
            Caption = 'GeoJSON-P'
            OnClick = GeoJSONP1Click
          end
        end
      end
      object LAStools1: TMenuItem
        Caption = 'LAStools'
        object Assignprojection1: TMenuItem
          Caption = 'Assign projection by EPSG'
          OnClick = Assignprojection1Click
        end
        object AssignprojectionUTM1: TMenuItem
          Caption = 'Assign projection by UTM zone'
          OnClick = AssignprojectionUTM1Click
        end
        object AssignbyEPSGandreprojecttoUTM1: TMenuItem
          Caption = 'Assign by EPSG and reproject to UTM'
          OnClick = AssignbyEPSGandreprojecttoUTM1Click
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object LASGeotoUTM1: TMenuItem
          Caption = 'Reproject, Geo to UTM'
          OnClick = LASGeotoUTM1Click
        end
        object ToUTM1: TMenuItem
          Caption = 'Reproject, Specfied to UTM'
          OnClick = ToUTM1Click
        end
        object ReprojectSpecifiedtoGeo1: TMenuItem
          Caption = 'Reproject, Specified to Geo'
          OnClick = ReprojectSpecifiedtoGeo1Click
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object LASionfo1: TMenuItem
          Caption = 'LASinfo'
          OnClick = LASionfo1Click
        end
        object XYZItoLAS1: TMenuItem
          Caption = 'XYZI to LAS'
          OnClick = XYZItoLAS1Click
        end
        object DEMtoLAZ1: TMenuItem
          Caption = 'DEM to LAZ'
          OnClick = DEMtoLAZ1Click
        end
      end
      object lilbLAS1: TMenuItem
        Caption = 'lasLIB'
        object lasLIB1: TMenuItem
          Caption = 'lasLIB UK  OS lidar to UTM'
          OnClick = lasLIB1Click
        end
        object laslibspecifiedEPSGtoUTM1: TMenuItem
          Caption = 'lasLIB specified EPSG to UTM'
          OnClick = laslibspecifiedEPSGtoUTM1Click
        end
      end
      object LASlidar1: TMenuItem
        Caption = '-'
      end
      object PrepOSMfiles1: TMenuItem
        Caption = 'Prep OSM/Natural Earth/GADM files'
        OnClick = PrepOSMfiles1Click
      end
    end
    object Create1: TMenuItem
      Caption = '&Create/Index'
      object Integrateddatabase1: TMenuItem
        Caption = 'Map library'
        object Pickmaplibrary1: TMenuItem
          Caption = 'Pick map library'
          OnClick = Pickmaplibrary1Click
        end
        object Createexternaldrive1: TMenuItem
          Caption = 'Create/update map library'
          OnClick = Createexternaldrive1Click
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object Adjustseriescolorusage1: TMenuItem
          Caption = 'Adjust series color/usage'
          OnClick = Adjustseriescolorusage1Click
        end
      end
      object TIGERindex1: TMenuItem
        Caption = 'TIGER index update'
        OnClick = TIGERindex1Click
      end
      object Verticaldatums1: TMenuItem
        Caption = 'Vertical datums'
        OnClick = Verticaldatums1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Header2: TMenuItem
        Caption = 'DEM/grid Header'
        OnClick = Header2Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object ASCIIduplicates1: TMenuItem
        Caption = 'ASCII duplicates'
        OnClick = ASCIIduplicates1Click
      end
      object ASCIIsort1: TMenuItem
        Caption = 'ASCII sort'
        OnClick = ASCIIsort1Click
      end
      object ASCIIsortremoveduplicates1: TMenuItem
        Caption = 'ASCII sort && remove duplicates'
        OnClick = ASCIIsortremoveduplicates1Click
      end
      object ASCIIremovelinesinsecondfile1: TMenuItem
        Caption = 'ASCII remove lines in second file'
        OnClick = ASCIIremovelinesinsecondfile1Click
      end
      object ASCIItrimcolumns1: TMenuItem
        Caption = 'ASCII trim columns'
        OnClick = ASCIItrimcolumns1Click
      end
      object ASCIIreverseorder1: TMenuItem
        Caption = 'ASCII reverse order'
        OnClick = ASCIIreverseorder1Click
      end
      object ASCIICRLF1: TMenuItem
        Caption = 'ASCII CR+LF'
        OnClick = ASCIICRLF1Click
      end
      object ASCIIthin1: TMenuItem
        Caption = 'ASCII thin'
        OnClick = ASCIIthin1Click
      end
      object ASCIIrandomize1: TMenuItem
        Caption = 'ASCII randomize'
        OnClick = ASCIIrandomize1Click
      end
      object ASCIIremove1: TMenuItem
        Caption = 'ASCII remove'
        object ASCIIremovetabs1: TMenuItem
          Caption = 'ASCII remove tabs'
          OnClick = ASCIIremovetabs1Click
        end
        object ASCIIremovecommas1: TMenuItem
          Caption = 'ASCII remove commas'
          OnClick = ASCIIremovecommas1Click
        end
        object ASCIIremovequotes1: TMenuItem
          Caption = 'ASCII remove quotes'
          OnClick = ASCIIremovequotes1Click
        end
        object ASCII01: TMenuItem
          Caption = 'ASCII remove #0'
          OnClick = ASCII01Click
        end
        object ASCIIremoveblanklines1: TMenuItem
          Caption = 'ASCII remove blank lines'
          OnClick = ASCIIremoveblanklines1Click
        end
        object ASCIIremoveafterdelimiter1: TMenuItem
          Caption = 'ASCII remove after delimiter'
          OnClick = ASCIIremoveafterdelimiter1Click
        end
        object ASCIIremoveleadingcharacter1: TMenuItem
          Caption = 'ASCII remove leading character'
          OnClick = ASCIIremoveleadingcharacter1Click
        end
        object ASCIItrimlines1: TMenuItem
          Caption = 'ASCII trim lines'
          OnClick = ASCIItrimlines1Click
        end
        object ASCIIremovelineswithsubstring1: TMenuItem
          Caption = 'ASCII remove lines with substring'
          OnClick = ASCIIremovelineswithsubstring1Click
        end
        object ASCIIremovelineswithoutsubstring1: TMenuItem
          Caption = 'ASCII remove lines without substring'
        end
      end
      object ASCIIheader1: TMenuItem
        Caption = 'ASCII header'
        object ASCIIinsertheaderline1: TMenuItem
          Caption = 'ASCII insert header line'
          OnClick = ASCIIinsertheaderline1Click
        end
        object ASCIIreplaceheaderline1: TMenuItem
          Caption = 'ASCII replace header line'
          OnClick = ASCIIreplaceheaderline1Click
        end
      end
      object ASCIIinsertlinenumbers1: TMenuItem
        Caption = 'ASCII insert line numbers'
        OnClick = ASCIIinsertlinenumbers1Click
      end
      object ASCIIfindlineswithsubstring1: TMenuItem
        Caption = 'ASCII find lines with substring'
        OnClick = ASCIIfindlineswithsubstring1Click
      end
      object ASCIImergefiles1: TMenuItem
        Caption = 'ASCII merge files'
        OnClick = ASCIImergefiles1Click
      end
      object ASCIIsplitfile1: TMenuItem
        Caption = 'ASCII split file'
        object Bynumberoffiles1: TMenuItem
          Caption = 'By number of files'
          OnClick = Bynumberoffiles1Click
        end
        object Bynumberoflines1: TMenuItem
          Caption = 'By number of lines'
          OnClick = Bynumberoflines1Click
        end
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object CSVputcommasaroundfields1: TMenuItem
        Caption = 'CSV put  quotes around fields'
        OnClick = CSVputcommasaroundfields1Click
      end
      object CSVmergefiles1: TMenuItem
        Caption = 'CSV merge files'
        OnClick = CSVmergefiles1Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object HTMLcleanup1: TMenuItem
        Caption = 'HTML cleanup'
        OnClick = HTMLcleanup1Click
      end
      object HTMLinventory1: TMenuItem
        Caption = 'HTML inventory'
        OnClick = HTMLinventory1Click
      end
      object HTMLextractlinks1: TMenuItem
        Caption = 'HTML extract links'
        OnClick = HTMLextractlinks1Click
      end
      object HTMLfilelinks1: TMenuItem
        Caption = 'HTML create file links'
        OnClick = HTMLfilelinks1Click
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object Fileextensions1: TMenuItem
        Caption = 'File extensions'
        object Changefileextensions1: TMenuItem
          Caption = 'Change file extensions'
          OnClick = Changefileextensions1Click
        end
        object Addfileextensions1: TMenuItem
          Caption = 'Add file extensions'
          OnClick = Addfileextensions1Click
        end
      end
      object Emailcleanup1: TMenuItem
        Caption = 'Email cleanup'
        OnClick = Emailcleanup1Click
      end
      object ANSIfilecleanup1: TMenuItem
        Caption = 'ANSI file cleanup'
        OnClick = ANSIfilecleanup1Click
      end
      object Filemodificationdates1: TMenuItem
        Caption = 'File modification dates'
        OnClick = Filemodificationdates1Click
      end
    end
    object Compressuncompress1: TMenuItem
      Caption = 'Compress/uncompress'
      OnClick = Compressuncompress1Click
    end
    object Download1: TMenuItem
      Caption = 'Download'
      object OpenTopography1: TMenuItem
        Caption = 'List of URLs'
        OnClick = OpenTopography1Click
      end
      object Chromelist1: TMenuItem
        Caption = 'Chrome list'
        OnClick = Chromelist1Click
      end
      object IElist1: TMenuItem
        Caption = 'IE list'
        OnClick = IElist1Click
      end
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      object Shapefile2: TMenuItem
        Caption = 'Shapefile'
        OnClick = Shapefile2Click
      end
      object KMLfiles1: TMenuItem
        Caption = 'KML files'
        OnClick = KMLfiles1Click
      end
      object Viewsheds1: TMenuItem
        Caption = 'Viewsheds'
        OnClick = Viewsheds1Click
      end
    end
    object Experimental1: TMenuItem
      Caption = 'Experimental'
      Visible = False
      object ranslatecoordsASCIIfile1: TMenuItem
        Caption = 'Translate coords, ASCII file'
        OnClick = ranslatecoordsASCIIfile1Click
      end
      object CloudCompare2: TMenuItem
        Caption = 'Cloud Compare compute roughness and curvature'
      end
      object InsertroughnessandcurvatureinDBF1: TMenuItem
        Caption = 'Insert roughness and curvature in DBF'
      end
      object BatchCSFdemo1: TMenuItem
        Caption = 'Batch CSFdemo for ground points'
      end
      object Addgroundclassifiedpoints1: TMenuItem
        Caption = 'Add ground classified points to DBF'
        OnClick = Addgroundclassifiedpoints1Click
      end
      object Histogramsbyclass1: TMenuItem
        Caption = 'Histograms by class'
      end
      object Addcolors1: TMenuItem
        Caption = 'Add colors'
        OnClick = Addcolors1Click
      end
      object Coloredmaps1: TMenuItem
        Caption = 'Colored maps'
      end
      object Statsbyclass1: TMenuItem
        Caption = 'Stats by class'
      end
      object LoadDBintoslicer1: TMenuItem
        Caption = 'Load DB into slicer'
      end
      object Classificationaccuracy1: TMenuItem
        Caption = 'Classification accuracy'
      end
      object Quickclassification1: TMenuItem
        Caption = 'Quick classification'
      end
      object Extractclassification1: TMenuItem
        Caption = 'Extract classification'
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      OnClick = Help1Click
    end
  end
end
