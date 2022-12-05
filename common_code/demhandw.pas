unit Demhandw;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$Define MST_Sidescan}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordGMTConvert}
      //{$Define RecordTDemHandFormFormClose}
      //{$Define RecordDuckProblems}
      //{$Define RecordSOESTtides}
      //{$Define RecordHandlingProblems}
      //{$Define RecordReformat}
      //{$Define RecordImportProblems}
      //{$Define RecordSatProblems}
      //{$Define RecordMergeProblems}
      //{$Define RecordClosingProblems}
      //{$Define RecordShapeFileContents}
      //{$Define RecordGAZProblems}
      //{$Define RecordGDAL}
   {$EndIf}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end needed for inline of the core DB functions


  SysUtils, Windows, Classes, Graphics, Forms, Dialogs, Menus, Grids,  StrUtils,
  System.Math,System.UITypes,
  Vcl.ComCtrls, Vcl.Controls, Vcl.StdCtrls,
  BaseGraf,DEMdefs,Petmar_types,PETMAR;

type
  TDemHandForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Import1: TMenuItem;
    Export1: TMenuItem;
    Close1: TMenuItem;
    Help1: TMenuItem;
    Subset1: TMenuItem;
    Create1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    Header2: TMenuItem;
    ASCIIXYZ1: TMenuItem;
    Satellite1: TMenuItem;
    BMP1: TMenuItem;
    Resample1: TMenuItem;
    ASCIIduplicates1: TMenuItem;
    ASCIIsort1: TMenuItem;
    Memo1: TMemo;
    BMPRGBcolor1: TMenuItem;
    ASCIItrimcolumns1: TMenuItem;
    ASCIICRLF1: TMenuItem;
    ASCIIremovetabs1: TMenuItem;
    ASCIIremovecommas1: TMenuItem;
    N6: TMenuItem;
    Shapefile1: TMenuItem;
    N7: TMenuItem;
    Shapefile2: TMenuItem;
    ASCIIsortremoveduplicates1: TMenuItem;
    Datumshift1: TMenuItem;
    TIGERindex1: TMenuItem;
    Specifyshift1: TMenuItem;
    AddLineEndPoints: TMenuItem;
    Database2: TMenuItem;
    USGSgazetteerdatabase1: TMenuItem;
    NIMAgazetteerdatabase1: TMenuItem;
    ASCIIremoveblanklines1: TMenuItem;
    IGERredistricting1: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    BinaryrawDEM1: TMenuItem;
    Createheader1: TMenuItem;
    DEMS2: TMenuItem;
    ASCII1: TMenuItem;
    StatusBar1: TStatusBar;
    ASCIIreverseorder1: TMenuItem;
    BinaryrawDEM32bit1: TMenuItem;
    ISOGravity1: TMenuItem;
    Repairheaders1: TMenuItem;
    ASCIIsplitfile1: TMenuItem;
    Cardfileimport1: TMenuItem;
    ICOADSLMRF1: TMenuItem;
    Integrateddatabase1: TMenuItem;
    ArbitrarytoLatLong1: TMenuItem;
    N4: TMenuItem;
    Adjustseriescolorusage1: TMenuItem;
    Monthlytimeseries1: TMenuItem;
    ExtractXYZfromDXFfile1: TMenuItem;
    RenameSF3fields1: TMenuItem;
    KMLfiles1: TMenuItem;
    ExtractXYZfromVRML1: TMenuItem;
    Ducknearshoresurveys1: TMenuItem;
    SF3Censusdata1: TMenuItem;
    HTMLcleanup1: TMenuItem;
    SINfiletoshapefile1: TMenuItem;
    ADFdirectory1: TMenuItem;
    Rename1: TMenuItem;
    ASCII01: TMenuItem;
    ASCIIremovequotes1: TMenuItem;
    BILdirectorytoMDDEM1: TMenuItem;
    Download1: TMenuItem;
    OpenTopography1: TMenuItem;
    AddXYZtoDBFfile1: TMenuItem;
    ASCIIrandomize1: TMenuItem;
    ASCIIinsertlinenumbers1: TMenuItem;
    Bynumberoffiles1: TMenuItem;
    Bynumberoflines1: TMenuItem;
    ASCIIXYZto3Dshape1: TMenuItem;
    ASCIIfindlineswithsubstring1: TMenuItem;
    ASCIIremoveafterdelimiter1: TMenuItem;
    ASCIIinsertheaderline1: TMenuItem;
    IMGdirectorytoMDDEM1: TMenuItem;
    ASCIIremove1: TMenuItem;
    ASCIImergefiles1: TMenuItem;
    ShiftXYZcoordinates1: TMenuItem;
    NCEPNCARReanalysiswinds1: TMenuItem;
    Addfilenameasfield1: TMenuItem;
    N5: TMenuItem;
    RepairSHPandSHXboundingboxes1: TMenuItem;
    Emailcleanup1: TMenuItem;
    Viewsheds1: TMenuItem;
    Changefileextensions1: TMenuItem;
    Addfileextensions1: TMenuItem;
    Fileextensions1: TMenuItem;
    Compressuncompress1: TMenuItem;
    ANSIfilecleanup1: TMenuItem;
    HTMLinventory1: TMenuItem;
    ASCIIremoveleadingcharacter1: TMenuItem;
    XYADBfile1: TMenuItem;
    Untilegeotiffs1: TMenuItem;
    GDALreprojectshapefile1: TMenuItem;
    LAStoXYZ1: TMenuItem;
    DEMformats1: TMenuItem;
    XYZRGB1: TMenuItem;
    XYZClassIntensity1: TMenuItem;
    GeoJSONP1: TMenuItem;
    DEMs1: TMenuItem;
    LASGeotoUTM1: TMenuItem;
    Maindatabase1: TMenuItem;
    Separatefile1: TMenuItem;
    GDALreprojectimagetoUTMNAD831: TMenuItem;
    Cloudcompare1: TMenuItem;
    RANSAC1: TMenuItem;
    GDALGeodatabasetoshapefile1: TMenuItem;
    CreateDBFfile1: TMenuItem;
    Icesat1: TMenuItem;
    OGRDBFtoSQLite1: TMenuItem;
    LAStools1: TMenuItem;
    ToUTM1: TMenuItem;
    LASionfo1: TMenuItem;
    GDALbandextraction1: TMenuItem;
    GDAL1: TMenuItem;
    GDALcreatemultibandTIFF1: TMenuItem;
    ConverttoGeotiff1: TMenuItem;
    OGRDXFtoshapefile1: TMenuItem;
    ASCIItrimlines1: TMenuItem;
    ENVIHDRIMG1: TMenuItem;
    XYZ1: TMenuItem;
    Ducknearshoresurveys2: TMenuItem;
    LineKML1: TMenuItem;
    XYZItoLAS1: TMenuItem;
    MaskDEMs1: TMenuItem;
    Assignprojection1: TMenuItem;
    MrSIDtotiff1: TMenuItem;
    Resizeimages1: TMenuItem;
    ASCIIremovelineswithsubstring1: TMenuItem;
    GDALwarpGeotifftoUTMNAD831: TMenuItem;
    GDALwarpGeotifftoGeoWGS841: TMenuItem;
    N1: TMenuItem;
    HTMLfilelinks1: TMenuItem;
    ASCfiles1: TMenuItem;
    PrepOSMfiles1: TMenuItem;
    CreateSHXfilesforDBFfiles1: TMenuItem;
    HTMLextractlinks1: TMenuItem;
    AssignprojectionUTM1: TMenuItem;
    ReprojectSpecifiedtoGeo1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    lasLIB1: TMenuItem;
    GDALassignandwarptoUTM1: TMenuItem;
    laslibspecifiedEPSGtoUTM1: TMenuItem;
    LASfilesMD1: TMenuItem;
    IrishgridtoUTM1: TMenuItem;
    UKOStoUTM1: TMenuItem;
    DefinedtoUTM1: TMenuItem;
    ChangeUTMzone1: TMenuItem;
    GDALmergeGeotiff1: TMenuItem;
    zshift1: TMenuItem;
    Filemodificationdates1: TMenuItem;
    LASlidar1: TMenuItem;
    lilbLAS1: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    OGRmergeshapefiles1: TMenuItem;
    XTfiles1: TMenuItem;
    SOESTtidetimeseries1: TMenuItem;
    ConvertDEMstoGeotiffGDAL1: TMenuItem;
    N11: TMenuItem;
    CSVputcommasaroundfields1: TMenuItem;
    GDALOGRvectors1: TMenuItem;
    LASfilesbysize1: TMenuItem;
    IFfiles1: TMenuItem;
    AssignbyEPSGandreprojecttoUTM1: TMenuItem;
    Experimental1: TMenuItem;
    N10: TMenuItem;
    ASCIIreplaceheaderline1: TMenuItem;
    ASCIIheader1: TMenuItem;
    Histogramsbyclass1: TMenuItem;
    Addcolors1: TMenuItem;
    Addgroundclassifiedpoints1: TMenuItem;
    Plateboundaryfile1: TMenuItem;
    ranslatecoordsASCIIfile1: TMenuItem;
    N17: TMenuItem;
    ASCIIremovelinesinsecondfile1: TMenuItem;
    Chromelist1: TMenuItem;
    IElist1: TMenuItem;
    Createexternaldrive1: TMenuItem;
    Pickmaplibrary1: TMenuItem;
    FLTfiles1: TMenuItem;
    xview2json1: TMenuItem;
    Verticaldatums1: TMenuItem;
    GDALwarpGeotifftoadjacentUTMzone1: TMenuItem;
    DEMtoLAZ1: TMenuItem;
    GDALwarpSentinel11: TMenuItem;
    CSVmergefiles1: TMenuItem;
    OGRshapefilestoGKPG1: TMenuItem;
    procedure ASCIIremovequotes1Click(Sender: TObject);
    procedure ASCII01Click(Sender: TObject);
    procedure HTMLcleanup1Click(Sender: TObject);
    procedure SF3Censusdata1Click(Sender: TObject);
    procedure Ducknearshoresurveys1Click(Sender: TObject);
    procedure KMLfiles1Click(Sender: TObject);
    procedure RenameSF3fields1Click(Sender: TObject);
    procedure ExtractXYZfromDXFfile1Click(Sender: TObject);
    procedure Adjustseriescolorusage1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Header2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ASCIIduplicates1Click(Sender: TObject);
    procedure ASCIIsort1Click(Sender: TObject);
    procedure ASCIItrimcolumns1Click(Sender: TObject);
    procedure ASCIICRLF1Click(Sender: TObject);
    procedure ASCIIthin1Click(Sender: TObject);
    procedure ASCIIremovetabs1Click(Sender: TObject);
    procedure ASCIIremovecommas1Click(Sender: TObject);
    procedure Conictolatlong1Click(Sender: TObject);
    procedure Shapefile2Click(Sender: TObject);
    procedure ASCIIsortremoveduplicates1Click(Sender: TObject);
    procedure Datumshift1Click(Sender: TObject);
    procedure TIGERindex1Click(Sender: TObject);
    procedure Specifyshift1Click(Sender: TObject);
    procedure AddlengthfieldtoDBFfile1Click(Sender: TObject);
    procedure AddlatlongfieldstoDBFfile1Click(Sender: TObject);
    procedure AddLineEndPointsClick(Sender: TObject);
    procedure USGSgazetteerdatabase1Click(Sender: TObject);
    procedure NIMAgazetteerdatabase1Click(Sender: TObject);
    procedure ASCIIremoveblanklines1Click(Sender: TObject);
    procedure IGERredistricting1Click(Sender: TObject);
    procedure ASCIIreverseorder1Click(Sender: TObject);
    procedure ISOGravity1Click(Sender: TObject);
    procedure Repairheaders1Click(Sender: TObject);
    procedure Cardfileimport1Click(Sender: TObject);
    procedure ICOADSLMRF1Click(Sender: TObject);
    procedure ArbitrarytoLatLong1Click(Sender: TObject);
    procedure Monthlytimeseries1Click(Sender: TObject);
    procedure SINfiletoshapefile1Click(Sender: TObject);
    procedure BatchRenameDEMfiles(Sender: TObject);
    procedure ADFdirectory1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure BILdirectorytoMDDEM1Click(Sender: TObject);
    procedure OpenTopography1Click(Sender: TObject);
    procedure AddXYZtoDBFfile1Click(Sender: TObject);
    procedure ASCIIrandomize1Click(Sender: TObject);
    procedure ASCIIinsertlinenumbers1Click(Sender: TObject);
    procedure Bynumberoflines1Click(Sender: TObject);
    procedure Bynumberoffiles1Click(Sender: TObject);
    procedure ASCIIfindlineswithsubstring1Click(Sender: TObject);
    procedure ASCIIremoveafterdelimiter1Click(Sender: TObject);
    procedure ASCIIinsertheaderline1Click(Sender: TObject);
    procedure IMGdirectorytoMDDEM1Click(Sender: TObject);
    procedure ASCIImergefiles1Click(Sender: TObject);
    procedure ShiftXYZcoordinates1Click(Sender: TObject);
    procedure NCEPNCARReanalysiswinds1Click(Sender: TObject);
    procedure Addfilenameasfield1Click(Sender: TObject);
    procedure RepairSHPandSHXboundingboxes1Click(Sender: TObject);
    procedure Emailcleanup1Click(Sender: TObject);
    procedure Viewsheds1Click(Sender: TObject);
    procedure Changefileextensions1Click(Sender: TObject);
    procedure Addfileextensions1Click(Sender: TObject);
    procedure ANSIfilecleanup1Click(Sender: TObject);
    procedure Compressuncompress1Click(Sender: TObject);
    procedure HTMLinventory1Click(Sender: TObject);
    procedure ASCIIremoveleadingcharacter1Click(Sender: TObject);
    procedure XYADBfile1Click(Sender: TObject);
    procedure Untilegeotiffs1Click(Sender: TObject);
    procedure GDALreprojectshapefile1Click(Sender: TObject);
    procedure XYZClassIntensity1Click(Sender: TObject);
    procedure XYZRGB1Click(Sender: TObject);
    procedure GeoJSONP1Click(Sender: TObject);
    procedure DEMs1Click(Sender: TObject);
    procedure LASGeotoUTM1Click(Sender: TObject);
    procedure Maindatabase1Click(Sender: TObject);
    procedure Separatefile1Click(Sender: TObject);
    procedure GDALreprojectimagetoUTMNAD831Click(Sender: TObject);
    procedure RANSAC1Click(Sender: TObject);
    procedure GDALGeodatabasetoshapefile1Click(Sender: TObject);
    procedure CreateDBFfile1Click(Sender: TObject);
    procedure Icesat1Click(Sender: TObject);
    procedure OGRDBFtoSQLite1Click(Sender: TObject);
    procedure ToUTM1Click(Sender: TObject);
    procedure LASionfo1Click(Sender: TObject);
    procedure GDALbandextraction1Click(Sender: TObject);
    procedure GDALcreatemultibandTIFF1Click(Sender: TObject);
    procedure ConverttoGeotiff1Click(Sender: TObject);
    procedure OGRDXFtoshapefile1Click(Sender: TObject);
    procedure ASCIItrimlines1Click(Sender: TObject);
    procedure ENVIHDRIMG1Click(Sender: TObject);
    procedure XYZ1Click(Sender: TObject);
    procedure LineKML1Click(Sender: TObject);
    procedure XYZItoLAS1Click(Sender: TObject);
    procedure MaskDEMs1Click(Sender: TObject);
    procedure Assignprojection1Click(Sender: TObject);
    procedure MrSIDtotiff1Click(Sender: TObject);
    procedure Resizeimages1Click(Sender: TObject);
    procedure ASCIIremovelineswithsubstring1Click(Sender: TObject);
    procedure GDALwarpGeotifftoUTMNAD831Click(Sender: TObject);
    procedure GDALwarpGeotifftoGeoWGS841Click(Sender: TObject);
    procedure HTMLfilelinks1Click(Sender: TObject);
    procedure ASCfiles1Click(Sender: TObject);
    procedure PrepOSMfiles1Click(Sender: TObject);
    procedure CreateSHXfilesforDBFfiles1Click(Sender: TObject);
    procedure HTMLextractlinks1Click(Sender: TObject);
    procedure AssignprojectionUTM1Click(Sender: TObject);
    procedure ReprojectSpecifiedtoGeo1Click(Sender: TObject);
    procedure lasLIB1Click(Sender: TObject);
    procedure GDALassignandwarptoUTM1Click(Sender: TObject);
    procedure laslibspecifiedEPSGtoUTM1Click(Sender: TObject);
    procedure IrishgridtoUTM1Click(Sender: TObject);
    procedure UKOStoUTM1Click(Sender: TObject);
    procedure DefinedtoUTM1Click(Sender: TObject);
    procedure ChangeUTMzone1Click(Sender: TObject);
    procedure GDALmergeGeotiff1Click(Sender: TObject);
    procedure zshift1Click(Sender: TObject);
    procedure Filemodificationdates1Click(Sender: TObject);
    procedure OGRmergeshapefiles1Click(Sender: TObject);
    procedure XTfiles1Click(Sender: TObject);
    procedure SOESTtidetimeseries1Click(Sender: TObject);
    procedure ConvertDEMstoGeotiffGDAL1Click(Sender: TObject);
    procedure CSVputcommasaroundfields1Click(Sender: TObject);
    procedure LASfilesbysize1Click(Sender: TObject);
    procedure IFfiles1Click(Sender: TObject);
    procedure AssignbyEPSGandreprojecttoUTM1Click(Sender: TObject);
    procedure ASCIIreplaceheaderline1Click(Sender: TObject);
    procedure Addcolors1Click(Sender: TObject);
    procedure Addgroundclassifiedpoints1Click(Sender: TObject);
    procedure Plateboundaryfile1Click(Sender: TObject);
    procedure ranslatecoordsASCIIfile1Click(Sender: TObject);
    procedure ASCIIremovelinesinsecondfile1Click(Sender: TObject);
    procedure Chromelist1Click(Sender: TObject);
    procedure IElist1Click(Sender: TObject);
    procedure Pickmaplibrary1Click(Sender: TObject);
    procedure Createexternaldrive1Click(Sender: TObject);
    procedure FLTfiles1Click(Sender: TObject);
    procedure xview2json1Click(Sender: TObject);
    procedure Verticaldatums1Click(Sender: TObject);
    procedure GDALwarpGeotifftoadjacentUTMzone1Click(Sender: TObject);
    procedure DEMtoLAZ1Click(Sender: TObject);
    procedure GDALwarpSentinel11Click(Sender: TObject);
    procedure CSVmergefiles1Click(Sender: TObject);
    procedure OGRshapefilestoGKPG1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     ActiveDEM : integer;
     SaveName  : PathStr;
  end;


var
   DemHandForm  : TDemHandForm;


function MakeGraphFromSOESTtides(fName : PathStr; var Lat,Long : float64; var Year1,Year2 : integer; var StationName : shortString) :  TThisBaseGraph;
procedure JSONtoShapefile(fname : pathStr);


implementation

{$R *.DFM}

uses
   {$IfDef ExVectorOverlay}
   {$Else}
      DEMEditW,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      DEMDataBase,
      DEMESRIShapeFile,
      DataBaseCreate,
   {$EndIf}

   {$IfDef ExTIGER}
   {$Else}
      DEMTiger,
   {$EndIf}

   {$IfDef ExSidescan}
   {$Else}
      MST_format,
   {$EndIf}

   {$IfDef ExAdvancedImportExport}
   {$Else}
      DEM_TIN,
      DEMXYZExport,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEROS,
      Geotiff,
   {$EndIf}

   {$IfDef MSTSidescan}
      MST_format,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      KoppenGr,
   {$EndIf}

   DEMXYZIm,
   BaseMap,

   {$IfDef ExPointCloud}
   {$Else}
      LAS_Lidar,
      Icesat_filter_form,
   {$EndIf}

   {$IfDef ExKML}
   {$Else}
      kml_opts,
   {$EndIf}

   {$IfDef ExTools}
   {$Else}
       compress_form,
       MD_use_tools,
   {$EndIf}

   {$IfDef ExMrSID}
   {$Else}
      MrSIDImagery,
   {$EndIf}

   {$IfDef ExMultigrid}
   {$Else}
      MultiGrid,
   {$EndIf}

   DEMMapDraw,
   DEMCoord,DEMMapF,
   DEM_Manager,
   PETMath,PETDBUtils,PetImage,
   GetLatLn,
   csv_export,
   Thread_timers,
   DEMDef_routines,
   DEM_indexes,
   gdal_tools,
   DEMstat,
   Make_Tables,
   DataBaseAddRec,
   KML_creator,
   Nevadia_Main;


const
   NISFStr = 'Not implemented for shape files';
type
   tHeaderMode = (hmInsert,hmReplace);
var
   LastCompressedFile,XYZName : PathStr;



{$IfDef ExAdvancedGIS}
{$Else}
   procedure ImportGazetteer(NGAversion : boolean);
   var
      k  : integer;
      DefaultFilter : byte;
      FilesWanted : tstringlist;
      GazWant :tCSVImport;
      Which : shortstring;
      fName,gName : PathStr;
   begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(GazetteerDir);
      DefaultFilter := 1;
      if NGAversion then begin
         GazWant := csvNGAgaz;
         Which := 'NGA';
      end
      else begin
         GazWant := csvUSGSgaz;
         Which := 'USGS';
      end;
      {$If Defined(RecordGAZProblems) or Defined(RecordProblems)} WriteLineToDebugFile('Gaz import ' + Which); {$EndIf}

      if GetMultipleFiles(Which +' gazetteer',Which + ' gazetteer|*.txt|Any file|*.*',FilesWanted,DefaultFilter) then begin
         {$IfDef RecordGAZProblems} WriteLineToDebugFile('Selected num files=' + IntToStr(FilesWanted.Count)); {$EndIf}
         for k := 0 to pred(FilesWanted.Count) do begin
            fName := UpperCase(FilesWanted.Strings[k]);
            {$IfDef RecordGAZProblems} WriteLineToDebugFile('file=' + fName); {$EndIf}
            gName := ChangeFileExt(fName,DefaultDBExt);
            fName := DoCSVFileImport(fName,GazWant);
         end;
      end;
      FilesWanted.Free;
      {$IfDef RecordGAZProblems} WriteLineToDebugFile('Gaz import over'); {$EndIf}
   end;
{$EndIf}



procedure DataFileHeader(aMode : tHeaderMode);
var
   FilesWanted,datafile : TStringList;
   DefaultFilter : byte;
   fName : PathStr;
   i : integer;
begin
   FilesWanted := TStringList.Create;
   DefaultFilter := 1;
   if GetMultipleFiles('ASCII files to insert header','*.*',FilesWanted,DefaultFilter) then begin
       Petmar.GetString('Header',MDDef.FileHeader,false,ReasonableTextChars);
       StartProgress('Insert header');
       for i := 0 to pred(FilesWanted.Count) do begin
          if (i mod 5 = 0) then UpdateProgressBar(i/FilesWanted.Count);
          datafile := tStringList.Create;
          fName := FilesWanted.Strings[i];
          datafile.LoadFromFile(fName);
          if aMode = hmInsert then begin
              if (datafile.Strings[0] <> MDDef.FileHeader) then begin
                 datafile.Insert(0,MDDef.FileHeader);
              end;
          end
          else begin
             datafile.Strings[0] := MDDef.FileHeader;
          end;
          datafile.SaveToFile(fName);
          Datafile.free;
       end;
       FilesWanted.Free;
       EndProgress;
   end;
end;


function MakeGraphFromSOESTtides(fName : PathStr; var Lat,Long : float64; var Year1,Year2 : integer; var StationName : shortString) :  TThisBaseGraph;
var
   i,j,Year,err,day,NumDays : integer;
   Ht : float64;
   MonthlyData : boolean;
   TStr,MenuStr : shortstring;
   rfile : file;
   v  : tGraphPoint32;
   ThisTide : tstringList;
begin
   {$IfDef RecordSOESTtides} WriteLineToDebugFile('MakeGraphFromSOESTtides in'); {$EndIf}
   MonthlyData := Copy(Uppercase(ExtractFileName(fName)),1,1) = 'M';
//m files are months
//deal with last line which all 9999 or spaces

      Result := TThisBaseGraph.Create(Application);

      with Result do begin
        ThisTide := tstringList.Create;
        ThisTide.LoadFromFile(fName);
        OpenDataFile(rfile);
        MenuStr := ThisTide.Strings[0];

        StationName := trim(Copy(MenuStr,4,14));
        Lat := 1.0 * strToInt(Copy(MenuStr,22,2)) + 1/60*StrToFloat(Copy(MenuStr,25,4));
        if Copy(MenuStr,29,1) = 'S' then Lat := -Lat;
        Long := 1.0 * strToInt(Copy(MenuStr,37,3)) + 1/60*StrToFloat(Copy(MenuStr,41,4));
        if Copy(MenuStr,45,1) = 'W' then Long := -Long;

        Year1 := 9999;
        Year2 := -9999;
        GraphDraw.MinVertAxis := 9999;
        GraphDraw.MaxVertAxis := -9999;
        GraphDraw.VertLabel := 'Water Level (m)';
        for i := 1 to pred(ThisTide.Count) do begin
           MenuStr := ThisTide.Strings[i] + '                                                                       ';
           Val(copy(MenuStr,12,4),Year,err);
           if Year <> 9999 then begin
              Val(copy(MenuStr,16,3),Day,err);
              if (Year > Year2) then Year2 := Year;
              if (Year < Year1) then Year1 := Year;
              NumDays := YearLength(Year);
              for j := 0 to 11 do begin
                 if MonthlyData then TStr := trim(copy(MenuStr,16+j*5,5))
                 else TStr := trim(copy(MenuStr,20+j*5,5));
                 if (TStr <> '') then begin
                    Val(TStr,Ht,err);
                    if (abs(Ht) <> 9999) then begin
                       Ht := 0.001 * Ht;
                       if (Ht > GraphDraw.MaxVertAxis) then GraphDraw.MaxVertAxis := Ht;
                       if (Ht < GraphDraw.MinVertAxis) then GraphDraw.MinVertAxis := Ht;
                       if MonthlyData then v[1] := Year + (15 + j * 30) / 360
                       else v[1] := Year + (Day + j) / NumDays;
                       v[2] := Ht;
                       BlockWrite(rfile,v,1);
                    end;
                 end;
              end;
           end;
        end;
        CloseFile(rFile);
        ThisTide.Free;
        GraphDraw.MinHorizAxis := Year1;
        GraphDraw.MaxHorizAxis := succ(Year2);
        PadAxis(GraphDraw.MinVertAxis,GraphDraw.MaxVertAxis);
        {$IfDef RecordSOESTtides} WriteLineToDebugFile('MakeGraphFromSOESTtides graph redraw'); {$EndIf}
        GraphDraw.GraphDrawn := true;
        RedrawDiagram11Click(Nil);
      end;
   {$IfDef RecordSOESTtides} WriteLineToDebugFile('MakeGraphFromSOESTtides out'); {$EndIf}
end;


procedure TDemHandForm.XTfiles1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;


procedure JSONtoShapefile(fname : pathStr);
var
   FileInMemory : tStringList;
   pos : integer;
   Str : String;
   Coords  : ANSIString;
   NameStr,LatStr,LongStr : shortstring;
   Table : tMyData;
   ShapeFileCreator : tShapeFileCreation;
begin
   {$IfDef RecordImportProblems} WriteLineToDebugFile('JSONtoShapefile ' + fname); {$EndIf}
   ShowHourglassCursor;
   FileInMemory := tStringList.Create;
   FileInMemory.LoadFromFile(fName);
   fName := ChangeFileExt(fName,DefaultDBExt);

   ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,3);
   ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftString,'FEAT_TYPE',15);
   ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftString,'SUBTYPE',15);

   Str := ptTrim(FileInMemory.Strings[0]);
   Pos := PosEx('"xy"',Str);
   if Pos > 0  then Delete(Str,Pos,Length(Str) - Pos);

   while length(Str) > 0 do begin
        Pos := PosEx('feature_type',Str);
        if Pos = 0 then break;

        Delete(Str,1,Pos);
        Petmar_Types.BeforeSpecifiedCharacter(Str,':',true,true);
        Table.Insert;

        NameStr := trim(Petmar_Types.BeforeSpecifiedCharacter(Str,',',true,true));
        StripCharacter(NameStr,'"');
        Table.SetFieldByNameAsString('FEAT_TYPE',NameStr);

        Pos := PosEx('subtype',Str);
        if Pos > 0 then begin
          Delete(Str,1,Pos);
          Petmar_Types.BeforeSpecifiedCharacter(Str,':',true,true);
          NameStr := trim(Petmar_Types.BeforeSpecifiedCharacter(Str,',',true,true));
          StripCharacter(NameStr,'"');
          Table.SetFieldByNameAsString('SUBTYPE',NameStr);
        end;

        Pos := PosEx('((',Str);
        Delete(Str,1,Pos+1);
        Coords := trim(Petmar_Types.BeforeSpecifiedCharacter(Str,')',true,true));
        Coords := Coords + ',';

        while length(Coords) > 0 do begin
           LongStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Coords,' ',true,true);
           LatStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Coords,',',true,true);
           Coords := ptTrim(Coords);
           ShapeFileCreator.AddPointToShapeStream(StrToFloat(LatStr),StrToFloat(LongStr));
        end;
        ShapeFileCreator.ProcessRecordForShapeFile;
        Table.Post;
      end;
    ShapeFileCreator.CloseShapeFiles;
    FileInMemory.Destroy;
end;


procedure TDemHandForm.xview2json1Click(Sender: TObject);
var
   fNames : tStringList;
   DefaultFilter : byte;
   i : integer;
begin
   {$IfDef RecordImportProblems} WriteLineToDebugFile('TDemHandForm.xview2json1Click in'); {$EndIf}
   fNames := tStringList.Create;
   fNames.Add(MainMapData);
   DefaultFilter := 1;
   Memo1.Visible := true;
   if Petmar.GetMultipleFiles('JSON files','JSON|*.json',fNames,DefaultFilter) then begin
      for i := 0 to pred(fNames.Count) do begin
         Memo1.Lines.Add(TimeToStr(Now) + '  ' + IntToStr(succ(I)) + '/' + IntToStr(fNames.Count) + '   ' + fNames[i]);
         JSONtoShapefile(fNames[i]);
      end;
      Memo1.Lines.Add(TimeToStr(Now) + '  complete');
      ShowDefaultCursor;
   end;
   fNames.Destroy;
   {$IfDef RecordImportProblems} WriteLineToDebugFile('TDemHandForm.xview2json1Click out'); {$EndIf}
end;


procedure TDemHandForm.XYADBfile1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
    FilesWanted : tStringList;
    DefaultFilter : byte;
    i : integer;
    fName : PathStr;
    tf : textfile;
    f1,f2,f3,f4 : ShortString;
    Table : tMyData;
    aLine : ANSIString;
begin
   FilesWanted := tStringList.Create;
   if GetMultipleFiles('XYABD files','XYADB files|*.xyabd',FilesWanted,DefaultFilter) then begin
      StartProgress('XYADB');
      for i := 0 to pred(FilesWanted.Count) do begin
         UpDateProgressBar(i/FilesWanted.Count);
         fName := FilesWanted.Strings[i];
         assignFile(tf,fname);
         reset(tf);
         fName := ChangeFileExt(fName,DefaultDBExt);
         CreateXYADBTable(fName);
         Table := tMyData.Create(fName);
         while not eof(tf) do begin
             readln(tf,aLine);
             f1 := Petmar_types.BeforeSpecifiedCharacterANSI(aline,#9,true,true);
             f2 := Petmar_types.BeforeSpecifiedCharacterANSI(aline,#9,true,true);
             f3 := Petmar_types.BeforeSpecifiedCharacterANSI(aline,#9,true,true);
             f4 := Petmar_types.BeforeSpecifiedCharacterANSI(aline,#9,true,true);
             if (f3 <> 'NaN') or  (f4 <> 'NaN') or (aLine <> 'NaN') then begin
                Table.Insert;
                Table.SetFieldByNameAsString('LONG',f1);
                Table.SetFieldByNameAsString('LAT',f2);
                if (f3 <> 'NaN') then Table.SetFieldByNameAsString('AGE_MA',f3);
                if (f4 <> 'NaN') then Table.SetFieldByNameAsString('DEPTH_BASE',f4);
                if (aline <> 'NaN') then Table.SetFieldByNameAsString('BATHY_M',aline);
                Table.Post;
             end;
         end;
         Table.Destroy;
         CloseFile(tf);
      end;
      EndProgress;
   end;
{$EndIf}
end;

procedure TDemHandForm.XYZ1Click(Sender: TObject);
begin
   {$ifDef ExPointCloud}
   {$Else}
      LidarAsciiOut(lasascXYYonly);
   {$EndIf}
end;

procedure TDemHandForm.XYZClassIntensity1Click(Sender: TObject);
begin
   {$ifDef ExPointCloud}
   {$Else}
      LidarAsciiOut(lasascClassInt);
   {$EndIf}
end;



procedure TDemHandForm.XYZItoLAS1Click(Sender: TObject);
begin
   {$ifDef ExPointCloud}
   {$Else}
       LAStoolsTextToLAS;
   {$EndIf}
end;


procedure TDemHandForm.XYZRGB1Click(Sender: TObject);
begin
   {$ifDef ExPointCloud}
   {$Else}
      LidarAsciiOut(lasascRGB);
   {$EndIf}
end;

procedure TDemHandForm.zshift1Click(Sender: TObject);
begin
   IrishgridtoUTM1Click(Sender);
end;


{$IfDef ExGeology}
{$Else}

procedure ImportHarvardCentroids(MainDB : boolean);
var
   CentFile,FilesWanted : tStringList;
   fName : PathStr;
   i,j,RecSize : integer;
   DefaultFilter : byte;
   Mag : float64;
   TheTable : tMyData;
   MenuStr : ShortString;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(DBDir);
   DefaultFilter := 1;

   if GetMultipleFiles('Centroid files','CMT files|*.ndk;*.dek;*.txt',FilesWanted,DefaultFilter) then begin
      ShowHourglassCursor;
      fName := FilesWanted.Strings[0];
      if FileExtEquals(fName,'.NDK') then begin
         RecSize := 5;
      end
      else begin
         RecSize := 4;
      end;

      if MainDB then fName := CMT_fault_cent_fName
      else fName := ChangeFileExt(fName,DefaultDBExt);

      if not (FileExists(fName)) or AnswerIsYes('Overwrite existing file') then begin
         if (FileExists(fName)) then SysUtils.DeleteFile(fName);
         MakeEarthQuakeCentroidsFile(fName);
      end;


      {$IfDef RecordProblems} WriteLineToDebugFile('ImportHarvardCentroids to ' + fName); {$EndIf}

      TheTable := tMyData.Create(fName);
      StartProgress('Centroids');
      for j := 0 to pred(FilesWanted.Count) do begin
         UpdateProgressBar(j/FilesWanted.Count);
         FName := FilesWanted.Strings[j];
         CentFile := tStringList.Create;
         CentFile.LoadFromFile(fName);
         {$IfDef RecordProblems} WriteLineToDebugFile(fName + '   quakes=' + IntToStr(CentFile.Count div RecSize)); {$EndIf}
         i := 0;
         while i*RecSize < pred(CentFile.Count) do  begin
            TheTable.Insert;
            if FileExtEquals(fName,'.NDK') then  begin
             //Line 1
                MenuStr := CentFile.Strings[i*RecSize];
                TheTable.SetFieldByNameAsString('YEAR',ptTrim(Copy(MenuStr,6,4)));
                TheTable.SetFieldByNameAsString('MONTH',ptTrim(Copy(MenuStr,11,2)));
                TheTable.SetFieldByNameAsString('DAY',ptTrim(Copy(MenuStr,14,2)));
                TheTable.SetFieldByNameAsString('TIME',ptTrim(Copy(MenuStr,17,10)));
                TheTable.SetFieldByNameAsString('LAT',ptTrim(Copy(MenuStr,28,6)));
                TheTable.SetFieldByNameAsString('LONG',ptTrim(Copy(MenuStr,35,7)));
                TheTable.SetFieldByNameAsString('DEPTH',ptTrim(Copy(MenuStr,43,6)));

                Mag := StrToFloat(ptTrim(Copy(MenuStr,49,3)));
                if (Mag > 0.001) then TheTable.SetFieldByNameAsFloat('MB',Mag);
                Mag := StrToFloat(ptTrim(Copy(MenuStr,53,3)));
                if (Mag > 0.001) then TheTable.SetFieldByNameAsFloat('MS',Mag);
                TheTable.SetFieldByNameAsString('REGION',ptTrim(Copy(MenuStr,57,24)));

             //Line 2
                MenuStr := CentFile.Strings[i*RecSize + 1];
                TheTable.SetFieldByNameAsString('EVENT_ID',ptTrim(Copy(MenuStr,1,16)));

             //Line 5
                MenuStr := CentFile.Strings[i*RecSize + 4];

                TheTable.SetFieldByNameAsString('PLUNGE_1',ptTrim(Copy(MenuStr,13,2)));
                TheTable.SetFieldByNameAsString('STRIKE_1',ptTrim(Copy(MenuStr,16,3)));
                TheTable.SetFieldByNameAsString('PLUNGE_2',ptTrim(Copy(MenuStr,28,2)));
                TheTable.SetFieldByNameAsString('STRIKE_2',ptTrim(Copy(MenuStr,31,3)));
                TheTable.SetFieldByNameAsString('PLUNGE_3',ptTrim(Copy(MenuStr,43,2)));
                TheTable.SetFieldByNameAsString('STRIKE_3',ptTrim(Copy(MenuStr,46,3)));

                TheTable.SetFieldByNameAsString('FP1_STRIKE',ptTrim(Copy(MenuStr,58,3)));
                TheTable.SetFieldByNameAsString('FP1_DIP',ptTrim(Copy(MenuStr,62,2)));
                TheTable.SetFieldByNameAsString('FP2_STRIKE',ptTrim(Copy(MenuStr,70,3)));
                TheTable.SetFieldByNameAsString('FP2_DIP',ptTrim(Copy(MenuStr,74,2)));
                inc(i);
            end
            else begin
             //Line 1
                MenuStr := CentFile.Strings[i*RecSize];
                TheTable.SetFieldByNameAsString('YEAR',ptTrim(Copy(MenuStr,16,2)));
                TheTable.SetFieldByNameAsString('MONTH',ptTrim(Copy(MenuStr,10,2)));
                TheTable.SetFieldByNameAsString('DAY',ptTrim(Copy(MenuStr,13,2)));
                TheTable.SetFieldByNameAsString('TIME',ptTrim(Copy(MenuStr,19,10)));
                TheTable.SetFieldByNameAsString('LAT',ptTrim(Copy(MenuStr,30,6)));
                TheTable.SetFieldByNameAsString('LONG',ptTrim(Copy(MenuStr,37,7)));
                TheTable.SetFieldByNameAsString('DEPTH',ptTrim(Copy(MenuStr,44,6)));

                Mag := StrToFloat(ptTrim(Copy(MenuStr,50,3)));
                if (Mag > 0.001) then TheTable.SetFieldByNameAsFloat('MB',Mag);
                Mag := StrToFloat(ptTrim(Copy(MenuStr,53,3)));
                if (Mag > 0.001) then TheTable.SetFieldByNameAsFloat('MS',Mag);
                TheTable.SetFieldByNameAsString('REGION',ptTrim(Copy(MenuStr,56,24)));
                TheTable.SetFieldByNameAsString('EVENT_ID',ptTrim(Copy(MenuStr,1,9)));

             //Line 5
                MenuStr := CentFile.Strings[i*RecSize + 3];

                TheTable.SetFieldByNameAsString('PLUNGE_1',ptTrim(Copy(MenuStr,9,2)));
                TheTable.SetFieldByNameAsString('STRIKE_1',ptTrim(Copy(MenuStr,12,3)));
                TheTable.SetFieldByNameAsString('PLUNGE_2',ptTrim(Copy(MenuStr,23,2)));
                TheTable.SetFieldByNameAsString('STRIKE_2',ptTrim(Copy(MenuStr,26,3)));
                TheTable.SetFieldByNameAsString('PLUNGE_3',ptTrim(Copy(MenuStr,37,2)));
                TheTable.SetFieldByNameAsString('STRIKE_3',ptTrim(Copy(MenuStr,40,3)));

                TheTable.SetFieldByNameAsString('FP1_STRIKE',ptTrim(Copy(MenuStr,51,3)));
                TheTable.SetFieldByNameAsString('FP1_DIP',ptTrim(Copy(MenuStr,55,2)));
                TheTable.SetFieldByNameAsString('FP2_STRIKE',ptTrim(Copy(MenuStr,63,3)));
                TheTable.SetFieldByNameAsString('FP2_DIP',ptTrim(Copy(MenuStr,67,2)));
                inc(i);
            end;
            if TheTable.GetFieldByNameAsInteger('PLUNGE_1') > 50 then TheTable.SetFieldByNameAsString('MECH','T');
            if TheTable.GetFieldByNameAsInteger('PLUNGE_2') > 50 then TheTable.SetFieldByNameAsString('MECH','S');
            if TheTable.GetFieldByNameAsInteger('PLUNGE_3') > 50 then TheTable.SetFieldByNameAsString('MECH','N');
            TheTable.Post;
         end;
         CentFile.Free;
      end;
      TheTable.Destroy;
      EndProgress;
      {$IfDef RecordProblems} WriteLineToDebugFile('ImportHarvardCentroids out'); {$EndIf}
   end;
end;


procedure ImportISOGravity;
var
   MenuStr : ShortString;
   CentFile,FilesWanted : tStringList;
   fName : PathStr;
   err,i,j : integer;
   DefaultFilter : byte;
   Lat,Long,Min : float64;
   TheTable : tMyData;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(DBDir);
   DefaultFilter := 1;
   if GetMultipleFiles('ISO gravity file','*.iso',FilesWanted,DefaultFilter) then begin
      ShowHourglassCursor;
      fName := DBDir + 'gravity_st' + DefaultDBExt;
      if not FileExists(fName) then begin
         CreateISOGravity(fName);
         ApplicationProcessMessages;
      end;
      TheTable := tMyData.Create(fName);
      for j := 0 to pred(FilesWanted.Count) do begin
         FName := FilesWanted.Strings[j];
         CentFile := tStringList.Create;
         CentFile.LoadFromFile(fName);
         for i := 0 to pred(CentFile.Count) do begin
            TheTable.Insert;
            MenuStr := CentFile.Strings[i];

            Val(Copy(MenuStr,10,2),Lat,err);
            Val(Copy(MenuStr,12,4),Min,err);
            Lat := Lat + 0.01 / 60 * Min;
            Val(Copy(MenuStr,17,3),Long,err);
            Val(Copy(MenuStr,20,4),Min,err);
            Long := -(Long + 0.01 / 60 * Min);
            TheTable.SetFieldByNameAsFloat('LAT',Lat);
            TheTable.SetFieldByNameAsFloat('LONG',Long);

            Val(Copy(MenuStr,24,6),Min,err);
            TheTable.SetFieldByNameAsFloat('ELEV',Min * 0.1 * FeetToMeters);

            Val(Copy(MenuStr,64,6),Min,err);
            TheTable.SetFieldByNameAsFloat('BOUGER',Min * 0.01);

            Val(Copy(MenuStr,70,6),Min,err);
            TheTable.SetFieldByNameAsFloat('ISOSTATIC',Min * 0.1 * FeetToMeters);
            TheTable.Post;
         end;
         CentFile.Free;
      end;
      TheTable.Destroy;
      ShowDefaultCursor;
   end;
end;

{$EndIf}


procedure TDemHandForm.TIGERindex1Click(Sender: TObject);
begin
   {$IfDef ExTiger}
   {$Else}
      IndexTigerFiles;
   {$EndIf}
end;


procedure TDemHandForm.Conictolatlong1Click(Sender: TObject);
{$IfDef ExGIS}
begin
{$Else}
var
   ShapeFile : tShapeFile;
   FileNames : tStringList;
   FileName  : PathStr;
   BatchMode,NoDBF,
   Success   : boolean;
   DefaultFilter : byte;
   i,GISNum : integer;
   xshift,yshift,zshift : float64;
begin
   Memo1.Visible := true;
   FileNames := tStringList.Create;
   FileNames.Add(DBDir);
   DefaultFilter := 0;
   if not GetMultipleFiles('Shape file','shape file|*.shp|dBase table|*.dbf',FileNames,DefaultFilter) then Exit;
   BatchMode := (FileNames.Count > 1);
   if (Sender = ShiftXYZcoordinates1) then begin
      xshift := 53.000;
      yshift := 0.609;
      zshift := -3.802;
      ReadDefault('xShift',xshift);
      ReadDefault('yShift',xshift);
      ReadDefault('zShift',xshift);
      StartProgress('Shifting');
   end;
   for i := 0 to pred(FileNames.Count) do begin
      FileName := FileNames.Strings[i];
      if (Sender = ShiftXYZcoordinates1) then begin
         UpdateProgressBar(i/FileNames.Count);
         if OpenNumberedGISDataBase(GISNum,FileName) then begin
            with GISdb[GISnum] do begin
               while not MyData.eof do  begin
                  MyData.Edit;
                  MyData.SetFieldByNameAsFloat('X',MyData.GetFieldByNameAsFloat('X') + XShift);
                  MyData.SetFieldByNameAsFloat('Y',MyData.GetFieldByNameAsFloat('Y') + YShift);
                  MyData.SetFieldByNameAsFloat('Z',MyData.GetFieldByNameAsFloat('Z') + ZShift);
                  MyData.Next;
               end;
               SavePointShapeFile;
            end;
            CloseAndNilNumberedDB(GISnum);
         end;
      end
      else if (FileExtEquals(FileName,DefaultDBExt)) then MessageToContinue('Convert ' + FileName + ' to shapefile first')
      else if ItsAShapeFile(FileName,NoDBF) then begin
         {$IfDef RecordShapeFileContents}
            ShapeFileDump(FileName,ShapeSummaries);
            WriteStringListToDebugFile(ShapeSummaries);
            FreeAndNil(ShapeSummaries);
         {$EndIf}
         if BatchMode and (i=0) then begin
            ShapeFileNewLeader := 'll-';
            PETMAR.GetString('Leader for file names', ShapeFileNewLeader,false,ValidDOSFileNameChars);
         end;
         ShapeFile := tShapeFile.Create(FileName,success);
         if Sender = AddXYZtoDBFfile1 then ShapeFile.AddFields(afXYZ)
         else if Sender = AddLineEndPoints then ShapeFile.AddFields(afLineMerge);
         Memo1.Lines.Add('Done process: ' + ExtractFileName(FileName) + ',  type=' + IntToStr(ShapeFile.MainFileHeader.ShapeType));
         ShapeFile.Destroy;
      end
      else Memo1.Lines.Add('Not shape file ' + ExtractFileName(FileName));
   end;
   FileNames.Free;
   EndProgress;
{$EndIf}
end;


procedure TDemHandForm.ConvertDEMstoGeotiffGDAL1Click(Sender: TObject);
begin
   ConverttoGeotiff1Click(Sender);
end;

procedure TDemHandForm.ConverttoGeotiff1Click(Sender: TObject);
var
   fName : PathStr;
   OldGrid,i : integer;
   DefaultFilter : byte;
   FilesWanted : tStringList;
begin
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.ConverttoGeotiff1Click in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   DefaultFilter := 2;
   ShowSatProgress := false;
   if GetMultipleFiles('Grids to convert to Geotiff','DEM|*.dem|All files|*.*',FilesWanted,DefaultFilter) then begin
      StartProgress('Convert');
      for I := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         UpDateProgressBar(i/FilesWanted.Count);
         LoadNewDEM(OldGrid,fName,false);
         fName := ChangeFileExt(fName,'.tif');
         DEMGlb[OldGrid].SaveAsGeotiff(fName);
         CloseAllDEMs;
      end;
   end;
   FilesWanted.Free;
   EndProgress;
   ShowSatProgress := true;
end;


procedure TDemHandForm.Datumshift1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;



procedure TDemHandForm.DefinedtoUTM1Click(Sender: TObject);
begin
   IrishgridtoUTM1Click(Sender);
end;

procedure TDemHandForm.DEMs1Click(Sender: TObject);
var
   fName : PathStr;
   FilesWanted : tStringList;
   LatNW,LongNW,LatSE,LongSE : float64;
   i,BigDEM : integer;
   Ext : ExtStr;
   GridLimits : tGridLimits;
begin
    LatNW := 40;
    LongNW := -30;
    LatSE := 0;
    LongSE := 80;

    GetLatLongDefault(WGS84DatumConstants,'NW corner',LatNW,LongNW);
    GetLatLongDefault(WGS84DatumConstants,'NW corner',LatSE,LongSE);
    if AnswerIsYes('Geotiff') then Ext := '.tif' else Ext := '.dem';

    FilesWanted := tStringList.Create;
    FilesWanted.Add(LastDEMName);
    if GetMultipleFiles('DEM',DEMFilterMasks,FilesWanted,MDDef.DefaultDEMFilter) then begin
       for i := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted.Strings[i];
          LoadNewDEM(BigDEM,fName,false);
          DEMGlb[BigDEM].LatLongDegreeToDEMGridInteger(LatNW,LongNW,GridLimits.XGridLow,GridLimits.YGridHigh);
          DEMGlb[BigDEM].LatLongDegreeToDEMGridInteger(LatSE,LongSE,GridLimits.XGridHigh,GridLimits.YGridLow);
          SafeMakeDir(ExtractFilePath(fName) + 'subset\');
          fName := ExtractFilePath(fName) + 'subset\' + DEMGlb[BigDEM].AreaName + Ext;
          DEMGlb[BigDEM].RectangleSubsetDEM(GridLimits,fName);
          CloseSingleDEM(BigDEM);
       end;
    end;
end;

procedure TDemHandForm.DEMtoLAZ1Click(Sender: TObject);
var
   InName,OutName : PathStr;
   Settings : shortstring;
   FilesWanted : tStringList;
   I,DEM : integer;
begin
   FilesWanted := tstringList.Create;
   FilesWanted.Add(LastDEMName);
   if GetMultipleFiles('DEMs to convert tyo LAZ',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
      Memo1.Visible := true;
      for I := 0 to pred(FilesWanted.Count) do begin
         Memo1.Lines.Add(TimeToStr(Now) + IntegerToString(i,4) + '/' + IntToStr(FilesWanted.Count));
         InName := FilesWanted.Strings[i];
         LoadNewDEM(DEM,InName,true);
         InName := DEMGlb[DEM].SelectionMap.GeotiffDEMNameOfMap;
         OutName := ChangeFileExt(DEMGlb[DEM].DEMFileName,'.laz');
         Lastools_DEMToLAZ(InName,OutName,'');
         CloseSingleDEM(DEM);
      end;
   end;
   FilesWanted.Free;
end;

procedure TDemHandForm.GDALassignandwarptoUTM1Click(Sender: TObject);
begin
   GDALreprojectimagetoUTMNAD831Click(Sender);
end;

procedure TDemHandForm.GDALbandextraction1Click(Sender: TObject);
begin
   GDALBandExtraction;
end;


procedure TDemHandForm.GDALcreatemultibandTIFF1Click(Sender: TObject);
begin
   GDALcreatemultibandTIFF;
end;


procedure TDemHandForm.GDALGeodatabasetoshapefile1Click(Sender: TObject);
begin
   GDALGeodatabasetoshapefile;
end;


procedure TDemHandForm.GDALmergeGeotiff1Click(Sender: TObject);
var
   fName : PathStr;
begin
   CallGDALMerge(fName,Nil);
end;


procedure TDemHandForm.GDALreprojectimagetoUTMNAD831Click(Sender: TObject);
var
   fName,fName2,outName : PathStr;
   DefaultFilter : byte;
   BatchFile,FileNames : tStringList;
   UTMzone,
   i : Integer;
   ch : ANSIchar;
   TStr2,outer,InProj,OutEPSG : shortString;
   cmd : ANSIString;
   GDALinfo : tGDALinfo;
begin
   {$IfDef RecordGDAL} WriteLineToDebugFile('TDemHandForm.GDALreprojectimagetoUTMNAD831Click in'); {$EndIf}
   ZeroGDALinfo(GDALinfo);
   FileNames := tStringList.Create;
   FileNames.Add(ExtractFilePath(LastImageName));
   DefaultFilter := 0;
   if not GetMultipleFiles('TIF file to reproject','Image|*.tif;*.jp2;*.pdf',FileNames,DefaultFilter) then Exit;
   inProj := '';
   fName := FileNames.Strings[0];
   if (Sender = GDALwarpGeotifftoGeoWGS841) then begin
       fName2 := GDAL_warp_name;
       TStr2 := 'warp';
       ch := 't';
       OutEPSG := '4326';
       outer := 'geo';
   end
   else if (Sender = GDALwarpGeotifftoadjacentUTMzone1) then begin
       fName2 := GDAL_warp_name;
       PickUTMZone(MDdef.DefaultUTMZone);
       TStr2 := 'warp';
       ch := 't';
       OutEPSG := '269' + AddDayMonthLeadingZero(MDdef.DefaultUTMZone);
       outer := 'utm_x_zone';
   end
   else begin
      GetGDALinfo(fName,GDALinfo);
      GetEPSG(GDALinfo);
      OutEPSG := IntToStr(GDALinfo.utmEPSG);
      outer := 'utm';
      if (Sender = GDALwarpGeotifftoUTMNAD831) or (Sender = GDALassignandwarptoUTM1) then begin
         fName2 := GDAL_warp_name;
         TStr2 := 'warp';
         ch := 't';
         if (Sender = GDALassignandwarptoUTM1) then begin
            inProj := ' -s_srs EPSG:' + IntToStr(GDALinfo.inEPSG);
         end;
      end
      else if (Sender = GDALwarpGeotifftoUTMNAD831) then begin
         fName2 := GDAL_translate_name;
         TStr2 := 'trans';
         ch := 'a';
      end;
   end;

   if IsGDALFilePresent(fName2) then begin
      StartGDALbatchFile(BatchFile);
      for i := 0 to pred(FileNames.Count) do begin
         fName := FileNames.Strings[i];
         OutName := ExtractFilePath(fName) + Tstr2 + '_' + outer + '_' + ExtractFileNameNoExt(fName) + '.tif';
         cmd := fName2 + ' -of Gtiff ' + inProj + ' -' + ch + '_srs EPSG:' + OutEPSG + ' ' +  fName + ' ' + OutName;
         {$IfDef RecordGDAL} WriteLineToDebugFile(cmd); {$EndIf}
         BatchFile.Add(cmd);
      end;
     EndBatchFile(MDTempDir + 'r2v.bat',batchfile);
   end;
   FileNames.Free;
end;


procedure TDemHandForm.GDALreprojectshapefile1Click(Sender: TObject);
begin
   GDALreprojectshapefile;
end;


procedure TDemHandForm.GDALwarpGeotifftoadjacentUTMzone1Click(Sender: TObject);
begin
   GDALreprojectimagetoUTMNAD831Click(Sender);
end;

procedure TDemHandForm.GDALwarpGeotifftoGeoWGS841Click(Sender: TObject);
begin
   GDALreprojectimagetoUTMNAD831Click(Sender);
end;

procedure TDemHandForm.GDALwarpGeotifftoUTMNAD831Click(Sender: TObject);
begin
   GDALreprojectimagetoUTMNAD831Click(GDALwarpGeotifftoUTMNAD831);
end;


procedure TDemHandForm.GDALwarpSentinel11Click(Sender: TObject);
var
   Paths : tStringList;
   i : integer;
begin
   {$IfDef RecordGDAL} WriteLineToDebugFile('TDemHandForm.GDALwarpSentinel11Click in'); {$EndIf}
   if IsGDALFilePresent(GDAL_warp_name) then begin
      Paths := tStringList.Create;
      Paths.Add(LastSatDir);
      if GetMultipleDirectories('Directories with Sentinel-1 images to warp',Paths) then begin
         for I := 0 to Paths.Count do

         ResampleSentinel_1(Paths.Strings[i]);
      end;
   end;

end;

procedure TDemHandForm.LineKML1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := MainMapData;
   if Petmar.GetFileFromDirectory('Lines in KML','*.kml',fName) then begin
      LineKMLtoStringList(fName);
   end;
end;


procedure TDemHandForm.FormCreate(Sender: TObject);
begin
   {$IfDef ExMrSID}
      MrSIDtotiff1.Visible := false;
   {$EndIf}

   Petmar.CheckFormPlacement(Self);
   {$IfDef HideHelpButtons} Help1.Visible := false; {$EndIf}
   FileMode := 2;
end;


procedure TDemHandForm.SINfiletoshapefile1Click(Sender: TObject);
const
   ArraySize = 64000;
type
   CoordsType = array[1..ArraySize] of array[1..2] of float32;
var
   Coords : ^CoordsType;
   DataFile : file;
   Result,i,j  : integer;
   PointsInPolyLine : integer;
   FileName : PathStr;
   dCoord : ^tDcoords;
   Table : tMyData;
   Parts : array[1..1] of integer;
   zs : array of float64;
   ShapeFileCreator : tShapeFileCreation;
begin
   FileName := '';
   if not Petmar.GetFileFromDirectory('SIN file','*.sin',FileName) then exit;
   assignFile(DataFile,FileName);
   reset(DataFile,8);
   New(Coords);
   PointsInPolyLine := 0;
   fileName := ChangeFileExt(FileName,DefaultDBExt);
   new(Dcoord);
   Parts[1] := 0;    //record in the stream where coordinates start
   LastRecPolyLineHeader.NumParts := 1;
   MakeBasicBoundingBoxTable(fileName);
   Make_tables.DefineAndCreateANewTable(FileName,false,false,false,true,true);
   ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,FileName,false, 3);
   ShapeFileCreator.Table := tMyData.Create(FileName);
   j := 0;
   while not EOF(DataFile) do begin
      BlockRead(DataFile,Coords^,ArraySize,Result);
      for i := 1 to Result do begin
         if (Coords^[i,1] < 9) then begin
            dCoord^[j].Lat := Coords^[i,1]/DegToRad;
            dCoord^[j].Long :=Coords^[i,2]/DegToRad;
            inc(j);
         end
         else begin
            ShapeFileCreator.OldProcessRecordForShapeFile(dCoord^,1,j,Parts,zs);
            Table.Insert;
            Table.Post;
            j := 0;
         end;
      end {for i};
   end {while};
   Dispose(Coords);
   Dispose(Dcoord);
   closeFile(DataFile);
   ShapeFileCreator.CloseShapeFiles;
end;


procedure TDemHandForm.SOESTtidetimeseries1Click(Sender: TObject);
{uses University of Hawaii tide station time series, http://uhslc.soest.hawaii.edu/}
var
   k,Year1,Year2  : integer;
   Lat,Long     : float64;
   IndexfName,
   fName : PathStr;
   StationName : ShortString;
   AllTides,
   TheIndex: tStringList;
   Graph : TThisBaseGraph;
begin
   Memo1.Visible := true;
   IndexfName := MainMapData + 'tide_stations_slr\Univ_hawaii\';    //TIDESTATIONS.dbf';
   AllTides := Nil;
   Petmar.FindMatchingFiles(IndexfName,'*.dat',AllTides);
   TheIndex := tStringList.Create;
   TheIndex.Add('LAT,LONG,LOCATION,START,END,DURATION,TIME_SER,RISE,' + RecNoFName);
   for k := 0 to pred(AllTides.Count) do begin
      fName := AllTides.Strings[k];
      Graph := MakeGraphFromSOESTtides(fName, Lat,Long, Year1,Year2,StationName);
      Graph.SpeedButton6Click(nil);
      TheIndex.Add(RealToString(Lat,-12,-2) + ',' + RealToString(Long,-12,-2) + ',' + StationName + ',' + IntToStr(Year1) + ',' + IntToStr(Year2) + ',' + IntToStr(succ(Year2-Year1)) + ',' + ExtractFileName(fName) + ',' + RealToString(1000 * FittedSlope,-12,-2) + ',' + IntToStr(succ(k)));
      Graph.Close;
      Memo1.Lines.Add(TimeToStr(now) + '  ' + IntToStr(k) + '/' + IntToStr(AllTides.Count) + '  ' + ExtractFileName(fName) + '  ' + RealToString(1000 * FittedSlope,-12,-2));
      wmdem.Closealltexteditwindows1Click(Sender);
   end;
   IndexfName := IndexfName + 'TIDESTATIONS.dbf';
   PetDBUtils.StringList2CSVtoDB(TheIndex,IndexFName,true);
   AllTides.Free;
end;


procedure TDemHandForm.Close1Click(Sender: TObject);
begin
   Close;
end;


procedure CloudCompare(Distances : boolean);
var
   Inf : TextFile;
   Line : ANSIstring;
   fNames,
   strlist : tStringList;
   Fname : PathStr;
   Table : tMyData;
   DefaultFilter : byte;
   i,j,k : integer;
   x,y,Lat,Long : float64;
begin
   DefaultFilter := 1;
   fNames := TStringList.Create;
   fNames.Add(MainMapData);
   Petmar.GetMultipleFiles('Cloud Compare output','*.txt',fNames,DefaultFilter);
   if (Fnames.Count > 0) then begin
       ReadDefault('UTM zone',MDDef.DefaultUTMZone);
       WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',MDDef.DefaultUTMZone,MDDef.DefaultLatHemi,'CloudCompare');
       ReadDefault('Thin factor',MDDef.CloudThinFactor);
       StartProgress('CC import');
       for k := 0 to pred(fNames.Count) do begin
         UpdateProgressBar(k/fNames.Count);
         fName := fNames[k];
         strlist := tStringList.Create;
         if Distances then begin
            StrList.Add('lat,long,xutm,yutm,z,c2c_dist,c2c_dist_x,c2c_dist_y,c2c_dist_z');
            StrList.Add('39.12345678,-112.12345678,377967.920,3631999.650,1205.170,111.719,111.300,111.640,110.380');
         end
         else begin
            StrList.Add('lat,long,xutm,yutm,z,cluster');
            StrList.Add('39.12345678,-112.12345678,377967.920,3631999.650,1205.170,Y');
         end;

         assignFile(inf,fName);
         reset(inf);
         readln(inf);
         fName := ChangeFileExt(fName,DefaultDBExt);
         StripBlanks(fName);
         StringList2CSVtoDB(StrList,fName,true);
         Table := tMyData.Create(fName);
         Table.Edit;
         Table.Delete;
         ShowHourglassCursor;

         i := 0;
         while not eof(inf) do begin
            if (i mod 1000 = 0) then begin
               wmdem.StatusBar1.Panels[0].Text := IntToStr(i);
               ApplicationProcessMessages;
            end;
            inc(i);
            readln(inf,Line);
            Table.Insert;
            x := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
            y := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
            WGS84DatumConstants.UTMtoLatLongDegree(x,y,Lat,Long);
            Table.SetFieldByNameAsFloat('LAT',Lat);
            Table.SetFieldByNameAsFloat('LONG',Long);
            Table.SetFieldByNameAsFloat('XUTM',x);
            Table.SetFieldByNameAsFloat('YUTM',y);
            Table.SetFieldByNameAsString('Z',Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
            if Distances then begin
               Table.SetFieldByNameAsString('C2C_DIST',Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
               Table.SetFieldByNameAsString('C2C_DIST_X',Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
               Table.SetFieldByNameAsString('C2C_DIST_Y',Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
               Table.SetFieldByNameAsString('C2C_DIST_Z',Line);
            end;
            Table.Post;
            for j := 1 to pred(MDDef.CloudThinFactor) do if not eof(inf) then readln(inf);
         end;
         Table.Destroy;
       end;
   end;
   fNames.Free;
   EndProgress;
   wmdem.StatusBar1.Panels[0].Text := '';
end;


procedure TDemHandForm.Help1Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme83sn.htm');
end;


procedure TDemHandForm.HTMLcleanup1Click(Sender: TObject);
{$IfDef ExKML}
begin
{$Else}
var
   Tstrl,FNames : tStringList;
   fName : PathStr;
   DefaultFilter : byte;
   i : integer;
   RemoveLinks : boolean;
begin
   FNames := tStringList.Create;
   DefaultFilter := 1;
   RemoveLinks := AnswerIsYes('Remove links');
   if GetMultipleFiles('HTML to clean up','HTM files|*.HTM;*.HTML',FNames,DefaultFilter) then begin
      for I := 0 to pred(fNames.Count) do begin
         fName := fNames.Strings[i];
         Tstrl := tStringList.Create;
         TStrl.LoadFromFile(fName);
         KML_opts.CleanHTMLFile(TStrl,RemoveLinks);
         if (FNames.Count = 0) then DisplayAndPurgeStringList(TStrl,'Cleaned HTML')
         else begin
            TStrl.SaveToFile(fName);
            TStrl.Free;
         end;
      end;
   end;
   fNames.Free;
{$EndIf}
end;


procedure TDemHandForm.HTMLextractlinks1Click(Sender: TObject);
var
   Links,ThisFile : tStringList;
   fName : PathStr;
   Dir,TStr : ANSIstring;
   SeekLink,
   ThisLine : ANSIstring;
   i      : integer;
begin
   Links := tStringList.Create;
   ShowHourglassCursor;
   if GetFileFromDirectory('HTML to extract links','*.*',fName) then begin
      ThisFile := tStringList.Create;
      ThisFile.LoadFromFile(fName);
      for i := 0 to pred(ThisFile.Count) do begin
          ThisLine := ThisFile.Strings[i];
          SeekLink := ThisLine;
          while (length(SeekLink) > 2) do begin
             if (Copy(SeekLink,1,2) = '<a') then begin
                while (SeekLink[1] <> '"') and (Length(SeekLink) > 1) do Delete(SeekLink,1,1);
                Delete(SeekLink,1,1);
                if (SeekLink <> '') then begin
                   TStr := '';
                   repeat
                      TStr := TStr + SeekLink[1];
                      Delete(SeekLink,1,1);
                   until (SeekLink[1] = '"') or (Length(SeekLink) = 0);
                   Links.Add(TStr);
                end;
             end
             else Delete(SeekLink,1,1);
          end;
      end;
   end;
   ShowDefaultCursor;
   fName := Dir + 'html_summary.htm';
   ExecuteFile(fName, '', '');
   DisplayAndPurgeStringList(Links,'Links in file');
   ThisFile.Free;
end;

procedure TDemHandForm.HTMLfilelinks1Click(Sender: TObject);
label
   Found;
var
   TheFiles,Links : tStringList;
   fName : PathStr;
   k      : integer;
   DefaultFilter : byte;
begin
    DefaultFilter := 1;
    TheFiles := tStringList.Create;
    if GetMultipleFiles('files for download links','any file|*.*',TheFiles,DefaultFilter) then begin
       ShowHourglassCursor;
       Links := tStringList.Create;
       Links.Add(StartHTMLString);
       Links.Add('<h2>Data download</h2>');
       for k := 0 to pred(TheFiles.Count) do begin
         fName := ExtractFileName(TheFiles.Strings[k]);
         Links.Add(CreateHTML_href(fName,fName));
         Links.Add('<br>');
       end;
       Links.Add(EndHTMLString);
      ShowDefaultCursor;
      fName := ExtractFilePath(TheFiles.Strings[0]) + 'data_download.htm';
      Links.SaveToFile(fName);
      Links.Free;
      ExecuteFile(fName, '', '');
   end;
   TheFiles.Free;
end;

procedure TDemHandForm.HTMLinventory1Click(Sender: TObject);
label
   Found;
var
   TheFiles,Links,Images,ThisFile,TheContents : tStringList;
   fName : PathStr;
   Dir,TStr,MenuStr : ANSIstring;
   SeekLink,
   ImageLink,
   Title,
   ThisLine : ANSIstring;
   i,j,k      : integer;
begin
   ThisFile := tStringList.Create;
   TheContents := tStringList.Create;
   Dir := 'C:\mydocs\md_help\html\';
   GetDOSPath('directory',Dir);
   TheContents.Add(StartTableString);
   Links := tStringList.Create;
   Images := tStringList.Create;
   ShowHourglassCursor;
   TheFiles := Nil;
   Petmar.FindMatchingFiles(Dir,'*.htm',TheFiles);
   for k := 0 to pred(TheFiles.Count) do begin
      fName := ExtractFileName(TheFiles.Strings[k]);
      ThisFile.LoadFromFile(Dir + fName);
      MenuStr := '<tr>' + StartColumnString + '<a href="html\' + fName + '">' + fName + '</a></td><td>';
      Title := '';
      Links.Clear;
      Images.Clear;
      for i := 0 to pred(ThisFile.Count) do begin
          ThisLine := ThisFile.Strings[i];
          if (Copy(ThisLine,1,7) = '<title>') then begin
             Delete(ThisLine,1,7);
             Delete(ThisLine,Length(ThisLine)-7,8);
             Title := ThisLine;
          end;
          ImageLink := ThisLine;
          while (length(ImageLink) > 4) do begin
             if Copy(ImageLink,1,4) = '<img' then begin
                while (ImageLink[1] <> '"') do Delete(ImageLink,1,1);
                Delete(ImageLink,1,1);
                TStr := '';
                repeat
                   TStr := TStr + ImageLink[1];
                   Delete(ImageLink,1,1);
                until (ImageLink[1] = '"') or (Length(ImageLink) = 0);
                Images.Add(TStr);
             end
             else Delete(ImageLink,1,1);
          end;

          SeekLink := ThisLine;
          while (length(SeekLink) > 2) do begin
             if (Copy(SeekLink,1,2) = '<a') then begin
                while (SeekLink[1] <> '"') and (Length(SeekLink) > 1) do Delete(SeekLink,1,1);
                Delete(SeekLink,1,1);
                if (SeekLink <> '') then begin
                   TStr := '';
                   repeat
                      TStr := TStr + SeekLink[1];
                      Delete(SeekLink,1,1);
                   until (SeekLink[1] = '"') or (Length(SeekLink) = 0);
                   Links.Add(TStr);
                end;
             end
             else Delete(SeekLink,1,1);
          end;
      end;

      SeekLink := StartColumnString;
      for j := 0 to pred(Links.Count) do begin
         if (j > 0) then SeekLink := SeekLink + '<p>';
         SeekLink := SeekLink + Links.Strings[j];
         if (j > 0) then SeekLink := SeekLink + '</p>';
      end;
      SeekLink := SeekLink + '&nbsp;</td>';

      ImageLink := '<td>';
      for j := 0 to pred(Images.Count) do begin
         if (j > 0 )then ImageLink := ImageLink + '<p>';
         ImageLink := ImageLink + Images.Strings[j];
         if (j > 0) then ImageLink := ImageLink + '</p>';
      end;
      ImageLink := ImageLink + '&nbsp;' + EndColumnString;
      TheContents.Add(MenuStr + Title + EndColumnString  + SeekLink + ImageLink + '</tr>');
      Found:;
   end;
   TheContents.Add(EndTableString);
   ShowDefaultCursor;
   fName := Dir + 'html_summary.htm';
   TheContents.SaveToFile(fName);
   TheContents.Free;
   ExecuteFile(fName, '', '');
   ThisFile.Free;
   TheFiles.Free;
end;


procedure TDemHandForm.Filemodificationdates1Click(Sender: TObject);
var
   FNames : tStringList;
   fName : PathStr;
   DefaultFilter : byte;
   i : integer;
begin
   FNames := tStringList.Create;
   DefaultFilter := 1;
   if GetMultipleFiles('change file modification date','all files|*.*',FNames,DefaultFilter) then begin
      for I := 0 to pred(fNames.Count) do begin
         fName := fNames.Strings[i];
         FileSetDate(fName, DateTimeToFileDate(Now));
      end;
   end;
   fNames.Free;
end;

procedure TDemHandForm.FLTfiles1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;

procedure TDemHandForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordTDemHandFormFormClose} WriteLineToDebugFile('TDemHandForm.FormClose in'); {$EndIf}
   Action := caFree;
   FileMode := 2;
   {$IfDef RecordTDemHandFormFormClose} WriteLineToDebugFile('TDemHandForm.FormClose out'); {$EndIf}
end;


procedure TDemHandForm.Header2Click(Sender: TObject);
begin
   EditDEMHeader;
end;


procedure TDemHandForm.AssignbyEPSGandreprojecttoUTM1Click(Sender: TObject);
begin
   LASGeotoUTM1Click(Sender);
end;

procedure TDemHandForm.Assignprojection1Click(Sender: TObject);
begin
   LASGeotoUTM1Click(Sender);
end;


procedure TDemHandForm.AssignprojectionUTM1Click(Sender: TObject);
begin
    LASGeotoUTM1Click(Sender);
end;


procedure TDemHandForm.BILdirectorytoMDDEM1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;

procedure TDemHandForm.UKOStoUTM1Click(Sender: TObject);
begin
   IrishgridtoUTM1Click(Sender);
end;

procedure TDemHandForm.Untilegeotiffs1Click(Sender: TObject);
begin
   GDALConvertImagesToGeotiff;
end;


procedure TDemHandForm.IrishgridtoUTM1Click(Sender: TObject);
var
   DefaultFilter : byte;
   FilesWanted : tstringList;
   i : integer;
begin
   DefaultFilter := 0;
   FilesWanted := tstringList.Create;
   if GetMultipleFiles('LAS file','LAS files|*.las',FilesWanted,DefaultFilter) then begin
      MDDef.Ls.DeleteLASAfterTransformation := AnswerIsYes('Delete files after transformation');
      if (Sender = DefinedtoUTM1) or (Sender = ChangeUTMzone1) or (Sender = zshift1) then PickUTMZone(MDDef.DefaultUTMZone);
      for i := 0 to pred(FilesWanted.Count) do begin
         wmdem.StatusBar1.Panels[0].Text := 'LAS  ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count);
         if (Sender = UKOStoUTM1) then ReprojectOKUSLasFileToUTM(FilesWanted.Strings[i])
         else if (Sender = DefinedtoUTM1) or (Sender = ChangeUTMzone1) then ReprojectDefinedLasFileToUTM(FilesWanted.Strings[i])
         else if (Sender = zshift1) then ZShiftDefinedLasFileToMeters(FilesWanted.Strings[i])
         else ReprojectIrishLasFileToUTM(FilesWanted.Strings[i]);
      end;
   end;
   if MDDef.Ls.DeleteLASAfterTransformation then for i := 0 to pred(FilesWanted.Count) do SysUtils.DeleteFile(FilesWanted.Strings[i]);
   FilesWanted.Free;
   wmdem.StatusBar1.Panels[0].Text := '';
end;


procedure TDemHandForm.Monthlytimeseries1Click(Sender: TObject);
var
   fName : PathStr;
   Year,FName2,value : shortstring;
   AllFile,CSV : tStringList;
   aLine : AnsiString;
   Start : boolean;
   i,j,Seq,db : integer;
   Sep  : AnsiChar;
begin
   fName := ExtractFilePath(LastDataBase);
   if GetFileFromDirectory('Monthly time series','*.*',FName) then begin
      fName2 := 'VALUE';
      GetValidDBfieldname(fName2);
      AllFile := tStringList.Create;
      AllFile.LoadFromFile(fName);
      CSV := tStringList.Create;
      CSV.Add('SEQ,YEAR,MONTH,' + fName2);
      Start := false;
      Seq := 0;
      for i := 0 to pred(AllFile.Count) do begin
         aLine := AllFile.Strings[i];
         if Start then begin
            if length(aline) > 4 then begin
               Year := BeforeSpecifiedCharacterANSI(aline,Sep,true,true);
               if (Length(Year) = 4) and (Year[1] in ['1','2']) then begin
                  for j := 1 to 12 do begin
                     Value := BeforeSpecifiedCharacterANSI(aline,Sep,true,true);
                     if (Value <> '') then begin
                        inc(Seq);
                        CSV.Add(IntToStr(Seq) + ',' + Year + ',' + Petmar_Types.MonthName[j] + ',' + Value);
                     end;
                  end;
               end;
            end;
         end;
         if UpperCase(copy(aline,1,4)) = 'YEAR' then begin
            GetSeparationCharacter(aLine,Sep);
            Start := true;
         end;
      end;
      fName := ChangeFileExt(fName,'.csv');
      CSV.SaveToFile(fName);
      CSV.Free;
      OpenNumberedGISDataBase(db,fName,true);
      AllFile.Free;
   end;
end;


procedure TDemHandForm.MrSIDtotiff1Click(Sender: TObject);
begin
   {$IfDef ExMrSID}
   {$Else}
      ConvertsSIDsToTiff;
   {$EndIf}
end;

procedure TDemHandForm.ASCIIduplicates1Click(Sender: TObject);
const
   FName          : PathStr = '';
var
   Infile,Outfile : TextFile;
   Dir            : DirStr;
   bName          : NameStr;
   Ext            : ExtStr;
   Line1,Line2    : ShortString;
   Read,Written   : integer;
begin
   if (FName = '') then FName := ProgramRootDir;
   if GetFileFromDirectory('file for sequantial duplicate line removal','*.*',FName) then begin
      {$IfDef RecordHandlingProblems} WriteLineToDebugFile('ASCIIduplicates1Click: ' + FName,true); {$EndIf}
      InsureFileIsNotReadOnly(FName);
      assignFile(Infile,FName);
      reset(Infile);
      FSplit(FName,Dir,BName,Ext);
      assignFile(Outfile,Dir + 'temptemp.tmp');
      rewrite(Outfile);
      Line2 := 'zqxvfg';
      Read := 0;
      Written := 0;
      StartCount('Process file');
      {$IfDef RecordHandlingProblems} WriteLineToDebugFile('start loop'); {$EndIf}
      while not EOF(Infile) do begin
          readln(Infile,Line1);
          inc(Read);
          if (Read mod 50 = 0) then UpdateCount(Read);
          if (Line1 <> Line2) then begin
             writeln(Outfile,Line1);
             Line2 := Line1;
             inc(written);
          end;
      end;
      EndCount;
      {$IfDef RecordHandlingProblems} WriteLineToDebugFile('end loop'); {$EndIf}
      closeFile(InFile);
      CloseFile(OutFile);
      DeleteFileIfExists(FName);
      SysUtils.RenameFile(Dir + 'temptemp.tmp',FName);
      MessageToContinue('Read ' + IntToStr(Read) + MessLineBreak + 'Write ' + IntToStr(Written) + ' lines');
   end;
end;


procedure TDemHandForm.Separatefile1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      ImportHarvardCentroids(false);
   {$EndIf}
end;

procedure TDemHandForm.SF3Censusdata1Click(Sender: TObject);
var
   fName : PathStr;
   Table : tMyData;
   Data  : tStringList;
   ID,Desc : shortstring;
   NewIDs,IDs,Descriptions : AnsiString;
   db : integer;
begin
    fName := MainMapData;
    if Petmar.GetFileFromDirectory('SF3 data file','*.txt',fName) then begin
       Data := tStringList.Create;
       Data.LoadFromFile(fName);
       IDs := Data.Strings[0];
       StripCharacter(IDs,'"');
       Descriptions := Data.Strings[1];
       StripCharacter(Descriptions,'"');

       fName := ProgramRootDir + 'SF3' + DefaultDBExt;
       if not FileExists(fName) then MakeSF3Table(fName);
       Table := tMyData.Create(fName);
       NewIDS := '';
       while length(IDS) > 0 do begin
          ID := Petmar_types.BeforeSpecifiedCharacterANSI(IDs,',',true,true);
          Desc := Petmar_types.BeforeSpecifiedCharacterANSI(Descriptions,',',true,true);
          Table.ApplyFilter( 'SF3_CODE=' + QuotedStr(ID));
          if Table.RecordCount = 0 then begin
             Table.Insert;
             Table.SetFieldByNameAsString('SF3_CODE',ID);
             Table.SetFieldByNameAsString('DESCRIPT',Desc);
             repeat
                Petmar.GetString('Name for ' + Desc,Desc,true,Petmar_types.DBaseFieldNameChars);
                if Length(Desc) > 10 then MessageToContinue('Name too long (> 10 characters)');
             until Length(Desc) <= 10;
             Table.SetFieldByNameAsString('SHORT_NAME',Desc);
             Table.Post;
          end
          else Desc := Table.GetFieldByNameAsString('SHORT_NAME');
          if Length(NewIDS) > 0 then NewIds := NewIds + ',';
          NewIds := NewIds + '"' + Desc + '"';
       end;
       Data.Delete(0);
       Data.Delete(0);
       Data.Insert(0,NewIds);
       fName := MDTempDir + 'sf3.csv';
       Data.SaveToFile(MDTempDir + 'sf3.csv');
       Data.Free;
       Table.Destroy;
       OpenNumberedGISDataBase(db,fName,true);
    end;
end;


procedure TDemHandForm.ASCIIsort1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList : tStringList;
begin
   if (fName = '') then FName := ProgramRootDir;
   while GetFileFromDirectory('ASCII sort','*.*',FName) do begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      TheList.Sort;
      TheList.SaveToFile(FName);
      TheList.Free;
     ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.ExtractXYZfromDXFfile1Click(Sender: TObject);
var
   fName : PathStr;
   Data  : tStringList;
   I,pts     : Integer;
   Table : tMyData;
   TStr,xc,yc,zc : ShortString;
begin
   fName := '';
   if not GetFileFromDirectory('DXF file','*.dxf',FName) then exit;
   Data := tStringList.Create;
   Data.LoadFromFile(fName);
   fName := ChangeFileExt(fName,DefaultDBExt);
   CreateXYZTable(fName,false);
   Table := tMyData.Create(fName);
   StartProgress('Convert');
   i := 0;
   pts := 0;
   while I <= (Data.Count - 5) do begin
      UpdateProgressBar(i/Data.Count);
      xc := ptTrim(Data.Strings[i]);
      yc := ptTrim(Data.Strings[i+2]);
      zc := ptTrim(Data.Strings[i+4]);
      if ((xc = '10') or (xc = '11')) and ((yc = '20') or (yc = '21')) and ((zc = '30') or (zc = '31')) then begin
        inc(Pts);
        Table.Insert;
        TStr := ptTrim(Data.Strings[i+1]);
        Table.SetFieldByNameAsFloat('X',StrToFloat(TStr));
        TStr := ptTrim(Data.Strings[i+3]);
        Table.SetFieldByNameAsFloat('Y',StrToFloat(TStr));
        TStr := ptTrim(Data.Strings[i+5]);
        Table.SetFieldByNameAsFloat('Z',StrToFloat(TStr));
        Table.Post;
      end;
      inc(i);
   end;
   EndProgress;
   Table.Destroy;
   MessageToContinue('Pts extracted: ' + IntToStr(Pts));
end;


procedure TDemHandForm.ASCIItrimcolumns1Click(Sender: TObject);
var
   Infile,Outfile : TextFile;
   FName          : PathStr;
   Dir            : DirStr;
   bName          : NameStr;
   Ext            : ExtStr;
   Line1,Line2    : ShortString;
   i,FirstColToCut,Read,
   NumColsToCut   : integer;
   FilesWanted : TStringList;
   DefaultFilter : byte;
begin
   fName := '';
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   if GetMultipleFiles('file for column removal','*.*',FilesWanted,DefaultFilter) then begin
      for i := 0 to pred(FilesWanted.Count) do begin
        fName := FilesWanted.Strings[i];
        StatusBar1.Panels[1].Text := IntToStr(i) + '   ' + fName;
        assignFile(Infile,FName);
        reset(Infile);
        if (i=0) then begin
           readln(Infile,Line1);
           reset(Infile);

           FSplit(FName,Dir,BName,Ext);
           assignFile(Outfile,Dir + 'temptemp.tmp');
           rewrite(Outfile);
           FirstColToCut := 1;
           NumColsToCut := 2;
           repeat
              ReadDefault('# columns to cut',NumColsToCut);
              ReadDefault('First column to cut',FirstColToCut);
              Line2 := Line1;
              System.Delete(Line2,FirstColToCut,NumColsToCut);
           until AnswerIsYes(Line1 + MessLineBreak +Line2 + MessLineBreak + 'OK');
        end;
        Read := 0;
        repeat
            readln(Infile,Line1);
            if (Line1 <> '') then begin
               Delete(Line1,FirstColToCut,NumColsToCut);
               inc(Read);
               if (Read mod 50000 = 0) then begin
                  StatusBar1.Panels[0].Text := IntToStr(Read);
                  Application.ProcessMessages;
               end;
               writeln(Outfile,Line1);
            end;
        until eof(infile);
        StatusBar1.Panels[0].Text := '';
        StatusBar1.Panels[1].Text := '';
        closeFile(InFile);
        CloseFile(OutFile);
        DeleteFileIfExists(FName);
        SysUtils.RenameFile(Dir + 'temptemp.tmp',FName);
      end;
   end;
end;


procedure TDemHandForm.ASCIItrimlines1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList : tStringList;
   i,m     : integer;
   MenuStr : ShortString;
   DefaultFilter : byte;
   FilesWanted : TStringList;
begin
   if (fName = '') then FName := ProgramRootDir;
   FilesWanted := TStringList.Create;
   FilesWanted.Add(fName);
   DefaultFilter := 1;
   if GetMultipleFiles('ASCII files to insert header','*.*',FilesWanted,DefaultFilter) then begin
      for m := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted.Strings[m];
          ShowHourglassCursor;
          TheList := TStringList.Create;
           TheList.LoadFromFile(FName);
           for i := 0 to pred(TheList.Count) do begin
              MenuStr := TheList.Strings[i];
              TheList.Strings[i] := ptTrim(MenuStr);
           end;
           TheList.SaveToFile(FName);
           TheList.Free;
          ShowDefaultCursor;
      end;
   end;
end;


procedure TDemHandForm.ASCfiles1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;

procedure TDemHandForm.ASCII01Click(Sender: TObject);
begin
   ASCIIremovetabs1Click(Sender);
end;


procedure TDemHandForm.ASCIICRLF1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList : tStringList;
   FName2  : PathStr;
   inf     : file;
   OutF    : TextFile;
   Buffer  : array[1..2048] of AnsiChar;
   FSize,DSize,
   i,NumRead  : integer;                           
   MenuStr    : ShortString;
begin
   if (fName = '') then FName := ProgramRootDir;
   while GetFileFromDirectory('ASCII insure LF+CR','*.*',FName) do begin
      ShowHourglassCursor;
      InsureFileIsNotReadOnly(fName);
      FSize := GetFileSize(FName);
      if (FSize < InMemoryStringSizeLimit) then begin
         TheList := TStringList.Create;
         TheList.LoadFromFile(FName);
         TheList.SaveToFile(FName);
         MessageToContinue('Lines in file: ' + IntToStr(TheList.Count));
         TheList.Free;
      end
      else begin
         FName2 := MDTempDir + 'temp cr lf file.txt';
         CopyFile(FName,FName2);
         assignFile(inf,FName2);
         reset(inf,1);
         assignFile(outf,FName);
         rewrite(outf);
         MenuStr := '';
         DSize := 0;
         StartProgress('Insure CR/LF');
         while not(EOF(Inf)) do begin
            BlockRead(inf,Buffer,2048,NumRead);
            inc(DSize,NumRead);
            UpdateProgressBar(DSize/FSize);
            for i := 1 to NumRead do begin
               if Buffer[i] in [#10,#13] then begin
                  if MenuStr <> '' then writeln(outf,MenuStr);
                  MenuStr := '';
               end
               else MenuStr := MenuStr + Buffer[i];
            end;
         end;
         EndProgress;
         if (MenuStr <> '') then writeln(outf,MenuStr);
         CloseFile(Outf);
         CloseFile(inf);
      end;
      ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.ASCIIthin1Click(Sender: TObject);
label
   DoneIt;
const
   FileName : PathStr = '';
var
   Dfile,Outf : TextFile;
   ThinFactor,i,skip : integer;
   Count,Size        : LongInt;
   MenuStr : ShortString;
   AddSkips : boolean;
   NewFileName : PathStr;
begin
   if (FileName = '') then FileName := ProgramRootDir;
   if GetFileFromDirectory('to thin','*.*',FileName) then begin
      if not GetFileNameDefaultExt('new thinned subset','*.*',NewFileName) then exit;
      assignFile(Dfile,FileName);
      reset(Dfile);
      count := 0;
      StartCount('Lines in file');
      while not EOF(Dfile) do begin
         readln(DFile);
         inc(Count);
         if (Count mod 100 = 0) then UpdateCount(Count);
      end;
      EndCount;
      CloseFile(DFile);
      reset(DFile);
      FileName := '';
      assignFile(Outf,NewFileName);
      rewrite(OutF);
      ThinFactor := 2;
      ReadDefault('Thin factor for ' + IntToStr(Count) + ' line file', ThinFactor);
      Skip := 0;
      ReadDefault('Lines to skip at start',Skip);
      if (Skip > 0) then AddSkips := AnswerIsYes('Put skipped lines in output')
      else AddSkips := false;

      Size := Count;
      Count := 0;
      StartProgress('Thin');
      for i := 1 to Skip do begin
         readln(Dfile,MenuStr);
         if AddSkips then writeln(OutF,MenuStr);
      end;
      while not EOF(DFile) do begin
         for i := 1 to ThinFactor do begin
            if EOF(DFile) then goto DoneIt;
            readln(DFile,MenuStr);
            if (i=1) then writeln(OutF,MenuStr);
         end;
         inc(Count);
         if (Count mod 100 = 0) then UpdateProgressBar(Count* ThinFactor / Size);
      end;
      DoneIt:;
      closeFile(DFile);
      closeFile(Outf);
      EndCount;
   end;
end;


procedure TDemHandForm.ASCIIremovetabs1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList : tStringList;
   i,j,m     : integer;
   OutCh : AnsiChar;
   TStr,MenuStr : ShortString;
   DefaultFilter : byte;
   FilesWanted : TStringList;
begin
   if (Sender = ASCIIremovecommas1) then begin
      OutCh := ',';
      TStr := 'commas';
   end
   else if (Sender = ASCII01) then begin
      OutCh := #0;
      TStr := '#0';
   end
   else if (Sender = ASCIIRemoveQuotes1) then begin
      OutCh := '"';
      TStr := '"';
   end
   else if (Sender = ASCIIremoveleadingcharacter1) then begin
      TStr := ',';
      GetString('Leading string to remove',TStr,false,ReasonableTextChars);
   end
   else begin
      OutCh := #9;
      TStr := 'tabs';
   end;

   if (fName = '') then FName := ProgramRootDir;

   FilesWanted := TStringList.Create;
   FilesWanted.Add(fName);
   DefaultFilter := 1;
   if GetMultipleFiles('ASCII files to remove text','*.*',FilesWanted,DefaultFilter) then begin
      for m := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted.Strings[m];
          ShowHourglassCursor;
          if (Sender = ASCII01) then begin
             RemoveASCII0FromFile(fName);
          end
          else begin
             TheList := TStringList.Create;
             TheList.LoadFromFile(FName);
             for i := 0 to pred(TheList.Count) do begin
                MenuStr := TheList.Strings[i];
                if (Sender = ASCIIremoveleadingcharacter1) then begin
                   if (Copy(MenuStr,1,Length(TStr)) = TStr) then Delete(MenuStr,1,Length(TStr));
                end
                else begin
                   for j := 1 to length(MenuStr) do if (MenuStr[j] = OutCh) then MenuStr[j] := ' ';
                end;
                TheList.Strings[i] := MenuStr;
             end;
             TheList.SaveToFile(FName);
             TheList.Free;
          end;
          ShowDefaultCursor;
       end;
    end;
end;


procedure TDemHandForm.ASCIIreplaceheaderline1Click(Sender: TObject);
begin
   DataFileHeader(hmReplace);
end;

procedure TDemHandForm.ASCIIremovecommas1Click(Sender: TObject);
begin
   ASCIIremovetabs1Click(Sender);
end;


procedure TDemHandForm.ASCIIremoveleadingcharacter1Click(Sender: TObject);
begin
   ASCIIremovetabs1Click(Sender);
end;


procedure TDemHandForm.ASCIIremovelinesinsecondfile1Click(Sender: TObject);
var
   FName,FName2 : PathStr;
   TheList,TheMatches,TheResults : tStringList;
   OriginalLines,Removed : integer;
   I,j : Integer;
   aLine : shortstring;
begin
   FName := ProgramRootDir;
   if GetFileFromDirectory('ASCII sort & remove match (will not be saved)','*.*',FName) then begin
      fName2 := ExtractFilePath(fName);
      if GetFileFromDirectory('matches to remove','*.*',FName2) then begin

         ShowHourglassCursor;
         TheList := TStringList.Create;
         TheList.LoadFromFile(FName);
         OriginalLines := TheList.Count;

         TheMatches := TStringList.Create;
         TheMatches.LoadFromFile(FName2);
         TheMatches.Sort;

         TheResults := TStringList.Create;
         Removed := 0;
         for I := 0 to pred(TheList.Count) do begin
             aline := TheList.Strings[i];
             if TheMatches.Find(aline,j) then begin
                inc(Removed);
             end
             else begin
                TheResults.Add(aline);
             end;
         end;

         Petmar.DisplayAndPurgeStringList(TheResults,ExtractFilename(fName) + '  removed=' + IntToStr(Removed) + '  left=' + IntToStr(TheResults.Count));
         TheList.Free;
         TheMatches.Free;
         ShowDefaultCursor;
      end;
   end;

end;

procedure TDemHandForm.ASCIIremovelineswithsubstring1Click(Sender: TObject);
var
   FName : PathStr;
   aLine : ANSIstring;
   SubString : shortString;
   i,j : integer;
   TheList,NewList : tStringList;
   tfile : system.Text;

      procedure CheckLine;
      begin
         if StrUtils.AnsiContainsText(UpperCase(aLine),UpperCase(SubString)) then begin
            inc(j);
         end
         else begin
            NewList.Add(aLine);
         end;
      end;

begin
   FName := MainMapData;
   if GetFileFromDirectory('ASCII remove lines with substring','*.*',FName) then begin
      SubString := '';
      Petmar.GetString('substring to search for',SubString,false,ReasonableTextChars);
      NewList := TStringList.Create;
      ShowHourglassCursor;

      if false and (GetFileSize(fName) < InMemoryStringSizeLimit) then begin
         TheList := TStringList.Create;
         TheList.LoadFromFile(FName);
         for I := 0 to pred(TheList.Count) do begin
             aLine := TheList.Strings[i];
             CheckLine;
         end;
         TheList.Free;
      end
      else begin
         AssignFile(tFile,fName);
         reset(tFile);
         i := 0;
         while not eof(tFile) do begin
            readln(tfile,aLine);
            CheckLine;
            inc(i);
            if (i mod 1000 = 0) then begin
                StatusBar1.Panels[0].Text := 'Lines=' + IntToStr(i) + '  matches=' + IntToStr(j);
                Application.ProcessMessages;
            end;
            if (NewList.Count > 100000) then begin
               NewList.SaveToFile(MDtempdir + 'f_' + IntToStr(NewList.Count) + '.txt');
               NewList.Clear;
            end;
          end;
         CloseFile(tFile);
         StatusBar1.Panels[0].Text := '';
      end;
      Petmar.DisplayAndPurgeStringList(NewList,IntToStr(NewList.Count ) + ' lines; removed ' + IntToStr(j) + ' with '+ QuotedStr(Substring) + ' in ' + ExtractFileName(fName));
      ShowDefaultCursor;
   end;
end;

procedure TDemHandForm.ASCIIremovequotes1Click(Sender: TObject);
begin
   ASCIIremovetabs1Click(Sender);
end;


procedure TDemHandForm.IMGdirectorytoMDDEM1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;


procedure TDemHandForm.Ducknearshoresurveys1Click(Sender: TObject);
var
   fName : PathStr;
   TheData,Dirs : tStringList;
   j : integer;
begin
   {$IfDef RecordDuckProblems} WriteLineToDebugFile('TDemHandForm.Ducknearshoresurveys1Click in'); {$EndIf}
   FindMatchingFiles('C:\mapdata\duck_crab\','*.3d*',Dirs,6);
   StartProgress('3D to DBF');
   for j := 0 to pred(dirs.Count) do begin
      UpDateProgressBar(j/dirs.Count);
      fName := Dirs.Strings[j];
      {$IfDef RecordDuckProblems} WriteLineToDebugFile(fName); {$EndIf}
      TheData := tStringList.Create;
      TheData.LoadFromFile(fName);
      TheData.Insert(0,'SKIP LINE SURVEY_NO LAT LONG SKIP SKIP DISTANCE OFFLINE ELEV SKIP DATE TIME SECONDS');
      fName := ChangeFileExt(fName,DefaultDBExt);
      PetDBUtils.StringList2CSVtoDB(TheData,fName,true);
   end;
   EndProgress;
   {$IfDef RecordDuckProblems} WriteLineToDebugFile('TDemHandForm.Ducknearshoresurveys1Click out');{$EndIf}
end;


procedure TDemHandForm.KMLfiles1Click(Sender: TObject);
begin
   CleanOutDirectory(MainMapData + 'kml\');
end;

procedure TDemHandForm.Shapefile2Click(Sender: TObject);
var
   FName : PathStr;
begin
   if AnswerIsYes('Verify delete shape file') then begin
      fName := ExtractFilePath(LastDataBase);
      while GetFileFromDirectory('shape file to delete','*.shp',FName) do DeleteShapeFile(fName);
   end;
end;


procedure TDemHandForm.ASCIIsortremoveduplicates1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList: tStringList;
   OriginalLines : integer;
begin
   if (fName = '') then FName := ProgramRootDir;
   while GetFileFromDirectory('ASCII sort & duplicate removal (will not be saved)','*.*',FName) do begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      OriginalLines := TheList.Count;
      TheList.Clear;
      TheList.Sorted := true;
      TheList.Duplicates := dupIgnore;
      TheList.LoadFromFile(FName);
      Petmar.DisplayAndPurgeStringList(TheList,ExtractFilename(fName) + '  had duplicates=' + IntToStr(OriginalLines - TheList.Count) + '  n=' + IntToStr(TheList.Count));
      TheList.Free;
      ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.Maindatabase1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      ImportHarvardCentroids(true);
   {$EndIf}
end;


procedure TDemHandForm.MaskDEMs1Click(Sender: TObject);
var
   fName : PathStr;
   Limits : tGridLimits;
   FilesWanted : tStringList;
   i,MaskDEM,OnDEM : integer;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(LastDEMName);
   if GetMultipleFiles('DEM to use as mask',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
      fName := FilesWanted.Strings[i];
      LoadNewDEM(MaskDEM,fName,false);
      FilesWanted.Clear;
      FilesWanted.Add(LastDEMName);

      if GetMultipleFiles('DEMs to mask',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
         SafeMakeDir(ExtractFilePath(fName) + 'masked\');
         for i := 0 to pred(FilesWanted.Count) do begin
            StatusBar1.Panels[0].Text := IntToStr(i) + '/' + IntToStr(FilesWanted.Count);
            fName := FilesWanted.Strings[i];
            LoadNewDEM(OnDEM,fName,true);
            DEMGlb[OnDEM].SelectionMap.MaskFromSecondGrid(MaskDEM,msSecondMissing,false);
            FName := ExtractFilePath(fName) + 'masked\' + ExtractFileName(fname);
            Limits := DEMGlb[OnDEM].FullDEMGridLimits;
            DEMGlb[OnDEM].FilledGridBox(Limits);
            DEMGlb[OnDEM].WriteNewFormatDEM(Limits,fName);
            CloseSingleDEM(OnDEM);
         end;
      end;
      CloseSingleDEM(MaskDEM);
   end;
   FilesWanted.Destroy;
   StatusBar1.Panels[0].Text := '';
end;


procedure TDemHandForm.Specifyshift1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;

procedure TDemHandForm.AddlengthfieldtoDBFfile1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;

procedure TDemHandForm.Bynumberoffiles1Click(Sender: TObject);
var
   fName : PathStr;
   Dir : DirStr;
   bName : NameStr;
   Ext : extStr;
   Length,i,k,Nfiles : Integer;
   NewFiles : array[1..25] of TStringList;
   OldF,NewF : System.TextFile;
   FileContents : tStringList;
   FirstLine,TStr,BaseName : ShortString;
begin
   fName := '';
   if GetFileFromDirectory('file to split','*.*',FName) then begin
      if (Sender = ByNumberofFiles1) then begin
         NFiles := 12;
         ReadDefault('Number of files',NFiles);
         if (NFiles > 25) then NFiles := 25;

         FileContents := tStringList.Create;
         FileContents.LoadFromFile(fname);
         FSplit(Fname,Dir,bName,Ext);
         for i := 1  to NFiles do NewFiles[i] := tStringList.Create;
         for i := 1 to FileContents.Count do begin
            TStr := FileContents.Strings[pred(i)];
            NewFiles[succ(i mod NFiles)].Add(TStr);
         end;
         for i := 1 to NFiles do NewFiles[i].SaveToFile(Dir + bName + '-' + IntToStr(i) + Ext);
         for i := 1 to NFiles do NewFiles[i].Free;
         FileContents.Free;
      end
      else begin
         Length := 1000000;
         ReadDefault('Lines per file',Length);
         BaseName := 'Part';
         Petmar.GetString('file base name',BaseName,false,ReasonableTextChars);
         AssignFile(OldF,fName);
         reset(OldF);
         k := 1;
         ShowHourglassCursor;
         Readln(OldF,FirstLine);
         Memo1.Visible := true;

         repeat
            fName := ExtractFilePath(fName) + BaseName + IntToStr(k) + '.txt';
            Memo1.Lines.Add(fName);
            AssignFile(NewF,fName);
            rewrite(NewF);
            Writeln(NewF,FirstLine);
            for I := 1 to Length do begin
               Readln(OldF,TStr);
               Writeln(NewF,TStr);
            end;
            CloseFile(NewF);
            inc(K);
         until eof(OldF);
         CloseFile(OldF);
         Memo1.Lines.Add('completed');
      end;
   end;
   ShowDefaultCursor;
end;

procedure TDemHandForm.Bynumberoflines1Click(Sender: TObject);
begin
   Bynumberoffiles1Click(Sender);
end;


procedure TDemHandForm.Compressuncompress1Click(Sender: TObject);
begin
   {$IfDef ExTools}
   {$Else}
      Compress_form.StartCompression;
   {$EndIf}
end;

procedure TDemHandForm.AddlatlongfieldstoDBFfile1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;

procedure TDemHandForm.AddLineEndPointsClick(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;

procedure TDemHandForm.USGSgazetteerdatabase1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      ImportGazetteer(false);
   {$EndIf}
end;


procedure TDemHandForm.NCEPNCARReanalysiswinds1Click(Sender: TObject);
var
   fName : PathStr;
   i,j,k : integer;
   Line : ANSIString;
   TheField : ShortString;
   Table : tMyData;
   Filter : byte;
   FileList,
   inFile : tStringList;
   Long : float64;
begin
   fName := DBDir + 'global_winds' + DefaultDBExt;
   if not FileExists(fName) then begin
      CreateWindComponentTable(fName);
      Table := tMyData.Create(fName);
      for i := 1 to 73 do begin
         for j := 1 to 144 do begin
            Table.Insert;
            Table.SetFieldByNameAsFloat('LAT',90 - pred(i) * 2.5);
            Long := pred(j) * 2.5;
            LongitudeAngleInRange(Long);
            Table.SetFieldByNameAsFloat('LONG',Long);
            Table.Post;
         end;
      end;
   end
   else Table := tMyData.Create(fName);

   FileList := tStringList.Create;
   Filter := 1;
   if GetMultipleFiles('Wind vectors','CSV|*.csv|All files|*.*',FileList,Filter) then begin
      for k := 0 to pred(FileList.Count) do begin
         InFile := tStringList.Create;
         InFile.LoadFromFile(FileList.Strings[k]);
         TheField := PetDBUtils.OrigPickField(Table,'Field in ' + ExtractFileName(FileList.Strings[k]),[ftFloat]);
         Table.First;
         for i := 1 to 73 do begin
            Line := InFile.Strings[pred(i)];
            for j := 1 to 144 do begin
               Table.Edit;
               Table.SetFieldByNameAsFloat(TheField,StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(Line,',',true,true)));
               Table.Next;
            end;
         end;
         InFile.Free;
      end;
      FileList.Free;
   end;
   Table.Destroy;
end;


procedure TDemHandForm.NIMAgazetteerdatabase1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      ImportGazetteer(true);
   {$EndIf}
end;


procedure TDemHandForm.OGRDBFtoSQLite1Click(Sender: TObject);
var
   FilesWanted : tStringList;
   DefaultFilter : byte;
   fName : PathStr;
   DeleteFiles : boolean;
   i : integer;
begin
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRDBFtoSQLite1Click in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   DefaultFilter := 1;
   if GetMultipleFiles('DBF files','DBF|*.dbf',FilesWanted,DefaultFilter) then begin
      DeleteFiles := AnswerIsYes('Delete files after conversion (not recommended)');
      for I := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         {$IfDef RecordReformat} WriteLineToDebugFile(fName); {$EndIf}
         ConvertDBFtoSQLite(fName);
         if DeleteFiles then SysUtils.DeleteFile(fName);
      end;
   end;
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRDBFtoSQLite1Click out'); {$EndIf}
end;


procedure TDemHandForm.OGRDXFtoshapefile1Click(Sender: TObject);
var
   FilesWanted : tStringList;
   DefaultFilter : byte;
   fName : PathStr;
   Ext : ExtStr;
   i : integer;
begin
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRDXFtoshapefile1Click in'); {$EndIf}
   if IsGDALFilePresent(GDAL_ogr_Name) then begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(MainMapData);
      DefaultFilter := 1;
      if GetMultipleFiles('Vector Files','All formats|*.gpx;*.dbf;*.osm;*.gpkg;*.geojson;*.json|DXF|*.dxf|GPX|*.gpx|OSM|*.osm|GeoJSON|*.geojson;*.json',FilesWanted,DefaultFilter) then begin
         for I := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[i];
            Ext := UpperCase(ExtractFileExt(fName));
            {$IfDef RecordReformat} WriteLineToDebugFile(fName); {$EndIf}
            if (Ext = '.GPX') then GDAL_ConvertGPXToSHP(fName)
            else if (Ext = '.JSON') or   (Ext = '.GEOJSON') then begin
               GDAL_Convert_JSON(fName);
            end
            else GeneralConvertToWGS84Shapefile(fName);
         end;
      end;
   end;
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRDXFtoshapefile1Click out'); {$EndIf}
end;


procedure TDemHandForm.OGRmergeshapefiles1Click(Sender: TObject);
var
   NewFileName,fName : PathStr;
   sl,FilesWanted : tStringList;
   DefaultFilter : byte;
   k : integer;
begin
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRDXFtoshapefile1Click in'); {$EndIf}
   if IsGDALFilePresent(GDAL_ogr_Name) then begin
      NewFileName := ExtractFilePath(LastDataBase);
      DefaultFilter := 1;
      if GetFileNameDefaultExt('New file for merged shape files','Shapefile|*.shp',NewFileName) then begin
         FilesWanted := tStringList.Create;
         FilesWanted.Add(ExtractFilePath(NewFileName));
         if GetMultipleFiles('Shape files to merge','ShapeFiles|*.shp|',FilesWanted,DefaultFilter) then begin
            sl := tStringList.Create;
            sl.Add('REM copy shapefile 1');
            sl.Add(GDAL_ogr_Name + NewFileName + ' ' + FilesWanted.Strings[0]);
            for k := 1 to pred(FilesWanted.Count) do begin
               sl.Add('REM add shapefile ' + IntToStr(k) + '/' + IntToStr(pred(FilesWanted.Count)));
               fName := FilesWanted.Strings[k];
               sl.Add(GDAL_ogr_Name + ' -update -append ' + NewFileName + ' ' + FilesWanted.Strings[k]);
            end;
         end;
         FilesWanted.Free;
         EndBatchFile(MDTempDir + 'merge_shp.bat',sl);
      end;
   end;
end;


procedure TDemHandForm.OGRshapefilestoGKPG1Click(Sender: TObject);
var
   NewFileName,fName : PathStr;
   k : integer;
   FilesWanted : tStringList;
   DefaultFilter : byte;
   DeleteFiles : boolean;
   i : integer;
begin
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRshapefilestoGKPG1Click in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   DefaultFilter := 1;
   if GetMultipleFiles('shapte files','Shape files|*.shp',FilesWanted,DefaultFilter) then begin
      for I := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         GDAL_Convert_Shapefile_2_geopackage(fName);
      end;
   end;
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.OGRshapefilestoGKPG1Click out'); {$EndIf}
end;

procedure TDemHandForm.OpenTopography1Click(Sender: TObject);
var
   FileList,FilesWanted : tStringList;
   NumSucc,NumFail, i,j,f,NumAlreadyDone : Integer;
   Input,OutPath : PathStr;
   TStr,inName,OutName : AnsiString;
   DefFilter : byte;
begin
   inPut := '';
   DefFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   if GetMultipleFiles('Files to download','Text or csv*.txt;*.csv',FilesWanted,DefFilter) then begin
      for f := 0 to pred(FilesWanted.Count) do begin
         Input := FilesWanted.Strings[f];
         Memo1.Visible := true;
         Memo1.Lines.Add(TimeToStr(Now) + '  Download list ' + IntToStr(succ(f)) + '/' + IntToStr(FilesWanted.Count) + '  ' + ExtractFileNameNoExt(Input));
         Memo1.Lines.Add('');
         if FileExists(Input) then begin
            FileList := tStringList.Create;
            FileList.LoadFromFile(Input);
            OutPath := ExtractFilePath(Input) + ExtractFileNameNoExt(Input) + '\';
            SafeMakeDir(OutPath);
            Memo1.Visible := true;
            Memo1.Lines.Add(TimeToStr(Now) + ' start download, files= ' + IntToStr(FileList.Count));
            wmdem.SetPanelText(3,'Started: ' + TimeToStr(Now));
            repeat
              NumSucc := 0;
              NumFail := 0;
              NumAlreadyDone := 0;
              StartProgressAbortOption('Download list ' + IntToStr(succ(f)) + '/' + IntToStr(FilesWanted.Count));
              for i := 0 to pred(FileList.Count) do begin
                 UpdateProgressBar(pred(i)/FileList.Count);
                 wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(FileList.Count));
                 TStr := FileList.Strings[i];
                 InName := TStr;
                 if (InName <> '') then begin
                    j := length(TStr);
                    repeat
                       dec(j)
                    until TStr[j] = '/';
                    OutName := OutPath + Copy(TStr,j+1,Length(TStr)-j);
                    if FileExists(OutName) then begin
                       inc(NumAlreadyDone);
                    end
                    else begin
                       if DownloadFileFromWeb(InName,OutName) then begin
                          TStr := TStr + ' success';
                          inc(NumSucc);
                       end
                       else begin
                          TStr := TStr + ' failure ***** ' + InName;
                          inc(NumFail);
                       end;
                       Memo1.Lines.Add(IntToStr(I) + '/' + IntToStr(FileList.Count) + '  ' + TimeToStr(Now) + ' ' + TStr);
                    end;
                 end;
                 if WantOut then begin
                    Memo1.Lines.Add('Aborted');
                    break;
                 end;
              end;
              EndProgress;
              Memo1.Lines.Add('Done; downloads=' + intToStr(NumSucc) + '  failures=' + IntToStr(NumFail) + '  already done=' + IntToStr(NumAlreadyDone));
              Memo1.Lines.Add('');
              Memo1.Lines.Add('');
              if NumFail = 0 then File2Trash(InName);
            until Wantout or (NumFail = 0) or (not AnswerIsYes('Retry failures'));
            FileList.Free;
         end;
      end;
   end;
   FilesWanted.Free;
   wmdem.SetPanelText(3,'');
   wmdem.SetPanelText(2,'');
end;


procedure TDemHandForm.Pickmaplibrary1Click(Sender: TObject);
begin
   PickMapIndexLocation;
end;


procedure TDemHandForm.Plateboundaryfile1Click(Sender: TObject);
var
   fName : PathStr;
   TheList : tStringList;
   aline : String;
   Lat,Long : float32;
   ShapeFileCreator : tShapeFileCreation;
   i: Integer;

    procedure DoARecord;
    begin
       ShapeFileCreator.ProcessRecordForShapeFile;
    end;

begin
   fName := MainMapData;
   if GetFileFromDirectory('ASCII plate boundaries','*.*',FName) then begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      fName := changeFileExt(fName,'.shp');
      ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,15);
      for i := 0 to pred(TheList.Count) do begin
         aline := trim(TheList.Strings[i]);
         if (length(aline) = 2) then begin
            if (i > 0) then DoARecord;
            ShapeFileCreator.RecordName := aline;
         end
         else begin
            Lat := StrToFloat(Petmar_types.BeforeSpecifiedCharacterUnicode(aline,' ',true,true));
            Long := StrToFloat(aline);
            ShapeFileCreator.AddPointToShapeStream(Lat,Long);
         end;
      end;
      DoARecord;
      ShapeFileCreator.CloseShapeFiles;
      TheList.Free;
      ShowDefaultCursor;
   end;

end;

procedure TDemHandForm.PrepOSMfiles1Click(Sender: TObject);
begin
   PrepOSMFiles;
end;


procedure TDemHandForm.ToUTM1Click(Sender: TObject);
begin
    LASGeotoUTM1Click(Sender);
end;


procedure TDemHandForm.ASCIIrandomize1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList : tStringList;
begin
   if (fName = '') then FName := MainMapData;
   while GetFileFromDirectory('ASCII randomize','*.*',FName) do begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      RandomizeStringList(TheList);
      TheList.SaveToFile(FName);
      TheList.Free;
      ShowDefaultCursor;
   end;
end;

procedure TDemHandForm.ASCIIremoveafterdelimiter1Click(Sender: TObject);
var
   FName : PathStr;
   Substring : shortstring;
   ch : ANSIchar;
   str : ANSIstring;
   i : integer;
   TheList,NewList : tStringList;
begin
   FName := MainMapData;
   if GetFileFromDirectory('ASCII remove afte delimiter','*.*',FName) then begin
      SubString := '';
      Petmar.GetString('delimiter',SubString,false,ReasonableTextChars);
      ch := Substring[1];
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      NewList := TStringList.Create;
      for I := 1 to pred(TheList.Count) do begin
         str := TheList.Strings[i];
         str := Petmar_types.BeforeSpecifiedCharacterANSI(str,ch);
         NewList.Add(str);
      end;
      TheList.Free;
      Petmar.DisplayAndPurgeStringList(NewList,'Before delimiter ' + ExtractFileName(fName) + '  n=' + IntToStr(NewList.Count));
   end;
end;

procedure TDemHandForm.ASCIIremoveblanklines1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList : tStringList;
   i       : integer;
   MenuStr : ShortString;
begin
   if fName = '' then FName := ProgramRootDir;
   while GetFileFromDirectory('ASCII remove blank lines','*.*',FName) do begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      for i := pred(TheList.Count) downto 0 do begin
         MenuStr := TheList.Strings[i];
         if MenuStr = '' then TheList.Delete(i);
      end;
      TheList.SaveToFile(FName);
      TheList.Free;
      ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.IGERredistricting1Click(Sender: TObject);
var
   fName : PathStr;
begin
   {$IfDef ExTiger}
   {$Else}
      fName := 'C:\mapdata\0--current_projects\md_redistrict\';
      if GetFileFromDirectory('Block/block group 2000/2010 shapefile','tl_*.shp',FName) then DEMTiger.RedistrictTigerFiles(fName);
   {$EndIf}
end;


procedure TDemHandForm.Addcolors1Click(Sender: TObject);
begin
   {$IfDef Include2019datafusion} experimental_md.AddColorFromJoin(Memo1); {$EndIf}
end;


procedure TDemHandForm.Addfileextensions1Click(Sender: TObject);
begin
   Changefileextensions1Click(Sender);
end;

procedure TDemHandForm.Addfilenameasfield1Click(Sender: TObject);
//puts file name into the "NAME" DBF field, in case you want to merge shapefiles that do not indicate what they contain.
var
  FilesWanted : tStringList;
  DefaultFilter : byte;
  k,Max : integer;
  bName : PathStr;
  Table : tMyData;
begin
    FilesWanted := tStringList.Create;
    FilesWanted.Add(DBDir);
    DefaultFilter := 1;
    if GetMultipleFiles('DB files to add name field',DBNameMask,FilesWanted,DefaultFilter) then    begin
       ShowHourglassCursor;
       Max := 0;
       for k := 0 to pred(FilesWanted.Count) do begin
           bName := ExtractFileNameNoExt(FilesWanted.Strings[k]);
           if Length(bName) > Max then Max := length(bName);
       end;

       for k := 0 to pred(FilesWanted.Count) do begin
          bName := FilesWanted.Strings[k];
          Table := tMyData.Create(bName);
          Table.InsureFieldPresentAndAdded(ftString,'NAME',Max,0);
          bName := ExtractFileNameNoExt(FilesWanted.Strings[k]);
          Table.First;
          while not Table.Eof do begin
            Table.Edit;
            Table.SetFieldByNameAsString('NAME',bName);
            Table.Next;
          end;
          Table.Destroy;
       end;
    end;
    FilesWanted.Free;
    ShowDefaultCursor;
end;

procedure TDemHandForm.Addgroundclassifiedpoints1Click(Sender: TObject);
begin
   {$IfDef Include2019datafusion} AddGroundPoints(Memo1); {$EndIf}
end;

procedure TDemHandForm.LASfilesbysize1Click(Sender: TObject);
begin
   {$ifDef ExPointClouds}
   {$Else}
       Memo1.Visible := true;
       SubsetLasfiles('',Memo1);
   {$EndIf}
end;


procedure TDemHandForm.LASGeotoUTM1Click(Sender: TObject);


      procedure LasToLasTools;
      const
         StartDir : PathStr = '';
      label
         LetMeOutOfHere;
      var
         fName,OutName,PName : PathStr;
         lf :  tLAS_data;
         elevparams,
         params,params2 : Shortstring;
         i : integer;
         DefaultFilter : byte;
         DeleteOriginalFiles : boolean;
         bf,theFileNames : tStringList;
      begin
         pName := ProgramRootDir + 'lastools\bin\las2las.exe';

         if FileExists(pName) then begin
            Params := '';
            Params2 := '';
            elevparams := '';
            if (Sender = AssignProjection1) or (Sender = AssignbyEPSGandreprojecttoUTM1) then begin
               params2 := ' -epsg 26985';
               GetString('Assign input EPSG code',params2,false,ReasonableTextChars);
               if (Sender = AssignProjection1) then begin
                  //params := ' -sp83 OH_N -feet -elevation_feet  -target_meter -target_utm auto';
                  //params := ' -sp83 MD -meter -elevation_meter  -target_meter -target_utm auto';
                  params := '-target_epsg 26985';
                  GetString('Assign target projection parameters',params,false,ReasonableTextChars);
               end;
            end;

            if (Sender = ToUTM1) or (Sender = AssignprojectionUTM1) or (Sender = AssignbyEPSGandreprojecttoUTM1) then begin
               PickUTMzone(MDDef.DefaultUTMZone);
               if AnswerIsYes('Are elevations in feet') then elevparams := ' -elevation_feet';
            end;
            DefaultFilter := 1;
            fName := MainMapData;

            bf := tStringList.Create;
            theFileNames := tStringList.Create;
            if StartDir = '' then StartDir := LastLidarDirectory;
            theFileNames.Add(StartDir);

            if GetMultipleFiles('input LAS file','Lidar files|*.las;*.laz|*.laz|LAS files|*.las|LAZ files|*.laz',theFileNames,DefaultFilter) then begin
               DeleteOriginalFiles := AnswerIsYes('Delete original files');

               for I := 0 to pred(TheFileNames.Count) do begin
                  fName := theFileNames.Strings[i];

                  if (i=0) then begin
                     StartDir := ExtractFilePath(fName);
                     if StrUtils.AnsiContainsText(fName,' ') then begin
                        if not AnswerIsYes('Filename with space in file/path name will fail, continue anyway') then begin
                           goto LetMeOutOfHere;
                        end;
                     end;
                  end;

                  if (Sender = Assignprojection1) then begin
                     OutName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_assigned.las';
                  end
                  else if (Sender = AssignprojectionUTM1) then begin
                     OutName := (ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_utm.las');
                     params := '-meter -utm ' + IntToStr(MDDef.DefaultUTMZone) + MDDef.DefaultLatHemi;
                  end
                  else if (Sender = ReprojectSpecifiedtoGeo1) then begin
                     OutName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_geo.las';
                     params := '-target_epsg 4326';
                  end
                  else begin
                     if (Sender = LASGeotoUTM1) then begin
                        lf := tLAS_data.Create(FName);
                        if (lf.lasProjectionDefinition.LASProjection.ModelType = LasLatLong) then begin
                           MDDef.DefaultUTMZone := GetUTMZone(0.5 * (lf.LAS_LatLong_Box.xmax - lf.LAS_LatLong_Box.xmin));
                        end;
                        if lf.LAS_LatLong_Box.ymax > 0 then MDDef.DefaultLatHemi := 'N' else MDDef.DefaultLatHemi := 'S';
                        lf.Destroy;
                     end;
                     params := '-target_meter -target_utm ' +  IntToStr(MDDef.DefaultUTMZone) + MDDef.DefaultLatHemi;
                     OutName := (ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_utm.las');
                  end;
                  bf.Add('REM ' + IntToStr(i) + '/' + IntToStr(TheFileNames.Count));
                  bf.Add(pName +  ' -i ' + fName + ' -o ' + OutName + ' ' + params + Params2 + elevparams);
                  if DeleteOriginalFiles then bf.Add('del ' + fName);
                  if i = TheFileNames.Count div 2 then begin
                     EndBatchFile(MDTempDir + 'las2las1.bat',bf,false);
                     bf := tStringList.Create;
                  end;
              end;
              EndBatchFile(MDTempDir + 'las2las2.bat',bf,false);
            end;

           LetMeOutOfHere:;
            TheFileNames.Free;
         end
         else begin
            MessageToContinue('Option requires ' + pName);
         end;
      end;


begin
   {$ifDef ExPointClouds}
   {$Else}
      LasToLasTools;
   {$EndIf}
end;



procedure TDemHandForm.LASionfo1Click(Sender: TObject);
begin
   CallLasInfo;
end;

procedure TDemHandForm.lasLIB1Click(Sender: TObject);
begin
   laslibReproject(true);
end;

procedure TDemHandForm.laslibspecifiedEPSGtoUTM1Click(Sender: TObject);
begin
   laslibReproject(false);
end;


procedure TDemHandForm.ASCIIreverseorder1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList,ReverseList : tStringList;
   i     : integer;
   MenuStr : ShortString;
begin
   if (fName = '') then FName := ProgramRootDir;
   while GetFileFromDirectory('ASCII reverse order','*.*',FName) do begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      ReverseList := TStringList.Create;
      if AnswerIsYes('Retain first line') then begin
         MenuStr := TheList.Strings[0];
         ReverseList.Add(MenuStr);
         TheList.Delete(0);
      end;
      for i := pred(TheList.Count) downto 0 do begin
         MenuStr := TheList.Strings[i];
         ReverseList.Add(MenuStr);
      end;
      ReverseList.SaveToFile(FName);
      TheList.Free;
      ReverseList.Free;
     ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.AddXYZtoDBFfile1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;

procedure TDemHandForm.ADFdirectory1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;


procedure TDemHandForm.BatchRenameDEMfiles(Sender: TObject);
const
   DataPath : PathStr = '';
var
   fName,OutPath,OutName : PathStr;
   TheFiles : tStringList;
   TStr : shortString;
   i,j : integer;
   Dir : DirStr;
   bName : NameStr;
   Ext,OutF : ExtStr;

         procedure SaveIt;
         begin
            if (OutF = '.dem') then DEMGlb[j].SavePartOfDEMWithData(OutName)
            else DEMGlb[j].SaveAsGeotiff(OutName);
         end;

begin
   {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.ADFgrids1Click in'); {$EndIf}

   if (Sender = BILdirectorytoMDDEM1) then TStr := '*.bil'
   else if (Sender = IMGdirectorytoMDDEM1) then TStr := '*.img'
   else if (Sender = ASCfiles1) then TStr := '*.asc'
   else if (Sender = XTfiles1) then TStr := '*.txt'
   else if (Sender = FLTfiles1) then TStr := '*.flt'
   else if (Sender = IFfiles1) then TStr := '*.tif'
   else TStr := 'w001001.adf';

   if (DataPath = '') then DataPath := MainMapData;
   GetDOSPath('input DEMs with ' + TStr,DataPath);
   if (Sender = ASCfiles1) then OutPath := DataPath  //these will replace the ASC files
   else GetDOSPath('DEM output',OutPath);

   TheFiles := Nil;
   Memo1.Visible := true;
   {$IfDef RecordReformat} WriteLineToDebugFile('datapath=' + DataPath + '  seeking files, mask=' + TStr); {$EndIf}

   Petmar.FindMatchingFiles(DataPath,TStr,TheFiles,3);

   if (TheFiles.Count = 0) then begin
      {$IfDef RecordReformat} WriteLineToDebugFile('No files found'); {$EndIf}
   end
   else begin
      if (Sender <> ASCfiles1) then if AnswerIsYes('Geotiff format rather than MD DEM') then OutF := '.tif' else OutF := '.dem';

      for I := 0 to pred(TheFiles.Count) do begin
         fName := TheFiles.Strings[i];
         {$IfDef RecordReformat} WriteLineToDebugFile(fName); {$EndIf}
         FSplit(fName,Dir,bName,Ext);
         Memo1.Lines.Add(TimeToStr(Now) + ' Convert ' + IntToStr(i) + '/' + IntToStr(pred(TheFiles.Count)) + '  ' + ExtractFileName(fName));
         ApplicationProcessMessages;
         OutName := OutPath + bName + OutF;

         if (not FileExists(OutName)) then begin
            if StrUtils.AnsiContainsText(fName,'shd') then begin
            end
            else begin
               if (Sender <> BILdirectorytoMDDEM1) or FileExists(ChangeFileExt(fName,'.hdr')) then begin
                  LoadNewDEM(j,fName,false);
                  if (j <> 0) and (DEMGlb[j] <> Nil) then begin
                     if (Sender <> ASCfiles1) then SaveIt;  //ASC files converted during LoadNewDEM
                     CloseSingleDEM(j);
                  end;
               end;
            end;
         end;
      end;
      FreeAndNil(TheFiles);
      Memo1.Lines.Add('Done');
   end;
   Close;
end;


procedure TDemHandForm.Emailcleanup1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   TheList,Outlist : tStringList;
   aLine : ansiString;
   i : integer;
begin
   if (fName = '') then FName := ProgramRootDir;
   while GetFileFromDirectory('email cleaup','*.*',FName) do begin
      ShowHourglassCursor;
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      Outlist := tStringList.Create;
      Outlist.Sorted := true;
      Outlist.Duplicates := dupIgnore;
      for i := 0 to pred(TheList.Count) do begin
          aLine := TheList.Strings[i];
          if Aline <> '' then begin
          if ANSIContainsText(aLine,'[') then begin
             Aline := AfterSpecifiedCharacter(aline,'[');
             if Aline[length(Aline)] = ']' then Delete(aline,length(aline),1);
          end;
          OutList.Add(aline);
          end;
      end;
      OutList.SaveToFile(FName);
      TheList.Free;
      Outlist.Free;
      ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.ENVIHDRIMG1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
  MultiGridArray : tMultiGridArray;
  fName : PathStr;
  TiffImage : tTiffImage;
  Success : boolean;
  MapProjection : tMapProjection;
  RegVars : tRegVars;
begin
   fName := MainMapData;
   if GetFileFromDirectory('ENVI or Geotiff multiband imagery','*.img;*.tif;*.hdr',FName) then begin
      if FileExtEquals(fName,'.TIF') then begin
         TiffImage := tTiffImage.CreateGeotiff(MapProjection,RegVars,false,fName,success);
         MultiGridArray := tMultiGridArray.Create;
         MultiGridArray.LoadFromSingleFileGeotiff(TiffImage);
         MultiGridArray.Destroy;
         TiffImage.Destroy;
      end
      else if FileExtEquals(fName,'.HDR') or FileExtEquals(fName,'.IMG') then begin
         MultiGridArray := tMultiGridArray.Create;
         MultiGridArray.LoadFromENVI(fName);
         MultiGridArray.Destroy;
      end;
   end;
{$EndIf}
end;


procedure TDemHandForm.ShiftXYZcoordinates1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;

procedure TDemHandForm.ISOGravity1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      ImportISOGravity;
   {$EndIf}
end;



procedure TDemHandForm.Verticaldatums1Click(Sender: TObject);
var
   FilesWanted,outf : tStringList;
   i,DEM : integer;
   fName : PathStr;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(LastDEMName);
   if GetMultipleFiles('Vertical datum transform grids',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
     outf := tStringList.Create;
     outF.Add('FILENAME,IMAGE_DESC,LAT_HI,LAT_LOW,LONG_HI,LONG_LOW');
     for i := 0 to pred(FilesWanted.Count) do begin
        fName := FilesWanted.Strings[i];
        LoadNewDEM(DEM,fName,false);
        if ValidDEM(DEM) then begin
           ReplaceCharacter(DEMGlb[DEM].GeotiffImageDesc,',',';');
           outF.Add(ExtractFileName(FName) + ',' + DEMGlb[DEM].GeotiffImageDesc + ',' + RealToString(DEMGlb[DEM].DEMBoundBoxGeo.YMax,-12,-4) + ',' + RealToString(DEMGlb[DEM].DEMBoundBoxGeo.YMin,-12,-4) +
                ',' + RealToString(DEMGlb[DEM].DEMBoundBoxGeo.xMax,-12,-4) + ',' + RealToString(DEMGlb[DEM].DEMBoundBoxGeo.xMin,-12,-4));
           CloseSingleDEM(DEM);
        end;
     end;
   end;
   outf.SaveToFile(MDtempdir + 'vert_datums_test.txt');
   PetDButils.StringList2CSVtoDB(outF,MDtempdir + 'vert_datum_grids.dbf');
end;



procedure TDemHandForm.Viewsheds1Click(Sender: TObject);
begin
   CleanOutDirectory(SaveViewshedDir);
end;

procedure TDemHandForm.RANSAC1Click(Sender: TObject);
begin
   CloudCompare(false);
end;

procedure TDemHandForm.ranslatecoordsASCIIfile1Click(Sender: TObject);
begin
   {$IfDef Include2019datafusion} TranslateXY(Memo1); {$EndIf}
end;

procedure TDemHandForm.Rename1Click(Sender: TObject);
var
   FromName,ToName : PathStr;
begin
   FromName := MainMapData;
   while Petmar.GetFileFromDirectory('Shapefile to rename','*.shp',FromName) do begin
      ToName := ExtractFileName(FromName);
      if Petmar.GetFileNameDefaultExt('new name','*.shp',ToName) then begin
         RenameShapeFile(FromName,ToName);
      end;
   end;
end;


procedure TDemHandForm.RenameSF3fields1Click(Sender: TObject);
var
   fName,tName : PathStr;
   Table : tMyData;
   FieldsRenamed,i : integer;
   OldFieldName,NewFieldName,
   NewName : ShortString;
   FieldsInDB : tStringList;
begin
   fName := DBDir;
   tName := ProgramRootDir + 'DP_TableDescriptions' + DefaultDBExt;
   if GetFileFromDirectory('file with name subsitutions',DefaultDBMask,tName) and GetFileFromDirectory('file to rename fields',DefaultDBMask,FName) then begin
      Table := tMyData.Create(fName);
      PetDBUtils.GetFields(Table,AllVis,[ftString,ftInteger,ftSmallInt,ftFloat],FieldsInDB,true);
      Table.Destroy;
      FieldsRenamed := 0;
      Table := tMyData.Create(tName);
      OldFieldName := 'CENS_NAME';
      if (Not Table.FieldExists(OldFieldName)) then OldFieldName := OrigPickField(Table,'Field name to replace',[ftString]);
      NewFieldName := 'DBF_NAME';
      if (Not Table.FieldExists(NewFieldName)) then NewFieldName := OrigPickField(Table,'Field name to replace with',[ftString]);

      for i := 0 to pred(FieldsInDB.Count) do begin
         Table.ApplyFilter(OldFieldName + '=' + QuotedStr(ptTrim(FieldsInDB.Strings[i])));
         if (Table.RecordCount = 1) then begin
            NewName := ptTrim(Table.GetFieldByNameAsString(NewFieldName));
            if (NewName <> '') then begin
               RenameDBaseField(fName,FieldsInDB.Strings[i],NewName);
               inc(FieldsRenamed);
            end;
         end;
      end;
      Table.Destroy;
      MessageToContinue('Fields renamed: ' + IntToStr(FieldsRenamed));
   end;
end;

procedure TDemHandForm.Repairheaders1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := ExtractFilePath(LastDatabase);
   while GetFileFromDirectory('dbase table to repair field names',DefaultDBMask,FName) do begin
      RepairDBaseFieldNames(fName);
   end;
end;


procedure TDemHandForm.RepairSHPandSHXboundingboxes1Click(Sender: TObject);
begin
   RepairShapeFileBoundingBoxes;
end;


procedure TDemHandForm.ReprojectSpecifiedtoGeo1Click(Sender: TObject);
begin
   LASGeotoUTM1Click(Sender);
end;

procedure TDemHandForm.Resizeimages1Click(Sender: TObject);
var
   fName   : PathStr;
   theFileNames : tStringList;
   i,x,y : integer;
   DefaultFilter : byte;
   inBMP,outBMP : tMyBitmap;
begin
   theFileNames := tStringList.Create;
   DefaultFilter := 1;
   if GetMultipleFiles('input images','Images|*.jpg;*.png;*.bmp',theFileNames,DefaultFilter) then begin
      x := 7131;
      y := 8851;
      ReadDefault('width new bitmaps',x);
      ReadDefault('height new bitmaps',y);
      StartProgress('Resize');
      for I := 0 to pred(theFileNames.Count) do begin
         UpdateProgressBar(i/theFileNames.Count);
         fName := theFileNames.Strings[i];
         inBMP := PetImage.LoadBitmapFromFile(fName);

         {$IfDef RecordProblems} WriteLineToDebugFile(fName + ',' + IntToStr(inBMP.Width) +  ',' + IntToStr(inBMP.Height) ); {$EndIf}

         CreateBitmap(OutBMP,x,y);
         OutBMP.canvas.StretchDraw(Rect(0,0,pred(x),pred(y)),inbmp);
         inBMP.Free;
         PetImage.SaveBitmap(OutBmp,fName);
         outBMP.Free;
      end;
      EndProgress;
   end;
   theFileNames.Free;
end;


procedure TDemHandForm.GeoJSONP1Click(Sender: TObject);
begin
   {$ifDef ExPointClouds}
   {$Else}
      LAS2GeoJSON;
   {$EndIf}
end;


procedure TDemHandForm.ANSIfilecleanup1Click(Sender: TObject);
var
   Tstrl,FNames : tStringList;
   fName : PathStr;
   DefaultFilter : byte;
   i : integer;
begin
   FNames := tStringList.Create;
   DefaultFilter := 1;
   if GetMultipleFiles('for ANSI cleanup','files|*.*',FNames,DefaultFilter) then begin
      for I := 0 to pred(fNames.Count) do  begin
         fName := fNames.Strings[i];
         Tstrl := tStringList.Create;
         TStrl.LoadFromFile(fName);
         TStrl.SaveToFile(fName);
         TStrl.Free;
      end;
   end;
   fNames.Free;
end;


procedure TDemHandForm.Cardfileimport1Click(Sender: TObject);
const
   MaxFields = 100;
var
   fName,fName2  : PathStr;
   inFile : textFile;
   tv : float64;
   oTable,
   fTable : tMyData;
   i,j,OnField : integer;
   Line,TStr : AnsiString;
   CreateDataBase : tCreateDataBase;
   Start,Len,Decs : array[1..MaxFields] of integer;
   fType,fldName : array[1..MaxFields] of ShortString;
begin
   fName := '';
   if GetFileFromDirectory('file to import','*.*',FName) then begin
      assignFile(InFile,fName);
      reset(InFile);
      fName2 := ChangeFileExt(fName,DefaultDBExt);
      fName := ChangeFileExt(fName,'_format' + DefaultDBExt);
      if FileExists(fName) or GetFileFromDirectory('file format',DefaultDBMask,FName) then begin
         fTable := tMyData.Create(fName);
         CreateDataBase := tCreateDataBase.Create(fName2);
         with CreateDataBase do begin
            onField := 0;
            while not fTable.Eof do begin
               inc(onField);
               Decs[OnField] := fTable.GetFieldByNameAsInteger('DECIMALS');
               Start[OnField] := fTable.GetFieldByNameAsInteger('START');
               len[OnField] := SUCC(fTable.GetFieldByNameAsInteger('END') - fTable.GetFieldByNameAsInteger('START'));
               if Decs[OnField] > 0 then inc(len[OnField]);
               fldName[OnField] := fTable.GetFieldByNameAsString('VARIABLE');
               fType[OnField] := fTable.GetFieldByNameAsString('TYPE');
               AddAField(fldName[OnField],ftFromString(fType[OnField]),Len[OnField],Decs[OnField]);
               fTable.Next;
            end;
            WriteCorrectHeader;
            Destroy;
         end;
         fTable.Destroy;
         PetDBUtils.RepairDBaseFieldNames(fName2);
         oTable := tMyData.Create(fName2);

//1827 823 600-1655  8382 0106    10        1NL 1325 26

         ShowHourglassCursor;
         while not eof(InFile) do begin
            Readln(Infile,Line);
            oTable.Insert;
            for i := 1 to OnField do  begin
               TStr := Copy(Line,Start[i],Len[i]);
               if (ptTrim(TStr) <> '') then  begin
                  if (Decs[i] > 0) then begin
                     if (ptTrim(TStr) = '0') then oTable.SetFieldByNameAsFloat(fldName[i],0)
                     else begin
                         tv := strToFloat(TStr);
                         for j := 1 to Decs[i] do tv := 0.1 * tv;
                         oTable.SetFieldByNameAsFloat(fldName[i],tv);
                     end;
                  end
                  else begin
                     oTable.SetFieldByNameAsString(fldName[i],tStr);
                  end;
               end;
            end;
            oTable.Post;
         end;
         oTable.Destroy;
         ShowDefaultCursor;
      end;
      closeFile(InFile);
   end;
end;


procedure TDemHandForm.Changefileextensions1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   DefaultFilter : byte;
   fName,fName2 : PathStr;
   Ext : ANSIString;
   i : integer;
begin
   FilesWanted := TStringList.Create;
   DefaultFilter := 1;
   if GetMultipleFiles('files to change extension','*.*',FilesWanted,DefaultFilter) then  begin
       Ext := '';
       Petmar.GetString('New Extension',Ext,false,ReasonableTextChars);
       for i := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted.Strings[i];
          if (Sender = Addfileextensions1) then fName2 := fName + Ext
          else fName2 := ChangeFileExt(fName,Ext);
          SysUtils.RenameFile(fName,fName2);
       end;
       FilesWanted.Free;
   end;
end;


procedure TDemHandForm.ChangeUTMzone1Click(Sender: TObject);
begin
   IrishgridtoUTM1Click(Sender);
end;


procedure TDemHandForm.Chromelist1Click(Sender: TObject);
var
   FileList : tStringList;
   i : Integer;
   Input : PathStr;
   pName : AnsiString;
begin
   inPut := '';
   if GetFileFromDirectory('Files to download','*.txt;*.csv',InPut) then begin
      FileList := tStringList.Create;
      FileList.LoadFromFile(Input);
      if (Sender = Chromelist1) then pName := 'chrome'
      else pName := '"C:\Program Files (x86)\Internet Explorer\iexplore.exe"';
      for i := 0 to pred(FileList.Count) do begin
         ExecuteFile(pName,FileList.Strings[i],'')
      end;
      FileList.Free;
   end;
end;


procedure TDemHandForm.Icesat1Click(Sender: TObject);
begin
   IcesatPhotonConvert(Memo1);
end;


procedure TDemHandForm.ICOADSLMRF1Click(Sender: TObject);
var
   aRec : array[0..68] of byte;
   inf : file;
   DefaultFilter : byte;
   i : integer;
   Long,Value : float64;
   Table1 : tMyData;
   fName : PathStr;
   FilesWanted : tStringList;

      function Decode(Offset,Bits : integer) : cardinal;
      begin
         Move(arec[Offset div 8],Result,4);
         Int4Swap(Result);
         Result := Result shl (Offset mod 8);
         Result := Result shr ((32 - Bits));
      end;

begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add('C:\new\old_weather\1863.apr');
   DefaultFilter := 0;
   Memo1.Visible := true;
   fName := DBDir + 'icoads' + DefaultDBExt;
   MakeICOADSTable(fName);
   Table1 := tMyData.Create(fName);
   if GetMultipleFiles('ICOADS','Any file|*.*',FilesWanted,DefaultFilter) then  begin
      for i := 0 to pred(FilesWanted.Count) do  begin
         Memo1.Lines.Add(FilesWanted.Strings[i]);
         assignFile(inf,FilesWanted.Strings[i]);
         reset(inf,64);
         while not EOF(inf) do begin
            BlockRead(inf,aRec,1);
            Table1.Insert;
            Table1.SetFieldByNameAsInteger('YEAR',1769 + Decode(26,8));
            Table1.SetFieldByNameAsInteger('MONTH',Decode(34,4));
            Table1.SetFieldByNameAsInteger('DAY',Decode(38,5));
            Table1.SetFieldByNameAsFloat('LAT',-90.01 + 0.01 * Decode(75,15));
            Long := -1 + 0.01 * Decode(59,16);
            if (Long > 180) Then Long := Long - 360;
            Table1.SetFieldByNameAsFloat('LONG',Long);
            Value := -100 + 0.1 * Decode(229,11);
            if (Value > -50) then Table1.SetFieldByNameAsFloat('SST',Value);
            Table1.SetFieldByNameAsFloat('WIND_SPEED',-1 + 0.1 * Decode(150,10));
            Table1.SetFieldByNameAsFloat('WIND_DIR',Decode(137,9));
            Value := -1 + 0.5 * Decode(281,7);
            if (Value > -0.5) then Table1.SetFieldByNameAsFloat('WAVE_HT',Value);
            Value := -1 + 0.5 * Decode(299,9);
            if (Value > -0.5) then Table1.SetFieldByNameAsFloat('SWELL_HT',Value);
            Table1.Post;
         end;
         CloseFile(inf);
      end;
   end;
   FilesWanted.Free;
   Table1.Destroy;
end;


procedure TDemHandForm.IElist1Click(Sender: TObject);
begin
   Chromelist1Click(Sender);
end;

procedure TDemHandForm.IFfiles1Click(Sender: TObject);
begin
   BatchRenameDEMfiles(Sender);
end;

procedure TDemHandForm.CreateDBFfile1Click(Sender: TObject);
var
   FileNames : tStringList;
   DefaultFilter : byte;
   i,j : integer;
   fname,tName : PathStr;
   Table : tMyData;
   aShapeFile : tShapefile;
   Success : boolean;
begin
   FileNames := tStringList.Create;
   FileNames.Add(DBDir);
   DefaultFilter := 0;
   if GetMultipleFiles('Shapefile','shapefile|*.shp',FileNames,DefaultFilter) then begin
      StartProgress('DBF');
      for i := 0 to pred(FileNames.Count) do begin
          UpDateProgressBar(i/ FileNames.Count);
          fName := FileNames.Strings[i];
          tName := ChangeFileExt(fName,DefaultDBExt);
          if not FileExists(tName) then begin
            aShapeFile := tShapefile.Create(fName, success);
            MakeDummyRecordTable(tName);
            Table := tMyData.Create(tName);
            for j := 1 to aShapeFile.NumRecs do begin
               Table.Insert;
               Table.SetFieldByNameAsInteger('RECORD',j);
               Table.Post;
            end;
            Table.Destroy;
            aShapeFile.Destroy;
          end;
      end;
      EndProgress;
      FileNames.Free;
   end;
end;

procedure TDemHandForm.Createexternaldrive1Click(Sender: TObject);
begin
   {$IfDef ExIndexes}
   {$Else}
      CreateMapLibrary(Memo1);
   {$EndIf}
end;

procedure TDemHandForm.CreateSHXfilesforDBFfiles1Click(Sender: TObject);
var
   DefaultFilter : byte;
   FileNames : tStringList;
   GISNum,i : integer;
begin
   FileNames := tStringList.Create;
   FileNames.Add('');
   if GetMultipleFiles('dBase file','dBase file|*.dbf',FileNames,DefaultFilter) then begin
      for i := 0 to pred(FileNames.Count) do begin
         if OpenNumberedGISDataBase(GISNum,FileNames.Strings[i]) then begin
            GISdb[GISnum].SavePointShapeFile(false);
            CloseAndNilNumberedDB(GISnum);
         end;
      end;
   end;
   FileNames.Destroy;
end;


procedure TDemHandForm.CSVmergefiles1Click(Sender: TObject);
var
   FNames : tStringList;
   OutName : PathStr;
   DefaultFilter : byte;
begin
   FNames := tStringList.Create;
   OutName := '';
   DefaultFilter := 1;
   if GetMultipleFiles('CSV to merge','CSV files|*.txt;*.csv',FNames,DefaultFilter) then begin
      MergeCSVFiles(Fnames,OutName);
   end;
end;


procedure TDemHandForm.CSVputcommasaroundfields1Click(Sender: TObject);
var
   FName : PathStr;
   aLine : ANSIstring;
   i : integer;
   OldList,NewList: tStringList;
begin
   FName := MainMapData;
   if GetFileFromDirectory('Quotes around fields','*.*',FName) then begin
      NewList := TStringList.Create;
      ShowHourglassCursor;
      OldList := TStringList.Create;
      OldList.LoadFromFile(fName);
      OldList.Text := StringReplace(OldList.Text, ',', '","', [rfReplaceAll]);
      for I := 0 to pred(OldList.Count) do begin
         aline := '"' + OldList.Strings[i] + '"';
         NewList.Add(aline);
      end;
      Petmar.DisplayAndPurgeStringList(NewList,IntToStr(NewList.Count ) + ' lines with');
      ShowDefaultCursor;
   end;
end;


procedure TDemHandForm.ArbitrarytoLatLong1Click(Sender: TObject);
begin
   Conictolatlong1Click(Sender);
end;


procedure TDemHandForm.Adjustseriescolorusage1Click(Sender: TObject);
begin
   AdjustIntegratedDataBaseSeries;
   Close;
end;


procedure TDemHandForm.ASCIIinsertheaderline1Click(Sender: TObject);
begin
   DataFileHeader(hmInsert);
end;

procedure TDemHandForm.ASCIIinsertlinenumbers1Click(Sender: TObject);
var
   FName : PathStr;
   TheList,NewList : tStringList;
   i : integer;
begin
   FName := MainMapData;
   if GetFileFromDirectory('ASCII line numbers','*.*',FName) then begin
      TheList := TStringList.Create;
      TheList.LoadFromFile(FName);
      NewList := TStringList.Create;
      for I := 1 to (TheList.Count) do begin
         NewList.Add(IntegerToString(i,5) + '--' + TheList.Strings[pred(i)]);
      end;
      TheList.Free;
      Petmar.DisplayAndPurgeStringList(NewList,'Line numbers for ' + ExtractFileName(fName));
   end;
end;

procedure TDemHandForm.ASCIImergefiles1Click(Sender: TObject);
const
   FName : PathStr = '';
var
   infile,outfile : tStringList;
   i,m,start     : integer;
   DefaultFilter : byte;
   fName2 : PathStr;
   HeaderLine : boolean;
   FilesWanted : TStringList;
begin
   if (fName = '') then FName := ProgramRootDir;
   FilesWanted := TStringList.Create;
   FilesWanted.Add(fName);
   DefaultFilter := 1;
   if GetMultipleFiles('ASCII files to merge','*.*',FilesWanted,DefaultFilter) then begin
      outfile := tStringList.Create;
      HeaderLine := AnswerIsYes('Header line to skip in all but first file');
      if Petmar.GetFileNameDefaultExt('new name','*.txt',fName2) then begin
          ShowHourglassCursor;
          if AnswerIsYes('Operation in memory') then begin
            for m := 0 to pred(FilesWanted.Count) do begin
                fName := FilesWanted.Strings[m];
                infile := tStringList.Create;
                inFile.LoadFromFile(fName);
                if HeaderLine then begin
                   if (m = 0) then start := 0 else start := 1;
                end
                else Start := 0;
                for i := start to pred(infile.Count) do begin
                    outfile.Add(infile.Strings[i]);
                end;
                infile.Free;
              end;
             OutFile.SaveToFile(fname2);
             Outfile.Free;
          end
          else begin
             MessageToContinue('not yet implemented');
          end;
      end;
      ShowDefaultCursor;
      FilesWanted.Free;
   end;
end;

procedure TDemHandForm.ASCIIfindlineswithsubstring1Click(Sender: TObject);
var
   FName : PathStr;
   aLine : ANSIstring;
   SubString,SecondString : shortString;
   i,j : integer;
   RetainHeader : boolean;
   NewList : tStringList;
   tfile : system.Text;
begin
   FName := MainMapData;
   if GetFileFromDirectory('ASCII find substring','*.*',FName) then begin
      RetainHeader := AnswerIsYes('Retain header line');
      SubString := '';
      Petmar.GetString('substring to search for',SubString,false,ReasonableTextChars);
      SecondString := '';
      Petmar.GetString('second substring to search for',SecondString,false,ReasonableTextChars);
      NewList := TStringList.Create;
      ShowHourglassCursor;

      AssignFile(tFile,fName);
      reset(tFile);
      i := 0;
      while not eof(tFile) do begin
         readln(tfile,aLine);
         if ( (i=0) and RetainHeader) then  NewList.Add(aLine)
         else begin
            if StrUtils.AnsiContainsText(UpperCase(aLine),UpperCase(SubString)) then begin
               if (SecondString = '') or StrUtils.AnsiContainsText(UpperCase(aLine),UpperCase(SecondString)) then begin
                  NewList.Add(aLine);
                  inc(j);
               end;
            end;
         end;
         inc(i);
         if (i mod 1000 = 0) then begin
             StatusBar1.Panels[0].Text := 'Lines=' + IntToStr(i) + '  matches=' + IntToStr(j);
             Application.ProcessMessages;
         end;
         if (NewList.Count > 100000) then begin
            NewList.SaveToFile(MDtempdir + 'f_' + IntToStr(NewList.Count) + '.txt');
            NewList.Clear;
         end;
       end;
      CloseFile(tFile);
      StatusBar1.Panels[0].Text := '';
      Petmar.DisplayAndPurgeStringList(NewList,IntToStr(NewList.Count ) + ' lines with ' + QuotedStr(Substring) + ' in ' + ExtractFileName(fName));
      ShowDefaultCursor;
   end;
end;


procedure InitializeDEMHandW;
begin
   {$IfDef MessageStartUpUnitProblems} MessageToContinue('DEMhandw initialization'); {$EndIf}
   XYZName := '';
   LastCompressedFile := 'c:\mapdata\';
end;


initialization
   InitializeDEMHandW;
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demhandw in',true); {$EndIf}
   {$IfDef RecordSatProblems} WriteLineToDebugFile('RecordSatProblems active in demhandw'); {$EndIf}
   {$IfDef RecordImportProblems} WriteLineToDebugFile('RecordImportProblems active in demhandw'); {$EndIf}
   {$IfDef RecordMergeProblems} WriteLineToDebugFile('RecordMergeProblems active in demhandw'); {$EndIf}
   {$IfDef RecordHandlingLeak} WriteLineToDebugFile('RecordHandlingProblems active in demhandw'); {$EndIf}
   {$IfDef RecordReformat} WriteLineToDebugFile('RecordReformat active in demhandw'); {$EndIf}
   {$IfDef RecordShapeFileContents} WriteLineToDebugFile('RecordShapeFileContents active in demhandw'); {$EndIf}
   {$IfDef RecordGAZProblems} WriteLineToDebugFile('RecordGAZProblems active in demhandw'); {$EndIf}
   {$IfDef RecordGMTConvert} WriteLineToDebugFile('RecordGAZProblems active in demhandw'); {$EndIf}
   {$IfDef RecordTDemHandFormFormClose} WriteLineToDebugFile('RecordTDemHandForm active in demhandw'); {$EndIf}
   {$IfDef RecordDuckProblems} WriteLineToDebugFile('RecordDuckProblems active in demhandw'); {$EndIf}
   {$IfDef RecordSOESTtides} WriteLineToDebugFile('RecordSOESTtides active in demhandw'); {$EndIf}
   {$IfDef RecordGDAL} WriteLineToDebugFile('RecordGDAL active in demhandw'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demhandw out'); {$EndIf}
end.





