{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}

unit las_files_grouping;

{$I nevadia_defines.inc}

{$IFDEF DEBUG}
   //{$Define NoParallelFor} //used to debug only
   {$Define NoInline}
{$ENDIF}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordLASMemoryAlocations}
      //{$Define RecordLASfiles}
      //{$Define RecordLASplot}
      //{$Define RecordLASexport}
      //{$Define RecordListFilesProcessed}
      //{$Define BasicOpens}
      //{$Define RecordLASKML}
      //{$Define RecordLASexport}
      //{$Define RecordMergeLASfilesDetailed}
      //{$Define RecordMergeLASfiles}
      //{$Define TimeMergeLAS}
      //{$Define RecordCreateLASfiles}
      //{$Define RecordCreateEveryFile}
      //{$Define RecordLASheader}
      //{$Define RecordLASheaderKeys}
      //{$Define RecordLASprojection}
      //{$Define RecordLASColors}
      //{$Define RecordLASHist}
      //{$Define RecordLASHistFull}
      //{$Define RecordLAS_subset}
   {$ENDIF}
{$EndIf}


interface

uses
   SysUtils,StrUtils,Classes, System.Threading,System.SyncObjs, System.UITypes, System.UIConsts,

//needed for inline of core DB functions
   Data.DB,Petmar_db,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end for inline of the core DB functions

   {$IfDef VCL}
      Graphics,Forms,DEMMapf,DEMLOSW,Winapi.Windows,
   {$EndIf}

   {$IfDef ExFMX3D}
      FMX.Graphics,FMX.Controls, FMX.Types,System.Types,
   {$EndIf}

   nevadia_main,
   Petmar_types,PetImage,PetMath,Petmar,
   DEMESRIShapeFile,
   DEMDefs,BaseMap,DEMMapDraw;


const
   MaxLASreturns = 15;
type
   tCloudStats = (csLoadStats,csReportStats);
type
   tLas_files = class
      private
      public
         LAS_fnames : tStringList;
         MinTime,MaxTime : double;
         MinZ,MaxZ,
         MinPointDensity,
         MaxPointDensity,
         AveragePointDensity,
         PointDensity,
         MinGPStime,MaxGPSTime,TranslateX,TranslateY,TranslateZ,ScaleUp : float64;
         MIN_INTEN,MAX_INTEN,INTEN_1,INTEN_99,
         NIR_1,NIR_99,RED_1,RED_99,GREEN_1,GREEN_99,BLUE_1,BLUE_99,
         MIN_RED,MAX_RED,MIN_GREEN,MAX_GREEN,MIN_BLUE,MAX_BLUE,MIN_NIR,MAX_NIR : word;
         LasExportThinFactor,
         UTMZone : integer;
         LatHemi : ANSIchar;
         TotalCloudPts : int64;
         UTMfiles,
         ShowLASProgress,
         HasIntensity,
         HasClassification,
         HasRGB,
         HasTime,
         HasPointID,
         HasUserData,
         HasScanAngle,
         HasReturnNumbers : boolean;
         CloudDir,
         CloudName : PathStr;
         UTMBBox,
         GeoBBox,
         CutBBox   : sfBoundBox;
         Cloud_ls  : tLidarDataSettings;
         constructor Create;
         destructor Destroy;
         function CloudStats : shortstring;
         function IndexTableName : PathStr;

         procedure FindCloudExtent;
         procedure RemoveTilesNotOnMap(BaseMapDraw : tMapDraw);
         procedure ElevRangeOnMap(BaseMapDraw : tMapDraw; var MinZ,MaxZ : float64);
         procedure RGBPalette;

         {$IfDef VCL}
            procedure SubdivideLasPointsByClassification;
            procedure ZeroLessUsefulFields;
            procedure SetClassificationToZero(var NewName : PathStr);
            procedure ElevationHistogram(BaseMap : tMapForm = Nil);
            procedure CloudStatistics(TileTable : boolean; CloudStats : tCloudStats; DoText : boolean = true; DoGraph : boolean = true);
            function MergeLasPoints(MergeLas : tMergeLas; BaseMap : tMapForm; var NewName : PathStr; Mask : tMyBitmap = Nil) : boolean;
            function OpenGLLasPoints(var NewName : PathStr; BaseMap : tMapForm; Mask : tMyBitmap = Nil) : boolean;
            function ExportBinary(MergeLas : tMergeLas; BaseMap : tMapForm; Layer : integer; var GeometryFName,ColorsFName : PathStr; ExportFilter : tLASClassificationCategory = lccAll) : boolean;

            procedure KMLLasPoints(InBoundBox : sfBoundBox;  ExportMode : tExportMode; NewName : PathStr = '');
            procedure ExtractGroundPoints(var NewName : PathStr);
         {$EndIf}
   end;

   tUsePC    = array[1..MaxClouds] of boolean;
   tLasFiles = array[1..MaxClouds] of tLas_files;


implementation


uses
   {$IfDef VCL}
      BaseGraf,
      Thread_Timers,
      Slicer_3D,
      KML_Creator,
      DEMDatabase,
   {$EndIf}

   las_lidar,
   PetDBUtils,
   DEMCoord, DEMDef_routines,
   Make_tables;


function tLas_files.IndexTableName : PathStr;
begin
   Result :=  ExtractFilePath(LAS_fNames[0]) + LastSubDir(ExtractFilePath(LAS_fNames[0])) + '_lidar_tile_index.dbf';
end;


function tLas_files.ExportBinary(MergeLas : tMergeLas; BaseMap : tMapForm; Layer : integer; var GeometryFName,ColorsFName : PathStr; ExportFilter : tLASClassificationCategory = lccAll) : boolean;
label
   MaxPoints;
var
   I,j,k,RecsRead,x,y,NPts : integer;
   NoFilter : boolean;
   lf : tLAS_data;
   fName : PathStr;
   Points : ^tPointXYZIArray;
   //last,xc,yc : integer;
   Outf : file;
   //zf,z,
   xutm,yutm : float64;
   //bmp : tMyBitmap;
   Range : float64;
   ThisPoint : tLASClassificationCategory;
   Color : tPlatformColor;
begin
   {$If Defined(RecordMergeLASfiles) or Defined(TimeMergeLAS) or Defined(RecordLASexport)} WriteLineToDebugFile('tLas_files.ExportBinary in'); {$EndIf}
   NPts := 0;

   Range := MDDef.MaxValidZinPointCloud - MDDef.LowValidZinPointCloud;
   New(Points);

   ShowHourglassCursor;
   StartThreadTimers('Extract '+ CloudName,1,true);
   for k := 0 to pred(Las_fNames.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
      fName := Las_fNames.Strings[k];
       {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('file=' + fName); {$EndIf}
       lf := tLAS_data.Create(fName);
       if lf.IsLASFileOnMap(BaseMap.MapDraw) then begin
          NoFilter := NoFilterWanted;
          lf.PrepDataRead;
          for i := 0 to lf.ReadsRequired do begin
             lf.ReadPoints(RecsRead);
             ThreadTimers.Gauge1.Progress := round(100 * i/lf.ReadsRequired);
             j := 1;
             while j <= RecsRead do begin
                if NoFilter or (lf.MeetsFilter(j)) then begin
                   lf.GetShotScreenCoordinatesAppropriate(BaseMap.MapDraw,j, x,y);
                   if BaseMap.MapDraw.OnScreen(x,y) then begin
                      ThisPoint := lf.LASClassificationCategory(j);
                      if (ExportFilter = lccAll) or (ThisPoint = ExportFilter) then begin
                         inc(NPts);
                         lf.GetShotCoordinatesUTM(j,xutm, yutm);
                         Points^[NPTs].x := xutm;
                         Points^[NPTs].y := yutm;
                         Points^[NPTs].z := lf.ExpandLAS_Z(j);

                         if MDdef.ls.ColorCoding = lasccCloudID then Color := MDDef.CloudMapSymbol[Layer].Color
                         else lf.GetColor(j,BaseMap.MapDraw.LasMinAreaZ,BaseMap.MapDraw.LasMaxAreaZ,Color);
                         Points^[NPTs].Int := Color.rgbtRed;
                         Points^[NPTs].Int2 := Color.rgbtGreen;
                         Points^[NPTs].Int3 := Color.rgbtBlue;
                         if (NPts >= MaxPts) then begin
                            {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('Hit points limit'); {$EndIf}
                            goto MaxPoints;
                         end;
                      end;
                   end;
                end;
                inc(j,LasExportThinFactor);
             end;
          end;
         {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('after file ' + IntToStr(k) + '  Pts=' + SmartNumberPoints(PointsOut)); {$EndIf}
        MaxPoints:;
         lf.FreeLASRecordMemory;
         lf.Destroy;
       end;
   end;
   Result := (NPts > 0);
   if Result then begin
      if (GeometryFName = '') then GeometryFName := Petmar.NextFileNumber(MDTempDir, 'cloud_slicer_','.xyzib');
      case MDdef.ls.ColorCoding of
         lasccIntensity : ColorsFName := Palette256Bitmap(p256Gray);
         lasccPointSourceID,
         lasccUserData  : case Layer of
                             1 : ColorsFName := Palette256Bitmap(p256RedRamp);
                             2 : ColorsFName := Palette256Bitmap(p256GreenRamp);
                             3 : ColorsFName := Palette256Bitmap(p256BlueRamp);
                             else ColorsFName := Palette256Bitmap(p256Gray);
                          end;
         else ColorsFName := FullPaletteBitmap;
      end;


      {$If Defined(RecordLASexport)} WriteLineToDebugFile('tLas_files.ExportBinary out, PointsOut=' + SmartNumberPoints(NPts) + '  write to ' + GeometryFName); {$EndIf}
      AssignFile(Outf,GeometryFName);
      rewrite(Outf,1);
      BlockWrite(OutF,Points^,NPTs * SizeOf(tPointXYZI));
      CloseFile(outf);
   end;

   EndThreadTimers;
   LasExportThinFactor := 1;
   Dispose(Points);
   ShowDefaultCursor;
   {$If Defined(RecordMergeLASfiles) or Defined(TimeMergeLAS) or Defined(RecordLASexport)} WriteLineToDebugFile('tLas_files.ExportBinary out, PointsOut=' + SmartNumberPoints(NPts)); {$EndIf}
end;



{$IfDef VCL}
function tLas_files.MergeLasPoints(MergeLas : tMergeLas; BaseMap : tMapForm; var NewName : PathStr; Mask : tMyBitmap = Nil) : boolean;
var
   NewLAS : tCreateLasFile;
   I,j,k,RecsRead,x,y,PointsOut : integer;
   Lat,Long,xf,yf : float64;
   zf,z : float32;
   NoFilter,AddPt : boolean;
   lf : tLAS_data;
   fName : PathStr;
   p1 :  tScreenPRGB;
begin
   {$If Defined(RecordMergeLASfiles) or Defined(TimeMergeLAS)} WriteLineToDebugFile('tLas_files.MergeLasPoints in'); {$EndIf}

   if (NewName = '') then begin
      NewName := ExtractFilePath(LAS_fnames.Strings[0]);
      Petmar.GetFileNameDefaultExt('Exported LAS file','*.las',NewName,false);
   end;
   {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('NewName=' + NewName); {$EndIf}

   if (Mask = Nil) then begin
      if (MergeLas = mlOnMask) then MergeLas := mlOnMap;
   end
   else begin
      PetImage.FillScanlineAddresses(Mask,P1);
   end;
   PointsOut := 0;

   {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('tLas_files.MergeLasPoints start loop'); {$EndIf}
   NewLas := Nil;
   ShowHourglassCursor;
   if ShowLasProgress then StartThreadTimers('Merge '+ CloudName,1,true);
   for k := 0 to pred(Las_fNames.Count) do begin
      if ShowLasProgress then ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
      fName := Las_fNames.Strings[k];

      {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('file=' + fName); {$EndIf}
       lf := tLAS_data.Create(fName);
       if lf.IsLASFileOnMap(BaseMap.MapDraw) then begin
          if (NewLAS = Nil) then begin
             {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('Create new LAS file'); {$EndIf}
             CreateNewLasFromOldHeader(NewName,NewLas,lf);
             NewLas.FirstLAS := k;
             NewLas.ZeroHeaderRange;
          end;

          NoFilter := NoFilterWanted;

          {$IfDef RecordMergeLASfiles}
             WriteLineToDebugFile('Input file ' + IntToStr(k) + '  ' + ScaleFactorsString(lf.LasHeader) + '  ' + OffsetString(lf.LasHeader)+ '  ' ' lf.XRangeString(LASheader) + '  ' + lf.YRangeString(LASheader) + '  ' + lf.ZRangeString(LASheader));
          {$EndIf}
          lf.PrepDataRead;
          for i := 0 to lf.ReadsRequired do begin
             {$IfDef RecordMergeLASfilesDetailed} WriteLineToDebugFile('Pass ' + IntToStr(i)); {$EndIf}
             lf.ReadPoints(RecsRead);
             if ShowLasProgress then ThreadTimers.Gauge1.Progress := round(100 * i/lf.ReadsRequired);
             j := 1;
             while j <= RecsRead do begin
                if NoFilter or (lf.MeetsFilter(j)) then begin
                    if (MergeLas = mlRGBFilter) then begin
                       AddPt := not lf.ColorFilterOut(j);
                    end
                    else if (MergeLas = mlThin) or (MergeLas = mlTranslate) or (MergeLas = mlScaleUp) or lf.lasProjectionDefinition.RawXYZFile then begin
                       AddPt := true;
                    end
                    else if (MergeLas = mlInBox) then begin
                       lf.GetShotCoordinatesLatLong(j,Lat,Long);
                       AddPt := PointInBoundingBox(Lat,Long,CutBBox);
                    end
                    else begin
                       if (MergeLas = mlOnMap) or (MergeLas = mlOnMask) then begin
                           lf.GetShotScreenCoordinatesAppropriate(BaseMap.MapDraw,j, x,y);
                           if (MergeLas = mlOnMap) then begin
                             AddPt := BaseMap.MapDraw.OnScreen(x,y);
                           end
                           else if (MergeLas = mlOnMask) then begin
                              AddPt := BaseMap.MapDraw.OnScreen(x,y) and (SameColor(p1[y]^[x],rgbTripleRed));
                           end;
                       end
                       else if (MergeLas = mlDEMCovered) then begin
                          lf.GetShotCoordinatesLatLong(j,Lat,Long);
                          DEMGlb[BaseMap.MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,x,y);
                          AddPt := DEMGlb[BaseMap.MapDraw.DEMonMap].GetElevMeters(round(x),round(y),z);
                       end;
                    end;

                    if AddPt then begin
                       xf := lf.ExpandLAS_X(j);
                       yf := lf.ExpandLAS_Y(j);
                       zf := lf.ExpandLAS_Z(j);
                       if (MergeLas = mlTranslate) then begin
                          xf := xf + TranslateX;
                          yf := yf + TranslateY;
                          zf := zf + TranslateZ;
                       end
                       else if (MergeLas = mlScaleUp) then begin
                          xf := xf * ScaleUp;
                          yf := yf * ScaleUp;
                          zf := zf * ScaleUp;
                       end;
                       lf.ReviseCoordinates(j,xf,yf,zf,NewLas.NewLasHeader);
                       NewLas.AddShotToOutputBuffer(lf,j);
                       inc(PointsOut);
                    end;
                end;
                inc(j,LasExportThinFactor);
            end;
         end;
         {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('after file ' + IntToStr(k) + '  Pts=' + SmartNumberPoints(PointsOut)); {$EndIf}
         lf.FreeLASRecordMemory;
         lf.Destroy;
       end;
   end;
   if (NewLas = Nil) then begin
      //MessageToContinue('No files in new file coverage area');
   end
   else begin
      {$If Defined(RecordMergeLASfiles) or Defined(TimeMergeLAS)}
         WriteLineToDebugFile('New LAS, pts=' + SmartNumberPoints(NewLas.NumPointRecs) + ' ' + ScaleFactorsString(NewLas.NewLasHeader) + ' ' + OffsetString(NewLas.NewLasHeader)+ ' Save to: ' + NewName);
         WriteLineToDebugFile('Merge pts  ' + AllRangeString(NewLas.NewLasHeader));
      {$EndIf}
      Result := (PointsOut > 0);
      NewLAS.Destroy;
   end;

   if ShowLasProgress then EndThreadTimers;
   LasExportThinFactor := 1;
   {$If Defined(RecordMergeLASfiles) or Defined(TimeMergeLAS)} WriteLineToDebugFile('tLas_files.MergeLasPoints out, PointsOut=' + SmartNumberPoints(PointsOut)); {$EndIf}
end;
{$EndIf}


function tLas_files.CloudStats : shortstring;
begin
    if (LAS_fnames.Count > 0) then begin
       Result := CloudName + '; pts=' + SmartNumberPoints(TotalCloudPts) + ';  files=' + IntToStr(LAS_fnames.Count) + '; ' + RealToString(PointDensity,-12,1) + ' pts/m²; Elev ' + RealToString(MinZ,-12,0) + '--' + RealToString(MaxZ,-12,0);
    end;
end;


constructor tLas_files.Create;
begin
   inherited;
   LAS_fnames := tStringList.Create;
   TotalCloudPts := 0;
   MinZ := 9999;
   MaxZ := -9999;
   PointDensity := -9999;
   MinGPStime := 99e45;
   MaxGPSTime := -99e45;

   MinTime := 99e25;
   MaxTime := 99e-25;
   Cloud_ls := MDDef.ls;
   LasExportThinFactor := 1;
   TranslateX := 0;
   TranslateY := 0;
   TranslateZ := 0;
   ScaleUp := 1;

   HasIntensity := true;
   HasClassification := true;
   HasPointID := true;
   HasUserData := true;
   HasScanAngle := true;
   HasReturnNumbers := true;
   HasTime := true;
   HasRGB := true;
end;


destructor tLas_files.Destroy;
begin
   inherited;
   Las_fNames.Destroy;
end;


procedure tLas_files.RGBPalette;
const
   ReportFreq = 10;
type
   tFullPalette = array[0..255,0..255,0..255] of integer;
var
  ColorsInPalette,MaxCount,ThisCount,
  I,j,k,RecsRead : Integer;
  fName : PathStr;
  LasData : tLas_Data;
  r,g,b : byte;
  Palette : tStringList;
  FullPalette : ^tFullPalette;
begin
   {$IfDef RecordLASColors} WriteLineToDebugFile('tLas_files.RGBPalette enter'); {$EndIf}
   {$IfDef VCL} StartThreadTimers('Point cloud',1,true); {$EndIf}
   Palette := tStringList.Create;
   Palette.Add('NAME,RED,GREEN,BLUE,COLOR,N');
   New(FullPalette);
   for r := 0 to 255 do
      for g := 0 to 255 do
         for b := 0 to 255 do
            FullPalette^[r,g,b] := 0;
   {$IfDef RecordLASColors} WriteLineToDebugFile('initialize done'); {$EndIf}

   for k := 0 to pred(LAS_fnames.Count) do begin
       {$IfDef VCL} ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count); {$EndIf}
      LasData := tLAS_data.Create(LAS_fNames[k]);
      LasData.PrepDataRead;
      for i := 0 to LasData.ReadsRequired do begin
          {$IfDef VCL} ThreadTimers.Gauge1.Progress := round(100 * i/LasData.ReadsRequired); {$EndIf}
          LasData.ReadPoints(RecsRead);
          for j := 1 to RecsRead do begin
             if LasData.GetRGBComponents(j,r,g,b) then begin
                inc(FullPalette^[r,g,b]);
             end;
          end;
      end;
      LasData.FreeLASRecordMemory;
   end;

   ColorsInPalette := 0;
   MaxCount := 0;
   for r := 0 to 255 do
      for g := 0 to 255 do
         for b := 0 to 255 do begin
            ThisCount := FullPalette^[r,g,b];
            if ThisCount > 0 then begin
               inc(ColorsInPalette);
               if ThisCount > MaxCount then MaxCount := ThisCount;
               if ThisCount > ReportFreq then
                  Palette.Add('color' + ',' + IntToStr(r) + ',' + intToStr(g) + ',' + IntToStr(b) + ',' +
                      IntToStr(RGB(r,g,b)) + ',' + IntToStr(ThisCount));
            end;
         end;
   Dispose(FullPalette);
   {$IfDef VCL} EndThreadTimers; {$EndIf}
   {$IfDef RecordLASColors} WriteLineToDebugFile('Palette entries=' + IntToStr(ColorsInPalette)); {$EndIf}
   fName := MDTempDir + 'palette.csv';
   {$IfDef VCL} PetdbUtils.StringList2CSVtoDB(Palette,fName,true); {$Else} Palette.Free; {$EndIf}
   {$IfDef RecordLASColors} WriteLineToDebugFile(' tLas_files.RGBPalette out'); {$EndIf}
end;



procedure tLas_files.ElevationHistogram;
const
   MaxIntensity = MaxWord;
   LowElev = -100;
   HighElev = 5000;
type
   tElevArray = array[LowElev..HighElev] of integer;
var
  ze : float64;
  fName : PathStr;
  LasData : tLas_Data;
  PointCat : tLASClassificationCategory;
  Elevs,Grounds,Vegs,Builds : tElevArray;
var
   i,j,k,RecsRead,db : integer;
   sl : tStringList;
   TheGraph : tThisBaseGraph;
begin
   {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram in ' + ExtractFilePath(LAS_fNames[0])); {$EndIf}
   StartThreadTimers('Elevation Histogram',1,true);
   for I := LowElev to HighElev do begin
      Elevs[i] := 0;
      Grounds[i] := 0;
      Vegs[i] := 0;
      Builds[i] := 0;
   end;
   for k := 0 to pred(LAS_fnames.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
      LasData := tLAS_data.Create(LAS_fNames[k]);
      {$IfDef RecordLASHist} WriteLineToDebugFile('Do ' + LAS_fNames[k]); {$EndIf}
         LasData.PrepDataRead;
         for i := 0 to LasData.ReadsRequired do begin
             ThreadTimers.Gauge1.Progress := round(100 * i/LasData.ReadsRequired);
             LasData.ReadPoints(RecsRead);
             for j := 1 to RecsRead do begin
                ze := Lasdata.ExpandLAS_Z(j);
                if (ze >= LowElev) and (ze <= HighElev) then begin
                    PointCat := LasData.LASClassificationCategory(j);
                    if (PointCat = lccGround) then inc(Grounds[round(ze)]);
                    if (PointCat = lccVeg) then inc(Vegs[round(ze)]);
                    if (PointCat = lccBuilding) then inc(Builds[round(ze)]);
                    inc(Elevs[round(ze)]);
                end;
             end;
         end;
         LasData.FreeLASRecordMemory;
         LasData.Destroy;
      end;
   EndThreadTimers;
     sl := tStringList.Create;
     sl.add('TOTAL,ELEV_M,VEG,GROUND,BUILDING');
     for i := LowElev to HighElev do begin
        if (Elevs[i] > 0) then sl.add(IntToStr(Elevs[i]) + ',' + IntToStr(i) + ',' + IntToStr(Vegs[i]) + ',' + IntToStr(Grounds[i])+ ',' + IntToStr(Builds[i]));
     end;
     fName := Petmar.NextFileNumber(MDtempDir,'las_hist_','.dbf');
     db := BaseMap.StringListtoLoadedDatabase(sl,fname);
     TheGraph := GISDB[db].CreateScatterGram('TOTAL','ELEV_M',clRed,true,'LAS elevation histogram');
     GISDB[db].AddSeriesToScatterGram(TheGraph,clLime,'VEG','ELEV_M',true);
     GISDB[db].AddSeriesToScatterGram(TheGraph,clBrown,'GROUND','ELEV_M',true);
     GISDB[db].AddSeriesToScatterGram(TheGraph,clAqua,'BUILDING','ELEV_M',true);
   {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram out'); {$EndIf}
end;


procedure tLas_files.CloudStatistics(TileTable : boolean; CloudStats : tCloudStats; DoText : boolean = true; DoGraph : boolean = true);
const
   MaxIntensity = MaxWord;
   LowElev = -100;
   HighElev = 5000;
type
   tIntArray = array[0..MaxIntensity] of integer;
   tElevArray = array[LowElev..HighElev] of integer;
var
   WithHeldPoints,OverlapPoints,z1,z99,
   //I,j,ThisReturn,
   k,RecsRead,z,First,Last,Total,TotalPoints,AirPoints : Integer;
   CenterLat,CenterLong : float64;
   ze : float64;
   r,g,b,n : word;
   LasData : tLas_Data;
   //PointCat : tLASClassificationCategory;
   Elevs : tElevArray;
   HasNIR : boolean;
   //ThisGraph,
   ThisGraph2 : TThisBaseGraph;
   rFile : file;
   v : tGraphPoint32;
   fName : PathStr;
   IndexTable : tMyData;
   NumCats,Category,Int : integer;
   Coding : array[0..MaxLasCat] of integer;
   Return : array[0..MaxLASreturns] of integer;
   ScanAngleHist : array[-128..127] of integer;
   UserDataHist : array[0..255] of integer;
   PointCloudID,Intensities,Reds,Greens,Blues,NIRs : ^tIntArray;
   TileStr : ANSIstring;
   sl,Cats,Tiles : tStringList;


         procedure AddHistogramFile(Int : tIntArray; LegendEntry : string35);
         var
            Total,z : integer;
         begin
            Total := 0;
            for z := First to Last do Total := Total + int[z];
            if Total = 0 then exit;
            ThisGraph2.GraphDraw.LegendList.Add(LegendEntry);
            ThisGraph2.OpenDataFile(rfile);
            for z := First to Last do begin
               v[1] := Int[z] /Total * succ(Last-First);
               v[2] := z;
               BlockWrite(rfile,v,1);
            end;
            CloseFile(RFile);
         end;

         procedure FindPercentileRange(PC : float64; Hist : tIntArray; var Min,Max : integer);
         var
            Tail : integer;
         begin
            PC := 0.01 * PC;
            Max := MaxWord;
            Tail := 0;
            repeat
               Tail := Tail + Hist[Max];
               dec(Max);
            until Tail > PC * TotalPoints;

            Min := 0;
            Tail := 0;
            repeat
               Tail := Tail + Hist[Min];
               inc(Min);
            until Tail > PC * TotalPoints;
            Cats.Add('  Tail ' + RealToString(100 * PC,-12,-2) + '  ' + IntToStr(Min) + ' to ' + IntToStr(Max));
         end;

         procedure ElevFindPercentileRange(PC : float64; Hist : tElevArray; var Min,Max : integer);
         var
            Tail : integer;
         begin
            PC := 0.01 * PC;
            Max := HighElev;
            Tail := 0;
            repeat
               Tail := Tail + Hist[Max];
               dec(Max);
            until Tail > PC * TotalPoints;

            Min := LowElev;
            Tail := 0;
            repeat
               Tail := Tail + Hist[Min];
               inc(Min);
            until Tail > PC * TotalPoints;
            Cats.Add('  Tail ' + RealToString(100 * PC,-12,-2) + '  ' + IntToStr(Min) + ' to ' + IntToStr(Max));
         end;

         procedure FindMinMax(what : shortstring; Hist : tIntArray);
         var
            Min,Max : integer;
         begin
            Cats.Add('');
            First := 0;
            While (Hist[First] = 0) and (First <= MaxIntensity) do inc(First);

            Last := MaxIntensity;
            While (Hist[Last] = 0) and (Last >= 0) do dec(Last);

            if (Last-First) = 0 then begin
                Cats.Add(What + ' all same value, ' + IntToStr(Last));
                if What = 'Intensity' then HasIntensity := false;
                Min := First;
                Max := First;
            end
            else begin
               Cats.Add(What + ' range: ' + IntToStr(First) + ' to ' + IntToStr(Last));
               FindPercentileRange(0.01,Hist,Min,Max);
               FindPercentileRange(0.1,Hist,Min,Max);
               FindPercentileRange(1,Hist,Min,Max);
               FindPercentileRange(5,Hist,Min,Max);
            end;
            TileStr := TileStr + ',' + IntToStr(First) + ',' + IntToStr(Last)+ ',' + IntToStr(Min) + ',' + IntToStr(Max);
         end;


         procedure Zerocounters;
         var
            i : integer;
         begin
            New(PointCloudID);
            New(Intensities);
            New(Reds);
            New(Blues);
            New(Greens);
            New(NIRs);
            for I := LowElev to HighElev do Elevs[i] := 0;
            for i := 0 to MaxLASreturns do Return[i] := 0;
            for i := 0 to MaxLasCat do Coding[i] := 0;
            for i := 0 to 255 do UserDataHist[i] := 0;
            for i := -128 to 127 do ScanAngleHist[i] := 0;
            for i := 0 to MaxWord do begin
               Intensities^[i] := 0;
               PointCloudID^[i] := 0;
               Reds^[i] := 0;
               Greens^[i] := 0;
               Blues^[i] := 0;
               NIRs^[i] := 0;
            end;
            Total := 0;
            TotalPoints := 0;
            AirPoints := 0;
            OverlapPoints := 0;
            WithHeldPoints := 0;
            HasRGB := false;
         end;


         procedure ReadPointCloudPropertiesFromTable;
         var
            fName : PathStr;
         begin
            fName := IndexTableName;
            {$IfDef RecordLASHist} WriteLineToDebugFile('Read Existing ' + fName); {$EndIf}
            IndexTable := tMyData.Create(fName);

            if IndexTable.FieldExists('LONG_HI') then begin
               GeoBBox.xmax := IndexTable.FindFieldMax('LONG_HI');
               GeoBBox.xmin := IndexTable.FindFieldMin('LONG_LOW');
               GeoBBox.ymax := IndexTable.FindFieldMax('LAT_HI');
               GeoBBox.ymin := IndexTable.FindFieldMin('LAT_LOW');
               {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('ReadPointCloudPropertiesFromTable, cloud geo box ' + sfBoundBoxToString(GeoBBox,6)); {$EndIf}
               WGS84DatumConstants.LatLongDegreetoUTM(GeoBBox.YMax,GeoBBox.XMax,UTMBBox.xmax,UTMBBox.ymax);
               WGS84DatumConstants.LatLongDegreetoUTM(GeoBBox.YMin,GeoBBox.XMin,UTMBBox.xmin,UTMBBox.ymin);
               {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('ReadPointCloudPropertiesFromTable, cloud utm box ' + sfBoundBoxToString(UTMBBox,1)); {$EndIf}
            end;

            if MDDef.LASPC99 then begin
               MaxZ := IndexTable.FindFieldMax('ELEV_99');
               MinZ := IndexTable.FindFieldMin('ELEV_1');
            end
            else begin
               MaxZ := IndexTable.FindFieldMax('MAX_ELEV');
               MinZ := IndexTable.FindFieldMin('MIN_ELEV');
            end;
            HasIntensity := IndexTable.FieldHasChar('HAS_INTEN','Y');
            if HasIntensity then begin
               Inten_99 := round(IndexTable.FindFieldMax('INTEN_99'));
               Inten_1 := round(IndexTable.FindFieldMax('INTEN_1'));
               Max_Inten := round(IndexTable.FindFieldMax('MAX_INTEN'));
               Min_Inten := round(IndexTable.FindFieldMax('MIN_INTEN'));
               //if MDDef.LASPC99 then begin
            end;

            HasClassification := IndexTable.FieldHasChar('HAS_CLASS','Y');
            HasRGB := IndexTable.FieldHasChar('HAS_RGB','Y');
            if HasRGB then begin
               if MDDef.LASPC99 then begin
                  Max_Red := round(IndexTable.FindFieldMax('RED_99'));
                  Min_Red := round(IndexTable.FindFieldMax('RED_1'));
                  Max_Green := round(IndexTable.FindFieldMax('GREEN_99'));
                  Min_Green := round(IndexTable.FindFieldMax('GREEN_1'));
                  Max_Blue := round(IndexTable.FindFieldMax('BLUE_99'));
                  Min_Blue := round(IndexTable.FindFieldMax('BLUE_1'));
               end
               else begin
                  Max_Red := round(IndexTable.FindFieldMax('MAX_RED'));
                  Min_Red := round(IndexTable.FindFieldMax('MIN_RED'));
                  Max_Green := round(IndexTable.FindFieldMax('MAX_GREEN'));
                  Min_Green := round(IndexTable.FindFieldMax('MIN_GREEN'));
                  Max_Blue := round(IndexTable.FindFieldMax('MAX_BLUE'));
                  Min_Blue := round(IndexTable.FindFieldMax('MIN_BLUE'));
               end;
            end;
            HasNIR := IndexTable.FieldHasChar('HAS_NIR','Y');
            if HasNIR then begin
               if MDDef.LASPC99 then begin
                  Max_NIR := round(IndexTable.FindFieldMax('MAX_99'));
                  Min_NIR := round(IndexTable.FindFieldMax('MIN_1'));
               end
               else begin
                  Max_NIR := round(IndexTable.FindFieldMax('MAX_NIR'));
                  Min_NIR := round(IndexTable.FindFieldMax('MIN_NIR'));
               end;
            end;
            HasTime := IndexTable.FieldHasChar('HAS_TIME','Y');
            HasPointID := IndexTable.FieldHasChar('HAS_PTID','Y');
            HasUserData := IndexTable.FieldHasChar('HAS_USRDAT','Y');
            HasScanAngle := IndexTable.FieldHasChar('HAS_CLASS','Y');
            HasReturnNumbers := IndexTable.FieldHasChar('HAS_CLASS','Y');
            IndexTable.Destroy;
            {$IfDef RecordLASHist} WriteLineToDebugFile('Done Existing ' + fName); {$EndIf}
         end;


         procedure DoResults(fName : PathStr;  LasData : tLas_Data);
         //var
            //i : integer;

                 procedure TimeForScanAngles;
                 var
                    i,First,Last : integer;
                 begin
                    First := -128;
                    While ScanAngleHist[First] = 0 do inc(First);
                    Last := 127;
                    While ScanAngleHist[Last] = 0 do dec(Last);

                    if abs(Last-First) < 0.01 then begin
                        Cats.Add('No recorded scan angles');
                        HasScanAngle := false;
                    end
                    else begin
                       Cats.Add('Scan angle range: ' + IntToStr(First) + ' to ' + IntToStr(Last));
                       if DoGraph then begin
                          sl := tStringList.Create;
                          sl.add('RETURNS,SCAN_ANGLE');
                          for i := -128 to 127 do begin
                             if ScanAngleHist[i] > 0 then sl.add(IntToStr(ScanAngleHist[i]) + ',' + IntToStr(i));
                          end;
                          GraphFromCSVfile(sl,false,true);
                       end;
                    end;
                    Cats.Add('');
                    {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram done scan angle'); {$EndIf}

                    if abs(MaxTime-MinTime) < 0.01 then begin
                       HasTime := false;
                       Cats.Add('No GPS times');
                    end
                    else begin
                       Cats.Add('GPS time range: ' + RealToString(MinTime,-18,-8) + ' to ' + RealToString(MaxTime,-18,-8) );
                       Cats.Add('');
                    end;
                    {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram done GPS time'); {$EndIf}

                     Cats.Add('');
                     NumCats := 0;
                     for i := 0 to MaxWord do
                        if PointCloudID^[i] > 0 then inc(NumCats);
                     If (NumCats <= 1) then HasPointID := false;
                     Cats.Add('Point Source ID     Returns');
                     for i := 0 to MaxWord do
                        if PointCloudID^[i] > 0 then
                           Cats.Add(IntegerToString(i,6) + IntegerToString(PointCloudID^[i],21));
                     Cats.Add('');
                     {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram done point cloud ID'); {$EndIf}

                     NumCats := 0;
                     for i := 0 to 255 do
                        if UserDataHist[i] > 0 then inc(NumCats);
                     If NumCats <= 1 then HasUserData := false;
                     Cats.Add('User Data Rec     Returns');
                     for i := 0 to 255 do begin
                        if UserDataHist[i] > 0 then Cats.Add(IntegerToString(i,6) + IntegerToString(UserDataHist[i],18));
                     end;
                 end;


                 procedure TimeForIntensities;
                 begin
                    FindMinMax('Intensity',Intensities^);
                    if ((Last-First) <> 0) and DoGraph then begin
                       ThisGraph2 := TThisBaseGraph.Create(Application);
                       ThisGraph2.GraphDraw.LegendList := tStringList.Create;
                       AddHistogramFile(Intensities^,'overall');
                       ThisGraph2.Caption := 'LAS Intensity Histogram: '+ Self.CloudName;
                       ThisGraph2.GraphDraw.HorizLabel := 'Concentration (fraction of uniform)';
                       ThisGraph2.GraphDraw.VertLabel := 'Return Intensity';
                       ThisGraph2.AutoScaleAndRedrawDiagram;
                    end;
                    {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram done intensity'); {$EndIf}
                    if HasRGB then begin
                       FindMinMax('Red',Reds^);
                       FindMinMax('Green',Greens^);
                       FindMinMax('Blue',Blues^);
                       if HasNIR then FindMinMax('NIR',NIRs^)
                       else TileStr := TileStr + ',-99,-99,-99,-99';
                       {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram done rgb'); {$EndIf}
                    end
                    else TileStr := TileStr + ',-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99';
                    Cats.Add('');
                 end;

                  procedure TimeForPointCats;
                  var
                     i,NumCats : integer;
                  begin
                      NumCats := 0;
                      for i := 0 to MaxLasCat do
                         if Coding[i] > 0 then inc(NumCats);
                      HasClassification := NumCats > 1;
                      if (NumCats = 0) then begin
                         Cats.Add('No classification');
                      end
                      else begin
                         Cats.Add('Category    Returns');
                         for i := 0 to MaxLasCat do
                            if (Coding[i] > 0) then
                               Cats.Add(IntegerToString(i,2) + IntegerToString(Coding[i],12) + RealToString(Coding[i]/TotalPoints * 100,8,2) + '%   ' + LasCatName[i]);
                      end;
                      Cats.Add('');

                      NumCats := 0;
                      for i := 0 to MaxLASreturns do
                         if Return[i] > 0 then inc(NumCats);

                      if (NumCats <= 1) then begin
                         HasReturnNumbers := false;
                         Cats.Add('No multiple returns')
                      end
                      else begin
                         Cats.Add('Return #    Returns');
                         for i := 0 to MaxLASreturns do
                            if Return[i] > 0 then begin
                               Cats.Add(IntegerToString(i,2) + IntegerToString(Return[i],18) + RealToString(Return[i]/TotalPoints * 100,8,2) + '%');
                            end;

                         Cats.Add('');
                         Cats.Add('Air Returns:' + IntegerToString(AirPoints,18) + RealToString(AirPoints/TotalPoints * 100,8,2) + '%');
                      end;
                      Cats.Add('');
                  end;


                 procedure TimeForElevations;
                 var
                    i : integer;
                 begin
                    First := LowElev;
                    While Elevs[First] = 0 do inc(First);
                    Last := HighElev;
                    While Elevs[Last] = 0 do dec(Last);
                    Cats.Add('Elev range: ' + IntToStr(First) + ' to ' + IntToStr(Last));
                    ElevFindPercentileRange(0.01,Elevs,z1,z99);
                    ElevFindPercentileRange(0.1,Elevs,z1,z99);
                    ElevFindPercentileRange(1,Elevs,z1,z99);
                    Cats.Add('');

                    if DoGraph then begin
                       sl := tStringList.Create;
                       sl.add('RETURNS,ELEV_M');
                       for i := First to Last do begin
                          if Elevs[i] > 0  then sl.add(RealToString(Elevs[i]/Total * succ(Last-First),-12,-6) + ',' + IntToStr(i));
                       end;
                       GraphFromCSVfile(sl,false,true);
                      {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram done elevation graph'); {$EndIf}
                    end
                    else begin
                      {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram skip elevation graph'); {$EndIf}
                    end;
                 end;


                  procedure AddPresence(Feature  : boolean);
                  var
                     ch : char;
                  begin
                     if Feature then ch := 'Y' else ch := 'N';
                     TileStr := TileStr + ',' + ch;
                  end;


         begin {DoResults}
            Cats.Add('LAS, files=' + IntToStr(LAS_fnames.Count));
            Cats.Add('First file=' + LAS_fnames.Strings[0]);
            Cats.Add('');
            Cats.Add('Total returns:   ' + IntToStr(TotalPoints));
            Cats.Add('Withheld points: ' + IntToStr(WithHeldPoints));
            Cats.Add('Overlap points:  ' + IntToStr(OverlapPoints));
            Cats.Add('');
            TimeForElevations;  //must be here to get z1 and z99

            TileStr := RealToString(CenterLat,-18,-7) + ',' +
                       RealToString(CenterLong,-18,-7) + ',' +

                       ExtractFileName(fName) + ',' +
                       IntToStr(LasData.NumPointRecs) + ',' +
                       RealToString(LasData.SingleFilePointDensity,-12,2) + ',' +

                      RealToString(LasData.LAS_LatLong_Box.ymax,-18,-7) + ',' +
                      RealToString(LasData.LAS_LatLong_Box.ymin,-18,-7) + ',' +
                      RealToString(LasData.LAS_LatLong_Box.xmax,-18,-7) + ',' +
                      RealToString(LasData.LAS_LatLong_Box.xmin,-18,-7) + ',' +

                       RealToString(LasData.LasHeader.MaxZ,-12,2)  + ',' +
                       RealToString(LasData.LasHeader.MinZ,-12,2)  + ',' +
                       RealToString(z1,-12,2)  + ',' +
                       RealToString(z99,-12,2)  + ',' +
                       RealToString(MinGPSTime,-18,-8) + ',' +
                       RealToString(MaxGPSTime,-18,-8);

            TimeForIntensities;
            TimeForPointCats;
            TimeForScanAngles;

            AddPresence(HasClassification);
            AddPresence(HasRGB);
            AddPresence(HasIntensity);
            AddPresence(HasTime);
            AddPresence(HasPointID);
            AddPresence(HasUserData);
            AddPresence(HasScanAngle);
            AddPresence(HasReturnNumbers);
            AddPresence(HasNIR);
            TileStr := TileStr + ',' + IntToStr(LasData.LidarPointType);
            Tiles.Add(TileStr);
         end;


         procedure DoTileStats(fName : PathStr);
         var
            i,j : integer;
         begin
            CenterLat := 0.5 * (LasData.LAS_LatLong_Box.ymax + LasData.LAS_LatLong_Box.ymin);
            CenterLong := 0.5 * (LasData.LAS_LatLong_Box.xmax + LasData.LAS_LatLong_Box.xmin);

            for i := 0 to LasData.ReadsRequired do begin
               {$IfDef RecordLASHistFull}
                  WriteLineToDebugFile('Read ' + IntToStr(i) + '/' + IntToStr(LasData.ReadsRequired)); WmDEM.SetPanelText(0,'Read ' + IntToStr(i) + '/' + IntToStr(LasData.ReadsRequired));
               {$EndIf}
                ThreadTimers.Gauge1.Progress := round(100 * i/LasData.ReadsRequired);
                LasData.ReadPoints(RecsRead);
                for j := 1 to RecsRead do begin
                   inc(TotalPoints);
                   ze := Lasdata.ExpandLAS_Z(j);
                   z := round(ze);
                   if (z >= -30) and (z <= 5000) then begin
                      inc(Elevs[z]);
                      inc(Total);
                   end;
                   if LasData.OverlapPoint(j) then inc(OverlapPoints);
                   if LasData.WithHeldPoint(j) then inc(WithHeldPoints);
                   if LasData.AirReturn(j) then inc(AirPoints);
                   inc(Return[LasData.ReturnNumber(j)]);

                   Int := LasData.GetShotMeasuredIntensity(j);
                   if (Int > MaxIntensity) then Int := MaxIntensity;
                   inc(Intensities^[Int]);
                   if LasData.LidarPointType in [2,3,7,8] then begin
                      LasData.GetRGBWord(j,r,g,b);
                      inc(Reds^[r]);
                      inc(Greens^[r]);
                      inc(Blues^[r]);
                   end;

                   if LasData.LidarPointType in [8] then begin
                      LasData.GetNIRWord(j,n);
                      inc(NIRs^[n]);
                   end;

                   Category := LasData.LASclassification(j);
                   if (Category in [1..MaxLasCat]) then inc(Coding[Category]);
                   ze := LasData.GetScanAngle(j);
                   if (ze >=-128) and (ze <=127) then inc(ScanAngleHist[round(ze)]);
                   z := LasData.PointSourceID(j);
                   inc(PointCloudID^[z]);
                   inc(UserDataHist[LasData.UserDataRec(j)]);

                   Petmath.CompareValueToExtremes(LasData.GetGPSTime(j),MinTime,MaxTime);
                   Petmath.CompareValueToExtremes(LasData.GetGPSTime(j),MinGPSTime,MaxGPSTime);
                end {for j};
            end {for i};
            DoResults(fName,LASData);
         end;



begin
   {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.CloudStatistics in ' + ExtractFilePath(LAS_fNames[0])); {$EndIf}
   IndexTable := Nil;
   if (CloudStats = csLoadStats) and FileExists(IndexTableName) then begin
      ReadPointCloudPropertiesFromTable;
      {$IfDef RecordLASHist} WriteLineToDebugFile('done read from table'); {$EndIf}
   end
   else begin
      Tiles := tStringList.Create;
      Tiles.Add('LAT,LONG,TILE,POINTS,DENSITY,LAT_HI,LAT_LOW,LONG_HI,LONG_LOW,MAX_ELEV,MIN_ELEV,ELEV_1,ELEV_99,MAX_TIME,MIN_TIME,' +
                'MIN_INTEN,MAX_INTEN,INTEN_1,INTEN_99,MIN_RED,MAX_RED,RED_1,RED_99,MIN_GREEN,MAX_GREEN,GREEN_1,GREEN_99,MIN_BLUE,MAX_BLUE,BLUE_1,BLUE_99,MIN_NIR,MAX_NIR,NIR_1,NIR_99,' +
                'HAS_CLASS,HAS_RGB,HAS_INTEN,HAS_TIME,HAS_PTID,HAS_USRDAT,HAS_SCAN,HAS_RET,HAS_NIR,LAS_PT_TYPE');

      StartThreadTimers('Point cloud stats ' + TimeToStr(Now),1,true);
      Cats := tStringList.Create;
      Cats.Add(CloudName);
      Cats.Add('');
      HasRGB := false;
      for k := 0 to pred(LAS_fnames.Count) do begin
         if TileTable or (k=0) then ZeroCounters;
         ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
         Cats.Clear;
         if (IndexTable <> Nil) then begin
            IndexTable.ApplyFilter('NAME=' + QuotedStr(LAS_fNames[k]));
         end
         else begin
            LasData := tLAS_data.Create(LAS_fNames[k]);
            LasData.RGBPresent := LasData.LidarPointType in [2,3,7,8];
            {$IfDef RecordLASHist} WriteLineToDebugFile('Do ' + LAS_fNames[k] + '  ' + IntToStr(LasData.LidarPointType)); {$EndIf}
            if LasData.RGBPresent then begin
               HasRGB := true;
            end;
            HasNIR := LasData.LidarPointType in [8];
            {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.CloudStatistics call do tile stats'); {$EndIf}
            LasData.PrepDataRead;
            DoTileStats(LAS_fNames[k]);
            {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.ElevationHistogram ready to free'); {$EndIf}
            LasData.FreeLASRecordMemory;
            {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.CloudStatistics call destroy'); {$EndIf}
            LasData.Destroy;
         end;
      end;
      {$IfDef RecordLASHist} WriteLineToDebugFile('done with tiles'); {$EndIf}

      if TileTable then begin
         {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.CloudStatistics TileTable'); {$EndIf}
         fName := ChangeFileExt(IndexTableName,'.csv');
         PetDBUtils.StringList2CSVtoDB(Tiles,fName,true);
         Cats.Free;
         ReadPointCloudPropertiesFromTable;
      end
      else begin
         {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.CloudStatistics NO TileTable'); {$EndIf}
         Tiles.Free;
         if DoText then Petmar.DisplayAndPurgeStringList(Cats,'LAS Clouds stats: ' + Self.CloudName)
         else Cats.Free;
      end;

      EndThreadTimers;

      Dispose(PointCloudID);
      Dispose(Intensities);
      Dispose(NIRs);
   end;
   {$IfDef RecordLASHist} WriteLineToDebugFile('tLas_files.CloudStatistics out'); {$EndIf}
end;


function tLas_files.OpenGLLasPoints(var NewName : PathStr; BaseMap : tMapForm; Mask : tMyBitmap = Nil) : boolean;
begin
   if (LAS_fnames <> Nil) and (LAS_fnames.Count > 0) then begin
      {$IfDef RecordLASfiles} WriteLineToDebugFile('tLas_files.OpenGLLasPoints for ' + ExtractFilePath(NewName)); {$EndIf}
      if (Mask = nil) and (LAS_fnames.Count = 1) and BaseMap.MapDraw.LatLongOnScreen(GeoBBox.ymin,GeoBBox.xmin) and BaseMap.MapDraw.LatLongOnScreen(GeoBBox.ymax,GeoBBox.xmax) then begin
         NewName := LAS_fNames.Strings[0];
         Result := true;
      end
      else begin
        if (NewName = '') then NewName := Petmar.NextFileNumber(MDTempDir, CloudName + '_','.las');
        Result := MergeLasPoints(mlOnMask,BaseMap,NewName,Mask);
      end;
   end;
end;


procedure tLas_files.KMLLasPoints(InBoundBox : sfBoundBox;  ExportMode : tExportMode; NewName : PathStr = '');
var
   I,j,k,RecsRead,npts : integer;
   Lat,Long,z : float64;
   NoFilter : boolean;
   lf : tLAS_data;
   csvout : tStringList;
   tStr : shortstring;
   kml : tKMLCreator;
   ch : ANSIchar;
   fName : PathStr;
begin
   if (LAS_fnames <> Nil) and (LAS_fnames.Count > 0) then begin
      {$IfDef RecordLASKML} WriteLineToDebugFile('tLas_files.KMLLasPoints in'); {$EndIf}
      if ShowLasProgress then StartThreadTimers('LAS export',1,true);
      if (ExportMode = exmKML) then begin
         kml := tKMLCreator.Create('','MICRODEM');
         kml.AddFolder('LAS export','Points');
         KML.KMLStringList.Add(' <Style id="dot1">' +  '<IconStyle>' +  '<scale>0.2</scale>' + '<Icon>' +  '<href>https://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>' + '	</Icon>' + '</IconStyle>' + '</Style>');
      end
      else if (ExportMode = exmDBF) then begin
         csvout := tStringList.Create;
         csvout.Add('LAT,LONG,Z,CLASS,RET,INTENSITY,' + RecNoFName);
      end;
      nPts := 0;
      for k := 0 to pred(Las_fNames.Count) do begin
         if ShowLasProgress then ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
         fName := Las_fNames.Strings[k];
         {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('file=' + fName); {$EndIf}
          lf := tLAS_data.Create(fName);
          if lf.InBoundBoxGeo(inBoundBox) then begin
             lf.PrepDataRead;
             NoFilter := NoFilterWanted;
             for i := 0 to lf.ReadsRequired do begin
                lf.ReadPoints(RecsRead);
                if ShowLasProgress then ThreadTimers.Gauge1.Progress := round(100 * i/lf.ReadsRequired);
                for j := 1 to RecsRead do begin
                   if NoFilter or (lf.MeetsFilter(j)) then begin
                       lf.GetShotCoordinatesLatLong(j,Lat,Long);
                       if PointInBoundingBox(Lat,Long,inBoundBox) then begin
                          Inc(NPts);
                          z := lf.ExpandLAS_Z(j);
                          if (ExportMode = exmKML) then KML.AddPlaceMark(Lat,Long,'','','',z + MDDef.KML_LAS_Offset,'dot1')
                          else if (ExportMode = exmDBF) then begin
                              TStr := RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8) + ',' + RealToString(lf.ExpandLAS_Z(j),-8,2) + ',' + IntToStr(lf.LASClassification(j));
                              if lf.SoloReturn(j) then ch := 'S'
                              else if lf.LastReturn(j) then ch := 'L'
                              else if lf.ReturnNumber(j) = 1 then ch := 'F'
                              else ch := 'I';
                              TStr := TStr + ',' + ch + ',' + IntToStr(lf.GetShotMeasuredIntensity(j)) + ',' + IntToStr(NPts);
                              csvout.Add(TStr);
                          end {else};
                       end {if PointIn};
                   end {if Nofilter};
                end {for j};
             end {for i};
          end;
          lf.Destroy;
      end;
      if ShowLasProgress then EndThreadTimers;

      if (ExportMode = exmKML) then begin
         NewName := Petmar.NextFileNumber(MDtempDir,'las_export_','.kml');
         kml.EndFolder;
         kml.CloseAndSaveFile(true,NewName);
         kml.Destroy;
      end
      else if (ExportMode = exmDBF) then begin
         CSVout.SaveToFile(NewName);
         CSVout.Free;
      end;
      {$IfDef RecordLASKML} WriteLineToDebugFile('tLas_files.KMLLasPoints out Pts=' + SmartNumberPoints(NPts)); {$EndIf}
   end {if};
end;


procedure tLas_files.SubdivideLasPointsByClassification;
var
   NewNonGround,NewVeg,NewBuilding,NewGround,NewOther,NewUnclassified : tCreateLasFile;
   I,j,k,RecsRead,Reads : integer;
   lf : tLAS_data;
   NewName,OutPath,
   fName : PathStr;


   procedure  CreateNew(Title : ShortString; var Creator : tCreateLasFile);
   begin
       Newname := OutPath + Title + CloudName + '.las';
       Creator := tCreateLasFile.Create;
       Creator.NewLasHeader := lf.LasHeader;
       Creator.CreateNewLASfile(NewName,lf.lasProjectionDefinition,lf.lasheader);
   end;


begin
   {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('tLas_files.SubdivideLasPointsByClassification in'); {$EndIf}
   Outpath := ExtractFilePath(Las_fNames.Strings[0]);
   GetDosPath('LAS class files',OutPath);

   {$IfDef VCL} StartThreadTimers('Class split LAS',1,true); {$EndIf}
   for k := 0 to pred(Las_fNames.Count) do begin
      {$IfDef VCL} ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count); {$EndIf}
      fName := Las_fNames.Strings[k];
      {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('file=' + fName); {$EndIf}
       lf := tLAS_data.Create(fName);
       if (k = 0) then begin
           {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('Create new LAS file'); {$EndIf}
           CreateNew('veg_',NewVeg);
           CreateNew('build_', NewBuilding);
           CreateNew('ground_',NewGround);
           CreateNew('non_ground_',NewNonGround);
           CreateNew('unclass_',NewUnclassified);
           CreateNew('other_',NewOther);
        end {if k};
        Reads := lf.ReadsRequired;

        for i := 0 to Reads do begin
           lf.ReadPoints(RecsRead);
           {$IfDef VCL} ThreadTimers.Gauge1.Progress := round(100 * i/Reads); {$EndIf}
           for j := 1 to RecsRead do begin
               case lf.LASClassificationCategory(j) of
                  lccUnclass : NewUnclassified.AddShotToOutputBuffer(lf,j);
                  lccGround  : NewGround.AddShotToOutputBuffer(lf,j);
                  lccVeg     : NewVeg.AddShotToOutputBuffer(lf,j);
                  lccBuilding : NewBuilding.AddShotToOutputBuffer(lf,j);
                  else NewOther.AddShotToOutputBuffer(lf,j)
                end;
                if lf.LASClassificationCategory(j) in [lccVeg,lccUnclass] then NewNonGround.AddShotToOutputBuffer(lf,j);
            end {for j};
       end {for i};
        lf.Destroy;
    end {for k};
   NewVeg.Destroy;
   NewBuilding.Destroy;
   NewGround.Destroy;
   NewNonGround.Destroy;
   NewOther.Destroy;
   NewUnclassified.Destroy;
   {$IfDef VCL} EndThreadTimers; {$EndIf}
end;


procedure tLas_files.ZeroLessUsefulFields;
var
   Simple,Aggressive,VeryAggressive : boolean;
   NewLAS : tCreateLasFile;
   I,j,k,RecsRead,Reads : integer;
   lf : tLAS_data;
   fName : PathStr;
   Prefix : shortString;
begin
   {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('tLas_files.ZeroLessUsefulFields in'); {$EndIf}

   Simple := AnswerIsYes('Simple removal');
   Aggressive := AnswerIsYes('Aggressive removal');
   VeryAggressive := AnswerIsYes('Very aggressive removal');

   if VeryAggressive then Prefix := 'zero_fields_v_agg_'
   else if Aggressive then Prefix := 'zero_fields_agg_'
   else if Simple then Prefix := 'zero_fields_mod_'
   else Prefix := 'zero_offset_';
   {$IfDef VCL} StartThreadTimers('Clear fields',1,true); {$EndIf}
   NewLas := Nil;
   for k := 0 to pred(Las_fNames.Count) do begin
      {$IfDef VCL} ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count); {$EndIf}
      fName := Las_fNames.Strings[k];
      {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('file=' + fName); {$EndIf}
       lf := tLAS_data.Create(fName);

       NewLas := tCreateLasFile.Create;
       NewLas.NewLasHeader := lf.LasHeader;
       NewLAS.CreateNewLASfile(ExtractFilePath(fName) + Prefix + ExtractFileName(fName),lf.lasProjectionDefinition,lf.LidarPointType);
       Reads := lf.ReadsRequired;

       for i := 0 to Reads do begin
          lf.ReadPoints(RecsRead);
          {$IfDef VCL} ThreadTimers.Gauge1.Progress := round(100 * i/Reads); {$EndIf}
          for j := 1 to RecsRead do begin
             if VeryAggressive then begin
                lf.LidarPoints0^[j].Classification := 0;
             end;
             if Aggressive then begin
                lf.LidarPoints0^[j].Intensity := 0;
                lf.LidarPoints0^[j].ReturnFlags := 0;
             end;
             lf.LidarPoints0^[j].ScanAngle := 0;
             lf.LidarPoints0^[j].UserData := 0;
             lf.LidarPoints0^[j].PointSourceID := 0;
             NewLas.AddShotToOutputBuffer(lf,j);
          end;
       end;
       lf.Destroy;
       NewLAS.Destroy;
    end;
    {$IfDef VCL} EndThreadTimers; {$EndIf}
end;


procedure tLas_files.ExtractGroundPoints(var NewName : PathStr);
var
   NewLAS : tCreateLasFile;
   I,j,k,RecsRead : integer;
   lf : tLAS_data;
   fName : PathStr;
begin
   {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('tLas_files.ExtractGroundPoints in, newname=' + NewName); {$EndIf}
   if (NewName = '') then begin
      NewName := MainMapData;
      Petmar.GetFileNameDefaultExt('LAS file','*.las',NewName,false);
   end;

   StartThreadTimers('Extract Ground',1,true);
   NewLas := Nil;
   for k := 0 to pred(Las_fNames.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
      fName := Las_fNames.Strings[k];
       lf := tLAS_data.Create(fName);
       if (NewLAS = Nil) then begin
          NewLas := tCreateLasFile.Create;
          NewLas.NewLasHeader := lf.LasHeader;
          NewLAS.CreateNewLASfile(NewName,lf.lasProjectionDefinition,lf.LidarPointType);
       end;
       for i := 0 to lf.ReadsRequired do begin
          lf.ReadPoints(RecsRead);
          ThreadTimers.Gauge1.Progress := round(100 * i/lf.ReadsRequired);
          for j := 1 to RecsRead do begin
             if (lf.LASClassification(j) = 2) then begin
                NewLas.AddShotToOutputBuffer(lf,j);
             end;
          end;
       end;
      lf.Destroy;
    end;
   if (NewLas <> Nil) then begin
      NewLAS.Destroy;
   end;
   EndThreadTimers;
end;


procedure tLas_files.SetClassificationToZero(var NewName : PathStr);
var
   NewLAS : tCreateLasFile;
   I,j,k,RecsRead : integer;
   lf : tLAS_data;
   fName : PathStr;
begin
   {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('tLas_files.SetClassificationToZero in'); {$EndIf}
   if (NewName = '') then begin
      NewName := MainMapData;
      Petmar.GetFileNameDefaultExt('LAS file','*.las',NewName,false);
   end;
   StartThreadTimers('Zero LAS classes',1,true);
   NewLas := Nil;
   for k := 0 to pred(Las_fNames.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * k / LAS_fnames.Count);
      fName := Las_fNames.Strings[k];

      {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('file=' + fName); {$EndIf}
       lf := tLAS_data.Create(fName);
          if (NewLAS = Nil) then begin
             {$IfDef RecordMergeLASfiles} WriteLineToDebugFile('Create new LAS file'); {$EndIf}
             NewLas := tCreateLasFile.Create;
             NewLas.NewLasHeader := lf.LasHeader;
             NewLAS.CreateNewLASfile(NewName,lf.lasProjectionDefinition,lf.LasHeader);
          end;
          for i := 0 to lf.ReadsRequired do begin
             lf.ReadPoints(RecsRead);
             ThreadTimers.Gauge1.Progress := round(100 * i/lf.ReadsRequired);
             for j := 1 to RecsRead do begin
                lf.LidarPoints0^[j].Classification := 0;
                NewLas.AddShotToOutputBuffer(lf,j);
             end;
          end;
         lf.Destroy;
    end;
   if (NewLas <> Nil) then begin
      NewLAS.Destroy;
   end;
   EndThreadTimers;
end;
//{$EndIf}


procedure ExtendBoundingBox(var MainBox : sfBoundBox; NewBox : sfBoundBox);
begin
   if NewBox.ymax > MainBox.ymax then MainBox.ymax := NewBox.ymax;
   if NewBox.ymin < MainBox.ymin then MainBox.ymin := NewBox.ymin;
   if NewBox.xmax > MainBox.xmax then MainBox.xmax := NewBox.xmax;
   if NewBox.xmin < MainBox.xmin then MainBox.xmin := NewBox.xmin;
end;

procedure tLas_files.FindCloudExtent;
begin
   RemoveTilesNotOnMap(Nil);
end;

procedure tLas_files.RemoveTilesNotOnMap(BaseMapDraw : tMapDraw);
var
   k,i,ToDo : integer;
   fname,BaseDir : PathStr;
   lf : tLAS_data;
begin
   {$IfDef Defined(RecordListFilesProcessed) or Defined(BasicOpens)} WriteLineToDebugFile('tLas_files.RemoveTilesNotOnMap in'); {$EndIf}
   MinPointDensity := 9999;
   MaxPointDensity := -9999;
   TotalCloudPts := 0;
   AveragePointDensity := 0;
   HasRGB := false;
   StartProgress('Find tiles on map');
   i := 0;
   InitializeBoundingBox(GeoBBox);
   InitializeBoundingBox(UTMBBox);
   {$IfDef RecordListFilesProcessed} if (BaseMapDraw <> Nil) then HighlightLineToDebugFile('Remove tiles, Map geo limits=' + sfBoundBoxToString(BaseMapDraw.MapCorners.BoundBoxGeo,6)
               + '   utm limits=' + sfBoundBoxToString(BaseMapDraw.MapCorners.BoundBoxUTM,1));  {$EndIf}
   ToDo := Las_fNames.Count;
   for k := pred(Las_fNames.Count) downto 0 do begin
      inc(i);
      UpdateProgressBar(i/ToDo);
      fName := Las_fNames.Strings[k];
      lf := tLAS_data.Create(fName);
      if lf.HasProjection then begin
         if (BaseMapDraw <> Nil) and (not lf.IsLASFileOnMap(BaseMapDraw)) then begin
            {$IfDef RecordListFilesProcessed} WriteLineToDebugFile('Not on map: ' + fName + ' LAS geo limits ' + sfBoundBoxToString(lf.LAS_LatLong_Box,6) + '  map geo limits=' + sfBoundBoxToString(BaseMapDraw.MapCorners.BoundBoxGeo,6)); {$EndIf}
            Las_fNames.Delete(k);
         end
         else begin
            UTMfiles := lf.lasProjectionDefinition.LASProjection.pName = UTMEllipsoidal;
            ExtendBoundingBox(GeoBBox,lf.LAS_LatLong_Box);
            ExtendBoundingBox(UTMBBox,lf.LAS_UTM_Box);
            if (lf.SingleFilePointDensity > MaxPointDensity) then MaxPointDensity := lf.SingleFilePointDensity;
            if (lf.SingleFilePointDensity < MinPointDensity) then MinPointDensity := lf.SingleFilePointDensity;
            AveragePointDensity := AveragePointDensity + lf.SingleFilePointDensity;
            TotalCloudPts := TotalCloudPts + lf.NumPointRecs;
            if (MinZ > lf.LasHeader.MinZ) then MinZ := lf.LasHeader.MinZ;
            if (MaxZ < lf.LasHeader.MaxZ) then MaxZ := lf.LasHeader.MaxZ;
            UTMZone := lf.UTMZone;
            LatHemi := lf.lasProjectionDefinition.LASProjection.LatHemi;
         end;
         {$IfDef RecordListFilesProcessed}
            WriteLineToDebugFile('On map: ' + fName + ' geo limits ' + sfBoundBoxToString(lf.LAS_LatLong_Box,6) + ' utm limits ' + sfBoundBoxToString(lf.LAS_UTM_Box,1) +
               '  z range=' + RealToString(lf.LasHeader.MinZ,-12,-2) + ' to ' + RealToString(lf.LasHeader.MaxZ,-12,-2) + '  Density=' + RealToString(lf.SingleFilePointDensity,-12,-2));
            WriteLineToDebugFile('Cloud limits now,  geo ' + sfBoundBoxToString(GeoBBox,6) + ' utm limits ' + sfBoundBoxToString(UTMBBox,1) +
               '  z range=' + RealToString(lf.LasHeader.MinZ,-12,-2) + ' to ' + RealToString(lf.LasHeader.MaxZ,-12,-2) + '  Density=' + RealToString(lf.SingleFilePointDensity,-12,-2));
         {$EndIf}
         if lf.RGBPresent then HasRGB := true;
      end
      else begin
         WriteLineToDebugFile('No projection: ' + fName);
         BaseDir := ExtractFilePath(fName) + 'problem_tiles\';
         SafeMakeDir(BaseDir);
         MoveFile(fName, BaseDir + ExtractFileName(fName));
         LAS_fnames.Delete(k);
      end;
      lf.Destroy;
   end;

   if (Las_fNames.Count > 0) then begin
      AveragePointDensity := AveragePointDensity / Las_fNames.Count;
      case MDDef.LasPointDensityBasedOn of
         0 : PointDensity := MaxPointDensity;
         1 : PointDensity := AveragePointDensity;
         2 : PointDensity := MinPointDensity;
      end;
   end;
   EndProgress;
   {$IfDef Defined(RecordListFilesProcessed) or Defined(BasicOpens)} WriteLineToDebugFile('tLas_files.RemoveTilesNotOnMap out, UTM zone=' + IntToStr(UTMzone) + LatHemi); {$EndIf}
end;


procedure tLas_files.ElevRangeOnMap(BaseMapDraw : tMapDraw; var MinZ,MaxZ : float64);
var
   k : integer;
   fName : PathStr;
   lf : tLAS_data;
begin
   MinZ := 9999;
   MaxZ := -9999;
   for k := 0 to pred(Las_fNames.Count) do begin
      fName := Las_fNames.Strings[k];
       lf := tLAS_data.Create(fName);
       if lf.IsLASFileOnMap(BaseMapDraw) then begin
          if (lf.LasHeader.MaxZ > MaxZ) then MaxZ := lf.LasHeader.MaxZ;
          if (lf.LasHeader.MinZ < MinZ) then MinZ := lf.LasHeader.MinZ;
       end;
       lf.Destroy;
   end;
end;

initialization
finalization
   {$IfDef RecordLASHist} WriteLineToDebugFile('RecordLASHist active in las_files_grouping'); {$EndIf}
end.

