unit dem_computations;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordGMTConvert}
      //{$Define PixelRectBackup}
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


procedure ShowVerticalEarthCurvature;
procedure ShowHorizontalEarthCurvature;
procedure StatsForTrainingSets;
procedure GetAtlasDirectories(var dbfDir,GridDir : PathStr);
procedure MakeDTEDTable;
procedure MakeUTMTable;
procedure PixelRectangles;
procedure DoGridSpacingAndDeclination(CurDEM : integer);



implementation

uses
   PetMath,PetImage,Petimage_form,petdbutils,
   Thread_timers,
   DEMeros,DEMdatabase,
   BaseMap,DEMCoord,DEM_Manager,DEMDef_routines;



procedure PixelRectangles;
var
   Lat : integer;
   Long : float64;
   Results : tStringList;
   fName : PathStr;

         procedure IvanGeodeticDxDy(Lat,PixSizeSeconds : float64; var dx,dy : float64);
         //from Marchesini, I., Ardizzone, F., Alvioli, M., Rossi, M., and Guzzetti, F.: Non-susceptible landslide areas in Italy and in the Mediterranean region,
         //      Nat. Hazards Earth Syst. Sci., 14, 2215–2231, https://doi.org/10.5194/nhess-14-2215-2014, 2014.
         var
            dx0 : float64;
         begin
            dx0 := 2 * Pi * WGS84DatumConstants.a * (PixSizeSeconds / 3600) / 360;
            dx := dx0 * cosDeg(Lat);
            dy := WGS84DatumConstants.a * (1 - WGS84DatumConstants.e2) * (PixSizeSeconds / 3600) / System.Math.Power((1 - (WGS84DatumConstants.e2 * sqr(sinDeg(Lat)))), 1.5) * DegToRad;
         end;

         function DoPixelSize(Secs : integer) : shortString;
         var
            d0,d1,d2,Bearing,Rect,dx,dy : float64;
         begin
            IvanGeodeticDxDy(Lat,Secs, dx,dy);
            VincentyCalculateDistanceBearing(Lat+ secs/3600,Long,Lat,Long,d0,Bearing);  //side of pixel
            VincentyCalculateDistanceBearing(Lat,Long,Lat,Long + secs/3600,d1,Bearing);  //bottom of pixel
            VincentyCalculateDistanceBearing(Lat + secs/3600,Long,Lat+secs/3600,Long + secs/3600,d2,Bearing);  //top of pixel
            Results.Add(RealToString(Lat,4,0) + ',' + IntegerToString(secs,8) + ',' + RealToString(d0,12,4) + ',' + RealToString(d1,12,4) + ',' + RealToString(d2,12,4) +
                  ',' + RealToString(100 * (d1-d2) / d1, -12,-8) + ',' + RealToString(d2/d0,-12,-4) + ',' + RealToString(dx,12,4) + ',' + RealToString(dy,12,4) );
         end;

begin
   Results := tStringList.Create;
   Results.Add('LAT,SEC,SIDE_M,BASE_M,TOP_M,ERROR_PC,RECT,IVAN_DX,IVAN_DY');
   long := 0;
   for lat := 85 downto 0 do begin
      DoPixelSize(1);
      DoPixelSize(3);
      DoPixelSize(30);
      DoPixelSize(60);
      DoPixelSize(round(7.5*60));
      DoPixelSize(3600);
   end;

   fName := Petmar.NextFileNumber(MDTempDir,'Rect_pixels_',DefaultDBExt);
   StringList2CSVtoDB(Results,fName);
end;



procedure MakeUTMTable;
var
   Results : tStringList;
   aLine : shortstring;
   UTM : tMapProjection;
   fName : PathStr;
   Lat : integer;
   Dist,GridTrueAngle,LongW,LongC,LongE  : float64;

         procedure DoMeridian(Long : float64);
         var
            xutm,yutm,Lat1,long1,lat2,long2 : float64;
         begin
            UTM.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
            UTM.UTMtoLatLongDegree(xutm,yutm-30,Lat1,Long1);
            UTM.UTMtoLatLongDegree(xutm,yutm+30,Lat2,Long2);
            VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Dist,GridTrueAngle);
            aLine := aLine + ',' + RealToString(Dist/2,-12,-3) + ',' + RealToString(GridTrueAngle,-8,-2);
   end;

begin
   Results := tStringList.Create;
   Results.Add('LAT,W_SPACE,W_ROTATE,MID_SPACE,MID_ROTATE,E_SPACE,E_ROTATE');

   UTM := tMapProjection.Create;
   UTM.StartUTMProjection(MDDef.DefaultUTMZone);
   LongC := UTM.Long0 / DegToRad;
   LongW := LongC - 3;
   LongE := LongC + 3;
   for Lat := 80 downto 0 do begin
      aLine := RealToString(Lat,-8,0);
      DoMeridian(LongW);
      DoMeridian(LongC);
      DoMeridian(LongE);
      Results.Add(aline);
   end;
    fName := Petmar.NextFileNumber(MDTempDir,'UTM_spacing_',DefaultDBExt);
    {$IfDef RecordHistogram} WriteLineToDebugFile('Convert and load table');    {$EndIf}
    StringList2CSVtoDB(Results,fName);
end;


procedure MakeDTEDTable;
var
   Lat : integer;
   fName : PathStr;
   Long,Distance1,Distance2,Distance3 : float64;
   Results : tStringList;
begin
   Results := tStringList.Create;
   Results.Add('Latitude,M_DEG_Y,M_DEG_X,M_DEG_AVG,M_SEC_Y,M_SEC_X,M_SEC_AVG');
  //Lat := 0;
   Long := -100;
   for Lat := 85 downto 0 do begin
      MetersPerDegree(Lat,Long,Distance1,Distance2,Distance3);
      Results.Add(IntToStr(Lat) + ',' +  RealToString(Distance1,-12,3) +  ',' + RealToString(Distance2,-12,3) +  ',' + RealToString(Distance3,-12,3)   + ',' +
            RealToString(Distance1/3600,-12,3) +  ',' + RealToString(Distance2/3600,-12,3) +  ',' + RealToString(Distance3/3600,-12,3));
   end;
   fName := MDTempDir + 'dem_gis_geo_factors.csv';
   PetDBUtils.StringList2CSVtoDB(Results,fName);
end;


procedure GetAtlasDirectories(var dbfDir,GridDir : PathStr);
begin
   StopSplashing;
   dbfDir := MainMapData + '0--current_projects\0_srtm_dbfs\';
   GridDir := MainMapData + '0--current_projects\0--srtm_grids\';
   if not PathIsValid(dbfDir) then GetDOSPath('DBFs',dbfDir);
   if not PathIsValid(GridDir) then GetDOSPath('atlas grids',GridDir);
end;


procedure StatsForTrainingSets;
var
   dbfDir,GridDir,fName : PathStr;
   Results,TheFiles,Classes : tStringList;
   i,j,GridsDB,db : integer;
   MomentVar : tMomentVar;
   Values : array[1..5000] of float32;
   Param : ShortString;
begin
   {$IfDef RecordGeostats} WriteLineToDebugFile('TPickGeoStat.Statsfortrainingset1Click in');  {$EndIf}
   GetAtlasDirectories(dbfDir,GridDir);
   GridsDB := OpenMultipleDataBases('',ExtractFilePath(GridDir) + 'param_stats.dbf');

   db := OpenMultipleDataBases('training set points');
   Classes := GISdb[db].MyData.UniqueEntriesInDB('NAME');

   TheFiles := Nil;
   Petmar.FindMatchingFiles(GridDir,'*.dem',TheFiles,0);

   Results := tStringList.Create;
   Results.Add('PARAM,NAME,CLASS,MEAN,STD_DEV,MIN,PERC_5,PERC_10,QUANT_25,MEDIAN,QUANT_75,PERC_90,PERC_95,MAX');

   StartThreadTimers('Stat',1);
   for i := 0 to pred(TheFiles.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * i/TheFiles.Count);
      fName := TheFiles.Strings[i];
      Param := UpperCase(ExtractFileNameNoExt(fName));
      {$IfDef RecordGeostats} WriteLineToDebugFile(Param);   {$EndIf}

      GISdb[GridsDB].MyData.ApplyFilter('PARAM=' + QuotedStr(Param));
      Results.Add(Param + ',WORLD,0,' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('MEAN'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('STD_DEV'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('MIN'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('PERC_5'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('PERC_10'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('QUANT_25'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('MEDIAN'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('QUANT_75'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('PERC_90'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('PERC_95'),-12,-4) + ',' +
                  RealToString(GISdb[GridsDB].MyData.GetFieldByNameAsFloat('MAX'),-12,-4) );

      for j := 0 to pred(Classes.Count) do begin
         {$IfDef RecordGeostats}  WriteLineToDebugFile('   ' + Classes.Strings[j]);   {$EndIf}
         GISdb[db].MyData.ApplyFilter('NAME=' + QuotedStr(Classes.Strings[j]));
         GISdb[db].EmpSource.Enabled := false;
         MomentVar.NPts := 0;
         while not GISdb[db].MyData.eof do begin
            inc(MomentVar.NPts);
            if not GISdb[db].MyData.CarefullyGetFieldByNameAsFloat32(Param,Values[MomentVar.NPts]) then begin
               dec(MomentVar.NPts);
            end;
            GISdb[db].MyData.Next;
         end;
         {$IfDef RecordGeostats} WriteLineToDebugFile('   ' + IntToStr(Npts)); {$EndIf}
         if MomentVar.NPts > 0 then begin
             HeapSort(MomentVar.NPts,Values);
             Moment(Values,MomentVar,msAll);
             Results.Add(Param + ',' + Classes.Strings[j] + ',' + IntToStr(succ(j)) + ',' +
                  RealToString(MomentVar.mean,-12,-4) + ',' + RealToString(MomentVar.sdev,-12,-4) + ',' +
                  RealToString(values[1],-12,-4) + ',' +
                  RealToString(Quantile(5,Values,MomentVar.NPts,true),-12,-4) + ',' +
                  RealToString(Quantile(10,Values,MomentVar.NPts,true),-12,-4) + ',' +
                  RealToString(Quantile(25,Values,MomentVar.NPts,true),-12,-4) + ',' +
                  RealToString(MomentVar.Median,-12,-4) + ',' +
                  RealToString(Quantile(75,Values,MomentVar.NPts,true),-12,-4) + ',' +
                  RealToString(Quantile(90,Values,MomentVar.NPts,true),-12,-4) + ',' +
                  RealToString(Quantile(95,Values,MomentVar.NPts,true),-12,-4) + ',' + RealToString(values[MomentVar.NPts],-12,-4));
         end;
      end;
   end;
   GISdb[db].MyData.ApplyFilter('');
   EndThreadTimers;
   Results.SaveToFile(ExtractFilePath(fName) + 'class_param_stats.csv');
   Results.Free;
   TheFiles.Free;
   Classes.Free;
end;


procedure DoGridSpacingAndDeclination(CurDEM : integer);
var
   Results,Summary : tStringList;
   x,y,GridInc,RecId,db,i : integer;
   fName : PathStr;
   JustCorners : boolean;
   dx_min,dx_max,dy_min,dy_max,grid_true_min,grid_true_max,Lat,Long : float64;
   dx,dy,GridTrueAngle : float32;

            procedure AddOne(CurDEM,x,y : integer);
            begin
               DEMGlb[CurDEM].PixelSpacingAndRotation(x,y,Lat,Long,dx,dy,GridTrueAngle,false);
               inc(RecID);
               Results.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7)  + ',' + IntToStr(x) + ',' + IntToStr(y) + ',' + RealToString(dx,-12,-3) + ',' + RealToString(dy,-12,-3)
                                + ',' + RealToString(GridTrueAngle,-12,-3) + ',' + RealToString(dx * dy,-12,-3) + ',' + RealToString(100 * dx / dy,-12,-3) + ',' + IntToStr(RecID) );
            end;


   procedure DoDEM(CurDEM : integer);
   var
      avgSpace : float64;
   begin
      RecID := 0;
      Results := tStringList.Create;
      Results.Add('LAT,LONG,GRID_COL,GRID_ROW,DX_M,DY_M,GRID_TRUE,AREA,DIFF_PC,REC_ID');
      if JustCorners then begin
         AddOne(CurDEM,0,pred(DEMGlb[CurDEM].DEMHeader.NumRow));
         AddOne(CurDEM,pred(DEMGlb[CurDEM].DEMHeader.NumCol),pred(DEMGlb[CurDEM].DEMHeader.NumRow));
         AddOne(CurDEM,pred(DEMGlb[CurDEM].DEMHeader.NumCol),pred(DEMGlb[CurDEM].DEMHeader.NumRow) div 2);
         AddOne(CurDEM,0,0);
         AddOne(CurDEM,pred(DEMGlb[CurDEM].DEMHeader.NumCol),0);
      end
      else begin
         x := 1;
         while x < pred(DEMGlb[CurDEM].DEMheader.NumCol) do begin
            y := 1;
            while y < pred(DEMGlb[CurDEM].DEMheader.NumRow) do begin
               AddOne(CurDEM,x,y);
               inc(y,GridInc);
            end;
            inc(x,GridInc);
         end;
      end;
      fName := Petmar.NextFileNumber(MDTempDir, 'Grid_spacing_','.dbf');
      db := DEMGlb[CurDEM].SelectionMap.StringListToLoadedDatabase(Results,fName);
      GISDB[db].EmpSource.Enabled := false;
      GISDB[db].MyData.FindFieldRange('DX_M',dx_min,dx_max);
      GISDB[db].MyData.FindFieldRange('DY_M',dy_min,dy_max);
      GISDB[db].MyData.FindFieldRange('GRID_TRUE',grid_true_min,grid_true_max);
      GISDB[db].EmpSource.Enabled := true;
      AvgSpace := (dx_min + dx_max + dy_min + dy_max) / 4;
      Summary.Add(RemoveUnderscores(DEMGlb[CurDEM].AreaName) + ',' + RealToString(dx_min,-12,3) + ',' + RealToString(dx_max,-12,3) + ',' + RealToString(dy_min,-12,3) + ',' +
          RealToString(dy_max,-12,3) + ',' + RealToString(grid_true_min,-12,3) + ',' + RealToString(grid_true_max,-12,3) + ',' + RealToString(dx_min,-12,3) + ' to ' + RealToString(dx_max,-12,3) + ',' +
          RealToString(dy_min,-12,3) + ' to ' + RealToString(dy_max,-12,3) + ',' + RealToString(grid_true_min,-12,3) + ' to ' + RealToString(grid_true_max,-12,3) + ',' + RealToString(avgspace,-12,-3));
   end;


begin
   Summary := tStringList.Create;
   Summary.Add('DEM,DX_MIN,DX_MAX,DY_MIN,DY_MAX,GRD_TR_MIN,GRD_TR_MAX,DX,DY,GRID_TRUE,AVG_SPACE');

   if (CurDEM <> 0) then begin
      JustCorners := AnswerIsYes('Just corners and center');
      if Not JustCorners then begin
         GridInc := 25;
         ReadDefault('Grid increment for computations',GridInc);
      end;
      DoDEM(CurDEM);
   end
   else begin
      JustCorners := true;
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) then begin
            CurDEM := i;
            DoDEM(CurDEM);
         end;
      end;
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'DEM_summary_','.dbf');
   DEMGlb[CurDEM].SelectionMap.StringListToLoadedDatabase(Summary,fName);
end;


procedure ShowHorizontalEarthCurvature;
const
   MaxPts = 24;
   LongVal : array[0..4] of float32 = (-2.9995,-1.5,0,1.5,2.9995);
var
   ThisGraph  : TThisBaseGraph;
   rf : array[1..5] of file;
   Lats,Longs,Dists : array[1..3] of Petmath.bfarray32;
   i,j,CurDEM,NPts : integer;
   Lat1,Long1,Lat2,Long2,
   Dist1,Dist2,Dist3,MaxVertValue,
   FullDistance,Bearing,Bearing2 : float64;
   MovieFile,Results : tStringList;
   DoMovie,DoTable,DoUTMGeodetic,DoLatLongGeodetic : boolean;
   bmp,BigBitmap : tMyBitmap;


      procedure DoDistance(MemoOutPut : boolean; FullDistance : float64; var rf : file; WhichDist : integer = 3);
      var
         v : tGraphPoint32;
         i,j : integer;
      begin
         if not MemoOutPut then ThisGraph.OpenDataFile(rf);
         Bearing := 0;
         while Bearing <= 361 do begin
            VincentyPointAtDistanceBearing(Lat1,Long1,FullDistance,Bearing,Lat2,Long2);
            NPts := MaxPts;
            for i := 1 to 3 do begin
               DEMGlb[CurDEM].GetStraightRouteLatLongDegree(Lat1,Long1,Lat2,Long2,tStraightAlgorithm(i),NPts,Lats[i],Longs[i],dists[i]);
            end;
            for j := 12 to 12 do begin
               VincentyCalculateDistanceBearing(Lats[1,j],Longs[1,j],Lats[2,j],Longs[2,j],Dist1,Bearing2);
               VincentyCalculateDistanceBearing(Lats[1,j],Longs[1,j],Lats[3,j],Longs[3,j],Dist2,Bearing2);
               VincentyCalculateDistanceBearing(Lats[3,j],Longs[3,j],Lats[2,j],Longs[2,j],Dist3,Bearing2);
               if MemoOutPut then
                  Results.Add(RealToString(FullDistance,8,0) + '/' + RealToString(Bearing,3,0) + RealToString(Dist1,18,1) + RealToString(Dist2,12,1) + RealToString(Dist3,15,1))
               else begin
                  v[1] := Bearing;
                  if (WhichDist = 2) then v[2] := Dist2
                  else v[2] := Dist3;
                  BlockWrite(rf,v,1);
               end;
            end;
            Bearing := Bearing + 5;
         end;
         if MemoOutPut then Results.Add('') else CloseFile(rf);
      end;


      procedure MakeAGraph(Title : string35; WhichDist : integer);
      var
         fName : PathStr;
      begin
         ThisGraph := TThisBaseGraph.Create(Application);
         ThisGraph.HideToolbar(true);
         ThisGraph.Caption := 'Horizontal earth curvature (' + Title + ') ' + LatLongDegreeToString(Lat1,Long1,MDDef.OutPutLatLongMethod);
         ThisGraph.GraphDraw.HorizLabel := 'Azimuth';
         ThisGraph.GraphDraw.VertLabel := 'Earth curvature (m)';
         DoDistance(false,10000,rf[1],WhichDist);
         DoDistance(false,25000,rf[2],WhichDist);
         DoDistance(false,50000,rf[3],WhichDist);
         DoDistance(false,100000,rf[4],WhichDist);
         DoDistance(false,150000,rf[5],WhichDist);

         ThisGraph.GraphDraw.MaxHorizAxis := 360;
         ThisGraph.GraphDraw.MaxVertAxis := MaxVertValue;
         ThisGraph.RedrawDiagram11Click(Nil);
         ThisGraph.Image1.Canvas.Font.Size := 16;
         ThisGraph.Image1.Canvas.Font.Style := [fsBold];
         ThisGraph.Image1.Canvas.TextOut(30,ThisGraph.Image1.Height - 25,LatLongDegreeToString(Lat1,Long1,NearestMinute));

         {$IfDef ExMovie}
         {$Else}
            if DoMovie then begin
               fName :=  NextFileNumber(MovieDir, 'horiz_curve_',OverlayFExt);
               PetImage.SaveImageAsBMP(ThisGraph.Image1,fName);
               MovieFile.Add(fName);
               ThisGraph.Close;
            end;
         {$EndIf}
      end;

      procedure ResultsForOneLocation;
      var
         x,y : integer;
      begin
         DEMGlb[CurDEM].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat1,Long1,x,y);
         ScreenSymbol(DEMGlb[CurDEM].SelectionMap.Image1.Canvas,x,y,FilledBox,5,claRed);

         if DoTable then begin
            Results := tStringList.Create;
            Results.Add('DEM: ' + DEMGlb[CurDEM].AreaName);
            Results.Add('Map center: ' + LatLongDegreeToString(Lat1,Long1,MDDef.OutPutLatLongMethod));
            Results.Add(' UTM zone: ' + IntToStr(DEMGlb[CurDEM].DEMMapProjection.projUTMZone));
            Results.Add('');
            Results.Add('Distance/Bearing   UTM/Lat-Long   UTM/Vincenty   Vincenty/LatLong');

            FullDistance := 10000;
            while (FullDistance < 150001) do begin
               DoDistance(true,FullDistance,rf[1]);
               FullDistance := FullDistance+ 10000;
            end;
            Petmar.DisplayAndPurgeStringList(Results,'Profile separation');
         end;
         if DoLatLongGeodetic then MakeAGraph('Geodetic/LatLong',3);
         if DoUTMGeodetic then MakeAGraph('Geodetic/UTM',2);
      end;

begin
   DoTable := false;
   DoMovie := AnswerIsYes('Do movie');

   if AnswerIsYes('UTM to geodetic') then begin
      DoUTMGeodetic := true;
      DoLatLongGeodetic := false;
   end
   else begin
      DoUTMGeodetic := false;
      DoLatLongGeodetic := true;
   end;

   if not GetDEM(CurDEM) then exit;
   if DoMovie then MovieFile := tStringList.Create;
   DEMGlb[CurDEM].ResetPrimaryDatumZone(DEMGlb[CurDEM].DEMSWcornerLong + 0.5 * DEMGlb[CurDEM].LongSizeMap);

   MaxVertValue := 30;

   if DoMovie then begin
      Lat1 := DEMGlb[CurDEM].DEMSWcornerLat + 0.5 * DEMGlb[CurDEM].LatSizeMap;
      Long1 := UTMZoneCentralLong(DEMGlb[CurDEM].DEMMapProjection.projUTMZone) - 2.995;
      ResultsForOneLocation;

      Long1 := UTMZoneCentralLong(DEMGlb[CurDEM].DEMMapProjection.projUTMZone) - 3;
      repeat
         Long1 := Long1 + 0.5;
         ResultsForOneLocation;
      until Long1 > UTMZoneCentralLong(DEMGlb[CurDEM].DEMMapProjection.projUTMZone) + 2.45;

      Long1 := UTMZoneCentralLong(DEMGlb[CurDEM].DEMMapProjection.projUTMZone) + 2.995;
      ResultsForOneLocation;

      {$IfDef ExMovie}
      {$Else}
         if DoMovie then begin
            MovieFile.SaveToFile(MovieDir + 'horiz_curve.mov');
            MovieFile.free;
            PetImage.MakeMovie('horiz_curve.mov');
         end;
      {$EndIf}
   end
   else begin
      CreateBitmap(BigBitmap,3032,2500);
      for i := 4 downto 0 do begin
         Lat1 := i * 15;
         if i = 0 then Lat1 := 5;
         if DoUTMGeodetic then begin
            for j := 0 to 4 do begin
               Long1 := UTMZoneCentralLong(DEMGlb[CurDEM].DEMMapProjection.projUTMZone) + LongVal[j];
               ResultsForOneLocation;
               CopyImageToBitmap(ThisGraph.Image1,bmp);
               bmp.savetofile(MDTempDir + 'temmpbmp' + OverlayFExt);
               BigBitmap.Canvas.Draw(j*605,(4-i)*500,bmp);
               bmp.Free;
               ThisGraph.Close;
               ThisGraph := Nil;
            end;
         end
         else begin
            for j := 0 to 2 do begin
               Long1 := UTMZoneCentralLong(DEMGlb[CurDEM].DEMMapProjection.projUTMZone);
               case j of
                  0 : MaxVertValue := 30;
                  1 : MaxVertValue := 250;
                  2 : MaxVertValue := 1000;
               end;
               ResultsForOneLocation;
               CopyImageToBitmap(ThisGraph.Image1,bmp);
               bmp.savetofile(MDTempDir + 'temmpbmp' + OverlayFExt);
               BigBitmap.Canvas.Draw(j*605,(4-i)*500,bmp);
               bmp.Free;
               ThisGraph.Close;
               ThisGraph := Nil;
            end;
         end;
      end;
      BigBitmap.Canvas.Font.Name := 'Verdana';
      BigBitmap.Canvas.Font.Size := 20;
      BigBitmap.Canvas.Font.Style := [fsBold];
      BigBitmap.Canvas.Font.Color := WinGraphColors[1];
      BigBitmap.Canvas.TextOut(0,2450,'10 km');
      BigBitmap.Canvas.Font.Color := WinGraphColors[2];
      BigBitmap.Canvas.TextOut(150,2450,'25 km');
      BigBitmap.Canvas.Font.Color := WinGraphColors[3];
      BigBitmap.Canvas.TextOut(300,2450,'50 km');
      BigBitmap.Canvas.Font.Color := WinGraphColors[4];
      BigBitmap.Canvas.TextOut(450,2450,'100 km');
      BigBitmap.Canvas.Font.Color := WinGraphColors[5];
      BigBitmap.Canvas.TextOut(600,2450,'150 km');

      BigBitmap.SaveToFile(MDTempDir + 'result' + OverlayFExt);
      {$IfDef ExMovie}
      {$Else}
         Petimage_form.DisplayBitmap(MDTempDir + 'result' + OverlayFExt,'Horizontal earth curvature');
      {$EndIf}
   end;
end;



procedure ShowVerticalEarthCurvature;
var
   ThisGraph  : TThisBaseGraph;
   NumMods,i,j : integer;
   rf : array[1..5] of file;
   v : tGraphPoint32;
   ca : array[1..5] of tVerticalCurvAlg;
   k : array[1..5] of float64;
   TStr : string35;
   VertFactor,HorizFactor : float64;
   IncludeRadio : boolean;

      procedure Segment(Start,Finish,Increment : float64);
      var
         Dist : float64;
         j : integer;
      begin
         Dist := Start;
         while (Dist <= Finish) do begin
            v[1] := Dist * HorizFactor;
            for j := 1 to NumMods do begin
               MDDef.CurvAlg := ca[j];
               MDDef.RadioK := k[j];
               v[2] := DropEarthCurve(1000*v[1]) * VertFactor;
               BlockWrite(rf[j],v,1);
            end;
            Dist := Dist + Increment;
         end;
      end;

begin
   DEMDef_Routines.SaveBackupDefaults;
   IncludeRadio := AnswerIsYes('Include Radio LOS');
   if IncludeRadio then NumMods := 5 else NumMods := 1;

   ThisGraph := TThisBaseGraph.Create(Application);
   ThisGraph.SetUpGraphForm;
   ThisGraph.Caption := 'Vertical earth curvature';

   if (MDDef.EnglishDistanceUnits = disMetric) then begin
      TStr := 'km';
      HorizFactor := 1;
   end
   else if (MDDef.EnglishDistanceUnits = disEnglish) then begin
      TStr := 'miles';
      HorizFactor := 0.62137;
   end
   else if (MDDef.EnglishDistanceUnits = disNautical) then begin
      TStr := 'nm';
      HorizFactor := 0.539956803;
   end;
   ThisGraph.GraphDraw.HorizLabel := 'Distance (' + Tstr+ ')';

   if MDdef.ElevDisplayMeters  then begin
      TStr := 'm';
      VertFactor := 1;
   end
   else begin
      TStr := 'ft';
      VertFactor := 1 /FeetToMeters;
   end;

   ThisGraph.GraphDraw.VertLabel := 'Earth curvature (' + Tstr+ ')';

   ThisGraph.GraphDraw.LegendList := tStringList.Create;
   ThisGraph.GraphDraw.LegendList.Add('TM5-441');
   ThisGraph.GraphDraw.LegendList.Add('Yoeli');
   ThisGraph.GraphDraw.LegendList.Add('Radio, k=0.67');
   ThisGraph.GraphDraw.LegendList.Add('Radio, k=4/3');
   ThisGraph.GraphDraw.LegendList.Add('Radio, k=10');
   for i := 1 to NumMods do begin
      ThisGraph.Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[i]);
      ThisGraph.OpenPointFile(rf[i],ThisGraph.Symbol);
   end;
   ca[1] := vcTM5441;
   ca[2] := vcYoeli;
   ca[3] := vcRadioLineOfSight;
   ca[4] := vcRadioLineOfSight;
   ca[5] := vcRadioLineOfSight;
   k[3] := 0.67;  //sub-standard, or earth bulge
   k[4] := 1.33;  //normal
   k[5] := 10;    //super-standard

   Segment(0.01,10,0.01);
   Segment(10,100,0.01);
   Segment(100,250,1);
   for j := 1 to NumMods do CloseFile(rf[j]);
   ThisGraph.AutoScaleAndRedrawDiagram;
   DEMDef_Routines.RestoreBackupDefaults;
end;



end.
