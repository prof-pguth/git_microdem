unit dem_nlcd;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


//originally NLCD was the only land cover avaiable for MICRODEM
//that has changed, but the name remains

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordNLCDProblems}
   //{$Define TrackWaterMasking}  //generally not needed, but helped finding a bad LC100 tile
   //{$Define RecordNLCDLegend}
   //{$Define RecordBarGraphs}
   //{$Define RecordDEMIX}
   //{$Define RecordBatch}
   //{$Define RecordBarGraphsDetailed}
   //{$Define RecordPaletteProblems}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

   Windows,Math,StrUtils,Forms, Menus, Graphics,SysUtils,Classes,
   Petmar_types,PETMAR,DEMDefs,DEMMapf;

type
   tCategory = record
      ShortName : ShortString;
      LongName  : string[128];
      Color     : tPlatformColor;
      UseCat    : boolean;
      UseStat   : boolean;
      Height    : float64;
   end;
   //tNLCDarray = array[0..MaxLandCoverCategories] of float64;
   tNLCDCats = array[0..MaxLandCoverCategories] of tCategory;

procedure SetUpNLCDCategories(AskLimit : boolean; LandCover : integer; var Categories : tNLCDCats);
function IsThisLandCover(fName : PathStr;  var LandCover : integer) : boolean;
procedure LandCoverBarGraphs(UseTable : boolean; Legend : boolean = true; MaxCat : boolean = true);
procedure LandCoverBarGraphLegends;

function MakeAnNLCDLegend(DEM : integer;  theLabel : shortstring = ''; Stats : tstringlist = nil) : integer;

function LoadLC100LandCover(fName : PathStr; bb : sfboundbox;OpenMap : boolean) : integer;

function ReclassifyLandCover(LandCoverGrid,Value : integer) : integer;

var
   LandCoverCatsUsed : array[0..MaxLandCoverCategories] of boolean;

const
   slcUndefined = 0;
   slcForest = 1;
   slcWater = 2;
   slcUrban = 3;
   slcBarren = 4;

function SimplifiedLandCover(LandCoverGrid : integer; Lat,Long : float32; var Value : float32) : integer;
procedure SimplifyLandCoverGrid(DEM : integer);
procedure MarkWaterMissingInAllOpenDEMs(DEM : integer; All : boolean = true);
procedure MarkWaterMissingInThisDEM(DEM : integer);



implementation


uses
  {$IfDef ExSat}
  {$Else}
      DEMEROS,
   {$EndIf}
   DEM_Manager, gdal_tools,DEMcoord,DEMdatabase,
   {$IfDef ExDEMIX}
   {$Else}
      demix_definitions,
      DEMIX_Control,
   {$EndIf}
   petimage_form,
   PetImage,PetMath,PetDBUtils,Toggle_db_use,nevadia_main,DEMDef_routines;


procedure MarkWaterMissingInThisDEM(DEM : integer);
begin
   MarkWaterMissingInAllOpenDEMs(DEM,false);
end;


procedure MarkWaterMissingInAllOpenDEMs(DEM : integer; All : boolean = true);
var
   j,lcGrid : integer;
   Fixed : int64;
   OpenMap : boolean;
begin
    {$IfDef TrackWaterMasking} OpenMap := True; {$Else} OpenMap := false; {$EndIf}
    lcgrid := LoadLC100LandCover('',DEMGlb[DEM].DEMBoundBoxGeo,OpenMap);
    {$IfDef TrackWaterMasking}
       DEMGlb[lcGrid].SelectionMap.DoBaseMapRedraw;
       MessageToContinue('Land cover opened');
    {$EndIf}
    if ValidDEM(lcGrid) then begin
       SimplifyLandCoverGrid(lcGrid);
       {$IfDef TrackWaterMasking} MessageToContinue('Simplified'); {$EndIf}
       DEMGLb[lcGrid].MarkInRangeMissing(slcWater-0.001,slcWater+0.001,Fixed,false);
       {$IfDef TrackWaterMasking}
          DEMGlb[lcGrid].SelectionMap.DoBaseMapRedraw;
          MessageToContinue('Water missing on water');
       {$EndIf}
       if All then begin
          for j := 1 to MaxDEMDataSets do begin
             if ValidDEM(j) and (j <> lcGrid) then begin
                EditsDone := 0;
                MaskStripFromSecondGrid(j,lcGrid, msSecondMissing);
                DEMGlb[j].CheckMaxMinElev;
             end;
          end;
       end
       else begin
          EditsDone := 0;
          MaskStripFromSecondGrid(DEM,lcGrid, msSecondMissing);
          DEMGlb[DEM].CheckMaxMinElev;
          if (DEMGlb[DEM].SelectionMap <> Nil) then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
          {$IfDef TrackWaterMasking} MessageToContinue('Water masked');  {$EndIf}
       end;
       CloseSingleDEM(lcGrid);
    end;
end;


procedure SimplifyLandCoverGrid(DEM : integer);
var
   x,y,Code : integer;
   z : float32;
begin
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].GetElevMetersOnGrid(x,y,z) then begin
            Code := ReclassifyLandCover(DEM,round(z));
            DEMGlb[DEM].SetGridElevation(x,y,Code);
         end;
      end;
   end;
   DEMGlb[DEM].DEMHeader.ElevUnits := euSimpleLandCover;
   DEMGlb[DEM].CheckForLandCover;
   if (DEMGlb[DEM].SelectionMap <> Nil) then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
end;


function ReclassifyLandCover(LandCoverGrid,Value : integer) : integer;
begin
   Result := slcUndefined;
   if (LandCoverGrid <> 0) then begin
      if (DEMGlb[LandCoverGrid].DEMHeader.ElevUnits = euGLCS_LC100) then begin
         if Value in [111..126] then Result := slcForest
         else if Value in [30,60,70] then Result := slcBarren
         else if Value in [50] then Result := slcUrban
         else if Value in [80,200] then Result := slcWater;
      end;
   end;
end;


function SimplifiedLandCover(LandCoverGrid : integer; Lat,Long : float32; var Value : float32) : integer;
var
   z : float32;
begin
   Result := slcUndefined;
   if ValidDEM(LandCoverGrid) then begin
      if DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
         if (DEMGlb[LandCoverGrid].DEMHeader.ElevUnits = euGLCS_LC100) then begin
            Result := ReclassifyLandCover(LandCoverGrid,round(z));
         end;
      end;
   end;
end;


function LoadLC100LandCover(fName : PathStr; bb : sfboundbox; OpenMap : boolean) : integer;
var
   Lat,Long : float32;
begin
   Lat := 0.5 * (bb.YMax + bb.YMin);
   Long := 0.5 * (bb.XMax + bb.XMin);
   LandCoverFName := GetLC100_fileName(Lat,Long);
   {$IfDef RecordDEMIX} writeLineToDebugFile('Landcover=' + LandCoverfName); {$EndIf}
   if FileExists(LandCoverFName) then begin
      Result := GDALsubsetGridAndOpen(bb,true,LandCoverFName,OpenMap);
      if ValidDEM(Result) then begin
         DEMGlb[Result].DEMHeader.ElevUnits := euGLCS_LC100;
         if (fName = '') then fName := Petmar.NextFileNumber(MDtempDir,'lcc100_','.dem');
         DEMGlb[Result].SaveAsGeotiff(fName);
      end
      else begin
         {$IfDef RecordDEMIX} HighlightLineToDebugFile('Landcover load fail ' + LandCoverfName + '  ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
      end;
   end
   else begin
      MessageToContinue('Missing land cover file ' + LandCoverFName);
   end;
end;


function MakeAnNLCDLegend(DEM : integer; theLabel : shortstring = ''; Stats : tstringlist = nil) : integer;
var
   x,y,zi,MaxCat : integer;
   z,Max,pc : float32;
   CatStr,FirstLine,NextLine : ANSIString;
   Title : shortstring;
   fName : PathStr;
   Results : tStringList;
   Total : int64;
   Count :  array[1..MaxLandCoverCategories] of int64;
begin
   {$IfDef TrackNLCD} WriteLineToDebugFile('TMapForm.NLCDLegend1Click in'); {$EndIf}
   if (DEMGLB[DEM].SelectionMap.MapDraw.ValidDEMonMap and (DEMGlb[DEM].LandCoverGrid)) then begin
      for x := 1 to MaxLandCoverCategories do Count[x] := 0;
      Total := 0;
      StartProgress('Land cover');
      for x := round(DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.xmin) to round(DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.xmax) do begin
         if (x mod 400 = 0) then UpDateProgressBar( (x-DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.xmin) /
            (DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.xmax - DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.xmin));
         for y := round(DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.ymin) to round(DEMGLB[DEM].SelectionMap.MapDraw.Mapcorners.BoundBoxDataGrid.ymax) do begin
            if DEMGlb[DEM].GetElevMetersOnGrid(x,y,z) then begin
               zi := round(z);
               if (zi > 0) and (zi <= MaxLandCoverCategories) and (DEMGlb[DEM].NLCDCats^[zi].UseStat) then begin
                  inc(Count[zi]);
                  inc(Total);
                  LandCoverCatsUsed[zi] := true;
               end;
            end;
         end;
      end;
      EndProgress;

      if (Total = 0) then begin
         Result := 0;
      end
      else begin
         Results := tStringList.Create;
         Title := 'PERCENT,NAME,COLOR,CODE';
         if MDDef.LongLandCoverResults then Title := Title +  ',CATEGORY,NUMBER';
         Results.Add(Title);
         if (Stats <> Nil) then begin
            if (Stats.Count = 1) then begin
               FirstLine := Stats.Strings[0];
               //NameStr := Petmar_types.BeforeSpecifiedCharacterANSI(FirstLine,',',true,true);
               CatStr := FirstLine;
               FirstLine := 'NAME,' + CatStr + '_MAX,MAJOR_CAT,MAJOR_NAME';
               for x := 1 to MaxLandCoverCategories do if (DEMGlb[DEM].NLCDCats^[x].LongName <> '') then begin
                  FirstLine := FirstLine + ',' + CatStr + '_' + IntToStr(x);
               end;
            end
            else FirstLine := '';
         end;
         Max := 0;
         for x := 1 to MaxLandCoverCategories do if (Count[x] > 0) and (DEMGlb[DEM].NLCDCats^[x].LongName <> '') then begin
            pc := 100 * Count[x]/Total;
            if (pc > Max) then begin
               Max := pc;
               MaxCat := x;
            end;

            Title := RealToString(pc,8,2) + ',' + DEMGlb[DEM].NLCDCats^[x].LongName + ',' + IntToStr(ConvertPlatformColorToTColor(DEMGlb[DEM].NLCDCats^[x].Color)) +
                ',' + IntToStr(x);
            if MDDef.LongLandCoverResults then Title := Title + ',' + IntToStr(x) + ',' + IntToStr(Count[x]);
            Results.Add(Title);
            {$IfDef TrackNLCD} WriteLineToDebugFile(Title); {$EndIf}
         end;
         if (Stats <> Nil) then begin
            if (FirstLine <> '') then begin
               Stats.Clear;
               Stats.Add(FirstLine);
            end;
            NextLine := TheLabel + ',' +  RealToString(max,-8,3) + ',' + IntToStr(MaxCat) + ',' + DEMGlb[DEM].NLCDCats^[MaxCat].LongName;
            for x := 1 to MaxLandCoverCategories do if (DEMGlb[DEM].NLCDCats^[x].LongName <> '') then begin
               NextLine := NextLine + ',' + RealToString(100 * Count[x]/Total,-8,3);
            end;
            Stats.Add(NextLine);
         end;

         Title := ElevUnitsAre(DEMGlb[DEM].DEMheader.ElevUnits) + '_' + DEMGlb[DEM].AreaName;
         fName := Petmar.NextFileNumber(MDTempDir, Title + '_','.dbf');
         Result := StringList2CSVtoDB(Results,fName);
      end;
   end;
   {$IfDef TrackNLCD} WriteLineToDebugFile('TMapForm.NLCDLegend1Click out'); {$EndIf}
end;

procedure LandCoverBarGraphLegends;
var
   OutPath : PathStr;
   db : integer;

   procedure ASeries(Name,Title : shortstring);
   var
     SaveName : PathStr;
   begin
      GISdb[db].ApplyGISFilter('SERIES=' + QuotedStr(Name));
      SaveName := OutPath + Title + '.png';
      GISdb[db].CreatePopupLegend(RemoveUnderscores(Title),SaveName);
   end;

begin
   try
      GetDEMIXpaths;
      OpenNumberedGISDataBase(db,LandCoverSeriesFName,false);
      OutPath := DEMIX_Base_DB_Path + 'test_land_class\';
      SafeMakeDir(OutPath);
      ASeries('CCI-LC','LCCS_300_m_2015');
      ASeries('Iwahashi','Iwahashi_and_Pike');
      ASeries('Meybeck','Meybeck_and_others');
      ASeries('Geomorphon','Geomorphons');
   finally
      CloseAllDatabases;
      EndDEMIXProcessing;
      {$If Defined(RecordBatch) or Defined(RecordDEMIX)} WriteLineToDebugFile('LandCoverBarGraphs out'); {$EndIf}
   end;
end;


procedure LandCoverBarGraphs(UseTable : boolean; Legend : boolean = true; MaxCat : boolean = true);
const
   TopSize = 40;
   EntryHeight = 32;
   AreaBlank = 12;
   IncludeTileName : boolean = true;
   IncludeTiles : boolean = false;
   IncludeLegend : boolean = false;
   IncludeAreaName : boolean = true;
var
   dbName,outPath : PathStr;
   What : shortstring;


   procedure ASeries(LandCoverfName : PathStr; aTitle,ShortName : shortstring);
   var
      NumDrawn,AreaWidth,TileWidth,
      j,top: integer;
      singleleg,bmp : tMyBitmap;
      Table : tMyData;
      Areas,SeriesStats : tStringList;

         function DoOne(bbox : sfBoundBox; aLabel : shortstring) : boolean;
         var
            fName : PathStr;
            NewDEM,NewDB : integer;
         begin
            {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Do series=' + aLabel); {$EndIf}
            try
               Result := true;
               NewDEM := GDALsubsetGridAndOpen(bbox,true,LandCoverfName,true);
               {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Grid opened'); {$EndIf}
               if ValidDEM(NewDEM) then begin
                  NewDB := DEMGlb[NewDEM].SelectionMap.MakeNLCDLegend(aLabel,SeriesStats);
                  {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Legend made'); {$EndIf}
                  inc(NumDrawn);  //outside loop in case nothing drawn
                  if ValidDB(NewDB) then begin
                     if (not IncludeLegend) then aLabel := '';
                     SingleLeg := GISDB[NewDB].BarGraphLegend(false,aLabel);
                     {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Bar graph legend made'); {$EndIf}
                     bmp.Canvas.Draw(AreaWidth + TileWidth,Top,SingleLeg);
                     SingleLeg.Destroy;
                     fName := GISDB[NewDB].dbFullName;
                  end
                  else begin
                     Result := false;
                     fName := '';
                  end;
               end
               else begin
                  {$IfDef RecordBatch} HighlightLineToDebugFile('No data for ' + aLabel); {$EndIf}
                  Result := false;
               end;
            finally
               CloseAndNilNumberedDB(NewDB);
               DeleteFileIfExists(fName);
               CloseSingleDEM(NewDEM);
               {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Do One Out'); {$EndIf}
            end;
         end;

          procedure AddTitle(Xloc : integer = 0);
          begin
             bmp.canvas.font.Size := 20;
             bmp.canvas.font.Style := [fsBold];
             bmp.Canvas.TextOut(5 + XLoc,0,aTitle);
             bmp.Canvas.Font.Size := 14;
          end;

         function DEMIXLoadRefDEMFromPath(AreaName : shortstring; LoadMap : boolean) : integer;
         var
            FilesWanted : tStringList;
            j : integer;
            fName : PathStr;
         begin
            FilesWanted := tStringList.Create;
            FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',FilesWanted,1);
            for j := 0 to pred(FilesWanted.Count) do begin
               fName := FilesWanted.Strings[j];
               if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
                  Result := OpenNewDEM(fName,LoadMap);
                  exit;
               end;
            end;
            FilesWanted.Free;
         end;


   var
      Name,Area,AreaName : shortstring;
      ItsDEMIX : boolean;
      i,RefDEM : integer;
      fName : PathStr;
   begin
       {$If Defined(RecordBatch) or Defined(RecordDEMIX)} HighLightLineToDebugFile('Do series=' + LandCoverfName); {$EndIf}
       NumDrawn := 0;
       for j := 0 to MaxLandCoverCategories do LandCoverCatsUsed[j] := false;
       FindDriveWithFile(LandCoverfName);
       if UseTable then begin
          Table := tMyData.Create(dbName);
          ItsDEMIX := Table.FieldExists('DEMIX_TILE') and Table.FieldExists('AREA');
          if ItsDEMIX then begin
             CreateBitmap(bmp,2400,1200);  //100 + (Table.RecordCount * EntryHeight) + (pred(Areas.Count) * AreaBlank));
             bmp.Canvas.Font.Size := 14;
             AreaWidth := 0;
             TileWidth := 0;
             SeriesStats := tStringList.Create;
             Areas := tStringList.Create;
             Table.First;
             while not Table.eof do begin
                Area := Table.GetFieldByNameAsString('AREA');
                if (Areas.IndexOf(Area) = -1) then begin
                   Areas.Add(Area);
                end;
                Table.Next;
             end;
             Top := 100;
             for i := 0 to pred(Areas.Count) do begin
                Table.ApplyFilter('AREA=' + QuotedStr(Areas.Strings[i]));
                if IncludeAreaName then begin
                   j := Bmp.Canvas.TextWidth(Table.GetFieldByNameAsString('AREA'));
                   if (j > AreaWidth) then AreaWidth := j;
                end;
                if IncludeTiles then begin
                   while not Table.Eof do begin
                      Top := Top + EntryHeight;
                      if IncludeTileName then begin
                         j := Bmp.Canvas.TextWidth(Table.GetFieldByNameAsString('DEMIX_TILE'));
                         if (j > TileWidth) then TileWidth := j;
                      end;
                      Table.Next;
                   end;
                end
                else Top := Top + EntryHeight;
                Top := Top + AreaBlank;
             end;
             bmp.Height := Top;
          end;
          {$If Defined(RecordBarGraphs) or Defined(RecordBatch)} WriteLineToDebugFile('Create bitmap ' + BitmapSize(bmp)); {$EndIf}
          Top := TopSize;
          if ItsDEMIX then begin
             AreaWidth := AreaWidth + 15;
             TileWidth := TileWidth + 15;
             {$IfDef RecordBatch} WriteLineToDebugFile('AreaWidth=' + IntToStr(AreaWidth) + '  TileWidth=' + IntToStr(TileWidth)); {$EndIf}
             AddTitle(AreaWidth + TileWidth);

             for i := 0 to pred(Areas.Count) do begin
                WMDEM.SetPanelText(2,IntToStr(i) + '/' + IntToStr(Areas.Count) + ' ' + Areas.Strings[i] );
                Table.ApplyFilter('AREA=' + QuotedStr(Areas.Strings[i]));
                AreaName := Table.GetFieldByNameAsString('AREA');
                Name := RemoveUnderScores(AreaName);
                {$IfDef RecordBarGraphs} WriteLineToDebugFile('Area=' + Name + ' at x=' + IntToStr(5) + '  y=' + IntToStr(Top)); {$EndIf}
                if (SeriesStats.Count = 0) then begin
                   SeriesStats.Add(ShortName{ + ',' + Name});
                end;
                if IncludeTiles then begin
                   if IncludeAreaName then Bmp.Canvas.TextOut(5,Top + Table.FiltRecsInDB div 2 * EntryHeight,Name);
                   while not Table.Eof do begin
                      Name := Table.GetFieldByNameAsString('DEMIX_TILE');
                      WMDEM.SetPanelText(3,Name);
                      if DoOne(DEMIXtileBoundingBox(Name),Name) then begin
                      end
                      else begin
                         {$IfDef RecordBatch} HighLightLineToDebugFile('No land type ' + Name); {$EndIf}
                      end;
                      if IncludeTileName then Bmp.Canvas.TextOut(AreaWidth + 5,Top,Name);
                      Top := Top + EntryHeight;
                      Table.Next;
                   end;
                   {$IfDef RecordBarGraphs} WriteLineToDebugFile('Tile=' + Name + ' at x=' + IntToStr(AreaWidth + 5) + '  y=' + IntToStr(Top)); {$EndIf}
                end
                else begin
                    if IncludeAreaName then Bmp.Canvas.TextOut(5,Top,Name);
                    RefDEM := DEMIXLoadRefDEMFromPath(AreaName,true);
                    DoOne(DEMGlb[RefDEM].DEMBoundBoxGeo,AreaName);
                    CloseSingleDEM(RefDEM);
                    Top := Top + EntryHeight;
                end;
                Top := Top + AreaBlank;
             end;
             {$IfDef RecordBatch} WriteLineToDebugFile('End DEMIX tiles'); {$EndIf}
          end
          else begin
             AddTitle;
             while not Table.Eof do begin
                if DoOne(Table.GetRecordBoundingBox,Table.GetFieldByNameAsString('NAME')) then Top := Top + EntryHeight;
                Table.Next;
             end;
          end;
          Table.Destroy;
       end
       else begin
          CreateBitmap(bmp,1200,100 + NumDEMDataSetsOpen * 60);
          AddTitle;
          for j := 1 to MaxDEMDataSets do begin
            if ValidDEM(j) then begin
               if DoOne(DEMGlb[j].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,DEMGlb[j].AreaName) then Top := Top + EntryHeight;
            end;
          end;
       end;
      {$IfDef RecordBatch} WriteLineToDebugFile('Drawing done bitmap, ' + BitmapSize(bmp)); {$EndIf}
      PutBitmapInBox(bmp);
      {$IfDef RecordBatch} WriteLineToDebugFile('Got image part bitmap, ' + BitmapSize(bmp)); {$EndIf}
      BMP.SaveToFile(OutPath + What + '_' + ShortName + '.png');
      DisplayBitmap(Bmp,What + '_' + ShortName + ' Land cover distribution');
      fName := OutPath + What + '_' + ShortName + '.dbf';
      StringList2CSVtoDB(SeriesStats,fName);
   end;

begin
   {$If Defined(RecordBatch) or Defined(RecordDEMIX)} WriteLineToDebugFile('LandCoverBarGraphs in'); {$EndIf}
   try
      GetDEMIXpaths;
      OutPath := DEMIX_Base_DB_Path + 'test_land_class\';
      SafeMakeDir(OutPath);
      IncludeTiles := true;
      What := 'Tiles';
      if UseTable and (not GetFileFromDirectory('bounding box data base','*.dbf',dbName)) then exit;
      ASeries('d:\landcover\lccs_300m\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif','LCCS 300 m 2015','LCCS');
      ASeries('d:\landcover\iwahashi\iwahashi.tif','Iwahashi and Pike','IP');
      ASeries('d:\landcover\Meybeck\Meybeck_1km1.tif','Meybeck and others','MEYB');
      ASeries('d:\landcover\Geomorphon\geomorphon_1KMmaj_GMTEDmd.tif','Geomorphons','GEOM');
   finally
      CloseAllDatabases;
      EndDEMIXProcessing;
      {$If Defined(RecordBatch) or Defined(RecordDEMIX)} WriteLineToDebugFile('LandCoverBarGraphs out'); {$EndIf}
   end;
end;



function IsThisLandCover(fName : PathStr;  var LandCover : integer) : boolean;
begin
   Landcover := 0;
   if (fName <> '') then begin
      fName := UpperCase(FName);
      if StrUtils.AnsiContainsText(fName,'CHANGE') and StrUtils.AnsiContainsText(fName,'NLCD') then LandCover := euNLCD_Change
      else if UpperCase(Copy(ExtractFileName(fName),1,7)) = 'NLCD_20' then LandCover := euNLCD2001up
      else if StrUtils.AnsiContainsText(fName,'GLC-2000') then LandCover := euGLC2000
      else if (StrUtils.AnsiContainsText(fName,'NLCD') and StrUtils.AnsiContainsText(fName,'1990')) then LandCover := euNLCD1992
      else if StrUtils.AnsiContainsText(fName,'NLCD') then LandCover := euNLCD2001up
      else if StrUtils.AnsiContainsText(fName,'LANDFIRE') and (not StrUtils.AnsiContainsText(fName,'EVT')) then LandCover := euLandFire
      else if StrUtils.AnsiContainsText(fName,'S2GLC') then LandCover := euS2GLC
      else if StrUtils.AnsiContainsText(fName,'ESRI2020') then LandCover := euESRI2020
      else if StrUtils.AnsiContainsText(fName,'ESACCI-LC-L4-LCCS') then LandCover := euCCI_LC
      else if StrUtils.AnsiContainsText(fName,'LC100_GLOBAL') then LandCover := euGLCS_LC100
      else if StrUtils.AnsiContainsText(fName,'ESA_WORLDCOVER_10M') then LandCover := euWorldCover10m
      else if StrUtils.AnsiContainsText(fName,'GLOBCOVER') then LandCover := euGLOBCOVER
      else if StrUtils.AnsiContainsText(fName,'CCAP') then LandCover := euCCAP
      else if StrUtils.AnsiContainsText(fName,'GEOMORPHON') then LandCover := euGeomorphon
      else if StrUtils.AnsiContainsText(fName,'PENNOCK') then LandCover := euPennock
      else if StrUtils.AnsiContainsText(fName,'IWAHASHI') then LandCover := euIwahashi
      else if StrUtils.AnsiContainsText(fName,'MEYBECK') then LandCover := euMeybeck
      else if (StrUtils.AnsiContainsText(fName,'COPERNICUS_DSM') and StrUtils.AnsiContainsText(fName,'EDM.TIF')) then LandCover := euCOPEDM
      else if (StrUtils.AnsiContainsText(fName,'TDM1_EDEM_') and StrUtils.AnsiContainsText(fName,'EDM.TIF')) then LandCover := euTANEDM
      else if (StrUtils.AnsiContainsText(fName,'COPERNICUS_DSM') and StrUtils.AnsiContainsText(fName,'FLM.TIF')) then LandCover := euCOPFLM
      //else if StrUtils.AnsiContainsText(fName,'Sentinel-2_L2A_Scene_classification_map') then LandCover := euSent2SLC      //it is a 3 color scene
      else if StrUtils.AnsiContainsText(fName,'LCMAP') then LandCover := euLCMAP;
   end;
   Result := (LandCover <> 0);
end;



procedure SetUpNLCDCategories(AskLimit : boolean; LandCover : integer; var Categories : tNLCDCats);
var
   CatTable : tMyData;

   procedure SetCategories(var Categories : tNLCDCats; LandCoverName : shortstring);
   var
      i : integer;
   begin
      for i := 0 to MaxLandCoverCategories do begin
         Categories[i].ShortName := '';
         Categories[i].LongName  := '';
         Categories[i].Color     := claWhite;
         Categories[i].UseCat    := false;
         Categories[i].UseStat   := false;
      end;
      CatTable.ApplyFilter( 'SERIES = ' + QuotedStr(LandCoverName));
      if (CatTable.RecordCount > 0) then begin
         if AskLimit then VerifyRecordsToUse(CatTable,'LONG_NAME','Fields to mask');
         {$IfDef RecordNLCDLegend} WriteLineToDebugFile('CatTable.Filter: ' + CatTable.Filter); {$EndIf}
         repeat
            i := CatTable.GetFieldByNameAsInteger('CATEGORY');
            Categories[i].ShortName := CatTable.GetFieldByNameAsString('SHORT_NAME');
            Categories[i].LongName  := CatTable.GetFieldByNameAsString('LONG_NAME');
            Categories[i].Color     := CatTable.PlatformColorFromTable;
            Categories[i].UseCat    := CatTable.GetFieldByNameAsString('USE') = 'Y';
            Categories[i].UseStat   := CatTable.GetFieldByNameAsString('STAT') = 'Y';
            if CatTable.GetFieldByNameAsString('HEIGHT') = '' then Categories[i].Height := 0 else Categories[i].Height := CatTable.GetFieldByNameAsFloat('HEIGHT');
            {$IfDef RecordNLCDLegend} WriteLineToDebugFile(IntToStr(i) + '  ' + LongName +  '  ' + Petmar.ColorString(Color)); {$EndIf}
            CatTable.Next;
         until CatTable.eof;
      end
      else begin
          MessageToContinue('Land cover ' + IntToStr(LandCover) + ' missing in ' + LandCoverFName);
      end;
   end;

   function LandCoverName(LandCover : integer) : shortstring;
   begin
      case LandCover of
         euNLCD_Change  : Result := 'NLCD-change';
         euCCI_LC  : Result := 'CCI-LC';
         euGLOBCOVER  : Result := 'GLOBCOVER';
         euNLCD1992  : Result := 'NLCD-1990';
         euNLCD2001up  : Result := 'NLCD-2001up';
         euS2GLC  : Result := 'S2GLC';
         euWorldCover10m  : Result := 'WorldCover10m';
         euLandFire  : Result :=  'LANDFIRE'   ;
         //euGLCS_LC100  : Result := 'CGLS-LC100';
         euCCAP  : Result :=  'CCAP';
         euMeybeck  : Result := 'MEYBECK';
         euESRI2020  : Result := 'ESRI2020';
         euIwahashi  : Result := 'IWAHASHI';
         euGeomorphon  : Result := 'GEOMORPHON';
         euPennock  : Result := 'PENNOCK';
         euLCMAP  : Result := 'LCMAP';
         //euSent2SLC  : Result := 'Sent2SLC';
         //euCOP  : Result :=     ;
         euCOPEDM : Result := 'COP-EDM';
         euCOPFLM : Result := 'COP-FLM';
         euGLC2000 : Result := 'GLC2000';
         euGLCS_LC100 : Result := 'GLCS-LC100';
         euSent2SLC : Result := 'L2A_scene_class';
         euTANEDM : Result := 'TAN-EDM';
      end;
   end;


begin
   {$IfDef RecordNLCDLegend} WriteLineToDebugFile('tLandCoverImage.SetUpCategories, Landcover=' + LandCover); {$EndIf}
   CatTable := tMyData.Create(LandCoverSeriesFName);
   SetCategories(Categories,LandCoverName(LandCover));
   CatTable.Destroy;
   {$IfDef RecordNLCDLegend} WriteLineToDebugFile('tLandCoverImage.SetUpCategories out'); {$EndIf}
end;

initialization
finalization
  {$IfDef RecordNLCDProblems}    WriteLineToDebugFile('RecordNLCDProblems in DEM_NLCD'); {$EndIf}
  {$IfDef RecordNLCDLegend}      WriteLineToDebugFile('RecordNLCDLegend in DEM_NLCD'); {$EndIf}
  {$IfDef RecordPaletteProblems} WriteLineToDebugFile('RecordPaletteProblems in DEM_NLCD'); {$EndIf}
end.


