unit dem_indexes;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
  //{$Define WarnMapIndexes}
  {$IFDEF DEBUG}
     //{$Define RecordMultipleFilesInBoundingBox}
     //{$Define RecordIndexProhlems}
     //{$Define RecordLoadMapLibraryBox}
     //{$Define MergeSummary}
     //{$Define TrackPixelIs}
     //{$Define RecordDuplicatesInIndex}
     //{$Define TrackDEMCorners}
     //{$Define RecordIndex}
     //{$Define LoadLibrary}
     //{$Define RecordAutoZoom}
     //{$Define RecordImageIndex}
     //{$Define RecordIndexFileNames}
     //{$Define RecordMerge}
     {$Define RecordMergeDetails}
     //{$Define RecordTimeMerge}
     //{$Define RecordIndexImagery}
     //{$Define RecordClosing}
     //{$Define RecordTileNameDecoding}
     //{$Define ListIndexFileName}
   {$EndIf}
{$EndIf}


interface


uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client,FireDAC.Comp.Dataset,FireDAC.Phys.SQLite,FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

   VCL.Controls,
   SysUtils,  Classes, Graphics,StdCtrls,Forms, StrUtils,
   System.Math,System.UITypes,
   Petmar_types,PETMAR,PETMath,DEMDefs,BaseMap;


function LoadMapLibraryBox(Load : boolean; bb : sfBoundBox; WantSeries : shortstring = ''; DisplayIt : boolean = true) : integer; //overload;
function LoadMapLibraryPoint(Load : boolean; Lat,Long : float64; WantSeries : shortstring = ''; DisplayIt : boolean = true) : integer;
function MergeMultipleDEMsHere(var DEMList : TStringList; DisplayIt,GDALversion : boolean; MergefDir : PathStr = '') : integer;
procedure MergeDEMs(Mode : integer);

procedure GetMapLibraryDataLimits(var MinLat,MaxLat,MinLong,MaxLong : float64);
procedure CreateMapLibrary(Memo1 : tMemo);
procedure ShowMapLibraryDataAtPoint(Lat,Long : float64);

function GetListOfDataInBoxInSeries(Series : shortString; bb : sfBoundBox) : tStringList; //overload;

procedure AdjustMapLibraryDataBaseSeries;
procedure PickDEMSeries(var sName : ShortString; WhatFor : shortstring);

procedure DefineShapeFileGrouping(fName : PathStr);
procedure SetUpDataBaseForOperations(AllowNoDataMap : boolean = false);

procedure CreateShapeFileGrouping(var fName : PathStr; var TheGroupingIndex : tMyData; Long : boolean; ShapeType : integer = 0);

function DataTypeFileName : PathStr;
function SeriesIndexFileName  : PathStr;
function MapLibraryFName  : PathStr;
procedure OpenIndexedSeriesTable(var IndexSeriesTable : tMyData);
procedure CreateLandsatIndex(Browse : boolean);

function PickMapIndexLocation : boolean;

procedure CheckDEMFileForMapLibrary(TheTable : Petmar_db.tMyData; Series : PathStr; var fName : PathStr);
procedure VerifyMapLibraryFilesExist(theTable : Petmar_db.tMyData; Memo1 : tMemo = Nil);

procedure DiluviumDEMIXtileReport;


var
   MergeSeriesName : shortstring;


implementation

uses
   BaseGraf,
   PETDBUtils,
   Petlistf,
   PetImage,
   DEMcoord,
   Toggle_db_use,
   DEMDef_routines,
   Make_Tables,
   Geotiff,
   DataBaseCreate,
   DEM_Manager,
   DEMMapf,
   DEMESRIShapeFile,
   gdal_tools,

   {$IfDef ExPointCloud}
   {$Else}
      Las_Lidar,
   {$EndIf}
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}
   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}

   Nevadia_Main;




procedure DiluviumDEMIXtileFill(DEM : integer; var sl : tStringList);
var
   Col,Row,db : integer;
   Pts,Coastal,Ocean,Highlands,DeepWater : int64;
   bbgrid : tGridLimits;
   z : float32;
   bb : sfBoundBox;
   fName : PathStr;
begin
   {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEMIXtileFill in, DEM=' + IntToStr(DEM)); {$EndIf}
   if ValidDEM(DEM) then try
      db := LoadDEMIXtileOutlinesNoMap(fName,DEMGlb[DEM].DEMBoundBoxGeo,false,false,false);
      if ValidDB(db) then try
         {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEMIXtileFill, db created recs=' + IntToStr(GISdb[Result].MyData.TotRecsInDB)); {$EndIf}
         GISdb[db].MyData.First;
         GISdb[db].EmpSource.Enabled := false;
         while not GISdb[db].MyData.eof do begin
            Pts := 0;
            Coastal := 0;
            Ocean := 0;
            Highlands := 0;
            DeepWater := 0;
            bb := GISdb[db].MyData.GetRecordBoundingBox;
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,bbgrid.xgridlow,bbgrid.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,bbgrid.xgridhigh,bbgrid.ygridhigh);
            for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
               //UpdateProgressBar((bbgrid.xgridhigh - Col) / (bbgrid.xgridhigh - bbgrid.xgridlow));
               for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                  inc(Pts);
                  if DEMGlb[DEM].MissingDataInGrid(Col,Row) then inc(Highlands)
                  else begin
                     if DEMGlb[DEM].GetElevMetersOnGrid(Col,Row,z) then begin
                        if z < -0.01 then inc(DeepWater)
                        else if z < 0.01 then inc(Ocean)
                        else inc(Coastal);
                     end;
                  end;
               end;
            end;

            if (Coastal > 1) then begin
               sl.Add(GISdb[db].MyData.GetFieldByNameAsString('NAME') + ',' +
                      RealToString(0.5 * (bb.YMax + bb.YMin),-12,-3)+ ',' +
                      RealToString(0.5 * (bb.xMax + bb.xMin),-12,-3)+ ',' +
                      RealToString(100 * Ocean / Pts,-12,-3)+ ',' +
                      RealToString(100 * DeepWater / Pts,-12,-3)+ ',' +
                      RealToString(100 * Coastal / Pts,-12,-3)+ ',' +
                      RealToString(100 * Highlands / Pts,-12,-3) );
            end;
            {$IfDef RecordDEMIXTiles} writeLineToDebugFile('File boundary, ' + GridLimitsToString(bbgrid) + RealToString(Full,8,2)); {$EndIf}
            GISdb[db].MyData.Next;
         end;
      finally
      end;
   finally
      CloseAndNilNumberedDB(db);
   end;
   {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEM contains parts of tiles=' + IntToStr(GISdb[Result].MyData.TotRecsInDB)); {$EndIf}
end;


procedure DiluviumDEMIXtileReport;
var
   sl : tStringList;
   DEM,i : integer;
   MapLib : tMyData;
   fName : PathStr;
begin
   if FileExists(MapLibraryFName) then begin
      try
         HeavyDutyProcessing := true;
         sl := tStringList.Create;
         sl.Add('DEMIX_TILE,LAT,LONG,OCEAN,DEEPWATER,COASTAL,HIGHLANDS');
         fName := MapLibraryFName;
         MapLib := tMyData.Create(fName);
         MapLib.ApplyFilter('SERIES=' + QuotedStr('DILUV'));
         //StartProgress('DEMIX tiles summary for DILUV');
         i := 0;
         while not MapLib.eof do begin
            wmDEM.SetPanelText(2,IntToStr(i) + '/' + IntToStr(MapLib.FiltRecsInDB));
            inc(i);
            DEM := OpenNewDEM(MapLib.GetFieldByNameAsString('FILENAME'),false);
            DiluviumDEMIXtileFill(DEM,sl);
            sl.SaveToFile(mdTempDir + 'floods.csv');
            CleanUpTempDirectory(false);
            MapLib.Next;
         end;
         fname := NextFileNumber(MDTempDir,'diluvium_dem_report_','.dbf');
         StringList2CSVtoDB(sl,fName);
      finally
         HeavyDutyProcessing := false;
      end;
   end;
end;



function PickMapIndexLocation : boolean;
begin
   Result := FindPath('Map library',':\mapdata\indexed_data\',MapLibDir);
   SaveMDdefaults;
end;


function BoundingBoxFromFileName(fName : PathStr; var bb : sfBoundBox) : boolean;
var
   WorkName,WorkingString : ANSIstring;
   x,TileSize : integer;

   function Decode : boolean;
   begin
      Result := false;
      if (Length(workingString) = 7) then begin
         if ((WorkingString[1] = 'N') or (WorkingString[1] = 'S')) and ((WorkingString[4] = 'E') or (WorkingString[4] = 'W')) then begin
            bb.xmin := StrToInt(Copy(WorkingString,5,3));
            if WorkingString[4] = 'W' then bb.xmin := -bb.xmin;
            bb.ymin := StrToInt(Copy(WorkingString,2,2));
            if WorkingString[1] = 'S' then bb.ymin := -bb.ymin;
            bb.xmax := bb.xmin + TileSize;
            bb.ymax := bb.ymin + TileSize;
            Result := true;
         end
         else  if ((WorkingString[5] = 'N') or (WorkingString[5] = 'S')) and ((WorkingString[1] = 'E') or (WorkingString[1] = 'W')) then begin
            bb.xmin := StrToInt(Copy(WorkingString,2,3));
            if WorkingString[1] = 'W' then bb.xmin := -bb.xmin;
            bb.ymin := StrToInt(Copy(WorkingString,6,2));
            if WorkingString[5] = 'S' then bb.ymin := -bb.ymin;
            bb.xmax := bb.xmin + TileSize;
            bb.ymax := bb.ymin + TileSize;
            Result := true;
         end;
      end;
   end;

begin
   {$IfDef RecordTileNameDecoding} WriteLineToDebugFile('Decode ' + fName); {$EndIf}
   WorkName := UpperCase(ExtractFileNameNoExt(fName));
   Result := false;
   if Length(WorkName) < 7 then exit;
   try
      if StrUtils.AnsiContainsText(UpperCase(fName),'MERIT') then TileSize := 5
      else if StrUtils.AnsiContainsText(UpperCase(fName),'_XSAR_') then TileSize := 10
      else TileSize := 1;

      if (Length(WorkName) = 7) then begin
         WorkingString := WorkName;
         Result := decode;
         if Result then exit;
      end;

      if StrUtils.AnsiContainsText(WorkName,'COPERNICUS') then begin  //Copernicus_DSM_10_N21_00_E024_00_DEM,  Copernicus_DSM_COG_10_N21_00_E024_00_DEM
         x := Pos('_N',WorkName);
         if x = 0 then x := Pos('_S',WorkName);
         if x <> 0 then begin
            WorkingString := Copy(WorkName,x+1,3);
            x := Pos('_E',WorkName);
            if x = 0 then x := Pos('_W',WorkName);
            if x <> 0 then begin
               WorkingString := WorkingString + Copy(WorkName,x+1,4);
               Result := decode;
               if Result then exit;
            end;
         end;
      end;

      if StrUtils.AnsiContainsText(WorkName,'NASADEM_HGT_') then begin
         WorkingString := Copy(WorkName,13,7);
         Result := decode;
         exit;
      end;

      if StrUtils.AnsiContainsText(WorkName,'_XSAR_') then begin
         WorkingString := Copy(WorkName,1,7);
         Result := decode;
         exit;
      end;


      if StrUtils.AnsiContainsText(WorkName,'ASTGTMV003_') then begin  //aster ASTGTMV003_N18W156_dem
         WorkingString := Copy(WorkName,11,7);
         Result := decode;
         exit;
      end;

      if StrUtils.AnsiContainsText(WorkName,'DiluviumDEM') then begin
         WorkingString := Copy(WorkName,13,3) + Copy(WorkName,20,4);
         Result := decode;
         exit;
      end;


      if (WorkName[4] = '_') then begin  //srtm n09_e038_1arc_v3.tif
         WorkingString := Copy(WorkName,1,3) + Copy(WorkName,5,4);
         Result := decode;
         if Result then exit;
      end;

      WorkingString := Copy(WorkName,1,7);    //ALOS   N021W160_AVE_DSM.tif
      Result := decode;
      if Result then exit;

      WorkingString := WorkName[1] + Copy(WorkName,3,6);   //ALOS   N021W160_AVE_DSM.tif  with superflous 0 in all Latitudes
      Result := decode;
      if Result then exit;

      if StrUtils.AnsiContainsText(WorkName,'TDM') then begin  //TanDEM-X  TDM1_DEM__04_N27W018_DEM.tif
         if StrUtils.AnsiContainsText(WorkName,'_N') then begin  //TanDEM-X  TDM1_DEM__04_N27W018_DEM.tif
            x := Pos('_N',WorkName);
            WorkingString := Copy(WorkName,x+1,7);
            Result := decode;
            if Result then exit;

            WorkingString := WorkName[x+1] + Copy(WorkName,x+3,6);
            Result := decode;
            if Result then exit;
         end;

         if StrUtils.AnsiContainsText(WorkName,'_S') then begin  //TanDEM-X  TDM1_DEM__04_N27W018_DEM.tif
            x := Pos('_S',WorkName);
            WorkingString := Copy(WorkName,x+1,7);
            Result := decode;
            if Result then exit;

            WorkingString := WorkName[x+1] + Copy(WorkName,x+3,6);
            Result := decode;
            if Result then exit;
         end;
      end;
   finally
   end;
   {$IfDef RecordTileNameDecoding} WriteLineToDebugFile('Decode failure=' + fName); {$EndIf}
end;


function AuxDEMTif(fName : PathStr) : boolean;
begin
   fName := UpperCase(fName);
   Result := StrUtils.AnsiContainsText(fname,'_STK.TIF') or StrUtils.AnsiContainsText(fname,'_MSK.TIF') or    //ALOS aux files
             StrUtils.AnsiContainsText(fname,'_NUM.TIF');                                                     //Aster aux files
end;


function DataTypeFileName : PathStr;
begin
   Result := MapLibDir + 'data_types' + DefaultDBExt;
end;

function SeriesIndexFileName : PathStr;
begin
   Result := MapLibDir + 'indexed_series' + DefaultDBExt;
end;

function MapLibraryFName : PathStr;
begin
   Result := MapLibDir + 'data_index' + DefaultDBExt;
end;

procedure OpenIndexedSeriesTable(var IndexSeriesTable : tMyData);
var
   fName : PathStr;
begin
   if (not FileExists(SeriesIndexFileName)) then PickMapIndexLocation;

   fName  := SeriesIndexFileName;
   IndexSeriesTable := tMyData.Create(fName);
   IndexSeriesTable.InsureFieldPresentAndAdded(ftInteger,'PIXEL_IS',2);
   IndexSeriesTable.InsureFieldPresentAndAdded(ftString,'VERT_DATUM',20);
end;


procedure AdjustMapLibraryDataBaseSeries;
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('AdjustIntegratedDataBaseSeries in, ' + SeriesIndexFileName); {$EndIf}
   Toggle_db_use.VerifyRecordsToUse(SeriesIndexFileName,'SERIES','Use indexed series ' + SeriesIndexFileName,'USE','DATA_TYPE','DATA_TYPE');
end;



procedure PickDEMSeries(var sName : ShortString; WhatFor : shortstring);
var
   Table : tMyData;
   PickNum : integer;
   fName : PathStr;
   aList : tStringList;
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('PickDEMSeries in'); {$EndIf}
   sName := '';
   if FileExists(SeriesIndexFileName) then begin
      fName := SeriesIndexFileName;
      Table := tMyData.Create(fName);
      Table.ApplyFilter('DATA_TYPE=' + QuotedStr('DEMS') + ' AND NUM_FILES > 0');
      aList := Table.ListUniqueEntriesInDB('SERIES');
      Table.Destroy;
      {$IfDef RecordIndex} WriteLineToDebugFile('DEM series count=' + IntToStr(aList.Count)); {$EndIf}
      if (aList.Count = 0) then MessageToContinue('No indexed DEMs')
      else begin
         PickNum := 0;
         if Petmar.MultiSelectSingleColumnStringList('Series for ' + WhatFor,PickNum,alist, true) then sName := alist.Strings[PickNum]
         else sName := '';
      end;
      aList.Free;
   end
   else MessageToContinue('Missing ' + SeriesIndexFileName)
end;


procedure SetUpDataBaseForOperations(AllowNoDataMap : boolean = false);
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('DEM_indexes.SetUpDataBaseForOperations in'); {$EndIf}
(*
   OpenIndexDataOnline;
   {$IfDef RecordIndex} WriteLineToDebugFile('db loaded, recs=' + IntToStr(IndexDataOnline.FiltRecsInDB)); {$EndIf}
   if (IndexDataOnline = Nil) or ((IndexDataOnline.FiltRecsInDB = 0) and (Not AllowNoDataMap)) then begin
      MessageToContinue('No data in selected series');
      CloseIndexDataOnline;
      exit;
   end;
   CloseIndexDataOnline;
*)
   if AllowNoDataMap then exit;
   {$IfDef RecordIndex} WriteLineToDebugFile('checking map status'); {$EndIf}
   ChangeDEMNowDoing(OpenMapsFromLibrary);
   {$IfDef RecordIndex} WriteLineToDebugFile('DEM_indexes.SetUpDataBaseForOperations out'); {$EndIf}
end;


procedure InsertMapLibraryRecord(TheTable : tMyData; FileName,Series : shortString; bb : sfBoundBox);
begin
   TheTable.Insert;
   TheTable.SetFieldByNameAsString('FILENAME',FileName);
   TheTable.SetFieldByNameAsString('SERIES',Series);
   TheTable.SetRecordBoundingBox(bb);
   TheTable.Post;
end;


procedure CheckDEMFileForMapLibrary(TheTable : Petmar_db.tMyData; Series : PathStr; var fName : PathStr);
var
   WantedDEM : integer;
   bb : sfBoundBox;
begin
   WantedDEM := 0;
   {$IfDef RecordIndexFileNames} WriteLineToDebugFile('check: ' + fName); {$EndIf}
   if MDDef.DeleteAuxTiffFiles and AuxDEMTif(fName) then begin
      File2Trash(fName);
   end
   else begin
      if BoundingBoxFromFileName(fName,bb) then begin
         InsertMapLibraryRecord(TheTable,fName,Series,bb);
      end
      else begin
         if NewArea(true,WantedDEM,'',FName,WantedDEM) and ValidDEM(WantedDEM) then begin
            {$IfDef RecordIndexFileNames} WriteLineToDebugFile(' DEM open OK'); {$EndIf}
            InsertMapLibraryRecord(TheTable,fName,Series,DEMglb[WantedDEM].DEMBoundBoxGeo);  //   DEMGlb[WantedDEM].DEMSWcornerLat,DEMGlb[WantedDEM].DEMSWcornerLong,DEMGlb[WantedDEM].DEMSWcornerLat+DEMGlb[WantedDEM].LatSizemap,DEMGlb[WantedDEM].DEMSWcornerLong+DEMGlb[WantedDEM].LongSizemap);
            DEM_Manager.CloseALLDEMs;
         end;
      end;
   end;
end;


procedure VerifyMapLibraryFilesExist(theTable : Petmar_db.tMyData; Memo1 : tMemo = Nil);
var
   NumFound,Tested,NumRenamed : integer;
   fName : PathStr;
   Missing : tStringList;
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('TDemHandForm.Verifyfilesexist1Click'); {$EndIf}
   if (Memo1 <> nil) then Memo1.Lines.Add('Check Map Library ' + MapLibraryFName);
   WMdem.Color := clInactiveCaption;
   NumFound := 0;
   NumRenamed := 0;
   Tested := 0;
   Missing := tStringList.Create;
   StartProgress('Check for missing files');
   while not TheTable.Eof do begin
       inc(Tested);
       if (Tested mod 500 = 0) then UpdateProgressBar(Tested/TheTable.RecordCount);
       fName := TheTable.GetFieldByNameAsString('FILENAME');
       if (not FileExists(fName)) then begin
          fName[1] := MapLibraryFName[1];
          if FileExists(fName) then begin
             TheTable.Edit;
             TheTable.SetFieldByNameAsString('FILENAME',fName);
             inc(NumRenamed);
          end
          else begin
             Missing.Add(fName);
             TheTable.Delete;
             inc(NumFound);
          end;
          TheTable.Next;
       end
       else if StrUtils.AnsiContainsText(fName,'original_') or AuxDEMTif(fName) then begin
          TheTable.Delete;
          inc(NumFound);
       end
       else begin
          TheTable.Next;
       end;
   end;
   if (Memo1 <> nil) then begin
      Memo1.Lines.Add('Removed missing files: ' + IntToStr(NumFound));
      if (NumRenamed > 0) then Memo1.Lines.Add('Renamed on wrong drive: ' + IntToStr(NumRenamed));
      Memo1.Lines.Add('Valid files remaining: ' + IntToStr(TheTable.RecordCount));
   end;
   DisplayAndPurgeStringList(Missing,'Missing files',true);
   EndProgress;
   WMdem.Color := clScrollBar;
end;



procedure CreateMapLibrary(Memo1 : tMemo);
var
   IndexSeriesTable,DataTypeTable : tMyData;


       procedure UpdateIndexSeriesTable(DataType,Series : shortstring; k,nf : integer);
       begin
            Series := UpperCase(Series);
            IndexSeriesTable.ApplyFilter('SERIES=' + QuotedStr(Series) + 'AND DATA_TYPE=' + QuotedStr(DataType));
            if (IndexSeriesTable.RecordCount = 0) then begin
               {$IfDef RecordIndex} WriteLineToDebugFile('First example of series ' + Series); {$EndIf}
               IndexSeriesTable.Insert;
               IndexSeriesTable.SetFieldByNameAsString('DATA_TYPE',DataType);
               IndexSeriesTable.SetFieldByNameAsString('SERIES',Series);
               IndexSeriesTable.SetFieldByNameAsString('USE','Y');
               IndexSeriesTable.SetFieldByNameAsInteger('NUM_FILES',nf);
               IndexSeriesTable.SetFieldByNameAsInteger('COLOR',WinGraphColors[k mod 15]);
               IndexSeriesTable.Post;
            end
            else begin
               IndexSeriesTable.Edit;
               IndexSeriesTable.SetFieldByNameAsInteger('NUM_FILES',nf);
               IndexSeriesTable.Post;
            end;
       end;


      procedure CreateUpdateIntegratedIndex(var TheTable : tMyData);
      var
         Dirs,RawData : TStringList;
         fName : PathStr;
         Series : ShortString;
         Ext : ExtStr;
         Min,Max : float64;
         i : integer;

           {$IfDef ExSat}
           {$Else}
              (*
              procedure CheckImageryFile(fName : PathStr);
              var
                 Success : boolean;
                 SatView : tSatView;
                 bb : sfBoundBox;
              begin
                 {$IfDef RecordIndexFileNames} WriteLineToDebugFile(fName); {$EndIf}
                     SatImage[1] := tSatImage.Create(SatView,Nil,fName,False,Success);
                     if Success then begin
                        bb := SatImage[1].SatelliteBoundBoxGeo(1);
                        InsertMapLibraryRecord(TheTable,fName,Series,bb);  //bb.ymin,bb.XMin,bb.YMax,bb.xmax);
                        SatImage[1].Destroy;
                     end;
                end;
                *)
             {$EndIf}


         procedure DataTypeTableInsert(What : string16);
         begin
            DataTypeTable.ApplyFilter('DATA_TYPE=' + QuotedStr(What));
            if (DataTypeTable.RecordCount = 0) then begin
               {$IfDef RecordIndex} WriteLineToDebugFile('DataTypeTableInsert must add ' + What); {$EndIf}
               DataTypeTable.Insert;
               DataTypeTable.SetFieldByNameAsString('DATA_TYPE',What);
               DataTypeTable.SetFieldByNameAsString('USE','Y');
               DataTypeTable.Post;
            end;
            DataTypeTable.ApplyFilter('');
         end;


         procedure IndexMapLibraryDataType(DataType : ShortString);
         var
            i,k,Where : integer;
            AlreadyIndexed : tStringList;
         begin
            {$IfDef RecordIndex} WriteLineToDebugFile('IndexMapLibraryDataType ' + DataType); {$EndIf}
            Memo1.Lines.Add(TimeToStr(Now) + '   ' + DataType);
            Dirs := GetSubDirsInDirectory(MapLibDir + DataType + '\');
            for k := 0 to pred(Dirs.Count) do begin
              Series := UpperCase(Dirs.Strings[k]);
              IndexSeriesTable.ApplyFilter('SERIES=' + QuotedStr(Series));
              wmdem.StatusBar1.Panels[1].Text := '';
              wmDEM.SetPanelText(0,'Process: ' + Series);
              RawData := Nil;
              FindMatchingFiles(MapLibDir + DataType + '\' + Series,'*.*',RawData,6);
              Memo1.Lines.Add(' ' + TimeToStr(Now) + ' Process: ' + Series + '  files= ' + IntToStr(RawData.Count));
              {$IfDef RecordIndex} WriteLineToDebugFile('Index series=' + Series); {$EndIf}
              ReallyReadDEM := false;
              TheTable.ApplyFilter('SERIES=' + QuotedStr(Series));
              AlreadyIndexed := TheTable.ListUniqueEntriesInDB('FILENAME');
              try
                 if (RawData.Count <> TheTable.RecordCount) then begin
                    for i := 0 to pred(RawData.Count) do begin
                       if (i mod 25) = 0 then begin
                          wmDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(pred(RawData.Count)));
                          CleanUpTempDirectory(false);
                          {$IfDef RecordIndex} if (i mod 500) = 0 then  WriteLineToDebugFile(IntToStr(i) + '/' + IntToStr(pred(RawData.Count))); {$EndIf}
                       end;
                       fName := RawData.Strings[i];
                       {$IfDef ListIndexFileName} Memo1.Lines.Add(fName); WriteLineToDebugFile(fName); {$EndIf}
                       Ext := UpperCase(ExtractFileExt(FName));
                       Where := AlreadyIndexed.IndexOf(fName);
                       if (Where = -1) then begin
                          {$IfDef ListIndexFileName} WriteLineToDebugFile(' file not indexed already'); {$EndIf}

                          {$ifDef ExSat}
                          {$Else}
                             (*
                             if ((DataType = 'IMAGERY') or (DataType = 'DRGS') ) and ValidImageryExt(ext) then begin
                                CheckImageryFile(fName);
                             end;
                             *)
                          {$EndIf}

                          if (DataType = 'DEMS') {or (DataType = 'BATHY'))} and ValidDEMExt(ext) then begin
                             if StrUtils.AnsiContainsText(fName,'original_') then begin
                                SysUtils.DeleteFile(fName);
                             end
                             else begin
                                CheckDEMFileForMapLibrary(TheTable,Series,fName);
                             end;
                          end;
                       end
                       else begin
                          AlreadyIndexed.Delete(Where);
                          {$IfDef ListIndexFileName} WriteLineToDebugFile(' file indexed already'); {$EndIf}
                       end;
                    end;
                 end;
              except
                 on Exception do begin end;
              end;
              AlreadyIndexed.Destroy;
              UpdateIndexSeriesTable(DataType,Series,k,RawData.Count);
              RawData.Free;
            end;
            ReallyReadDEM := true;
            Dirs.Free;
         end;


      begin
         {$IfDef RecordIndex} WriteLineToDebugFile('Integrated indexing in'); {$EndIf}
         CleanUpTempDirectory;
         ShowDEMReadingProgress := false;
         SkipMenuUpdating := true;

         if not FileExists(SeriesIndexFileName) then begin
            {$IfDef RecordIndex} WriteLineToDebugFile('Create ' + SeriesIndexFileName); {$EndIf}
            CreateIndexSeriesFile(SeriesIndexFileName);
         end;
         OpenIndexedSeriesTable(IndexSeriesTable);
         {$IfDef RecordIndex} WriteLineToDebugFile('Current series: ' + IntToStr(IndexSeriesTable.RecordCount)); {$EndIf}
         if not FileExists(DataTypeFileName) then begin
            CreateDataUseTable(DataTypeFileName);
         end;
         fName := DataTypeFileName;
         IndexMapLibraryDataType('DEMS');

         {$IfDef RecordIndex} WriteLineToDebugFile('All data types done'); {$EndIf}

         IndexSeriesTable.ApplyFilter('');
         IndexSeriesTable.First;
         while not IndexSeriesTable.eof do begin
            TheTable.ApplyFilter('SERIES=' + QuotedStr(IndexSeriesTable.GetFieldByNameAsString('SERIES')));
            if (TheTable.RecordCount > 0) then begin
               IndexSeriesTable.Edit;
               IndexSeriesTable.SetFieldByNameAsInteger('NUM_FILES',TheTable.RecordCount);
               if TheTable.FindFieldRange('LAT_LOW',Min,Max) then IndexSeriesTable.SetFieldByNameAsFloat('LAT_LOW',Min);
               if TheTable.FindFieldRange('LAT_HI',Min,Max) then IndexSeriesTable.SetFieldByNameAsFloat('LAT_HI',Max);
               if TheTable.FindFieldRange('LONG_LOW',Min,Max) then IndexSeriesTable.SetFieldByNameAsFloat('LONG_LOW',Min);
               if TheTable.FindFieldRange('LONG_HI',Min,Max) then IndexSeriesTable.SetFieldByNameAsFloat('LONG_HI',Max);
               IndexSeriesTable.Next;
            end
            else IndexSeriesTable.Delete;
         end;
         IndexSeriesTable.Destroy;
         for i := 0 to 1 do wmdem.StatusBar1.Panels[i].Text := '';
         SkipMenuUpdating := false;
         ShowDEMReadingProgress := true;
         {$IfDef RecordIndex} WriteLineToDebugFile('Integrated indexing out'); {$EndIf}
      end;

var
   TheTable : Petmar_db.tMyData;
   fname : PathStr;
begin {procedure CreateMapLibrary}
   {$IfDef RecordIndex} WriteLineToDebugFile('CreateMapLibrary in'); {$EndIf}
   try
      WMdem.Color := clInactiveCaption;
      ReportErrors := false;
      if (Memo1 <> Nil) then Memo1.Visible := true;
      PickMapIndexLocation;

      fName := MapLibraryFName;
      if FileExists(MapLibraryFName) then begin
         TheTable := Petmar_db.tMyData.Create(FName);
      end
      else begin
         CreateIntegratedDataBaseTable(MapLibraryFName);
         TheTable := Petmar_db.tMyData.Create(FName);
      end;

      CreateUpdateIntegratedIndex(TheTable);
      if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' Update over');
   finally
      ReportErrors := true;
      TheTable.Destroy;
      WMdem.Color := clScrollBar;
      {$IfDef RecordIndex} WriteLineToDebugFile('CreateMapLibrary out'); {$EndIf}
   end;
end {procedure CreateMapLibrary};


procedure CreateShapeFileGrouping(var fName : PathStr; var TheGroupingIndex : tMyData; Long : boolean; ShapeType : integer = 0);
var
   bName : PathStr;
begin
    if (fName = '') then begin
       bName := 'db-group';
       repeat
          GetString('Group index name',bName,true,ValidDOSFileNameChars);
          fName := DBDir + 'groups\' + bName + DefaultDBExt;
       until (not FileExists(fname)) or AnswerIsYes('Overwrite existing file');
    end;
    if not FileExists(fName) then CreateIndexSymbologyTable(fName,Long,ShapeType);
    TheGroupingIndex := tMyData.Create(fName);
    WriteStringListToDebugFile(TheGroupingIndex.GetTableStructure);
end;


procedure DefineShapeFileGrouping(fName : PathStr);
var
   TheGroupingIndex : tMyData;
   FilesToAdd : tStringList;
   DefaultFilter : byte;
   fNum,i,Layer,db : integer;
   NumFieldColoring : boolean;
   FieldsInDB : tStringList;
begin
    CreateShapeFileGrouping(fName,TheGroupingIndex,true,0);
    FilesToAdd := tStringList.Create;
    FilesToAdd.Add(DBDir);
    DefaultFilter := 0;
    while GetMultipleFiles('shape files',DBNameMask + '|Shape files|*.shp|All files|*.*',FilesToAdd,DefaultFilter) do begin
       NumFieldColoring := AnswerIsYes('Allow coloring from numeric field');
       for i := 0 to pred(FilesToAdd.Count) do begin
          fName := FilesToAdd.Strings[i];
          if OpenNumberedGISDataBase(db,fName) then with GISdb[db] do begin
             TheGroupingIndex.Insert;
             TheGroupingIndex.SetFieldByNameAsString('FILENAME',fName);
             fName := ChangeFileExt(ExtractFileName(fName),'');
             Petmar.GetString('Label caption',fName,false,ReasonableTextChars);
             TheGroupingIndex.SetFieldByNameAsString('NAME',fName);
             if (GISdb[db].ShapeFileType = 0) then GISdb[db].ShapeFileType := 1;
             TheGroupingIndex.SetFieldByNameAsInteger('SHAPE_TYPE',GISdb[db].ShapeFileType);
             repeat
                if (i < 10) then Layer := 9-i else Layer := 0;
                ReadDefault('Layer [9 (top, last drawn) to 0 (bottom, first drawn)]',Layer);
             until (Layer in [0..9]);

             TheGroupingIndex.SetFieldByNameAsInteger('PLOT_ORDER',Layer);
             TheGroupingIndex.SetFieldByNameAsString('PLOT','Y');

             if NumFieldColoring and (GISdb[db].NumericFields > 0) and AnswerIsYes('Color code from numeric field') then begin
                PetdbUtils.GetFields(GISdb[db].MyData,GISdb[db].dbOpts.VisCols,NumericFieldTypes,FieldsInDB);
                fNum := 0;
                Petmar.MultiSelectSingleColumnStringList('field to color',fNum,FieldsInDB);
                TheGroupingIndex.SetFieldByNameAsString('FIELD_NAME',FieldsInDB.Strings[fNum]);
             end
             else begin
               if GISdb[db].ItsAPointDB then begin
                   GISdb[db].dbOpts.Symbol.DrawingSymbol := FilledBox;
                   GISdb[db].dbOpts.Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[i mod 14]);
                   GISdb[db].dbOpts.Symbol.Size := 3;
                   GetSymbol(GISdb[db].dbOpts.Symbol.DrawingSymbol,GISdb[db].dbOpts.Symbol.Size,GISdb[db].dbOpts.Symbol.Color,fName);
               end
               else if LineShapeFile(GISdb[db].ShapeFileType) then begin
                   GISdb[db].dbOpts.LineColor := ConvertTColorToPlatformColor(WinGraphColors[i mod 14]);
                   GISdb[db].dbOpts.LineWidth := 3;
                   PickLineSizeAndColor(fName,Nil,GISdb[db].dbOpts.LineColor,GISdb[db].dbOpts.LineWidth);
               end
               else if AreaShapeFile(GISdb[db].ShapeFileType) then begin
                   GISdb[db].dbOpts.FillColor := ConvertTColorToPlatformColor(WinGraphColors[i mod 14]);
                   GISdb[db].dbOpts.AreaSymbolFill := bsSolid;
                   GISdb[db].dbOpts.LineColor := claBlack;
                   GISdb[db].dbOpts.LineWidth := 1;
                   PickPattern(fName + ' area fill',GISdb[db].dbOpts.AreaSymbolFill,GISdb[db].dbOpts.FillColor,GISdb[db].dbOpts.LineColor,GISdb[db].dbOpts.LineWidth);
               end;
               GISdb[db].WriteDisplaySymbology(TheGroupingIndex);
             end;
             TheGroupingIndex.Post;
          end;
          CloseAndNilNumberedDB(db);
       end;
    end;
    TheGroupingIndex.Destroy;
end;


procedure GetMapLibraryDataLimits(var MinLat,MaxLat,MinLong,MaxLong : float64);
var
    Lat,Long : float64;
    IndexSeriesTable : tMyData;
begin
   OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.ApplyFilter('USE=' + QuotedStr('Y'));
   MaxLat := 70;
   MinLat := -70;
   MinLong := -180;
   MaxLong := 180;
   if IndexSeriesTable.FieldExists('LAT_HI') then begin
      if not IndexSeriesTable.FindFieldRange('LAT_HI',Lat,MaxLat) then MaxLat := 70;
      if not IndexSeriesTable.FindFieldRange('LAT_LOW',MinLat,Lat) then MinLat := -70;
      if not IndexSeriesTable.FindFieldRange('LONG_HI',Long,MaxLong) then MaxLong := 180;
      if not IndexSeriesTable.FindFieldRange('LONG_LOW',MinLong,Long) then MinLong := -180;
   end;
   IndexSeriesTable.Destroy;
   {$If Defined(RecordIndex) or Defined(RecordImageIndex)} WriteLineToDebugFile('GetMapLibraryDataLimits  NW corner: ' + LatLongDegreeToString(MaxLat,MinLong) + '  SE corner: ' + LatLongDegreeToString(MinLat,MaxLong)); {$EndIf}
end;


procedure ShowMapLibraryDataAtPoint(Lat,Long : float64);
const
   Tolerance = 0.0001;
var
   i : integer;
   Findings : tStringList;
   IndexDataOnline : tMyData;
   fName : PathStr;
begin
   fName := MapLibraryFName;
   IndexDataOnline := tMyData.Create(fName);
   IndexDataOnline.ApplyFilter(MakeGeoFilterFromCorners(Lat+Tolerance,Long-Tolerance,Lat-Tolerance,Long+Tolerance));
   Findings := tStringList.Create;
   for i := 1 to IndexDataOnline.RecordCount do begin
      Findings.Add(IndexDataOnline.GetFieldByNameAsString('SERIES') + ':   ' + IndexDataOnline.GetFieldByNameAsString('FILENAME'));
      IndexDataOnline.Next;
   end;
   Petmar.DisplayAndPurgeStringList(Findings,'Data available at point');
   IndexDataOnline.Destroy;
end;




function LoadMapLibraryPoint(Load : boolean; Lat,Long : float64; WantSeries : shortstring = ''; DisplayIt : boolean = true) : integer;
const
   extra = 0.0001;
var
   bb : sfBoundBox;
begin
   bb.YMax:= Lat+extra;
   bb.XMin := Long-extra;
   bb.YMin := Lat-extra;
   bb.XMax := Long+extra;
   Result := LoadMapLibraryBox(Load,bb,WantSeries,DisplayIt);
end;




function GetListOfDataInBoxInSeries(Series : shortString; bb : sfBoundBox) : tStringList;

      function GetMapLibraryDataInBoundingBox(theFilter : string) : tStringList;
      var
         fName : PathStr;
         IndexDataOnline : tMyData;
      begin
         {$If Defined(RecordIndex) or Defined(RecordImageIndex) or Defined(RecordIndexProhlems)} WriteLineToDebugFile('GetDataInBoundingBox in DB filter: ' + theFilter); {$EndIf}
         fName := MapLibraryFName;
         IndexDataOnline := tMyData.Create(fName);
         IndexDataOnline.ApplyFilter(theFilter);
         Result := tStringList.Create;
         Result.Sorted := true;
         if (IndexDataOnline.RecordCount > 0) then begin
            Result.Duplicates := dupIgnore;
            while not IndexDataOnline.EOF do begin
               fName := IndexDataOnline.GetFieldByNameAsString('FILENAME');
               fName[1] := SeriesIndexFileName[1];
               if not StrUtils.AnsiContainsText(UpperCase(fName),'ORIGINAL_') then begin
                  Result.Add(fName);
                  {$If Defined(RecordIndex) or Defined(RecordImageIndex)} WriteLineToDebugFile('GetDataInBoundingBox found: ' + ExtractFileName(fName)); {$EndIf}
               end;
               IndexDataOnline.Next;
            end;
            {$If Defined(RecordMultipleFilesInBoundingBox)}
               if (Result.Count > 1) then begin
                  WriteLineToDebugFile(''); WriteLineToDebugFile('Multiple files in box with filter=' + theFilter); WriteStringListToDebugFile(Result); WriteLineToDebugFile('');
               end;
            {$EndIf}
         end;
         IndexDataOnline.Destroy;
      end;

begin
   Result := GetMapLibraryDataInBoundingBox('(' + MakeGeoFilterFromBoundingBox(bb) + ') AND SERIES = ' + QuotedStr(Series));
end;


procedure MergeDEMs(Mode : integer);
var
   UseGDALvrt : boolean;
   DEMList,NewFiles : tStringList;
   i, NewDEM : integer;
   aPath : PathStr;
begin
   {$If Defined(RecordMenu) or Defined(RecordMerge)} WriteLineToDebugFile('Enter MergeDEMs, mode=' + IntToStr(Mode)); {$EndIf}
   UseGDALvrt := Mode in [dmMergeGDAL,dmMergeDirectories];
   DEMList := tStringList.Create;
   MergeSeriesName := '';
   if Mode in [dmMergeGDAL, dmMergeMDnative] then begin
      DEMList.Add(LastDEMName);
      if Petmar.GetMultipleFiles('DEMs to merge',DEMFilterMasks,DEMList,MDDef.DefaultDEMFilter) then begin
         {$IfDef RecordMenu} WriteStringListToDebugFile(DEMList); {$EndIf}
         if (DEMList.Count = 1) and (UpperCase(ExtractFileExt(DEMList.Strings[0])) <> '.ASC') then begin
            NewDEM := OpenNewDEM(DEMList.Strings[0]);
            DEMList.Destroy;
         end
         else begin
            NewDEM := MergeMultipleDEMsHere(DEMList,true,UseGDALvrt);
         end;
      end;
   end
   else begin
      aPath := ExtractFilePath(LastDEMname);
      repeat
         GetDOSPath('Directory with DEMs to merge',aPath);
         if (aPath <> '') then begin
            NewFiles := Nil;
            FindMatchingFiles(aPath,'*.*',NewFiles,1);
            for i := 0 to pred(NewFiles.Count) do begin
               DEMList.Add(NewFiles.Strings[i]);
            end;
            {$IfDef RecordMenu} WriteLineToDebugFile('   add ' + IntToStr(NewFiles.Count) + ' from ' + aPath); {$EndIf}
            NewFiles.Destroy;
         end;
      until (aPath = '');
      if (DEMList.Count > 0) then begin
         NewDEM := MergeMultipleDEMsHere(DEMList,true,UseGDALvrt);
      end;
   end;
   StopSplashing;
   WMDEM.SetMenusForVersion;
   {$IfDef TrackDEMCorners} DEMGlb[NewDEM].WriteDEMCornersToDebugFile('Merge DEMs, mode=' + IntToStr(Mode)); {$EndIf}
   {$If Defined(RecordMenu) or Defined(RecordMerge)} WriteLineToDebugFile('Exit MergeDEMs, mode=' + IntToStr(Mode)); {$EndIf}
end;



function MergeMultipleDEMsHere(var DEMList : TStringList; DisplayIt,GDALversion : boolean; MergefDir : PathStr = '') : integer;
var
   FName : ShortString;
   zf : float32;
   XSpace,YSpace,tf,xgrid,ygrid,xutm,yutm,
   xmin,xmax,ymin,ymax,Lat,Long  : float64;
   NewHeader   : tDEMheader;
   Inbounds,UTMDEMs : boolean;
   MergeUTMzone,TileX,TileY : int32;
   CurDEM     : integer;
   aName,ProjName,OldDEMName,OutVRT  : PathStr;
   MenuStr : ShortString;
   SaveIt : boolean;

      procedure OldMICRODEMmerge;
      //still an option because MICRODEM had problems with some of the very large Geotiffs created by GDAL
      var
        i,Row,Col : integer;
      begin
         {$If Defined(RecordMerge) or Defined(RecordMergeDetails) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('MD merge for DEM'); {$EndIf}
         if (DEMList.Count > MaxDEMsToMerge) then MessageToContinue('Trying to merge too many DEMs--' + IntToStr(DEMList.Count) + '  and limit=' + IntToStr(MaxDEMsToMerge));
         OldDEMName := LastDEMName;
         SubsequentDEM := false;
         ReallyReadDEM := false;
         DEMlist.Sorted := false;
         for i := pred(DEMList.Count) downto 0 do begin
            FName := DEMList.Strings[i];
            {$If Defined(RecordMergeDetails)} HighLightLineToDebugFile('Check DEM ' + IntToStr(i) + '/' + IntToStr(DEMList.Count) + '  ' + fName); {$EndIf}
            WMDEM.StatusBar1.Panels[0].Text := 'Merge still Check ' + IntToStr(succ(I)) + '/' + IntToStr(DEMList.Count);
            if FileExists(fName) then begin
               if NewArea(true,CurDEM,'',FName) then begin
                  {$If Defined(TrackPixelIs)} WriteLineToDebugFile('merge ' + DEMGlb[CurDEM].AreaName + '  ' + DEMGlb[CurDEM].GridCornerModelAndPixelIsString); {$EndIf}

                  if (not SubsequentDEM) then begin
                     NewHeader := DEMGlb[CurDEM].DEMheader;
                     xmin := DEMGlb[CurDEM].DEMheader.DEMSWCornerX;
                     ymin := DEMGlb[CurDEM].DEMheader.DEMSWCornerY;
                     xMax := xMin + pred(DEMGlb[CurDEM].DEMheader.NumCol) * DEMGlb[CurDEM].DEMheader.DEMxSpacing;
                     yMax := yMin + pred(DEMGlb[CurDEM].DEMheader.NumRow) * DEMGlb[CurDEM].DEMheader.DEMySpacing;
                     XSpace := DEMGlb[CurDEM].DEMheader.DEMxSpacing;
                     YSpace := DEMGlb[CurDEM].DEMheader.DEMySpacing;
                     MergeUTMzone := DEMGlb[CurDEM].DEMHeader.UTMZone;
                     UTMDEMs := DEMGlb[CurDEM].DEMHeader.DEMUsed = UTMBasedDEM;
                     SubsequentDEM := true;
                     DEMlist.Strings[i] := DEMGlb[CurDEM].DEMfileName;
                     {$If Defined(RecordMergeDetails)} WriteLineToDebugFile('First DEM, Spacing: ' + RealToString(Xspace,-12,-6) + 'x' + RealToString(Xspace,-12,-6) ); {$EndIf}
                  end
                  else begin
                     if UTMDEMs and (DEMGlb[CurDEM].DEMHeader.UTMZone <> MergeUTMZone) then begin
                        {$IfDef RecordMerge} WriteLineToDebugFile('Wrong UTM zone, exclude ' + DEMGlb[CurDEM].AreaName); {$EndIf}
                        DEMlist.Delete(i);
                     end
                     else begin
                        if (DEMGlb[CurDEM].DEMheader.DEMSWCornerX < xmin) then xmin := DEMGlb[CurDEM].DEMheader.DEMSWCornerX;
                        tf := DEMGlb[CurDEM].DEMheader.DEMSWCornerX + pred(DEMGlb[CurDEM].DEMheader.NumCol) * DEMGlb[CurDEM].DEMheader.DEMxSpacing;
                        if (xMax < tf) then xmax := tf;
                        if (DEMGlb[CurDEM].DEMheader.DEMSWCornerY < ymin) then ymin := DEMGlb[CurDEM].DEMheader.DEMSWCornerY;
                        tf := DEMGlb[CurDEM].DEMheader.DEMSWCornerY + pred(DEMGlb[CurDEM].DEMheader.NumRow) * DEMGlb[CurDEM].DEMheader.DEMySpacing;
                        if (yMax < tf) then ymax := tf;
                        NewHeader.DEMSWCornerX := xmin;
                        NewHeader.DEMSWCornerY := ymin;
                        NewHeader.NumCol := succ(round((xmax - xmin) / XSpace));
                        NewHeader.NumRow := succ(round((ymax - ymin) / YSpace));
                        DEMlist.Strings[i] := DEMGlb[CurDEM].DEMfileName;
                        {$If Defined(RecordMergeDetails)} WriteLineToDebugFile('Next DEM, grid size= ' + IntToStr(NewHeader.NumCol) + 'x' + IntToStr(NewHeader.NumRow) ); {$EndIf}
                     end;
                  end {if};
                  {$IfDef RecordMergeDetails}
                     WriteLineToDebugFile('------------------------------------------------------------------------------------------------');
                     WriteLineToDebugFile(DEMGlb[CurDEM].AreaName + ' merge cols=' + IntToStr(NewHeader.NumCol) + '  rows=' + IntToStr(NewHeader.NumRow));
                     WriteLineToDebugFile('Merge UTM zone ' + IntToStr(DEMGlb[CurDEM].DEMHeader.UTMZone) + '  ' + DEMGlb[CurDEM].AreaName + ' x range: ' +
                         RealToString(xmin,-18,-6) + '--' + RealToString(xmax,-18,-6) + ' y range: ' + RealToString(ymin,-18,-6) + '--' + RealToString(ymax,-18,-6));
                  {$EndIf}
                  CloseSingleDEM(CurDEM);
               end
               else begin
                  {$IfDef RecordProblemsMerge} HighlightLineToDebugFile('DEM merge missing file  ' + fName); {$EndIf}
                  DEMlist.Delete(i);
               end;
            end;
         end;

         {$If Defined(RecordMerge) or Defined(RecordMergeDetails) or Defined(RecordTimeMerge) } WriteLineToDebugFile('done first pass in MD DEM Merge'); {$EndIf}
         NewHeader.ElevUnits := euMeters;
         if OpenAndZeroNewDEM(false,NewHeader,Result,'Merge',InitDEMmissing) then begin
            {$IfDef RecordMergeDetails} WriteLineToDebugFile('New DEM ' + IntToStr(Result) + ' opened'); {$EndIf}
            if (MergeSeriesName = '') then DEMGlb[Result].AreaName := 'merge_from_' + LastSubDir(ExtractFilePath(DEMList.Strings[0]))
            else DEMGlb[Result].AreaName := MergeSeriesName;
            {$If Defined(TrackPixelIs)} WriteLineToDebugFile('merge is ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].GridCornerModelAndPixelIsString); {$EndIf}
            ReallyReadDEM := true;
            for i := 0 to pred(DEMList.Count) do begin
               FName := DEMList.Strings[i];
               if FileExists(fName) then begin
                  if NewArea(true,CurDEM,'',FName) then begin
                     if UTMDEMs then begin
                        DEMGlb[CurDEM].DEMGridToUTM(0,0,xutm,yutm);
                        DEMGlb[Result].UTMToDEMGrid(XUTM,YUTM,XGrid,YGrid,InBounds);
                        TileX := round(Xgrid);
                        TileY := round(Ygrid);
                     end;
                     ShowHourglassCursor;
                     MenuStr := 'Merge ' + IntToStr(succ(I)) + '/' + IntToStr(DEMList.Count);
                     StartProgress(MenuStr);
                     {$IfDef RecordMergeDetails} WriteLineToDebugFile(MenuStr); {$EndIf}
                     WMDEM.StatusBar1.Panels[0].Text := MenuStr;
                     {$IfDef RecordMergeDetails} WriteLineToDebugFile('Merging DEM=' + IntToStr(CurDEM) + '  ' + DEMGlb[CurDEM].AreaName); {$EndIf}
                     for Row := 0 to pred(DEMGlb[CurDEM].DEMheader.NumRow) do begin
                        for Col := 0 to pred(DEMGlb[CurDEM].DEMheader.NumCol) do begin
                           if DEMGlb[CurDEM].GetElevMeters(Col,Row,zf) then begin
                              if UTMDEMs then begin
                                DEMGlb[Result].SetGridElevation(Col + TileX,Row + TileY,zf);
                              end
                              else begin
                                 DEMGlb[CurDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                                 DEMGlb[Result].SetGridElevationLatLongDegree(Lat,Long,zf);
                              end;
                           end;
                        end {for Row};
                     end;
                     EndProgress;
                     CloseSingleDEM(CurDEM);
                  end;
               end;
            end {for i};
            LastDEMName := OldDEMName;
            DEMList.Free;
            if SaveIt then DEMGlb[Result].WriteNewFormatDEM(MergefDir,'merged DEM');
            {$If Defined(RecordMerge) or Defined(RecordMergeDetails) or Defined(RecordTimeMerge)  or Defined(RecordPixelIs)}
               WriteLineToDebugFile('MD merge done, DEM=' + IntToStr(Result) + '  ' + DEMGlb[Result].GridCornerModelAndPixelIsString);
            {$EndIf}
          end;
     end;


begin
   {$If Defined(RecordMerge) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('arrive Merge for DEM'); {$EndIf}
   SkipMenuUpdating := true;
   DEMMergeInProgress := true;
   try
      aName := DEMList.Strings[0];
      SaveIt := (MergefDir <> '');
      if (MergefDir = '') then MergefDir := MDTempDir;
      MergefDir := Petmar.NextFileNumber(MergefDir,LastSubDir(ExtractFilePath(DEMList.Strings[0])) + '_','.tif');

      if GDALversion and GDALGridFormat(ExtractFileExt(aName),false) then begin
         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('GDAL options for DEM'); {$EndIf}
         ProjName := '';
         if FileExtEquals(aName,'.ASC') then begin //Spanish and Trento and French DEMs have no projection in the ASC files, and user must put WKT file in directory  (Spain now Geotiff)
            {$If Defined(RecordMerge) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('ASC reprojection'); {$EndIf}
            ProjName := FindSingleWKTinDirectory(ExtractFilePath(aName));
            if (ProjName <> '') then ProjName := '-a_srs ' + ProjName;
         end;

         UseGDAL_VRT_to_merge(MergefDir,OutVRT,DEMList,ProjName);
         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('GDAL VRT over for DEM, open ' + MergeFDir); {$EndIf}
         if FileExists(MergefDir) then begin
            Result := OpenNewDEM(MergefDir,false);
         end
         else begin
            Result := 0;
         end;
         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('DEM=' + IntToStr(Result) + '  open ' + MergeFDir); {$EndIf}
      end
      else begin
         OldMICRODEMmerge;
      end;

    if ValidDEM(Result) then begin
         {$IfDef TrackPixelIs} WriteLineToDebugFile('MergeMultipleDEMsHere defined, ' + DEMGlb[Result].GridCornerModelAndPixelIsString); {$EndIf}
         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) } WriteLineToDebugFile('Merge set up, Result=' + IntToStr(Result)); {$EndIf}
         if MDdef.AutoFillHoles then begin
            DEMGlb[Result].InterpolateAcrossHoles(false);
            {$If Defined(RecordMerge) or Defined(RecordTimeMerge)} WriteLineToDebugFile('Holes done'); {$EndIf}
         end;
         if MDdef.MissingToSeaLevel then begin
            DEMGlb[Result].MissingDataToConstantVelue;
            {$If Defined(RecordMerge) or Defined(RecordTimeMerge) } WriteLineToDebugFile('Sea level done'); {$EndIf}
         end;
         SkipMenuUpdating := false;
         DEMGlb[Result].CheckMaxMinElev;
         DEMGlb[Result].DefineDEMVariables(True);
         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) } WriteLineToDebugFile('Elev check Merge: ' + DEMGlb[Result].ZRange); {$EndIf}
         if DisplayIt then CreateDEMSelectionMap(Result,true,MDDef.DefElevsPercentile,MDdef.DefElevMap);
      end
      else begin
         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) } WriteLineToDebugFile('Failed set up, Result=' + IntToStr(Result)); {$EndIf}
      end;
   finally
      WMDEM.StatusBar1.Panels[0].Text := '';
      UpdateMenusForAllMaps;
      DEMMergeInProgress := false;
      SubsequentDEM := false;
      {$If Defined(RecordMerge) or Defined(MergeSummary) or Defined(RecordTimeMerge) or Defined(RecordPixelIs)}
          if Result <> 0 then WriteLineToDebugFile('MergeMultipleDEMsHere out, merged  ' + DEMGlb[Result].AreaName)
          else WriteLineToDebugFile('MergeMultipleDEMsHere out, failure');
      {$EndIf}
   end;
end;


function LoadMapLibraryBox(Load : boolean; bb : sfBoundBox; WantSeries : shortstring = ''; DisplayIt : boolean = true) : integer;
var
   MergedName : ShortString;

         procedure LoadTheDEMs(var LoadList : tStringList);
         var
            fName : PathStr;
            i : integer;
         begin
             {$If Defined(RecordIndex) or Defined (RecordDuplicatesInIndex)} WriteLineToDebugFile('Enter LoadTheDEMs, count='+IntToStr(LoadList.Count)); {$EndIf}
             SortAndRemoveDuplicates(LoadList);
             {$If Defined(RecordIndex) or Defined (RecordDuplicatesInIndex)} WriteLineToDebugFile('Sorted LoadTheDEMs, count='+IntToStr(LoadList.Count)); {$EndIf}
             if not FileExists(LoadList.Strings[0]) then begin
                LoadList.Sorted := false;
                for i := 0 to pred(LoadList.Count) do begin
                   fName := LoadList.Strings[i];
                   fName[1] := SeriesIndexFileName[1];
                   LoadList.Strings[i] := fName;
                end;
             end;

             for i := pred(LoadList.Count) downto 0 do begin
                fName := LoadList.Strings[i];
                if not FileExists(fName) then begin
                   MessageToContinue('File missing: ' + fName);
                   LoadList.Delete(i);
                end;
             end;
             if (LoadList.Count = 0) then begin
                LoadList.Destroy;
                exit;
             end;

             if (LoadList.Count = 1) then begin
                if (LoadList.Count) = 1 then begin
                   fName := LoadList.Strings[0];
                   {$IfDef RecordIndex} WriteLineToDebugFile('load single DEM=' + ExtractFileName(fName)); {$EndIf}
                   LoadNewDEM(Result,fName,DisplayIt);
                end;
                LoadList.Destroy;
             end
             else begin
                {$IfDef RecordIndex} WriteLineToDebugFile('call MergeDEMs, count=' + IntToStr(LoadList.Count)); {$EndIf}
                Result := MergeMultipleDEMsHere(LoadList,DisplayIt,false);  //May 2023, set to use old MICRODEM merge
                DEMGlb[Result].DEMFileName := NextFileNumber(MDTempDir, MergeSeriesName + '_','.dem');
                DEMGlb[Result].WriteNewFormatDEM(DEMGlb[Result].DEMFileName);
             end;
             //Loadone := true;
             if ValidDEM(Result) then DEMGlb[Result].AreaName := MergedName;
             {$If Defined(TrackPixelIs)} WriteLineToDebugFile('Loaded ' + DEMGlb[WantDEM].AreaName + '  ' + DEMGlb[WantDEM].GridCornerModelAndPixelIsString); {$EndIf}
             {$If Defined(RecordIndex) or Defined(RecordLoadMapLibraryBox)} WriteLineToDebugFile('Exit LoadTheDEMs, WantDEM=' + IntToStr(WantDEM)); {$EndIf}
         end;

var
   DataInSeries : tStringList;
   IndexSeriesTable : tMyData;
   DataType : ShortString;
begin
   {$If Defined(RecordIndex) or Defined(RecordImageIndex) or Defined(LoadLibrary)} WriteLineToDebugFile('Enter LoadMapLibraryBox, display=' + TrueOrFalse(DisplayIt) + '  Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   try
      LoadingFromMapLibrary := true;
      ShowHourglassCursor;
      //LoadOne := false;
      Result := 0;
      MDDef.MissingToSeaLevel := false;
      OpenIndexedSeriesTable(IndexSeriesTable);
      if (WantSeries = '') then IndexSeriesTable.ApplyFilter('(USE = ' + QuotedStr('Y') + ')')
      else IndexSeriesTable.ApplyFilter('(SERIES = ' + QuotedStr(WantSeries) + ')');

      while not IndexSeriesTable.Eof do begin
         MergeSeriesName := IndexSeriesTable.GetFieldByNameAsString('SERIES');
         MergedName := '';
         if IndexSeriesTable.FieldExists('SHORT_NAME') then MergedName := IndexSeriesTable.GetFieldByNameAsString('SHORT_NAME');
         if (MergedName = '') then MergedName := MergeSeriesName;
         {$If Defined(RecordIndex) or Defined(LoadLibrary)} WriteLineToDebugFile('Merge Series: ' + MergeSeriesName); {$EndIf}
         DataInSeries := GetListOfDataInBoxInSeries(MergeSeriesName,bb);
         {$If Defined(RecordIndex) or Defined(LoadLibrary)} WriteLineToDebugFile('Files found: ' + IntToStr(DataInSeries.Count)); {$EndIf}
         if (DataInSeries.Count > 0) then begin
            DataType := UpperCase(IndexSeriesTable.GetFieldByNameAsString('DATA_TYPE'));
            LoadTheDEMs(DataInSeries);
            {$If Defined(RecordMerge) or Defined(LoadLibrary)} WriteLineToDebugFile('LoadTheDEMs completed'); {$EndIf}
         end;
         IndexSeriesTable.Next;
      end;
      IndexSeriesTable.Destroy;
      ShowDefaultCursor;
   finally
      LoadingFromMapLibrary := false;
   end;
   {$If Defined(RecordIndex) or Defined(RecordImageIndex) or Defined(LoadLibrary)} WriteLineToDebugFile('Out LoadMapLibraryBox; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
end;

(*

procedure CopyMapLibraryBox(bb : sfBoundBox);
var
   DataInSeries : tStringList;
   IndexSeriesTable : tMyData;
   i : integer;
   OutPath,ThisOutPath : PathStr;
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('Enter CopyMapLibraryBox ' + sfBoundBoxToString(bb)); {$EndIf}
   ShowHourglassCursor;
   OutPath := MainMapData;
   GetDOSPath('copy map library data',OutPath);
   OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.ApplyFilter('(USE = ' + QuotedStr('Y') + ')');

   while not IndexSeriesTable.Eof do begin
      DataInSeries := GetListOfDataInBoxInSeries(IndexSeriesTable.GetFieldByNameAsString('SERIES'),bb);
      {$IfDef RecordIndex} WriteLineToDebugFile('Merge Series: ' + MergeSeriesName + '  DEMs=' + IntToStr(DataInSeries.Count)); {$EndIf}
      if (DataInSeries <> Nil) and (DataInSeries.Count > 0) then begin
         ThisOutPath := OutPath + IndexSeriesTable.GetFieldByNameAsString('SERIES') + '\';
         SafeMakeDir(ThisOutPath);
         for i := 0 to pred(DataInSeries.Count) do begin
            CopyFile(DataInSeries.Strings[i],ThisOutPath + ExtractFileName(DataInSeries.Strings[i]));
         end;
      end;
      DataInSeries.Destroy;
      IndexSeriesTable.Next;
   end;
   IndexSeriesTable.Destroy;
   ShowDefaultCursor;
   {$IfDef RecordIndex} WriteLineToDebugFile('Out CopyMapLibraryBox'); {$EndIf}
end;
*)

procedure CreateLandsatIndex(Browse : boolean);
var
   Path,fName : PathStr;
   TheFiles,Results : tStringList;
   LandsatMetadata : tLandsatMetadata;
   i,db : integer;
begin
   Path := MainMapData;
   TheFiles := Nil;
   if Browse then begin
      GetDosPath('Landsat browse images',Path);
      Petmar.FindMatchingFiles(Path,'*.jpg',TheFiles,3);
   end
   else begin
      GetDosPath('Landsat full scenes',Path);
      Petmar.FindMatchingFiles(Path,'*_MTL.txt',TheFiles,3);
   end;

   {$IfDef RecordGeostats} WriteLineToDebugFile('Twmdem.Landsatbrowseindex1Click for ' + Path); {$EndIf}

   TheFiles.Sort;
   Results := tStringlist.Create;
   Results.Add('TM,SENSOR,PATH_ROW,PATH,ROW,YEAR,JULIAN_DAY,DATE,TIME,CLOUD_COVR,SUN_AZMTH,SUN_ELEV,IMAGE_QUAL,IMAGE');
   for i := 0 to pred(TheFiles.Count) do begin
       fName := TheFiles.Strings[i];
       GetLandsatMetadata(fName,LandsatMetadata);
       Results.Add( IntToStr(LandsatMetadata.TM_No) + ',' +
                    LandsatMetadata.Sensor + ',' +
                    IntToStr(LandsatMetadata.Path) + '/' + IntToStr(LandsatMetadata.Row) + ',' +
                    IntToStr(LandsatMetadata.Path) + ',' +
                    IntToStr(LandsatMetadata.Row) + ',' +
                    IntToStr(LandsatMetadata.Year) + ',' +
                    IntToStr(LandsatMetadata.JDay) + ',' +
                    LandsatMetadata.Date + ',' +
                    LandsatMetadata.SceneTime + ',' +
                    FloatToStr(LandsatMetadata.CloudCover) + ',' +
                    RealToString(LandsatMetadata.SunAzimuth,-12,2) + ',' +
                    RealToString(LandsatMetadata.SunElevation,-12,2) + ',' +
                    IntToStr(LandsatMetadata.ImageQuality) + ',' +
                    fName);
   end;
   {$IfDef RecordGeostats} WriteStringListToDebugFile(Results); {$EndIf}
   db := StringList2CSVtoDB(Results,Path + 'inventory.csv');
   GISdb[db].SplitDateField(dfMDYSlash);
   GISdb[db].TimeFieldsToDecYears;
   TheFiles.Free;
end;



initialization
   {$IfDef MessageStartUpUnit} MessageToContinue('Startup dem_indexes'); {$EndIf}
   //IndexDataOnline := Nil;
   MergeSeriesName := '';
finalization
   //CloseIndexDataOnline;
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosing active in dem_indexes'); {$EndIf}
   {$IfDef RecordIndex} WriteLineToDebugFile('RecordIndex active in dem_indexes'); {$EndIf}
   {$IfDef RecordAutoZoom} WriteLineToDebugFile('RecordAutoZoom active in dem_indexes'); {$EndIf}
   {$IfDef RecordMerge} WriteLineToDebugFile('RecordMerge active in dem_indexes'); {$EndIf}
   {$IfDef RecordIndexFileNames} WriteLineToDebugFile('RecordIndexFileNames active in dem_indexes'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing dem_indexes'); {$EndIf}
end.





