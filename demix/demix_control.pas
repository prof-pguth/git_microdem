unit demix_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   {$Define RecordDEMIXLoad}
   //{$Define RecordDEMIXVDatum}
   //{$Define RecordFullDEMIX}
{$EndIf}


interface

uses
    System.SysUtils,System.Classes,StrUtils,VCL.ExtCtrls,
    Petmar,Petmar_types;


function LoadDEMIXReferenceDEMs(var RefDEM : integer) : boolean;
function LoadDEMIXCandidateDEMs(RefDEM : integer; OpenMaps : boolean = false) : boolean;
function LoadDEMIXarea(cfName : pathStr) : boolean;
procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
procedure OpenDEMIXArea(fName : PathStr = '');

function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;

const
   MaxTestDEM = 10;
var
   TestDEM : array[1..MaxTestDEM] of integer;
   TestSeries : array[1..MaxTestDEM] of shortstring;
   DEMIXRefDEM,AddLocalVDatum,SubLocalVDatum,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea,GeoidGrid,  COPRefDTM, COPRefDSM : integer;
   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,BaseGraf,PetImage_form,
   DEMCoord,DEMdefs,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,Petmar_db,PetMath;

function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
begin
   DEMName := UpperCase(DEMName);
   if DEMName = 'TIE' then Result := claBrown;
   if DEMName = 'ALOS' then Result := RGBtrip(230,159,0)
   else if DEMName = 'ASTER' then Result := RGBtrip(0,114,178)
   else if DEMName = 'COP' then Result := RGBtrip(86,180,233)
   else if DEMName = 'FABDEM' then Result := RGBtrip(204,121,167)
   else if DEMName = 'NASA' then Result := RGBtrip(0,158,115)
   else if DEMName = 'SRTM' then Result := RGBtrip(213,94,0);
end;

function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
var
   DEM : integer;
begin
   Result.Size := 4;
   Result.Color := DEMIXColorFromDEMName(DEMName);
   Result.DrawingSymbol := FilledBox;
end;


procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
begin
   if StrUtils.AnsiContainsText(TestSeries,'ALOS') or StrUtils.AnsiContainsText(TestSeries,'AW3D') then begin
      UseDTM := RefDTMArea;
      UseDSM := RefDSMArea;
   end
   else if StrUtils.AnsiContainsText(TestSeries,'COP') then begin
      if (COPRefDTM <> 0) then UseDTM := COPRefDTM else UseDTM := RefDTMpoint;
      if (COPRefDSM <> 0) then UseDSM := COPRefDSM else UseDSM := RefDSMpoint;
   end
   else begin
      UseDTM := RefDTMPoint;
      UseDSM := RefDSMPoint;
   end;
end;



procedure DoDifferenceMaps(AreaName,ShortName,LongName : shortString; var Graph1,Graph2 : tThisBaseGraph);
var
   i : integer;
   fName : PathStr;
   Min,Max,BinSize : float32;
   TestGrid,DSMgrid,DTMGrid,DiffGrid,
   UseDSM,UseDTM : integer;
   DSMElevFiles,DSMLegendFiles,DTMElevFiles,DTMLegendFiles : tStringList;

      procedure ModifyGraph(Graph : tThisBaseGraph);
      var
         I : integer;
      begin
         for I := 1 to Graph.GraphDraw.LegendList.Count do begin
            Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
         end;
         Graph.RedrawDiagram11Click(Nil);
         Graph.Image1.Canvas.Draw(Graph.GraphDraw.LeftMargin+15,Graph.GraphDraw.TopMargin+10,Graph.MakeLegend(Graph.GraphDraw.LegendList,false));
      end;



     procedure MakeDifferenceGrid(RefGrid : integer; RefType : shortstring; LegendFiles,ElevFiles : tStringList);

            function SaveValuesFromGrid(DEM : integer; What : shortstring) : ShortString;
            var
               Col,Row,Npts :integer;
               zs : ^bfarray32;
               z : float32;
            begin
               New(ZS);
               NPts := 0;
               for Col := 0 to pred(DEMGlb[DEM].DEMHeader.NumCol) do begin
                  for Row := 0 to pred(DEMGlb[DEM].DEMHeader.NumRow) do begin
                     if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                       zs^[Npts] := z;
                       inc(NPts);
                     end;
                  end;
               end;

               if (NPts > 0) then begin
                  Result := DEMIXtempfiles + DEMGlb[DEM].AreaName + '_' + AreaName + '.z';
                  SaveSingleValueSeries(npts,zs^,Result);
               end;
               Dispose(zs);
            end;

     var
        DiffGrid : integer;
        fName : PathStr;
     begin
         DiffGrid := MakeDifferenceMap(RefGrid,TestGrid,true,false,false);
         DEMglb[DiffGrid].AreaName := AreaName + '_' + TestSeries[i] + '_' + ShortName + '_' + RefType;
         fName := DEMIXtempfiles + DEMglb[DiffGrid].AreaName + '.dem';
         DEMglb[DiffGrid].WriteNewFormatDEM(fName);
         ElevFiles.Add(SaveValuesFromGrid(DiffGrid,ShortName + '_' + RefType + '_'));
         LegendFiles.Add(TestSeries[i]);
         CloseSingleDEM(DiffGrid);
         if (ShortName <> 'elvd') then begin
            fName := AreaName + '_percent_diff_' + TestSeries[i] + '_' + ShortName + '_' + RefType;
            DiffGrid := PercentDifferentTwoGrids(RefGrid,TestGrid,fName);
            fName := DEMIXtempfiles + fName + '.dem';
            DEMglb[DiffGrid].WriteNewFormatDEM(fName);
            CloseSingleDEM(RefGrid);
         end;
     end;



begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('start differences ' + LongName); {$EndIf}
   MDDef.DefaultGraphXSize := 1000;
   MDDef.DefaultGraphYSize := 600;
   DTMElevFiles := tStringList.Create;
   DTMLegendFiles := tStringList.Create;
   DSMElevFiles := tStringList.Create;
   DSMLegendFiles := tStringList.Create;

   for I := 1 to MaxTestDEM do begin
      if ValidDEM(TestDEM[i]) then begin
         GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
         if ShortName = 'elvd' then begin
            TestGrid := TestDEM[i];
            DTMGrid := UseDTM;
         end;

         if ShortName = 'slpd' then begin
            TestGrid := CreateSlopeMap(TestDEM[i]);
            DTMGrid := CreateSlopeMap(UseDTM);
         end;
         if ShortName = 'rufd' then begin
            TestGrid := CreateRoughnessSlopeStandardDeviationMap(TestDEM[i],3);
            DTMGrid := CreateRoughnessSlopeStandardDeviationMap(UseDTM,3);
         end;

         {$IfDef RecordDEMIX} writeLineToDebugFile(Testseries[i] + ' DTMs ' + DEMGlb[DTMgrid].AreaName + '  ' + DEMGlb[Testgrid].AreaName + ' ' + IntToStr(DTMGrid) + '/' + IntToStr(TestGrid)); {$EndIf}

         MakeDifferenceGrid(DTMGrid,'dtm',DTMLegendFiles,DTMElevFiles);

         if (UseDSM <> 0) then begin
            if ShortName = 'elvd' then begin
               DSMGrid := UseDSM;
            end;
            if ShortName = 'slpd' then begin
               DSMGrid := CreateSlopeMap(UseDSM);
            end;
            if ShortName = 'rufd' then begin
               DSMGrid := CreateRoughnessSlopeStandardDeviationMap(UseDSM,3);
            end;
            {$IfDef RecordDEMIX} writeLineToDebugFile(Testseries[i] + ' DSMs ' + DEMGlb[DSMgrid].AreaName + '  ' + DEMGlb[Testgrid].AreaName + ' ' + IntToStr(DSMGrid) + '/' + IntToStr(TestGrid)); {$EndIf}
            MakeDifferenceGrid(DSMGrid,'dsm',DSMLegendFiles,DSMElevFiles);
         end;
         if (ShortName <> 'elvd') then CloseSingleDEM(Testgrid);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('After ' + TestSeries[i] + ', Open grids now=' + IntToStr(NumDEMDataSetsOpen) ); {$EndIf}
      end;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('start graphs'); {$EndIf}
      if ShortName = 'elvd' then begin
         Min := -50;
         Max := 50;
         BinSize := 0.25;
      end;

      if ShortName = 'slpd' then begin
         Min := -50;
         Max := 50;
         BinSize := 0.25;
      end;
      if ShortName = 'rufd' then begin
         Min := -20;
         Max := 20;
         BinSize := 0.15;
      end;

   Graph1 := CreateMultipleHistograms(MDDef.CountHistograms,DTMElevFiles,DTMLegendFiles,AreaName + ' DTM ' + LongName + ' difference','DTM ' + LongName + ' difference distribution',100,Min,Max,BinSize);
   ModifyGraph(Graph1);
   if (DSMElevFiles.Count > 0) then begin
      Graph2 := CreateMultipleHistograms(MDDef.CountHistograms,DSMElevFiles,DSMLegendFiles,AreaName + ' DSM ' + LongName + ' difference','DSM ' + LongName + ' difference distribution',100,Min,Max,BinSize);
      ModifyGraph(Graph2);
   end
   else begin
      DSMElevFiles.Free;
      DSMLegendFiles.Free;
      Graph2 := Nil;
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('done differences'); {$EndIf}
end;



procedure OpenDEMIXArea(fName : PathStr = '');
const
   DoCHM = false;
   DoAirOrDirt = false;
   DoElevationDifference = true;
   DoSlopeDifference = true;
   DoRoughnessDifference = true;
var
   AreaName : shortstring;
   UseDSM,UseDTM,
   i,AirOrDirt,Cols : integer;
   BigMap : tStringList;
   Graph1,Graph2 : tThisBaseGraph;


         procedure AddImage(GraphImage : tImage);
         var
           Bitmap : tMyBitmap;
         begin
            CopyImageToBitmap(GraphImage,Bitmap);
            Bitmap.Height := Bitmap.Height + 25;
            fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
            Bitmap.SaveToFile(fName);
            BigMap.Add(fName);
         end;


begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea ' + fName); {$EndIf}
      HeavyDutyProcessing := true;
      MDDef.GridLegendLocation.DrawItem := true;
      MDDef.MapNameLocation.DrawItem := true;
      AreaName := ExtractFileNameNoExt(fName);
      if LoadDEMIXarea(fName) then begin
         if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
            if DoCHM then begin
               if ValidDEM(RefDTMpoint) and ValidDEM(RefDSMpoint) then begin
                  MakeDifferenceMapOfBoxRegion(RefDTMPoint,RefDSMpoint,DEMGlb[RefDSMPoint].FullDEMGridLimits,true,false,false,AreaName + '_points_chm');
               end;
               if ValidDEM(RefDTMarea) and ValidDEM(RefDSMarea) then begin
                  MakeDifferenceMapOfBoxRegion(RefDTMarea,RefDSMarea,DEMGlb[RefDSMarea].FullDEMGridLimits,true,false,false,AreaName + '_areas_chm');
               end;
            end;
            if LoadDEMIXCandidateDEMs(DEMIXRefDEM,true) then begin
               if DoAirOrDirt then begin
                  BigMap := tStringList.Create;
                  BigMap.Add(DEMGlb[RefDTMPoint].SelectionMap.MapDraw.FullMapfName);
                  for I := 1 to MaxTestDEM do begin
                     if ValidDEM(TestDEM[i]) then begin
                        GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                        AirOrDirt := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDTM);
                        BigMap.Add(DEMGlb[AirOrDirt].SelectionMap.MapDraw.FullMapfName);
                     end;
                  end;
                  DEMGlb[TestDEM[1]].SelectionMap.Allsamepixelsizeasthismap1Click(Nil);
                  MakeBigBitmap(BigMap,'');
               end;

               BigMap := tStringList.Create;
               if DoElevationDifference then begin
                  DoDifferenceMaps(AreaName,'elvd','Elevation',Graph1,Graph2);
                  AddImage(Graph1.Image1);
                  if (Graph2 <> Nil) then AddImage(Graph2.Image1);
               end;
               if DoSlopeDifference then begin
                  DoDifferenceMaps(AreaName,'slpd','Slope',Graph1,Graph2);
                  AddImage(Graph1.Image1);
                  if (Graph2 <> Nil) then AddImage(Graph2.Image1);
               end;
               if DoRoughnessDifference then begin
                  DoDifferenceMaps(AreaName,'rufd','Roughness',Graph1,Graph2);
                  AddImage(Graph1.Image1);
                  if (Graph2 <> Nil) then AddImage(Graph2.Image1);
               end;
               if (BigMap.Count > 0) then begin
                  fName := NextFileNumber(DEMIXtempFiles,AreaName + '_difference_histograms_','.png');
                  if (RefDSMArea = 0) then Cols := 1 else Cols := 2;
                  MakeBigBitmap(BigMap,'',fName,Cols);
                  DisplayBitmap(fName);
               end
               else BigMap.Free;
            end;
         end;
      end;
      HeavyDutyProcessing := false;
   end;
end;


procedure CheckVerticalDatumShift(var DEM : integer; VertDatum : ShortString);
//Reproject vertical datum to EGM2008 if required because DEM is EGM96
var
  Col,Row,NewDEM : integer;
  z,z2 : float32;
  Lat,Long : float64;
  DrawMap : boolean;
begin
   {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift in, DEM=' + IntToStr(DEM)); {$EndIf}
   if ValidDEM(DEM) then begin
      DrawMap := DEMGlb[DEM].SelectionMap <> Nil;
      //have to resave because input DEMs are all integer resolution
      DEMGlb[DEM].ResaveNewResolution(fcSaveFloatingPoint,NewDEM);
      DEMGlb[NewDEM].AreaName := DEMGlb[DEM].AreaName;  // + '_egm2008';
      if (VertDatum = '5773') then begin
         {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift with shift ' + DEMGlb[DEM].AreaName); {$EndIf}
         for Col := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumCol) do begin
            for Row := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumRow) do begin
                if DEMGlb[NewDEM].GetElevMetersOnGrid(Col,Row,z) then begin
                   DEMGlb[NewDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                   if DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                      DEMGlb[NewDEM].SetGridElevation(Col,Row,z+z2);
                   end;
                end;
            end;
         end;
         DEMGlb[NewDEM].CheckMaxMinElev;
      end
      else begin
         {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift without shift ' + DEMGlb[DEM].AreaName); {$EndIf}
         if DrawMap then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
      end;

      CloseSingleDEM(DEM);
      DEM := NewDEM;
      If Drawmap then CreateDEMSelectionMap(DEM,true,false,MDDef.DefDEMMap);
   end;
   {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift out, DEM=' + IntToStr(DEM)); {$EndIf}
end;


function LoadDEMIXarea(cfName : pathStr) : boolean;
var
   Rules : tMyData;
   fName : PathStr;
begin
   GeodeticFName := '';
   IceSatFName := '';
   LandCoverFName := '';
   LocalDatumAddFName := '';
   LocalDatumSubFName := '';
   RefDSMPointFName := '';
   RefDSMareaFName := '';
   RefDTMPointFName := '';
   RefDTMareaFName := '';
   COPRefDTMFName := '';
   COPRefDSMFName := '';

   DEMIXRefDEM := 0;
   AddLocalVDatum := 0;
   SubLocalVDatum := 0;
   RefDTMpoint := 0;
   RefDTMarea := 0;
   RefDSMpoint := 0;
   RefDSMarea := 0;
   GeoidGrid := 0;
   COPRefDTM := 0;
   COPRefDSM := 0;

   {$IfDef RecordDEMIX} writeLineToDebugFile('File Read rules ' + cfName); {$EndIf}
   Rules := tMyData.Create(cfName);
   Result := Rules.FieldExists('DATA') and Rules.FieldExists('FILENAME');
   if Result then begin
      while (not Rules.eof) do begin
         fName := Rules.GetFieldByNameAsString('FILENAME');
         if (fName <> '') then begin
            if Not FileExists(fName) then fName[1] := cfName[1];   //fix for external hard drive which moves around
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'GEODETIC' then GeodeticFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'ICESAT2' then IceSatFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_POINT' then RefDTMPointFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_AREA' then RefDTMareaFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_POINT' then RefDSMPointFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_AREA' then RefDSMareaFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'COP_REF_DTM' then COPRefDTMFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'COP_REF_DSM' then COPRefDSMFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_ADD' then LocalDatumAddFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_SUB' then LocalDatumSubFName := fName;
            if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LANDCOVER' then LandCoverFName := fName;
         end;
         Rules.Next;
      end;
      {$IfDef RecordDEMIXFull} writeLineToDebugFile('ProcessDEMIXtestarea in, rules read ' + ExtractFilePath(fName)); {$EndIf}
   end
   else begin
      MessageToContinue('Invalid file: ' + cfName);
   end;
   Rules.Destroy;
end;



function LoadDEMIXCandidateDEMs(RefDEM : integer; OpenMaps : boolean = false) : boolean;
var
   AllDEMs,WantSeries : shortstring;
   IndexSeriesTable : tMyData;
   WantDEM,WantImage,Ser,i : integer;
begin
   {$IfDef RecordDEMIXLoad} writeLineToDebugFile('LoadDEMIXCandidateDEMs in'); {$EndIf}
   AllDEMs := '';
   for I := 1 to MaxTestDEM do begin
      TestDEM[i] := 0;
      TestSeries[i] := '';
   end;

   OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.ApplyFilter('USE=' + QuotedStr('Y'));
   Ser := 0;
   Result := true;
   while not IndexSeriesTable.eof do begin
      WantSeries := IndexSeriesTable.GetFieldByNameAsString('SERIES');
      {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('Try ' + WantSeries + ' ' + IntToStr(Ser) + '/' + IntToStr(IndexSeriesTable.FiltRecsInDB)); {$EndIf}
      wmdem.SetPanelText(0,WantSeries);
      {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('Ref DEM=' + DEMGlb[RefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[RefDEM].DEMBoundBoxGeo,6)); {$EndIf}
      if LoadMapLibraryBox(WantDEM,WantImage,true,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,WantSeries,OpenMaps) and ValidDEM(WantDEM) then begin
         inc(Ser);
         TestDEM[Ser] := WantDEM;
         TestSeries[Ser] := IndexSeriesTable.GetFieldByNameAsString('SHORT_NAME');
         if not AllOfBoxInAnotherBox(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,DEMGlb[WantDEM].DEMBoundBoxGeo) then begin
            AllDEMs := AllDEMs + TestSeries[Ser] + ' (partial  ' + sfBoundBoxToString(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo) + ')   ';
         end;
         DEMGlb[WantDEM].AreaName := TestSeries[Ser];
         DEMGlb[WantDEM].DEMFileName := NextFileNumber(MDTempDir, DEMGlb[TestDEM[Ser]].AreaName + '_', '.dem');

         {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Opened:' + WantSeries + '  DEM=' + IntToStr(WantDEM)); {$EndIf}
         if (RefDEM <> 0) then DEMGlb[TestDEM[Ser]].SelectionMap.ClipDEMtoregion(DEMGlb[RefDEM].DEMBoundBoxGeo);
         CheckVerticalDatumShift(TestDEM[Ser],IndexSeriesTable.GetFieldByNameAsString('VERT_DATUM'));
      end
      else begin
         AllDEMs := AllDEMs + WantSeries + ' (missing)  ';
         Result := false;
      end;
      IndexSeriesTable.Next;
   end;
   IndexSeriesTable.Destroy;
   CloseSingleDEM(GeoidGrid);
   {$IfDef RecordDEMIX} if (AllDEMs <> '') then HighlightLineToDebugFile(' DEM problem, ' + AllDEMs); {$EndIf}
   {$IfDef RecordDEMIXLoad} writeLineToDebugFile('LoadDEMIXCandidateDEMs out'); {$EndIf}
end;


function LoadDEMIXReferenceDEMs(var RefDEM : integer) : boolean;

         procedure ReferenceFileOpen(var DEM : integer; fName : PathStr);
         begin
            if (FName <> '') and FileExists(fName) then begin
               DEM := OpenNewDEM(FName);   //must load map for the DEMIX tile computation
               if (DEM <> 0) then begin
                  //move to EGM2008
                  if AddLocalVDatum <> 0 then DEMGlb[DEM].AddaDEM(AddLocalVDatum);
                  if SubLocalVDatum <> 0 then DEMGlb[DEM].SubtractaDEM(AddLocalVDatum);
                  if (RefDEM = 0) then RefDEM := DEM;
               end;
            end
            else DEM := 0;
         end;

begin
   RefDEM := 0;
   if (LocalDatumAddFName <> '') and FileExists(LocalDatumAddFName) then AddLocalVDatum := OpenNewDEM(LocalDatumAddFName, false);
   if (LocalDatumSubFName <> '') and FileExists(LocalDatumSubFName) then SubLocalVDatum := OpenNewDEM(LocalDatumSubFName, false);

   ReferenceFileOpen(RefDTMpoint,RefDTMpointFName);
   ReferenceFileOpen(RefDTMarea,RefDTMareaFName);
   ReferenceFileOpen(RefDSMpoint,RefDSMpointFName);
   ReferenceFileOpen(RefDSMarea,RefDSMareaFName);
   ReferenceFileOpen(COPRefDTM,COPRefDTMFName);
   ReferenceFileOpen(COPRefDSM,COPRefDSMFName);
   Result := ValidDEM(RefDEM);
   if Result then begin
      {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('ProcessDEMIXtestarea in, ref DEMs open'); {$EndIf}
      if (AddLocalVDatum <> 0) then CloseSingleDEM(AddLocalVDatum);
      if (SubLocalVDatum <> 0) then CloseSingleDEM(SubLocalVDatum);

      GeoidGrid := OpenNewDEM(GeoidDiffFName,false,'geoid difference from EGM96 to EGM2008');  //to move DEMs from EGM96 to EGM2008
      GeoidDiffFName := DEMGlb[GeoidGrid].DEMFileName;
      {$If Defined(RecordFullDEMIX)} WriteLineToDebugFile('ProcessDEMIXtestarea in, geoid grid opened, REFDEM=' + IntToStr(RefDEM)); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIX} HighlightLineToDebugFile('Failure, ref DEMs open '  + IntToStr(RefDEM)); {$EndIf}
   end;
end;



end.
