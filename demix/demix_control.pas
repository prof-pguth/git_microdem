unit demix_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   {$Define RecordDEMIXLoad}
   //{$Define RecordDEMIXMovies}
   //{$Define RecordDEMIXVDatum}
   //{$Define RecordFullDEMIX}
   //{$Define ShowDEMIXWhatsOpen}
{$EndIf}


interface

uses
    System.SysUtils,System.Classes,StrUtils,VCL.ExtCtrls,
    Petmar,Petmar_types;


function LoadDEMIXReferenceDEMs(var RefDEM : integer) : boolean;
function LoadDEMIXCandidateDEMs(RefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
function LoadDEMIXarea(cfName : pathStr) : boolean;
procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
procedure OpenDEMIXArea(fName : PathStr = '');

function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;

procedure CopAlosCompareReference;

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
   DEMstat,Make_grid,PetImage,BaseGraf,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,
   DEMCoord,DEMdefs,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,Petmar_db,PetMath;



procedure CopAlosCompareReference;


   procedure DoOne(VatName,OutName : PathStr; Cats : integer);
   var
      TheFiles,Results : tStringList;
      i : Integer;
      Table : tMyData;
      Sum : float64;
      Fname : PathStr;
      aLine : shortstring;
   begin
      TheFiles := tStringList.Create;
      Petmar.FindMatchingFiles('H:\aa_half_sec_test\',VATname,TheFiles,2);
      Results := tStringList.Create;
      if Cats = 4 then Results.Add('AREA,BOTH_HIGH,BOTH_CLOSE,BOTH_LOW,COMPLEX')
      else Results.Add('area,HIGH_HIGH,HIGH_GOOD,HIGH_LOW,GOOD_HIGH,GOOD_GOOD,GOOD_LOW,LOW_HIGH,LOW_GOOD,LOW_LOW');
      for i := 0 to pred(TheFiles.Count) do begin
         fName := TheFiles.Strings[i];
         Table := tMyData.Create(fName);
         Sum := Table.FieldSum('N');
         aline := LastSubDir(ExtractFilePath(fName));
         Table.First;
         while not Table.eof do begin
            aline := aline + ',' + RealToString(Table.GetFieldByNameAsInteger('N') * 100 / Sum, -12,-2);
            Table.Next;
         end;
         Results.Add(aline);
         Table.Destroy;
      end;
      TheFiles.Free;
      PetdbUtils.StringList2CSVtoDB(Results,OutName);
   end;


begin
   DoOne('cop-alos-dtm4.vat.dbf',mdTempDir + 'dtm-4.dbf',4);
   DoOne('cop-alos-dsm4.vat.dbf',mdTempDir + 'dsm-4.dbf',4);
   DoOne('cop-alos-dtm9.vat.dbf',mdTempDir + 'dtm-9.dbf',9);
   DoOne('cop-alos-dsm9.vat.dbf',mdTempDir + 'dsm-9.dbf',9);
end;



function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
begin
   DEMName := UpperCase(DEMName);
   if DEMName = 'TIE' then Result := claBrown
   else if DEMName = 'ALOS' then Result := RGBtrip(230,159,0)
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
   TestGrid,DSMgrid,DTMGrid,DiffGrid,
   i,NoSlopeMap,UseDSM,UseDTM : integer;
   fName : PathStr;
   Min,Max,BinSize : float32;
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
            if (ShortName = 'elvd') then begin
               DSMGrid := UseDSM;
            end;
            if (ShortName = 'slpd') then begin
               DSMGrid := CreateSlopeMap(UseDSM);
            end;
            if (ShortName = 'rufd') then begin
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
   DEMIX_Movie = false;
   DEMIX_SaveDEMs = true;
   DEMIX_HalfSecondCompareMaps = true;
   DEMIX_GeomorphMaps = true;
   DEMIXhistograms = false;
var
   AreaName : shortstring;
   HalfSec : array[1..10] of integer;
   UseDSM,UseDTM,chm,HalfSecRefDTM,HalfSecRefDSM,SlopeMap,RuffMap,AspectMap,COP_ALOS_Diff,
   COP_ALOS_DSM4,COP_ALOS_DTM4,COP_ALOS_DSM9,COP_ALOS_DTM9,
   i,Cols : integer;
   AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;
   BigMap,Movie : tStringList;
   Graph1,Graph2 : tThisBaseGraph;
   SaveDir : PathStr;


         procedure AddImage(GraphImage : tImage);
         var
           Bitmap : tMyBitmap;
         begin
            if MDDef.DEMIXCompositeImage then begin
               CopyImageToBitmap(GraphImage,Bitmap);
               Bitmap.Height := Bitmap.Height + 25;
               fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
               Bitmap.SaveToFile(fName);
               BigMap.Add(fName);
            end;
         end;

         function CreateHalfSecRefDEM(RefPoint,RefArea : integer) : integer;
         begin
             if (RefPoint <> 0) and (RefArea <> 0) then begin
                Result := DEMGlb[RefPoint].SelectionMap.CreateGridToMatchMap(cgLatLong,false,FloatingPointDEM,0.5,0.5,MDdef.DefaultUTMZone,PixelIsPoint);
                DEMGlb[Result].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[Result].FullDEMGridLimits,RefPoint,hfOnlyHole);
                DEMGlb[Result].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[Result].FullDEMGridLimits,RefArea,hfOnlyHole);
                DEMGlb[Result].InterpolateAcrossHoles(false);
                DEMGlb[Result].AreaName := DEMGlb[RefPoint].AreaName + '_half_sec';
                CreateDEMSelectionMap(Result,true,true,mtIHSReflect);
                CloseSingleDEM(RefPoint);
                CloseSingleDEM(RefArea);
             end
             else begin
                Result := 0;
             end;
         end;

         procedure AddFrame(DEM : integer; What : ShortString);
         begin
            if ValidDEM(DEM) then begin
               {$IfDef RecordDEMIXMovies} writeLineToDebugFile('Add frame, dem=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName + '  ' + DEMGlb[DEM].SelectionMap.MapDraw.FullMapfName); {$EndIf}
               Movie.Add(DEMGlb[DEM].SelectionMap.MapDraw.FullMapfName);
            end
            else begin
               {$IfDef RecordDEMIXMovies} writeLineToDebugFile('Could not find map for DEM=' + IntToStr(DEM) + '  ' + What); {$EndIf}
            end;
         end;

         procedure SaveDEM(DEM : integer; fName : PathStr);
         begin
            if ValidDEM(DEM) then begin
               if (not FileExists(fName)) then DEMGlb[DEM].WriteNewFormatDEM(fName);
               fName := ChangeFileExt(fName,'.png');
               SaveImageAsBMP(DEMGlb[DEM].SelectionMap.Image1,fName);
            end;
         end;



begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      if DEMIX_SaveDEMs then begin
         SaveDir := 'H:\aa_half_sec_test\' + AreaName + '\';
         if PathIsValid(SaveDir) then exit;
         SafeMakeDir(SaveDir);
      end;
try
      HeavyDutyProcessing := true;
      AutoOverwriteDBF := true;
      NakedMapOptions;  //also does SaveBackupDefaults;
      MDDef.ScaleBarLocation.DrawItem := true;
      MDDef.GIFfileLabels := false;
      MDdef.DefaultMapXSize := 1000;
      MDdef.DefaultMapYSize := 1000;
      MDDef.MapNameLocation.MapPosition := lpNEmap;
      MDDef.MapNameLocation.DrawItem := true;

      UseDSM := 0;
      UseDTM := 0;
      HalfSecRefDTM := 0;
      HalfSecRefDSM := 0;
      SlopeMap := 0;
      RuffMap := 0;
      COP_ALOS_DSM4 := 0;
      COP_ALOS_DTM4 := 0;
      COP_ALOS_DSM9 := 0;
      COP_ALOS_DTM9 := 0;
      COP_ALOS_Diff := 0;
      CHM := 0;
      for I := 1 to 10 do begin
         HalfSec[i] := 0;
         AirOrDirt[i] := 0;
         AirOrDirt2[i] := 0;
         AirOrDirt3[i] := 0;
      end;

      MDDef.GridLegendLocation.DrawItem := true;
      MDDef.MapNameLocation.DrawItem := true;
      if LoadDEMIXarea(fName) then begin
         {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXarea complete'); {$EndIf}
         if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXReferenceDEMs complete; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + 'DEMIXRefDEM=' + IntToStr(DEMIXRefDEM)); {$EndIf}

            if MDDef.DEMIX_DoHalfSecDEMs then begin
               HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
               HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
               DEMIXRefDEM := HalfSecRefDTM;
               {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec ref dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            end;

            if DEMIX_GeomorphMaps then begin
               SlopeMap := -1;   //forces creation of slope and roughness maps
               RuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(HalfSecRefDTM,3,SlopeMap);
               DEMGlb[SlopeMap].AreaName := 'ref_dtm_slope_%';
               DEMGlb[RuffMap].AreaName := 'ref_dtm_roughness_%';
               AspectMap := MakeAspectMap(HalfSecRefDTM);
            end;

            {$IfDef RecordDEMIX} writeLineToDebugFile('Slope and Ruff dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

            if (MDDef.DEMIX_DoCHM) then begin
               if ValidDEM(HalfSecRefDSM) and ValidDEM(HalfSecRefDTM) then begin
                  chm := MakeDifferenceMapOfBoxRegion(HalfSecRefDTM,HalfSecRefDSM,DEMGlb[HalfSecRefDTM].FullDEMGridLimits,true,false,false,AreaName + '_half_sec_chm');
                  //DEMglb[chm].SelectionMap.Elevationpercentiles1Click(Nil);
                  {$IfDef RecordDEMIX} writeLineToDebugFile('CHM created; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               end;
            end;

            {$IfDef RecordDEMIX} writeLineToDebugFile('Ref dems done; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

            {$IfDef ShowDEMIXWhatsOpen} writeStringListToDebugFile(GetWhatsOpen); {$EndIf}

            if MDDef.DEMIX_DoHalfSecDEMs or MDDef.DEMIX_DoElevParamGraphs or MDDef.DEMIX_DoAirOrDirt or MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff then begin
               if LoadDEMIXCandidateDEMs(DEMIXRefDEM,true,false) then begin
                  {$IfDef RecordDEMIX} HighlightLineToDebugFile('Candidate DEMs loaded'); {$EndIf}
                  {$IfDef ShowDEMIXWhatsOpen} writeLineToDebugFile('candidates loaded');  writeStringListToDebugFile(GetWhatsOpen); {$EndIf}
                  if MDDef.DEMIX_DoHalfSecDEMs then begin
                     {$IfDef RecordDEMIX} writeLineToDebugFile('Start creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                     for I := 1 to MaxTestDEM do begin
                        if ValidDEM(TestDEM[i]) then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Initial DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                           fName := MDtempDir + DEMGlb[TestDEM[i]].AreaName + '_geo_reint_0.5sec.dem';
                           HalfSec[i] := DEMGlb[TestDEM[i]].ReinterpolateLatLongDEM(0.5,fName);
                           {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[HalfSec[i]].AreaName + ' new half second'); {$EndIf}
                           CloseSingleDEM(TestDEM[i]);
                           TestDEM[i] := HalfSec[i];
                           {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' now TestDEM=' + IntToStr(TestDEM[i])); {$EndIf}
                           CreateDEMSelectionMap(TestDEM[i],true,true,mtIHSReflect);
                           {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Half sec DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                           if MDDef.DEMIX_DoAirOrDirt then begin
                              AirOrDirt[i] := AirBallDirtBallMap(TestDEM[i],HalfSecRefDSM ,HalfSecRefDTM, DEMGlb[TestDEM[i]].AreaName + '_canopy' ); //checks if point within canopy
                              AirOrDirt2[i] := AirBallDirtBallMap(TestDEM[i],HalfSecRefDSM ,0,DEMGlb[TestDEM[i]].AreaName + 'air_dirt_dsm');     //compares to DSM
                              AirOrDirt3[i] := AirBallDirtBallMap(TestDEM[i],0,HalfSecRefDTM,DEMGlb[TestDEM[i]].AreaName + 'air_dirt_dtm');     //compares to DTM
                              {$IfDef RecordDEMIX} writeLineToDebugFile('air or dirt done, canopy=' + IntToStr(AirOrDirt[i]) + '  dsm=' + IntToStr(AirOrDirt2[i])   + '  dtm=' + IntToStr(AirOrDirt3[i]) ); {$EndIf}
                           end;
                         end;
                     end;

                     if DEMIX_HalfSecondCompareMaps then begin
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec candidates done, Make difference map'); {$EndIf}
                        COP_ALOS_Diff := MakeDifferenceMap(TestDEM[1],TestDEM[2],true,false,false,'COP-ALOS_difference');

                        {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DSM COP-ALOS'); {$EndIf}
                        COP_ALOS_DSM4 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],'DSM',true,'COP-ALOS_compare_DSM-4');
                        COP_ALOS_DSM9 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],'DSM',false,'COP-ALOS_compare_DSM-9');

                        {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DTM COP-ALOS'); {$EndIf}
                        COP_ALOS_DTM4 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],'DTM',true,'COP-ALOS_compare_DTM-4');
                        COP_ALOS_DTM9 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],'DTM',false,'COP-ALOS_compare_DTM-9');

                        if DEMIXhistograms then begin
                           HistogramsFromVATDEM(COP_ALOS_DSM4,HalfSecRefDSM,SlopeMap,RuffMap,AspectMap);
                           HistogramsFromVATDEM(COP_ALOS_DTM4,HalfSecRefDTM,SlopeMap,RuffMap,AspectMap);
                        end;
                     end;
                  end;

                  {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec dems done; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

                  if MDDef.DEMIX_DoAirOrDirt then begin
                     if MDDef.DEMIXCompositeImage then begin
                        BigMap := tStringList.Create;
                        BigMap.Add(DEMGlb[RefDTMPoint].SelectionMap.MapDraw.FullMapfName);
                     end;
                     for I := 1 to MaxTestDEM do begin
                        if ValidDEM(TestDEM[i]) then begin
                           GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                           AirOrDirt[i] := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDTM);
                           AirOrDirt2[i] := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDSM);
                           if MDDef.DEMIXCompositeImage then begin
                              BigMap.Add(DEMGlb[AirOrDirt[i]].SelectionMap.MapDraw.FullMapfName);
                              if (AirOrDirt2[i] <> 0) then BigMap.Add(DEMGlb[AirOrDirt2[i]].SelectionMap.MapDraw.FullMapfName);
                           end;
                        end;
                     end;
                     DEMGlb[TestDEM[1]].SelectionMap.Allsamepixelsizeasthismap1Click(Nil);
                     if MDDef.DEMIXCompositeImage and (BigMap.Count > 0) then MakeBigBitmap(BigMap,'');
                  end;

                  if MDDef.DEMIX_DoElevParamGraphs then ElevationSlopePlot(0);

                  if MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff then begin
                     if MDDef.DEMIXCompositeImage then BigMap := tStringList.Create;    //actually this will be graphs, not maps
                     if MDDef.DEMIX_DoElevDiff then begin
                        DoDifferenceMaps(AreaName,'elvd','Elevation',Graph1,Graph2);
                        AddImage(Graph1.Image1);
                        if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                     end;
                     if MDDef.DEMIX_DoSlopeDiff then begin
                        DoDifferenceMaps(AreaName,'slpd','Slope',Graph1,Graph2);
                        AddImage(Graph1.Image1);
                        if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                     end;
                     if MDDef.DEMIX_DoRuffDiff then begin
                        DoDifferenceMaps(AreaName,'rufd','Roughness',Graph1,Graph2);
                        AddImage(Graph1.Image1);
                        if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                     end;
                     if MDDef.DEMIXCompositeImage then begin
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
            end;
         end;
      end;

      {$IfDef ShowDEMIXWhatsOpen} writeStringListToDebugFile(GetWhatsOpen); {$EndIf}

      if DEMIX_Movie then begin
         Movie := tStringList.Create;
         fName := NextFileNumber(MDtempDir,'map_4_movie_','.mov');
         {$IfDef RecordDEMIXMovies} WriteLineToDebugFile('Making movie, ' + fName); {$EndIf}
         AddFrame(HalfSecRefDTM,'Ref DTM');
         AddFrame(HalfSecRefDSM,'Ref DSM');
         for i := 1 to 2 do begin
            AddFrame(AirOrDirt[i],'AirDirt');
            AddFrame(AirOrDirt2[i],'AirDirt2');
            AddFrame(AirOrDirt3[i],'AirDirt3');
         end;

         AddFrame(SlopeMap,'Ref slope');
         AddFrame(RuffMap,'Ref ruff');
         AddFrame(COP_ALOS_DSM4,'COP_ALOS_DSM4');
         AddFrame(COP_ALOS_DTM4,'COP_ALOS_DTM4');
         AddFrame(COP_ALOS_DSM9,'COP_ALOS_DSM9');
         AddFrame(COP_ALOS_DTM9,'COP_ALOS_DTM9');

         Movie.SaveToFile(fName);
         CreateNewMovie(fName);
         Movie.Free;
      end;

      if DEMIX_SaveDEMs then begin
         {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea start saving DEMs'); {$EndIf}
         SaveDEM(TestDEM[1],SaveDir + 'alos.dem');
         SaveDEM(TestDEM[2],SaveDir + 'cop.dem');

         SaveDEM(HalfSecRefDSM,SaveDir + 'ref_dsm.dem');
         SaveDEM(HalfSecRefDTM,SaveDir + 'ref_dtm.dem');

         if DEMIX_HalfSecondCompareMaps then begin
            SaveDEM(COP_ALOS_Diff,SaveDir + 'cop-alos-diff.dem');
            SaveDEM(COP_ALOS_DSM4,SaveDir + 'cop-alos-dsm4.dem');
            SaveDEM(COP_ALOS_DTM4,SaveDir + 'cop-alos-dtm4.dem');
            SaveDEM(COP_ALOS_DSM9,SaveDir + 'cop-alos-dsm9.dem');
            SaveDEM(COP_ALOS_DTM9,SaveDir + 'cop-alos-dtm9.dem');
         end;

         if DEMIX_GeomorphMaps then begin
            SaveDEM(SlopeMap,SaveDir + 'slope_ref_dtm.dem');
            SaveDEM(RuffMap,SaveDir + 'roughness_ref_dtm.dem');
            SaveDEM(AspectMap,SaveDir + 'aspect_ref_dtm.dem');
         end;
      end;
finally
      HeavyDutyProcessing := false;
      AutoOverwriteDBF := false;
      RestoreBackupDefaults;
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea out'); {$EndIf}
end;
   end;
end;


procedure CheckVerticalDatumShift(var DEM : integer; VertDatum : ShortString);
//Reproject vertical datum to EGM2008 if required because DEM is EGM96
//we could not get this to run doing nothing for DEM already on EGM2008
var
  Col,Row,NewDEM : integer;
  z,z2 : float32;
  Lat,Long : float64;
  DrawMap : boolean;
begin
   {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift in, DEM=' + IntToStr(DEM)); {$EndIf}
   DrawMap := (DEMGlb[DEM].SelectionMap <> Nil);
   if ValidDEM(DEM) then begin
      NewDEM := DEMGlb[DEM].ResaveNewResolution(fcSaveFloatingPoint); //have to resave because input DEMs are all integer resolution
      DEMGlb[NewDEM].AreaName := DEMGlb[DEM].AreaName;  // + '_egm2008';
      {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift with shift ' + DEMGlb[DEM].AreaName); {$EndIf}
      z2 := 0;
      for Col := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumCol) do begin
         for Row := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumRow) do begin
             if DEMGlb[NewDEM].GetElevMetersOnGrid(Col,Row,z) then begin
                DEMGlb[NewDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                if (VertDatum <> '5773')or DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                   DEMGlb[NewDEM].SetGridElevation(Col,Row,z+z2);
                end;
             end;
         end;
      end;
      DEMGlb[NewDEM].CheckMaxMinElev;
      CloseSingleDEM(DEM);
      DEM := NewDEM;
      If Drawmap then CreateDEMSelectionMap(DEM,true,false,MDDef.DefDEMMap);
   end;
   {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift out, DEM=' + IntToStr(DEM)); {$EndIf}
end;


function LoadDEMIXarea(cfName : pathStr) : boolean;
var
   fName : PathStr;
   db : integer;
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
   //Rules := tMyData.Create(cfName);
   db := OpenDataBase('DEMIX area rules',cfName, false);
   Result := GISdb[db].MyData.FieldExists('DATA') and GISdb[db].MyData.FieldExists('FILENAME');
   if Result then begin
      while (not GISdb[db].MyData.eof) do begin
         fName := GISdb[db].MyData.GetFieldByNameAsString('FILENAME');
         if (fName <> '') then begin
            if Not FileExists(fName) then fName[1] := cfName[1];   //fix for external hard drive which moves around
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'GEODETIC' then GeodeticFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'ICESAT2' then IceSatFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_POINT' then RefDTMPointFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_AREA' then RefDTMareaFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_POINT' then RefDSMPointFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_AREA' then RefDSMareaFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'COP_REF_DTM' then COPRefDTMFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'COP_REF_DSM' then COPRefDSMFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_ADD' then LocalDatumAddFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_SUB' then LocalDatumSubFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LANDCOVER' then LandCoverFName := fName;
         end;
         GISdb[db].MyData.Next;
      end;
      {$IfDef RecordDEMIXFull} writeLineToDebugFile('ProcessDEMIXtestarea in, rules read ' + ExtractFilePath(fName)); {$EndIf}
   end
   else begin
      MessageToContinue('Invalid file: ' + cfName);
   end;
   CloseAndNilNumberedDB(db);
end;



function LoadDEMIXCandidateDEMs(RefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
var
   AllDEMs,WantSeries,ShortName : shortstring;
   IndexSeriesTable : tMyData;
   WantDEM,WantImage,Ser,i,NumPts : integer;
begin
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   Result := false;
   AllDEMs := '';
   for I := 1 to MaxTestDEM do begin
      TestDEM[i] := 0;
      TestSeries[i] := '';
   end;

   OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.ApplyFilter('USE=' + QuotedStr('Y'));
   Ser := 0;
   while not IndexSeriesTable.eof do begin
      WantSeries := IndexSeriesTable.GetFieldByNameAsString('SERIES');
      ShortName := IndexSeriesTable.GetFieldByNameAsString('SHORT_NAME');
      if AllCandidates or (ShortName = 'COP') or (ShortName = 'ALOS') then begin
         {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoad)} writeLineToDebugFile('Try ' + WantSeries + ' ' + ShortName + '  ' + IntToStr(Ser) + '/' + IntToStr(IndexSeriesTable.FiltRecsInDB)); {$EndIf}
         wmdem.SetPanelText(0,WantSeries);
         {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('Ref DEM=' + DEMGlb[RefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[RefDEM].DEMBoundBoxGeo,6)); {$EndIf}
         if LoadMapLibraryBox(WantDEM,WantImage,true,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,WantSeries,OpenMaps) and ValidDEM(WantDEM) then begin
            {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs done LoadMapLib; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            inc(Ser);
            TestDEM[Ser] := WantDEM;
            TestSeries[Ser] := ShortName;
            if not AllOfBoxInAnotherBox(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,DEMGlb[WantDEM].DEMBoundBoxGeo) then begin
               AllDEMs := AllDEMs + TestSeries[Ser] + ' (partial  ' + sfBoundBoxToString(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo) + ')  ';
            end;
            DEMGlb[TestDEM[Ser]].AreaName := TestSeries[Ser];
            DEMGlb[TestDEM[Ser]].DEMFileName := NextFileNumber(MDTempDir, DEMGlb[TestDEM[Ser]].AreaName + '_', '.dem');

            {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Opened:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            DEMGlb[TestDEM[Ser]].SelectionMap.ClipDEMtoregion(DEMGlb[RefDEM].DEMBoundBoxGeo);
            {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Clipped:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            if (DEMGlb[TestDEM[Ser]].DEMHeader.MinElev < 0.01) then DEMGlb[TestDEM[Ser]].MarkInRangeMissing(0,0,NumPts);
            CheckVerticalDatumShift(TestDEM[Ser],IndexSeriesTable.GetFieldByNameAsString('VERT_DATUM'));
            {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Datum shift:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            Result := true;
         end
         else begin
            AllDEMs := AllDEMs + WantSeries + ' (missing)  ';
         end;
      end;
      IndexSeriesTable.Next;
   end;
   IndexSeriesTable.Destroy;
   CloseSingleDEM(GeoidGrid);
   {$IfDef RecordDEMIX} if (AllDEMs <> '') then HighlightLineToDebugFile(' DEM problem, ' + AllDEMs); {$EndIf}
   {$IfDef RecordDEMIXLoad} writeLineToDebugFile('LoadDEMIXCandidateDEMs out; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
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
