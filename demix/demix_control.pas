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
   {$Define RecordDEMIXsave}
   {$Define RecordCreateHalfSec}
   //{$Define RecordDEMIXMovies}
   //{$Define RecordDEMIXVDatum}
   //{$Define RecordFullDEMIX}
   //{$Define ShowDEMIXWhatsOpen}
{$EndIf}


interface

uses
    System.SysUtils,System.Classes,StrUtils,VCL.ExtCtrls,VCL.Forms, VCL.Graphics, WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf;

const
   caBest = 1;
   ca4cat = 2;
   ca9cat = 3;

//service functions and procedures
   function LoadDEMIXReferenceDEMs(var RefDEM : integer) : boolean;
   function LoadDEMIXCandidateDEMs(RefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   function LoadDEMIXarea(cfName : pathStr) : boolean;
   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   procedure OpenDEMIXArea(fName : PathStr = '');


//map comparisons
   procedure CopAlosCompareReference;
   procedure PixelByPixelCopAlos;
   procedure HighLowCopAlosGeomorphometry(fName : PathStr = '');
   procedure COP_ALOS_compare(What : integer);
   procedure OpenDEMIXRidges(fName : PathStr = '');
   procedure HillshadesForWineContest(fName : PathStr = '');



//DEMIX wine contest procedures based on the database
   procedure WinsAndTies(DBonTable : integer);
   function BestByParameterSorting(DBonTable : integer; TileParam,Criterion,DEMtype : shortstring) : tThisBaseGraph;
   procedure MultipleBestByParameters(DBonTable : integer);
   function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
   procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
   procedure BestDEMSbyCategory(DBonTable : integer);


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
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   DEMCoord,DEMdefs,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,Petmar_db,PetMath;


function CreateHalfSecRefDEM(RefPoint,RefArea : integer; CloseInputs : boolean = true) : integer;
//intended for two DEMs covering the same area with different pixel-is-what geometries, such at COP and ALOS
//places the elevations from both at the correct locations in a 1/2 sec grid, and then linearly interpolates to fill voids
//can also be used for a single DEM
var
   fName : PathStr;

   function Case1 : integer;
   begin
       Result := DEMGlb[RefPoint].SelectionMap.CreateGridToMatchMap(cgLatLong,false,FloatingPointDEM,0.5,0.5,MDdef.DefaultUTMZone,PixelIsPoint);
       if (RefPoint <> 0) then begin
          DEMGlb[Result].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[Result].FullDEMGridLimits,RefPoint,hfJustReferencePostings);
          {$If Defined(RecordCreateHalfSec)} fName := MDTempDir + 'HalfSecAddFirst.dem'; DEMGlb[Result].WriteNewFormatDEM(fName); {$EndIf}
       end;

       if (RefArea <> 0) then begin
          DEMGlb[Result].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[Result].FullDEMGridLimits,RefArea,hfJustReferencePostings);
          {$If Defined(RecordCreateHalfSec)} fName := MDTempDir + 'HalfSecAddSecond.dem'; DEMGlb[Result].WriteNewFormatDEM(fName); {$EndIf}
       end;
       //DEMGlb[Result].InterpolateAcrossHoles(false);   //did not work, and this routine might require adjustment
       DEMGlb[Result].SpecialInterpolateAcrossHoles;     //gives spikey results
       {$If Defined(RecordCreateHalfSec)} fName := MDTempDir + 'HalfSecInterpolated_case1.dem'; DEMGlb[Result].WriteNewFormatDEM(fName); {$EndIf}
       DEMGlb[Result].AreaName := DEMGlb[RefPoint].AreaName + '_half_sec';
       CreateDEMSelectionMap(Result,true,true,mtIHSReflect);
   end;


   function Case2 : integer;
   var
      x,y : integer;
      Lat,Long : float64;
      z1,z2 : float32;
   begin
      if (RefPoint <> 0) and  (RefArea <> 0) then begin
          Result := DEMGlb[RefPoint].SelectionMap.CreateGridToMatchMap(cgLatLong,false,FloatingPointDEM,0.5,0.5,MDdef.DefaultUTMZone,PixelIsPoint);
          DEMGlb[Result].AreaName := DEMGlb[RefPoint].AreaName + '_half_sec_double_average_intepolation';
          for x := 0 to pred(DEMGlb[Result].DEMHeader.NumCol) do begin
             for y := 0 to pred(DEMGlb[Result].DEMHeader.NumRow) do begin
                 DEMGlb[Result].DEMgridToLatLongDegree(x,y,Lat,Long);
                 if DEMGlb[RefPoint].GetElevFromLatLongDegree(Lat,Long,z1) and  DEMGlb[RefArea].GetElevFromLatLongDegree(Lat,Long,z2) then
                    DEMGlb[Result].SetGridElevation(x,y,0.5 * (z1 + z2));
             end;
          end;
          DEMGlb[Result].CheckMaxMinElev;
          {$If Defined(RecordCreateHalfSec)} fName := MDTempDir + DEMGlb[Result].AreaName + '.dem'; DEMGlb[Result].WriteNewFormatDEM(fName); {$EndIf}
          CreateDEMSelectionMap(Result,true,true,mtIHSReflect);
      end;
   end;



begin
    Result := 0;
    if (RefPoint <> 0) or (RefArea <> 0) then begin
       Case1;
       Case2;

       if CloseInputs then begin
          CloseSingleDEM(RefPoint);
          CloseSingleDEM(RefArea);
       end;
    end;
end;


function CriterionTieTolerance(Criterion : shortstring) : float32;
var
    TieToleranceTable : tMyData;
begin
   TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);
   TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
   Result := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');
   TieToleranceTable.Destroy;
end;


procedure GetFilterAndHeader(i,j : integer; var aHeader,aFilter : shortString);
var
   RefFilter : shortstring;
begin
   RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
   case j of
      1 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg slope > 18%';
            aFilter := 'AVG_SLOPE > 18' + RefFilter;
          end;
      2 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg slope < 18%';
            aFilter := 'AVG_SLOPE < 18' + RefFilter;
          end;
      3 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile relief < 25m';
            aFilter := 'RELIEF < 25' + RefFilter;
          end;
      4 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile relief > 500m';
            aFilter := 'RELIEF > 500' + RefFilter;
          end;
      5 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg rough > 10%';
            aFilter := 'AVG_ROUGH > 10' + RefFilter;
          end;
      6 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg rough < 5%';
            aFilter := 'AVG_ROUGH < 5' + RefFilter;
          end;
      7 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile urban > 25%';
            aFilter := 'URBAN_PC > 25' + RefFilter;
          end;
      8 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile forest > 50%';
            aFilter := 'FOREST_PC > 50' + RefFilter;
          end;
      9 : begin
            aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile barren > 25%';
            aFilter := 'BARREN_PC > 25' + RefFilter;
          end;
   end;
end;


procedure COP_ALOS_compare(What : integer);
var
   RefDEM,ALOS,Cop,DEM4 : integer;
begin
   PickSeveralExistingDEMs('Reference DEM','ALOS','COPDEM','', RefDEM,ALOS,Cop,DEM4);
   ReadDefault('Tolerance',MDDef.DEMIXSimpleTolerance);
   case What of
      caBest : BestCopOrALOSmap(RefDEM,ALOS,Cop,MDDef.DEMIXSimpleTolerance,'ALOS-COP-better');
      ca4Cat : TwoDEMHighLowMap(RefDEM,ALOS,COP,'Ref DEM',MDDef.DEMIXSimpleTolerance,true);
      ca9Cat : TwoDEMHighLowMap(RefDEM,ALOS,COP,'Ref DEM',MDDef.DEMIXSimpleTolerance,false);
   end;
end;



procedure PixelByPixelCopAlos;
const
   Params : array[1..3] of shortstring = ('best_elev_ref_dtm.vat.dbf','best_slope_ref_dtm.vat.dbf','best_roughness_ref_dtm.vat.dbf');
   Names : array[1..3] of shortstring = ('elevation','slope','roughness');
var
   Dir : PathStr;
   Results,TheFiles : tStringList;
   Sum : float64;
   i,j,db : integer;
   fName : PathStr;
   aLine : shortstring;
begin
   Dir := 'H:\aa_half_sec_test\';
   Results := tStringList.Create;
   Results.Add('AREA,PARAM,ALOS_BEST,BOTH_TIED,COP_BEST');
   for I := 1 to 3 do begin
       TheFiles := tStringList.Create;
       FindMatchingFiles(Dir,Params[i],TheFiles,2);
       for j := 0 to pred(TheFiles.Count) do begin
          fName := TheFiles.Strings[j];
          OpenNumberedGISDataBase(db,fName);
          GISdb[db].RenameField('NAME',Names[i]);
          sum := GISdb[db].MyData.FieldSum('N');
          aLine := LastSubDir(ExtractFilePath(fName)) + ',' + Names[i];
          GISdb[db].MyData.First;
          while not GISdb[db].MyData.eof do begin
             aline := aline + ',' + RealToString(100 * GISdb[db].MyData.GetFieldByNameAsInteger('N') / Sum,-12,-2);
             GISdb[db].MyData.Next;
          end;
          Results.Add(aline);
       end;
       TheFiles.Destroy;
   end;
   fName := NextFileNumber(MDTempDir,'p_by_cop_alos','.dbf');
   StringList2CSVtoDB(Results,fName);
end;



procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
var
   which,RefFilter : shortstring;
   Compare,i,j,ties,opinions,db : integer;
   fName : PathStr;
   Findings,Criteria,DEMs : tStringList;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,Cop,ALOS,Ties,FAB,dem : integer;
      aLine : shortString;
      Counts : array[0..10] of integer;
   begin
      {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DO-ONE  ' + theFilter); {$EndIf}
      WMDEM.SetPanelText(1,theFilter);
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (Opinions >= 10) then begin
         if Compare = 1 then begin
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('tie'));
            ties := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('alos'));
            ALOS := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('cop'));
            COP := GISdb[DBonTable].MyData.FiltRecsInDB;
            Findings.Add(Header + '  (n=' + IntToStr(Opinions) + '),' + RealToString(100.0 * alos/opinions,-8,-2)+ ','  + RealToString(100.0 * cop/opinions,-8,-2));
         end
         else if Compare = 2 then begin
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr('alos'));
            ALOS := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr('cop'));
            COP := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr('fabdem'));
            FAB := GISdb[DBonTable].MyData.FiltRecsInDB;
            Findings.Add(Header + '  (n=' + IntToStr(Opinions) + '),' + RealToString(100.0 * alos/opinions,-8,-2) + ',' + RealToString(100.0 * cop/opinions,-8,-2) + ',' + RealToString(100.0 * fab/opinions,-8,-2));
         end
         else begin
            GISdb[DBonTable].MyData.First;
            for DEM := 0 to 10 do Counts[DEM] := 0;

            while not GISdb[DBonTable].MyData.EOF do begin
               aLine := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
               for DEM := 0 to pred(DEMs.Count) do
                  if StrUtils.AnsiContainsText(aline,DEMs.Strings[DEM]) then inc(Counts[DEM]);
               GISdb[DBonTable].MyData.Next;
            end;
            aline := Header + '  (n=' + IntToStr(Opinions) + ')';
            for DEM := 0 to pred(DEMs.Count) do aline := aline + ',' + RealToString(100.0 * Counts[DEM]/opinions,-8,-2);
            Findings.Add(aLine);
         end;
      end;
   end;

var
   aHeader,aFilter,TStr : shortstring;
   n : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXisCOPorALOSbetter in'); {$EndIf}
   try
      GISdb[DBonTable].ApplyGISFilter('');
      ShowHourglassCursor;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      DEMs := tStringList.Create;
      DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

      for Compare := 1 to 3 do begin

         for i := 1 to 2 do begin
            {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXisCOPorALOSbetter start ' + RefDEMType[i]); {$EndIf}
            ShowHourglassCursor;
            Findings := tStringList.Create;
            if Compare = 1 then Findings.Add('FILTER,ALOS,COP')
            else if Compare = 2 then Findings.Add('FILTER,ALOS,COP,FABDEM')
            else begin
               TStr := 'FILTER';
               for j := 0 to pred(DEMs.Count) do Tstr := Tstr + ',' + DEMs.Strings[j];
               Findings.Add(TStr);
            end;

            RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]);
            for j := 1 to MaxLandType do begin
               DoOne(RefDEMType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
            end;
            Findings.Add('SKIP');

            if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
            for j := 1 to n do begin
               GetFilterAndHeader(i,j,aHeader,aFilter);
               DoOne(aHeader,aFilter);
            end;

            Findings.Add('SKIP');
            for j := 0 to pred(Criteria.Count) do begin
               DoOne(RefDEMType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
            end;
            if Compare = 1 then TStr := '_cop_or_alos_'
            else if Compare = 2 then TStr := '_fab_cop_or_alos_'
            else TStr := '_share_first_';

            fName := NextFileNumber(MDTempDir,RefDEMType[i] + TStr,'.dbf');
            db := StringList2CSVtoDB(Findings,fName);
            if Compare = 1 then TStr := 'COP or ALOS Winning Percentage'
            else if Compare = 2 then TStr := 'COP or ALOS or FABDEM Winning Percentage'
            else TStr := 'DEM share of First Place';
            DEMIXwineContestScoresGraph(DB,Tstr + ' (%)',0,100);
         end;
      end;
   finally
      Criteria.Destroy;
      GISdb[DBonTable].ApplyGISFilter('');
      GISdb[DBonTable].ShowStatus;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXisCOPorALOSbetter out'); {$EndIf}
end;




procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
var
   RefFilter : shortstring;
   CriteriaTable : tMyData;
   BigBMPFiles : tStringList;
   fName,TileSortName : PathStr;
   ch : ANSIchar;


    procedure OneLoop;
    const
       InterAreaSkip = 2;
    var
       NumAreas,NumTiles : integer;
       TileList,AreaList : tStringList;
       i,j,k,CurrentY,DEM,Center,NumTies : integer;
       Criterion,Best,TStr,AreaFilter : shortstring;
       aDEM : array[1..10] of shortstring;
       Graph : tThisBaseGraph;
       rfile : array[1..10] of file;
       v : array[1..2] of float32;
       Symbol : tFullSymbolDeclaration;
       fName : PathStr;
       bmp : tMyBitmap;
       TieTolerance : float32;

                procedure LocateBest(Best : shortstring; Center : float32);
                var
                   DEM : integer;
                begin
                    for DEM := 1 to NumDEMIXDEM do begin
                       if (Best = DEMIXDEMType[DEM]) then begin
                          v[2] := CurrentY;
                          v[1] := Center;
                          BlockWrite(rfile[DEM],v,1);
                       end;
                    end;
                end;


    begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('One loop in, ' + RefFilter); {$EndIf}
       ShowHourglassCursor;
       GISdb[DBonTable].ApplyGISFilter(RefFilter);
       GISdb[DBonTable].EmpSource.Enabled := false;
       NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
       GISdb[DBonTable].EmpSource.Enabled := false;

       AreaList := tStringList.Create;
       if SortByArea then begin
          NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');
          GISdb[DBonTable].EmpSource.Enabled := false;
          fName := DEMIXSettingsDir +  'demix_areas_sorted_by_lat.txt';
          AreaList.LoadFromFile(fName);
       end
       else begin
          AreaList.Add('by sort');
          fName := TileSortName;
          TileList := tStringList.Create;
          TileList.LoadFromFile(fName);
          AreaFilter := '';
          NumAreas := 1;
       end;

       MDDef.DefaultGraphXSize := 1200;
       MDDef.DefaultGraphYSize := NumTiles * 20 + 50;

      Graph := tThisBaseGraph.Create(Application);

      Graph.GraphDraw.LegendList := tStringList.Create;
      for DEM := 1 to NumDEMIXDEM do begin
         Symbol := SymbolFromDEMName(DEMIXDEMType[DEM]);
         Symbol.DrawingSymbol := FilledBox;
         Graph.OpenPointFile(rfile[DEM],Symbol);
         Graph.GraphDraw.LegendList.Add(DEMIXDEMType[DEM]);
      end;

      Graph.GraphDraw.TopLabel := RefFilter  + '  n=' + IntToStr(NumTiles);
      Graph.GraphDraw.HorizLabel := '';
      Graph.Caption := Graph.GraphDraw.TopLabel;
      Graph.GraphDraw.GraphAxes := NoGrid;
      Graph.GraphDraw.MinHorizAxis := 0;
      Graph.GraphDraw.MaxHorizAxis := 20;
      Graph.GraphDraw.MaxVertAxis := NumTiles + InterAreaSkip * NumAreas;
      Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
      Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
      Graph.GraphDraw.TopLeftLabel := ExtractFileNameNoExt(fName);

      CriteriaTable.ApplyFilter('');
      while not CriteriaTable.eof do begin
         TStr := CriteriaTable.GetFieldByNameAsString('CRITERION');
         TieTolerance := CriterionTieTolerance(TStr);
         TStr := IntToStr(CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE')) + ',' + TStr + ' (' + RealToString(TieTolerance,-8,-2) + ')';
         Graph.GraphDraw.GraphBottomLabels.Add(TStr);
         CriteriaTable.next;
      end;

       CurrentY := NumTiles + InterAreaSkip * NumAreas;

       for i := 0 to pred(AreaList.Count) do begin
          {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(AreaList.Strings[i]); {$EndIf}
          WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
          WMDEM.SetPanelText(1,RefFilter);
          if SortByArea then AreaFilter := ' AND AREA=' + QuotedStr(AreaList.Strings[i]);
          GISdb[DBonTable].ApplyGISFilter(RefFilter + AreaFilter);
          TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');

          if SortByArea and (TileList.Count > 0) then begin
             TStr := IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
             Graph.GraphDraw.GraphLeftLabels.Add(TStr);
          end;

          for j := 0 to pred(TileList.Count) do begin
             dec(CurrentY);
             if not SortByArea then begin
                TStr := IntToStr(CurrentY) + ',' + TileList.Strings[j];
                Graph.GraphDraw.GraphLeftLabels.Add(TStr);
                {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile('Left label=' + TStr); {$EndIf}
             end;

             TStr := RefFilter + AreaFilter + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]);
             GISdb[DBonTable].ApplyGISFilter(TStr);
             {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
             GISdb[DBonTable].EmpSource.Enabled := false;
             while not GISdb[DBonTable].MyData.eof do begin
                Criterion := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
                Best := uppercase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC'));
                {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(Criterion + '   ' + Best); {$EndIf}

                CriteriaTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
                if (CriteriaTable.FiltRecsInDB = 1) then begin
                   Center := CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE');
                   ch := 'X';
                   if StrUtils.AnsiContainsText(Best,',') then ch := ',';
                   if StrUtils.AnsiContainsText(Best,';') then ch := ';';
                   if StrUtils.AnsiContainsText(Best,'-') then ch := '-';

                   if (ch in [',',';','-']) then begin //this is a tie for best
                      NumTies := 0;
                      repeat
                         inc(NumTies);
                         aDEM[NumTies] := BeforeSpecifiedCharacterANSI(Best,ch,true,true);
                      until Best = '';
                      for DEM := 1 to NumTies do begin
                         LocateBest(aDEM[DEM],Center-0.3 + DEM * 0.15);
                      end;
                   end
                   else begin //single best
                      LocateBest(Best,Center);
                   end;
                end;
                GISdb[DBonTable].MyData.Next;
             end;
          end;

          if (TileList.Count > 0) then dec(CurrentY,InterAreaSkip);  //add a blank line between areas, but only if this area had tiles
       end;

       for DEM := 1 to NumDEMIXDEM do begin
          CloseFile(rfile[DEM]);
       end;

       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
       Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
       Graph.GraphDraw.BottomMargin := 150;
       Graph.GraphDraw.TopMargin := 50;
       Graph.Height := 1250;
       Graph.GraphDraw.LLlegend := true;
       Graph.GraphDraw.MinVertAxis := CurrentY;
       Graph.RedrawDiagram11Click(Nil);
       fName := NextFileNumber(MDTempDir,'best_dem_graph_','.png');
       SaveImageAsBMP(Graph.Image1,fName);
       BigBMPfiles.Add(fName);
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('One loop out'); {$EndIf}
    end;


var
   i,j : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile in'); {$EndIf}
   try
      SaveBackupDefaults;
      fName := DEMIXSettingsDir + 'demix_criteria_order_with_gaps.dbf';
      CriteriaTable := tMyData.Create(fName);

      if (not SortByArea) then begin
         if (not GetExistingFileName('sorting order for tiles','*.txt',TileSortName)) then exit;
      end;

      for i := 1 to 2 do begin
         BigBMPfiles := tStringList.Create;
         if SortByArea then begin
            for j := 1 to MaxLandType do begin
               RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr(LandType[j]);
               OneLoop;
            end;
         end
         else begin
            RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
            OneLoop;
         end;
         fName := NextFileNumber(MDtempDir,'criteria_by_tile_','.png');
         MakeBigBitmap(BigBMPfiles,'',fName,4);
         DisplayBitmap(fName,'');
      end;
   finally
      CriteriaTable.Destroy;
      GISdb[DBonTable].EmpSource.Enabled := true;
      GISdb[DBonTable].ClearGISFilter;
      WMDEM.SetPanelText(0,'');
      WMDEM.SetPanelText(1,'');
      EndProgress;
      RestoreBackupDefaults;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile out'); {$EndIf}
end;




function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
var
   DEMsPresent : tstringList;
   Symbol : tFullSymbolDeclaration;
   DEM,OnTile : integer;
   rfile : array[1..10] of file;
   v : array[1..2] of float32;


         procedure DoRefType;
         var
            Tile,DEM,SlopeCat : integer;
            TStr : shortstring;
         begin
            GISdb[DBonTable].MyData.First;
            while not GISdb[DBonTable].MyData.eof do begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER');
               inc(OnTile);
               if (TStr = 'SKIP') then begin
                  Result.GraphDraw.GraphLeftLabels.Add(IntToStr(OnTile) + ',------------------------');
               end
               else begin
                  Result.GraphDraw.GraphLeftLabels.Add(IntToStr(OnTile) + ',' + TStr);
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
               end;
               GISdb[DBonTable].MyData.Next;
            end;
         end;

var
   i,x : integer;
   fName : PathStr;
   found : boolean;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, table=' + IntToStr(DBontable)); {$EndIf}

   if GISdb[DBonTable].MyData.FieldExists('FILTER') then begin
      SaveBackupDefaults;
      GISdb[DBonTable].EmpSource.Enabled := false;

      DEMsPresent := tStringList.Create;
      DEMsPresent := GISdb[DBonTable].MyData.FieldsInDataBase;
      MDDef.DefaultGraphXSize := 1200;
      MDDef.DefaultGraphYSize := 1400;

      Result := tThisBaseGraph.Create(Application);
      Result.GraphDraw.LegendList := tStringList.Create;
      Result.GraphDraw.HorizLabel := XScaleLabel;
      Result.Caption := GISdb[DBonTable].DBName;

      for DEM := pred(DEMsPresent.Count) downto 0 do begin
         Found := false;
         for i := 1 to NumDEMIXDEM do begin
            if DEMIXDEMType[i] = UpperCase(DEMsPresent.Strings[DEM]) then begin
               found := true;
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found: ' + DEMsPresent.Strings[DEM]); {$EndIf}
            end;
         end;
         if not Found then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Not found: ' + DEMsPresent.Strings[DEM]); {$EndIf}
            DEMsPresent.Delete(DEM);
         end;
       end;

      for DEM := pred(DEMsPresent.Count) downto 0 do begin
         for i := 1 to NumDEMIXDEM do begin
            if DEMIXDEMType[i] = UpperCase(DEMsPresent.Strings[DEM]) then begin
               Symbol := SymbolFromDEMName(DEMsPresent.Strings[DEM]);
               Result.OpenPointFile(rfile[succ(DEM)],Symbol);
               Result.GraphDraw.LegendList.Add(DEMsPresent.Strings[DEM]);
            end;
         end;
       end;

      Result.GraphDraw.GraphLeftLabels := tStringList.Create;

      OnTile := 0;

      DoRefType;

      for DEM := 1 to DEMsPresent.Count do begin
         CloseFile(rfile[DEM]);
      end;

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram, MaxVertAxis should be ' + IntToStr(OnTile)); {$EndIf}
      Result.GraphDraw.GraphAxes := XPartGridOnly;
      Result.GraphDraw.MinHorizAxis := MinHoriz;
      Result.GraphDraw.MaxHorizAxis := MaxHoriz;
      Result.GraphDraw.MinVertAxis := 0;
      Result.GraphDraw.MaxVertAxis := OnTile + 1;
      Result.GraphDraw.ShowHorizAxis0 := true;
      Result.GraphDraw.LeftMargin := 600;

      Result.AutoScaleAndRedrawDiagram(false,false,false,false);
      Result.Height := 1200;
      Result.RedrawDiagram11Click(Nil);
      fName := NextFileNumber(MDTempDir,'best_dem_graph_','.png');
      SaveImageAsBMP(Result.Image1,fName);

      DEMsPresent.Free;
      GISdb[DBonTable].ApplyGISFilter('');
      GISdb[DBonTable].EmpSource.Enabled := true;
      RestoreBackupDefaults;

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out, MaxVertAxis=' + RealToString(Result.GraphDraw.MaxVertAxis,-12,-2)); {$EndIf}
   end
   else begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, requires BestDEMSbyCategory'); {$EndIf}
      BestDEMSbyCategory(DBonTable);
   end;
end;




procedure MultipleBestByParameters(DBonTable : integer);
//currently set up to pick parameters in the IDE, and then run
const
   NumCriteria = 3;
   NumParameter = 4;
   Criteria : array[1..NumCriteria] of shortstring = ('ELVD_RMSE','SLPD_MAE','RUFD_RMSE');
   Parameter  : array[1..NumParameter] of shortstring = ('RELIEF','AVG_SLOPE','AVG_ROUGH','FOREST_PC');
(*
   NumCriteria = 6;
   NumParameter = 7;
   //Criteria : array[1..9] of shortstring = ('ELVD_AVD','ELVD_RMSE','ELVD_MAE', 'SLPD_AVD','SLPD_RMSE','SLPD_MAE','RUFD_AVD','RUFD_RMSE','RUFD_MAE');
   Criteria : array[1..NumCriteria] of shortstring = ('ELVD_RMSE','ELVD_MAE', 'SLPD_RMSE','SLPD_MAE','RUFD_RMSE','RUFD_MAE');
   Parameter  : array[1..NumParameter] of shortstring = ('RELIEF','AVG_ELEV','AVG_SLOPE','AVG_ROUGH','FOREST_PC','URBAN_PC','BARREN_PC');
*)
var
    Gr : TThisBaseGraph;
    Movie : tStringList;
    fName : PathStr;
    BigBitmap : tMyBitmap;
    DEMtype : shortstring;
    ap,LeftStart : integer;

    procedure MakeGraph(aP : integer; TheCriteria,TheParameter,TheDEM : shortstring);
    var
       Bitmap : tMyBitmap;
       SourceRect, DestRect : TRect;
       UsefulWidth : integer;
    begin
      Gr := BestByParameterSorting(DBonTable,TheParameter,TheCriteria,theDEM);
      CopyImageToBitmap(gr.Image1,Bitmap);
      fName := MDTempDir + 'frame_' + TheParameter + '_' + TheCriteria + '_' + theDEM + '.bmp';
      Bitmap.SaveToFile(fName);
      if aP = 1 then begin
         UseFulWidth := Bitmap.Width - Gr.GraphDraw.LeftMargin;
         CreateBitmap(BigBitmap,Bitmap.Width + pred(NumCriteria * 2) * (UsefulWidth + 10), Bitmap.Height);
         BigBitmap.Canvas.Draw(0,0,Bitmap);
         LeftStart := Bitmap.Width + 10;
      end
      else begin
         SourceRect := Rect(Gr.GraphDraw.LeftMargin,0,Bitmap.Width,Bitmap.Height);
         DestRect := Rect(LeftStart,0,LeftStart + UsefulWidth,Bitmap.Height);
         BigBitmap.Canvas.CopyRect(DestRect,Bitmap.Canvas,SourceRect);
         LeftStart := LeftStart + UsefulWidth + 10;
      end;
      Movie.Add(fName);
      Gr.Destroy;
      Bitmap.Free;
    end;

var
   aCriteria,aParameter,k : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters in, table=' + IntToStr(DBontable)); {$EndIf}
      for aParameter := 1 to NumParameter do begin
         Movie := tStringList.Create;
         ap := 1;
         for aCriteria := 1 to NumCriteria do begin
            for k := 1 to 2 do begin
               MakeGraph(ap,Criteria[aCriteria],Parameter[aParameter],RefDEMType[k]);
               inc(ap);
            end;
         end;
         fName := MDTempDir + Parameter[aParameter] + '_DSM_and_DTM_criteria.bmp';

         MakeBigBitmap(Movie,'DSM (blue) and DTM (tan)',fName,Movie.Count);
         DeleteMultipleFiles(MDTempDir,'frame_*.bmp');

         BigBitmap.Width := LeftStart;
         DisplayBitmap(BigBitmap);
         BigBitmap.Free;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters done graph=' + IntToStr(aParameter)); {$EndIf}
      end;


(*
   for k := 1 to 2 do begin
      DEMtype := RefDEMType[k];
      for aParameter := 1 to 7 do begin
         Movie := tStringList.Create;
         for aCriteria := 1 to 9 do begin
            MakeGraph(aCriteria,aParameter);
         end;
         fName := MDTempDir + Parameter[aParameter] + '_' + DEMtype + '_criteria.bmp';
         MakeBigBitmap(Movie,DEMType,fName,Movie.Count);
      end;
   end;
*)
(*
   for k := 1 to 2 do begin
      DEMtype := RefDEMType[k];
      for aCriteria := 1 to 9 do begin
         Movie := tStringList.Create;
         for aParameter := 1 to 7 do begin
            MakeGraph(aCriteria,aParameter);
         end;
         fName := MDTempDir + Criteria[aCriteria]  + '_' + DEMtype + '_parameters.bmp';
         MakeBigBitmap(Movie,DEMtype,fName,Movie.Count);
      end;
   end;
*)
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;



function BestByParameterSorting(DBonTable : integer; TileParam,Criterion,DEMtype : shortstring) : tThisBaseGraph;
var
   TileValue : float32;
   rfile: file;
   i : integer;
   v : array[1..3] of float32;
   Winners : shortstring;
begin
    GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(DEMType) + ' AND LAND_TYPE=' + QuotedStr('ALL') + ' AND CRITERION=' + QuotedStr(Criterion));
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestByParameterSorting, param=' + TileParam + '  filter=' + GISdb[DBonTable].MyData.Filter); {$EndIf}
    GISdb[DBonTable].EmpSource.Enabled := false;
    Result := tThisBaseGraph.Create(Application);
    Result.GraphDraw.VertLabel := RemoveUnderscores({GISdb[DBonTable].MyData.Filter + '  ' +} TileParam);
    Result.GraphDraw.HorizLabel := Criterion;
    Result.Caption := GISdb[DBonTable].DBName;
    Result.OpenXYColorFile(rfile);

    if DEMtype = 'DSM' then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);

    while not GISdb[DBonTable].MyData.eof do begin
       TileValue := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(TileParam);
       Winners := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
       for i := 1 to NumDEMIXDEM do begin
          if StrUtils.AnsiContainsText(Winners,DEMIXDEMType[i]) then begin
             v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMType[i]));
             v[2] := TileValue;
             v[1] := i;
             BlockWrite(rfile,v,1);
          end;
       end;
       GISdb[DBonTable].MyData.Next;
    end;
    CloseFile(rfile);
    Result.GraphDraw.MinHorizAxis := 0.5;
    Result.GraphDraw.MaxHorizAxis := 6.5;
    Result.GraphDraw.GraphAxes := YFullGridOnly;
    Result.AutoScaleAndRedrawDiagram(true,false);
    Result.Height := 1200;
    Result.Width := 250;
    Result.GraphDraw.MinVertAxis := 0;
    Result.RedrawDiagram11Click(Nil);
end;




procedure WinsAndTies(DBonTable : integer);
const
    nDEMs = 7;
    nT = 6;
    nParams = 4;
    TheDEMs : array[1..NDEMs] of shortstring = ('TIES','ALOS_TIE','COP_TIE','FABDEM_TIE','NASA_TIE','SRTM_TIE','ASTER_TIE');
    TheParams : array[1..NParams] of shortstring = ('*','ELVD','SLPD','RUFD');
var
   Findings : array[1..nDEMs,1..nDEMs] of integer;
   aFilter : shortstring;
   Results : tStringList;
   i,j,k : integer;
begin
   try
      GISdb[DBonTable].EmpSource.Enabled := false;
      for k := 1 to nParams do begin
         for i := 1 to nDEMs do begin
            for j := 2 to nDEMs do begin
               Findings[i,j] := 0;
            end;
         end;
         Results := tStringList.Create;
         aFilter := TheDEMs[1];
         for j := 2 to nDEMs do aFilter :=  aFilter + ',' + TheDEMs[j];
         Results.Add(aFilter);

         for i := 1 to nDEMs do begin
            for j := 1 to nT do begin
               if (k = 1) then aFilter := TheDEMs[i] + '=' + IntToStr(j)
               else aFilter := 'CRIT_CAT=' + QuotedStr(TheParams[k])+ ' AND ' + TheDEMs[i] + '=' + IntToStr(j);
               GISdb[DBonTable].ApplyGISfilter(aFilter);
               Findings[i,j] := GISdb[DBonTable].MyData.FiltRecsInDB;
            end;
         end;

         Results := tStringList.Create;
         aFilter := 'RANK_' + TheParams[k];
         for j := 1 to nDEMs do aFilter := aFilter + ',' + TheDEMs[j];
         Results.Add(aFilter);
         for j := 1 to nT do begin
            aFilter := IntToStr(j);
            for i := 1 to nDEMs do begin
               aFilter := aFilter + ',' + IntToStr(Findings[i,j]);
            end;
            Results.Add(afilter);
         end;
         Results.SaveToFile(MDTempDir + 'wins_and_ties_' + IntToStr(k) + '.csv');
         Results.Free;
      end;
   finally
      GISdb[DBonTable].ClearGISfilter;
      GISdb[DBonTable].ShowStatus;
   end;
end;



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



procedure DoDEMIX_DifferenceMaps(AreaName,ShortName,LongName : shortString; var Graph1,Graph2 : tThisBaseGraph);
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


const
   ALOSHalfSecfName = 'alos.tif';
   CopHalfSecfName = 'cop.tif';

   HalfSecRefDSMfName = 'ref_dsm.tif';

   HalfSecRefDTMfName = 'ref_dtm.tif';
   COP_ALOS_DifffName = 'cop-alos-diff.tif';
   COP_ALOS_DSM4fName = 'cop-alos-dsm4.tif';
   COP_ALOS_DTM4fName = 'cop-alos-dtm4.tif';
   COP_ALOS_DSM9fName = 'cop-alos-dsm9.tif';
   COP_ALOS_DTM9fName = 'cop-alos-dtm9.tif';
   SlopeMapfName = 'slope_ref_dtm.tif';
   RuffMapfName = 'roughness_ref_dtm.tif';
   AspectMapfName = 'aspect_ref_dtm.tif';
   BestCOP_ALOS_slopefName = 'best_slope_ref_dtm.tif';
   BestCOP_ALOS_rufffName = 'best_roughness_ref_dtm.tif';
   BestCOP_ALOS_elevfName = 'best_elev_ref_dtm.tif';

   DTMElevDiffMapALOSfName = 'elev_diff_dtm_alos.tif';
   DTMElevDiffMapCOPfName = 'elev_diff_dtm_cop.tif';
   DTMSlopeDiffMapALOSfName = 'slope_diff_dtm_alos.tif';
   DTMSlopeDiffMapCOPfName = 'slope_diff_dtm_cop.tif';
   DTMRuffDiffMapALOSfName = 'ruff_diff_dtm_alos.tif';
   DTMRuffDiffMapCOPfName = 'ruff_diff_dtm_cop.tif';



procedure HighLowCopAlosGeomorphometry(fName : PathStr = '');
var
   AreaName : shortString;
   SaveDir : PathStr;
   HalfSecRefDTM,SlopeMap,RuffMap,AspectMap,
   DTMElevDiffMapALOS,DTMElevDiffMapCOP : integer;


   procedure DifferenceStats(DEM,RefDEM : integer);
   var
      Diff5,Diff95 : float32;
   begin
       Diff5 := DEMGlb[DEM].FindPercentileElevation(5);
       Diff95 := DEMGlb[DEM].FindPercentileElevation(95);
   end;


begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('HighLowCopAlosGeomorphometry ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      SaveDir := 'H:\aa_half_sec_test\' + AreaName + '\';

      DTMElevDiffMapALOS := OpenNewDEM(SaveDir + 'elev_diff_dtm_alos.tif');
      DTMElevDiffMapCOP := OpenNewDEM(SaveDir + 'elev_diff_dtm_cop.tif');

      HalfSecRefDTM := OpenNewDEM(SaveDir + 'ref_dtm.tif');
      SlopeMap := OpenNewDEM(SaveDir + 'slope_ref_dtm.tif');
      RuffMap := OpenNewDEM(SaveDir + 'roughness_ref_dtm.tif');
      AspectMap := OpenNewDEM(SaveDir + 'aspect_ref_dtm.tif');

      if FileExists(SaveDir + 'slope_ref_dtm.tif') then begin
         SlopeMap := OpenNewDEM(SaveDir + 'slope_ref_dtm.tif');
         RuffMap := OpenNewDEM(SaveDir + 'roughness_ref_dtm.tif');
      end
      else begin
         SlopeMap := -1;   //forces creation of slope and roughness maps
         RuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(HalfSecRefDTM,3,SlopeMap);
         AspectMap := MakeAspectMap(HalfSecRefDTM);
      end;
   end;
end;



var
   UseDSM,UseDTM,chm,
   HalfSecRefDTM,HalfSecRefDSM,HalfSecDTM,HalfSecALOS,HalfSecCOP,
   RefSlopeMap,RefRuffMap,RefAspectMap,
   COP_ALOS_Diff,CopSlope,CopRuff,ALOSslope,ALOSruff,
   BestCOP_ALOS_elev, BestCOP_ALOS_slope,BestCOP_ALOS_ruff,
   COP_ALOS_DSM4,COP_ALOS_DTM4,COP_ALOS_DSM9,COP_ALOS_DTM9,
   DTMElevDiffMapALOS,DTMElevDiffMapCOP,DTMSlopeDiffMapALOS, DTMSlopeDiffMapCOP,DTMRuffDiffMapALOS,DTMRuffDiffMapCOP : integer;
   HalfSec,AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;

procedure ZeroDEMs;
var
   i : integer;
begin
   UseDSM := 0;
   UseDTM := 0;
   HalfSecRefDSM := 0;
   HalfSecRefDTM := 0;
   HalfSecALOS := 0;
   HalfSecCOP := 0;
   RefSlopeMap := 0;
   RefRuffMap := 0;
   COP_ALOS_DSM4 := 0;
   COP_ALOS_DTM4 := 0;
   COP_ALOS_DSM9 := 0;
   COP_ALOS_DTM9 := 0;
   COP_ALOS_Diff := 0;

   CopSlope := 0;
   CopRuff := 0;
   ALOSslope := 0;
   ALOSruff := 0;

   BestCOP_ALOS_elev := 0;
   BestCOP_ALOS_slope := 0;
   BestCOP_ALOS_ruff := 0;
   DTMElevDiffMapALOS := 0;
   DTMElevDiffMapCOP := 0;
   DTMSlopeDiffMapALOS := 0;
   DTMSlopeDiffMapCOP := 0;
   DTMRuffDiffMapALOS := 0;
   DTMRuffDiffMapCOP := 0;

   CHM := 0;
   for I := 1 to 10 do begin
      HalfSec[i] := 0;
      AirOrDirt[i] := 0;
      AirOrDirt2[i] := 0;
      AirOrDirt3[i] := 0;
   end;
end;


procedure HillshadesForWineContest(fName : PathStr = '');
var
   AreaName : shortstring;
   RefDEM : integer;
begin
 (*
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      AreaName := ExtractFileNameNoExt(fName);
   if LoadDEMIXReferenceDEMs(RefDEM) then begin
   if LoadDEMIXCandidateDEMs(RefDEM,true,true)
   then begin

*)
end;



procedure OpenDEMIXRidges(fName : PathStr = '');
const
   CatName : array[1..7] of shortstring = ('Ref','ALOS','ALOS+Ref', 'COP','Cop+Ref','ALOS+COP','ALOS+Cop+Ref');
   CatColor : array[1..7] of tColor = (clBlue,clGreen,clAqua,clLime,clTeal,clPurple,clBrown);
var
   AreaName : shortstring;
   SaveDir : PathStr;
   Fixed,i,x,y,SumDEM : integer;
   z : float32;
   Higher,Lower : array[1..3] of integer;
   Hist : array[1..7] of int64;
   Merge : tDEMbooleans;
   VAT : tStringList;
begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXRidges ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      SaveDir := fName[1] + ':\aa_half_sec_test\' + AreaName + '\';
      ZeroDEMs;
      HalfSecRefDTM := OpenNewDEM(SaveDir + HalfSecRefDTMfName);
      HalfSecALOS := OpenNewDEM(SaveDir + ALOSHalfSecfName);
      HalfSecCOP := OpenNewDEM(SaveDir + CopHalfSecfName);

      NumHighLowNeighborsMaps(HalfSecRefDTM,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify, Higher[1],Lower[1]);
      NumHighLowNeighborsMaps(HalfSecALOS,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify, Higher[2],Lower[2]);
      NumHighLowNeighborsMaps(HalfSecCOP,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify, Higher[3],Lower[3]);
      ShowHourglassCursor;
      for I := 1 to 3 do DEMGlb[Lower[i]].MarkBelowMissing(4.5,Fixed);
      DEMGlb[Lower[1]].ReclassifyRange(1,8,1);
      DEMGlb[Lower[2]].ReclassifyRange(1,8,2);
      DEMGlb[Lower[3]].ReclassifyRange(1,8,4);


      for i := 1 to MaxDEMDataSets do Merge[i] := false;
      for I := 1 to 3 do Merge[Lower[i]] := true;
      ShowHourglassCursor;
      SumDEM := SumDEMs(Lower[1],Merge,AreaName + '_ridges',false,false);
      DEMGlb[SumDEM].MarkBelowMissing(0.5,Fixed);

      for i := 1 to 7 do Hist[i] := 0;
      ShowHourglassCursor;

      for x := 0 to pred(DEMGlb[SumDEM].DEMheader.NumCol) do begin
         for y := 0 to pred(DEMGlb[SumDEM].DEMheader.NumRow) do begin
            if DEMGlb[SumDEM].GetElevMetersOnGrid(x,y,z) then begin
               inc(Hist[round(z)]);
            end;
         end;
      end;

      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');
      for i := 7 downto 1 do if (Hist[i] > 0) then Vat.add(IntToStr(i) + ',' + CatName[i] + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(CatColor[i]));

      fName := MDTempDir + AreaName + '.vat.dbf';
      StringList2CSVtoDB(vat,fName,true);
      DEMGlb[SumDEM].VATFileName := fName;
      DEMglb[SumDEM].CheckMaxMinElev;
      DEMglb[SumDEM].SetUpMap(SumDEM,true,mtDEMVATTable);


   end;
end;


procedure OpenDEMIXArea(fName : PathStr = '');
//these are currently hard wired in the code, but eventually might be options enabled at run time
const
   DEMIX_Movie = false;
   OverwriteFiles = false;
   DEMIXhistograms = false;
   DEMIX_SaveDEMs = true;
   DEMIX_HalfSecondCompareMaps = true;
   DEMIX_GeomorphMapsBestDEM = true;
   DEMIX_MakeDifferenceMaps = true;
   DEMIX_GeomorphMaps = true;
var
   AreaName : shortstring;
   HalfSec,AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;
   i,Cols : integer;
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
               if OverwriteFiles or (not FileExists(fName)) then begin
                  {$IfDef RecordDEMIXsave} writeLineToDebugFile('Save ' + fName + ' elev units=' + ElevUnitsAre(DEMGlb[DEM].DEMheader.ElevUnits)); {$EndIf}
                  DEMGlb[DEM].WriteNewFormatDEM(fName);
               end;
               fName := ChangeFileExt(fName,'.png');
               SaveImageAsBMP(DEMGlb[DEM].SelectionMap.Image1,fName);
            end;
         end;


begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      if DEMIX_SaveDEMs then begin
         SaveDir := fName[1] + ':\aa_half_sec_test\' + AreaName + '\';
         //if PathIsValid(SaveDir) then exit;
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
         MDDef.MapNameLocation.DrawItem := true;
         MDDef.MapNameLocation.MapPosition := lpNEmap;
         MDDef.MapNameLocation.DrawItem := true;
         MDDef.GridLegendLocation.DrawItem := true;

         ZeroDEMs;

         if LoadDEMIXarea(fName) then begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXarea complete'); {$EndIf}
            if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXReferenceDEMs complete; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + 'DEMIXRefDEM=' + IntToStr(DEMIXRefDEM)); {$EndIf}

               if MDDef.DEMIX_DoHalfSecDEMs then begin
                  //HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
                  HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
                  exit;


                  //if FileExists(HalfSecRefDSMfName) then HalfSecRefDSM := OpenNewDEM(HalfSecRefDSMfName) else HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
                  //if FileExists(HalfSecRefDTMfName) then HalfSecRefDTM := OpenNewDEM(HalfSecRefDTMfName) else HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
                  DEMIXRefDEM := HalfSecRefDTM;
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec ref dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               end;

               if DEMIX_GeomorphMaps or DEMIX_GeomorphMapsBestDEM then begin
                  if FileExists(RuffMapfName) then begin
                     RefSlopeMap := OpenNewDEM(SlopeMapfName);
                     RefRuffMap := OpenNewDEM(RuffMapfName);
                  end
                  else begin
                     RefSlopeMap := -1;   //forces creation of slope and roughness maps
                     RefRuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(HalfSecRefDTM,3,RefSlopeMap);
                  end;
                  DEMGlb[RefSlopeMap].AreaName := 'ref_dtm_slope_%';
                  DEMGlb[RefRuffMap].AreaName := 'ref_dtm_roughness_%';
                  if FileExists(AspectMapfName) then HalfSecRefDSM := OpenNewDEM(AspectMapfName) else RefAspectMap := MakeAspectMap(HalfSecRefDTM);
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

               if MDDef.DEMIX_DoHalfSecDEMs or MDDef.DEMIX_DoElevParamGraphs or MDDef.DEMIX_DoAirOrDirt or MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff or DEMIX_GeomorphMapsBestDEM then begin
                  if LoadDEMIXCandidateDEMs(DEMIXRefDEM,true,false) then begin
                     {$IfDef RecordDEMIX} HighlightLineToDebugFile('Candidate DEMs loaded'); {$EndIf}
                     {$IfDef ShowDEMIXWhatsOpen} writeLineToDebugFile('candidates loaded');  writeStringListToDebugFile(GetWhatsOpen); {$EndIf}
                     if MDDef.DEMIX_DoHalfSecDEMs then begin
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Start creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                        for I := 1 to MaxTestDEM do begin
                           if ValidDEM(TestDEM[i]) then begin
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Initial DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                              fName := MDtempDir + DEMGlb[TestDEM[i]].AreaName + '_0.5sec.dem';
                              HalfSec[i] := DEMGlb[TestDEM[i]].ReinterpolateLatLongDEM(0.5,fName);
                              //CloseSingleDEM(TestDEM[i]);
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
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Done creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

                        if DEMIX_GeomorphMapsBestDEM then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM begin'); {$EndIf}
                           ALOSslope := -1;   //forces creation of slope and roughness maps
                           ALOSruff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEM[1],3,ALOSslope);
                           DEMGlb[ALOSSlope].AreaName := 'alos_slope_%';
                           DEMGlb[ALOSRuff].AreaName := 'alos_roughness_%';

                           COPslope := -1;   //forces creation of slope and roughness maps
                           COPruff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEM[2],3,copSlope);
                           DEMGlb[COPSlope].AreaName := 'cop_slope_%';
                           DEMGlb[COPRuff].AreaName := 'cop_roughness_%';

                           BestCOP_ALOS_elev := BestCopOrALOSmap(HalfSecRefDTM,TestDEM[1],TestDEM[2],1.5,'best_dem_elevation');
                           BestCOP_ALOS_slope := BestCopOrALOSmap(RefSlopeMap,ALOSslope,COPslope,1.5,'best_dem_slope');
                           BestCOP_ALOS_ruff := BestCopOrALOSmap(RefRuffMap,ALOSruff,COPruff,1.5,'best_dem_roughness');

                           if DEMIX_MakeDifferenceMaps then begin
                              DTMElevDiffMapALOS := MakeDifferenceMap(HalfSecRefDTM,TestDEM[1],true,false,false);
                              DTMElevDiffMapCOP := MakeDifferenceMap(HalfSecRefDTM,TestDEM[2],true,false,false);
                              DTMSlopeDiffMapALOS := MakeDifferenceMap(RefSlopeMap,ALOSslope,true,false,false);
                              DTMSlopeDiffMapCOP := MakeDifferenceMap(RefSlopeMap,COPSlope,true,false,false);
                              DTMRuffDiffMapALOS := MakeDifferenceMap(RefRuffMap,ALOSRuff,true,false,false);
                              DTMRuffDiffMapCOP := MakeDifferenceMap(RefRuffMap,COPRuff,true,false,false);
                           end;

                           {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM done'); {$EndIf}
                        end;

                        if DEMIX_HalfSecondCompareMaps then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec candidates done, Make difference map'); {$EndIf}
                           COP_ALOS_Diff := MakeDifferenceMap(TestDEM[1],TestDEM[2],true,false,false,'COP-ALOS_difference');

                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DSM COP-ALOS'); {$EndIf}
                           COP_ALOS_DSM4 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],'DSM',MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DSM-4');
                           COP_ALOS_DSM9 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],'DSM',MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DSM-9');

                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DTM COP-ALOS'); {$EndIf}
                           COP_ALOS_DTM4 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],'DTM',MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DTM-4');
                           COP_ALOS_DTM9 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],'DTM',MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DTM-9');

                           if DEMIXhistograms then begin
                              HistogramsFromVATDEM(COP_ALOS_DSM4,HalfSecRefDSM,RefSlopeMap,RefRuffMap,RefAspectMap);
                              HistogramsFromVATDEM(COP_ALOS_DTM4,HalfSecRefDTM,RefSlopeMap,RefRuffMap,RefAspectMap);
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

                     if MDDef.DEMIX_DoElevParamGraphs then ElevationSlopePlot(DEMListForAllOpenDEM);

                     if MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff then begin
                        if MDDef.DEMIXCompositeImage then BigMap := tStringList.Create;    //actually this will be graphs, not maps
                        if MDDef.DEMIX_DoElevDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'elvd','Elevation',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIX_DoSlopeDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'slpd','Slope',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIX_DoRuffDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'rufd','Roughness',Graph1,Graph2);
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

            AddFrame(RefSlopeMap,'Ref slope');
            AddFrame(RefRuffMap,'Ref ruff');
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
            SaveDEM(TestDEM[1],SaveDir + ALOSHalfSecfName);
            SaveDEM(TestDEM[2],SaveDir + CopHalfSecfName);

            SaveDEM(HalfSecRefDSM,SaveDir + HalfSecRefDSMfName);
            SaveDEM(HalfSecRefDTM,SaveDir + HalfSecRefDTMfName);

            if DEMIX_HalfSecondCompareMaps then begin
               SaveDEM(COP_ALOS_Diff,SaveDir + COP_ALOS_DifffName);
               SaveDEM(COP_ALOS_DSM4,SaveDir + COP_ALOS_DSM4fName);
               SaveDEM(COP_ALOS_DTM4,SaveDir + COP_ALOS_DTM4fName);
               SaveDEM(COP_ALOS_DSM9,SaveDir + COP_ALOS_DSM9fName);
               SaveDEM(COP_ALOS_DTM9,SaveDir + COP_ALOS_DTM9fName);
            end;

            if DEMIX_GeomorphMaps then begin
               SaveDEM(RefSlopeMap,SaveDir + SlopeMapfName);
               SaveDEM(RefRuffMap,SaveDir + RuffMapfName);
               SaveDEM(RefAspectMap,SaveDir + AspectMapfName);
            end;

            if DEMIX_GeomorphMapsBestDEM then begin
               SaveDEM(BestCOP_ALOS_slope,SaveDir + BestCOP_ALOS_slopefName);
               SaveDEM(BestCOP_ALOS_ruff,SaveDir + BestCOP_ALOS_rufffName);
               SaveDEM(BestCOP_ALOS_elev,SaveDir + BestCOP_ALOS_elevfName);

               if DEMIX_MakeDifferenceMaps then begin
                  SaveDEM(DTMElevDiffMapALOS,SaveDir + DTMElevDiffMapALOSfName);
                  SaveDEM(DTMElevDiffMapCOP,SaveDir + DTMElevDiffMapCOPfName);
                  SaveDEM(DTMSlopeDiffMapALOS,SaveDir + DTMSlopeDiffMapALOSfName);
                  SaveDEM(DTMSlopeDiffMapCOP,SaveDir +  DTMSlopeDiffMapCOPfName );
                  SaveDEM(DTMRuffDiffMapALOS,SaveDir + DTMRuffDiffMapALOSfName);
                  SaveDEM(DTMRuffDiffMapCOP,SaveDir + DTMRuffDiffMapCOPfName);
               end;
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
   db := OpenMultipleDataBases('DEMIX area rules',cfName, false);
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



procedure BestDEMSbyCategory(DBonTable : integer);
var
   DEMs,Criteria,Besties,Findings,Findings2,AverageScore,FiltersTooFewOpinions : tStringList;
   i : integer;
   theSum,MaxSum : float64;
   theSums : array[0..10] of float64;
   fName : PathStr;
   aLine : shortstring;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,DEM,Ties,ThisCat,Opinions : integer;
   begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DO-ONE  ' + theFilter); {$EndIf}
      WMDEM.SetPanelText(1,theFilter);
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (Opinions >= 10) then begin
         Ties := Opinions;
         Total := Opinions;
         aline := Header + '  (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMs.Strings[DEM]) );
            ThisCat := GISdb[DBonTable].MyData.FiltRecsInDB;
            aLine := aLine + ',' + IntToStr(ThisCat);
            Ties := Ties - ThisCat;
         end;
         Besties.Add(aLine + ',' + IntToStr(Ties) );

         GISdb[DBonTable].ApplyGISFilter(theFilter);
         MaxSum := 0;
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            theSums[DEM] := GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            if theSums[DEM] > maxSum then MaxSum := theSums[DEM];
            Findings.Add(Header + ',' + DEMs.Strings[DEM] + ',' + RealToString(theSums[DEM],12,-4));
         end;

         aline := Header + ' (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' + RealToString(theSums[DEM] / MaxSum,12,-4);
         end;
         Findings2.Add(aLine);

         aline := Header + '  (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' +  RealToString(theSums[DEM] / Opinions,12,-4);
         end;
         AverageScore.Add(aLine);
      end
      else begin
         FiltersTooFewOpinions.Add(theFilter + ',' + IntToStr(Opinions));
      end;
   end;


var
   Header,aFilter,RefFilter : ShortString;
   j,n,ScoresDB : integer;
   Graph : tThisBaseGraph;
   bmp : tMyBitmap;
   LegendfName : PathStr;
   BigBitmap : tStringList;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory in, ' + GISdb[DBonTable].dbName); {$EndIf}
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(DEMIXSettingsDir + 'demix_criteria.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
   ShowHourglassCursor;
   if not GISdb[DBonTable].MyData.FieldExists(DEMs.Strings[0] + '_SCR') then begin
      RankDEMS(DBonTable);
   end;

   FiltersTooFewOpinions := tStringList.Create;
   FiltersTooFewOpinions.Add('FILTER,OPINIONS');

   Besties := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Besties.Add(aLine + ',TIES');

   Findings2 := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Findings2.Add(aLine);

   Findings := tStringList.Create;
   Findings.Add('FILTER,DEM,SCORE_SUM');

   for i := 1 to 2 do begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory DEM=' + RefDEMType[i] ); {$EndIf}
      AverageScore := tStringList.Create;
      aLine := 'FILTER,OPINIONS';
      for j := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[j];
      AverageScore.Add(aLine);
      BigBitmap := tStringList.Create;
      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]);
      for j := 1 to MaxLandType do begin
         DoOne(RefDEMType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
      end;
      AverageScore.Add('SKIP');

      if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
      for J := 1 to n do begin
         GetFilterAndHeader(i,j, Header,aFilter);
         DoOne(Header,aFilter);
      end;
      AverageScore.Add('SKIP');

      for j := 0 to pred(Criteria.Count) do begin
         DoOne(RefDEMType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
      end;
      //if i=1 then AverageScore.Add('SKIP');

      fName := NextFileNumber(MDTempDir,RefDEMType[i] + '_average_scores_','.dbf');
      ScoresDB := StringList2CSVtoDB(AverageScore,fName);
      Graph := DEMIXwineContestScoresGraph(ScoresDB,'Average score',0.5, 6.25);
      LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
      bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
      SaveBitmap(bmp,LegendfName);
      Bmp.free;
      fName := NextFileNumber(MDTempDir,'best_graph_','.png');
      SaveImageAsBMP(Graph.Image1,fName);
      BigBitmap.Add(fName);
      BigBitmap.Add(LegendfName);
      fName := NextFileNumber(MDTempDir,'best_graph_with_legend_','.png');
      MakeBigBitmap(BigBitmap,'',fName,2);
      DisplayBitmap(fName,'');

      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].ShowStatus;
      GISdb[DBonTable].EmpSource.Enabled := true;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Done BestDEMSbyCategory DEM=' + RefDEMType[i] ); {$EndIf}
   end;

      //unclear if these are really needed
         fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'criteria_best_count.dbf';
         StringList2CSVtoDB(Besties,fName);

         fName := NextFileNumber(MDTempDir, 'multi_line_scores_','.dbf');
         StringList2CSVtoDB(Findings,fName);

         fName := NextFileNumber(MDTempDir,'single_line_scores_','.dbf');
         StringList2CSVtoDB(Findings2,fName);

         fName := NextFileNumber(MDTempDir,'filters_too_few_opinions_','.dbf');
         StringList2CSVtoDB(FiltersTooFewOpinions,fName);

   DEMs.Destroy;
   Criteria.Destroy;
   ShowDefaultCursor;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;



end.
