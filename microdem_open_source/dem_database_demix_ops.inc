{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}

//include file for demdatbase


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
   RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
   case j of
      1 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile avg slope > 18%';
            aFilter := 'AVG_SLOPE > 18' + RefFilter;
          end;
      2 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile avg slope < 18%';
            aFilter := 'AVG_SLOPE < 18' + RefFilter;
          end;
      3 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile relief < 25m';
            aFilter := 'AVG_SLOPE > 18' + RefFilter;
          end;
      4 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile relief > 500m';
            aFilter := 'RELIEF > 500' + RefFilter;
          end;
      5 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile avg rough > 10%';
            aFilter := 'AVG_ROUGH > 10' + RefFilter;
          end;
      6 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile avg rough < 5%';
            aFilter := 'AVG_ROUGH < 5' + RefFilter;
          end;
      7 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile urban > 25%';
            aFilter := 'URBAN_PC > 25' + RefFilter;
          end;
      8 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile forest > 50%';
            aFilter := 'FOREST_PC > 50' + RefFilter;
          end;
      9 : begin
            aHeader := RefType[i] + ' ' + 'ALL pixels  Tile barren > 25%';
            aFilter := 'BARREN_PC > 25' + RefFilter;
          end;
   end;
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
            if DEMType[i] = UpperCase(DEMsPresent.Strings[DEM]) then begin
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
            if DEMType[i] = UpperCase(DEMsPresent.Strings[DEM]) then begin
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
                       if (Best = DEMType[DEM]) then begin
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
          TileList := tStringList.Create;
          fName := TileSortName;
          TileList.LoadFromFile(fName);
          AreaFilter := '';
          NumAreas := 1;
       end;

       MDDef.DefaultGraphXSize := 1200;
       MDDef.DefaultGraphYSize := NumTiles * 20 + 50;

      Graph := tThisBaseGraph.Create(Application);

      Graph.GraphDraw.LegendList := tStringList.Create;
      for DEM := 1 to NumDEMIXDEM do begin
         Symbol := SymbolFromDEMName(DEMType[DEM]);
         Symbol.DrawingSymbol := FilledBox;
         Graph.OpenPointFile(rfile[DEM],Symbol);
         Graph.GraphDraw.LegendList.Add(DEMType[DEM]);
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
                if CriteriaTable.FiltRecsInDB = 1 then begin
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
               RefFilter := 'REF_TYPE=' + QuotedStr(RefType[i]) + ' AND LAND_TYPE=' + QuotedStr(LandType[j]);
               OneLoop;
            end;
         end
         else begin
            RefFilter := 'REF_TYPE=' + QuotedStr(RefType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
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
            {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXisCOPorALOSbetter start ' + RefType[i]); {$EndIf}
            ShowHourglassCursor;
            Findings := tStringList.Create;
            if Compare = 1 then Findings.Add('FILTER,ALOS,COP')
            else if Compare = 2 then Findings.Add('FILTER,ALOS,COP,FABDEM')
            else begin
               TStr := 'FILTER';
               for j := 0 to pred(DEMs.Count) do Tstr := Tstr + ',' + DEMs.Strings[j];
               Findings.Add(TStr);
            end;

            RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]);
            for j := 1 to MaxLandType do begin
               DoOne(RefType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
            end;
            Findings.Add('SKIP');

            if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
            for j := 1 to n do begin
               GetFilterAndHeader(i,j,aHeader,aFilter);
               DoOne(aHeader,aFilter);
            end;

            Findings.Add('SKIP');
            for j := 0 to pred(Criteria.Count) do begin
               DoOne(RefType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
            end;
            if Compare = 1 then TStr := '_cop_or_alos_'
            else if Compare = 2 then TStr := '_fab_cop_or_alos_'
            else TStr := '_share_first_';

            fName := NextFileNumber(MDTempDir,RefType[i] + TStr,'.dbf');
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



procedure FilterOutSignedCriteria(DBonTable : integer);
const
   MeanParams : array[1..3] of shortstring = ('ELVD_MEAN','SLPD_MEAN','RUFD_MEAN');
   MedianParams : array[1..3] of shortstring = ('ELVD_MED','SLPD_MED','RUFD_MED');
var
   aFilter : ANSIstring;
   i : integer;
begin
    aFilter := '';
    for I := 1 to 3 do begin
       aFilter := AddAndIfNeeded(aFilter) + 'CRITERION<>' + QuotedStr(MeanParams[i]);
       aFilter := AddAndIfNeeded(aFilter) + 'CRITERION<>' + QuotedStr(MedianParams[i]);
    end;
    GISdb[DBOntable].ApplyGISFilter(aFilter);
    GISdb[DBonTable].ShowStatus;
end;


procedure RankDEMS(DBonTable : integer);
const
   IttyBitty = 0.00001;  //added for floating point funny business between Delphi and Python
var
   DEMsRanked,Scores : tStringList;
   DEM,Value,N,i,j,Score,BestScore : integer;
   Sum,fValue,BestValue,TieTolerance,Cop,alos : float32;
   fName : PathStr;
   BestValueString,aLine,Which : shortstring;
   Values : array[0..10] of float32;
   RankDEMs : array[0..10] of shortstring;
   Results,Scoring : tStringList;
   TieToleranceTable : tMyData;
   GridForm : TGridForm;

       procedure FindTies(StartDEM : integer; var EndDEM : integer);
       var
          i,N : integer;
       begin
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties in, DEM=' + IntToStr(StartDEM)); {$EndIf}
          BestValueString := RankDEMs[StartDEM];
          EndDEM := succ(StartDEM);
          BestScore := succ(StartDEM);
          N := 1;
          while (Values[EndDEM] < (Values[StartDEM] + TieTolerance + IttyBitty)) and (EndDEM < DEMsRanked.Count) do begin
             BestValueString := BestValueString + ',' + RankDEMs[EndDEM];
             inc(EndDEM);
             inc(N);
             BestScore := BestScore + EndDEM;
          end;
          if (StartDEM = 0) then GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',BestValueString);
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties =' + BestValueString); {$EndIf}

          for i := StartDEM to pred(EndDEM) do begin
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(RankDEMs[i] + '_SCR',BestScore/N);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(RankDEMs[i] + RealToString(BestScore/N,64,-2) + RealToString(Values[i],8,2)); {$EndIf}
          end;
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties out, EndDEM=' + IntToStr(EndDEM)); {$EndIf}
       end;

var
  FirstDEM : integer;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    if not GetFileFromDirectory('DEMIX criterial tolerances','*.dbf',MDDef.DEMIX_criterion_tolerance_fName) then exit;
    if GISdb[DBonTable].MyData.GetFieldLength('DEM_LOW_SC') < 48 then GISdb[DBonTable].MyData.TrimField('DEM_LOW_SC',48);
    DEMsRanked := tStringList.Create;
    DEMsRanked.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
    i := 0;
    if GetFromListZeroBased('DEMs to use in ranking',i,DEMsRanked,true,true) then begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load criterion tolerances, ' + MDDef.DEMIX_criterion_tolerance_fName); {$EndIf}
       TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);

       GridForm := TGridForm.Create(Application);
       StartProgress('Rank DEMs');
       j := 0;
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.eof do begin
          {$If Defined(RecordDEMIXties)} HighLightLineToDebugFile('Loop =' + IntToStr(j)); {$EndIf}
          inc(j);
          UpdateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;

          Scoring := tStringList.Create;
          Scoring.Add('DEM,VALUE,SCORE');
          for DEM := 0 to pred(DEMsRanked.Count) do begin
             aLine := DEMsRanked.Strings[DEM] + ',';
             aLine := aLine + RealToString(abs(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsRanked.Strings[DEM])),-12,-2) + ',';
             aLine := aLine + RealToString(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsRanked.Strings[DEM] + '_SCR'),-12,-2);
             Scoring.Add(aline);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(aLine); {$EndIf}
          end;
          fName := MDTempDir + 'ranking.csv';
          Scoring.SaveToFile(fName);
          GridForm.ReadCSVFile(fName);
          Scoring.Destroy;
          SortGrid(GridForm.StringGrid1,0,2,true);

          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Now sorted'); {$EndIf}
          for DEM := 0 to pred(DEMsRanked.Count) do begin
             RankDEMs[DEM] := GridForm.StringGrid1.Cells[0,succ(DEM)];
             Values[DEM] := StrToFloat(GridForm.StringGrid1.Cells[1,succ(DEM)]);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(RankDEMs[DEM] + RealToString(Values[DEM],8,-2)); {$EndIf}
          end;

          TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
          if TieToleranceTable.FiltRecsInDB = 0 then TieTolerance := 0.25
          else TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');

          cop := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('COP');
          alos := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('ALOS');
          if (abs(cop-alos) <= (TieTolerance + IttyBitty)) then which := 'tie'
          else if (cop < alos) then which := 'cop'
          else which := 'alos';

          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetFieldByNameAsFloat('TOLERANCE',TieTolerance);
          GISdb[DBonTable].MyData.SetFieldByNameAsString('COP_ALOS',which);

          FirstDEM := 0;
          repeat
             FindTies(FirstDEM,DEM);
             FirstDEM := DEM;
          until DEM = DEMsRanked.Count;
          GISdb[DBonTable].MyData.Next;
       end;
       GridForm.Destroy;
    end;
    DEMsRanked.Destroy;
    GISdb[DBonTable].ShowStatus;
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;


procedure SumsOfRankDEMS(DBonTable : integer);
var
   DEMs : tStringList;
   DEM  : integer;
   Findings : tStringList;
   fName : PathStr;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs in, ' + GISdb[DBonTable].dbName); {$EndIf}
    StopSplashing;
    DEMs := tStringList.Create;
    DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
    Findings := tStringList.Create;
    Findings.Add('DEM,SCORE_SUM');
    for DEM := 0 to pred(DEMs.Count) do begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
       Findings.Add(DEMs.Strings[DEM] + ',' + RealToString(GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR'),12,-2));
    end;
    DEMs.Destroy;
    GISdb[DBonTable].ShowStatus;
    fName := NextFileNumber(MDTempDir,'scores_','.dbf');
    StringList2CSVtoDB(Findings,fName);
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs out, ' + GISdb[DBonTable].dbName); {$EndIf}
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
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory DEM=' + RefType[i] ); {$EndIf}
      AverageScore := tStringList.Create;
      aLine := 'FILTER,OPINIONS';
      for j := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[j];
      AverageScore.Add(aLine);
      BigBitmap := tStringList.Create;
      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]);
      for j := 1 to MaxLandType do begin
         DoOne(RefType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
      end;
      AverageScore.Add('SKIP');

      if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
      for J := 1 to n do begin
         GetFilterAndHeader(i,j, Header,aFilter);
         DoOne(Header,aFilter);
      end;
      AverageScore.Add('SKIP');

      for j := 0 to pred(Criteria.Count) do begin
         DoOne(RefType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
      end;
      //if i=1 then AverageScore.Add('SKIP');

      fName := NextFileNumber(MDTempDir,RefType[i] + '_average_scores_','.dbf');
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
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Done BestDEMSbyCategory DEM=' + RefType[i] ); {$EndIf}
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




procedure TransposeDEMIXwinecontestGraph(DBonTable : integer);
var
   BaseFilter,BaseFilterAnd : shortstring;

      procedure OneField;
      var
         Graph : tThisBaseGraph;
         Symbol : tFullSymbolDeclaration;
         DEM,OnTile : integer;
         rfile : array[1..10] of file;
         v : array[1..2] of float32;


         procedure DoRefType(aFilterElement : shortstring; Title : shortString);
         var
            DEM : integer;
         begin
            GISdb[DBonTable].MyData.First;
            GISdb[DBonTable].ApplyGISFilter(BaseFilterAnd + aFilterElement);
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add('');
               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  if GISdb[DBonTable].MyData.FieldExists('DEMIX_TILE') then begin
                      Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE') + '  ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('LAND_TYPE'));
                  end
                  else if GISdb[DBonTable].MyData.FieldExists('FILTER') then begin
                      Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER'));
                  end;
                  inc(OnTile);

                  for DEM := 1 to NumDEMIXDEM do begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Filter= ' + GISdb[DBonTable].MyData.Filter + '  recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}

                     if (GISdb[DBonTable].MyData.GetFieldByNameAsString(DEMType[DEM]) <> '') then begin
                        v[2] := Ontile;
                        v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMType[DEM]);
                        if v[1] > Graph.GraphDraw.MaxHorizAxis then Graph.GraphDraw.MaxHorizAxis := v[1] + 1;
                        if v[1] < Graph.GraphDraw.MinHorizAxis then Graph.GraphDraw.MinHorizAxis := v[1] - 1;
                        BlockWrite(rfile[DEM],v,1);
                     end;
                  end;
                  GISdb[DBonTable].MyData.Next;
               end;
               if (Title <> '') then begin
                  inc(OnTile);
                  Graph.GraphDraw.GraphLeftLabels.Add(Title);
               end;
            end;
         end;

      var
         RefTypes : tStringList;
         i : integer;
      begin
         //{$If Defined(RecordDEMIX)} WriteLineToDebugFile('OneField in, ' + aField); {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         Graph := tThisBaseGraph.Create(Application);
         if GISdb[DBonTable].MyData.FieldExists('CRITERION') then Graph.GraphDraw.HorizLabel := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
         Graph.Caption := GISdb[DBonTable].DBName;

         Graph.GraphDraw.LegendList := tStringList.Create;
         for DEM := 1 to NumDEMIXDEM do begin
            Symbol := SymbolFromDEMName(DEMType[DEM]);
            Graph.OpenPointFile(rfile[DEM],Symbol);
            Graph.GraphDraw.LegendList.Add(DEMType[DEM]);
         end;

         Graph.GraphDraw.GraphAxes := XPartGridOnly;
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MaxHorizAxis := 0;
         Graph.GraphDraw.GraphLeftLabels := tStringList.Create;

         OnTile := 0;

         RefTypes := GISdb[DBonTable].MyData.UniqueEntriesInDB('REF_TYPE');
         for i := 0 to RefTypes.Count  do begin
            DoRefType('REF_TYPE=' + QuotedStr(RefTypes.Strings[i]),RefTypes.Strings[i]);
         end;
         RefTypes.Free;
         for DEM := 1 to NumDEMIXDEM  do begin
            CloseFile(rfile[DEM]);
         end;

         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
         Graph.GraphDraw.MinVertAxis := 0;
         Graph.GraphDraw.MaxVertAxis := OnTile;
         Graph.GraphDraw.ShowHorizAxis0 := true;
         Graph.AutoScaleAndRedrawDiagram(false,false);
         Graph.GraphDraw.LeftMargin := 225;
         Graph.Height := 130 + 26 * OnTile;
         Graph.RedrawDiagram11Click(Nil);

         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
         GISdb[DBonTable].ApplyGISFilter(BaseFilter);
      end;

begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXwinecontestGraph in'); {$EndIf}
   BaseFilter := GISdb[DBonTable].MyData.Filter;
   BaseFilterAnd := '';
   if (BaseFilter <> '') then BaseFilterAnd := BaseFilter + ' AND ';

   OneField;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXwinecontestGraph out'); {$EndIf}
end;



procedure DEMIXwineContestMeanMedianGraph(What,DBonTable : integer);
const
   InterAreaSkip = 4;
var
   AreaList,AllTiles,AllCriteria,DEMsPresent,SlopesPresent,BigGraph : tstringList;
   PickParam,ThisTile : shortstring;
   WhatNow : integer;
   LegendfName,fName : PathStr;


         procedure MakeGraphForFilter(Param,LandType,DEMType : shortstring);
         var
            Graph : tThisBaseGraph;
            Symbol : tFullSymbolDeclaration;
            DEM,CurrentY,DEMis,i,j,k,NumAreas,NumTiles : integer;
            rfile : array[1..10] of file;
            TileList : tStringList;
            v : array[1..2] of float32;
            Max : float32;
            aMinVal,aMaxVal : float64;
            RefFilter,aFilter : shortstring;
            bmp : tMyBitmap;


               procedure OnePercentage(aFilter : shortstring);
               var
                  Total,i : integer;
               begin
                  GISdb[DBonTable].ApplyGISFilter(aFilter);
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  Total := GISdb[DBonTable].MyData.FiltRecsInDB;
                  dec(CurrentY);
                  Graph.GraphDraw.GraphLeftLabels.Add(aFilter);
                  for i := 0 to pred(DEMsPresent.Count) do begin
                     GISdb[DBonTable].ApplyGISFilter(aFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMsPresent.Strings[i]));
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     v[2] := CurrentY;
                     v[1] := 100 * GISdb[DBonTable].MyData.FiltRecsInDB / Total;
                     BlockWrite(rfile[succ(i)],v,1);
                  end;
               end;


         begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphForFilter in, ' + Param + ' ' + Landtype + ' ' + DEMtype); {$EndIf}
            WMDEM.SetPanelText(1,Param + ' ' + Landtype + ' ' + DEMtype);
            if (What in [dgAllValues,dgAllScores,dgPercentBest]) then begin
            end
            else if (What in [dgArea]) then begin
               RefFilter := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end
            else if (What in [dgPick,dg7Params,dgNormalizedDiff ]) then begin
               RefFilter := 'CRITERION=' + QuotedStr(Param)  + ' AND LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end;

            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(RefFilter);
            if (GISdb[DBonTable].MyData.FiltRecsInDB = 0) then begin //primarily needed if there are no DSM opinions
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No Recs for ' + GISdb[DBonTable].MyData.Filter); {$EndIf}
               exit;
            end;

            GISdb[DBonTable].EmpSource.Enabled := false;
            NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
            GISdb[DBonTable].EmpSource.Enabled := false;
            NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');

            CurrentY := NumTiles + (InterAreaSkip * NumAreas);

            Graph := tThisBaseGraph.Create(Application);
            Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
            Graph.GraphDraw.LegendList := tStringList.Create;
            //Graph.GraphDraw.HorizLabel := Filter2;
            Graph.GraphDraw.VertLabel := '';
            Graph.Caption := GISdb[DBonTable].DBName;
            Graph.GraphDraw.MaxHorizAxis := -99999;
            Graph.GraphDraw.MinHorizAxis := 99999;
            Graph.GraphDraw.MinVertAxis := -1;
            Graph.GraphDraw.MaxVertAxis := CurrentY;
            Graph.GraphDraw.ShowHorizAxis0 := true;

            GISdb[DBonTable].EmpSource.Enabled := false;
            for DEM := 1 to DEMsPresent.Count do begin
               GISdb[DBonTable].MyData.FindFieldRange(DEMsPresent.Strings[pred(DEM)],aMinVal,aMaxVal);
               if (aminVal < Graph.GraphDraw.MinHorizAxis) then Graph.GraphDraw.MinHorizAxis := aMinVal;
               if (amaxval > Graph.GraphDraw.MaxHorizAxis) then Graph.GraphDraw.MaxHorizAxis := aMaxVal;
            end;
            Graph.GraphDraw.GraphAxes := XPartGridOnly;

            for DEM := 1 to DEMsPresent.Count do begin
               Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
               Graph.OpenPointFile(rfile[DEM],Symbol);
               Graph.GraphDraw.LegendList.Add(DEMsPresent.Strings[Pred(DEM)]);
            end;

            for i := 0 to pred(AreaList.Count) do begin
             GISdb[DBonTable].EmpSource.Enabled := false;
                GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND AREA=' + QuotedStr(AreaList.Strings[i]));
                WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
                WMDEM.SetPanelText(1,RefFilter);
                if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
                   Graph.GraphDraw.GraphLeftLabels.Add(IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA'));
                   for j := 0 to pred(TileList.Count) do begin
                      dec(CurrentY);
                      GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND AREA=' + QuotedStr(AreaList.Strings[i]) + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]));
                      GISdb[DBonTable].EmpSource.Enabled := false;

                     if What in [dgArea] then begin
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.FieldSum(DEMsPresent.Strings[DEM] + '_SCR') / GISdb[DBonTable].MyData.FiltRecsInDB;
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                        GISdb[DBonTable].MyData.Next;
                     end
                     else if What in [dgAllValues,dgAllScores] then begin
                        (*
                        Graph.GraphDraw.VertLabel := '';
                        GISdb[DBonTable].MyData.First;
                        while not GISdb[DBonTable].MyData.eof do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           inc(OnTile);
                           Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION') + '  ' + LandType + '  ' + DEMType);
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[2] := Ontile;
                              if What in [dgAllValues] then v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM])
                              else v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM] + '_SCR');
                              BlockWrite(rfile[succ(DEM)],v,1);
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;
                        *)
                     end
                     else if What in [dgPercentBest] then begin
                        (*
                        for i := 1 to 2 do OnePercentage('REF_TYPE=' + QuotedStr(RefType[i]));
                        inc(OnTile);
                        Graph.GraphDraw.GraphLeftLabels.Add('-----------');

                        for i := 1 to MaxLandType do OnePercentage('LAND_TYPE=' + QuotedStr(LandType[i]));
                        inc(OnTile);
                        Graph.GraphDraw.GraphLeftLabels.Add('-----------');
                        for i := 1 to AllCriteria.Count do OnePercentage('CRITERION=' + QuotedStr(AllCriteria.Strings[pred(i)]));
                        *)
                     end
                     else if (What in [dg7Params,dgPick,dgNormalizedDiff]) then begin
                        if  (What in [dgNormalizedDiff]) then begin
                           Max := 0;
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                              if v[1] > Max then Max := v[1];
                           end;
                        end
                        else Max := 1;
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]) / Max;
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                     end
                     else begin
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                      end;
                   end;
                   Dec(currentY,InterAreaSkip);
                end;
             end;

            for DEM := 1 to DEMsPresent.Count do begin
               CloseFile(rfile[DEM]);
            end;

            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
            Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
            Graph.Width := 1200;
            Graph.Height := 2400;
            Graph.GraphDraw.BottomMargin := 75;

            if What in [dgArea] then begin
               Graph.GraphDraw.HorizLabel := 'Average Ranking compared to reference ' + DEMType;
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;

            if (What in [dgPercentBest]) then begin
               Graph.GraphDraw.HorizLabel := 'Percentage of Criteria Where DEM is Best';
               Graph.GraphDraw.MinHorizAxis := 0;
               Graph.GraphDraw.MaxHorizAxis := 100;
            end;

            if (What in [dgAllValues]) then begin
               if WhatNow in [dgAllValues] then begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Difference';
                  Graph.GraphDraw.MinHorizAxis := 0;
               end;
               Graph.GraphDraw.HorizLabel := LandType + '  ' + DEMType + '  ' + Graph.GraphDraw.HorizLabel;
            end;

            if What in [dgPick,dg7Params,dgNormalizedDiff] then begin
               Graph.GraphDraw.HorizLabel := RefFilter;
               if What in [dgNormalizedDiff] then begin
                  Graph.GraphDraw.MinHorizAxis := 0;
                  Graph.GraphDraw.MaxHorizAxis := 1.1;
               end;
            end
            else if What in [dgAllValues] then begin
               Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Difference';
               Graph.GraphDraw.MinHorizAxis := 0;
            end
            else if What in [dgAllScores] then begin
               Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Ranking';
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;
            Graph.RedrawDiagram11Click(Nil);
            fName := NextFileNumber(MDTempDir,'graph_','.png');
            SaveImageAsBMP(Graph.Image1,fName);
            BigGraph.Add(fName);
            if (LegendfName = '') then begin
               LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
               bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
               SaveBitmap(bmp,LegendfName);
               Bmp.free;
            end;
         end;

var
   J,k,ColsInBigGraph : integer;
   TheParams : tStringList;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestMeanMedianGraph in, table=' + IntToStr(DBontable)); {$EndIf}

   if (What in [dg7Params,dgNormalizedDiff,dgPick]) then begin
      fName := DEMIXSettingsDir;
      if not GetExistingFileName('criteria to plot','*.txt',fName) then exit;
      TheParams := tStringList.Create;
      TheParams.LoadFromFile(fName);
   end;
   (*
   if (What = dgPick) then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      AllCriteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      PickParam := GetFromList('Criterion to plot',AllCriteria);
      TheParams := tStringList.Create;
      TheParams.Add(PickParam);
      AllCriteria.Free;
   end;
   *)

   GISdb[DBonTable].EmpSource.Enabled := false;
   ColsInBigGraph := 3;
   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
   AreaList := tStringList.Create;
   AreaList.LoadFromFile(DEMIXSettingsDir + 'demix_areas_sorted_by_lat.txt');

   SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;

   GISdb[DBonTable].EmpSource.Enabled := false;

   if What in [dgAllValues,dgAllScores] then begin
      AllTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
      ThisTile := GetFromList('DEMIX tile to plot',AllTiles);
      FreeAndNil(AllTiles);
   end;

   BigGraph := tstringList.Create;
   LegendfName := '';
   if What in [dgPercentBest] then begin
      MakeGraphForFilter('','','');
   end
   else begin
      for j := 1 to 2 do begin
         if (What in [dgPick,dg7Params,dgNormalizedDiff]) then begin
            for k := 0 to pred(TheParams.Count) do MakeGraphForFilter(TheParams.Strings[k],LandType[1],RefType[j]);
         end
         else if (What in [dgAllValues,dgAllScores]) then begin
            what := dgAllValues; MakeGraphForFilter('',LandType[1],RefType[j]);
            what := dgAllScores; MakeGraphForFilter('',LandType[1],RefType[j]);
         end
         else if (What in [dgArea]) then begin
            MakeGraphForFilter('',LandType[1],RefType[j]);
         end;
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click graphs all made'); {$EndIf}

   DEMsPresent.Free;
   SlopesPresent.Free;
   if (What in [dgPick,dg7Params,dgNormalizedDiff]) then TheParams.Free;

   if (LegendFName <> '') then BigGraph.Add(LegendfName);
   fName := NextFileNumber(MDtempDir,'demix_big_graph_','.png');
   if MakeBigBitmap(BigGraph,'',fName,ColsInBigGraph) then  DisplayBitmap(fName,'');

   WMDEm.SetPanelText(1,'');
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
end;


procedure DEMIXTileSummary(DBonTable : integer);
var
   DEMsPresent,LandTypesPresent,Report,CriteriaPresent : tStringList;
   j,k : integer;
   aLine : shortstring;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;

   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

   GISdb[DBonTable].EmpSource.Enabled := false;
   LandTypesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   CriteriaPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('Land_Types present=' + IntToStr(LandTypesPresent.Count)); {$EndIf}
   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('Criteria present=' + IntToStr(CriteriaPresent.Count)); {$EndIf}

   Report := tStringList.Create;
   aline := 'REF_TYPE,AREA,DEMIX_TILE';
   for j := 0 to pred(LandTypesPresent.Count) do aLine := aLine + ',' + LandTypesPresent.strings[j];
   Report.Add(Aline);

   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}

   for j := 2 downto 1 do begin
      aline := RefType[j];
      GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefType[j]));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE'));
      for k := 0 to pred(LandTypesPresent.Count) do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefType[j]) + ' AND LAND_TYPE=' + QuotedStr(LandTypesPresent[k]));
         aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB div CriteriaPresent.Count);
      end;
      {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}
      Report.Add(aline);
   end;

   DEMsPresent.Free;
   LandTypesPresent.Free;
   CriteriaPresent.Free;
   fName := NextFileNumber(MDTempDir,'demix_tile_summary_','.dbf');
   StringList2CSVtoDB(Report,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   GISdb[DBonTable].ShowStatus;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary out'); {$EndIf}
end;


procedure ExtractTheDEMIXtiles(DBonTable : integer);
//for each record in the database, extracts the DEMs from the map library, subsets them to the extent of the record, and saves in c:\mapdata\demix_tiles. 
//(not recently tested; the current philosophy is to subset on the fly for a larger DEM covering a test area)
var
   bb : sfBoundBox;
   WantDEM,WantImage,i : integer;
   OutPath : PathStr;
   TileName,LastTileName,DEMIXname : shortstring;
begin
   {$IfDef RecordExports} WriteLineToDebugFile('Tdbtablef.ExtractDEMIXtiles1, n=' + IntToStr(GISdb[DBonTable].MyData.RecordCount)); {$EndIf}
    SaveBackupDefaults;    //to keep from plotting on the DEMs, and not being able to loop through all records
    MDdef.DBsOnAllMaps := false;
    LastTileName := '';
    GISdb[DBonTable].MyData.First;
    repeat
        {$IfDef RecordExports} WriteLineToDebugFile('Start loop: ' + IntToStr(GISdb[DBonTable].MyData.RecNo)); {$EndIf}
        bb := GISdb[DBonTable].MyData.GetRecordBoundingBox;
        TileName := SWcornerString(bb.ymin,bb.ymax,1);
        DEMIXname := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME');
        {$IfDef RecordExports} WriteLineToDebugFile(IntToStr(GISdb[DBonTable].MyData.RecNo) + '  ' + DEMIXname); {$EndIf}
        OutPath := 'c:\mapdata\demix_tiles\' + DEMIXname + '\';
        if (TileName <> LastTileName) then begin
           {$IfDef RecordExports} WriteLineToDebugFile('Load library: ' + IntToStr(GISdb[DBonTable].MyData.RecNo)); {$EndIf}
           CloseAllDEMs;
           LoadMapLibraryBox(WantDEM,WantImage,true,bb);
           SafeMakeDir(OutPath);
           {$IfDef RecordExports} WriteLineToDebugFile('DEMs loaded: ' + IntToStr(GISdb[DBonTable].MyData.RecNo)); {$EndIf}
        end;

        for i := 1 to MaxDEMDataSets do begin
           if ValidDEM(i) then begin
              DEMGlb[i].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
              DEMGlb[i].SaveGridSubsetGeotiff(DEMGlb[i].SelectionMap.MapDraw.MapAreaDEMGridLimits,OutPath + DEMGlb[i].AreaName + '.tif');
              {$IfDef RecordExports} WriteLineToDebugFile('DEM tile written: ' + IntToStr(i) + '  rec='+ IntToStr(GISdb[DBonTable].MyData.RecNo)); {$EndIf}
           end;
        end;
        CloseAllDEMs;
        LastTileName := TileName;
        GISdb[DBonTable].MyData.Next;
        {$IfDef RecordExports} WriteLineToDebugFile(IntToStr(GISdb[DBonTable].MyData.RecNo) + ' now doing'); {$EndIf}
    until GISdb[DBonTable].MyData.eof;
    RestoreBackupDefaults;
   {$IfDef RecordExports} WriteLineToDebugFile('Tdbtablef.ExtractDEMIXtiles1, out'); {$EndIf}
end;



