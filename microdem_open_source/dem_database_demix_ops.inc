{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}

//include file for demdatbase


const
   MaxLandType = 7;
   NumDEMIXDEM = 6;
   RefType : array[1..2] of shortstring = ('DSM','DTM');
   LandType : array[1..MaxLandType] of shortstring = ('ALL','CLIFF','STEEP','GENTLE','FLAT','URBAN','FOREST');
   DEMType : array[1..NumDEMIXDEM] of shortstring = ('FABDEM','COP','ALOS','NASA','SRTM','ASTER');
   MeanParams : array[1..3] of shortstring = ('ELVD_MEAN','SLPD_MEAN','RUFD_MEAN');
   MedianParams : array[1..3] of shortstring = ('ELVD_MED','SLPD_MED','RUFD_MED');
   SimpleParams : array[1..3] of shortstring = ('ELVD_RMSE','SLPD_MAE','RUFD_AVG');
   Elev7Params : array[1..7] of shortstring = ('ELVD_AVG','ELVD_STD','ELVD_RMSE','ELVD_MAE','ELVD_LE90','ELVD_MEAN','ELVD_MED');


function CriterionTieTolerance(Criterion : shortstring) : float32;
var
    TieToleranceTable : tMyData;
begin
   TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);
   TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
   Result := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');
   TieToleranceTable.Destroy;
end;

function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
var
   DEM : integer;
begin
   Result.Size := 4;
   DEMName := UpperCase(DEMName);
   for DEM := 1 to NumDEMIXDEM do begin
      if DEMName = DEMType[DEM] then Result.Color := ConvertTColorToPlatformColor(WinGraphColors[DEM]);
   end;
   if DEMName = 'FABDEM' then Result.DrawingSymbol := FilledBox
   else if DEMName = 'COP' then Result.DrawingSymbol := FilledCircle
   else if DEMName = 'ALOS' then Result.DrawingSymbol := FilledDiamond
   else if DEMName = 'NASA' then Result.DrawingSymbol := FilledDownTri
   else if DEMName = 'SRTM' then Result.DrawingSymbol := FilledUpTri
   else if DEMName = 'ASTER' then Result.DrawingSymbol := Box
   else Result.DrawingSymbol := FilledBox;
   if DEMName = 'TIE' then Result.Color := claBrown;
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
            aHeader := RefType[i] + ' ' + 'ALL pixels  ALL pixels  Tile relief < 25m';
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
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out, MaxVertAxis=' + RealToString(Result.GraphDraw.MaxVertAxis,-12,-2)); {$EndIf}
   end
   else begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, requires BestDEMSbyCategory'); {$EndIf}
      BestDEMSbyCategory(DBonTable);
   end;
end;



procedure DEMIX_graph_best_in_Tile(DBonTable : integer);
var
   RefFilter : shortstring;
   CriteriaTable : tMyData;
   BigBMPFiles : tStringList;
   //LegendMade : boolean;
   LegendfName,fName : PathStr;


    procedure OneLoop;
    const
       InterAreaSkip = 2;
    var
       NumAreas,NumTiles : integer;
       TileList,AreaList : tStringList;
       i,j,k,CurrentY,DEM,Center,NumTies : integer;
       Criterion,Best,TStr : shortstring;
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
                       if (Best = DEMType[DEM]) then break;
                    end;
                    v[2] := CurrentY;
                    v[1] := Center;
                    BlockWrite(rfile[DEM],v,1);
                end;


    begin
       ShowHourglassCursor;
       GISdb[DBonTable].ApplyGISFilter(RefFilter);
       GISdb[DBonTable].EmpSource.Enabled := false;
       NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');
       GISdb[DBonTable].EmpSource.Enabled := false;
       NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
       GISdb[DBonTable].EmpSource.Enabled := false;
       AreaList := GISdb[DBonTable].MyData.UniqueEntriesInDB('SORT_AREA');
       MDDef.DefaultGraphXSize := 1200;
       MDDef.DefaultGraphYSize := 1400;

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
         Graph.GraphDraw.MinVertAxis := 0;
         Graph.GraphDraw.MaxVertAxis := NumTiles + InterAreaSkip * NumAreas;
         Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
         Graph.GraphDraw.GraphBottomLabels := tStringList.Create;

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
          WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
          WMDEM.SetPanelText(1,RefFilter);
          GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND SORT_AREA=' + QuotedStr(AreaList.Strings[i]));
          //{$If Defined(RecordDEMIX)} WriteLineToDebugFile(GISdb[DBonTable].MyData.Filter + '  currenty=' + IntToStr(CurrentY)); {$EndIf}
          TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
          TStr := IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
          Graph.GraphDraw.GraphLeftLabels.Add(TStr);
          //{$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
          for j := 0 to pred(TileList.Count) do begin
             dec(CurrentY);
             GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND SORT_AREA=' + QuotedStr(AreaList.Strings[i]) + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]));
             //{$If Defined(RecordDEMIX)} WriteLineToDebugFile(GISdb[DBonTable].MyData.Filter); {$EndIf}

             GISdb[DBonTable].EmpSource.Enabled := false;
             while not GISdb[DBonTable].MyData.eof do begin
                Criterion := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
                Best := uppercase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC'));

                CriteriaTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
                Center := CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE');

                if StrUtils.AnsiContainsText(Best,'-') then begin //this is a tie for best
                   NumTies := 0;
                   repeat
                      inc(NumTies);
                      aDEM[NumTies] := BeforeSpecifiedCharacterANSI(Best,'-',true,true);
                   until Best = '';
                   for DEM := 1 to NumTies do begin
                      LocateBest(aDEM[DEM],Center-0.3 + DEM * 0.15);
                   end;
                end
                else begin //single best
                    LocateBest(Best,Center);
                end;
                GISdb[DBonTable].MyData.Next;
             end;
          end;
          dec(CurrentY,InterAreaSkip);  //add a blank line between areas
       end;

       for DEM := 1 to NumDEMIXDEM do begin
          CloseFile(rfile[DEM]);
       end;

       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
       Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
       //Graph.GraphDraw.LeftMargin := 100;
       Graph.GraphDraw.BottomMargin := 195;
       Graph.GraphDraw.TopMargin := 50;
       Graph.Height := 1250;
       Graph.RedrawDiagram11Click(Nil);
       fName := NextFileNumber(MDTempDir,'best_dem_graph_','.png');
       SaveImageAsBMP(Graph.Image1,fName);
       BigBMPfiles.Add(fName);
       if (LegendfName = '') then begin
          LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
          bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
          SaveBitmap(bmp,LegendfName);
          Bmp.free;
       end;
    end;



var
   i,j : integer;
   //fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile in'); {$EndIf}
   try
      SaveBackupDefaults;
      fName := ProgramRootDir + 'demix_criteria_order_with_gaps.dbf';
      CriteriaTable := tMyData.Create(fName);

      for i := 1 to 2 do begin
         //LegendMade := false;
         LegendfName := '';
         BigBMPfiles := tStringList.Create;
         for j := 1 to MaxLandType do begin
            RefFilter := 'REF_TYPE=' + QuotedStr(RefType[i]) + ' AND LAND_TYPE=' + QuotedStr(LandType[j]);
            OneLoop;
         end;
         BigBMPfiles.Add(LegendfName);
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
   cop,alos,TieTolerance : float32;
   which,RefFilter : shortstring;
   i,j,ties,opinions,db : integer;
   fName : PathStr;
   TieToleranceTable : tMyData;
   Findings,Criteria : tStringList;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,Cop,ALOS,Ties : integer;
   begin
      {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DO-ONE  ' + theFilter); {$EndIf}
      WMDEM.SetPanelText(1,theFilter);
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (Opinions >= 10) then begin
         GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('alos'));
         ALOS := GISdb[DBonTable].MyData.FiltRecsInDB;
         GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('cop'));
         COP := GISdb[DBonTable].MyData.FiltRecsInDB;
         GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('tie'));
         ties := GISdb[DBonTable].MyData.FiltRecsInDB;
         Findings.Add(Header + '  (n=' + IntToStr(Opinions) + '),' + RealToString(100.0 * alos/opinions,-8,-2)+ ',' + RealToString(100.0 * ties/opinions,-8,-2)+ ',' + RealToString(100.0 * cop/opinions,-8,-2));
      end;
   end;

var
   aHeader,aFilter : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXisCOPorALOSbetter in'); {$EndIf}
   if not GetFileFromDirectory('DEMIX criterial tolerances','*.dbf',MDDef.DEMIX_criterion_tolerance_fName) then exit;
   try
      GISdb[DBonTable].ApplyGISFilter('');
      ShowHourglassCursor;
      if GISdb[DBonTable].MyData.FieldExists('COP_ALOS') then begin
         GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'COP_ALOS',8);
         TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);
         while not GISdb[DBonTable].MyData.eof do begin
            cop := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('COP');
            alos := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('ALOS');
            TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
            TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');
            if (abs(cop-alos) < TieTolerance) then which := 'tie'
            else if (cop < alos) then which := 'cop'
            else which := 'alos';
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsString('COP_ALOS',which);
            GISdb[DBonTable].MyData.Next;
         end;
         TieToleranceTable.Destroy;
      end;

      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');

      for i := 1 to 2 do begin
         {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXisCOPorALOSbetter start ' + RefType[i]); {$EndIf}
         ShowHourglassCursor;
         Findings := tStringList.Create;
         Findings.Add('FILTER,ALOS,TIE,COP');

         RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]);
         for j := 1 to MaxLandType do begin
            DoOne(RefType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
         end;
         Findings.Add('SKIP');

         for j := 1 to 8 do begin
            GetFilterAndHeader(i,j,aHeader,aFilter);
            DoOne(aHeader,aFilter);
         end;

         Findings.Add('SKIP');
         for j := 0 to pred(Criteria.Count) do begin
            DoOne(RefType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
         end;

         fName := NextFileNumber(MDTempDir,RefType[i] + '_cop_or_alos_','.dbf');
         db := StringList2CSVtoDB(Findings,fName);
         DEMIXwineContestScoresGraph(DB,'Winning percentage',0,100);
      end;
   finally
      Criteria.Destroy;
      ShowDefaultCursor;
      GISdb[DBonTable].ApplyGISFilter('');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXisCOPorALOSbetter out'); {$EndIf}

end;


procedure FilterOutSignedCriteria(DBonTable : integer);
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
end;


function TransposeDEMIXcriteria(DBonTable : integer; CriteriaFile : PathStr = '') : PathStr;
const
   MaxDEMs = 10;
   MaxCriteria = 50;
var
   Headers,DEMs,Criteria,Output,Tiles : tStringList;
   Line : ANSIString;
   fName : PathStr;
   value,ThisDEM,Missing : shortstring;
   Cycles,Done,
   i,j,  Tile, Ref, Slope,DEM,Criterion : Integer;
   Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile(''); WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
   StopSplashing;
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DEM_LOW_SC',48);

   Headers := tStringList.Create;
   Headers.LoadFromFile(ProgramRootDir + 'demix_headers.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');

   if not FileExists(CriteriaFile) then CriteriaFile := ProgramRootDir + 'demix_criteria.txt';
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(CriteriaFile);

   for Criterion := pred(Criteria.Count) downto 0 do begin
      if not GISdb[DBonTable].MyData.FieldExists(Criteria.Strings[Criterion]) then begin
         Criteria.Delete(Criterion);
      end;
   end;

   Output := tStringList.Create;
   Line := '';
   for i := 0 to pred(Headers.Count) do Line := Line + Headers.Strings[i] + ',';
   Line := Line + 'CRITERION,';
   for i := 0 to pred(DEMs.Count) do begin
      Line := Line + DEMs.Strings[i];
      if i < pred(DEMs.Count) then Line := Line + ',';
   end;
   Output.Add(Line);

   GISdb[DBonTable].ClearGISFilter;
   Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria tiles=' + IntToStr(Tiles.Count)); {$EndIf}

   Cycles := Tiles.Count * 2 * 7;
   Done := 0;
   StartProgress('Transposing ' + GISdb[DBontable].DBName);
   for Tile := 0 to Pred(Tiles.Count) do begin
      UpdateProgressBar(Done/Cycles);
      for Ref := 1 to 2 do begin
         Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefType[Ref]);
         GISdb[DBonTable].ApplyGISFilter(Line);
         GISdb[DBonTable].EmpSource.Enabled := false;
         if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //there are DSMs or DTMs for this tile
            for Slope := 1 to 7 do begin
               Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(LandType[Slope]);
               GISdb[DBonTable].ApplyGISFilter(Line);
               GISdb[DBonTable].EmpSource.Enabled := false;
               if (GISdb[DBonTable].MyData.FiltRecsInDB = DEMs.Count) then begin
                  for i := 0 to MaxDems do
                     for j := 0 to MaxCriteria do
                         Values[i,j] := '-9999';

                  while not GISdb[DBonTable].MyData.eof do begin
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     ThisDEM := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                     DEM := DEMs.IndexOf(ThisDEM);
                     for Criterion := 0 to pred(Criteria.Count) do begin
                        value := GISdb[DBonTable].MyData.GetFieldByNameAsString(Criteria[Criterion]);
                        if (value <> '') then Values[DEM,Criterion] := value;
                     end;
                     GISdb[DBonTable].MyData.Next;
                  end;

                  for Criterion := 0 to pred(Criteria.Count) do begin
                     Line := '';
                     for i := 0 to pred(Headers.Count) do Line := Line + GISdb[DBonTable].MyData.GetFieldByNameAsString(Headers[i]) + ',';
                     Line := Line + Criteria[Criterion] + ',';

                     for DEM := 0 to pred(DEMs.Count) do Line := Line + Values[DEM,Criterion] + ',';
                     Delete(Line,Length(Line),1);
                     Output.Add(Line);
                  end;
               end
               else begin
                  {$If Defined(RecordDEMIX)}  //this is to track down why some tiles do not have 6 records
                     if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //if it was 0, it was something like CLIFF in the flatlands
                        Missing := '  Present: ';
                        GISdb[DBonTable].MyData.First;
                        while not GISdb[DBonTable].MyData.eof do begin
                           Missing := Missing + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM') + ', ';
                           GISdb[DBonTable].MyData.Next;
                        end;
                        WriteLineToDebugFile('filter=' + GISdb[DBonTable].MyData.Filter + '   Matches=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + Missing);
                     end;
                  {$EndIf}
               end;
            end;
         end;
      end;
   end;
   EndProgress;
   GISdb[DBonTable].ClearGISFilter;
   Headers.Free;
   Criteria.Free;
   DEMs.Free;
   Result := ChangeFileExt(GISdb[DBonTable].dbFullName, '_transpose_' + ExtractFileNameNoExt(CriteriaFile) + '.dbf');
   DeleteFileIfExists(Result);
   Result := ChangeFileExt(Result,'.csv');
   OutPut.SaveToFile(Result);
   OutPut.Free;
   GISdb[DBonTable].ShowStatus;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria out, created ' + Result); {$EndIf}
end;



procedure RankDEMS(DBonTable : integer);
var
   DEMsRanked,DEMs,Scores : tStringList;
   DEM,Value,N,i,Score : integer;
   Sum,fValue,BestValue,TieTolerance : float32;
   fName : PathStr;
   BestValueString,aLine : shortstring;
   Values,Sorted : array[0..10] of float32;
   Results : tStringList;
   TieToleranceTable : tMyData;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    if not GetFileFromDirectory('DEMIX criterial tolerances','*.dbf',MDDef.DEMIX_criterion_tolerance_fName) then exit;

    DEMs := tStringList.Create;
    DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    for DEM := 0 to pred(DEMs.Count) do begin
       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,DEMs.Strings[DEM] + '_SCR',8,2);
    end;

    DEMsRanked := tStringList.Create;
    DEMsRanked.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    i := 0;
    if GetFromListZeroBased('DEMs to use in ranking',i,DEMsRanked,true,true) then begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load criterion tolerances, ' + MDDef.DEMIX_criterion_tolerance_fName); {$EndIf}
       TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);

       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,'TOLERANCE',8,2);
       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DEM_LOW_SC',48);
       if GISdb[DBonTable].MyData.GetFieldLength('DEM_LOW_SC') < 48 then GISdb[DBonTable].MyData.TrimField('DEM_LOW_SC',48);

       StartProgress('Rank DEMs');
       i := 0;
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.eof do begin
          inc(i);
          UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;

          for DEM := 0 to pred(DEMs.Count) do begin
             if (DEMsRanked.IndexOf(DEMs.Strings[DEM]) <> -1) then Values[DEM] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs[DEM])
             else Values[DEM] := 9999;
             Sorted[DEM] := Values[DEM];
          end;

          HeapSort(DEMs.Count, Sorted);
          TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
          TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');

          GISdb[DBonTable].MyData.Edit;
          for DEM := 0 to pred(DEMs.Count) do begin
             if (DEMsRanked.IndexOf(DEMs.Strings[DEM]) <> -1) then begin
                Sum := 0;
                N := 0;
                for value := pred(DEMs.Count) downto 0 do begin
                   if (abs(Values[DEM]-Sorted[Value]) < TieTolerance) then begin
                      Sum := Sum + succ(Value);
                      inc(n);
                   end;
                end;
                Sum := Sum/N;
             end
             else Sum := DEMs.Count;
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(DEMs.Strings[DEM] + '_SCR',Sum);
          end;
          GISdb[DBonTable].MyData.Next;
       end;

       StartProgress('Find best DEMs');

       i := 0;
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.eof do begin
          inc(i);
          UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;

          TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
          TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');

          BestValue := 99e38;
          for DEM := 0 to pred(DEMs.Count) do begin
             if (DEMsRanked.IndexOf(DEMs.Strings[DEM]) <> -1) then begin
                fValue := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs[DEM]);
                if (fValue > 0) then begin
                   if (abs(fValue - BestValue) < TieTolerance) then begin
                      BestValueString := BestValueString + '-' + DEMs[DEM];
                   end;
                   if (fValue < BestValue) then begin
                      BestValue := fValue;
                      BestValueString := DEMs[DEM];
                   end;
                end;
             end;
          end;
          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',BestValueString);
          GISdb[DBonTable].MyData.SetFieldByNameAsFloat('TOLERANCE',TieTolerance);
          GISdb[DBonTable].MyData.Next;
       end;

       Results := tStringList.Create;
       Results.Add('DEM,SUM_SCORES');
       for DEM := 0 to pred(DEMs.Count) do begin
          Results.Add(DEMs.Strings[DEM] + ',' + RealToString(GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR'),-12,-2));
       end;

       fName := NextFileNumber(MDTempDir,'sum_scores_','.dbf');
       StringList2CSVtoDB(Results,fName);

       Scores := tStringList.Create;
       Scores.LoadFromFile(ProgramRootDir + 'demix_scores.txt');

       StartProgress('Histogram Rank DEMs');
       Results := tStringList.Create;
       aline := 'DEM';
       for Score := 0 to pred(Scores.Count) do aLine := aLine + ',' + Scores[Score];
       Results.Add(aLine);

       for DEM := 0 to pred(DEMs.Count) do begin
          UpdateProgressBar(DEM/DEMs.Count);
          aLine := DEMs.Strings[DEM];
          fValue := 1;
          for Score := 0 to pred(Scores.Count) do begin
             GISdb[DBonTable].ApplyGISFilter(DEMs.Strings[DEM] + '_SCR < ' + RealToString(fValue + 0.1,-6,-1) + ' AND ' + DEMs.Strings[DEM] + '_SCR > ' + RealToString(fValue - 0.1,-6,-1)  );
             aLine := aLine + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB);
             fValue :=fValue + 0.5;
          end;
          Results.Add(aLine);
       end;

       fName := NextFileNumber(MDTempDir,'score_histograms_','.dbf');
       Results.SaveToFile(mdtempdir + 'test.csv');
       StringList2CSVtoDB(Results,fName);
       Scores.Destroy;
    end;
    GISdb[DBonTable].ClearGISFilter;
    DEMs.Destroy;
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
    DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
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
   j,ScoresDB : integer;
   Graph : tThisBaseGraph;
   bmp : tMyBitmap;
   LegendfName : PathStr;
   BigBitmap : tStringList;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory in, ' + GISdb[DBonTable].dbName); {$EndIf}
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(ProgramRootDir + 'demix_criteria.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
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

      for J := 1 to 8 do begin
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
   SimpleTiles : array[1..4] of shortstring = ('N43PW002B','N37YW076F','N28VW018B','S24TW047D');
   InterAreaSkip = 4;
var
   AreaList,AllTiles,AllCriteria,DEMsPresent,SlopesPresent,BigGraph : tstringList;
   PickParam,ThisTile : shortstring;
   WhatNow : integer;
   //LegendMade : boolean;
   LegendfName,fName : PathStr;


         procedure MakeGraphForFilter(Param,LandType,DEMType : shortstring);
         var
            Graph : tThisBaseGraph;
            Symbol : tFullSymbolDeclaration;
            DEM,CurrentY,DEMis,i,j,NumAreas,NumTiles : integer;
            rfile : array[1..10] of file;
            TileList : tStringList;
            v : array[1..2] of float32;
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
            if (What = dgSimpleExample) then begin
               RefFilter := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
               aFilter := '';
               for I := 1 to 3 do aFilter := AddOrIfNeeded(aFilter) + 'CRITERION=' + QuotedStr(SimpleParams[i]);
               RefFilter := RefFilter + ' AND (' + aFilter + ')';
            end
            else if (What in [dgAllValues,dgAllScores,dgPercentBest]) then begin
            end
            else if (What in [dgArea,dg7Params]) then begin
               RefFilter := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end
            else if (What in [dgPick,dgMean,dgMedian,dgJust3Params]) then begin
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

            CurrentY := NumTiles + InterAreaSkip * NumAreas;

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

            if What in [dg7Params] then begin
               for i := 1 to 7 do begin
                  Symbol.Size := 4;
                  Symbol.DrawingSymbol := FilledBox;
                  Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[i]);
                  Graph.OpenPointFile(rfile[i],Symbol);
                  Graph.GraphDraw.LegendList.Add(Elev7Params[i]);
               end;
            end
            else begin
               for DEM := 1 to DEMsPresent.Count do begin
                  Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
                  Graph.OpenPointFile(rfile[DEM],Symbol);
                  Graph.GraphDraw.LegendList.Add(DEMsPresent.Strings[Pred(DEM)]);
               end;
            end;

            for i := 0 to pred(AreaList.Count) do begin
                GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND AREA=' + QuotedStr(AreaList.Strings[i]));
                WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
                WMDEM.SetPanelText(1,RefFilter);
                if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
                   //TStr := IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
                   Graph.GraphDraw.GraphLeftLabels.Add(IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA'));
                   for j := 0 to pred(TileList.Count) do begin
                        dec(CurrentY);
                        GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND AREA=' + QuotedStr(AreaList.Strings[i]) + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]));


                     if What in [dgArea] then begin
                        (*
                        for i := 1 to AllAreas.Count do begin
                           GISdb[DBonTable].ApplyGISFilter(RefFilter + 'AND AREA=' + QuotedStr(AllAreas.Strings[pred(i)]));
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           Graph.GraphDraw.GraphLeftLabels.Add(AllAreas.Strings[pred(i)] + '  ' + LandType + '  ' + DEMType);
                           inc(OnTile);
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[2] := Ontile;
                              v[1] := GISdb[DBonTable].MyData.FieldSum(DEMsPresent.Strings[DEM] + '_SCR') / GISdb[DBonTable].MyData.FiltRecsInDB;
                              BlockWrite(rfile[succ(DEM)],v,1);
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;
                        *)
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
                     else if What in [dgSimpleExample] then begin
                        (*
                        Graph.GraphDraw.VertLabel := '';
                        GISdb[DBonTable].MyData.First;
                        while not GISdb[DBonTable].MyData.eof do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           inc(OnTile);
                           Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION') + '  ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[2] := Ontile;
                              if WhatNow in [dgAllValues] then v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM])
                              else v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM] + '_SCR');
                              BlockWrite(rfile[succ(DEM)],v,1);
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;
                        *)
                     end
                     else if What in [dg7Params] then begin
                        GISdb[DBonTable].MyData.First;
                        while not GISdb[DBonTable].MyData.eof do begin
                           var TStr : shortstring;
                           TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
                           var k : integer;
                           for k := 1 to 7 do begin
                              if (TStr = Elev7Params[k]) then begin
                                 v[2] := CurrentY;
                                 v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Param);
                                 BlockWrite(rfile[k],v,1);
                                 Break;
                              end;
                           end;
                           GISdb[DBonTable].MyData.Next;
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
            Graph.Height := 1400;
            Graph.GraphDraw.BottomMargin := 75;

            if What in [dgArea] then begin
               Graph.GraphDraw.HorizLabel := 'Average Ranking';
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;

            if (What in [dg7Params]) then begin
               Graph.GraphDraw.HorizLabel := 'DEM=' + Param + ' ' + RefFilter;
            end;

            if (What in [dgPercentBest]) then begin
               Graph.GraphDraw.HorizLabel := 'Percentage of Criteria Where DEM is Best';
               Graph.GraphDraw.MinHorizAxis := 0;
               Graph.GraphDraw.MaxHorizAxis := 100;
            end;

            if (What in [dgSimpleExample]) then begin
               if WhatNow in [dgAllValues] then begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Difference';
                  Graph.GraphDraw.MinHorizAxis := 0;
               end
               else begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Ranking';
                  Graph.GraphDraw.MinHorizAxis := 0.8;
                  Graph.GraphDraw.MaxHorizAxis := 6.2;
               end;
               Graph.GraphDraw.HorizLabel := LandType + '  ' + DEMType + '  ' + Graph.GraphDraw.HorizLabel;
            end;

            if What in [dgPick,dgMean,dgMedian,dgJust3Params] then begin
               Graph.GraphDraw.HorizLabel := RefFilter;
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
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestMeanMedianGraph in, table=' + IntToStr(DBontable)); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   ColsInBigGraph := 3;
   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
   AreaList := tStringList.Create;
   AreaList.LoadFromFile(ProgramRootDir + 'demix_areas_sorted_by_lat.txt');
   //NumAreas := AreaList.Count;


            ////GISdb[DBonTable].EmpSource.Enabled := false;
            //NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('SORT_AREA');
            //AreaList := GISdb[DBonTable].MyData.UniqueEntriesInDB('SORT_AREA');

   SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   AllCriteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');

   GISdb[DBonTable].EmpSource.Enabled := false;
   if (What = dgPick) then begin
      //GISdb[DBonTable].EmpSource.Enabled := false;
      PickParam := GetFromList('Criterion to plot',AllCriteria);
   end;
   if What in [dgAllValues,dgAllScores] then begin
      AllTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
      ThisTile := GetFromList('DEMIX tile to plot',AllTiles);
      FreeAndNil(AllTiles);
   end;

   (*
   if What in [dgArea] then begin
      AllAreas := GISdb[DBonTable].MyData.UniqueEntriesInDB('AREA');
   end;
   *)

   BigGraph := tstringList.Create;
   LegendfName := '';

   if What in [dgSimpleExample] then begin
      WhatNow := dgAllValues;
      MakeGraphForFilter('','ALL','DTM');
      WhatNow := dgAllScores;
      MakeGraphForFilter('','ALL','DTM');
   end
   else if What in [dgPercentBest] then begin
      MakeGraphForFilter('','','');
   end
   else begin
      for j := 1 to 2 do begin
         if (What = dgJust3Params) then begin
            for k := 1 to 3 do MakeGraphForFilter(SimpleParams[k],LandType[1],RefType[j]);
         end
         else if (What = dgMean) then begin
            for k := 1 to 3 do MakeGraphForFilter(MeanParams[k],LandType[1],RefType[j]);
         end
         else if (What = dgMedian) then begin
            for k := 1 to 3 do MakeGraphForFilter(MedianParams[k],LandType[1],RefType[j]);
         end
         else if (What in [dgAllValues,dgAllScores]) then begin
            what := dgAllValues; MakeGraphForFilter('',LandType[1],RefType[j]);
            what := dgAllScores; MakeGraphForFilter('',LandType[1],RefType[j]);
         end
         else if (What in [dgArea]) then begin
            MakeGraphForFilter('',LandType[1],RefType[j]);
         end
         else if (What = dg7Params) then begin
            MakeGraphForFilter('ALOS',LandType[1],RefType[j]);
            MakeGraphForFilter('ASTER',LandType[1],RefType[j]);
         end
         else if (What = dgPick) then begin
            MakeGraphForFilter(PickParam,LandType[1],RefType[j]);
         end;
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click graphs all made'); {$EndIf}

   DEMsPresent.Free;
   SlopesPresent.Free;
   AllCriteria.Free;
   //FreeAndNil(AllAreas);

   if (What = dg7Params) then ColsInBigGraph := 2;

   BigGraph.Add(LegendfName);
   fName := NextFileNumber(MDtempDir,'demix_big_graph_','.png');
   MakeBigBitmap(BigGraph,'',fName,ColsInBigGraph);
   DisplayBitmap(fName,'');

   WMDEm.SetPanelText(1,'');
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
end;


procedure DEMIXTileSummary(DBonTable : integer);
var
   DEMsPresent,SlopesPresent,Report,CriteriaPresent : tStringList;
   j,k : integer;
   aLine : shortstring;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;

   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(ProgramRootDir + 'demix_dems.txt');

   GISdb[DBonTable].EmpSource.Enabled := false;
   SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   CriteriaPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');

   Report := tStringList.Create;
   aline := 'REF_TYPE,AREA,DEMIX_TILE';
   for j := 0 to pred(SlopesPresent.Count) do aLine := aLine + ',' + SlopesPresent.strings[j];
   Report.Add(Aline);

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}

   for j := 2 downto 1 do begin
      aline := RefType[j];
      GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefType[j]));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE'));
      for k := 0 to pred(SlopesPresent.Count) do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefType[j]) + ' AND LAND_TYPE=' + QuotedStr(SlopesPresent[k]));
         aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB div CriteriaPresent.Count);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}
      end;
      Report.Add(aline);
   end;

   DEMsPresent.Free;
   SlopesPresent.Free;
   CriteriaPresent.Free;
   fName := NextFileNumber(MDTempDir,'demix_tile_summary_','.dbf');
   StringList2CSVtoDB(Report,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary out'); {$EndIf}
end;


procedure ExtractTheDEMIXtiles(DBonTable : integer);
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

        LastTileName := TileName;
        GISdb[DBonTable].MyData.Next;
        {$IfDef RecordExports} WriteLineToDebugFile(IntToStr(GISdb[DBonTable].MyData.RecNo) + ' now doing'); {$EndIf}
    until GISdb[DBonTable].MyData.eof;
    CloseAllDEMs;
    RestoreBackupDefaults;
   {$IfDef RecordExports} WriteLineToDebugFile('Tdbtablef.ExtractDEMIXtiles1, out'); {$EndIf}
end;




(*
procedure DEMIXwinecontestGraph(DBonTable : integer);

      procedure OneField(aField : shortstring; Extra : shortString = '');
      var
         TilesPresent,DEMsPresent,SlopesPresent : tstringList;
         Graph : tThisBaseGraph;
         Symbol : tFullSymbolDeclaration;
         DEM,OnTile : integer;
         rfile : array[1..10] of file;
         v : array[1..2] of float32;


         procedure DoRefType(aFilterElement,Title : shortString);
         var
            Tile,DEM,SlopeCat : integer;
         begin
            GISdb[DBonTable].MyData.First;
            GISdb[DBonTable].ApplyGISFilter(aFilterElement);
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add('');
               for Tile := 1 to TilesPresent.Count do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Tile ' + TilesPresent.Strings[pred(Tile)]); {$EndIf}
                  for SlopeCat := 1 to SlopesPresent.Count do begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SlopeCat=' + SlopesPresent.Strings[Pred(SlopeCat)]); {$EndIf}
                     Graph.GraphDraw.GraphLeftLabels.Add(TilesPresent.Strings[pred(Tile)] + '  ' + SlopesPresent.Strings[Pred(SlopeCat)]);
                     inc(OnTile);
                     for DEM := 1 to DEMsPresent.Count do begin
                        GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(TilesPresent.Strings[pred(Tile)]) + ' AND ' + aFilterElement +
                           ' AND DEM=' + QuotedStr(DEMsPresent.Strings[Pred(DEM)]) + ' AND LAND_TYPE=' + QuotedStr(SlopesPresent.Strings[Pred(SlopeCat)]));
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Filter= ' + GISdb[DBonTable].MyData.Filter + '  recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
                        if (GISdb[DBonTable].MyData.GetFieldByNameAsString(aField) <> '') then begin
                           v[2] := Ontile;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(aField);
                           if v[1] > Graph.GraphDraw.MaxHorizAxis then Graph.GraphDraw.MaxHorizAxis := v[1] + 1;
                           if v[1] < Graph.GraphDraw.MinHorizAxis then Graph.GraphDraw.MinHorizAxis := v[1] - 1;
                           BlockWrite(rfile[DEM],v,1);
                        end;
                        GISdb[DBonTable].MyData.Next;
                     end;
                  end;
               end;
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add(Title);
            end;
         end;


      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('OneField in, ' + aField); {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         TilesPresent := nil;
         DEMsPresent := nil;
         TilesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
         DEMsPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEM');
         SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
         Graph := tThisBaseGraph.Create(Application);
         Graph.GraphDraw.LegendList := tStringList.Create;
         Graph.GraphDraw.HorizLabel := aField + ' ' + Extra;
         Graph.Caption := GISdb[DBonTable].DBName;

         for DEM := 1 to DEMsPresent.Count do begin
            Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
         end;

         Graph.GraphDraw.GraphAxes := XPartGridOnly;
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MaxHorizAxis := 0;
         Graph.GraphDraw.GraphLeftLabels := tStringList.Create;

         OnTile := 0;

         DoRefType('REF_TYPE=' + QuotedStr('DTM'),'DTM');
         DoRefType('REF_TYPE=' + QuotedStr('DSM'),'DSM');

         for DEM := 1 to DEMsPresent.Count do begin
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
         TilesPresent.Free;
         DEMsPresent.Free;
         GISdb[DBonTable].ApplyGISFilter('');
      end;

begin
   if (GISdb[DBonTable].DBName = 'elev_merge_difference_ref_dem') then begin
      OneField('ELVD_MEAN');
      OneField('ELVD_MED');
      OneField('ELVD_RMSE');
      OneField('ELVD_LE90');
      //OneField('ELVD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'slope_merge_difference_ref_dem') then begin
      OneField('SLPD_MED');
      OneField('SLPD_MEAN');
      OneField('SLPD_RMSE');
      //OneField('SLPD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'ruff_merge_difference_ref_dem') then begin
      OneField('RUFD_MED');
      OneField('RUFD_MEAN');
      OneField('RUFD_RMSE');
      //OneField('RUFD_STD');
   end
   else if GISdb[DBonTable].DBName = 'Slope_moments_diff' then begin
      OneField('SDIFF_MEAN');
      OneField('SDIFF_MED');
      OneField('SDIFF_STD');
      OneField('SDIFF_SKEW');
      OneField('SDIFF_KURT');
   end
   else if GISdb[DBonTable].DBName = 'Elevation_moments_diff' then begin
      OneField('EDIFF_MEAN');
      OneField('EDIFF_MED');
      OneField('EDIFF_STD');
      OneField('EDIFF_SKEW');
      OneField('EDIFF_KURT');
   end
   else MessageToContinue('Not DEMIX results table');
   GISdb[DBonTable].EmpSource.Enabled := true;
end;
*)


