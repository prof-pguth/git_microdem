{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}

//include file for demdbtable


procedure TansposeDEMIXcriteria(DBonTable : integer);
const
   RefType : array[1..2] of shortstring = ('DTM','DSM');
   SlopeType : array[1..5] of shortstring = ('ALL','STEEP','FLAT','URBAN','FOREST');
const
   MaxDEMs = 10;
   MaxCriteria = 50;
var
   Headers,DEMs,Criteria,Output,Tiles : tStringList;
   Line : ANSIString;
   fName : PathStr;
   value,ThisDEM : shortstring;
   i,j,  Tile, Ref, Slope,DEM,Criterion : Integer;
   Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
begin
   Headers := tStringList.Create;
   Headers.LoadFromFile(ProgramRootDir + 'demix_headers.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(ProgramRootDir + 'demix_criteria.txt');
   for Criterion := pred(Criteria.Count) downto 0 do begin
      if not GISdb[DBonTable].MyData.FieldExists(Criteria.Strings[Criterion]) then Criteria.Delete(Criterion);
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

   Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');

   for Tile := 0 to Pred(Tiles.Count) do begin
      for Ref := 1 to 2 do begin
         for Slope := 1 to 5 do begin
            Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefType[Ref])  + ' AND REF_SLOPE=' + QuotedStr(SlopeType[Slope]);
            GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefType[Ref])  + ' AND REF_SLOPE=' + QuotedStr(SlopeType[Slope]) );
            GISdb[DBonTable].EmpSource.Enabled := false;
            if GISdb[DBonTable].MyData.FiltRecsInDB > 0 then begin

               for i := 0 to MaxDems do
                  for j := 0 to MaxCriteria do
                      Values[i,j] := '-9999';

               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  ThisDEM := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                  DEM := DEMs.IndexOf(ThisDEM);

                  for Criterion := 0 to pred(Criteria.Count) do begin
                     value := GISdb[DBonTable].MyData.GetFieldByNameAsString(Criteria[Criterion]);
                     if value <> '' then Values[DEM,Criterion] := value;
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
            end;
         end;
      end;
   end;
   GISdb[DBonTable].ClearGISFilter;
   Headers.Free;
   Criteria.Free;
   DEMs.Free;
   fName := GISdb[DBonTable].dbFullName;
   fName := ExtractFilePath(fname) +'transpose_' + ExtractFileName(fName);
   (*
   PetDBUtils.StringList2CSVtoDB(Output,fName);
   *)

   fName := changeFileExt(fName,'.csv');  //so they can be merged into a single CSV and then turned to DBF
   OutPut.SaveToFile(fName);
   OutPut.Free;
   GISdb[DBonTable].ShowStatus;
end;



procedure DEMIXwinecontestGraph(DBonTable : integer);

      procedure OneField(aField : shortstring; Extra : shortString = '');
      var
         TilesPresent,DEMsPresent,SlopesPresent : tstringList;
         Graph : tThisBaseGraph;
         Symbol : tFullSymbolDeclaration;
         DEM,Tile,OnTile : integer;
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
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  for SlopeCat := 1 to SlopesPresent.Count do begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SlopeCat=' + SlopesPresent.Strings[Pred(SlopeCat)]); {$EndIf}
                     Graph.GraphDraw.GraphLeftLabels.Add(TilesPresent.Strings[pred(Tile)] + '  ' + SlopesPresent.Strings[Pred(SlopeCat)]);
                     inc(OnTile);
                     for DEM := 1 to DEMsPresent.Count do begin
                        GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(TilesPresent.Strings[pred(Tile)]) + ' AND ' + aFilterElement +
                           ' AND DEM=' + QuotedStr(DEMsPresent.Strings[Pred(DEM)]) + ' AND REF_SLOPE=' + QuotedStr(SlopesPresent.Strings[Pred(SlopeCat)]));
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
         SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('REF_SLOPE');
         Graph := tThisBaseGraph.Create(Application);
         Graph.GraphDraw.LegendList := tStringList.Create;
         Graph.GraphDraw.HorizLabel := aField + ' ' + Extra;
         Graph.Caption := GISdb[DBonTable].DBName;

         for DEM := 1 to DEMsPresent.Count do begin
            Symbol.Size := 4;
            Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[DEM mod 15]);
            case DEM of
                1 : Symbol.DrawingSymbol := FilledBox;
                2 : Symbol.DrawingSymbol := FilledCircle;
                3 : Symbol.DrawingSymbol := FilledDiamond;
                4 : Symbol.DrawingSymbol := FilledDownTri;
                5 : Symbol.DrawingSymbol := FilledUpTri;
                6 : Symbol.DrawingSymbol := Box;
                else Symbol.DrawingSymbol := FilledBox;
            end;
            Graph.OpenPointFile(rfile[DEM],Symbol);
            Graph.GraphDraw.LegendList.Add(DEMsPresent.Strings[Pred(DEM)]);
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
   if (GISdb[DBonTable].DBName = 'Elev_diff_stats') {or (GISdb[DBonTable].DBName = 'Flat_Elev_diff_stats')  or (GISdb[DBonTable].DBName = 'Steep_Elev_diff_stats')} then begin
      OneField('ELD_MEAN');
      OneField('ELD_MEDIAN');
      OneField('ELD_RMSE');
      OneField('ELD_LE90');
      //OneField('ELD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'Slope_diff_stats') {or (GISdb[DBonTable].DBName = 'Flat_Slope_diff_stats') or (GISdb[DBonTable].DBName = 'Steep_Slope_diff_stats')} then begin
      OneField('SMD_MEDIAN');
      OneField('SMD_MEAN');
      OneField('SMD_RMSE');
      //OneField('SMD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'Roughness_diff_stats') {or (GISdb[DBonTable].DBName = 'Flat_Roughness_diff_stats') or (GISdb[DBonTable].DBName = 'Steep_Roughness_diff_stats')} then begin
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

