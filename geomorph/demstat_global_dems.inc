{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{                                        }
{   include file for demstat             }
{________________________________________}

type
   tLidarStringArray = array[1..10] of shortstring;
var
   LidarFields,LidarLabels : tLidarStringArray;
   ns : integer;


function GetCloudHeightFilter : shortstring;
var
   Min,Max : float32;
begin
   if AnswerIsYes('Cloud height filter') then begin
       Min := 0;
       Max := 5;
       ReadDefault('Min cloud height',Min);
       ReadDefault('Max cloud height',Max);
       Result := 'CLOUD_HT > ' + RealToString(Min,-8,2) + ' AND CLOUD_HT < ' + RealToString(Max,-8,2);
   end
   else begin
      Result := '';
   end;
end;


procedure GetFieldsAndLabels(DBonTable : integer);
var
   i : integer;
   fName : ShortString;
begin
  if GISdb[DBonTable].MyData.FieldExists('ICESAT_GRD') then  begin
     LidarFields[1] := 'ICESAT_GRD';
     LidarFields[2] := 'ICESAT_CAN';
     LidarLabels[1] := 'ICESat-2 ground';
     LidarLabels[2] := 'ICESat-2 canopy';
   end
   else begin
     LidarFields[1] := 'CLOUD_0_5';
     LidarFields[2] := 'CLOUD_995';
     LidarLabels[1] := 'Lidar ground';
     LidarLabels[2] := 'Lidar canopy';
   end;
   ns := 2;

   with GISdb[DBonTable] do for i := 0 to pred(MyData.FieldCount) do begin
     fName := MyData.GetFieldName(i);
     if (Copy(fName,1,2) = 'Z_')  then begin
        inc(ns);
        LidarFields[ns] := fName;
        LidarLabels[ns] := Copy(fName,3,Length(fName)-2);
     end;
   end;
end;


procedure GetFiltersList(DB : integer; Title : shortString; var Filters,Titles : tStringList);

     procedure AddOne(af,at : shortstring);
     begin
        Filters.Add(af);
        Titles.Add(at);
     end;

var
   FieldName : shortstring;
   PtsNeeded,j : integer;
   DataThere : tStringList;
begin
   Filters := tStringList.Create;
   Titles := tStringList.Create;
   if (Title = 'Slope') then begin
      AddOne('','All slopes');
      AddOne('SLP_COP >' + RealToString(MdDef.GeomorphSlopeCut[1],-12,-2), 'Slope <' + RealToString(MdDef.GeomorphSlopeCut[1],-12,-2) +'%');
      AddOne('SLP_COP >'+ RealToString(MdDef.GeomorphSlopeCut[2],-12,-2) + ' AND SLP_SRTM <=' + RealToString(MdDef.GeomorphSlopeCut[3],-12,-2),'Slope ' + RealToString(MdDef.GeomorphSlopeCut[2],-12,-2) + ' to ' + RealToString(MdDef.GeomorphSlopeCut[3],-12,-2) +'%' );
      AddOne('SLP_COP >'+ RealToString(MdDef.GeomorphSlopeCut[3],-12,-2) + ' AND SLP_SRTM <=' + RealToString(MdDef.GeomorphSlopeCut[4],-12,-2),'Slope ' + RealToString(MdDef.GeomorphSlopeCut[3],-12,-2) + ' to ' + RealToString(MdDef.GeomorphSlopeCut[4],-12,-2) +'%' );
      AddOne('SLP_COP >' + RealToString(MdDef.GeomorphSlopeCut[4],-12,-2), 'Slope >' + RealToString(MdDef.GeomorphSlopeCut[4],-12,-2) +'%');
   end
   else if (Title = 'Landcover') then begin
      AddOne('','All landcover');
      AddOne('COP_GLC100' + '>= 20 AND ' + 'COP_GLC100' + '<= 40','Low vegetation' );
      AddOne('COP_GLC100' + '>= 111 AND ' + 'COP_GLC100' + '<= 116','Closed forest' );
      AddOne('COP_GLC100' + '>= 121 AND ' + 'COP_GLC100' + '<= 126','Open forest' );
   end
   else if (Title = 'Canopy') then begin
      AddOne('','All canopy');
      if GISdb[DB].MyData.FieldExists('CLOUD_HT') then FieldName := 'CLOUD_HT' else FieldName := 'H_CANOPY';
      AddOne(FieldName +  '<= 2', 'Canopy < 2 m');
      AddOne(FieldName +  '> 2 AND ' + FieldName + '<= 5','Canopy 2-5 m' );
      AddOne(FieldName +  '> 5 AND ' + FieldName + '<= 10','Canopy 5-10 m' );
      AddOne(FieldName +  '> 10 AND ' + FieldName + '<= 20','Canopy 10-20 m' );
      AddOne(FieldName +  '> 20','Canopy > 20 m' );
   end
   else begin
      PtsNeeded := 1000;
      ReadDefault('Min number in category to plot',PtsNeeded);
      FieldName := GISdb[DB].PickField('field for graphs',[ftString,ftInteger,ftSmallInt]);
      GISdb[DB].EmpSource.Enabled := false;
      FindUniqueEntries(GISdb[DB].MyData,FieldName,DataThere);
      AddOne('','All');
      for j := 0 to pred(DataThere.Count) do begin
         AddOne(FieldName + '=' + DataThere[j],FieldName + '=' + DataThere[j]);
      end;
      DataThere.Free;
   end;
end;


procedure LandCoverSummary;
var
   Bitmap : tMyBitmap;
   Top : integer;
   Categories : tNLCDCats;
   CatUsed : array[1..MaxLandCoverCategories] of boolean;


      function DoOne(DB : integer) : integer;
      var
        LegendY,xstart,xend,i,j,k,total,cat : Integer;
        Cum  : float64;
        Count :  array[1..MaxLandCoverCategories] of int64;
      begin
         {$IfDef RecordGlobalDEM} WriteLineToDebugFile('DoOne in, db=' + GISdb[DB].dbName); {$EndIf}
         if GISdb[DB].MyData.FieldExists('COP_GLC100') then begin
            for i := 1 to MaxLandCoverCategories do begin
               Count[i] := 0;
            end;
            Total := 0;
            GISdb[DB].EmpSource.Enabled := false;
            GISdb[DB].MyData.First;
            while not GISdb[DB].MyData.Eof do begin
                if GISdb[DB].MyData.CarefullyGetFieldByNameAsInteger('COP_GLC100',Cat) then begin
                   if (Cat <> 200) then begin
                       inc(Count[Cat]);
                       inc(Total);
                   end;
                end;
                GISdb[DB].MyData.Next;
            end;
            xstart := 150;
            Cum := 0;
            for i := 1 to MaxLandCoverCategories do begin
               if (Count[i] > 0) then begin
                  CatUsed[i] := true;
                  Cum := Cum + Count[i] / Total;
                  xend := xStart + round(Count[i] / Total*500);
                  Bitmap.Canvas.Brush.Style := bsSolid;
                  Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(Categories[i].Color);
                  Bitmap.Canvas.Rectangle(xstart,Top,xend,Top + 45);
                  xstart := xend;
               end;
            end;

            Bitmap.Canvas.Brush.Style := bsClear;
            Bitmap.Canvas.Font.Size := 14;
            Bitmap.Canvas.TextOut(5,Top+5,RemoveUnderScores(GISdb[DB].DBName));

            Top := Top + 50;
            GISdb[DB].ShowStatus;
         end
         else begin
            {$IfDef RecordGlobalDEM} WriteLineToDebugFile('Fail, no land cover in, ' + GISdb[DB].DBName); {$EndIf}
         end;
      end;


var
   i,j,LegendY : integer;
begin
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('LandCoverSummary in'); {$EndIf}
   if (GetCompareNames = 0) then begin
      exit;
   end;
   for j := 1 to MaxLandCoverCategories do begin
      CatUsed[j] := false;
   end;
   SetUpNLCDCategories(false,euSent2SLC,Categories);
   CreateBitmap(Bitmap,1600,1850);
   Top := 0;
   for j := 1 to MaxDataBase do if ValidDB(j) then begin
      DoOne(j);
   end;
   LegendY := 0;
   for i := 1 to MaxLandCoverCategories do begin
      if CatUsed[i] then begin
         Bitmap.Canvas.Brush.Style := bsSolid;
         Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(Categories[i].Color);
         Bitmap.Canvas.Rectangle(700,LegendY,730,LegendY + 24);
         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.Font.Size := 12;
         Bitmap.Canvas.TextOut(735,LegendY+5,Categories[i].LongName);
         LegendY := LegendY + 24;
      end;
   end;
   DisplayBitmap(Bitmap,'Land cover distribution');
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('LandCoverSummary out'); {$EndIf}
end;


procedure ElevationSlopePlotCompareDEMs;
var
   BinSize : integer;
   sl : tStringList;
   fName : PathStr;


      function GraphPlot(DB : integer) : TThisBaseGraph;
      var
         i,j,Bin,n : integer;
         z,slp : float64;
         rfile :  array[1..MaxCompare] of file;
         v       : tGraphPoint32;
         ElevBinCount : array[1..MaxCompare,-100..5000] of integer;
         BinSlope  : array[1..MaxCompare,-100..5000] of float64;
      begin
          {$IfDef RecordGlobalDEM} WriteLineToDebugFile('GraphPlot, db=' + IntToStr(DB) + '  recs=' + IntToStr(GISdb[DB].MyData.RecordCount)); {$EndIf}
          Result := TThisBaseGraph.Create(Application);
          Result.GraphDraw.VertLabel := 'Elev (m)';
          Result.GraphDraw.HorizLabel := RemoveUnderScores(GISDB[DB].dbName) + '  Avg Slope ' + ' (%)  n=' + IntToStr(GISdb[DB].MyData.RecordCount);

          Result.GraphDraw.MinHorizAxis := 0.0;
          Result.GraphDraw.MaxHorizAxis := 250;
          Result.GraphDraw.MinVertAxis := 6000;
          Result.GraphDraw.MaxVertAxis := -500;
          Result.SetUpGraphForm;
          //Result.GraphDraw.LegendList := tStringList.Create;
          ShowHourglassCursor;
           for i := 1 to MaxCompare do begin
              for Bin := -100 to 5000 do begin
                 ElevBinCount[i,Bin] := 0;
                 BinSlope[i,Bin] := 0;
              end;
           end;
           BinSize := 10;
          {$IfDef RecordGlobalDEM} WriteLineToDebugFile('Init done'); {$EndIf}
            StartProgress('Get slopes and elevations');
            GISdb[DB].MyData.First;
            j := 0;

            while not GISdb[DB].MyData.Eof do begin
               if j mod 1000 = 0 then UpdateProgressBar(j/GISdb[DB].MyData.FiltRecsInDB);
               inc(j);
               for i := 1 to MaxCompare do begin
                  if (DEM_Manager.CompareDEMNames[i] <> '') then begin
                     if GISdb[DB].MyData.CarefullyGetFieldByNameAsFloat64('Z_' + DEM_Manager.CompareDEMNames[i],z) and
                         GISdb[DB].MyData.CarefullyGetFieldByNameAsFloat64('SLP_' + DEM_Manager.CompareDEMNames[i],slp) then begin
                             Bin := BinSize * round(z/BinSize);
                             inc(ElevBinCount[i,Bin]);
                             BinSlope[i,Bin] := BinSlope[i,Bin] + slp;
                      end;
                   end;
                end;
                GISdb[DB].MyData.Next;
            end;
          {$IfDef RecordGlobalDEM} WriteLineToDebugFile('Data loaded'); {$EndIf}

         for i := 0 to pred(MaxCompare) do begin
            if (DEM_Manager.CompareDEMNames[i] <> '') then begin
               //Result.GraphDraw.LegendList.Add(DEM_Manager.CompareDEMNames[i]);
               Result.OpenDataFile(rfile[i],DEM_Manager.CompareDEMNames[i]);
               Result.GraphDraw.LineSize256[i] := 1;
               Result.GraphDraw.ShowLine[i] := true;
               Result.GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors(i));
               for Bin := -100 to 5000 do begin
                   if ElevBinCount[i,Bin] > 0 then begin
                      if Bin > Result.GraphDraw.MaxVertAxis then Result.GraphDraw.MaxVertAxis := Bin;
                      if Bin < Result.GraphDraw.MinVertAxis then Result.GraphDraw.MinVertAxis := Bin;
                      v[1] := BinSlope[i,Bin] / ElevBinCount[i,Bin];
                      v[2] := Bin;
                      BlockWrite(rfile[i],v,1);
                   end;
               end;
               CloseFile(Rfile[i]);
            end;
         end;
         EndProgress;
         {$IfDef RecordGlobalDEM} WriteLineToDebugFile('Files written'); {$EndIf}
         Result.RedrawDiagram11Click(Nil);
         fName := Petmar.NextFileNumber(MDTempDir,'elev_Slope_','.bmp');
         SaveImageAsBMP(Result.Image1,fName);
         sl.Add(fName);
         {$IfDef RecordGlobalDEM} WriteLineToDebugFile('GraphPlot out, db=' + IntToStr(DB)); {$EndIf}
      end;


var
   j : integer;
   Graph : TThisBaseGraph;
   bmp : tMyBitmap;
begin {proc ElevationSlopePlot}
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('ElevationSlopePlotCompareDEMs in'); {$EndIf}
   if (GetCompareNames = 0) then begin
      exit;
   end;
   sl := tStringList.Create;
   for j := 1 to MaxDataBase do if ValidDB(j) then begin
      Graph := GraphPlot(j);
   end;
   bmp := Graph.MakeLegend;
   fName := Petmar.NextFileNumber(MDTempDir,'elev_Slope_','.bmp');
   Bmp.saveToFile(fName);
   BMP.free;
   sl.Add(fName);
   MakeBigBitmap(sl,'');

  ShowDefaultCursor;
  {$IfDef RecordGlobalDEM} WriteLineToDebugFile('ElevationSlopePlotCompareDEMs out'); {$EndIf}
end;



function FiveSeriesGraph(DB : integer; Lat,Long,Tolerance : float64; DirField : shortstring) : TThisbasegraph;
// if lat/long not defined, graph uses a filter set before calling, to get a desired profile along ICESat profile
// if lat/long defined, show an EW or NS profile through the lidar point cloud

      function OnHorizontalDatum(d :  integer) : TThisbasegraph;
      var
        i,j,k{,xLen,OnFile} : integer;
        //TStr : ShortString;
        offset : float32;
        OK : boolean;
        rFiles : array[1..5] of file;
        v : tGraphPoint32;
      begin
         with GISdb[db] do begin
           Result := TThisbasegraph.Create(Application);
           Result.SetUpGraphForm;
           GetFieldsAndLabels(DB);
           EmpSource.Enabled := false;
           Result.Symbol.DrawingSymbol := dbOpts.Symbol.DrawingSymbol;
           for i := 1 to ns do begin
              case i of
                  1 : Result.Symbol.Color := ConvertTColorToPlatformColor(clBrown);
                  2 : Result.Symbol.Color := claGreen;
                  else Result.Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors(i));
              end;
              if i in [1,2] then Result.Symbol.Size := 3
              else Result.Symbol.Size := 2;
              Result.OpenPointSymbolFile(rfiles[i],LidarLabels[i],Result.Symbol);
           end;

           //Result.GraphDraw.LegendList := tStringList.Create;
           //for i := 1 to ns do Result.GraphDraw.LegendList.Add(LidarLabels[i]);

           if (DirField = LatFieldName) then begin
              Result.GraphDraw.HorizLabel := 'Latitude';
           end
           else begin
              Result.GraphDraw.HorizLabel := 'Longitude';
           end;
           Result.GraphDraw.LLcornerText := GISdb[db].dbName + ' ' + MyData.Filter + '  n=' + IntToStr(MyData.FiltRecsInDB);
           if d = 0 then Result.GraphDraw.VertLabel := 'Elevation (m)'
           else if d = 1 then Result.GraphDraw.VertLabel := 'Ground level (m)'
           else if d = 2 then Result.GraphDraw.VertLabel := 'Top Canopy (m)';
           Result.Caption := Result.GraphDraw.LLcornerText;
           MyData.First;
           while not MyData.eof do begin
              if GetFloat32FromTableLinkPossible(DirField,v[1]) then begin
                  OK := true;
                  if (d = 0) then Offset := 0
                  else begin
                      OK := GetFloat32FromTableLinkPossible(LidarFields[d],Offset);
                  end;
                  if OK then for i := 1 to ns do begin
                     if GetFloat32FromTableLinkPossible(LidarFields[i],v[2]) then begin
                        v[2] := v[2] - Offset;
                        BlockWrite(Rfiles[i],v,1);
                     end;
                  end;
              end;
              MyData.Next;
           end;
           {$IfDef RecordProblems} WriteLineToDebugFile('FiveSeriesGraph, recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}
           for i := 1 to ns do Result.ClosePointDataFile(rfiles[i]);
           Result.AutoScaleAndRedrawDiagram;
           EmpSource.Enabled := true;
         end;
      end;

var
   d : integer;
   graph : array[0..0] of TThisbasegraph;
begin
   OnHorizontalDatum(0);
   Result := graph[0];
   GISdb[db].LastGraph := Result;
end;


const
  PtsNeeded : integer = 1;


{$IfDef ExcludeDirtAirShots}
{$Else}
procedure DirtAndAirShots(DB : integer; Title : shortString);
var
   s1,Filters,Titles : tStringList;
   j : integer;
   TheFilter : shortstring;

      function DoOne(DBonTable : integer) : integer;
      var
        Above,Below,Within : array[1..MaxCompare] of integer;
        fAbove,fBelow,fWithin : array[1..MaxCompare] of float64;
        z : array[1..MaxCompare] of float64;
        BaseZ,TopZ : float64;
        Output : tStringList;
        i,j,k,total,l1,l2,l3 : Integer;
        Bitmap : tMyBitmap;
        fName : PathStr;
      begin
         {$IfDef RecordGlobalDEM} WriteLineToDebugFile('DirtAndAirShots DoOne=' + GISdb[DBonTable].dbName); {$EndIf}
         ShowHourglassCursor;
         GISdb[DBonTable].ApplyGISFilter(TheFilter);
         Title := RemoveUnderScores(GISdb[DBonTable].dbName) + ' n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB);
         GetFieldsAndLabels(DBonTable);

         for i := 1 to MaxCompare do begin
           Above[i] := 0;
           Within[i] := 0;
           Below[i] := 0;
         end;
         Total := 0;

         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.Eof do begin
             if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(LidarFields[1],BaseZ) and GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(LidarFields[2],TopZ) then begin
                for i := 1 to MaxCompare do begin
                   if (DEM_Manager.CompareDEMNames[i] <> '') and GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64('Z_' + DEM_Manager.CompareDEMNames[i],z[i]) then begin
                      if z[i] < BaseZ-MDDef.CloudFilterTolerance then inc(Below[i])
                      else if z[i] > TopZ+MDDef.CloudFilterTolerance then inc(Above[i])
                      else inc(WithIn[i]);
                   end;
                end;
                inc(Total);
             end;
             GISdb[DBonTable].MyData.Next;
         end;
         {$IfDef RecordGlobalDEM} WriteLineToDebugFile('Found numbers'); {$EndIf}

         GISdb[DBonTable].ShowStatus;
         Result := 0;
         if (Total > PtsNeeded) then begin
            j := 0;
            for i := 1 to MaxCompare do begin
               if (DEM_Manager.CompareDEMNames[i] <> '') then begin
                  fBelow[i] := 100*Below[i]/Total;
                  fWithIn[i] := 100*WithIn[i]/Total;
                  fAbove[i] := 100*Above[i]/Total;
                  inc(j);
               end;
            end;
            CreateBitmap(Bitmap,10 + j * 50,240);
            Bitmap.Canvas.Font.Style := [fsBold];
            Bitmap.Canvas.Font.Size := 10;
            Bitmap.Canvas.TextOut(5,215,Title);
            for i := 1 to MaxCompare do begin
               if (DEM_Manager.CompareDEMNames[i] <> '') then begin
                  j := pred(i) * 50;
                  k := j + 45;
                  Bitmap.Canvas.Brush.Style := bsSolid;
                  Bitmap.Canvas.Brush.Color := clBlue;
                  l1 := round(2 * fAbove[i]);
                  Bitmap.Canvas.Rectangle(j,0,k,l1);
                  l2 := round(2 * (fAbove[i] + fWithin[i]));
                  case i of
                     1 : Bitmap.Canvas.Brush.Style := bsCross;
                     2 : Bitmap.Canvas.Brush.Style := bsDiagCross;
                     3 : Bitmap.Canvas.Brush.Style := bsVertical;
                     4 : Bitmap.Canvas.Brush.Style := bsFDiagonal;
                     5 : Bitmap.Canvas.Brush.Style := bsBDiagonal;
                  end;
                  Bitmap.Canvas.Brush.Color := clLime;
                  Bitmap.Canvas.Rectangle(j,l1,k,l2);

                  Bitmap.Canvas.Brush.Style := bsSolid;
                  Bitmap.Canvas.Brush.Color := clBrown;
                  Bitmap.Canvas.Rectangle(j,l2,k,200);
                  Bitmap.Canvas.Brush.Style := bsClear;
                  Bitmap.Canvas.TextOut(j+5,200,DEM_Manager.CompareDEMNames[i]);
               end;
            end;
            if (s1 <> Nil) then begin
               fName := Petmar.NextFileNumber(MDTempDir,'hist_series_','.bmp');
               Bitmap.SaveToFile(fName);
               Bitmap.Destroy;
               s1.Add(fName);
            end
            else DisplayBitmap(Bitmap,Title);
            result := total;
            GISdb[DBonTable].ClearGISFilter;
            GISdb[DBonTable].ShowStatus;
            {$IfDef RecordGlobalDEM} WriteLineToDebugFile('DoOne out'); {$EndIf}
         end;
      end;

      function DoFilter(aFilter : shortstring; aTitle : shortstring = '') : integer;
      begin
         TheFilter := aFilter;
         if aTitle = '' then Title := aFilter
         else Title := atitle;
         Result := DoOne(DB);
      end;

      procedure MakeLegend;
      var
         Bitmap : tMyBitmap;
         fName : PathStr;
      begin
         CreateBitmap(Bitmap,400,400);
         Bitmap.Canvas.Brush.Style := bsSolid;
         Bitmap.Canvas.Brush.Color := clBlue;
         Bitmap.Canvas.Rectangle(5,5,35,35);

         Bitmap.Canvas.Brush.Color := clLime;
         Bitmap.Canvas.Rectangle(5,40,35,70);

         Bitmap.Canvas.Brush.Color := clBrown;
         Bitmap.Canvas.Rectangle(5,75,35,105);

         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.Font.Size := 13;
         Bitmap.Canvas.TextOut(38,15,'DEM point above point cloud');
         Bitmap.Canvas.TextOut(38,50,'DEM point within point cloud');
         Bitmap.Canvas.TextOut(38,85,'DEM point below point cloud');

         fName := Petmar.NextFileNumber(MDTempDir,'hist_series_','.bmp');
         Bitmap.SaveToFile(fName);
         Bitmap.Destroy;
         s1.Add(fName);
      end;

var
   DataThere : tStringList;
   FieldName,SaveTitle : ShortString;
   TStr : string;
   i : integer;
begin
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('DirtAndAirShots in'); {$EndIf}
   s1 := nil;
   if GetCompareNames = 0 then begin
      exit;
   end;

   ReadDefault('Cloud tolerance (m)',MDDef.CloudFilterTolerance);
   TheFilter := '';
   if (DB=0) then begin
      TheFilter := GetCloudHeightFilter;
      s1 := tStringList.Create;
      for j := 1 to MaxDataBase do if ValidDB(j) then begin
         TStr := GISdb[j].DBname;
         Title := BeforeSpecifiedCharacter(TStr,'_');
         DoOne(j);
      end;
      if (s1.Count > 0) then begin
         MakeLegend;
         MakeBigBitmap(s1,'Dirt and air shots, tolerance=' + RealToString(MDDef.CloudFilterTolerance,-6,-2) + ' m');
      end;
   end
   else begin
      if (Title = '') then begin
         DoOne(DB);
      end
      else begin
         s1 := tStringList.Create;
         SaveTitle := Title;
         GetFiltersList(DB,Title,Filters,Titles);
         for i := 0 to pred(Filters.Count) do begin
            wmDEM.SetPanelText(0,IntToStr(succ(i)) + '/' + IntToStr(DataThere.Count) + ' categories');
            DoFilter(Filters[i],Titles[i]);
         end;
         Filters.Free;
         Titles.Free;
         MakeBigBitmap(s1,GISdb[DB].dbName + '  ' + SaveTitle + ', tolerance=' + RealToString(MDDef.CloudFilterTolerance,-8,-2));
      end;
   end;
   wmDEM.SetPanelText(0,'');
   ShowDefaultCursor;
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('DirtAndAirShots out'); {$EndIf}
end;

{$EndIf}



procedure HistogramPointCloudAndGlobalDEMs(DB : integer = 0; Title : shortString = '');
var
   g2 : tThisBaseGraph;
   s2 : tStringList;
   LegendFName : PathStr;
   j : integer;
   DoBigBitmap : boolean;
   TheFilter : shortstring;
   Min,Max : float64;

      procedure DoOne(theDB : integer; TheFilter,TheTitle : shortstring);
      var
         fName : PathStr;
         TStr : string;
         i : integer;
         bmp : tMyBitmap;
         HistFields : tStringList;
      begin
         {$IfDef RecordGlobalDEM} WriteLineToDebugFile('DoOne=' + GISdb[theDB].dbName); {$EndIf}
         TStr := GISdb[thedb].dbName;
         GISdb[theDB].ApplyGISFilter(TheFilter);
         if (TheTitle = '') then TheTitle := BeforeSpecifiedCharacter(TStr,'_');
         if (GISdb[theDB].MyData.FiltRecsInDB > PtsNeeded) then begin
            MDDef.FlipHistogram := true;
            HistFields := tStringList.Create;
            for i := 1 to MaxCompare do begin
               if (DEM_Manager.CompareDEMNames[i] <> '') then begin
                  fname := 'FR_' + DEM_Manager.CompareDEMNames[i];
                  if GISdb[theDB].MyData.FieldExists(fName) then HistFields.Add(fName);
               end;
            end;
            if HistFields.Count > 0 then begin
               g2 := GISdb[theDB].CreateHistogramFromDatabase(true,HistFields,false);  //,false,-2,2,0.05);
               HistFields.Destroy;
               g2.GraphDraw.VertLabel := 'Fraction';
               g2.GraphDraw.HorizLabel := TheTitle + '  ' + RemoveUnderscores(GISdb[thedb].dbName) + ' (n=' + IntToStr(GISdb[theDB].MyData.FiltRecsInDB) + ')';
               g2.HighlightBox := true;
               g2.RedrawDiagram11Click(Nil);
               if DoBigBitmap then begin
                  fName := Petmar.NextFileNumber(MDTempDir,'hist_series_','.bmp');
                  SaveImageAsBMP(g2.Image1,fName);
                  s2.Add(fName);
                  if (LegendFName = '') then begin
                     bmp := g2.MakeLegend;
                     LegendFName := Petmar.NextFileNumber(MDTempDir,'legend_','.bmp');
                     SaveBitmap(bmp,LegendFName);
                     bmp.Free;
                  end;
                  g2.Destroy;
               end;
            end;
         end;
      end;

var
  Filters,Titles : tStringList;
   i : integer;
begin
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('HistogramPointCloudAndGlobalDEMs in'); {$EndIf}
   if (GetCompareNames = 0) then begin
      exit;
   end;
   DoBigBitmap := (DB=0);
   LegendFName := '';
   s2 := tStringList.Create;
   if (DB=0) then begin
      if (LidarFields[1] = 'ICESAT_GRD') then TheFilter := ''
      else TheFilter := GetCloudHeightFilter;
      {$IfDef RecordGlobalDEM} WriteLineToDebugFile('HistogramPointCloudAndGlobalDEMs, TheFilter=' + TheFilter); {$EndIf}
      for i := 1 to MaxDataBase do if ValidDB(i) then begin
         DoOne(i,TheFilter,GISdb[i].dbName);
      end;
   end
   else begin
      if (Title = '') then begin
         DoOne(DB,'','');
      end
      else begin
         GetFiltersList(DB,Title,Filters,Titles);
         DoBigBitmap := true;
         for i := 0 to pred(Filters.Count) do begin
            wmdem.SetPanelText(0,IntToStr(succ(i)) + '/' + IntToStr(Filters.Count) + ' categories');
            DoOne(DB,Filters[i],Titles[i]);
         end;
         Filters.Free;
         Titles.Free;
      end;
   end;
   wmdem.SetPanelText(0,'');
   if DoBigBitmap then begin
      if (LegendFName <> '') then s2.Add(LegendFName);
      MakeBigBitmap(s2,'Position of DEM grid elevation within normalized point cloud');
   end;
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('HistogramPointCloudAndGlobalDEMs out'); {$EndIf}
end;


procedure CloudSummaryGlobalDEMs(DB : integer);
var
   s1 : tStringList;
   j : integer;
   Min,Max : float64;
   fName : PathStr;

      procedure DoOne(theDB : integer);
      var
         TStr : ShortString;
         TStr2 : string;
         MinLat,MinLong,MaxLat,MaxLong,
         SRTMmin,SRTMmax,
         MaxZ,MinZ : float64;
         Moment1,Moment2,Moment3 : tMomentVar;
      begin
         ShowHourglassCursor;
         GISdb[thedb].EmpSource.Enabled := false;
         if (DB=0) then begin
            TStr2 := GISdb[thedb].dbName;
            TStr2 := BeforeSpecifiedCharacter(TStr2,'_')
         end
         else begin
            TStr2 := GISdb[theDB].MyData.Filter;
         end;
         MaxZ := GISdb[thedb].MyData.FindFieldMax(LidarFields[2]);
         MinZ := GISdb[thedb].MyData.FindFieldMin(LidarFields[1]);

         GISdb[thedb].MyData.FindFieldRange('LAT',MinLat,MaxLat);
         GISdb[thedb].MyData.FindFieldRange('LONG',MinLong,MaxLong);
         GISdb[thedb].MyData.FindFieldRange('SRTM',SRTMMin,SRTMMax);
         Moment1 := GISdb[thedb].GetFieldStatistics('SLP_SRTM');
         Moment2 := GISdb[thedb].GetFieldStatistics('CLOUD_HT');
         Moment3 := GISdb[thedb].GetFieldStatistics('CLOUD_PTS');
         Tstr := TStr2 + ',' + LatLongDegreeToString( 0.5 * (MaxLat+MinLat),0.5 * (MaxLong+MinLong),VeryShortDegrees)  +  ',' +
            IntToStr(GISdb[thedb].MyData.FiltRecsInDB)  +  ',' +  RealToString(Moment3.mean,-8,0)   +  ',' +
            RealToString(MinZ,-8,0) +  ' to ' + RealToString(MaxZ,-8,0) +  ',' +
            RealToString(SRTMmin,-8,0) +  ' to ' + RealToString(SRTMMax,-8,0) +  ',' +
            RealToString(Moment1.mean,-8,2) + '±' + RealToString(Moment1.std_dev,-8,2) +  ',' +
            RealToString(Moment2.mean,-8,2) + '±' + RealToString(Moment2.std_dev,-8,2);
         s1.Add(TStr);
         GISdb[thedb].EmpSource.Enabled := true;
      end;

var
   DataThere : tStringList;
   FieldName : ShortString;
begin

MessageToContinue('need to deal with all 5 dems');
exit;

   GetFieldsAndLabels(DB);
   s1 := tStringList.Create;
   s1.add('AREA,LOCATION,CELLS,PTS_CELL,CLD_ELEVS,SRTM_ELEVS,SLOPE_PC,CANOPY_M');
   if (DB = 0) then begin
      for j := 1 to MaxDataBase do if (GISdb[j] <> Nil) then begin
         DoOne(j);
      end;
   end
   else begin
      FieldName := GISdb[DB].PickField('field for graphs',[ftString,ftInteger,ftSmallInt]);
      GISdb[DB].EmpSource.Enabled := false;
      FindUniqueEntries(GISdb[DB].MyData,FieldName,DataThere);
      for j := 0 to pred(DataThere.Count) do begin
         GISdb[DB].MyData.ApplyFilter(FieldName + '=' + DataThere[j]);
         DoOne(DB);
      end;
      GISdb[DB].ClearGISFilter;
   end;
   fname := NextFileNumber(MDTempDir, 'cloud_stats_','.csv');
   StringList2CSVtoDB(S1,fName,true);
   if (gisdb[1].TheMapOwner <> Nil) then gisdb[1].TheMapOwner.LoadDataBaseFile(fName);
   ShowDefaultCursor;
end;

{$IfDef ExIceSat}
{$Else}
procedure IcesatPhotonConvert(Memo1 : tMemo);
const
   BeamName : array[0..6] of string10 = ('None','GT1R','GT1L','GT2R','GT2L','GT3R','GT3L');
var
   DefaultFilter : byte;
   outCSV,csv,FilesWanted  : tStringList;
   Table1 : tMyData;
   fName,OutName : PathStr;
   GeoidGrid,i,j,conf : integer;
   TStr : string;
   Beam,ts : shortstring;
   NewLas : tCreateLasFile;
   NewLasProjection : tlasProjectionDefinition;
   Lat,Long,z : float64;
   lp1 : tLidarPointType1;
   DateStr : shortstring;
   Year,Month,Day : integer;
   myDateTime : TDateTime;

      procedure CheckAndAddShot;
      var
         zd : float32;
      begin
         if lp1.lp0.Classification in [23,24] then begin
            lp1.lp0.xPt := round( (Long - NewLAS.NewLasHeader.Xoffset) / NewLAS.NewLasHeader.XScaleFac);
            lp1.lp0.yPt := round( (Lat - NewLAS.NewLasHeader.Yoffset) / NewLAS.NewLasHeader.YScaleFac);
            if DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,zd) then z := z + zd;
            lp1.lp0.zPt := round( (z - NewLAS.NewLasHeader.Zoffset) / NewLAS.NewLasHeader.ZScaleFac);
            NewLAS.AddShotToOutputBuffer(lp1);
            outcsv.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + RealToString(z,-12,-2) + ',' + DateStr + ',' + BeamName[lp1.lp0.UserData] + ',' + IntToStr(conf));
         end;
      end;

begin
   GetIcesatFilter;
   FilesWanted := tStringList.Create;
   FilesWanted.Add('G:\sheep_range_icesat2_photons\track_1263');
   DefaultFilter := 0;
   Memo1.Visible := true;
   if GetMultipleFiles('IceSat-2 point cloud data','Any file|*.dbf;*.csv',FilesWanted,DefaultFilter) then begin
      ZeroLidarPointType0(lp1.lp0);
      GeoidGrid := OpenNewDEM(Geoid2008FName,false);
      DEMGlb[GeoidGrid].MultiplyGridByConstant(-1);

      NewLas := tCreateLasFile.Create;
      NewLasProjection.LASProj := tMapProjection.Create('new las');
      NewLasProjection.LASProj.ModelType := LasLatLong;
      outName := Petmar.NextFileNumber(ExtractFilePath(FilesWanted.Strings[i]),'icesat2_export_','.las');
      NewLAS.CreateNewLASfile(outName,NewLasProjection,1);

      StartProgress('Icesat-2 to LAS');
      for i := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         Memo1.Lines.Add(TimeToStr(Now) + '  ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count) + '  ' + fName);
         lp1.lp0.PointSourceID := StrToInt(GetFieldFromXMLMetadata(ChangeFileExt(fName,'.xml'),'trackid'));
         Beam := GetFieldFromXMLMetadata(ChangeFileExt(fName,'.xml'),'beam');
         lp1.lp0.UserData := 0;
         for j := 1 to 6 do
            if (Beam = BeamName[j]) then lp1.lp0.UserData := j;

         if MDDef.icesat2.UseBeam[lp1.lp0.UserData] then begin
            StartProgress('Icesat-2 to LAS, ' + Beam);
            lp1.lp0.ReturnFlags := 9;   //Return 1 of 1
            ts := ExtractFileNameNoExt(fName);
            Year := StrToInt(Copy(ts,8,4));
            Month := StrToInt(Copy(ts,13,2));
            Day := StrToInt(Copy(ts,16,2));
            DateStr := AddDayMonthLeadingZero(Month) + '/' + AddDayMonthLeadingZero(Day) + '/' + IntToStr(Year);
            myDateTime := StrToDateTime(DateStr + '   12');
            lp1.GPSTime := DateTimeToGPSTime(myDateTime);

            if UpperCase(ExtractFileExt(fName)) = '.DBF' then begin
               j := 0;
               Table1 := tMyData.Create(fName);
               while not Table1.eof do begin
                  inc(j);
                  if (j mod 500 = 0) then UpdateProgressBar(j/Table1.FiltRecsInDB);
                  if Table1.ValidLatLongFromTable(Lat,Long) then begin
                     Conf := Table1.GetFieldByNameAsInteger('CONFIDENCE');
                     if (Conf >= MDDef.icesat2.BeamConfidence[lp1.lp0.UserData]) then begin
                        lp1.lp0.Classification := 20 + conf;
                        z := Table1.GetFieldByNameAsFloat('HEIGHT');
                        CheckAndAddShot;
                     end;
                  end;
                  Table1.Next;
               end;
               Table1.Destroy;
            end
            else begin
               csv := tStringList.Create;
               csv.LoadFromFile(fName);
               outCSV := tStringList.Create;
               Outcsv.add('LAT,LONG,HEIGHT,DATE,BEAM,CONFIDENCE');
               for j := 1 to pred(CSV.Count) do begin
                   if (j mod 500 = 0) then UpdateProgressBar(j/csv.Count);
                   Tstr := csv.Strings[j];
                   Lat := StrToFloat(Petmar_types.BeforeSpecifiedCharacter(Tstr,',',true,true));
                   Long := StrToFloat(Petmar_types.BeforeSpecifiedCharacter(Tstr,',',true,true));
                   z := StrToFloat(Petmar_types.BeforeSpecifiedCharacter(Tstr,',',true,true));
                   Conf := StrToInt(Tstr);
                   if (Conf >= MDDef.icesat2.BeamConfidence[lp1.lp0.UserData]) then begin
                      lp1.lp0.Classification := 20 + conf;
                      CheckAndAddShot;
                   end;
               end;
//photon_2019-11-01_t547_1582843538009.csv
               FName := ExtractFilePath(fName) + copy(extractFileNameNoExt(fName),1,23) + Beam + '.csv';
               outcsv.SaveToFile(fName);
               outcsv.Destroy;
               csv.Destroy;
            end;
         end;
     end;
     NewLas.Destroy;
     Memo1.Lines.Add(TimeToStr(Now) + '  complete');
     CloseSingleDEM(GeoidGrid);
   end;
   FilesWanted.Free;
   EndProgress;
end;


procedure IcesatProcessCanopy(dbOnTable : integer; AddDEMs : boolean; LimitDecimals : boolean = false);

var
   i,j,rc,Wait,geoidGrid : integer;
   zgeoid,zsrtm,z,zh : float32;
   Lat,Long : float64;
   TStr : shortstring;
   fName : PathStr;
   MapOwner : tMapForm;


      procedure Fraction(z,zh,zdem : float64; fName : shortstring);
      var
         f : float64;
      begin
         if abs(zh - z) < 0.01 then f := -9999
         else f := (zdem - z) / (zh - z);
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(fName,f);
      end;


begin
   {$IfDef RecordIceSat} WriteLineToDebugFile('IcesatProcessCanopy in'); {$EndIf}
   if ValidDB(dbOnTable) then begin
      GeoidGrid := OpenNewDEM(Geoid2008FName,false,'ellipsoid/geoid shift');
      if (GeoidGrid = 0) then begin
         MessageToContinue('Ellipsoid to Geoid grid required');
         exit;
      end;
      if not FileExists(ChangeFileExt(GISdb[DBonTable].DBfullName,'.shx')) then GISdb[DBonTable].SavePointShapeFile(false);    //create SHX file

      if LimitDecimals then begin
         GISdb[DBonTable].LimitFieldDecimals('H_TE_BEST_',2);
         GISdb[DBonTable].LimitFieldDecimals('H_TE_UNCER',2);
         GISdb[DBonTable].LimitFieldDecimals('H_CANOPY',2);
         GISdb[DBonTable].LimitFieldDecimals('H_CANOPY_U',2);
         {$IfDef RecordIceSat} WriteLineToDebugFile('Decimals limited'); {$EndIf}
      end;

      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EGM2008_GD',9,2);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'ICESAT_GRD',9,2);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'ICESAT_CAN',9,2);
      GISdb[DBonTable].AddFieldToDataBase(ftString,'DATE',8);
      {$IfDef RecordIceSat} WriteLineToDebugFile('Fields added'); {$EndIf}

      GISdb[DBonTable].MyData.First;
      StartProgress('Icesat-2 elevs');
      i := 0;
      rc := GISdb[DBonTable].MyData.FiltRecsInDB;
      Wait := ProgressIncrement(rc);
      while not GISdb[DBonTable].MyData.eof do begin
         if (i mod wait = 0) then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            UpdateProgressBar(i/rc);
         end;
         inc(i);
         if GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long) then begin
            if DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,zgeoid) then begin
               GISdb[DBonTable].MyData.Edit;
               if GISdb[DBonTable].MyData.FieldExists('FILE_NAME') then begin
                  TStr := GISdb[DBonTable].MyData.GetFieldByNameAsSTRING('FILE_NAME');
                  GISdb[DBonTable].MyData.SetFieldByNameAsString('DATE',Copy(TStr,17,8));   //extract date from file name
               end;
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('EGM2008_GD',zgeoid);
               if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat32('H_TE_BEST_',z) then begin
                  z := z - zgeoid;
                  GISdb[DBonTable].MyData.SetFieldByNameAsFloat('ICESAT_GRD',z);
                  if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat32('H_CANOPY',zh) then begin
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat('ICESAT_CAN',z+zh);
                  end;
               end;
            end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
     {$IfDef RecordIceSat} WriteLineToDebugFile('IcesatProcessCanopy start cleanup'); {$EndIf}
      CloseSingleDEM(GeoidGrid);
      //GISdb[DBonTable].MyData.DeleteField('FILE_NAME');
      GISdb[DBonTable].MyData.DeleteField('SEG_BEGIN');
      GISdb[DBonTable].MyData.DeleteField('SEG_END');
      {$IfDef RecordIceSat} WriteLineToDebugFile('Fields deleted'); {$EndIf}
      if AddDEMs then AddGlobalDEMs(dbOnTable);

      fName := GISdb[DBonTable].DBFullName;
      Mapowner := GISdb[DBonTable].theMapOwner;
      {$IfDef RecordIceSat} WriteLineToDebugFile('IcesatProcessCanopy out'); {$EndIf}
   end
   else begin
      {$IfDef RecordIceSat} WriteLineToDebugFile('IcesatProcessCanopy invalid DB=' + IntToStr(dbOnTable)); {$EndIf}
   end;
end;


procedure AddEGMfields(dbOnTable : integer);
var
   i,j,rc,Wait,geoid2008,geoid1996,geoidDiff : integer;
   Lat,Long: float64;
   zgeoid : float32;
begin
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddEGMfields in'); {$EndIf}
      //GetGeoid;
      Geoid2008 := OpenNewDEM(Geoid2008FName,false,'geoid/ellipsoid shift');
      Geoid1996 := OpenNewDEM(Geoid96FName,false,'geoid/ellipsoid shift');
      GeoidDiff := OpenNewDEM(GeoidDiffFName,false,'geoid/ellipsoid shift');

      if (Geoid2008 = 0) then begin
         MessageToContinue('Geoid to ellipsoid grid required');
         exit;
      end;

      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EGM2008_GD',9,2);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EGM96_GD',9,2);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EGM_DIFF',9,2);

      GISdb[DBonTable].MyData.First;
      StartProgress('EGM fields');
      i := 0;
      rc := GISdb[DBonTable].MyData.FiltRecsInDB;
      Wait := ProgressIncrement(rc);
      while not GISdb[DBonTable].MyData.eof do begin
         if (i mod wait = 0) then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            UpdateProgressBar(i/rc);
         end;
         inc(i);
         if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         GISdb[DBonTable].MyData.Edit;
            if DEMGlb[Geoid2008].GetElevFromLatLongDegree(Lat,Long,zgeoid) then GISdb[DBonTable].MyData.SetFieldByNameAsFloat('EGM2008_GRD',zgeoid);
            if DEMGlb[Geoid1996].GetElevFromLatLongDegree(Lat,Long,zgeoid) then GISdb[DBonTable].MyData.SetFieldByNameAsFloat('EGM96_GRD',zgeoid);
            if DEMGlb[GeoidDiff].GetElevFromLatLongDegree(Lat,Long,zgeoid) then GISdb[DBonTable].MyData.SetFieldByNameAsFloat('EGM_DIFF',zgeoid);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      CloseSingleDEM(Geoid2008);
      CloseSingleDEM(Geoid1996);
      CloseSingleDEM(GeoidDiff);
      GISdb[dbOnTable].ShowStatus;
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddEGMfields out'); {$EndIf}
end;


procedure AddGlobalDEMs(dbOnTable : integer);

var
   i,j,rc,Wait : integer;
   ztop,zbot, Lat,Long : float64;
   sumDSM,sumDTM,zc : array[1..MaxCompare] of float32;
   count : array[1..MaxCompare] of integer;
   Results : tstringlist;
   fName : PathStr;


      procedure Fraction(ztop,zbot,zdem : float64; fName : shortstring);
      var
         f : float64;
      begin
         if abs(ztop-zbot) < 0.01 then f := -9999
         else f := (zdem - zbot) / (ztop - zbot);
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(fName,f);
      end;

begin
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddGlobalDEMs in'); {$EndIf}
   if not FileExists(ChangeFileExt(GISdb[DBonTable].DBfullName,'.shx')) then GISdb[DBonTable].SavePointShapeFile(false);      //create SHX file
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddGlobalDEMs shx done'); {$EndIf}
   if LoadDEMsCoveringBox(GISdb[DBonTable].dbBoundBox) = 0 then begin
      MessageToContinue('No DEMs found');
      exit;
   end;
   GetFieldsAndLabels(DBonTable);
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddGlobalDEMs dems loaded'); {$EndIf}

   for j := 1 to MaxCompare do begin
      SumDSM[j] := 0;
      SumDTM[j] := 0;
      Count[j] := 0;
      if (DEM_Manager.CompareDEMNames[j] <> '') then begin
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,'Z_' + DEM_Manager.CompareDEMNames[j],9,2);
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,'SLP_' + DEM_Manager.CompareDEMNames[j],9,2);
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,'FR_' + DEM_Manager.CompareDEMNames[j],9,2);
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DTM_' + DEM_Manager.CompareDEMNames[j],9,2);
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DSM_' + DEM_Manager.CompareDEMNames[j],9,2);
      end;
   end;
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddGlobalDEMs fields added'); {$EndIf}

   GISdb[DBonTable].MyData.First;
   StartProgress('Add global DEMs');
   i := 0;
   rc := GISdb[DBonTable].MyData.FiltRecsInDB;
   Wait := ProgressIncrement(rc);
   while not GISdb[DBonTable].MyData.eof do begin
      if (i mod wait = 0) then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         UpdateProgressBar(i/rc);
      end;
      inc(i);
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
           ztop := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LidarFields[2]);
           zbot := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LidarFields[1]);
           if (ztop < 0.0001) then ztop := zbot;

           GISdb[DBonTable].MyData.Edit;
           for j := 1 to MaxCompare do begin
               if (DEM_Manager.CompareDEMNames[j] <> '') then begin
                  if DEMGlb[DEM_Manager.CompareDEMIndexes[j]].GetElevFromLatLongDegree(Lat,Long,zc[j]) then begin
                     if DEMGlb[DEM_Manager.CompareDEMIndexes[j]].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM96 then zc[j] := zc[j] + GISdb[DBonTable].MyData.GetFieldByNameAsFloat('EGM_DIFF');
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Z_' + DEM_Manager.CompareDEMNames[j],zc[j]);
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat('SLP_' + DEM_Manager.CompareDEMNames[j],DEMGlb[DEM_Manager.CompareDEMIndexes[j]].SlopePercentFromLatLong(MDDef.SlopeCompute,Lat,Long));
                     Fraction(ztop,zbot,zc[j],'FR_' + DEM_Manager.CompareDEMNames[j] );
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DTM_' + DEM_Manager.CompareDEMNames[j], zc[j]-zbot);
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DSM_' + DEM_Manager.CompareDEMNames[j], zc[j]-ztop);

                     SumDSM[j] := SumDSM[j] + sqr(zc[j]-ztop);
                     SumDTM[j] := SumDTM[j] + sqr(zc[j]-zbot);
                     inc(count[j]);
                  end;
               end;
           end;
       end;
       GISdb[DBonTable].MyData.Next;
   end;
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddGlobalDEMs start cleanup'); {$EndIf}
   GISdb[dbOnTable].ShowStatus;
   Results := tstringlist.Create;
   Results.Add('DEM,RMSE');
   for j := 1 to MaxCompare do begin
      if (DEM_Manager.CompareDEMIndexes[j] <> 0) then begin
         Results.Add(DEMGlb[DEM_Manager.CompareDEMIndexes[j]].AreaName + '_DSM,' + RealToString(sqrt(SumDSM[j] / Count[j]),-12,2));
      end;
   end;
   for j := 1 to MaxCompare do begin
      if (DEM_Manager.CompareDEMIndexes[j] <> 0) then begin
         Results.Add(DEMGlb[DEM_Manager.CompareDEMIndexes[j]].AreaName + '_DTM,' + RealToString(sqrt(SumDTM[j] / Count[j]),-12,2));
      end;
   end;
   fName := Petmar.NextFileNumber(MDTempDir,'icesat_criterion__',DefaultDBExt);
   StringList2CSVtoDB(Results,fName);
   CloseCompareDEMs;
   {$IfDef RecordIceSat} WriteLineToDebugFile('AddGlobalDEMs out'); {$EndIf}
end;

{$EndIf}






