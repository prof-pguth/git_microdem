
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demdatabase      }
{___________________________________}


function TGISdataBaseModule.ActuallyDrawGraph(Graphtype : tdbGraphType) : TThisbasegraph;
var
  i,j,k,TimeCount,OnFile : integer;
  Hist : array[0..1000] of integer;
  TStr : ShortString;
  rFile,rfile2 : file;
  SubField : ShortString;
  MultFiles,
  GoodPoint : boolean;
  aCapt : shortstring;
  XMin : float64;
  v : array[1..3] of float32;
  SubVals,DataThere : tStringList;
  bmp : tMyBitmap;

     procedure MakeOneGraph;
     begin
         if (LastGraph <> nil) then Result := LastGraph
         else Result := TThisbasegraph.Create(Application);
         Result.SetDataBaseOnGraph(DBNumber, dbOpts.XField,dbOpts.YField,MyData.Filter);
         Result.GraphDraw.UpperLeftText := aCapt;
         if (GraphType = dbgtN2Dgraphsimplelines1) then Result.OpenDataFile(rfile)
         else Result.OpenPointFile(rfile,Result.Symbol);
         TimeCount := 0;
         MyData.First;
         repeat
             if GetFloat32FromTableLinkPossible(dbOpts.XField,v[2]) then begin
                inc(TimeCount);
                v[1] := TimeCount;
                BlockWrite(Rfile,v,1);
             end;
             MyData.Next;
          until MyData.EOF;
          Result.ClosePointDataFile(rfile);
          Result.AutoScaleAndRedrawDiagram;
     end;

     procedure OpenGraphFiles;
     begin
        if (GraphType = dbgtPlot) or (GraphType = dbgtPlot1series1) then begin
           Result.OpenPointFile(rfile,dbOpts.Symbol);
        end
        else if (GraphType = dbgtN2Dgraph2series1) then begin
           Result.Symbol.DrawingSymbol := dbOpts.Symbol.DrawingSymbol;
           Result.OpenPointFile(rfile,Result.Symbol);
           Result.Symbol.Color := claLime;
           Result.OpenPointFile(rfile2,Result.Symbol);
        end
        else if (GraphType in[dbgtCluster1,dbgtN2DgraphCOLORfield1,dbgtByLatitude1,dbgtByLongitude1,dbgtByLatitude2,dbgtByLongitude2,dbgtN2Dgraphcolorcoded1,dbgtN2Dgraphcolorcodetext1]) then begin
           Result.OpenXYColorFile(rfile);
           if (GraphType = dbgtByLatitude2) or ( GraphType = dbgtByLongitude2) then Result.GraphDraw.CorrectScaling := true;
        end
        else if (GraphType = dbgtN2Dgraphsimplelines1) then begin
           Result.OpenDataFile(rfile);
           Result.GraphDraw.ShowLine[1] := true;
           Result.GraphDraw.ShowPoints[1] := false;
        end
        else if (GraphType = dbgtN2Dgraph2yaxes1) then Result.OpenXYZFile(rfile)
        else begin
           Result.Symbol.Color := dbOpts.LineColor;
           Result.Symbol.DrawingSymbol := dbOpts.Symbol.DrawingSymbol;
           Result.Symbol.Size := 3;
           Result.OpenPointFile(rfile,Result.Symbol);
        end;
     end;

     procedure DoSettingsForSpecialTypes;
     begin
        if (GraphType = dbgtCluster1) or ( GraphType = dbgtCluster1) or ( GraphType = dbgtMultiplegraphmatrix1) then begin
           Result.GraphDraw.MarginFreeboard := 25;
        end
        else if ( GraphType = dbgtPlot1series1) then begin
           Result.GraphDraw.VertLabel := RemoveUnderscores(dbOpts.XField);
        end
        else if (GraphType = dbgtN2Dgraph2series1) then begin
           Result.GraphDraw.VertLabel := RemoveUnderscores(dbOpts.YField) + ' / ' + RemoveUnderscores(dbOpts.ZField);
           Result.GraphDraw.LegendList := tStringList.Create;
           Result.GraphDraw.LegendList.Add(RemoveUnderscores(dbOpts.YField));
           Result.GraphDraw.LegendList.Add(RemoveUnderscores(dbOpts.ZField));
        end
        else begin
           Result.GraphDraw.HorizLabel := RemoveUnderscores(dbOpts.XField);
           Result.GraphDraw.VertLabel := RemoveUnderscores(dbOpts.YField);
           if ( GraphType = dbgtN2Dgraph2yaxes1) then Result.GraphDraw.VertLabel2 := RemoveUnderscores(dbOpts.ZField);
        end;
        if (GraphType = dbgtByLatitude2) then Result.GraphDraw.HorizLabel := 'North south distance (km)';
        if (GraphType = dbgtByLongitude2) then Result.GraphDraw.HorizLabel := 'East west distance (km)';
        if (dbOpts.YField = 'DEPTH') or (Copy(dbOpts.YField,1,6) = 'DEPTH_') then Result.GraphDraw.NormalCartesianY := false;
    end;

     procedure CloseTheFiles;
     begin
        Result.ClosePointDataFile(rfile);
        if ( GraphType = dbgtN2Dgraph2series1) then begin
           Result.ClosePointDataFile(rfile2);
        end;
     end;


begin
     {$IfDef RecordDBGraphs} WriteLineToDebugFile('TGISdataBaseModule.ActuallyDrawGraph in'); {$EndIf}
     EmpSource.Enabled := false;
     DataThere := nil;
     if (GraphType = dbgtByLatitude2) then XMin := MyData.FindFieldMin(LatFieldName);
     if (GraphType = dbgtByLongitude2) then XMin := MyData.FindFieldMin(LongFieldName);
     if (GraphType = dbgtPlotforsubsamples1) then  begin
        SubField := PickField('subsampling',[ftString,ftInteger]);
        DBFieldUniqueEntries(SubField,SubVals);
     end;
     if (GraphType = dbgtN2Dgraphcolorcodetext1) then begin
        DBFieldUniqueEntries(dbOpts.StringColorField,DataThere);
     end;
     if (GraphType = dbgtPlotforsubsamples1) then  begin
        for I := 0 to pred(SubVals.Count) do begin
           MyData.ApplyFilter(SubField + '=' + QuotedStr(SubVals.Strings[i]));
           {$IfDef RecordProblems} WriteLineToDebugFile(IntToStr(i) + '  ' + MyData.Filter); {$EndIf}
           MakeOneGraph;
        end;
        SubVals.Free;
        exit;
     end;
     for i := 0 to 1000 do Hist[i] := 0;
     MultFiles := MyData.FieldExists('SEP_FILE');
     Result := TThisbasegraph.Create(Application);
     //Result.GraphDraw.UpperLeftText := GraphUpperLeftText;
     if MDDef.StatGrafReverseYAxis then Result.GraphDraw.NormalCartesianY := false;
     if (GraphType = dbgtN2Dgraph2yaxes1) then begin
        Result.GraphDraw.GraphType := gtTwoVertAxes;
        Result.GraphDraw.RightMargin := 65;
        Result.GraphDraw.ShowLine[1] := true;
        Result.GraphDraw.ShowLine[2] := true;
     end;
     if (GraphType = dbgtPlot1series1) then Result.Caption := RemoveUnderscores(dbOpts.XField) + ' time series from ' + dbName
     else if (GraphType = dbgtN2Dgraph2series1) then begin
        Result.Caption := RemoveUnderscores(dbOpts.XField) + ' vs ' + RemoveUnderscores(dbOpts.YField) + ' & ' +RemoveUnderscores(dbOpts.ZField) +  ' from ' + dbName + ' ' + dbOpts.MainFilter;
     end
     else begin
        if (GraphType = dbgtN2Dgraphcolorcoded1) then TStr := ' (color from ' + RemoveUnderscores(dbOpts.NumericColorField) + ') '
        else if (GraphType = dbgtN2Dgraphcolorcodetext1) then TStr := ' (color from ' + RemoveUnderscores(dbOpts.StringColorField) + ') '

        else TStr := '';
        Result.Caption := RemoveUnderscores(dbOpts.XField) + ' vs ' + RemoveUnderscores(dbOpts.YField) + TStr + ' from ' + dbName + ' ' + dbOpts.MainFilter;
        Result.GraphDraw.SetAllDrawingSymbols(FilledBox);
     end;
     if ( GraphType = dbgtByLatitude1) or ( GraphType = dbgtByLongitude1) then begin
        Result.GraphDraw.HardColors := true;
        Result.GraphDraw.NormalCartesianY := false;
     end;

     Result.SetDataBaseOnGraph(DBNumber,dbOpts.XField,dbOpts.YField,MyData.Filter);
     Result.GraphDraw.RainBowColors := (GraphType = dbgtN2Dgraphcolorcoded1);
     Result.GraphDraw.RedGray := (DbTablef <> Nil) and dbtablef.RedGrayGraph;
     Result.SetUpGraphForm;

     j := 0;
     OnFile := 1;
     repeat
        EmpSource.Enabled := false;
        ShowHourglassCursor;
        if (j > 0) and (dbTablef <> Nil) then dbtablef.BitBtn4Click(nil);   //(Graphtype);
        inc(j);
        Result.GraphDraw.Symbol[j].Size := 1;
        Result.GraphDraw.LineSize256[j] := 0;

        OpenGraphFiles;
        DoSettingsForSpecialTypes;

        TimeCount := 0;
        MyData.First;
        repeat
           if MultFiles then begin
              if MyData.GetFieldByNameAsInteger('SEP_FILE') <> OnFile then begin
                 CloseTheFiles;
                 OpenGraphFiles;
                 OnFile := MyData.GetFieldByNameAsInteger('SEP_FILE');
              end;
           end;
           if (GraphType = dbgtPlot1series1) then begin
             if GetFloat32FromTableLinkPossible(dbOpts.XField,v[2]) then begin
                inc(TimeCount);
                v[1] := TimeCount;
                BlockWrite(Rfile,v,1);
             end;
           end
           else if GetFloat32FromTableLinkPossible(dbOpts.XField,v[1]) and GetFloat32FromTableLinkPossible(dbOpts.YField,v[2]) then begin
              if (GraphType = dbgtN2Dgraph2yaxes1) then begin
                 GoodPoint := GetFloat32FromTableLinkPossible(dbOpts.ZField,v[3]);
              end
              else GoodPoint := true;
              if GoodPoint then begin
                 if (GraphType = dbgtCluster1) or ( GraphType = dbgtN2DgraphCOLORfield1) then v[3] := MyData.TColorFromTable;
                 if (GraphType = dbgtN2Dgraphcolorcoded1) then begin
                    if dbtablef.RedGrayGraph then begin
                       if MyData.GetFieldByNameAsString(dbOpts.ZField) = 'Y' then v[3] := 1 else v[3] := 0;
                    end
                    else begin
                       GetFloat32FromTableLinkPossible(dbOpts.ZField,v[3]);
                    end;
                 end;
                 {$IfDef ExGeology}
                 {$Else}
                    if ( GraphType = dbgtByLatitude1) or ( GraphType = dbgtByLongitude1) or ( GraphType = dbgtByLatitude2) or ( GraphType = dbgtByLongitude2) then begin
                       v[3] := ConvertPlatformColorToTColor(FocalMechanismColor);
                       if (GraphType = dbgtByLatitude2) then begin
                           v[1] := (v[1] - xmin) * 111;
                       end
                       else if ( GraphType = dbgtByLongitude2) then begin
                           v[1] := (v[1] - xmin) * 111 * cosDeg(MyData.GetFieldByNameAsFloat(LatFieldName));
                       end
                       else if (Result.XField = LongFieldName) then begin
                           if (v[1] < TheMapOwner.MapDraw.LongLeftSide) then v[1] := v[1] + 360;
                        end;
                    end;
                 {$EndIf}
                 if (GraphType = dbgtN2Dgraphcolorcoded1) or (GraphType = dbgtN2Dgraphcolorcodetext1) then begin
                    TStr := '';
                    if (GraphType = dbgtN2Dgraphcolorcodetext1) then TStr := GetStringFromTableLinkPossible(dbOpts.StringColorField);
                    if (GraphType = dbgtN2Dgraphcolorcoded1) then TStr := GetStringFromTableLinkPossible(dbOpts.NumericColorField);
                    GoodPoint := Tstr <> '';
                    if GoodPoint then begin
                       for k := 0 to pred(DataThere.Count) do
                          if (DataThere.Strings[k] = TStr) then break;
                        inc(Hist[k]);
                        v[3] := ConvertPlatformColorToTColor(Result.GraphDraw.FileColors256[succ(k Mod 256)]);
                    end;
                 end;
                 if (GraphType = dbgtN2Dgraphcolorcoded1) then begin
                    GetFloat32FromTableLinkPossible(dbOpts.NumericColorField,v[3]);
                 end;
                 if GoodPoint then BlockWrite(Rfile,v,1);
                 if (GraphType = dbgtN2Dgraph2series1) and GetFloat32FromTableLinkPossible(dbOpts.ZField,v[2]) then begin
                    BlockWrite(Rfile2,v,1);
                 end;
              end;
           end;
           MyData.Next;
        until MyData.EOF;
        CloseTheFiles;
     until (GraphType in [dbgtN2Dgraph2series1,dbgtByLatitude1,dbgtByLongitude1,dbgtMultiplegraphmatrix1,dbgtCluster1,dbgtPlot]) or (not MDDef.MultipleDBFiltersForGraphing) or (Not AnswerIsYes('Another data filter'));

    {$IfDef RecordDBGraphs} WriteLineToDebugFile('Graph setup over'); {$EndIf}
     if (GraphType = dbgtMultiplegraphmatrix1) then begin
        Result.GraphDraw.MaxHorizAxis := dbtablef.ForceXMax;
        Result.GraphDraw.MaxVertAxis := dbtablef.ForceYMax;
        Result.RedrawDiagram11Click(Nil);
        Result.Linearfit1Click(Nil);
     end
     else begin
        if (dbTablef <> nil) and (dbTablef.BitBtn24.Enabled) then AddTopLabelToGraph(Result,DBnumber);

        Result.AutoScaleAndRedrawDiagram(true,true,false,false);
     end;
    {$IfDef RecordDBGraphs} WriteLineToDebugFile('Graph draw over'); {$EndIf}

     if (GraphType = dbgtN2Dgraphcolorcodetext1) then begin
        Result.GraphDraw.LegendList := tStringList.Create;
        for k := 0 to pred(DataThere.Count) do begin
           TStr := dbOpts.StringColorField + '=' + DataThere.Strings[k] + ' (n=' + IntToStr(Hist[k]) + ')';
           Result.GraphDraw.LegendList.Add(Tstr);
        end;
        Result.SetMenus;
     end;

     if (GraphType = dbgtCluster1) or ( GraphType = dbgtMultiplegraphmatrix1) then begin
        CopyImageToBitmap(Result.Image1,bmp);
        dbtablef.AllGraphBitmap.Canvas.Draw(dbtablef.xdrawspot,dbtablef.ydrawspot,bmp);
        Bmp.Free;
        Result.Close;
     end
     else ShowStatus;
     FreeAndNil(DataThere);
     LastGraph := Result;
     LastGraphType := GraphType;
     {$IfDef RecordDBGraphs} WriteLineToDebugFile('TGISdataBaseModule.ActuallyDrawGraph out'); {$EndIf}
end;


function TGISdataBaseModule.MakeGraph(Graphtype : tdbGraphType; Ask : boolean = true) : TThisbasegraph;
var
    i : integer;
begin
     if (GraphType in [dbgtCluster1,dbgtMultiplegraphmatrix1,dbgtPlot]) then begin
     end
     else if (GraphType in [dbgtN2Dgraphsimplelines1,dbgtN2Dgraph1]) then begin
        if Ask then PickNumericFields(GraphType,2,'X axis variable','Y axis variable','')
     end
     else if (GraphType in [dbgtByLatitude1,dbgtByLongitude1,dbgtByLatitude2,dbgtByLongitude2]) then begin
        dbOpts.YField := 'DEPTH';
        dbOpts.ZField := 'MECH';
     end
     else if (GraphType = dbgtN2Dgraph2series1) then begin
        if Ask then PickNumericFields(GraphType,3,'X axis variable','Y axis series 1','Y axis series 2')
     end
     else if (GraphType = dbgtN2Dgraph2yaxes1) then begin
        if Ask then PickNumericFields(GraphType,3,'X axis variable','Y axis left axis','Y axis right series');
     end
     else if (GraphType in [dbgtPlot1series1,dbgtPlotforsubsamples1]) then begin
        if Ask then PickNumericFields(GraphType,1,'time series variable','','');
     end
     else if (GraphType = dbgtN2Dgraphcolorcodetext1) then begin
        if Ask then begin
           PickNumericFields(GraphType,3,'X axis variable','Y axis variable','',true);
        end;
     end
     else begin
        if (GraphType = dbgtN2Dgraphcolorcoded1) then i := 3 else i := 2;
        if Ask then PickNumericFields(GraphType,i,'X axis variable','Y axis variable','color coding variable');
     end;
     Result := ActuallyDrawGraph(Graphtype);
end;

{$EndIf}



procedure TGISdataBaseModule.HistogramByCategory(WantXField,FilterField : shortstring; AutoRescale : boolean; Summary : tStringList = nil);
var
  j : integer;
  MaxY,MaxX : float64;
  fName,BigName : PathStr;
  ValuesToFilter,PixList : tStringList;
  Hists : array[1..25] of TThisBaseGraph;
begin
   {$IfDef RecordDBGraphs} WriteLineToDebugFile('TGISdataBaseModule.HistogramByCategory in,  ' + WantXField); {$EndIf}
    if (WantXField = '') then WantXField := PickField('Field for histogram',[ftFloat,ftInteger,ftSmallInt]);
    if (WantXField <> '') then begin
       if (FilterField = '') then FilterField := PickField('Field for categories',[ftString,ftInteger,ftSmallInt]);
       DBFieldUniqueEntries(FilterField,ValuesToFilter);
       BigName := ChangeFileExt(DBfullName,'_class_' + WantXField + '.bmp');
       if FileExists(BigName) then exit;
       StartProgress('Histograms');
       for j := 0 to pred(ValuesToFilter.Count) do begin
          UpdateProgressBar(j/ValuesToFilter.Count);
          MyData.ApplyFilter(FilterField + '=' + ValuesToFilter[j]);
          MDDef.NoHistFreqLabels := true;
          CreateSmallGraph := true;
          MaxY := -9999;
          MaxX := -9999;
          Hists[j] := OldCreateHistogramFromDataBase(true,WantXField,'','',false);
          Hists[j].GraphDraw.LLcornerText := MyData.Filter + '  n=' + IntToStr(MyData.FiltRecsInDB);
          if (Hists[j].GraphDraw.MaxVertAxis > MaxY) then MaxY := Hists[j].GraphDraw.MaxVertAxis;
          if (Hists[j].GraphDraw.MaxHorizAxis > MaxX) then MaxX := Hists[j].GraphDraw.MaxHorizAxis;
          if Summary <> Nil then Summary.Add(ExtractFileNameNoExt(DBFullName) + ',' + ValuesToFilter[j] + ',' + WantXField + ',' +
             RealToString(Hists[j].GraphDraw.MinHorizAxis,12,4) + ',' + RealToString(Hists[j].GraphDraw.MaxHorizAxis,12,4));
          {$IfDef RecordDBGraphs}
             WriteLineToDebugFile(ExtractFileNameNoExt(DBFullName) + ValuesToFilter[j] +  '   ' + WantXField + RealToString(Hists[j].GraphDraw.MinHorizAxis,12,4) + RealToString(Hists[j].GraphDraw.MaxHorizAxis,12,4));
          {$EndIf}
       end;

        MDDef.NoHistFreqLabels := false;
        CreateSmallGraph := false;
        PixList := tStringList.Create;

        if AutoRescale or AnswerIsYes('Rescale') then begin
          if not AutoRescale then begin
             ReadDefault('MaxX',MaxX);
             ReadDefault('MaxY',MaxY);
          end;
          StartProgress('Update');
          for j := 0 to pred(ValuesToFilter.Count) do begin
              UpdateProgressBar(j/ValuesToFilter.Count);
              Hists[j].GraphDraw.MaxHorizAxis := MaxX;
              Hists[j].GraphDraw.MaxVertAxis := MaxY;
              Hists[j].GraphDraw.BottomMargin := 50;
              Hists[j].GraphDraw.LeftMargin := 60;
              Hists[j].RedrawDiagram11Click(Nil);

              fName := MDTempDir + 'hist_' + IntToStr(j) + '.bmp';
              PixList.Add(fName);
              SaveImageAsBMP(Hists[j].Image1,fName);
              Hists[j].Destroy;
          end;
        end;
        {$IfDef RecordDBGraphs} writeLineToDebugFile('TGISdataBaseModule.HistogramByCategory call make big'); {$EndIf}
        MakeBigBitmap(PixList,'',BigName);
        ValuesToFilter.free;
        ClearGISFilter;
    end;
   {$IfDef RecordDBGraphs} WriteLineToDebugFile('TGISdataBaseModule.HistogramByCategory out'); {$EndIf}
end;


function GraphFromCSVfile(var sl : tStringList; ShowTable : boolean; Lines : boolean = false) : tThisBaseGraph;
var
   GISNum : integer;
   fName : PathStr;
begin
   {$If Defined(RecordHyperion)} WriteLineToDebugFile('GraphFromCSVfile in, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}

   if (sl.count < 2) then begin
      sl.free;
      exit;
   end;
   fName := NextFileNumber(MDTempDir,'temp_graph_','.csv');
   sl.SaveToFile(fName);
   sl.free;
   if OpenNumberedGISDataBase(GISNum,fName,ShowTable) then begin
      GISdb[GISNum].dbOpts.XField := GISdb[GISNum].MyData.GetFieldName(0);
      GISdb[GISNum].dbOpts.YField := GISdb[GISNum].MyData.GetFieldName(1);
      if Lines then begin
         GISdb[GISNum].dbOpts.LineWidth := 2;
         Result := GISdb[GISNum].MakeGraph(dbgtN2Dgraphsimplelines1,false);
      end
      else begin
         GISdb[GISNum].dbOpts.Symbol.Size := 3;
         Result := GISdb[GISNum].MakeGraph(dbgtPlot,false);
      end;
      if (not ShowTable) then CloseAndNilNumberedDB(GISNum);
   end;
   {$If Defined(RecordHyperion)} WriteLineToDebugFile('GraphFromCSVfile out, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}
end;


procedure TGISdataBaseModule.MonthlyWindPlotCurrentPoint;
var
  MaxRange,xc,yc,Lat,Long : float64;
  i : integer;
  rFile : file;
  v : array[1..3] of float32;
  ThisGraph : TThisbasegraph;
begin
   EmpSource.Enabled := false;
   ValidLatLongFromTable(Lat,Long);
   ThisGraph := TThisbasegraph.Create(Application);
   ThisGraph.OpenXYZFile(rfile);
   ThisGraph.GraphDraw.GraphType := gtTadpole;
   MaxRange := 0;
   MyData.First;
   for i := 1 to 12 do begin
      v[1] := i;
      xc := MyData.GetFieldByNameAsFloat(UpperCase(MonthName[i]) + '_U_MS');
      yc := MyData.GetFieldByNameAsFloat(UpperCase(MonthName[i]) + '_V_MS');
      v[3] := sqrt(xc*xc+yc*yc);
      v[2] := HeadingOfLine(xc,yc);
      if (v[3] > MaxRange) then MaxRange := v[3];
      BlockWrite(rfile,v,1);
      MyData.Next;
   end;

   v[1] := 15;
   xc := MyData.GetFieldByNameAsFloat('ANN_U_MS');
   yc := MyData.GetFieldByNameAsFloat('ANN_V_MS');
   v[3] := sqrt(xc*xc+yc*yc);
   v[2] := HeadingOfLine(xc,yc);
   BlockWrite(rfile,v,1);
   CloseFile(rfile);
   ThisGraph.GraphDraw.LLcornerText := LatLongDegreeToString(Lat,Long,VeryShortDegrees);
   ThisGraph.InitializeTadpole('Monthly winds at ' + LatLongDegreeToString(Lat,Long,NearestMinute),-2,18,MaxRange);
   ShowStatus;
end;


function TGISdataBaseModule.WaveFormGraph(SingleGraph : boolean) : tThisBaseGraph;
var
   sl,sl2 : tStringList;
   LastI,FileNum : integer;
   fname : shortstring;
   z,lastz : float64;

      procedure ProcessRecord;
      var
         i : integer;
      begin
         LastZ := 0;
         Lasti := 0;
         for i := 10 to 100 do begin
            fName := 'RH' + IntToStr(i);
            if MyData.FieldExists(fName) then begin
               z := MyData.GetFieldByNameAsFloat(fName);
               if z > LastZ then begin
                  if MDDef.LVISGabonGraphs in [lVISGabonBoth,LVISGabonCum] then begin
                     sl.add(IntToStr(i)+ ',' + RealToString(z,-12,-2)+ ',' + IntToStr(FileNum));
                  end;
                  if MDDef.LVISGabonGraphs in [lVISGabonBoth,LVISGabonHist] then begin
                     sl2.add(RealToString( (i-LastI) / (z-Lastz),-12,-2) + ',' + RealToString(0.5*(z + Lastz),-12,-2) + ',' + IntToStr(FileNum));
                  end;
               end;
               LastI := i;
               LastZ := z;
            end;
         end;
      end;

begin
   if not SingleGraph then begin
      if (MyData.FiltRecsInDB > 250) then begin
         if not AnswerIsYes('Proceed with recs=' + IntToStr(MyData.FiltRecsInDB)) then exit;
      end;
   end;

   if MDDef.LVISGabonGraphs in [lVISGabonBoth,LVISGabonCum] then begin
      sl := tStringList.Create;
      sl.add('CUM_PC,HEIGHT_M,SEP_FILE');
   end;
   if MDDef.LVISGabonGraphs in [lVISGabonBoth,LVISGabonHist] then begin
      sl2 := tStringList.Create;
      sl2.add('ENERGY_P_M,HEIGHT_M,SEP_FILE');
   end;
   EmpSource.Enabled := false;
   FileNum := 0;
   if SingleGraph then begin
      ProcessRecord;
   end
   else begin
      MyData.First;
      while not MyData.eof do begin
         inc(FileNum);
         ProcessRecord;
         MyData.Next;
      end;
   end;

   if MDDef.LVISGabonGraphs in [lVISGabonBoth,LVISGabonCum] then GraphFromCSVfile(sl,false,true);
   if MDDef.LVISGabonGraphs in [lVISGabonBoth,LVISGabonHist] then GraphFromCSVfile(sl2,false,true);
   ShowStatus;
end;


         procedure TGISdataBaseModule.SingleRose(AddTitle : shortString; Field1,Field2 : ShortString);
         var
           i,j : integer;
           Bins : CircleFreqType;
           TStr : shortstring;
           Angle : float64;
           ThisGraph : TThisBaseGraph;
         begin
            EmpSource.Enabled := false;
            ShowHourglassCursor;
            i := 0;
            for j := 0 to 360 do Bins[j] := 0;
            StartProgressAbortOption('Rose');
            MyData.First;
            repeat
              inc(i);
              if (i mod 1000 = 0) then UpdateProgressBar(i/ MyData.RecordCount);

              if (Field1 = 'dd_from_strike') then begin
                 Angle := MyData.GetFieldByNameAsFloat('FP1_STRIKE') + 90;
                 inc(Bins[round(FindCompassAngleInRange(Angle))]);
                 Angle := MyData.GetFieldByNameAsFloat('FP2_STRIKE') + 90;
                 inc(Bins[round(FindCompassAngleInRange(Angle))]);
              end
              else begin
                 Angle := MyData.GetFieldByNameAsFloat(Field1);
                 inc(Bins[round(FindCompassAngleInRange(Angle))]);

                 if (Field2 <> '') then begin
                    Angle := MyData.GetFieldByNameAsFloat(Field2);
                    inc(Bins[round(FindCompassAngleInRange(Angle))]);
                 end;
              end;
              MyData.Next;
              if WantOut then break;
            until MyData.EOF;
            if Field1 = 'dd_from_strike' then begin
              TStr := 'Focal plane dip directions';
            end
            else begin
              tStr :=  Field1;
              if (Field2 <> '') then TStr := TStr + ' + ' + Field2;
            end;
            ThisGraph := TThisBaseGraph.Create(Application);
            ThisGraph.GraphFilter := MyData.Filter;
            ThisGraph.DrawAspectRose(Bins,TStr + ' from ' + dbName + ' ' + AddTitle);
            EndProgress;
         end;


function TGISdataBaseModule.Stationtimeseries : tThisBaseGraph;
{uses University of Hawaii tide station time series, http://uhslc.soest.hawaii.edu/}
var
   Year1,Year2 : integer;
   Rise,Lat,Long     : float64;
   fName : PathStr;
   StationName : ShortString;
begin
   {$IfDef RecordStationTimeSeries} WriteLineToDebugFile('TGISdataBaseModule.Stationtimeseries in'); {$EndIf}
   fName := ExtractFilePath(dbFullName) + MyData.GetFieldByNameAsString('TIME_SER');
   if Not FileExists(fName) then begin
      {$IfDef RecordStationTimeSeries} WriteLineToDebugFile('missing=' + fName); {$EndIf}
      exit;
   end;

   Result := MakeGraphFromSOESTtides(fName, Lat,Long, Year1,Year2, StationName);
   Rise := MyData.GetFieldByNameAsFloat('RISE');
   if abs(Rise) > 0.0001 then Result.Caption := StationName + '  ' + LatLongDegreeToString(Lat,Long,DecMinutes)  + '  sea level rise=' + RealToString(Rise,-12,-4) + ' mm/yr'
   else Result.Caption := StationName + '  ' + LatLongDegreeToString(Lat,Long,DecMinutes);
   {$IfDef RecordStationTimeSeries} WriteLineToDebugFile('TGISdataBaseModule.Stationtimeseries out'); {$EndIf}
end;


procedure TGISdataBaseModule.AddSeriesToScatterGram(Graph : TThisbasegraph; Color : tColor; anXField,anYField : ShortString;  Connect : boolean = false);
var
  rFile : file;
  v : array[1..2] of float32;
begin
   ShowHourglassCursor;
   Graph.GraphDraw.LegendList.Add(anXField);
   if Connect then begin
      Graph.OpenDataFile(rfile);
   end
   else begin
      Graph.Symbol.Size := 3;
      Graph.Symbol.Color := ConvertTColorToPlatformColor(Color);
      Graph.OpenPointFile(rfile,Graph.Symbol);
   end;
   MyData.First;
   repeat
      if GetFloat32FromTableLinkPossible(anXField,v[1]) and GetFloat32FromTableLinkPossible(anYField,v[2]) then begin
         BlockWrite(Rfile,v,1);
      end;
      MyData.Next;
   until MyData.EOF;
   CloseFile(rfile);
   ShowDefaultCursor;
   Graph.AutoScaleAndRedrawDiagram;
end;


function TGISdataBaseModule.CreateScatterGram(anXField,anYField : ShortString; Color : tColor = clRed; Connect : boolean = false; Capt : shortstring = '';
    H_lab : shortstring = ''; V_lab : shortString = ''; NormProb : boolean = false) : TThisbasegraph;
begin
   {$IfDef RecordDBGraphs} writeLineToDebugFile('TGISdataBaseModule.CreateScatterGram in'); {$EndIf}
   EmpSource.Enabled := false;
   Result := TThisbasegraph.Create(Application);
   Result.GraphDraw.LegendList := TStringList.Create;
   if (Capt = '') then Result.Caption := anXField + ' vs ' + anYField + ' from ' + dbName
   else Result.Caption := Capt;
   if NormProb then begin
      Result.GraphDraw.VertAxisFunctionType := LongCumulativeNormalAxis;
   end;
   Result.GraphDraw.SetShowAllLines(Connect);
   Result.GraphDraw.LineSize256[1] := 1;
   Result.SetUpGraphForm;
   if (H_lab = '') then Result.GraphDraw.HorizLabel := RemoveUnderscores(anXField)
   else Result.GraphDraw.HorizLabel := H_Lab;
   if (V_lab = '') then Result.GraphDraw.VertLabel := RemoveUnderscores(anYField)
   else Result.GraphDraw.VertLabel := V_Lab;
   {$IfDef RecordDBGraphs} writeLineToDebugFile('TGISdataBaseModule.CreateScatterGram labels set'); {$EndIf}
   if (Color > 0) then AddSeriesToScatterGram(Result,Color,anXField,anYField,Connect);
   {$IfDef RecordDBGraphs} writeLineToDebugFile('TGISdataBaseModule.CreateScatterGram out'); {$EndIf}
end;


function TGISdataBaseModule.OldCreateHistogramFromDataBase(RegHist: boolean; WantXField, WantYField, WantZField : shortString; AllDBs: boolean;
     MinUse : float64 = 1; MaxUse : float64 = -1; BinSize : float64 = -99): TThisBaseGraph;
var
   NumSeries,i,j : integer;
   NPts : int64;
   Values : ^bfarray32;
   TStr,MenuStr,TheDBName : ShortString;
   FilesList,LegendList : tStringList;
   LegendStr : ANSIString;
   UseNumbers : boolean;


      procedure Save;
      var
         fName : PathStr;
      begin
         fName := BaseGraf.SaveSingleValueSeries(Npts,Values^{,0,Npts});
         {$IfDef RecordDBPlot} writeLineToDebugFile('save: ' + fName); {$EndIf}
         FilesList.Add(fName);
         Dispose(Values);
      end;


begin
   NumSeries := 1;
   UseNumbers := true;
   if (WantYField <> '') then NumSeries := 2;
   if (WantZField <> '') then NumSeries := 3;
     {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram start'); {$EndIf}
     FilesList := tStringList.Create;
     LegendList := tStringList.Create;

     MenuStr := '';
     for i := 1 to NumSeries do begin
        case i of
           1 : TStr := WantXField;
           2 : TStr := WantYField;
           3 : TStr := WantZField;
        end;
        MenuStr := MenuStr + TStr + '  ';
        if AllDBs then begin
           TheDBName := '';
           for j := 1 to MaxDataBase do if (GISdb[j] <> Nil) then begin
              if MyData.FieldExists(TStr) then begin
                 New(Values);
                 GISdb[j].GetPointArrayForDBField(TStr,Values^,NPts,MinUse,MaxUse);
                 Save;
                 GISdb[j].EmpSource.Enabled := true;
                 LegendStr := RemoveUnderScores(GISdb[j].dbName);
                 LegendList.Add(LegendStr + ',  n=' + IntToStr(Npts));
                 {$IfDef RecordDBPlot} WriteLineToDebugFile(IntToStr(j) + '  ' + GISDB[j].dbName); {$EndIf}
              end;
           end;
        end
        else begin
           TheDBName := ' (' + DBName + ')';
           New(Values);
           LegendList.Add(TStr);
           GetPointArrayForDBField(TStr,Values^,NPts,MinUse,MaxUse);
           Save;
        end;
     end;
     {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram done prep'); {$EndIf}

     Result := CreateMultipleHistogram(UseNumbers,FilesList,LegendList,MenuStr,'Histogram ' + MenuStr + TheDBName + ' ' + MyData.Filter,100,MinUse,MaxUse,BinSize);
     {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram out'); {$EndIf}
end;


function TGISdataBaseModule.CreateHistogramFromDataBase(RegHist: boolean; Fields : tStringList; AllDBs: boolean; MinUse : float64 = 1; MaxUse : float64 = -1; BinSize : float64 = -99): TThisBaseGraph;
var
   j : integer;
   Min,Max : float64;
   FilesList,
   LegendList : tStringList;

      procedure DoOneDB(db : integer);
      var
         NPts : int64;
         I : integer;
         fName : PathStr;
         Values : ^bfarray32;
         TheDBName : ShortString;
      begin
         FilesList := tStringList.Create;
         LegendList := tStringList.Create;
         for i := 1 to Fields.Count do begin
            TheDBName := ' (' + DBName + ')';
            New(Values);
            LegendList.Add(RemoveUnderScores(Fields[pred(i)]));
            GISDB[db].GetPointArrayForDBField(Fields[pred(i)],Values^,NPts,MinUse,MaxUse);
            HeapSort(Npts,Values^);
            if Values^[0] < Min then Min := Values^[0];
            if Values^[pred(NPts)] > Max then Max := Values^[pred(NPts)];

            fName := BaseGraf.SaveSingleValueSeries(Npts,Values^{,0,Npts});
            {$IfDef RecordDBPlot} writeLineToDebugFile('save: ' + fName); {$EndIf}
            FilesList.Add(fName);
            Dispose(Values);
         end;
      end;

begin
  {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram start'); {$EndIf}
  Min := 99e39;
  Max := -99e30;
  if AllDBs then begin
     for j := 1 to MaxDataBase do if ValidDB(j) then begin
        DoOneDB(j);
     end;
  end
  else begin
     DoOneDB(DBNumber);
  end;

  {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram done prep'); {$EndIf}

  Result := CreateMultipleHistogram(True,FilesList,LegendList,'','Histogram ' +  MyData.Filter,100,Min,Max,BinSize);
  {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram out'); {$EndIf}
end;


function TGISdataBaseModule.CreateHistogramFromClustersInDataBase(WantXField: shortString;  UseClusters: boolean): TThisBaseGraph;
var
   i,NPts,Round : int64;
   Values : ^bfarray32;
   ch : AnsiChar;
   OldFilter,
   TStr,MenuStr : ShortString;
   FilesList,LegendList : tStringList;
begin
     {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('TGISDataBase.CreateHistogram start'); {$EndIf}
     OldFilter := MyData.Filter;
     if (MyData.Filter = '') then MenuStr := '' else MenuStr := ' AND (' + MyData.Filter + ')';

     FilesList := tStringList.Create;
     LegendList := tStringList.Create;

     New(Values);
     {$IfDef NoClustering}
        Round := 2;
     {$Else}
        if UseClusters then Round := MaxClusters else Round := 2;
     {$EndIf}

     for i := 1 to Round do begin
        if UseClusters then begin
           TStr := 'CLUSTER=' + IntToStr(i);
        end
        else begin
           if i=1 then ch := 'Y' else ch := 'N';
           TStr := 'MASK=' + QuotedStr(ch);
        end;
        MyData.ApplyFilter(TStr + MenuStr);
        if (MyData.RecordCount > 0) then begin
           LegendList.Add(TStr);
           GetPointArrayForDBField(WantXField,Values^,NPts);
           FilesList.Add(BaseGraf.SaveSingleValueSeries(Npts,Values^));
        end;
     end;

     {$IfDef RecordDataBaseTiming} writeLineToDebugFile('TGISDataBase.CreateHistogram done'); {$EndIf}
     Dispose(Values);
     MyData.ApplyFilter(OldFilter);
end;


