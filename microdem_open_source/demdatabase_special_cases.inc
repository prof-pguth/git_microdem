{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{ include file for demdatabase       }
{____________________________________}



procedure DoKMeansClustering(DBonTable : integer);
var
   j,Sampler : integer;
   MVClusterClientDataSet : TMVClusterClientDataSet;
   fName : PathStr;
   PercentMaskInLargestCluster : float64;
   ClustersWithMask,LargestMaskCluster,
   NinCluster,NinClusterAndMask,
   SizeLargestCluster,ClusterRuns,
   MinClusterUsed,MaxClusterUsed,
   NClustersUsed,NinMask,
   FieldsUsed : integer;
   FieldsToUse : array of Ansistring;
   UsingFields,
   ClusterSuccess : tStringList;
   ClusterField : ShortString;
   TStr : ANSIString;
   Bitmap : tMyBitmap;


         procedure MakeBigHistogram(UseClusters : boolean);
         var
            i,j : integer;
            Graph :  tThisBaseGraph;
            Bmp,AllGraphBitmap : tMyBitmap;
         begin
            with GISdb[DBonTable] do begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               i := succ(FieldsUsed div 3);
               if (FieldsUsed mod 3 = 0) then inc(i);
               if (FieldsUsed < 3) then j := FieldsUsed else j := 3;
               PetImage.CreateBitmap(AllGraphBitmap,(500 * j),(400 * i));
               AllGraphBitmap.Canvas.Font.Style := [fsBold];
               AllGraphBitmap.Canvas.Font.Size := 10;
               StartThreadTimers('Histograms',1);
               for i := 0 to pred(FieldsUsed) do begin
                  ThreadTimers.OverallGauge9.Progress := round(100 * i/FieldsUsed);
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  Graph := CreateHistogramFromClustersInDataBase(FieldsToUse[i],UseClusters);
                  CopyImageToBitmap(Graph.Image1,bmp);
                  AllGraphBitmap.Canvas.Draw((i mod 3) * 500, (i div 3) * 400,bmp);
                  FreeAndNil(Bmp);
                  if (i = pred(FieldsUsed)) then begin
                     Bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
                  end;
                  Graph.Close;
               end;
               i := FieldsUsed;
               if (BMP <> Nil) then begin
                  AllGraphBitmap.Canvas.Draw((i mod 3) * 500 + 50, (i div 3) * 400,bmp);
                  FreeAndNil(BMP);
               end;
               PetImage_form.DisplayBitmap(AllGraphBitmap,'Histograms');
               AllGraphBitmap.Free;
               EndThreadTimers;
            end;
         end;


      procedure MakeScatterPlots;

               procedure MakeBigGraph;
               var
                  i,j,xdrawspot,ydrawspot : integer;
                  TStr : shortstring;
                  AllGraphBitmap,bmp : tMyBitmap;
                  gr : tThisBaseGraph;
               begin
                  with GISdb[DBonTable],MyData do  begin
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     PetImage.CreateBitmap(AllGraphBitmap,(FieldsUsed*MDDef.DefaultGraphXSize) + 75,(FieldsUsed*MDDef.DefaultGraphYSize) + 50);
                     AllGraphBitmap.Canvas.Font.Style := [fsBold];
                     AllGraphBitmap.Canvas.Font.Size := 18;
                     StartThreadTimers('Big graphs',1);

                     for i := 0 to pred(FieldsUsed) do begin
                        ThreadTimers.OverallGauge9.Progress := round(100 * i/FieldsUsed);
                        dbOpts.XField := FieldsToUse[i];
                        xdrawspot := AllGraphBitmap.Canvas.TextHeight(dbOpts.YField) + 15 + i * MDDef.DefaultGraphXSize;
                        AllGraphBitmap.Canvas.TextOut(xdrawspot + MDDef.DefaultGraphXSize div 2,5,RemoveUnderscores(dbOpts.XField));
                        //AllGraphBitmap.Canvas.TextOut(xdrawspot + MDDef.DefaultGraphXSize div 2,AllGraphBitmap.Height-25,dbOpts.XField);
                        for j := 0 to pred(FieldsUsed) do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           ydrawspot := 75 + j * MDDef.DefaultGraphYSize;
                           dbOpts.YField := FieldsToUse[j];
                           Petmar.TextOutVertical(AllGraphBitmap.Canvas,5,YDrawspot + 25 + AllGraphBitmap.Canvas.TextWidth(dbOpts.YField),RemoveUnderscores(dbOpts.YField));
                           //Petmar.TextOutVertical(AllGraphBitmap.Canvas,AllGraphBitmap.Width - 15,YDrawspot + 25 + AllGraphBitmap.Canvas.TextWidth(dbOpts.YField),dbOpts.YField);
                           if FieldExists('CLUSTER') then dbOpts.ZField := 'CLUSTER' else dbOpts.ZField := 'MASK';
                           gr := GISdb[DBonTable].MakeGraph(dbgtN2DgraphCOLORfield1,false);
                           gr.GraphDraw.HorizLabel := '';
                           gr.GraphDraw.VertLabel := '';
                           //gr.GraphDraw.MarginFreeboard := 10;
                           gr.GraphDraw.ResetMargins := true;
                           gr.RedrawDiagram11Click(Nil);

                           CopyImageToBitmap(gr.Image1,bmp);
                           AllGraphBitmap.Canvas.Draw(xdrawspot,ydrawspot,bmp);
                           Bmp.Free;
                           gr.Destroy;
                        end;
                     end;
                     if dbTablef.RedGrayGraph then TStr := 'Masked region'
                     else TStr := 'Cluster composition';
                     PetImage_form.DisplayBitmap(AllGraphBitmap,TStr);
                     AllGraphBitmap.Free;
                     EndThreadTimers;
                  end;
               end;

      begin
         if MDDef.ShowClusterScatterPlots or MDDef.ShowMaskScatterPlots then begin
            {$IfDef RecordClustering} WriteLineToDebugFile('ScatterPlots started'); {$EndIf}
            SaveBackupDefaults;
            GISdb[DBonTable].EmpSource.Enabled := false;

            MDDef.DefaultGraphFont.Size := 11;
            MDDef.DefaultGraphXSize := 300;
            MDDef.DefaultGraphYSize := 200;

            if MDDef.ShowClusterScatterPlots then MakeBigGraph;

            if MDDef.ShowMaskScatterPlots then begin
               GISdb[DBonTable].dbTablef.RedGrayGraph := true;
               MakeBigGraph;
               GISdb[DBonTable].dbTablef.RedGrayGraph := false;
            end;
            MDDef.DefaultGraphXSize := 500;
            MDDef.DefaultGraphYSize := 400;
            RestoreBackupDefaults;
         end;
      end;


   procedure ProcessClustering;
   var
      i,j,Skip,fLen,rc : integer;
      f2Name : ShortString;
      NewDB : tStringList;
   begin
      {$IfDef RecordClustering} WriteLineToDebugFile('ProcessClustering in'); {$EndIf}
      if (FieldsUsed > EdburgMaxVariables) then  begin
         FieldsUsed := EdburgMaxVariables;
         MessageToContinue('Too many fields ' + IntToStr(FieldsUsed) + '/' + IntToStr(EdburgMaxVariables));
      end;
      ShowHourglassCursor;
      MVClusterClientDataSet := TMVClusterClientDataSet.Create(Nil);
      GISdb[DBonTable].EmpSource.Enabled := false;
      for i := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
          if (GISdb[DBonTable].dbOpts.VisCols[i]) then begin
             if (GISdb[DBonTable].MyData.GetFieldType(i) in [ftString]) then fLen := GISdb[DBonTable].MyData.GetFieldDataSize(i)
             else fLen := 0;
             MVClusterClientDataSet.FieldDefs.Add(GISdb[DBonTable].MyData.GetFieldName(i),GISdb[DBonTable].MyData.GetFieldType(i), fLen, False);
             {$IfDef RecordClustering} WriteLineToDebugFile(GISdb[DBonTable].MyData.GetFieldName(i)); {$EndIf}
          end;
      end;
      Skip := 1;
      while (GISdb[DBonTable].MyData.RecordCount div Skip > EdburgGeneralFuncsMaxObservations) do inc(Skip);

      MVClusterClientDataSet.CreateDataset;
      MVClusterClientDataSet.Open;
      GISdb[DBonTable].MyData.First;
      j := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      StartProgress('Load');
      while not GISdb[DBonTable].MyData.eof do begin
         inc(j);
         if (j mod 500 = 0) then begin
            UpdateProgressBar(j/rc);
            GISdb[DBonTable].EmpSource.Enabled := false;
         end;
         MVClusterClientDataSet.Append;
         for i := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
            if (GISdb[DBonTable].dbOpts.VisCols[i]) then begin
               f2Name := GISdb[DBonTable].MyData.GetFieldName(i);
               MVClusterClientDataSet.FieldByName(f2Name).AsString := GISdb[DBonTable].MyData.GetFieldByNameAsString(f2Name);
            end;
         end;
         MVClusterClientDataSet.Post;
         for i := 1 to skip do GISdb[DBonTable].MyData.Next;
      end;
      EndProgress;

      {$IfDef RecordClustering} WriteLineToDebugFile('Loaded'); {$EndIf}

      ShowHourglassCursor;
      DefineMICRODEMClusteringOptions(MVClusterClientDataSet);

      {$IfDef RecordClustering} WriteLineToDebugFile('Start K Means Clustering'); {$EndIf}
      wmDEM.SetPanelText(0,'K Means Clustering');
      MVClusterClientDataSet.KMeansClustering(FieldsToUse, FieldsUsed, MDTempDir + 'Cluster_Results.HTML');
      GISdb[DBonTable].dbTablef.SaveHiddenColumns;
      GISdb[DBonTable].dbTablef.UnHideColumns;

      {$IfDef RecordClustering} WriteLineToDebugFile('Update DB with clusters'); {$EndIf}

      wmDEM.SetPanelText(0,'Update DB with clusters');
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      i := 0;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsInteger('COLOR',WinGraphColors[MVClusterClientDataSet.ClsLabs[i] mod 15]);
         GISdb[DBonTable].MyData.SetFieldByNameAsInteger(ClusterField,MVClusterClientDataSet.ClsLabs[i]);
         GISdb[DBonTable].MyData.Next;
      end;

      {$IfDef RecordClustering} WriteLineToDebugFile('Update DB over'); {$EndIf}

      if GISdb[DBonTable].MyData.FieldExists('MASK') then begin
         GISdb[DBonTable].MyData.ApplyFilter('MASK=' + QuotedStr('Y'));
         NinMask := GISdb[DBonTable].MyData.RecordCount;
      end;

      ClustersWithMask := 0;
      LargestMaskCluster := 0;
      PercentMaskInLargestCluster := 0;
      MinClusterUsed := 999;
      MaxClusterUsed := -999;
      NClustersUsed := 0;
      {$IfDef RecordClustering} WriteLineToDebugFile('Cluster Number'); {$EndIf}
      for i := 1 to MDDef.NumClusters do begin
         GISdb[DBonTable].MyData.ApplyFilter(ClusterField + '=' + IntToStr(i));
         NinCluster := GISdb[DBonTable].MyData.RecordCount;
         if (NinCluster > 0) then begin
            inc(NClustersUsed);
            if (i < MinClusterUsed) then MinClusterUsed := i;
            if (i > MaxClusterUsed) then MaxClusterUsed := i;

            if GISdb[DBonTable].MyData.FieldExists('MASK') then begin
               GISdb[DBonTable].MyData.ApplyFilter('CLUSTER=' + IntToStr(i) + ' AND MASK=' + QuotedStr('Y'));
               NinClusterAndMask := GISdb[DBonTable].MyData.RecordCount;
               if (NinClusterAndMask > 0) then begin
                  inc(ClustersWithMask);
                  if (NinClusterAndMask > LargestMaskCluster) then begin
                     LargestMaskCluster := NinClusterAndMask;
                     SizeLargestCluster := NinCluster;
                     PercentMaskInLargestCluster := 100 * NinClusterAndMask / NinCluster;
                  end;
               end;
            end;
            {$IfDef RecordClustering} WriteLineToDebugFile('Cluster ' + IntegerToString(i,3) + IntegerToString(NinCluster,12)); {$EndIf}
         end;
      end;

      if (ClusterSuccess <> Nil) then begin
         TStr := '';
         for i := 0 to pred(FieldsUsed) do TStr := TStr + FormatString(FieldsToUse[i],12,RightJustify) + ' ';
         ClusterSuccess.Add(TStr + IntegerToString(NClustersUsed,8) + IntegerToString(ClustersWithMask,8) +
               IntegerToString(LargestMaskCluster,8) + RealToString(PercentMaskInLargestCluster,8,2) +
               RealToString(100 * LargestMaskCluster / NinMask,8,2));
      end;

      MVClusterClientDataSet.Free;
      GISdb[DBonTable].MyData.ApplyFilter( '');

         if MDDef.ShowClusterScatterPlots or MDDef.ShowMaskScatterPlots or MDDef.ShowClusterHistograms or MDDef.ShowMaskHistograms then begin
            wmDEM.SetPanelText(0,'Graphs');
            //Make legend
            CreateBitmap(Bitmap,200,20* NClustersUsed);
            for i := 1 to NClustersUsed do begin
               Bitmap.Canvas.Brush.Color := WinGraphColors[i mod 15];
               Bitmap.Canvas.Brush.Style := bsSolid;
               Bitmap.Canvas.Rectangle(5,2 + pred(i) * 20, 25, (i) * 20 - 2);
               Bitmap.Canvas.Brush.Style := bsClear;
               Bitmap.Canvas.TextOut(30,2 + pred(i) * 20,'Cluster ' + IntToStr(i));
            end;
            PetImage_form.DisplayBitmap(Bitmap,'Legend');
            Bitmap.Free;

            if MDDef.ShowClusterHistograms then MakeBigHistogram(true);
            if MDDef.ShowMaskHistograms then MakeBigHistogram(false);
            MakeScatterPlots;
         end;


         NewDB := tStringList.Create;
         if GISdb[DBonTable].LatLongFieldsPresent then TStr := 'LAT,LONG,'
         else TStr := '';

         TStr := TStr + 'CLUSTER,COLOR';
         for I := 0 to pred(FieldsUsed) do TStr := TStr + ',' + FieldsToUse[i];
         NewDB.Add(TStr);
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.eof do
         begin
            if GISdb[DBonTable].LatLongFieldsPresent then TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(GISdb[DBonTable].LatFieldName) + ',' +
                    GISdb[DBonTable].MyData.GetFieldByNameAsString(GISdb[DBonTable].LongFieldName) + ','
            else TStr := '';

            TStr := TStr + GISdb[DBonTable].MyData.GetFieldByNameAsString('CLUSTER') + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('COLOR');

            for I := 0 to pred(FieldsUsed) do TStr := TStr + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString(FieldsToUse[i]);
            NewDB.Add(TStr);
            GISdb[DBonTable].MyData.Next;
         end;

         if GISdb[DBonTable].LatLongFieldsPresent and (GISdb[DBonTable].TheMapOwner <> Nil) then begin
            fName := Petmar.NextFileNumber(ExtractFilePath(GISdb[DBonTable].dbFullName),GISdb[DBonTable].dbName + '_cluster_',DefaultDBExt);
            {$IfDef RecordClustering} WriteLineToDebugFile('save and open ' + fName); {$EndIf}
            GISdb[DBonTable].TheMapOwner.StringListToLoadedDatabase(NewDB,fName);
            GISdb[DBonTable].dbOpts.FloatColorField := 'CLUSTER';
            GISdb[DBonTable].dbOpts.dbAutoShow := dbasColorByString;
            GISdb[DBonTable].RedrawLayerOnMap;
         end;

         if MDDef.ShowClusterResults then Petmar.QuickOpenEditWindow(MDTempDir + 'Clster_Results.HTML','Cluster results');
         wmDEM.SetPanelText(0,'');
         GISdb[DBonTable].dbTablef.RestoreHiddenColumns;
   end;


   function SetUpRun : boolean;
    begin
      {$IfDef RecordClustering} WriteLineToDebugFile('Cluster SetUpRun enter'); {$EndIf}
      Sampler := 1;
      while ( (GISdb[DBonTable].MyData.RecordCount div Sampler) > EdburgGeneralFuncsMaxObservations) do inc(Sampler);
      Result := GetClusterOptions(Sampler);
      if Result then begin
          GISdb[DBonTable].EmpSource.Enabled := true;

          if (ClusterRuns > 0) then ClusterField := 'CLUSTER' + IntToStr(ClusterRuns)
          else begin
             ClusterField := 'CLUSTER';
             GISdb[DBonTable].AddFieldToDataBase(ftInteger,'COLOR',9,0);
          end;

          GISdb[DBonTable].AddFieldToDataBase(ftInteger,ClusterField,3,0);
          GISdb[DBonTable].dbtablef.Hideunusedfields1Click(nil);
          UsingFields := GISdb[DBonTable].GetAnalysisFields;
          inc(ClusterRuns);
          GISdb[DBonTable].EmpSource.Enabled := false;
          {$IfDef RecordClustering} WriteLineToDebugFile('Cluster variables for ' + ClusterField); WriteStringListToDebugFile(UsingFields,true); {$EndIf}
      end;
   end;

begin
   with GISdb[DBonTable] do begin
      {$IfDef RecordClustering} WriteLineToDebugFile('DoKMeansClustering in'); {$EndIf}
      DEMDef_routines.SaveBackupDefaults;
      ClusterRuns := 0;
      if SetUpRun then begin
         ClusterSuccess := Nil;
         if MyData.FieldExists('MASK') then ClusterSuccess := tStringList.Create
         else begin
            MDDef.ShowMaskScatterPlots:= false;
            MDDef.ShowMaskHistograms := false;
         end;

        (*
         if false and (UsingFields.Count > 5) and (AnswerIsYes('Find best combination')) then begin
            MDDef.ShowClusterScatterPlots  := false;
            MDDef.ShowMaskScatterPlots  := false;
            MDDef.ShowClusterHistograms  := false;
            MDDef.ShowMaskHistograms  := false;

            for I := 0 to pred(UsingFields.Count) - 4 do  begin
               wmDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(pred(UsingFields.Count) - 4));
               {$IfDef RecordClustering}
               WriteLineToDebugFile('i=' + IntToStr(i));
               {$EndIf}
               for j := succ(i) to pred(UsingFields.Count) - 3 do            begin
                  wmdem.StatusBar1.Panels[1].Text := IntToStr(j) + '/' + IntToStr(pred(UsingFields.Count) - 3);
                  ApplicationProcessMessages;
                  for k := succ(j) to pred(UsingFields.Count) - 2 do               begin
                     for l := succ(k) to pred(UsingFields.Count) - 1 do                  begin
                        for m := succ(l) to pred(UsingFields.Count) do                    begin
                            FieldsUsed := 5;
                            SetLength(FieldsToUse,FieldsUsed);
                            FieldsToUse[0] := UsingFields.Strings[i];
                            FieldsToUse[1] := UsingFields.Strings[j];
                            FieldsToUse[2] := UsingFields.Strings[k];
                            FieldsToUse[3] := UsingFields.Strings[l];
                            FieldsToUse[4] := UsingFields.Strings[m];
                            ProcessClustering;
                        end;
                     end;
                  end;
               end;
            end;
            UsingFields.Free;
            {$IfDef RecordClustering}
            WriteLineToDebugFile('Big Loops over');
            {$EndIf}
         end
         else *)
         begin
            FieldsUsed := UsingFields.Count;
            SetLength(FieldsToUse,FieldsUsed);
            for j := 0 to pred(FieldsUsed) do FieldsToUse[j] := UsingFields.Strings[j];
            ProcessClustering;
            UsingFields.Free;
         end;
      end;
      GISdb[DBonTable].dbTablef.UnHideColumns;
      GISdb[DBonTable].dbTablef.ShowStatus;
      DEMDef_routines.RestoreBackupDefaults;

      if (ClusterSuccess <> Nil) and (ClusterSuccess.Count > 0) then Petmar.DisplayAndPurgeStringList(ClusterSuccess,'Cluster results');
      {$IfDef RecordClustering} WriteLineToDebugFile('DoKMeansClustering over'); {$EndIf}
   end;
end;



procedure ComputeVDatumShift(dbOnTable : integer);
var
   Lat,Long,Lat2,Long2 : float64;
   Distance,Bearing : float64;
   zPresent,FileConvert : boolean;
   LocalToWGS84,WGS84toEGM2008 : integer;
   z,z2,AddZ,SubZ : float32;
begin
   with GISdb[DBonTable] do begin
      EmpSource.Enabled := false;
      AddFieldToDataBase(ftFloat,'VERT_SHIFT',8,2);
      AddFieldToDataBase(ftFloat,'HORZ_SHIFT',8,2);
      AddFieldToDataBase(ftFloat,'X_SHIFT',8,3);
      AddFieldToDataBase(ftFloat,'Y_SHIFT',8,3);
      AddFieldToDataBase(ftFloat,'HORIZ_AZ',8,1);
      zPresent := MyData.FieldExists('ELEV') and MyData.FieldExists('ELEV2');
      FileConvert := FileExists(GeoidWGS84ellipsoidToLocalVDatum) and FileExists(Geoid2008FName);
      FileConvert := false;
      if FileConvert then begin
         LoadDatumShiftGrids(LocalToWGS84,WGS84toEGM2008);
         AddFieldToDataBase(ftFloat,'GRID_VERT',8,2);
      end;

      EmpSource.Enabled := false;
      MyData.First;
      while not MyData.EOF do begin
         MyData.Edit;
         if MyData.ValidLatLongFromTable(Lat,Long) and ValidLat2Long2FromTable(Lat2,Long2) then begin
            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
            MyData.SetFieldByNameAsFloat('HORZ_SHIFT',Distance);
            MyData.SetFieldByNameAsFloat('X_SHIFT',Distance*CosDeg(Bearing));
            MyData.SetFieldByNameAsFloat('Y_SHIFT',Distance*SinDeg(Bearing));
            MyData.SetFieldByNameAsFloat('HORIZ_AZ',Bearing);
            if zPresent then begin
               z := MyData.GetFieldByNameAsFloat('ELEV');
               z2 := MyData.GetFieldByNameAsFloat('ELEV2');
               MyData.SetFieldByNameAsFloat('VERT_SHIFT',(z2-z));
            end;
            if FileConvert then begin
               if DEMGlb[WGS84toEGM2008].GetElevFromLatLongDegree(Lat,Long,AddZ) and DEMGlb[LocalToWGS84].GetElevFromLatLongDegree(Lat,Long,SubZ) then begin
                  MyData.SetFieldByNameAsFloat('GRID_VERT',(-SubZ+AddZ));
               end;
            end;
         end;
         MyData.Next;
      end;
   end;
   if FileConvert then begin
      CloseSingleDEM(LocalToWGS84);
      CloseSingleDEM(WGS84toEGM2008);
   end;
end;


function AnalyzeVDatumShift(CSVName : PathStr; ErrorLog : tStringList = Nil) : integer;
var
   fName2,fName : Pathstr;
   f1,f2,Merged : tStringList;
   TStr : ShortString;
   j : integer;
begin
   if (CSVName = '') then GetFileFromDirectory('VDATUM output file','*.csv',CSVName);
   fName2 := ChangeFileExt(CSVName, '.dbf');
   if FileExists(fName2) then begin
      OpenNumberedGISDataBase(Result,fName2);
   end
   else begin
      fName := ExtractFilePath(CSVName) + 'result\' + ExtractFileName(CSVName);  //if VDATUM run on local machine
      if not FileExists(fName) then begin
         fName := ExtractFilePath(CSVName) + ExtractFileNameNoExt(CSVName) + '_result' + ExtractFileExt(CSVName);  //VDATUM run on web
         if not FileExists(fName) then begin
            TStr := 'VDATUM not run yet for ' + CSVName;
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
            if ErrorLog <> Nil then ErrorLog.Add(TStr);
            Result := 0;
            exit;
         end;
      end;
      f1 := tStringList.Create;
      f1.LoadFromFile(CSVName);
      f2 := tStringList.Create;
      f2.LoadFromFile(fName);
      Merged := tStringList.Create;
      Merged.Add('LONG,LAT,ELEV,LONG2,LAT2,ELEV2');
      for j := 1 to pred(f1.Count) do begin
         Merged.Add(f1.Strings[j] + ',' + f2.Strings[j]);
      end;
      f1.Free;
      f2.Free;
      Result := StringList2CSVtoDB(Merged,fName2);
      ComputeVDatumShift(Result);
   end;
end;


{$IfDef ExSidescan}
{$Else}
(*
      procedure GetCoverageCorners(theData : tMyData; var CornerLats,CornerLongs: array of float64);
      var
         SidescanRunlength ,Lat,Long,Lat2,Long2,Layback,LineHeading,Range : float64;
      begin
         Lat := theData.GetFieldByNameAsFloat('LAT');
         Long := theData.GetFieldByNameAsFloat('LONG');
         Lat2 := theData.GetFieldByNameAsFloat('LAT2');
         Long2 := theData.GetFieldByNameAsFloat('LONG2');
         LineHeading := theData.GetFieldByNameAsFloat('HEADING');
         Range := theData.GetFieldByNameAsFloat('RANGE');
         Layback := theData.GetFieldByNameAsFloat('LAYBACK');
         SidescanRunlength := theData.GetFieldByNameAsFloat('LENGTH');

         VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,SidescanRunlength,LineHeading);
         if (Range = 0) then Range := 25;
         VincentyPointAtDistanceBearing(Lat,Long,Layback,LineHeading-180,Lat,Long);
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading-90,CornerLats[0],CornerLongs[0]);   //upper left
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading+90,CornerLats[1],CornerLongs[1]);  //upper right
         VincentyPointAtDistanceBearing(Lat2,Long2,Layback,LineHeading-180,Lat,Long);
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading-90,CornerLats[3],CornerLongs[3]);   //lower left
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading+90,CornerLats[2],CornerLongs[2]);   //lower right
      end;


      procedure TGISdataBaseModule.PlotSingleSideScanLeg(Bitmap : tMyBitmap);
      var
         CornerLats,CornerLongs : array[1..4] of float64;
         PolyLinePoints  : ^tPolyLinePts;
         i : integer;
      begin
         New(PolyLinePoints);
         GetCoverageCorners(MyData,CornerLats,CornerLongs);
         for i := 1 to 4 do TheMapOwner.MapDraw.LatLongDegreeToScreen(CornerLats[i],CornerLongs[i],PolyLinePoints^[pred(i)].x,PolyLinePoints^[pred(i)].y);
         Bitmap.Canvas.Polygon(Slice(PolyLinePoints^,4));
         Dispose(PolyLinePoints);
      end;

      procedure TGISdataBaseModule.PlotSideScanCoverage(Bitmap : tMyBitmap);
      begin
         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.Pen.Width := 1;
         Bitmap.Canvas.Pen.Color := clRed;
         MyData.First;
         while not MyData.eof do begin
            PlotSingleSideScanLeg(Bitmap);
            MyData.Next;
         end;
      end;
*)
{$EndIf}



{$IfDef ExGeography}
{$Else}
   procedure tGISdataBaseModule.PlotKoppenStations(Bitmap : tMyBitmap);
   var
      xp,yp : integer;
      Color : tPlatformColor;
   begin
      EmpSource.Enabled := false;
      MyData.First;
      while not MyData.EOF do begin
        TheMapOwner.MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat(LatFieldName),MyData.GetFieldByNameAsFloat(LongFieldName),xp,yp);
        if TheMapOwner.MapDraw.OnScreen(xp,yp) and GetKoppenColor(MyData.GetFieldByNameAsString('CLASS'),Color) then
           ScreenSymbol(Bitmap.Canvas,xp,yp,dbOpts.Symbol.DrawingSymbol,dbOpts.Symbol.Size,Color);
        MyData.Next;
      end;
      EmpSource.Enabled := true;
   end;
{$EndIf}


{$IfDef ExRedistrict}
{$Else}
   procedure tGISdataBaseModule.SetRedistrictPattern(var Bitmap2 : tMyBitmap);
   begin
      if (RedistrictForm <> Nil) and (RedistrictForm.RadioGroup1.ItemIndex <> 0) then begin
         Bitmap2.Canvas.Pen.Color := clBlack;
         Bitmap2.Canvas.Pen.Width := 1;
         if (RedistrictForm.RadioGroup1.ItemIndex = 1) and (MyData.GetFieldByNameAsFloat('BLACK_PC') * 100 > RedistrictForm.ColorThreshhold) or
            (RedistrictForm.RadioGroup1.ItemIndex = 2) and (MyData.GetFieldByNameAsFloat('HISPAN_PC') * 100 > RedistrictForm.ColorThreshhold) or
            (RedistrictForm.RadioGroup1.ItemIndex = 3) and (MyData.GetFieldByNameAsFloat('POP') > RedistrictForm.PopThreshhold) or
            (RedistrictForm.RadioGroup1.ItemIndex = 4) and (MyData.GetFieldByNameAsFloat('POP') > RedistrictForm.PopThreshhold) and  (MyData.GetFieldByNameAsFloat('BLACK_PC') * 100 > RedistrictForm.ColorThreshhold) or
            (RedistrictForm.RadioGroup1.ItemIndex = 5) and (MyData.GetFieldByNameAsFloat('POP') > RedistrictForm.PopThreshhold) and  (MyData.GetFieldByNameAsFloat('HISPAN_PC') * 100 > RedistrictForm.ColorThreshhold) then begin
               Bitmap2.Canvas.Brush.Style := bsSolid;
         end
         else begin
            Bitmap2.Canvas.Brush.Style := bsDiagCross;
         end;
      end;
   end;
{$EndIf}

