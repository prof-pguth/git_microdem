{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demdatabase      }
{___________________________________}



procedure Tdbtablef.Networkends1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   StrahlerOrder : integer;
   ThalwegLength,TotalLength : float64;
begin
   with GISdb[DBonTable] do begin
      if LineShapeFile(ShapeFileType) then
      begin
         AddFieldToDataBase(ftString,'THALWEG',1,0);
         AddFieldToDataBase(ftInteger,'STRAHLER_O',3,0);
         EmpSource.Enabled := false;
         GetDrainageBasinNetwork;
         EmpSource.Enabled := true;
         GetDrainageBasinStats(StrahlerOrder,ThalwegLength,TotalLength);
         MessageToContinue('Basin statistics' + MessLineBreak +
                  '  Strahler order: ' + IntToStr(StrahlerOrder) + MessLineBreak +
                  '  Thalweg length: ' + RealToString(ThalwegLength,-18,1) + ' km' + MessLineBreak +
                  '  Channels length: ' + RealToString(TotalLength,-18,1) + ' km',True);
         TheMapOwner.LoadDataBaseFile(MDTempDir + 'thalweg_' + MyData.GetFieldByNameAsString('BASIN_ID') + DefaultDBExt);
         TheMapOwner.LoadDataBaseFile(MDTempDir + MyData.GetFieldByNameAsString('BASIN_ID') + DefaultDBExt);
      end;
   end;
{$EndIf}
end;


procedure Tdbtablef.Fillindrainagebasin1Click(Sender: TObject);
begin
{$IfDef ExGeostats}
{$Else}
   if LineShapeFile(GISdb[DBonTable].ShapeFileType) then begin
      GISdb[DBonTable].FindDrainageBasinFromStreams;
   end;
{$EndIf}
end;



procedure LoadNodeTableToMemory(NodeTable : tMyData; var NumNodes : integer; var RiverNetwork : pRiverNetwork; ReadLinks,ReadCont,ReadOrders : boolean);
begin
     New(RiverNetWork);
     NumNodes := 0;
     NodeTable.ApplyFilter('');
     if (NodeTable.RecordCount > MaxRiverBasinNodes) then MessageToContinue('Too many nodes: ' + IntToStr(NodeTable.RecordCount));

     while (not NodeTable.eof) do begin
       if (NumNodes mod 1000 = 0) then begin
          {$IfDef VCL} ThreadTimers.UpdateThreadStats(1,round(100 * NumNodes/NodeTable.RecordCount)); {$EndIf}
       end;

       inc(NumNodes);
       RiverNetwork^[NumNodes].Lat := NodeTable.GetFieldByNameAsFloat('LAT');
       RiverNetwork^[NumNodes].Long := NodeTable.GetFieldByNameAsFloat('LONG');
       RiverNetwork^[NumNodes].Basin := NodeTable.GetFieldByNameAsString('BASIN_ID');

       if ReadLinks then begin
          RiverNetwork^[NumNodes].DOWN := NodeTable.GetFieldByNameAsString('DOWN');
          RiverNetwork^[NumNodes].UP_MAIN := NodeTable.GetFieldByNameAsString('UP_MAIN');
          RiverNetwork^[NumNodes].UP_TRIB := NodeTable.GetFieldByNameAsString('UP_TRIB');
          RiverNetwork^[NumNodes].UP_TRIB2 := NodeTable.GetFieldByNameAsString('UP_TRIB2');
       end
       else begin
          RiverNetwork^[NumNodes].DOWN := '';
          RiverNetwork^[NumNodes].UP_MAIN := '';
          RiverNetwork^[NumNodes].UP_TRIB := '';
          RiverNetwork^[NumNodes].UP_TRIB2 := '';
       end;

       if ReadCont then  begin
          RiverNetwork^[NumNodes].CONT_MAIN := NodeTable.GetFieldByNameAsInteger('CONT_MAIN');
          RiverNetwork^[NumNodes].CONT_TRIB := NodeTable.GetFieldByNameAsInteger('CONT_TRIB');
          RiverNetwork^[NumNodes].CONT_TRIB2 := NodeTable.GetFieldByNameAsInteger('CONT_TRIB2');
       end
       else  begin
          RiverNetwork^[NumNodes].CONT_MAIN := -99;
          RiverNetwork^[NumNodes].CONT_TRIB := -99;
          RiverNetwork^[NumNodes].CONT_TRIB2 := -99;
       end;

       if ReadOrders then begin
          RiverNetwork^[NumNodes].UP_MAIN_O := NodeTable.GetFieldByNameAsInteger('UP_MAIN_O');
          RiverNetwork^[NumNodes].UP_TRIB_O := NodeTable.GetFieldByNameAsInteger('UP_TRIB_O');
          RiverNetwork^[NumNodes].UP_TRIB2_O := NodeTable.GetFieldByNameAsInteger('UP_TRIB2_O');
          RiverNetwork^[NumNodes].DOWN_ORD := NodeTable.GetFieldByNameAsInteger('DOWN_ORD');
       end
       else  begin
          RiverNetwork^[NumNodes].UP_MAIN_O := 0;
          RiverNetwork^[NumNodes].UP_TRIB_O := 0;
          RiverNetwork^[NumNodes].UP_TRIB2_O := 0;
          RiverNetwork^[NumNodes].DOWN_ORD := 0;
       end;
       NodeTable.Next;
     end {while};
end;

procedure tGISdataBaseModule.FindDrainageBasinFromStreams;
const
   MaxBasins = 32000;
var
   UseField : ShortString;
   Hist : array[1..32000] of int32;
   NewHeadRecs : tDEMHeader;
   Order,Basin,i,j,NewDEM,xpic,ypic,x,y,z,NPts,Pass : integer;
   outFName : PathStr;
   FoundNew : boolean;
   zf : float64;
   TheBasins,Index : tStringList;
   Cont : ShortString;


   procedure CheckPoint(x,y,NeedZ,ThisBasin : integer);
   var
      z : integer;
   begin
      if DEMGlb[NewDEM].MissingDataInGrid(x,y) then begin
         if DEMGlb[TheMapOwner.MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,zf) then begin
            z := round(zf);
            if (z = NeedZ) then begin
               DEMGlb[NewDEM].SetGridElevation(x,y,Basin);
               FoundNew := true;
               Inc(NPts);
            end;
         end;
      end;
   end;


begin
   {$IfDef RecordBasinBreakdown} writeLineToDebugFile('tGISDataBase.FindDrainageBasinFromStreams in'); {$EndIf}

    Cont := Copy(ExtractFileName(DBFullName),1,2);
    for Order := 8 downto 4 do begin
      {$IfDef RecordBasinBreakdown} writeLineToDebugFile('order=' + IntToStr(Order)); {$EndIf}

      UseField := 'SUB_ORD_' + IntToStr(Order);
      outfName := ExtractFilePath(DEMGlb[TheMapOwner.MapDraw.DEMonMap].DEMFileName) + cont + '_' + UseField + '.dem';
      {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('outfname=' + OutfName); {$EndIf}
      if FileExists(outFName) then begin
         {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('File Existed'); {$EndIf}
      end
      else begin
         {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('filter and find basin names');{$EndIf}
          MyData.ApplyFilter(UseField + '<>' + QuotedStr(''));
          DBFieldUniqueEntries(UseField,TheBasins);
          if (TheBasins.Count = 0) then begin
             {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('TheBasins.Count = 0'); {$EndIf}
          end
          else begin
             {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('make DEM');{$EndIf}
              NewHeadRecs := DEMGlb[TheMapOwner.MapDraw.DEMonMap].DEMheader;
              NewHeadRecs.DEMPrecision := SmallIntDEM;
              NewHeadRecs.ElevUnits := euIntCode;
              if not OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,'',true) then exit;

              Index := tStringList.Create;
              Index.Add('VALUE,BASIN_ID,N');
              for i  := 1 to MaxBasins do Hist[i] := 0;

             {$IfDef RecordBasinBreakdown} writeLineToDebugFile('draw basin'); {$EndIf}
              StartThreadTimers('Subbasins order ' + IntToStr(Order),1);
              ThreadTimers.EnableGauge(1,true,'Draw basins');
              for Basin := 1 to (TheBasins.Count) do begin
                  if (Basin mod 25 = 0) then ThreadTimers.UpdateThreadStats(1,round(100*Basin/TheBasins.Count));
                  MyData.ApplyFilter(UseField + '=' + QuotedStr(TheBasins.Strings[pred(Basin)]));
                  EmpSource.Enabled := false;
                  while not MyData.EOF do begin
                     aShapeFile.GetLineCoords(MyData.RecNo,false);
                     for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                       DEMGlb[NewDEM].LatLongDegreeToDEMGridInteger(aShapeFile.CurrentLineCoords^[i].Lat,aShapeFile.CurrentLineCoords^[i].Long,xpic,ypic);
                       DEMGlb[NewDEM].SetGridElevation(xpic,ypic,-Basin);
                       inc(NPts);
                     end;
                     MyData.Next;
                  end;
              end;

            Pass := 0;
            repeat
                inc(Pass);
                ThreadTimers.EnableGauge(1,true,'Pass ' + IntToStr(Pass));
                FoundNew := false;
                NPts := 0;
                for x := 1 to DEMGlb[NewDEM].DEMheader.NumCol - 2 do begin
                   if (x mod 10 = 0) then ThreadTimers.UpdateThreadStats(1,round(100*x/DEMGlb[NewDEM].DEMheader.NumCol ));
                   for y := 1 to DEMGlb[NewDEM].DEMheader.NumRow - 2 do begin
                      if DEMGlb[NewDEM].GetElevMetersOnGrid(x,y,Basin) and (Basin < 0) then begin
                         DEMGlb[NewDEM].SetGridElevation(x,y,-Basin);
                         inc(Hist[-Basin]);
                         CheckPoint(pred(x),pred(y),128,Basin);
                         CheckPoint(pred(x),y,1,Basin);
                         CheckPoint(pred(x),succ(y),2,Basin);
                         CheckPoint(x,pred(y),64,Basin);
                         CheckPoint(x,succ(y),4,Basin);
                         CheckPoint(succ(x),pred(y),32,Basin);
                         CheckPoint(succ(x),y,16,Basin);
                         CheckPoint(succ(x),succ(y),8,Basin);
                      end {if};
                   end {for y};
                end {for x};
               {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('Pass ' + IntToStr(Pass) + '   found: ' + IntToStr(NPts)); {$EndIf}
            until (not FoundNew) or (Pass = 50);
          end {if TheBasins.Count > 0};
          EndThreadTimers;
          if (TheBasins.Count > 0) then begin
             NPts := 0;
             for I := 1 to TheBasins.Count do begin
                Index.Add(IntToStr(i) + ',' + TheBasins.Strings[pred(i)] + ',' + IntToStr(Hist[i]));
                NPts := NPts + Hist[i];
             end;
             if (NPts > 0) then begin
                Index.SaveToFile(ExtractFilePath(DEMGlb[TheMapOwner.MapDraw.DEMonMap].DEMFileName) + cont + '_' + UseField + '.vat.csv');
                DEMGlb[NewDEM].WriteNewFormatDEM(outfName);
                DEMGlb[NewDEM].SaveAsGeotiff(ChangeFileExt(outfName,'.tif'));
             end;
             CloseSingleDEM(NewDEM);
          end;
          Index.Free;
          TheBasins.Free;
       end {if not DEM Exists};
    end {for Order};
    MyData.ApplyFilter('');
    ShowStatus;
end;


procedure tGISdataBaseModule.GetDrainageBasinNetwork;
label
   CleanUp,Located;
var
   RiverNetwork : pRiverNetwork;
   PassNum,n,n1,n2,n3,NumPts,i,Down,Up,NumDone,NumNodes,ActiveNodes,LastActiveNodes : integer;
   NodeTable : tMyData;
   fName,fName2 : PathStr;
   MainLine : shortstring;
   DEMAvailable,StillWorking,RedoNodes : boolean;
   OldFilter : string;

        procedure PropogateDownstream(i : integer);
        var
           j : integer;
        begin
            for j := 1 to NumNodes do begin
               if (i <> j) then begin
                 if RiverNetwork^[i].DOWN = RiverNetwork^[j].UP_MAIN then begin
                    RiverNetwork^[j].UP_MAIN_O := RiverNetwork^[i].DOWN_ORD;
                    exit;
                 end
                 else if RiverNetwork^[i].DOWN = RiverNetwork^[j].UP_TRIB then begin
                    RiverNetwork^[j].UP_TRIB_O := RiverNetwork^[i].DOWN_ORD;
                    exit;
                 end
                 else if RiverNetwork^[i].DOWN = RiverNetwork^[j].UP_TRIB2 then begin
                    RiverNetwork^[j].UP_TRIB2_O := RiverNetwork^[i].DOWN_ORD;
                    exit;
                 end;
               end;
            end;
        end;

        procedure ReportSBProgress(what : shortString; Progress : float64);
        var
           TStr : shortString;
        begin
           if (Progress < 0) then TStr := ''
           else TStr :=  ' ' + IntToStr(Round(100* Progress)) + '%';
           wmdem.SetPanelText(0,What + TStr);
        end;

        procedure CreateNodesTable;
        var
           NodeTable0 : tMyData;
           tfName : PathStr;
           Coords2 : ^tdCoords;
           PartSize : tPartSize;
           NParts : integer;
        begin
          tfName := MDTempDir + 't_' + MyData.GetFieldByNameAsString('BASIN_ID') + DefaultDBExt;
          {$IfDef RecordNetworkends} WriteLineToDebugFile('CreateNodesTable ' + tfName); {$EndIf}

          MakeNodeTable(tfName);
          NodeTable0 := tMyData.Create(tfName);
          New(Coords2);

          NumDone := 0;
          MyData.First;
          while not MyData.EOF do begin
               if (NumDone mod 100 = 0) then begin
                  ThreadTimers.UpdateThreadStats(1,round(100 * NumDone/MyData.RecordCount));
                  ReportSBProgress('Nodes,',NumDone/MyData.RecordCount);
               end;
               inc(NumDone);

               aShapeFile.OldGetLineCoords(MyData.RecNo,NumPts,NParts,Coords2^,PartSize);
               Up := 0;
               Down := pred(NumPts);
               NodeTable0.ApplyFilter(PointVeryCloseGeoFilter('LAT','LONG',Coords2^[Down].Lat,Coords2^[Down].Long));
               if (NodeTable0.RecordCount = 0) then begin
                  NodeTable0.Insert;
                  NodeTable0.SetFieldByNameAsFloat('LAT',Coords2^[Down].Lat);
                  NodeTable0.SetFieldByNameAsFloat('LONG',Coords2^[Down].Long);
               end
               else NodeTable0.Edit;

               if NodeTable0.GetFieldByNameAsString('UP_MAIN') = '' then begin
                  NodeTable0.SetFieldByNameAsString('UP_MAIN',MyData.GetFieldByNameAsString('ARCID'));
                  NodeTable0.SetFieldByNameAsInteger('CONT_MAIN',MyData.GetFieldByNameAsInteger('UP_CELLS'));
               end
               else if NodeTable0.GetFieldByNameAsString('UP_TRIB') = '' then begin
                  if MyData.GetFieldByNameAsInteger('UP_CELLS') > NodeTable0.GetFieldByNameAsInteger('CONT_MAIN') then begin
                     NodeTable0.SetFieldByNameAsString('UP_TRIB',NodeTable0.GetFieldByNameAsString('UP_MAIN'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_TRIB',NodeTable0.GetFieldByNameAsInteger('CONT_MAIN'));
                     NodeTable0.SetFieldByNameAsString('UP_MAIN',MyData.GetFieldByNameAsString('ARCID'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_MAIN',MyData.GetFieldByNameAsInteger('UP_CELLS'));
                  end
                  else begin
                     NodeTable0.SetFieldByNameAsString('UP_TRIB',MyData.GetFieldByNameAsString('ARCID'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_TRIB',MyData.GetFieldByNameAsInteger('UP_CELLS'));
                  end;
               end
               else begin
                  if MyData.GetFieldByNameAsInteger('UP_CELLS') > NodeTable0.GetFieldByNameAsInteger('CONT_MAIN') then begin
                     NodeTable0.SetFieldByNameAsString('UP_TRIB2',NodeTable0.GetFieldByNameAsString('UP_MAIN'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_TRIB2',NodeTable0.GetFieldByNameAsInteger('CONT_MAIN'));
                     NodeTable0.SetFieldByNameAsString('UP_MAIN',MyData.GetFieldByNameAsString('ARCID'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_MAIN',MyData.GetFieldByNameAsInteger('UP_CELLS'));
                  end
                  else begin
                     NodeTable0.SetFieldByNameAsString('UP_TRIB2',MyData.GetFieldByNameAsString('ARCID'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_TRIB2',MyData.GetFieldByNameAsInteger('UP_CELLS'));
                  end;
                  if NodeTable0.GetFieldByNameAsInteger('CONT_TRIB2') > NodeTable0.GetFieldByNameAsInteger('CONT_TRIB') then begin
                     NodeTable0.SetFieldByNameAsInteger('CONT_TRIB2',NodeTable0.GetFieldByNameAsInteger('CONT_TRIB'));
                     NodeTable0.SetFieldByNameAsInteger('UP_TRIB2',NodeTable0.GetFieldByNameAsInteger('UP_TRIB'));
                     NodeTable0.SetFieldByNameAsString('UP_TRIB',MyData.GetFieldByNameAsString('ARCID'));
                     NodeTable0.SetFieldByNameAsInteger('CONT_TRIB',MyData.GetFieldByNameAsInteger('UP_CELLS'));
                  end;

               end;
               NodeTable0.Post;

               NodeTable0.ApplyFilter(PointVeryCloseGeoFilter('LAT','LONG',Coords2^[Up].Lat,Coords2^[Up].Long));
               if (NodeTable0.RecordCount = 0) then begin
                  NodeTable0.Insert;
                  NodeTable0.SetFieldByNameAsFloat('LAT',Coords2^[Up].Lat);
                  NodeTable0.SetFieldByNameAsFloat('LONG',Coords2^[Up].Long);
               end
               else NodeTable0.Edit;
               NodeTable0.SetFieldByNameAsString('DOWN',MyData.GetFieldByNameAsString('ARCID'));
               NodeTable0.Post;
               MyData.Next;
          end;
          Dispose(Coords2);
          if ShowSatProgress then EndProgress;
          NodeTable0.Destroy;
          SysUtils.RenameFile(tfName,fName);
       end;

     procedure MarkThalweg;
     var
        j,Order : integer;
        ThalTable : tMyData;
        TLine : ShortString;
        ShapeFileCreator : tShapeFileCreation;
     begin
       {$IfDef RecordThalweg} WriteLineToDebugFile('Thalweg for ' + MyData.GetFieldByNameAsString('BASIN_ID'), true,true);   {$EndIf}
        ReportSBProgress('Thalweg',-999);
        fName2 := MDTempDir + 'thalweg_' + MyData.GetFieldByNameAsString('BASIN_ID') + DefaultDBExt;
        if FileExists(fName2) then exit;

        CreateThalwegTable(fName2);
        if DEMAvailable then j := 13 else j := 3;
        ShapeFileCreator := tShapeFileCreation.Create(FName2,j);
        ThalTable := tMyData.Create(FName2);

        NodeTable.ApplyFilter('DOWN=' + QuotedStr(''));
        MainLine := NodeTable.GetFieldByNameAsString('UP_MAIN');
        if (NodeTable.RecordCount > 1) then begin
           Order := NodeTable.GetFieldByNameAsInteger('CONT_MAIN');
           while not NodeTable.eof do begin
              if NodeTable.GetFieldByNameAsInteger('CONT_MAIN') > Order then begin
                 MainLine := NodeTable.GetFieldByNameAsString('UP_MAIN');
                 Order := NodeTable.GetFieldByNameAsInteger('CONT_MAIN');
              end;
              NodeTable.Next;
           end;

           NodeTable.First;
           while not NodeTable.eof do begin
              if NodeTable.GetFieldByNameAsInteger('CONT_MAIN') <> Order then begin
                 TLine := NodeTable.GetFieldByNameAsString('UP_MAIN');
                 MyData.ApplyFilter('ARCID=' + tLine);
                 MyData.Edit;
                 MyData.SetFieldByNameAsString('BASIN_ID','reclass');
                 MyData.Post;
                 NodeTable.Edit;
                 NodeTable.Delete;
              end
              else NodeTable.Next;
           end;
        end;

          repeat
             if (MainLine <> '') then begin
                 MyData.ApplyFilter('ARCID=' + MainLine);
                 aShapeFile.GetLineCoords(MyData.RecNo);
                 {$IfDef RecordThalweg} writeLineToDebugFile(MainLine + '  pts=' + IntToStr(aShapeFile.CurrentPolyLineHeader.NumPoints));   {$EndIf}
                 for j := pred(aShapeFile.CurrentPolyLineHeader.NumPoints) downto 0 do begin
                    ShapeFileCreator.AddPointToShapeStream(aShapeFile.CurrentLineCoords^[j].Lat,aShapeFile.CurrentLineCoords^[j].Long);
                 end;
                 MyData.Edit;
                 MyData.SetFieldByNameAsString('THALWEG', 'Y');
                 MyData.Post;
                 NodeTable.ApplyFilter('DOWN=' + QuotedStr(MainLine));
                 MainLine := NodeTable.GetFieldByNameAsString('UP_MAIN');
             end;
          until (MainLine = '');

        if DEMAvailable then begin
           for j := 0 to pred(ShapeFileCreator.PtsInShapeStream) do DEMGlb[TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(ShapeFileCreator.ShapeStreamCoords^[j].Lat,ShapeFileCreator.ShapeStreamCoords^[j].Long,ShapeFileCreator.ShapeStreamZs^[j]);
        end;

        ThalTable.Edit;
        ShapeFileCreator.ProcessRecordForShapeFile(ThalTable);  //(ShapeFileCreator.ShapeStreamCoords^,1,NumPtsThalweg,ShapeFileCreator.PartSize,ShapeFileCreator.zs^);
        ThalTable.SetFieldByNameAsString('BASIN_ID',NodeTable.GetFieldByNameAsString('BASIN_ID'));
        ThalTable.Post;
        ShapeFileCreator.CloseShapeFiles(ThalTable);
        {$IfDef RecordNetworkends} WriteLineToDebugFile('Thalweg found'); {$EndIf}
     end;

        {$IfDef RecordNetworkNodes}
        procedure SaveCSVFile(fName : PathStr);
        var
           Results : tStringList;
           i : integer;
        begin
            Results := tStringList.Create;
            Results.Add('Lat,Long,DOWN,UP_MAIN,UP_TRIB,CONT_MAIN,CONT_TRIB,UP_MAIN_O,UP_TRIB_O,DOWN_ORD');
            for I := 1 to NumNodes do begin
               if I < 25 then with RiverNetwork^[i] do
                  Results.Add(RealToString(Lat,12,7) + ',' + RealToString(Long,12,7)+ ',' +
                      DOWN + ',' + UP_MAIN + ',' + UP_TRIB + ',' + IntToStr(CONT_MAIN) + ',' + IntToStr(CONT_TRIB) + ',' +
                      IntToStr(UP_MAIN_O) + ',' +  IntToStr(UP_TRIB_O) + ',' + IntToStr(DOWN_ORD));
            end;
            Results.SaveToFile(FName);
            Results.Free;
       end;
       {$EndIf}


begin
   if LineShapeFile(ShapeFileType) then begin
      {$IfDef RecordNetworkends} writeLineToDebugFile('tGISDataBase.GetDrainageBasinNetwork in, ' + GetFieldByNameAsString('BASIN_ID')); {$EndIf}
        wmdem.SetPanelText(3,TimeToStr(Now) + '   ' + dbName + '   ' + IntToStr(MyData.RecordCount));

        ShowHourglassCursor;

        DEMAvailable := (TheMapOwner.MapDraw <> Nil) and (TheMapOwner.MapDraw.DEMonMap <> 0);
        OldFilter := MyData.Filter;

        fName := MDTempDir + MyData.GetFieldByNameAsString('BASIN_ID') + DefaultDBExt;
        if FileExists(fName) then begin
           RedoNodes := false;
        end
        else begin
           CreateNodesTable;
           RedoNodes := true;
        end;
        ThreadTimers.UpdateThreadStats(1,100);

        NodeTable := tMyData.Create(fName);
        if (NodeTable.RecordCount > MaxRiverBasinNodes) then begin
           MessageToContinue('Too many nodes=' + IntToStr(NodeTable.RecordCount));
        end;

        {$IfDef RecordNetworkends} writeLineToDebugFile('Node entry complete');    {$EndIf}

        MarkThalweg;

        if not RedoNodes then begin
           NodeTable.Destroy;
           exit;
        end;

        LoadNodeTableToMemory(NodeTable,NumNodes,RiverNetwork,true,true,true);

        ThreadTimers.UpdateThreadStats(2,100);
        {$IfDef RecordNetworkends} writeLineToDebugFile('Network in memory'); {$EndIf}
        {$IfDef RecordNetworkNodes} SaveCSVFile(MDTempDir + 'raw_nodes.csv'); {$EndIf}

        ActiveNodes := NumNodes;
        ReportSBProgress('Order 1',-999);
        for I := 1 to ActiveNodes do begin
           if (RiverNetwork^[i].UP_MAIN = '') and (RiverNetwork^[i].UP_TRIB = '') then begin
              RiverNetwork^[i].DOWN_ORD := 1;
              PropogateDownStream(i);
           end {if};
        end {for i};
        {$IfDef RecordNetworkends} writeLineToDebugFile('First order found'); {$EndIf}
        {$IfDef RecordNetworkNodes} SaveCSVFile(MDTempDir + 'first_order.csv'); {$EndIf}
        PassNum := 0;
        repeat
            Inc(PassNum);
            ReportSBProgress('Orders,',(NumNodes-ActiveNodes)/NumNodes);
            ThreadTimers.UpdateThreadStats(3,round(100 * (NumNodes-ActiveNodes)/NumNodes));
            LastActiveNodes := ActiveNodes;
            StillWorking := false;
            ActiveNodes := 0;
            for I := 1 to NumNodes do begin
               if (RiverNetwork^[i].DOWN_ORD = 0) then begin
                   n1 := RiverNetwork^[i].UP_MAIN_O;
                   n2 := RiverNetwork^[i].UP_TRIB_O;
                   n3 := RiverNetwork^[i].UP_TRIB2_O;
                   if (n1 > 0) and (RiverNetwork^[i].UP_TRIB = '') then begin
                      RiverNetwork^[i].DOWN_ORD := n1;
                      PropogateDownStream(i);
                   end
                   else if (n1 > 0) and (n2 > 0) then begin
                      if (n1 = n2) then n2 := succ(n1)
                      else MinOfPairFirst(n1,n2);
                      RiverNetwork^[i].DOWN_ORD := n2;
                      PropogateDownStream(i);
                   end;

                   if (RiverNetwork^[i].DOWN_ORD = 0) then begin
                      StillWorking := true;
                      inc(activeNodes);
                   end;
               end;
            end {for i};
           {$IfDef RecordNetworkends} WriteLineToDebugFile('ActiveNodes=' + IntToStr(ActiveNodes)'); {$EndIf}
           {$IfDef RecordNetworkNodes} SaveCSVFile(MDTempDir + 'pass_' + IntToStr(PassNum) + '.csv'); {$EndIf}
        until (not StillWorking) or (LastActiveNodes = ActiveNodes);
        ActiveNodes := NumNodes;
        ThreadTimers.UpdateThreadStats(3,100);

        {$IfDef RecordNetworkends} writeLineToDebugFile('Everything ordered, nodes= ' + IntToStr(NumNodes)); {$EndIf}
        {$IfDef RecordNetworkNodes} SaveCSVFile(MDTempDir + 'all_orders.csv'); {$EndIf}

        NodeTable.First;
        for I := 1 to NumNodes do begin
           if (i Mod 25 = 0) then begin
              ReportSBProgress('To DB,',i/NumNodes);
              ThreadTimers.UpdateThreadStats(4,round(100 * (i/NumNodes)));
           end;
           NodeTable.Edit;
           NodeTable.SetFieldByNameAsInteger('DOWN_ORD',RiverNetwork^[i].DOWN_ORD);
           NodeTable.SetFieldByNameAsInteger('UP_MAIN_O',RiverNetwork^[i].UP_MAIN_O);
           NodeTable.SetFieldByNameAsInteger('UP_TRIB_O',RiverNetwork^[i].UP_TRIB_O);
           NodeTable.SetFieldByNameAsInteger('UP_TRIB2_O',RiverNetwork^[i].UP_TRIB2_O);
           NodeTable.Next;

        //this would be the slowdown.  to speed up, put the rivers into memory, and write sequentially
           if (RiverNetwork^[i].DOWN_ORD > 0) and (RiverNetwork^[i].DOWN <> '') then begin
              {$IfDef RecordNetworkends} WriteLineToDebugFile('Filter for='+ RiverNetwork^[i].DOWN); {$EndIf}
              MyData.ApplyFilter('ARCID=' + QuotedStr(RiverNetwork^[i].DOWN));
              MyData.Edit;
              MyData.SetFieldByNameAsInteger('STRAHLER_O',RiverNetwork^[i].DOWN_ORD);
              MyData.Post;
           end {if};
        end {for i};

        {$IfDef RecordNetworkends} writeLineToDebugFile('Getting memory=' + IntToStr(NodeTable.RecordCount)); {$EndIf}
        Dispose(RiverNetwork);
        NodeTable.Destroy;
        EmpSource.Enabled := true;
        MyData.ApplyFilter(OldFilter);
   end;
   wmdem.StatusBar1.Panels[0].Text := '';
   wmdem.StatusBar1.Panels[3].Text := '';
   {$IfDef RecordNetworkends} writeLineToDebugFile('Out'); {$EndIf}
end;



procedure tGISdataBaseModule.GetDrainageBasinStats(var StrahlerOrder : integer; var ThalwegLength,TotalLength : float64);
var
   MinOrder,MaxOrder : float64;
   OldFilter : string;

     function DoLength(theFilter : AnsiString) : float64;
     begin
        Result := 0;
        MyData.ApplyFilter(TheFilter);
        if MyData.RecordCount > 0 then begin
          while not MyData.EOF do begin
             Result := Result + aShapeFile.LineLength(MyData.RecNo);
             MyData.Next;
          end;
          Result := 0.001 * Result;
        end;
     end;

begin
    OldFilter := MyData.Filter;
    ThalwegLength := DoLength(PetDBUtils.AddAndIfNeeded(OldFilter) + ' THALWEG=' +  QuotedStr('Y'));
    TotalLength := DoLength(OldFilter);
    FieldRange('STRAHLER_O',MinOrder,MaxOrder);
    if (MaxOrder > 0) and (MaxOrder < 99) then StrahlerOrder := round(MaxOrder)
    else begin
       MaxOrder := 0;
       WriteLineToDebugFile('Strahler order failure, filter=' + MyData.Filter);
    end;
end;

