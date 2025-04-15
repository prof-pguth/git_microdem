
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{      include file for demmapf          }
{________________________________________}

type
   tLandCoverSource = (lcsNLCD1992,lcsNLCD2001,lcsShapeFile,lcsNBCD,lcsLandFire);


{$IfDef IncludeMakeLandCoverGrid}



procedure TMapForm.VegetationobstaclegridfromNBCD20001Click(Sender: TObject);
begin
   MakeLandCoverGrid(MapDraw.DEMonMap,lcsNBCD,false);
end;


procedure TMapForm.VegetationobstaclegridfromNLCD1Click(Sender: TObject);
begin
   MakeLandCoverGrid(MapDraw.DEMonMap,lcsNLCD2001,false);
end;

procedure TMapForm.Vegetationobstaclegridfromshapefile1Click(Sender: TObject);
begin
   MakeLandCoverGrid(MapDraw.DEMonMap,lcsShapeFile,false);
end;

procedure TMapForm.LandFiregrid1Click(Sender: TObject);
begin
   MakeLandCoverGrid(MapDraw.DEMonMap,lcsLandFire,true);
end;

procedure TMapForm.NLCD1992grid1Click(Sender: TObject);
begin
   MakeLandCoverGrid(MapDraw.DEMonMap,lcsNLCD1992,true);
end;


      procedure MakeLandCoverGrid(WhichDEM : integer; Source : tLandCoverSource; CategoryGrid : boolean);
      {$IfDef ExMakeLCGrid}
      begin
      {$Else}
      var
         tDEM,LastRow,WantedDEM,i,j,k,v,RealDEMonMap,x,y,VegDB : integer;
         DefFilter : byte;
         flt,Lat,Long,Col,Row,VegHt : float64;
         fName : PathStr;
         MaskBMP : tMyBitmap;
         FileNames : tStringList;
         NLCDRow : tImageRow;
         Row16Bit : tWordRow16Bit;


               procedure MakeMap(AllRecords : boolean);
               begin
                  CloneImageToBitmap(DEMGlb[WhichDEM].SelectionMap.Image1,MaskBMP);
                  MaskBMP.Canvas.Pen.Color := clBlack;
                  MaskBMP.Canvas.Brush.Color := clBlack;
                  MaskBMP.Canvas.Brush.Style := bsSolid;
                  if AllRecords then begin
                     DEMGlb[WhichDEM].SelectionMap.MapDraw(VegDB,MaskBMP);
                     PetImage.RecolorBitmap(MaskBMP,claBlack);
                  end
                  else GISdb[VegDB].DisplayCurrentRecordOnMap(MaskBMP);
                  DEMGlb[WhichDEM].SelectionMap.Image1.Picture.Graphic := MaskBMP;
                  MaskBMP.Free;
              end;

      begin
         {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('Make LandCoverGrid in,  Base map, DEM=' + DEMGlb[WhichDEM].AreaName);
         if CategoryGrid then WriteLineToDebugFile('Category grid');
         {$EndIf}
         with DEMGlb[WhichDEM] do begin
            WantedDEM := 0;
            fName := ExtractFilePath(DEMGlb[WhichDEM].DEMFileName);

            OpenDEMDataStructures(WantedDEM);
            DEMGlb[WantedDEM].DEMheader := DEMGlb[WhichDEM].DEMheader;
            if CategoryGrid then  begin
               DEMGlb[WantedDEM].DEMheader.DEMPrecision := byteDEM;
               if (Source in [lcsLandFire]) then DEMGlb[WantedDEM].DEMheader.ElevUnits := LandFire
               else if (Source in [lcsNLCD2011]) then DEMGlb[WantedDEM].DEMheader.ElevUnits := NLCD2011
               else if (Source in [lcsNLCD2006]) then DEMGlb[WantedDEM].DEMheader.ElevUnits := NLCD2006
               else if (Source in [lcsNLCD2001]) then DEMGlb[WantedDEM].DEMheader.ElevUnits := NLCD2001
               else if (Source in [lcsNLCD1992]) then DEMGlb[WantedDEM].DEMheader.ElevUnits := NLCD1992;
               {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('New grid ElevUnits=' + ElevUnitsAre[DEMGlb[WantedDEM].HeadRecs.ElevUnits]); {$EndIf}
            end
            else begin
               if MDDef.RealVegGrids then begin
                  DEMGlb[WantedDEM].DEMheader.DEMPrecision := FloatingPointDEM;
                  DEMGlb[WantedDEM].DEMheader.ElevUnits := euMeters;
               end
               else begin
                  DEMGlb[WantedDEM].DEMheader.ElevUnits := DeciMeters;
                  DEMGlb[WantedDEM].DEMheader.DEMPrecision := SmallIntDEM;  //could be byte
               end;
            end;
            if not DEMGlb[WantedDEM].AllocateDEMMemory then exit;
            DEMGlb[WantedDEM].DefineDEMvariables(True);

            DefFilter := 0;
            FileNames := tStringList.Create;
            {$IfDef EXNLCD}
            {$Else}
            if (Source = lcsNBCD) then begin
               {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('From NBCD'); {$EndIf}
               DefFilter := 1;
               FileNames.Add(MainMapData);
               Petmar.GetMultipleFiles('NBCD files','NBCD files|*.tif',FileNames,DefFilter);
               for k := 0 to pred(FileNames.Count) do begin
                  fName := FileNames.Strings[k];
                  if OpenAndDisplayNewScene(Nil,fName,false,false,true) then begin
                     if ShowSatProgress then StartProgressAbortOption('Land coverage, Pass 2');
                     {second pass fills any holes}
                     LastRow := -9999;
                     for i := 0 to pred(DEMheader.NumCol) do  begin
                        if (i mod 25 = 0) and ShowSatProgress then UpdateProgressBar(i/DEMheader.NumCol);
                        for j := 0 to pred(DEMheader.NumRow) do  begin
                           if DEMGlb[WantedDEM].MissingData(i,j) then  begin
                              DEMGridToLatLongDegree(i,j,Lat,Long);
                              if SatImage[NewSatImage].LatLongDegreeInDataSet(Lat,Long) then  begin
                                 SatImage[NewSatImage].LatLongDegreeToSatGrid(Lat,Long,Col,Row);
                                 if Row <> LastRow then begin
                                    SatImage[NewSatImage].TiffImage[1].GetTiffRow16bit(1,round(Row),Row16Bit);
                                    LastRow := round(Row);
                                 end;
                                 DEMGlb[WantedDEM].SetGridElevation(i,j,0.1 * Row16Bit[round(Col)]);
                              end;
                           end;
                        end;
                     end;
                  end;
                  CloseSingleSatelliteImage(NewSatImage);
               end;
            end
            else if (Source in [lcsNLCD1992,lcsNLCD2001,lcsNLCD2006,lcsNLCD2011,lcsLandFire]) then  begin
               DefFilter := 1;
               if (Source in [lcsNLCD2011]) then begin
                  DEMGlb[WantedDEM].AreaName := 'NLCD-2011';
               end
               else if (Source in [lcsNLCD2006]) then begin
                  DEMGlb[WantedDEM].AreaName := 'NLCD-2006';
               end
               else if (Source in [lcsNLCD2001]) then begin
                  DEMGlb[WantedDEM].AreaName := 'NLCD-2001';
               end
               else if (Source in [lcsNLCD1992]) then begin
                  DEMGlb[WantedDEM].AreaName := 'NLCD-1992';
               end
               else begin
                  DEMGlb[WantedDEM].AreaName := 'Landfire';
               end;
               {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('From ' + DEMGlb[WantedDEM].AreaName);  {$EndIf}
               FileNames.Add(LastLandCoverName);
               Petmar.GetMultipleFiles(DEMGlb[WhichDEM].AreaName + ' files',DEMGlb[WantedDEM].AreaName + ' files|*.tif:*.tiff',FileNames,DefFilter);

               for k := 0 to pred(FileNames.Count) do  begin
                  LastLandCoverName := FileNames.Strings[k];
                  {$IfDef MakeLandCoverGrid}  WriteLineToDebugFile('Load=' + LastLandCoverName);  {$EndIf}
                  if (Source in [lcsNLCD1992,lcsNLCD2001,lcsNLCD2006,lcsNLCD2011,lcsLandFire]) then begin
                     if LoadNewDEM(tDem,LastLandCoverName,false) then begin
                        if ShowSatProgress then StartProgressAbortOption('Land coverage');
                        for i := 0 to pred(DEMheader.NumCol) do begin
                           if (i mod 25 = 0) and ShowSatProgress then UpdateProgressBar(i/DEMheader.NumCol);
                           for j := 0 to pred(DEMheader.NumRow) do begin
                              if DEMGlb[WantedDEM].MissingData(i,j) then begin
                                 DEMGridToLatLongDegree(i,j,Lat,Long);
                                 DEMGlb[tDEM].LatLongDegreeToDEMGrid(Lat,Long,x,y);
                                 if DEMGlb[tDEM].GetElevMeters(x,y,flt) then
                                     DEMGlb[WantedDEM].SetGridElevation(i,j,flt);
                              end;
                           end;
                        end;
                        {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('call close tDEM'); {$EndIf}
                        CloseSingleDEM(tDEM);
                        {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('close tDEM done'); {$EndIf}
                     end;
                  end
                  else if OpenAndDisplayNewScene(Nil,LastLandCoverName,false,false,true) then begin
                     if (SatImage[NewSatImage].LandCoverImage = Nil) then begin
                        MessageToContinue('NLCD file not selected');
                        DisplayHTMLTopic('html/recognize_nlcd.htm');
                     end
                     else begin
                        if ShowSatProgress then StartProgressAbortOption('Land coverage, Pass 1');
                        {first pass puts each point on NLCD into the grid}
                        for j := 0 to pred(SatImage[NewSatImage].LandCoverImage.NumRows) do begin
                           if (j mod 25 = 0) and ShowSatProgress then UpdateProgressBar(j/SatImage[NewSatImage].LandCoverImage.NumRows);
                           SatImage[NewSatImage].GetSatRow(1,j,NLCDRow);
                           for i := 0 to pred(SatImage[NewSatImage].LandCoverImage.NumCols) do begin
                              SatImage[NewSatImage].SatGridToLatLongDegree(i,j,Lat,Long);
                              v := NLCDRow[i];
                              if CategoryGrid then begin
                                 DEMGlb[WantedDEM].SetGridElevationLatLongDegree(Lat,Long,v);
                              end
                              else if (v < MaxNLCDCategories) and (v > 0) then begin
                                 flt := SatImage[NewSatImage].LandCoverImage.Categories[round(v)].Height;
                                 DEMGlb[WantedDEM].SetGridElevationLatLongDegree(Lat,Long,flt);
                              end;
                           end;
                        end;
                        {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('Pass 1 done'); {$EndIf}

                        if ShowSatProgress then StartProgressAbortOption('Land coverage, Pass 2');
                        {second pass fills any holes}

                        for i := 0 to pred(DEMheader.NumCol) do begin
                           if (i mod 25 = 0) and ShowSatProgress then UpdateProgressBar(i/DEMheader.NumCol);
                           for j := 0 to pred(DEMheader.NumRow) do begin
                              if DEMGlb[WantedDEM].MissingData(i,j) then begin
                                 DEMGridToLatLongDegree(i,j,Lat,Long);
                                 if (Lat > DEMSWcornerLat-0.01) and (Lat < DEMSWcornerLat + LatSizeMap + 0.01) then begin
                                    v := SatImage[NewSatImage].LandCoverImage.NLCDCategory(Lat,Long);
                                    if CategoryGrid then DEMGlb[WantedDEM].SetGridElevation(i,j,v)
                                    else if (v < MaxNLCDCategories) and (v > 0) then begin
                                       flt := SatImage[NewSatImage].LandCoverImage.Categories[v].Height;
                                       DEMGlb[WantedDEM].SetGridElevation(i,j,flt);
                                    end;
                                 end;
                              end;
                           end;
                        end;
                     end;
                     CloseSingleSatelliteImage(NewSatImage);
                  end;
               end;
            end
            else {$EndIf}
            begin
               {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('from shapefiles'); {$EndIf}
               FileNames.Add(LastDataBase);
               if not GetMultipleFiles('Vegetation masks',DBNameMask,FileNames,DefFilter) then Exit;

               RealDEMonMap := SelectionMap.MapDraw.DEMonMap;
               SelectionMap.MapDraw.DEMonMap := WantedDEM;
               for i := 0 to pred(FileNames.Count) do begin
                  {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('db=' + FileNames.Strings[i]); {$EndIf}

                  if OpenNumberedGISDataBase(VegDB,FileNames.Strings[i],false,false,SelectionMap) then begin
                     VegHt := -99;
                     if GISdb[VegDB].MyData.FieldExists('HEIGHT') then   begin
                        while not GISdb[VegDB].MyData.EOF do begin
                           MakeMap(false);
                           VegHt := 10 * GISdb[VegDB].MyData.GetFieldByNameAsFloat('HEIGHT');
                           SelectionMap.EditMapViaColor(emvcSetElevation,clBlack,VegHt);
                           GISdb[VegDB].MyData.Next;
                        end;
                     end
                     else begin
                        VegHt := 10;
                        ReadDefault('Vegetation height (m)',VegHt);
                        VegHt := 10 * VegHt;
                        MakeMap(true);
                        SelectionMap.EditMapViaColor(emvcSetElevation,clBlack,VegHt);
                      end;
                      DEMGlb[WantedDEM].MissingDataToSeaLevel;
                      CloseAndNilNumberedDB(VegDB);
                  end {if};
               end {for i};
            end;
            if ShowSatProgress then EndProgress;
            FileNames.Free;
            DEMGlb[WhichDEM].SelectionMap.DoFastMapRedraw;
            DEMGlb[WantedDEM].SetUpMap(WantedDEM,mtDEMChromaDepth,DEMGlb[WantedDEM].AreaName);
            DEMGlb[WantedDEM].WriteNewFormatDEM(fName, DEMGlb[WantedDEM].AreaName);
            {$IfDef MakeLandCoverGrid} if fName <> '' then WriteLineToDebugFile('New grid saved to ' + fName); {$EndIf}
         end {with};
         {$IfDef MakeLandCoverGrid}  WriteLineToDebugFile('MakeLandCoverGrid out'); {$EndIf}
      {$EndIf}
      end;

      {$Else}
      procedure MakeLandCoverGrid(WhichDEM : integer; Source : tLandCoverSource; CategoryGrid : boolean);
      begin

      end;
{$EndIf}


