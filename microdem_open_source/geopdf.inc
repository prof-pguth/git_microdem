



   {$If Defined( ExDRGimport) or Defined(ExGeoPDF)}
   {$Else}
      procedure GDALconvertGeoPDF(Option : tGDALGeoPDF);
      var
         BatchName,fName,MergeFName,tName : PathStr;
         lt,Code,bfn,UseThreads : integer;
         UTMzone,i : int16;
         BatchFile,
         OutNames,
         FilesWanted : tStringList;
         DefFilter : byte;

            procedure AllGeoPDFLayersOff(var KeepOn : boolean);
            begin
               MDDef.DRGCollar := false;
               MDDef.DRGStructures := false;
               MDDef.DRGTransport := false;
               MDDef.DRGHydrography := false;
               MDDef.DRGShadedRelief := false;
               MDDef.DRGBoundaries := false;
               MDDef.DRGOrthos := false;
               MDDef.DRGGrid := false;
               MDDef.DRGContours := false;
               MDDef.DRGWoodland := false;
               KeepOn := true;
            end;

            function ConvertGeoPDFtoGeoTiff(fName,OutName : PathStr) : ANSIString;
            var
               ExtraOptions : ANSIString;
               TempName,ClipName : PathStr;
            begin
                 if FileExists(OutName) and (Uppercase(ExtractFilePath(OutName)) <> UpperCase(MDTempDir)) then begin
                    if AnswerIsYes(OutName + ' exists; Overwrite') then Sysutils.DeleteFile(OutName)
                    else exit;
                 end;
                 ExtraOptions := ' --config GDAL_PDF_LAYERS ';
                 if MDDef.DRGCollar then ExtraOptions := ExtraOptions + 'Map_Collar,Map_Collar.Map_Elements,';
                 if MDDef.DRGGrid then ExtraOptions := ExtraOptions + 'Map_Frame,Map_Frame.Projection_and_Grids,Map_Frame.Geographic_Names,';
                 if MDDef.DRGStructures then ExtraOptions := ExtraOptions + 'Map_Frame.Structures,';
                 if MDDef.DRGTransport then ExtraOptions := ExtraOptions + 'Map_Frame.Transportation,Map_Frame.Transportation.Road_Features,Map_Frame.Transportation.Trails,Map_Frame.Transportation.Railroads,Map_Frame.Transportation.Airports,';
                 if MDDef.DRGHydrography then ExtraOptions := ExtraOptions + 'Map_Frame.Hydrography,';
                 if MDDef.DRGContours then ExtraOptions := ExtraOptions + 'Map_Frame.Terrain,Map_Frame.Terrain.Contours,';
                 if MDDef.DRGPLSS then ExtraOptions := ExtraOptions + 'Map_Frame.PLSS';
                 if MDDef.DRGWoodland then ExtraOptions := ExtraOptions + 'Map_Frame.Woodland,';
                 if MDDef.DRGShadedRelief then ExtraOptions := ExtraOptions + 'Map_Frame.Terrain.Shaded_Relief,';
                 if MDDef.DRGBoundaries then ExtraOptions := ExtraOptions + 'Map_Frame.Boundaries,Map_Frame.Boundaries.Jurisdictional_Boundaries,Map_Frame.Boundaries.Jurisdictional_Boundaries.International,' +
                      'Map_Frame.Boundaries.Jurisdictional_Boundaries.State_or_Territory,Map_Frame.Boundaries.Jurisdictional_Boundaries.County_or_Equivalent,Map_Frame.Boundaries.Federal_Administered_Lands,' +
                      'Map_Frame.Boundaries.Federal_Administered_Lands.National_Park_Service,Map_Frame.Boundaries.Federal_Administered_Lands.Department_of_Defense,Map_Frame.Boundaries.Federal_Administered_Lands.Forest_Service,';
                 if MDDef.DRGOrthos then ExtraOptions := ExtraOptions + 'Images,Images.Orthoimage,';
                 Delete(ExtraOptions,length(ExtraOptions),1);

                 TempName := MDTempDir + {'temp_' +} ExtractFileName(OutName);
                 if MDDef.DRGQuadClip then ClipName := ' -crop_to_cutline  -cutline ' + MDTempDir + ExtractFileNameNoExt(fName) + '_clip.csv '
                 else ClipName := '';
                 Result := GDAL_translate_name + ' -of Gtiff ' + fName + ' ' + TempName + ExtraOptions;
                 {$IfDef RecordGeoPDF} WriteLineToDebugFile('ConvertGeoPDFtoGetTiff cmd=' + Result); {$EndIf}
                 BatchFile.Add('REM     ');
                 BatchFile.Add('REM     create   ' + TempName);
                 BatchFile.Add(Result);
                 BatchFile.Add(GDAL_warp_name + ' -dstnodata "255 255 255" -of Gtiff' + ' -t_srs EPSG:' + IntToStr(Code) + ' ' + ClipName + TempName + ' ' + OutName);
            end;

             procedure ExtractByLayers;

                      procedure SingleLayer(var WhichLayer : boolean; OutName : PathStr);
                      begin
                         AllGeoPDFLayersOff(WhichLayer);
                         ConvertGeoPDFtoGeoTiff(fName,OutName);
                      end;

             begin
                MDDef.DRGQuadClip := false;
                SingleLayer(MDDef.DRGGrid,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_grid.tif');
                SingleLayer(MDDef.DRGContours,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_contours.tif');
                SingleLayer(MDDef.DRGCollar, ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_collar.tif');
                SingleLayer(MDDef.DRGStructures,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_structures.tif');
                SingleLayer(MDDef.DRGTransport,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_transport.tif');
                SingleLayer(MDDef.DRGHydrography,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_hydrography.tif');
                SingleLayer(MDDef.DRGShadedRelief,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_shaded_relief.tif');
                SingleLayer(MDDef.DRGWoodland,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_woodland.tif');
                SingleLayer(MDDef.DRGBoundaries,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_boundaries.tif');
                SingleLayer(MDDef.DRGOrthos,ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_orthos.tif');
             end;


      begin
         {$IfDef RecordGeoPDF} WriteLineToDebugFile('OpenGeoPDF in'); {$EndIf}
         if IsGDALFilePresent(GDAL_translate_name) then begin
            SaveBackupDefaults;
            if (option=gdalOpenGeoPDFimagelayer1) then AllGeoPDFLayersOff(MDDef.DRGOrthos);
            FilesWanted := tStringList.Create;
            FilesWanted.Add(MainMapData);
            DefFilter := 1;
            if GetMultipleFiles('GeoPDF','*.PDF',FilesWanted,DefFilter) then begin
              MergefName := '';

              if (FilesWanted.Count > 1) and (option=gdalMergeGeoPDF1) then begin
                MergefName := MDTempDir;
                Petmar.GetFileNameDefaultExt('Merged maps','*.tif',MergefName);
              end;
              {$IfDef RecordGeoPDF} WriteLineToDebugFile('Files picked');   {$EndIf}

              OutNames := tStringList.Create;
              BatchGDALinfo(FilesWanted,MDDef.DRGQuadClip,UTMzone);
              {$IfDef RecordGeoPDF} WriteLineToDebugFile('BatchGDALinfo done'); {$EndIf}
               Code := 26900 + UTMZone;

               StartGDALbatchFile(BatchFile);
               BatchFile.Add('REM     convert GeoPDF');
               if (FilesWanted.Count < MDDef.MaxThreadsForPC) then UseThreads := FilesWanted.Count
               else UseThreads := MDDef.MaxThreadsForPC;
               lt := FilesWanted.Count div UseThreads;
               if (lt = 0) then lt := 1;
               bfn := 0;
               for i := 0 to pred(FilesWanted.Count) do begin
                  fName := FilesWanted.Strings[i];
                  BatchFile.Add('REM  GeoPDF ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count) );
                  if (option = gdalAllindividuallayers1) then begin
                     ExtractByLayers;
                  end
                  else begin
                     if (MergeFName = '') then tName := ChangeFileExt(fName,'.tif')
                     else tName := MDTempDir + 'f' + IntToStr(succ(i)) + '.tif';
                     OutNames.Add(tName);
                     if not FileExists(tName) then ConvertGeoPDFtoGeoTiff(fName,tName);
                  end;

                  if (Succ(i) mod lt = 0) or (i=pred(FilesWanted.Count)) then begin
                     inc(bfn);
                     BatchName := MDTempDir + 'convert' + IntToStr(bfn) + '.bat';
                     BatchFile.Add('del ' + MDTempDir + IntToStr(bfn) + '.tmp');
                     BatchFile.Add('exit');
                     BatchFile.SaveToFile(BatchName);
                     BatchFile.Clear;
                     BatchFile.Add(SetGDALdataStr);
                     BatchFile.Add('REM convert GeoPDF');
                  end;
               end;
               FilesWanted.Free;
               BatchFile.Clear;
               for I := 1 to bfn do begin
                  BatchFile.Add('echo %time% > ' + MDTempDir + IntToStr(i) + '.tmp');
                  BatchFile.Add('start "' + IntToStr(i) + '" ' + MDTempDir + 'convert' + IntToStr(i) + '.bat');
               end;
               BatchFile.Add('@ping -n 1 127.0.0.1 > nul');
               BatchFile.Add(':loop');
               BatchFile.Add('@echo Processing......');
               BatchFile.Add('if not exist *.tmp goto :next');
               BatchFile.Add('    @ping -n 5 127.0.0.1 > nul');
               BatchFile.Add('goto loop');
               BatchFile.Add(':next');
               BatchFile.Add('@echo Done Processing!');
               BatchFile.Add('exit');
               BatchName := MDTempDir + 'All_convert.bat';
               BatchFile.SaveToFile(BatchName);
               WinExecAndWait32(BatchName);
               {$IfDef RecordGeoPDF} WriteLineToDebugFile('Translate and warp batch file finished'); {$EndIf}
               if (MergeFName = '') then begin
                  for i := 0 to pred(OutNames.Count) do begin
                     OpenAndDisplayNewScene(Nil,OutNames[i],true,true,true);
                  end;
               end
               else begin
                  CallGDALmerge(MergefName,OutNames);
                  OpenAndDisplayNewScene(Nil,MergefName,true,true,true);
               end;
              BatchFile.Free;
              OutNames.Free;
            end;
            RestoreBackupDefaults;
         end;
         {$IfDef RecordGeoPDF} WriteLineToDebugFile('OpenGeoPDF out'); {$EndIf}
      end;
    {$EndIf}
