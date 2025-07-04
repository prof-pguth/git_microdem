{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{     include file for nevadia_main      }
{________________________________________}


procedure Sentinel2BatchOps;
var
   Paths : tStringList;
   fName,sName,dName : PathStr;
   i,Sat,NDVIgrid,NBRgrid,db : integer;
begin
   {$IfDef FireStats} WriteLineToDebugFile('Sentinel2BatchOps in'); {$EndIf}
   HeavyDutyProcessing := true;

   Paths := tStringList.Create;
   Paths.Add('G:\debuse\sentinel_2\');
   fName := 'G:\debuse\calwood.shp';
   if GetMultipleDirectories('Landsat or Sentinel-2 image',Paths) then begin
      for i := 0 to pred(Paths.Count) do begin
         WMDEM.SetPanelText(1,'Scene: ' + IntToStr(succ(i)) + '/' + IntToStr(Paths.Count));
         wmdem.ClearStatusBarPanelText;
         LastSatDir := Paths[i];
         Sat := OpenSatImageFromDirectory(LastSatDir);
         db := SatImage[Sat].SelectionMap.LoadDataBaseFile(fName, false,true);
         dName := GetParentDirectory(LastSatDir) + 'ndvi\';
         SafeMakeDir(dName);
         sName := dName + 'Sentinel-2_' + SatImage[Sat].ImageDateString + '_ndvi.dem';
         if Not FileExists(sName) then begin
            {$IfDef FireStats} WriteLineToDebugFile('Sentinel2BatchOps  create ' + sName); {$EndIf}
            NDVIgrid := SatImage[Sat].SelectionMap.NewSatWindow(nsbNDVI);
            MaskDEMFromShapeFile(NDVIgrid,db,true,true,0,MDDef.MaskDistance);
            DEMGlb[NDVIgrid].SavePartOfDEMWithData(sName);
         end;

         dName := GetParentDirectory(LastSatDir) + 'nbr\';
         SafeMakeDir(dName);
         sName := dName + 'Sentinel-2_' + SatImage[Sat].ImageDateString + '_nbr.dem';
         if Not FileExists(sName) then begin
            {$IfDef FireStats} WriteLineToDebugFile('Sentinel2BatchOps  create ' + sName); {$EndIf}
            NBRgrid := SatImage[Sat].SelectionMap.NewSatWindow(nsbNBRNormalizedBurnIndex);
            MaskDEMFromShapeFile(NBRgrid,db,true,true,0,MDDef.MaskDistance);
            DEMGlb[NBRgrid].SavePartOfDEMWithData(sName);
         end;
         CloseAndNilNumberedDB(db);
         CloseSingleSatelliteImage(Sat);
         CloseSingleDEM(NBRgrid);
         CloseSingleDEM(NDVIgrid);
      end;
   end;
   Paths.Free;
   wmdem.ClearStatusBarPanelText;
   HeavyDutyProcessing := false;

   {$IfDef FireStats} WriteLineToDebugFile('Sentinel2BatchOps out'); {$EndIf}
end;


procedure Sentinel2Batch_dNBR;
var
   fName : PathStr;
   RefNBR,i,dNBRgrid,DEM : integer;
begin
   {$IfDef FireStats} WriteLineToDebugFile('Sentinel2BatchdNBR in'); {$EndIf}
   HeavyDutyProcessing := true;
   fName := '';
   OpenNewDEM(fName,true,'NBR grids');
   if GetDEM(RefNBR, true,' pre fire NBR for dNBR reference time') then begin
      for i := 1 to MaxDEMDataSets do if (i <> RefNBR) and ValidDEM(i) then begin
         WMDEM.SetPanelText(1,'Scene: ' + IntToStr(i) + '/' + IntToStr( MaxDEMDataSets));
         //AreaName := StringReplace(DEMglb[i].AreaName,'_nbr','_dNBR',[rfReplaceAll, rfIgnoreCase]);
         dNBRgrid := MakeDNBRMap(RefNBR,i);
         fName := StringReplace(DEMglb[i].DEMfileName,'_nbr','_dNBR',[rfReplaceAll, rfIgnoreCase]);
         DEMGlb[dNBRgrid].WriteNewFormatDEM(fName);
         //MessageToContinue('yes');
         DEM := i;
         CloseSingleDEM(DEM);
         CloseSingleDEM(dNBRgrid);
      end;
   end;
   CloseSingleDEM(RefNBR);
   wmdem.ClearStatusBarPanelText;
   HeavyDutyProcessing := false;
   {$IfDef FireStats} WriteLineToDebugFile('Sentinel2BatchdNBR out'); {$EndIf}
end;


procedure BatchResampleForDEMIX(FilesWanted : tStringList = Nil);
//aggregates to 1" DEMs, designed to be called from the command line
//June 2023, not recently tested
var
   fName : PathStr;
   i,DEM : integer;
begin
   {$If Defined(RecordIniMemoryOverwrite) or Defined(TimeLoadDEM)} IniMemOverwriteCheck('start BatchResampleForDEMIX'); {$EndIf}
   if (FilesWanted = Nil) then begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(LastDEMName);
      if not GetMultipleFiles('DEMs for aggregating',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then exit;
   end;

   try
      HeavyDutyProcessing := true;
      StartSingleThreadTimer('Create DEMIX reference DEMs');
      for i := 0 to pred(FilesWanted.Count) do begin
        ThreadTimers.UpdateThreadStats(9, round(100 * (i / FilesWanted.Count)));
        fName := FilesWanted.Strings[i];
        LoadNewDEM(DEM,fName,true);
        if ValidDEM(DEM) then begin
           ResampleForDEMIXOneSecDEMs(true,true,DEM);
           //CloseSingleDEM(DEM);
        end;
      end;
   finally
      EndThreadTimers;
      HeavyDutyProcessing := false;
   end;
end;




