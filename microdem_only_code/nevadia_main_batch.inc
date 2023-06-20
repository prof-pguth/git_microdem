{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{ include file for nevadia_main      }
{____________________________________}


procedure Sentinel2BatchOps;
var
   Paths : tStringList;
   fName,sName : PathStr;
   i,Sat,NDVIgrid,NBRgrid,db : integer;
begin
   Paths := tStringList.Create;
   Paths.Add(LastSatDir);
   fName := 'H:\mid_projects\debuse\calwood.shp';
   if GetMultipleDirectories('Landsat or Sentinel-2 image',Paths) then begin
      for i := 0 to pred(Paths.Count) do begin
         LastSatDir := Paths[i];
         Sat := OpenSatImageFromDirectory(LastSatDir);
         db := SatImage[Sat].SelectionMap.LoadDataBaseFile(fName, false,true);
         NDVIgrid := SatImage[Sat].SelectionMap.NewSatWindow(nsbNDVI);
         MaskDEMFromShapeFile(NDVIgrid,db,true,true,0,MDDef.MaskDistance);
         sName := GetParentDirectory(LastSatDir) + 'ndvi\';
         SafeMakeDir(sName);
         sName := sName + SatImage[Sat].SceneBaseName + '_ndvi.dem';
         DEMGlb[NDVIgrid].SavePartOfDEMWithData(sName);

         NBRgrid := SatImage[Sat].SelectionMap.NewSatWindow(nsbNBRNormalizedBurnIndex);
         MaskDEMFromShapeFile(NBRgrid,db,true,true,0,MDDef.MaskDistance);
         sName := GetParentDirectory(LastSatDir) + 'nbr\';
         SafeMakeDir(sName);
         sName := sName + SatImage[Sat].SceneBaseName + '_nbr.dem';
         DEMGlb[NDVIgrid].SavePartOfDEMWithData(sName);

         CloseSingleSatelliteImage(Sat);
      end;
   end;
   Paths.Free;
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
           ResampleForDEMIXOneSecDEMs(DEM);
           CloseSingleDEM(DEM);
        end;
      end;
   finally
      EndThreadTimers;
      HeavyDutyProcessing := false;
   end;
end;


//some of these have the file names hard coded
//this was designed for a particular purpose, and might not be generally applicable
//from this point on, experimental routines not likely to work fully or correctly

procedure CreateDEMsfromLidar;
var
   CloudFNames,DEMsToCreate : PathStr;
   Clouds : tStringList;
   I : Integer;
begin
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click in'); {$EndIf}
   StopSplashing;
   CloudFNames := 'c:\microdem\clouds.txt';
   DEMsToCreate := 'c:\microdem\match_tandemx_dems.dbf';
   Clouds := tStringList.Create;
   Clouds.LoadFromFile(CloudFNames);
   for I := 0 to pred(Clouds.Count) do begin
      {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Clouds.Strings[i]'); {$EndIf}
       if PathIsValid(Clouds.Strings[i]) then begin
          OvelayPointClouds(Nil,Clouds.Strings[i]);
          if (pt_cloud_opts_fm.LasFiles[1].CloudName <> '') then begin
             pt_cloud_opts_fm.AutoSaveDir := 'c:\temp\';
             pt_cloud_opts_fm.DEMrulesFName := DEMstoCreate;
             pt_cloud_opts_fm.BitBtn50Click(Nil);
          end
          else begin
             {$IfDef RecordProblems} HighlightLineToDebugFile('No LAS files in ' + Clouds.Strings[i]); {$EndIf}
          end;
          pt_cloud_opts_fm_Close;
          CloseAllWindowsAndData;
       end
       else begin
          {$IfDef RecordProblems} HighlightLineToDebugFile('Invalid dirctory  ' + Clouds.Strings[i]); {$EndIf}
       end;
   end;
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click out'); {$EndIf}
end;





