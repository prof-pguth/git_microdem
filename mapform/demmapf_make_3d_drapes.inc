
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{      include file for demmapf          }
{________________________________________}


procedure CopyMapTo3Dview(MapForm : tMapForm);
begin
   {$IfDef ExFMX3D}
   {$Else}
      {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview in'); {$EndIf}
      MapForm.MapDraw.MapCorners.BoundBoxUTM := MapForm.MapDraw.GetBoundBoxUTM;
      {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview got bound box'); {$EndIf}
      if MDDef.OpenGLCleanOverlays then begin
         SaveBackupDefaults;
         NakedMapOptions;
         MapForm.DoFastMapRedraw;
      end;
      {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview call Map3D'); {$EndIf}
      Map3d := MapTo3DView(MapForm.MapDraw);
      {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview back Map3D'); {$EndIf}
      if MDDef.OpenGLCleanOverlays then begin
         RestoreBackupDefaults;
         MapForm.DoFastMapRedraw;
      end;
      {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview out'); {$EndIf}
   {$EndIf}
end;

procedure DrapeMultipleMapsOnThisDEM(MapForm : tMapForm);
var
   i : integer;
   DEMsWanted : tDEMbooleanArray;
   Viewer : TView3DForm;
 begin
   {$IfDef Record3d} WriteLineToDebugFile('TMapForm.Selectmultiplegrids1Click in'); {$EndIf}
   GetMultipleDEMsFromList('Grids to drape on ' + DEMGlb[MapForm.MapDraw.DEMonMap].AreaName  + '(max ' + IntToStr(pred(MaxClouds)) + ')',DEMsWanted);
   Viewer := MapTo3DView(MapForm.MapDraw);
   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] and ValidDEM(i) and (i <> MapForm.MapDraw.DEMonMap) then begin
         DEMGlb[i].SelectionMap.MapDraw.DEM2onMap := MapForm.MapDraw.DEMonMap;
         Viewer.AddMap(DEMGlb[i].SelectionMap.MapDraw);
      end;
   end;
   {$IfDef Record3d} WriteLineToDebugFile('TMapForm.Selectmultiplegrids1Click out'); {$EndIf}
end;

procedure Make3DviewMultipleMaps(MapForm : tMapForm);
var
   Viewer : TView3DForm;
   Maps : tStringList;
   i,j,Num : integer;
begin
   PickMaps(Maps,'Maps for 3D view (max ' + IntToStr(MaxClouds) + ')');
   if (Maps.Count > 0) then begin
      MapForm.MatchThiscoverageareaandsamepixelsize1Click(Nil);
      Viewer := MapTo3DView(MapForm.MapDraw);
      Num := 1;
      for i := 0 to pred(WMDEM.MDIChildCount) do begin
         if WMDEM.MDIChildren[i] is tMapForm and (WmDEM.MDIChildren[i].Handle <> MapForm.Handle) then begin
            for j := 0 to pred(Maps.Count) do begin
               if (Num < MaxClouds) and (Maps.Strings[j] = (WMDEM.MDIChildren[i] as TMapForm).Caption) then begin
                  inc(Num);
                  Viewer.AddMap((WMDEM.MDIChildren[i] as TMapForm).MapDraw);
               end;
            end;
         end;
      end;
   end;
   Maps.Free;
end;

procedure Make3DviewSelectedDEMs;
var
   i{,DEM1,DEM2} : integer;
   DEMsWanted : tDEMbooleanArray;
   Viewer : TView3DForm;
 begin
   GetMultipleDEMsFromList('Grids for 3D view (max ' + IntToStr(MaxClouds) + ')',DEMsWanted);
   Viewer := nil;
   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] then begin
         if (Viewer = Nil) then Viewer := MapTo3DView(DEMGlb[i].SelectionMap.MapDraw)
         else Viewer.AddMap(DEMGlb[i].SelectionMap.MapDraw);
      end;
   end;
end;

