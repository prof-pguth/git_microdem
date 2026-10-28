
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{      include file for demmapf          }
{________________________________________}


function MapToXYZIBfile(MapDraw : tMapDraw) : PathStr;
//{$Define TrackMapColors}
{$Define Record_scaling}
{$Define Record3D}
var
   Points : ^tPointXYZIArray;
   MemReq : int64;
   x,y,NPts : integer;
   Good : boolean;
   //GeometryFName : PathStr;
   z : float32;
   xu,yu,xgrid,ygrid,Lat,Long : float64;
   Color : tPlatformColor;
   BMPMem : tBMPMemory;
   Bitmap : tMyBitmap;
   //FileList : tStringList;
   OutF : file;
   {$IfDef TrackMapColors} ColorsUsed : tStringList; {$EndIf}
   {$IfDef Record_scaling} xsmin,xsmax,ysmin,ysmax,zsmin,zsmax : float32; {$Endif}
begin
   {$If Defined(Record3D)} WriteLineToDebugFile('MapToXYZIBfile in ' + MapDraw.BaseTitle); {$EndIf}
   {$IfDef TrackMapColors} ColorsUsed := tStringList.Create; ColorsUsed.Sorted := true;  ColorsUsed.Duplicates := dupIgnore; {$EndIf}
   Bitmap := LoadBitmapFromFile(MapDraw.FullMapfName);
   BMPMem := tBMPMemory.Create(Bitmap);
   {$IfDef Record_scaling}
     xsmin := 99e38;
     xsmax := -99e38;
     ysmin := 99e38;
     ysmax := -99e38;
     zsmin := 99e38;
     zsmax := -99e38;
   {$Endif}


   MemReq := succ(MapDraw.MapXSize) * succ(MapDraw.MapXSize) * SizeOf(tPointXYZI);
   GetMem(Points,MemReq);
   NPts := 0;
   for x := 0 to pred(MapDraw.MapXSize) do begin
      for y := 0 to pred(MapDraw.MapYSize) do begin
         if ValidDEM(MapDraw.DEM2OnMap) then begin
            MapDraw.ScreenToDEMGrid(x,y,xgrid,ygrid);
            DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xgrid,ygrid,Lat,Long);
            Good := DEMGlb[MapDraw.DEM2onMap].GetElevFromLatLongDegree(Lat,Long,z);
         end
         else begin
            Good := MapDraw.ScreenToElev(x,y,z);
         end;
         if Good then begin
            MapDraw.ScreenToUTM(x,y,xu,yu);
            if (xu > 100) and (yu > 100) then begin
               Color := BMPMem.GetPixelColor(x,y);
               if not SameColor(Color, MDdef.MissingDataColor) then begin
                  {$IfDef TrackMapColors} if (ColorsUsed.Count < 1200) then ColorsUsed.Add(ColorString(Color)); {$EndIf}
                   {$IfDef Record_scaling}
                      CompareValueToExtremes(xu,xsmin,xsmax);
                      CompareValueToExtremes(yu,ysmin,ysmax);
                      CompareValueToExtremes(z,zsmin,zsmax);
                   {$Endif}

                  inc(NPts);
                  Points^[Npts].x := xu;
                  Points^[NPts].y := yu;
                  Points^[NPts].z := z;
                  Points^[NPts].Int1 := Color.rgbtRed;
                  Points^[NPts].Int2 := Color.rgbtGreen;
                  Points^[NPts].Int3 := Color.rgbtBlue;
               end;
            end;
         end;
      end;
   end;
   BMPMem.Destroy;
   Bitmap.Destroy;
   Result := Petmar.NextFileNumber(MDTempDir, 'Map3D_','.xyzib');
   AssignFile(Outf,Result);
   rewrite(Outf,1);
   BlockWrite(OutF,Points^,NPts * SizeOf(tPointXYZI));
   CloseFile(outf);
   FreeMem(Points,MemReq);
   {$If Defined(Record3D)} WriteLineToDebugFile('MapTo3DView over, pts= ' + IntToStr(NPts) + '  in  ' + Result); {$EndIf}
   {$IfDef Record_scaling}
        WriteLineToDebugFile('MapTo3DView over,  x:' + RealToString(xsmin,-12,-2) + ' to ' + RealToString(xsmax,-12,-2) +
                             '  y:' + RealToString(ysmin,-12,-2) + ' to ' + RealToString(ysmax,-12,-2) +
                             '  z:' + RealToString(zsmin,-12,-2) + ' to ' + RealToString(zsmax,-12,-2));
   {$Endif}
end;


function XYZIBfromMap(MapForm : tMapForm) : PathStr;
begin
   if MDDef.OpenGLCleanOverlays then begin
      SaveBackupDefaults;
      NakedMapOptions;
      MapForm.DoFastMapRedraw;
   end;
   {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview in'); {$EndIf}
   MapForm.MapDraw.MapCorners.BoundBoxUTM := MapForm.MapDraw.GetBoundBoxUTM;
   {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview got bound box'); {$EndIf}
   Result := MapToXYZIBfile(MapForm.MapDraw);
   if MDDef.OpenGLCleanOverlays then begin
      RestoreBackupDefaults;
      MapForm.DoFastMapRedraw;
   end;
end;


procedure CopyMapTo3Dview(MapForm : tMapForm);
var
   FileList : tStringList;
begin
   SetColorForProcessing;
   FileList := tStringList.Create;
   FileList.Add(XYZIBfromMap(MapForm));
   FMX3dViewer(false,FileList);
   {$IfDef Record3d} WriteLineToDebugFile('CopyMapTo3Dview out'); {$EndIf}
   SetColorForWaiting;
end;

procedure DrapeMultipleMapsOnThisDEM(MapForm : tMapForm);
var
   FileList : tStringList;
   i : integer;
   DEMsWanted : tDEMbooleanArray;
 begin
   GetMultipleDEMsFromList('Grids to drape on ' + DEMGlb[MapForm.MapDraw.DEMonMap].AreaName  + '(max ' + IntToStr(pred(MaxClouds)) + ')',DEMsWanted);
   SetColorForProcessing;
   FileList := tStringList.Create;
   FileList.Add(XYZIBfromMap(MapForm));
   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] and ValidDEM(i) and (i <> MapForm.MapDraw.DEMonMap) then begin
         //MatchAnotherMapThisCoverageArea(MapForm,DEMGlb[i].SelectionMap);
         MapForm.MapDraw.DEMonMap := DEMGlb[i].SelectionMap.MapDraw.DEM2onMap;
         FileList.Add(XYZIBfromMap(MapForm));
         MapForm.MapDraw.DEMonMap := 0;
      end;
   end;
   FMX3dViewer(false,FileList);
   SetColorForWaiting;

   {$IfDef Record3d} WriteLineToDebugFile('DrapeMultipleMapsOnThisDEM out'); {$EndIf}
end;

procedure Make3DviewMultipleMaps(MapForm : tMapForm);
var
   FileList : tStringList;
   Maps : tStringList;
   i,j,Num : integer;
begin
   Maps := PickMaps('Maps for 3D view (max ' + IntToStr(MaxClouds) + ')');
   if (Maps.Count > 0) then begin
      SetColorForProcessing;
      FileList := tStringList.Create;
      FileList.Add(XYZIBfromMap(MapForm));
      for i := 0 to pred(WMDEM.MDIChildCount) do begin
         if WMDEM.MDIChildren[i] is tMapForm and (WmDEM.MDIChildren[i].Handle <> MapForm.Handle) then begin
            for j := 0 to pred(Maps.Count) do begin
               if (Maps.Strings[j] = (WMDEM.MDIChildren[i] as TMapForm).Caption) then begin
                  MatchAnotherMapThisCoverageArea(MapForm,(WMDEM.MDIChildren[i] as TMapForm));
                  FileList.Add(XYZIBfromMap((WMDEM.MDIChildren[i] as TMapForm)));
               end;
            end;
         end;
      end;
      FMX3dViewer(false,FileList);
      SetColorForWaiting;

   end;
   {$IfDef Record3d} WriteLineToDebugFile('DrapeMultipleMapsOnThisDEM out'); {$EndIf}
end;

procedure Make3DviewSelectedDEMs;
var
   i : integer;
   FileList : tStringList;
   DEMsWanted : tDEMbooleanArray;
begin
   GetMultipleDEMsFromList('Grids for 3D view (max ' + IntToStr(MaxClouds) + ')',DEMsWanted);
   SetColorForProcessing;
   FileList := tStringList.Create;
   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] then begin
         FileList.Add(XYZIBfromMap((DEMGlb[i].SelectionMap)))
      end;
   end;
   SetColorForWaiting;
end;


procedure TraceRoute3D(DBonTable : integer);
var
   OutForm : TView3DForm;
   Lat,Long,xutm,yutm : float64;
   z : float32;
begin
   MessageToContinue('Disabled');
(*
   if GISdb[DBonTable].LayerIsOn then GISdb[DBonTable].ToggleLayer(false);
   GISdb[DBonTable].theMapOwner.Image1.Canvas.Pixels[0,0] := clRed;
   OutForm := MapTo3DView(GISdb[DBonTable].TheMapOwner.MapDraw,10000);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         if DEMGlb[GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
            GISdb[DBonTable].theMapOwner.MapDraw.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
            OutForm.AddPointWithSpecificColor(xutm,yutm,z+1{,0,0});
            OutForm.AddPointWithSpecificColor(xutm+1,yutm+1,z+1{,0,0});
            OutForm.AddPointWithSpecificColor(xutm+1,yutm-1,z+1{,0,0});
            OutForm.AddPointWithSpecificColor(xutm-1,yutm+1,z+1{,0,0});
            OutForm.AddPointWithSpecificColor(xutm-1,yutm-1,z+1{,0,0});
            Delay(250);
            OutForm.Activate;
            if (GISdb[DBonTable].MyData.GetFieldByNameAsString('HEADING') <> '') then begin
               OutForm.Layout3D1.RotationAngle.Y := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('HEADING') + 90;
            end;
            OutForm.Show;
         end;
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   OutForm.Show;
   GISdb[DBonTable].ShowStatus;
*)
end;



      procedure ExplorexyziFile(fName : PathStr);
      var
         Points : ^tPointXYZIArray;
         tfile : File;
         Pts : integer;
         i : integer;
         c : TAlphaColorRec;
         Results : tStringList;
      begin
           {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('LoadFromMICRODEMxyzi in ' + fName); {$EndIf}
           if FileExists(fName) then begin
              Results := tStringList.Create;
              Results.Add('X,Y,Z,RED, GREEN,BLUE');
              assignFile(tfile,fName);
              reset(tFile,sizeOf(tPointXYZI));
              new(Points);
              BlockRead(tfile,Points^,MaxPts,Pts);
              CloseFile(tFile);
              i := 1;
              while i <= Pts do begin
                  Results.Add(RealToString(Points^[i].x,-12,-2) + ',' + RealToString(Points^[i].y,-12,-2) + ',' + RealToString(Points^[i].z,-12,-2) + ',' +
                      IntToStr(Points^[i].int1) + ',' + IntToStr(Points^[i].int2) + ',' + IntToStr(Points^[i].int3) );
                  inc(i,25);
              end;
              Dispose(Points);
              StringList2CSVtoDB(Results,MDTempDir + ExtractFileNameNoExt(fName) + '.dbf');
           end;
      end;







