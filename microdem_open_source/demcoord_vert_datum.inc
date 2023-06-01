{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{ include file for demcoord          }
{____________________________________}



procedure VerticalDatumShiftWithVDATUM(AreaName : shortstring; DEM,db : integer; SaveName : PathStr; ErrorLog : tStringList = nil);
var
  dx,dy,dz : float32;
begin
   if db = 0 then begin
      db := OpenMultipleDataBases('VDATUM results','',false);
   end;
   if ValidDB(db) then begin
      dx := GISdb[db].MyData.FieldAverage('X_SHIFT');
      dy := GISdb[db].MyData.FieldAverage('Y_SHIFT');
      dz := GISdb[db].MyData.FieldAverage('VERT_SHIFT');
      if (abs(dx) < 0.01) and (abs(dy) < 0.01) and (abs(dz) < 0.01) then begin
         ErrorLog.Add(AreaName + ' no change in dx, dy, and dz');
      end
      else begin
         if (DEM = 0) then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load ' + SaveName); {$EndIf}
            DEM := OpenNewDEM(SaveName,false);
         end;
         if (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey <> VertCSNAVD88) then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM not NAVD88; VDATUM invalid'); {$EndIf}
         end
         else if (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM2008) then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM already EGM2008=' + IntToStr(DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey)); {$EndIf}
         end
         else begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM was ' + IntToStr(DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey)); {$EndIf}
            DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
            DEMGlb[DEM].DEMHeader.DEMSWCornerX := DEMGlb[DEM].DEMHeader.DEMSWCornerX + dx;
            DEMGlb[DEM].DEMHeader.DEMSWCornerY := DEMGlb[DEM].DEMHeader.DEMSWCornerY + dy;
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('For EGM2008 added dz =' + RealToString(dz,-8,-2)); {$EndIf}
            DEMGlb[DEM].AddConstantToGrid(dz);
            if (SaveName <> '') then begin
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Writing DEM format ' + SaveName); {$EndIf}
               DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName);
               CloseSingleDEM(DEM);
            end;
         end;
      end;
      CloseAndNilNumberedDB(db);
   end;
end;


procedure VerticalDatumShiftWithGDAL(DEM : integer; var SaveName : PathStr);
//obviously hard wired for testing
var
   BatchFile : tStringList;
   aName,InName : PathStr;
   cmd,SRSstring,tsrsstring : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL in'); {$EndIf}
   if (SaveName = '') then begin
      if not GetFileNameDefaultExt('Vertical shifted DEM','*.tif',SaveName) then begin
         exit;
      end;
   end;
   InName := DEMGlb[DEM].GeotiffDEMName;

   if (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM96) then begin
      SRSstring := ' -s_srs EPSG:4326+5773';
      tsrsstring := ' -t_srs EPSG:4326+3855';
   end
   else begin
      SRSstring := ' -s_srs EPSG:26911+5703';
      tsrsstring := ' -t_srs EPSG:32611+3855';
   end;

   StartGDALbatchFile(BatchFile);
   cmd := 'gdalwarp -overwrite -ot float32 ' + InName + ' ' + SaveName + SRSString + tsrsstring;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL cmd=' + cmd); {$EndIf}
   BatchFile.Add(cmd);
   aName :=  Petmar.NextFileNumber(MDTempDir, 'gdal_datumshift_','.bat');
   EndBatchFile(aName,BatchFile);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL out'); {$EndIf}
end;


procedure VerticalDatumShift(DEM : integer; vdShift : tvdShift);
var
   i,geoidGrid : Integer;
   Merge : tDEMbooleans;
   fName : PathStr;
   TheShift : shortString;
begin
   {$IfDef RecordVertDatumShift} WriteLineToDebugFile('VerticalDatumShift in, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].ZRange);  {$EndIf}
   GetGeoid;
   if (vdShift = vdEGM96toEGM2008) then fName := GeoidDiffFName
   else fName := Geoid2008FName;
   GeoidGrid := OpenNewDEM(fName,false);

   case vdShift of
      vdWGS84toEGM2008 : TheShift := 'gs84_to_egm2008';
      vdEGM2008toWGS84 : TheShift := 'egm2008_to_wgs84';
      vdEGM96toEGM2008 : TheShift := 'egm96_to_egm2008';
   end;

   if (vdShift = vdEGM2008toWGS84) then DEMGlb[GeoidGrid].MultiplyGridByConstant(-1);
   for i := 1 to MaxDEMDataSets do Merge[i] := false;
   Merge[DEM] := true;
   Merge[GeoidGrid] := true;
   SumDEMs(DEM, Merge,DEMGlb[DEM].AreaName + '_vdatum_shift_' + TheShift);
   CloseSingleDEM(GeoidGrid);
   case vdShift of
      vdWGS84toEGM2008 : DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
      vdEGM2008toWGS84 : DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey := VertCSWGS84;
      vdEGM96toEGM2008 : DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
   end;
   DEMGlb[DEM].CheckMaxMinElev;
   {$IfDef RecordVertDatumShift} WriteLineToDebugFile('VerticalDatumShift out, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].ZRange);  {$EndIf}
end;





