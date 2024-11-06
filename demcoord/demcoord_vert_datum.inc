{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
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


procedure VerticalDatumShift(DEM : integer; vdShift : tvdShift);
var
   geoidGrid,Col,Row : Integer;
   fName : PathStr;
   TheShift : shortString;
   z1,z2  : float32;
begin
   {$IfDef RecordVertDatumShift} WriteLineToDebugFile('VerticalDatumShift in, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].ZRange); {$EndIf}
   if (vdShift = vdEGM96toEGM2008) then fName := GeoidDiffFName
   else fName := Geoid2008FName;
   GeoidGrid := OpenNewDEM(fName,false,'geoid shift');
   if ValidDEM(GeoidGrid) then begin

      case vdShift of
         vdWGS84toEGM2008 : TheShift := 'wgs84_to_egm2008';
         vdEGM2008toWGS84 : TheShift := 'egm2008_to_wgs84';
         vdEGM96toEGM2008 : TheShift := 'egm96_to_egm2008';
      end;

      if (vdShift = vdEGM2008toWGS84) then DEMGlb[GeoidGrid].MultiplyGridByConstant(-1);

      StartProgress('Datum shift');
      for Col := 0 to pred(DEMGlb[DEM].DEMHeader.NumCol) do begin
          if (Col mod 150 = 0) then UpdateProgressBar(Col/DEMGlb[DEM].DEMHeader.NumCol);
          for Row := 0 to pred(DEMGlb[DEM].DEMHeader.NumRow) do begin
             if DEMGlb[DEM].GetElevMetersFromThisAndSecondDEM(GeoidGrid,Col,Row,z1,z2) then begin
                DEMGlb[DEM].SetGridElevation(Col,Row,z1+z2);
             end;
          end;
      end;

      CloseSingleDEM(GeoidGrid);
      case vdShift of
         vdWGS84toEGM2008 : DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
         vdEGM2008toWGS84 : DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey := VertCSWGS84;
         vdEGM96toEGM2008 : DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
      end;
      DEMGlb[DEM].SelectionMap.RespondToChangedDEM;
   end;
   {$IfDef RecordVertDatumShift} WriteLineToDebugFile('VerticalDatumShift out, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].ZRange);  {$EndIf}
end;





