{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}



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





