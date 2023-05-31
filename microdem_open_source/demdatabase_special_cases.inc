{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}
{ include file for demdatabase       }
{____________________________________}


procedure ComputeVDatumShift(dbOnTable : integer);
var
   Lat,Long,Lat2,Long2 : float64;
   Distance,Bearing : float64;
   zPresent,FileConvert : boolean;
   LocalToWGS84,WGS84toEGM2008 : integer;
   z,z2,AddZ,SubZ : float32;
begin
   with GISdb[DBonTable] do begin
      EmpSource.Enabled := false;
      AddFieldToDataBase(ftFloat,'VERT_SHIFT',8,2);
      AddFieldToDataBase(ftFloat,'HORZ_SHIFT',8,2);
      AddFieldToDataBase(ftFloat,'X_SHIFT',8,3);
      AddFieldToDataBase(ftFloat,'Y_SHIFT',8,3);
      AddFieldToDataBase(ftFloat,'HORIZ_AZ',8,1);
      zPresent := MyData.FieldExists('ELEV') and MyData.FieldExists('ELEV2');
      FileConvert := FileExists(GeoidWGS84ellipsoidToLocalVDatum) and FileExists(Geoid2008FName);
      FileConvert := false;
      if FileConvert then begin
         LoadDatumShiftGrids(LocalToWGS84,WGS84toEGM2008);
         AddFieldToDataBase(ftFloat,'GRID_VERT',8,2);
      end;

      EmpSource.Enabled := false;
      MyData.First;
      while not MyData.EOF do begin
         MyData.Edit;
         if MyData.ValidLatLongFromTable(Lat,Long) and ValidLat2Long2FromTable(Lat2,Long2) then begin
            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
            MyData.SetFieldByNameAsFloat('HORZ_SHIFT',Distance);
            MyData.SetFieldByNameAsFloat('X_SHIFT',Distance*CosDeg(Bearing));
            MyData.SetFieldByNameAsFloat('Y_SHIFT',Distance*SinDeg(Bearing));
            MyData.SetFieldByNameAsFloat('HORIZ_AZ',Bearing);
            if zPresent then begin
               z := MyData.GetFieldByNameAsFloat('ELEV');
               z2 := MyData.GetFieldByNameAsFloat('ELEV2');
               MyData.SetFieldByNameAsFloat('VERT_SHIFT',(z2-z));
            end;
            if FileConvert then begin
               if DEMGlb[WGS84toEGM2008].GetElevFromLatLongDegree(Lat,Long,AddZ) and DEMGlb[LocalToWGS84].GetElevFromLatLongDegree(Lat,Long,SubZ) then begin
                  MyData.SetFieldByNameAsFloat('GRID_VERT',(-SubZ+AddZ));
               end;
            end;
         end;
         MyData.Next;
      end;
   end;
   if FileConvert then begin
      CloseSingleDEM(LocalToWGS84);
      CloseSingleDEM(WGS84toEGM2008);
   end;
end;


function AnalyzeVDatumShift(CSVName : PathStr; ErrorLog : tStringList = Nil) : integer;
var
   fName2,fName : Pathstr;
   f1,f2,Merged : tStringList;
   TStr : ShortString;
   j : integer;
begin
   if (CSVName = '') then GetFileFromDirectory('VDATUM output file','*.csv',CSVName);
   fName2 := ChangeFileExt(CSVName, '.dbf');
   if FileExists(fName2) then begin
      OpenNumberedGISDataBase(Result,fName2);
   end
   else begin
      fName := ExtractFilePath(CSVName) + 'result\' + ExtractFileName(CSVName);  //if VDATUM run on local machine
      if not FileExists(fName) then begin
         fName := ExtractFilePath(CSVName) + ExtractFileNameNoExt(CSVName) + '_result' + ExtractFileExt(CSVName);  //VDATUM run on web
         if not FileExists(fName) then begin
            TStr := 'VDATUM not run yet for ' + CSVName;
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
            if ErrorLog <> Nil then ErrorLog.Add(TStr);
            Result := 0;
            exit;
         end;
      end;
      f1 := tStringList.Create;
      f1.LoadFromFile(CSVName);
      f2 := tStringList.Create;
      f2.LoadFromFile(fName);
      Merged := tStringList.Create;
      Merged.Add('LONG,LAT,ELEV,LONG2,LAT2,ELEV2');
      for j := 1 to pred(f1.Count) do begin
         Merged.Add(f1.Strings[j] + ',' + f2.Strings[j]);
      end;
      f1.Free;
      f2.Free;
      Result := StringList2CSVtoDB(Merged,fName2);
      ComputeVDatumShift(Result);
   end;
end;





{$IfDef ExSidescan}
{$Else}
(*
      procedure GetCoverageCorners(theData : tMyData; var CornerLats,CornerLongs: array of float64);
      var
         SidescanRunlength ,Lat,Long,Lat2,Long2,Layback,LineHeading,Range : float64;
      begin
         Lat := theData.GetFieldByNameAsFloat('LAT');
         Long := theData.GetFieldByNameAsFloat('LONG');
         Lat2 := theData.GetFieldByNameAsFloat('LAT2');
         Long2 := theData.GetFieldByNameAsFloat('LONG2');
         LineHeading := theData.GetFieldByNameAsFloat('HEADING');
         Range := theData.GetFieldByNameAsFloat('RANGE');
         Layback := theData.GetFieldByNameAsFloat('LAYBACK');
         SidescanRunlength := theData.GetFieldByNameAsFloat('LENGTH');

         VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,SidescanRunlength,LineHeading);
         if (Range = 0) then Range := 25;
         VincentyPointAtDistanceBearing(Lat,Long,Layback,LineHeading-180,Lat,Long);
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading-90,CornerLats[0],CornerLongs[0]);   //upper left
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading+90,CornerLats[1],CornerLongs[1]);  //upper right
         VincentyPointAtDistanceBearing(Lat2,Long2,Layback,LineHeading-180,Lat,Long);
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading-90,CornerLats[3],CornerLongs[3]);   //lower left
         VincentyPointAtDistanceBearing(Lat,Long,Range,LineHeading+90,CornerLats[2],CornerLongs[2]);   //lower right
      end;


      procedure TGISdataBaseModule.PlotSingleSideScanLeg(Bitmap : tMyBitmap);
      var
         CornerLats,CornerLongs : array[1..4] of float64;
         PolyLinePoints  : ^tPolyLinePts;
         i : integer;
      begin
         New(PolyLinePoints);
         GetCoverageCorners(MyData,CornerLats,CornerLongs);
         for i := 1 to 4 do TheMapOwner.MapDraw.LatLongDegreeToScreen(CornerLats[i],CornerLongs[i],PolyLinePoints^[pred(i)].x,PolyLinePoints^[pred(i)].y);
         Bitmap.Canvas.Polygon(Slice(PolyLinePoints^,4));
         Dispose(PolyLinePoints);
      end;

      procedure TGISdataBaseModule.PlotSideScanCoverage(Bitmap : tMyBitmap);
      begin
         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.Pen.Width := 1;
         Bitmap.Canvas.Pen.Color := clRed;
         MyData.First;
         while not MyData.eof do begin
            PlotSingleSideScanLeg(Bitmap);
            MyData.Next;
         end;
      end;
*)
{$EndIf}



{$IfDef ExGeography}
{$Else}
procedure tGISdataBaseModule.PlotKoppenStations(Bitmap : tMyBitmap);
var
   xp,yp : integer;
   Color : tPlatformColor;
begin
   EmpSource.Enabled := false;
   MyData.First;
   while not MyData.EOF do begin
     TheMapOwner.MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat(LatFieldName),MyData.GetFieldByNameAsFloat(LongFieldName),xp,yp);
     if TheMapOwner.MapDraw.OnScreen(xp,yp) and GetKoppenColor(MyData.GetFieldByNameAsString('CLASS'),Color) then
        ScreenSymbol(Bitmap.Canvas,xp,yp,dbOpts.Symbol.DrawingSymbol,dbOpts.Symbol.Size,Color);
     MyData.Next;
   end;
   EmpSource.Enabled := true;
end;
{$EndIf}



{$IfDef ExRedistrict}
{$Else}
procedure tGISdataBaseModule.SetRedistrictPattern(var Bitmap2 : tMyBitmap);
begin
   if (RedistrictForm <> Nil) and (RedistrictForm.RadioGroup1.ItemIndex <> 0) then begin
      Bitmap2.Canvas.Pen.Color := clBlack;
      Bitmap2.Canvas.Pen.Width := 1;
      if (RedistrictForm.RadioGroup1.ItemIndex = 1) and (MyData.GetFieldByNameAsFloat('BLACK_PC') * 100 > RedistrictForm.ColorThreshhold) or
         (RedistrictForm.RadioGroup1.ItemIndex = 2) and (MyData.GetFieldByNameAsFloat('HISPAN_PC') * 100 > RedistrictForm.ColorThreshhold) or
         (RedistrictForm.RadioGroup1.ItemIndex = 3) and (MyData.GetFieldByNameAsFloat('POP') > RedistrictForm.PopThreshhold) or
         (RedistrictForm.RadioGroup1.ItemIndex = 4) and (MyData.GetFieldByNameAsFloat('POP') > RedistrictForm.PopThreshhold) and  (MyData.GetFieldByNameAsFloat('BLACK_PC') * 100 > RedistrictForm.ColorThreshhold) or
         (RedistrictForm.RadioGroup1.ItemIndex = 5) and (MyData.GetFieldByNameAsFloat('POP') > RedistrictForm.PopThreshhold) and  (MyData.GetFieldByNameAsFloat('HISPAN_PC') * 100 > RedistrictForm.ColorThreshhold) then begin
            Bitmap2.Canvas.Brush.Style := bsSolid;
      end
      else begin
         Bitmap2.Canvas.Brush.Style := bsDiagCross;
      end;
   end;
end;
{$EndIf}

