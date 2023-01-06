{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}
{ include file for demdatabase       }
{____________________________________}



{$IfDef ExSidescan}
{$Else}

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

