{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{                                        }
{   include file for demdatabase         }
{________________________________________}



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

