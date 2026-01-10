{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{   include file for demdatabase         }
{________________________________________}



      procedure tGISdataBaseModule.FocalPlaneStats(What : tFocalPlaneWhat);
      begin
         if (What = fpRoseStrike) then begin
            SingleRose('','FP1_STRIKE','FP2_STRIKE');
         end;
         if (What = fpRoseDipDir) then begin
            SingleRose('','dd_from_strike','');
         end;
         dbtablef.ShowStatus;
      end;


      function tGISdataBaseModule.QuakeRadius : integer;
      var
         Mag : float64;
      begin
         if (MDDef.NetDef.BeachBallSize = bbsAll) then Result := MDDef.NetDef.AllBeachBallSize
         else begin
             if (MDDef.NetDef.BeachBallSize = bbsMs) then Mag := MyData.GetFieldByNameAsFloat('MS')
             else Mag := MyData.GetFieldByNameAsFloat('MB');
             if (Mag < MDDef.NetDef.MinScaleMagnitude) then Mag := MDDef.NetDef.MinScaleMagnitude;
             if (Mag > MDDef.NetDef.MaxScaleMagnitude) then Mag := MDDef.NetDef.MaxScaleMagnitude;
             Result := MDDef.NetDef.M3BeachBallSize + Round( (Mag - MDDef.NetDef.MinScaleMagnitude) / (MDDef.NetDef.MaxScaleMagnitude - MDDef.NetDef.MinScaleMagnitude) * (MDDef.NetDef.M9BeachBallSize - MDDef.NetDef.M3BeachBallSize));
         end;
      end;

      function tGISdataBaseModule.QuakeColor : tPlatformColor;
      var
         Mag : float64;

               function FindColor(Mag,Min,Max : float64) : tPlatformColor;
               begin
                    if (Mag < Min) then Mag := Min;
                    if (Mag > Max) then Mag := Max;
                    Result := PlatformRainbowColorFunct(Mag,Min,Max);
               end;

      begin
           if (MDDef.NetDef.BeachBallColor = bbcAll) then Result := claBlack
           else if (MDDef.NetDef.BeachBallcolor in [bbcDepth]) then begin
              Mag := MyData.GetFieldByNameAsFloat('DEPTH');
              Result := FindColor(Mag,MDDef.NetDef.MinColorDepth,MDDef.NetDef.MaxColorDepth);
           end
           else begin
              if (MDDef.NetDef.BeachBallcolor in [bbcMs,bbcMw]) then Mag := MyData.GetFieldByNameAsFloat('MS')
              else Mag := MyData.GetFieldByNameAsFloat('MB');
              Result := FindColor(Mag,MDDef.NetDef.MinColorMagnitude,MDDef.NetDef.MaxColorMagnitude);
           end;
      end;


      procedure TGISdataBaseModule.PlotTernary(var Bitmap : tMyBitmap);
      var
         xp,yp : integer;
         sum,x,y,z : float64;
         Color : tPlatformColor;
      begin
         ShowHourglassCursor;
         EmpSource.Enabled := false;
         MyData.First;
         repeat
            x := MyData.GetFieldByNameAsFloat(dbOpts.XField);
            y := MyData.GetFieldByNameAsFloat(dbOpts.YField);
            z := MyData.GetFieldByNameAsFloat(dbOpts.ZField);
            sum := x + y + z;
            if (sum > 0.001) and ((not MDDef.TernaryPercentageValues) or ((sum > 98) and (sum < 102))) then begin
               Color := ConvertTColorToPlatformColor(RGB(round(255*x/sum),round(255*y/sum),round(255*z/sum)));
               if ValidScreenPositionFromTable(xp,yp) then ScreenSymbol(Bitmap.Canvas,xp,yp,FilledBox,dbOpts.Symbol.Size,Color);
            end;
            MyData.Next;
         until MyData.EOF;
         ShowDefaultCursor;
         EmpSource.Enabled := true;
      end;


      procedure tGISdataBaseModule.PlotEarthQuakesByMechanism(Bitmap : tMyBitmap);
      var
         xp,yp,i,j,Thin,rc : integer;
         Color : tPlatformColor;
         sBitmap : tMyBitmap;
      begin
         {$IfDef RecordBeachBall} WriteLineToDebugFile('tGISDataBase.PlotEarthQuakesByMechanism in'); {$EndIf}
         Thin := 1;
         rc := MyData.FiltRecsInDB;
         if (dbOpts.DBAutoShow = dbasBeachBall) then begin
            while (rc div Thin > MDDef.NetDef.MaxNumBeachBalls) do inc(Thin);
         end;
         MyData.First;
         if (dbOpts.DBAutoShow = dbasBeachBall) then begin
            Bitmap.Canvas.CopyMode := cmSrcAnd;
            dbtablef.Formworking := true;
         end;
         try
             StartProgressAbortOption('Focal mechanisms');
             i := 0;
             rc := rc div thin;
             while (not MyData.EOF) do begin
                inc(i);
                EmpSource.Enabled := false;
                if (i mod 25 = 0) then begin
                   UpdateProgressBar(i/rc);
                end;

                TheMapOwner.MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat(LatFieldName),MyData.GetFieldByNameAsFloat(LongFieldName),xp,yp);
                if TheMapOwner.MapDraw.OnScreen(xp,yp) then begin
                   if (dbOpts.DBAutoShow = dbasBeachBall) then begin
                      sBitmap := DrawFocalMechanism(QuakeRadius);
                      EmpSource.Enabled := false;
                      DrawAndDeleteOverlay(Bitmap,sBitmap,100,xp-sBitmap.Width div 2,yp-sBitmap.Height div 2);
                   end
                   else begin
                      Color := FocalMechanismColor;
                      ScreenSymbol(Bitmap.Canvas,xp,yp,FilledBox,dbOpts.Symbol.Size,Color);
                   end;
                end;
                for j := 1 to thin do begin
                   MyData.Next;
                end;
                if WantOut then break;
             end;
         finally
            dbtablef.Formworking := false;
            EndProgress;
         end;
         {$IfDef RecordBeachBall} WriteLineToDebugFile('tGISDataBase.PlotEarthQuakesByMechanism out'); {$EndIf}
      end;


      function tGISdataBaseModule.FocalMechanismColor : tPlatformColor;
      var
         TStr : shortString;
      begin
        Result := claOlive;
        if MyData.FieldExists('MECH') then begin
          TStr := MyData.GetFieldByNameAsString('MECH');
          if (TStr <> '') then case TStr[1] of
             'N' : Result := claLime;
             'T' : Result := claRed;
             'S' : Result := claBlue;
             else Result := claOlive;
          end;
        end;
      end;


      function tGISdataBaseModule.GetFocalPlanes : tStringList;

             procedure Output(Plane : integer);
             var
                Dip,Strike : integer;
             begin
                Dip := MyData.GetFieldByNameAsInteger('FP' + IntToStr(Plane) + '_DIP');
                Strike := MyData.GetFieldByNameAsInteger('FP' + IntToStr(Plane) + '_STRIKE');
                Result.Add('Focal plane ' + IntToStr(Plane));
                Result.Add('   Strike/dip:  ' +   CreateStrikeAndDipString(Strike,Dip));
                Result.Add(IntegerToString(Strike,8) + IntegerToString(Dip,8));
                Result.Add('  Dip: ' + IntToStr(Dip)  + '  toward ' + IntToStr(Round(DipDirectionRHR(Strike))));
             end;

      begin
         Result := tStringList.Create;
         Output(1);
         Result.Add('');
         Output(2);
      end;


      function tGISdataBaseModule.DrawFocalMechanism(NetRadius : integer) : tMyBitmap;
      var
         NetDraw : TNetDraw;
         Color  : tColor;
         Mech : ANSIstring;
         xd,yd,Dip,Strike,Thick : integer;
         Sym : tFullSymbolDeclaration;

               procedure FillQuadrant(Dip,Strike : integer);
               var
                  Margin : integer;
               begin
                  {$IfDef RecordQuadFill} WriteLineToDebugFile('Dip=' + IntToStr(Dip) +  '  strike='+ IntToStr(Strike)); {$EndIf}
                  if (Length(Mech) > 0) and (Mech[1] in ['N','S']) then Margin := 10
                  else Margin := 3;

                  if (Dip > 90-Margin) then Dip := 90-Margin;
                  if (Dip < Margin) then Dip := Margin;

                  if Strike > 360 then Strike := Strike - 360;

                  NetDraw.WorkingBitmap.Canvas.Brush.Color := Color;
                  NetDraw.WorkingBitmap.Canvas.Brush.Style := bsSolid;

                  NetDraw.ReallyPlot := false;
                  NetDraw.PlotPointOnNet(LinePlot,Dip,Strike,Sym,xd,yd);
                  NetDraw.ReallyPlot := true;

                  NetDraw.WorkingBitmap.Canvas.FloodFill(xd,yd,Color,fsBorder);

                  {$IfDef RecordQuadFill}
                     WriteLineToDebugFile('Dip=' + IntToStr(Dip) +  '  strike'+ IntToStr(Strike) + '  xd=' + IntToStr(xd) + '  yd=' + IntToStr(yd));
                     Sym.DrawingSymbol := FilledBox;
                     Sym.Size := 5;
                     Sym.Color := claYellow;
                     NetDraw.WorkingBitmap.PlotPointOnNet(LinePlot,Dip,Strike,Sym,xd,yd);
                  {$EndIf}
               end;

               {$IfDef RecordQuadFill}
                  procedure DoPoles;
                  begin
                    Sym.DrawingSymbol := FilledBox;
                    Sym.Size := 2;
                    Sym.Color := claRed;
                    NetDrawPlotPointOnNet(LinePlot,MyData.GetFieldByNameAsInteger('PLUNGE_1'),MyData.GetFieldByNameAsInteger('STRIKE_1'),Sym,xd,yd);
                    Sym.Color := claLime;
                    NetDraw.PlotPointOnNet(LinePlot,MyData.GetFieldByNameAsInteger('PLUNGE_2'),MyData.GetFieldByNameAsInteger('STRIKE_2'),Sym,xd,yd);
                    Sym.Color := claBlue;
                    NetDraw.PlotPointOnNet(LinePlot,MyData.GetFieldByNameAsInteger('PLUNGE_3'),MyData.GetFieldByNameAsInteger('STRIKE_3'),Sym,xd,yd);
                  end;
               {$EndIf}

      begin
         DEMDef_routines.SaveBackupDefaults;
         MDDef.NetDef.NetUsed := Schmidt;
         MDDef.NetDef.HemiSphereUsed := Lower;
         MDDef.NetDef.DrawGridCircles := ngNone;
         MDDef.NetDef.NorthTick := false;
         MDDef.NetDef.CenterTick := false;
         MDDef.CreateNetHidden := true;
         NetDraw := TNetDraw.Create;
         NetDraw.NetOffset := 0;
         NetDraw.PlotRad := NetRadius;
         NetDraw.NewNet;
         Thick := 2;

         NetDraw.GreatCircleOnNet(MyData.GetFieldByNameAsInteger('FP1_DIP'),MyData.GetFieldByNameAsInteger('FP1_STRIKE')+90,Thick,QuakeColor);
         NetDraw.GreatCircleOnNet(MyData.GetFieldByNameAsInteger('FP2_DIP'),MyData.GetFieldByNameAsInteger('FP2_STRIKE')+90,Thick,QuakeColor);

         Color := ConvertPlatformColorToTColor(QuakeColor);

         if MyData.FieldExists('PLUNGE_2') then begin
           Mech := MyData.GetFieldByNameAsString('MECH');
           Dip := MyData.GetFieldByNameAsInteger('PLUNGE_1');
           Strike := MyData.GetFieldByNameAsInteger('STRIKE_1');
           if (Mech = 'N') and (Dip >  MyData.GetFieldByNameAsInteger('FP1_DIP')) then Dip := MyData.GetFieldByNameAsInteger('FP1_DIP') -5;
           FillQuadrant(Dip,Strike);
           if (Mech = 'S') then begin
               FillQuadrant(Dip,Strike + 180);
           end;
           if (Mech = 'N') then begin
               //if Dip >  MyData.GetFieldByNameAsInteger('FP1_DIP') then Dip := MyData.GetFieldByNameAsInteger('FP1_DIP') -5;
               FillQuadrant(Dip,Strike + 180);
           end;
           if (Mech = 'T') then begin
               //FillQuadrant(90-Dip,Strike+180);
               //FillQuadrant(Dip,Strike + 180);
           end;

            {$IfDef RecordQuadFill}
               DoPoles;
            {$EndIf}
         end;
         NetDraw.EraseOutsideNet;

         CloneBitmap(NetDraw.WorkingBitmap,Result);
         Result.Canvas.Draw(0,0,NetDraw.WorkingBitmap);
         NetDraw.Destroy;
         ApplicationProcessMessages;
         DEMDef_routines.RestoreBackupDefaults;
      end;


      procedure tGISdataBaseModule.QuakeFocalMechs(xpic,ypic : integer; Lat,Long : float64; var RecsFound : integer; var Bitmap : tMyBitmap; NetRad : integer);
      const
         ScreenTolerance = 5;
      var
         LatLow,LongLow,LatHigh,LongHigh : float64;
         i : integer;
         OldFilter : string;
         theMainFilter : string;
      begin
         EmpSource.Enabled := false;
         OldFilter := MyData.Filter;
         if (dbOpts.MainFilter = '') then theMainFilter := '' else theMainFilter := dbOpts.MainFilter + ' AND ';
         i := 0;
         repeat
             inc(i);
             TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic-i,ypic-i,LatHigh,LongLow);
             TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic+i,ypic+i,LatLow,LongHigh);
             MyData.ApplyFilter(TheMainFilter + MakePointGeoFilter(LatFieldName,LongFieldName,LatHigh,LongLow,LatLow,LongHigh));
         until (MyData.RecordCount > 0) or (i >= ScreenTolerance);
         RecsFound := MyData.RecordCount;
         if (MyData.RecordCount > 0) then begin
            Bitmap := DrawFocalMechanism(NetRad);
         end;
         MyData.ApplyFilter(OldFilter);
         EmpSource.Enabled := true;
      end;


      procedure tGISdataBaseModule.PlotDipsAndStrikes(Bitmap : tMyBitmap);
      const
         VectorSize = 15;
      var
         Dip,Strike,DipDir : float32;
         ft,x,y,xd,yd        : integer;
         InBounds     : boolean;
         GeoData      : ShortString;
         NetForm      : TNetForm;
      begin
         if DipStrikeFieldExists or DipAndStrikeFieldsExist then begin
             EmpSource.Enabled := false;
            if MDDef.StructGeologyShowNet and (NetForm = Nil) then begin
               NetForm := TNetForm.Create(Application);
               NetForm.nd.NetTitle := DBName;
               NetForm.Top := WMDEM.ClientHeight - NetForm.Height;
               NetForm.Left := WMDEM.ClientWidth - NetForm.Width;
            end;

            MyData.First;
            repeat
               EmpSource.Enabled := true;
               if DipStrikeFieldExists then GeoData := MyData.GetFieldByNameAsString('DIPSTRIKE')
               else GeoData := MyData.GetFieldByNameAsString('STRIKE')  + ' ' + MyData.GetFieldByNameAsString('DIP');
               if (GeoData <> '')  then begin
                 StripDipAndStrike(GeoData,Dip,Strike,DipDir,InBounds);
                 {$IfDef RecordDipStrike} WriteLineToDebugFile('Value is ' + GeoData  + '  dip=' + IntToStr(Round(dip)) + '  strike=' + IntToStr(Round(Strike))); {$EndIf}
                 if MyData.FieldExists('FEATURE') then ft := MyData.GetFieldByNameAsInteger('FEATURE')
                 else begin
                    if (StructureOptions = Nil) then ft := 0
                    else ft := StructureOptions.RadioGroup1.ItemIndex;
                 end;
                 if InBounds then begin
                    TheMapOwner.MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat(LatFieldName),MyData.GetFieldByNameAsFloat(LongFieldName),x,y);
                    PlotDipSymbol(Bitmap,x,y,VectorSize,round(Dip),round(Strike),round(DipDir),tStructureType(ft),MDDef.StructGeologyLabelVals,MDDef.MapGeoSymColor,2);
                    if MDDef.StructGeologyShowNet then begin
                       NetForm.nd.PlotPointOnNet(PolePlot,round(Dip),round(DipDir),NetSym,xd,yd);
                       NetForm.nd.Sum[1] := NetForm.nd.sum[1] + cosDeg(Dip) * cosDeg(DipDir);
                       NetForm.nd.Sum[2] := NetForm.nd.sum[2] + cosDeg(Dip) * sinDeg(DipDir);
                       NetForm.nd.Sum[3] := NetForm.nd.sum[3] + sinDeg(Dip);
                       inc(NetForm.nd.NumDataPoints);
                    end;
                 end;
               end;
               MyData.Next;
            until MyData.EOF;
            EmpSource.Enabled := true;
         end;
      end;

