{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 7/30/2014       }
{                                 }
{ include file for demmapf        }
{_________________________________}


       procedure GetEulerPole(var EulerLat,EulerLong,EulerOmega : float64);
       begin
          EulerLat := GISdb[PickRotationForm.MotionTable].MyData.GetFieldByNameAsFloat('LAT');
          EulerLong := GISdb[PickRotationForm.MotionTable].MyData.GetFieldByNameAsFloat('LONG');
          EulerOmega := GISdb[PickRotationForm.MotionTable].MyData.GetFieldByNameAsFloat('OMEGA');
       end;




procedure tMapForm.BroadCastDrawPlane;
var
   k : integer;
begin
   if (WmDEM.MDIChildCount > 0) then begin
     for k := WmDEM.MDIChildCount-1 downto 0 do  begin
         if WmDEM.MDIChildren[k].Handle <> Self.Handle then begin
            PostMessage(WmDEM.MDIChildren[k].Handle,WM_BroadcastDrawPlane,0,0);
         end;
     end;
   end;
end;

procedure TMapForm.WMBroadcastDrawPlane(var Msg: TMessage);
var
   xDEMg1,yDEMg1 : float64;
begin
   if (MapDraw.ValidDEMonMap) then begin
      DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(BroadCastPlaneData.Lat,BroadCastPlaneData.Long,xDEMg1,yDEMg1);
      ThreadCheckPlane(false,xDEMg1,yDEMg1,BroadCastPlaneData.Dip,BroadCastPlaneData.aStrike,BroadCastPlaneData.DipDir);
   end;
end;

procedure TMapForm.FigureThreePointProblem(DB : integer);
var
   Lat,Long : float64;
begin
   DEMGlb[MapDraw.DEMonMap].DEMGridToUTM(xDEMg1,yDEMg1,BroadCastPlaneData.xutm1,BroadCastPlaneData.yutm1);
   DEMGlb[MapDraw.DEMonMap].DEMGridToUTM(xDEMg2,yDEMg2,BroadCastPlaneData.xutm2,BroadCastPlaneData.yutm2);
   DEMGlb[MapDraw.DEMonMap].DEMGridToUTM(xDEMg3,yDEMg3,BroadCastPlaneData.xutm3,BroadCastPlaneData.yutm3);

   DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg1,yDEMg1,BroadCastPlaneData.z1);
   DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg2,yDEMg2,BroadCastPlaneData.z2);
   DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg3,yDEMg3,BroadCastPlaneData.z3);

   DipAndStrikeFromThreePoints(BroadCastPlaneData.xutm1,BroadCastPlaneData.yutm1,BroadCastPlaneData.z1,
       BroadCastPlaneData.xutm2,BroadCastPlaneData.yutm2,BroadCastPlaneData.z2,
       BroadCastPlaneData.xutm3,BroadCastPlaneData.yutm3,BroadCastPlaneData.z3,
       BroadCastPlaneData.DipAndStrike,BroadCastPlaneData.SlopeStr,BroadCastPlaneData.Dip,BroadCastPlaneData.aStrike,BroadCastPlaneData.DipDir);

   BroadCastPlaneData.xglo := round(MinFloat(xDEMg1,xDEMg2,xDEMg3) - MDDef.ThreePointExtrap / DEMGlb[MapDraw.DEMonMap].AverageXSpace);
   BroadCastPlaneData.xghi := round(MaxFloat(xDEMg1,xDEMg2,xDEMg3) + MDDef.ThreePointExtrap / DEMGlb[MapDraw.DEMonMap].AverageXSpace);
   BroadCastPlaneData.yglo := round(MinFloat(yDEMg1,yDEMg2,yDEMg3) - MDDef.ThreePointExtrap / DEMGlb[MapDraw.DEMonMap].AverageYSpace);
   BroadCastPlaneData.yghi := round(MaxFloat(yDEMg1,yDEMg2,yDEMg3) + MDDef.ThreePointExtrap / DEMGlb[MapDraw.DEMonMap].AverageYSpace);

   if (ThreePointer <> Nil) then begin
      ThreePointer.xDEMg1 := xdemg1;
      ThreePointer.yDEMg1 := ydemg1;
      ThreePointer.Edit1.Text := RealToString(BroadCastPlaneData.aStrike,-8,-2);
      ThreePointer.Edit2.Text := RealToString(BroadCastPlaneData.Dip,-8,-2);
      ThreePointer.ComboBox1.Text := AzimuthToDirection(round(BroadCastPlaneData.DipDir));
      ThreePointer.SetDipAndStrike;

      UpdateThreePointer;
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
      ThreePointer.DrawPlaneAtPoint(Lat,Long,ThreePointer.CurrentDip,ThreePointer.CurrentStrike,ThreePointer.CurrentDipDir);
   end;

   if ValidDB(DB) then begin
      GISdb[DB].MyData.Prior;
      GISdb[DB].MyData.Edit;
      GISdb[DB].MyData.SetFieldByNameAsString('DIPSTRIKE',BroadCastPlaneData.DipAndStrike);
      GISdb[DB].MyData.SetFieldByNameAsFloat('STRIKE',BroadCastPlaneData.aStrike);
      GISdb[DB].MyData.SetFieldByNameAsFloat('DIP',BroadCastPlaneData.Dip);
      GISdb[DB].MyData.Next;
      GISdb[DB].MyData.Next;
   end;
end;



procedure TMapForm.ThreePointProblems(DB : integer; DoThreePoints : boolean =  true);
var
   TrendSurf : tTrendSurf;

      procedure DoPoint(var xg,yg : float64; Advance : boolean);
      var
         Lat,Long : float64;
      begin
         Lat := GISdb[DB].MyData.GetFieldByNameAsFloat(GISdb[DB].LatFieldName);
         Long := GISdb[DB].MyData.GetFieldByNameAsFloat(GISdb[DB].LongFieldName);
         DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
         if Advance then GISdb[DB].MyData.Next;
      end;

begin
   {$IfDef RecordGeology} WriteLineToDebugFile('TMapForm.ThreePointProblems in'); {$EndIf}
   if ValidDB(DB) then begin
      if (GISdb[DB].MyData.RecordCount >= 3) then begin
         if DoThreePoints then begin
            {$IfDef RecordGeology} WriteLineToDebugFile('Doing 3 points'); {$EndIf}
            GISdb[DB].MyData.First;
            DoPoint(xDEMg1,yDEMg1,true);
            DoPoint(xDEMg2,yDEMg2,true);
            DoPoint(xDEMg3,yDEMg3,false);
            FigureThreePointProblem(DB);

            GISdb[DB].EmpSource.Enabled := false;
            while not GISdb[DB].MyData.eof do begin
               xDEMg1 := xDEMg2;
               yDEMg1 := yDEMg2;
               xDEMg2 := xDEMg3;
               yDEMg2 := yDEMg3;
               DoPoint(xDEMg3,yDEMg3,false);
               FigureThreePointProblem(DB);
            end;
         end;
         GISdb[DB].EmpSource.Enabled := true;

         {$IfDef RecordGeology} WriteLineToDebugFile('Start trend surface'); {$EndIf}
         SaveBackupDefaults;
         MDDef.TrendMapDev := true;
         MDDef.TrendDoGraph := false;
         MDDef.TrendHistDev := false;
         MDDef.TrendText := false;
         MDDef.TrendSurfMap := false;
         TrendSurf := tTrendSurf.Create;
         TrendSurf.CurDB := DB;
         TrendSurf.SetDEM(MapDraw.DEMonMap,MapDraw.MapAreaDEMGridLimits);
         if (ThreePointer <> Nil) and (ThreePointer.PlanarDevDEM <> 0) then CloseSingleDEM(ThreePointer.PlanarDevDEM);
         TrendSurf.CurrentOrderTrendSurface := 1;
         TrendSurf.ComputeTrendSurface;
         if (ThreePointer = Nil) then begin
            MessageToContinue('Trend surface plane orientation: '+ TrendSurf.DipAndStrike + '   ' + TrendSurf.SlopeStr);
         end
         else begin
            {$IfDef RecordGeology} WriteLineToDebugFile('Report back to ThreePointer Form'); {$EndIf}
            ThreePointer.PlanarDevDEM := TrendSurf.DevDEM;
            {$IfDef RecordGeology} WriteLineToDebugFile('ThreePointer.PlanarSurfDEM=' + IntToStr(ThreePointer.PlanarDevDEM)); {$EndIf}
            ThreePointer.Memo1.Lines.Add('');
            ThreePointer.Memo1.Lines.Add('Trend surface plane orientation');
            ThreePointer.Memo1.Lines.Add(TrendSurf.DipAndStrike + '   ' + TrendSurf.SlopeStr);
            ThreePointer.Memo1.Lines.Add('');
            ThreePointer.Edit1.Text := RealToString(TrendSurf.Strike,-8,-2);
            ThreePointer.Edit2.Text := RealToString(TrendSurf.Dip,-8,-2);
            ThreePointer.ComboBox1.Text := AzimuthToDirection(round(TrendSurf.DipDir));
            ThreePointer.EnableOptions;
         end;
         TrendSurf.Destroy;
         RestoreBackupDefaults;
      end
      else begin
         MessageToContinue('Requires at least 3 points');
      end;
   end;
   {$IfDef RecordGeology} WriteLineToDebugFile('TMapForm.ThreePointProblems out'); {$EndIf}
end;


procedure GetSpreadingRateAndDirection(Lat,Long : float64; var v,Azimuth : float64);
{equations from http://people.whitman.edu/~jordanbt/platemo.html,  verified with http://people.whitman.edu/~jordanbt/platemo.xls}
//http://sps.unavco.org/crustal_motion/dxdt/model/
var
   EulerLat,EulerLong,EulerOmega,Top,Bottom,Distance,Bearing : float64;
begin
   GetEulerPole(EulerLat,EulerLong,EulerOmega);
   VincentyCalculateDistanceBearing(Lat,Long,EulerLat,EulerLong,Distance,Bearing);
   {$IfDef RecordFullDriftRate} WriteLineToDebugFile('Pole at Location: ' + LatLongDegreeToString(Lat,Long) + '  Euler: ' + LatLongDegreeToString(EulerLat,EulerLong) +  '   Omega: ' + RealToString(EulerOmega,-12,4)); {$EndIf}
   //{$IfDef RecordFullDriftRate} WriteLineToDebugFile('Distance to pole: ' + RealToString(Distance,-12,0) + '  Bearing to pole:  ' + RealToString(Bearing,5,1)); {$EndIf}
(*
Plate motions are described by geocentric rotation vectors either given by the geographic coordinates PHI,LAM [°]
of the rotation pole and the angular velocity OMEGA[ °/Ma], or by angular velocities around the cartesian axes,
omegax, omegay, omegaz [rad/109 a]. The velocities of a point with geographic coordinates phi,lam [°] or
cartesian coordinates x,y,z [in units of 1000 km] are then computed  by
dphi/dt = OMEGA·cos(PHI)·sin(lam-LAM)
dlam/dt = OMEGA·[sin(PHI) - cos(lam-LAM)·tan(phi)·cos(PHI)]

dx/dt = omegay·z - omegaz·y
dy/dt = omegaz·x - omegax·z
dz/dt = omegax·y - omegay·x

where dphi/dt and dlam/dt are given in [°/Ma] and dx/dt, dy/dt and dz/dt are given in [mm/a] .
*)
   EulerLat := EulerLat * Petmar_types.DegToRad;
   EulerLong := EulerLong * Petmar_types.DegToRad;
   EulerOmega := EulerOmega * Petmar_types.DegToRad;
   Lat := Lat * Petmar_types.DegToRad;
   Long := Long * Petmar_types.DegToRad;
   v := (EulerOmega)* SphericalEarthAkm * SIN((ArcCOS(SIN(LAT)*SIN(EULERLAT)+COS(LAT)*COS(EULERLAT)*COS(EulerLong-Long))));
   Top := COS(EULERLAT)*SIN(EULERLONG-Long);
   Bottom := SIN(ArcCOS(SIN(LAT)*SIN(EULERLAT)+COS(LAT)*COS(EULERLAT)*COS(EULERLONG-Long)));

   if (Bearing > 90) and (Bearing < 270) then Azimuth := 270 - ArcSin(Top/Bottom) / Petmar_types.DegToRad
   else Azimuth := 90 + ArcSin(Top/Bottom) / Petmar_types.DegToRad;

   v := abs(v);

   //{$IfDef RecordFullDriftRate} WriteLineToDebugFile('v: ' + RealToString(v,-12,4)+ ' top: ' + RealToString(top,-12,4) + ' bottom: ' + RealToString(bottom,-12,4) + ' Azimuth: ' + RealToString(Azimuth,-12,4)); {$EndIf}
end;


procedure TMapForm.UpdateThreePointer;
begin
   if ThreePointer.PointUpdate then begin
      ThreePointer.Memo1.Lines.Add(DEMGlb[MapDraw.DEMonMap].DEMLocationString(XDEMg1,YDEMg1));
      ThreePointer.Memo1.Lines.Add(BroadCastPlaneData.DipAndStrike + '   ' + BroadCastPlaneData.SlopeStr);
      ThreePointer.Memo1.Lines.Add('');
   end;
end;



function TMapForm.CheckGeologyOptions(Lat,Long : float64; LastX,LastY : integer; NotSamePoint : boolean) : boolean;
var
   v,azimuth,xutm,yutm,xSatg3,ysatg3  : float64;
   z : float32;
   x2,y2,RecsFound : integer;
   P1Name,P2Name,P3Name,
   TStr : ShortString;
   fName : PathStr;
   Pts : tStringList;


         procedure CheckEarthquakeOptions;
         var
            i : integer;
         begin
            if (DEMNowDoing in [EarthquakeFocalMech,ProjectFocalMechToSurface,GetStratcolColumn]) then begin
               ShowHourglassCursor;
               for i := 1 to MaxDataBase do begin
                 if (GISdb[i] <> nil) and (GISdb[i].theMapOwner = Self) then begin
                     if (GISdb[i].StratColPresent) then begin
                        GISdb[i].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,False,false,TStr);
                        sc_ColMain.ColMainF.NewFileToDiagram(GISdb[i].MyData.GetFieldByNameAsString('COL_NAME'));
                        MapScreenX1 := LastX;
                        MapScreenY1 := LastY;
                     end;
                     if (GISdb[i].FocalMechsPresent) then begin
                        GISdb[i].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,False,false,TStr);
                        if (DEMNowDoing in [EarthquakeFocalMech])then begin
                           FocalRad := GISdb[i].QuakeRadius;
                           GISdb[i].QuakeFocalMechs(LastX,LastY,Lat,Long,RecsFound,FocalMechBMP,FocalRad);
                           Image1.Canvas.Brush.Style := bsClear;
                           Image1.Canvas.Draw(LastX-FocalMechBMP.Width div 2, LastY-FocalMechBMP.Height div 2, FocalMechBMP);
                        end;
                     end;
                 end;
              end;
              ShowDefaultCursor;
            end;
         end;


       function VelocityVectorLength(v : float64) : float64;
       begin
          Result := v * MDDef.RotateVectMult / 5;
       end;

       procedure GetComponents(var xv1,yv1 : float32; Plabel : shortstring = '');
       begin
           xv1 := v * SinDeg(Azimuth);
           yv1 := v * CosDeg(Azimuth);
           if PickRotationForm.CheckBox1.Checked then begin
              Pts.Add(RealToString(xv1,-12,-5) + ',' + RealToString(yv1,-12,-5) + ',' + PLabel + ',' + IntToStr(ord(FilledBox)) + ',4,' + IntToStr(clRed));
           end;
       end;

          procedure ExpandVertAxis;
          begin
             PickRotationForm.ThisGraph.GraphDraw.MinVertAxis := PickRotationForm.ThisGraph.GraphDraw.MinVertAxis - 25;
             PickRotationForm.ThisGraph.GraphDraw.MaxVertAxis := PickRotationForm.ThisGraph.GraphDraw.MaxVertAxis + 25;
          end;


          procedure ExpandHorizAxis;
          begin
             PickRotationForm.ThisGraph.GraphDraw.MinHorizAxis := PickRotationForm.ThisGraph.GraphDraw.MinHorizAxis - 25;
             PickRotationForm.ThisGraph.GraphDraw.MaxHorizAxis := PickRotationForm.ThisGraph.GraphDraw.MaxHorizAxis + 25;
          end;


         procedure VelocityScale;
         var
            TStr,TStr2 : ShortString;

                  procedure Tick(v : integer);
                  begin
                      x2 := 10 + round(2* VelocityVectorLength(v));
                      Image1.Canvas.MoveTo(x2,35);
                      Image1.Canvas.LineTo(x2,25);
                      if v = 100 then TStr := TStr2
                      else TStr := IntToStr(v);
                      x2 := x2 - 10;
                      if x2 < 4 then x2 := 4;
                      Image1.Canvas.TextOut(x2,5,TStr);
                  end;

         begin
             if MDDef.PlateTectonicVelocity then begin
                Image1.Canvas.Brush.Color := clWhite;
                Image1.Canvas.Brush.Style := bsSolid;
                Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(PickRotationForm.RotLineColor);
                Image1.Canvas.Pen.Width := 1;
                x2 := 10 + round(2*VelocityVectorLength(100));
                TStr2 := '100 mm/yr';
                Image1.Canvas.Rectangle(2,2,x2 - 5 + Image1.Canvas.TextWidth(TStr2),45);
                Image1.Canvas.MoveTo(10,35);
                Image1.Canvas.LineTo(x2,35);
                Tick(100);
                Tick(50);
                Tick(25);
                Tick(0);
             end;
         end;


    procedure PlotVector(Lat,Long,v,Azimuth : float64; StartArrowFromPoint : boolean = false; Plabel : shortstring = '');
    var
       xp,yp,f : integer;
       Bitmap : tMyBitmap;
       TStr : shortstring;
    begin
       {$IfDef RecordFullDrift} WriteLineToDebugFile('Plot vector: ' + RealToString(v,-8,-1) + '@' + RealToString(Azimuth,-8,-1)); {$EndIf}
       MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
       {$IfDef RecordFullDrift} WriteLineToDebugFile('At ' + LatLongDegreeToString(Lat,Long) + '  screen x=' + IntToStr(xp) + '  y=' + IntToStr(yp)); {$EndIf}
       if StartArrowFromPoint then f := 2 else f := 1;

       CopyImageToBitmap(Image1,Bitmap);
       PlotOrientedLine(Bitmap,xp,yp,round(VelocityVectorLength(v)),round(Azimuth),PickRotationForm.RotLineColor,PickRotationForm.RotLineSize,true,false,StartArrowFromPoint);
       Image1.Picture.Graphic := bitmap;
       Bitmap.Free;
       LoadMyFontIntoWindowsFont(MDDef.LegendFont,Self.Image1.Canvas.Font);

       x2 := round(Xp + f * VelocityVectorLength(v) * SinDeg(Azimuth));
       y2 := round(Yp - f * VelocityVectorLength(v) * CosDeg(Azimuth));
       {$IfDef RecordFullDrift} WriteLineToDebugFile('Vector Label End, x=' + IntToStr(x2) + ' y=' + IntToStr(y2)); {$EndIf}
       if MDDef.PlateLabelVectors then begin
          f := round(azimuth);
          if (PLabel <> '') then Plabel := Plabel + '=';
          TStr := PLabel + RealToString(v,-12,1);
          if (f in [0..45])  then begin
             x2 := x2 + 4;
             y2 := y2 - 2 - Image1.Canvas.TextHeight(TStr);
          end
          else if (f in [45..135])  then begin
             x2 := x2 + 4;
             y2 := y2 - Image1.Canvas.TextHeight(TStr) div 2;
          end
          else if (f in [135..180])  then begin
             x2 := x2 + 4;
             y2 := y2 + Image1.Canvas.TextHeight(TStr);
          end
          else if (f in [180..225])  then begin
             x2 := x2 - 4 - Image1.Canvas.TextWidth(TStr);
             y2 := y2 + Image1.Canvas.TextHeight(TStr);
          end
          else if (f >= 225) and (f <=315) then begin
             x2 := x2 - 4 - Image1.Canvas.TextWidth(TStr);
             y2 := y2 - Image1.Canvas.TextHeight(TStr) div 2;
          end
          else begin
             x2 := x2 - 4 - Image1.Canvas.TextWidth(TStr);
             y2 := y2 - 2 - Image1.Canvas.TextHeight(TStr);
          end;
          Image1.Canvas.TextOut(x2+2,y2+2,TStr);
       end;
    end;

begin
  {$IfDef RecordDrift} WriteLineToDebugFile('');  WriteLineToDebugFile('Check geology at ' + LatLongDegreeToString(Lat,Long) + 'map coords, x=' + IntToStr(LastX) + ' y=' + IntToStr(LastY)); {$EndIf}
   Result := (DEMNowDoing in [EarthquakeFocalMech,ProjectFocalMechToSurface,GetStratcolColumn,PickEulerPole,GetDriftVectors,GetSpreadingRate,
        SeekingPlaneContact,TraceContact,SeekingThickness,SeekingThirdThreePoint,SeekingSecondThreePoint,SeekingFirstThreePoint]);
   if not Result then exit;

   CheckEarthquakeOptions;
   if (DEMNowDoing = PickEulerPole) then begin
      PickRotationForm.Edit1.Text := RealToString(Lat,-8,4);
      PickRotationForm.Edit2.Text := RealToString(Long,-9,4);
      PickRotationForm.BitBtn1Click(Nil);
      PickRotationForm.BitBtn8Click(Nil);
   end;
   if (DEMNowDoing = GetDriftVectors) then begin
       {$IfDef RecordDrift} WriteLineToDebugFile('DEMNowDoing = GetDriftVectors'); {$EndIf}
       GetSpreadingRateAndDirection(Lat,Long,v,Azimuth);
       {$IfDef RecordDrift} WriteLineToDebugFile('  call PlotVector at ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
       PlotVector(Lat,Long,v,Azimuth);
       VelocityScale;
       PickRotationForm.Memo1.Lines.Insert(0,'');
       PickRotationForm.Memo1.Lines.Insert(0,PickRotationForm.Plate1ComboBox.Text + '   ' + RealToString(v,-12,2) + ' mm/yr   ' + RealToString(Azimuth,-5,0) + '°');
       PickRotationForm.Memo1.Lines.Insert(0,PickRotationForm.ModelComboBox.Text + '   ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
   end;

   if (DEMNowDoing = GetSpreadingRate) then begin
       PickRotationForm.Memo1.Lines.Insert(0,PickRotationForm.ModelComboBox.Text + '  ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
       if PickRotationForm.CheckBox1.Checked then begin
          PickRotationForm.ThisGraph := tThisBaseGraph.Create(Application);
          PickRotationForm.ThisGraph.ClientWidth := 1000;
          PickRotationForm.ThisGraph.ClientHeight := 800;

          PickRotationForm.ThisGraph.Caption := PickRotationForm.ModelComboBox.Text + ' Plate motions ' + LatLongDegreeToString(Lat,Long,VeryShortDegrees);
          PickRotationForm.ThisGraph.GraphDraw.LLCornerText := PickRotationForm.ThisGraph.Caption;
          PickRotationForm.ThisGraph.GraphDraw.CorrectScaling := true;
          PickRotationForm.ThisGraph.GraphDraw.HorizLabel := 'Ve (mm/yr)';
          PickRotationForm.ThisGraph.GraphDraw.VertLabel := 'Vn (mm/yr)';
          PickRotationForm.ThisGraph.GraphDraw.ShowHorizAxis0 := true;
          PickRotationForm.ThisGraph.GraphDraw.ShowVertAxis0 := true;

          Pts := tStringList.Create;
          Pts.Add('X_COORD,Y_COORD,NAME,SYM_TYPE,SYM_SIZE,SYM_COLOR');
       end;
       P1Name := PickRotationForm.Plate1ComboBox.Text;
       P2Name := PickRotationForm.Plate2ComboBox.Text;

       if (PickRotationForm.RadioGroup1.ItemIndex = 3) then begin
          if (P1Name = P2Name) then begin
             MessageToContinue('Requires different plates');
             exit;
          end;
       end
       else begin
          P3Name := PickRotationForm.Plate3ComboBox.Text;
          if (P1Name = P2Name) or (P1Name = P3Name) or (P2Name = P3Name) then begin
             MessageToContinue('Requires different plates');
             exit;
          end;
       end;

       GISdb[PickRotationForm.MotionTable].MyData.ApplyFilter('MODEL = ' + QuotedStr(PickRotationForm.ModelComboBox.Text) + ' AND PLATE_NAME= ' + QuotedStr(P1Name));
       GetSpreadingRateAndDirection(Lat,Long,v,Azimuth);

       TStr := PickRotationForm.Plate1ComboBox.Text + ' (Plate 1)';
       if MDDef.PlateVectors then begin
          if MDDef.PlateNumbers then TStr := 'P1'  else TStr := P1Name;
          PlotVector(Lat,Long,v,Azimuth,true,TStr);
       end;
       GetComponents(PickRotationForm.xv1,PickRotationForm.yv1,TStr);

       PickRotationForm.Memo1.Lines.Insert(1,TStr + '  ' + RealToString(v,12,2) + ' mm/yr   ' + RealToString(Azimuth,-5,0) + '°');
       {$IfDef RecordDrift} WriteLineToDebugFile(PickRotationForm.Plate1ComboBox.Text + ' (Plate 1)' + RealToString(v,12,2) + ' mm/yr   ' + RealToString(Azimuth,-5,0) + '°'); {$EndIf}

       GISdb[PickRotationForm.MotionTable].MyData.ApplyFilter( 'MODEL=' + QuotedStr(PickRotationForm.ModelComboBox.Text) + ' AND PLATE_NAME= ' + QuotedStr(P2Name));
       GetSpreadingRateAndDirection(Lat,Long,v,Azimuth);

       TStr := PickRotationForm.Plate2ComboBox.Text + ' (Plate 2)';
       if MDDef.PlateVectors then begin
          if MDDef.PlateNumbers then TStr := 'P2' else TStr := P2Name;
          PlotVector(Lat,Long,v,Azimuth,true,TStr);
       end;

       GetComponents(PickRotationForm.xv2,PickRotationForm.yv2,TStr);

       PickRotationForm.Memo1.Lines.Insert(2,TStr + ' ' + RealToString(v,12,2) + ' mm/yr   ' + RealToString(Azimuth,-5,1) + '°');
       {$IfDef RecordDrift} WriteLineToDebugFile(PickRotationForm.Plate2ComboBox.Text + ' (Plate 2) '+ RealToString(v,12,2) + ' mm/yr   ' + RealToString(Azimuth,-5,0) + '°'); {$EndIf}

       if (PickRotationForm.RadioGroup1.ItemIndex = 3) then begin
          v := sqrt(sqr(PickRotationForm.xv2-PickRotationForm.xv1) + sqr(PickRotationForm.yv2-PickRotationForm.yv1));
          Azimuth := HeadingOfLine((PickRotationForm.xv2-PickRotationForm.xv1),(PickRotationForm.yv2-PickRotationForm.yv1));
          PickRotationForm.Memo1.Lines.Insert(3,'Plate motion: (' + PickRotationForm.Plate1ComboBox.Text + ' fixed)');
          PickRotationForm.Memo1.Lines.Insert(4,RealToString(v,12,2) + ' mm/yr  toward' + RealToString(Azimuth,6,1)+ '°' );
          PickRotationForm.Memo1.Lines.Insert(5,'');
          {$IfDef RecordDrift} WriteLineToDebugFile('Plate motion: (' + PickRotationForm.Plate1ComboBox.Text + ' fixed) ' + RealToString(v,-12,2) + ' mm/yr  toward ' + RealToString(Azimuth,-6,1)+ '°'); {$EndIf}
          if MDDef.ResultantPlateVectors then begin
             PlotVector(Lat,Long,v,Azimuth,false,'res');
          end;
       end
       else begin
          GISdb[PickRotationForm.MotionTable].MyData.ApplyFilter('MODEL=' + QuotedStr(PickRotationForm.ModelComboBox.Text) + ' AND PLATE_NAME= ' + QuotedStr(P3Name));
          GetSpreadingRateAndDirection(Lat,Long,v,Azimuth);
          TStr := PickRotationForm.Plate2ComboBox.Text + ' (Plate 2)';
          if MDDef.PlateVectors then begin
             if MDDef.PlateNumbers then TStr := 'P3' else TStr := P3Name;
             PlotVector(Lat,Long,v,Azimuth,true,TStr);
          end;
          GetComponents(PickRotationForm.xv3,PickRotationForm.yv3,TStr);
          PickRotationForm.BitBtn14.Enabled := true;
          PickRotationForm.Memo1.Lines.Insert(3,TStr + '  ' + RealToString(v,12,2) + ' mm/yr ' + RealToString(Azimuth,-5,1) + '°');
          PickRotationForm.Memo1.Lines.Insert(4,'');
          {$IfDef RecordDrift} WriteLineToDebugFile(TStr + '  ' + RealToString(v,12,2) + ' mm/yr ' + RealToString(Azimuth,-5,1) + '°'); {$EndIf}
       end;

       if PickRotationForm.CheckBox1.Checked then begin
          fName := Petmar.NextFileNumber(MDTempDir,'points_',DefaultDBExt);
          StringList2CSVtoDB(Pts,fName,true);
          PickRotationForm.ThisGraph.GraphDraw.DBFPointFilesPlotted.Add(fName);
          PickRotationForm.ThisGraph.AutoScaleAndRedrawDiagram;
          ExpandVertAxis;
          ExpandHorizAxis;
          if (PickRotationForm.ThisGraph.GraphDraw.MaxVertAxis - PickRotationForm.ThisGraph.GraphDraw.MinVertAxis) < 100 then ExpandVertAxis;
          if (PickRotationForm.ThisGraph.GraphDraw.MaxHorizAxis - PickRotationForm.ThisGraph.GraphDraw.MinHorizAxis) < 100 then ExpandHorizAxis;

          PickRotationForm.ThisGraph.FormResize(Nil);
       end;
       VelocityScale;
   end;

   if (DEMNowDoing = SeekingPlaneContact) then begin
      {$IfDef RecordGeology}
         WriteLineToDebugFile('(SeekingPlaneContact), LastX=' + IntToStr(LastX) + '  lasty=' + IntToStr(LastY) + '  xdemg1=' + RealToString(xdemg1,-12,-2) + '  ydemg1=' + RealToString(ydemg1,-12,-2));
         WriteLineToDebugFile('     ' + LatLongDegreeToString(Lat,Long));
      {$EndIf}
      MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
      UpdateThreePointer;
      ThreePointer.DrawPlaneAtPoint(Lat,Long,ThreePointer.CurrentDip,ThreePointer.CurrentStrike,ThreePointer.CurrentDipDir);
      BackToWandering;
   end;

   if (DEMNowDoing = TraceContact) then begin
      CheckThisPoint(LastX,LastY,xDEMg3,yDEMg3,xsatg3,ysatg3,CheckReasonable);
      MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
      MapDraw.ScreenToUTM(LastX,LastY,xutm,yutm);
      DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
      GISdb[LineTable].MyData.Insert;
      GISdb[LineTable].MyData.SetFieldByNameAsFloat('LAT',Lat);
      GISdb[LineTable].MyData.SetFieldByNameAsFloat('LONG',Long);
      GISdb[LineTable].MyData.SetFieldByNameAsFloat('Z',z);
      GISdb[LineTable].MyData.SetFieldByNameAsFloat('X_UTM',xutm);
      GISdb[LineTable].MyData.SetFieldByNameAsFloat('Y_UTM',yutm);
      GISdb[LineTable].MyData.Post;
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
   end;

   if (DEMNowDoing = SeekingThickness) then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,CheckNothing);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,BroadCastPlaneData.Lat,BroadCastPlaneData.Long);
      ThreePointer.ThicknessAtLatLong(Lat,Long);
   end;

   if (DEMNowDoing = SeekingThirdThreePoint) and NotSamePoint then begin
      CheckThisPoint(LastX,LastY,xDEMg3,yDEMg3,xsatg3,ysatg3,CheckReasonable);
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
      FigureThreePointProblem(LineTable);
      BackToWandering;
   end;

   if (DEMNowDoing = SeekingSecondThreePoint) and NotSamePoint then begin
      CheckThisPoint(LastX,LastY,xDEMg2,yDEMg2,xsatg2,ysatg2,CheckReasonable);
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
      ChangeDEMNowDoing(SeekingThirdThreePoint);
      MapScreenX1 := Lastx;
      MapScreenY1 := LastY;
   end;

   if (DEMNowDoing = SeekingFirstThreePoint) then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,CheckReasonable);
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
      ChangeDEMNowDoing(SeekingSecondThreePoint);
      MapScreenX1 := Lastx;
      MapScreenY1 := LastY;
   end;
end;


procedure TMapForm.ThreadCheckPlane(GetOrientation: boolean ; xg1,yg1 : float64; Dip,Strike,DipDir  : float32);
var
   Bitmap : tMyBitmap;
begin
   {$IfDef RecordThreadCheckPlane} WriteLineToDebugFile('TMapForm.ThreadCheckPlane in  Map=' + Caption); {$EndIf}
   if ClosingMapNow or (MapDraw.DEMonMap = 0) or (Not DEMGlb[MapDraw.DEMonMap].GridInDataSet(xg1,yg1)) then exit;
   repeat
       if GetOrientation then begin
          GetStrikeAndDip('plane',BroadCastPlaneData.DipAndStrike,Dip,Strike,DipDir);
          {$IfDef RecordThreadCheckPlane} WriteLineToDebugFile('Plane thread: ' + BroadCastPlaneData.DipAndStrike + '   ' + RealToString(BroadCastPlaneData.Dip,8,2) ); {$EndIf}
       end;
       CopyImageToBitmap(Image1,Bitmap);
       MapDraw.DrawThreadCheckPlane(Bitmap, BroadCastPlaneData.xglo,BroadCastPlaneData.yglo,BroadCastPlaneData.xghi,BroadCastPlaneData.yghi,xg1,yg1,Dip,Strike,DipDir);
       Image1.Picture.Graphic := Bitmap;
       Bitmap.Free;
    until (Not GetOrientation) or (not AnswerIsYes('Another plane through same point'));
end;


procedure tMapForm.PlateRotationSetup;
begin
    Self.Left := 0;
    Self.Top := 0;
    PickRotationForm := TPickRotationForm.Create(Application);
    Image1.Canvas.Font.Size := 14;
    Image1.Canvas.Font.Style := [fsBold];

    inc(NumPlateMotionMaps);
    PickRotationForm.Caption := 'Plate rotation controls ' + IntToStr(NumPlateMotionMaps);
    VectorMap[LastVectorMap].MapDraw.BaseTitle := 'Plate rotations map ' + IntToStr(NumPlateMotionMaps);

    PickRotationForm.PlatesDB  := LoadDataBaseFile(ContCrustOutlineFile);
    if ValidDB(PickRotationForm.PlatesDB) then begin
       GISdb[PickRotationForm.PlatesDB].dbOpts.LineColor := claBlue;
       GISdb[PickRotationForm.PlatesDB].DBTableF.WindowState := wsMinimized;
       GISdb[PickRotationForm.PlatesDB].LayerIsOn := MDDef.PlateRotateBoundaries;
    end;

    PickRotationForm.BoundariesDB := LoadDataBaseFile(PlateBoundaryFile);
    if ValidDB(PickRotationForm.BoundariesDB) then begin
       if (GISdb[PickRotationForm.BoundariesDB].DBTableF <> Nil) then GISdb[PickRotationForm.BoundariesDB].DBTableF.WindowState := wsMinimized;
       GISdb[PickRotationForm.BoundariesDB].LayerIsOn := MDDef.PlateRotateContCrust;
    end;

    PickRotationForm.MapForm := Self;
    Self.Closable := false;
    PickRotationForm.Show;
    PickRotationForm.CheckBox6.Checked := true;
    Self.SetFocus;
end;

