{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{   include file for demcoord.pas    }
{____________________________________}



//{$Define CarlosErrorTrap}
//{$Define MakePartialGrids}


procedure tDEMDataSet.RichardsonExtrapolationSlopeMaps(Save : boolean = false);
//Extrapolated Evans-Young method (López-Vázquez, 2022), using Richardson extrapolation
//Carlos López-Vázquez (2022) Uncertainty interval estimates for computing slope and aspect from a gridded digital elevation model,
//   International Journal of Geographical Information Science, 36:8, 1601-1628, DOI: 10.1080/13658816.2022.2063294

//DEM is entirely in memory, and could be stored as bytes, short integers, long integers, unsigned integers, or floats
//while some of these do not make sense for a DEM, they do for other grids, where it does not make sense to store bytes as floats
//calls to get elevations sort out the storage issues and return floats for all the internal computations

//there is error trapping code, with a conditional compilation directive, to store intermediate values for debugging with the partial derivatives
//this creates a lot of grids, which are not necessarily easy to interpret or immediately useful

{$IfDef CarlosErrorTrap}
const
   CarlosXRecord : integer = 600;
   CarlosYRecord : integer = 400;
{$EndIf}


    procedure MapAndSave(DEM : integer);
    begin
        if ValidDEM(DEM) then begin
           if Save then DEMGlb[DEM].SaveAsGeotiff(ExtractFilePath(DEMFileName) + DEMGlb[DEM].AreaName + '.tif')
           else begin
              DEMGlb[DEM].SetUpMap(true,mtElevSpectrum);
           end;
        end;
    end;


var
   {$IfDef MakePartialGrids}
      dxgrid,dygrid,dx2grid,dy2grid,dx3grid,dy3grid,
   {$EndIf}

   Aspect_3x3Grid,Aspect_5x5Grid,Aspect_7x7Grid,Aspect4thGrid,Aspect6thGrid,
   Slope3x3Grid,slope5x5Grid,Slope4thgrid,slope7x7Grid,Slope6thgrid,
   SlopeErrorBoundGrid,SlopeUncertaintyGrid,TruncationErrorGrid,
   x,y : integer;

   dzdx_3x3,dzdy_3x3,dzdx_5x5,dzdy_5x5,dzdx_7x7,dzdy_7x7,dzdx_4th,dzdy_4th,dzdx_6th,dzdy_6th,
   slope_3x3,slope_5x5,slope_4th,slope_7x7,slope_6th,
   Aspect_3x3, Aspect_5x5,Aspect4th,Aspect_7x7,Aspect6th,
   SlopeErrBound,uncertainty_bndX, uncertainty_bndY,TruncationError,
   UB_fx,LB_fx,UB_fy,LB_fy,LB_fx2,UB_fx2,LB_fy2, UB_fy2,LB_m,UB_m,UB_slope,LB_slope,uncertainty_slope,
   Lat,Long : float64;
   znw,zw,zsw,zn,z,zs,zne,ze,zse,dx,dy,GridTrueAngle : float32;
begin
   {$IfDef RecordProblems} WriteLineToDebugFile('tDEMDataSet.CarlosLopezSlopeMaps in'); {$EndIf}

   Slope3x3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope3x3',euPercentSlope);
   slope5x5Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope5x5',euPercentSlope);
   slope7x7Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope7x7',euPercentSlope);
   Slope4thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_4th',euPercentSlope);
   Slope6thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_6th',euPercentSlope);
   SlopeErrorBoundGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_error',euPercentSlope);
   SlopeUncertaintyGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_uncertainty',euPercentSlope);
   TruncationErrorGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_truncation_error',euUndefined);

   Aspect_3x3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect3x3',euAspectDeg);
   Aspect_5x5Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect5x5',euAspectDeg);
   Aspect4thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect_4th',euAspectDeg);
   Aspect_7x7grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect7x7',euAspectDeg);
   Aspect6thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect_6th',euAspectDeg);

   {$IfDef MakePartialGrids}
      dxGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdx3x3',euPercentSlope);
      dyGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdy3x3',euPercentSlope);
      dx2Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdx5x5',euPercentSlope);
      dy2Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdy5x5',euPercentSlope);
      dx3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdx7x7',euPercentSlope);
      dy3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdy7x7',euPercentSlope);
   {$EndIf}

   //grid containing DEM is zero based, 0,0 at the SW corner
   for x := 1 to (DEMHeader.NumCol - 2) do begin   //don't worry about edge and corner cases, leave them missing (merge with neighbor if needed)
      for y := 1 to (DEMHeader.NumRow - 2) do begin
         {$IfDef CarlosErrorTrap}
            if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                WriteLineToDebugFile('Point to check: ' + IntToStr(CarlosXRecord) + '/' + IntToStr(CarlosYRecord) + '  ' + DEMLocationString(CarlosXRecord,CarlosYRecord));
            end;
         {$EndIf}
         if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse,1) then begin //get elevations in first tier surrounding point
            PixelSpacingAndRotation(x,y,Lat,Long,dx,dy,GridTrueAngle,true);

            {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                  WriteLineToDebugFile('Pixel spacing, x=' + RealToString(dx,-8,-2) + '  y=' + RealToString(dy,-8,-2));
                  WriteLineToDebugFile('znw=' + RealToString(znw,-8,-2) + '  zn=' + RealToString(zn,-8,-2) + '  zne=' + RealToString(zne,-8,-2));
                  WriteLineToDebugFile('zw=' + RealToString(zw,-8,-2) + '  z=' + RealToString(z,-8,-2) + '  ze=' + RealToString(zne,-8,-2));
                  WriteLineToDebugFile('zsw=' + RealToString(zsw,-8,-2) + '  zs=' + RealToString(zs,-8,-2) + '  zse=' + RealToString(zse,-8,-2));
               end;
            {$EndIf}

            dzdx_3x3 := 1 / dx / 6 * (+zne+ze+zse-znw-zw-zsw);
            dzdy_3x3 := 1 / dy / 6 * (+znw+zn+zne-zsw-zs-zse);
            slope_3x3 := 100 * sqrt(sqr(dzdx_3x3) + sqr(dzdy_3x3));
            DEMGlb[Slope3x3Grid].SetGridElevation(x,y,slope_3x3);
            if (slope_3x3 > 0.01) then begin
               Aspect_3x3 := CompassAngleInRangeFloat32(HeadingOfLine(dzdx_3x3,dzdy_3x3)+180);
               DEMGlb[Aspect_3x3Grid].SetGridElevation(x,y,Aspect_3x3);
            end;

            {$IfDef MakePartialGrids}
               DEMGlb[dxGrid].SetGridElevation(x,y,100 * dzdx_3x3);
               DEMGlb[dyGrid].SetGridElevation(x,y,100 * dzdy_3x3);
            {$EndIf}

            {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                  WriteLineToDebugFile('slope_3x3=' + RealToString(slope_3x3,-8,-2) + '  dzdx=' + RealToString(100*dzdx_3x3,-8,-2) + '  dzdy=' + RealToString(100*dzdy_3x3,-8,-2) + '  aspect=' + RealToString(Aspect_3x3,-8,-2));
               end;
            {$EndIf}

            //don't worry about second row from edges
            if (x > 1) and (x < (DEMHeader.NumCol - 3)) and (y > 1) and (y < (DEMHeader.NumRow - 3)) then begin
               if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse,2) then begin //get elevations in second tier surrounding point
                  {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                        WriteLineToDebugFile('znw=' + RealToString(znw,-8,-2) + '  zn=' + RealToString(zn,-8,-2) + '  zne=' + RealToString(zne,-8,-2));
                        WriteLineToDebugFile('zw=' + RealToString(zw,-8,-2) + '  z=' + RealToString(z,-8,-2) + '  ze=' + RealToString(zne,-8,-2));
                        WriteLineToDebugFile('zsw=' + RealToString(zsw,-8,-2) + '  zs=' + RealToString(zs,-8,-2) + '  zse=' + RealToString(zse,-8,-2));
                     end;
                  {$EndIf}
                  dzdx_5x5 := 1 / (2*dx) / 6 * (+zne+ze+zse-znw-zw-zsw);
                  dzdy_5x5 := 1 / (2*dy) / 6 * (+znw+zn+zne-zsw-zs-zse);
                  slope_5x5 := 100 * sqrt(sqr(dzdx_5x5) + sqr(dzdy_5x5));
                  DEMGlb[slope5x5Grid].SetGridElevation(x,y,slope_5x5);

                  {$IfDef MakePartialGrids}
                     DEMGlb[dx2Grid].SetGridElevation(x,y,100 * dzdx_5x5);
                     DEMGlb[dy2Grid].SetGridElevation(x,y,100 * dzdy_5x5);
                  {$EndIf}

                  if (slope_5x5 > 0.01) then begin
                     Aspect_5x5 := CompassAngleInRangeFloat32(HeadingOfLine(dzdx_5x5,dzdy_5x5)+180);
                     DEMGlb[Aspect_5x5Grid].SetGridElevation(x,y,Aspect_5x5);
                  end;

                  {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                        WriteLineToDebugFile('slope_5x5=' + RealToString(slope_5x5,-8,-2) + '  dzdx=' + RealToString(100*dzdx_5x5,-8,-2) + '  dzdy=' + RealToString(100*dzdy_5x5,-8,-2) +
                             '  aspect=' + RealToString(Aspect_5x5,-8,-2));
                     end;
                  {$EndIf}

                  dzdx_4th := 4/3*dzdx_3x3-1/3*dzdx_5x5;
                  dzdy_4th := 4/3*dzdy_3x3-1/3*dzdy_5x5;

                  slope_4th := 100 * sqrt(sqr(dzdx_4th) + sqr(dzdy_4th));

                  DEMGlb[Slope4thGrid].SetGridElevation(x,y,slope_4th);
                  if (slope_4th > 0.01) then begin
                     Aspect4th := CompassAngleInRangeFloat32(HeadingOfLine(dzdx_4th,dzdy_4th)+180);
                     DEMGlb[Aspect4thGrid].SetGridElevation(x,y,Aspect4th);
                  end;

                  if (x > 2) and (x < (DEMHeader.NumCol - 4)) and (y > 2) and (y < (DEMHeader.NumRow - 4)) then begin
                     if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse,3) then begin //get elevations in third tier surrounding point
                        dzdx_7x7 := 1 / (3*dx) / 6 * (+zne+ze+zse-znw-zw-zsw);
                        dzdy_7x7 := 1 / (3*dy) / 6 * (+znw+zn+zne-zsw-zs-zse);
                        slope_7x7 := 100 * sqrt(sqr(dzdx_7x7) + sqr(dzdy_7x7));

                        DEMGlb[slope7x7Grid].SetGridElevation(x,y,slope_7x7);
                        if (slope_7x7 > 0.01) then begin
                           Aspect_7x7 := CompassAngleInRangeFloat32(HeadingOfLine(dzdx_7x7,dzdy_7x7)+180);
                           DEMGlb[Aspect_7x7Grid].SetGridElevation(x,y,Aspect_7x7);
                        end;

                        dzdx_6th := 3/2*dZdX_3x3 - 3/5*dZdX_5x5 + 1/10*dZdX_7x7;
                        dzdy_6th := 3/2*dZdy_3x3 - 3/5*dZdy_5x5 + 1/10*dZdy_7x7;

                        {$IfDef MakePartialGrids}
                           DEMGlb[dx3Grid].SetGridElevation(x,y,100 * dzdx_7x7);
                           DEMGlb[dy3Grid].SetGridElevation(x,y,100 * dzdy_7x7);
                        {$EndIf}

                        slope_6th := 100 * sqrt(sqr(dzdx_6th) + sqr(dzdy_6th));

                        DEMGlb[Slope6thGrid].SetGridElevation(x,y,slope_6th);
                        if (slope_6th > 0.01) then begin
                           Aspect6th := CompassAngleInRangeFloat32(HeadingOfLine(dzdx_6th,dzdy_6th)+180);
                           DEMGlb[Aspect6thGrid].SetGridElevation(x,y,Aspect6th);
                        end;
                     end;
                  end;

                  uncertainty_bndX := abs(dzdx_3x3-dzdx_5x5) / 3;  //will be needed later
                  uncertainty_bndY := abs(dzdy_3x3-dzdy_5x5) / 3;

                  dzdx_4th := dzdx_3x3 + (dzdx_3x3-dzdx_5x5) / 3;  //Now this is 4th order. It has been using Richardson Extrapolation after a 2nd order estimate
                  dzdy_4th := dzdy_3x3 + (dzdy_3x3-dzdy_5x5) / 3;

                  //The uncertainty bound for the slope should be computed using Interval Arithmetic. To make it simple I will introduce some extra variables
                  UB_fx := dzdx_4th + uncertainty_bndX;  //This stands for Upper Bound
                  LB_fx := dzdx_4th - uncertainty_bndX;  //This stands for Lower Bound
                  UB_fy := dzdy_4th + uncertainty_bndY;  //same for y
                  LB_fy := dzdy_4th - uncertainty_bndY;  //same for y
                  if (LB_fx > 0) then begin
                     LB_fx2 := LB_fx* LB_fx;
                     UB_fx2 := UB_fx* UB_fx;
                  end
                  else if (UB_fx < 0) then begin
                     LB_fx2 := UB_fx * UB_fx;
                     UB_fx2 := LB_fx * LB_fx;
                  end
                  else begin
                     LB_fx2 := 0;
                     UB_fx2 := max(LB_fx * LB_fx, UB_fx * UB_fx);
                  end;
                  if (LB_fy > 0) then begin
                     LB_fy2 := LB_fy* LB_fy;
                     UB_fy2 := UB_fy* UB_fy;
                  end
                  else if (UB_fy < 0) then begin
                     LB_fy2 := UB_fy* UB_fy;
                     UB_fy2 := LB_fy* LB_fy;
                  end
                  else begin
                     LB_fy2 := 0;
                     UB_fy2 := max(LB_fy* LB_fy, UB_fy* UB_fy);
                  end;

                  //Now we should find bounds for the sum of intervals
                  LB_m := LB_fx2 + LB_fy2;
                  UB_m := UB_fx2 + UB_fy2; //m stands for the sum of squares of the partial derivatives
                  UB_slope := 100*sqrt(UB_m);
                  LB_slope := 100*sqrt(LB_m); //slope should belong to the interval [LB_slope, UB_slope]
                  uncertainty_slope := (UB_slope-LB_slope);  //must be positive. equivalent to what denoted as SlopeErrBound
                  SlopeErrBound := abs(slope_3x3 - slope_5x5) / 3;
                  TruncationError := 1/3*(dZdX_3x3-dZdX_5x5);
                  DEMGlb[SlopeErrorBoundGrid].SetGridElevation(x,y,SlopeErrBound);
                  DEMGlb[SlopeUncertaintyGrid].SetGridElevation(x,y,uncertainty_slope);
                  DEMGlb[TruncationErrorGrid].SetGridElevation(x,y,TruncationError);

                  {$IfDef CarlosErrorTrap}
                     if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                        WriteLineToDebugFile('slope_4th=' + RealToString(slope_4th,-8,-2) + '  dzdx_4th=' + RealToString(dzdx_4th,-8,-2) + '  dzdy_4th =' + RealToString(dzdy_4th ,-8,-2));
                        WriteLineToDebugFile('uncertainty_bndX=' + RealToString(uncertainty_bndX,-8,-2) + '  uncertainty_bndY=' + RealToString(uncertainty_bndY,-8,-2));
                        WriteLineToDebugFile('UB_slope=' + RealToString(UB_slope,-8,-2) + '  LB_slope=' + RealToString(LB_slope,-8,-2));
                        WriteLineToDebugFile('uncertainty_slope=' + RealToString(uncertainty_slope,-8,-2) + '  SlopeErrBound=' + RealToString(SlopeErrBound,-8,-2));
                     end;
                  {$EndIf}
               end;
            end;
         end;
      end;
   end;
   MapAndSave(Slope3x3Grid);
   MapAndSave(slope5x5Grid);
   MapAndSave(slope7x7Grid);
   MapAndSave(Slope4thGrid);
   MapAndSave(Slope6thGrid);

   MapAndSave(Aspect_3x3Grid);
   MapAndSave(Aspect_5x5Grid);
   MapAndSave(Aspect_7x7Grid);
   MapAndSave(Aspect4thGrid);
   MapAndSave(Aspect6thGrid);

   MapAndSave(SlopeErrorBoundGrid);
   MapAndSave(SlopeUncertaintyGrid);
   MapAndSave(TruncationErrorGrid);

   {$IfDef MakePartialGrids}
      MapAndSave(dxGrid);
      MapAndSave(dx2Grid);
      MapAndSave(dx3Grid);
      MapAndSave(dyGrid);
      MapAndSave(dy2Grid);
      MapAndSave(dy3Grid);
   {$EndIf}

    if not Save then AllAspects;  //create aspect roses

   {$IfDef RecordProblems} WriteLineToDebugFile('tDEMDataSet.CarlosLopezSlopeMaps out'); {$EndIf}
end;
