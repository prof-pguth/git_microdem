{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}


procedure SlopeMethodsReportFromLatLong(DEM : integer; lat,Long : float64; Title : shortstring = '');
var
   Col,Row : integer;
begin
   if DEMglb[DEM].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row) then SlopeMethodsReport(DEM,Col,Row,Title);
end;


procedure StartSlopeReport(var Results : tStringList; DEM,xp,yp : integer; Title : shortstring = '');
var
   SlopeAspectRec : tSlopeAspectRec;
begin
   Results := tStringList.Create;
   Results.Add(DEMglb[DEM].AreaName);
   if (Title <> '') then Results.Add(Title);
   Results.Add('Point: ' + DEMglb[DEM].DEMLocationString(xp,yp));
   Results.Add('Col=' + IntToStr(xp) + '  & row=' + IntToStr(yp));
   Results.Add('Classified as: ' + PointTypeName(DEMglb[DEM].ClassifyAPoint(xp,yp)));
   Results.Add('');
   if DEMglb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,xp,yp,SlopeAspectRec) then begin
      Results.Add(  RealToString(SlopeAspectRec.znw,13,2) + RealToString(SlopeAspectRec.zn,13,2) + RealToString(SlopeAspectRec.zne,13,2) +  MessLineBreak +
                    RealToString(SlopeAspectRec.zw,13,2)  + RealToString(SlopeAspectRec.z,13,2)  + RealToString(SlopeAspectRec.ze,13,2)  +  MessLineBreak +
                    RealToString(SlopeAspectRec.zsw,13,2) + RealToString(SlopeAspectRec.zs,13,2) + RealToString(SlopeAspectRec.zse,13,2) +  MessLineBreak + '  ' + MessLineBreak );
      Results.Add('');
   end;
end;


procedure AddSlopeAtPoint(var Results : tStringList; DEM,xp,yp : integer);
var
   k,Decs : integer;
   SlopeAspectRec : tSlopeAspectRec;
   aLine : shortstring;
begin
   if DEMglb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,xp,yp,SlopeAspectRec) then begin
      aLine := SlopeMethodName(MDDef.SlopeCompute) + RealToString(SlopeAspectRec.SlopePercent,7,2) + '%' + RealToString(SlopeAspectRec.AspectDirTrue,6,1) + '°T  ';
      for k := 1 to 15 do begin
         if (abs(SlopeAspectRec.B[k]) > 0.00000001) then begin
            case k of
                1 : Decs := 2;
                2,3 : Decs := 4;
                4..6 : Decs := 6;
                else Decs := 8;
            end;
            Aline := Aline + ' ';
            Aline := Aline + RealToString(SlopeAspectRec.B[k],-12,Decs);
            if (k > 1) then Aline := Aline + '*' + TrendSurMultiples[k];
         end;
      end;
      Results.Add(aLine);
   end;
end;


procedure TrendSurfaceEquations(DEM,xp,yp : integer; Title : shortstring = '');
var
   Results : tStringList;
   //CurMethod : byte;
   i,j,m : integer;
begin
   try
      SaveBackupDefaults;
      MDDef.EvansApproximationAllowed := false;
      {$IfDef OptionSaveSlopeComputePoints} SaveSlopeComputationPoints := true; {$EndIf}
      StartSlopeReport(Results,DEM,xp,yp,Title);
      MDDef.SlopeCompute.AlgorithmName := smLSQ;
      for i := 1 to 4 do begin
         MDDef.SlopeCompute.WindowRadius := i;
         for j := 1 to 4 do begin
            MDDef.SlopeCompute.LSQorder := j;
            for m := 1 to 3 do begin
               MDDef.SlopeCompute.UsePoints := m;
               if ((m = UseEdge) and (i > 1)) or (m in [UseAll,UseQueens]) then begin
                  AddSlopeAtPoint(Results,DEM,xp,yp);
               end;
            end;
         end;
         Results.Add('');
      end;
      DisplayAndPurgeStringList(Results,'Point Slope Algorithms');
   finally
      RestoreBackupDefaults;
      {$IfDef OptionSaveSlopeComputePoints} SaveSlopeComputationPoints := false; {$EndIf}
   end;
end;


procedure PointCurvatures(DEM,xp,yp : integer; Title : shortstring = '');
var
   Results : tStringList;
   //CurveMethod : byte;
   i,j : integer;


      procedure AddCurvatureAtPoint(var Results : tStringList; DEM,xp,yp : integer);
      var
         //k,Decs : integer;
         //SlopeAspectRec : tSlopeAspectRec;
         aLine : shortstring;
         PlanC,ProfC,TangC : float64;
      begin
         if ComputeLSQCurvature(DEM,eucurv_plan,xp,yp,PlanC) and ComputeLSQCurvature(DEM,eucurv_tang,xp,yp,TangC) and
            ComputeLSQCurvature(DEM,eucurv_tang,xp,yp,ProfC) then begin
            aLine := SlopeMethodName(MDDef.CurveCompute) + RealToString(PlanC,12,8) + RealToString(TangC,12,8) + RealToString(ProfC,12,8);
            Results.Add(aLine);
         end;
      end;


begin
   try
      SaveBackupDefaults;
      MDDef.EvansApproximationAllowed := false;
      MDDef.CurveCompute.UsePoints := UseAll;
      StartSlopeReport(Results,DEM,xp,yp,Title);
      Results.Add('         Plan     Tangential     Profile  ');
      MDDef.CurveCompute.AlgorithmName := smLSQ;
      for i := 1 to 4 do begin
         MDDef.CurveCompute.WindowRadius := i;
         for j := 2 to 4 do begin
            MDDef.CurveCompute.LSQorder := j;
            AddCurvatureAtPoint(Results,DEM,xp,yp);
         end;
         Results.Add('');
      end;
      DisplayAndPurgeStringList(Results,'Point Curvature Algorithms');
   finally
      RestoreBackupDefaults;
   end;
end;



{$IfDef SlopeWindowEdgeEffect}
  procedure LocalTrendSurfaceEdgeEffects(DEM,xp,yp : integer; Title : shortstring = '');
  var
     Results : tStringList;
     CurMethod : byte;
     i,j,m : integer;
     aLine : shortstring;
     SAR :  tSlopeAspectRec;

     procedure TryOne;
     begin
        if DEMglb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,xp,yp,SAR) then begin
          aLine := SlopeMethodName(MDDef.SlopeCompute) + RealToString(SAR.SlopePercent,7,2) + '%' + RealToString(SAR.AspectDirTrue,6,1) + '°T';
          Results.Add(aline);
        end;
     end;

  begin
     try
        SaveBackupDefaults;
        MDDef.EvansApproximationAllowed := false;
        StartSlopeReport(Results,DEM,xp,yp,Title);
        MDDef.SlopeCompute.AlgorithmName := smLSQ;
        MDDef.SlopeCompute.WindowRadius := 1;
        MDDef.SlopeCompute.LSQorder := 2;
        TestEdgeEffect := false;
        TryOne;
        TestEdgeEffect := true;
        TryOne;
        Results.Add('');
        MDDef.SlopeCompute.WindowRadius := 2;
        MDDef.SlopeCompute.LSQorder := 3;
        TestEdgeEffect := false;
        TryOne;
        TestEdgeEffect := true;
        TryOne;
        Results.Add('');
        MDDef.SlopeCompute.WindowRadius := 3;
        MDDef.SlopeCompute.LSQorder := 4;
        TestEdgeEffect := false;
        TryOne;
        TestEdgeEffect := true;
        TryOne;
        DisplayAndPurgeStringList(Results,'Point Slope Algorithms Edge Effects');
     finally
        RestoreBackupDefaults;
     end;
  end;
{$EndIf}


procedure LocalTrendSurface(DEM,xp,yp : integer; Title : shortstring = '');
var
   //CurMethod : byte;
   //i,j,NPts,m,
   x,y  : integer;
   SAR :  tSlopeAspectRec;
   Findings : tStringList;
   aline : Shortstring;
   //xf,yf : float64;
   //CMethod : byte;
   //i,j,   m,
   //x,ySlopeAspectRec;
   //Findings : tStringList;
   //aline : Shortstring;
   //xf,yf : float64;
   z : float32;
   {$IfDef TrendSurfacePointClouds}
      Points : ^tPointXYZIArray;
      Outf : file;
      fName1,fname2,fName3 : PathStr;
   {$EndIf}

   procedure DoOne {$IfDef TrendSurfacePointClouds} (fName : PathStr; r,g,b : byte) {$EndIf};
   var
      x,y,k : integer;
      xf,yf,z : float64;
   begin
      if DEMGlb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,xp,yp,SAR) then begin
         Findings.Add('');
         Findings.Add('');

         Findings.Add(SlopeMethodName(MDDef.SlopeCompute) + RealToString(SAR.SlopePercent,7,2) + '%' + RealToString(SAR.AspectDirTrue,6,1) + '°T  ');

         aLine := '';
         for x := -MDdef.SlopeCompute.WindowRadius to MDdef.SlopeCompute.WindowRadius do aline := aline + ',x=' + IntToStr(x);
         Findings.Add(aLine);
         for y := MDdef.SlopeCompute.WindowRadius downto -MDdef.SlopeCompute.WindowRadius do begin
            aLine := 'y=' + intToStr(y);
            yf := y*SAR.dy;
            for x := -MDdef.SlopeCompute.WindowRadius to MDdef.SlopeCompute.WindowRadius do begin
               xf := x * SAR.dx;
               z := CalculateTrendSurface(MDdef.SlopeCompute.LSQorder, SAR.B,xf,yf);
               aLine := Aline + ',' + RealToString(z,-12,-2);
            end;
            Findings.Add(aLine);
         end;

         aline := ' ';
         for k := 2 to 15 do begin
            if {(k=1) or} (abs(SAR.B[k]) > 0.00000001) then begin
               Aline := Aline + ',' + TrendSurMultiples[k];
            end;
         end;
         Findings.Add(aLine);
         aLine := RealToString(SAR.B[1],-15,-9);
         for k := 2 to 15 do begin
            if (abs(SAR.B[k]) > 0.00000001) then begin
               Aline := Aline + ',' + RealToString(SAR.B[k],-15,-9);
            end;
         end;
         Findings.Add(aLine);

         {$IfDef TrendSurfacePointClouds}
            NPts := 0;
            xf := -MDdef.SlopeCompute.WindowRadius;
            while xf <= MDdef.SlopeCompute.WindowRadius do begin
               yf := -MDdef.SlopeCompute.WindowRadius;
               while yf <= MDdef.SlopeCompute.WindowRadius do begin
                  z := CalculateTrendSurface(MDdef.SlopeCompute.LSQorder, SAR.B,xf,yf);
                  inc(NPts);
                  Points^[succ(NPTs)].x := xf;
                  Points^[succ(NPTs)].y := yf;
                  Points^[succ(NPTs)].z := z;
                  Points^[succ(NPTs)].Int := r;
                  Points^[succ(NPTs)].Int2 := g;
                  Points^[succ(NPTs)].Int3 := b;
                  yf := yf + 0.005;
               end;
               xf := xf + 0.005;
            end;
            {$IfDef RecordLSQ} WriteLineToDebugFile(fName + ' has pts=' + IntToStr(NPts)); {$EndIf}
            AssignFile(Outf,fName);
            rewrite(Outf,1);
            BlockWrite(OutF,Points^,NPTs * SizeOf(tPointXYZI));
            CloseFile(outf);
         {$EndIf}
     end;
   end;
  // end;

begin
   try
      SaveBackupDefaults;
      MDDef.EvansApproximationAllowed := false;
      Findings := tStringList.Create;
      Findings.Add('Elevations at ' + DEMGlb[DEM].DEMLocationString(xp,yp));
      aLine := '';
      for x := -MDdef.SlopeCompute.WindowRadius to MDdef.SlopeCompute.WindowRadius do aline := aline + ',x=' + IntToStr(x);
      Findings.Add(aLine);

      for y := MDdef.SlopeCompute.WindowRadius downto -MDdef.SlopeCompute.WindowRadius do begin
         aLine := 'y=' + intToStr(y);
         for x := -MDdef.SlopeCompute.WindowRadius to MDdef.SlopeCompute.WindowRadius do begin
            if DEMGlb[DEM].GetElevMetersOnGrid(xp+x,yp+y,z) then
               aLine := Aline + ',' + RealToString(z,-12,-2);
         end;
         Findings.Add(aLine);
      end;

      {$IfDef TrendSurfacePointClouds}
         New(Points);
         NPts := 0;
         xf := -MDdef.SlopeCompute.WindowRadius;
         while xf <= MDdef.SlopeCompute.WindowRadius do begin
            yf := -MDdef.SlopeCompute.WindowRadius;
            while yf <= MDdef.SlopeCompute.WindowRadius do begin
               if DEMGlb[DEM].GetElevMeters(xp+xf,yp+yf,z) then  begin
                  inc(NPts);
                  Points^[NPTs].x := xf;
                  Points^[NPTs].y := yf;
                  Points^[NPTs].z := z;
                  Points^[NPTs].Int := 255;
                  Points^[NPTs].Int2 := 0;
                  Points^[NPTs].Int3 := 0;
               end;
               yf := yf + 0.005;
            end;
            xf := xf + 0.005;
         end;
         fName1 := NextFileNumber(MdTempDir,'elevs_','.xyzi');
         fName2 := NextFileNumber(MdTempDir,'window_5x5_','.xyzi');
         fName3 := NextFileNumber(MdTempDir,'window_3x3_','.xyzi');
         {$IfDef RecordLSQ} WriteLineToDebugFile(fName1 + ' has pts=' + IntToStr(NPts)); {$EndIf}
         AssignFile(Outf,fName1);
         rewrite(Outf,1);
         BlockWrite(OutF,Points^,NPTs * SizeOf(tPointXYZI));
         CloseFile(outf);
      {$EndIf}

      MDDef.SlopeCompute.AlgorithmName := smLSQ;
      MDdef.SlopeCompute.UsePoints := useAll;
      MDdef.SlopeCompute.WindowRadius := 2;
      MDdef.SlopeCompute.LSQorder := 3;
      DoOne {$IfDef TrendSurfacePointClouds} (fName2,0,255,0) {$EndIf};
      MDdef.SlopeCompute.WindowRadius := 1;
      MDdef.SlopeCompute.LSQorder := 2;
      DoOne {$IfDef TrendSurfacePointClouds} (fName3,0,0,255) {$EndIf};
      DisplayAndPurgeStringList(Findings,'Local Trend Surface');


      {$IfDef TrendSurfacePointClouds}
         Dispose(Points);
         MDDef.New3Dviewer := true;
         FMX3dViewer(true,fName1,Fname2,fName3,'','');
      {$EndIf}
   finally
      RestoreBackupDefaults;
   end;
end;


procedure TrendSurfacePartialDerivatives(DEM,xp,yp : integer; Title : shortstring = '');
var
   Results : tStringList;
   //CurMethod : byte;
   i,j,m : integer;
   aLine : shortstring;
   SAR :  tSlopeAspectRec;
begin
   try
      SaveBackupDefaults;
      MDDef.EvansApproximationAllowed := false;
      StartSlopeReport(Results,DEM,xp,yp,Title);
      MDDef.SlopeCompute.AlgorithmName := smLSQ;
      for i := 1 to 4 do begin
         MDDef.SlopeCompute.WindowRadius := i;
         for j := 1 to 4 do begin
            MDDef.SlopeCompute.LSQorder := j;
            for m := 1 to 3 do begin
               MDDef.SlopeCompute.UsePoints := m;
               if DEMglb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,xp,yp,SAR) then begin
                  aLine := SlopeMethodName(MDDef.SlopeCompute) + RealToString(SAR.SlopePercent,7,2) + '%' + RealToString(SAR.AspectDirTrue,6,1) + '°T  ' +
                     ' zx=' + RealToString(SAR.zx,9,6) + ' zy=' + RealToString(SAR.zy,9,6);
                  if j > 1 then aLine := aLine + ' zxx=' + RealToString(SAR.zxx,9,6) + ' zxy=' + RealToString(SAR.zxy,9,6) +  ' zxy=' + RealToString(SAR.zxy,9,6);
                  if j > 2 then aLine := aLine + ' zxxx=' + RealToString(SAR.zxxx,9,6) + ' zxxy=' + RealToString(SAR.zxxy,9,6) +  ' zxyy=' + RealToString(SAR.zxyy,9,6) +  ' zyyy=' + RealToString(SAR.zyyy,9,6);
                  Results.Add(aline);
               end;
            end;
         end;
         Results.Add('');
      end;
      DisplayAndPurgeStringList(Results,'Point Slope Algorithms');
   finally
      RestoreBackupDefaults;
   end;
end;


procedure TrendSurfaceEquationsUseCenterPoint(DEM,xp,yp : integer; Title : shortstring = '');
const
   Fill : array[1..2] of shortstring = (' Use center ',' Skip center');
var
   Results : tStringList;
   //CurMethod : byte;
   i,j,m : integer;
begin
   try
      SaveBackupDefaults;
      MDDef.EvansApproximationAllowed := false;
      StartSlopeReport(Results,DEM,xp,yp,Title);
      MDDef.SlopeCompute.AlgorithmName := smLSQ;
      for i := 1 to 4 do begin
         MDDef.SlopeCompute.WindowRadius := i;
         for j := 1 to 2 do begin
            MDDef.SlopeCompute.LSQorder := j;
            for m := 1 to 2 do begin
               SkipCenterPoint := (m = 2);
               //aMethod := SlopeMethodName(MDDef.SlopeCompute) + Fill[m];
               AddSlopeAtPoint(Results,DEM,xp,yp);
            end;
         end;
         Results.Add('');
      end;
      DisplayAndPurgeStringList(Results,'Point Slope Algorithms');
   finally
      RestoreBackupDefaults;
      SkipCenterPoint := false;
   end;
end;


procedure SlopeMethodsReport(DEM,xp,yp : integer; Title : shortstring = '');
var
   Results : tStringList;
   CurMethod : byte;
   i,j,k : integer;
   //aLine : shortString;
   SAR : tSlopeAspectRec;

   procedure DoOneMethod;
   begin
      if DEMglb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,xp,yp,SAR,true,false) then begin
         Results.Add(RealToString(SAR.SlopePercent,8,2) + '%   ' + RealToString(SAR.SlopeDegree,8,2) + '°   ' +
            RealToString(SAR.AspectDirTrue,7,1) + '°T   ' + RealToString(SAR.AspectDirGrid,8,1) + '° Grid    '  + SlopeMethodName(MDDef.SlopeCompute))
      end;
   end;

begin
   SaveBackupDefaults;
   StartSlopeReport(Results,DEM,xp,yp,Title);
   Results.Add('         Slope                 Aspect                    Method');
   for CurMethod := FirstSlopeMethod to LastSlopeMethod do begin
      MDDef.SlopeCompute.AlgorithmName := CurMethod;
      if (CurMethod = smLSQ) then begin
         for i := 1 to 4 do begin
            MDDef.SlopeCompute.WindowRadius := i;
            for j := 1 to 4 do begin
               MDDef.SlopeCompute.LSQorder := j;
               DoOneMethod;
            end;
         end;
      end
      else begin
         for i := 1 to 4 do begin
            MDDef.SlopeCompute.WindowRadius := i;
            DoOneMethod;
         end;
      end;
   end;
   DisplayAndPurgeStringList(Results,'Point Slope Algorithms');
   RestoreBackupDefaults
end;


procedure DirectionalSlopesReport(DEM : integer; Lat,Long : float64);
var
   MenuStr : AnsiString;
   x,y : integer;
   xgf,ygf : float64;
   SAR : tSlopeAspectRec;
begin
   DEMglb[DEM].LatLongDegreeToDEMGrid(Lat,Long,xgf,ygf);
   x := round(xgf);
   y := round(ygf);

   DEMglb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,x,y,SAR,false,false);
   with DEMglb[DEM] do MenuStr := 'Point: ' + DEMglb[DEM].DEMLocationString(x,y) +  MessLineBreak +  MessLineBreak + 'Elevations: ' +  MessLineBreak +
              RealToString( SAR.znw,13,2) + RealToString( SAR.zn,13,2) + RealToString( SAR.zne,13,2) +  MessLineBreak +
              RealToString( SAR.zw,13,2)  + RealToString( SAR.z,13,2)   + RealToString( SAR.ze,13,2)  +  MessLineBreak +
              RealToString( SAR.zsw,13,2) + RealToString( SAR.zs,13,2)  + RealToString( SAR.zse,13,2) +  MessLineBreak + '  ' + MessLineBreak +
              'Directional Slopes: ' +  MessLineBreak  +
              RealToString( 100*(SAR.znw-SAR.z)/AverageDiaSpace,12,2) + '%' + RealToString( 100*(SAR.zn-SAR.z)/AverageYSpace,12,2) + '%' + RealToString( 100*(SAR.zne-SAR.z)/AverageDiaSpace,12,2) + '%' +  MessLineBreak +
              RealToString( 100*(SAR.zw-SAR.z)/AverageXSpace,12,2) + '%' + '             ' + RealToString( 100*(SAR.ze-SAR.z)/AverageXSpace,12,2) + '%' +  MessLineBreak +
              RealToString( 100*(SAR.zsw-SAR.z)/AverageDiaSpace,12,2) + '%' + RealToString( 100*(SAR.zs-SAR.z)/AverageYSpace,12,2) + '%' + RealToString( 100*(SAR.zse-SAR.z)/AverageDiaSpace,12,2) + '%'+  MessLineBreak + '  ' +
              MessLineBreak +
              'Point slope:' + MessLineBreak +   RealToString(SAR.SlopePercent,8,2) + '%   ' + RealToString(SAR.SlopeDegree,8,2) + '°   aspect ' +
                   RealToString(SAR.AspectDirTrue,8,1) + '°    ' + SlopeMethodName(MDDef.SlopeCompute);
   MessageToContinue(MenuStr);
end;





