unit weapons_fan_thread;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$IFDEF DEBUG}
   {$Define NoParallelFor} //used to debug only
{$ELSE}
   //{$Define NoParallelFor}
{$ENDIF}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordFanProblems}

      //{$Define RecordThreadProgress}
      //{$Define RecordThreadTiming}
      //{$Define RecordFanRadialsInMetadata}
      //{$Define RecordMainThreadProgress}
      //{$Define RecordFanMetadata}
      //{$Define RecordFanSaveBitmap}
      //{$Define RecordVisiblePoints}
      //{$Define RecordFanSaveRadials}
      //{$Define RecordFanRadials}       //moderate slowdown
      //{$Define AllowFanCoverageDEM}
      //{$Define RecordFanInnerLoops}    //severe slowdown
      {$IfDef Android}
         //{$Define AndroidProgressMonitor}
      {$EndIf}
   {$ENDIF}
{$EndIf}




interface

uses
  System.UITypes,System.SyncObjs,System.Math,
  {$IfDef VCL}
     Forms,Graphics,
  {$EndIf}
  {$IfDef ExFMX3D}
     FMX.Graphics,
  {$EndIf}
  {$IfDef MSWindows}
     Windows,
  {$EndIf}
  {$IfDef RecordTime}
     System.Diagnostics,System.TimeSpan,
  {$Endif}

   {$IfDef NoParallelFor}
   {$Else}
      System.Threading,
   {$EndIf}
  Classes,  SysUtils,
  Petmar_types,Petmar,
  DEMDefs,PetImage,
  DEMMapDraw;

type
  tDrawFan = class
  private
    procedure SaveFanBitmaps;
    procedure DoRadial(Alpha : float64);
    procedure RadialToEdge(x,y : integer);
    procedure DoSideOfBox(Side : integer);
    procedure DoPieSlice(FirstAngle : float64; NumRays : integer);
  protected
     {$If Defined(RecordFanSaveRadials) or Defined(RecordFanProblems)}
        RadialsSaved : integer;
     {$EndIf}
  public
    BaseMapDraw,aNewMapDraw : DEMMapDraw.tMapDraw;
    WeaponsFan : tWeaponsFan;
    SavedRadialPath  : PathStr;
    VisBitmap,MaskedBitmap : tMyBitmap;
    ComputeFanMetadata,useVeg : boolean;
    PointsPerRay,NumRays, RadialsDone,TotalRadials,
    VisPts,MaskedPts : integer;
    FanDistOut,
    PercentCoverage : float64;
    FanMetaData : tStringList;
    VisName,MaskName,MergeName,CoverName : PathStr;
    MaskRGB,MixedRGB : TPlatformColor;
    BMPMemVis,BMPMemMask : tBMPMemory;
    constructor Create(inUseVeg : boolean; inMap : tMapDraw; inWeaponsFan : tWeaponsFan; inVisName,inMaskName,inMergeName,InCoverName : PathStr);
    procedure ExecuteFan;
  end;

var
   FanCoverageDEM : integer;

function FanCoverage(CoverName : PathStr) : float64;

implementation

uses
   {$IfDef VCL}
      BaseGraf,
      DEMMapF,
      Thread_timers,
   {$EndIf}

   {$IfDef FMXMD}
      Main_fm_md,
   {$EndIf}

   Petmath,
   BaseMap,
   DEMESRIShapeFile,
   DEMCoord,
   DEMDef_routines;


function FanCoverage(CoverName : PathStr) : float64;
var
   Results : tStringList;
begin
   FanCoverage := 0;
   Results := tStringList.Create;
   Results.LoadFromFile(CoverName);
   FanCoverage := StrToFloat(ptTrim(Results.Strings[0]));
   Results.Free;
   DeleteFileIfExists(CoverName);
end;


{ tFanThread }

procedure tDrawFan.SaveFanBitmaps;
var
   Results : tStringList;
   fName : PathStr;
   bmp   : tMyBitmap;
begin
   {$IfDef RecordFanProblems} WriteLineToDebugFile('tFanThread.SaveFanBitmaps in'); {$EndIf}
   try
      {$IfDef VCL}
         if (MergeName <> '') then begin
            CreateBitmap(bmp,VisBitmap.Width,VisBitmap.Height);
            bmp.Canvas.CopyMode := cmSrcAnd;
            bmp.Canvas.Draw(0,0,MaskedBitmap);
            bmp.Canvas.Draw(0,0,VisBitmap);
            PetImage.SaveBitmap(bmp,MergeName);
            aNewMapDraw.WriteMapsWorldFile(MergeName);
            bmp.Free;
         end;
      {$EndIf}

      if (VisName <> '') then begin
         PetImage.SaveBitmap(VisBitmap,VisName);
         aNewMapDraw.WriteMapsWorldFile(VisName);
         CoverName := ChangeFileExt(VisName,'.txt');
         {$IfDef RecordFanProblems} WriteLineToDebugFile('saved ' + VisName); {$EndIf}
      end;

      if (MaskName <> '') then begin
         PetImage.SaveBitmap(MaskedBitmap,MaskName);
         aNewMapDraw.WriteMapsWorldFile(MaskName);
         {$IfDef RecordFanProblems} WriteLineToDebugFile('saved ' + MaskName); {$EndIf}
      end;

      if (CoverName <> '') then begin
         Results := tStringList.Create;
         with WeaponsFan do Results.Add(RealToString(PercentCoverage,8,2));
         Results.SaveToFile(CoverName);
         Results.Free;
         {$IfDef RecordFanProblems} WriteLineToDebugFile('saved ' + CoverName); {$EndIf}
      end;

      if (FanMetaData <> Nil) then begin
         fName := ChangeFileExt(VisName,'.met');
         FanMetadata.SaveToFile(fName);
         FanMetaData.Free;
         {$IfDef RecordFanProblems} WriteLineToDebugFile('saved ' + fName); {$EndIf}
      end;
      {$IfDef MSWindows}
      DEMESRIShapeFile.AddProjectionFile(VisName);
      {$EndIf}
   except
      on EOutOfResources do begin end;
   end;
   {$IfDef RecordFanProblems} WriteLineToDebugFile('tFanThread.SaveFanBitmaps out');  {$EndIf}
end;

      function DistanceOK(Dist : float64) : boolean; inline;
      begin
         Result := (Dist > MDDef.wf.ClosestBlockingDistance) and (Dist >= MDDef.DefWeaponsMinRange);
      end;


     procedure TDrawFan.RadialToEdge(x,y : integer);
     var
        Lat,Long,Dist,Bearing : float64;
     begin
         inc(RadialsDone);
         aNewMapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
         VincentyCalculateDistanceBearing(WeaponsFan.w_lat,WeaponsFan.w_long,lat,long,Dist,Bearing);
         if (( WeaponsFan.EndAngle < WeaponsFan.StartAngle) and ((Bearing >= WeaponsFan.StartAngle) or (Bearing <= WeaponsFan.EndAngle)))
            or ((Bearing >= WeaponsFan.StartAngle) and (Bearing <= WeaponsFan.EndAngle)) then DoRadial(Bearing);
     end;


procedure TDrawFan.DoRadial(Alpha : float64);
var
   PointOnRay,RayVis,RayMask,OffMap,x,y : integer;
   xgrids,ygrids, elevs : tRayArray32;
   dists : tRayArray64;
   VisPoints : array[0..MaxOnRay] of boolean;
   Lat2,Long2 : float64;

      {$IfDef VCL}
         procedure SaveRadial(Alpha : float64);
         var
            PointOnRay : integer;
            fName : PathStr;
            RadialsList : tStringList;
         begin
            if (WeaponsFan.Fan_Name = '') then WeaponsFan.Fan_Name := 'fan';
            fName := SavedRadialPath + WeaponsFan.Fan_Name +  '-' + RealToString(Alpha,-8,2) + '.PRO';
            {$IfDef RecordFanSaveRadials} if (RadialsSaved mod 25 = 0) or (RadialsSaved < 10) then WriteLineToDebugFile('Save radial=' + fName); {$EndIf}
            RadialsList := tStringList.Create;
            RadialsList.Add('meters');
            For PointOnRay := 0 to PointsPerRay do if (Elevs[PointOnRay] < 32000) then
                RadialsList.Add(RealToString(dists[PointOnRay],12,2) + RealToString(Elevs[PointOnRay],9,1));
            RadialsList.SaveToFile(fName);
            RadialsList.Free;
         end;
      {$EndIf}


      procedure RadialStraightRoute(LatLong : boolean; Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer);
      var
         I : integer;
         //InBounds : boolean;
         //xUTM1,xUTM2,yUTM1,yUTM2,
         Lat,Long,Lat4,Long4,
         //dx,dy,x,y,
         dLat,dLong,
         dDistance,FullDistance,Bearing : float64;

               (*
               procedure ComputeVersion1;
               var
                  i : integer;
               begin
                  WGS84DatumConstants.LatLongDegreeToUTM(Lat1,Long1,XUTM1,YUTM1);
                  WGS84DatumConstants.LatLongDegreeToUTM(Lat2,Long2,XUTM2,YUTM2);
                  dx := (xutm2-xutm1) / NumPoints;
                  dy := (yutm2-yutm1) / NumPoints;
                  dDistance := sqrt( sqr(xutm2-xutm1) + sqr(yutm2-yutm1)) / NumPoints;

                  for i := 0 to NumPoints do begin
                     x := xutm1 + i * dx;
                     y := yutm1 + i * dy;
                     if LatLong then WGS84DatumConstants.UTMtoLatLongDegree(x,y,ygrids[i],xgrids[i])
                     else DEMGLB[aNewMapDraw.DEMonMap].UTMtoDEMGrid(x,y,xgrids[i],ygrids[i],InBounds);
                     Dists[i] := i * dDistance;
                  end;
               end;
               *)

               procedure ComputeVersion2;
               var
                  i : integer;
               begin
                  for i := 0 to NumPoints do begin
                     Dists[i] := dDistance * i;
                     VincentyPointAtDistanceBearing(Lat1,Long1,Dists[i],Bearing,Lat,Long);
                     if LatLong then begin
                        XGrids[i] := Long;
                        YGrids[i] := Lat;
                     end
                     else DEMGLB[aNewMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xgrids[i],ygrids[i]);
                  end;
               end;

      begin {procedure RadialStraightRoute}
         VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,FullDistance,Bearing);    //does not depend on UTM zone
         dDistance := FullDistance / NumPoints;

         dLat := (Lat2-Lat1) / NumPoints;
         dLong := (Long2-Long1) / NumPoints;
         for i := 0 to NumPoints do begin
            Lat4 := Lat1+ i * dLat;
            Long4 := Long1 + i * dLong;
            if LatLong then begin
               XGrids[i] := Long4;
               YGrids[i] := Lat4;
            end
            else DEMGLB[aNewMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat4,Long4,xgrids[i],ygrids[i]);
            Dists[i] := i * dDistance;
         end;

         (*
         {$IfDef SingleStraightAlgorithm}
            if (FullDistance < MDDef.wf.SmartSwitchOver) and (Lat1 > 0) and (Lat2 > 0)) then begin
               ComputeVersion1;
            end
            else begin
               ComputerVersion2;
            end;
         {$Else}
            if (StraightAlgorithm = saLatLong) then begin
               dLat := (Lat2-Lat1) / NumPoints;
               dLong := (Long2-Long1) / NumPoints;
               for i := 0 to NumPoints do begin
                  Lat4 := Lat1+ i * dLat;
                  Long4 := Long1 + i * dLong;
                  if LatLong then begin
                     XGrids[i] := Long4;
                     YGrids[i] := Lat4;
                  end
                  else DEMGLB[aNewMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat4,Long4,xgrids[i],ygrids[i]);
                  Dists[i] := i * dDistance;
               end;
            end
            else if ((StraightAlgorithm = saDEMGrid) and (DEMGLB[aNewMapDraw.DEMonMap].DEMheader.DEMUsed = ArcSecDEM)) then begin
               DEMGLB[aNewMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat1,Long1,xgrids[0],ygrids[0]);
               DEMGLB[aNewMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat2,Long2,xgrids[NumPoints],ygrids[NumPoints]);
               dx := (xgrids[NumPoints]-xgrids[0]) / NumPoints;
               dy := (ygrids[NumPoints]-ygrids[0]) / NumPoints;
               for i := 0 to NumPoints do begin
                  XGrids[i] := XGrids[0] + i * dx;
                  YGrids[i] := YGrids[0] + i * dy;
                  if LatLong then DEMGLB[aNewMapDraw.DEMonMap].DEMGridToLatLongDegree( XGrids[i],YGrids[i], YGrids[i], XGrids[i]);
                  Dists[i] := i * dDistance;
               end;
            end
            else ComputeVersion2;
            *)
(*
            else if (StraightAlgorithm = saUTM) or ((StraightAlgorithm = saSmart) and (FullDistance < MDDef.wf.SmartSwitchOver) and (Lat1 > 0) and (Lat2 > 0))
                  or ((StraightAlgorithm = saDEMGrid) and (DEMGLB[aNewMapDraw.DEMonMap].DEMheader.DEMUsed = UTMBasedDEM)) then begin
               ComputeVersion1;
            end
            else if (StraightAlgorithm = saVincenty) or ( (StraightAlgorithm = saSmart) and ((FullDistance > MDDef.wf.SmartSwitchOver) or (Lat1 < 0) or (Lat2 < 0))) then begin
               ComputeVersion2;
            end;
         {$EndIf}
*)
      end;


      procedure NowGetVisiblePoints(ObsElev,W_TargetUp,MinViewPort,MaxViewPort : float64; ObserverTerrainHug,TargetTerrainHug : boolean; PointsPerRay : integer);
      var
         NewTanAngle,TanAngle,MaxTanAngle,zt,DeltaZ,TargetCushionUp : float64;
         veg_ht : float32;
         PointOnRay : integer;
         AboveVeg : boolean;
      begin
         {$IfDef RecordVisiblePoints} WriteLineToDebugFile('NowGetVisiblePoints, DEM=' + IntToStr(aNewMapDraw.DEMonMap) +
              ' point 0, x=' + RealToString(xgrids[0],-12,-2) + ' y=' + RealToString(ygrids[0],-12,-2) +
              ' last point 0, x=' + RealToString(xgrids[0],-12,-2) + ' y=' + RealToString(ygrids[0],-12,-2) );
         {$EndIf}
         MaxTanAngle := -999;
         DEMGLB[aNewMapDraw.DEMonMap].GetElevMeters(xgrids[0],ygrids[0],elevs[0]);
         VisPoints[0] := true;
         if ObserverTerrainHug then begin
            ObsElev := elevs[0] + ObsElev;
         end
         else begin
            if (ObsElev < elevs[0]) then begin  //observer below ground
               For PointOnRay := 0 to PointsPerRay do VisPoints[PointOnRay] := false;
               exit;
            end;
         end;
         MinViewPort := TanDeg(MinViewPort);
         MaxViewPort := TanDeg(MaxViewPort);
         TargetCushionUp := W_TargetUp + 0.0001;
         veg_ht := 0;
         For PointOnRay := 1 to PointsPerRay do begin
            VisPoints[PointOnRay] := false;
            if DEMGLB[aNewMapDraw.DEMonMap].GetElevMeters(xgrids[PointOnRay],ygrids[PointOnRay],elevs[PointOnRay]) then begin
               {$IfDef ExVegDensity}
               {$Else}
                  if (DEMGlb[aNewMapDraw.DEMonMap].VegGrid[1] > 0) and UseVeg then DEMGlb[DEMGlb[aNewMapDraw.DEMonMap].VegGrid[1]].GetJustVegHeight(xgrids[PointOnRay],ygrids[PointOnRay],veg_ht);
               {$EndIf}
               if TargetTerrainHug then begin
                  zt := elevs[PointOnRay] + W_TargetUp;
                  AboveVeg := (TargetCushionUp > Veg_ht);
               end
               else begin
                  zt := W_TargetUp;
                  AboveVeg := (TargetCushionUp > elevs[PointOnRay] + Veg_ht);
               end;
               DeltaZ := DropEarthCurve(Dists[PointOnRay]) + ObsElev;
               TanAngle :=  (Zt - DeltaZ) / Dists[PointOnRay];  {line to target}
               NewTanAngle := (elevs[PointOnRay]  + Veg_Ht - DeltaZ) / Dists[PointOnRay]; {line along ground which blocks the view for points farther away}
               VisPoints[PointOnRay] := AboveVeg and (TanAngle >= MaxTanAngle) and (TanAngle >= MinViewPort) and (TanAngle <= MaxViewPort);
            end
            else begin
               if MDDef.MissingDataBlocksLOS then begin
                  NewTanAngle := 9999;
               end;
            end;
            if (NewTanAngle > MaxTanAngle) and (Dists[PointOnRay] > MDDef.wf.ClosestBlockingDistance) then MaxTanAngle := NewTanAngle;
         end;
      end;


begin
   {$IfDef RecordFanRadials} if (RadialsSaved mod 25 = 0) or (RadialsSaved < 10) then WriteLineToDebugFile('Do radial in, alpha=' + RealToString(Alpha,-12,-2)); {$EndIf}

   VincentyPointAtDistanceBearing(WeaponsFan.W_Lat,WeaponsFan.W_Long,FanDistOut,Alpha,Lat2,Long2);
   RadialStraightRoute(false,WeaponsFan.W_Lat,WeaponsFan.W_Long,Lat2,Long2,MDDef.wf.StraightAlgorithm,PointsPerRay);
   if (not WeaponsFan.NoUseSensorNoTerrainBlock) then begin
      NowGetVisiblePoints(WeaponsFan.W_Up,WeaponsFan.W_TargetUp,WeaponsFan.DownAngle,WeaponsFan.UpAngle,WeaponsFan.ObserverTerrainHug,WeaponsFan.TargetTerrainHug,PointsPerRay);
   end;

   {$IfDef VCL}
      if MDDef.SaveFanRadiaProfiles then SaveRadial(Alpha);
   {$EndIf}

   {$If Defined(RecordFanSaveRadials) or Defined(RecordFanProblems)}
      inc(RadialsSaved);
   {$EndIf}

   RayVis := 0;
   RayMask := 0;
   For PointOnRay := 1 to PointsPerRay do begin
      aNewMapDraw.DEMGridtoScreen(xgrids[PointOnRay],ygrids[PointOnRay],X,Y);
      {$IfDef AllowFanCoverageDEM}
         if DisplayFanPointCoverage then begin
            xg := Round(xgrids^[PointOnRay]);
            yg := Round(ygrids^[PointOnRay]);
            if DEMGlb[FanCoverageDEM].GetElevMeters(xg,yg,z) then DEMGlb[FanCoverageDEM].SetGridElevation(xg,yg,z+1)
            else DEMGlb[FanCoverageDEM].SetGridElevation(xg,yg,1);
         end;
      {$EndIf}

      if aNewMapDraw.OnScreen(X,Y) and DistanceOK(Dists[PointOnRay]) then begin
         {$IfDef RecordFanInnerLoops} WriteLineToDebugFile('Pt=' + IntToStr(PointOnRay) + '  x=' + IntToStr(x) + '  & y=' + IntToStr(y)); {$EndIf}
         If (VisPoints[PointOnRay]) then begin
            BMPMemVis.SetPixelColor(x,y,WeaponsFan.ThisFanColor);
            inc(RayVis);
         end
         else begin
            if (BMPMemMask <> Nil) then BMPMemMask.SetPixelColor(x,y,MaskRGB);
            inc(RayMask);
         end;
      end
      else begin
          inc(OffMap);
      end;
   end {for PointOnRay};

   {$IfDef VCL}
      TInterlocked.Add(VisPts,RayVis);
      TInterlocked.Increment(RadialsDone);
      if (RadialsDone mod 100 = 0) and ShowSatProgress then begin
          UpdateProgressBar(RadialsDone/TotalRadials);
      end;
   {$EndIf}

   {$IfDef RecordFanRadialsInMetadata} if ComputeFanMetadata and (RayVis + RayMask > 0) then FanMetadata.Add('Alpha=' + RealToString(Alpha,-18,3) + '  Vis: ' + RealToString(100 * RayVis/(RayVis+RayMask),-8,2)); {$EndIf}

   {$IfDef RecordFanRadials}
      if (RayVis + RayMask > 0) then
         WriteLineToDebugFile('Alpha=' + RealToString(Alpha,-18,3) + '  Vis: ' + RealToString(100 * RayVis/(RayVis+RayMask),-8,2) + '%' + '   off map: ' + IntToStr(offMap))
      else begin
         WriteLineToDebugFile('Alpha=' + RealToString(Alpha,-18,3) + '  Nothing on ray');
      end;
   {$EndIf}
end;


procedure TDrawFan.DoPieSlice(FirstAngle : float64; NumRays : integer);
var
   Ray : integer;
begin
   for Ray := 1 to NumRays do begin
      DoRadial(FirstAngle);
      FirstAngle := FirstAngle + MDDef.wf.MaskRaySpacingDeg;
   end {for Ray};
end;


constructor TDrawFan.Create;
begin
   {$If Defined(RecordFanSaveRadials) or Defined(RecordFanProblems)} RadialsSaved := 0; {$EndIf}

  BaseMapDraw := inMap;
  WeaponsFan := inWeaponsFan;
  useVeg := inUseVeg;
  VisName := inVisName;
  MaskName := inMaskName;
  MergeName := inMergeName;
  CoverName := inCoverName;
  ComputeFanMetadata := true;
  {$IfDef RecordFanProblems} WriteLineToDebugFile('TDrawFan.Create out'); {$EndIf}
end;


procedure tDrawFan.DoSideOfBox(Side : integer);
var
   i : integer;
begin
   {$IfDef RecordFanProblems} WriteLineToDebugFile('tDrawFan.DoSideOfBox in, side=' + IntToStr(Side)); {$EndIf}
   i := 0;
   if (Side = 1) then begin
       while i <= pred(VisBitmap.Width) do begin
          RadialToEdge(i,0);
          inc(i,MDDef.RadialSkip);
       end;
   end
   else if (Side = 2) then begin
        while i <= pred(VisBitmap.Width) do begin
          RadialToEdge(i,pred(VisBitmap.Height));
          inc(i,MDDef.RadialSkip);
       end;
   end
   else if (Side = 3) then begin
       while i <= (VisBitmap.Height-2) do begin
          RadialToEdge(0,i);
          inc(i,MDDef.RadialSkip);
       end;
   end
   else if (Side = 4) then begin
       while i <= (VisBitmap.Height-2) do begin
          RadialToEdge(pred(VisBitmap.Width),i);
          inc(i,MDDef.RadialSkip);
       end;
   end;
   {$IfDef RecordFanProblems} WriteLineToDebugFile('tDrawFan.DoSideOfBox out'); {$EndIf}
end;


procedure tDrawFan.ExecuteFan;
var
   x,y, FanX,FanY,FanSize,
   BMPVis,BMPMasked,BMPMixed,
   XStart,YStart,XEnd,YEnd,
   PointsPerRay2 : integer;
   HorizDist,Distance,
   LeftLimit,RightLimit,
   Alpha,Beta,Lat2,Long2,xu,yu,Heading,BlockDist : float64;
   Inbounds : boolean;
   FanGX,FanGY,XGrid,YGrid,ObsElev,z,FanZ : float32;
    {$IfDef NoParallelFor}
    {$Else}
       UseThreads : integer;
       Angle1 : float32;
    {$EndIf}


      {$IfDef VCL}
      procedure CheckRadialSaving;
      begin
         if MDDef.SaveFanRadiaProfiles then begin
            {$IfDef RecordFanSaveRadials} WriteLineToDebugFile('CheckRadialSaving'); {$EndIf}
            SavedRadialPath := ProjectDir + 'fans\';
            SafeMakeDir(SavedRadialPath);
            Petmar.DeleteMultipleFiles(SavedRadialPath,'Saved_Radials-*.PRO');
         end;
      end;
      {$EndIf}

      procedure CheckShot(Col,Row : integer);
      begin
         DEMGlb[aNewMapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat2,Long2);
         VincentyCalculateDistanceBearing(WeaponsFan.W_Lat,WeaponsFan.W_Long,Lat2,Long2,HorizDist,Heading);
         if (HorizDist > FanDistOut) then FanDistOut := HorizDist;
         if (Heading < Alpha) then Alpha := Heading;
         if (Heading > Beta) then Beta := Heading;
      end;


      procedure FanOverhead;
      begin
         LeftLimit := WeaponsFan.StartAngle;
         RightLimit := WeaponsFan.EndAngle;
         if (LeftLimit < 0) then LeftLimit := LeftLimit + 360;
         if (RightLimit > 360) then RightLimit := RightLimit - 360;
         DEMGlb[aNewMapDraw.DEMonMap].GetElevFromLatLongDegree(WeaponsFan.W_Lat,WeaponsFan.W_Long,FanZ);
         if WeaponsFan.ObserverTerrainHug then FanZ := Fanz + WeaponsFan.W_TargetUp
         else FanZ := WeaponsFan.W_TargetUp;
         if ComputeFanMetadata then begin
            FanMetadata := tStringList.Create;
            FanMetadata.Add('Fan at : ' + LatLongDegreeToString(WeaponsFan.W_Lat,WeaponsFan.W_Long));
            FanMetadata.Add('Created ' + {$IfDef VCL} 'by: ' + EXENameWithBuild + '  ' + {$EndIf} DateToStr(Now) + ' ' + TimeToStr(Now));
            FanMetadata.Add('DEM: ' + DEMGlb[BaseMapDraw.DEMonMap].AreaName);
            FanMetadata.Add(DEMGlb[BaseMapDraw.DEMonMap].KeyDEMParams(true));
            FanMetadata.Add('Sensor height: ' + RealToString(WeaponsFan.W_Up,-12,2));
            FanMetadata.Add('Target height: ' + RealToString(WeaponsFan.W_TargetUp,-12,2));
            FanMetadata.Add('Vertical earth curvature: ' + CurvAlgName(MDdef.CurvAlg));
            FanMetadata.Add('Viewshed size (pixels): ' + IntToStr(VisBitmap.Width) + 'x' + IntToStr(VisBitmap.Height));
            FanMetadata.Add('Viewshed pixel size (m): ' + RealToString(aNewMapDraw.ScreenPixelSize,-18,-2));
            {$IfDef RecordFanMetadata} WriteStringListToDebugFile(FanMetaData); {$EndIf}
         end;
      end;


      {$IfDef VCL}
         procedure ComputeFullIntervisibity;
         var
            x,y : integer;
         begin
            aNewMapDraw.LatLongDegreetoScreen(WeaponsFan.W_Lat,WeaponsFan.W_Long,FanX,FanY);
            FanSize := round(WeaponsFan.W_Range / aNewMapDraw.ScreenPixelSize);
            XStart := FanX - FanSize;
            XEnd := FanX + FanSize;
            YStart := FanY - FanSize;
            YEnd := FanY + FanSize;
            DEMGlb[aNewMapDraw.DEMonMap].ClipDEMGrid(XStart,Ystart);
            DEMGlb[aNewMapDraw.DEMonMap].ClipDEMGrid(XEnd,YEnd);

            {$IfDef RecordFanProblems}
               WriteLineToDebugFile(' Angle: ' + IntToStr(round(LeftLimit)) + '--' + IntToStr(round(RightLimit)) + '  Xs: ' + IntToStr(xstart) + '--' + IntToStr(xend) +
                  '  Ys: ' + IntToStr(ystart) + '--' + IntToStr(yend));
            {$EndIf}

            {for each point on the screen, compute intervisibility}
            for y := YStart to YEnd do begin
               for x := XStart to XEnd do begin
                  aNewMapDraw.ScreenToUTM(x,y,xu,yu);
                  DEMGlb[aNewMapDraw.DEMonMap].UTMtoDEMGrid(xu,yu,xgrid,ygrid,Inbounds);
                  if MDDef.wf.FanTargetMustBeGrid then begin
                     xgrid := Round(Xgrid);
                     ygrid := Round(Ygrid);
                  end;
                  if Inbounds and (DEMGlb[aNewMapDraw.DEMonMap].DistanceMetersBetweenPoints(FanGX,FanGY,xgrid,ygrid,Heading) <= WeaponsFan.W_Range) then begin
                     Heading := HeadingOfLine(xgrid-Fangx,ygrid-Fangy);
                     if ((Heading >= LeftLimit) and (Heading <= RightLimit)) or ((LeftLimit > RightLimit) and ((Heading <= RightLimit)) or
                        (LeftLimit > RightLimit) and (Heading >= LeftLimit)) then begin
                           if DEMGlb[aNewMapDraw.DEMonMap].GetElevMeters(xgrid,ygrid,z) then begin
                              if WeaponsFan.noUseSensorNoTerrainBlock or (DEMGlb[aNewMapDraw.DEMonMap].GridPointsIntervisible(FanGX,FanGY,WeaponsFan.W_TargetUp,xgrid,ygrid,
                                         WeaponsFan.W_TargetUp,Distance,BlockDist) and DistanceOK(Distance)) then begin
                                 Inc(VisPts);
                                 BMPMemVis.SetPixelColor(x,y, WeaponsFan.ThisFanColor);
                              end
                              else begin
                                 Inc(MaskedPts);
                                 BMPMemMask.SetPixelColor(x,y, MaskRGB);
                              end;
                           end;
                     end;
                  end;
               end;
            end;
         end;
      {$EndIf}


      procedure UnthreadsSidesLoop;
       var
          side : integer;
       begin
          {$IfDef RecordFanProblems} WriteLineToDebugFile('UnthreadsSidesLoop in'); {$EndIf}
          for Side := 1 to 4 do begin
             {$IfDef RecordFanProblems} WriteLineToDebugFile('Serial side=' + IntToStr(Side)); {$EndIf}
             DoSideOfBox(Side);
          end;
       end;


begin
    try
       {$If Defined(RecordFanProblems) or Defined(RecordTime)}
          WriteLineToDebugFile('tFanThread.Execute in');
          Stopwatch1 := TStopwatch.StartNew;
       {$EndIf}
       aNewMapDraw := Nil;
       VisPts    := 0;
       MaskedPts := 0;

       if MDDef.wf.FanViewerMustBeGrid then begin
          DEMGlb[BaseMapDraw.DEMonMap].LatLongDegreetoDEMGrid(WeaponsFan.W_Lat,WeaponsFan.W_Long,FanGX,FanGY);
          FanGX := Round(FanGX);
          FanGY := Round(FanGY);
          DEMGlb[BaseMapDraw.DEMonMap].DEMGridToLatLongDegree(FanGX,FanGY,WeaponsFan.W_Lat,WeaponsFan.W_Long);
       end;
       aNewMapDraw := BaseMapDraw.BlowUpAtPoint(WeaponsFan.W_Lat,WeaponsFan.W_Long,WeaponsFan.W_Range,WeaponsFan.FanZoomFactor);

       MaskRGB := WeaponsFan.ThisMaskColor;
       MixedRGB := MDDef.ViewshedMixedPixelColor;

       CreateBitmap(VisBitmap, aNewMapDraw.MapXSize,aNewMapDraw.MapYSize);
       CreateBitmap(MaskedBitmap, aNewMapDraw.MapXSize,aNewMapDraw.MapYSize);

       BMPMemVis := tBMPMemory.Create(VisBitmap);
       BMPMemMask := tBMPMemory.Create(MaskedBitmap);

       {$IfDef RecordFanProblems} WriteLineToDebugFile('Bitmap size: ' + IntToStr(VisBitmap.Height) + 'x' + IntToStr(VisBitmap.Width)); {$EndIf}

       if DEMGlb[BaseMapDraw.DEMonMap].LatLongDegreeInDEM(WeaponsFan.W_Lat,WeaponsFan.W_Long) then begin
          FanOverhead;

          if (MDDef.wf.FanMethod = fmAllPointToPoint) then begin
             {$IfDef VCL}
                {$IfDef RecordFanProblems} WriteLineToDebugFile('(MDDef.wf.FanMethod = fmAllPointToPoint)'); {$EndIf}
                ComputeFullIntervisibity;
             {$EndIf}
          end
          else with aNewMapDraw do begin  //radial method
             {$IfDef RecordFanProblems} WriteLineToDebugFile('(MDDef.wf.FanMethod radial)'); {$EndIf}
             {$IfDef AllowFanCoverageDEM}
                if MDDef.DisplayFanPointCoverage then begin
                   FanCoverageDEM := 0;
                   OpenAndZeroNewDEM(true,DEMGlb[DEMonMap].HeadRecs,FanCoverageDEM);
                   DEMGlb[FanCoverageDEM].AreaName := 'Calculations for each point in DEM';
                end;
             {$EndIf}

             if (MDDef.wf.FanMethod = fmRadialIHS) then begin
                MDDef.wf.MaskAreaInterval := MDDef.wf.FanDEMSpaceMultiple * (DEMGlb[aNewMapDraw.DEMonMap].AverageSpace);
                z := MDDef.wf.FanMapSpaceMultiple * ScreenPixelSize;
                if (z < MDDef.wf.MaskAreaInterval) then MDDef.wf.MaskAreaInterval := z;
                MDDef.wf.MaskRaySpacingDeg := 2 * (45 - ArcTan((WeaponsFan.w_Range - ScreenPixelSize) / WeaponsFan.w_Range) / DegToRad);
             end;

             with WeaponsFan do begin
                Alpha := StartAngle;
                Beta  := EndAngle;
                FanDistOut := w_range;
             end;

             if (Alpha > Beta) then Beta := Beta + 360;
             DEMGlb[DEMonMap].GetElevFromLatLongDegree(WeaponsFan.W_Lat,WeaponsFan.W_Long,ObsElev);

             {$IfDef RecordFanRadials} WriteLineToDebugFile('DEM grid spacing: ' + RealToString(DEMGlb[DEMonMap].AverageSpace,-12,-2) + '  Map pixel size: ' + RealToString(aNewMapDraw.ScreenPixelSize,-12,-2)); {$EndIf}
             if (MDDef.wf.LOSAlgorithm = losMicrodemFractional) then begin
                PointsPerRay := Round(WeaponsFan.W_range / (MDDef.wf.FanDEMSpaceMultiple * DEMGlb[DEMonMap].AverageSpace));
                PointsPerRay2 := Round(WeaponsFan.W_range / (MDDef.wf.FanMapSpaceMultiple * aNewMapDraw.ScreenPixelSize));
                {$IfDef RecordFanRadials} WriteLineToDebugFile('MDDef.wf.LOSAlgorithm = losMicrodemFractional, PointsPerRay=' + IntToStr(PointsPerRay) + ' PointsPerRay2=' + IntToStr(PointsPerRay2)); {$EndIf}
             end;

             if (MDDef.wf.LOSAlgorithm = losMicrodemConstant) then begin
                PointsPerRay := Round(WeaponsFan.W_range / (MDDef.wf.MaskAreaInterval));
                PointsPerRay2 := PointsPerRay;
                {$IfDef RecordFanRadials} WriteLineToDebugFile('MDDef.wf.LOSAlgorithm =losMicrodemConstant, PointsPerRay=' + IntToStr(PointsPerRay) + ' PointsPerRay2=' + IntToStr(PointsPerRay2)); {$EndIf}
             end;

             if (PointsPerRay2 > PointsPerRay) then PointsPerRay := PointsPerRay2;
             if PointsPerRay > MaxOnRay then PointsPerRay := MaxOnRay;
             {$IfDef RecordFanRadials} WriteLineToDebugFile('Final PointsPerRay=' + IntToStr(PointsPerRay)); {$EndIf}

             if (MDDef.wf.FanMethod = fmRadialIHS) then begin
                NumRays := 2 * pred(VisBitmap.Height) + 2 * pred(VisBitmap.Width);
                MDDef.wf.MaskRaySpacingDeg := (Beta-Alpha) / NumRays;
             end
             else begin
                NumRays := succ(round( (Beta-Alpha) / MDDef.wf.MaskRaySpacingDeg));
                MDDef.wf.MaskRaySpacingDeg := (Beta-Alpha) / pred(NumRays);
             end;
             {$IfDef RecordFanRadials} WriteLineToDebugFile('Final NumRays=' + IntToStr(NumRays)); {$EndIf}

             {$IfDef VCL}
                if MDDef.SaveFanRadiaProfiles then CheckRadialSaving;
             {$EndIf}

             if ComputeFanMetadata then begin
                FanMetadata.Add('Current map zoom: ' + IntToStr(WeaponsFan.FanZoomFactor));
                FanMetadata.Add('Radials computed: ' + IntToStr(NumRays));
                FanMetadata.Add('Radial spacing: ' + RealToString(MDDef.wf.MaskRaySpacingDeg, -18,2) + DegSym);
                FanMetadata.Add('Points per radial: ' + IntToStr(PointsPerRay));
                FanMetadata.Add('Spacing along radial: ' + RealToString(WeaponsFan.w_range / PointsPerRay, -18,2) + ' m');
                FanMetadata.Add('Sensor range: '+ RealToString(WeaponsFan.w_range,-18,0) + ' m');
                FanMetadata.Add('Closest blocking distance: '+ RealToString(MDDef.wf.ClosestBlockingDistance,-18,0) + ' m');
                FanMetadata.Add('Min sensor range: ' + RealToString(MDDef.DefWeaponsMinRange,-18,0) + ' m');
                {$IfDef RecordFanMetadata}
                   WriteLineToDebugFile('Fan metadata',true);
                   WriteStringListToDebugFile(FanMetaData);
                {$EndIf}
             end;

             RadialsDone := 0;
             if (MDDef.wf.FanMethod = fmRadialIHS) then begin
                TotalRadials := 2 * pred(VisBitmap.Width) + 2 * pred(VisBitmap.Height);
                {$IfDef RecordFanProblems} WriteLineToDebugFile('MDDef.wf.FanMethod = fmRadialIHS, TotalRadials=' + IntToStr(TotalRadials)); {$EndIf}
                {$IfDef NoParallelFor}
                    {$IfDef RecordFanProblems} WriteLineToDebugFile('threads disabled'); {$EndIf}
                    UnthreadsSidesLoop;
                    {$IfDef RecordFanProblems} WriteLineToDebugFile('Fan drawn'); {$EndIf}
                {$Else}
                   {$IfDef RecordFanProblems} WriteLineToDebugFile('Parallel fan start, MaxThreads=' + IntToStr(MDdef.MaxThreadsForPC) +  '   MDDef.SaveFanRadials=' + TrueOrFalse(MDDef.SaveFanRadiaProfiles) );{$EndIf}
                   if (MDdef.MaxThreadsForPC = 1) or MDDef.SaveFanRadiaProfiles then begin
                      {$IfDef RecordFanProblems} WriteLineToDebugFile('Call UnthreadsSidesLoop, because (MDdef.MaxActiveThreads = 1) or MDDef.SaveFanRadials'); {$EndIf}
                      UnthreadsSidesLoop;
                   end
                   else begin
                      {$IfDef RecordFanProblems} WriteLineToDebugFile('Start threads=4 (one per box side)'); {$EndIf}
                      ThreadsWorking := true;
                      TParallel.For(1,4,
                          procedure (Value: Integer)
                          begin
                            DoSideOfBox(Value);
                          end);
                      ThreadsWorking := false;
                   end;
                   {$IfDef RecordFanProblems} WriteLineToDebugFile('Parallel fan done'); {$EndIf}
                {$EndIf}
             end
             else begin
                TotalRadials := NumRays;
                {$IfDef NoParallelFor}
                   DoPieSlice(Alpha,NumRays);
                {$Else}
                   {$IfDef RecordFanProblems} WriteLineToDebugFile('Parallel fan start'); {$EndIf}
                   if (MDdef.MaxThreadsForPC = 1) or MDDef.SaveFanRadiaProfiles then begin
                      UseThreads := 1;
                   end
                   else begin
                      ThreadsWorking := true;
                      UseThreads := MDdef.MaxThreadsForPC;
                   end;

                   TParallel.For(1,UseThreads,
                       procedure (Value: Integer)
                       begin
                         NumRays := NumRays div UseThreads;
                         Angle1 := Alpha + pred(Value) * NumRays;
                         if Value = MDdef.MaxThreadsForPC then begin
                            NumRays := TotalRadials - pred(UseThreads * NumRays);
                         end;
                         DoPieSlice(Angle1,NumRays);
                       end);
                   ThreadsWorking := false;
                   {$IfDef RecordFanProblems} WriteLineToDebugFile('Parallel fan done'); {$EndIf}
                {$EndIf}
             end;
           end;

           {$IfDef VCL}
              if ComputeFanMetadata then begin
                FanMetadata.Add('');
                FanMetadata.Add('Total points computed: ' + IntToStr(VisPts + MaskedPts));
                FanMetadata.Add('Visible points computed: ' + IntToStr(VisPts) + RealToString(100.0*VisPts /(VisPts + MaskedPts),8,2) + '%');
                FanMetadata.Add('Masked points computed: ' + IntToStr(MaskedPts) + RealToString(100.0*MaskedPts /(VisPts + MaskedPts),9,2) + '%');
                BMPVis := 0;
                BMPMasked := 0;
                BMPMixed := 0;

                for x := 0 to pred(VisBitmap.Width) do begin
                   for y := 0 to pred(VisBitmap.Height) do begin
                      if BmpMemMask.SameColor(x,y,MaskRGB) then begin
                         if BmpMemVis.SameColor(x,y,WeaponsFan.ThisFanColor) then inc(BMPMixed)
                         else inc(BMPMasked);
                      end
                      else if BmpMemVis.SameColor(x,y,WeaponsFan.ThisFanColor) then inc(BMPVis);
                   end;
                end;

                PercentCoverage := 100.0*BMPVis /(BMPVis+BMPMasked+BMPMixed);
                FanMetadata.Add('');
                FanMetadata.Add('Total points on bitmap: ' + IntToStr(BMPVis+BMPMasked+BMPMixed));
                FanMetadata.Add('Visible points on bitmap: ' + IntToStr(BMPVis) + RealToString(PercentCoverage,8,2) + '%');
                FanMetadata.Add('Masked points on bitmap: ' + IntToStr(BMPMasked) + RealToString(100.0*BMPMasked /(BMPVis+BMPMasked+BMPMixed),8,2) + '%');
                FanMetadata.Add('Mixed points on bitmap: ' + IntToStr(BMPMixed) + RealToString(100.0*BMPMixed /(BMPVis+BMPMasked+BMPMixed),9,2) + '%');
             end;
             if MDDef.ShowViewshedMixedPixels then begin
                for x := 0 to pred(VisBitmap.Width) do begin
                   for y := 0 to pred(VisBitmap.Height) do begin
                      if (not BmpMemMask.SameColor(x,y,RGBTripleWhite)) and (not BmpMemVis.SameColor(x,y,RGBTripleWhite)) then begin
                         BmpMemMask.SetPixelColor(x,y,MixedRGB);
                         BmpMemVis.SetPixelColor(x,y,MixedRGB);
                      end;
                   end;
                end;
             end;
          {$EndIf}
       end;

       BMPMemVis.Destroy;
       BMPMemMask.Destroy;

       SaveFanBitmaps;

       {$IfDef Android}
          //bad crash if try to aNewMapDraw.Destroy;
       {$Else}
          if (aNewMapDraw <> Nil) then aNewMapDraw.Destroy;
       {$EndIf}

       if (VisBitmap <> Nil) then FreeAndNil(VisBitmap);
       if (MaskedBitmap <> Nil) then FreeAndNil(MaskedBitmap);

       {$If Defined(RecordFanProblems) or Defined(RecordTime)}
          Elapsed := Stopwatch1.Elapsed;
          WriteLineToDebugFile('Fan execute out ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec');
       {$EndIf}
    except   //eat all exceptions
    end;
end;



initialization
   {$IfDef MessageStartUpUnitProblems} MessageToContinue('Startup weapons_fan_thread');  {$EndIf}
finalization
    {$IfDef RecordThreadProgress} WriteLineToDebugFile('RecordThreadProgress active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordMainThreadProgress} WriteLineToDebugFile('RecordMainThreadProgress active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordFanProblems} WriteLineToDebugFile('RecordFanProblems active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordThreadTiming} WriteLineToDebugFile('RecordThreadTiming active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordFanInnerLoops} WriteLineToDebugFile('RecordFanInnerLoops active, weapons_fan_thread (severe slowdown)'); {$EndIf}
    {$IfDef RecordFanRadials} WriteLineToDebugFile('RecordFanRadials active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordFanRadialsInMetadata} WriteLineToDebugFile('RecordFanRadialsInMetadata active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordFanMetadata} WriteLineToDebugFile('RecordFanMetadata active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordFanSaveBitmap} WriteLineToDebugFile('RecordFanSaveBitmap active, weapons_fan_thread'); {$EndIf}
    {$IfDef RecordFanSaveRadials} WriteLineToDebugFile('RecordFanSaveRadials active, weapons_fan_thread'); {$EndIf}
end.



