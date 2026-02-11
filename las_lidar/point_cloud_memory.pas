unit point_cloud_memory;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}




{$I nevadia_defines.inc}

{$Define NoInLine}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordPointCloudmemory}
      //{$Define RecordPointCloudLimits}
      //{$Define RecordMemoryAllocations}
   {$EndIf}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end inline for core DB functions

   {$IfDef VCL}
      Graphics,Forms,Controls,
      DEMMapf,BaseGraf,
   {$EndIf}

   SysUtils,Classes,Windows,StrUtils,
   Petmar_types,PetImage,PetMath,
   DEMDefs,BaseMap,DEMMapDraw;

const
   MaxXDensity = 5000;
   MaxYDensity = 50;
   MaxZDensity = 50;
   DistCoord = 1;
   AzimCoord = 2;
   ElevCoord = 3;
type
   tHugeByteArray = packed array[1..TheMaxPointsMemPtCloud] of byte;
   tHugeBooleanArray = packed array[1..TheMaxPointsMemPtCloud] of boolean;
   tHugeColorArray = packed array[1..TheMaxPointsMemPtCloud] of TRGBTriple;
   tHugePointArray1d = packed array[1..TheMaxPointsMemPtCloud] of float64;
   tHugePointArray2d = packed array[1..TheMaxPointsMemPtCloud] of array[1..2] of float64;
   tHugePointArray3d = packed array[1..64000000] of array[1..3] of float64;
   tAlongProfileDensity = packed array[0..MaxXDensity,0..MaxZDensity] of byte;
   tAlongProfileGround = packed array[0..MaxXDensity] of float64;

   tAcrossProfileDensity = packed array[-MaxYDensity..MaxYDensity,0..MaxZDensity] of byte;
   tAcrossProfileGround = packed array[-MaxYDensity..MaxYDensity] of float64;

   tMemoryPointCloud = class
      private
         {$IfDef RecordPointCloudLimits}
            procedure ShowXYLimits(Where : ShortString);
         {$EndIf}
         procedure ZeroXLimits;
         procedure ZeroYLimits;
         procedure ZeroZLimits;
         procedure GetPointUTMCoords(i : integer; var xutm,yutm : float64); {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function PlotClassification(i : integer; var RGBColor : TRGBTriple) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
      public
         AllocatedPoints,NumMemPts : integer;
         StayLatLong : boolean;
         pcMinX,pcMaxX,pcMinY,pcMaxY,pcMinZ,pcMaxZ : float64;
         PolarCoords : ^tHugePointArray3D;
         xyPts  : ^tHugePointArray2D;
         zPts  : ^tHugePointArray1D;
         PtRGB : ^tHugeColorArray;
         PtClass : ^tHugeByteArray;
         PtReturnNumber : ^tHugeByteArray;
         LASsourceName,CloudName : PathStr;
         AlongProfileDensity : ^tAlongProfileDensity;
         AlongProfileGround : tAlongProfileGround;
         AcrossProfileDensity : ^tAcrossProfileDensity;
         AcrossProfileGround : tAcrossProfileGround;
         MapOwner : tMapForm;

         constructor Create(fName : PathStr; inMapOwner : tMapForm; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z'; inCloudName : shortstring = ''; inStayLatLong : boolean = false);
         destructor Destroy;
         procedure LoadFromShapeFile(fName : PathStr; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z');
         procedure LoadFromASCIIFile(fName : PathStr);
         procedure LoadFromLASFile(BaseMap : tMapForm; fName : PathStr);
         procedure LoadFromLVISFile(fName : PathStr);
         procedure SetPolarCoords(xc,yc,zc : float64);
         procedure ExtractPointsNearHere(Lat,Long,FullBoxSizeMeters : float64);
         procedure ExportCSV(fName : PathStr);
         function AddPointToMemory(x,y,z : float64) : boolean;  {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure PlotSlice(Cloud : integer; var BMPMemory : tBMPMemory; TheGraph : tThisBaseGraph; c1,c2,c3 : integer; LowLimit,HighLimit : float64;  var NPts : integer; ThreadTimer : integer = 0);
         procedure TranslateAndRotate(dx,dy,Angle : float64);
         procedure ExtractMinMaxPoints(Lat, Long : float64; var MinZ,MaxZ: float64; UseOnlyGroundClass : boolean);
         procedure ExtractMinMaxPointsFromDistance(Dist : float64; var MinZ,MaxZ: float64; UseOnlyGroundClass : boolean);
         procedure GetNumPointsNearLocation(Dist,Height : float64; var NumPts : integer);
         procedure GetNumPointsAboveLocation(Dist, Height: float64; var NumPts: integer);
         procedure GetNumPointsNearAndAboveLocation(Dist, Height: float64; var NumPtsNear,NumPtsAbove: integer);
         procedure RotateXY(Angle: float64);
         procedure RotateXZ(Angle: float64);
         procedure RotateYZ(Angle: float64);
         procedure GetRegionStats(Lat,Long : float64);
         procedure GetAlongProfileDensity;
         procedure GetAcrossProfileDensity(xz : float64);
         procedure HowHighUp;
   end;

type
   tTwoPointClouds = array[1..2] of Point_cloud_memory.tMemoryPointCloud;

implementation


uses
   Petmar,
   DEMESRIShapeFile,
   DEMcoord,
   DEMDataBase,
   DEM_Manager,
   DEM_indexes,
   Las_lidar,
   LVIS,
   DEMStat,
   PetDBUtils,
   Nevadia_main,
   Thread_timers;


procedure tMemoryPointCloud.SetPolarCoords(xc,yc,zc : float64);
var
   i : integer;
   dx,dy,dz : float64;
begin
   if (PolarCoords = nil) then GetMem(PolarCoords,AllocatedPoints * 3 * sizeOf(float64));
   StartProgress('Coordinates');
   for i := 1 to NumMemPts do begin
      if (i mod 1000 = 0) then UpdateProgressBar(i/NumMemPts);
      dx := xyPts^[i,1] - xc;
      dy := xyPts^[i,2] - yc;
      dz := zPts^[i] - zc;
      PolarCoords^[i,DistCoord] := sqrt(sqr(dx) + sqr(dy));
      PolarCoords^[i,AzimCoord] := HeadingOfLine(dx,dy);
	   PolarCoords^[i,ElevCoord] := arcTanDeg(dz/PolarCoords^[i,DistCoord]);
   end;
   EndProgress;
end;


{ tMemoryPointCloud }

constructor tMemoryPointCloud.Create(fName : PathStr; inMapOwner : tMapForm; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z'; inCloudName : shortstring = ''; inStayLatLong : boolean = false);
var
   Ext : ExtStr;
   Name : NameStr;
   TStr : AnsiString;
begin
   {$IfDef RecordPointCloudMemory} WriteLinetoDebugFile('tMemoryPointCloud.Create in ' + fName); {$EndIf}
   ShowHourglassCursor;
   MapOwner := inMapOwner;
   NumMemPts := 0;
   LASsourceName := fName;
   xyPts := Nil;
   PtReturnNumber := Nil;
   AlongProfileDensity := nil;
   AcrossProfileDensity := nil;
   PtClass := Nil;
   PtRGB := Nil;
   PolarCoords := nil;
   StayLatLong := inStayLatLong;
   ZeroXLimits;
   ZeroYLimits;
   ZeroZLimits;
   CloudName := inCloudName;
   if inCloudName = '' then begin
      CloudName := ExtractFileNameNoExt(fname);
      if StrUtils.AnsiContainsText(CloudName,'subset') then begin
         TStr := CloudName;
         CloudName := Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'_');
      end;
   end;

   Ext := ExtractFileExt(fName);
   Name := UpperCase(ExtractFileName(fName));

   if ExtEquals(Ext, '.LAS') then begin
      {$IfDef RecordPointCloudMemory} WriteLineToDebugFile('tMemoryPointCloud.Create LAS'); {$EndIf}
      LoadFromLASFile(MapOwner,fName);
   end
   else begin
      if ExtEquals(Ext, '.shp') or ExtEquals(Ext, DefaultDBExt) then begin
         {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('shapefile'); {$EndIf}
         LoadFromShapeFile(fName,XField,YField,ZField);
      end
      else if ExtEquals(Ext, '.txt') or ExtEquals(Ext, '.csv') then begin
         {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('text/csv'); {$EndIf}
         LoadFromASCIIFile(fName);
      end
      else begin
         AllocatedPoints := MDDef.MemoryPointCloudMaxPts;
         {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('allocate tMemoryPointCloud.Create, not LAS, pointer size=' + IntToStr(AllocatedPoints * 3 * sizeOf(float64))); {$EndIf}
         GetMem(xyPts,AllocatedPoints * 2 * sizeOf(float64));
         GetMem(zPts,AllocatedPoints * 1 * sizeOf(float64));
         {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('Load LVIS'); {$EndIf}
         LoadFromLVISFile(fName);
      end;
   end;
end;


destructor tMemoryPointCloud.Destroy;
begin
   CloseCompareDEMs;
   FreeMem(XYPts,AllocatedPoints * 2 * SizeOf(float64));
   FreeMem(ZPts,AllocatedPoints * 1 * SizeOf(float64));
   if (PolarCoords <> Nil) then FreeMem(PolarCoords,AllocatedPoints * 3 * SizeOf(float64));
   if (PtClass <> Nil) then FreeMem(PtClass,AllocatedPoints);
   if (PtReturnNumber <> Nil) then FreeMem(PtReturnNumber,AllocatedPoints);
   if (AlongProfileDensity <> nil) then Dispose(AlongProfileDensity);
   if (AcrossProfileDensity <> nil) then Dispose(AcrossProfileDensity);
end;


procedure tMemoryPointCloud.ExportCSV(fName : PathStr);
var
   i,Thin: integer;
   OutputF : tStringList;
   x,y,z : float64;
   TStr : shortstring;
begin
   OutputF := tStringList.Create;
   if (PtRGB <> Nil) then OutputF.Add('x,y,z,r,g,b')
   else OutputF.Add('x,y,z');
   i := 1;
   Thin := 1;
   while (I <= NumMemPts) do begin
      x := xypts^[i,1];
      y := xypts^[i,2];
      z := zpts^[i];
      if (PtRGB <> Nil) then TStr := ',' + IntToStr(PtRGB^[i].rgbtRed) + ',' + IntToStr(PtRGB^[i].rgbtGreen) + ',' + IntToStr(PtRGB^[i].rgbtBlue)
      else TStr := '';
      OutPutF.Add(RealToString(x,-18,-8) + ',' + RealToString(y,-18,-8) + ',' + RealToString(z,-18,-8) + Tstr );
      inc(i,Thin);
   end;
   if (fName = '') then fName := MDTempDir + 'pt_cloud_results.csv';
   OutputF.SaveToFile(fName);
   OutPutF.Free;
end;


procedure SetXYBox(Dist : float64; var xlo,xhi,ylo,yhi : float64); inline;
begin
   xlo := Dist - 0.5 * MDDef.CloudSliceThick;
   xhi := Dist + 0.5 * MDDef.CloudSliceThick;
   ylo := -0.5 * MDDef.CloudSliceThick;
   yhi := 0.5 * MDDef.CloudSliceThick;
end;


procedure tMemoryPointCloud.GetNumPointsNearLocation(Dist, Height: float64; var NumPts: integer);
var
   xlo,xhi,ylo,yhi,zhi,zlo : float64;
   i : integer;
begin
   NumPts := 0;
   if (MapOwner <> Nil) then begin
      SetXYBox(Dist,xlo,xhi,ylo,yhi);
      zlo := Height - 0.5 * MDDef.CloudSliceThick;
      zhi := Height + 0.5 * MDDef.CloudSliceThick;
      for i := 1 to NumMemPts do begin
         if (xyPts^[i,2] >= xlo) and (xyPts^[i,2] <= xhi) and (xyPts^[i,1] >= ylo) and (xyPts^[i,1] <= yhi) and (zPts^[i] >= zlo) and (zPts^[i] <= zhi) then begin
            inc(NumPts);
         end;
      end;
   end;
end;


procedure tMemoryPointCloud.GetNumPointsAboveLocation(Dist, Height: float64; var NumPts: integer);
var
   xlo,xhi,ylo,yhi : float64;
   i : integer;
begin
   NumPts := 0;
   if (MapOwner <> Nil) then begin
      SetXYBox(Dist,xlo,xhi,ylo,yhi);
      for i := 1 to NumMemPts do begin
         if (xyPts^[i,2] >= xlo) and (xyPts^[i,2] <= xhi) and (xyPts^[i,1] >= ylo) and (xyPts^[i,1] <= yhi) and (zPts^[i] >= Height) then begin
            inc(NumPts);
         end;
      end;
   end;
end;

procedure tMemoryPointCloud.GetNumPointsNearAndAboveLocation(Dist, Height: float64; var NumPtsNear,NumPtsAbove: integer);
var
   xlo,xhi,ylo,yhi,zhi,zlo : float64;
   i : integer;
begin
   NumPtsNear := 0;
   NumPtsAbove := 0;
   if (MapOwner <> Nil) then begin
      SetXYBox(Dist,xlo,xhi,ylo,yhi);
      zlo := Height - 0.5 * MDDef.CloudSliceThick;
      zhi := Height + 0.5 * MDDef.CloudSliceThick;
      for i := 1 to NumMemPts do begin
         if (zPts^[i] >= zlo) then begin
            if (xyPts^[i,2] >= xlo) and (xyPts^[i,2] <= xhi) and (xyPts^[i,1] >= ylo) and (xyPts^[i,1] <= yhi) then begin
               if (zPts^[i] <= zhi) then inc(NumPtsNear) else inc(NumPtsAbove);
            end;
         end;
      end;
   end;
end;


procedure tMemoryPointCloud.GetAcrossProfileDensity(xz : float64);
var
   i,x,y,z : integer;
begin
   if (AcrossProfileDensity = nil) then begin
     {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('allocate tMemoryPointCloud.GetAcrossProfileDensity, pointer size=' + IntToStr(SizeOf(AcrossProfileDensity))); {$EndIf}
     new(AcrossProfileDensity);
   end;
   for y := -MaxYDensity to MaxYDensity do begin
      AcrossProfileGround[y] := 9999;
      for z := 0 to MaxZDensity do AcrossProfileDensity^[y,z] := 0;
   end;

   for i := 1 to NumMemPts do begin
      if (xyPts^[i,2] >= xz-0.5) and (xyPts^[i,2] <= xz+0.5) then begin
         x := trunc(xyPts^[i,1]);
         if ( x >= -MaxYDensity) and (x <= -MaxYDensity) then begin
            if AcrossProfileGround[x] > zPts^[i] then AcrossProfileGround[x] := zPts^[i];
         end;
      end;
   end;

   for i := 1 to NumMemPts do begin
      if (xyPts^[i,2] >= xz-0.5) and (xyPts^[i,2] <= xz+0.5) then begin
         x := trunc(xyPts^[i,1]);
         if ( x >= 0) and (x <= MaxXDensity) then begin
            z := trunc(zPts^[i] - AcrossProfileGround[x]);
            if (z >= 0) and (z <= MaxzDensity) then begin
               inc(AcrossProfileDensity^[x,z]);
            end;
         end;
      end;
   end;
end;


procedure tMemoryPointCloud.GetAlongProfileDensity;
var
   i,x,z : integer;
begin
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.GetAlongProfileDensity in'); {$EndIf}
  if (AlongProfileDensity = nil) then begin
     new(AlongProfileDensity);
     for x := 0 to MaxXDensity do begin
        AlongProfileGround[x] := 9999;
        for z := 0 to MaxYDensity do AlongProfileDensity^[x,z] := 0;
     end;

     for i := 1 to NumMemPts do begin
        if (xyPts^[i,1] >= -0.5) and (xyPts^[i,1] <= 0.5) then begin
           x := trunc(xyPts^[i,2]);
           if (x >= 0) and (x <= MaxXDensity) then begin
              if AlongProfileGround[x] > zPts^[i] then AlongProfileGround[x] := zPts^[i];
           end;
        end;
     end;

     for i := 1 to NumMemPts do begin
        if (xyPts^[i,1] >= -0.5) and (xyPts^[i,1] <= 0.5) then begin
           x := trunc(xyPts^[i,2]);
           if ( x >= 0) and (x <= MaxXDensity) then begin
              z := trunc(zPts^[i] - AlongProfileGround[x]);
              if (z >= 0) and (z <= MaxYDensity) then begin
                 inc(AlongProfileDensity^[x,z]);
              end;
           end;
        end;
     end;
  end;
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.GetAlongProfileDensity out'); {$EndIf}
end;


procedure tMemoryPointCloud.GetPointUTMCoords(i : integer; var xutm,yutm : float64);
begin
   xutm := xyPts^[i,1];
   yutm := xyPts^[i,2];
end;


procedure tMemoryPointCloud.HowHighUp;
label
   NotThisPoint;
var
   zd : float32;
   xlo,xhi,ylo,yhi, Lat,Long,CellDZ,
   PC_99,PC_95,PC_5,PC_1,xcoord,ycoord : float64;
   //WantImage,
   DensityGrid,i,j,x,y,db,n : integer;
   TStr : shortString;
   Results : tStringList;
   fName : pathStr;
   zvs : ^bfarray32;
   MomentVar : tMomentVar;
   z,fr,pc,slp : array[1..MaxCompare] of float32;
   BoxSize : float64;


         function PCfromZ(z : float32; var f : float32) : float32;
         var
            i : integer;
         begin
            if (z > MomentVar.MaxZ) then begin
               Result := 110;
            end
            else if (z < MomentVar.MinZ) then begin
               Result := -10;
            end
            else begin
               i := MomentVar.Npts;
               while zvs^[i] > z do dec(i);
               Result := (100 * i/MomentVar.Npts);
            end;
            if abs(MomentVar.MaxZ-MomentVar.MinZ) < 0.01 then f := -9999
            else f := (z - MomentVar.MinZ) / (MomentVar.MaxZ-MomentVar.MinZ);
         end;


begin
   if (MapOwner <> Nil) then begin
      if AnswerIsYes('Load existing DBF to explore results') then begin
         MapOwner.OpenDBonMap('','');
         //LoadDEMs(MapOwner.MapDraw.MapCorners.BoundBoxGeo);
      end
      else begin
         fName := ExtractFilePath(LastDEMName);
         DensityGrid := OpenNewDEM(fName,false,'lidar density grid');
         if ValidDEM(DensityGrid) then begin

            if LoadDEMsCoveringBox(DEMGlb[DensityGrid].DEMBoundBoxGeo) = 0 then begin
               MessageToContinue('No DEMs found');
            end
            else begin

               BoxSize := 0.5 * MDDef.Icesat2.BoxSize / 3600;

               Results := tStringList.Create;
               TStr := 'LAT,LONG,CLOUD_PTS,CLOUD_MAX,CLOUD_99,CLOUD_95,CLOUD_MEAN,CLOUD_MED,CLOUD_5,CLOUD_1,CLOUD_MIN,CLOUD_HT,AVG_SLP';
               for j := 1 to MaxCompare do begin
                  if CompareDEMIndexes[j] <> 0 then begin
                      TStr := TStr + ',' + CompareDEMNames[j] + '_Z,' + TStr + CompareDEMNames[j] + '_SLP,'  + TStr + CompareDEMNames[j] + '_FR,'  + TStr + CompareDEMNames[j] + '_PC';
                  end;
               end;
               Results.Add(TStr);
               New(zvs);
               StartProgress('Canopy position started ' + TimeToStr(Now));
               for x := 0 to pred(DEMGlb[DensityGrid].DEMheader.NumCol)do begin
                  UpdateProgressBar(x/DEMGlb[DensityGrid].DEMheader.NumCol);
                  wmDEM.SetPanelText(0,'Cells processed: ' + IntToStr(Pred(Results.Count)));
                  for y := 0 to pred(DEMglb[DensityGrid].DEMheader.NumRow)do begin
                     if DEMGlb[DensityGrid].GetElevMetersOngrid(x,y,zd) and (zd > 0.1) then begin
                        for j := 1 to MaxCompare do begin
                           pc[j] := -9999;
                           slp[j] := -9999;
                           z[j] := -9999;
                           fr[j] := -9999;
                        end;
                        DEMGlb[DensityGrid].DEMGridToLatLongDegree(X,Y,Lat,Long);
                        xcoord := Long;
                        yCoord := Lat;
                        xlo := xcoord - BoxSize;
                        xhi := xcoord + BoxSize;
                        ylo := ycoord - BoxSize;
                        yhi := ycoord + BoxSize;

                        MomentVar.Npts := 0;
                        for i := 1 to NumMemPts do begin
                           xcoord := xyPts^[i,1];
                           ycoord := xyPts^[i,2];
                           if (xcoord >= xlo) and (xcoord <= xhi) and (ycoord >= ylo) and (ycoord <= yhi) then begin
                              zvs^[MomentVar.Npts] := zPts^[i];
                              inc(MomentVar.NPts);
                           end;
                        end;

                        if (MomentVar.NPts > MDDef.Icesat2.PCPtsRequired) then begin
                           Petmath.HeapSort(MomentVar.NPts,zvs^);
                           Moment(zvs^,MomentVar,msAll);
                           dec(MomentVar.Npts);
                           PC_99 := zvs^[round(0.99 * MomentVar.Npts)];
                           PC_95 := zvs^[round(0.95 * MomentVar.Npts)];
                           PC_5 := zvs^[round(0.05 * MomentVar.Npts)];
                           PC_1 := zvs^[round(0.01 * MomentVar.Npts)];
                           n := 0;
                           MomentVar.Median := Median(zvs^,MomentVar.Npts);
                           for j := 1 to MaxCompare do begin
                              if (CompareDEMIndexes[j] <> 0) then begin
                                 if DEMGlb[CompareDEMIndexes[j]].GetElevFromLatLongDegree(Lat,Long,z[j]) then begin
                                    inc(n);
                                    pc[j] := PCfromZ(z[j],fr[j]);
                                    slp[j] := DEMGlb[CompareDEMIndexes[j]].SlopePercentFromLatLong(MDDef.SlopeCompute,Lat,Long);
                                 end;
                              end;
                           end;

                          CellDZ := Median(slp,n) * 30 / 100;
                          TStr := RealToString(Lat,-12,-6) + ',' +  RealToString(Long,-12,-6) + ',' +
                                 IntToStr(succ(MomentVar.NPts)) + ',' + RealToString(MomentVar.MaxZ,-8,-2) + ',' + RealToString(PC_99,-8,-2) + ',' +  RealToString(PC_95,-8,-2) + ',' +
                                 RealToString(MomentVar.mean,-8,-2) + ',' +  RealToString(MomentVar.Median,-8,-2) + ',' +
                                 RealToString(PC_5,-8,-2) + ',' +  RealToString(PC_1,-8,-2) + ',' +RealToString(MomentVar.MinZ,-8,-2) + ',' +
                                 RealToString(MomentVar.MaxZ-MomentVar.MinZ,-8,-2)+ ',' +  RealToString(CellDZ,-8,-2);
                           for j := 1 to MaxCompare do begin
                              if CompareDEMIndexes[j] <> 0 then begin
                                 TStr := TStr + ',' + RealToString(z[j],-12,-2)+ ',' + RealToString(slp[j],-12,-2)+ ',' + RealToString(fr[j],-12,-2)+ ',' + RealToString(pc[j],-12,-2);
                              end;
                          end;
                           Results.Add(TStr);
                           NotThisPoint:;
                        end;
                     end;
                  end;
               end;
               Dispose(zvs);
               EndProgress;
               fName :=  Petmar.NextFileNumber(ExtractFilePath(DEMGlb[DensityGrid].DEMFileName),CloudName + 'point_cloud_','.csv');
               db := MapOwner.DisplayAndPurgeStringListDB(Results,fName);
               CloseSingleDEM(DensityGrid);
            end;
            HistogramPointCloudAndGlobalDEMs(db);
         end;
      end;
   end;
end;


procedure tMemoryPointCloud.GetRegionStats(Lat,Long : float64);
var
   xlo,xhi,ylo,yhi,xcoord,ycoord, BoxSize{,z1,z2,z3} : float64;
   i: integer;
   //TStr : shortString;
   Results,Distributions : tStringList;
   fName : pathStr;
   z : float32;
   zvs : ^bfarray32;
   MomentVar : tMomentVar;
   Histogram : TThisBaseGraph;


      procedure DEMSymbol(DEM : integer; Color : tPlatformColor);
      var
         rfile : file;
         v : array[1..2] of float32;
      begin
         if ValidDEM(DEM) and DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,v[2]) then begin
            Histogram.MainSymbol.Size := 4;
            Histogram.MainSymbol.Color := Color;
            Histogram.MainSymbol.DrawingSymbol := FilledBox;
            ScreenSymbol(Histogram.Image1.Canvas, Histogram.GraphDraw.GraphX(0)+2, Histogram.GraphDraw.GraphY(v[2]),Histogram.MainSymbol);
            Histogram.OpenPointSymbolFile(rfile,'',Histogram.MainSymbol);
            v[1] := 0;
            Blockwrite(rfile,v,1);
            CloseFile(rfile);
         end;
      end;

      procedure DEMPercentile(DEM : integer; aLabel : shortString);
      begin
         if ValidDEM(DEM) and DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z) then begin
            i := MomentVar.Npts;
            while zvs^[i] > z do dec(i);
            Results.Add(ALabel + ' z:  ' + RealToString(z,8,2) + RealToString(100 * i / MomentVar.Npts,8,2) + '%   ' + RealToString( (z - MomentVar.MinZ) / (MomentVar.MaxZ-MomentVar.MinZ),-12,-2));
         end;
      end;


begin
   if (MapOwner <> Nil) then begin
      LoadDEMsCoveringBox(MapOwner.MapDraw.MapCorners.BoundBoxGeo);

      BoxSize := 0.5 * 1 / 3600;
      xcoord := Long;
      yCoord := Lat;

      xlo := xcoord - BoxSize;
      xhi := xcoord + BoxSize;
      ylo := ycoord - BoxSize;
      yhi := ycoord + BoxSize;

      Results := tStringList.Create;
      MomentVar.Npts := 0;
      New(zvs);
      for i := 1 to NumMemPts do begin
         xcoord := xyPts^[i,1];
         ycoord := xyPts^[i,2];
         if (xcoord >= xlo) and (xcoord <= xhi) and (ycoord >= ylo) and (ycoord <= yhi) then begin
            zvs^[MomentVar.Npts] := zPts^[i];
            inc(MomentVar.NPts);
         end;
      end;

      Results.Add('NPts:   ' + IntToStr(MomentVar.NPts));
      if (MomentVar.NPts > 0) then begin
         Petmath.HeapSort(MomentVar.NPts,zvs^);
         Moment(zvs^,MomentVar,msAll);
         dec(MomentVar.Npts);
         Results.Add('Max:    ' + RealToString(MomentVar.MaxZ,8,2));
         Results.Add('99 pc: ' + RealToString(zvs^[round(0.99 * MomentVar.Npts)],8,2));
         Results.Add('95 pc: ' + RealToString(zvs^[round(0.95 * MomentVar.Npts)],8,2));
         Results.Add('90 pc: ' + RealToString(zvs^[round(0.90 * MomentVar.Npts)],8,2));
         Results.Add('Mean:   ' + RealToString(MomentVar.mean,8,2));
         Results.Add('Median: ' + RealToString(MomentVar.Median,8,2));
         Results.Add('50 pc: ' + RealToString(zvs^[round(0.50 * MomentVar.Npts)],8,2));
         Results.Add(' 5 pc: ' + RealToString(zvs^[round(0.05 * MomentVar.Npts)],8,2));
         Results.Add('Min:   ' + RealToString(MomentVar.MinZ,8,2));
         Results.Add('StdDev: ' + RealToString(MomentVar.std_dev,8,2));

         for i := 1 to MaxCompare do begin
            if (CompareDEMIndexes[i] <> 0) then DEMPercentile(CompareDEMIndexes[i],CompareDEMNames[i]);
         end;

         MDDef.FlipHistogram := true;


         //Histogram := DeprecatedCreateHistogram(true,MomentVar.NPts,zvs^,'Elevations','Distribution at ' + LatLongDegreeToString(Lat,Long) );

         Distributions := tStringList.Create;
         fName := NextFileNumber(MDTempDir,'pc_distrib_','.z');
         Distributions.Add(SaveSingleValueSeries(MomentVar.NPts,zvs^,fName));
         Histogram := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Nil,'', '');   //,200,Min,Max,BinSize);


         //Histogram.GraphDraw.LegendList := tStringList.Create;
         //Histogram.GraphDraw.LegendList.Add('Point cloud');
         for i := 1 to MaxCompare do begin
            if (CompareDEMIndexes[i] <> 0) then begin
               //Histogram.GraphDraw.LegendList.Add(CompareDEMNames[i]);
               DEMSymbol(CompareDEMIndexes[i],ConvertTColorToPlatformColor(WinGraphColors(i)));
            end;
         end;
         Histogram.GraphDraw.LLcornerText := LatLongDegreeToString(Lat,Long);
      end;
      Dispose(zvs);
      fName := MDTempDir + 'pt_location_.csv';
      DisplayAndPurgeStringList(Results,'Stats');
   end;
end;


procedure tMemoryPointCloud.ExtractPointsNearHere(Lat, Long, FullBoxSizeMeters: float64);
var
   xlo,xhi,ylo,yhi,
   xutm,yutm,BoxSize : float64;
   i : integer;
   TStr : shortString;
   Results : tStringList;
   fName : pathStr;
begin
   if (MapOwner <> Nil) then begin
      MapOwner.MapDraw.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
      BoxSize := 0.5 * FullBoxSizeMeters;
      xlo := xutm - BoxSize;
      xhi := xutm + BoxSize;
      ylo := yutm - BoxSize;
      yhi := yutm + BoxSize;
      TStr := 'LAT,LONG,X_UTM,Y_UTM,Z';
      if (xyPts <> Nil) then TStr := TStr + ',ALONG_PROF,ACROSS_PRF';
      if (PtClass <> Nil) then TStr := TStr + ',CLASS';
      if (PtReturnNumber <> Nil) then TStr := TStr + ',RET_NUMBER';

      Results := tStringList.Create;
      Results.Add(TStr);

      for i := 1 to NumMemPts do begin
         GetPointUTMCoords(i,xutm,yutm);
         if (xutm >= xlo) and (xutm <= xhi) and (yutm >= ylo) and (yutm <= yhi) then begin
            MapOwner.MapDraw.PrimMapProj.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
            TStr := RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8) + ',' + RealToString(xutm,-18,-2) + ',' + RealToString(yutm,-18,-2) + ',' + RealToString(zPts^[i],-18,-2);
            if (PtClass <> Nil) then TStr := TStr + ',' + IntToStr(PtClass^[i]);
            if (PtReturnNumber <> Nil) then TStr := TStr + ',' + IntToStr(PtReturnNumber^[i]);
            Results.Add(TStr);
         end;
      end;
      fName := MDTempDir + 'pt_location_.csv';
      MapOwner.DisplayAndPurgeStringListDB(Results,fName);
   end;
end;


procedure tMemoryPointCloud.ExtractMinMaxPoints(Lat, Long : float64; var MinZ,MaxZ : float64; UseOnlyGroundClass : boolean);
var
   xlo,xhi,ylo,yhi,
   xutm,yutm : float64;
   i : integer;
begin
   MinZ := 29999;
   MaxZ := -29999;
   if (MapOwner <> Nil) then begin
      MapOwner.MapDraw.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
      xlo := xutm - 0.5 * MDDef.CloudSliceThick;
      xhi := xutm + 0.5 * MDDef.CloudSliceThick;
      ylo := yutm - 0.5 * MDDef.CloudSliceThick;
      yhi := yutm + 0.5 * MDDef.CloudSliceThick;
      for i := 1 to NumMemPts do begin
         GetPointUTMCoords(i,xutm,yutm);
         if (xutm >= xlo) and (xutm <= xhi) and (yutm >= ylo) and (yutm <= yhi) then begin
            if UseOnlyGroundClass and (PtClass^[NumMemPts] = 2) then Petmath.CompareValueToExtremes(zPts^[i],MinZ,MaxZ)
            else Petmath.CompareValueToExtremes(zPts^[i],MinZ,MaxZ);
         end;
      end;
   end;
end;


procedure tMemoryPointCloud.ExtractMinMaxPointsFromDistance(Dist : float64; var MinZ, MaxZ: float64; UseOnlyGroundClass : boolean);
var
   xlo,xhi,ylo,yhi : float64;
   i : integer;
begin
   MinZ := 29999;
   MaxZ := -29999;
   if (MapOwner <> Nil) then begin
      xlo := Dist - 0.5 * MDDef.CloudSliceThick;
      xhi := Dist + 0.5 * MDDef.CloudSliceThick;
      ylo := -0.5 * MDDef.CloudSliceThick;
      yhi := 0.5 * MDDef.CloudSliceThick;
      if MDDef.DifferentFloorSliceThickness then begin
         for i := 1 to NumMemPts do begin
            if ( xyPts^[i,2] >= xlo) and ( xyPts^[i,2] <= xhi) and ( xyPts^[i,1] >= ylo) and ( xyPts^[i,1] <= yhi) then begin
               if (zPts^[i] > MaxZ) then MaxZ := zPts^[i];
            end;
         end;

         xlo := Dist - 0.5 * MDDef.FloorSliceThick;
         xhi := Dist + 0.5 * MDDef.FloorSliceThick;
         ylo := -0.5 * MDDef.FloorSliceThick;
         yhi := 0.5 * MDDef.FloorSliceThick;

         for i := 1 to NumMemPts do begin
            if ( xyPts^[i,2] >= xlo) and ( xyPts^[i,2] <= xhi) and ( xyPts^[i,1] >= ylo) and ( xyPts^[i,1] <= yhi) then begin
               if ( not UseOnlyGroundClass) or (PtClass^[NumMemPts] = 2) then begin
                  if (zPts^[i] < MinZ) then MinZ := zPts^[i];
               end;
            end;
         end;
      end
      else begin
         for i := 1 to NumMemPts do begin
            if ( xyPts^[i,2] >= xlo) and ( xyPts^[i,2] <= xhi) and ( xyPts^[i,1] >= ylo) and ( xyPts^[i,1] <= yhi) then begin
               if (not UseOnlyGroundClass) or (PtClass^[NumMemPts] = 2) then begin
                  Petmath.CompareValueToExtremes(zPts^[i],MinZ,MaxZ);
               end
               else begin
                  if (zPts^[i] > MaxZ) then MaxZ := zPts^[i];
               end;
            end;
         end;
      end;
   end;
end;


procedure tMemoryPointCloud.ZeroXLimits;
begin
    pcMinX := 99e39;
    pcMaxX := -99e39;
end;

procedure tMemoryPointCloud.ZeroYLimits;
begin
    pcMinY := 99e39;
    pcMaxY := -99e39;
end;

procedure tMemoryPointCloud.ZeroZLimits;
begin
    pcMinZ := 99e39;
    pcMaxZ := -99e39;
end;


{$IfDef RecordPointCloudLimits}
procedure tMemoryPointCloud.ShowXYLimits(Where : ShortString);
begin
   WriteLineToDebugFile('Point cloud limits ' + Where + ' SW x=' + RealToString(pcMinX,-18,8) + ' y=' + RealToString(pcMinY,-18,8) + ' NE x=' + RealToString(pcMaxX,-18,8) + ' y=' + RealToString(pcMaxY,-18,8));
end;
{$EndIf}


function tMemoryPointCloud.AddPointToMemory(x,y,z : float64) : boolean;
begin
   if (NumMemPts < AllocatedPoints) then begin
       inc(NumMemPts);
       XYPts^[NumMemPts][1] := x;
       XYPts^[NumMemPts][2] := y;
       zPts^[NumMemPts] := z;
       Petmath.CompareValueToExtremes(x,pcMinX,pcMaxX);
       Petmath.CompareValueToExtremes(y,pcMinY,pcMaxY);
       Petmath.CompareValueToExtremes(z,pcMinZ,pcMaxZ);
       result := true;
   end
   else begin
      MessageToContinue('Reached point max (' + IntToStr(AllocatedPoints) + '), you could increase to ' + IntToStr(TheMaxPointsMemPtCloud) + '  LAS tab, options form');
      Result := false;
   end;
end;


procedure tMemoryPointCloud.RotateXY(Angle: float64);
var
  I : Integer;
  sine,cosine,oldx,oldy : float64;
begin
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.RotateXY in'); {$EndIf}
   {$IfDef RecordPointCloudLimits} ShowXYLimits('Before rotation'); {$EndIf}
   sine := sinDeg(Angle);
   cosine := CosDeg(Angle);
   ZeroXLimits;
   ZeroYLimits;
   for I := 1 to NumMemPts do begin
      Oldx := xyPts^[i,1];
      oldY := xyPts^[i,2];
      xypts^[i,1] := oldX * cosine - OldY * sine;
      xypts^[i,2] := OldX * sine   + OldY * cosine;
      Petmath.CompareValueToExtremes(xypts^[i,1],pcMinX,pcMaxX);
      Petmath.CompareValueToExtremes(xypts^[i,2],pcMinY,pcMaxY);
   end;
   {$IfDef RecordPointCloudLimits} ShowXYLimits('After rotation'); {$EndIf}
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.RotateXY out'); {$EndIf}
end;


procedure tMemoryPointCloud.RotateXZ(Angle: float64);
var
  I : Integer;
  sine,cosine,oldx,oldz : float64;
begin
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.TranslateAndRotate in'); {$EndIf}
   {$IfDef RecordPointCloudLimits} ShowXYLimits('Before rotation'); {$EndIf}

   sine := sinDeg(Angle);
   cosine := CosDeg(Angle);

   ZeroXLimits;
   ZeroZLimits;

   for I := 1 to NumMemPts do begin
      Oldx := xyPts^[i,1];
      oldZ := zPts^[i];
      xypts^[i,1] := OldX * cosine - OldZ * sine;
      zpts^[i] := OldX * sine   + OldZ * cosine;
      Petmath.CompareValueToExtremes(xypts^[i,1],pcMinX,pcMaxX);
      Petmath.CompareValueToExtremes(zpts^[i],pcMinZ,pcMaxZ);
   end;
   {$IfDef RecordPointCloudLimits} ShowXYLimits('After rotation'); {$EndIf}
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.TranslateAndRotate out'); {$EndIf}
end;


procedure tMemoryPointCloud.RotateYZ(Angle: float64);
var
  I : Integer;
  sine,cosine,OldY,OldZ : float64;
begin
   {$If Defined(RecordPointCloudmemory) or Defined(RecordPointCloudLimits)} ShowXYLimits('Before rotation'); {$EndIf}
   sine := sinDeg(Angle);
   cosine := CosDeg(Angle);
   ZeroYLimits;
   ZeroZLimits;
   for I := 1 to NumMemPts do begin
      OldY := xyPts^[i,1];
      OldZ := zPts^[i];
      xypts^[i,2] := OldY * cosine - OldZ * sine;
      zpts^[i] := OldY * sine   + OldZ * cosine;
      Petmath.CompareValueToExtremes(xypts^[i,2],pcMinY,pcMaxY);
      Petmath.CompareValueToExtremes(zpts^[i],pcMinZ,pcMaxZ);
   end;
   {$If Defined(RecordPointCloudmemory) or Defined(RecordPointCloudLimits)} ShowXYLimits('After rotation'); {$EndIf}
end;


procedure tMemoryPointCloud.TranslateAndRotate(dx, dy, Angle: float64);
var
  I : Integer;
begin
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.TranslateAndRotate in'); {$EndIf}
   {$IfDef RecordPointCloudLimits}
      ShowXYLimits('tMemoryPointCloud.TranslateAndRotate initial');
      WriteLineToDebugFile('Translate x=' + RealToString(dx,-12,0) + '  and y=' + RealToString(dy,-12,0));
   {$EndIf}

   if (xyPts = Nil) then begin
      GetMem(xyPts,AllocatedPoints * 2 * sizeOf(float64));
      {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('allocate xypoints in tMemoryPointCloud.TranslateAndRotate=' + IntToStr(AllocatedPoints * 2 * sizeOf(float64))); {$EndIf}
   end;

   ZeroXLimits;
   ZeroYLimits;

   for I := 1 to NumMemPts do begin
      xypts^[i,1] := xypts^[i,1] + dx;
      xypts^[i,2] := xypts^[i,2] + dy;
      Petmath.CompareValueToExtremes(xypts^[i,1],pcMinX,pcMaxX);
      Petmath.CompareValueToExtremes(xypts^[i,2],pcMinY,pcMaxY);
   end;
   {$IfDef RecordPointCloudLimits} ShowXYLimits('After translation'); {$EndIf}
   if abs(Angle) > 0.001 then begin
      RotateXY(Angle);
      {$IfDef RecordPointCloudLimits} ShowXYLimits('After rotation'); {$EndIf}
   end;
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.TranslateAndRotate out'); {$EndIf}
end;


procedure tMemoryPointCloud.LoadFromLASFile(BaseMap : tMapForm; fName: PathStr);
label
   DataFull;
var
   i,j,RecsRead,ColorsOut,ReadsNeeded : integer;
   LAS : tLAS_data;
   xc,yc : float64;
begin
   if not FileExists(fName) then begin
      {$IfDef RecordPointCloudMemory} WriteLinetoDebugFile('tMemoryPointCloud.LoadFromLASFile missing ' +  ExtractFileNameNoExt(fName)); {$EndIf}
      exit;
   end
   else begin
      {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.LoadFromLASFile in ' +  ExtractFileNameNoExt(fName)); {$EndIf}
   end;
   {$IfDef RecordPointCloudLimits} ShowXYLimits('Before LASLoad'); {$EndIf}
   LASsourceName := fName;
   LAS := tLAS_data.Create(FName);
   {$IfDef RecordPointCloudLimits} WriteLinetoDebugFile('LAS file range: ' + AllRangeString(Las.LasHeader)); {$EndIf}

   AllocatedPoints := LAS.NumPointRecs;
   if (AllocatedPoints > MDDef.MemoryPointCloudMaxPts) then begin
      {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('Too many xypoints=' + IntToStr(AllocatedPoints)); {$EndIf}
      AllocatedPoints := MDDef.MemoryPointCloudMaxPts;
      if (AllocatedPoints < TheMaxPointsMemPtCloud) then begin
         if AnswerIsYes('Reached point max (' + IntToStr(AllocatedPoints) + '), increase to ' + IntToStr(TheMaxPointsMemPtCloud) + '  LAS tab, options form') then begin
            MDDef.MemoryPointCloudMaxPts := TheMaxPointsMemPtCloud;
            AllocatedPoints := LAS.NumPointRecs;
            if (AllocatedPoints > MDDef.MemoryPointCloudMaxPts) then AllocatedPoints := MDDef.MemoryPointCloudMaxPts;
         end;
      end;
   end;
   {$IfDef RecordMemoryAllocations} WriteLineToDebugFile('Try to allocated xypoints=' + IntToStr(AllocatedPoints)); {$EndIf}
   GetMem(xyPts,AllocatedPoints * 2 * sizeOf(float64));
   GetMem(zPts,AllocatedPoints * 1 * sizeOf(float64));
   {$IfDef RecordMemoryAllocations} WriteLinetoDebugFile('allocated in tMemoryPointCloud.LoadFromLASFile, bytes=' + IntToStr(AllocatedPoints * 3 * sizeOf(float64))); {$EndIf}

   if (PtRGB = Nil) and (LAS.LidarPointType in [2,3]) and MDdef.LoadLASRGBToMemory then GetMem(PtRGB,3*AllocatedPoints);
   if (PtClass = Nil) and MDdef.LoadLASclassificationToMemory then GetMem(PtClass,AllocatedPoints);
   if (PtReturnNumber = Nil) and MDdef.LoadLASreturnNumberToMemory then GetMem(PtReturnNumber,AllocatedPoints);
   Las.PrepDataRead;
   StartProgress('LAS to memory');
    ColorsOut := 0;
    ReadsNeeded := succ(LAS.NumPointRecs div MaxLASPtsToRead);
    for i := 1 to ReadsNeeded do begin
        UpDateProgressBar(i/ReadsNeeded);
        LAS.ReadPoints(RecsRead);
        for j := 1 to RecsRead do begin
           if MDDef.PointCloudColorFilter and LAS.ColorFilterOut(j) then begin
              inc(ColorsOut);
           end
           else begin
              if StayLatLong then begin
                 LAS.GetShotCoordinatesLatLong(j,yc,xc);
              end
              else begin
                 LAS.GetShotCoordinatesUTM(j,xc,yc);
              end;

              if AddPointToMemory(xc,yc,LAS.ExpandLas_Z(j)) then begin
                if (PtClass <> Nil) then PtClass^[NumMemPts] := LAS.LASClassification(j);
                if (PtReturnNumber <> Nil) then PtReturnNumber^[NumMemPts] := LAS.ReturnNumber(j);
                if (PtRGB <> Nil) then PtRGB^[NumMemPts] := LAS.GetRGBColor(j);
              end;
              if (NumMemPts = AllocatedPoints) then goto DataFull;
           end;
        end;
    end;
  DataFull:;
    Las.FreeLASRecordMemory;
    Las.Destroy;
    EndProgress;
   {$IfDef RecordPointCloudLimits} ShowXYLimits('After LASLoad'); {$EndIf}
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('remove for color=' + IntToStr(ColorsOut) + '  loaded, pts=' + IntToStr(NumMemPts)); {$EndIf}
end;


procedure tMemoryPointCloud.LoadFromLVISFile(fName: PathStr);
var
   i : integer;
   LVIS : tLVIS_Dataset;
   lce : tlce;
   lge : tlge;
   xutm,yutm : float64;
begin
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.LoadFromLVISFile in'); {$EndIf}
   if (NumMemPts = AllocatedPoints) then exit;
   LVIS := tLVIS_Dataset.Create(fName,nil);
    LVIS.OpenLCEFile;
    LVIS.OpenLGEfile;
    StartProgress('Load LVIS');
    while not eof(LVIS.lcef) do begin
      inc(i);
      if (i mod 100 = 0) then UpDateprogressBar(i/LVIS.NumRecs);
      lce := LVIS.ReadLCErecord;
      lge := LVIS.ReadLGErecord;
      WGS84DatumConstants.ForwardProjectDegrees(lce.tlat,lce.tlong,xutm,yutm);
      if AddPointToMemory(xutm,yutm,lce.zt) then begin
         WGS84DatumConstants.ForwardProjectDegrees(lge.glat,lge.glon,xutm,yutm);
         if not AddPointToMemory(xutm,yutm,lge.zg) then break;
      end
      else break;
    end;
    closeFile(LVIS.lcef);
    closeFile(LVIS.lgef);
    EndProgress;
    LVIS.Free;
end;


procedure tMemoryPointCloud.LoadFromASCIIFile(fName: PathStr);
begin
   fName := CSVFileImportToDB(fName);
   LoadFromShapeFile(fName);
end;


procedure tMemoryPointCloud.LoadFromShapeFile(fName: PathStr; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z');
var
   MyData : tMyData;
   BasePath : PathStr;
   fName2 : PathStr;
   k : integer;
   sf : tShapeFile;
   Table : tMyData;
         function GetPointsInFile(Name1 : PathStr) : integer;
         var
            Success : boolean;
         begin
            Result := 0;
            if FileExists(Name1) then begin
               if ExtEquals(ExtractFileExt(Name1), DefaultDBExt) then begin
                   Table := tMyData.Create(Name1);
                   Table.Filtered := false;
                   Result := Table.RecordCount;
               end
               else begin
                   sf := tShapeFile.Create(Name1,Success);
                   Result := sf.NumRecs;
               end;
               {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('GetPointsInFile, ' + ExtractFileName(Name1) + ' recs= ' + IntToStr(Result)); {$EndIf}
            end;
         end;

         procedure LoadOneFile(Name1 : PathStr; TStr : ShortString = '');
         var
            i,TotalRead,RecsRead,tr  : integer;
            x,y,z : float64;
            ShapePoints : ^tLotsOfPoints3D;
            //Success : boolean;
         begin
            if FileExists(Name1) then begin
               TotalRead := 0;
               StartProgress('Read ' + TStr);
               if ExtEquals(ExtractFileExt(Name1), DefaultDBExt) then begin
                   {$IfDef RecordPointCloudmemory} WriteLineToDebugFile('Read dbf ' + ExtractFileName(Name1)); {$EndIf}
                   tr := Table.RecordCount;
                   while not Table.eof do begin
                      inc(TotalRead);
                      if (TotalRead mod 1000 = 0) then UpdateProgressBar(TotalRead/tr);
                      x := Table.GetFieldByNameAsFloat(XField);
                      y := Table.GetFieldByNameAsFloat(YField);
                      z := Table.GetFieldByNameAsFloat(ZField);
                      if not AddPointToMemory(x,y,z) then break;
                      Table.Next;
                   end;
                   Table.Destroy;
               end
               else begin
                    {$IfDef RecordPointCloudmemory} WriteLineToDebugFile('Read shapefile ' + ExtractFileName(Name1)); {$EndIf}
                    new(ShapePoints);
                    reset(sf.ShapeFile,1);
                    BlockRead(sf.ShapeFile,sf.MainFileHeader,SizeOf(sf.MainFileHeader));
                    repeat
                       BlockRead(sf.ShapeFile,ShapePoints[1],sfMaxPoints*Sizeof(sfPointsZWithHeader),RecsRead);
                       RecsRead := RecsRead div SizeOf(sfPointsZWithHeader);
                       for i := 1 to RecsRead do begin
                          if not AddPointToMemory(ShapePoints^[i].x,ShapePoints^[i].y,ShapePoints^[i].z) then break;
                          inc(TotalRead);
                       end;
                       UpdateProgressBar(TotalRead/sf.NumRecs);
                       if (NumMemPts = AllocatedPoints) then break;
                    until System.eof(sf.ShapeFile);
                    Dispose(ShapePoints);
                    FreeAndNil(sf);
               end;
               EndProgress;
               {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('LoadOneFile ' + ExtractFileName(Name1) + '  recs= ' + IntToStr(TotalRead) + '  total points now=' + IntToStr(NumMemPts)); {$EndIf}
            end
            else begin
               {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('LoadOneFile ' + ExtractFileName(Name1) + ' File not found'); {$EndIf}
            end;
         end;


begin
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.LoadFromShapeFile in'); {$EndIf}

   if (Copy(ExtractFileName(fName),1,4) = 'PROJ') and ExtEquals(ExtractFileExt(fName), DefaultDBExt) then begin
       BasePath := ExtractFilePath(fName);
       MyData := tMyData.Create(fName);
       AllocatedPoints := 0;

       while not MyData.EOF do begin
          if MyData.GetFullFileName(fName2) then AllocatedPoints := AllocatedPoints + GetPointsInFile(fName2);
          {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile(ExtractFileName(fName2) +  ' GetPointsInFile: ' + IntToStr(GetPointsInFile(fName2)) +  '  total pts: ' + IntToStr(AllocatedPoints)); {$EndIf}
          MyData.Next;
       end;
       MyData.Destroy;
   end
   else begin
      AllocatedPoints := GetPointsInFile(fName);
   end;
   AllocatedPoints := AllocatedPoints + 100;
   if (AllocatedPoints > MDDef.MemoryPointCloudMaxPts) then AllocatedPoints := MDDef.MemoryPointCloudMaxPts;

   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('Allocating pts: ' + IntToStr(AllocatedPoints)); {$EndIf}
   GetMem(xyPts,AllocatedPoints * 2 * sizeOf(float64));
   GetMem(zPts,AllocatedPoints * 1 * sizeOf(float64));

   if (Copy(ExtractFileName(fName),1,4) = 'PROJ') and ExtEquals(ExtractFileExt(fName), DefaultDBExt) then begin
      {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('PROJ definition DB'); {$EndIf}
       BasePath := ExtractFilePath(fName);
       MyData := tMyData.Create(fName);
       k := 1;
       while not MyData.EOF do begin
          if MyData.GetFullFileName(fName2) then begin
             LoadOneFile(fName2,IntToStr(k) + '/' + IntToStr(MyData.RecordCount));
             inc(k);
             if (NumMemPts = AllocatedPoints) then break;
          end;
          MyData.Next;
       end;
       pcMinX := MyData.FindFieldMin('MIN_X');
       pcMaxX := MyData.FindFieldMax('MAX_X');
       pcMinY := MyData.FindFieldMin('MIN_Y');
       pcMaxY := MyData.FindFieldMax('MAX_Y');
       pcMinZ := MyData.FindFieldMin('MIN_Z');
       pcMaxZ := MyData.FindFieldMax('MAX_Z');
       MyData.Destroy;
   end
   else begin
      {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('single file'); {$EndIf}
      LoadOneFile(fName,'single file');
   end;
   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.LoadFromShapeFile out'); {$EndIf}
end;


function tMemoryPointCloud.PlotClassification(i : integer; var RGBColor : TRGBTriple) : boolean;
begin
   Result := true;
   RGBColor := Las_rgb_colors[PtClass^[i]];
end;


procedure tMemoryPointCloud.PlotSlice(Cloud : integer; var BMPMemory : tBMPMemory; TheGraph : tThisBaseGraph; c1,c2,c3 : integer; LowLimit,HighLimit : float64; var NPts : integer; ThreadTimer : integer = 0);
var
   i : int64;
   xi,yi,Return : integer;
   sl,xutm,yutm : float64;
   bmp2 : tMyBitmap;
   Color : tPlatFormColor;
   p2 : tBMPMemory;

         function SliceCoord(i,axis : integer) : float64;
         begin
            if axis in [1,2] then Result := XYPts^[i,axis]
            else Result := ZPts^[i];
         end;


begin
  {$IfDef RecordPointCloudMemory} WriteLinetoDebugFile('tMemoryPointCloud.PlotSlice in'); {$EndIf}
   if (MDDef.SliceColorOpt = scoClass) and (PtClass = Nil) then exit;
   if (MDDef.SliceColorOpt = scoRetNum) and (PtReturnNumber = Nil) then exit;
   if (MDDef.SliceColorOpt = scoRGB) and (PtRGB = Nil) then exit;

   if (MapOwner <> Nil) then begin
      MapOwner.DoFastMapRedraw;
      CloneImageToBitmap(MapOwner.Image1,bmp2);
      p2 := tBMPMemory.Create(bmp2);
   end;
   Npts := 0;
   i := 1;
   Color := MDDef.CloudSymbol[Cloud].Color;
   if (MDDef.SliceColorOpt = scoGray) then Color := ConvertTColorToPlatformColor(clSilver);
   while (i <= NumMemPts) do begin
       if (pred(i) mod 10000 = 0) and (ThreadTimer > 0) and (ThreadTimers <> Nil) then ThreadTimers.UpdateThreadStats(ThreadTimer,round(100*i/NumMemPts));
       sl := SliceCoord(i,c3);
       if (sl >= LowLimit) and (sl <= HighLimit) then begin
          xi := TheGraph.GraphDraw.GraphX(SliceCoord(i,c1));
          yi := TheGraph.GraphDraw.GraphY(SliceCoord(i,c2));
          if TheGraph.GraphDraw.PtOnGraph(xi,yi) then begin
             if (MDDef.SliceColorOpt = scoClass) then Color := Las_rgb_colors[PtClass^[i]];
             if (MDDef.SliceColorOpt = scoRetNum) then begin
                Return := PtReturnNumber^[i];
                if (Return > MaxReturns) then Return := MaxReturns;
                Color := Las_ret_colors[Return];
             end;
             if (MDDef.SliceColorOpt = scoRGB) then Color := PtRGB^[i];
             BMPMemory.SetPixelColorSize(xi,yi,MDDef.CloudSymbol[Cloud].Size,Color);
             inc(NPts);

             if (MapOwner <> Nil) then begin
                xutm := XYPts^[i,1];
                yutm := XYPts^[i,2];
                MapOwner.MapDraw.UTMToScreen(xutm,yutm,xi,yi);
                p2.SetPixelColorSize(xi,yi,MDDef.CloudSymbol[Cloud].Size,claBlack);
             end;
          end;
       end;
       inc(i, MDDef.CloudSliceThinFactor);
    end;

   if (MapOwner <> Nil) then begin
      BMP2.SaveToFile(MDTempDir + 'slice_coverage.bmp');
      MapOwner.IHSmergeOntoMap(Bmp2);
      bmp2.Free;
   end;

   {$IfDef RecordPointCloudmemory} WriteLinetoDebugFile('tMemoryPointCloud.PlotSlice out'); {$EndIf}
end;


initialization
finalization
end.
