unit veg_density;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   unit created 3/14/2012        }
{   file verified 5/13/2018       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface


uses
   SysUtils,Graphics,Classes, StdCtrls,Windows,
   DEMMapf,DEMDefs,BaseGraf,
   Petmar_types;

type
   tVegDensity = class
      private
         function RandomHalfHorizontal(x : float64) : float64;   inline;
         function RandomHorizontal(x : float64) : float64;   inline;
         function RandomVertical(z : float64) : float64;   inline;
      public
         VegDensityName : shortstring;
         VegLayers : array[1..MaxVegLayers] of integer;
         VegLayerFiles : array[1..MaxVegLayers] of PathStr;
         DTMused,
         LayersPresent,
         MaxDensity    : integer;
         constructor Create(var fileName : PathStr; inDTM : integer; DoAsk : boolean = true);
         destructor Destroy;
         procedure OverlayVegLayerOnMap(Map : tMapForm; LayerNum : integer; LabelLayer : boolean = false);
         procedure MakeMovie(Map : tMapForm);
         procedure FindPowerLines(Map : tMapForm; Memo1 : tMemo);
         procedure ClearVegLayers;
         procedure GetStats;
         procedure ShowGraphAtPoint(VegGraph: tThisBaseGraph; Lat,Long,xgrid, ygrid: float64);
         procedure ExportToLAS(lox, loy, hix, hiy: integer; var FName: PathStr);
   end;
   pVegDensity = ^tVegDensity;



implementation


uses
   DEMCoord,Petmar,PetMath,PetImage,
   //Read_dem,
   DEMMapDraw,
   DEM_Manager,
   point_cloud_options,las_lidar;


{ tVegDensity }


procedure tVegDensity.ShowGraphAtPoint(VegGraph : tThisBaseGraph; Lat,Long,xgrid,ygrid : float64);
var
   j,k,x,y,xg,yg : integer;
   z : float32;
   TStr : shortstring;

   procedure AProfile(j,k : integer);
   var
      i : integer;
   begin
      for i := 1 to MaxVegLayers do begin
         if (VegLayers[i] <> 0) then begin
            y := VegGraph.GraphDraw.GraphY(i);
            if (not DEMGlb[VegLayers[i]].GetElevMeters(xg+j,yg+k,z)) then z := 0;
            x := VegGraph.GraphDraw.GraphX(z);
            if i = 1 then VegGraph.Image1.Canvas.MoveTo(x,y)
            else VegGraph.Image1.Canvas.LineTo(x,y);
         end;
      end;
   end;


   procedure AverageProfile;
   var
      i,j,k : integer;
      sum : float64;
   begin
      for i := 1 to MaxVegLayers do begin
         if (VegLayers[i] <> 0) then begin
             Sum := 0;
             for j := -1 to 1 do begin
                for k := -1 to 1 do begin
                   if DEMGlb[VegLayers[i]].GetElevMeters(xg+j,yg+k,z) then Sum := Sum + z;
                end;
             end;
            x := VegGraph.GraphDraw.GraphX(sum / 9);
            y := VegGraph.GraphDraw.GraphY(i);
            if (i = 1) then VegGraph.Image1.Canvas.MoveTo(x,y)
            else VegGraph.Image1.Canvas.LineTo(x,y);
         end {if};
      end {for};
   end;



begin
   VegGraph.RedrawDiagram11Click(Nil);
   TStr :=  LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
   VegGraph.Caption  := ' Veg profile ' + Tstr;
   VegGraph.Image1.Canvas.TextOut(VegGraph.GraphDraw.LeftMargin+3,5,TStr);
   xg := round(Xgrid);
   yg := round(Ygrid);

   if MDDef.VegDensityGraphAverage then begin
       VegGraph.Image1.Canvas.Pen.Color := clLime;
       VegGraph.Image1.Canvas.Pen.Width := 3;
       AverageProfile;
   end
   else begin
      VegGraph.Image1.Canvas.Pen.Color := clGreen;
      VegGraph.Image1.Canvas.Pen.Width := 1;
       for j := -1 to 1 do begin
          for k := -1 to 1 do begin
             if (j <> 0) and (k <> 0 ) then begin
                AProfile(j,k);
             end;
          end;
       end;
       VegGraph.Image1.Canvas.Pen.Color := clLime;
       VegGraph.Image1.Canvas.Pen.Width := 3;
       AProfile(0,0);
   end;
end;


procedure tVegDensity.ClearVegLayers;
var
   i : integer;
begin
   for i := 1 to MaxVegLayers do begin
      if VegLayerFiles[i] <> '' then begin
         SysUtils.DeleteFile(VegLayerFiles[i]);
         VegLayerFiles[i] := '';
      end;
   end;
end;

constructor tVegDensity.Create;
var
   BaseDir : PathStr;
   i : integer;
begin
   LayersPresent := 0;
   MaxDensity := 0;

   BaseDir := ExtractFilepath(FileName);
   if DoAsk then GetDOSPath(' vegetation density grids',BaseDir);

   VegDensityName := LastSubDir(BaseDir);
   DTMused := inDTM;

   StartProgress('Veg layers');
   for i := 1 to MaxVegLayers do begin
      UpdateProgressBar(i/MaxVegLayers);
      VegLayers[i] := 0;
      VegLayerFiles[i] := '';
      FileName := BaseDir + 'las_density_' + IntToStr(i) + '.dem';
      if FileExists(FileName) then begin
         inc(LayersPresent);
         ShowDEMReadingProgress := false;
         LoadNewDEM(VegLayers[i],FileName,False);
         ShowDEMReadingProgress := false;
         DEMGlb[VegLayers[i]].HiddenGrid := true;
         if (DEMGlb[VegLayers[i]].DEMheader.MaxElev > MaxDensity) then MaxDensity := round(DEMGlb[VegLayers[i]].DEMheader.MaxElev);
         if not DEMGlb[DTMused].SecondGridIdentical(VegLayers[i]) then begin
             EndProgress;
             MessageToContinue('Incompatible density grids');
             LayersPresent := 0;
             exit;
         end;
      end;
   end;
   for i := 1 to MaxVegLayers do if VegLayers[i] <> 0 then DEMGlb[VegLayers[i]].DEMheader.MaxElev := MaxDensity;
   Filename := BaseDir;
   EndProgress;
end;


procedure tVegDensity.GetStats;
var
   fName : PathStr;
   i : integer;
   Findings : tStringList;
   NPts : integer;
   Sum,OldMax,TPts : float64;
begin
   StartProgress('Veg stats');
   Findings := tStringList.Create;
   Findings.Add('VEG_LAYER,POINTS,RETURNS,MAX_DENSTY,PC_COVER');
   for i := MaxVegLayers downto 1 do begin
      UpdateProgressBar( (MaxVegLayers-i) / MaxVegLayers);
      if VegLayers[i] <> 0 then begin
         DEMGlb[VegLayers[i]].GetDEMPointsSum(NPts,Sum);
         OldMax := DEMGlb[VegLayers[i]].DEMheader.MaxElev;
         DEMGlb[VegLayers[i]].CheckMaxMinElev;
         TPts := DEMGlb[VegLayers[i]].DEMheader.NumCol * DEMGlb[VegLayers[i]].DEMheader.NumRow;
         if (Npts > 0) then Findings.Add(IntToStr(i) + ',' + IntToStr(NPts) + ',' + IntToStr(Round(Sum)) + ','
             + IntToStr(round(DEMGlb[VegLayers[i]].DEMheader.MaxElev)) + ',' + RealToString(100 * NPts / TPts,-12,2));
         DEMGlb[VegLayers[i]].DEMheader.MaxElev := OldMax;
      end;
   end;
   EndProgress;
   fName := MDTempDir + 'veg_density_stats.csv';
   DEMGlb[DTMUsed].SelectionMap.DisplayAndPurgeStringListDB(Findings,fName);
end;


destructor tVegDensity.Destroy;
var
   i : integer;
begin
    inherited;
    ClearVegLayers;
    for i := 1 to MaxVegLayers do CloseSingleDEM(VegLayers[i]);
end;


procedure tVegDensity.FindPowerLines(Map: tMapForm; Memo1 : tMemo);
var
   OccupiedLayersDEM,NewDEM,x,y,xp,yp,OccupiedVoxel,NumOccupiedVoxels,NumOccupiedNeighbors,NumOccupiedSecondNeighbors : integer;
   NewHeadRecs : tDEMheader;
   loc : byte;


   procedure PlotPoint(x,y : integer; aColor : tPlatformColor);
   begin
      with Map,MapDraw do begin
         DEMGridToScreen(x,y,xp,yp);
         if OnScreen(xp,yp) then ScreenSymbol(Image1.Canvas,xp,yp,FilledBox,2,aColor);
      end;
   end;


   function LayersOccupied(x,y : integer) : byte;
   var
      z : integer;
      zf : float32;
   begin
       Result := 0;
       for z := 1 to LayersPresent do begin
           if DEMGlb[VegLayers[z]].GetElevMeters(x,y,zf) and (zf  > 0) then begin
              inc(Result);
           end;
       end;
   end;


   function ThisOccupiedVoxel(x,y : integer) : boolean;
   var
      z : integer;
      zf : float32;
   begin
       Result := false;
       if (x < 0) or (y < 0) or (x >= DEMGlb[Map.MapDraw.DEMonMap].DEMheader.NumCol) or (y >= DEMGlb[Map.MapDraw.DEMonMap].DEMheader.NumRow) then exit;
       for z := MDDef.PL_FirstVacantVoxel to MDDef.PL_LastVacantVoxel do begin
          if DEMGlb[VegLayers[z]].GetElevMeters(x,y,zf) and (zf  > 0) then exit;
       end;
       OccupiedVoxel := 0;
       for z := succ(MDDef.PL_LastVacantVoxel) to LayersPresent do begin
           if DEMGlb[VegLayers[z]].GetElevMeters(x,y,zf) and (zf  > 0) then begin
              inc(OccupiedVoxel);
           end;
       end;
       Result := true;   //(OccupiedVoxel > 0);
   end;


   function OccupiedNeighbors(x,y : integer) : boolean;
   var
      xi,yi,{z,}Neighbors,SecondNeighbors  : integer;
   begin
      Neighbors := 0;
      Result := false;
      for xi := -1 to 1 do begin
          for yi := -1 to 1 do begin
              if (xi <> 0) and (yi <> 0) then begin
                 if ThisOccupiedVoxel(x+xi,y+yi) then inc(Neighbors);
                 if (Neighbors > MDDef.PL_NeighborsRequired) then exit;
              end;
          end;
      end;
      if (Neighbors < MDDef.PL_NeighborsRequired) then exit;
      if MDDef.PL_PlotFirstNeighbor then PlotPoint(x,y,claBlue);
      inc(NumOccupiedNeighbors);

      SecondNeighbors := 0;
      for xi := -2 to 2 do begin
          for yi := -2 to 2 do begin
              if (xi = 2) or (yi = 2) then begin
                 if ThisOccupiedVoxel(x+xi,y+yi) then inc(SecondNeighbors);
                 if (SecondNeighbors > MDDef.PL_NeighborsRequired) then exit;
              end;
          end;
      end;
      Result := (SecondNeighbors = MDDef.PL_NeighborsRequired);
   end;


begin
   with Map,MapDraw,DEMGlb[DEMonMap] do begin
       StartProgress('Wires');
       NewHeadRecs := DEMGlb[DEMonMap].DEMheader;
       NewHeadRecs.DEMPrecision := ByteDEM;
       NewHeadRecs.ElevUnits := Undefined;
       OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,'Occupied_layers_above_clear_zone',true);
       OpenAndZeroNewDEM(true,NewHeadRecs,OccupiedLayersDEM,'Occupied_veg_layers',true);

       NumOccupiedVoxels := 0;
       NumOccupiedNeighbors := 0;
       NumOccupiedSecondNeighbors := 0;
       for x := 0 to pred(DEMheader.NumCol) do begin
          if (x mod 25 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
          for y := 0 to pred(DEMheader.NumRow) do begin
              loc := LayersOccupied(x,y);
              if (loc > 0) then DEMGlb[OccupiedLayersDEM].SetGridElevation(x,y, loc);

              if ThisOccupiedVoxel(x,y) then begin
                 if (OccupiedVoxel > 0) then DEMGlb[NewDEM].SetGridElevation(x,y,OccupiedVoxel);
                 if (OccupiedVoxel <= MDDef.PL_MaxOccupiedVoxels) then begin
                    if MDDef.PL_PlotOccupied then PlotPoint(x,y,claLime);
                    inc(NumOccupiedVoxels);
                    if OccupiedNeighbors(x,y) then begin
                       if MDDef.PL_PlotSecondNeighbor then PlotPoint(x,y,claRed);
                       inc(NumOccupiedSecondNeighbors);
                    end;
                 end;
              end;
          end;
       end;
       EndProgress;
       SetUpMap(NewDEM,true,mtElevSpectrum,DEMGlb[NewDEM].AreaName);
       SetUpMap(OccupiedLayersDEM,true,mtElevSpectrum,DEMGlb[OccupiedLayersDEM].AreaName);

       if (Memo1 <> Nil) then begin
          Memo1.Lines.Add('Occupied voxel: ' + IntToStr(NumOccupiedVoxels));
          Memo1.Lines.Add('Occupied neighbors: ' + IntToStr(NumOccupiedNeighbors));
          Memo1.Lines.Add('Occupied 2nd neighbors: ' + IntToStr(NumOccupiedSecondNeighbors))
       end;
   end;
end;


procedure tVegDensity.MakeMovie(Map: tMapForm);
{$IfDef ExMovies}
begin
{$Else}
var
   MovieList : tStringList;
   FName     : PathStr;
   i : integer;
begin
   MovieList := TStringList.Create;
   StartProgress('Movie');
   for i := 1 to MaxVegLayers do begin
      UpdateProgressBar(i/MaxVegLayers);
      if (VegLayers[i] <> 0) then begin
         Map.DoFastMapRedraw;
         OverlayVegLayerOnMap(Map,i,true);
         FName := DEMdefs.MovieDir + 'veg_layer_' + intToStr(i) + MovieFileExt;
         PetImage.SaveImageAsBMP(Map.Image1,FName);
         if (VegLayerFiles[i] <> '') then MovieList.Add(ExtractFileName(FName));
      end;
   end;
   EndProgress;
   FName := DEMdefs.MovieDir + 'veg_density.mov';
   MovieList.SaveToFile(FName);
   MovieList.Free;
   PetImage.MakeMovie(Fname);
{$EndIf}
end;


procedure tVegDensity.OverlayVegLayerOnMap(Map : tMapForm; LayerNum: integer; LabelLayer : boolean = false);
var
   RealDEM : integer;
   NewBitmap  : tMyBitmap;
   OldMapType : tMapType;
begin
   if (VegLayers[LayerNum] <> 0) then with Map,MapDraw do begin
      if (VegLayerFiles[LayerNum] = '') then begin
         RealDEM := DEMonMap;
         CloneImageToBitmap(Map.Image1,NewBitmap);
         OldMapType := MapType;
         MapType := mtElevRainbow;
         DEMonMap := VegLayers[LayerNum];
         MaxMapElev := DEMGlb[DEMonMap].DEMheader.MaxElev;
         MinMapElev := DEMGlb[DEMonMap].DEMheader.MinElev;
         ShowSatProgress := false;
         ColorTintedElevationMap(NewBitMap);
         ShowSatProgress := true;
         VegLayerFiles[LayerNum] := MDTempDir + 'veg_layer_' + IntToStr(LayerNum) + OverlayFExt;
         if LabelLayer then begin
            NewBitmap.Canvas.Font.Size := 18;
            NewBitmap.Canvas.Font.Style := [fsBold];
            NewBitmap.Canvas.Font.Color := clLime;
         end;
         NewBitmap.SaveToFile(VegLayerFiles[LayerNum]);
         DEMonMap := RealDEM;
         MapType := OldmapType;
         MaxMapElev := DEMGlb[DEMonMap].DEMheader.MaxElev;
         MinMapElev := DEMGlb[DEMonMap].DEMheader.MinElev;
      end
      else NewBitmap := LoadBitmapFromFile(VegLayerFiles[LayerNum]);

      NewBitmap.Canvas.TextOut(0,0,'z=' + IntToStr(LayerNum));
      Map.IHSmergeOntoMap(NewBitmap);
      NewBitmap.Free;
   end;
end;


function tVegDensity.RandomHorizontal(x: float64): float64;
begin
   Result := x + (random -0.5) * MDDef.VegGridRandomizationDistance;
end;

function tVegDensity.RandomHalfHorizontal(x: float64): float64;
begin
   Result := x + (random - 0.25) * MDDef.VegGridRandomizationDistance;
end;


function tVegDensity.RandomVertical(z: float64): float64;
begin
   Result := z + (random - 0.25) * MDDef.VegGridRandomizationDistance;
end;


procedure tVegDensity.ExportToLAS(lox,loy,hix,hiy : integer; var FName : PathStr);
var
  xf,yf : float64;
  z,ze : float32;
  i,col,row,j,DEM : integer;
  NewLas : tCreateLasFile;
  lf : tLAS_data;
  lp0 : tLidarPointType0;
begin
  if (pt_cloud_opts_fm = Nil) or (pt_cloud_opts_fm.LasFiles[1] = nil) then begin
     MessageToContinue('LAS files must be loaded for projection info');
     exit;
  end;

  lp0.Intensity := 0;
  lp0.ReturnFlags := 1 + 8;  //return number in bits 0-2, the number of returns from pulse in bits 3-5
  lp0.ScanAngle := 0;
  lp0.UserData := 77;
  lp0.PointSourceID := 0;

  lf := tLAS_data.Create(pt_cloud_opts_fm.LasFiles[1].LAS_fnames.Strings[0]);
  NewLas := tCreateLasFile.Create;
  NewLas.NewLasHeader := lf.LasHeader;
  NewLAS.CreateNewLASfile(fName,lf.lasProjectionDefinition,lf.LASHeader);
  //NewLas.InitializeOutputBuffer;
  lf.Destroy;

  with NewLas.NewLasHeader do begin
     for col := LoX to HiX do begin
        for row := LoY to HiY do begin
           if DEMGlb[DTMused].GetElevMeters(Col,Row,ze) then begin
              DEMGlb[DTMused].DEMGridtoUTM(Col,Row,xf,yf);

              if MDDef.VegDensityGroundPoints then begin
                 lp0.Classification := 2;
                 lp0.xPt := round(RandomHalfHorizontal(xf) / xScaleFac - xoffset);
                 lp0.yPt := round(RandomHalfHorizontal(yf) / yScaleFac - yoffset);
                 lp0.zPt := round(ze / zScaleFac - zoffset);
                 NewLas.AddShotToOutputBuffer(lp0);
              end;

              lp0.Classification := 4;
              for DEM := 1 to MaxVegLayers do begin
                 i := VegLayers[DEM];
                 if (i <> 0) then begin
                    if DEMGlb[i].GetElevMeters(Col,Row,z) then begin
                      for j := 1 to round(z) do begin
                         lp0.xPt := round(RandomHorizontal(xf) / xScaleFac - xoffset);
                         lp0.yPt := round(RandomHorizontal(yf) / yScaleFac - yoffset);
                         lp0.zPt := round(RandomVertical(ze + DEM) / zScaleFac - zoffset);
                         NewLas.AddShotToOutputBuffer(lp0);
                      end;
                    end;
                 end;
              end;
          end {for Row};
       end {for Col};
     end {with};
     NewLas.Destroy;
  end;
end;


initialization
finalization
end.


