{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demcoord         }
{___________________________________}


{$IfDef ExVegDensity}
{$Else}

procedure tDEMDataSet.OpenVegGrid(var fName : PathStr; Which : integer = 1);
var
   NewHeadRecs : tDEMHeader;
   OldVeg      : integer;
begin
   {$IfDef RecordVegGrid} WriteLineToDebugFile('tDEMDataSet.OpenVegGrid open');  {$EndIf}
   if (VegGrid[Which] <> 0) then begin
      {$IfDef RecordVegGrid} WriteLineToDebugFile('tDEMDataSet.OpenVegGrid close old veg grid');   {$EndIf}
      if (fName = DEMGlb[VegGrid[Which]].DEMFileName) then exit
      else CloseVegGrid(which);
   end;
   if (UpperCase(fName) = DEMFileName) then fName := extractFilePath(fName);

   if LoadNewDEM(VegGrid[Which],fName,MDDef.OpenVegGridMap) then begin
      if not SecondGridIdentical(VegGrid[Which]) then begin
         {$IfDef RecordVegGrid} WriteLineToDebugFile('VegGrid not identical'); {$EndIf}
         if AnswerIsYes('Not compatible grid; convert it now') then begin
            {$IfDef RecordVegGrid} WriteLineToDebugFile('User wants to create new grid'); {$EndIf}
            OldVeg := VegGrid[Which];
            NewHeadRecs := DEMheader;
            NewHeadRecs.DEMPrecision := FloatingPointDEM;

            OpenAndZeroNewDEM(true,NewHeadRecs,VegGrid[1],'',InitDEMmissing);
            DEMGlb[VegGrid[1]].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[VegGrid[1]].FullDEMGridLimits,OldVeg,hfEverything);
            fName := ExtractFilePath(fName) + 'New_veg.dem';
            if GetFileNameDefaultExt('new vegetation grid','DEM|*.DEM',FName) then begin
               {$IfDef RecordVegGrid} WriteLineToDebugFile('Save new grid'); {$EndIf}
               DEMGlb[VegGrid[Which]].WriteNewFormatDEM(fName);
            end;
            CloseSingleDEM(OldVeg);
         end
         else begin
            {$IfDef RecordVegGrid} WriteLineToDebugFile('User punting veg grid'); {$EndIf}
            CloseVegGrid(which);
            LastDEMName := '';
         end;
      end;
   end;
   {$IfDef RecordVegGrid} WriteLineToDebugFile('tDEMDataSet.OpenVegGrid open'); {$EndIf}
end;


procedure tDEMDataSet.OpenDSMGrid;
var
   NewHeadRecs : tDEMHeader;
   x,y      : integer;
   z,z2 : float32;
begin
   if (VegGrid[1] <> 0) and (DSMGrid = 0) then begin
      NewHeadRecs := DEMheader;
      NewHeadRecs.DEMPrecision := FloatingPointDEM;
      OpenAndZeroNewDEM(true,NewHeadRecs,DSMGrid,'DSM for ' + AreaName,InitDEMmissing);
      for x := 0 to pred(DEMheader.NumCol) do begin
         for y := 0 to pred(DEMheader.NumRow) do begin
            if GetElevMeters(x,y,z) and DEMGlb[VegGrid[1]].GetElevMeters(x,y,z2) then
               DEMGlb[DSMGrid].SetGridElevation(x,y,z+z2);
         end;
      end;
   end;
end;


procedure tDEMDataSet.CloseVegGrid;
begin
   If (which in [0,1]) and (VegGrid[1] <> 0) then begin
      {$IfDef RecordVegGrid} WriteLineToDebugFile('tDEMDataSet.CloseVegGrid calling to close VegGrid ' + IntToStr(VegGrid[1])); {$EndIf}
      CloseSingleDEM(VegGrid[1]);
      VegGrid[1] := 0;
   end;
   If (which in [0,2]) and (VegGrid[2] <> 0) then begin
      CloseSingleDEM(VegGrid[2]);
      VegGrid[2] := 0;
   end;
   If (DSMGrid <> 0) then CloseSingleDEM(DSMGrid);
   DSMGrid := 0;
end;


procedure tDEMDataSet.GetJustVegHeight(xgrid,ygrid : float32; var veg_ht : float32);
var
   veg : float32;
begin
   veg_ht := 0;
   {$IfDef ExNLCD}
   {$Else}
   if LandCoverGrid then begin
      if GetElevMeters(xgrid,ygrid,veg) and (veg > 0) and (veg < MaxLandCoverCategories) then
         veg_ht := NLCDCats^[round(veg)].Height;
   end
   else {$EndIf}
   begin
      GetElevMeters(xgrid,ygrid,veg_ht);
   end;
end;

function tDEMDataSet.GetElevMetersWithVeg(x,y : float64; var z : float32) : boolean;
var
   veg_ht : float32;
begin
   if GetElevMeters(x,y,z) then begin
      Result := true;
      if (VegGrid[1] <> 0) and (DEMGlb[VegGrid[1]] <> Nil) then begin
          DEMGlb[VegGrid[1]].GetJustVegHeight(x,y,veg_ht);
          z := z + veg_ht;
      end;
   end
   else Result := false;
end;


function tDEMDataSet.GetElevMetersWithVegFromLatLong(Lat,Long : float64; var z : float32) : boolean;
var
   x,y : float64;
begin
    LatLongDegreeToDEMGrid(Lat,Long,x,y);
    Result := GetElevMetersWithVeg(x,y,z);
end;

{$EndIf}

