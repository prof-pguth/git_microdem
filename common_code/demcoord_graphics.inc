{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{     include file for demcoord     }
{___________________________________}



procedure tDEMDataSet.DrawCrossTrackProfile(var Graf : tThisBaseGraph; inLat,inLong,Azimuth,Distance,Spacing : float64);
var
   NPts   : integer;
   Lat,Long : float64;
   z : float32;
   rFile : file;
   v : tFloatPoint;
   Lats,Longs,dists : Petmath.bfarray32;


       procedure DoFile(Color : tColor; UseVeg : boolean);

           procedure CheckZ(i : integer; Mult : integer);
           var
              ValidPt : boolean;
           begin
               {$IfDef ExVegDensity}
                  ValidPt := GetElevFromLatLongDegree(Lats[i],Longs[i],z);
               {$Else}
                  if UseVeg then ValidPt := GetElevMetersWithVegFromLatLong(Lats[i],Longs[i],z)
                  else ValidPt := GetElevFromLatLongDegree(Lats[i],Longs[i],z);
               {$EndIf}
               if ValidPt then begin
                   v[1] := Mult * Dists[i];
                   v[2] := z;
                   Graf.AddPointToDataBuffer(rfile,v);
               end;
           end;

       var
          i : integer;
       begin
           Graf.OpenPointFile(rfile,Graf.Symbol);
           NPts := 500;
           VincentyPointAtDistanceBearing(inLat,inLong,Distance,Azimuth+90,Lat,Long);
           GetStraightRouteLatLongDegree(inLat,inLong,Lat,Long,saSmart,NPts,Lats,Longs,dists);
           for I := 500 downto 0 do CheckZ(i,1);
           VincentyPointAtDistanceBearing(inLat,inLong,Distance,Azimuth-90,Lat,Long);
           GetStraightRouteLatLongDegree(inLat,inLong,Lat,Long,saSmart,NPts,Lats,Longs,dists);
           for I := 1 to 500 do CheckZ(i,-1);
           Graf.ClosePointDataFile(rfile);
       end;

begin
     Graf.GraphDraw.DataFilesPlotted.Clear;
     DoFile(clBrown,false);
     Graf.GraphDraw.LineSize256[1] := 2;
     Graf.GraphDraw.FileColors256[1] := ConvertTColorToPLatformColor(clBrown);
     Graf.GraphDraw.Symbol[1].Size := 0;

   {$IFDef ExVegDensity}
   {$Else}
     if (VegGrid[1] <> 0) then begin
        DoFile(clLime,true);
        Graf.GraphDraw.LineSize256[2] := 1;
        Graf.GraphDraw.FileColors256[2] := claLime;
        Graf.GraphDraw.Symbol[2].Size := 0;
     end;
     {$EndIf}

     Graf.RedrawDiagram11Click(Nil);
end;


function tDEMDataSet.LoadColorVATTable(var Colors : tVATColors) : boolean;
var
   Table : tMyData;
   fName,CodeField : PathStr;
   fp : boolean;
   i : integer;
begin
   Result := false;
   if (VATFileName = '') then fName := ChangeFileExt(DEMFileName,'.vat.dbf')
   else fName := VATFileName;
   if not FileExists(fName) then begin
      {$IfDef RecordVAT} WriteLineToDebugFile('tDEMDataSet.LoadColorVATTable, could not find VAT ' + fName); {$EndIf}
      exit;
   end;

   {$IfDef RecordVAT} WriteLineToDebugFile('tDEMDataSet.LoadColorVATTable found=' + fName); {$EndIf}
   VATFileName := fName;
   Table := tMyData.Create(fName);
   if (Table.NCountField = '') then begin
      Table.Destroy;
      CreateVATforDEM;
      Table := tMyData.Create(fName);
   end;
   if Table.FieldExists('CODE') then CodeField := 'CODE' else CodeField := 'VALUE';

   if Table.FieldExists('RED') or Table.FieldExists('COLOR') then begin
       fp := Table.IsFloatField('RED');
       Table.First;
       while not Table.eof do begin
          i := Table.GetFieldByNameAsInteger(CodeField);
          if (i >= 0) and (i <= MaxVatCats) then begin
             Colors[i] := Table.PlatformColorFromTable;
          end;
          Table.Next;
       end;
       Result := true;
   end;
   Table.Destroy;
end;


procedure tDEMDataSet.ExpandSpecifiedZRange;
var
   x,y       : integer;
   NewzLo,NewzHi,
   zHi,zLo,z,nz : float32;
begin
    zLo := DEMheader.MinElev;
    zHi := DEMheader.MaxElev;
    NewzLo := DEMheader.MinElev;
    NewZHi := DEMheader.MaxElev;
    repeat
       ReadDefault('Lower limit of range to expand',zLo);
       ReadDefault('Upper limit of range to expand',zHi);
       ReadDefault('Lower limit of expanded range',NewzLo);
       ReadDefault('Upper limit of expanded range',NewzHi);
    until AnswerIsYes('Expand ' + RealToString(zLo,-12,-2) + ' to ' + RealToString(NewzLo,-12,-2) + MessLineBreak + ' and ' + RealToString(zHi,-12,-2) + ' to ' + RealToString(NewZHi,-12,-2) );

    StartProgress('Check DEM');
    for x := 0 to pred(DEMheader.NumCol) do begin
       for y := 0 to pred(DEMheader.NumRow) do begin
          if GetElevMeters(x,y,z) then begin
             if z < zLo then z := NewZLo
             else if z > zHi then z := NewZHi
             else nz := NewZLo + (z - zlo) * (NewZHi - NewZLo) / (zHi - zLo);
             SetGridElevation(x,y,nz);
          end;
       end;
       if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
    end;
    CheckMaxMinElev;
    CloseElevPercentiles;  //because they have been changed
    EndProgress;
end;

procedure tDEMDataSet.RoundZRangeByPercentileToByte;
var
   x,y   : integer;
   NewzLo,NewzHi,
   zHi,zLo,z,nz : float32;
begin
    zLo := 1;
    zHi := 99;
    NewzLo := 0;
    NewZHi := 254;
    ReadDefault('Low percentile to set to 0',zLo);
    ReadDefault('Upper percentile to set to 254',zHi);

    zLo := FindPercentileElevation(zLo);
    zHi := FindPercentileElevation(zHi);

    StartProgress('Check DEM');
    for x := 0 to pred(DEMheader.NumCol) do begin
       for y := 0 to pred(DEMheader.NumRow) do begin
          if GetElevMeters(x,y,z) then begin
             if z < zLo then nz := NewZLo
             else if z > zHi then nz := NewZHi
             else nz := NewZLo + (NewZHi - NewZLo) * (z - zlo) / (zHi - zLo);
             SetGridElevation(x,y,nz);
          end;
       end;
       if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
    end;
    CheckMaxMinElev;
    CloseElevPercentiles;  //because they have been changed
    EndProgress;
end;



procedure tDEMDataSet.VATLegend;
var
   db : integer;
begin
   if FileExists(VATFileName) then begin
      db := SelectionMap.OpenDBonMap('',VATFileName,false,false);
      GISdb[db].CreatePopupLegend;
   end;
end;


procedure tDEMDataSet.CreateVATforDEM(OptionToCopy : boolean = true);
var
   fName,fName2 : PathStr;
   Table : tMyData;
   aColor : tColor;
   NewFile : boolean;
   CodeField,NameField,
   CountFieldName : ShortString;
   i,x,y,z,NumOutOfRange,Max : integer;
   zf : float32;
   Hist : array[MinVATValue..MaxVATCats] of int64;

        procedure InsertCategory(x : integer);
        begin
            Table.Insert;
            Table.SetFieldByNameAsInteger(CountFieldName,Hist[x]);
            Table.SetFieldByNameAsInteger(CodeField,x);
            Table.SetFieldByNameAsString('NAME','Category ' + IntToStr(x));
            Table.SetFieldByNameAsInteger('COLOR',-1);
            Table.Post;
        end;

begin
  {$IfDef RecordVAT} WriteLineToDebugFile('tDEMDataSet.CreateVATforDEM, in ' + DEMFileName); {$EndIf}
   if (DEMFileName = '') then begin
      WriteNewFormatDEM(DEMFileName,' save DEM before vat creation');
   end;
  fName := DEMFileName + '.vat.dbf';
  if not FileExists(fName) then fName := ChangeFileExt(DEMFileName,'.vat.dbf');
  NewFile := false;
  if FileExists(fName) then begin
     VATFileName := fName;
     Table := tMyData.Create(fName);
     {$IfDef RecordVAT} WriteLineToDebugFile('tDEMDataSet.CreateVATforDEM Found VAT, ' + fName + '  recs=' + IntToStr(Table.RecordCount)); {$EndIf}

     VatLegendStrings := tStringList.Create;
     if Table.FieldExists('CODE') then CodeField := 'CODE' else CodeField := 'VALUE';
     NameField := Table.AssignLabelName;
     Max := 0;
     while not Table.eof do begin
        i := Table.GetFieldByNameAsInteger(CodeField);
        if (I > Max) then Max := i;
        Table.Next;
     end;
     for i := 0 to Max do VatLegendStrings.Add('');
     Table.First;
     while not Table.eof do begin
        i := Table.GetFieldByNameAsInteger(CodeField);
        VatLegendStrings.Strings[i] := Table.GetFieldByNameAsString(NameField);
        Table.Next;
     end;

     CountFieldName := Table.NCountField;
     if (CountFieldName = '') then begin
        CountFieldName := 'COUNT';
        Table.InsureFieldPresentAndAdded(ftInteger,CountFieldName,12);
        NewFile := true;
     end
     else begin
       Table.Destroy;
       exit;
     end;
  end
  else begin
     fName2 := LastDEMname;
     if OptionToCopy and AnswerIsYes('Copy VAT template from another grid with same categories') and Petmar.GetFileFromDirectory('VAT template',DefaultDBMask,fname2) then begin
        {$IfDef RecordVAT} WriteLineToDebugFile('copy VAT in ' + fName2); {$EndIf}
        CopyFile(fName2,fname);
     end
     else begin
        {$IfDef RecordVAT} WriteLineToDebugFile('create new VAT'); {$EndIf}
        NewFile := true;
        Make_tables.CreateTableForVATforDEM(fName);
     end;
     Table := tMyData.Create(fName);
     if Table.FieldExists('CODE') then CodeField := 'CODE' else CodeField := 'VALUE';
     CountFieldName := Table.NCountField;
  end;

   Table.ApplyFilter(CountFieldName + '>0');
   if (Table.RecordCount = 0) then begin
      NumOutOfRange := 0;
      for x := MinVATValue to MaxVATCats do Hist[x] := 0;
      {$IfDef RecordVAT} WriteLineToDebugFile('fill histogram'); {$EndIf}
      for y := 0 to pred(DEMheader.NumRow) do begin
         for x := 0 to pred(DEMheader.NumCol) do begin
            if GetElevMetersOnGrid(x,y,zf) then begin
               z := round(zf);
               if (z >= MinVATValue) and (z <= MaxVATCats) then begin
                  Inc(Hist[z]);
               end
               else inc(NumOutOfRange);
            end;
         end;
      end;
      if (NumOutOfRange > 0) then MessageToContinue('Values of of range (' + IntToStr(MinVatValue) + '--' + IntToStr(MaxVATCats) + '):' + IntToStr(NumOutOfRange));

      for x := MinVATValue to MaxVATCats do if (Hist[x] > 0) then begin
         if NewFile then begin
            InsertCategory(x);
         end
         else begin
            Table.ApplyFilter(CodeField + '=' + IntToStr(x));
            if (Table.RecordCount = 1) then begin
               Table.Edit;
               Table.SetFieldByNameAsInteger(CountFieldName,Hist[x]);
               Table.Post;
            end
            else begin
               InsertCategory(x);
            end;
         end;
      end;

      if not NewFile then begin
         Table.ApplyFilter('');
         Table.First;
         while not Table.eof do begin
            if (Table.GetFieldByNameAsInteger(CountFieldName) < 1) then begin
               Table.Edit;
               Table.Delete;
            end
            else Table.Next;
         end;
         i := 1;
         Table.First;
         while not Table.eof do begin
            aColor := WinGraphColors[i];
            Table.Edit;
            Table.SetFieldByNameAsInteger('COLOR',AColor);
            Table.Next;
            inc(i);
            if (i > 14)  then i := 1;
         end;
      end;
   end;
   Table.Destroy;
end;


procedure tDEMDataSet.GetRectangleFromDEMGrid(var GridLimits : tGridLimits);
begin
   {$IfDef RecordGetGridLimits} WriteLineToDebugFile('tDEMDataSet.GetRectangleFromDEMGrid in'); {$EndIf}
   GridLimits := FullDEMGridLimits;
   repeat
      Pick_Limits.PickLimits(Self,SelectionMap,GridLimits );
   until (GridLimits.XGridHigh > GridLimits.XGridLow) and (GridLimits.YGridHigh > GridLimits.YGridLow);
   {$IfDef RecordGetGridLimits} WriteLineToDebugFile('left=' + intToStr(LeftGrid) + '  right=' + intToStr(RightGrid) + '  bottom=' + intToStr(BottGrid)+ '  top=' + intToStr(TopGrid)); {$EndIf}
   ClipDEMGrid(GridLimits.XGridLow,GridLimits.YGridHigh);
   ClipDEMGrid(GridLimits.XGridHigh,GridLimits.YGridLow);
   {$IfDef RecordGetGridLimits} WriteLineToDebugFile('out with left=' + intToStr(LeftGrid) + '  right=' + intToStr(RightGrid) + '  bottom=' + intToStr(BottGrid)+ '  top=' + intToStr(TopGrid)); {$EndIf}
end;


procedure tDEMDataSet.VerifyDefaultPosition(Mess : AnsiString; var xg,yg : float32; var XUTM,YUTM : float64);
var
   lat,Long : float64;
begin
   DEMGridToLatLongDegree(xg,yg,Lat,Long);
   GetLatLn.GetLatLongDefault(DEMMapProj,Mess,Lat,Long);
   LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
   DEMGridToUTM(xg,yg,xutm,yutm);
end;


procedure tDEMDataSet.SaveAsGeoJSON;
var
   i,j : integer;
   z : float32;
   JSONstring : ANSIString;
   sl : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('tDEMDataSet.SaveAsGeoJSON, fname=' + DEMFileName);  {$EndIf}

   ShowHourglassCursor;
   fName := ChangeFileExt(DEMFileName,'.json');
   if FileExists(fName) then begin
      {$IfDef RecordWriteDEM} WriteLineToDebugFile('JSON existed'); {$EndIf}
   end
   else begin
       JSONString := '{"v":"GeoJSON-G 0.1"' +
                      ',"w":' + IntToStr(DEMheader.NumCol) +
                      ',"h":' + IntToStr(DEMheader.NumRow) +
                      ',"minz":' + RealToString(DEMheader.MinElev,-18,-1) +
                      ',"maxz":' + RealToString(DEMheader.MaxElev,-18,-1) +
                      ',"llx":' + RealToString(DEMheader.DEMSWCornerX,-18,-6) +
                      ',"lly":' + RealToString(DEMheader.DEMSWCornerY,-18,-6) +
                      ',"dx":' + RealToString(DEMheader.DEMxSpacing,-18,-6) +
                      ',"dy":' + RealToString(DEMheader.DEMySpacing,-18,-6) +
                      ',"proj":';
       if DEMheader.DEMUsed = ArcSecDEM then  JSONString := JSONString  + '"GCS"'
       else JSONString := JSONString + '"UTM' + IntToStr(DEMheader.UTMZone) +  DEMMapProj.LatHemi + '"';

       JSONString := JSONString + ',"data":[';
       for j := pred(DEMheader.NumRow) downto 0 do begin
          for i := 0 to pred(DEMheader.NumCol) do begin
              if GetElevMeters(i,j,z) then begin
                 JSONString := JSONString + RealToString(z,-12,-MDDef.GeoJSONG_zdec) + ',';
              end
              else begin
                 JSONString := JSONString + 'null,';
              end;
          end;
       end;
       Delete(JSONString,Length(JSONString),1);
       JSONString := JSONString + ']}';
       sl := tStringList.Create;
       sl.Add(JSONstring);
       sl.SaveToFile(fName);
       sl.Free;
   end;
   MainBZ2(fName);
   ShowDefaultCursor;
end;




