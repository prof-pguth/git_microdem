{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{-----------------------------------}
{ include file for demdatabase      }
{___________________________________}



function TGISdataBaseModule.AddFieldToDataBase(ft: TFieldType; FName: ShortString; Length : integer; Decimals : integer = 0) : boolean;
begin
    if (MyData <> Nil) then begin
      {$IfDef RecordAddField} WriteLineToDebugFile('TGISDataBase.AddFieldToDataBase must add field ' + FName + ' for ' + dbName); {$EndIf}
       ShowHourglassCursor;
       EmpSource.Enabled := false;
       Result := MyData.InsureFieldPresentAndAdded(ft,fName,Length,Decimals);
       if Result then begin
          {$IfDef RecordAddField} WriteLineToDebugFile('TGISDataBase.AddFieldToDataBase field successfully added,  recs=' + IntToStr(MyData.RecordCount)); {$EndIf}
          RespondToChangedDB;
       end
       else begin
          {$IfDef RecordAddField} WriteLineToDebugFile('TGISDataBase.AddFieldToDataBase field failure'); {$EndIf}
       end;
       ShowStatus;
    end;
end;


procedure TGISdataBaseModule.RemoveDuplicatePositions;
var
   LastPos,NextPos : shortstring;
begin
   MyData.First;
   EmpSource.Enabled := false;
   LastPos := '';
   while not MyData.eof do begin
      NextPos := MyData.GetFieldByNameAsString(LatFieldName) + MyData.GetFieldByNameAsString(LongFieldName);
      if (NextPos = LastPos) then begin
         MyData.Edit;
         MyData.SetFieldByNameAsString(LatFieldName,'');
         MyData.SetFieldByNameAsString(LongFieldName,'');
      end
      else LastPos := NextPos;
      MyData.Next;
   end;
   ShowStatus;
end;


procedure tGISdataBaseModule.FillTrackVoids;
var
   Lat,Long : float64;
begin
   LinearInterpolateAcrossGap('REC_ID',LatFieldName);
   LinearInterpolateAcrossGap('REC_ID',LongFieldName);
   Lat := 0;
   Long := 0;
   MyData.First;
   EmpSource.Enabled := false;
   while not MyData.eof do begin
      if not MyData.ValidLatLongFromTable(Lat,Long) then begin
         MyData.Edit;
         MyData.SetFieldByNameAsFloat(LatFieldName,Lat);
         MyData.SetFieldByNameAsFloat(LongFieldName,Long);
      end;
      MyData.Next;
   end;
   ShowStatus;
end;


procedure tGISdataBaseModule.AddLatLong;
begin
   AddFieldToDataBase(ftFloat,'LAT',12,8);
   AddFieldToDataBase(ftFloat,'LONG',13,8);
   LatFieldName := 'LAT';
   LongFieldName := 'LONG';
end;


procedure tGISdataBaseModule.AddSymbolToDB;
begin
   AddFieldToDataBase(ftInteger,'SYM_COLOR',8);
   AddFieldToDataBase(ftInteger,'SYM_SIZE',2);
   AddFieldToDataBase(ftInteger,'SYM_TYPE',2);
end;


procedure TGISdataBaseModule.AddFieldGroupToTable(What : shortstring);
var
   Table : tMyData;
   fts : ShortString;
   ft : tFieldType;
begin
   {$IfDef RecordAddField} WriteLineToDebugFile('TGISdataBaseModule.AddFieldGroupToTable enter'); {$EndIf}
    if (MyData <> Nil) then begin
       if FileExists(TableDefinitionsFileName) then begin
          Table := tMyData.Create(TableDefinitionsFileName);
          Table.ApplyFilter('TABLE_TYPE=' + QuotedStr(What));
          EmpSource.Enabled := false;
          while not Table.eof do begin
             fts :=  UpperCase(Table.GetFieldByNameAsString('TYPE'));
             if fts = 'STRING' then ft := ftstring
             else if fts = 'INTEGER' then ft := ftInteger
             else if fts = 'float64' then ft := ftFloat;
             AddFieldToDataBase(ft,Table.GetFieldByNameAsString('NAME'),Table.GetFieldByNameAsInteger('LENGTH'),Table.GetFieldByNameAsInteger('DECIMALS'));
             Table.Next;
          end;
          Table.Destroy;
          MyData.AssignEmpSource(EmpSource);
       end
       else MessageToContinue(TableDefinitionsFileName + ' missing; cannot add ' + what);
    end;
   {$IfDef RecordAddField} WriteLineToDebugFile('TGISdataBaseModule.AddFieldGroupToTable exit'); {$EndIf}
end;


procedure TGISdataBaseModule.AddImage;
begin
   AddFieldToDatabase(ftString,'IMAGE',128);
   ImageFieldNames[1] := 'IMAGE';
   ImagePresent := true;
end;

procedure TGISdataBaseModule.AddWWW;
begin
   AddFieldToDatabase(ftString,'WWW',128);
   WWWFieldNames[1] := 'WWW';
   WWWpresent := true;
end;


procedure TGISdataBaseModule.TrimStringFields(theField : ANSIString = '');
begin
   {$IfDef RecordGAZ} WriteLineToDebugFile('TGISdataBaseModule.TrimStringFields, theField=' + theField); {$EndIf}
   EmpSource.Enabled := false;
   ShowHourglassCursor;
   MyData.TrimAllStringFields(theField);
   ShowStatus;
   {$IfDef RecordGAZ} WriteLineToDebugFile('TGISdataBaseModule.TrimStringFields out'); {$EndIf}
end;


procedure TGISdataBaseModule.LinearInterpolateAcrossGap(yfield,zField : shortstring);
type
   thevalues = Array[0..64000] of float64;
var
   y,z,yn,zn : ^theValues;
   yv,zv : float64;
   NumPairs,NumNewPairs,n : integer;
begin
   New(y);
   New(z);
   New(zn);
   New(yn);
   ClearGISFilter;
   NumPairs := 0;
   NumNewPairs := 0;
   EmpSource.Enabled := false;
   ShowHourglassCursor;
   MyData.First;
   while not MyData.eof do begin
      if MyData.CarefullyGetFieldByNameAsFloat64(yField,yv) then begin
         yn^[NumNewPairs] := yv;
         if MyData.CarefullyGetFieldByNameAsFloat64(zField,zv) then begin
            y^[NumPairs] := yv;
            z^[NumPairs] := zv;
            inc(NumPairs);
         end;
         inc(NumNewPairs);
      end;
      MyData.Next;
   end;
   if (NumNewPairs = NumPairs) then begin
      MessageToContinue('No gaps in time series');
   end
   else begin
      {call with yn set for the new distances along the profile, and return zn with the new elevations}
      InterpolateProfile(NumPairs,y^,z^,NumNewPairs,yn^,zn^);
      MyData.First;
      n := 0;
      while not MyData.eof do begin
         if Round(zn^[n]) <> MaxSmallInt then begin
            MyData.Edit;
            MyData.SetFieldByNameAsFloat(zField,zn^[n]);
         end;
         inc(n);
         MyData.Next;
      end;
   end;

   Dispose(y);
   Dispose(z);
   Dispose(zn);
   Dispose(yn);
   ShowStatus;
end;

function TGISdataBaseModule.AddNavFieldDefinitions : boolean;
var
   f1,f2,f3,f4,TStr,zfield : ShortString;
   dd : integer;
begin
    if MDDef.UseMeters then begin
       f1 := 'DISTANCE_M';
       f2 := 'CUM_M';
       f3 := 'SPEED_MPS';
       dd := 2;
    end
    else begin
       f1 := 'DIST_KM';
       f2 := 'CUM_KM';
       f3 := 'SPEED_KPH';
       dd := 4;
    end;

    if MDdef.AddSpeed then begin
       if MyData.FieldExists('DEC_HOURS') or MyData.FieldExists('DEC_DAYS') or MyData.FieldExists('DEC_YEAR') then begin
          if MyData.FieldExists('DEC_HOURS') then begin
             f4 := 'DEC_HOURS';
          end
          else if MyData.FieldExists('DEC_DAYS') then begin
             f4 := 'DEC_DAYS';
          end
          else if MyData.FieldExists('DEC_YEAR') then begin
             f4 := 'DEC_YEAR';
          end
       end
       else begin
          Result := not AnswerIsYes('DEC_HOURS or DAYS or YEAR required to compute speed; do you want to fix that first');
          if not Result then exit;
          MDdef.AddSpeed := false;
       end;
       AddFieldToDataBase(ftFloat,f3,12,2);
    end;
    if MDdef.AddAzimuth then AddFieldToDataBase(ftFloat,'AZIMUTH',6,2);
    if MDdef.AddDist then AddFieldToDataBase(ftFloat,f1,8,dd);
    if MDdef.AddCumDist then AddFieldToDataBase(ftFloat,f2,8,dd);
    if MDdef.Add3DDist then begin
       AddFieldToDataBase(ftFloat,'DIST_3D_M',8,dd);
       AddFieldToDataBase(ftFloat,'CUM_3D_M',8,dd);
       AddFieldToDataBase(ftFloat,'DZ_M',8,2);
       AddFieldToDataBase(ftFloat,'GAIN_M',8,2);
       AddFieldToDataBase(ftFloat,'DROP_M',8,2);
    end;
    Result := true;
    {$IfDef RecordNavigation} WriteLineToDebugFile('Fields added'); {$EndIf}
end;


procedure TGISdataBaseModule.AddNavFields;
var
   xutm,yutm,xutm2,yutm2,z,LastZ,
   Last4Grad,This4Grad,Lat,Long,LastLat,LastLong,Az,Dist,FirstAz,CumDist,CumDist3d,Factor,h1,h2,tf,dz : float64;
   f1,f2,f3,f4,TStr,zfield : ShortString;
   dd,i,rc : integer;
begin
    {$IfDef RecordNavigation} WriteLineToDebugFile('TGISdataBaseModule.AddNavFields in'); {$EndIf}
    if not AddNavFieldDefinitions then exit;
    if MDDef.UseMeters then begin
       f1 := 'DISTANCE_M';
       f2 := 'CUM_M';
       f3 := 'SPEED_MPS';
       Factor := 1;
       tf := 24*3600;
       dd := 2;
    end
    else begin
       f1 := 'DIST_KM';
       f2 := 'CUM_KM';
       Factor := 0.001;
       f3 := 'SPEED_KPH';
       tf := 24;
       dd := 4;
    end;
    if MDdef.AddSpeed then begin
       if MyData.FieldExists('DEC_HOURS') or MyData.FieldExists('DEC_DAYS') or MyData.FieldExists('DEC_YEAR') then begin
          if MyData.FieldExists('DEC_HOURS') then begin
             f4 := 'DEC_HOURS';
             tf := tf / 24;
          end
          else if MyData.FieldExists('DEC_DAYS') then begin
             f4 := 'DEC_DAYS';
          end
          else if MyData.FieldExists('DEC_YEAR') then begin
             f4 := 'DEC_YEAR';
             tf := tf * 365;
          end
       end
       else begin
          if AnswerIsYes('DEC_HOURS or DAYS or YEAR required to compute speed; do you want to fix that first') then exit;
          MDdef.AddSpeed := false;
       end;
    end;

   EmpSource.Enabled := false;
   MyData.First;
   FirstAz := -9999;
   CumDist := 0;
   CumDist3D := 0;
   ValidLatLongFromTable(LastLat,LastLong);
   RedefineWGS84DatumConstants(LastLong);
   if MDdef.AddSpeed then h1 := MyData.GetFieldByNameAsFloat(f4);
   if MDdef.AddCUMDist then begin
      MyData.Edit;
      MyData.SetFieldByNameAsFloat(f2,0);
      MyData.Post;
   end;
   if MDdef.Add3DDist then begin
      zField := GetElevationField(false);
      LastZ := MyData.GetFieldByNameAsFloat(zField);
   end;
   StartProgress('Add nav');
   i := 0;
   rc := ProgressIncrement(MyData.FiltRecsInDB);
   MyData.Next;
   while not MyData.eof do begin
      if (i mod rc = 0) then UpdateProgressBar(i/MyData.FiltRecsInDB);
      inc(i);
      if ValidLatLongFromTable(Lat,Long) then begin
         VincentyCalculateDistanceBearing(LastLat,LastLong,Lat,Long,Dist,Az);
         if MDdef.AddSpeed then h2 := MyData.GetFieldByNameAsFloat(f4);
         MyData.Edit;
         if (Abs(Dist) > 0.0001) and (abs(Az) < 360) then begin
            if (FirstAz < -1000) then FirstAz := Az;
            Dist := Dist * Factor;
            if MDdef.AddSpeed then begin
               if (abs(h2-h1) > 0.00001) then begin
                  MyData.SetFieldByNameAsFloat(f3,Dist / ((h2-h1) * tf));
               end;
             end;
            if MDdef.AddAzimuth then MyData.SetFieldByNameAsFloat('AZIMUTH',Az);
            if MDdef.AddCumDist then begin
               CumDist := CumDist + Dist;
               MyData.SetFieldByNameAsFloat(f2,CumDist);
            end;
            if MDdef.AddDist then MyData.SetFieldByNameAsFloat(f1,Dist);

            if MDdef.Add3DDist then begin
               Z := MyData.GetFieldByNameAsFloat(zField);
               dz := z-LastZ;
               VincentyCalculateDistanceBearing(LastLat,LastLong,Lat,Long,Dist,Az);
               Dist := sqrt( sqr(Dist) + sqr(dz));
               MyData.SetFieldByNameAsFloat('DIST_3D_M',Dist);
               CumDist3d := CumDist3d + Dist;
               MyData.SetFieldByNameAsFloat('CUM_3D_M',CumDist3D);
               MyData.SetFieldByNameAsFloat('DZ_M',dz);
               if (dz > 0.001) then MyData.SetFieldByNameAsFloat('GAIN_M',dz);
               if (dz < -0.001) then MyData.SetFieldByNameAsFloat('DROP_M',dz);
            end;
         end
         else begin
            if MDdef.AddSpeed then MyData.SetFieldByNameAsFloat(f3,0);
            if MDdef.AddDist then MyData.SetFieldByNameAsFloat(f1,0);
            if MDdef.AddCumDist then MyData.SetFieldByNameAsFloat(f2,CumDist);
            if MDdef.Add3DDist then MyData.SetFieldByNameAsFloat('DIST_3D_M',0);
         end;
         LastLat := Lat;
         LastLong := Long;
         if MDdef.Add3DDist then LastZ := z;
         if MDdef.AddSpeed then h1 := h2;
      end;
      MyData.Next;
   end;
   ShowStatus;
end;


         procedure TGISdataBaseModule.RenameField(OldName,NewName : ShortString);
         begin
            if (MyData <> Nil) then begin
               if MyData.FieldExists(oldName) then begin
                  EmpSource.Enabled := false;
                  MyData.Destroy;
                  ApplicationProcessMessages;
                  RenameDBaseField(dbFullName,OldName,NewName);
                  MyData := TMyData.Create(dbFullName);
                  MyData.AssignEmpSource(EmpSource);
               end;
            end
            else MessageToContinue('Not allowed for this type of database');
         end;


         procedure TGISdataBaseModule.ChangeFieldType(OldName : ShortString; NewType : ANSIchar);
         begin
            if (MyData <> Nil) then begin
               EmpSource.Enabled := false;
               MyData.Destroy;
               ApplicationProcessMessages;
               ChangeDBaseFieldType(dbFullName,OldName,NewType);
               MyData := TMyData.Create(dbFullName);
               MyData.AssignEmpSource(EmpSource);
            end
            else MessageToContinue(NotAllowedDBtype);
         end;

         procedure TGISdataBaseModule.ChangeFieldDecimals(OldName : ShortString; NewDecimals : byte);
         begin
            if (MyData <> Nil) then begin
               EmpSource.Enabled := false;
               MyData.Destroy;
               ApplicationProcessMessages;
               ChangeDBaseFieldDecimals(dbFullName,OldName,NewDecimals);
               MyData := TMyData.Create(dbFullName);
               MyData.AssignEmpSource(EmpSource);
            end
            else MessageToContinue(NotAllowedDBtype);
         end;

         procedure TGISdataBaseModule.AddIcon(Fill : boolean = true);
         var
            fname : PathStr;
            New : ShortString;
         begin
            {$IfDef RecordIcons} WriteLineToDebugFile('TGISdataBaseModule.AddIcon in'); {$EndIf}
            New := 'ICON';
            AddFieldToDatabase(ftString,New,35,0);
            {$IfDef RecordIcons} WriteLineToDebugFile('TGISdataBaseModule.AddIcon field added'); {$EndIf}
            IconFieldNames[1] := New;
            dbOpts.IconField := New;
            IconPresent := true;
            if Fill then begin

            fName := MainMapData + 'Icons\';
            if GetGraphicsFileName('',fName) then begin
               {$IfDef RecordIcons} WriteLineToDebugFile('TGISdataBaseModule.AddIcon icon found'); {$EndIf}
               fName := ExtractFileName(fName);
               FillFieldWithValue(New,fName);
            end;
            end;
            {$IfDef RecordIcons} WriteLineToDebugFile('TGISdataBaseModule.AddIcon out'); {$EndIf}
         end;


procedure TGISdataBaseModule.FillFieldsFromJoinedTable(var TheFields : tStringList; ForceOverWrite : boolean);
{$IfDef VCL}
var
   aCopyFieldThisDB,aCopyFieldOtherDB : ShortString;
   Overwrite : boolean;
   TStr,TStr2 : ShortString;
   ft : tFieldType;
   fLength,Decs,i,j,rc : integer;
begin
    if (TheFields.Count > 0) then begin
       {$IfDef RecordCopyFieldLinkDB} WriteLineToDebugFile('TGISdataBaseModule.FillFieldsFromJoinedTable in'); {$EndIf}
       Overwrite := true;
       if not ForceOverwrite then begin
         for j := pred(TheFields.Count) downto 0 do begin
            aCopyFieldOtherDB := TheFields.Strings[j];
            {$IfDef RecordCopyFieldLinkDB} WriteLineToDebugFile('Field: ' + aCopyFieldOtherDB); {$EndIf}
            if MyData.FieldExists(aCopyFieldOtherDB) then begin
               if not AnswerIsYes('Field ' + aCopyFieldOtherDB + ' already exists; proceed') then TheFields.Delete(j);
               if (TheFields.Count = 1) then Overwrite := AnswerIsYes('Overwrite field ' + aCopyFieldOtherDB + ' if filled');
            end;
         end;
       end;

       StartThreadTimers('DB Fields from join',1);
       for j := 0 to pred(TheFields.Count) do begin
         ThreadTimers.OverallGauge9.Progress := round(100 * j/TheFields.Count);
         aCopyFieldOtherDB := TheFields.Strings[j];
         ThreadTimers.EnableGauge(1,true,aCopyFieldOtherDB);
         aCopyFieldThisDB := aCopyFieldOtherDB;

         if not MyData.FieldExists(aCopyFieldOtherDB) then begin
            fLength := LinkTable.GetFieldLength(aCopyFieldOtherDB);
            ft := LinkTable.GetFieldType(aCopyFieldOtherDB);
            if (ft = ftFloat) then Decs := LinkTable.GetFieldPrecision(aCopyFieldOtherDB)
            else Decs := 0;
            {$IfDef RecordCopyFieldLinkDB} WriteLineToDebugFile('field=' + TheFields.Strings[j] + '  fieldType=' + FieldTypeStr(ft) + '  fieldLength=' + IntToStr(fLength) +   '  decs=' + IntToStr(Decs)); {$EndIf}
            AddFieldToDataBase(ft,aCopyFieldThisDB,fLength,Decs);
         end
         else begin
            {$IfDef RecordCopyFieldLinkDB} WriteLineToDebugFile('field alredy present, ' + TheFields.Strings[j]); {$EndIf}
         end;
         EmpSource.Enabled := false;
      end {for j};

       i := 0;
       MyData.First;
       while not MyData.eof do begin
          inc(i);
          if (i mod 100 = 0) then begin
              ThreadTimers.UpdateThreadStats(1,round(100 * i/MyData.FiltRecsInDB));
              EmpSource.Enabled := false;
          end;

          TStr := MyData.GetFieldByNameAsString(dbOpts.LinkFieldThisDB);
             if (TStr <> '') and FindValidJoin(TStr) then begin
                MyData.Edit;
                for j := 0 to pred(TheFields.Count) do begin
                   MyData.SetFieldByNameAsString(TheFields.Strings[j],LinkTable.GetFieldByNameAsString(TheFields.Strings[j]));
                end;
             end;
          MyData.Next;
       end {while};
      EndThreadTimers;
    end;
    TheFields.Free;
{$Else}
begin
{$EndIf}
end;


            procedure TGISdataBaseModule.AddGeometry(What : tAddGeometry);
            var
               area,Length,xcent,ycent,Lat,Long,Lat2,Long2,Dist,Bearing : float64;
               i : integer;
            begin
               if (What = agCentroid) then begin
                  AddFieldToDataBase(ftFloat,'LAT_CENTRD',12,6);
                  AddFieldToDataBase(ftFloat,'LON_CENTRD',12,6);
                  CentroidPresent := true;
               end;
               if (What = agAreaKM2) then begin
                  AddFieldToDataBase(ftFloat,'AREA_KM2',14,6);
               end;
               if (What = agAreaM2) then begin
                  AddFieldToDataBase(ftFloat,'AREA_M2',12,1);
               end;
               if (What = agPerimeter) then begin
                  AddFieldToDataBase(ftFloat,'PERIMTR_KM',12,4);
               end;
               if (What = agLength) then begin
                  AddFieldToDataBase(ftFloat,'LENGTH_KM',12,4);
               end;
               if (What = agZStats) then begin
                  AddFieldToDataBase(ftFloat,'FIRST_Z',12,2);
                  AddFieldToDataBase(ftFloat,'LAST_Z',12,2);
               end;
               if (What = asSinuousity) then begin
                  AddFieldToDataBase(ftFloat,'SINUOSITY',8,3);
               end;
               if (What = agNumPts) then begin
                  AddFieldToDataBase(ftInteger,'REC_PNTS',6);
               end;
               if (What = agElevationDeltas) then begin
                  AddFieldToDataBase(ftFloat,'DELTA_Z',8,2);
               end;
               if (What = agDirection) then begin
                  AddFieldToDataBase(ftFloat,'REC_DIR',6,2);
               end;
               if (What = agEndPoints) then begin
                  AddFieldToDataBase(ftFloat,'LAT_START',11,7);
                  AddFieldToDataBase(ftFloat,'LONG_START',12,7);
                  AddFieldToDataBase(ftFloat,'LAT_END',11,7);
                  AddFieldToDataBase(ftFloat,'LONG_END',12,7);
               end;

               if (What = agCompact) then AddFieldToDataBase(ftFloat,'COMPACT',8,5);
               if (What = agMeanWidth) then AddFieldToDataBase(ftFloat,'MEAN_WIDTH',8,5);
               if (What = agShapeNum) then AddFieldToDataBase(ftFloat,'SHAPE_NUM',8,5);
               if (What = agSchwartz) then AddFieldToDataBase(ftFloat,'SCHWARTZ',8,5);
               if (What = agP2A) then AddFieldToDataBase(ftFloat,'P2A',8,5);

               i := 0;
               StartProgress('Geometry');
               MyData.First;
               while not MyData.eof do begin
                  if (i mod 1000 = 0) then begin
                     UpdateProgressBar(i/MyData.FiltRecsInDB);
                     EmpSource.Enabled := false;
                  end;
                  inc(i);

                  MyData.Edit;
                  if (What = agLength) or (What = agPerimeter) or (What = asSinuousity) then begin
                     Length := aShapeFile.LineLength(MyData.RecNo);
                     if (What = agPerimeter) then MyData.SetFieldByNameAsFloat('PERIMTR_KM',0.001 * Length);
                     if (What = agLength) then MyData.SetFieldByNameAsFloat('LENGTH_KM',0.001 * Length);
                  end
                  else if LineShapeFile(ShapeFileType) and (What = agCentroid) then  begin
                     ycent := 0.5 * (MyData.GetFieldByNameAsFloat('LAT_HI') + MyData.GetFieldByNameAsFloat('LAT_LOW'));
                     xcent := 0.5 * (MyData.GetFieldByNameAsFloat('LONG_HI') + MyData.GetFieldByNameAsFloat('LONG_LOW'));
                  end
                  else if (What = agAreaKM2) or (What = agCentroid) or (What = agAreaM2)  then begin
                     Area := aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,ycent,xcent);
                     if (What = agAreaKM2) then  MyData.SetFieldByNameAsFloat('AREA_KM2',0.000001*Area);
                     if (What = agAreaM2) then MyData.SetFieldByNameAsFloat('AREA_M2',Area);
                     if (What = agCentroid) then begin
                        MyData.SetFieldByNameAsFloat('LAT_CENTRD',yCent);
                        MyData.SetFieldByNameAsFloat('LON_CENTRD',xCent);
                     end;
                  end
                  else if (What = agElevationDeltas) or (What = agEndPoints) then aShapeFile.GetLineCoords(MyData.RecNo,true)
                  else if (What in [agCompact,agMeanWidth,agShapeNum,agSchwartz]) then begin
                     Length := 0.0001 * aShapeFile.LineLength(MyData.RecNo);
                     Area := 0.001 * 0.0001 * aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,ycent,xcent);
                     if Area > 0.000001 then begin
                        if (What = agCompact) then MyData.SetFieldByNameAsFloat('COMPACT',4 * Pi  * Area /sqr(Length));
                        if (What = agMeanWidth) then MyData.SetFieldByNameAsFloat('MEAN_WIDTH',2 * Area / Length);
                        if (What = agShapeNum) then MyData.SetFieldByNameAsFloat('SHAPE_NUM',Length / sqrt(Area));
                        if (What = agSchwartz) then MyData.SetFieldByNameAsFloat('SCHWARTZ_NUM',1 / (Length / ( 2 * Pi * sqrt(Area/Pi))));
                        if (What = agP2A) then MyData.SetFieldByNameAsFloat('P2A',sqr(Length)/Area);
                     end;
                  end;
                  if (What = agElevationDeltas) then begin
                     if aShapeFile.CurrentPolyLineHeader.NumPoints > 2 then begin
                        MyData.SetFieldByNameAsFloat('DELTA_Z',abs(aShapeFile.CurrentLineZs^[0]-aShapeFile.CurrentLineZs^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)]));
                     end;
                  end
                  else if (What = asSinuousity) or (What = agEndPoints) then begin
                     if (aShapeFile.CurrentPolyLineHeader.NumPoints > 2) then begin
                         Lat := aShapeFile.CurrentLineCoords^[0].Lat;
                         Long := aShapeFile.CurrentLineCoords^[0].Long;
                         Lat2 := aShapeFile.CurrentLineCoords^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)].Lat;
                         Long2 := aShapeFile.CurrentLineCoords^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)].Long;
                         if (What = agEndPoints) then begin
                            MyData.SetFieldByNameAsFloat('LAT_START',Lat);
                            MyData.SetFieldByNameAsFloat('LONG_START',Long);
                            MyData.SetFieldByNameAsFloat('LAT_END',Lat2);
                            MyData.SetFieldByNameAsFloat('LONG_END',Long2);
                         end
                         else begin
                            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Dist,Bearing);
                            if (Dist > 0.01) then MyData.SetFieldByNameAsFloat('SINUOSITY',Length/Dist);
                         end;
                     end;
                  end
                  else if (What = agDirection) then begin
                     MyData.SetFieldByNameAsFloat('REC_DIR',aShapeFile.LineHeading(MyData.RecNo));
                  end
                  else if (What = agZStats) or (What = agNumPts) then begin
                     aShapeFile.GetLineCoords(MyData.RecNo,(What = agZStats));
                     if (What = agZStats) then begin
                       {$IfDef RecordMakeLineArea}
                          if MyData.FieldExists('NAME') then TStr := '  ' + MyData.GetFieldByNameAsString('NAME') else TStr := '';
                          WriteLineToDebugFile(IntToStr(MyData.RecNo) + RealToString(zs^[0],12,1) + RealToString(zs^[pred(NumPts)],12,1) + TStr);
                       {$EndIf}
                       MyData.CarefullySetFloat('FIRST_Z',aShapeFile.CurrentLineZs^[0], 0.01);
                       MyData.CarefullySetFloat('LAST_Z',aShapeFile.CurrentLineZs^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)],0.01);
                     end;
                     if (What = agNumPts) then begin
                        MyData.SetFieldByNameAsInteger('REC_PNTS',aShapeFile.CurrentPolyLineHeader.NumPoints);
                     end;
                  end;
                  MyData.Next;
               end;
               ShowStatus;
            end;


procedure TGISdataBaseModule.AddXYZfields;
begin
   AddFieldToDataBase(ftFloat,'X',18,6);
   AddFieldToDataBase(ftFloat,'Y',18,6);
   AddFieldToDataBase(ftFloat,'Z',18,6);
end;


procedure TGISdataBaseModule.RemoveLeadingZerosInField(Table : tMyData; WantedFieldName : ShortString);
var
   TStr : shortstring;
begin
   EmpSource.Enabled := false;
   ShowHourglassCursor;
   Table.First;
   while not Table.EOF do begin
     TStr := Table.GetFieldByNameAsString(WantedFieldName);
     if TStr <> '' then begin
        while (TStr[1] = '0') and (Length(TStr) > 1) do System.Delete(TStr,1,1);
        Table.Edit;
        Table.SetFieldByNameAsString(WantedFieldName,TStr);
     end;
     Table.Next;
   end;
   Table.First;
   ShowStatus;
end;


procedure TGISdataBaseModule.AddSequentialIndex(fName : shortstring; Fill : boolean = true);
var
   I : int64;
   OldFilter : shortstring;
begin
   {$IfDef RecordAddField} WriteLineToDebugFile('TGISdataBaseModule.AddSequentialIndex enter ' + fName + ' for ' + dbName + '  recs=' + IntToStr(Mydata.RecordCount)); {$EndIf}
   if MyData.Filtered then exit;
   OldFilter := MyData.Filter;
   MyData.ApplyFilter('');

   if (not MyData.FieldExists(fName)) then begin
      AddFieldToDataBase(ftInteger,fName,9);
      Fill := true;
     {$IfDef RecordAddField} WriteLineToDebugFile('TGISdataBaseModule.AddSequentialIndex added, recs=' + IntToStr(Mydata.RecordCount)); {$EndIf}
   end
   else begin
      if not Fill then begin
         MyData.First;
         i := MyData.GetFieldByNameAsInteger(fName);
         if (i <> 1) then Fill := true;
         MyData.Last;
         i := MyData.GetFieldByNameAsInteger(fName);
         if (i <> MyData.TotRecsInDB) then Fill := true;
      end;
   end;

   if Fill then begin
      {$IfDef RecordAddField} WriteLineToDebugFile('TGISdataBaseModule.AddSequentialIndex fill ' + fName + ' for ' + dbName + '  recs=' + IntToStr(Mydata.RecordCount)); {$EndIf}
      i := 0;
      EmpSource.Enabled := false;
      {$IfDef VCL}  StartProgress('Fill ' + fName + ' in ' + dbName); {$EndIf}
      MyData.First;
      while not MyData.eof do begin
         inc(i);
         if (i mod 500 = 0) then begin
            {$IfDef VCL} UpDateProgressBar(i/MyData.TotRecsInDB);   {$EndIf}
            EmpSource.Enabled := false;
         end;
         MyData.Edit;
         MyData.SetFieldByNameAsInteger(fName,i);
         MyData.Next;
      end;
   end;
   MyData.ApplyFilter(OldFilter);
   ShowStatus;
   {$IfDef RecordAddField} WriteLineToDebugFile('TGISdataBaseModule.AddSequentialIndex out, recs=' + IntToStr(Mydata.RecordCount)); {$EndIf}
end;


procedure tGISdataBaseModule.AddConcatenatedField(NewField,WantedFieldName,SecondFieldName,ThirdFieldName,Preceder,Separator,SecondSeparator,Follower  : shortstring);
var
   i : integer;
   TStr,TStr2 : ShortString;
begin
   i := MyData.GetFieldLength(WantedFieldName)  + Length(Separator) + Length(SecondSeparator) + Length(Preceder) + Length(Follower);
   if (SecondFieldName <> '') then i := i + MyData.GetFieldLength(SecondFieldName);
   if (ThirdFieldName <> '') then i := i + MyData.GetFieldLength(ThirdFieldName);
   AddFieldToDataBase(ftString,NewField,i,0);
   EmpSource.Enabled := false;
   MyData.First;
   i := 0;
   {$IfDef VCL} StartProgress('Concatenate'); {$EndIf}
   while not MyData.eof do begin
     inc(i);
     {$IfDef VCL} if (i mod 1000 = 0) then UpdateProgressBar(i/MyData.RecordCount); {$EndIf}
     MyData.Edit;
     if (SecondFieldName <> '') then TStr := MyData.GetFieldByNameAsString(SecondFieldName) else TStr := '';
     if (ThirdFieldName <> '') then TStr2 := MyData.GetFieldByNameAsString(ThirdFieldName)  else TStr2 := '';
     MyData.SetFieldByNameAsString(NewField,Preceder + MyData.GetFieldByNameAsString(WantedFieldName) + Separator + TStr + SecondSeparator + TStr2 +  Follower);
     MyData.Next;
   end;
   {$IfDef VCL} EndProgress; {$EndIf}
end;


procedure TGISdataBaseModule.FillUseField(UnfilterFirst : boolean; ch : char);
begin
   EmpSource.Enabled := false;
   if UnfilterFirst then MyData.ApplyFilter('');
   MyData.First;
   while not MyData.EOF do begin
      MyData.Edit;
      MyData.SetFieldByNameAsString('USE',ch);
      MyData.Next;
   end;
   EmpSource.Enabled := true;
end;


procedure TGISdataBaseModule.AddSymbolizationToLayerTable(Caption : shortstring);
begin
   LayerTable.Insert;
   LayerTable.SetFieldByNameAsString('NAME',Caption);
   LayerTable.SetFieldByNameAsString('FILTER',MyData.Filter);
   LayerTable.SetFieldByNameAsInteger('PLOT_ORDER',1);
   LayerTable.SetFieldByNameAsString('PLOT','Y');
   WriteDisplaySymbology(LayerTable);
   LayerTable.Post;
end;


procedure TGISdataBaseModule.FillFieldWithValue(WantedFieldName : ShortString; WantedValue : shortString; ShowProgress : boolean = true);
var
  i,StartingRecCount,RecNo : integer;
begin
   EmpSource.Enabled := false;
   {$IfDef VCL} if ShowProgress then StartProgress('Fill field ' + WantedFieldName); {$EndIf}
   repeat
      StartingRecCount := MyData.RecordCount;
      i := 0;
      MyData.First;
      while not MyData.eof do begin
         inc(i);
         {$IfDef VCL}
            if ShowProgress and (I mod 250 = 0) then begin
               UpdateProgressBar(i/MyData.RecordCount);
               EmpSource.Enabled := false;
            end;
         {$EndIf}
         RecNo := MyData.GetFieldByNameAsInteger(RecNoFName);
         MyData.Edit;
         MyData.SetFieldByNameAsString(WantedFieldName,WantedValue);
         MyData.Post;
         if (RecNo = MyData.GetFieldByNameAsInteger(RecNoFName)) then MyData.Next;
      end;
   until (MyData.RecordCount = StartingRecCount) or (MyData.RecordCount = 0);
    {$IfDef VCL}
       if ShowProgress then begin
         EmpSource.Enabled := true;
         EndProgress;
       end;
    {$EndIf}
end;

