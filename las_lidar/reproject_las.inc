procedure ReprojectOKUSLasFileToUTM(fName : PathStr);
var
   StartProjection : BaseMap.tMapProjection;
begin
   Startprojection := CreateUKOSprojection;
   ReprojectLasFileToUTM(fName,30,StartProjection,'OGB-A');
end;


procedure ReprojectIrishLasFileToUTM(fName : PathStr);
var
   StartProjection : BaseMap.tMapProjection;
begin
   {$IfDef RecordReprojectLAS} writeLineToDebugFile('ReprojectIrishLasFileToUTM ' + fName); {$EndIf}
   Startprojection := CreateIrishProjection;
   {$IfDef RecordReprojectLAS} writeLineToDebugFile('ReprojectIrishLasFileToUTM step 2'); {$EndIf}
   ReprojectLasFileToUTM(fName,29,StartProjection,'IRL');
end;


procedure ReprojectDefinedLasFileToUTM(fName : PathStr);
var
   SP : BaseMap.tMapProjection;
begin
   ReprojectLasFileToUTM(fName,MDDef.DefaultUTMZone,SP,'');
end;

procedure ZShiftDefinedLasFileToMeters(fName : PathStr);
var
   SP : BaseMap.tMapProjection;
begin
   ReprojectLasFileToUTM(fName,MDDef.DefaultUTMZone,SP,'',FeetToMeters);
end;


procedure ReprojectLasFileToUTM(fName : PathStr; UTMZone : integer; StartProjection : BaseMap.tMapProjection; StartDatum : shortString; zscaler : float64 = 1);
var
   lf : tLAS_data;
   NewLas : tCreateLasFile;
   i,j,RecsRead,xp,yp,zp : integer;
   x,y,z,Lat,Long : float64;
   offsetsDone,NeedReprojection,NeedDatumShift : boolean;
   NewName : PathStr;
   OldDatum : tMapProjection;
   NewLasProjection : tlasProjectionDefinition;
begin
   {$IfDef RecordReprojectLAS} writeLineToDebugFile('ReprojectLasFileToUTM ' + fName + '   ' + StartDatum ); {$EndIf}
    OldDatum := tMapProjection.Create('reproject las, olddatum');
    Lf := Las_Lidar.tLAS_data.Create(fName);
    if (StartDatum = '') then begin
       StartProjection := Lf.lasProjectionDefinition.LASProjection;
       StartDatum := Lf.lasProjectionDefinition.LasProjection.H_DatumCode;
      {$IfDef RecordReprojectLAS}   WriteLineToDebugFile('LAS file start datum=' + StartDatum ); {$EndIf}
    end;

    AllowNoProjectionLAS := true;
    NeedReprojection := true;
    if WGSEquivalentDatum(StartDatum) then begin
       NeedDatumShift := true;
       if (Lf.LasProjectionDefinition.LasProjection.ProjectedCSTypeGeoKey = 26900 + UTMZone) then NeedReprojection := false;
    end
    else NeedDatumShift := false;

    if NeedDatumShift then begin
       {$IfDef RecordReprojectLAS} WriteLineToDebugFile('reprojection needed'); {$EndIf}
       OldDatum.DefineDatumFromUTMZone(StartDatum,UTMZone,'N','LAS ReprojectLasFileToUTM');
       WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',UTMZone,'N','LAS RedefineWGS84');
    end;

    NewLasProjection.LasProjection := tMapProjection.Create('reproject las, new');
    NewLasProjection.LasProjection.ThisIsUTMFile := true;
    NewLasProjection.LasProjection.ProjectedCSTypeGeoKey := 26900 + UTMZone;
    NewLasProjection.LasProjection.ProjLinearUnitsGeoKey := 9001;
    NewLasProjection.LasProjection.VerticalUnitsGeoKey := 9001;

    NewName := ChangeFileExt(fName, '_utm.las');
    NewLas := tCreateLasFile.Create;
    NewLas.NewLasHeader := lf.LasHeader;

    NewLAS.CreateNewLASfile(NewName,NewLasProjection,lf.lasHeader);

    OffsetsDone := false;
   StartProgress('Reproject ' + ExtractFileNameNoExt(fName));
   lf.PrepDataRead;
   for i := 0 to lf.ReadsRequired do begin
      UpdateProgressBar(i/lf.ReadsRequired);
      lf.ReadPoints(RecsRead);
      {$IfDef RecordReprojectLAS} writeLineToDebugFile('Read ' + IntToStr(RecsRead)); {$EndIf}
      for j := 1 to RecsRead do begin
         x := lf.ExpandLAS_X(j);
         y := lf.ExpandLAS_Y(j);
         z := lf.ExpandLAS_Z(j);

         if NeedReprojection then begin
            StartProjection.InverseProjectDegrees(x,y,Lat,Long);
            if NeedDatumShift then MolodenskiyTransformation(Lat,Long,Lat,Long,OldDatum,WGS84DatumConstants);
            WGS84DatumConstants.ForwardProjectDegrees(Lat,Long,x,y);
         end;

         if not OffsetsDone then begin
            NewLas.NewLasHeader.Xoffset := x;
            NewLas.NewLasHeader.Yoffset := y;
            NewLas.NewLasHeader.Zoffset := z * zscaler;
            NewLas.NewLasHeader.XscaleFac := 0.01;
            NewLas.NewLasHeader.YscaleFac := 0.01;
            OffsetsDone := true;
         end;

         lf.ReviseCoordinates(j,x,y,z*zscaler,NewLas.NewLasHeader);
         NewLas.AddShotToOutputBuffer(lf,j);
      end;
   end;
   lf.FreeLASRecordMemory;

   EndProgress;
   {$IfDef RecordReprojectLAS} WriteLineToDebugFile('ReprojectLasFileToUTM done reading'); {$EndIf}

    Lf.Destroy;
    NewLas.Destroy;
    OldDatum.Destroy;
    NewLasProjection.LasProjection.Destroy;
    AllowNoProjectionLAS := false;
   {$IfDef RecordReprojectLAS} writeLineToDebugFile('ReprojectLasFileToUTM out'); {$EndIf}
end;


