{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{include file for demcoord.pas}


function tDEMDataSet.ReadDEMNow(var tFile : PathStr; transformtoNewDatum : boolean) : boolean;
var
   Ext   : ExtStr;
   TStr  : ANSIString;
   Error : boolean;


   {$IfDef ExGeotiff}
   {$Else}
      function ReadTIFFDEM(var DEMFileName : PathStr) : boolean;
      var
         TiffImage : tTiffImage;
         RegVars : tRegVars;
      begin
         {$If Defined(RecordReadDEM) or Defined(RecordGeotiff)} WriteLineToDebugFile('tDEMDataSet.ReadDEMNow ReadTIFFDEM in'); {$EndIf}
         TiffImage := tTiffImage.CreateGeotiff(false,DEMMapProjection,RegVars,false,DEMFileName,Result,false,false);
         {$If Defined(RecordGeotiff) or Defined(TrackHorizontalDatum)} WriteLineToDebugFile('DEM TiffImage created, datum=' + DEMMapProjection.h_DatumCode); {$EndIf}

         if Result then begin
            {$If Defined(RecordReadDEM) or Defined(RecordGeotiff)} WriteLineToDebugFile('Call TiffImage.CreateTiffDEM'); {$EndIf}
            Result := TiffImage.CreateTiffDEM(Self);
            {$IfDef TrackHorizontalDatum} WriteLineToDebugFile('ReadDEMNOw CreateTiffDEM out, DigitizeDatum=' + StringFromDatumCode(DEMheader.DigitizeDatum)); {$EndIf}
         end
         else begin
            {$If Defined(RecordReadDEM) or Defined(RecordGeotiff)} WriteLineToDebugFile('tDEMDataSet.ReadDEMNow ReadTIFFDEM error, ' + DEMFileName); {$EndIf}
         end;
         TiffImage.Destroy;
         {$If Defined(RecordReadDEM) or Defined(RecordGeotiff)} WriteLineToDebugFile('tDEMDataSet.ReadDEMNow ReadTIFFDEM out, ' + DEMMapProjection.GetProjectionName); {$EndIf}
      end;
   {$EndIf}

   {$If Defined(ExGDAL) or Defined(ExGeotiff)}
   {$Else}
      function TryGDAL(var DEMFileName : PathStr) : boolean;
      begin
         DEMFileName := GDAL_Translate_2_geotiff(DEMFileName);
         if FileExists(DEMFileName) then begin
            Result := ReadTIFFDEM(DEMFileName);
         end
         else Result := false;
      end;
   {$EndIf}


   {$IfDef AllowOddballDEMs}
      {$I ..\common_code\demcoord_read_odd_dems.inc}
   {$EndIf}

   {$IfDef ExDTED}
   {$Else}
      {$I demcoord_read_dted.inc}
   {$EndIf}


    function ReadNewMicroDEMFormat(DEMFileName : PathStr) : boolean;
    label
       CleanUp;
    var
       fFile : file;
       IntKey : integer;
       KeyValue : float64;
       TStr,NextLine,key  : String;
       RewriteDEM,
       ShowProgress : boolean;
       LogRecA : array[0..2048] of Byte;
       //l1 : float64;
       BytesToRead,BytesRead,
       //LogRecAOffset,   i,
       NumRead,HeaderSize,
       x{,HeaderOffset,DEMOffset} : integer;
       Magic : shortstring;
       //aTstr : ANSIString;

       {$IfDef AllowV1V2V3DEMHeaders}
          {$I read_old_md_headers.inc}
       {$EndIf}

         function GoodMagic(Magic : string) : boolean;
         begin
             RewriteDEM := false;
             Result := (Magic = '*MICRODEMv4') or (Magic = '*MICRODEMv5');
             {$IfDef AllowV1V2V3DEMHeaders}
                if (Magic ='*MICRODEM DEM') or (Magic ='*MICRODEM v.2') or (Magic ='*MICRODEM v.3') then begin
                   RewriteDEM := true;
                   Result := true;
                end;
             {$EndIf}
         end;

          procedure ReadHeader;
          var
             i : integer;
          begin
             reset(fFile,1);
             BlockRead(Ffile,LogRecA[1],HeaderSize,NumRead);
             if (NumRead < HeaderSize) then begin
                {$IfDef RecordReadMDDEM} WriteLineToDebugFile('premature file end'); {$EndIf}
                exit;
             end;
             TStr := '';
             for i := 1 to HeaderSize do if (LogRecA[i] <> 0) then TStr := TStr + Chr(LogRecA[i]);
             Magic := Petmar_types.BeforeSpecifiedCharacter(TStr,#13,true,true);
          end;

    begin
       {$IfDef RecordReadMDDEM} WriteLineToDebugFile('ReadNewMicroDEMFormat for ' + DEMFileName); {$EndIf}
       Result := false;
       if not FileExists(DEMFileName) then begin
          {$IfDef RecordReadMDDEM} WriteLineToDebugFile('missing file'); {$EndIf}
          exit;
       end;
       InsureFileIsNotReadOnly(DEMFileName);
       assignFile(fFile,DEMFileName);
       HeaderSize := 1024;

       ReadHeader;

       {$IfDef RecordReadMDDEM} WriteLineToDebugFile('Recognition string=' + Magic); {$EndIf}

       if GoodMagic(Magic) then begin
          {$If Defined(RecordReadMDDEM)} WriteLineToDebugFile('Acceptable MD DEM'); {$EndIf}
          AreaName := ExtractFileNameNoExt(DEMFileName);
          Result := true;
          if (Magic ='*MICRODEMv5') then begin
              HeaderSize := 2048;
              ReadHeader;
          end;
          if (Magic ='*MICRODEMv4') or (Magic ='*MICRODEMv5') then begin
             repeat
                NextLine := Petmar_types.BeforeSpecifiedCharacter(TStr,#13,true,true);
                {$If Defined(RecordReadMDDEM)} WriteLineToDebugFile(NextLine); {$EndIf}
                if (NextLine <> '*END') and (NextLine <> '') or (Copy(NextLine,1,5) = #0#0#0#0#0) then begin
                   Key := Petmar_types.BeforeSpecifiedCharacter(NextLine,'=',true,true);
                   if key = 'numcol' then DEMheader.NumCol := StrToInt(NextLine)
                   else if key = 'numrow' then DEMheader.NumRow := StrToInt(NextLine)
                   else if key = 'maxz' then DEMheader.StoredMaxElev := StrToFloat(NextLine)
                   else if key = 'minz' then DEMheader.StoredMinElev := StrToFloat(NextLine)
                   else if key = 'dx' then DEMheader.DEMxSpacing := StrToFloat(NextLine)
                   else if key = 'dy' then DEMheader.DEMySpacing := StrToFloat(NextLine)
                   else if key = 'llx' then DEMheader.DEMSWCornerX := StrToFloat(NextLine)
                   else if key = 'lly' then DEMheader.DEMSWCornerY := StrToFloat(NextLine)
                   else if key = 'utmzone' then DEMheader.UTMZone := StrToInt(NextLine)
                   else if key = 'fipszone' then
                   else if (key = 'projtype') or (key = 'newproj') then begin
                      DEMheader.DEMUsed := StrToInt(NextLine);
                      if DEMheader.DEMUsed = UTMbasedDEM then DEMMapProjection.PName := UTMEllipsoidal;
                      if DEMheader.DEMUsed = ArcSecDEM then DEMMapProjection.PName := PlateCaree;
                   end
                   else if key = 'precision' then DEMheader.DEMPrecision := tDEMprecision(StrToInt(NextLine))
                   else if key = 'spaceunit' then DEMheader.DataSpacing := tSpacingUnit(StrToInt(NextLine))
                   else if key = 'elevunit' then DEMheader.ElevUnits := tElevUnit(StrToInt(NextLine))
                   else if key = 'digdatum' then DEMheader.DigitizeDatum := tDigitizeDatum(StrToInt(NextLine))
                   else if key = 'hemi' then begin
                      NextLine := UpperCase(NextLine);
                      if NextLine = 'N' then DEMheader.LatHemi := 'N' else DEMheader.LatHemi := 'S';
                   end
                   else if key = 'h_datum' then DEMMapProjection.h_DatumCode  := NextLine
                   else if key = 'VertCSType' then DEMheader.VerticalCSTypeGeoKey := StrToInt(NextLine)
                   else if key = 'pixel_is' then DEMheader.RasterPixelIsGeoKey1025 := StrToInt(NextLine)
                   else if key = 'wkt' then begin
                      DEMheader.wktString := NextLine;
                      if (DEMheader.wktString <> '') then DEMheader.DEMUsed := WKTDEM;
                   end
                   else begin
                       IntKey := StrToInt(Key);
                       KeyValue := StrToFloat(NextLine);
                       DEMmapProjection.SetProjectionParameterFromGeotiffKey(IntKey,KeyValue);
                   end;
                   key := '';
                end;
             until (NextLine = '*END') or (NextLine = '') or (Copy(NextLine,1,5) = #0#0#0#0#0);
             {$IfDef RecordMinMax}  WriteLineToDebugFile(AreaName + '  header read, ' + zRange); {$EndIf}
          end
          else begin
             {$IfDef AllowV1V2V3DEMHeaders}
                ReadThatOldHeader;
            {$EndIf}
          end;

          {$IfDef RecordReadMDDEM} if not DEMMergeInProgress then WriteLineToDebugFile('Read new DEM call DefineDEMvariables'); {$EndIf}
          DefineDEMVariables(true);
          DEMMapProjection.LatHemi := DEMheader.LatHemi;

          if (DEMMapProjection.GeoKeys.Code3075 in [1,7,8,11,14,15,17]) or (DEMMapProjection.PName in [UTMEllipsoidal,PlateCaree]) then begin
             DEMMapProjection.GetProjectParameters;
          end
          else if (DEMheader.DigitizeDatum = UK_OS_grid) then begin
             DEMMapProjection.PName := UK_OS;
             DEMMapProjection.GetProjectParameters;
          end;

          DEMheader.MaxElev := zInMeters(DEMheader.StoredMaxElev);
          DEMheader.MinElev := zInMeters(DEMheader.StoredMinElev);

          {$IfDef RecordReadMDDEM} if not DEMMergeInProgress then WriteLineToDebugFile('Read new DEM, ' + zRange +  '  '  + KeyDEMParams); {$EndIf}

          TStr := UpperCase(ptCopy(AreaName,1,5));

          (*
          if MDdef.WrapETOPO and (UpperCase(ptCopy(AreaName,1,6)) = 'ETOPO5') then begin
             if MDDef.DoubleEtopoImport then DEMheader.Numcol := 2 * DEMheader.NumCol
             else DEMheader.NumCol := 5420;
             DEMheader.DEMSWCornerX := DEMheader.DEMSWCornerX - 360;
          end;
          if (TStr = 'AGES_') then begin
             if MDdef.WrapETOPO then DEMheader.NumCol := 4801;
          end;
          *)

          if ReallyReadDEM and AllocateDEMMemory(InitDEMNone) then begin
             Showprogress := WantShowProgress and (DEMheader.NumCol > 4000) and ShowDEMReadingProgress;
             if ShowProgress then StartProgress('Read DEM ' + AreaName);
             for x := 0 to pred(DEMheader.NumCol) do begin
                if ShowProgress and (x mod 1000 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
                if (DEMheader.DEMPrecision = FloatingPointDEM) then begin
                   BytesToRead := 4 * DEMheader.NumRow;
                   BlockRead(Ffile,ShortFloatElevations^[x]^,BytesToRead,BytesRead);
                end
                else if (DEMheader.DEMPrecision = ByteDEM) then begin
                   BytesToRead := DEMheader.NumRow;
                   BlockRead(Ffile,ByteElevations^[x]^,BytesToRead,BytesRead);
                end
                else if (DEMheader.DEMPrecision = WordDEM) then begin
                   BytesToRead := 2 * DEMheader.NumRow;
                   BlockRead(Ffile,WordElevations^[x]^,BytesToRead,BytesRead);
                end
                else if (DEMheader.DEMPrecision = LongWordDEM) then begin
                   BytesToRead := 4 * DEMheader.NumRow;
                   BlockRead(Ffile,LongWordElevations^[x]^,BytesToRead,BytesRead);
                end
                else begin
                   BytesToRead := 2 * DEMheader.NumRow;
                   BlockRead(Ffile,SmallIntElevations[x]^,BytesToRead,BytesRead);
                end;
                if (BytesToRead <> BytesRead) then begin
                   MessageToContinue('eof reached early for ' + AreaName);
                   Result := false;
                   goto CleanUp;
                end;
             end;
             if (DEMheader.MaxElev - DEMheader.MinElev < 0.01) then begin
                //not clear why this is needed, but download of ETOPO1 had 0,0 for elevation range
                CheckMaxMinElev;
             end;
          end;

          (*
          if MDdef.WrapETOPO and (UpperCase(ptCopy(AreaName,1,6)) = 'ETOPO5') then begin
            for x := 4320 to pred(DEMheader.NumCol) do Move(SmallIntElevations[x-4320]^,SmallIntElevations[x]^,2*DEMheader.NumRow);
          end;
          *)

         Cleanup:;
          if ShowProgress then EndProgress;
          CloseFile(Ffile);

          if RewriteDEM then begin
             WriteNewFormatDEM(DEMFileName);
          end;
          {$IfDef TrackDEMCorners} WriteDEMCornersToDebugFile('Read MD format'); {$EndIf}
          {$IfDef RecordReadMDDEM} writeLineToDebugFile('Read DEM ' + AreaName + ' ' + DEMMapProjection.GetProjectionName); DEMMapProjection.ProjectionParamsToDebugFile('Ending read DEM'); {$EndIf}
          {$IfDef RecordDEMDigitizeDatum}  WriteLineToDebugFile('Leaving Read DEM Now, Digitize Datum=' + StringFromDatumCode(DEMheader.DigitizeDatum)); {$EndIf}
       end
       else begin
          {$IfDef RecordReadMDDEM} WriteLineToDebugFile('NOT acceptable MD DEM, Header=' + TStr); {$EndIf}
       end;
    end;


   procedure ReadSRTMHGTField(fName : PathStr; var Error : boolean);
   var
      inf : file;
      Row,Col,Lat,Long,err : integer;
      fs : int64;
      zsmallint : array[0..3601] of SmallInt;
      zbyte : array[0..3601] of byte;
      zfloat: array[0..3601] of float32;
      zf : float32;
   begin
      {$IfDef RecordReadDEM} WriteLineToDebugFile('Read SRTM HGT file '+ FName); {$EndIf}
      ZeroDEMHeader(DEMheader,false);
      AreaName := ExtractFileName(fName);

      fs := GetFileSize(fName);

      if fs > (1201 * 1201 * 4) then begin
         DEMheader.NumCol := 3601;
         DEMheader.DEMySpacing := 1/3600;
      end
      else begin
         DEMheader.NumCol := 1201;
         DEMheader.DEMySpacing := 1/1200;
      end;

      fs := fs div DEMheader.NumCol div DEMheader.NumCol;

      case fs of
         4 : DEMHeader.DEMPrecision := FloatingPointDEM;
         2 : DEMHeader.DEMPrecision := SmallIntDEM;
         1 : DEMHeader.DEMPrecision := ByteDEM;
      end;

      DEMheader.NumRow := DEMheader.NumCol;
      DEMheader.DEMxSpacing := DEMheader.DEMySpacing;
      DEMheader.RasterPixelIsGeoKey1025 := 2;

      Val(ptCopy(AreaName,2,2),Lat,Err);
      if (AreaName[1] in ['s','S']) then begin
         lat := -lat;
         DEMheader.LatHemi := 'S';
      end;
      Val(ptCopy(AreaName,5,3),Long,Err);
      if (AreaName[4] in ['w','W']) then long := -long;

      DEMheader.UTMZone := GetUTMZone(long + 0.25);
      DEMheader.DEMSWCornerX := long;
      DEMheader.DEMSWCornerY := lat;
      if ReallyReadDEM then begin
         if not AllocateDEMMemory(InitDEMnone) then begin
            closeFile(inf);
            exit;
         end;
         assignFile(inf,FName);
         reset(Inf,1);
         if UpperCase(ptCopy(fName,length(fName)-6,7)) = 'HGT.BIN' then seek(inf,128);
         for Row := pred(DEMheader.NumRow) downto 0 do begin
            case fs of
               4 : BlockRead(inf,zfloat,BytesPerColumn);
               2 : BlockRead(inf,zsmallint,BytesPerColumn);
               1 : BlockRead(inf,zbyte,BytesPerColumn);
            end;
            for Col := 0 to pred(DEMheader.NumCol) do begin
               case fs of
                  1 :  zf := zbyte[Col];
                  2 :  zf := swap(zsmallint[Col]);
                  4 :  begin
                          SwapToShortFloat(zfloat[Col]);
                          zf := zfloat[Col];
                       end;
               end;
               if (abs(zf) < 15000) then SetGridElevation(Col,Row,zf);
            end;
         end;
         if (MDdef.AutoFillHoles) and (not DEMMergeInProgress) then InterpolateAcrossHoles(false);
         closeFile(inf);
         CheckMaxMinElev;
      end;
      Error := false;
   end;


var
   ofName,WKTfile : PathStr;
begin
   {$IfDef RecordReadDEM} WriteLineToDebugFile('enter read DEM for ' +  tFile); {$EndIf}
   InsureFileIsNotReadOnly(tFile);
   DEMFileName := tfile;

   Ext := ExtractFileExt(DEMFileName);
   Error := true;
   WKTfile := FindSingleWKTinDirectory(ExtractFilePath(DEMFileName));
   if ExtEquals(Ext, '.DEM') then begin
      Error := not ReadNewMicroDEMFormat(DEMFileName);
      if Error then begin
         File2Trash(DEMFileName);
        {$If Defined(RecordReadDEM) or Defined(RecordProblems)} HighlightLineToDebugFile('Broken file deleted ' +  DEMFileName); {$EndIf}
      end;
   end
   else if ExtEquals(Ext,'.ASC') and (WKTFile <> '') then begin
      ofName := DEMFileName;
      DEMFileName := ChangeFileExt(DEMFileName,'.tif');
      GDAL_Translate_2_geotiff(ofName,DEMFileName,'',true);
      GDALAssignDEMProjection(DEMFileName,WKTfile);
      Error := not ReadTIFFDEM(DEMFileName);
   end
   else if ExtEquals(Ext, '.TIF') or ExtEquals(Ext, '.TIFF') then begin
      if StrUtils.AnsiContainsText(DEMFileName,'original_') and (not AnswerIsYes('GDAL has tried with the file already; are you sure you want to continue ' + DEMFileName)) then begin
         Result := false;
         exit;
      end;
      Error := not ReadTIFFDEM(DEMFileName);
      {$If Defined(RecordUKOS) or Defined(RecordReadDEM)} WriteLineToDebugFile('Load Geotiff read ' + AreaName + '  ' + DEMMapProjection.GetProjectionName); {$EndIf}
      {$If Defined(RecordUKOS)} DEMMapProjection.ShortProjInfo('AreaName Tiff Read'); {$EndIf}
   end
   else if ExtEquals(Ext, '.HGT') or ExtEquals(Ext, '.SWB') or ExtEquals(Ext, '.HGTS') or ExtEquals(Ext, '.NUM') or ExtEquals(Ext,'.BIN') then begin
      ReadSRTMHGTField(DEMFileName,Error);
   end
   else if GDALGridFormat(Ext) then begin
      Error := not TryGDAL(DEMFileName);
      if (not error) and ExtEquals(Ext, '.ADF') then AreaName := LastSubDir(DEMFileName);
   end;

   {$IfDef AllowOddballDEMs}
      if Error then TryOddball(DEMFileName);
   {$EndIf}

   if Error then begin
      if (Tfile <> '') then begin
         {$IfDef RecordReadDEM} WriteLineToDebugFile(DEMFileName + ' Not acceptable DEM'); {$EndIf}
         if ReportErrors then begin
            MessageToContinue(DEMFileName + MessLineBreak + 'Not acceptable DEM (maybe open elsewhere?)');
         end;
      end;
      Self.Destroy;
      {$IfDef MSWindows}
         Self := Nil;
      {$EndIf}
      Result := false;
      exit;
   end;
   Result := true;
   DeleteMissingDataPoints;
   if (MDdef.AutoFillHoles) and (not DEMMergeInProgress) then InterpolateAcrossHoles(false);
   DefineDEMVariables(TransformToNewDatum);
   DEMstatus := dsSaved;
   {$If Defined(RecordUKOS)} DEMMapProjection.ShortProjInfo('Exit NewArea for ' + AreaName); {$EndIf}
   {$IfDef TrackPixelIs} WriteLineToDebugFile('tDEMDataSet.ReadDEMNow exit, Pixel is = ' + RasterPixelIsString(DEMHeader.RasterPixelIsGeoKey1025)); {$EndIf}
   {$IfDef RecordReadDEM} WriteLineToDebugFile('exit NewArea with DEM: ' + AreaName + '  ' + HorizontalDEMSpacing); {$EndIf}
end;


procedure ConvertUKOSDEMtoUTM(BigDEM : integer; ToUTM : boolean);
var
   fName : PathStr;
   TStr : shortstring;
   xutm,yutm,LatUK,LongUK,LatWGS,LongWGS : array[1..4] of float64;
   //i,
   NewDEM,Col,Row : integer;
   Lat,Long : float64;
   z : float32;
   UKOS : tMapProjection;
   NewHeader : tDEMheader;

       procedure Convert(Col,Row,i : integer);
       begin
          DEMGlb[BigDEM].DEMGridToLatLongDegree(Col,Row,LatUK[i],LongUK[i]);
          MolodenskiyTransformation(LatUK[i],LongUK[i], LatWGS[i],LongWGS[i],UKOS,WGS84DatumConstants);
          if ToUTM then WGS84DatumConstants.ForwardProjectDegrees(LatWGS[i],LongWGS[i],xutm[i],yutm[i]);
       end;

begin
   {$IfDef RecordUKOS} WriteLineToDebugFile('ConvertUKOSDEMtoUTM in, ' + DEMGlb[BigDEM].AreaName); {$EndIf}
    UKOS := tMapProjection.Create;
    UKOS.DefineDatumFromUTMZone('OGB-A',30,'N','ConvertUKOSDEMtoUTM');
    RedefineWGS84DatumConstants(-1);

    Convert(0,0,1);
    Convert(0,pred(DEMGlb[BigDEM].DEMheader.NumRow),2);
    Convert(pred(DEMGlb[BigDEM].DEMheader.NumCol),pred(DEMGlb[BigDEM].DEMheader.NumRow),3);
    Convert(pred(DEMGlb[BigDEM].DEMheader.NumRow),0,4);

    ZeroDEMHeader(NewHeader, true);
   {$IfDef RecordUKOS} WriteLineToDebugFile('ZeroDEMHeader done'); {$EndIf}

    NewHeader.UTMZone := GetUTMZone(-1);
    NewHeader.DEMPrecision := FloatingPointDEM;
    NewHeader.LatHemi := 'N';
    if ToUTM then begin
       NewHeader.DEMxSpacing := DEMGlb[BigDEM].DEMheader.DEMxSpacing;
       NewHeader.DEMySpacing := NewHeader.DEMxSpacing;
       NewHeader.DEMSWCornerX := round(xutm[1]);
       NewHeader.DEMSWCornerY := round(yutm[1]);
       NewHeader.NumCol := round((xutm[3] - NewHeader.DEMSWCornerX) / NewHeader.DEMxSpacing);
       NewHeader.NumRow := round((yutm[3] - NewHeader.DEMSWCornerY) / NewHeader.DEMxSpacing);
    end
    else begin
       NewHeader.DEMxSpacing := 0.1;
       ReadDefault('Arc second spacing',NewHeader.DEMxSpacing);
       NewHeader.DEMxSpacing  := NewHeader.DEMxSpacing / 3600;
       NewHeader.DEMySpacing := NewHeader.DEMxSpacing;
       NewHeader.DataSpacing := SpaceDegrees;
       NewHeader.DEMSWCornerX := LongWGS[1];
       NewHeader.DEMSWCornerY := LatWGS[1];
       NewHeader.NumCol := round((LongWGS[3] - NewHeader.DEMSWCornerX) / NewHeader.DEMxSpacing);
       NewHeader.NumRow := round((LatWGS[3] - NewHeader.DEMSWCornerY) / NewHeader.DEMxSpacing);
    end;
    OpenAndZeroNewDEM(false,NewHeader,NewDEM,'DEM to match maps',InitDEMmissing);
    {$IfDef RecordUKOS} WriteLineToDebugFile('OpenAndZeroNewDEM done'); {$EndIf}
    {$IfDef VCL} StartProgress('Reinterpolate ' + DEMGlb[BigDEM].AreaName); {$EndIf}
    for Col := 0 to pred(NewHeader.NumCol) do begin
       {$IfDef VCL} if (col mod 10 = 0) then UpDateProgressBar(Col/NewHeader.NumCol); {$EndIf}
       for Row := 0 to pred(NewHeader.NumCol) do begin
          DEMGlb[NewDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
          MolodenskiyTransformation(Lat,Long, Lat,Long,WGS84DatumConstants,UKOS);
          if DEMGlb[BigDEM].GetElevFromLatLongDegree(Lat,Long,z) then DEMGlb[NewDEM].SetGridElevation(Col,Row,z);
       end;
    end;
    {$IfDef VCL} EndProgress; {$EndIf}
    {$IfDef RecordUKOS} WriteLineToDebugFile('Reinterpolated'); {$EndIf}
    fName := DEMGlb[BigDEM].DEMFileName;
    if ToUTM then TStr := 'utm' else Tstr := 'geo';

    fName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_' + TStr + ExtractFileExt(fName);
    DEMGlb[NewDEM].CheckMaxMinElev;
    DEMGlb[NewDEM].WriteNewFormatDEM(fName,'New DEM');
    UKOS.Destroy;
   {$IfDef RecordUKOS} WriteLineToDebugFile('Written and out'); {$EndIf}
end;


function CreateNewGlobalGrid(GreenwhichLeft : boolean = true; Resolution : tDEMprecision = FloatingPointDEM; Spacing : float64 = -99) : integer;
var
   OK : boolean;
   NewHeader : tDEMheader;
begin
   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGlobalGrid in'); {$EndIf}
   ZeroDEMHeader(NewHeader, false);
   NewHeader.DEMPrecision := Resolution;
   if GreenwhichLeft then NewHeader.DEMSWCornerX := 0
   else NewHeader.DEMSWCornerX := -180;
   NewHeader.DEMSWCornerY := -90;
   repeat
      if (Spacing < 0) then begin
         NewHeader.DEMxSpacing := 0.02;
         {$IfDef VCL}
         ReadDefault('Data spacing (degrees)',NewHeader.DEMxSpacing);
         {$EndIf}
      end
      else NewHeader.DEMxSpacing := Spacing;

      NewHeader.DEMySpacing := NewHeader.DEMxSpacing;

      NewHeader.NumCol := round(360 / NewHeader.DEMxSpacing);
      NewHeader.NumRow := round(180 / NewHeader.DEMxSpacing);
      OK := (NewHeader.NumRow <= MaxElevArraySize) and (NewHeader.NumCol <= DEMDefs.MaxColsInRAM);
      if not OK then MessageToContinue(ImpGridIncSpace);
    until OK;
    ShowHourglassCursor;
    OpenAndZeroNewDEM(false,NewHeader,Result,'Global DEM',InitDEMmissing);

    {$IfDef NoMapOptions}
    {$Else}
    DEMGlb[Result].SetUpMap(Result,false,MDDef.DefDEMMap);
    {$EndIf}

    ShowDefaultCursor;
   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGlobalGrid out'); {$EndIf}
end;


function MakeNewBlankDEMMap(LatCorner,LongCorner,LatSize,LongSize : float64; xpixels,ypixels : integer) : integer;
var
   NewHeadRecs : tDEMheader;
begin
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('MakeNewBlankDEMMap  Size=' + IntToStr(xpixels) + 'x' + IntToStr(YPixels) + '  SW corner: ' + LatLongDegreeToString(LatCorner,LongCorner)); {$EndIf}

   ZeroDEMHeader(NewHeadRecs, false);
   NewHeadRecs.NumCol := xPixels;
   NewHeadRecs.NumRow := yPixels;
   NewHeadRecs.DEMySpacing := LatSize / pred(yPixels);
   NewHeadRecs.DEMxSpacing := LongSize / pred(xPixels);
   NewHeadRecs.DEMSWCornerX := LongCorner;
   NewHeadRecs.DEMSWCornerY := LatCorner;
   {$IfDef RecordCreateNewDEM}
      WriteLineToDebugFile('xll=' + RealToString(NewHeadRecs.DEMSWCornerX,-18,-8) + ' yll=' + RealToString(NewHeadRecs.DEMSWCornerY,-18,-8) + ' dy=' + RealToString(NewHeadRecs.DEMySpacing,-18,-8) + ' dx=' + RealToString(NewHeadRecs.DEMxSpacing,-18,-8));
   {$EndIf}

   Result := 0;
   OpenAndZeroNewDEM(true,NewHeadRecs,Result,'',InitDEMmissing);

   {$IfDef VCL}
      CreateDEMSelectionMap(Result,false,false,mtDEMBlank);
      DEMGlb[Result].SelectionMap.N11view1Click(Nil);
      DEMGlb[Result].SelectionMap.FormResize(Nil);
   {$EndIf}
end;


function UK_OS_projection(fName : PathStr) : boolean;
begin
   if FileExtEquals(fName,'.asc') then Result := StrUtils.AnsiContainsText(UpperCase(FName),'UK-OS') or StrUtils.AnsiContainsText(UpperCase(FName),'UK_OS')
   else Result := FileExists(ExtractFilePath(fName) + 'UK_OS_Grid.txt');
end;


procedure ReadFusionDTM(FileName : PathStr; WantedDEM : tDEMDataSet; var Error : boolean);
type
   TOneRow = array[0..MaxColsInRAM] of float32;
   tPlansHeader = packed record
      Recognition : array[0..20] of byte;
      DTM_Name : array[0..60] of byte;
      Version : float32;
      llx,lly,
      Minz,maxz,
      Rotation,
      SpaceX,SpaceY : float64;
      NumCol,NumRow : int32;
      SpaceUnits,ZUnits,Ztype,CoordSys,CoordZone,HDatum,VDatum : int16;
      Extra : array[164..199] of byte;
   end;
var
   PlansHeader : tPlansHeader;
   Col,Row,NumRead,RecSize : integer;
   OneRow : ^tOneRow;
   inf : System.file;
begin
   AssignFile(inf,FileName);
   Reset(inf,1);
   BlockRead(inf,PlansHeader,SizeOf(PlansHeader),NumRead);
   if (PlansHeader.SpaceUnits <> 1) or (PlansHeader.ZUnits <> 1) or (PlansHeader.Ztype <> 2) or (PlansHeader.CoordSys <> 1) or (PlansHeader.VDatum <> 2) then begin
      Error := true;
   end
   else begin
      WantedDEM.AreaName := ExtractFileNameNoExt(FileName);
      WantedDEM.DEMheader.DigitizeDatum := NAD83d;
      WantedDEM.DEMheader.DEMUsed := UTMBasedDEM;
      WantedDEM.DEMheader.DataSpacing := SpaceMeters;
      WantedDEM.DEMheader.DEMxSpacing := PlansHeader.SpaceX;
      WantedDEM.DEMheader.DEMySpacing := PlansHeader.SpaceY;
      WantedDEM.DEMheader.DEMSWCornerX := PlansHeader.llx;
      WantedDEM.DEMheader.DEMSWCornerY := PlansHeader.lly;
      WantedDEM.DEMheader.MaxElev := PlansHeader.maxz;
      WantedDEM.DEMheader.MinElev := PlansHeader.minz;
      WantedDEM.DEMheader.NumCol := PlansHeader.NumCol;
      WantedDEM.DEMheader.NumRow := PlansHeader.NumRow;
      WantedDEM.DEMheader.UTMZone := PlansHeader.CoordZone;
      WantedDEM.DEMheader.DEMPrecision := FloatingPointDEM;
      WantedDEM.AllocateDEMMemory(InitDEMnone);
      RecSize := 4*WantedDEM.DEMheader.NumRow;
      GetMem(OneRow,RecSize);
      for Col := 0 to pred(WantedDEM.DEMheader.NumCol) do begin
         BlockRead(inf,OneRow^,RecSize,NumRead);
         for Row := 0 to pred(WantedDEM.DEMheader.NumRow) do begin
            if (abs(OneRow^[Row] + 1) > 0.001) then
               WantedDEM.SetGridElevation(Col,Row,OneRow^[Row]);
         end;
      end;
      FreeMem(OneRow,RecSize);
      Error := false;
   end;
   closeFile(inf);
end;


procedure ReadASCIIArcGrid(FileName : PathStr; WantedDEM : tDEMDataSet; var Error : boolean; ReallyReadDEM : boolean);
type
   tOneRow = array[0..MaxColsInRAM] of float32;
var
   InputFile   : TextFile;
   i : integer;
   NoData,Spacing,xbase,ybase,z,zmult : float64;
   zs : float32;
   ProjFileName,fName2 : PathStr;
   Dir                : DirStr;
   Name               : NameStr;
   Ext                : ExtStr;
   err : integer;
   Line : shortstring;
   BinFile : file;
   zRow : ^toneRow;
   Hemi : ANSIchar;
   MenuStr : ShortString;
   DatCode : ShortString;
   //SPCSZone : shortstring;
   PName :  tProjectType;
   llCornerCoords,GeoLatLong : boolean;


         procedure ReadValue(var TheValue : float64); overload;
         var
            i : integer;
         begin
            readln(InputFile,Line);
            for i := 1 to length(line) do if Line[i] = #9 then Line[i] := ' ';
            val(ptTrim(AfterSpecifiedCharacter(Line,' ')),TheValue,err);
         end;

         procedure ReadValue(var TheValue : int32);  overload;
         var
            i : integer;
         begin
            readln(InputFile,Line);
            for i := 1 to length(line) do if Line[i] = #9 then Line[i] := ' ';
            val(ptTrim(AfterSpecifiedCharacter(Line,' ')),TheValue,err);
         end;

         procedure ReadTheDEM;
         var
            i,x,y : integer;
         begin
            with WantedDEM,DEMheader do begin
               StartProgress('Read ASC DEM ' + Name);
               if FileExists(Dir+Name+'.flt') then begin
                  AssignFile(BinFile,Dir+Name+'.flt');
                  reset(BinFile,1);
                  New(zRow);
                  for y := pred(NumRow) downto 0 do begin
                     UpdateProgressBar( (NumRow-y)/y);
                     BlockRead(Binfile,zRow^,4*NumCol);
                     for x := 0 to Pred(NumCol) do begin
                        zs := zRow^[x];
                        SwapToShortFloat(zs);
                        if abs(zs-NoData) > 0.0001 then begin
                           SetGridElevation(x,y,zs);
                        end;
                     end;
                  end;
                  Dispose(zRow);
                  CloseFile(BinFile);
               end
               else begin
                  reset(InputFile);
                  for i := 1 to 6 do readln(InputFile,MenuStr);
                  for y := pred(NumRow) downto 0 do begin
                     UpdateProgressBar( (NumRow-y)/y);
                     for x := 0 to Pred(NumCol) do begin
                        read(Inputfile,z);
                        if abs(z-NoData) > 0.0001 then begin
                           SetGridElevation(x,y,z * zMult);
                        end;
                     end;
                  end;
               end;
               WantedDEM.CheckMaxMinElev;
               EndProgress;
            end;
         end;


var
   HeaderLines : array[1..6] of shortstring;
begin
   with WantedDEM do begin
      DEMheader.ElevUnits := euMeters;
      FSplit(FileName,Dir,Name,Ext);
      AreaName := Name;
      ApplicationProcessMessages;

      fName2 := ChangeFileExt(FileName,'.prj');
      if not FileExists(fName2) then fName2 := ChangeFileExt(FileName,'.wkt');
      if FileExists(fName2) then begin
         DEMMapProjection.InitializeProjectionFromWKT(fName2);
      end;

      assignFile(InputFile,FileName);
      reset(InputFile);

      for i := 1 to 6 do begin
         readln(InputFile,HeaderLines[i]);
         {$IfDef RecordImport} WriteLineToDebugFile('  ' + MenuStr); {$EndIf}
      end;
      llCornerCoords := UpperCase(Copy(HeaderLines[1],1,9)) = 'XLLCORNER';
      reset(InputFile);

      ReadValue(DEMheader.NumCol);
      ReadValue(DEMheader.NumRow);
      ReadValue(xbase);
      ReadValue(ybase);
      ReadValue(Spacing);

      if (StrUtils.ANSIContainsText(HeaderLines[6],'NODATA')) then begin
         ReadValue(NoData);   //UK 50 m grid does not have NODATA line
      end;

      if llCornerCoords then begin
         xbase := xbase + 0.5 * Spacing;
         ybase := ybase + 0.5 * Spacing;
      end
      else begin
         xbase := xbase - 0.5 * Spacing;
         ybase := ybase - 0.5 * Spacing;
      end;

      DEMheader.LatHemi := MDdef.DefaultLatHemi;

      if (MDDef.DEMZunits = zuMeters) then ElevationMultiple := 1
      else ElevationMultiple := 1 / FeetToMeters;
      DEMheader.ElevUnits := euMeters;

      Zmult := 1;
      if StrUtils.AnsiContainsText(UpperCase(FileName),'_MM_UNITS') then zMult := 0.001;

      DEMheader.DigitizeDatum := WGS84d;
      if UK_OS_projection(FileName) then begin
         DEMheader.DataSpacing := SpaceMeters;
         DEMheader.DEMUsed := UTMBasedDEM;
         DEMheader.DigitizeDatum := UK_OS_grid;
         DEMheader.UTMZone := 1;
         CheckUK_OS;
      end
      else if FileExists(Dir + 'rect.txt') then begin
        DEMheader.DataSpacing := SpaceMeters;
        DEMheader.DEMUsed := UTMBasedDEM;
        DEMheader.DigitizeDatum := Rectangular;
        DEMheader.UTMZone := 1;
      end
      else if (abs(xBase) > 180) then begin
         fName2 := Dir + Name + '.prj';
         if FileExists(fName2) then begin
            DEMMapProjection.InitializeProjectionFromWKT(fName2);
            DEMMapProjection.DefineDatumFromUTMZone(DEMMapProjection.h_DatumCode,GetUTMZone(DEMMapProjection.Long0 / DegToRad),MDDef.DefaultLatHemi,'read dem 3');
         end
         else if not DEMMapProjection.CheckForAllInDirectoryWKT(Dir) then begin
            DatCode := 'WGS84';
            Hemi := MDDef.DefaultLatHemi;
            if DEMMergeInProgress and SubsequentDEM then begin
               DEMheader.UTMZone := MDDef.DefaultUTMzone;
               Hemi := MDDef.DefaultLongHemi;
            end
            else begin
               {$IfDef VCL} GetMapParametersSPCSOption(ProjFileName,Hemi,DEMheader.UTMZone,DatCode,PName,GeoLatLong,false); {$EndIf}
               if DEMMergeInProgress then begin
                  MDDef.DefaultUTMzone := DEMheader.UTMZone;
                  MDDef.DefaultLongHemi := Hemi;
               end;
            end;
         end;
         DEMheader.DataSpacing := SpaceMeters;
         DEMheader.DigitizeDatum := DatumCodeFromString(DatCode);
         DEMheader.DEMUsed := UTMBasedDEM;
      end
      else begin
         DEMheader.DEMUsed := ArcSecDEM;
         DEMheader.DataSpacing := SpaceDegrees;
         WantedDEM.DEMMapProjection.PName := PlateCaree;
      end;

      DEMheader.DEMxSpacing := Spacing * RectSpacingFactor(DEMheader.DataSpacing);
      DEMheader.DEMySpacing := Spacing * RectSpacingFactor(DEMheader.DataSpacing);
      DEMheader.DEMSWCornerX := xbase * RectSpacingFactor(DEMheader.DataSpacing);
      DEMheader.DEMSWCornerY := ybase * RectSpacingFactor(DEMheader.DataSpacing);

      DEMheader.DEMPrecision := FloatingPointDEM;

      if ReallyReadDEM then begin
         if (DEMheader.NumCol = 0) or (DEMheader.NumRow = 0) or (not AllocateDEMMemory(InitDEMnone)) then begin
            Error := true;
            exit;
         end;
         ReadTheDEM;
      end;
      closeFile(InputFile);

      {$IfDef RecordImport} WriteLineToDebugFile(' DEM read OK'); {$EndIf}
      Error := false;
   end;
end;


function LoadNewDEM(var WantedDem : integer; var FullFileName : PathStr; CreateMap : boolean = true; DEMMessage : shortstring = 'New DEM'; MapTitleBar : shortstring = ''; DrawTheMap : boolean = true; ChangeDefaultName : boolean = true) : boolean;
var
   i       : integer;
   SavedName : PathStr;
begin
   {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM)} WriteLineToDebugFile('function LoadNewDEM in ' + FullFileName); {$EndIf}
   Result := false;
   DEMNowDoing := Calculating;

   {$IfDef VCL}
      if ValidDBfName(FullFileName) then begin
         if not AnswerIsYes('File appears to be a vector dataset; are you sure you want to try to open it as a raster DEM/grid') then begin
            exit;
         end;
      end;
      ChangeDEMNowDoing(JustWandering);
   {$EndIf}

   if CheckIfCompressedFile(FullFileName) then exit;

   SavedName := LastDEMName;
   ShowHourglassCursor;
   {$IfDef RecordReadDEM} WriteLineToDebugFile('LoadNewDEM calling new area for ' + FullFileName); {$EndIf}

   if NewArea(True,WantedDEM,DEMMessage,FullFileName) then begin
      {$If Defined(RecordZRange) or Defined(RecordReadDEM)} WriteLineToDebugFile('LoadNewDEM NewArea over, ' + DEMGlb[WantedDEM].AreaName + '  ' + DEMGlb[WantedDEM].Zrange); {$EndIf}
      {$If Defined(TrackHorizontalDatum)}
         WriteLineToDebugFile('LoadNewDEM NewArea over, ' + DEMGlb[WantedDEM].AreaName + '  ' +  DEMGlb[WantedDEM].DEMMapProjection.h_DatumCode + '  ' +
           StringFromDatumCode(DEMGlb[WantedDEM].DEMheader.DigitizeDatum));
      {$EndIf}
      Result := true;
      if CreateMap then begin
        {$IfDef VCL}
           {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM)} WriteLineToDebugFile('LoadNewDEM 2 SetUpSelectionsMap ' + DEMGlb[WantedDEM].AreaName + '  ' + DEMGlb[WantedDEM].DEMMapProjection.GetProjectionName); {$EndIf}
           CreateDEMSelectionMap(WantedDEM,DrawTheMap,MDDef.DefElevsPercentile,mtDEMBlank);
           {$IfDef RecordZRange} WriteLineToDebugFile('LoadNewDEM Selection map over, '+ DEMGlb[WantedDEM].AreaName + '  ' + DEMGlb[WantedDEM].Zrange); {$EndIf}
           {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM)} WriteLineToDebugFile('LoadNewDEM done SetUpSelectionsMap ' + DEMGlb[WantedDEM].AreaName + '  ' + DEMGlb[WantedDEM].DEMMapProjection.GetProjectionName); {$EndIf}
            if MDDef.USOutlinesOnDEMs then begin
               AddOrSubtractOverlay(DEMGlb[WantedDEM].SelectionMap,ovoUSOUtlines,true);
               DEMGlb[WantedDEM].SelectionMap.DoFastMapRedraw;
            end;
            {$If Defined(TrackHorizontalDatum)}  DEMGlb[WantedDEM].TrackHorizontalDatumDebugLog('LoadNewDEM map created,'); {$EndIf}
         {$EndIf}
      end;
   end
   else begin
      WantedDEM := 0;
      {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM)} WriteLineToDebugFile('LoadNewDEM failed, ' + FullFileName); {$EndIf}
   end;
   if not ChangeDefaultName then LastDEMName := SavedName;
   {$IfDef VCL} if (WMDEM <> Nil) then WMDEM.SetMenusForVersion; {$EndIf}
   ShowDefaultCursor;
   StopSplashing;
   UpdateMenusForAllMaps;
   ChangeDEMNowDoing(JustWandering);

   {$IfDef TrackLastDEMName} WriteLineToDebugFile('exit LoadNewDEM out, ' + LastDEMName); {$EndIf}
   {$If Defined(RecordShortDefineDatum) or Defined(RecordReadDEM)  or Defined(TimeLoadDEM)}
      if (WantedDEM <> 0) then WriteLineToDebugFile('Exit LoadNewDEM=' + IntToStr(WantedDEM) + '  ' + DEMGlb[WantedDEM].KeyDEMParams(true));
   {$EndIf}
   {$If Defined(TrackHorizontalDatum)}  DEMGlb[WantedDEM].TrackHorizontalDatumDebugLog('exit LoadNewDEM,'); {$EndIf}
end;


function OpenAndZeroNewDEM(TransformToNewDatum : boolean; NewHeader : tDEMheader; var WantedDEM : integer; DEMname : ShortString; InitDEMMode : byte; InitialValue : float64 = 0) : boolean;
begin
   {$IfDef RecordCreateNewDEM} writelineToDebugFile('OpenAndZeroNewDEM ' + NewHeader.WKTString); {$EndIf}
   try
      OpeningNewGrid := true;
      WantedDEM := 0;
      ShowHourglassCursor;
      OpenDEMDataStructures(WantedDEM);
      if (DEMName <> '') then DEMGlb[WantedDEM].AreaName := DEMName;
      DEMGlb[WantedDEM].DEMheader := NewHeader;
      Result := DEMGlb[WantedDEM].AllocateDEMMemory(InitDEMmode,InitialValue);
      DEMGlb[WantedDEM].DefineDEMVariables(TransformToNewDatum);
      DEMGlb[WantedDEM].DEMstatus := dsUnsaved;
   finally
      OpeningNewGrid := false;
   end;
   {$IfDef RecordCreateNewDEM} WritelineToDebugFile('Depart with DEMHeader.WKTString=' + DEMGlb[WantedDEM].DEMHeader.WKTString); {$EndIf}
end;


procedure OpenDEMDataStructures(var WantedDEM : integer);
begin
   if (WantedDEM = 0) then repeat
      inc(WantedDEM);
   until (WantedDEM > MaxDEMDataSets) or (DEMGlb[WantedDEM] = Nil);
   if (WantedDEM > MaxDEMDataSets) then begin
      {$IfDef RecordReadDEM} WriteLineToDebugFile(TooManyDEMsOpenString); {$EndIf}
      WantedDEM := 0;
      MessageToContinue(TooManyDEMsOpenString);
   end
   else begin
      DEMGlb[WantedDEM] := tDEMDataSet.Create(WantedDEM);
   end;
end;


function NewArea(TransformToNewDatum : boolean; var ImportingDEM : integer; DEMMessage : shortstring; var TFile : PathStr; DEMtoUse : integer = 0) : boolean;
var
   NumPts : int64;
begin
   {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM) or Defined(BasicOpens)} if (TFile <> '') and not FileExists(TFile) then WriteLineToDebugFile('NewArea enter, missing File=' + TFile) else WriteLineToDebugFile('NewArea enter ' + tFile); {$EndIf}
   DEMStatsString := '';
   if not FileExists(TFile) then tFile := '';

   if (TFile = '') then begin
      {$IfDef VCL}
         if PathIsValid(tFile) then LastDEMName := tfile
         else if not FileExists(LastDEMName) then LastDEMName := WriteDEMDir;
         {$IfDef TrackLastDEMName} WriteLineToDebugFile('call GetFileMultipleMask with ' + LastDEMName); {$EndIf}
         if (not GetFileMultipleMask(DEMMessage,DEMFilterMasks,tfile,MDDef.DefaultDEMFilter)) or (not FileExists(tFile)) then begin
            Result := false;
            exit;
         end;
      {$EndIf}
      {$IfDef RecordReadDEM} if (not DEMMergeInProgress) then WriteLineToDebugFile('DEM selected: ' + LastDEMName); {$EndIf}
   end
   else begin
      {$IfDef RecordReadDEM} if (not DEMMergeInProgress) then WriteLineToDebugFile('DEM passed in: ' + tfile); {$EndIf}
   end;

   CheckFileNameForSpaces(tFile);
   LastDEMName := tFile;

   ImportingDEM := DEMtoUse;
   ShowHourglassCursor;
   OpenDEMDataStructures(ImportingDEM);
   {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM)} WriteLineToDebugFile('off to ReadDEMNow, DEM=' + IntToStr(ImportingDEM)); {$EndIf}
   Result := DEMGlb[ImportingDEM].ReadDEMNow(tFile,TransformToNewDatum);
   {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM)} if (not Result) then WriteLineToDebugFile('ReadDEM failure'); {$EndIf}

   if MDdef.MissingToSeaLevel and DEMGlb[ImportingDEM].ElevationDEM then DEMGlb[ImportingDEM].MissingDataToConstantVelue;
   if MDdef.SeaLevelToMissing and DEMGlb[ImportingDEM].ElevationDEM then DEMGlb[ImportingDEM].MarkInRangeMissing(-0.01,0.01,NumPts);

   if (ImportingDEM <> 0) and FileExists(DEMGlb[ImportingDEM].MetadataFileName) then DEMGlb[ImportingDEM].DEMmetadata.LoadFromFile(DEMGlb[ImportingDEM].MetadataFileName);
   {$If Defined(RecordReadDEM) or Defined(TimeLoadDEM) or Defined(ShortDEMLoad)}
      WriteLineToDebugFile('exit ReadDEMNow, ' + DEMGlb[ImportingDEM].AreaName + '  ' + DEMGlb[ImportingDEM].KeyDEMParams(true) + ' ' + DEMGlb[ImportingDEM].DEMHorizontalSpacingSummary);
   {$EndIf}
   {$If Defined(TrackHorizontalDatum)}
      WriteLineToDebugFile('exit ReadDEMNow, ' + DEMGlb[ImportingDEM].AreaName + '  ' +  DEMGlb[ImportingDEM].DEMMapProjection.h_DatumCode + '  ' +
        StringFromDatumCode(DEMGlb[ImportingDEM].DEMheader.DigitizeDatum));
   {$EndIf}
end;





