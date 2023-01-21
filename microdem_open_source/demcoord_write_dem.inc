{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}
{include file for demcoord.pas       }
{____________________________________}



{$IfDef AllowOddballDEMexports}
   {$I ..\common_code\demcoord_write_odd_dems.inc}
{$EndIf}

   {$IfDef ExDTED}
   {$Else}
      DEM_save_DTED,
   {$EndIf}



procedure tDEMDataSet.SaveGridSubsetGeotiff(DEMGridLimits : tGridLimits; fName : PathStr = '');
var
   tName : PathStr;
   NewDEM : integer;
begin
   tName := MDTempDir + 'sub-' + AreaName + '.dem';
   WriteNewFormatDEM(DEMGridLimits,tName);
   LoadNewDEM(NewDEM,tName,false);
   DEMGlb[NewDEM].SaveAsGeotiff(fName);
   CloseSingleDEM(NewDEM);
end;


procedure tDEMDataSet.SaveSpecifiedPartOfDEM(var FileName : PathStr; Limits : tGridLimits);
begin
   WriteNewFormatDEM(Limits,FileName);
end;


procedure tDEMDataSet.SavePartOfDEMWithData(var FileName : PathStr);
var
   Limits : tGridLimits;
begin
   Limits := FullDEMGridLimits;
   FilledGridBox(Limits);
   WriteNewFormatDEM(Limits,FileName);
end;


procedure tDEMDataSet.WriteNewFormatDEM(var FileName : PathStr; WhatFor : shortstring = '');
begin
   DEMFileName := FileName;
   WriteNewFormatDEM(FullDEMGridLimits,FileName,WhatFor);
end;


procedure tDEMDataSet.WriteNewFormatDEM(Limits : tGridLimits; var FileName : PathStr; WhatFor : shortstring = '');

      function HeaderToString(NewHeadRecs : tDEMheader) : ANSIstring;
      var
         i : integer;
      begin
         Result := '*MICRODEMv5' + #13 +
                   'numcol=' + IntToStr(NewHeadRecs.NumCol) + #13 +
                   'numrow=' + IntToStr(NewHeadRecs.NumRow) + #13 +
                   'maxz=' + RealToString(NewHeadRecs.StoredMaxElev,-18,-6) + #13 +
                   'minz=' + RealToString(NewHeadRecs.StoredMinElev,-18,-6) + #13 +
                   'dx=' + RealToString(NewHeadRecs.DEMxSpacing,-18,-10) + #13 +
                   'dy=' + RealToString(NewHeadRecs.DEMySpacing,-18,-10) + #13 +
                   'llx=' + RealToString(NewHeadRecs.DEMSWCornerX,-18,-8) + #13 +
                   'lly=' + RealToString(NewHeadRecs.DEMSWCornerY,-18,-8) + #13 +
                   'utmzone=' + IntToStr(NewHeadRecs.UTMZone) + #13 +
                   'newproj=' + IntToStr(NewHeadRecs.DEMUsed)  + #13 +
                   'precision=' + IntToStr(Ord(tDEMprecision(NewHeadRecs.DEMPrecision)))  + #13 +
                   'spaceunit=' +IntToStr(Ord(tSpacingUnit(NewHeadRecs.DataSpacing)))  + #13 +
                   'elevunit=' + IntToStr(Ord(tElevUnit(NewHeadRecs.ElevUnits)))  + #13 +
                   'digdatum=' + IntToStr(Ord(tDigitizeDatum(NewHeadRecs.DigitizeDatum)))  + #13 +
                   'hemi=' + NewHeadRecs.LatHemi + #13 +
                   'h_datum=' + DEMMapProjection.h_DatumCode  + #13 +
                   'VertCSType=' + IntToStr(DEMheader.VerticalCSTypeGeoKey) + #13 +
                   'pixel_is=' + IntToStr(DEMheader.RasterPixelIsGeoKey1025) + #13 +
                   'wkt=' + DEMheader.wktString + #13;
          if (DEMMapProjection.GeoKeys.Code3075 <> 0) then begin
             Result := Result + '3075=' + IntToStr(DEMMapProjection.GeoKeys.Code3075) + #13;
             for I := 1 to DEMMapProjection.GeoKeys.NumKeys do
                Result := Result + IntToStr(DEMMapProjection.GeoKeys.KeyCode[i]) + '=' + RealToString(DEMMapProjection.GeoKeys.KeyVal[i],-18,-8) + #13;
          end;
          Result := Result + '*END' + #13;
      end;


var
   fFile : file;
   x    : integer;
   MinZ,MaxZ : float32;
   Dir      : DirStr;
   Name     : NameStr;
   Ext      : ExtStr;
   HeaderString : ANSIstring;
   HeaderBytes : array[0..2048] of byte;
   NewHeadRecs : tDEMheader;
   avgelev : float32;
begin
   {$If Defined(RecordWriteDEM) or Defined(RecordZRange)} WriteLineToDebugFile('WriteNewFormatDEM Elev range: ' + ZRange); {$EndIf}

    {$IfDef ExGeotiffWrite}
    {$Else}
       if (FileExtEquals(FileName,'.TIF')) then begin
          SaveAsGeotiff(FileName);
          exit;
       end;
    {$EndIf}

   {$IfDef VCL}
   if (FileName = '') or (not FileExtEquals(FileName,'.DEM')) then begin
      if (FileName = '') or (not PathIsValid(FileName)) then begin
         FileName := AreaName;
         StripInvalidPathNameChars(Filename);
         FileName := DEMDefs.WriteDEMDir + FileName;
      end;
      if (WhatFor <> '') then WhatFor := ' for ' + WhatFor;

      if not GetFileNameDefaultExt('New DEM' + WhatFor,'DEM|*.dem',FileName) then exit;
      DEMDefs.WriteDEMDir := ExtractFilePath(FileName);
   end;
   {$EndIf}

   try
      ClosingMapNotAllowed := true;
      FSplit(FileName,Dir,Name,Ext);
      SafeMakeDir(Dir);

      {$IfDef RecordWriteDEM} WriteLineToDebugFile('Start DEM write ' + FileName); {$EndIf}

      NewHeadRecs := DEMheader;
      NewHeadRecs.NumCol := succ(Limits.XGridHigh - Limits.XGridLow);
      NewHeadRecs.NumRow := succ(Limits.YGridHigh - Limits.YGridLow);
      if (DEMheader.NumCol <> NewHeadRecs.NumCol) or (DEMheader.NumRow <> NewHeadRecs.NumRow) then begin
         NewHeadRecs.DEMSWCornerX := DEMHeader.DEMSWCornerX + Limits.XGridLow * DEMheader.DEMxSpacing;
         NewHeadRecs.DEMSWCornerY := DEMHeader.DEMSWCornerY + Limits.YGridLow * DEMheader.DEMySpacing;
      end;

      BoxAreaExtremeElevations(Limits,MinZ,MaxZ,avgelev);
      NewHeadRecs.MinElev := MinZ;
      NewHeadRecs.MaxElev := MaxZ;
      NewHeadRecs.StoredMinElev := MinZ / ElevationMultiple;
      NewHeadRecs.StoredMaxElev := MaxZ / ElevationMultiple;

       assignFile(fFile,FileName);
       rewrite(fFile,1);
       FillChar(HeaderBytes,2049,0);
       HeaderString := HeaderToString(NewHeadRecs);
       StringToByteArray(HeaderString,HeaderBytes);
       BlockWrite(fFile,HeaderBytes[1],2048);

       if ShowDEMReadingProgress and (NewHeadRecs.NumCol > 4000) then StartProgress('Write ' + ExtractFileName(FileName));
       ShowHourglassCursor;
       for x := Limits.XGridLow to Limits.XGridHigh do begin
          if (x mod 50 = 0) and ShowDEMReadingProgress and (NewHeadRecs.NumCol > 4000) then UpDateProgressBar(x/pred(NewHeadRecs.NumCol));
          if (NewHeadRecs.DEMPrecision = FloatingPointDEM) then begin
             BlockWrite(Ffile,ShortFloatElevations^[x]^[Limits.YGridLow],4*NewHeadRecs.NumRow);
          end
          else if (NewHeadRecs.DEMPrecision = ByteDEM) then begin
             BlockWrite(Ffile,ByteElevations^[x]^[Limits.YGridLow],NewHeadRecs.NumRow);
          end
          else if (NewHeadRecs.DEMPrecision = WordDEM) then begin
             BlockWrite(Ffile,WordElevations^[x]^[Limits.YGridLow],2*NewHeadRecs.NumRow);
          end
          else if (NewHeadRecs.DEMPrecision = LongWordDEM) then begin
             BlockWrite(Ffile,LongWordElevations^[x]^[Limits.YGridLow],4*NewHeadRecs.NumRow);
          end
          else begin
             BlockWrite(Ffile,SmallIntElevations[x]^[Limits.YGridLow],2*NewHeadRecs.NumRow);
             {$IfDef RecordFullWriteDEM} writeLineToDebugFile('Last in column: ' + IntToStr(DTEDCols[x]^[pred(HeadRecs.NumRow)])); {$EndIf}
          end;
       end;
       CloseFile(FFile);

       if FileExists(VATFileName) then begin
         FileName := ChangeFileExt(FileName,'.vat.dbf');
         CopyFile(VATFileName,FileName);
       end;

    finally
       EndProgress;
       DEMstatus := dsSaved;
       ClosingMapNotAllowed := false;
    end;
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('End DEM write'); {$EndIf}
end;



{$IfDef ExGeotiffWrite}
{$Else}

   {$Define WriteSminSmax}
   {$Define WriteMissing}

   procedure StreamLongInt(var TIFFFile : TBufferedFileStream; Value : LongInt);  inline;
   begin
      TiffFile.Write(Value,4);
   end;

   procedure StreamWord(var TIFFFile : TBufferedFileStream; Value : Word); inline;
   begin
      TiffFile.Write(Value,2);
   end;

   procedure StreamFieldEntry(var TIFFFile : TBufferedFileStream; Tag,TypeField : word; Length,Offset : LongInt);
   begin
      StreamWord(TIFFFile,Tag);
      StreamWord(TIFFFile,TypeField);
      StreamLongInt(TIFFFile,Length);
      StreamLongInt(TIFFFile,Offset);
      {$IfDef GeotiffSave} WriteLineToDebugFile('tag=' + IntegerToString(Tag,5) + '  type=' + TiffTypeName(TypeField) + '  Length=' + IntegerToString(Length,4) + '  Value=' + IntToStr(Offset));    {$EndIf}
   end;

   procedure StreamWordFieldEntry(var TIFFFile : TBufferedFileStream; Tag,TypeField,Length,Offset : word);
   begin
      StreamWord(TIFFFile,Tag);
      StreamWord(TIFFFile,TypeField);
      StreamWord(TiffFile,Length);
      StreamWord(TIFFFile,Offset);
      {$IfDef GeotiffSave} WriteLineToDebugFile('tag=' + IntegerToString(Tag,5) + '  type=' + TiffTypeName(TypeField) + '  Length=' + IntegerToString(Length,4) + '  Value=' + IntToStr(Offset));    {$EndIf}
   end;

   procedure StreamFieldEntryIncrementOffset(var TIFFFile : TBufferedFileStream; Tag,TypeField : word; Length : longint; var Offset : LongInt);
   begin
      StreamWord(TIFFFile,Tag);
      StreamWord(TIFFFile,TypeField);
      StreamLongInt(TIFFFile,Length);
      StreamLongInt(TIFFFile,Offset);
      {$IfDef GeotiffSave} WriteLineToDebugFile('tag=' +  IntegerToString(Tag,5) + ' type=' + TiffTypeName(TypeField) + ' Length=' + IntegerToString(Length,4) + ' Offset=' + IntToStr(Offset) + '  Bytes=' + IntToStr(Length * GeotiffTypeSize(TypeField)));     {$EndIf}
      Offset := Offset + Length * GeotiffTypeSize(TypeField);
   end;


procedure tDEMDataSet.SaveAsGeotiff(SaveName : PathStr = '');
var
   missing : double;
   WField : word;
   Offset,NumEnt, ASCIIsize,SF,MissingSize,WidthImage,HeightImage,BytesPer,RowBytes,GeokeyEntries : integer;
   TIFFHeader : tTiffHeader;
   b  : array[1..4] of byte;
   TIFFFile : TBufferedFileStream;
   MissingStr,AsciiStr : ShortString;


         procedure WriteTheDEM;
         //we are writing from the top left down, using default and not setting tag 274
         var
            LongRun  : ^tByteRow;
            zees     : ^tIntRow;
            FloatRow : ^tFloatRow;
            x,y : integer;
            z : float32;
         begin
            if ShowDEMReadingProgress then StartProgress('Save ' + ExtractFileName(SaveName));
            if TiffHeader.BitsPerSample = 8 then New(LongRun)
            else if TiffHeader.BitsPerSample = 16 then new(zees)
            else New(FloatRow);
            for y := pred(DEMheader.NumRow) downto 0 do begin
               if (TiffHeader.BitsPerSample = 32) then begin
                  for x := 0 to pred(DEMheader.NumCol) do begin
                     if GetElevMeters(x,y,z) then FloatRow^[x] := z
                     else FloatRow^[x] := MDDef.GeotiffMissingValue;
                  end;
                  TiffFile.Write(FloatRow^[0],RowBytes);
               end
               else if (TiffHeader.BitsPerSample = 16) then begin
                  for x := 0 to pred(DEMheader.NumCol) do begin
                     if GetElevMeters(x,y,z) then zees^[x] := round(z)
                     else zees^[x] := MDDef.GeotiffMissingValue;
                  end;
                 TiffFile.Write(zees^[0],RowBytes);
               end
               else if (TiffHeader.BitsPerSample = 8) then begin
                  for x := 0 to pred(DEMheader.NumCol) do begin
                     if GetElevMeters(x,y,z) then LongRun^[x] := ValidByteRange(round(z))
                     else LongRun^[x] := 255;
                  end;
                  TiffFile.Write(LongRun^[0],RowBytes);
               end;
               if (y mod 50 = 0) and ShowSatProgress then UpdateProgressBar((pred(DEMheader.NumRow)-y)/pred(DEMheader.NumRow));
            end {for y};
            if (TiffHeader.BitsPerSample = 8) then Dispose(LongRun)
            else if (TiffHeader.BitsPerSample = 16) then Dispose(zees)
            else Dispose(FloatRow);
            if ShowDEMReadingProgress then EndProgress;
         end;


         procedure BlockWriteDouble(z : Double);
         begin
            TiffFile.Write(z,SizeOf(Double));
         end;

begin
   if (SaveName = '') then begin
      if (DEMFileName <> '') then SaveName := ChangeFileExt(DEMFileName,'.tif')
      else SaveName := areaName + '.tif';
      {$IfDef VCL}
         if not GetFileNameDefaultExt('GEOTIFF name','GEOTIFF|*.tif',SaveName) then exit;
      {$EndIf}
   end;
   {$IfDef GeotiffSave} WriteLineToDebugFile('tDEMDataSet.SaveAsGeotiff in, ' + SaveName); {$EndIf}

   case DEMheader.DEMPrecision of
      ByteDEM             : TiffHeader.BitsPerSample := 8;
      WordDEM,SmallIntDEM : TiffHeader.BitsPerSample := 16;
      LongWordDEM,
      FloatingPointDEM    : TiffHeader.BitsPerSample := 32;
   end;

   case DEMheader.DEMPrecision of
      WordDEM,ByteDEM   : SF := 1;
      SmallIntDEM       : SF := 2;
      FloatingPointDEM  : SF := 3;
   end;

    NumEnt := 15;
   {$IfDef WriteSminSmax}
      NumEnt := NumEnt + 2;
   {$EndIf}
   {$IfDef WriteMissing}
      NumEnt := NumEnt + 1;
   {$EndIf}

   GeoKeyEntries := 5;
   WidthImage := DEMheader.NumCol;
   HeightImage := DEMheader.NumRow;
   BytesPer := TiffHeader.BitsPerSample div 8;
   RowBytes := BytesPer * WidthImage;
   TiffHeader.StripByteCounts := WidthImage * HeightImage * BytesPer;

   if (DEMheader.DEMUsed = UTMbasedDEM) then AsciiStr := ' UTM|'
   else AsciiStr := ' Geographic (Lat/Lon)|Units = dd|OK';
   AsciiStr := MapFromMICRODEMstr + AsciiStr + '|mdz=' + IntToStr(ord(DEMheader.ElevUnits)) + '/' + ElevUnitsAre(DEMheader.ElevUnits) + #0;
   ASCIIsize := length(AsciiStr);

   MissingStr := RealToString(MDDef.GeotiffMissingValue,-12,-2) + #0;
   MissingSize := Length(MissingStr);

   if (DEMheader.DEMUsed = UTMbasedDEM) or ((DEMMapProjection <> Nil) and (DEMMapProjection.PName = UK_OS)) then wfield := 1
   else wField := 2;
   if (DEMheader.DEMPrecision = ByteDEM) then Missing := 255
   else Missing := MDDef.GeotiffMissingValue;

   TiffHeader.StripOffsets := NumEnt*12 +  8 {TIFF version plus first offset} + 2 {NumFields} + 4 {0 ending offset};
   TiffFile := TBufferedFileStream.Create(SaveName, fmCreate);

   {Byte Order}             B[1] := 73;   B[2] := 73;  TiffFile.Write(B,2);
   {TIFF version number}    B[1] := 42;   B[2] := 0;   TiffFile.Write(B,2);
   StreamLongInt(TiffFile,8);  {Offset to first Image File Directory}
   StreamWord(TiffFile,NumEnt);

   StreamFieldEntry(TIFFFile,256,3,1,WidthImage);            //1
   StreamFieldEntry(TIFFFile,257,3,1,HeightImage);           //2
   StreamFieldEntry(TIFFFile,258,3,1,TiffHeader.BitsPerSample);   //3
   StreamFieldEntry(TIFFFile,259,3,1,1);                     //4  Compression
   StreamFieldEntry(TIFFFile,262,3,1,1);                     //5   PhotometricInterpretation
   StreamFieldEntry(TIFFFile,273,3,1,TiffHeader.StripOffsets);   //6

   Offset := TiffHeader.StripOffsets + TiffHeader.StripByteCounts;

   StreamFieldEntry(TIFFFile,277,3,1,1);                            //7     SamplesPerPixel
   StreamFieldEntry(TIFFFile,278,4,1,HeightImage);                  //8    {RowsPerStrip}
   StreamFieldEntry(TIFFFile,279,4,1,TiffHeader.StripByteCounts);   //9
   StreamFieldEntry(TIFFFile,284,3,1,1);                            //10  {PlanarConfiguration}
   StreamFieldEntry(TIFFFile,339,3,1,SF);                           //11

   {$IfDef WriteSminSmax}
      StreamFieldEntryIncrementOffset(TIFFFile,340,12,1,Offset);        //12
      StreamFieldEntryIncrementOffset(TIFFFile,341,12,1,Offset);        //13
   {$EndIf}

   {start Geotiff entries}
   StreamFieldEntryIncrementOffset(TIFFFile,33550,12,3,Offset);     //14     scale x,y,z
   StreamFieldEntryIncrementOffset(TIFFFile,33922,12,6,Offset);     //15    ModelTiePointOffset
   StreamFieldEntryIncrementOffset(TIFFFile,34735,3,4*GeoKeyEntries,Offset);  //16    Geotiff GeoKeyDirectoryOffset
   StreamFieldEntryIncrementOffset(TIFFFile,34737,2,ASCIIsize,Offset);  //17

   {$IfDef WriteMissing}
      StreamFieldEntryIncrementOffset(TIFFFile,42113,2,MissingSize,Offset);       //18   Missing Data
   {$EndIf}
   {end Geotiff}

   {0 offset marking end} StreamLongInt(TIFFFile,0);

   {$IfDef GeotiffSave} WriteLineToDebugFile('starting DEM write, filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
   WriteTheDEM;
   {$IfDef GeotiffSave} WriteLineToDebugFile('done DEM write, filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}

   {$IfDef WriteSminSmax}
      BlockWriteDouble(DEMheader.MinElev);  //340
      BlockWriteDouble(DEMheader.MaxElev);  //341
   {$EndIf}

   {$IfDef GeotiffSave} WriteLineToDebugFile('start 33550, filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
   BlockWriteDouble(DEMheader.DEMxSpacing);
   BlockWriteDouble(DEMheader.DEMySpacing);
   BlockWriteDouble(0);

   {$IfDef GeotiffSave} WriteLineToDebugFile('start 33922, filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
   {$IfDef GeotiffCorner} WriteLineToDebugFile('Write DEM,  NW cornerX=' + RealToString(DEMheader.DEMSWCornerX,-18,-6) + '  NW cornerY=' + RealToString(DEMheader.DEMSWCornerY + DEMheader.NumRow * DEMheader.DEMySpacing,-18,-6)); {$EndIf}
   BlockWriteDouble(0);
   BlockWriteDouble(0);
   BlockWriteDouble(0);
   BlockWriteDouble(DEMheader.DEMSWCornerX);
   BlockWriteDouble(DEMheader.DEMSWCornerY + DEMheader.NumRow * DEMheader.DEMySpacing);
   BlockWriteDouble(0);

   {$IfDef GeotiffSave} WriteLineToDebugFile('start 34735 Geotiff keys, filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
   StreamWordFieldEntry(TIFFFile,1,1,0,pred(GeoKeyEntries));
   StreamWordFieldEntry(TIFFFile,1024,0,1,wField);  {1 is for projection coords, 2 for Lat/long}
   StreamWordFieldEntry(TIFFFile,1025,0,1,DEMheader.RasterPixelIsGeoKey1025); {1 RasterPixelIsArea, 2=RasterPixelIsPoint}
   StreamWordFieldEntry(TIFFFile,1026,34737,12,0);

   if (DEMMapProjection <> Nil) and (DEMMapProjection.PName = UK_OS) then StreamWordFieldEntry(TIFFFile,3072,0,1,27700)
   else if (DEMheader.DEMUsed = UTMbasedDEM) then StreamWordFieldEntry(TIFFFile,3072,0,1,GetUTMDatumCode(DEMMapProjection))
   else StreamWordFieldEntry(TIFFFile,2048,0,1,GetGeoDatumCode(DEMMapProjection));

   {$IfDef GeotiffSave} WriteLineToDebugFile('start 34737 ASCII, ' + ASCIIstr + '  filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
   TiffFile.Write(AsciiStr[1],ASCIIsize);

   {$IfDef WriteMissing}
      {$IfDef GeotiffSave} WriteLineToDebugFile('start 42113 ASCII, ' + Missingstr + '  filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
      TiffFile.Write(MissingStr[1],MissingSize);
   {$EndIf}

   {$IfDef GeotiffSave} WriteLineToDebugFile('all done, filepos=' + IntToStr(FilePos(TiffFile))); {$EndIf}
   TiffFile.Free;
   DEMstatus := dsSaved;
   {$IfDef GeotiffSave} WriteLineToDebugFile('tDEMDataSet.SaveAsGeotiff out'); {$EndIf}
end;


function tDEMDataSet.ExportASCII(x1,y1,x2,y2 : integer) : PathStr;
var
   ElevationDecimals,LatLongDecimals,UTMDecimals,
   tf,Col,Row,OutputZFormat : integer;
   z: float32;
   xutm,yutm,Lat,Long,xc,yc,zc : float64;
   ZStr      : shortstring;
   TextOutXYZFile : textfile;
   ASCIIFormat : tASCIIFormat;
   XYZformatform : TXYZformatform;
   Output : tStringList;
   UseStringList : boolean;

   procedure ProcessLine(aLine : shortString);
   begin
      if UseStringList then Output.Add(aline)
      else writeln(TextOutXYZFile,aline);
   end;


begin
   {$IfDef RecordASCIIExport} WriteLineToDebugFile('tDEMDataSet.ExportASCII in'); {$EndIf}
   Result := DEMDefs.WriteDEMDir;

   GetFileNameDefaultExt('extracted XYZ triples','XYZ triples|*.XYZ',Result);
   DEMDefs.WriteDEMDir := ExtractFilePath(Result);
   UseStringList := true;

   XYZformatform := TXYZformatform.Create(Application);
   XYZformatform.RadioGroup1.ItemIndex := MDDef.ASCIIXYFormat;
   XYZformatform.RadioGroup1.ItemIndex := MDDef.ASCIIZUnits;
   if ElevationDEM then begin
      XYZformatform.RadioGroup1.Items[0] := 'Integer default ' + ElevUnitsAre(DEMheader.ElevUnits);
      XYZformatform.ShowModal;
   end;
   MDDef.ASCIIXYFormat := XYZformatform.RadioGroup1.ItemIndex;
   MDDef.ASCIIZUnits := XYZformatform.RadioGroup1.ItemIndex;
   ASCIIFormat := tASCIIFormat(XYZformatform.RadioGroup2.ItemIndex);
   OutputZFormat := XYZformatform.RadioGroup1.ItemIndex;
   CheckEditString(XYZformatform.Edit3.Text,ElevationDecimals);
   CheckEditString(XYZformatform.Edit2.Text,LatLongDecimals);
   CheckEditString(XYZformatform.Edit1.Text,UTMDecimals);
   if (ASCIIFormat = Recenter) then begin
      xc := DEMheader.DEMSWCornerX + 0.5 * DEMheader.NumCol * DEMheader.DEMxSpacing;
      yc := DEMheader.DEMSWCornerY + 0.5 * DEMheader.NumRow * DEMheader.DEMySpacing;
      zc := 0.5 * (DEMheader.MaxElev + DEMheader.MinElev);
   end;

   {$IfDef RecordASCIIExport} WriteLineToDebugFile('ASCII DEM extract to ' + FileName + '  Output z format selected: ' + IntToStr(OutputZFormat) + '   ASCII selected: ' + IntToStr(XYZformatform.RadioGroup2.ItemIndex)); {$EndIf}

   if (ASCIIFormat in [UTMFormat,MGRSFormat,GenerateFormat]) then begin
      if (not CheckForUTMZones) then exit;
      WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',MDDef.DefaultUTMZone,MDDef.DefaultLatHemi,'demcoord 1');
      {$IfDef RecordASCIIExport} WriteLineToDebugFile('UTM zone= ' + IntToStr(MDDef.DefaultUTMZone));   {$EndIf}
   end;

   if UseStringList then begin
      Output := tStringList.Create;
   end
   else begin
      assignFile(TextOutXYZFile,Result);
      rewrite(TextOutXYZFile);
   end;

   if XYZformatform.CheckBox1.Checked then begin
      ZStr := '';
      case ASCIIFormat of
         UTMFormat     : ZStr := 'XUTM,YUTM,ELEVATION';
         LatLongFormat : ZStr := 'LAT,LONG,ELEVATION';
         LongLatFormat : ZStr := 'LONG,LAT,ELEVATION';
      end;
      if (ZStr <> '') then ProcessLine(ZStr);
   end;
   tf := 1;
   CheckEditString(XYZformatform.Edit4.Text,tf);
   XYZformatform.Free;

   {$IfDef RecordASCIIExport} WriteLineToDebugFile('thin factor= ' + IntToStr(tf)); {$EndIf}

   StartProgress('Write ASCII');
   Col := x1;
   while (Col <= x2) do begin
      if (Col mod 100 = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
      Row := y1;
      while (Row <= y2) do begin
         if GetElevMeters(Col,Row,z) then begin
            case OutputZFormat of
               0,
               1 : ZStr := RealToString(z,-7,0);
               2 : ZStr := RealToString(z,-12,ElevationDecimals);
               3 : ZStr := RealToString(10*z,-8,0);
            end;
            DEMGridToLatLongDegree(Col,Row,Lat,Long);
            if ASCIIFormat in [LatLongFormat,LongLatFormat] then begin
               if (ASCIIFormat = LatLongFormat) then ZStr := RealToString(Lat,-15,-LatLongDecimals) + ',' + RealToString(Long,-15,-LatLongDecimals) + ',' + ZStr
               else ZStr := RealToString(Long,-15,-LatLongDecimals) + ',' + RealToString(Lat,-15,LatLongDecimals) + ',' + ZStr;
            end
            else begin
               WGS84DatumConstants.ForwardProjectDegrees(Lat,Long,XUTM,YUTM);
               case ASCIIFormat of
                  UTMFormat  :  ZStr := RealToString(xutm,-15,-UTMDecimals) +  RealToString(yutm,-15,-UTMDecimals) + ',' + ZStr;
                  MGRSFormat :  ZStr:= DEMMapProjection.CalculateMGRS(Xutm,Yutm,10) + ',' + ZStr;
                  GenerateFormat : ZStr := '1,'+ RealToString(xutm,-15,-UTMDecimals) + ',' + RealToString(yutm,-15,-UTMDecimals) + ',' + ZStr;
                  Recenter : ZStr := RealToString(xutm - xc,-12,-UTMDecimals) + ',' + RealToString(yutm - yc,-12,-UTMDecimals) + ',' + RealToString(z - zc,-12,ElevationDecimals);
               end {case};
            end;
            ProcessLine(ZStr);
         end;
         inc(Row,tf);
      end {for};
      inc(Col,tf);
   end;
   if UseStringList then begin
      OutPut.SaveToFile(Result);
      OutPut.Destroy;
   end
   else begin
      if (ASCIIFormat = GenerateFormat) then writeln(TextOutXYZFile,'END');
      closeFile(TextOutXYZFile);
   end;
   EndProgress;
end;


{$EndIf}



