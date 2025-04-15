{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for  demesrishapefile   }
{________________________________________}


procedure tShapeFileCreation.AddPointToShapeStream(Lat,Long : float64);
begin
   if (ShapeStreamCoords <> Nil) and (PtsInShapeStream < sfMaxPoints) then begin
     {$IfDef RecordShapeFileWrite} WriteLineToDebugFile('add point to shape stream ' + IntToStr(PtsInShapeStream) + '   ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
     ShapeStreamCoords^[PtsInShapeStream].Lat := Lat;
     ShapeStreamCoords^[PtsInShapeStream].Long := Long;
     inc(PtsInShapeStream);
   end;
end;

procedure tShapeFileCreation.AddPointWithZToShapeStream(Lat,Long,z : float64);
begin
  AddPointToShapeStream(Lat,Long);
  if (ShapeStreamZs <> Nil) then ShapeStreamZs^[PtsInShapeStream] := z;
end;


procedure tShapeFileCreation.ProcessShapeFileRecord;
var
   PolyLineHeader : sfPolyLineHeader;
   Table : tMyData;
begin
   Table := Nil;
   if DEMMapf.PolygonDigitizing(DEMNowDoing) then begin
      ShapeStreamCoords^[PtsInShapeStream] := ShapeStreamCoords^[0];
      inc(PtsInShapeStream);
   end;
   PolyLineHeader.NumParts := 1;
   PolyLineHeader.NumPoints := PtsInShapeStream;
   {$IfDef TrackShapeDigitization} WritelineToDebugFile('ProcessShapeFileRecord, pts=: ' + IntToStr(PtsInShapeStream)); {$EndIf}
   if (DEMNowDoing = ShapePoint) then begin
      ProcessPointForShapeFile(ShapeStreamCoords^[0].Lat,ShapeStreamCoords^[0].Long);
   end
   else begin
      ProcessRecordForShapeFile;
   end;
   MapScreenX1 := -9999;
   MapScreenY1 := -9999;
end;



constructor tShapeFileCreation.Create(var inFromDatum : tMapProjection; fName : PathStr; MakeDBase : boolean; TheShapeType : int32; inDoSHP : boolean = true; inAppendingShapeFile : tShapeFile = Nil);


      procedure InitializeMainShapeFileHeader(var MainFileHeader : sfMainFileHeader);
      var
         i : int32;
      begin
         MainFileHeader.FileCode := 9994;
         Int4Swap(MainFileHeader.FileCode);
         for i := 1 to 5 do begin
            MainFileHeader.Unused[i] := 0;
            Int4Swap(MainFileHeader.Unused[i]);
         end;
         MainFileHeader.Version := 1000;
         InitializeBoundingBox(MainFileHeader.BoundBox);
         if TheShapeType in [11,13,15,23,25] then begin
            MainFileHeader.BoundBoxZMin := 99999;
            MainFileHeader.BoundBoxZMax := -99999;
         end
         else begin
            MainFileHeader.BoundBoxZMin := 0;
            MainFileHeader.BoundBoxZMax := 0;
         end;
         MainFileHeader.BoundBoxMMin := 0;
         MainFileHeader.BoundBoxMMax := 0;
      end;


begin
   {$IfDef RecordCreateShapeFile} inFromDatum.ShortProjInfo('tShapeFileCreation.Create in'); {$EndIf}
   AppendingShapeFile := inAppendingShapeFile;
   FromDatum := inFromDatum;
   DoSHP := inDoSHP;
   PtsInShapeStream := 0;
   RecsInShapeStream := 0;
   RecordName := '';
   NParts := 1;
   PartSize[1] := 0;
   Table := nil;
   AddProjectionFile(fName);

   New(ShapeStreamCoords);
   ShapeStreamZs := nil;
   if TheShapeType in [11,13,15,23,25] then New(ShapeStreamZs);

   if MakeDBase then begin
      Make_Tables.MakeBasicBoundingBoxTable(fName);
      Table := tMyData.Create(fName);
      {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('Set up new dbf ' + fName); {$EndIf}
   end
   else Table := nil;

   if (AppendingShapeFile = Nil) then begin
        {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('Set up new shapefile ' + fName); {$EndIf}
        fName := ChangeFileExt(fName,'');
        ShapeFileDigitizing := fName + '.shp';

        if DoSHP then begin
           assignFile(glShapeFile,fName + '.shp');
           rewrite(glShapeFile,1);
        end;

        assignFile(glIndexFile,fName + '.shx');
        rewrite(glIndexFile,1);

        InitializeMainShapeFileHeader(glMainFileHeader);
        glMainFileHeader.ShapeType := TheShapeType;
        if DoSHP then BlockWrite(glShapeFile,glMainFileHeader,SizeOf(glMainFileHeader));
        BlockWrite(glIndexFile,glMainFileHeader,SizeOf(glMainFileHeader));
        PtsInShapeStream := 0;
        RecsInShapeStream := 0;
        if (ShapeStreamCoords = Nil) then New(ShapeStreamCoords);
       {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('shapefile set up done'); {$EndIf}
   end
   else begin
      {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('appending shapefile ' + fName); {$EndIf}
      glMainFileHeader := AppendingShapeFile.MainFileHeader;

      if DoSHP then begin
         CloseFile(AppendingShapeFile.ShapeFile);
         assignFile(glShapeFile,ChangeFileExt(AppendingShapeFile.ShapeFileName,'.shp'));
         reset(glShapeFile,1);
      end;

      CloseFile(AppendingShapeFile.IndexFile);
      assignFile(glIndexFile,ChangeFileExt(AppendingShapeFile.ShapeFileName,'.shx'));
      reset(glIndexFile,1);
   end;
end;

destructor tShapeFileCreation.Destroy;
begin
   inherited;
   Dispose(ShapeStreamCoords);
   if glMainFileHeader.ShapeType in [11,13,15,23,25] then Dispose(ShapeStreamZs);
end;


procedure tShapeFileCreation.GetPolyLineHeader(var PolyLineHeader : sfPolyLineHeader; var zMin,zMax : float64);
var
   i : int32;
begin
   InitializeBoundingBox(PolyLineHeader.BoundBox);
   PolyLineHeader.NumPoints := PtsInShapeStream;
   PolyLineHeader.NumParts := NParts;
   PolyLineHeader.RecordNumber := RecsInShapeStream;
   PolyLineHeader.ShapeType := glMainFileHeader.ShapeType;
   for i := 0 to pred(PtsInShapeStream) do begin
      Petmath.CompareValueToExtremes(ShapeStreamCoords^[i].Lat,PolyLineHeader.BoundBox.YMin,PolyLineHeader.BoundBox.YMax);
      Petmath.CompareValueToExtremes(ShapeStreamCoords^[i].Long,PolyLineHeader.BoundBox.XMin,PolyLineHeader.BoundBox.XMax);
      if ShapeFile3D(glMainFileHeader.ShapeType) then begin
         if (i = 0) then begin
            zMin := ShapeStreamZs^[i];
            zMax := ShapeStreamZs^[i];
         end
         else begin
            Petmath.CompareValueToExtremes(ShapeStreamZs^[i],zMin,zMax);
         end;
      end;
   end;
   PolyLineHeader.ContentLength := SizeOf(PolyLineHeader) + (4 * PolyLineHeader.NumParts) + (16 * PolyLineHeader.NumPoints);
   if ShapeFile3D(glMainFileHeader.ShapeType) then begin
      PolyLineHeader.ContentLength := PolyLineHeader.ContentLength + (2 + PolyLineHeader.NumPoints) * 16;
   end;
   PolyLineHeader.ContentLength := PolyLineHeader.ContentLength div 2;
   CheckMainFileBoundingBox(glMainFileHeader,PolyLineHeader.BoundBox.YMin,PolyLineHeader.BoundBox.YMax,PolyLineHeader.BoundBox.XMin,PolyLineHeader.BoundBox.XMax);
end;


procedure tShapeFileCreation.AddLineUTMToShapeFile(x1,y1,x2,y2,delta : float64);
const
   Bitty = 0.000001;
var
   x,y : float64;
   MainFileHeader : sfMainFileHeader;
   Convert : boolean;

      procedure AddPoint(x,y : float64);
      var
         Lat,Long : float64;
      begin
         FromDatum.UTMtoLatLongDegree(X,Y,Lat,Long);
         if Convert then begin
            MolodenskiyTransformation(Lat,Long,Lat,Long,FromDatum,WGS84DatumConstants);
         end;
         CheckMainFileBoundingBox(MainFileHeader,Lat,Long);
         AddPointToShapeStream(Lat,Long);
      end;

begin
   {$If Defined(RecordCreateShapeFileFull)} FromDatum.ShortProjInfo('tShapeFileCreation.AddLineUTMToShapeFile'); {$EndIf}
   Convert := not (WGSEquivalentDatum(FromDatum.h_DatumCode) );
   InitializeBoundingBox(MainFileHeader.BoundBox);
   x := x1;
   y := y1;
   {$IfDef RawProjectInverse} DebugRawProjectInverse := true; {$EndIf}
   AddPoint(x,y);
   {$IfDef RawProjectInverse} DebugRawProjectInverse := false; {$EndIf}
   if abs(y2-y1) < Bitty then begin
      repeat
         x := x + Delta;
         if x > x2 then x := x2;
         AddPoint(x,y);
         x := x + Delta;
      until (x >= x2 - Bitty);
   end
   else begin
      repeat
         y := y + Delta;
         if y > y2 then y := y2;
         AddPoint(x,y);
         y := y + Delta;
      until (y >= y2 - Bitty);
   end;
   ProcessRecordForShapeFile;
end;


procedure tShapeFileCreation.AddLineLatLongToShapeFile;
Label
   Offscale;
const
   Bitty = 0.000001;
var
   MainFileHeader : sfMainFileHeader;
   Lat,Long : float64;

      procedure AddPoint(Lat,Long : float64);
      begin
         if Convert then MolodenskiyTransformation(Lat,Long,Lat,Long,FromDatum,WGS84DatumConstants);
         CheckMainFileBoundingBox(MainFileHeader,Lat,Long);
         AddPointToShapeStream(Lat,Long);
      end;

begin
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('tShapeFileCreation.AddLineLatLongToShapeFile in'); {$EndIf}
   ValueInRange(Lat1,-90,90);
   ValueInRange(Lat2,-90,90);

   Lat := Lat1;
   Long := Long1;
   AddPoint(Lat,Long);
   if abs(lat1-lat2) < Bitty then begin
      if (Lat < -90.00001) or (Lat > 90.000001) then goto OffScale;
      repeat
         Long := Long + Delta;
         if (Long > Long2) then Long := Long2;
         AddPoint(Lat,Long);
         Long := Long + Delta;
      until Long >= Long2 - Bitty;
   end
   else begin
      if (Long < -720.00001) or (Long > 720.000001) then goto OffScale;
      repeat
         Lat := Lat + Delta;
         if (Lat > Lat2) then Lat := Lat2;
         AddPoint(Lat,Long);
         Lat := Lat + Delta;
      until Lat >= Lat2 - Bitty;
   end;
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('points done'); {$EndIf}
   ProcessRecordForShapeFile;
   Offscale:;
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('tShapeFileCreation.AddLineLatLongToShapeFile out'); {$EndIf}
end;



procedure tShapeFileCreation.AddBoxLatLongToShapeFile(Lat1,Long1,Lat2,Long2 : float64);
begin
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('tShapeFileCreation.AddBoxLatLongToShapeFile in'); {$EndIf}
   AddPointToShapeStream(Lat1,Long1);
   AddPointToShapeStream(Lat1,Long2);
   AddPointToShapeStream(Lat2,Long2);
   AddPointToShapeStream(Lat2,Long1);
   AddPointToShapeStream(Lat1,Long1);
   ProcessRecordForShapeFile;
end;


procedure tShapeFileCreation.AddBoundBoxUTMToShapeStream(bb : sfBoundBox);
var
   Lat,Long : float64;

   procedure ACorner(xutm,yutm : float64);
   begin
      FromDatum.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
      AddPointToShapeStream(Lat,Long);
   end;

begin
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('tShapeFileCreation.AddBoundBoxGeoToShapeStream in, ' + sfBoundBoxToString(bb)); {$EndIf}
   ACorner(bb.ymax,bb.xmin);   //NW corner
   ACorner(bb.ymax,bb.xmax);   //NE corner
   ACorner(bb.ymin,bb.xmax);   //SE corner
   ACorner(bb.ymin,bb.xmin);   //SW corner
   ACorner(bb.ymax,bb.xmin);   //NE corner
   ProcessRecordForShapeFile;
end;

procedure tShapeFileCreation.AddBoundBoxGeoToShapeStream(bb : sfBoundBox);
begin
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('tShapeFileCreation.AddBoundBoxUTMToShapeStream in, ' + sfBoundBoxToString(bb)); {$EndIf}
   AddPointToShapeStream(bb.ymax,bb.xmin);   //NW corner
   AddPointToShapeStream(bb.ymax,bb.xmax);   //NE corner
   AddPointToShapeStream(bb.ymin,bb.xmax);   //SE corner
   AddPointToShapeStream(bb.ymin,bb.xmin);   //SW corner
   AddPointToShapeStream(bb.ymax,bb.xmin);   //NE corner
   ProcessRecordForShapeFile;
end;


procedure tShapeFileCreation.ProcessRecordForShapeFile;
var
   off,ConL : int32;
   plHdr : sfPolyLineHeader;
begin
   if PtsInShapeStream > 0  then begin
      inc(RecsInShapeStream);
      GetPolyLineHeader(plHdr,zMin,zMax);
      {$IfDef RecordShapeFileWrite} WriteLineToDebugFile('ProcessRecordForShapeFile rec=' + IntToStr(RecsInShapeStream) + ' Parts=' + IntToStr(plHdr.NumParts) + ' Pts=' + IntToStr(plHdr.NumPoints)); {$EndIf}
      Off := FileSize(glShapeFile) div 2;
      ConL := plHdr.ContentLength;

      BBString := RealToString(plHdr.BoundBox.YMin,-12,-7) + ',' + RealToString(plHdr.BoundBox.YMax,-12,-7) + ',' +  RealToString(plHdr.BoundBox.xMin,-12,-7) + ',' + RealToString(plHdr.BoundBox.xMax,-12,-7);

      {$IfDef RecordShapeFileWrite}
         WriteLineToDebugFile('offset=' + intToStr(Off) + '  size=' + IntToStr(plHdr.ContentLength));
         WriteLineToDebugFile(' SW corner: ' + LatLongDegreeToString(plHdr.BoundBox.YMin,plHdr.BoundBox.XMin) + ' NE corner: ' + LatLongDegreeToString(plHdr.BoundBox.YMax,plHdr.BoundBox.XMax));
      {$EndIf}

      Int4Swap(plHdr.RecordNumber);
      Int4Swap(plHdr.ContentLength);

      Petmath.CompareValueToExtremes(plHdr.BoundBox.YMin,glMainFileHeader.BoundBox.YMin,glMainFileHeader.BoundBox.YMax);
      Petmath.CompareValueToExtremes(plHdr.BoundBox.YMax,glMainFileHeader.BoundBox.YMin,glMainFileHeader.BoundBox.YMax);
      Petmath.CompareValueToExtremes(plHdr.BoundBox.XMin,glMainFileHeader.BoundBox.XMin,glMainFileHeader.BoundBox.XMax);
      Petmath.CompareValueToExtremes(plHdr.BoundBox.XMax,glMainFileHeader.BoundBox.XMin,glMainFileHeader.BoundBox.XMax);

      if DoSHP then begin
         seek(glShapeFile,FileSize(glShapeFile));
         BlockWrite(glShapeFile,plHdr,SizeOf(plHdr));
         BlockWrite(glShapeFile,PartSize,4*plHdr.NumParts);
         BlockWrite(glShapeFile,ShapeStreamCoords^,16*plHdr.NumPoints);

         if ShapeFile3D(glMainFileHeader.ShapeType) then begin
            BlockWrite(glShapeFile,zMin,8);
            BlockWrite(glShapeFile,zMax,8);
            BlockWrite(glShapeFile,ShapeStreamZs^[0],8*plHdr.NumPoints);
         end;
      end;

      Int4Swap(Off);
      Int4Swap(ConL);
      Seek(glIndexFile,FileSize(glIndexFile));
      BlockWrite(glIndexFile,Off,4);
      BlockWrite(glIndexFile,ConL,4);

      if (Table <> Nil) then begin
         Table.Insert;
         PutBoundBoxInTable(Table,plHdr.BoundBox,RecordName);
         Table.Post;
      end;
   end;
   PtsInShapeStream := 0;
   RecordName := '';
end;


procedure tShapeFileCreation.ProcessPointForShapeFile(Lat,Long : float64);
var
   PointsHeader : sfPointsWithHeader;
   off : int32;
begin
   CheckMainFileBoundingBox(glMainFileHeader,Lat,Long);
   with PointsHeader do begin
      RecordNumber := RecsInShapeStream;
      ContentLength := 10;
      {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('Rec=' + IntToStr(RecordNumber) + '  long=' + IntToStr(ContentLength)); {$EndIf}
      Int4Swap(RecordNumber);
      Int4Swap(ContentLength);
      ShapeType := 1;
      y := Lat;
      x := Long;
   end;

   if DoSHP then BlockWrite(glShapeFile,Pointsheader,Sizeof(PointsHeader));
   Off := 50 + pred(RecsInShapeStream) * Sizeof(PointsHeader) div 2;
   Int4Swap(Off);
   BlockWrite(glIndexFile,Off,4);

   off := Sizeof(PointsHeader) div 2;
   Int4Swap(off);
   BlockWrite(glIndexFile,Off,4);
   PtsInShapeStream := 0;
end;


procedure tShapeFileCreation.ProcessPointForShapeFile(Lat,Long,zin : float64);
var
   PointZsHeader : sfPointsZWithHeader;
   off : int32;
begin
   CheckMainFileBoundingBox(glMainFileHeader,Lat,Long,zin);
   with PointZsHeader do begin
      RecordNumber := RecsInShapeStream;
      ContentLength := 14;
      {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('Rec=' + IntToStr(RecordNumber) + '  long=' + IntToStr(ContentLength)); {$EndIf}
      Int4Swap(RecordNumber);
      Int4Swap(ContentLength);
      ShapeType := 11;
      y := Lat;
      x := Long;
      z := zin;
   end;

   if DoSHP then BlockWrite(glShapeFile,PointZsheader,Sizeof(PointZsHeader));
   Off := 50 + pred(RecsInShapeStream) * Sizeof(PointZsHeader) div 2;
   Int4Swap(Off);
   BlockWrite(glIndexFile,Off,4);

   off := Sizeof(PointZsHeader) div 2;
   Int4Swap(off);
   BlockWrite(glIndexFile,Off,4);
   PtsInShapeStream := 0;
end;


procedure tShapeFileCreation.oldProcessRecordForShapeFile;
var
   off,ConL : int32;
   zMin,zMax : float64;

        procedure OldGetPolyLineHeader(var dCoord : tDcoords; TheNumParts,NumPts : int32; Parts : array of int32; var zs : array of float64;
           var PolyLineHeader : sfPolyLineHeader; var zMin,zMax : float64);
        var
           i : int32;
        begin
           with PolyLineHeader do begin
              InitializeBoundingBox(PolyLineHeader.BoundBox);
              PolyLineHeader.NumPoints := NumPts;
              PolyLineHeader.NumParts := TheNumParts;
              PolyLineHeader.RecordNumber := RecsInShapeStream;
              PolyLineHeader.ShapeType := glMainFileHeader.ShapeType;
              for i := 0 to pred(NumPoints) do begin
                 Petmath.CompareValueToExtremes(dCoord[i].Lat,PolyLineHeader.BoundBox.YMin,PolyLineHeader.BoundBox.YMax);
                 Petmath.CompareValueToExtremes(dCoord[i].Long,PolyLineHeader.BoundBox.XMin,PolyLineHeader.BoundBox.XMax);
                 if ShapeFile3D(glMainFileHeader.ShapeType) then begin
                    if (i = 0) then begin
                       zMin := zs[i];
                       zMax := zs[i];
                    end
                    else begin
                       Petmath.CompareValueToExtremes(zs[i],zMin,zMax);
                    end;
                 end;
              end;
              PolyLineHeader.ContentLength := SizeOf(PolyLineHeader) + (4 * NumParts) + (16 * NumPoints);
              if ShapeFile3D(glMainFileHeader.ShapeType) then begin
                 PolyLineHeader.ContentLength := PolyLineHeader.ContentLength + (2 + NumPoints) * 16;
              end;
              PolyLineHeader.ContentLength := PolyLineHeader.ContentLength div 2;
              CheckMainFileBoundingBox(glMainFileHeader,PolyLineHeader.BoundBox.YMin,PolyLineHeader.BoundBox.YMax,PolyLineHeader.BoundBox.XMin,PolyLineHeader.BoundBox.XMax);
           end;
        end;


begin
   inc(RecsInShapeStream);
   {$IfDef RecordShapeFileWrite} WriteLineToDebugFile('oldProcessRecordForShapeFile rec=' + IntToStr(RecsInShapeStream) + ' Parts=' + IntToStr(NumParts) + '  Pts=' + IntToStr(NumPts)); {$EndIf}
   OldGetPolyLineHeader(dCoord,NumParts,NumPts,Parts,zs,LastRecPolyLineHeader,zmin,zmax);
   with LastRecPolyLineHeader do begin
      Off := FileSize(glShapeFile) div 2;
      ConL := LastRecPolyLineHeader.ContentLength;
      {$IfDef RecordShapeFileWrite} WriteLineToDebugFile('offset=' + intToStr(Off) + ' size=' + IntToStr(LastRecPolyLineHeader.ContentLength)); {$EndIf}

      Int4Swap(LastRecPolyLineHeader.RecordNumber);
      Int4Swap(LastRecPolyLineHeader.ContentLength);

      if DoSHP then begin
         seek(glShapeFile,FileSize(glShapeFile));
         BlockWrite(glShapeFile,LastRecPolyLineHeader,SizeOf(LastRecPolyLineHeader));
         BlockWrite(glShapeFile,Parts,4*LastRecPolyLineHeader.NumParts);
         BlockWrite(glShapeFile,dCoord,16*LastRecPolyLineHeader.NumPoints);

         if ShapeFile3D(glMainFileHeader.ShapeType) then begin
            BlockWrite(glShapeFile,zMin,16);
            BlockWrite(glShapeFile,zMax,16);
            BlockWrite(glShapeFile,zs[0],16*LastRecPolyLineHeader.NumPoints);
         end;
      end;

      Int4Swap(Off);
      Int4Swap(ConL);
      Seek(glIndexFile,FileSize(glIndexFile));
      BlockWrite(glIndexFile,Off,4);
      BlockWrite(glIndexFile,ConL,4);
   end;
   PtsInShapeStream := 0;
end;




function tShapeFileCreation.CloseShapeFiles : integer;
begin
   with glMainFileHeader do begin
      {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('Close new shape file, records: ' + IntToStr(RecsInShapeStream) + '  SW corner: ' + LatLongDegreeToString(BoundBox.YMin,BoundBox.XMin) + '   NE corner: ' + LatLongDegreeToString(BoundBox.YMax,BoundBox.XMax)); {$EndIf}

      FileLength := RecsInShapeStream * 4 + 50;
      Int4Swap(FileLength);
      Seek(glIndexFile,0);
      BlockWrite(glIndexFile,glMainFileHeader,SizeOf(glMainFileHeader));
      CloseFile(glIndexFile);

      if DoSHP then begin
         FileLength := FileSize(glShapeFile) div 2;
         Int4Swap(FileLength);
         seek(glShapeFile,0);
         BlockWrite(glShapeFile,glMainFileHeader,SizeOf(glMainFileHeader));
         CloseFile(glShapeFile);
      end;
   end;

   if (ShapeStreamCoords <> Nil) then Dispose(ShapeStreamCoords);
   ShapeStreamCoords := Nil;
   PtsInShapeStream := 0;
   {$IfDef NoDBMaps}
   {$Else}
      if (MapForShapeDigitizing <> Nil) then begin
          ShapeFileDigitizing := ChangeFileExt(ShapeFileDigitizing,DefaultDBExt);
          Result := MapForShapeDigitizing.OpenDBonMap('',ShapeFileDigitizing);
      end;
      MapForShapeDigitizing := Nil;
   {$EndIf}

   if (AppendingShapeFile <> Nil) then begin
      if DoSHP then begin
         assignFile(AppendingShapeFile.ShapeFile,ChangeFileExt(AppendingShapeFile.ShapeFileName,'.shp'));
         reset(AppendingShapeFile.ShapeFile,1);
      end;
      assignFile(AppendingShapeFile.IndexFile,ChangeFileExt(AppendingShapeFile.ShapeFileName,'.shx'));
      reset(AppendingShapeFile.IndexFile,1);
      AppendingShapeFile.MainFileHeader := glMainFileHeader;
   end;
   Destroy;
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('CloseShapeFiles:'); {$EndIf}
end;

