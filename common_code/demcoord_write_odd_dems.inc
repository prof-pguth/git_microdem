{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{include file for demcoord.pas}


procedure tDEMDataSet.SaveAsArcGridASCII(SaveName : PathStr = '');
var
   fFile : TextFile;
   x,y    : integer;
   z : float32;
begin
   if (SaveName = '') then begin
      SaveName := DEMDefs.WriteDEMDir;
      if not GetFileNameDefaultExt('new ASCII DEM','DEM|*.ASC',SaveName) then exit;
   end;

   assignFile(fFile,SaveName);
   rewrite(fFile);
   writeln(fFile,'ncols         ' + IntToStr(DEMheader.NumCol));
   writeln(fFile,'nrows         ' + IntToStr(DEMheader.NumRow));
   writeln(fFile,'xllcorner     ' + RealToString(DEMheader.DEMSWCornerX,-24,8));
   writeln(fFile,'yllcorner     ' + RealToString(DEMheader.DEMSWCornerY,-24,8));
   writeln(fFile,'cellsize      ' + RealToString(DEMheader.DEMxSpacing,-24,-14));
   writeln(fFile,'nodata_value  -9999');

   StartProgress('Write ' + ExtractFileName(SaveName));
   with DEMheader do begin
      for y := pred(NumRow) downto 0 do begin
         if (y mod 25 = 0) then UpdateProgressBar((NumRow-y)/NumRow);
         for x := 0 to pred(NumCol) do begin
            if GetElevMeters(x,y,z) then write(ffile,' ' + RealToString(z,-18,-2))
            else write(ffile,' -9999');
         end;
      end;
   end;
   CloseFile(fFile);
   EndProgress;
end;


procedure tDEMDataSet.SaveAsUSGSASCII(SaveName : PathStr = '');
type
   Rec1024Type = array[1..1024] of AnsiChar;
   string24    = string[24];
var
   Rec1024      : Rec1024Type;
   OutFile      : file of Rec1024Type;
   ColMinElev,
   ColMaxElev,
   FirstZ,LastZ,
   OnByte,zi,
   Col,i,j      : integer;
   z : float32;
   Lat,Long,xutm,yutm    : float64;
   MenuStr,TStr : ShortString;


         function Dformat(x : float64) : string24;
         var
            i : integer;
         begin
            Result := FloatToStrF(x,ffExponent,13,2);
            for i := 1 to length(Result) do if Result[i] = 'E' then Result[i] := 'D';
            while length(Result) < 24 do Result := ' ' + Result;
         end;

         function Eformat(x : integer) : ShortString;
         begin
            Result := '0.000000E+01';
            if (x < 10) then begin
               TStr := IntToStr(x);
               Result[3] := TStr[1];
            end
            else if (x < 100) then begin
               TStr := IntToStr(x);
               Result[3] := TStr[1];
               Result[4] := TStr[2];
               Result[12] := '2';
            end
            else if (x < 1000) then begin
               TStr := IntToStr(x);
               Result[3] := TStr[1];
               Result[4] := TStr[2];
               Result[5] := TStr[3];
               Result[12] := '3';
            end;
         end;

begin
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('Start USGS ASCII save'); {$EndIf}
   if (SaveName = '') then begin
      SaveName := DEMDefs.WriteDEMDir;
      GetFileNameDefaultExt('USGS ASCII format DEM','DEM|*.dem',SaveName);
      DEMDefs.WriteDEMDir := ExtractFilePath(SaveName);
   end;

   AssignFile(OutFile,SaveName);
   rewrite(outFile);
   with DEMheader do begin
      DefineDEMVariables(false);
      FillChar(Rec1024,1024,' ');
      MenuStr := AreaName + ' converted with PETMAR Trilobites MicroDEM';
      Move(MenuStr[1],Rec1024[1],length(MenuStr));
      Rec1024[150] := '1';   //DEM level
      Rec1024[156] := '1';   {regular elevation pattern}
      case DEMUsed of
         UTMBasedDEM  :  Rec1024[162] := '1';
         ArcSecDEM :  Rec1024[162] := '0';
      end;
      TStr := IntegerToString(UTMZone,2);
      Rec1024[167] := TStr[1];
      Rec1024[168] := TStr[2];
      for i := 1 to 15 do begin
         TStr := DFormat(0.0);
         Move(TStr[1],Rec1024[169 + pred(i)*24],24);
      end;

      case DataSpacing of
         SpaceMeters  : Rec1024[534] := '2';
         oldSpaceSeconds : Rec1024[534] := '3';
      end;
      case ElevUnits of
         Decimeters,
         Centimeters,
         euMeters : Rec1024[540] := '2';
         Feet   : Rec1024[540] := '1';
      end;
      Rec1024[546] := '4'; {sides of polygon}

      if (DEMUsed = ArcSecDEM) then begin
         Lat := 3600 * DEMSWcornerLat;
         Long := 3600 * DEMSWcornerLong;
         MenuStr := DFormat(Long) + DFormat(Lat) +
                DFormat(Long+round(3600*LongSizeMap)) + DFormat(Lat) +
                DFormat(Long+round(3600*LongSizeMap)) + DFormat(Lat+round(3600*LatSizeMap)) +
                DFormat(Long) + DFormat(Lat+round(3600*LatSizeMap));
         Move(MenuStr[1],Rec1024[547],length(MenuStr));
         {$IfDef RecordWriteDEM}
             WriteLineToDebugFile(LatLongDegreeToString(BaseLat,BaseLong,DecMinutes) + RealToString(LatSizemap,8,2) + RealToString(LongSizeMap,8,2));
             WriteLineToDebugFile(MenuStr);
         {$EndIf}
      end
      else begin
         DEMGridToUTM(0,0,xutm,yutm);
         MenuStr := DFormat(xutm) + DFormat(Yutm);
         DEMGridToUTM(0,pred(NumRow),xutm,yutm);
         MenuStr := MenuStr + DFormat(xutm) + DFormat(Yutm);
         DEMGridToUTM(pred(NumCol),pred(NumRow),xutm,yutm);
         MenuStr := MenuStr + DFormat(xutm) + DFormat(Yutm);
         DEMGridToUTM(pred(NumCol),0,xutm,yutm);
         MenuStr := MenuStr + DFormat(xutm) + DFormat(Yutm);
         Move(MenuStr[1],Rec1024[547],length(MenuStr));
      end;
      MenuStr := DFormat(MinElev) + DFormat(MaxElev) + DFormat(0.0);
      Move(MenuStr[1],Rec1024[739],length(MenuStr));
      Rec1024[816] := '0';
      if DEMheader.DEMUsed = UTMBasedDEM then
         MenuStr := EFormat(round(DEMxSpacing)) + EFormat(round(DEMySpacing)) + Eformat(1)
      else MenuStr := EFormat(round(3600 * DEMxSpacing)) + EFormat(round(3600 * DEMySpacing)) + Eformat(1);
      Move(MenuStr[1],Rec1024[817],length(MenuStr));

//   USGS specs state that NumRow is normally set to 1; some software requires this
      MenuStr := '     1' + IntegerToString(NumCol,6);

      Move(MenuStr[1],Rec1024[853],length(MenuStr));
      case DigitizeDatum of
         WGS72d : Rec1024[892] := '2';
         WGS84d : Rec1024[892] := '3';
         NAD27d : Rec1024[892] := '1';
         NAD83d : Rec1024[892] := '4';
         else Rec1024[892] := '3';
      end;
      write(OutFile,Rec1024);

      {$IfDef RecordWriteDEM} WriteLineToDebugFile('Write file header: ' + Rec1024); {$EndIf}
      StartProgress('Export');
      for Col := 1 to NumCol do begin
         UpdateprogressBar(Col/NumCol);
         FillChar(Rec1024,1024,' ');
         MenuStr := IntegerToString(1,6) + IntegerToString(Col,6);
         Move(MenuStr[1],Rec1024[1],length(MenuStr));

         FirstZ := 0;
         while MissingDataInGrid(Col,Firstz) and (FirstZ < NumRow) do inc(FirstZ);
         LastZ := pred(NumRow);
         while MissingDataInGrid(Col,Lastz) and (LastZ > 0) do dec(LastZ);
         if not (LastZ >= FirstZ) then begin
            FirstZ := 0;
            LastZ := 0;
         end;

         if (LastZ >= FirstZ) then begin
            if (DEMUsed = UTMBasedDEM) then begin
               DEMGridtoUTM(pred(Col),FirstZ,XUTM,YUTM);
               MenuStr := dFormat(xutm) + dFormat(yutm);
            end
            else begin
               DEMGridtoLatLongDegree(pred(Col),FirstZ,Lat,Long);
               MenuStr := dFormat(Long*3600) + dFormat(Lat*3600);
            end;

            ColMaxElev := -MaxSmallInt;
            ColMinElev := MaxSmallInt;
            for j := FirstZ to LastZ do begin
               if GetElevMeters(pred(Col),j,z) then begin
                  if (z > ColMaxElev) then ColMaxElev := round(z);
                  if (z < ColMinElev) then ColMinElev := round(z);
               end;
            end;

            MenuStr := IntegerToString(succ(LastZ-FirstZ),6) + IntegerToString(1,6) + MenuStr + DFormat(0.0) + DFormat(ColMinElev) + DFormat(ColMaxElev);
            Move(MenuStr[1],Rec1024[13],length(MenuStr));

            OnByte := 145;
            for j := FirstZ to LastZ do begin
               if GetElevMeters(pred(Col),j,z) then zi := round(z)
               else zi := MDDef.ASCIIMissingValue;
               TStr := IntegerToString(zi,6);
               Move(TStr[1],Rec1024[OnByte],6);
               inc(OnByte,6);
               if (Onbyte > 1020) then begin
                  write(OutFile,Rec1024);
                  FillChar(Rec1024,1024,' ');
                  OnByte := 1;
               end;
            end;
            if (Onbyte > 1) then write(OutFile,Rec1024);
         end;
      end;

     {$IfDef RecordWriteDEM} WriteLineToDebugFile('Start Logical Record C'); {$EndIf}

   {create and write blank Logical Record C}
      FillChar(Rec1024,1024,' ');
      MenuStr := '';
      for i := 1 to 10 do MenuStr := MenuStr + IntegerToString(0,6);
      Move(MenuStr[1],Rec1024[1],length(MenuStr));
      write(OutFile,Rec1024);
      closeFile(OutFile);
      EndProgress;
   end;
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('End USGS ASCII save'); {$EndIf}
end;


procedure tDEMDataSet.Save16BitBSQ;
var
   SaveName : PathStr;
   x,y,MissVal : integer;
   outf : file of SmallInt;
begin
   MissVal := MaxSmallInt;
   with DEMheader do begin
      SaveName := DEMDefs.WriteDEMDir;
      GetFileNameDefaultExt('16 bit BSQ format DEM','DEM|*.dem',SaveName);
      DEMDefs.WriteDEMDir := ExtractFilePath(SaveName);
      ReadDefault('Missing value',MissVal);
      AssignFile(outf,SaveName);
      rewrite(outf);
      StartProgress('Write ' + ExtractFileName(SaveName));
      for y := pred(NumRow) downto 0 do begin
         if (y mod 1000 = 0) then UpdateProgressBar( (NumRow-y) / NumRow);
         for x := 0 to pred(NumCol) do begin
            if MissingDataInGrid(x,y) then SmallIntElevations[x]^[y] := MissVal;
            write(Outf,SmallIntElevations[x]^[y]);
         end;
      end;
      CloseFile(OutF);
      EndProgress;
   end;
end;

procedure tDEMDataSet.SavePGM(Bit16 : boolean);
var
   SaveName : PathStr;
   x,y : integer;
   Results : tStringList;
   aLine : shortString;
   z : float32;
   TStr : ShortString;
begin
   with DEMheader do begin
      SaveName := System.IOUtils.TPath.Combine(DEMDefs.WriteDEMDir, AreaName + '.PGM');
      if Bit16 then TStr := '16 bit' else TStr := '8 bit';
      GetFileNameDefaultExt(TStr + ' PGM format DEM','PGM|*.PGM',SaveName);
      Results := tStringList.Create;
      Results.Add('P2');
      Results.Add('# Generated by MICRODEM');
      Results.Add('# Brought to you by PETMAR TRILOBITE BREEDING RANCH');
      Results.Add(IntToStr(NumCol) + ' ' + IntToStr(NumRow));
      if Bit16 then Results.Add('65535')
      else Results.Add('255');

      StartProgress('Write ' + ExtractFileName(SaveName));
      aLine := '';
      for y := pred(NumRow) downto 0 do begin
         if (y mod 1000 = 0) then UpdateProgressBar( (NumRow-y) / NumRow);
         for x := 0 to pred(NumCol) do begin
            if GetElevMeters(x,y,z) then begin
               if Bit16 then aline := aline + ' ' + IntToStr(round(65535 * (z-MinElev)/(MaxElev - MinElev)))
               else aline := aline + ' ' + IntToStr(round(255 * (z-MinElev)/(MaxElev - MinElev)));
            end
            else aLine := aLine + ' 0';
            if (Length(Aline) > 62) then begin
               Results.Add(aline);
               Aline := '';
            end;
         end;
      end;
      if (Length(Aline) > 0) then Results.Add(aline);
      Results.SaveToFile(SaveName);
      EndProgress;
   end;
end;

{$EndIf}


{$IfDef IncludeBILWrite}

procedure WritePRJfile(fName : PathStr; DEMUsed : byte);
var
   Header : tStringList;
begin
   Header := tStringList.Create;
   if not (DEMUsed = ArcSecDEM) then MessageToContinue('Problems; this only works with geographic DEMs');
   Header.Add('Projection    GEOGRAPHIC');
   if MDdef.PreferPrimaryDatum = 'NAR-C' then Header.Add('Datum         NAD83')
   else if MDdef.PreferPrimaryDatum = 'WGS84' then Header.Add('Datum         WGS84');
   Header.Add('Spheroid      GRS80');
   Header.Add('Units         DD');
   Header.Add('Zunits        METERS');
   Header.Add('Parameters');
   Header.SaveToFile(fName);
   Header.Free;
end;


procedure tDEMDataSet.WriteGridFloatFormatDEM(var FileName : PathStr);
type
   tFloatRow = array[0..MaxElevArraySize] of float32;
var
   fFile  : file;
   x,y    : integer;
   Dir    : DirStr;
   Name   : NameStr;
   Ext    : ExtStr;
   z : float32;
   Header : tStringList;
   fRow   : ^tFloatRow;
begin
   if (FileName = '') or (not FileExtEquals(FileName, '.FLT')) then begin
      if (FileName = '') or (not PathIsValid(FileName)) then FileName := DEMDefs.WriteDEMDir;
      if not GetFileNameDefaultExt('new float64 DEM','DEM|*.FLT',FileName) then exit;
   end;
   FSplit(FileName,Dir,Name,Ext);
   Header := tStringList.Create;

   Header.Add('ncols         ' + IntToStr(DEMheader.NumCol));
   Header.Add('nrows         ' + IntToStr(DEMheader.NumRow));
   Header.Add('xllcorner     ' + RealToString(DEMheader.DEMSWCornerX,24,14));
   Header.Add('yllcorner     ' + RealToString(DEMheader.DEMSWCornerY,24,14));
   Header.Add('cellsize      ' + RealToString(DEMheader.DEMxSpacing,24,14));
   Header.Add('NODATA_value  -9999');
   Header.Add('byteorder     LSBFIRST');
   Header.SaveToFile(Dir + Name + '.HDR');
   Header.Clear;

   WritePRJfile(Dir + Name + '.PRJ',DEMheader.DEMUsed);

   New(fRow);
   AssignFile(fFile,Dir + Name + '.FLT');
   rewrite(fFile,4*DEMheader.NumCol);
   with DEMheader do begin
      for y := pred(NumRow) downto 0 do begin
         for x := 0 to pred(NumCol) do begin
            if not GetElevMeters(x,y,z) then z := -9999;
            fRow^[x] := z;
         end;
         BlockWrite(fFile,fRow^,1);
      end;
   end;
   CloseFile(fFile);
   Dispose(fRow);
end;


procedure tDEMDataSet.WriteBILFormatDEM(var FileName : PathStr);
var
   fFile : file;
   x,y    : integer;
   Mean,Std,AveDev,z : float32;
   Dir      : DirStr;
   Name     : NameStr;
   Ext      : ExtStr;
   Header : tStringList;
   Row16Bit : tElevColPointer;
begin
   {$IfDef RecordWriteDEM}   WriteLineToDebugFile('tDEMDataSet.WriteBILFormatDEM in'); {$EndIf}
   if (FileName = '') or  (not FileExtEquals(FileName, '.BIL')) then begin
      if (FileName = '') or (not PathIsValid(FileName)) then FileName := DEMDefs.WriteDEMDir;
      if not GetFileNameDefaultExt('new BIL DEM','BIL DEM|*.BIL',FileName) then exit;
   end;
   FSplit(FileName,Dir,Name,Ext);
   Header := tStringList.Create;
   Header.Add('BYTEORDER      I');
   Header.Add('LAYOUT       BIL');
   Header.Add('NROWS         ' + IntToStr(DEMheader.NumRow));
   Header.Add('NCOLS         ' + IntToStr(DEMheader.NumCol));
   Header.Add('NBANDS        1');
   x := 2;
   Header.Add('NBITS         16');
   Header.Add('BANDROWBYTES         ' + IntToStr(16*DEMheader.NumCol));
   Header.Add('TOTALROWBYTES        ' + IntToStr(16*DEMheader.NumCol));
   Header.Add('BANDGAPBYTES         0');
   Header.SaveToFile(Dir + Name + '.HDR');
   Header.Clear;
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('hdr writtein'); {$EndIf}

   WritePRJfile(Dir + Name + '.PRJ',DEMheader.DEMUsed);

   Header.Add(RealToString(DEMheader.DEMxSpacing,20,14));
   Header.Add(RealToString(0,20,14));
   Header.Add(RealToString(0,20,14));
   Header.Add(RealToString(-DEMheader.DEMySpacing,20,14));
   Header.Add(RealToString(DEMheader.DEMSWCornerX,20,14));
   Header.Add(RealToString((DEMheader.DEMSWCornerY+pred(DEMheader.NumRow)*DEMheader.DEMySpacing),20,14));
   Header.SaveToFile(Dir + Name + '.BLW');
   Header.Clear;
   {$IfDef RecordWriteDEM}   WriteLineToDebugFile('blw written'); {$EndIf}
   ElevationStatistics(Mean,Std,AveDev,true);
   Header.Add('1 ' + RealToString(ZinMeters(DEMheader.MinElev),-12,0) + ' ' + RealToString(ZinMeters(DEMheader.MaxElev),-12,0) + ' ' +  RealToString(Mean,-12,1) + ' ' + RealToString(Std,-12,1)  );
   Header.SaveToFile(Dir + Name + '.STX');
   Header.Free;
   {$IfDef RecordWriteDEM}   WriteLineToDebugFile('stx written'); {$EndIf}

   AssignFile(fFile,Dir + Name + '.BIL');

   rewrite(fFile,2*DEMheader.NumCol);
   New(Row16Bit);
   with DEMheader do begin
      for y := pred(NumRow) downto 0 do begin
         for x := 0 to pred(NumCol) do begin
            if not GetElevMeters(x,y,z) then z := -9999;
            Row16Bit^[x] := round(z);
         end;
         BlockWrite(fFile,row16Bit^,1);
      end;
   end;
   Dispose(Row16Bit);
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('16 bit bil written'); {$EndIf}
   CloseFile(fFile);
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('tDEMDataSet.WriteBILFormatDEM out'); {$EndIf}
end;
{$EndIf}


function tDEMDataSet.VRMLFile(GridPts : integer; xu1,yu1,GridSize : float64; TextureName : shortString; WalkAboutFile : tStringList = Nil; NumTextures : integer = 1; TextureInVRML : boolean = true) : tStringList;
var
   i,x,y : integer;
   zmin,zmax,
   xu,yu,z : float32;
   LongStr : AnsiString;
begin
     VRMLfile := tStringList.Create;
     VRMLfile.Add('#VRML V2.0 utf8');
     VRMLfile.Add('#- BRUGIERE & CLAD & GUTH');
     VRMLfile.Add('#- Exploitation .xyz');
     {$IfDef VCL}  VRMLfile.Add('#- MD Vers ' + BuildString); {$EndIf}
     {$IfDef FMX}  VRMLfile.Add('#- MICRODEM'); {$EndIf}
     VRMLfile.Add('');

     VRMLfile.Add('WorldInfo {');
     VRMLfile.Add('   title " MICRODEM world "');
     VRMLfile.Add('   info  " From PETMAR Trilobites "');
     VRMLfile.Add('}');

     VRMLfile.Add('NavigationInfo');
     VRMLfile.Add('{');
     VRMLfile.Add('	type "EXAMINE"');
     VRMLfile.Add('}');

     VRMLfile.Add('#lumiere');
     VRMLfile.Add('PointLight {');
     VRMLfile.Add('on TRUE');

     VRMLfile.Add('location 2996.0 3500 3032.0');
     VRMLfile.Add('radius 10000');
     VRMLfile.Add('intensity 1.0');
     VRMLfile.Add('ambientIntensity 1.0');
     VRMLfile.Add('color 1.0, 1.0, 1.0');
     VRMLfile.Add('attenuation 1.0 0.0 1.0');
     VRMLfile.Add('}');

     VRMLfile.Add('#point de vue');
     VRMLfile.Add('Viewpoint {');
     VRMLfile.Add('description " Main view "');
     VRMLfile.Add('position 1998.0  1454.6001 8064.0');
     VRMLfile.Add('fieldOfView 0.80');
     VRMLfile.Add('}');

     VRMLfile.Add('#affichage des points');
     VRMLfile.Add('Transform');
     VRMLfile.Add('{');
     VRMLfile.Add('children [');
     VRMLfile.Add('Shape');
     VRMLfile.Add('{');

     VRMLfile.Add('appearance Appearance');
     VRMLfile.Add('{');
     VRMLfile.Add('	material  Material');
     VRMLfile.Add('	{');
     VRMLfile.Add('	ambientIntensity 0.9');
     VRMLfile.Add('	diffuseColor  1 1 1');
     VRMLfile.Add('	}');
     if TextureInVRML then begin
        VRMLfile.Add('	texture ImageTexture');
        VRMLfile.Add('	{');
        VRMLfile.Add('	url  "' + TextureName + '"');
        VRMLfile.Add('	}');
     end;
     VRMLfile.Add('}');

     VRMLfile.Add('geometry ElevationGrid');
     VRMLfile.Add('{');
     VRMLfile.Add('colorPerVertex TRUE');
     VRMLfile.Add('creaseAngle 20');
     VRMLfile.Add('	xDimension ' + IntToStr(GridPts));
     VRMLfile.Add('	xSpacing   ' + RealToString(GridSize,-8,2));
     VRMLfile.Add('	zDimension ' + IntToStr(GridPts));
     VRMLfile.Add('	zSpacing   ' + RealToString(GridSize,-8,2));
     VRMLfile.Add('	solid FALSE');
     VRMLfile.Add('	height [');

      zmin := 9999;
      zmax := -9999;
      for y := pred(GridPts) downto 0 do begin
         LongStr := '          ';
         yu := yu1 - y * GridSize;
         for x := pred(GridPts) downto 0 do begin
            xu := xu1 + x * GridSize;
            GetElevFromUTM(xu,yu,z);
            if z < zmin then zmin := z;
            if z > zmax then zmax := z;
            LongStr := LongStr + RealToString(z,7,1);
         end;
         VRMLfile.Add(LongStr);
      end;

     VRMLfile.Add('				]');
     VRMLfile.Add('}');
     VRMLfile.Add('}');
     VRMLfile.Add(']');
     VRMLfile.Add('}');

     if (WalkAboutFile <> Nil) then begin
        WalkAboutFile.Add('# scene file for walkabout');
        WalkAboutFile.Add('');
        WalkAboutFile.Add('#eastern then northern then elevation - all low to high');
        WalkAboutFile.Add('COORDINATES (' + RealToString(xu1,-12,0) + ' ' +
           RealToString(xu1 + GridPts * GridSize,-12,0) + ' ' + RealToString(yu1,-12,0) + ' ' +
           RealToString(yu1 + GridPts * GridSize,-12,0) + ' ' + RealToString(zmin,-12,0) + ' ' + RealToString(zmax,-12,0) + ')');
        WalkAboutFile.Add('');
        WalkAboutFile.Add('#fast move then slow move then turn then sidestep');
        WalkAboutFile.Add('SPEEDS (25.0 5.0 2.5 3.0)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#how tall the user is');
        WalkAboutFile.Add('USER_HEIGHT (6)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#what is the field of view of the camera');
        WalkAboutFile.Add('CAMERA_FOV (90)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#what is the distance between the user''s eyes');
        WalkAboutFile.Add('INTER_PUPIL (0.200000)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#show the text overlays be shown');
        WalkAboutFile.Add('SHOW_TEXT');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#networking');
        WalkAboutFile.Add('REALTIME_POSITIONING_TIME_INTERVAL (0.1)');
        WalkAboutFile.Add('REALTIME_POSITIONING_TIMEOUT (30)');
        WalkAboutFile.Add('REALTIME_POSITIONING_PORT (6000)');
        WalkAboutFile.Add('REALTIME_POSITIONING_CLIENT (10.0.1.3)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#imagery');
        for i := 1 to NumTextures do WalkAboutFile.Add('TEXTURE_MAP (0,DRAPE_MAP' + IntToStr(i) + '.JPG)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#landscapes');
        WalkAboutFile.Add('OBJECT (file(test.wrl) orientation(0 0 180)');

        WalkAboutFile.Add('');
        WalkAboutFile.Add('#colour of the background');
        WalkAboutFile.Add('SKY (0.3 0.6 0.8)');
     end;
end;


{$IfDef ExDTED}
{$Else}
   procedure tDEMDataSet.SaveAsDTED(OutLatInterval,OutLongInterval : integer; OutName : PathStr = ''; ShowProgress : boolean = false);
   const
      MaxRows = MaxElevArraySize*3+12;
   type
      televs  = array[1..MaxRows] of byte;
   var
      outf : file;
      elevs       : ^televs;
      UHL         : ^tuhl;
      DSI         : ^tDSI;
      ACC         : ^tACC;
      TStr        : shortstring;
      CheckSum    : longInt;
      Lats,Longs  : tFourFloats;
      ch,LongHemi : AnsiChar;
      df : integer;
      Sec,nfLatInterval,nfLongInterval,
      Lat,Long,Xgrid,Ygrid,z,
      CornerLat,CornerLong : float64;
      OutNumCol,OutNumRow,
      Rec,i,j,te,
      bLat,bLong,Missing,
      Deg,Min : integer;
      Dted_save_form : TDted_save_form;
   begin
      {$IfDef RecordWriteDEM} WriteLineToDebugFile('tDEMDataSet.SaveAsDTED in'); {$EndIf}
      try
         GetDEMLimits(bLat,bLong,Lats,Longs);

         {$IfDef RecordWriteDEM} WriteLineToDebugFile('prelims over'); {$EndIf}

         if (OutLatInterval = 0) and (OutLongInterval = 0) then begin
            Dted_save_form := TDted_save_form.Create(Application);
            Dted_save_form.Edit3.Text := IntToStr(bLat);
            Dted_save_form.Edit4.Text := IntToStr(bLong);
            Dted_save_form.Edit5.Text := IntToStr(MDDef.DTEDMissingValue);
            nfLatInterval := 3;
            nfLongInterval := nfLatInterval;
            Dted_save_form.Edit1.Text := RealToString(nfLatInterval,-8,-2);
            Dted_save_form.Edit2.Text := RealToString(nfLongInterval,-8,-2);

            {$IfDef RecordWriteDEM} WriteLineToDebugFile('off to Dted_save_form showmodal'); {$EndIf}
            Dted_save_form.ShowModal;
            {$IfDef RecordWriteDEM} WriteLineToDebugFile('back from Dted_save_form showmodal'); {$EndIf}

            CheckEditString(Dted_save_form.Edit1.Text,nfLatInterval);
            CheckEditString(Dted_save_form.Edit2.Text,nfLongInterval);
            CheckEditString(Dted_save_form.Edit5.Text,MDDef.DTEDMissingValue);
            if Dted_save_form.FullCellCheckBox.Checked then begin
               CheckEditString(Dted_save_form.Edit3.Text,Lats[1]);
               CheckEditString(Dted_save_form.Edit4.Text,Longs[1]);
               Lats[4] := Lats[1] + 1;
               Longs[4] := Longs[1] + 1;
            end;
            Dted_save_form.Close;
            OutLatInterval := round(10 * nfLatInterval);
            OutLongInterval := round(10 * nfLongInterval);
         end;

         OutNumCol := succ(round((3600.0 * (Longs[4] - Longs[1]) / (0.1 * OutLongInterval))));
         OutNumRow := succ(round((3600.0 * (Lats[4] - Lats[1]) / (0.1 * OutLatInterval))));

         {$IfDef RecordWriteDEM}
            WriteLineToDebugFile('Long range, deg: ' + RealToString((Longs[4] - Longs[1]),-18,-6) +'  Long range, sec: ' + RealToString(3600.0*(Longs[4] - Longs[1]),-18,-6) + ' Output, ' + IntToStr(OutNumCol) + ' x ' + IntToStr(OutNumRow));
         {$EndIf}

         if (OutNumRow > MaxRows) then begin
            Destroy;
            MessageToContinue('Too many rows requested');
            Dted_save_form.Close;
            exit;
         end;

         new(elevs);
         new(uhl);
         new(dsi);
         new(acc);

         FillChar(UHL^,80,#32);
         FillChar(DSI^,648,#32);
         FillChar(ACC^,2700,#32);
         TStr := 'UHL1';
         Move(TStr[1],UHL^[1],4);

         CornerLat := round(Lats[1]*3600) / 3600;
         CornerLong := round(Longs[1]*3600) / 3600;
         DegreesToDegMinSec(CornerLong,Deg,Min,Sec);
         if (CornerLong >= 0) then LongHemi := 'E' else LongHemi := 'W';
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,2,0) + LongHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],UHL^[5],8);

         DegreesToDegMinSec(CornerLat,Deg,Min,Sec);
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,2,0) + DEMMapProjection.LatHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],UHL^[13],8);

         TStr := IntegerToString(OutLongInterval,4) + IntegerToString(OutLatInterval,4);
         for i := 1 to 8 do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],UHL^[21],8);

         Tstr := FormatString(Dted_save_form.StringGrid1.Cells[1,3],4,RightJustify) + FormatString(Dted_save_form.StringGrid1.Cells[1,2],1,RightJustify);
         Move(TStr[1],UHL^[29],5);

         TStr := IntegerToString(OutNumCol,4) + IntegerToString(OutNumRow,4);
         for i := 1 to 8 do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],UHL^[48],8);

         TStr := '0                        '; {Multiple Accuracy and Reserved}
         Move(TStr[1],UHL^[56],25);

         TStr := 'DSI' + FormatString(Dted_save_form.StringGrid1.Cells[1,2],1,RightJustify);
         Move(TStr[1],DSI^[1],Length(TStr));

         if OutLatInterval in [3,4] then begin
            ch := '3';
            df := 4;
         end
         else if OutLatInterval = 10 then begin
            ch := '2';
            df := 3;
         end
         else if OutLatInterval = 30 then begin
            ch := '1';
            df := 2;
         end
         else if OutLatInterval = 300 then begin
            ch := '0';
            df := 1;
         end
         else begin
            ch := 'n';
            df := 2;
         end;
         TStr := 'DTED' + ch  + 'XXX XXX';
         Move(TStr[1],DSI^[60],12);
         Tstr := FormatString(Dted_save_form.StringGrid1.Cells[1,0],2,RightJustify) + 'A' +
                 FormatString(Dted_save_form.StringGrid1.Cells[1,6],4,RightJustify) +
                 FormatString(Dted_save_form.StringGrid1.Cells[1,7],4,RightJustify) +  '0000' +
                 FormatString(Dted_save_form.StringGrid1.Cells[1,1],8,LeftJustify);
         Move(TStr[1],DSI^[88],Length(TStr));

         TStr := 'SPEXDLMS2008304MSLWGS84' +
                  FormatString(Dted_save_form.StringGrid1.Cells[1,9],10,LeftJustify) +
                  FormatString(Dted_save_form.StringGrid1.Cells[1,6],4,RightJustify);

         Move(TStr[1],DSI^[127],Length(TStr));

       {ORIGIN OF DATA}
         DegreesToDegMinSec(CornerLat,Deg,Min,Sec);
         TStr := IntegerToString(Deg,2) + IntegerToString(Min,2) + RealToString(Sec,4,1) + DEMMapProjection.LatHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[186],9);

         DegreesToDegMinSec(CornerLong,Deg,Min,Sec);
         if CornerLong > 0 then LongHemi := 'E' else LongHemi := 'W';
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,4,1) + LongHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[195],10);

        {SW corner of Data}
         DegreesToDegMinSec(Lats[1],Deg,Min,Sec);
         TStr := IntegerToString(Deg,2) + IntegerToString(Min,2) + RealToString(Sec,2,0) + DEMMapProjection.LatHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[205],7);

         DegreesToDegMinSec(Longs[1],Deg,Min,Sec);
         if CornerLong > 0 then LongHemi := 'E' else LongHemi := 'W';
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,2,0) + LongHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[212],8);

        {NW corner of Data}
         DegreesToDegMinSec(Lats[4],Deg,Min,Sec);
         TStr := IntegerToString(Deg,2) + IntegerToString(Min,2) + RealToString(Sec,2,0) + DEMMapProjection.LatHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[220],7);

         DegreesToDegMinSec(Longs[1],Deg,Min,Sec);
         if CornerLong > 0 then LongHemi := 'E' else LongHemi := 'W';
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,2,0) + LongHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[227],8);

        {NE corner of Data}
         DegreesToDegMinSec(Lats[4],Deg,Min,Sec);
         TStr := IntegerToString(Deg,2) + IntegerToString(Min,2) + RealToString(Sec,2,0) + DEMMapProjection.LatHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[235],7);

         DegreesToDegMinSec(Longs[4],Deg,Min,Sec);
         if CornerLong > 0 then LongHemi := 'E' else LongHemi := 'W';
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,2,0) + LongHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[242],8);

        {SE corner of Data}
         DegreesToDegMinSec(Lats[1],Deg,Min,Sec);
         TStr := IntegerToString(Deg,2) + IntegerToString(Min,2) + RealToString(Sec,2,0) + DEMMapProjection.LatHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[250],7);

         DegreesToDegMinSec(Longs[4],Deg,Min,Sec);
         if CornerLong > 0 then LongHemi := 'E' else LongHemi := 'W';
         TStr := IntegerToString(Deg,3) + IntegerToString(Min,2) + RealToString(Sec,2,0) + LongHemi;
         for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then TStr[i] := '0';
         Move(TStr[1],DSI^[257],8);

         TStr := IntegerToString(OutLatInterval,4);
         Move(TStr[1],DSI^[274],4);
         TStr := IntegerToString(OutLongInterval,4);
         Move(TStr[1],DSI^[278],4);
         TStr := IntegerToString(OutNumCol,4) + IntegerToString(OutNumRow,4) + '00' + Dted_save_form.StringGrid1.Cells[1,9];
         Move(TStr[1],DSI^[282],Length(TStr));
         for i := 265 to 289 do if DSI^[i] = ' ' then DSI^[i] := '0';
         DSI^[272] := '.';

         TStr := 'ACCNA  NA  NA  NA               ';

         Move(TStr[1],ACC^[1],25);
         TStr := '00';
         Move(TStr[1],ACC^[56],2);

         if (OutName = '') then begin
            OutName := DEMDefs.WriteDEMDir + DEMMapProjection.LatHemi + IntToStr(trunc(CornerLat));
            if not GetFileNameDefaultExt('Output DEM in DTED format','DTED 0|*.DT0|DTED 1|*.DT1|DTED 2|*.DT2|DTED 3|*.DT3',OutName,df,false) then exit;
            DEMDefs.WriteDEMDir := ExtractFilePath(OutName);
         end;

         if (Outname <> '') then begin
            {$IfDef RecordWriteDEM} WriteLineToDebugFile('Save DTED format (' + OutName + '), SW corner (WGS84): ' + LatLongDegreeToString(CornerLat,CornerLong,DecMinutes)); {$EndIf}

            assignFile(outf,OutName);
            rewrite(OutF,1);
            BlockWrite(OutF,UHL^,80);
            BlockWrite(OutF,DSI^,648);
            BlockWrite(OutF,ACC^,2700);
            {$IfDef RecordWriteDEM} WriteLineToDebugFile('Headers done'); {$EndIf}

            if ShowProgress then StartProgress('Create DTED');
            Missing := 0;
            for i := 0 to pred(OutNumCol) do begin
               if ShowProgress and (i mod 25 = 0) then UpDateProgressBar( i / pred(OutNumCol) );
               Long := Longs[1] + i * OutLongInterval / 36000;
               for j := 0 to pred(OutNumRow) do begin
                  Lat := Lats[1] + j * OutLatInterval / 36000;
                  LatLongDegreeToDEMGrid(Lat,Long,XGrid,YGrid);
                  if GetElevMeters(xgrid,ygrid,z) and (z < 32000) then begin
                     te := round(z);
                  end
                  else begin
                     te := MDDef.DTEDMissingValue;
                     inc(Missing);
                  end;

                  if te < 0 then begin
                     te := (32768 - te);
                     //te := -(te and $7fff);
                  end
                  else te := (te and $7fff);
                  Elevs^[2*j+9]  := hi(te);
                  Elevs^[2*j+10] := lo(te);
               end;
               Elevs^[1] := 170;  //252
               Elevs^[2] := 0;
               Elevs^[3] := hi(i);
               Elevs^[4] := lo(i);
               Elevs^[5] := hi(i);
               Elevs^[6] := lo(i);
               Elevs^[7] := hi(0);
               Elevs^[8] := lo(0);
               checksum := 0;
               Rec := 8+2*OutNumRow;
               for j := 1 to Rec do Checksum := checksum + Elevs^[j];
               Elevs^[Rec+1] := 0;
               Elevs^[Rec+2] := checksum div 256 div 256;
               Elevs^[Rec+3] := checksum mod (256 * 256) div 256;
               Elevs^[Rec+4] := checksum mod 256;
               BlockWrite(Outf,Elevs^,Rec+4);
            end;
            if (Missing = 0) then TStr := '00'
            else begin
               j := 100 - round(100.0 * Missing / OutNumCol / OutNumRow);
               if (j > 99) then j := 99;
               TStr := IntToStr(j);
               if (j < 10) then TStr := '0' + TStr;
            end;
            Move(TStr[1],DSI^[290],2);
            Seek(OutF,80);
            BlockWrite(OutF,DSI^,648);
            closeFile(OutF);
            {$IfDef RecordWriteDEM} WriteLineToDebugFile('File done');   {$EndIf}
            if ShowProgress then EndProgress;
         end;
      finally
         {$IfDef RecordWriteDEM} WriteLineToDebugFile('Finally Freed'); {$EndIf}
         DEMstatus := dsSaved;
         Dispose(elevs);
         Dispose(UHL);
         Dispose(dsi);
         Dispose(ACC);
         {$IfDef RecordWriteDEM} WriteLineToDebugFile('Disposed'); {$EndIf}
      end;
   end;
{$EndIf}





