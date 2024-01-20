
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{include file for demcoord.pas       }
{____________________________________}

procedure GetDefaultWorldFile(var ReadFileName : PathStr);
var
   Ext : ExtStr;
begin
   {$IfDef RecordWorldFile} WriteLineToDebugFile('GetDefaultWorldFile in ' + ReadFileName); {$EndIf}
   if not FindExistingWorldFile(ReadFileName) then begin
      Ext := UpperCase(ExtractFileExt(ReadFileName));
      if (Ext= '.JPG') or (Ext= '.JPEG')  or (Ext= '.JPE') then  begin
         ReadFileName := ChangeFileExt(ReadFileName,'.jgw');
      end
      else if (Ext= '.TIF') or (Ext= '.TIFF') then  begin
         ReadFileName := ChangeFileExt(ReadFileName,'.tfw');
      end
      else if (Ext= '.GIF') then  begin
         ReadFileName := ChangeFileExt(ReadFileName,'.gfw');
      end
      else if (Ext= '.PNG') then  begin
         if FileExists(ChangeFileExt(ReadFileName,'.PNW')) then ReadFileName := ChangeFileExt(ReadFileName,'.pnw')
         else ReadFileName := ChangeFileExt(ReadFileName,'.pgw');
      end
      else if (Ext= '.BMP') then  begin
         ReadFileName := ChangeFileExt(ReadFileName,'.bpw');
      end
      else if (Ext= '.SID') then begin
         ReadFileName := ChangeFileExt(ReadFileName,'.sdw');
      end;
   end;
   {$IfDef RecordWorldFile} WriteLineToDebugFile('GetDefaultWorldFile out ' + ReadFileName); {$EndIf}
end;


function ValidWorldFile(FileName : PathStr) : boolean;
var
   Ext : ExtStr;
begin
   Ext := UpperCase(ExtractFileExt(FileName));
   Result :=  ExtEquals(Ext, '.WLD') or ExtEquals(Ext, '.JPW') or ExtEquals(Ext, '.JGW') or ExtEquals(Ext, '.TFW') or ExtEquals(Ext, '.GFW') or
              ExtEquals(Ext, '.PNW') or ExtEquals(Ext, '.PGW') or ExtEquals(Ext, '.BPW') or ExtEquals(Ext, '.SDW') or ExtEquals(Ext, '.WORLD');
end;


function FindExistingWorldFile(var ReadFileName : PathStr) : boolean;
var
   Ext : ExtStr;

         function FindWorld(NewExt : shortstring) : boolean;
         var
            tName : PathStr;
         begin
            FindWorld := false;
            tName := ChangeFileExt(ReadFileName,NewExt);
            if FileExists(tName) then begin
               ReadFileName := tName;
               FindWorld := true;
            end;
         end;

begin
   {$IfDef RecordWorldFile} WriteLineToDebugFile('ChangeFileNameToWorldFile in ' + ReadFileName); {$EndIf}
   Result := false;
   if ValidWorldFile(ReadFileName) then begin
      Result := true;
   end
   else  begin
      Ext := UpperCase(ExtractFileExt(ReadFileName));
      if (Ext = '.JPG') or (Ext = '.JPEG')  or (Ext = '.JPE') then begin
         Result := (FindWorld('.jpw')) or (FindWorld('.jgw'))
      end
      else if (Ext= '.TIF') or (Ext= '.TIFF') then begin
         Result := FindWorld('.TFW') or FindWorld('.TIFW');
      end
      else if (Ext= '.GIF') then begin
         Result := FindWorld('.GFW');
      end
      else if (Ext= '.PNG') then begin
         Result := FindWorld('.pnw') or FindWorld('.pgw');
      end
      else if (Ext= '.BMP') then begin
          Result := FindWorld('.BPW');
      end
      else if (Ext= '.SID') then begin
          Result := FindWorld('.SDW');
      end;
   end;
   {$IfDef RecordWorldFile} WriteLineToDebugFile('ChangeFileNameToWorldFile out ' + ReadFileName); {$EndIf}
end;

function CreateWorldFileStringList(xPixelSize,yPixelSize,xutm,yutm : float64) : tStringList;
begin
    Result := tStringList.Create;
    Result.Add(RealToString(xPixelSize,20,12));
    Result.Add('0');
    Result.Add('0');
    Result.Add(RealToString(-yPixelSize,20,12));
    Result.Add(RealToString(xutm,20,12));
    Result.Add(RealToString(yutm,20,12));
end;


procedure SaveWorldFile(FName : PathStr; xPixelSize,yPixelSize,xutm,yutm : float64; DatumCode : ShortString; UTMZone : integer; SatLatHemi : AnsiChar);
var
   WorldFile : tStringList;
begin
    GetDefaultWorldFile(FName);
    WorldFile := CreateWorldFileStringList(xPixelSize,yPixelSize,xutm,yutm);
    WorldFile.SaveToFile(FName);
    {$IfDef RecordWorldFile} WriteLineToDebugFile('Save with world file ' + fName); WriteStringListToDebugFile(WorldFile); {$EndIf}

    WorldFile.Clear;
    WorldFile.Add(DatumCode);
    WorldFile.Add(IntToStr(UTMZone));
    WorldFile.Add(SatLatHemi);
    WorldFile.SaveToFile(ChangeFileExt(FName,'.datum'));    //for at least some, this may be duplicate effort

    {$IfDef RecordWorldFile} WriteLineToDebugFile('Save datum with world file ' + fName); WriteStringListToDebugFile(WorldFile); {$EndIf}
    WorldFile.Free;
end;


function ReadWorldFileValues(FileName : PathStr; var DeltaX,Xrot,YRot,DeltaY,UpLeftX,UpLeftY : float64) : boolean;
var
   i,err : integer;
   TFWFile : tStringList;
   MenuStr : ANSIString;
begin
    {$IfDef RecordWorldFile} WriteLineToDebugFile('Read world file values: ' + FileName); {$EndIf}
    if FindExistingWorldFile(FileName) then begin
       TFWFile := TStringList.Create;
       TFWFile.LoadFromFile(FileName);
       if (TFWFile.Count = 0)then  begin
          {$IfDef RecordWorldFile} WriteLineToDebugFile('World file is 0 length'); {$EndIf}
       end
       else begin
           if (TFWFile.Count = 1) then begin
              {$IfDef RecordWorldFile} WriteLineToDebugFile('Single line: ' + TFWFile.Strings[0]); {$EndIf}
              MenuStr := TFWFile.Strings[0];
              for i := 0 to 5 do TFWFile.Add(BeforeSpecifiedCharacterANSI(MenuStr,#10,true,true));
              TFWFile.Clear;
           end;
           for i := 0 to 5 do begin
              MenuStr := ptTrim(TFWFile.Strings[i]);
              TFWFile.Strings[i] := MenuStr;
           end;
           Val(TFWFile.Strings[0],DeltaX,Err);
           Val(TFWFile.Strings[1],Xrot,Err);
           Val(TFWFile.Strings[2],Yrot,Err);
           Val(TFWFile.Strings[3],DeltaY,Err);
           Val(TFWFile.Strings[4],UpLeftX,Err);
           Val(TFWFile.Strings[5],UpLeftY,Err);
           {$IfDef RecordWorldFile} WriteLineToDebugFile('ReadWorldFileValues ' + FileName); WriteStringListToDebugFile(TFWFile); {$EndIf}
           Result := true;
       end;
       TFWFile.Free;
    end
    else begin
       {$IfDef RecordWorldFile} WriteLineToDebugFile('Did not find the existing world file');{$EndIf}
       Result := false;
    end;
end;


function ReadWorldFile(var MapProjection : tMapProjection; var DigitizeDatum : ShortString; FileName : PathStr; var RegVars : tRegVars) : boolean;
var
   DatumFile : tStringList;
   RawDatum : tMapProjection;
   err : integer;
   Lat,Long,xrot,yrot : float64;
   tFile : textFile;
   HaveToAsk,GeoLatLong : boolean;
   TStr : shortstring;
   vmProjection : tMapProjection;
   fName, DatumFName : PathStr;

       procedure DefinePlateCareeProjection;
       begin
          RegVars.Registration := RegProjection;
          MapProjection.PName := PlateCaree;
          MapProjection.GetProjectParameters;
       end;

       procedure DefineMercatorProjection;
       begin
          RegVars.Registration := RegProjection;
          MapProjection.Long0 := 0;
          MapProjection.PName := MercatorEllipsoid;
          MapProjection.GetProjectParameters;
       end;

       procedure DefineSouthPolarStereographicProjection;
       begin
          RegVars.Registration := RegProjection;
          MapProjection.Long0 := 0;
          MapProjection.Lat0 := -71 * DegToRad;
          MapProjection.PName := PolarStereographicEllipsoidal;
          MapProjection.GetProjectParameters;
       end;



begin
    Result := false;
    if not FileExists(FileName) then exit;
    Result := true;
    RawDatum := tMapProjection.Create('world file');

    {$IfDef RecordWorldFile} WriteLineToDebugFile('Read world file for ' + FileName); {$EndIf}

    with RegVars,RawDatum do begin
       RawDatum.PName := UTMellipsoidal;
       Registration := RegProjection;
       h_DatumCode := 'WGS84';
       projUtmZone := MDdef.DefaultUTMZone;

       if not ReadWorldFileValues(FileName,pr_DeltaX,Xrot,YRot,pr_DeltaY,UpLeftX,UpLeftY) then begin
          {$IfDef RecordWorldFile} WriteLineToDebugFile('Failure reading world file'); {$EndIf}
          RegVars.Registration := RegNone;
          Result := false;
          exit;
       end;

       {$IfDef RecordWorldFile} WriteLineToDebugFile('Read world file, ' + RealToString(UpLeftX,12,1) + RealToString(UpLeftY,12,1) + ' UL ' + RealToString(pr_DeltaX,18,-6) + RealToString(pr_DeltaY,18,-6) + ' deltas'); {$EndIf}

       if (abs(Xrot) > 0.0001) or (abs(yrot) > 0.0001) then MessageToContinue('Rotation not implemented for ' + FileName);
       pr_deltaY := -pr_deltaY;
       if (abs(UpLeftY) < 90.001) and (abs(UpLeftX) < 180.001) then begin
          DefinePlateCareeProjection;
       end
       else begin
          DatumFName := ChangeFileExt(FileName,'.datum');
          if not FileExists(DatumFName) then begin
             {$IfDef RecordWorldFile} WriteLineToDebugFile('Datum file is missing'); {$EndIf}
             DatumFName := ExtractFilePath(FileName) + 'all.datum';
             {$IfDef RecordWorldFile} if FileExists(DatumFName) then WriteLineToDebugFile('Using ' + FileName); {$EndIf}
          end;

          if FileExists(ExtractFilePath(FileName) + 'mercator.txt') then  begin
             {$IfDef RecordWorldFile} WriteLineToDebugFile('found mercator.txt'); {$EndIf}
             DefineMercatorProjection;
          end
          else if FileExists(ExtractFilePath(FileName) + 'south_polar_stereographic.txt') then  begin
             {$IfDef RecordWorldFile} WriteLineToDebugFile('found south_polar_stereographic.txt'); {$EndIf}
             DefineSouthPolarStereographicProjection;
          end
          else if FileExists(ChangeFileExt(FileName,'.prj')) then begin
             {$IfDef RecordWorldFile} WriteLineToDebugFile('Using PRJ file'); {$EndIf}
             vmProjection.InitializeProjectionFromWKT(ChangeFileExt(FileName,'.prj'));
             if vmProjection.ProjUTMZone in [1..60] then begin
                projUTMzone := vmProjection.ProjUTMZone;
                vmProjection.Pname := UTMellipsoidal;
             end
             else begin
                MessageToContinue('PRJ file type not supported yet');
             end;
          end
          else if FileExists(DatumfName) then begin
             {$IfDef RecordWorldFile} WriteLineToDebugFile('Using Datum file'); {$EndIf}
             AssignFile(tfile,DatumfName);
             reset(Tfile);
             readln(tFile,TStr);
             if (TStr = 'South Polar Stereographic') then begin
                DefineMercatorProjection;
             end
             else if (TStr = 'Mercator') then begin
                DefineMercatorProjection;
             end
             else begin
                RawDatum.h_DatumCode := TStr;
                readln(tfile,projUtmZone);
                readln(tfile,LatHemi);
             end;
             CloseFile(Tfile);
          end
          else begin
             {$IfDef RecordWorldFile} WriteLineToDebugFile('Fall through case'); {$EndIf}
             HaveToAsk := false;
             //if (MapProjection.PName = PlateCaree) and (UpLeftY < 0) then SatLatHemi := 'S';
             TStr := UpperCase(ExtractFileName(FileName));
             if (TStr[1] in ['C','F','O','G']) then begin
                h_DatumCode := 'NAD27';
                Val(Copy(TStr,4,3),long,err);
                if (err = 0) then projUtmZone := GetUTMZone(-long + 0.25);
             end;
             if (MapProjection.PName <> PlateCaree) and (pr_DeltaX < 0.01) then begin
                {$IfDef RecordWorldFile} WriteLineToDebugFile('Get datum and UTM zone for ' + TStr); {$EndIf}
                if (TStr[1] in ['N','S']) and (TStr[2] = '-') and (TStr[3] in ['0'..'9']) and (TStr[4] in ['0'..'9']) then begin
                   {$IfDef RecordWorldFile} WriteLineToDebugFile('Geocover'); {$EndIf}
                   LatHemi := TStr[1];
                   projUtmZone := StrToInt(Copy(TStr,3,2));
                end
                else begin
                   HaveToAsk := true;
                end;
             end;

             if HaveToAsk or MDDef.VerifyAllWorldFiles then begin
                 {$IfDef RecordWorldFile} WriteLineToDebugFile('Have to ask'); {$EndIf}
                 GetMapParametersSPCSOption(fName,LatHemi,projUtmZone,h_DatumCode,PName,GeoLatLong,false,TStr);
             end;

             DatumFile := TStringList.Create;
             DatumFile.Add(h_DatumCode);
             DatumFile.Add(IntToStr(projUtmZone));
             DatumFile.Add(LatHemi);
             If (DatumFile.Count > 0) then DatumFile.SaveToFile(DatumfName);
             DatumFile.Free;
          end;
       end;

       {$IfDef RecordDefineDatum} WriteLineToDebugFile('ReadWorldFile raw'); {$EndIf}
       RawDatum.DefineDatumFromUTMZone(h_DatumCode,projUTMZone,LatHemi,'read dem 1');
       {$IfDef RecordDefineDatum} WriteLineToDebugFile('ReadWorldFile SatDatumConstants'); {$EndIf}
       MapProjection.DefineDatumFromUTMZone(MDdef.PreferPrimaryDatum,projUTMZone,LatHemi,'readdem 2');
       if (RawDatum.h_DatumCode <> MDdef.PreferPrimaryDatum) then begin
          if (Registration = RegProjection) then begin
          end
          else if (MapProjection.PName = UTMellipsoidal) then begin
             RawDatum.UTMtoLatLongDegree(UpLeftX,UpLeftY,Lat,Long);
             MolodenskiyTransformation(Lat,Long,Lat,Long,RawDatum,MapProjection);
             MapProjection.ForwardProjectDegrees(Lat,Long,UpLeftX,UpLeftY);
          end
          else begin
             MolodenskiyTransformation(UpLeftY,UpLeftX,UpLeftY,UpLeftX,RawDatum,MapProjection);
          end;
          DigitizeDatum := RawDatum.h_DatumCode;
          {$IfDef RecordWorldFile} WriteLineToDebugFile('World file datum transformed, from ' + RawDatum.h_DatumCode + ' to ' + MDdef.PreferPrimaryDatum); {$EndIf}
       end;

       {$IfDef RecordWorldFile} WriteLineToDebugFile(RealToString(UpLeftX,12,1) + RealToString(UpLeftY,12,1) + ' UL ' + RealToString(pr_DeltaX,18,-6) + RealToString(pr_DeltaY,18,-6) + ' deltas'); {$EndIf}
    end;
    RawDatum.Destroy;
    {$IfDef RecordWorldFile} WriteLineToDebugFile('Read world file out'); {$EndIf}
end;
