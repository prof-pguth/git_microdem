unit gps_strings;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}
{$EndIf}


interface

uses
   SysUtils,System.classes,
   Petmar_types;

  function ProcessGPRMCstring(GPSPartialString : AnsiString; var Lat,long : float64; var TimeStr : ShortString) : boolean;
  function ProcessGPGGAstring(GPSPartialString : AnsiString; var Lat,long,Elev : float64;  var TimeStr : ShortString) : boolean;

  function ProcessGPGSAstring(GPSPartialString : AnsiString; var PDOP,HDOP,VDOP : float64) : boolean;
  function ProcessPMGNWPLstring(GPSPartialString : AnsiString; var Lat,Long : float64; var WPTName : ShortString) : boolean;
  function ProcessGPGSVstring(GPSPartialString : AnsiString; var LastSentence : boolean) : boolean;

  function ProcessLineFromTRKFile(MenuStr : ShortString; var Lat,Long,z,Hour : float64) : boolean;
  function ProcessWPT_Plus_String(i : integer; MenuStr : AnsiString; var WPTName : string10; var Lat,Long : float64; var TimeStr : string35) : boolean;

  procedure TextNMEAfiletoDBF(var fName : PathStr);

type
  tGPSSatStatus = record
     Visible,UseForFix   : boolean;
     Elev,Azimuth,Signal : SmallInt;
  end;

const
   MaxGPSSat = 36;
var
   GPSSatStatus : array[1..MaxGPSSat] of tGPSSatStatus;

implementation

uses
   Petmar,DEMDef_routines,Petdbutils;


procedure TextNMEAfiletoDBF(var fName : PathStr);
var
   GPSPartialString : AnsiString;
   Lat,long,Elev : float64;
   TimeStr : ShortString;
   inf,outf : tStringList;
   i : integer;
begin
   inf := tStringList.Create;
   inf.LoadFromFile(fName);
   outf := tStringList.Create;
   outf.Add('Lat,Long,Elev,Time_str');
   for i := 0 to pred(inf.Count) do begin
       if ProcessGPGGAstring(inf.Strings[i], Lat,long,Elev,TimeStr) then begin
          outf.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + RealToString(Elev,-12,-2) + ',' + TimeStr);
       end;
   end;
   inf.Destroy;
   fName := ChangeFileExt(fName,'.dbf');
   StringList2CSVtoDB(OutF,fName,true);
end;


procedure DecodeLatLongFromString(var GPSPartialString : AnsiString; var Lat,Long : float64);
var
  err : integer;
  Min : float64;
begin
   {get latitude}
   Lat := -999;
   Long := -999;

   Val(copy(GPSPartialString,1,2),Lat,err);
   if err <> 0 then exit;
   System.Delete(GPSPartialString,1,2);
   Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),Min,err);
   if (err <> 0) then exit;
   lat := Lat + Min / 60;

   if Length(GPSPartialString) < 2 then exit;
   if GPSPartialString[1] = 'S' then Lat := -Lat;
   System.Delete(GPSPartialString,1,2);

   {get longitude}
   Val(copy(GPSPartialString,1,3),Long,err);
   if (err <> 0) then exit;
   System.Delete(GPSPartialString,1,3);

   Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),Min,err);
   if err <> 0 then exit;

   Long := Long + Min / 60;
   if Length(GPSPartialString) < 2 then exit;
   if GPSPartialString[1] = 'W' then Long := -Long;
   System.Delete(GPSPartialString,1,2);
end;


procedure DecodeTimeFromString(var GPSPartialString : AnsiString; var TimeStr : ShortString);
var
   i : integer;
   RTime : shortString;
begin
   i := 0;
   while(GPSPartialString[succ(i)] <> ',') do inc(i);
   RTime := Copy(GPSPartialString,1,i);
   TimeStr := copy(RTime,1,2) + ':' +
              copy(Rtime,3,2) + ':' +
              Copy(Rtime,5,5) + '.00';
   System.Delete(GPSPartialString,1,succ(i));
end;


procedure DeleteField(var GPSPartialString : AnsiString);
var
   i : integer;
begin
   {delete next field}
   if GPSPartialString = '' then exit;
   i := 0;
   while(GPSPartialString[succ(i)] <> ',') and (succ(i) < Length(GPSPartialString)) do inc(i);
   System.Delete(GPSPartialString,1,succ(i));
end;


function ProcessPMGNWPLstring(GPSPartialString : AnsiString; var Lat,Long : float64; var WPTName : ShortString) : boolean;
var
   i : integer;
begin
   Result := false;
   If Copy(GPSPartialString ,1,8) <> '$PMGNWPL' then exit;
   System.Delete(GPSPartialString,1,9);  //delete first field
   DecodeLatLongFromString(GPSPartialString,Lat,Long);
   for i := 1 to 2 do DeleteField(GPSPartialString);
   WPTName := BeforeSpecifiedCharacterANSI(GPSPartialString,',');
   Result := true;
end;


function ProcessWPT_Plus_String(i : integer; MenuStr : AnsiString; var WPTName : string10; var Lat,Long : float64; var TimeStr : string35) : boolean;
var
   DatumName,TStr : string35;
   err,err1 : integer;
begin
   Result := false;
   TimeStr := '';
   if (Length(MenuStr) > 0) then begin
      TStr := BeforeSpecifiedCharacterANSI(MenuStr,',',true,true);
      DatumName := BeforeSpecifiedCharacterANSI(MenuStr,',',true,true);
      if (TStr = 'WP') then WPTName := BeforeSpecifiedCharacterANSI(MenuStr,',',true,true)
      else WPTName := 'TR' + IntToStr(i);
      Val(BeforeSpecifiedCharacterANSI(MenuStr,',',true,true),Lat,err);
      Val(BeforeSpecifiedCharacterANSI(MenuStr,',',true,true),Long,err1);
      if (TStr = 'TP') then TimeStr := MenuStr;
      Result := (err = 0) and (err1 = 0);
   end;
end;


function ProcessLineFromTRKFile(MenuStr : ShortString; var Lat,Long,z,Hour : float64) : boolean;
var
   Min,Sec : float64;
   i,Err : integer;
begin
   Result := false;
   if (MenuStr[1] = 'T') then begin
      for i := 1 to length(MenuStr) do if MenuStr[i] = #9 then MenuStr[i] := ' ';
      if (MenuStr[34] = '/') then Insert(' ',MenuStr,37);
      if (MenuStr[7] = '.') then begin
      while (MenuStr[17] <> ' ') do Insert(' ',MenuStr,15);
          Val(Copy(MenuStr,5,10),Lat,Err);
          if (Err <> 0) then exit;
          if (MenuStr[4] = 'S') then Lat := -Lat;
          Val(Copy(MenuStr,19,11),Long,Err);
          if (Err <> 0) then exit;
          if (MenuStr[18] = 'W') then Long := -Long;
          System.Delete(MenuStr,1,30);  {deletes the T and lat, long}
      end
      else begin
          Val(Copy(MenuStr,5,2),Lat,Err);
          if (Err <> 0) then exit;
          Val(Copy(MenuStr,7,8),Min,Err);
          if Err <> 0 then exit;
          Lat := Lat + Min / 60;
          if MenuStr[4] = 'S' then Lat := -Lat;
          Val(Copy(MenuStr,17,3),Long,Err);
          if Err <> 0 then exit;
          Val(Copy(MenuStr,20,8),Min,Err);
          if Err <> 0 then exit;
          Long := Long + Min / 60;
          if MenuStr[16] = 'W' then Long := -Long;
          System.Delete(MenuStr,1,27);  {deletes the T and lat, long}
      end;
      while(MenuStr[1] = ' ') do System.Delete(MenuStr,1,1);
      repeat
         System.Delete(MenuStr,1,1);
      until (MenuStr[1] = ' ');  {deletes the date}
      while(MenuStr[1] = ' ') do System.Delete(MenuStr,1,1);
      Val(Copy(MenuStr,1,2),Hour,Err);
      if Err <> 0 then exit;
      Val(Copy(MenuStr,4,2),Min,Err);
      if Err <> 0 then exit;
      Val(Copy(MenuStr,7,2),Sec,Err);
      if Err <> 0 then exit;
      Hour := Hour + Min / 60 + Sec / 3600;
      System.Delete(MenuStr,1,8);  {deletes the time}
      StripBlanks(MenuStr);
      Val(MenuStr,z,Err);
      if Err <> 0 then exit;
      result := true;
   end;
end;


function ProcessGPGSAstring(GPSPartialString : AnsiString; var PDOP,HDOP,VDOP : float64) : boolean;
var
   i,err,sat : integer;
   CopyString : AnsiString;
{
        GSA - GPS DOP and active satellites
        GSA,A,3,04,05,,09,12,,,24,,,,,2.5,1.3,2.1*39
           A            Auto selection of 2D or 3D fix (M = manual)
           3            3D fix
           04,05...     PRNs of satellites used for fix (space for 12)
           2.5          PDOP (dilution of precision)
           1.3          Horizontal dilution of precision (HDOP)
           2.1          Vertical dilution of precision (VDOP)
}
//$GPGSA,A,3,3,19,18,,31,,,,,,,,2.38,1.51,1.84*33
begin
   Result := (Copy(GPSPartialString,1,6) = '$GPGSA');
   if Result then begin
      for i := 1 to MaxGPSSat do GPSSatStatus[i].UseForFix := false;
      System.Delete(GPSPartialString,1,7);
      CopyString := GPSPartialString;
      for i := 1 to 2 do DeleteField(GPSPartialString);
      for i := 1 to 12 do begin
         Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),sat,err);
         if (err = 0) and (Sat in [1..MaxGPSSat]) then GPSSatStatus[Sat].UseForFix := true;
      end;
      GPSPartialString := CopyString;
      for i := 1 to 14 do DeleteField(GPSPartialString);

      Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),PDOP,err);
      Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),HDOP,err);
      Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),VDOP,err);
   end;
end;


function ProcessGPRMCstring(GPSPartialString : AnsiString; var Lat,long : float64;  var TimeStr : ShortString) : boolean;
{
        RMC - Recommended minimum specific GPS/Transit data
        RMC,225446,A,4916.45,N,12311.12,W,000.5,054.7,191194,020.3,E*68
           225446       Time of fix 22:54:46 UTC
           A            Navigation receiver warning A = OK, V = warning
           4916.45,N    Latitude 49 deg. 16.45 min North
           12311.12,W   Longitude 123 deg. 11.12 min West
           000.5        Speed over ground, Knots
           054.7        Course Made Good, True
           191194       Date of fix  19 November 1994
           020.3,E      Magnetic variation 20.3 deg East
           *68          mandatory checksum

$GPRMC,164617,A,3858.98,N,07628.75,W,00.0,000.0,151096,11.,W*41
}
begin
   Result := Copy(GPSPartialString ,1,6) = '$GPRMC';
   If Result then begin
      System.Delete(GPSPartialString,1,7);  //delete first field
      DecodeTimeFromString(GPSPartialString,TimeStr);
      DeleteField(GPSPartialString);
      DecodeLatLongFromString(GPSPartialString,Lat,Long);
   end;
end;


function ProcessGPGSVstring(GPSPartialString : AnsiString; var LastSentence : boolean) : boolean;
(*
        GSV - Satellites in view
        GSV,2,1,08,01,40,083,46,02,17,308,41,12,07,344,39,14,22,228,45*75
           2            Number of sentences for full data
           1            sentence 1 of 2
           08           Number of satellites in view
           01           Satellite PRN number
           40           Elevation, degrees
           083          Azimuth, degrees
           46           Signal strength - higher is better
           <repeat for up to 4 satellites per sentence>
                There my be up to three GSV sentences in a data packet


$GPGSA,A,3,,06,09,,,17,,21,,,,,2.3,2.1,1.0

$GPGSV,3,3,11,  22,20,285,00,
                26,49,044,00,
                29,38,045,00

$GPGSV,3,2,11,  15,24,315,00,
                17,10,136,00,
                18,51,303,00,
                21,74,293,44
$GPGSV,3,1,11,  03,06,321,00,
                06,16,208,44,
                09,34,149,37,
                10,11,087,00
*)
var
   NumSentences,ThisSentence,{VisSats,}i,Sat,err : integer;
begin
   Result := Copy(GPSPartialString,1,6) = '$GPGSV';
   If Result then begin
      System.Delete(GPSPartialString,1,7);  //delete first field

      Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),NumSentences,err);
      Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),ThisSentence,err);

      if (ThisSentence = 1) then for i := 1 to MaxGPSSat do with GPSSatStatus[i] do begin
          Elev := 0;
          Azimuth := 0;
          Signal := 0;
          Visible := false;
      end;
      while length(GPSPartialString) > 0 do begin
         Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),Sat,err);
         if Sat in [1..MaxGPSSat] then with GPSSatStatus[Sat] do begin
            Visible := true;
            Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),Elev,err);
            Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),Azimuth,err);
            Val(BeforeSpecifiedCharacterANSI(GPSPartialString,',',True,True),Signal,err);
         end;
      end;
      LastSentence := (ThisSentence = NumSentences);
   end;
end;


function ProcessGPGGAstring(GPSPartialString : AnsiString; var Lat,long,Elev : float64;  var TimeStr : ShortString) : boolean;
var
   i,err : integer;
(*
        GGA - Global Positioning System Fix Data
        GGA,123519,4807.038,N,01131.324,E,1,08,0.9,545.4,M,46.9,M, , *42
           123519       Fix taken at 12:35:19 UTC
           4807.038,N   Latitude 48 deg 07.038' N
           01131.324,E  Longitude 11 deg 31.324' E
           1            Fix quality: 0 = invalid
                                     1 = GPS fix
                                     2 = DGPS fix
           08           Number of satellites being tracked
           0.9          Horizontal dilution of position
           545.4,M      Altitude, Metres, above mean sea level
           46.9,M       Height of geoid (mean sea level) above WGS84
                        ellipsoid
           (empty field) time in seconds since last DGPS update
           (empty field) DGPS station ID number
*)
{
$GPGGA,013245.0,0747.898,S,11022.380,E,1,05,2.45,00136,M,004,M,,*40
}
begin
   Result := Copy(GPSPartialString ,1,6) = '$GPGGA';
   If Result then begin
      System.Delete(GPSPartialString,1,7);  //delete first field
      DecodeTimeFromString(GPSPartialString,TimeStr);
      DecodeLatLongFromString(GPSPartialString,Lat,Long);
      for i := 1 to 3 do DeleteField(GPSPartialString);
      Val(BeforeSpecifiedCharacterANSI(GPSPartialString,','),Elev,err);
   end;
end;


end.
