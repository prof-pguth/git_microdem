//include file for DEMdefs
//these headers have not been used since July 2014

type
   tDMAMapRawDefinition = packed record
      h_Adat,
      h_f          : double;
      h_XDat,
      h_YDat,
      h_ZDat       : int16;
      h_DatumCode  : array[1..6] of byte;
      h_EllipsCode : array[1..3] of byte;
   end;


type
   tHeaderRecordv3 = packed record  //for a DEM, version 3, introduced 22 May 2010
      DEMUsed      : tDEMAvailable;
      DEMPrecision : tDEMprecision;
      DataSpacing  : tSpacingUnit;
      MaxElev,                    {max elevation in data set}
      MinElev,                    {min elevation in data set}
      fLatInterval,               {y spacing}
      fLongInterval : float32; {x spacing}
      OldNumCol,OldNumRow : word;  //removed 6/16/2011, so you can get larger grids
      ElevUnits    : tElevUnit;
      {$IfDef MSWindows}
         oldDefProjName  : string[7];   {Pascal, 8 bytes, byte[0] is string length} {complex projection definition}
      {$Else}
         oldDefProjName  : array[0..7] of byte;
      {$EndIf}
      {$IfDef MSWindows}
         unusedSquareID : string[2];
      {$Else}
         unusedSquareID  : array[0..2] of byte;
      {$EndIf}
      UTMZone      : Byte;        {6 degree UTM Zone number, USGS/MGRS standard: 1 = W177, 60 = E177}
      DMAMapDefinition  : tDMAMapRawDefinition;
      DigitizeDatum : tDigitizeDatum; {sets datum for DEM, and it is transformed to the desired local datum for use}
      LatHemi       : AnsiChar;       {N or S}

      {$IfDef MSWindows}
         CreationInformation : string[29];
      {$Else}
         CreationInformation : array[0..29] of byte;
      {$EndIf}

      FIPS_Zone : Int16;

      hdfSWCornerx,
      hdfSWCornerY   : float64;
      NumCol,NumRow  : int32;  //added 6/16/2011, for even larger grids
      FutureUse : array[1..7] of byte;
   end;

   tHeaderRecordV2 = packed record  //for a DEM, version 2, introduced 17 May 2004
      DEMUsed      : tDEMAvailable;
      DEMPrecision : tDEMprecision;
      DataSpacing  : tSpacingUnit;
      NumCol,                    {number of columns in data set}
      NumRow       : int16;      {number of rows in data set}
      MaxElev,                   {max elevation in data set}
      MinElev,                   {min elevation in data set}
      fLatInterval,              {y spacing}
      fLongInterval,             {x spacing}
      oldhdfSWCornerx,
      oldhdfSWCornerY   : float32;
      ElevUnits    :  tElevUnit;
      oldDefProjName  : string[7];   {Pascal, 8 bytes, byte[0] is the length} {complex projection definition}
      SquareID     : string[2];     {Pascal, 3 bytes, byte[0] is the length}
      UTMZone      : Byte;        {6 degree UTM Zone number, USGS/MGRS standard: 1 = W177, 60 = E177}
      DMAMapDefinition    : tDMAMapRawDefinition;
      DigitizeDatum : tDigitizeDatum; {sets datum for DEM, and it is transformed to the desired local datum for use}
      LatHemi          : AnsiChar;       {N or S}
      CreationInformation : string[29];
      FIPS_Zone : int16;
      hdfSWCornerx,
      hdfSWCornerY   : float64;
      FutureUse : array[1..7] of byte;
   end;

   tHeaderRecordv1 = packed record  //for a DEM, set a very long time ago
      NumCol,                  {number of columns in data set}
      NumRow       : int32; {number of rows in data set}
      oSquareID    : string[2];  {currently unused?}
      oMaxElev,                 {max elevation in data set}
      oMinElev      : int16; {min elevation in data set}
      oiLatInterval : byte;     {x data point spacing, will be removed at some point}
      oiLongInterval : byte;    {y data point spacing, will be removed at some point}
      fLatInterval,            //added Dec 2002 in place of previously unused bytes
      fLongInterval,           //need to be floats for 1/3" NED
      fSWCornerx,              //all are 4 byte IEEE floating point values
      fSWCornerY    : float32;
      UnusedLengthBytes : array[1..2] of byte;
      UnusedWhichSphere  : AnsiChar;
      UnusedOldDatum     : byte;
      {end original structure; additions required decrease in FutureUse, originally 99 bytes}
      oBaseXUTM24K,                     {lower left (SW) UTM for 1:24K DEM, added April 1988 }
      oBaseYUTM24K  : int32;          {for lat/long DEMs it is in seconds (or other spacing unit)}
      ElevUnits    :  tElevUnit;
      oldDefProjName  : string[7];    //added Dec 2002
      UTMZone      : Byte;        {6ø UTM Zone number, added Sept 88, USGS/MGRS standard: 1 = W177, 60 = E177}
      DEMUsed      : tDEMAvailable;   {added March 89}
      DataSpacing  : tSpacingUnit;    {added 5/2/89 for DEMs in arc minutes}
      DigitizeDatum : tDigitizeDatum; {added 5/9/89 for new datums, and spherical data; sets the datum for the DEM, and it is transformed to the desired local datum for use}
      DigitizeLong0,                    {in whole degrees--currently unused?}
      DigitizeLat0,                     {in whole degrees--currently unused?}
      DigitizeScale       : int16;   {times 1000--currently unused?}
      DigitizeRadEarth    : int32;    {times 10--currently unused?}
      ElevOffset          : int16;   {add to all elevs for real; added Mar 93--currently unused}
      LatHemi          : AnsiChar;       {N or S, added Feb 94}
      unusedDatumNumber     : byte;       {added July 95} {obsolete June 98}
      unusedEllipsoidNumber : int16;   {added July 95} {obsolete June 98}
      DMAMapDefinition    : tDMAMapRawDefinition; {added July 95, first used summer 97, and now obsolete}
      FutureUse           : byte;
      CreationInformation : string[29];   {added Jan 90}
   end;



                procedure ReadHeaderV2;
                var
                   HeaderV2 : tHeaderRecordV2;
                begin
                   BlockRead(Ffile,Headerv2,SizeOf(HeaderV2));
                   DEMheader.DEMUsed      := HeaderV2.DEMUsed;
                   DEMheader.DEMPrecision := HeaderV2.DEMPrecision;
                   DEMheader.DataSpacing  := HeaderV2.DataSpacing;
                   DEMheader.MaxElev := HeaderV2.MaxElev;
                   DEMheader.MinElev := HeaderV2.MinElev;
                   DEMheader.DEMySpacing := HeaderV2.fLatInterval;
                   DEMheader.DEMxSpacing := HeaderV2.fLongInterval;
                   DEMheader.NumCol := HeaderV2.NumCol;
                   DEMheader.NumRow := HeaderV2.NumRow;
                   DEMheader.ElevUnits := HeaderV2.ElevUnits;
                   DEMheader.UTMZone  := HeaderV2.UTMZone;
                   //DEMheader.DMAMapDefinition := HeaderV2.DMAMapDefinition;
                   //DEMheader.aDigitizeDatum  := HeaderV2.DigitizeDatum;
                   DEMheader.LatHemi := HeaderV2.LatHemi;
                   DEMheader.DEMPrecision := HeaderV2.DEMPrecision;
                   DEMheader.SWCornerX := HeaderV2.hdfSWCornerx;
                   DEMheader.SWCornerY  := HeaderV2.hdfSWCornerY;
                   if (abs(DEMheader.SWCornerX) < 0.0000001) and (abs(HeaderV2.oldhdfSWCornerY) > 0.0000001) then begin
                      DEMheader.SWCornerX := HeaderV2.oldhdfSWCornerx;
                      HeaderV2.oldhdfSWCornerx := 0;
                   end;
                   if (abs(DEMheader.SWCornerY) < 0.0000001) and (abs(HeaderV2.oldhdfSWCornerY) > 0.0000001) then begin
                      DEMheader.SWCornerY := HeaderV2.oldhdfSWCornerY;
                      HeaderV2.oldhdfSWCornerY := 0;
                   end;
                end;

                procedure ReadHeaderV1;
                var
                   OldHeadRecs : tHeaderRecordv1;
                   l1 : float32;
                begin
                   BlockRead(Ffile,OldHeadRecs,SizeOf(OldHeadRecs));
                   DEMheader.DEMUsed := OldHeadRecs.DEMUsed;
                   DEMheader.DataSpacing  := OldHeadRecs.DataSpacing;
                   DEMheader.NumCol := OldHeadRecs.NumCol;
                   DEMheader.NumRow := OldHeadRecs.NumRow;
                   DEMheader.ElevUnits := OldHeadRecs.ElevUnits;
                   DEMheader.UTMZone   := OldHeadRecs.UTMZone;
                   //DEMheader.DMAMapDefinition := OldHeadRecs.DMAMapDefinition;
                   //DEMheader.aDigitizeDatum := OldHeadRecs.DigitizeDatum;
                   DEMheader.LatHemi    := OldHeadRecs.LatHemi;
                   if (OldHeadrecs.oiLatInterval <> 0) and (OldHeadrecs.oiLongInterval <> 0) then begin
                      case OldHeadRecs.DataSpacing of
                         SpaceMeters : l1 := 1.0;
                         SpaceKM     : l1 := 1000.0;
                         Space100m   : l1 := 100.0;
                         Space10m    : l1 := 10.0;
                         SpaceFeet   : l1 := Petmar_types.FeetToMeters;
                         SpaceKFeet  : l1 := 304.8;
                         oldSpaceMinutes : l1 := 60;
                         oldSpaceSeconds : l1 := 3600;
                         else L1 := 1;
                      end;
                      DEMheader.SWCornerY := OldHeadrecs.oBaseYUTM24K / l1;
                      DEMheader.SWCornerX := OldHeadrecs.oBaseXUTM24K / l1;
                      DEMheader.DEMySpacing  := OldHeadrecs.oiLatInterval / l1;
                      DEMheader.DEMxSpacing := OldHeadrecs.oiLongInterval / l1;
                   end
                   else begin
                      DEMheader.DEMySpacing := OldHeadRecs.fLatInterval;
                      DEMheader.DEMxSpacing := OldHeadRecs.fLongInterval;
                      DEMheader.SWCornerX := OldHeadRecs.fSWCornerx;
                      DEMheader.SWCornerY := OldHeadRecs.fSWCornerY;
                   end;
                   DEMheader.MaxElev := ZinMeters(OldHeadRecs.oMaxElev);
                   DEMheader.MinElev := ZinMeters(OldHeadRecs.oMinElev);
                   DEMheader.DEMPrecision := SmallIntDEM;
                end;


                procedure ReadHeaderV3;
                var
                   HeaderV3 : tHeaderRecordV3;
                begin
                   BlockRead(Ffile,Headerv3,SizeOf(Headerv3)); //version 3 header, post 5/22/2010
                   DEMheader.DEMUsed      := HeaderV3.DEMUsed;
                   DEMheader.DEMPrecision := HeaderV3.DEMPrecision;
                   DEMheader.DataSpacing  := HeaderV3.DataSpacing;
                   DEMheader.MaxElev := HeaderV3.MaxElev;
                   DEMheader.MinElev := HeaderV3.MinElev;
                   DEMheader.DEMySpacing := HeaderV3.fLatInterval;
                   DEMheader.DEMxSpacing := HeaderV3.fLongInterval;
                   DEMheader.NumCol := HeaderV3.NumCol;
                   DEMheader.NumRow := HeaderV3.NumRow;
                   DEMheader.ElevUnits := HeaderV3.ElevUnits;
                   DEMheader.UTMZone  := HeaderV3.UTMZone;
                   //DEMheader.DMAMapDefinition := HeaderV3.DMAMapDefinition;
                   //DEMheader.aDigitizeDatum  := HeaderV3.DigitizeDatum;
                   DEMheader.LatHemi  := HeaderV3.LatHemi;
                   DEMheader.DEMPrecision := HeaderV3.DEMPrecision;
                   DEMheader.SWCornerX := HeaderV3.hdfSWCornerx;
                   DEMheader.SWCornerY  := HeaderV3.hdfSWCornerY;
                   if (DEMheader.NumCol = 0) or (DEMheader.NumRow = 0) then begin // DEM from program version before 6/16/2011
                      DEMheader.NumCol := HeaderV3.OldNumCol;
                      DEMheader.NumRow := HeaderV3.OldNumRow;
                   end;
                end;


             procedure ReadThatOldHeader;

                   function FirstIntegerOnLine(var aString : AnsiString) : integer;

                            function NextLineFromString(var aString : AnsiString) : AnsiString;
                            var
                               i : integer;
                            begin
                               i := 1;
                               while not (aString[i] in [#10,#13]) do begin
                                  inc(i);
                                  if (i > length(aString)) then begin
                                     Result := '';
                                     exit;
                                  end;
                               end;
                               Result := ptCopy(astring,1,i);
                               Delete(astring,1,i);
                            end;

                   var
                      TStr : AnsiString;
                      err : integer;
                   begin
                      TStr := NextLineFromString(aString);
                      TStr := BeforeSpecifiedCharacterANSI(TStr,' ',false);
                      val(TStr,Result,err);
                   end;

             var
                aTStr : shortstring;
                HeaderOffset,DEMoffset : integer;
             begin
                aTStr := TStr;
                HeaderOffset := FirstIntegerOnLine(aTStr);
                DEMoffset := FirstIntegerOnLine(aTStr);
                //if (DEMOffset > 192) then LogRecAOffset := FirstIntegerOnLine(aTStr)
                //else LogRecAOffset := 0;

                reset(fFile,1);
                Seek(fFile,HeaderOffset);
                {$IfDef Record_MD_DEM} WriteLineToDebugFile('Magic=' + Magic);  {$EndIf}

                if (Magic ='*MICRODEM DEM') then ReadHeaderV1
                else if (Magic ='*MICRODEM v.2') then ReadHeaderV2
                else if (Magic ='*MICRODEM v.3') then ReadHeaderV3;

                (*
                case DEMheader.DigitizeDatum of
                  NAD27d : StringToByteArray('NAS-C',DEMheader.DMAMapDefinition.h_DatumCode);
                  WGS72d : StringToByteArray('WGS72',DEMheader.DMAMapDefinition.h_DatumCode);
                  NAD83d : StringToByteArray('NAR-C',DEMheader.DMAMapDefinition.h_DatumCode);
                  WGS84d : StringToByteArray('WGS84',DEMheader.DMAMapDefinition.h_DatumCode);
               end {case};
               *)
             end;





