//include file for DEMdefs
//these headers have not been used since July 2014


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



