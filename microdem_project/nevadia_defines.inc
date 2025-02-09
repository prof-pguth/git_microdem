{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

//this file should be in every unit of the program
//it accomplishes several things:
//   1. turns options on or off
//   2. adds debug code to track down problems
//   3. adapt to different supported operating systems (only Windows has been tested recently)


{$O+}    //code optimizations, were off but turned on Nov 2024
{$W-}    //stack frames changed from plus, Nov 2024
{$V-}    //relaxed var strings
{$J+}    //allows modifying typed constants


{$WEAKLINKRTTI ON}

//{$Define ExDEMIXexperimentalOptions}

//{$Define IncludePython}

//{$Define ExFresnel}

//{$Define TrackElevationPointers}

{$Define FMXU_point_cloud}

{$Define MICRODEM}
{$Define VCL}
{$Define RecordProblems}

{$IfDef MSWindows}
   {$Define UseTDBF}
   {$Define dBase_DefaultDBs}
   {$Define ExactRecordCountFix}
   {$Define UseTCLientDataSet}
{$EndIf}

{$Define RecordTime}
{$Define RecordIniMemoryOverwrite}

{$Define AllowGeomorphometry}
{$Define AllowDEMGeomorph}

{$Define UseFireDacSQLlite}

{$Define ExOddballDEMexports}
{$Define ExOddballDEMs}
{$Define ExBILWrite}
{$Define ExDRGimport}
{$Define ExGeoPDF}
{$Define ExWMS}
{$Define ExLandsatQA}
{$Define ExWaveLengthHeight}
{$Define ExDTED}

 //deleted 4/19/2023 to track down problems with recent optimizations; they are not key options
   {$Define ExSidescan}
   {$Define ExOceanography}
   {$Define ExDriftModel}
   {$Define ExSieve}
   {$Define ExLabDownloads}
   {$Define ExRedistrict}
   //{$Define ExTIN}      //this will take some work to get out of core program


