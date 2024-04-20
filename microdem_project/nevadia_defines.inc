{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

//this file should be in every unit of the program
//it accomplishes several things:
//   1. turns options on or off
//   2. adds debug code to track down problems
//   3. adapt to the different supported operating systems


{$O-,W+,V-,J+}


{$WEAKLINKRTTI ON}


//{$Define ExDEMIXexperimentalOptions}


{$Define TrackDEMboundingBox}


 //deleted 4/19/2023 to track down problems; they are not key options
   {$Define ExSidescan}
   {$Define ExOceanography}
   {$Define ExDriftModel}
   {$Define ExSieve}
   {$Define ExLabDownloads}
   {$Define ExRedistrict}
   //{$Define ExTIN}      //this will take some work to get out of the core program


//{$Define IncludePython}

//{$Define ExFresnel}


{$Define ExWaveLengthHeight}

{$Define ExDRGimport}
{$Define ExGeoPDF}
{$Define ExWMS}
{$Define ExMrSID}
{$Define ExLandsatQA}


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
   {$Define ExDTED}
   {$Define RecordIniMemoryOverwrite}
   {$Define microdem}

   {$Define AllowOddballDEMs}
   {$Define IncludeBILWrite}
   {$Define AllowGeomorphometry}
   {$Define AllowOddballDEMexports}
   {$Define AllowDEMGeomorph}
   {$Define UseFireDacSQLlite}



