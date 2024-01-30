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


{$Define PlainMICRODEM}

//deleted 5/8/2023 in anticipation of USNA website going down
   {$Define ExGeologyDownload}

 //these were deleted 4/19/2023 to track down problems; they are not key options
   {$Define ExSidescan}
   {$Define ExOceanography}
   {$Define ExDriftModel}
   {$Define ExSieve}
   //{$Define ExTIN}      //this will take some work to get out of the core program
   {$Define ExLabDownloads}
   {$Define ExRedistrict}


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

//{$Define BasicOpens}


//{$Define AllowCurvatureStatistics}  //not recommended, and must be renabled here


{$IfDef PlainMICRODEM}
   {$Define RecordTime}
   {$Define ExDTED}
   {$Define  RecordIniMemoryOverwrite}
   {$Define microdem}
   //{$Define AllowV1V2DEMHeaders}   //must be in DEMDefs and DEMCoord
   //{$Define BasicOpens}

   {$Define AllowOddballDEMs}
   //{$Define AllowV1V2V3DEMHeaders}
   {$Define IncludeBILWrite}
   {$Define AllowGeomorphometry}
   {$Define AllowOddballDEMexports}
   {$Define AllowDEMGeomorph}
   {$Define UseFireDacSQLlite}
{$Else}
         //options included
            {$Define Nevadia}
            {$Define Judomia}
            {$Define DragonPlot}
            {$Define HideHelpButtons}
            {$Define RecordProblems}
            {$Define VCL}
            {$Define dBase_DefaultDBs}
            {$Define NoModeChanges}
            {$Define ExGDAL}
            {$Define ExPointCloud}
            {$Define ExLasLidar}
            {$Define ExSatStats}
            {$Define ExDBImages}
            {$Define ExFMX3D}
            {$Define NoDBGrafs}
            {$Define ExStereoNet}
            {$Define ExMrSID}
            {$Define ExTerrainClassify}
            {$Define ExPOTRACE}
            {$Define ExPrincComp}
            {$Define ExMultiGrid}
            {$Define ExcludeExternalTools}
            {$Define NoExternalPrograms}
            {$Define ExTissot}
            {$Define ExAutoOpen}
            {$Define ExOTB}
            {$Define ExSplash}
            {$Define ExRiverNetworks}
            {$Define ExDigitize}
            {$Define ExMGT}
            {$Define ExMovie}
            {$Define ExMake_grid}
            {$Define ExLabs}

            {$Define ExMilIcon}
            {$Define NoTiming}

            {$Define NoDBGrafs}
            {$Define NoDBGraphs}

            {$Define ExDEMReports}
            {$Define ExMICRONET}
            {$Define ExLCP}
            {$Define ExHypImage}
            {$Define ExTools}
            {$Define ExactRecordCountFix}
            {$Define UseTCLientDataSet}
             {$Define ExOpenGL}

            {$Define ExWaveRefraction}

         {$Define ExOSM}
         {$Define ExGeology}
         {$Define NoSatelliteForms}
         {$Define ExPointCloudMemory}
         {$Define NoClustering}
         {$Define ExGeography}
         {$Define ExMICRONET}
         {$Define ExGeology}
         {$Define ExMrSID}
         {$Define ExGeomorphGrids}
         {$Define ExMultiGrid}
         {$Define ExOceanography}

         {$Define ExNaturalEarth}

         {$Define ExMapReports}

         {$Define ExMilIcons}
         {$Define ExViewDEMHeader}
         {$Define ExAdvanced3D}
         {$Define ExVectorOverlay}
         //{$Define ExDTED}
         {$Define ExOddballDEMexports}
         {$Define ExStereoNet}
         {$Define ExProjects}

         {$Define ExAdvancedGIS}
         {$Define ExAdvancedSats}
         {$Define ExAdvancedImportExport}
         {$Define ExAllSubframeImagery}
         {$Define ExAlphaBlending}
         {$Define exaltimeter}
         {$Define ExAmbush}
         {$Define ExAsterLag}
         {$Define ExAVI}
         {$Define ExAVI2GIF}
         {$Define ExBlendMovie}
         {$Define ExBlueMarble}
         {$Define ExCartography}
         {$Define ExCGM}
         {$Define ExCGMDemo}
         {$Define ExComplexGeostats}
         {$Define ExComPort}
         {$Define ExConvert}
         {$Define ExDataManip}
         {$Define ExDownloadData}
         {$Define ExDrainage}
         {$Define ExDriftModel}
         {$Define ExExif}
         {$Define ExExoticMaps}
         {$Define ExFly}
         {$Define ExFourier}
         {$Define ExFresnel}
         {$Define ExFTP}
         {$Define ExGeography}
         {$Define ExGeostats}
         {$Define ExGeosym}
         {$Define ExGMTGridding}
         {$Define ExGPS}
         {$Define ExGridComputing}
         {$Define ExGridOverMap}
         {$Define ExHyperSpectral}
         {$Define ExImageClassify}
         {$Define ExMakeLCGrid}
         {$Define ExMilIcons}
         {$Define ExModis}
         {$Define ExMovies}
         {$Define Exnet}
         {$Define ExOblique}
         {$Define ExOceanography}
         {$Define ExOpenGL}
         {$Define ExPClouder}
         {$Define ExPocketPanorama}
         {$Define ExRasterGIS}
         {$Define ExRedistrict}
         {$Define ExRegisterImage}
         {$Define ExRGBThreeColors}
         {$Define ExSatAdvanced}
         {$Define ExSatImport}
         {$Define exscanner}
         {$Define ExSensorManager}
         {$Define ExSidescan}
         {$Define ExSlicer3D}
         {$Define ExStereo}
         {$Define ExStratcol}
         {$Define ExTIN}
         {$Define ExTernary}
         {$Define ExTextResultsWindow}
         {$Define ExTINGraph}
         {$Define ExTopoGrain}
         {$Define ExTrackSat}
         {$Define ExVariableOpaques}
         {$Define ExVegGrid}
         {$Define ExVegDensity}
         {$Define ExZipatone}
         {$Define NoClustering}
{$EndIf}


