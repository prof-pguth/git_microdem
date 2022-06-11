unit demlos_draw;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordLOSAlgorithm}
      //{$Define RecordPointClouds}
      //{$Define RecordUTMZones}
      {$Define RecordLOS}
      //{$Define RecordLOSPrettyDrawing}
      //{$Define RecordRandomProfiles}
      //{$Define RecordWaveLenghtHeight}
      //{$Define RecordAllLOS}
      //{$Define RecordClosing}
      //{$Define RecordNearestCrest}
      //{$Define RecordThreadCrest}
      //{$Define RecordMGT}
   {$Endif}
{$Endif}

interface


uses
  SysUtils,Classes, Math,System.UITypes,System.Types,  System.UIConsts,

  {$IfDef VCL}
     Graphics,Windows,
  {$EndIf}
  {$IfDef FMX}
     FMX.Graphics,FMX.Types,
  {$EndIf}

//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end for inline of the core DB functions

  {$IFDef ExPointCloudMemory}
  {$Else}
     point_cloud_memory,
  {$EndIf}

  Petmar_types,PetDBUtils,PETMAR,DEMDefs,DEMCoord,PetImage,PetMath,DEMMapDraw;


type
   tLOSCalculation = class
      protected
      private
      public
         ComputePoints : integer;
         constructor Create;
         destructor Destroy;
         function Execute(var UseData : tMyData; DEMonView : tDEMDataSet; LatLeft,LongLeft,LatRight,LongRight,LeftObsUp,RightObsUp : float64; FresnelDB : integer
                {$IfDef ExPointCloudMemory}
                 ) : tLOSResult;
                {$Else}
                ; pc1,pc2 : Point_cloud_memory.tMemoryPointCloud ) :  tLOSResult;
                {$EndIf}

   end;

   tLOSdraw = class
      protected
      private
      public
        LOSVariety : tLOSVariety;
        FormSectLenMeters : float64;
        BaseMapDraw : tMapDraw;
        RidgeLoc,LOSAzimuth,ObsGroundElev,TargetGroundElev,
        ObsGroundElevLAS,TargetGroundElevLAS,
        LatLeft,LongLeft,LatRight,LongRight,FormVertExag,
        FormScaleFac,DropCurve,ZoomLeft,ZoomRight,
        SliceDx,SliceDy,SliceRotate : float64;
        PixLong,PixelsHigh,ProfileBot,StartLOSLeft,StartLOSDown,
        DEMOnView,ProfileDropDown,
        FresnelDB,FirstX,FirstY,MinAreaZ,MaxAreaZ : integer;
        ZoomedDiagram,CompareDensityAlongProfile,ShowSeaLevel,
        EnvelopeDone   : boolean;

        ProfLats,ProfLongs : Petmath.bfarray32;
        ShowProfile : array[1..MaxDEMDataSets] of boolean;
        ProfileName : array[1..MaxDEMDataSets] of ShortString;
        TargetName,SensorName : shortstring;
        MultipleProfilesToShow : tStringList;

        {$IfDef VCL}
           p3 : tBMPMemory;
        {$EndIf}

        {$IFDef ExPointCloudMemory}
        {$Else}
           LOSMemoryPointCloud : tTwoPointClouds;
           PtCldInUse : integer;
        {$EndIf}

        {$IFDef ExVegDensity}
        {$Else}
           VegGridUsed : integer;
        {$EndIf}

         constructor Create;
         destructor Destroy;
         procedure DrawCollar(var Bitmap : tMyBitmap);
         procedure LOSScreenLocation(Dist,Elev : float64; var xp,yp : integer);
         procedure LOSExtremeElevationsAndScreenCoordinates;
         procedure DrawTheProfile(var Bitmap : tMyBitmap; FieldName : ShortString; Color : tColor; Width : integer; SecondName : ShortString = ''; Mult : integer = 1);
         procedure DrawProfile(var bitmap : tMyBitmap);
         procedure RecalculateProfile;
         procedure SetSize(var Bitmap : tMybitmap; x,y : integer);
         procedure CreateProfileBMP(var Bitmap : tMyBitmap);

        {$IfDef ExPointCloudMemory}
        {$Else}
           procedure OverlayProfilePointCloud(var Bitmap : tMyBitmap);
        {$EndIf}
   end;



{$IfDef ExFresnel}
{$Else}
   function LOSComputeOnly(var UseData : tMyData; DEMonView : tDEMDataSet; VegGrid : integer; LatLeft,LongLeft,LatRight,LongRight,LeftObsUp,RightObsUp : float64) :  tLOSResult;
   function FresnelZoneResult(LosResult : tLOSResult) : shortstring;
   function FresnelZoneColor(LosResult : tLOSResult) : tPlatformColor;
{$EndIf}


{$IfDef ExGeostats}
{$Else}
   procedure FindWavelengthStats(var UseData : tMyData; var WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd : float64; ColorFields : boolean = false);
   procedure AddToWaveSpacingHeightResults(var Findings : TStringList; WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd : float64);
{$EndIf}


function CreateLOS(inLOSVariety : tLOSVariety; inBaseMap : tMapDraw; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64) : tMyBitmap;


var
   Symbol : tSymbols15;
   LineColors256 : tPlatformColors256;
   LineSize256 :  tbytes256;


implementation

uses
   {$IfDef ExPointCloud}
   {$Else}
      las_lidar,
   {$EndIf}
   Make_tables,
   DEMDatabase,
   BaseMap,
   DEMDef_Routines;


function CreateLOS(inLOSVariety : tLOSVariety; inBaseMap : tMapDraw; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64) : tMyBitmap;
var
   LOSdraw : tLOSdraw;
begin
   {$IfDef RecordLOS} WriteLineToDebugFile('CreateLOS in'); {$EndIf}
   if DEMGlb[DEMonMap].LatLongDegreeInDEM(Lat1,Long1) and DEMGlb[DEMonMap].LatLongDegreeInDEM(Lat2,Long2) then begin
      LOSDraw := tLOSdraw.Create;
      LOSdraw.LatLeft := Lat1;
      LOSdraw.LongLeft := Long1;
      LOSdraw.LatRight := Lat2;
      LOSdraw.LongRight := Long2;
      LOSdraw.DEMonView := DEMonMap;
      LOSdraw.BaseMapDraw := inBaseMap;
      LOSdraw.LOSVariety := inLOSVariety;
      CreateBitmap(Result,2400,1200);

      {$IfDef FMX}
         Result.Canvas.BeginScene;
         Result.Canvas.Stroke.Color := clBlack;
      {$EndIf}

      LOSdraw.CreateProfileBMP(Result);

      {$IfDef FMX}
         Result.Canvas.EndScene;
      {$EndIf}
      LOSdraw.Destroy;
   end;
   {$IfDef RecordLOS} WriteLineToDebugFile('CreateLOS out'); {$EndIf}
end;



      procedure tLOSdraw.SetSize(var Bitmap : tMybitmap; x,y : integer);
      begin
         if MDDef.LOSLeftTixLabels then StartLOSLeft := 10 + Bitmap.Canvas.TextWidth('10000') else StartLOSLeft := 10;
         StartLOSDown := 25;
         if (LOSVariety = losSimpleMagModel) then PixelsHigh := PixelsHigh div 2;
         ProfileBot := y - 50;
         PixelsHigh := ProfileBot - StartLOSDown;
         PixLong := x - 105 - StartLOSLeft;
      end;



procedure tLOSdraw.CreateProfileBMP(var Bitmap : tMyBitmap);
var
   i : integer;
begin
   {$IfDef RecordLOS} WriteLineToDebugFile('tLOSdraw.CreateProfileBMP in'); {$EndIf}

   {$IfDef VCL}
      LoadMyFontIntoWindowsFont(MDDef.LOSfont,Bitmap.Canvas.Font);
   {$EndIf}

   SetSize(Bitmap,Bitmap.Width,Bitmap.Height);

   LOSExtremeElevationsAndScreenCoordinates;
   DrawCollar(Bitmap);

     if LOSVariety in [losSimpleOne,losVanilla] then begin
       if DEMGlb[DEMonView].LatLongDegreeInDEM(LatLeft,LongLeft) and DEMGlb[DEMonView].LatLongDegreeInDEM(LatRight,LongRight) then begin
           DrawProfile(BitMap);
       end;
     end;

     if (LOSVariety in [losAllDEMs,losAllDEMDropDown]) and (MultipleProfilesToShow = Nil) then begin
        {$IfDef RecordLOS} WriteLineToDebugFile('Set up multiple profiles'); {$EndIf}
        MultipleProfilesToShow := tStringList.Create;
        for i := 1 to MaxDEMDataSets do begin
           if ValidDEM(i) and ShowProfile[i] then begin
              DEMonView := i;
              if DEMGlb[i].LatLongDegreeInDEM(LatLeft,LongLeft) and DEMGlb[i].LatLongDegreeInDEM(LatRight,LongRight) then begin
                  {$IfDef RecordLOS} WriteLineToDebugFile('Recalculate profile, DEM=' + IntToStr(i)); {$EndIf}
                  RecalculateProfile;
                  MultipleProfilesToShow.Add(GISdb[FresnelDB].DBFullName);
                  CloseAndNilNumberedDB(FresnelDB);
              end;
           end;
        end;
     end;

     if (MultipleProfilesToShow <> Nil) then begin
        {$IfDef RecordLOS} WriteLineToDebugFile('(MultipleProfilesToShow <> Nil)  image size: ' + IntToStr(Bitmap.Width) + 'x' + IntToStr(Bitmap.Height)); {$EndIf}
        DrawCollar(Bitmap);
        for i := 1 to MultipleProfilesToShow.Count do begin
           OpenNumberedGISDataBase(FresnelDB,MultipleProfilesToShow.Strings[pred(i)]);
           DrawTheProfile(Bitmap,'ELEV_M',ConvertPlatformColorToTColor(LineColors256[i]),LineSize256[i]);
           CloseAndNilNumberedDB(FresnelDB);
        end;
     end;
     {$IfDef RecordLOS} WriteLineToDebugFile('Left side:  ' + LatLongDegreeToString(LatLeft,LongLeft)); {$EndIf}
end;


{$IFDef ExPointCloudMemory}
{$Else}

procedure tLOSdraw.OverlayProfilePointCloud(var Bitmap : tMyBitmap);
var
   rgbColor : tRGBTriple;
   TStr : shortString;
   MyFont : tMyFont;
   x,y,z,i,xpic,ypic,SymSize,LastYPic : integer;
   HalfThick : float64;
begin
  if (PtCldInUse <> 0) and (LOSMemoryPointCloud[PtCldInUse] <> Nil) then begin
     if (MDdef.ShowPointCloundOnProfile in [spcDensity,spcPoints]) and (not CompareDensityAlongProfile)then begin
        GetMyFontFromWindowsFont(MyFont,Bitmap.Canvas.Font);
        Bitmap.Canvas.Font.Size := 18;
        Bitmap.Canvas.Font.Color := clLime;
        Bitmap.Canvas.Font.Style := [fsBold];
        Bitmap.Canvas.Font.Name := 'Verdana';
        TStr := 'Cloud: ' + LOSMemoryPointCloud[PtCldInUse].CloudName;
        Bitmap.Canvas.TextOut(Bitmap.Width div 2 - Bitmap.Canvas.TextWidth(TStr),10,TStr);
        LoadMyFontIntoWindowsFont(MyFont,Bitmap.Canvas.Font);
     end;

     {$IfDef VCL}
        if (MDdef.ShowPointCloundOnProfile = spcDensity) then begin
          {$IfDef RecordLOS} WriteLineToDebugFile('TDEMLOSF.OverlayProfilePointCloud, sVXDensity'); {$EndIf}
           LOSMemoryPointCloud[PtCldInUse].GetAlongProfileDensity;
           SymSize := 1;
           with LOSMemoryPointCloud[PtCldInUse] do begin
              for x := 0 to MaxXDensity do begin
                 if (x < FormSectLenMeters) then begin
                    LOSScreenLocation(x,AlongProfileGround[x],xpic,LastYpic);
                    for z := 0 to MaxYDensity do begin
                       LOSScreenLocation(x,z + AlongProfileGround[x],xpic,ypic);
                       if (AlongProfileDensity^[x,z] > 0) then begin
                           rgbColor := VegDenstColors[AlongProfileDensity^[x,z]];
                           if (XPic < 32000) then for y := LastYpic downto Ypic do begin
                              p3.SetPixelColorSize(xpic,y,SymSize,rgbColor);
                           end;
                       end;
                       LastYpic := YPic;
                    end;
                 end;
              end;
           end;
        end;

        if (MDdef.ShowPointCloundOnProfile = spcPoints) then begin
          {$IfDef RecordLOS} WriteLineToDebugFile('TDEMLOSF.OverlayProfilePointCloud, sVXPoints'); {$EndIf}
            SymSize := MDDef.CloudSymbol[1].Size;
            with LOSMemoryPointCloud[PtCldInUse] do begin
               rgbColor := ConvertTColorToPlatformColor(clAqua);
               HalfThick := 0.5*MDDef.CloudSliceThick;
               for i := 1 to NumMemPts do begin
                 if (xyPts^[i,1] > -HalfThick) and (xyPts^[i,1] <= HalfThick) and (xyPts^[i,2] > 0) and (xyPts^[i,2] <= FormSectLenMeters) then begin
                     if (MDdef.ls.ColorCoding = lasccReturnNumber) and (PtReturnNumber <> Nil) then rgbColor := Las_ret_colors[PtReturnNumber^[i]]
                     else begin
                        if (PtClass <> Nil) then rgbcolor := Las_rgb_colors[PtClass^[i]];
                     end;
                     LOSScreenLocation(xyPts^[i,2],zPts^[i],xpic,ypic);
                     if (XPic < 32000) then begin
                        p3.SetPixelColorSize(xpic,ypic,SymSize,rgbColor);
                     end;
                  end;
               end;
            end;
        end;
     {$EndIf}

     if MDDef.LOSshowIntervisibilityFromLAS then begin
       {$IfDef RecordLOS} WriteLineToDebugFile('TDEMLOSF.OverlayProfilePointCloud, MDDef.LOSshowIntervisibilityFromLAS'); {$EndIf}
        DrawTheProfile(Bitmap,'MAXZ_PTCLD',-99,MDDef.LOSVisLineWidth,'COLOR_LAS');
     end;
  end;
  {$IfDef RecordLOS} WriteLineToDebugFile('TDEMLOSF.OverlayProfilePointCloud out'); {$EndIf}
end;
{$EndIf}


procedure tLOSdraw.RecalculateProfile;
var
   fName : PathStr;
   LOSCalculation : tLOSCalculation;
begin
    {$IfDef RecordLOS} WriteLineToDebugFile('tLOSdraw.RecalculateProfile in'); {$EndIf}
    if LOSVariety in [losMagModel,losAllDEMs,losAllDEMDropDown,losSimpleMagModel,losSimpleOne] then begin
       CalculatingCurvature := false;
    end;
     {$IfDef LoadLastLOS}
        LastSavedLOSfName := ProjectDir + 'last_los.csv';
        Saveprofileendpoints1Click(nil);
     {$EndIf}

      if ValidDB(FresnelDB) then begin
         {$IfDef RecordLOS} WriteLineToDebugFile('Closing FresnelDB'); {$EndIf}
         CloseAndNilNumberedDB(FresnelDB);
      end;
      {$IfDef VCL}
         fName := NextFileNumber(MDTempDir, DEMGlb[DEMonView].AreaName + '_los_',DefaultDBExt);
      {$Else}
         fName := NextFileNumber(MDTempDir,'topo_los_',DefaultDBExt);
      {$EndIf}

      {$IfDef RecordLOS} WriteLineToDebugFile('Create=' + fName);   {$EndIf}

      MakeFresnelTable(fName,
           {$IfDef ExFresnel}
              false,
           {$Else}
              MDDef.DrawFresnel,
           {$EndIf}
           {$IFDef ExVegDensity}
              false,
           {$Else}
              (DEMGlb[DEMonView].VegGrid[1] <> 0),
           {$EndIf}

           {$IfDef ExPointCloudMemory}
              false,
           {$Else}
              (LOSMemoryPointCloud[1] <> Nil),
           {$EndIf}
           MDDef.DoGrazingFields,
           false,

           {$IFDef ExVegDensity}
              false,
           {$Else}
              (DEMGlb[DEMonView].VegGrid[2] <> 0),
           {$EndIf}

           {$IfDef ExPointCloudMemory}
              false,
           {$Else}
              (LOSMemoryPointCloud[2] <> Nil),
           {$EndIf}
           {$IFDef ExVegDensity}
              false,false );
           {$Else}
              (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil), (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil)  );
           {$EndIf}

      {$IfDef RecordLOS} WriteLineToDebugFile('Open=' + fName); {$EndIf}
      FresnelDB := 0;
      ZeroRecordsAllowed := true;
      OpenNumberedGISDataBase(FresnelDB,fName,false);
      if ValidDB(FresnelDB) then GISdb[FresnelDB].LayerIsOn := false;

      {$IfDef RecordLOS} WriteLineToDebugFile('try LOSCalculation := tLOSCalculation.Create'); {$EndIf}
      LOSCalculation := tLOSCalculation.Create;
      LosCalculation.Execute(GISdb[FresnelDB].MyData,DEMGlb[DEMonView],LatLeft,LongLeft,LatRight,LongRight,MDDef.ObsAboveGround, MDDef.TargetAboveGround,FresnelDB
         {$IfDef ExPointCloudMemory}{$Else},LOSMemoryPointCloud[1],LOSMemoryPointCloud[2] {$EndIf}    );
      LosCalculation.Destroy;

      GISdb[FresnelDB].MyData.First;
      ObsGroundElev := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      GISdb[FresnelDB].MyData.Last;
      TargetGroundElev := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      CalculatingCurvature := true;
    {$IfDef RecordLOS} WriteLineToDebugFile('tLOSdraw.RecalculateProfile out'); {$EndIf}
end;



procedure tLOSdraw.DrawProfile(var bitmap : tMyBitmap);
var
   DownSymbol,j,k,XPic,YPic,Color : integer;
   Dist,Pitch,z  : float64;
   FirstPoint : boolean;
   TStr: ShortString;
   Legend : tStringList;
   Protractor : tMyBitmap;
   {$IfDef ExVegDensity}
   {$Else}
      MyFont : tMyFont;
      Col,Row,i,SymSize,ypic2 : integer;
      Lat,Long,VegHt : float64;
      z2 : float32;
      f1,f2 : ShortString;
   {$EndIf}

      procedure DrawPitchLine(Pitch : float64);
      label
         Done;
      var
         xpic,ypic : integer;
         z,z2 : float64;
      begin
          {$IfDef VCL}
             Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.PitchLineColor);
             Bitmap.Canvas.Pen.Width := MDDef.PitchLineWidth;
          {$EndIf}
          GISdb[FresnelDB].MyData.First;
          FirstPoint := true;
          z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M') + MDDef.ObsAboveGround;
            while not GISdb[FresnelDB].MyData.eof do begin
               Dist := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
               z2 := z + TanDeg(Pitch) * 1000 * Dist;
               LOSScreenLocation(1000*Dist,Z2,xpic,ypic);
               if (yPic >= StartLOSDown + PixelsHigh) or (yPic < 0) then goto Done;
               if (XPic < 32000) then begin
                  {$IfDef VCL}
                     if FirstPoint then begin
                        Bitmap.Canvas.MoveTo(xpic,ypic);
                        FirstPoint := false;
                     end
                     else Bitmap.Canvas.LineTo(xpic,ypic);
                  {$EndIf}
               end;
               GISdb[FresnelDB].MyData.Next;
            end;
          Done:;
      end;

      procedure ShowDenityAlongProfile(Thename : shortstring; TheField : ShortString; Offset : integer = 0);
      begin
          if (TheName <> '') then Legend.Add(theName);
          GISdb[FresnelDB].MyData.First;
          while not GISdb[FresnelDB].MyData.eof do begin
             if (GISdb[FresnelDB].MyData.GetFieldByNameAsString(TheField) <> '') then begin
                Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LOS_HT_M');
                j := GISdb[FresnelDB].MyData.GetFieldByNameAsInteger(TheField);
                LOSScreenLocation(Dist,z,xpic,ypic);
                {$IfDef VCL}
                   if (XPic < 32000) then PetImage.BitmapSymbol(Bitmap,XPic,YPic + Offset,VertLine,2,VegDenstColors[j]);
                {$EndIf}
             end;
             GISdb[FresnelDB].MyData.Next;
          end;
      end;

        procedure ShowBlocksAlongProfile(aField : ShortString; Grid,TopLabel,Offset : integer);
        begin
            {$IFDef ExVegDensity}
            {$Else}
               Legend.Add(DEMGlb[DEMGlb[DEMonView].VegGrid[Grid]].AreaName);
            {$EndIf}
            GISdb[FresnelDB].MyData.ApplyFilter(aField + '=' + QuotedStr('Y'));
            while not GISdb[FresnelDB].MyData.eof do begin
                Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LOS_HT_M');
                LOSScreenLocation(Dist,z,xpic,ypic);
                {$IfDef VCL}
                   if (XPic < 32000) then PetImage.BitmapSymbol(Bitmap,XPic,YPic+Offset,VertLine,2,claLime);
                {$EndIf}
                GISdb[FresnelDB].MyData.Next;
            end;
            GISdb[FresnelDB].MyData.ApplyFilter('');
        end;

begin
  {$IfDef RecordLOS} WriteLineToDebugFile('tLOSdraw.DrawProfile start draw Profile'); {$EndIf}

   with DEMGlb[DEMonView] do begin
     if (FresnelDB = 0) then RecalculateProfile;
     {$IfDef RecordLOS} WriteLineToDebugFile('tLOSdraw.DrawProfile Profile recalculated'); {$EndIf}
     {$IfDef VCL}
        p3 := tBMPMemory.Create(Bitmap);
     {$EndIf}
     DrawCollar(Bitmap);
     if ValidDB(FresnelDB) then GISdb[FresnelDB].EmpSource.Enabled := false;

   {$IfDef ExFresnel}
   {$Else}
     if (not (LOSVariety in [losSimpleOne])) and MDDef.DrawFresnel then begin
        DrawTheProfile(Bitmap,'LOS_HT_M', ConvertPlatformColorToTColor(MDDef.FresnelZone1Color), MDDef.FresnelZone1Width,'FRESNEL1_M',1);
        DrawTheProfile(Bitmap,'LOS_HT_M', ConvertPlatformColorToTColor(MDDef.FresnelZone1Color), MDDef.FresnelZone1Width,'FRESNEL1_M',-1);
        DrawTheProfile(Bitmap,'LOS_HT_M', ConvertPlatformColorToTColor(MDDef.FresnelZone2Color), MDDef.FresnelZone2Width,'FRESNEL2_M',1);
        DrawTheProfile(Bitmap,'LOS_HT_M', ConvertPlatformColorToTColor(MDDef.FresnelZone2Color), MDDef.FresnelZone2Width,'FRESNEL2_M',-1);
     end;
     {$EndIf}

     if (not (LOSVariety in [losSimpleOne])) and  MDDef.ShowMaskedAirspace then DrawTheProfile(Bitmap,'MASK_AIR', ConvertPlatformColorToTColor(MDDef.MaskedAirspaceColor), MDDef.MaskedAirspaceWidth);

     if (not (LOSVariety in [losSimpleOne])) and MDDef.LOSVisible then begin
        if MDDef.LOSVisShowLine then begin
           DrawTheProfile(Bitmap,'ELEV_M',-99,MDDef.LOSVisLineWidth,'COLOR');
        end
        else begin
            GISdb[FresnelDB].MyData.First;
            while not GISdb[FresnelDB].MyData.eof do begin
               GISdb[FresnelDB].MyData.Edit;
               Dist := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
               z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
               Color := GISdb[FresnelDB].MyData.GetFieldByNameAsInteger('COLOR');
               LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
               {$IfDef VCL}
               if (XPic < 32000) then begin
                  p3.SetPixelColorSize(xpic,ypic,3,ConvertTColorToPlatformColor(Color));
               end;
               {$EndIf}
               GISdb[FresnelDB].MyData.Next;
            end;
        end;
     end
     else begin
        if MDDef.ShowDEMTerrainProfile then DrawTheProfile(Bitmap,'ELEV_M', ConvertPlatformColorToTColor(MDDef.TerrainProfileColor), MDDef.TerrainProfileWidth);
     end;

     {$IfDef ExVegDensity}
     {$Else}
         OverlayProfilePointCloud(Bitmap);

         if MDDef.VegEffectsVoxels and (BaseMapDraw.VegDensityLayerInUse <> 0) and (DEMGlb[DEMonView].VegDensityLayers[BaseMapDraw.VegDensityLayerInUse]<> Nil) then begin
            if not CompareDensityAlongProfile then begin
                TStr := 'Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[BaseMapDraw.VegDensityLayerInUse].VegDensityName;
                GetMyFontFromWindowsFont(MyFont,Bitmap.Canvas.Font);
                Bitmap.Canvas.Font.Size := 18;
                Bitmap.Canvas.Font.Color := clLime;
                Bitmap.Canvas.Font.Style := [fsBold];
                Bitmap.Canvas.Font.Name := 'Verdana';
                Bitmap.Canvas.TextOut(Bitmap.Width div 2 - Bitmap.Canvas.TextWidth(TStr),10,TStr);
                LoadMyFontIntoWindowsFont(MyFont,Bitmap.Canvas.Font);
            end;

             Bitmap.Canvas.Pen.Width := 2;
             SymSize := 1;
             GISdb[FresnelDB].MyData.First;
             while not GISdb[FresnelDB].MyData.eof do begin
                 Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                 Lat := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LAT');
                 Long := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LONG');
                 z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');

                 DEMGlb[DEMonView].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row);
                 for i := 1 to MaxVegLayers do with DEMGlb[DEMonView] do begin
                    if (VegDensityLayers[BaseMapDraw.VegDensityLayerInUse].VegLayers[i] <> 0) and DEMGlb[VegDensityLayers[BaseMapDraw.VegDensityLayerInUse].VegLayers[i]].GetElevMeters(Col,Row,z2) then begin
                       LOSScreenLocation(Dist,Z + pred(i),xpic,ypic);
                       LOSScreenLocation(Dist,Z+i,xpic,ypic2);
                       Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(VegDenstColors[round(z2)]);
                       Bitmap.Canvas.MoveTo(xpic,ypic);
                       Bitmap.Canvas.LineTo(xpic,ypic2);
                    end;
                 end {for i};
                 GISdb[FresnelDB].MyData.Next;
             end {while};
         end;

          if MDDef.ShowCloudDensity and ValidDB(FresnelDB) and (LOSMemoryPointCloud[PtCldInUse] <> Nil) then begin
             if PtCldInUse = 1 then ShowDenityAlongProfile('','PTS_AROUND');
             if PtCldInUse = 2 then ShowDenityAlongProfile('','PTS2_AROUND');
          end;
          if MDDef.LOSShowVoxelDensity and ValidDB(FresnelDB) then begin
             if (BaseMapDraw.VegDensityLayerInUse = 1) and (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then ShowDenityAlongProfile('','VPTS_AROUND');
             if (BaseMapDraw.VegDensityLayerInUse = 2) and (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then ShowDenityAlongProfile('','VPT2_AROUND');
          end;
      {$EndIf}

      if CompareDensityAlongProfile then begin
         Legend := TStringList.Create;

         {$IfDef ExVegDensity}
         {$Else}
             if (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then begin
                ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[2].VegDensityName,'VPT2_ABOVE',-32);
             end;
             if (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then begin
                ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[1].VegDensityName,'VPTS_ABOVE',-24);
             end;
         {$EndIf}

        {$IfDef ExPointCloudMemory}
        {$Else}
             if (LOSMemoryPointCloud[2] <> Nil) then begin
                ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[2].CloudName,'PTS2_ABOVE',-16);
             end;
             if (LOSMemoryPointCloud[1] <> Nil) then begin
                ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[1].CloudName,'PTS_ABOVE',-8);
             end;
         {$EndIf}

         {$IFDef ExVegDensity}
         DownSymbol := 0;
         {$Else}
         if (VegGrid[1] > 0) then ShowBlocksAlongProfile('BLOCK_VEG',1,30,0);
         if (VegGrid[2] > 0) then begin
            ShowBlocksAlongProfile('BLOCK_VEG2',2,45,8);
            DownSymbol := 8;
         end
         else begin
            DownSymbol := 0;
         end;
         {$EndIf}

        {$IfDef ExPointCloudMemory}
        {$Else}
             if (LOSMemoryPointCloud[1] <> Nil) then begin
                ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[1].CloudName,'PTS_AROUND',8 + DownSymbol);
             end;
             if (LOSMemoryPointCloud[2] <> Nil) then begin
                ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[2].CloudName,'PTS2_AROUND',16 + DownSymbol);
             end;
         {$EndIf}

         {$IfDef ExVegDensity}
         {$Else}
             if (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then begin
                ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[1].VegDensityName,'VPTS_AROUND',24 + DownSymbol);
             end;
             if (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then begin
                ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[2].VegDensityName,'VPT2_AROUND',32 + DownSymbol);
             end;
         {$EndIf}

         for k := 0 to pred(Legend.Count) do begin
            BitmapTextOut(Bitmap,Bitmap.Width div 2 - 40,10 + k * 15,Legend.Strings[k]);
         end;
         Legend.Free;
      end;

       {$IfDef ExVegGrid}
       {$Else}
           if (VegGrid[1] > 0) and MDDef.ShowGridVegEffects then begin
             if VegGridUsed = 1 then begin
               f1 := 'VEG_HT';
               f2 := 'BLOCK_VEG';
             end
             else begin
               f1 := 'VEG2_HT';
               f2 := 'BLOCK_VEG2';
             end;

             Bitmap.Canvas.Pen.Color := clGreen;
             Bitmap.Canvas.Pen.Width := 1;
             GISdb[FresnelDB].MyData.ApplyFilter(f1 + ' > 0.001');
             while not GISdb[FresnelDB].MyData.eof do begin
                Dist := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
                VegHt := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat(f1);
                LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
                if (XPic < 32000) then begin
                   Bitmap.Canvas.MoveTo(xpic,ypic);
                   LOSScreenLocation(1000 * Dist,Z + VegHt,xpic,ypic);
                   Bitmap.Canvas.LineTo(xpic,ypic);
                end;
                GISdb[FresnelDB].MyData.Next;
             end;

             Bitmap.Canvas.Pen.Color := clLime;
             GISdb[FresnelDB].MyData.ApplyFilter(f2 +  '=' + QuotedStr('Y'));
             while not GISdb[FresnelDB].MyData.eof do begin
                Dist := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
                VegHt := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat(f1);
                LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
                if (XPic < 32000) then begin
                   Bitmap.Canvas.MoveTo(xpic,ypic);
                   LOSScreenLocation(1000 * Dist,Z + VegHt,xpic,ypic);
                   Bitmap.Canvas.LineTo(xpic,ypic);
                end;
                GISdb[FresnelDB].MyData.Next;
             end;
             GISdb[FresnelDB].MyData.ApplyFilter('');
          end {if veg};
      {$EndIf}

      {$IfDef ExGeostats}
      {$Else}
          if ValidDB(FresnelDB) and MDDef.PlotCrest and MDDef.ForceCrestComputations then begin
             GISdb[FresnelDB].MyData.First;
             while not GISdb[FresnelDB].MyData.eof do begin
                Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
                LOSScreenLocation(Dist,z,xpic,ypic);
                if (XPic < 32000) then begin
                   if GISdb[FresnelDB].MyData.GetFieldByNameAsString('PEAK') = 'Y' then begin
                     {$IfDef VCL}
                     PetImage.BitmapSymbol(Bitmap,XPic,YPic,FilledBox,3,claLime);
                     {$EndIf}
                   end
                   else if GISdb[FresnelDB].MyData.GetFieldByNameAsString('PIT') = 'Y' then begin
                     {$IfDef VCL}
                     PetImage.BitmapSymbol(Bitmap,XPic,YPic,FilledBox,3,claRed);
                     {$EndIf}
                   end;
                end;
                GISdb[FresnelDB].MyData.Next;
             end;
          end;
      {$EndIf}

      if ElevationDEM and (Not (LOSVariety in [losAllDEMs,losAllDEMDropDown,losSimpleOne])) then begin
         {$IfDef VCL}
           if MDDef.DrawLOS then begin    {Draw line of sight}
              Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.LosConnectionColor);
              Bitmap.Canvas.Pen.Width := MDDef.LosConnectionWidth;
              LOSScreenLocation(0,ObsGroundElev + MDDef.ObsAboveGround,xpic,ypic);
              Bitmap.Canvas.MoveTo(xpic,ypic);
              LOSScreenLocation(FormSectLenMeters,TargetGroundElev +  MDDef.TargetAboveGround,xpic,ypic);
              Bitmap.Canvas.LineTo(xpic,ypic);
           end {if};
       {$EndIf}

         if MDDef.LOSShowPitch then begin
            DrawPitchLine(MDDef.LOSMinPitch);
            DrawPitchLine(MDDef.LOSMaxPitch);
         end;
      end {if};

      if MDDef.DrawLOSProtractor then begin
         {$IfDef VCL}
            Protractor := CreateProtractor(false,false,FormVertExag);
            Bitmap.Canvas.Draw(30,30,Protractor);
            Protractor.Free;
         {$EndIf}
      end;

      if (not (LOSVariety in [losSimpleOne])) and MDDef.LOSShowPitch then begin
         Pitch := arcTan( -((ObsGroundElev +   MDDef.ObsAboveGround) - (TargetGroundElev +  MDDef.TargetAboveGround - Dropcurve)) / FormSectLenMeters) / DegToRad;
         TStr := '  Pitch= ' + RealToString(Pitch,-8,-2) + DegSym;
      end
      else TStr := '';
   end;
end {proc DrawProfile};




procedure tLOSdraw.DrawTheProfile(var Bitmap : tMyBitmap; FieldName : ShortString; Color : tColor; Width : integer; SecondName : ShortString = ''; Mult : integer = 1);
var
   z,Dist: float64;
   FirstPoint : boolean;
   xpic,ypic,xp,yp : integer;
   ColorFromDB : boolean;
begin
   GISdb[FresnelDB].MyData.First;
   FirstPoint := true;
   ColorFromDB := (Color < 0);

   {$IfDef VCL}
      if Color >= 0 then Bitmap.Canvas.Pen.Color := Color;
      Bitmap.Canvas.Pen.Width := Width;
   {$EndIf}

    {$IfDef FMX}
       Bitmap.Canvas.Stroke.Color := ConvertTColorToPlatformColor(Color);
       Bitmap.Canvas.Stroke.Thickness := Width;
    {$EndIf}

   while not GISdb[FresnelDB].MyData.eof do begin
      Dist := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
      if GISdb[FresnelDB].MyData.CarefullyGetFieldByNameAsFloat64(FieldName,Z) then begin
         if (SecondName <> '') and (not ColorFromDB) then begin
            z := z + Mult * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat(SecondName);
         end;
         LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
         if (XPic < 32000) then begin
            if FirstPoint then begin
               FirstPoint := false;
            end
            else begin
               if (ColorFromDB) then begin
                  {$IfDef VCL}
                  Bitmap.Canvas.Pen.Color := GISdb[FresnelDB].MyData.GetFieldByNameAsInteger(SecondName);
                  {$EndIf}
                  {$IfDef FMX}
                  Bitmap.Canvas.Stroke.Color := ConvertTColorToPlatformColor(GISdb[FresnelDB].MyData.GetFieldByNameAsInteger(SecondName));
                  {$EndIf}
               end;
               PetImage.DrawLine(Bitmap,xpic,ypic,xp,yp);
            end;
            xp := xpic;
            yp := ypic;
         end;
      end
      else FirstPoint := true;
      GISdb[FresnelDB].MyData.Next;
   end;
end;


procedure tLOSdraw.LOSExtremeElevationsAndScreenCoordinates;
var
   PointElev : float32;
   j : integer;
   dists : ^Petmath.bfarray32;

   procedure OneProfile;
   var
      i : integer;
   begin
       {$IfDef RecordLOS} WriteLineToDebugFile('OneProfile, DEM=' + IntToStr(DEMonView)); {$EndIf}
       DEMGlb[DEMonView].GetStraightRouteLatLongDegree(LatLeft,LongLeft,LatRight,LongRight,MDDef.wf.StraightAlgorithm,PixLong,ProfLats,ProfLongs,dists^);
       {$IfDef RecordLOS} WriteLineToDebugFile('OneProfile, start loop'); {$EndIf}
       For i := 0 to PixLong do begin
          if DEMGlb[DEMonView].GetElevFromLatLongDegree(ProfLats[i],ProfLongs[i],PointElev) then begin
             {$IfDef RecordAllLOS}
                if (i mod 50 = 0) or (i = PixLong) then begin
                   WriteLineToDebugFile( IntToStr(i) + '  ' + LatLongDegreeToString(ProfLats[i],ProfLongs[i]) + RealToString(PointElev,12,1));
                end;
             {$EndIf}
             Petmath.CompareValueToExtremes(round(PointElev),MinAreaZ,MaxAreaZ);
          end;
       end;
       {$IfDef RecordLOS} WriteLineToDebugFile('Done OneProfile, DEM=' + IntToStr(DEMonView)); {$EndIf}
   end;

begin
   {$IfDef RecordLOS} WriteLineToDebugFile('TDEMLOSF.LOSExtremeElevationsAndScreenCoordinates,  DEM=' + IntToStr(DEMonView));   {$EndIf}
   VincentyCalculateDistanceBearing(LatLeft,LongLeft,LatRight,LongRight,FormSectLenMeters,LOSAzimuth);
   if (FormSectLenMeters > 25000) then MDDef.LOSHorizTickSpacing_KM := true;
   new(Dists);

     if (LOSVariety in [losAllDEMs,losAllDEMDropDown]) and (MultipleProfilesToShow = Nil) then begin
         if (MinAreaZ > 32000) then begin
            for j := 1 to MaxDEMDataSets do if ValidDEM(j) and ShowProfile[j] then begin
               {$IfDef RecordLOS} WriteLineToDebugFile('Get elevations, DEM=' + IntToStr(j)); {$EndIf}
               DEMonView := j;
               OneProfile;
            end;
            if LOSVariety in [losAllDEMDropDown] then MinAreaZ := MinAreaZ - pred(NumDEMDataSetsOpen) * MDDef.DropDownPerProfile;
         end;
     end
     else begin
        OneProfile;
     end;

   MinAreaZ := MinAreaZ - 1;
   MaxAreaZ := MaxAreaZ + 1;
   Dropcurve := DropEarthCurve(FormSectLenMeters);
   FormScaleFac := 1.0 * (MaxAreaZ - MinAreaZ + Dropcurve) / PixelsHigh;
   FormVertExag := (FormSectLenMeters /PixLong)/ FormScaleFac;
   {$IfDef RecordLOS}
      WriteLineToDebugFile('PixLong=' + IntToStr(PixLong) + '  Dropcurve=' + RealToString(Dropcurve,-12,-2));
      WriteLineToDebugFile('FormVertExag=' + RealToString(FormVertExag,-12,-2) + ' FormScaleFac=' + RealToString(FormScaleFac,-12,-2 )+ ' FormSectLenMeters=' + RealToString(FormSectLenMeters,-12,-2));
   {$EndIf}
   Dispose(Dists);
end;


procedure tLOSdraw.LOSScreenLocation(Dist,Elev : float64; var xp,yp : integer);
begin
   if ZoomedDiagram then begin
      if (Dist >= ZoomLeft) and (Dist <= ZoomRight) then xp := StartLOSLeft + round( (Dist - ZoomLeft) / (ZoomRight-ZoomLeft) * PixLong)
      else begin
         xp := MaxSmallInt;
         yp := MaxSmallInt;
         exit;
      end;
   end
   else xp := StartLOSLeft + round(Dist / FormSectLenMeters * PixLong);
   yp := StartLOSDown + Round( (MaxAreaZ - Elev + DropEarthCurve(Dist)) / FormScaleFac);
end;


procedure tLOSdraw.DrawCollar(var Bitmap : tMyBitmap);
var
   x,TickElev,LastXP,TickInt,xp,yp : integer;
   i,j,Long,Factor : float64;
   TStr,TStr2 : shortString;
   First : boolean;
begin
   {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile('TDEMLOSF.DrawCollar, bitmap size= ' + IntToStr(Bitmap.Width) + 'x' + IntToStr(Bitmap.Height)); {$EndIf}

   if MDdef.ElevDisplayMeters then Factor := 1
   else Factor := 1 / FeetToMeters;

   TickInt := round(GetTickInt(PixelsHigh,round((MaxAreaZ-MinAreaZ) * Factor),Bitmap.Canvas.TextHeight('089')));
   if (TickInt = 0) then TickInt := 1;

   {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile('Tick Elev range: ' + IntToStr(MinAreaZ) + ' to ' + IntToStr(MaxAreaZ) + '  TickInt=' + IntToStr(TickInt)); {$EndIf}

   if not ZoomedDiagram then begin
      ZoomRight := FormSectLenMeters;
      ZoomLeft := 0;
   end;
   Long := ZoomRight - ZoomLeft;

   if MDDef.EnglishDistanceUnits = disEnglish then begin
      Long := Long * 0.001 * Km2Miles;
   end
   else if MDDef.EnglishDistanceUnits = disNautical then begin
      Long := Long * 0.001 * Km2NauticalMiles;
   end
   else begin
      if MDDef.LOSHorizTickSpacing_KM then begin
         if (Long > 2500) then Long := Long * 0.001
         else MDDef.LOSHorizTickSpacing_KM := false;
      end;
   end;

   {$IfDef VCL}
   Bitmap.Canvas.Pen.Color := clBlack;
   Bitmap.Canvas.Pen.Width := MDDef.FrameLineWidth;
   {$EndIf}

   TickElev := round(MaxAreaZ * Factor) - (round(MaxAreaZ * Factor) mod TickInt);
   First := true;
   while (TickElev >= MinAreaZ * Factor) do begin
      {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile('z=' + IntToStr(TickElev)); {$EndIf}
      for x := 0 to PixLong do begin
        LOSScreenLocation((FormSectLenMeters * x/PixLong),TickElev,xp,yp);
        if MDDef.ProfileShowZLevel and (x mod 3 in [0,1]) then begin
            {$IfDef VCL}
            Bitmap.Canvas.Pixels[xp,yp] := clTeal;
            {$EndIf}
         end;
         if MDDef.ProfileShowSeaLevel and (TickElev = 0) then begin
            {$IfDef VCL}
            Bitmap.Canvas.Pixels[xp,yp] := clBlue;
            {$EndIf}
         end;
      end;

     {draw ticks on left side profile}
      LOSScreenLocation(ZoomLeft,TickElev,xp,yp);
      PetImage.DrawLine(Bitmap,0,YP,StartLOSLeft,YP);
      {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile(' left=' + IntToStr(xp) + '/' + IntToStr(Yp)); {$EndIf}

      {right side}
      LOSScreenLocation(ZoomRight,TickElev,xp,yp);
      PetImage.DrawLine(Bitmap,xp,YP,xp+10,YP);
      {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile(' right=' + IntToStr(xp) + '/' + IntToStr(Yp)); {$EndIf}

      {label right side ticks}
       if not First then begin
          {$IfDef VCL}
          yp := Yp-Bitmap.Canvas.TextHeight('8') div 2;
          {$EndIf}
          BitmapTextOut(Bitmap,xp+15,yp,IntegerToString(TickElev,4));
          if MDDef.LOSLeftTixLabels then BitmapTextOut(Bitmap,0,yp,IntegerToString(TickElev,4));
       end;
      First := false;
      dec(TickElev,TickInt);
   end {while};

   if DEMGlb[DEMOnView].ElevationDEM then begin
      if MDdef.ElevDisplayMeters then TStr := 'meters'
      else TStr := 'feet';
   end
   else TStr := LabelElevUnits[DEMGlb[DEMOnView].DEMheader.ElevUnits];

   BitmapTextOut(Bitmap,StartLOSLeft + PixLong + 20,StartLOSDown-20,TStr);

   if MDDef.ForceLOSHorizTickSpacing then j := MDDef.LosHorizTick
   else begin
      if Long < 0.25 then j := 0.01
      else if Long < 0.75 then j := 0.02
      else if Long < 1.5 then j := 0.05
      else if Long < 2.5 then j := 0.1
      else if Long < 5 then j := 0.25
      else if Long < 25 then j := 1
      else if Long <   50.0 then j := 2
      else if Long <  100.0 then j := 5
      else if Long <  250.0 then j := 10
      else if Long <  500.0 then j := 50
      else if Long < 1000.0 then j := 100
      else if Long < 2500.0 then j := 250
      else j := 500;
   end;

   LastXP := -999;

   if ZoomedDiagram then i := ZoomLeft
   else i := 0;
   repeat
      LOSScreenLocation(i,0,xp,yp);
      PetImage.DrawLine(Bitmap,XP,ProfileBot,XP,ProfileBot+8);
      if MDDef.EnglishDistanceUnits  = disEnglish then  TStr := RealToString(0.001 * i * Km2Miles,-8,-2)
      else if MDDef.EnglishDistanceUnits = disNautical then TStr := RealToString(0.001 * i * Km2NauticalMiles,-8,-2)
      else if MDDef.LOSHorizTickSpacing_KM then TStr := RealToString(0.001 * i,-8,-2)
      else TStr := RealToString(i,-8,-2);

      {$IfDef VCL}
      XP := xp - Bitmap.Canvas.TextWidth(TStr) div 2;
      {$EndIf}
      if xp < 2 then xp := 2;

      if MDDef.EnglishDistanceUnits  = disEnglish then i := i + 1000 * j / Km2Miles
      else if MDDef.EnglishDistanceUnits  = disNautical then i := i + 1000 * j / Km2NauticalMiles
      else if MDDef.LOSHorizTickSpacing_KM then i := i + 1000 * j
      else i := i + j;

      if (i > ZoomRight) then begin
         if MDDef.EnglishDistanceUnits = disEnglish then TStr2 := ' miles'
         else if MDDef.EnglishDistanceUnits = disNautical then TStr2 := ' nm'
         else if MDDef.LOSHorizTickSpacing_KM then TStr2 := ' km'
         else TStr2 := ' m';
      end
      else TStr2 := '';

      if (XP < LastXP) then TStr := '';

      {$IfDef FMX}
      Bitmap.canvas.Stroke.Kind := TBrushKind.Solid;
      Bitmap.canvas.Stroke.Thickness := 1;
      bitmap.Canvas.Fill.Color := TAlphaColors.Black;
      {$EndIf}

      BitmapTextOut(Bitmap,XP+3,ProfileBot + 10,TStr + TStr2);

      if (XP > LastXP) then begin
         {$IfDef VCL}
         LastXP := XP + Bitmap.Canvas.TextWidth(TStr) + 8;
         {$EndIf}
      end;
   until (i > ZoomRight);

    {Draw Left side of profile}
    LOSScreenLocation(ZoomLeft,ObsGroundElev,xp,yp);
    PetImage.DrawLine(Bitmap,StartLOSLeft,ProfileBot,XP,YP);
    LOSScreenLocation(ZoomRight,TargetGroundElev,xp,yp);
    PetImage.DrawLine(Bitmap,XP,ProfileBot,XP,YP);     //right side
    PetImage.DrawLine(Bitmap,XP,ProfileBot,StartLOSLeft,ProfileBot);     //bottom of profile

    if MDDef.LabelMultipleProf then begin
        TStr := BaseMapDraw.PrimMapProj.PreferLocationString(LatLeft,LongLeft);
        BitmapTextOut(Bitmap,0,0,TStr + ' ' + SensorName);
        TStr := BaseMapDraw.PrimMapProj.PreferLocationString(LatRight,LongRight);
        TStr := TStr + ' ' + TargetName;
        BitmapTextOut(Bitmap,PixLong - round(Bitmap.Canvas.TextWidth(TStr)),0,TStr);
    end;
   {$IfDef RecordLOSPrettyDrawing}  Bitmap.SaveToFile(Petmar.NextFileNumber(MDTempDir, 'map_collar_','.bmp') ); {$EndIf}
end {proc DrawCollor};


{$IfDef ExFresnel}
{$Else}

function FresnelZoneResult(LosResult : tLOSResult) : shortstring;
begin
   case LOSResult of
      losIsVisible : Result := 'Visible';
      losBlockByTerrain : Result := 'Blocked by terrain';
      losEdgeFresnelIntrusion : Result := 'Near anteanna Fresnel blockage';
      losCenterFresnelIntrusion : Result := 'Center link Fresnel blockage';
   end;
end;


function FresnelZoneColor(LosResult : tLOSResult) : tPlatformColor;
begin
   case LOSResult of
      losIsVisible : Result := claGreen;
      losBlockByTerrain : Result := claRed;
      losEdgeFresnelIntrusion : Result := claMaroon;
      losCenterFresnelIntrusion : Result := claPurple;
   end;
end;


function LOSComputeOnly(var UseData : tMyData; DEMonView : tDEMDataSet; VegGrid : integer; LatLeft,LongLeft,LatRight,LongRight,LeftObsUp,RightObsUp : float64) :  tLOSResult;
var
   LOSCalculation : tLOSCalculation;
   fName : PathStr;
begin
  {$IfDef RecordLOS} WriteLineToDebugFile('enter LOSComputeOnly'); {$EndIf}
   if (DEMonView = Nil) then exit;
      fName := Petmar.NextFileNumber(MDTempDir, 'radio_los_',DefaultDBExt);
      MakeFresnelTable(fName,true,(VegGrid <> 0),false,MDDef.DoGrazingFields,false,false,false,false,false);
      UseData := tMyData.Create(fName);

   LOSCalculation := tLOSCalculation.Create;
   Result := LosCalculation.Execute(UseData,DEMonView,LatLeft,LongLeft,LatRight,LongRight,LeftObsUp,RightObsUp,0
       {$IfDef ExPointCloudMemory}
       );
       {$Else}
       ,Nil,Nil);
       {$EndIf}
   LosCalculation.Destroy;
  {$IfDef RecordLOS} WriteLineToDebugFile('exit LOSComputeOnly'); {$EndIf}
end;

{$EndIf}



{ tLOSCalculation }

constructor tLOSCalculation.Create;
begin
   inherited;
end;

destructor tLOSCalculation.Destroy;
begin
   inherited;
end;


function tLOSCalculation.Execute(var UseData : tMyData; DEMonView : tDEMDataSet; LatLeft,LongLeft,LatRight,LongRight,LeftObsUp,RightObsUp : float64; FresnelDB : integer
     {$IfDef ExPointCloudMemory}
     ) :  tLOSResult;
     {$Else}
     ;pc1,pc2 : Point_cloud_memory.tMemoryPointCloud) :  tLOSResult;
     {$EndIf}
label
   ExitNow;
type
   tBooleans = array[0..MaxScreenXMax] of boolean;
var
   i,j,VegLayer,NumPts,NumPts2 : integer;
   LocalPitch,EarthCurve,KM_short_end,
   n1,n2,n3,MinVal,MaxVal,IntrudePC,
   l1,l2,l3,Graz2,Slope2,
   Pitch,Lat,Long,Fresnel1,Fresnel2,LOSHt,
   ObsElev,ztarget,lz,LOSLen,LOSAzimuth,
   DropCurve : float64;
   VegHt : float32;
   Color : tPlatformColor;
   IsPeak,IsPit,VisPoints : tBooleans;
   ch : char;
   NeedToCheckPointCloud : boolean;
   NeedZ,xgrids,ygrids,dists,elevs : ^Petmath.bfarray32;
   SlopeAspectRec : tSlopeAspectRec;


    procedure RemoveNeighbors(var IsWhat : tBooleans);
    var
       j,k,l,m : integer;
    begin
      j := 0;
      while (j < ComputePoints) do begin
         if IsWhat[j] then begin
            k := j;
            while IsWhat[k] and (K < ComputePoints)  do inc(k);
            if (k > j) then begin
               if Odd(k-j) then begin
                   l := j + (k-j) div 2 + round(Random(1));
               end
               else l := j + (k-j) div 2;
               for m := j to k do if (m <> l) then IsWhat[m] := false;
            end;
            inc(j,succ(k-j));
         end
         else inc(j);
      end;
    end;

         {$IFDef ExVegDensity}
         {$Else}
         procedure PointFromDensityLayer(VegDensLayer,j : integer;  f1,f2 : ShortString);
         var
            k,DEM,xg,yg : integer;
            z : float32;
         begin
             VegLayer := round(LOSHt - elevs^[j]);
             if (VegLayer > 0) and (VegLayer < DEMonView.VegDensityLayers[VegDensLayer].LayersPresent) then begin
                xg := round(xgrids^[j]);
                yg := round(ygrids^[j]);
                if (DEMonView.VegDensityLayers[VegDensLayer].VegLayers[VegLayer] <> 0) and DEMGlb[DEMonView.VegDensityLayers[VegDensLayer].VegLayers[VegLayer]].GetElevMeters(xg,yg,z) then
                        UseData.SetFieldByNameAsInteger(f1,round(z));
                NumPts := 0;
                for k := succ(VegLayer) to DEMonView.VegDensityLayers[VegDensLayer].LayersPresent do begin
                   DEM := DEMonView.VegDensityLayers[VegDensLayer].VegLayers[k];
                   if (DEM <> 0) and DEMGlb[DEM].GetElevMeters(xg,yg,z) then
                      NumPts := NumPts + round(z);
                end;
                if (NumPts > 0) then UseData.SetFieldByNameAsInteger(f2,NumPts);
             end;
         end {if};
         {$EndIf}


begin
   {$IfDef RecordLOS} WriteLineToDebugFile('tLOSCalculation.Execute in, DEM average space=' + RealToString(DEMonView.AverageSpace,-18,2)); {$EndIf}

     new(xgrids);
     new(ygrids);
     new(dists);
     new(elevs);
     if MDDef.ShowMaskedAirspace then New(NeedZ);
     {$IfDef RecordLOS} WriteLineToDebugFile('start GetStraightRouteLatLongDegree'); {$EndIf}

     VincentyCalculateDistanceBearing(LatLeft,LongLeft,LatRight,LongRight,LOSLen,LOSAzimuth);
     ComputePoints := round(2 * LOSLen / DEMonView.AverageSpace);
     {$IfDef RecordLOS} WriteLineToDebugFile('ComputePoints=' + IntToStr(ComputePoints)); {$EndIf}
     DEMonView.GetStraightRoute(false,LatLeft,LongLeft,LatRight,LongRight,MDDef.wf.StraightAlgorithm,ComputePoints,xgrids^,ygrids^,dists^);
     DropCurve := DropEarthCurve(LOSLen);
     {$IfDef RecordLOS} WriteLineToDebugFile('end GetStraightRouteLatLongDegree'); {$EndIf}

     DEMonView.GetVisiblePoints(LeftObsUp,RightObsUp,-89,89,true,true,ComputePoints,xgrids^,ygrids^,dists^,elevs^,VisPoints);
     if MDDef.ShowMaskedAirspace then DEMonView.LatLongDegreePointsRequiredAntenna(ComputePoints,LatLeft,LongLeft,MDDef.ObsAboveGround,LatRight,LongRight,xgrids^,ygrids^,dists^,NeedZ^);
     {$IfDef RecordLOS} WriteLineToDebugFile('end GetVisiblePoints'); {$EndIf}

      ObsElev := elevs^[0] + LeftObsUp;
      zTarget := elevs^[ComputePoints] + RightObsUp;
      Pitch := arcTan( -(ObsElev - (zTarget - DropCurve)) / LOSLen) / DegToRad;

      if (UseData = Nil) and (not VisPoints[ComputePoints]) then begin
         Result := losBlockByTerrain;
         Goto ExitNow;
      end;

      for j := 0 to ComputePoints do begin
         UseData.Insert;
         EarthCurve := DropEarthCurve(dists^[j]);
         UseData.CarefullySetFloat('CURV_M',EarthCurve,0.01);
         LOSHt := ObsElev + TanDeg(Pitch) * dists^[j];
         DEMonView.DEMGridToLatLongDegree(xgrids^[j],ygrids^[j],Lat,Long);

         UseData.SetFieldByNameAsFloat('LOS_HT_M',LOSHt);
         UseData.SetFieldByNameAsFloat('LAT',Lat);
         UseData.SetFieldByNameAsFloat('LONG',Long);
         if (elevs^[j] < 32000) then begin
            UseData.SetFieldByNameAsFloat('ELEV_M',elevs^[j]);
            if LOSHt < elevs^[j] then begin
               ch := 'Y';
               UseData.SetFieldByNameAsString('BLOCK_TERR',ch);
            end;
         end;

         UseData.SetFieldByNameAsFloat('RANGE_KM',0.001 * dists^[j]);

         if MDDef.ShowMaskedAirspace then begin
            if NeedZ^[j] > 0.01 then begin
               UseData.SetFieldByNameAsFloat('MASK_AIR',NeedZ^[j] + elevs^[j]);
            end;
         end;

         {$IFDef ExVegDensity}
         {$Else}
            if (DEMonView.VegDensityLayers[1] <> Nil) then PointFromDensityLayer(1,j,'VPTS_AROUND','VPTS_ABOVE');
            if (DEMonView.VegDensityLayers[2] <> Nil) then PointFromDensityLayer(2,j,'VPT2_AROUND','VPT2_ABOVE');
         {$EndIf}

         {$IFDef ExVegDensity}
            NeedToCheckPointCloud := false;
         {$Else}
            if (DEMonView.VegGrid[1] <> 0) then begin
               if (DEMonView.VegGrid[2] <> 0) then begin
                   DEMGlb[DEMonView.VegGrid[2]].GetJustVegHeight(xgrids^[j],ygrids^[j],veght);
                   if (VegHt < 32000) then begin
                      UseData.SetFieldByNameAsFloat('VEG2_HT',VegHt);
                      if (elevs^[j] < 32000) then begin
                         if (LOSHt < elevs^[j] + VegHt) then begin
                            UseData.SetFieldByNameAsString('BLOCK_VEG2','Y');
                            NeedToCheckPointCloud := true;
                         end;
                     end;
                   end;
                end;
               DEMGlb[DEMonView.VegGrid[1]].GetJustVegHeight(xgrids^[j],ygrids^[j],veght);
               NeedToCheckPointCloud := false;
               if (VegHt < 32000) then begin
                  UseData.SetFieldByNameAsFloat('VEG_HT',VegHt);
                  if (elevs^[j] < 32000) then begin
                     if (LOSHt < elevs^[j] + VegHt) then begin
                        ch := 'Y';
                        UseData.SetFieldByNameAsString('BLOCK_VEG',ch);
                        NeedToCheckPointCloud := true;
                     end;
                 end;
               end;
            end
            else NeedToCheckPointCloud := true;
         {$EndIf}

         {$IfDef ExPointCloudMemory}
         {$Else}
            if NeedToCheckPointCloud and ((pc1 <> Nil) or (pc2 <> Nil)) then begin
                if (LosHt > elevs^[j]) and (LosHt < elevs^[j] + MDDef.MaxVegHeight) then begin
                   if (pc1 <> Nil) then begin
                      pc1.GetNumPointsNearAndAboveLocation(dists^[j],LosHt,NumPts,NumPts2);
                      if (NumPts > 0) then GISdb[FresnelDB].MyData.SetFieldByNameAsInteger('PTS_AROUND',NumPts);
                      if (NumPts2 > 0) then GISdb[FresnelDB].MyData.SetFieldByNameAsInteger('PTS_ABOVE',NumPts2);
                   end;
                   if (pc2 <> Nil) then begin
                      pc2.GetNumPointsNearAndAboveLocation(dists^[j],LosHt,NumPts,NumPts2);
                      if (NumPts > 0) then GISdb[FresnelDB].MyData.SetFieldByNameAsInteger('PTS2_AROUND',NumPts);
                      if (NumPts2 > 0) then GISdb[FresnelDB].MyData.SetFieldByNameAsInteger('PTS2_ABOVE',NumPts2);
                   end;
                end;
            end;
         {$EndIf}

         {$IfDef ExFresnel}
         {$Else}
             if (MDdef.CurvAlg = vcRadioLineOfSight) and MDdef.DrawFresnel then begin
               Fresnel2 := 17.31 * 5 * sqrt(0.001 * dists^[j] * (LOSLen - dists^[j]) / MDdef.FresnelFreq / LOSLen);
               UseData.CarefullySetFloat('FRESNEL2_M',Fresnel2,0.1);
               Fresnel1 := 17.31 * sqrt(0.001 * dists^[j] * (LOSLen - dists^[j]) / MDdef.FresnelFreq / LOSLen);
               UseData.CarefullySetFloat('FRESNEL1_M',Fresnel1,0.1);
             end;
         {$EndIf}

          if MDDef.ForceCrestComputations then begin
             IsPeak[j] := false;
             IsPit[j] := false;
          end;

          if (Elevs^[j] < 32000) then begin
             if MDDef.ForceCrestComputations then begin
                if (j > MDdef.PeakPitPostings) and (j < ComputePoints - MDdef.PeakPitPostings) then begin
                   IsPeak[j] := true;
                   IsPit[j] := true;
                   for I := j-MDdef.PeakPitPostings to j+MDdef.PeakPitPostings do begin
                      if (i <> j) then begin
                         if elevs^[j] < elevs^[i] then IsPeak[j] := false;
                         if elevs^[j] > elevs^[i] then IsPit[j] := false;
                      end;
                   end;
                end;
             end;

              if (MDDef.LosVisible and VisPoints[j]) then Color := MDDef.FanColor else Color := MDDef.MaskColor;

              UseData.SetColorFromPlatformColor(Color);

              if MDDef.DoGrazingFields then begin
                 if (j > 0) then begin
                    Slope2 := ArcTan((elevs^[j] - elevs^[pred(j)]) / (dists^[j] - dists^[pred(j)])) / DegToRad;
                    if abs(Slope2) < 0.1 then Slope2 := 0;
                    UseData.CarefullySetFloat('SLOPE_2D',Slope2,0.1);
                 end;

                 if DEMonView.GetSlopeAndAspectFromLatLong(Lat,Long,SlopeAspectRec) then begin
                  //DEMonView.LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                  //Slope3 := DEMonView.SlopeOfPoint(round(xg),round(yg),Dir,AspDir);
                     if (SlopeAspectRec.SlopePercent > 0.0001) then begin
                        UseData.CarefullySetFloat32('ASPECT',SlopeAspectRec.AspectDir,0.1);
                     end;
                     if abs(SlopeAspectRec.SlopePercent) < 0.1 then SlopeAspectRec.SlopePercent := 0;
                     UseData.CarefullySetFloat32('SLOPE_3D',SlopeAspectRec.SlopePercent,0.1);
                     n1 := cosDeg(SlopeAspectRec.SlopeDegree) * sinDeg(SlopeAspectRec.AspectDir);
                     n2 := cosDeg(SlopeAspectRec.SlopeDegree) * cosDeg(SlopeAspectRec.AspectDir);
                     n3 := -sinDeg(SlopeAspectRec.SlopeDegree);
                 end;

                  if (j > 0) then begin
                     LocalPitch := -ArcTan((ObsElev - lz - EarthCurve ) /dists^[j]) / DegToRad;
                     if abs(Pitch) < 0.1 then Pitch := 0;
                     UseData.CarefullySetFloat('PITCH',LocalPitch,0.1);

                     Slope2 := ArcTan((elevs^[j] - lz) / (dists^[j] - dists^[pred(j)])) / DegToRad;
                     if abs(Slope2) < 0.1 then Slope2 := 0;
                     UseData.CarefullySetFloat('SLOPE_2D',Slope2,0.1);

                     if VisPoints[j] then begin
                        l1 := cosDeg(Pitch) * sinDeg(LOSAzimuth);
                        l2 := cosDeg(Pitch) * cosDeg(LOSAzimuth);
                        l3 := sinDeg(Pitch);
                        Graz2 := arcCos(n1*l1 + n2*l2 + n3*l3) / DegToRad;
                        Slope2 := Slope2+LocalPitch;
                        UseData.CarefullySetFloat('GRAZING_2D',Slope2,0.1);
                        UseData.CarefullySetFloat('GRAZING_3D',Graz2,0.1);
                     end;
                  end;
              end;

             {$IfDef ExFresnel}
             {$Else}
                 if (MDdef.CurvAlg = vcRadioLineOfSight) and MDdef.DrawFresnel then begin
                    if (LOSHt < elevs^[j]) then Intrudepc := 100
                    else if (LOSHt > elevs^[j] + Fresnel1) then Intrudepc := 0
                    else Intrudepc := 100 * (elevs^[j] - (LOSHt - Fresnel1)) / Fresnel1;
                    UseData.SetFieldByNameAsFloat('INTRUDE_pc',Intrudepc);
                 end;
              {$EndIf}

              UseData.Post;
         end
         else begin
            {$IfDef ExFresnel}
            {$Else}
            if ((dists^[j] < 1000)  or (dists^[j] > LosLen - 1000)) and (Intrudepc > 0.001) then begin
               result := losEdgeFresnelIntrusion;
               goto ExitNow;
            end
            else begin
               if (Intrudepc > 40) then begin
                  result := losCenterFresnelIntrusion;
                  goto ExitNow;
               end;
            end;
            {$EndIf}
         end;
         lz := elevs^[j];
      end;

      result := losIsVisible;

      if MDDef.ForceCrestComputations then begin
         {$IfDef RecordLOS} WriteLineToDebugFile('Start peaks/pits');    {$EndIf}
         RemoveNeighbors(IsPeak);
         RemoveNeighbors(IsPit);

         UseData.First;
         for j := 0 to ComputePoints do begin
            if IsPeak[j] or IsPit[j] then begin
               UseData.Edit;
               if IsPeak[j] then UseData.SetFieldByNameAsString('PEAK','Y')
               else if IsPit[j] then UseData.SetFieldByNameAsString('PIT','Y');
            end;
            UseData.Next;
         end;

         {$IfDef RecordLOS} WriteLineToDebugFile('Done  peaks/pits'); {$EndIf}
      end;

      if (UseData <> Nil) then begin
        {$IfDef RecordLOS} WriteLineToDebugFile('Start VisComp'); {$EndIf}

          if VisPoints[ComputePoints] then begin
             {$IfDef ExFresnel}
             {$Else}
             if MDdef.DrawFresnel then begin
                UseData.ApplyFilter( 'RANGE_KM < 1');
                UseData.FindFieldRange('INTRUDE_pc',MinVal,MaxVal);
                if (MaxVal > 0.001) then Result := losEdgeFresnelIntrusion
                else begin
                   KM_short_end := 0.001 * LOSLen - 1;
                   UseData.ApplyFilter('RANGE_KM > ' + RealToString(KM_short_end,-12,2));
                   UseData.FindFieldRange('INTRUDE_pc',MinVal,MaxVal);
                   if MaxVal > 0.001 then Result := losEdgeFresnelIntrusion
                   else begin
                      UseData.ApplyFilter('RANGE_KM > 1 AND RANGE_KM < ' + RealToString(KM_short_end,-12,2));
                      UseData.FindFieldRange('INTRUDE_pc',MinVal,MaxVal);
                      if (MaxVal > 40) then Result := losCenterFresnelIntrusion
                      else Result := losIsVisible;
                   end;
                end;
             end;
             {$EndIf}
          end
          else Result := losBlockByTerrain;
        {$IfDef RecordLOS} WriteLineToDebugFile('End VisComp'); {$EndIf}
      end;

 ExitNow:;
  Dispose(xgrids);
  Dispose(ygrids);
  Dispose(dists);
  Dispose(elevs);
  if MDDef.ShowMaskedAirspace then Dispose(NeedZ);
end;



{$IfDef ExGeostats}
{$Else}

procedure FindWavelengthStats(var UseData : tMyData; var WavelengthMean,WavelengthMedian,WavelengthStdDev,
   HeightMean,HeightMedian,HeightStd : float64; ColorFields : boolean = false);
var
   MomentVar : tMomentVar;
   lastx,lasty,x,y,dz : float64;
   LastPit,ThisPit : boolean;
   TStr,TStr2  : shortString;
   Factor : integer;
   xs,ys : array[0..500] of float32;

         procedure ReadValues;
         begin
            Y := UseData.GetFieldByNameAsFloat('ELEV_M');
            X :=  UseData.GetFieldByNameAsFloat('RANGE_KM');
            ThisPit :=  UseData.GetFieldByNameAsString('PIT') = 'Y';
            UseData.Next;
         end;

         procedure ColorAllFields(Color : tColor);
         begin
            UseData.First;
            repeat
               UseData.Edit;
               UseData.SetFieldByNameAsInteger('COLOR',Color);
               UseData.Next;
            until UseData.EOF;
         end;

begin
  {$IfDef RecordWaveLenghtHeight} WriteLineToDebugFile('FindWavelengthStats in, table=' + UseData.TableName); {$EndIf}
   if (UseData = Nil) then begin
      exit;
   end;
   if UseData.FieldExists('PEAK') then begin
      WavelengthMean := 0;
      WavelengthMedian := 0;
      WavelengthStdDev := 0;
      HeightMean := 0;
      HeightMedian := 0;
      HeightStd := 0;

      if ColorFields then ColorAllFields(clYellow);
      UseData.ApplyFilter( 'PIT=' + QuotedStr('Y') + ' OR PEAK=' + QuotedStr('Y'));
      MomentVar.NPts := 0;
      ReadValues;

      while not UseData.eof do begin
         LastX := x;
         LastY := y;
         LastPit := ThisPit;
         repeat
            ReadValues;
            dz := abs(LastY-y);
            if (dz >= MDDef.MinWaveHeight) then begin
               TStr := RealToString(x-Lastx,11,3) + RealToString(dz,16,2);
               Factor := 2;
               if (LastPit = ThisPit) then begin
                  if LastPit and ThisPit then TStr2 := 'adjacent Pits'
                  else TStr2 := 'adjacent peaks';
                  TStr := TStr + '   ' + TStr2;
                  Factor := 1;
               end;
               xs[MomentVar.NPts] := Factor * (x-Lastx);
               ys[MomentVar.NPts] := dz;
               inc(MomentVar.NPts);
            end;
         until (dz >= MDDef.MinWaveHeight) or UseData.eof;
      end;

      if (MomentVar.NPts > MDDef.WaveHtValuesNeeded) then begin
         Moment(xs,MomentVar,msAll);
         WaveLengthMedian := MomentVar.Median;
         WavelengthMean := MomentVar.mean;
         WaveLengthStdDev := MomentVar.sdev;
         Moment(ys,MomentVar,msAll);
         HeightMedian := MomentVar.Median;
         HeightMean := MomentVar.mean;
         HeightStd := MomentVar.sdev;
      end
      else begin
         {$IfDef RecordWaveLenghtHeight} WriteLineToDebugFile('NPts < MDDef.WaveHtValuesNeeded ' + IntToStr(Npts) + '<' + IntToStr(MDDef.WaveHtValuesNeeded)); {$EndIf}
      end;

      if ColorFields then begin
         UseData.ApplyFilter('PIT=' + QuotedStr('Y'));
         ColorAllFields(clRed);
         UseData.ApplyFilter('PEAK=' + QuotedStr('Y'));
         ColorAllFields(clLime);
      end;
   end;
end;


procedure AddToWaveSpacingHeightResults(var Findings : TStringList; WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd : float64);
begin
    Findings.Add('Wavelength (m)');
    Findings.Add('   Mean: ' + RealToString(1000*WavelengthMean,-18,-2));
    Findings.Add('   Std Dev: ' + RealToString(1000*WavelengthStdDev,-18,-2));
    Findings.Add('   Median: ' + RealToString(1000*WaveLengthMedian,-18,-2));
    Findings.Add(' ');
    Findings.Add('Height (m)');
    Findings.Add('   Mean: ' + RealToString(HeightMean,-18,-2));
    Findings.Add('   Std Dev: ' + RealToString(HeightStd,-18,-2));
    Findings.Add('   Median: ' + RealToString(HeightMedian,-18,-2));
    Findings.Add(' ');
end;

{$EndIf}


{ tLOSdraw }

constructor tLOSdraw.Create;
var
   whichdem : integer;
   {$IfDef ExPointCloudMemory}
   {$Else}
   j : integer;
   {$EndIf}
begin
   {$IfDef RecordLOS} WriteLineToDebugFile('tLOSdraw.Create'); {$EndIf}
   TargetName := '';
   SensorName := '';
   MultipleProfilesToShow := Nil;
   LOSVariety := losVanilla;
   ZoomedDiagram := false;
   ShowSeaLevel := true;
   CompareDensityAlongProfile := false;
   //EraseFresnelDB := false;
   EnvelopeDone := false;
   ProfileDropDown := 0;
   FresnelDB := 0;
   MinAreaZ := MaxSmallInt;
   MaxAreaZ := -MaxSmallInt;
   TargetGroundElevLAS := -9999;
   ObsGroundElevLAS := -9999;
   for WhichDEM := 1 to MaxDEMDataSets do ShowProfile[WhichDEM] := true;
   {$IfDef ExPointCloudMemory}
   {$Else}
      PtCldInUse := 1;
      for j := 1 to 2 do LOSMemoryPointCloud[j] := Nil;
   {$EndIf}
   for WhichDEM := 1 to NumDEMDataSetsOpen do begin
      ProfileName[WhichDEM] := '';
      if (DEMGlb[WhichDEM] <> Nil) then begin
         {$IfDef RecordLOS} WriteLineToDebugFile(DEMGlb[WhichDEM].AreaName); {$EndIf}
         ProfileName[WhichDEM] := RemoveUnderScores(DEMGlb[WhichDEM].AreaName)
      end;
   end;
end;

destructor tLOSdraw.Destroy;
begin
end;

initialization
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demlosw in'); {$EndIf}
   {$IfDef RecordLOS} WriteLineToDebugFile('RecordLOSProblems active in DEMLOSW'); {$EndIf}
   {$IfDef RecordAllLOS} WriteLineToDebugFile('RecordAllLOSProblems active in DEMLOSW'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems active in DEMLOSW'); {$EndIf}
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('RecordLOSAlgorithm active in DEMLOSW'); {$EndIf}
   {$IfDef RecordWaveLenghtHeight} WriteLineToDebugFile('RecordWaveLenghtHeightProblems active in DEMLOSW');    {$EndIf}
   {$IfDef RecordNearestCrest} WriteLineToDebugFile('RecordNearestCrest active in DEMLOSW');   {$EndIf}
   {$IfDef RecordThreadCrest} WriteLineToDebugFile('RecordThreadCrest active in DEMLOSW');   {$EndIf}
   {$IfDef RecordPointClouds} WriteLineToDebugFile('RecordPointClouds active in DEMLOSW');   {$EndIf}
   {$IfDef RecordMGT} WriteLineToDebugFile('RecordMGTProblems active in DEMLOSW');   {$EndIf}
   {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile('RecordLOSPrettyDrawing active in DEMLOSW');   {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demlosw out');   {$EndIf}
end.
