unit geomorph_filter;


//      8/14/2019   This appears not to be used any more; holdover from Gogia

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordGeomorphFilter}
   //{$Define RecordVerboseGeomorphFilter}
{$EndIf}                


interface

uses
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


  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, System.Math,
  Petmar_types,Petmar, StdCtrls, Buttons, ComCtrls, ExtCtrls;

const
   MaxMasks = 25;

type
  TGeomorphFilterForm = class(TForm)
    DataSource1: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    BitBtn2: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn9: TBitBtn;
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GetMaskParameters(var GridMaskDEM : integer; var MaskMinVal,MaskMaxVal : float64);
    procedure ParseParametersTable;
    procedure LoadGeomorphDEMs;
    function GetParametersToUse : boolean;
  public
    { Public declarations }
    NumMasks : integer;
    BaseDEM,ViewshedGrid : integer;
    CenterLat,CenterLong : float64;
    ProjectPath,
    ViewshedGridFileName : PathStr;
    RegionTable : tMyData;
    MaximaTable : tMyData;
    ParamsTable : tMyData;
    MaskDEMs : array[1..MaxMasks] of integer;
    AbsoluteMaskDEM  : array[1..MaxMasks] of boolean;
    ShortMaskDEMFieldNames,
    MaskDEMFieldNames : array[1..MaxMasks] of ShortString;
    MaskMins,MaskMaxes : array[1..MaxMasks] of float64;
  end;


function DoGeomorphicFiltering(aBaseDEM : integer; Lat,Long : float64; RegionTable : tMyData = Nil) : boolean;


implementation

uses
{Main program MDI window for different programs that use this module}
   Nevadia_Main,
{End of the MDI parent declaration}
   {$IfDef ExGeoStats}
   {$Else}
   DEMStat,
   {$EndIf}
   map_masking,

   PetDBUtils,PetImage,
   Read_DEM,DEMDefs,
   DEMDef_Routines,
   DEM_Manager,
   Databasecreate,
   DEMDataBase,
   DEMESRIShapeFile,
   DEMCoord,
   DEMMapf,BaseGraf,
   PETMath, pick_limits, toggle_db_use,
   demoptions, Make_tables, make_grid;


{$R *.dfm}


const
   MaxLoc = 2500;


function DoGeomorphicFiltering(aBaseDEM : integer; Lat,Long : float64; RegionTable : tMyData = Nil) : boolean;
var
   GeomorphFilterForm  : TGeomorphFilterForm;
   fName : PathStr;
begin
   {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('DoGeomorphicFiltering in, ' + '  ' + fName);  {$EndIf}
   Result := false;
    fName := ProgramRootDir + 'geomorph' + DefaultDBExt;

    if not FileExists(fName) then begin
       MessageToContinue('Parameters listing missing: ' + fName);
       exit;
    end;
    SaveBackupDefaults;   //keep default DEM name

    GeomorphFilterForm := TGeomorphFilterForm.Create(Application);
    GeomorphFilterForm.RegionTable := RegionTable;
    with GeomorphFilterForm do begin
       BaseDEM := aBaseDEM;
       CenterLat := Lat;
       CenterLong := Long;

       ProjectPath := ExtractFilePath(DEMGlb[BaseDEM].DEMFileName);
       MaximaTable := Nil;
       ParamsTable := tMyData.Create(fName);

       ParamsTable.First;
       while not ParamsTable.EOF do begin
          ParamsTable.Edit;
          ParamsTable.SetFieldByNameAsInteger('DEM',0);
          ParamsTable.Next;
       end;

      {$IfDef RecordGeomorphFilter}
      WriteLineToDebugFile('Off to parameters in file: ' + IntToStr(ParamsTable.RecordCount) + '  ');
      {$EndIf}

      if GetParametersToUse then begin
         Result := true;
         RestoreBackupDefaults;
         ParamsTable.AssignEmpSource(DataSource1);
         //DataSource1.DataSet := ParamsTable.TheData;
         DataSource1.Enabled := true;
         BitBtn2.Enabled := true;
         BitBtn5.Enabled := true;
      end
      else begin
         Result := false;
         GeomorphFilterForm.Close;
      end;
   end;
   {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('DoGeomorphicFiltering out '); {$EndIf}
end;


function MakeRegionalBestMask(CloneDEM : integer) : integer;
var
  Col,Row,EndCol,EndRow,
  i,Pts : integer;
  Locations : array[0..maxLoc] of tGridZ;
begin
   OpenAndZeroNewDEM(true,DEMGlb[CloneDEM].DEMheader,Result);
   if (Result <> 0) then with MDDef.LocalOptimaOpts do begin
      StartProgress('Regions');
      if EdgeBuffer then begin
         Col := ColInc div 2;
         EndCol := pred(DEMGlb[CloneDEM].DEMheader.NumCol - ColInc div 2);
      end
      else begin
         Col := 0;
         EndCol := pred(DEMGlb[CloneDEM].DEMheader.NumCol);
      end;
      while (Col < EndCol) do begin
         UpdateProgressBar(Col/EndCol);
         if EdgeBuffer then begin
            Row := RowInc div 2;
            EndRow := pred(DEMGlb[CloneDEM].DEMheader.NumRow - RowInc div 2);
         end
         else begin
            Row := 0;
            EndRow := pred(DEMGlb[CloneDEM].DEMheader.NumRow);
         end;
         UpdateProgressBar(Col/DEMGlb[CloneDEM].DEMheader.NumCol);
         while (Row < EndRow) do with DEMGlb[CloneDEM] do begin
            Pts := NPts;
            if FindLocationOfMultipleMaxima(SpecifyDEMGridLimits(Col,Row,Col+ColInc,Row+RowInc),Pts,Locations) then begin
               for I := 0 to pred(Pts) do begin
                  DEMGlb[Result].SetGridElevation(Locations[i].x,Locations[i].y,Locations[i].z);
               end;
            end;
            Row := Row + RowInc;
         end;
         Col := Col + ColInc;
      end;
      EndProgress;
   end;
end;


procedure TGeomorphFilterForm.BitBtn9Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
  Col,Row,StartCol,StartRow,EndCol,EndRow,
  i : integer;
  GridLimits : tGridLimits;
  AddOnlyMaxViewshed,Predictions,DoViewShedMax : boolean;
  fName : PathStr;
  MaskBitmap : tMyBitmap;
  NewTable,LongVersion : boolean;
  xg,yg : float64;
  CreateDataBase : tCreateDataBase;


      procedure ProcessDEM(DEMNumber : integer; TheName : string35; Color : tColor);
      var
         MaxI,i,Pts : integer;
         MaxTrue,Lat,Long,zv,Optimal : float64;
         Locations : array[0..maxLoc] of tGridZ;

            procedure AddPointToTable(i : integer; zv : float64);
            var
               j : integer;
            begin
               DEMGlb[DEMNumber].DEMGridToLatLongDegree(Locations[i].x,Locations[i].y,Lat,Long);
               RegionTable.Insert;
               RegionTable.SetFieldByNameAsFloat('LAT',Lat);
               RegionTable.SetFieldByNameAsFloat('LONG',Long);
               if LongVersion then begin
                  RegionTable.SetFieldByNameAsFloat('Z',DEMGlb[BaseDEM].GridElevMeters(Locations[i].x,Locations[i].y));
                  RegionTable.SetFieldByNameAsInteger('X_GRID',Locations[i].x);
                  RegionTable.SetFieldByNameAsInteger('Y_GRID',Locations[i].y);
               end;
               if Predictions then begin
                  if LongVersion then begin
                     RegionTable.SetFieldByNameAsFloat('VALUE',Locations[i].z);
                     RegionTable.SetFieldByNameAsString('PARAMETER',TheName);
                  end;
                  if DoViewShedMax then begin
                     RegionTable.SetFieldByNameAsFloat('VIEWSHED',zv);
                     RegionTable.SetFieldByNameAsFloat('VIEW_PERC',DEMGlb[ViewshedGrid].PercentileOfPoint(Locations[i].x,Locations[i].y,GridLimits));
                     RegionTable.SetFieldByNameAsFloat('VIEW_OPT',Optimal);
                     RegionTable.SetFieldByNameAsFloat('VIEW_PC_OP',100 * zv / Optimal);
                  end;
               end
               else begin
                  RegionTable.SetFieldByNameAsFloat('VIEWSHED',Locations[0].z);
                  for j := 1 to NumMasks do begin
                     RegionTable.SetFieldByNameAsFloat(MaskDEMFieldNames[j],DEMGlb[MaskDEMs[j]].GridElevMeters(Locations[i].x,Locations[i].y));
                     RegionTable.SetFieldByNameAsFloat(ShortMaskDEMFieldNames[j] + '_P',DEMGlb[MaskDEMs[j]].PercentileOfPoint(Locations[i].x,Locations[i].y,GridLimits));
                  end;
               end;
               RegionTable.Post;
            end;

      begin
         ParamsTable.ApplyFilter( 'DEM=' + IntToStr(DEMNumber));
         if (ParamsTable.RecordCount = 1) and (UpperCase(ParamsTable.GetFieldByNameAsString('USE')) = 'Y') then begin
            if DoViewShedMax then begin
               Pts := 1;
               if (not DEMGlb[ViewshedGrid].FindLocationOfMultipleMaxima(GridLimits,Pts,Locations)) then exit;
               Optimal := Locations[0].z;
               if (Optimal > 999) then exit;
            end;

            Pts := MDDef.LocalOptimaOpts.NPts;
            if Predictions then begin
               {$IfDef RecordVerboseGeomorphFilter}
               WriteLineToDebugFile('Grid box:', true);
               WriteLineToDebugFile('    x:' + RealToString(GridLimits.XGridLow,8,0) + RealToString(GridLimits.XGridHigh,8,0));
               WriteLineToDebugFile('    y:' + RealToString(GridLimits.YGridLow,8,0) + RealToString(GridLimits.YGridHigh,8,0));
               {$EndIf}
               if DEMGlb[DEMNumber].FindLocationOfMultipleMaxima(GridLimits,Pts,Locations) then begin
                  if AddOnlyMaxViewshed then begin
                     MaxI := 0;
                     MaxTrue := -1;
                     for I := 0 to pred(Pts) do begin
                        {$IfDef RecordVerboseGeomorphFilter}
                        WriteLineToDebugFile(RealToString(Locations[i].x,8,0) + RealToString(Locations[i].y,8,0));
                        {$EndIf}
                        if DEMGlb[ViewshedGrid].GetElevMeters(Locations[i].x,Locations[i].y,zv) then begin
                           if (zv > MaxTrue) then begin
                              MaxTrue := zv;
                              MaxI := i;
                           end;
                        end;
                     end;
                     if (MaxTrue > 0) then AddPointToTable(MaxI,MaxTrue);
                  end
                  else begin
                     for I := 0 to pred(Pts) do begin
                        {$IfDef RecordVerboseGeomorphFilter}
                        WriteLineToDebugFile(RealToString(Locations[i].x,8,0) + RealToString(Locations[i].y,8,0));
                        {$EndIf}
                        if (Not DoViewShedMax) or DEMGlb[ViewshedGrid].GetElevMeters(Locations[i].x,Locations[i].y,zv) then begin
                           AddPointToTable(i,zv);
                        end;
                     end;
                  end;
               end
               else begin
                  {$IfDef RecordVerboseGeomorphFilter}
                  WriteLineToDebugFile('   nothing here');
                  {$EndIf}
               end;
            end
            else begin
               AddPointToTable(0,zv);
            end;
         end;
         ParamsTable.ApplyFilter('');
      end;


begin
   {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('TGeomorphFilterForm.BitBtn9Click (Regions) in ');  {$EndIf}
   ParseParametersTable;
   Predictions := true;
   AddOnlyMaxViewShed := false;
   LongVersion := true;
   DeleteFileIfExists(MDTempDir + 'mask.bmp');

   {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('Start mask map ');  {$EndIf}

   if not Map_Masking.MaskMap(DEMGlb[1].SelectionMap,false,true,100) then exit;

   DEMOptions.OptimaRegionsDefaults(BaseDEM);

   {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('Defaults picked ');  {$EndIf}

   with MDDef.LocalOptimaOpts do begin
      DoViewShedMax := TrilobiteComputer and (ViewshedGrid <> 0) and AnswerIsYes('Use exhaustive viewshed grid');
      if (NPts > MaxLoc) then NPts := MaxLoc;

       NewTable := (RegionTable = Nil);
       if NewTable then begin
          fName := ProjectPath;
          Petmar.GetFileNameDefaultExt('optimal locations',DBNameMask,FName,true);

          if FileExists(fName) then begin
             {$IfDef RecordGeomorphFilter}
             WriteLineToDebugFile('Using existing database database');
             {$EndIf}
          end
          else begin
             {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('TGeomorphFilterForm.BitBtn9Click create database');  {$EndIf}
             CreateDataBase := tCreateDataBase.Create(fName);
             with CreateDataBase do begin
                   AddAField('LAT',ftFloat,12,7);
                   AddAField('LONG',ftFloat,12,7);
                   AddAField('USE',ftString,1,0);
                   AddAField('NAME',ftString,36,0);
                   AddAField('SENSOR_RNG',ftFloat,10,0);
                   AddAField('MIN_RNG',ftFloat,9,1);
                   AddAField('SENSOR_UP',ftFloat,10,2);
                   AddAField('TARGET_UP',ftFloat,10,2);
                   AddAField('LEFT_AZ',ftFloat,6,2);
                   AddAField('RIGHT_AZ',ftFloat,6,2);
                   AddAField('MAX_VERT',ftFloat,10,2);
                   AddAField('MIN_VERT',ftFloat,10,2);

                 AddAField('Z',ftFloat,8,1);
                 if Predictions then begin
                    if DoViewShedMax then begin
                       AddAField('VIEWSHED',ftFloat,12,4);
                       AddAField('VIEW_PERC',ftFloat,12,2);
                       AddAField('VIEW_OPT',ftFloat,12,4);
                       AddAField('VIEW_PC_OP',ftFloat,12,4);
                    end;
                 end
                 else begin
                    AddAField('VIEWSHED',ftFloat,12,4);
                    for i := 1 to NumMasks do begin
                       AddAField(MaskDEMFieldNames[i],ftFloat,12,2);
                       AddAField(ShortMaskDEMFieldNames[i] + '_P',ftFloat,12,2);
                    end;
                 end;

                 if LongVersion then begin
                    AddAField('X_GRID',ftInteger,5,0);
                    AddAField('Y_GRID',ftInteger,5,0);
                    if Predictions then begin
                       AddAField('PARAMETER',ftString,18,0);
                       AddAField('VALUE',ftFloat,12,4);
                    end;
                 end;

                WriteCorrectHeader;
                Destroy;
             end;

          end;
          RegionTable := tMyData.Create(FName);
       end;

      if FileExists(MDTempDir + 'mask.bmp') then begin
         MaskBitmap := tMyBitmap.Create;
         MaskBitmap.LoadFromFile(MDTempDir + 'mask.bmp');
         for i := 1 to NumMasks do begin
            DEMGlb[MaskDEMs[i]].SelectionMap.ResizeByPercentage(100,true,true);
            DEMGlb[MaskDEMs[i]].SelectionMap.Image1.Picture.Graphic := MaskBitmap;
            DEMGlb[MaskDEMs[i]].SelectionMap.EditMapViaColor(emvcAllButSelectedColor,clRed);
         end;
         {$IfDef RecordGeomorphFilter} WriteLineToDebugFile('Over with Masks');  {$EndIf}
      end;

      LongVersion := RegionTable.FieldExists('Z') and RegionTable.FieldExists('X_GRID') and RegionTable.FieldExists('Y_GRID');

      if (MDDef.LocalOptimaOpts_DEMRegion = drEntireDEM) then begin
         {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('drEntireDEM'); {$EndIf}
         if EdgeBuffer then begin
            StartCol := ColInc div 2;
            EndCol := pred(DEMGlb[BaseDEM].DEMheader.NumCol - ColInc div 2);
            StartRow := RowInc div 2;
            EndRow := pred(DEMGlb[BaseDEM].DEMheader.NumRow - RowInc div 2);
         end
         else begin
            StartCol := 0;
            EndCol := pred(DEMGlb[BaseDEM].DEMheader.NumCol);
            StartRow := 0;
            EndRow := pred(DEMGlb[BaseDEM].DEMheader.NumRow);
         end;
      end
      else if (MDDef.LocalOptimaOpts_DEMRegion = drFullMap) then begin
         {$IfDef RecordGeomorphFilter}
         WriteLineToDebugFile('drFullMap', true);
         {$EndIf}
         StartCol := round(DEMGlb[BaseDEM].SelectionMap.MapDraw.MapCorners.BoundBoxDataGrid.xmin);
         EndCol := round(DEMGlb[BaseDEM].SelectionMap.MapDraw.MapCorners.BoundBoxDataGrid.xmax);
         StartRow := round(DEMGlb[BaseDEM].SelectionMap.MapDraw.MapCorners.BoundBoxDataGrid.ymin);
         EndRow := round(DEMGlb[BaseDEM].SelectionMap.MapDraw.MapCorners.BoundBoxDataGrid.ymax);
      end
      else if (MDDef.LocalOptimaOpts_DEMRegion = drPickBox) then begin
         {$IfDef RecordGeomorphFilter} WriteLineToDebugFile('drPickBox'); {$EndIf}
         DEMGlb[BaseDEM].LatLongDegreeToDEMGrid(CenterLat,CenterLong,xg,yg);
         StartCol := round(xg - MDDef.OptimaBoxSize/DEMGlb[BaseDEM].AverageXSpace);
         EndCol := round(xg + MDDef.OptimaBoxSize/DEMGlb[BaseDEM].AverageXSpace);
         StartRow := round(yg - MDDef.OptimaBoxSize/DEMGlb[BaseDEM].AverageYSpace);
         EndRow := round(yg + MDDef.OptimaBoxSize/DEMGlb[BaseDEM].AverageYSpace);
         DEMGlb[BaseDEM].ClipDEMGrid(StartCol,StartRow);
         DEMGlb[BaseDEM].ClipDEMGrid(EndCol,EndRow);
      end;

      {$IfDef RecordGeomorphFilter}
      WriteLineToDebugFile('Region:');
      WriteLineToDebugFile('  cols=' + IntToStr(StartCol) + ' to ' + IntToStr(EndCol));
      WriteLineToDebugFile('  rows=' + IntToStr(StartRow) + ' to ' + IntToStr(EndRow));
      WriteLineToDebugFile('  ColInc=' + IntToStr(ColInc) + '  RowInc=' + IntToStr(RowInc));
      WriteLineToDebugFile('  Pts per=' + IntToStr(MDDef.LocalOptimaOpts.NPts));
      {$EndIf}

      StartProgress('Regions');
      Col := StartCol;
      while (Col  + ColInc < EndCol) do begin
         UpdateProgressBar(Col/EndCol);
         Row := StartRow;
         while (Row + RowInc < EndRow) do begin
            GridLimits := DEMGlb[BaseDEM].SpecifyDEMGridLimits(Col,Row,Col + pred(ColInc),Row + pred(RowInc));
            if DoViewShedMax then ProcessDEM(ViewshedGrid,'Viewshed',clRed);
            for i := 1 to NumMasks do ProcessDEM(MaskDEMs[i],DEMGlb[MaskDEMs[i]].AreaName,WinGraphColors[i]);
            Row := Row + RowInc;
         end;
         Col := Col + ColInc;
      end;
      EndProgress;
      {$IfDef RecordGeomorphFilter}
      WriteLineToDebugFile('Regions found');
      {$EndIf}
   end;

   if NewTable then begin
      {$IfDef RecordGeomorphFilter} WriteLineToDebugFile('New table operation start');  {$EndIf}
      RegionTable.Destroy;
      DEMGlb[BaseDEM].SelectionMap.LoadDataBaseFile(fName);
      {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('New table operation over');  {$EndIf}
   end;
   {$IfDef RecordGeomorphFilter}  WriteLineToDebugFile('TGeomorphFilterForm.BitBtn9Click out');  {$EndIf}
{$EndIf}
end;


procedure TGeomorphFilterForm.ParseParametersTable;
var
   Regsize : integer;
   ch : AnsiChar;
   TStr : ShortString;
begin
   {$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.ParseParametersTable in ');
   {$EndIf}
   NumMasks := 0;
   ParamsTable.First;
   while (not ParamsTable.EOF) do begin
      if UpperCase(ParamsTable.GetFieldByNameAsString('USE')) = 'Y' then begin
         inc(NumMasks);
         GetMaskParameters(MaskDEMs[NumMasks],MaskMins[NumMasks],MaskMaxes[NumMasks]);
         ch := AnsiChar(ord(ParamsTable.GetFieldByNameAsString('PARAMETER')[1]));
         //ch := ch2;
         RegSize := ParamsTable.GetFieldByNameAsInteger('REGION');
         MaskDEMFieldNames[NumMasks] := Make_grid.ShortDerivativeMapName(ch,RegSize);
         TStr := MaskDEMFieldNames[NumMasks];
         if (Length(TStr) > 8) then TStr := Copy(TStr,1,8);
         ShortMaskDEMFieldNames[NumMasks] := TStr;
         AbsoluteMaskDEM[NumMasks] := (MaskMins[NumMasks] > -98e39) or (MaskMaxes[NumMasks] < 98e39);
      end;
      ParamsTable.Next;
   end;
   {$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.ParseParametersTable out ');
   {$EndIf}
end;


procedure TGeomorphFilterForm.LoadGeomorphDEMs;
var
   RegSize,DEM : integer;
   ch : AnsiChar;
   fName : PathStr;
begin
   {$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.LoadGeomorphDEMs in ');
   {$EndIf}
    ParamsTable.First;
    while not ParamsTable.EOF do begin
       DEM := ParamsTable.GetFieldByNameAsInteger('DEM');
       if (DEM = 0) or (DEMGlb[DEM] = Nil) then begin
          ch :=  AnsiChar(ord(ParamsTable.GetFieldByNameAsString('PARAMETER')[1]));
          RegSize := ParamsTable.GetFieldByNameAsInteger('REGION');
          {$IfDef Gogia}
          fName := ProjectPath + ShortDerivativeMapName(ch,RegSize) + '.dem';
          {$Else}
          fName := MDTempDir + ShortDerivativeMapName(ch,RegSize) + '.dem';
          {$EndIf}
          if (UpperCase(ParamsTable.GetFieldByNameAsString('USE')) = 'Y') then begin
             if FileExists(fName) then begin
                Read_DEM.LoadNewDEM(DEM,fName,true,'','',false);

                {$IfDef Gogia}
                DEMGlb[DEM].SelectionMap.ResizeByPercentage(GogiaDefs.GeomorphZoom,true,true);
                DEMGlb[DEM].SelectionMap.MapDraw.MapType := mtDEMBlank;
                {$Else}
                //DEMGlb[DEM].SelectionMap.N11view1Click(Nil);
                {$EndIf}

                {$IfDef RecordGeomorphFilter}
                WriteLineToDebugFile('Loaded ' + fname, true);
                {$EndIf}
                {$IfDef RecordVerboseGeomorphFilter}
                WriteLineToDebugFile(DEMGlb[DEM].KeyDEMParams);
                {$EndIf}

                if DEMGlb[BaseDEM].SecondGridIdentical(DEM) then begin
                   DEMGlb[DEM].AreaName := DerivativeMapName(ch,RegSize);
                   DEMGlb[DEM].ShortName := ShortDerivativeMapName(ch,RegSize);
                   DEMGlb[DEM].SelectionMap.Caption := DEMGlb[DEM].AreaName;
                   {$IfDef RecordGeomorphFilter}
                   WriteLineToDebugFile('    ' + DEMGlb[DEM].AreaName);
                   WriteLineToDebugFile('    ' + DEMGlb[DEM].ShortName);
                   {$EndIf}
                end
                else begin
                   MessageToContinue(fName + ' not created with ' +DEMGlb[BaseDEM].AreaName);
                   CloseSingleDEM(DEM);
                   DeleteFileIfExists(fName);
                end;
             end;

             if not FileExists(fName) then begin
                {$IfDef RecordGeomorphFilter}
                WriteLineToDebugFile('Make ' + fname + '  ');
                WriteLineToDebugFile('    ' + ch + '   ' + IntToStr(RegSize));
                {$EndIf}
                DEM := MakeSingleNewDerivativeMap(ch,BaseDEM,RegSize);
                {$IfDef RecordGeomorphFilter}
                WriteLineToDebugFile('created '+ TimeToStr(now));
                {$EndIf}
                DEMGlb[DEM].WriteNewFormatDEM(fName);
                {$IfDef RecordGeomorphFilter}
                WriteLineToDebugFile('written');
                {$EndIf}
             end;
          end
          else begin
             DEM := 0;
          end;
          ParamsTable.Edit;
          ParamsTable.SetFieldByNameAsInteger('DEM',DEM);
          ParamsTable.SetFieldByNameAsString('NAME',DerivativeMapName(ch,RegSize));
          if (DEM <> 0) then ParamsTable.SetFieldByNameAsString('DEM_NAME',ExtractFileName(fName))
          else ParamsTable.SetFieldByNameAsString('DEM_NAME','');
       end;
       ParamsTable.Next;
    end;
    ParseParametersTable;
    {$IfDef RecordGeomorphFilter}
    WriteLineToDebugFile('TGeomorphFilterForm.LoadGeomorphDEMs out');
    {$EndIf}
end;


procedure TGeomorphFilterForm.FormCreate(Sender: TObject);
begin
   ViewshedGridFileName := '';
   Left := wmdem.ClientWidth - width - 5;
   Top := 0;
   ViewshedGrid := 0;
   NumMasks := 0;
   PlaceFormAtMousePosition(Self);
end;


procedure TGeomorphFilterForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.FormClose in');
   {$EndIf}
   if (MaximaTable <> Nil) then MaximaTable.Destroy;
   ParamsTable.Destroy;
   Action := caFree;
   {$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile(' FormClose out');
   {$EndIf}
end;

procedure TGeomorphFilterForm.GetMaskParameters(var GridMaskDEM : integer; var MaskMinVal,MaskMaxVal : float64);
begin
   If ParamsTable.GetFieldByNameAsString('MIN') = '' then MaskMinVal := -99e39
   else MaskMinVal := ParamsTable.GetFieldByNameAsFloat('MIN');
   If ParamsTable.GetFieldByNameAsString('MAX') = '' then MaskMaxVal := 99e39
   else MaskMaxVal := ParamsTable.GetFieldByNameAsFloat('MAX');
   GridMaskDEM := ParamsTable.GetFieldByNameAsInteger('DEM');
end;


procedure TGeomorphFilterForm.BitBtn1Click(Sender: TObject);
begin
   if (ViewshedGrid <> 0) then begin
      CloseSingleDEM(ViewshedGrid);
      Read_DEM.LoadNewDEM(ViewshedGrid,ViewshedGridFileName);
   end;
end;


procedure TGeomorphFilterForm.BitBtn2Click(Sender: TObject);
begin
{$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.BitBtn2Click (filter)');
{$EndIf}
   if (ViewshedGrid <> 0) then begin
      DEMGlb[ViewshedGrid].ReloadDEM(true);
      ParamsTable.First;
      while not ParamsTable.EOF do begin
         if UpperCase(ParamsTable.GetFieldByNameAsString('USE')) = 'Y' then begin
            GetMaskParameters(GridMaskDEM,MaskMinVal,MaskMaxVal);
            MaskGrid(DEMGlb[ViewshedGrid].SelectionMap,ViewshedGrid,true);
         end;
         ParamsTable.Next;
      end;
      DEMGlb[ViewshedGrid].SelectionMap.ForceRedraw1Click(nil);
   end;
end;


function TGeomorphFilterForm.GetParametersToUse : boolean;
begin
   Result := Toggle_db_use.VerifyRecordsToUse(ParamsTable,'NAME','Parameters to use');
   if Result then LoadGeomorphDEMs;
end;


procedure TGeomorphFilterForm.BitBtn5Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   //RegSize,
   i, NPts1,NPts2 : int64;
   Rank,
   xp,yp,xloc,yloc,Col,Row : integer;
   //MinZ,MaxZ,
   LocMax,Lat,Long : float64;
   //ch : AnsiChar;
   Percentiles,DrawHistograms : boolean;
   Color : tColor;
   fName,f2,f1 : PathStr;
   Vals1,Vals2 : ^PetMath.bfarray;
   Graph1,Graph2 : BaseGraf.TThisBaseGraph;
   HTMLString : string;
   HTMLStrings : tStringList;
   Locations : array[0..MaxLoc] of tGridZ;
   CreateDataBase : tCreateDataBase;
begin
{$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.BitBtn5Click (local minima)');
{$EndIf}
   if (ViewshedGrid = 0) then exit;
    SetSmallGraphs;
    OptimaRegionsDefaults(BaseDEM);
    Percentiles := false;
    DrawHistograms := true;

    fName := ProjectPath + 'optima' + DefaultDBExt;
    CreateDataBase := tCreateDataBase.Create(fName);
    with CreateDataBase do begin

       AddLatLongToTable;
       AddAField('X_GRID',ftInteger,5,0);
       AddAField('Y_GRID',ftInteger,5,0);
       AddAField('COVERAGE',ftFloat,6,2);
       AddAField('REG_RANK',ftFloat,4,1);

       ParseParametersTable;
       for i := 1 to NumMasks do AddAField(MaskDEMFieldNames[i],ftFloat,12,4);
       //CloseNewDBaseFile(DbaForm);
       WriteCorrectHeader;
       Destroy;

    end;
    MaximaTable := tMyData.Create(FName);

   StartProgress('Optima');
   Col := MDDef.LocalOptimaOpts.ColInc div 2;
   while (Col < DEMGlb[ViewshedGrid].DEMheader.NumCol - MDDef.LocalOptimaOpts.ColInc div 2 - 1)  do begin
      Row := MDDef.LocalOptimaOpts.RowInc div 2;
      UpdateProgressBar(Col/DEMGlb[ViewshedGrid].DEMheader.NumCol);
      while (Row < DEMGlb[ViewshedGrid].DEMheader.NumRow - MDDef.LocalOptimaOpts.RowInc div 2 -1) do begin
         if DEMGlb[ViewshedGrid].FindLocationOfMultipleMaxima(DEMGlb[ViewshedGrid].SpecifyDEMGridLimits(Col,Row,Col+MDDef.LocalOptimaOpts.Colinc,Row+MDDef.LocalOptimaOpts.RowInc),
                      MDDef.LocalOptimaOpts.NPts,Locations) then begin
            for Rank := 0 to pred(MDDef.LocalOptimaOpts.Npts) do begin
               xloc := Locations[Rank].x;
               yloc := Locations[Rank].y;
               LocMax := Locations[Rank].z;
               DEMGlb[ViewshedGrid].SelectionMap.MapDraw.DEMGridToScreen(xloc,yloc,xp,yp);
               Petmar.ScreenSymbol(DEMGlb[ViewshedGrid].SelectionMap.Image1.Canvas,xp,yp,FilledBox,3,ConvertTColorToPlatformColor(Color));

               DEMGlb[ViewshedGrid].DEMGridToLatLongDegree(xloc,yloc,Lat,Long);

               MaximaTable.Insert;
               MaximaTable.SetFieldByNameAsFloat('LAT',Lat);
               MaximaTable.SetFieldByNameAsFloat('LONG',Long);
               MaximaTable.SetFieldByNameAsInteger('X_GRID',xLoc);
               MaximaTable.SetFieldByNameAsInteger('Y_GRID',yLoc);
               MaximaTable.SetFieldByNameAsFloat('COVERAGE',LocMax);
               MaximaTable.SetFieldByNameAsInteger('REG_RANK',Rank);

               (*
               if (ScoreGrid <> 0) and DEMGlb[ScoreGrid].GetElevMeters(xloc,yloc,Score) then begin
                  MaximaTable.SetFieldByNameAsFloat('SCORE',Score;
                  for i := 1 to NumMasks do begin
                     if DEMGlb[MaskDEMs[i]].GetElevMeters(xloc,yloc,z) then begin
                        if Percentiles then z := DEMGlb[MaskDEMs[i]].PercentileOfPoint(xloc,yloc,Col,Row,Col+xinc,Row+yinc);
                        MaximaTable.SetFieldByNameAsFloat(MaskDEMFieldNames[i],z;
                     end;
                  end;
               end;
               *)
               MaximaTable.Post;
            end;
         end;
         Row := Row + MDDef.LocalOptimaOpts.RowInc;
      end;
      Col := Col + MDDef.LocalOptimaOpts.ColInc;
   end;

   if DrawHistograms then begin
      New(Vals1);
      New(Vals2);
      HTMLString := DEMGlb[BaseDEM].AreaName  + StartTableString;

      for i := 1 to NumMasks do begin
         GetPointArrayForDBField(MaximaTable,Nil,MaskDEMFieldNames[i],'','',Vals1^,Npts1);
         with DEMGlb[MaskDEMs[i]] do GetElevationsInLongArray(FullDEMGridLimits,NPts2,Vals2^,false);

         Graph1 := CreateDualHistogram(False,Npts1,NPts2,Vals1^,Vals2^,MaskDEMFieldNames[i],MaskDEMFieldNames[i]);

         f1 := 'image1-' + IntToStr(i) + OverlayFExt;
         PetImage.SaveImageAsBMP(Graph1.Image1,ProjectPath + f1);

         Graph2 := CreateCumProbabilityFromFile(Graph1.GraphDraw.DataFilesPlotted,MaskDEMFieldNames[i],MaskDEMFieldNames[i]);

         f2 := 'image2-' + IntToStr(i) + OverlayFExt;
         PetImage.SaveImageAsBMP(Graph2.Image1,ProjectPath + f2);

         HTMLString := HTMLString + StartRowString + HTMLTableCell(LinkImageString(f1)) + HTMLTableCell(LinkImageString(f2)) + EndRowString;
      end;

      HTMLString := HTMLString + EndTableString;

      HTMLStrings := tStringList.Create;
      HTMLStrings.Add(HTMLString);
      HTMLStrings.SaveToFile(ProjectPath + 'results.htm');
      HTMLStrings.Free;

      Dispose(Vals1);
      Dispose(Vals2);
   end;
   MaximaTable.Destroy;

   EndProgress;
   DEMDef_routines.RestoreBackupDefaults;
{$EndIf}
end;


procedure TGeomorphFilterForm.BitBtn7Click(Sender: TObject);
begin
   DEM_Manager.CloseAllWindowsAndData;
end;


initialization
finalization
   {$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('RecordGeomorphFilter active in geomorph_filter');
   {$EndIf}
   {$IfDef RecordVerboseGeomorphFilter}
   WriteLineToDebugFile('RecordVerboseGeomorphFilter active in geomorph_filter');
   {$EndIf}
end.


procedure TGeomorphFilterForm.BitBtn4Click(Sender: TObject);
var
   Col,Row,Mask : integer;
   z,Score : float64;
   GoodPoint : boolean;
begin
{$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.BitBtn4Click (score maps)');
{$EndIf}
   ParseParametersTable;
   if OpenAndZeroNewDEM(true,DEMGlb[BaseDEM].HeadRecs,ScoreGrid) then begin
      DEMGlb[ScoreGrid].Headrecs.ElevUnits := UndefinedHundredth;
      DEMGlb[ScoreGrid].DefineDEMvariables(true);
      for Col := 0 to pred(DEMGlb[ScoreGrid].HeadRecs.NumCol) do begin
         for Row := 0 to pred(DEMGlb[ScoreGrid].HeadRecs.NumRow) do begin
            Score := 0;
            GoodPoint := false;
            for Mask := 1 to NumMasks do begin
               if DEMGlb[MaskDEMs[Mask]].GetElevMeters(Col,Row,z) then begin
                 GoodPoint := true;
                 if (z >= MaskMins[Mask]) and (z <= MaskMaxes[Mask]) then Score := Score + 1;
               end;
            end;
            if GoodPoint then DEMGlb[ScoreGrid].SetGridElevation(Col,Row,Score);
         end;
      end;
     DEMGlb[ScoreGrid].CheckMaxMinElev;
     DEMGlb[ScoreGrid].AreaName := 'Coverage Scores';
     DEMGlb[ScoreGrid].SelectionMap := TMapForm.Create(Application);
     DEMGlb[ScoreGrid].SelectionMap.SetUpNewMapWindow(ScoreGrid,mtDEMElevationSpectrum,DEMGlb[ScoreGrid].AreaName,true,true);
   end;
end;


procedure TGeomorphFilterForm.BitBtn6Click(Sender: TObject);
var
   GridRows,MinScore,MaxScore,Col,Row,Score : integer;
   MinZ,MaxZ,ave,adev,sdev,svar,skew,curt, z : float64;
   ScoreFiles,LegendFiles : tStringList;
   NPts,TotalPts  : integer;
   zvs : ^Petmath.bfArray;
begin
{$IfDef RecordGeomorphFilter}
   WriteLineToDebugFile('TGeomorphFilterForm.BitBtn6Click ()');
{$EndIf}
   if (ViewshedGrid <> 0) then exit;
   if (ScoreGrid = 0) then BitBtn4Click(Sender);
   SetSmallGraphs;
   TabSheet3.TabVisible := true;

    StringGrid2.Cells[0,0] := 'Score';
    StringGrid2.Cells[1,0] := 'n';
    StringGrid2.Cells[2,0] := 'PerCent';

    StringGrid2.Cells[3,0] := 'Min';
    StringGrid2.Cells[4,0] := 'Max';
    StringGrid2.Cells[5,0] := 'Average';
    StringGrid2.Cells[6,0] := 'Std Dev';
    GridRows := 1;

   MinScore := round(DEMGlb[ScoreGrid].HeadRecs.MinElev);
   MaxScore := round(DEMGlb[ScoreGrid].HeadRecs.MaxElev);
   ScoreFiles := tStringList.Create;
   LegendFiles := tStringList.Create;
   New(zvs);
   TotalPts := 0;
   MinZ := 99e39;
   MaxZ := -99e39;

   for Col := 0 to pred(DEMGlb[ScoreGrid].HeadRecs.NumCol) do
      for Row := 0 to pred(DEMGlb[ScoreGrid].HeadRecs.NumRow) do
         if DEMGlb[ScoreGrid].GetElevMeters(Col,Row,z) and DEMGlb[ViewshedGrid].GetElevMeters(Col,Row,z) then begin
            zvs^[TotalPts] := z;
            inc(TotalPts);
            if z < MinZ then MinZ := z;
            if z > MaxZ then MaxZ := z;
         end;


   Moment(zvs^,TotalPts,ave,adev,sdev,svar,skew,curt);
   LegendFiles.Add('Score=' + IntToStr(Score));
   StringGrid2.Cells[0,GridRows] := 'Total';
   StringGrid2.Cells[1,GridRows] := IntToStr(TotalPts);
   StringGrid2.Cells[2,GridRows] := '100';
   StringGrid2.Cells[3,GridRows] := RealToString(MinZ,-18,-3);
   StringGrid2.Cells[4,GridRows] := RealToString(MaxZ,-18,-3);
   StringGrid2.Cells[5,GridRows] := RealToString(Ave,-18,-3);
   StringGrid2.Cells[6,GridRows] := RealToString(sdev,-18,-3);
   inc(GridRows);

   for Score := MinScore to MaxScore do begin
      Npts := 0;
      MinZ := 99e39;
      MaxZ := -99e39;
      for Col := 0 to pred(DEMGlb[ScoreGrid].HeadRecs.NumCol) do begin
         for Row := 0 to pred(DEMGlb[ScoreGrid].HeadRecs.NumRow) do begin
            if DEMGlb[ScoreGrid].GetElevMeters(Col,Row,z) then begin
               if (z > Score - 0.50) and (z < Score + 0.50) then begin
                  if DEMGlb[ViewshedGrid].GetElevMeters(Col,Row,z) then begin
                     zvs^[Npts] := z;
                     inc(Npts);
                     if z < MinZ then MinZ := z;
                     if z > MaxZ then MaxZ := z;
                  end;
               end;
            end;
         end;
      end;

      if (Npts > 0) then begin
         ScoreFiles.Add(SaveSingleValueSeries(npts,zvs^));
         Moment(zvs^,npts,ave,adev,sdev,svar,skew,curt);
         LegendFiles.Add('Score=' + IntToStr(Score));
         StringGrid2.Cells[0,GridRows] := IntToStr(Score);
         StringGrid2.Cells[1,GridRows] := IntToStr(Npts);
         StringGrid2.Cells[2,GridRows] := RealToString(100 * Npts / TotalPts,-18,-3);
         StringGrid2.Cells[3,GridRows] := RealToString(MinZ,-18,-3);
         StringGrid2.Cells[4,GridRows] := RealToString(MaxZ,-18,-3);
         StringGrid2.Cells[5,GridRows] := RealToString(Ave,-18,-3);
         StringGrid2.Cells[6,GridRows] := RealToString(sdev,-18,-3);
         inc(GridRows);
      end;
   end;
   StringGrid2.ColCount := 7;
   StringGrid2.RowCount := GridRows;
   Dispose(zvs);
   CreateMultipleHistograms(True,ScoreFiles,LegendFiles,'Viewshed coverage','Viewshed coverage by score',WinGraphColors);
   CreateMultipleHistograms(False,ScoreFiles,LegendFiles,'Viewshed coverage','Viewshed coverage by score',WinGraphColors);
   ScoreFiles.Free;
   LegendFiles.Free;
   DEMDef_routines.RestoreBackupDefaults;
end;



