unit get_thalwegs_thread;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


removed, Aug 2017 (had not been in use for a long time?)


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordThalwegs}
   //{$Define FullRecordThalwegs}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
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


  Classes,SysUtils,System.Math,
  Petmar_Types;

type
  find_thalwegs = class(TThread)
  private
    { Private declarations }
     procedure UpdateThreadProgress;
  protected
    BasinsName,RiversName,OutPath : PathStr;
    BasinsDB,RiversDB,
    ProgressBar,PerCentDone : integer;
    BasinCode : shortstring;
    SizeFilter : AnsiString;
  public
     constructor Create(inBasinCode : shortstring; inBasinName,InRiversName,inOutPath : PathStr; inProgressBar : integer);
     procedure Execute; override;
  end;

procedure CreateThalwegs(BasinCode : shortstring; BasinsName,RiversName,OutPath : PathStr);



implementation

{ extract_basins }

uses
   DEMDatum,DEMDefs,DEMDataBase,PetDBUtils,Thread_Timers,DEMMapf,Petmar;


procedure CreateThalwegs(BasinCode : shortstring; BasinsName,RiversName,OutPath : PathStr);
{$IfDef ExGeostats}
begin
{$Else}
var
   BasinsDB,RiversDB,
   PerCentDone : integer;
   i : integer;
   StrahlerOrder : integer;
   ThalwegLength,TotalLength : float;
   TStr : shortString;
   fName,fName2 : PathStr;

           procedure DoBasinStats;
           begin
              GISdb[RiversDB].GetDrainageBasinStats(StrahlerOrder,ThalwegLength,TotalLength);
              GISdb[BasinsDB].MyData.Edit;
              GISdb[BasinsDB].MyData.SetFieldByNameAsFloat('THALWEG_KM',ThalwegLength);
              GISdb[BasinsDB].MyData.SetFieldByNameAsFloat('CHANNEL_KM',TotalLength);
              GISdb[BasinsDB].MyData.SetFieldByNameAsFloat('PERIMTR_KM',0.001 * GISdb[BasinsDB].aShapeFile.LineLength({DEMDatum.WGS84DatumConstants,}GISdb[BasinsDB].MyData.RecNo));
              GISdb[BasinsDB].MyData.SetFieldByNameAsInteger('STRAHLER_O',StrahlerOrder);
              GISdb[BasinsDB].MyData.Post;
           end;

begin
  { Place thread code here }
  {$IfDef RecordThalwegs}
  WriteLineToDebugFile('find_thalwegs.Execute in',true,true);
  {$EndIf}

   RiversDB := 2;
   BasinsDB := 1;
   OpenGISDataBase(RiversDB,'',GISdb[RiversDB],RiversName);
   OpenGISDataBase(BasinsDB,'',GISdb[BasinsDB],BasinsName);
   if (RiversDB <> 0) and (BasinsDB <> 0) then begin
      i := 0;
      repeat
         inc(i);
         PercentDone := round(100 * i/GISdb[BasinsDB].MyData.RecordCount);
         ThreadTimers.UpdateThreadStats(9,PercentDone);
         ThreadTimers.UpdateThreadStats(1,0);
         ThreadTimers.UpdateThreadStats(2,0);
         ThreadTimers.UpdateThreadStats(3,0);
         ThreadTimers.UpdateThreadStats(4,0);

          DEMNowDoing := Calculating;
          TStr := uppercase(GISdb[BasinsDB].MyData.GetFieldByNameAsString('BASIN_ID'));
          if (TStr <> '') then begin
             GISdb[RiversDB].MyData.ApplyFilter( 'BASIN_ID=' + QuotedStr(TStr));
             fName := NodesDir + TStr + DefaultDBExt;
             fName2 := ThalwegDir + 'thalweg_' + TStr + DefaultDBExt;
             if (FileExists(fName) and FileExists(fName2)) then begin
                {$IfDef FullRecordThalwegs}
                WriteLineToDebugFile('Existed: ' + TStr,false,true);
                {$EndIf}
             end
             else begin
                if (GISdb[RiversDB].MyData.RecordCount = 0) then begin
                   {$IfDef RecordThalwegs}
                   WriteLineToDebugFile('Nothing for basin: ' + TStr,false,true);
                   {$EndIf}
                end
                else begin
                   {$IfDef RecordThalwegs}
                   ClearDebugLog;
                   WriteLineToDebugFile('Start basin: ' + TStr + '',false,true);
                   {$EndIf}
                   GISdb[RiversDB].GetDrainageBasinNetwork;
                   {$IfDef RecordThalwegs}
                   WriteLineToDebugFile('Get stats: ' + TStr + '',false,true);
                   {$EndIf}
                   DoBasinStats;
                 end;
              end;
          end;
          if CeaseThreads then break;
          GISdb[BasinsDB].MyData.Next;
       until GISdb[BasinsDB].MyData.EOF;

       CloseAndNilNumberedDB(RiversDB);
       CloseAndNilNumberedDB(BasinsDB);
       {$IfDef RecordThalwegs}
       WriteLineToDebugFile('find_thalwegs.Execute out',false,true);
       {$EndIf}
   end;
{$EndIf}
end;




constructor find_thalwegs.Create(inBasinCode : shortstring; inBasinName,InRiversName,inOutPath : PathStr; inProgressBar : integer);
begin
    inherited Create(false);
    BasinsName := inBasinName;
    RiversName := inRiversName;
    BasinCode := inBasinCode;
    OutPath := inOutPath;
    ProgressBar := inProgressBar;
    FreeOnTerminate := true;   //not DoProgress;
end;


procedure find_thalwegs.UpdateThreadProgress;
begin
    ThreadTimers.UpdateThreadStats(9,PercentDone);
end;


procedure find_thalwegs.Execute;
{$IfDef ExGeostats}
begin
{$Else}
var
   i : integer;
   StrahlerOrder : integer;
   ThalwegLength,TotalLength : float;
   TStr : shortString;
   fName,fName2 : PathStr;

           procedure DoBasinStats;
           begin
              GISdb[RiversDB].GetDrainageBasinStats(StrahlerOrder,ThalwegLength,TotalLength);
              GISdb[BasinsDB].MyData.Edit;
              GISdb[BasinsDB].MyData.SetFieldByNameAsFloat('THALWEG_KM',ThalwegLength);
              GISdb[BasinsDB].MyData.SetFieldByNameAsFloat('CHANNEL_KM',TotalLength);
              GISdb[BasinsDB].MyData.SetFieldByNameAsFloat('PERIMTR_KM',0.001 * GISdb[BasinsDB].aShapeFile.LineLength({DEMDatum.WGS84DatumConstants,}GISdb[BasinsDB].MyData.RecNo));
              GISdb[BasinsDB].MyData.SetFieldByNameAsInteger('STRAHLER_O',StrahlerOrder);
              GISdb[BasinsDB].MyData.Post;
           end;

begin
  { Place thread code here }
  {$IfDef RecordThalwegs}
  WriteLineToDebugFile('find_thalwegs.Execute in',true,true);
  {$EndIf}

   RiversDB := 2 * ProgressBar;
   BasinsDB := pred(2 * ProgressBar);
   OpenGISDataBase(RiversDB,'',GISdb[RiversDB],RiversName);
   OpenGISDataBase(BasinsDB,'',GISdb[BasinsDB],BasinsName);
   if (RiversDB <> 0) and (BasinsDB <> 0) then begin
      i := 0;
      repeat
         inc(i);
         PercentDone := round(100 * i/GISdb[BasinsDB].MyData.RecordCount);
         ThreadTimers.UpdateThreadStats(9,PercentDone);
         ThreadTimers.UpdateThreadStats(1,0);
         ThreadTimers.UpdateThreadStats(2,0);
         ThreadTimers.UpdateThreadStats(3,0);
         ThreadTimers.UpdateThreadStats(4,0);

          DEMNowDoing := Calculating;
          TStr := uppercase(GISdb[BasinsDB].MyData.GetFieldByNameAsString('BASIN_ID'));
          if (TStr <> '') then begin
             GISdb[RiversDB].MyData.ApplyFilter(  'BASIN_ID=' + QuotedStr(TStr));
             fName := NodesDir + TStr + DefaultDBExt;
             fName2 := ThalwegDir + 'thalweg_' + TStr + DefaultDBExt;
             if (FileExists(fName) and FileExists(fName2)) then begin
                {$IfDef FullRecordThalwegs}
                WriteLineToDebugFile('Existed: ' + TStr,false,true);
                {$EndIf}
             end
             else begin
                if (GISdb[RiversDB].MyData.RecordCount = 0) then begin
                   {$IfDef RecordThalwegs}
                   WriteLineToDebugFile('Nothing for basin: ' + TStr,false,true);
                   {$EndIf}
                end
                else begin
                   {$IfDef RecordThalwegs}
                   ClearDebugLog;
                   WriteLineToDebugFile('Start basin: ' + TStr);
                   {$EndIf}
                   GISdb[RiversDB].GetDrainageBasinNetwork;
                   {$IfDef RecordThalwegs}
                   WriteLineToDebugFile('Get stats: ' + TStr);
                   {$EndIf}
                   DoBasinStats;
                 end;
              end;
          end;
          if CeaseThreads then break;
          GISdb[BasinsDB].MyData.Next;
       until GISdb[BasinsDB].MyData.EOF;

       CloseAndNilNumberedDB(RiversDB);
       CloseAndNilNumberedDB(BasinsDB);
       PercentDone := 999;
       Synchronize(UpdateThreadProgress);
       {$IfDef RecordThalwegs}
       WriteLineToDebugFile('find_thalwegs.Execute out',false,true);
       {$EndIf}
   end;
{$EndIf}
end;



initialization
finalization
     {$IfDef RecordThalwegs}
     WriteLineToDebugFile('RecordThalwegs active in get_thalwegs_thread');
     {$EndIf}
     {$IfDef FullRecordThalwegs}
     WriteLineToDebugFile('FullRecordThalwegs active in get_thalwegs_thread');
     {$EndIf}
end.
