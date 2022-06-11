unit extract_basins_thread;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

removed Aug 2017, had not been used for a while


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
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


  Classes,SysUtils,
  Petmar_Types;

type
  extract_basins = class(TThread)
  private
    { Private declarations }
     procedure UpdateThreadProgress;
  protected
    BasinsName,RiversName,OutPath : PathStr;
    ProgressBar,PerCentDone : integer;
    BasinCode : shortstring;
  public
     constructor Create(inBasinCode : shortstring; inBasinName,InRiversName,inOutPath : PathStr; inProgressBar : integer);
     procedure Execute; override;
  end;

implementation


{ extract_basins }

uses
   DEMDataBase,PetDBUtils,Thread_Timers,Petmar;


constructor extract_basins.Create(inBasinCode : shortstring; inBasinName,InRiversName,inOutPath : PathStr; inProgressBar : integer);
begin
    inherited Create(false);
    BasinsName := inBasinName;
    RiversName := inRiversName;
    BasinCode := inBasinCode;
    OutPath := inOutPath;
    ProgressBar := inProgressBar;
    FreeOnTerminate := true;   //not DoProgress;
end;


procedure extract_basins.UpdateThreadProgress;
begin
    ThreadTimers.UpdateThreadStats(ProgressBar,PercentDone);
end;


procedure extract_basins.Execute;
var
   RiversDB,i : integer;
   TStr : shortString;
   fName : PathStr;
   Table : tMyData;
begin
  { Place thread code here }
  if OpenNumberedGISDataBase(RiversDB,RiversName) then begin
       Table := tMyData.Create(BasinsName);
       i := 0;
       while not Table.eof do begin
          inc(i);
          if (i mod 25 = 0) then begin
             PercentDone := round(100 * i/Table.RecordCount);
             Synchronize(UpdateThreadProgress);
          end;
          TStr := Table.GetFieldByNameAsString('BASIN_ID');
          if (TStr <> '') then begin
             TStr := BasinCode + '_'+ TStr;
             fName := OutPath + TStr + DefaultDBExt;
             if not FileExists(fName) then begin
                GISdb[RiversDB].MyData.ApplyFilter( 'BASIN_ID=' + QuotedStr(TStr));
                if GISdb[RiversDB].MyData.RecordCount > 0 then
                   GISdb[RiversDB].SubsetShapeFile(fName,1,true);
             end;
          end;
          Table.Next;
          if CeaseThreads then break;
       end;
       Table.Destroy;
       CloseAndNilNumberedDB(RiversDB);
  end;
end;



end.
