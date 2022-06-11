unit update_files;

no longer used

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordUpdateFiles}
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Menus,WinSock,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
   ToolWin,UrlMon,SysUtils,
  petmar_types,DEMMapf;

type
  TSDIAppForm = class(TForm)
    Memo1: TMemo;
  private
    { Private declarations }
     procedure DownLoadStatus(Progress, ProgressMax, StatusCode: cardinal; StatusText: string;  var Abort : boolean);
  public
    { Public declarations }
     FileDone : boolean;
     function DownLoadFileHTTP(FarName,LocalName : string) : boolean;
  end;



implementation

uses
   //web_download,
   DEMDefs,DEMDef_routines,petmar, Petdbutils,
{$IfDef ExSats}
{$Else}
   DEMEROSM,
{$EndIf}
   DEM_manager,
   nevadia_main;

{$R *.dfm}




procedure TSDIAppForm.DownLoadStatus(Progress, ProgressMax, StatusCode: cardinal; StatusText: string;  var Abort : boolean);
begin
end;


function TSDIAppForm.DownLoadFileHTTP(FarName,LocalName : string) : boolean;
begin
end;


initialization
finalization
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing update_files in', true);
   {$EndIf}

{$IfDef RecordUpdateFiles}
   WriteLineToDebugFile('RecordUpdateFiles active in update_files');
{$EndIf}
end.
