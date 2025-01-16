unit lvis_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordLVIS}
{$EndIf}


interface

uses
//need for inline of core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

  System.Math,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  Dialogs, StdCtrls, Buttons,
  DEMMapf, BaseGraf,LVIS;

type
  Tlvis_form1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    Memo1: TMemo;
    BitBtn8: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BaseMap : tMapForm;
    ReturnGraph : TThisBaseGraph;
    LVIS : tLVIS_Dataset;
    ReturnNumber : integer;
    procedure Open(inMap : tMapForm);
  end;

var
  lvis_form1: Tlvis_form1;

implementation

uses
  Petmar,Petmar_types,
  PetDBUtils,Make_tables, demdefs, BaseMap,slicer_3d;

{$R *.dfm}

procedure Tlvis_form1.BitBtn1Click(Sender: TObject);
begin
   BaseMap.DoFastMapRedraw;
   LVIS.PlotCanopyElevationOnMap;
end;

procedure Tlvis_form1.BitBtn2Click(Sender: TObject);
begin
   BaseMap.DoFastMapRedraw;
   LVIS.PlotGroundElevationOnMap;
end;

procedure Tlvis_form1.BitBtn3Click(Sender: TObject);
begin
   BaseMap.MapDraw.MaximizeLatLongMapCoverage(LVIS.BoundBox.YMin,LVIS.BoundBox.XMin,LVIS.BoundBox.YMax,LVIS.BoundBox.XMax);
   BaseMap.DoCompleteMapRedraw;
   LVIS.PlotCanopyElevationOnMap;
end;

procedure Tlvis_form1.BitBtn4Click(Sender: TObject);
begin
   {$IfDef RecordLVIS} WriteLineToDebugFile('Tlvis_form1.BitBtn4Click (graph return)'); {$EndIf}
   LVIS.GraphReturn(ReturnGraph,ReturnNumber);
end;


procedure Tlvis_form1.BitBtn5Click(Sender: TObject);
begin
  if ReturnNumber < LVIS.NumRecs then inc(ReturnNumber);
  BitBtn4Click(Sender);
end;

procedure Tlvis_form1.BitBtn6Click(Sender: TObject);
begin
  if ReturnNumber > 0 then dec(ReturnNumber);
  BitBtn4Click(Sender);
end;

procedure Tlvis_form1.BitBtn7Click(Sender: TObject);
begin
   {$IfDef RecordLVIS} WriteLineToDebugFile('Tlvis_form1.BitBtn7Click (pulse stats)'); {$EndIf}
   LVIS.PulseStats;
end;

procedure Tlvis_form1.BitBtn8Click(Sender: TObject);
var
   NewName2 : PathStr;
   ProjectTable : tMyData;
   xutm,yutm : float64;
begin
   {$IfDef RecordLVIS}   WriteLineToDebugFile('Tlvis_form1.BitBtn8Click (slices)'); {$EndIf}
   NewName2 := Petmar.NextFileNumber(MDTempDir, 'Proj_temp_subset_',DefaultDBExt);
   Make_Tables.CreatPointCloudProjectTable(NewName2);
   ProjectTable := tMyData.Create(NewName2);
   ProjectTable.Insert;
   ProjectTable.SetFieldByNameAsInteger('COLOR',clRed);
   ProjectTable.SetFieldByNameAsString('NAME',ExtractFilename(lvis.lceFName));
   ProjectTable.SetFieldByNameAsString('FILENAME',lvis.lceFName);
   ProjectTable.SetFieldByNameAsString('USE','Y');
   ProjectTable.SetFieldByNameAsString('TYPE','geometry');
   ProjectTable.SetFieldByNameAsInteger('SYM_SIZE',2);

   RedefineWGS84DatumConstants(0.5 * (lvis.BoundBox.XMin + lvis.BoundBox.XMax));

   WGS84DatumConstants.ForwardProjectDegrees(lvis.BoundBox.YMin,lvis.BoundBox.XMin,xutm,yutm);
   ProjectTable.SetFieldByNameAsFloat('MIN_X',xutm);
   ProjectTable.SetFieldByNameAsFloat('MIN_Y',yutm);

   WGS84DatumConstants.ForwardProjectDegrees(lvis.BoundBox.YMax,lvis.BoundBox.XMax,xutm,yutm);
   ProjectTable.SetFieldByNameAsFloat('MAX_X',xutm);
   ProjectTable.SetFieldByNameAsFloat('MAX_Y',yutm);

   ProjectTable.SetFieldByNameAsFloat('MIN_Z',lvis.GroundZMin);
   ProjectTable.SetFieldByNameAsFloat('MAX_Z',lvis.CanopyZMax);

   ProjectTable.Post;
   DEMDefs.VasaProjectFName := NewName2;
   {$IfDef RecordLVIS} WriteLineToDebugFile('calling Slicer_3D.DB_3dSlices'); {$EndIf}
   Slicer_3D.DB_3dSlices(BaseMap,Nil,Nil);
end;

procedure Tlvis_form1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   LVIS.Destroy;
end;

procedure Tlvis_form1.Open(inMap: tMapForm);
var
   fName : PathStr;
begin
   BaseMap := inMap;
   ReturnGraph := Nil;
   ReturnNumber := 0;
   fName := '';
   LVIS := tLVIS_Dataset.Create(fname,inMap);
   if fName = '' then begin
      Close;
   end
   else begin
      Show;
      BitBtn3Click(Nil);
      Memo1.Lines.Add(ExtractFileName(LVIS.lceFName));
      Memo1.Lines.Add('Records: ' + IntToStr(LVIS.NumRecs));
   end;
end;


initialization
finalization
   {$IfDef RecordLVIS} WriteLineToDebugFile('RecordLVIS active in lvis_form'); {$EndIf}
end.
