unit DataBaseAddRec;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordAddRecProblems}
{$EndIf}

{$I nevadia_defines.inc}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end inline for core DB functions

   Windows, Messages, SysUtils, Classes, Graphics, Controls,Forms,Dialogs,
   StdCtrls, Mask, DBCtrls, ExtCtrls, Grids, DBGrids, Buttons,   //ComCtrls,
   PETMAR,Petmar_types,DEMESRIShapeFile;


type
  TDbaddRecForm = class(TForm)
    DataSource1: TDataSource;
    Panel1: TPanel;
    Button1: TButton;
    HelpBtn: TBitBtn;
    DBGrid1: TDBGrid;
    Button2: TButton;
    CheckBox1: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure WriteRecContinueDigitButton(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
  private
    { Private declarations }
  public
     Table1 : tMyData;
     DEMused : integer;
     ShapeFileCreator : tShapeFileCreation;
     LastFName : shortString;
     procedure CompleteAddingPoints;
     procedure AddPointRecord(Lat,Long : float64; ContinueOn : boolean = false);
  end;

var
  DbaddRecForm : TDbaddRecForm;


procedure EditTable(FName : PathStr; ShapeFileType : integer; DEM : integer = 0);


implementation

uses
   {$IfDef ExSlicer3D}
   {$Else}
      Slicer_3D,
   {$EndIf}
   Nevadia_Main,
   DEMMapf,
   BaseMap,
   DEMCoord,
   PetMath,
   DEMDefs,
   PetDBUtils;

{$R *.DFM}


procedure EditTable(FName : PathStr; ShapeFileType : integer; DEM : integer = 0);
begin
   {$IfDef RecordAddRecProblems} WriteLineToDebugFile('EditTable start ' + FName);     {$EndIf}
   DbaddRecForm := TDbaddRecForm.Create(Application);
   DbaddRecForm.ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,false,ShapeFileType);
   DBAddRecForm.Table1 := tMyData.Create(fName);
   DbaddRecForm.Table1.AssignEmpSource(DbaddRecForm.DataSource1);
   DbaddRecForm.DEMused := DEM;
   {$IfDef RecordAddRecProblems} WriteLineToDebugFile('EditTable success'); {$EndIf}
end;


procedure TDbaddRecForm.AddPointRecord(Lat,Long : float64; ContinueOn : boolean = false);
var
   z : float32;
begin
   LongitudeAngleInRange(Long);
   Table1.First;
   Table1.Insert;
   if DbaddRecForm.Table1.FieldExists('ID') then begin
      Table1.SetFieldByNameAsInteger('ID',succ(ShapeFileCreator.RecsInShapeStream));
   end;
   Table1.SetFieldByNameAsString('LAT',RealToString(Lat,-12,-7));
   Table1.SetFieldByNameAsString('LONG',RealToString(Long,-12,-7));
   if (DEMNowDoing = ShapeXYZPoint) then begin
      if (DEMused <> 0) and DEMGlb[DEMUsed].GetElevFromLatLongDegree(Lat,Long,z) then begin
         Table1.SetFieldByNameAsFloat('Z',z);
      end;
   end
   else if CreateTrainingSet then begin
      Table1.SetFieldByNameAsString('NAME',LastFName);
   end;
   ShapeFileCreator.ProcessPointForShapeFile(Lat,Long);
   if ContinueOn then Show
   else ShowModal;
   Petmath.CompareValueToExtremes(Lat,ShapeFileCreator.glMainFileHeader.BoundBox.YMin,ShapeFileCreator.glMainFileHeader.BoundBox.YMax);
   Petmath.CompareValueToExtremes(Long,ShapeFileCreator.glMainFileHeader.BoundBox.xMin,ShapeFileCreator.glMainFileHeader.BoundBox.xMax);
end;


procedure TDbaddRecForm.WriteRecContinueDigitButton(Sender: TObject);
begin
   LastFName := Table1.GetFieldByNameAsString('NAME');
   Table1.Post;
   if (Sender = Button1) then begin
   end
   else begin
      ChangeDEMNowDoing(JustWandering);
      Table1.Destroy;
      ShapeFileCreator.CloseShapeFiles;
      LockMaps := false;
      {$IfDef RecordAddRecProblems} WriteLineToDebugFile('TDbaddRecForm.WriteRecContinueDigitButton done and close'); {$EndIf}
   end;
   Close;
end;


procedure TDbaddRecForm.Button2Click(Sender: TObject);
begin
   WriteRecContinueDigitButton(Sender);
end;


procedure TDbaddRecForm.CompleteAddingPoints;
begin
   WriteRecContinueDigitButton(Nil);
end;

procedure TDbaddRecForm.DataSource1DataChange(Sender: TObject; Field: TField);
begin
   Button1.Enabled := true;
   Button2.Enabled := true;
end;

procedure TDbaddRecForm.DBGrid1CellClick(Column: TColumn);
begin
   Button1.Enabled := true;
   Button2.Enabled := true;
end;

procedure TDbaddRecForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\database_add_record.htm');
end;


procedure TDbaddRecForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordAddRecProblems} WriteLineToDebugFile('TDbaddRecForm.FormCreate');  {$EndIf}
   CheckFormPlacement(Self);
   LastFName := '';
end;


initialization
finalization
   {$IfDef RecordAddRecProblems} WriteLineToDebugFile('RecordAddRecProblems active in DataBaseAddRec'); {$EndIf}
end.










