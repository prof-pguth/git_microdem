unit demxyzdisplay;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define XYZDigitizeProblems}
{$EndIf}


interface


uses
//needed for inline of core DB functions
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
//end for inline of the core DB functions


  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  System.Math,
  Dialogs, Grids, DBGrids, ComCtrls, StdCtrls, ExtCtrls, Menus,
  Petmar_types,PETMAR,PETMath,DEMMapF;

  
type
  TXYZDisplayForm = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Opennew1: TMenuItem;
    Append1: TMenuItem;
    Close1: TMenuItem;
    Panel1: TPanel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Digitizemode1: TMenuItem;
    Stream1: TMenuItem;
    Point1: TMenuItem;
    Zentry1: TMenuItem;
    Editbox1: TMenuItem;
    DEM1: TMenuItem;
    DEMconstant1: TMenuItem;
    Plotpoints1: TMenuItem;
    Edit3: TMenuItem;
    Dragpoints1: TMenuItem;
    Deletepoints1: TMenuItem;
    CheckBox1: TCheckBox;
    Editzvalue1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Append1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Opennew1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Stream1Click(Sender: TObject);
    procedure Point1Click(Sender: TObject);
    procedure Editbox1Click(Sender: TObject);
    procedure DEM1Click(Sender: TObject);
    procedure DEMconstant1Click(Sender: TObject);
    procedure Plotpoints1Click(Sender: TObject);
    procedure Dragpoints1Click(Sender: TObject);
    procedure Deletepoints1Click(Sender: TObject);
    procedure Editzvalue1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DisplayXYZPoints;
    procedure EnableOptions;
  public
    { Public declarations }
    XYZTableGIS : integer;
    MapOwner : tMapForm;
    AboveDEM : float64;
    procedure OpenXYZTableDisplay(fName : PathStr);
    procedure AddPoint(x,y : integer);
  end;

var
   XYZDisplayForm : TXYZDisplayForm;


implementation

{$R *.dfm}

uses
   DEMDefs,DataBaseCreate,
   DEMDatabase,
   Make_Tables,
   PetDBUtils;


procedure TXYZDisplayForm.FormCreate(Sender: TObject);
begin
   {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('TXYZDisplayForm.FormCreate'); {$EndIf}
   Edit2.Text := IntToStr(MDDef.ContDigitizeSeparation);
   Visible := true;
   DigitizeMode1.Enabled := false;
   ZEntry1.Enabled := false;
   PlotPoints1.Enabled := false;
   FormStyle := fsStayOnTop;
   Width := 300;
   Height := 200;
   AboveDEM := 2;
end;


procedure tXYZDisplayForm.AddPoint(x,y : integer);
var
   Lat,Long,z,Factor : float64;
begin
    if (MapOwner <> NIL) then {with MapOwner do} begin
       MapOwner.MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
       GISdb[XYZTableGIS].MyData.Insert;
       GISdb[XYZTableGIS].MyData.SetFieldByNameAsFloat('LAT',Lat);
       GISdb[XYZTableGIS].MyData.SetFieldByNameAsFloat('LONG',Long);
       CheckEditString(Self.Edit1.Text,z);
       if CheckBox1.Checked then Factor := 0.3046 else Factor := 1;
       GISdb[XYZTableGIS].MyData.SetFieldByNameAsFloat('Z',z * factor);
       GISdb[XYZTableGIS].MyData.Post;
       PETMAR.ScreenSymbol(MapOwner.Image1.Canvas,x,y,Box,2,claRed);
       Self.StatusBar1.Panels[0].Text := IntToStr(GISdb[XYZTableGIS].MyData.RecordCount) + ' records';
    end;
end;


procedure tXYZDisplayForm.OpenXYZTableDisplay(fName : PathStr);
begin
   {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('TXYZDisplayForm.OpenXYZTableDisplay ' + fname); {$EndIf}
   Caption := 'XYZ data: ' + ExtractFileName(fName);
   if OpenNumberedGISDataBase(XYZTableGIS,fName,false,false,MapOwner) then begin
      GISdb[XYZTableGIS].MyData.AssignEmpSource(DataSource1);
      DataSource1.Enabled := true;
      Self.StatusBar1.Panels[0].Text := IntToStr(GISdb[XYZTableGIS].MyData.RecordCount) + ' records';
      {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('Record count: ' + IntToStr(GISdb[XYZTableGIS].MyData.RecordCount)); {$EndIf}
      DisplayXYZPoints;
      OpenNew1.Visible := false;
      Append1.Visible := false;
      DigitizeMode1.Enabled := true;
      ZEntry1.Enabled := true;
      PlotPoints1.Enabled := true;
   end;
end;


procedure tXYZDisplayForm.DisplayXYZPoints;
var
   Lat,Long : float64;
   x,y : integer;
begin
   {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('tXYZDisplayForm.DisplayXYZPoints in'); {$EndIf}
      GISdb[XYZTableGIS].EmpSource.Enabled := false;
      GISdb[XYZTableGIS].MyData.First;
      while not GISdb[XYZTableGIS].MyData.Eof do begin
         if GISdb[XYZTableGIS].ValidLatLongFromTable(Lat,Long) then begin
            MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
            if MapOwner.MapDraw.OnScreen(x,y) then PETMAR.ScreenSymbol(MapOwner.Image1.Canvas,x,y,Box,2,claRed);
         end;
         Next;
      end;
      GISdb[XYZTableGIS].EmpSource.Enabled := true;
   {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('tXYZDisplayForm.DisplayXYZPoints out'); {$EndIf}
end;


procedure TXYZDisplayForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.ContDigitizeSeparation);
end;


procedure TXYZDisplayForm.Append1Click(Sender: TObject);
var
   fName : PathStr;
begin
   FName := ExtractFilePath(LastScanMapName);
   if GetFileFromDirectory('XYZ dBase file','XYZ-*' + DefaultDBExt,FName) then begin
      OpenXYZTableDisplay(fName);
      EnableOptions;
   end;
end;


procedure TXYZDisplayForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CloseAndNilNumberedDB(XYZTableGIS);
   GISdb[XYZTableGIS].theMapOwner.BackToWandering;
   Self := Nil;
end;


procedure TXYZDisplayForm.OpenNew1Click(Sender: TObject);
var
   FName : PathStr;
begin
   FName := TINDir;
   if GetFileNameDefaultExt('XYZ triples dBase table','XYZ data|XYZ-*' + DefaultDBExt,FName) then begin
      {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('TXYZDisplayForm.Opennew1Click ' + fname); {$EndIf}
      CreateLatLongZTable(fName);
      OpenXYZTableDisplay(fName);
      EnableOptions;
   end;
end;


procedure TXYZDisplayForm.EnableOptions;
begin
    DigitizeMode1.Enabled := true;
    Zentry1.Enabled := true;
    Edit3.Enabled := true;
    PlotPoints1.Enabled := true;
end;

procedure TXYZDisplayForm.Close1Click(Sender: TObject);
begin
   Close;
end;


procedure TXYZDisplayForm.Stream1Click(Sender: TObject);
begin
   Stream1.Checked := true;
   Point1.Checked := false;
   MDDef.DigitizeMode := dmStream;
   ChangeDEMNowDoing(DigitizeContourStream);
end;


procedure TXYZDisplayForm.Point1Click(Sender: TObject);
begin
   Stream1.Checked := false;
   Point1.Checked := true;
   MDDef.DigitizeMode := dmPoint;
   ChangeDEMNowDoing(DigitizeContourPoint);
end;


procedure TXYZDisplayForm.Editbox1Click(Sender: TObject);
begin
   EditBox1.Checked := true;
   Edit1.Enabled := true;
   DEM1.Checked := false;
   DEMconstant1.Checked := false;
end;


procedure TXYZDisplayForm.DEM1Click(Sender: TObject);
begin
   EditBox1.Checked := false;
   Edit1.Enabled := false;
   DEM1.Checked := true;
   DEMconstant1.Checked := false;
end;


procedure TXYZDisplayForm.DEMconstant1Click(Sender: TObject);
begin
   EditBox1.Checked := false;
   Edit1.Enabled := false;
   DEM1.Checked := false;
   DEMconstant1.Checked := true;
   ReadDefault('Height above DEM surface',AboveDEM);
end;


procedure TXYZDisplayForm.Plotpoints1Click(Sender: TObject);
begin
   DisplayXYZPoints;
end;


procedure TXYZDisplayForm.Dragpoints1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(MovePointDBRecs);
   DBEditting := XYZTableGIS;
end;


procedure TXYZDisplayForm.Deletepoints1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(DeletePointDBRecs);
   DBEditting := XYZTableGIS;
end;

procedure TXYZDisplayForm.Editzvalue1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(EditZDBRecs);
   DBEditting := XYZTableGIS;
end;

initialization
   XYZDisplayForm := Nil;
finalization
   {$IfDef XYZDigitizeProblems} WriteLineToDebugFile('XYZDigitizeProblems active in demxyzdisplay'); {$EndIf}
end.
