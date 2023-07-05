unit Demeditw;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordEditProblems}
{$EndIf}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,SysUtils,
   StdCtrls, ExtCtrls, Grids, Menus;

type
  TDEMeditForm = class(TForm)
    HelpBtn: TBitBtn;
    StringGrid1: TStringGrid;
    MainMenu1: TMainMenu;
    Editmode1: TMenuItem;
    Delete1: TMenuItem;
    Replace1: TMenuItem;
    Map1: TMenuItem;
    Update1 : TMenuItem;
    Showwindow1: TMenuItem;
    N1: TMenuItem;
    Deletepoints1: TMenuItem;
    Entirewindow1: TMenuItem;
    Column1: TMenuItem;
    Row1: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    Clickoptions1: TMenuItem;
    Slopealgorithms1: TMenuItem;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Delete1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Update1Click(Sender: TObject);
    procedure Showwindow1Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure HelpBtnClick(Sender: TObject);
    procedure Entirewindow1Click(Sender: TObject);
    procedure Column1Click(Sender: TObject);
    procedure Row1Click(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure Slopealgorithms1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Col,Row,LastCol,LastRow,
    EditDEM,NumDec,
    SubsetLeftCol,SubsetBottRow : integer;
    AllowEdit : boolean;
    procedure FillInGrid;
    procedure ChangeValue(Col,Row : integer; Ask : boolean = true);
  end;


procedure ShowAndEditDEMGrid(var GridForm : TDEMeditForm;  DEM : integer);


implementation

{$R *.DFM}
               
uses
   Nevadia_Main,
   PETMAR, Petmar_types,
   DEMDefs,DEMDef_routines,DEMCoord,DEMMapF;

var
   Lastx,lasty : integer;


procedure ShowAndEditDEMGrid(var GridForm : TDEMeditForm;  DEM : integer);
begin
   if (GridForm = Nil) then begin
      GridForm := TDEMeditForm.Create(Application);
      GridForm.Left := wmdem.ClientWidth - GridForm.Width - 3;
      GridForm.Top := 0;
      GridForm.EditDEM := DEM;
      if (DEM <> 0) then begin
         DEMGlb[DEM].SelectionMap.EditGridButton.Visible := true;
         DEMGlb[DEM].SelectionMap.MapDraw.MapOwner := moEditMap;
         DEMGlb[DEM].SelectionMap.CheckProperTix;
         DEMGlb[DEM].SelectionMap.EraserSize1.Visible := true;
         DEMGlb[DEM].SelectionMap.Edit1.Visible := true;
         if (DEMGlb[DEM].DEMheader.DEMPrecision <> FloatingPointDEM) then begin
            GridForm.NumDec := 0;
            GridForm.BitBtn1.Visible := false;
         end;
      end;
   end;
   if (DEM <> 0) then ChangeDEMNowDoing(CornerEditBox);
end;


procedure TDEMeditForm.FillInGrid;
var
   i,j : integer;
   z : float32;
begin
   with DEMGlb[EditDEM] do begin
      Caption := 'DEM: ' + AreaName + ' ' + ElevUnitsAre(DEMheader.ElevUnits);

      if (SubsetLeftCol < 0) then SubsetleftCol := 0;
      if (SubsetBottRow < 0) then SubsetBottRow := 0;
      if SubsetLeftCol + StringGrid1.ColCount > pred(DEMheader.NumCol) then StringGrid1.ColCount := succ(DEMheader.NumCol) - SubsetLeftCol;
      if SubsetBottRow + StringGrid1.RowCount > pred(DEMheader.NumRow) then StringGrid1.RowCount := succ(DEMheader.NumRow) - SubsetBottRow;

      StringGrid1.Cells[0,0] := 'Row\Col';
      for i := 0 to (StringGrid1.ColCount - 2) do
         for j := 0 to (StringGrid1.RowCount - 2) do begin
            if GetElevMeters(i + SubsetLeftCol,SubsetBottRow + (StringGrid1.RowCount-2) - j,z) then begin
               if (not CheckBox1.Checked) or (abs(z) > 0.001) then begin
                  StringGrid1.Cells[succ(i),succ(j)] := RealToString(z,-8,NumDec);
               end
               else StringGrid1.Cells[succ(i),succ(j)] := '';
            end
            else StringGrid1.Cells[succ(i),succ(j)] := '';
         end;
      for i := 1 to pred(StringGrid1.RowCount) do StringGrid1.Cells[0,i] := IntToStr(SubsetBottRow + (StringGrid1.RowCount - 1) - i);
      for i := 1 to pred(StringGrid1.ColCount) do StringGrid1.Cells[i,0] := IntToStr(SubsetLeftCol + i - 1);
   end {with};
end;


procedure TDEMeditForm.FormCreate(Sender: TObject);
begin
   SubsetLeftCol := 0;
   SubsetBottRow := 0;
   LastCol := -99;
   LastRow := -99;
   NumDec := 3;
   AllowEdit := true;
end;

procedure TDEMeditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Self := Nil;
   Action := caFree;
end;

procedure TDEMeditForm.Delete1Click(Sender: TObject);
begin
   Delete1.Checked := Sender = Delete1;
   Replace1.Checked := Sender = Replace1;
   DEMGlb[EditDEM].SelectionMap.Delete1.Checked := Delete1.Checked;
   DEMGlb[EditDEM].SelectionMap.Replace1.Checked := Replace1.Checked;
   DEMGlb[EditDEM].SelectionMap.EraserSize1.Visible := Delete1.Checked;
   DEMGlb[EditDEM].SelectionMap.CheckProperTix;
end;

procedure TDEMeditForm.Replace1Click(Sender: TObject);
begin
   Delete1Click(Sender);
end;

procedure TDEMeditForm.Update1Click(Sender: TObject);
begin
   DEMGlb[EditDEM].CheckMaxMinElev;
   DEMGlb[EditDEM].SelectionMap.DoBaseMapRedraw;
end;


procedure TDEMeditForm.ShowWindow1Click(Sender: TObject);
begin
   with DEMGlb[EditDEM],SelectionMap,DEMheader,StringGrid1 do
      ShowSubsetOnMap(SubsetLeftCol,SubsetBottRow+RowCount,SubsetLeftCol + ColCount,SubsetBottRow);
end;


procedure TDEMeditForm.BitBtn1Click(Sender: TObject);
begin
   ReadDefault('Decimal places',NumDec);
end;

procedure TDEMeditForm.ChangeValue(Col,Row : integer; Ask : boolean = true);
var
   z : float32;
begin
   if AllowEdit then begin
      {$IfDef RecordEditProblems}
         GetElevMeters(Col,Row,z);
         WriteLineToDebugFile('ChangeValue, col=' + IntToStr(Col) +  '  row=' + IntToStr(Row) + '  z=' + RealToString(z,-18,-3),  true);
      {$EndIf}
      if Ask then begin
        if Replace1.Checked then begin
           DEMGlb[EditDEM].GetElevMeters(Col,Row,z);
           ReadDefault('New z value',z);
           {$IfDef RecordEditProblems}   WriteLineToDebugFile('entered z=' +  RealToString(z,-18,-3));   {$EndIf}
           DEMGlb[EditDEM].SetGridElevation(Col,Row,z);
           {$IfDef RecordEditProblems}
               GetElevMeters(Col,Row,z);
               WriteLineToDebugFile('new DEM z=' +  RealToString(z,-18,-3));
           {$EndIf}
        end
        else DEMGlb[EditDEM].SetGridMissing(Col,Row);
        FillInGrid;
      end;
      DEMGlb[EditDEM].DEMstatus := dsUnsaved;
   end {with};
end;


procedure TDEMeditForm.CheckBox1Click(Sender: TObject);
begin
   FillInGrid;
end;

procedure TDEMeditForm.StringGrid1DblClick(Sender: TObject);
begin
   if AllowEdit then ChangeValue(Col,Row);
end;


procedure TDEMeditForm.StringGrid1GetEditText(Sender: TObject; ACol,  ARow: Integer; var Value: string);
var
   Err : integer;
   z : float64;
begin
   if AllowEdit then begin
      Val(StringGrid1.Cells[Acol,aRow],z,err);
      if (err = 0) then begin
          DEMGlb[EditDEM].SetGridElevation(Col,Row,z);
          ChangeValue(Col,Row,false);
          FillInGrid;
          Update1Click(nil);
      end;
   end;
end;

procedure TDEMeditForm.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   Lastx := x;
   Lasty := y;
   StringGrid1.MouseToCell(LastX,Lasty,Col,Row);
   if (Col > 0) and (Row > 0) then begin
      Col := pred(Col) + SubsetLeftCol;
      Row := SubsetBottRow + (StringGrid1.RowCount) - pred(Row) - 2;
      if (EditDEM <> 0) and ((Col <> LastCol) or (Row <> LastRow)) then begin
         Label1.Caption := DEMGlb[EditDEM].DEMLocationString(Col,Row);
         DEMGlb[EditDEM].DEMGridToLatLongDegree(Col,Row,LastRoamLat,LastRoamLong);
         BroadcastLatLong(Self.Handle,LastRoamLat,LastRoamLong);
      end;
      LastCol := Col;
      LastRow := Row;
   end;
end;


procedure TDEMeditForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme6ibb.htm');
end;


procedure TDEMeditForm.Entirewindow1Click(Sender: TObject);
var
   Col,Row : integer;
begin
   if AllowEdit then begin
      if Sender = Entirewindow1 then begin
         for Col := SubSetLeftCol to SubSetLeftCol + pred(StringGrid1.ColCount) do begin
            if (Col < DEMGlb[EditDEM].DEMheader.NumCol) then for Row := SubsetBottRow to SubsetBottRow + pred(StringGrid1.RowCount) do begin
               if (Row < DEMGlb[EditDEM].DEMheader.NumRow) then DEMGlb[EditDEM].SetGridMissing(Col,Row);
            end;
         end;
      end;
      if Sender = Column1 then begin
         if StringGrid1.Col > 1 then begin
            Col := SubSetLeftCol + pred(StringGrid1.Col);
            if (Col < DEMGlb[EditDEM].DEMheader.NumCol) then begin
               for Row := SubsetBottRow to SubsetBottRow + pred(StringGrid1.RowCount) do
                  if (Row < DEMGlb[EditDEM].DEMheader.NumRow) then DEMGlb[EditDEM].SetGridMissing(Col,Row);
            end;
         end;
      end;
      if Sender = Row1 then begin
         if StringGrid1.Row > 1 then begin
            Row := SubsetBottRow + pred(StringGrid1.RowCount) - StringGrid1.Row;
            if (Row < DEMGlb[EditDEM].DEMheader.NumRow) then begin
               for Col := SubSetLeftCol to SubSetLeftCol + pred(StringGrid1.ColCount) do
                  if (Col < DEMGlb[EditDEM].DEMheader.NumCol) then DEMGlb[EditDEM].SetGridMissing(Col,Row);
            end;
         end;
      end;
      FillInGrid;
      Update1Click(nil);
      DEMGlb[EditDEM].DEMstatus := dsUnsaved;
   end;
end;


procedure TDEMeditForm.Column1Click(Sender: TObject);
begin
   Entirewindow1Click(Sender)
end;

procedure TDEMeditForm.Row1Click(Sender: TObject);
begin
   Entirewindow1Click(Sender)
end;


procedure TDEMeditForm.StringGrid1Click(Sender: TObject);
{$IfDef ExDEMReports}
begin
{$Else}
var
  znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
begin
   if Slopealgorithms1.Checked then
      if DEMGlb[EditDEM].SurroundedPointElevs(LastCol,LastRow,znw,zw,zsw,zn,z,zs,zne,ze,zse) then
         DEMGlb[EditDEM].SlopeMethodsReport(LastCol,LastRow);
{$EndIf}
end;


procedure TDEMeditForm.Slopealgorithms1Click(Sender: TObject);
begin
   Slopealgorithms1.Checked := not Slopealgorithms1.Checked;
end;


initialization
finalization
   {$IfDef RecordEditProblems} WriteLineToDebugFile('RecordEditProblem active in demeditw'); {$EndIf}
end.




