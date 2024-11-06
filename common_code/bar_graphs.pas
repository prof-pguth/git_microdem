unit bar_graphs;

//disabled 12 June 2023
//it appears to have been hard coded

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   //{$Define BarGraphPlot}
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
// end units for inline of the core DB functions

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,
  DEMDataBase,BaseGraf,Petmar_types, Buttons, StdCtrls;

type
  Tbargraphform = class(TForm)
    RedrawSpeedButton12: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    SpeedButton2: TSpeedButton;
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
     GIS : TGISdataBaseModule;
     Graph,Graph2,Graph3 : TThisBaseGraph;
     BarHeight,Separator : integer;
     SeriesField,LabelName,DataField : ShortString;
     procedure DrawGraph;
     procedure DrawGraph2;
  public
    { Public declarations }
  end;


procedure MakeBarGraph(inGIS : TGISdataBaseModule);


implementation

{$R *.dfm}

uses
   Petmar,PetDBUtils,Nevadia_main, PETMath,PetImage;


procedure MakeBarGraph(inGIS : TGISdataBaseModule);
var
  bargraphform : Tbargraphform;
  TheFieldsInDB : tStringList;
begin
  bargraphform := Tbargraphform.Create(Application);
  with bargraphform do begin
     wmdem.FormPlacementInCorner(BarGraphForm);
     GIS := inGIS;
     if GIS.MyData.FieldExists('AREA') then LabelName := 'AREA'
     else if GIS.MyData.FieldExists('NAME') then LabelName := 'NAME';
     DataField := 'SLOPE_AVG';
     SeriesField := 'ASTER_GDEM';

     BarHeight := 5;
     Separator := 3;
     Edit1.Text := IntToStr(BarHeight);
     Edit2.Text := IntToStr(Separator);
     PetDBUtils.GetFields(GIS.MyData,GIS.dbOpts.VisCols,NumericFieldTypes,TheFieldsInDB);
     ComboBox1.Text := DataField;
     ComboBox1.Items := TheFieldsInDB;
     BarGraphForm.Show;
  end;
end;


procedure Tbargraphform.BitBtn1Click(Sender: TObject);
var
   ht : integer;
begin
   ht := Graph.Height;
   ReadDefault('From height',ht);
   Graph.Height := ht;
   Graph.FormResize(nil);
   RedrawSpeedButton12Click(Sender);
end;

procedure Tbargraphform.BitBtn2Click(Sender: TObject);
begin
   GIS.dbOpts.LabelField := GIS.PickField('unique values' ,[ftString,ftInteger,ftSmallInt]);
   GIS.dbtablef.Legend1Click(Nil);
end;

procedure Tbargraphform.BitBtn3Click(Sender: TObject);
begin
   EditMyFont(GIS.dbOpts.GisLabelFont1);
end;

procedure Tbargraphform.BitBtn4Click(Sender: TObject);
var
   v    : array[1..5] of float64;
   i,j  : integer;
   aField : ShortString;
   Results : tStringList;
   GDEMBigger : array[1..5] of integer;
begin
   GIS.EmpSource.Enabled := false;
   Results := tStringList.Create;
   with GIS,MyData do begin
      for I := 0 to pred(ComboBox1.Items.Count) do begin
        afield := ComboBox1.Items[i];
        for j := 1 to 5 do GDEMBigger[j] := 0;
        First;
        while not eof do begin
           for j := 1 to 5 do begin
              v[j] := GetFieldByNameAsFloat(aField);
              Next;
           end;
           for j := 2 to 5 do if v[j] < v[1] then inc(GDEMBigger[j]);
        end;
        Results.Add(afield + RealtoString(100.0 * GDEMBigger[2]/52,8,1) + RealtoString(100.0 * GDEMBigger[3]/52,8,1) + RealtoString(100.0 * GDEMBigger[4]/52,8,1)+ RealtoString(100.0 * GDEMBigger[5]/52,8,1));
      end;
   end;
   GIS.EmpSource.Enabled := true;
   Petmar.DisplayAndPurgeStringList(Results,'GDEM');
end;


procedure Tbargraphform.ComboBox1Change(Sender: TObject);
begin
   DataField := ComboBox1.Text;
   if (Graph <> Nil) then begin
      Graph.GraphDraw.HorizLabel := DataField;
      GIS.FieldRange32(DataField,Graph.GraphDraw.MinHorizAxis,Graph.GraphDraw.MaxHorizAxis);
      RedrawSpeedButton12Click(Sender);
      DrawGraph;
   end;

   if (Graph2 <> Nil) then begin
      Graph2.GraphDraw.HorizLabel := DataField;
      GIS.FieldRange32(DataField,Graph2.GraphDraw.MinHorizAxis,Graph2.GraphDraw.MaxHorizAxis);
      DrawGraph2;
   end;
end;


procedure Tbargraphform.DrawGraph;
var
   Top : integer;
   xf : float64;
   LastName,ThisName : ShortString;
begin
   CheckEditString(Edit1.Text,BarHeight);
   CheckEditString(Edit2.Text,Separator);
   if (Graph = Nil) then begin
      Graph := TThisBaseGraph.Create(Application);
      with Graph,GraphDraw do begin
         GraphAxes := XFullGridOnly;
         ScrollGraph := true;
         GIS.FieldRange32(DataField,Graph.GraphDraw.MinHorizAxis,Graph.GraphDraw.MaxHorizAxis);
         Graph.GraphDraw.HorizLabel := DataField;
         SetUpGraphForm;
      end;
   end;

   Graph.Image1.Height := BarHeight * GIS.MyData.RecordCount +  Graph.GraphDraw.TopMargin +  Graph.GraphDraw.BottomMargin;
   GIS.EmpSource.Enabled := false;
   with GIS,MyData do begin
      First;
      Top := 0;
      LastName := '';
      while not eof do begin
         ThisName := GetFieldByNameAsString(LabelName);
         if (ThisName <> LastName) then begin
            if (Top <> 0) then inc(Top,Separator);
            Graph.Image1.Canvas.Brush.Style := bsClear;
            Graph.Image1.Canvas.TextOut(4,Top,ThisName);
            LastName := ThisName;
         end;
         Graph.Image1.Canvas.Pen.Color := GetFieldByNameAsInteger('COLOR');
         Graph.Image1.Canvas.Brush.Color := GetFieldByNameAsInteger('COLOR');
         Graph.Image1.Canvas.Brush.Style := bsSolid;
         xf := GetFieldByNameAsFloat(DataField);
         {$IfDef BarGraphPlot} WriteLineToDebugFile(ThisName + '  ' + RealToString(xf,12,2) + '  ' + IntToStr(Graph.GraphDraw.GraphX(xf))); {$EndIf}

         Graph.Image1.Canvas.Rectangle(succ(Graph.GraphDraw.LeftMargin),Top,Graph.GraphDraw.GraphX(xf),Top+pred(BarHeight));
         inc(Top,BarHeight);
         Next;
      end;
   end;
   GIS.EmpSource.Enabled := true;
end;


procedure Tbargraphform.DrawGraph2;
var
   rfile : file;
   LastName,ThisName : ShortString;
   v : array[1..3] of float64;
begin
   if (Graph2 = Nil) then begin
      Graph2 := TThisBaseGraph.Create(Application);
      with Graph2,GraphDraw do begin
          SetUpGraphForm;
          if CheckBox1.Checked then begin
             Graph2.GraphDraw.MinHorizAxis := 0.1;
             Graph2.GraphDraw.MaxHorizAxis := 100;
             Graph2.GraphDraw.MinVertAxis := 0.1;
             Graph2.GraphDraw.MaxVertAxis := 100;
             HorizAxisFunct := Petmath.Log10;
             VertAxisFunct := Petmath.Log10;
             Graph2.GraphDraw.HorizAxisFunctionType := Log10Axis;
             Graph2.GraphDraw.VertAxisFunctionType := Log10Axis;
          end
          else begin
             GIS.FieldRange32(DataField,Graph2.GraphDraw.MinHorizAxis,Graph2.GraphDraw.MaxHorizAxis);
          end;
          GraphDraw.HorizLabel := DataField;
      end;
   end;
   Graph2.GraphDraw.XYColorFilesPlotted.Clear;
   Graph2.OpenXYColorFile(rfile);
   Graph2.RedrawDiagram11Click(Nil);
   GIS.EmpSource.Enabled := false;
   with GIS,MyData do begin
      First;
      LastName := '';
      while not eof do begin
         ThisName := GetFieldByNameAsString(LabelName);
         if (ThisName <> LastName) then begin
            v[1] := GetFieldByNameAsFloat(DataField);
            LastName := ThisName;
         end
         else begin;
            v[3] := GetFieldByNameAsInteger('COLOR');
            v[2] := GetFieldByNameAsFloat(DataField);
            BlockWrite(rfile,v,1);
         end;
         Next;
      end;
      CloseFile(rFile);
      if CheckBox1.Checked then  Graph2.RedrawDiagram11Click(Nil)
      else Graph2.AutoScaleAndRedrawDiagram;
   end;
   GIS.EmpSource.Enabled := true;
end;


procedure Tbargraphform.FormCreate(Sender: TObject);
begin
   Graph := Nil;
   Graph2 := Nil;
   Graph3 := Nil;
   GIS := Nil;
end;

procedure Tbargraphform.RedrawSpeedButton12Click(Sender: TObject);
begin
   DrawGraph;
end;

procedure Tbargraphform.SpeedButton1Click(Sender: TObject);
begin
   DrawGraph2;
end;


procedure Tbargraphform.SpeedButton2Click(Sender: TObject);
begin
    if (Graph3 = Nil) then begin
        Graph3 := TThisBaseGraph.Create(Application);
        Graph3.SetUpGraphForm;
        GIS.PickNumericFields(dbgtUnspecified,2,'X axis','Y axis','x');
        GIS.FieldRange32(GIS.dbOpts.XField,Graph3.GraphDraw.MinHorizAxis,Graph3.GraphDraw.MaxHorizAxis);
        GIS.FieldRange32(GIS.dbOpts.YField,Graph3.GraphDraw.MinVertAxis,Graph3.GraphDraw.MaxVertAxis);
        Graph3.GraphDraw.HorizLabel := GIS.dbOpts.XField;
        Graph3.GraphDraw.VertLabel := GIS.dbOpts.YField;
    end;
   Graph3.RedrawDiagram11Click(Nil);
   GIS.EmpSource.Enabled := false;
   GIS.MyData.First;
   while not GIS.MyData.eof do begin
      Petmar.ScreenSymbol(Graph3.Image1.Canvas,  Graph3.GraphDraw.GraphX(GIS.MyData.GetFieldByNameAsFloat(GIS.dbOpts.XField)),
           Graph3.GraphDraw.GraphY(GIS.MyData.GetFieldByNameAsFloat(GIS.dbOpts.YField)),tDrawingSymbol(GIS.MyData.GetFieldByNameAsInteger('SYM_TYPE')),
           GIS.MyData.GetFieldByNameAsInteger('SYM_SIZE'),ConvertTColorToPlatformColor(GIS.MyData.GetFieldByNameAsInteger('SYM_COLOR')) );
       GIS.MyData.Next;
   end;
   GIS.EmpSource.Enabled := true;
end;



end.
