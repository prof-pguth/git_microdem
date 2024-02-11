unit param_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}
   //{$Define RecordPGraphs}
{$EndIf}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Data.DB,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  baseGraf;

type
  TParamGraphForm = class(TForm)
    ComboBox1: TComboBox;
    Image1: TImage;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    theDB : integer;
    ThisGraph : TThisbasegraph;
  end;


procedure ParamGraphForm(DB : integer);


implementation

{$R *.dfm}

uses
   DEMDatabase,Petmar,Petmar_types;


procedure ParamGraphForm(DB : integer);
var
  ParamGraphForm : TParamGraphForm;
  ParamList : tStringList;
  i : integer;
begin
   {$IfDef RecordPGraphs} WriteLineToDebugFile('ParamGraphForm in'); {$EndIf}
   ParamGraphForm := TParamGraphForm.Create(Application);
   ParamGraphForm.theDB := DB;
   ParamGraphForm.ThisGraph := Nil;
   ParamGraphForm.Show;
   with ParamGraphForm do begin
       GISdb[theDB].MyData.ApplyFilter('');
       GISdb[theDB].EmpSource.Enabled := false;
       ParamList := GISdb[theDB].MyData.UniqueEntriesInDB('PARAM');
       for I := 0 to pred(ParamList.Count) do begin
          {$IfDef RecordPGraphs} WriteLineToDebugFile(ParamList.Strings[i]); {$EndIf}
          ComboBox1.Items.Add(ParamList.Strings[i]);
       end;
       ParamList.Free;
       ComboBox1.Text := ComboBox1.Items[0];
       ComboBox1Change(Nil);
   end;
   {$IfDef RecordPGraphs} WriteLineToDebugFile('ParamGraphForm out');{$EndIf}
end;


procedure TParamGraphForm.BitBtn1Click(Sender: TObject);
begin
   if Sender = BitBtn1 then begin
      if ComboBox1.ItemIndex = 0 then ComboBox1.ItemIndex := pred(ComboBox1.Items.Count)
      else ComboBox1.ItemIndex := ComboBox1.ItemIndex - 1;
   end
   else begin
      if ComboBox1.ItemIndex = pred(ComboBox1.Items.Count) then ComboBox1.ItemIndex := 0
      else ComboBox1.ItemIndex := ComboBox1.ItemIndex + 1;
   end;
   ComboBox1.Text := ComboBox1.Items[ComboBox1.ItemIndex];
   ComboBox1Change(Sender);
end;


procedure TParamGraphForm.BitBtn2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TParamGraphForm.ComboBox1Change(Sender: TObject);
var
   Param : ShortString;
   rfile : file;
   bmp : tMyBitmap;

   procedure WritePair(v2,v1 : float64);
   var
      v : tGraphPoint32;
   begin
      v[1] := v1;
      v[2] := v2;
      BlockWrite(Rfile,v,1);
   end;

begin
   {$IfDef RecordPGraphs} WriteLineToDebugFile('TParamGraphForm.ComboBox1Change in'); {$EndIf}
   Param := ComboBox1.Text;
   GISdb[theDB].MyData.ApplyFilter('PARAM=' + QuotedStr(Param));
   if (ThisGraph <> Nil) then ThisGraph.Close;

   ThisGraph := TThisbasegraph.Create(Application);
   ThisGraph.GraphDraw.HorizLabel := Param;
   ThisGraph.GraphDraw.VertLabel := 'Percentage';
   ThisGraph.Caption := Param + 'v Percentage';
   ThisGraph.GraphDraw.LegendList := tStringList.Create;

   while not GISdb[theDB].MyData.eof do begin
      ThisGraph.OpenDataFile(rfile);
      ThisGraph.GraphDraw.LegendList.Add(GISdb[theDB].MyData.GetFieldByNameAsString('NAME'));
      WritePair(0,GISdb[theDB].MyData.GetFieldByNameAsFloat('MIN'));
      WritePair(5,GISdb[theDB].MyData.GetFieldByNameAsFloat('PERC_5'));
      WritePair(10,GISdb[theDB].MyData.GetFieldByNameAsFloat('PERC_10'));
      WritePair(25,GISdb[theDB].MyData.GetFieldByNameAsFloat('QUANT_25'));
      WritePair(50,GISdb[theDB].MyData.GetFieldByNameAsFloat('MEAN'));
      WritePair(75,GISdb[theDB].MyData.GetFieldByNameAsFloat('QUANT_75'));
      WritePair(90,GISdb[theDB].MyData.GetFieldByNameAsFloat('PERC_90'));
      WritePair(95,GISdb[theDB].MyData.GetFieldByNameAsFloat('PERC_95'));
      WritePair(100,GISdb[theDB].MyData.GetFieldByNameAsFloat('MAX'));
      CloseFile(rfile);
      GISdb[theDB].MyData.Next;
   end;
   ThisGraph.AutoScaleAndRedrawDiagram;;
   bmp := ThisGraph.MakeLegend(ThisGraph.GraphDraw.LegendList,false);
   Image1.Picture.Graphic := bmp;
   bmp.Free;
   GISdb[theDB].dbTablef.ShowStatus;
end;


initialization
finalization
   {$IfDef RecordPGraphs} WriteLineToDebugFile('RecordPGraphs active in param_graphs'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing param_graphs out'); {$EndIf}
end.
