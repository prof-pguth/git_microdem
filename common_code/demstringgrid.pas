unit demstringgrid;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{-----------------------------------------------------------------------------------------------------------------}
{originally based on, but now heavily revised:                                                                    }
{Copyright  © 2002-2005, Gary Darby, www.DelphiForFun.org                                                         }
{Program may be used or modified for any non-commercial purpose so long as this original notice remains in place. }
{All other rights are reserved                                                                                    }
{-----------------------------------------------------------------------------------------------------------------}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define StringGridSortProblems}
   //{$Define StringGridProblems}
   //{$Define StringGridColors}
   //{$Define CorrleationMatrixProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, StdCtrls, Buttons,
  DEMDefs,Petmath,Petmar,Petmar_types,Petmar_db;

const
   MaxMatrixSize = 250;

type
  TGridForm = class(TForm)
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    Edit2: TEdit;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    CheckBox1: TCheckBox;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    CheckBox2: TCheckBox;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    R          : array[1..MaxMatrixSize,1..MaxMatrixSize] of float64;
    FieldNames : array[1..MaxMatrixSize] of shortstring;
    NumVar,NumDec     : integer;
    OutputHeader : tStringList;
    AutoSizeCols : boolean;
    DoR : tGridCorrelationMatrix;
    theTitle,URstring,LegUnits : shortString;
    NeedRs : boolean;
    procedure HideCorrelationControls(Show : boolean = false);
    procedure ShowSortingControls(Show : boolean = false);
    procedure SetFormSize;
    procedure AddRow(Name,Value : shortstring);
    procedure ReadCSVFile(fName : PathStr);
  end;


function OpenCorrelationMatrix(Title : shortString; fName : PathStr) : DEMStringGrid.TGridForm;

procedure SortGrid(Grid : TStringGrid; const SortCol : integer; const datatype : integer; const ascending : boolean);


implementation

{$R *.dfm}

uses
   PetDBUtils,PetImage,BaseGraf, Petimage_form,
   DEMdatabase,nevadia_main;


procedure Mergesort(Grid:TStringgrid; var Vals: array of integer; sortcol,datatype : integer; ascending : boolean);
var
   AVals : array of integer;

        function compare(val1,val2:string):integer;
        var
          int1,int2:integer;
          errcode:integer;
          float1,float2:extended;
        begin
          case datatype of
            0: result := ANSIComparetext(val1,val2);
            1: begin
                 int1:=strtointdef(val1,0);
                 int2:=strtointdef(val2,0);
                 if (int1>int2) then result:=1
                 else if int1<int2 then result:=-1
                 else result := 0;
               end;

            2: begin
                 val(val1,float1,errcode);
                 if errcode<>0 then float1:=0;
                 val(val2,float2,errcode);
                 if errcode<>0 then float2:=0;
                 if float1>float2 then result:=1
                 else if float1<float2 then result:=-1
                 else result:=0;
               end;
             else result:=0;
            end;{case}
          end;

        {---------- Merge -------------}
        procedure Merge(ALo,AMid,AHi:Integer);
           var
              i,j,k,m,n:Integer;
        begin
          i:=0;
          setlength(Avals,Amid-alo+1);
          for j:=ALo to AMid do begin
            {copy lower half of Vals into temporary array AVals}
            AVals[i]:=Vals[j];
            inc(i);
          end;

          i:=0;
          j:=AMid + 1;
          k:=ALo;
          while ((k < j) and (j <= AHi)) do begin
            {Merge: Compare upper half to copied verasion of the lower half and move the appropriate value (smallest for ascending, largest for descending)
                into the lower half positions, for equals use Avals to preserve original order}
            with grid do
            n:=compare(cells[sortcol,Vals[j]],cells[sortcol,Avals[i]]);
            if ascending and (n>=0) or ((not ascending) and (n<=0)) then begin
              Vals[k]:=AVals[i];
              inc(i);inc(k);
            end
            else begin
              Vals[k]:=Vals[j];
              inc(k);inc(j);
            end;
          end;

          {copy any remaining, unsorted, elements}
          for m:=k to j - 1 do begin
            Vals[m]:=AVals[i];
            inc(i);
          end;
        end;

        {------------ PerformMergeSort ------------}
        procedure PerformMergeSort(ALo,AHi:Integer);
        {recursively split the split the value into 2 pieces and merge them back together as we unwind the recursion}
        var
           AMid:Integer;
        begin
            if (ALo < AHi) then begin
               AMid:=(ALo + AHi) shr 1;
               PerformMergeSort(ALo,AMid);
               PerformMergeSort(AMid + 1,AHi);
               Merge(ALo,AMid,AHi);
            end;
        end;

begin
  PerformMergeSort(0,high(vals));
end;


procedure SortGrid(Grid : TStringGrid; const SortCol : integer; const datatype : integer; const ascending : boolean);
{Setup to run quicksort then call it}
var
   i : integer;
   tempgrid : tstringGrid;
   list : array of integer;
begin
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('Sortgrid in'); {$EndIf}
  ShowHourglassCursor;
  TempGrid := TStringgrid.create(nil);
  TempGrid.rowcount := grid.rowcount;
  TempGrid.colcount := grid.colcount;
  TempGrid.fixedrows := grid.fixedrows;
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('Point 2'); {$EndIf}

   setlength(list,Grid.rowcount-Grid.fixedrows);
   for i:= Grid.fixedrows to Grid.rowcount-1 do begin
      list[i-Grid.fixedrows]:=i;
      tempgrid.rows[i].assign(grid.rows[i]);
   end;
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('call mergesort'); {$EndIf}
   MergeSort(Grid, list,sortcol+1,datatype, ascending);
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('back from mergesort'); {$EndIf}
   for i := 0 to Grid.rowcount-Grid.fixedrows-1 do begin
      Grid.rows[i+Grid.fixedrows].assign(tempgrid.rows[list[i]])
   end;
   Grid.row:=Grid.fixedrows;
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('Point 3'); {$EndIf}
  tempgrid.free;
  setlength(list,0);
  ShowDefaultCursor;
  {$IfDef StringGridSortProblems} WriteLineToDebugFile('Sortgrid out'); {$EndIf}
end;

{------------------------------------------------------------------------------------------------------------------}


function OpenCorrelationMatrix(Title : shortString; fName : PathStr) : DEMStringGrid.TGridForm;
begin
   {$IfDef StringGridProblems} WriteLineToDebugFile('OpenCorrelationMatrix in'); {$EndIf}
   Result := TGridForm.Create(Application);
   Result.theTitle := Title;
   Result.Caption := Title;
   Result.NeedRs := true;
   Result.ReadCSVFile(fName);
  //Result.Variety := Variety;
   Result.SetFormSize;
   {$IfDef StringGridProblems} WriteLineToDebugFile('OpenCorrelationMatrix out'); {$EndIf}
end;


procedure TGridForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TGridForm.SetFormSize;
begin
   ClientWidth := StringGrid1.ColCount * StringGrid1.DefaultColWidth;
   if ClientWidth > wmdem.ClientWidth - 100 then ClientWidth := wmdem.ClientWidth - 100;
   ClientHeight := StringGrid1.RowCount * StringGrid1.DefaultRowHeight + Panel1.Height;
   if ClientHeight > wmdem.ClientHeight - 100 then ClientWidth := wmdem.ClientHeight - 100;
end;


procedure TGridForm.FormCreate(Sender: TObject);
begin
   OutputHeader := Nil;
   AutoSizeCols := false;
   ShowSortingControls(false);
   URString := '';
   LegUnits := 'r';
   NumDec := 4;
end;

procedure TGridForm.BitBtn10Click(Sender: TObject);
var
   i,j : integer;
begin
   if (Sender <> Nil) then ReadDefault('Decimals in grid',NumDec);
   for i := 1 to NumVar do begin
      for j := 1 to NumVar do begin
         StringGrid1.Cells[i,j] := RealToString(r[i,j],8,NumDec);
      end;
   end;
end;

procedure TGridForm.BitBtn11Click(Sender: TObject);
var
   fName : PathStr;
   db : integer;
begin
  fName := NextFileNumber(MDTempDir,'string_grid_export_','.csv');
  StringGridToCSVFile(fName,StringGrid1,Nil);
  OpenNumberedGISDataBase(db,fName,true);
  //DoCSVFileImport(fName);
end;

procedure TGridForm.BitBtn1Click(Sender: TObject);
var
   i,j,NumUsed : integer;
   Highlight : float64;
   UseVar : array[1..MaxMatrixSize] of integer;
begin
   Highlight := 0;
   Petmar.CheckEditString(Edit1.Text,Highlight);

   for i := 1 to NumVar do UseVar[i] := 0;
   NumUsed := 0;
   for i := 1 to NumVar do begin
      for j := 1 to NumVar do begin
         if (UseVar[i] = 0) and (i<>J) and (abs(R[i,j]) > Highlight) then  begin
            inc(NumUsed);
            UseVar[i] := NumUsed;
         end;
      end;
   end;

   StringGrid1.ColCount := succ(NumUsed);
   StringGrid1.RowCount := succ(NumUsed);

   for i := 1 to NumVar do begin
      if (UseVar[i] > 0) then  begin
         StringGrid1.Cells[0,UseVar[i]] := FieldNames[i];
         StringGrid1.Cells[UseVar[i],0] := FieldNames[i];
      end;
      for j := 1 to NumVar do  begin
         if (abs(R[i,j]) > Highlight) then StringGrid1.Cells[UseVar[i],UseVar[j]] := RealToString(r[i,j],8,NumDec)
         else StringGrid1.Cells[i,j] := '';
      end;
   end;
end;


procedure TGridForm.ReadCSVFile(fName : PathStr);
var
   FileInMemory : tStringList;
   i,j,OnLine : integer;
   MenuStr : ANSIString;
   TStr : shortString;
   SepChar : AnsiChar;
begin
  {$IfDef StringGridProblems} WriteLineToDebugFile('TGridForm.ReadCSVFile in'); {$EndIf}
  FileInMemory := tStringList.Create;
  FileInMemory.LoadFromFile(fName);

  MenuStr := ptTrim(FileInMemory.Strings[0]);
  GetSeparationCharacter(MenuStr,SepChar);
  OnLine := 0;
  for i := 0 to pred(FileInMemory.Count) do begin
     MenuStr := ptTrim(FileInMemory.Strings[i]);
     for j := pred(Length(MenuStr)) downto 1 do
        if (MenuStr[j] = SepChar) and (MenuStr[succ(j)] = SepChar) then System.Insert(' ',MenuStr,succ(j));
     j := 0;
     repeat
         TStr := ptTrim(BeforeSpecifiedCharacterANSI(MenuStr,SepChar,false,true));
         StringGrid1.Cells[j,OnLine] := TStr;
         inc(j);
     until (MenuStr = '');
     StringGrid1.ColCount := j;
     inc(OnLine);
  end;
  NumVar := pred(StringGrid1.ColCount);
  StringGrid1.RowCount := FileInMemory.Count;

   if NeedRs then begin
      {$IfDef StringGridProblems} WriteLineToDebugFile('TGridForm.ReadCSVFile find R'); {$EndIf}
        for i := 1 to pred(StringGrid1.ColCount) do begin
           FieldNames[i] := ptTrim(StringGrid1.Cells[i,0]);
           for j := 1 to pred(StringGrid1.RowCount) do begin
              TStr := StringGrid1.Cells[i,j];
              if (TStr <> '') and (UpperCase(TStr) <> 'NAN') and (i <= MaxMatrixSize) and (j <= MaxMatrixSize) then begin
                 r[i,j] := StrToFloat(TStr);
              end;
           end;
        end;
      BitBtn10Click(Nil);
   end;
   FileInMemory.Free;
   {$IfDef StringGridProblems} WriteLineToDebugFile('TGridForm.ReadCSVFile out'); {$EndIf}
end;

procedure TGridForm.HideCorrelationControls(Show : boolean = false);
begin
   BitBtn1.Visible := Show;
   BitBtn4.Visible := Show;
   BitBtn6.Visible := Show;
   BitBtn7.Visible := Show;
   Edit1.Visible := Show;
   Edit2.Visible := Show;
   Label1.Visible := Show;
   NeedRs := false;
end;


procedure TGridForm.ShowSortingControls(Show: boolean);
begin
   BitBtn8.Visible := show;
   BitBtn9.Visible := show;
   BitBtn19.Visible := show;
   BitBtn20.Visible := show;
end;


procedure TGridForm.BitBtn2Click(Sender: TObject);
begin
   EditTFont(StringGrid1.Font);
   FormResize(Nil);
end;


procedure TGridForm.FormResize(Sender: TObject);
var
   dWidth : integer;
begin
   if AutosizeCols then dwidth := clientwidth div StringGrid1.ColCount
   else begin
      dWidth := 100;
      CheckEditString(Edit2.Text,dwidth);
      if (dWidth < ClientWidth div StringGrid1.ColCount) then dWidth := ClientWidth div StringGrid1.ColCount;
   end;
   StringGrid1.DefaultColWidth := dWidth;
   StringGrid1.DefaultRowHeight := 2 * StringGrid1.Canvas.TextHeight('9');
end;


procedure TGridForm.BitBtn3Click(Sender: TObject);
begin
   PetDBUtils.HTMLReport(Caption,StringGrid1);
end;


procedure TGridForm.BitBtn4Click(Sender: TObject);
var
  i,j : integer;
  LowTStr,HighTStr : ANSIstring;
  Results : tStringList;
  Highlight : float64;
  fName : PathStr;
begin
   Highlight := 0;
   Petmar.CheckEditString(Edit1.Text,Highlight);
   Results := tStringList.Create;
   Results.Add(StartHTMLString);
   Results.Add('  ');
   Results.Add(StartTableString);
   Results.Add(StartRowString + StartColumnString + 'Field' + EndColumnString + StartColumnString + 'Positive Correlation > ' + RealToString(Highlight,-12,-2) + EndColumnString +
                  StartColumnString + 'Negative Correlation <  -' + RealToString(Highlight,-12,-2) + EndColumnString + EndRowString);
   for i := 1 to NumVar do  begin
      LowTStr := ' ';
      HighTStr := ' ';
      for j := 1 to NumVar do
         if (i <> j) then begin
            if (R[i,j] > Highlight) then HighTStr := HighTStr + FieldNames[j] + '  ';
            if (R[i,j] < -Highlight) then LowTStr := LowTStr + FieldNames[j] + '  ';
         end;
      Results.Add(StartRowString + StartColumnString + FieldNames[i] + EndColumnString + StartColumnString + HighTStr + '  ' + EndColumnString + StartColumnString + LowTStr + '  ' + EndColumnString + EndRowString);
   end;
   Results.Add(EndTableString);
   Results.Add(EndHTMLString);
   fName := MDTempDir + 'correlations.htm';
   Results.SaveToFile(fName);
   Results.Free;
   ExecuteFile(fName, '', '');
end;


procedure TGridForm.BitBtn5Click(Sender: TObject);
begin
   StringGridToCSVFile('',StringGrid1,OutputHeader);
end;


procedure TGridForm.BitBtn6Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
const
   LeftOffset : integer = 0;
   LegendSize : integer = 250;
   BoxSize  : integer = 0;
   FontSize : integer = 0;
   fName : PathStr = '';
var
   Bitmap,Bitmap2 : tMyBitmap;
   Table : tMyData;
   TStr : shortString;
   x1,y1,x2,y2,i,j : integer;
   MinVal,MaxVal : float64;
   r,delta,value,Perfect : float64;
   ShowHeader : boolean;
begin
   {$IfDef StringGridColors} WriteLineToDebugFile('TGridForm.BitBtn6Click in'); {$EndIf}
   if (BoxSize = 0) then begin
      if (StringGrid1.ColCount > 100) then begin
         BoxSize := 10;
         FontSize := 8;
      end
      else begin
         BoxSize := 25;
         FontSize := 14;
      end;
   end;

   if (Sender <> nil) then begin
      ReadDefault('Box size',BoxSize);
      ReadDefault('Font size',FontSize);
      ShowHeader := AnswerIsYes('Show caption');
      if (fName = '') then begin
         fName := ProgramRootDir + 'correlations' + DefaultDBExt;
         GetFileFromDirectory('Correlation color table',DefaultDBMask,fName);
      end;
   end
   else ShowHeader := true;

    MinVal := 99999;
    MaxVal := -9999;
    for i := 1 to pred(StringGrid1.ColCount) do begin
       for j := 1 to pred(StringGrid1.RowCount) do  begin
          if (j <> i) then Petmath.CompareValueToExtremes(StrToFloat(StringGrid1.Cells[i,j]),MinVal,MaxVal);
       end;
    end;

    if (DoR = gcmMAvD) then fName := ProgramRootDir + 'red_blue_diverge.dbf'
    else fName := ProgramRootDir + 'correlation_matrix_color.dbf';
    if FileExists(fName) then begin
      Table := tMyData.Create(fName);

      if (DoR = gcmR) then begin
         if (MaxVal > MDDef.PerfectR) and (MDDef.PerfectR > 0) then begin
            delta := (MDDef.PerfectR-MinVal) / pred(Table.TotRecsInDB);
            Table.Edit;
            Table.SetFieldByNameAsFloat('MAX',MaxVal);
            Table.SetFieldByNameAsFloat('MIN',MDDef.PerfectR);
            Table.Next;
            Value := MDDef.PerfectR;
         end
         else begin
            delta := (MaxVal-MinVal) / Table.TotRecsInDB;
            Value := MaxVal;
         end;
         while not Table.eof do begin
            Table.Edit;
            Table.SetFieldByNameAsFloat('MAX',value);
            Value := Value - delta;
            Table.SetFieldByNameAsFloat('MIN',value);
            Table.Next;
         end;
      end
      else begin
         if (DoR = gcmMAbD) then begin
            Perfect := MDDef.PerfectMAbD;
            if (MinVal < Perfect) then begin
               delta := (MaxVal-Perfect) / pred(Table.TotRecsInDB);
               Table.Edit;
               Table.SetFieldByNameAsFloat('MIN',MinVal);
               Table.SetFieldByNameAsFloat('MAX',Perfect);
               Table.Next;
               Value := Perfect;
            end
            else begin
               delta := (MaxVal-MinVal) / Table.TotRecsInDB;
               Value := MinVal;
            end;
            while not Table.eof do begin
               Table.Edit;
               Table.SetFieldByNameAsFloat('MIN',value);
               Value := Value + delta;
               Table.SetFieldByNameAsFloat('MAX',value);
               Table.Next;
            end;
         end
         else begin
            delta := (MaxVal-MinVal) / Table.TotRecsInDB;
            Value := MaxVal;
            while not Table.eof do begin
               Table.Edit;
               Table.SetFieldByNameAsFloat('MAX',value);
               Value := Value - delta;
               Table.SetFieldByNameAsFloat('MIN',value);
               Table.Next;
            end;
         end;
      end;

    end
    else begin
       Table := Nil;
      {$IfDef CorrleationMatrixProblems} WriteLineToDebugFile('TGridForm.BitBtn6Click, range of correlations ' + RealToString(MinVal,-8,-4) + ' to ' + RealToString(MaxVal,-8,-4)); {$EndIf}
    end;

   ShowHourglassCursor;
   CreateBitmap(Bitmap,2000,2000);
   Bitmap.Canvas.Font.Name := 'Verdana';
   Bitmap.Canvas.Font.Size := FontSize;
   Bitmap.Canvas.Font.Style := [fsBold];

   for j := 1 to pred(StringGrid1.RowCount) do begin
      i := Bitmap.Canvas.TextWidth(StringGrid1.Cells[0,j]) + 15;
      if I > LeftOffset then LeftOffset := i;
   end;
   LegendSize := 65 + Bitmap.Canvas.TextWidth(' 1.0000 > ' + LegUnits + ' > -1.0000');

   {$IfDef StringGridProblems} WriteLineToDebugFile('start columns'); {$EndIf}
   for i := 1 to pred(StringGrid1.ColCount) do  begin
      {$IfDef StringGridProblems}    WriteLineToDebugFile('i=' + IntToStr(i)); {$EndIf}
      Bitmap.Canvas.Brush.Style := bsClear;
      Petmar.TextOutVertical(Bitmap.Canvas,LeftOffset+pred(i)*BoxSize,LeftOffset-5,RemoveUnderscores(StringGrid1.Cells[i,0]));
      for j := 1 to pred(StringGrid1.RowCount) do  begin
         Bitmap.Canvas.Brush.Style := bsClear;
         TStr := RemoveUnderscores(StringGrid1.Cells[0,j]);
         Bitmap.Canvas.TextOut(LeftOffset - 5 - Bitmap.Canvas.TextWidth(TStr),LeftOffset+5 + pred(j) * BoxSize,TStr);
         r := StrToFloat(StringGrid1.Cells[i,j]);
         x1 := LeftOffset + pred(i)*BoxSize;
         y1 := LeftOffset + pred(j)*BoxSize;
         x2 := LeftOffset + (i)*BoxSize;
         y2 := LeftOffset + (j)*BoxSize;
         if (i=j) then begin
            //Bitmap.Canvas.Brush.Color := clWhite;
            Bitmap.Canvas.MoveTo(x1,y1);  Bitmap.Canvas.LineTo(x2,y2);
            Bitmap.Canvas.MoveTo(x1,y2);  Bitmap.Canvas.LineTo(x2,y1);
            Bitmap.Canvas.Brush.Style := bsClear;
         end
         else begin
            if (Table = Nil) then begin
               Bitmap.Canvas.Brush.Color := Petmar.TerrainTColor(r,MinVal,MaxVal);
               {$IfDef StringGridColors} WriteLineToDebugFile('i=' + IntToStr(i) + ' j=' + IntToStr(j) + ' r=' + RealToString(r,-8,3) + ' ' + ColorString(Bitmap.Canvas.Brush.Color)); {$EndIf}
            end
            else begin
                Table.First;
                while (r < Table.GetFieldByNameAsFloat('MIN')) or (r > Table.GetFieldByNameAsFloat('MAX')) do begin
                   Table.Next;
                   if Table.eof then break;
                end;
                Bitmap.Canvas.Brush.Color := Table.TColorFromTable;
            end;
            Bitmap.Canvas.Brush.Style := bsSolid;
         end;
         Bitmap.Canvas.Rectangle(x1,y1,x2,y2);
      end;
   end;
   GetImagePartOfBitmap(Bitmap);

   {$IfDef StringGridProblems} WriteLineToDebugFile('if (Table = Nil) check'); {$EndIf}

   if (Table = Nil) then begin
       Bitmap2 := DefaultVerticalLegendOnBitmap(MinVal,MaxVal,'','',LegTerrain);
   end
   else begin
       Table.First;
       j := 1;
       CreateBitmap(Bitmap2,1250,1200);
       Bitmap2.Canvas.Font.Name := 'Verdana';
       Bitmap2.Canvas.Font.Size := FontSize;
       Bitmap2.Canvas.Font.Style := [fsBold];

       while not Table.eof do begin
          Bitmap2.Canvas.Brush.Color := Table.TColorFromTable;
          Bitmap2.Canvas.Brush.Style := bsSolid;
          Bitmap2.Canvas.Rectangle(pred(StringGrid1.ColCount)*BoxSize +10,4+(j)*BoxSize+30, pred(StringGrid1.ColCount)*BoxSize +40, succ(j)*BoxSize +30);
          Bitmap2.Canvas.Brush.Style := bsClear;
          TStr := RealToString(Table.GetFieldByNameAsFloat('MAX'),6,4) + ' > ' + LegUnits + ' > ' +  RealToString(Table.GetFieldByNameAsFloat('MIN'),6,4);
          Bitmap2.Canvas.TextOut(pred(StringGrid1.ColCount)*BoxSize + 50,(j)*BoxSize+30,TStr);
          Table.Next;
          inc(j);
       end;
       Table.Destroy;
       GetImagePartOfBitmap(Bitmap2);
   end;
   LeftOffset := Bitmap.Width;
   Bitmap.Width := Bitmap.Width  + 10 + Bitmap2.Width;
   Bitmap.Canvas.Draw(LeftOffset + 10,BitMap.Height - 10 - Bitmap2.Height,Bitmap2);
   FreeAndNil(Bitmap2);

   Bitmap.Canvas.Font.Size := 18;
   Bitmap.Canvas.Font.Style := [fsBold];
   if ShowHeader then Bitmap.Canvas.TextOut(2,2,RemoveUnderscores(Caption));

   Bitmap.Canvas.Font.Size := 24;
   if (URstring <> '') then Bitmap.Canvas.TextOut(Bitmap.Width - Bitmap.Canvas.TextWidth(URstring),5,URString);

   Petimage_form.DisplayBitmap(Bitmap,theTitle);
   ShowDefaultCursor;
   {$IfDef StringGridProblems} WriteLineToDebugFile('TGridForm.BitBtn6Click out'); {$EndIf}
{$EndIf}
end;


procedure TGridForm.BitBtn7Click(Sender: TObject);
var
   i,j : integer;
   r : float64;
   Graph : BaseGraf.TThisBaseGraph;
   rfile : file;
   v : array[1..3] of float64;
begin
   Graph := BaseGraf.TThisBaseGraph.Create(application);
   Graph.SetUpGraphForm;
   Graph.OpenXYZFile(rfile);
   Graph.GraphDraw.RainBowColors := true;
   for i := 1 to pred(StringGrid1.ColCount) do begin
      for j := 1 to pred(StringGrid1.RowCount) do begin
         r := StrToFloat(StringGrid1.Cells[i,j]);
         if (i=j) and (r > 0.9999999) then begin
         end
         else begin
             v[1] := i;
             v[2] := j;
             v[3] := r;
             BlockWrite(rfile,v,1);
         end;
      end;
   end;
   CloseFile(rFile);
   Graph.AutoScaleAndRedrawDiagram;
end;


procedure TGridForm.BitBtn8Click(Sender: TObject);
begin
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('BitBtn8Click in'); {$EndIf}
   if (Sender = BitBtn8) or (Sender = BitBtn9) then begin  //integer
      SortGrid(StringGrid1,0,1,Sender = BitBtn9);
   end
   else begin  //string
      SortGrid(StringGrid1,-1,0,Sender = BitBtn20);
   end;
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('BitBtn8Click out'); {$EndIf}
end;


procedure TGridForm.BitBtn9Click(Sender: TObject);
begin
   BitBtn8Click(Sender);
end;


procedure TGridForm.CheckBox1Click(Sender: TObject);
begin
   AutoSizeCols := CheckBox1.Checked;
   if AutosizeCols then FormResize(sender);
end;

procedure TGridForm.CheckBox2Click(Sender: TObject);
var
   i,j : integer;
   x : float64;
begin
   for i := 1 to NumVar do begin
      for j := 1 to NumVar do begin
         if CheckBox2.Checked then x := sqr(r[i,j])
         else x := r[i,j];
         StringGrid1.Cells[i,j] := RealToString(x,8,NumDec);
      end;
   end;
end;


procedure TGridForm.AddRow(Name,Value : shortstring);
begin
   StringGrid1.Cells[0,StringGrid1.RowCount] := Name;
   StringGrid1.Cells[1,StringGrid1.RowCount] := Value;
   StringGrid1.RowCount := StringGrid1.RowCount + 1;
end;


initialization
finalization
   {$IfDef StringGridSortProblems} WriteLineToDebugFile('StringGridSortProblems active in demstringgrid'); {$EndIf}
   {$IfDef StringGridProblems} WriteLineToDebugFile('StringGridProblems active in demstringgrid'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demstringgrid out'); {$EndIf}
end.
