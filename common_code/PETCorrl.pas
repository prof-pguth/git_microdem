unit Petcorrl;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls,SysUtils,
   PETMAR,Petmar_types, Menus;

const
   MaxVars = 36;

type
  TCorrelationForm = class(TForm)
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    MainMenu1: TMainMenu;
    Statistics1: TMenuItem;
    Histogram1: TMenuItem;
    Scattergram1: TMenuItem;
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Histogram1Click(Sender: TObject);
    procedure Scattergram1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NumVals,
    NumHeaderLines : integer;
    NumPoints   : LongInt;
    StdDev,MinVal,MaxVal,Sum : array[1..MaxVars] of float64;
    SP          : array[1..MaxVars,1..MaxVars] of float64;
    Titles,
    VarNames    : tStringList;
    DataFile    : PathStr;
  end;

var
  CorrelationForm : TCorrelationForm;


procedure FigureCorrelations(FName : PathStr);


implementation

{$R *.DFM}

uses
   BaseGraf,PETMath;

procedure GetBins(MaxMax,MinMin : float64; var BinSize,StartIndex : float64);
var
   ElevRange : float64;
begin
   ElevRange := MaxMax - MinMin;
   BinSize := 0.01 * ElevRange;
   StartIndex := MinMin;
end;


procedure FigureCorrelations(FName : PathStr);
var
   i,j : integer;
   inf,Where : system.text;
   Data : array[1..MaxVars] of float64;
   UseVals : array[1..MaxVars] of boolean;
   EigenVectors,VarCoVar : ^tTrendMatrix;
   EigenValues: tTrendVector;
   nrot : integer;
   Value : float64;
   MenuStr : ShortString;


   procedure ProcessReading;
   var
      i,j : integer;
   begin
      with CorrelationForm do for i := 1 to NumVals do begin
         if Data[i] > MaxVal[i] then MaxVal[i] := Data[i];
         if Data[i] < MinVal[i] then MinVal[i] := Data[i];
         Sum[i] := Sum[i] + Data[i];
         for j := i to NumVals do SP[i,j] := SP[i,j] + 1.0 * Data[i] * Data[j];
      end {for i};
   end;

begin
   if not FileExists(FName) then exit;
   new(EigenVectors);
   new(VarCoVar);
   assignFile(inf,FName);
   reset(inf);
   CorrelationForm := tCorrelationForm.Create(Application);
   with CorrelationForm do begin
      DataFile := FName;
      NumVals := 0;
      NumHeaderLines := 0;
      NumPoints := 0;
      for i := 1 to MaxVars do begin
         MinVal[i] := MaxInt;
         MaxVal[i] := -MaxInt;
         Sum[i] := 0;
         for j := 1 to MaxVars do SP[i,j] := 0;
      end;

      repeat
         readln(Inf,MenuStr);
         if MenuStr[1] = '*' then begin
            inc(NumVals);
            Delete(MenuStr,1,1);
            VarNames.Add(MenuStr);
            inc(NumHeaderLines);
            MenuStr := '*';
         end;
         if MenuStr[1] = '+' then begin
            Delete(MenuStr,1,1);
            Titles.Add(MenuStr);
            inc(NumHeaderLines);
            MenuStr := '+';
         end;
      until not (MenuStr[1] in ['+','*']);

      if NumVals = 0 then begin
         repeat
            inc(NumVals);
            read(inf,Data[NumVals]);
         until EOLn(inf);
         for i := 1 to NumVals do VarNames.Add('Variable' + IntegerToString(i,4));
      end;

      ComboBox1.Items := VarNames;
      ComboBox2.Items := VarNames;
      ComboBox3.Items := VarNames;
      ComboBox1.Text := VarNames[0];
      ComboBox2.Text := VarNames[1];
      ComboBox3.Text := VarNames[1];

      StartCount('Process correlations');
      reset(inf);
      for i := 1 to NumHeaderLines do readln(inf);
      while not EOF(inf) do begin
         for i := 1 to NumVals do read(inf,Data[i]);
         readln(inf);
         inc(NumPoints);
         if (NumPoints mod 1000 = 0) then UpDateCount(NumPoints);
         ProcessReading;
         While EOLn(inf) and (not EOF(Inf)) do readln(inf);
      end;
      EndCount;
      CloseFile(inf);
      if NumPoints < 2 then exit;

      assignFile(Where,MDTempDir + 'CompStat.txt');
      rewrite(Where);
      for i := 1 to Titles.Count do writeln(Where,Titles.Strings[pred(i)]);
      writeln(Where);
      for i := 1 to NumVals do writeln(Where,chr(i+64),'  ',VarNames.Strings[pred(i)]);
      writeln(Where);
      writeln(Where,'   n=',NumPoints);
      writeln(Where);

      writeln(Where,'         Min           Max             Avg          Std Dev');
      for i := 1 to NumVals do begin
         StdDev[i] := sqrt((SP[i,i] * NumPoints - 1.0 * Sum[i] * Sum[i]) / (1.0 * NumPoints * pred(NumPoints)));
         writeln(Where,chr(i+64),MinVal[i]:15:5,MaxVal[i]:15:5,(Sum[i] / NumPoints):15:5,StdDev[i]:15:5,'    ',VarNames.Strings[pred(i)]);
      end;

      writeln(Where);
      writeln(Where,'Correlation coefficients');
      if (NumVals < 11) then write(Where,'        ');
      for i := 1 to NumVals do write(Where,chr(i+64):7);
      writeln(Where);
      for i := 1 to NumVals do begin
         if NumVals < 11 then write(Where,'        ');
         write(Where,chr(i+64));
         for j := 1 to NumVals do
             if (j < i) or (StdDev[j] < 0.001) or  (StdDev[i] < 0.001) then
                  write(Where,'       ')
             else write(Where, (SP[i,j] - (Sum[j] * Sum[i] / NumPoints) )
                  / pred(NumPoints) / StdDev[j] / StdDev[i]:7:3);
         writeln(Where);
      end;

      if AnswerIsYes('Selected correlations') then begin
          for i := 1 to NumVals do UseVals[i] := AnswerIsYes('Include ' + VarNames[pred(i)]);
          writeln(Where);
          for i := 1 to NumVals do if UseVals[i] then writeln(Where,chr(i+64),'  ',VarNames.Strings[pred(i)]);
          writeln(Where);

          writeln(Where);
          writeln(Where,'Selected Correlation coefficients');
          if NumVals < 11 then write(Where,'        ');
          for i := 1 to NumVals do if UseVals[i] then write(Where,chr(i+64):7);
          writeln(Where);
          for i := 1 to NumVals do if UseVals[i] then begin
             if NumVals < 11 then write(Where,'        ');
             write(Where,chr(i+64));
             for j := 1 to NumVals do if UseVals[j] then
                 if (j < i) or (StdDev[j] < 0.001) or  (StdDev[i] < 0.001) then
                      write(Where,'       ')
                 else write(Where, (SP[i,j] - (Sum[j] * Sum[i] / NumPoints) )
                      / pred(NumPoints) / StdDev[j] / StdDev[i]:7:3);
             writeln(Where);
          end;
      end;

      for i := 1 to NumVals do begin
         for j := 1 to NumVals do begin
             if j >= i then begin
                VarCovar^[i,j] := (SP[i,j] - (Sum[i] * Sum[j]) / NumPoints) / pred(NumPoints) / StdDev[j] / StdDev[i];
                VarCovar^[j,i] := VarCoVar^[i,j];
             end;
         end;
      end;

      writeln(Where);

      Jacobi(VarCoVar^,NumVals,Eigenvalues,EigenVectors^,Nrot);
      Eigsrt(EigenValues,EigenVectors^,NumVals);

      writeln(Where,'Eigenvalues');
      write(Where,'       ');
      for i := 1 to NumVals do write(Where,Eigenvalues[i]:8:2);
      writeln(Where);
      writeln(Where);
      writeln(Where,'Percent of Variance Explained');
      Value := 0;
      for i := 1 to NumVals do Value := Value + Eigenvalues[i];
      write(Where,'       ');
      for i := 1 to NumVals do write(Where,(100.0 * Eigenvalues[i] / Value):8:2);
      writeln(Where);
      writeln(Where);
      writeln(Where,'Normalized eigen vectors');
      writeln(Where);

      for i := 1 to NumVals do begin
         write(Where,' ',chr(ord('A') + pred(i)),' ');
         for j := 1 to NumVals do write(Where, EigenVectors^[i,j]:8:4);
         writeln(Where);
      end;
      writeln(Where);
      for i := 1 to NumVals do
         writeln(Where,chr(i+64),'  ',VarNames.Strings[pred(i)]);
      writeln(Where);

      CloseFile(Where);

      QuickOpenEditWindow(MDTempDir + 'CompStat.txt','Difference Statistics');
      Erase(Where);
      CorrelationForm.Show;
      Dispose(EigenVectors);
      Dispose(VarCoVar);
   end;
end;


procedure TCorrelationForm.FormCreate(Sender: TObject);
begin
   VarNames := tStringList.Create;
   Titles := tStringList.Create;
end;


procedure TCorrelationForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   Action := caFree;
   VarNames.Free;
   Titles.Free;
end;



procedure TCorrelationForm.Histogram1Click(Sender: TObject);
var
   VarWant   : integer;
   BinCounts : array[0..100] of LongInt;
   i,j         : integer;
   Count,Range     : float64;
   NumDone   : LongInt;
   Inf       : System.Text;
   ThisGraph2,
   ThisGraph : tThisBaseGraph;
   rfile     : file;
   OutData   : array[0..100,1..2] of float64;
   Data : array[1..MaxVars] of float64;
begin
   for VarWant := 1 to ComboBox1.Items.Count do
      if ComboBox1.Text = ComboBox1.Items[pred(VarWant)] then break;

   Range := MaxVal[VarWant] - MinVal[VarWant];
   for i := 0 to 100 do BinCounts[i] := 0;
   AssignFile(inf,DataFile);
   reset(inf);
   for i := 1 to NumHeaderLines do readln(inf);
   StartCount('Get histogram');
   NumDone := 0;
   while not EOF(inf) do begin
      for i := 1 to NumVals do read(inf,Data[i]);
      readln(inf);
      J := trunc(100 * (Data[VarWant] - MinVal[VarWant])/Range);
      if j > 100 then j := 100;
      if j < 0 then j := 0;
      inc(BinCounts[j]);
      inc(NumDone);
      if NumDone mod 250 = 0 then UpdateCount(NumDone);
   end;
   EndCount;
   closeFile(inf);
   ThisGraph := tThisBaseGraph.Create(Application);
   with ThisGraph,GraphDraw do begin
      Caption := VarNames[pred(VarWant)] + ' Frequency Distribution';
      MinHorizAxis := MinVal[VarWant];
      MaxHorizAxis := MaxVal[VarWant];
      HorizLabel := VarNames[pred(VarWant)];
      OpenDataFile(rfile);
      VertLabel := 'Percentage of Data';
      MaxVertAxis := 0;
      for i := 0 to 100 do begin
         Count := 100.0 * BinCounts[i] / NumDone;
         if Count > MaxVertAxis then MaxVertAxis := Count;
         OutData[i,1] := (MinVal[VarWant] + (0.01 * i + 0.005) * Range);
         OutData[i,2] := Count;
      end;
      BlockWrite(rfile,OutData,101);
      CloseFile(Rfile);
      SetUpGraphForm;
      GraphDraw.GraphDrawn := true;
      PadAxis(MinVertAxis,MaxVertAxis);
      RedrawDiagram11Click(Nil);
   end;

   ThisGraph2 := tThisBaseGraph.Create(Application);
   with ThisGraph2,GraphDraw do begin
      Caption := VarNames[pred(VarWant)] + ' Cumulative Frequency Distribution';
      MinHorizAxis := MinVal[VarWant];
      MaxHorizAxis := MaxVal[VarWant];
      HorizLabel := VarNames[pred(VarWant)];;
      VertAxisFunctionType := CumulativeNormalAxis;
      VertLabel := 'Cumulative Percentage of Data';
      SetUpGraphForm;
      OpenDataFile(rfile);
      for i := 1 to 100 do OutData[i,2] := OutData[i,2] + OutData[pred(i),2];
      BlockWrite(rfile,OutData,101);
      CloseFile(Rfile);
      SetUpGraphForm;
      GraphDraw.GraphDrawn := true;
      RedrawDiagram11Click(Nil);
   end;
   SetFocus;
end;


procedure TCorrelationForm.Scattergram1Click(Sender: TObject);
var
   VarWant,VarWant2,VarWant3,BuffSize,
   i         : integer;
   NumDone   : LongInt;
   Inf       : System.Text;
   ThisGraph : tThisBaseGraph;
   rfile     : file;
   OutData   : array[1..100,1..2] of float64;
   XYZData   : array[1..100,1..3] of float64;
   Data : array[1..MaxVars] of float64;
begin
   for VarWant := 1 to ComboBox1.Items.Count do  if ComboBox1.Text = ComboBox1.Items[pred(VarWant)] then break;
   for VarWant2 := 1 to ComboBox2.Items.Count do if ComboBox2.Text = ComboBox2.Items[pred(VarWant2)] then break;
   for VarWant3 := 1 to ComboBox3.Items.Count do if ComboBox3.Text = ComboBox3.Items[pred(VarWant3)] then break;

   ThisGraph := tThisBaseGraph.Create(Application);
   with ThisGraph,GraphDraw do begin
      Caption := 'Frequency Distribution';
      MinHorizAxis := MinVal[VarWant];
      MaxHorizAxis := MaxVal[VarWant];
      HorizLabel := VarNames[pred(VarWant)];
      MinVertAxis := MinVal[VarWant2];
      MaxVertAxis := MaxVal[VarWant2];
      VertLabel := VarNames[pred(VarWant2)];
      PadAxis(MinHorizAxis,MaxHorizAxis);
      PadAxis(MinVertAxis,MaxVertAxis);
      SetUpGraphForm;
      GraphDraw.Symbol[1].size := 1;
      LineSize256[1] := 0;
      if CheckBox1.Checked then begin
         OpenXYZFile(rfile);
         MinZ := MinVal[VarWant3];
         MaxZ := MaxVal[VarWant3];
      end
      else OpenPointFile(rfile,ThisGraph.Symbol);

      AssignFile(inf,DataFile);
      reset(inf);
      for i := 1 to NumHeaderLines do readln(inf);

      StartCount('Plotting scattergram');
      NumDone := 0;
      BuffSize := 0;
      while not EOF(inf) do begin
         for i := 1 to NumVals do read(inf,Data[i]);
         readln(inf);
         inc(NumDone);
         inc(BuffSize);
         if CheckBox1.Checked then begin
            XYZData[BuffSize,1] := Data[VarWant];
            XYZData[BuffSize,2] := Data[VarWant2];
            XYZData[BuffSize,3] := Data[VarWant3];
            if NumDone mod 100 = 0 then begin
               UpdateCount(NumDone);
               BlockWrite(rfile,XYZData,BuffSize);
               BuffSize := 0;
            end;
         end
         else begin
            OutData[BuffSize,1] := Data[VarWant];
            OutData[BuffSize,2] := Data[VarWant2];
            if NumDone mod 100 = 0 then begin
               UpdateCount(NumDone);
               BlockWrite(rfile,OutData,BuffSize);
               BuffSize := 0;
            end;
         end;
      end;
      if CheckBox1.Checked then BlockWrite(rfile,XYZData,BuffSize)
      else BlockWrite(rfile,OutData,BuffSize);
      EndCount;
      closeFile(inf);
      CloseFile(Rfile);
      GraphDraw.GraphDrawn := true;
      RedrawDiagram11Click(Nil);
   end;
   SetFocus;
end;


procedure TCorrelationForm.Button1Click(Sender: TObject);
begin
   Histogram1Click(Sender);
end;

procedure TCorrelationForm.Button2Click(Sender: TObject);
begin
   Scattergram1Click(Sender);
end;

procedure TCorrelationForm.CheckBox1Click(Sender: TObject);
begin
   ComboBox3.Enabled := CheckBox1.Checked;
end;




end.
