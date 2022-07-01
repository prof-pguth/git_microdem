unit sieve_main;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,Math,
  Menus,
  PETMAR,Petmar_types;

type
  TSieveMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    Window1: TMenuItem;
    Cascade1: TMenuItem;
    Tile1: TMenuItem;
    NewDiagram1: TMenuItem;
    Open1: TMenuItem;
    Createfile1: TMenuItem;
    Options1: TMenuItem;
    Phiandnormalprobabilityaxis1: TMenuItem;
    MMandarithmeticaxis1: TMenuItem;
    Editfile1: TMenuItem;
    N1: TMenuItem;
    Showstatistics1: TMenuItem;
    Display1: TMenuItem;
    Showsigmas1: TMenuItem;
    LoadImage1: TMenuItem;
    N2: TMenuItem;
    Close2: TMenuItem;
    N3: TMenuItem;
    Onephisieves1: TMenuItem;
    FindPhi1: TMenuItem;
    procedure NewDiagram1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Createfile1Click(Sender: TObject);
    procedure Phiandnormalprobabilityaxis1Click(Sender: TObject);
    procedure MMandarithmeticaxis1Click(Sender: TObject);
    procedure Cascade1Click(Sender: TObject);
    procedure Tile1Click(Sender: TObject);
    procedure Editfile1Click(Sender: TObject);
    procedure Showstatistics1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Showsigmas1Click(Sender: TObject);
    procedure Phitomm1Click(Sender: TObject);
    procedure Mmtophi1Click(Sender: TObject);
    procedure LoadImage1Click(Sender: TObject);
    procedure Onephisieves1Click(Sender: TObject);
    procedure FindPhi1Click(Sender: TObject);
  private                               
    { Private declarations }
  public
    { Public declarations }
    procedure PlotFile(var FileName : PathStr; TheColor : tColor);
  end;


implementation

{$R *.DFM}

uses
   PETMath,PETImage,PetImage_form,
   BaseGraf,
   SievEntr;

var
  ThisGraph : TThisBaseGraph;
  Percentage : float32;

procedure FindPhi(Probability : float32; PlotColor : tColor; var PhiSize : float32);
var
   xl,xr,y : integer;
begin
    y := ThisGraph.GraphDraw.GraphY(Probability);
    xl := ThisGraph.GraphDraw.LeftMargin;
    while (xl <= ThisGraph.GraphDraw.XWindowSize) and (ThisGraph.Image1.Canvas.Pixels[xl,y] <> PlotColor) do inc(xl);
    xr := xl;
    while (xr <= ThisGraph.GraphDraw.XWindowSize) and (ThisGraph.Image1.Canvas.Pixels[succ(xr),y] = PlotColor) do inc(xr);
    PhiSize := ThisGraph.GraphDraw.MinHorizAxis + (0.5 * (xr + xl) - ThisGraph.GraphDraw.LeftMargin) / (ThisGraph.GraphDraw.XWindowSize - ThisGraph.GraphDraw.LeftMargin) * (ThisGraph.GraphDraw.MaxHorizAxis - ThisGraph.GraphDraw.MinHorizAxis);
end;


procedure TSieveMainForm.NewDiagram1Click(Sender: TObject);
begin
   ThisGraph := TThisBaseGraph.Create(Application);
   Caption := 'Sand size distribution';
   ThisGraph.GraphDraw.VertLabel := 'Cumulative Weight Percent Retained';
   if PhiAndNormalProbabilityAxis1.Checked then begin
      ThisGraph.GraphDraw.MinHorizAxis := -2;
      ThisGraph.GraphDraw.MaxHorizAxis := 4;
      ThisGraph.GraphDraw.HorizLabel := 'Phi Size';
      ThisGraph.GraphDraw.VertAxisFunctionType := CumulativeNormalAxis;
      ThisGraph.SetUpGraphForm;
      ThisGraph.ShowNormalDistribution;
   end;
   if MMandArithmeticAxis1.Checked then begin
      ThisGraph.GraphDraw.HorizAxisFunct := Log10;
      ThisGraph.GraphDraw.HorizAxisFunctionType := Log10Axis;
      ThisGraph.GraphDraw.MinHorizAxis := 0.01;
      ThisGraph.GraphDraw.MaxHorizAxis := 10;
      ThisGraph.GraphDraw.HorizLabel := 'Diameter (mm)';
      ThisGraph.SetUpGraphForm;
   end;
   ThisGraph.RedrawDiagram11Click(Nil);
end;


procedure TSieveMainForm.Open1Click(Sender: TObject);
const
   WantFile : PathStr = '';
var
   TheFiles : tStringList;
   DefaultFilter : byte;
   i : integer;
begin
   StopSplashing;
   if (WantFile = '') then WantFile := ProgramRootDir;
   TheFiles := tStringList.Create;
   TheFiles.Add(WantFile);
   DefaultFilter := 0;
   if GetMultipleFiles('Sieve data','Sieve files|*.PHI',TheFiles,DefaultFilter) then begin
      NewDiagram1Click(Sender);
      for i := 0 to pred(TheFiles.Count) do begin
         WantFile := TheFiles.Strings[i];
         PlotFile(WantFile,WinGraphColors[succ(i)]);
      end;
      QuickOpenEditWindow(ProgramRootDir + 'PhiSize.txt','Size Distribution Data');
   end;
end;


procedure TSieveMainForm.PlotFile(var FileName : PathStr; TheColor : tColor);
var
   LegendFile,
   DataFile : System.Text;
   Bitmap : tMyBitmap;
   i,j,NumDataPoints : integer;
   TotalWeight,
   MinDataX,temp,
   Phi16,Phi50,Phi84,
   MaxDataX : float32;
   x,y      : array[1..1000] of float32;
   v        : array[1..2] of float32;
   rfile    : file;
begin
   assignFile(DataFile,FileName);
   reset(DataFile);
   NumDataPoints := 0;
   TotalWeight := 0;
   MinDataX := 99e99;
   MaxDataX := -99e99;
   while not EOF(DataFile) do begin
      if EOLN(DataFile) then readln(DataFile)
      else begin
         inc(NumDataPoints);
         readln(DataFile,x[NumDataPoints],y[NumDataPoints]);
         if x[NumDataPoints] > MaxDataX then MaxDataX := x[NumDataPoints];
         if x[NumDataPoints] < MinDataX then MinDataX := x[NumDataPoints];
         TotalWeight := TotalWeight + y[NumDataPoints];
      end {if};
   end {while};
   closeFile(DataFile);
   if TotalWeight < 0.0001 then exit;
   {bubble sort data}
   for i := 1 to pred(NumDataPoints) do begin
      for j := 1 to pred(NumDataPoints) do begin
         if x[j] > x[succ(j)] then begin
            temp := x[j];
            x[j] := x[succ(j)];
            x[succ(j)] := temp;
            temp := y[j];
            y[j] := y[succ(j)];
            y[succ(j)] := temp;
         end {if};
      end {for j};
   end {for i};

   ThisGraph.OpenDataFile(rfile);
   if ThisGraph.GraphDraw.LegendList = Nil then ThisGraph.GraphDraw.LegendList := tStringList.Create;

   ThisGraph.GraphDraw.LegendList.Add(ExtractFileName(FileName));
   CopyImageToBitmap(ThisGraph.Image1,Bitmap);
   Bitmap.Canvas.Pen.Color := TheColor;
   Bitmap.Canvas.Pen.Width := 3;
   ThisGraph.GraphDraw.FileColors256[ThisGraph.GraphDraw.DataFilesPlotted.Count] := ConvertTColorToPlatformColor(TheColor);
   if MMandArithmeticAxis1.Checked then with ThisGraph do begin
      for i := 1 to NumDataPoints do begin
         y[i] := 100.0 * y[i] / TotalWeight;
         if i > 1 then y[i] := y[i] + y[pred(i)];
         if y[i] < 0.000001 then y[i] := 0.000001;
         x[i] := Math.Power(2,-x[i]);
      end {for i};

      for i := 1 to NumDataPoints do begin
         v[1] := x[i];
         v[2] := y[i];
         BlockWrite(rfile,v,1)
      end;
      CloseFile(rfile);
      with GraphDraw.DataFilesPlotted do PlotAFile(Bitmap,Strings[pred(Count)],1);
      RedrawDiagram11Click(nil);
   end;

      if PhiAndNormalProbabilityAxis1.Checked then with ThisGraph do begin
         if ShowStatistics1.Checked then begin
            if FileExists(ProgramRootDir + 'PhiSize.txt') then begin
               assignFile(LegendFile,ProgramRootDir + 'PhiSize.txt');
               append(LegendFile);
               writeln(LegendFile);
               writeln(LegendFile);
            end
            else begin
               assignFile(LegendFile,ProgramRootDir + 'PhiSize.txt');
               rewrite(LegendFile);
            end;
            writeln(LegendFile,FileName);
            writeln(LegendFile,'Phi    Weight  % Retained  Cum % Retained   Cum % Passed');
         end;
      if {PanWeight} true then begin
         for i := 1 to NumDataPoints do begin
            if ShowStatistics1.Checked then write(LegendFile,x[i]:5:2,y[i]:8:2);
            y[i] := 100.0 * y[i] / TotalWeight;
            if ShowStatistics1.Checked then write(LegendFile,y[i]:12:2);
            if i > 1 then y[i] := y[i] + y[pred(i)];
            if ShowStatistics1.Checked then writeln(LegendFile,y[i]:10:2, (100- y[i]):17:2);
            if y[i] < 0.000001 then y[i] := 0.000001;
         end {for i};
         if ShowStatistics1.Checked then begin
            writeln(LegendFile,'Total weight',TotalWeight:12:2);
            writeln(LegendFile);
         end;
      end
      else begin
         for i := 1 to NumDataPoints do begin
            if ShowStatistics1.Checked then begin
               if i = 1 then write(LegendFile,x[i]:5:2,y[i]:11:2)
               else write(LegendFile,x[i]:5:2,(y[i]-y[pred(i)]):11:2);
               writeln(LegendFile,y[i]:32:2, (100- y[i]):15:2);
               writeln(LegendFile);
            end;
            if y[i] < 0.000001 then y[i] := 0.000001;
         end {for i};
      end;

      for i := 1 to NumDataPoints do begin
            v[1] := x[i];
            v[2] := y[i];
            BlockWrite(rfile,v,1)
      end;
      CloseFile(rfile);
      with GraphDraw.DataFilesPlotted do PlotAFile(Bitmap,Strings[pred(Count)],1);

      ThisGraph.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      if ShowStatistics1.Checked then begin
         FindPhi(16,TheColor,Phi16);
         FindPhi(50,TheColor,Phi50);
         FindPhi(84,TheColor,Phi84);

         RedrawDiagram11Click(nil);
         ThisGraph.ShowNormalDistribution;

         if Phi16 > 4.00 then ReadDefault('Phi16',Phi16);
         if Phi50 > 4.00 then ReadDefault('Phi50',Phi50);
         if Phi84 > 4.00 then ReadDefault('Phi84',Phi84);

         writeln(LegendFile);
         writeln(LegendFile,'Phi16=',Phi16:5:2,'   Phi50=',Phi50:5:2,'   Phi84=',Phi84:5:2);
         if Abs(Phi84 - Phi16) > 0.0001 then
            writeln(LegendFile,'Mean=',(0.3333*(Phi16+Phi50+Phi84)):5:2, '    Sigma (Std dev)=',(0.5*(Phi84-Phi16)):5:2, '    Alpha (skewness)=', ((0.3333*(Phi16+Phi50+Phi84)-Phi50) / (0.5*(Phi84-Phi16))):5:2);
         writeln(LegendFile);
         closeFile(LegendFile);
      end;
   end;
end;


procedure TSieveMainForm.Createfile1Click(Sender: TObject);
var
   fName : PathStr;
   fFile : system.text;
   i,err   : integer;
   Wt,Phi : float32;
begin
   StopSplashing;
   SieveEntryForm := TSieveEntryForm.Create(Application);

   if not OnePhiSieves1.Checked then with SieveEntryForm,StringGrid1 do begin
      RowCount := 12;
      for i := 1 to RowCount do begin
         Cells[1,i] := '0';
         Cells[2,i] := '0';
         Cells[3,i] := '0';
      end;
      Cells[0,1] := '-2.5';
      Cells[0,2] := '-1.5';
      Cells[0,3] := '-0.75';
      Cells[0,4] := '-0.25';
      Cells[0,5] := '0.75';
      Cells[0,6] := '1.0';
      Cells[0,7] := '1.75';
      Cells[0,8] := '2.0';
      Cells[0,9] := '2.75';
      Cells[0,10] := '3.25';
      Cells[0,11] := 'Pan';
   end;

   if (SieveEntryForm.ShowModal <> mrCancel) then begin
      FName := ProgramRootDir;
      if GetFileNameDefaultExt('sieve data','Phi size file|*.PHI',FName) then with SieveEntryForm.StringGrid1 do begin
         assignFile(ffile,fName);
         rewrite(ffile);
         for i := 1 to RowCount do begin
            val(Cells[3,i],Wt,err);
            if Wt < -0.00001 then begin
               val(Cells[0,i],Phi,err);
               MessageToContinue('Negative weight on sieve' + RealToString(Phi,6,2));
               ReadDefault('Corrent weight on Sieve' + RealToString(Phi,6,2),Phi);
            end;
            if (err = 0) then begin
               if Cells[0,i] = 'Pan' then writeln(ffile,'      5',Wt:8:2)
               else begin
                  val(Cells[0,i],Phi,err);
                  if err = 0 then writeln(ffile,Phi:8:2,Wt:8:2);
               end;
            end;
         end;
         closeFile(ffile);
      end;
   end;
   SieveEntryForm.Free;
   if (ThisGraph = Nil) then NewDiagram1Click(Sender);
   PlotFile(fName,clRed);
   QuickOpenEditWindow(ProgramRootDir + 'PhiSize.txt','Size Distribution Data');
end;


procedure TSieveMainForm.Phiandnormalprobabilityaxis1Click(Sender: TObject);
begin
   MMandarithmeticaxis1.Checked := false;
   PhiAndNormalProbabilityAxis1.Checked := true;
end;

procedure TSieveMainForm.MMandarithmeticaxis1Click(Sender: TObject);
begin
   MMandarithmeticaxis1.Checked := true;
   PhiAndNormalProbabilityAxis1.Checked := false;
end;

procedure TSieveMainForm.Cascade1Click(Sender: TObject);
begin
   Cascade;
end;

procedure TSieveMainForm.Tile1Click(Sender: TObject);
begin
   Tile;
end;

procedure TSieveMainForm.Editfile1Click(Sender: TObject);
var
   WantFile : PathStr;
begin
   WantFile := ProgramRootDir;
   if GetFileFromDirectory('Sieve data','*.PHI',WantFile) then ModalEditWindow(WantFile,'Editing ' + WantFile);
end;

procedure TSieveMainForm.Showstatistics1Click(Sender: TObject);
begin
   ShowStatistics1.Checked := not ShowStatistics1.Checked;
end;

procedure TSieveMainForm.Close1Click(Sender: TObject);
begin
   Close;
end;

procedure TSieveMainForm.Showsigmas1Click(Sender: TObject);
begin
   ThisGraph.ShowNormalDistribution;
end;


var
   Size : float32;

procedure TSieveMainForm.Phitomm1Click(Sender: TObject);
begin
   ReadDefault('Phi Size ',size);
   MessageToContinue(RealToString(size,6,2) + ' Phi =' + RealToString(Math.Power(2,-size),5,2) +  ' mm');
end;

procedure TSieveMainForm.Mmtophi1Click(Sender: TObject);
begin
   ReadDefault('Size in mm',size);
   MessageToContinue(RealToString(size,6,2) + ' mm =' + RealToString(-Ln(size)/Ln(2),5,2) +  ' Phi');
end;

procedure TSieveMainForm.LoadImage1Click(Sender: TObject);
var
   LoadedImage : TImageDisplayForm;
begin
   LoadedImage := TImageDisplayForm.Create(Application);
end;

procedure TSieveMainForm.Onephisieves1Click(Sender: TObject);
begin
   Onephisieves1.Checked := not Onephisieves1.Checked;
end;


procedure TSieveMainForm.FindPhi1Click(Sender: TObject);
var
   Phi : float32;
   Bitmap : tMyBitmap;
begin
   if ThisGraph <> Nil then begin
   ReadDefault('Percentage',Percentage);
   with ThisGraph do begin
      CopyImageToBitmap(Image1,Bitmap);
      Bitmap.Canvas.Pen.Color := clRed;
      Bitmap.Canvas.Pen.Width := 3;
      with GraphDraw.DataFilesPlotted do PlotAFile(Bitmap,Strings[pred(Count)],1);
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      FindPhi(Percentage,clRed,Phi);
   end;
   MessageToContinue('Phi =' + RealToString(Percentage,8,-2) + ' is ' +  RealToString(Phi,5,2));
   end;
end;

initialization
   ThisGraph := Nil;
   Size := 0.5;
   Percentage := 50;
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing sieve_main out'); {$EndIf}
end.
