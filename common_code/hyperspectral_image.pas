unit hyperspectral_image;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordHyperspectral}
   //{$Define RecordMakeBitmap}
   //{$Define RecordReadFile}
   //{$Define TimeHyperspectral}
{$EndIf}

interface

uses
   Graphics,Sysutils,Winapi.Windows,
   Petmar,Petmar_types,BaseGraf;

const
   MaxHyperBands = 224;
   MaxCols = 512;
type
   tSingleRow = array[0..(MaxHyperBands*614)-1] of SmallInt;
   tBILRow = array[0..pred(MaxCols)] of SmallInt;
   tHypData =(hdAviris,hdHICO);

type
   tHypersectralImage = class
   protected
   private
      SingleRow : tSingleRow;
      RedRow,GreenRow,BlueRow,GrayRow : tBILRow;
      function GetBILRow(Band,Row : integer) : tBILRow;
   public
      HypData : tHypData;
      AvirisFile : file;
      RedBand,BlueBand,GreenBand,GrayBand,
      NumCols,NumRows,NumBands : integer;
      HyperFilename : PathStr;
      Scatter1,Scatter2,
      RefGraph,RefGraph2,
      Histogram  : tThisBaseGraph;
      BandCenters : array[1..MaxHyperBands] of float32;
      constructor Create(FName : PathStr; var Success : boolean);
      destructor Destroy; override;
      function GetBandBitmap(Gray,Red,Green,Blue : integer) : tMyBitmap;
      procedure DisplayCube(Gray,Red,Green,Blue : integer; x : integer = 0; y : integer = 0);
      procedure DisplayHistogram(Gray,Red,Green,Blue : integer);
      procedure ScatterPlot(Red,Green,Blue : integer);
      procedure CreateTable;
      procedure OpenHypFile;
      procedure CloseHypFile;
   end;


implementation

uses
   DEMDefs,
   Petimage_form,Petmath,
   PetImage,petdbutils,Make_Tables,Nevadia_main;


procedure tHypersectralImage.CloseHypFile;
begin
   CloseFile(AvirisFile);
end;

constructor tHypersectralImage.Create(FName : PathStr; var Success : boolean);
var
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;

   procedure ReadBandCenters(fName : PathStr);
   var
      tf : TextFile;
      i : integer;
   begin
      assignFile(tf,fName);
      reset(tf);
      for i  := 1 to NumBands do readln(tf,BandCenters[i]);
      CloseFile(tf);
   end;

begin
   Histogram := Nil;
   RefGraph := Nil;
   RefGraph2 := Nil;
   Scatter1 := Nil;
   Scatter2 := Nil;
   Success := FileExists(fName);
   if Success then begin
      HyperFilename := fName;
      FSplit(fName,Dir,bName,Ext);
      if ExtEquals(Ext,'.RFL') then begin
          HypData := hdAviris;
          NumCols := 614;
          NumRows := 512;
          NumBands := 224;
          Delete(bName,Length(bName)-6,7);
          fName := Dir + bName + '.a.spc';
          ReadBandCenters(fName);
          RedBand := 128;
          GreenBand := 33;
          BlueBand := 10;
      end
      else if ExtEquals(Ext, '.BIL') then begin
          HypData := hdHICO;
          NumCols := 512;
          NumRows := 2000;
          NumBands := 128;
          RedBand := 100;
          GreenBand := 33;
          BlueBand := 10;
      end;
   end;
end;


procedure tHypersectralImage.CreateTable;
begin
end;


destructor tHypersectralImage.Destroy;

         procedure DestroyGraph(Graph : tThisBasegraph);
         begin
            if (Graph <> Nil) then begin
               Graph.CanCloseGraph := true;
               Graph.Destroy;
            end;
         end;

begin
   inherited;
   DestroyGraph(Histogram);
   DestroyGraph(RefGraph);
   DestroyGraph(RefGraph2);
   DestroyGraph(Scatter1);
   DestroyGraph(Scatter2);
end;


procedure SetColors(ColorImage : boolean; SingleRow : tSingleRow; NumBands,j,Gray,Red,Green,Blue : integer; var ref,RefRed,RefGreen,RefBlue : integer);  inline;

    function BandValue(SingleRow : tSingleRow; j,Band : integer) : integer;  inline;
    begin
       Result := Swap(SingleRow[pred(Band) + j*224]) div 25;
       Result := ValidByteRange(Result);
    end;

begin
   if ColorImage then begin
      RefBlue  := BandValue(SingleRow,j,Blue);
      RefRed   := BandValue(SingleRow,j,Red);
      RefGreen := BandValue(SingleRow,j,Green);
      Ref := 0;
   end
   else begin
      Ref := BandValue(SingleRow,j,Gray);
      RefBlue  := Ref;
      RefRed   := Ref;
      RefGreen := Ref;
   end;
end;


function tHypersectralImage.GetBILRow(Band,Row : integer) : tBILRow;
begin
   {$IfDef RecordReadFile} WriteLineToDebugFile('tHypersectralImage.GetBILRow ' + IntToStr(Band) + '-' + IntToStr(Row)); {$EndIf}
   seek(AvirisFile,Row*NumBands + pred(Band));
   BlockRead(AvirisFile,Result,1);
end;



function tHypersectralImage.GetBandBitmap(Gray,Red,Green,Blue : integer) : tMyBitmap;
var
   i,j,ref,RefBlue,RefRed,RefGreen : integer;
   p0 : pRGB;
   ColorImage : boolean;
begin
   {$IfDef RecordMakeBitmap} WritelineToDebugFile('tHypersectralImage.GetBandBitmap in'); {$EndIf}
   ColorImage := (Gray = 0);
   OpenHypFile;
   CreateBitmap(Result,NumCols,NumRows);
   if (HypData = hdAviris) then begin
       for i := 0 to pred(NumRows) do begin
         {$IfDef RecordMakeBitmap}
         WritelineToDebugFile('Row=' + IntToStr(i));
         {$EndIf}
          if eof(AvirisFile) then break;
          BlockRead(AvirisFile,SingleRow,1);
          p0 := Result.ScanLine[i];
          for j := 0 to Pred(NumCols) do begin
             SetColors(ColorImage,SingleRow,NumBands,j,Gray,Red,Green,Blue,ref,RefRed,RefGreen,RefBlue);
             p0[j] := RGBtrip(RefRed,RefGreen,RefBlue);
          end;
       end;
   end
   else begin
       for i := 0 to pred(NumRows) do begin
          p0 := Result.ScanLine[i];
          if ColorImage then begin
             RedRow := GetBILRow(Red,i);
             BlueRow := GetBILRow(Blue,i);
             GreenRow := GetBILRow(Green,i);
             for j := 0 to Pred(NumCols) do begin
                RefRed := ValidByteRange(RedRow[j] div 25);
                RefBlue := ValidByteRange(BlueRow[j] div 25);
                RefGreen := ValidByteRange(GreenRow[j] div 25);
                p0[j] := RGBtrip(RefRed,RefGreen,RefBlue);
             end;
          end
          else begin
             GrayRow := GetBILRow(Gray,i);
             for j := 0 to Pred(NumCols) do begin
                RefRed := ValidByteRange(GrayRow[j] div 25);
                p0[j] := RGBtrip(RefRed,RefRed,RefRed);
             end;
          end;
       end;
   end;
   CloseHypFile;
   {$IfDef RecordMakeBitmap} WritelineToDebugFile('tHypersectralImage.GetBandBitmap out'); {$EndIf}
end;


procedure tHypersectralImage.OpenHypFile;
begin
   AssignFile(AvirisFile,HyperFilename);
   if (HypData = hdAviris) then Reset(AvirisFile,NumBands*NumCols*2)
   else Reset(AvirisFile,NumCols*2)
end;


procedure tHypersectralImage.DisplayCube(Gray,Red,Green,Blue : integer; x : integer = 0; y : integer = 0);
var
   i,j,Band,ref,RefBlue,RefRed,RefGreen : integer;
   p0 : pRGB;
   ColorImage : boolean;
    k: Integer;
    BMP : tMyBitmap;
begin
   ColorImage := (Gray = 0);
   CreateBitmap(BMP,NumCols+NumRows,NumRows + NumBands);
   if x = 0 then x := pred(NumCols);
   if y = 0 then y := pred(NumRows);
   OpenHypFile;
   if (HypData = hdAviris) then begin
       for i := 0 to y do begin
          if eof(AvirisFile) then break;
          BlockRead(AvirisFile,SingleRow,1);
          p0 := BMP.ScanLine[i];
          for j := 0 to x do begin
             SetColors(ColorImage,SingleRow,NumBands,j,Gray,Red,Green,Blue,ref,RefRed,RefGreen,RefBlue );
             p0[j + y-i] := RGBtrip(RefRed,RefGreen,RefBlue);
          end;
          for Band := 1 to NumBands do begin
             p0 := BMP.ScanLine[i+Band];
             SetColors(false,SingleRow,x,NumBands,Band,Red,Green,Blue,Ref,RefBlue,RefRed,RefGreen);
             p0[j + y-i] := RGBtrip(Ref,Ref,Ref);
          end;
       end;

       for Band := 1 to NumBands do begin
          p0 := BMP.ScanLine[y+Band];
          for k := 0 to x do begin
             SetColors(false,SingleRow,NumBands,k,Band,Red,Green,Blue,ref,RefRed,RefGreen,RefBlue );
             p0[k] := RGBtrip(Ref,Ref,Ref);
          end;
       end;
   end
   else begin
       MessageToContinue('Not yet implemented');
   end;

   CloseFile(AvirisFile);
   PetImage_form.DisplayBitmap(BMP,'Image cube');
   BMP.Free;
end;


procedure tHypersectralImage.DisplayHistogram(Gray,Red,Green,Blue : integer);
var
   Hist : array[1..3,0..255] of LongInt;
   NumBands,xp,yp,i,j,ref,RefBlue,RefRed,RefGreen : integer;
   ColorImage : boolean;
begin
   if (Histogram = Nil) then begin
      Histogram := TThisBaseGraph.Create(Nil);
      Histogram.CanCloseGraph := false;
      Histogram.Caption := 'Hyperspectral band histograms';
      Histogram.GraphDraw.MaxHorizAxis := 255;
      Histogram.GraphDraw.HorizLabel := 'Reflectance';
      Histogram.GraphDraw.VertLabel := 'Number of Points';
      Histogram.GraphDraw.FileColors256[1] := claTeal;
      Histogram.SetUpGraphForm;
   end;
   ColorImage := (Gray = 0);
   OpenHypFile;
   for i := 0 to 255 do
      for j := 1 to 3 do
          Hist[j,i] := 0;

   if (HypData = hdAviris) then begin
     for i := 0 to pred(NumRows) do begin
        if eof(AvirisFile) then break;
        BlockRead(AvirisFile,SingleRow,1);
        for j := 0 to pred(NumCols) do begin
           SetColors(ColorImage,SingleRow,NumBands,j,Gray,Red,Green,Blue,ref,RefRed,RefGreen,RefBlue);
           if ColorImage then begin
              inc(Hist[1,RefRed]);
              inc(Hist[2,RefGreen]);
              inc(Hist[3,RefBlue]);
           end
           else begin
              inc(Hist[1,Ref]);
           end;
        end;
     end;
   end
   else begin
     for i := 0 to pred(NumRows) do begin
          if ColorImage then begin
             RedRow := GetBILRow(Red,i);
             BlueRow := GetBILRow(Blue,i);
             GreenRow := GetBILRow(Green,i);
             for j := 0 to Pred(NumCols) do begin
                RefRed := ValidByteRange(RedRow[j] div 25);
                RefBlue := ValidByteRange(BlueRow[j] div 25);
                RefGreen := ValidByteRange(GreenRow[j] div 25);
                inc(Hist[1,RefRed]);
                inc(Hist[2,RefGreen]);
                inc(Hist[3,RefBlue]);
             end;
          end
          else begin
             GrayRow := GetBILRow(Gray,i);
             for j := 0 to Pred(NumCols) do begin
                RefRed := ValidByteRange(GrayRow[j] div 25);
                inc(Hist[1,RefRed]);
              end;
          end;
        end;
     end;

   Histogram.GraphDraw.MaxVertAxis := 0;
   for i := 0 to 255 do begin
      for J := 1 to 3 do
         if Hist[j,i] > Histogram.GraphDraw.MaxVertAxis then Histogram.GraphDraw.MaxVertAxis := Hist[j,i];
   end;
   Histogram.RedrawDiagram11Click(nil);
   Histogram.Image1.Canvas.Pen.Width := 3;
   if ColorImage then begin
      NumBands := 3;
      Histogram.Caption := 'Hyperspectral histogram, ' + RGBString(Red,Green,Blue);
   end
   else begin
      NumBands := 1;
      Histogram.Caption := 'Hyperspectral histogram, Band=' + IntToStr(Gray);
   end;

   for J := 1 to NumBands do begin
      case j of
         1 : Histogram.Image1.Canvas.Pen.Color := clRed;
         2 : Histogram.Image1.Canvas.Pen.Color := clLime;
         3 : Histogram.Image1.Canvas.Pen.Color := clBlue;
      end;
      for i := 0 to 255 do begin
         xp := Histogram.GraphDraw.GraphX(i);
         yp := Histogram.GraphDraw.GraphY(Hist[j,i]);
         if i = 0 then Histogram.Image1.Canvas.MoveTo(xp,yp)
         else Histogram.Image1.Canvas.LineTo(xp,yp)
      end;
   end;
   CloseHypFile;
end;


procedure tHypersectralImage.ScatterPlot(Red,Green,Blue : integer);
var
   i,j,ref,RefBlue,RefRed,RefGreen : integer;
   File1,File2 : file;
   v1,v2 : array[0..613,1..2] of float64;
   Symbol : tFullSymbolDeclaration;
begin
   {$IfDef TimeHyperspectral} WriteLineToDebugFile('tHypersectralImage.ScatterPlot in'); {$EndIf}
   if (Scatter1 = Nil) then begin
      Scatter1 := TThisBaseGraph.Create(Nil);
      Scatter1.CanCloseGraph := false;
      Scatter1.Caption := 'Hyperspectral band scatterplot';
      Scatter1.GraphDraw.MaxHorizAxis := 255;
      Scatter1.GraphDraw.MaxVertAxis := 255;
   end;
   Scatter1.GraphDraw.HorizLabel := 'Band ' + IntToStr(Red) + ' (' + RealToString(BandCenters[Red],-12,-2) + ' nm)';
   Scatter1.GraphDraw.VertLabel := 'Band ' + IntToStr(Green) + ' (' + RealToString(BandCenters[Green],-12,-2) + ' nm)';;
   Scatter1.ClearDataOnGraph;
   Symbol.Size := 2;
   Symbol.Color := claRed;
   Symbol.DrawingSymbol := FilledBox;
   Scatter1.OpenPointSymbolFile(file1,'hyper',Symbol);

   if (Scatter2 = Nil) then begin
      Scatter2 := TThisBaseGraph.Create(Nil);
      Scatter2.CanCloseGraph := false;
      Scatter2.Caption := 'Hyperspectral band scatterplot';
      Scatter2.GraphDraw.MaxHorizAxis := 255;
      Scatter2.GraphDraw.MaxVertAxis := 255;
   end;
   Scatter2.GraphDraw.HorizLabel := 'Band ' + IntToStr(Red) + ' (' + RealToString(BandCenters[Red],-12,-2) + ' nm)';
   Scatter2.GraphDraw.VertLabel := 'Band ' + IntToStr(Blue) + ' (' + RealToString(BandCenters[Blue],-12,-2) + ' nm)';
   Symbol.Color := claLime;
   Scatter2.OpenPointSymbolFile(file2,'hyper',Symbol);

   OpenHypFile;
   if (HypData = hdAviris) then begin
       for i := 0 to pred(NumRows) do begin
          if eof(AvirisFile) then break;
          BlockRead(AvirisFile,SingleRow,1);
          for j := 0 to pred(NumCols) do begin
             SetColors(true,SingleRow,NumBands,j,1,Red,Green,Blue,ref,RefRed,RefGreen,RefBlue );
             v1[j,1] := RefRed;
             v1[j,2] := RefGreen;
             v2[j,1] := RefRed;
             v2[j,2] := RefBlue;
          end;
          BlockWrite(file1,v1,NumCols);
          BlockWrite(file2,v2,NumCols);
       end;
   end;
   {$IfDef TimeHyperspectral} WriteLineToDebugFile('Files written'); {$EndIf}
   CloseFile(file1);
   CloseFile(File2);
   Scatter1.AutoScaleAndRedrawDiagram(false,false,false,false);
   Scatter2.AutoScaleAndRedrawDiagram(false,false,false,false);

   CloseHypFile;
   {$IfDef TimeHyperspectral} WriteLineToDebugFile('tHypersectralImage.ScatterPlot out'); {$EndIf}
end;


initialization
finalization
   {$IfDef RecordHyperspectral} WriteLineToDebugFile('RecordHyperspectral active in hypespectral_image'); {$EndIf}
   {$IfDef TimeHyperspectral} WriteLineToDebugFile('TimeHyperspectral active in hypespectral_image'); {$EndIf}
end.


