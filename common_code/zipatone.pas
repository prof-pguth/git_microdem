unit zipatone;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}


interface

uses
   PETMAR,petmar_types,Graphics,SysUtils;

const
   MaxPatterns = 200;

type
   tPatternMask   = packed array[0..2,0..23] of byte;

   tPatternRecord = Packed Record
      PatternName   : string3;
      NumCols,
      NumRows,
      EGAColor      : byte;
      WinColor      : TColor;
      PatternMasks  : tPatternMask;
   end;
   ColorAndPatternOptionsType = (ColoredPatterns,SolidColors,BlackAndWhitePatterns,NoPatterns);
   tPatternPointer = array[1..MaxPatterns] of tPatternRecord;

var
   StandardPattern  : ^tPatternPointer;
   NumStandardPattern  : integer;
   PatternFileName : PathStr;

function ReadPatternFile : boolean;
function PatternFromString(Name : string3) : tPatternRecord;
procedure FillInBox(Canvas : TCanvas; StartCol,StartRow,EndCol,EndRow : integer; CurrentPattern : tPatternRecord; DrawBox : boolean);
procedure SaveZipatonesToBMP;
procedure SaveCurrentPatternAsBMP( CurrentPattern : tPatternRecord);


implementation

uses
   DEMDefs,PetImage;

procedure SaveZipatonesToBMP;
var
   i : integer;
begin
   for i := 1 to NumStandardPattern do begin
      SaveCurrentPatternAsBMP(StandardPattern^[i]);
   end;
end;

procedure SaveCurrentPatternAsBMP(CurrentPattern : tPatternRecord);
var
   x,y,j      : integer;
   bmp : tMyBitmap;
begin
   CreateBitmap(Bmp,CurrentPattern.NumCols,CurrentPattern.NumRows);
   if (CurrentPattern.NumRows > 0) and (CurrentPattern.NumCols > 0) then begin
      for x := 0 to pred(CurrentPattern.NumCols) do
         for y := 0 to pred(CurrentPattern.NumRows) do
            if (CurrentPattern.PatternMasks[y mod CurrentPattern.NumRows div 8,x mod CurrentPattern.NumCols] and MaskBit[y mod CurrentPattern.NumRows mod 8]) > 0 then
               bmp.Canvas.Pixels[x,y] := CurrentPattern.WinColor;
      for j := 1 to 3 do if CurrentPattern.PatternName[j] = '?' then CurrentPattern.PatternName[j] := 'Q';

      bmp.SaveToFile(MainMapData + 'zipatone\' + CurrentPattern.PatternName + '.bmp');
      bmp.free;
   end;
end {proc};


procedure FillInBox(Canvas : TCanvas; StartCol,StartRow,EndCol,EndRow : integer; CurrentPattern : tPatternRecord; DrawBox : boolean);
var
   x,y : integer;
begin
   Canvas.Pen.Color := clBlack;
   Canvas.Pen.Width := 1;
   if DrawBox then  begin
      Canvas.MoveTo(StartCol,StartRow);
      Canvas.LineTo(EndCol,StartRow);
      Canvas.LineTo(EndCol,EndRow);
      Canvas.LineTo(StartCol,EndRow);
      Canvas.LineTo(StartCol,StartRow);
   end;
   with CurrentPattern,Canvas do if (NumRows > 0) and (NumCols > 0) then begin
         for x := succ(StartCol) to pred(EndCol) do
            for y := succ(StartRow) to pred(EndRow) do
               if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) > 0 then
                  Pixels[x,y] := WinColor;
   end {with};
end {proc};


function PatternFromString(Name : string3) : tPatternRecord;
var
   x  : integer;
begin
   if (StandardPattern = Nil) then ReadPatternFile;
   x := 1;
   while (Name <> StandardPattern^[x].PatternName) and (x < NumStandardPattern) do inc(x);
   Result := StandardPattern^[x];
end {proc PatternFromString};


function ReadPatternFile : boolean;
var
   PatternFile : file of tPatternRecord;
begin
   {$IfDef RecordProblems} WriteLineToDebugFile('Pattern file read: ' + PatternFileName + '  in'); {$EndIf}
   if (StandardPattern = Nil) then  begin
      PatternFileName := ProgramRootDir + 'rocks.pat';
      New(StandardPattern);
      NumStandardPattern := 0;
      if not FileExists(PatternFileName) then begin
         {$IfDef RecordProblems} WriteLineToDebugFile('Pattern file missing: ' + PatternFileName); {$EndIf}
         result := false;
         exit;
      end {while};
      InsureFileIsNotReadOnly(PatternFileName);
      assign(PatternFile,PatternFileName);
      reset(PatternFile);
      while not EOF(PatternFile) and (NumStandardPattern < MaxPatterns) do begin
         inc(NumStandardPattern);
         read(PatternFile,StandardPattern^[NumStandardPattern]);
      end {while};
      close(PatternFile);
      Result := true;
      {$IfDef RecordProblems}WriteLineToDebugFile('Pattern file read: ' + PatternFileName + ' out with Patterns read: ' + IntToStr(NumStandardPattern)); {$EndIf}
   end
   else begin
      {$IfDef RecordProblems} WriteLineToDebugFile('Pattern file already read'); {$EndIf}
   end;
end {proc ReadPatternFile};


initialization
   StandardPattern := Nil;
finalization
   if (StandardPattern <> Nil) then Dispose(StandardPattern);
end.
