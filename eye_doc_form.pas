unit eye_doc_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Buttons;

type
  TEyeDocF = class(TForm)
    StringGrid1: TStringGrid;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    OpenGLSpeedButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure OpenGLSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EyeDocF: TEyeDocF;

implementation

{$R *.dfm}
uses
{$IfDef OldOpenGL}
   ogl_DEMOpenGLMain,
{$EndIf}
   Petmar,Petmar_types,DEMCoord,DEMdefs,Read_DEM,Nevadia_main;



procedure TEyeDocF.BitBtn1Click(Sender: TObject);
const
   Start : array[1..10] of byte = (3,2,1,0,0,0,0,1,2,3);
var
   fName : PathStr;
   i,j : integer;
   aLine : AnsiString;
   inf : tStringList;


   procedure ProcessLine(Values : Ansistring; Line : integer);
   var
      i,NVals : integer;
   begin
      for i := 1 to length(Values) do if (not (Values[i] in ['0'..'9'])) then Values[i] := ' ';
      Values := ptTrim(Values);
      for I := length(Values) downto 2 do if (Values[i] = ' ') and (Values[pred(i)] = ' ') then Delete(Values,i,1);
     // NVals := 1;
     // for i := 1 to length(Values) do if Values[i] = ' ' then inc(NVals);
      NVals := 0;
      repeat
          Inc(NVals);
          StringGrid1.Cells[Start[Line]+Nvals,Line] := Petmar_types.BeforeSpecifiedCharacter(Values,' ',true,true);
      until (Values = '');

   end;

begin
   fName := 'C:\mapdata\eye_doc\newone.txt';
   Petmar.GetFileFromDirectory('data','*.txt;*.asc',fName);
   inf := tStringList.Create;
   inf.LoadFromFile(fName);
   if UpperCase(ExtractFileExt(fName)) = '.TXT' then begin
      for i := 0 to 9 do ProcessLine(inf.Strings[i],succ(i));
   end
   else begin
      for i := 6 to 15 do begin
         aline := inf.Strings[i];
         for j := 1 to 10 do
            StringGrid1.Cells[i-5,j] := Petmar_types.BeforeSpecifiedCharacter(aline,' ',true,true);
         FormCreate(Nil);
      end;
   end;
   Inf.Free;
end;


procedure TEyeDocF.BitBtn2Click(Sender: TObject);
begin
   BitBtn3Click(Nil);
   MDDef.DefDEMMap := mtDEMChromaDepth;
   {$IfDef MICRODEM}
   wmdem.OpenNewDEM(MDTempDir + 'eyedoc.asc');
   {$EndIf}
end;

procedure TEyeDocF.BitBtn3Click(Sender: TObject);
var
   Outf : tStringList;
   Line : AnsiString;
   fName : PathStr;
   i,j: Integer;
begin
   outf := tStringList.Create;
   outf.Add('ncols 10');
   outf.Add('nrows 10');
   outf.Add('xllcorner 0');
   outf.Add('yllcorner 0');
   outf.Add('cellsize 1');
   outf.Add('NODATA_value -9999');
   for i := 1 to 10 do begin
      Line := '';
      for j := 1 to 10 do begin
         if StringGrid1.Cells[i,j] = 'XXXX' then Line := Line + '0 ' //'-9999 '
         else Line := Line + StringGrid1.Cells[i,j] + ' ';
      end;
      Outf.Add(Line);
   end;
   if (Sender = Nil) then fName := MDTempDir + 'eyedoc.asc'
   else begin
      fName := MainMapData + 'eye_doc\';
      Petmar.GetFileNameDefaultExt('data file','*.asc',fName);
   end;
   outf.SaveToFile(fName);
   outf.Clear;
   outf.SaveToFile(ExtractFilePath(fName) + 'rect.txt');
   outf.Free;
end;


procedure TEyeDocF.FormCreate(Sender: TObject);
const
   NoData : array[1..10,1..10] of byte =
    ((1,1,1,0,0,0,0,1,1,1),
     (1,1,0,0,0,0,0,0,1,1),
     (1,0,0,0,0,0,0,0,0,1),
     (0,0,0,0,0,0,0,0,0,0),
     (0,0,0,0,0,0,0,0,0,0),
     (0,0,0,0,0,0,0,0,0,0),
     (0,0,0,0,0,0,0,0,0,0),
     (1,0,0,0,0,0,0,0,0,1),
     (1,1,0,0,0,0,0,0,1,1),
     (1,1,1,0,0,0,0,1,1,1));
var
   i,j : integer;
begin
    with StringGrid1 do begin
       Cells[1,0] := 'A';
       Cells[2,0] := 'B';
       Cells[3,0] := 'C';
       Cells[4,0] := 'D';
       Cells[5,0] := 'E';
       Cells[6,0] := 'F';
       Cells[7,0] := 'G';
       Cells[8,0] := 'H';
       Cells[9,0] := 'I';
       Cells[10,0] := 'J';
       for I := 1 to 10 do Cells[0,i] := IntToStr(i);
       for i := 1 to 10 do
          for j := 1 to 10 do
            if NoData[i,j] = 1 then Cells[j,i] := 'XXXX';
    end;
    if Sender <> Nil then begin
        MDDef.OGLDefs.DefVertExag := 0.05;
        wmdem.FormPlacementInCorner(self);
    end;
end;



procedure TEyeDocF.OpenGLSpeedButtonClick(Sender: TObject);
begin
{$IfDef OldOpenGL}
   with DEMGlb[1],HeadRecs do
      OpenGLView(1,0,0,pred(NumCol),pred(NumRow));
{$EndIf}
end;

end.
