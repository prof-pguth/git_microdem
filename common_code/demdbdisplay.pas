unit demdbdisplay;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDEMDBDisplay}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  System.UITypes,
  Grids, DBGrids, ExtCtrls, Buttons, StdCtrls,
  PETMAR,Petmar_types,demdbtable,DEMdatabase,PetDBUtils;

type
  Tdblimit = class(TForm)
    OKBtn: TBitBtn;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    BitBtn3: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure BitBtn3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
     procedure DrawForm;
  public
    { Public declarations }
    DBGrid1 : TDBGrid;
    LastX,LastY : integer;
    DB : integer;
  end;


procedure ManageDBDisplay(DBGrid : TDBGrid; inDB : integer);


implementation

{$R *.DFM}

uses
    PETImage;


procedure Tdblimit.DrawForm;
var
   Bitmap : tMyBitmap;
   j,Vis : integer;
   TStr : shortString;
begin
   {$IfDef RecordDEMDBDisplay} WriteLineToDebugFile('Tdblimit.DrawForm in ' + IntToStr(DBGrid1.Columns.Count)); {$EndIf}
   PetImage.CreateBitmap(bitmap,180,18 * DBGrid1.Columns.Count);
   Vis := 0;
   for j := 0 to pred(DBGrid1.Columns.Count) do begin
      TStr := DBGrid1.Columns[j].FieldName;
      Bitmap.Canvas.Font.Size := 11;
      GISdb[DB].dbOpts.VisCols[j] := DBGrid1.Columns[j].Visible;
      if DBGrid1.Columns[j].Visible then  begin
         Bitmap.Canvas.Font.Style := [fsbold];
         Bitmap.Canvas.Font.Color := clBlack;
         inc(vis);
      end
      else begin
         Bitmap.Canvas.Font.Style := [];
         Bitmap.Canvas.Font.Color := clSilver;
      end;
      Bitmap.Canvas.TextOut(5,j*18+2,TStr);
   end;
   Label1.Caption := 'Cols=' + IntToStr(DBGrid1.Columns.Count);
   Label2.Caption := 'Vis=' + IntToStr(Vis);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.free;
   ShowDefaultCursor;
end;


procedure ManageDBDisplay(DBGrid : TDBGrid; inDB : integer);
var
   dblimit : Tdblimit;
begin
   {$IfDef RecordDEMDBDisplay} WriteLineToDebugFile('ManageDBDisplay in');  {$EndIf}
   dblimit := Tdblimit.Create(Application);
   dblimit.DBGrid1 := DBGrid;
   dblimit.DB := inDB;
   dblimit.Left := Mouse.CursorPos.X;
   dblimit.Top := Mouse.CursorPos.Y;
   dblimit.DrawForm;
   dblimit.ShowModal;
   GISdb[inDB].dbtablef.ShowStatus;
   {$IfDef RecordDEMDBDisplay} WriteLineToDebugFile('ManageDBDisplay out');  {$EndIf}
end;


procedure Tdblimit.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure Tdblimit.Image1DblClick(Sender: TObject);
var
   OnLine : integer;
begin
   OnLine := LastY div 18;
   if (OnLine >= DBGrid1.Columns.Count) then OnLine := pred(DBGrid1.Columns.Count);
   DBGrid1.Columns[Online].Visible := not DBGrid1.Columns[Online].Visible;
   DrawForm;
end;


procedure Tdblimit.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;


procedure Tdblimit.BitBtn1Click(Sender: TObject);
var
  FName : PathStr;
  fields : tStringList;
  j : integer;
begin
   fName := ProgramRootDir;
   if Petmar.GetFileNameDefaultExt('saved field names','DB fields|*.fields',fname) then begin
      fields := tStringList.Create;
      for j := 0 to pred(DBGrid1.Columns.Count) do begin
         if DBGrid1.Columns[j].Visible then
            Fields.Add(trim(DBGrid1.Columns[j].FieldName));
      end;
      Fields.SaveToFile(fName);
      Fields.Free;
   end;
end;


procedure Tdblimit.BitBtn2Click(Sender: TObject);
var
  FName : PathStr;
  fields : tStringList;
  index,j : integer;
  TStr : ANSIString;
begin
   fName := ProgramRootDir;
   if Petmar.GetFileFromDirectory('restore field names','DB fields|*.fields',fname) then begin
      fields := tStringList.Create;
      Fields.Sorted := true;
      Fields.LoadFromFile(fName);

      for j := 0 to pred(DBGrid1.Columns.Count) do begin
         TStr := trim(DBGrid1.Columns[j].FieldName);
         DBGrid1.Columns[j].Visible := Fields.Find(TStr, Index);
         writeLineToDebugFile(TStr + '   ' + Index.toString);
      end;
      Fields.Free;
      DrawForm;
   end;

end;

procedure Tdblimit.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\db_hide.htm');
end;


procedure Tdblimit.Button1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(DBGrid1.Columns.Count) do DBGrid1.Columns[i].Visible := (Sender = Button2);
   DrawForm;
end;


procedure Tdblimit.Button2Click(Sender: TObject);
begin
   Button1Click(Sender);
end;


initialization
finalization
   {$IfDef RecordDEMDBDisplay} WriteLineToDebugFile('RecordDEMDBDisplay active in DEMDBDisplay'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demdbdisplay out'); {$EndIf}
end.





