unit gis_legend;

removed 4/30/2020

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, SysUtils,  Classes, Graphics, Forms, Dialogs, Grids, ClipBrd,
  Petmar_types, Petmar_db,
  DEMDataBase,PetDBUtils, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.Controls;

type
  TGIS_leg_form = class(TForm)
    DrawGrid1: TDrawGrid;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    Quick: TBitBtn;
    SpeedButton1: TSpeedButton;
    ClipboardSpeedButton: TSpeedButton;
    RadioGroup2: TRadioGroup;
    FontDialog1: TFontDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    BitBtn6: TBitBtn;
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure QuickClick(Sender: TObject);
    procedure DrawGrid1Click(Sender: TObject);
    procedure DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure ClipboardSpeedButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
  private
    { Private declarations }
    procedure SizeForm;
  public
    { Public declarations }
    Table : tMyData;
    aGISDB : TGISdataBaseModule;
    CaptionField,UseField : ShortString;
    LastX,LastY,Lastx2,Lasty2 : integer;
  end;


procedure CreateGISLegend(fName : PathStr; GISDB : TGISdataBaseModule);


implementation

{$R *.dfm}

uses
   {$IfDef ExZipatone}
   {$Else}
   sc_ColLith,ZipATone,
   {$EndIf}
   Petmar,Toggle_db_use, DEMESRIShapeFile,
   PETImage;

const
   BoxMargin : integer = 2;
   TextHoriz : integer = 5;
   TextVert  : integer = 3;


procedure CreateGISLegend(fName : PathStr; GISDB : TGISdataBaseModule);
var
   GIS_leg_form : TGIS_leg_form;
begin
   GIS_leg_form := TGIS_leg_form.Create(Application);
   if (GISDB = Nil) then begin
      GIS_leg_form.Table := tMyData.Create(fName);
      GIS_leg_form.Caption := ExtractFileName(fName) + ' legend';
      GIS_leg_form.BitBtn6.Enabled := false;
   end
   else begin
      GIS_leg_form.Table := GISDB.LayerTable;
   end;
   if GIS_leg_form.Table.FieldExists('PLOT') then GIS_leg_form.UseField := 'PLOT'
   else GIS_leg_form.UseField := 'USE';
   if GIS_leg_form.Table.FieldExists('CAPTION') then GIS_leg_form.CaptionField := 'CAPTION'
   else GIS_leg_form.UseField := 'NAME';
   GIS_leg_form.Table.ApplyFilter( GIS_leg_form.UseField + '=' + QuotedStr('Y'));
   GIS_leg_form.aGISDB := GISDB;
   GIS_leg_form.SizeForm;
   GIS_leg_form.DrawGrid1.RowCount := succ(GIS_leg_form.Table.RecordCount);
   GIS_leg_form.FormStyle := fsStayOnTop;
   GIS_leg_form.Show;
end;


procedure TGIS_leg_form.BitBtn6Click(Sender: TObject);
begin
   if (aGISDB <> Nil) then with agisDB,theMapOwner do if (theMapOwner <> Nil) then begin
      aGISDB.RedrawLayerOnMap;
   end;
end;

procedure TGIS_leg_form.ClipboardSpeedButtonClick(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   PetImage.CreateBitmap(Bitmap,DrawGrid1.Width,DrawGrid1.Height);
   Bitmap.Canvas.CopyRect(Rect(0,0,pred(Bitmap.Width),pred(Bitmap.Height)),DrawGrid1.Canvas,Rect(0,0,pred(DrawGrid1.Width),pred(DrawGrid1.Height)));
   Clipboard.Assign(Bitmap);
end;

procedure TGIS_leg_form.DrawGrid1Click(Sender: TObject);
var
   i,Col,Row : integer;
   ZipName : string3;
   What : string35;
begin
   DrawGrid1.MouseToCell(LastX,LastY,Col,Row);
   Table.First;
   for i := 1 to pred(Row) do Table.Next;
   What := Table.GetFieldByNameAsString(CaptionField);
   Table.Edit;
   if (Col = 1) then begin
      Petmar.GetString('New caption',What,false,ReasonableTextChars);
      Table.Edit;
      Table.SetFieldByNameAsString(CaptionField,What);
   end
   else begin
      aGISDB.SetColorsFromDB(aGISDB.LayerTable);

      if aGISDB.ItsAPointDB then begin
         //aGISDB.PickSymbolForGIS(Nil,What);
      end;

      if AreaShapeFile(aGISDB.ShapeFileType) then begin
         {$IfDef ExZipatone}
         with aGISDB.dbOpts do begin
            Petmar.PickPattern(What,AreaSymbolFill,FillColor,LineColor,LineWidth);
         end;
         {$Else}
         if (RadioGroup1.ItemIndex = 0) then with aGISDB.dbOpts do  begin
            Petmar.PickPattern(What,AreaSymbolFill,FillColor,LineColor,LineWidth);
         end
         else begin
            ZipName := sc_ColLith.GetAPatternName(What);
         end;
         {$EndIf}
      end;
   end;
   Table.Edit;
   aGISDB.WriteDisplaySymbology(aGISDB.LayerTable);
   Table.Post;
end;


procedure TGIS_leg_form.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   i : integer;
   {$IfDef ExZipatone}
   {$Else}
   ThisPattern : tPatternRecord;
   {$EndIf}
begin
    if (ACol = 0) and (aRow = 0) then begin
        DrawGrid1.Canvas.TextOut(Rect.Left + TextHoriz, Rect.Top + TextVert, Edit1.Text);
    end;
    if (ACol = 1) and (aRow = 0) then begin
        DrawGrid1.Canvas.TextOut(Rect.Left + TextHoriz, Rect.Top + TextVert, Edit2.Text);
    end;
    DrawGrid1.Canvas.Brush.Style := bsClear;
    DrawGrid1.Canvas.Brush.Color := clWhite;
    DrawGrid1.Canvas.Pen.Color := clWhite;
    if (ACol = 1) and (aRow > 0) then begin
       with DrawGrid1 do begin
          Canvas.FillRect(Rect);
          Table.First;
          for i := 1 to pred(ARow) do Table.Next;
          Canvas.TextOut(Rect.Left + TextHoriz, Rect.Top + TextVert, Table.GetFieldByNameAsString(CaptionField));
       end;
    end;
    if (ACol = 0) and (aRow > 0) then with aGISDB.dbOpts do begin
       with DrawGrid1 do begin
          Canvas.FillRect(Rect);
          Table.First;
          for i := 1 to pred(ARow) do Table.Next;

          aGISDB.SetColorsFromDB(Table);

          if aGISDB.ItsAPointDB then begin
             ScreenSymbol(Canvas,(Rect.Left+Rect.Right) div 2, (Rect.Top+Rect.Bottom) div 2,Symbol);//,SymbolSize,BasicColor);
          end;

          if AreaShapeFile(aGISDB.ShapeFileType) then begin
             if (aGISDB.ZipPatName <> '') then begin
               {$IfDef ExZipatone}
               {$Else}
                ThisPattern := PatternFromString(aGISDB.ZipPatName);
                FillInBox(Canvas,Rect.Left +  BoxMargin, Rect.Top +  BoxMargin,Rect.Right- BoxMargin,Rect.Bottom -  BoxMargin,ThisPattern,true);
                {$EndIf}
             end
             else begin
                Canvas.Brush.Style := AreaSymbolFill;
                Canvas.Brush.Color := ConvertPlatformColorToTColor(FillColor);
                Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColor);
                Canvas.Pen.Width := LineWidth;
                Canvas.Rectangle(Rect.Left +  BoxMargin, Rect.Top +  BoxMargin,Rect.Right- BoxMargin,Rect.Bottom -  BoxMargin);
                Canvas.Brush.Style := bsClear;
                Canvas.Brush.Color := clWhite;
                Canvas.Pen.Color := clWhite;
             end;
          end;
       end;
    end;
end;


procedure TGIS_leg_form.DrawGrid1MouseMove(Sender: TObject; Shift: TShiftState;  X, Y: Integer);
begin
    LastX := x;
    LastY := y;
end;

procedure TGIS_leg_form.Edit1Change(Sender: TObject);
begin
   DrawGrid1.Repaint;
end;

procedure TGIS_leg_form.Edit2Change(Sender: TObject);
begin
   DrawGrid1.Repaint;
end;

procedure TGIS_leg_form.QuickClick(Sender: TObject);
begin
   Table.ApplyFilter( '');
   VerifyRecordsToUse(Table,CaptionField,'Records to plot',UseField);
   Table.ApplyFilter( UseField + '=' + QuotedStr('Y'));
   DrawGrid1.RowCount := succ(Table.RecordCount);
end;


procedure TGIS_leg_form.RadioGroup2Click(Sender: TObject);
begin
    DrawGrid1.GridLineWidth := RadioGroup2.ItemIndex;
    DrawGrid1.Repaint;
end;


procedure TGIS_leg_form.SizeForm;
var
   i,high,wide : integer;
begin
    Table.First;
    high := 0;
    Wide := 0;
    while not Table.eof do begin
       i := DrawGrid1.Canvas.TextWidth(Table.GetFieldByNameAsString(CaptionField));
       if i > Wide then Wide := i;
       i := DrawGrid1.Canvas.TextHeight(Table.GetFieldByNameAsString(CaptionField));
       if i > High then High := i;
       Table.Next;
    end;
    //DrawGrid1.DefaultColWidth := wide + 2 * TextHoriz;
    DrawGrid1.DefaultRowHeight := High + 2 * TextVert;
    DrawGrid1.ColWidths[0] := DrawGrid1.Canvas.TextWidth('Symbol') + 2 * TextHoriz;
    DrawGrid1.ColWidths[1] := wide + 2 * TextHoriz;
    DrawGrid1.Width := DrawGrid1.ColWidths[0] + DrawGrid1.ColWidths[1] + 5;
    DrawGrid1.Height := DrawGrid1.RowCount * DrawGrid1.DefaultRowHeight + 10;
    with StringGrid1 do begin
       Cells[1,0] := 'Pixels';
       Cells[0,1] := 'Text horiz';
       Cells[1,1] := IntToStr(TextHoriz);
       Cells[0,2] := 'Text vert';
       Cells[1,2] := IntToStr(TextVert);
       Cells[0,3] := 'Sym margin';
       Cells[1,3] := IntToStr(BoxMargin);
    end;
end;

procedure TGIS_leg_form.SpeedButton1Click(Sender: TObject);
begin
   FontDialog1.Font := DrawGrid1.Font;
   if FontDialog1.Execute then begin
      DrawGrid1.Font := FontDialog1.Font;
      DrawGrid1.Canvas.Font := FontDialog1.Font;
      SizeForm;
      DrawGrid1.Repaint;
   end;
end;


procedure TGIS_leg_form.StringGrid1Click(Sender: TObject);
var
   Col,Row : integer;
begin
   StringGrid1.MouseToCell(Lastx2,Lasty2,Col,Row);
   case Row of
      1 : ReadDefault('Text horizontal space',TextHoriz);
      2 : ReadDefault('Text vertical space',TextVert);
      3 : ReadDefault('Symbol margin',BoxMargin);
   end;
   SizeForm;
   DrawGrid1.Repaint;
end;


procedure TGIS_leg_form.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX2 := x;
   LastY2 := y;
end;



end.

