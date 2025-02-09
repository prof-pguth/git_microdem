unit demprintpreview;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}      //normally only defined for debugging specific problems
   //{$Define RecordPrint}
{$EndIf}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Printers,
  DEMMapf, ExtCtrls, StdCtrls, Buttons,
  PETMAR,Petmar_types,DEMMapDraw;

type
  TPrintPreviewForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    BitBtn1: TBitBtn;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn2: TBitBtn;
    PrintDialog1: TPrintDialog;
    HelpBtn: TBitBtn;
    BitBtn3: TBitBtn;
    PrinterSetupDialog1: TPrinterSetupDialog;
    BitBtn5: TBitBtn;
    RadioGroup1: TRadioGroup;
    BitBtn6: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
      procedure DrawPreview;
  public
    { Public declarations }
     InchesOnMap,YInches,ThePixelSize : float64;
     XPages,YPages : integer;
     DrawImage1 : tImage;
     MapDraw : tMapDraw;
  end;


procedure MapPrintPreview(inMapDraw : tMapDraw; inImage : tImage; inPixelSize : float64);


implementation

{$R *.DFM}


uses
   PetMath,DEMDefs,BaseMap, PETImage;


procedure TPrintPreviewForm.DrawPreview;
var
   Bitmap          : tMyBitmap;
   xs,ys,xp,yp,Diver,
   PrinterXDPI,PrinterYDPI,
   PrinterHeight,PrinterWidth : integer;
   Distance,Bearing,
   Lat1,Lat2,Lat3,Long1,Long2,Long3 : float64;

         procedure DrawLines(Canvas : tCanvas; Width : integer; Color : tColor; Style : tPenStyle);
         var
            x,y : integer;
         begin
            Canvas.Pen.Width := Width;
            Canvas.Pen.Color := Color;
            Canvas.Pen.Style := Style;
            Canvas.Brush.Style := bsClear;
            Canvas.Rectangle(0,0,xs,ys);
            if (XPages > 1) then for x := 1 to xPages do begin
               Canvas.MoveTo(xs * x div XPages,0);
               Canvas.LineTo(xs * x div XPages,ys);
            end;
            if (yPages > 1) then for y := 1 to yPages do begin
               Canvas.MoveTo(0,ys * y div yPages);
               Canvas.LineTo(xs,ys * y div yPages);
            end;
         end;


begin
   {$IfDef RecordPrint} WriteLineToDebugFile('Enter TPrintPreviewForm.DrawPreview in demprintpreview'); {$EndIf}
   ShowHourglassCursor;
   BitBtn1.Caption := 'Scale 1:' + RealToString(MDdef.PrinterScale,-18,0);

   if PetImage.CopyImageToBitmap(DrawImage1,Bitmap) then begin
       MapDraw.ScreenToLatLongDegree(0,0,Lat1,Long1);
       MapDraw.ScreenToLatLongDegree(pred(DrawImage1.Width),0,Lat2,Long2);
       MapDraw.ScreenToLatLongDegree(0,pred(DrawImage1.Height),Lat3,Long3);

       VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Bearing);
       InchesOnMap :=  100 * Distance / MDDef.PrinterScale / 2.54; {figures x size of map in inches}

       VincentyCalculateDistanceBearing(Lat1,Long1,Lat3,Long3,Distance,Bearing);
       YInches := 100 * Distance / MDDef.PrinterScale / 2.54;      {figures y size of map in inches}

       if (InchesOnMap > YInches) then RadioGroup1.ItemIndex := 1 else RadioGroup1.ItemIndex := 0;

       {$IfDef RecordPrint}
          WriteLineToDebugFile('Printer scale, 1:' + IntToStr(Round(PrinterScale)));
          WriteLineToDebugFile(' Map size: ' + RealToString(InchesOnMap,-8,2) + ' by ' + RealToString(YInches,-8,2) + '"');
          WriteLineToDebugFile(' Map size: ' + RealToString(InchesOnMap*2.54,-8,1) + ' by ' + RealToString(YInches*2.54,-8,1) + ' cm');
          WriteLineToDebugFile(' Map pixels: ' + IntToStr(MapDraw.MapXSize) + ' by ' + IntToStr(MapDraw.MapYSize));
          WriteLineToDebugFile(' Image pixels: ' + IntToStr(DrawImage1.Width) + ' by ' + IntToStr(DrawImage1.Height));
       {$EndIf}

       PrinterWidth := GetDeviceCaps(Printer.Handle,HorzRes);
       PrinterXDPI := GetDeviceCaps(Printer.Handle,LogPixelsX);
       PrinterHeight := GetDeviceCaps(Printer.Handle,VertRes);
       PrinterYDPI := GetDeviceCaps(Printer.Handle,LogPixelsY);
       xPages := succ(trunc(InchesOnMap / (PrinterWidth / PrinterXDPI)));
       yPages := succ(trunc(YInches / (PrinterHeight / PrinterYDPI)));

      Label1.Caption := IntToStr(xPages*yPages) + ' pages, ' + IntToStr(xPages) + ' w x ' + IntToStr(yPages) + ' h';
      Label2.Caption := RealToString(InchesOnMap,-8,2) + ' by ' + RealToString(YInches,-8,2) + '"';
      xs := PrinterWidth * xPages;
      ys := PrinterHeight * yPages;
      Diver := 1;
      while (xs div Diver > 400) or (ys div Diver > 400) do inc(Diver);
      xs := xs div Diver;
      ys := ys div Diver;

      xp := round(InchesOnMap / (xPages * (PrinterWidth / PrinterXDPI)) * xs);
      yp := round(YInches / (yPages * (PrinterHeight / PrinterYDPI)) * ys);
      Image2.Stretch := true;
      Image2.Height := yp;
      Image2.Width := xp;
      Bitmap.Canvas.Pen.Width := 9;
      Bitmap.Canvas.Pen.Color := clSilver;
      Bitmap.Canvas.Brush.Style := bsClear;
      Bitmap.Canvas.Rectangle(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
      Image2.Picture.Graphic := Bitmap;
      Bitmap.Free;
      Image1.Stretch := false;
      Image1.Height := ys;
      Image1.Width := xs;

      CreateBitmap(Bitmap,xs,ys);
      DrawLines(Bitmap.Canvas,3,clBlack,psSolid);
      DrawLines(Bitmap.Canvas,1,clRed,psDash);
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      if (Image1.Height > 245) then ClientHeight := Image1.Height + 25
      else ClientHeight := 265;
   end;
   ShowDefaultCursor;
   {$IfDef RecordPrint} WriteLineToDebugFile('Exit TPrintPreviewForm.DrawPreview in demprintpreview'); {$EndIf}
end;


procedure TPrintPreviewForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TPrintPreviewForm.BitBtn1Click(Sender: TObject);
begin
   repeat
      ReadDefault('map scale, 1:',MDdef.PrinterScale);
      DrawPreview;
   until (XPages * YPages < 10) or AnswerIsYes('Confirm print on ' + IntToStr(XPages*YPages) + ' pages');
end;

procedure TPrintPreviewForm.BitBtn2Click(Sender: TObject);
var
   i : integer;
begin
   if (XPages * YPages > 10) and (not AnswerIsYes('Confirm print on ' + IntToStr(XPages*YPages) + ' pages')) then exit;
   if PrintDialog1.Execute then  for i := 1 to PrintDialog1.Copies do
       PrintImageToSpecifiedSize(DrawImage1,InchesOnMap,YInches);
end;


procedure TPrintPreviewForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\print_preview.htm');
end;


procedure TPrintPreviewForm.BitBtn3Click(Sender: TObject);
begin
   if PrinterSetupDialog1.Execute then DrawPreview;
end;


procedure TPrintPreviewForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordPrint} WriteLineToDebugFile('TPrintPreviewForm.FormCreate in'); {$EndIf}
   Petmar.PlaceFormAtMousePosition(Self);
   BitBtn5.Enabled := false;
   {$IfDef RecordPrint} WriteLineToDebugFile('TPrintPreviewForm.FormCreate out'); {$EndIf}
end;


procedure MapPrintPreview(inMapDraw : tMapDraw; inImage : tImage; inPixelSize : float64);
var
  PrintPreviewForm : TPrintPreviewForm;
begin
   {$IfDef RecordPrint} WriteLineToDebugFile('Enter MapPrintPreview in demprintpreview'); {$EndIf}
   PrintPreviewForm := TPrintPreviewForm.Create(Application);
   with PrintPreviewForm do begin
      ThePixelSize := InPixelSize;
      MapDraw := inMapDraw;
      DrawImage1 := inImage;
      DrawPreview;
      ShowModal;
   end;
   PrintPreviewForm.Close;
end;


initialization
finalization
{$IfDef RecordPrintProblem} WriteLineToDebugFile('RecordPrintProblem active in demprintpreview');  {$EndIf}
end.

