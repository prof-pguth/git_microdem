unit dem_legend;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
  //{$Define LegendIssues}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  PETMAR,  PETImage,PetImage_form,Petmar_types,  DEMMapf;

type
  TSetUpLegendForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    FontDialog1: TFontDialog;
    CheckBox4: TCheckBox;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    SpeedButton1: TSpeedButton;
    RecollarBitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    GridSpeedButton15: TSpeedButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RecollarBitBtn8Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure GridSpeedButton15Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MapOwner : tMapForm;
    PrinterImage : TImageDispForm;
    TopHeight : integer;
    TickSize,LabelSize : float64;
    TheAngleMeasure : Petmar_types.tAngleMeasure;
    procedure UpdateDefaults;
  end;


implementation

{$R *.dfm}

uses
   Nevadia_Main,
   DEMDefs,DEMGrPik,
   DEMPrintPreview;


procedure TSetUpLegendForm.UpdateDefaults;


   procedure UpdateRecord(fName : Shortstring; Value : boolean);
   var
      ch : AnsiChar;
   begin
      if (PrinterImage.Overlays <> Nil) then with PrinterImage.Overlays do begin
         PrinterImage.Overlays.ApplyFilter('FILENAME=' + QuotedStr(fName));
         if (PrinterImage.Overlays.RecordCount > 0) then begin
            Edit;
            if Value then ch := 'Y' else ch := 'N';
            SetFieldByNameAsString('PLOT',ch);
            Post;
         end;
      end;
   end;

begin
   if (PrinterImage <> Nil) then with MDDef.PrinterLegend do begin
      ShowTitle := CheckBox4.Checked;
      ShowScaleBar :=  CheckBox3.Checked;
      ShowDeclinationDiagram := CheckBox2.Checked;
      ShowColorLegend := CheckBox1.Checked;
      Edit1.Enabled := ShowTitle;
      Edit2.Enabled := ShowTitle;

      UpdateRecord('maptitle.bmp',ShowTitle);
      UpdateRecord('scalebar.bmp',ShowScaleBar);
      UpdateRecord('decdiag.bmp',ShowDeclinationDiagram);
      UpdateRecord('elevleg.bmp',ShowColorLegend);
      BitBtn1Click(nil);
   end;
end;


procedure TSetUpLegendForm.BitBtn1Click(Sender: TObject);
var
   Bitmap      : tMyBitmap;
   fName : PathStr;
   w1,w2,x,y : integer;
begin
   Bitmap := tMyBitmap.Create;
   LoadMyFontIntoWindowsFont(MDDef.TitleLabelFont,Bitmap.Canvas.Font);
   w1 := Bitmap.Canvas.TextWidth(Edit1.Text);
   w2 := Bitmap.Canvas.TextWidth(Edit2.Text);
   if (w2 > w1) then w1 := w2;
   Bitmap.Width := w1 + 10;
   w1 := Bitmap.Canvas.TextHeight(Edit1.Text);
   w2 := Bitmap.Canvas.TextHeight(Edit2.Text);
   Bitmap.Height := w1 + w2 + 10;
   Bitmap.Canvas.TextOut(0,0,Edit1.Text);
   Bitmap.Canvas.TextOut(0,w1+10,Edit2.Text);
   Bitmap.SaveToFile(MDTempDir + 'maptitle.bmp');
   PrinterImage.Overlays.ApplyFilter('FILENAME=' + QuotedStr('maptitle.bmp'));
   if (PrinterImage.Overlays.RecordCount > 0) then begin
      x := PrinterImage.Overlays.GetFieldByNameAsInteger('X');
      y := PrinterImage.Overlays.GetFieldByNameAsInteger('Y');
   end
   else begin
      x := 0;
      y := 0;
   end;

   PrinterImage.AddImageOverlay(x,y,Bitmap.Width,Bitmap.HEIGHT,'maptitle.bmp',MDDef.PrinterLegend.ShowTitle);
   Bitmap.Free;

   CheckEditString(Edit3.Text,x);
   CheckEditString(Edit4.Text,y);
   CreateBitmap(Bitmap,x,y);

   fName := MDTempDir + 'blank.bmp';
   Bitmap.SaveToFile(fName);
   PrinterImage.SizeCorrect := false;
   PrinterImage.LoadImage(fName);
   BitMap.Free;
   PrinterImage.DrawOverlays;
   PrinterImage.Caption := 'Map Legend Placement';
   PrinterImage.FormResize(nil);
end;


procedure TSetUpLegendForm.BitBtn2Click(Sender: TObject);
begin
   PrinterImage.Image1.Canvas.Pen.Width := 1;
   PrinterImage.Image1.Canvas.Pen.Color := clBlack;
   PrinterImage.Image1.Canvas.Brush.Style := bsClear;
   PrinterImage.Image1.Canvas.Rectangle(0,0,pred(PrinterImage.Image1.Width),pred(PrinterImage.Image1.Height));
   MapPrintPreview(MapOwner.MapDraw,PrinterImage.Image1,MapOwner.MapDraw.ScreenPixelSize);
end;


procedure TSetUpLegendForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   PrinterImage.CanCloseItself := true;
   PrinterImage.Close;
end;


procedure TSetUpLegendForm.BitBtn3Click(Sender: TObject);
begin
   EditMyFont(MDDef.TitleLabelFont);
end;


procedure TSetUpLegendForm.FormCreate(Sender: TObject);
begin
   TheAngleMeasure := amDegree;
   TickSize := -99;
   CheckBox8.Checked := MDDef.HorizGratText;
   CheckBox7.Checked := MDDef.JustSimpleGrid;
   CheckBox6.Checked := MDDef.FullUTMGridLabels;
   CheckBox4.Checked := MDDef.PrinterLegend.ShowTitle;
   CheckBox3.Checked := MDDef.PrinterLegend.ShowScaleBar;
   CheckBox2.Checked := MDDef.PrinterLegend.ShowDeclinationDiagram;
   CheckBox1.Checked := MDDef.PrinterLegend.ShowColorLegend;
   PlaceFormAtMousePosition(Self);
end;


procedure TSetUpLegendForm.GridSpeedButton15Click(Sender: TObject);
begin
   ChangeGridOptions(MapOwner);
end;

procedure TSetUpLegendForm.CheckBox1Click(Sender: TObject);
begin
   UpdateDefaults;
end;


procedure TSetUpLegendForm.CheckBox4Click(Sender: TObject);
begin
   UpdateDefaults;
end;


procedure TSetUpLegendForm.CheckBox3Click(Sender: TObject);
begin
   UpdateDefaults;
end;


procedure TSetUpLegendForm.CheckBox2Click(Sender: TObject);
begin
   UpdateDefaults;
end;


procedure TSetUpLegendForm.BitBtn4Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\map_legend_creation.htm');
end;


procedure TSetUpLegendForm.CheckBox5Click(Sender: TObject);
begin
   UpdateDefaults;
end;


procedure TSetUpLegendForm.CheckBox6Click(Sender: TObject);
begin
   MDDef.FullUTMGridLabels := CheckBox6.Checked;
end;

procedure TSetUpLegendForm.CheckBox7Click(Sender: TObject);
begin
   MDDef.JustSimpleGrid := CheckBox7.Checked;
end;

procedure TSetUpLegendForm.CheckBox8Click(Sender: TObject);
begin
   MDDef.HorizGratText := CheckBox8.Checked;
end;

procedure TSetUpLegendForm.BitBtn5Click(Sender: TObject);
begin
   Close;
end;


procedure TSetUpLegendForm.BitBtn6Click(Sender: TObject);
label
   FoundHeight,FoundWidth;
var
   Bitmap : tMyBitmap;
   x,y,Width,Height : integer;
begin
   ShowHourglassCursor;
   CopyImageToBitmap(PrinterImage.Image1,Bitmap);
   Width := pred(Bitmap.Width);
   Height := pred(Bitmap.Height);
   while (Height > 0) do begin
      for x := 0 to pred(Width) do begin
         if (Bitmap.Canvas.Pixels[x,Height] <> clWhite) then goto FoundHeight;
      end;
      dec(Height);
   end;
   FoundHeight:;
   Edit3.Text := IntToStr((Height+5));

   while (Width > 0) do begin
      for y := 0 to pred(Height) do begin
         if Bitmap.Canvas.Pixels[Width,y] <> clWhite then goto FoundWidth;
      end;
      dec(Width);
   end;
   FoundWidth:;
   Edit4.Text := IntToStr(Width+5);
   BitBtn1Click(Sender);
   Bitmap.Free;
   ShowDefaultCursor;
end;


procedure TSetUpLegendForm.BitBtn7Click(Sender: TObject);
begin
   BitBtn6Click(Sender);
   PETImage.SaveImageAsBMP(PrinterImage.Image1);
end;

procedure TSetUpLegendForm.SpeedButton1Click(Sender: TObject);
begin
   BitBtn6Click(Sender);
   AssignImageToClipBoard(PrinterImage.Image1);
end;

procedure TSetUpLegendForm.BitBtn9Click(Sender: TObject);
begin
   {$IfDef LegendIssues} WriteLineToDebugFile('TSetUpLegendForm.BitBtn9Click in, font=' + MyFontToString(MDDef.CollarUnitsFont));  {$EndIf}
   EditMyFont(MDDef.CollarUnitsFont);
   RecollarBitBtn8Click(Sender);
   {$IfDef LegendIssues} WriteLineToDebugFile('TSetUpLegendForm.BitBtn9Click out, font=' + MyFontToString(MDDef.CollarUnitsFont)); {$EndIf}
end;


procedure TSetUpLegendForm.RecollarBitBtn8Click(Sender: TObject);
var
   Bitmap2 : tMyBitmap;
begin
   MapOwner.DrawCollaredMap(TickSize,LabelSize,Bitmap2);
   Bitmap2.Free;
   BitBtn1Click(Nil);
end;


procedure TSetUpLegendForm.BitBtn10Click(Sender: TObject);
begin
   GetAngle('Ticks',TickSize,TheAngleMeasure);
   BitBtn10.Caption := 'Ticks: ' + AngleFormat(TickSize,TheAngleMeasure);
   RecollarBitBtn8Click(Sender);
end;

procedure TSetUpLegendForm.BitBtn11Click(Sender: TObject);
begin
   GetAngle('Labels',LabelSize,TheAngleMeasure);
   BitBtn11.Caption := 'Labels: ' + AngleFormat(LabelSize,TheAngleMeasure);
   RecollarBitBtn8Click(Sender);
end;


initialization
finalization
end.
