unit get_db_coloring;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define TrackColoring}
   //{$Define TrackAllColoring}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Petmar_types,DEMDefs, DEMMapf,
  DEMDataBase;

type
  TDBColorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Image1: TImage;
    ComboBox1: TComboBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    BitBtn3: TBitBtn;
    CheckBox2: TCheckBox;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
     procedure DrawLegend(Redraw : boolean = true);
  public
    { Public declarations }
    MapOwner : tMapForm;
    BaseDB : integer;
    MaybeMap : boolean;
  end;


procedure GetColorValuesForDataBase(theDB : integer; vth : ShortString; AllowMinMax : boolean = true);


implementation

{$R *.dfm}

uses
   Petmar, PETImage;


procedure GetColorValuesForDataBase(theDB : integer; vth : ShortString; AllowMinMax : boolean = true);
var
  DBColorForm : TDBColorForm;
begin
  {$IfDef TrackColoring} WriteLineToDebugFile('GetColorValuesForDataBase in, db=' +  IntToStr(theDB)); {$EndIf}
  DBColorForm := TDBColorForm.Create(Application);
  DBColorForm.MaybeMap := false;
  DBColorForm.BaseDB := theDB;
  DBColorForm.Caption := vth;
  if AllowMinMax then begin
     DBColorForm.Edit1.Text := RealToString(GISdb[theDB].dbOpts.ColorMin,-18,-2);
     DBColorForm.Edit2.Text := RealToString(GISdb[theDB].dbOpts.ColorMax,-18,-2);
  end
  else begin
     DBColorForm.Edit1.Visible := false;
     DBColorForm.Edit2.Visible := false;
     DBColorForm.Label1.Visible := false;
     DBColorForm.Label2.Visible := false;
     DBColorForm.Edit1.Text := '0';
     DBColorForm.Edit2.Text := '100';
  end;
  FillComboBoxWithColorPalettes(ColorBrewerName,DBColorForm.ComboBox1);
  if (GISdb[theDB].dbOpts.DBColorPaletteName = '') then begin
     DBColorForm.ComboBox1.Text := DBColorForm.ComboBox1.Items[0];
     DBColorForm.ComboBox1.ItemIndex := 0;
  end
  else DBColorForm.ComboBox1.Text := GISdb[theDB].dbOpts.DBColorPaletteName;
  DBColorForm.RadioGroup1.ItemIndex := ord(GISdb[theDB].dbOpts.DBColorScheme);
  DBColorForm.CheckBox2.Checked := GISdb[theDB].dbOpts.ReverseColorTable;
  DBColorForm.MaybeMap := true;
  {$IfDef TrackColoring}   WriteLineToDebugFile('GetColorValuesForDataBase start; color range ' + DBColorForm.Edit1.Text + ' to ' + DBColorForm.Edit2.Text);   {$EndIf}
  DBColorForm.ShowModal;
end;


procedure TDBColorForm.RadioGroup1Click(Sender: TObject);
begin
   GISdb[BaseDB].dbOpts.DBColorScheme := tLegendColors(RadioGroup1.ItemIndex);
   ComboBox1.Enabled := GISdb[BaseDB].dbOpts.DBColorScheme = LegChloropleth;
   BitBtn1.Enabled := GISdb[BaseDB].dbOpts.DBColorScheme = LegChloropleth;
   BitBtn2.Enabled := GISdb[BaseDB].dbOpts.DBColorScheme = LegChloropleth;
   if (GISdb[BaseDB].dbOpts.DBColorScheme = LegChloropleth) and (GISdb[BaseDB].dbOpts.DBColorPaletteName = '') then GISdb[BaseDB].dbOpts.DBColorPaletteName := ComboBox1.Text;
   DrawLegend;
end;


procedure TDBColorForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   if MaybeMap and ValidDB(BaseDB) then begin
      GISDB[BaseDB].RedrawLayerOnMap;
   end;
end;

procedure TDBColorForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TDBColorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TDBColorForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pick_db_coding_colors.htm');
end;

procedure TDBColorForm.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.QuickMapRedraw;
   Petmar.PlaceFormAtMousePosition(Self);
   DrawLegend;
end;

procedure TDBColorForm.BitBtn1Click(Sender: TObject);
begin
   if (Sender = BitBtn1) then begin
      if ComboBox1.ItemIndex = 0 then ComboBox1.ItemIndex := pred(ComboBox1.Items.Count)
      else ComboBox1.ItemIndex := ComboBox1.ItemIndex - 1;
   end
   else begin
      if ComboBox1.ItemIndex = pred(ComboBox1.Items.Count) then ComboBox1.ItemIndex := 0
      else ComboBox1.ItemIndex := ComboBox1.ItemIndex + 1;
   end;
   ComboBox1.Text := ComboBox1.Items[ComboBox1.ItemIndex];
   ComboBox1Change(Sender);
end;


procedure TDBColorForm.BitBtn2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TDBColorForm.BitBtn3Click(Sender: TObject);
begin
   {$IfDef TrackColoring} WriteLineToDebugFile('TDBColorForm.BitBtn3Click in; color range ' + Edit1.Text + ' to ' + Edit2.Text); {$EndIf}
   GISdb[BaseDB].FieldRange(GISdb[BaseDB].dbOpts.FloatColorField,GISdb[BaseDB].dbOpts.ColorMin,GISdb[BaseDB].dbOpts.ColorMax,GISdb[BaseDB].MyData.Filtered,true);
   Edit1.Text := RealToString(GISdb[BaseDB].dbOpts.ColorMin,-18,-6);
   Edit2.Text := RealToString(GISdb[BaseDB].dbOpts.ColorMax,-18,-6);
   DrawLegend(true);
   {$IfDef TrackColoring} WriteLineToDebugFile('TDBColorForm.BitBtn3Click in; color range ' + Edit1.Text + ' to ' + Edit2.Text); {$EndIf}
end;

procedure TDBColorForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.QuickMapRedraw := CheckBox1.Checked;
   if MDDef.QuickMapRedraw and ValidDB(BaseDB) then DrawLegend;
end;

procedure TDBColorForm.CheckBox2Click(Sender: TObject);
begin
   GISdb[BaseDB].dbOpts.ReverseColorTable := CheckBox2.Checked;
   DrawLegend;
end;

procedure TDBColorForm.ComboBox1Change(Sender: TObject);
begin
   GISdb[BaseDB].dbOpts.DBColorPaletteName := ComboBox1.Text;
   DrawLegend(true);
end;

procedure TDBColorForm.DrawLegend(Redraw : boolean = true);
var
   Bitmap : tMyBitmap;
begin
   {$IfDef TrackAllColoring} WriteLineToDebugFile('TDBColorForm.DrawLegend in DB=' + IntToStr(BaseDB)); {$EndIf}
   if ValidDB(BaseDB) then begin
      GISdb[BaseDB].DefineDBColorTable;
      Bitmap := GISdb[BaseDB].ChloroplethLegend('');
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      if Redraw and MDDef.QuickMapRedraw then RedrawSpeedButton12Click(nil);
   end;
   {$IfDef TrackAllColoring} WriteLineToDebugFile('TDBColorForm.DrawLegend out'); {$EndIf}
end;


procedure TDBColorForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,GISdb[BaseDB].dbOpts.ColorMin);
   DrawLegend(false);
end;

procedure TDBColorForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,GISdb[BaseDB].dbOpts.ColorMax);
   DrawLegend(false);
end;



initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing get db coloring out'); {$EndIf}
end.
