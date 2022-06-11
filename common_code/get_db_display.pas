unit get_db_display;

no longer used

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
    //{$Define RecordDBColorOptions}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  DEMDataBase;

type
  TGetDBColorForm = class(TForm)
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    BitBtn1: TBitBtn;
    Opacity: TLabel;
    Edit1: TEdit;
    CheckBox4: TCheckBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RadioGroup20Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TheDB : TGISdataBaseModule;
  end;



procedure GetDBColorOptions(GISdb : TGISdataBaseModule);


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,DEMDefs,DEMESRIShapeFile, PETImage,
   {$IfDef ExMilIcons}
   {$Else}
   dem_milicon,
   {$EndIf}
   PetDBUtils;


procedure GetDBColorOptions(GISdb : TGISdataBaseModule);
var
   GetDBColorForm: TGetDBColorForm;
   i : integer;
begin
   GetDBColorForm := TGetDBColorForm.Create(Application);
   GetDBColorForm.TheDB := GISDB;
   with GetDBColorForm do begin
      Edit1.Text := IntToStr(TheDB.dbOpts.Opacity);
      if TheDB.SimplePointFile then BitBtn1.Caption := 'Point'
      else if LineShapeFile(TheDB.ShapeFileType) then BitBtn1.Caption := 'Line'
      else if (AreaShapeFile(TheDB.ShapeFileType) or ((Not TheDB.ItsAShapeFile) and (TheDB.LatLongCornersPresent))) then BitBtn1.Caption := 'Area';
   end;
   GISDB.ColorButtonForSymbol(GetDBColorForm.BitBtn1);
   GetDBColorForm.ShowModal;
end;


procedure TGetDBColorForm.BitBtn1Click(Sender: TObject);
var
   fName : PathStr;
begin
   with TheDB do begin
      if LineShapeFile(ShapeFileType) then begin
         PickLineSizeAndColor(dbName,BitBtn1,dbOpts.LineColor,dbOpts.LineSize);
         {$IfDef RecordDBColorOptions}
         WriteLineToDebugFile('TGetDBColorForm.BitBtn1Click for line');
         WriteLineToDebugFile('   line size=' + IntToStr(LineSize) + '   color=' + IntToStr(LineColor));
         {$EndIf}
      end
      else if (dbOpts.dbAutoShow = dbasIconAll) then begin
          if (TheDB.dbOpts.AllIconFName = '') then FName := MainMapData + 'Icons\';
          if not PetImage.GetGraphicsFileName(FName) then begin
             TheDB.dbOpts.AllIconFName := '';
          end
          else TheDB.dbOpts.AllIconFName := ExtractFileName(fName);
          ColorButtonForSymbol(BitBtn1,'Icon');
      end
      else if (ItsAPointDB or XYZfile) then begin
           PickSymbol(BitBtn1,dbOpts.Symbol);
           BitBtn1.Caption := 'Point';
           {$IfDef RecordDBColorOptions}
           WriteLineToDebugFile('TGetDBColorForm.BitBtn1Click for point');
           WriteLineToDebugFile('   Symbol color=' + IntToStr(SymbolColor) + '  size=' + IntToStr(SymbolSize) + ' sym=' + IntToStr(Ord(Symbol)));
           {$EndIf}
      end
      else if (AreaShapeFile(ShapeFileType) or ((Not ItsAShapeFile) and (LatLongCornersPresent))) then begin
         PickPattern(dbName,dbOpts.AreaSymbolFill,dbOpts.FillColor,dbOpts.LineColor,dbOpts.LineSize);
         ColorButtonForSymbol(BitBtn1);
         {$IfDef RecordDBColorOptions}
         WriteLineToDebugFile('TGetDBColorForm.BitBtn1Click for area');
         {$EndIf}
      end;
   end;
end;

procedure TGetDBColorForm.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;

procedure TGetDBColorForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\db_sym_select.htm');
end;

procedure TGetDBColorForm.OKBtnClick(Sender: TObject);
begin
   CheckEditString(Edit1.Text,TheDB.dbOpts.Opacity);
   TheDB.dbOpts.HideTable := CheckBox4.Checked;
   Close;
end;

procedure TGetDBColorForm.RadioGroup20Click(Sender: TObject);
begin
   //TheDB.dbOpts.DBAutoShow := dbasDefault;
   //theDB.ColorButtonForSymbol(BitBtn1);
end;


initialization
finalization
   {$IfDef RecordDBColorOptions}
   WriteLineToDebugFile('RecordDBColorOptions active in get_db_display');
   {$EndIf}

   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing get_db_display');
   {$EndIf}
end.
