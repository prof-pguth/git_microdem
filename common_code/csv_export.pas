unit csv_export;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls,

  Petmar_types,Petmar_db,Petmar,DEMMapf;

type
  TCVSExportForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    Edit7: TEdit;
    ProgressBar1: TProgressBar;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fName : PathStr;
    BaseMap : tMapForm
  end;


procedure ExportCSVLatLongGrid(inMap : tMapForm);


implementation

{$R *.dfm}

uses
   DEMDefs,BaseMap,DEMmagvar;


procedure ExportCSVLatLongGrid;
var
  CVSExportForm: TCVSExportForm;
begin
   CVSExportForm := TCVSExportForm.Create(Application);
   CVSExportForm.BaseMap := inMap;
   CVSExportForm.Edit1.Text := IntToStr(succ(trunc(InMap.MapDraw.MapCorners.BoundBoxGeo.ymax)));
   CVSExportForm.Edit2.Text := IntToStr((trunc(InMap.MapDraw.MapCorners.BoundBoxGeo.ymin)));
   CVSExportForm.Edit4.Text := IntToStr(succ(trunc(InMap.MapDraw.MapCorners.BoundBoxGeo.xmax)));
   CVSExportForm.Edit3.Text := IntToStr((trunc(InMap.MapDraw.MapCorners.BoundBoxGeo.xmin)));
   if (CVSExportForm.BaseMap.MapDraw.DEMonMap = 0) then begin
      CVSExportForm.CheckBox2.Checked := true;
      CVSExportForm.CheckBox2.Enabled := false;
   end;
   CVSExportForm.Show;
end;


procedure TCVSExportForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   fName := Petmar.NextFileNumber(MDTempDir, 'grid_','.csv');
   Label3.Caption := fName;
end;


procedure TCVSExportForm.BitBtn1Click(Sender: TObject);
var
   Results : tStringList;
   n : integer;
   Line : ANSIstring;
   Prime : boolean;
   TotalShiftUTM,TotalShiftGeo,
   Maph,Mapk,
   Lat,Long,x1,x2,y1,y2,dx,dy,z,DEC,DIP,TI,GV : float64;
begin
   Results := tStringList.Create;
   CheckEditString(Edit3.Text,x1);
   CheckEditString(Edit4.Text,x2);
   CheckEditString(Edit1.Text,y2);
   CheckEditString(Edit2.Text,y1);
   CheckEditString(Edit5.Text,dy);
   CheckEditString(Edit6.Text,dx);
   CheckEditString(Edit7.Text,z);

   if CheckBox3.Checked or CheckBox4.Checked then begin
      GetSecondaryDatum;
      BaseMap.MapDraw.SecondaryMapProj := tMapProjection.Create('TCVSExportForm.BitBtn1Click');
      BaseMap.MapDraw.SecondaryMapProj.DefineDatumFromUTMZone(MDdef.PreferSecondaryDatum,BaseMap.MapDraw.SecondaryMapProj.projUTMZone,MDDef.DefaultLatHemi,'TCVSExportForm.BitBtn1Click');
   end;

   if CheckBox6.Checked then begin
      Line :='';
      if CheckBox1.Checked then Line := RecNoFName + ',';
      Line := Line + 'LAT,LONG';
      if CheckBox2.Checked then Line := Line + ',ELEV';
      if CheckBox3.Checked then Line := Line + ',DIFF_GEO';
      if CheckBox4.Checked then Line := Line + ',DIFF_UTM';
      if CheckBox5.Checked then Line := Line + ',H_DISTORT,K_DISTORT';
      if CheckBox7.Checked then Line := Line + ',MAG_DEC,MAG_INTENS';
      Results.Add(Line);
   end;

   n := 0;
   Long := x1;
   while Long <= x2 do begin
      ProgressBar1.Position := round(100 * (Long-x1) / (x2-x1) );
      Lat := y1;
      while Lat <= y2 do begin
         inc(n);
         Line :='';
         if CheckBox1.Checked then Line := Line + IntToStr(n) + ',';
         Line := Line + RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8);
         if CheckBox2.Checked then Line := Line + ',' + RealToString(z,-18,-8);

         if (CheckBox3.Checked or CheckBox4.Checked) then {with BaseMap.MapDraw do} begin
            BaseMap.MapDraw.ComputeDatumShifts(BaseMap.Image1.Canvas,Lat,Long,TotalShiftUTM,TotalShiftGeo);
            if CheckBox3.Checked then Line := Line + ',' + RealToString(TotalShiftGeo,-18,-2);
            if CheckBox4.Checked then Line := Line + ',' + RealToString(TotalShiftUTM,-18,-2);
         end;
         if CheckBox5.Checked then begin
            BaseMap.MapDraw.PrimMapProj.GetMapScaleFactor(Lat,Long,Maph,Mapk,Prime);
            Line := Line + ',' + RealToString(MapH,-18,-5) + ',' + RealToString(MapK,-18,-5);
         end;
         if CheckBox7.Checked then begin
             MagVr1(0,Lat,Long,CurMagYear, DEC,DIP,TI,GV);
             Line := Line + ',' + RealToString(dec,-12,-2) + ',' + RealToString(TI,-12,-2);
         end;
         Results.Add(Line);
         Lat := Lat + dy;
      end {while};
      Long := Long + dx;
   end {while};
   Results.SaveToFile(fName);
   Results.Free;
   if CheckBox3.Checked or CheckBox4.Checked then begin
      BaseMap.MapDraw.SecondaryMapProj.Destroy;
      BaseMap.MapDraw.SecondaryMapProj := Nil;
   end;

   if (BaseMap <> Nil) then BaseMap.LoadDataBaseFile(fName);
end;


procedure TCVSExportForm.BitBtn4Click(Sender: TObject);
begin
   Petmar.GetFileNameDefaultExt('CSV file','.csv',fName);
   Label3.Caption := fName;
end;


procedure TCVSExportForm.CheckBox2Click(Sender: TObject);
begin
   Label4.Enabled := CheckBox2.Checked;
   Edit7.Enabled := CheckBox2.Checked;
end;

procedure TCVSExportForm.BitBtn2Click(Sender: TObject);
begin
   Close;
end;


procedure TCVSExportForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TCVSExportForm.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\csvexport.htm');
end;


end.
