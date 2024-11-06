unit raster_2_vector;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DEMMapf, Vcl.Buttons, Vcl.ExtCtrls;

type
  Trast_2_vect_f = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    Edit2: TEdit;
    RadioGroup1: TRadioGroup;
    Label3: TLabel;
    Edit3: TEdit;
    CheckBox2: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
      procedure MapRasterToVector(MapOwner : tMapForm);
  public
    { Public declarations }
     MapOwner : tMapForm;
  end;


procedure RasterToVector(inMapOwner : tMapForm);


implementation

uses
   Petmar,Petmar_types,PetImage,Petmath,
   DEMESRIShapefile,
   MD_use_tools,
   gdal_tools,
   DEMDefs,DEMdef_routines,DEMCoord;

{$R *.dfm}


procedure RasterToVector(inMapOwner : tMapForm);
//uses GDAL and potrace, https://potrace.sourceforge.net/
//not recently tested
var
   rast_2_vect_f : Trast_2_vect_f;
begin
   rast_2_vect_f := Trast_2_vect_f.Create(Application);
   rast_2_vect_f.MapOwner := inMapOwner;
   inMapOwner.ResizeByPercentage(100);
end;


procedure Trast_2_vect_f.MapRasterToVector(MapOwner : tMapForm);
var
   cmd : ANSIstring;
   BatchFile : tStringList;
   TStr : shortString;
   Decs,i,extra : integer;
   bmp,bmp2 : tMyBitmap;
   fName,aDir,fName2,ToName : PathStr;
   cx,cy : float64;
begin
    bmp := Petimage.LoadBitmapFromFile(MapOwner.MapDraw.BaseMapFName);

    if CheckBox2.Checked then MakeTheBitmapGrayScale(bmp);
    ThreshholdGrayscale(bmp,round(255*MDDef.potrace_black),255);
    if MDDef.potrace_invert then Petimage.GrayscaleNegative(bmp);

    MapOwner.Image1.Picture.Graphic := bmp;
    fName := Petmar.NextFileNumber(MDTempDir, 'potrace_', '.bmp');

    if MDDef.potrace_outline in [1,2] then begin
      CreateBitmap(bmp2,bmp.Width + 4,bmp.Height+4);
      if MDDef.potrace_outline in [1] then bmp2.Canvas.Pen.Color := clWhite;
      bmp2.Canvas.Pen.Width := 2;
      bmp2.Canvas.Brush.Style := bsSolid;
      bmp2.Canvas.Rectangle(0,0,pred(bmp2.Width),pred(bmp2.Height));
      bmp2.Canvas.Draw(2,2,bmp);
      bmp.Free;
      PetImage.SaveBitmap(BMP2,fName);
      bmp2.Free;
      extra := 2;
    end
    else begin
       PetImage.SaveBitmap(BMP,fName);
       BMP.Free;
       extra := 0;
    end;

    if (DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMUsed = ArcSecDEM) then begin
       cx := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapCorners.BoundBoxGeo.xmin - Extra * DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMySpacing;
       cy := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapCorners.BoundBoxGeo.ymin - Extra * DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMySpacing;
       Decs := -7;
    end
    else begin
       cx := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapCorners.BoundBoxUTM.xmin- Extra * DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMySpacing;
       cy := DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapCorners.BoundBoxUTM.ymin - Extra * DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMySpacing;
       Decs := -1;
    end;

    cmd := ProgramRootDir + 'potrace\potrace ' + fName +
       ' -b geojson' +
       ' -L ' + RealToString(cx,-12,Decs) +
       ' -B ' + RealToString(cy,-12,Decs) +
       ' -x ' + RealToString(DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMySpacing,-12,Decs) +
       ' -t ' + IntToStr(MDDef.potrace_tsize) +
       ' -a ' + RealToString(MDDef.potrace_corner,-8,2);

    WinExecAndWait32(cmd);

    repeat
       inc(i);
       aDir := MDTempDir + 'shp' + IntToStr(i);
    until Not ValidPath(aDir);

    fName2 := ExtractShortPathName(GDAL_ogr_Name);
    if IsGDALFilePresent(fName2) then begin
      StartGDALbatchFile(BatchFile);
      if DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.DEMUsed = ArcSecDEM then       begin
         TStr := 'EPSG:4326'
      end
      else begin
         TStr := 'EPSG:269' + AddDayMonthLeadingZero(MapOwner.MapDraw.PrimMapProj.projUTMZone);
      end;

      BatchFile.Add(fName2 + ' -t_srs EPSG:4326 -s_srs ' + TStr + ' ' + aDir + ' ' + ChangeFileExt(fName,'.json'));

      EndBatchFile(MDTempDir + 'r2v.bat',batchfile);

      fName := aDir + '\OGRGeoJSON.shp';
      ToName := adir + 'potrace_' + LastSubDir(Adir) + '.shp';
      RenameShapeFile(fName,ToName);
      MapOwner.LoadDataBaseFile(ToName);
    end;
end;


procedure Trast_2_vect_f.BitBtn1Click(Sender: TObject);
begin
   MDDef.potrace_outline := RadioGroup1.ItemIndex;
   MapRasterToVector(MapOwner);
end;

procedure Trast_2_vect_f.CheckBox1Click(Sender: TObject);
begin
   MDDef.potrace_invert := CheckBox1.Checked;
end;


procedure Trast_2_vect_f.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.potrace_tsize);
end;


procedure Trast_2_vect_f.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.potrace_black);
end;


procedure Trast_2_vect_f.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.potrace_corner);
end;

procedure Trast_2_vect_f.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure Trast_2_vect_f.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.potrace_invert;
   Edit1.Text := IntToStr(MDDef.potrace_tsize);
   Edit2.Text := realToString(MDDef.potrace_black,-8,2);
   Edit3.Text := realToString(MDDef.potrace_corner,-8,2);
   RadioGroup1.ItemIndex := MDDef.potrace_outline;
end;


initialization
finalization
end.
