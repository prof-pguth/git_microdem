unit curv_tend_map;

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
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,


  DEMMapf,Petmar_types;

type
  TCurvMapForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    OKBtn: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    TrackBar1: TTrackBar;
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
     TheMap : tMapForm;
     fName,backName,frontName : PathStr;
  end;


procedure DrawCurveCatMap(BaseMap : tMapForm);


implementation

{$R *.dfm}

uses
   PetImage,Petmar,DEMDefs,DEMCoord,DEMMapDraw;

procedure DrawCurveCatMap(BaseMap : tMapForm);
var
  CurvMapForm: TCurvMapForm;
begin
   CurvMapForm := TCurvMapForm.Create(Application);
   CurvMapForm.TheMap := BaseMap;
   CurvMapForm.Edit1.Text := RealToString(MDDef.CurveFlatVal,-12,-5);
   CurvMapForm.fName:= MDTempDir + 'conv-map.bmp';
   CurvMapForm.backName:= MDTempDir + 'back_leg.bmp';
   CurvMapForm.frontName:= MDTempDir + 'fr_leg.bmp';
   CurvMapForm.ShowModal;
end;


procedure TCurvMapForm.BitBtn1Click(Sender: TObject);
var
  Bitmap1,Bitmap2,bmp : tMyBitmap;


      function DrawCurveCatMap(Bitmap : tMyBitmap) : boolean;
      const
         ClassChar : array[-1..1] of char = ('X','G','V');
      var
         PlanIndex,ProfIndex,i,j,sum,xg,yg : integer;
         MaxSlp,crossc,MaxCurve,MinCurve,PlanC,ProfC : float64;
         LegBMP : tMyBitmap;
         Colors : array[-1..1,-1..1] of tColor;
         Count  : array[-1..1,-1..1] of integer;

               function GetIndex(C : float64) : integer;
               begin
                   if (C < -MDDef.CurveFlatVal) then Result := -1
                   else if (C < MDDef.CurveFlatVal) then Result := 0
                   else  Result := 1;
               end;

      begin
         with TheMap,MapDraw do begin
            Colors[-1,-1] := clRed;
            Colors[-1,0] := clMaroon;
            Colors[-1,1] := clLime;
            Colors[0,-1] := clGreen;
            Colors[0,0] := clBlue;
            Colors[0,1] := clNavy;
            Colors[1,-1] := clOlive;
            Colors[1,0] := clPurple;
            Colors[1,1] := clYellow;
            for i := -1 to 1 do for j := -1 to 1 do Count[i,j] := 0;
            with Bitmap.Canvas do begin
               StartProgress('Curvature');
               for j := 0 to MapYSize do begin
                  if j mod 25 = 0 then UpDateProgressBar(j/MapYSize);
                  for i := 0 to pred(MapXSize) do begin
                     ScreenToDEMGrid(i,j,XG,yg);
                     //if DEMGlb[DEMonMap].GetCurvature(xg,yg,PlanC,ProfC) then begin
                     if DEMGlb[DEMonMap].GetEvansParams(xg,yg,MDDef.WoodRegionSize,MaxSlp,ProfC,PlanC,crossc,MaxCurve,MinCurve) then begin
                        ProfIndex := GetIndex(ProfC);
                        PlanIndex := GetIndex(PlanC);
                        Pixels[i,j] := Colors[ProfIndex,PlanIndex];
                        inc(Count[ProfIndex,PlanIndex]);
                     end
                     else Pixels[i,j] := ConvertPlatformColorToTColor(DEMGlb[DEMonMap].MissingColorRGBTriple(xg,yg));
                  end {while};
                  if WantOut then exit;
               end {for i};
            end;
            EndProgress;
            Sum := 0;
            for i := -1 to 1 do for j := -1 to 1 do Sum := sum + Count[i,j];

            CutOutCenterOfMap(LegBMP,500,120);
            LegBMP.Height := 200;
            LegBMP.Canvas.Font.Color := ClBlack;
            LegBMP.Canvas.Font.Name := 'Verdana';
            LegBMP.Canvas.Font.Style := [fsbold];
            LegBMP.Canvas.Font.Size := 12;
            LegBMP.Canvas.TextOut(20, 130, 'Straight (G) abs(curvature) < ' + RealToString(MDDef.CurveFlatVal,-12,-5));
            LegBMP.Canvas.TextOut(20, 150, 'X = convex, G = straight, V = concave');
            LegBMP.Canvas.TextOut(20, 170, 'First letter profile curvature, second plan curvature');
            LegBMP.SaveToFile(backName);
            LegBMP.Free;
            CreateBitmap(LegBMP,500,200);
            LegBMP.Canvas.Font.Name := 'Verdana';
            LegBMP.Canvas.Font.Size := 14;
            LegBMP.Canvas.Font.Style := [fsbold];
            LegBMP.Canvas.Brush.Style := bsSolid;
            LegBMP.Canvas.Brush.Color := clWhite;
            for i := -1 to 1 do begin
               for j := -1 to 1 do begin
                  LegBMP.Canvas.Font.Color := Colors[i,j];
                  LegBMP.Canvas.TextOut(180 + i * 150, 50 + j * 40,ClassChar[i] + '/' +  ClassChar[j] + RealToString(100*Count[i,j]/Sum,6,2) +'%');
               end;
            end;
            LegBMP.SaveToFile(frontName);
         end;
         FreeAndNil(LegBMP);
      end;


begin
   //with TheMap do begin
      TheMap.DoFastMapRedraw;
      CloneImageToBitmap(TheMap.Image1,Bmp);
      if FileExists(fName) then begin
         BMP.LoadFromFile(fName);
      end
      else begin
         DrawCurveCatMap(Bmp);
         BMP.SaveToFile(fName);
      end;
      CreateBitmap(Bitmap1,1,1);
      Bitmap1.LoadFromFile(backName);
      CreateBitmap(Bitmap2,1,1);
      Bitmap2.LoadFromFile(frontName);
      if RadioGroup1.ItemIndex = 1 then begin
         IHSMergePurgeBitmaps(Bitmap1,Bitmap2);
      end
      else begin
         DrawAndDeleteOverlay(Bitmap1,Bitmap2,TrackBar1.Position);
      end;
      Self.Image1.Picture.Graphic := Bitmap1;
      FreeAndNil(Bitmap1);
      TheMap.IHSmergeOntoMap(bmp,RadioGroup1.ItemIndex = 1,TrackBar1.Position);
   //end;
end;


procedure TCurvMapForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.CurveFlatVal);
   SysUtils.DeleteFile(fName);
   SysUtils.DeleteFile(backName);
   SysUtils.DeleteFile(frontName);
end;

procedure TCurvMapForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Edit1Change(Sender);
   Action := caFree;
end;


initialization
finalization
end.
