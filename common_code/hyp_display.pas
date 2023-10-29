unit hyp_display;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordColorImage}
   //{$Define RecordHyperion}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,ComCtrls, StdCtrls, Menus, Buttons,  StrUtils,
  System.UItypes,
  Hyperspectral_Image, BaseGraf,DEMMapf,Petmar_types;

type
  THyperspectralForm = class(TForm)
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioGroup1: TRadioGroup;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox1: TCheckBox;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    BitBtn7: TBitBtn;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ListBox2: TListBox;
    BitBtn11: TBitBtn;
    BitBtn10: TBitBtn;
    CheckBox2: TCheckBox;
    BitBtn8: TBitBtn;
    TabSheet2: TTabSheet;
    BitBtn1: TBitBtn;
    ListBox1: TListBox;
    BitBtn3: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    BitBtn16: TBitBtn;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Red: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    BitBt12: TBitBtn;
    BitBtn9: TBitBtn;
    RadioGroup2: TRadioGroup;
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure TrackBar1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrackBar2KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrackBar3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrackBar3EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure BitBtn7Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
    procedure ColorImage;
    procedure Relabel;
    procedure ShowBandsOnGraph(Graph: tThisBaseGraph; UseWave : boolean = true);
    procedure ShowBandsOnGraphs;
    procedure NewBands(ShowColor : boolean);
    procedure AssociateBandDEMs(WhatFor : shortstring);
    procedure MakeScatterPlotsWithClasses;
    procedure LabelBands;
  public
    { Public declarations }
     HypersectralImage : tHypersectralImage;
     MultiGridUsed,ClassDEM,
     RedBandNum,GreenBandNum,BlueBandNum,GrayBandNum,
     RedBandDEM,GreenBandDEM,BlueBandDEM,GrayBandDEM,
     LastX,LastY : integer;
     ThreeColor,ChangeAllowed,
     HyperionImage : boolean;
     SpecLib  : tThisBaseGraph;
     BaseMap : tMapForm;
     procedure SetForHyperion;
  end;


procedure OpenHyperspectralImage(AutoOpen : boolean = false);


implementation

{$R *.dfm}

uses
   DEMEROS,
   sup_class,
   demdatabase,
   MultiGrid,
   PetMath, Petmar,DEMDefs,DEMCoord, DEMStat,nevadia_main, basemap;

type
   tHyperDoing = (hypGetCube,hypRoam);
var
   HyperDoing : tHyperDoing;


procedure THyperspectralForm.LabelBands;
begin
   ChangeAllowed := false;
   ComboBox1.Text := DEMGlb[MultiGridArray[MultiGridUsed].Grids[RedBandNum]].AreaName;
   ComboBox2.Text := DEMGlb[MultiGridArray[MultiGridUsed].Grids[GreenBandNum]].AreaName;
   ComboBox3.Text := DEMGlb[MultiGridArray[MultiGridUsed].Grids[BlueBandNum]].AreaName;
   ComboBox4.Text := DEMGlb[MultiGridArray[MultiGridUsed].Grids[GrayBandNum]].AreaName;
   ChangeAllowed := true;
end;


procedure THyperspectralForm.SetForHyperion;
var
   i : integer;
   TStr : shortstring;
begin
    {$If Defined(RecordColorImage) or Defined(RecordHyperion)} WriteLineToDebugFile('THyperspectralForm.SetForHyperion in, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}
    Panel2.Visible := true;
    Panel2.Left := 0;
    PageControl1.Left := 0;
    ScrollBox1.Visible := false;
    Panel1.Visible := false;
    ClientWidth := Panel2.Width;
    //BitBtn10Click(Nil);
    RedBandNum := 48;  //NIR
    GreenBandNum := 32;
    BlueBandNum := 18;
    GrayBandNum := RedBandNum;

    //if (MultiGridArray[MultiGridUsed].SatImageIndex = 0) then begin
       CreateDEMSelectionMap(MultiGridArray[MultiGridUsed].Grids[GrayBandNum],true,false,mtElevGray);
      {$If Defined(RecordHyperion)} WriteLineToDebugFile('Created selection map, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}

       BaseMap := DEMGlb[MultiGridArray[MultiGridUsed].Grids[GrayBandNum]].SelectionMap;
       BaseMap.MapDraw.MultiGridOnMap := MultiGridUsed;
       BaseMap.ClosingMapNotAllowed := true;
       DEMGlb[MultiGridArray[MultiGridUsed].Grids[GrayBandNum]].SelectionMap.MapDraw.MultiGridOnMap := MultiGridUsed;
       DEMGlb[MultiGridArray[MultiGridUsed].Grids[GrayBandNum]].SelectionMap.CheckProperTix;
       MultiGridArray[MultiGridUsed].MapOwner := DEMGlb[MultiGridArray[MultiGridUsed].Grids[GrayBandNum]].SelectionMap;
       BaseMap.SetMultibandToShowOnMap(GrayBandNum);
      {$If Defined(RecordHyperion)} WriteLineToDebugFile('Multiband shown, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}

       Caption := MultiGridArray[MultiGridUsed].MG_Name;
       //ListBox2.Sorted := false;
         for i := 1 to MaxGridsInMG do begin
            if ValidDEM(MultiGridArray[MultiGridUsed].Grids[i]) then begin
               //if MultiGridArray[MultiGridUsed].SatImageIndex = 0 then TStr := DEMGlb[MultiGridArray[MultiGridUsed].Grids[i]].AreaName
               //else TStr := SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i];
               TStr := DEMGlb[MultiGridArray[MultiGridUsed].Grids[i]].AreaName;
               //ListBox2.Items.Add(TStr);
               ComboBox1.Items.Add(TStr);
               ComboBox2.Items.Add(TStr);
               ComboBox3.Items.Add(TStr);
               ComboBox4.Items.Add(TStr);
            end;
         end;

         LabelBands;

         if (MultiGridArray[MultiGridUsed].SatImageIndex = 0) then begin
(*
            ComboBox1.Visible := false;
            ComboBox2.Visible := false;
            ComboBox3.Visible := false;
            ComboBox4.Visible := false;
*)
         end
         else begin
(*
            ListBox2.Visible := false;
            ComboBox1.Text := DEMGlb[MultiGridArray[MultiGridUsed].Grids[i]].AreaName;
            ComboBox2.Text := DEMGlb[MultiGridArray[MultiGridUsed].Grids[i]].AreaName;
            ComboBox3.Text := SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[BlueBandNum];
            ComboBox4.Text := SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[GrayBandNum];
*)
         end;
    //end
    //else begin
       //BaseMap := SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].SelectionMap;
    //end;
    BitBtn10Click(Nil);
    wmDEM.FormPlacementInCorner(self);
    Show;
    {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.SetForHyperion out'); {$EndIf}
end;



procedure OpenHyperspectralImage;
var
   HyperspectralForm : THyperspectralForm;
   success : boolean;
   Filter : byte;
   ThisOne : integer;
begin
   StopSplashing;
   Filter := 1;
   if AutoOpen or Petmar.GetDOSPath('hyperspectral imagery',LastHypFile) then begin
      if StrUtils.AnsiContainsText(LastHypFile,'EO1H') or FileExtEquals(LastHypFile,'.TIF') then begin
         if FindOpenMultigrid(ThisOne) then begin
            if StrUtils.AnsiContainsText(LastHypFile,'EO1H') then begin
               OpenHyperionMultigrid(ThisOne,LastHypFile);
            end;
         end;
      end
      else begin
         HyperspectralForm := THyperspectralForm.Create(Application);
         HyperspectralForm.HypersectralImage := tHypersectralImage.Create(LastHypFile,success);
         HyperspectralForm.TrackBar1.Max := HyperspectralForm.HypersectralImage.NumBands;
         HyperspectralForm.TrackBar2.Max := HyperspectralForm.HypersectralImage.NumBands;
         HyperspectralForm.TrackBar3.Max := HyperspectralForm.HypersectralImage.NumBands;
         HyperspectralForm.ColorImage;
         HyperspectralForm.Caption := 'Hyperspectral image ' + ExtractFileName(LastHypFile);
         HyperspectralForm.Show;
      end;
   end;
end;


procedure THyperspectralForm.AssociateBandDEMs(WhatFor: shortstring);
begin
   BaseMap.MapDraw.BaseTitle := MultiGridArray[MultiGridUsed].MG_short_name + ' R=' + IntToStr(RedBandNum) + ' G=' + IntToStr(GreenBandNum)  + ' B=' + IntToStr(BlueBandNum);
   ThreeColor := true;
   RedBandDEM := MultiGridArray[MultiGridUsed].Grids[RedBandNum];
   GreenBandDEM := MultiGridArray[MultiGridUsed].Grids[GreenBandNum];
   BlueBandDEM := MultiGridArray[MultiGridUsed].Grids[BlueBandNum];
   BaseMap.SetRGBMultibandToShowOnMap('User bands',RedBandDEM,GreenBandDEM,BlueBandDEM);
end;


procedure THyperspectralForm.BitBtn10Click(Sender: TObject);
begin
   //if (MultiGridArray[MultiGridUsed].SatImageIndex = 0) then begin
      RedBandNum := 48;
      GreenBandNum := 32;
      BlueBandNum := 18;
   //end;
   if (Sender <> Nil) then begin
      AssociateBandDEMs('False color');
      {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.BitBtn10Click out, title=' + BaseMap.MapDraw.BaseTitle); {$EndIf}
   end;
end;

procedure THyperspectralForm.BitBtn11Click(Sender: TObject);
begin
   //if MultiGridArray[MultiGridUsed].SatImageIndex = 0  then begin
      RedBandNum := 32;
      GreenBandNum := 18;
      BlueBandNum := 14;
   //end;
   if (Sender <> Nil) then begin
      AssociateBandDEMs('True color');
     {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.BitBtn11Click out, title=' + BaseMap.MapDraw.BaseTitle); {$EndIf}
   end;
end;

procedure THyperspectralForm.BitBtn13Click(Sender: TObject);
begin
   SingleDEMHistogram(MultiGridArray[MultiGridUsed].Grids[RedBandNum],true);
   SingleDEMHistogram(MultiGridArray[MultiGridUsed].Grids[BlueBandNum],true);
   SingleDEMHistogram(MultiGridArray[MultiGridUsed].Grids[GreenBandNum],true);
   SingleDEMHistogram(MultiGridArray[MultiGridUsed].Grids[GrayBandNum],true);
end;

procedure THyperspectralForm.BitBtn14Click(Sender: TObject);
var
   Band1,Band2 : integer;
begin
   GridScatterGram(DEMGlb[MultiGridArray[MultiGridUsed].Grids[RedBandNum]].FullDEMGridLimits,MultiGridArray[MultiGridUsed].Grids[RedBandNum],MultiGridArray[MultiGridUsed].Grids[GreenBandNum]);
   GridScatterGram(DEMGlb[MultiGridArray[MultiGridUsed].Grids[RedBandNum]].FullDEMGridLimits,MultiGridArray[MultiGridUsed].Grids[RedBandNum],MultiGridArray[MultiGridUsed].Grids[BlueBandNum]);
   GridScatterGram(DEMGlb[MultiGridArray[MultiGridUsed].Grids[GreenBandNum]].FullDEMGridLimits,MultiGridArray[MultiGridUsed].Grids[GreenBandNum],MultiGridArray[MultiGridUsed].Grids[BlueBandNum]);
end;

procedure THyperspectralForm.BitBtn15Click(Sender: TObject);
var
   Incr : integer;
begin
   Incr := 10;
   ReadDefault('Sampling interval',Incr);
   GridCorrelationMatrix(incr);
end;

procedure THyperspectralForm.BitBtn16Click(Sender: TObject);
begin
   BaseMap.SetMultibandToShowOnMap(GrayBandDEM);
end;

procedure THyperspectralForm.BitBtn1Click(Sender: TObject);
begin
   SpectralLibraryGraph('');
end;


procedure THyperspectralForm.BitBtn2Click(Sender: TObject);
begin
   if (RadioGroup1.ItemIndex = 0) then HypersectralImage.GrayBand := HypersectralImage.RedBand
   else HypersectralImage.GrayBand := 0;
   with HypersectralImage do DisplayCube(GrayBand,RedBand,GreenBand,BlueBand);
end;



procedure THyperspectralForm.MakeScatterPlotsWithClasses;

         function MakeGraph(xaxis,yaxis,colors : integer) : TThisbasegraph;
         var
            x,y : integer;
            zx,zy,zc : float32;
            rfile : file;
            v : array[1..3] of float32;
         begin
            if ValidDEM(xaxis) and ValidDEM(yaxis) and ValidDEM(Colors) then begin
               ShowHourglassCursor;
               Result := TThisbasegraph.Create(Application);
               Result.OpenXYColorFile(rfile);
               for x := 0 to pred(DEMGlb[xaxis].DEMheader.NumCol) do begin
                  for y := 0 to pred(DEMGlb[xaxis].DEMheader.NumRow) do begin
                     if DEMGlb[xaxis].GetElevMetersOnGrid(x,y,v[1]) and DEMGlb[yaxis].GetElevMetersOnGrid(x,y,v[2]) and DEMGlb[Colors].GetElevMetersOnGrid(x,y,zc) then begin
                        v[3] := WingraphColors[round(zc) mod 15];
                        BlockWrite(Rfile,v,1);
                     end;
                  end;
               end;
               Result.ClosePointDataFile(rfile);
               Result.GraphDraw.HorizLabel := DEMGlb[xaxis].AreaName;
               Result.GraphDraw.VertLabel := DEMGlb[yaxis].AreaName;
               Result.AutoScaleAndRedrawDiagram(true,true,false,false);
            end;
         end;

begin
   MakeGraph(RedBandDEM,BlueBandDEM,ClassDEM);
   MakeGraph(RedBandDEM,GreenBandDEM,ClassDEM);
   MakeGraph(GreenBandDEM,BlueBandDEM,ClassDEM);
   ShowDefaultCursor;
end;


procedure THyperspectralForm.BitBtn3Click(Sender: TObject);
begin
   ClassDEM := UnsupervisedClassification(MultiGridUsed,BaseMap);
   MakeScatterPlotsWithClasses;
   BitBtn9.Enabled := true;
end;

procedure THyperspectralForm.BitBtn4Click(Sender: TObject);
begin
   HyperDoing := hypGetCube;
end;

procedure THyperspectralForm.BitBtn5Click(Sender: TObject);
begin
   with HypersectralImage do begin
      RedBand := TrackBar1.Position;
      BlueBand := TrackBar3.Position;
      GreenBand := TrackBar2.Position;
      HypersectralImage.ScatterPlot(RedBand,GreenBand,BlueBand);
   end;
end;

procedure THyperspectralForm.BitBtn6Click(Sender: TObject);
begin
   TrackBar1.Position := 35;
   TrackBar2.Position := 19;
   TrackBar3.Position := 9;
   ColorImage;
end;


procedure THyperspectralForm.BitBtn7Click(Sender: TObject);
begin
   ColorImage;
end;

procedure THyperspectralForm.BitBtn8Click(Sender: TObject);
var
   ThisDB : integer;
   GoodData : shortstring;
begin
   ThisDB := MultiGridArray[BaseMap.MapDraw.MultiGridOnMap].BasicStats;
   GISDB[ThisDB].EmpSource.Enabled := false;
   GISDB[ThisDB].MyData.First;
   ListBox2.Clear;
   ListBox2.Sorted := false;
   while not GISDB[ThisDB].MyData.eof do begin
      if GISDB[ThisDB].MyData.GetFieldByNameAsString('MIN') = GISDB[ThisDB].MyData.GetFieldByNameAsString('MAX') then begin
         GISDB[ThisDB].MyData.Edit;
         GISDB[ThisDB].MyData.SetFieldByNameAsString('USE','N');
      end;
      if GISDB[ThisDB].MyData.GetFieldByNameAsString('USE') = 'N' then GoodData := '   No data in band'  else GoodData := '';
      ListBox2.Items.Add( GISDB[ThisDB].MyData.GetFieldByNameAsString('BAND') + GoodData);
      GISDB[ThisDB].MyData.Next;
   end;
   GISDB[ThisDB].EmpSource.Enabled := true;
end;

procedure THyperspectralForm.BitBtn9Click(Sender: TObject);
begin
   MakeScatterPlotsWithClasses;
end;

procedure THyperspectralForm.CheckBox2Click(Sender: TObject);
begin
   MDDef.BandsByWavelength := CheckBox2.Checked;
end;


procedure THyperspectralForm.ShowBandsOnGraph(Graph : tThisBaseGraph; UseWave : boolean = true);

     procedure Line(Band : integer; aColor : tColor);
     var
        x : integer;
     begin
        if UseWave then x:= Graph.GraphDraw.GraphX(HypersectralImage.BandCenters[Band])
        else x := Graph.GraphDraw.GraphX(Band);
        Graph.Image1.Canvas.Pen.Color := acolor;
        Graph.Image1.Canvas.MoveTo(x, Graph.GraphDraw.GraphY(0));
        Graph.Image1.Canvas.LineTo(x, 0);
     end;

begin
   if CheckBox1.Checked and (Graph <> Nil) then  begin
      Graph.RedrawDiagram11Click(Nil);
      Image1.Canvas.Pen.Width := 3;
      if (RadioGroup1.ItemIndex = 0) then Line(HypersectralImage.RedBand,clGray)
      else Line(HypersectralImage.RedBand,clRed);
      if (RadioGroup1.ItemIndex = 1) then  begin
         Line(HypersectralImage.GreenBand,clLime);
         Line(HypersectralImage.BlueBand,clBlue);
      end;
   end;
end;


procedure THyperspectralForm.ShowBandsOnGraphs;
begin
   ShowBandsOnGraph(SpecLib);
   if HyperionImage then exit;
   ShowBandsOnGraph(HypersectralImage.RefGraph);
   ShowBandsOnGraph(HypersectralImage.RefGraph2,false);
end;


procedure THyperspectralForm.Relabel;
begin
   if HyperionImage then exit;
    with HypersectralImage do begin
       RedBand := TrackBar1.Position;
       GreenBand := TrackBar2.Position;
       BlueBand := TrackBar3.Position;
       if (RadioGroup1.ItemIndex = 0) then begin
          GrayBand := RedBand;
          Label1.Caption := 'Grayscale:  Band ' + IntToStr(RedBand) + RealToString(HypersectralImage.BandCenters[RedBand],10,2) + ' microns';
       end
       else begin
          GrayBand := 0;
          Label1.Caption := 'Red:  Band ' + IntToStr(RedBand) + RealToString(HypersectralImage.BandCenters[RedBand],10,2) + ' microns';
       end;
       Label2.Caption := 'Green:  Band ' + IntToStr(GreenBand) + RealToString(HypersectralImage.BandCenters[GreenBand],10,2) + ' microns';
       Label3.Caption := 'Blue:  Band ' + IntToStr(BlueBand) + RealToString(HypersectralImage.BandCenters[BlueBand],10,2) + ' microns';
    end;
    BitBtn7.Enabled := true;
end;

procedure THyperspectralForm.ColorImage;
var
   Bitmap : tMyBitmap;
begin
   if HyperionImage then exit;
   {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.ColorImage in'); {$EndIf}

    with HypersectralImage do begin
       Relabel;
       ShowHourglassCursor;

       Bitmap := HypersectralImage.GetBandBitmap(GrayBand,RedBand,GreenBand,BlueBand);
       Image1.Picture.Graphic := Bitmap;
       Bitmap.Free;
       {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.ColorImage bmp done '); {$EndIf}

       HypersectralImage.DisplayHistogram(GrayBand,RedBand,GreenBand,BlueBand);
       ShowBandsOnGraphs;
      {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.ColorImage out '); {$EndIf}
    end;
    BitBtn7.Enabled := false;
    ShowDefaultCursor;
end;

procedure THyperspectralForm.ComboBox1Change(Sender: TObject);
begin
   if ChangeAllowed then NewBands(true);
end;


procedure THyperspectralForm.ComboBox2Change(Sender: TObject);
begin
   if ChangeAllowed then NewBands(true);
end;

procedure THyperspectralForm.ComboBox3Change(Sender: TObject);
begin
   if ChangeAllowed then NewBands(true);
end;

procedure THyperspectralForm.ComboBox4Change(Sender: TObject);
begin
   NewBands(false);
end;


procedure THyperspectralForm.NewBands(ShowColor : boolean);
var
   i : integer;
   fName : shortstring;
begin


(*
   for i := 1 to SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].NumBands do begin
      if ComboBox1.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then RedBandNum := i;
      if ComboBox2.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then GreenBandNum := i;
      if ComboBox3.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then BlueBandNum := i;
      if ComboBox4.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then GrayBandNum := i;
   end;
*)

   for i := 1 to MaxGridsInMG do begin
      if ValidDEM(MultiGridArray[MultiGridUsed].Grids[i]) then begin
         //if MultiGridArray[MultiGridUsed].SatImageIndex = 0 then TStr := DEMGlb[MultiGridArray[MultiGridUsed].Grids[i]].AreaName
         //else TStr := SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i];
         fName := DEMGlb[MultiGridArray[MultiGridUsed].Grids[i]].AreaName;
         if ComboBox1.Text = fName then RedBandNum := i;
         if ComboBox2.Text = fName then GreenBandNum := i;
         if ComboBox3.Text = fName then BlueBandNum := i;
         if ComboBox4.Text = fName then GrayBandNum := i;
      end;
   end;

   if ShowColor and (RadioGroup2.ItemIndex  = 3) then AssociateBandDEMs('User picks')
   else BaseMap.SetMultibandToShowOnMap(GrayBandNum);
end;


procedure THyperspectralForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.MaxImagePercentile);
end;

procedure THyperspectralForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.MinImagePercentile);
end;

procedure THyperspectralForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   BaseMap.ClosingMapNotAllowed := false;
   if (SpecLib <> Nil) then SpecLib.Destroy;
   MultiGridArray[MultiGridUsed].Destroy;
end;


procedure THyperspectralForm.FormCreate(Sender: TObject);
var
   Files : tStringList;
   i : integer;
begin
   {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.FormCreate in');  {$EndIf}
   ScrollBox1.DoubleBuffered := True;  //for image panning
   ScrollBox1.HorzScrollBar.Visible := true;
   ScrollBox1.VertScrollBar.Visible := true;
   HyperionImage := false;
   ThreeColor := false;
   Files := nil;
   SpecLib := nil;
   Petmar.FindMatchingFiles(MainMapData  + 'spectral_library\','*.txt',Files,1);
   for I := 0 to pred(Files.Count) do ListBox1.Items.Add(ExtractFileName(Files.Strings[i]));
   Files.Free;
   Edit2.Text := RealToString(MDDef.MinImagePercentile,-12,-2);
   Edit1.Text := RealToString(MDDef.MaxImagePercentile,-12,-2);
   CheckBox2.Checked := MDDef.BandsByWavelength;
   wmDEM.FormPlacementInCorner(self);
   {$IfDef RecordColorImage} WriteLineToDebugFile('THyperspectralForm.FormCreate out'); {$EndIf}
end;

procedure THyperspectralForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\hyperpsectral_imagery.htm');
end;

procedure THyperspectralForm.Image1MouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
   wmdem.StatusBar1.Panels[1].Text := 'x=' + IntToStr(x) + '    & y=' + IntToStr(y);
end;


procedure THyperspectralForm.ListBox1Click(Sender: TObject);
var
   Left,Top : integer;
   Old : boolean;
   MaxVert : float64;
   fName : PathStr;
begin
   Old := false;
   if (SpecLib <> Nil) then begin
      Left := SpecLib.Left;
      Top := SpecLib.Top;
      Old := true;
      MaxVert := SpecLib.GraphDraw.MaxVertAxis;
      SpecLib.CanCloseGraph := true;
      SpecLib.Destroy;
   end
   else MaxVert := 100;
   if (Sender = BitBtn1) then fName := ''
   else fName := MainMapData  + 'spectral_library\' + ListBox1.Items[ListBox1.ItemIndex];
   SpecLib := SpectralLibraryGraph(fName,MaxVert);
   SpecLib.CanCloseGraph := false;
   if Old then begin
      SpecLib.Left := Left;
      SpecLib.Top := Top;
   end;
   ShowBandsOnGraphs;
end;

procedure THyperspectralForm.ListBox2Click(Sender: TObject);
var
   tStr : shortstring;
   i : integer;
begin
   if MultiGridArray[MultiGridUsed].SatImageIndex = 0 then begin
      tStr := ListBox2.Items[ListBox2.ItemIndex];
      Delete(tStr,1,4);
      tStr := Petmar_types.BeforeSpecifiedString(Tstr,' ');
      GrayBandNum := StrToInt(TStr);
   end
   else begin
      for i := 1 to SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].NumBands do begin
         if ComboBox1.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then RedBandNum := i;
         if ComboBox2.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then GreenBandNum := i;
         if ComboBox3.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then BlueBandNum := i;
         if ComboBox4.Text = SatImage[MultiGridArray[MultiGridUsed].SatImageIndex].BandLongName[i] then GrayBandNum := i;
      end;
   end;
   ThreeColor := false;
   BaseMap.SetMultibandToShowOnMap(GrayBandNum);
end;

procedure THyperspectralForm.RadioGroup1Click(Sender: TObject);
begin
   if HyperionImage then exit;
   TrackBar3.Enabled := RadioGroup1.ItemIndex = 1;
   TrackBar2.Enabled := RadioGroup1.ItemIndex = 1;
   Label2.Enabled := RadioGroup1.ItemIndex = 1;
   Label3.Enabled := RadioGroup1.ItemIndex = 1;
   ColorImage;
end;

procedure THyperspectralForm.RadioGroup2Click(Sender: TObject);
begin
   case RadioGroup2.ItemIndex of
      0 : BaseMap.SetMultibandToShowOnMap(GrayBandNum);
      1 : BitBtn11Click(Sender);
      2 : BitBtn10Click(Sender);
      3 : AssociateBandDEMs('Pick bands');
   end;
end;

procedure THyperspectralForm.TrackBar1Change(Sender: TObject);
begin
   Relabel;
end;

procedure THyperspectralForm.TrackBar1KeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   ColorImage;
end;

procedure THyperspectralForm.TrackBar2Change(Sender: TObject);
begin
   Relabel;
end;

procedure THyperspectralForm.TrackBar2KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   ColorImage;
end;

procedure THyperspectralForm.TrackBar3Change(Sender: TObject);
begin
   Relabel;
end;

procedure THyperspectralForm.TrackBar3EndDrag(Sender, Target: TObject; X,Y: Integer);
begin
   ColorImage;
end;

procedure THyperspectralForm.TrackBar3KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   ColorImage;
end;


initialization
   {$IfDef MessageStartUpProblems} MessageToContinue('Startup hyp_display'); {$EndIf}
   HyperDoing := hypRoam;
finalization
   {$IfDef RecordColorImage} WriteLineToDebugFile('RecordColorImage active in hyp_display'); {$EndIf}
end.
