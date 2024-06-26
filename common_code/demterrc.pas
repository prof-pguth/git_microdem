unit Demterrc;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordTerrCats}
{$EndIf}


interface

uses
   Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls,
   System.SysUtils,System.Math,
   Windows, Classes, Graphics, Forms,    Dialogs,
   DEMDefs, DEMMapf;

type
   tTerrainCatOpts = (tcNormal,tcElevOnly,tcSlopeOnly,tcTwoAspects);

type
  TGetTerrC = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    Panel1: TPanel;
    CheckBox15: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox14: TCheckBox;
    Panel2: TPanel;
    Edit2: TEdit;
    Edit1: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    Panel3: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit8: TEdit;
    Edit4: TEdit;
    Edit9: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Panel4: TPanel;
    Edit5: TEdit;
    Label7: TLabel;
    Label11: TLabel;
    Edit7: TEdit;
    Edit6: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Memo1: TMemo;
    Panel5: TPanel;
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    CheckBox10: TCheckBox;
    OKBtn: TBitBtn;
    BitBtn2: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn4: TBitBtn;
    Label16: TLabel;
    Label17: TLabel;
    CheckBox17: TCheckBox;
    Panel6: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Label20: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    Label21: TLabel;
    Edit16: TEdit;
    Edit17: TEdit;
    CheckBox18: TCheckBox;
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox17Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure CheckBox18Click(Sender: TObject);
  private
    { Private declarations }
    UserChanging : boolean;
    procedure CheckAvailable;
    procedure SizeForm;
    procedure DefineTerrainCategory;
  public
    { Public declarations }
     MapOwner : tMapform;
     DEMUsed : integer;
     FullOptions : boolean;
     TerrainCategory : tTerrainCatDefinition;
     CatDoing : tTerrainCatOpts;
  end;


function GetTerrainCategory(inCatDoing : tTerrainCatOpts; TheMap : tMapForm; inDEM : integer; var inTerrainCategory : tTerrainCatDefinition; FullOptions : boolean) : boolean;


implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types,Petmar_db,PETMath,PetImage,
   DEMCoord, demdef_routines;



function GetTerrainCategory;
var
  GetTerrC: TGetTerrC;
begin
   {$IfDef RecordTerrCats} WriteLineToDebugFile('GetTerrainCategory in=' + DEMGlb[inDEM].TerrainCategoryLabel(inTerrainCategory)); {$EndIf}
   GetTerrC := tGetTerrC.Create(Application);
   GetTerrC.FullOptions := FullOptions;
   with GetTerrC do begin
      MapOwner := theMap;
      DEMUsed := inDEM;
      CatDoing := inCatDoing;
      TerrainCategory := inTerrainCategory;
      SymbolOnButton(BitBtn3,MapOwner.DrSymbol);
      Label3.Caption := 'Z value: ' + ElevUnitsAre(DEMGlb[DEMUsed].DEMheader.ElevUnits);
      Label8.Visible := DEMGlb[DEMUsed].DEMheader.ElevUnits = euMeters;
      Label9.Visible := Label8.Visible;
      Label10.Visible := Label8.Visible;

      if not FullOptions then begin
         Edit3.Visible := false;
         Edit4.Visible := false;
         Edit5.Visible := false;
         Edit6.Visible := false;
         Edit7.Visible := false;
         Edit8.Visible := false;
         Edit9.Visible := false;
         Label1.Visible := false;
         Label2.Visible := false;
         Label4.Visible := false;
         Label5.Visible := false;
         Label7.Visible := false;
         Label8.Visible := false;
         Label9.Visible := false;
         Label10.Visible := false;
         Label11.Visible := false;
         GroupBox1.Visible := false;
      end;
      BitBtn1.Enabled := (CatDoing in [tcNormal]);
      BitBtn10.Enabled := (CatDoing = tcElevOnly) or (TerrainCategory.UseElevation and (not TerrainCategory.UseRelief) and (not TerrainCategory.UseSlope) and (not TerrainCategory.UseAspect));
      BitBtn3.Enabled := (CatDoing in [tcElevOnly,tcSlopeOnly]);
      BitBtn4.Enabled := (CatDoing <> tcTwoAspects);
      SizeForm;
      {$IfDef RecordTerrCats} WriteLineToDebugFile('GetTerrainCategory form sized'); {$EndIf}

      BitBtn10.Enabled := (CatDoing = tcElevOnly);

      CheckBox13.Checked := TerrainCategory.UseElevation and (CatDoing <> tcTwoAspects);
      CheckBox14.Checked := TerrainCategory.UseRelief and (CatDoing <> tcTwoAspects);
      CheckBox15.Checked := TerrainCategory.UseSlope and (CatDoing <> tcTwoAspects);
      CheckBox16.Checked := TerrainCategory.UseAspect and (CatDoing <> tcTwoAspects);
      CheckBox17.Checked := (CatDoing = tcTwoAspects);

      Edit1.Text := RealToString(TerrainCategory.CatMinElev,-12,-4);
      Edit2.Text := RealToString(TerrainCategory.CatMaxElev,-12,-4);
      Edit3.Text := RealToString(TerrainCategory.CatMinSlope,-12,-2);
      Edit4.Text := RealToString(TerrainCategory.CatMaxSlope,-12,-2);
      Edit5.Text := IntegerToString(TerrainCategory.CatMinRelief,-12);
      Edit6.Text := IntegerToString(TerrainCategory.CatMaxRelief,-12);
      Edit7.Text := IntegerToString(TerrainCategory.CatReliefRadius,-12);
      Edit8.Text := RealToString(ArcTan(0.01 * TerrainCategory.CatMinSlope) / DegToRad,-8,-1);
      Edit9.Text := RealToString(ArcTan(0.01 * TerrainCategory.CatMaxSlope) / DegToRad,-8,-1);
      Label9.Caption := RealToString(TerrainCategory.CatMinElev / FeetToMeters,-8,-1);
      Label10.Caption := RealToString(TerrainCategory.CatMaxElev / FeetToMeters,-8,-1);
      CheckBox10.Checked := MDDef.IHSTerrainCategory;
      CheckBox10.Enabled := (CatDoing in [tcNormal,tcTwoAspects,tcSlopeOnly]);

      ColorBitBtn(BitBtn1,TerrainCategory.CatColor);

      if (CatDoing = tcTwoAspects) then begin
         {$IfDef ExGeology}
         {$Else}
            Edit10.Text := MDDef.FPMinAsp1.ToString;
            Edit11.Text := MDDef.FPMaxAsp1.ToString;
            Edit12.Text := MDDef.FPMinAsp2.ToString;
            Edit13.Text := MDDef.FPMaxAsp2.ToString;
            Edit14.Text := MDDef.FPMinSlope1.ToString;
            Edit15.Text := MDDef.FPMinSlope2.ToString;
            Edit16.Text := MDDef.FPMaxSlope1.ToString;
            Edit17.Text := MDDef.FPMaxSlope2.ToString;
         {$EndIf}

         BitBtn4.Visible := false;
         BitBtn7.Visible := false;
         BitBtn8.Visible := false;
         BitBtn9.Visible := false;
         BitBtn10.Visible := false;

         GetTerrC.Caption := 'Identify focal planes';
         ColorBitBtn(BitBtn5,MDDef.ColorFP1);
         ColorBitBtn(BitBtn6,MDDef.ColorFP2);
         {$IfDef RecordTerrCats} WriteLineToDebugFile('GetTerrainCategory FP colored'); {$EndIf}
      end;
      if (CatDoing = tcSlopeOnly) then begin
         Edit4.Enabled := false;
         Edit9.Enabled := false;
         GetTerrC.Caption := 'Excessive slopes';
      end;
      if (CatDoing in [tcTwoAspects,tcSlopeOnly]) then OKBtn.Visible := false;

       if (CatDoing in [tcNormal]) then begin
         inTerrainCategory := TerrainCategory;
       end;
       Petmar.PlaceFormAtMousePosition(GetTerrC);
       GetTerrC.FormStyle := fsStayOnTop;
       GetTerrC.Show;
      {$IfDef RecordTerrCats} WriteLineToDebugFile('GetTerrainCategory out=' + DEMGlb[inDEM].TerrainCategoryLabel(TerrainCategory)); {$EndIf}
   end;
end;


procedure TGetTerrC.SizeForm;
var
   Top : integer;
begin
   Panel1.Visible := (CatDoing = tcNormal) and FullOptions;
   Panel2.Visible := (CatDoing in [tcNormal,tcElevOnly]) and CheckBox13.Checked;
   Panel3.Visible := (CatDoing in [tcNormal,tcSlopeOnly]) and FullOptions and CheckBox15.Checked;
   Panel4.Visible := (CatDoing = tcNormal) and FullOptions  and CheckBox14.Checked;
   Panel6.Visible := (CatDoing in [tcTwoAspects]) and CheckBox17.Checked;
   GroupBox1.Visible := (CatDoing = tcNormal) and FullOptions and CheckBox16.Checked;;
   Memo1.Visible := CatDoing in [tcElevOnly,tcSlopeOnly,tcTwoAspects];
   if not Panel3.Visible then CheckBox15.Checked := false;

   Top := 0;
   if Panel1.Visible then begin
      Panel1.Top := Top;
      Top := Top + Panel1.Height;
   end;
   if Panel2.Visible then begin
      Panel2.Top := Top;
      Top := Top + Panel2.Height;
   end;
   if Panel3.Visible then begin
      Panel3.Top := Top;
      Top := Top + Panel3.Height;
   end;
   if Panel4.Visible then begin
      Panel4.Top := Top;
      Top := Top + Panel4.Height;
   end;
   if Panel6.Visible then begin
      Panel6.Top := Top;
      Top := Top + Panel6.Height;
   end;
   if GroupBox1.Visible then begin
      GroupBox1.Top := Top;
      Top := Top +GroupBox1.Height;
   end;
   if Memo1.Visible then begin
      Memo1.Top := Top;
      Top := Top + Memo1.Height;
   end;
   Panel5.Top := Top;
   ClientHeight := Top + Panel5.Height;
end;

procedure TGetTerrC.CheckAvailable;
begin
   Edit1.Enabled := CheckBox13.Checked;
   Edit2.Enabled := CheckBox13.Checked;
   Label3.Enabled := CheckBox13.Checked;
   Label8.Enabled := CheckBox13.Checked;
   Label9.Enabled := CheckBox13.Checked;
   Label10.Enabled := CheckBox13.Checked;

   Edit3.Enabled := CheckBox15.Checked;
   Edit4.Enabled := CheckBox15.Checked;
   Edit8.Enabled := CheckBox15.Checked;
   Edit9.Enabled := CheckBox15.Checked;
   Label4.Enabled := CheckBox15.Checked;
   Label5.Enabled := CheckBox15.Checked;

   Edit5.Enabled := CheckBox14.Checked;
   Edit6.Enabled := CheckBox14.Checked;
   Edit7.Enabled := CheckBox14.Checked;
   Label7.Enabled := CheckBox15.Checked;
   Label11.Enabled := CheckBox15.Checked;

   GroupBox1.Enabled := CheckBox16.Checked;
   CheckBox1.Enabled := CheckBox16.Checked;
   CheckBox2.Enabled := CheckBox16.Checked;
   CheckBox3.Enabled := CheckBox16.Checked;
   CheckBox4.Enabled := CheckBox16.Checked;
   CheckBox5.Enabled := CheckBox16.Checked;
   CheckBox6.Enabled := CheckBox16.Checked;
   CheckBox7.Enabled := CheckBox16.Checked;
   CheckBox8.Enabled := CheckBox16.Checked;
   CheckBox9.Enabled := CheckBox16.Checked;
   CheckBox11.Enabled := CheckBox16.Checked;
   CheckBox12.Enabled := CheckBox16.Checked;
   SizeForm;
end;


procedure TGetTerrC.Edit3Change(Sender: TObject);
var
   f : float64;
begin
   if (not UserChanging) then begin
      UserChanging := true;
      CheckEditString(Edit3.Text,f);
      Edit8.Text := RealToString(ArcTan(0.01 * f) / DegToRad,-8,-1);
      UserChanging := false;
   end;
end;


procedure TGetTerrC.Edit8Change(Sender: TObject);
var
   f : float64;
begin
   if (not UserChanging) then begin
      UserChanging := true;
      CheckEditString(Edit8.Text,f);
      Edit3.Text := RealToString(100 * TanDeg(f),-8,-1);
      UserChanging := false;
   end;
end;


procedure TGetTerrC.Edit4Change(Sender: TObject);
begin
   if (not UserChanging) then begin
      UserChanging := true;
      CheckEditString(Edit4.Text,TerrainCategory.CatMaxSlope);
      if (Sender = Edit4) then Edit9.Text := RealToString(ArcTan(0.01 * TerrainCategory.CatMaxSlope) / DegToRad,-8,-1);
      UserChanging := false;
   end;
end;


procedure TGetTerrC.Edit9Change(Sender: TObject);
var
   f : float64;
begin
   if (not UserChanging) then begin
      CheckEditString(Edit9.Text,f);
      Edit4.Text :=  RealToString(100 * TanDeg(f),-8,-1);
   end;
end;


procedure TGetTerrC.HelpBtnClick(Sender: TObject);
begin
   if (CatDoing in [tcTwoAspects]) then DisplayHTMLTopic('html\id_focal.htm')
   else DisplayHTMLTopic('html\tbme2515.htm');
end;


procedure TGetTerrC.OKBtnClick(Sender: TObject);
begin
   {$IfDef RecordTerrCats} WriteLineToDebugFile('TGetTerrC.OKBtnClick'); {$EndIf}
   DefineTerrainCategory;
   MapOwner.MapDraw.MapOverlays.ovTerrainCat.Add(DEMDef_routines.TerrainCategoryToString(TerrainCategory));
   Close;
end;

procedure TGetTerrC.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,TerrainCategory.CatMinElev);
   Label9.Caption := RealToString(TerrainCategory.CatMinElev / FeetToMeters,-8,-1);
   Label16.Caption := RealToString(DEMGlb[DEMUsed].PercentileofElevation(TerrainCategory.CatMinElev),-18,-1);
end;


procedure TGetTerrC.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,TerrainCategory.CatMaxElev);
   Label10.Caption := RealToString(TerrainCategory.CatMaxElev / FeetToMeters,-8,-1);
   Label17.Caption := RealToString(DEMGlb[DEMUsed].PercentileOfElevation(TerrainCategory.CatMaxElev),-18,-1);
end;


procedure TGetTerrC.CheckBox10Click(Sender: TObject);
begin
   MDDef.IHSTerrainCategory := CheckBox10.Checked;
end;

procedure TGetTerrC.CheckBox13Click(Sender: TObject);
begin
   CheckAvailable;
end;

procedure TGetTerrC.CheckBox14Click(Sender: TObject);
begin
  CheckAvailable;
end;

procedure TGetTerrC.CheckBox15Click(Sender: TObject);
begin
   CheckAvailable;
end;

procedure TGetTerrC.CheckBox16Click(Sender: TObject);
begin
   if CheckBox16.Checked then CheckBox17.Checked := false;
   CheckAvailable;
end;

procedure TGetTerrC.CheckBox17Click(Sender: TObject);
begin
   if CheckBox17.Checked then CheckBox16.Checked := false;
   CheckAvailable;
end;

procedure TGetTerrC.CheckBox18Click(Sender: TObject);
begin
   MDDef.TerrainCatLegend.DrawItem := CheckBox18.Checked;
end;

procedure TGetTerrC.CheckBox9Click(Sender: TObject);
begin
   CheckBox1.Checked := CheckBox9.Checked;
   CheckBox2.Checked := CheckBox9.Checked;
   CheckBox3.Checked := CheckBox9.Checked;
   CheckBox4.Checked := CheckBox9.Checked;
   CheckBox5.Checked := CheckBox9.Checked;
   CheckBox6.Checked := CheckBox9.Checked;
   CheckBox7.Checked := CheckBox9.Checked;
   CheckBox8.Checked := CheckBox9.Checked;
   CheckBox11.Checked := CheckBox9.Checked;
   CheckBox12.Checked := CheckBox9.Checked;
end;


procedure TGetTerrC.BitBtn10Click(Sender: TObject);
var
   fName : PathStr;
   DBlist : tStringList;
begin

   if (CatDoing = tcElevOnly) then begin
      DBlist := tStringList.Create;
      DefineTerrainCategory;
      DBlist.Add('LAT,LONG,ELEV');
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.HighZ := TerrainCategory.CatMaxElev;
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.LowZ := TerrainCategory.CatMinElev;
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.PlotExtremeZValues(nil,dblist);
      fName := Petmar.NextFileNumber(MDTempDir, 'terrain_cat_',DefaultDBExt);
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.StringListToLoadedDatabase(DBList,fName);
      EndProgress;
   end;
end;

procedure TGetTerrC.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,TerrainCategory.CatColor);
end;


procedure TGetTerrC.BitBtn2Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   MinSlope,MinSlope2,
   MaxSlope,MaxSlope2 : float64;

    function AspectOverlay : tMyBitmap;
    var
       FP1MatchSlope,FP1MatchAspect,
       FP2MatchSlope,FP2MatchAspect,
       FP1Count,FP2Count,TotCount : int64;
       xg1,yg1,i,j    : integer;
       P0      : pRGB;
       Complex,Complex2 : boolean;
       SlopeAspectRec : tSlopeAspectRec;

       function GoodPoint(Complex : boolean; MinAspectDir,MaxAspectDir,MinSlope,MaxSlope : float64; var Count,MatchSlope,MatchAspect : int64) : boolean;
       var
          SlpMatch,AspMatch : boolean;
       begin
          if Complex then begin
             AspMatch := (SlopeAspectRec.AspectDir >= MinAspectDir) or (SlopeAspectRec.AspectDir <= MaxAspectDir);
          end
          else begin
             AspMatch := (SlopeAspectRec.AspectDir >= MinAspectDir) and (SlopeAspectRec.AspectDir <= MaxAspectDir);
          end;
          SlpMatch := (SlopeAspectRec.Slope >= MinSlope) and (SlopeAspectRec.Slope <= MaxSlope);
          Result := SlpMatch and AspMatch;
          if SlpMatch then inc(MatchSlope);
          if AspMatch then inc(MatchAspect);
          if Result then inc(Count);
       end;

    begin
       {$IfDef RecordTerrCatOverlays} WriteLineToDebugFile('TMapDraw.CreateCategoryOverlay: ' +  DEMGlb[DEMonMap].TerrainCategoryLabel(TerrainCategory)); {$EndIf}
       with MapOwner do try
          CloneImagetoBitmap(MapOwner.Image1,Result);
          CheckEditString(Edit10.Text,MDDef.FPMinAsp1);
          CheckEditString(Edit11.Text,MDDef.FPMaxAsp1);
          CheckEditString(Edit12.Text,MDDef.FPMinAsp2);
          CheckEditString(Edit13.Text,MDDef.FPMaxAsp2);
          CheckEditString(Edit14.Text,MDDef.FPMinSlope1);
          CheckEditString(Edit15.Text,MDDef.FPMinSlope2);
          CheckEditString(Edit16.Text,MDDef.FPMaxSlope1);
          CheckEditString(Edit17.Text,MDDef.FPMaxSlope2);

          MinSlope := TanDeg(MDDef.FPMinSlope1);
          MinSlope2 := TanDeg(MDDef.FPMinSlope2);
          MaxSlope := TanDeg(MDDef.FPMaxSlope1);
          MaxSlope2 := TanDeg(MDDef.FPMaxSlope2);
          Complex := MDDef.FPMinAsp1 > MDDef.FPMaxAsp1;
          Complex2 := MDDef.FPMinAsp2 > MDDef.FPMaxAsp2;

          FP1Count := 0;
          FP2Count := 0;
          TotCount := 0;

          FP1MatchSlope:= 0;
          FP1MatchAspect:= 0;
          FP2MatchSlope := 0;
          FP2MatchAspect:= 0;

          StartProgress('Terrain Category');
          for j := 0 to pred(Result.Height) do begin
             if (j mod 500 = 0) then UpDateProgressBar(j/MapDraw.MapYSize);
             p0 := Result.ScanLine[j];
             for i := 0 to pred(Result.Width) do begin
                MapDraw.ScreenToDEMGrid(i,j,XG1,YG1);
                if DEMGlb[MapDraw.DEMonMap].GetSlopeAndAspect(round(xg1),round(yg1),SlopeAspectRec) then begin
                   inc(TotCount);
                   if GoodPoint(Complex, MDDef.FPMinAsp1,MDDef.FPMaxAsp1,MinSlope,MaxSlope,FP1Count,FP1MatchSlope,FP1MatchAspect) then begin
                      p0[i] := MDDef.ColorFP1;
                   end;
                   if GoodPoint(Complex2,MDDef.FPMinAsp2,MDDef.FPMaxAsp2,MinSlope2,MaxSlope2,FP2Count,FP2MatchSlope,FP2MatchAspect) then begin
                      p0[i] := MDDef.ColorFP2;
                   end;
                end;
             end;
          end {for i};
          Memo1.Lines.Clear;
          Memo1.Lines.Add('FP1: ' + RealToString(100 * FP1Count / TotCount,6,2) + '%');
          Memo1.Lines.Add('   matched slopes: ' + RealToString(100 * FP1MatchSlope / TotCount,6,2) + '%');
          Memo1.Lines.Add('   matched aspect: ' + RealToString(100 * FP1MatchAspect / TotCount,6,2) + '%');
          Memo1.Lines.Add('FP2: ' + RealToString(100 * FP2Count / TotCount,6,2) + '%');
          Memo1.Lines.Add('   matched slopes: ' + RealToString(100 * FP2MatchSlope / TotCount,6,2) + '%');
          Memo1.Lines.Add('   matched aspect: ' + RealToString(100 * FP2MatchAspect / TotCount,6,2) + '%');
       finally
          if ShowSatProgress then EndProgress;
       end;
    end;


var
   Bitmap,cbmp : tMyBitmap;
begin
   {$IfDef RecordTerrCats} WriteLineToDebugFile('BitBtn2Click entered'); {$EndIf}
   MapOwner.InsureGrayScaleReflectanceMap;
   MapOwner.DoFastMapRedraw;

   if CheckBox17.Checked then begin
       Bitmap := AspectOverlay;
       if MDDef.IHSTerrainCategory then MapOwner.IHSmergeOntoMap(Bitmap)
       else begin
          MapOwner.Image1.Canvas.CopyMode := cmSrcAnd;
          MapOwner.Image1.Canvas.Draw(0,0,Bitmap);
          Bitmap.Free;
       end;

       CheckEditString(Edit14.Text,MinSlope);
       CheckEditString(Edit15.Text,MinSlope2);
       CheckEditString(Edit16.Text,MaxSlope);
       CheckEditString(Edit17.Text,MaxSlope2);

       if MDDef.TerrainCatLegend.DrawItem then begin
          PetImage.CopyImageToBitmap(MapOwner.Image1,cbmp);
          CreateBitmap(Bitmap,300,45);
          Bitmap.Canvas.Brush.Style := bsSolid;
          Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.ColorFP1);
          Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.ColorFP1);
          Bitmap.Canvas.Rectangle(5,5,25,20);
          Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.ColorFP2);
          Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.ColorFP2);
          Bitmap.Canvas.Rectangle(5,25,25,40);
          Bitmap.Canvas.Brush.Style := bsClear;
          Bitmap.Canvas.Font.Size := 12;
          Bitmap.Canvas.Font.Style := [fsBold];
          Bitmap.Canvas.TextOut(30,5,'Dip dir: ' + RealToString(MDDef.FPMinAsp1,-8,0) + '--' + RealToString(MDDef.FPMaxAsp1,-8,0) + '  Dip: ' + RealToString(MinSlope,-8,0) + '--' + RealToString(MaxSlope,-8,0));
          Bitmap.Canvas.TextOut(30,25,'Dip dir: ' + RealToString(MDDef.FPMinAsp2,-8,0) + '--' + RealToString(MDDef.FPMaxAsp2,-8,0) + '  Dip: ' + RealToString(MinSlope2,-8,0)+ '--' + RealToString(MaxSlope2,-8,0));
          PetImage.GetImagePartOfBitmap(Bitmap);
          MapOwner.MapDraw.PositionBitmap(cbmp,Bitmap,lpNEMap);
          MapOwner.Image1.Picture.Graphic := cbmp;
          cbmp.Free;
       end;
   end
   else begin
     DefineTerrainCategory;

     if (CatDoing = tcElevOnly) then begin
        MapOwner.HighZ := TerrainCategory.CatMaxElev;
        MapOwner.LowZ := TerrainCategory.CatMinElev;
        MapOwner.PlotExtremeZValues(Memo1);
     end;

     if (CatDoing = tcSlopeOnly) then begin
        PetImage.CopyImageToBitmap(MapOwner.Image1,cbmp);
        MapOwner.FindSlopePoints(Memo1,TerrainCategory.CatMinSlope,CheckBox10.Checked);
        CreateBitmap(Bitmap,300,45);
        Bitmap.Canvas.Brush.Style := bsSolid;
        Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MapOwner.DrSymbol.Color);
        Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MapOwner.DrSymbol.Color);
        Bitmap.Canvas.Rectangle(5,5,25,20);
        Bitmap.Canvas.Brush.Style := bsClear;
        Bitmap.Canvas.Font.Size := 12;
        Bitmap.Canvas.Font.Style := [fsBold];
        Bitmap.Canvas.TextOut(30,5,'Slope >  ' + RealToString(TerrainCategory.CatMinSlope,-8,0) + DegSym);
        PetImage.GetImagePartOfBitmap(Bitmap);
        MapOwner.MapDraw.PositionBitmap(cbmp,Bitmap,lpNEMap);
        MapOwner.Image1.Picture.Graphic := cbmp;
        cbmp.Free;
     end;

     if (CatDoing = tcNormal) then begin
        CopyImageToBitmap(MapOwner.Image1,Bitmap);
        MapOwner.MapDraw.OverlayCategories(BitMap,TerrainCategory);
        if MDDef.IHSTerrainCategory then MapOwner.IHSmergeOntoMap(Bitmap,MDDef.IHSTerrainCategory)
        else begin
           MapOwner.Image1.Picture.Graphic := Bitmap;
           Bitmap.Free;
        end;
     end;
   end;
   MapOwner.CheckThatLegendsAreOnTop;
   {$IfDef RecordTerrCats} WriteLineToDebugFile('BitBtn2Click out'); {$EndIf}
{$EndIf}
end;


procedure TGetTerrC.BitBtn3Click(Sender: TObject);
begin
   PickSymbol(BitBtn3,MapOwner.DrSymbol,'Points');
end;

procedure TGetTerrC.BitBtn4Click(Sender: TObject);
begin
   DefineTerrainCategory;
   MapOwner.MapDraw.MapOverlays.ovTerrainCat.Add(DEMDef_routines.TerrainCategoryToString(TerrainCategory));
end;

procedure TGetTerrC.BitBtn5Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      QueryColor(BitBtn5,MDDef.ColorFP1);
   {$EndIf}
end;


procedure TGetTerrC.BitBtn6Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      QueryColor(BitBtn6,MDDef.ColorFP2);
   {$EndIf}
end;


procedure TGetTerrC.BitBtn7Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   NewHeadRecs : tDEMheader;
   NewDEM,xg1,yg1      : integer;
   NewValue    : byte;
begin
   if (Sender = BitBtn7) then begin
      NewHeadRecs := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader;
      NewHeadRecs.DEMPrecision := byteDEM;
      NewHeadRecs.ElevUnits := euIntCode;
      NewValue := 1;
      ReadDefault('New grid value',NewValue);
      if not OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,'',InitDEMmissing) then exit;
   end;

   DefineTerrainCategory;
   StartProgress('Terrain Category');

   for xg1 := 0 to pred(DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.NumCol) do begin
      if (xg1 mod 100 = 0) then UpDateProgressBar(xg1/DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.NumCol);
      for yg1 := 0 to pred(DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.NumRow) do begin
         if DEMGlb[MapOwner.MapDraw.DEMonMap].InTerrainCategory(xg1,yg1,TerrainCategory) then begin
            if (Sender = BitBtn7) then DEMGlb[NewDEM].SetGridElevation(xg1,yg1,NewValue);
            if (Sender = BitBtn9) then DEMGlb[MapOwner.MapDraw.DEMonMap].SetGridMissing(xg1,yg1);
         end
         else begin
            if (Sender = BitBtn8) then DEMGlb[MapOwner.MapDraw.DEMonMap].SetGridMissing(xg1,yg1);
         end;
      end;
   end {for xg1};
   if (Sender = BitBtn7) then  begin
      DEMGlb[NewDEM].AreaName := 'Class mask ' + DEMGlb[MapOwner.MapDraw.DEMonMap].AreaName;
      DEMGlb[NewDEM].SetUpMap(NewDEM,false);
   end
   else begin
      DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.RespondToChangedDEM;
   end;
   EndProgress;
{$EndIf}
end;


procedure TGetTerrC.BitBtn8Click(Sender: TObject);
begin
    BitBtn7Click(Sender);
end;

procedure TGetTerrC.BitBtn9Click(Sender: TObject);
begin
   BitBtn7Click(Sender);
end;

procedure TGetTerrC.CancelBtnClick(Sender: TObject);
begin
   {$IfDef RecordTerrCats} WriteLineToDebugFile('TGetTerrC.CancelBtnClick'); {$EndIf}
   MapOwner.MapDraw.MapOverlays.ovTerrainCat.Clear;
   MapOwner.DoFastMapRedraw;
   Close;
end;


procedure TGetTerrC.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   CheckBox18.Checked := MDDef.TerrainCatLegend.DrawItem;
   UserChanging := false;
end;


procedure TGetTerrC.DefineTerrainCategory;
begin
   with TerrainCategory do begin
      UseElevation := CheckBox13.Checked;
      UseRelief := CheckBox14.Checked;
      UseSlope := CheckBox15.Checked;
      UseAspect := CheckBox16.Checked;
      CheckEditString(Edit1.Text,CatMinElev);
      CheckEditString(Edit2.Text,CatMaxElev);
      CheckEditString(Edit3.Text,CatMinSlope);
      CheckEditString(Edit4.Text,CatMaxSlope);
      CheckEditString(Edit5.Text,CatMinRelief);
      CheckEditString(Edit6.Text,CatMaxRelief);
      CheckEditString(Edit7.Text,CatReliefRadius);
      CatAspects := [];
      if CheckBox1.Checked then include(CatAspects,cdN);
      if CheckBox2.Checked then include(CatAspects,cdNE);
      if CheckBox3.Checked then include(CatAspects,cdE);
      if CheckBox4.Checked then include(CatAspects,cdSE);
      if CheckBox5.Checked then include(CatAspects,cdS);
      if CheckBox6.Checked then include(CatAspects,cdSW);
      if CheckBox7.Checked then include(CatAspects,cdW);
      if CheckBox8.Checked then include(CatAspects,cdNW);
      if CheckBox11.Checked then include(CatAspects,cdPit);
      if CheckBox12.Checked then include(CatAspects,cdFlat);
   end;
end;


initialization
finalization
   {$IfDef RecordTerrCats} WriteLineToDebugFile('RecordTerrCats active in demterrc'); {$EndIf}
end.
