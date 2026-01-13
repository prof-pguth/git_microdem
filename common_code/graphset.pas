unit Graphset;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordGrafSize}
   {$ELSE}

   {$ENDIF}
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,  StdCtrls, ExtCtrls,
   System.SysUtils,VCL.Menus,
   PETMAR, BaseGraf, Vcl.ComCtrls;

type
  TGraphSettingsForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn2: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox6: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    Label8: TLabel;
    Edit10: TEdit;
    GroupBox1: TGroupBox;
    Edit11: TEdit;
    Edit12: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox7: TCheckBox;
    Edit1: TEdit;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    CheckBox4: TCheckBox;
    XMinLabel: TLabel;
    XMinEdit: TEdit;
    XMaxEdit: TEdit;
    XMaxLabel: TLabel;
    XLabelEdit: TEdit;
    TabSheet2: TTabSheet;
    YLabelEdit: TEdit;
    YMaxLabel: TLabel;
    YMaxEdit: TEdit;
    YMinEdit: TEdit;
    YMinLabel: TLabel;
    CheckBox2: TCheckBox;
    CheckBox5: TCheckBox;
    TabSheet3: TTabSheet;
    Edit13: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    ComboBox2: TComboBox;
    ComboBox1: TComboBox;
    ComboBox3: TComboBox;
    CheckBox8: TCheckBox;
    TabSheet4: TTabSheet;
    Edit6: TEdit;
    Label4: TLabel;
    Edit5: TEdit;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    ScrollBar1: TScrollBar;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    CheckBox9: TCheckBox;
    HistogramTabSheet: TTabSheet;
    Edit16: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Edit17: TEdit;
    Edit18: TEdit;
    Label15: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure XMaxEditChange(Sender: TObject);
    procedure YMaxEditChange(Sender: TObject);
    procedure XMinEditChange(Sender: TObject);
    procedure Edit16Change(Sender: TObject);
    procedure Edit17Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure Edit18Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     OwningGraph : TThisBaseGraph;
     procedure CheckSettings;
     procedure InitializeSettings;
  end;

implementation

{$R *.DFM}

uses
   Nevadia_Main,
   Petmath,Petmar_types,Petmar_db,Petdbutils,DEMdefs,DEMstat;


procedure TGraphSettingsForm.InitializeSettings;
var
   i : integer;
   FieldsInDB : tStringList;
   MyTable : tMyData;
   fName : PathStr;
   VisCols : Array100Boolean;
begin
    with OwningGraph,GraphDraw do begin
         MarginsGood := true;
         Edit13.Text := OwningGraph.GraphDraw.VertLabel2;
         ComboBox1.ItemIndex := ord(GraphAxes);
         ComboBox2.Visible := VertAxisFunctionType in [ShortCumNormalAxis,CumulativeNormalAxis,LongCumulativeNormalAxis,LongerCumulativeNormalAxis];

         CheckBox1.Visible := GraphAxes in [XTimeYFullGrid,XTimeYPartGrid];
         CheckBox1.Checked := AnnualCycle;
         CheckBox2.Checked := not NormalCartesianY;
         CheckBox3.Checked := Draw1to1Line;
         CheckBox5.Checked := not NormalCartesianX;
         CheckBox6.Checked := CorrectScaling;

         CheckBox8.Checked := OwningGraph.GraphDraw.ShowGraphLeftLabels;

         Edit7.Text := IntToStr(LeftMargin);
         Edit8.Text := IntToStr(TopMargin);
         Edit9.Text := IntToStr(BottomMargin);
         Edit10.Text := LLcornerText;
         Edit11.Text := IntToStr(Width);
         Edit12.Text := IntToStr(Height);
         Edit16.Text := RealToString(OwningGraph.HistogramBinSize,-12,-3);
         Edit17.Text := RealToString(OwningGraph.HistogramNumBins,-12,-3);
         Edit18.Text := IntToStr(MdDef.GraphTickSize);
         XLabelEdit.Text := HorizLabel;
         YLabelEdit.Text := VertLabel;
         YMinEdit.Text := RealToString(OwningGraph.GraphDraw.MinVertAxis,-18,-6);
         YMaxEdit.Text := RealToString(OwningGraph.GraphDraw.MaxVertAxis,-18,-6);


         if (DBFLineFilesPlotted <> Nil) and (DBFLineFilesPlotted.Count > 0) then begin
            ComboBox3.Visible := true;
            fName := DBFLineFilesPlotted.Strings[0];
            MyTable := tMyData.Create(fName);
            PetdbUtils.GetFields(MyTable,VisCols,NumericFieldTypes,FieldsInDB,true);
            for i := 0 to pred(FieldsInDB.Count) do ComboBox3.Items.Add(FieldsInDB.Strings[i]);
            ComboBox3.Text := '';
            MyTable.Destroy;
         end;
         if (XYZFilesPlotted <> Nil) and (XYZFilesPlotted.Count > 0) then begin
            Label1.Visible := true;
            Label4.Visible := true;
            Edit5.Visible := true;
            Edit6.Visible := true;
            Edit5.Text := RealToString(MinZ,-12,-6);
            Edit6.Text := RealToString(MaxZ,-12,-6);
         end;
         if ComboBox2.Visible then begin
            case VertAxisFunctionType of
               ShortCumNormalAxis : ComboBox2.ItemIndex := 0;
               CumulativeNormalAxis: ComboBox2.ItemIndex := 1;
               LongCumulativeNormalAxis : ComboBox2.ItemIndex := 2;
               LongerCumulativeNormalAxis : ComboBox2.ItemIndex := 3;
            end;
         end;
         if GraphDraw.GraphType in [gtTwoVertAxes] then begin
            YMaxLabel.Visible := false;
            YMinLabel.Visible := false;
            Edit14.Text := RealToString(OwningGraph.GraphDraw.MinVertAxis2,-18,-6);
            Edit15.Text := RealToString(OwningGraph.GraphDraw.MaxVertAxis2,-18,-6);
         end
         else begin
            TabSheet3.TabVisible := false;
         end;
         HistogramTabSheet.Visible := GraphDraw.GraphType in [gtMultHist];

         if OwningGraph.GraphDraw.InsideMarginLegend = lpNone then RadioGroup1.ItemIndex := 0;
         if OwningGraph.GraphDraw.InsideMarginLegend = lpNWMap then RadioGroup1.ItemIndex := 1;
         if OwningGraph.GraphDraw.InsideMarginLegend = lpNEMap then RadioGroup1.ItemIndex := 2;
         if OwningGraph.GraphDraw.InsideMarginLegend = lpSWMap then RadioGroup1.ItemIndex := 3;
         if OwningGraph.GraphDraw.InsideMarginLegend = lpSEMap then RadioGroup1.ItemIndex := 4;
    end;


   if OwningGraph.GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
      XMinLabel.Caption := 'Ending';
      XMaxLabel.Caption := 'Starting';
      XLabelEdit.Visible := false;
      XMinEdit.Text := IntToStr(OwningGraph.GraphDraw.Day2);
      XMaxEdit.Text := IntToStr(OwningGraph.GraphDraw.Day1);
      Label2.Visible := true;
      Label3.Visible := true;
      Edit1.Text := IntToStr(OwningGraph.GraphDraw.Month1);
      Edit2.Text := IntToStr(OwningGraph.GraphDraw.Month2);
      Edit3.Text := IntToStr(OwningGraph.GraphDraw.Year1);
      Edit4.Text := IntToStr(OwningGraph.GraphDraw.Year2);
      Edit1.Visible := true;
      Edit2.Visible := true;
      Edit3.Visible := true;
      Edit4.Visible := true;
   end
   else begin
      Edit1.Visible := false;
      Edit2.Visible := false;
      Edit3.Visible := false;
      Edit4.Visible := false;
      Label2.Visible := false;
      Label3.Visible := false;
      XMinLabel.Caption := 'Min x';
      XMaxLabel.Caption := 'Max x';
      XLabelEdit.Visible := true;
      XMinEdit.Text := RealToString(OwningGraph.GraphDraw.MinHorizAxis,-18,-6);
      XMaxEdit.Text := RealToString(OwningGraph.GraphDraw.MaxHorizAxis,-18,-6);
   end;
end;

procedure TGraphSettingsForm.CheckBox7Click(Sender: TObject);
begin
   OwningGraph.ScrollGraph := CheckBox7.Checked;
   OwningGraph.ScrollBox1.AutoScroll := CheckBox7.Checked;
end;

procedure TGraphSettingsForm.CheckBox8Click(Sender: TObject);
begin
   OwningGraph.GraphDraw.ShowGraphLeftLabels := CheckBox8.Checked;
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.CheckBox9Click(Sender: TObject);
begin
   XMinEdit.Enabled := not CheckBox9.Checked;
   XMinLabel.Enabled := not CheckBox9.Checked;
   if CheckBox9.Checked then begin
      XMinEdit.Text := '-' + XMaxEdit.Text;
   end;
end;


procedure TGraphSettingsForm.CheckSettings;
var
   i : integer;
begin
   {$If Defined(RecordGrafSize)} WriteLineToDebugFile('TGraphSettingsForm.CheckSetting in Client size, ' + IntToStr(OwningGraph.ClientWidth) + 'x' + IntToStr(OwningGraph.ClientHeight) + ' ' +  OwningGraph.GraphDraw.AxisRange); {$EndIf}
   with OwningGraph, GraphDraw do begin
      AnnualCycle := CheckBox1.Checked;
      NormalCartesianX := not CheckBox5.Checked;
      NormalCartesianY := not CheckBox2.Checked;
      Draw1to1Line := CheckBox3.Checked;
      CorrectScaling := CheckBox6.Checked;
      if (ComboBox3.Text <> '') then DBFXFieldName := ComboBox3.Text;

      if ComboBox2.Visible then begin
         for i := 0 to pred(ComboBox2.Items.Count) do
            if ComboBox2.Items.Strings[i] = ComboBox2.Text then
               case i of
                  0 : VertAxisFunctionType := ShortCumNormalAxis;
                  1 : VertAxisFunctionType := CumulativeNormalAxis;
                  2 : VertAxisFunctionType := LongCumulativeNormalAxis;
                  3 : VertAxisFunctionType := LongerCumulativeNormalAxis;
               end;
      end;
      for i := 0 to pred(ComboBox1.Items.Count) do
         if ComboBox1.Items.Strings[i] = ComboBox1.Text then
            GraphAxes := tGraphAxes(i);

      if GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
         MinHorizAxis := JulDay(Month1,Day1,Year1);
         MaxHorizAxis := JulDay(Month2,Day2,Year2);
      end
      else begin
          HorizLabel := XLabelEdit.Text;
          CheckEditString(XMinEdit.Text,OwningGraph.GraphDraw.MinHorizAxis);
          CheckEditString(XMaxEdit.Text,OwningGraph.GraphDraw.MaxHorizAxis);
       end;
       VertLabel := YLabelEdit.Text;
       VertLabel2 := Edit13.Text;

       LLcornerText := Edit10.Text;
       CheckEditString(YMinEdit.Text,MinVertAxis);
       CheckEditString(YMaxEdit.Text,MaxVertAxis);
       CheckEditString(Edit14.Text,MinVertAxis2);
       CheckEditString(Edit15.Text,MaxVertAxis2);

       if MaxVertAxis < MinVertAxis then SwapPair32(MinVertAxis,MaxVertAxis);
       if MaxVertAxis2 < MinVertAxis2 then SwapPair32(MinVertAxis2,MaxVertAxis2);
       if MaxHorizAxis < MinHorizAxis then SwapPair32(MinHorizAxis,MaxHorizAxis);
       CheckEditString(Edit5.Text,MinZ);
       CheckEditString(Edit6.Text,MaxZ);

       //CheckEditString(Edit7.Text,OwningGraph.GraphDraw.LeftMargin);
       CheckEditString(Edit8.Text,OwningGraph.GraphDraw.TopMargin);
       //CheckEditString(Edit9.Text,OwningGraph.GraphDraw.BottomMargin);
       CheckEditString(Edit11.Text,OwningGraph.GraphDraw.XWindowSize);
       CheckEditString(Edit12.Text,OwningGraph.GraphDraw.YWindowSize);
       {$If Defined(RecordGrafSize)} WriteLineToDebugFile('TGraphSettingsForm.CheckSetting window, ' + IntToStr(XWindowSize) + 'x' + IntToStr(YWindowSize)); {$EndIf}
       OwningGraph.ClientWidth := OwningGraph.GraphDraw.XWindowSize;
       OwningGraph.ClientHeight := OwningGraph.GraphDraw.YWindowSize;
       {$If Defined(RecordGrafSize)} WriteLineToDebugFile('TGraphSettingsForm.CheckSetting out Client size, ' + IntToStr(OwningGraph.ClientWidth) + 'x' + IntToStr(OwningGraph.ClientHeight) + ' ' +  OwningGraph.GraphDraw.AxisRange); {$EndIf}
    end;
end;


procedure TGraphSettingsForm.BitBtn1Click(Sender: TObject);
begin
   ReadDefault('Horiz label interval', OwningGraph.GraphDraw.ForceHorizCycleSize);
   ReadDefault('Horiz tick interval', OwningGraph.GraphDraw.ForceHorizTickIncr);
   ReadDefault('Vert label interval', OwningGraph.GraphDraw.ForceVertCycleSize);
   ReadDefault('Vert tick interval', OwningGraph.GraphDraw.ForceVertTickIncr);
end;

procedure TGraphSettingsForm.BitBtn2Click(Sender: TObject);
begin
   if (OwningGraph <> Nil) then OwningGraph.Font1Click(Sender);
end;


procedure TGraphSettingsForm.BitBtn3Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to 15 do begin
      OwningGraph.GraphDraw.Symbol[i].Size := OwningGraph.GraphDraw.Symbol[i].Size + 1;
   end;
   inc(MDDef.DemixSymSize);
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.BitBtn4Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to 15 do begin
      if (OwningGraph.GraphDraw.Symbol[i].Size) > 0 then OwningGraph.GraphDraw.Symbol[i].Size := OwningGraph.GraphDraw.Symbol[i].Size - 1;
   end;
   dec(MDDef.DemixSymSize);
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.BitBtn5Click(Sender: TObject);
begin
   MDDef.DefMarginLegend := OwningGraph.GraphDraw.InsideMarginLegend;
end;

procedure TGraphSettingsForm.BitBtn6Click(Sender: TObject);
begin
   QueryTColor(OwningGraph.GraphDraw.GraphBackgroundColor);
   OwningGraph.RedrawDiagram11Click(Nil);
end;

procedure TGraphSettingsForm.BitBtn7Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to 255 do begin
      OwningGraph.GraphDraw.LineSize256[i] := OwningGraph.GraphDraw.LineSize256[i] + 1;
   end;
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.BitBtn8Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to 255 do begin
      if (OwningGraph.GraphDraw.LineSize256[i] > 0) then OwningGraph.GraphDraw.LineSize256[i] := OwningGraph.GraphDraw.LineSize256[i] - 1;
   end;
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.FormCreate(Sender: TObject);
begin
   OwningGraph := Nil;
   PlaceFormAtMousePosition(Self);
end;

procedure TGraphSettingsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\graph_options.htm');
end;


procedure TGraphSettingsForm.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
       0 : OwningGraph.GraphDraw.InsideMarginLegend := lpNone;
       1 : OwningGraph.GraphDraw.InsideMarginLegend := lpNWMap;
       2 : OwningGraph.GraphDraw.InsideMarginLegend := lpNEMap;
       3 : OwningGraph.GraphDraw.InsideMarginLegend := lpSWMap;
       4 : OwningGraph.GraphDraw.InsideMarginLegend := lpSEMap;
   end;
   OwningGraph.RedrawDiagram11Click(Nil);
end;


procedure TGraphSettingsForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   CheckSettings;
   if OwningGraph.HistogramChanged and (OwningGraph.GraphDraw.HistogramDistributionFileList <> Nil) then begin
      OwningGraph.HistogramChanged := false;
      MultipleHistogramPrep(OwningGraph);
   end;
   OwningGraph.RedrawDiagram11Click(Nil);
end;

procedure TGraphSettingsForm.XMaxEditChange(Sender: TObject);
begin
   if CheckBox9.Checked then begin
      XMinEdit.Text := '-' + XMaxEdit.Text;
   end;
   OwningGraph.HistogramChanged := true;
end;

procedure TGraphSettingsForm.XMinEditChange(Sender: TObject);
begin
   OwningGraph.HistogramChanged := true;
end;

procedure TGraphSettingsForm.YMaxEditChange(Sender: TObject);
begin
   OwningGraph.HistogramChanged := true;
end;

procedure TGraphSettingsForm.ComboBox3Change(Sender: TObject);
begin
   XLabelEdit.Text := ComboBox3.Text;
end;



procedure TGraphSettingsForm.Edit16Change(Sender: TObject);
begin
   if Sender = Edit16 then begin
      CheckEditString(Edit16.Text,OwningGraph.HistogramBinSize);
      if (OwningGraph.HistogramBinSize > 0) then begin
         OwningGraph.HistogramNumBins := round((OwningGraph.GraphDraw.MaxHorizAxis - OwningGraph.GraphDraw.MinHorizAxis) / OwningGraph.HistogramBinSize);
         Edit17.Text := IntToStr(OwningGraph.HistogramNumBins);
         OwningGraph.HistogramChanged := true;
      end;
   end;
end;

procedure TGraphSettingsForm.Edit17Change(Sender: TObject);
begin
   if Sender = Edit17 then begin
      CheckEditString(Edit17.Text,OwningGraph.HistogramNumBins);
      if OwningGraph.HistogramNumBins > 0 then begin
         OwningGraph.HistogramBinSize := round((OwningGraph.GraphDraw.MaxHorizAxis - OwningGraph.GraphDraw.MinHorizAxis) / OwningGraph.HistogramNumBins);
         OwningGraph.HistogramChanged := true;
      end;
   end;
end;



procedure TGraphSettingsForm.Edit18Change(Sender: TObject);
begin
   CheckEditString(Edit18.Text,MdDef.GraphTickSize);
end;

procedure TGraphSettingsForm.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,OwningGraph.GraphDraw.LeftMargin);
end;

procedure TGraphSettingsForm.Edit9Change(Sender: TObject);
begin
   CheckEditString(Edit9.Text,OwningGraph.GraphDraw.BottomMargin);
end;

end.



