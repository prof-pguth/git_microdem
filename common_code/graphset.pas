unit Graphset;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 4/3/2016        }
{_________________________________}

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
    Edit5: TEdit;
    Edit6: TEdit;
    Colors: TLabel;
    Label1: TLabel;
    Label4: TLabel;
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
   Petmath,Petmar_types,Petmar_db,Petdbutils;


procedure TGraphSettingsForm.InitializeSettings;
var
   i : integer;
   FieldsInDB : tStringList;
   MyTable : tMyData;
   fName : PathStr;
   VisCols : Array100Boolean;
begin
    with OwningGraph,GraphDraw do begin
         Edit11.Text := IntToStr(Width);
         Edit12.Text := IntToStr(Height);
         XLabelEdit.Text := HorizLabel;
         YLabelEdit.Text := VertLabel;
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

         if (DBFLineFilesPlotted.Count > 0) then begin
            ComboBox3.Visible := true;
            fName := DBFLineFilesPlotted.Strings[0];
            MyTable := tMyData.Create(fName);
            PetdbUtils.GetFields(MyTable,VisCols,NumericFieldTypes,FieldsInDB,true);
            for i := 0 to pred(FieldsInDB.Count) do ComboBox3.Items.Add(FieldsInDB.Strings[i]);
            ComboBox3.Text := '';
            MyTable.Destroy;
         end;
         if (XYZFilesPlotted.Count > 0) then begin
            Colors.Visible := true;
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
           // YLabelEdit.Visible := false;
            //YMaxEdit.Visible := false;
            //YMinEdit.Visible := false;
            YMaxLabel.Visible := false;
            YMinLabel.Visible := false;
            Edit14.Text := RealToString(OwningGraph.GraphDraw.MinVertAxis2,-18,-6);
            Edit15.Text := RealToString(OwningGraph.GraphDraw.MaxVertAxis2,-18,-6);
         end
         else begin
            TabSheet3.TabVisible := false;
         end;
    end;

    YMinEdit.Text := RealToString(OwningGraph.GraphDraw.MinVertAxis,-18,-6);
    YMaxEdit.Text := RealToString(OwningGraph.GraphDraw.MaxVertAxis,-18,-6);


         if OwningGraph.GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
            XMinLabel.Caption := 'Ending';
            XMaxLabel.Caption := 'Starting';
            XLabelEdit.Visible := false;
            //Day.Visible := true;
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
           // Day.Visible := false;
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
            GraphAxes := AxesType(i);

      if GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
         MinHorizAxis := JulDay(Month1,Day1,Year1);
         MaxHorizAxis := JulDay(Month2,Day2,Year2);
      end
      else begin
          HorizLabel := XLabelEdit.Text;
          CheckEditString(XMinEdit.Text,MinHorizAxis);
          CheckEditString(XMaxEdit.Text,MaxHorizAxis);
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

       CheckEditString(Edit7.Text,LeftMargin);
       CheckEditString(Edit8.Text,TopMargin);
       CheckEditString(Edit9.Text,BottomMargin);
       CheckEditString(Edit11.Text,GraphDraw.XWindowSize);
       CheckEditString(Edit12.Text,GraphDraw.YWindowSize);
       {$If Defined(RecordGrafSize)} WriteLineToDebugFile('TGraphSettingsForm.CheckSetting window, ' + IntToStr(XWindowSize) + 'x' + IntToStr(YWindowSize)); {$EndIf}
       OwningGraph.ClientWidth := GraphDraw.XWindowSize;
       OwningGraph.ClientHeight := GraphDraw.YWindowSize;

       (*
       CheckEditString(Edit11.Text,i);
       OwningGraph.ClientWidth := i;  //XWindowSize);
       CheckEditString(Edit12.Text,i);
       OwningGraph.ClientHeight := i;  //YWindowSize);
       *)
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
   for i := 1 to 15 do begin
      OwningGraph.GraphDraw.Symbol[i].Size := OwningGraph.GraphDraw.Symbol[i].Size + 1;
   end;
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.BitBtn4Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to 15 do begin
      OwningGraph.GraphDraw.Symbol[i].Size := OwningGraph.GraphDraw.Symbol[i].Size - 1;
   end;
   RedrawSpeedButton12Click(Sender);
end;

procedure TGraphSettingsForm.FormCreate(Sender: TObject);
begin
   OwningGraph := Nil;
   Petmar.PlaceFormAtMousePosition(Self);
end;

procedure TGraphSettingsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\graph_options.htm');
end;

procedure TGraphSettingsForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   CheckSettings;
   OwningGraph.RedrawDiagram11Click(Nil);
end;

procedure TGraphSettingsForm.ComboBox3Change(Sender: TObject);
begin
   XLabelEdit.Text := ComboBox3.Text;
end;



end.
