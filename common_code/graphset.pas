unit Graphset;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 4/3/2016        }
{_________________________________}

{$I nevadia_defines.inc}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,  StdCtrls, ExtCtrls,
   PETMAR, BaseGraf;

type
  TGraphSettingsForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    XMinLabel: TLabel;
    XMaxLabel: TLabel;
    YMinLabel: TLabel;
    YMaxLabel: TLabel;
    XMinEdit: TEdit;
    XMaxEdit: TEdit;
    YMinEdit: TEdit;
    YMaxEdit: TEdit;
    XLabelEdit: TEdit;
    YLabelEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn1: TBitBtn;
    Day: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    CheckBox1: TCheckBox;
    Edit5: TEdit;
    Edit6: TEdit;
    Colors: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    CheckBox2: TCheckBox;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BitBtn2: TBitBtn;
    ComboBox3: TComboBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    Label8: TLabel;
    Edit10: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     OwningGraph        : TThisBaseGraph;
     procedure CheckSettings;
  end;

implementation

{$R *.DFM}

uses
   Petmath;

procedure TGraphSettingsForm.CheckSettings;
var
   i : integer;
begin
   with OwningGraph, GraphDraw do begin
      AnnualCycle := CheckBox1.Checked;
      NormalCartesianX := not CheckBox5.Checked;
      NormalCartesianY := not CheckBox2.Checked;
      Draw1to1Line := CheckBox3.Checked;
      CorrectScaling := CheckBox6.Checked;
      if ComboBox3.Text <> '' then DBFXFieldName := ComboBox3.Text;

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
       LLcornerText := Edit10.Text;
       CheckEditString(YMinEdit.Text,MinVertAxis);
       CheckEditString(YMaxEdit.Text,MaxVertAxis);
       if MaxVertAxis < MinVertAxis then SwapPair32(MinVertAxis,MaxVertAxis);
       if MaxHorizAxis < MinHorizAxis then SwapPair32(MinHorizAxis,MaxHorizAxis);
       CheckEditString(Edit5.Text,MinZ);
       CheckEditString(Edit6.Text,MaxZ);

       CheckEditString(Edit7.Text,LeftMargin);
       CheckEditString(Edit8.Text,TopMargin);
       CheckEditString(Edit9.Text,BottomMargin);
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
