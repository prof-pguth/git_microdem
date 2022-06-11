unit dem3band;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM                }
{ PETMAR Trilobite Breeding Ranch }
{   file verified  3/16/2013      }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordPickBandProblems}
{$EndIf}

interface

uses
   Vcl.ComCtrls,
   Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
   Buttons, ExtCtrls, OkCancl2,
   DEMMapf;

type
  TPickThreeBandForm = class(TOKRightDlg)
    HelpBtn: TButton;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ComboBox2: TComboBox;
    Label2: TLabel;
    ComboBox3: TComboBox;
    GroupBox2: TGroupBox;
    ComboBox4: TComboBox;
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    Quick: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    UpDown1: TUpDown;
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuickClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
  public
    { Public declarations }
     FormSetUp,
     Multiband : boolean;
     Sat : integer;
     BaseMap : tMapForm;
     procedure ToggleBands;
     procedure ColorPreview;
  end;


implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types,DEMEros,DEMDefs,
   Nevadia_Main;


procedure TPickThreeBandForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme9dpq.htm');
end;


procedure TPickThreeBandForm.QuickClick(Sender: TObject);
begin
   MDDef.QuickBandRedraw := Quick.Checked;
end;


procedure TPickThreeBandForm.BitBtn1Click(Sender: TObject);
begin
   if (BaseMap <> Nil)  then begin
      FormSetup := false;
      if (Sender = BitBtn1) then begin
         ComboBox1.Text := SatImage[BaseMap.MapDraw.SatOnMap].BandTitle[SatImage[BaseMap.MapDraw.SatOnMap].DefaultRedTrue];
         ComboBox2.Text := SatImage[BaseMap.MapDraw.SatOnMap].BandTitle[SatImage[BaseMap.MapDraw.SatOnMap].DefaultGreenTrue];
         ComboBox3.Text := SatImage[BaseMap.MapDraw.SatOnMap].BandTitle[SatImage[BaseMap.MapDraw.SatOnMap].DefaultBlueTrue];
      end
      else begin
         ComboBox1.Text := SatImage[BaseMap.MapDraw.SatOnMap].BandTitle[SatImage[BaseMap.MapDraw.SatOnMap].DefaultRedFalse];
         ComboBox2.Text := SatImage[BaseMap.MapDraw.SatOnMap].BandTitle[SatImage[BaseMap.MapDraw.SatOnMap].DefaultGreenFalse];
         ComboBox3.Text := SatImage[BaseMap.MapDraw.SatOnMap].BandTitle[SatImage[BaseMap.MapDraw.SatOnMap].DefaultBlueFalse];
      end;
      FormSetup := true;
      RadioGroup1.ItemIndex := 1;
      ColorPreview;
   end;
end;


procedure TPickThreeBandForm.BitBtn2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;


procedure TPickThreeBandForm.ColorPreview;
var
   i : integer;
   OldNumCol : integer;
begin
   if FormSetup then begin
      {$IfDef RecordPickBandProblems}
      WriteLineToDebugFile('TPickThreeBandForm.ColorPreview in');
      {$EndIf}
      if (BaseMap <> Nil) then begin
         with BaseMap,MapDraw do begin
            for i := 1 to SatImage[SatOnMap].AvailableBands do begin
               if ComboBox1.Text = SatImage[SatOnMap].BandTitle[i] then SatView.RedBand := i;
               if ComboBox2.Text = SatImage[SatOnMap].BandTitle[i] then SatView.GreenBand := i;
               if ComboBox3.Text = SatImage[SatOnMap].BandTitle[i] then SatView.BlueBand := i;
               if ComboBox4.Text = SatImage[SatOnMap].BandTitle[i] then begin
                  SatView.BandInWindow := i;
                  //Updown1.Position := 1;
               end;
            end;
            if Multiband then i := SatView.RedBand
            else i := SatView.BandInWindow;
            OldNumCol := SatImage[SatOnMap].NumSatCol;
            SatView.SatColorImage := Multiband;

            SatImage[SatOnMap].DefineImageFromTiff(i);
               if OldNumCol <> SatImage[SatOnMap].NumSatCol then begin
                  BaseMap.FullDEM1Click(Nil);
               end
               else if (MDDef.QuickBandRedraw) then begin
                  MapDraw.DeleteSingleMapLayer(MapDraw.BaseMapFName);
                  BaseMap.DoFastMapRedraw;
               end;
            SatImage[Sat].ShowPreview(Self.Image1,MultiBand,SatView.BandInWindow,SatView.RedBand,SatView.GreenBand,SatView.BlueBand);
         end;
      end;
      {$IfDef RecordPickBandProblems}
      WriteLineToDebugFile('TPickThreeBandForm.ColorPreview out');
      {$EndIf}
   end
   else begin
      SatImage[Sat].ShowPreview(Self.Image1,MultiBand,BaseMap.MapDraw.SatView.BandInWindow,BaseMap.MapDraw.SatView.RedBand,BaseMap.MapDraw.SatView.GreenBand,BaseMap.MapDraw.SatView.BlueBand);
   end;
end;


procedure TPickThreeBandForm.ToggleBands;
begin
   MultiBand := (RadioGroup1.ItemIndex = 1);
   GroupBox2.Enabled := Not MultiBand;
   GroupBox1.Enabled := MultiBand;
   ComboBox1.Enabled := MultiBand;
   ComboBox2.Enabled := MultiBand;
   ComboBox3.Enabled := MultiBand;
   ComboBox4.Enabled := Not MultiBand;
   UpDown1.Enabled := Not MultiBand;
end;


procedure TPickThreeBandForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   if (BaseMap <> Nil)  then begin
      {$IfDef RecordPickBandProblems}
      WriteLineToDebugFile('TPickThreeBandForm.UpDown1Click in  UpDown1.Position=' + IntToStr(UpDown1.Position) + '  UpDown1.Min=' + IntToStr(UpDown1.Min)+ '  UpDown1.Max=' + IntToStr(UpDown1.Max));
      {$EndIf}
      ComboBox4.Text := ComboBox4.Items[pred(UpDown1.Position)];
      ColorPreview;
   end;
end;

procedure TPickThreeBandForm.RadioGroup1Click(Sender: TObject);
begin
  ToggleBands;
  ColorPreview;
end;


procedure TPickThreeBandForm.ComboBox4Change(Sender: TObject);
begin
   ColorPreview;
end;

procedure TPickThreeBandForm.ComboBox1Change(Sender: TObject);
begin
   ColorPreview;
end;


procedure TPickThreeBandForm.ComboBox2Change(Sender: TObject);
begin
   ColorPreview;
end;


procedure TPickThreeBandForm.ComboBox3Change(Sender: TObject);
begin
   ColorPreview;
end;


procedure TPickThreeBandForm.FormCreate(Sender: TObject);
begin
   FormSetUp := false;
   BaseMap := Nil;
   Quick.Checked := MDDef.QuickBandRedraw;

   wmDEM.FormPlacementInCorner(self);
   {$IfDef RecordPickBandProblems}
   WriteLineToDebugFile('TPickThreeBandForm.FormCreate OK');
   {$EndIf}
end;



initialization
finalization
{$IfDef RecordPickBandProblems}
   WriteLineToDebugFile('RecordPickBandProblems active in DEM3band');
{$EndIf}
end.

