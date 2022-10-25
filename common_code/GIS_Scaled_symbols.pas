unit GIS_Scaled_symbols;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 3/23/2016       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   //{$Define RecordSym}
   //{$Define RecordQuickFilter}
   //{$Define RecordGISvectors}
   //{$Define RecordFormSetup}
   //{$Define RecordColorPalette}
   //{$Define RecordLayerSymbology}
   //{$Define RecordQuantile}
   //{$Define RecordDataBase}
   //{$Define RecordDataInsideLoopPlots}
{$EndIf}


interface

uses
//need for inline of core DB functions
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, Menus,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  System.UITypes,
  DEMDefs,BaseGraf,
  Petmar_types;

type
  Tgis_scaled_form = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    RadioGroup3: TRadioGroup;
    Panel4: TPanel;
    RadioGroup1 : TRadioGroup;
    BitBtn3: TBitBtn;
    Label4: TLabel;
    PlotScaledSymbolsButton: TBitBtn;
    BitBtn2: TBitBtn;
    Lengend: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn8: TBitBtn;
    Panel6: TPanel;
    BitBtn9: TBitBtn;
    ArrowCheckBox: TCheckBox;
    Edit4: TEdit;
    Label7: TLabel;
    Panel7: TPanel;
    FontButton: TBitBtn;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Label8: TLabel;
    Font2Button: TBitBtn;
    CheckBox4: TCheckBox;
    Panel8: TPanel;
    BitBtn11: TBitBtn;
    Label10: TLabel;
    Edit5: TEdit;
    Label11: TLabel;
    Edit6: TEdit;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    Label9: TLabel;
    Label12: TLabel;
    Panel10: TPanel;
    ComboBox10: TComboBox;
    ListBox2: TListBox;
    FontButton2: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn12: TBitBtn;
    CheckBox8: TCheckBox;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    Label15: TLabel;
    Edit7: TEdit;
    Panel11: TPanel;
    BitBtn16: TBitBtn;
    RadioGroup5: TRadioGroup;
    Label16: TLabel;
    Edit8: TEdit;
    Label17: TLabel;
    Panel13: TPanel;
    ComboBox14: TComboBox;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    BitBtn19: TBitBtn;
    Panel14Legend: TPanel;
    Image1: TImage;
    TrackBar1: TTrackBar;
    Panel15: TPanel;
    TrackBar2: TTrackBar;
    BitBtn15: TBitBtn;
    ListBox3: TListBox;
    RadioGroup6: TRadioGroup;
    Label23: TLabel;
    Edit16: TEdit;
    RadioGroup8: TRadioGroup;
    Panel23: TPanel;
    BitBtn21: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn25: TBitBtn;
    CheckBox5: TCheckBox;
    Edit17: TEdit;
    CheckBox11: TCheckBox;
    Label14: TLabel;
    Edit18: TEdit;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    Label24: TLabel;
    Label25: TLabel;
    Edit20: TEdit;
    Edit21: TEdit;
    CheckBox2: TCheckBox;
    Panel5: TPanel;
    Edit15: TEdit;
    Label21: TLabel;
    ComboBox2: TComboBox;
    CheckBox3: TCheckBox;
    Panel9: TPanel;
    Label3: TLabel;
    Label5: TLabel;
    NumbersComboBox2: TComboBox;
    BitBtn18: TBitBtn;
    BitBtn5: TBitBtn;
    Panel12Icons: TPanel;
    Label22: TLabel;
    Edit3: TEdit;
    BitBtn1: TBitBtn;
    PanelDBASdefault: TPanel;
    Button1: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Size: TLabel;
    PanelPolygon: TPanel;
    PanelLine: TPanel;
    BitBtn4: TBitBtn;
    BitBtn23: TBitBtn;
    BitBtnPoint: TBitBtn;
    RadioGroup4: TRadioGroup;
    PanelColorByString: TPanel;
    StringsComboBox2: TComboBox;
    Label6: TLabel;
    BitBtn24: TBitBtn;
    CheckBox10: TCheckBox;
    CheckBox1: TCheckBox;
    BitBtn17: TBitBtn;
    Label26: TLabel;
    Label27: TLabel;
    CheckBox9: TCheckBox;
    CheckBox14: TCheckBox;
    Label28: TLabel;
    BitBtn7: TBitBtn;
    BitBtn26: TBitBtn;
    ComboBox3: TComboBox;
    Label29: TLabel;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    Label30: TLabel;
    Panel2: TPanel;
    Label13: TLabel;
    Edit19: TEdit;
    Label31: TLabel;
    Edit22: TEdit;
    Edit24: TEdit;
    Label32: TLabel;
    BitBtn6: TBitBtn;
    BitBtn20: TBitBtn;
    Panel12: TPanel;
    qf2ComboBox9: TComboBox;
    qfBitBtn4: TBitBtn;
    qfBitBtn3: TBitBtn;
    qfBitBtn2: TBitBtn;
    qfBitBtn1: TBitBtn;
    qf1ComboBox10: TComboBox;
    CheckBox17: TCheckBox;
    qfCheckBox3: TCheckBox;
    qfRadioGroup1: TRadioGroup;
    CheckBox18: TCheckBox;
    BitBtn27: TBitBtn;
    Label33: TLabel;
    Edit23: TEdit;
    Panel14: TPanel;
    qflabel2: TLabel;
    qflabel1: TLabel;
    qfcombo2: TComboBox;
    qfcombo1: TComboBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    RadioGroup2: TRadioGroup;
    qfcheck2: TCheckBox;
    qfcheck3: TCheckBox;
    qfBitBtn5: TBitBtn;
    qfBitBtn6: TBitBtn;
    qflabel3: TLabel;
    qfcombo3: TComboBox;
    qfcheck1: TCheckBox;
    ColorDialog1: TColorDialog;
    procedure PlotScaledSymbolsButtonClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure NumbersComboBox2Change(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure LengendClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure Font2ButtonClick(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure ComboBox10Change(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure FontButton2Click(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure ComboBox14Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure Edit11Change(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure Edit14Change(Sender: TObject);
    procedure ComboBox15Change(Sender: TObject);
    procedure ComboBox16Change(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure Modifylegend1Click(Sender: TObject);
    procedure Edit16Change(Sender: TObject);
    procedure RadioGroup8Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure Edit17Change(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure Edit18Change(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure ArrowCheckBoxClick(Sender: TObject);
    procedure ComboBox6Click(Sender: TObject);
    procedure ComboBox7Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
    procedure Edit21Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Edit15Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure StringsComboBox2Change(Sender: TObject);
    procedure Edit19Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure Edit22Change(Sender: TObject);
    procedure Edit24Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure CheckBox17Click(Sender: TObject);
    procedure qf1ComboBox10Change(Sender: TObject);
    procedure qfBitBtn1Click(Sender: TObject);
    procedure qfBitBtn2Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure Edit23Change(Sender: TObject);
    procedure qfcheck1Click(Sender: TObject);
    procedure qfcheck2Click(Sender: TObject);
    procedure qfcheck3Click(Sender: TObject);
    procedure qfcombo1Change(Sender: TObject);
    procedure qfcombo2Change(Sender: TObject);
    procedure qfcombo3Change(Sender: TObject);
    procedure qfBitBtn4Click(Sender: TObject);
    procedure qfBitBtn3Click(Sender: TObject);
    procedure qfBitBtn5Click(Sender: TObject);
    procedure qfBitBtn6Click(Sender: TObject);
  private
    { Private declarations }
      Refiltering : boolean;
      procedure MakeStringFieldLegend;
      procedure MakeHistogram;
      procedure ClearLegend;
      procedure ChangeDisplayOptions;
      //procedure StartQuickFilter(TwoFields : boolean = true);
      procedure Refilter;
      procedure ChangeQFCheckBox(qfCheck : tCheckBox; qfCombo : TComboBox; qfLabel : tLabel; var qfField : shortstring);
      procedure ChangeComboBox(theBox : tComboBox; Direction : integer);
  public
    { Public declarations }
     TheDB : integer;
     NowIdle,
     StillSettingUp : boolean;
     HistGraph : tThisBaseGraph;
     procedure SetPanels;
     procedure CheckThemPanels;
     procedure ShowFieldRanges(ResetRange : boolean);

     procedure SetAutoSpacing;
     procedure FillInLegendPanel(ClearIt : boolean = false);
     procedure DrawSymbol;
  end;


function DBFieldNotLatLong(FieldsInDB : tStringList) : integer;



implementation

{$R *.dfm}

uses
   {$IfDef ExMilIcons}
   {$Else}
   DEM_MilIcon,
   {$EndIf}
   DEMDataBase,
   DEMESRIShapeFile,
   PetMath,PETImage,Petmar,
   rgb_colors_three_params,
   DEMMapf,
   Petimage_form,
   Petmar_ini_file,
   KML_creator,
   PetDBUtils,DEMDef_Routines, get_db_coloring,
   legend_placement,
   DEMCoord, DEM_indexes,

{Main program MDI window for different programs that use this module}
   Nevadia_Main;
{End of the MDI parent declaration}

const
   CreateHistograms = false;
   vth = ' value to highlight';

function DBFieldNotLatLong(FieldsInDB : tStringList) : integer;
begin
   result := 0;
   while (Result < (FieldsInDB.Count)) and ((FieldsInDB.Strings[Result] = 'LAT') or (FieldsInDB.Strings[Result] = 'LONG')
      or (FieldsInDB.Strings[Result] = 'LAT_HI') or (FieldsInDB.Strings[Result] = 'LAT_LOW')
      or (FieldsInDB.Strings[Result] = 'LONG_HI') or (FieldsInDB.Strings[Result] = 'LONG_LOW') ) do inc(Result);
   if (Result >= FieldsInDB.Count) then Result := 0;
end;


procedure Tgis_scaled_form.Refilter;
var
   ff,f1,f2,f3,tstr : shortstring;

   function Element(theText,fName : shortstring) : shortstring;
   begin
      if (theText = '') then Result := ''
      else begin
         if GISdb[theDB].MyData.GetFieldType(fName) = ftString then TStr := QuotedStr(ptTrim(theText))
         else TStr := ptTrim(theText);
         Result := fName + '=' + TStr;
      end;
   end;

begin
   if Refiltering then exit;
   {$IfDef RecordQuickFilter} WriteLineToDebugFile('TQuickFilterForm.Refilter in'); {$EndIf}
   Refiltering := true;

   f1 := Element(qfCombo1.Text,GISdb[theDB].dbOpts.QFField1);
   f2 := Element(qfCombo2.Text,GISdb[theDB].dbOpts.QFField2);
   f3 := Element(qfCombo3.Text,GISdb[theDB].dbOpts.QFField3);

   if (f1 = '') and (f2 = '') and (f3 = '') then begin
      GISdb[theDB].ClearGISFilter;
   end
   else begin
      ff := f1;
      if (f2 <> '') then ff := PetDBUtils.AddAndIfNeeded(ff) + f2;
      if (f3 <> '') then ff := PetDBUtils.AddAndIfNeeded(ff) + f3;
      GISdb[theDB].dbOpts.MainFilter := ff;
      GISdb[theDB].AssembleGISFilter;
   end;

   GISdb[theDB].EmpSource.Enabled := false;
   {$IfDef RecordQuickFilter} WriteLineToDebugFile('Filter applied, ' + ff);   {$EndIf}

   if MDDef.ApplySameFilterAllDBs then GISdb[theDB].ShowOtherDBsWithSameFilter(CheckBox19.Checked,CheckBox20.Checked,true)
   else GISdb[theDB].dbTableF.ShowFilteredDB(CheckBox19.Checked,CheckBox20.Checked);

   {$IfDef NoDBGraphs}
   {$Else}
      if (GISdb[theDB].LastGraph <> Nil) then begin
         //GISdb[theDB].LastGraph.Destroy;
         GISdb[theDB].ActuallyDrawGraph(GISdb[theDB].LastGraphtype);
      end;
   {$EndIf}
   Refiltering := false;
   {$IfDef RecordQuickFilter} WriteLineToDebugFile('TQuickFilterForm.Refilter out'); {$EndIf}
end;


(*   //removed 6/16/2022
procedure Tgis_scaled_form.StartQuickFilter(TwoFields : boolean = true);
begin
   Refilter;
   {$IfDef NoDBGrafs}
   {$Else}
      if (GISdb[theDB].theGraphOwner <> Nil) then CopyImageToBitmap(GISdb[theDB].TheGraphOwner.Image1,GISdb[theDB].dbTablef.GraphOwnerBitmap);
   {$EndIf}
   {$IfDef RecordQuickFilter} WriteLineToDebugFile('Tdbtablef.Quickfiltering1Click' + ' ComboBox8.Items.Count=' + IntToStr(ComboBox10.Items.Count) + ' qf2ComboBox9.Items.Count=' + IntToStr(qf2ComboBox9.Items.Count)); {$EndIf}
   CheckThemPanels;
end;
*)

procedure Tgis_scaled_form.CheckThemPanels;
var
   TheTop : integer;

   procedure CheckPanel(ThePanel : tPanel);
   begin
      if ThePanel.Visible then begin
         ThePanel.Top := TheTop;
         ThePanel.Left := 0;
         ThePanel.Width := ClientWidth;
         theTop := theTop + ThePanel.Height;
      end;
   end;

begin
   theTop := 0;
   CheckPanel(Panel15);
   CheckPanel(Panel10);
   CheckPanel(Panel5);
   CheckPanel(Panel12Icons);
   CheckPanel(PanelDBASDefault);
   CheckPanel(PanelPolygon);
   CheckPanel(PanelLine);
   CheckPanel(PanelColorByString);
   CheckPanel(Panel4);
   CheckPanel(Panel9);
   CheckPanel(Panel3);
   CheckPanel(Panel6);
   CheckPanel(Panel7);
   CheckPanel(Panel8);
   CheckPanel(Panel2);
   CheckPanel(Panel11);
   CheckPanel(Panel13);
   CheckPanel(Panel14Legend);
   CheckPanel(Panel23);
   CheckPanel(Panel12);
   CheckPanel(Panel1);
   ClientHeight := theTop;
end;


procedure Tgis_scaled_form.SetPanels;
begin
   if (theDB = 0) or (GISdb[theDB] = nil) then exit;
   {$IfDef RecordFormSetup} WriteLineToDebugFile('Tgis_scaled_form.SetPanels, dbOpts.dbAutoShow = ' + IntToStr(GISdb[theDB].dbOpts.dbAutoShow)); {$EndIf}

   Panel2.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasVector,dbasTTFontSymbol,dbasColorByString,dbasColorByNumeric,dbasConnectTwoPointsInRec]) and GISdb[theDB].dbOpts.DBLegendLocation.DrawItem;
  if (GISdb[theDB].dbOpts.dbAutoShow in [dbasVector]) and GISdb[theDB].dbOpts.GISVectorsByMaxSpeed then Panel2.Visible := false;

  if (GISdb[theDB].dbOpts.dbAutoShow = dbasColorPosNeg) then begin
     StringsComboBox2.Visible := false;
     StringsComboBox2.Enabled := false;
  end;

  StringsComboBox2.Visible := (GISdb[theDB].dbOpts.dbAutoShow in [dbasColorByString]);
  Label6.Visible := (GISdb[theDB].dbOpts.dbAutoShow in [dbasColorByString]);

   Panel3.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasScaledSquares,dbasScaledCircles,dbasConnectTwoPointsInRec] );
   Panel4.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasScaledSquares,dbasScaledCircles,dbasAnimate]);
   Panel5.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow = dbasColorPosNeg);
   Panel6.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasConnectSeqPts]) or (GISdb[theDB].dbOpts.ConnectUnderPoints);
   PanelColorByString.Visible := (GISdb[theDB].dbOpts.dbAutoShow = dbasColorByString);
   PanelDBASDefault.Visible := (GISdb[theDB].ItsAPointDB or GISdb[theDB].XYZfile) and (GISdb[theDB].dbOpts.dbAutoShow in [dbasDefault,dbasColorByString,dbasColorByNumeric,dbasKoppen]);
   GISdb[theDB].ColorButtonForSymbol(BitBtnPoint);
   CheckBox14.Enabled := GISdb[theDB].ItsAPointDB;

   PanelLine.Visible := LineShapeFile(GISdb[theDB].ShapeFileType) and (GISdb[theDB].dbOpts.dbAutoShow in [dbasDefault,dbasZvalues]) ;
   PanelPolygon.Visible := (GISdb[theDB].dbOpts.dbAutoShow = dbasDefault) and AreaShapeFile(GISdb[theDB].ShapeFileType);

   if (GISdb[theDB].dbOpts.dbAutoShow = dbasDefault) then begin
      GISdb[theDB].ColorButtonForSymbol(BitBtn4);
      GISdb[theDB].ColorButtonForSymbol(BitBtn23);
   end;

   ColorLineWidthBitBtn(BitBtn24,GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
   ComboBox5.Enabled :=(GISdb[theDB].ItsAPointDB or GISdb[theDB].XYZfile);
   Font2Button.Enabled := (GISdb[theDB].ItsAPointDB or GISdb[theDB].XYZfile);

   CheckBox3.Checked := GISdb[theDB].dbOpts.LabelDBPlots;
   Panel7.Visible := GISdb[theDB].CanPlot and GISdb[theDB].dbOpts.LabelDBPlots;
   Panel8.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasVector]);
   Panel9.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasScaledSquares,dbasScaledCircles,dbasColorByNumeric]);
   Panel10.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasAnimate]);
   Panel11.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasAnimate]);
   Panel12Icons.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow in [dbasIconField,dbasIconAll]);

   Panel13.Visible := GISdb[theDB].CanPlot and (GISdb[theDB].dbOpts.dbAutoShow = dbasMultiFieldRGB);
   Panel14Legend.Visible := false;  //(GISdb[theDB].dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasColorByString,dbasColorByNumeric,dbasAnimate]);
   Panel15.Visible := false;   //(GISdb[theDB].GISWhatDraw = gisTimeSeq);

   {$IfDef ExGeography}
      CheckBox9.Visible := false;
   {$Else}
      CheckBox9.Checked := (GISdb[theDB].dbOpts.dbAutoShow in [dbasMonthlyTemp,dbasMonthlyRain]) or GISdb[theDB].MyData.FieldExists('JAN_U_MS') or (GISDB[theDB].dbFullName = GlobalCurrentsFName);
   {$EndIf}

   Panel23.Visible := (GISdb[theDB].MyData.FieldExists(GISdb[theDB].MonthFieldName) or CheckBox9.Checked);
   CheckBox9.Enabled := GISdb[theDB].MyData.FieldExists(GISdb[theDB].MonthFieldName);
   Edit17.Text := RealToString(GISdb[theDB].dbOpts.GISVectorsMaxSpeed,-8,-2);
   RadioGroup4.ItemIndex := GISdb[theDB].DbOpts.dbColorMode;
   CheckBox14.Checked := GISdb[theDB].dbOpts.ConnectUnderPoints;

   if (GISdb[theDB].dbOpts.dbAutoShow = dbasMonthlyTemp) or (GISdb[theDB].dbOpts.dbAutoShow in [dbasVector]) then begin
      Edit20.Visible := false;
      Edit21.Visible := false;
      Label24.Visible := false;
      Label25.Visible := false;
   end;

   if (GISdb[theDB].dbOpts.dbAutoShow = dbasMultiFieldRGB) then begin
      ComboBox14.Text := GISdb[theDB].dbOpts.RedField;
      ComboBox15.Text := GISdb[theDB].dbOpts.GreenField;
      ComboBox16.Text := GISdb[theDB].dbOpts.BlueField;
      ComboBox14Change(ComboBox14);
      ComboBox14Change(ComboBox15);
      ComboBox14Change(ComboBox16);
   end;

   BitBtn25.Visible := (GISdb[theDB].dbOpts.dbAutoShow <> dbasMonthlyTemp) and (GISdb[theDB].dbOpts.dbAutoShow <> dbasMonthlyRain);

   CheckBox10.Enabled := AreaShapeFile(GISdb[theDB].ShapeFileType) and (GISdb[theDB].dbOpts.dbAutoShow in [dbasColorField,dbasColorByString,dbasColorByNumeric,dbasScaledSquares,dbasScaledCircles]);
   BitBtn24.Enabled := CheckBox10.Enabled;

   CheckThemPanels;
   InsureFormIsOnScreen(Self);
end;


procedure Tgis_scaled_form.SpeedButton1Click(Sender: TObject);
begin
   SpeedButton2Click(Sender);
end;


procedure Tgis_scaled_form.SpeedButton2Click(Sender: TObject);
var
   SymCount : integer;
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.SpeedButton 1 or 2 Click (symbol pick), SymCount was ' + IntToStr(ord(GISdb[theDB].dbOpts.Symbol.DrawingSymbol))); {$EndIf}
   SymCount := ord(GISdb[theDB].dbOpts.Symbol.DrawingSymbol);
   if (Sender = SpeedButton1) then begin
      dec(SymCount);
      if (SymCount > ord(LastSymbol)) then SymCount := 0;
   end
   else begin
      dec(SymCount);
      if (SymCount < 0) then SymCount := ord(LastSymbol);
   end;
   GISdb[theDB].dbOpts.Symbol.DrawingSymbol := tDrawingSymbol(SymCount);
   GISdb[theDB].ColorButtonForSymbol(BitBtnPoint);
   PlotScaledSymbolsButtonClick(Nil);
 end;


procedure Tgis_scaled_form.TrackBar1Change(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.TrackBar1Change (opacity)'); {$EndIf}
   GISdb[theDB].dbOpts.Opacity:= TrackBar1.Position;
   GISdb[theDB].theMapOwner.DoFastMapRedraw;
end;


procedure Tgis_scaled_form.BitBtn20Click(Sender: TObject);
begin
   GISdb[theDB].ProcessDBIniFile(iniInit,'',GISdb[theDB].IniFileName);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.BitBtn21Click(Sender: TObject);
begin
   if (RadioGroup8.ItemIndex > 0) then RadioGroup8.ItemIndex := pred(RadioGroup8.ItemIndex)
   else RadioGroup8.ItemIndex := 11;
end;

procedure Tgis_scaled_form.BitBtn22Click(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn22Click (> month)'); {$EndIf}
   if (RadioGroup8.ItemIndex < 10) then RadioGroup8.ItemIndex := succ(RadioGroup8.ItemIndex)
   else RadioGroup8.ItemIndex := 0;
end;

procedure Tgis_scaled_form.BitBtn23Click(Sender:TObject);
begin
   PickPattern(GISdb[theDB].dbName,GISdb[theDB].dbOpts.AreaSymbolFill,GISdb[theDB].dbOpts.FillColor,GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
   GISdb[theDB].ColorButtonForSymbol(BitBtn23);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.BitBtn24Click(Sender: TObject);
begin
  PickLineSizeAndColor(GISdb[theDB].dbName,BitBtn24,GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
end;

procedure Tgis_scaled_form.BitBtn25Click(Sender: TObject);
begin
   RadioGroup8.ItemIndex := -1;
   RadioGroup8Click(Sender);
end;

procedure Tgis_scaled_form.BitBtn26Click(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn26Click, size was ' + IntToStr(GISdb[theDB].dbOpts.Symbol.Size)); {$EndIf}
   if GISdb[theDB].dbOpts.Symbol.Size > 0 then GISdb[theDB].dbOpts.Symbol.Size := GISdb[theDB].dbOpts.Symbol.Size - 1;
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn26Click, new size ' + IntToStr(GISdb[theDB].dbOpts.Symbol.Size)); {$EndIf}
   GISdb[theDB].ColorButtonForSymbol(BitBtnPoint);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.BitBtn27Click(Sender: TObject);
var
   I : integer;
begin
   if (not StillSettingUp) and NowIdle then begin
      NowIdle := false;
      for i := 1 to MaxDataBase do if (GISdb[i] <> Nil) and (i <> theDB) then begin
         GISdb[i].dbOpts := GISdb[theDB].dbOpts;
         GISdb[i].RedrawLayerOnMap;
      end;
      NowIdle := true;
   end;
end;

procedure Tgis_scaled_form.BitBtn2Click(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn2Click (close)'); {$EndIf}
   Close;
end;

procedure Tgis_scaled_form.Font2ButtonClick(Sender: TObject);
begin
   EditMyFont(GISdb[theDB].dbOpts.GisLabelFont2);
   LoadMyFontIntoWindowsFont(GISdb[theDB].dbOpts.GisLabelFont2,Font2Button.Font);
   Font2Button.Caption := 'Font';
end;

procedure Tgis_scaled_form.FontButton2Click(Sender: TObject);
begin
   EditMyFont(GISdb[theDB].dbOpts.GisLabelFont1);
   LoadMyFontIntoWindowsFont(GISdb[theDB].dbOpts.GisLabelFont1,FontButton2.Font);
   FontButton2.Caption := 'Font';
end;

procedure Tgis_scaled_form.FontButtonClick(Sender: TObject);
begin
   EditMyFont(GISdb[theDB].dbOpts.GisLabelFont1);
   LoadMyFontIntoWindowsFont(GISdb[theDB].dbOpts.GisLabelFont1,FontButton.Font);
   FontButton.Caption := 'Font';
end;

procedure Tgis_scaled_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if (HistGraph <> Nil) then begin
      HistGraph.Close;
      HistGraph := Nil;
   end;
   if (theDB <> 0) and (GISdb[theDB] <> Nil) and (GISdb[theDB].gis_scaled_form <> Nil) then begin
      GISdb[theDB].gis_scaled_form := Nil;
      GISdb[theDB].AutoRedrawAllowed := true;
   end;
   Self := Nil;
end;


procedure Tgis_scaled_form.ArrowCheckBoxClick(Sender: TObject);
begin
   MDDef.ConnectArrows := ArrowCheckBox.Checked;
end;

procedure Tgis_scaled_form.BitBtn10Click(Sender: TObject);
begin
  if (ListBox2.ItemIndex > 0) then ListBox2.ItemIndex := ListBox2.ItemIndex - 1
  else ListBox2.ItemIndex := pred(ListBox2.Items.Count);
  ListBox2Click(nil);
end;

procedure Tgis_scaled_form.BitBtn11Click(Sender: TObject);
begin
   PickLineSizeAndColor('Vectors',BitBtn11, GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
end;


procedure Tgis_scaled_form.BitBtn12Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile(' Tgis_scaled_form.BitBtn12Click'); {$EndIf}
   if ListBox2.ItemIndex < ListBox2.Items.Count-2 then ListBox2.ItemIndex := ListBox2.ItemIndex + 1
   else ListBox2.ItemIndex := 0;
   ListBox2Click(nil);
end;


procedure Tgis_scaled_form.BitBtn13Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Connect points',BitBtn13,GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
end;


procedure Tgis_scaled_form.BitBtn14Click(Sender: TObject);
{$IfDef ExMovies}
begin
{$Else}
var
   fName2 : PathStr;
   i : integer;
   MovieList : tStringList;
begin
   MovieList := tStringList.Create;
   for I := 0 to pred(ListBox2.Items.Count) do begin
      ListBox2.ItemIndex := i;
      ListBox2Click(Sender);
      fName2 := 'frame' + IntToStr(i) + MovieFileExt;
      GISdb[theDB].theMapOwner.Image1.Picture.Graphic.SaveToFile(DEMdefs.MovieDir + fName2);
      MovieList.Add(fName2);
   end;
   ApplicationProcessMessages;
   fName2 := 'animate_field.mov';
   MovieList.SaveToFile(DEMdefs.MovieDir + fName2);
   MovieList.Free;
   PetImage.MakeMovie(fName2);
{$EndIf}
end;


procedure Tgis_scaled_form.BitBtn15Click(Sender: TObject);
var
   Bm2,Bitmap : tMyBitmap;
begin
   with GISdb[theDB] do begin
      CopyImageToBitmap(TheMapOwner.Image1,Bm2);
      MyData.First;
      while not MyData.eof do begin
         CloneImageToBitmap(TheMapOwner.Image1,Bitmap);
         Bitmap.Canvas.Draw(0,0,bm2);
         DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
         Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
         Delay(TrackBar2.Position);
         MyData.Next;
      end;
      Bm2.Free;
   end;
end;

procedure Tgis_scaled_form.BitBtn17Click(Sender: TObject);
begin
   GISdb[theDB].dbTablef.BitBtn5Click(Sender);
   ShowDefaultCursor;
end;

procedure Tgis_scaled_form.BitBtn18Click(Sender: TObject);
begin
   MakeHistogram;
end;

procedure Tgis_scaled_form.BitBtn19Click(Sender: TObject);
var
   ThisGraph : tThisBaseGraph;
   rfile : file;
   i : integer;
   v : array[1..3] of float32;
begin
   ThisGraph := tThisBaseGraph.Create(Application);
   ThisGraph.OpenXYColorFile(rfile);
   with GISdb[theDB] do begin
      ThisGraph.GraphDraw.HorizLabel := RemoveUnderscores(dbOpts.RedField);
      ThisGraph.GraphDraw.VertLabel := RemoveUnderscores(dbOpts.GreenField);
      i :=0;
      EmpSource.Enabled := false;
      StartProgress('Graph');
      MyData.First;
      while not MyData.EOF do begin
         inc(i);
         UpdateProgressBar(i/MyData.RecordCount);
         if GetFloat32FromTableLinkPossible(dbOpts.RedField,v[1]) and GetFloat32FromTableLinkPossible(dbOpts.GreenField,v[2]) then begin
            v[3] := GISdb[theDB].RGBColorFromThreeNumericFields;
            BlockWrite(rfile,v,1);
         end;
         MyData.Next;
      end;
      CloseFile(rfile);
      EndProgress;
      ThisGraph.AutoScaleAndRedrawDiagram;
      EmpSource.Enabled := true;
   end;
end;


procedure Tgis_scaled_form.BitBtn1Click(Sender: TObject);
var
   fName : PathStr;
begin
    if (GISdb[theDB].dbOpts.AllIconFName = '') then FName := MainMapData + 'Icons\';
    if not PetImage.GetGraphicsFileName('',FName) then begin
       GISdb[theDB].dbOpts.AllIconFName := '';
    end
    else GISdb[theDB].dbOpts.AllIconFName := ExtractFileName(fName);
    GISdb[theDB].ColorButtonForSymbol(BitBtn1,'Icon');
    PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.MakeHistogram;
begin
{$IfDef NoDBGrafs}
{$Else}
   BaseGraf.CreateSmallGraph := true;
   HistGraph := GISdb[theDB].OldCreateHistogramFromDataBase(true,NumbersComboBox2.Text,'','',false);
   BaseGraf.CreateSmallGraph := false;
   HistGraph.Top := 0;
   HistGraph.Left := wmdem.Width - HistGraph.Width - 10;
{$EndIf}
end;


procedure Tgis_scaled_form.MakeStringFieldLegend;
var
   LegendBitmap : tMyBitmap;
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile('Tgis_scaled_form.MakeStringFieldLegend'); {$EndIf}
   LegendBitmap := GISdb[theDB].StringFieldLegend;
   DisplayBitmap(LegendBitmap,'Legend ' + Edit19.Text);
   FreeAndNil(LegendBitmap);
end;


procedure Tgis_scaled_form.Modifylegend1Click(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.Modifylegend1Click in'); {$EndIf}
   if (GISdb[theDB].TheMapOwner.MapDraw <> Nil) then GISdb[theDB].theMapOwner.MapDraw.DeleteSingleMapLayer(GISdb[theDB].TheMapOwner.MapDraw.LegendOverlayfName);
   LegendOptions('data base',MDDef.LegendFont, GISdb[theDB].dbOpts.DBLegendLocation);
   if ((GISdb[theDB].dbOpts.dbAutoShow in [dbasVector]) and CheckBox5.Checked) then begin
      FillInLegendPanel(true);
   end
   else if (GISdb[theDB].dbOpts.DBColorScheme = LegChloropleth) and (RadioGroup4.ItemIndex = 1) then begin
      //DisplayBitmap(GISdb[theDB].ChloroplethLegend(Edit19.Text),'Legend ' + Edit19.Text);
      FillInLegendPanel(true);
   end
   else if (GISdb[theDB].dbOpts.dbAutoShow in [dbasColorByString]) then begin
      FillInLegendPanel(true);
      MakeStringFieldLegend;
   end
   else begin
      FillInLegendPanel;
   end;
   SetPanels;
   PlotScaledSymbolsButtonClick(nil);
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.Modifylegend1Click out'); {$EndIf}
end;


procedure Tgis_scaled_form.ClearLegend;
var
   Bitmap : tMyBitmap;
begin
    CreateBitmap(Bitmap,Image1.Width,Image1.Height);
    Image1.Picture.Graphic := Bitmap;
    Bitmap.Free;
end;


procedure Tgis_scaled_form.RadioGroup1Click(Sender: TObject);
begin
   if RadioGroup1.ItemIndex = 0 then GISdb[theDB].dbOpts.dbAutoShow := dbasScaledSquares
   else if RadioGroup1.ItemIndex = 1 then GISdb[theDB].dbOpts.dbAutoShow := dbasScaledCircles
   else if RadioGroup1.ItemIndex = 2 then GISdb[theDB].dbOpts.dbAutoShow := dbasTTFontSymbol;
   BitBtn3.Enabled := (RadioGroup1.ItemIndex = 2);
   PlotScaledSymbolsButtonClick(Nil);
end;


procedure Tgis_scaled_form.BitBtn3Click(Sender: TObject);
begin
   {$IfDef ExMilIcons}
   {$Else}
   DEM_MilIcon.PickFontAndCharacter(GISdb[theDB].dbOpts.TTSymbolFontName,GISdb[theDB].dbOpts.TTSymbolFontChar);
   DrawSymbol;
   PlotScaledSymbolsButtonClick(Sender);
   {$EndIf}
end;


procedure Tgis_scaled_form.BitBtn4Click(Sender: TObject);
begin
   PickLineSizeAndColor(GISdb[theDB].dbName,BitBtn4,GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.DrawSymbol;
var
   Bitmap : tMyBitmap;
begin
   CreateBitmap(Bitmap,40,40);
   if GISdb[theDB].dbOpts.dbAutoShow = dbasTTFontSymbol then begin
      Bitmap.Canvas.Font.Name := GISdb[theDB].dbOpts.TTSymbolFontName;
      Bitmap.Canvas.Font.Size := 36;
      Bitmap.Canvas.TextOut(0,0,GISdb[theDB].dbOpts.TTSymbolFontChar);
      Petimage.GetImagePartOfBitmap(Bitmap);
   end;
   BitBtn3.Glyph := Bitmap;
   Bitmap.Free;
end;


procedure Tgis_scaled_form.BitBtn5Click(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn5Click (get color), dbOpts.dbAutoShow = ' + IntToStr(GISdb[theDB].dbOpts.dbAutoShow)); {$EndIf}
   if (GISdb[theDB].dbOpts.dbAutoShow = dbasDefault) or (GISdb[theDB].dbOpts.dbColorMode = dbcmConstant) then Petmar.QueryColor(BitBtn5,MDDef.ScaledSymbolColor)
   else begin
      Get_db_coloring.GetColorValuesForDataBase(theDB,NumbersComboBox2.Text);
      Label3.Caption := 'Color range: ' + RealToString(GISdb[theDB].dbOpts.ColorMin,-12,-2) + ' to ' + RealToString(GISdb[theDB].dbOpts.ColorMax,-12,-2);
      Clearlegend;
   end;
   FillInLegendPanel;
   PlotScaledSymbolsButtonClick(Nil);
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn5Click out, ' + Label3.Caption); {$EndIf}
end;


procedure Tgis_scaled_form.BitBtn6Click(Sender: TObject);
{$IfDef ExMovies}
begin
{$Else}
var
   fname,MovieName : PathStr;
   MovieFiles : tstringlist;
   i : integer;
begin
   MovieFiles := tStringList.Create;
   for i := 1 to 12 do begin
      RadioGroup8.ItemIndex := pred(i);
      fName := MovieDir + 'climate_' + IntToStr(i) + '.bmp';
      PetImage.SaveImageAsBMP(GISdb[theDB].TheMapOwner.IMage1,fName);
      MovieFiles.Add(ExtractFileName(fName));
   end;
   MovieName := MovieDir + 'montly_animatioin_movie.mov';
   MovieFiles.SaveToFile(MovieName);
   MovieFiles.Free;
   Petimage.MakeMovie(ExtractFileName(MovieName));
{$EndIf}
end;

procedure Tgis_scaled_form.PlotScaledSymbolsButtonClick(Sender: TObject);
begin
   if StillSettingUp or ((Sender = nil) and (not MDDef.QuickMapRedraw)) then exit;
   if NowIdle then begin
      NowIdle := false;
      {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.PlotScaledSymbolsButtonClick in, dbAutoShow=' + IntToStr(GISdb[theDB].dbOpts.dbAutoShow) + ' DbOpts.dbColorMode=' + IntToStr(GISdb[theDB].DbOpts.dbColorMode));   {$EndIf}
      GISdb[theDB].RedrawLayerOnMap;
      GISdb[theDB].ColorButtonForSymbol(GISdb[theDB].dbTableF.BitBtn1);

      {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.PlotScaledSymbolsButtonClick out'); {$EndIf}
      NowIdle := true;
   end;
end;

procedure Tgis_scaled_form.qf1ComboBox10Change(Sender: TObject);
begin
   Refilter;
end;


procedure Tgis_scaled_form.ChangeComboBox(theBox : tComboBox; Direction : integer);
begin
   if Direction = 1 then begin
      if theBox.ItemIndex < theBox.Items.Count -1 then theBox.ItemIndex := theBox.ItemIndex + 1
      else theBox.ItemIndex := 0;
   end
   else begin
      if (theBox.ItemIndex > 0) then theBox.ItemIndex := theBox.ItemIndex - 1
      else theBox.ItemIndex := theBox.Items.Count - 1;
   end;
   Refilter;
end;

procedure Tgis_scaled_form.qfBitBtn1Click(Sender: TObject);
begin
   ChangeComboBox(qfCombo1, 1);
end;

procedure Tgis_scaled_form.qfBitBtn2Click(Sender: TObject);
begin
   ChangeComboBox(qfCombo1, -1);
end;


procedure Tgis_scaled_form.qfBitBtn3Click(Sender: TObject);
begin
   ChangeComboBox(qfCombo2, 1);
end;

procedure Tgis_scaled_form.qfBitBtn4Click(Sender: TObject);
begin
   ChangeComboBox(qfCombo2, -1);
end;

procedure Tgis_scaled_form.qfBitBtn5Click(Sender: TObject);
begin
   ChangeComboBox(qfCombo3, 1);
end;

procedure Tgis_scaled_form.qfBitBtn6Click(Sender: TObject);
begin
   ChangeComboBox(qfCombo3, 2);
end;

procedure Tgis_scaled_form.ChangeQFCheckBox(qfCheck : tCheckBox; qfCombo : TComboBox; qfLabel : tLabel; var qfField : shortstring);
begin
  if qfCheck.Checked then begin
     if (QFField = '') or (Not GISdb[theDB].MyData.FieldExists(QFField)) then begin
        QFField := GISdb[theDB].PickField('Field',[ftString,ftInteger,ftSmallInt]);
     end;
     GISdb[theDB].FillComboBoxFromField(qfCombo,QFField);
  end
  else begin
     QFField := '';
  end;
  qfLabel.Caption := QFField;
  Refilter;
  {$IfDef NoDBGrafs}
  {$Else}
      if (GISdb[theDB].theGraphOwner <> Nil) then CopyImageToBitmap(GISdb[theDB].TheGraphOwner.Image1,GISdb[theDB].dbTablef.GraphOwnerBitmap);
  {$EndIf}
  {$IfDef RecordQuickFilter} WriteLineToDebugFile('Tdbtablef.Quickfiltering1Click' + ' ComboBox8.Items.Count=' + IntToStr(ComboBox10.Items.Count) + ' qf2ComboBox9.Items.Count=' + IntToStr(qf2ComboBox9.Items.Count)); {$EndIf}
   CheckThemPanels;
end;


procedure Tgis_scaled_form.qfcheck1Click(Sender: TObject);
begin
   ChangeQFCheckBox(qfCheck1,qfcombo1,qfLabel1,GISdb[theDB].dbOpts.QFField1);
end;

procedure Tgis_scaled_form.qfcheck2Click(Sender: TObject);
begin
   ChangeQFCheckBox(qfCheck2,qfcombo2,qfLabel2,GISdb[theDB].dbOpts.QFField2);
end;

procedure Tgis_scaled_form.qfcheck3Click(Sender: TObject);
begin
   ChangeQFCheckBox(qfCheck3,qfcombo3,qfLabel3,GISdb[theDB].dbOpts.QFField3);
end;

procedure Tgis_scaled_form.qfcombo1Change(Sender: TObject);
begin
   Refilter;
end;

procedure Tgis_scaled_form.qfcombo2Change(Sender: TObject);
begin
   Refilter;
end;

procedure Tgis_scaled_form.qfcombo3Change(Sender: TObject);
begin
   Refilter;
end;

procedure Tgis_scaled_form.BitBtn7Click(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.BitBtn26Click, size was ' + IntToStr(GISdb[theDB].dbOpts.Symbol.Size)); {$EndIf}
   if (GISdb[theDB].dbOpts.Symbol.Size > 0) then GISdb[theDB].dbOpts.Symbol.Size := GISdb[theDB].dbOpts.Symbol.Size + 1;
   GISdb[theDB].ColorButtonForSymbol(BitBtnPoint);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.BitBtn8Click(Sender: TObject);
begin
   GISdb[theDB].DisplayTable(GISdb[theDB].MyData.Filter);
   PlotScaledSymbolsButtonClick(Sender);
end;

procedure Tgis_scaled_form.BitBtn9Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Connect points',BitBtn9,GISdb[theDB].dbOpts.LineColor,GISdb[theDB].dbOpts.LineWidth);
end;

procedure Tgis_scaled_form.Button1Click(Sender: TObject);
begin
   ColorDialog1.Color := ConvertPlatformColorToTColor(GISdb[theDB].dbOpts.Symbol.Color);
   if ColorDialog1.Execute then GISdb[theDB].dbOpts.Symbol.Color := ConvertTColorToPlatFormColor(ColorDialog1.Color);
   GISdb[theDB].ColorButtonForSymbol(BitBtnPoint);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.CheckBox10Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.OutlinePolygons := CheckBox10.Checked;
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.CheckBox11Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.WindAutoSpace := CheckBox11.Checked;
   SetAutoSpacing;
end;

procedure Tgis_scaled_form.CheckBox12Click(Sender: TObject);
begin
   CheckBox13.Enabled := CheckBox12.Checked;
end;

procedure Tgis_scaled_form.CheckBox14Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.ConnectUnderPoints := CheckBox14.Checked;
   Panel6.Visible := (GISdb[theDB].dbOpts.dbAutoShow in [dbasConnectSeqPts]) or (GISdb[theDB].dbOpts.ConnectUnderPoints);
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.CheckBox15Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.GrayScale := CheckBox15.Checked;
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.CheckBox16Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.Subdue := CheckBox16.Checked;
   PlotScaledSymbolsButtonClick(Nil);
end;

procedure Tgis_scaled_form.CheckBox17Click(Sender: TObject);
begin
   Panel12.Visible := CheckBox17.Checked;
   CheckThemPanels;
   //StartQuickFilter(false);
end;


procedure Tgis_scaled_form.SetAutoSpacing;
begin
   Label14.Enabled := GISdb[theDB].dbOpts.WindAutoSpace;
   Edit18.Enabled := GISdb[theDB].dbOpts.WindAutoSpace;
   Label11.Enabled := not GISdb[theDB].dbOpts.WindAutoSpace;
   Edit6.Enabled := not GISdb[theDB].dbOpts.WindAutoSpace;
end;

procedure Tgis_scaled_form.CheckBox1Click(Sender: TObject);
begin
  MDDef.ApplySameFilterAllDBs := CheckBox1.Checked;
end;

procedure Tgis_scaled_form.CheckBox2Click(Sender: TObject);
begin
   MDDef.QuickMapRedraw := CheckBox2.Checked;
end;

procedure Tgis_scaled_form.CheckBox3Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.LabelDBPlots := CheckBox3.Checked;
   SetPanels;
end;

procedure Tgis_scaled_form.CheckBox4Click(Sender: TObject);
begin
   MDDef.AvoidTextOverprints := CheckBox4.Checked;
end;

procedure Tgis_scaled_form.CheckBox5Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.GISVectorsByMaxSpeed := CheckBox5.Checked;
   SetPanels;
end;

procedure Tgis_scaled_form.CheckBox6Click(Sender: TObject);
begin
   MDDef.PlotArrowHead := CheckBox6.Checked;
end;

procedure Tgis_scaled_form.CheckBox7Click(Sender: TObject);
begin
   MDDef.ReverseArrow := CheckBox7.Checked;
end;

procedure Tgis_scaled_form.CheckBox9Click(Sender: TObject);
begin
   SetPanels;
end;

procedure Tgis_scaled_form.ComboBox10Change(Sender: TObject);
var
   DataThere : tStringList;
begin
   GISdb[theDB].EmpSource.Enabled := false;
   GISdb[theDB].MyData.ApplyFilter('');
   DataThere := GISdb[theDB].MyData.UniqueEntriesInDB(ComboBox10.Text);
   ListBox2.Items := DataThere;
   DataThere.Free;
   ListBox2.ItemIndex := 0;
   GISdb[theDB].EmpSource.Enabled := true;
end;


procedure Tgis_scaled_form.ComboBox14Change(Sender: TObject);
begin
   GISdb[theDB].EmpSource.Enabled := false;
   if (Sender = ComboBox14) then begin
      GISdb[theDB].dbOpts.RedField := ComboBox14.Text;
      if (GISdb[theDB].dbOpts.RedField = '') then begin
         Edit9.Text := '';
         Edit10.Text := '';
      end
      else begin
         GISdb[theDB].FieldRange(GISdb[theDB].dbOpts.GreenField,MinRedRange,MaxRedRange,GISdb[theDB].MyData.Filtered);
         Edit9.Text := RealToString(MinRedRange,-18,-4);
         Edit10.Text := RealToString(MaxRedRange,-18,-4);
      end;
   end;

   if (Sender = ComboBox15) then begin
      GISdb[theDB].dbOpts.GreenField := ComboBox15.Text;
      if (GISdb[theDB].dbOpts.GreenField = '') then begin
         Edit11.Text := '';
         Edit12.Text := '';
      end
      else begin
         GISdb[theDB].FieldRange(GISdb[theDB].dbOpts.GreenField,MinGreenRange,MaxGreenRange,GISdb[theDB].MyData.Filtered);
         Edit11.Text := RealToString(MinGreenRange,-18,-4);
         Edit12.Text := RealToString(MaxGreenRange,-18,-4);
      end;
   end;

   if (Sender = ComboBox16) then begin
      GISdb[theDB].dbOpts.BlueField := ComboBox16.Text;
      if (GISdb[theDB].dbOpts.BlueField = '') then begin
         Edit13.Text := '';
         Edit14.Text := '';
      end
      else begin
         GISdb[theDB].FieldRange(GISdb[theDB].dbOpts.BlueField,MinBlueRange,MaxBlueRange,GISdb[theDB].MyData.Filtered);
         Edit13.Text := RealToString(MinBlueRange,-18,-4);
         Edit14.Text := RealToString(MaxBlueRange,-18,-4);
      end;
   end;
   GISdb[theDB].EmpSource.Enabled := true;
end;

procedure Tgis_scaled_form.ComboBox15Change(Sender: TObject);
begin
   ComboBox14Change(Sender);
end;

procedure Tgis_scaled_form.ComboBox16Change(Sender: TObject);
begin
   ComboBox14Change(Sender);
end;

procedure Tgis_scaled_form.ComboBox1Change(Sender: TObject);
var
   Num,Valid : integer;
   Sum : float64;
begin
   GISdb[theDB].EmpSource.Enabled := false;
   GISdb[theDB].dbOpts.SymSizeField := ComboBox1.Text;
   GISdb[theDB].FindFieldRangeLinkPossible(ComboBox1.Text,Num,Valid,Sum,GISdb[theDB].dbOpts.SizeMin,GISdb[theDB].dbOpts.SizeMax);
   Label4.Caption := RealToString(GISdb[theDB].dbOpts.SizeMin,-12,-6) + ' to ' + RealToString(GISdb[theDB].dbOpts.SizeMax,-12,-6);
   Edit2.Text := RealToString(GISdb[theDB].dbOpts.SizeMax,-12,-6);
   Edit24.Text := RealToString(GISdb[theDB].dbOpts.SizeMin,-12,-6);
   GISdb[theDB].EmpSource.Enabled := true;
   if (not StillSettingUp ) then begin
      PlotScaledSymbolsButtonClick(Sender);
   end;
end;

procedure Tgis_scaled_form.ComboBox2Change(Sender: TObject);
begin
   GISdb[theDB].dbOpts.FloatColorField := ComboBox2.Text;
   ChangeDisplayOptions;
end;

procedure Tgis_scaled_form.ComboBox3Change(Sender: TObject);
begin
   GISdb[theDB].dbOpts.SegSepField := ComboBox3.Text;
end;

procedure Tgis_scaled_form.ChangeDisplayOptions;
begin
   //GISdb[theDB].NeedToDefineColorTable := true;
   GISdb[theDB].EmpSource.Enabled := false;
   FillInLegendPanel;
   GISdb[theDB].EmpSource.Enabled := true;
   ClearLegend;
   PlotScaledSymbolsButtonClick(nil);
end;

procedure Tgis_scaled_form.StringsComboBox2Change(Sender: TObject);
begin
   GISdb[theDB].dbOpts.FloatColorField := StringsComboBox2.Text;
   if (GISdb[theDB].StringCategories <> Nil) then begin
      GISdb[theDB].StringCategories.Free;
      GISdb[theDB].StringCategories := Nil;
   end;
   GISdb[theDB].EmpSource.Enabled := false;
   PetDBUtils.FindUniqueEntriesLinkPossible(GISdb[theDB].MyData,GISdb[theDB].LinkTable,GISdb[theDB].dbOpts.LinkFieldThisDB,GISdb[theDB].dbOpts.LinkFieldOtherDB,StringsComboBox2.Text,GISdb[theDB].StringCategories);
   GISdb[theDB].EmpSource.Enabled := true;
   Label6.Caption := 'Categories: ' + IntToStr(GISdb[theDB].StringCategories.Count);
   Edit19.Text := GISdb[theDB].dbOpts.FloatColorField;
   GISdb[theDB].LegendCaption := Edit19.Text;
   ChangeDisplayOptions;
end;

procedure Tgis_scaled_form.ShowFieldRanges(ResetRange : boolean);
var
   TStr : shortstring;
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.ShowFieldRanges in (change color symbolization)'); {$EndIf}
   if (NumbersComboBox2.Text = '') then exit;

   if ResetRange or (abs(GISdb[theDB].dbOpts.ColorMax - GISdb[theDB].dbOpts.ColorMin) < 0.01)  then GISdb[theDB].FieldRange(NumbersComboBox2.Text,GISdb[theDB].dbOpts.ColorMin,GISdb[theDB].dbOpts.ColorMax,GISdb[theDB].MyData.Filtered);

   tStr := RealToString(GISdb[theDB].dbOpts.ColorMin,-12,-2) + ' to ' + RealToString(GISdb[theDB].dbOpts.ColorMax,-12,-2);
   Label3.Caption := 'Color: ' + TStr;
   Label5.Caption := 'Field: ' + TStr;
   if CreateHistograms then MakeHistogram;
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.ShowFieldRanges out ,' + Label3.Caption + '  ' + Label5.Caption ); {$EndIf}
end;

procedure Tgis_scaled_form.NumbersComboBox2Change(Sender: TObject);
begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.ComboBox2Change in (change color symbolization)'); {$EndIf}
   if (HistGraph <> Nil) then begin
      HistGraph.Close;
      HistGraph := Nil;
   end;
   Label3.Caption := '';
   Label5.Caption := '';
   Edit19.Text := NumbersComboBox2.Text;
   GISdb[theDB].dbOpts.FloatColorField := NumbersComboBox2.Text;
   ShowFieldRanges(true);
   ChangeDisplayOptions;
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.ComboBox2Change out'); {$EndIf}
end;

procedure Tgis_scaled_form.ComboBox4Change(Sender: TObject);
begin
   GISdb[theDB].dbOpts.LabelField := ComboBox4.Text;
end;

procedure Tgis_scaled_form.ComboBox5Change(Sender: TObject);
begin
   GISdb[theDB].dbOpts.SecondLabelField := ComboBox5.Text;
end;

procedure Tgis_scaled_form.ComboBox6Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.MagField := ComboBox6.Text;
end;

procedure Tgis_scaled_form.ComboBox7Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.DirField := ComboBox7.Text;
end;

procedure Tgis_scaled_form.HelpBtnClick(Sender: TObject);
var
   Topic : shortString;
begin
   if (GISdb[theDB].dbOpts.dbAutoShow in [dbasTTFontSymbol]) then  Topic := 'html\gis_scaled_symbols.htm'
   else if (GISdb[theDB].dbOpts.dbAutoShow in [dbasColorByString,dbasColorByNumeric]) then Topic := 'html\gis_color_db_field.htm'
   else if (GISdb[theDB].dbOpts.dbAutoShow in [dbasVector]) then Topic := 'html\gis_vector_overlay.htm'
   else if (GISdb[theDB].dbOpts.dbAutoShow = dbasConnectTwoPointsInRec) then Topic := 'html\db_pt_sep.htm'
   else if (GISdb[theDB].dbOpts.dbAutoShow = dbasConnectSeqPts) then Topic := 'html\db_plot_connect_seq.htm'
   else if GISdb[theDB].dbOpts.dbAutoShow = dbasAnimate then Topic := 'html\db_animate.htm';
   DisplayHTMLTopic(Topic);
end;

procedure Tgis_scaled_form.LengendClick(Sender: TObject);
begin
   Modifylegend1Click(Sender);
end;


procedure Tgis_scaled_form.FillInLegendPanel(ClearIt : boolean = false);
var
   Bitmap : tMyBitmap;
begin
   if (GISdb[theDB].dbOpts.dbAutoShow in [dbasColorByString]) then begin
      ClearLegend;
      exit;
   end;

   if (NumbersComboBox2.Text <> '') and (GISdb[theDB].dbOpts.ColorMax > GISdb[theDB].dbOpts.ColorMin) then begin
      Bitmap := Nil;
      if (GISdb[theDB].dbOpts.dbAutoShow in [dbasVector]) and GISdb[theDB].dbOpts.GISVectorsByMaxSpeed then
          Bitmap := DefaultHorizontalLegendOnBitmap(0,GISdb[theDB].dbOpts.GISVectorsMaxSpeed,'','SPEED',GISdb[theDB].dbOpts.DBColorScheme,GISdb[theDB].dbOpts.DBColorPaletteName)
      else Bitmap := DefaultHorizontalLegendOnBitmap(GISdb[theDB].dbOpts.ColorMin,GISdb[theDB].dbOpts.ColorMax,'',NumbersComboBox2.Text,GISdb[theDB].dbOpts.DBColorScheme,GISdb[theDB].dbOpts.DBColorPaletteName);
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
   //else CreateBitmap(Bitmap,Image1.Width,Image1.Height);

   if Panel9.Visible then begin
      BitBtn5.Caption := '';
      GISdb[theDB].DefineColorTable;
      BitBtn5.Glyph := MakeColorScaleBitmap(60,14,GISdb[theDB].dbOpts.DBColorScheme,GISdb[theDB].ColorDefTable);
   end;
end;


procedure Tgis_scaled_form.ListBox2Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile(' Tgis_scaled_form.ListBox2Click'); {$EndIf}
   GISdb[theDB].dbOpts.MainFilter := ComboBox10.Text + '=' + QuotedStr(ListBox2.Items[ListBox2.ItemIndex]);
   GISdb[theDB].AssembleGISFilter;
   PlotScaledSymbolsButtonClick(Sender);
end;

procedure Tgis_scaled_form.Edit10Change(Sender: TObject);
begin
  CheckEditString(Edit10.Text,MaxRedRange);
end;

procedure Tgis_scaled_form.Edit11Change(Sender: TObject);
begin
   CheckEditString(Edit11.Text,MinGreenRange);
end;

procedure Tgis_scaled_form.Edit12Change(Sender: TObject);
begin
   CheckEditString(Edit12.Text,MaxGreenRange);
end;

procedure Tgis_scaled_form.Edit13Change(Sender: TObject);
begin
   CheckEditString(Edit13.Text,MinBlueRange);
end;

procedure Tgis_scaled_form.Edit14Change(Sender: TObject);
begin
   CheckEditString(Edit14.Text,MaxBlueRange);
end;

procedure Tgis_scaled_form.Edit15Change(Sender: TObject);
begin
   CheckEditString(Edit15.Text,GISdb[theDB].ZeroTol);
end;

procedure Tgis_scaled_form.Edit16Change(Sender: TObject);
begin
   CheckEditString(Edit16.Text,MDDef.GISLabelSkip);
end;

procedure Tgis_scaled_form.Edit17Change(Sender: TObject);
begin
   CheckEditString(Edit17.Text,GISdb[theDB].dbOpts.GISVectorsMaxSpeed);
end;

procedure Tgis_scaled_form.Edit18Change(Sender: TObject);
begin
   CheckEditString(Edit18.Text,GISdb[theDB].dbOpts.WindPixelSpace);
end;


procedure Tgis_scaled_form.Edit19Change(Sender: TObject);
begin
   if StillSettingUp then exit;
   GISdb[theDB].LegendCaption := Edit19.Text;
end;

procedure Tgis_scaled_form.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,GISdb[theDB].dbOpts.ScaledSymMaxSize);
end;

procedure Tgis_scaled_form.Edit20Change(Sender: TObject);
begin
   CheckEditString(Edit20.Text,GISdb[theDB].dbOpts.ColorMin);
end;

procedure Tgis_scaled_form.Edit21Change(Sender: TObject);
begin
   CheckEditString(Edit21.Text,GISdb[theDB].dbOpts.ColorMax);
end;

procedure Tgis_scaled_form.Edit22Change(Sender: TObject);
begin
   CheckEditString(Edit22.Text,GISdb[theDB].dbOpts.ScaledSymMinSize);
end;

procedure Tgis_scaled_form.Edit23Change(Sender: TObject);
begin
   CheckEditString(Edit23.Text,GISdb[theDB].dbOpts.xlabelOff);
end;

procedure Tgis_scaled_form.Edit24Change(Sender: TObject);
begin
   CheckEditString(Edit24.Text,GISdb[theDB].dbOpts.SizeMin);
end;

procedure Tgis_scaled_form.Edit2Change(Sender: TObject);
begin
    CheckEditString(Edit2.Text,GISdb[theDB].dbOpts.SizeMax);
end;

procedure Tgis_scaled_form.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,GISdb[theDB].dbOpts.IconScalingFactor);
end;

procedure Tgis_scaled_form.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.ConnectArrowSpacing);
end;

procedure Tgis_scaled_form.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,GISdb[theDB].dbOpts.VectorLineMult);
end;

procedure Tgis_scaled_form.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,GISdb[theDB].dbOpts.VectorThinFactor);
end;

procedure Tgis_scaled_form.Edit7Change(Sender: TObject);
var
   DataThere : tStringList;
   Req,i : integer;
begin
   CheckEditString(Edit7.Text,Req);
   GISdb[theDB].EmpSource.Enabled := false;
   DataThere := GISdb[theDB].MyData.UniqueEntriesInDB(ComboBox10.Text);
   if (Req > 1) then begin
      for I := pred(DataThere.Count) downto 0 do begin
         GISdb[theDB].MyData.ApplyFilter( ComboBox10.Text + '=' + DataThere.Strings[i]);
         if (GISdb[theDB].MyData.RecordCount < Req) then DataThere.Delete(i);
      end;
   end;
   ListBox2.Items := DataThere;
   DataThere.Free;
   ListBox2.ItemIndex := 0;
   GISdb[theDB].EmpSource.Enabled := true;
end;


procedure Tgis_scaled_form.Edit8Change(Sender: TObject);
begin
   CheckEditString(Edit8.Text,MDDef.MaxLabelDecimals);
end;

procedure Tgis_scaled_form.Edit9Change(Sender: TObject);
begin
   CheckEditString(Edit9.Text,MinRedRange);
end;



procedure Tgis_scaled_form.FormCreate(Sender: TObject);
begin
   HistGraph := Nil;
   theDB := 0;
   StillSettingUp := true;
   NowIdle := true;
   Label3.Caption := '';
   Label5.Caption := '';
   Refiltering := false;
   CheckBox1.Checked := MDDef.ApplySameFilterAllDBs;
   CheckBox2.Checked := MDDef.QuickMapRedraw;
   CheckBox7.Checked := MDDef.ReverseArrow;
   Edit16.Text := IntToStr(MDDef.GISLabelSkip);
   Panel12.Visible := false;
   PlaceFormAtMousePosition(Self);
end;

procedure Tgis_scaled_form.RadioGroup3Click(Sender: TObject);
begin
   GISdb[theDB].DbOpts.ConstantSymSize := RadioGroup3.ItemIndex = 1;
   ComboBox1.Enabled := RadioGroup3.ItemIndex = 0;
end;


procedure Tgis_scaled_form.RadioGroup4Click(Sender: TObject);
begin
   {$If Defined(RecordSymProblems) OR Defined(RecordFormSetup)} WriteLineToDebugFile('Tgis_scaled_form.RadioGroup4Click, DbOpts.dbColorMode=' + IntToStr(RadioGroup4.ItemIndex)); {$EndIf}
   GISdb[theDB].DbOpts.dbColorMode := RadioGroup4.ItemIndex;
   if RadioGroup4.ItemIndex = dbcmFieldQuantile then begin
      GISdb[theDB].dbOpts.DBColorScheme := LegChloropleth;
   end;
   if (RadioGroup4.ItemIndex = dbcmFieldLog) and (GISdb[theDB].dbOpts.DBColorScheme = LegChloropleth) then GISdb[theDB].dbOpts.DBColorScheme := LegRainbows;
   ChangeDisplayOptions;
end;


procedure Tgis_scaled_form.RadioGroup5Click(Sender: TObject);
begin
   if (GISdb[theDB].theMapOwner <> Nil) then begin
      GISdb[theDB].theMapOwner.Forceredraw1Click(Nil);
      Case RadioGroup5.ItemIndex of
         0 : begin end;
         1 : begin
                GISdb[theDB].MyData.ApplyFilter('');
                GISdb[theDB].RedrawLayerOnMap;
             end;
      End;
   end;
end;

procedure Tgis_scaled_form.RadioGroup6Click(Sender: TObject);
begin
   GISdb[theDB].dbOpts.VectorsByPolar := RadioGroup6.ItemIndex = 0;
   if GISdb[theDB].dbOpts.VectorsByPolar then begin
      Label9.Caption := 'Magnitude';
      Label12.Caption := 'Direction';
   end
   else begin
      Label9.Caption := 'x (u) component';
      Label12.Caption := 'y (v) component';
   end;
end;


procedure Tgis_scaled_form.RadioGroup8Click(Sender: TObject);
var
   TStr : shortstring;
   Month,i,LabelDB : integer;

      procedure DoOne;
      begin
          {$IfDef RecordSym} WriteLineToDebugFile('DoOne in'); {$EndIf}
          ShowHourglassCursor;
          Edit19.Text := NumbersComboBox2.Text;
          GISdb[theDB].dbOpts.FloatColorField := NumbersComboBox2.Text;
          GISdb[theDB].RedrawLayerOnMap;
          ShowHourglassCursor;
          {$IfDef RecordSym} WriteLineToDebugFile('DoOne out'); {$EndIf}
      end;

      function MakeTimeFilter(aDB : integer) : boolean;
      //var
         //NewTimeFilter : shortstring;
      begin
         {$IfDef RecordSym} WriteLineToDebugFile('MakeTimeFilter'); {$EndIf}
         if GISdb[aDB].MyData.FieldExists('JAN_U_MS') then begin
            if (RadioGroup8.ItemIndex <> -1) then begin
               ComboBox6.Text := UpperCase(MonthName[Month]) + '_U_MS';
               ComboBox7.Text := UpperCase(MonthName[Month]) + '_V_MS';
            end;
            GISdb[aDB].dbOpts.MagField := ComboBox6.Text;
            GISdb[aDB].dbOpts.DirField := ComboBox7.Text;
         end;
         //GISdb[aDB].MakeNewMonthlyFilterAndRedraw(Month);
      end;


begin
   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.RadioGroup8Click, month =' + IntToStr(succ(RadioGroup8.ItemIndex))); {$EndIf}
   ShowHourglassCursor;
   Month := succ(RadioGroup8.ItemIndex);
   LabelDB := theDB;
   if (theDB <> 0) then begin
       GISdb[theDB].EmpSource.Enabled := false;
       CheckEditString(Edit20.Text,GISdb[theDB].dbOpts.ColorMin);
       CheckEditString(Edit21.Text,GISdb[theDB].dbOpts.ColorMax);
      {$IfDef ExGeography}
      {$Else}
      if (GISdb[theDB].dbOpts.dbAutoShow = dbasMonthlyTemp) then begin
         StringsComboBox2.Text := UpperCase(MonthName[Month]) + '_TEMP';
         NumbersComboBox2.Text := StringsComboBox2.Text;
         GISdb[theDB].dbOpts.FloatColorField := StringsComboBox2.Text;
         DoOne;
      end
      else if (GISdb[theDB].dbOpts.dbAutoShow = dbasMonthlyRain) then begin
         StringsComboBox2.Text := UpperCase(MonthName[Month]) + '_PRECIP';
         NumbersComboBox2.Text := StringsComboBox2.Text;
         GISdb[theDB].dbOpts.FloatColorField := StringsComboBox2.Text;
         DoOne;
      end
      else
      {$EndIf}
      begin
         if CheckBox9.Checked then MakeTimeFilter(theDB);
      end;
   end;

   if MDDef.ApplySameFilterAllDBs then begin
     for I := 1 to MaxDataBase do if (i <> theDB) and (GISdb[i] <> Nil) and GISdb[i].LayerIsOn then begin
        {$IfDef RecordSym} WriteLineToDebugFile('GIS=' + IntToStr(i)); {$EndIf}
        GISdb[i].EmpSource.Enabled := false;
        if MakeTimeFilter(i) then GISdb[i].RedrawLayerOnMap;
        GISdb[i].DBTablef.ShowStatus;
        if (LabelDB=0) then LabelDB := i;
      end;
      {$IfDef RecordSym} WriteLineToDebugFile('All db done'); {$EndIf}
   end
   else begin
      GISdb[theDB].EmpSource.Enabled := false;
      //if MakeTimeFilter(theDB) then GISdb[theDB].RedrawLayerOnMap;
      GISdb[theDB].MakeNewMonthlyFilterAndRedraw(Month);
      GISdb[theDB].DBTablef.ShowStatus;
   end;
   ShowHourglassCursor;

   if (LabelDB <> 0) and (GISdb[LabelDB].theMapOwner <> Nil) and GISdb[TheDB].CanPlot and CheckBox12.Checked then begin
      GISdb[LabelDB].theMapOwner.Image1.Canvas.Font.Size := 12;
      GISdb[LabelDB].theMapOwner.Image1.Canvas.Font.Style := [fsBold];
      if (RadioGroup8.ItemIndex >= 0) then TStr := MonthName[succ(RadioGroup8.ItemIndex)]
      else TStr := '';
      if CheckBox13.Checked then TStr := TStr +  '   n=' + IntToStr(GISdb[LabelDB].MyData.RecordCount);
      GISdb[LabelDB].theMapOwner.Image1.Canvas.TextOut(5,5, TStr);
   end;
   ShowDefaultCursor;

   {$IfDef RecordSym} WriteLineToDebugFile('Tgis_scaled_form.RadioGroup8Click out'); {$EndIf}
end;


initialization
finalization
   {$IfDef RecordSym} WriteLineToDebugFile('RecordSymProblems active in gis_scaled_symbols');   {$EndIf}
   {$IfDef RecordDataBase} WriteLineToDebugFile('RecordDataBaseProblems active in gis_scaled_symbols');   {$EndIf}
   {$IfDef RecordDataInsideLoopPlots} WriteLineToDebugFile('RecordDataInsideLoopPlots active in gis_scaled_symbols');   {$EndIf}
   {$IfDef RecordQuantile} WriteLineToDebugFile('RecordQuantileProblems active in gis_scaled_symbols');   {$EndIf}
   {$IfDef RecordGISvectors} WriteLineToDebugFile('RecordGISvectors active in gis_scaled_symbols');   {$EndIf}
   {$IfDef RecordFormSetup} WriteLineToDebugFile('RecordFormSetup active in gis_scaled_symbols'); {$EndIf}
   {$IfDef RecordColorPalette} WriteLineToDebugFile('RecordColorPalette active in gis_scaled_symbols'); {$EndIf}
   {$IfDef RecordQuickFilter} WriteLineToDebugFile('RecordQuickFilter active in gis_scaled_symbols'); {$EndIf}

end.


