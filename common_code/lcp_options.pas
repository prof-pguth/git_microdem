unit lcp_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordLCP}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics,Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,Vcl.ComCtrls;

type
  TLCP_form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    BitBtn10: TBitBtn;
    BitBtn3: TBitBtn;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CreateCostSurfaceBitBtn1: TBitBtn;
    Edit9: TEdit;
    RouteStartBitBtn4: TBitBtn;
    CreatePathSurfaceBitBtn5: TBitBtn;
    StatusBar1: TStatusBar;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    RouteEndBitBtn6: TBitBtn;
    Label10: TLabel;
    Edit10: TEdit;
    CreateRouteCostBitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    RoadNetworkBitBtn9: TBitBtn;
    Edit11: TEdit;
    Label11: TLabel;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CreateCostSurfaceBitBtn1Click(Sender: TObject);
    procedure CreatePathSurfaceBitBtn5Click(Sender: TObject);
    procedure RouteStartBitBtn4Click(Sender: TObject);
    procedure RouteEndBitBtn6Click(Sender: TObject);
    procedure CreateRouteCostBitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure RoadNetworkBitBtn9Click(Sender: TObject);
    procedure CheckLCPSettings;
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure LeastCostPathOptions(Mode : integer = 0);


implementation

{$R *.dfm}

uses
   Petmar,Petmar_db,Petmar_types,
   DEMDefs, DEMDef_routines,
   Least_Cost_Path,
   Nevadia_Main,
   demdbtable,
   DEMmapf,
   DEMCoord,
   DEM_Manager,
   DEMDatabase;


procedure LeastCostPathOptions(Mode : integer = 0);
var
  LCP_form: TLCP_form;
begin
   {$IfDef RecordLCP} WritelineToDebugFile('Enter LeastCostPathOptions'); {$EndIf}
   StopSplashing;
   LCP_form := TLCP_form.Create(Application);
   LCP_form.Show;
   if (Mode=1) then LCP_form.CreateRouteCostBitBtn7Click(Nil);
   {$IfDef RecordLCP} WritelineToDebugFile('Exit LeastCostPathOptions'); {$EndIf}
end;


procedure TLCP_form.CreateRouteCostBitBtn7Click(Sender: TObject);
var
   GISStart,GISend,i,rc,NumDEMs,NumStarts : integer;
   Started : shortstring;
   Lat,Long : float64;
   fName : PathStr;
   AccumCostDEMs : array[1..100] of integer;
begin
   {$IfDef RecordLCP} WritelineToDebugFile('Enter TLCP_form.CreateRouteCostBitBtn7Click'); {$EndIf}
   CheckLCPSettings;
   CloseAllDEMs;
   if not FileExists(MDDef.LCPStartfName) then RouteStartBitBtn4Click(Sender);
   if not FileExists(MDDef.LCPendfName) then RouteEndBitBtn6Click(Sender);

   if OpenNumberedGISDataBase(GISStart, MDDef.LCPStartfName) and OpenNumberedGISDataBase(GISend, MDDef.LCPendfName) then begin
      {$IfDef RecordLCP} WritelineToDebugFile('Opened ' + MDDef.LCPStartfName + ' & ' + MDDef.LCPendfName); {$EndIf}
      NumDEMs := 0;
      NumStarts := 0;
      Started := TimeToStr(now);
      rc := GISdb[GISstart].MyData.RecordCount;
      while not GISdb[GISstart].MyData.Eof do begin
         inc(NumStarts);

         if MDDef.LCP_LeastCost then begin
            inc(NumDEMS);
            fName := LCP_AccumCostfName(GISstart);
            LoadNewDEM(AccumCostDEMs[NumDEMs],fName,false,'accum cost surf');
            DEMglb[AccumCostDEMs[NumDEMs]].AreaName := 'P' + IntToStr(NumStarts) + '_cost';

            inc(NumDEMS);
            fName := LCP_AccumDistancefName(GISstart);
            LoadNewDEM(AccumCostDEMs[NumDEMs],fName,false,'accum dist surf');
            DEMglb[AccumCostDEMs[NumDEMs]].AreaName := 'P' + IntToStr(NumStarts) + '_dist';
         end;

         if MDDef.LCP_ShortestDistance then begin
             inc(NumDEMS);
             fName := LDP_AccumCostfName(GISstart);
             LoadNewDEM(AccumCostDEMs[NumDEMs],fName,false,'accum cost surf');
             DEMglb[AccumCostDEMs[NumDEMs]].AreaName := 'P' + IntToStr(NumStarts) + '_sh_cost';

             inc(NumDEMS);
             fName := LDP_AccumDistancefName(GISstart);
             LoadNewDEM(AccumCostDEMs[NumDEMs],fName,false,'accum dist surf');
             DEMglb[AccumCostDEMs[NumDEMs]].AreaName := 'P' + IntToStr(NumStarts) + '_sh_dist';
         end;

         GISdb[GISstart].MyData.Next;
      end;
      {$IfDef RecordLCP} WritelineToDebugFile('while loop over'); {$EndIf}
      GISdb[GISend].AddAndFillFieldFromDEM(adElevAllGrids,'',2);
      for i := 1 to NumDEMs do CloseSingleDEM(AccumCostDEMs[i]);
      {$IfDef RecordLCP} WritelineToDebugFile('completed AddAndFillFieldFromDEM'); {$EndIf}

      GISdb[GISstart].MyData.First;
      i := 0;
      while not GISdb[GISstart].MyData.Eof do begin
         if GISdb[GISstart].MyData.ValidLatLongFromTable(Lat,Long) then begin
            inc(i);
            StatusBar1.Panels[0].Text := IntToStr(i) + '/' + IntToStr(Rc) + '  ' + GISdb[GISstart].MyData.GetFieldByNameAsString(MDDef.PrecintField) + '  ' + started;
            {$IfDef RecordLCP} WritelineToDebugFile('distances i=' + IntToStr(i)); {$EndIf}
            GISdb[GISend].DistAllRecsToPoint(Lat,Long,1,'P' + IntToStr(i));
         end;
         GISdb[GISstart].MyData.Next;
      end;

      CloseAndNilNumberedDB(GISstart);
      CloseAndNilNumberedDB(GISend);
      StatusBar1.Panels[0].Text := '';
   end;
   {$IfDef RecordLCP} WritelineToDebugFile('Exit TLCP_form.CreateRouteCostBitBtn7Click'); {$EndIf}
end;


procedure TLCP_form.BitBtn8Click(Sender: TObject);
begin
   {$IfDef RecordLCP} WritelineToDebugFile('enter TLCP_form.BitBtn8Click (one step)'); {$EndIf}
   CheckLCPSettings;
   CreateCostSurfaceBitBtn1Click(Sender);
   {$IfDef RecordLCP} WritelineToDebugFile('TLCP_form.BitBtn8Click done cost surface'); {$EndIf}
   CreatePathSurfaceBitBtn5Click(Sender);
   {$IfDef RecordLCP} WritelineToDebugFile('TLCP_form.BitBtn8Click done create path'); {$EndIf}
   CreateRouteCostBitBtn7Click(Sender);
   {$IfDef RecordLCP} WritelineToDebugFile('exit TLCP_form.BitBtn8Click (one step)'); {$EndIf}
end;


procedure TLCP_form.RoadNetworkBitBtn9Click(Sender: TObject);
begin
   GetExistingFileName('Road network shapefile','Shapefile|*.shp',MDDef.LCPRoadfName);
   Edit11.Text := MDDef.LCPRoadfName;
end;

procedure TLCP_form.BitBtn10Click(Sender: TObject);
begin
   Close;
end;

procedure TLCP_form.CreateCostSurfaceBitBtn1Click(Sender: TObject);
begin
   {$IfDef RecordLCP} WritelineToDebugFile('enter TLCP_form.CreateCostSurfaceBitBtn1Click'); {$EndIf}
   StatusBar1.Panels[0].Text := 'Creating cost surface';
   CheckLCPSettings;
   SaveBackupDefaults;
   if not FileExists(MDDef.LCPRoadfName) then RoadNetworkBitBtn9Click(Sender);
   StatusBar1.Panels[0].Text := 'Calling CreateCostPathSurface';
   CreateCostPathSurface(StatusBar1);
   RestoreBackupDefaults;
   StatusBar1.Panels[0].Text := '';
   {$IfDef RecordLCP} WritelineToDebugFile('exit TLCP_form.CreateCostSurfaceBitBtn1Click'); {$EndIf}
end;

procedure TLCP_form.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\least_cost_path\lcp.htm');
end;

procedure TLCP_form.RouteStartBitBtn4Click(Sender: TObject);
begin
   GetExistingFileName('Least cost path, starting locations','Database files|*.dbf',MDDef.LCPStartfName);
   Edit9.Text := MDDef.LCPStartfName;
end;

procedure TLCP_form.CreatePathSurfaceBitBtn5Click(Sender: TObject);
var
   GISNum,i,rc : integer;
   Started : shortstring;
begin
   {$IfDef RecordLCP} WritelineToDebugFile('Enter TLCP_form.CreatePathSurfaceBitBtn5Click'); {$EndIf}
   CheckLCPSettings;
   if not FileExists(MDDef.LCPStartfName) then RouteStartBitBtn4Click(Sender);
   if OpenNumberedGISDataBase(GISNum, MDDef.LCPStartfName) then begin
      i := 0;
      Started := TimeToStr(now);
      rc := GISdb[GISnum].MyData.RecordCount;
      while not GISdb[GISnum].MyData.Eof do begin
         inc(i);
         StatusBar1.Panels[0].Text := IntToStr(i) + '/' + IntToStr(Rc) + '  ' + GISdb[GISnum].MyData.GetFieldByNameAsString(MDDef.PrecintField) + '  ' + started;
         {$IfDef RecordLCP} WritelineToDebugFile(StatusBar1.Panels[0].Text); {$EndIf}
         LeastCostFromCurrentRecord(GISNum);
         GISdb[GISnum].MyData.Next;
      end;
      CloseAndNilNumberedDB(GISnum);
      StatusBar1.Panels[0].Text := '';
   end;
   {$IfDef RecordLCP} WritelineToDebugFile('Exit TLCP_form.CreatePathSurfaceBitBtn5Click'); {$EndIf}
end;


procedure TLCP_form.RouteEndBitBtn6Click(Sender: TObject);
begin
   GetExistingFileName('Least cost path, ending locations','Database files|*.dbf',MDDef.LCPEndfName);
   Edit10.Text := MDDef.LCPEndfName;
end;


procedure TLCP_form.FormCreate(Sender: TObject);
begin
   Edit1.Text := IntToStr(MDdef.ImpossibleCost);
   Edit2.Text := IntToStr(MDdef.MaxImpossibleInPath);
   Edit3.Text := IntToStr(MDdef.BufferCost);
   Edit4.Text := IntToStr(MDdef.BufferRounds);
   Edit5.Text := IntToStr(MDdef.CostSurfResolution);
   Edit6.Text := IntToStr(MDdef.StartFree);
   Edit7.Text := MDdef.PrecintField;
   Edit9.Text := MDDef.LCPStartfName;
   Edit10.Text := MDDef.LCPEndfName;
   Edit11.Text := MDDef.LCPRoadfName;
   if MDDef.LCPdist then RadioGroup1.ItemIndex := 0
   else RadioGroup1.ItemIndex := 1;
   CheckBox1.Checked := MDDef.LCPdiagonals;
   CheckBox2.Checked := MDDef.LCPsavecost;
   CheckBox3.Checked := MDDef.LCPsavedir;
   CheckBox4.Checked := MDDef.LCPsavedist;
   CheckBox5.Checked := MDDef.LCPOverwrite;
   CheckBox6.Checked := MDDef.LCP_ShortestDistance;
   CheckBox7.Checked := MDDef.LCP_LeastCost;
end;


procedure TLCP_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CheckLCPSettings;
end;


procedure TLCP_form.CheckBox6Click(Sender: TObject);
begin
   MDDef.LCP_ShortestDistance := CheckBox6.Checked;
end;

procedure TLCP_form.CheckBox7Click(Sender: TObject);
begin
   MDDef.LCP_LeastCost := CheckBox7.Checked;
end;

procedure TLCP_form.CheckLCPSettings;
begin
   CheckEditString(Edit1.Text,MDdef.ImpossibleCost);
   CheckEditString(Edit2.Text,MDdef.MaxImpossibleInPath);
   CheckEditString(Edit3.Text,MDdef.BufferCost);
   CheckEditString(Edit4.Text,MDdef.BufferRounds);
   CheckEditString(Edit5.Text,MDdef.CostSurfResolution);
   CheckEditString(Edit6.Text,MDdef.StartFree);
   MDdef.PrecintField := Edit7.Text;
   MDDef.LCPdist := RadioGroup1.ItemIndex = 0;
   MDDef.LCPdiagonals := CheckBox1.Checked;
   MDDef.LCPsavecost := CheckBox2.Checked;
   MDDef.LCPsavedir := CheckBox3.Checked;
   MDDef.LCPsavedist := CheckBox4.Checked;
   MDDef.LCPOverwrite := CheckBox5.Checked;
end;


initialization
finalization
end.
