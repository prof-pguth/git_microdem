unit grid_over_map;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define TrackAtlas}
   //{$Define TrackAtlasSubset}
   //{$Define TrackAtlasScattergram}
{$EndIf}



interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
   FireDAC.Stan.ExprFuncs,
   FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
   FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
   FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
   FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
   FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
   dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
   DBClient,
   {$EndIf}
//end

  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms, Dialogs, Buttons,ClipBrd,
  System.Math,
  Petmar_types,DEMMapf, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls;

const
   MaxGeomorpFilters = 5;

type
  TGridOverlayonMap = class(TForm)
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Label5: TLabel;
    ComboBox3: TComboBox;
    Edit5: TEdit;
    Edit6: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    BitBtn3: TBitBtn;
    Label14: TLabel;
    BitBtn4: TBitBtn;
    ComboBox4: TComboBox;
    Label18: TLabel;
    Edit10: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    Edit11: TEdit;
    HistBitBtn: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    BitBtn6: TBitBtn;
    StatusBar1: TStatusBar;
    Label23: TLabel;
    ComboBox5: TComboBox;
    Edit7: TEdit;
    Label24: TLabel;
    Edit12: TEdit;
    Label25: TLabel;
    CheckBox5: TCheckBox;
    GroupBox1: TGroupBox;
    CheckBox7: TCheckBox;
    Edit8: TEdit;
    Label15: TLabel;
    Label22: TLabel;
    Label16: TLabel;
    Edit9: TEdit;
    Label13: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GroupBox2: TGroupBox;
    ComboBox6: TComboBox;
    BitBtn1: TBitBtn;
    CheckBox8: TCheckBox;
    RadioGroup1: TRadioGroup;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label17: TLabel;
    BitBtn2: TBitBtn;
    CheckBox9: TCheckBox;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Label21: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Edit17: TEdit;
    BitBtn5: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Edit18: TEdit;
    Label30: TLabel;
    CheckBox10: TCheckBox;
    BitBtn11: TBitBtn;
    RadioGroup2: TRadioGroup;
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure Edit11Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure HistBitBtnClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure Edit18Change(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
  private
    { Private declarations }
    procedure EraseDEMBitmap(DEM : integer);
    procedure ClearFilterGraphs;
    procedure HideFilterDEMs;
    procedure LabelPercentiles;
    procedure UpdateAtlasDefTable;
    procedure ShowMaskOnMainMap;
    procedure GetMaskBitmap(var fName : PathStr; var BMP : tMyBitmap);
    procedure ChangeComboBox(var TheDEM : integer; TheBox : tComboBox; LowEdit,HighEdit : tEdit; HS : ShortString = ''; LS : ShortString = '');
    function DEMforName(fName: ShortString): integer;
    function ShortBaseName(fName: shortstring): shortstring;
  public
    { Public declarations }
    BaseMap    : tMapForm;
    AtlasDataTable,MaskDEM,ClusterDEM,LastDEMtoUse : integer;
    AtlasDefTable : tMyData;
    f1,f2 : PathStr;
    UseGrids : boolean;
    ClassDEMS : array[1..MaxGeomorpFilters] of integer;
    LowVals,HighVals : array[1..MaxGeomorpFilters] of float64;
  end;


procedure GridOverlayPointsOnMap(theBaseMap : tMapForm; inAtlasDB : integer = 0);

var
   LoadAtlasMap     : boolean;
   InitialSetupOver : boolean;
   AtlasDBDefTableName : PathStr;

implementation

{$R *.dfm}

uses
   {$IfDef ExSat}
   {$Else}
   DEMEros,
   {$EndIf}

   DEMDefs,DEMCoord, DEMDef_Routines, BaseGraf,DEMDataBase,
   PetImage,Petmar,Petimage_form,Petmath,
   DEMStat, Make_grid,
   Make_Tables,
   rgb_colors_three_params,
   PetDBUtils, toggle_db_use,
   BaseMap, dem_manager,Nevadia_main, DEM_indexes,
   DataBaseCreate;


function TGridOverlayonMap.DEMforName(fName : ShortString) : integer;
var
   i : integer;
begin
   for I := 2 to LastDEMtoUse do begin
      if ValidDEM(i) and (DEMGlb[i].AreaName = MDDef.GeomorphNameModifier + fName) then begin
         Result := i;
         exit;
      end;
   end;
   Result := 0;
end;

procedure TGridOverlayonMap.ChangeComboBox(var TheDEM : integer; TheBox : tComboBox; LowEdit,HighEdit : tEdit; HS : ShortString = ''; LS : ShortString = '');
var
   i : integer;
   Min,Max : float64;
begin
   InitialSetupOver := false;
   if UseGrids then begin
      for I := 2 to LastDEMtoUse do begin
         if ValidDEM(i) and (DEMGlb[i].AreaName = MDDef.GeomorphNameModifier + TheBox.Text) then begin
            TheDEM := i;
            if (HS = '') then HS := RealToString(DEMGlb[i].DEMheader.MaxElev,-12,-2);
            if (LS = '') then LS := RealToString(DEMGlb[i].DEMheader.MinElev,-12,-2);
            HighEdit.Text := HS;
            LowEdit.Text := LS;
            LabelPercentiles;
         end;
      end;
   end
   else begin
      AtlasDefTable.ApplyFilter('PARAMETER=' + QuotedStr(TheBox.Text));
      if (AtlasDefTable.RecordCount = 0) then begin
         AtlasDefTable.Insert;
         AtlasDefTable.SetFieldByNameAsString('PARAMETER',TheBox.Text);
         GISDB[AtlasDataTable].FieldRange(TheBox.Text,Min,Max);

         AtlasDefTable.SetFieldByNameAsFloat('MAX_CAT',Max);
         AtlasDefTable.SetFieldByNameAsFloat('MIN_CAT',Min);
         AtlasDefTable.SetFieldByNameAsFloat('MAX_COLOR',Max);
         AtlasDefTable.SetFieldByNameAsFloat('MIN_COLOR',Min);
         AtlasDefTable.Post;
      end;
      HighEdit.Text := AtlasDefTable.GetFieldByNameAsString('MAX_CAT');
      LowEdit.Text := AtlasDefTable.GetFieldByNameAsString('MIN_CAT');
   end;
   InitialSetupOver := true;
   HideFilterDEMs;
end;


procedure GridOverlayPointsOnMap(theBaseMap : tMapForm; inAtlasDB : integer = 0);
var
  GridOverlayonMap : TGridOverlayonMap;
  NewDEM,i,j : integer;
  OldName,fName : PathStr;
  NumerFields : tStringList;
  pName    : shortstring;
  LS,HS : ShortString;


      procedure InitializeComboBox(j : integer; TheBox : tComboBox; LowEdit,HighEdit : tEdit; HS : ShortString = ''; LS : ShortString = '');
      begin
         GridOverlayOnMap.ClassDems[j] := NewDEM;
         TheBox.Text := pName;
         if (HS = '') then HS := RealToString(DEMGlb[j].DEMheader.MaxElev,-12,-2);
         if (LS = '') then LS := RealToString(DEMGlb[j].DEMheader.MinElev,-12,-2);
         HighEdit.Text := HS;
         LowEdit.Text := LS;
      end;

      procedure AddNameToComboBoxes(pName : ShortString);
      begin
          with GridOverlayOnMap do begin
             pName := ptTrim(pName);
             ComboBox1.Items.Add(pName);
             ComboBox2.Items.Add(pName);
             ComboBox3.Items.Add(pName);
             ComboBox4.Items.Add(pName);
             ComboBox5.Items.Add(pName);
             ComboBox6.Items.Add(pName);
             ComboBox7.Items.Add(pName);
             ComboBox8.Items.Add(pName);
             ComboBox9.Items.Add(pName);
          end;
      end;


begin
   {$IfDef TrackAtlas} WriteLineToDebugFile('GridOverlayPointsOnMap in, db= ' + IntToStr(inAtlasDB));{$EndIf}
   OldName := LastDEMName;
   GridOverlayonMap := TGridOverlayonMap.Create(Application);
   GridOverlayonMap.UseGrids := (inAtlasDB = 0);
   with GridOverlayOnMap do begin
      InitialSetupOver := false;
      BaseMap := theBaseMap;
      for i := 1 to MaxGeomorpFilters do ClassDEMs[i] := 0;
      if UseGrids then begin
         {$IfDef TrackAtlas} WriteLineToDebugFile('GridOverlayPointsOnMap use grids');{$EndIf}
         GetDOSPath('Geomorph atlas dir',GeomorphAtlasDir);
         GridOverlayonMap.Caption := 'Geomorph atlas ' + GeomorphAtlasDir;
         AtlasDBDefTableName := GeomorphAtlasDir + 'srtm_params' + DefaultDBExt;   //Geomorph_Atlas.OpenSRTMGeomorphAtlas(true);
         AtlasDefTable := tMyData.Create(AtlasDBDefTableName);
         toggle_db_use.VerifyRecordsToUse(AtlasDefTable,'PARAMETER');

         AtlasDefTable.ApplyFilter('CLASS=1');
         if (AtlasDefTable.RecordCount = 0) then begin
            AtlasDefTable.ApplyFilter('USE=' + QuotedStr('Y'));
            AtlasDefTable.Edit;
            AtlasDefTable.SetFieldByNameAsInteger('CLASS',1);
            AtlasDefTable.Post;
         end;

         AtlasDefTable.ApplyFilter('USE=' + QuotedStr('Y'));
         while not AtlasDefTable.eof do begin
             pName := ptTrim(AtlasDefTable.GetFieldByNameAsString('PARAMETER'));
             AddNameToComboBoxes(pName);
             j := AtlasDefTable.GetFieldByNameAsInteger('CLASS');
             fName := GeomorphAtlasDir + MDDef.GeomorphNameModifier + pName + '.DEM';
             if not FileExists(fName) then begin
                MDDef.GeomorphNameModifier := '';
                fName := GeomorphAtlasDir + MDDef.GeomorphNameModifier + pName + '.DEM';
             end;
            {$IfDef TrackAtlas}
            WriteLineToDebugFile('Load ' + fName);
            {$EndIf}
             NewDEM := 0;
             LoadNewDEM(NewDem,fName,LoadAtlasMap);
             if j in [1..5] then GridOverlayOnMap.ClassDems[j] := NewDEM;
             if AtlasDefTable.FieldExists('WEIGHT') then VariableWeightings[NewDEM] := AtlasDefTable.GetFieldByNameAsFloat('WEIGHT');
             (*
             AtlasDefTable.Edit;
             if AtlasDefTable.GetFieldByNameAsString('MAX_CAT') = '' then AtlasDefTable.SetFieldByNameAsFloat('MAX_CAT',DEMGlb[NewDEM].HeadRecs.MaxElev);
             if AtlasDefTable.GetFieldByNameAsString('MAX_COLOR') = '' then AtlasDefTable.SetFieldByNameAsFloat('MAX_COLOR',DEMGlb[NewDEM].HeadRecs.MaxElev);
             if AtlasDefTable.GetFieldByNameAsString('MIN_CAT') = '' then AtlasDefTable.SetFieldByNameAsFloat('MIN_CAT',DEMGlb[NewDEM].HeadRecs.MinElev);
             if AtlasDefTable.GetFieldByNameAsString('MIN_COLOR') = '' then AtlasDefTable.SetFieldByNameAsFloat('MIN_COLOR',DEMGlb[NewDEM].HeadRecs.MinElev);
             AtlasDefTable.Post;

             HS := AtlasDefTable.GetFieldByNameAsString('MAX_CAT');
             LS := AtlasDefTable.GetFieldByNameAsString('MIN_CAT');
             *)
             HS := RealToString(DEMGlb[NewDEM].DEMheader.MaxElev,-12,-2);
             LS := RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2);

             if j=1 then InitializeComboBox(1,ComboBox1,Edit2,Edit1,hs,ls);
             if j=2 then InitializeComboBox(2,ComboBox2,Edit3,Edit4,hs,ls);
             if j=3 then InitializeComboBox(3,ComboBox3,Edit6,Edit5,hs,ls);
             if j=4 then InitializeComboBox(4,ComboBox4,Edit11,Edit10,hs,ls);
             if j=5 then InitializeComboBox(5,ComboBox5,Edit12,Edit7,hs,ls);
             AtlasDefTable.Next;
         end;
         LastDEMtoUse := NewDEM;
         LastDEMName := OldName;
      end
      else begin
         //AtlasDBDataTableName := 'C:\mapdata\0--current_projects\geomorph_dbs\etopo1_15min' + DefaultDBExt;
         //AtlasDBDefTableName := 'C:\mapdata\0--current_projects\geomorph_dbs\etopo1_30min_params' + DefaultDBExt;
         //AtlasDefTableName := AtlasDBDefTableName;
         //theBaseMap.OpenDBonMap('',AtlasDBDataTableName);
         AtlasDBDefTableName := GISDB[inAtlasDB].DBAuxDir + ExtractFileNameNoExt(GISDB[inAtlasDB].DBFullName) + '_classes.dbf';
         if not FileExists(AtlasDBDefTableName) then MakeAtlasParametersTable(AtlasDBDefTableName);
         AtlasDefTable := tMyData.Create(AtlasDBDefTableName);
         //GISdb[db].LayerIsOn := false;
         GridOverlayonMap.Caption := 'Geomorph atlas ' + ExtractFilename(GISDB[inAtlasDB].DBName);
         theBaseMap.DoFastMapRedraw;
         AtlasDataTable := inAtlasDB;
         //PetdbUtils.GetFields(GISdb[db].MyData,GISdb[db].dbOpts.VisCols,[ftFloat],NumerFields,false,true);
         for i := 0 to Pred(NumerFields.Count) do AddNameToComboBoxes(NumerFields.Strings[i]);
         NumerFields.Free;
         Label7.Visible := false;
         Label8.Visible := false;
         Label9.Visible := false;
         Label19.Visible := false;
         Label25.Visible := false;
         Edit8.Enabled := false;
         Edit9.Enabled := false;
         HistBitBtn.Enabled := false;
         BitBtn6.Enabled := false;
         BitBtn8.Enabled := false;
      end;

      CheckBox1.Checked := ComboBox1.Text <> '';
      CheckBox2.Checked := ComboBox2.Text <> '';
      CheckBox3.Checked := ComboBox3.Text <> '';
      CheckBox4.Checked := ComboBox4.Text <> '';
      CheckBox5.Checked := ComboBox5.Text <> '';
      Edit8.Text := IntToStr(MDDef.GemorphAtlasFilterSize);
      Edit9.Text := IntToStr(MDDef.GemorphAtlasMatchNeed);

      Edit8Change(Nil);

      {$IfDef TrackAtlas} WriteLineToDebugFile('Edit8Change complete'); {$EndIf}

      Label14.Caption := DEMGlb[TheBaseMap.MapDraw.DEMonMap].AreaName;
      {$IfDef TrackAtlas} WriteLineToDebugFile('HideFilterDEMs complete'); {$EndIf}
      InitialSetupOver := true;
      if UseGrids then LabelPercentiles;
      HideFilterDEMs;
      Show;
   end;
end;


procedure TGridOverlayonMap.ClearFilterGraphs;
begin
   if InitialSetupOver then begin
      DeleteFileIfExists(f1);
      DeleteFileIfExists(f2);
   end;
end;


procedure TGridOverlayonMap.EraseDEMBitmap(DEM : integer);
begin
   if UseGrids and InitialSetupOver then begin
      DeleteFileIfExists(GeomorphAtlasDir + DEMGlb[DEM].AreaName + '.bmp');
      ClearFilterGraphs;
   end;
end;


procedure TGridOverlayonMap.HideFilterDEMs;
var
   i : integer;
begin
   if InitialSetupOver then begin
      ComboBox1.Enabled := CheckBox1.Checked;
      Edit1.Enabled := CheckBox1.Checked;
      Edit2.Enabled := CheckBox1.Checked;

      ComboBox2.Enabled := CheckBox2.Checked;
      Edit3.Enabled := CheckBox2.Checked;
      Edit4.Enabled := CheckBox2.Checked;

      ComboBox3.Enabled := CheckBox3.Checked;
      Edit5.Enabled := CheckBox3.Checked;
      Edit6.Enabled := CheckBox3.Checked;

      ComboBox4.Enabled := CheckBox4.Checked;
      Edit10.Enabled := CheckBox4.Checked;
      Edit11.Enabled := CheckBox4.Checked;

      ComboBox5.Enabled := CheckBox5.Checked;
      Edit7.Enabled := CheckBox5.Checked;
      Edit12.Enabled := CheckBox5.Checked;
      ClearFilterGraphs;

      RadioGroup1.Items.Clear;
      RadioGroup1.Items.Add('Merge');

      for I := 1 to MaxGeomorpFilters do begin
         {$IfDef TrackAtlas} if i <> 0 then WriteLineToDebugFile(IntToStr(i) + '   DEM=' + IntToStr(ClassDems[i])); {$EndIf}
         if (ClassDEMs[i] <> 0) and (DEMGLB[ClassDEMs[i]] <> Nil) then begin
            RadioGroup1.Items.Add(ShortBaseName(DEMGlb[ClassDems[i]].AreaName));
         end;
      end;

      if CheckBox7.Checked then RadioGroup1.Items.Add('Region filter');
      RadioGroup1.Columns := 3;
      RadioGroup1.ItemIndex := -1;
   end;
end;


function MakeDEMMask(DEM : integer; Min,Max : float64; var NumPts : integer) : tMyBitmap;
var
   x,y : integer;
   z  : float32;
   i : integer;
   p0 : pRGB;
   fName : PathStr;
begin
   if (DEM=0) or (DEMGlb[DEM] = Nil) then begin
      exit;
   end;
   
   fName := GeomorphAtlasDir + DEMGlb[DEM].AreaName + '.bmp';
   {$IfDef TrackAtlas} WriteLineToDebugFile('MakeDEMMask for ' + DEMGlb[DEM].AreaName + '  fName = ' + fName + '  Range = ' + RealToString(Min,8,2) + ' to ' + RealToString(Max,8,2)); {$EndIf}
   CreateBitmap(Result,DEMGlb[DEM].DEMheader.NumCol,DEMGlb[DEM].DEMheader.NumRow);
   if FileExists(fName) then begin
      Result.LoadFromFile(fName);
   end
   else begin
      NumPts := 0;
      if abs(Max-Min) > 0.00000001 then begin
         StartProgressAbortOption('Map ' + DEMGlb[DEM].AreaName);
         y := 0;
         while y <= pred(DEMGlb[DEM].DEMheader.NumRow) do begin
            if (y mod 50 = 0) then UpDateProgressBar(y/pred(DEMGlb[DEM].DEMheader.NumRow));
            p0 := Result.ScanLine[pred(DEMGlb[DEM].DEMheader.NumRow)-y];
            x := 0;
            while x <= pred(DEMGlb[DEM].DEMheader.NumCol) do begin
               for i := 1 to MaxGeomorpFilters do begin
                  if (DEMGlb[DEM].GetElevMeters(x,y,z) and (z >= Min) and (z <=Max)) then begin
                     p0[x] := rgbTripleRed;
                     inc(NumPts);
                  end;
               end;
               inc(x);
            end;
            if WantOut then break;
            inc(y);
         end;
      end;

      Result.SaveToFile(fName);
      {$IfDef ExSat}
      {$Else}
      with DEMGlb[DEM],DEMheader do begin
         SaveWorldFile(FName,DEMxSpacing,DEMySpacing,DEMSWcornerLong,DEMSWcornerLat + LatSizeMap,'WGS84',1,'N');
      end;
      {$EndIf}

      EndProgress;
   end;
   {$IfDef TrackAtlas} WriteLineToDebugFile('MakeDEMMask out'); {$EndIf}
end;


procedure TGridOverlayonMap.BitBtn11Click(Sender: TObject);
var
   DEM,NewDEM : integer;
   NormName : PathStr;
begin
    for DEM := 2 to MaxDEMDataSets do if (DEMGlb[DEM] <> Nil) then begin
       NormName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'NORM_' + ExtractFileName(DEMGlb[DEM].DEMFileName);
       if not FileExists(NormName) then begin
           Newdem := MakeSingleNewDerivativeMap('n',DEM);
           DEMGlb[NewDEM].WriteNewFormatDEM(NormName);
           CloseSingleDEM(NewDEM);
       end;
    end;
end;

procedure TGridOverlayonMap.BitBtn12Click(Sender: TObject);
begin
   ClusterGrids(2,MaxDEMDataSets);
end;

procedure TGridOverlayonMap.BitBtn1Click(Sender: TObject);
var
   minv,maxv : float64;
begin
   AtlasDefTable.ApplyFilter('PARAMETER=' + QuotedStr(ComboBox6.Text));
   minv := AtlasDefTable.GetFieldByNameAsFloat('MIN_COLOR');
   maxv := AtlasDefTable.GetFieldByNameAsFloat('MAX_COLOR');
   ReadDefault('Min value for coloring',minv);
   ReadDefault('Max value for coloring',maxv);
   AtlasDefTable.Edit;
   AtlasDefTable.SetFieldByNameAsFloat('MIN_COLOR',minv);
   AtlasDefTable.SetFieldByNameAsFloat('MAX_COLOR',maxv);
   AtlasDefTable.Post;
   if UseGrids then begin
      ComboBox6Change(Sender);
   end;
end;

procedure TGridOverlayonMap.BitBtn2Click(Sender: TObject);
var
   SlopeGrid,ConvexGrid,RoughGrid,i,y,xg,yg,Cat,Total : integer;
   Slope,Convex,Rough,Lat,Long : float64;
   bmp : tMyBitmap;
   SlopeField,ConvexField,RoughField : ShortString;
   Hist : array[1..16] of Integer;
   pc : array[1..16] of float64;
begin
   if (ComboBox7.text <> '') and (ComboBox8.text <> '') and (ComboBox9.text <> '') then begin

      CheckEditString(Edit13.Text,MDDef.SlopeCut1);
      CheckEditString(Edit14.Text,MDDef.SlopeCut2);
      CheckEditString(Edit15.Text,MDDef.SlopeCut3);
      CheckEditString(Edit16.Text,MDDef.ConvexCut);
      CheckEditString(Edit17.Text,MDDef.RoughnessCut);

      for i := 1 to 16 do Hist[i] := 0;
      Total := 0;
      for i := 1 to 16 do Total := Total + Hist[i];
      if UseGrids then begin
         SlopeGrid := DEMforName(ComboBox7.Text);
         ConvexGrid := DEMforName(ComboBox8.Text);
         RoughGrid := DEMforName(ComboBox9.Text);

        //Make_grid.CreateIwashishiPikeMap(DEMGlb[BaseMap.MapDraw.DEMonMap].AreaName,SlopeGrid,RoughGrid,ConvexGrid);


         (*
         ConvexityMeanCut := DEMGLB[ConvexGrid].FindPercentileElevation(MDDef.ConvexCut);
         TextureMeanCut := DEMGLB[RoughGrid].FindPercentileElevation(MDDef.RoughnessCut);
         SlopeMeanCut := DEMGLB[SlopeGrid].FindPercentileElevation(MDDef.SlopeCut1);
         SlopeQuarterCut := DEMGLB[SlopeGrid].FindPercentileElevation(MDDef.SlopeCut2);
         SlopeEigthCut := DEMGLB[SlopeGrid].FindPercentileElevation(MDDef.SlopeCut3);
         CloneImageToBitmap(BaseMap.Image1,bmp);
         StartProgress('Classify');
         for y := 0 to pred(bmp.Height) do begin
            p0 := bmp.ScanLine[y];
            if (y mod 1000 = 0) then UpdateProgressBar(y/bmp.Height);

            for x := 0 to pred(bmp.Width) do begin
               BaseMap.MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
               DEMGlb[SlopeGrid].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
               if DEMGlb[SlopeGrid].GetElevMeters(xg,yg,Slope) then begin
                  Convex := DEMGlb[ConvexGrid].GridElevMeters(xg,yg);
                  Rough := DEMGlb[RoughGrid].GridElevMeters(xg,yg);
                  p0[x] := IwashuriPikeColor(Slope,Convex,Rough,Cat);
                  inc(Hist[Cat]);
               end;
            end;
         end;
         *)

      end
      else begin
         SlopeField := ComboBox7.Text;
         ConvexField := ComboBox8.Text;
         RoughField := ComboBox9.Text;
         ConvexityMeanCut := GISdb[AtlasDataTable].GetFieldPercentile(ConvexField,MDDef.ConvexCut);
         TextureMeanCut := GISdb[AtlasDataTable].GetFieldPercentile(RoughField,MDDef.RoughnessCut);
         SlopeMeanCut := GISdb[AtlasDataTable].GetFieldPercentile(SlopeField,MDDef.SlopeCut1);
         SlopeQuarterCut := GISdb[AtlasDataTable].GetFieldPercentile(SlopeField,MDDef.SlopeCut2);
         SlopeEigthCut := GISdb[AtlasDataTable].GetFieldPercentile(SlopeField,MDDef.SlopeCut3);
         GISdb[AtlasDataTable].EmpSource.Enabled := false;
         CloneImageToBitmap(BaseMap.Image1,bmp);
         StartProgress('Classify');
         y := 0;
         GISdb[AtlasDataTable].MyData.First;
         repeat
            inc(y);
            if (y mod 1000 = 0) then UpdateProgressBar(y/GISdb[AtlasDataTable].MyData.RecordCount);
            Slope := GISdb[AtlasDataTable].MyData.GetFieldByNameAsFloat(SlopeField);
            Convex := GISdb[AtlasDataTable].MyData.GetFieldByNameAsFloat(ConvexField);
            Rough := GISdb[AtlasDataTable].MyData.GetFieldByNameAsFloat(RoughField);
            GISdb[AtlasDataTable].ValidLatLongFromTable(Lat,Long);
            BaseMap.MapDraw.LatLongDegreeToScreen(Lat,Long,xg,yg);
            Petmar.ScreenSymbol(bmp.Canvas,xg,yg,FilledBox,GISdb[AtlasDataTable].dbOpts.Symbol.Size,IwashuriPikeColor(Slope,Convex,Rough,Cat));
            inc(Hist[Cat]);
            GISdb[AtlasDataTable].MyData.Next;
         until GISdb[AtlasDataTable].MyData.EOF;
         GISdb[AtlasDataTable].EmpSource.Enabled := true;
      end;

      if CheckBox9.Checked then BaseMap.IHSmergeOntoMap(BMP)
      else begin
         BaseMap.Image1.Picture.Graphic := bmp;
         bmp.Free;
      end;

      EndProgress;

      Total := 0;
      for i := 1 to 16 do Total := Total + Hist[i];
      for i := 1 to 16 do begin
         pc[i] := 100 * Hist[i] / Total;
         Memo1.Lines.Add('Cat' + IntegerToString(i,3) +
         RealToString(pc[i],8,2) + '%');
      end;
      IandPLegend(pc);
   end;
end;


procedure TGridOverlayonMap.BitBtn3Click(Sender: TObject);
var
   CurDEM : integer;
begin
   GetDEM(CurDEM);
   if (DEMGlb[CurDEM].SelectionMap <> Nil) then BaseMap := DEMGlb[CurDEM].SelectionMap;
   Label14.Caption := DEMGlb[CurDEM].AreaName;
   DEMGlb[CurDEM].SelectionMap.BringToFront;
end;


procedure TGridOverlayonMap.BitBtn4Click(Sender: TObject);
var
   Bitmap,bm2 : tMyBitmap;
   p0 : pRGB;
   TheFilter : ANSIstring;
   TotalRecs : integer;


   procedure MakeFilteredMap;
   var
      x,y,xp,yp,AreaNum,Matches,Count,Miss : integer;
      OverHalf : boolean;
      ps : array[-MaxFilterSize..MaxFilterSize] of prgb;
   begin
      Matches := 0;
      AreaNum := sqr(succ(2*MDDef.GemorphAtlasFilterSize));
      OverHalf := 2 * MDDef.GemorphAtlasMatchNeed > AreaNum;
      StartProgressAbortOption('Region filter');
      for y := MDDef.GemorphAtlasMatchNeed to pred(DEMGlb[ClassDems[1]].DEMheader.NumRow - MDDef.GemorphAtlasFilterSize) do begin
         if (y mod 1000 = 0) then UpDateProgressBar(y/DEMGlb[ClassDems[1]].DEMheader.NumRow);
         for x := -MDDef.GemorphAtlasFilterSize to MDDef.GemorphAtlasFilterSize do ps[x] := Bitmap.ScanLine[y+x];
         p0 := BM2.ScanLine[y];
         for x := MDDef.GemorphAtlasFilterSize to pred(DEMGlb[ClassDems[1]].DEMheader.NumCol - MDDef.GemorphAtlasFilterSize) do begin
             Count := 0;
             Miss := 0;
             for yp := -MDDef.GemorphAtlasFilterSize to MDDef.GemorphAtlasFilterSize do begin
                for xp := -MDDef.GemorphAtlasFilterSize to MDDef.GemorphAtlasFilterSize do begin
                   if SameColor(ps[yp][x+xp],rgbTripleRed) then inc(Count) else inc(Miss);
                end;
                if OverHalf then begin
                   if (Count >= MDDef.GemorphAtlasMatchNeed) then break;
                end
                else begin
                   if Miss > (AreaNum - MDDef.GemorphAtlasMatchNeed) then break;
                end;
             end;
             if (Count >= MDDef.GemorphAtlasMatchNeed) then begin
                p0[x] := rgbTripleRed;
                inc(Matches);
             end;
         end;
      end;
      Memo1.Lines.Add('Regions: ' + IntToStr(Matches));
      Petimage.SaveBitmap(bm2,f2);
      {$IfDef ExSat}
      {$Else}
      with DEMGlb[ClassDems[1]],DEMheader do begin
         SaveWorldFile(F2,DEMxSpacing,DEMySpacing,DEMSWcornerLong,DEMSWcornerLat + LatSizeMap,'WGS84',1,'N');
      end;
      {$EndIf}
      Bitmap.Free;
    end;

      procedure MakeAllMaps;
          var
         x,y,LowG,HighG,LeftG,RightG : integer;
         i : integer;
         NumPts : array[0..MaxGeomorpFilters] of integer;
         Bitmap : array[0..MaxGeomorpFilters] of tMyBitmap;
         p0,p1 : pRGB;
      begin
         BaseMap.DoFastMapRedraw;
         with BaseMap,MapDraw do begin
            LeftG := 0;
            RightG := pred(DEMGlb[ClassDems[1]].DEMheader.NumCol);
            LowG := 0;
            HighG := pred(DEMGlb[ClassDems[1]].DEMheader.NumRow);
            ShowHourglassCursor;
            if FileExists(f1) then begin
               CreateBitmap(Bitmap[0],DEMGlb[ClassDems[1]].DEMheader.NumCol,DEMGlb[ClassDems[1]].DEMheader.NumRow);
               Bitmap[0].LoadFromFile(f1);
            end
            else begin
               Bitmap[0] := Nil;
               for i := 1 to MaxGeomorpFilters do begin
                  if ((i=1) and CheckBox1.Checked and (ComboBox1.Text <> '')) or ((i=2) and CheckBox2.Checked and (ComboBox2.Text <> '')) or ((i=3) and CheckBox3.Checked and (ComboBox3.Text <> '')) or
                     ((i=4) and CheckBox4.Checked and (ComboBox4.Text <> '')) or ((i=5) and CheckBox5.Checked and (ComboBox5.Text <> '')) then begin
                        Bitmap[i] := MakeDEMMask(ClassDems[i],LowVals[i],HighVals[i],NumPts[i]);
                        if (Bitmap[0] = Nil) then begin
                          CreateBitmap(Bitmap[0],DEMGlb[ClassDems[1]].DEMheader.NumCol,DEMGlb[ClassDems[1]].DEMheader.NumRow);
                          Bitmap[0].Canvas.Draw(0,0,Bitmap[i]);
                        end
                        else begin
                           y := LowG;
                           while y <= HighG do begin
                              p0 := Bitmap[0].ScanLine[y];
                              if Bitmap[i] <> Nil then begin
                                 p1 := Bitmap[i].ScanLine[y];
                                 x := LeftG;
                                 while x <= RightG do begin
                                    if SameColor(p0[x],rgbTripleRed) and (not SameColor(p1[x],rgbTripleRed)) then begin
                                       p0[x] := rgbTripleWhite;
                                       Inc(NumPts[0]);
                                    end;
                                    inc(x);
                                 end;
                              end;
                              inc(y);
                           end;
                        end;
                        Bitmap[i].Free;
                  end;
               end;
               PetImage.SaveBitmap(Bitmap[0],f1);
               {$IfDef ExSat}
               {$Else}
               with DEMGlb[ClassDems[1]],DEMheader do begin
                  SaveWorldFile(F1,DEMxSpacing,DEMySpacing,DEMSWcornerLong,DEMSWcornerLat + LatSizeMap,'WGS84',1,'N');
               end;
               {$EndIf}
           end;
         end;
         NumPts[0] := 0;
         y := LowG;
         while y <= HighG do begin
            p0 := Bitmap[0].ScanLine[y];
            x := LeftG;
            while x <= RightG do begin
               if SameColor(p0[x],rgbTripleRed) then Inc(NumPts[0]);
               inc(x);
            end;
            inc(y);
         end;
         Bitmap[0].Free;
         ShowDefaultCursor;
         if CheckBox1.Checked then Memo1.Lines.Add(ComboBox1.Text + '  ' + Label7.Caption);
         if CheckBox2.Checked then Memo1.Lines.Add(ComboBox2.Text + '  ' + Label8.Caption);
         if CheckBox3.Checked then Memo1.Lines.Add(ComboBox3.Text + '  ' + Label9.Caption);
         if CheckBox4.Checked then Memo1.Lines.Add(ComboBox4.Text + '  ' + Label19.Caption);
         if CheckBox5.Checked then Memo1.Lines.Add(ComboBox5.Text + '  ' + Label25.Caption);
         Memo1.Lines.Add('Matches: '  + IntToStr(NumPts[0]));
      end;



            function PCString(i : integer) : shortstring;
            begin
               Result := ' (' + RealToString(100.0 * i / TotalRecs,-8,-2) + '%)';
            end;



      procedure UpDateFilter(CheckBox : tCheckBox; ComboBox : tComboBox; Edit1,Edit2 : tEdit);
      var
        pf : Ansistring;
        recs : integer;
      begin
         if CheckBox.Checked then begin
            if (TheFilter <> '') then TheFilter := TheFilter + ' AND ';
            pf := ComboBox.Text + '<=' + Edit1.Text + ' AND ' + ComboBox.Text + '>=' + Edit2.Text;
            GISdb[AtlasDataTable].ApplyGISFilter(pf);
            Recs := GISdb[AtlasDataTable].MyData.RecordCount;
            Memo1.Lines.Add(ComboBox.Text + '=' + IntToStr(Recs) + PCString(Recs));
            TheFilter := TheFilter + pf;
         end;
      end;


begin
   BM2 := nil;
   Memo1.Lines.Clear;
   if UseGrids then begin
      MakeAllMaps;
      if CheckBox7.Checked and (not FileExists(f2)) then begin
         CreateBitmap(Bitmap,DEMGlb[ClassDems[1]].DEMheader.NumCol,DEMGlb[ClassDems[1]].DEMheader.NumRow);
         Bitmap.LoadFromFile(f1);
         CreateBitmap(BM2,DEMGlb[ClassDems[1]].DEMheader.NumCol,DEMGlb[ClassDems[1]].DEMheader.NumRow);
         MakeFilteredMap;
      end;
      EndProgress;
      if (BM2 <> Nil) then BM2.Free;
   end
   else begin
      TheFilter := '';
      GISdb[AtlasDataTable].MyData.ApplyFilter('');
      TotalRecs := GISdb[AtlasDataTable].MyData.RecordCount;
      UpDateFilter(CheckBox1,ComboBox1,Edit1,Edit2);
      UpDateFilter(CheckBox2,ComboBox2,Edit4,Edit3);
      UpDateFilter(CheckBox3,ComboBox3,Edit5,Edit6);
      UpDateFilter(CheckBox4,ComboBox4,Edit10,Edit11);
      UpDateFilter(CheckBox5,ComboBox5,Edit7,Edit12);
      GISdb[AtlasDataTable].MyData.ApplyFilter(TheFilter);
      GISdb[AtlasDataTable].dbTablef.ShowStatus;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Total=' + IntToStr(GISdb[AtlasDataTable].MyData.RecordCount) + PCString(GISdb[AtlasDataTable].MyData.RecordCount));
      GISdb[AtlasDataTable].TheMapOwner.MapDraw.ClearGISLayer(AtlasDataTable);
      BaseMap.DoFastMapRedraw;
      GISdb[AtlasDataTable].RedrawLayerOnMap; 
   end;
end;



procedure TGridOverlayonMap.BitBtn5Click(Sender: TObject);
var
   GISNum,DEM,x,y,Npts,skip,LowG,HighG,LeftG,RightG : integer;
   z,AveDev  : float32;
   Lat,Long : float64;
   fName,fName2 : PathStr;
   TrainingSets : boolean;
   Table1,WhereTrainDB : tMyData;
   Mean,Std : array[1..MaxDEMDataSets] of float32;
   CreateDataBase : tCreateDataBase;


   procedure DoProcess(aName : ShortString);
   label
      DoneInput;
   var
      DEM : integer;
      z64 : float64;
   begin
      {$IfDef TrackAtlasSubset} WriteLineToDebugFile('DoProcess in'); {$EndIf}
      StartProgress('DBF creation');
      x := LeftG;
      while (x <= RightG) do begin
         if (x mod 250 = 0) then UpdateProgressBar((x-leftG) / (RightG - LeftG));
         y := LowG;
         while (y <= HighG) do begin
            if DEMGlb[2].GetElevMeters(x,y,z) then begin
               inc(NPts);
               Table1.Insert;
               For DEM := 2 to LastDEMtoUse do  begin
                  if (DEM = 2) then begin
                    DEMGlb[2].DEMGridToLatLongDegree(x,y,Lat,Long);
                    Table1.SetFieldByNameAsFloat('LAT',Lat);
                    Table1.SetFieldByNameAsFloat('LONG',Long);
                  end;
                  if DEMGlb[DEM].GetElevMeters(x,y,z) then begin
                     if (MDDef.GeomorphNameModifier = '') then z := (z - Mean[DEM]) / std[DEM];
                     z64 := z;
                     Table1.CarefullySetFloat(ShortBaseName(DEMGlb[DEM].AreaName),z64,0.001);
                  end;
               end;
               if (aName <> '') then Table1.SetFieldByNameAsString('AREA',aName);
               Table1.Post;
               if NPts >=  EdburgGeneralFuncsMAXOBSERVATIONS then begin
                  MessageToContinue('Too many points');
                  goto DoneInput;
               end;
            end;
            inc(y,skip);
         end;
         inc(x,skip);
      end;
      DoneInput:;
      EndProgress;
   end;



begin
   {$IfDef TrackAtlasSubset} WriteLineToDebugFile('TGridOverlayonMap.BitBtn5Click in'); {$EndIf}
   fName := MDTempDir + 'atlas_subset' + DefaultDBExt;
    TrainingSets := AnswerIsYes('Training sets');

    CreateDataBase := tCreateDataBase.Create(fName);
    CreateDataBase.AddLatLongToTable;
    if TrainingSets then CreateDataBase.AddAField('AREA',ftstring,24);
    for DEM := 2 to LastDEMtoUse do begin
       CreateDataBase.AddAField(ShortBaseName(DEMGlb[DEM].AreaName),ftFloat,9,4);
       DEMGlb[DEM].ElevationStatistics(Mean[DEM],Std[DEM],AveDev,true);
    end;
    CreateDataBase.AddAField('CLUSTER',ftFloat,3,0);
    CreateDataBase.WriteCorrectHeader;
   Table1 := tMyData.Create(fName);

   Npts := 0;
   Skip := 1;
   if TrainingSets then begin
      {$IfDef TrackAtlasSubset} WriteLineToDebugFile('TrainingSets start'); {$EndIf}
      fName2 := 'C:\mapdata\0--current_projects\dune_fields\training_areas\merged.dbf';
      WhereTrainDB := tMyData.Create(fName2);
      while not WhereTrainDB.eof do begin
         Lat := WhereTrainDB.GetFieldByNameAsFloat('LAT_HI');
         Long := WhereTrainDB.GetFieldByNameAsFloat('LONG_HI');
         DEMGlb[2].LatLongDegreeToDEMGridInteger(Lat,Long,RightG,HighG);

         Lat := WhereTrainDB.GetFieldByNameAsFloat('LAT_LOW');
         Long := WhereTrainDB.GetFieldByNameAsFloat('LONG_LOW');
         DEMGlb[2].LatLongDegreeToDEMGridInteger(Lat,Long,LeftG,LowG);
         DoProcess(WhereTrainDB.GetFieldByNameAsString('NAME'));
         WhereTrainDB.Next;
      end;
      WhereTrainDB.Destroy;
   end
   else begin
      LeftG := 0;
      RightG := pred(DEMGlb[2].DEMheader.NumCol);
      LowG := 0;
      HighG := pred(DEMGlb[2].DEMheader.NumRow);
      while succ(RightG - LeftG) * succ(HighG - LowG) div Skip div skip > EdburgGeneralFuncsMAXOBSERVATIONS do inc(Skip);
      ReadDefault('Skip factor',Skip);
      DoProcess('');
   end;

   Table1.Destroy;
   DEMDataBase.OpenNumberedGISDataBase(GISNum,fName,true,false,DEMGlb[1].SelectionMap);
end;

procedure TGridOverlayonMap.BitBtn6Click(Sender: TObject);
begin
   if AnswerIsYes('Might hang; proceed') then HistBitBtnClick(Sender);
end;

procedure TGridOverlayonMap.BitBtn7Click(Sender: TObject);
var
   Dir : PathStr;
   TheFiles : tStringList;
   i : Integer;
   fName : shortstring;
begin
   Dir := ExtractFilePath(AtlasDefTable.TableName);
   TheFiles := Nil;
   Petmar.FindMatchingFiles(Dir,'*.dem',TheFiles);
   for i := 0 to pred(TheFiles.Count) do begin
      fName := Petmar.ExtractFileNameNoExt(TheFiles.Strings[i]);
      AtlasDefTable.ApplyFilter('PARAMETER=' + QuotedStr(fName));
      if AtlasDefTable.RecordCount = 0 then begin
         AtlasDefTable.Insert;
         AtlasDefTable.SetFieldByNameAsString('PARAMETER',fName);
         AtlasDefTable.SetFieldByNameAsString('USE','N');
         AtlasDefTable.Post;
      end;
   end;
   TheFiles.Free;
end;

procedure TGridOverlayonMap.BitBtn8Click(Sender: TObject);
begin
   if UseGrids then begin
      ClusterDEM := ClusterGrids(2,LastDEMtoUse);
   end
   else begin

   end;
end;


procedure TGridOverlayonMap.BitBtn9Click(Sender: TObject);
var
   NewHeadRecs : tDEMheader;
   Sampler,x,y : integer;
   fName : PathStr;
begin
   if (MaskDEM = 0) then begin
      NewHeadRecs := DEMGlb[2].DEMheader;
      NewHeadRecs.DEMPrecision := SmallIntDEM;

      if not OpenAndZeroNewDEM(true,NewHeadRecs,MaskDEM,'Mask',InitDEMmissing) then exit;
      with DEMGlb[MaskDEM],DEMheader do begin
         //AreaName := 'Mask';
         ShortName := 'Mask';
         DEMheader.ElevUnits := Undefined;
         DefineDEMVariables(true);
      end {with};

     Sampler := 1;
     while (NewHeadRecs.NumCol div Sampler) * (NewHeadRecs.NumRow div Sampler) >  EdburgGeneralFuncsMaxObservations do inc(Sampler);
     ReadDefault('Sampling interval',Sampler);
     x := 0;
     while x < NewHeadRecs.NumCol do begin
         y := 0;
         while y < NewHeadRecs.NumRow do begin
            if not DEMGlb[2].MissingDataInGrid(x,y) then begin
               DEMGlb[MaskDEM].SetGridElevation(x,y,1);
            end {if};
            inc(y,Sampler);
         end {while};
         inc(x,Sampler);
     end {While};
     Memo1.Lines.Add('Mask DEM n=' + IntToStr(DEMGlb[MaskDEM].ValidElevsInDEM));
     fName := ExtractFilePath(AtlasDefTable.TableName) + 'cluster_mask.dem';
     DEMGlb[MaskDEM].WriteNewFormatDEM(fName);
     ShowMaskOnMainMap;
   end {if};
end;


procedure TGridOverlayonMap.ShowMaskOnMainMap;
var
   x,y : integer;
   Lat,Long : float64;
begin
   with DEMGlb[MaskDEM] do begin
     x := 0;
     while x < DEMheader.NumCol do begin
         y := 0;
         while y < DEMheader.NumRow do begin
            if not DEMGlb[MaskDEM].MissingDataInGrid(x,y) then begin
               DEMGlb[MaskDEM].DEMGridtoLatLongDegree(x,y,Lat,Long);
               with DEMGlb[1].SelectionMap do MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,FilledBox,2,claRed);
            end {if};
            inc(y);
         end {while};
         inc(x);
     end {While};
   end;
end;

procedure TGridOverlayonMap.CancelBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TGridOverlayonMap.CheckBox10Click(Sender: TObject);
begin
   MDDef.LabelEachGraph := CheckBox10.Checked;
end;

procedure TGridOverlayonMap.CheckBox1Click(Sender: TObject);
begin
   HideFilterDEMs;
end;

procedure TGridOverlayonMap.CheckBox2Click(Sender: TObject);
begin
  HideFilterDEMs;
end;

procedure TGridOverlayonMap.CheckBox3Click(Sender: TObject);
begin
   HideFilterDEMs;
end;

procedure TGridOverlayonMap.CheckBox4Click(Sender: TObject);
begin
   HideFilterDEMs;
end;

procedure TGridOverlayonMap.CheckBox5Click(Sender: TObject);
begin
   HideFilterDEMs;
end;

procedure TGridOverlayonMap.CheckBox7Click(Sender: TObject);
begin
   HideFilterDEMs;
end;

procedure TGridOverlayonMap.ComboBox1Change(Sender: TObject);
begin
   ChangeComboBox(ClassDems[1],ComboBox1,Edit2,Edit1);
end;

procedure TGridOverlayonMap.ComboBox2Change(Sender: TObject);
begin
   ChangeComboBox(ClassDems[2],ComboBox2,Edit3,Edit4);
end;


procedure TGridOverlayonMap.ComboBox3Change(Sender: TObject);
begin
   ChangeComboBox(ClassDems[3],ComboBox3,Edit6,Edit5);
end;


procedure TGridOverlayonMap.ComboBox4Change(Sender: TObject);
begin
   ChangeComboBox(ClassDems[4],ComboBox4,Edit11,Edit10);
end;

procedure TGridOverlayonMap.ComboBox5Change(Sender: TObject);
begin
   ChangeComboBox(ClassDems[5],ComboBox5,Edit12,Edit7);
end;


procedure TGridOverlayonMap.ComboBox6Change(Sender: TObject);
var
   i ,x,y : integer;
   MaskBMP,overlayBMP : tMyBitmap;
   fName : PathStr;
   p0,p1 : PRGB;
begin
   AtlasDefTable.ApplyFilter('PARAMETER=' + QuotedStr(ComboBox6.Text));
   if UseGrids then begin
      for I := 2 to MaxDEMDataSets do begin
         if ValidDEM(i) and (DEMGlb[i].AreaName = MDDef.GeomorphNameModifier + ComboBox6.Text) then begin
            BaseMap.MapDraw.AssignSecondDEM(i);
            UpdateAtlasDefTable;
            AtlasDefTable.ApplyFilter('PARAMETER=' + QuotedStr(ComboBox6.Text));
            BaseMap.DoFastMapRedraw;
            //OverlayBMP := BaseMap.MapDraw.MapLayerFromSecondGrid;
            if CheckBox8.Checked then begin
               MaskBMP := Nil;
               GetMaskBitmap(fName,MaskBMP);
               if (MaskBMP <> Nil) then begin
                  for y := 0 to pred(OverlayBMP.height) do begin
                     p0 := OverlayBMP.ScanLine[y];
                     p1 := MaskBMP.ScanLine[y];
                     for x := 0 to pred(OverlayBMP.Width) do begin
                        if SameColor(p1[x],rgbTripleWhite) then begin
                           p0[x] := rgbTripleWhite;
                        end;
                     end;
                  end;
               FreeAndNil(MaskBMP);
               end;
            end;
            BaseMap.IHSmergeOntoMap(OverlayBMP);
         end;
      end;
   end
   else begin
      GISdb[AtlasDataTable].MyData.ApplyFilter('');
      GISDB[AtlasDataTable].TheMapOwner.MapDraw.ClearGISLayer(AtlasDataTable);
      BaseMap.DoFastMapRedraw;
      GISdb[AtlasDataTable].LimitDBtoMapArea;
      GISdb[AtlasDataTable].PlotFieldOnMap(ComboBox6.Text,AtlasDefTable.GetFieldByNameAsFloat('MIN_COLOR'),AtlasDefTable.GetFieldByNameAsFloat('MAX_COLOR'));
   end;
end;

procedure TGridOverlayonMap.Edit18Change(Sender: TObject);
begin
   CheckEditString(Edit18.Text,MDDef.DefaultGraphXSize);
end;


procedure TGridOverlayonMap.Edit10Change(Sender: TObject);
begin
   CheckEditString(Edit10.Text,HighVals[4]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[4]);
end;

procedure TGridOverlayonMap.Edit11Change(Sender: TObject);
begin
   CheckEditString(Edit11.Text,LowVals[4]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[4]);
end;

procedure TGridOverlayonMap.Edit12Change(Sender: TObject);
begin
   CheckEditString(Edit12.Text,LowVals[5]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[5]);
end;

procedure TGridOverlayonMap.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,HighVals[1]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[1]);
end;

procedure TGridOverlayonMap.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,LowVals[1]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[1]);
end;

procedure TGridOverlayonMap.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,LowVals[2]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[2]);
end;

procedure TGridOverlayonMap.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,HighVals[2]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[2]);
end;

procedure TGridOverlayonMap.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,HighVals[3]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[3]);
end;

procedure TGridOverlayonMap.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,LowVals[3]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[3]);
end;

procedure TGridOverlayonMap.LabelPercentiles;

   procedure MakeLabel(Which : tLabel; DEM : integer);
   begin
      if ClassDems[DEM] <> 0 then                                 
          Which.Caption := RealToString(DEMGlb[ClassDems[DEM]].PercentileofElevation(LowVals[DEM]),-18,-2) + '--' +
               RealToString(DEMGlb[ClassDems[DEM]].PercentileofElevation(HighVals[DEM]),-18,-2) + '%';
   end;

begin
   if InitialSetupOver then begin
      MakeLabel(Label7,1);
      MakeLabel(Label8,2);
      MakeLabel(Label9,3);
      MakeLabel(Label19,4);
      MakeLabel(Label25,5);
   end;
end;


procedure TGridOverlayonMap.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,HighVals[5]);
   LabelPercentiles;
   EraseDEMBitmap(ClassDems[5]);
end;

procedure TGridOverlayonMap.Edit8Change(Sender: TObject);
var
   i : integer;
begin
   if InitialSetupOver then DeleteFileIfExists(f2);

   CheckEditString(Edit8.Text,MDDef.GemorphAtlasFilterSize);
   CheckEditString(Edit9.Text,MDDef.GemorphAtlasMatchNeed);

   i := succ(2*MDDef.GemorphAtlasFilterSize);
   Label22.Caption := FilterSizeStr(i) + '  (' + IntToStr(i*i) + ')';
   Label13.Caption := IntToStr(Round(100*MDDef.GemorphAtlasMatchNeed/i/i)) + '%';
end;

procedure TGridOverlayonMap.Edit9Change(Sender: TObject);
begin
   Edit8Change(Sender);
end;


function TGridOverlayonMap.ShortBaseName(fName : shortstring) : shortstring;
begin
   Result := fName;
     if (Length(MDDef.GeomorphNameModifier) > 0) and (Copy(Result,1,Length(MDDef.GeomorphNameModifier)) = MDDef.GeomorphNameModifier) then begin
         Delete(Result,1,Length(MDDef.GeomorphNameModifier));
      end;
end;

procedure TGridOverlayonMap.UpdateAtlasDefTable;
var
   I : integer;

   procedure UpDateParam(Name : shortstring; i : integer);
   begin
      Name := ShortBaseName(Name);
      AtlasDefTable.ApplyFilter('PARAMETER=' + QuotedStr(Name));
      if (AtlasDefTable.RecordCount > 0) then begin
         AtlasDefTable.Edit;
         AtlasDefTable.SetFieldByNameAsFloat('MAX_CAT',HighVals[i]);
         AtlasDefTable.SetFieldByNameAsFloat('MIN_CAT',LowVals[i]);
         AtlasDefTable.SetFieldByNameAsInteger('CLASS',i);
         AtlasDefTable.Post;
      end;
   end;

begin
  if UseGrids then begin
      for I := 1 to MaxGeomorpFilters do begin
         if ClassDems[i] in [1..5] then UpDateParam(ptTrim(DEMGlb[ClassDems[i]].AreaName),i);
      end;
  end
  else begin
     UpDateParam(ComboBox1.Text,1);
     UpDateParam(ComboBox2.Text,2);
     UpDateParam(ComboBox3.Text,3);
     UpDateParam(ComboBox4.Text,4);
     UpDateParam(ComboBox5.Text,5);
  end;
end;


procedure TGridOverlayonMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if AtlasDefTable <> Nil then begin
      AtlasDefTable.ApplyFilter('');
      while not AtlasDefTable.eof do begin
         AtlasDefTable.Edit;
         AtlasDefTable.SetFieldByNameAsInteger('CLASS',0);
         AtlasDefTable.Post;
         AtlasDefTable.Next;
      end;
      UpdateAtlasDefTable;
      AtlasDefTable.Destroy;
   end;
   DEM_Manager.CloseAllDEMs;
end;


procedure TGridOverlayonMap.FormCreate(Sender: TObject);
begin
   MaskDEM := 0;
   ClusterDEM := 0;
   WMDEM.FormPlacementInCorner(Self);
   Edit13.Text := RealToString(MDDef.SlopeCut1,-12,-1);
   Edit14.Text := RealToString(MDDef.SlopeCut2,-12,-1);
   Edit15.Text := RealToString(MDDef.SlopeCut3,-12,-1);
   Edit16.Text := RealToString(MDDef.ConvexCut,-12,-1);
   Edit17.Text := RealToString(MDDef.RoughnessCut,-12,-1);
   Edit18.Text := IntToStr(MDDef.DefaultGraphXSize);
   CheckBox10.Checked := MDDef.LabelEachGraph;
   f1 := GeomorphAtlasDir + 'classify.bmp';
   f2 := GeomorphAtlasDir + 'large_regions.bmp';
   AtlasDefTable := Nil;
end;


procedure TGridOverlayonMap.HistBitBtnClick(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   Graph : array[0..40] of TThisBaseGraph;
   AllGraphBitmap,lbmp,lbmp2 : tMyBitmap;
   xdrawspot,yDrawspot : integer;


         procedure MakeBigGraph;
         var
            i,j : integer;
         begin
            SaveBackupDefaults;
            MDDef.DefaultGraphFont.Size := 10;
            MDDef.DefaultGraphXSize := 400;
            MDDef.DefaultGraphYSize := 300;
            j := 0;
               for i := 2 to LastDEMtoUse do begin
                  Graph[j] := DEMGlb[i].CreateWholeDEMHistogram;  //(f1,f2);
                  inc(j);
               end;

            PetImage.CreateBitmap(AllGraphBitmap,(3*MDDef.DefaultGraphXSize) + 50,((succ(j div 3))*MDDef.DefaultGraphYSize) + 25);
            AllGraphBitmap.Canvas.Font.Style := [fsBold];
            AllGraphBitmap.Canvas.Font.Size := 10;
            {$IfDef TrackAtlas}
            WriteLineToDebugFile('AllGraphBitmap=' + IntToStr(AllGraphBitmap.Width) + 'x' + IntToStr(AllGraphBitmap.Height));
            {$EndIf}

            ApplicationProcessMessages;
            for i := 0 to pred(j) do begin
               lBMP := nil;
               CopyImageToBitmap(Graph[i].Image1,lbmp);
               xdrawspot := 25 + i mod 3 * MDDef.DefaultGraphXSize;
               ydrawspot := 25 + i div 3 * MDDef.DefaultGraphYSize;
               AllGraphBitmap.Canvas.Draw(xdrawspot,ydrawspot,lbmp);
               FreeAndNil(lBmp);
               Graph[i].Close;
            end;
         end;


         procedure MakeScatterPlot;
         var
            aGraph,aGraph2 : TThisBaseGraph;
            DemList : tStringList;
            i,xp,yp,xg,yg,XDem,Ydem : integer;
            zx,zy,zc : float32;
            MaskBMP : tMyBitmap;
            TStr : ShortString;
            p1s,p0s,p0 : tScreenRGB;
            rgbTripleGray,trip : TRGBTriple;
            Colors : tVATColors;

              procedure SetUpGraph(var theGraph : TThisBaseGraph; xaxisDEM,YaxisDEM : integer; var abmp : tMyBitmap; var p : tScreenRGB);
              var
                 y : integer;
              begin
                 theGraph := TThisBaseGraph.Create(Application);
                 with theGraph,GraphDraw do begin
                     MinHorizAxis := DEMGlb[xaxisDEM].FindPercentileElevation(MDDef.MinElevPercentile);
                     MaxHorizAxis := DEMGlb[xaxisDEM].FindPercentileElevation(MDDef.MaxElevPercentile);
                     if MDDef.LabelEachGraph then HorizLabel := DEMGlb[xaxisDEM].AreaName;

                     MinVertAxis := DEMGlb[yaxisDEM].FindPercentileElevation(MDDef.MinElevPercentile);
                     MaxVertAxis := DEMGlb[yaxisDEM].FindPercentileElevation(MDDef.MaxElevPercentile);
                     if MDDef.LabelEachGraph then VertLabel := DEMGlb[yaxisDEM].AreaName;
                     //GraphDraw.DrawLowAxisValues := false;
                     SetUpGraphForm;
                     CopyImageToBitmap(theGraph.Image1,abmp);
                     for y := 0 to pred(abmp.Height) do p[y] := abmp.ScanLine[y];
                  end;
              end;

         begin
            rgbTripleGray := ConvertTColorToPlatformColor(clSilver);

            if (ClusterDEM <> 0) then DEMGlb[ClusterDEM].LoadColorVATTable(Colors);

            SaveBackupDefaults;
            MDDef.DefaultGraphFont.Size := 10;

            MDDef.DefaultGraphYSize := MDDef.DefaultGraphXSize;

            PetImage.CreateBitmap(AllGraphBitmap,(pred(LastDEMtoUse)*MDDef.DefaultGraphXSize) + 25,(pred(LastDEMtoUse)*MDDef.DefaultGraphYSize) + 5);
            AllGraphBitmap.Canvas.Font.Style := [fsBold];
            AllGraphBitmap.Canvas.Font.Size := 10;

            DemList := tStringList.Create;

            MaskBMP := nil;

            for xDEM := 2 to LastDEMtoUse do begin
               {$IfDef TrackAtlasScattergram} WriteLineToDebugFile('Start scattergrams for ' + IntToStr(xcDEM)); {$EndIf}
               for yDEM := 2 to LastDEMtoUse do begin
                  Self.StatusBar1.Panels[0].Text := IntToStr(YDEM) + '/' + IntToStr(LastDEMtoUse);
                  SetUpGraph(aGraph,xDEM,YDEM,lbmp,p0s);
                  if (xdem <> ydem) then begin
                     SetUpGraph(aGraph2,yDEM,xDEM,lbmp2,p1s);
                     for xp := 0 to pred(DEMGlb[xDEM].DEMheader.NumCol) do begin
                        for yp := 0 to pred(DEMGlb[xDEM].DEMheader.NumRow) do begin
                           if DEMGlb[xDEM].GetElevMeters(xp,yp,zx) and DEMGlb[yDEM].GetElevMeters(xp,yp,zy) then begin
                              xg := aGraph.GraphDraw.GraphX(zx);
                              yg := aGraph.GraphDraw.GraphY(zy);
                              if aGraph.GraphDraw.PtOnGraph(xg,yg) then begin

                                 Trip := rgbTripleGray;
                                 if (ClusterDEM <> 0) and DEMGlb[ClusterDEM].GetElevMeters(xp,yp,zc) then Trip := Colors[round(zc)];

                                 p0s[yg][xg] := Trip;

                                 xg := aGraph2.GraphDraw.GraphX(zy);
                                 yg := aGraph2.GraphDraw.GraphY(zx);
                                 if aGraph2.GraphDraw.PtOnGraph(xg,yg) then p1s[yg][xg] := Trip;
                              end;
                           end;
                        end;
                     end;
                  end;

                  if (MaskBMP <> Nil) then begin
                    for yp := 0 to pred(DEMGlb[xDEM].DEMheader.NumRow) do begin
                       P0[yp] := MaskBMP.ScanLine[pred(DEMGlb[xDEM].DEMheader.NumRow) - yp];
                       for xp := 0 to pred(DEMGlb[xDEM].DEMheader.NumCol) do begin
                          if SameColor(p0[yp][xp],rgbTripleRed) and DEMGlb[xDEM].GetElevMeters(xp,yp,zx) and DEMGlb[yDEM].GetElevMeters(xp,yp,zy) then begin
                             xg := aGraph.GraphDraw.GraphX(zx);
                             yg := aGraph.GraphDraw.GraphY(zy);
                             p0s[yg][xg] := rgbTripleRed;
                             if (xdem <> ydem) then begin
                                xg := aGraph2.GraphDraw.GraphX(zy);
                                yg := aGraph2.GraphDraw.GraphY(zx);
                                p1s[yg][xg] := rgbTripleRed;
                             end;
                          end;
                       end;
                    end;
                  end;

                  xdrawspot := 5 + (xdem-2) * MDDef.DefaultGraphXSize;
                  ydrawspot := 5 + (ydem-2) * MDDef.DefaultGraphYSize;
                  AllGraphBitmap.Canvas.Draw(xdrawspot,ydrawspot,lbmp);
                  FreeAndNil(lBmp);
                  aGraph.Close;

                  if (xdem <> ydem) then begin
                     xdrawspot := 5 + (ydem-2) * MDDef.DefaultGraphXSize;
                     ydrawspot := 5 + (xdem-2) * MDDef.DefaultGraphYSize;
                     AllGraphBitmap.Canvas.Draw(xdrawspot,ydrawspot,lbmp2);
                     FreeAndNil(lBmp2);
                     aGraph2.Close;
                  end;
                  //CloseSingleDEM(yDEM);
               end;
               //CloseSingleDEM(xDEM);
            end;
            if not MDDef.LabelEachGraph then begin
               AllGraphBitmap.Canvas.Font.Size := 12;
               for i := 0 to pred(DEMList.Count) do begin
                  TStr := DEMList.Strings[i];
                  xdrawspot := 0;
                  ydrawspot := 5 + i * MDDef.DefaultGraphYSize +  (MDDef.DefaultGraphYSize - AllGraphBitmap.Canvas.TextWidth(TStr)) Div 2;
                  TextOutVertical(AllGraphBitmap.Canvas,xDrawSpot,YDrawSpot,TStr);
                  ydrawspot := AllGraphBitmap.Height - AllGraphBitmap.Canvas.TextHeight(TStr) - 4;
                  xdrawspot := 5 + i * MDDef.DefaultGraphXSize +  (MDDef.DefaultGraphYSize - AllGraphBitmap.Canvas.TextWidth(TStr)) Div 2;
                  AllGraphBitmap.Canvas.TextOut(xDrawSpot,YDrawSpot,TStr);
               end;
            end;
            DEMList.Free;
            MaskBMP.Free;
            Self.StatusBar1.Panels[0].Text := '';
         end;

begin
   if (Sender = BitBtn6) then MakeScatterPlot
   else MakeBigGraph;
   PetImage_form.DisplayBitmap(AllGraphBitmap,'');
   AllGraphBitmap.Free;
   RestoreBackupDefaults;
{$EndIf}
end;


procedure TGridOverlayonMap.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/atlas_classify.htm');
end;


procedure TGridOverlayonMap.GetMaskBitmap(var fName : PathStr; var BMP : tMyBitmap);
var
   TStr : shortString;
begin
   Bmp := Nil;
   TStr := RadioGroup1.Items[RadioGroup1.ItemIndex];
   if TStr = 'Merge' then fName := f1
   else if TStr = 'Region filter' then begin
      fName := f2;
      if not FileExists(fName) then CheckBox7.Checked := true;
      BitBtn4Click(Nil);
   end
   else fName := GeomorphAtlasDir + MDDef.GeomorphNameModifier + TStr + '.bmp';

   {$IfDef TrackAtlas} WriteLineToDebugFile('TGridOverlayonMap.RadioGroup1Click fname=' + fName); {$EndIf}
   if FileExists(fName) then CopyImageToBitmap(BaseMap.Image1,Bmp);
end;


procedure TGridOverlayonMap.RadioGroup1Click(Sender: TObject);
var
   SummaryBitmap : tMyBitmap;
   fName  : PathStr;
begin
   BaseMap.DoFastMapRedraw;
   GetMaskBitmap(fName,SummaryBitmap);
   if (SummaryBitmap <> Nil) then begin
       BaseMap.MapDraw.DrawWorldFileImageOnMap(SummaryBitmap,FName);
       BaseMap.Image1.Picture.Graphic := SummaryBitmap;
       SummaryBitmap.Free;
   end
   else MessageToContinue('Missing mask file');
end;


procedure TGridOverlayonMap.RadioGroup2Click(Sender: TObject);
begin
   GISDB[AtlasDataTable].dbOpts.Symbol.Size := succ(RadioGroup2.ItemIndex);
end;

initialization
   LoadAtlasMap := false;
finalization
   {$IfDef TrackAtlas} WriteLineToDebugFile('TrackAtlas active in grid_over_map'); {$EndIf}
   {$IfDef TrackAtlasSubset} WriteLineToDebugFile('TrackAtlasSubset active in grid_over_map'); {$EndIf}
   {$IfDef TrackAtlasScattergram} WriteLineToDebugFile('TrackAtlasScattergram active in grid_over_map'); {$EndIf}
end.


