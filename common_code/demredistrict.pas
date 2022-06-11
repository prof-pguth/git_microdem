unit demredistrict;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordNewDistricts}
   //{$Define FillOutDistricts}
   //{$Define RecordRedistrict}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,ClipBrd, Menus,
  Grids, DBGrids, DB,
  Petmar_types,Petmar_db,PETMAR,PetDBUtils, Vcl.ComCtrls;

type
  TRedistrictForm = class(TForm)
    ComboBox1: TComboBox;
    ColorDialog1: TColorDialog;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Image1: TImage;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    PopupMenu1: TPopupMenu;
    Showdistricts1: TMenuItem;
    Showpercentageminoritypopulation1: TMenuItem;
    Showpopulation1: TMenuItem;
    PopupMenu2: TPopupMenu;
    Copylegendtoclipboard1: TMenuItem;
    PopupMenu3: TPopupMenu;
    Clearalldistricts1: TMenuItem;
    Randomizedistricts1: TMenuItem;
    CreateNewDistricts1: TMenuItem;
    Edit1: TEdit;
    Createnewdistrict1: TMenuItem;
    Recolorcurrentdistrict1: TMenuItem;
    Showpopulationdensity1: TMenuItem;
    Assigntodistricts1: TMenuItem;
    Verticalbands1: TMenuItem;
    Horizontalbands1: TMenuItem;
    ShowpercentageHispanicpopulation1: TMenuItem;
    Label1: TLabel;
    Edit2: TEdit;
    CheckBox2: TCheckBox;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    Edit3: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox1: TCheckBox;
    StatusBar1: TStatusBar;
    procedure Recolorcurrentdistrict1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CreateNewDistricts1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Showpopulation1Click(Sender: TObject);
    procedure Showpercentageminoritypopulation1Click(Sender: TObject);
    procedure Showdistricts1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure HelpBtnClick(Sender: TObject);
    procedure Copylegendtoclipboard1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure Clearalldistricts1Click(Sender: TObject);
    procedure Randomizedistricts1Click(Sender: TObject);
    procedure Createnewdistrict1Click(Sender: TObject);
    procedure Showpopulationdensity1Click(Sender: TObject);
    procedure Verticalbands1Click(Sender: TObject);
    procedure Horizontalbands1Click(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure ShowpercentageHispanicpopulation1Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
  private
    { Private declarations }
     procedure Pop0Filter;
     procedure FillOutRedistrictingForm;
     procedure UpdateTable;
  public
    { Public declarations }
    DBonTable,
    LastX,LastY,
    ChangedBlocks,
    TotalPop : integer;
    Colors : tStringList;
    PopThreshhold,
    ColorThreshhold : float64;
    CurrentColor : tColor;
    Closable : boolean;
    ResultsTable: tMyData;
     procedure DistrictsChanged;
     procedure ChangeBlock;
  end;


procedure StartRedistricting(DBonTable : integer);
procedure LegislativeRedistrict(fName : PathStr = ''; Roads : PathStr = ''; CityOutline : PathStr = '');

var
  RedistrictForm : TRedistrictForm;

implementation

{$R *.dfm}

uses
   demdatabase,Make_tables,DEMdefs,
   Map_overlays,
   PetImage,DEMMapF, BaseGraf, DEMdbTable;


procedure LegislativeRedistrict(fName : PathStr = ''; Roads : PathStr = ''; CityOutline : PathStr = '');
var
   BaseDB : integer;
begin
   {$IfDef RecordRedistrict} WriteLineToDebugFile('LegislativeRedistrict in'); {$Endif}
   BaseDB := LoadBlankVectorMapAndOverlay(false,false,fName);
   //BaseDB := LastDBLoaded;
   GISdb[BaseDB].dbOpts.DBLegendLocation.DrawItem := true;
   GISdb[BaseDB].dbOpts.DBAutoShow := dbasColorField;
   if (Roads <> '') then begin
      GISdb[BaseDB].theMapOwner.LoadDataBaseFile(Roads);
   end
   else AddOrSubtractOverlay(GISdb[BaseDB].theMapOwner,ovoTiger,True);
   if (CityOutline <> '') then begin
      GISdb[BaseDB].theMapOwner.LoadDataBaseFile(CityOutline);
   end;
   DEMRedistrict.StartRedistricting(BaseDB);
end;


procedure StartRedistricting(DBonTable : integer);
var
   b,w : float64;
begin
   {$IfDef RecordRedistrict} WriteLineToDebugFile('StartRedistricting in'); {$EndIf}
   RedistrictForm := TRedistrictForm.Create(Application);
   RedistrictForm.DBonTable := DBonTable;
   RedistrictForm.ChangedBlocks := 0;
   RedistrictForm.StatusBar1.Panels[0].Text := '';
   GISdb[DBonTable].Redistricting := true;
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftString,'DISTRICT',12,0);
      AddFieldToDataBase(ftInteger,'COLOR',8,0);
      if Not MyData.FieldExists('AREA_KM2') then begin
         MessageToContinue('Create AREA_KM2 field first');
         RedistrictForm.Close;
         exit;
      end;
      if Not MyData.FieldExists('POP_DENS') then begin
         MessageToContinue('Create POP_DENS field first');
         RedistrictForm.Close;
         exit;
      end;
      if Not MyData.FieldExists('BLACK_PC') then begin
         AddFieldToDataBase(ftFloat,'BLACK_PC',6,2);
         MyData.First;
         while not MyData.EOF do begin
            MyData.Edit;
            b := MyData.GetFieldByNameAsFloat('BLACK1');
            w := MyData.GetFieldByNameAsFloat('WHITE1');
            MyData.SetFieldByNameAsFloat('BLACK_PC',   b / (b + w));
            MyData.Next;
         end;
      end;
      RedistrictForm.Caption := 'Redistricting ' + DBName;
   end;
   RedistrictForm.FillOutRedistrictingForm;
   RedistrictForm.Edit2.Text := IntToStr(MDDef.RedistrictEvenness);
   {$IfDef RecordRedistrict} WriteLineToDebugFile('StartRedistricting out'); {$Endif}
end;


procedure TRedistrictForm.FillOutRedistrictingForm;
var
   DataThere : tStringList;
   i : integer;
   Bitmap : tMyBitmap;
   TStr : ShortString;
   fName : PathStr;
begin
   {$IfDef FillOutDistricts} WriteLineToDebugFile('TRedistrictForm.FillOutRedistrictingForm enter'); {$EndIf}
   TotalPop := Round(GISdb[DBonTable].FieldSum('POP'));
   if (Colors = Nil) then Colors := tStringList.Create;
   GISdb[DBonTable].EmpSource.Enabled := false;
   DataThere := GISdb[DBonTable].MyData.UniqueEntriesInDB('DISTRICT');
   {$IfDef FillOutDistricts} WriteLineToDebugFile('District count=' + IntToStr(DataThere.Count)); {$Endif}
   if (DataThere.Count > RedistrictForm.ComboBox1.Items.Count) then begin
      for i := 0 to pred(DataThere.Count) do begin
         TStr := ptTrim(DataThere.Strings[i]);
         if (TStr <> '') then begin
            RedistrictForm.ComboBox1.Items.Add(TStr);
            GISdb[DBonTable].MyData.ApplyFilter('DISTRICT=' + QuotedStr(TStr));
            Colors.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('COLOR'));
         end;
      end;
   end;
   GISdb[DBonTable].MyData.ApplyFilter('');
   if (DataThere.Count > 0) then begin
      Bitmap := Nil;
      GISdb[DBonTable].RedrawLayerOnMap;
   end;
   Pop0Filter;

   fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'redistrict_' + ExtractFileNameNoExt(GISdb[DBonTable].dbFullName) + DefaultDBExt;
   if (Not FileExists(fName)) then MakeRedistrictingTable(fName);
   RedistrictForm.ResultsTable := tMyData.Create(fName);
   if RedistrictForm.ResultsTable.FieldExists('HISP_PC') then begin
      {$IfDef FillOutDistricts} WriteLineToDebugFile('TRedistrictForm.FillOutRedistrictingForm rename HISP_PC'); {$Endif}
      RedistrictForm.ResultsTable.Destroy;
      RenameDBaseField(fName,'HISP_PC','HISPAN_PC');
      RedistrictForm.ResultsTable := tMyData.Create(fName);
   end;
   RedistrictForm.ResultsTable.AssignEmpSource(DataSource1);
   UpdateTable;
   DataThere.Free;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$IfDef FillOutDistricts} WriteLineToDebugFile('TRedistrictForm.FillOutRedistrictingForm out'); {$Endif}
end;



procedure TRedistrictForm.UpdateTable;
var
   i,err : integer;
   Bitmap : tMyBitmap;
   Using,Height,white,black,Hispanic,Asian,pop,width : integer;
   TStr : ShortString;
   fGoal : float64;

      procedure PostResults(Goal : boolean);
      begin
         Pop := Round(GISdb[DBonTable].FieldSum('POP',false));
         White := Round(GISdb[DBonTable].FieldSum('WHITE1',false));
         Black := Round(GISdb[DBonTable].FieldSum('BLACK1',false));
         Asian := Round(GISdb[DBonTable].FieldSum('ASIAN1',false));
         Hispanic := Round(GISdb[DBonTable].FieldSum('HISPANIC',false));
         if CheckBox2.Checked then Using := White + Black + Asian
         else Using := Pop;

         ResultsTable.Insert;
         ResultsTable.SetFieldByNameAsString('DISTRICT', TStr);
         ResultsTable.SetFieldByNameAsInteger('BLOCKS',GISdb[DBonTable].MyData.RecordCount);
         ResultsTable.SetFieldByNameAsInteger('POP',Pop);
         ResultsTable.SetFieldByNameAsInteger('WHITE1',White);
         ResultsTable.SetFieldByNameAsInteger('BLACK1', Black);
         ResultsTable.SetFieldByNameAsInteger('HISPANIC', Hispanic);
         if Goal then begin
            fGoal := 100 * Pop * ComboBox1.Items.Count / TotalPop;
            ResultsTable.SetFieldByNameAsFloat('GOAL',fGoal);
         end;
         ResultsTable.SetFieldByNameAsFloat('WHITE_PC', 100 * White / Using);
         ResultsTable.SetFieldByNameAsFloat('BLACK_PC', 100 * Black / Using);
         ResultsTable.SetFieldByNameAsFloat('HISPAN_PC', 100 * Hispanic / Using);
         ResultsTable.Post;
      end;

begin
   {$IfDef FillOutDistricts} WriteLineToDebugFile('TRedistrictForm.UpdateTable enter'); {$Endif}
   ResultsTable.First;
   While not ResultsTable.EOF do ResultsTable.Delete;

   GISdb[DBonTable].EmpSource.Enabled := false;

   Height := ComboBox1.Items.Count * 20 + 15;
   CreateBitmap(Bitmap,Image1.Width,Height);
   Bitmap.Canvas.Pen.Color := clSilver;
   Bitmap.Canvas.MoveTo(20,0);   Bitmap.Canvas.LineTo(20,height);
   Bitmap.Canvas.MoveTo(40,0);   Bitmap.Canvas.LineTo(40,Height);
   Bitmap.Canvas.Pen.Color := clBlack;

   for i := 0 to pred(ComboBox1.Items.Count) do begin
      TStr := ptTrim(ComboBox1.Items.Strings[i]);
      if (TStr <> '') then begin
         GISdb[DBonTable].MyData.ApplyFilter('DISTRICT=' + QuotedStr(TStr));
         {$IfDef FillOutDistricts} WriteLineToDebugFile('Filter=' +  GISDB[DBonTable].MyData.Filter); {$EndIf}
         PostResults(true);
      end;
      if (TStr = ComboBox1.Text) then Bitmap.Canvas.Pen.Width := 3;

      Val(Colors[i],CurrentColor,err);
      Width := round(0.01 * 20 * fGoal);
      Bitmap.Canvas.Pen.Width := 1;
      Bitmap.Canvas.Brush.Color := CurrentColor;
      Bitmap.Canvas.Brush.Style := bsSolid;
      Bitmap.Canvas.Rectangle(5,18+i*18,20+Width,18+16+i*18);
      Bitmap.Canvas.Font.Size := 8;
      Bitmap.Canvas.TextOut(7,19+i*18,TStr);
   end;

   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;

   ResultsTable.Insert;
   ResultsTable.SetFieldByNameAsString('DISTRICT', 'Total');
   PostResults(false);
   GISdb[DBonTable].MyData.ApplyFilter('');
   GISdb[DBonTable].EmpSource.Enabled := true;
   RedistrictForm.ResultsTable.AssignEmpSource(DataSource1);
   ResultsTable.First;
   {$IfDef FillOutDistricts} WriteLineToDebugFile('TRedistrictForm.UpdateTable out'); {$EndIf}
end;


procedure TRedistrictForm.Verticalbands1Click(Sender: TObject);
begin
   Randomizedistricts1Click(Sender);
end;

procedure TRedistrictForm.BitBtn1Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      if MyData.Filtered then begin
         if (RedistrictForm.ComboBox1.Text = '') then MessageToContinue('Must pick district')
         else begin
            ShowHourglassCursor;
            MyData.First;
            EmpSource.Enabled := false;
            while not MyData.EOF do begin
               MyData.Edit;
               MyData.SetFieldByNameAsString('DISTRICT',RedistrictForm.ComboBox1.Text);
               MyData.SetFieldByNameAsInteger('COLOR',RedistrictForm.CurrentColor);
               MyData.Next;
            end;
            RedistrictForm.Showdistricts1Click(Nil);
            RedistrictForm.UpdateTable;
            ShowDefaultCursor;
            EmpSource.Enabled := true;
         end;
      end
      else MessageToContinue('Database not filtered');
   end;
end;

procedure TRedistrictForm.BitBtn2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(RecolorRedistrict);
end;


procedure TRedistrictForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TRedistrictForm.CheckBox2Click(Sender: TObject);
begin
   UpdateTable;
end;

procedure TRedistrictForm.Clearalldistricts1Click(Sender: TObject);
begin
   if AnswerIsYes('Confirm clear districts') then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      Pop0Filter;
      GISdb[DBonTable].FillFieldWithValue('DISTRICT','');
      GISdb[DBonTable].FillFieldWithValue('COLOR','');
      GISdb[DBonTable].EmpSource.Enabled := true;
      ComboBox1.Items.Clear;
      ComboBox1.Text := '';
      UpdateTable;
   end;
end;


procedure TRedistrictForm.ComboBox1Change(Sender: TObject);
var
   i,err : integer;
begin
   ResultsTable.First;
   for i := 0 to pred(ComboBox1.Items.Count) do begin
      if (ComboBox1.Items.Strings[i] = ComboBox1.Text) then begin
         BitBtn1.Enabled := true;
         BitBtn2.Enabled := true;
         BitBtn3.Enabled := true;
         UpdateTable;
         Val(Colors[i],CurrentColor,err);
         exit;
      end;
      ResultsTable.Next;
   end;
end;


procedure TRedistrictForm.Copylegendtoclipboard1Click(Sender: TObject);
begin
   Clipboard.Assign(Image1.Picture);
end;

procedure TRedistrictForm.Createnewdistrict1Click(Sender: TObject);
var
   TStr : ShortString;
   NumDist,i : integer;
begin
   {$IfDef RecordNewDistricts} WriteLineToDebugFile('TRedistrictForm.Createnewdistrict1Click in'); {$Endif}
   NumDist := 8;
   ReadDefault('Districts to add',NumDist);
   if (NumDist > 0) then begin
      for i := 1 to NumDist do begin
         TStr := 'D' + IntToStr(i);
            ComboBox1.Items.Add(TStr);
            ComboBox1.Text := TStr;
            if (Sender = Createnewdistrict1) then begin
               if ColorDialog1.Execute then begin
                  CurrentColor := ColorDialog1.Color;
               end;
            end
            else begin
               CurrentColor := WinGraphColors[i];
            end;
            RedistrictForm.Colors.Add(IntToStr(CurrentColor));
      end;
      ComboBox1Change(Sender);
   end;
   {$IfDef RecordNewDistricts}
   WriteLineToDebugFile('TRedistrictForm.Createnewdistrict1Click out');
   {$Endif}
end;

procedure TRedistrictForm.CreateNewDistricts1Click(Sender: TObject);
begin
   Colors.Clear;
   ComboBox1.Items.Clear;
   Createnewdistrict1Click(Sender);
   Randomizedistricts1Click(Sender);
   FillOutRedistrictingForm;
end;


procedure TRedistrictForm.DBGrid1DrawColumnCell(Sender: TObject;  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
   Value : float64;
begin
   if (Column.FieldName = 'GOAL') then begin
      Value := ResultsTable.GetFieldByNameAsFloat('GOAL');
      DBGrid1.Canvas.Font.Color := clBlack;
      if Value < 100 - MDDef.RedistrictEvenness then DBGrid1.Canvas.Font.Color := clRed
      else if Value > 100 + MDDef.RedistrictEvenness then DBGrid1.Canvas.Font.Color := clLime;
      DBGrid1.Canvas.Font.Style := [fsBold];
   end;
   DBGrid1.DefaultDrawColumnCell(Rect,DataCol,Column,State);
end;

procedure TRedistrictForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,ColorThreshhold);
end;

procedure TRedistrictForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.RedistrictEvenness);
   UpdateTable;
end;

procedure TRedistrictForm.Edit3Change(Sender: TObject);
begin
    CheckEditString(Edit3.Text,PopThreshhold);
end;

procedure TRedistrictForm.BitBtn3Click(Sender: TObject);
begin
   ChangeDEMNowDoing(RecolorRedistrictBox);
end;

procedure TRedistrictForm.Pop0Filter;
begin
   GISdb[DBonTable].MyData.ApplyFilter('POP > 0');
end;


procedure TRedistrictForm.RadioGroup1Click(Sender: TObject);
begin
   Edit1.Enabled := (RadioGroup1.ItemIndex in [1,2,4,5]);
   Edit3.Enabled := (RadioGroup1.ItemIndex in [3,4,5]);
   GISdb[DBonTable].RedrawLayerOnMap;
end;

procedure TRedistrictForm.Randomizedistricts1Click(Sender: TObject);
var
   i : integer;
   LatLow,LatHigh,LongLow,LongHigh,Low,Size : float64;

       procedure AssignColor(i : integer);
       begin
          if GISdb[DBonTable].MyData.GetFieldByNameAsString('DISTRICT') = '' then begin
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.SetFieldByNameAsString('COLOR', Colors[pred(i)]);
             GISdb[DBonTable].MyData.SetFieldByNameAsString('DISTRICT', ComboBox1.Items[pred(i)]);
          end;
          GISdb[DBonTable].MyData.Next;
       end;


begin
   if AnswerIsYes('Confirm randomize districts') then begin
      Randomize;
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].FillFieldWithValue('DISTRICT','');
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (Sender = Horizontalbands1) then begin
         LatLow := GISdb[DBonTable].aShapeFile.MainFileHeader.BoundBox.YMin;
         LatHigh := GISdb[DBonTable].aShapeFile.MainFileHeader.BoundBox.YMax;
         Size := (LatHigh-LatLow) / Colors.Count;
         for i := 1 to Colors.Count do begin
            Low := LatLow + Size * i;
            GISdb[DBonTable].MyData.ApplyFilter('LAT_HI <=' + RealToString(Low,-12,-6));
            while not GISdb[DBonTable].MyData.EOF do begin
               AssignColor(i);
            end;
         end;
      end
      else if (Sender = Verticalbands1) then begin
         LongLow := GISdb[DBonTable].aShapeFile.MainFileHeader.BoundBox.XMin;
         LongHigh := GISdb[DBonTable].aShapeFile.MainFileHeader.BoundBox.XMax;
         Size := (LongHigh-LongLow) / pred(Colors.Count);
         for i := 1 to Colors.Count do begin
            Low := LongLow + Size * i;
            GISdb[DBonTable].MyData.ApplyFilter('LONG_HI <=' + RealToString(Low,-12,-6));
            while not GISdb[DBonTable].MyData.EOF do begin
               AssignColor(i);
            end;
         end;
      end
      else begin
          GISdb[DBonTable].MyData.First;
          while not GISdb[DBonTable].MyData.EOF do begin
             for i := 1 to Colors.Count do begin
                AssignColor(i);
                if GISdb[DBonTable].MyData.EOF then break;
             end;
          end;
      end;
      UpdateTable;
      Showdistricts1Click(Sender);
   end;
end;

procedure TRedistrictForm.Recolorcurrentdistrict1Click(Sender: TObject);
var
   i,err : integer;
begin
   if ComboBox1.Text = '' then begin
      MessageToContinue('Select district');
      exit;
   end;
   i := 0;
   while i <= pred(ComboBox1.Items.Count) do begin
      if (ComboBox1.Items.Strings[i] = ComboBox1.Text) then begin
         Val(Colors[i],CurrentColor,err);
         break;
      end
      else inc(i);
   end;
   QueryTColor(CurrentColor);
   Colors[i] := IntToStr(CurrentColor);
   GISdb[DBonTable].MyData.ApplyFilter('DISTRICT=' + QuotedStr(ComboBox1.Text));
   GISdb[DBonTable].EmpSource.Enabled := false;
   while not GISdb[DBonTable].MyData.EOF do begin
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsInteger('COLOR', CurrentColor);
      GISdb[DBonTable].MyData.Next;
   end;
   GISdb[DBonTable].MyData.ApplyFilter('');
   GISdb[DBonTable].EmpSource.Enabled := true;
   UpdateTable;
   Showdistricts1Click(Sender);
end;


procedure TRedistrictForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   GISdb[DBonTable].RedrawLayerOnMap;
   ComboBox1Change(Nil);
   ChangedBlocks := 0;
   StatusBar1.Panels[0].Text := '';
end;

procedure TRedistrictForm.BitBtn4Click(Sender: TObject);
begin
   if (Sender = BitBtn4) then PopUpMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TRedistrictForm.BitBtn5Click(Sender: TObject);
begin
   //HTMLReport(ResultsTable,DataSource1,dbGrid1);
end;


procedure TRedistrictForm.FormCreate(Sender: TObject);
begin
   Petmar.CheckFormPlacement(Self);
   Colors := Nil;
   Closable := false;
   ColorThreshhold := 50;
   PopThreshhold := 250;
end;


procedure TRedistrictForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\red_control.htm');
end;

procedure TRedistrictForm.Horizontalbands1Click(Sender: TObject);
begin
   Randomizedistricts1Click(Sender);
end;

procedure TRedistrictForm.Image1DblClick(Sender: TObject);
var
   District : integer;
begin
   District := ((LastY - 18) div 18);
   ComboBox1.Text := ComboBox1.Items[District];
   ComboBox1Change(Sender);
end;

procedure TRedistrictForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then PopUpMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TRedistrictForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;

procedure TRedistrictForm.BitBtn6Click(Sender: TObject);
begin
   PopUpMenu3.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TRedistrictForm.ChangeBlock;
begin
   inc(ChangedBlocks);
   StatusBar1.Panels[0].Text := 'Blocks changed since last redraw: ' + IntToStr(ChangedBlocks);
end;


procedure TRedistrictForm.DistrictsChanged;
begin
   if ChangedBlocks > 0 then begin
      {$IfDef RecordRedistrict} WriteLineToDebugFile('TRedistrictForm.DistrictsChanged in');    {$Endif}
      ChangedBlocks := 0;
      StatusBar1.Panels[0].Text := '';
      GISdb[DBonTable].RedrawLayerOnMap;
      {$IfDef RecordRedistrict} WriteLineToDebugFile('TRedistrictForm.DistrictsChanged map redrawn'); {$Endif}
      ComboBox1Change(Nil);
      {$IfDef RecordRedistrict} WriteLineToDebugFile('TRedistrictForm.DistrictsChanged out'); {$Endif}
   end;
end;


procedure TRedistrictForm.Showdistricts1Click(Sender: TObject);
begin
   Pop0Filter;
   GISdb[DBonTable].dbOpts.DBAutoShow := dbasColorField;
   GISdb[DBonTable].RedrawLayerOnMap;
end;


procedure TRedistrictForm.ShowpercentageHispanicpopulation1Click(Sender: TObject);
begin
   GISdb[DBonTable].PlotFieldOnMap('HISPAN_PC');
end;

procedure TRedistrictForm.Showpercentageminoritypopulation1Click(Sender: TObject);
begin
   GISdb[DBonTable].PlotFieldOnMap('BLACK_PC');
end;

procedure TRedistrictForm.Showpopulation1Click(Sender: TObject);
begin
   GISdb[DBonTable].PlotFieldOnMap('POP');
end;

procedure TRedistrictForm.Showpopulationdensity1Click(Sender: TObject);
begin
   GISdb[DBonTable].PlotFieldOnMap('POP_DENS');
end;

procedure TRedistrictForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := Closable;
   if not Closable then MessageToContinue('Close Redistricting form by closing DB table ' + GISdb[DBonTable].dbName);
end;

initialization
   RedistrictForm := Nil;
finalization
   {$IfDef RecordNewDistricts} WriteLineToDebugFile('RecordNewDistricts active in DEMredistrict'); {$Endif}
   {$IfDef FillOutDistricts}   WriteLineToDebugFile('FillOutDistricts active in DEMredistrict'); {$EndIf}
end.









