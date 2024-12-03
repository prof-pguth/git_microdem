unit demshowdbrecord;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDEditProblems}
   //{$Define RecordLinkProblems}
   //{$Define RecordCloseShowRecord}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end inline for core DB functions

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,ClipBrd,
  Grids, ComCtrls, ExtCtrls, Menus,
  StdCtrls, Buttons, ToolWin,
  DEMDBTable,PetImage,
  PETMAR,petmar_types, Petdbutils;

type
  Tshowrecordform = class(TForm)
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    WWWBtn1: TBitBtn;
    ColorBitBtn: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn10: TBitBtn;
    PopupMenu1: TPopupMenu;
    Saveimage1: TMenuItem;
    Copyimagetoclipboard1: TMenuItem;
    BitBtn7: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn16: TBitBtn;
    Timer1: TTimer;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    Climographoptions1: TMenuItem;
    BitBtn6: TBitBtn;
    Focalmechanismoptions1: TMenuItem;
    PopupMenu2: TPopupMenu;
    Openprofile1: TMenuItem;
    Copyimagetoclipboard2: TMenuItem;
    Resizeimage1: TMenuItem;
    SpeedButton1: TSpeedButton;
    PopupMenu3: TPopupMenu;
    Copyimagetoclipboard3: TMenuItem;
    Redrawgraph1: TMenuItem;
    BitBtn9: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    Resia1: TMenuItem;
    Popoutgraph1: TMenuItem;
    Popoutgraph2: TMenuItem;
    Focalmechanismsize1: TMenuItem;
    Focalmechanismfill1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    StringGrid1: TStringGrid;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    TabSheet3DProf: TTabSheet;
    Image2: TImage;
    TabSheet4: TTabSheet;
    Memo3: TMemo;
    TabSheet5: TTabSheet;
    Image1: TImage;
    TabSheet6: TTabSheet;
    Memo4: TMemo;
    TabSheet8: TTabSheet;
    Image5: TImage;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WWWBtn1Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure ColorBitBtnClick(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure Copyimagetoclipboard1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Climographoptions1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Focalmechanismoptions1Click(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Copyimagetoclipboard2Click(Sender: TObject);
    procedure Resizeimage1Click(Sender: TObject);
    procedure Openprofile1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Image5DblClick(Sender: TObject);
    procedure Copyimagetoclipboard3Click(Sender: TObject);
    procedure Redrawgraph1Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure Resia1Click(Sender: TObject);
    procedure Popoutgraph1Click(Sender: TObject);
    procedure Popoutgraph2Click(Sender: TObject);
    procedure Image5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Focalmechanismsize1Click(Sender: TObject);
    procedure Focalmechanismfill1Click(Sender: TObject);
  private  { Private declarations }
    LinkFieldThisDB,LinkFieldOtherDB : ShortString;
    Color : tPlatFormColor;
    LatLine,LongLine,
    AnimateInterval,
    ColorLine : integer;
    ShowBlankFields,CanDisplay,PopOutGraph,
    ShowAllFields : boolean;
    LabelField : ShortString;
    FullImageName : PathStr;
    procedure SetSize;
    procedure ClearImage1;
  public  { Public declarations }
     CaptBase : ShortString;
     DBshown : integer;
     CurrentRec_ID : integer;
     procedure ShowRecords(ShowRec : integer = -99);
  end;


var
  FullDBName : PathStr;
  PrevNextButtonsEnabled : boolean;


procedure DisplaySingleDataBaseRecord(TheDB : integer; CanEdit : boolean;  Title : shortString; j : integer; ShowItModal : boolean = true;
    DelayTime : integer = -999; RecordInterval : integer = 1; inLabelField : ShortString = ''); //overload;


implementation

uses
   {$IfDef ExSidescan}
   {$Else}
      SideImg,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      KoppenGr, koppen_opts,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      Beach_Ball_Options,
   {$EndIf}

   {$IfDef ExStereoNet}
   {$Else}
      NetMainW,
   {$EndIf}

   DEMDataBase, DEMESRIShapeFile, DEMCoord,
   BaseGraf,
   DEM_Manager,
   KML_Opts,
   DEMDefs, BaseMap, getlatln, nevadia_main,
   demdef_routines,
   demstringgrid;


{$R *.DFM}



procedure DisplaySingleDataBaseRecord(TheDB : integer; CanEdit : boolean;  Title : shortString; j : integer; ShowItModal : boolean = true;
    DelayTime : integer = -999; RecordInterval : integer = 1; inLabelField : ShortString = '') overload;
var
   showrecordform : Tshowrecordform;

      procedure CheckWWWButton(WWWBtn1 : tBitBtn; WWWFieldName : ShortString);
      begin
         with GISdb[theDB],MyData do begin
            WWWBtn1.Visible := (DBTableF <> Nil) and GISdb[DBTableF.DBonTable].WWWpresent and (WWWFieldName <> '') and (GetFieldByNameAsString(WWWFieldName) <> '');
            if WWWFieldName = 'HYPERLINK'  then WWWBtn1.Caption := 'LINK'
            else WWWBtn1.Caption := Copy(WWWFieldName,5,Length(WWWFieldName)-4);
         end;
      end;

begin
  {$IfDef RecordDEditProblems} WriteLinetoDebugFile('DisplaySingleDataBaseRecord (long one) in');{$EndIf}
   ShowRecordForm := Tshowrecordform.Create(Application);
   with ShowRecordForm do begin
      CaptBase := Title;
      ShowBlankFields := CanEdit;
      LabelField := inLabelField;
      DBshown := theDB;
      SetSize;
      BitBtn1.Visible := CanEdit;
      BitBtn8.Visible := CanEdit and GISdb[DBshown].LatLongFieldsPresent;
      ColorBitBtn.Visible := CanEdit and GISdb[DBshown].MyData.FieldExists('COLOR');
      BitBtn7.Visible := GISdb[DBshown].ItsFanFile;
      BitBtn9.Visible := GISdb[DBshown].FileNameFieldExists;
      BitBtn10.Visible := GISdb[DBshown].FileNameFieldExists;

      if CanEdit then StringGrid1.Options := StringGrid1.Options + [goEditing];

     with GISdb[DBshown] do begin
        if LineOrAreaShapefile(ShapeFileType) then begin
           ColorLineWidthBitBtn(BitBtn6,MDDef.HighLightColor,MDDef.HighlightLineWidth);
        end
        else begin
          SymbolOnButton(BitBtn6,MDDef.HighlightSymbol);
        end;
     end;
      BitBtn4.Visible := GISDb[DBShown].MyData.FieldExists('DEM') and (GISDb[DBShown].MyData.GetFieldByNameAsString('DEM') <> '');

      CheckWWWButton(WWWBtn1,GISdb[DBshown].WWWFieldNames[1]);

      ShowAllFields := false;
      CanDisplay := true;
      ShowRecords(j);

      if TabSheet2.TabVisible then PageControl1.ActivePage := TabSheet2
      else if TabSheet3DProf.TabVisible then PageControl1.ActivePage := TabSheet3DProf
      else if TabSheet4.TabVisible then PageControl1.ActivePage := TabSheet4
      else if TabSheet5.TabVisible then PageControl1.ActivePage := TabSheet5
      else if TabSheet8.TabVisible then PageControl1.ActivePage := TabSheet8
      else PageControl1.ActivePage := TabSheet1;

      if ShowItModal then begin
         if (DelayTime > 0) then begin
            Timer1.Interval := DelayTime;
            AnimateInterval := RecordInterval;
            BitBtn3.Visible := true;
            BitBtn5.Visible := true;
            wmDEM.FormPlacementInCorner(ShowRecordForm,lpSEMap);
            Timer1.Enabled := true;
         end;
         ShowModal;
      end
      else begin
         ShowRecordForm.FormStyle := fsMDIChild;
         ShowRecordForm.Visible := true;
      end;
   end;
  {$IfDef RecordDEditProblems}WriteLineToDebugFile('DisplaySingleDataBaseRecord (long one), out'); {$EndIf}
end;


procedure Tshowrecordform.ShowRecords;
var
   DEMGridSize,
   fLoX,fHiX,fLoY,fHiY,k,i,err : integer;
   xDEMg,yDEMg, xSATg,ySATg : float64;
   xg1,yg1,s1s2,s2s3,Trend,RoughnessFactor,Lat,Long : float64;
   z : float32;
   TStr2 : String;
   TStr : ShortString;
   bmp,bmp2,bmp3 : tMyBitmap;
   FPs  : tStringList;
   OldFilter : shortstring;
   Saved : TTabSheet;
   fName : PathStr;
   Graph : tThisBaseGraph;
   {$IfDef ExGeography}
   {$Else}
      kGraph : KoppenGr.TKoppenGraph;
   {$EndIf}
begin
  if not ValidDB(DBshown) then begin
     MessageToContinue('Database closed');
     exit;
  end;

  CanDisplay := false;
  saved := PageControl1.ActivePage;

  if ShowRec > 0 then begin
     OldFilter := GISDb[DBShown].MyData.Filter;
     GISDb[DBShown].MyData.ApplyFilter(RecNoFName + '=' + IntToStr(ShowRec));
  end;

  CurrentRec_ID := GISDb[DBShown].MyData.GetFieldByNameAsInteger(RecNoFName);

  if (GISDb[DBShown].MyData.RecordCount > 1) then TStr  := IntToStr(GISDb[DBShown].MyData.RecNo) + '/' + IntToStr(GISDb[DBShown].MyData.TotRecsInDB)
  else TStr := '';
  if LabelField = '' then begin
     if GISDb[DBShown].MyData.FieldExists('NAME') then LabelField := 'NAME';
     if GISDb[DBShown].MyData.FieldExists('LOCATION') then LabelField := 'LOCATION';
  end;
  if (LabelField <> '') then TStr := TStr + '  ' + LabelField + '=' + GISDb[DBShown].MyData.GetFieldByNameAsString(LabelField);
  Caption := CaptBase + ' Record ' + Tstr;
  with GISDb[DBShown].MyData do begin
      StringGrid1.Cells[0,0] := 'Field';
      StringGrid1.Cells[1,0] := 'Value';
      k := 1;
      for i := 0 to pred(FieldCount) do if GISDb[DBShown].dbOpts.VisCols[i] or ShowAllFields then begin
         TStr := GetFieldName(i);
         TStr2 := GetFieldByNameAsString(TStr);
         if (TStr = 'LAT') then LatLine := k;
         if (TStr = 'LONG') then LongLine := k;

         if (TStr = 'COLOR') then begin
            ColorLine := k;
            if (TStr2 = '') then Color := ConvertTColorToPlatformColor(clBlack) else Color := ConvertTColorToPlatformColor(StrToInt(TStr2));
            Petmar.ColorBitBtn(ColorBitBtn,Color);
         end;
         if Copy(TStr,1,9) = 'TEXTFILE_' then begin
            if FileExists(TStr2) then begin
               Memo1.Lines.LoadFromFile(TStr2);
               TabSheet2.TabVisible := true;
               TabSheet2.Caption := TStr;
            end;
         end
         else if (GetFieldType(TStr) = ftMemo) then begin
            Memo1.Lines.Add(TStr2);
            TabSheet2.TabVisible := true;
            TabSheet2.Caption := TStr;
         end
         else if (TStr2 <> '') or ShowBlankFields then begin
            StringGrid1.Cells[0,k] := TStr;
            StringGrid1.Cells[1,k] := TStr2;
            inc(k);
         end;
      end;

      if (GISDb[DBShown].LinkTable <> Nil) then begin
         {$IfDef RecordLinkProblems} WriteLineToDebugFile('showing link data in Tshowrecordform.ShowRecords');  {$EndIf}
         if GISDb[DBShown].FindValidJoin(GISDb[DBShown].MyData.GetFieldByNameAsString(LinkFieldThisDB)) then begin
            {$IfDef RecordLinkProblems} WriteLineToDebugFile('Link found for filter=' + LinkData.Filter);  {$EndIf}
            for i := 0 to pred(GISDb[DBShown].LinkTable.FieldCount) do begin
               TStr := GISDb[DBShown].LinkTable.GetFieldName(i);
               TStr2 := GISDb[DBShown].LinkTable.GetFieldByNameAsString(GISDb[DBShown].LinkTable.GetFieldName(i));
               if (TStr2 <> '') or ShowBlankFields then begin
                  StringGrid1.Cells[0,k] := TStr;
                  StringGrid1.Cells[1,k] := TStr2;
                  inc(k);
               end;
            end;
         end
         else begin
            {$IfDef RecordLinkProblems} WriteLineToDebugFile('NO link found for filter=' + LinkData.Filter); {$EndIf}
         end;
      end;

      if MDDef.DB_ID_grids and (not LineOrAreaShapefile(GISdb[dbShown].ShapeFileType)) then begin
          if GISDb[DBShown].ValidLatLongFromTable(Lat,Long) then begin
            for I := 1 to MaxDEMDataSets do begin
               if ValidDEM(i) then begin
                  StringGrid1.Cells[0,k] := DEMGlb[i].AreaName;
                  DEMGlb[i].LatLongDegreeToDEMGrid(Lat,Long,xg1,yg1);
                  DEMGlb[i].DEMGridToLatLongDegree(round(xg1),round(yg1),Lat,Long);
                  if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z) then begin
                     StringGrid1.Cells[1,k] := RealToString(z,-12,-4)  + ' ' + ElevUnitsAre(DEMGlb[i].DEMheader.ElevUnits);
                  end;
                  inc(k);
               end;
            end;
          end;
      end;

      StringGrid1.RowCount := k;

      {$IfDef ExGeography}
         TabSheet5.TabVisible := false;
      {$Else}
         TabSheet5.TabVisible := GISDb[DBShown].MyData.FieldExists('JAN_TEMP') and GISDb[DBShown].MyData.FieldExists('JAN_PRECIP');
         if TabSheet5.TabVisible then begin
            kGraph := OpenKoppenGraph(TabSheet5.Width, TabSheet5.Height);
            Image1.Picture.Graphic := kGraph.Image1.Picture.Graphic;
            if (not PopOutGraph) then kGraph.Close;
         end;
      {$EndIf}

      {$IfDef ExStereoNet}
         TabSheet3.TabVisible := false;
         TabSheet7.TabVisible := TabSheet3.TabVisible;
      {$Else}
         if (GISDb[DBShown].TheMapOwner = nil) then i := 0
         else i := GISDb[DBShown].TheMapOwner.MapDraw.DEMonMap;
      {$EndIf}

      {$IfDef NoDBGrafs}
      {$Else}
         TabSheet8.TabVisible := GISDb[DBShown].TimeSeriesPresent;
         if TabSheet8.TabVisible then begin
            Graph := GISDb[DBShown].Stationtimeseries;
            Graph.ClientWidth := Image5.Width;
            Graph.ClientHeight := Image5.Height;
            Image5.Picture.Graphic := Graph.Image1.Picture.Graphic;
            if (Not PopOutGraph) then Graph.Close;
         end;
      {$EndIf}

      {$IfDef ExGeology}
      {$else}
         if (GISDb[DBShown].DBTableF <> Nil) and GISDb[DBShown].FocalMechsPresent then begin
            bmp := GISDb[DBShown].DrawFocalMechanism(MDDef.NetDef.AllBeachBallSize);
            Image1.Picture.Graphic := bmp;
            bmp.free;
            FPs := GISDb[DBShown].GetFocalPlanes;
            Memo4.Lines.Clear;
            for i := 0 to Pred(FPs.Count) do begin
               Memo4.Lines.Add(FPs.Strings[i]);
            end;
            FPs.Free;
            TabSheet5.TabVisible := true;
            TabSheet6.TabVisible := true;
            TabSheet5.Caption := 'Focal mechanism';
         end;
      {$EndIf}

      if (GISDb[DBShown].DBTableF <> Nil) and (GISDb[DBShown].ShapeFileType in [13,23]) then begin
         TabSheet3DProf.TabVisible := true;
         Graph := GISDb[DBShown].DBTableF.Do3Dshapefileprofile1Click(Nil);
         Graph.ClientWidth := Image2.Width;
         Graph.ClientHeight := Image2.Height;
         CopyImageToBitmap(Graph.Image1,bmp);
         Image2.Picture.Graphic := bmp;
         Graph.Free;
         Bmp.Free;
      end;

      if GISDb[DBShown].MyData.FieldExists(GISDb[DBShown].ImageFieldNames[1]) and (not TabSheet5.TabVisible) then begin
         ClearImage1;
         if GISDb[DBShown].GetRotatedImage(bmp,FullImageName) then begin
            bmp2 := StretchBitmapToFill(bmp,Image1.Width,Image1.Height);
            Image1.Picture.Graphic := bmp2;
            bmp.free;
            bmp2.Free;
            TabSheet5.TabVisible := true;
            if GISDb[DBShown].ValidLatLongFromTable(Lat,Long) then TabSheet5.Caption := 'Image with GPS coordinates'
            else TabSheet5.Caption := 'Image';
            GISDb[DBShown].OutlineCurrentViewOnMap;
         end;
      end;

      GISDb[DBShown].dbtablef.Highlightrecordonmap1Click(nil);
      PageControl1.ActivePage := saved;
      if BitBtn12.Enabled or BitBtn14.Enabled then begin
         BitBtn12.Enabled := not GISDb[DBShown].MyData.EOF;
         BitBtn14.Enabled := not GISDb[DBShown].MyData.Bof;
      end;
   end;
   CanDisplay := true;
   PopOutGraph := false;
   if ShowRec > 0 then begin
      GISDb[DBShown].MyData.ApplyFilter(OldFilter);
   end;
end;


procedure Tshowrecordform.SpeedButton1Click(Sender: TObject);
begin
   ConvertToKML(GISDb[DBShown].dbTablef.DBonTable,'',nil,true);
end;

procedure Tshowrecordform.Timer1Timer(Sender: TObject);
var
   i : integer;
begin
   if (Not GISDb[DBShown].MyData.Eof) then begin
      for I := 1 to AnimateInterval do GISDb[DBShown].MyData.Next;
      ShowRecords;
   end;
end;

procedure Tshowrecordform.Saveimage1Click(Sender: TObject);
begin
  {$IfDef ExImages}
  {$Else}
     PetImage.SaveImageAsBMP(Image1);
  {$EndIf}
end;

procedure Tshowrecordform.SetSize;
var
   k,i : integer;
begin
    BitBtn1.Visible := false;
    BitBtn2.Visible := false;
    BitBtn4.Visible := false;
    BitBtn8.Visible := false;
    BitBtn11.Visible := GISDb[DBShown].MyData.FieldExists('SENSOR_UP') and GISDb[DBShown].MyData.FieldExists('SENSOR_RNG');
    WWWBtn1.Visible := false;

    ColorBitBtn.Visible := false;

    TabSheet2.TabVisible := false;
    TabSheet3DProf.TabVisible := false;
    TabSheet4.TabVisible := false;
    TabSheet5.TabVisible := false;

   k := 1;
   for i := 0 to pred(GISDb[DBShown].MyData.FieldCount) do if GISDb[DBShown].dbOpts.VisCols[i] then inc(k);
   Toolbar1.Visible := BitBtn1.Visible or BitBtn2.Visible or BitBtn8.Visible or  BitBtn10.Visible;
end;


procedure Tshowrecordform.BitBtn10Click(Sender: TObject);
begin
   StringGridToCSVFile('',StringGrid1);
end;

procedure Tshowrecordform.BitBtn11Click(Sender: TObject);
begin
   GISDb[DBShown].dbTablef.Fanproperties1Click(Sender);
   ShowRecords;
end;

procedure Tshowrecordform.BitBtn12Click(Sender: TObject);
begin
   if BitBtn1.Visible then BitBtn1Click(Sender);
   GISDb[DBShown].MyData.Next;
   CurrentRec_ID := GISDb[DBShown].MyData.GetFieldByNameAsInteger(RecNoFName);
   ShowRecords;
end;

procedure Tshowrecordform.BitBtn13Click(Sender: TObject);
begin
   StringGrid1.Font.Size := StringGrid1.Font.Size + 1;
   StringGrid1.DefaultRowHeight := StringGrid1.DefaultRowHeight + 4;
   FormResize(Nil);
   MDDef.DBrecordTextFont.Size := StringGrid1.Font.Size;
   ShowRecords;
end;

procedure Tshowrecordform.BitBtn14Click(Sender: TObject);
begin
   if BitBtn1.Visible then BitBtn1Click(Sender);
   GISDb[DBShown].MyData.Prior;
   CurrentRec_ID := GISDb[DBShown].MyData.GetFieldByNameAsInteger(RecNoFName);
   ShowRecords;
end;

procedure Tshowrecordform.BitBtn15Click(Sender: TObject);
begin
   SingleRecordHTMLReport(false,GISDb[DBShown].MyData,GISDb[DBShown].dbOpts.VisCols);
end;

procedure Tshowrecordform.BitBtn16Click(Sender: TObject);
begin
   GISDb[DBShown].dbTablef.DBGrid1DblClick(Sender);
end;

procedure Tshowrecordform.BitBtn17Click(Sender: TObject);
begin
   GISDb[DBShown].MyData.Edit;
   GISDb[DBShown].MyData.SetFieldByNameAsString('USE','N');
   GISDb[DBShown].MyData.Post;
end;

procedure Tshowrecordform.BitBtn18Click(Sender: TObject);
var
   fName : PathStr;
begin
   if GISDb[DBShown].GetFullImageName(fName) then begin
      if (fname <> FullImageName) then begin
         MessageToContinue('Not active DB record');
      end
      else if FileExists(fName) then begin
         Sysutils.DeleteFile(fName);
         GISDb[DBShown].MyData.SetAllFieldsBlank;
         ClearImage1;
         GISDb[DBShown].MyData.Next;
         ShowRecords;
      end;
   end;
end;


procedure Tshowrecordform.ClearImage1;
var
   bmp2 : tMyBitmap;
begin
   CreateBitmap(bmp2,Image1.Width,Image1.Height);
   Image1.Picture.Graphic := bmp2;
   bmp2.Free;
end;


procedure Tshowrecordform.BitBtn1Click(Sender: TObject);
var
   i : integer;
   TStr : ShortString;
   OldFilter : shortstring;
begin
   OldFilter := GISDb[DBShown].MyData.Filter;
   GISDb[DBShown].EmpSource.Enabled := false;
   GISDb[DBShown].MyData.ApplyFilter(RecNoFName + '=' + IntToStr(CurrentRec_ID));
   GISDb[DBShown].MyData.Edit;
   for i := 0 to pred(GISDb[DBShown].MyData.FieldCount) do begin
     {$IfDef RecordDEditProblems} WriteLineToDebugFile(StringGrid1.Cells[0,succ(i)] +   '   /  ' + StringGrid1.Cells[1,succ(i)]); {$EndIf}
     TStr := StringGrid1.Cells[0,succ(i)];
     if (TStr <> '') and GISDb[DBShown].MyData.FieldExists(TStr) then begin
        GISDb[DBShown].MyData.SetFieldByNameAsString(TStr,StringGrid1.Cells[1,succ(i)]);
     end;
   end;
   GISDb[DBShown].MyData.Post;

   GISDb[DBShown].MyData.ApplyFilter(OldFilter);
   GISDb[DBShown].EmpSource.Enabled := true;

   if (Sender = BitBtn1) then Close;
end;


procedure Tshowrecordform.BitBtn20Click(Sender: TObject);
begin
   DEMStringGrid.SortGrid(StringGrid1,-1,0,true);
end;


procedure Tshowrecordform.BitBtn2Click(Sender: TObject);
begin
   ShowAllFields := true;
   BitBtn2.Visible := false;
   SetSize;
   ShowRecords;
end;


procedure Tshowrecordform.BitBtn3Click(Sender: TObject);
begin
   BitBtn3.Enabled := not BitBtn3.Enabled;
   BitBtn5.Enabled := not BitBtn3.Enabled;
   Timer1.Enabled := BitBtn3.Enabled;
end;

procedure Tshowrecordform.BitBtn5Click(Sender: TObject);
begin
   BitBtn3Click(Sender);
end;


procedure Tshowrecordform.BitBtn4Click(Sender: TObject);
begin
   OpenNewDEM(GISDb[DBShown].MyData.GetFieldByNameAsString('DEM'));
end;


procedure Tshowrecordform.Focalmechanismfill1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      ReadDefault('Focal mechanism fill', MDDef.NetDipFill);
      ShowRecords;
   {$EndIf}
end;

procedure Tshowrecordform.Focalmechanismoptions1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      SetBeachBallOptions(false);
      ShowRecords;
   {$EndIf}
end;

procedure Tshowrecordform.Focalmechanismsize1Click(Sender: TObject);
begin
   {$IfDef ExMICRONET}
   {$Else}
      ReadDefault('Diagram size (1-6)',MDDef.NetDef.NetScreenMult);
      ShowRecords;
   {$EndIf}
end;

procedure Tshowrecordform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   MDDef.DBRecHt := Height;
   MDDef.DBRecWd := Width;
   {$IfDef RecordCloseShowRecord} WriteLineToDebugFile('Tshowrecordform.FormClose   DB filter=' + GISDb[DBShown].MyData.Filter + '   recs=' + IntToStr(GISDb[DBShown].MyData.RecordCount)); {$EndIf}
end;

procedure Tshowrecordform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := true;
end;

procedure Tshowrecordform.FormCreate(Sender: TObject);
begin
   CanDisplay := false;
   PopOutGraph := false;
   Height := MDDef.DBRecHt;
   Width := MDDef.DBRecWd;
   CurrentRec_ID := -99;
   if not MDDef.DBRecShowToolbarTop then Toolbar1.Align := alBottom;
   LoadMyFontIntoWindowsFont(MDDef.DBrecordTextFont,StringGrid1.Font);
end;

procedure Tshowrecordform.FormResize(Sender: TObject);
begin
   StringGrid1.ColWidths[0] := StringGrid1.Canvas.TextWidth('WWWWWWWWWW');
   StringGrid1.ColWidths[1] := ClientWidth - StringGrid1.ColWidths[0];
   StringGrid1.RowHeights[0] := 5 * StringGrid1.Canvas.TextHeight('WWWWWWWWWW') div 4;
   if (CurrentRec_ID <> 0) then ShowRecords(CurrentRec_ID);
end;


procedure Tshowrecordform.Image1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      {$IfDef ExGeography}
         Climographoptions1.Visible := false;
      {$Else}
         Climographoptions1.Visible := GISDb[DBShown].MyData.FieldExists('JAN_TEMP') and GISDb[DBShown].MyData.FieldExists('JAN_PRECIP');
      {$EndIf}
      {$IfDef ExGeology}
         Focalmechanismoptions1.Visible := false;
      {$Else}
         Focalmechanismoptions1.Visible := GISDb[DBShown].FocalMechsPresent;
      {$EndIf}
      Focalmechanismsize1.Visible := Focalmechanismoptions1.Visible;
      Focalmechanismfill1.Visible := Focalmechanismoptions1.Visible;
      PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;

procedure Tshowrecordform.Image2MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      PopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;

procedure Tshowrecordform.Image5DblClick(Sender: TObject);
begin
   PopupMenu3.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tshowrecordform.Image5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      PopupMenu3.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;

procedure Tshowrecordform.Openprofile1Click(Sender: TObject);
begin
   GISDb[DBShown].dbTablef.Do3Dshapefileprofile1Click(Nil);
end;

procedure Tshowrecordform.Popoutgraph1Click(Sender: TObject);
begin
   PopoutGraph := true;
   ShowRecords(CurrentRec_ID);
end;

procedure Tshowrecordform.Popoutgraph2Click(Sender: TObject);
begin
   PopOutGraph := true;
   ShowRecords(CurrentRec_ID);
end;

procedure Tshowrecordform.Redrawgraph1Click(Sender: TObject);
begin
   if (TabSheet5.TabVisible or TabSheet8.Visible) and CanDisplay then ShowRecords(CurrentRec_ID);
end;

procedure Tshowrecordform.Resia1Click(Sender: TObject);
begin
   ShowRecords(CurrentRec_ID);
end;

procedure Tshowrecordform.Resizeimage1Click(Sender: TObject);
begin
   ShowRecords(CurrentRec_ID);
end;

procedure Tshowrecordform.WWWBtn1Click(Sender: TObject);
begin
   DisplayWWWFromDataBase(GISDb[DBShown].MyData,GISDb[DBShown].WWWFieldNames[1]);
end;


procedure Tshowrecordform.BitBtn6Click(Sender: TObject);
begin
   if LineOrAreaShapefile(GISDb[DBShown].ShapeFileType) then begin
      Petmar.PickLineSizeAndColor('Highlight records',BitBtn6,MDDef.HighLightColor,MDDef.HighlightLineWidth);
   end
   else begin
      Petmar.PickSymbol(BitBtn6,MDDef.HighlightSymbol,'Highlight records');
   end;
end;

procedure Tshowrecordform.BitBtn7Click(Sender: TObject);
begin
   GISDB[dbshown].dbtablef.HighlightFan(claRed);
end;

procedure Tshowrecordform.BitBtn8Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   Lat := StrToFloat(StringGrid1.Cells[1,LatLine]);
   Long := StrToFloat(StringGrid1.Cells[1,LongLine]);
   GetLatLn.GetLatLongDefault(WGS84DatumConstants,'revised location',Lat,Long);
   StringGrid1.Cells[1,LatLine] := FloatToStr(Lat);
   StringGrid1.Cells[1,LongLine] := FloatToStr(Long);
end;

procedure Tshowrecordform.BitBtn9Click(Sender: TObject);
begin
   GISDb[DBShown].MyData.Edit;
   GISDb[DBShown].MyData.SetFieldByNameAsString('USE','Y');
   GISDb[DBShown].MyData.Post;
end;

procedure Tshowrecordform.Climographoptions1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      Koppen_Opts.KoppenOptions;
      ShowRecords;
   {$EndIf}
end;

procedure Tshowrecordform.ColorBitBtnClick(Sender: TObject);
begin
   QueryColor(ColorBitBtn,Color);
   StringGrid1.Cells[1,ColorLine] := IntToStr(ConvertPlatformColorToTColor(Color));
end;

procedure Tshowrecordform.Copyimagetoclipboard1Click(Sender: TObject);
begin
   PetImage.AssignImageToClipBoard(Image1);
end;

procedure Tshowrecordform.Copyimagetoclipboard2Click(Sender: TObject);
begin
   PetImage.AssignImageToClipBoard(Image2);
end;

procedure Tshowrecordform.Copyimagetoclipboard3Click(Sender: TObject);
begin
   PetImage.AssignImageToClipBoard(Image5);
end;

initialization
finalization
   {$IfDef RecordDEditProblems} WriteLineToDebugFile('RecordDEditProblems active in demeshowdbrecord'); {$EndIf}
   {$IfDef RecordLinkProblems}  WriteLineToDebugFile('RecordLinkProblems active in demeshowdbrecord'); {$EndIf}
   {$IfDef RecordCloseShowRecord} WriteLineToDebugFile('RecordCloseShowRecord active in demeshowdbrecord'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demshowdbrecord'); {$EndIf}
end.


