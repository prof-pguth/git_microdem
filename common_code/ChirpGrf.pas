unit ChirpGrf;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 4/12/2021       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
    {$IfDef Debug}
       //{$Define RecordTraces}
       //{$Define RecordChirpGraph}
       //{$Define RecordChirpFence}
    {$EndIf}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
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
//end needed for inline core DB functions

  Vcl.Samples.Spin, Vcl.ToolWin,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Dialogs,
  System.Types,System.UITypes, System.RTLConsts,

  {$IfDef ExFMX3D}
  {$Else}
     FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects3D, FMX.Types3D,
     FMX.Layouts, FMX.Layers3D, System.Math.Vectors, FMX.Controls3D,
     FMX.MaterialSources, FMX.StdCtrls,
     FMX.Forms3D, FMX.Graphics, FMX.Controls.Presentation,
  {$EndIf}

  Windows,  SysUtils, Classes, Graphics, Controls, Forms,  Math,
  Menus,  StdCtrls, Buttons,
  BASEGRAF,Petmar_types,PETMAR,DEMMapf;

const
   MeterResolution = 0.0173;

type
   TjsfMessageHeader = packed record
      StartMarker : Word;
      ProtocolVersion : byte;
      SessionID : byte;
      MessageType : Word;
      CommandType : byte;
      SubsystemNumber : byte;
      Channel : byte;
      SeqNumber : byte;
      Reserved : word;
      SizeToFollow : LongInt;        //12-15
   end;
   tNavData  = packed record
      XCoord,
      YCoord,
      XCoord2,
      YCoord2 : LongInt;
      CoordUnits : SmallInt;
   end;
   tPulseInfo  = packed record
      AnnotationString : array[1..24] of byte;
      DataSamplesInPacket : word;                  //114-115
      SampleIntNannosec : LongWord;
      GainFactorADC : word;
      UserPulsePower : SmallInt;
      Reserved : SmallInt;
      ChirpStartFreq : Word;
      ChirpEndFreq : word;
      SweepLen : word;
      Pressure,
      Depth : LongInt;
      Reserved2 : array[1..2] of word;
      Altitude : LongInt;
      Reserved3 : array[1..2] of LongInt;
   end;
   tCPUTime  = packed record
      Data : array[1..6] of SmallInt;
   end;
   tMessageType80 = packed record  //240 bytes
      TraceSeqNumber : Int32;           //0-3
      StartingDepth,                    //4-7
      PingNumber,                       //8-11
      Reserved : LongWord;              //12-15
      Unused : array[1..6] of SmallInt;
      IDCode : SmallInt;
      Unused2 : Longword;
      DataFormat,                        //34-35
      DistAntTow,                        //36-37
      DistAntTowStarDir : Int16;         //38-39
      Reserved2 : array[1..2] of Int16;
      RS232Data : array[1..28] of byte;
      NavData : tNavData;
      PulseInfo : tPulseInfo;           //140-155
      CPUTime : tCPUTime;               //156-167
      WeightFactor,                     //168-169
      PulsesInWater : SmallInt;         //170-171
      CompassHeading : Word;            //172-173
      OrientationSensor : array[1..3] of SmallInt;    //pitch, roll, reserved
      UserDefined : array[1..60] of byte;
   end;

type
  tFreqDisplay = (LowFreq,HighFreq,ColorMerge);
  tTraceGrafs = (tgIntensity,tgGrayscale,tgBoth);
  tChirpGraphDoing = (cgdNothing,cgdGetDepth,cgdGetTop,cgdGetBase);
  tGraphDoingWhat = (DoingNothing,ZonMap,SegmentProfile);
  TChirpGraph = class(TThisBaseGraph)
    SpinButton1: TSpinButton;
    SpinButton2: TSpinButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Chirpsoptions1: TMenuItem;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    BitBtn3: TBitBtn;
    CheckBox2: TCheckBox;
    Chirpoptions1: TMenuItem;
    ChirpPopupMenu2: TPopupMenu;
    Subset1: TMenuItem;
    race1: TMenuItem;
    ChirpBitBtn1: TBitBtn;
    Depthfile1: TMenuItem;
    New1: TMenuItem;
    OPen1: TMenuItem;
    Digitize1: TMenuItem;
    Depths1: TMenuItem;
    hicknesses1: TMenuItem;
    Done1: TMenuItem;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Segment1: TMenuItem;
    Index1: TMenuItem;
    RadioGroup3: TRadioGroup;
    BitBtn1: TBitBtn;
    N4: TMenuItem;
    Returnstoshow1: TMenuItem;
    Displayoptions1: TMenuItem;
    procedure BitBtn3Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure SpinButton2DownClick(Sender: TObject);
    procedure SpinButton2UpClick(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure Index1Click(Sender: TObject);
    procedure Chirpsoptions1Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure Subset1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure OPen1Click(Sender: TObject);
    procedure Depths1Click(Sender: TObject);
    procedure hicknesses1Click(Sender: TObject);
    procedure Done1Click(Sender: TObject);
    procedure ChirpBitBtn1Click(Sender: TObject);
    procedure Chirpoptions1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure Segment1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Returnstoshow1Click(Sender: TObject);
    procedure race1Click(Sender: TObject);
    procedure Displayoptions1Click(Sender: TObject);
  private
    { Private declarations }
    FormClosing : boolean;
    function JSFDepth(y : integer) : float64;
    function JSFTWTT(y : integer) : float64;
    procedure GraphChirpRecord;
  public
    { Public declarations }
     dbFileName,
     ChirpFileName : PathStr;
     LineLength,LineBearing,
     TopDepth,BaseDepth : float64;
     FreqDisplay   : tFreqDisplay;
     TraceGrafs : tTraceGrafs;
     NumSubbottomRecords,
     ChirpZdb,
     TrackDB,OnSegment,
     XSkipSize,YSkipSize,XDupeSize,
     BlockSize          : integer;
     Inf                : file;
     ReverseLine,
     LatsSet            : boolean;
     Lats,Longs         : array[0..MaxScreenXMax] of float32;
     PingNum            : array[0..MaxScreenXMax] of Integer;
     SegBreaks          : array[0..25] of integer;
     GraphDoingWhat     : tGraphDoingWhat;
     FirstX,FirstY : integer;
     ChirpMapOwner : tMapForm;
     procedure RedrawChirps;
     procedure OpenChirpsFile(fName : PathStr);
  end;


procedure StartChirps(MapOwner : tMapForm; fName : PathStr = '');


implementation

{$R *.DFM}

uses
   Nevadia_Main,
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   {$IfDef ExFMX3D}
   {$Else}
      view3d_main,
   {$EndIf}

   PETMath,
   DEMStringGrid,
   ChirpOpt,
   Make_Tables,
   DEMDef_Routines,
   PetDBUtils,
   DEMDefs, BaseMap, PETImage;

var
   ChirpGraphDoing : tChirpGraphDoing;
   {$IfDef ExFMX3D}
   {$Else}
      FenceDiagram : TView3DForm;
   {$EndIf}
   MakeFenceDiagram : boolean;

(*
function FindBottom(CorrelatedReturn : tCorrelatedReturn) : float64;
var
   i : integer;
begin
   i := round(2 * MDDef.SonarMapDef.MinDraft / 50 * 1599);
   while (abs(CorrelatedReturn[i]) < MDDef.SonarMapDef.BottomValue) and (i < 3200) do inc(i);
   Result := 50.0 * (i div 2) / 1599;
end;
*)

procedure ChangeChirpGraphDoing(NowChirpGraphDoing : tChirpGraphDoing);
begin
   ChirpGraphDoing := NowChirpGraphDoing;
   case ChirpGraphDoing of
       cgdNothing   : wmDEM.StatusBar1.Panels[0].Text := '';
       cgdGetDepth  : wmDEM.StatusBar1.Panels[0].Text := 'Depth';
       cgdGetTop    : wmDEM.StatusBar1.Panels[0].Text := 'Layer top in Chirp record';
       cgdGetBase   : wmDEM.StatusBar1.Panels[0].Text := 'Layer base in Chirp record';
   end;
end;


procedure StartChirps(MapOwner : tMapForm; fName : PathStr = '');
var
   NumRec,i  : LongInt;
   ChirpDisplayForm : TChirpGraph;
   FilesWanted : TStringList;
   DefaultFilter : byte;

      procedure OpenFile(fName : PathStr);
      begin
         {$IfDef RecordChirpGraph} WriteLineToDebugFile('OpenFile in ' + fName); {$EndIf}
         ChirpDataPath := ExtractFilePath(fName);
         ChirpDisplayForm := TChirpGraph.Create(Application);
         ChirpDisplayForm.ChirpMapOwner := MapOwner;
         ChirpDisplayForm.OpenChirpsFile(fName);
         NumRec := GISdb[ChirpDisplayForm.TrackDB].MyData.RecordCount;

         ChirpDisplayForm.ChirpFileName := FName;
         ChirpDisplayForm.GraphDraw.YWindowSize := ChirpDisplayForm.ClientHeight - ChirpDisplayForm.Panel1.Height - ChirpDisplayForm.GraphDraw.BottomMargin;
         if (ChirpDisplayForm.LineBearing > 135) and (ChirpDisplayForm.LineBearing < 215) then begin
            ChirpDisplayForm.CheckBox1.Checked := true;
            ChirpDisplayForm.ReverseLine := ChirpDisplayForm.CheckBox1.Checked;
         end;
         ChirpDisplayForm.RedrawChirps;
         {$IfDef RecordChirpGraph} WriteLineToDebugFile('Openfile out'); {$EndIf}
      end;


begin
   if (fName <> '') then OpenFile(fName)
   else begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(ChirpDataPath);
      DefaultFilter := 1;
      if GetMultipleFiles('Subbottom data file','subbottom files|*.JSF',FilesWanted,DefaultFilter) then begin
         for I := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[i];
            OpenFile(fName);
         end;
      end;
      FilesWanted.Free;
   end;
end;


procedure TChirpGraph.Open1Click(Sender: TObject);
var
   fName : PathStr;
begin
  inherited;
   fName := ExtractFilePath(ChirpFileName);
   if Petmar.GetFileFromDirectory('depth file',DefaultDBMask,fName) then begin
      CloseAndNilNumberedDB(ChirpZdb);
      ChirpZdb := ChirpMapOwner.LoadDataBaseFile(fName);
      Digitize1.Enabled := true;
   end;
end;


procedure TChirpGraph.OpenChirpsFile(fName : PathStr);
var
   PulseRecs : integer;
   dbFile : tMyData;
   f : file;
   Lat,Long,Lat2,Long2 : float64;
   jsfMessageHeader : tjsfMessageHeader;
   MessageType80 : tMessageType80;
   DataPulse : array[0..48000] of Int16;

   procedure MakeTable;
   begin
      {$IfDef RecordChirpGraph} HighlightLineToDebugFile('MakeTable in'); {$EndIf}
      Make_Tables.MakeSideScanRecordFile(DBFileName,false);
      dbfile := tMyData.Create(DBFileName);
      ShowHourglassCursor;
      if FileExtEquals(fname,'.JSF') then begin
         if GetFileSize(fName) mod 8192 = 0 then BlockSize := 8192              //.10 file, Geo-Star from
         else if (GetFileSize(fName) mod 8352) = 0 then BlockSize := 8352;
         AssignFile(f,fName);
         reset(f,1);
         while not EOF(f) do begin
            BlockRead(f,jsfMessageHeader,16);
            if (jsfMessageHeader.MessageType = 80) then begin
               BlockRead(f,MessageType80,SizeOf(MessageType80));
               with MessageType80 do begin
                  PulseRecs := (jsfMessageHeader.SizeToFollow - SizeOf(MessageType80)) div 2;
                  BlockRead(f,DataPulse,(jsfMessageHeader.SizeToFollow - 240));   //DataSamplesInPacket*2);
                  Lat := MessageType80.NavData.YCoord2 * 0.0001 /60;
                  Long := MessageType80.NavData.XCoord2 * 0.0001 /60;
                  dbFile.Insert;
                  dbFile.SetFieldByNameAsFloat('LAT',Lat);
                  dbFile.SetFieldByNameAsFloat('LONG',Long);
                  dbFile.SetFieldByNameAsInteger('PING',PingNumber);
                  dbFile.SetFieldByNameAsFloat('DEPTH',-0.001 * PulseInfo.Depth);
                  dbFile.SetFieldByNameAsString('TIME',IntToStr(CPUTime.Data[3]) + ':' + IntToStr(CPUTime.Data[4]) + ':'  + IntToStr(CPUTime.Data[5]) + '.' + IntToStr(CPUTime.Data[6]));
                  dbFile.Post;
               end;
            end
            else begin
               BlockRead(f,DataPulse,jsfMessageHeader.SizeToFollow);   //DataSamplesInPacket*2);
            end;
         end;
         CloseFile(f);
         dbFile.Destroy;

         MDDef.UseMeters := true;
         MDdef.AddSpeed := false;
         MDdef.AddAzimuth := true;
         MDdef.AddDist := true;
         MDdef.AddCumDist := true;
         {$IfDef RecordChirpGraph} WriteLineToDebugFile('Loading DB'); {$EndIf}

         TrackDB := ChirpMapOwner.LoadDataBaseFile(DBFileName);
         GISDB[TrackDB].AddSequentialIndex(RecNoFName,true);
         {$IfDef RecordChirpGraph} WriteLineToDebugFile('call remove dupe positions'); {$EndIf}
         GISDB[TrackDB].RemoveDuplicatePositions;
         {$IfDef RecordChirpGraph} WriteLineToDebugFile('call fill track voids'); {$EndIf}
         GISDB[TrackDB].FillTrackVoids;
         {$IfDef RecordChirpGraph} WriteLineToDebugFile('call add nav fields'); {$EndIf}
         GISDB[TrackDB].AddNavFields;
         GISDB[TrackDB].SavePointShapeFile(false);
         ShowDefaultCursor;
      end;
      {$IfDef RecordChirpGraph} WriteLineToDebugFile('MakeTable out'); {$EndIf}
   end;


begin
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('TChirpGraph.OpenChirpsFile in'); {$EndIf}
   SaveBackupDefaults;
   InsureFileIsNotReadOnly(fName);
   DBFileName := ChangeFileExt(fName,DefaultDBExt);

   if (FileExists(DBFileName)) then begin
      TrackDB := ChirpMapOwner.LoadDataBaseFile(DBFileName);
      if not GISDB[TrackDB].MyData.FieldExists('CUM_M') then begin
         CloseAndNilNumberedDB(TrackDB);
         MakeTable;
      end;
   end
   else begin
      MakeTable;
   end;
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('TrackDB=' + IntToStr(TrackDB)); {$EndIf}

   NumSubbottomRecords := GISDB[TrackDB].MyData.RecordCount;
   GISDB[TrackDB].MyData.First;
   while not GISDB[TrackDB].MyData.ValidLatLongFromTable(Lat,Long) do GISDB[TrackDB].MyData.Next;

   GISDB[TrackDB].MyData.Last;
   while (GISDB[TrackDB].MyData.GetFieldByNameAsFloat('CUM_M') = 0) do GISDB[TrackDB].MyData.Prior;
   LineLength := GISDB[TrackDB].MyData.GetFieldByNameAsFloat('CUM_M');
   while not GISDB[TrackDB].MyData.eof do begin
      GISDB[TrackDB].MyData.Edit;
      GISDB[TrackDB].MyData.SetFieldByNameAsFloat('CUM_M',LineLength);
      GISDB[TrackDB].MyData.Next;
   end;

   GISDB[TrackDB].MyData.Last;
   repeat
      if not GISDB[TrackDB].MyData.ValidLatLongFromTable(Lat2,Long2) then GISDB[TrackDB].MyData.Prior;
   until GISDB[TrackDB].MyData.ValidLatLongFromTable(Lat2,Long2);


   while not GISDB[TrackDB].MyData.eof do begin
      GISDB[TrackDB].MyData.Edit;
      GISDB[TrackDB].MyData.SetFieldByNameAsFloat('LAT',Lat2);
      GISDB[TrackDB].MyData.SetFieldByNameAsFloat('LONG',Long2);
      GISDB[TrackDB].MyData.Next;
   end;

   WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',GetUTMZone(Long2),HemiFromLat(Lat2),'Chirp');

   RestoreBackupDefaults;
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('File: ' + fName + '  Recs: ' + IntToStr(NumSubbottomRecords)); {$EndIf}
end;


procedure TChirpGraph.race1Click(Sender: TObject);
begin
   inherited;
   GraphChirpRecord;
end;

procedure TChirpGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormClosing := true;
  inherited;

  Action := caFree;
end;


procedure TChirpGraph.FormCreate(Sender: TObject);
begin
  {$IfDef RecordChirpGraph} WriteLineToDebugFile('TChirpGraph.FormCreate in'); {$EndIf}
  inherited;  //needed for base form
   FormClosing := false;
   LatsSet := false;
   ChirpZdb := 0;
   ClientHeight := MDDef.ChirpReturnsToShow + 100;
   ClientWidth := 1200;
   MakeFenceDiagram := false;
   TraceGrafs := tTraceGrafs(0);
   Label3.Caption := RealToString(MDDef.ChirpGain,-4,1);
   GraphDoingWhat := DoingNothing;
   Option1.Visible := true;
   RadioGroup1.ItemIndex := pred(MDDef.ChirpXThinning);
   RadioGroup2.ItemIndex := pred(MDDef.ChirpYThinning);
   RadioGroup3.ItemIndex := pred(MDDef.ChirpXDupe);
   Image1.AutoSize := true;
  {$IfDef RecordChirpGraph}WriteLineToDebugFile('TChirpGraph.FormCreate out'); {$EndIf}
end;

procedure TChirpGraph.hicknesses1Click(Sender: TObject);
begin
  inherited;
   ChangeChirpGraphDoing(cgdGetTop);
end;


function Intensity(ret : integer) : integer;  inline;
begin
   Ret := abs(ret);
   Ret := round(Ret / MDDef.ChirpGain);   // * (FilterA + (Ret/3200) * FilterB)));
   if (Ret > 32767) then Ret := 32767;
   Result := 255 - Result;
end;


procedure TChirpGraph.RadioGroup1Click(Sender: TObject);
begin
   MDDef.ChirpXThinning:= succ(RadioGroup1.ItemIndex);
   if CheckBox5.Checked then RedrawChirps;
end;


procedure TChirpGraph.RadioGroup2Click(Sender: TObject);
begin
   MDDef.ChirpYThinning:= succ(RadioGroup2.ItemIndex);
   if CheckBox5.Checked then RedrawChirps;
end;

procedure TChirpGraph.RadioGroup3Click(Sender: TObject);
begin
   MDDef.ChirpXDupe := succ(RadioGroup3.ItemIndex);
   if CheckBox5.Checked then RedrawChirps;
end;


procedure TChirpGraph.RedrawChirps;
var
   BitMap,Protractor : tMyBitmap;
   Inf               : file;
   jsfMessageHeader  : tjsfMessageHeader;
   MessageType80     : tMessageType80;
   NP,
   FirstRec,LastRec,
   Dupe,
   i,x,y,xp,yp : integer;
   xutm,yutm,z,Lat,Long,
   VE : float64;
   TStr : ShortString;
   DataPulse : array[0..48000] of SmallInt;
   rp,ip : array[0..8000] of float32;
   BMPmem : tBMPMemory;
   {$IfDef ExFMX3D}
   {$Else}
      P : array[0..0] of TPoint3D;
   {$EndIf}

      procedure SkipRecords(NumToSkip : integer);
      var
        Skipped : integer;
        DataPulse : array[0..48000] of SmallInt;
      begin
         Skipped := 0;
         Dec(NumToSkip);
         if (NumToSkip > 0) and (not eof(inf)) then repeat
            BlockRead(inf,jsfMessageHeader,16);
            if (jsfMessageHeader.MessageType = 80) then begin
               BlockRead(inf,MessageType80,SizeOf(MessageType80));
               with MessageType80 do BlockRead(inf,DataPulse,jsfMessageHeader.SizeToFollow - 240);   //DataSamplesInPacket*2);
               inc(Skipped);
            end
            else begin
               BlockRead(inf,DataPulse,jsfMessageHeader.SizeToFollow);   //DataSamplesInPacket*2);
            end;
         until eof(inf) or (Skipped = NumToSkip);
      end;

      function MarginsString : shortstring;
      begin
         Result := ' Margins, left=' + IntToStr(GraphDraw.LeftMargin) + ' top= ' + IntToStr(GraphDraw.TopMargin) +  ' bottom=' + IntToStr(GraphDraw.BottomMargin);
      end;

begin
   if (ChirpFileName = '') or (NumSubbottomRecords = 0) or (not FileExists(ChirpFileName)) then exit;
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('RedrawChirps in'); {$EndIf}

   XSkipSize := succ(RadioGroup1.ItemIndex);
   YSkipSize := succ(RadioGroup2.ItemIndex);
   XDupeSize := succ(RadioGroup3.ItemIndex);

   ScrollGraph := true;
   ScrollBox1.AutoScroll := true;
   GraphDraw.TopMargin := 36;
   Panel1.Height := 50;

   GraphDraw.XWindowSize := XDupeSize * NumSubbottomRecords div XSkipSize + GraphDraw.LeftMargin + GraphDraw.RightMargin;
   GraphDraw.YWindowSize := (MDDef.ChirpReturnsToShow div YSkipSize) + GraphDraw.BottomMargin + GraphDraw.TopMargin;

   Image1.Width := GraphDraw.XWindowSize;
   Image1.Height := GraphDraw.YWindowSize;

   Width := GraphDraw.XWindowSize;
   Height := GraphDraw.YWindowSize;

   {$IfDef RecordChirpGraph} WriteLineToDebugFile('defined, GraphDraw.WindowSize=' + IntToStr(GraphDraw.XWindowSize) + 'x' + IntToStr(GraphDraw.YWindowSize) );{$EndIf}
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('Want ImageSize=' + ImageSize(Image1) );{$EndIf}
   GraphDraw.MinHorizAxis := 0;
   GraphDraw.MaxHorizAxis := LineLength;
   GraphDraw.MinVertAxis := 0;

   TopDepth := 0;
   BaseDepth := MeterResolution * MDDef.ChirpReturnsToShow;

   if CheckBox4.Checked then begin
      GraphDraw.MaxVertAxis := BaseDepth / 1500 * 2;
      GraphDraw.VertLabel := 'TWTT (ms)';
   end
   else begin
      GraphDraw.MaxVertAxis := BaseDepth;
      GraphDraw.VertLabel := 'Depth (m)';
   end;

   GraphDraw.NormalCartesianY := false;
   GraphDraw.HorizLabel := 'Distance (m)';
   SetUpGraphForm;

   {$IfDef RecordChirpGraph} WriteLineToDebugFile('after setupgraph, GraphDraw.WindowSize=' + IntToStr(GraphDraw.XWindowSize) + 'x' + IntToStr(GraphDraw.YWindowSize) );{$EndIf}
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('Created ImageSize=' + ImageSize(Image1) );{$EndIf}
   PetImage.CopyImageToBitmap(Image1,Bitmap);
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('Created Bitmap Size=' + BitmapSize(Bitmap) + MarginsString ); {$EndIf}

    try
      InsureFileIsNotReadOnly(ChirpFileName);
      assignFile(inf,ChirpFileName);
      reset(inf,1);
      NP := 0;
      Caption := ExtractFileName(ChirpFileName);
      StartProgressAbortOption('Display ' + Caption);
      ShowHourglassCursor;
      ReverseLine := CheckBox1.Checked;

      if ReverseLine then begin
         x := pred(NumSubbottomRecords div XSkipSize);
         LastRec := x + GraphDraw.LeftMargin;
      end
      else begin
         x := 0;
         FirstRec := GraphDraw.LeftMargin;
      end;
      BMPmem := tBMPMemory.Create(Bitmap);
      {$IfDef RecordChirpGraph} WriteLineToDebugFile('Loop, Bitmap Size=' + BitmapSize(Bitmap) + MarginsString); {$EndIf}

      GISDB[TrackDB].MyData.First;
      GISDB[TrackDB].EmpSource.Enabled := false;
      while (not EOF(inf)) and (x >= 0) and (x <= NumSubbottomRecords) do begin
          if ReverseLine then UpDateProgressBar((NumSubbottomRecords - x)/ (NumSubbottomRecords div XSkipSize))
          else UpDateProgressBar(x / (NumSubbottomRecords div XSkipSize));
          BlockRead(inf,jsfMessageHeader,16);
          if (jsfMessageHeader.MessageType = 80) then begin
             BlockRead(inf,MessageType80,SizeOf(MessageType80));
             GISDB[TrackDB].MyData.ValidLatLongFromTable(Lat,Long);
             GISDB[TrackDB].MyData.Next;
             BlockRead(inf,DataPulse,jsfMessageHeader.SizeToFollow - 240);
             for I := 0 to pred(MessageType80.PulseInfo.DataSamplesInPacket) do begin
                rp[i] := DataPulse[i*2] * Math.Power(2,-MessageType80.WeightFactor);
                ip[i] := DataPulse[succ(i*2)] * Math.Power(2,-MessageType80.WeightFactor);
             end;
             i := 0;
             while (I <= MDDef.ChirpReturnsToShow) do begin
                for Dupe := 1 to XDupeSize do begin
                   xp := GraphDraw.LeftMargin + x * XDupeSize + pred(Dupe);

                   Lats[xp] := Lat;
                   Longs[xp] := Long;

                   PingNum[xp] := MessageType80.PingNumber;
                   y := ValidByteRange(255 - round(sqrt(sqr(rp[i]) + sqr(ip[i])) * MDDef.ChirpGain * ( 1 + (MDDef.ChirpTVG-1) * (i/MDDef.ChirpReturnsToShow))));
                   yp := GraphDraw.TopMargin + i div YSkipSize;
                   if BMPMem.OnBitmap(xp,yp) then BMPMem.SetPixelRGB(xp,yp,y,y,y);

                   {$IfDef ExFMX3D}
                   {$Else}
                      if MakeFenceDiagram and (Dupe = 1) then begin
                           if (I < MDDef.ChirpReturnsToShow) and (NP < FenceDiagram.NPtsAllocated[FenceDiagram.CurCloud]) then begin
                              z := -i;
                              WGS84DatumConstants.LatLongDegreeToUTM(Lats[xp],Longs[xp],xutm,yutm);
                              //we use coordinates with x and y horizontal, and z is the vertical
                              P[0].x := FenceDiagram.ScaledX(xutm);
                              P[0].z := FenceDiagram.ScaledY(yutm);
                              P[0].y := FenceDiagram.ScaledZ(z);
                              FenceDiagram.VertexBuffer[FenceDiagram.CurCloud].Vertices[NP] := P[0]*25;                   // Save the vertex
                              FenceDiagram.VertexBuffer[FenceDiagram.CurCloud].TexCoord0[NP] := System.Types.PointF(0,(y/255)); // Set texture value from elevation color ramp
                              FenceDiagram.IndexBuffer[FenceDiagram.CurCloud][NP] := NP;                                // Set the index to the vertex
                              NP := NP+1;
                           end;
                           FenceDiagram.NPtsUsed[FenceDiagram.CurCloud] := NP;
                      end;
                  {$EndIf}
                end;
                inc(i,YSkipSize);
             end;
             if ReverseLine then begin
                FirstRec := x + GraphDraw.LeftMargin;
                dec(x);
             end
             else begin
                LastRec := x + GraphDraw.LeftMargin;
                inc(x);
             end;
             SkipRecords(XSkipSize);
          end
          else begin
             BlockRead(inf,DataPulse,jsfMessageHeader.SizeToFollow);   //DataSamplesInPacket*2);
          end;
      end;
      {$IfDef RecordChirpGraph} WriteLineToDebugFile('Drawn, BitmapSize=' + BitmapSize(Bitmap) + MarginsString); {$EndIf}
      Self.Caption := ExtractFileName(ChirpFileName) + RealToString(LineLength,10,0) + ' m' + RealToString(LineBearing,6,0) + DegSym;
      Bitmap.Canvas.Font.Color := clRed;
      Bitmap.Canvas.Font.Size := 14;
      Bitmap.Canvas.Font.Style := [fsBold];
      TStr := LatLongDegreeToString(Lats[LastRec],Longs[LastRec]);
      while (Bitmap.Canvas.TextWidth(TStr + '   ' + Self.Caption)) > (Bitmap.Width - GraphDraw.LeftMargin) do begin
         Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size - 1;
         if Bitmap.Canvas.Font.Size < 10 then Bitmap.Canvas.Font.Style := [];
      end;

      Bitmap.Canvas.TextOut(GraphDraw.LeftMargin,5,LatLongDegreeToString(Lats[FirstRec],Longs[FirstRec],DecDegrees) + '  ' + Caption);
      Bitmap.Canvas.TextOut(Bitmap.Width - Bitmap.Canvas.TextWidth(TStr)-5,5,TStr);

      if CheckBox2.Checked then begin
         VE :=  ( GraphDraw.YWindowSize) / (BaseDepth - TopDepth) /  (GraphDraw.XWindowSize / LineLength);
         Protractor := CreateProtractor(false,true,VE);
         Bitmap.Canvas.Draw(30+ GraphDraw.LeftMargin,35,Protractor);
         Protractor.Free;
      end;
      {$IfDef RecordChirpGraph} WriteLineToDebugFile('Got to finally, BitmapSize=' + BitmapSize(Bitmap) + MarginsString); {$EndIf}
   finally
      EndProgress;
      LatsSet := true;
      BMPMem.Destroy;
      Image1.Picture.Graphic := bitmap;
      Bitmap.Free;
      closeFile(inf);
      GraphDoingWhat := DoingNothing;
      GISDB[TrackDB].ShowStatus;
   end;
   {$IfDef RecordChirpGraph} WriteLineToDebugFile('Out, ImageSize=' + ImageSize(Image1) + MarginsString); {$EndIf}
end;


var
   LastXm : integer;

function TChirpGraph.JSFDepth(y : integer) : float64;
begin
   Result := MeterResolution * y * YSkipSize;
end;

function TChirpGraph.JSFTWTT(y : integer) : float64;
begin
   Result := MeterResolution * y * YSkipSize / 1500 * 2;
end;

procedure TChirpGraph.New1Click(Sender: TObject);
var
   fName : PathStr;
begin
  inherited;
   fName := ExtractFilePath(ChirpFileName);
   if Petmar.GetFileNameDefaultExt('depth file',DefaultDBMask,fName,true) then begin
      DEMDataBase.CloseAndNilNumberedDB(ChirpZdb);
      Make_tables.CreateLatLongZTable(fName,true,false,false,false,false,true);
      ChirpZdb := ChirpMapOwner.LoadDataBaseFile(fName);
      Digitize1.Enabled := true;
   end;
end;


procedure TChirpGraph.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   j : integer;
begin
   if FormClosing then exit;

   if LatsSet then begin
      wmdem.StatusBar1.Panels[1].Text := ChirpMapOwner.MapDraw.PrimMapProj.PreferLocationString(Lats[x],Longs[x]);
      wmdem.StatusBar1.Panels[2].Text := 'Ping=' + IntToStr(PingNum[x]);
      wmdem.StatusBar1.Panels[3].Text := 'TWTT=' + RealToString(JSFTWTT(y),-12,4) + ' ms '  + '  depth=' + RealToString(JSFDepth(y),-12,2) + ' m';
   end;
   LastRoamLat := Lats[x];
   LastRoamLong := Longs[x];
   if (WmDEM.MDIChildCount > 0) then
     for j := WmDEM.MDIChildCount-1 downto 0 do
        if (WmDEM.MDIChildren[j].Handle <> Self.Handle) then
           PostMessage(WmDEM.MDIChildren[j].Handle,WM_BroadcastLatLongMessage,0,0);
   ApplicationProcessMessages;
   LastXM := x;
end;


procedure TChirpGraph.Index1Click(Sender: TObject);
var
   fName,dbName : PathStr;
   Table,Table2 : tMyData;
   TheFiles : tStringList;
   i : integer;
   DefaultFilter : byte;
   HiLat,LowLong,LowLat,HighLong : float64;
begin
   TheFiles := tStringList.Create;
   TheFiles.Add(ChirpDataPath);
   DefaultFilter := 1;
   if GetMultipleFiles('Subbottom data file','subbottom files|*.JSF',TheFiles,DefaultFilter) then begin
      fName := ExtractFilePath(TheFiles.Strings[0]) + 'chirp_index' + DefaultDBExt;
      if not FileExists(fName) then begin
         MakeHydrographyIndex(fName);
      end;
      Table := tMyData.Create(fName);
      for i := 0 to pred(TheFiles.Count) do begin
         fName := TheFiles.Strings[i];
         wmdem.StatusBar1.Panels[1].Text := ExtractFileName(fName);
         ApplicationProcessMessages;
         Table.ApplyFilter('FILENAME=' + QuotedStr(fName));
         if (Table.RecordCount = 0) then begin
            OpenChirpsFile(fName);
            dbName := ChangeFileExt(fName,DefaultDBExt);
            Table2 := tMyData.Create(dbName);
            FindPointFileGeoLimits(Table2,HiLat,LowLong,LowLat,HighLong);
            Table.Insert;
            Table.SetFieldByNameAsString('FILENAME',fName);
            Table.SetFieldByNameAsInteger('CHIRP',Table2.RecordCount);
            Table.SetFieldByNameAsFloat('LAT_HI',HiLat);
            Table.SetFieldByNameAsFloat('LAT_LOW',LowLat);
            Table.SetFieldByNameAsFloat('LONG_HI',HighLong);
            Table.SetFieldByNameAsFloat('LONG_LOW',LowLong);
            Table.Post;
            Table2.Destroy;
         end;
      end;
   end;
   wmdem.StatusBar1.Panels[0].Text := '';
   wmdem.StatusBar1.Panels[1].Text := '';
   Table.Destroy;
end;


procedure TChirpGraph.SpinButton2DownClick(Sender: TObject);
begin
   MDDef.ChirpTVG := MDDef.ChirpTVG / 1.25;
   Label4.Caption := RealToString(MDDef.ChirpTVG,-6,3);
   if CheckBox5.Checked then RedrawChirps;
end;


procedure TChirpGraph.SpinButton2UpClick(Sender: TObject);
begin
   MDDef.ChirpTVG := MDDef.ChirpTVG * 1.25;
   Label4.Caption := RealToString(MDDef.ChirpTVG,-6,3);
   if CheckBox5.Checked then RedrawChirps;
end;


procedure TChirpGraph.Subset1Click(Sender: TObject);
var
   x,Segment : integer;
   OutName : PathStr;
   f : file;
   jsfMessageHeader : tjsfMessageHeader;
   MessageType80 : tMessageType80;
   DataPulse : array[0..48000] of SmallInt;
   FirstRec,LastRec : integer;
   Ext : ExtStr;
begin
   ChirpGraphDoing := cgdNothing;
   InsureFileIsNotReadOnly(ChirpFileName);
   Ext := ExtractFileExt(ChirpFileName);
   assignFile(inf,ChirpFileName);

   for Segment := 1 to 25 do begin
      GISdb[TrackDB].MyData.ApplyFilter('SEGMENT=' + IntToStr(Segment));
      GISdb[TrackDB].RedrawLayerOnMap;
      if (GISdb[TrackDB].MyData.RecordCount > 0) and AnswerIsYes('Save this segment') then begin
         OutName := ExtractFilePath(ChirpFileName) + ExtractFileNameNoExt(ChirpFileName) + '_seg_' + IntToStr(Segment) + Ext;
         GISdb[TrackDB].MyData.First;
         FirstRec := GISdb[TrackDB].MyData.GetFieldByNameAsInteger('PING');
         GISdb[TrackDB].MyData.Last;
         LastRec := GISdb[TrackDB].MyData.GetFieldByNameAsInteger('PING');

         AssignFile(f,OutName);
         Rewrite(f,1);

         StartProgressAbortOption('Subset');
         if ExtEquals(Ext,'.JSF') then begin
            reset(inf,1);
            x := 1;
            while not EOF(inf) do begin
               BlockRead(inf,jsfMessageHeader,16);
               UpDateProgressBar(x/NumSubbottomRecords);
               if (jsfMessageHeader.MessageType = 80) then begin
                  BlockRead(inf,MessageType80,SizeOf(MessageType80));
                  with MessageType80 do begin
                     BlockRead(inf,DataPulse,jsfMessageHeader.SizeToFollow - 240);
                     if (PingNumber >= FirstRec) and (PingNumber <= LastRec) then begin
                        BlockWrite(f,jsfMessageHeader,16);
                        BlockWrite(f,MessageType80,SizeOf(MessageType80));
                        BlockWrite(f,DataPulse,jsfMessageHeader.SizeToFollow - 240);
                     end;
                  end;
                  inc(x);
               end
               else begin
                  BlockRead(inf,DataPulse,jsfMessageHeader.SizeToFollow);
               end;
            end;
         end;
         CloseFile(f);
      end;
      GISdb[TrackDB].MyData.ApplyFilter('');
      GISdb[TrackDB].RedrawLayerOnMap;
   end;
   CloseFile(inf);
   EndProgress;
end;

procedure TChirpGraph.CheckBox1Click(Sender: TObject);
begin
   if CheckBox5.Checked then RedrawChirps;
end;

procedure TChirpGraph.CheckBox2Click(Sender: TObject);
begin
  inherited;
   if CheckBox5.Checked then RedrawChirps;
end;

procedure TChirpGraph.CheckBox4Click(Sender: TObject);
begin
  inherited;
  if CheckBox5.Checked then RedrawChirps;
end;

procedure TChirpGraph.CheckBox5Click(Sender: TObject);
begin
   if CheckBox5.Checked then RedrawChirps;
end;

procedure TChirpGraph.ChirpBitBtn1Click(Sender: TObject);
begin
  inherited;
   Subset1.Enabled := GISDB[TrackDB].MyData.FieldExists('SEGMENT');
   ChirpPopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TChirpGraph.Chirpoptions1Click(Sender: TObject);
begin
  inherited;
  ChirpPopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


{$IfDef ExFMX3D}
{$Else}
procedure MakeChirpFence(var FirstForm : tChirpGraph);
var
   i,ys,NP : integer;
   fName : PathStr;
begin
  {$IfDef RecordChirpFence} WriteLineToDebugFile('MakeChirpFence in'); {$EndIf}
   ShowHourglassCursor;
   MakeFenceDiagram := true;
   FenceDiagram := TView3DForm.Create(Application);
   FenceDiagram.ScaleViewToMapExtent(FirstForm.ChirpMapOwner.MapDraw);
   FenceDiagram.rMinZ := -(MDDef.ChirpReturnsToShow div FirstForm.YSkipSize);
   FenceDiagram.rMaxZ := 0;
   FenceDiagram.GrayScale := true;
   FenceDiagram.GroupBox1.Visible := false;
   ys := 256;

   fName := Palette256Bitmap(p256Gray);

   for i := 1 to 5 do FenceDiagram.DrapeFile[i] := fname;

   for i := 0 to pred(WMDEM.MDIChildCount) do begin
      if (WMDEM.MDIChildren[i] is tChirpGraph) and FileExists((WMDEM.MDIChildren[i] as tChirpGraph).ChirpFileName) then begin
         {$IfDef RecordChirpFence} WriteLineToDebugFile('Draw and export ' + IntToStr(FenceDiagram.CurCloud) + '  ' + WMDEM.MDIChildren[i].Caption); {$EndIf}
         NP := succ((WMDEM.MDIChildren[i] as tChirpGraph).XDupeSize * ((WMDEM.MDIChildren[i] as tChirpGraph).NumSubbottomRecords div
             (WMDEM.MDIChildren[i] as tChirpGraph).XSkipSize) * succ(MDDef.ChirpReturnsToShow div (WMDEM.MDIChildren[i] as tChirpGraph).YSkipSize));
         FenceDiagram.Initialize(NP + 100);
         (WMDEM.MDIChildren[i] as tChirpGraph).RedrawChirps;
         if (FenceDiagram.CurCloud = 1) then FenceDiagram.CheckBox2.Text := WMDEM.MDIChildren[i].Caption;
         if (FenceDiagram.CurCloud = 2) then FenceDiagram.CheckBox3.Text := WMDEM.MDIChildren[i].Caption;
         if (FenceDiagram.CurCloud = 3) then FenceDiagram.CheckBox4.Text := WMDEM.MDIChildren[i].Caption;
         if (FenceDiagram.CurCloud = 4) then FenceDiagram.CheckBox5.Text := WMDEM.MDIChildren[i].Caption;
         if (FenceDiagram.CurCloud = 5) then FenceDiagram.CheckBox6.Text := WMDEM.MDIChildren[i].Caption;
      end;
      if (FenceDiagram.CurCloud = 5) then break;
   end;
   FenceDiagram.CheckBox3.Visible := FenceDiagram.CurCloud > 1;
   FenceDiagram.CheckBox4.Visible := FenceDiagram.CurCloud > 2;
   FenceDiagram.CheckBox5.Visible := FenceDiagram.CurCloud > 3;
   FenceDiagram.CheckBox6.Visible := FenceDiagram.CurCloud > 4;
   FenceDiagram.Show;
   MakeFenceDiagram := false;
   ShowDefaultCursor;
  {$IfDef RecordChirpFence} WriteLineToDebugFile('MakeChirpFence out'); {$EndIf}
end;
{$EndIf}


procedure TChirpGraph.BitBtn1Click(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
       MakeChirpFence(Self);
   {$EndIf}
end;


procedure TChirpGraph.BitBtn3Click(Sender: TObject);
begin
  inherited;
  RedrawChirps;
end;

procedure TChirpGraph.SpinButton1DownClick(Sender: TObject);
begin
   MDDef.ChirpGain := MDDef.ChirpGain / 1.25;
   Label3.Caption := RealToString(MDDef.ChirpGain,-6,3);
   if CheckBox5.Checked then RedrawChirps;
end;


procedure TChirpGraph.SpinButton1UpClick(Sender: TObject);
begin
   MDDef.ChirpGain := MDDef.ChirpGain * 1.25;
   Label3.Caption := RealToString(MDDef.ChirpGain,-6,3);
   if CheckBox5.Checked then RedrawChirps;
end;


procedure TChirpGraph.Segment1Click(Sender: TObject);
begin
  inherited;
   GraphDoingWhat := SegmentProfile;
   GISdb[TrackDB].AddFieldToDataBase(ftInteger,'SEGMENT',3);
   OnSegment := 0;
   ShowDefaultCursor;
end;

procedure TChirpGraph.SpeedButton10Click(Sender: TObject);
begin
   Chirpsoptions1Click(Sender);
end;

procedure TChirpGraph.SpeedButton4Click(Sender: TObject);
begin
   Graphsettings2Click(Sender);
end;

procedure TChirpGraph.Chirpsoptions1Click(Sender: TObject);
var
   GetChirpOptions : ChirpOpt.tC;
begin
   GetChirpOptions := tC.Create(Application);
   with GetChirpOptions do begin
      RadioGroup1.ItemIndex := ord(FreqDisplay);
      RadioGroup2.ItemIndex := ord(TraceGrafs);
      Edit3.Text := RealToString(TopDepth,-8,2);
      Edit4.Text := RealToString(BaseDepth,-8,2);
      ShowModal;
      FreqDisplay := tFreqDisplay(RadioGroup1.ItemIndex);
      TraceGrafs := tTraceGrafs(RadioGroup2.ItemIndex);
      CheckEditString(Edit3.Text,TopDepth);
      CheckEditString(Edit4.Text,BaseDepth);
      Close;
   end;
end;

procedure TChirpGraph.Depths1Click(Sender: TObject);
begin
  inherited;
   ChangeChirpGraphDoing(cgdGetDepth);
end;

procedure TChirpGraph.Displayoptions1Click(Sender: TObject);
begin
   Chirpsoptions1Click(Sender);
end;

procedure TChirpGraph.Done1Click(Sender: TObject);
begin
  inherited;
  ChangeChirpGraphDoing(cgdNothing);
end;


procedure TChirpGraph.Returnstoshow1Click(Sender: TObject);
begin
  ReadDefault('Returns to show',MDDef.ChirpReturnsToShow);
end;


var
   topdepthvalue : float64;


procedure TChirpGraph.Image1DblClick(Sender: TObject);
var
   j,p1,p2 : integer;
   TStr : shortstring;

   procedure InsertZRecord(z : float64);
   begin
      with GISdb[ChirpZdb] do begin
          MyData.Insert;
          MyData.SetFieldByNameAsFloat('LAT',Lats[Lastx]);
          MyData.SetFieldByNameAsFloat('LONG',Longs[Lastx]);
          MyData.SetFieldByNameAsFloat('Z',z);
          MyData.SetFieldByNameAsString('SOURCE',ExtractFileName(ChirpFileName));
          MyData.Post;
          ScreenSymbol(Image1.Canvas,LastX,LastY,FilledBox,2,claRed);
      end;
   end;

begin
    if (GraphDoingWhat = SegmentProfile) then begin
       {$IfDef RecordChirpGraph} WriteLineToDebugFile('TChirpGraph.Image1DblClick, (GraphDoingWhat = SegmentProfile) in  segment=' + IntToStr(OnSegment)); {$EndIf}
       Image1.Canvas.Pen.Width := 3;
       Image1.Canvas.Pen.Color := clRed;
       Image1.Canvas.MoveTo(LastXM,0);
       Image1.Canvas.LineTo(LastXM,pred(Image1.Height));

       SegBreaks[OnSegment] := PingNum[LastXm];
       if (OnSegment = 0) then begin
          GISDB[TrackDB].FillFieldWithValue('SEGMENT','0');
       end
       else begin
          p1 := SegBreaks[pred(OnSegment)];
          p2 := SegBreaks[OnSegment];
          MinOfPairFirst(p1,p2);
          TStr := 'PING>=' + IntToStr(p1) + ' AND PING <=' + IntToStr(p2);
          {$IfDef RecordChirpGraph} WriteLineToDebugFile('filter=' + Tstr); {$EndIf}
          GISDB[TrackDB].MyData.ApplyFilter(TStr);
          GISDB[TrackDB].FillFieldWithValue('SEGMENT',IntToStr(OnSegment));
          GISDB[TrackDB].MyData.ApplyFilter('');
          GISDB[TrackDB].PlotFieldOnMap('SEGMENT',0,OnSegment);
       end;
       inc(OnSegment);
    end;
    if (GraphDoingWhat = ZonMap) then begin
      LastRoamLat := Lats[Lastx];
      LastRoamLong := Longs[Lastx];

      if (WmDEM.MDIChildCount > 0) then
        for j := WmDEM.MDIChildCount-1 downto 0 do
           if WmDEM.MDIChildren[j].Handle <> Self.Handle then
              PostMessage(WmDEM.MDIChildren[j].Handle,WM_BroadcastLatLongMessage,0,0);
       ApplicationProcessMessages;
       exit;
    end;
    if ValidDB(ChirpZdb) then begin
       if ChirpGraphDoing = cgdGetDepth then begin
          InsertZRecord(-JSFDepth(LastY));
       end;
       if (ChirpGraphDoing = cgdGetTop) then begin
          topdepthvalue := JSFDepth(LastY);
          ChangeChirpGraphDoing(cgdGetBase);
          ScreenSymbol(Image1.Canvas,LastX,LastY,FilledBox,2,claLime);
          FirstX := LastX;
          FirstY := LastY;
       end;
       if (ChirpGraphDoing = cgdGetBase) then begin
          InsertZRecord(abs(JSFDepth(LastY)-topdepthvalue));
          ChangeChirpGraphDoing(cgdGetTop);
       end;
    end;
end;

procedure TChirpGraph.SpeedButton6Click(Sender: TObject);
begin
  GraphDoingWhat := ZonMap;
end;

procedure TChirpGraph.SpeedButton8Click(Sender: TObject);
begin
  RedrawChirps;
end;


procedure TChirpGraph.GraphChirpRecord;
var
   Chirp : integer;

      procedure JSFGraph;
      var
         f : file;
         i : integer;
         jsfMessageHeader : tjsfMessageHeader;
         MessageType80 : tMessageType80;
         DataPulse : array[0..48000] of SmallInt;
         rp,ip : array[0..8000] of float32;
         v : tGraphPoint32;
         ThisGraph : tThisBaseGraph;
         rf : file;
      begin
         ShowHourglassCursor;
         AssignFile(f,ChirpFileName);
         reset(f,1);
         while not EOF(f) do begin
            BlockRead(f,jsfMessageHeader,16);
            if jsfMessageHeader.MessageType = 80 then begin
               BlockRead(f,MessageType80,SizeOf(MessageType80));
               with MessageType80 do begin
                  BlockRead(f,DataPulse,jsfMessageHeader.SizeToFollow - 240);   //DataSamplesInPacket*2);
                  if (PingNumber = Chirp) then begin
                     for I := 0 to pred(PulseInfo.DataSamplesInPacket) do begin
                        rp[i] := DataPulse[i*2] * Math.Power(2,-WeightFactor);
                        ip[i] := (DataPulse[succ(i*2)]) * Math.Power(2,-WeightFactor);
                     end;

                     ThisGraph := TThisBaseGraph.Create(Application);
                     ThisGraph.SetUpGraphForm;
                     ThisGraph.GraphDraw.HorizLabel := 'Return';
                     ThisGraph.GraphDraw.VertLabel := 'Depth (m)';
                     ThisGraph.OpenDataFile(rf);
                     ThisGraph.Caption := 'Intensity ping=' + IntToStr(Chirp);

                     for I := 0 to pred(PulseInfo.DataSamplesInPacket) do begin
                        v[2] := -i * PulseInfo.SampleIntNannosec / 10e9 * 1500;
                        v[1] := sqrt(sqr(rp[i]) + sqr(ip[i]));
                        BlockWrite(rf,v,1);
                     end;
                     CloseFile(rf);
                     ThisGraph.AutoScaleAndRedrawDiagram;
                  end;
               end;
            end
            else begin
               BlockRead(f,DataPulse,jsfMessageHeader.SizeToFollow);   //DataSamplesInPacket*2);
            end;
         end;
         CloseFile(f);
         ShowDefaultCursor;
      end;


begin
   Chirp := 1000;
   ReadDefault('Chirp record for display',Chirp);
   if FileExtEquals(ChirpFileName,'.JSF') then begin
      JSFGraph;
   end
end;


initialization
   Lastxm := MaxInt;
   ChirpGraphDoing := cgdNothing;
finalization
  {$IfDef RecordChirpGraph} WriteLineToDebugFile('RecordChirpGraph active in ChirpGrf'); {$EndIf}
  {$IfDef RecordChirpFence} WriteLineToDebugFile('RecordChirpFence active in ChirpGrf'); {$EndIf}
end.




