unit Sideimg;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$Define UseMemStream}


//{$Define UseInline}

//{$Define KleinIntegers}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define LogSidescanFileInfo}
   //{$Define LogMerge}
   //{$Define LogSidescanOpenAndDisplay}
   //{$Define LogMemStream}
   //{$Define LogPingLocations}
   //{$Define LogAllStructures}
   //{$Define LogSidescanPingInfo}
   //{$Define LogScaledImageDisplay}
{$EndIf}                                

interface

uses
//needed for inline of the core DB functions
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
//end core DB functions definitions

  SysUtils, Windows, Messages, Classes, Graphics, Controls,ClipBrd,
  StrUtils,System.Math,System.UITypes,
  Forms, Dialogs,  Menus, Buttons, ToolWin,
  Petmar_types,PETMAR,
  DEMMapf, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

const
   ColorIncr = 400;
   CornerNames : array [0..3] of shortstring = ('NW','NE','SW','SE');

type
   tFreqUp = (LowSideScanFreq,HighSideScanFreq,MergeFreq);

   tChanInfo = packed record
      TypeOfChannel,
      SubchannelNumber : byte;
      CorrectionFlags,            //1 is slant range, 2 is ground range (corrected)
      UniPolar,
      BytesPerSample   : word;    //1=8 bit, 2=16 bit, 4=32 bit
      Reserved         : Longword;
      ChannelName      : array[0..15] of ansichar;
      VoltScale,
      Frequency,
      HorizBeamAngle,
      TiltAngle,
      BeamWidth,
      OffsetX,
      OffsetY,
      OffsetZ,
      OffsetYaw,
      OffsetPitch,
      OffsetRoll   : Single;
      ReservedArea : array[1..56] of byte;
   end;

   tXTFFileHeader = packed record            //1024 bytes total
      FileFormat,                              //123
      SystemType : byte;                       //1
      RecordingProgramName : array[1..8] of ansichar;
      RecordingProgramVersion : array[1..8] of ansichar;
      SonarName : array[1..16] of ansichar;
      SonarType : word;
      NoteString : array[1..64] of ansichar;
      ThisFileName : array[1..64] of ansichar;
      NavUnits,
      NumberOfSonarChannels,
      NumberOfBathymetryChannels : word;
      Reserved  : array[1..6] of word;
      ProjectionType : array[1..12] of byte;
      SpheroidType : array[1..10] of byte;
      NavigationLatency : integer;
      OriginY,
      OriginX,
      NavOffsetY,
      NavOffsetX,
      NavOffsetZ,
      NavOffsetYaw,
      MRUOffsetY,
      MRUOffsetX,
      MRUOffsetZ,
      MRUOffsetYaw,
      MRUOffsetPitch,
      MRUOffsetRoll : single;
      ChanInfo : array[1..6] of tChanInfo;
   end;

   tXTFPingHeader = packed Record    //total 256 bytes
      MagicNumber                  : word;   //64206
      HeaderType,SubChannelNumber  : byte;
      NumChansToFollow : word;
      Reserved : array[1..2] of word;
      NumBytesThisRecord : LongWord;
      Year  : word;
      Month,
      Day,
      Hour,
      Minute,
      Second,
      HSeconds  : byte;
      JulianDay : word;
      EventNumber : LongWord;
      PingNumber : LongWord;
      SoundVelocity,
      OceanTide : single;
      Reserved2 : LongWord;
      ConductivityFreq,
      TemperatureFreq,
      PressureFreq,
      PressureTemp,
      Conductivity,
      WaterTemperature,
      Pressure,
      ComputedSoundVelocity,
      MagX,
      MagY,
      MagZ : single;
      AuxVal : array[1..6] of single;
      SpeedLog,
      Turbidity,
      ShipSpeed,
      ShipGyro : single;
      ShipYcoordinate,
      ShipXcoordinate : double;
      ShipAltitude,
      ShipDepth       : Word;
      FixTimeHour,
      FixTimeMinute,
      FixTimeSecond,
      FixTimeHSecond   : byte;
      SensorSpeed,
      KP : single;
      SensorYcoordinate,
      SensorXcoordinate  : double;
      SonarStatus,
      RangeToFish,
      BearingToFish,
      CableOut           : word;
      Layback,
      CableTension,
      SensorDepth,
      SensorPrimaryAltitude,
      SensorAuxAltitude,
      SensorPitch,
      SensorRoll,
      SensorHeading,
      Heave,
      Yaw : single;
      AttitudeTimeTag : LongWord;
      DOT  : single;
      NavFixMillisends : longWord;
      ComputerClockHOur,
      ComputerClockMinute,
      ComputerClocSecond,
      ComputerClockHSec : byte;
      FishPositionDeltaX,
      FishPositionDeltaY : int16;
      FishPositionErrorCode : AnsiChar;
      ReservedSpace2 : array[1..11] of byte;
   end;

   tXTFPingChanHeader = packed record      //total 64 bytes
       ChannelNumber,                         //0       0=port low, 1=stb low, 2=port high, 3=port low
       DownSampleMethod : word;               //2       2=Max, 4=RMS
       SlantRange,                            //4
       GroundRange,                           //8
       TimeDelay,                             //12
       TimeDuration,                          //16
       SecondsPerPing : single;               //20
       ProcessingFlags,                       //24
       Frequency,                             //26
       InitialGainCode,                       //28
       GainCode,                              //30
       BandWidth : word;                      //32
       ContactNumber : LongWord;              //34
       ContactClassification : word;          //38
       ContactSubNumer,                       //40
       ContactType  : byte;                   //41
       NumSamples : LongWord;                 //42, number of samples in the channel
       MilliVoltScale : word;                 //46
       ContactTimeOffTrack : single;          //48
       ContactCloseNumber,                    //52
       Reserved2 : byte;                      //53
       FixedVSOP : single;                    //54
       ReservedSpace : array[1..6] of byte;   //58
   end;

const
   VelocitySound = 1500;
   MaxBytesRecord = 64000;

type
  Tsideimage = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Calculate1: TMenuItem;
    Distance1: TMenuItem;
    Close1: TMenuItem;
    Twowaytraveltime1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Saveimage1: TMenuItem;
    Subset1: TMenuItem;
    Sidescanoptions1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Displayoptions1: TMenuItem;
    Displayoptions2: TMenuItem;
    N1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Saveimage2: TMenuItem;
    //Exportasregisteredimage1: TMenuItem;
    Makethisthefirstrecord1: TMenuItem;
    Makethisthelastrecorddisplayed1: TMenuItem;
    Entirerecord1: TMenuItem;
    Fullresolutiondisplay1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Grap1: TMenuItem;
    Showcoverageonmap1: TMenuItem;
    BitBtn1: TBitBtn;
    ExportcurrentsubsetXTF1: TMenuItem;
    Savelabelledimage1: TMenuItem;
    Labelcorners1: TMenuItem;
    Inserttargetrecord1: TMenuItem;
    Histogram1: TMenuItem;
    ComboBox1: TComboBox;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    N2: TMenuItem;
    N3: TMenuItem;
    ExportKML1: TMenuItem;
    Mapinfo1: TMenuItem;
    N4: TMenuItem;
    Exportsanitizedcoordinates1: TMenuItem;
    MeasureDistanceSpeedButton14: TSpeedButton;
    XTFfileheader1: TMenuItem;
    Merge1: TMenuItem;
    OpenhistogramDB1: TMenuItem;
    Showpingtable1: TMenuItem;
    RedrawSpeedButton12: TSpeedButton;
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Displayoptions1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Distance1Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Close1Click(Sender: TObject);
    procedure Twowaytraveltime1Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Subset1Click(Sender: TObject);
    procedure Displayoptions2Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Makethisthefirstrecord1Click(Sender: TObject);
    procedure Makethisthelastrecorddisplayed1Click(Sender: TObject);
    procedure Entirerecord1Click(Sender: TObject);
    procedure Fullresolutiondisplay1Click(Sender: TObject);
    procedure Grap1Click(Sender: TObject);
    procedure Showcoverageonmap1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ExportcurrentsubsetXTF1Click(Sender: TObject);
    procedure Savelabelledimage1Click(Sender: TObject);
    procedure Labelcorners1Click(Sender: TObject);
    procedure Inserttargetrecord1Click(Sender: TObject);
    procedure Histogram1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure ExportKML1Click(Sender: TObject);
    procedure Mapinfo1Click(Sender: TObject);
    procedure Exportsanitizedcoordinates1Click(Sender: TObject);
    procedure MeasureDistanceSpeedButton14Click(Sender: TObject);
    procedure XTFfileheader1Click(Sender: TObject);
    procedure Merge1Click(Sender: TObject);
    procedure OpenhistogramDB1Click(Sender: TObject);
    procedure Showpingtable1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
  private
    { Private declarations }
    ImageXSize,ImageYSize : integer;
    CornerLats,CornerLongs : array[0..3] of float64;
    RowPing : array[0..MaxScreenYMax] of Integer;
    DataFileName : PathStr;
    XTFheader : tXTFFileHeader;
    XTFPingHeader : tXTFPingHeader;
    XTFPingChanHeader :  tXTFPingChanHeader;
    DataRecByte : packed array[0..MaxBytesRecord] of byte;
    DataRecInt16 : packed array[0..MaxBytesRecord] of int16;
{$ifDef KleinIntegers}
    DataRecInt32,DataRecInt32_2 : packed array[0..MaxBytesRecord] of int32;   //single;
{$Else}
    DataRecInt32,DataRecInt32_2 : packed array[0..MaxBytesRecord] of single;
{$EndIf}
    dbFile  : tMyData;
    TargetGIS : integer;
    {$IfDef UseMemStream}
    MemStream : tMemoryStream;
    procedure LoadMemStreamAndReadFileHeader;
    procedure CloseMemStream;
    {$EndIf}
    procedure SplitBigFile;
    procedure Subset(StartRecord,EndRecord : integer; xshift : float64 = 0; yshift : float64 = 0);
    procedure CheckSideScanOptions;
    function GetPingReturns : boolean;
    function FindNextPingHeader(Found: tStringList; ZeroOnly : boolean) : boolean;
    procedure Histogram;
    function ReadXTFPing : boolean; {$IfDef UseInline} inline; {$EndIf}
  public
    { Public declarations }
    AnOverviewImage,DualFreq  : boolean;
    RowLats,RowLongs : array[0..MaxScreenYMax] of float64;
    MapOwner : tMapForm;
    FreqUp : tFreqUp;
    DBFileName,FrqFileName : PathStr;
    LowDisplayValue,HighDisplayValue,
    FirstPingDisplayed,LastPingDisplayed,PingsDisplayed,
    PingRepeats,PingThinning,AcrossTrackThinning,SlantRange,
    SidescanRange,sx,sy,ImageStart,ImageEnds,BytesPerSample,
    FirstSSPing,LastSSPing,NumSSRecords : integer;
    AlongTrackPixelSize,AcrossTrackPixelSize,
    StraightLineDist,
    SidescanRunlength,LineHeading : float64;
    PingTime,StartTime,EndTime : shortstring;
    MultipleRangeInFile,
    NoResizing,
    MouseIsDown : Boolean;
    ReverseGrayscale,
    ReverseImageDirection : boolean;
    procedure DisplayImage;
    procedure RangeFromPercentiles;
    function SideScanImageStats: tStringList;
    function OpenSideScan(var InName : PathStr; var NumRec : integer; StartDisplay : boolean = true) : boolean;
  end;

var
  bName : NameStr;
  LowGain,HighGain,FishHeight,
  MinSlantRange : float64;
  BotVal     : integer;
  SideIndexDB,
  LowColor,HighColor : integer;
  AvgMetersPerPing : float64;
  SideScanColorTable : array[0..255] of tRGBTriple;


procedure SetSideScanColorTable;
procedure XTFFileInfo;
procedure LoadSideScanFile(Owner : tMapForm; fName : PathStr);


implementation

{$R *.DFM}

uses
   DEMEros,
   PETMath,PetImage,
   BaseGraf,
   ss_options,
   DataBaseCreate,
   DEMDataBase,
   BaseMap,
   KML_opts,KML_creator,
   DEMDefs,DEMCoord,PetDBUtils, Make_tables,

{Main program MDI window for different programs that use this module}
   Nevadia_Main, toggle_db_use;
{End of the MDI parent declaration}


type
   tDoingWhat = (JustWandering,FirstDistancePoint,SecondDistancePoint,FirstTWTTPoint,SecondTWTTPoint,GraphicalSubset,GraphicalFileSubset);
var
   Firstx,Firsty,
   lastx,lasty : integer;
   DoingWhat   : tDoingWhat;
   Freq : array[0..255] of LongInt;
   NormalColor,
   DisplayUpsideDown,CopyImage,Starboard : boolean;
   Max,ColSkip,i,x,
   ColsSidebySide : integer;
   FName : PathStr;

const
   MaxSideImage = 50;
var
   SideImage : array[1..MaxSideImage] of TSideImage;
   NumSideImages : integer;


procedure LoadSideScanFile(Owner : tMapForm; fName : PathStr);
var
   NUmRec : integer;
begin
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('LoadSideScanFile in ' + fName); {$EndIf}
    InputSideScanFileName := fName;
    inc(NumSideImages);

    SideImage[NumSideImages] := TSideImage.Create(Application);
    if SideImage[NumSideImages].OpenSideScan(InputSideScanFileName,NumRec) then begin
      {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('opensidecan success'); {$EndIf}
       SideImage[NumSideImages].MapOwner := Owner;
       SideImage[NumSideImages].Fullresolutiondisplay1Click(Nil);
       if (SideImage[NumSideImages].LineHeading > 135) and (SideImage[NumSideImages].LineHeading < 215) then SideImage[NumSideImages].ReverseImageDirection := true;
       SideImage[NumSideImages].Fullresolutiondisplay1Click(Nil);
       SideImage[NumSideImages].Showcoverageonmap1Click(Nil);
    end
    else begin
       {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('LoadSideScanFile open fail ' + fName); {$EndIf}
       SideImage[NumSideImages].close;
    end;
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('LoadSideScanFile out'); {$EndIf}
end;


function ArrayOfCharToString(Chars : array of AnsiChar; StringLong : integer; StartChar : integer = 0) : string;
var
   i : integer;
begin
   Result := '';
   for i := StartChar to pred(StartChar + StringLong) do if (Chars[i] <> #0) then Result := result + Chars[i];
end;


procedure MergeXTF;
var
   FirstFile,SecondFile,FileName : PathStr;
   OutFile,
   File1,File2  : file;
   DefFilter : byte;
   i : integer;
   XTFheader : tXTFFileHeader;
   XTFPingHeader : tXTFPingHeader;
   DataRecInt16 : packed array[0..MaxBytesRecord] of int16;
   FilesWanted : TStringList;
begin
   {$IfDef LogMerge} WriteLineToDebugFile('MergeXTF in'); {$EndIf}
   DefFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(SideDataPath);
   if GetMultipleFiles('XTF files to merge','XTF record|*.XTF', FilesWanted,DefFilter) then begin
      FileName := Petmar.NextFileNumber(ExtractFilePath(FilesWanted.Strings[0]), ExtractFileNameNoExt(FilesWanted.Strings[0]) + '_merge_', '.xtf');
      GetFileNameDefaultExt('new record','XTF record|*.XTF',FileName);
      {$IfDef LogMerge} WriteLineToDebugFile('Output=' + FileName); {$EndIf}

      assignFile(Outfile,Filename);
      rewrite(Outfile,1);
      ShowHourglassCursor;

      for i := 1 to FilesWanted.Count do begin
         FirstFile := FilesWanted.Strings[pred(i)];
         {$IfDef LogMerge} WriteLineToDebugFile('Add=' + FirstFile); {$EndIf}
         assignFile(File1,FirstFile);
         reset(File1,1);

         BlockRead(File1,XTFheader,Sizeof(XTFheader));
         if (i = 1) then BlockWrite(Outfile,XTFheader,Sizeof(XTFheader));
         while not eof(File1) do begin
            BlockRead(File1,XTFPingHeader,Sizeof(XTFPingHeader));
            BlockRead(File1,DataRecInt16,XTFPingHeader.NumBytesThisRecord - Sizeof(XTFPingHeader));
            BlockWrite(OutFile,XTFPingHeader,Sizeof(XTFPingHeader));
            BlockWrite(OutFile,DataRecInt16,XTFPingHeader.NumBytesThisRecord - Sizeof(XTFPingHeader));
         end;
         CloseFile(File1);
      end;
      CloseFile(Outfile);
      FilesWanted.Free;
   end;
   ShowDefaultCursor;
   {$IfDef LogMerge} WriteLineToDebugFile('MergeXTF out'); {$EndIf}
end;


function Tsideimage.ReadXTFPing : boolean;
var
  j,PingDataSize,StartMemPos : integer;
      b : array[1..4] of byte;
      value : int32;
begin
   if FindNextPingHeader(Nil,true) and (XTFPingHeader.HeaderType = 0) then begin
       StartMemPos := MemStream.Position;
       SlantRange := round(XTFPingChanHeader.SlantRange);
       Result := true;
       for j := 1 to XTFPingHeader.NumChansToFollow do begin
           if StartMemPos +Sizeof(tXTFPingChanHeader) > MemStream.Size then begin
              Result := false;
              exit;
           end;

           MemStream.Read(XTFPingChanHeader,Sizeof(tXTFPingChanHeader));
           PingDataSize := XTFPingChanHeader.NumSamples * XTFHeader.ChanInfo[1].BytesPerSample;
           {$IfDef LogSidescanPingInfo}
              if false then begin
                WriteLineToDebugFile('channel number=' + IntToStr(XTFPingChanHeader.ChannelNumber));
                WriteLineToDebugFile('slant range=' + RealToString(XTFPingChanHeader.SlantRange,-12,-2));
                WriteLineToDebugFile('ground range=' + RealToString(XTFPingChanHeader.GroundRange,-12,-2));
                WriteLineToDebugFile('NumSamples=' + IntToStr(XTFPingChanHeader.NumSamples));
             end;
          {$EndIf}

           if StartMemPos +Sizeof(tXTFPingChanHeader) + PingDataSize > MemStream.Size then begin
              Result := false;
              exit;
           end;
           if (XTFHeader.ChanInfo[1].BytesPerSample = 4) then begin
               if (j = 1) then MemStream.Read(DataRecInt32[0],PingDataSize)
               else if (j = 2) then MemStream.Read(DataRecInt32[XTFPingChanHeader.NumSamples],PingDataSize)
               else if (j = 3) then MemStream.Read(DataRecInt32_2[0],PingDataSize)
               else if (j = 4) then MemStream.Read(DataRecInt32_2[XTFPingChanHeader.NumSamples],PingDataSize);
           end
           else if (XTFHeader.ChanInfo[1].BytesPerSample = 2) then begin
               if (j = 1) then MemStream.Read(DataRecInt16[0],PingDataSize)
               else if (j = 2) then MemStream.Read(DataRecInt16[XTFPingChanHeader.NumSamples],PingDataSize);
           end
           else begin
              if (j = 1) then MemStream.Read(DataRecByte,PingDataSize)
              else if (j = 2) then MemStream.Read(DataRecByte[XTFPingChanHeader.NumSamples],PingDataSize);
           end {for j};
       end;

       {$IfDef KleinIntegers}
          if (XTFHeader.ChanInfo[1].BytesPerSample = 4) then begin
             for j := 0 to pred(2*XTFPingChanHeader.NumSamples) do begin
                Int4swap(DataRecInt32[j]);
                Int4swap(DataRecInt32_2[j]);
                DataRecInt32[j] := (DataRecInt32[j]) shr 4;
                DataRecInt32_2[j] := (DataRecInt32_2[j]) shr 4;
             end;
          end;
       {$EndIf}
       MemStream.Position := StartMemPos + XTFPingHeader.NumBytesThisRecord - Sizeof(tXTFPingHeader);
   end
   else begin
       Result := false;
   end;
end;



procedure Tsideimage.RedrawSpeedButton12Click(Sender: TObject);
begin
   DisplayImage;
end;


procedure Tsideimage.Histogram;
{$IfDef UseMemStream}
label
  Done;
const
   MinRef = 0;
   MaxRef = 256000;
type
  tBigArray = array[MinRef..MaxRef] of int32;
var
   i,j,xpic : integer;
   Hist : ^tBigArray;
   Total,Cum : int64;
   Hists : tStringList;
begin
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Enter sideimage.Histogram'); {$EndIf}
   ShowHourglassCursor;
   LoadMemStreamAndReadFileHeader;
   if not (XTFHeader.ChanInfo[1].BytesPerSample in [2,4]) then exit;
   Total := 0;
   New(Hist);
   for i := MinRef to MaxRef do begin
      Hist^[i] := 0;
   end;
   while (MemStream.Position < (MemStream.Size - 256)) do begin
      ReadXTFPing;
      if (XTFHeader.ChanInfo[1].BytesPerSample = 4) then begin
         for xpic := 0 to pred(2 * XTFPingChanHeader.NumSamples) do begin
            if (DataRecInt32[xpic] >= MinRef) and  (DataRecInt32[xpic] <= MaxRef) then begin
               inc(Hist^[round(DataRecInt32[xpic])]);
               Inc(Total);
            end;
         end;
      end;
      if (XTFHeader.ChanInfo[1].BytesPerSample = 2) then begin
         for xpic := 0 to pred(2 * XTFPingChanHeader.NumSamples) do begin
            if (DataRecInt16[xpic] >= MinRef) and (DataRecInt16[xpic] <= MaxRef) then begin
               inc(Hist^[DataRecInt16[xpic]]);
               Inc(Total);
            end;
         end;
      end;
   end;
   CloseMemStream;
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Hist found'); {$EndIf}

   Hists := tStringList.Create;
   Hists.Add('DN,NPTS,PERCENTILE');
   Cum := 0;
   for i := MinRef to MaxRef do begin
      if (Hist^[i] > 0) then begin
         Cum := Cum + Hist^[i];
         Hists.Add(IntToStr(i) + ',' + IntToStr(Hist^[i]) + ',' + RealToString(100* Cum / Total,-12,-2) {+ ',' + IntToStr(Hist[-i])} );
      end;
   end;
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('StringList Filled, n=' + IntToStr(Hists.Count)); {$EndIf}
   PetDBUtils.StringList2CSVtoDB(Hists,FrqFileName,true);
   Dispose(Hist);
   ShowDefaultCursor;

   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Exit sideimage.Histogram'); {$EndIf}
{$Else}
begin
{$EndIf}
end;


procedure Tsideimage.RangeFromPercentiles;
var
   Table : tMyData;
   pc : float64;
begin
    if MDDef.SonarMapDef.MinPC < 0 then MDDef.SonarMapDef.MinPC := 0;
    if MDDef.SonarMapDef.MaxPC > 100 then MDDef.SonarMapDef.MaxPC := 100;
    if not FileExists(FrqFileName) then Histogram;
    if FileExists(FrqFileName) then begin
       Table := tMyData.Create(FrqFileName);
       if not Table.FieldExists('PERCENTILE') then begin
          Table.Destroy;
          Histogram;
          Table := tMyData.Create(FrqFileName);
       end;

       repeat
          PC := Table.GetFieldByNameAsFloat('PERCENTILE');
          LowDisplayValue := Table.GetFieldByNameAsInteger('DN');
          Table.Next;
       until PC >= MDDef.SonarMapDef.MinPC;
       Table.Last;
       repeat
          PC := Table.GetFieldByNameAsFloat('PERCENTILE');
          HighDisplayValue := Table.GetFieldByNameAsInteger('DN');
          Table.Prior;
       until PC <= MDDef.SonarMapDef.MaxPC;
       Table.Destroy;
    end;
end;


procedure Tsideimage.OpenhistogramDB1Click(Sender: TObject);
begin
   MapOwner.OpenDBonMap('',FrqFileName);
   Self.BringToFront;
end;


function Tsideimage.OpenSideScan(var InName : PathStr; var NumRec : integer; StartDisplay : boolean = true) : boolean;
{$IfDef ExSidescan}
begin
{$Else}
const
   XTFMaxSize = 1200000000;
var
   LocalPath: PathStr;
   Dir         : DirStr;
   Ext         : ExtStr;
   SizeRead,Pings : integer;
   LastLat,LastLong,
   Lat,Long: float64;
   MakeNewDBF,
   First     : boolean;
   DataFile  : file;
   IndexTable : tMyData;


        procedure AddPingToFile;
        begin
           if MakeNewDBF then begin
              Lat := XTFPingHeader.SensorYcoordinate;
              Long := XTFPingHeader.SensorXcoordinate;
              if (abs(Lat) > 0.001) and (abs(Long) > 0.0001) and (abs(Lat) < 90) and (abs(Long) < 180) then begin
                  if (abs(Lat-LastLat) > 0.0000001) and (abs(Long-LastLong) > 0.0000001) then begin
                     dbFile.Insert;
                     dbFile.SetFieldByNameAsFloat('LAT',Lat);
                     dbFile.SetFieldByNameAsFloat('LONG',Long);
                     dbFile.SetFieldByNameAsString('TIME',CreateTimeString(XTFPingHeader.Hour,XTFPingHeader.Minute,XTFPingHeader.Second));
                     dbFile.SetFieldByNameAsInteger('PING',XTFPingHeader.PingNumber);
                     dbFile.SetFieldByNameAsFloat('SPEED',XTFPingHeader.ShipSpeed);
                     dbFile.SetFieldByNameAsFloat('HEADING',XTFPingHeader.ShipGyro);
                     dbFile.SetFieldByNameAsFloat('FISH_HEAD',XTFPingHeader.SensorHeading);
                     dbFile.SetFieldByNameAsFloat('FISH_DEPTH',XTFPingHeader.SensorDepth);
                     if (SlantRange <> 0) then dbFile.SetFieldByNameAsInteger('SLANT_RNG',SlantRange);
                     dbFile.SetFieldByNameAsFloat('WATER_TEMP',XTFPingHeader.WaterTemperature);
                     dbFile.SetFieldByNameAsFloat('PRESSURE',XTFPingHeader.Pressure);
                     dbFile.SetFieldByNameAsFloat('FISH_DEPTH',XTFPingHeader.SensorDepth);
                     dbFile.SetFieldByNameAsFloat('FISH_PITCH',XTFPingHeader.SensorPitch);
                     dbFile.SetFieldByNameAsFloat('FISH_ROLL',XTFPingHeader.SensorRoll);
                     dbFile.SetFieldByNameAsFloat('FISH_ALT',XTFPingHeader.SensorPrimaryAltitude);
                     dbFile.Post;
                  end;
              end;
           end;
           LastLat := Lat;
           LastLong := Long;
        end;


        procedure FindMagicNumbers;
        var
           Found : tStringList;
        begin
           {$IfDef UseMemStream}
              LoadMemStreamAndReadFileHeader;
              Found := tStringList.Create;
              while (MemStream.Position < MemStream.Size - 256) do begin
                 FindNextPingHeader(Found,false);
              end;
              CloseMemStream;
              Petmar.DisplayAndPurgeStringList(Found,'XTF packets');
           {$EndIf}
        end;

begin
   {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Enter sideimage.OpenSideScan ' + InName); {$EndIf}
   LocalPath := '';
   if FileExists(InName) then begin
   end
   else begin
     InName := SideDataPath;
     if not GetFileMultipleMask('side scan records','Any sidescan image|*.XTF|ISIS XTF|*.XTF',InName,MDDef.SonarMapDef.SideDefFilter) then begin
        Result := false;
        exit;
     end;
     SideDataPath := ExtractFilePath(InName);
   end;
   DataFileName := InName;

   Result := true;
   FSplit(InName,Dir,bName,Ext);
   InsureFileIsNotReadOnly(InName);

   DBFileName := ChangeFileExt(InName,DefaultDBExt);
   FrqFileName := ChangeFileExt(InName,'_frq' + DefaultDBExt);

   MakeNewDBF := not FileExists(DBFileName);

   if ExtEquals(Ext,'.XTF') then begin
      {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Doing XTF file ' + bName); {$EndIf}

      if MakeNewDBF then begin
         {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('MakeNewDBF ' + DBFileName); {$EndIf}
         SlantRange := 0;
         LastLat := 999;
         LastLong := 999;
         LoadMemStreamAndReadFileHeader;

          if (XTFHeader.FileFormat <> 123) and (XTFHeader.SystemType <> 1) then begin
             WriteLineToDebugFile(ExtractFileNameNoExt(InName) + '  format=' + IntToStr(XTFHeader.FileFormat));
             CloseMemStream;
             exit;
          end;
          Make_Tables.MakeSideScanRecordFile(DBFileName);
          dbFile := tMyData.Create(DBFileName);
          ShowHourglassCursor;
          Pings := 0;
          while ReadXTFPing do begin
             if (Pings Mod 500 = 0) then wmdem.SetPanelText(0,IntToStr(Pings));
             AddPingToFile;
             inc(Pings);
          end;
          CloseMemStream;
          ShowDefaultCursor;
         {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('done MakeNewDBF'); {$EndIf}
      end
      else begin
         {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Open existing DBF ' + DBFileName); {$EndIf}
         dbFile := tMyData.Create(DBFileName);
      end;


      dbFile.LengthPointRecords(SidescanRunlength,StraightLineDist,LineHeading);
      dbFile.First;
      MultipleRangeInFile := dbFile.NumUniqueEntriesInDB('SLANT_RNG') > 1;

      if (GetFileSize(InName) > XTFMaxSize) then begin
         SplitBigFile;
         MessageToContinue('File split');
         exit;
      end;

      if (not StartDisplay) then exit;

      {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('File size= ' + Petmar_Types.SmartMemorySize(GetFileSize(InName))); {$EndIf}

      if (GetFileSize(InName) > XTFMaxSize) and AnswerIsYes('Safe read') then begin
         {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Picked safe read');  {$EndIf}
      end
      else  begin
         {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Picked LoadMemStream'); {$EndIf}
          LoadMemStreamAndReadFileHeader;
          DualFreq := XTFHeader.NumberOfSonarChannels = 4;
          SpeedButton8.Enabled := DualFreq;
          SpeedButton9.Enabled := DualFreq;

         {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Loaded LoadMemStream'); {$EndIf}

          if FileExists(SidescanIndexFName) then begin
             IndexTable := tMyData.Create(SidescanIndexFName);
             IndexTable.ApplyFilter( 'FILENAME=' + QuotedStr(InName));
             if (IndexTable.RecordCount = 1) then  begin
                SidescanRange := IndexTable.GetFieldByNameAsInteger('RANGE');
                SidescanRunlength := IndexTable.GetFieldByNameAsFloat('LENGTH');
                MDDef.SonarMapDef.SidescanLayback := IndexTable.GetFieldByNameAsInteger('LAYBACK');
                LineHeading := IndexTable.GetFieldByNameAsFloat('HEADING');
                GetCoverageCorners(IndexTable,CornerLats,CornerLongs);
             end;
             FreeAndNil(IndexTable);
          end
          else SidescanRange := -99;

          First := true;
          NumSSRecords := 0;

          repeat
             if ReadXTFPing then begin
                if First then begin
                   FirstSSPing := XTFPingHeader.PingNumber;
                   First := false;
                end;
                if (XTFPingHeader.PingNumber > FirstSSPing) then LastSSPing := XTFPingHeader.PingNumber;
                inc(NumSSRecords);
             end;
          until (MemStream.Position > MemStream.Size - 256);

          {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('File finished'); {$EndIf}
          RangeFromPercentiles;
          CloseMemStream;
          FirstPingDisplayed := FirstSSPing;
          LastPingDisplayed := LastSSPing;
          AcrossTrackThinning := 2;
          FreqUp := LowSideScanFreq;
          PingThinning := 2;
      end;
   end
   else begin
      Result := false;
      MessageToContinue('Invalid file');
   end;

   if (XTFHeader.ChanInfo[1].BytesPerSample <> 2) then begin
      SpeedButton11.Enabled := false;
      SpeedButton12.Enabled := false;
      SpeedButton13.Enabled := false;
      SpeedButton10.Enabled := false;
   end;

   {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Exit sideimage.OpenSideScan'); {$EndIf}
{$EndIf}
end;


procedure Tsideimage.SplitBigFile;
label
   FileFull;
var
   OutName,InName : PathStr;
   i,RecsRead : integer;
   DataFile, OutFile : file;
begin
   Inname := DataFileName;
   assignFile(DataFile,InName);
   reset(DataFile,1);
   BlockRead(DataFile,XTFheader,Sizeof(XTFheader));
   i := 1;

   while not eof(DataFile) do  begin
     outName := ExtractFileNameNoExt(DataFileName) + '_part_' + IntToStr(i) + '.xtf';
     assignFile(Outfile,OutName);
     rewrite(Outfile,1);
     BlockWrite(Outfile,XTFheader,Sizeof(XTFheader));

     ShowHourglassCursor;
     RecsRead := 0;
     repeat
         while not eof(DataFile) do begin
            inc(RecsRead);
            if (RecsRead Mod 500 = 0) then wmdem.SetPanelText(0,IntToStr(i) + '/' + IntToStr(RecsRead));

            BlockRead(DataFile,XTFPingHeader,Sizeof(XTFPingHeader));
            BlockRead(DataFile,DataRecInt16,XTFPingHeader.NumBytesThisRecord - Sizeof(XTFPingHeader));

            BlockWrite(OutFile,XTFPingHeader,Sizeof(XTFPingHeader));
            BlockWrite(OutFile,DataRecInt16,XTFPingHeader.NumBytesThisRecord - Sizeof(XTFPingHeader));
            if (RecsRead >= 12000) then goto FileFull;
         end;
          InName[length(InName)-4] := succ(InName[length(InName)-4]);
          if FileExists(InName) then begin
             CloseFile(DataFile);
             AssignFile(DataFile,InName);
             reset(DataFile,1);
          end;
     until not FileExists(InName);
     FileFull:;
     CloseFile(Outfile);
     inc(i);
   end;

   CloseFile(DataFile);
   ShowDefaultCursor;
   wmdem.SetPanelText(0,'');
end;


procedure Tsideimage.Showcoverageonmap1Click(Sender: TObject);
var
   BoatLat,BoatLong,FishLat,FishLong,Heading,Lat,Long,Range : float64;
   xp,yp,xc,yc : integer;
   Bitmap : tMyBitmap;
begin
   if (dbFile <> Nil) then begin
      CopyImageToBitmap(MapOwner.Image1,Bitmap);
      CloneImageToBitmap(MapOwner.Image1,Bitmap);
      dbFile.First;
      while not dbFile.EOF do begin
        BoatLat := dbfile.GetFieldByNameAsFloat('LAT');
        BoatLong := dbfile.GetFieldByNameAsFloat('LONG');
        Heading := dbfile.GetFieldByNameAsFloat('HEADING');
        Range := dbfile.GetFieldByNameAsFloat('SLANT_RNG');
        VincentyPointAtDistanceBearing(BoatLat,BoatLong,MDDef.SonarMapDef.SidescanLayback,Heading,FishLat,FishLong);

        MapOwner.MapDraw.LatLongDegreeToScreen(FishLat,FishLong,xc,yc);

        VincentyPointAtDistanceBearing(FishLat,FishLong,Range,Heading+90,Lat,Long);

        if True then begin
           MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
           Bitmap.Canvas.Pen.Color := clLime;
           PetImage.DrawLine(Bitmap,xc,yc,xp,yp);
        end
        else begin
           MapOwner.MapDraw.MapSymbolAtLatLongDegree(Bitmap.Canvas,Lat,Long,FilledBox,2,claLime);
        end;
        VincentyPointAtDistanceBearing(FishLat,FishLong,Range,Heading-90,Lat,Long);
        if True then begin
           MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
           Bitmap.Canvas.Pen.Color := clRed;
           PetImage.DrawLine(Bitmap,xc,yc,xp,yp);
        end
        else begin
           MapOwner.MapDraw.MapSymbolAtLatLongDegree(Bitmap.Canvas,Lat,Long,FilledBox,2,claRed);
        end;
        dbfile.Next;
      end;
      if True then begin
        MapOwner.IHSmergeOntoMap(Bitmap);
      end
      else begin
         MapOwner.Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
      end;
   end;
end;

procedure Tsideimage.Showpingtable1Click(Sender: TObject);
begin
   MapOwner.OpenDBonMap('',dbFile.TableName);
   Self.BringToFront;
end;

procedure Tsideimage.SpeedButton10Click(Sender: TObject);
begin
   MDDef.SonarMapDef.MinPC := MDDef.SonarMapDef.MinPC - 2;
   DisplayImage;
end;

procedure Tsideimage.SpeedButton11Click(Sender: TObject);
begin
   MDDef.SonarMapDef.MinPC := MDDef.SonarMapDef.MinPC + 2;
   DisplayImage;
end;

procedure Tsideimage.SpeedButton12Click(Sender: TObject);
begin
   MDDef.SonarMapDef.MaxPC := MDDef.SonarMapDef.MaxPC - 2;
   DisplayImage;
end;

procedure Tsideimage.SpeedButton13Click(Sender: TObject);
begin
   MDDef.SonarMapDef.MaxPC := MDDef.SonarMapDef.MaxPC + 2;
   DisplayImage;
end;

procedure Tsideimage.SpeedButton2Click(Sender: TObject);
begin
   Fullresolutiondisplay1Click(Sender);
end;


procedure Tsideimage.SpeedButton3Click(Sender: TObject);
begin
    AssignImageToClipBoard(Image1);
end;

procedure Tsideimage.SpeedButton4Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure Tsideimage.SpeedButton6Click(Sender: TObject);
begin
   DoingWhat := GraphicalSubset;
end;

procedure Tsideimage.SpeedButton7Click(Sender: TObject);
begin
   Entirerecord1Click(Sender);
end;

procedure Tsideimage.SpeedButton8Click(Sender: TObject);
begin
   SpeedButton8.Enabled := false;
   SpeedButton9.Enabled := true;
   FreqUp := LowSideScanFreq;
   DisplayImage;
end;

procedure Tsideimage.SpeedButton9Click(Sender: TObject);
begin
   SpeedButton9.Enabled := false;
   SpeedButton8.Enabled := true;
   FreqUp := HighSideScanFreq;
   DisplayImage;
end;


procedure Tsideimage.Makethisthefirstrecord1Click(Sender: TObject);
begin
   if ReverseImageDirection then LastPingDisplayed := RowPing[FirstY]
   else FirstPingDisplayed := RowPing[FirstY];
   DisplayImage;
end;

procedure Tsideimage.Makethisthelastrecorddisplayed1Click(Sender: TObject);
begin
   if ReverseImageDirection then FirstPingDisplayed := RowPing[FirstY]
   else LastPingDisplayed := RowPing[FirstY];
   DisplayImage;
end;

procedure Tsideimage.Mapinfo1Click(Sender: TObject);
var
   Info : tStringList;
begin
   Info := SideScanImageStats;
   Petmar.DisplayAndPurgeStringList(Info,'XTF:' + ExtractFileName(fName));
end;

procedure Tsideimage.MeasureDistanceSpeedButton14Click(Sender: TObject);
begin
   Distance1Click(Sender);
end;

procedure Tsideimage.Merge1Click(Sender: TObject);
begin
   MergeXTF;
end;

procedure Tsideimage.Entirerecord1Click(Sender: TObject);
begin
   FirstPingDisplayed := FirstSSPing;
   LastPingDisplayed := LastSSPing;
   DisplayImage;
end;


procedure Tsideimage.Fullresolutiondisplay1Click(Sender: TObject);
begin
   PingThinning := 1;
   PingsDisplayed := 1;
   PingsDisplayed := succ(LastPingDisplayed - FirstPingDisplayed);
   while (PingsDisplayed div PingThinning > MaxScreenYMax div 3) do inc(PingThinning);

   ImageYSize := PingsDisplayed * PingRepeats div PingThinning;
   AlongTrackPixelSize := SidescanRunlength / ImageYSize;
   AcrossTrackThinning := 25;
   repeat
      Dec(AcrossTrackThinning);
      ImageXSize := (2 * XTFPingChanHeader.NumSamples) div AcrossTrackThinning;
      AcrossTrackPixelSize := 2 * SidescanRange / ImageXSize;
   until (AcrossTrackPixelSize <= AlongTrackPixelSize) or (AcrossTrackThinning = 1);
   DisplayImage;
end;


procedure Tsideimage.Grap1Click(Sender: TObject);
begin
    DoingWhat := GraphicalFileSubset;
end;


function Tsideimage.GetPingReturns : boolean;
var
  xpic : integer;
begin
   if ReadXTFPing then begin
        if (XTFHeader.ChanInfo[1].BytesPerSample = 4) then begin
            for xpic := 0 to pred(2 * XTFPingChanHeader.NumSamples) do begin
               if FreqUp = LowSideScanFreq then
                  DataRecByte[xpic] := ValidByteRange(round( 255.0 * (DataRecInt32[xpic]-LowDisplayValue) / (HighDisplayValue - LowDisplayValue)))
               else
                  DataRecByte[xpic] := ValidByteRange(round( 255.0 * (DataRecInt32_2[xpic]-LowDisplayValue) / (HighDisplayValue - LowDisplayValue)))
            end;
        end;
        if (XTFHeader.ChanInfo[1].BytesPerSample = 2) then begin
            for xpic := 0 to pred(2 * XTFPingChanHeader.NumSamples) do begin
               DataRecByte[xpic] := ValidByteRange(round( 255.0 * (DataRecInt16[xpic]-LowDisplayValue) / (HighDisplayValue - LowDisplayValue)));
            end;
        end;
        Result := true;
  end
  else begin
     Result := false;
  end;
end;



procedure Tsideimage.DisplayImage;
{$IfDef ExSidescan}
begin
{$Else}
label
   Done;
var
   Contrast : float64;
   Repeater,LastLine,Range,Center,ImageRow,i,xpic : integer;
   RefVal : byte;
   Bitmap : tMyBitmap;
   p0 : prgb;
begin
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Enter sideimage.DisplayImage ' + ExtractFileName(DataFileName)); {$EndIf}
   Caption := 'Sidescan: ' + ExtractFileName(DataFileName);
   NoResizing := true;
   ShowHourglassCursor;
   RangeFromPercentiles;

      if (FirstPingDisplayed > LastPingDisplayed) then Petmath.SwapPair(FirstPingDisplayed,LastPingDisplayed);

      PingsDisplayed := succ(LastPingDisplayed - FirstPingDisplayed);
      while (PingsDisplayed div PingThinning > MaxScreenYMax div 2) do inc(PingThinning);

      ImageXSize := (2 * XTFPingChanHeader.NumSamples) div AcrossTrackThinning + 24;
      ImageYSize := (PingsDisplayed div PingThinning) * PingRepeats;

      {$IfDef LogSidescanFileInfo}
         WriteLineToDebugFile('Display pings  ' + IntToStr(FirstPingDisplayed) + ' to ' + IntToStr(LastPingDisplayed),true);
         WriteLineToDebugFile('Pings Displayed: ' + IntToStr(PingsDisplayed));
         WriteLineToDebugFile('Samples per ping: ' + IntToStr(2 * XTFPingChanHeader.NumSamples));
         WriteLineToDebugFile('Across track Thinning: ' + IntToStr(AcrossTrackThinning ));
         WriteLineToDebugFile('Ping Thinning: ' + IntToStr(PingThinning));
         WriteLineToDebugFile('Pings Repeats: ' + IntToStr(PingRepeats));
         WriteLineToDebugFile('BMP size: ' + IntToStr(ImageXSize) + 'x' + IntToStr(ImageYSize));
      {$EndIf}

      Image1.Width := ImageXSize;
      Image1.Height := ImageYSize;

      if (FreqUp = LowSideScanFreq) then Contrast := LowGain
      else Contrast := HighGain;

      {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Creating bitmap, ' + IntToStr(ImageXSize) + 'x' + IntToStr(ImageYSize)); {$EndIf}

      CreateBitmap(Bitmap,ImageXSize,ImageYSize);

      if ReverseImageDirection then ImageRow := 0
      else ImageRow := pred(ImageYSize);

      LoadMemStreamAndReadFileHeader;

      {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Start display'); {$EndIf}

      StartTime := '';
      while (MemStream.Position < (MemStream.Size - 256)) do begin
         for i := 1 to PingThinning do begin
            if GetPingReturns then begin
               if (StartTime = '') then StartTime := PingTime;
               EndTime := PingTime;
               {$IfDef LogSidescanPingInfo} WriteLineToDebugFile('Read ping ' + IntToStr(XTFPingHeader.PingNumber)); {$EndIf}
            end;
         end;

        if (XTFPingHeader.PingNumber >= FirstPingDisplayed) and (XTFPingHeader.PingNumber <= LastPingDisplayed)then begin
           {$IfDef LogSidescanPingInfo} WriteLineToDebugFile('Display ping ' + IntToStr(XTFPingHeader.PingNumber) + ' on image row=' + IntToStr(ImageRow)); {$EndIf}
           for Repeater := 1 to PingRepeats do begin
              p0 := Bitmap.ScanLine[ImageRow];
              VincentyPointAtDistanceBearing(XTFPingHeader.SensorYcoordinate,XTFPingHeader.SensorXcoordinate,MDDef.SonarMapDef.SidescanLayback,XTFPingHeader.SensorHeading,RowLats[ImageRow],RowLongs[ImageRow]);
              RowPing[ImageRow] := XTFPingHeader.PingNumber;
              xpic := 0;
              while (xpic div AcrossTrackThinning < ImageXSize) do begin
                 Refval := DataRecByte[xpic];
                 if ReverseGrayscale then RefVal := 255 - RefVal;
                 x := (xpic) div AcrossTrackThinning;
                 if ReverseImageDirection then x := ImageXSize - x;
                 if (x < ImageXSize+24) and (x < Bitmap.Width) then p0[x] := SideScanColorTable[RefVal];
                 inc(Xpic,AcrossTrackThinning);
              end;
              {$IfDef LogPingLocations}
              if (ImageRow mod 10 = 0) then begin
                 WriteLineToDebugFile('Ping=' + IntToStr(XTFPingHeader.PingNumber) + '  im_row=' + IntToStr(ImageRow) + '  ' + LatLongDegreeToString(RowLats[ImageRow],RowLongs[ImageRow],DecDegrees));
              end;
              {$EndIf}
              if ReverseImageDirection then inc(ImageRow) else dec(ImageRow);
           end;
           if (ImageRow < 0) or (ImageRow > pred(ImageYSize)) then begin
              {$IfDef LogPingLocations} WriteLineToDebugFile('ImageRow ' + intToStr(ImageRow) + '  off image'); {$EndIf}
              break;
           end;
        end
        else begin
           {$IfDef LogPingLocations} WriteLineToDebugFile('Ping ' + intToStr(XTFPingHeader.PingNumber) + '  outside range'); {$EndIf}
        end;
     end;
     Done:;

    {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Adjust RowLats'); {$EndIf}

    if ReverseImageDirection then begin
       while ImageRow < pred(ImageYSize) do begin
          RowLats[ImageRow] := RowLats[ImageRow - 1];
          RowLongs[ImageRow] := RowLongs[ImageRow - 1];
          inc(ImageRow);
          RowLats[ImageRow] := RowLats[ImageRow - 1];
          RowLongs[ImageRow] := RowLongs[ImageRow - 1];
       end;
    end
    else begin
       while ImageRow > 0 do begin
          RowLats[ImageRow] := RowLats[ImageRow + 1];
          RowLongs[ImageRow] := RowLongs[ImageRow + 1];
          dec(ImageRow);
          RowLats[ImageRow] := RowLats[ImageRow + 1];
          RowLongs[ImageRow] := RowLongs[ImageRow + 1];
       end;

    end;

    {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Data read'); {$EndIf}

      SideScanRange := SlantRange;
      AlongTrackPixelSize := SidescanRunlength/ImageYSize;

      VincentyPointAtDistanceBearing(RowLats[0],RowLongs[0],SlantRange,LineHeading-90,CornerLats[1],CornerLongs[1]);
      VincentyPointAtDistanceBearing(RowLats[0],RowLongs[0],SlantRange,LineHeading+90,CornerLats[0],CornerLongs[0]);
      VincentyPointAtDistanceBearing(RowLats[pred(Bitmap.Height)],RowLongs[pred(Bitmap.Height)],SlantRange,LineHeading-90,CornerLats[2],CornerLongs[2]);
      VincentyPointAtDistanceBearing(RowLats[pred(Bitmap.Height)],RowLongs[pred(Bitmap.Height)],SlantRange,LineHeading+90,CornerLats[3],CornerLongs[3]);

      {$IfDef LogSidescanFileInfo}
      WriteLineToDebugFile('');
      for i := 0 to 3 do
        WriteLineToDebugFile(CornerNames[i] + ' Corner=' + {IntToStr(i) +} '  ' + LatLongDegreeToString(CornerLats[i],CornerLongs[i],DecSeconds));
      {$EndIf}

      if MDDef.SonarMapDef.OverlayGrid then begin
         Bitmap.Canvas.Pen.Width := MDDef.SonarMapDef.SSGridWidth;
         Bitmap.Canvas.Pen.Color := ConvertPlatFormColorToTColor(MDDef.SonarMapDef.SSGridColor);
         Range := 0;
         Center := (XTFPingChanHeader.NumSamples) div AcrossTrackThinning;
         while Range <= SlantRange do begin
            xpic := Center - round(Range / SlantRange * Center);
            Bitmap.Canvas.MoveTo(Xpic,0);
            Bitmap.Canvas.LineTo(Xpic,pred(Bitmap.Height));
            xpic :=succ(Center) + round(Range / SlantRange * Center);
            Bitmap.Canvas.MoveTo(Xpic,0);
            Bitmap.Canvas.LineTo(Xpic,pred(Bitmap.Height));
            inc(Range,MDDef.SonarMapDef.GridSpaceAcross);
         end;

         i := 0;
         LastLine := round(MDDef.SonarMapDef.GridSpaceAlong / AlongTrackPixelSize);
         if (LastLine < 1) then LastLine := 1;

         while i <= pred(Bitmap.Height) do begin
            inc(i,LastLine);
            Bitmap.Canvas.MoveTo(0,i);
            Bitmap.Canvas.LineTo(pred(Bitmap.Width),i);
         end;
      end;

      PETImage.PlotOrientedLine(Bitmap,50,50,45,{360-}LineHeading,claRed,2,true);
      Bitmap.Canvas.Font.Color := clRed;
      BitmapTextOut(Bitmap,0,0,'N');
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;

      Showcoverageonmap1Click(Nil);

      ShowDefaultCursor;

      CloseMemStream;
   NoResizing := false;

{$EndIf}
   {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('Exit sideimage.DisplayImage'); {$EndIf}
end;


function Tsideimage.FindNextPingHeader(Found : tStringList; ZeroOnly : boolean) : boolean;
begin
   if (MemStream.Position < MemStream.Size - 256) then begin
      repeat
        MemStream.Read(XTFPingHeader,256);
        if (XTFPingHeader.MagicNumber = 64206) {and (I < NumRead + 128)} then begin
           if (Found <> Nil) then Found.Add(IntegerToString(MemStream.Position,12) + '   ' + IntegerToString(XTFPingHeader.HeaderType,5) + '   ' + IntToStr(XTFPingHeader.NumBytesThisRecord));
           if (ZeroOnly and (XTFPingHeader.HeaderType in [0])) or ((not ZeroOnly) and (XTFPingHeader.HeaderType in [0,200])) then begin
              with XTFPingHeader do
                 PingTime := IntToStr(Month) + '/' + IntToStr(Day) + '/' + IntToStr(Year) + '  ' + IntToStr(Hour) + ':' + IntToStr(Minute) + ':' + IntToStr(Second) + '.' + IntToStr(HSeconds);
              Result := true;
              exit;
           end
           else begin
              MemStream.Position := MemStream.Position - 255;
           end;
        end
        else MemStream.Position := MemStream.Position - 255;
      until (MemStream.Position > MemStream.Size - 256);
   end;
   Result := false;
end;


function LogXTFHeader(XTFHeader : tXTFFileHeader; InName : PathStr) : tStringList;
var
   i : integer;
begin
   Result := tStringList.Create;
    with XTFHeader do begin
       Result.Add('XTF header ' + InName);
       Result.Add('Recording program: ' + ArrayOfCharToString(XTFHeader.RecordingProgramName,8));
       Result.Add('Version: ' + ArrayOfCharToString(RecordingProgramVersion,8));
       Result.Add('Sonar name: ' + ArrayOfCharToString(XTFHeader.SonarName,16));
       Result.Add('Sonar type: ' + IntToStr(XTFHeader.SonarType));
       Result.Add('Nav units: ' + IntToStr(XTFHeader.NavUnits));
       Result.Add('Notes: ' + ArrayOfCharToString(NoteString,64));
       Result.Add('FileFormat/System type: ' + IntToStr(XTFHeader.FileFormat) + '/' + IntToStr(XTFHeader.SystemType));

       for I := 1 to XTFHeader.NumberOfSonarChannels do with ChanInfo[i] do begin
          Result.Add('');
          Result.Add('Channel ' + IntToStr(i));
          Result.Add('Type of channel: ' + IntToStr(TypeOfChannel));
          Result.Add('Subchannel: ' + IntToStr(SubchannelNumber));
          Result.Add('Channel name: ' + ArrayOfCharToString(ChannelName,16));
          Result.Add('Bytes per sample: ' + IntToStr(BytesPerSample));
          Result.Add('Reserved (once samples/channel): ' + IntToStr(Reserved));
          if abs(Frequency) > 0.0001 then Result.Add('Frequency ' + RealToString(Frequency,-12,-2));
          if abs(TiltAngle) > 0.0001 then Result.Add('TiltAngle ' + RealToString(TiltAngle,-12,-2));
          if abs(BeamWidth) > 0.0001 then  Result.Add('BeamWidth ' + RealToString(BeamWidth,-12,-2));
          if abs(HorizBeamAngle) > 0.0001 then Result.Add('HorizBeamAngle ' + RealToString(HorizBeamAngle,-12,-2));
       end;
    end;
end;


procedure XTFFileInfoForFile(FName : PathStr);
var
   DataFile : file;
   XTFheader : tXTFfileHeader;
   Info : tStringList;
begin
    AssignFile(DataFile,fName);
    reset(DataFile,1);
    BlockRead(DataFile,XTFheader,Sizeof(XTFheader));
    if (XTFHeader.FileFormat <> 123) and (XTFHeader.SystemType <> 1) then begin
       WriteLineToDebugFile(ExtractFileNameNoExt(fName) + '  format=' + IntToStr(XTFHeader.FileFormat));
    end
    else begin
       Info :=  LogXTFHeader(XTFHeader,fName);
       Petmar.DisplayAndPurgeStringList(Info,'XTF:' + ExtractFileName(fName));
    end;
    CloseFile(DataFile);
end;


procedure XTFFileInfo;
var
   fName : PathStr;
begin
   fName := SideDataPath;
   if GetFileMultipleMask('side scan records','Any sidescan image|*.XTF',fName,MDDef.SonarMapDef.SideDefFilter) then begin
      XTFFileInfoForFile(FName);
   end;
end;


procedure Tsideimage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   FreeAndNil(dbFile);
end;


procedure SetSideScanColorTable;
var
   i : integer;
   Table : tMyData;
begin
   for i := 0 to 255 do SideScanColorTable[i] := RGBtrip(i,i,i);
   if MDDef.SonarMapDef.CustomPalette then begin
      Table := tMyData.Create(ColorBrewerName);
      Table.ApplyFilter('PALETTE=' + QuotedStr('Sidescan gold 256 step'));
      if (Table.RecordCount = 256) then begin
         for i := 0 to 255 do begin
            SideScanColorTable[i] := RGBtrip(Table.GetFieldByNameAsInteger('RED'),Table.GetFieldByNameAsInteger('GREEN'),Table.GetFieldByNameAsInteger('BLUE'));
            Table.Next;
         end;
      end;
      Table.Destroy;
   end;
end;

function TimeStrToDecHours(TStr : ANSIstring) : float64;
var
   Hours,Min,Sec : float64;
begin
   Hours := StrToFloat(Petmar_Types.BeforeSpecifiedCharacterANSI(Tstr,':',true,true));
   Min := StrToFloat(Petmar_Types.BeforeSpecifiedCharacterANSI(Tstr,':',true,true));
   Sec := StrToFloat(TStr);
   Result := Hours + Min/60 + Sec/3600;
end;


function Tsideimage.SideScanImageStats : tStringList;
var
   TStr : ShortString;
   StartHours,EndHours,Duration : float64;
begin
   Result := tStringList.Create;
   Result.Add('Image: ' + ExtractFileName(DataFileName) + '  ' + SmartMemorySizeBytes(GetFileSize(DataFileName)));
   Result.Add('Image start: ' + LatLongDegreeToString(RowLats[0],RowLongs[0],MDDef.OutPutLatLongMethod));
   Result.Add('Image ends: ' + LatLongDegreeToString(RowLats[pred(ImageYSize)],RowLongs[pred(ImageYSize)],MDDef.OutPutLatLongMethod));
   Result.Add('Image size: ' + IntToStr(ImageXSize) + 'x' +  IntToStr(ImageYSize) + ' pixels');
   Result.Add('');
   if MultipleRangeInFile then begin
      Result.Add('Multiple ranges in the file');
      Result.Add('');
   end;

   Result.Add('Line start: ' + StartTime);
   TStr := trim(Petmar_types.AfterSpecifiedCharacter(StartTime,' '));

   StartHours := TimeStrToDecHours(TStr);
   Result.Add('Line end: ' + EndTime);
   TStr := trim(Petmar_types.AfterSpecifiedCharacter(EndTime,' '));

   EndHours := TimeStrToDecHours(TStr);
   Duration := EndHours-StartHours;
   Result.Add('Line duration: ' + RealToString(Duration,-18,3) + ' hours');

//Line start: 6/7/2012  13:12:50.42
//Line end: 6/7/2012  13:15:2.32

   Result.Add('Sidescan range: ' + IntToStr(SidescanRange));
   Result.Add('Track length (GPS, connect end point): ' + RealToString(StraightLineDist,-18,-2) + ' m');
   Result.Add('Track length (GPS, along route): ' + RealToString(SidescanRunlength,-18,-2) + ' m');
   if ReverseImageDirection then TStr := ' (reversed)' else TStr := '';
   Result.Add('Ship speed (GPS): ' + RealToString(0.001 * SidescanRunlength / Duration, -12,2)  + ' km/hr');
   Result.Add('Track heading: ' + RealToString(LineHeading,-18,-1) + DegSym + Tstr);
   Result.Add('Pings in record: ' + IntToStr(succ(LastPingDisplayed-FirstPingDisplayed)));
   Result.Add('');
   Result.Add('Samples per channel: ' + IntToStr(XTFPingChanHeader.NumSamples));
   Result.Add('Across track thinning: ' + IntToStr(AcrossTrackThinning));
   Result.Add('Across track pixel size: ' + RealToString(2 * SidescanRange / Image1.Width,-18,-2) + ' m');
   Result.Add('');
   Result.Add('Along track thinning: ' + IntToStr(PingThinning));
   Result.Add('Along track pixel size: ' + RealToString(SidescanRunlength/Image1.Height,-18,-2) + ' m');
   Result.Add('');
   Result.Add('Upper left= ' + LatLongDegreeToString(CornerLats[0],CornerLongs[0],MDDef.OutPutLatLongMethod));    //0=NW
   Result.Add('Upper right= ' + LatLongDegreeToString(CornerLats[1],CornerLongs[1],MDDef.OutPutLatLongMethod));   //1=NE
   Result.Add('Lower left= ' + LatLongDegreeToString(CornerLats[2],CornerLongs[2],MDDef.OutPutLatLongMethod));    //2=SW
   Result.Add('Lower right= ' + LatLongDegreeToString(CornerLats[3],CornerLongs[3],MDDef.OutPutLatLongMethod));   //3=SE
end;

procedure Tsideimage.BitBtn1Click(Sender: TObject);
var
   Results : tStringList;
begin
   Results := SideScanImageStats;
   Petmar.DisplayAndPurgeStringList(Results,'Sonar image statistics');
end;

procedure Tsideimage.CheckSideScanOptions;
var
   SideScanOptions : ss_options.tSS_opts_form;
begin
   SideScanOptions := TSS_opts_form.Create(Application);
   SideScanOptions.SSImage := self;
   SideScanOptions.Show;
   if (SideScanOptions <> Nil) then with SideScanOptions do begin
      case FreqUp of
         LowSideScanFreq  : RadioGroup1.ItemIndex := 0;
         HighSideScanFreq : RadioGroup1.ItemIndex := 1;
         MergeFreq        : RadioGroup1.ItemIndex := 2;
      end;
      Edit1.Text := RealToString(LowGain,-8,-2);
      Edit2.Text := RealToString(HighGain,-8,-2);
      Edit3.Text := IntToStr(MDDef.SonarMapDef.GridSpaceAcross);
      Edit4.Text := IntToStr(FirstPingDisplayed);
      Edit5.Text := IntToStr(LastPingDisplayed);
      Edit6.Text := IntToStr(MDDef.SonarMapDef.GridSpaceAlong);
      Edit7.Text := RealToString(FishHeight,-8,-2);
      Edit8.Text := IntToStr(AcrossTrackThinning);
      Edit9.Text := IntToStr(PingThinning);
      Edit10.Text := IntToStr(LowDisplayValue);
      Edit11.Text := IntToStr(HighDisplayValue);
      Edit12.Text := IntToStr(MDDef.SonarMapDef.SidescanLayback);
      Edit14.Text := IntToStr(PingRepeats);
      Edit13.Text := RealToString(MDDef.SonarMapDef.MinPC,-12,-2);
      Edit15.Text := RealToString(MDDef.SonarMapDef.MaxPC,-12,-2);

      CheckBox1.Checked := ReverseImageDirection;
      CheckBox2.Checked := ReverseGrayscale;
      CheckBox3.Checked := MDDef.SonarMapDef.CustomPalette;
      CheckBox4.Checked := MDDef.SonarMapDef.OverlayGrid;

      RadioGroup1.Enabled := DualFreq;
      UpdateStats;
      Self.Caption := bName + ' records ' + IntegerToString(FirstPingDisplayed) + ' to ' + IntegerToString(LastPingDisplayed);
   end;
end;


procedure InitializeSideImg;
begin
   ColsSidebySide := 1;
   BotVal := 10;
   MinSlantRange := 10;
   CopyImage := false;
   DisplayUpsideDown  := false;
   NormalColor := true;
   Starboard := false;
   ColSkip := 1;
   DoingWhat := JustWandering;
   LowGain := 12;
   HighGain := 0.5;
   FishHeight := 3;
   SetSideScanColorTable;
   SideIndexDB := 0;
end;


procedure Tsideimage.FormCreate(Sender: TObject);
var
   NumRec : integer;
begin
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Tsideimage.FormCreate in'); {$EndIf}
   InitializeSideImg;
   FreqUp := LowSideScanFreq;
   NoResizing := false;

   TargetGIS := 0;
   MapOwner := nil;
   Petmar.CheckFormPlacement(Self);
   Image1.Stretch := true;             //Required for Delphi 6 "feature"
   FileMode := 2;
   AnOverViewImage := false;
   MouseIsDown := false;
   MemStream := nil;
   LowDisplayValue := 0;
   HighDisplayValue := 255;
   PingRepeats := 1;

   AnOverViewImage := true;
   Top := 0;
   FileMode := 2;
   Max := 0;
   FillChar(Freq,sizeof(Freq),0);
   NormalColor := true;
   ImageXSize := MDDef.SonarMapDef.DefaultPixelsWide;
   ImageYSize := MDDef.SonarMapDef.DefaultPixelsHigh;

   ClientWidth := MDDef.SonarMapDef.DefaultPixelsWide;
   ClientHeight := MDDef.SonarMapDef.DefaultPixelsHigh;

   ReverseImageDirection := false;
   ReverseGrayscale := false;

   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('Tsideimage.FormCreate out'); {$EndIf}

end;


procedure Tsideimage.Distance1Click(Sender: TObject);
begin
   Distance1.Checked := not Distance1.Checked;
   if Distance1.Checked then begin
      DoingWhat := FirstDistancePoint;
      WmDEM.StatusBar1.Panels[0].Text := 'First point for distance';
      TwoWayTravelTime1.Checked := false;
   end;
end;


procedure Tsideimage.Image1DblClick(Sender: TObject);
var
   Dist : float64;
begin
   if DoingWhat = FirstDistancePoint then begin
      DoingWhat := SecondDistancePoint;
      WmDEM.StatusBar1.Panels[0].Text := 'Second point for distance';
      FirstX := LastX;
      FirstY := LastY;
      exit;
   end;
   if DoingWhat = SecondDistancePoint then begin
      Dist := sqrt( sqr(AcrossTrackPixelSize*(FirstX - LastX)) + sqr(AlongTrackPixelSize*(FirstY - LastY)));
      MessageToContinue('Distance: ' + RealToString(Dist,8,1) + ' m');
      DoingWhat := JustWandering;
      WmDEM.StatusBar1.Panels[0].Text := '';
   end;

(*
   if DoingWhat = FirstTWTTPoint then begin
      DoingWhat := SecondTWTTPoint;
      WmDEM.StatusBar1.Panels[0].Text := 'Second point TWTT';
      FirstX := LastX;
      FirstY := LastY;
      exit;
   end;
   if DoingWhat in [SecondTWTTPoint,SecondDistancePoint] then begin
      with Image1.Canvas do begin
         Pen.Mode := pmNotXor;
         Pen.Width := 3;
         Pen.Color := clLime;
         MoveTo(FirstX,FirstY);
         LineTo(Lastx,Lasty);
      end;
      if DoingWhat = SecondDistancePoint then begin
         Dist := sqrt( sqr(1.0*FirstX - LastX) + sqr(1.0*FirstY - LastY)) * PixelDimension;
         MessageToContinue(true,'Distance: ' + RealToString(Dist,8,1) + ' m');
         DoingWhat := FirstDistancePoint;
         WmDEM.StatusBar1.Panels[0].Text := 'First point for distance';
      end
      else begin
         Dist := sqrt( sqr(1.0*FirstX - LastX) + sqr(1.0*FirstY - LastY)) * PixelDimension / 3000;
         MessageToContinue(true,'Two way travel time: ' + RealToString(Dist,8,4) + ' sec');
         DoingWhat := FirstTWTTPoint;
         WmDEM.StatusBar1.Panels[0].Text := 'First point TWTT';
      end;
      with Image1.Canvas do begin
         Pen.Mode := pmNotXor;
         Pen.Width := 3;
         Pen.Color := clLime;
         MoveTo(FirstX,FirstY);
         LineTo(Lastx,Lasty);
      end;
   end;
*)
end;


procedure Tsideimage.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   j,newx2,newy2 : integer;
begin
   if MouseIsDown and not (DoingWhat in [GraphicalFileSubset, GraphicalSubset]) then begin
      ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + SX - X;
      ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + SY - Y;
      exit;
   end;

   LastRoamLat := RowLats[y];
   LastRoamLong := RowLongs[y];

   WmDEM.StatusBar1.Panels[1].Text := LatLongDegreeToString(LastRoamLat,LastRoamLong,MDDef.OutPutLatLongMethod) + '  Ping=' + IntToStr(RowPing[y]);

   if (DoingWhat in [GraphicalFileSubset, GraphicalSubset]) then begin
      Newx2 := X;
      NewY2 := Y;
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Pen.Width := 4;
      Image1.Canvas.Rectangle(FirstX,FirstY,LastX,LastY);
      Image1.Canvas.Rectangle(FirstX,FirstY,newx2,newy2);
      Lastx := x;
      lasty := y;
      exit;
   end;

   LastX := x;
   LastY := y;
   if (DoingWhat = SecondDistancePoint) then begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clBlack;
      Image1.Canvas.Pen.Width := 3;
      Image1.Canvas.MoveTo(FirstX,FirstY);
      Image1.Canvas.LineTo(x,y);
   end;

   if (WmDEM.MDIChildCount > 0) then
     for j := WmDEM.MDIChildCount-1 downto 0 do
        if WmDEM.MDIChildren[j].Handle <> Self.Handle then
           PostMessage(WmDEM.MDIChildren[j].Handle,WM_BroadcastLatLongMessage,0,0);
   ApplicationProcessMessages;
end;


procedure Tsideimage.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MouseIsDown := false;
   if (DoingWhat in [GraphicalFileSubset, GraphicalSubset]) then begin
      Petmath.MinOfPairFirst(FirstY,Y);
      FirstPingDisplayed := RowPing[FirstY];
      LastPingDisplayed := RowPing[Y];
      if (DoingWhat in [GraphicalSubset]) then DisplayImage;
      if (DoingWhat in [GraphicalFileSubset]) then Subset(FirstPingDisplayed,LastPingDisplayed);
      DoingWhat := JustWandering;
   end;
end;


procedure Tsideimage.Inserttargetrecord1Click(Sender: TObject);
var
   fName : PathStr;
begin
   if (TargetGIS=0) then begin
      fName := ExtractFilePath(DataFileName) + 'targets' + DefaultDBExt;
      if not FileExists(fName) then MakeSideScanTargetFile(fName);
      TargetGIS := MapOwner.LoadDataBaseFile(fName);
      //TargetGIS := LastDBLoaded;
   end;

   with GISdb[TargetGIS] do begin
      MyData.Last;
      MyData.Insert;
      MyData.SetFieldByNameAsFloat(LatFieldName,RowLats[FirstY]);
      MyData.SetFieldByNameAsFloat(LongFieldName,RowLongs[FirstY]);
      MyData.SetFieldByNameAsInteger('PING',RowPing[FirstY]);
      MyData.SetFieldByNameAsString('FILENAME',DataFileName);
      MyData.Post;
      ApplicationProcessMessages;
      DisplayTheRecord(MyData.RecordCount,true,true);
   end;
end;

procedure Tsideimage.Labelcorners1Click(Sender: TObject);
var
   TStr : shortstring;
begin
   Image1.Canvas.Font.Color := clRed;
   Image1.Canvas.Font.Size := 14;
   Image1.Canvas.Font.Style := [fsBold];

   Image1.Canvas.TextOut(5,Image1.Height-24,LatLongDegreeToString(CornerLats[1],CornerLongs[1],DecSeconds));  //lower left
   Image1.Canvas.TextOut(5,4,LatLongDegreeToString(CornerLats[2],CornerLongs[2],DecSeconds));                 //upper left

   TStr := LatLongDegreeToString(CornerLats[3],CornerLongs[3],DecSeconds);
   Image1.Canvas.TextOut(ImageXSize-5-Image1.Canvas.TextWidth(TStr),4,TStr);                                  //upper right

   TStr := LatLongDegreeToString(CornerLats[0],CornerLongs[0],DecSeconds);
   Image1.Canvas.TextOut(ImageXSize-5-Image1.Canvas.TextWidth(TStr),Image1.Height-24,TStr);                   //lower right
end;


procedure Tsideimage.LoadMemStreamAndReadFileHeader;
begin
   {$IfDef LogMemStream} WriteLineToDebugFile('Tsideimage.LoadMemStream in'); {$EndIf}

   if (MemStream = Nil) then begin
       {$IfDef LogMemStream} WriteLineToDebugFile('Tsideimage.LoadMemStream must create'); {$EndIf}
       MemStream := tMemoryStream.Create;
       MemStream.LoadFromFile(DataFileName);
   end;
   MemStream.Position := 0;
   ShowHourglassCursor;
   MemStream.Read(XTFheader,Sizeof(XTFheader));
   MemStream.Position := Sizeof(XTFheader);

   {$IfDef LogMemStream} WriteLineToDebugFile('Tsideimage.LoadMemStream out'); {$EndIf}
end;

procedure Tsideimage.Close1Click(Sender: TObject);
begin
   Close;
end;

procedure Tsideimage.CloseMemStream;
begin
   MemStream.Free;
   MemStream := Nil;
end;

procedure Tsideimage.ComboBox1Change(Sender: TObject);
begin
   AcrossTrackThinning := succ(ComboBox1.ItemIndex);
   PingThinning := succ(ComboBox1.ItemIndex);
   DisplayImage;
end;


procedure Tsideimage.Copytoclipboard1Click(Sender: TObject);
begin
   Clipboard.Assign(Image1.Picture);
end;


procedure Tsideimage.Subset(StartRecord,EndRecord : integer; xshift : float64 = 0; yshift : float64 = 0);
var
   FileName : PathStr;
   DataFile,
   OutFile  : file;
begin
   if (StartRecord < -99) and (EndRecord < -99) then begin
       StartRecord := 0;
       ReadDefault('Start record',StartRecord);
       EndRecord := StartRecord + 100;
       ReadDefault('End record',EndRecord);
   end;

   FileName := Petmar.NextFileNumber(ExtractFilePath(DataFileName), ExtractFileNameNoExt(DataFileName) + '_sub_', '.xtf');
   GetFileNameDefaultExt('new record','XTF record|*.XTF',FileName);

   assignFile(Outfile,Filename);
   rewrite(Outfile,1);
   assignFile(DataFile,DataFileName);
   reset(DataFile,1);
   BlockRead(DataFile,XTFheader,Sizeof(XTFheader),i);
   BlockWrite(Outfile,XTFheader,Sizeof(XTFheader));
   ShowHourglassCursor;
   while not eof(DataFile) do begin
      BlockRead(DataFile,XTFPingHeader,Sizeof(XTFPingHeader));
      BlockRead(DataFile,DataRecInt16,XTFPingHeader.NumBytesThisRecord - Sizeof(XTFPingHeader));
      if (XTFPingHeader.PingNumber >= StartRecord) then begin

         XTFPingHeader.SensorYcoordinate := XTFPingHeader.SensorYcoordinate + yshift;
         XTFPingHeader.SensorXcoordinate := XTFPingHeader.SensorXcoordinate + yshift;

         BlockWrite(OutFile,XTFPingHeader,Sizeof(XTFPingHeader));
         BlockWrite(OutFile,DataRecInt16,XTFPingHeader.NumBytesThisRecord - Sizeof(XTFPingHeader));
      end;
      if (XTFPingHeader.PingNumber >= EndRecord) then break;
   end;
   CloseFile(Outfile);
   CloseFile(DataFile);
   ShowDefaultCursor;
end;



procedure Tsideimage.Twowaytraveltime1Click(Sender: TObject);
begin
   Twowaytraveltime1.Checked := not Twowaytraveltime1.Checked;
   if Twowaytraveltime1.Checked then begin
      DoingWhat := FirstTWTTPoint;
      WmDEM.StatusBar1.Panels[0].Text := 'First point two way travel time';
      Distance1.Checked := false;
   end;
end;


procedure Tsideimage.XTFfileheader1Click(Sender: TObject);
begin
   XTFFileInfoForFile(Self.DataFileName);
end;


procedure Tsideimage.ExportcurrentsubsetXTF1Click(Sender: TObject);
begin
   Subset(FirstPingDisplayed,LastPingDisplayed);
end;


procedure Tsideimage.ExportKML1Click(Sender: TObject);
var
   Result : tStringList;
   fName,fname2 : PathStr;
   i : integer;
   North,East,West,South : float64;
begin
   Result := tStringList.Create;
   Result.Add('LAT,LONG');
   for i := 0 to 3 do Result.Add(RealToString(CornerLats[i],-12,-7) + ',' + RealToString(CornerLongs[i],-12,-7));
   fName := MDTempDir + 'ss_corners.dbf';
   i := PetDBUtils.StringList2CSVtoDB(Result,fName);
   if ValidDB(i) then begin
      ConvertToKML(i,'');
      CloseAndNilNumberedDB(i);
      Savelabelledimage1Click(Nil);

      South := PetMath.MinFloat(CornerLats[0],CornerLats[1],CornerLats[2],CornerLats[3]);
      North := PetMath.MaxFloat(CornerLats[0],CornerLats[1],CornerLats[2],CornerLats[3]);

      West := PetMath.MinFloat(CornerLongs[0],CornerLongs[1],CornerLongs[2],CornerLongs[3]);
      East := PetMath.MaxFloat(CornerLongs[0],CornerLongs[1],CornerLongs[2],CornerLongs[3]);

      fName := LastKMLDir + 'ss.png';
      PetImage.SaveImageAsBMP(Image1,fName);
      FName2 := 'Side_scan_image_' + ExtractFileNameNoExt(DataFileName) + '.kml';
      CreateKMLImageOverlay(LastKMLDir,fName,fname2,North,East,West,South);

      if OpenNumberedGISDataBase(i,DBFileName) then begin
         MDDef.DB2KMKLThin := 50;
         ConvertToKML(i,'');
         CloseAndNilNumberedDB(i);
         MDDef.DB2KMKLThin := 1;
      end;
   end;
end;


procedure Tsideimage.Exportsanitizedcoordinates1Click(Sender: TObject);
var
   xshift,yshift : float64;
begin
   xshift := -0.215;
   yshift := +0.0635;
   ReadDefault('xshift (degrees)',xshift);
   ReadDefault('yshift (degrees)',yshift);
   Subset(FirstPingDisplayed,LastPingDisplayed,xshift,yshift);
end;

procedure Tsideimage.Saveimage1Click(Sender: TObject);
begin
   SaveImageAsBMP(Image1);
end;


procedure Tsideimage.Saveimage2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure Tsideimage.Savelabelledimage1Click(Sender: TObject);
begin
   Image1.Canvas.Font.Color := clRed;
   Image1.Canvas.Font.Size := 14;
   Image1.Canvas.Font.Style := [fsBold];
   Image1.Canvas.TextOut(5,ImageYSize-24,ExtractFileNameNoExt(DataFileName) + '  ' + IntToStr(SidescanRange) + ' m');
   PetImage.SaveImageAsBMP(Image1,ChangeFileExt(DataFileName,'.png'));
end;


procedure Tsideimage.FormResize(Sender: TObject);
begin
   MDDef.SonarMapDef.DefaultPixelsWide:= ImageXSize;
   MDDef.SonarMapDef.DefaultPixelsHigh := ImageYSize;

   if NoResizing then exit;

   if (ClientWidth > Image1.Width + 5) then ClientWidth := Image1.Width + 5;
   if (ClientHeight > Image1.Height + Toolbar1.Height + 12) then ClientHeight:= Image1.Height + Toolbar1.Height + 12;

   ScrollBox1.HorzScrollBar.Visible := true;  //ImageXSize  > ClientWidth;
   ScrollBox1.VertScrollBar.Visible := true;  //ImageYSize > ClientHeight;
   if (Width > wmdem.ClientWidth - 50) then begin
      Width := wmdem.ClientWidth - 50;
      Left := 5;
   end;
end;


procedure Tsideimage.Subset1Click(Sender: TObject);
begin
   Subset(-999,-999);
end;


procedure Tsideimage.Image1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
begin
   if (DoingWhat in [GraphicalFileSubset, GraphicalSubset]) then begin
      FirstX := x;
      FirstY := y;
      MouseIsDown := true;
      exit;
   end;
   if (Button = mbLeft) then begin
      SX := X;  // X start co-ordinate, image panning
      SY := Y;  // Y start co-ordinate, image panning
      MouseIsDown := true;
      Forms.Screen.Cursor := crHandPoint;
      exit;
   end;
   if (Button = mbRight) then begin
      FirstX := x;
      FirstY := y;
      EntireRecord1.Visible := (FirstPingDisplayed <> FirstSSPing) or (LastPingDisplayed <> LastSSPing);
      FullResolutionDisplay1.Visible := (PingThinning <> 1) or (AcrossTrackThinning <> 1);
      PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure Tsideimage.Displayoptions1Click(Sender: TObject);
begin
   CheckSideScanOptions;
   DisplayImage;
end;

procedure Tsideimage.Displayoptions2Click(Sender: TObject);
begin
   CheckSideScanOptions;
end;


procedure Tsideimage.Histogram1Click(Sender: TObject);
var
   db : integer;
begin
   Histogram;
   db := MapOwner.OpenDBonMap('',FrqFileName,  false,false);
   GISdb[db].CreateScatterGram('DN','NPTS',true);
   CloseAndNilNumberedDB(db);
end;


procedure InitializeSS;
var
   i : integer;
begin
   for i := 1 to MaxSideImage do SideImage[i] := Nil;
   NumSideImages := 0;
end;



initialization
   InitializeSS;
finalization
   {$IfDef LogSidescanFileInfo} WriteLineToDebugFile('LogSidescanFileInfo active in SideIMG'); {$EndIf}
   {$IfDef LogScaledImageDisplay} WriteLineToDebugFile('LogScaledImageDisplay active in SideIMG'); {$EndIf}
   {$IfDef LogSidescanPingInfo}   WriteLineToDebugFile('LogSidescanPingInfo active in SideIMG'); {$EndIf}
   {$IfDef LogAllStructures} WriteLineToDebugFile('LogAllStructures active in SideIMG'); {$EndIf}
   {$IfDef LogPingLocations} WriteLineToDebugFile('LogPingLocations active in SideIMG'); {$EndIf}
   {$IfDef LogSidescanOpenAndDisplay} WriteLineToDebugFile('LogSidescanOpenAndDisplay active in SideIMG'); {$EndIf}

   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing sideimg out'); {$EndIf}
end.





