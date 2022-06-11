unit PetMovie;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

   {$Define ExGIF}
   {$Define ExAVI}

{$IfDef RecordProblems}
   //{$Define RecordMovieProblems}
   //{$Define RecordMovieFrameProblems}   //slowdown
{$EndIf}



interface

uses
  Windows,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus,
  PETMAR,Petmar_types,
  ComCtrls, Vcl.MPlayer;

type
   tDisplayMode = (MovieForward,MovieBackWard,ForwardLoop,SlideOnly);

   TMovieForm = class(TForm)
    Panel1: TPanel;
    Image2: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    Close1: TMenuItem;
    Timer1: TTimer;
    Timedelay1: TMenuItem;
    Advance1: TMenuItem;
    Backward1: TMenuItem;
    Movie1: TMenuItem;
    Forward1: TMenuItem;
    Backward2: TMenuItem;
    Loop1: TMenuItem;
    Panel2: TPanel;
    Panel4: TPanel;
    Image3: TImage;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    N1: TMenuItem;
    N2: TMenuItem;
    ImageGif: TImage;
    Sidebysidemerge1: TMenuItem;
    N3: TMenuItem;
    Deletemovie1: TMenuItem;
    Addmovietitle1: TMenuItem;
    Animate1: TAnimate;
    Panel3: TPanel;
    //MediaPlayer1: TMediaPlayer;
    BitBtn5: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn6: TBitBtn;
    ScrollBar1: TScrollBar;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    Copytoclipboard1: TMenuItem;
    TrackBar1: TTrackBar;
    Addlogotoframes1: TMenuItem;
    RadioGroup1: TRadioGroup;
    procedure TrackBar1Change(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Close1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timedelay1Click(Sender: TObject);
    procedure Advance1Click(Sender: TObject);
    procedure Backward1Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure Forward1Click(Sender: TObject);
    procedure Backward2Click(Sender: TObject);
    procedure Loop1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Sidebysidemerge1Click(Sender: TObject);
    procedure Deletemovie1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Addmovietitle1Click(Sender: TObject);
    procedure Createmoviepanningstillphoto1Click(Sender: TObject);
    procedure Addlogotoframes1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
      procedure LoadMovieList(Infile : PathStr; Reduction : Integer = 1);
      procedure OpenFile(Infile : PathStr);
  public
    { Public declarations }
     MovieList,
     aList,bList,
     MovieCaptions : TStringList;
     ReductionFactor,LastTime,
     TargetX,TargetY,NumMove,
     CurrentImage  : integer;
     DisplayMode   : tDisplayMode;
     TimeStamp     : tTimeStamp;
     MovieBaseName,
     MovieDirectory : PathStr;
     RepeatView,
     TwoViews   : boolean;
     MovieUp    : AnsiChar;
     procedure MoveForward;
     procedure MoveBackward;
     procedure DisplayImage(FName : PathStr; FirstFrame : boolean = false);
     procedure EnableOptions;
     procedure StartMovie;
     procedure AddTitleToMovie;
  end;


procedure MakeMovieFromStills(mlist2 : tstringList = nil);

var
  MovieScenesApart : integer;


implementation

{$R *.DFM}

uses
{Main program MDI window for different programs that use this module}
   Nevadia_Main,
{End of the MDI parent declaration}

   {$IfDef MICRODEM}
   DEMDefs,
   {$EndIf}

   PetImage;

var
   MovieForm: TMovieForm;




procedure MakeMovieFromStills(mlist2 : tstringList = nil);

         procedure MovieFromFile(Fname : PathStr; Reduction : Integer = 1; OfferShowTarget : boolean = false);
         begin
            {$IfDef RecordMovieProblems} WriteLineToDebugFile('MovieFromFile: ' + fName); {$EndIf}
            MovieForm := TMovieForm.Create(Application);
            if FileExists(fName) then MovieForm.OpenFile(fName)
            else MovieForm.Load1Click(Nil);
            {$IfDef RecordMovieProblems} WriteLineToDebugFile('opened or loaded'); {$EndIf}
            MovieForm.ShowModal;
         end;


var
  mlist : tstringList;
  theDir : PathStr;
  i : integer;
begin
   //StopSplashing;
   if Mlist2 = Nil then begin
      mlist := nil;
      theDir := MainMapData;
      Petmar.GetDosPath('Images',theDir);
      ShowHourglassCursor;
      Petmar.FindMatchingFiles(TheDir,'*.*',mlist,1);
      mlist2 := tstringlist.create;
      for i := 0 to pred(Mlist.count) do begin
         if ValidImageFileName(Mlist.strings[i]) then begin
            mlist2.add(ExtractFileName(Mlist.strings[i]));
         end;
      end;
      Mlist.free;
   end
   else begin
      TheDir := MDtempdir;
   end;

   TheDir := TheDir + 'filelist.mov';
   mlist2.SaveToFile(TheDir);
   mlist2.Free;
   MovieFromFile(theDir);
end;


procedure tMovieForm.LoadMovieList(Infile : PathStr; Reduction : Integer = 1);
var
   i,MissingReports : integer;
begin
   {$IfDef RecordMovieProblems} WriteLineToDebugFile('tMovieForm.LoadMovieList in'); {$EndIf}
   if not FileExists(InFile) then exit;
   ReductionFactor := Reduction;
   MovieList := tStringList.Create;
   MovieList.LoadFromFile(Infile);
   {$IfDef RecordMovieProblems}  WriteLineToDebugFile('file read w/ frames=' + IntToStr(MovieList.Count)); {$EndIf}
   MovieBaseName := InFile;
   MissingReports := 0;
   if (MovieList.Count > 0) then begin
      for i := pred(MovieList.Count) downto 0 do begin
         {$IfDef RecordMovieProblems} WriteLineToDebugFile('  ' + MovieList[i]); {$EndIf}
         if not FileExists(MovieList[i]) then MovieList[i] := ExtractFilePath(MovieBaseName) + MovieList[i];

         if not FileExists(MovieList[i]) then begin
            {$IfDef RecordMovieProblems} WriteLineToDebugFile('----->   frame missing'); {$EndIf}
            inc(MissingReports);
            if (MissingReports < 5) then MessageToContinue(MovieList[i] + ' missing');
            MovieList.Delete(i);
         end;
      end;
      {$IfDef RecordMovieProblems} WriteLineToDebugFile('file exist check done  w/ frames=' + IntToStr(MovieList.Count)); {$EndIf}

      if (MovieList.Count > 1) then begin
          TwoViews := (MovieList[0][length(MovieList[0]) - 4] = 'a') and (MovieList[1][length(MovieList[0]) - 4] = 'b') ;
          if TwoViews then begin
             aList := tStringList.Create;
             bList := tStringList.Create;
             i := 0;
             while (i < pred(MovieList.Count)) do begin
                aList.Add(MovieList[i]);
                bList.Add(MovieList[succ(i)]);
                inc(i,2);
             end;
             MovieList := aList;
             MovieUp := 'a';
          end;
          Button1.Visible := TwoViews;
      end;
   end;
   {$IfDef RecordMovieProblems} WriteLineToDebugFile('tMovieForm.LoadMovieList out w/ frames=' + IntToStr(MovieList.Count)); {$EndIf}
end;


procedure DeleteMovie(FName : PathStr);

   procedure DeleteTheFile;
   var
      inf   : textFile;
      Ext   : ExtStr;
      MenuStr : ShortString;
   begin
      Ext := UpperCase(ExtractFilePath(FName));
      if (Ext = '.GIF') or (Ext = '.AVI') then DeleteFileIfExists(FName)
      else if FileExists(FName) then begin
         assignFile(inf,FName);
         reset(inf);
         while not EOF(inf) do begin
            readln(inf,MenuStr);
            DeleteFileIfExists(MenuStr);
         end;
         closeFile(inf);
         DeleteFileIfExists(FName);
         DeleteFileIfExists(FName + '.TGT');
         DeleteFileIfExists(FName + '.FLT');
      end;
   end;
   
var
   DefExt : byte;
begin
   if FileExists(FName) then DeleteTheFile
   else begin
{$IfDef MICRODEM}
      FName := DEMDefs.MovieDir;
{$Else}
      FName := '';
{$EndIf}
      DefExt := 0;
      while GetFileMultipleMask('movie to delete','MD/TBII format|*.MOV|AVI|*.AVI|GIF|*.GIF',FName,DefExt) do DeleteTheFile;
   end;
end;


procedure TMovieForm.EnableOptions;
begin
   BitBtn1.Enabled := (MovieList <> Nil);
   BitBtn2.Enabled := (MovieList <> Nil);
   BitBtn3.Enabled := (MovieList <> Nil);
   BitBtn4.Enabled := (MovieList <> Nil);
   BitBtn5.Enabled := (MovieList <> Nil);
   BitBtn6.Enabled := (MovieList <> Nil);
   Movie1.Enabled := (MovieList <> Nil);
   Advance1.Enabled := (MovieList <> Nil);
   Backward1.Enabled := (MovieList <> Nil);
   Loop1.Enabled := (MovieList <> Nil);
end;


procedure tMovieForm.StartMovie;
begin
   if (MovieList = Nil) or (MovieList.Count = 0) then exit;
   Self.DisplayImage(MovieList.Strings[0],true);
   MoveForward;
   EnableOptions;
   Timer1.Enabled := true;
   DisplayMode := ForwardLoop;
end;


procedure TMovieForm.DisplayImage(FName : PathStr; FirstFrame : boolean = false);
var
   Bitmap,Bitmap2 : tBitMap;
   bName  : NameStr;
begin
   FName := FName;
   if FileExists(FName) then begin
      MovieForm.Panel1.Visible := true;
      Bitmap := Petimage.LoadBitmapFromFile(fName);
      bName := ExtractFileName(fName);

      if CheckBox1.Checked then with Bitmap.Canvas do begin
         Pen.Color := clRed;
         MoveTo(TargetX-3,TargetY);
         LineTo(TargetX+3,TargetY);
         MoveTo(TargetX,TargetY-3);
         LineTo(TargetX,TargetY+3);
      end;
      if RepeatView then begin
         CreateBitmap(Bitmap2,Bitmap.Width,Bitmap.height);
         Bitmap2.Canvas.Draw(0,0,Bitmap);
         Bitmap2.Canvas.Draw(Bitmap.Width,0,Bitmap);
         MovieForm.Image2.Picture.Graphic := Bitmap;
         Bitmap2.Free;
      end
      else MovieForm.Image2.Picture.Graphic := Bitmap;

      if FirstFrame then begin
         if FirstFrame then begin
            if RepeatView then begin
              MovieForm.Panel1.Width := 2 * Bitmap.Width div ReductionFactor + 21;
            end
            else begin
               MovieForm.Panel1.Width := Bitmap.Width div ReductionFactor + 21;
            end;
            MovieForm.Panel1.Height := Bitmap.Height div ReductionFactor + 21;
         end;
         MovieForm.Image2.Height := MovieForm.Panel1.Height - 21;
         MovieForm.Image2.Width := MovieForm.Panel1.Width - 21;
      end;
      Bitmap.Free;

      Panel1.Left := (ClientWidth - Panel1.Width) div 2;
      Panel1.Top := Panel3.Height + (ClientHeight - Panel1.Height - Panel2.Height - Panel3.Height) div 2;

      Panel2.Caption := bName;
   end;
end;


procedure TMovieForm.MoveForward;
begin
   if (MovieList <> Nil) and (CurrentImage < pred(MovieList.Count)) then begin
      inc(CurrentImage,NumMove);
      if CurrentImage > pred(MovieList.Count) then CurrentImage := pred(MovieList.Count);

      {$IfDef RecordMovieFrameProblems} WriteLineToDebugFile('Move forward, display frame=' + IntToStr(CurrentImage)); {$EndIf}
      DisplayImage(MovieList.Strings[CurrentImage]);
      //TrackBar1.Position := round(200 * CurrentImage / MovieList.Count);
   end;
end;

procedure TMovieForm.MoveBackWard;
begin
   if (MovieList <> Nil) and (CurrentImage > 0) then begin
      dec(CurrentImage);
      {$IfDef RecordMovieFrameProblems} WriteLineToDebugFile('Move backward, display frame=' + IntToStr(CurrentImage)); {$EndIf}
      DisplayImage(MovieList.Strings[CurrentImage]);
      //TrackBar1.Position := round(200 * CurrentImage / MovieList.Count);
   end;
end;


procedure TMovieForm.FormCreate(Sender: TObject);
var
   Bitmap : tBitMap;
begin
{$IfDef RecordMovieProblems} WriteLineToDebugFile('TMovieForm.FormCreate'); {$EndIf}
   CurrentImage := 0;
   NumMove := 1;
   DisplayMode := SlideOnly;
   EnableOptions;
   WindowState := wsMaximized;
   Bitmap := tBitmap.Create;
   Image2.Picture.Graphic := Bitmap;
   Bitmap.free;
   ReductionFactor := 1;
   MovieCaptions := Nil;
   MovieList := Nil;
   aList := Nil;
   bList := Nil;
   SideBySideMerge1.Visible := TrilobiteComputer;
   RepeatView := false;
   LastTime := -99;
{$IfDef MICRODEM}
   MovieDirectory := DEMDefs.MovieDir;
{$Else}
   MovieDirectory := ProgramRootDirectory;
{$EndIf}
end;


procedure TMovieForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Timer1.Enabled := false;
   Delay(5000);
   ApplicationProcessMessages;
   if (MovieCaptions <> Nil) then MovieCaptions.Free;
   if (MovieList <> Nil) then MovieList.Free;
   Action := caFree;
   {$IfDef RecordMovieProblems} WriteLineToDebugFile('TMovieForm.FormClose');  {$EndIf}
end;


procedure TMovieForm.Close1Click(Sender: TObject);
begin
   Close;
end;

procedure TMovieForm.Load1Click(Sender: TObject);
var
   InFile : PathStr;
   DefExt : byte;
begin
   InFile := MovieDirectory;
   DefExt := 0;
   Animate1.FileName := '';
   Animate1.Visible := false;
   Animate1.Active := false;

   Panel1.Align := alNone;
   Panel1.Visible := false;
   Panel1.Width := 21;
   Panel1.Height := 21;

   if GetFileMultipleMask('movie','Any movie|*.MOV',InFile,DefExt) then begin
      OpenFile(Infile);
      Caption := 'Movie Player: ' + ExtractFileName(InFile);
      Panel2.Caption := '';
   end;
end;


procedure TMovieForm.OpenFile(Infile : PathStr);
var
   Ext   : ExtStr;
   inf   : textfile;
begin
   Ext := UpperCase(ExtractFileExt(InFile));
   (*
   BitBtn1.Enabled := (Ext = '.MOV');
   BitBtn4.Enabled := (Ext = '.MOV');
   BitBtn5.Enabled := (Ext = '.MOV');
   BitBtn1.Visible := (Ext = '.MOV');
   BitBtn2.Visible := (Ext = '.MOV');
   BitBtn3.Visible := (Ext = '.MOV');
   BitBtn4.Visible := (Ext = '.MOV');
   BitBtn5.Visible := (Ext = '.MOV');
   BitBtn6.Visible := (Ext = '.MOV');
   ScrollBar1.Visible := (Ext = '.MOV');
   TrackBar1.Visible := (Ext = '.MOV');
   Label1.Visible := (Ext = '.MOV');
*)
      if (MovieList <> Nil) then MovieList.Free;
      if FileExists(Infile + '.TGT') then begin
         CheckBox1.Visible := true;
         assignFile(inf,Infile + '.TGT');
         reset(inf);
         readln(inf,TargetX,TargetY);
         closeFile(inf);
      end;
      LoadMovieList(InFile);
      StartMovie;
      MoveForward;
      EnableOptions;
   //ScrollBar1.Enabled := (Ext = '.MOV');
end;


procedure TMovieForm.RadioGroup1Click(Sender: TObject);
begin
   NumMove := succ(RadioGroup1.ItemIndex);
end;

procedure TMovieForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case Upcase(Key) of
      'F' : MoveForward;
      'B' : MoveBackward;
   end;
end;


procedure TMovieForm.FormResize(Sender: TObject);
begin
   if (MovieList <> Nil) then DisplayImage(MovieList.Strings[CurrentImage]);
end;

procedure TMovieForm.BitBtn2Click(Sender: TObject);
begin
   DisplayMode := SlideOnly;
   MoveForward;
   if (Animate1.FileName <> '') then Animate1.Active := true;
end;


procedure TMovieForm.BitBtn4Click(Sender: TObject);
begin
   DisplayMode := SlideOnly;
   MoveBackward;
end;


procedure TMovieForm.FormKeyDown(Sender: TObject; var Key: Word;   Shift: TShiftState);
begin
   MoveForward;
end;

procedure TMovieForm.BitBtn1Click(Sender: TObject);
begin
   DisplayMode := MovieForward;
   if (Animate1.FileName <> '') then Animate1.Active := true;
end;


procedure TMovieForm.BitBtn5Click(Sender: TObject);
begin
   DisplayMode := MovieBackward;
end;


procedure TMovieForm.BitBtn3Click(Sender: TObject);
begin
   DisplayMode := SlideOnly;
   if (Animate1.FileName <> '') then Animate1.Active := false;
end;


procedure TMovieForm.Timer1Timer(Sender: TObject);
begin
   if DisplayMode in [MovieForward,ForwardLoop] then begin
      MoveForward;
      if (DisplayMode = ForwardLoop) and (CurrentImage = pred(MovieList.Count)) then  CurrentImage := -1;
   end;
   if (DisplayMode = MovieBackWard) then MoveBackward;
end;

procedure TMovieForm.Timedelay1Click(Sender: TObject);
var
   i : integer;
begin
   i := Timer1.Interval;
   ReadDefault('Delay between images (millisec)',i);
   Timer1.Interval := i;
end;


procedure TMovieForm.Advance1Click(Sender: TObject);
begin
   MoveForward;
end;


procedure TMovieForm.Backward1Click(Sender: TObject);
begin
   MoveBackward;
end;


procedure TMovieForm.BitBtn6Click(Sender: TObject);
begin
   DisplayMode := ForwardLoop;
end;


procedure TMovieForm.Forward1Click(Sender: TObject);
begin
   DisplayMode := MovieForward;
end;


procedure TMovieForm.Backward2Click(Sender: TObject);
begin
   DisplayMode := MovieBackward;
end;

procedure TMovieForm.Loop1Click(Sender: TObject);
begin
   DisplayMode := ForwardLoop;
end;

procedure TMovieForm.ScrollBar1Change(Sender: TObject);
begin
   Timer1.Interval := ScrollBar1.Position;
end;

procedure TMovieForm.AddTitleToMovie;
var
   Bitmap : tBitmap;
   i      : integer;
   TheTitle : ShortString;
begin
   CurrentImage := 0;
   DisplayImage(MovieList.Strings[CurrentImage]);
   if AnswerIsYes('Movie title') then begin
      Bitmap := tBitmap.Create;
      Bitmap.LoadFromFile(MovieList.Strings[0]);
      Bitmap.Canvas.Font.Color := clLime;
      Bitmap.Canvas.Font.Size := 18;
      Bitmap.Canvas.Font.Name := 'Times New Roman';
      FontDialog1.Font := Bitmap.Canvas.Font;
      FontDialog1.Execute;
      Bitmap.Canvas.Font := FontDialog1.Font;

      TheTitle := '';
      GetString('Movie title',TheTitle,False,ReasonableTextChars);
      Bitmap.Canvas.Brush.Style := bsClear;
      Bitmap.Canvas.TextOut(50,50,TheTitle);

      Bitmap.Canvas.Font.Size := 14;
      Bitmap.Canvas.TextOut(Bitmap.Width - 175,Bitmap.Height-25,'MICRODEM');
      DisplayNevadella(BitMap.Canvas,Bitmap.Width-110,Bitmap.Height-200,1,clBlue);
      for i := 0 to 5 do begin
         Bitmap.SaveToFile(ExtractFilePath(MovieBaseName) + 'title' + integerToString(i,1) + '.bmp');
         MovieList.Insert(0,'title' + integerToString(i,1) + '.bmp');
      end;
      MovieList.SaveToFile(MovieBaseName);
      Bitmap.Free;
      DisplayImage(MovieList.Strings[CurrentImage]);
   end;
end;


procedure TMovieForm.Sidebysidemerge1Click(Sender: TObject);
var
   DataPath1,Infile : PathStr;
   Mlist1,Mlist2,MList3,MovList      : tStringList;
   Bitmap,Bitmap2                    : tBitmap;
   xs,ys,i                      : integer;
begin
   InFile := MovieDirectory;
   if GetFileFromDirectory('Movie 1','*.mov',InFile) then begin
      MList1 := tStringList.Create;
      MList1.LoadFromFile(InFile);
      DataPath1 := ExtractFilePath(InFile);
      if GetFileFromDirectory('Movie 2','*.mov',InFile) then begin
         MList2 := tStringList.Create;
         MList2.LoadFromFile(InFile);
         if GetFileFromDirectory('Movie 3','*.mov',InFile) then begin
            MList3 := tStringList.Create;
            MList3.LoadFromFile(InFile);
         end
         else MList3 := Nil;

         Bitmap := tBitmap.Create;
         Bitmap.LoadFromFile(MList1.Strings[0]);
         xs := Bitmap.Width;
         ys := Bitmap.Height;
         MovList := tStringList.Create;
         StartProgress('Merge');
         for i := 0 to pred(MList1.Count) do begin
            UpdateProgressBar(i/MList1.Count);
            Bitmap2 := tBitmap.Create;
            Bitmap2.Height := ys;
            if MList3 = Nil then Bitmap2.Width := xs * 2 + 5
            else Bitmap2.Width := xs * 3 + 10;
            Bitmap.LoadFromFile(MList1.Strings[i]);
            Bitmap2.Canvas.Draw(0,0,Bitmap);
            if (i < MList2.Count) and FileExists(MList2.Strings[i]) then begin
               Bitmap.LoadFromFile(MList2.Strings[i]);
               Bitmap2.Canvas.Draw(xs + 5,0,Bitmap);
               if (MList3 <> Nil) and (i < MList3.Count) and FileExists(MList3.Strings[i]) then begin
                  Bitmap.LoadFromFile(MList2.Strings[i]);
                  Bitmap2.Canvas.Draw(xs*2 + 10,0,Bitmap);
               end;
               Bitmap2.SaveToFile(DataPath1 + 'merge' + IntegerToString(i,-2) + '.bmp');
               MovList.Add('merge' + integerToString(i,-2) + '.bmp');
            end;
            Bitmap2.Free;
         end;
         EndProgress;
         Bitmap.Free;
         Mlist2.Free;
         if MList3 <> Nil then MList3.Free;
         MovList.saveToFile(DataPath1  + 'merge.mov');
         MovList.Free;
      end;
      MList1.Free;
   end;
end;


procedure TMovieForm.Deletemovie1Click(Sender: TObject);
begin
   DeleteMovie('');
end;


procedure TMovieForm.Button1Click(Sender: TObject);
begin
   if MovieUp = 'a' then begin
      MovieList := bList;
      MovieUp := 'b';
   end
   else begin
      MovieList := aList;
      MovieUp := 'a';
   end;
end;


procedure TMovieForm.Addlogotoframes1Click(Sender: TObject);
begin
(*
var
   fName : PathStr;
   Bitmap,FrameBMP : tBitmap;
   i,x,y : integer;
begin
   fName := '';
   if PetImage.GetGraphicsFileName(fName,'Logo') then begin
      Bitmap := PetImage.LoadBitmapFromFile(fName);
      x := 5;
      y := 5;
      for i := 0 to pred(MovieList.Count) do begin
         fName := MovieList.Strings[i];
         FrameBMP := PetImage.LoadBitmapFromFile(fName);
         FrameBMP.Canvas.Draw(x,y,Bitmap);
         PetImage.SaveImageAsBMP(FrameBMP,true,fName);
         FrameBMP.Free;
      end;
      Bitmap.Free;
   end;
*)
end;

procedure TMovieForm.Addmovietitle1Click(Sender: TObject);
begin
   //AddTitleToMovie;
end;


procedure TMovieForm.Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TMovieForm.Copytoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image2);
end;

procedure TMovieForm.TrackBar1Change(Sender: TObject);
begin
   CurrentImage := round(TrackBar1.Position / 200 * pred(MovieList.Count));
   DisplayImage(MovieList.Strings[CurrentImage]);
end;



procedure TMovieForm.Createmoviepanningstillphoto1Click(Sender: TObject);
begin
(*
var
   fName : PathStr;
   Bitmap,bm2,bm3 : Graphics.tBitmap;
   FirstX,FirstY,LastX,LastY,FirstWidth,LastWidth,FirstHeight,LastHeight,
   MovieHeight,MovieWidth,NumFrames : integer;
   i,x1,y1,x2,y2 : Integer;
   MovieList : tStringList;
begin
   MovieList := tStringList.Create;


   fName := 'c:\temp\tiger2.jpg';
   Bitmap := PetImage.LoadBitmapFromFile(fName);
   FirstX := 1962-(3*1188 div 2);
   FirstY := 0;
   FirstWidth := 3*1188 div 2;  //2400;
   FirstHeight := 1188; //1600;
   LastX := 1502;  //1500;
   LastY := 96;  //720;


   fName := 'c:\temp\team.jpg';
   Bitmap := PetImage.LoadBitmapFromFile(fName);
   FirstX := 937;
   FirstY := 805;
   FirstWidth := 150;
   FirstHeight := 100;
   LastX := 0;
   LastY := 0;
   LastWidth := 2286;
   LastHeight := 1524;
   MovieWidth := 720;
   MovieHeight := 480;
   NumFrames := 120;
   for i := 0 to NumFrames do begin
      x1 := FirstX + i * (LastX - FirstX ) div NumFrames;
      y1 := FirstY + i * (LastY - FirstY ) div NumFrames;
      x2 := x1 + FirstWidth + i * (LastWidth - FirstWidth) div NumFrames;
      y2 := y1 + FirstHeight + i * (LastHeight - FirstHeight) div NumFrames;
      CreateBitmap(bm2,MovieWidth,MovieHeight,pf24bit);
      CreateBitmap(bm3,succ(x2-x1),succ(y2-y1),pf24bit);
      bm3.Canvas.CopyRect(Rect(0,0,bm3.Width,bm3.Height), Bitmap.Canvas,Rect(x1,y1,x2,y2));
      bm2.Canvas.StretchDraw(Rect(0,0,bm2.Width,bm2.Height),bm3);
      fName := 'pan_out' + intToStr(i) + '.bmp';
      bm2.SaveToFile(MovieDirectory + fName);
      bm2.Free;
      bm3.Free;
      MovieList.Insert(0,fName);
   end;

   fName := 'c:\temp\tiger.jpg';
   Bitmap := PetImage.LoadBitmapFromFile(fName);
   FirstX := 1962-(3*1188 div 2);
   FirstY := 0;
   FirstWidth := 2400;
   FirstHeight := 1600;
   LastX := 1500;
   LastY := 720;
   LastWidth := 240;
   LastHeight := 160;
   MovieWidth := 720;
   MovieHeight := 480;
   NumFrames := 120;
   for i := NumFrames downto 0 do begin
      x1 := FirstX + i * (LastX - FirstX ) div NumFrames;
      y1 := FirstY + i * (LastY - FirstY ) div NumFrames;
      x2 := x1 + FirstWidth + i * (LastWidth - FirstWidth) div NumFrames;
      y2 := y1 + FirstHeight + i * (LastHeight - FirstHeight) div NumFrames;
      CreateBitmap(bm2,MovieWidth,MovieHeight,pf24bit);
      CreateBitmap(bm3,succ(x2-x1),succ(y2-y1),pf24bit);
      bm3.Canvas.CopyRect(Rect(0,0,bm3.Width,bm3.Height), Bitmap.Canvas,Rect(x1,y1,x2,y2));
      bm2.Canvas.StretchDraw(Rect(0,0,bm2.Width,bm2.Height),bm3);
      fName := 'pan' + intToStr(i) + '.bmp';
      bm2.SaveToFile(MovieDirectory + fName);
      bm2.Free;
      bm3.Free;
      MovieList.Add(fName);
   end;


   fName := MovieDirectory + 'pan_movie.mov';
   MovieList.SaveToFile(fName);
   MovieList.Free;
   OpenFile(fName);
*)
end;



initialization
   {$IfDef UnitStartUpProblems}
   MessageToContinue('Petmove initialization');
   {$EndIf}
   MovieScenesApart := 0;
finalization
   {$IfDef RecordMovieProblems}
   WriteLineToDebugFile('RecordMovieProblems active in petmovie');
   {$EndIf}
   {$IfDef RecordMovieFrameProblems}
   WriteLineToDebugFile('RecordMovieFrameProblems active in petmovie   (slowdown)');
   {$EndIf}
end.



