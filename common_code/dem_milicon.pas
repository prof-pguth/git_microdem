unit dem_milicon;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMilIcons}
{$EndIf}


interface                           

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, DB,
  System.UITypes,
  DemMapF,
  Petmar_types,Petmar_db,Petmar,
  Menus;
  

type
  tMilIcon = record
     MilSymFont,TextFont,UnitName,ParentName,TopText,BottomText : ShortString;
     Lat,Long : float64;
     UnitLevel,FillLevel,
     SymFontSize,TextFontSize,CharNum : int32;
     TextBold : boolean;
     MilSymColor,
     FillColor,TextColor : tPlatformColor;
  end;


type
  TMilIconsForm = class(TForm)
    TabControl1: TTabControl;
    IconMenu1: TMainMenu;
    Close1: TMenuItem;
    Symboltoclipboard1: TMenuItem;
    Help1: TMenuItem;
    Font1: TMenuItem;
    FontDialog1: TFontDialog;
    Savetofile1: TMenuItem;
    Options1: TMenuItem;
    Military0widthfonts1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Largefontsamples1: TMenuItem;
    Mulitipleicons1: TMenuItem;
    Shownavalsymbols1: TMenuItem;
    Showairsymbols1: TMenuItem;
    Panel1: TPanel;
    Image2: TImage;
    Label4: TLabel;
    Label3: TLabel;
    UpDown2: TUpDown;
    UpDown1: TUpDown;
    RadioGroup1: TRadioGroup;
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RadioGroup2: TRadioGroup;
    BitBtn4: TBitBtn;
    Edit1: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Edit2: TEdit;
    Edit4: TEdit;
    Label6: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    BitBtn5: TBitBtn;
    Newsymbolfont1: TMenuItem;
    extfont1: TMenuItem;
    ClipboardSpeedButton: TSpeedButton;
    Showmilitarygroundsymbols1: TMenuItem;
    procedure TabControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Close1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure Symboltoclipboard1Click(Sender: TObject);
    procedure Savetofile1Click(Sender: TObject);
    procedure Military0widthfonts1Click(Sender: TObject);
    procedure Largefontsamples1Click(Sender: TObject);
    procedure Mulitipleicons1Click(Sender: TObject);
    procedure Shownavalsymbols1Click(Sender: TObject);
    procedure Showairsymbols1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure extfont1Click(Sender: TObject);
    procedure Newsymbolfont1Click(Sender: TObject);
    procedure ClipboardSpeedButtonClick(Sender: TObject);
    procedure Showmilitarygroundsymbols1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     LastX,LastY,BoxSize : integer;
     StampBitmap : tMyBitmap;
     baseMap    : tMapForm;
     IconList,
     SavingName : PathStr;
     CurMilIcon : tMilIcon;
     procedure DrawSymbol;
     procedure HideLots;
  end;


var
  MilIconsForm : TMilIconsForm;


function CreateMilIcon(CurMilIcon : tMilIcon) : tMyBitmap;


procedure DrawMilIcon(var StampBitmap : tMyBitmap; xc,yc : integer; CurMilIcon : tMilIcon);

procedure SaveMilIcon(fName : PathStr; CurMilIcon : tMilIcon);
procedure GetMilIcon(Table : tMyData; var CurMilIcon : tMilIcon);

procedure PickFontAndCharacter(var FontName : ANSIString; var ch : AnsiChar);
procedure PickFontAndCharacterAndColorAndSize(var FontName : AnsiString; var ch : AnsiChar; var inColor : tPlatformColor; var inSize : byte);

function CleanUnitName(TStr : shortString) : shortstring;


implementation

{$R *.DFM}

uses
   demdatabase,DataBaseCreate,PetDBUtils,PetImage,DEMDefs, nevadia_main;


function CleanUnitName(TStr : shortString) : shortstring;
var
   i : integer;
begin
   Result := TStr;
   for i := length(TStr) downto 1 do
      if Result[i] in [' ',',','.'] then Delete(Result,i,1);
end;


procedure PickFontAndCharacter(var FontName : ANSIString; var ch : AnsiChar);
begin
  MilIconsForm := TMilIconsForm.Create(Application);
  with MilIconsForm do begin
     MilIconsForm.HideLots;
     ShowModal;
     FontName := CurMilIcon.MilSymFont;
     ch := AnsiChar(CurMilIcon.CharNum);
  end;
end;


procedure PickFontAndCharacterAndColorAndSize(var FontName : ANSIString; var ch : AnsiChar; var inColor : tPlatformColor; var inSize : byte);
begin
  MilIconsForm := TMilIconsForm.Create(Application);
  with MilIconsForm do begin
     with CurMilIcon do begin
         MilSymFont := FontName;
         MilSymColor := inColor;
         TextFontSize := inSize;
         TabControl1Change(nil);
         DrawSymbol;
     end;
     MilIconsForm.HideLots;
     ShowModal;
     FontName := CurMilIcon.MilSymFont;
     ch := AnsiChar(CurMilIcon.CharNum);
     inColor := CurMilIcon.MilSymColor;
     inSize := UpDown1.Position;
  end;
end;


procedure SaveMilIcon(fName : PathStr; CurMilIcon : tMilIcon);
var
   Table : tMyData;
   i : integer;
   bmp : tMyBitmap;
begin
   Table := tMyData.Create(fName);
   with Table,CurMilIcon do begin
      Insert;
      SetFieldByNameAsFloat('LAT',Lat);
      SetFieldByNameAsFloat('LONG',Long);
      SetFieldByNameAsString('UNITNAME',UnitName);
      SetFieldByNameAsString('PARENT',ParentName);
      SetFieldByNameAsString('TOP_TEXT',TopText);
      SetFieldByNameAsString('BTM_TEXT',BottomText);
      SetFieldByNameAsString('MILSYMFONT',MilSymFont);
      SetFieldByNameAsString('TEXTFONT',TextFont);
      SetFieldByNameAsInteger('UNITLEVEL',UnitLevel);
      SetFieldByNameAsInteger('FILLLEVEL',FillLevel);
      SetFieldByNameAsInteger('SYMFONTS',SymFontSize);
      SetFieldByNameAsInteger('TEXFONTS',TextFontSize);
      SetFieldByNameAsInteger('CHARNUM',CharNum);
      if TextBold then i := 1 else i := 0;
      SetFieldByNameAsInteger('TEXTBOLD',i);
      SetFieldByNameAsInteger('FILLCOLOR',ConvertPlatformColorToTColor(FillColor));
      SetFieldByNameAsInteger('TEXTCOLOR',ConvertPlatformColorToTColor(TextColor));
      SetFieldByNameAsInteger('FONTCOLOR',ConvertPlatformColorToTColor(CurMilIcon.MilSymColor));

      bmp := CreateMilIcon(CurMilIcon);
      fName := Petmar.NextFileNumber(MDTempDir, 'MilIcon-',OverlayFExt);
      PetImage.SaveBitmap(bmp,fName);
      bmp.Free;
      SetFieldByNameAsString('ICON',fName);
      Post;
      Table.Destroy;
      Free;
   end;
end;


procedure GetMilIcon(Table : tMyData; var CurMilIcon : tMilIcon);
begin
   with Table,CurMilIcon do begin
      Lat := GetFieldByNameAsFloat('LAT');
      Long := GetFieldByNameAsFloat('LONG');
      UnitName :=  GetFieldByNameAsString('UNITNAME');
      ParentName := GetFieldByNameAsString('PARENT');
      TopText := GetFieldByNameAsString('TOP_TEXT');
      BottomText := GetFieldByNameAsString('BTM_TEXT');
      MilSymFont := GetFieldByNameAsString('MILSYMFONT');
      TextFont := GetFieldByNameAsString('TEXTFONT');
      UnitLevel := GetFieldByNameAsInteger('UNITLEVEL');
      FillLevel := GetFieldByNameAsInteger('FILLLEVEL');
      SymFontSize := GetFieldByNameAsInteger('SYMFONTS');
      TextFontSize := GetFieldByNameAsInteger('TEXFONTS');
      CharNum := GetFieldByNameAsInteger('CHARNUM');
      TextBold  := GetFieldByNameAsInteger('TEXTBOLD') = 1;
      FillColor := ConvertTColorToPlatformColor(GetFieldByNameAsInteger('FILLCOLOR'));
      TextColor := ConvertTColorToPlatformColor(GetFieldByNameAsInteger('TEXTCOLOR'));
      CurMilIcon.MilSymColor := ConvertTColorToPlatformColor(GetFieldByNameAsInteger('FONTCOLOR'));
   end;
end;


procedure TMilIconsForm.HideLots;
begin
     RadioGroup1.Visible := false;
     RadioGroup2.Visible := false;
     Edit1.Visible := false;
     Edit2.Visible := false;
     UpDown2.Visible := false;
     Label1.Visible := false;
     Label2.Visible := false;
     Label3.Visible := false;
     Label4.Visible := false;
end;

procedure TMilIconsForm.TabControl1Change(Sender: TObject);
var
   Bitmap : tMyBitmap;
   i,j : integer;
   TStr : ShortString;
begin
   {$IfDef RecordMilIcons} WriteLineToDebugFile('TMilIconsForm.TabControl1Change in ' + TabControl1.Tabs[TabControl1.TabIndex]); {$EndIf}

   with TabControl1,CurMilIcon do begin
      TStr := TabControl1.Tabs[TabControl1.TabIndex];
      CurMilIcon.MilSymFont := TStr;
      RadioGroup1.Visible := false;
      RadioGroup2.Visible := false;
      BitBtn1.Visible := false;
      if (TStr = 'MapSym-FR-Land') or (TStr = 'MapSym-EN-Land') or (TStr = 'MapSym-NU-Land') then begin
         RadioGroup1.Visible := true;
         RadioGroup2.Visible := true;
         RadioGroup1.Enabled := true;
         RadioGroup2.Enabled := true;
         BitBtn1.Visible := true;
      end;
   end;
   {$IfDef RecordMilIcons}  WriteLineToDebugFile('TMilIconsForm.TabControl1Change found font'); {$EndIf}

   if Largefontsamples1.Checked then begin
      PetImage.CreateBitmap(Bitmap,640,560);
      Bitmap.Canvas.Font.Size := 28;
      BoxSize := 40;
      ScrollBox1.HorzScrollBar.Visible := true;
      ScrollBox1.VertScrollBar.Visible := true;
   end
   else begin
      ScrollBox1.HorzScrollBar.Visible := false;
      ScrollBox1.VertScrollBar.Visible := false;
      PetImage.CreateBitmap(Bitmap,480,400);
      Bitmap.Canvas.Font.Size := 18;
      BoxSize := 30;
   end;
   {$IfDef RecordMilIcons} WriteLineToDebugFile('Filling in image with font=' + CurMilIcon.MilSymFont); {$EndIf}
   Bitmap.Canvas.Font.Name := CurMilIcon.MilSymFont;

   for i := 32 to 255 do begin
      j := i - 32;
      Bitmap.Canvas.TextOut((j mod 16) * BoxSize, (j div 16) * BoxSize,chr(i));
   end;
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   {$IfDef RecordMilIcons} WriteLineToDebugFile('TMilIconsForm.TabControl1Change out'); {$EndIf}
end;


procedure TMilIconsForm.FormCreate(Sender: TObject);

    procedure AddTab(fName : ShortString);
    begin
       {$IfDef RecordMilIcons} WriteLineToDebugFile('add tab ' + fName); {$EndIf}
       if FontInstalled(fName) then TabControl1.Tabs.Add(fName);
    end;

begin
   {$IfDef RecordMilIcons} WriteLineToDebugFile('TMilIconsForm.FormCreate in'); {$EndIf}
   with CurMilIcon do begin
      TextFont := 'Arial';
      TextFontSize := 12;
      SymFontSize := 48;
      UpDown2.Position := TextFontSize;
      UpDown1.Position := SymFontSize;
      FillColor := claLime;
      CurMilIcon.MilSymColor := claBlack;
      ColorBitBtn(BitBtn1,FillColor);
      ColorBitBtn(BitBtn1,CurMilIcon.MilSymColor);
   end;
   IconList := '';
   BaseMap := Nil;

   ShowAirSymbols1.Checked := MDDef.MilAirIcons;
   ShowNavalsymbols1.Checked := MDDef.MilSeaIcons;
   TabControl1.Tabs.Clear;
   if MDDef.MilLandIcons then begin
      AddTab('MapSym-FR-Land');
      AddTab('MapSym-EN-Land');
      AddTab('MapSym-NU-Land');
   end;
   if MDDef.MilSeaIcons then begin
      AddTab('MapSym-FR-Sea');
      AddTab('MapSym-EN-Sea');
      AddTab('MapSym-NU-Sea');
   end;
   if MDDef.MilAirIcons then begin
      AddTab('MapSym-FR-Air');
      AddTab('MapSym-EN-Air');
      AddTab('MapSym-NU-Air');
   end;
   AddTab('WingDings');


   SafeMakeDir(MainMapData + 'Icons\');
   TabControl1Change(Nil);
   WMDEM.FormPlacementInCorner(Self);
   {$IfDef RecordMilIcons} WriteLineToDebugFile('TMilIconsForm.FormCreate out'); {$EndIf}
end;


procedure TMilIconsForm.Image1DblClick(Sender: TObject);
begin
   CurMilIcon.CharNum := 32 + LastX div BoxSize + LastY div BoxSize * 16;
   DrawSymbol;
end;


procedure TMilIconsForm.DrawSymbol;
begin
   if (StampBitmap <> Nil) then StampBitmap.Free;
   CreateBitmap(StampBitmap,200,200);
   with CurMilIcon do begin
      UnitLevel := RadioGroup1.ItemIndex;
      FillLevel := RadioGroup2.ItemIndex;
      UnitName := Edit1.Text;
      ParentName := Edit2.Text;
      TopText := Edit3.Text;
      BottomText := Edit4.Text;
   end;
   DrawMilIcon(StampBitmap,100,100,CurMilIcon);
   Image2.Picture.Graphic := StampBitmap;
   Label3.Caption := 'Symbol size (' + IntToStr(CurMilIcon.SymFontSize) + ')';
   Label4.Caption := 'Text size (' + IntToStr(CurMilIcon.TextFontSize) + ')';
end;


function CreateMilIcon(CurMilIcon : tMilIcon) : tMyBitmap;
begin
   CreateBitmap(Result,200,200);
   DrawMilIcon(Result,100,100,CurMilIcon);
   GetImagePartOfBitmap(Result);
end;

procedure DrawMilIcon(var StampBitmap : tMyBitmap; xc,yc : integer; CurMilIcon : tMilIcon);
var
   x,y : integer;
   TStr,TStr2,OpStr : ShortString;
   OldFont : tFont;
begin
   with CurMilIcon do begin
      OldFont := tFont.Create;
      OldFont.Name := StampBitmap.Canvas.Font.Name;
      OldFont.Size := StampBitmap.Canvas.Font.Size;
      OldFont.Color := StampBitmap.Canvas.Font.Color;
      OldFont.Style := StampBitmap.Canvas.Font.Style;

      StampBitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(CurMilIcon.MilSymColor);
      StampBitmap.Canvas.Font.Name := MilSymFont;
      StampBitmap.Canvas.Font.Size := SymFontSize;
      StampBitmap.Canvas.Brush.Style := bsClear;
      TStr := chr(CharNum);
      OpStr := '';
      if (UnitLevel > 0) then begin
         if (MilSymFont = 'DRMS T1') then OpStr := chr(UnitLevel + 47)
         else TStr := chr(UnitLevel + 48) + TStr;
      end;
      x := StampBitmap.Canvas.TextWidth(TStr);
      y := StampBitmap.Canvas.TextHeight(TStr);

      if (FillLevel > 0) then begin
         case FillLevel of
            1 : TStr2 := AnsiChar(132);
            2 : TStr2 := AnsiChar(131);
            3 : TStr2 := AnsiChar(130);
            4 : TStr2 := '@';
         end;
         StampBitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(FillColor);
         StampBitmap.Canvas.TextOut(xc - x div 2,yc-y div 2,TStr2);
      end;

      StampBitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(CurMilIcon.MilSymColor);
      StampBitmap.Canvas.TextOut(xc - x div 2,yc-y div 2,TStr);
      if (OpStr <> '') then StampBitmap.Canvas.TextOut(xc - x div 2,yc-y div 2,OpStr);

      if TextBold then StampBitmap.Canvas.Font.Style := [fsBold]
      else StampBitmap.Canvas.Font.Style := [];

      StampBitmap.Canvas.Font.Name := TextFont;
      StampBitmap.Canvas.Font.Size := TextFontSize;

      if (TopText <> '') then StampBitmap.Canvas.TextOut(xc{+x div 2}-StampBitmap.Canvas.TextWidth(TopText) div 2,yc-y div 2-StampBitmap.Canvas.TextHeight(TopText),TopText);
      if (BottomText <> '') then StampBitmap.Canvas.TextOut(xc{+x div 2}-StampBitmap.Canvas.TextWidth(BottomText) div 2,yc + y div 2 + 5,BottomText);
      if (UnitName <> '') then StampBitmap.Canvas.TextOut(xc-x div 2-StampBitmap.Canvas.TextWidth(UnitName),yc+y div 2-StampBitmap.Canvas.TextHeight(UnitName)-10,UnitName);
      if (ParentName <> '') then StampBitmap.Canvas.TextOut(xc+x div 2,yc+y div 2-StampBitmap.Canvas.TextHeight(ParentName)-10,ParentName);

      StampBitmap.Canvas.Font.Name := OldFont.Name;
      StampBitmap.Canvas.Font.Size := OldFont.Size;
      StampBitmap.Canvas.Font.Color := OldFont.Color;
      StampBitmap.Canvas.Font.Style := OldFont.Style;
      OldFont.Free;
      GetImagePartOfBitmap(StampBitmap);
   end;
end;


procedure TMilIconsForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;


procedure TMilIconsForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   CurMilIcon.SymFontSize := UpDown1.Position;
   DrawSymbol;
end;


procedure TMilIconsForm.RadioGroup1Click(Sender: TObject);
begin
   DrawSymbol;
end;


procedure TMilIconsForm.Edit1Change(Sender: TObject);
begin
   DrawSymbol;
end;

procedure TMilIconsForm.Edit2Change(Sender: TObject);
begin
   DrawSymbol;
end;

procedure TMilIconsForm.Edit3Change(Sender: TObject);
begin
   DrawSymbol;
end;

procedure TMilIconsForm.Edit4Change(Sender: TObject);
begin
   DrawSymbol;
end;

procedure TMilIconsForm.extfont1Click(Sender: TObject);
begin
   FontDialog1.Font.Name := CurMilIcon.TextFont;
   FontDialog1.Font.Color := ConvertPlatformColorToTColor(CurMilIcon.TextColor);
   FontDialog1.Font.Size := CurMilIcon.TextFontSize;

   if FontDialog1.Execute then begin
      CurMilIcon.TextFont := FontDialog1.Font.Name;
      CurMilIcon.TextFontSize := FontDialog1.Font.Size;
      CurMilIcon.TextColor := ConvertTColorToPlatformColor(FontDialog1.Font.Color);
      DrawSymbol;
   end;
   {$IfDef RecordMilIcons} WriteLineToDebugFile('TMilIconsForm.Font1Click out with font=' + CurMilIcon.MilSymFont); {$EndIf}
end;

procedure TMilIconsForm.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
   CurMilIcon.TextFontSize := UpDown2.Position;
   DrawSymbol;
end;

procedure TMilIconsForm.RadioGroup2Click(Sender: TObject);
begin
   DrawSymbol;
end;

procedure TMilIconsForm.RadioGroup5Click(Sender: TObject);
begin
   DrawSymbol;
end;

procedure TMilIconsForm.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,CurMilIcon.FillColor);
   DrawSymbol;
end;

procedure TMilIconsForm.BitBtn2Click(Sender: TObject);
begin
   Close1Click(Sender);
end;

procedure TMilIconsForm.BitBtn3Click(Sender: TObject);
begin
   QueryColor(BitBtn3,CurMilIcon.MilSymColor);
   DrawSymbol;
end;

procedure TMilIconsForm.BitBtn4Click(Sender: TObject);
begin
   {$IfDef ExMilIcons}
   {$Else}
      ChangeDEMNowDoing(AddMilIcon);
   {$EndIf}
end;

procedure TMilIconsForm.BitBtn5Click(Sender: TObject);
begin
   QueryColor(BitBtn5,CurMilIcon.TextColor);
   DrawSymbol;
end;

procedure TMilIconsForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TMilIconsForm.ClipboardSpeedButtonClick(Sender: TObject);
begin
   AssignImageToClipBoard(Image2);
end;

procedure TMilIconsForm.Close1Click(Sender: TObject);
var
   db : integer;
begin
   StampBitmap.Free;
   if (BaseMap <> Nil) then begin
      BaseMap.Image1.Picture.Graphic := BaseMap.SavedMapImage;
      db := BaseMap.LoadDataBaseFile(SavingName);
      GISdb[db].dbOpts.DBAutoShow := dbasIconField;
      BaseMap.DoFastMapRedraw;
      FreeAndNil(BaseMap.SavedMapImage);
      ChangeDEMNowDoing(JustWandering);
   end;
   Close;
end;


procedure TMilIconsForm.Help1Click(Sender: TObject);
begin
   DisplayHTMLTopic('images\military_unit_icons.htm');
end;


procedure TMilIconsForm.Symboltoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image2);
end;

procedure TMilIconsForm.Savetofile1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   CopyImageToBitmap(Image2,Bitmap);
   SaveBitmap(Bitmap,MainMapData + 'Icons\');
end;

procedure TMilIconsForm.Showairsymbols1Click(Sender: TObject);
begin
   ShowAirSymbols1.Checked := not ShowAirSymbols1.Checked;
   MDDef.MilAirIcons := ShowAirSymbols1.Checked;
   FormCreate(nil);
end;

procedure TMilIconsForm.Showmilitarygroundsymbols1Click(Sender: TObject);
begin
   Showmilitarygroundsymbols1.Checked := not Showmilitarygroundsymbols1.Checked;
   MDDef.MilLandIcons := Showmilitarygroundsymbols1.Checked;
   FormCreate(nil);
end;

procedure TMilIconsForm.Shownavalsymbols1Click(Sender: TObject);
begin
   Shownavalsymbols1.Checked := not Shownavalsymbols1.Checked;
   MDDef.MilSeaIcons := Shownavalsymbols1.Checked;
   FormCreate(nil);
end;

procedure TMilIconsForm.Military0widthfonts1Click(Sender: TObject);
begin
   Military0widthfonts1.Checked := not Military0widthfonts1.Checked;
   RadioGroup1.Enabled := Military0widthfonts1.Checked;
   RadioGroup2.Enabled := Military0widthfonts1.Checked;
end;


procedure TMilIconsForm.Mulitipleicons1Click(Sender: TObject);
var
   Units : tStringList;
   i : Integer;
   Bitmap : tMyBitmap;
begin
   if (IconList <> '') and FileExists(IconList) then begin
       ShowHourglassCursor;
       Units := tStringList.Create;
       Units.LoadFromFile(IconList);
       for i := 0 to pred(Units.Count) do begin
          Edit1.Text := Units.Strings[i];
          CopyImageToBitmap(Image2,Bitmap);
          SaveBitmap(Bitmap,ExtractFilePath(IconList) + CleanUnitName(Edit1.Text) + OverlayFExt);
          Bitmap.Free;
       end;
       Units.Free;
       ShowDefaultCursor;
   end;
end;


procedure TMilIconsForm.Newsymbolfont1Click(Sender: TObject);
begin
   FontDialog1.Font.Name := CurMilIcon.MilSymFont;
   FontDialog1.Font.Size := CurMilIcon.SymFontSize;
   FontDialog1.Font.Color := ConvertPlatformColorToTColor(CurMilIcon.MilSymColor);

   if FontDialog1.Execute then begin
      Military0WidthFonts1.Checked := false;
      RadioGroup2.ItemIndex := 0;
      RadioGroup2.Enabled := false;
      CurMilIcon.MilSymFont := FontDialog1.Font.Name;
      CurMilIcon.SymFontSize := FontDialog1.Font.Size;
      CurMilIcon.MilSymColor := ConvertTColorToPlatformColor(FontDialog1.Font.Color);
      TabControl1.Tabs.Add(FontDialog1.Font.Name);
      TabControl1.TabIndex := TabControl1.Tabs.Count - 1;
      TabControl1Change(Nil);
   end;
   {$IfDef RecordMilIcons} WriteLineToDebugFile('TMilIconsForm.Font1Click out with symbol font=' + CurMilIcon.MilSymFont); {$EndIf}
end;


procedure TMilIconsForm.Largefontsamples1Click(Sender: TObject);
begin
   Largefontsamples1.Checked := not Largefontsamples1.Checked;
   TabControl1Change(Nil);
end;


initialization
finalization
   {$IfDef RecordMilIcons} WriteLineToDebugFile('RecordMilIcons active in dem_milicons'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing dem_MilIcon out'); {$EndIf}
end.
