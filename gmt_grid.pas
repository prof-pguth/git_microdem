unit gmt_grid;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

removed 2/27/2015


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordGMTGridProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,Controls, Forms, Dialogs,
  Petmar_types,Petmar, StdCtrls, Buttons, ExtCtrls;

type
  Tgmt_form = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RadioGroup1: TRadioGroup;
    Edit5: TEdit;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Edit6: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit7: TEdit;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    RadioGroup2: TRadioGroup;
    CheckBox2: TCheckBox;
    Label6: TLabel;
    Edit8: TEdit;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    BitBtn4: TBitBtn;
    Label7: TLabel;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
      function ExtentString(Sender: TObject) : shortString;
      function VerboseString : String8;
      function GridSpacingString : shortString;
      function SearchString : shortString;
      function GridSizeOK : boolean;
      procedure EnableButtons;
      procedure RunCommand(Command : string; fName : PathStr);
  public
    { Public declarations }
     nCol,nRow : integer;
     MinX,MinY,MaxX,MaxY : float;
     GridName: PathStr;
     ExtentEnabled : boolean;
     IntFName : array[0..3] of PathStr;
  end;


procedure Do_GMT_Grid;


implementation

{$R *.dfm}


uses
   Nevadia_Main,
   Read_DEM,

   {$IfDef ExTin}
   {$Else}
   DEM_TIN,
   {$EndIf}

   DEMDef_routines,
   DEM_Manager,
   DEMCoord,
   DEMDatum,
   DEMDefs, PETImage;



procedure Do_GMT_Grid;
var
   gmt_form: Tgmt_form;
   fname2 : PathStr;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
   InputFile : textFile;
   zmin,zmax : float;
   xyzTableName : PathStr;
   XFieldName,YFieldName, ZFieldName : string12;
begin
   gmt_form := Tgmt_form.Create(Application);
   gmt_form.IntFName[3] := DEMDefs.TINDir;
   if Petmar.GetFileFromDirectory('xyz triples','*.xyz',gmt_form.IntFName[3]) then with gmt_form do begin
      fSplit(gmt_form.IntFName[3],Dir,bName,Ext);
      fName2 := Dir + 'xyz-' + bName + '.rng';
      gmt_form.Edit9.Text := IntToStr(MDDef.GMTMaskRadius);
      gmt_form.GridName := Dir + bName + '.grd';
      gmt_form.IntFName[0] := Dir + 'mean-' + bName + '.xyz';
      gmt_form.IntFName[1] := Dir + 'median-' + bName + '.xyz';
      gmt_form.IntFName[2] := Dir + 'mode-' + bName + '.xyz';

      if FileExists(fName2) then begin
         AssignFile(InputFile,fName2);
         reset(InputFile);
         readln(InputFile,minx);
         readln(InputFile,maxx);
         readln(InputFile,minY);
         readln(InputFile,maxY);
         readln(InputFile,zmin);
         readln(InputFile,zmax);
         CloseFile(InputFile);
      end
      else begin
         {$IfDef ExTIN}
         {$Else}
         XFieldName := 'X';
         YFieldName := 'Y';
         ZFieldName := 'Z';
         DEM_TIN.ConvertXYZASCIItoDBF(Nil,gmt_form.IntFName[3],xyzTableName,MinX,MinY,MaxX,MaxY,zMin,zMax,
             XFieldName,YFieldName,ZFieldName,false);

        {$EndIf}
      end;

      with gmt_form do begin
         Edit5.Text := RealToString(MDDef.GMTxinc,-18,-8);
         Edit7.Text := RealToString(MDDef.GMTyinc,-18,-8);
         Edit1.Text := RealToString(MaxY,-18,-8);
         Edit2.Text := RealToString(MinY,-18,-8);
         Edit3.Text := RealToString(MinX,-18,-8);
         Edit4.Text := RealToString(MaxX,-18,-8);
         EnableButtons;
      end;

      gmt_form.ShowModal;
   end;
end;


procedure Tgmt_form.EnableButtons;
begin
   BitBtn2.Enabled := FileExists(IntFName[RadioGroup1.ItemIndex]);
   BitBtn3.Enabled := FileExists(GridName);
   Label1.Enabled := RadioGroup2.ItemIndex = 0;
   Edit6.Enabled := RadioGroup2.ItemIndex = 0;

   Label6.Enabled := RadioGroup2.ItemIndex = 2;
   Edit8.Enabled := RadioGroup2.ItemIndex = 2;
   RadioGroup3.Enabled := RadioGroup2.ItemIndex = 2;
end;


procedure Tgmt_form.RunCommand(Command : string; fName : PathStr);
var
   cList : tStringList;
begin
   cList := TStringList.Create;
   cList.Add(Command);
   cList.SaveToFile(TINDir + fName);
   cList.Free;
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile(TimeToStr(Now) + ' Tgmt_form.BitBtn1Click: ' + Command);
   {$EndIf}
   WinExecAndWait32(fName);
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile(TimeToStr(Now) + ' Tgmt_form.BitBtn1Click execute OK');
   {$EndIf}
end;


function Tgmt_form.VerboseString : String8;
begin
   if CheckBox2.Checked then Result := ' -V '
   else Result := '';
end;


function Tgmt_form.ExtentString(Sender: TObject) : shortString;
var
   Distance,Distance2,Bearing : float;
begin
   if (Sender = Edit5) then begin
      CheckEditString(Edit5.Text,MDDef.GMTxinc);
      if (abs(MDDef.GMTxinc) < 0.00000000001) then exit;
      if (RadioGroup4.ItemIndex = 1) then MDDef.GMTxinc := MDDef.GMTxinc * 60;
      if (RadioGroup4.ItemIndex = 2) then MDDef.GMTxinc := MDDef.GMTxinc * 3600;
      nCol := succ(trunc((MaxX - MinX) / MDDef.GMTxinc));
   end;

   if (Sender = Edit7) or ((Sender = Edit5) and Checkbox1.Checked) then begin
      CheckEditString(Edit7.Text,MDDef.GMTyinc);
      if (abs(MDDef.GMTyinc) < 0.00000000001) then exit;
      if (RadioGroup4.ItemIndex = 1) then MDDef.GMTyinc := MDDef.GMTyinc * 60;
      if (RadioGroup4.ItemIndex = 2) then MDDef.GMTyinc := MDDef.GMTyinc * 3600;
      nRow := succ(trunc((MaxY - MinY) / MDDef.GMTyinc));
   end;

   if (Sender = Edit1) or (Sender = Edit2) or (Sender = Edit3) or (Sender = Edit4) then begin
      CheckEditString(Edit3.Text,MinX);
      CheckEditString(Edit4.Text,MaxX);
      CheckEditString(Edit2.Text,MinY);
      CheckEditString(Edit1.Text,MaxY);
      nCol := succ(trunc((MaxX - MinX) / MDDef.GMTxinc));
      nRow := succ(trunc((MaxY - MinY) / MDDef.GMTyinc));
   end;
   nCol := 100 * (ncol div 100);
   nRow := 100 * (nRow div 100);

   Edit8.Text := RealToString(5*MDDef.GMTxinc,-18,-8);
   Edit10.Text := IntToStr(ncol);
   Edit11.Text := IntToStr(nRow);

   CalculateDistanceBearing(MaxY,0.5* (MaxX + MinX),MinY,0.5* (MaxX + MinX), Distance,Bearing);
   CalculateDistanceBearing(0.5* (MaxY + MinY),MaxX,0.5* (MaxY + MinY),MinX, Distance2,Bearing);
   Label5.Caption := RealToString((Distance2/pred(NCol)),-18,1) + ' x ' + RealToString(Distance/pred(NRow),-18,1) + ' m';

   Result := ' -R' + Edit3.Text + '/' + {Edit4.Text} RealToString(MinX + (nCol) * MDDef.GMTxinc,-18,-8) + '/' + Edit2.Text + '/' + {Edit1.Text}
       RealToString(MinY + (nRow) * MDDef.GMTyinc,-18,-8) + ' ';
end;


function Tgmt_form.GridSpacingString : shortString;
begin
   Result := ' -I' + ptTrim(Edit5.Text) + '/' + ptTrim(Edit7.Text) + ' ';
end;


function Tgmt_form.SearchString : shortString;
begin
   if RadioGroup3.ItemIndex = 1 then Result := ' -N8 '
   else Result := ' -N4';
end;



procedure Tgmt_form.BitBtn1Click(Sender: TObject);
var
   Command : string;
   TStr : shortString;
begin
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('TTgmt_form.BitBtn1Click');
   {$EndIf}
   if not GridSizeOK then exit;
   case RadioGroup1.ItemIndex of
      0 : TStr := 'blockmean';
      1 : TStr := 'blockmedian';
      2 : TStr := 'blockmode';
      else exit;
   end;
   Command := GMTDir + TStr + ExtentString(Sender) + VerboseString +
      IntFName[3] +   GridSpacingString + ' -bo > ' + IntFName[RadioGroup1.ItemIndex];
   {$IfDef RecordGMTGridProblems}
   if not FileExists(GMTDir + TStr + '.exe') then WriteLineToDebugFile('preprocessor exe missing');
   WriteLineToDebugFile(Command);
   {$EndIf}
   RunCommand(Command,'pre-proc.bat');
   BitBtn2.Enabled := true;
end;


procedure Tgmt_form.BitBtn2Click(Sender: TObject);
var
   Command : string;
begin
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('TTgmt_form.BitBtn2Click');
   {$EndIf}
   if not GridSizeOK then exit;
   Command := ' -bi3 ' + IntFName[RadioGroup1.ItemIndex] +  ExtentString(Sender) +
           GridSpacingString + ' -G' + GridName + VerboseString;
   case RadioGroup2.ItemIndex of
      0 : Command := GMTDir + 'surface' + Command + ' -T' + ptTrim(Edit6.Text) + ' -C0.1';
      1 : Command := GMTDir + 'triangulate' + Command;
      2 : Command := GMTDir + 'nearneighbor' + Command + ' -S' + ptTrim(Edit8.Text) + SearchString;
   end;
   {$IfDef RecordGMTGridProblems}
   if not FileExists(GMTDir + 'surface.exe') then WriteLineToDebugFile('surface.exe missing');
   WriteLineToDebugFile(Command);
   {$EndIf}
   RunCommand(Command,'grid.bat');
   BitBtn3.Enabled := true;
end;


procedure Tgmt_form.BitBtn3Click(Sender: TObject);
var
   WantedDEM : integer;
begin
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('TTgmt_form.BitBtn3Click');
   {$EndIf}
   if FileExists(GridName) then begin
      BitBtn4.Enabled := true;
      CloseAllDEMs;
      WantedDEM := 0;
      LoadNewDEM(WantedDem,GridName,true,'','GMT grid ' + GridName);
   end;
end;


procedure Tgmt_form.CheckBox1Click(Sender: TObject);
begin
   Edit7.Enabled := not CheckBox1.Checked;
end;


procedure Tgmt_form.RadioGroup1Click(Sender: TObject);
begin
   EnableButtons;
end;


procedure Tgmt_form.RadioGroup2Click(Sender: TObject);
begin
   EnableButtons;
end;

procedure Tgmt_form.Edit5Change(Sender: TObject);
begin
   if CheckBox1.Checked then Edit7.Text := Edit5.Text;
   ExtentString(Sender);
end;


procedure Tgmt_form.Edit7Change(Sender: TObject);
begin
   ExtentString(Sender);
end;


procedure MaskDEMwithXYZFile(DEM : integer; xyzname : PathStr);
var
   fName : PathStr;
   MaskDEM : integer;
   inf : TextFile;
   Lat,Long,xg,yg,z : float;
   WantedDEM,Num,
   x,y : integer;
   Bitmap : tMyBitmap;
begin
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('MaskDEMwithXYZFile');
   {$EndIf}
   if DEMGlb[DEM] <> Nil then begin
      SaveBackupDefaults;
      MDdef.MissingToSeaLevel := false;
      MaskDEM := 0;
      PetImage.CopyImageToBitmap(DEMGlb[DEM].SelectionMap.Image1,Bitmap);
      if OpenAndZeroNewDEM(true,DEMGlb[DEM].HeadRecs,MaskDEM) then begin
         AssignFile(inf,xyzname);
         reset(inf);
         Num := 0;
         while not EOF(inf) do begin
            inc(num);
            if (Num mod 5000 = 0) then DEMGlb[DEM].SelectionMap.Image1.Picture.Graphic := Bitmap;
            if (Num mod 1000 = 0) then wmDEM.SetPanelText(0,IntToStr(Num));
            readln(inf,long,lat,z);
            DEMGlb[DEM].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
            Petmar.ScreenSymbol(Bitmap.Canvas,x,y,FilledBox,1,clRed);
            DEMGlb[MaskDEM].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
            for x := round(xg-MDDef.GMTMaskRadius) to round(xg+MDDef.GMTMaskRadius) do
               for y := round(yg-MDDef.GMTMaskRadius) to round(yg+MDDef.GMTMaskRadius) do
                  DEMGlb[MaskDEM].SetGridElevation(x,y,1);
         end;
         CloseFile(inf);
         DEMGlb[DEM].SelectionMap.Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;

         wmdem.StatusBar1.Panels[0].Text := '';
         for x := 0 to pred(DEMGlb[MaskDEM].HeadRecs.NumCol) do
            for y := 0 to pred(DEMGlb[MaskDEM].HeadRecs.NumRow) do begin
               if not DEMGlb[MaskDEM].MissingData(x,y) then begin
                  DEMGlb[MaskDEM].SetGridElevation(x,y,DEMGlb[DEM].GridElevMeters(x,y));
               end;
            end;
         fName := '';
         DEMGlb[MaskDEM].WriteNewFormatDEM(fName);
         CloseSingleDEM(MaskDEM);
         Read_DEM.LoadNewDEM(WantedDEM,fName);
      end;
      RestoreBackupDefaults;
   end;
end;


procedure Tgmt_form.BitBtn4Click(Sender: TObject);
begin
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('TTgmt_form.BitBtn4Click');
   {$EndIf}
   MaskDEMwithXYZFile(1,IntFName[3]);
end;

procedure Tgmt_form.Edit1Change(Sender: TObject);
begin
   ExtentString(Sender);
end;

procedure Tgmt_form.Edit3Change(Sender: TObject);
begin
   ExtentString(Sender);
end;

procedure Tgmt_form.Edit2Change(Sender: TObject);
begin
   ExtentString(Sender);
end;

procedure Tgmt_form.Edit4Change(Sender: TObject);
begin
   ExtentString(Sender);
end;


procedure Tgmt_form.Edit9Change(Sender: TObject);
begin
   CheckEditString(Edit9.Text,MDDef.GMTMaskRadius);
end;


procedure Tgmt_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('Tgmt_form.FormClose');
   {$EndIf}
   CloseAllDEMs;
   Action := caFree;
end;

procedure Tgmt_form.FormCreate(Sender: TObject);
begin
   Petmar.CheckFormPlacement(Self);
end;


function Tgmt_form.GridSizeOK : boolean;
begin
  Result := (ncol <= DEMDefs.MaxColsInRAM) and (nrow <= MaxElevArraySize);
  if not Result then Result := AnswerIsYes('Proceed with large grid; MICRODEM cannot read');
end;


initialization
finalization
   {$IfDef RecordGMTGridProblems}
   WriteLineToDebugFile('RecordGMTGridProblems active in gmt_grid');
   {$EndIf}
end.

