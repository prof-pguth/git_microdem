unit drainage_controller;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


this is no longer used

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDrainage}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TDrainage_Process_Form = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    RadioGroup1: TRadioGroup;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    HelpBtn: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Outx,OutY,
    WantedDEM : integer;
  end;

  procedure StartDrainage;

implementation

{$R *.dfm}


uses
   Petmar_types,Petmar,
   DEMDefs,
   DEMCoord,
   Read_DEM;

var
   TarDEMDir : PathStr;


procedure StartDrainage;
var
  Drainage_Process_Form : TDrainage_Process_Form;
begin
  Drainage_Process_Form := TDrainage_Process_Form.Create(Application);
  Drainage_Process_Form.ShowModal;
  Drainage_Process_Form.Free;
end;


procedure TDrainage_Process_Form.BitBtn1Click(Sender: TObject);
var
   fName : PathStr;
begin
   {$IfDef RecordDrainage}
   WriteLineToDebugFile('TDrainage_Process_Form.BitBtn1Click');
   {$EndIf}
   fName := '';
   Read_DEM.LoadNewDEM(WantedDEM,fName);
   if (WantedDEM <> 0) then begin
      if (DEMGlb[WantedDEM].HeadRecs.DEMUsed <> UTMBasedDEM) and  (Abs(DEMGlb[WantedDEM].AverageXSpace - DEMGlb[WantedDEM].AverageYSpace) > 0.001) then begin
         MessageToContinue('UTM DEM with x = y spacing required');
         CloseSingleDEM(WantedDEM);
      end
      else begin
         BitBtn2.Enabled := true;
         BitBtn1.Enabled := false;
      end;
   end;
end;


procedure TDrainage_Process_Form.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TDrainage_Process_Form.BitBtn2Click(Sender: TObject);
var
   fName : PathStr;
   bFile : TStringList;
begin
   {$IfDef RecordDrainage}
   WriteLineToDebugFile('TDrainage_Process_Form.BitBtn2Click');
   {$EndIf}
   fName := MainMapData + 'drainage\dem.asc';
   DEMGlb[WantedDEM].SaveAsArcGridASCII(fName);
   bFile := tStringList.Create;
   bFile.Add('flood.exe ' + fName);
   bFile.Add('d8.exe ' + fName);
   bFile.Add('aread8.exe  ' + fName + ' -nc');
   bFile.Add('gridnet  ' + fName);
   fName := MDTempDir + 'drain.bat';
   bFile.SaveToFile(fName);
   bFile.Free;
   ChDir(TarDemDir);
   ExecuteFile(fName, '', '', SW_SHOW);
   BitBtn5.Enabled := true;
end;


procedure TDrainage_Process_Form.BitBtn3Click(Sender: TObject);
var
   fName : PathStr;
   bFile : TStringList;
   NewDEM : integer;
begin
   {$IfDef RecordDrainage}
   WriteLineToDebugFile('TDrainage_Process_Form.BitBtn3Click');
   {$EndIf}
   fName := MainMapData + 'drainage\dem.asc';
   bFile := tStringList.Create;
   bFile.Add('subbasinsetup ' + fName);

   fName := MDTempDir + 'drain.bat';
   bFile.SaveToFile(fName);
   bFile.Free;
   ChDir(TarDemDir);
   ExecuteFile(fName, '', '', SW_SHOW);
   Application.ProcessMessages;

   fName := MainMapData + 'drainage\demw.asc';
   Read_DEM.LoadNewDEM(NewDEM,fName);
end;



procedure TDrainage_Process_Form.RadioGroup1Click(Sender: TObject);
var
   fName : PathStr;
   TStr  : AnsiString;
   aDEM  : integer;
begin
   TStr := RadioGroup1.Items[RadioGroup1.ItemIndex];
   TStr := BeforeSpecifiedCharacter(TStr,'-');
   fName := MainMapData + 'drainage\dem' + TStr  + '.asc';;
   Read_DEM.LoadNewDEM(aDEM,fName);
   if (aDEM <> 0) then begin
      if DEMGlb[aDEM].HeadRecs.ElevUnits = Meters then DEMGlb[aDEM].HeadRecs.ElevUnits := OtherElev;
      DEMGlb[aDEM].AreaName := RadioGroup1.Items[RadioGroup1.ItemIndex];
      DEMGlb[aDEM].SelectionMap.Caption := DEMGlb[aDEM].AreaName;
   end;
end;


procedure TDrainage_Process_Form.BitBtn4Click(Sender: TObject);
var
   fName : PathStr;
   bFile : TStringList;
   tFile : TextFile;
   Row,Col : integer;
   OutZ,
   z : float;
begin
   {$IfDef RecordDrainage}
   WriteLineToDebugFile('TDrainage_Process_Form.BitBtn4Click');
   {$EndIf}
   fName := 'C:\d8-ltd\dtm_13.val';
   assignFile(tFile,fName);
   rewrite(tfile);

   OutX := 356;
   OutY := 400;
   with DEMGlb[WantedDEM],HeadRecs do begin
      GetElevMeters(OutX,OutY,OutZ);
      for Row := pred(NumRow) downto 0 do begin
         for Col := 0 to pred(NumCol) do begin
            if GetElevMeters(Col,Row,z) and ( ((OutX = Col) and (OutY = Row)) or (z > OutZ)) then write(tFile,z:8:1)
            else write(tFile,' 0.0');
         end;
         writeln(tfile);
      end;
      CloseFile(tFile);

      bFile := tStringList.Create;
      bFile.Add('Grid spacing along the x-direction =            ' + RealToString(AverageXSpace,8,2));
      bFile.Add('Grid spacing along the y-direction =            ' + RealToString(AverageYSpace,8,2));
      bFile.Add('DEM rectangle size along the x-direction =     ' + IntToStr(NumCol));
      bFile.Add('DEM rectangle size along the y-direction =    ' + IntToStr(NumRow));
      bFile.Add('Number of cells within the catchment =      ');
      bFile.Add('Depit threshold slope =                          0.600E-03');
      bFile.Add('Path threshold slope =                           0.100E-03');
      bFile.Add('Drainage directions method (LAD:1,LTD:2) =       2');
      bFile.Add('D8-LAD/D8-LTD method dampening factor =          1.00');
      bFile.SaveToFile('C:\d8-ltd\hap.in');

      bFile.Clear;
      bFile.Add('wpar');
      bFile.Add('wbb');
      bFile.Add('rn');
      bFile.Add('rbb');
   end;

   CloseSingleDEM(WantedDEM);

   ChDir('C:\d8-ltd\');

   fName := MDTempDir + 'd8-ltd.bat';
   bFile.SaveToFile(fName);
   bFile.Free;
   ChDir(TarDemDir);
   ExecuteFile(fName, '', '', SW_SHOW);
end;


procedure TDrainage_Process_Form.BitBtn5Click(Sender: TObject);
var
   fName : PathStr;
   bFile : TStringList;
   xutm,yutm : float;
begin
   {$IfDef RecordDrainage}
   WriteLineToDebugFile('TDrainage_Process_Form.BitBtn5Click');
   {$EndIf}
   OutX := 75;
   OutY := 5;
   DEMGlb[WantedDEM].DEMGridToUTM(OutX,OutY,xutm,yutm);

   fName := MainMapData + 'drainage\dem.asc';
   bFile := tStringList.Create;
   bFile.Add('netsetup ' + fName + ' -m  4 .4 .1 .05 20 -xy ' + RealToString(xutm,-12,-1) + ' ' + RealToString(yutm,-12,-1));

   fName := MDTempDir + 'drain.bat';
   bFile.SaveToFile(fName);
   bFile.Free;
   ChDir(TarDemDir);
   ExecuteFile(fName, '', '', SW_SHOW);
   Application.ProcessMessages;
   BitBtn3.Enabled := true;
end;


procedure TDrainage_Process_Form.FormCreate(Sender: TObject);
begin
   Petmar.CheckFormPlacement(Self,1);
end;


procedure TDrainage_Process_Form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\drainage_ops.htm');
end;

initialization
   TarDEMDir := '\tardem\tardem_noav\exec\';
finalization
   {$IfDef RecordDrainage}
   WriteLineToDebugFile('RecordDrainage active in drainage_controller');
   {$EndIf}
end.
