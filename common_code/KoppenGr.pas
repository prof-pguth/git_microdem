unit Koppengr;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordKoppen}
{$EndIf}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs,  Menus, Vcl.ExtCtrls,
  PETMAR,Petmar_db,Petmar_types;

type
  tClimateData = record
    Temps,Precips            : array[1..12] of float32;
    Lat,Long,Elevation,
    Tconst,
    WinterPrecip,
    SummerPrecip,
    TotalPrecip,
    MinT,MaxT,MeanT          : float64;
    HottestMonth,ColdestMonth,
    NumHotMonth,NumDryMonth  : integer;
    WhenRain                 : shortstring;
    Location                 : string;
    L1,L2,L3                 : AnsiChar;
  end;

  TKoppenGraph = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Saveimage1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Saveimage2: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Copytoclipboard2: TMenuItem;
    Modifygraphoptions1: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure RedrawGraph(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Saveimage2Click(Sender: TObject);
    procedure Copytoclipboard2Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Modifygraphoptions1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ClimateData : tClimateData;
    function TempY(T : float32) : integer;
    function PrecipY(P : float32) : integer;
    function MonthCenter(Month : integer) : integer;
    function MakeKoppenBitmap(inClientWidth,inClientHeight : integer) : tMyBitmap;
  end;

var
   //KoppenGraph: TKoppenGraph;
   ClimographDB : tMyData;

function OpenKoppenGraph(Width, Height : integer) : TKoppenGraph;  overload;
function OpenKoppenGraph(Width, Height : integer; ClimateData : tClimateData) : TKoppenGraph;  overload;
function GetKoppenColor(KoppenClass : shortstring; var Color : tPlatformColor) : boolean;
function KoppenLegend(Small : boolean = true) : tMyBitmap;
procedure CreateKoppenLegend(Small : boolean = true);
procedure LoadKoppenDefs;
function ProcessClimateStationData(var ClimateData : tClimateData) : boolean;
procedure LoadClimateData(var ClimateData : tClimateData);
function ClassifyClimate(var ClimateData : tClimateData) : boolean;



const
   NumKoppenClass = 35;
type
   tKoppenClass = record
      ClassName : string3;
      ClassDescription : string[64];
      ClassColor : tPlatformColor;
   end;
var
   KoppenClasses : array[1..NumKoppenClass] of tKoppenClass;
   KoppenDefsLoaded : boolean;


implementation

{$R *.DFM}

uses
   Koppen_opts,DEMDefs,DEMDef_routines,PETMath,PetImage, PetImage_form,demdatabase,PetDBUtils,
   Multigrid,DEMCoord;



procedure LoadClimateData(var ClimateData : tClimateData);
var
   I : integer;
   {$IfDef RecordKoppen}
      TempStr,RainStr : shortstring;
   {$EndIf}
begin
   {$IfDef RecordKoppen}
      TempStr := 'Temp:';
      RainStr := 'Rain:';
   {$EndIf}
   for I := 1 to 12 do begin
      DEMGlb[MultiGridArray[TempMG].Grids[i]].GetElevFromLatLongDegree(ClimateData.Lat,ClimateData.Long,ClimateData.Temps[i]);
      DEMGlb[MultiGridArray[PrecipMG].Grids[i]].GetElevFromLatLongDegree(ClimateData.Lat,ClimateData.Long,ClimateData.Precips[i]);
      ClimateData.Precips[i] := 0.1 * ClimateData.Precips[i];
      {$IfDef RecordKoppen}
         TempStr := TempStr + RealToString(ClimateData.Temps[i],6,1);
         RainStr := RainStr + RealToString(ClimateData.Precips[i],6,1);
      {$EndIf}
   end;
   {$IfDef RecordKoppen}
      if (ClimateData.Temps[1] < 32000) and (ClimateData.Precips[1] < 3200) then begin
         WriteLineToDebugFile('Koppen ' + LatLongDegreeToString(ClimateData.Lat,ClimateData.Long));
         WriteLineToDebugFile(TempStr);
         WriteLineToDebugFile(RainStr);
      end;
   {$EndIf}
end;



    function CheckDryClimate(var ClimateData : tClimateData) : boolean;
    var
       HumidLine,SteppeLine : float64;
       i  : integer;
    begin
       with ClimateData do begin
          MeanT := 0;
          for i := 1 to 12 do MeanT := MeanT + Temps[i];
          MeanT := DegCtoF( MeanT / 12);

          HumidLine  := 0.44 * (MeanT - TConst) * 2.54;

          if (HumidLine < TotalPrecip) then begin
             CheckDryClimate := false;
          end
          else begin
             L1 := 'B';
             CheckDryClimate := true;
             SteppeLine := 0.22 * (MeanT - TConst) * 2.54;
             if SteppeLine < TotalPrecip then L2 := 'S' else L2 := 'W';
             if DegFtoC(MeanT) > 18 then L3 := 'h' else L3 := 'k';
          end;
          MeanT := DegFtoC(MeanT);
       end;
    end;


    procedure CandDclimateMoreLetters(var ClimateData : tClimateData);
    begin
       with ClimateData do begin
          L2 := 'f';
          if WhenRain = 'Winter' then L2 := 's';
          if WhenRain = 'Summer' then L2 := 'w';
          if (MaxT > 22) then L3 := 'a'
          else begin
             if NumHotMonth >= 4 then L3 := 'b'
             else if MinT < -38 then L3 := 'd'
             else L3 := 'c';
          end;
       end;
    end;


   function ClassifyClimate(var ClimateData : tClimateData) : boolean;
   label
      GoodData;
   var
      i : integer;
   begin
      with ClimateData do begin
         L1 := ' ';
         L2 := ' ';
         L3 := ' ';

         for i := 1 to 12 do begin
            if (abs(Temps[i]) > 0.001) and (abs(Precips[i]) > 0.001) and (abs(Temps[i]) < 3200) and (abs(Precips[i]) < 3200) then goto GoodData;
         end;
         Result := false;
         exit;

       GoodData:;
         Result := true;

         MinT := 100;
         MaxT := -100;
         NumHotMonth := 0;
         NumDryMonth := 0;
         HottestMonth := 0;
         ColdestMonth := 0;
         for i := 1 to 12 do begin
            if Temps[i] > MaxT then begin
               MaxT := Temps[i];
               HottestMonth := i;
            end;
            if Temps[i] < MinT then begin
               MinT := Temps[i];
               ColdestMonth := i;
            end;
            if Temps[i] > 10 then inc(NumHotMonth);
            if Precips[i] < 6.1 then inc(NumDryMonth);
         end;

          TotalPrecip := 0;
          for i := 1 to 12 do TotalPrecip := TotalPrecip + Precips[i];

          WinterPrecip := 0;
          if (ClimateData.Lat > 0) then begin
             for i := 10 to 12 do ClimateData.WinterPrecip := WinterPrecip + Precips[i];
             for i := 1 to 3 do ClimateData.WinterPrecip := WinterPrecip + Precips[i];
          end
          else for i := 4 to 8 do ClimateData.WinterPrecip := ClimateData.WinterPrecip + ClimateData.Precips[i];

          SummerPrecip := 0;
          if (ClimateData.Lat > 0) then for i := 4 to 8 do SummerPrecip := SummerPrecip + Precips[i]
          else begin
             for i := 10 to 12 do ClimateData.SummerPrecip := ClimateData.SummerPrecip + ClimateData.Precips[i];
             for i := 1 to 3 do ClimateData.SummerPrecip := ClimateData.SummerPrecip + ClimateData.Precips[i];
          end;

          if (ClimateData.SummerPrecip > 0.7 * ClimateData.TotalPrecip) then begin
             TConst := 7;
             WhenRain := 'Summer';
          end
          else if ClimateData.WinterPrecip > 0.7 * ClimateData.TotalPrecip then begin
             TConst := 32;
             WhenRain := 'Winter';
          end
          else begin
             TConst := 19.5;
             WhenRain := 'Even';
          end;

         if not CheckDryClimate(ClimateData) then begin
            if (MinT >= 17.999999) then begin
               L1 := 'A';
               case NumDryMonth of
                     0  : L2 := 'f';
                  1..3  : L2 := 'm';
                  4..12 : L2 := 'w';
               end {case};
            end
            else begin
               if (MaxT <= 9.9999999) then begin
                  L1 := 'E';
                  if (MaxT > 0) then L2 := 'T';
                  if (MaxT < 0) then L2 := 'F';
               end
               else begin
                  if (MinT > -3) and (MinT < 18) and (MaxT > 10) then begin
                     L1 := 'C';
                     CandDclimateMoreLetters(ClimateData);
                  end
                  else begin
                     L1 := 'D';
                     if (MaxT > 10) then begin
                        CandDclimateMoreLetters(ClimateData);
                     end
                     else begin
                        L1 := '?';
                        MessageToContinue('Classification problem');
                     end;
                  end;
               end;
            end;
         end {if};
      end;
   end;


function ProcessClimateStationData(var ClimateData : tClimateData) : boolean;
var
   i : integer;
begin
   Result := false;
   for i := 1 to 12 do begin
      ClimateData.Precips[i] := 0;
      ClimateData.Temps[i] := 0;
   end;
   ClimateData.L1 := ' ';
   with GISdb[ClimateStationDB],MyData,ClimateData do begin
      Location := GetFieldByNameAsString('PLACE');
      Lat := GetFieldByNameAsFloat('LAT');
      Long := GetFieldByNameAsFloat('LONG');
      if GetFieldByNameAsString('ELEV') = '' then Elevation := -9999
      else Elevation := GetFieldByNameAsFloat('ELEV');
      for i := 1 to 12 do begin
         if GetFieldByNameAsString(MonthName[i] + '_TEMP') <> '' then Temps[i] := GetFieldByNameAsFloat(MonthName[i] + '_TEMP')
         else if (GetFieldByNameAsString(MonthName[i] + '_MAXT') <> '') and (GetFieldByNameAsString(MonthName[i] + '_MINT') <> '') then begin
            Temps[i] := 0.5 * (GetFieldByNameAsFloat(MonthName[i] + '_MAXT') + GetFieldByNameAsFloat(MonthName[i] + '_MINT'));
            Edit;
            SetFieldByNameAsFloat(MonthName[i] + '_TEMP',Temps[i]);
            Post;
         end
         else begin
            exit;
         end;
      end;
      for i := 1 to 12 do begin
         if (GetFieldByNameAsString(MonthName[i] + '_PRECIP') <> '') then Precips[i] := GetFieldByNameAsFloat(MonthName[i] + '_PRECIP')
         else exit;
      end;
   end;
   ClassifyClimate(ClimateData);
   Result := true;
end;


procedure LoadKoppenDefs;
var
   Table : tMyData;
   i : integer;
begin
   if (not KoppenDefsLoaded) then begin
      KoppenDefsLoaded := true;
      Table := tMyData.Create(KoppenDefFName);
      i := 0;
      while not Table.Eof do begin
         inc(i);
         KoppenClasses[i].ClassName := Table.GetFieldByNameAsString('CLASS');
         KoppenClasses[i].ClassDescription := Table.GetFieldByNameAsString('NAME');
         KoppenClasses[i].ClassColor := Table.PlatformColorFromTable;
         Table.Next;
         if (i=NumKoppenClass) then break;
      end;
      Table.Destroy;
   end;
end;

function GetKoppenColor(KoppenClass : shortstring; var Color : tPlatformColor) : boolean;
var
   i : integer;
begin
   Result := false;
   if (KoppenClass = '') then exit;
   for I := 1 to NumKoppenClass do begin
      if (KoppenClass = KoppenClasses[i].ClassName) then begin
         Color := KoppenClasses[i].ClassColor;
         Result := true;
         exit;
      end;
   end;
end;


function KoppenLegend(Small : boolean = true) : tMyBitmap;
var
   i : integer;
   Color : tPlatformColor;
   TStr : shortstring;
begin
   i := 340;
   CreateBitmap(Result,i,NumKoppenClass * 20+30);
   Result.Canvas.TextOut(5,0,'Koppen Climate Classes');
   for i := 1 to NumKoppenClass do begin
      if GetKoppenColor(KoppenClasses[i].ClassName,Color) then begin
         Result.Canvas.Brush.Style := bsSolid;
         Result.Canvas.Brush.Color := ConvertPlatformColorToTColor(Color);
         Result.Canvas.Rectangle(5,i*20,25,i*20+15);
         Result.Canvas.Brush.Style := bsClear;
         if Small then TStr := KoppenClasses[i].ClassName
         else TStr := KoppenClasses[i].ClassName + '  ' + KoppenClasses[i].ClassDescription;
         Result.Canvas.TextOut(30,I*20+2,TStr);
      end;
   end;
   PetImage.GetImagePartOfBitmap(Result);
end;


procedure CreateKoppenLegend(Small : boolean = true);
begin
   PetImage_form.DisplayBitmap(KoppenLegend(Small),'Koppen categories');
end;


function OpenKoppenGraph(Width,Height : integer) : TKoppenGraph;
begin
   Result := TKoppenGraph.Create(Application);
   ProcessClimateStationData(Result.ClimateData);
   Result.Width := Width;
   Result.Height := Height;
   Result.RedrawGraph(Nil);
end;

function OpenKoppenGraph(Width,Height : integer; ClimateData : tClimateData) : TKoppenGraph;
begin
   Result := TKoppenGraph.Create(Application);
   Result.ClimateData := ClimateData;
   ClassifyClimate(ClimateData);
   Result.Width := Width;
   Result.Height := Height;
   Result.RedrawGraph(Nil);
end;


var
   XOff,YBottom,YDown : integer;
   MonthWidth : integer;



    function TKoppenGraph.TempY(T : float32) : integer;
    begin
       if (T > MDDef.KoppenOpts.MaxTemp) then T := MDDef.KoppenOpts.MaxTemp;
       if (T < -40) then T := -40;
       TempY := YDown + round((MDDef.KoppenOpts.MaxTemp - T) / (MDDef.KoppenOpts.MaxTemp + 40) * (ClientHeight - YDown - YBottom));
    end;

    function TKoppenGraph.PrecipY(P : float32) : integer;
    begin
       if (P > MDDef.KoppenOpts.MaxPrecip) then P := MDDef.KoppenOpts.MaxPrecip;
       PrecipY := YDown + round((MDDef.KoppenOpts.MaxPrecip - P) / MDDef.KoppenOpts.MaxPrecip * (ClientHeight - YDown - YBottom));
    end;

procedure TKoppenGraph.Modifygraphoptions1Click(Sender: TObject);
begin
   Koppen_opts.KoppenOptions;
end;

function TKoppenGraph.MonthCenter(Month : integer) : integer;
    begin
       MonthCenter := XOff - MonthWidth + round(Month * (ClientWidth - (2 * XOff)) / 12);
    end;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{Classification follows                                                   }
{   Oliver, J.E., and Fairbridge, R.W., eds., 1987, The encyclopedia of   }
{      climatology: Van Nostrand Reinhold Company, New York, p.225-227.   }
{except: difference between Am and Aw climates follows                    }
{   McKnight, T.L., 1990, Physical geography: a landscape appreciation:   }
{      Prentice Hall, Englewood Cliffs, New Jersey, p.193-194.            }
{except: difference between summer and winter concentration of rain       }
{   Critchfield, H.J., 1974, General climatology (3d ed): Prentice-Hall,  }
{      Englewood Cliffs, New Jersey, p.146-148.                           }
{                                                                         }
{ I had to make the following assumptions:                                }
{   all decisions use < or >, so in the case of an = might be wrong.      }
{   summer half year April to Sept; winter half year Oct to March         }
{       (opposite in southern hemisphere)                                 }
{       for Cs,Cf,Cw and Ds,Df,Dw                                         }
{   classification as a B or not depends heavily upon the following:      }
{        summer concentration of rain is Apr-Sept > 70% annual total      }
{        winter concentration of rain is Oct--Mar > 70% annual total      }
{   I used the øC and cm versions of the boundaries                       }
{_________________________________________________________________________}
{ May 2000: changed C and D winter/summer rain/dry season to use the      }
{       70% rule like the B climates.                                     }
{_________________________________________________________________________}


function TKoppenGraph.MakeKoppenBitmap(inClientWidth,inClientHeight : integer) : tMyBitmap;
var
   i : integer;
   TStr : shortString;
begin
   with ClimateData do begin
      ClientWidth := inClientWidth;
      ClientHeight := inClientHeight;

      CreateBitmap(Result,ClientWidth,ClientHeight);
      Result.Canvas.Font.Style := [fsBold];
      Result.Canvas.Font.Size := MDDef.KoppenOpts.KoppenFontSize;

      YDown := 3 * Result.Canvas.TextHeight('-40') div 2;
      if MDDef.KoppenOpts.ShowTempAndRain then inc(YDown,20);
      YBottom := 5 + Result.Canvas.TextHeight('W');;

      XOff := 5 + Result.Canvas.TextWidth('-40');

      MonthWidth := (ClientWidth - 2 * XOff) div 12 div 2;

      with Result.Canvas do begin
         Rectangle(XOff,YDown,ClientWidth - XOff,ClientHeight - YBottom);

         Pen.Style := psDot;
         Result.Canvas.Font.Color := clRed;
         i := 40;
         while (i >= -40) do begin
            MoveTo(XOff-5,TempY(1.0*i));
            LineTo(ClientWidth-XOff,TempY(1.0*i));
            TStr := IntToStr(i);
            Result.Canvas.TextOut(Xoff - 4 - Result.Canvas.TextWidth(TStr),TempY(1.0*i)-Result.Canvas.TextHeight(TStr) div 2,TStr);
            dec(i,10);
         end {while};
         Result.Canvas.TextOut(0,5,DegSym + 'C');

         Pen.Color := clSilver;
         Pen.Width := 2;
         Pen.Style := psDash;
         MoveTo(XOff,TempY(22));  LineTo(ClientWidth-XOff,TempY(22));
         MoveTo(XOff,TempY(18));  LineTo(ClientWidth-XOff,TempY(18));
         MoveTo(XOff,TempY(10));  LineTo(ClientWidth-XOff,TempY(10));
         MoveTo(XOff,TempY(0));   LineTo(ClientWidth-XOff,TempY(0));
         MoveTo(XOff,TempY(-3));  LineTo(ClientWidth-XOff,TempY(-3));

         Pen.Style := psSolid;

         Result.Canvas.Font.Color := clBlack;
         for i := 1 to 12 do begin
            if (MonthWidth < 20) then TStr := MonthName[i][1]
            else TStr := MonthName[i];
            Result.Canvas.TextOut(MonthCenter(i)-Result.Canvas.TextWidth(TStr) div 2, ClientHeight - YBottom + 5, TStr);
         end;

         Result.Canvas.Font.Color := clBlue;
         for i := 0 to 7 do begin
            Result.Canvas.Pen.Color := clBlack;
            Result.Canvas.Pen.Width := 1;
            MoveTo(ClientWidth - XOff,PrecipY(10.0 * i));
            LineTo(ClientWidth - xoff + 5, PrecipY(10.0 * i));
            TStr := IntegerToString(10*i,2);
            Result.Canvas.TextOut(ClientWidth - xoff + 8,PrecipY(10.0 * i)-Result.Canvas.TextHeight(TStr) div 2,TStr);
         end;
         Result.Canvas.TextOut(ClientWidth - XOff + 5,2,'cm');

         for i := 1 to 12 do begin
               Pen.Color := clBlue;
               Pen.Width := 2;
               Rectangle(MonthCenter(i) - MonthWidth,PrecipY(Precips[i]), MonthCenter(succ(i)) - MonthWidth,ClientHeight - YBottom);
               Pen.Color := clRed;
               Pen.Width := 3;
               if (i = 12) then begin
                  MoveTo(ClientWidth -XOff,TempY(0.5 * (Temps[1] + Temps[12])));
                  LineTo(MonthCenter(i),TempY(Temps[i]));
               end;
               if (i > 1) then begin
                  MoveTo(MonthCenter(pred(i)),TempY(Temps[pred(i)]));
                  LineTo(MonthCenter(i),TempY(Temps[i]));
               end;
               Pen.Width := 1;
               ScreenSymbol(Result.Canvas,MonthCenter(i),TempY(Temps[i]),Box,2,ConvertTColorToPlatFormColor(clRed));
               if i = 12 then begin
                  Pen.Width := 3;
                  MoveTo(XOff,TempY(0.5 * (Temps[1] + Temps[12])));
                  LineTo(MonthCenter(1),TempY(Temps[1]));
                  Pen.Width := 1;
               end;
         end {for i};

         if MDDef.KoppenOpts.ShowTempAndRain then begin
            Result.Canvas.Font.Color := clRed;
            TStr := 'Mean:'+ RealToString(ClimateData.MeanT,5,1) + DegSym +'C';
            Result.Canvas.TextOut(35,25,TStr);
            Result.Canvas.Font.Color := clBlue;
            Result.Canvas.TextOut(35 + 10 +  TextWidth(TStr),25,'Rain ' + RealToString(ClimateData.TotalPrecip,5,1)+ ' cm  ' + WhenRain);
         end;

         if MDDef.KoppenOpts.ShowLatLong then TStr := LatLongDegreeToString(Lat,Long,VeryShortDegrees)
         else TStr := '';
         TStr := Location + '  ' + TStr + ' ' + L1 + L2 + L3;
         Caption := TStr + ' Climograph';
         if MDDef.KoppenOpts.ShowElevation and (Elevation > -999) then begin
            TStr := TStr + ' ' + RealToString(Elevation,-8,0) + ' m';
         end;

         while Result.Canvas.TextWidth(TStr) > (ClientWidth - 2 * xoff) do Result.Canvas.Font.Size := Result.Canvas.Font.Size - 1;
         Result.Canvas.Font.Color := clBlack;
         Result.Canvas.TextOut(ClientWidth div 2 - Result.Canvas.TextWidth(TStr) div 2,3,TStr);
      end {with};
   end;
end;


procedure TKoppenGraph.RedrawGraph(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   Bitmap := MakeKoppenBitmap(ClientWidth,ClientHeight);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;

procedure TKoppenGraph.FormResize(Sender: TObject);
begin
   RedrawGraph(Sender);
end;

procedure TKoppenGraph.Image1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TKoppenGraph.Close1Click(Sender: TObject);
begin
   Free;
end;

procedure TKoppenGraph.Copytoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TKoppenGraph.Copytoclipboard2Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TKoppenGraph.Saveimage1Click(Sender: TObject);
begin
   PetImage.SaveImageAsBMP(Image1);
end;

procedure TKoppenGraph.Saveimage2Click(Sender: TObject);
begin
  PetImage.SaveImageAsBMP(Image1);
end;

procedure TKoppenGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TKoppenGraph.FormCreate(Sender: TObject);
begin
   Width := MDDef.KoppenOpts.KopWidth;
   Height := MDDef.KoppenOpts.KopHeight;
   LoadKoppenDefs;
end;


initialization
   KoppenDefsLoaded := false;
finalization
end.

