unit dem_tin;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

//{$Define ExTINGraph}
//{$Define TriangulationInterpolation}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordTINRoamProblems}
   //{$Define RecordTINProblems}
   //{$Define RecordXYZConvertProblems}
   //{$Define RecordXYZReadProblems}
   //{$Define RecordInterpolateImage}
   //{$Define RecordImportProblems}
   //{$Define RecordTINInterpolationProblems}      //degrades performance
   //{$Define RecordDetailedTINProblems}           //degrades performance
{$EndIf}


{Ware, J.M., 1998, A procedure for automatically correcting invalid flat triangles occurring in triangulated contour data: Computers & Geoscience, vol.14, no.2, p141-150.}


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
// end units for inline of the core DB functions

   {$IfDef TriangulationInterpolation}
      DEMEROSM,
   {$EndIf}

   Forms,SysUtils,Graphics,StdCtrls,Classes,Windows,Controls,
   PetdbUtils,PETMAR,Petmar_types,DEMMapf;

type
   tTIN = class
    private
       procedure FigureDeterminant; inline;
       procedure UseDeterminant(x,y : float64); inline;
    protected
       a,b,c,d,determinant,rhs1,rhs2,xc,yc,zc,
       z1,z2,z3,x1,y1,x2,y2,x3,y3,extra    : float64;
       xim1,yim1,xim2,yim2,xim3,yim3 : integer;
       procedure RealWorldCoordinates;  inline;
       procedure RealWorldElevations;   inline;
       procedure RealImageCoordinates;  inline;
    public
       TINTable : tMyData;
       ExtrapolateOK : boolean;
       BaseMap : tMapForm;
       TINContourInterval : float64;
       constructor Create(inBaseMap : tMapForm; fName : PathStr; NoZ : boolean; Quiet : boolean = false);
       destructor Destroy;
       function InterpolateZ(x,y : float64) : float64;
       function ElevInTriangle(x,y : float64; var z : float64) : boolean;
       procedure InterpolateDEM;
       function ImageCoordsInTriangle(x,y : float64; var xim,yim : integer) : boolean;
       procedure InterpolateImageCoords(x,y : float64; var xim,yim : integer);
       function XYInTriangle(x,y : float64; var xreal1,yreal1 : float64) : boolean;
       function InterpolateXY(x, y: float64; var xreal1, yreal1 : float64): boolean;
       procedure GetRangeInFile(var TheMinLat,TheMinLong,LatRange,LongRange : float64);

       procedure ShowTrianglesOnMap;
       {$IfDef TriangulationInterpolation}
          procedure InterpolateImage(Map : tMapForm; SatImageNum : integer; SatForm : TSatelliteForm = Nil; Progress : ShortString = '');
          procedure PickImageTriangle(SatForm : TSatelliteForm; xim,yim : integer);
          procedure ShowTriangulationOnSatForm(Map : TSatelliteForm);
       {$ENdIf}
       procedure DrawTriangulatedContoursOnMap;
       procedure UpdateBaseMap;
   end;


function ProcessASCIIXYZLine(MenuStr : AnsiString; ZMult : float32; var x,y,z : float32) : boolean;
function ConvertXYZASCIItoDBF(Memo1 : tMemo; var FileName,xyzTableName : PathStr; var xMin,yMin,xMax,yMax,zMin,zMax : float32; var XFieldName,YFieldName,ZFieldName : ShortString; CheckDupes : boolean = true) : boolean;


function SmallEnoughTriangle(x1,x2,x3,y1,y2,y3 : float32) : boolean; inline;

implementation


uses
   ContGraf,
   PetMath,PetImage,
   BaseGraf,
   DEMxyzIm,
   DataBaseCreate,
   DEMEROS,
   DEMdefs,DEMCoord,BaseMap,
   Nevadia_Main;

{$IfDef ExTINGraph}
{$Else}
   var
      PlotForm2 : TThisContourGraph; //can't go into tTIN due to circular references
{$EndIf}

function SmallEnoughTriangle(x1,x2,x3,y1,y2,y3 : float32) : boolean;
begin
   Result := (abs(x1-x2) < MDDef.MaxTriSide) and (abs(x1-x3) < MDDef.MaxTriSide) and (abs(x3-x2) < MDDef.MaxTriSide) and
             (abs(y1-y2) < MDDef.MaxTriSide) and (abs(y1-y3) < MDDef.MaxTriSide) and (abs(y3-y2) < MDDef.MaxTriSide)
end;

function ConvertXYZASCIItoDBF(Memo1 : tMemo; var FileName,xyzTableName : PathStr; var xMin,yMin,xMax,yMax,zMin,zMax : float32; var XFieldName,YFieldName,ZFieldName : ShortString; CheckDupes : boolean = true) : boolean;
const
   LG = 'Likely grid ';
var
   LastNum,i,
   Count : integer;
   Lat,Long  : float64;
   x,y,z,
   LastX,LastY,xs,ys : float32;
   AddPoint,NeedX,NeedY : boolean;
   InputFile : TextFile;
   xyzTable : tMyData;
   TStr,
   MenuStr : AnsiString;
   fName : PathStr;
   Dir     : DirStr;
   bName   : NameStr;
   Ext     : ExtStr;
   rFile  : file;
   v : array[1..3] of float32;
   xyzStringList,
   xyStringList : tStringList;

   {$IfDef ExTINGraph}
   {$Else}
      PlotForm : TThisContourGraph;
   {$EndIf}

      procedure CheckExtremes;
      begin
         PetMath.CompareValueToExtremes(x,xMin,xMax);
         PetMath.CompareValueToExtremes(y,yMin,yMax);
         PetMath.CompareValueToExtremes(z,zMin,zMax);
      end;

begin
   {$IfDef RecordImportProblems} WriteLineToDebugFile('ConvertXYZASCIItoDBF in'); {$EndIf}
   xMin := 99e99;
   yMin := 99e99;
   zMin := 99999.9;
   xMax := -99e99;
   yMax := -99e99;
   zMax := -999999.9;
   LastX := -99999.9;
   LastY := -99999.9;
   NeedX := true;
   NeedY := true;
   Count := 0;
   Result := false;
   if (FileName = '') or (not (FileExists(FileName))) then begin
      FileName := ExtractFilePath(LastDEMName);
      if not GetFileFromDirectory('ASCII xyz grid','*.XYZ;xyz-*' + DefaultDBExt,FileName) then exit;
   end;
   FSplit(FileName,Dir,bName,Ext);

   if (Memo1 <> Nil) then Memo1.Visible := true;

   if ExtEquals(Ext,DefaultDBExt) then begin
      xyzTableName := FileName;
      xyzTable := tMyData.Create(xyzTableName);

      if not xyzTable.FieldExists('X') then XFieldName := 'LONG';
      if not xyzTable.FieldExists('Y') then YFieldName := 'LAT';

      if (ZFieldName = '') or (not xyzTable.FieldExists(ZFieldName)) then begin
         if (xyzTable.FieldExists('Z')) then ZFieldName := 'Z'
         else if (xyzTable.FieldExists('ELEV')) then ZFieldName := 'ELEV'
         else if (xyzTable.FieldExists('ELEVATION')) then ZFieldName := 'ELEVATION'
         else if (xyzTable.FieldExists('DEPTH')) then ZFieldName := 'DEPTH'
         else zFieldName := xyzTable.PickField('z values',NumericFieldTypes);
      end;

      AddDelauneyZ := (ZFieldName <> '');
      AddDelauneyImage := (ZFieldName = '');
      if AddDelauneyImage then for i := 4 to 5 do if (Pnt[i] = Nil) then New(Pnt[i]);

      fName := ChangeFileExt(xyzTableName,'.rng');
      if FileExists(fName) then begin
         AssignFile(InputFile,fName);
         reset(InputFile);
         readln(InputFile,xmin);
         readln(InputFile,xmax);
         readln(InputFile,ymin);
         readln(InputFile,ymax);
         readln(InputFile,zmin);
         readln(InputFile,zmax);
         CloseFile(InputFile);
      end
      else begin
         while (not xyzTable.EOF) do begin
            x := XYZTable.GetFieldByNameAsFloat(XFieldName);
            y := XYZTable.GetFieldByNameAsFloat(YFieldName);
            z := XYZTable.GetFieldByNameAsFloat(ZFieldName);
            CheckExtremes;
            XYZTable.Next;
         end;
      end;
      xyzTable.Destroy;
   end
   else begin
      xyStringList := tStringList.Create;
      xyStringList.Duplicates := dupIgnore;
      xyStringList.Sorted := true;

      if (Memo1 <> Nil) then begin
         Memo1.Lines.Add(FileName);
         Memo1.Lines.Add('');
      end;
      ImportParamsDialog := TImportParamsDialog.Create(Application);
      ImportParamsDialog.Edit1.Text := IntToStr(MDDef.ASCIIMissingValue);

      {$IfDef RecordImportProblems} WriteLineToDebugFile('Input file: ' + FileName); {$EndIf}
      assignFile(InputFile,FileName);
      reset(InputFile);
      for i := 1 to 15 do begin
         readln(InputFile,MenuStr);
         ImportParamsDialog.Memo1.Lines.Add(MenuStr);
         {$IfDef RecordImportProblems} WriteLineToDebugFile('Line ' + IntToStr(i) + ':  ' + MenuStr); {$EndIf}
      end {for i};
      ImportParamsDialog.Visible := false;

      if (ImportParamsDialog.ShowModal = mrCancel) then begin
          Result := false;
          exit;
      end
      else begin
         CheckEditString(ImportParamsDialog.Edit1.Text,MDDef.ASCIIMissingValue);
         {$IfDef RecordImportProblems} WriteLineToDebugFile('  Checked out file OK'); {$EndIf}

         FSplit(FileName,Dir,bName,Ext);
         xyzStringList := tStringList.Create;
         if (ImportParamsDialog.RadioGroup2.ItemIndex = 0) then begin
            TStr := ',LAT,LONG';
            WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',MDDef.DefaultUTMZone,MDDef.DefaultLatHemi,'ConvertXYZASCIItoDBF');
         end
         else TStr := '';
         if (ImportParamsDialog.RadioGroup2.ItemIndex in [2,3,4]) then begin
            XFieldName := 'LONG';
            YFieldName := 'LAT';
         end;
         xyzStringList.Add(XFieldName + ',' + YFieldName + ',Z' + TStr);
         xyzTableName := TINDir + 'xyz-' + bName + '.csv';

         {$IfDef ExTINGraph}
         {$Else}
            If ImportParamsDialog.CheckBox1.Checked then begin
               PlotForm := TThisContourGraph.Create(Application);
               PlotForm.OpenXYZFile(rfile);
            end;
         {$EndIf};

         reset(InputFile);
         while not EOF(InputFile) do begin
            inc(Count);
            if (Count mod 1000 = 0) then wmDEM.SetPanelText(0,IntToStr(Count));
            {$I-}
            readln(InputFile,MenuStr);

            if ProcessASCIIXYZLine(MenuStr,ImportParamsDialog.ZMult,x,y,z) then begin
               if CheckDupes then begin
                  LastNum := XYStringList.Count;
                  XYStringList.Add(RealToString(x,18,6) + RealToString(y,18,6));
                  if (XYStringList.Count > LastNum) then AddPoint := true
                  else begin
                     AddPoint := false;
                     {$IfDef RecordXYZReadProblems} WriteLineToDebugFile('Dupe pt ignored: ' + RealToString(x,18,6) + RealToString(y,18,6) + RealToString(z,18,6)); {$EndIf}
                  end;
               end
               else AddPoint := true;

               if AddPoint then begin
                  if ImportParamsDialog.RadioGroup2.ItemIndex = 0 then begin
                     WGS84DatumConstants.UTMtoLatLongDegree(x,y,Lat,Long);
                     TStr := ',' + RealToString(Lat,-12,-7) + ',' +  RealToString(Long,-12,-7);
                  end
                  else TStr := '';
                  xyzStringList.Add(RealToString(x,-18,-8) + ',' + RealToString(y,-18,-8) + ',' + RealToString(z,-12,-8) + TStr);
               end;

               {$IfDef ExTINGraph}
               {$Else}
                  If ImportParamsDialog.CheckBox1.Checked then begin
                     v[1] := x;
                     v[2] := y;
                     v[3] := z;
                     BlockWrite(rfile,v,1);
                  end;
               {$EndIf}

               if (abs(x) > 0.001) and (abs(Y) > 0.0001) then begin
                  if NeedX then begin
                     if (LastX < -99999) then LastX := x
                     else begin
                        if abs(LastX-x) > 0.00001 then begin
                           LastX := abs(LastX-x);
                           NeedX := false;
                        end;
                     end;
                  end;
                  if NeedY then begin
                     if (LastY < -99999) then LastY := y
                     else begin
                        if abs(LastY-y) > 0.00001 then begin
                           LastY := abs(LastY-y);
                           NeedY := false;
                        end;
                     end;
                  end;
                  CheckExtremes;
               end
               else dec(Count);
            end
            else dec(Count);
            while EOLn(InputFile) and (not EOF(InputFile)) do readln(InputFile);
            for i := 1 to pred(MDDef.ImportThinFactor) do if  (not EOF(InputFile)) then readln(InputFile);
         end {while};

         xyzStringList.SaveToFile(xyzTableName);
         xyzStringList.Free;
         xyzTable := tMyData.Create(xyzTableName);

         closeFile(inputFile);
         xyzStringList := tStringList.Create;
         xyzStringList.Add(RealToString(xmin,18,8) + '   =xmin');
         xyzStringList.Add(RealToString(xmax,18,8) + '   =xmax');
         xyzStringList.Add(RealToString(ymin,18,8) + '   =ymin');
         xyzStringList.Add(RealToString(ymax,18,8) + '   =ymax');
         xyzStringList.Add(RealToString(zmin,18,8) + '   =zmin');
         xyzStringList.Add(RealToString(zmax,18,8) + '   =zmax');

         {$IfDef RecordXYZReadProblems} WriteStringListToDebugFile(xyzStringList); {$EndIf}

         xyzStringList.SaveToFile(ChangeFileExt(xyzTableName,'.rng') );
         xyzStringList.Free;

         wmdem.StatusBar1.Panels[0].Text := '';
         if (Memo1 <> Nil) then begin
            Memo1.Lines.Add('x range:' + RealToString(xmin,15,5) + '--' + RealToString(xmax,15,5));
            Memo1.Lines.Add('y range:' + RealToString(ymin,15,5) + '--' + RealToString(ymax,15,5));
            Memo1.Lines.Add('z range:' + RealToString(zmin,15,5) + '--' + RealToString(zmax,15,5));
            if ImportParamsDialog.RadioGroup2.ItemIndex in [0,1] then begin
               Memo1.Lines.Add(LG + ' dx: ' + RealToString(LastX,-18,-1));
               Memo1.Lines.Add(LG + ' dy: ' + RealToString(LastY,-18,-1));
            end
            else begin
               Memo1.Lines.Add(LG + ' dx: ' + RealToString(LastX,-18,-8) + '°  ' + RealToString(60*LastX,-18,-3) + '''  ' + RealToString(3600*LastX,-18,-3) + '"' );
               Memo1.Lines.Add(LG + ' dy: ' + RealToString(LastY,-18,-8) + '°  ' + RealToString(60*LastY,-18,-3) + '''  ' + RealToString(3600*LastY,-18,-3) + '"' );
            end;
            Memo1.Lines.Add('xyz triples:' + IntegerToString(Count,8));
            if (Count <> XYZTable.RecordCount) then begin
               Memo1.Lines.Add('Dupe points: ' + IntToStr(Count - XYZTable.RecordCount));
            end;
         end;
         if (Count = 0) then begin
            MessageToContinue('No valid data points');
            exit;
         end;
         XYStringList.Free;

         {$IfDef ExTINGraph}
         {$Else}
            If ImportParamsDialog.CheckBox1.Checked then begin
               PlotForm.Caption := FileName + ' Data Points';
               with PlotForm,GraphDraw do begin
                    MinHorizAxis := xMin;
                    MaxHorizAxis := xMax;
                    MinVertAxis := yMin;
                    MaxVertAxis := yMax;
                    MaxZ := zMax / ImportParamsDialog.zMult;
                    MinZ := zMin / ImportParamsDialog.zMult;
                    SetUpGraphForm;
                    GraphDraw.CorrectScaling := true;
                    CloseFile(rfile);
                    PadAxis(MinHorizAxis,MaxHorizAxis);
                    PadAxis(MinVertAxis,MaxVertAxis);
                    GraphDraw.GraphDrawn := false;
                    xs := ClientWidth - LeftMargin;
                    ys := ClientHeight - BottomMargin;
                    xs := (MaxHorizAxis - MinHorizAxis) / xs;
                    ys := (MaxVertAxis - MinVertAxis) / ys;
                    if (XS/ys < 5) and (xs/ys > 0.2) then begin
                       if xs < ys then ClientWidth := round( (MaxHorizAxis - MinHorizAxis) / ys + LeftMargin)
                       else ClientHeight := round( (MaxVertAxis - MinVertAxis) / xs + BottomMargin);
                    end;
                    GraphDraw.GraphDrawn := true;
                    RedrawDiagram11Click(nil);
               end;
            end;
         {$EndIf}
         xyzTable.Destroy;
      end;
   end;
   Result := true;
end;


procedure tTIN.ShowTrianglesOnMap;
var
   x,y,xs1,ys1,xs2,ys2,xs3,ys3 : integer;
begin
   if (BaseMap <> Nil) then begin
      {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.ShowTrianglesOnMap'); {$EndIf}
      BaseMap.Image1.Canvas.Pen.Width := MDDef.DelaunayLineThick;
      BaseMap.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.DelaunayLineColor);
      TinTable.First;
      while not TinTable.eof do begin
         RealWorldCoordinates;
         BaseMap.MapDraw.UTMToScreen(y1,x1,xs1,ys1);
         BaseMap.MapDraw.UTMToScreen(y2,x2,xs2,ys2);
         BaseMap.MapDraw.UTMToScreen(y3,x3,xs3,ys3);

         BaseMap.Image1.Canvas.MoveTo(xs1,ys1);
         BaseMap.Image1.Canvas.LineTo(xs2,ys2);
         BaseMap.Image1.Canvas.LineTo(xs3,ys3);
         BaseMap.Image1.Canvas.LineTo(xs1,ys1);
         TinTable.Next;
      end;
   end;
end;


procedure tTIN.GetRangeInFile(var TheMinLat,TheMinLong,LatRange,LongRange : float64);
var
   MinLat,MinLong,MaxLong,MaxLat : array[1..3] of float64;
begin
   TINTable.FindFieldRange('X1',MinLong[1],MaxLong[1]);
   TINTable.FindFieldRange('X2',MinLong[2],MaxLong[2]);
   TINTable.FindFieldRange('X3',MinLong[3],MaxLong[3]);

   TINTable.FindFieldRange('Y1',MinLat[1],MaxLat[1]);
   TINTable.FindFieldRange('Y2',MinLat[2],MaxLat[2]);
   TINTable.FindFieldRange('Y3',MinLat[3],MaxLat[3]);

   TheMinLat := MinFloat(MinLat[1],MinLat[2],MinLat[3]);
   MaxLat[1] := MaxFloat(MaxLat[1],MaxLat[2],MaxLat[3]);
   LatRange := MaxLat[1] - TheMinLat;

   TheMinLong := MinFloat(MinLong[1],MinLong[2],MinLong[3]);
   MaxLong[1] := MaxFloat(MaxLong[1],MaxLong[2],MaxLong[3]);
   LongRange := MaxLong[1] - TheMinLong;
end;


constructor tTIN.Create;
var
   TriFName,XYZTableName,ASCIIFileName : PathStr;
   ThisFileName : NameStr;
   Dir   : DirStr;
   Ext   : ExtStr;
begin
   ExtrapolateOK := false;
   if (fName = '') then begin
      if (LastTINName = '') then fName := MainMapData + 'tins\';
      if not GetFileFromDirectory('XYZ or TIN data','*.XYZ;xyz-*.dbf;tin-*.dbf;*' + DefaultDBExt,fName) then exit;
      LastTINName := fName;
   end;

   if (inBaseMap <> Nil) then BaseMap := InBaseMap
   else BaseMap := Nil;

   ASCIIFileName := fName;
   FSplit(ASCIIFileName,Dir,ThisFileName,Ext);
   TriFName := Dir + 'tin-' + ThisFileName + DefaultDBExt;

   {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.create: ' + ASCIIFileName); {$EndIf}

  // if NoZ then ZChar := '' else ZChar := 'Z';

   ContGraf.ProcessXYZFile(ASCIIFileName,XYZTableName,false,false,'','','',0.00001);
   FigureIncrements;

   {$IfDef ExTINGraph}
   {$Else}
      {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.create CreateNewGraph(PlotForm2)'); {$EndIf}
      CreateNewGraph(PlotForm2);
      PlotForm2.MinZ := ContourData^.zMin;
      PlotForm2.MaxZ := ContourData^.zMax;
      PlotForm2.WhatsOnGraph := TriangulationContour;
      PlotForm2.TinfName:= TriFName;
      PlotForm2.TIN := self;
      PlotForm2.Contour;
      PlotForm2.TINInterpolateButton.Visible := true;
      PlotForm2.Panel1.Visible := true;
      PlotForm2.Panel1.Height := 35;
      PlotForm2 := Nil;
      TinTable := tMyData.Create(TriFName);
   {$EndIf}

   UpDateBaseMap;

   {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.create out'); {$EndIf}
end;



function ProcessASCIIXYZLine(MenuStr : AnsiString; ZMult : float32; var x,y,z : float32) : boolean;


     Procedure ProcessLatLong(var MenuStr : AnsiString; var DecDeg : float32);
     var
        Mult,Deg,Min,Sec : float32;
        err : integer;
        TStr : AnsiString;
     begin
         Val(BeforeSpecifiedCharacterANSI(MenuStr,'-',false,true),Deg,err);
         Val(BeforeSpecifiedCharacterANSI(MenuStr,'-',false,true),Min,err);
         TStr := BeforeSpecifiedCharacterANSI(MenuStr,' ',false,true);
         if TStr[length(TStr)] in ['N','E'] then Mult := 1 else Mult := -1;
         Delete(TStr,length(TStr),1);
         Val(TStr,Sec,err);
         DecDeg := Mult * (Deg + Min / 60 + Sec / 3600);
     end;

var
   err,i : integer;
   tf : float32;
begin
   for i := 1 to length(MenuStr) do if MenuStr[i] in [#9,','] then MenuStr[i] := ' ';
   while MenuStr[1] = ' ' do Delete(MenuStr,1,1);
   MenuStr := MenuStr + ' ';

   if (ImportParamsDialog.RadioGroup2.ItemIndex in [4]) then begin
      ProcessLatLong(MenuStr,y);
      ProcessLatLong(MenuStr,x);
      Val(BeforeSpecifiedCharacterANSI(MenuStr,' ',false,true),z,err);
   end
   else begin
      Val(BeforeSpecifiedCharacterANSI(MenuStr,' ',false,true),x,err);
      while MenuStr[1] = ' ' do Delete(MenuStr,1,1);
      Val(BeforeSpecifiedCharacterANSI(MenuStr,' ',false,true),y,err);
      while MenuStr[1] = ' ' do Delete(MenuStr,1,1);
      Val(BeforeSpecifiedCharacterANSI(MenuStr,' ',false,true),z,err);
   end;
   if (ImportParamsDialog.RadioGroup2.ItemIndex in [2,3]) and (x > 180) then x := x - 360;
   if ImportParamsDialog.RadioGroup2.ItemIndex in [2] then begin
      tf := y;
      y := x;
      x := tf;
   end;
   Result := (abs(z - MDDef.ASCIIMissingValue) > 0.0001) and (err = 0);
   Z := z * ZMult;
end;



destructor tTIN.Destroy;
begin
   {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.Destroy'); {$EndIf}
   TinTable.Destroy;
end;


procedure tTIN.InterpolateDEM;
var
   x,y,NewDEM,Count : integer;
   fName : PathStr;
   z,Lat,Long,xlo,ylo,xhi,yhi : float64;
   NewHeadRecs : tDEMheader;
   {$IfDef ExTINGraph}
   {$Else}
      Bitmap : tMyBitmap;
   {$EndIf}
begin
   {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.InterpolateDEM'); {$EndIf}

   NewHeadRecs := DEMGlb[BaseMap.MapDraw.DEMonMap].DEMheader;
   OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,'tin_dem',InitDEMmissing);
   DEMGlb[NewDEM].AreaName := 'TIN interpolation';
   with DEMGlb[NewDEM],DEMheader do begin
      StartProgress('Interpolate');
      TinTable.ApplyFilter('');
      Count := 0;
      {$IfDef ExTINGraph}
      {$Else}
         CopyImageToBitmap(PlotForm2.Image1,Bitmap);
      {$EndIf}
      while not TinTable.Eof do begin
         inc(count);
         if (Count mod 25 = 0) then UpdateProgressBar(Count/TinTable.RecordCount);
         LatLongDegreeToDEMGrid(TinTable.GetFieldByNameAsFloat('Y_LOW'),TinTable.GetFieldByNameAsFloat('X_LOW'),xlo,ylo);
         LatLongDegreeToDEMGrid(TinTable.GetFieldByNameAsFloat('Y_HI'),TinTable.GetFieldByNameAsFloat('X_HI'),xhi,yhi);
         for x := round(xlo) to round(xhi) do begin
            for y := round(ylo) to round(yhi) do begin
               DEMGridToLatLongDegree(x,y,lat,long);
               if ElevInTriangle(Long,Lat,z) then begin
                  {$IfDef ExTINGraph}
                  {$Else}
                     ScreenSymbol(Bitmap.Canvas,PlotForm2.GraphDraw.GraphX(long),Plotform2.GraphDraw.GraphY(lat),FilledBox,1,
                         PlatformRainBowColorFunct(z,ContourData^.zmin,ContourData^.zmax));
                  {$EndIf}
                  if GridInDataSetInteger(X,Y) then begin
                     SetGridElevation(x,y,z);
                  end;
               end
            end;
         end;
         {$IfDef ExTINGraph}
         {$Else}
            PlotForm2.Image1.Picture.Graphic := Bitmap;
         {$EndIf}
         TinTable.Next;
      end {while};
      EndProgress;
      {$IfDef ExTINGraph}
      {$Else}
         Bitmap.Free;
      {$EndIf}
      fName := MainMapData + 'tins\tin-dem.dem';
      {$IfDef RecordTINProblems} WriteLineToDebugFile('writing'); {$EndIf}
      WriteNewFormatDEM(fName);
      DEMGlb[NewDEM].SetUpMap(true);
      SelectionMap.Panel1.Visible := false;
      SelectionMap.Panel1.Height := 0;

      {$IfDef RecordTINProblems} WriteLineToDebugFile('set map, interpolation done'); {$EndIf}
   end;
end;

procedure tTIN.FigureDeterminant;
begin
    a := x1 - x3;
    b := x2 - x3;
    c := y1 - y3;
    d := y2 - y3;
    Determinant := a*d - b*c;
    {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Det: ' +  RealToString(determinant,-20,-8)); {$EndIf}
end;


procedure tTin.UseDeterminant(x,y : float64);
begin
   RHS1 := X - x3;
   RHS2 := Y - y3;
   xC := (RHS1 * d - RHS2 * b) / determinant;
   yC := (RHS2 * a - RHS1 * c) / determinant;
   zC := 1.0 - xC - yC;
end;

function tTIN.ImageCoordsInTriangle(x,y : float64; var xim,yim : integer) : boolean;
begin
    Result := false;
    FigureDeterminant;

    if (abs(determinant) > 0.00000001) or ExtrapolateOK then begin
      UseDeterminant(x,y);
      if (xc > 0) and (yc > 0) and (zc > 0) then begin
         xim := round(xc * XIM1  + yc * XIM2 + zc * XIM3);
         yim := round(xc * YIM1  + yc * YIM2 + zc * YIM3);
         Result := true;
      end;
   end;
end;

function tTIN.ElevInTriangle(x,y : float64; var z : float64) : boolean;
var
  determinant,xc,yc,zc : float64;
begin
   Result := false;
   z := MaxSmallInt;
   with TinTable do begin
        RealWorldCoordinates;
        FigureDeterminant;
        if (abs(determinant) > 0.000000001) then begin
          UseDeterminant(x,y);
          if (xc > 0) and (yc > 0) and (zc > 0) then begin
             z := xc * GetFieldByNameAsFloat('Z1')  + yc * GetFieldByNameAsFloat('Z2') + zc * GetFieldByNameAsFloat('Z3');
             Result := true;
          end;
       end;
   end;
end;


function tTIN.XYInTriangle(x,y : float64; var xreal1,yreal1 : float64) : boolean;
var
  a,b,c,d,determinant,rhs1,rhs2,xc,yc,zc : float64;
begin
    Result := false;
    RealImageCoordinates;
    a := xim1 - xim3;
    b := xim2 - xim3;
    c := yim1 - yim3;
    d := yim2 - yim3;
    determinant := a*d - b*c;
    {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Det: ' +  RealToString(determinant,-20,-8)); {$EndIf}

    if ExtrapolateOK or (abs(determinant) > 0.00000001) then begin
      RHS1 := X - xim3;
      RHS2 := Y - yim3;

      xC := (RHS1 * d - RHS2 * b) / determinant;
      yC := (RHS2 * a - RHS1 * c) / determinant;
      zC := 1.0 - xC - yC;
      if ExtrapolateOK or ((xc > 0) and (yc > 0) and (zc > 0)) then begin
         RealWorldCoordinates;
         xreal1 := xc * x1  + yc * x2 + zc * x3;
         yreal1 := xc * y1  + yc * y2 + zc * y3;
         Result := true;
      end;
   end;
end;


function tTIN.InterpolateZ(x,y : float64) : float64;
label
   Done;
var
   z: float64;
begin
   Result := MaxSmallInt;
   if not TinTable.FieldExists('Z1') then exit;

   {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Unfiltered: ' + IntToStr(Tintable.RecordCount)); {$EndIf}

   Tintable.ApplyFilter( 'X_LOW < ' + RealToString(x+extra,-18,-8) + ' AND X_HI > ' + RealToString(x-extra,-18,-8) + ' AND ' +
                      'Y_LOW < ' + RealToString(y+extra,-18,-8) + ' AND Y_HI > ' + RealToString(y-extra,-18,-8));
   {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Filter: ' + Tintable.Filter +   'recs=' + IntToStr(Tintable.RecordCount)); {$EndIf}

   while not TinTable.EOF do begin
      if ElevInTriangle(x,y,z) then begin
         Result := z;
         goto Done;
      end;
      TinTable.Next;
   end;

  Done:;
   Tintable.ApplyFilter( '');
end;


function tTIN.InterpolateXY(x,y : float64; var xreal1,yreal1 : float64) : boolean;
label
   Done;
var
   xi,yi : integer;
begin
   Result := false;
   {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Unfiltered: ' + IntToStr(Tintable.RecordCount)); {$EndIf}
   xi := round(x);
   yi := round(Y);

   Tintable.ApplyFilter( 'XIM_LOW <= ' + IntToStr(xi) + ' AND XIM_HI >= ' + IntToStr(xi) + ' AND ' + 'YIM_LOW <= ' + IntToStr(yi) + ' AND YIM_HI >= ' + IntToStr(yi));
   {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Filter: ' + Tintable.Filter +   'recs=' + IntToStr(Tintable.RecordCount)); {$EndIf}

   while not TinTable.EOF do begin
      if XYInTriangle(xi,yi,xreal1,yreal1) then begin
         Result := true;
         goto Done;
      end;
      TinTable.Next;
   end;
  Done:;
   Tintable.ApplyFilter( '');
end;


procedure tTIN.RealImageCoordinates;
begin
  with TinTable do begin
     xim1 := GetFieldByNameAsInteger('XIM1');
     xim2 := GetFieldByNameAsInteger('XIM2');
     xim3 := GetFieldByNameAsInteger('XIM3');
     yim1 := GetFieldByNameAsInteger('YIM1');
     yim2 := GetFieldByNameAsInteger('YIM2');
     yim3 := GetFieldByNameAsInteger('YIM3');
  end;
end;

procedure tTIN.RealWorldCoordinates;
begin
  with TinTable do begin
     x1 := GetFieldByNameAsFloat('X1');
     x2 := GetFieldByNameAsFloat('X2');
     x3 := GetFieldByNameAsFloat('X3');
     y1 := GetFieldByNameAsFloat('Y1');
     y2 := GetFieldByNameAsFloat('Y2');
     y3 := GetFieldByNameAsFloat('Y3');
  end;
end;

procedure tTIN.RealWorldElevations;
begin
    z1 := TinTable.GetFieldByNameAsFloat('Z1');
    z2 := TinTable.GetFieldByNameAsFloat('Z2');
    z3 := TinTable.GetFieldByNameAsFloat('Z3');
end;

{$IfDef TriangulationInterpolation}

      procedure tTIN.InterpolateImage(Map: tMapForm; SatImageNum : integer; SatForm : TSatelliteForm = Nil; Progress : ShortString = '');
      var
         Bitmap,Bitmap2,MaskBMP : tMyBitmap;
         j,x,y,xim,yim : integer;
         Lat,Long : float64;
         NewMap : tMapForm;
         pMask : PRGB;
         p1,p0 :  tScreenPRGB;
         polypts :   array[0..4] of Windows.tPoint;
      begin
         {$IfDef RecordInterpolateImage} WriteLineToDebugFile('tTIN.InterpolateImage in'); {$EndIf}

         if (SatForm <> Nil) then begin
            PetImage.CopyImageToBitmap(SatForm.Image1,Bitmap2);
            {$IfDef RecordInterpolateImage} WriteLineToDebugFile('use satellite image'); {$EndIf}
         end
         else if (SatImage[SatImageNum].SelectionMap <> Nil) then begin
            PetImage.CopyImageToBitmap(SatImage[SatImageNum].SelectionMap.Image1,Bitmap2);
            {$IfDef RecordInterpolateImage} WriteLineToDebugFile('use selection map'); {$EndIf}
         end
         else begin
            exit;
         end;
         PetImage.CloneImageToBitmap(Map.Image1,Bitmap);
         FillScanlineAddresses(Bitmap,P0);
         {$IfDef RecordInterpolateImage}
            WriteLineToDebugFile('Create : ' +  BitmapSizeString(Bitmap));
            WriteLineToDebugFile('Source : ' +  BitmapSizeString(Bitmap2));
         {$EndIf}

         StartProgress('Image ' + Progress);
         FillScanlineAddresses(Bitmap2,P1);

         j := 0;
         TinTable.First;
         while not TinTable.EOF do begin
            RealWorldCoordinates;
            CloneImageToBitmap(Map.Image1,MaskBMP);
            MaskBMP.Canvas.Brush.Style := bsSolid;
            MaskBMP.Canvas.Brush.Color := clBlack;
            Map.MapDraw.LatLongDegreeToScreen(y1,x1,polypts[0].x,polypts[0].y);
            Map.MapDraw.LatLongDegreeToScreen(y2,x2,polypts[1].x,polypts[1].y);
            Map.MapDraw.LatLongDegreeToScreen(y3,x3,polypts[2].x,polypts[2].y);
            MaskBMP.Canvas.Polygon(slice(PolyPts,3));
            inc(j);
            UpdateProgressBar(j/TinTable.RecordCount);
            RealWorldCoordinates;
            RealImageCoordinates;
            for y := 0 to pred(MaskBMP.Height) do begin
               pMask := MaskBMP.ScanLine[y];
               for x := 0 to pred(MaskBMP.Width) do begin
                  Map.MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
                  if SameColor(pMask[x],RGBTripleBlack) then begin
                     if ImageCoordsInTriangle(Long,Lat,xim,yim) then begin
                        if (SatForm <> Nil) then begin
                           SatForm.SatGridToScreen(xim,yim,xim,yim);
                           if SatForm.OnScreen(xim,yim) then p0[y]^[x] := p1[yim]^[xim];
                        end
                        else begin
                           SatImage[SatImageNum].SelectionMap.MapDraw.DataGridToScreen(xim,yim,xim,yim);
                           if SatImage[SatImageNum].SelectionMap.MapDraw.OnScreen(xim,yim) then p0[y]^[x] := p1[yim]^[xim];
                        end;
                     end;
                  end;
               end;
            end;
            MaskBMP.Free;
            TinTable.Next;
         end;
         EndProgress;

         if (SatForm = Nil) then begin
            NewMap := Nil;
            NewMap := Map.DuplicateMap(false);
            NewMap.Image1.Picture.Graphic := Bitmap;
            NewMap.Caption := 'Reprojected ' + SatImage[SatImageNum].SceneBaseName;
         end
         else begin
            Map.Image1.Picture.Graphic := Bitmap;
         end;
         Bitmap.Free;
         Bitmap2.Free;
         EndProgress;
      end;


      procedure tTIN.PickImageTriangle(SatForm : TSatelliteForm;xim,yim : integer);
      var
        a,b,c,d,determinant,rhs1,rhs2,xc,yc,zc : float64;
      begin
        TinTable.First;
         with TinTable,SatForm do begin
            while not TinTable.eof do begin
               RealImageCoordinates;
               a := xim1 - xim3;
               b := xim2 - xim3;
               c := yim1 - yim3;
               d := yim2 - yim3;
               determinant := a*d - b*c;
               if (abs(determinant) > 0.00000001) then begin
                  RHS1 := Xim - xim3;
                  RHS2 := Yim - yim3;
                  xC := (RHS1 * d - RHS2 * b) / determinant;
                  yC := (RHS2 * a - RHS1 * c) / determinant;
                  zC := 1.0 - xC - yC;
                  if (xc > 0) and (yc > 0) and (zc > 0) then begin
                     Image1.Canvas.Pen.Color := clLime;
                     Image1.Canvas.Pen.Width := 3;
                     Image1.Canvas.MoveTo(XSatToScreen(xim1),XSatToScreen(yim1));
                     Image1.Canvas.LineTo(XSatToScreen(xim2),XSatToScreen(yim2));
                     Image1.Canvas.LineTo(XSatToScreen(xim3),XSatToScreen(yim3));
                     Image1.Canvas.LineTo(XSatToScreen(xim1),XSatToScreen(yim1));
                     exit;
                  end;
               end;
               TinTable.Next;
            end;
         end {with};
      end;

      procedure tTIN.ShowTriangulationOnSatForm(Map: TSatelliteForm);
      begin
         if (Map.SatInWindow <> 0)and (TinTable <> Nil) then with SatImage[Map.SatInWindow],Map do begin
            Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.DelaunayLineColor);
            Image1.Canvas.Pen.Width := MDDef.DelaunayLineThick;
            TinTable.First;
            while (not TinTable.EOF) do with TINTable do begin
               RealImageCoordinates;
               Image1.Canvas.MoveTo(XSatToScreen(xim1),XSatToScreen(yim1));
               Image1.Canvas.LineTo(XSatToScreen(xim2),XSatToScreen(yim2));
               Image1.Canvas.LineTo(XSatToScreen(xim3),XSatToScreen(yim3));
               Image1.Canvas.LineTo(XSatToScreen(xim1),XSatToScreen(yim1));
               TinTable.Next;
            end;
         end;
      end;

{$EndIf}


procedure tTIN.UpdateBaseMap;
begin
   if (BaseMap <> Nil) then begin
      {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.UpdateBaseMap'); {$EndIf}
      BaseMap.DoFastMapRedraw;
      if MDDef.ShowDelauneyTriangles then ShowTrianglesOnMap;
      DrawTriangulatedContoursOnMap;
   end;
end;


procedure tTIN.InterpolateImageCoords(x,y : float64; var xim,yim : integer);
label
   Done;
begin
   xim := MaxSmallInt;
   yim := MaxSmallInt;
   {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Unfiltered: ' + IntToStr(Tintable.RecordCount)); {$EndIf}

   Tintable.ApplyFilter( 'X_LOW < ' + RealToString(x+extra,-18,-8) + ' AND X_HI > ' + RealToString(x-extra,-18,-8) + ' AND ' + 'Y_LOW < ' + RealToString(y+extra,-18,-8) + ' AND Y_HI > ' + RealToString(y-extra,-18,-8));
   {$IfDef RecordTINRoamProblems} WriteLineToDebugFile('Filter: ' + Tintable.Filter+ '  Filtered: ' + IntToStr(Tintable.RecordCount)); {$EndIf}

   while not TinTable.EOF do begin
      RealWorldCoordinates;
      RealImageCoordinates;
      if ImageCoordsInTriangle(x,y,xim,yim) then goto Done;
      TinTable.Next;
   end;
  Done:;
end;


procedure tTIN.DrawTriangulatedContoursOnMap;
label
   NoLineDrawn2;
type
   ContType  = array[1..MaxContours] of float64;
const
   PointTol = 0.000001;
var
   JC,k1,L1,L2,i,j,k,l,Tris,NC,rc,
   NumContourLines,xs,ys,xs2,ys2  : integer;
   xn1,yn1,xn2,yn2,
   Top,Bot,ABit,CZ : float64;
   Cont   : ^ContType;
begin
   {$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.DrawTriangulatedContoursOnMap in'); {$EndIf}
   New(Cont);
   StartProgress('Draw TIN contours');
   TinTable.First;
   Tris := 0;
   TinTable.ProgressVars(rc,nc);
   while Not TinTable.EOF do begin
       inc(Tris);
       if (Tris mod rc=0) then UpdateProgressBar(Tris/nc);
       RealWorldCoordinates;
       RealWorldElevations;
       if SmallEnoughTriangle(x1,x2,x3,y1,y2,y3) then begin
         {find contour intersections}
         if (z1 = z2) or (z1 = z3) or (z2 = z3) then Abit := 1e-10
         else ABit := 0;
         Top := z1;
         Bot := z1;
         if z2 > Top then Top := z2
         else if z2 < Bot then Bot := z2;
         if z3 > Top then Top := z3
         else if z3 < Bot then Bot := z3;

         if Bot > 0 then Cont^[1] := (trunc(Bot) / TINContourInterval) * TINContourInterval
         else Cont^[1] := (pred(trunc(Bot)) / TINContourInterval) * TINContourInterval;
         NumContourLines := 1;
         repeat
            inc(NumContourLines);
            Cont^[NumContourLines] :=  Cont^[1] + pred(NumContourLines) * TINContourInterval;
         until Cont^[NumContourLines] > Top;
         {$IfDef RecordDetailedTINProblems}
            WriteLineToDebugFile('NumContourLines=' + IntToStr(NumContourLines));
            WriteLineToDebugFile('Range: ' + RealToString(Bot,-12,-2) + ' to ' + RealToString(Top,-12,-2));
            for JC := 1 to NumContourLines do WriteLineToDebugFile( RealToString(Cont^[JC],12,2));
         {$EndIf}

         for JC := 1 to NumContourLines do begin
            if (Cont^[JC] > Bot) and (Cont^[JC] < Top) then begin
               CZ := (Cont^[JC] - z1) / (z2- z1 + ABit);
               if (CZ <= 0) or (CZ >= 1) then begin
                  CZ := (Cont^[JC] - z1) / (z3 - z1 + ABit);
                  if (CZ < 0) or (CZ > 1) then Goto NoLineDrawn2;
                  Xn1 := (x1 + (x3 - x1) * CZ);
                  Yn1 := (y1 + (y3 - y1) * CZ);
                  CZ := (Cont^[JC] - z2) / (z3 - z2 + ABit);
                  if (CZ < 0) or (CZ > 1) then Goto NoLineDrawn2;
                  Xn2 := (x2 + (x3 - x2) * CZ);
                  Yn2 := (y2 + (y3 - y2) * CZ);
               end
               else begin
                  Xn1 := (x1 + (x2 - x1) * CZ);
                  Yn1 := (y1 + (y2 - y1) * CZ);
                  CZ := (Cont^[JC] - z1) / (z3 - z1 + ABit);
                  if (CZ < 0) or (CZ > 1) then begin
                     CZ := (Cont^[JC]-z2)/(z3 - z2 + ABit);
                     if (CZ < 0) or (CZ > 1) then Goto NoLineDrawn2;
                     Xn2 := (x2 + (x3 - x2) * CZ);
                     Yn2 := (y2 + (y3 - y2) * CZ);
                  end
                  else begin
                     Xn2 := (x1 + (x3 - x1) * CZ);
                     Yn2 := (y1 + (y3 - y1) * CZ);
                  end;
               end;
               with BaseMap.Image1.Canvas do begin
                  Pen.Color := ConvertPlatformColorToTColor(MDDef.ContourLineColor);
                  Pen.Width := MDDef.ContourLineWidth;
                  BaseMap.MapDraw.LatLongDegreeToScreen(yn1,xn1,xs,ys);
                  BaseMap.MapDraw.LatLongDegreeToScreen(yn2,xn2,xs2,ys2);
                  MoveTo(xs,ys);
                  LineTo(xs2,ys2);
               end;
            end {if};
            NoLineDrawn2:;
         end {for JC};
       end;
       TinTable.Next;
   end;
   Dispose(Cont);
   EndProgress;
{$IfDef RecordTINProblems} WriteLineToDebugFile('tTIN.DrawTriangulatedContoursOnMap out'); {$EndIf}
end;


initialization
finalization
end.


