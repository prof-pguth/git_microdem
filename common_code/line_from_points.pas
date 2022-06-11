unit line_from_points;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordMakeLineArea}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,DB,
  Petmar_Types,Petmar_db,DEMdataBase;

type
  TlinePointForm = class(TForm)
    Next: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
     FormGISDB : TGISdataBaseModule;
     MinRecsNeeded,
     CurRec : integer;
     BaseBitmap : tMyBitmap;
  end;

procedure MakeLinesFromPoints(GISDataBase : TGISdataBaseModule; fName : PathStr = ''; ShapeTypeWanted : integer = -99; Thin : integer = -1);


implementation


{$R *.dfm}

uses
   Petmar,DEMMapf,DataBaseCreate, BaseMap,
   PetDBUtils,
   DEMESRIShapefile, Make_tables;


procedure MakeLinesFromPoints(GISDataBase : TGISdataBaseModule; fName : PathStr = ''; ShapeTypeWanted : integer = -99; Thin : integer = -1);
var
   i, fLength,fPrec : integer;
   aft : tFieldType;
   NewTable : tMyData;
   ThreeD,MultipleLines,TimeField,LineEndPoints : boolean;
   Field3D,MultipleField,TimeFName : ShortString;
   MultipleValues : tStringList;
   ShapeFileCreator : tShapeFileCreation;
   LastLat,LastLong : float64;

   procedure DoPoint;
   var
      Lat,Long : float64;
   begin
      with GISDataBase do begin
           if GISDataBase.ValidLatLongFromTable(Lat,Long) then begin
              if (abs(LastLat - Lat) > 0.00000001) or (abs(LastLong - Long) > 0.00000001) then begin
                  if ThreeD then begin
                     if (MyData.GetFieldByNameAsString(Field3D) <> '') then
                        ShapeFileCreator.AddPointWithZToShapeStream(Lat,Long,MyData.GetFieldByNameAsFloat(Field3D));
                  end
                  else ShapeFileCreator.AddPointToShapeStream(Lat,Long);
                  LastLat := Lat;
                  LastLong := Long;
              end;
           end;
      end;
   end;

     procedure DoALine(LineName : string35; RecNum : integer);
     var
        i : integer;
        Lat1,Lat2,Long1,Long2 : float64;
        Time1,Time2 : shortstring;
      begin
         with GISDataBase do begin
            ShowHourglassCursor;
            MyData.First;
            GISDataBase.ValidLatLongFromTable(Lat1,Long1);
            if (TimeField) then Time1 := GISDataBase.MyData.GetFieldByNameAsString(TimeFName);
            EmpSource.Enabled := false;
            while not MyData.eof do begin
               DoPoint;                 //this will have the first point
               for i := 1 to Thin do MyData.Next;
            end;
            if (Thin > 1) then DoPoint;   //insure the last point is included
            GISDataBase.ValidLatLongFromTable(Lat2,Long2);
            if (TimeField) then Time2 := GISDataBase.MyData.GetFieldByNameAsString(TimeFName);
            if ShapeFileCreator.PtsInShapeStream > 0 then begin
               NewTable.Insert;
               NewTable.SetFieldByNameAsInteger(RecNoFName,RecNum);
               NewTable.SetFieldByNameAsString('NAME',LineName);
               if TimeField then begin
                  NewTable.SetFieldByNameAsString('TIME_START',Time1);
                  NewTable.SetFieldByNameAsString('TIME_END',Time2);
               end;
               if LineEndPoints then begin
                  NewTable.SetFieldByNameAsFloat('LAT_START',Lat1);
                  NewTable.SetFieldByNameAsFloat('LONG_START',Long1);
                  NewTable.SetFieldByNameAsFloat('LAT_END',Lat2);
                  NewTable.SetFieldByNameAsFloat('LONG_END',Long2);
               end;
               NewTable.Post;
               ShapeFileCreator.ProcessShapeFileRecord;
            end;
         end;
      end;


begin
   LastLat := 99;
   LastLong := 999;
   if (fName = '') then GetFileNameDefaultExt('New shape file',DBNameMask,FName,false);

   if (fName <> '') then with GISDataBase do begin
      {$IfDef RecordMakeLineArea} WriteLineToDebugFile('MakeLinesFromPoints new file: ' + fName); {$EndIf}
      if (ShapeTypeWanted < 0) then begin
         if AnswerIsYes('Area shape file') then ShapeTypeWanted := 5 else ShapeTypeWanted := 3;
      end;

      ThreeD := AnswerIsYes('3D shapefile');
      if ThreeD then begin
         Field3D := PickField('Three D values' ,[ftInteger,ftFloat]);
         ShapeTypeWanted := ShapeTypeWanted + 10;
      end;

      MultipleLines := AnswerIsYes('Multiple records based on filter');
      if MultipleLines then begin
         MultipleField := PickField('Multiple lines',[ftInteger,ftString]);
      end;

      if (Thin < 0) then begin
         Thin := 1;
         ReadDefault('Thinning factor',Thin);
      end;

      TimeField := AnswerIsYes('Start/end times');
      if TimeField then begin
         TimeFName := PickField('Multiple lines' ,[ftInteger,ftString,ftFloat]);
      end;

      LineEndPoints := (ShapeTypeWanted in [3,13]) and AnswerIsYes('Line end points');

      Make_tables.DefineAndCreateANewTable(FName,false,false,false,true,true);
      NewTable := tMyData.Create(fName);
      if TimeField then begin
         aft := MyData.GetFieldType(TimeFName);
         fLength := MyData.GetFieldLength(TimeFName);
         fPrec := MyData.GetFieldPrecision(TimeFName);
         NewTable.InsureFieldPresentAndAdded(aft,'TIME_START',fLength,fPrec);
         NewTable.InsureFieldPresentAndAdded(aft,'TIME_END',fLength,fPrec);
      end;
      if LineEndPoints then begin
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LAT_START',11,7);
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LONG_START',12,7);
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LAT_END',11,7);
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LONG_END',12,7);
      end;

      System.Delete(Fname,Length(FName)-3,4);
      MapScreenX1 := -9999;
      MapScreenY1 := -9999;
      {$IfDef RecordMakeLineArea} WriteLineToDebugFile('Calling SetUpShapeFiles'); {$EndIf}
      ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,false,ShapeTypeWanted);
      ShowHourglassCursor;
      with GISDataBase do begin
         if MultipleLines then begin
           DBFieldUniqueEntries(MultipleField,MultipleValues);
           for I := 0 to pred(MultipleValues.Count) do begin
              EmpSource.Enabled := false;
              MyData.ApplyFilter(MultipleField + '=' + QuotedStr(MultipleValues.Strings[i]));
              DoALine(MultipleValues.Strings[i],succ(i));
           end;
           MultipleValues.Free;
           ClearGISFilter;
         end
         else DoALine(DBName,1);
         EmpSource.Enabled := true;
      end;
      {$IfDef RecordMakeLineArea} WriteLineToDebugFile('Closeing ShapeFile'); {$EndIf}
      ShapeFileCreator.CloseShapeFiles;
      GISDataBase.TheMapOwner.OpenDBonMap('',fName);
   end;
end;


initialization
finalization
   {$IfDef RecordMakeLineArea}  WriteLineToDebugFile('RecordMakeLineArea active in line_from_points'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing line_from_points out'); {$EndIf}
end.
