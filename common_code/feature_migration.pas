unit feature_migration;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
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


  Winapi.Windows, Winapi.Messages, System.SysUtils,  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Petmar_types,DEMMapf, Vcl.Buttons;

type
  TFeatureMigrationForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Edit5: TEdit;
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ID,OriginalGrid,
    ShiftSign,
    MovedGrid : integer;
    BaseMap : tMapForm;
  end;


procedure CalculateFeatureMigration(BaseMap : tMapForm; DEM1 : integer = 0; DEM2 : integer = 0);


implementation

{$R *.dfm}


uses
   Petmar,PetMath,
   BaseGraf,Thread_timers,
   DEM_Manager,
   Nevadia_main,
   DEMDefs,DEMCoord,DEM_indexes,DEMstat;


procedure CalculateFeatureMigration(BaseMap : tMapForm; DEM1 : integer = 0; DEM2 : integer = 0);
var
   FeatureMigrationForm : TFeatureMigrationForm;
begin
   FeatureMigrationForm := TFeatureMigrationForm.Create(Application);
   FeatureMigrationForm.Show;
   FeatureMigrationForm.OriginalGrid := DEM1;
   FeatureMigrationForm.MovedGrid := DEM2;
   FeatureMigrationForm.BaseMap := BaseMap;
   if (DEM1 = 0) then FeatureMigrationForm.BitBtn1Click(Nil);
   if (DEM2 = 0) then FeatureMigrationForm.BitBtn2Click(Nil);
end;


procedure TFeatureMigrationForm.BitBtn1Click(Sender: TObject);
begin
    GetDEM(OriginalGrid,true,'original features');
    if (OriginalGrid <> 0) then Label2.Caption := DEMGlb[OriginalGrid].AreaName;
end;


procedure TFeatureMigrationForm.BitBtn2Click(Sender: TObject);
begin
    GetDEM(MovedGrid,true,'migrated features');
    if (MovedGrid <> 0) then Label3.Caption := DEMGlb[MovedGrid].AreaName;
end;


procedure TFeatureMigrationForm.BitBtn3Click(Sender: TObject);
var
   Table : tMyData;
   NPTFieldName : shortString;
   Results : tStringList;
   N,nMatch,WhereX,WhereY,xs,ys,NFound,
   NAvg,i,j : integer;
   MaxPC,PC,z : float32;
   fName : PathStr;
   TStr : ShortString;
   Neigh : array[1..5] of integer;
   NeighPC : array[1..5] of float64;


    procedure DoOne;
    var
       fLoX,fHiX,fLoY,fHiY,
       x,y,x2,y2,i : integer;
       Hist : array[0..32000] of integer;
    begin
       fLoX := Table.GetFieldByNameAsInteger('BOUND_XMIN');
       fHiX := Table.GetFieldByNameAsInteger('BOUND_XMAX');
       fLoY := Table.GetFieldByNameAsInteger('BOUND_YMIN');
       fHiY := Table.GetFieldByNameAsInteger('BOUND_YMAX');
       MaxPC := 0;
       NAvg := 0;
       if (Sender = BitBtn3) then Memo1.Lines.Add('Feature ' + Edit5.Text);
       for x2 := MDDef.ShiftLoX to MDDef.ShiftHighX do begin
         for y2 := MDDef.ShiftLoY to MDDef.ShiftHighY do begin
             N := 0;
             Nmatch := 0;
             for x := fLoX to fHiX do begin
               for y := fLoY to fHiY do begin
                  if DEMGLB[OriginalGrid].GetElevMeters(x,y,z) and (round(z) = ID) then begin
                     inc(N);
                     if DEMGLB[MovedGrid].GetElevMeters(x+x2,y+y2,z) then inc(NMatch);
                  end;
               end;
             end;
             if (NMatch > 0) then begin
                PC := 100 * NMatch / N;
                if (PC > MaxPC) then begin
                   MaxPC := PC;
                   WhereX := x2;
                   WhereY := y2;
                   NAvg := 1;
                   if (Sender = BitBtn3) then begin
                      Memo1.Clear;
                      Memo1.Lines.Add('Feature ' + Edit5.Text);
                      Memo1.Lines.Add(IntegerToString(x2,4)+ IntegerToString(Y2,6) + RealToString(PC,12,2) + '%');
                   end;
                end
                else if abs(PC-MaxPC) < 0.0001 then begin
                   if (Sender = BitBtn3) then Memo1.Lines.Add(IntegerToString(x2,4)+ IntegerToString(Y2,6) + RealToString(PC,12,2) + '%');
                   WhereX := WhereX + x2;
                   WhereY := WhereY + y2;
                   Inc(NAvg);
                end;
                if (Sender = BitBtn3) then Memo1.Lines.Add(IntegerToString(x2,4)+ IntegerToString(Y2,6) + RealToString(PC,12,2) + '%');
             end;
         end;
       end;
       if (MaxPC > 0) then begin
          if (Sender = BitBtn3) then begin
             Memo1.Lines.Add('');
             Memo1.Lines.Add('Best match for ' + IntegerToString(ID,4) + '  x=' + RealToString(WhereX/NAvg,-12,2) + '  y=' +  RealToString(WhereY/NAvg,-12,2) + RealToString(MaxPC,12,2) + '% of ' + IntToStr(n) + '  navg=' + IntToStr(NAvg));
          end;
          xs := round(WhereX/NAvg);
          ys := round(WhereY/NAvg);
          NMatch := 0;
          N := 0;
          for I := 0 to 32000 do Hist[i] := 0;

           for x := fLoX to fHiX do begin
             for y := fLoY to fHiY do begin
                if DEMGLB[OriginalGrid].GetElevMeters(x,y,z) and (round(z) = ID) then begin
                   inc(N);
                   if DEMGLB[MovedGrid].GetElevMeters(x+xs,y+ys,z) then begin
                      inc(NMatch);
                      inc(Hist[round(z)]);
                   end;
                end;
             end;
           end;

            NFound := 0;
            for I := 0 to 32000 do if (Hist[i] > 0) then begin
               inc(NFound);
               if NFound <= 5  then begin
                  Neigh[NFound] := i;
                  NeighPC[nFound] := 100 * Hist[i] / N;
               end;
               if (Sender = BitBtn3) then Memo1.Lines.Add(IntegerToString(I,4) + RealToString(100 * Hist[i] / N,8,2));
            end;
       end;
    end;

begin
   try
     Table := tMyData.Create(DEMGlb[OriginalGrid].VATFileName);
     if (Sender = BitBtn3) then begin
        Table.ApplyFilter('ID=' + IntToStr(ID));
        DoOne;
     end;

     if (Sender = BitBtn4) or (Sender = BitBtn5) then begin
        NPTFieldName := Table.NCountField;
        Results := tStringList.Create;
        Results.Add('ID,NPTS,LAT,LONG,X_SHIFT,Y_SHIFT,PC,NAVG,NEIGH,NEIGH_1,NEIGH_1PC,NEIGH_2,NEIGH_2PC,NEIGH_3,NEIGH_3PC');
        i := 0;
        StartProgress(Caption + ' ' + IntToStr(ShiftSign));
        Memo1.Lines.Add('Start ' + IntToStr(ShiftSign) + '  ' + TimeToStr(Now));
        while not Table.eof do begin
           inc(i);
           if (I mod 10 = 0) then UpdateProgressBar(i/Table.RecordCount);

           ID := Table.GetFieldByNameAsInteger('ID');
           DoOne;

            if (NAvg > 0) then begin
               TStr := RealToString(ShiftSign * WhereX/NAvg*DEMGlb[OriginalGrid].AverageXSpace,-12,-2) + ',' + RealToString(ShiftSign*WhereY/NAvg*DEMGlb[OriginalGrid].AverageYSpace,-12,-2) + ',' +
                       RealToString(MaxPC,-12,-2) + ',' + IntToStr(NAvg) + ',' + IntToStr(NFound);
               for j := 1 to 3 do TStr := TStr + ',' + IntToStr(Neigh[j]) + ',' + RealToString(NeighPC[j],-8,2);
            end
            else TStr := ' , ,0,0,0, , , , , ,  ';

            Results.Add(IntToStr(ID)  + ',' +  IntToStr(Table.GetFieldByNameAsInteger(NPTFieldName)) + ',' + RealToString(Table.GetFieldByNameAsFloat('LAT'),-12,-8) + ',' +
                           RealToString(Table.GetFieldByNameAsFloat('LONG'),-12,-8) + ',' + TStr);
            Table.Next;
        end;
        Memo1.Lines.Add('Done ' + TimeToStr(Now));
        fName := ExtractFilePath(DEMGlb[OriginalGrid].DEMFileName) + ptTrim(DEMGlb[OriginalGrid].AreaName) + '_to_' + ptTrim(DEMGlb[MovedGrid].AreaName) + '_feature_migration.csv';
        BaseMap.StringListtoLoadedDatabase(Results,fName);
     end;

   finally
      Table.Destroy;
      EndProgress;
   end;
end;



procedure TFeatureMigrationForm.BitBtn4Click(Sender: TObject);
begin
   BitBtn3Click(Sender);
end;


procedure TFeatureMigrationForm.BitBtn5Click(Sender: TObject);

        procedure ShiftValues;
        var
           i : integer;
        begin
           ShiftSign := -ShiftSign;
           i := MDDef.ShiftHighX;
           MDDef.ShiftHighX := -MDDef.ShiftLoX;
           MDDef.ShiftLoX := - i;
           i := MDDef.ShiftHighY;
           MDDef.ShiftHighY := -MDDef.ShiftLoY;
           MDDef.ShiftLoY := - i;
           SwapPair(OriginalGrid,MovedGrid);
        end;

begin
   BitBtn3Click(Sender);
   ShiftValues;
   BitBtn3Click(Sender);
   ShiftValues;
end;

procedure TFeatureMigrationForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.ShiftHighY);
end;

procedure TFeatureMigrationForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.ShiftLoY);
end;

procedure TFeatureMigrationForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.ShiftLoX);
end;

procedure TFeatureMigrationForm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.ShiftHighX);
end;

procedure TFeatureMigrationForm.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,ID);
end;

procedure TFeatureMigrationForm.FormCreate(Sender: TObject);
begin
   wmDEM.FormPlacementInCorner(Self,lpNEMap);
   OriginalGrid := 0;
   Movedgrid := 0;
   ShiftSign := 1;
   Caption := ShortEXEName + ' feature migration';

   Edit3.Text := IntToStr(MDdef.ShiftLoX);
   Edit4.Text := IntToStr(MDdef.ShiftHighX);
   Edit2.Text := IntToStr(MDdef.ShiftLoY);
   Edit1.Text := IntToStr(MDdef.ShiftHighY);
end;



initialization
finalization
end.
