unit mask_multiple;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms,
  Controls,  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TMask_mult_form = class(TForm)
    Memo1: TMemo;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DEM : integer;
  end;


procedure MultMaskDEM(TheDEM : integer);



implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,DEMCoord,DEMDefs;


procedure MultMaskDEM(TheDEM : integer);
var
  Mask_mult_form : TMask_mult_form;
begin
   Mask_mult_form := TMask_mult_form.Create(Application);
   Mask_mult_form.DEM := TheDEM;
   Mask_mult_form.ShowModal;
end;



procedure TMask_mult_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\edit_mark_missing.htm');
end;

procedure TMask_mult_form.OKBtnClick(Sender: TObject);
var
   values : set of byte;
   i,x,y,zi : integer;
   z : float32;
   b : byte;
begin
   if Memo1.Lines.Count = 0 then MessageToContinue('No values selected')
   else if Memo1.Lines.Count > 255 then MessageToContinue('Only 255 values allowed at a time')
   else begin
      Values := [];
      for I := 0 to pred(Memo1.Lines.Count) do begin
         b := StrToInt(Memo1.Lines[i]);
         Values := Values + [b];
      end;

       with DEMGlb[DEM] do begin
          ShowHourglassCursor;
          if (DEMheader.NumCol > 1500) then StartProgress('Mark missing ' + AreaName);
          for x := 0 to pred(DEMheader.NumCol) do begin
             if (x mod 500 = 0) and (DEMheader.NumCol > 1500) then UpdateProgressBar(x/DEMheader.NumCol);
             for y := 0 to pred(DEMheader.NumRow) do begin
                if GetElevMeters(x,y,z) then begin
                   zi := round(z);
                   if ((RadioGroup1.ItemIndex = 0) and (zi in Values)) or ((RadioGroup1.ItemIndex = 1) and (not (zi in Values))) then
                        SetGridMissing(x,y);
                end;
             end;
          end;
          if (DEMheader.NumCol > 1500) then EndProgress;
          CheckMaxMinElev;
          SelectionMap.MapDraw.MinMapElev := DEMheader.MinElev;
          SelectionMap.MapDraw.MaxMapElev := DEMheader.MaxElev;
          SelectionMap.DoBaseMapRedraw;
       end;
   end;
   Close;
end;

initialization
finalization
end.
