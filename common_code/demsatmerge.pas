unit demsatmerge;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Graphics,Windows, SysUtils, Classes,  Forms, Controls, StdCtrls,Buttons, ExtCtrls, //OkCancl1,
   Petmar_types,PETMAR,DEMRefOp,PETMath, OKCANCL1;

type
  TIHSMergeForm = class(TOKBottomDlg)
    HelpBtn: TButton;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar2Change(Sender: TObject);
    procedure ScrollBar3Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ColorBar;
  public
    { Public declarations }
    OnlyHue : boolean;
    BaseBitmap : tMyBitmap;
  end;


function GetIHSparameters(var I,H,S : SmallInt; HueSatOnly : boolean = false; BackBMP : tMyBitmap = Nil) : boolean;


implementation

{$R *.DFM}


uses
   PetImage;

function GetIHSparameters(var I,H,S : SmallInt; HueSatOnly : boolean = false; BackBMP : tMyBitmap = Nil) : boolean;
var
  IHSMergeForm: TIHSMergeForm;
begin
   IHSMergeForm := TIHSMergeForm.Create(Application);
   with IHSMergeForm do begin
      BaseBitmap := BackBMP;
      if (BaseBitmap <> Nil) then begin
         Image1.Height := BaseBitmap.Height;
         Image1.Width := BaseBitmap.Width;
         height := 340;
         OKBtn.Top := 280;
         HelpBtn.Top := 280;
         CancelBtn.Top := 280;
         Bevel1.Height := 257;
      end;
      OnlyHue := HueSatOnly;
      ScrollBar1.Position := i;
      ScrollBar2.Position := h;
      ScrollBar3.Position := s;
      Label3.Caption := IntToStr(i);
      Label4.Caption := IntToStr(h);
      Label5.Caption := IntToStr(s);
      ColorBar;
      if HueSatOnly then begin
         ScrollBar1.Enabled := false;
         CheckBox1.Enabled := false;
         Button1.Enabled := false;
      end;
      if (IHSMergeForm.ShowModal <> idCancel) then begin
         i := ScrollBar1.Position;
         h := ScrollBar2.Position;
         s := ScrollBar3.Position;
         Result := true;
      end
      else Result := false;
      Close;
   end;
end;


procedure TIHSMergeForm.ColorBar;
var
   Bitmap   : tMyBitmap;
   i,h,s,x,y  : integer;
   Hue      : float64;
   P0,PB    : pRGB;
begin
   Label3.Caption := IntegerToString(ScrollBar1.Position,3);
   Label4.Caption := IntegerToString(ScrollBar2.Position,3);
   Label5.Caption := IntegerToString(ScrollBar3.Position,3);
   PetImage.CreateBitmap(Bitmap,255,25);
   i := ScrollBar1.Position;
   h := ScrollBar2.Position;
   s := ScrollBar3.Position;
   if (BaseBitmap = Nil) then begin
      with Bitmap.Canvas do for x := 0 to 255 do begin
         if OnlyHue then Hue := h
         else Hue := (360.0 - (H/255) * (x / 255 * 360.0));
         Pen.Color := ConvertPlatformColorToTColor(RGBtripFromHSI(Hue,s,i));
         MoveTo(x,0);
         LineTo(x,24);
      end;
   end
   else begin
      Bitmap.Assign(BaseBitmap);
      Hue := h;
      for y := 12 to pred(BaseBitmap.Height)-12 do begin
         p0 := Bitmap.ScanLine[y];
         pB := BaseBitmap.ScanLine[y];
         for x := 24 to pred(BaseBitmap.Width)-24 do begin
           I := PB[x].rgbtRed;
           p0[x] := RGBtripFromHSI(Hue,S,I);
         end;
      end;
   end;
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TIHSMergeForm.ScrollBar1Change(Sender: TObject);
begin
   ColorBar;
end;

procedure TIHSMergeForm.ScrollBar2Change(Sender: TObject);
begin
   ColorBar;
end;

procedure TIHSMergeForm.ScrollBar3Change(Sender: TObject);
begin
   ColorBar;
end;


procedure TIHSMergeForm.Button1Click(Sender: TObject);
begin
   ChangeReflectanceOptions(nil);
end;


procedure TIHSMergeForm.CheckBox1Click(Sender: TObject);
begin
   Button1.Enabled := CheckBox1.Checked;;
end;


procedure TIHSMergeForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TIHSMergeForm.Button2Click(Sender: TObject);
begin
   ScrollBar1.Position := 210;
   ScrollBar2.Position := 210;
   ScrollBar3.Position := 125;
   ColorBar;
end;


procedure TIHSMergeForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;

initialization
finalization
end.

