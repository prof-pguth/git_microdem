unit grays_game;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Image1: TImage;
    Image2: TImage;
    RadioGroup1: TRadioGroup;
    Label3: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit2: TEdit;
    Edit1: TEdit;
    TabSheet2: TTabSheet;
    Image3: TImage;
    Image4: TImage;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    BitBtn3: TBitBtn;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    TrackBar3: TTrackBar;
    BitBtn4: TBitBtn;
    Label7: TLabel;
    CheckBox1: TCheckBox;
    Darker: TBitBtn;
    BitBtn5: TBitBtn;
    Edit3: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure DarkerClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Rand,SliderIncrement : integer;
    Red,Green,Blue,Gray,
    Tint,Difference : integer;
    ColorGuesses,
    NumCorrect,NumWrong : integer;
    ColorError : double;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
   Bitmap : tBitmap;
   Color : tColor;
begin
   RadioGroup1.ItemIndex := -1;
   Bitmap := tBitmap.Create;
   Bitmap.PixelFormat := pf24bit;
   Bitmap.Height := 150;
   Bitmap.Width := 150;

   Color := rgb(Gray,Gray,Gray);
   Bitmap.Canvas.Pen.Color := Color;
   Bitmap.Canvas.Brush.Color := Color;
   Bitmap.Canvas.Rectangle(0,0,150,150);
   Image1.Picture.Graphic := Bitmap;
   Rand := (-1 + Random(3)) * difference;

   Color := rgb(Gray+Rand,Gray+Rand,Gray+Rand);
   Bitmap.Canvas.Pen.Color := Color;
   Bitmap.Canvas.Brush.Color := Color;
   Bitmap.Canvas.Rectangle(0,0,150,150);
   Image2.Picture.Graphic := Bitmap;
   Bitmap.Free;
   Label1.Caption := 'Score: ' + IntToStr(NumCorrect) + '/' +  IntToStr(NumCorrect + NumWrong);
end;


procedure TForm1.RadioGroup1Click(Sender: TObject);
var
   Correct : boolean;
begin
   case RadioGroup1.ItemIndex of
      0 : Correct := Rand < 0;
      1 : Correct := Rand = 0;
      2 : Correct := Rand > 0;
   end;
   if Correct then inc(NumCorrect) else inc(NumWrong);
   BitBtn1Click(nil);
end;


procedure TForm1.TrackBar1Change(Sender: TObject);
var
   Bitmap : tBitmap;
   Color : tColor;
begin
   Bitmap := tBitmap.Create;
   Bitmap.PixelFormat := pf24bit;
   Bitmap.Height := 200;
   Bitmap.Width := 200;

   Color := rgb(TrackBar1.Position,TrackBar2.Position,TrackBar3.Position);
   Label4.Caption := IntToStr(TrackBar1.Position);
   Label5.Caption := IntToStr(TrackBar2.Position);
   Label6.Caption := IntToStr(TrackBar3.Position);


   Bitmap.Canvas.Pen.Color := Color;
   Bitmap.Canvas.Brush.Color := Color;
   Bitmap.Canvas.Rectangle(0,0,200,200);
   Image3.Picture.Graphic := Bitmap;
   Bitmap.Free;

end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
   TrackBar1Change(Sender);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
   TrackBar1Change(Sender);
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
var
   Bitmap : tBitmap;
   Color : tColor;
begin
   Bitmap := tBitmap.Create;
   Bitmap.PixelFormat := pf24bit;
   Bitmap.Height := 200;
   Bitmap.Width := 200;

   Randomize;
   Red := Random(255);
   Green := Random(255);
   Blue := Random(255);

   Color := rgb(Red,Green,Blue);
   Bitmap.Canvas.Pen.Color := Color;
   Bitmap.Canvas.Brush.Color := Color;
   Bitmap.Canvas.Rectangle(0,0,200,200);
   Image4.Picture.Graphic := Bitmap;
   Bitmap.Free;
   if (Sender = BitBtn3) or (Sender = CheckBox1) then begin
      ColorGuesses := 0;
      ColorError := 0;
      Label7.Caption := '';
   end;
end;



procedure TForm1.BitBtn4Click(Sender: TObject);
var
   rg,bg,gg : integer;
   Mess : string;
   Dist : double;
begin
   rg := TrackBar1.Position;
   bg := TrackBar3.Position;
   gg := TrackBar2.Position;
   Dist := sqrt(sqr(rg-Red) + sqr(bg-Blue) + sqr(gg-Green));
   Str(Dist:8:2,Mess);
   Mess := 'R=' + IntToStr(Red) + '  G=' + IntToStr(Green) +   '  B=' + IntToStr(Blue) + #13 + 
   'Red error: ' + IntToStr(rg-Red) + #13 + 'Blue error: ' + IntToStr(bg-Blue) + #13 +
       'Green error: ' + IntToStr(gg-Green) + #13#13 + 'Overall: ' + Mess;

   MessageDlg(Mess,mtInformation,[mbOK],0);
   inc(ColorGuesses);
   ColorError := ColorError + Dist;
   Str(ColorError/ColorGuesses:8:2,Mess);
   Label7.Caption := 'Average score: ' + Mess + '  (n=' + IntToStr(ColorGuesses) +')';
   if not CheckBox1.Checked then BitBtn3Click(Sender);
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
   if TrackBar1.Position < 255 then TrackBar1.Position := TrackBar1.Position + SliderIncrement;
   if TrackBar2.Position < 255 then TrackBar2.Position := TrackBar2.Position + SliderIncrement;
   if TrackBar3.Position < 255 then TrackBar3.Position := TrackBar3.Position + SliderIncrement;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
   if not CheckBox1.Checked then BitBtn3Click(Sender);
end;

procedure TForm1.DarkerClick(Sender: TObject);
begin
   if TrackBar1.Position > 0 then TrackBar1.Position := TrackBar1.Position - SliderIncrement;
   if TrackBar2.Position > 0 then TrackBar2.Position := TrackBar2.Position - SliderIncrement;
   if TrackBar3.Position > 0 then TrackBar3.Position := TrackBar3.Position - SliderIncrement;
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
   err : integer;
begin
   Val(Edit1.Text,Difference,err);
   NumWrong := 0;
   NumCorrect := 0;
   BitBtn1Click(nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   Difference := 5;
   BitBtn1Click(nil);
   BitBtn3Click(nil);
   Image1.Stretch := true;             //Required for Delphi 6 "feature"
   Image2.Stretch := true;             //Required for Delphi 6 "feature"
   Image3.Stretch := true;             //Required for Delphi 6 "feature"
   Image4.Stretch := true;             //Required for Delphi 6 "feature"
   SliderIncrement := 5;
   TrackBar1Change(Sender);
end;



procedure TForm1.Label6Click(Sender: TObject);
begin
   TrackBar1Change(Sender);
end;

procedure TForm1.PageControl1Enter(Sender: TObject);
begin
   Gray := 150;
   Difference := 5;
   BitBtn1Click(nil);
end;

procedure TForm1.Edit2Change(Sender: TObject);
var
   err : integer;
begin
   Val(Edit2.Text,Gray,err);
   NumWrong := 0;
   NumCorrect := 0;
   BitBtn1Click(nil);
end;




procedure TForm1.Edit3Change(Sender: TObject);
begin
   SliderIncrement := StrToInt(Edit3.Text);
end;

end.
