unit petfont;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 5/21/2013       }
{_________________________________}

interface

uses
   Windows,Classes,Graphics,Forms,Controls,Buttons,
   StdCtrls,ExtCtrls,ColorGrd,Dialogs,SysUtils;

type
  TFontDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit3: TEdit;
    Label7: TLabel;
    Edit4: TEdit;
    FontDialog1: TFontDialog;
    BitBtn1: TBitBtn;
    Edit2: TEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    procedure Edit3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure Redraw;
  public
    { Public declarations }
     XPic,YPic,
     FontRotationAngle : integer;
     OwningCanvas      : tCanvas;
     Running           : boolean;
  end;


var
  FontDlg : TFontDlg;

implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types,PETImage,PetImage_form;


procedure TFontDlg.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,XPic);
   Redraw;
end;

procedure TFontDlg.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,FontRotationAngle);
   Redraw;
end;


procedure TFontDlg.BitBtn1Click(Sender: TObject);
begin
   if FontDialog1.Execute then begin
      OwningCanvas.Font := FontDialog1.Font;
      Redraw;
   end;
end;

procedure TFontDlg.FormCreate(Sender: TObject);
begin
   FontRotationAngle := 0;
   Edit1.Text := '0';
   CancelBtn.Enabled := false;
   Running := false;
end;


procedure TFontDlg.Redraw;
var
   Angle : integer;
begin
   if Running then begin
      Angle := FontRotationAngle;
      while Angle < 0 do inc(Angle,360);
      while Angle > 360 do dec(Angle,360);
      //LoadedImageForm.Image1.Picture.LoadFromFile(MDTempDir + 'BACKUP.BMP');
      OwningCanvas.Font := FontDialog1.Font;
      with OwningCanvas do begin
         if CheckBox1.Checked then Brush.Style := bsClear
         else Brush.Style := bsSolid;
      end;
      PETMAR.CanvasTextOutAngle(OwningCanvas,XPic,YPic,10*Angle,Edit2.Text);
      CancelBtn.Enabled := true;
      Edit3.Text := IntToStr(Xpic);
      Edit4.Text := IntToStr(Ypic);
   end;
end;

procedure TFontDlg.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,YPic);
   Redraw;
end;


procedure TFontDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   if Running then CanClose := AnswerIsYes('Verify text correct')
   else Running := true;
end;


procedure TFontDlg.Edit2Change(Sender: TObject);
begin
   Redraw;
end;

procedure TFontDlg.CheckBox1Click(Sender: TObject);
begin
   Redraw;
end;


procedure TFontDlg.CancelBtnClick(Sender: TObject);
begin
   Edit2.Text := '';
   Redraw;
   Running := false;
   Close;
end;


initialization
finalization
end.
