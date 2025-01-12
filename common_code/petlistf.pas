unit Petlistf;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls;

type
  TPetList = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    ListBox1: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BitBtn1: TBitBtn;
    procedure ListBox1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     Cancel : boolean;
  end;


implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types;


procedure TPetList.ListBox1Click(Sender: TObject);
begin
   if (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then Panel3.Caption := ListBox1.Items[ListBox1.ItemIndex];
end;

procedure TPetList.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\select_from_list.htm');
end;

procedure TPetList.ListBox1DblClick(Sender: TObject);
begin
   if (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then begin
      Panel3.Caption := ListBox1.Items[ListBox1.ItemIndex];
      Cancel := false;
      Close;
   end;
end;

procedure TPetList.BitBtn1Click(Sender: TObject);
var
   fName : PathStr;
begin
   if GetExistingFileName('File with entries','*.txt',fName) then begin
      ListBox1.Items.Clear;
      ListBox1.Items.LoadFromFile(fName);
   end;
end;

procedure TPetList.CancelBtnClick(Sender: TObject);
begin
   Cancel := true;
end;

procedure TPetList.OKBtnClick(Sender: TObject);
begin
   Cancel := false;
end;


procedure TPetList.FormCreate(Sender: TObject);
begin
   Cancel := true;
   Width:= 400;
   Petmar.PlaceFormAtMousePosition(Self);
end;

initialization
finalization
end.
