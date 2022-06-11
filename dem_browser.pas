unit dem_browser;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 11/4/2012       }
{_________________________________}


removed 4 May 2020

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, Menus, ComCtrls, OleCtrls, Buttons, ToolWin, ActnList, ImgList, SHDocVw,
  ActiveX,
  Petmar_types;

type
  TBrowserMainForm = class(TForm)
    StatusBar1 : TStatusBar;
    WebBrowser1 : TWebBrowser;
    Memo1: TMemo;
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WebBrowser1DownloadComplete(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    UpdateCombo: Boolean;
  public
      URLs : ANSIString;
      Closable : boolean;
      procedure FindAddress;
      function WebBrowserScreenShot(BitmapWidth,BitmapHeight : integer) : tMyBitmap;
      procedure WB_SaveAs_HTML(const FileName : string);
      procedure SaveAllFrames(PageName : shortstring);
  end;

var
  BrowserMainForm : TBrowserMainForm;


implementation

 uses
    mshtml,PetImage, nevadia_main, ComObj, petmar;


{$R *.dfm}

function GetBrowserForFrame(Doc: IHTMLDocument2; nFrame: Integer): IWebBrowser2;
  //Thanks to Rik Barker
  //returns an interface to the frame's browser
var
  pContainer: IOLEContainer;
  enumerator: ActiveX.IEnumUnknown;
  nFetched: PLongInt;
  unkFrame: IUnknown;
  hr: HRESULT;
begin
  Result := nil;
  nFetched := nil;
  // Cast the page as an OLE container
  pContainer := Doc as IOleContainer;
  // Get an enumerator for the frames on the page
  hr := pContainer.EnumObjects(OLECONTF_EMBEDDINGS or OLECONTF_OTHERS, enumerator);
  if hr <> S_OK then
  begin
    pContainer._Release;
    Exit;
  end;
  // Now skip to the frame we're interested in
  enumerator.Skip(nFrame);
  // and get the frame as IUnknown
  enumerator.Next(1,unkFrame, nFetched);
  // Now QI the frame for a WebBrowser Interface - I'm not  entirely
  // sure this is necessary, but COM never ceases to surprise me
  unkframe.QueryInterface(IID_IWebBrowser2, Result);
end;

function GetFrameSource(WebDoc: iHTMLDocument2): string;
  //returns frame HTML and scripts as a text string
var
  re: integer;
  HTMLel: iHTMLElement;
  HTMLcol: iHTMLElementCollection;
  HTMLlen: Integer;
begin
  Result := '';
  if Assigned(WebDoc) then begin
    HTMLcol := WebDoc.Get_all;
    HTMLlen := HTMLcol.Length;
    for re := 0 to HTMLlen - 1 do begin
      HTMLel := HTMLcol.Item(re, 0) as iHTMLElement;
      if HTMLEl.tagName = 'HTML' then
        Result := Result + HTMLEl.outerHTML;
    end;
  end;
end;

function WB_SaveFrameToFile(HTMLDocument: IHTMLDocument2;
  const FileName: TFileName): Boolean;
// Save IHTMLDocument2 to a file
var
  PersistFile: IPersistFile;
begin
  PersistFile := HTMLDocument as IPersistFile;
  PersistFile.Save(StringToOleStr(FileName), System.True);
end;


function SaveWBFrames(WebBrowser1: TWebBrowser): string;
// return the source for all frames in the browser
var
  Webdoc, HTMLDoc: ihtmldocument2;
  framesCol: iHTMLFramesCollection2;
  FramesLen: integer;
  pickFrame: olevariant;
  p: integer;
begin
  try
    WebDoc := WebBrowser1.Document as IHTMLDocument2;
    Result := GetFrameSource(WebDoc);

    // §§§ Hier kann Result in eine Datei gespeichert werden §§§§  oder  mit
    // WB_SaveFrameToFile(WebDoc,'c:\MainPage.html');

    //Handle multiple or single frames
    FramesCol := WebDoc.Get_frames;
    FramesLen := FramesCol.Get_length;
    if FramesLen > 0 then
      for p := 0 to FramesLen - 1 do begin
        pickframe := p;
        HTMLDoc   := WebBrowser1.Document as iHTMLDocument2;

        WebDoc := GetBrowserForFrame(HTMLDoc, pickframe).document as iHTMLDocument2;
        if WebDoc <> nil then  begin
          Result := GetFrameSource(WebDoc);
          WB_SaveFrameToFile(WebDoc, 'c:\Frame' + IntToStr(p) + '.html');
          // ShowMessage(HTMLDoc.Get_parentWindow.Get_name);
          // ShowMessage(HTMLDoc.Get_parentWindow.Parent.Get_document.nameProp);
         end;
      end;
  except
    Result := 'No Source Available';
  end;
end;



procedure TBrowserMainForm.SaveAllFrames;
var
  IpStream: IPersistStreamInit;
  AStream: TMemoryStream;
  iw: IWebbrowser2;
  i: Integer;
  sl: TStringList;


      function GetFrame(FrameNo: Integer): IWebbrowser2;
      var
        OleContainer: IOleContainer;
        enum: ActiveX.IEnumUnknown;
        unk: IUnknown;
        Fetched: PLongint;
      begin
        while (Webbrowser1.ReadyState <> READYSTATE_COMPLETE) do Petmar.ApplicationProcessMessages;
        if Assigned(Webbrowser1.document) then begin
          Fetched := nil;
          OleContainer := Webbrowser1.Document as IOleContainer;
          OleContainer.EnumObjects(OLECONTF_EMBEDDINGS, Enum);
          Enum.Skip(FrameNo);
          Enum.Next(1, Unk, Fetched);
          Result := Unk as IWebbrowser2;
        end
        else
          Result := nil;
      end;


begin
  for i := 0 to Webbrowser1.OleObject.Document.frames.Length - 1 do begin
    iw := GetFrame(i);
    AStream := TMemoryStream.Create;
    try
      IpStream := iw.document as IPersistStreamInit;
      if Succeeded(IpStream.save(TStreamadapter.Create(AStream), True)) then begin
        AStream.Seek(0, 0);
        sl := TStringList.Create;
        sl.LoadFromStream(AStream);
        sl.SaveToFile('c:\temp\frame' + IntToStr(i) + '_' + PageName + '.htm');
        sl.Free;
      end;
    except
    end;
    AStream.Free;
  end;
end;


 procedure TBrowserMainForm.WB_SaveAs_HTML(const FileName : string);
 //other options
 //   http://www.swissdelphicenter.ch/torry/showcode.php?id=1112
 //   http://www.swissdelphicenter.ch/torry/showcode.php?id=2054
 var
    stl : tStringList;
    document : IHTMLDocument2;
    s : string;
begin
   if not Assigned(WebBrowser1.Document) then begin
     ShowMessage('Document not loaded!');
     Exit;
   end;

   Document := Webbrowser1.Document as IHTMLDocument2;
   s := Document.Body.innerText;
   if s = '' then s := Document.Body.InnerHTML;

   stl := tStringList.Create;
   stl.Add(s);
   Stl.SaveToFile(FileName);
   stl.Free;
end; (* WB_SaveAs_HTML *)



function TBrowserMainForm.WebBrowserScreenShot(BitmapWidth,BitmapHeight : integer)  : tMyBitmap;
//http://delphi.about.com/od/vclusing/a/wb_scren_shot.htm
 var
   viewObject : IViewObject;
   r : TRect;
 begin
   if WebBrowser1.Document <> nil then begin
       if BitmapWidth + 25 > wmdem.ClientWidth then wmdem.ClientWidth := BitmapWidth + 25;
       if BitmapHeight + 25 > wmdem.ClientHeight then wmdem.ClientHeight := BitmapHeight + 125;
       WebBrowser1.Height := BitmapHeight + 50;
       WebBrowser1.Width := BitmapWidth + 50;
       Height:= BitmapHeight + 50;
       Width := BitmapWidth + 75;
       VertScrollBar.Position := 0;
       HorzScrollBar.Position := 0;
       WindowState := wsMaximized;
       ApplicationProcessMessages;

     WebBrowser1.Document.QueryInterface(IViewObject, viewObject) ;
     if Assigned(viewObject) then
     try
       PetImage.CreateBitmap(Result,1,1);
       try
         r := Rect(0, 0, WebBrowser1.Width, WebBrowser1.Height) ;

         Result.Height := WebBrowser1.Height;
         Result.Width := WebBrowser1.Width;

         viewObject.Draw(DVASPECT_CONTENT, 1, nil, nil, Application.Handle, Result.Canvas.Handle, @r, nil, nil, 0) ;
       finally
       end;
     finally
       viewObject._Release;
     end;
   end;
end;


procedure TBrowserMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;


procedure TBrowserMainForm.FindAddress;
var
  Flags : OLEVariant;
begin
  Flags := 0;
  UpdateCombo := True;
  ShowHourglassCursor;
  WebBrowser1.Navigate(WideString(Urls), Flags, Flags, Flags, Flags);
end;


procedure TBrowserMainForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   if Closable then begin
      action := caFree;
      WebBrowser1.Free;
      Self := Nil;
      BrowserMainForm := nil;
   end;
end;


procedure TBrowserMainForm.WebBrowser1DownloadComplete(Sender: TObject);
begin
   ShowDefaultCursor;
end;


procedure TBrowserMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := Closable;
end;


procedure TBrowserMainForm.FormCreate(Sender: TObject);
begin
   Closable := false;
   ClientWidth := 650;
   ClientHeight := 650;
   Left := 5;
   Top := 5;
end;


initialization
   BrowserMainForm := nil;
finalization
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing dembrowser out');
   {$EndIf}
end.
