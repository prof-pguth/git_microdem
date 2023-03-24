unit simple_python;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$Define RecordPython}




//{$IfDef IncludePython}

  interface

  uses
    Classes, SysUtils,
    Windows, Messages, Graphics, Controls, Forms, Dialogs,
    StdCtrls, ComCtrls, ExtCtrls,
    PythonEngine, Vcl.PythonGUIInputOutput;

  type
    TPythonForm1 = class(TForm)
      Memo1: TMemo;
      Panel1: TPanel;
      Button1: TButton;
      Splitter1: TSplitter;
      Button2: TButton;
      Button3: TButton;
      OpenDialog1: TOpenDialog;
      SaveDialog1: TSaveDialog;
      Memo2: TMemo;
      procedure Button1Click(Sender: TObject);
      procedure Button2Click(Sender: TObject);
      procedure Button3Click(Sender: TObject);
      procedure FormCreate(Sender: TObject);
    private
      { Déclarations privées }
    public
      { Déclarations publiques }
    end;

  procedure StartPython;

  var
    PythonForm1 : TPythonForm1;

  implementation

  {$R *.DFM}

  uses
     Petmar_types;


  procedure StartPython;
  begin
     {$IfDef RecordPython} WriteLineToDebugFile('StartPython in');  {$EndIf}
     PythonForm1 := TPythonForm1.Create(application);
     PythonForm1.Show;
     {$IfDef RecordPython} WriteLineToDebugFile('StartPython out');  {$EndIf}
  end;

  procedure TPythonForm1.Button1Click(Sender: TObject);
  begin
    PythonEngine1.ExecStrings( Memo1.Lines );
  end;

  procedure TPythonForm1.Button2Click(Sender: TObject);
  begin
    with OpenDialog1 do  begin
        if Execute then
          Memo1.Lines.LoadFromFile( FileName );
      end;
  end;

  procedure TPythonForm1.Button3Click(Sender: TObject);
  begin
    with SaveDialog1 do begin
        if Execute then
          Memo1.Lines.SaveToFile( FileName );
      end;
  end;


  (*
  procedure TPythonVersion.AssignTo(PythonEngine: TPersistent);
  begin
    if PythonEngine is TPythonEngine then begin
      TPythonEngine(PythonEngine).UseLastKnownVersion := False;
      TPythonEngine(PythonEngine).RegVersion := SysVersion;
      TPythonEngine(PythonEngine).DllName := DLLName;
      TPythonEngine(PythonEngine).DllPath := DLLPath;
      TPythonEngine(PythonEngine).APIVersion := ApiVersion;
      if Is_venv then begin
        TPythonEngine(PythonEngine).VenvPythonExe := PythonExecutable;
        TPythonEngine(PythonEngine).SetPythonHome(DLLPath);
      end else if not IsRegistered or Is_conda then
        {
           Not sure why but PythonHome needs to be set even for
           registered conda distributions
           Note also that for conda distributions to work properly,
           you need to add Format('%s;%0:s\Library\bin;', [Version.InstallPath]
           to your Windows path if it is not there already.
        }
        TPythonEngine(PythonEngine).SetPythonHome(InstallPath);
    end;
  end;
  *)


  procedure TPythonForm1.FormCreate(Sender: TObject);
  //var
     //PythonVersion: TPythonVersion;
  begin
     try
        try
         {$IfDef RecordPython} WriteLineToDebugFile('TPythonForm1.FormCreate up and away');  {$EndIf}
         MaskFPUExceptions(True);
         exit;

         PythonEngine1.DLLPath := 'C:\Users\pguth\anaconda3\';
         {$IfDef RecordPython} WriteLineToDebugFile('TPythonForm1.FormCreate set dll path');  {$EndIf}
         PythonEngine1.DLLName := 'python310.dll';
         PythonEngine1.LoadDLL;
         {$IfDef RecordPython} WriteLineToDebugFile('TPythonForm1.FormCreate dll loaded');  {$EndIf}
        except
           on Exception do {$IfDef RecordPython} WriteLineToDebugFile('TPythonForm1.FormCreate exception');  {$EndIf}
        end;
     finally
        {$IfDef RecordPython} WriteLineToDebugFile('TPythonForm1.FormCreate finally');  {$EndIf}
     end;

      {$IfDef RecordPython} WriteLineToDebugFile('TPythonForm1.FormCreate out');  {$EndIf}

  (*
    var PythonVersion: TPythonVersion;

    if GetRegisteredPythonVersion(SysVersion, PythonVersion) then
    or
    if PythonVersionFromPath(Path, PythonVersion) then
    begin
      PythonVersion.AssignTo(PythonEngine)
      PythonEngine.LoadDLL
    end
    else
  *)

  (*
    if GetRegisteredPythonVersion(SysVersion, PythonVersion) or PythonVersionFromPath(Path, PythonVersion) then begin
      PythonVersion.AssignTo(PythonEngine)
      PythonEngine.LoadDLL
    end
    else
  *)
  end;


(*
{$Else}


interface
implementation


{$EndIf}

*)



end.
