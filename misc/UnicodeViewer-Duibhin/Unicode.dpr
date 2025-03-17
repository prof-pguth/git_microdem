// Copyright (C) 2000, Earl F. Glynn, efg's Computer Lab, Overland Park, KS.
// All Rights Reserved Worldwide.
// May be used freely for non-commercial use.
// Reproduction for profit requires permission.

program Unicode;

uses
  Forms,
  ScreenUnicode in 'ScreenUnicode.pas' {FormUnicode},
  UnicodeLibrary in 'UnicodeLibrary.pas',
  ScreenSave in 'ScreenSave.pas' {FormSave};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormUnicode, FormUnicode);
  Application.CreateForm(TFormSave, FormSave);
  Application.Run;
end.
