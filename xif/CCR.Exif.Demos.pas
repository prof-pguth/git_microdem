{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.1                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is CCR.Exif.Demos.pas.                                             }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2010 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Modified for MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


unit CCR.Exif.Demos;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Themes;

type
  TForm = class(Forms.TForm)
  strict private const
    CmdLineTimerID = 999;
  strict private
    FFileName: string;
  protected
    SupportOpeningFiles: Boolean;
    procedure DoCreate; override;
    procedure DoFileOpen(const FileName : string); virtual;
  public
    procedure OpenFile(const FileName: string);
    property FileName: string read FFileName;
  end;

procedure CreateNewExeInstance(const FileName: string);
function IsThemingEnabled: Boolean;
procedure SelectFileInExplorer(const FileName: string);

implementation

uses
  ShellApi, CCR.Exif.Consts, CCR.Exif.BaseUtils;


procedure CreateNewExeInstance(const FileName: string);
var
  Params: string;
begin
  if Pos(' ', FileName) > 0 then
    Params := '"' + FileName + '"'
  else
    Params := FileName;
  ShellExecute(0, nil, PChar(Application.ExeName), PChar(Params), PChar(GetCurrentDir), SW_SHOWNORMAL);
end;

function IsThemingEnabled: Boolean;
begin
{$IF Declared(StyleServices)}
  Result := StyleServices.Enabled;
{$ELSE}
  Result := ThemeServices.ThemesEnabled;
{$IFEND}
end;

procedure SelectFileInExplorer(const FileName: string);
begin
  ShellExecute(Screen.ActiveForm.Handle, 'open', 'explorer.exe', PChar('/select,"' + FileName + '"'), nil, SW_SHOWNORMAL)
end;

{$IFDEF BackfillStyleServices}
function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$ENDIF}


type
  TControlAccess = class(TControl);

procedure TForm.DoCreate;

  procedure UpdateFont(Root: TControl);
  var
    I: Integer;
  begin
    if TControlAccess(Root).ParentFont then Exit;
    with TControlAccess(Root).Font do begin
      if Name = 'Tahoma' then Name := 'Segoe UI';
      if Size = 8 then Size := 9;
    end;
    if Root is TWinControl then
      for I := 0 to TWinControl(Root).ControlCount - 1 do
        UpdateFont(TWinControl(Root).Controls[I]);
  end;

begin
  UpdateFont(Self);
  if Application.MainForm = nil then begin
    Caption := Caption + ' v' + CCRExifVersion;
    Application.Title := Caption;
  end;
  inherited; //calls any OnCreate handler
  if SupportOpeningFiles then SetTimer(Handle, CmdLineTimerID, 10, nil);
end;

procedure TForm.DoFileOpen(const FileName{, FileToCompare:} : string);
begin
end;

procedure TForm.OpenFile(const FileName : {, FileToCompare:} string);
var
  SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Caption := ExtractFileName(FileName) + ' - ' + Application.Title;
    FFileName := FileName;
    try
      DoFileOpen(FileName{, FileToCompare});
    except
      Caption := Application.Title;
      raise;
    end;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

initialization
end.
