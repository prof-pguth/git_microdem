unit compress_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/26/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordCompressionProblems}
{$EndIf}


interface

uses
  Vcl.ComCtrls,System.Threading,System.SyncObjs,
  SysUtils, Windows, Messages, Classes, Graphics, Controls,StrUtils,
  Forms, Dialogs, Menus, Grids, ExtCtrls, db,
  StdCtrls,
  DEMdefs,Petmar_types, PETMAR;

type
  Tpetcompressform = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    Help1: TMenuItem;
    Memo1: TMemo;
    Uncompressgzfile1: TMenuItem;
    ExpandTarFile1: TMenuItem;
    UncompressZIPfile1: TMenuItem;
    Uncompress1: TMenuItem;
    StatusBar1: TStatusBar;
    ZIPcompress1: TMenuItem;
    Zipcompress2: TMenuItem;
    Mutliplesinglefilearchives1: TMenuItem;
    Zipcompress3: TMenuItem;
    Allzipsindirsubdirs1: TMenuItem;
    LASLAZ1: TMenuItem;
    CompressLAS1: TMenuItem;
    UncompressLAZ1: TMenuItem;
    CompressLASretain1: TMenuItem;
    CompressLASpurge1: TMenuItem;
    UncompressLAZretain1: TMenuItem;
    UncompressLAZPurge1: TMenuItem;
    BZIP21: TMenuItem;
    Unixtargzfile1: TMenuItem;
    N1: TMenuItem;
    CompressLAStozLAS1: TMenuItem;
    UncompresszLAStoLAS1: TMenuItem;
    FilesinallsubdirectoriespurgeLAZ1: TMenuItem;
    IFfromunixtar1: TMenuItem;
    HGTfromzip1: TMenuItem;
    IFfromzip1: TMenuItem;
    LandsatGeotiffsfromunixtar1: TMenuItem;
    Verylargezipusing7Zip1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    procedure Mutliplesinglefilearchives1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExpandTarFile1Click(Sender: TObject);
    procedure UncompressZIPfile1Click(Sender: TObject);
    procedure ZIPcompress1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Uncompressgzfile1Click(Sender: TObject);
    procedure Allzipsindirsubdirs1Click(Sender: TObject);
    procedure CompressLASretain1Click(Sender: TObject);
    procedure CompressLASpurge1Click(Sender: TObject);
    procedure UncompressLAZretain1Click(Sender: TObject);
    procedure UncompressLAZPurge1Click(Sender: TObject);
    procedure BZIP21Click(Sender: TObject);
    procedure Unixtargzfile1Click(Sender: TObject);
    procedure CompressLAStozLAS1Click(Sender: TObject);
    procedure UncompresszLAStoLAS1Click(Sender: TObject);
    procedure FilesinallsubdirectoriespurgeLAZ1Click(Sender: TObject);
    procedure IFfromunixtar1Click(Sender: TObject);
    procedure HGTfromzip1Click(Sender: TObject);
    procedure IFfromzip1Click(Sender: TObject);
    procedure LandsatGeotiffsfromunixtar1Click(Sender: TObject);
    procedure Verylargezipusing7Zip1Click(Sender: TObject);
    //procedure Sentinel2overlylongfilename1Click(Sender: TObject);
  private
    procedure ExtractUnixFiles(ExtMode: integer);
    { Private declarations }
  public
    { Public declarations }
  end;

procedure StartCompression(aMessage : shortstring = '');

function CheckIfCompressedFile(fName : PathStr) : boolean;

implementation

{$R *.DFM}

uses
   DEM_indexes,
   DEMDef_routines,
   Make_Tables,

{Main program MDI window for different programs that use this module}
   Nevadia_Main;
{End of the MDI parent declaration}


const
   AFC = 'All files completed';


function CheckIfCompressedFile(fName : PathStr) : boolean;
var
   Ext : extstr;
begin
   Ext := UpperCase(ExtractFileExt(fName));
   Result := (Ext = '.TGZ') or (Ext = '.ZIP') or (Ext = '.TAR') or (Ext = '.GZ') or (Ext = '.LAZ');
   if Result then  StartCompression('Decompress before opening: ' + fName);
end;

procedure StartCompression(aMessage : shortstring = '');
var
   petcompressform : Tpetcompressform;
begin
    petcompressform  := Tpetcompressform.Create(Application);
    petcompressform.Memo1.Lines.Add(aMessage);
    petcompressform.ShowModal;
end;


procedure Tpetcompressform.FormCreate(Sender: TObject);
begin
   Uncompressgzfile1.Enabled := SevenZipPresent;
   ExpandTarFile1.Enabled := SevenZipPresent;
   BZIP21.Enabled := SevenZipPresent;
   Unixtargzfile1.Enabled := SevenZipPresent;
   Petmar.PlaceFormAtMousePosition(Self);
   FileMode := 2;
end;


procedure Tpetcompressform.Allzipsindirsubdirs1Click(Sender: TObject);
begin
   UncompressZIPfile1Click(Sender);
end;

procedure Tpetcompressform.BZIP21Click(Sender: TObject);
var
   RawName : PathStr;
   TheFiles : tStringList;
   i : integer;
   DefaultFilter : byte;
   DeleteOriginals : boolean;
begin
   TheFiles := tStringList.Create;
   TheFiles.Add(ProgramRootDir);
   DefaultFilter := 1;
   if GetMultipleFiles('to BZIP2','*.BZ2',TheFiles,DefaultFilter) then begin
      DeleteOriginals := RecycleCompressFile;  //AnswerIsYes('Recycle original files');
      StartProgress('BZ2');
      for i := 0 to pred(TheFiles.Count) do begin
         UpdateProgressBar(i/TheFiles.Count);
         RawName := TheFiles.Strings[i];
         MainBZ2(RawName);
         if DeleteOriginals then File2Trash(RawName);
      end;
      EndProgress;
   end;
   TheFiles.Free;
end;


procedure Tpetcompressform.Close1Click(Sender: TObject);
begin
   Close;
end;


procedure Tpetcompressform.Help1Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme83sn.htm');
end;


procedure Tpetcompressform.HGTfromzip1Click(Sender: TObject);
var
   inDir,OutDir : PathStr;
   DefaultFilter : byte;
   FilesWanted,Paths : tStringList;
   i : integer;
   bfile : tStringList;
begin
   if SevenZipPresent then begin
      DefaultFilter := 1;
      FilesWanted := tStringList.Create;
      if GetMultipleFiles('Extract HGT files','zip files|*.zip',FilesWanted,DefaultFilter) then begin
         GetDOSPath('HGT files',OutDir);
         bFile := tStringList.Create;
         for i := 0 to pred(FilesWanted.Count) do begin
            bFile.Add('rem  ' + IntToStr(i) + '/' + IntToStr(FilesWanted.Count));
            bfile.Add(SevenZipfName + ' e ' + FilesWanted.Strings[i] + ' -r *.hgt -o' + OutDir);
         end;
         EndBatchFile(MDTempDir + 'hgt_zip_extract.bat',bfile);
      end;
   end
   else Missing7ZipMessage;
end;



procedure Tpetcompressform.IFfromunixtar1Click(Sender: TObject);
var
   inDir,OutDir,SubDir,fName : PathStr;
   DefaultFilter : byte;
   FilesWanted,Paths : tStringList;
   i,j,k : integer;
   bfile : tStringList;
begin
   if SevenZipPresent then begin
      Paths := tStringList.Create;
      Paths.Add(MainMapData);
      if GetMultipleDirectories('Extract TIFF files from Tar',Paths) then begin
         GetDOSPath('tiff files',OutDir);
         bFile := tStringList.Create;
         K := 0;
         for j := 0 to pred(Paths.Count) do begin
            FilesWanted := Nil;
            SubDir := OutDir + LastSubDir(Paths.Strings[j]) + '\';
            SafeMakeDir(SubDir);
            FindMatchingFiles(Paths.Strings[j],'*.tar',FilesWanted,5);
            for i := 0 to pred(FilesWanted.Count) do begin
               inc(k);
               bFile.Add('rem  ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count) + '   ' + IntToStr(k) + '  ' + LastSubDir(Paths.Strings[j]) + '  ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
               bfile.Add(SevenZipfName + ' e ' + FilesWanted.Strings[i] + ' -r *_DEM.tif -o' + SubDir);
            end;
         end;
         EndBatchFile(MDTempDir + 'tiff_tar_extract.bat',bfile);
      end;
   end
   else Missing7ZipMessage;
end;



procedure Tpetcompressform.IFfromzip1Click(Sender: TObject);
var
   inDir,OutDir : PathStr;
   DefaultFilter : byte;
   FilesWanted,Paths : tStringList;
   i : integer;
   bfile : tStringList;
begin
   if SevenZipPresent then begin
      DefaultFilter := 1;
      FilesWanted := tStringList.Create;
      if GetMultipleFiles('Extract TIF files','tif files|*.tif',FilesWanted,DefaultFilter) then begin
         GetDOSPath('extractted TIF files',OutDir);
         bFile := tStringList.Create;
         for i := 0 to pred(FilesWanted.Count) do begin
            bFile.Add('rem  ' + IntToStr(i) + '/' + IntToStr(FilesWanted.Count));
            bfile.Add(SevenZipfName + ' e ' + FilesWanted.Strings[i] + ' -r *.tif -o' + OutDir);
         end;
         EndBatchFile(MDTempDir + 'tif_zip_extract.bat',bfile);
      end;
   end
   else Missing7ZipMessage;
end;

procedure Tpetcompressform.LandsatGeotiffsfromunixtar1Click(Sender: TObject);
begin
   ExtractUnixFiles(4);
end;

procedure Tpetcompressform.FilesinallsubdirectoriespurgeLAZ1Click(Sender: TObject);
begin
   CompressLASretain1Click(Sender);
end;

procedure Tpetcompressform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   FileMode := 2;
   ApplicationProcessMessages;
end;


procedure Tpetcompressform.ExpandTarFile1Click(Sender: TObject);
begin
   ExtractUnixFiles(1);
end;

procedure Tpetcompressform.Uncompressgzfile1Click(Sender: TObject);
begin
   ExtractUnixFiles(2);
end;


procedure Tpetcompressform.ExtractUnixFiles(ExtMode : integer);
{$IfDef ExUnixFiles}
begin
{$Else}
var
   fName,OutPath : PathStr;
   DefaultFilter : byte;
   i : integer;
   Ext : ExtStr;
   DeleteTheFile : boolean;
   WhatFor,TheFilters : ANSIString;
   FilesWanted : tStringList;
begin
   {$IfDef RecordCompressionProblems} WriteLineToDebugFile('TDemHandForm.Uncompressgzfile1Click, mode=' + IntToStr(ExtMode)); {$EndIf}
   OutPath := '';
   Memo1.Visible := true;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(LastCompressedFile);
   DefaultFilter := ExtMode;
   TheFilters := 'TAR files|*.tar|GZIP files|*.gz;*.tgz|tar.gz files|*.tar.gz;*.tgz;*.tar;*.gz';
   if GetMultipleFiles(WhatFor,TheFilters,FilesWanted,DefaultFilter) then begin
      if (FilesWanted.Count > 0) then begin
         DeleteTheFile := RecycleCompressFile;  //AnswerIsYes('Recycle input files');
         StartProgressAbortOption('Unix decompression');
         for i := 0 to pred(FilesWanted.Count) do begin
            UpdateProgressBar(succ(i)/FilesWanted.Count);
            fName := FilesWanted.Strings[i];
            LastCompressedFile := fName;
            Memo1.Lines.Add(WhatFor + '=' + fName);
            ShowHourglassCursor;
            OutPath := ExtractFilePath(fName);

            if ExtMode = 4 then begin
               OutPath := copy(fname,1,length(fName)-3);
               SafeMakeDir(OutPath);
            end
            else if StrUtils.AnsiContainsText(fName,'.tar.gz') or StrUtils.AnsiContainsText(fName,'.tgz') then ExtMode := 3
            else begin
               Ext := UpperCase(ExtractFileExt(fName));
               if ExtEquals(Ext,'.gz') then ExtMode := 2
               else ExtMode := 1;
            end;

            if (ExtMode in [1,3]) then begin
               MainGzip(fName);
               if DeleteTheFile then File2Trash(fName);
            end;

            if (ExtMode in [3]) then begin
               Delete(fName,length(fName)-2,3);
               OutPath := copy(fname,1,length(fName)-3);
               SafeMakeDir(OutPath);
            end;

            if (ExtMode in [2,3,4]) then begin
               MainExtar(fName, OutPath);
               if DeleteTheFile then File2Trash(fName);
            end;
            if DeleteTheFile then Memo1.Lines.Add('  done; original file recycled')
            else Memo1.Lines.Add('  done; original file unchanged');
            if WantOut then break;
         end;
         EndProgress;
         ShowDefaultCursor;
         Memo1.Lines.Add(AFC);
      end;
   end;
   FilesWanted.Free;
{$EndIf}
end;


procedure Tpetcompressform.UncompressLAZPurge1Click(Sender: TObject);
begin
   CompressLASretain1Click(Sender);
end;


procedure Tpetcompressform.UncompressLAZretain1Click(Sender: TObject);
begin
   CompressLASretain1Click(Sender);
end;


var
   NumDone,NumToDo : integer;


procedure UnzipListWithZips(var FilesWanted : tstringList; Memo1 : tMemo = Nil; DeleteThem : boolean = false);
var
   i : integer;
   fName : PathStr;
begin
   wmDEM.SetPanelText(3,'Started ' + TimeToStr(now));
   for I := 0 to pred(FilesWanted.Count) do begin
      fName := FilesWanted.Strings[i];
      if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(now) + ' ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count) + ' ' + ExtractFileName(fName))
      else wmDEM.SetPanelText(2,IntToStr(i) + '/' + IntToStr(FilesWanted.Count));
      UnzipSingleFile(fName);
     {$IfDef VCL}
     TInterlocked.Increment(NumDone);
     UpDateProgressBar(NumDone /FilesWanted.Count);
     {$EndIf}
   end;

   if DeleteThem then begin
      {$IfDef RecordCompressionProblems} WriteLinetoDebugFile('Recylce zips'); {$EndIf}
      for i := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         File2Trash(fName);
      end;
      Memo1.Lines.Add('Zips moved to recycle bin');
      {$IfDef RecordCompressionProblems} WriteLinetoDebugFile('Recyle OK'); {$EndIf}
   end;
   Memo1.Lines.Add(AFC);
   FilesWanted.Free;
   wmDEM.SetPanelText(2,'');
   wmDEM.SetPanelText(3,'');
end;


procedure Tpetcompressform.UncompressZIPfile1Click(Sender: TObject);
{$IfDef ExUnixFiles}
begin
{$Else}
var
   FilesWanted : tStringList;
   fName : PathStr;
   i : integer;
   DefaultFilter : byte;

      procedure UnzipList;
      var
         i  : integer;
         DeleteThem : boolean;
         ThreadStringList : array[1..MaxThreadsAllowed] of tStringList;
      begin
         {$IfDef RecordCompressionProblems}
            WriteLineToDebugFile('Unzipping:');
            WriteStringListToDebugFile(FilesWanted);
         {$EndIf}
         DeleteThem := RecycleCompressFile;  //AnswerIsYes('Recylce ZIP files');
         UnzipListWithZips(FilesWanted,Memo1,DeleteThem);
      end;

begin
   {$IfDef RecordCompressionProblems} WriteLinetoDebugFile('Tpetcompressform.UncompressZIPfile1Click in');  {$EndIf}
   Memo1.Visible := true;
   if (Sender = Allzipsindirsubdirs1) then  begin
      {$IfDef RecordCompressionProblems} WriteLineToDebugFile('(Sender = Allzipsindirsubdirs1)'); {$EndIf}
      fName := ExtractFilePath(LastCompressedFile);
      GetDOSPath('ZIP files',fName);
      FilesWanted := Nil;
      FindMatchingFiles(fName,'*.zip',FilesWanted,5);
   end
   else begin
      DefaultFilter := 1;
      FilesWanted := tStringList.Create;
      FilesWanted.Add(LastCompressedFile);
      GetMultipleFiles('Compressed files|*.zip;*.piz;*.kmx;*.shz;*.7z|ZIP file','ZIP file|*.ZIP|KMZ file|*.KMZ|SHZ file|*.shz|7Zip file|*.7z',FilesWanted,DefaultFilter);
   end;
   if (FilesWanted.Count > 0) then begin
      Memo1.Lines.Add('Unzipping '+ FilesWanted.Count.ToString + ' files from ' + ExtractFilepath(FilesWanted.Strings[0]));
      LastCompressedFile := FilesWanted.Strings[0];
      //ChDir(ExtractFilePath(LastCompressedFile));
      UnzipList;
   end;
   //FilesWanted.Free;
   //Memo1.Lines.Add(AFC);
   {$IfDef RecordCompressionProblems}   WriteLinetoDebugFile('Tpetcompressform.UncompressZIPfile1Click out');      {$EndIf}
{$EndIf}
end;


procedure Tpetcompressform.UncompresszLAStoLAS1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   FilesWanted : tStringList;
   fName,LazName,oPath : PathStr;
   i : integer;
   DefaultFilter : byte;
   Decompress : shortString;
   cmd : ANSIstring;
   Filter : shortstring;
   NewExt : extStr;
begin
   LazName := ProgramRootDir + 'EzLAS\EzLAS.exe';
   if FileExists(LazName) then begin
       if (Sender = UncompresszLAStoLAS1) then begin
          Filter := 'zLAS|*.zLAS';
          Decompress := ' /D';
          NewExt := '.las';
          NewExt := '';
       end
       else begin
          Filter := 'LAS|*.las';
          Decompress := '';
          NewExt := '.zlas';
       end;

       FilesWanted := tStringList.Create;
       FilesWanted.Add(LastLidarDirectory);
       DefaultFilter := 1;

       if GetMultipleFiles('LAS files',Filter,FilesWanted,DefaultFilter) then begin
          ShowHourglassCursor;
          LastLidarDirectory := ExtractFilePath(FilesWanted.Strings[0]);
          ChDir(LastLidarDirectory);
          i := 0;
          StartProgress('zLAS');
          for i := 0 to pred(FilesWanted.Count) do begin
             UpdateProgressBar(i/FilesWanted.Count);
             fName := FilesWanted.Strings[i];
             oPath := ExtractFilePath(fName);
             Delete(oPath,length(oPath),1);
             if (Sender = UncompresszLAStoLAS1) then begin
                cmd := LazName + ' ' + fName + ' ' + oPath + Decompress;
             end
             else begin
                cmd := LazName + ' ' + fName + ' ' + oPath + Decompress;
             end;
             WinExecAndWait32(cmd);
          end;
          EndProgress;
       end;
   end
   else begin
      MessageToContinue(LazName + ' missing');
   end;
{$EndIf}
end;


procedure Tpetcompressform.Unixtargzfile1Click(Sender: TObject);
begin
   ExtractUnixFiles(3);
end;


procedure Tpetcompressform.Verylargezipusing7Zip1Click(Sender: TObject);
var
   TheFiles : tStringList;
   DefaultFilter : byte;
   fName : PathStr;
   i : integer;
begin
   if SevenZipPresent then begin
      TheFiles := tStringList.Create;
      TheFiles.Add(ProgramRootDir);
      DefaultFilter := 1;
      if GetMultipleFiles('to unZIP','*.ZIP;*.7Z',TheFiles,DefaultFilter) then begin
         for i := 0 to pred(TheFiles.Count) do begin
            fName := theFiles.Strings[i];
            Memo1.Lines.Add(TimeToStr(Now) + '  ' + fName);
            ChDir(ExtractFilePath(fName));
            SafeMakeDir(ExtractFileNameNoExt(fName));
            Main7Z(fName);
         end;
         Memo1.Lines.Add(TimeToStr(Now) + '  unzip over');
         for i := 0 to pred(TheFiles.Count) do File2Trash(theFiles.Strings[i]);
      end;
   end
   else Missing7ZipMessage;
end;

procedure Tpetcompressform.ZIPcompress1Click(Sender: TObject);
{$IfDef ExUnixFiles}
begin
{$Else}
var
   ZipName : PathStr;
   TheFiles : tStringList;
   DefaultFilter : byte;
begin
   TheFiles := tStringList.Create;
   TheFiles.Add(ProgramRootDir);
   DefaultFilter := 1;
   if GetMultipleFiles('to ZIP','*.ZIP',TheFiles,DefaultFilter) then begin
      ZIPName := ExtractFilePath(TheFiles.Strings[0]);
      Petmar.GetFileNameDefaultExt('zip file','zip|*.zip',ZipName);
      ZipMasterZipFiles(ZipName,TheFiles);
   end;
   TheFiles.Free;
{$EndIf}
end;


procedure Tpetcompressform.Mutliplesinglefilearchives1Click(Sender: TObject);
{$IfDef ExUnixFiles}
begin
{$Else}
var
   RawName,ZipName : PathStr;
   TheFiles,ZipFiles : tStringList;
   i : integer;
   DefaultFilter : byte;
   DeleteOriginals : boolean;
begin
   TheFiles := tStringList.Create;
   TheFiles.Add(ProgramRootDir);
   DefaultFilter := 1;
   if GetMultipleFiles('to ZIP','*.ZIP',TheFiles,DefaultFilter) then begin
      DeleteOriginals := RecycleCompressFile;  //AnswerIsYes('Recycle original files');
      StartProgress('Zip');
      for i := 0 to pred(TheFiles.Count) do begin
         UpdateProgressBar(i/TheFiles.Count);
         RawName := TheFiles.Strings[i];
         ZIPName := ChangeFileExt(RawName,'.zip');
         ZipFiles := tStringList.Create;
         ZipFiles.Add(RawName);
         ZipMasterZipFiles(ZipName,ZipFiles);
         ZipFiles.Free;
         if DeleteOriginals then File2Trash(RawName);   //SysUtils.DeleteFile(RawName);
      end;
      EndProgress;
   end;
   TheFiles.Free;
{$EndIf}
end;


procedure Tpetcompressform.CompressLASpurge1Click(Sender: TObject);
begin
   CompressLASretain1Click(Sender);
end;

procedure Tpetcompressform.CompressLASretain1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   FilesWanted,bfile : tStringList;
   fName,LazName,Dir,ResultingFName : PathStr;
   i,j,lt : integer;
   DefaultFilter : byte;
   cmd : ANSIstring;
   Filter : shortstring;
   NewExt : ExtStr;
   DeCompressing,DeleteFiles : boolean;
begin
   {$IfDef RecordCompressionProblems} WriteLinetoDebugFile('Tpetcompressform.CompressLASretain1Click in'); {$EndIf}
   LazName := ProgramRootDir + 'lastools\bin\laszip.exe';
   if FileExists(LazName) then begin
       FilesWanted := tStringList.Create;
       DefaultFilter := 1;
       DeleteFiles := (Sender = CompressLASPurge1) or (Sender = UncompressLAZPurge1) or ((Sender = FilesinallsubdirectoriespurgeLAZ1));
       DeCompressing := (Sender = FilesinallsubdirectoriespurgeLAZ1) or (Sender = UncompressLAZRetain1) or (Sender = UncompressLAZPurge1);
       if (Sender = FilesinallsubdirectoriespurgeLAZ1) then begin
          Dir := LastLidarDirectory;
          GetDOSPath('directory for LAZ files in subdirectories',Dir);
          FindMatchingFiles(Dir,'*.LAZ',FilesWanted,5);
       end
       else begin
          FilesWanted.Add(LastLidarDirectory);
          if (Sender = UncompressLAZRetain1) or (Sender = UncompressLAZPurge1) then Filter := 'LAZ|*.laz'
          else Filter := 'LAS|*.las';
          GetMultipleFiles('LAS files',Filter,FilesWanted,DefaultFilter)
       end;

       if (FilesWanted.Count > 0) then begin
          {$IfDef RecordCompressionProblems} WriteLinetoDebugFile('Tpetcompressform.CompressLASretain1Click Files picked');  {$EndIf}
          if Decompressing then NewExt := '.las' else NewExt := '.laz';
          bFile := tStringList.Create;
          ShowHourglassCursor;
          LastLidarDirectory := ExtractFilePath(FilesWanted.Strings[0]);
          ChDir(LastLidarDirectory);
          Memo1.Lines.Add('Files requested=' + IntToStr(FilesWanted.Count));
          j := 0;
          for i := 0 to pred(FilesWanted.Count) do begin
             fName := FilesWanted.Strings[i];
             ResultingFName := ChangeFileExt(fName,NewExt);
             if FileExists(ResultingFName) then begin
                inc(j);
             end
             else begin
                cmd := LazName + ' ' + fName;
                bfile.Add('REM ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
                bfile.Add(cmd);
             end;
             if DeleteFiles then bFile.Add('del ' + fName);
          end;
          if j > 0 then Memo1.Lines.Add('Files skipped that already existed: ' + IntToStr(j));
          if (bfile.Count = 0) then Bfile.Destroy
          else EndBatchFile(MDtempDir + 'laz_work_' + IntToStr(i) + '.bat',bfile,false);
          {$IfDef RecordCompressionProblems} WriteLinetoDebugFile('Tpetcompressform.CompressLASretain1Click Done work'); {$EndIf}
       end;
       Memo1.Lines.Add('You can work while laszip works in the background');
       FilesWanted.Free;
       ShowDefaultCursor;
   end
   else MessageToContinue(LazName + ' missing');
{$EndIf}
end;


procedure Tpetcompressform.CompressLAStozLAS1Click(Sender: TObject);
begin
   UncompresszLAStoLAS1Click(Sender);
end;

initialization
   {$IfDef MessageStartupProblems} MessageToContinue('start compress_form'); {$EndIf}
finalization
   {$IfDef RecordCompressionProblems} WriteLineToDebugFile('RecordCompressionProblems active in compress_form'); {$EndIf}
end.


