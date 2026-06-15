# git_microdem
 
Last changes posted 15 June 2026

This is about 400,000 lines of Delphi code, going back to the first iteration of Turbo Pascal.  While I use Delphi 13.1, it probably does not use any options that will not work in the previous few Delphi versions. It can be compiled with the free community edition of Delphi (https://www.embarcadero.com/products/delphi/starter/free-download).  Following the code, and finding routines called within any module, is best done in the Delphi IDE.

Starting to look at the code:

--The project file is "microdem_project\microdem.dpr"

--The main program form is "microdem_only_code\nevadia_main.pas" 

--The DEMIX processing pipeline is "microdem_only_code\nevadia_main_batch.inc".  The helpfile directory has an HTML file with the seqeuential steps of the processing.  It is from the MICRODEM CHM help file (https://microdem.org/microdem_downloads/microdem.chm) which would be required for the links with the full details.

There are directories with compiled executables for Windows 64
--compiled_exe_installation which has versions that were used for publications; the file name includes the date.  Because I do not guarantee that future improvements will be 100% backward compatible, these versions will be maintained only in compiled form.

If you want to work with the code, I suggest you contact me. The code was not written with the intention of getting to where it is now, and I am working to refactor it to make the variables names more consistent, and to eliminate duplications of code.  Unless I know someone is working on parts of the code, I might make major "improvements" that could make your life difficult.

I am in the process of reorganizing the code, and moving files into small subdirectories.  Old files may still be present; use the Delphi project file to find all the source code files.  It is only easy to update a full directory with less than 100 files, and currently at least one directory is much too large.

The help file is too large to post here.  If you need it, it is online at https://microdem.org  Use its index, table of contents, and full text search.
