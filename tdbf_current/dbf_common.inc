// define this if you need more SPEEEEEDDDD!!!
// useful if you index dbf files over a network

{.$define USE_CACHE}

// define the following if you want support for 65535 length character
// fields for all dbase files (and not only foxpro); if you define this, 
// you will not be able to read MS Excel generated .dbf files!

{.$define USE_LONG_CHAR_FIELDS}

// modifies unix unit dbf_wtil to use hungarian encodings (hack)

{.$define HUNGARIAN}

// enables assembler routines, 486+ only
{$ifndef Win64}
{$define USE_ASSEMBLER_486_UP}
{$endif}

// test compatibility

{.$define TDBF_UPDATE_FIRSTLAST_NODE}

// use this directive to suppress math exceptions,
// instead NAN is returned.
// Using this directive is slightly less efficient
//
// used in Dbf_PrsDef
//
// NAN support needs to be rewritten and is currently absent

{.$define NAN}

//------------------------------------------------------
//--- Define all SUPPORT_xxx; undef if not supported ---
//------------------------------------------------------


//------------------------------------------------------
//--- Delphi versions                                ---
//------------------------------------------------------


{$ifdef VER80}          //	Delphi 1.0
  #ERROR tDbf needs Delphi or C++ Builder 3 minimum.
{$endif}

{$ifdef VER90}          //	 Delphi 2.0
  #ERROR tDbf needs Delphi or C++ Builder 3 minimum.
{$endif}

{$ifdef VER93}          //	 is BCB++ 1.0
  #ERROR tDbf needs Delphi or C++ Builder 3 minimum.
{$endif}

// DELPHI_X is defined if the compiler is at least that version of Delphi
// e.g. DELPHI_5 means, it's Delphi 5 or newer

{$ifdef VER100}         // Delphi 3
  {$define DELPHI_3}
{$endif}

{$ifdef VER110}      // CBuilder 3
  {$define DELPHI_3}
{$endif}

{$ifdef VER120}      // Delphi 4
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER125} // C++ BUILDER 4
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER130} // Delphi 5
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER135} // C++ Builder 5 ??
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER140} // Delphi 6
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER145} // C++ Builder 6
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER150} // Delphi 7 :-) For once I am not late (12/07/2001)
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER155} // C++ Builder 7
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER160} // Delphi 8
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER170} // Delphi 2005
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER180} // Delphi 2006
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER185} // Delphi 2007 Win32
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER200} // Delphi 2009
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER210} // Delphi 2010
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER220} // Delphi XE
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER230} // Delphi XE2
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER240} // Delphi XE3 (not tested)
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER250} // Delphi XE4 (not tested)
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER260} // Delphi XE5
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER270} // Delphi XE6
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER280} // Delphi XE7
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER290} // Delphi XE8
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER300} // Delphi 10 Seattle
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER310} // Delphi 10.1 Berlin
  {$define DELPHI_101}
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER320} // Delphi 10.2 Tokyo
  {$define DELPHI_102}
  {$define DELPHI_101}
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER330} // Delphi 10.3 Rio
  {$define DELPHI_103}
  {$define DELPHI_102}
  {$define DELPHI_101}
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER340} // Delphi 10.4 Sydney
  {$define DELPHI_104}
  {$define DELPHI_103}
  {$define DELPHI_102}
  {$define DELPHI_101}
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER350} // Delphi 11 Alexandria
  {$define DELPHI_11}
  {$define DELPHI_104}
  {$define DELPHI_103}
  {$define DELPHI_102}
  {$define DELPHI_101}
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER360} // Delphi 12
  {$define DELPHI_12}
  {$define DELPHI_11}
  {$define DELPHI_104}
  {$define DELPHI_103}
  {$define DELPHI_102}
  {$define DELPHI_101}
  {$define DELPHI_10}
  {$define DELPHI_XE8}
  {$define DELPHI_XE7}
  {$define DELPHI_XE6}
  {$define DELPHI_XE5}
  {$define DELPHI_XE4}
  {$define DELPHI_XE3}
  {$define DELPHI_XE2}
  {$define DELPHI_XE}
  {$define DELPHI_2010}
  {$define DELPHI_2009}
  {$define DELPHI_2007}
  {$define DELPHI_2006}
  {$define DELPHI_2005}
  {$define DELPHI_8}
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

//-------------------------------------------------------
//--- Conclude supported features from delphi version ---
//-------------------------------------------------------

{$ifdef DELPHI_3}
  {$define SUPPORT_VARIANTS}
  {$define WINDOWS}
  {$define ENDIAN_LITTLE}
  {$define SUPPORT_EXPINTF}
{$endif}

{$ifdef DELPHI_4}
  {$define SUPPORT_DEFCHANGED}
  {$define SUPPORT_DEFAULT_PARAMS}
  {$define SUPPORT_OVERLOAD}
  {$define SUPPORT_NEW_TRANSLATE}
  {$define SUPPORT_INT64}
  {$define SUPPORT_REINTRODUCE}
  {$define SUPPORT_FIELDDEFS_UPDATED}
  {$define SUPPORT_FIELDDEF_ATTRIBUTES}
  {$define SUPPORT_FIELDDEF_TPERSISTENT}
  {$define SUPPORT_FIELDDEF_INDEX}
  {$define SUPPORT_FIELDTYPES_V4}
  {$define SUPPORT_UINT32_CARDINAL}
  {$define SUPPORT_MATH_UNIT}
{$endif}

{$ifdef DELPHI_5}
  {$define SUPPORT_BACKWARD_FIELDDATA}
  {$define SUPPORT_INITDEFSFROMFIELDS}
  {$define SUPPORT_DEF_DELETE}
  {$define SUPPORT_FREEANDNIL}
  {$define SUPPORT_TWODIGITYEARCENTURYWINDOW}
{$endif}

{$ifdef DELPHI_6}
  {$define SUPPORT_PATHDELIM}
  {$define SUPPORT_SEPARATE_VARIANTS_UNIT}

  {$define SUPPORT_INCLTRAILPATHDELIM} // Was missing 20130529
  {$define SUPPORT_INCLTRAILBACKSLASH} // Was missing 20130529
{$endif}

{$ifdef Delphi_7}
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CODE off}
  {$warn UNSAFE_CAST off}

  {$define SUPPORT_NATIVEINT}
  {$define SUPPORT_INT64_SEEK}
  {$define SUPPORT_ROUNDTO}
{$endif}

{$ifdef Delphi_2005}
  (* This define tells the code that it can use TFormatSettings
     and pass it to SysUtils.FloatToText.
     (This is not the FormatSettings Variable in SysUtils, which was
     introduced with Delphi XE.)
     It was possibly supported in earlier versions than Delphi 2005.
     If you are sure, please correct this. *)
  {$define SUPPORT_FORMATSETTINGSTYPE}
{$endif}

{$ifdef Delphi_2009}
  {$define WINAPI_IS_UNICODE}
  {$define SUPPORT_TRECORDBUFFER}
  {$define SUPPORT_CHARINSET}
  {$define SUPPORT_MAXLISTSIZEDEPRECATED}
{$endif}

{$ifdef Delphi_XE}
  {$define SUPPORT_FORMATSETTINGS}
  {$define SUPPORT_FORMATSETTINGS_CREATE}
{$endif}

{$ifdef DELPHI_XE3}
  {$define InternalAddRecord_Wants_TRecordBuffer}
  {$define FLOATREC_DIGITS_IS_BYTE}
  {$define SUPPORT_TVALUEBUFFER}
  {$define SUPPORT_GENERICS_FIELDLIST}
{$endif}

{$ifdef DELPHI_XE4}
  {$define SUPPORT_TRECBUF}
  // Note for SUPPORT_ANSISTRINGS_UNIT: AnsiStrings.pas was shipped since D2009,
  // but SysUtils supported PAnsiChar until XE3
  {$define SUPPORT_ANSISTRINGS_UNIT}
  {$define SUPPORT_TVALUEBUFFER_VAR}
  {$define SUPPORT_CALCULATEFIELDS_NATIVEINT}
{$endif}

{$ifdef DELPHI_XE6}
  {$define SUPPORT_FIELD_LIFECYCLES}
{$endif}

//------------------------------------------------------
//--- Conclude supported features in FreePascal      ---
//------------------------------------------------------

{$ifdef FPC_VERSION}

  {$mode delphi}
  {$h+}

{$ifndef CPUI386}
  {$undef USE_ASSEMBLER_486_UP}
{$endif}

{$ifdef USE_ASSEMBLER_486_UP}
  {$asmmode intel}
{$endif}

  {$define SUPPORT_INT64}
  {$define SUPPORT_DEFAULT_PARAMS}
  {$define SUPPORT_OVERLOAD}
  {$define SUPPORT_NEW_TRANSLATE}
  {$define SUPPORT_FIELDDEF_ATTRIBUTES}
  {$define SUPPORT_FIELDDEF_TPERSISTENT}
  {$define SUPPORT_FIELDTYPES_V4}
  {$define SUPPORT_UINT32_CARDINAL}
  {$define SUPPORT_REINTRODUCE}
  {$define SUPPORT_MATH_UNIT}
  {$define SUPPORT_VARIANTS}
  {$define SUPPORT_SEPARATE_VARIANTS_UNIT}
  {$define SUPPORT_ROUNDTO}
  {$define SUPPORT_INT64_SEEK}
  {$define SUPPORT_FORMATSETTINGSTYPE}
  {$undef SUPPORT_EXPINTF}
  {$define SUPPORT_BACKWARD_FIELDDATA}

  // FPC 1.0.x exceptions: no 0/0 support
  {$ifdef VER1_0}
    {$undef NAN}
    {$undef SUPPORT_DEFAULT_PARAMS}
    {$undef SUPPORT_NEW_TRANSLATE}

    {$message error TDbf needs fpc 1.9 minimum.}

  {$endif}

{$endif}

//----------------------------------------------------------
//--- Conclude supported features in non-Windows platforms ---
//----------------------------------------------------------

{$ifndef WINDOWS}

    {$define SUPPORT_PATHDELIM}
    {$define SUPPORT_INCLTRAILPATHDELIM} // Was incorrect SUPPORT_INCLUDETRAILPATHDELIM
    {$define SUPPORT_INCLTRAILBACKSLASH} // Was incorrect SUPPORT_INCLUDETRAILBACKSLASH

{$endif}

{$ifndef ENDIAN_LITTLE}
{$ifndef ENDIAN_BIG}
{$error Neither ENDIAN_LITTLE nor ENDIAN_BIG defined!}
{$endif}
{$endif}

