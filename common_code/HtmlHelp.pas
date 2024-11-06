(******************************************************************************
*                                  HtmlHelp                                   *
* HtmlHelp constants, translated from htmlhelp.h                              *
* Translated by: Ron Loewy, HyperAct, Inc.                                    *
* Last Update: Oct. 21, 1997                                                  *
******************************************************************************)


unit HtmlHelp;

interface

uses
    windows,Graphics;

(* Commands to pass to HtmlHelp *)
const
   HH_DISPLAY_TOPIC		   = $0000;
   HH_DISPLAY_TOC		   = $0001;	// not currently implemented
   HH_DISPLAY_INDEX		   = $0002;	// not currently implemented
   HH_DISPLAY_SEARCH		   = $0003;	// not currently implemented
   HH_SET_WIN_TYPE 		   = $0004;
   HH_GET_WIN_TYPE 		   = $0005;
   HH_GET_WIN_HANDLE		   = $0006;
   HH_GET_INFO_TYPES		   = $0007;	// not currently implemented
   HH_SET_INFO_TYPES		   = $0008;	// not currently implemented
   HH_SYNC 			   = $0009;
   HH_ADD_NAV_UI		   = $000A;	// not currently implemented
   HH_ADD_BUTTON		   = $000B;	// not currently implemented
   HH_GETBROWSER_APP		   = $000C;	// not currently implemented
   HH_KEYWORD_LOOKUP		   = $000D;
   HH_DISPLAY_TEXT_POPUP           = $000E;	// display string resource id or text in a popup window
   HH_HELP_CONTEXT 		   = $000F;	// display mapped numeric value in dwData

(* window properties *)
const
   HHWIN_PROP_ONTOP	      = 2;	  // Top-most window (not currently implemented)
   HHWIN_PROP_NOTITLEBAR      = 4;	  // no title bar
   HHWIN_PROP_NODEF_STYLES    = 8;	  // no default window styles (only HH_WINTYPE.dwStyles)
   HHWIN_PROP_NODEF_EXSTYLES  = 16;	  // no default extended window styles (only HH_WINTYPE.dwExStyles)
   HHWIN_PROP_TRI_PANE 	      = 32;	  // use a tri-pane window
   HHWIN_PROP_NOTB_TEXT	      = 64;   // no text on toolbar buttons
   HHWIN_PROP_POST_QUIT	      = 128;  // post WM_QUIT message when window closes
   HHWIN_PROP_AUTO_SYNC	      = 256;  // automatically ssync contents and index
   HHWIN_PROP_TRACKING 	      = 512; // send tracking notification messages
   HHWIN_PROP_TAB_SEARCH      = 1024; // include search tab in navigation pane
   HHWIN_PROP_TAB_HISTORY     = 2048;   // include history tab in navigation pane
   HHWIN_PROP_TAB_FAVORITES   = 4096;   // include favorites tab in navigation pane
   HHWIN_PROP_CHANGE_TITLE    = 8192;   // Put current HTML title in title bar
   HHWIN_PROP_NAV_ONLY_WIN    = 16384;   // Only display the navigation window
   HHWIN_PROP_NO_TOOLBAR      = 32768;   // Don't display a toolbar

(* window parameters *)
const
  HHWIN_PARAM_PROPERTIES	 =	2;	 // valid fsWinProperties
  HHWIN_PARAM_STYLES	  	 = 4;	 // valid dwStyles
  HHWIN_PARAM_EXSTYLES		 = 8;	 // valid dwExStyles
  HHWIN_PARAM_RECT	  	 = 16;	 // valid rcWindowPos
  HHWIN_PARAM_NAV_WIDTH		 = 32;	 // valid iNavWidth
  HHWIN_PARAM_SHOWSTATE		 = 64;	 // valid nShowState
  HHWIN_PARAM_INFOTYPES		 = 128;	 // valid ainfoTypes
  HHWIN_PARAM_TB_FLAGS		 = 256;	 // valid fsToolBarFlags
  HHWIN_PARAM_EXPANSION		 = 512;	 // valid fNotExpanded
  HHWIN_PARAM_TABPOS	  	 = 1024;	 // valid tabpos
  HHWIN_PARAM_TABORDER		 = 2048;	 // valid taborder
  HHWIN_PARAM_HISTORY_COUNT      = 4096;	 // valid cHistory
  HHWIN_PARAM_CUR_TAB 		 = 8192;	 // valid curNavType

(* button constants *)
const
   HHWIN_BUTTON_EXPAND 	   = 2;	 // Expand/contract button
   HHWIN_BUTTON_BACK		   = 4;	 // Back button
   HHWIN_BUTTON_FORWARD	   = 8;	 // Forward button
   HHWIN_BUTTON_STOP		   = 16;	 // Stop button
   HHWIN_BUTTON_REFRESH		= 32;	 // Refresh button
   HHWIN_BUTTON_HOME			= 64;	 // Home button
   HHWIN_BUTTON_BROWSE_FWD = 128;	 // not implemented
   HHWIN_BUTTON_BROWSE_BCK = 256;	 // not implemented
   HHWIN_BUTTON_NOTES		= 512;	 // not implemented
   HHWIN_BUTTON_CONTENTS	= 1024;	 // not implemented
   HHWIN_BUTTON_SYNC			= 2048;	 // Sync button
   HHWIN_BUTTON_OPTIONS		= 4096;	 // Options button
   HHWIN_BUTTON_PRINT		= 8192;	 // Print button
   HHWIN_BUTTON_INDEX		= 16384;	 // not implemented
   HHWIN_BUTTON_SEARCH 		= 32768;	 // not implemented
   HHWIN_BUTTON_HISTORY		= 65536;	 // not implemented
   HHWIN_BUTTON_FAVORITES	= 131072;	 // not implemented
   HHWIN_BUTTON_JUMP1		= 262144; // Jump 1 button
   HHWIN_BUTTON_JUMP2		= 524288; // Jump 2 button
   HHWIN_BUTTON_ZOOM			= HHWIN_Button_Jump2 * 2;
   HHWIN_BUTTON_TOC_NEXT	= HHWIN_Button_Zoom * 2;
   HHWIN_BUTTON_TOC_PREV	= HHWIN_Button_Toc_Next * 2;

const
   HHWIN_DEF_Buttons = HHWIN_Button_Expand or
                       HHWIN_Button_Back or
                       HHWIN_Button_Options or
                       HHWIN_Button_Print;

(* Button ID's *)
const
   IDTB_EXPAND 			= 200;
   IDTB_CONTRACT			= 201;
   IDTB_STOP				= 202;
   IDTB_REFRESH			= 203;
   IDTB_BACK				= 204;
   IDTB_HOME				= 205;
   IDTB_SYNC				= 206;
   IDTB_PRINT				= 207;
   IDTB_OPTIONS			= 208;
   IDTB_FORWARD			= 209;
   IDTB_NOTES				= 210; // not implemented;
   IDTB_BROWSE_FWD 		= 211;
   IDTB_BROWSE_BACK		= 212;
   IDTB_CONTENTS			= 213; // not implemented;
   IDTB_INDEX				= 214; // not implemented;
   IDTB_SEARCH 			= 215; // not implemented;
   IDTB_HISTORY			= 216; // not implemented;
   IDTB_FAVORITES			= 217; // not implemented;
   IDTB_JUMP1				= 218;
   IDTB_JUMP2				= 219;
   IDTB_CUSTOMIZE			= 221;
   IDTB_ZOOM				= 222;
   IDTB_TOC_NEXT			= 223;
   IDTB_TOC_PREV			= 224;

(* Notification Codes *)
const
   HHN_First = cardinal(-860);
   HHN_Last =  cardinal(-879);

   HHN_NavComplete = HHN_First - 0;
   HHN_Track = HHN_First - 1;

type
   HHN_Notify = record
      hdr : pointer; // NMHDR ?
      pszUrl : PWideChar; // PWideChar
   end;

   HH_Popup = record
      cbStruct : integer; // sizeof this structure
      hinst : THandle; // instance handle for string resource
      idString : cardinal; // string resource id, or text id if pszfile is specified in HH Call
      pszText : pANSIchar; // used if idstring is 0
      pt : TPoint; // top center of popup window
      clrForeground : TColor; // ? ColorRef, -1 for default
      clrBackground : TColor; // ? ColorRef, -1 for default
      rcMargins : TRect; // amount of space between edges of window and text, -1 for each member to ignore
      pszFont : pANSIchar; // facename, pointsize, charset, BOLD ITALIC UNDERLINE
   end; // HH_Popup

   HH_AKLINK = record
      cbStruct : integer; // struct size
      fReserved : bool; // set to false
      pszKeywords : pANSIchar; // ; separated keywords
      pszUrl : pANSIchar; // url to jump if no keywords found
      pszMsgText : pANSIchar; // message if no kwd and url is NULL
      pszMsgTitle : pANSIchar; // title ....
      pszWindow : pANSIchar; // window to display url in
      fIndexOnFail : bool; // display index if kwd lookup fails
   end; // HH_AKLINK

type
   HHWin_NavTypes = (HHWIN_NAVTYPE_TOC,
                     HHWIN_NAVTYPE_INDEX,
                     HHWIN_NAVTYPE_SEARCH,
                     HHWIN_NAVTYPE_HISTORY,
                     HHWIN_NAVTYPE_FAVOURITES);

type
   HH_InfoType = longint;
   PHH_InfoType = ^ HH_InfoType;

type
   HHWin_NavTabs = (HHWIN_NavTab_Top,
                    HHWIN_NavTab_Left,
                    HHWIN_NavTab_Bottom);

const
   HH_Max_Tabs = 19; // max # of tabs

type
   HH_Tabs = (
	HH_TAB_CONTENTS,
	HH_TAB_INDEX,
	HH_TAB_SEARCH,
	HH_TAB_HISTORY,
	HH_TAB_FAVORITES
   );

// HH_DISPLAY_SEARCH Command Related Structures and Constants

const
   HH_FTS_DEFAULT_PROXIMITY = (-1);

type
   HH_FTS_Query = record
      cbStruct : integer; // sizeof structure
      fUniCodeStrings : bool; // true if all strings are unicode
      pszSearchQuery : pANSIchar; // string with the search query
      iProximity : longint; // word proximity
      fStemmedSearch : bool; // true for stemmed search only
      fTitleOnly : bool; // true for title search only
      fExecute : bool; // true to initiate the search
      pszWindow : pANSIchar; // window to display in
   end; // HH_FTS_Query

type
   HH_WinType = record
      cbStruct : integer; // IN: struct size incl. all infotypes
      fUniCodeStrings : bool; // IO: True if all strings are in unicode
      pszType : pANSIchar; // IO: Name of type of window
      fsValidMembers : longint; // IN: Bit flag of valid members (HHWIN_Param_*)
      fsWinProperties : longint; // IO: properties/attributes of the window (HHWIN_*)

      pszCaption : pANSIchar; // IO: Win Title
      dwStyles : longint; // IO: Window Styles
      dwExStyles : longint; // IO: Extended win styles
      rcWindowPos : TRect; // IN: Starting pos, Out: Current pos
      nShowState : integer; // In: Show State (SW_Show etc...)

      hwndHelp : THandle; // Out: Window Handle
      hwndCaller : THandle; // Out: Who called this window

      paInfoTypes : ^ HH_InfoType; // In: pointer to an array of info types

	// The following members are only valid if HHWIN_PROP_TRI_PANE is set

      hwndToolbar : THandle; // Out: toolbar window in tripane
      hwndNavigation : THandle; // Out: Nav window in tripane
      hwndHTML : THandle; // Out: Window displaying HTML in tripane
      iNavWidth : integer; // IO: Width of nav window
      rcHTML : TRect; // Out: HTML Rect

      pszToc : pANSIchar; // In: location of toc file
      pszIndex : pANSIchar; // In: location of index file
      pszFile : pANSIchar; // In: default location of html file
      pszHome : pANSIchar; // IO: html file for home button
      fsToolbarFlags : longint; // In: flags controling TB appearance
      fNotExpanded : bool; // In: Set expanded mode, Out: Current state
      curNavType : integer; // IO: UI to display in nav pane
      tabPos : integer; // IO: NAVTAB_*
      idNotify : integer; // IN: ID to use for wm_notify messages
      TabOrder : array[0 .. HH_Max_Tabs + 1] of byte; // IO: Tab order
      cHistory : integer; // IO: # of history items to keep
      pszJump1 : pANSIchar; // Text for HHWIN_BUTTON_JUMP1
      pszJump2 : pANSIchar; // Text for HHWIN_BUTTON_JUMP2
      pszUrlJump1 : pANSIchar; // Url for HHWIN_BUTTON_JUMP1
      pszUrlJump2 : pANSIchar; // Url for HHWIN_BUTTON_JUMP2
      rcMinSize : TRect; // min size for window
      
   end; // HH_WinType

   PHH_WinType = ^ HH_WinType;
   
type
   HHACTTYpes = (
	HHACT_TAB_CONTENTS,
	HHACT_TAB_INDEX,
	HHACT_TAB_SEARCH,
	HHACT_TAB_HISTORY,
	HHACT_TAB_FAVORITES,

	HHACT_EXPAND,
	HHACT_CONTRACT,
	HHACT_BACK,
	HHACT_FORWARD,
	HHACT_STOP,
	HHACT_REFRESH,
	HHACT_HOME,
	HHACT_SYNC,
	HHACT_OPTIONS,
	HHACT_PRINT,
	HHACT_HIGHLIGHT,
	HHACT_CUSTOMIZE,
	HHACT_JUMP1,
	HHACT_JUMP2,
	HHACT_ZOOM,
	HHACT_TOC_NEXT,
	HHACT_TOC_PREV,
	HHACT_NOTES,

	HHACT_LAST_ENUM
   );

type
   HHNTRACK = record
      hdr : TNMHDR; // NMHDR ?
      pszCurUrl : PWideChar; // Multi-byte, nul-terminated string
      idAction : integer; // HHACT_*
      phhWinType : ^ HH_WinType; // current window type struct
   end; // HHNTRACK 
   PHHNTRACK = ^ HHNTRACK;

   HHNNAVCOMPLETE = record
      hdr : TNMHDR;
      pszUrl : pANSIchar;
   end;
   PHHNNAVCOMPLETE = ^ HHNNAVCOMPLETE;

type 
     THtmlHelpA = function(hwndCaller : THandle; pszFile : pANSIchar; uCommand : cardinal; dwData : longint) : THandle; stdCall;
     THtmlHelpW = function(hwndCaller : THandle; pszFile : pANSIchar; uCommand : cardinal; dwData : longint) : THandle; stdCall;

function HH(hwndCaller : THandle; pszFile : pANSIchar; uCommand : cardinal; dwData : longint) : THandle;
function HtmlHelpInstalled : boolean;

implementation

const
   ATOM_HTMLHELP_API_ANSI     = #14#0;
   ATOM_HTMLHELP_API_UNICODE  = #15#0;

var
   HtmlHelpA : THtmlHelpA;
//   HtmlHelpW : THtmlHelpW;
   OCXHandle : THandle;

(******************************************************************************
*                                  HtmlHelp                                   *
******************************************************************************)
function HH;
begin
   result := 0;
   if (Assigned(HtmlHelpA)) then result := HtmlHelpA(hwndCaller, pszFile, uCommand, dwData);
end; { HtmlHelp }


(******************************************************************************
*                                   boolean                                   *
******************************************************************************)
function HtmlHelpInstalled : boolean;
begin
   result := (Assigned(HtmlHelpA));
end; { HtmlHelpInstalled }



initialization
   HtmlHelpA := nil;
   OCXHandle := LoadLibrary('HHCtrl.OCX');
   if (OCXHandle <> 0) then HtmlHelpA := GetProcAddress(OCXHandle, 'HtmlHelpA');
finalization
   if (OCXHandle <> 0) then FreeLibrary(OCXHandle);
end.
