(*
{$IFDEF FMX}
  {$UNDEF VCL}
{$ELSE}
  {$IFDEF MSWINDOWS}
    {$DEFINE VCL}             //if not FMX, assume VCL on Windows
  {$ENDIF}
{$ENDIF}
 *)

{$IfDef Android}
{$Else}
  {$DEFINE VCL}             //if not FMX, assume VCL on Windows
{$EndIf}


{$IF CompilerVersion >= 18.5}
  {$DEFINE CanInline}         //inlining record methods is unstable before D2007
{$IFEND}

{$IF CompilerVersion >= 20}
  {$DEFINE DepCom}            //deprecated directive allows comments from D2009
  {$DEFINE HasToString}
{$IFEND}

{$IF CompilerVersion >= 21}
  {$DEFINE NoRecEnumBug}      //QC 72213
  {$DEFINE HasIOUtils}
{$IFEND}

{$IF CompilerVersion >= 22}
  {$DEFINE HasTTimeZone}
{$IFEND}

{$IFOPT Q+}
  {$DEFINE OverflowCheckingOn}
{$ENDIF}

{$IFOPT R-}
  {$DEFINE RangeCheckingOff}
{$ENDIF}



