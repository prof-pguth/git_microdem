Unit MVClusterClientDataSet;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


//used in demdatabase_special_cases.inc for MICRODEM perform K-means clustering

{$I nevadia_defines.inc}

{$IfDef RecordProblems}
   //{$Define LogOps}
   //{$Define LogResults}
{$EndIf}

(*****************************************************************************

Author: Fred Edberg; 11/30/02 (fedberg@teleport.com)

Distribution/Use Limitation(s):

This code may be used in conformance with terms of the Mozilla Public License
(MPL).  The user is granted permission to modify and/or use the code in compiled
programs so long as the details of the MPL license are honored.

In the case that the user is able to include this code into a commercially
distributed program... please be so kind as to include the usual public credit
to myself.  It would be greatly appreciated.  Additionally, although it is
certainly not required... if you do find this code useful and wish to forward
a small token of appreciation (of the monetary variety), then you are certainly
encouraged to do so.  Your $25 (US) contribution buys you kind regards and
the possibility of more correspondence and moral support in your pursuit of
a functional statistically-oriented application.

No support is provided.  However, users of this code are recommended to study
the sample project(s) which use this component to better understand its use.

>>> Although I have made a good faith effort that this code contains
no deleterious side-effects or errors... The user assumes all risk and
consequences in using this code. <<<

This Component: A "data-aware" TClientDataSet descendant to which provides very
basic ISODATA clustering functions - results are a text file which lists the
development of the clusters (by iteration) and the final cluster assignments
[for each input record] are contained in ClsLabs publicly exposed array

Delphi Version(s):  Originally written in Delphi6, should compile fine in D7+

******************************************************************************)


//8/21/2003: I have been unable to find a site for this with a Google search, and the email bounces back
//this has been revised for newer versions of Delphi, to use larger data sets, and to integrate with the rest of my code

Interface

Uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

  System.UITypes,
  SysUtils, Classes, Math,Dialogs,
  Petmar_types;

{These were in GeneralFuncs, and have been moved here}

const
   RANGE1to10          = [1..10];
   MAXSIGNATURES       = 50;    // Arbitrary, for now - max # of signatures
   MaxClusters         = 50;    // Change to increase # of clusters allowed
   MAXITERATIONS       = 200;   // Max number of iterations for clustering
   VERYSMALLNUMBER     = -9999999999999999999999.0;  // Small number; arbitrary
   VERYLARGENUMBER     =  9999999999999999999999.0;  // Large number; arbitrary
   MAXHISTOBINS        = EdburgGeneralFuncsMaxObservations;   // Arbitrary - # of 'bins' for histogram
   ONE                 = 1;
   TWO                 = 2;
   ONE_HALF            = 1 / 2;
   ONE_QUARTER         = 1 / 4;
   ONE_EIGHTH          = 1 / 8;

   //CRLF                = sLineBreak;
   //CRLF2               = sLineBreak + sLineBreak;
   //HTMLRed             = '"#DD0000"';
   //HTMLBlack           = '"#000000"';
   ERROR_PREFIX        = 'Error in ';
   ABS_NEAR_ZERO       = 0.00001;
   CLOSE_TO_ZERO       = 0.01;
   DISP_ZERO_CELL      = '------';


   // Strings which are used when raising exceptions in stat. funcs/methods/etc
   VarianceProportionsVectorError  = ERROR_PREFIX + ' VarianceProportionsVector';
   CheckDataError                  = ERROR_PREFIX + ' CheckData';
   ChiSquareError                  = ERROR_PREFIX + ' ChiSquare';
   SmoothError                     = ERROR_PREFIX + ' SmoothData';
   SkewError                       = ERROR_PREFIX + ' GetSkew';
   KurtosisError                   = ERROR_PREFIX + ' GetKurtosis';
   GetMinError                     = ERROR_PREFIX + ' GetMin';
   GetMaxError                     = ERROR_PREFIX + ' GetMax';
   GetMeanError                    = ERROR_PREFIX + ' GetMean';
   GetSumError                     = ERROR_PREFIX + ' GetSum';
   GetVarianceError                = ERROR_PREFIX + ' GetVariance';
   GetCofVariationError            = ERROR_PREFIX + ' GetCoefficientOfVariation';
   VarianceVectorError             = ERROR_PREFIX + ' VarianceVector';
   CalcCpError                     = ERROR_PREFIX + ' CalcCp';
   CalcFpError                     = ERROR_PREFIX + ' CalcFp';
   CalcMSEpError                   = ERROR_PREFIX + ' CalcMSEp';
   TCalculatedError                = ERROR_PREFIX + ' TCalculated';
   SmallSampleError                = ERROR_PREFIX + ' SmallSampleCIforMean';
   LargeSampleError                = ERROR_PREFIX + ' LargeSampleCIfor Mean';
   BVBeta1CIError                  = ERROR_PREFIX + ' BivarRegressionBeta1CI';
   BVRegCIError                    = ERROR_PREFIX + ' BivarRegressionCI';
   BVRegPIError                    = ERROR_PREFIX + ' BivarRegressionPI';
   BVRegTestError                  = ERROR_PREFIX + ' BivarRegressionTestStatistic';
   BVRegReverseError               = ERROR_PREFIX + ' BivarRegressionReverse';
   BVCorrelationSampleTestStatError= ERROR_PREFIX + ' BVCorrelationSampleTestStat';
   MVRegCIError                    = ERROR_PREFIX + ' MultiVarRegressionCI';
   MVRegPIError                    = ERROR_PREFIX + ' MultiVarRegressionPI';
   MVRegBetaTestStatError          = ERROR_PREFIX + ' MultiVarRegressionBetaTestStatistic';
   MVRegBetaCIError                = ERROR_PREFIX + ' MultiVarRegressionBetaCI';
   BVResidualsError                = ERROR_PREFIX + ' BivarResiduals';
   BVPredictedError                = ERROR_PREFIX + ' BivarPredictedY';
   CorrelationMatrixError          = ERROR_PREFIX + ' CorrelationMatrix';
   CovarianceMatrixError           = ERROR_PREFIX + ' CovarianceMatrix';
   MeansDifferencesMatrixError     = ERROR_PREFIX + ' MeansDifferencesMatrix';
   VectorDivideError               = ERROR_PREFIX + ' VectorDivideScalar';
   NormalizeVectorError            = ERROR_PREFIX + ' NormalizeVectorByMax';
   TransformInverseError           = ERROR_PREFIX + ' TransformInverse';
   TransformSquareError            = ERROR_PREFIX + ' TransformSquare';
   TransformSquareRootError        = ERROR_PREFIX + ' TransformSquareRoot';
   TransformLogError               = ERROR_PREFIX + ' TransformLog';
   StandardizeError                = ERROR_PREFIX + ' StandardizeData';
   NormalizeError                  = ERROR_PREFIX + ' NormalizeData';
   CreateInteractionVarError       = ERROR_PREFIX + ' CreateInteractionVar';
   NormalizeDataByMaxError         = ERROR_PREFIX + ' NormalizeDataByMax';
   MVResidualsError                = ERROR_PREFIX + ' MultiVarResiduals';
   MVStdResidualsError             = ERROR_PREFIX + ' MultiVarStdResiduals';
   MVStudentResidualsError         = ERROR_PREFIX + ' MultiVarStudentResiduals';
   MVPredictedError                = ERROR_PREFIX + ' MultiVarPredictedY';
   CooksDError                     = ERROR_PREFIX + ' CooksDistance';
   FlagOutliersError               = ERROR_PREFIX + ' FlagOutliers';
   DurbinWatsonError               = ERROR_PREFIX + ' DurbinWatson';
   MahalanobisDistanceError        = ERROR_PREFIX + ' MahalanobisDistance';
   MahalanobisDistancesError       = ERROR_PREFIX + ' MahalanobisDistances';
   MaximumLikelihoodDistancesError = ERROR_PREFIX + ' MaximumLikelihoodDistances';
   EuclidDistError                 = ERROR_PREFIX + ' EuclideanDistances';
   NormEuclidDistError             = ERROR_PREFIX + ' NormEuclideanDistances';
   AvgNormDistanceError            = ERROR_PREFIX + ' AvgNormDistance';
   DivergenceError                 = ERROR_PREFIX + ' Divergence';


type

// Separability types - to quantify degree of distinctness between
// classes (using mean vector, variance/covariance matrix, etc)
TSeparabilityMeasure = (smAveNormDistance,smDivergence,smJeffriesMatusitaDistance,smBhattacharyyaDistance);

// Method used to measure 'distance' to class [means, for the most part]
TDistanceMeasure = (dmLDist,dmEuclidean,dmNormEuclidean,dmMahalanobis,dmMaxLikelihood);

// tTable significance levels available (90%, 95%, 97.5%, 99%, 99.5%)
TSignificanceLevel = (sl90,sl95,sl975,sl99,sl995);

// a [k x n] data array of single for input data
TRegressDataArray = array [0..EdburgMaxVariables-1,0..EdburgGeneralFuncsMaxObservations-1] of double;
PTRegressDataArray = ^TRegressDataArray;

// a [n] array to hold residuals or predicted Y's
TDoubleArray = array [0..EdburgGeneralFuncsMaxObservations-1] of double;
PDoubleArray = ^TDoubleArray;

// a [n] array to hold byte/int values for each obs
TIntegerArray = array [0..EdburgGeneralFuncsMaxObservations-1] of integer;

// a [k x k] matrix (used for covariance, correlation functions)
TMatrix = array [0..EdburgMaxVariables,0..EdburgMaxVariables] of double;
XMatrix = array [1..EdburgMaxVariables+1,1..EdburgMaxVariables+1] of double;

// 2-d matrix structure used to compute contingency "error" matrix
TErrorMatrix = array [0..MAXSIGNATURES+2,0..MAXSIGNATURES+2] of double;

// some [0..n] and [1..n+1] vectors to hold means, mins, maxs...
TVector        = array [0..EdburgMaxVariables-1] of double;
XVector        = array [1..EdburgMaxVariables+1] of double;

// Holds a pair of double vars to represent upper & lower bounds
// for a confidence/prediction interval, etc.
TIntervalRec = record
  Upper : extended;
  Lower : extended;
end;

// Array types to hold interval records-used in ANOVARec
TIntervalArray = array [0..EdburgMaxVariables-1] of TIntervalRec;
TIntervalMatrix = array [0..EdburgMaxVariables-1,0..EdburgMaxVariables-1] of TIntervalRec;

// Holds means for cluster routines
TClusterMeansArray = array [1..MaxClusters,1..EdburgMaxVariables] of double;
TClusterVariancesArray = TClusterMeansArray;

// Array of xMatrix's to store C matrix for each cluster
TClusterCovariancesArray = array [1..MaxClusters] of XMatrix;

// Class assignment for each observation 1..n
TVectorClassLabels = array [1..EdburgGeneralFuncsMaxObservations] of byte;

// A signature object - to characterize data contained in a group of records
TSignature = class(TObject)
public
  sSourceFieldName    : string;
  iSourceFieldValue   : integer;
  k                   : integer;
  Mins                : XVector;
  Maxs                : XVector;
  Means               : XVector;
  Variances           : XVector;
  Covariances         : XMatrix;
  InverseCovariances  : XMatrix;
  dInvCovDeterminant  : double;
  iCount              : integer; // "n"
  ConfusionAlarm      : boolean; // signify when a signature overlaps (via piped)
  constructor Create;
  procedure ClearSignature;                         // Initialization
  procedure SaveToFile (const sFileName: string);   // Save to simple ASCII
  procedure LoadFromFile (const sFileName: string); // Load from " "
end;

// Array of signature objects
TSignatures = array [0..MAXSignatures-1] of TSignature;

// Signatures 'list' object - basically, a 'wrapper' for TSignatures
TSignatureList = class(TObject)
public
  Signatures: TSignatures;
  iCount: integer;
  constructor Create;
  procedure SaveToFile (const sFileName: string);    // Save to simple ASCII
  procedure LoadFromFile (const sFileName: string);  // Load from " "
  procedure AddSignature (const sigSignature: TSignature); // Add method
  function DeleteSignature (const iSignatureID: integer): boolean; // Delete
end;


// Output record from Kruskal Wallis test
TKWRec = record
    RankSums : XVector;  // Hold rank sums
    HCalc    : extended; // H Calc value
end;

// Output record structure from ANOVA function(s)
TAnovaRec = record
    VarNames           : array [0..EdburgMaxVariables] of string[32];
    k,                 // Number of groups ('treatments')
    n: integer;        // Number of observations
    S,                 // sqrt(k-1) * F(k-1,n-k,1-alpha)
    G,                 // Sums of treatments
    G2,                // Sqr(Sums of treatments)
    YBarBar,           // Grand Mean (treatment means)
    T2,                // Intermediate sum2 value
    Y2,                // Sqr(Sum of all observations)
    MST,               // Total Sums
    MSE,               // Sums of Error
    FStat              : double;   // F statistic  (MST/MSE)
    FCritical          : single;   // F Critical
    TreatmentMeans     : TVector;  // vector of group means
    TreatmentVariances : TVector;  // vector of group variances
    DistancesMatrix    : TMatrix;  // matrix of diff's between group means
    NumberCombinations : byte;
    IntervalArray      : TIntervalArray;
    IntervalMatrix     : TIntervalMatrix;
end;

// Output from univariate summary
TUnivariateRec = record
    k : byte;            // number of vars
    n : integer;         // number of obs
    Sums,                // vector of sums
    Mins,                // vector of mins
    Maxs,                // vector of maxs
    Ranges,              // vector of maxs-mins
    Means,               // vector of means
    StdDevs,             // vector of stddevs
    Variances,           // vector of variances
    Skews,               // vector of skews
    Kurts,               // vector of kurtosis
    CofVs,               // vector of CofVs
    Medians: XVector;    // vector of medians
end;

// A Single Variable Summary Record
TVarSummaryRec = record
   n : integer;
   Sum,
   Min,
   Max,
   Range,
   Mean,
   StdDev,
   Variance,
   Skew,
   Kurt,
   CofV,
   Median: extended;
end;

// A BivarStatRec used in BivarRegression function
TBivarStatRec = record
    N     : integer;   // Number of observations
    df    : integer;
    r,                 // Correlation coefficient
    XMean,             // Mean of X data values
    YMean,             // Mean of Y data values
    Beta0,             // B0 : Y intercept of LsLine
    Beta1,             // B1 : slope of LsLine
    SSXX,              // SumX2 - sqr(SumX's)/N
    SSYY,              // SumY2 - sqr(SumY's)/N
    SSXY,              // SumXY - (SumX*SumY)/N
    SumX,              // Sum of X[i]'s
    SumY,              // Sum of Y[i]'s
    SumXY,             // Sum of X[i]'s * Y[i]'s
    SumX2,             // Sum of sqr(X[i]'s)
    SumY2: extended;   // Sum of sqr(Y[i]'s)
end;

// A TMultStatRec used in MutlipleRegression function
TMultStatRec = record
    VarNames : array [1..EdburgMaxVariables] of string[32];
    N,                  // Number of Obs.
    k        : integer; // Number of variables
    R,                  // Multiple Correlation
    R2,                 // Sqr(R)
    FStat,              // F-statistic
    RMSE,               // Root of MSE
    MSE,                // Mean Sqr for Error
    MSR,                // Mean Sqr for Regression
    SSE,                // Sum Sqrs of Error
    SSR,                // Sum Sqrs of Regression
    SST,                // Sum Sqrs Total
    S2,                 // Overall variance
    S       : extended; // Overall S..Sqrt(S2)
    Beta    : XVector;  // Vector for Beta params
    SeBeta  : XVector;  // Vector for StdErr of Beta's
    BetaCILower : XVector;
    BetaCIUpper : XVector;
    TValue  : XVector;  // Vector for TValues for Beta's
    DFModel : integer;  // Deg. Freedom for Model
    DFError : integer;  // Deg. Freedom for Error
    DFTotal : integer;  // Deg. Freedom Total
end;

// For automatic multiple reg model assessment-size = 68bytes
// the array size of 10, is the upper limit on the number of
// predictor (X) variables allowed in the All-Possible-Combos
// function (yielding 1023 or so unique models to assess)
TMultRegComboRec = record
    NumberVars : integer;
    VarsArray  : array [1..10] of byte;
    FactorRank : single;
    R2p        : double;
    Fp         : double;
    MSEp       : double;
    Cp         : double;
    Simple     : double;
end;

// Record to hold basic stats for error matrix calcs/output
TKappaRec = record
    ObsGrandTotal   : double;
    ObsTotalCorrect : double;
    ObsPctCorrect   : double;
    ExpGrandTotal   : double;
    ExpTotalCorrect : double;
    ExpPctCorrect   : double;
    Kappa           : double;
end;

// Holds values from input ASCII file for Model selection func
TMultRegComboRecArray = array [1..1024] of TMultRegComboRec;

THistoRec = record
    Lower: double;
    Upper: double;
    iCount: integer;
    iCumulativeCount: integer;
    dProportionOfTotal: double;
end;

// Data type to store a computed histogram (for a single variable [field])
THistogramArray = array [1..MAXHISTOBINS] of THistoRec;

// Record to hold a t-value
tStatRec = record
    df   : byte;
    t10  : single;
    t05  : single;
    t025 : single;
    t010 : single;
    t005 : single;
end;

// Clustering-specific container arrays
TSSEArray      = array [1..MAXITERATIONS] of extended;
TDivergArray   = array [1..MaxClusters,1..MaxClusters] of double;
DistArray      = array [0..255] of extended;
TClusterCounts = array [1..MaxClusters] of integer;

TSignatureSeparabilityArray = array [1..MAXSIGNATURES,1..MAXSIGNATURES] of double;

TStringArray = array [0..EdburgMaxVariables-1] of string;

// A record to represent - both a Value and a Rank
// used in functions which sort and assign rank
TSortPair = record
  dValue: double;
  iRank: word;
end;

// An array of TSortPair
TSortArray = array [0..EdburgGeneralFuncsMaxObservations-1] of TSortPair;


PTSortArray = ^TSortArray;


// To hold t-tables
var
  tStatArr : array [1..50] of tStatRec;

function MaxVariablesAllowed:integer;

function MaxObservationsAllowed:integer;

function MaxClustersAllowed:integer;

function InitBivarStatRec: TBivarStatRec;

function InitMultStatRec: TMultStatRec;

function InitInterval: TIntervalRec;

function InitKappaRec: TKappaRec;

procedure InitErrorMatrix ( var emat: TErrorMatrix);

function Cube ( const dValue : double ): extended;

function Permutations ( const TotalNumber : byte; const SubGroup    : byte): extended;

function Combinations ( const TotalNumber : byte; const SubGroup    : byte): extended;

function Factorial ( const IntIn : byte): extended;


function FastMatrixMultiply             (const aMatrix1 : XMatrix;
                                         const aMatrix2 : XMatrix;
                                         const k        : byte): XMatrix;

function MultiplyMatrices               (const aMatrix1 : XMatrix;
                                         const aMatrix2 : XMatrix;
                                         const k        : byte): XMatrix;

function UpperTriangularMatrixVectorMultiply(
                                              const aVector : XVector;
                                              const aMatrix : XMatrix;
                                              const k       : byte): double;

function Determinant2x2 ( const a1: double;
                          const b2: double;
                          const c3: double;
                          const d4: double):double;

function TriangularizeMatrix          ( aVector  : XVector;
                                        aMatrix  : XMatrix;
                                        const k  : byte): XMatrix;

function DivideMatrixScalar             (const aMatrix1 : XMatrix;
                                         const k        : byte;
                                         const aScalar  : extended): XMatrix;

function NormalizeMatrix                (const aMatrix: XMatrix;
                                         const k : byte): XMatrix;

function DivideMatrixDiagonalScalar     ( const aMatrix1 : XMatrix;
                                          const k        : byte;
                                          const aScalar  : extended): XMatrix;

function MultiplyMatrixByVector          (const aMatrix : XMatrix;
                                          const aVector : XVector;
                                          const k       : byte): XVector;

function MultiplyVectorByVector          (const Vector1 : XVector;
                                          const Vector2 : XVector;
                                          const k       : byte): extended;

function MultiplyVectorByMatrix          (const aMatrix : XMatrix;
                                          const aVector : XVector;
                                          const k       : byte): XVector;

function MultiplyVectorByTransposeVector (const Vector1 : XVector;
                                          const Vector2 : XVector;
                                          const k       : byte): extended;

function InitMatrix                       (const k      : byte;
                                           const aValue : integer): XMatrix;

function InitVector                       (const k      : byte;
                                           const aValue : integer): XVector;

procedure InitResidArray                 (var AnArray   : array of double;
                                         const aValue   : integer);

procedure InitIntArray                 (var AnArray     : array of integer;
                                        const aValue    : integer);

function InvertMatrix                     (const InMatrix : XMatrix;
                                           const k        : byte;
                                           var Det        : double): XMatrix;

function IdentityMatrix                   (const k        : byte): XMatrix;

function CopyMatrix                       (const aMatrix  : XMatrix;
                                           const k        : byte): XMatrix;

function MakePiped                         (const Mean   : double;
                                            const StdDev : double;
                                            const Scalar : double): TIntervalRec;

function TTableValue2                      (const n      : integer;
                                            const aSL    : TSignificanceLevel): single;

function ZTableValue2                      (const PercentageLevel : string): extended;

function TransposeVector                 (const Vector1 : XVector;
                                          const k       : byte): XVector;

function DivideVector                    (const Vector1 : XVector;
                                          const k       : byte;
                                          const dValue  : double): XVector;

function AddVector                       (const Vector1 : XVector;
                                          const Vector2 : XVector;
                                          const k       : byte): XVector;

function SubtractVector                  (const Vector1 : XVector;
                                          const Vector2 : XVector;
                                          const k       : byte): XVector;

function AddMatrices                    (const aMatrix1 : XMatrix;
                                         const aMatrix2 : XMatrix;
                                         const k        : byte): XMatrix;

function SubtractMatrices               (const aMatrix1 : XMatrix;
                                         const aMatrix2 : XMatrix;
                                         const k        : byte): XMatrix;

function DotProductMultiplyMatrices     ( const aVector1: XVector;
                                          const aVector2: XVector;
                                          const k       : byte): XMatrix;

function MatrixTrace                    (const aMatrix  : XMatrix;
                                         const k        : byte): extended;

function ZTableValue                      (const aZ     : extended): extended;

function LargeSampleCIforMean             (const YBar       : double;
                                           const Sigma      : double;
                                           const Percentage : string): TIntervalRec;

function SmallSampleCIforMean             (const YBar       : double;
                                           const S          : double;
                                           const n          : integer;
                                           const aSL        : TSignificanceLevel): TIntervalRec;

function CheckKN                           (const k     : byte;
                                            const n     : integer): boolean;

function GetMidpt                        (const anArray : array of double;
                                          const n       : integer): extended;

function GetZValue                 (const aValue       : extended;
                                    const aMean        : extended;
                                    const aStdDev      : extended): extended;

function GetCenterValue            (const aValue       : extended;
                                    const aMean        : extended): extended;

function GetMedian                 (const AnArray      : array of double;
                                    const n            : integer): extended;

function GetSum                    (const anArray      : array of double;
                                    const n            : integer): extended;

function GetMin                    (const anArray      : array of double;
                                    const n            : integer): extended;

function GetMax                    (const anArray      : array of double;
                                    const n            : integer): extended;

function GetMean                   (const anArray      : array of double;
                                    const n            : integer): extended;

function GetVariance               (const anArray      : array of double;
                                    const n            : integer): extended;

function GetCoefficientOfVariation (const AnArray      : array of double;
                                    const n            : integer): extended;

function GetSkew                   (const AnArray      : array of double;
                                    const n            : integer): single;

function GetKurtosis               (const AnArray      : array of double;
                                    const n            : integer): single;

function GetTotalVariation         (const VarVector    : XVector;
                                    const k            : byte): extended;

function MeanVector                 (const PRegressData : PTRegressDataArray;
                                     const k            : byte;
                                     const n            : integer): XVector;

function VarianceVector            (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XVector;

function VarianceProportionsVector (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XVector;

function SumVector                 (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XVector;

function MinVector                 (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XVector;

function MaxVector                 (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XVector;

function NormalizeVector           (const aVector      : XVector;
                                    const k            : byte): XVector;

function MVCheckXData            (  const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): boolean;

function CrossProducts             ( const PRegressData : PTRegressDataArray;
                                     const k            : byte;
                                     const n            : integer): XMatrix;

function BVRegressionShort         ( const  X           : array of double;
                                     const  Y           : array of double;
                                     const  n           : integer): double;

function CalcCp                         (const SSEp   : double;
                                         const MSEk   : double;
                                         const p      : byte;
                                         const n      : integer): double;

function CalcFp                         (const R2p    : double;
                                         const R2k    : double;
                                         const k      : byte;
                                         const p      : byte;
                                         const n      : integer): double;

function CalcMSEp                       (const SSEp   : double;
                                         const p      : byte;
                                         const n      : integer): double;

function CalcF                            (const MSR  : extended;
                                           const MSE  : extended): double;

function CalcT                            (const XBar : double;
                                           const Meu  : double;
                                           const S    : double;
                                           const n    : integer): double;

function ProbabilityZIsHigher             (const aSD  : extended): extended;

function ProbabilityZIsLower              (const aSD  : extended): extended;

function ProbabilityZBetweenPlusMinus     (const aSD  : extended): extended;

function CorrelationMatrix        (const PRegressData : PTRegressDataArray;
                                   const k            : byte;
                                   const n            : integer): XMatrix;

function CovarianceMatrix         (const PRegressData : PTRegressDataArray;
                                   const k            : byte;
                                   const n            : integer): XMatrix;

function InverseCorrelationMatrix (const PRegressData : PTRegressDataArray;
                                   const k            : byte;
                                   const n            : integer): XMatrix;

function InverseCovarianceMatrix  (const PRegressData : PTRegressDataArray;
                                   const k            : byte;
                                   const n            : integer): XMatrix;

function GetAvgNormDistance (const MeanVector1 : XVector;
                             const MeanVector2 : XVector;
                             const CovMatrix1  : XMatrix;
                             const CovMatrix2  : XMatrix;
                             const k           : byte): extended;

function GetDivergence (const MeanVector1 : XVector;
                        const MeanVector2 : XVector;
                        const xMatCov1    : XMatrix;
                        const xMatCov2    : XMatrix;
                        const k           : byte;
                        const iScalar     : integer): extended;

function GetBhattacharyyaDistance (const MeanVector1   : XVector;
                                   const MeanVector2   : XVector;
                                   const xMatCov1      : XMatrix;
                                   const xMatCov2      : XMatrix;
                                   const k             : byte): extended;

function GetJeffriesMatusitaDistance (const MeanVector1   : XVector;
                                      const MeanVector2   : XVector;
                                      const xMatCov1      : XMatrix;
                                      const xMatCov2      : XMatrix;
                                      const k             : byte;
                                      const iScalar       : integer): extended;

function CaseFormat(const aString: string): string;

function MinSampleSizeTwoMeans ( const zOneMinusAlpha       : double;
                                 const zOneMinusBeta        : double;
                                 const dPopulationVariance  : double;
                                 const dSampleMeansDiff     : double): integer;

procedure MeanSmoothData       ( var AnArray       : array of double;
                                 const n           : integer;
                                 const iWindowSize : byte);

function CalcChiSquare (const Y1 : array of double;
                        const Y2 : array of double;
                        const n  : integer): extended;

function GetLDistance (  const aVector    : XVector;     // 'X'
                         const MeanVector : XVector;     // 'M'
                         const k          : byte): extended;

function GetEuclideanDistance (const aVector    : XVector;     // 'X'
                               const MeanVector : XVector;     // 'M'
                               const k          : byte): extended;

function GetNormEuclideanDistance (const aVector    : XVector;    // 'X'
                                   const MeanVector : XVector;    // 'M'
                                   const CovMatrix  : XMatrix;
                                   const k          : byte): extended;

function GetMahalanobisDistance       (const aVector    : XVector;    // 'X'
                                       const MeanVector : XVector;    // 'M'
                                       const CovMatrix  : XMatrix;
                                       const k          : byte): extended;

function GetMahalanobisDistanceInvCov (const aVector      : XVector;    // 'X'
                                       const MeanVector   : XVector;    // 'M'
                                       const InvCovMatrix : XMatrix;
                                       const k            : byte): extended;

function GetMaximumLikelihoodDistance (const aVector    : XVector;    // 'X'
                                       const MeanVector : XVector;    // 'M'
                                       const CovMatrix  : XMatrix;
                                       const k          : byte;
                                       const APriori    : extended): extended;

function GetMaximumLikelihoodDistanceInvCov
                                      (const aVector      : XVector;    // 'X'
                                       const MeanVector   : XVector;    // 'M'
                                       const InvCovMatrix : XMatrix;
                                       const dDeterminant : double;
                                       const k            : byte;
                                       const APriori      : extended): extended;

function GetVector( const PRegressData  : PTRegressDataArray;
                    const iRecCnt       : integer;
                    const iVars         : integer): XVector;

function FindSmallestPos ( const Arr     : array of double;
                           const iSigCnt : integer): integer;

function FindLargestPos ( const Arr     : array of double;
                          const iSigCnt : integer): integer;

function IsInPiped( const vecVector   : XVector;
                    const vecMeans    : XVector;
                    const vecVariances : XVector;
                    const k           : integer;
                    const dDist       : double): boolean;

function ErrorMatrixSum                 (const ematErrorMatrix : TErrorMatrix;
                                         const k               : byte): double;

function CrossTabulation                (const arrX : TIntegerArray;
                                         const arrY : TIntegerArray;
                                         const n    : integer;
                                         var emat   : TErrorMatrix): integer;

procedure CompileErrorMatrix            (var ematCompiled : TErrorMatrix;
                                             k            : byte);

function GetKappaValue                  (var ematErrorMatrix : TErrorMatrix;
                                             k               : byte): TKappaRec;

function CopyVector ( const aVector : XMatrix;
                      const k       : byte): XVector;

procedure RescaleDataArrayZeroMin ( var dArray             : array of double;
                                    const n                : integer;
                                    const bTruncValues     : boolean);

procedure RandomDataArray (var dArray   : array of double;
                           const n      : integer;
                           const iValue : integer);

procedure RandomNormalDataArray (var dArray    : array of double;
                                 const n       : integer;
                                 const dMean   : double;
                                 const dStdDev : double);

function CalcHistogram  ( const dArray            : array of double;
                          const n                 : integer;
                          const dBinSize          : double;
                          const bRoundBinStart    : boolean;
                          var Histogram           : THistogramArray): integer;

procedure ApplyLinearCombination (const PRegressData  : PTRegressDataArray;
                                  var   Y             : array of double;
                                  const k             : byte;
                                  const n             : integer;
                                  const arrWeights    : XVector);

function MeansDifferencesMatrix (const MeansVector : XVector;
                                 const k           : byte): XMatrix;

function SpearmansR ( const X : TIntegerArray;
                      const Y : TIntegerArray;
                      const n : integer): double;

function SpearmansRStdError ( const X : TIntegerArray;
                              const Y : TIntegerArray;
                              const n : integer): extended;



{End of GeneralFuncs}


const
  sDEFAULT_FILE_NAME = 'cluster.txt';

type

  TInitializationOption = (ioMinMax,ioStdDev,ioRandom);

  TMVClusterClientDataSet = class(TClientDataSet)
    private
      fNIterations: integer;
      fOutputFile : AnsiString;
      fShowDoneMessage: boolean;
      fEndOnConvergence : boolean;
      fResetAfterCalcs: boolean;
      fConvergenceThreshold: double;
      fInitOption: TInitializationOption;
      fAccumulateClusterStats: boolean;
      procedure SetInitOption ( Value : TInitializationOption);

      procedure MVPrepare;

      function FieldTypesOK              (const sXFields : array of AnsiString;
                                          const k        : byte): boolean;

      function GetCurrentData            (const sXFields : array of AnsiString;
                                          const k        : byte): integer;

      function ComputeInitialClusterCentersMinMax (
                                            const aMeanVector    : XVector;
                                            const aMinVector     : XVector;
                                            const aMaxVector     : XVector;
                                            const k              : byte;
                                            const NumberClusters : byte): TClusterMeansArray;

      function ComputeInitialClusterCentersStdDev (
                                            const aMeanVector    : XVector;
                                            const aVarianceVector: XVector;
                                            const k              : byte;
                                            const NumberClusters : byte): TClusterMeansArray;

      function ComputeInitialClusterCentersRandom (
                                            const aMeanVector    : XVector;
                                            const aMinVector     : XVector;
                                            const aMaxVector     : XVector;
                                            const k              : byte;
                                            const NumberClusters : byte): TClusterMeansArray;

      function AssignDataToCluster ( const PRegressData   : PTRegressDataArray;
                                     const Clusters       : TClusterMeansArray;
                                     const NumberClusters : byte;
                                     const k              : byte;
                                     const n              : integer):TVectorClassLabels;
      function AssignDataToOrderedClusters (  const PRegressData   : PTRegressDataArray;
                                                        const Clusters       : TClusterMeansArray;
                                                        const NumberClusters : byte;
                                                        const k              : byte;
                                                        const n              : integer)  :  TVectorClassLabels;
      function CalcClusterSSE      (  const PRegressData : PTRegressDataArray;
                                      const ClsLabs      : TVectorClassLabels;
                                      const NewCenters   : TClusterMeansArray;
                                      const k            : byte;
                                      const n            : integer): extended;

      function ComputeClusterMeans ( when : shortString;
                                      const PRegressData   : PTRegressDataArray;
                                      const ClsLabls       : TVectorClassLabels;
                                      const NumberClusters : byte;
                                      const k              : byte;
                                      const n              : integer): TClusterMeansArray;

    protected
      DivergencesArray : TDivergArray;
      ClusterTotalSumDistances: TSSEArray;
      ClusterVariances : TClusterVariancesArray;
      DistArr : DistArray;
      TmpClsLabs : TVectorClassLabels;
      XTmpArray : TDoubleArray;
      YTmpArray : TDoubleArray;

      procedure GatherFinalStats   ( const k: byte);

      function CorrespondenceFactor( const TmpClsLabs : TVectorClassLabels;
                                     const ClsLabs    : TVectorClassLabels): double;

    public
      NClusters: integer;                //Moved from protected by PLG so it will be accessible outside the routine for classifying
      ClsCenters : TClusterMeansArray;   //Moved from protected by PLG so it will be accessible outside the routine for classifying
      ClusterMeans : TClusterMeansArray; //Moved from protected by PLG so it will be accessible outside the routine for classifying
      ClsCounts : TClusterCounts;        //Moved from protected by PLG so it will be accessible outside the routine for classifying
      NewCenters : TClusterMeansArray;   //Moved from protected by PLG so it will be accessible outside the routine for classifying
      PRegressData      : PTRegressDataArray; // Holds X variables values [Dynamic]
      n                 : integer;
      MeanVect          : XVector;
      VarVect           : XVector;
      MinVect           : XVector;
      MaxVect           : XVector;
      CovarMatrix       : XMatrix;
      Interval          : TIntervalRec;  // (upper,lower)
      SSEArray          : TSSEArray;
      ClsLabs           : TVectorClassLabels;
      ClusterCovariances: TClusterCovariancesArray;

      constructor Create (aOwner: TComponent); override;

      destructor Destroy; override;

      procedure SaveToHTML(const sFileName : AnsiString; const sHeader   : AnsiString);

      function KMeansClustering ( var ClusterSummary    : tStringList;
                                  const sXFields        : array of AnsiString;
                                  const k               : byte;
                                  const sHTMLFileName   : AnsiString) : extended;

    published
      property NIterations : integer read FNIterations write FNIterations;
      property OutputFileName : AnsiString read FOutputFile write FOutputFile;
      property AccumulateClusterStats : boolean read FAccumulateClusterStats write FAccumulateClusterStats;
      property ShowDoneMessage : boolean read FShowDoneMessage write FShowDoneMessage;
      property EndOnConvergence : boolean read FEndONConvergence write FEndOnConvergence;
      property ResetAfterCalcs : boolean  read FResetAfterCalcs write FResetAfterCalcs;
      property ConvergenceThreshold : double read FConvergenceThreshold write FConvergenceThreshold;
      property InitOption : TInitializationOption read FInitOption write SetInitOption;
    end;



implementation




{these were in GeneralFuncs}


        // TSignature Methods=========================================================


        (*****************************************************************************
        * Name     :  [TSignature] Create
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  TSignature constructor
        * Result   :
        * History  :  New
        ******************************************************************************)
        constructor TSignature.Create;
        begin
          inherited Create;

          with Self do begin
            ClearSignature;
            ConfusionAlarm:= FALSE;
          end; // with

        end;

        (*****************************************************************************
        * Name     :  [TSignature] ClearSignature
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  Clear the Signature object (init to 0's for the most part)
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure TSignature.ClearSignature;
        var
          i,j:integer;
        begin

          with Self do begin
            sSourceFieldName  := '';
            iSourceFieldValue := -1;
            iCount            := 0;
            ConfusionAlarm    := FALSE;

            for i:= 1 to k do begin
              Mins[i]      := 1e20;
              Maxs[i]      := -1e20;
              Means[i]     := 0;
              Variances[i] := 0;
            end; // for

           for i:= 1 to k do
             for j:= 1 to k do
               Covariances[i,j]:= 0;
          end; // with Self

        end;

        (*****************************************************************************
        * Name     :  [TSignature] SaveToFile
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  Save the signature contents to a text file
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure TSignature.SaveToFile (const sFileName: string); // save to simple ASCII
        var
         i,j:integer;
         slOutput: TStringList;
        begin

           if ( sFileName <> '') then begin
             slOutput:= TStringList.Create;
             with Self do begin
               try
                 slOutput.Add(sSourceFieldName);
                 slOutput.Add(IntToStr(iSourceFieldValue));
                 slOutput.Add(IntToStr(iCount));
                 slOutput.Add(IntToStr(k));

                 for i:= 1 to k do
                   slOutput.Add(FloatToStr(Mins[i]));

                 for i:= 1 to k do
                   slOutput.Add(FloatToStr(Maxs[i]));

                 for i:= 1 to k do
                   slOutput.Add(FloatToStr(Means[i]));

                 for i:= 1 to k do
                   slOutput.Add(FloatToStr(Variances[i]));

                 for i:= 1 to k do
                   for j:= 1 to k do
                     slOutput.Add(FloatToStr(Covariances[i,j]));

                 slOutput.Add('==========END===========');

                 slOutput.SaveToFile(sFileName);

               finally
                 slOutput.Free;
               end; // try - finally - end
             end; // with Self
           end; // if

        end;

        (*****************************************************************************
        * Name     :  [TSignature] LoadFromFile
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  Load contents of a signature from a previously saved file
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure TSignature.LoadFromFile(const sFileName: string);
        var
         slInput: TStringList;
         iStrCnt: integer;
         i,j: integer;
        begin

           if ( sFileName <> '') then begin
             slInput:= TStringList.Create;
             slInput.LoadFromFile(sFileName);
             iStrCnt:= 0;

             with Self do begin
               ClearSignature;

               try
                 sSourceFieldName  := slInput[iStrCnt];
                 inc(iStrCnt);

                 iSourceFieldValue := StrToInt(slInput[iStrCnt]);
                 inc(iStrCnt);

                 iCount            := StrToInt(slInput[iStrCnt]);
                 inc(iStrCnt);

                 k                 := StrToInt(slInput[iStrCnt]);
                 inc(iStrCnt);

                 for i:= 1 to k do begin
                   Mins[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do begin
                   Maxs[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do begin
                   Means[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do begin
                   Variances[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do
                   for j:= 1 to k do begin
                     Covariances[i,j]:= StrToFloat(slInput[iStrCnt]);
                     inc(iStrCnt);
                   end; // begin

               finally
                 slInput.Free;
               end; // try - finally - end

             end; // with Self
           end; // if

        end;


        // TSignatureList Methods=====================================================


        (*****************************************************************************
        * Name     :  [TSignatureList].Create
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  Constructor
        * Result   :
        * History  :  New
        ******************************************************************************)
        constructor TSignatureList.Create;
        begin
          inherited Create;
          iCount:= 0;
        end;

        (*****************************************************************************
        * Name     :  [TSignatureList] SaveToFile
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  Save every signature in array to output text files
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure TSignatureList.SaveToFile (const sFileName: string); // save to simple ASCII
        var
         i,a,b,z:integer;
         slOutput: TStringList;
        begin

           if ( sFileName <> '' ) then begin
             slOutput:= TStringList.Create;

             with Self do begin
               try

                 for z:= 1 to iCount do begin
                   slOutput.Add(Signatures[z].sSourceFieldName);
                   slOutput.Add(IntToStr(Signatures[z].iSourceFieldValue));
                   slOutput.Add(IntToStr(Signatures[z].iCount));
                   slOutput.Add(IntToStr(Signatures[z].k));

                   for i:= 1 to Signatures[z].k do
                     slOutput.Add(FloatToStr(Signatures[z].Mins[i]));

                   for i:= 1 to Signatures[z].k do
                     slOutput.Add(FloatToStr(Signatures[z].Maxs[i]));

                   for i:= 1 to Signatures[z].k do
                     slOutput.Add(FloatToStr(Signatures[z].Means[i]));

                   for i:= 1 to Signatures[z].k do
                     slOutput.Add(FloatToStr(Signatures[z].Variances[i]));

                   for a:= 1 to Signatures[z].k do
                     for b:= 1 to Signatures[z].k do
                       slOutput.Add(FloatToStr(Signatures[z].Covariances[a,b]));

                   slOutput.Add('==========END===========');
                 end; // for i

                 slOutput.SaveToFile(sFileName);

               finally
                 slOutput.Free;
               end; // try - finally - end

             end; // with Self
           end; // if

        end;

        (*****************************************************************************
        * Name     :  [TSignatureList] LoadFromFile
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  >>Load a set of signatures into the SignatureList object (TBA)<<
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure TSignatureList.LoadFromFile(const sFileName: string);
        //var aSig: TSignature;
        begin
        (*
           if ( sFileName <> '') then begin

             slInput := TStringList.Create;
             slInput.LoadFromFile(sFileName);

             iStrCnt := 0;
             aSig    := TSignature.Create;

             with Self do begin

               try
                 sSourceFieldName  := slInput[iStrCnt];
                 inc(iStrCnt);

                 iSourceFieldValue := StrToInt(slInput[iStrCnt]);
                 inc(iStrCnt);

                 Count             := StrToInt(slInput[iStrCnt]);
                 inc(iStrCnt);

                 k                 := StrToInt(slInput[iStrCnt]);
                 inc(iStrCnt);

                 for i:= 1 to k do begin
                   Mins[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do begin
                   Maxs[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do begin
                   Means[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do begin
                   Variances[i]:= StrToFloat(slInput[iStrCnt]);
                   inc(iStrCnt);
                 end; // end

                 for i:= 1 to k do
                   for j:= 1 to k do begin
                     Covariances[i,j]:= StrToFloat(slInput[iStrCnt]);
                     inc(iStrCnt);
                   end; // end

               finally
                 slInput.Free;
               end; // try - finally - end

             end; // with Self

           end; // if   *)

        end;

        (*****************************************************************************
        * Name     :  [TSignatureList] AddSignature
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  Add a signature into the SignatureList, inc counter
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure TSignatureList.AddSignature(const sigSignature: TSignature);
        begin
          inc(iCount);
          Move(sigSignature,Self.Signatures[iCount],sizeof(sigSignature));
        end;

        (*****************************************************************************
        * Name     :  [TSignatureList] DeleteSignature
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :  "Delete" a signature (by ID) from SignatureList
        * Result   :  Boolean
        * History  :  New
        ******************************************************************************)
        function TSignatureList.DeleteSignature(const iSignatureID: integer): boolean;
        var
         i: integer;
         iTemp: integer;
         sigTemp: TSignature;
        begin
          Result:= TRUE;

          if ( iSignatureID > -1 ) then begin
            sigTemp  := TSignature.Create;

            try
              try
                iTemp := iSignatureID;
                for i:= iTemp to iCount do begin
                  sigTemp:= Signatures[iTemp+1];
                  Signatures[iTemp]:= sigTemp;
                end; // begin
                dec(Self.iCount);

              except
                Result:= FALSE;
              end; // try - except
            finally
              sigTemp.Free;
            end; // try - finally

          end; // if ()

        end;


        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  Cube
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function Cube ( const dValue : double ): extended;
        begin
          Result:= ( dValue * dValue ) * dValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  Permutations
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function Permutations ( const TotalNumber : byte;
                                const SubGroup    : byte): extended;

        var
          T1 : extended;
          T2 : extended;
        begin
          Result:= 0;

          if ( TotalNumber in [1..25] ) and
             ( SubGroup in [1..24] ) and
             ( TotalNumber > SubGroup ) then begin

            T1:= Factorial( TotalNumber );
            T2:= Factorial( TotalNumber - SubGroup );

            if ( abs(T2) < 0.01 ) then begin
              Result:= 0;
              MessageDlg ('Error calculating permutations',mtError,[mbOK],0);
            end // if ()
            else
              result:= T1 / T2;

          end; // if ( TotalNumber )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  Combinations
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function Combinations ( const TotalNumber : byte;
                                const SubGroup    : byte): extended;

        var
          T1 : extended;
          T2 : extended;
          T3 : extended;
        begin
          Result:= 0;

          if ( TotalNumber in [1..25] ) and
             ( SubGroup in [1..24] ) and
             ( TotalNumber > SubGroup ) then begin

            T1:= Factorial( TotalNumber );
            T2:= Factorial( TotalNumber - SubGroup );
            T3:= Factorial( Subgroup );

            if ( abs(T2) < 0.01 ) or ( abs(T3) < 0.01 ) then begin
              Result:= 0;
              MessageDlg ('Error calculating combinations',mtError,[mbOK],0);
            end  // if ( abs )
            else
              result:= T1 / ( T2 * T3 );

          end; // if ( TotalNumber )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  Factorial
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function Factorial ( const IntIn : byte): extended;

        type
          TFactorials = array [1..25] of extended;
        var
          Factorials  : TFactorials;
        begin
          Result:= 0;

          if ( IntIn in [1..25] ) then begin
            Factorials[1]  :=                          1.0;
            Factorials[2]  :=                          2.0;
            Factorials[3]  :=                          6.0;
            Factorials[4]  :=                         24.0;
            Factorials[5]  :=                        120.0;
            Factorials[6]  :=                        720.0;
            Factorials[7]  :=                       5040.0;
            Factorials[8]  :=                      40320.0;
            Factorials[9]  :=                     362880.0;
            Factorials[10] :=                    3628800.0;
            Factorials[11] :=                   39916800.0;
            Factorials[12] :=                  479001600.0;
            Factorials[13] :=                 6227020800.0;
            Factorials[14] :=                87178291200.0;
            Factorials[15] :=              1307674368000.0;
            Factorials[16] :=             20922789888000.0;
            Factorials[17] :=            355687428096000.0;
            Factorials[18] :=           6402373705728000.0;
            Factorials[19] :=         121645100408800000.0;
            Factorials[20] :=        2432902008177000000.0;
            Factorials[21] :=       51090942171710000000.0;
            Factorials[22] :=     1124000727778000000000.0;
            Factorials[23] :=    25852016738880000000000.0;
            Factorials[24] :=   620448401733200000000000.0;
            Factorials[25] := 15511210043330000000000000.0;

            // Only 25! handled at present.
            Result:= Factorials[IntIn];
          end;

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :
        * Result   :  Int
        * History  :  New
        ******************************************************************************)
        function GetVector( const PRegressData  : PTRegressDataArray;
                            const iRecCnt       : integer;
                            const iVars         : integer): XVector;
        var
          i:integer;
        begin
          Result:= InitVector(iVars,0);

          for i:= 1 to iVars do
            Result[i]:= PRegressData[i,iRecCnt];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :
        * Result   :  Int
        * History  :  New
        ******************************************************************************)
        function FindSmallestPos ( const Arr     : array of double;
                                   const iSigCnt : integer): integer;
        var
          i:integer;
          iMinPos: integer;
          dMinValue: double;
        begin
          iMinPos   := -1;
          dMinValue := 1e20;

          for i:= 0 to iSigCnt-1 do begin
            if ( Arr[i] < dMinValue ) and ( Arr[i] > 0 ) then begin
              dMinValue:= Arr[i];
              iMinPos:= i;
            end; // if
          end; // for i

          Result:= iMinPos+1;
        end; // function FindSmallestDistance

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :
        * Result   :  Int
        * History  :  New
        ******************************************************************************)
        function FindLargestPos ( const Arr     : array of double;
                                  const iSigCnt : integer): integer;
        var
          i:integer;
          iMaxPos: integer;
          dMaxValue: double;
        begin
          iMaxPos   := -1;
          dMaxValue := -1e20;

          for i:= 1 to iSigCnt do begin
            if ( Arr[i] > dMaxValue ) then begin
              dMaxValue:= Arr[i];
              iMaxPos:= i;
            end; // if
          end; // for i

          Result:= iMaxPos;
        end; // function FindSmallestDistance

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/25/03
        * Purpose  :
        * Result   :  Int
        * History  :  New
        ******************************************************************************)
        function IsInPiped( const vecVector     : XVector;
                            const vecMeans      : XVector;
                            const vecVariances  : XVector;
                            const k             : integer;
                            const dDist         : double): boolean;
        var
          i:integer;
          ppdRange: TIntervalRec;
        begin
          Result:= TRUE;

          for i:= 1 to k do begin
            ppdRange:= MakePiped(vecMeans[i],sqrt(vecVariances[i]),dDist);
            with ppdRange do begin
              if ( vecVector[i] < Lower ) or ( vecVector[i] > Upper ) then begin
                Result:= FALSE;
                EXIT;
              end; // if (vec)
            end; // with
          end; // for i

        end; // function IsInPiped

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  Nicely format a field name; Upper[1] + Lower[2..n] = result
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function CaseFormat(const aString: string): string;
        var
          sFirstChar,s: string;
        begin
          sFirstChar  := UpperCase(aString);
          s           := Copy(sFirstChar,1,1);
          Result      := LowerCase(aString);
          Delete(Result,1,1);
          Insert(s,Result,1);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function MaxVariablesAllowed:integer;
        begin
          Result:= EdburgMaxVariables;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function MaxObservationsAllowed:integer;
        begin
          Result:= EdburgGeneralFuncsMaxObservations;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/02
        * Purpose  :  Destroy
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function MaxClustersAllowed:integer;
        begin
          Result:= MaxClusters;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitMultStatRec
        * Result   :  TMultStatRec
        * History  :  New
        ******************************************************************************)
        function InitMultStatRec: TMultStatRec;
var
  //i : TMultStatRec;
  j : integer;
        //var i:byte;

        begin

          with Result do begin
            N            := 0;
            k            := 0;
            R            := 0;
            R2           := 0;
            FStat        := 0;
            RMSE         := 0;
            MSE          := 0;
            MSR          := 0;
            SSE          := 0;
            SSR          := 0;
            SST          := 0;
            S2           := 0;
            S            := 0;
            DFModel      := 0;
            DFError      := 0;
            DFTotal      := 0;

            // Below is old way - EdburgMaxVariables doesn't matter
            // in using this method
            { for i:= 1 to EdburgMaxVariables+1 do begin
              Beta[i]          := 0;
              BetaCIUpper[i]   := 0;
              BetaCILower[i]   := 0;
              SeBeta[i]        := 0;
              TValue[i]        := 0;
            end; // for i:= 1 to EdburgMaxVariables+1 }


              for j := 1 to 15 do begin
                 Beta[j] := 0;
                 BetaCIUpper[j] := 0;
                 BetaCILower[j] := 0;
                 SeBeta[j] := 0;
                 TValue[j] := 0;
              end;

(*
              Beta[1]   := 0;
              Beta[2]   := 0;
              Beta[3]   := 0;
              Beta[4]   := 0;
              Beta[5]   := 0;
              Beta[6]   := 0;
              Beta[7]   := 0;
              Beta[8]   := 0;
              Beta[9]   := 0;
              Beta[10]  := 0;
              Beta[11]  := 0;
              Beta[12]  := 0;
              Beta[13]  := 0;
              Beta[14]  := 0;
              Beta[15]  := 0;

              BetaCIUpper[1]  := 0;
              BetaCIUpper[2]  := 0;
              BetaCIUpper[3]  := 0;
              BetaCIUpper[4]  := 0;
              BetaCIUpper[5]  := 0;
              BetaCIUpper[6]  := 0;
              BetaCIUpper[7]  := 0;
              BetaCIUpper[8]  := 0;
              BetaCIUpper[9]  := 0;
              BetaCIUpper[10] := 0;
              BetaCIUpper[11] := 0;
              BetaCIUpper[12] := 0;
              BetaCIUpper[13] := 0;
              BetaCIUpper[14] := 0;
              BetaCIUpper[15] := 0;

              BetaCILower[1]  := 0;
              BetaCILower[2]  := 0;
              BetaCILower[3]  := 0;
              BetaCILower[4]  := 0;
              BetaCILower[5]  := 0;
              BetaCILower[6]  := 0;
              BetaCILower[7]  := 0;
              BetaCILower[8]  := 0;
              BetaCILower[9]  := 0;
              BetaCILower[10] := 0;
              BetaCILower[11] := 0;
              BetaCILower[12] := 0;
              BetaCILower[13] := 0;
              BetaCILower[14] := 0;
              BetaCILower[15] := 0;

              SeBeta[1]  := 0;
              SeBeta[2]  := 0;
              SeBeta[3]  := 0;
              SeBeta[4]  := 0;
              SeBeta[5]  := 0;
              SeBeta[6]  := 0;
              SeBeta[7]  := 0;
              SeBeta[8]  := 0;
              SeBeta[9]  := 0;
              SeBeta[10] := 0;
              SeBeta[11] := 0;
              SeBeta[12] := 0;
              SeBeta[13] := 0;
              SeBeta[14] := 0;
              SeBeta[15] := 0;

              TValue[1]  := 0;
              TValue[2]  := 0;
              TValue[3]  := 0;
              TValue[4]  := 0;
              TValue[5]  := 0;
              TValue[6]  := 0;
              TValue[7]  := 0;
              TValue[8]  := 0;
              TValue[9]  := 0;
              TValue[10] := 0;
              TValue[11] := 0;
              TValue[12] := 0;
              TValue[13] := 0;
              TValue[14] := 0;
              TValue[15] := 0;
*)
          end; // with aMultStatRec

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitInterval
        * Result   :  TIntervalRec
        * History  :  New
        ******************************************************************************)
        function InitInterval: TIntervalRec;
        begin
          with Result do begin
            Upper:= 0;
            Lower:= 0;
          end; // with Result
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitBivarStatRec
        * Result   :
        * History  :  New
        ******************************************************************************)
        function InitBivarStatRec: TBivarStatRec;
        begin
          with Result do begin
            N       := 0;
            r       := 0;
            XMean   := 0;
            YMean   := 0;
            Beta0   := 0;
            Beta1   := 0;
            SSXX    := 0;
            SSYY    := 0;
            SSXY    := 0;
            SumX    := 0;
            SumY    := 0;
            SumXY   := 0;
            SumX2   := 0;
            SumY2   := 0;
            df      := 0;
          end; // with BVStatRec
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitIntArray
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        procedure InitIntArray (var AnArray: array of integer; const aValue: integer);
        var
          i: integer;
        begin
          for i:= 0 to EdburgGeneralFuncsMaxObservations-1 do AnArray[i]:= aValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitResidArray
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        procedure InitResidArray (var AnArray: array of double; const aValue: integer);
        var
          i: integer;
        begin
          for i:= 0 to EdburgGeneralFuncsMaxObservations-1 do AnArray[i]:= aValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/29/03
        * Purpose  :  InitErrorMatrix
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure InitErrorMatrix ( var emat: TErrorMatrix);
        var
          i,j:integer;
        begin
          for i:= 0 to MAXSIGNATURES+2 do
            for j:= 0 to MAXSIGNATURES+2 do
              emat[i,j]:= 0;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/29/03
        * Purpose  :  InitKappaRec
        * Result   :  TKappaRec
        * History  :  New
        ******************************************************************************)
        function InitKappaRec: TKappaRec;
        begin
          with Result do begin
            ObsGrandTotal   := 0;
            ObsTotalCorrect := 0;
            ObsPctCorrect   := 0;
            ExpPctCorrect   := 0;
            ExpGrandTotal   := 0;
            ExpTotalCorrect := 0;
            Kappa           := 0;
          end; // with
        end; // function

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/6/03
        * Purpose  :  ChiSqrTableValue
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function ChiSqrTableValue (const n     : integer;
                                   const Alpha : single): single;

        type
          TChiSqrRec = record
                         df  : byte;
                         t05 : single;
                         t01 : single;
                       end;

        var
          ChiArr : array [0..30] of TChiSqrRec;

        begin
          result:= 0;

                ChiArr[1].df:= 1;
                ChiArr[1].t05:= 3.841;
                ChiArr[1].t01:= 6.635;

                ChiArr[2].df:= 2;
                ChiArr[2].t05:=  5.991;
                ChiArr[2].t01:=  9.210;

                ChiArr[3].df:= 3;
                ChiArr[3].t05:=  7.815;
                ChiArr[3].t01:=  11.345;

                ChiArr[4].df:= 4;
                ChiArr[4].t05:=  9.488;
                ChiArr[4].t01:= 13.277;

                ChiArr[5].df:= 5;
                ChiArr[5].t05:=  11.070;
                ChiArr[5].t01:=  15.086;

                ChiArr[6].df:= 6;
                ChiArr[6].t05:=  12.592;
                ChiArr[6].t01:=  16.812;

                ChiArr[7].df:= 7;
                ChiArr[7].t05:=  14.067;
                ChiArr[7].t01:=  18.475;

                ChiArr[8].df:= 8;
                ChiArr[8].t05:=  15.507;
                ChiArr[8].t01:=  20.090;

                ChiArr[9].df:= 9;
                ChiArr[9].t05:=  16.919;
                ChiArr[9].t01:=  21.666;

                ChiArr[10].df:= 10;
                ChiArr[10].t05:= 18.307;
                ChiArr[10].t01:= 23.209;

                ChiArr[11].df:= 11;
                ChiArr[11].t05:= 19.675;
                ChiArr[11].t01:= 24.725;

                ChiArr[12].df := 12;
                ChiArr[12].t05:= 21.026;
                ChiArr[12].t01:= 26.217;

                ChiArr[13].df:= 13;
                ChiArr[13].t05:= 22.362;
                ChiArr[13].t01:= 27.688;

                ChiArr[14].df := 14;
                ChiArr[14].t05:= 23.685;
                ChiArr[14].t01:= 29.141;

                ChiArr[15].df:= 15;
                ChiArr[15].t05:= 24.996;
                ChiArr[15].t01:= 30.578;

                ChiArr[16].df:= 16;
                ChiArr[16].t05:= 26.296;
                ChiArr[16].t01:= 32.000;

                ChiArr[17].df := 17;
                ChiArr[17].t05:= 27.587;
                ChiArr[17].t01:= 33.409;

                ChiArr[18].df:= 18;
                ChiArr[18].t05:= 28.869;
                ChiArr[18].t01:= 34.805;

                ChiArr[19].df:= 19;
                ChiArr[19].t05:= 30.144;
                ChiArr[19].t01:= 36.191;

                ChiArr[20].df := 20;
                ChiArr[20].t05:= 31.410;
                ChiArr[20].t01:= 37.566;

                ChiArr[21].df:= 21;
                ChiArr[21].t05:= 32.671;
                ChiArr[21].t01:= 38.932;

                ChiArr[22].df:= 22;
                ChiArr[22].t05:= 33.924;
                ChiArr[22].t01:= 40.289;

                ChiArr[23].df:= 23;
                ChiArr[23].t05:= 35.172;
                ChiArr[23].t01:= 41.638;

                ChiArr[24].df := 24;
                ChiArr[24].t05:= 36.415;
                ChiArr[24].t01:= 42.98;

                ChiArr[25].df := 25;
                ChiArr[25].t05:= 37.652;
                ChiArr[25].t01:= 44.314;

                ChiArr[26].df := 26;
                ChiArr[26].t05:= 38.885;
                ChiArr[26].t01:= 45.642;

                ChiArr[27].df:= 27;
                ChiArr[27].t05:= 40.113;
                ChiArr[27].t01:= 46.963;

                ChiArr[28].df:= 28;
                ChiArr[28].t05:= 41.337;
                ChiArr[28].t01:= 48.278;

                ChiArr[29].df:= 29;
                ChiArr[29].t05:= 42.557;
                ChiArr[29].t01:= 49.588;

                ChiArr[30].df:= 30;
                ChiArr[30].t05:= 43.773;
                ChiArr[30].t01:= 50.892;

          if (round(Alpha * 1000) = 10) then result:= ChiArr[n].t01;
          if (round(Alpha * 1000) = 50) then result:= ChiArr[n].t05;

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  ChiSqrTableValue
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function CalcChiSquare (const Y1 : array of double;
                                const Y2 : array of double;
                                const n  : integer): extended;

        var i    : integer;
            CSqr : extended;
            Tmp  : extended;

        begin
          Result:=0;
          CSqr:=0;

          if ( n > 2 ) and ( n < EdburgGeneralFuncsMaxObservations ) then begin
            for i:= 0 to n-1 do begin        // X: obs, Y: expected
              if (Y1[i] > 0 ) then begin
                Tmp:= Sqr(Y1[i] - Y2[i]);
                CSqr:= CSqr + (Tmp / Y1[i]);
              end; // if (Y1)
            end; // for i

            Result:= CSqr;
          end; // if

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  ZTableValue
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function ZTableValue (const aZ: extended): extended;

        type
          ZArray = array [0..310] of single;
        var
          TmpIndx : integer;
          TmpVal  : extended;
          Z       : ZArray;
        begin
        //{$Include z.inc}

                Z[0]:= 0.0000;
                Z[1]:= 0.0040;
                Z[2]:= 0.0080;
                Z[3]:= 0.0120;
                Z[4]:= 0.0160;
                Z[5]:= 0.0199;
                Z[6]:= 0.0239;
                Z[7]:= 0.0279;
                Z[8]:= 0.0319;
                Z[9]:= 0.0359;

                Z[10]:= 0.0398;
                Z[11]:= 0.0438;
                Z[12]:= 0.0478;
                Z[13]:= 0.0517;
                Z[14]:= 0.0557;
                Z[15]:= 0.0596;
                Z[16]:= 0.0636;
                Z[17]:= 0.0675;
                Z[18]:= 0.0714;
                Z[19]:= 0.0753;

                Z[20]:= 0.0793;
                Z[21]:= 0.0832;
                Z[22]:= 0.0871;
                Z[23]:= 0.0910;
                Z[24]:= 0.0948;
                Z[25]:= 0.0987;
                Z[26]:= 0.1026;
                Z[27]:= 0.1064;
                Z[28]:= 0.1103;
                Z[29]:= 0.1141;

                Z[30]:= 0.1179;
                Z[31]:= 0.1217;
                Z[32]:= 0.1255;
                Z[33]:= 0.1293;
                Z[34]:= 0.1331;
                Z[35]:= 0.1368;
                Z[36]:= 0.1406;
                Z[37]:= 0.1443;
                Z[38]:= 0.1480;
                Z[39]:= 0.1517;

                Z[40]:= 0.1554;
                Z[41]:= 0.1591;
                Z[42]:= 0.1628;
                Z[43]:= 0.1664;
                Z[44]:= 0.1700;
                Z[45]:= 0.1736;
                Z[46]:= 0.1772;
                Z[47]:= 0.1808;
                Z[48]:= 0.1844;
                Z[49]:= 0.1879;

                Z[50]:= 0.1915;
                Z[51]:= 0.1950;
                Z[52]:= 0.1985;
                Z[53]:= 0.2019;
                Z[54]:= 0.2054;
                Z[55]:= 0.2088;
                Z[56]:= 0.2123;
                Z[57]:= 0.2157;
                Z[58]:= 0.2190;
                Z[59]:= 0.2224;

                Z[60]:= 0.2257;
                Z[61]:= 0.2291;
                Z[62]:= 0.2324;
                Z[63]:= 0.2357;
                Z[64]:= 0.2389;
                Z[65]:= 0.2422;
                Z[66]:= 0.2454;
                Z[67]:= 0.2486;
                Z[68]:= 0.2517;
                Z[69]:= 0.2549;

                Z[70]:= 0.2580;
                Z[71]:= 0.2611;
                Z[72]:= 0.2642;
                Z[73]:= 0.2673;
                Z[74]:= 0.2704;
                Z[75]:= 0.2734;
                Z[76]:= 0.2764;
                Z[77]:= 0.2794;
                Z[78]:= 0.2823;
                Z[79]:= 0.2852;

                Z[80]:= 0.2881;
                Z[81]:= 0.2910;
                Z[82]:= 0.2939;
                Z[83]:= 0.2967;
                Z[84]:= 0.2995;
                Z[85]:= 0.3023;
                Z[86]:= 0.3051;
                Z[87]:= 0.3078;
                Z[88]:= 0.3106;
                Z[89]:= 0.3133;

                Z[90]:= 0.3159;
                Z[91]:= 0.3186;
                Z[92]:= 0.3212;
                Z[93]:= 0.3238;
                Z[94]:= 0.3264;
                Z[95]:= 0.3289;
                Z[96]:= 0.3315;
                Z[97]:= 0.3340;
                Z[98]:= 0.3365;
                Z[99]:= 0.3389;

                Z[100]:= 0.3413;
                Z[101]:= 0.3438;
                Z[102]:= 0.3461;
                Z[103]:= 0.3485;
                Z[104]:= 0.3508;
                Z[105]:= 0.3531;
                Z[106]:= 0.3554;
                Z[107]:= 0.3577;
                Z[108]:= 0.3599;
                Z[109]:= 0.3621;

                Z[110]:= 0.3643;
                Z[111]:= 0.3665;
                Z[112]:= 0.3686;
                Z[113]:= 0.3708;
                Z[114]:= 0.3729;
                Z[115]:= 0.3749;
                Z[116]:= 0.3770;
                Z[117]:= 0.3790;
                Z[118]:= 0.3810;
                Z[119]:= 0.3830;

                Z[120]:= 0.3849;
                Z[121]:= 0.3869;
                Z[122]:= 0.3888;
                Z[123]:= 0.3907;
                Z[124]:= 0.3925;
                Z[125]:= 0.3944;
                Z[126]:= 0.3962;
                Z[127]:= 0.3980;
                Z[128]:= 0.3997;
                Z[129]:= 0.4015;

                Z[130]:= 0.4032;
                Z[131]:= 0.4049;
                Z[132]:= 0.4066;
                Z[133]:= 0.4082;
                Z[134]:= 0.4099;
                Z[135]:= 0.4115;
                Z[136]:= 0.4131;
                Z[137]:= 0.4147;
                Z[138]:= 0.4162;
                Z[139]:= 0.4177;

                Z[140]:= 0.4192;
                Z[141]:= 0.4207;
                Z[142]:= 0.4222;
                Z[143]:= 0.4236;
                Z[144]:= 0.4251;
                Z[145]:= 0.4265;
                Z[146]:= 0.4279;
                Z[147]:= 0.4292;
                Z[148]:= 0.4306;
                Z[149]:= 0.4319;

                Z[150]:= 0.4332;
                Z[151]:= 0.4345;
                Z[152]:= 0.4357;
                Z[153]:= 0.4370;
                Z[154]:= 0.4382;
                Z[155]:= 0.4394;
                Z[156]:= 0.4406;
                Z[157]:= 0.4418;
                Z[158]:= 0.4429;
                Z[159]:= 0.4441;

                Z[160]:= 0.4452;
                Z[161]:= 0.4463;
                Z[162]:= 0.4474;
                Z[163]:= 0.4484;
                Z[164]:= 0.4495;
                Z[165]:= 0.4505;
                Z[166]:= 0.4515;
                Z[167]:= 0.4525;
                Z[168]:= 0.4535;
                Z[169]:= 0.4545;

                Z[170]:= 0.4554;
                Z[171]:= 0.4564;
                Z[172]:= 0.4573;
                Z[173]:= 0.4582;
                Z[174]:= 0.4591;
                Z[175]:= 0.4599;
                Z[176]:= 0.4608;
                Z[177]:= 0.4616;
                Z[178]:= 0.4625;
                Z[179]:= 0.4633;

                Z[180]:= 0.4641;
                Z[181]:= 0.4649;
                Z[182]:= 0.4656;
                Z[183]:= 0.4664;
                Z[184]:= 0.4671;
                Z[185]:= 0.4678;
                Z[186]:= 0.4686;
                Z[187]:= 0.4693;
                Z[188]:= 0.4699;
                Z[189]:= 0.4706;

                Z[190]:= 0.4713;
                Z[191]:= 0.4719;
                Z[192]:= 0.4726;
                Z[193]:= 0.4732;
                Z[194]:= 0.4738;
                Z[195]:= 0.4744;
                Z[196]:= 0.4750;
                Z[197]:= 0.4756;
                Z[198]:= 0.4761;
                Z[199]:= 0.4767;

                Z[200]:= 0.4772;
                Z[201]:= 0.4778;
                Z[202]:= 0.4783;
                Z[203]:= 0.4788;
                Z[204]:= 0.4793;
                Z[205]:= 0.4798;
                Z[206]:= 0.4803;
                Z[207]:= 0.4808;
                Z[208]:= 0.4812;
                Z[209]:= 0.4817;

                Z[210]:= 0.4821;
                Z[211]:= 0.4826;
                Z[212]:= 0.4830;
                Z[213]:= 0.4834;
                Z[214]:= 0.4838;
                Z[215]:= 0.4842;
                Z[216]:= 0.4846;
                Z[217]:= 0.4850;
                Z[218]:= 0.4854;
                Z[219]:= 0.4857;

                Z[220]:= 0.4861;
                Z[221]:= 0.4864;
                Z[222]:= 0.4868;
                Z[223]:= 0.4871;
                Z[224]:= 0.4875;
                Z[225]:= 0.4878;
                Z[226]:= 0.4881;
                Z[227]:= 0.4884;
                Z[228]:= 0.4887;
                Z[229]:= 0.4890;

                Z[230]:= 0.4893;
                Z[231]:= 0.4896;
                Z[232]:= 0.4898;
                Z[233]:= 0.4901;
                Z[234]:= 0.4904;
                Z[235]:= 0.4906;
                Z[236]:= 0.4909;
                Z[237]:= 0.4911;
                Z[238]:= 0.4913;
                Z[239]:= 0.4916;

                Z[240]:= 0.4918;
                Z[241]:= 0.4920;
                Z[242]:= 0.4922;
                Z[243]:= 0.4925;
                Z[244]:= 0.4927;
                Z[245]:= 0.4929;
                Z[246]:= 0.4931;
                Z[247]:= 0.4932;
                Z[248]:= 0.4934;
                Z[249]:= 0.4936;

                Z[250]:= 0.4938;
                Z[251]:= 0.4940;
                Z[252]:= 0.4941;
                Z[253]:= 0.4943;
                Z[254]:= 0.4945;
                Z[255]:= 0.4946;
                Z[256]:= 0.4948;
                Z[257]:= 0.4949;
                Z[258]:= 0.4951;
                Z[259]:= 0.4952;

                Z[260]:= 0.4953;
                Z[261]:= 0.4955;
                Z[262]:= 0.4956;
                Z[263]:= 0.4957;
                Z[264]:= 0.4959;
                Z[265]:= 0.4960;
                Z[266]:= 0.4961;
                Z[267]:= 0.4962;
                Z[268]:= 0.4963;
                Z[269]:= 0.4964;

                Z[270]:= 0.4965;
                Z[271]:= 0.4966;
                Z[272]:= 0.4967;
                Z[273]:= 0.4968;
                Z[274]:= 0.4969;
                Z[275]:= 0.4970;
                Z[276]:= 0.4971;
                Z[277]:= 0.4972;
                Z[278]:= 0.4973;
                Z[279]:= 0.4974;

                Z[280]:= 0.4974;
                Z[281]:= 0.4975;
                Z[282]:= 0.4976;
                Z[283]:= 0.4977;
                Z[284]:= 0.4977;
                Z[285]:= 0.4978;
                Z[286]:= 0.4979;
                Z[287]:= 0.4979;
                Z[288]:= 0.4980;
                Z[289]:= 0.4981;

                Z[290]:= 0.4981;
                Z[291]:= 0.4982;
                Z[293]:= 0.4982;
                Z[294]:= 0.4983;
                Z[295]:= 0.4984;
                Z[296]:= 0.4984;
                Z[297]:= 0.4985;
                Z[298]:= 0.4985;
                Z[299]:= 0.4986;
                Z[300]:= 0.4986;

                Z[301]:= 0.4987;
                Z[302]:= 0.4987;
                Z[303]:= 0.4987;
                Z[304]:= 0.4988;
                Z[305]:= 0.4988;
                Z[306]:= 0.4989;
                Z[307]:= 0.4989;
                Z[308]:= 0.4989;
                Z[309]:= 0.4990;
                Z[310]:= 0.4990;

          if ( aZ < -3.1 ) then  TmpVal:= -3.1
          else if ( aZ > 3.1 ) then TmpVal:= 3.1
          else TmpVal:= aZ;
          TmpIndx:= round(abs(TmpVal*100));     //  2.42 becomes ... 242

          result:= Z[TmpIndx];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/27/03
        * Purpose  :
        * Result   :  n/a
        * History  :  New
        ******************************************************************************)
        function MinSampleSizeTwoMeans ( const zOneMinusAlpha       : double;
                                         const zOneMinusBeta        : double;
                                         const dPopulationVariance  : double;
                                         const dSampleMeansDiff     : double): integer;
        var
         dTmp: double;
        begin
          dTmp  := sqr(zOneMinusAlpha + zOneMinusBeta);
          Result:= Round(((2 * dTmp * dPopulationVariance) / dSampleMeansDiff) + 0.5);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CalcT
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function CalcT ( const XBar : double;
                         const Meu  : double;
                         const S    : double;
                         const n    : integer): double;

        begin
          result:= 0;
          if ( S > 0 ) and ( n > 0 ) then
            result:= ( XBar - Meu ) / ( S * sqrt(n) )
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CalcF
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function CalcF ( const MSR : extended; const MSE : extended): double;
        begin
          result:= 0;
          if ( MSE > 0 ) then
            result:= MSR * ( 1 / MSE );
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CalcCp
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function CalcCp (const SSEp : double;
                         const MSEk : double;
                         const    p : byte;
                         const    n : integer): double;

        // p: subset of vars used in reduced model
        // k: set of vars used in maximum model
        begin
          if ( MSEk <> 0 ) then
            result:= (SSEp / MSEk) - ( n - ( 2 * ( p+1 ) ) )
          else
            result:= SSEp - ( n - ( 2 * ( p+1 ) ) )
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CalcFp
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function CalcFp (const R2p : double;
                         const R2k : double;
                         const   k : byte;
                         const   p : byte;
                         const   n : integer): double;

        var
          tmp1 : extended;
          tmp2 : extended;
        begin
          result:= 0;

          if ( ( k - p ) > 0 ) and ( ( n-k-1 ) <> 0 ) then begin
              tmp1    := ( R2k - R2p ) / ( k - p );
              tmp2    := ( 1 - R2k ) / ( n - k - 1 );
              result  := tmp1 / tmp2;
          end; // if ( ( k - p ) <> 0 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CalcMSEp
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function CalcMSEp (const SSEp : double;
                           const    p : byte;
                           const    n : integer): double;

        var
          iTmp: integer;
        begin
          result   := 0;
          iTmp     := n-p-1;
          if ( iTmp <> 0 ) then
              result:= SSEp * ( 1 / iTmp );
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  ProbabilityZIsHigher
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function ProbabilityZIsHigher (const aSD: extended): extended;
        var
          TheZValue: extended;
        begin
          result:= 0;
          TheZValue:= ZTableValue(aSD);

          if ( aSD < 0 ) then
            result:= 0.5 + TheZValue
          else if ( aSD > 0 ) then
            result:= 0.5 - TheZValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  ProbabilityZIsLowe
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function ProbabilityZIsLower (const aSD: extended): extended;
        var
          TheZValue: extended;
        begin
          result:= 0;
          TheZValue:= ZTableValue(aSD);

          if ( aSD < 0 ) then
            result:= 0.5 - TheZValue
          else if ( aSD > 0 ) then
            result:= 0.5 + TheZValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  ProbabilityZBetweenPlusMinus
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function ProbabilityZBetweenPlusMinus (const aSD: extended): extended;
        var
          TheZValue : extended;
        begin
          TheZValue:= ZTableValue(aSD); // the area under the z curve for upper half
          result:= TheZValue*2;         // double it for both sides of curve
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  BVRegression
        * Result   :  Return only correlation coefficient (r) in this short version
        * History  :  New
        ******************************************************************************)
        function BVRegressionShort (const  X : array of double;
                                    const  Y : array of double;
                                    const  n : integer): double;

        var
          Statistics : TBivarStatRec;
          i          : integer;
          dMult      : double;

        begin
          Result       := 0;
          Statistics   := InitBivarStatRec;      // Init stats
          Statistics.N := n;                     // N=size of input array
                                                 // This part goes slow
          if ( Statistics.N > 2 ) then begin     // Compute sum's and sum's sqr's
            with Statistics do begin             // by looping through each obs
              df    := 0;
              dMult := ( 1 / N );
              for i:= 0 to N-1 do begin
                SumX  := SumX + X[i];
                SumY  := SumY + Y[i];
                SumXY := SumXY + (X[i] * Y[i]);
                SumX2 := SumX2 + (X[i] * X[i]);
                SumY2 := SumY2 + (Y[i] * Y[i]);
              end; // for i:= 0 to N-1
            end; // with Statistics do               This goes quick

            with Statistics do begin             // Do final &
              SSXX  := SumX2 - (SumX * SumX) * ( dMult );
              SSYY  := SumY2 - (SumY * SumY) * ( dMult );
              SSXY  := SumXY - (SumX * SumY) * ( dMult );
              if ( SSXX * SSYY ) > 0 then
                Result:= SSXY * ( 1 / sqrt(SSXX * SSYY) ) // 'r'
              else begin
                Raise Exception.Create('Error calculating r, B1 or B0');
                Exit;
              end; // begin - end
            end; // with

          end; // if ( Statistics.N > 2 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CorrelationMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function CorrelationMatrix (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XMatrix;

        var
          i,j: byte;
        begin
          Result := InitMatrix(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin

            // Note: 'k' is the number of dimensions (variables) of the input data.
            for i:= 1 to k do
              for j:= 1 to k do begin
                if ( i > j ) then begin
                  Result[i,j]:= BVRegressionShort(PRegressData^[i],PRegressData^[j],n);
                  Result[j,i]:= Result[i,j];       // Mirror value (*)
                end; // if ( i>j) then
                Result[i,i]:= 1;
              end; // for j:= 1 to k

          end; // if ( n > 2 ) and ( k < n )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CovarianceMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function CovarianceMatrix (const PRegressData : PTRegressDataArray;
                                   const k            : byte;
                                   const n            : integer): XMatrix;
        var
          i,j        : byte;
          StdDevArr  : array [1..EdburgMaxVariables+1] of double;
        begin
          result:= InitMatrix(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
            for i:= 1 to EdburgMaxVariables+1 do
              StdDevArr[i]:= 0;

            Result:= CorrelationMatrix(PRegressData,k,n);

            for i:= 1 to k do
              StdDevArr[i]:= sqrt(GetVariance(PRegressData^[i],n));

            for i:= 1 to k do
              for j:= 1 to k do begin
                if (i>=j) then begin           // do every unique pair of x/y calcs:
                  Result[i,j]:= Result[i,j] * (StdDevArr[i] * StdDevArr[j]);
                  Result[j,i]:= Result[i,j];      // Mirror val.
                end // if (i>=j) then begin
              end; // for j:= 1 to k do begin
          end; // if ( n > 2 ) and ( k < n )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/02/03
        * Purpose  :  InverseCorrelationMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function InverseCorrelationMatrix (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): XMatrix;

        var
          matTemp: XMatrix;
          i,j: integer;
          dDeterm: double;
        begin
          matTemp := InitMatrix(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin

            // Note: 'k' is the number of dimensions (variables) of the input data.
            for i:= 1 to k do
              for j:= 1 to k do begin
                if ( i > j ) then begin
                  matTemp[i,j]:= BVRegressionShort(PRegressData^[i],PRegressData^[j],n);
                  matTemp[j,i]:= matTemp[i,j];       // Mirror value (*)
                end; // if ( i>j) then
                matTemp[i,i]:= 1;
              end; // for j:= 1 to k

              Result:= InvertMatrix(matTemp,k,dDeterm);
          end; // if ( n > 2 ) and ( k < n )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/02/03
        * Purpose  :  InverseCovarianceMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function InverseCovarianceMatrix  (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): XMatrix;
        var
          i,j        : byte;
          dDeterm    : double;
          StdDevArr  : array [1..EdburgMaxVariables+1] of double;
          matTemp    : XMatrix;
        begin
          matTemp:= InitMatrix(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
            for i:= 1 to EdburgMaxVariables+1 do
              StdDevArr[i]:= 0;

            matTemp:= CorrelationMatrix(PRegressData,k,n);

            for i:= 1 to k do
              StdDevArr[i]:= sqrt(GetVariance(PRegressData^[i],n));

            for i:= 1 to k do
              for j:= 1 to k do begin
                if (i>=j) then begin           // do every unique pair of x/y calcs:
                  matTemp[i,j]:= matTemp[i,j] * (StdDevArr[i] * StdDevArr[j]);
                  matTemp[j,i]:= matTemp[i,j];      // Mirror val.
                end // if (i>=j) then begin
              end; // for j:= 1 to k do begin

             Result:= InvertMatrix(matTemp,k,dDeterm);
          end; // if ( n > 2 ) and ( k < n )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/6/03
        * Purpose  :  MeanSmoothData
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        procedure MeanSmoothData (var AnArray       : array of double;
                                  const n           : integer;
                                  const iWindowSize : byte);

        var i        : integer;
            dSum     : double;
            TmpArray : TDoubleArray;
        begin

          if ( n > 2 ) and ( n < EdburgGeneralFuncsMaxObservations ) then begin

            for i:= 0 to n-1 do
              TmpArray[i] := AnArray[i];

              case iWindowSize of
               3: begin
                   for i:= 1 to n-2 do begin  // don't use first nor last element of array
                     dSum:= ( TmpArray[i-1]+
                              TmpArray[i]+
                              TmpArray[i+1] ) / 3;
                     AnArray[i]:= dSum;
                   end; // for i
               end; // 3
               5: begin
                   for i:= 2 to n-3 do begin  // don't use first nor last element of array
                     dSum:= (
                       TmpArray[i-2]+
                       TmpArray[i-1]+
                       TmpArray[i]+
                       TmpArray[i+1]+
                       TmpArray[i+2] ) / 5;
                     AnArray[i]:= dSum;
                   end; // for i
               end; // 5

            end; // case
          end; // if ()

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MVCheckXData
        * Result   :  boolean
        * History  :  New
        ******************************************************************************)
        function MVCheckXData (const PRegressData : PTRegressDataArray;
                               const k            : byte;
                               const n            : integer): boolean;

        var
          c          : integer;
          a,b        : byte;
          ZeroSums   : byte;
          ZeroDiffs  : byte;
          tmpnum     : extended;
          aSumVector : XVector;
        begin
          try
            ZeroSums   := 0;
            ZeroDiffs  := 0;
            ASumVector := InitVector(EdburgMaxVariables,0);

            if  ( k < 1 ) or ( k > EdburgMaxVariables ) or ( n > EdburgGeneralFuncsMaxObservations ) then begin
              Result:= FALSE;
              exit;
            end; // if ( k < 1 ) or ( n < k )

            // if the sum of a column = 0, then it is likely a problem for matrix inversion
            aSumVector:= SumVector(PRegressData,k,n);

            for a:= 1 to k do begin
              if ( abs ( aSumVector[a] ) < CLOSE_TO_ZERO ) then
                inc(ZeroSums);
            end; // for a:= 1 to k do

            // checks to see if any two sums of column values have a zero difference
            // if a 0 diff exists, it is likely that two columns have identical values
            // this also will likely be a problem for the matrix inversion
            for a:= 1 to k do begin
              for b:= 1 to k do begin
                if ( a > b ) then begin
                  tmpnum:= 0;

                  for c:= 0 to n-1 do
                    tmpnum:= tmpnum + (PRegressData^[a,c] - PRegressData^[b,c]);

                  if (abs(tmpnum) < 1) then
                    inc(ZeroDiffs);

                end; // if ( a > b )
              end; // for b:= 1 to k
            end; // for a:= 1 to k

            // The below checks to see how many columns (variables) in the input data
            // array summed to very near 0.0.  If two or more did actually sum to 0.0,
            // then this could very likely make it impossible to invert the X matrix
            // which is required in the MultiVarRegression routine...
            // So, catch it here first if needed.
            if ( ZeroSums > 1 ) or ( ZeroDiffs > 0 ) then
              result:= FALSE
            else
              result:= TRUE;

          except
            Raise Exception.Create(CheckDataError);
          end; // try - except - end

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CrossProducts
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function CrossProducts ( const PRegressData : PTRegressDataArray;
                                 const k            : byte;
                                 const n            : integer): XMatrix;

        var
          i,j   : byte;
          cnt   : integer;
          dsum  : extended;
        begin
          // This function computes (X'X); where k= vars, n=obs;
          // XTemp is a k+1 x k+1 matrix

          // It is non-std in how it approaches the matrix multiplication-
          // so I'll describe it in detail below:

          { Goal: To compute X' * X: The result is an k+1 by k+1 matrix (so
            for example, if 2 variables are used in the regression (k=2) then,
            a 3x3 results...(Note: the matrix subscript notation is defined as
            XTempInv[1..k+1,1..k+1] -the lowest subscript is based at 1, not 0.

                X'      *        X     =   (X'X) - a 3x3
            1  1  1  1        1  1  2       4  10  20
            1  2  3  4        1  2  4      10  30  60
            2  4  6  8        1  3  6      20  60 120
                              1  4  8

          I'll describe how I do the above by the each of 4 steps below:

          Step 1. Compute the along each of the top left corner (since, the first
          column is the filler column if all 1's, it is possible just to sum the
          columns of [x,1] or [1,x]

                                           X11 X21 X31
                   Compute these first:
                   (then, mirror the value
                   into X12, X13)

          Step 2. Compute the totals for the diagonal:

                                           X11
                                               X22
                   Compute these next:             X33


          Step 3. Now, get all the others which lie elsewhere: These are the off-
                  diagonal and not in the outer (top-left) edges...

                   Compute these next:
                   (mirror the value into          X32
                   X23)

          Step 4. Finally, stick in 'n' the number of obs. for X11 := n.   }

          Result := InitMatrix(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin // compute the outer rows/cols of the matrix
            for i := 1 to k do begin             // Step 1.
              dsum := 0;
              for cnt := 0 to n-1 do
                dsum := dsum + PRegressData^[i,cnt];
              Result[1,i+1]:= dsum;
              Result[i+1,1]:= dsum;
            end; // for i:= 1 to k

            // Total up those elements on the diagonal
            // for j:= 0 to n-1 do
            //   XTemp[i+1,i+1] = RegressData[i,j] * RegressData[i,j]
            for i := 1 to k do begin              // Step 2.
              dsum := 0;
              for cnt := 0 to n-1 do
                dsum := dsum + (PRegressData^[i,cnt] * PRegressData^[i,cnt]);
              Result[i+1,i+1]:= dsum;
            end; // for i:= 1 to k

            // Now do those elements which are neither on the
            // diagonal nor along the edges of the matrix
            for i := 1 to k do begin                 // Step 3.
              for j := 1 to k do begin
                if ( j > i ) then begin
                  dsum := 0;
                  for cnt := 0 to n-1 do begin
                    dsum := dsum + (PRegressData^[i,cnt] * PRegressData^[j,cnt]);
                  end; // for cnt := 0 to n-1
                  Result[i+1,j+1] := dsum;
                  Result[j+1,i+1] := dsum;
                end; // if ( j > 1 ) then begin
              end; // for j:= 1 to k
            end; // for i:= 1 to k
                                              // Step 4.
            Result[1,1]:= n; // XTemp[1,1] (upper-left) = n
          end; // if ( n > 2 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMidPt
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMidpt (const anArray: array of double; const n: integer): extended;
        begin
          result:= ( GetMax(anArray,n) + GetMin(anArray,n) ) / 2;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetZValue
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetZValue (const aValue  : extended;
                            const aMean   : extended;
                            const aStdDev : extended): extended;
        begin
          result:= 0;
          if ( aStdDev > 0 ) then
            result:= ( aValue - aMean ) * ( 1 / aStdDev );
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetCenterValue
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetCenterValue ( const aValue: extended; const aMean: extended): extended;
        begin
          result:= aValue - aMean;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMedian
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMedian (const AnArray: array of double; const n: integer): extended;

          function RankSort ( AnArr : array of double;
                              n     : integer): extended;

          var
             i,j: integer;
             t: double;
             iIndexNum: integer;

          begin

            for i := 1 to n do
              for j := n downto i+1 do
              if ( AnArr[i] > AnArr[j] ) then begin
                T        := AnArr[i];
                AnArr[i] := AnArr[j];
                AnArr[j] := T;
              end; // if ( AnArr[i] )

            if odd(n) then begin
              iIndexNum:= (n-1) div 2;
              result:= AnArr[iIndexNum];
            end // if odd(n) then
            else begin
              iIndexNum:= n div 2;
              iIndexNum:= iIndexNum - 1;
              result:= ( AnArr[iIndexNum] + AnArr[iIndexNum+1] ) / 2;
            end; // if NOT Odd(n)

          end; // begin - end


        begin
          result:= 0;

          if ( n > 0 ) and ( n < EdburgGeneralFuncsMaxObservations ) then
            result:= RankSort(AnArray,n);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetSum
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetSum (const anArray: array of double; const n: integer): extended;

        var
          i: integer;
        begin
          result := 0;
          if ( n < EdburgGeneralFuncsMaxObservations ) then begin
            for i:= 0 to n-1 do
              Result:= Result + (AnArray[i]);
          end; // if ( n > 1 )
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMin
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMin (const anArray: array of double; const n: integer): extended;

        var
          i     : integer;
          aMin  : extended;
        begin
          result:= 0;
          if ( n < EdburgGeneralFuncsMaxObservations ) then begin
             aMin := VERYLARGENUMBER;
             for i:= 0 to n-1 do begin
                if ( anArray[i] < aMin ) then
                   aMin := anArray[i];
             end; // for i:= 0 to n-1
             result:= aMin;
          end; // if ( n > 1 )
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMax
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMax (const anArray: array of double; const n: integer): extended;

        var
          i     : integer;
          aMax  : extended;
        begin
          result:= 0;
          if ( n < EdburgGeneralFuncsMaxObservations ) then begin
             aMax := VERYSMALLNUMBER;
             for i:= 0 to n-1 do begin
                if ( anArray[i] > aMax ) then
                   aMax := anArray[i];
             end; // for i:= 0 to n-1
             result:= aMax;
          end; // if ( n > 1 ) and
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMean
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMean (const anArray: array of double; const n: integer): extended;

        var
          i: integer;
        begin
          Result:= 0;
          if ( n < EdburgGeneralFuncsMaxObservations ) then begin
              for i:= 0 to n-1 do
                Result:= Result + anArray[i];
              Result:= Result * (  1 / n );
          end; // if ( n > 1 ) and
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetVariance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetVariance ( const anArray: array of double; const n: integer): extended;

        var
          i     : integer;
          aSum  : extended;
          aSum2 : extended;
          numer : extended;
        begin
          result:= 0;

          if ( n > 1 ) then begin
             aSum    := 0;
             aSum2   := 0;

             for i:= 0 to n-1 do begin
                aSum  := aSum + anArray[i];
                aSum2 := aSum2 + ( anArray[i] * anArray[i] );
             end; // for i:= 0 to n-1

             Numer   := aSum2 - ( ( aSum * aSum ) * ( 1 / n ));
             Result  := Numer * ( 1 / ( n-1 ));
          end; // if ( n > 1 ) then

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetCoefficientOfVariation
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetCoefficientOfVariation (const AnArray : array of double;
                                            const n       : integer): extended;

        var
          aMean     : extended;
          aVariance : extended;
        begin
          result:= 0;

          if ( n > 1 ) then begin
              aMean:= GetMean(AnArray,n);
              aVariance:= GetVariance(AnArray,n);

              if ( aMean <> 0 ) then
                result:= sqrt(aVariance) * ( 1 / aMean );
          end; // if ( n > 2 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetSkew
        * Result   :  single
        * History  :  New
        *  Notes   : Adapted from Borland' Math unit - This is different in that
        *            by allowing a user to pass in an 'n' - one can avoid having
        *            to process the entire contents of the input AnArray array
        *            (very useful where such a thing is not required nor desired)
        ******************************************************************************)
        function GetSkew (const AnArray: array of double; const n: integer): single;

        var
          Sum,
          SumSquares,
          SumCubes,
          OverN,
          Accum,
          M1Sqr,
          S2N,
          S3N,
          M1,
          M2,
          M3: Extended;
          i: Integer;
        begin
          OverN       := 1 / (n + 1);
          Sum         := 0;
          SumSquares  := 0;
          SumCubes    := 0;

          for i := 0 to n-1 do begin
            Sum          := Sum + AnArray[i];
            Accum        := AnArray[i] * AnArray[i];
            SumSquares   := SumSquares + Accum;
            Accum        := Accum * AnArray[i];
            SumCubes     := SumCubes + Accum;
          end; // for i:= 0

          M1    := Sum * OverN;
          M1Sqr := M1 * M1;
          S2N   := SumSquares * OverN;
          S3N   := SumCubes * OverN;
          M2    := S2N - M1Sqr;
          M3    := S3N - (M1 * 3 * S2N) + 2*M1Sqr*M1;
          Result:= M3 * Power(M2, -3/2);

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetKurtosis
        * Result   :  single
        * History  :  New
        *  Notes   : Adapted from Borland' Math unit - This is different in that
        *            by allowing a user to pass in an 'n' - one can avoid having
        *            to process the entire contents of the input AnArray array
        *            (very useful where such a thing is not required nor desired)
        ******************************************************************************)
        function GetKurtosis (const AnArray: array of double; const n: integer): single;

        var
          M1, M2, M4,
          Sum,
          SumSquares,
          SumCubes,
          SumQuads,
          OverN,
          Accum,
          M1Sqr,
          S2N,
          S3N: Extended;
          i: Integer;
        begin
          OverN       := 1 / (n + 1);
          Sum         := 0;
          SumSquares  := 0;
          SumCubes    := 0;
          SumQuads    := 0;

          for i := 0 to n-1 do begin
            Sum          := Sum + AnArray[i];
            Accum        := AnArray[i] * AnArray[i];
            SumSquares   := SumSquares + Accum;
            Accum        := Accum * AnArray[i];
            SumCubes     := SumCubes + Accum;
            SumQuads     := SumQuads + Accum * AnArray[i];
          end; // for i:= 0 to

          M1       := Sum * OverN;
          M1Sqr    := M1 * M1;
          S2N      := SumSquares * OverN;
          S3N      := SumCubes * OverN;
          M2       := S2N - M1Sqr;
          M4       := (SumQuads * OverN) - (M1 * 4 * S3N) + (M1Sqr*6*S2N - 3*Sqr(M1Sqr));
          Result   := M4 / Sqr(M2);

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetTotalVariation
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetTotalVariation ( const VarVector: XVector; const k: byte): extended;

        var
          i: byte;
        begin
          result:= 0;
          for i:= 1 to k do
            result:= result + VarVector[i];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CheckK
        * Result   :  boolean
        * History  :  New
        ******************************************************************************)
        function CheckKN (const k: byte; const n: integer): boolean;
        begin
          Result:= FALSE;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
             if ( n > 1 ) and ( n < EdburgGeneralFuncsMaxObservations ) then
                Result:= TRUE;
          end; // if ( k )
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MeanVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function MeanVector (const PRegressData : PTRegressDataArray;
                             const k            : byte;
                             const n            : integer): XVector;

        var
          i: byte;
          j: integer;
          dDivTemp: double;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
            dDivTemp:= 1 / n;
            for i:= 1 to k do begin
              Result[i]:= 0;
              for j:= 0 to n-1 do
                Result[i]:= Result[i] + PRegressData^[i,j];
              Result[i]:= Result[i] * dDivTemp;
            end; // for i:= 1 to k

          end; // if ( n > 2 ) and

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  VarianceVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function VarianceVector (const PRegressData : PTRegressDataArray;
                                 const k            : byte;
                                 const n            : integer): XVector;

        var
          i: byte;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
             for i:= 1 to k do
                Result[i]:= GetVariance(PRegressData^[i],n);
          end; // if ( n > 2 ) and
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  VarianceProportionsVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function VarianceProportionsVector (const PRegressData : PTRegressDataArray;
                                            const k            : byte;
                                            const n            : integer): XVector;

        var
          i               : byte;
          aSum            : extended;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
             Result:= VarianceVector(PRegressData,k,n);
             aSum:= 0;

             for i:= 1 to k do
                aSum:= aSum + Result[i];

             if ( aSum > 0 ) then begin
                for i:= 1 to k do
                  Result[i]:= Result[i] * ( 1 / aSum );
             end; // if ( aSum > 0 ) then
          end; // if ( n > 2 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  SumVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function SumVector (const PRegressData : PTRegressDataArray;
                            const k            : byte;
                            const n            : integer): XVector;

        var
          i: byte;
          j: integer;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
            for i:= 1 to k do begin
              Result[i]:= 0;
              for j:= 0 to n-1 do
                Result[i]:= Result[i] + PRegressData^[i,j];
            end; // for i:= 1 to k
          end; // if ( n > 2 ) and

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MinVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function MinVector (const PRegressData : PTRegressDataArray;
                            const k            : byte;
                            const n            : integer): XVector;

        var
          i           : byte;
          j           : integer;
          aMin        : extended;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
            for i:= 1 to k do begin
              aMin:= VERYLARGENUMBER;

              for j:= 0 to n-1 do begin
                if ( PRegressData^[i,j] < aMin ) then
                  aMin := PRegressData^[i,j];
              end; // for j:= 0 to n-1
              Result[i]:= aMin;
            end; // for i:= 1 to k
          end; // if ( n > 2 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MaxVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function MaxVector (const PRegressData : PTRegressDataArray;
                            const k            : byte;
                            const n            : integer): XVector;

        var
          i           : byte;
          j           : integer;
          aMax        : extended;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          if CheckKN(k,n) then begin
            for i:= 1 to k do begin
              aMax:= VERYSMALLNUMBER;

              for j:= 0 to n-1 do begin
                if ( PRegressData^[i,j] > aMax ) then
                  aMax := PRegressData^[i,j];
              end; // for j:= 0 to n-1
              Result[i]:= aMax;
            end; // for i:= 1 to k
          end; // if ( n > 2 )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/02/03
        * Purpose  :  DivideVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function DivideVector                    (const Vector1 : XVector;
                                                  const k       : byte;
                                                  const dValue  : double): XVector;
        var
          i:integer;
        begin
          Result:= InitVector(k,0);

          for i:= 1 to k do begin
            if ( dValue > 0 ) then
              Result[i]:= Vector1[i] / dValue
            else
              Result[i]:= 0;
          end; // for i

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/02/03
        * Purpose  :  NormalizeVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function NormalizeVector           (const aVector      : XVector;
                                            const k            : byte): XVector;
        var
          dDiv: double;
        begin
          Result  := InitVector(k,0);
          dDiv    := GetMax(aVector,k);
          Result  := DivideVector(aVector,k,dDiv);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  SmallSampleCIForMean
        * Result   :  TIntervalRec
        * History  :  New
        ******************************************************************************)
        function SmallSampleCIforMean ( const YBar       : double;
                                        const S          : double;
                                        const n          : integer;
                                        const aSL        : TSignificanceLevel): TIntervalRec;

        begin
          Result := InitInterval;
          if ( n > 0 ) then
            Result:= MakePiped(YBar,TTableValue2(n,aSL),s/sqrt(n));
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  LargeSampleCIForMean
        * Result   :  TIntervalRec
        * History  :  New
        ******************************************************************************)
        function LargeSampleCIforMean (const YBar       : double;
                                       const Sigma      : double;
                                       const Percentage : string): TIntervalRec;

        begin
          Result := InitInterval;
          Result := MakePiped(YBar,ZTableValue2(Percentage),Sigma);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  TransposeVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function TransposeVector (const Vector1 : XVector; const k : byte): XVector;

        var
          i,cnt: byte;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          cnt:= k;
          for i:= 1 to k do begin
            Result[cnt]:= Vector1[i];
            dec(cnt);
          end; // for i:= 1 to k

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  AddVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function AddVector (const Vector1 : XVector;
                            const Vector2 : XVector;
                            const k       : byte): XVector;

        var
          i: byte;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          for i:= 1 to k do
            Result[i]:= Vector1[i] + Vector2[i];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  SubtractVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function SubtractVector ( const Vector1 : XVector;
                                  const Vector2 : XVector;
                                  const k       : byte): XVector;

        var
          i: byte;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          for i:= 1 to k do
            Result[i]:= Vector1[i] - Vector2[i];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  AddMatrices
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function AddMatrices ( const aMatrix1 : XMatrix;
                               const aMatrix2 : XMatrix;
                               const k        : byte): XMatrix;

        var
          i,j: byte;
        begin
          Result:= InitMatrix(EdburgMaxVariables,0);

          for i:= 1 to k do
            for j:= 1 to k do
              Result[i,j] := aMatrix1[i,j] + aMatrix2[i,j];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  SubtractMatrices
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function SubtractMatrices ( const aMatrix1 : XMatrix;
                                    const aMatrix2 : XMatrix;
                                    const k        : byte): XMatrix;

        var
          i,j: byte;
        begin
          Result:= InitMatrix(EdburgMaxVariables,0);

          for i:= 1 to k do
            for j:= 1 to k do
              Result[i,j] := aMatrix1[i,j] - aMatrix2[i,j];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  ZTableValue2
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function ZTableValue2 (const PercentageLevel : string): extended;
        begin
          result:= 0;

          if (PercentageLevel = '.90') or (PercentageLevel = '0.90') then
             result:= 1.645;

          if (PercentageLevel = '.95') or (PercentageLevel = '0.95') then
             result:= 1.960;

          if (PercentageLevel = '.99') or (PercentageLevel = '0.99') then
             result:= 2.576;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  TTableValue2
        * Result   :  single
        * History  :  New
        ******************************************************************************)
        function TTableValue2 (const n : integer; const aSL: TSignificanceLevel): single;
        var
          df: integer;
        begin
        //I tvalues.inc}
                tStatArr[1].df:= 1;
                tStatArr[1].t10:=   3.078;
                tStatArr[1].t05:=   6.314;
                tStatArr[1].t025:= 12.706;
                tStatArr[1].t010:= 31.821;
                tStatArr[1].t005:= 63.657;

                tStatArr[2].df:= 2;
                tStatArr[2].t10:=  1.886;
                tStatArr[2].t05:=  2.920;
                tStatArr[2].t025:= 4.303;
                tStatArr[2].t010:= 6.965;
                tStatArr[2].t005:= 9.925;

                tStatArr[3].df:= 3;
                tStatArr[3].t10:=  1.638;
                tStatArr[3].t05:=  2.353;
                tStatArr[3].t025:= 3.182;
                tStatArr[3].t010:= 4.541;
                tStatArr[3].t005:= 5.841;

                tStatArr[4].df:= 4;
                tStatArr[4].t10:=  1.533;
                tStatArr[4].t05:=  2.132;
                tStatArr[4].t025:= 2.776;
                tStatArr[4].t010:= 3.747;
                tStatArr[4].t005:= 4.604;

                tStatArr[5].df:= 5;
                tStatArr[5].t10:=  1.476;
                tStatArr[5].t05:=  2.015;
                tStatArr[5].t025:= 2.571;
                tStatArr[5].t010:= 3.365;
                tStatArr[5].t005:= 4.032;

                tStatArr[6].df:= 6;
                tStatArr[6].t10:=  1.440;
                tStatArr[6].t05:=  1.943;
                tStatArr[6].t025:= 2.447;
                tStatArr[6].t010:= 3.143;
                tStatArr[6].t005:= 3.707;

                tStatArr[7].df:= 7;
                tStatArr[7].t10:=  1.415;
                tStatArr[7].t05:=  1.895;
                tStatArr[7].t025:= 2.365;
                tStatArr[7].t010:= 2.998;
                tStatArr[7].t005:= 3.499;

                tStatArr[8].df:= 8;
                tStatArr[8].t10:=  1.397;
                tStatArr[8].t05:=  1.860;
                tStatArr[8].t025:= 2.306;
                tStatArr[8].t010:= 2.896;
                tStatArr[8].t005:= 3.355;

                tStatArr[9].df:= 9;
                tStatArr[9].t10:=  1.383;
                tStatArr[9].t05:=  1.833;
                tStatArr[9].t025:= 2.262;
                tStatArr[9].t010:= 2.821;
                tStatArr[9].t005:= 3.250;

                tStatArr[10].df:= 10;
                tStatArr[10].t10:=  1.372;
                tStatArr[10].t05:=  1.812;
                tStatArr[10].t025:= 2.228;
                tStatArr[10].t010:= 2.764;
                tStatArr[10].t005:= 3.169;

                tStatArr[11].df:= 11;
                tStatArr[11].t10:=  1.363;
                tStatArr[11].t05:=  1.796;
                tStatArr[11].t025:= 2.201;
                tStatArr[11].t010:= 2.718;
                tStatArr[11].t005:= 3.106;

                tStatArr[12].df := 12;
                tStatArr[12].t10:=  1.356;
                tStatArr[12].t05:=  1.782;
                tStatArr[12].t025:= 2.179;
                tStatArr[12].t010:= 2.681;
                tStatArr[12].t005:= 3.055;

                tStatArr[13].df:= 13;
                tStatArr[13].t10:=  1.350;
                tStatArr[13].t05:=  1.771;
                tStatArr[13].t025:= 2.160;
                tStatArr[13].t010:= 2.650;
                tStatArr[13].t005:= 3.012;

                tStatArr[14].df := 14;
                tStatArr[14].t10:=  1.345;
                tStatArr[14].t05:=  1.761;
                tStatArr[14].t025:= 2.145;
                tStatArr[14].t010:= 2.624;
                tStatArr[14].t005:= 2.977;

                tStatArr[15].df:= 15;
                tStatArr[15].t10:=  1.341;
                tStatArr[15].t05:=  1.753;
                tStatArr[15].t025:= 2.131;
                tStatArr[15].t010:= 2.602;
                tStatArr[15].t005:= 2.947;

                tStatArr[16].df:= 16;
                tStatArr[16].t10:=  1.337;
                tStatArr[16].t05:=  1.746;
                tStatArr[16].t025:= 2.120;
                tStatArr[16].t010:= 2.583;
                tStatArr[16].t005:= 2.921;

                tStatArr[17].df := 17;
                tStatArr[17].t10:=  1.333;
                tStatArr[17].t05:=  1.740;
                tStatArr[17].t025:= 2.110;
                tStatArr[17].t010:= 2.567;
                tStatArr[17].t005:= 2.898;

                tStatArr[18].df:= 18;
                tStatArr[18].t10:=  1.330;
                tStatArr[18].t05:=  1.734;
                tStatArr[18].t025:= 2.101;
                tStatArr[18].t010:= 2.552;
                tStatArr[18].t005:= 2.878;

                tStatArr[19].df:= 19;
                tStatArr[19].t10:=  1.328;
                tStatArr[19].t05:=  1.729;
                tStatArr[19].t025:= 2.093;
                tStatArr[19].t010:= 2.539;
                tStatArr[19].t005:= 2.861;

                tStatArr[20].df := 20;
                tStatArr[20].t10:=  1.325;
                tStatArr[20].t05:=  1.725;
                tStatArr[20].t025:= 2.086;
                tStatArr[20].t010:= 2.528;
                tStatArr[20].t005:= 2.845;

                tStatArr[21].df:= 21;
                tStatArr[21].t10:=  1.323;
                tStatArr[21].t05:=  1.721;
                tStatArr[21].t025:= 2.080;
                tStatArr[21].t010:= 2.518;
                tStatArr[21].t005:= 2.831;

                tStatArr[22].df:= 22;
                tStatArr[22].t10:=  1.321;
                tStatArr[22].t05:=  1.717;
                tStatArr[22].t025:= 2.074;
                tStatArr[22].t010:= 2.508;
                tStatArr[22].t005:= 2.819;

                tStatArr[23].df:= 23;
                tStatArr[23].t10:=  1.319;
                tStatArr[23].t05:=  1.714;
                tStatArr[23].t025:= 2.069;
                tStatArr[23].t010:= 2.500;
                tStatArr[23].t005:= 2.807;

                tStatArr[24].df := 24;
                tStatArr[24].t10:=  1.318;
                tStatArr[24].t05:=  1.711;
                tStatArr[24].t025:= 2.064;
                tStatArr[24].t010:= 2.492;
                tStatArr[24].t005:= 2.797;

                tStatArr[25].df := 25;
                tStatArr[25].t10:=  1.316;
                tStatArr[25].t05:=  1.708;
                tStatArr[25].t025:= 2.060;
                tStatArr[25].t010:= 2.485;
                tStatArr[25].t005:= 2.787;

                tStatArr[26].df := 26;
                tStatArr[26].t10:=  1.315;
                tStatArr[26].t05:=  1.706;
                tStatArr[26].t025:= 2.056;
                tStatArr[26].t010:= 2.479;
                tStatArr[26].t005:= 2.779;

                tStatArr[27].df:= 27;
                tStatArr[27].t10:=  1.314;
                tStatArr[27].t05:=  1.703;
                tStatArr[27].t025:= 2.052;
                tStatArr[27].t010:= 2.473;
                tStatArr[27].t005:= 2.771;

                tStatArr[28].df:= 28;
                tStatArr[28].t10:=  1.313;
                tStatArr[28].t05:=  1.701;
                tStatArr[28].t025:= 2.048;
                tStatArr[28].t010:= 2.467;
                tStatArr[28].t005:= 2.763;

                tStatArr[29].df:= 29;
                tStatArr[29].t10:=  1.311;
                tStatArr[29].t05:=  1.699;
                tStatArr[29].t025:= 2.045;
                tStatArr[29].t010:= 2.462;
                tStatArr[29].t005:= 2.756;

                tStatArr[30].df:= 30;
                tStatArr[30].t10:=  1.282;
                tStatArr[30].t05:=  1.645;
                tStatArr[30].t025:= 1.960;
                tStatArr[30].t010:= 2.326;
                tStatArr[30].t005:= 2.576;

                // added these 10/98

                tStatArr[31].df := 35;
                tStatArr[31].t10:=  1.306;
                tStatArr[31].t05:=  1.690;
                tStatArr[31].t025:= 2.030;
                tStatArr[31].t010:= 2.438;
                tStatArr[31].t005:= 2.724;

                tStatArr[32].df:= 40;
                tStatArr[32].t10:=  1.303;
                tStatArr[32].t05:=  1.684;
                tStatArr[32].t025:= 2.021;
                tStatArr[32].t010:= 2.423;
                tStatArr[32].t005:= 2.704;

                tStatArr[33].df:= 45;
                tStatArr[33].t10:=  1.301;
                tStatArr[33].t05:=  1.679;
                tStatArr[33].t025:= 2.014;
                tStatArr[33].t010:= 2.412;
                tStatArr[33].t005:= 2.690;

                tStatArr[34].df:= 50;
                tStatArr[34].t10:=  1.299;
                tStatArr[34].t05:=  1.676;
                tStatArr[34].t025:= 2.009;
                tStatArr[34].t010:= 2.403;
                tStatArr[34].t005:= 2.678;

                tStatArr[35].df := 60;
                tStatArr[35].t10:=  1.296;
                tStatArr[35].t05:=  1.671;
                tStatArr[35].t025:= 2.000;
                tStatArr[35].t010:= 2.390;
                tStatArr[35].t005:= 2.660;

                tStatArr[36].df := 70;
                tStatArr[36].t10:=  1.294;
                tStatArr[36].t05:=  1.667;
                tStatArr[36].t025:= 1.994;
                tStatArr[36].t010:= 2.381;
                tStatArr[36].t005:= 2.648;

                tStatArr[37].df := 80;
                tStatArr[37].t10:=  1.292;
                tStatArr[37].t05:=  1.664;
                tStatArr[37].t025:= 1.990;
                tStatArr[37].t010:= 2.374;
                tStatArr[37].t005:= 2.639;

                tStatArr[38].df:= 90;
                tStatArr[38].t10:=  1.291;
                tStatArr[38].t05:=  1.662;
                tStatArr[38].t025:= 1.987;
                tStatArr[38].t010:= 2.368;
                tStatArr[38].t005:= 2.632;

                tStatArr[39].df:= 100;
                tStatArr[39].t10:=  1.290;
                tStatArr[39].t05:=  1.660;
                tStatArr[39].t025:= 1.984;
                tStatArr[39].t010:= 2.364;
                tStatArr[39].t005:= 2.626;

                tStatArr[40].df:= 120;
                tStatArr[40].t10:=  1.289;
                tStatArr[40].t05:=  1.658;
                tStatArr[40].t025:= 1.980;
                tStatArr[40].t010:= 2.358;
                tStatArr[40].t005:= 2.617;




          result:= 0;

          df:= n;
          if ( df > 30 ) then
             df := 30;  // t table values 'peak out' at 30

          //if ( if in [1..30] ) then begin - removed this to improve speed;
          // slightly less safe, but n should NEVER be < 0)
            case aSL of
              sl90  : result:= tStatArr[df].t10;   // 0.10
              sl95  : result:= tStatArr[df].t05;   // 0.05
              sl99  : result:= tStatArr[df].t010;  // 0.01
              sl975 : result:= tStatArr[df].t025;  // 0.025
              sl995 : result:= tStatArr[df].t005;  // 0.005
            end; // case
          //end; // if ( df in [1..30] )
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MakePiped
        * Result   :  TIntervalRec
        * History  :  New
        ******************************************************************************)
        function MakePiped (const Mean   : double;
                            const StdDev : double;
                            const Scalar : double): TIntervalRec;

        var
          dTmp: double;
        begin
          dTmp:= StdDev * Scalar;
          with Result do begin
             Lower := Mean - dTmp;
             Upper := Mean + dTmp;
          end; // with Result do begin
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InvertMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function InvertMatrix (const InMatrix : XMatrix;
                               const k        : byte;
                               var Det        : double): XMatrix;

        // Inverts a (k x k) matrix

        var
          i,j,l   : byte;
          dDiver  : extended;
          dRatio  : extended;
          aMatrix : XMatrix;
        begin
          Result  := IdentityMatrix(k);  // 'create' identity (k x k)
          aMatrix := CopyMatrix(InMatrix,k);

          Det     := 1;                  // determinant

          for i := 1 to k do
          begin       // ok, do it
            dDiver := aMatrix[i,i];
            Det   := Det * dDiver;
            for j := 1 to k do
            begin
              if ( dDiver <> 0 ) then
              begin
                aMatrix  [i,j]:= aMatrix [i,j]  * ( 1 / dDiver );
                Result   [i,j]:= Result[i,j] * ( 1 / dDiver );
              end // if ( Diver <> 0 )
              else
              begin
                Raise Exception.Create('Error div by 0 inverting matrices');
                Exit;
              end; // else begin - end
            end; // for j:= 1 to k

            for j := 1 to k do
            begin
              if ( i-j <> 0 ) then
              begin
                dRatio:= aMatrix[j,i];
                for l := 1 to k do
                begin
                  aMatrix   [j,l]:= aMatrix  [j,l] - ( dRatio * aMatrix [i,l] );
                  Result    [j,l]:= Result[j,l] - ( dRatio * Result[i,l] );
                end; // for l:= 1 to k
              end; // if (i-j <> 0 )
            end; // for j:= 1 to k
          end; // for i:= 1 to k

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  IdentityMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function IdentityMatrix (const k : byte): XMatrix;
        var
          i,j: byte;
        begin
          for i:= 1 to k do
          begin
             for j:= 1 to k do
             begin
               Result[i,j]:= 0;
             end; // for j
             Result[i,i]:= 1;
          end; // for i
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CopyMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function CopyMatrix (const aMatrix: XMatrix; const k: byte): XMatrix;
        begin
          Result:= InitMatrix(EdburgMaxVariables,0);
          Move(aMatrix,Result,sizeof(aMatrix));
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  IdentityMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function NormalizeMatrix (const aMatrix : XMatrix;
                                  const k       : byte): XMatrix;
        var
          i,j: byte;
          dMax: double;
        begin
          dMax:= VERYSMALLNUMBER;

          for i:= 1 to k do
             for j:= 1 to k do
               if ( aMatrix[i,j] > dMax ) then
                  dMax:= aMatrix[i,j];

          for i:= 1 to k do
            for j:= 1 to k do
            begin
              if ( aMatrix[i,j] <> 0 ) then
                Result[i,j]:= aMatrix[i,j] / dMax
              else
                Result[i,j]:= 0;
            end; // for j

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  CopyMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function CopyVector (const aVector: XMatrix; const k: byte): XVector;
        begin
          Result:= InitVector(EdburgMaxVariables,0);
          Move(aVector,Result,sizeof(aVector));
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MultiplyMatrices
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function MultiplyMatrices (const aMatrix1 : XMatrix;
                                   const aMatrix2 : XMatrix;
                                   const k        : byte): XMatrix;

        // Matrix multiply a (kxk) by a (kxk)

        var
          i,j,cnt : byte;
          asum    : extended;

        begin

          if ( k in Range1to10 ) then
            Result:= FastMatrixMultiply(aMatrix1,aMatrix2,k)

          else if ( k in [11..EdburgMaxVariables] ) then
          begin
             Result:= InitMatrix(EdburgMaxVariables,0);
             for i:= 1 to k do
             begin
                for j:= 1 to k do
                begin
                   asum:= 0;
                   for cnt:= 1 to k do
                      asum:= asum + (aMatrix1[cnt,j] * aMatrix2[i,cnt]);
                   Result[i,j]:= asum;
                end; // for j:= 1 to k
             end; // for i:= 1 to k
          end; // else if

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/27/03
        * Purpose  :  DotProductMultiplyMatrices
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function DotProductMultiplyMatrices ( const aVector1: XVector;
                                              const aVector2: XVector;
                                              const k       : byte): XMatrix;
        var
          i,j:integer;
        begin
          Result:= InitMatrix(EdburgMaxVariables,0);

          for i:= 1 to k do
            for j:= 1 to k do
              Result[i,j] := aVector1[i] * aVector2[j];
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/03/03
        * Purpose  :  Do an upper triangular mult with Vector * Matrix
        * Result   :  double
        * History  :  New
        ******************************************************************************)
        function UpperTriangularMatrixVectorMultiply(
                                                      const aVector : XVector;
                                                      const aMatrix : XMatrix;
                                                      const k       : byte): double;

        var
          i,j         : byte;
          dSum,dTotal : double;
        begin
          dTotal := 0;

          for i:= 0 to k-1 do
          begin (* multiply only the lower triangular     *)
            dSum:= 0;                  (* part of the invrs of *)
            for j:= 0 to i do          (* class var/cov matrix *)
              dSum := ( aVector[j] * aMatrix[i,j]) + dSum;
            dTotal := ( dSum * aVector[i] ) + dTotal;
          end; // for i

          Result:= dTotal;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/15/03
        * Purpose  :  Calc determinant using 2x2 pairs of matrix values
        * Result   :  double
        * History  :  New
        ******************************************************************************)
        function Determinant2x2 ( const a1: double; const b2: double;
                                  const c3: double; const d4: double):double;
        begin
          Result:= ( a1 * d4 ) - ( b2 * c3 );
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/15/03
        * Purpose  :  Triangularize matrix using 'Y' Vector
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function TriangularizeMatrix (aVector : XVector;
                                      aMatrix : XMatrix;
                                      const k : byte): XMatrix;
        var
          i,j,l:integer;
        begin

          for l:= 1 to k-1 do
          begin
            for i:= l+1 to k do
            begin
              for j:= l+1 to k do
              begin
                aMatrix[i,j]:= Determinant2x2(aMatrix[l,l],
                                    aMatrix[i,l],
                                    aMatrix[l,j],
                                    aMatrix[i,j]);
              end; // for j
              aVector[i]:= Determinant2x2(aMatrix[l,l],
                                aMatrix[i,l],
                                aVector[l],
                                aVector[i]);
              aMatrix[i,l] := 0;
            end; // for i
          end; // for l

          Result:= aMatrix;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  DivideMatrixScalar
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function DivideMatrixScalar (const aMatrix1 : XMatrix;
                                     const k        : byte;
                                     const aScalar  : extended): XMatrix;
        var
          i,j: byte;
        begin
          Result:= InitMatrix(EdburgMaxVariables,0);

          for i:= 1 to k do
            for j:= 1 to k do
              Result[i,j] := aMatrix1[i,j] / aScalar;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  DivideMatrixScalar
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function DivideMatrixDiagonalScalar ( const aMatrix1 : XMatrix;
                                              const k        : byte;
                                              const aScalar  : extended): XMatrix;
        var
          i: byte;
        begin
          Result:= InitMatrix(EdburgMaxVariables,0);

          for i:= 1 to k do
            Result[i,i] := aMatrix1[i,i] / aScalar;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MultiplyVectorByMatrix
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function MultiplyVectorByMatrix    (const aMatrix : XMatrix;
                                            const aVector : XVector;
                                            const k       : byte): XVector;

        // To Premultipy a Vector (1x3) by a Matrix (3x3)

        var
          i,j       : byte;
          aSum      : extended;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          for i:= 1 to k do
          begin                          // rows
            aSum:= 0;
            for j:= 1 to k do
              aSum := aSum + (aVector[j] * aMatrix[j,i]);  // changed aMatrix[i,j] to
            Result[i]:= aSum;                              // aMatrix[j,i] 11/30/98
          end; // for i:= 1 to k

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MultiplyMatrixByVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function MultiplyMatrixByVector    (const aMatrix : XMatrix;
                                            const aVector : XVector;
                                            const k       : byte): XVector;

        // To [post] multiply
        //   a Matrix (3x3) by a vector (3x1) for example

        var
          i,j       : byte;
        begin
          Result:= InitVector(EdburgMaxVariables,0);

          for i:= 1 to k do
          begin
            Result[i]:= 0;
            for j:= 1 to k do
              Result[i] := Result[i] + (aVector[j] * aMatrix[j,i]);
          end; // for i:= 1 to k

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MultiplyVectorByVector
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function MultiplyVectorByVector (const Vector1 : XVector;
                                         const Vector2 : XVector;
                                         const k       : byte): extended;

        var
          i    : byte;
        begin
          Result:= 0;

          if ( k in Range1to10 ) then
          begin

             case k of

             10: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] +
                   Vector1[5] * Vector2[5] +
                   Vector1[6] * Vector2[6] +
                   Vector1[7] * Vector2[7] +
                   Vector1[8] * Vector2[8] +
                   Vector1[9] * Vector2[9] +
                   Vector1[10] * Vector2[10];

             9: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] +
                   Vector1[5] * Vector2[5] +
                   Vector1[6] * Vector2[6] +
                   Vector1[7] * Vector2[7] +
                   Vector1[8] * Vector2[8] +
                   Vector1[9] * Vector2[9] ;

             8: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] +
                   Vector1[5] * Vector2[5] +
                   Vector1[6] * Vector2[6] +
                   Vector1[7] * Vector2[7] +
                   Vector1[8] * Vector2[8] ;

             7: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] +
                   Vector1[5] * Vector2[5] +
                   Vector1[6] * Vector2[6] +
                   Vector1[7] * Vector2[7] ;

             6: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] +
                   Vector1[5] * Vector2[5] +
                   Vector1[6] * Vector2[6] ;

             5: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] +
                   Vector1[5] * Vector2[5] ;

             4: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] +
                   Vector1[4] * Vector2[4] ;

             3: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2] +
                   Vector1[3] * Vector2[3] ;

             2: Result:=
                   Vector1[1] * Vector2[1] +
                   Vector1[2] * Vector2[2];

             1: Result:= Vector1[1] * Vector2[1];

             end; // case
          end // if ()

          else begin
             for i:= 1 to k do
                Result:= Result + Vector1[i] * Vector2[i];
          end; // else begin

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MultiplyVectorByTransposeVector
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function MultiplyVectorByTransposeVector (const Vector1 : XVector;
                                                  const Vector2 : XVector;
                                                  const k       : byte): extended;

        var
          i,j : byte;
          aSum  : extended;
        begin
          aSum:= 0;
          j:= k;

          for i:= 1 to k do begin
            aSum:= aSum + Vector1[i] * Vector2[j];
            dec(j);
          end; // for i:= 1 to k

          result:= aSum;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetAvgNormDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetAvgNormDistance (const MeanVector1 : XVector;
                                     const MeanVector2 : XVector;
                                     const CovMatrix1  : XMatrix;
                                     const CovMatrix2  : XMatrix;
                                     const k           : byte): extended;

        // Computes the below quantity for each band - then
        // computes the overall means for the set of values
        //
        // Sum of, for i = 1..vars:
        //
        //    | Mean[1]   -   Mean[2] |
        //      --------------------
        //      StdDev[1] + StdDev[2]
        //
        // Note: StdDev1 and StdDev2 are computed from the input
        // CovarianceMatrices respectively

        var
          i        : byte;
          dTheDiff : double;
          dTheSum  : double;
        begin
          Result  := 0;

          try
            dTheSum:= 0;
            if ( k in [1..EdburgMaxVariables-1] ) then begin
              for i:= 1 to k do begin
                dTheDiff := abs(MeanVector1[i] - MeanVector2[i]);
                dTheSum  := dTheSum +
                  ( dTheDiff / ( sqrt(CovMatrix1[i,i]) + sqrt(CovMatrix2[i,i]) ));
              end; // for i:= 1 to k
              Result:= dTheSum / k;
            end; // if ( k in [1..])

          except
            Raise Exception.Create(AvgNormDistanceError);
          end; // try - except - end

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/5/03
        * Purpose  :  GetDivergence
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetDivergence (const MeanVector1   : XVector;
                                const MeanVector2   : XVector;
                                const xMatCov1      : XMatrix;
                                const xMatCov2      : XMatrix;
                                const k             : byte;
                                const iScalar       : integer): extended;

        // Computes the [non-scaled] divergence between two sample groups.  These are
        // characterized by a mean vector and a covariance matrix for each group.
        // Also, the number of dimensions must be passed to the function as well.
        // An extended value for the divergence (transformed by scalar) between groups
        // 1 and 2 is passed back.

        // Where:
        //   C[i] class(i) covariance matrix
        //   C[j] class(j) covariance matrix
        //   InvC[i] inverse of class(i) covariance matrix
        //   InvC[j] inverse of class(j) covariance matrix
        //   u[i] class(i) mean vector
        //   u[j] class(j) mean vector

        // Also, the user should apply a scalar to the raw divergence
        // score-for remote sensing, the following is commonly used:

        //  TransformedDivergence = Scalar * (1 - exp(-Divergence/8)


        var xMatSubtractedCov    : XMatrix;
            xMatSubtractedInvCov : XMatrix;
            xMatAddedInvCov      : XMatrix;
            xMat1                : XMatrix;
            xMat2                : XMatrix;
            xMat3                : XMatrix;
            xMatInvCov1          : XMatrix;
            xMatInvCov2          : XMatrix;
            vecSubtractedMean    : XVector;
            vecTemp              : XVector;
            dValue1              : extended;
            dValue2              : extended;
            dDeterm              : double;

        //   Divergence = .5Tr [ (C[i] - C[j]) (InvC[j] - InvC[i]) ] +
        //                          k x k         k x k

        //   .5Tr [ (InvC[i] + InvC[j]) (u[i] - u[j]) (u[i] - u[j])t ]
        //                k x k           k x 1           1 x k
        //                k x k                 k x k
        //                    =    a k x k

        begin
          xMatSubtractedCov    := InitMatrix(EdburgMaxVariables,0);
          xMatSubtractedInvCov := InitMatrix(EdburgMaxVariables,0);
          xMatAddedInvCov      := InitMatrix(EdburgMaxVariables,0);
          xMat1                := InitMatrix(EdburgMaxVariables,0);
          xMat2                := InitMatrix(EdburgMaxVariables,0);
          xMat3                := InitMatrix(EdburgMaxVariables,0);
          xMatInvCov1          := InitMatrix(EdburgMaxVariables,0);
          xMatInvCov2          := InitMatrix(EdburgMaxVariables,0);
          vecSubtractedMean    := InitVector(EdburgMaxVariables,0);
          vecTemp              := InitVector(EdburgMaxVariables,0);
          Result               := -1;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            try
              xMatInvCov1:= InvertMatrix(xMatCov1,k,dDeterm);
              xMatInvCov2:= InvertMatrix(xMatCov2,k,dDeterm);

              xMatSubtractedCov     := SubtractMatrices(xMatCov1,xMatCov2,k);
              xMatSubtractedInvCov  := SubtractMatrices(xMatInvCov2,xMatInvCov1,k);
              xMat1                 := MultiplyMatrices(  xMatSubtractedCov,
                                                          xMatSubtractedInvCov,
                                                          k);
              dValue1               := MatrixTrace(xMat1,k) * ONE_HALF;

              xMatAddedInvCov       := AddMatrices(xMatInvCov2,xMatInvCov1,k);
              vecSubtractedMean     := SubtractVector(MeanVector1,MeanVector2,k);
              xMat2                 := DotProductMultiplyMatrices( vecSubtractedMean,
                                                                   vecSubtractedMean,
                                                                   k);

              xMat3   := MultiplyMatrices(xMatAddedInvCov,xMat2,k);
              dValue2 := MatrixTrace(xMat3,k) * ONE_HALF;

              Result  := dValue1 + dValue2;

              // Apply scaling to 'raw' value
              Result  := iScalar * ( 1 - exp(-Result / 8));

            except on E: Exception do
              begin
                ShowMessage('Exception - ' + E.Message + '. In GetDivergence');
                Result:= -1;
              end; // begin
            end; // try - except - end

          end; // if ( k )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/5/03
        * Purpose  :  GetBhattacharyyaDivergence
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetBhattacharyyaDistance (const MeanVector1   : XVector;
                                           const MeanVector2   : XVector;
                                           const xMatCov1      : XMatrix;
                                           const xMatCov2      : XMatrix;
                                           const k             : byte): extended;

        (* BhatDist:
                         T        -1
         D  = 0.25(M -M ) (C + C )  (M -M )
          ij        i  j    i   j     i  j


                             | C  + C  |
                                i    j
              +0.5 ln   (-----------------)
                                 1/2   1/2
                         2 | C  |   | C   |
                              i        j

        where
              Dij = Bhattacharyya distance between classes i and j
              Ci  = covariance matrix for class i
              Cj  = covariance matrix for class j
              Mi  = mean vector for class i
              Mj  = mean vector for class j

        The average distance for all class pairs is also calculated. *)

        var xMatSubtractedCov           : XMatrix;
            xMatSubtractedInvCov        : XMatrix;
            xMatAddedInvCov             : XMatrix;
            xMat1                       : XMatrix;
            xMat2                       : XMatrix;
            xMatInvCov1                 : XMatrix;
            xMatInvCov2                 : XMatrix;
            vecSubtractedMeans          : XVector;
            vecSubtractedMeansTranspose : XVector;
            vecTemp1                    : Xvector;
            dValue1                     : double;
            dValue2                     : double;
            dDeterm1                    : double;
            dDeterm2                    : double;
            dDetermTemp                 : double;
        begin
          xMatSubtractedCov           := InitMatrix(EdburgMaxVariables,0);
          xMatSubtractedInvCov        := InitMatrix(EdburgMaxVariables,0);
          xMatAddedInvCov             := InitMatrix(EdburgMaxVariables,0);
          xMat1                       := InitMatrix(EdburgMaxVariables,0);
          xMat2                       := InitMatrix(EdburgMaxVariables,0);
          xMatInvCov1                 := InitMatrix(EdburgMaxVariables,0);
          xMatInvCov2                 := InitMatrix(EdburgMaxVariables,0);
          vecSubtractedMeans          := InitVector(EdburgMaxVariables,0);
          vecSubtractedMeansTranspose := InitVector(EdburgMaxVariables,0);
          vecTemp1                    := InitVector(EdburgMaxVariables,0);
          Result                      := -1;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            try
              xMatInvCov1 := InvertMatrix(xMatCov1,k,dDeterm1);
              xMatInvCov2 := InvertMatrix(xMatCov2,k,dDeterm2);

              // First half of equation
              vecSubtractedMeans  := SubtractVector(MeanVector1,MeanVector2,k);
              vecSubtractedMeansTranspose:= TransposeVector(vecSubtractedMeans,k);

              xMat1     := AddMatrices(xMatCov1,xMatCov2,k);
              xMat2     := InvertMatrix(xMat1,k,dDetermTemp);
              vecTemp1  := MultiplyVectorByMatrix(xMat2,vecSubtractedMeansTranspose,k);
              dValue1   := ONE_QUARTER * MultiplyVectorByVector(vecTemp1,vecSubtractedMeans,k);

              // Second half of equation
              dValue2   := ( dDetermTemp / ( TWO * ( sqrt(dDeterm1) * sqrt(dDeterm2)) ) );

              // Add results of first half and second half for total
              Result    := dValue1 + ( ONE_HALF * ln(dValue2) );

            except on E: Exception do
              begin
                ShowMessage('Exception - ' + E.Message + '. In GetBhattacharyyaDistance');
                Result:= -1;
              end; // begin
            end; // try - except - end

          end; // if ( k )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/31/03
        * Purpose  :  GetJeffriesMatusitaDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetJeffriesMatusitaDistance (const MeanVector1   : XVector;
                                              const MeanVector2   : XVector;
                                              const xMatCov1      : XMatrix;
                                              const xMatCov2      : XMatrix;
                                              const k             : byte;
                                              const iScalar       : integer): extended;

        // Computes the [non-scaled] divergence between two sample groups.  These are
        // characterized by a mean vector and a covariance matrix for each group.
        // Also, the number of dimensions must be passed to the function as well.
        // An extended value for the divergence (transformed by scalar) between groups
        // 1 and 2 is passed back.

        // Where:
        //   C[i] class(i) covariance matrix
        //   C[j] class(j) covariance matrix
        //   InvC[i] inverse of class(i) covariance matrix
        //   InvC[j] inverse of class(j) covariance matrix
        //   u[i] class(i) mean vector
        //   u[j] class(j) mean vector

        var xMatSubtractedCov           : XMatrix;
            xMatSubtractedInvCov        : XMatrix;
            xMatAddedInvCov             : XMatrix;
            xMat1                       : XMatrix;
            xMat2                       : XMatrix;
            xMat3                       : XMatrix;
            xMatInvCov1                 : XMatrix;
            xMatInvCov2                 : XMatrix;
            vecSubtractedMeans          : XVector;
            vecSubtractedMeansTranspose : XVector;
            vecTemp1                    : Xvector;
            dValue1                     : double;
            dValue2                     : double;
            dValue3                     : double;
            dDeterm1                    : double;
            dDeterm2                    : double;
            dDetermTemp                 : double;
        begin
          xMatSubtractedCov           := InitMatrix(EdburgMaxVariables,0);
          xMatSubtractedInvCov        := InitMatrix(EdburgMaxVariables,0);
          xMatAddedInvCov             := InitMatrix(EdburgMaxVariables,0);
          xMat1                       := InitMatrix(EdburgMaxVariables,0);
          xMat2                       := InitMatrix(EdburgMaxVariables,0);
          xMat3                       := InitMatrix(EdburgMaxVariables,0);
          xMatInvCov1                 := InitMatrix(EdburgMaxVariables,0);
          xMatInvCov2                 := InitMatrix(EdburgMaxVariables,0);
          vecSubtractedMeans          := InitVector(EdburgMaxVariables,0);
          vecSubtractedMeansTranspose := InitVector(EdburgMaxVariables,0);
          vecTemp1                    := InitVector(EdburgMaxVariables,0);
          Result                      := -1;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            try
              xMatInvCov1 := InvertMatrix(xMatCov1,k,dDeterm1);
              xMatInvCov2 := InvertMatrix(xMatCov2,k,dDeterm2);

              vecSubtractedMeans  := SubtractVector(MeanVector1,MeanVector2,k);
              vecSubtractedMeansTranspose:= TransposeVector(vecSubtractedMeans,k);

              xMat1     := AddMatrices(xMatCov1,xMatCov2,k);
              xMat1     := DivideMatrixScalar(xMat1,k,2);
              xMat2     := InvertMatrix(xMat1,k,dDetermTemp);

              vecTemp1  := MultiplyVectorByMatrix(xMat2,vecSubtractedMeansTranspose,k);
              dValue1   := ONE_EIGHTH * MultiplyVectorByVector(vecTemp1,vecSubtractedMeans,k);


              xMat3     := AddMatrices(xMatCov1,xMatCov2,k);
              xMat3     := DivideMatrixScalar(xMat3,k,2);
              InvertMatrix(xMat3,k,dValue2);

              dValue3   := dValue1 + (ONE_HALF * ln((dValue2) / sqrt(dDeterm1 * dDeterm2)));

              // Compute final result and apply user-specified scaling
              Result    := iScalar * ( sqrt ( TWO * ( ONE  - exp(-dValue3))) );

            except on E: Exception do
              begin
                ShowMessage('Exception - ' + E.Message + '. In GetJeffriesMatusitaDistance');
                Result:= -1;
              end; // begin
            end; // try - except - end

          end; // if ( k )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  FastMatrixMultiply - 'unroll' all the looping req'd for mat mults.
        * Result   :  XMatrix
        * History  :  New - revived from some test code
        ******************************************************************************)
        function FastMatrixMultiply (const aMatrix1 : XMatrix;
                                     const aMatrix2 : XMatrix;
                                     const k        : byte): XMatrix;
        begin
          InitMatrix(EdburgMaxVariables,0);

          case k of

          10: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  +
               aMatrix1[5,1] * aMatrix2[1,5]  +
               aMatrix1[6,1] * aMatrix2[1,6]  +
               aMatrix1[7,1] * aMatrix2[1,7]  +
               aMatrix1[8,1] * aMatrix2[1,8]  +
               aMatrix1[9,1] * aMatrix2[1,9]  +
               aMatrix1[10,1] * aMatrix2[1,10]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  +
               aMatrix1[5,2] * aMatrix2[1,5]  +
               aMatrix1[6,2] * aMatrix2[1,6]  +
               aMatrix1[7,2] * aMatrix2[1,7]  +
               aMatrix1[8,2] * aMatrix2[1,8]  +
               aMatrix1[9,2] * aMatrix2[1,9]  +
               aMatrix1[10,2] * aMatrix2[1,10]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  +
               aMatrix1[5,3] * aMatrix2[1,5]  +
               aMatrix1[6,3] * aMatrix2[1,6]  +
               aMatrix1[7,3] * aMatrix2[1,7]  +
               aMatrix1[8,3] * aMatrix2[1,8]  +
               aMatrix1[9,3] * aMatrix2[1,9]  +
               aMatrix1[10,3] * aMatrix2[1,10]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  +
               aMatrix1[5,4] * aMatrix2[1,5]  +
               aMatrix1[6,4] * aMatrix2[1,6]  +
               aMatrix1[7,4] * aMatrix2[1,7]  +
               aMatrix1[8,4] * aMatrix2[1,8]  +
               aMatrix1[9,4] * aMatrix2[1,9]  +
               aMatrix1[10,4] * aMatrix2[1,10]  ;

             Result[1,5]:=
               aMatrix1[1,5] * aMatrix2[1,1]  +
               aMatrix1[2,5] * aMatrix2[1,2]  +
               aMatrix1[3,5] * aMatrix2[1,3]  +
               aMatrix1[4,5] * aMatrix2[1,4]  +
               aMatrix1[5,5] * aMatrix2[1,5]  +
               aMatrix1[6,5] * aMatrix2[1,6]  +
               aMatrix1[7,5] * aMatrix2[1,7]  +
               aMatrix1[8,5] * aMatrix2[1,8]  +
               aMatrix1[9,5] * aMatrix2[1,9]  +
               aMatrix1[10,5] * aMatrix2[1,10]  ;

             Result[1,6]:=
               aMatrix1[1,6] * aMatrix2[1,1]  +
               aMatrix1[2,6] * aMatrix2[1,2]  +
               aMatrix1[3,6] * aMatrix2[1,3]  +
               aMatrix1[4,6] * aMatrix2[1,4]  +
               aMatrix1[5,6] * aMatrix2[1,5]  +
               aMatrix1[6,6] * aMatrix2[1,6]  +
               aMatrix1[7,6] * aMatrix2[1,7]  +
               aMatrix1[8,6] * aMatrix2[1,8]  +
               aMatrix1[9,6] * aMatrix2[1,9]  +
               aMatrix1[10,6] * aMatrix2[1,10]  ;

             Result[1,7]:=
               aMatrix1[1,7] * aMatrix2[1,1]  +
               aMatrix1[2,7] * aMatrix2[1,2]  +
               aMatrix1[3,7] * aMatrix2[1,3]  +
               aMatrix1[4,7] * aMatrix2[1,4]  +
               aMatrix1[5,7] * aMatrix2[1,5]  +
               aMatrix1[6,7] * aMatrix2[1,6]  +
               aMatrix1[7,7] * aMatrix2[1,7]  +
               aMatrix1[8,7] * aMatrix2[1,8]  +
               aMatrix1[9,7] * aMatrix2[1,9]  +
               aMatrix1[10,7] * aMatrix2[1,10]  ;

             Result[1,8]:=
               aMatrix1[1,8] * aMatrix2[1,1]  +
               aMatrix1[2,8] * aMatrix2[1,2]  +
               aMatrix1[3,8] * aMatrix2[1,3]  +
               aMatrix1[4,8] * aMatrix2[1,4]  +
               aMatrix1[5,8] * aMatrix2[1,5]  +
               aMatrix1[6,8] * aMatrix2[1,6]  +
               aMatrix1[7,8] * aMatrix2[1,7]  +
               aMatrix1[8,8] * aMatrix2[1,8]  +
               aMatrix1[9,8] * aMatrix2[1,9]  +
               aMatrix1[10,8] * aMatrix2[1,10]  ;

             Result[1,9]:=
               aMatrix1[1,9] * aMatrix2[1,1]  +
               aMatrix1[2,9] * aMatrix2[1,2]  +
               aMatrix1[3,9] * aMatrix2[1,3]  +
               aMatrix1[4,9] * aMatrix2[1,4]  +
               aMatrix1[5,9] * aMatrix2[1,5]  +
               aMatrix1[6,9] * aMatrix2[1,6]  +
               aMatrix1[7,9] * aMatrix2[1,7]  +
               aMatrix1[8,9] * aMatrix2[1,8]  +
               aMatrix1[9,9] * aMatrix2[1,9]  +
               aMatrix1[10,9] * aMatrix2[1,10]  ;

             Result[1,10]:=
               aMatrix1[1,10] * aMatrix2[1,1]  +
               aMatrix1[2,10] * aMatrix2[1,2]  +
               aMatrix1[3,10] * aMatrix2[1,3]  +
               aMatrix1[4,10] * aMatrix2[1,4]  +
               aMatrix1[5,10] * aMatrix2[1,5]  +
               aMatrix1[6,10] * aMatrix2[1,6]  +
               aMatrix1[7,10] * aMatrix2[1,7]  +
               aMatrix1[8,10] * aMatrix2[1,8]  +
               aMatrix1[9,10] * aMatrix2[1,9]  +
               aMatrix1[10,10] * aMatrix2[1,10]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  +
               aMatrix1[5,1] * aMatrix2[2,5]  +
               aMatrix1[6,1] * aMatrix2[2,6]  +
               aMatrix1[7,1] * aMatrix2[2,7]  +
               aMatrix1[8,1] * aMatrix2[2,8]  +
               aMatrix1[9,1] * aMatrix2[2,9]  +
               aMatrix1[10,1] * aMatrix2[2,10]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  +
               aMatrix1[5,2] * aMatrix2[2,5]  +
               aMatrix1[6,2] * aMatrix2[2,6]  +
               aMatrix1[7,2] * aMatrix2[2,7]  +
               aMatrix1[8,2] * aMatrix2[2,8]  +
               aMatrix1[9,2] * aMatrix2[2,9]  +
               aMatrix1[10,2] * aMatrix2[2,10]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  +
               aMatrix1[5,3] * aMatrix2[2,5]  +
               aMatrix1[6,3] * aMatrix2[2,6]  +
               aMatrix1[7,3] * aMatrix2[2,7]  +
               aMatrix1[8,3] * aMatrix2[2,8]  +
               aMatrix1[9,3] * aMatrix2[2,9]  +
               aMatrix1[10,3] * aMatrix2[2,10]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  +
               aMatrix1[5,4] * aMatrix2[2,5]  +
               aMatrix1[6,4] * aMatrix2[2,6]  +
               aMatrix1[7,4] * aMatrix2[2,7]  +
               aMatrix1[8,4] * aMatrix2[2,8]  +
               aMatrix1[9,4] * aMatrix2[2,9]  +
               aMatrix1[10,4] * aMatrix2[2,10]  ;

             Result[2,5]:=
               aMatrix1[1,5] * aMatrix2[2,1]  +
               aMatrix1[2,5] * aMatrix2[2,2]  +
               aMatrix1[3,5] * aMatrix2[2,3]  +
               aMatrix1[4,5] * aMatrix2[2,4]  +
               aMatrix1[5,5] * aMatrix2[2,5]  +
               aMatrix1[6,5] * aMatrix2[2,6]  +
               aMatrix1[7,5] * aMatrix2[2,7]  +
               aMatrix1[8,5] * aMatrix2[2,8]  +
               aMatrix1[9,5] * aMatrix2[2,9]  +
               aMatrix1[10,5] * aMatrix2[2,10]  ;

             Result[2,6]:=
               aMatrix1[1,6] * aMatrix2[2,1]  +
               aMatrix1[2,6] * aMatrix2[2,2]  +
               aMatrix1[3,6] * aMatrix2[2,3]  +
               aMatrix1[4,6] * aMatrix2[2,4]  +
               aMatrix1[5,6] * aMatrix2[2,5]  +
               aMatrix1[6,6] * aMatrix2[2,6]  +
               aMatrix1[7,6] * aMatrix2[2,7]  +
               aMatrix1[8,6] * aMatrix2[2,8]  +
               aMatrix1[9,6] * aMatrix2[2,9]  +
               aMatrix1[10,6] * aMatrix2[2,10]  ;

             Result[2,7]:=
               aMatrix1[1,7] * aMatrix2[2,1]  +
               aMatrix1[2,7] * aMatrix2[2,2]  +
               aMatrix1[3,7] * aMatrix2[2,3]  +
               aMatrix1[4,7] * aMatrix2[2,4]  +
               aMatrix1[5,7] * aMatrix2[2,5]  +
               aMatrix1[6,7] * aMatrix2[2,6]  +
               aMatrix1[7,7] * aMatrix2[2,7]  +
               aMatrix1[8,7] * aMatrix2[2,8]  +
               aMatrix1[9,7] * aMatrix2[2,9]  +
               aMatrix1[10,7] * aMatrix2[2,10]  ;

             Result[2,8]:=
               aMatrix1[1,8] * aMatrix2[2,1]  +
               aMatrix1[2,8] * aMatrix2[2,2]  +
               aMatrix1[3,8] * aMatrix2[2,3]  +
               aMatrix1[4,8] * aMatrix2[2,4]  +
               aMatrix1[5,8] * aMatrix2[2,5]  +
               aMatrix1[6,8] * aMatrix2[2,6]  +
               aMatrix1[7,8] * aMatrix2[2,7]  +
               aMatrix1[8,8] * aMatrix2[2,8]  +
               aMatrix1[9,8] * aMatrix2[2,9]  +
               aMatrix1[10,8] * aMatrix2[2,10]  ;

             Result[2,9]:=
               aMatrix1[1,9] * aMatrix2[2,1]  +
               aMatrix1[2,9] * aMatrix2[2,2]  +
               aMatrix1[3,9] * aMatrix2[2,3]  +
               aMatrix1[4,9] * aMatrix2[2,4]  +
               aMatrix1[5,9] * aMatrix2[2,5]  +
               aMatrix1[6,9] * aMatrix2[2,6]  +
               aMatrix1[7,9] * aMatrix2[2,7]  +
               aMatrix1[8,9] * aMatrix2[2,8]  +
               aMatrix1[9,9] * aMatrix2[2,9]  +
               aMatrix1[10,9] * aMatrix2[2,10]  ;

             Result[2,10]:=
               aMatrix1[1,10] * aMatrix2[2,1]  +
               aMatrix1[2,10] * aMatrix2[2,2]  +
               aMatrix1[3,10] * aMatrix2[2,3]  +
               aMatrix1[4,10] * aMatrix2[2,4]  +
               aMatrix1[5,10] * aMatrix2[2,5]  +
               aMatrix1[6,10] * aMatrix2[2,6]  +
               aMatrix1[7,10] * aMatrix2[2,7]  +
               aMatrix1[8,10] * aMatrix2[2,8]  +
               aMatrix1[9,10] * aMatrix2[2,9]  +
               aMatrix1[10,10] * aMatrix2[2,10]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  +
               aMatrix1[5,1] * aMatrix2[3,5]  +
               aMatrix1[6,1] * aMatrix2[3,6]  +
               aMatrix1[7,1] * aMatrix2[3,7]  +
               aMatrix1[8,1] * aMatrix2[3,8]  +
               aMatrix1[9,1] * aMatrix2[3,9]  +
               aMatrix1[10,1] * aMatrix2[3,10]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  +
               aMatrix1[5,2] * aMatrix2[3,5]  +
               aMatrix1[6,2] * aMatrix2[3,6]  +
               aMatrix1[7,2] * aMatrix2[3,7]  +
               aMatrix1[8,2] * aMatrix2[3,8]  +
               aMatrix1[9,2] * aMatrix2[3,9]  +
               aMatrix1[10,2] * aMatrix2[3,10]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  +
               aMatrix1[5,3] * aMatrix2[3,5]  +
               aMatrix1[6,3] * aMatrix2[3,6]  +
               aMatrix1[7,3] * aMatrix2[3,7]  +
               aMatrix1[8,3] * aMatrix2[3,8]  +
               aMatrix1[9,3] * aMatrix2[3,9]  +
               aMatrix1[10,3] * aMatrix2[3,10]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  +
               aMatrix1[5,4] * aMatrix2[3,5]  +
               aMatrix1[6,4] * aMatrix2[3,6]  +
               aMatrix1[7,4] * aMatrix2[3,7]  +
               aMatrix1[8,4] * aMatrix2[3,8]  +
               aMatrix1[9,4] * aMatrix2[3,9]  +
               aMatrix1[10,4] * aMatrix2[3,10]  ;

             Result[3,5]:=
               aMatrix1[1,5] * aMatrix2[3,1]  +
               aMatrix1[2,5] * aMatrix2[3,2]  +
               aMatrix1[3,5] * aMatrix2[3,3]  +
               aMatrix1[4,5] * aMatrix2[3,4]  +
               aMatrix1[5,5] * aMatrix2[3,5]  +
               aMatrix1[6,5] * aMatrix2[3,6]  +
               aMatrix1[7,5] * aMatrix2[3,7]  +
               aMatrix1[8,5] * aMatrix2[3,8]  +
               aMatrix1[9,5] * aMatrix2[3,9]  +
               aMatrix1[10,5] * aMatrix2[3,10]  ;

             Result[3,6]:=
               aMatrix1[1,6] * aMatrix2[3,1]  +
               aMatrix1[2,6] * aMatrix2[3,2]  +
               aMatrix1[3,6] * aMatrix2[3,3]  +
               aMatrix1[4,6] * aMatrix2[3,4]  +
               aMatrix1[5,6] * aMatrix2[3,5]  +
               aMatrix1[6,6] * aMatrix2[3,6]  +
               aMatrix1[7,6] * aMatrix2[3,7]  +
               aMatrix1[8,6] * aMatrix2[3,8]  +
               aMatrix1[9,6] * aMatrix2[3,9]  +
               aMatrix1[10,6] * aMatrix2[3,10]  ;

             Result[3,7]:=
               aMatrix1[1,7] * aMatrix2[3,1]  +
               aMatrix1[2,7] * aMatrix2[3,2]  +
               aMatrix1[3,7] * aMatrix2[3,3]  +
               aMatrix1[4,7] * aMatrix2[3,4]  +
               aMatrix1[5,7] * aMatrix2[3,5]  +
               aMatrix1[6,7] * aMatrix2[3,6]  +
               aMatrix1[7,7] * aMatrix2[3,7]  +
               aMatrix1[8,7] * aMatrix2[3,8]  +
               aMatrix1[9,7] * aMatrix2[3,9]  +
               aMatrix1[10,7] * aMatrix2[3,10]  ;

             Result[3,8]:=
               aMatrix1[1,8] * aMatrix2[3,1]  +
               aMatrix1[2,8] * aMatrix2[3,2]  +
               aMatrix1[3,8] * aMatrix2[3,3]  +
               aMatrix1[4,8] * aMatrix2[3,4]  +
               aMatrix1[5,8] * aMatrix2[3,5]  +
               aMatrix1[6,8] * aMatrix2[3,6]  +
               aMatrix1[7,8] * aMatrix2[3,7]  +
               aMatrix1[8,8] * aMatrix2[3,8]  +
               aMatrix1[9,8] * aMatrix2[3,9]  +
               aMatrix1[10,8] * aMatrix2[3,10]  ;

             Result[3,9]:=
               aMatrix1[1,9] * aMatrix2[3,1]  +
               aMatrix1[2,9] * aMatrix2[3,2]  +
               aMatrix1[3,9] * aMatrix2[3,3]  +
               aMatrix1[4,9] * aMatrix2[3,4]  +
               aMatrix1[5,9] * aMatrix2[3,5]  +
               aMatrix1[6,9] * aMatrix2[3,6]  +
               aMatrix1[7,9] * aMatrix2[3,7]  +
               aMatrix1[8,9] * aMatrix2[3,8]  +
               aMatrix1[9,9] * aMatrix2[3,9]  +
               aMatrix1[10,9] * aMatrix2[3,10]  ;

             Result[3,10]:=
               aMatrix1[1,10] * aMatrix2[3,1]  +
               aMatrix1[2,10] * aMatrix2[3,2]  +
               aMatrix1[3,10] * aMatrix2[3,3]  +
               aMatrix1[4,10] * aMatrix2[3,4]  +
               aMatrix1[5,10] * aMatrix2[3,5]  +
               aMatrix1[6,10] * aMatrix2[3,6]  +
               aMatrix1[7,10] * aMatrix2[3,7]  +
               aMatrix1[8,10] * aMatrix2[3,8]  +
               aMatrix1[9,10] * aMatrix2[3,9]  +
               aMatrix1[10,10] * aMatrix2[3,10]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  +
               aMatrix1[5,1] * aMatrix2[4,5]  +
               aMatrix1[6,1] * aMatrix2[4,6]  +
               aMatrix1[7,1] * aMatrix2[4,7]  +
               aMatrix1[8,1] * aMatrix2[4,8]  +
               aMatrix1[9,1] * aMatrix2[4,9]  +
               aMatrix1[10,1] * aMatrix2[4,10]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  +
               aMatrix1[5,2] * aMatrix2[4,5]  +
               aMatrix1[6,2] * aMatrix2[4,6]  +
               aMatrix1[7,2] * aMatrix2[4,7]  +
               aMatrix1[8,2] * aMatrix2[4,8]  +
               aMatrix1[9,2] * aMatrix2[4,9]  +
               aMatrix1[10,2] * aMatrix2[4,10]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  +
               aMatrix1[5,3] * aMatrix2[4,5]  +
               aMatrix1[6,3] * aMatrix2[4,6]  +
               aMatrix1[7,3] * aMatrix2[4,7]  +
               aMatrix1[8,3] * aMatrix2[4,8]  +
               aMatrix1[9,3] * aMatrix2[4,9]  +
               aMatrix1[10,3] * aMatrix2[4,10]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  +
               aMatrix1[5,4] * aMatrix2[4,5]  +
               aMatrix1[6,4] * aMatrix2[4,6]  +
               aMatrix1[7,4] * aMatrix2[4,7]  +
               aMatrix1[8,4] * aMatrix2[4,8]  +
               aMatrix1[9,4] * aMatrix2[4,9]  +
               aMatrix1[10,4] * aMatrix2[4,10]  ;

             Result[4,5]:=
               aMatrix1[1,5] * aMatrix2[4,1]  +
               aMatrix1[2,5] * aMatrix2[4,2]  +
               aMatrix1[3,5] * aMatrix2[4,3]  +
               aMatrix1[4,5] * aMatrix2[4,4]  +
               aMatrix1[5,5] * aMatrix2[4,5]  +
               aMatrix1[6,5] * aMatrix2[4,6]  +
               aMatrix1[7,5] * aMatrix2[4,7]  +
               aMatrix1[8,5] * aMatrix2[4,8]  +
               aMatrix1[9,5] * aMatrix2[4,9]  +
               aMatrix1[10,5] * aMatrix2[4,10]  ;

             Result[4,6]:=
               aMatrix1[1,6] * aMatrix2[4,1]  +
               aMatrix1[2,6] * aMatrix2[4,2]  +
               aMatrix1[3,6] * aMatrix2[4,3]  +
               aMatrix1[4,6] * aMatrix2[4,4]  +
               aMatrix1[5,6] * aMatrix2[4,5]  +
               aMatrix1[6,6] * aMatrix2[4,6]  +
               aMatrix1[7,6] * aMatrix2[4,7]  +
               aMatrix1[8,6] * aMatrix2[4,8]  +
               aMatrix1[9,6] * aMatrix2[4,9]  +
               aMatrix1[10,6] * aMatrix2[4,10]  ;

             Result[4,7]:=
               aMatrix1[1,7] * aMatrix2[4,1]  +
               aMatrix1[2,7] * aMatrix2[4,2]  +
               aMatrix1[3,7] * aMatrix2[4,3]  +
               aMatrix1[4,7] * aMatrix2[4,4]  +
               aMatrix1[5,7] * aMatrix2[4,5]  +
               aMatrix1[6,7] * aMatrix2[4,6]  +
               aMatrix1[7,7] * aMatrix2[4,7]  +
               aMatrix1[8,7] * aMatrix2[4,8]  +
               aMatrix1[9,7] * aMatrix2[4,9]  +
               aMatrix1[10,7] * aMatrix2[4,10]  ;

             Result[4,8]:=
               aMatrix1[1,8] * aMatrix2[4,1]  +
               aMatrix1[2,8] * aMatrix2[4,2]  +
               aMatrix1[3,8] * aMatrix2[4,3]  +
               aMatrix1[4,8] * aMatrix2[4,4]  +
               aMatrix1[5,8] * aMatrix2[4,5]  +
               aMatrix1[6,8] * aMatrix2[4,6]  +
               aMatrix1[7,8] * aMatrix2[4,7]  +
               aMatrix1[8,8] * aMatrix2[4,8]  +
               aMatrix1[9,8] * aMatrix2[4,9]  +
               aMatrix1[10,8] * aMatrix2[4,10]  ;

             Result[4,9]:=
               aMatrix1[1,9] * aMatrix2[4,1]  +
               aMatrix1[2,9] * aMatrix2[4,2]  +
               aMatrix1[3,9] * aMatrix2[4,3]  +
               aMatrix1[4,9] * aMatrix2[4,4]  +
               aMatrix1[5,9] * aMatrix2[4,5]  +
               aMatrix1[6,9] * aMatrix2[4,6]  +
               aMatrix1[7,9] * aMatrix2[4,7]  +
               aMatrix1[8,9] * aMatrix2[4,8]  +
               aMatrix1[9,9] * aMatrix2[4,9]  +
               aMatrix1[10,9] * aMatrix2[4,10]  ;

             Result[4,10]:=
               aMatrix1[1,10] * aMatrix2[4,1]  +
               aMatrix1[2,10] * aMatrix2[4,2]  +
               aMatrix1[3,10] * aMatrix2[4,3]  +
               aMatrix1[4,10] * aMatrix2[4,4]  +
               aMatrix1[5,10] * aMatrix2[4,5]  +
               aMatrix1[6,10] * aMatrix2[4,6]  +
               aMatrix1[7,10] * aMatrix2[4,7]  +
               aMatrix1[8,10] * aMatrix2[4,8]  +
               aMatrix1[9,10] * aMatrix2[4,9]  +
               aMatrix1[10,10] * aMatrix2[4,10]  ;


             Result[5,1]:=
               aMatrix1[1,1] * aMatrix2[5,1]  +
               aMatrix1[2,1] * aMatrix2[5,2]  +
               aMatrix1[3,1] * aMatrix2[5,3]  +
               aMatrix1[4,1] * aMatrix2[5,4]  +
               aMatrix1[5,1] * aMatrix2[5,5]  +
               aMatrix1[6,1] * aMatrix2[5,6]  +
               aMatrix1[7,1] * aMatrix2[5,7]  +
               aMatrix1[8,1] * aMatrix2[5,8]  +
               aMatrix1[9,1] * aMatrix2[5,9]  +
               aMatrix1[10,1] * aMatrix2[5,10]  ;

             Result[5,2]:=
               aMatrix1[1,2] * aMatrix2[5,1]  +
               aMatrix1[2,2] * aMatrix2[5,2]  +
               aMatrix1[3,2] * aMatrix2[5,3]  +
               aMatrix1[4,2] * aMatrix2[5,4]  +
               aMatrix1[5,2] * aMatrix2[5,5]  +
               aMatrix1[6,2] * aMatrix2[5,6]  +
               aMatrix1[7,2] * aMatrix2[5,7]  +
               aMatrix1[8,2] * aMatrix2[5,8]  +
               aMatrix1[9,2] * aMatrix2[5,9]  +
               aMatrix1[10,2] * aMatrix2[5,10]  ;

             Result[5,3]:=
               aMatrix1[1,3] * aMatrix2[5,1]  +
               aMatrix1[2,3] * aMatrix2[5,2]  +
               aMatrix1[3,3] * aMatrix2[5,3]  +
               aMatrix1[4,3] * aMatrix2[5,4]  +
               aMatrix1[5,3] * aMatrix2[5,5]  +
               aMatrix1[6,3] * aMatrix2[5,6]  +
               aMatrix1[7,3] * aMatrix2[5,7]  +
               aMatrix1[8,3] * aMatrix2[5,8]  +
               aMatrix1[9,3] * aMatrix2[5,9]  +
               aMatrix1[10,3] * aMatrix2[5,10]  ;

             Result[5,4]:=
               aMatrix1[1,4] * aMatrix2[5,1]  +
               aMatrix1[2,4] * aMatrix2[5,2]  +
               aMatrix1[3,4] * aMatrix2[5,3]  +
               aMatrix1[4,4] * aMatrix2[5,4]  +
               aMatrix1[5,4] * aMatrix2[5,5]  +
               aMatrix1[6,4] * aMatrix2[5,6]  +
               aMatrix1[7,4] * aMatrix2[5,7]  +
               aMatrix1[8,4] * aMatrix2[5,8]  +
               aMatrix1[9,4] * aMatrix2[5,9]  +
               aMatrix1[10,4] * aMatrix2[5,10]  ;

             Result[5,5]:=
               aMatrix1[1,5] * aMatrix2[5,1]  +
               aMatrix1[2,5] * aMatrix2[5,2]  +
               aMatrix1[3,5] * aMatrix2[5,3]  +
               aMatrix1[4,5] * aMatrix2[5,4]  +
               aMatrix1[5,5] * aMatrix2[5,5]  +
               aMatrix1[6,5] * aMatrix2[5,6]  +
               aMatrix1[7,5] * aMatrix2[5,7]  +
               aMatrix1[8,5] * aMatrix2[5,8]  +
               aMatrix1[9,5] * aMatrix2[5,9]  +
               aMatrix1[10,5] * aMatrix2[5,10]  ;

             Result[5,6]:=
               aMatrix1[1,6] * aMatrix2[5,1]  +
               aMatrix1[2,6] * aMatrix2[5,2]  +
               aMatrix1[3,6] * aMatrix2[5,3]  +
               aMatrix1[4,6] * aMatrix2[5,4]  +
               aMatrix1[5,6] * aMatrix2[5,5]  +
               aMatrix1[6,6] * aMatrix2[5,6]  +
               aMatrix1[7,6] * aMatrix2[5,7]  +
               aMatrix1[8,6] * aMatrix2[5,8]  +
               aMatrix1[9,6] * aMatrix2[5,9]  +
               aMatrix1[10,6] * aMatrix2[5,10]  ;

             Result[5,7]:=
               aMatrix1[1,7] * aMatrix2[5,1]  +
               aMatrix1[2,7] * aMatrix2[5,2]  +
               aMatrix1[3,7] * aMatrix2[5,3]  +
               aMatrix1[4,7] * aMatrix2[5,4]  +
               aMatrix1[5,7] * aMatrix2[5,5]  +
               aMatrix1[6,7] * aMatrix2[5,6]  +
               aMatrix1[7,7] * aMatrix2[5,7]  +
               aMatrix1[8,7] * aMatrix2[5,8]  +
               aMatrix1[9,7] * aMatrix2[5,9]  +
               aMatrix1[10,7] * aMatrix2[5,10]  ;

             Result[5,8]:=
               aMatrix1[1,8] * aMatrix2[5,1]  +
               aMatrix1[2,8] * aMatrix2[5,2]  +
               aMatrix1[3,8] * aMatrix2[5,3]  +
               aMatrix1[4,8] * aMatrix2[5,4]  +
               aMatrix1[5,8] * aMatrix2[5,5]  +
               aMatrix1[6,8] * aMatrix2[5,6]  +
               aMatrix1[7,8] * aMatrix2[5,7]  +
               aMatrix1[8,8] * aMatrix2[5,8]  +
               aMatrix1[9,8] * aMatrix2[5,9]  +
               aMatrix1[10,8] * aMatrix2[5,10]  ;

             Result[5,9]:=
               aMatrix1[1,9] * aMatrix2[5,1]  +
               aMatrix1[2,9] * aMatrix2[5,2]  +
               aMatrix1[3,9] * aMatrix2[5,3]  +
               aMatrix1[4,9] * aMatrix2[5,4]  +
               aMatrix1[5,9] * aMatrix2[5,5]  +
               aMatrix1[6,9] * aMatrix2[5,6]  +
               aMatrix1[7,9] * aMatrix2[5,7]  +
               aMatrix1[8,9] * aMatrix2[5,8]  +
               aMatrix1[9,9] * aMatrix2[5,9]  +
               aMatrix1[10,9] * aMatrix2[5,10]  ;

             Result[5,10]:=
               aMatrix1[1,10] * aMatrix2[5,1]  +
               aMatrix1[2,10] * aMatrix2[5,2]  +
               aMatrix1[3,10] * aMatrix2[5,3]  +
               aMatrix1[4,10] * aMatrix2[5,4]  +
               aMatrix1[5,10] * aMatrix2[5,5]  +
               aMatrix1[6,10] * aMatrix2[5,6]  +
               aMatrix1[7,10] * aMatrix2[5,7]  +
               aMatrix1[8,10] * aMatrix2[5,8]  +
               aMatrix1[9,10] * aMatrix2[5,9]  +
               aMatrix1[10,10] * aMatrix2[5,10]  ;


             Result[6,1]:=
               aMatrix1[1,1] * aMatrix2[6,1]  +
               aMatrix1[2,1] * aMatrix2[6,2]  +
               aMatrix1[3,1] * aMatrix2[6,3]  +
               aMatrix1[4,1] * aMatrix2[6,4]  +
               aMatrix1[5,1] * aMatrix2[6,5]  +
               aMatrix1[6,1] * aMatrix2[6,6]  +
               aMatrix1[7,1] * aMatrix2[6,7]  +
               aMatrix1[8,1] * aMatrix2[6,8]  +
               aMatrix1[9,1] * aMatrix2[6,9]  +
               aMatrix1[10,1] * aMatrix2[6,10]  ;

             Result[6,2]:=
               aMatrix1[1,2] * aMatrix2[6,1]  +
               aMatrix1[2,2] * aMatrix2[6,2]  +
               aMatrix1[3,2] * aMatrix2[6,3]  +
               aMatrix1[4,2] * aMatrix2[6,4]  +
               aMatrix1[5,2] * aMatrix2[6,5]  +
               aMatrix1[6,2] * aMatrix2[6,6]  +
               aMatrix1[7,2] * aMatrix2[6,7]  +
               aMatrix1[8,2] * aMatrix2[6,8]  +
               aMatrix1[9,2] * aMatrix2[6,9]  +
               aMatrix1[10,2] * aMatrix2[6,10]  ;

             Result[6,3]:=
               aMatrix1[1,3] * aMatrix2[6,1]  +
               aMatrix1[2,3] * aMatrix2[6,2]  +
               aMatrix1[3,3] * aMatrix2[6,3]  +
               aMatrix1[4,3] * aMatrix2[6,4]  +
               aMatrix1[5,3] * aMatrix2[6,5]  +
               aMatrix1[6,3] * aMatrix2[6,6]  +
               aMatrix1[7,3] * aMatrix2[6,7]  +
               aMatrix1[8,3] * aMatrix2[6,8]  +
               aMatrix1[9,3] * aMatrix2[6,9]  +
               aMatrix1[10,3] * aMatrix2[6,10]  ;

             Result[6,4]:=
               aMatrix1[1,4] * aMatrix2[6,1]  +
               aMatrix1[2,4] * aMatrix2[6,2]  +
               aMatrix1[3,4] * aMatrix2[6,3]  +
               aMatrix1[4,4] * aMatrix2[6,4]  +
               aMatrix1[5,4] * aMatrix2[6,5]  +
               aMatrix1[6,4] * aMatrix2[6,6]  +
               aMatrix1[7,4] * aMatrix2[6,7]  +
               aMatrix1[8,4] * aMatrix2[6,8]  +
               aMatrix1[9,4] * aMatrix2[6,9]  +
               aMatrix1[10,4] * aMatrix2[6,10]  ;

             Result[6,5]:=
               aMatrix1[1,5] * aMatrix2[6,1]  +
               aMatrix1[2,5] * aMatrix2[6,2]  +
               aMatrix1[3,5] * aMatrix2[6,3]  +
               aMatrix1[4,5] * aMatrix2[6,4]  +
               aMatrix1[5,5] * aMatrix2[6,5]  +
               aMatrix1[6,5] * aMatrix2[6,6]  +
               aMatrix1[7,5] * aMatrix2[6,7]  +
               aMatrix1[8,5] * aMatrix2[6,8]  +
               aMatrix1[9,5] * aMatrix2[6,9]  +
               aMatrix1[10,5] * aMatrix2[6,10]  ;

             Result[6,6]:=
               aMatrix1[1,6] * aMatrix2[6,1]  +
               aMatrix1[2,6] * aMatrix2[6,2]  +
               aMatrix1[3,6] * aMatrix2[6,3]  +
               aMatrix1[4,6] * aMatrix2[6,4]  +
               aMatrix1[5,6] * aMatrix2[6,5]  +
               aMatrix1[6,6] * aMatrix2[6,6]  +
               aMatrix1[7,6] * aMatrix2[6,7]  +
               aMatrix1[8,6] * aMatrix2[6,8]  +
               aMatrix1[9,6] * aMatrix2[6,9]  +
               aMatrix1[10,6] * aMatrix2[6,10]  ;

             Result[6,7]:=
               aMatrix1[1,7] * aMatrix2[6,1]  +
               aMatrix1[2,7] * aMatrix2[6,2]  +
               aMatrix1[3,7] * aMatrix2[6,3]  +
               aMatrix1[4,7] * aMatrix2[6,4]  +
               aMatrix1[5,7] * aMatrix2[6,5]  +
               aMatrix1[6,7] * aMatrix2[6,6]  +
               aMatrix1[7,7] * aMatrix2[6,7]  +
               aMatrix1[8,7] * aMatrix2[6,8]  +
               aMatrix1[9,7] * aMatrix2[6,9]  +
               aMatrix1[10,7] * aMatrix2[6,10]  ;

             Result[6,8]:=
               aMatrix1[1,8] * aMatrix2[6,1]  +
               aMatrix1[2,8] * aMatrix2[6,2]  +
               aMatrix1[3,8] * aMatrix2[6,3]  +
               aMatrix1[4,8] * aMatrix2[6,4]  +
               aMatrix1[5,8] * aMatrix2[6,5]  +
               aMatrix1[6,8] * aMatrix2[6,6]  +
               aMatrix1[7,8] * aMatrix2[6,7]  +
               aMatrix1[8,8] * aMatrix2[6,8]  +
               aMatrix1[9,8] * aMatrix2[6,9]  +
               aMatrix1[10,8] * aMatrix2[6,10]  ;

             Result[6,9]:=
               aMatrix1[1,9] * aMatrix2[6,1]  +
               aMatrix1[2,9] * aMatrix2[6,2]  +
               aMatrix1[3,9] * aMatrix2[6,3]  +
               aMatrix1[4,9] * aMatrix2[6,4]  +
               aMatrix1[5,9] * aMatrix2[6,5]  +
               aMatrix1[6,9] * aMatrix2[6,6]  +
               aMatrix1[7,9] * aMatrix2[6,7]  +
               aMatrix1[8,9] * aMatrix2[6,8]  +
               aMatrix1[9,9] * aMatrix2[6,9]  +
               aMatrix1[10,9] * aMatrix2[6,10]  ;

             Result[6,10]:=
               aMatrix1[1,10] * aMatrix2[6,1]  +
               aMatrix1[2,10] * aMatrix2[6,2]  +
               aMatrix1[3,10] * aMatrix2[6,3]  +
               aMatrix1[4,10] * aMatrix2[6,4]  +
               aMatrix1[5,10] * aMatrix2[6,5]  +
               aMatrix1[6,10] * aMatrix2[6,6]  +
               aMatrix1[7,10] * aMatrix2[6,7]  +
               aMatrix1[8,10] * aMatrix2[6,8]  +
               aMatrix1[9,10] * aMatrix2[6,9]  +
               aMatrix1[10,10] * aMatrix2[6,10]  ;


             Result[7,1]:=
               aMatrix1[1,1] * aMatrix2[7,1]  +
               aMatrix1[2,1] * aMatrix2[7,2]  +
               aMatrix1[3,1] * aMatrix2[7,3]  +
               aMatrix1[4,1] * aMatrix2[7,4]  +
               aMatrix1[5,1] * aMatrix2[7,5]  +
               aMatrix1[6,1] * aMatrix2[7,6]  +
               aMatrix1[7,1] * aMatrix2[7,7]  +
               aMatrix1[8,1] * aMatrix2[7,8]  +
               aMatrix1[9,1] * aMatrix2[7,9]  +
               aMatrix1[10,1] * aMatrix2[7,10]  ;

             Result[7,2]:=
               aMatrix1[1,2] * aMatrix2[7,1]  +
               aMatrix1[2,2] * aMatrix2[7,2]  +
               aMatrix1[3,2] * aMatrix2[7,3]  +
               aMatrix1[4,2] * aMatrix2[7,4]  +
               aMatrix1[5,2] * aMatrix2[7,5]  +
               aMatrix1[6,2] * aMatrix2[7,6]  +
               aMatrix1[7,2] * aMatrix2[7,7]  +
               aMatrix1[8,2] * aMatrix2[7,8]  +
               aMatrix1[9,2] * aMatrix2[7,9]  +
               aMatrix1[10,2] * aMatrix2[7,10]  ;

             Result[7,3]:=
               aMatrix1[1,3] * aMatrix2[7,1]  +
               aMatrix1[2,3] * aMatrix2[7,2]  +
               aMatrix1[3,3] * aMatrix2[7,3]  +
               aMatrix1[4,3] * aMatrix2[7,4]  +
               aMatrix1[5,3] * aMatrix2[7,5]  +
               aMatrix1[6,3] * aMatrix2[7,6]  +
               aMatrix1[7,3] * aMatrix2[7,7]  +
               aMatrix1[8,3] * aMatrix2[7,8]  +
               aMatrix1[9,3] * aMatrix2[7,9]  +
               aMatrix1[10,3] * aMatrix2[7,10]  ;

             Result[7,4]:=
               aMatrix1[1,4] * aMatrix2[7,1]  +
               aMatrix1[2,4] * aMatrix2[7,2]  +
               aMatrix1[3,4] * aMatrix2[7,3]  +
               aMatrix1[4,4] * aMatrix2[7,4]  +
               aMatrix1[5,4] * aMatrix2[7,5]  +
               aMatrix1[6,4] * aMatrix2[7,6]  +
               aMatrix1[7,4] * aMatrix2[7,7]  +
               aMatrix1[8,4] * aMatrix2[7,8]  +
               aMatrix1[9,4] * aMatrix2[7,9]  +
               aMatrix1[10,4] * aMatrix2[7,10]  ;

             Result[7,5]:=
               aMatrix1[1,5] * aMatrix2[7,1]  +
               aMatrix1[2,5] * aMatrix2[7,2]  +
               aMatrix1[3,5] * aMatrix2[7,3]  +
               aMatrix1[4,5] * aMatrix2[7,4]  +
               aMatrix1[5,5] * aMatrix2[7,5]  +
               aMatrix1[6,5] * aMatrix2[7,6]  +
               aMatrix1[7,5] * aMatrix2[7,7]  +
               aMatrix1[8,5] * aMatrix2[7,8]  +
               aMatrix1[9,5] * aMatrix2[7,9]  +
               aMatrix1[10,5] * aMatrix2[7,10]  ;

             Result[7,6]:=
               aMatrix1[1,6] * aMatrix2[7,1]  +
               aMatrix1[2,6] * aMatrix2[7,2]  +
               aMatrix1[3,6] * aMatrix2[7,3]  +
               aMatrix1[4,6] * aMatrix2[7,4]  +
               aMatrix1[5,6] * aMatrix2[7,5]  +
               aMatrix1[6,6] * aMatrix2[7,6]  +
               aMatrix1[7,6] * aMatrix2[7,7]  +
               aMatrix1[8,6] * aMatrix2[7,8]  +
               aMatrix1[9,6] * aMatrix2[7,9]  +
               aMatrix1[10,6] * aMatrix2[7,10]  ;

             Result[7,7]:=
               aMatrix1[1,7] * aMatrix2[7,1]  +
               aMatrix1[2,7] * aMatrix2[7,2]  +
               aMatrix1[3,7] * aMatrix2[7,3]  +
               aMatrix1[4,7] * aMatrix2[7,4]  +
               aMatrix1[5,7] * aMatrix2[7,5]  +
               aMatrix1[6,7] * aMatrix2[7,6]  +
               aMatrix1[7,7] * aMatrix2[7,7]  +
               aMatrix1[8,7] * aMatrix2[7,8]  +
               aMatrix1[9,7] * aMatrix2[7,9]  +
               aMatrix1[10,7] * aMatrix2[7,10]  ;

             Result[7,8]:=
               aMatrix1[1,8] * aMatrix2[7,1]  +
               aMatrix1[2,8] * aMatrix2[7,2]  +
               aMatrix1[3,8] * aMatrix2[7,3]  +
               aMatrix1[4,8] * aMatrix2[7,4]  +
               aMatrix1[5,8] * aMatrix2[7,5]  +
               aMatrix1[6,8] * aMatrix2[7,6]  +
               aMatrix1[7,8] * aMatrix2[7,7]  +
               aMatrix1[8,8] * aMatrix2[7,8]  +
               aMatrix1[9,8] * aMatrix2[7,9]  +
               aMatrix1[10,8] * aMatrix2[7,10]  ;

             Result[7,9]:=
               aMatrix1[1,9] * aMatrix2[7,1]  +
               aMatrix1[2,9] * aMatrix2[7,2]  +
               aMatrix1[3,9] * aMatrix2[7,3]  +
               aMatrix1[4,9] * aMatrix2[7,4]  +
               aMatrix1[5,9] * aMatrix2[7,5]  +
               aMatrix1[6,9] * aMatrix2[7,6]  +
               aMatrix1[7,9] * aMatrix2[7,7]  +
               aMatrix1[8,9] * aMatrix2[7,8]  +
               aMatrix1[9,9] * aMatrix2[7,9]  +
               aMatrix1[10,9] * aMatrix2[7,10]  ;

             Result[7,10]:=
               aMatrix1[1,10] * aMatrix2[7,1]  +
               aMatrix1[2,10] * aMatrix2[7,2]  +
               aMatrix1[3,10] * aMatrix2[7,3]  +
               aMatrix1[4,10] * aMatrix2[7,4]  +
               aMatrix1[5,10] * aMatrix2[7,5]  +
               aMatrix1[6,10] * aMatrix2[7,6]  +
               aMatrix1[7,10] * aMatrix2[7,7]  +
               aMatrix1[8,10] * aMatrix2[7,8]  +
               aMatrix1[9,10] * aMatrix2[7,9]  +
               aMatrix1[10,10] * aMatrix2[7,10]  ;


             Result[8,1]:=
               aMatrix1[1,1] * aMatrix2[8,1]  +
               aMatrix1[2,1] * aMatrix2[8,2]  +
               aMatrix1[3,1] * aMatrix2[8,3]  +
               aMatrix1[4,1] * aMatrix2[8,4]  +
               aMatrix1[5,1] * aMatrix2[8,5]  +
               aMatrix1[6,1] * aMatrix2[8,6]  +
               aMatrix1[7,1] * aMatrix2[8,7]  +
               aMatrix1[8,1] * aMatrix2[8,8]  +
               aMatrix1[9,1] * aMatrix2[8,9]  +
               aMatrix1[10,1] * aMatrix2[8,10]  ;

             Result[8,2]:=
               aMatrix1[1,2] * aMatrix2[8,1]  +
               aMatrix1[2,2] * aMatrix2[8,2]  +
               aMatrix1[3,2] * aMatrix2[8,3]  +
               aMatrix1[4,2] * aMatrix2[8,4]  +
               aMatrix1[5,2] * aMatrix2[8,5]  +
               aMatrix1[6,2] * aMatrix2[8,6]  +
               aMatrix1[7,2] * aMatrix2[8,7]  +
               aMatrix1[8,2] * aMatrix2[8,8]  +
               aMatrix1[9,2] * aMatrix2[8,9]  +
               aMatrix1[10,2] * aMatrix2[8,10]  ;

             Result[8,3]:=
               aMatrix1[1,3] * aMatrix2[8,1]  +
               aMatrix1[2,3] * aMatrix2[8,2]  +
               aMatrix1[3,3] * aMatrix2[8,3]  +
               aMatrix1[4,3] * aMatrix2[8,4]  +
               aMatrix1[5,3] * aMatrix2[8,5]  +
               aMatrix1[6,3] * aMatrix2[8,6]  +
               aMatrix1[7,3] * aMatrix2[8,7]  +
               aMatrix1[8,3] * aMatrix2[8,8]  +
               aMatrix1[9,3] * aMatrix2[8,9]  +
               aMatrix1[10,3] * aMatrix2[8,10]  ;

             Result[8,4]:=
               aMatrix1[1,4] * aMatrix2[8,1]  +
               aMatrix1[2,4] * aMatrix2[8,2]  +
               aMatrix1[3,4] * aMatrix2[8,3]  +
               aMatrix1[4,4] * aMatrix2[8,4]  +
               aMatrix1[5,4] * aMatrix2[8,5]  +
               aMatrix1[6,4] * aMatrix2[8,6]  +
               aMatrix1[7,4] * aMatrix2[8,7]  +
               aMatrix1[8,4] * aMatrix2[8,8]  +
               aMatrix1[9,4] * aMatrix2[8,9]  +
               aMatrix1[10,4] * aMatrix2[8,10]  ;

             Result[8,5]:=
               aMatrix1[1,5] * aMatrix2[8,1]  +
               aMatrix1[2,5] * aMatrix2[8,2]  +
               aMatrix1[3,5] * aMatrix2[8,3]  +
               aMatrix1[4,5] * aMatrix2[8,4]  +
               aMatrix1[5,5] * aMatrix2[8,5]  +
               aMatrix1[6,5] * aMatrix2[8,6]  +
               aMatrix1[7,5] * aMatrix2[8,7]  +
               aMatrix1[8,5] * aMatrix2[8,8]  +
               aMatrix1[9,5] * aMatrix2[8,9]  +
               aMatrix1[10,5] * aMatrix2[8,10]  ;

             Result[8,6]:=
               aMatrix1[1,6] * aMatrix2[8,1]  +
               aMatrix1[2,6] * aMatrix2[8,2]  +
               aMatrix1[3,6] * aMatrix2[8,3]  +
               aMatrix1[4,6] * aMatrix2[8,4]  +
               aMatrix1[5,6] * aMatrix2[8,5]  +
               aMatrix1[6,6] * aMatrix2[8,6]  +
               aMatrix1[7,6] * aMatrix2[8,7]  +
               aMatrix1[8,6] * aMatrix2[8,8]  +
               aMatrix1[9,6] * aMatrix2[8,9]  +
               aMatrix1[10,6] * aMatrix2[8,10]  ;

             Result[8,7]:=
               aMatrix1[1,7] * aMatrix2[8,1]  +
               aMatrix1[2,7] * aMatrix2[8,2]  +
               aMatrix1[3,7] * aMatrix2[8,3]  +
               aMatrix1[4,7] * aMatrix2[8,4]  +
               aMatrix1[5,7] * aMatrix2[8,5]  +
               aMatrix1[6,7] * aMatrix2[8,6]  +
               aMatrix1[7,7] * aMatrix2[8,7]  +
               aMatrix1[8,7] * aMatrix2[8,8]  +
               aMatrix1[9,7] * aMatrix2[8,9]  +
               aMatrix1[10,7] * aMatrix2[8,10]  ;

             Result[8,8]:=
               aMatrix1[1,8] * aMatrix2[8,1]  +
               aMatrix1[2,8] * aMatrix2[8,2]  +
               aMatrix1[3,8] * aMatrix2[8,3]  +
               aMatrix1[4,8] * aMatrix2[8,4]  +
               aMatrix1[5,8] * aMatrix2[8,5]  +
               aMatrix1[6,8] * aMatrix2[8,6]  +
               aMatrix1[7,8] * aMatrix2[8,7]  +
               aMatrix1[8,8] * aMatrix2[8,8]  +
               aMatrix1[9,8] * aMatrix2[8,9]  +
               aMatrix1[10,8] * aMatrix2[8,10]  ;

             Result[8,9]:=
               aMatrix1[1,9] * aMatrix2[8,1]  +
               aMatrix1[2,9] * aMatrix2[8,2]  +
               aMatrix1[3,9] * aMatrix2[8,3]  +
               aMatrix1[4,9] * aMatrix2[8,4]  +
               aMatrix1[5,9] * aMatrix2[8,5]  +
               aMatrix1[6,9] * aMatrix2[8,6]  +
               aMatrix1[7,9] * aMatrix2[8,7]  +
               aMatrix1[8,9] * aMatrix2[8,8]  +
               aMatrix1[9,9] * aMatrix2[8,9]  +
               aMatrix1[10,9] * aMatrix2[8,10]  ;

             Result[8,10]:=
               aMatrix1[1,10] * aMatrix2[8,1]  +
               aMatrix1[2,10] * aMatrix2[8,2]  +
               aMatrix1[3,10] * aMatrix2[8,3]  +
               aMatrix1[4,10] * aMatrix2[8,4]  +
               aMatrix1[5,10] * aMatrix2[8,5]  +
               aMatrix1[6,10] * aMatrix2[8,6]  +
               aMatrix1[7,10] * aMatrix2[8,7]  +
               aMatrix1[8,10] * aMatrix2[8,8]  +
               aMatrix1[9,10] * aMatrix2[8,9]  +
               aMatrix1[10,10] * aMatrix2[8,10]  ;


             Result[9,1]:=
               aMatrix1[1,1] * aMatrix2[9,1]  +
               aMatrix1[2,1] * aMatrix2[9,2]  +
               aMatrix1[3,1] * aMatrix2[9,3]  +
               aMatrix1[4,1] * aMatrix2[9,4]  +
               aMatrix1[5,1] * aMatrix2[9,5]  +
               aMatrix1[6,1] * aMatrix2[9,6]  +
               aMatrix1[7,1] * aMatrix2[9,7]  +
               aMatrix1[8,1] * aMatrix2[9,8]  +
               aMatrix1[9,1] * aMatrix2[9,9]  +
               aMatrix1[10,1] * aMatrix2[9,10]  ;

             Result[9,2]:=
               aMatrix1[1,2] * aMatrix2[9,1]  +
               aMatrix1[2,2] * aMatrix2[9,2]  +
               aMatrix1[3,2] * aMatrix2[9,3]  +
               aMatrix1[4,2] * aMatrix2[9,4]  +
               aMatrix1[5,2] * aMatrix2[9,5]  +
               aMatrix1[6,2] * aMatrix2[9,6]  +
               aMatrix1[7,2] * aMatrix2[9,7]  +
               aMatrix1[8,2] * aMatrix2[9,8]  +
               aMatrix1[9,2] * aMatrix2[9,9]  +
               aMatrix1[10,2] * aMatrix2[9,10]  ;

             Result[9,3]:=
               aMatrix1[1,3] * aMatrix2[9,1]  +
               aMatrix1[2,3] * aMatrix2[9,2]  +
               aMatrix1[3,3] * aMatrix2[9,3]  +
               aMatrix1[4,3] * aMatrix2[9,4]  +
               aMatrix1[5,3] * aMatrix2[9,5]  +
               aMatrix1[6,3] * aMatrix2[9,6]  +
               aMatrix1[7,3] * aMatrix2[9,7]  +
               aMatrix1[8,3] * aMatrix2[9,8]  +
               aMatrix1[9,3] * aMatrix2[9,9]  +
               aMatrix1[10,3] * aMatrix2[9,10]  ;

             Result[9,4]:=
               aMatrix1[1,4] * aMatrix2[9,1]  +
               aMatrix1[2,4] * aMatrix2[9,2]  +
               aMatrix1[3,4] * aMatrix2[9,3]  +
               aMatrix1[4,4] * aMatrix2[9,4]  +
               aMatrix1[5,4] * aMatrix2[9,5]  +
               aMatrix1[6,4] * aMatrix2[9,6]  +
               aMatrix1[7,4] * aMatrix2[9,7]  +
               aMatrix1[8,4] * aMatrix2[9,8]  +
               aMatrix1[9,4] * aMatrix2[9,9]  +
               aMatrix1[10,4] * aMatrix2[9,10]  ;

             Result[9,5]:=
               aMatrix1[1,5] * aMatrix2[9,1]  +
               aMatrix1[2,5] * aMatrix2[9,2]  +
               aMatrix1[3,5] * aMatrix2[9,3]  +
               aMatrix1[4,5] * aMatrix2[9,4]  +
               aMatrix1[5,5] * aMatrix2[9,5]  +
               aMatrix1[6,5] * aMatrix2[9,6]  +
               aMatrix1[7,5] * aMatrix2[9,7]  +
               aMatrix1[8,5] * aMatrix2[9,8]  +
               aMatrix1[9,5] * aMatrix2[9,9]  +
               aMatrix1[10,5] * aMatrix2[9,10]  ;

             Result[9,6]:=
               aMatrix1[1,6] * aMatrix2[9,1]  +
               aMatrix1[2,6] * aMatrix2[9,2]  +
               aMatrix1[3,6] * aMatrix2[9,3]  +
               aMatrix1[4,6] * aMatrix2[9,4]  +
               aMatrix1[5,6] * aMatrix2[9,5]  +
               aMatrix1[6,6] * aMatrix2[9,6]  +
               aMatrix1[7,6] * aMatrix2[9,7]  +
               aMatrix1[8,6] * aMatrix2[9,8]  +
               aMatrix1[9,6] * aMatrix2[9,9]  +
               aMatrix1[10,6] * aMatrix2[9,10]  ;

             Result[9,7]:=
               aMatrix1[1,7] * aMatrix2[9,1]  +
               aMatrix1[2,7] * aMatrix2[9,2]  +
               aMatrix1[3,7] * aMatrix2[9,3]  +
               aMatrix1[4,7] * aMatrix2[9,4]  +
               aMatrix1[5,7] * aMatrix2[9,5]  +
               aMatrix1[6,7] * aMatrix2[9,6]  +
               aMatrix1[7,7] * aMatrix2[9,7]  +
               aMatrix1[8,7] * aMatrix2[9,8]  +
               aMatrix1[9,7] * aMatrix2[9,9]  +
               aMatrix1[10,7] * aMatrix2[9,10]  ;

             Result[9,8]:=
               aMatrix1[1,8] * aMatrix2[9,1]  +
               aMatrix1[2,8] * aMatrix2[9,2]  +
               aMatrix1[3,8] * aMatrix2[9,3]  +
               aMatrix1[4,8] * aMatrix2[9,4]  +
               aMatrix1[5,8] * aMatrix2[9,5]  +
               aMatrix1[6,8] * aMatrix2[9,6]  +
               aMatrix1[7,8] * aMatrix2[9,7]  +
               aMatrix1[8,8] * aMatrix2[9,8]  +
               aMatrix1[9,8] * aMatrix2[9,9]  +
               aMatrix1[10,8] * aMatrix2[9,10]  ;

             Result[9,9]:=
               aMatrix1[1,9] * aMatrix2[9,1]  +
               aMatrix1[2,9] * aMatrix2[9,2]  +
               aMatrix1[3,9] * aMatrix2[9,3]  +
               aMatrix1[4,9] * aMatrix2[9,4]  +
               aMatrix1[5,9] * aMatrix2[9,5]  +
               aMatrix1[6,9] * aMatrix2[9,6]  +
               aMatrix1[7,9] * aMatrix2[9,7]  +
               aMatrix1[8,9] * aMatrix2[9,8]  +
               aMatrix1[9,9] * aMatrix2[9,9]  +
               aMatrix1[10,9] * aMatrix2[9,10]  ;

             Result[9,10]:=
               aMatrix1[1,10] * aMatrix2[9,1]  +
               aMatrix1[2,10] * aMatrix2[9,2]  +
               aMatrix1[3,10] * aMatrix2[9,3]  +
               aMatrix1[4,10] * aMatrix2[9,4]  +
               aMatrix1[5,10] * aMatrix2[9,5]  +
               aMatrix1[6,10] * aMatrix2[9,6]  +
               aMatrix1[7,10] * aMatrix2[9,7]  +
               aMatrix1[8,10] * aMatrix2[9,8]  +
               aMatrix1[9,10] * aMatrix2[9,9]  +
               aMatrix1[10,10] * aMatrix2[9,10]  ;


             Result[10,1]:=
               aMatrix1[1,1] * aMatrix2[10,1]  +
               aMatrix1[2,1] * aMatrix2[10,2]  +
               aMatrix1[3,1] * aMatrix2[10,3]  +
               aMatrix1[4,1] * aMatrix2[10,4]  +
               aMatrix1[5,1] * aMatrix2[10,5]  +
               aMatrix1[6,1] * aMatrix2[10,6]  +
               aMatrix1[7,1] * aMatrix2[10,7]  +
               aMatrix1[8,1] * aMatrix2[10,8]  +
               aMatrix1[9,1] * aMatrix2[10,9]  +
               aMatrix1[10,1] * aMatrix2[10,10]  ;

             Result[10,2]:=
               aMatrix1[1,2] * aMatrix2[10,1]  +
               aMatrix1[2,2] * aMatrix2[10,2]  +
               aMatrix1[3,2] * aMatrix2[10,3]  +
               aMatrix1[4,2] * aMatrix2[10,4]  +
               aMatrix1[5,2] * aMatrix2[10,5]  +
               aMatrix1[6,2] * aMatrix2[10,6]  +
               aMatrix1[7,2] * aMatrix2[10,7]  +
               aMatrix1[8,2] * aMatrix2[10,8]  +
               aMatrix1[9,2] * aMatrix2[10,9]  +
               aMatrix1[10,2] * aMatrix2[10,10]  ;

             Result[10,3]:=
               aMatrix1[1,3] * aMatrix2[10,1]  +
               aMatrix1[2,3] * aMatrix2[10,2]  +
               aMatrix1[3,3] * aMatrix2[10,3]  +
               aMatrix1[4,3] * aMatrix2[10,4]  +
               aMatrix1[5,3] * aMatrix2[10,5]  +
               aMatrix1[6,3] * aMatrix2[10,6]  +
               aMatrix1[7,3] * aMatrix2[10,7]  +
               aMatrix1[8,3] * aMatrix2[10,8]  +
               aMatrix1[9,3] * aMatrix2[10,9]  +
               aMatrix1[10,3] * aMatrix2[10,10]  ;

             Result[10,4]:=
               aMatrix1[1,4] * aMatrix2[10,1]  +
               aMatrix1[2,4] * aMatrix2[10,2]  +
               aMatrix1[3,4] * aMatrix2[10,3]  +
               aMatrix1[4,4] * aMatrix2[10,4]  +
               aMatrix1[5,4] * aMatrix2[10,5]  +
               aMatrix1[6,4] * aMatrix2[10,6]  +
               aMatrix1[7,4] * aMatrix2[10,7]  +
               aMatrix1[8,4] * aMatrix2[10,8]  +
               aMatrix1[9,4] * aMatrix2[10,9]  +
               aMatrix1[10,4] * aMatrix2[10,10]  ;

             Result[10,5]:=
               aMatrix1[1,5] * aMatrix2[10,1]  +
               aMatrix1[2,5] * aMatrix2[10,2]  +
               aMatrix1[3,5] * aMatrix2[10,3]  +
               aMatrix1[4,5] * aMatrix2[10,4]  +
               aMatrix1[5,5] * aMatrix2[10,5]  +
               aMatrix1[6,5] * aMatrix2[10,6]  +
               aMatrix1[7,5] * aMatrix2[10,7]  +
               aMatrix1[8,5] * aMatrix2[10,8]  +
               aMatrix1[9,5] * aMatrix2[10,9]  +
               aMatrix1[10,5] * aMatrix2[10,10]  ;

             Result[10,6]:=
               aMatrix1[1,6] * aMatrix2[10,1]  +
               aMatrix1[2,6] * aMatrix2[10,2]  +
               aMatrix1[3,6] * aMatrix2[10,3]  +
               aMatrix1[4,6] * aMatrix2[10,4]  +
               aMatrix1[5,6] * aMatrix2[10,5]  +
               aMatrix1[6,6] * aMatrix2[10,6]  +
               aMatrix1[7,6] * aMatrix2[10,7]  +
               aMatrix1[8,6] * aMatrix2[10,8]  +
               aMatrix1[9,6] * aMatrix2[10,9]  +
               aMatrix1[10,6] * aMatrix2[10,10]  ;

             Result[10,7]:=
               aMatrix1[1,7] * aMatrix2[10,1]  +
               aMatrix1[2,7] * aMatrix2[10,2]  +
               aMatrix1[3,7] * aMatrix2[10,3]  +
               aMatrix1[4,7] * aMatrix2[10,4]  +
               aMatrix1[5,7] * aMatrix2[10,5]  +
               aMatrix1[6,7] * aMatrix2[10,6]  +
               aMatrix1[7,7] * aMatrix2[10,7]  +
               aMatrix1[8,7] * aMatrix2[10,8]  +
               aMatrix1[9,7] * aMatrix2[10,9]  +
               aMatrix1[10,7] * aMatrix2[10,10]  ;

             Result[10,8]:=
               aMatrix1[1,8] * aMatrix2[10,1]  +
               aMatrix1[2,8] * aMatrix2[10,2]  +
               aMatrix1[3,8] * aMatrix2[10,3]  +
               aMatrix1[4,8] * aMatrix2[10,4]  +
               aMatrix1[5,8] * aMatrix2[10,5]  +
               aMatrix1[6,8] * aMatrix2[10,6]  +
               aMatrix1[7,8] * aMatrix2[10,7]  +
               aMatrix1[8,8] * aMatrix2[10,8]  +
               aMatrix1[9,8] * aMatrix2[10,9]  +
               aMatrix1[10,8] * aMatrix2[10,10]  ;

             Result[10,9]:=
               aMatrix1[1,9] * aMatrix2[10,1]  +
               aMatrix1[2,9] * aMatrix2[10,2]  +
               aMatrix1[3,9] * aMatrix2[10,3]  +
               aMatrix1[4,9] * aMatrix2[10,4]  +
               aMatrix1[5,9] * aMatrix2[10,5]  +
               aMatrix1[6,9] * aMatrix2[10,6]  +
               aMatrix1[7,9] * aMatrix2[10,7]  +
               aMatrix1[8,9] * aMatrix2[10,8]  +
               aMatrix1[9,9] * aMatrix2[10,9]  +
               aMatrix1[10,9] * aMatrix2[10,10]  ;

             Result[10,10]:=
               aMatrix1[1,10] * aMatrix2[10,1]  +
               aMatrix1[2,10] * aMatrix2[10,2]  +
               aMatrix1[3,10] * aMatrix2[10,3]  +
               aMatrix1[4,10] * aMatrix2[10,4]  +
               aMatrix1[5,10] * aMatrix2[10,5]  +
               aMatrix1[6,10] * aMatrix2[10,6]  +
               aMatrix1[7,10] * aMatrix2[10,7]  +
               aMatrix1[8,10] * aMatrix2[10,8]  +
               aMatrix1[9,10] * aMatrix2[10,9]  +
               aMatrix1[10,10] * aMatrix2[10,10]  ;
             end; // 10

          9: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  +
               aMatrix1[5,1] * aMatrix2[1,5]  +
               aMatrix1[6,1] * aMatrix2[1,6]  +
               aMatrix1[7,1] * aMatrix2[1,7]  +
               aMatrix1[8,1] * aMatrix2[1,8]  +
               aMatrix1[9,1] * aMatrix2[1,9]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  +
               aMatrix1[5,2] * aMatrix2[1,5]  +
               aMatrix1[6,2] * aMatrix2[1,6]  +
               aMatrix1[7,2] * aMatrix2[1,7]  +
               aMatrix1[8,2] * aMatrix2[1,8]  +
               aMatrix1[9,2] * aMatrix2[1,9]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  +
               aMatrix1[5,3] * aMatrix2[1,5]  +
               aMatrix1[6,3] * aMatrix2[1,6]  +
               aMatrix1[7,3] * aMatrix2[1,7]  +
               aMatrix1[8,3] * aMatrix2[1,8]  +
               aMatrix1[9,3] * aMatrix2[1,9]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  +
               aMatrix1[5,4] * aMatrix2[1,5]  +
               aMatrix1[6,4] * aMatrix2[1,6]  +
               aMatrix1[7,4] * aMatrix2[1,7]  +
               aMatrix1[8,4] * aMatrix2[1,8]  +
               aMatrix1[9,4] * aMatrix2[1,9]  ;

             Result[1,5]:=
               aMatrix1[1,5] * aMatrix2[1,1]  +
               aMatrix1[2,5] * aMatrix2[1,2]  +
               aMatrix1[3,5] * aMatrix2[1,3]  +
               aMatrix1[4,5] * aMatrix2[1,4]  +
               aMatrix1[5,5] * aMatrix2[1,5]  +
               aMatrix1[6,5] * aMatrix2[1,6]  +
               aMatrix1[7,5] * aMatrix2[1,7]  +
               aMatrix1[8,5] * aMatrix2[1,8]  +
               aMatrix1[9,5] * aMatrix2[1,9]  ;

             Result[1,6]:=
               aMatrix1[1,6] * aMatrix2[1,1]  +
               aMatrix1[2,6] * aMatrix2[1,2]  +
               aMatrix1[3,6] * aMatrix2[1,3]  +
               aMatrix1[4,6] * aMatrix2[1,4]  +
               aMatrix1[5,6] * aMatrix2[1,5]  +
               aMatrix1[6,6] * aMatrix2[1,6]  +
               aMatrix1[7,6] * aMatrix2[1,7]  +
               aMatrix1[8,6] * aMatrix2[1,8]  +
               aMatrix1[9,6] * aMatrix2[1,9]  ;

             Result[1,7]:=
               aMatrix1[1,7] * aMatrix2[1,1]  +
               aMatrix1[2,7] * aMatrix2[1,2]  +
               aMatrix1[3,7] * aMatrix2[1,3]  +
               aMatrix1[4,7] * aMatrix2[1,4]  +
               aMatrix1[5,7] * aMatrix2[1,5]  +
               aMatrix1[6,7] * aMatrix2[1,6]  +
               aMatrix1[7,7] * aMatrix2[1,7]  +
               aMatrix1[8,7] * aMatrix2[1,8]  +
               aMatrix1[9,7] * aMatrix2[1,9]  ;

             Result[1,8]:=
               aMatrix1[1,8] * aMatrix2[1,1]  +
               aMatrix1[2,8] * aMatrix2[1,2]  +
               aMatrix1[3,8] * aMatrix2[1,3]  +
               aMatrix1[4,8] * aMatrix2[1,4]  +
               aMatrix1[5,8] * aMatrix2[1,5]  +
               aMatrix1[6,8] * aMatrix2[1,6]  +
               aMatrix1[7,8] * aMatrix2[1,7]  +
               aMatrix1[8,8] * aMatrix2[1,8]  +
               aMatrix1[9,8] * aMatrix2[1,9]  ;

             Result[1,9]:=
               aMatrix1[1,9] * aMatrix2[1,1]  +
               aMatrix1[2,9] * aMatrix2[1,2]  +
               aMatrix1[3,9] * aMatrix2[1,3]  +
               aMatrix1[4,9] * aMatrix2[1,4]  +
               aMatrix1[5,9] * aMatrix2[1,5]  +
               aMatrix1[6,9] * aMatrix2[1,6]  +
               aMatrix1[7,9] * aMatrix2[1,7]  +
               aMatrix1[8,9] * aMatrix2[1,8]  +
               aMatrix1[9,9] * aMatrix2[1,9]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  +
               aMatrix1[5,1] * aMatrix2[2,5]  +
               aMatrix1[6,1] * aMatrix2[2,6]  +
               aMatrix1[7,1] * aMatrix2[2,7]  +
               aMatrix1[8,1] * aMatrix2[2,8]  +
               aMatrix1[9,1] * aMatrix2[2,9]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  +
               aMatrix1[5,2] * aMatrix2[2,5]  +
               aMatrix1[6,2] * aMatrix2[2,6]  +
               aMatrix1[7,2] * aMatrix2[2,7]  +
               aMatrix1[8,2] * aMatrix2[2,8]  +
               aMatrix1[9,2] * aMatrix2[2,9]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  +
               aMatrix1[5,3] * aMatrix2[2,5]  +
               aMatrix1[6,3] * aMatrix2[2,6]  +
               aMatrix1[7,3] * aMatrix2[2,7]  +
               aMatrix1[8,3] * aMatrix2[2,8]  +
               aMatrix1[9,3] * aMatrix2[2,9]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  +
               aMatrix1[5,4] * aMatrix2[2,5]  +
               aMatrix1[6,4] * aMatrix2[2,6]  +
               aMatrix1[7,4] * aMatrix2[2,7]  +
               aMatrix1[8,4] * aMatrix2[2,8]  +
               aMatrix1[9,4] * aMatrix2[2,9]  ;

             Result[2,5]:=
               aMatrix1[1,5] * aMatrix2[2,1]  +
               aMatrix1[2,5] * aMatrix2[2,2]  +
               aMatrix1[3,5] * aMatrix2[2,3]  +
               aMatrix1[4,5] * aMatrix2[2,4]  +
               aMatrix1[5,5] * aMatrix2[2,5]  +
               aMatrix1[6,5] * aMatrix2[2,6]  +
               aMatrix1[7,5] * aMatrix2[2,7]  +
               aMatrix1[8,5] * aMatrix2[2,8]  +
               aMatrix1[9,5] * aMatrix2[2,9]  ;

             Result[2,6]:=
               aMatrix1[1,6] * aMatrix2[2,1]  +
               aMatrix1[2,6] * aMatrix2[2,2]  +
               aMatrix1[3,6] * aMatrix2[2,3]  +
               aMatrix1[4,6] * aMatrix2[2,4]  +
               aMatrix1[5,6] * aMatrix2[2,5]  +
               aMatrix1[6,6] * aMatrix2[2,6]  +
               aMatrix1[7,6] * aMatrix2[2,7]  +
               aMatrix1[8,6] * aMatrix2[2,8]  +
               aMatrix1[9,6] * aMatrix2[2,9]  ;

             Result[2,7]:=
               aMatrix1[1,7] * aMatrix2[2,1]  +
               aMatrix1[2,7] * aMatrix2[2,2]  +
               aMatrix1[3,7] * aMatrix2[2,3]  +
               aMatrix1[4,7] * aMatrix2[2,4]  +
               aMatrix1[5,7] * aMatrix2[2,5]  +
               aMatrix1[6,7] * aMatrix2[2,6]  +
               aMatrix1[7,7] * aMatrix2[2,7]  +
               aMatrix1[8,7] * aMatrix2[2,8]  +
               aMatrix1[9,7] * aMatrix2[2,9]  ;

             Result[2,8]:=
               aMatrix1[1,8] * aMatrix2[2,1]  +
               aMatrix1[2,8] * aMatrix2[2,2]  +
               aMatrix1[3,8] * aMatrix2[2,3]  +
               aMatrix1[4,8] * aMatrix2[2,4]  +
               aMatrix1[5,8] * aMatrix2[2,5]  +
               aMatrix1[6,8] * aMatrix2[2,6]  +
               aMatrix1[7,8] * aMatrix2[2,7]  +
               aMatrix1[8,8] * aMatrix2[2,8]  +
               aMatrix1[9,8] * aMatrix2[2,9]  ;

             Result[2,9]:=
               aMatrix1[1,9] * aMatrix2[2,1]  +
               aMatrix1[2,9] * aMatrix2[2,2]  +
               aMatrix1[3,9] * aMatrix2[2,3]  +
               aMatrix1[4,9] * aMatrix2[2,4]  +
               aMatrix1[5,9] * aMatrix2[2,5]  +
               aMatrix1[6,9] * aMatrix2[2,6]  +
               aMatrix1[7,9] * aMatrix2[2,7]  +
               aMatrix1[8,9] * aMatrix2[2,8]  +
               aMatrix1[9,9] * aMatrix2[2,9]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  +
               aMatrix1[5,1] * aMatrix2[3,5]  +
               aMatrix1[6,1] * aMatrix2[3,6]  +
               aMatrix1[7,1] * aMatrix2[3,7]  +
               aMatrix1[8,1] * aMatrix2[3,8]  +
               aMatrix1[9,1] * aMatrix2[3,9]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  +
               aMatrix1[5,2] * aMatrix2[3,5]  +
               aMatrix1[6,2] * aMatrix2[3,6]  +
               aMatrix1[7,2] * aMatrix2[3,7]  +
               aMatrix1[8,2] * aMatrix2[3,8]  +
               aMatrix1[9,2] * aMatrix2[3,9]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  +
               aMatrix1[5,3] * aMatrix2[3,5]  +
               aMatrix1[6,3] * aMatrix2[3,6]  +
               aMatrix1[7,3] * aMatrix2[3,7]  +
               aMatrix1[8,3] * aMatrix2[3,8]  +
               aMatrix1[9,3] * aMatrix2[3,9]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  +
               aMatrix1[5,4] * aMatrix2[3,5]  +
               aMatrix1[6,4] * aMatrix2[3,6]  +
               aMatrix1[7,4] * aMatrix2[3,7]  +
               aMatrix1[8,4] * aMatrix2[3,8]  +
               aMatrix1[9,4] * aMatrix2[3,9]  ;

             Result[3,5]:=
               aMatrix1[1,5] * aMatrix2[3,1]  +
               aMatrix1[2,5] * aMatrix2[3,2]  +
               aMatrix1[3,5] * aMatrix2[3,3]  +
               aMatrix1[4,5] * aMatrix2[3,4]  +
               aMatrix1[5,5] * aMatrix2[3,5]  +
               aMatrix1[6,5] * aMatrix2[3,6]  +
               aMatrix1[7,5] * aMatrix2[3,7]  +
               aMatrix1[8,5] * aMatrix2[3,8]  +
               aMatrix1[9,5] * aMatrix2[3,9]  ;

             Result[3,6]:=
               aMatrix1[1,6] * aMatrix2[3,1]  +
               aMatrix1[2,6] * aMatrix2[3,2]  +
               aMatrix1[3,6] * aMatrix2[3,3]  +
               aMatrix1[4,6] * aMatrix2[3,4]  +
               aMatrix1[5,6] * aMatrix2[3,5]  +
               aMatrix1[6,6] * aMatrix2[3,6]  +
               aMatrix1[7,6] * aMatrix2[3,7]  +
               aMatrix1[8,6] * aMatrix2[3,8]  +
               aMatrix1[9,6] * aMatrix2[3,9]  ;

             Result[3,7]:=
               aMatrix1[1,7] * aMatrix2[3,1]  +
               aMatrix1[2,7] * aMatrix2[3,2]  +
               aMatrix1[3,7] * aMatrix2[3,3]  +
               aMatrix1[4,7] * aMatrix2[3,4]  +
               aMatrix1[5,7] * aMatrix2[3,5]  +
               aMatrix1[6,7] * aMatrix2[3,6]  +
               aMatrix1[7,7] * aMatrix2[3,7]  +
               aMatrix1[8,7] * aMatrix2[3,8]  +
               aMatrix1[9,7] * aMatrix2[3,9]  ;

             Result[3,8]:=
               aMatrix1[1,8] * aMatrix2[3,1]  +
               aMatrix1[2,8] * aMatrix2[3,2]  +
               aMatrix1[3,8] * aMatrix2[3,3]  +
               aMatrix1[4,8] * aMatrix2[3,4]  +
               aMatrix1[5,8] * aMatrix2[3,5]  +
               aMatrix1[6,8] * aMatrix2[3,6]  +
               aMatrix1[7,8] * aMatrix2[3,7]  +
               aMatrix1[8,8] * aMatrix2[3,8]  +
               aMatrix1[9,8] * aMatrix2[3,9]  ;

             Result[3,9]:=
               aMatrix1[1,9] * aMatrix2[3,1]  +
               aMatrix1[2,9] * aMatrix2[3,2]  +
               aMatrix1[3,9] * aMatrix2[3,3]  +
               aMatrix1[4,9] * aMatrix2[3,4]  +
               aMatrix1[5,9] * aMatrix2[3,5]  +
               aMatrix1[6,9] * aMatrix2[3,6]  +
               aMatrix1[7,9] * aMatrix2[3,7]  +
               aMatrix1[8,9] * aMatrix2[3,8]  +
               aMatrix1[9,9] * aMatrix2[3,9]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  +
               aMatrix1[5,1] * aMatrix2[4,5]  +
               aMatrix1[6,1] * aMatrix2[4,6]  +
               aMatrix1[7,1] * aMatrix2[4,7]  +
               aMatrix1[8,1] * aMatrix2[4,8]  +
               aMatrix1[9,1] * aMatrix2[4,9]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  +
               aMatrix1[5,2] * aMatrix2[4,5]  +
               aMatrix1[6,2] * aMatrix2[4,6]  +
               aMatrix1[7,2] * aMatrix2[4,7]  +
               aMatrix1[8,2] * aMatrix2[4,8]  +
               aMatrix1[9,2] * aMatrix2[4,9]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  +
               aMatrix1[5,3] * aMatrix2[4,5]  +
               aMatrix1[6,3] * aMatrix2[4,6]  +
               aMatrix1[7,3] * aMatrix2[4,7]  +
               aMatrix1[8,3] * aMatrix2[4,8]  +
               aMatrix1[9,3] * aMatrix2[4,9]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  +
               aMatrix1[5,4] * aMatrix2[4,5]  +
               aMatrix1[6,4] * aMatrix2[4,6]  +
               aMatrix1[7,4] * aMatrix2[4,7]  +
               aMatrix1[8,4] * aMatrix2[4,8]  +
               aMatrix1[9,4] * aMatrix2[4,9]  ;

             Result[4,5]:=
               aMatrix1[1,5] * aMatrix2[4,1]  +
               aMatrix1[2,5] * aMatrix2[4,2]  +
               aMatrix1[3,5] * aMatrix2[4,3]  +
               aMatrix1[4,5] * aMatrix2[4,4]  +
               aMatrix1[5,5] * aMatrix2[4,5]  +
               aMatrix1[6,5] * aMatrix2[4,6]  +
               aMatrix1[7,5] * aMatrix2[4,7]  +
               aMatrix1[8,5] * aMatrix2[4,8]  +
               aMatrix1[9,5] * aMatrix2[4,9]  ;

             Result[4,6]:=
               aMatrix1[1,6] * aMatrix2[4,1]  +
               aMatrix1[2,6] * aMatrix2[4,2]  +
               aMatrix1[3,6] * aMatrix2[4,3]  +
               aMatrix1[4,6] * aMatrix2[4,4]  +
               aMatrix1[5,6] * aMatrix2[4,5]  +
               aMatrix1[6,6] * aMatrix2[4,6]  +
               aMatrix1[7,6] * aMatrix2[4,7]  +
               aMatrix1[8,6] * aMatrix2[4,8]  +
               aMatrix1[9,6] * aMatrix2[4,9]  ;

             Result[4,7]:=
               aMatrix1[1,7] * aMatrix2[4,1]  +
               aMatrix1[2,7] * aMatrix2[4,2]  +
               aMatrix1[3,7] * aMatrix2[4,3]  +
               aMatrix1[4,7] * aMatrix2[4,4]  +
               aMatrix1[5,7] * aMatrix2[4,5]  +
               aMatrix1[6,7] * aMatrix2[4,6]  +
               aMatrix1[7,7] * aMatrix2[4,7]  +
               aMatrix1[8,7] * aMatrix2[4,8]  +
               aMatrix1[9,7] * aMatrix2[4,9]  ;

             Result[4,8]:=
               aMatrix1[1,8] * aMatrix2[4,1]  +
               aMatrix1[2,8] * aMatrix2[4,2]  +
               aMatrix1[3,8] * aMatrix2[4,3]  +
               aMatrix1[4,8] * aMatrix2[4,4]  +
               aMatrix1[5,8] * aMatrix2[4,5]  +
               aMatrix1[6,8] * aMatrix2[4,6]  +
               aMatrix1[7,8] * aMatrix2[4,7]  +
               aMatrix1[8,8] * aMatrix2[4,8]  +
               aMatrix1[9,8] * aMatrix2[4,9]  ;

             Result[4,9]:=
               aMatrix1[1,9] * aMatrix2[4,1]  +
               aMatrix1[2,9] * aMatrix2[4,2]  +
               aMatrix1[3,9] * aMatrix2[4,3]  +
               aMatrix1[4,9] * aMatrix2[4,4]  +
               aMatrix1[5,9] * aMatrix2[4,5]  +
               aMatrix1[6,9] * aMatrix2[4,6]  +
               aMatrix1[7,9] * aMatrix2[4,7]  +
               aMatrix1[8,9] * aMatrix2[4,8]  +
               aMatrix1[9,9] * aMatrix2[4,9]  ;


             Result[5,1]:=
               aMatrix1[1,1] * aMatrix2[5,1]  +
               aMatrix1[2,1] * aMatrix2[5,2]  +
               aMatrix1[3,1] * aMatrix2[5,3]  +
               aMatrix1[4,1] * aMatrix2[5,4]  +
               aMatrix1[5,1] * aMatrix2[5,5]  +
               aMatrix1[6,1] * aMatrix2[5,6]  +
               aMatrix1[7,1] * aMatrix2[5,7]  +
               aMatrix1[8,1] * aMatrix2[5,8]  +
               aMatrix1[9,1] * aMatrix2[5,9]  ;

             Result[5,2]:=
               aMatrix1[1,2] * aMatrix2[5,1]  +
               aMatrix1[2,2] * aMatrix2[5,2]  +
               aMatrix1[3,2] * aMatrix2[5,3]  +
               aMatrix1[4,2] * aMatrix2[5,4]  +
               aMatrix1[5,2] * aMatrix2[5,5]  +
               aMatrix1[6,2] * aMatrix2[5,6]  +
               aMatrix1[7,2] * aMatrix2[5,7]  +
               aMatrix1[8,2] * aMatrix2[5,8]  +
               aMatrix1[9,2] * aMatrix2[5,9]  ;

             Result[5,3]:=
               aMatrix1[1,3] * aMatrix2[5,1]  +
               aMatrix1[2,3] * aMatrix2[5,2]  +
               aMatrix1[3,3] * aMatrix2[5,3]  +
               aMatrix1[4,3] * aMatrix2[5,4]  +
               aMatrix1[5,3] * aMatrix2[5,5]  +
               aMatrix1[6,3] * aMatrix2[5,6]  +
               aMatrix1[7,3] * aMatrix2[5,7]  +
               aMatrix1[8,3] * aMatrix2[5,8]  +
               aMatrix1[9,3] * aMatrix2[5,9]  ;

             Result[5,4]:=
               aMatrix1[1,4] * aMatrix2[5,1]  +
               aMatrix1[2,4] * aMatrix2[5,2]  +
               aMatrix1[3,4] * aMatrix2[5,3]  +
               aMatrix1[4,4] * aMatrix2[5,4]  +
               aMatrix1[5,4] * aMatrix2[5,5]  +
               aMatrix1[6,4] * aMatrix2[5,6]  +
               aMatrix1[7,4] * aMatrix2[5,7]  +
               aMatrix1[8,4] * aMatrix2[5,8]  +
               aMatrix1[9,4] * aMatrix2[5,9]  ;

             Result[5,5]:=
               aMatrix1[1,5] * aMatrix2[5,1]  +
               aMatrix1[2,5] * aMatrix2[5,2]  +
               aMatrix1[3,5] * aMatrix2[5,3]  +
               aMatrix1[4,5] * aMatrix2[5,4]  +
               aMatrix1[5,5] * aMatrix2[5,5]  +
               aMatrix1[6,5] * aMatrix2[5,6]  +
               aMatrix1[7,5] * aMatrix2[5,7]  +
               aMatrix1[8,5] * aMatrix2[5,8]  +
               aMatrix1[9,5] * aMatrix2[5,9]  ;

             Result[5,6]:=
               aMatrix1[1,6] * aMatrix2[5,1]  +
               aMatrix1[2,6] * aMatrix2[5,2]  +
               aMatrix1[3,6] * aMatrix2[5,3]  +
               aMatrix1[4,6] * aMatrix2[5,4]  +
               aMatrix1[5,6] * aMatrix2[5,5]  +
               aMatrix1[6,6] * aMatrix2[5,6]  +
               aMatrix1[7,6] * aMatrix2[5,7]  +
               aMatrix1[8,6] * aMatrix2[5,8]  +
               aMatrix1[9,6] * aMatrix2[5,9]  ;

             Result[5,7]:=
               aMatrix1[1,7] * aMatrix2[5,1]  +
               aMatrix1[2,7] * aMatrix2[5,2]  +
               aMatrix1[3,7] * aMatrix2[5,3]  +
               aMatrix1[4,7] * aMatrix2[5,4]  +
               aMatrix1[5,7] * aMatrix2[5,5]  +
               aMatrix1[6,7] * aMatrix2[5,6]  +
               aMatrix1[7,7] * aMatrix2[5,7]  +
               aMatrix1[8,7] * aMatrix2[5,8]  +
               aMatrix1[9,7] * aMatrix2[5,9]  ;

             Result[5,8]:=
               aMatrix1[1,8] * aMatrix2[5,1]  +
               aMatrix1[2,8] * aMatrix2[5,2]  +
               aMatrix1[3,8] * aMatrix2[5,3]  +
               aMatrix1[4,8] * aMatrix2[5,4]  +
               aMatrix1[5,8] * aMatrix2[5,5]  +
               aMatrix1[6,8] * aMatrix2[5,6]  +
               aMatrix1[7,8] * aMatrix2[5,7]  +
               aMatrix1[8,8] * aMatrix2[5,8]  +
               aMatrix1[9,8] * aMatrix2[5,9]  ;

             Result[5,9]:=
               aMatrix1[1,9] * aMatrix2[5,1]  +
               aMatrix1[2,9] * aMatrix2[5,2]  +
               aMatrix1[3,9] * aMatrix2[5,3]  +
               aMatrix1[4,9] * aMatrix2[5,4]  +
               aMatrix1[5,9] * aMatrix2[5,5]  +
               aMatrix1[6,9] * aMatrix2[5,6]  +
               aMatrix1[7,9] * aMatrix2[5,7]  +
               aMatrix1[8,9] * aMatrix2[5,8]  +
               aMatrix1[9,9] * aMatrix2[5,9]  ;


             Result[6,1]:=
               aMatrix1[1,1] * aMatrix2[6,1]  +
               aMatrix1[2,1] * aMatrix2[6,2]  +
               aMatrix1[3,1] * aMatrix2[6,3]  +
               aMatrix1[4,1] * aMatrix2[6,4]  +
               aMatrix1[5,1] * aMatrix2[6,5]  +
               aMatrix1[6,1] * aMatrix2[6,6]  +
               aMatrix1[7,1] * aMatrix2[6,7]  +
               aMatrix1[8,1] * aMatrix2[6,8]  +
               aMatrix1[9,1] * aMatrix2[6,9]  ;

             Result[6,2]:=
               aMatrix1[1,2] * aMatrix2[6,1]  +
               aMatrix1[2,2] * aMatrix2[6,2]  +
               aMatrix1[3,2] * aMatrix2[6,3]  +
               aMatrix1[4,2] * aMatrix2[6,4]  +
               aMatrix1[5,2] * aMatrix2[6,5]  +
               aMatrix1[6,2] * aMatrix2[6,6]  +
               aMatrix1[7,2] * aMatrix2[6,7]  +
               aMatrix1[8,2] * aMatrix2[6,8]  +
               aMatrix1[9,2] * aMatrix2[6,9]  ;

             Result[6,3]:=
               aMatrix1[1,3] * aMatrix2[6,1]  +
               aMatrix1[2,3] * aMatrix2[6,2]  +
               aMatrix1[3,3] * aMatrix2[6,3]  +
               aMatrix1[4,3] * aMatrix2[6,4]  +
               aMatrix1[5,3] * aMatrix2[6,5]  +
               aMatrix1[6,3] * aMatrix2[6,6]  +
               aMatrix1[7,3] * aMatrix2[6,7]  +
               aMatrix1[8,3] * aMatrix2[6,8]  +
               aMatrix1[9,3] * aMatrix2[6,9]  ;

             Result[6,4]:=
               aMatrix1[1,4] * aMatrix2[6,1]  +
               aMatrix1[2,4] * aMatrix2[6,2]  +
               aMatrix1[3,4] * aMatrix2[6,3]  +
               aMatrix1[4,4] * aMatrix2[6,4]  +
               aMatrix1[5,4] * aMatrix2[6,5]  +
               aMatrix1[6,4] * aMatrix2[6,6]  +
               aMatrix1[7,4] * aMatrix2[6,7]  +
               aMatrix1[8,4] * aMatrix2[6,8]  +
               aMatrix1[9,4] * aMatrix2[6,9]  ;

             Result[6,5]:=
               aMatrix1[1,5] * aMatrix2[6,1]  +
               aMatrix1[2,5] * aMatrix2[6,2]  +
               aMatrix1[3,5] * aMatrix2[6,3]  +
               aMatrix1[4,5] * aMatrix2[6,4]  +
               aMatrix1[5,5] * aMatrix2[6,5]  +
               aMatrix1[6,5] * aMatrix2[6,6]  +
               aMatrix1[7,5] * aMatrix2[6,7]  +
               aMatrix1[8,5] * aMatrix2[6,8]  +
               aMatrix1[9,5] * aMatrix2[6,9]  ;

             Result[6,6]:=
               aMatrix1[1,6] * aMatrix2[6,1]  +
               aMatrix1[2,6] * aMatrix2[6,2]  +
               aMatrix1[3,6] * aMatrix2[6,3]  +
               aMatrix1[4,6] * aMatrix2[6,4]  +
               aMatrix1[5,6] * aMatrix2[6,5]  +
               aMatrix1[6,6] * aMatrix2[6,6]  +
               aMatrix1[7,6] * aMatrix2[6,7]  +
               aMatrix1[8,6] * aMatrix2[6,8]  +
               aMatrix1[9,6] * aMatrix2[6,9]  ;

             Result[6,7]:=
               aMatrix1[1,7] * aMatrix2[6,1]  +
               aMatrix1[2,7] * aMatrix2[6,2]  +
               aMatrix1[3,7] * aMatrix2[6,3]  +
               aMatrix1[4,7] * aMatrix2[6,4]  +
               aMatrix1[5,7] * aMatrix2[6,5]  +
               aMatrix1[6,7] * aMatrix2[6,6]  +
               aMatrix1[7,7] * aMatrix2[6,7]  +
               aMatrix1[8,7] * aMatrix2[6,8]  +
               aMatrix1[9,7] * aMatrix2[6,9]  ;

             Result[6,8]:=
               aMatrix1[1,8] * aMatrix2[6,1]  +
               aMatrix1[2,8] * aMatrix2[6,2]  +
               aMatrix1[3,8] * aMatrix2[6,3]  +
               aMatrix1[4,8] * aMatrix2[6,4]  +
               aMatrix1[5,8] * aMatrix2[6,5]  +
               aMatrix1[6,8] * aMatrix2[6,6]  +
               aMatrix1[7,8] * aMatrix2[6,7]  +
               aMatrix1[8,8] * aMatrix2[6,8]  +
               aMatrix1[9,8] * aMatrix2[6,9]  ;

             Result[6,9]:=
               aMatrix1[1,9] * aMatrix2[6,1]  +
               aMatrix1[2,9] * aMatrix2[6,2]  +
               aMatrix1[3,9] * aMatrix2[6,3]  +
               aMatrix1[4,9] * aMatrix2[6,4]  +
               aMatrix1[5,9] * aMatrix2[6,5]  +
               aMatrix1[6,9] * aMatrix2[6,6]  +
               aMatrix1[7,9] * aMatrix2[6,7]  +
               aMatrix1[8,9] * aMatrix2[6,8]  +
               aMatrix1[9,9] * aMatrix2[6,9]  ;


             Result[7,1]:=
               aMatrix1[1,1] * aMatrix2[7,1]  +
               aMatrix1[2,1] * aMatrix2[7,2]  +
               aMatrix1[3,1] * aMatrix2[7,3]  +
               aMatrix1[4,1] * aMatrix2[7,4]  +
               aMatrix1[5,1] * aMatrix2[7,5]  +
               aMatrix1[6,1] * aMatrix2[7,6]  +
               aMatrix1[7,1] * aMatrix2[7,7]  +
               aMatrix1[8,1] * aMatrix2[7,8]  +
               aMatrix1[9,1] * aMatrix2[7,9]  ;

             Result[7,2]:=
               aMatrix1[1,2] * aMatrix2[7,1]  +
               aMatrix1[2,2] * aMatrix2[7,2]  +
               aMatrix1[3,2] * aMatrix2[7,3]  +
               aMatrix1[4,2] * aMatrix2[7,4]  +
               aMatrix1[5,2] * aMatrix2[7,5]  +
               aMatrix1[6,2] * aMatrix2[7,6]  +
               aMatrix1[7,2] * aMatrix2[7,7]  +
               aMatrix1[8,2] * aMatrix2[7,8]  +
               aMatrix1[9,2] * aMatrix2[7,9]  ;

             Result[7,3]:=
               aMatrix1[1,3] * aMatrix2[7,1]  +
               aMatrix1[2,3] * aMatrix2[7,2]  +
               aMatrix1[3,3] * aMatrix2[7,3]  +
               aMatrix1[4,3] * aMatrix2[7,4]  +
               aMatrix1[5,3] * aMatrix2[7,5]  +
               aMatrix1[6,3] * aMatrix2[7,6]  +
               aMatrix1[7,3] * aMatrix2[7,7]  +
               aMatrix1[8,3] * aMatrix2[7,8]  +
               aMatrix1[9,3] * aMatrix2[7,9]  ;

             Result[7,4]:=
               aMatrix1[1,4] * aMatrix2[7,1]  +
               aMatrix1[2,4] * aMatrix2[7,2]  +
               aMatrix1[3,4] * aMatrix2[7,3]  +
               aMatrix1[4,4] * aMatrix2[7,4]  +
               aMatrix1[5,4] * aMatrix2[7,5]  +
               aMatrix1[6,4] * aMatrix2[7,6]  +
               aMatrix1[7,4] * aMatrix2[7,7]  +
               aMatrix1[8,4] * aMatrix2[7,8]  +
               aMatrix1[9,4] * aMatrix2[7,9]  ;

             Result[7,5]:=
               aMatrix1[1,5] * aMatrix2[7,1]  +
               aMatrix1[2,5] * aMatrix2[7,2]  +
               aMatrix1[3,5] * aMatrix2[7,3]  +
               aMatrix1[4,5] * aMatrix2[7,4]  +
               aMatrix1[5,5] * aMatrix2[7,5]  +
               aMatrix1[6,5] * aMatrix2[7,6]  +
               aMatrix1[7,5] * aMatrix2[7,7]  +
               aMatrix1[8,5] * aMatrix2[7,8]  +
               aMatrix1[9,5] * aMatrix2[7,9]  ;

             Result[7,6]:=
               aMatrix1[1,6] * aMatrix2[7,1]  +
               aMatrix1[2,6] * aMatrix2[7,2]  +
               aMatrix1[3,6] * aMatrix2[7,3]  +
               aMatrix1[4,6] * aMatrix2[7,4]  +
               aMatrix1[5,6] * aMatrix2[7,5]  +
               aMatrix1[6,6] * aMatrix2[7,6]  +
               aMatrix1[7,6] * aMatrix2[7,7]  +
               aMatrix1[8,6] * aMatrix2[7,8]  +
               aMatrix1[9,6] * aMatrix2[7,9]  ;

             Result[7,7]:=
               aMatrix1[1,7] * aMatrix2[7,1]  +
               aMatrix1[2,7] * aMatrix2[7,2]  +
               aMatrix1[3,7] * aMatrix2[7,3]  +
               aMatrix1[4,7] * aMatrix2[7,4]  +
               aMatrix1[5,7] * aMatrix2[7,5]  +
               aMatrix1[6,7] * aMatrix2[7,6]  +
               aMatrix1[7,7] * aMatrix2[7,7]  +
               aMatrix1[8,7] * aMatrix2[7,8]  +
               aMatrix1[9,7] * aMatrix2[7,9]  ;

             Result[7,8]:=
               aMatrix1[1,8] * aMatrix2[7,1]  +
               aMatrix1[2,8] * aMatrix2[7,2]  +
               aMatrix1[3,8] * aMatrix2[7,3]  +
               aMatrix1[4,8] * aMatrix2[7,4]  +
               aMatrix1[5,8] * aMatrix2[7,5]  +
               aMatrix1[6,8] * aMatrix2[7,6]  +
               aMatrix1[7,8] * aMatrix2[7,7]  +
               aMatrix1[8,8] * aMatrix2[7,8]  +
               aMatrix1[9,8] * aMatrix2[7,9]  ;

             Result[7,9]:=
               aMatrix1[1,9] * aMatrix2[7,1]  +
               aMatrix1[2,9] * aMatrix2[7,2]  +
               aMatrix1[3,9] * aMatrix2[7,3]  +
               aMatrix1[4,9] * aMatrix2[7,4]  +
               aMatrix1[5,9] * aMatrix2[7,5]  +
               aMatrix1[6,9] * aMatrix2[7,6]  +
               aMatrix1[7,9] * aMatrix2[7,7]  +
               aMatrix1[8,9] * aMatrix2[7,8]  +
               aMatrix1[9,9] * aMatrix2[7,9]  ;


             Result[8,1]:=
               aMatrix1[1,1] * aMatrix2[8,1]  +
               aMatrix1[2,1] * aMatrix2[8,2]  +
               aMatrix1[3,1] * aMatrix2[8,3]  +
               aMatrix1[4,1] * aMatrix2[8,4]  +
               aMatrix1[5,1] * aMatrix2[8,5]  +
               aMatrix1[6,1] * aMatrix2[8,6]  +
               aMatrix1[7,1] * aMatrix2[8,7]  +
               aMatrix1[8,1] * aMatrix2[8,8]  +
               aMatrix1[9,1] * aMatrix2[8,9]  ;

             Result[8,2]:=
               aMatrix1[1,2] * aMatrix2[8,1]  +
               aMatrix1[2,2] * aMatrix2[8,2]  +
               aMatrix1[3,2] * aMatrix2[8,3]  +
               aMatrix1[4,2] * aMatrix2[8,4]  +
               aMatrix1[5,2] * aMatrix2[8,5]  +
               aMatrix1[6,2] * aMatrix2[8,6]  +
               aMatrix1[7,2] * aMatrix2[8,7]  +
               aMatrix1[8,2] * aMatrix2[8,8]  +
               aMatrix1[9,2] * aMatrix2[8,9]  ;

             Result[8,3]:=
               aMatrix1[1,3] * aMatrix2[8,1]  +
               aMatrix1[2,3] * aMatrix2[8,2]  +
               aMatrix1[3,3] * aMatrix2[8,3]  +
               aMatrix1[4,3] * aMatrix2[8,4]  +
               aMatrix1[5,3] * aMatrix2[8,5]  +
               aMatrix1[6,3] * aMatrix2[8,6]  +
               aMatrix1[7,3] * aMatrix2[8,7]  +
               aMatrix1[8,3] * aMatrix2[8,8]  +
               aMatrix1[9,3] * aMatrix2[8,9]  ;

             Result[8,4]:=
               aMatrix1[1,4] * aMatrix2[8,1]  +
               aMatrix1[2,4] * aMatrix2[8,2]  +
               aMatrix1[3,4] * aMatrix2[8,3]  +
               aMatrix1[4,4] * aMatrix2[8,4]  +
               aMatrix1[5,4] * aMatrix2[8,5]  +
               aMatrix1[6,4] * aMatrix2[8,6]  +
               aMatrix1[7,4] * aMatrix2[8,7]  +
               aMatrix1[8,4] * aMatrix2[8,8]  +
               aMatrix1[9,4] * aMatrix2[8,9]  ;

             Result[8,5]:=
               aMatrix1[1,5] * aMatrix2[8,1]  +
               aMatrix1[2,5] * aMatrix2[8,2]  +
               aMatrix1[3,5] * aMatrix2[8,3]  +
               aMatrix1[4,5] * aMatrix2[8,4]  +
               aMatrix1[5,5] * aMatrix2[8,5]  +
               aMatrix1[6,5] * aMatrix2[8,6]  +
               aMatrix1[7,5] * aMatrix2[8,7]  +
               aMatrix1[8,5] * aMatrix2[8,8]  +
               aMatrix1[9,5] * aMatrix2[8,9]  ;

             Result[8,6]:=
               aMatrix1[1,6] * aMatrix2[8,1]  +
               aMatrix1[2,6] * aMatrix2[8,2]  +
               aMatrix1[3,6] * aMatrix2[8,3]  +
               aMatrix1[4,6] * aMatrix2[8,4]  +
               aMatrix1[5,6] * aMatrix2[8,5]  +
               aMatrix1[6,6] * aMatrix2[8,6]  +
               aMatrix1[7,6] * aMatrix2[8,7]  +
               aMatrix1[8,6] * aMatrix2[8,8]  +
               aMatrix1[9,6] * aMatrix2[8,9]  ;

             Result[8,7]:=
               aMatrix1[1,7] * aMatrix2[8,1]  +
               aMatrix1[2,7] * aMatrix2[8,2]  +
               aMatrix1[3,7] * aMatrix2[8,3]  +
               aMatrix1[4,7] * aMatrix2[8,4]  +
               aMatrix1[5,7] * aMatrix2[8,5]  +
               aMatrix1[6,7] * aMatrix2[8,6]  +
               aMatrix1[7,7] * aMatrix2[8,7]  +
               aMatrix1[8,7] * aMatrix2[8,8]  +
               aMatrix1[9,7] * aMatrix2[8,9]  ;

             Result[8,8]:=
               aMatrix1[1,8] * aMatrix2[8,1]  +
               aMatrix1[2,8] * aMatrix2[8,2]  +
               aMatrix1[3,8] * aMatrix2[8,3]  +
               aMatrix1[4,8] * aMatrix2[8,4]  +
               aMatrix1[5,8] * aMatrix2[8,5]  +
               aMatrix1[6,8] * aMatrix2[8,6]  +
               aMatrix1[7,8] * aMatrix2[8,7]  +
               aMatrix1[8,8] * aMatrix2[8,8]  +
               aMatrix1[9,8] * aMatrix2[8,9]  ;

             Result[8,9]:=
               aMatrix1[1,9] * aMatrix2[8,1]  +
               aMatrix1[2,9] * aMatrix2[8,2]  +
               aMatrix1[3,9] * aMatrix2[8,3]  +
               aMatrix1[4,9] * aMatrix2[8,4]  +
               aMatrix1[5,9] * aMatrix2[8,5]  +
               aMatrix1[6,9] * aMatrix2[8,6]  +
               aMatrix1[7,9] * aMatrix2[8,7]  +
               aMatrix1[8,9] * aMatrix2[8,8]  +
               aMatrix1[9,9] * aMatrix2[8,9]  ;


             Result[9,1]:=
               aMatrix1[1,1] * aMatrix2[9,1]  +
               aMatrix1[2,1] * aMatrix2[9,2]  +
               aMatrix1[3,1] * aMatrix2[9,3]  +
               aMatrix1[4,1] * aMatrix2[9,4]  +
               aMatrix1[5,1] * aMatrix2[9,5]  +
               aMatrix1[6,1] * aMatrix2[9,6]  +
               aMatrix1[7,1] * aMatrix2[9,7]  +
               aMatrix1[8,1] * aMatrix2[9,8]  +
               aMatrix1[9,1] * aMatrix2[9,9]  ;

             Result[9,2]:=
               aMatrix1[1,2] * aMatrix2[9,1]  +
               aMatrix1[2,2] * aMatrix2[9,2]  +
               aMatrix1[3,2] * aMatrix2[9,3]  +
               aMatrix1[4,2] * aMatrix2[9,4]  +
               aMatrix1[5,2] * aMatrix2[9,5]  +
               aMatrix1[6,2] * aMatrix2[9,6]  +
               aMatrix1[7,2] * aMatrix2[9,7]  +
               aMatrix1[8,2] * aMatrix2[9,8]  +
               aMatrix1[9,2] * aMatrix2[9,9]  ;

             Result[9,3]:=
               aMatrix1[1,3] * aMatrix2[9,1]  +
               aMatrix1[2,3] * aMatrix2[9,2]  +
               aMatrix1[3,3] * aMatrix2[9,3]  +
               aMatrix1[4,3] * aMatrix2[9,4]  +
               aMatrix1[5,3] * aMatrix2[9,5]  +
               aMatrix1[6,3] * aMatrix2[9,6]  +
               aMatrix1[7,3] * aMatrix2[9,7]  +
               aMatrix1[8,3] * aMatrix2[9,8]  +
               aMatrix1[9,3] * aMatrix2[9,9]  ;

             Result[9,4]:=
               aMatrix1[1,4] * aMatrix2[9,1]  +
               aMatrix1[2,4] * aMatrix2[9,2]  +
               aMatrix1[3,4] * aMatrix2[9,3]  +
               aMatrix1[4,4] * aMatrix2[9,4]  +
               aMatrix1[5,4] * aMatrix2[9,5]  +
               aMatrix1[6,4] * aMatrix2[9,6]  +
               aMatrix1[7,4] * aMatrix2[9,7]  +
               aMatrix1[8,4] * aMatrix2[9,8]  +
               aMatrix1[9,4] * aMatrix2[9,9]  ;

             Result[9,5]:=
               aMatrix1[1,5] * aMatrix2[9,1]  +
               aMatrix1[2,5] * aMatrix2[9,2]  +
               aMatrix1[3,5] * aMatrix2[9,3]  +
               aMatrix1[4,5] * aMatrix2[9,4]  +
               aMatrix1[5,5] * aMatrix2[9,5]  +
               aMatrix1[6,5] * aMatrix2[9,6]  +
               aMatrix1[7,5] * aMatrix2[9,7]  +
               aMatrix1[8,5] * aMatrix2[9,8]  +
               aMatrix1[9,5] * aMatrix2[9,9]  ;

             Result[9,6]:=
               aMatrix1[1,6] * aMatrix2[9,1]  +
               aMatrix1[2,6] * aMatrix2[9,2]  +
               aMatrix1[3,6] * aMatrix2[9,3]  +
               aMatrix1[4,6] * aMatrix2[9,4]  +
               aMatrix1[5,6] * aMatrix2[9,5]  +
               aMatrix1[6,6] * aMatrix2[9,6]  +
               aMatrix1[7,6] * aMatrix2[9,7]  +
               aMatrix1[8,6] * aMatrix2[9,8]  +
               aMatrix1[9,6] * aMatrix2[9,9]  ;

             Result[9,7]:=
               aMatrix1[1,7] * aMatrix2[9,1]  +
               aMatrix1[2,7] * aMatrix2[9,2]  +
               aMatrix1[3,7] * aMatrix2[9,3]  +
               aMatrix1[4,7] * aMatrix2[9,4]  +
               aMatrix1[5,7] * aMatrix2[9,5]  +
               aMatrix1[6,7] * aMatrix2[9,6]  +
               aMatrix1[7,7] * aMatrix2[9,7]  +
               aMatrix1[8,7] * aMatrix2[9,8]  +
               aMatrix1[9,7] * aMatrix2[9,9]  ;

             Result[9,8]:=
               aMatrix1[1,8] * aMatrix2[9,1]  +
               aMatrix1[2,8] * aMatrix2[9,2]  +
               aMatrix1[3,8] * aMatrix2[9,3]  +
               aMatrix1[4,8] * aMatrix2[9,4]  +
               aMatrix1[5,8] * aMatrix2[9,5]  +
               aMatrix1[6,8] * aMatrix2[9,6]  +
               aMatrix1[7,8] * aMatrix2[9,7]  +
               aMatrix1[8,8] * aMatrix2[9,8]  +
               aMatrix1[9,8] * aMatrix2[9,9]  ;

             Result[9,9]:=
               aMatrix1[1,9] * aMatrix2[9,1]  +
               aMatrix1[2,9] * aMatrix2[9,2]  +
               aMatrix1[3,9] * aMatrix2[9,3]  +
               aMatrix1[4,9] * aMatrix2[9,4]  +
               aMatrix1[5,9] * aMatrix2[9,5]  +
               aMatrix1[6,9] * aMatrix2[9,6]  +
               aMatrix1[7,9] * aMatrix2[9,7]  +
               aMatrix1[8,9] * aMatrix2[9,8]  +
               aMatrix1[9,9] * aMatrix2[9,9]  ;
             end; // 9

          8: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  +
               aMatrix1[5,1] * aMatrix2[1,5]  +
               aMatrix1[6,1] * aMatrix2[1,6]  +
               aMatrix1[7,1] * aMatrix2[1,7]  +
               aMatrix1[8,1] * aMatrix2[1,8]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  +
               aMatrix1[5,2] * aMatrix2[1,5]  +
               aMatrix1[6,2] * aMatrix2[1,6]  +
               aMatrix1[7,2] * aMatrix2[1,7]  +
               aMatrix1[8,2] * aMatrix2[1,8]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  +
               aMatrix1[5,3] * aMatrix2[1,5]  +
               aMatrix1[6,3] * aMatrix2[1,6]  +
               aMatrix1[7,3] * aMatrix2[1,7]  +
               aMatrix1[8,3] * aMatrix2[1,8]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  +
               aMatrix1[5,4] * aMatrix2[1,5]  +
               aMatrix1[6,4] * aMatrix2[1,6]  +
               aMatrix1[7,4] * aMatrix2[1,7]  +
               aMatrix1[8,4] * aMatrix2[1,8]  ;

             Result[1,5]:=
               aMatrix1[1,5] * aMatrix2[1,1]  +
               aMatrix1[2,5] * aMatrix2[1,2]  +
               aMatrix1[3,5] * aMatrix2[1,3]  +
               aMatrix1[4,5] * aMatrix2[1,4]  +
               aMatrix1[5,5] * aMatrix2[1,5]  +
               aMatrix1[6,5] * aMatrix2[1,6]  +
               aMatrix1[7,5] * aMatrix2[1,7]  +
               aMatrix1[8,5] * aMatrix2[1,8]  ;

             Result[1,6]:=
               aMatrix1[1,6] * aMatrix2[1,1]  +
               aMatrix1[2,6] * aMatrix2[1,2]  +
               aMatrix1[3,6] * aMatrix2[1,3]  +
               aMatrix1[4,6] * aMatrix2[1,4]  +
               aMatrix1[5,6] * aMatrix2[1,5]  +
               aMatrix1[6,6] * aMatrix2[1,6]  +
               aMatrix1[7,6] * aMatrix2[1,7]  +
               aMatrix1[8,6] * aMatrix2[1,8]  ;

             Result[1,7]:=
               aMatrix1[1,7] * aMatrix2[1,1]  +
               aMatrix1[2,7] * aMatrix2[1,2]  +
               aMatrix1[3,7] * aMatrix2[1,3]  +
               aMatrix1[4,7] * aMatrix2[1,4]  +
               aMatrix1[5,7] * aMatrix2[1,5]  +
               aMatrix1[6,7] * aMatrix2[1,6]  +
               aMatrix1[7,7] * aMatrix2[1,7]  +
               aMatrix1[8,7] * aMatrix2[1,8]  ;

             Result[1,8]:=
               aMatrix1[1,8] * aMatrix2[1,1]  +
               aMatrix1[2,8] * aMatrix2[1,2]  +
               aMatrix1[3,8] * aMatrix2[1,3]  +
               aMatrix1[4,8] * aMatrix2[1,4]  +
               aMatrix1[5,8] * aMatrix2[1,5]  +
               aMatrix1[6,8] * aMatrix2[1,6]  +
               aMatrix1[7,8] * aMatrix2[1,7]  +
               aMatrix1[8,8] * aMatrix2[1,8]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  +
               aMatrix1[5,1] * aMatrix2[2,5]  +
               aMatrix1[6,1] * aMatrix2[2,6]  +
               aMatrix1[7,1] * aMatrix2[2,7]  +
               aMatrix1[8,1] * aMatrix2[2,8]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  +
               aMatrix1[5,2] * aMatrix2[2,5]  +
               aMatrix1[6,2] * aMatrix2[2,6]  +
               aMatrix1[7,2] * aMatrix2[2,7]  +
               aMatrix1[8,2] * aMatrix2[2,8]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  +
               aMatrix1[5,3] * aMatrix2[2,5]  +
               aMatrix1[6,3] * aMatrix2[2,6]  +
               aMatrix1[7,3] * aMatrix2[2,7]  +
               aMatrix1[8,3] * aMatrix2[2,8]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  +
               aMatrix1[5,4] * aMatrix2[2,5]  +
               aMatrix1[6,4] * aMatrix2[2,6]  +
               aMatrix1[7,4] * aMatrix2[2,7]  +
               aMatrix1[8,4] * aMatrix2[2,8]  ;

             Result[2,5]:=
               aMatrix1[1,5] * aMatrix2[2,1]  +
               aMatrix1[2,5] * aMatrix2[2,2]  +
               aMatrix1[3,5] * aMatrix2[2,3]  +
               aMatrix1[4,5] * aMatrix2[2,4]  +
               aMatrix1[5,5] * aMatrix2[2,5]  +
               aMatrix1[6,5] * aMatrix2[2,6]  +
               aMatrix1[7,5] * aMatrix2[2,7]  +
               aMatrix1[8,5] * aMatrix2[2,8]  ;

             Result[2,6]:=
               aMatrix1[1,6] * aMatrix2[2,1]  +
               aMatrix1[2,6] * aMatrix2[2,2]  +
               aMatrix1[3,6] * aMatrix2[2,3]  +
               aMatrix1[4,6] * aMatrix2[2,4]  +
               aMatrix1[5,6] * aMatrix2[2,5]  +
               aMatrix1[6,6] * aMatrix2[2,6]  +
               aMatrix1[7,6] * aMatrix2[2,7]  +
               aMatrix1[8,6] * aMatrix2[2,8]  ;

             Result[2,7]:=
               aMatrix1[1,7] * aMatrix2[2,1]  +
               aMatrix1[2,7] * aMatrix2[2,2]  +
               aMatrix1[3,7] * aMatrix2[2,3]  +
               aMatrix1[4,7] * aMatrix2[2,4]  +
               aMatrix1[5,7] * aMatrix2[2,5]  +
               aMatrix1[6,7] * aMatrix2[2,6]  +
               aMatrix1[7,7] * aMatrix2[2,7]  +
               aMatrix1[8,7] * aMatrix2[2,8]  ;

             Result[2,8]:=
               aMatrix1[1,8] * aMatrix2[2,1]  +
               aMatrix1[2,8] * aMatrix2[2,2]  +
               aMatrix1[3,8] * aMatrix2[2,3]  +
               aMatrix1[4,8] * aMatrix2[2,4]  +
               aMatrix1[5,8] * aMatrix2[2,5]  +
               aMatrix1[6,8] * aMatrix2[2,6]  +
               aMatrix1[7,8] * aMatrix2[2,7]  +
               aMatrix1[8,8] * aMatrix2[2,8]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  +
               aMatrix1[5,1] * aMatrix2[3,5]  +
               aMatrix1[6,1] * aMatrix2[3,6]  +
               aMatrix1[7,1] * aMatrix2[3,7]  +
               aMatrix1[8,1] * aMatrix2[3,8]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  +
               aMatrix1[5,2] * aMatrix2[3,5]  +
               aMatrix1[6,2] * aMatrix2[3,6]  +
               aMatrix1[7,2] * aMatrix2[3,7]  +
               aMatrix1[8,2] * aMatrix2[3,8]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  +
               aMatrix1[5,3] * aMatrix2[3,5]  +
               aMatrix1[6,3] * aMatrix2[3,6]  +
               aMatrix1[7,3] * aMatrix2[3,7]  +
               aMatrix1[8,3] * aMatrix2[3,8]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  +
               aMatrix1[5,4] * aMatrix2[3,5]  +
               aMatrix1[6,4] * aMatrix2[3,6]  +
               aMatrix1[7,4] * aMatrix2[3,7]  +
               aMatrix1[8,4] * aMatrix2[3,8]  ;

             Result[3,5]:=
               aMatrix1[1,5] * aMatrix2[3,1]  +
               aMatrix1[2,5] * aMatrix2[3,2]  +
               aMatrix1[3,5] * aMatrix2[3,3]  +
               aMatrix1[4,5] * aMatrix2[3,4]  +
               aMatrix1[5,5] * aMatrix2[3,5]  +
               aMatrix1[6,5] * aMatrix2[3,6]  +
               aMatrix1[7,5] * aMatrix2[3,7]  +
               aMatrix1[8,5] * aMatrix2[3,8]  ;

             Result[3,6]:=
               aMatrix1[1,6] * aMatrix2[3,1]  +
               aMatrix1[2,6] * aMatrix2[3,2]  +
               aMatrix1[3,6] * aMatrix2[3,3]  +
               aMatrix1[4,6] * aMatrix2[3,4]  +
               aMatrix1[5,6] * aMatrix2[3,5]  +
               aMatrix1[6,6] * aMatrix2[3,6]  +
               aMatrix1[7,6] * aMatrix2[3,7]  +
               aMatrix1[8,6] * aMatrix2[3,8]  ;

             Result[3,7]:=
               aMatrix1[1,7] * aMatrix2[3,1]  +
               aMatrix1[2,7] * aMatrix2[3,2]  +
               aMatrix1[3,7] * aMatrix2[3,3]  +
               aMatrix1[4,7] * aMatrix2[3,4]  +
               aMatrix1[5,7] * aMatrix2[3,5]  +
               aMatrix1[6,7] * aMatrix2[3,6]  +
               aMatrix1[7,7] * aMatrix2[3,7]  +
               aMatrix1[8,7] * aMatrix2[3,8]  ;

             Result[3,8]:=
               aMatrix1[1,8] * aMatrix2[3,1]  +
               aMatrix1[2,8] * aMatrix2[3,2]  +
               aMatrix1[3,8] * aMatrix2[3,3]  +
               aMatrix1[4,8] * aMatrix2[3,4]  +
               aMatrix1[5,8] * aMatrix2[3,5]  +
               aMatrix1[6,8] * aMatrix2[3,6]  +
               aMatrix1[7,8] * aMatrix2[3,7]  +
               aMatrix1[8,8] * aMatrix2[3,8]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  +
               aMatrix1[5,1] * aMatrix2[4,5]  +
               aMatrix1[6,1] * aMatrix2[4,6]  +
               aMatrix1[7,1] * aMatrix2[4,7]  +
               aMatrix1[8,1] * aMatrix2[4,8]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  +
               aMatrix1[5,2] * aMatrix2[4,5]  +
               aMatrix1[6,2] * aMatrix2[4,6]  +
               aMatrix1[7,2] * aMatrix2[4,7]  +
               aMatrix1[8,2] * aMatrix2[4,8]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  +
               aMatrix1[5,3] * aMatrix2[4,5]  +
               aMatrix1[6,3] * aMatrix2[4,6]  +
               aMatrix1[7,3] * aMatrix2[4,7]  +
               aMatrix1[8,3] * aMatrix2[4,8]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  +
               aMatrix1[5,4] * aMatrix2[4,5]  +
               aMatrix1[6,4] * aMatrix2[4,6]  +
               aMatrix1[7,4] * aMatrix2[4,7]  +
               aMatrix1[8,4] * aMatrix2[4,8]  ;

             Result[4,5]:=
               aMatrix1[1,5] * aMatrix2[4,1]  +
               aMatrix1[2,5] * aMatrix2[4,2]  +
               aMatrix1[3,5] * aMatrix2[4,3]  +
               aMatrix1[4,5] * aMatrix2[4,4]  +
               aMatrix1[5,5] * aMatrix2[4,5]  +
               aMatrix1[6,5] * aMatrix2[4,6]  +
               aMatrix1[7,5] * aMatrix2[4,7]  +
               aMatrix1[8,5] * aMatrix2[4,8]  ;

             Result[4,6]:=
               aMatrix1[1,6] * aMatrix2[4,1]  +
               aMatrix1[2,6] * aMatrix2[4,2]  +
               aMatrix1[3,6] * aMatrix2[4,3]  +
               aMatrix1[4,6] * aMatrix2[4,4]  +
               aMatrix1[5,6] * aMatrix2[4,5]  +
               aMatrix1[6,6] * aMatrix2[4,6]  +
               aMatrix1[7,6] * aMatrix2[4,7]  +
               aMatrix1[8,6] * aMatrix2[4,8]  ;

             Result[4,7]:=
               aMatrix1[1,7] * aMatrix2[4,1]  +
               aMatrix1[2,7] * aMatrix2[4,2]  +
               aMatrix1[3,7] * aMatrix2[4,3]  +
               aMatrix1[4,7] * aMatrix2[4,4]  +
               aMatrix1[5,7] * aMatrix2[4,5]  +
               aMatrix1[6,7] * aMatrix2[4,6]  +
               aMatrix1[7,7] * aMatrix2[4,7]  +
               aMatrix1[8,7] * aMatrix2[4,8]  ;

             Result[4,8]:=
               aMatrix1[1,8] * aMatrix2[4,1]  +
               aMatrix1[2,8] * aMatrix2[4,2]  +
               aMatrix1[3,8] * aMatrix2[4,3]  +
               aMatrix1[4,8] * aMatrix2[4,4]  +
               aMatrix1[5,8] * aMatrix2[4,5]  +
               aMatrix1[6,8] * aMatrix2[4,6]  +
               aMatrix1[7,8] * aMatrix2[4,7]  +
               aMatrix1[8,8] * aMatrix2[4,8]  ;


             Result[5,1]:=
               aMatrix1[1,1] * aMatrix2[5,1]  +
               aMatrix1[2,1] * aMatrix2[5,2]  +
               aMatrix1[3,1] * aMatrix2[5,3]  +
               aMatrix1[4,1] * aMatrix2[5,4]  +
               aMatrix1[5,1] * aMatrix2[5,5]  +
               aMatrix1[6,1] * aMatrix2[5,6]  +
               aMatrix1[7,1] * aMatrix2[5,7]  +
               aMatrix1[8,1] * aMatrix2[5,8]  ;

             Result[5,2]:=
               aMatrix1[1,2] * aMatrix2[5,1]  +
               aMatrix1[2,2] * aMatrix2[5,2]  +
               aMatrix1[3,2] * aMatrix2[5,3]  +
               aMatrix1[4,2] * aMatrix2[5,4]  +
               aMatrix1[5,2] * aMatrix2[5,5]  +
               aMatrix1[6,2] * aMatrix2[5,6]  +
               aMatrix1[7,2] * aMatrix2[5,7]  +
               aMatrix1[8,2] * aMatrix2[5,8]  ;

             Result[5,3]:=
               aMatrix1[1,3] * aMatrix2[5,1]  +
               aMatrix1[2,3] * aMatrix2[5,2]  +
               aMatrix1[3,3] * aMatrix2[5,3]  +
               aMatrix1[4,3] * aMatrix2[5,4]  +
               aMatrix1[5,3] * aMatrix2[5,5]  +
               aMatrix1[6,3] * aMatrix2[5,6]  +
               aMatrix1[7,3] * aMatrix2[5,7]  +
               aMatrix1[8,3] * aMatrix2[5,8]  ;

             Result[5,4]:=
               aMatrix1[1,4] * aMatrix2[5,1]  +
               aMatrix1[2,4] * aMatrix2[5,2]  +
               aMatrix1[3,4] * aMatrix2[5,3]  +
               aMatrix1[4,4] * aMatrix2[5,4]  +
               aMatrix1[5,4] * aMatrix2[5,5]  +
               aMatrix1[6,4] * aMatrix2[5,6]  +
               aMatrix1[7,4] * aMatrix2[5,7]  +
               aMatrix1[8,4] * aMatrix2[5,8]  ;

             Result[5,5]:=
               aMatrix1[1,5] * aMatrix2[5,1]  +
               aMatrix1[2,5] * aMatrix2[5,2]  +
               aMatrix1[3,5] * aMatrix2[5,3]  +
               aMatrix1[4,5] * aMatrix2[5,4]  +
               aMatrix1[5,5] * aMatrix2[5,5]  +
               aMatrix1[6,5] * aMatrix2[5,6]  +
               aMatrix1[7,5] * aMatrix2[5,7]  +
               aMatrix1[8,5] * aMatrix2[5,8]  ;

             Result[5,6]:=
               aMatrix1[1,6] * aMatrix2[5,1]  +
               aMatrix1[2,6] * aMatrix2[5,2]  +
               aMatrix1[3,6] * aMatrix2[5,3]  +
               aMatrix1[4,6] * aMatrix2[5,4]  +
               aMatrix1[5,6] * aMatrix2[5,5]  +
               aMatrix1[6,6] * aMatrix2[5,6]  +
               aMatrix1[7,6] * aMatrix2[5,7]  +
               aMatrix1[8,6] * aMatrix2[5,8]  ;

             Result[5,7]:=
               aMatrix1[1,7] * aMatrix2[5,1]  +
               aMatrix1[2,7] * aMatrix2[5,2]  +
               aMatrix1[3,7] * aMatrix2[5,3]  +
               aMatrix1[4,7] * aMatrix2[5,4]  +
               aMatrix1[5,7] * aMatrix2[5,5]  +
               aMatrix1[6,7] * aMatrix2[5,6]  +
               aMatrix1[7,7] * aMatrix2[5,7]  +
               aMatrix1[8,7] * aMatrix2[5,8]  ;

             Result[5,8]:=
               aMatrix1[1,8] * aMatrix2[5,1]  +
               aMatrix1[2,8] * aMatrix2[5,2]  +
               aMatrix1[3,8] * aMatrix2[5,3]  +
               aMatrix1[4,8] * aMatrix2[5,4]  +
               aMatrix1[5,8] * aMatrix2[5,5]  +
               aMatrix1[6,8] * aMatrix2[5,6]  +
               aMatrix1[7,8] * aMatrix2[5,7]  +
               aMatrix1[8,8] * aMatrix2[5,8]  ;


             Result[6,1]:=
               aMatrix1[1,1] * aMatrix2[6,1]  +
               aMatrix1[2,1] * aMatrix2[6,2]  +
               aMatrix1[3,1] * aMatrix2[6,3]  +
               aMatrix1[4,1] * aMatrix2[6,4]  +
               aMatrix1[5,1] * aMatrix2[6,5]  +
               aMatrix1[6,1] * aMatrix2[6,6]  +
               aMatrix1[7,1] * aMatrix2[6,7]  +
               aMatrix1[8,1] * aMatrix2[6,8]  ;

             Result[6,2]:=
               aMatrix1[1,2] * aMatrix2[6,1]  +
               aMatrix1[2,2] * aMatrix2[6,2]  +
               aMatrix1[3,2] * aMatrix2[6,3]  +
               aMatrix1[4,2] * aMatrix2[6,4]  +
               aMatrix1[5,2] * aMatrix2[6,5]  +
               aMatrix1[6,2] * aMatrix2[6,6]  +
               aMatrix1[7,2] * aMatrix2[6,7]  +
               aMatrix1[8,2] * aMatrix2[6,8]  ;

             Result[6,3]:=
               aMatrix1[1,3] * aMatrix2[6,1]  +
               aMatrix1[2,3] * aMatrix2[6,2]  +
               aMatrix1[3,3] * aMatrix2[6,3]  +
               aMatrix1[4,3] * aMatrix2[6,4]  +
               aMatrix1[5,3] * aMatrix2[6,5]  +
               aMatrix1[6,3] * aMatrix2[6,6]  +
               aMatrix1[7,3] * aMatrix2[6,7]  +
               aMatrix1[8,3] * aMatrix2[6,8]  ;

             Result[6,4]:=
               aMatrix1[1,4] * aMatrix2[6,1]  +
               aMatrix1[2,4] * aMatrix2[6,2]  +
               aMatrix1[3,4] * aMatrix2[6,3]  +
               aMatrix1[4,4] * aMatrix2[6,4]  +
               aMatrix1[5,4] * aMatrix2[6,5]  +
               aMatrix1[6,4] * aMatrix2[6,6]  +
               aMatrix1[7,4] * aMatrix2[6,7]  +
               aMatrix1[8,4] * aMatrix2[6,8]  ;

             Result[6,5]:=
               aMatrix1[1,5] * aMatrix2[6,1]  +
               aMatrix1[2,5] * aMatrix2[6,2]  +
               aMatrix1[3,5] * aMatrix2[6,3]  +
               aMatrix1[4,5] * aMatrix2[6,4]  +
               aMatrix1[5,5] * aMatrix2[6,5]  +
               aMatrix1[6,5] * aMatrix2[6,6]  +
               aMatrix1[7,5] * aMatrix2[6,7]  +
               aMatrix1[8,5] * aMatrix2[6,8]  ;

             Result[6,6]:=
               aMatrix1[1,6] * aMatrix2[6,1]  +
               aMatrix1[2,6] * aMatrix2[6,2]  +
               aMatrix1[3,6] * aMatrix2[6,3]  +
               aMatrix1[4,6] * aMatrix2[6,4]  +
               aMatrix1[5,6] * aMatrix2[6,5]  +
               aMatrix1[6,6] * aMatrix2[6,6]  +
               aMatrix1[7,6] * aMatrix2[6,7]  +
               aMatrix1[8,6] * aMatrix2[6,8]  ;

             Result[6,7]:=
               aMatrix1[1,7] * aMatrix2[6,1]  +
               aMatrix1[2,7] * aMatrix2[6,2]  +
               aMatrix1[3,7] * aMatrix2[6,3]  +
               aMatrix1[4,7] * aMatrix2[6,4]  +
               aMatrix1[5,7] * aMatrix2[6,5]  +
               aMatrix1[6,7] * aMatrix2[6,6]  +
               aMatrix1[7,7] * aMatrix2[6,7]  +
               aMatrix1[8,7] * aMatrix2[6,8]  ;

             Result[6,8]:=
               aMatrix1[1,8] * aMatrix2[6,1]  +
               aMatrix1[2,8] * aMatrix2[6,2]  +
               aMatrix1[3,8] * aMatrix2[6,3]  +
               aMatrix1[4,8] * aMatrix2[6,4]  +
               aMatrix1[5,8] * aMatrix2[6,5]  +
               aMatrix1[6,8] * aMatrix2[6,6]  +
               aMatrix1[7,8] * aMatrix2[6,7]  +
               aMatrix1[8,8] * aMatrix2[6,8]  ;


             Result[7,1]:=
               aMatrix1[1,1] * aMatrix2[7,1]  +
               aMatrix1[2,1] * aMatrix2[7,2]  +
               aMatrix1[3,1] * aMatrix2[7,3]  +
               aMatrix1[4,1] * aMatrix2[7,4]  +
               aMatrix1[5,1] * aMatrix2[7,5]  +
               aMatrix1[6,1] * aMatrix2[7,6]  +
               aMatrix1[7,1] * aMatrix2[7,7]  +
               aMatrix1[8,1] * aMatrix2[7,8]  ;

             Result[7,2]:=
               aMatrix1[1,2] * aMatrix2[7,1]  +
               aMatrix1[2,2] * aMatrix2[7,2]  +
               aMatrix1[3,2] * aMatrix2[7,3]  +
               aMatrix1[4,2] * aMatrix2[7,4]  +
               aMatrix1[5,2] * aMatrix2[7,5]  +
               aMatrix1[6,2] * aMatrix2[7,6]  +
               aMatrix1[7,2] * aMatrix2[7,7]  +
               aMatrix1[8,2] * aMatrix2[7,8]  ;

             Result[7,3]:=
               aMatrix1[1,3] * aMatrix2[7,1]  +
               aMatrix1[2,3] * aMatrix2[7,2]  +
               aMatrix1[3,3] * aMatrix2[7,3]  +
               aMatrix1[4,3] * aMatrix2[7,4]  +
               aMatrix1[5,3] * aMatrix2[7,5]  +
               aMatrix1[6,3] * aMatrix2[7,6]  +
               aMatrix1[7,3] * aMatrix2[7,7]  +
               aMatrix1[8,3] * aMatrix2[7,8]  ;

             Result[7,4]:=
               aMatrix1[1,4] * aMatrix2[7,1]  +
               aMatrix1[2,4] * aMatrix2[7,2]  +
               aMatrix1[3,4] * aMatrix2[7,3]  +
               aMatrix1[4,4] * aMatrix2[7,4]  +
               aMatrix1[5,4] * aMatrix2[7,5]  +
               aMatrix1[6,4] * aMatrix2[7,6]  +
               aMatrix1[7,4] * aMatrix2[7,7]  +
               aMatrix1[8,4] * aMatrix2[7,8]  ;

             Result[7,5]:=
               aMatrix1[1,5] * aMatrix2[7,1]  +
               aMatrix1[2,5] * aMatrix2[7,2]  +
               aMatrix1[3,5] * aMatrix2[7,3]  +
               aMatrix1[4,5] * aMatrix2[7,4]  +
               aMatrix1[5,5] * aMatrix2[7,5]  +
               aMatrix1[6,5] * aMatrix2[7,6]  +
               aMatrix1[7,5] * aMatrix2[7,7]  +
               aMatrix1[8,5] * aMatrix2[7,8]  ;

             Result[7,6]:=
               aMatrix1[1,6] * aMatrix2[7,1]  +
               aMatrix1[2,6] * aMatrix2[7,2]  +
               aMatrix1[3,6] * aMatrix2[7,3]  +
               aMatrix1[4,6] * aMatrix2[7,4]  +
               aMatrix1[5,6] * aMatrix2[7,5]  +
               aMatrix1[6,6] * aMatrix2[7,6]  +
               aMatrix1[7,6] * aMatrix2[7,7]  +
               aMatrix1[8,6] * aMatrix2[7,8]  ;

             Result[7,7]:=
               aMatrix1[1,7] * aMatrix2[7,1]  +
               aMatrix1[2,7] * aMatrix2[7,2]  +
               aMatrix1[3,7] * aMatrix2[7,3]  +
               aMatrix1[4,7] * aMatrix2[7,4]  +
               aMatrix1[5,7] * aMatrix2[7,5]  +
               aMatrix1[6,7] * aMatrix2[7,6]  +
               aMatrix1[7,7] * aMatrix2[7,7]  +
               aMatrix1[8,7] * aMatrix2[7,8]  ;

             Result[7,8]:=
               aMatrix1[1,8] * aMatrix2[7,1]  +
               aMatrix1[2,8] * aMatrix2[7,2]  +
               aMatrix1[3,8] * aMatrix2[7,3]  +
               aMatrix1[4,8] * aMatrix2[7,4]  +
               aMatrix1[5,8] * aMatrix2[7,5]  +
               aMatrix1[6,8] * aMatrix2[7,6]  +
               aMatrix1[7,8] * aMatrix2[7,7]  +
               aMatrix1[8,8] * aMatrix2[7,8]  ;


             Result[8,1]:=
               aMatrix1[1,1] * aMatrix2[8,1]  +
               aMatrix1[2,1] * aMatrix2[8,2]  +
               aMatrix1[3,1] * aMatrix2[8,3]  +
               aMatrix1[4,1] * aMatrix2[8,4]  +
               aMatrix1[5,1] * aMatrix2[8,5]  +
               aMatrix1[6,1] * aMatrix2[8,6]  +
               aMatrix1[7,1] * aMatrix2[8,7]  +
               aMatrix1[8,1] * aMatrix2[8,8]  ;

             Result[8,2]:=
               aMatrix1[1,2] * aMatrix2[8,1]  +
               aMatrix1[2,2] * aMatrix2[8,2]  +
               aMatrix1[3,2] * aMatrix2[8,3]  +
               aMatrix1[4,2] * aMatrix2[8,4]  +
               aMatrix1[5,2] * aMatrix2[8,5]  +
               aMatrix1[6,2] * aMatrix2[8,6]  +
               aMatrix1[7,2] * aMatrix2[8,7]  +
               aMatrix1[8,2] * aMatrix2[8,8]  ;

             Result[8,3]:=
               aMatrix1[1,3] * aMatrix2[8,1]  +
               aMatrix1[2,3] * aMatrix2[8,2]  +
               aMatrix1[3,3] * aMatrix2[8,3]  +
               aMatrix1[4,3] * aMatrix2[8,4]  +
               aMatrix1[5,3] * aMatrix2[8,5]  +
               aMatrix1[6,3] * aMatrix2[8,6]  +
               aMatrix1[7,3] * aMatrix2[8,7]  +
               aMatrix1[8,3] * aMatrix2[8,8]  ;

             Result[8,4]:=
               aMatrix1[1,4] * aMatrix2[8,1]  +
               aMatrix1[2,4] * aMatrix2[8,2]  +
               aMatrix1[3,4] * aMatrix2[8,3]  +
               aMatrix1[4,4] * aMatrix2[8,4]  +
               aMatrix1[5,4] * aMatrix2[8,5]  +
               aMatrix1[6,4] * aMatrix2[8,6]  +
               aMatrix1[7,4] * aMatrix2[8,7]  +
               aMatrix1[8,4] * aMatrix2[8,8]  ;

             Result[8,5]:=
               aMatrix1[1,5] * aMatrix2[8,1]  +
               aMatrix1[2,5] * aMatrix2[8,2]  +
               aMatrix1[3,5] * aMatrix2[8,3]  +
               aMatrix1[4,5] * aMatrix2[8,4]  +
               aMatrix1[5,5] * aMatrix2[8,5]  +
               aMatrix1[6,5] * aMatrix2[8,6]  +
               aMatrix1[7,5] * aMatrix2[8,7]  +
               aMatrix1[8,5] * aMatrix2[8,8]  ;

             Result[8,6]:=
               aMatrix1[1,6] * aMatrix2[8,1]  +
               aMatrix1[2,6] * aMatrix2[8,2]  +
               aMatrix1[3,6] * aMatrix2[8,3]  +
               aMatrix1[4,6] * aMatrix2[8,4]  +
               aMatrix1[5,6] * aMatrix2[8,5]  +
               aMatrix1[6,6] * aMatrix2[8,6]  +
               aMatrix1[7,6] * aMatrix2[8,7]  +
               aMatrix1[8,6] * aMatrix2[8,8]  ;

             Result[8,7]:=
               aMatrix1[1,7] * aMatrix2[8,1]  +
               aMatrix1[2,7] * aMatrix2[8,2]  +
               aMatrix1[3,7] * aMatrix2[8,3]  +
               aMatrix1[4,7] * aMatrix2[8,4]  +
               aMatrix1[5,7] * aMatrix2[8,5]  +
               aMatrix1[6,7] * aMatrix2[8,6]  +
               aMatrix1[7,7] * aMatrix2[8,7]  +
               aMatrix1[8,7] * aMatrix2[8,8]  ;

             Result[8,8]:=
               aMatrix1[1,8] * aMatrix2[8,1]  +
               aMatrix1[2,8] * aMatrix2[8,2]  +
               aMatrix1[3,8] * aMatrix2[8,3]  +
               aMatrix1[4,8] * aMatrix2[8,4]  +
               aMatrix1[5,8] * aMatrix2[8,5]  +
               aMatrix1[6,8] * aMatrix2[8,6]  +
               aMatrix1[7,8] * aMatrix2[8,7]  +
               aMatrix1[8,8] * aMatrix2[8,8]  ;
             end; // 8

          7: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  +
               aMatrix1[5,1] * aMatrix2[1,5]  +
               aMatrix1[6,1] * aMatrix2[1,6]  +
               aMatrix1[7,1] * aMatrix2[1,7]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  +
               aMatrix1[5,2] * aMatrix2[1,5]  +
               aMatrix1[6,2] * aMatrix2[1,6]  +
               aMatrix1[7,2] * aMatrix2[1,7]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  +
               aMatrix1[5,3] * aMatrix2[1,5]  +
               aMatrix1[6,3] * aMatrix2[1,6]  +
               aMatrix1[7,3] * aMatrix2[1,7]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  +
               aMatrix1[5,4] * aMatrix2[1,5]  +
               aMatrix1[6,4] * aMatrix2[1,6]  +
               aMatrix1[7,4] * aMatrix2[1,7]  ;

             Result[1,5]:=
               aMatrix1[1,5] * aMatrix2[1,1]  +
               aMatrix1[2,5] * aMatrix2[1,2]  +
               aMatrix1[3,5] * aMatrix2[1,3]  +
               aMatrix1[4,5] * aMatrix2[1,4]  +
               aMatrix1[5,5] * aMatrix2[1,5]  +
               aMatrix1[6,5] * aMatrix2[1,6]  +
               aMatrix1[7,5] * aMatrix2[1,7]  ;

             Result[1,6]:=
               aMatrix1[1,6] * aMatrix2[1,1]  +
               aMatrix1[2,6] * aMatrix2[1,2]  +
               aMatrix1[3,6] * aMatrix2[1,3]  +
               aMatrix1[4,6] * aMatrix2[1,4]  +
               aMatrix1[5,6] * aMatrix2[1,5]  +
               aMatrix1[6,6] * aMatrix2[1,6]  +
               aMatrix1[7,6] * aMatrix2[1,7]  ;

             Result[1,7]:=
               aMatrix1[1,7] * aMatrix2[1,1]  +
               aMatrix1[2,7] * aMatrix2[1,2]  +
               aMatrix1[3,7] * aMatrix2[1,3]  +
               aMatrix1[4,7] * aMatrix2[1,4]  +
               aMatrix1[5,7] * aMatrix2[1,5]  +
               aMatrix1[6,7] * aMatrix2[1,6]  +
               aMatrix1[7,7] * aMatrix2[1,7]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  +
               aMatrix1[5,1] * aMatrix2[2,5]  +
               aMatrix1[6,1] * aMatrix2[2,6]  +
               aMatrix1[7,1] * aMatrix2[2,7]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  +
               aMatrix1[5,2] * aMatrix2[2,5]  +
               aMatrix1[6,2] * aMatrix2[2,6]  +
               aMatrix1[7,2] * aMatrix2[2,7]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  +
               aMatrix1[5,3] * aMatrix2[2,5]  +
               aMatrix1[6,3] * aMatrix2[2,6]  +
               aMatrix1[7,3] * aMatrix2[2,7]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  +
               aMatrix1[5,4] * aMatrix2[2,5]  +
               aMatrix1[6,4] * aMatrix2[2,6]  +
               aMatrix1[7,4] * aMatrix2[2,7]  ;

             Result[2,5]:=
               aMatrix1[1,5] * aMatrix2[2,1]  +
               aMatrix1[2,5] * aMatrix2[2,2]  +
               aMatrix1[3,5] * aMatrix2[2,3]  +
               aMatrix1[4,5] * aMatrix2[2,4]  +
               aMatrix1[5,5] * aMatrix2[2,5]  +
               aMatrix1[6,5] * aMatrix2[2,6]  +
               aMatrix1[7,5] * aMatrix2[2,7]  ;

             Result[2,6]:=
               aMatrix1[1,6] * aMatrix2[2,1]  +
               aMatrix1[2,6] * aMatrix2[2,2]  +
               aMatrix1[3,6] * aMatrix2[2,3]  +
               aMatrix1[4,6] * aMatrix2[2,4]  +
               aMatrix1[5,6] * aMatrix2[2,5]  +
               aMatrix1[6,6] * aMatrix2[2,6]  +
               aMatrix1[7,6] * aMatrix2[2,7]  ;

             Result[2,7]:=
               aMatrix1[1,7] * aMatrix2[2,1]  +
               aMatrix1[2,7] * aMatrix2[2,2]  +
               aMatrix1[3,7] * aMatrix2[2,3]  +
               aMatrix1[4,7] * aMatrix2[2,4]  +
               aMatrix1[5,7] * aMatrix2[2,5]  +
               aMatrix1[6,7] * aMatrix2[2,6]  +
               aMatrix1[7,7] * aMatrix2[2,7]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  +
               aMatrix1[5,1] * aMatrix2[3,5]  +
               aMatrix1[6,1] * aMatrix2[3,6]  +
               aMatrix1[7,1] * aMatrix2[3,7]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  +
               aMatrix1[5,2] * aMatrix2[3,5]  +
               aMatrix1[6,2] * aMatrix2[3,6]  +
               aMatrix1[7,2] * aMatrix2[3,7]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  +
               aMatrix1[5,3] * aMatrix2[3,5]  +
               aMatrix1[6,3] * aMatrix2[3,6]  +
               aMatrix1[7,3] * aMatrix2[3,7]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  +
               aMatrix1[5,4] * aMatrix2[3,5]  +
               aMatrix1[6,4] * aMatrix2[3,6]  +
               aMatrix1[7,4] * aMatrix2[3,7]  ;

             Result[3,5]:=
               aMatrix1[1,5] * aMatrix2[3,1]  +
               aMatrix1[2,5] * aMatrix2[3,2]  +
               aMatrix1[3,5] * aMatrix2[3,3]  +
               aMatrix1[4,5] * aMatrix2[3,4]  +
               aMatrix1[5,5] * aMatrix2[3,5]  +
               aMatrix1[6,5] * aMatrix2[3,6]  +
               aMatrix1[7,5] * aMatrix2[3,7]  ;

             Result[3,6]:=
               aMatrix1[1,6] * aMatrix2[3,1]  +
               aMatrix1[2,6] * aMatrix2[3,2]  +
               aMatrix1[3,6] * aMatrix2[3,3]  +
               aMatrix1[4,6] * aMatrix2[3,4]  +
               aMatrix1[5,6] * aMatrix2[3,5]  +
               aMatrix1[6,6] * aMatrix2[3,6]  +
               aMatrix1[7,6] * aMatrix2[3,7]  ;

             Result[3,7]:=
               aMatrix1[1,7] * aMatrix2[3,1]  +
               aMatrix1[2,7] * aMatrix2[3,2]  +
               aMatrix1[3,7] * aMatrix2[3,3]  +
               aMatrix1[4,7] * aMatrix2[3,4]  +
               aMatrix1[5,7] * aMatrix2[3,5]  +
               aMatrix1[6,7] * aMatrix2[3,6]  +
               aMatrix1[7,7] * aMatrix2[3,7]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  +
               aMatrix1[5,1] * aMatrix2[4,5]  +
               aMatrix1[6,1] * aMatrix2[4,6]  +
               aMatrix1[7,1] * aMatrix2[4,7]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  +
               aMatrix1[5,2] * aMatrix2[4,5]  +
               aMatrix1[6,2] * aMatrix2[4,6]  +
               aMatrix1[7,2] * aMatrix2[4,7]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  +
               aMatrix1[5,3] * aMatrix2[4,5]  +
               aMatrix1[6,3] * aMatrix2[4,6]  +
               aMatrix1[7,3] * aMatrix2[4,7]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  +
               aMatrix1[5,4] * aMatrix2[4,5]  +
               aMatrix1[6,4] * aMatrix2[4,6]  +
               aMatrix1[7,4] * aMatrix2[4,7]  ;

             Result[4,5]:=
               aMatrix1[1,5] * aMatrix2[4,1]  +
               aMatrix1[2,5] * aMatrix2[4,2]  +
               aMatrix1[3,5] * aMatrix2[4,3]  +
               aMatrix1[4,5] * aMatrix2[4,4]  +
               aMatrix1[5,5] * aMatrix2[4,5]  +
               aMatrix1[6,5] * aMatrix2[4,6]  +
               aMatrix1[7,5] * aMatrix2[4,7]  ;

             Result[4,6]:=
               aMatrix1[1,6] * aMatrix2[4,1]  +
               aMatrix1[2,6] * aMatrix2[4,2]  +
               aMatrix1[3,6] * aMatrix2[4,3]  +
               aMatrix1[4,6] * aMatrix2[4,4]  +
               aMatrix1[5,6] * aMatrix2[4,5]  +
               aMatrix1[6,6] * aMatrix2[4,6]  +
               aMatrix1[7,6] * aMatrix2[4,7]  ;

             Result[4,7]:=
               aMatrix1[1,7] * aMatrix2[4,1]  +
               aMatrix1[2,7] * aMatrix2[4,2]  +
               aMatrix1[3,7] * aMatrix2[4,3]  +
               aMatrix1[4,7] * aMatrix2[4,4]  +
               aMatrix1[5,7] * aMatrix2[4,5]  +
               aMatrix1[6,7] * aMatrix2[4,6]  +
               aMatrix1[7,7] * aMatrix2[4,7]  ;


             Result[5,1]:=
               aMatrix1[1,1] * aMatrix2[5,1]  +
               aMatrix1[2,1] * aMatrix2[5,2]  +
               aMatrix1[3,1] * aMatrix2[5,3]  +
               aMatrix1[4,1] * aMatrix2[5,4]  +
               aMatrix1[5,1] * aMatrix2[5,5]  +
               aMatrix1[6,1] * aMatrix2[5,6]  +
               aMatrix1[7,1] * aMatrix2[5,7]  ;

             Result[5,2]:=
               aMatrix1[1,2] * aMatrix2[5,1]  +
               aMatrix1[2,2] * aMatrix2[5,2]  +
               aMatrix1[3,2] * aMatrix2[5,3]  +
               aMatrix1[4,2] * aMatrix2[5,4]  +
               aMatrix1[5,2] * aMatrix2[5,5]  +
               aMatrix1[6,2] * aMatrix2[5,6]  +
               aMatrix1[7,2] * aMatrix2[5,7]  ;

             Result[5,3]:=
               aMatrix1[1,3] * aMatrix2[5,1]  +
               aMatrix1[2,3] * aMatrix2[5,2]  +
               aMatrix1[3,3] * aMatrix2[5,3]  +
               aMatrix1[4,3] * aMatrix2[5,4]  +
               aMatrix1[5,3] * aMatrix2[5,5]  +
               aMatrix1[6,3] * aMatrix2[5,6]  +
               aMatrix1[7,3] * aMatrix2[5,7]  ;

             Result[5,4]:=
               aMatrix1[1,4] * aMatrix2[5,1]  +
               aMatrix1[2,4] * aMatrix2[5,2]  +
               aMatrix1[3,4] * aMatrix2[5,3]  +
               aMatrix1[4,4] * aMatrix2[5,4]  +
               aMatrix1[5,4] * aMatrix2[5,5]  +
               aMatrix1[6,4] * aMatrix2[5,6]  +
               aMatrix1[7,4] * aMatrix2[5,7]  ;

             Result[5,5]:=
               aMatrix1[1,5] * aMatrix2[5,1]  +
               aMatrix1[2,5] * aMatrix2[5,2]  +
               aMatrix1[3,5] * aMatrix2[5,3]  +
               aMatrix1[4,5] * aMatrix2[5,4]  +
               aMatrix1[5,5] * aMatrix2[5,5]  +
               aMatrix1[6,5] * aMatrix2[5,6]  +
               aMatrix1[7,5] * aMatrix2[5,7]  ;

             Result[5,6]:=
               aMatrix1[1,6] * aMatrix2[5,1]  +
               aMatrix1[2,6] * aMatrix2[5,2]  +
               aMatrix1[3,6] * aMatrix2[5,3]  +
               aMatrix1[4,6] * aMatrix2[5,4]  +
               aMatrix1[5,6] * aMatrix2[5,5]  +
               aMatrix1[6,6] * aMatrix2[5,6]  +
               aMatrix1[7,6] * aMatrix2[5,7]  ;

             Result[5,7]:=
               aMatrix1[1,7] * aMatrix2[5,1]  +
               aMatrix1[2,7] * aMatrix2[5,2]  +
               aMatrix1[3,7] * aMatrix2[5,3]  +
               aMatrix1[4,7] * aMatrix2[5,4]  +
               aMatrix1[5,7] * aMatrix2[5,5]  +
               aMatrix1[6,7] * aMatrix2[5,6]  +
               aMatrix1[7,7] * aMatrix2[5,7]  ;


             Result[6,1]:=
               aMatrix1[1,1] * aMatrix2[6,1]  +
               aMatrix1[2,1] * aMatrix2[6,2]  +
               aMatrix1[3,1] * aMatrix2[6,3]  +
               aMatrix1[4,1] * aMatrix2[6,4]  +
               aMatrix1[5,1] * aMatrix2[6,5]  +
               aMatrix1[6,1] * aMatrix2[6,6]  +
               aMatrix1[7,1] * aMatrix2[6,7]  ;

             Result[6,2]:=
               aMatrix1[1,2] * aMatrix2[6,1]  +
               aMatrix1[2,2] * aMatrix2[6,2]  +
               aMatrix1[3,2] * aMatrix2[6,3]  +
               aMatrix1[4,2] * aMatrix2[6,4]  +
               aMatrix1[5,2] * aMatrix2[6,5]  +
               aMatrix1[6,2] * aMatrix2[6,6]  +
               aMatrix1[7,2] * aMatrix2[6,7]  ;

             Result[6,3]:=
               aMatrix1[1,3] * aMatrix2[6,1]  +
               aMatrix1[2,3] * aMatrix2[6,2]  +
               aMatrix1[3,3] * aMatrix2[6,3]  +
               aMatrix1[4,3] * aMatrix2[6,4]  +
               aMatrix1[5,3] * aMatrix2[6,5]  +
               aMatrix1[6,3] * aMatrix2[6,6]  +
               aMatrix1[7,3] * aMatrix2[6,7]  ;

             Result[6,4]:=
               aMatrix1[1,4] * aMatrix2[6,1]  +
               aMatrix1[2,4] * aMatrix2[6,2]  +
               aMatrix1[3,4] * aMatrix2[6,3]  +
               aMatrix1[4,4] * aMatrix2[6,4]  +
               aMatrix1[5,4] * aMatrix2[6,5]  +
               aMatrix1[6,4] * aMatrix2[6,6]  +
               aMatrix1[7,4] * aMatrix2[6,7]  ;

             Result[6,5]:=
               aMatrix1[1,5] * aMatrix2[6,1]  +
               aMatrix1[2,5] * aMatrix2[6,2]  +
               aMatrix1[3,5] * aMatrix2[6,3]  +
               aMatrix1[4,5] * aMatrix2[6,4]  +
               aMatrix1[5,5] * aMatrix2[6,5]  +
               aMatrix1[6,5] * aMatrix2[6,6]  +
               aMatrix1[7,5] * aMatrix2[6,7]  ;

             Result[6,6]:=
               aMatrix1[1,6] * aMatrix2[6,1]  +
               aMatrix1[2,6] * aMatrix2[6,2]  +
               aMatrix1[3,6] * aMatrix2[6,3]  +
               aMatrix1[4,6] * aMatrix2[6,4]  +
               aMatrix1[5,6] * aMatrix2[6,5]  +
               aMatrix1[6,6] * aMatrix2[6,6]  +
               aMatrix1[7,6] * aMatrix2[6,7]  ;

             Result[6,7]:=
               aMatrix1[1,7] * aMatrix2[6,1]  +
               aMatrix1[2,7] * aMatrix2[6,2]  +
               aMatrix1[3,7] * aMatrix2[6,3]  +
               aMatrix1[4,7] * aMatrix2[6,4]  +
               aMatrix1[5,7] * aMatrix2[6,5]  +
               aMatrix1[6,7] * aMatrix2[6,6]  +
               aMatrix1[7,7] * aMatrix2[6,7]  ;


             Result[7,1]:=
               aMatrix1[1,1] * aMatrix2[7,1]  +
               aMatrix1[2,1] * aMatrix2[7,2]  +
               aMatrix1[3,1] * aMatrix2[7,3]  +
               aMatrix1[4,1] * aMatrix2[7,4]  +
               aMatrix1[5,1] * aMatrix2[7,5]  +
               aMatrix1[6,1] * aMatrix2[7,6]  +
               aMatrix1[7,1] * aMatrix2[7,7]  ;

             Result[7,2]:=
               aMatrix1[1,2] * aMatrix2[7,1]  +
               aMatrix1[2,2] * aMatrix2[7,2]  +
               aMatrix1[3,2] * aMatrix2[7,3]  +
               aMatrix1[4,2] * aMatrix2[7,4]  +
               aMatrix1[5,2] * aMatrix2[7,5]  +
               aMatrix1[6,2] * aMatrix2[7,6]  +
               aMatrix1[7,2] * aMatrix2[7,7]  ;

             Result[7,3]:=
               aMatrix1[1,3] * aMatrix2[7,1]  +
               aMatrix1[2,3] * aMatrix2[7,2]  +
               aMatrix1[3,3] * aMatrix2[7,3]  +
               aMatrix1[4,3] * aMatrix2[7,4]  +
               aMatrix1[5,3] * aMatrix2[7,5]  +
               aMatrix1[6,3] * aMatrix2[7,6]  +
               aMatrix1[7,3] * aMatrix2[7,7]  ;

             Result[7,4]:=
               aMatrix1[1,4] * aMatrix2[7,1]  +
               aMatrix1[2,4] * aMatrix2[7,2]  +
               aMatrix1[3,4] * aMatrix2[7,3]  +
               aMatrix1[4,4] * aMatrix2[7,4]  +
               aMatrix1[5,4] * aMatrix2[7,5]  +
               aMatrix1[6,4] * aMatrix2[7,6]  +
               aMatrix1[7,4] * aMatrix2[7,7]  ;

             Result[7,5]:=
               aMatrix1[1,5] * aMatrix2[7,1]  +
               aMatrix1[2,5] * aMatrix2[7,2]  +
               aMatrix1[3,5] * aMatrix2[7,3]  +
               aMatrix1[4,5] * aMatrix2[7,4]  +
               aMatrix1[5,5] * aMatrix2[7,5]  +
               aMatrix1[6,5] * aMatrix2[7,6]  +
               aMatrix1[7,5] * aMatrix2[7,7]  ;

             Result[7,6]:=
               aMatrix1[1,6] * aMatrix2[7,1]  +
               aMatrix1[2,6] * aMatrix2[7,2]  +
               aMatrix1[3,6] * aMatrix2[7,3]  +
               aMatrix1[4,6] * aMatrix2[7,4]  +
               aMatrix1[5,6] * aMatrix2[7,5]  +
               aMatrix1[6,6] * aMatrix2[7,6]  +
               aMatrix1[7,6] * aMatrix2[7,7]  ;

             Result[7,7]:=
               aMatrix1[1,7] * aMatrix2[7,1]  +
               aMatrix1[2,7] * aMatrix2[7,2]  +
               aMatrix1[3,7] * aMatrix2[7,3]  +
               aMatrix1[4,7] * aMatrix2[7,4]  +
               aMatrix1[5,7] * aMatrix2[7,5]  +
               aMatrix1[6,7] * aMatrix2[7,6]  +
               aMatrix1[7,7] * aMatrix2[7,7]  ;
             end; // 7


          6: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  +
               aMatrix1[5,1] * aMatrix2[1,5]  +
               aMatrix1[6,1] * aMatrix2[1,6]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  +
               aMatrix1[5,2] * aMatrix2[1,5]  +
               aMatrix1[6,2] * aMatrix2[1,6]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  +
               aMatrix1[5,3] * aMatrix2[1,5]  +
               aMatrix1[6,3] * aMatrix2[1,6]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  +
               aMatrix1[5,4] * aMatrix2[1,5]  +
               aMatrix1[6,4] * aMatrix2[1,6]  ;

             Result[1,5]:=
               aMatrix1[1,5] * aMatrix2[1,1]  +
               aMatrix1[2,5] * aMatrix2[1,2]  +
               aMatrix1[3,5] * aMatrix2[1,3]  +
               aMatrix1[4,5] * aMatrix2[1,4]  +
               aMatrix1[5,5] * aMatrix2[1,5]  +
               aMatrix1[6,5] * aMatrix2[1,6]  ;

             Result[1,6]:=
               aMatrix1[1,6] * aMatrix2[1,1]  +
               aMatrix1[2,6] * aMatrix2[1,2]  +
               aMatrix1[3,6] * aMatrix2[1,3]  +
               aMatrix1[4,6] * aMatrix2[1,4]  +
               aMatrix1[5,6] * aMatrix2[1,5]  +
               aMatrix1[6,6] * aMatrix2[1,6]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  +
               aMatrix1[5,1] * aMatrix2[2,5]  +
               aMatrix1[6,1] * aMatrix2[2,6]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  +
               aMatrix1[5,2] * aMatrix2[2,5]  +
               aMatrix1[6,2] * aMatrix2[2,6]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  +
               aMatrix1[5,3] * aMatrix2[2,5]  +
               aMatrix1[6,3] * aMatrix2[2,6]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  +
               aMatrix1[5,4] * aMatrix2[2,5]  +
               aMatrix1[6,4] * aMatrix2[2,6]  ;

             Result[2,5]:=
               aMatrix1[1,5] * aMatrix2[2,1]  +
               aMatrix1[2,5] * aMatrix2[2,2]  +
               aMatrix1[3,5] * aMatrix2[2,3]  +
               aMatrix1[4,5] * aMatrix2[2,4]  +
               aMatrix1[5,5] * aMatrix2[2,5]  +
               aMatrix1[6,5] * aMatrix2[2,6]  ;

             Result[2,6]:=
               aMatrix1[1,6] * aMatrix2[2,1]  +
               aMatrix1[2,6] * aMatrix2[2,2]  +
               aMatrix1[3,6] * aMatrix2[2,3]  +
               aMatrix1[4,6] * aMatrix2[2,4]  +
               aMatrix1[5,6] * aMatrix2[2,5]  +
               aMatrix1[6,6] * aMatrix2[2,6]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  +
               aMatrix1[5,1] * aMatrix2[3,5]  +
               aMatrix1[6,1] * aMatrix2[3,6]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  +
               aMatrix1[5,2] * aMatrix2[3,5]  +
               aMatrix1[6,2] * aMatrix2[3,6]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  +
               aMatrix1[5,3] * aMatrix2[3,5]  +
               aMatrix1[6,3] * aMatrix2[3,6]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  +
               aMatrix1[5,4] * aMatrix2[3,5]  +
               aMatrix1[6,4] * aMatrix2[3,6]  ;

             Result[3,5]:=
               aMatrix1[1,5] * aMatrix2[3,1]  +
               aMatrix1[2,5] * aMatrix2[3,2]  +
               aMatrix1[3,5] * aMatrix2[3,3]  +
               aMatrix1[4,5] * aMatrix2[3,4]  +
               aMatrix1[5,5] * aMatrix2[3,5]  +
               aMatrix1[6,5] * aMatrix2[3,6]  ;

             Result[3,6]:=
               aMatrix1[1,6] * aMatrix2[3,1]  +
               aMatrix1[2,6] * aMatrix2[3,2]  +
               aMatrix1[3,6] * aMatrix2[3,3]  +
               aMatrix1[4,6] * aMatrix2[3,4]  +
               aMatrix1[5,6] * aMatrix2[3,5]  +
               aMatrix1[6,6] * aMatrix2[3,6]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  +
               aMatrix1[5,1] * aMatrix2[4,5]  +
               aMatrix1[6,1] * aMatrix2[4,6]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  +
               aMatrix1[5,2] * aMatrix2[4,5]  +
               aMatrix1[6,2] * aMatrix2[4,6]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  +
               aMatrix1[5,3] * aMatrix2[4,5]  +
               aMatrix1[6,3] * aMatrix2[4,6]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  +
               aMatrix1[5,4] * aMatrix2[4,5]  +
               aMatrix1[6,4] * aMatrix2[4,6]  ;

             Result[4,5]:=
               aMatrix1[1,5] * aMatrix2[4,1]  +
               aMatrix1[2,5] * aMatrix2[4,2]  +
               aMatrix1[3,5] * aMatrix2[4,3]  +
               aMatrix1[4,5] * aMatrix2[4,4]  +
               aMatrix1[5,5] * aMatrix2[4,5]  +
               aMatrix1[6,5] * aMatrix2[4,6]  ;

             Result[4,6]:=
               aMatrix1[1,6] * aMatrix2[4,1]  +
               aMatrix1[2,6] * aMatrix2[4,2]  +
               aMatrix1[3,6] * aMatrix2[4,3]  +
               aMatrix1[4,6] * aMatrix2[4,4]  +
               aMatrix1[5,6] * aMatrix2[4,5]  +
               aMatrix1[6,6] * aMatrix2[4,6]  ;


             Result[5,1]:=
               aMatrix1[1,1] * aMatrix2[5,1]  +
               aMatrix1[2,1] * aMatrix2[5,2]  +
               aMatrix1[3,1] * aMatrix2[5,3]  +
               aMatrix1[4,1] * aMatrix2[5,4]  +
               aMatrix1[5,1] * aMatrix2[5,5]  +
               aMatrix1[6,1] * aMatrix2[5,6]  ;

             Result[5,2]:=
               aMatrix1[1,2] * aMatrix2[5,1]  +
               aMatrix1[2,2] * aMatrix2[5,2]  +
               aMatrix1[3,2] * aMatrix2[5,3]  +
               aMatrix1[4,2] * aMatrix2[5,4]  +
               aMatrix1[5,2] * aMatrix2[5,5]  +
               aMatrix1[6,2] * aMatrix2[5,6]  ;

             Result[5,3]:=
               aMatrix1[1,3] * aMatrix2[5,1]  +
               aMatrix1[2,3] * aMatrix2[5,2]  +
               aMatrix1[3,3] * aMatrix2[5,3]  +
               aMatrix1[4,3] * aMatrix2[5,4]  +
               aMatrix1[5,3] * aMatrix2[5,5]  +
               aMatrix1[6,3] * aMatrix2[5,6]  ;

             Result[5,4]:=
               aMatrix1[1,4] * aMatrix2[5,1]  +
               aMatrix1[2,4] * aMatrix2[5,2]  +
               aMatrix1[3,4] * aMatrix2[5,3]  +
               aMatrix1[4,4] * aMatrix2[5,4]  +
               aMatrix1[5,4] * aMatrix2[5,5]  +
               aMatrix1[6,4] * aMatrix2[5,6]  ;

             Result[5,5]:=
               aMatrix1[1,5] * aMatrix2[5,1]  +
               aMatrix1[2,5] * aMatrix2[5,2]  +
               aMatrix1[3,5] * aMatrix2[5,3]  +
               aMatrix1[4,5] * aMatrix2[5,4]  +
               aMatrix1[5,5] * aMatrix2[5,5]  +
               aMatrix1[6,5] * aMatrix2[5,6]  ;

             Result[5,6]:=
               aMatrix1[1,6] * aMatrix2[5,1]  +
               aMatrix1[2,6] * aMatrix2[5,2]  +
               aMatrix1[3,6] * aMatrix2[5,3]  +
               aMatrix1[4,6] * aMatrix2[5,4]  +
               aMatrix1[5,6] * aMatrix2[5,5]  +
               aMatrix1[6,6] * aMatrix2[5,6]  ;


             Result[6,1]:=
               aMatrix1[1,1] * aMatrix2[6,1]  +
               aMatrix1[2,1] * aMatrix2[6,2]  +
               aMatrix1[3,1] * aMatrix2[6,3]  +
               aMatrix1[4,1] * aMatrix2[6,4]  +
               aMatrix1[5,1] * aMatrix2[6,5]  +
               aMatrix1[6,1] * aMatrix2[6,6]  ;

             Result[6,2]:=
               aMatrix1[1,2] * aMatrix2[6,1]  +
               aMatrix1[2,2] * aMatrix2[6,2]  +
               aMatrix1[3,2] * aMatrix2[6,3]  +
               aMatrix1[4,2] * aMatrix2[6,4]  +
               aMatrix1[5,2] * aMatrix2[6,5]  +
               aMatrix1[6,2] * aMatrix2[6,6]  ;

             Result[6,3]:=
               aMatrix1[1,3] * aMatrix2[6,1]  +
               aMatrix1[2,3] * aMatrix2[6,2]  +
               aMatrix1[3,3] * aMatrix2[6,3]  +
               aMatrix1[4,3] * aMatrix2[6,4]  +
               aMatrix1[5,3] * aMatrix2[6,5]  +
               aMatrix1[6,3] * aMatrix2[6,6]  ;

             Result[6,4]:=
               aMatrix1[1,4] * aMatrix2[6,1]  +
               aMatrix1[2,4] * aMatrix2[6,2]  +
               aMatrix1[3,4] * aMatrix2[6,3]  +
               aMatrix1[4,4] * aMatrix2[6,4]  +
               aMatrix1[5,4] * aMatrix2[6,5]  +
               aMatrix1[6,4] * aMatrix2[6,6]  ;

             Result[6,5]:=
               aMatrix1[1,5] * aMatrix2[6,1]  +
               aMatrix1[2,5] * aMatrix2[6,2]  +
               aMatrix1[3,5] * aMatrix2[6,3]  +
               aMatrix1[4,5] * aMatrix2[6,4]  +
               aMatrix1[5,5] * aMatrix2[6,5]  +
               aMatrix1[6,5] * aMatrix2[6,6]  ;

             Result[6,6]:=
               aMatrix1[1,6] * aMatrix2[6,1]  +
               aMatrix1[2,6] * aMatrix2[6,2]  +
               aMatrix1[3,6] * aMatrix2[6,3]  +
               aMatrix1[4,6] * aMatrix2[6,4]  +
               aMatrix1[5,6] * aMatrix2[6,5]  +
               aMatrix1[6,6] * aMatrix2[6,6]  ;
             end; // 6


          5: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  +
               aMatrix1[5,1] * aMatrix2[1,5]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  +
               aMatrix1[5,2] * aMatrix2[1,5]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  +
               aMatrix1[5,3] * aMatrix2[1,5]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  +
               aMatrix1[5,4] * aMatrix2[1,5]  ;

             Result[1,5]:=
               aMatrix1[1,5] * aMatrix2[1,1]  +
               aMatrix1[2,5] * aMatrix2[1,2]  +
               aMatrix1[3,5] * aMatrix2[1,3]  +
               aMatrix1[4,5] * aMatrix2[1,4]  +
               aMatrix1[5,5] * aMatrix2[1,5]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  +
               aMatrix1[5,1] * aMatrix2[2,5]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  +
               aMatrix1[5,2] * aMatrix2[2,5]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  +
               aMatrix1[5,3] * aMatrix2[2,5]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  +
               aMatrix1[5,4] * aMatrix2[2,5]  ;

             Result[2,5]:=
               aMatrix1[1,5] * aMatrix2[2,1]  +
               aMatrix1[2,5] * aMatrix2[2,2]  +
               aMatrix1[3,5] * aMatrix2[2,3]  +
               aMatrix1[4,5] * aMatrix2[2,4]  +
               aMatrix1[5,5] * aMatrix2[2,5]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  +
               aMatrix1[5,1] * aMatrix2[3,5]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  +
               aMatrix1[5,2] * aMatrix2[3,5]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  +
               aMatrix1[5,3] * aMatrix2[3,5]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  +
               aMatrix1[5,4] * aMatrix2[3,5]  ;

             Result[3,5]:=
               aMatrix1[1,5] * aMatrix2[3,1]  +
               aMatrix1[2,5] * aMatrix2[3,2]  +
               aMatrix1[3,5] * aMatrix2[3,3]  +
               aMatrix1[4,5] * aMatrix2[3,4]  +
               aMatrix1[5,5] * aMatrix2[3,5]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  +
               aMatrix1[5,1] * aMatrix2[4,5]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  +
               aMatrix1[5,2] * aMatrix2[4,5]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  +
               aMatrix1[5,3] * aMatrix2[4,5]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  +
               aMatrix1[5,4] * aMatrix2[4,5]  ;

             Result[4,5]:=
               aMatrix1[1,5] * aMatrix2[4,1]  +
               aMatrix1[2,5] * aMatrix2[4,2]  +
               aMatrix1[3,5] * aMatrix2[4,3]  +
               aMatrix1[4,5] * aMatrix2[4,4]  +
               aMatrix1[5,5] * aMatrix2[4,5]  ;


             Result[5,1]:=
               aMatrix1[1,1] * aMatrix2[5,1]  +
               aMatrix1[2,1] * aMatrix2[5,2]  +
               aMatrix1[3,1] * aMatrix2[5,3]  +
               aMatrix1[4,1] * aMatrix2[5,4]  +
               aMatrix1[5,1] * aMatrix2[5,5]  ;

             Result[5,2]:=
               aMatrix1[1,2] * aMatrix2[5,1]  +
               aMatrix1[2,2] * aMatrix2[5,2]  +
               aMatrix1[3,2] * aMatrix2[5,3]  +
               aMatrix1[4,2] * aMatrix2[5,4]  +
               aMatrix1[5,2] * aMatrix2[5,5]  ;

             Result[5,3]:=
               aMatrix1[1,3] * aMatrix2[5,1]  +
               aMatrix1[2,3] * aMatrix2[5,2]  +
               aMatrix1[3,3] * aMatrix2[5,3]  +
               aMatrix1[4,3] * aMatrix2[5,4]  +
               aMatrix1[5,3] * aMatrix2[5,5]  ;

             Result[5,4]:=
               aMatrix1[1,4] * aMatrix2[5,1]  +
               aMatrix1[2,4] * aMatrix2[5,2]  +
               aMatrix1[3,4] * aMatrix2[5,3]  +
               aMatrix1[4,4] * aMatrix2[5,4]  +
               aMatrix1[5,4] * aMatrix2[5,5]  ;

             Result[5,5]:=
               aMatrix1[1,5] * aMatrix2[5,1]  +
               aMatrix1[2,5] * aMatrix2[5,2]  +
               aMatrix1[3,5] * aMatrix2[5,3]  +
               aMatrix1[4,5] * aMatrix2[5,4]  +
               aMatrix1[5,5] * aMatrix2[5,5]  ;
             end; // 5


          4: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  +
               aMatrix1[4,1] * aMatrix2[1,4]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  +
               aMatrix1[4,2] * aMatrix2[1,4]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  +
               aMatrix1[4,3] * aMatrix2[1,4]  ;

             Result[1,4]:=
               aMatrix1[1,4] * aMatrix2[1,1]  +
               aMatrix1[2,4] * aMatrix2[1,2]  +
               aMatrix1[3,4] * aMatrix2[1,3]  +
               aMatrix1[4,4] * aMatrix2[1,4]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  +
               aMatrix1[4,1] * aMatrix2[2,4]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  +
               aMatrix1[4,2] * aMatrix2[2,4]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  +
               aMatrix1[4,3] * aMatrix2[2,4]  ;

             Result[2,4]:=
               aMatrix1[1,4] * aMatrix2[2,1]  +
               aMatrix1[2,4] * aMatrix2[2,2]  +
               aMatrix1[3,4] * aMatrix2[2,3]  +
               aMatrix1[4,4] * aMatrix2[2,4]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  +
               aMatrix1[4,1] * aMatrix2[3,4]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  +
               aMatrix1[4,2] * aMatrix2[3,4]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  +
               aMatrix1[4,3] * aMatrix2[3,4]  ;

             Result[3,4]:=
               aMatrix1[1,4] * aMatrix2[3,1]  +
               aMatrix1[2,4] * aMatrix2[3,2]  +
               aMatrix1[3,4] * aMatrix2[3,3]  +
               aMatrix1[4,4] * aMatrix2[3,4]  ;


             Result[4,1]:=
               aMatrix1[1,1] * aMatrix2[4,1]  +
               aMatrix1[2,1] * aMatrix2[4,2]  +
               aMatrix1[3,1] * aMatrix2[4,3]  +
               aMatrix1[4,1] * aMatrix2[4,4]  ;

             Result[4,2]:=
               aMatrix1[1,2] * aMatrix2[4,1]  +
               aMatrix1[2,2] * aMatrix2[4,2]  +
               aMatrix1[3,2] * aMatrix2[4,3]  +
               aMatrix1[4,2] * aMatrix2[4,4]  ;

             Result[4,3]:=
               aMatrix1[1,3] * aMatrix2[4,1]  +
               aMatrix1[2,3] * aMatrix2[4,2]  +
               aMatrix1[3,3] * aMatrix2[4,3]  +
               aMatrix1[4,3] * aMatrix2[4,4]  ;

             Result[4,4]:=
               aMatrix1[1,4] * aMatrix2[4,1]  +
               aMatrix1[2,4] * aMatrix2[4,2]  +
               aMatrix1[3,4] * aMatrix2[4,3]  +
               aMatrix1[4,4] * aMatrix2[4,4]  ;
             end; // 4

          3: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  +
               aMatrix1[3,1] * aMatrix2[1,3]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  +
               aMatrix1[3,2] * aMatrix2[1,3]  ;

             Result[1,3]:=
               aMatrix1[1,3] * aMatrix2[1,1]  +
               aMatrix1[2,3] * aMatrix2[1,2]  +
               aMatrix1[3,3] * aMatrix2[1,3]  ;


             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  +
               aMatrix1[3,1] * aMatrix2[2,3]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  +
               aMatrix1[3,2] * aMatrix2[2,3]  ;

             Result[2,3]:=
               aMatrix1[1,3] * aMatrix2[2,1]  +
               aMatrix1[2,3] * aMatrix2[2,2]  +
               aMatrix1[3,3] * aMatrix2[2,3]  ;


             Result[3,1]:=
               aMatrix1[1,1] * aMatrix2[3,1]  +
               aMatrix1[2,1] * aMatrix2[3,2]  +
               aMatrix1[3,1] * aMatrix2[3,3]  ;

             Result[3,2]:=
               aMatrix1[1,2] * aMatrix2[3,1]  +
               aMatrix1[2,2] * aMatrix2[3,2]  +
               aMatrix1[3,2] * aMatrix2[3,3]  ;

             Result[3,3]:=
               aMatrix1[1,3] * aMatrix2[3,1]  +
               aMatrix1[2,3] * aMatrix2[3,2]  +
               aMatrix1[3,3] * aMatrix2[3,3]  ;
             end; // 3


           2: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  +
               aMatrix1[2,1] * aMatrix2[1,2]  ;

             Result[1,2]:=
               aMatrix1[1,2] * aMatrix2[1,1]  +
               aMatrix1[2,2] * aMatrix2[1,2]  ;

             Result[2,1]:=
               aMatrix1[1,1] * aMatrix2[2,1]  +
               aMatrix1[2,1] * aMatrix2[2,2]  ;

             Result[2,2]:=
               aMatrix1[1,2] * aMatrix2[2,1]  +
               aMatrix1[2,2] * aMatrix2[2,2]  ;
             end; // 2


          1: begin
             Result[1,1]:=
               aMatrix1[1,1] * aMatrix2[1,1]  ;
             end; // 1

          end;

        end; // func

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitVector
        * Result   :  XVector
        * History  :  New
        ******************************************************************************)
        function InitVector ( const k: byte; const aValue: integer): XVector;
        var
          i: byte;
        begin

          if ( k in Range1to10 ) then begin
             case k of

                10: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                   Result[5]:= aValue;
                   Result[6]:= aValue;
                   Result[7]:= aValue;
                   Result[8]:= aValue;
                   Result[9]:= aValue;
                   Result[10]:= aValue;
                end; // 10

                9: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                   Result[5]:= aValue;
                   Result[6]:= aValue;
                   Result[7]:= aValue;
                   Result[8]:= aValue;
                   Result[9]:= aValue;
                end; // 9

                8: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                   Result[5]:= aValue;
                   Result[6]:= aValue;
                   Result[7]:= aValue;
                   Result[8]:= aValue;
                end; // 8

                7: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                   Result[5]:= aValue;
                   Result[6]:= aValue;
                   Result[7]:= aValue;
                end; // 7

                6: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                   Result[5]:= aValue;
                   Result[6]:= aValue;
                end; // 6

                5: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                   Result[5]:= aValue;
                end; // 5

                4: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                   Result[4]:= aValue;
                 end; // 4

                3: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                   Result[3]:= aValue;
                end; // 3

                2: begin
                   Result[1]:= aValue;
                   Result[2]:= aValue;
                end; // 2

                1: Result[1]:= aValue;

             end; // case
          end // if () begin

          else if ( k in [11..EdburgMaxVariables] ) then
            for i:= 1 to k do
                Result[i]:= aValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  InitMatrix
        * Result   :  XMatrix
        * History  :  New
        ******************************************************************************)
        function InitMatrix ( const k: byte; const aValue : integer):XMatrix;
        var
          i,j:byte;
        begin
(*
          if ( k in Range1to10 ) then begin

             case k of
             10: begin

                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;
                Result[1,5]:= aValue;
                Result[1,6]:= aValue;
                Result[1,7]:= aValue;
                Result[1,8]:= aValue;
                Result[1,9]:= aValue;
                Result[1,10]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;
                Result[2,5]:= aValue;
                Result[2,6]:= aValue;
                Result[2,7]:= aValue;
                Result[2,8]:= aValue;
                Result[2,9]:= aValue;
                Result[2,10]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;
                Result[3,5]:= aValue;
                Result[3,6]:= aValue;
                Result[3,7]:= aValue;
                Result[3,8]:= aValue;
                Result[3,9]:= aValue;
                Result[3,10]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
                Result[4,5]:= aValue;
                Result[4,6]:= aValue;
                Result[4,7]:= aValue;
                Result[4,8]:= aValue;
                Result[4,9]:= aValue;
                Result[4,10]:= aValue;

                Result[5,1]:= aValue;
                Result[5,2]:= aValue;
                Result[5,3]:= aValue;
                Result[5,4]:= aValue;
                Result[5,5]:= aValue;
                Result[5,6]:= aValue;
                Result[5,7]:= aValue;
                Result[5,8]:= aValue;
                Result[5,9]:= aValue;
                Result[5,10]:= aValue;

                Result[6,1]:= aValue;
                Result[6,2]:= aValue;
                Result[6,3]:= aValue;
                Result[6,4]:= aValue;
                Result[6,5]:= aValue;
                Result[6,6]:= aValue;
                Result[6,7]:= aValue;
                Result[6,8]:= aValue;
                Result[6,9]:= aValue;
                Result[6,10]:= aValue;

                Result[7,1]:= aValue;
                Result[7,2]:= aValue;
                Result[7,3]:= aValue;
                Result[7,4]:= aValue;
                Result[7,5]:= aValue;
                Result[7,6]:= aValue;
                Result[7,7]:= aValue;
                Result[7,8]:= aValue;
                Result[7,9]:= aValue;
                Result[7,10]:= aValue;

                Result[8,1]:= aValue;
                Result[8,2]:= aValue;
                Result[8,3]:= aValue;
                Result[8,4]:= aValue;
                Result[8,5]:= aValue;
                Result[8,6]:= aValue;
                Result[8,7]:= aValue;
                Result[8,8]:= aValue;
                Result[8,9]:= aValue;
                Result[8,10]:= aValue;

                Result[9,1]:= aValue;
                Result[9,2]:= aValue;
                Result[9,3]:= aValue;
                Result[9,4]:= aValue;
                Result[9,5]:= aValue;
                Result[9,6]:= aValue;
                Result[9,7]:= aValue;
                Result[9,8]:= aValue;
                Result[9,9]:= aValue;
                Result[9,10]:= aValue;

                Result[10,1]:= aValue;
                Result[10,2]:= aValue;
                Result[10,3]:= aValue;
                Result[10,4]:= aValue;
                Result[10,5]:= aValue;
                Result[10,6]:= aValue;
                Result[10,7]:= aValue;
                Result[10,8]:= aValue;
                Result[10,9]:= aValue;
                Result[10,10]:= aValue;
             end;

             9: begin

                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;
                Result[1,5]:= aValue;
                Result[1,6]:= aValue;
                Result[1,7]:= aValue;
                Result[1,8]:= aValue;
                Result[1,9]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;
                Result[2,5]:= aValue;
                Result[2,6]:= aValue;
                Result[2,7]:= aValue;
                Result[2,8]:= aValue;
                Result[2,9]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;
                Result[3,5]:= aValue;
                Result[3,6]:= aValue;
                Result[3,7]:= aValue;
                Result[3,8]:= aValue;
                Result[3,9]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
                Result[4,5]:= aValue;
                Result[4,6]:= aValue;
                Result[4,7]:= aValue;
                Result[4,8]:= aValue;
                Result[4,9]:= aValue;

                Result[5,1]:= aValue;
                Result[5,2]:= aValue;
                Result[5,3]:= aValue;
                Result[5,4]:= aValue;
                Result[5,5]:= aValue;
                Result[5,6]:= aValue;
                Result[5,7]:= aValue;
                Result[5,8]:= aValue;
                Result[5,9]:= aValue;

                Result[6,1]:= aValue;
                Result[6,2]:= aValue;
                Result[6,3]:= aValue;
                Result[6,4]:= aValue;
                Result[6,5]:= aValue;
                Result[6,6]:= aValue;
                Result[6,7]:= aValue;
                Result[6,8]:= aValue;
                Result[6,9]:= aValue;

                Result[7,1]:= aValue;
                Result[7,2]:= aValue;
                Result[7,3]:= aValue;
                Result[7,4]:= aValue;
                Result[7,5]:= aValue;
                Result[7,6]:= aValue;
                Result[7,7]:= aValue;
                Result[7,8]:= aValue;
                Result[7,9]:= aValue;

                Result[8,1]:= aValue;
                Result[8,2]:= aValue;
                Result[8,3]:= aValue;
                Result[8,4]:= aValue;
                Result[8,5]:= aValue;
                Result[8,6]:= aValue;
                Result[8,7]:= aValue;
                Result[8,8]:= aValue;
                Result[8,9]:= aValue;

                Result[9,1]:= aValue;
                Result[9,2]:= aValue;
                Result[9,3]:= aValue;
                Result[9,4]:= aValue;
                Result[9,5]:= aValue;
                Result[9,6]:= aValue;
                Result[9,7]:= aValue;
                Result[9,8]:= aValue;
                Result[9,9]:= aValue;
             end;  //9

             8: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;
                Result[1,5]:= aValue;
                Result[1,6]:= aValue;
                Result[1,7]:= aValue;
                Result[1,8]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;
                Result[2,5]:= aValue;
                Result[2,6]:= aValue;
                Result[2,7]:= aValue;
                Result[2,8]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;
                Result[3,5]:= aValue;
                Result[3,6]:= aValue;
                Result[3,7]:= aValue;
                Result[3,8]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
                Result[4,5]:= aValue;
                Result[4,6]:= aValue;
                Result[4,7]:= aValue;
                Result[4,8]:= aValue;

                Result[5,1]:= aValue;
                Result[5,2]:= aValue;
                Result[5,3]:= aValue;
                Result[5,4]:= aValue;
                Result[5,5]:= aValue;
                Result[5,6]:= aValue;
                Result[5,7]:= aValue;
                Result[5,8]:= aValue;

                Result[6,1]:= aValue;
                Result[6,2]:= aValue;
                Result[6,3]:= aValue;
                Result[6,4]:= aValue;
                Result[6,5]:= aValue;
                Result[6,6]:= aValue;
                Result[6,7]:= aValue;
                Result[6,8]:= aValue;

                Result[7,1]:= aValue;
                Result[7,2]:= aValue;
                Result[7,3]:= aValue;
                Result[7,4]:= aValue;
                Result[7,5]:= aValue;
                Result[7,6]:= aValue;
                Result[7,7]:= aValue;
                Result[7,8]:= aValue;

                Result[8,1]:= aValue;
                Result[8,2]:= aValue;
                Result[8,3]:= aValue;
                Result[8,4]:= aValue;
                Result[8,5]:= aValue;
                Result[8,6]:= aValue;
                Result[8,7]:= aValue;
                Result[8,8]:= aValue;
             end; //8

             7: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;
                Result[1,5]:= aValue;
                Result[1,6]:= aValue;
                Result[1,7]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;
                Result[2,5]:= aValue;
                Result[2,6]:= aValue;
                Result[2,7]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;
                Result[3,5]:= aValue;
                Result[3,6]:= aValue;
                Result[3,7]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
                Result[4,5]:= aValue;
                Result[4,6]:= aValue;
                Result[4,7]:= aValue;

                Result[5,1]:= aValue;
                Result[5,2]:= aValue;
                Result[5,3]:= aValue;
                Result[5,4]:= aValue;
                Result[5,5]:= aValue;
                Result[5,6]:= aValue;
                Result[5,7]:= aValue;

                Result[6,1]:= aValue;
                Result[6,2]:= aValue;
                Result[6,3]:= aValue;
                Result[6,4]:= aValue;
                Result[6,5]:= aValue;
                Result[6,6]:= aValue;
                Result[6,7]:= aValue;

                Result[7,1]:= aValue;
                Result[7,2]:= aValue;
                Result[7,3]:= aValue;
                Result[7,4]:= aValue;
                Result[7,5]:= aValue;
                Result[7,6]:= aValue;
                Result[7,7]:= aValue;
             end; //7

             6: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;
                Result[1,5]:= aValue;
                Result[1,6]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;
                Result[2,5]:= aValue;
                Result[2,6]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;
                Result[3,5]:= aValue;
                Result[3,6]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
                Result[4,5]:= aValue;
                Result[4,6]:= aValue;

                Result[5,1]:= aValue;
                Result[5,2]:= aValue;
                Result[5,3]:= aValue;
                Result[5,4]:= aValue;
                Result[5,5]:= aValue;
                Result[5,6]:= aValue;

                Result[6,1]:= aValue;
                Result[6,2]:= aValue;
                Result[6,3]:= aValue;
                Result[6,4]:= aValue;
                Result[6,5]:= aValue;
                Result[6,6]:= aValue;
             end; //6

             5: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;
                Result[1,5]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;
                Result[2,5]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;
                Result[3,5]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
                Result[4,5]:= aValue;

                Result[5,1]:= aValue;
                Result[5,2]:= aValue;
                Result[5,3]:= aValue;
                Result[5,4]:= aValue;
                Result[5,5]:= aValue;
             end; //5

             4: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;
                Result[1,4]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;
                Result[2,4]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
                Result[3,4]:= aValue;

                Result[4,1]:= aValue;
                Result[4,2]:= aValue;
                Result[4,3]:= aValue;
                Result[4,4]:= aValue;
             end; //4

             3: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[1,3]:= aValue;

                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
                Result[2,3]:= aValue;

                Result[3,1]:= aValue;
                Result[3,2]:= aValue;
                Result[3,3]:= aValue;
             end; //3

             2: begin
                Result[1,1]:= aValue;
                Result[1,2]:= aValue;
                Result[2,1]:= aValue;
                Result[2,2]:= aValue;
             end; //2

             1: Result[1,1]:= aValue;

             end; // case
          end

          else *)
          if ( k in [11..EdburgMaxVariables] ) then
             for i:= 1 to k do
                for j:= 1 to k do
                   Result[i,j]:= aValue;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  MatrixTrace
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function MatrixTrace ( const aMatrix : XMatrix; const k : byte): extended;

        var
          i: byte;
        begin
          Result:= 0;

(*
          if ( k in Range1to10 ) then begin

          case k of
             10: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4] +
                   aMatrix[5,5] +
                   aMatrix[6,6] +
                   aMatrix[7,7] +
                   aMatrix[8,8] +
                   aMatrix[9,9] +
                   aMatrix[10,10];

             9: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4] +
                   aMatrix[5,5] +
                   aMatrix[6,6] +
                   aMatrix[7,7] +
                   aMatrix[8,8] +
                   aMatrix[9,9];

             8: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4] +
                   aMatrix[5,5] +
                   aMatrix[6,6] +
                   aMatrix[7,7] +
                   aMatrix[8,8];

             7: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4] +
                   aMatrix[5,5] +
                   aMatrix[6,6] +
                   aMatrix[7,7];

             6: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4] +
                   aMatrix[5,5] +
                   aMatrix[6,6];

             5: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4] +
                   aMatrix[5,5];

             4: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3] +
                   aMatrix[4,4];

             3: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2] +
                   aMatrix[3,3];

             2: Result:=
                   aMatrix[1,1] +
                   aMatrix[2,2];

             1: Result:= aMatrix[1,1];

             end; // case
           end // if ()

           else begin
              for i := 1 to k do
                 Result:= Result + aMatrix[i,i];
           end; // else begin
           *)
           for i := 1 to k do
              Result:= Result + aMatrix[i,i];

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetLDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetLDistance (  const aVector    : XVector;     // 'X'
                                 const MeanVector : XVector;     // 'M'
                                 const k          : byte): extended;

        // Euclidean Dist: LD = (Sum of (X[i] - M(i)))

        var
          i: byte;
        begin
          Result:= 0;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            for i:= 1 to k do
              Result:= Result + abs(aVector[i] - MeanVector[i]);
          end; // if ( k in [1..])

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetEuclideanDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetEuclideanDistance (const aVector    : XVector;     // 'X'
                                       const MeanVector : XVector;     // 'M'
                                       const k          : byte): extended;

        // Euclidean Dist: ED = sqrt(Sum of sqr(X[i] - M(i)))

        var
          i    : byte;
          dTemp : extended;
        begin
          Result:= 0;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            for i:= 1 to k do begin
              dTemp  := aVector[i] - MeanVector[i];
              Result:= Result + ( dTemp * dTemp );
            end; // for i:= 1 to k
            Result:= sqrt(Result);
          end; // if ( k in [1..])

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetNormEuclideanDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetNormEuclideanDistance (const aVector    : XVector;    // 'X'
                                           const MeanVector : XVector;    // 'M'
                                           const CovMatrix  : XMatrix;
                                           const k          : byte): extended;

        var
          i    : byte;
          dTemp : extended;
        begin
          result:= 0;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            for i:= 1 to k do begin
              dTemp:= ( aVector[i] - MeanVector[i] ) / sqrt(CovMatrix[i,i]);
              Result:= Result + ( dTemp * dTemp );
            end; // for i:= 1 to k
            Result:= sqrt(Result);
          end; // if ( k in [])

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMahalanobisDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMahalanobisDistance       (const aVector    : XVector;    // 'X'
                                               const MeanVector : XVector;    // 'M'
                                               const CovMatrix  : XMatrix;
                                               const k          : byte): extended;

        // Formula: MDistance = (X-M(i))t * InvC(i) * X-M(i)
        //                      [   TempVector    ]
        // Where: X is data vector,
        //        M is mean sample/group values vector
        //        InvC is the inverse of the sample/group
        //          covariance matrix
        //        k is number of vars

        var
          InvCovMatrix         : XMatrix;
          DataVectorMinusMeans : XVector;
          TempVector           : XVector;
          i,j                  : byte;
          dSum                 : extended;
          dDet                 : double;
        begin
          InvCovMatrix := InitMatrix(EdburgMaxVariables,0);
          result       := 0;

          try
            if ( k in [1..EdburgMaxVariables-1] ) then begin
              InvCovMatrix := InvertMatrix(CovMatrix,k,dDet);

              for i:= 1 to k do
                DataVectorMinusMeans[i]:= aVector[i] - MeanVector[i];

              for i:= 1 to k do begin
                dSum:= 0;
                  for j:= 1 to k do
                    dSum := dSum + DataVectorMinusMeans[j] * InvCovMatrix[i,j];
                TempVector[i]:= dSum;
              end; // for i:= 1 to k

              dSum:= 0;
              for i:= 1 to k do
                dSum:= dSum + TempVector[i] * DataVectorMinusMeans[i];

              Result:= dSum;
            end; // if ( k in [1..])

          except
            Raise Exception.Create(MahalanobisDistanceError);
          end; // try - except - end

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMahalanobisDistanceInvCov
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMahalanobisDistanceInvCov
                                              (const aVector      : XVector;    // 'X'
                                               const MeanVector   : XVector;    // 'M'
                                               const InvCovMatrix : XMatrix;
                                               const k            : byte): extended;

        // Formula: MDistance = (X-M(i))t * InvC(i) * X-M(i)
        //                      [   TempVector    ]
        //
        // Where: X is data vector,
        //        M is mean sample/group values vector
        //        InvC is the inverse of the sample/group
        //          covariance matrix
        //        k is number of vars

        var
          DataVectorMinusMeans : XVector;
          TempVector           : XVector;
          i,j                  : byte;
          dSum                 : extended;
        begin
          result:= 0;

          try
            if ( k in [1..EdburgMaxVariables-1] ) then begin

              for i:= 1 to k do
                DataVectorMinusMeans[i]:= aVector[i] - MeanVector[i];

              for i:= 1 to k do begin
                dSum:= 0;
                  for j:= 1 to k do
                    dSum := dSum + DataVectorMinusMeans[j] * InvCovMatrix[i,j];
                TempVector[i]:= dSum;
              end; // for i:= 1 to k

              dSum:= 0;
              for i:= 1 to k do
                dSum:= dSum + TempVector[i] * DataVectorMinusMeans[i];

              Result:= dSum;
            end; // if ( k in [1..])

          except
            Raise Exception.Create(MahalanobisDistanceError);
          end; // try - except - end

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMaximumLikelihoodDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMaximumLikelihoodDistance
                                              (const aVector    : XVector;    // 'X'
                                               const MeanVector : XVector;    // 'M'
                                               const CovMatrix  : XMatrix;
                                               const k          : byte;
                                               const APriori    : extended): extended;

        var
          dMLDist       : extended;
          InvCovMatrix  : XMatrix;
          dDeterminant  : double;
        begin
          Result        := 0;
          InvCovMatrix := InitMatrix(EdburgMaxVariables,0);

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            try
              InvCovMatrix:= InvertMatrix(CovMatrix,k,dDeterminant);
              dMLDist:= GetMahalanobisDistance(aVector,MeanVector,CovMatrix,k); // 1st, Mahalanobis Dists

              if ( APriori > 0 ) then result:= APriori - ( ONE_HALF * ln(dDeterminant) ) - ( ONE_HALF * dMLDist )
              else                    result:= ln(dDeterminant) + dMLDist ;

            except
              Raise Exception.Create(MaximumLikelihoodDistancesError);
            end; // try - except - end
          end; // if

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  11/16/00
        * Purpose  :  GetMaximumLikelihoodDistance
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function GetMaximumLikelihoodDistanceInvCov
                                              (const aVector      : XVector;    // 'X'
                                               const MeanVector   : XVector;    // 'M'
                                               const InvCovMatrix : XMatrix;
                                               const dDeterminant : double;
                                               const k            : byte;
                                               const APriori      : extended): extended;

        var
          dMLDist: extended;
        begin
          Result:= 0;

          if ( k in [1..EdburgMaxVariables-1] ) then begin
            try
              dMLDist:= GetMahalanobisDistanceInvCov(aVector,MeanVector,InvCovMatrix,k); // 1st, Mahalanobis Dists

              if ( APriori > 0 ) then  result:= APriori - ( ONE_HALF * ln(dDeterminant) ) - ( ONE_HALF * dMLDist )
              else                     result:= ln(dDeterminant) + dMLDist ;

            except
              Raise Exception.Create(MaximumLikelihoodDistancesError);
            end; // try - except - end
          end; // if

        end;


        // ===========================================================================


        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/29/03
        * Purpose  :  ErrorMatrixSum
        * Result   :  extended
        * History  :  New
        ******************************************************************************)
        function ErrorMatrixSum (const ematErrorMatrix : TErrorMatrix;
                                 const k               : byte): double;

        var
          i,j: integer;
        begin
          Result:= 0;
          if ( k < MAXSIGNATURES ) then begin
            for i:= 0 to k-1 do
              for j:= 0 to k-1 do
                Result:= Result + ematErrorMatrix[i,j];
          end; // if ()
        end;


        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/29/03
        * Purpose  :  CrossTabulation - simple 1 'level' using 2 arrays of int
        * Result   :  TErrorMatrix
        * History  :  New
        ******************************************************************************)
        function CrossTabulation(const arrX : TIntegerArray;
                                 const arrY : TIntegerArray;
                                 const n    : integer;
                                 var emat   : TErrorMatrix): integer;

        var
          i,iVar1,iVar2 : integer;
          iMaxXValue    : integer;
          iMaxYValue    : integer;
          bTooManyGroups : boolean;
        begin
          InitErrorMatrix(emat);
          bTooManyGroups:= FALSE;

          iMaxXValue := 0;
          iMaxYValue := 0;

          for i:= 0 to n-1 do begin
            if ( arrX[i] > MAXSIGNATURES ) then bTooManyGroups:= TRUE;
            if ( arrY[i] > MAXSIGNATURES ) then bTooManyGroups:= TRUE;
            if ( bTooManyGroups ) then begin
              Result:= -1;
              Raise Exception.Create('Error - too many groups in CrossTabulation');
              EXIT;
            end; // if
          end; // for

          for i:= 0 to n-1 do begin
            iVar1:= arrX[i];
            iVar2:= arrY[i];

            if ( iVar1 > iMaxXValue ) then
              iMaxXValue:= iVar1;
            if ( iVar2 > iMaxYValue ) then
              iMaxYValue:= iVar2;

            emat[iVar1-1,iVar2-1]:= emat[iVar1-1,iVar2-1] + 1;
          end; // for i

          if ( iMaxXValue > iMaxYValue ) then
            result:= iMaxXValue
          else
            result:= iMaxYValue;

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/29/03
        * Purpose  :  CompileErrorMatrix
        * Result   :
        * History  :  New
        ******************************************************************************)
        procedure CompileErrorMatrix (var ematCompiled : TErrorMatrix;
                                          k            : byte);

        // Contingency matrix (errors of omission vs. commission)

        // Remember, that the 0..k-1 rows/columns represent the summary
        // correct values for each var-the k..k+1 represent the row/col.
        // totals and percent error respectively.  (See diagram below)

        var
          i,j           : byte;
          dTotalCorrect : double;
        begin                 // i = rows , j = columns
          try
            dTotalCorrect:= 0;

          // compute column 'k' totals for each row
          for i:= 0 to k-1 do
            for j:= 0 to k-1 do
              ematCompiled[i,k]:= ematCompiled[i,k] + ematCompiled[i,j];

          // compute row 'k' totals for each column
          for i:= 0 to k-1 do      // rows
            for j:= 0 to k-1 do     // cols
              ematCompiled[k,j] := ematCompiled[k,j] + ematCompiled[i,j];

          // divide diagonal by row totals to create %error (comm.)
          for i:= 0 to k-1 do begin
            if ( ematCompiled[i,k] > 0 ) then
              ematCompiled[i,k+1]:= 1 - ( ematCompiled[i,i] / ematCompiled[i,k] )
            else
              ematCompiled[i,k+1]:= 0;
          end; // for i

          // divide diagonal by col. totals to create %error (omis)
          for i:= 0 to k-1 do begin
            if ( ematCompiled[k,i] > 0 ) then
              ematCompiled[k+1,i]:= 1 - ( ematCompiled[i,i] / ematCompiled[k,i] )
            else
              ematCompiled[k+1,i]:= 0;
          end; // for i

          // calculate total "n" by adding values in column 'k'
          for i:= 0 to k-1 do
            ematCompiled[k,k]:= ematCompiled[k,k] + ematCompiled[i,k];

          // sum the correctly assigned elements (total of diag.)
          for i:= 0 to k-1 do
            dTotalCorrect:= dTotalCorrect + ematCompiled[i,i];

          // Error= 1-(#elements grouped correctly/total elements)
          ematCompiled[k+1,k+1]:= dTotalCorrect;

        { Diagram:

         For 3 groups (eg. k=3) : (Remeber rows=i, cols=j)

                               TRUE

                        x1     x2     x3   Totals  %Error
                   x1 [0,0]     .      .     .     [0,k+1]
        Assigned
                   x2 .         .
                                       .
                   x3 .

               Totals .                    [k,k]

              %Error [k+1,0]                      [k+1,k+1]
        }
          except
            Raise Exception.Create('Error compiling error matrix');
          end; // try - except - end

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  10/29/03
        * Purpose  :
        * Result   :  TKappaRec
        * History  :  New
        ******************************************************************************)
        function GetKappaValue (var ematErrorMatrix : TErrorMatrix;
                                    k               : byte): TKappaRec;
        var
          i,j: integer;
          RowandColProducts: TErrorMatrix;
          KappaRec: TKappaRec;
        begin
          KappaRec:= InitKappaRec;

          try
            with KappaRec do begin
              ObsGrandTotal:= ErrorMatrixSum(ematErrorMatrix,k);

              for i:= 0 to k-1 do
                ObsTotalCorrect:= ObsTotalCorrect + ematErrorMatrix[i,i];

              if ( ObsGrandTotal > 0 ) then
                ObsPctCorrect:= ObsTotalCorrect / ObsGrandTotal
              else
                ObsPctCorrect:= 0;

              for i:= 0 to k-1 do
                for j:= 0 to k-1 do
                  RowandColProducts[i,j]:= ematErrorMatrix[k,i] * ematErrorMatrix[j,k];
              ExpGrandTotal:= ErrorMatrixSum(RowandColProducts,k);

              for i:= 0 to k-1 do
                ExpTotalCorrect:= ExpTotalCorrect + RowandColProducts[i,i];

              if ( ExpGrandTotal > 0 ) then
                ExpPctCorrect:= ExpTotalCorrect / ExpGrandTotal
              else
                ExpPctCorrect:= 0;

              Kappa:= ( ObsPctCorrect - ExpPctCorrect ) / ( 1 - ExpPctCorrect );
            end; // with KappaRec

            Result:= KappaRec;
          except
            Raise Exception.Create('Error computing Kappa error matrix summary');
          end; // try - except - end

        end;


        // ===========================================================================

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  RescaleDataArrayZeroMin
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        procedure RescaleDataArrayZeroMin ( var dArray             : array of double;
                                            const n                : integer;
                                            const bTruncValues     : boolean);
        var
          dMin: double;
          i: integer;
        begin
          dMin:= GetMin(dArray,n);

          if ( bTruncValues ) then begin
            for i:= 0 to n-1 do
              dArray[i]:= Trunc(dArray[i] - dMin);
          end // if

          else begin
            for i:= 0 to n-1 do
              dArray[i]:= dArray[i] - dMin;
          end; // else begin

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  RandomDataArray
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        procedure RandomDataArray (var dArray   : array of double;
                                   const n      : integer;
                                   const iValue : integer);
        var
          i: integer;
        begin
          Randomize;
          for i:= 0 to n-1 do
            dArray[i]:= Random(iValue);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  RandomNormalDataArray
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        procedure RandomNormalDataArray (var dArray    : array of double;
                                         const n       : integer;
                                         const dMean   : double;
                                         const dStdDev : double);
        var
          i : integer;
        begin
          Randomize;
          for i:= 0 to n-1 do
            dArray[i]:= RandG(dMean,dStdDev);
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  CalcHistogram
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function CalcHistogram  ( const dArray            : array of double;
                                  const n                 : integer;
                                  const dBinSize          : double;
                                  const bRoundBinStart    : boolean;
                                  var Histogram           : THistogramArray): integer;
        var
          i,j: integer;
          dRange: double;
          dMin: double;
          dMax: double;
          dStartValue: double;
          dEndValue: double;
          iRunningSum: integer;
          iNumberBins: integer;
        begin
          Result:= 0;

          dMin:= GetMin(dArray,n);
          dMax:= GetMax(dArray,n);
          dRange:= dMax - dMin;


          if ( n < 3 ) then begin
            MessageDlg('Error - number of input observations is too small (<3). Aborted',
              mtError,[mbOK],0);
            EXIT;
          end; // if ()

          if ( dBinSize > 0 ) then
            iNumberBins:= Round( dRange / dBinSize )
          else begin
            MessageDlg('Error - bin size = 0. Aborted',
               mtError,[mbOK],0);
            EXIT;
          end; // else begin

          if ( iNumberBins > MAXHISTOBINS ) then begin
            MessageDlg('Error - range specified yields too many histogram bins. Aborted',
              mtError,[mbOK],0);
            EXIT;
          end; // if ()


          // Init histograms
          for i:= 1 to MAXHISTOBINS do begin
            Histogram[i].Lower  := 0;
            Histogram[i].Upper  := 0;
            Histogram[i].iCount := 0;
            Histogram[i].iCumulativeCount:= 0;
          end; // for i

          dStartValue := dMin;
          dEndValue   := dMin + dBinSize;

          // Calc histogram bins:
          Histogram[1].Lower:= dStartValue;
          Histogram[1].Upper:= dEndValue;

          for i:= 2 to iNumberBins do begin
            dStartValue := dStartValue + dBinSize;
            dEndValue   := dEndValue + dBinSize;
            Histogram[i].Lower:= dStartValue;
            Histogram[i].Upper:= dEndValue;
          end; // for i

          for i:= 0 to n-1 do begin
            for j:= 1 to iNumberBins do begin
              if ( dArray[i] >= Histogram[j].Lower ) and
                 ( dArray[i]  < Histogram[j].Upper ) then begin
                    inc(Histogram[j].iCount);
              end; // if ()
            end; // for j
          end; // for i

          // Now calc the cumulative bin totals
          //iRunningSum                     := 0;
          Histogram[1].iCumulativeCount   := Histogram[1].iCount;
          Histogram[1].dProportionOfTotal := Histogram[1].iCount / n;
          iRunningSum                     := Histogram[1].iCumulativeCount;

          for i:= 2 to iNumberBins do begin
            iRunningSum:= iRunningSum + Histogram[i].iCount;
            Histogram[i].iCumulativeCount:= iRunningSum;
            Histogram[i].dProportionOfTotal:= Histogram[i].iCount / n;
          end; // for i

          Result:= iNumberBins;
        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  ApplyLinearCombination
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        procedure ApplyLinearCombination (const PRegressData  : PTRegressDataArray;
                                          var   Y             : array of double;
                                          const k             : byte;
                                          const n             : integer;
                                          const arrWeights    : XVector);

        var
          i,j:integer;
          dTmp: double;
        begin

          for i:= 0 to n-1 do begin
            dTmp:= 0;

            for j:= 1 to k do
              dTmp:= dTmp + ( arrWeights[j] * PRegressData[j,i] );

            Y[i]:= dTmp;
          end; // for i

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  MeansDifferencesMatrix
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function MeansDifferencesMatrix (const MeansVector : XVector;
                                         const k           : byte): XMatrix;

        var
          i,j          : integer;
          MeansDMatrix : XMatrix;
        begin
          MeansDMatrix := InitMatrix(EdburgMaxVariables,0);
          result       := MeansDMatrix;

          if ( k in [1..EdburgMaxVariables-1] ) then begin

            for i:= 1 to k do
              for j:= 1 to k do

                if ( i >= j ) then begin
                  MeansDMatrix[i,j]:= MeansVector[i] - MeansVector[j];
                  MeansDMatrix[j,i]:= MeansDMatrix[i,j];      // Mirror val.
                end;  // if ( i >= j )

              result:= MeansDMatrix;          // Return ACovMatrix

          end; // if ( k )

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  SpearmansR
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function SpearmansR ( const X : TIntegerArray;
                              const Y : TIntegerArray;
                              const n : integer): double;

        var
          dSum         : extended;
          dNumerator   : extended;
          dDenominator : extended;
          i            : integer;
        begin
          dSum         := 0;
          result       := 0;

          try
            if ( n > 2 ) and ( n < EdburgGeneralFuncsMaxObservations ) then begin
              for i:= 0 to n-1 do
                dSum:= dSum + sqr(X[i]-Y[i]);

              dNumerator   := dSum * 6;
              dDenominator := Cube(n) - n;

              result:= 1 - ( dNumerator / dDenominator );
            end; // if ( n )

          except
            ShowMessage('Error calculating Spearman''s r');
          end; // try - except - end

        end;

        (*****************************************************************************
        * Author   :  F. Edberg
        * Date     :  2/26/04
        * Purpose  :  SpearmansRStdError
        * Result   :
        * History  :  New - stubbed in
        ******************************************************************************)
        function SpearmansRStdError ( const X : TIntegerArray;
                                      const Y : TIntegerArray;
                                      const n : integer): extended;

        var
          dRStdError : extended;
          dR         : extended;
        begin
          result    := 0;
          dRStdError := 0;

          try
            if ( n > 2 ) and ( n < EdburgGeneralFuncsMaxObservations ) then begin
              dR:= SpearmansR(X,Y,n);
              dRStdError:= sqrt( ( 1 - sqr(dR) ) / ( n-2 ) );
            end; // if ()

            result:= dRStdError;

          except
            ShowMessage('Error calculating Spearman''s R std. error');
          end; // try - except - end

        end;

{end of what what in GeneralFuncs}


(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/16/02
* Purpose  :  Create
* Result   :  n/a
* History  :  New
******************************************************************************)
constructor TMVClusterClientDataSet.Create (AOwner: TComponent);
begin
  inherited;

  New(PRegressData);
  MVPrepare;
  MinVect    := InitVector(EdburgMaxVariables,0);
  MaxVect    := InitVector(EdburgMaxVariables,0);
  MeanVect   := InitVector(EdburgMaxVariables,0);
end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/16/02
* Purpose  :  Destroy
* Result   :  n/a
* History  :  New
******************************************************************************)
destructor TMVClusterClientDataSet.Destroy;
begin
  Dispose (PRegressData);
  inherited Destroy;
end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  9/16/02
* Purpose  :  Property setter for initialization option
* Result   :  n/a
* History  :  New
******************************************************************************)
procedure TMVClusterClientDataSet.SetInitOption(Value: TInitializationOption);
begin
  if FInitOption <> Value then FInitOption:= Value;
end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  MVPrepare
* Result   :  n/a
* History  :  New
******************************************************************************)
procedure TMVClusterClientDataSet.MVPrepare;
var
  i: integer;
  j: byte;
begin
  for i:= 0 to EdburgGeneralFuncsMaxObservations-1 do PRegressData^[0,i]:= 1;
  for i:= 0 to EdburgGeneralFuncsMaxObservations-1 do
    for j:= 1 to EdburgMaxVariables-1 do  PRegressData^[j,i]:= 0;

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  FieldTypesOK
* Result   :  boolean
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.FieldTypesOK ( const sXFields : array of AnsiString;
                                                const k        : byte): boolean;
var
  i:byte;
begin
  Result:= TRUE;

  with Self do begin
     for i:= 1 to k do begin
        if ( sXFields[i-1] = '' ) or
           ( FieldByName(sXFields[i-1]).FieldName = '' ) or
           NOT ( FieldByName(sXFields[i-1]).DataType in
           [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftLargeint]) then begin
              Result:= FALSE;
              EXIT;
        end; // if ( sXFields[i-1] )
    end; // if ( Self.XField)
  end; // with

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  GetCurrentData
* Result   :  integer
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.GetCurrentData
                                        (   const sXFields : array of AnsiString;
                                            const k        : byte): integer;
var
   i: integer;
   j: byte;
begin

  if ( FieldTypesOK(sXFields,k) ) then begin
     with Self do begin
        try
        // >>> Check to see that the field names are non-blank <<<
        DisableControls;
        First;

        // Loop through result set to grab values from fields specified
        i:= 0;
        While not Eof do begin
           for j:= 1 to k do
              PRegressData^[j,i]:= FieldByName(sXFields[j-1]).AsFloat;
           Next;
           inc(i);
        end; // while not Self.Eof

        finally
           EnableControls;
           if ( fResetAfterCalcs ) then First;
        end; // try - finally - end
     end; // with Self

     Result:= i;
  end // if ( FIeldTypesOK()

  else
    Raise Exception.Create('An input field was not numeric or not specified.');
end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  ComputeInitialClusterCentersMinMax
* Result   :  TClusterMeansArray
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.ComputeInitialClusterCentersMinMax (
                                            const aMeanVector    : XVector;
                                            const aMinVector     : XVector;
                                            const aMaxVector     : XVector;
                                            const k              : byte;
                                            const NumberClusters : byte): TClusterMeansArray;

var
  dIncrementSize : double;
  i,j : integer;
  f: textfile;
begin

  try

    for j:= 1 to k do begin
      dIncrementSize      := ( aMaxVector[j] - aMinVector[j] ) / NumberClusters;
      ClusterMeans[1,j]   := aMinVector[j] + ( ONE_HALF * dIncrementSize );

      for i:= 2 to NumberClusters do
        ClusterMeans[i,j]:= ClusterMeans[i-1,j] + dIncrementSize;

    end; // for j:= 1 to k

    Assignfile (f,OutputFileName);
    System.Append (f);
    Writeln(f);
    Writeln(f,'Initial Cluster Centers (min/max):');

    for i:= 1 to NumberClusters do begin
      Write (f,'Cluster#:',i:3);
      for j:= 1 to k do if ClusterMeans[i,j] > 0 then Write(f,'  ',ClusterMeans[i,j]:8:2);
      Writeln(f);
    end; // for i:= 1 to NumberClusters

    Writeln(f);
    Closefile(f);
    Result:= ClusterMeans;

  except
    Raise Exception.Create('ClusterInitialError');
  end; // try - except - end

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  ComputeInitialClusterCentersStdDev
* Result   :  TClusterMeansArray
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.ComputeInitialClusterCentersStdDev (
                                            const aMeanVector    : XVector;
                                            const aVarianceVector: XVector;
                                            const k              : byte;
                                            const NumberClusters : byte): TClusterMeansArray;

var
  f    : textfile;
  i,j  : integer;
  dTmp : double;
begin

  if ( NumberClusters > 0 ) then begin
     try
        for j:= 1 to k do begin

           // First, or 'initial' cluster
           dTmp:= ONE_HALF * sqrt(aVarianceVector[j]);
           ClusterMeans[1,j] := aMeanVector[j] + dTmp ;
           ClusterMeans[2,j] := aMeanVector[j] - dTmp;

           // Compute the other cluster means by calculating 'away' from
           // initial or starting mean
           for i:= 3 to NumberClusters do begin
              if odd (i) then
                 ClusterMeans[i,j]:= ClusterMeans[i-2,j] + sqrt(aVarianceVector[j])
              else
                 ClusterMeans[i,j]:= ClusterMeans[i-2,j] - sqrt(aVarianceVector[j]);
           end; // for i:= 3 to
        end; // for j:= 1 to k

        Assignfile (f,OutputFileName);
        System.Append (f);
        Writeln(f);
        Writeln(f,'Initial Cluster Centers (std dev):');

        for i:= 1 to NumberClusters do begin
           Write (f,'Cluster#: ',i:3);

           for j:= 1 to k do if ClusterMeans[i,j] > 0 then

              Write (f,'  ',ClusterMeans[i,j]:8:2);
           Writeln(f);
        end; // for i:= 1 to NumberClusters
        Writeln(f);
        Closefile(f);

        Result:= ClusterMeans;

     except
        Raise Exception.Create('ClusterInitialError');
     end; // try - except - end

  end; // if ( NumberClusters > 1 )

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  ComputeInitialClusterCentersMinMax
* Result   :  TClusterMeansArray
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.ComputeInitialClusterCentersRandom (
                                            const aMeanVector    : XVector;
                                            const aMinVector     : XVector;
                                            const aMaxVector     : XVector;
                                            const k              : byte;
                                            const NumberClusters : byte): TClusterMeansArray;

var
  f: textfile;
  i,j: integer;
begin
  randomize;

  try

    for i:= 1 to NumberClusters do begin
        for j:= 1 to k do
           ClusterMeans[i,j]:= Random(Round(aMaxVector[j])+1);
    end; // for j:= 1 to k

    Assignfile (f,OutputFileName);
    System.Append (f);
    Writeln(f);
    Writeln(f,'Initial Cluster Centers (random):');
    for i:= 1 to NumberClusters do begin
      Write (f,'Cluster#: ',i:3);

      for j:= 1 to k do {if ClusterMeans[i,j] > 0 then} Write (f,'  ',ClusterMeans[i,j]:8:2);
      Writeln(f);
    end; // for i:= 1 to NumberClusters
    Writeln(f);
    Closefile(f);

    Result:= ClusterMeans;

  except
    Raise Exception.Create('ClusterInitialError');
  end; // try - except - end

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  AssignDataToCluster
* Result   :  TVectorClassLabels
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.AssignDataToCluster (  const PRegressData   : PTRegressDataArray;
                                                        const Clusters       : TClusterMeansArray;
                                                        const NumberClusters : byte;
                                                        const k              : byte;
                                                        const n              : integer)  :  TVectorClassLabels;
var
  i,j,ObsCnt : integer;
  aSum       : extended;
  aMin       : extended;
  BestClass  : integer;
begin
  for ObsCnt:= 1 to n do begin
     for j := 1 to NumberClusters do begin
        DistArr[j] := 0;
        aSum := 0;
        for i:= 1 to k do
          aSum := aSum + sqr(PRegressData^[i,ObsCnt-1] - Clusters[j,i]);
        DistArr[j]:= aSum;
     end;

     aMin:= DistArr[1];
     BestClass:= 1;
     for j:= 2 to NumberClusters do begin
        if ( DistArr[j] < aMin ) then begin
           aMin := DistArr[j];
           BestClass:= j;
        end;
     end;
     Result[ObsCnt]:= BestClass;
  end;
end;


function TMVClusterClientDataSet.AssignDataToOrderedClusters (  const PRegressData   : PTRegressDataArray;
                                                        const Clusters       : TClusterMeansArray;
                                                        const NumberClusters : byte;
                                                        const k              : byte;
                                                        const n              : integer)  :  TVectorClassLabels;
//added by PLG, October 2023
var
  i,j{,ObsCnt} : integer;
  aSum       : extended;
  //aMin       : extended;
  //BestClass  : integer;
begin
     for j := 1 to NumberClusters do begin
        DistArr[j] := 0;
        aSum := 0;
        for i:= 1 to k do
          aSum := aSum + sqr(Clusters[j,i]);
        DistArr[j]:= aSum;
        WriteLineToDebugFile(IntToStr(j) + ',' + RealToString(DistArr[j],-12,-4));
     end;
(*
  for ObsCnt:= 1 to n do begin
     aMin:= DistArr[1];
     BestClass:= 1;
     for j:= 2 to NumberClusters do begin
        if ( DistArr[j] < aMin ) then begin
           aMin := DistArr[j];
           BestClass:= j;
        end;
     end;
     Result[ObsCnt]:= BestClass;
  end;
*)
end;



(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  CalcClusterSSE
* Result   :  extended
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.CalcClusterSSE ( const PRegressData : PTRegressDataArray;
                                                  const ClsLabs      : TVectorClassLabels;
                                                  const NewCenters   : TClusterMeansArray;
                                                  const k            : byte;
                                                  const n            : integer): extended;

var
  i        : integer;
  j        : byte;
  TmpLabel : integer;
begin
  Result:= 0;
  // Find overall 'fit' of cluster scheme at this iteration. SSE is the overall error
  for i:= 1 to n do begin
    TmpLabel:= ClsLabs[i];
    for j:= 1 to k do
      Result:= Result + sqr(NewCenters[TmpLabel,j] - PRegressData^[j,i-1]);
      //Result:= Result + abs(NewCenters[TmpLabel,j] - PRegressData^[j,i-1]);
  end; // for i:= 1 to n do

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  ComputeClusterMeans
* Result   :  TClusterMeansArray
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.ComputeClusterMeans (when : shortString;
                                                        const PRegressData   : PTRegressDataArray;
                                                        const ClsLabls       : TVectorClassLabels;
                                                        const NumberClusters : byte;
                                                        const k              : byte;
                                                        const n              : integer): TClusterMeansArray;
var
  f: textfile;
  i,j: integer;
  ObCount: integer;
  TmpLabel: word;
begin

  try

    for i:= 1 to NumberClusters do begin
      for j:= 1 to k do begin
        Result[i,j]:= 0;
      end; // for j
      ClsCounts[i]:= 0;
    end; // for i

    for ObCount:= 1 to n do begin
      TmpLabel:= ClsLabls[ObCount];
      for j:= 1 to k do
       Result[TmpLabel,j] := Result[TmpLabel,j] + PRegressData^[j,ObCount-1];
      inc(ClsCounts[TmpLabel]);
    end; // for ObCount:= 1 to n

    for i:= 1 to NumberClusters do begin
      if (ClsCounts[i] > 0) then begin// if no pts are assigned to Class[i], then skip it
        for j:= 1 to k do
          Result[i,j]:= Result[i,j] * ( 1 / ClsCounts[i] ); // find mean
      end // if ( ClsCounts[i] > 0 ) then
    end; // for i:= 1 to NumberClusters do

    Assignfile (f,OutputFileName);
    System.Append (f);
    Writeln(f);
    Writeln(f,When + ' New Cluster Means:');
    for i:= 1 to NumberClusters do begin
        if Result[i,1] > 0 then begin
           Write (f,'Cluster#: ',i:3,' N:',ClsCounts[i]:5);
           if ( clsCounts[i] > 0 ) then begin
              for j := 1 to k do
                 Write (f,'  ',Result[i,j]:6:2);
           end; // if ( ClsCounts[i]
        end;
      Writeln(f);
    end; // for i:= 1 to NumberClusters
    Writeln(f);
    Closefile(f);

  except
    Raise Exception.Create('ClusterMeansError');
  end; // try - except - end

  //result:= NewClusterMeans;
end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  8/26/02
* Purpose  :  Check two input arrays and calc the degree the two array contents match
* Result   :  double (value between 0..100)
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.CorrespondenceFactor( const TmpClsLabs : TVectorClassLabels;
                                                       const ClsLabs    : TVectorClassLabels): double;

var
  i,iTmpCounter: integer;
begin
  iTmpCounter := 0;

  for i:= 1 to NClusters do begin
     if ( TmpClsLabs[i] = ClsLabs[i] ) then
        inc(iTmpCounter);
  end; // for j:= 1 to

  Result:= ( iTmpCounter / NClusters ) * 100;
end;


(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/26/02
* Purpose  :  KMeansClusteringMinMax
* Result   :  extended
* History  :  New
******************************************************************************)
function TMVClusterClientDataSet.KMeansClustering ( var ClusterSummary : tStringList;
                                                    const sXFields : array of AnsiString;
                                                    const k        : byte;
                                                    const sHTMLFileName : AnsiString): extended;
var
  //ClusterCount,
  StartCluster,
  i,j,l,iEndIter : integer;
  f              : textfile;
  cf             : double;
  slHTMLStrings  : TStringList;
  aLine : shortstring;
begin
   {$IfDef LogOps} WriteLineToDebugFile('TMVClusterClientDataSet.KMeansClustering in'); {$EndIf}

  Result      := -1;
  iEndIter    := -1;
  cf          := -1;
  n           := GetCurrentData(sXFields,k);

 {$IfDef LogOps} WriteLineToDebugFile('n=' + n.ToString); {$EndIf}

  for i := 1 to MAXITERATIONS do SSEArray[i]:= 0;

  slHTMLStrings:= TStringList.Create;

  try
    try
     if ( FOutputFile = '' ) then FOutputFile:= sDEFAULT_FILE_NAME;

     Assignfile (f,OutputFileName);
     Rewrite    (f);
     Writeln    (f,'Cluster Results:');
     Writeln    (f);
     Closefile  (f);

     MinVect    := MinVector(PRegressData,k,n);
     MaxVect    := MaxVector(PRegressData,k,n);
     MeanVect   := MeanVector(PRegressData,k,n);
     VarVect    := VarianceVector(PRegressData,k,n);

     StartCluster := NClusters;

     if ( fInitOption = ioMinMax ) then ClsCenters := ComputeInitialClusterCentersMinMax(MeanVect,MinVect,MaxVect,k,NClusters)
     else if ( fInitOption = ioStdDev ) then ClsCenters := ComputeInitialClusterCentersStdDev(MeanVect,VarVect,k,NClusters)
     else if ( fInitOption = ioRandom) then ClsCenters := ComputeInitialClusterCentersRandom(MeanVect,MinVect,MaxVect,k,NClusters);

    {$IfDef LogOps} WriteLineToDebugFile('Cluster centers computed'); {$EndIf}

     ClsLabs    := AssignDataToCluster(PRegressData,ClsCenters,NClusters,k,n);
     NewCenters := ComputeClusterMeans('Initial assignment',PRegressData,ClsLabs,NClusters,k,n);

    {$IfDef LogOps} WriteLineToDebugFile('Assigned to clusters and means computed'); {$EndIf}

     for i := 1 to NIterations do begin
       {$IfDef LogOpsFull} WriteLineToDebugFile('Start iteration ' + IntToStr(i)); {$EndIf}
        for j := 1 to NClusters do TmpClsLabs[j]:= ClsLabs[j];

        ClsLabs     := AssignDataToCluster(PRegressData,NewCenters,NClusters,k,n);
        NewCenters  := ComputeClusterMeans('Iteration ' + IntToStr(i),PRegressData,ClsLabs,NClusters,k,n);
        {$IfDef LogOpsFull} WriteLineToDebugFile('Assigned to clusters and means computed'); {$EndIf}

        SSEArray[i] := CalcClusterSSE (PRegressData,ClsLabs,NewCenters,k,n);
        {$IfDef LogOpsFull} WriteLineToDebugFile('CalcClusterSSE computed'); {$EndIf}
        Result      := SSEArray[i];
        cf          := CorrespondenceFactor(TmpClsLabs,ClsLabs);

        {$IfDef LogOps}
           ClusterCount := 0;
           for j:= 1 to NClusters do if (ClsCounts[j] > 0) then inc(ClusterCount);

           WriteLineToDebugFile('iteration=' + IntToStr(i) + '  clusters=' + IntToStr(ClusterCount) + '  SSE=' + RealToString(SSEArray[i],-15,2) +  ' cf= ' + RealToString(cf,-18,-2) +
              ' ConvergenceThreshold=' + RealToString(ConvergenceThreshold,-18,-2)  );
        {$EndIf}

        iEndIter:= i;
        //  10/3/2023  It is unclear where the ConvergenceThreshold is set at 500, and how a reasonable value would be picked , so this probably never applies
        if ( cf > ConvergenceThreshold ) then begin
           {$IfDef LogOps} WriteLineToDebugFile('Break; exceed threshhold= ' + RealToString(ConvergenceThreshold,-18,-2)); {$EndIf}
           break;
        end; // if ( cf > )
     end; // for i:= 1 t NIterations
     ClusterMeans := NewCenters;

     {$IfDef LogResults}
        for j := 1 to NClusters do if (ClsCounts[j] > 0) then begin
           aline := 'Cluster: ' + IntToStr(j) + '  n=' + IntToStr(ClsCounts[j]) + '  Variable means';
           for i := 1 to k do aline := aline + RealToString(NewCenters[j,i],12,6);
           WriteLineToDebugFile(aline);
        end;
     {$EndIf}

    for j := 1 to NClusters do if (ClsCounts[j] > 0) then begin
       aline := IntToStr(StartCluster) + ',' + IntToStr(iEndIter) + ',' + IntToStr(j) + ',' + IntToStr(ClsCounts[j]) + ',' + RealToString(SSEArray[iEndIter],-15,4) + ',' + IntToStr(WinGraphColors[j mod 15]);
       for i := 1 to k do aline := aline + ',' + RealToString(NewCenters[j,i],12,6);
       ClusterSummary.Add(aline);
    end;

     System.Append (f);
     Writeln(f);
     Writeln(f,'End Iteration: ',iEndIter:2);
     Writeln(f);
     Writeln(f,'Convergence Factor : ',cf:5:2);
     Writeln(f);
     Writeln(f,'Sum Sqrs of Error: ');
     Writeln(f);
     for i:= 1 to NIterations do Writeln(f,'  SSE : (iteration',i:3,') ',SSEArray[i]:15:2);
     Writeln(f);
     Closefile(f);

     {$IfDef LogOps} WriteLineToDebugFile('TMVClusterClientDataSet.KMeansClustering done work'); {$EndIf}

     if fAccumulateClusterStats then begin
        GatherFinalStats(k);
        System.Append (f);
        Writeln(f,'===============================================================================');
        for j := 1 to NClusters do begin
           Writeln(f);
           Writeln(f,'Cluster: ',j);
           Writeln(f,'                         Means         StdDev.      Variances');
           for i:= 1 to k do
              Writeln(f,sXFields[i-1]:15,' (',i,')',NewCenters[j,i]:12:4,', ',sqrt(ClusterVariances[j,i]):12:4,', ', ClusterVariances[j,i]:12:4);
           Writeln(f);
        end;
        Writeln(f,'===============================================================================');

        Writeln(f);
        Writeln(f);
        Writeln(f,'Cluster Covariances:');
        Writeln(f);
        for i := 1 to NClusters do begin
           Writeln(f);
           Writeln(f,'Cluster: ',i);
           Writeln(f);
           for j:= 1 to k do begin
              for l:= 1 to k do
                Write (f,ClusterCovariances[i,j,l]:12:4,' ');
              Writeln(f);
           end;
        end;
        Writeln(f);
        Writeln(f,'===============================================================================');

        Writeln(f);
        Writeln(f,'Pairwise Divergence:');
        Writeln(f);
        for i:= NClusters downto 1 do begin
           for j:= NClusters downto 1 do begin
              if ( i > j ) then begin
                  Writeln(f,'Clusters: [',i,',',j,'] : ',DivergencesArray[i,j]:12:3);
              end;
           end;
        end;

        Writeln(f);
        Writeln(f,'Total Avg Divergence:   Sum           Mean');
        Writeln(f);
        for i := 1 to NClusters do
           Writeln(f,'Clusters: [',i,'] : ',ClusterTotalSumDistances[i]:12:3,', ',ClusterTotalSumDistances[i] / ( NClusters - 1):12:3);
        Writeln(f,'===============================================================================');
        CloseFile(f);
     end; // if ( fAccumulateClusterStats )

     if ( sHTMLFileName <> '' ) then begin
        with slHTMLStrings do begin
           LoadFromFile(OutputFileName);
           for i:= 0 to Count-1 do
              Strings[i]:= '<FONT FACE="Verdana" SIZE="1">' + Strings[i] + '</FONT><BR>';
           SaveToFile(sHTMLFileName);
        end; // with slHTMLStrings
     end; // if ( sHTMLFileName <> '')

     if ( ShowDoneMessage ) then
        ShowMessage('Clustering completed.');

     //AssignDataToOrderedClusters(PRegressData,ClsCenters,NClusters,k,n);

     except On E: Exception do
        Raise Exception.Create('KMeansClusterError' + E.Message);
     end; // try - except - end
   finally
     slHTMLStrings.Free;
   end; // try - finally - end
   {$IfDef LogOps} WriteLineToDebugFile('TMVClusterClientDataSet.KMeansClustering out'); {$EndIf}
end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/24/02
* Purpose  :  After clustering is done, makes a final pass to compute
*                variances (covariances - later on) for each cluster.
* Result   :  n/a
* History  :  New - 11/24/02
******************************************************************************)
procedure TMVClusterClientDataSet.GatherFinalStats ( const k : byte );

var
  iMaxClusterID: integer;
  iClusterCount:integer;
  aSum,aSum2: extended;
  iRecordCnt: integer;
  iVarsCnt: integer;
  iTargetValue:integer;
  a,b,i,j,l:integer;
  numer,numer2: extended;
  xvMean1: XVector;
  xvMean2: XVector;
begin
  iMaxClusterID  := 0;
  iTargetValue   := 0;

  for i:= 1 to MaxClusters do
     ClusterCovariances[i]:= InitMatrix(0,EdburgMaxVariables);

  for i:= 1 to MaxClusters do
     for j:= 1 to MaxClusters do
        DivergencesArray[i,j]:= 0;

  for i:= 0 to EdburgGeneralFuncsMaxObservations-1 do begin
     XTmpArray[i]:= 0;
     YTmpArray[i]:= 0;
  end; // for i:= 0

  for i:= 1 to RecordCount do begin
     if ( ClsLabs[i] > iMaxClusterID ) then
        iMaxClusterID:= ClsLabs[i];
  end; // for i:= 0 to


  repeat
     inc(iTargetValue);

     for iVarsCnt := 1 to k do begin
        aSum:= 0;
        aSum2:= 0;

        if ( ClsCounts[ iTargetValue ]  > 1 ) then begin
           for iRecordCnt:= 0 to RecordCount-1 do begin
              if ( iTargetValue = ClsLabs[ iRecordCnt+1 ] ) then begin
                 aSum  := aSum + PRegressData^[iVarsCnt,iRecordCnt] ;
                 aSum2 := aSum2 + sqr(PRegressData^[iVarsCnt,iRecordCnt]);
              end; // if ( iTargetValue = xxx)
           end; // for iVarsCnt

           numer    := aSum2 - ( ( aSum * aSum ) / n );
           numer2   := numer / ( n-1 );
           ClusterVariances[iTargetValue,iVarsCnt]:= numer2;
        end; // ( ClsCounts[] )
     end; // for iVarsCnt

  until ( iTargetValue >= iMaxClusterID ) ;

  // Calculate Covariance matrix for each cluster using only the
  // observations (records) which belong to each cluster from PRegressData
  // - To be added -fje 11/28/02
  for iClusterCount:= 1 to iTargetValue do begin
     for i:= 1 to k do begin
        for j:= 1 to k do begin
           if ( i >= j ) then begin
              a:= 0;
              for b:= 0 to RecordCount-1 do begin
                 if ( ClsLabs[b+1] = iClusterCount ) then begin
                    XTmpArray[a]:= PRegressData^[i,b];
                    YTmpArray[a]:= PRegressData^[j,b];
                    inc(a);
                 end; // for b:= 0
              end; // for a:= 0

              ClusterCovariances[iClusterCount,i,j]:=
                 BVRegressionShort(XTmpArray,YTmpArray,RecordCount) *
                 sqrt(ClusterVariances[iClusterCount,i]) *
                 sqrt(ClusterVariances[iClusterCount,j]);
              ClusterCovariances[iClusterCount,j,i]:=
                 ClusterCovariances[iClusterCount,i,j];
           end; // if ( i > j )
        end; // for j:= 1
     end; // for i:= 1
  end; // for ClusterCount

  for i:= NClusters downto 1 do begin
     for j:= NClusters downto 1 do begin
        if ( i > j ) then begin
           for l:= 1 to k do begin
              xvMean1[l]:= NewCenters[i,l];
              xvMean2[l]:= NewCenters[j,l];
           end; // for l:= 1 to k
           DivergencesArray[i,j]:= GetDivergence(xvMean1,xvMean2,ClusterCovariances[i],ClusterCovariances[j],k,2000);
           DivergencesArray[j,i]:= DivergencesArray[i,j];
        end; // if ( i > j )
     end; // for j
  end; // for i

  iTargetValue:= 1;
  repeat
     for i:= 1 to NClusters do begin
        for j:= 1 to NClusters do begin
           if ( i > j ) then begin
              if ( i = iTargetValue ) xor ( j = iTargetValue ) then begin
                 ClusterTotalSumDistances[iTargetValue] :=
                    ClusterTotalSumDistances[iTargetValue] + DivergencesArray[i,j];
              end; // if ( i = )
           end;
        end; // for j:=
     end; // for i:=
     inc(iTargetValue);
  until ( iTargetValue > NClusters ) ;

end;

(*****************************************************************************
* Author   :  F. Edberg
* Date     :  11/02/03
* Purpose  :  SaveToHTML
* Result   :  n/a
* History  :  New
******************************************************************************)
procedure TMVClusterClientDataSet.SaveToHTML(const sFileName : AnsiString;
                                             const sHeader   : AnsiString);

const
  DefaultHeaderString = 'ISODATA Clustering Results';
var
  sStrList: TStringList;
  sStrTmp: TStringList;
begin
   sStrList:= TStringList.Create;
   sStrTmp:= TStringList.Create;

   with sStrList do try
       sStrTmp.LoadFromFile(sFileName);
       Add(StartHTMLString + '<BODY>');
       Add('<BR><HR><BR><BR>');

       AddStrings(sStrTmp);

       Add('<BR><A HREF="#Top">(Top)</A><BR>');
       Add('Generated: ' + DateTimeToStr(Now));
       Add('</BODY>' + EndHTMLString);
       SaveToFile(sFileName);
     finally
        sStrList.Free;
        sStrTmp.Free;
     end; // with try - finally - end
end;


initialization
finalization
end.
