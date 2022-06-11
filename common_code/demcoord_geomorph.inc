{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 9/27/2015       }
{                                 }
{ include file for demcoord       }
{_________________________________}

{$IfDef ExComplexGeostats}
{$Else}

procedure tDEMDataSet.FractalBox(GridLimits: tGridLimits; var FracDim,r : float64; SkipDraw : boolean = false; CloseGraph : boolean = false);
var
   Results : tStringList;
   DoResults : boolean;

  
