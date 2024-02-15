unit OceanCal;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


interface

uses
   System.classes, System.SysUtils,VCl.Forms,VCL.Controls,
   PETMAR,Petmar_Types,PETMath,PetDButils;


type
   SoundVelocityAlgorithm = (Medwin,Leroy,Simple);


function CalSigmaT(T,S : float64) : float64;
function SoundVelocity(T,S,Z : float64) : float64;
function CalSigmaTheta(Temp,Sal,Depth : float64) : float64;

procedure ImportCTDFile;


implementation


uses
   Nevadia_main,DEMmapf,DEMdefs;

var
   SoundVelAlg : SoundVelocityAlgorithm;



procedure ImportCTDFile;
type
   tProfInfo = record
      No_seg : int32;
      Prof_type : string[4];
      Deep_Depth : int32;
   end;
var
   fName : PathStr;
   DataFileInMemory,Stations,SubFile,Points  : tStringList;
   aLine,bLine : AnsiString;
   TempLine,SalLine : array[1..5] of ANSIString;
   TStr : shortstring;
   MinSV,sofar_m,
   LowDepth,HighDepth,
   LowLat,HighLat,LowLong,HighLong,
   aLat,aLong : float64;
   ExtractSubset,IncludeXBT,StationsDB,DepthLimits,
   InBox,CreatePoints : boolean;
   Year,Month,Day,i,j,k,Start,
   OnLine,No_Depths,OnDepth,OnDepth2,Mismatches,
   No_prof : integer;
   DTG,Stn_Num,CruiseID : ShortString;
   ProfInfo : array[1..30] of tProfInfo;
   sv,Depth,Temperature,Salinity : array[1..6000] of float64;
begin
   StopSplashing;
   fName := '';
   Petmar.GetFileFromDirectory('CTD data, MEDS format','*.*', fName);
   {$IfDef RecordOceanography} WriteLineToDebugFile('Twmdem.ImportCTDfile1Click inf=' + fName); {$EndIf}
   CreatePoints := AnswerIsYes('Create points DB');
   StationsDB := AnswerIsYes('Create stations DB');
   ExtractSubset := AnswerIsYes('Extract MEDS file');
   IncludeXBT := AnswerIsYes('Include XBT stations');
   DepthLimits := AnswerIsYes('Depth limits');

   if ExtractSubset then begin
      LowLat := -80;
      HighLat := 80;
      LowLong := -180;
      HighLong := 180;
      ReadDefault('Low lat',LowLat);
      ReadDefault('High lat',HighLat);
      ReadDefault('Low long',LowLong);
      ReadDefault('High long',HighLong);
   end;

   if DepthLimits then begin
      LowDepth := 495;
      HighDepth := 505;
      ReadDefault('Upper depth limit',LowDepth);
      ReadDefault('Lower depth limit',HighDepth);
   end
   else begin
      LowDepth := -1;
      HighDepth := 9999;
   end;

   if CreatePoints then begin
      Points := tStringList.Create;
      Points.Add('LAT,LONG,DEPTH,TEMP_C,SALINITY,SOUND_VEL');
   end;

   ShowHourglassCursor;
   if StationsDB then begin
      Stations := tStringList.Create;
      Stations.Add('DTG,CRUISEID,STATION,LAT,LONG,DEPTH,MAX_TEMP,MIN_TEMP,MAX_SAL,MIN_SAL,SOFAR_M,INSTRUMENT');
   end;

   DataFileInMemory := tStringList.Create;
   DataFileInMemory.LoadFromFile(fName);

   if ExtractSubset then begin
      Subfile := tStringList.Create;
   end;

   OnLine := -1;
   MisMatches := 0;

   while online < pred(DataFileInMemory.Count) do begin
      inc(Online);
      if (Online mod 500 = 0) then wmdem.SetPanelText(0,IntToStr(Online));

      aLine := DataFileInMemory.Strings[OnLine];
      aLat := StrToFloat(ptTrim(Copy(aline,63,8)));
      aLong := -StrToFloat(ptTrim(Copy(aline,71,9)));
      Year := StrToInt(Copy(aline,27,4));
      Month := StrToInt(Copy(aline,31,2));
      Day := StrToInt(Copy(aline,33,2));
      No_prof := StrToInt(Copy(aline,122,2));

      Stn_Num := copy(aline,41,12);
      DTG := copy(aline,27,12);
      CruiseID := copy(aline,17,10);

      if ExtractSubset then begin
         InBox := (aLat > LowLat) and (aLat < HighLat) and (aLong > LowLong) and (aLong < HighLong);
      end
      else InBox := true;

      for I := 1 to No_Prof do  begin
         Start := 130 + pred(i) * 14;
         ProfInfo[i].No_seg := StrToInt(ptTrim(Copy(aline,Start+1,2)));
         ProfInfo[i].Prof_Type := Copy(aline,Start+3,4);
      end;

      OnDepth := 0;
      OnDepth2 := 0;
      for i := 1 to No_Prof do begin
         for j := 1 to ProfInfo[i].No_seg do begin
            //Get next line from memory
            inc(OnLine);
            bLine := DataFileInMemory.Strings[OnLine];
            //save line for export later
            if ProfInfo[i].Prof_Type = 'TEMP' then TempLine[j] := bline
            else SalLine[j] := bline;
            //process the line
            No_Depths := StrToInt(ptTrim(Copy(bline,59,4)));
            for k := 1 to No_Depths do begin
               if ProfInfo[i].Prof_Type = 'TEMP' then begin
                  inc(OnDepth);
                  Depth[OnDepth] := StrToFloat(ptTrim(Copy(bline,64 + pred(k) * 17,6)));
                  Temperature[OnDepth] := StrToFloat(ptTrim(Copy(bline,64 + 8 + pred(k) * 17,9)));
               end
               else if ProfInfo[i].Prof_Type = 'PSAL' then begin
                  inc(OnDepth2);
                  Salinity[OnDepth2] := StrToFloat(ptTrim(Copy(bline,64 + 8 + pred(k) * 17,9)))
               end;
            end;
         end;
      end;

      if (OnDepth = OnDepth2) then begin
         if ExtractSubset and (OnDepth > 10) and InBox then begin
            SubFile.Add(aline);
            for i := 1 to ProfInfo[1].No_seg do SubFile.Add(TempLine[i]);
            for i := 1 to ProfInfo[1].No_seg do SubFile.Add(SalLine[i]);
         end;

         MinSV := 9999;
         if (No_Prof = 2) then begin
            for i := 1 to OnDepth do begin
               sv[i] := SoundVelocity(Temperature[i],Salinity[i],Depth[i]);
               if sv[i] < MinSV then begin
                  MinSv := sv[i];
                  Sofar_m := Depth[i];
               end;
            end;

            if InBox and CreatePoints then begin
               for i := 1 to OnDepth do begin
                  if (Salinity[i] > 0.1) and (Depth[i] > LowDepth) and (Depth[i] < HighDepth) then begin
                     {$IfDef RecordFullOceanography} writeLineToDebugFile(aLine); {$EndIf}
                     Points.Add(RealToString(aLat,-12,-4) + ',' +
                                RealToString(aLong,-12,-2) + ',' +
                                RealToString(Depth[i],-12,-2) + ',' +
                                RealToString(Temperature[i],-12,-3) + ',' +
                                RealToString(Salinity[i],-12,-4) + ',' +
                                RealToString(sv[i],-12,-4));
                  end;
               end;
            end;
         end
         else begin
            TStr := '  ,  ,  ,XBT';
         end;

         if StationsDB and ((No_Prof = 2) or IncludeXBT) then begin
            aLine := DTG + ',' + CruiseID + ',' + Stn_Num + ',' + RealToString(aLat,-12,-3) + ',' + RealToString(aLong,-12,-3) + ',' + RealToString(Depth[OnDepth],-12,-3) + ',' +
                RealToString(PetMath.MaxInArray(OnDepth,Temperature),-12,-2) + ',' +
                RealToString(PetMath.MinInArray(OnDepth,Temperature),-12,-2) + ',' +  TStr;
            Stations.Add(aLine);
         end;
      end
      else inc(MisMatches);

   end;

   if ExtractSubset then begin
      SubFile.SaveToFile(ExtractFilePath(fName) + ExtractFileNameNoExt(fName ) + '_sub');
   end;

   DataFileInMemory.Destroy;
   wmdem.SetPanelText(0,'');

   wmdem.VectorMapButtonClick(Nil);
   if StationsDB then begin
      {$IfDef RecordOceanography} WriteLineToDebugFile('File had stations=' + IntToStr(Stations.Count)); {$EndIf}
      fName := ChangeFileExt(fName,'.csv');
      VectorMap[LastVectorMap].StringListToLoadedDatabase(Stations,fName);
   end;

   if CreatePoints then begin
      ShowHourglassCursor;
      Points.SaveToFile('c:\temp\pts.csv');
      fName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName ) + '_points.csv';
      VectorMap[LastVectorMap].StringListToLoadedDatabase(Points,fName);
   end;
end;






function CalSigmaTheta(Temp,Sal,Depth : float64) : float64;

      function Kappa(s,t,p : float64) : float64;
      var
         temp,p2,s3_2,t2,t3,t4 : float64;
      begin
         s3_2 := sqrt(s*s*s);
         t2   := sqr(t);
         t3   := t * t2;
         t4   := sqr(t2);
         p2   := sqr(p);
         temp := 19652.21 + 148.4206 * T         - 2.327105 * T2;
         temp := temp + 1.360477e-2 * t3         - 5.155288e-5 * T4;
         temp := temp + 3.239908 * p             + 1.43713e-3 * t * p;
         temp := temp + 1.16092e-4 * T2 * p      - 5.77905e-7 * t3 * p;
         temp := temp + 8.50935e-5 * p2          - 6.12293e-6 * t * p2;
         temp := temp + 5.2787e-8 * T2 * p2;
         temp := temp + 54.6746 * S              - 0.603459 * t * s;
         temp := temp + 1.09987e-2 * t2 * s      - 6.1670e-5 * t3 * s;
         temp := temp + 7.944e-2 * s3_2          + 1.6483e-2 * t * s3_2;
         temp := temp - 5.3009e-4 * t2 * s3_2    + 2.2838e-3 * p * s;
         temp := temp - 1.0981e-5 * t * p * s    - 1.6078e-6 * t2 * p * s;
         temp := temp + 1.91075e-4 * p * s3_2    - 9.9348e-7* P2 * s;
         temp := temp + 2.0816e-8 * t * P2 * S   + 9.1697e-10 * T2 * P2 * s;
         Kappa := temp;
      end;

begin
   Result := CalSigmaT(Temp,Sal);
   Result := 1 + 1000 * Result;
   Depth := 0.1 * Depth; {convert to bars}
   Result := Result / (1 - Depth / Kappa(Sal,Temp,Depth));
   Result := 0.001 * (Result - 1);
end;


function CalSigmaT(T,S : float64) : float64;
var
   T2,ssqrt,rho : float64;
begin
   T2 := sqr(T);
   Ssqrt := sqrt(S) * S;
   rho := 999.842594 + 6.793952e-2 * T;
   rho := rho - 9.095290e-3 * T2 + 1.001685e-4 * T * T2;
   rho := rho - 1.120083e-6 * sqr(T2) + 6.536322e-9 * T * sqr(T2);
   rho := rho + 8.24493e-1 * S -4.0899e-3 * T * S;
   rho := rho + 7.6438e-5 * T2 * S - 8.2467e-7 * T * T2 * S;
   rho := rho + 5.3875e-9 * sqr(T2) * S - 5.72466e-3 * Ssqrt;
   rho := rho + 1.0227e-4 * T * Ssqrt;
   rho := rho - 1.6546e-6 * T2 * Ssqrt;
   rho := rho + 4.8314e-4 * sqr(S);
   CalSigmaT := rho - 1000;
end;


function SoundVelocity(T,S,Z : float64) : float64;
begin
   case SoundVelAlg of
      Leroy :  {Leroy, J.Acoust.Soc.Am., 1969, 46:216}
         SoundVelocity := 1492.9 + 3.0 * (T-10) - 0.006 * sqr(T-10) - 0.04 * sqr(T-18) + 1.2 * (S-35) - 0.01 * (T-18) * (S-35) + (Z / 61.0);
      Medwin :  {Medwin, J.Acoust.Soc.Am., 1975, 58:1318}
          SoundVelocity := 1449.2 + 4.6 * T - 0.055 * sqr(T) + 0.00029 * sqr(T) * T + (1.34 - 0.01 * T) * (S - 35) + 0.016 * Z;
      Simple : {eq.5-9, p.73, Seawater: Its composition, properties, and behaviour}
          SoundVelocity := 1410.0 + 4.21 * T - 0.037 * sqr(T) + 1.14 * S + 0.018 * Z;
   end {case};
end;


begin
   SoundVelAlg := Medwin;
end.




