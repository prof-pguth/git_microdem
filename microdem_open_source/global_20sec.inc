      function ReadTopo30Header(FName : PathStr; var GTopo30Header : tGTopo30Header) : boolean;
      var
         Header : tStringList;
         i      : integer;
         Value  : float64;
         TStr  : ShortString;


         function Parameter(i : integer; What : ShortString; var Value : float64) : boolean;
         var
            Err,l : integer;
         begin
             TStr := Header.Strings[pred(i)];
             l := Length(What);
             Result := Copy(TStr,1,l) = What;
             if Result then begin
                Delete(TStr,1,L);
                StripBlanks(TStr);
                Val(Tstr,Value,err);
             end;
         end;

      begin;
         Header := tStringList.Create;
         Header.LoadFromFile(FName);
         {$IfDef RecordReadDEM} WriteStringListToDebugFile(Header);  {$EndIf}
         with GTopo30Header do begin
            NumRows := 0;
            NumCols := 0;
            for i := 1 to Header.Count do begin
               if Parameter(i,'NROWS',Value) then NumRows := round(Value);
               if Parameter(i,'NCOLS',Value) then NumCols := round(Value);
               if Parameter(i,'ULXMAP',Value) then ULXMap := Value;
               if Parameter(i,'ULYMAP',Value) then ULYMap := Value;
               if Parameter(i,'XDIM',Value) then XDim := Value;
               if Parameter(i,'YDIM',Value) then YDim := Value;
               if Parameter(i,'NODATA',Value) then NoData := round(Value);
            end;
            Long2 := ULXMap + pred(NumCols) * XDim;
            Lat2 := ULYMap - pred(NumRows) * YDim;
            NeedSwap := true;
            Result := (NumRows > 0) and (NumCols > 0);
         end;
         Header.Free;
      end;


      function ReadGLOBEHeader(FName : PathStr; var GTopo30Header : tGTopo30Header) : boolean;
      var
         Header : tStringList;
         i      : integer;
         Value  : float64;
         TStr  : ShortString;

               function Parameter(i : integer; What : ShortString; var Value : float64) : boolean;
               var
                  Err : integer;
               begin
                   TStr := Header.Strings[pred(i)];
                   Result := UpperCase(Copy(TStr,1,Length(What))) = UpperCase(What);
                   if Result then begin
                      TStr := ptTrim(AfterSpecifiedCharacter(TStr,'='));
                      Val(Tstr,Value,err);
                   end;
               end;

      begin;
         Header := tStringList.Create;
         Header.LoadFromFile(FName);
         {$IfDef RecordReadDEMProblems} WriteStringListToDebugFile(Header); {$EndIf}
         with GTopo30Header do begin
            NumRows := 0;
            NumCols := 0;
            for i := 1 to Header.Count do begin
               if Parameter(i,'number_of_rows',Value) then NumRows := round(Value);
               if Parameter(i,'number_of_columns',Value) then NumCols := round(Value);
               if Parameter(i,'left_map_x',Value) then ULXMap := Value;
               if Parameter(i,'upper_map_y',Value) then ULYMap := Value;
               if Parameter(i,'grid_size(x)',Value) then XDim := Value;
               if Parameter(i,'grid_size(y)',Value) then YDim := Value;
               if Parameter(i,'grid_size',Value) then begin
                  YDim := Value;
                  XDim := Value;
               end;
               if Parameter(i,'elev_m_missing_flag',Value) then NoData := round(Value);
               if Parameter(i,'missing_flag',Value) then NoData := round(Value);
            end;
            Long2 := ULXMap + pred(NumCols) * XDim;
            Lat2 := ULYMap - pred(NumRows) * YDim;
            NeedSwap := false;
            Result := (NumRows > 0) and (NumCols > 0);
         end;
         Header.Free;
      end;


      function ExtractGTopo30(ImportingDEM : tDEMDataSet; InName : PathStr; GTopo30Header : tGTopo30Header; NWLat,NwLong,SELat,SELong : float64; NewName : shortstring) : boolean;
      type
         tRow =  array[0..12000] of SmallInt;
      var
         LowCol,HighCol,
         i,j,z,LowRow,HighRow : integer;
         Inf   : file;
         Row   :  ^TRow;
      begin
         Result := false;
         with GTopo30Header do begin
            LowCol := round((NWLong - ULXMap) / XDim);
            HighCol := round((SELong - ULXMap) / XDim);
            if (LowCol < 0) then LowCol := 0;
            if (LowCol > pred(NumCols)) then exit;
            if (HighCol < 0) then exit;
            if (HighCol > pred(NumCols)) then HighCol := pred(NumCols);
            HighRow := round((ULYMap - NWLat) / YDim);
            LowRow :=  round((ULYMap - SELat) / YDim);
            if (LowRow < 0) then exit;
            if (LowRow > pred(NumRows)) then LowRow := pred(NumRows);
            if (HighRow < 0) then HighRow := 0;
            if (HighRow > pred(NumRows)) then exit;

            with MDdef,ImportingDEM,DEMheader do begin
               AreaName := ExtractFileName(InName);
               NumCol := succ(HighCol - LowCol);
               NumRow := succ(LowRow - HighRow);
               if (NumCol <= 1) or (NumRow <= 1) then exit;
               DEMySpacing  := YDim;
               DEMxSpacing := XDim;
               DEMSWCornerX := (ULXMap + LowCol * XDim);
               DEMSWCornerY := (ULYMap - LowRow * YDim);

               ElevUnits    := euMeters;
               ElevationMultiple := 1;
               DEMUsed      := ArcSecondDEM;
               DataSpacing  := SpaceSeconds;
               DigitizeDatum := Spherical;
               if not AllocateDEMMemory then exit;

               InsureFileIsNotReadonly(InName);
               AssignFile(Inf,InName);
               Reset(Inf,2*NumCols);
               seek(inf,HighRow);
               {$IfDef VCL} if ShowDEMReadingProgress then StartProgress('Import Lat' + RealToString(NWLat,4,0) + '  Long' + RealToString(NWLong,5,0)); {$EndIf}
               New(Row);
               for i := pred(NumRow) downto 0 do begin
                   {$IfDef VCL} if ShowDEMReadingProgress then UpdateProgressBar((NumRow-i)/NumRow); {$EndIf}
                  BlockRead(inf,Row^,1);
                  if  NeedSwap then  for j := 0 to pred(NumCol) do Row^[LowCol + j] := swap(Row^[LowCol + j]);
                  for j := 0 to pred(NumCol) do begin
                     z := Row^[LowCol + j];
                     if (z <> NoData) then begin
                        SetGridElevation(j,i,z);
                        Result := true;
                     end;
                  end;
               end;
               Dispose(Row);
               CheckMaxMinElev;
               DefineDEMVariables(true);
               if (abs(DEMheader.MaxElev) < 0.0001) and (abs(DEMheader.MinElev) < 0.0001) then Result := false;
                {$IfDef VCL} if ShowDEMReadingProgress then EndProgress;  {$EndIf}
               CloseFile(inf);
            end;
         end;
         {$IfDef RecordReadDEMProblems} WriteLineToDebugFile('Finished ' + InName); {$EndIf}
      end;


      procedure ReadGlobeOrGtopo30(ImportingDEM : tDEMDataSet; fName : PathStr; var Error : boolean);
      var
         InName : PathStr;
         GTopo30Header : tGTopo30Header;
         Dir : DirStr;
         bName : NameStr;
         Ext   : ExtStr;
         NewName : shortstring;
         ErrorMessage : ShortString;
      begin
         InsureFileIsNotReadOnly(FName);
         FSplit(FName,Dir,bName,Ext);
         Error := true;
         if Read30secondHeader(FName,GTopo30Header) then begin
            with GTopo30Header do begin
               {$IfDef RecordReadDEMProblems}
               WriteLineToDebugFile('GlobeOrGtopo30 extract from file: ' + FName);
               WriteLineToDebugFile('   NW corner:' + LatLongDegreeToString(ULYMap,ULXMap,MDdef.OutPutLatLongMethod) + '   SE corner:' + LatLongDegreeToString(Lat2,Long2,MDdef.OutPutLatLongMethod));
               {$EndIf}
               Inname := Dir + bName;
               if Not FileExists(InName) then Inname := Dir + bName + '.dem';
               {$IfDef VCL}
               if Not FileExists(InName) then begin
                  InName := ChangeFileExt(InName,'.hgt');
                  if not FileExists(InName) then if not GetFileFromDirectory('Input DEM binary file','*.*',InName) then exit;
               end;
               {$EndIf}
               if ExtractGTopo30(ImportingDEM,InName,GTopo30Header,ULYMap,ULXMap,Lat2,Long2,NewName) then Error := false;
            end;
         end;
      end;

      function Read30secondHeader(FName : PathStr; var GTopo30Header : tGTopo30Header) : boolean;
      begin
         Result := FileExists(fName) and (ReadGLOBEHeader(FName,GTopo30Header) or ReadTopo30Header(FName,GTopo30Header));
      end;

