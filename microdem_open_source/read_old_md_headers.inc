                procedure ReadHeaderV1;
                var
                   OldHeadRecs : tHeaderRecordv1;
                begin
                   BlockRead(Ffile,OldHeadRecs,SizeOf(OldHeadRecs));
                   DEMheader.DEMUsed := OldHeadRecs.DEMUsed;
                   DEMheader.DataSpacing  := OldHeadRecs.DataSpacing;
                   DEMheader.NumCol := OldHeadRecs.NumCol;
                   DEMheader.NumRow := OldHeadRecs.NumRow;
                   DEMheader.ElevUnits := OldHeadRecs.ElevUnits;
                   DEMheader.UTMZone   := OldHeadRecs.UTMZone;
                   DEMheader.DMAMapDefinition := OldHeadRecs.DMAMapDefinition;
                   DEMheader.DigitizeDatum := OldHeadRecs.DigitizeDatum;
                   DEMheader.LatHemi    := OldHeadRecs.LatHemi;
                   if (OldHeadrecs.oiLatInterval <> 0) and (OldHeadrecs.oiLongInterval <> 0) then begin
                      case OldHeadRecs.DataSpacing of
                         SpaceMeters : l1 := 1.0;
                         SpaceKM     : l1 := 1000.0;
                         Space100m   : l1 := 100.0;
                         Space10m    : l1 := 10.0;
                         SpaceFeet   : l1 := Petmar_types.FeetToMeters;
                         SpaceKFeet  : l1 := 304.8;
                         oldSpaceMinutes : l1 := 60;
                         oldSpaceSeconds : l1 := 3600;
                         else L1 := 1;
                      end;
                      DEMheader.DEMSWCornerY := OldHeadrecs.oBaseYUTM24K / l1;
                      DEMheader.DEMSWCornerX := OldHeadrecs.oBaseXUTM24K / l1;
                      DEMheader.DEMySpacing  := OldHeadrecs.oiLatInterval / l1;
                      DEMheader.DEMxSpacing := OldHeadrecs.oiLongInterval / l1;
                   end
                   else
                   begin
                      DEMheader.DEMySpacing := OldHeadRecs.fLatInterval;
                      DEMheader.DEMxSpacing := OldHeadRecs.fLongInterval;
                      DEMheader.DEMSWCornerX := OldHeadRecs.fSWCornerx;
                      DEMheader.DEMSWCornerY := OldHeadRecs.fSWCornerY;
                   end;
                   DEMheader.MaxElev := ZinMeters(OldHeadRecs.oMaxElev);
                   DEMheader.MinElev := ZinMeters(OldHeadRecs.oMinElev);
                   DEMheader.DEMPrecision := SmallIntDEM;
                end;

                procedure ReadHeaderV2;
                var
                   HeaderV2 : tHeaderRecordV2;
                begin
                   BlockRead(Ffile,Headerv2,SizeOf(HeaderV2));
                   DEMheader.DEMUsed      := HeaderV2.DEMUsed;
                   DEMheader.DEMPrecision := HeaderV2.DEMPrecision;
                   DEMheader.DataSpacing  := HeaderV2.DataSpacing;
                   DEMheader.MaxElev := HeaderV2.MaxElev;
                   DEMheader.MinElev := HeaderV2.MinElev;
                   DEMheader.DEMySpacing := HeaderV2.fLatInterval;
                   DEMheader.DEMxSpacing := HeaderV2.fLongInterval;
                   DEMheader.NumCol := HeaderV2.NumCol;
                   DEMheader.NumRow := HeaderV2.NumRow;
                   DEMheader.ElevUnits := HeaderV2.ElevUnits;
                   DEMheader.UTMZone  := HeaderV2.UTMZone;
                   DEMheader.DMAMapDefinition := HeaderV2.DMAMapDefinition;
                   DEMheader.DigitizeDatum  := HeaderV2.DigitizeDatum;
                   DEMheader.LatHemi := HeaderV2.LatHemi;
                   DEMheader.DEMPrecision := HeaderV2.DEMPrecision;
                   DEMheader.DEMSWCornerX := HeaderV2.hdfSWCornerx;
                   DEMheader.DEMSWCornerY  := HeaderV2.hdfSWCornerY;
                   if (abs(DEMheader.DEMSWCornerX) < 0.0000001) and (abs(HeaderV2.oldhdfSWCornerY) > 0.0000001) then begin
                      DEMheader.DEMSWCornerX := HeaderV2.oldhdfSWCornerx;
                      HeaderV2.oldhdfSWCornerx := 0;
                   end;
                   if (abs(DEMheader.DEMSWCornerY) < 0.0000001) and (abs(HeaderV2.oldhdfSWCornerY) > 0.0000001) then begin
                      DEMheader.DEMSWCornerY := HeaderV2.oldhdfSWCornerY;
                      HeaderV2.oldhdfSWCornerY := 0;
                   end;
                end;

                procedure ReadHeaderV3;
                var
                   HeaderV3 : tHeaderRecordV3;
                begin
                   BlockRead(Ffile,Headerv3,SizeOf(Headerv3)); //version 3 header, post 5/22/2010
                   DEMheader.DEMUsed      := HeaderV3.DEMUsed;
                   DEMheader.DEMPrecision := HeaderV3.DEMPrecision;
                   DEMheader.DataSpacing  := HeaderV3.DataSpacing;
                   DEMheader.MaxElev := HeaderV3.MaxElev;
                   DEMheader.MinElev := HeaderV3.MinElev;
                   DEMheader.DEMySpacing := HeaderV3.fLatInterval;
                   DEMheader.DEMxSpacing := HeaderV3.fLongInterval;
                   DEMheader.NumCol := HeaderV3.NumCol;
                   DEMheader.NumRow := HeaderV3.NumRow;
                   DEMheader.ElevUnits := HeaderV3.ElevUnits;
                   DEMheader.UTMZone  := HeaderV3.UTMZone;
                   DEMheader.DMAMapDefinition := HeaderV3.DMAMapDefinition;
                   DEMheader.DigitizeDatum  := HeaderV3.DigitizeDatum;
                   DEMheader.LatHemi  := HeaderV3.LatHemi;
                   DEMheader.DEMPrecision := HeaderV3.DEMPrecision;
                   DEMheader.DEMSWCornerX := HeaderV3.hdfSWCornerx;
                   DEMheader.DEMSWCornerY  := HeaderV3.hdfSWCornerY;
                   if (DEMheader.NumCol = 0) or (DEMheader.NumRow = 0) then begin // DEM from program version before 6/16/2011
                      DEMheader.NumCol := HeaderV3.OldNumCol;
                      DEMheader.NumRow := HeaderV3.OldNumRow;
                   end;
                end;
				

             procedure ReadThatOldHeader;

                   function FirstIntegerOnLine(var aString : AnsiString) : integer;

                            function NextLineFromString(var aString : AnsiString) : AnsiString;
                            var
                               i : integer;
                            begin
                               i := 1;
                               while not (aString[i] in [#10,#13]) do begin
                                  inc(i);
                                  if (i > length(aString)) then begin
                                     Result := '';
                                     exit;
                                  end;
                               end;
                               Result := ptCopy(astring,1,i);
                               Delete(astring,1,i);
                            end;

                   var
                      TStr : AnsiString;
                      err : integer;
                   begin
                      TStr := NextLineFromString(aString);
                      TStr := BeforeSpecifiedCharacterANSI(TStr,' ',false);
                      val(TStr,Result,err);
                   end;


             begin
                aTStr := TStr;
                HeaderOffset := FirstIntegerOnLine(aTStr);
                DEMoffset := FirstIntegerOnLine(aTStr);
                if (DEMOffset > 192) then LogRecAOffset := FirstIntegerOnLine(aTStr)
                else LogRecAOffset := 0;

                reset(fFile,1);
                Seek(fFile,HeaderOffset);
                {$IfDef Record_MD_DEM} WriteLineToDebugFile('Magic=' + Magic);  {$EndIf}

                if (Magic ='*MICRODEM DEM') then ReadHeaderV1
                else if (Magic ='*MICRODEM v.2') then ReadHeaderV2
                else if (Magic ='*MICRODEM v.3') then ReadHeaderV3;

                case DEMheader.DigitizeDatum of
                  NAD27d : StringToByteArray('NAS-C',DEMheader.DMAMapDefinition.h_DatumCode);
                  WGS72d : StringToByteArray('WGS72',DEMheader.DMAMapDefinition.h_DatumCode);
                  NAD83d : StringToByteArray('NAR-C',DEMheader.DMAMapDefinition.h_DatumCode);
                  WGS84d : StringToByteArray('WGS84',DEMheader.DMAMapDefinition.h_DatumCode);
               end {case};
             end;
				
				
