{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for base_graph        }
{________________________________________}

const
    mgsNone = 0;
    mgsByRow = 1;
    mgsAll = 2;

   procedure ZeroGraphArray(var gr : t2Dgrapharray);
   var
      f1,f2 : integer;
   begin
      for f2 := 0 to Max2DGraphArray do
         for f1 := 0 to Max2DGraphArray do gr[f1,f2] := Nil;
   end;


procedure EndGraphGrid(var gr : t2Dgrapharray; var Labels1,Labels2 : tStringList; LegendBMP : tMyBitmap = nil; Caption : shortstring = '');
const
   StartX = 75;
   StartY = 75;
var
   BigBitmap,Bitmap : tMyBitmap;
   xsize,ysize,f1,f2,XPanel,YPanel,Offset,x,y,MaxLeftMargin,CutonLeft,CutonBase,FullX,FullY : integer;
   GraphMaxY,GraphMinY : float32;
   OneGraphBMP : tMyBitmap;
begin
   MaxLeftMargin := 0;
   for f1 := 0 to pred(Labels1.Count) do begin
     for f2 := 0 to pred(Labels2.Count) do begin
         if (gr[f1,f2] <> Nil) then begin
            if (gr[f1,f2].GraphDraw.LeftMargin > MaxLeftMargin) then MaxLeftMargin := gr[f1,f2].GraphDraw.LeftMargin;
         end;
     end;
   end;

   xsize := 0;
   ySize := 0;
   for f1 := 0 to pred(Labels1.Count) do begin
      for f2 := 0 to pred(Labels2.Count) do begin
         if (gr[f1,f2] <> Nil) then begin
            if (gr[f1,f2].GraphDraw.XWindowSize > xsize) then xsize := gr[f1,f2].GraphDraw.XWindowSize;
            if (gr[f1,f2].GraphDraw.YWindowSize > ysize) then Ysize := gr[f1,f2].GraphDraw.YWindowSize;
         end;
      end;
   end;

   if (MDDef.DEMIX_MultiGraphCommonScaling = mgsAll) then begin
       //reset extremes for each all graphs
       GraphMinY := 99e38;
       GraphMaxY := -99e38;
       for f1 := 0 to pred(Labels1.Count) do begin
         for f2 := 0 to pred(Labels2.Count) do begin
             if (gr[f1,f2] <> Nil) then begin
                if (gr[f1,f2].GraphDraw.MaxVertAxis > GraphMaxY) then GraphMaxY := gr[f1,f2].GraphDraw.MaxVertAxis;
                if (gr[f1,f2].GraphDraw.MinVertAxis < GraphMinY) then GraphMinY := gr[f1,f2].GraphDraw.MinVertAxis;
             end;
         end;
       end;
       for f1 := 0 to pred(Labels1.Count) do begin
         for f2 := 0 to pred(Labels2.Count) do begin
             if (gr[f1,f2] <> Nil) then begin
                gr[f1,f2].GraphDraw.MaxVertAxis := GraphMaxY;
                gr[f1,f2].GraphDraw.MinVertAxis := GraphMinY;
                gr[f1,f2].RedrawDiagram;
             end;
          end;
       end;
   end;

   if (MDDef.DEMIX_MultiGraphCommonScaling = mgsByRow) then begin
       for f1 := 0 to pred(Labels1.Count) do begin
         //reset extremes for each row
         GraphMinY := 99e38;
         GraphMaxY := -99e38;
         for f2 := 0 to pred(Labels2.Count) do begin
             if (gr[f1,f2] <> Nil) then begin
                if (gr[f1,f2].GraphDraw.MaxVertAxis > GraphMaxY) then GraphMaxY := gr[f1,f2].GraphDraw.MaxVertAxis;
                if (gr[f1,f2].GraphDraw.MinVertAxis < GraphMinY) then GraphMinY := gr[f1,f2].GraphDraw.MinVertAxis;
             end;
          end;
          for f2 := 0 to pred(Labels2.Count) do begin
             if (gr[f1,f2] <> Nil) then begin
                gr[f1,f2].GraphDraw.MaxVertAxis := GraphMaxY;
                gr[f1,f2].GraphDraw.MinVertAxis := GraphMinY;
                gr[f1,f2].RedrawDiagram;
             end;
          end;
       end;
   end;

   for f1 := 0 to pred(Labels1.Count) do begin
      for f2 := 0 to pred(Labels2.Count) do begin
          if (gr[f1,f2] <> Nil) then begin
             gr[f1,f2].RedrawDiagram;
             CutOnLeft := gr[f1,f2].GraphDraw.LeftMargin - 5;
             CutOnBase := gr[f1,f2].GraphDraw.BottomMargin - 5;
             FullX := gr[f1,f2].Image1.Width;
             FullY := gr[f1,f2].Image1.Height;
          end;
      end;
   end;

   XPanel := FullX + 20;
   YPanel := FullY + 20;
   CreateBitmap(BigBitmap,StartX + Labels2.Count * XPanel, StartY + Labels1.Count * YPanel);
   BigBitmap.Canvas.Font.Size := 24;
   BigBitmap.Canvas.Font.Style := [fsBold];

   y := StartY;
   for f1 := 0 to pred(Labels1.Count) do begin
      Offset := Y + (Fully - CutOnBase) - (Fully - CutOnBase - BigBitmap.Canvas.TextWidth(Labels1.Strings[f1])) div 2;
      TextOutVertical(BigBitmap.Canvas,2, Offset,Labels1.Strings[f1]);
      x := StartX;
      for f2 := 0 to pred(Labels2.Count) do begin
         if (f1 = 0) then begin //put column labels on
            if (f2 = 0) then Offset := (X + CutOnLeft + BigBitmap.Canvas.TextWidth(Labels2.Strings[f2])) div 2
            else Offset := X + BigBitmap.Canvas.TextWidth(Labels2.Strings[f2]) div 2;
            BigBitmap.Canvas.TextOut(Offset,2,Labels2.Strings[f2]);
         end;
         if (gr[f1,f2] = Nil) then begin
         end
         else begin
            CopyImageToBitmap(gr[f1,f2].Image1,OneGraphBMP);
            {$IfDef RecordMergePanels}
               WriteLineToDebugFile('');
               WriteLineToDebugFile('f1=' + IntToStr(f1) + ' f2=' + IntToStr(f2) + '  Incoming: ' + BitmapSizeString(OneGraphBMP));
            {$EndIf}
            if (MDDef.DEMIX_MultiGraphCommonScaling in [mgsByRow,mgsAll]) and (f2 > 0) then begin
               ReplaceBitmapWithSubset(OneGraphBMP,CutOnLeft,pred(OneGraphBMP.Width),0,pred(OneGraphBMP.Height));
            end;
            if {(MDDef.DEMIX_MultiGraphCommonScaling in [mgsAll]) and} (f1 < pred(Labels1.Count)) then begin
               OneGraphBMP.Height := OneGraphBMP.Height - CutOnBase;
            end;
            {$IfDef RecordMergePanels}
               WriteLineToDebugFile('Clipped: ' + BitmapSizeString(OneGraphBMP));
               WriteLineToDebugFile('Drawing location  x=' + IntToStr(x) + '  y=' + IntToStr(y));
               WriteLineToDebugFile('');
            {$EndIf}
            BigBitMap.Canvas.Draw(x,y,OneGraphBMP);
            OneGraphBMP.destroy;
         end;
         if (MDDef.DEMIX_MultiGraphCommonScaling in [mgsByRow,mgsAll]) and (f2 > 0) then inc(x,FullX-CutOnLeft + 15)
         else inc(x,FullX + 15);
      end;
      {if (f1 < pred(Labels1.Count)) then} inc(y,FullY-CutOnBase + 15)
      //else inc(y,FullY + 15);
   end;

(*
   for f1 := 0 to pred(Labels1.Count) do begin
      Offset := (YPanel - BigBitmap.Canvas.TextWidth(Labels1.Strings[f1])) div 2;
      TextOutVertical(BigBitmap.Canvas,2,StartY + succ(F1) * YPanel - Offset,Labels1.Strings[f1]);
   end;

   for f2 := 0 to pred(Labels2.Count) do begin
      Offset := (XPanel - BigBitmap.Canvas.TextWidth(Labels2.Strings[f2])) div 2;
      BigBitmap.Canvas.TextOut(StartX + f2 * XPanel + Offset,2,Labels2.Strings[f2]);
   end;

   for f1 := 0 to pred(Labels1.Count) do begin
      for f2 := 0 to pred(Labels2.Count) do begin
         if (gr[f1,f2] <> Nil) and CopyImageToBitmap(gr[f1,f2].Image1,Bitmap) then begin
            BigBitMap.Canvas.Draw(StartX + f2 * XPanel, StartY + F1 * YPanel,Bitmap);
            Bitmap.Free;
         end;
      end;
   end;
*)
   if (LegendBMP <> Nil) then begin
      x := (BigBitMap.Width - LegendBMP.Width) div 2;
      y := BigBitmap.Height + 20;
      BigBitmap.Height := BigBitMap.Height + 20 + LegendBMP.Height;
      BigBitmap.Canvas.Draw(x,y, LegendBMP);
   end;
   DrawCollarAroundBitmap(BigBitmap);

   if (Caption = '') then Caption := 'graph_grid';
   DisplayBitmap(BigBitmap,Caption);
   Labels1.Destroy;
   Labels2.Destroy;
   SetColorForWaiting;
   wmdem.ClearStatusBarPanelText;
end {procedure EndGraphGrid};



procedure DrawLegendBelowBigBitmap(var BigBMP : tMyBitmap; Legend : PathStr; LRText : shortstring = '');
var
   LegBMP : tMyBitmap;
   x,y : integer;
begin
    if (Legend <> '') then begin
        LegBMP := LoadBitmapFromFile(Legend);
        if (LegBMP.Height < 75) then begin
           BigBmp.Height := BigBmp.Height + LegBMP.Height + 5;
           y := BigBmp.Height - LegBMP.Height - 4;
           x := (BigBmp.Width - LegBMP.Width) div 2;
        end;
        BigBmp.Canvas.Draw(X,y,LegBmp);
        LegBMP.Free;
    end;
    if (LRText <> '') then begin
       LRText := RemoveUnderscores(LRText);
       BigBmp.Canvas.Font.Style := [fsBold];
       BigBmp.Canvas.Font.Size := 24;
       while (BigBmp.Canvas.TextWidth(LRText) > BigBmp.Width - 10) do BigBmp.Canvas.Font.Size := BigBmp.Canvas.Font.Size - 1;
       x := BigBmp.Width - 10 - BigBmp.Canvas.TextWidth(LRText);
       y := BigBmp.Height - 5 - BigBmp.Canvas.TextHeight(LRText);
       BigBmp.Canvas.TextOut(x,y,LRText);
    end;
end;


function MergeBitmapsHorizontal(theFiles : tStringList) : PathStr;
var
   i : integer;
   BigBitMap,Bitmap : tMyBitmap;
   fName : PathStr;
begin
   if (theFiles.Count = 1) then begin
      Result := theFiles[i];
   end
   else begin
       for i := 0 to pred(theFiles.Count) do begin
          fName := theFiles[i];
          Bitmap := LoadBitmapFromFile(fName);
          if (i = 0) then begin
             CreateBitmap(BigBitMap,Bitmap.Width,theFiles.Count * (Bitmap.Height + 15))
          end;
          BigBitmap.Canvas.Draw(0,i * (Bitmap.Height + 15),Bitmap);
          Bitmap.Destroy;
       end;
       Result := NextFileNumber(MDtempdir,'Horiz_panel_','.bmp');
       SaveBitmap(BigBitmap,Result);
       BigBitMap.Destroy;
   end;
end;



function MergeGraphPanelsHorizontal(nGraphs : integer;  Graphs : t1DGraphArray; RemoveLeftLabels : boolean; LegendFName : PathStr; LRtext : shortstring = '') : PathStr;
var
   i,StartX,DestX,PixWide : integer;
   BigBitMap,Bitmap : tMyBitmap;
   Dest,Source : tRect;
begin
   {$IfDef RecordMergePanels} WriteLineToDebugFile('MergeGraphPanelsHorizontal in, nGraphs=' + IntToStr(NGraphs)); {$EndIf}
   CopyImageToBitmap(Graphs[0].Image1,BigBitmap);
   for i := 1 to pred(nGraphs) do begin
      {$IfDef RecordMergePanels} WriteLineToDebugFile('Start panel=' + IntToStr(I)); {$EndIf}
      CopyImageToBitmap(Graphs[i].Image1,Bitmap);
      if RemoveLeftLabels then begin
        StartX := graphs[i].GraphDraw.LeftMargin;
        PixWide := Bitmap.Width - graphs[i].GraphDraw.LeftMargin;
      end
      else begin
         StartX := 0;
         PixWide := Bitmap.Width;
      end;
      DestX := BigBitMap.Width + 15;
      BigBitMap.Width := DestX + PixWide;
      Dest := rect(DestX,0,DestX+PixWide,BigBitMap.Height);
      Source := rect(StartX,0,Bitmap.Width,Bitmap.Height);
      BigBitmap.Canvas.CopyRect(Dest,Bitmap.Canvas,Source);
      Bitmap.Destroy;
   end;
   if (LegendFName <> '') then DrawLegendBelowBigBitmap(BigBitmap,LegendFName,LRtext);
   DrawCollarAroundBitmap(BigBitMap);
   Result := NextFileNumber(MDtempdir,'Horiz_panel_','.bmp');
   SaveBitmap(BigBitmap,Result);
   BigBitMap.Destroy;
   {$IfDef RecordMergePanels} WriteLineToDebugFile('MergeGraphPanelsHorizontal out'); {$EndIf}
end;



function MergeGraphPanelsVertical(nGraphs : integer;  Graphs : t1DGraphArray; RemoveLeftLabels : boolean; Legend : tMyBitmap = nil) : PathStr;
var
   i,StartX,StartY,PixWide : integer;
   BigBitMap,Bitmap,CollarBitmap : tMyBitmap;
   fName : PathStr;
begin
   for i := 0 to pred(nGraphs) do begin
      CopyImageToBitmap(Graphs[i].Image1,Bitmap);
      if i = 0 then begin
         CreateBitmap(BigBitMap,Bitmap.Width,Bitmap.Height + 15);
         StartY := 0;
      end
      else begin
         StartY := BigBitmap.Height;
         BigBitmap.Height := BigBitmap.Height + Bitmap.Height + 15;
      end;
      if RemoveLeftLabels then begin
        StartX := graphs[i].GraphDraw.LeftMargin;
        PixWide := Bitmap.Width - graphs[i].GraphDraw.LeftMargin;
      end
      else begin
         StartX := 0;
         PixWide := Bitmap.Width;
      end;
      BigBitmap.Canvas.CopyRect(Rect(0,StartY,PixWide,StartY + BitMap.Height),Bitmap.Canvas,Rect(StartX,0,Bitmap.Width,Bitmap.Height));
      Bitmap.Destroy;
   end;
   fName := NextFileNumber(MDtempDir,'legend_','.bmp');
   DrawLegendBelowBigBitmap(BigBitmap,fName);
   DrawCollarAroundBitmap(BigBitmap);
   Result := NextFileNumber(MDtempdir,'Graph_panels_','.bmp');
   Bitmap.SaveToFile(fName);
   BitMap.Destroy;

(*
   GetImagePartOfBitmap(BigBitmap);
   GetImagePartOfBitmap(BigBitmap);
   CreateBitmap(CollarBitmap,BigBitmap.Width + 4,BigBitmap.Height + 4);
   CollarBitmap.Canvas.Draw(2,2,BigBitmap);
   BigBitmap.Destroy;
   xx

   if (Legend <> Nil) then begin
       CollarBitmap.Height := CollarBitmap.Height + 15 + Legend.Height;
       CollarBitmap.Canvas.Draw((CollarBitmap.Width - Legend.Width) div 2,CollarBitmap.Height - Legend.Height,Legend);
       Legend.Destroy;
   end;
   Result := NextFileNumber(MDtempdir,'Graph_panels_','.bmp');
   CollarBitmap.SaveToFile(Result);
   CollarBitmap.Destroy;
*)
end;

 procedure MergeVerticalPanels(Findings : tStringList; Legend : tMyBitmap; PanelName : shortstring);
  var
     BigBitmap,Bitmap : tMyBitmap;
     i : integer;
  begin
     Bigbitmap := tMyBitmap.Create;
     Bigbitmap.LoadFromFile(Findings.strings[0]);

     for I := 1 to pred(Findings.Count) do begin
        bitmap := tMyBitmap.Create;
        bitmap.LoadFromFile(Findings.strings[i]);
        BigBitmap.Width :=  BigBitmap.Width + 15 + Bitmap.Width;
        BigBitmap.Canvas.Draw(BigBitmap.Width - Bitmap.Width, 0, Bitmap);
        Bitmap.Destroy;
     end;
     if (Legend <> Nil) then begin
         BigBitMap.Height := BigBitMap.Height + 15 + Legend.Height;
         BigBitmap.Canvas.Draw((BigBitmap.Width - Legend.Width) div 2,BigBitmap.Height - Legend.Height,Legend);
         Legend.Destroy;
     end;
     DisplayBitmap(BigBitmap,PanelName);
     Findings.Destroy;
  end;











