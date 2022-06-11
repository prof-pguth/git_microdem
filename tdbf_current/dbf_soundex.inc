function Soundex(const aString: AnsiString): AnsiString;
const
  soundextable:packed array['A'..'Z'] of AnsiChar = '.123.12..22455.12623.1.2.2';
var
   SoundString: AnsiString;
   I1: Integer;      // input
   I2: Integer;      // output
   C2: AnsiChar;
   len: integer;
begin
  len := Length(aString);
  result := '0000';
  if len > 0 then
    begin
      // find first non-blank char
      I1 := 0;
      repeat
        Inc(I1);
        C2 := UpCase(aString[I1]);
      until (I1 >= len) or (C2 <> ' ');

      // if first non-blank is not alphabetic, result is 0000
      if (C2 < 'A') or (C2 > 'Z') then
        exit;

      // calculate sound value for each remaining character
      Len := Len - I1 + 1;
      SetLength(SoundString, Len);
      Fillchar(SoundString[1], Len, '0');
      SoundString[1] := C2;
      for I2 := I1+1 to Length(astring) do
        begin
          C2 := UpCase(astring[I2]);
          if (C2 < 'A') or (C2 > 'Z') then
            break;
          SoundString[i2-i1+1] := SoundexTable[C2];
        end;

      // if adjacent chars have the same sound value, delete the second
      I1 := 2;
      repeat
        if I1 < Length(SoundString) then
          if SoundString[I1] = SoundString[I1+1] then
            Delete(SoundString, I1+1, 1)
          else
            Inc(I1);
      until I1 >= Length(SoundString);

      // result is 1st char followed by next 3 non-ignored chars padded to
      // right with '0', if necessary
      I1 := 1;
      I2 := 1;
      repeat
        if SoundString[I2] <> '.' then
          begin
            result[I1] := SoundString[I2];
            inc(I1);
          end;
        inc(I2);
      until (I1 > 4) or (I2 > Length(SoundString));
    end;
end;

