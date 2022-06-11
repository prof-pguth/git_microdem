unit dbf_str;

{ Dutch }

interface

{$I dbf_common.inc}
{$I dbf_str.inc}

implementation

initialization

  STRING_FILE_NOT_FOUND               := 'Openen: bestand niet gevonden: "%s"';

  STRING_RECORD_LOCKED                := 'Record in gebruik.';
  STRING_READ_ERROR                   := 'Error tijdens lezen.';
  STRING_WRITE_ERROR                  := 'Error tijdens schrijven. (Disk vol?)';
  STRING_WRITE_INDEX_ERROR            := 'Error tijdens schrijven; indexen waarschijnlijk beschadigd. (Disk vol?)';
  STRING_KEY_VIOLATION                := 'Indexsleutel bestond al in bestand.'+#13+#10+
                                         'Index: %s'+#13+#10+'Record=%d Sleutel=''%s''';

  STRING_INVALID_DBF_FILE             := 'Ongeldig DBF bestand.';
  STRING_FIELD_TOO_LONG               := 'Waarde is te lang: %d karakters (maximum is %d).';
  STRING_INVALID_FIELD_COUNT          := 'Ongeldig aantal velden: %d (moet tussen 1 en 4095).';
  STRING_INVALID_FIELD_TYPE           := 'Veldtype ''%s'' is ongeldig voor veld ''%s''.';
  STRING_INVALID_VCL_FIELD_TYPE       := 'Veld "%s": VCL veldtype %x wordt niet ondersteund door DBF.';

  STRING_INVALID_INDEX_FILE           := 'Ongeldig index bestand.';
  STRING_PARSER_UNKNOWN_FIELD         := 'Onbekend veld "%s".';
  STRING_PARSER_INVALID_FIELDTYPE     := 'Veldtype is ongeldig voor veld ''%s''.';
  STRING_INDEX_EXPRESSION_TOO_LONG    := 'Index expressie resultaat "%s" is te lang, >100 karakters (%d).';
  STRING_INVALID_INDEX_TYPE           := 'Ongeldig index type: kan alleen karakter of numeriek.';
  STRING_CANNOT_OPEN_INDEX            := 'Openen index gefaald: "%s".';
  STRING_TOO_MANY_INDEXES             := 'Toevoegen index onmogenlijk: te veel indexen in bestand.';
  STRING_INDEX_NOT_EXIST              := 'Index "%s" bestaat niet.';
  STRING_NEED_EXCLUSIVE_ACCESS        := 'Exclusieve toegang is vereist voor deze actie.';

  STRING_PROGRESS_PACKINGRECORDS      := 'Packs records';
  STRING_PROGRESS_READINGRECORDS      := 'Leest records';
  STRING_PROGRESS_APPENDINGRECORDS    := 'Voegt records';
  STRING_PROGRESS_SORTING_RECORDS     := 'Sorteert records';
  STRING_PROGRESS_WRITING_RECORDS     := 'Schrijft records';
end.

