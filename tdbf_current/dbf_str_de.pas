unit dbf_str;

{ German }

interface

{$I dbf_common.inc}
{$I dbf_str.inc}

implementation

initialization

  STRING_FILE_NOT_FOUND               := 'Öffnen: Datei nicht gefunden: "%s"';

  STRING_RECORD_LOCKED                := 'Datensatz gesperrt.';
  STRING_READ_ERROR                   := 'Lesefehler.';
  STRING_WRITE_ERROR                  := 'Schreibfehler (Festplatte voll?)';
  STRING_WRITE_INDEX_ERROR            := 'Schreibfehler; Indizes wahrscheinlich beschädigt. (Festplatte voll?)';
  STRING_KEY_VIOLATION                := 'Indexverletzung (Indexschlüssel bereits vorhanden)'+#13+#10+
                                         'Index: %s'+#13+#10+'Record=%d Schlüssel=''%s''';

  STRING_INVALID_DBF_FILE             := 'Ungültige DBF-Datei.';
  STRING_FIELD_TOO_LONG               := 'Wert ist zu lang: %d Zeichen (maximal erlaubt: %d).';
  STRING_INVALID_FIELD_COUNT          := 'Ungültige Anzahl von Feldern: %d (muss zwischen 1 und 4095 sein).';
  STRING_INVALID_FIELD_TYPE           := 'Ungültiger Feldtyp ''%s'' für das Feld ''%s''.';
  STRING_INVALID_VCL_FIELD_TYPE       := 'Feld "%s" kann nicht erzeugt werden: VCL-Feldtyp %x wird nicht von DBF unterstützt.';

  STRING_INVALID_INDEX_FILE           := 'Ungültige Index-Datei.';
  STRING_PARSER_UNKNOWN_FIELD         := 'Unbekanntem Feld "%s".';
  STRING_PARSER_INVALID_FIELDTYPE     := 'Ungültiger Feldtyp für das Feld ''%s''.';
  STRING_INDEX_EXPRESSION_TOO_LONG    := 'Das Ergebnis des Index-Ausdrucks "%s" ist zu lang, >100 Zeichen (%d).';
  STRING_INVALID_INDEX_TYPE           := 'Ungültiger Indextyp: nur Zeichen oder Numerisch erlaubt.';
  STRING_CANNOT_OPEN_INDEX            := 'Öffnen des Index ist gescheitert: "%s".';
  STRING_TOO_MANY_INDEXES             := 'Erzeugen eines Index nicht möglich: Zu viele Indizes in der Datei.';
  STRING_INDEX_NOT_EXIST              := 'Index "%s" existiert nicht.';
  STRING_NEED_EXCLUSIVE_ACCESS        := 'Exclusivzugriff für diese Aktion nötig.';

  STRING_PROGRESS_PACKINGRECORDS      := 'Packt datensätzen';
  STRING_PROGRESS_READINGRECORDS      := 'Liest datensätzen';
  STRING_PROGRESS_APPENDINGRECORDS    := 'Anhängt datensätzen';
  STRING_PROGRESS_SORTING_RECORDS     := 'Sortiert datensätzen';
  STRING_PROGRESS_WRITING_RECORDS     := 'Schreibt datensätzen';
end.

