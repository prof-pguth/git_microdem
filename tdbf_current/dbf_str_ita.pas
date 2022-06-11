unit dbf_str;

{ Italian }

interface

{$I dbf_common.inc}
{$I dbf_str.inc}

implementation

initialization

  STRING_FILE_NOT_FOUND               := 'Apertura: file non trovato: "%s"';

  STRING_RECORD_LOCKED                := 'Record già in uso.';
  STRING_READ_ERROR                   := 'Errore di lettura.';
  STRING_WRITE_ERROR                  := 'Errore di scrittura. (Disk full?)';
  STRING_WRITE_INDEX_ERROR            := 'Errore di scrittura; indici probabilmente danneggiati. (Disk full?)';
  STRING_KEY_VIOLATION                := 'Violazione di chiave. (Chiave già presente in file).'+#13+#10+
                                         'Indice: %s'+#13+#10+'Record=%d Chiave=''%s''.';

  STRING_INVALID_DBF_FILE             := 'File DBF non valido.';
  STRING_FIELD_TOO_LONG               := 'Valore troppo elevato: %d caratteri (esso non può essere più di %d).';
  STRING_INVALID_FIELD_COUNT          := 'Campo non valido (count): %d (deve essere tra 1 e 4095).';
  STRING_INVALID_FIELD_TYPE           := 'Tipo di campo non valido ''%s'' per il campo ''%s''.';
  STRING_INVALID_VCL_FIELD_TYPE       := 'Non può creare campo "%s", tipo di campo VCL %x non supportato da DBF.';

  STRING_INVALID_INDEX_FILE           := 'File indice non valido.';
  STRING_PARSER_UNKNOWN_FIELD         := 'Campo sconosciuto "%s"';
  STRING_PARSER_INVALID_FIELDTYPE     := 'Tipo di campo non valido per il campo ''%s''.';
  STRING_INDEX_EXPRESSION_TOO_LONG    := 'Risultato index per "%s" troppo a lungo, >100 caratteri (%d).';
  STRING_INVALID_INDEX_TYPE           := 'Tipo indice non valido: Può essere solo string o float';
  STRING_CANNOT_OPEN_INDEX            := 'Non è possibile aprire indice : "%s"';
  STRING_TOO_MANY_INDEXES             := 'Non è possibile creare indice: Troppi indici aperti.';
  STRING_INDEX_NOT_EXIST              := 'Indice "%s" non esiste.';
  STRING_NEED_EXCLUSIVE_ACCESS        := 'L''Accesso in esclusiva è richiesto per questa operazione.';

  STRING_PROGRESS_PACKINGRECORDS      := 'Comprime records';
  STRING_PROGRESS_READINGRECORDS      := 'Legge records';
  STRING_PROGRESS_APPENDINGRECORDS    := 'Agguinge records';
  STRING_PROGRESS_SORTING_RECORDS     := 'Ordina records';
  STRING_PROGRESS_WRITING_RECORDS     := 'Scrive records';
end.

