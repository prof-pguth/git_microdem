unit dbf_str;

{ French }

interface

{$I dbf_common.inc}
{$I dbf_str.inc}

implementation

initialization

  STRING_FILE_NOT_FOUND               := 'Ouverture: fichier non trouvé: "%s"';

  STRING_RECORD_LOCKED                := 'Enregistrement verrouillé.';
  STRING_READ_ERROR                   := 'Erreur de lecture.';
  STRING_WRITE_ERROR                  := 'Erreur d''écriture. (Disque plein?)';
  STRING_WRITE_INDEX_ERROR            := 'Erreur d''écriture; indices probablement corrompus. (Disque plein?)';
  STRING_KEY_VIOLATION                := 'Violation de clé. (doublon dans un index).'+#13+#10+
                                         'Index: %s'+#13+#10+'Enregistrement=%d Cle=''%s''';

  STRING_INVALID_DBF_FILE             := 'Fichier DBF invalide.';
  STRING_FIELD_TOO_LONG               := 'Valeur trop longue: %d caractères (ne peut dépasser %d).';
  STRING_INVALID_FIELD_COUNT          := 'Nombre de champs non valide: %d (doit être entre 1 et 4095).';
  STRING_INVALID_FIELD_TYPE           := 'Type de champ ''%s'' invalide pour le champ %s.';
  STRING_INVALID_VCL_FIELD_TYPE       := 'Impossible de créer le champ "%s", champ type %x VCL non supporté par DBF';

  STRING_INVALID_INDEX_FILE           := 'Fichier index invalide.';
  STRING_PARSER_UNKNOWN_FIELD         := 'Champ inconnu %s';
  STRING_PARSER_INVALID_FIELDTYPE     := 'Type de champ invalide pour le champ %s';
  STRING_INDEX_EXPRESSION_TOO_LONG    := 'Résultat d''Index trop long pour "%s", >100 caractères (%d).';
  STRING_INVALID_INDEX_TYPE           := 'Type d''index non valide: doit être string ou float';
  STRING_CANNOT_OPEN_INDEX            := 'Impossible d''ouvrir l''index: "%s"';
  STRING_TOO_MANY_INDEXES             := 'Impossible de créer l''index: trop d''index dans le fichier.';
  STRING_INDEX_NOT_EXIST              := 'L''index "%s" n''existe pas.';
  STRING_NEED_EXCLUSIVE_ACCESS        := 'Access exclusif requis pour cette opération.';

  STRING_PROGRESS_PACKINGRECORDS      := 'Emballe enregistrements';
  STRING_PROGRESS_READINGRECORDS      := 'Lit enregistrements';
  STRING_PROGRESS_APPENDINGRECORDS    := 'Ajoute enregistrements';
  STRING_PROGRESS_SORTING_RECORDS     := 'Écrit records';
  STRING_PROGRESS_WRITING_RECORDS     := 'Trie records';
end.

