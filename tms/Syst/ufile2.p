DEF INPUT PARAMETER   file-name  AS C  NO-UNDO.
DEF OUTPUT PARAMETER  db-name    AS C  NO-UNDO.
DEF OUTPUT PARAMETER  file-label AS C  NO-UNDO.



/*******************************
* Retrive the file definition  *
* record from data dictionary. *
* Return the file label string *
*******************************/
FIND dictdb._file WHERE dictdb._file._file-name = file-name NO-LOCK NO-ERROR.
IF AVAIL dictdb._file THEN DO:
   ASSIGN 
     file-label = dictdb._file._file-label
     db-name    = ldbname("dictdb").

     IF file-label = ? THEN file-label = "*" + file-name + "*".

END.
ELSE ASSIGN 
   file-label = ?
   db-name    = ?.
