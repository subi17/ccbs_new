DEF INPUT PARAMETER  file-name  AS C  NO-UNDO.
DEF OUTPUT PARAMETER db-name    AS C  NO-UNDO.
DEF OUTPUT PARAMETER file-label AS C  NO-UNDO.

DEF VAR i         AS I  NO-UNDO.

/* search the file from all connected databases */
DO i = 1 TO NUM-dbs:
   /* set this database as ACTIVE */
   CREATE ALIAS "dictdb" FOR DATABASE VALUE(ldbname(i)).
   RUN Syst/ufile2.p (INPUT file-name, OUTPUT db-name, OUTPUT file-label).
   IF file-label NE ? THEN LEAVE.
END.

