/* --------------------------------------------------------------------------
 MODULE ...........: ulfscon.p
 TASK .............: Convert a long lf separated string into temp-table records
 SOLUTION .........: 
 CREATED ..........: 12.12.2001 lp
 CHANGED ..........: 
 VERSION ..........: M15
--------------------------------------------------------------------------- */

DEF TEMP-TABLE t-text
   FIELD tt-no   AS I
   FIELD tt-line AS C.

DEF INPUT  PARAMETER  lf-sep-string   AS C  NO-UNDO.
DEF INPUT  PARAMETER  RowMaxLen       AS I  NO-UNDO.

DEF OUTPUT PARAMETER  TABLE FOR t-text.

DEF VAR memotext  AS C  NO-UNDO.
DEF VAR memoline  AS C  NO-UNDO.
DEF VAR i         AS I  NO-UNDO.
DEF VAR x         AS I  NO-UNDO.
DEF VAR nrow      AS I  NO-UNDO.
DEF VAR space-pos AS I  NO-UNDO.
DEF VAR lf        AS C  NO-UNDO. 

/* Line Feed Char used as line separator in memotext field */
LF = chr(10).

/* copy memotext (line by line) into temp table */

DO i = 1 TO NUM-ENTRIES(lf-sep-string,lf):
   /* pick up next line */
   memoline = ENTRY(i,lf-sep-string,lf).

   REPEAT:
      IF LENGTH(memoline) > RowMaxLen THEN DO:

         /* look for the last space from right to left */
         space-pos = RowMaxLen.
         DO x = RowMaxLen TO 1 BY -1:
            IF SUBSTR(memoline,x,1) = " " THEN ASSIGN space-pos = x x = 0.
         END.

         CREATE t-text.
         ASSIGN
         nrow           = nrow + 1
         t-text.tt-no   = nrow
         t-text.tt-line = SUBSTR(memoline,1,space-pos).

         /* cut away leading 'RowMaxLen' bytes */
         memoline = TRIM(SUBSTR(memoline,space-pos) ).
      END.   
      ELSE LEAVE.
   END. 
   CREATE t-text.
   ASSIGN 
      nrow           = nrow + 1
      t-text.tt-no   = nrow
      t-text.tt-line = memoline.
END. 

