DEF NEW shared STREAM excel.

/* GLOBAL variables, we shall CREATE these only once */
def var my-nl  as c  no-undo format "x(2)".
def var my-nl2 as c  no-undo format "x(2)".
DEF VAR my-i   AS i  NO-UNDO init 1.
DEF VAR my-ok  AS lo NO-UNDO.
DEF VAR tab    AS c  NO-UNDO.
DEF VAR uname  AS c  NO-UNDO.


/* DEFAULT: tabulator - column separator in excel 
            new-line  - ROW separator: 21.12.1999 kl: chr(10) FOR ALL */
ASSIGN
   tab    = chr(9)
   my-nl  = chr(10)
   my-nl2 = chr(13) + chr(10).

FUNCTION fPutExcel RETURNS logical
   (INPUT line AS CHAR, INPUT sep AS CHAR, INPUT jump AS INT):

   /* replace separating CHARACTER WITH tabulator */
   if sep ne "" THEN DO:
      repeat WHILE my-i NE 0:
         my-i = index(line,sep).
         IF my-i NE 0 THEN 
            ASSIGN substr(line,my-i,length(sep)) = tab.
      END.
   END.

   /* write line into excel -file */
   PUT STREAM excel UNFORMATTED line.

   /* CREATE NEW lines AS wanted */
   DO my-i = 1 TO jump.
      PUT STREAM excel UNFORMATTED my-nl.
   END.

   IF error-status:error THEN my-ok = FALSE.
   ELSE my-ok = TRUE.

   RETURN my-ok.

END.

