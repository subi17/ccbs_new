/*------------------------------------------------------------
  MODULE .......: JUNKSIZE.P
  FUNCTION .....: Check junk file SIZE + make a NEW one IF needed
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 15.11.1999 BY kl
  MODIFIED .....: 19.12.1999 BY kl: email IF file is NOT found
                  29.12.1999 BY kl: move OR check (AND move IF needed)
  VERSION ......: M15
--------------------------------------------------------------*/

{Syst/commali.i}
{Func/function.i}
{Func/cparam2.i}
{Func/mailx.i}

DEF INPUT PARAMETER fname AS c  NO-UNDO.
DEF INPUT PARAMETER move  AS lo NO-UNDO.

DEF VAR line  AS c NO-UNDO.
DEF VAR fsize AS i NO-UNDO.
DEF VAR i     AS i NO-UNDO.
def var sep   as c no-undo init " ".
DEF VAR ctmp  AS c NO-UNDO.
DEF VAR path  AS c NO-UNDO.
DEF VAR file  AS c NO-UNDO.
DEF VAR ext   AS c NO-UNDO.
DEF VAR errd  AS c NO-UNDO.

FUNCTION fRemEmptyCol RETURNS CHAR
  (INPUT src AS CHAR, INPUT sep AS CHAR).

   DEF VAR loop AS i NO-UNDO init 1.

   /* loop until EOF */
   DO WHILE keycode(substr(src,loop,1)) NE -1.
      /* FIND FIRST separator, remove the following ones */
      IF substr(src,loop,1) = sep THEN DO WHILE (substr(src,loop + 1,1) = sep).
         substr(src,loop + 1,1) = "".
      END.
      loop = loop + 1.
   END.
   RETURN src.

END.

IF search(fname) = ? THEN DO:
   fMailX("kyosti@starnet.fi","\"JunkFile Was Not Found\"").
   RETURN.
END.

/* split into path & file */
DO i = length(fname) TO 1 BY -1.

   if substr(fname,i,1) = "/" THEN DO:
      ASSIGN
         path = substr(fname,1,i)
         file = substr(fname,i + 1)
         i    = 0.
   END.

END.

/* IF no path was found */
if path = "" THEN file = fname.

/* split into file & extension */
i = index(file,".").
IF i > 0 THEN ASSIGN
   ext  = substr(file,i)
   file = substr(file,1,i - 1).

/* move directly */
IF move THEN RUN pMoveJunk.
/* check + move IF needed */
ELSE         RUN pCheckJunk. 

PROCEDURE pCheckJunk.

   /* list filename */
   input through value("ls -l " + fname).

   IMPORT UNFORMATTED line.

   /* 'clean' line */
   line = fRemEmptyCol(line,sep).

   /* filesize in the 5:th position */
   fsize = int(entry(5,line,sep)).

   /* IF bigger that 2,1 GB */
   IF fsize > 2100000000 THEN RUN pMoveJunk.

END PROCEDURE.

PROCEDURE pMoveJunk.

   ASSIGN
      ctmp = string(day(today),"99")    + 
             string(month(today),"99")  + 
             string(year(today),"9999")
      file = file + ctmp + ext
      path = path + file.

   /* rename WITH DATE VALUE: only IF file doesn't already exist */
   if search(path) = ? then unix silent value("mv " + fname + " " + path).

END PROCEDURE.



