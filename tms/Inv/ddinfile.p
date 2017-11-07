/* ----------------------------------------------------------------------
  MODULE .......: ddinfile.p
  TASK .........: Read responses to a direct debit file csb19 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 25.01.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}          
{Func/cparam2.i}            
{Ar/ddtrans.i}

DEF INPUT  PARAMETER icFile  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead  AS INT  NO-UNDO. 

DEF VAR lcLine    AS CHAR NO-UNDO.
DEF VAR lcInvNum  AS CHAR NO-UNDO.
DEF VAR liInvNum  AS INT  NO-UNDO.
DEF VAR liCust    AS INT  NO-UNDO. 
DEF VAR liCancel  AS INT  NO-UNDO.
DEF VAR ldAmount  AS DEC  NO-UNDO.
DEF VAR liError   AS INT  NO-UNDO. 

DEF STREAM sRead.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

INPUT STREAM sRead FROM VALUE(icFile).

REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.
   
   IF SUBSTRING(lcLine,1,2) NE "xx" THEN NEXT.

   ASSIGN lcInvNum = SUBSTRING(lcLine,3,36)
          liCancel = INTEGER(SUBSTRING(lcLine,110,1))
          ldAmount = DECIMAL(SUBSTRING(lcLine,112,10)) / 100
          oiRead   = oiRead + 1
          liError  = 0 NO-ERROR. 
          
   IF ERROR-STATUS:ERROR THEN NEXT. 
   
END.

INPUT STREAM sRead CLOSE.

/* move to processed */
RUN pDDAuthTrans(icFile).

