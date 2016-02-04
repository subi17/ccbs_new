/* errorcdr_delete_batch.p   04.01.11/aam divided from errocdr_delete.p
*/

{Syst/commpaa.i}
gcBrand = "1".
katun = "cron".

{Func/lib/eventlog.i}
{Func/direct_dbconnect.i}

DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO.
DEF VAR ldaRunFrom  AS DATE NO-UNDO EXTENT 2.
DEF VAR ldaRunTo    AS DATE NO-UNDO EXTENT 2.
DEF VAR liState     AS INT  NO-UNDO.
DEF VAR liRun       AS INT  NO-UNDO.
DEF VAR ldaDate     AS DATE NO-UNDO.
DEF VAR lcKeyValue  AS CHAR NO-UNDO.
DEF VAR liQty       AS INT  NO-UNDO.
DEF VAR liTotalQty  AS INT  NO-UNDO.

/* not on 1. of month */
IF DAY(TODAY) = 1 THEN QUIT.

fELog("DAILY","ErrorCDRDeleteStarted").

ASSIGN 
   ldaToDate   = TODAY - 120
   ldaFromDate = ?.
   
/* only certain errorcodes are deleted, so find out which day the last run
   reached and continue from there */
DO ldaDate = TODAY TO TODAY - 5 BY -1:

   lcKeyValue = STRING(YEAR(ldaDate),"9999") + 
                STRING(MONTH(ldaDate),"99") + 
                STRING(DAY(ldaDate),"99").
                
   FOR FIRST ActionLog NO-LOCK USE-INDEX TableName WHERE
             ActionLog.Brand     = gcBrand    AND
             ActionLog.TableName = "ErrorCDR" AND
             ActionLog.KeyValue  = lcKeyValue AND
             ActionLog.ActionID  = "DELETECDR" AND
             ActionLog.ActionStatus = 3:
      ldaFromDate = ActionLog.ToDate.
   END.

   IF ldaFromDate NE ? THEN LEAVE.
END.

IF ldaFromDate = ? THEN ldaFromDate = 6/9/10.


/* do two runs if db has been renewed during the period */
RUN pGetDBPeriods(gcBrand,
                  "ErrorCDR",
                  ldaFromDate,
                  ldaToDate,
                  OUTPUT ldaRunFrom[1],
                  OUTPUT ldaRunTo[1],
                  OUTPUT ldaRunFrom[2],
                  OUTPUT ldaRunTo[2]).
 
DO liRun = 1 TO 2:

   IF ldaRunFrom[liRun] = ? THEN NEXT.
   
   /* connect to correct cdr dbs */
   fInitializeConnectTables("ErrorCDR","").

   RUN pDirectConnect2Dbs(gcBrand,
                          "",
                          ldaRunTo[liRun],
                          ldaRunTo[liRun]).

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      fELog("DAILY","ErrorCDRDelete:" + RETURN-VALUE).
      QUIT.
   END.

   RUN Mm/errorcdr_delete.p (ldaRunFrom[liRun],
                          ldaRunTo[liRun],
                          OUTPUT liQty).

   liTotalQty = liTotalQty + liQty.
END.                        
                     
fELog("DAILY","ErrorCDRDeleteStopped:" + STRING(liTotalQty)).

