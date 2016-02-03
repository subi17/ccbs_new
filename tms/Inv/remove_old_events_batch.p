/* -----------------------------------------------
  MODULE .......: remove_old_events_batch.p
  FUNCTION .....: Remove old unbilled events from billing (batch)
  AUTHOR .......: aam
  CREATED ......: 03.04.12
------------------------------------------------------ */

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun = "cron".
{Func/lib/eventlog.i}
{Inv/old_unbilled_events.i}

DEF VAR liEvents     AS INT  NO-UNDO.
DEF VAR ldaEventDate AS DATE NO-UNDO.


ldaEventDate = fOldUnbilledEventLimit(0).

fELog("REMOVE_OLD_UNBILLED_EVENTS","Started:Limit" +
                                   STRING(ldaEventDate,"99-99-9999")).

RUN remove_old_events.p(0,
                        ldaEventDate,
                        TRUE,
                        TRUE,
                        TRUE,
                        OUTPUT liEvents).
 
IF RETURN-VALUE BEGINS "ERROR" THEN DO TRANS:
   CREATE ErrorLog.
   ASSIGN 
      ErrorLog.Brand     = gcBrand
      ErrorLog.ActionID  = "RemoveOld"
      ErrorLog.TableName = "BillEvents"
      ErrorLog.KeyValue  = STRING(YEAR(ldaEventDate),"9999") + 
                           STRING(MONTH(ldaEventDate),"99") + 
                           STRING(DAY(ldaEventDate),"99")
      ErrorLog.ErrorMsg  = RETURN-VALUE
      ErrorLog.ErrorChar = ""
      ErrorLog.UserCode  = katun.
      ErrorLog.ActionTS  = fMakeTS().
END.

fELog("REMOVE_OLD_UNBILLED_EVENTS","Stopped:" + STRING(liEvents)).

QUIT.



