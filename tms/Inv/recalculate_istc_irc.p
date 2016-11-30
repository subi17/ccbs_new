/* ----------------------------------------------------------------------
MODULE .......: recalculate_istc_irc.p
TASK .........: Recalculates possible erroneous invrowcounters after iSTC
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 12.02.14
Version ......: yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/date.i}
{Func/istc.i}
{Syst/tmsconst.i}
{Inv/chk_cdr_invrowcounter.i}

DEFINE INPUT PARAMETER iiDate AS DATE NO-UNDO. 
DEFINE INPUT PARAMETER icMode AS CHAR NO-UNDO. 

DEFINE VARIABLE ldeFrom AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeTo AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldaPeriodFrom AS DATE NO-UNDO. 
DEFINE VARIABLE ldaPeriodEnd AS DATE NO-UNDO. 
DEFINE VARIABLE ldaSTCDate AS DATE NO-UNDO. 
DEFINE VARIABLE liToQueue AS INTEGER NO-UNDO. 
DEFINE VARIABLE liChecked AS INTEGER NO-UNDO. 
DEFINE VARIABLE liErrors AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCounterCnt AS INTEGER NO-UNDO. 
DEFINE VARIABLE liOrder AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcActionID AS CHARACTER NO-UNDO.
   
IF iiDate EQ ? OR DAY(iiDate) EQ 1 THEN RETURN.

ASSIGN
   ldaPeriodFrom = DATE(MONTH(iiDate),1,YEAR(iiDate))
   ldaPeriodEnd = fLastDayOfMonth(iiDate).

IF icMode EQ "month" THEN ASSIGN
   ldeFrom = YEAR(iiDate) * 10000 + MONTH(iiDate) * 100 + 2
   ldeTo   = fMake2Dt(fLastDayOfMonth(iiDate) - 1,86399)
   lcActionID = STRING(YEAR(iiDate) * 100 + 
                MONTH(iiDate)). 
ELSE ASSIGN
   ldeFrom = fMake2Dt(iiDate,0)
   ldeTo   = fMake2Dt(iiDate,86399)
   lcActionID = STRING(YEAR(iiDate) * 10000 + 
                MONTH(iiDate) * 100 +
                DAY(iiDate)).

IF CAN-FIND (FIRST ActionLog NO-LOCK WHERE
                   ActionLog.Brand = gcBrand AND
                   ActionLog.TableName = "Cron" AND
                   ActionLog.KeyValue BEGINS lcActionID AND
                   ActionLog.ActionID = "ISTC_IRC_CHK" AND
                   ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE})
   THEN RETURN.

DO TRANS:
   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand        = gcBrand
      ActionLog.ActionID     = "ISTC_IRC_CHK"
      ActionLog.ActionTS     = fMakeTS()
      ActionLog.TableName    = "Cron"
      ActionLog.KeyValue     = lcActionID
      ActionLog.UserCode     = katun
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
      ActionLog.ActionPeriod = YEAR(iiDate) * 100 + MONTH(iiDate)
      ActionLog.ActionStatus = 0.
END.

FOR EACH msrequest NO-LOCK WHERE
         msrequest.brand = gcBrand AND
         msrequest.reqtype = 0 AND
         msrequest.reqstatus = 2 and
         msrequest.actstamp >= ldeFrom and
         msrequest.actstamp < ldeTo:

   IF msrequest.reqcparam1 BEGINS "TARJ" AND
      msrequest.reqcparam2 BEGINS "TARJ" THEN NEXT.

   fTS2Date(msrequest.actstamp, output ldaSTCDate).
   
   IF fGetISTCDate(msrequest.msseq,
                   msrequest.custnum,
                   ldaSTCDate) EQ ? THEN NEXT.
   
   liOrder = liOrder + 1.

   CREATE ttSubs.
   ASSIGN
      ttSubs.MsSeq   = msrequest.msseq
      ttSubs.InvCust = msrequest.custnum
      ttSubs.Order   = liOrder.
END.

IF CAN-FIND(FIRST ttSubs) THEN
   RUN Inv/chk_cdr_invrowcounter.p(INPUT TABLE ttSubs BY-REFERENCE,
                               0, /* fr run id */
                               ldaPeriodEnd,
                               0, /* fr process id */
                               0, /* fr update interval */
                               OUTPUT liCounterCnt).

FOR EACH ttSubs:

   IF ttSubs.ErrorFound THEN DO:
      
      RUN Inv/recalculate_invrowcounter.p(
         ttSubs.InvCust,
         ttSubs.msseq,
         0, /* invseq */
         ldaPeriodFrom,
         ldaPeriodEnd,
         OUTPUT liToQueue).

      liErrors = liErrors + 1.
   END.

   liChecked = liChecked + 1.
END.

   
DO TRANS:
   ASSIGN
      ActionLog.ActionDec    = liErrors
      ActionLog.ActionChar   = "Finished at " + fTS2HMS(fMakeTS()) + CHR(10) + 
                             SUBST("Checked &1, Errors &2", liChecked,liErrors) 
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}.
   RELEASE ActionLog.
END.

FINALLY:
   EMPTY TEMP-TABLE ttSubs.
END.
