/* ----------------------------------------------------------------------
MODULE .......: recalculate_istc_irc.p
TASK .........: Recalculates possible erroneous invrowcounters after iSTC
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 12.02.14
Version ......: yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
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
DEF VAR lcRunID AS CHAR NO-UNDO. 
   
IF iiDate EQ ? OR DAY(iiDate) EQ 1 THEN RETURN.

ASSIGN
   ldaPeriodFrom = DATE(MONTH(iiDate),1,YEAR(iiDate))
   ldaPeriodEnd = Func.Common:mLastDayOfMonth(iiDate).

IF icMode EQ "month" THEN ASSIGN
   ldeFrom = YEAR(iiDate) * 10000 + MONTH(iiDate) * 100 + 2
   ldeTo   = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(iiDate) - 1,86399)
   lcActionID = STRING(YEAR(iiDate) * 100 + 
                MONTH(iiDate)). 
ELSE ASSIGN
   ldeFrom = Func.Common:mMake2DT(iiDate,0)
   ldeTo   = Func.Common:mMake2DT(iiDate,86399)
   lcActionID = STRING(YEAR(iiDate) * 10000 + 
                MONTH(iiDate) * 100 +
                DAY(iiDate)).

IF CAN-FIND (FIRST ActionLog NO-LOCK WHERE
                   ActionLog.Brand = Syst.CUICommon:gcBrand AND
                   ActionLog.TableName = "Cron" AND
                   ActionLog.KeyValue BEGINS lcActionID AND
                   ActionLog.ActionID = "ISTC_IRC_CHK" AND
                   ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE})
   THEN RETURN.

DO TRANS:
   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand        = Syst.CUICommon:gcBrand
      ActionLog.ActionID     = "ISTC_IRC_CHK"
      ActionLog.ActionTS     = Func.Common:mMakeTS()
      ActionLog.TableName    = "Cron"
      ActionLog.KeyValue     = lcActionID
      ActionLog.UserCode     = Syst.CUICommon:katun
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_ACTIVE}
      ActionLog.ActionPeriod = YEAR(iiDate) * 100 + MONTH(iiDate)
      ActionLog.ActionStatus = 0.
END.

FOR EACH msrequest NO-LOCK WHERE
         msrequest.brand = Syst.CUICommon:gcBrand AND
         msrequest.reqtype = 0 AND
         msrequest.reqstatus = 2 and
         msrequest.actstamp >= ldeFrom and
         msrequest.actstamp < ldeTo:

   IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.CLIType EQ msrequest.reqcparam1 AND
                     CLIType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID})
      AND
      CAN-FIND(FIRST CLIType NO-LOCK WHERE
                     CLIType.CLIType EQ msrequest.reqcparam2 AND
                     CLIType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID}) THEN NEXT.

   Func.Common:mTS2Date(msrequest.actstamp, output ldaSTCDate).
   
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

IF CAN-FIND(FIRST ttSubs) THEN DO:

   lcRunID = replace(string(time,"HH:MM:SS"),":","") + "_istc".

   RUN Inv/chk_cdr_invrowcounter.p(INPUT TABLE ttSubs BY-REFERENCE,
                               lcRunID,
                               ldaPeriodEnd,
                               0, /* fr process id */
                               0, /* fr update interval */
                               OUTPUT liCounterCnt).
END.

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
      ActionLog.ActionChar   = "Finished at " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) + CHR(10) + 
                             SUBST("Checked &1, Errors &2", liChecked,liErrors) 
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}.
   RELEASE ActionLog.
END.

FINALLY:
   EMPTY TEMP-TABLE ttSubs.
END.
