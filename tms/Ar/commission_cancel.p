/* ----------------------------------------------------------------------
  MODULE .......: commission_cancel.p
  TASK .........: Cancel an already activated commission
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 06.03.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/barrfunc.i}

DEF OUTPUT PARAMETER oiChecked   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiCancelled AS INT  NO-UNDO.
 
DEF VAR lcDebtBarring AS CHAR NO-UNDO.
 

/***** MAIN start *******/

/* barrings that are used for debt collection, should not be on */
lcDebtBarring = {&FRAUD_BARR_CODES}.
 
RUN pCancelCommission (OUTPUT oiChecked,
                       OUTPUT oiCancelled).

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "CoTarg"  
      ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                               STRING(MONTH(TODAY),"99") + 
                               STRING(DAY(TODAY),"99")
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "COMMCANCEL"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionDec    = oiCancelled
      ActionLog.ActionChar   = "Handled: " + STRING(oiChecked) + CHR(10) + 
                               " Cancelled: " + STRING(oiCancelled)
      ActionLog.ActionStatus = 3.
      ActionLog.ActionTS     = fMakeTS().
END.

RETURN RETURN-VALUE.

/******MAIN end ***********/


PROCEDURE pCancelCommission:

   DEF OUTPUT PARAMETER oiHandled   AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiCancelled AS INT  NO-UNDO.
   
   DEF VAR lcBarring AS CHAR NO-UNDO.
   DEF VAR liDone    AS INT  NO-UNDO. 
   DEF VAR liCount   AS INT NO-UNDO.
   DEF VAR llOngoing AS LOG NO-UNDO. 
   DEF VAR lrBarring  AS ROWID NO-UNDO.

   FORM 
      oiHandled   AT 2 FORMAT ">>>>>>>9" LABEL "Handled ." SKIP
      oiCancelled AT 2 FORMAT ">>>>>>>9" LABEL "Cancelled" SKIP
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " CANCEL COMMISSION "
      FRAME fQty.

   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      VIEW FRAME fQty.
   END.
   
   CommissionCancel:
   FOR EACH CoTarg NO-LOCK USE-INDEX CommStatus WHERE
            CoTarg.Brand      = gcBrand AND
            CoTarg.CommStatus = 2       AND
            CoTarg.TargType = "M",
      FIRST FATime NO-LOCK USE-INDEX HostTable WHERE
            FATime.Brand     = gcBrand  AND
            FATime.HostTable = "CoTarg" AND
            FATime.KeyValue  = STRING(CoTarg.CoTargID) AND
            FATime.InvNum    = 0,
      FIRST MobSub NO-LOCK WHERE 
            MobSub.MsSeq = INTEGER(CoTarg.CoTarg),
      FIRST CoRule NO-LOCK WHERE
            CoRule.Brand    = gcBrand AND
            CoRule.CoRuleID = CoTarg.CoRuleID AND
            CoRule.RuleType = 2:   /* 2=referee */
            
      oiHandled = oiHandled + 1.

      IF NOT SESSION:BATCH AND 
         (oiHandled < 100 OR oiHandled MOD 100 = 0) 
      THEN DO:
         PAUSE 0.
         DISP oiHandled oiCancelled WITH FRAME fQty.
      END.

      llOngoing = fCheckBarrStatus(MobSub.MsSeq, OUTPUT lcBarring, OUTPUT lrBarring).
      IF llOngoing THEN NEXT.

      /*check all barrings, that are they in list*/
      IF fIsInList(lcDebtBarring, lcBarring) EQ TRUE THEN DO:
         RUN commission_term(MobSub.MsSeq,
                             "Debt",
                             OUTPUT liDone).

         IF liDone > 0 THEN 
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "COTarg",
                          STRING(CoTarg.CoTargID),
                          MobSub.InvCust,
                          "FATime Cancelled",
                          "Commission cancelled for period " +
                             STRING(YEAR(TODAY),"9999") +
                             STRING(MONTH(TODAY),"99") +
                             " due to unpaid bills.").

         oiCancelled = oiCancelled + liDone.
      END.
    
   END.   

   IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.     

END PROCEDURE.

