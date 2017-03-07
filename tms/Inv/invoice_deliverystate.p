/* ----------------------------------------------------------------------
  MODULE .......: invoice_deliverystate.p
  TASK .........: Set delivery state to invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 07.04.09
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/timestamp.i}
{Syst/funcrunprocess_update.i}

DEF INPUT  PARAMETER idaInvDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType        AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiState          AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER idaInvDueDate    AS DATE NO-UNDO. 
DEF OUTPUT PARAMETER oiMarked         AS INT  NO-UNDO.

DEF BUFFER bInv FOR Invoice. 

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "Invoice"  
      ActionLog.ActionID     = "DelState"
      ActionLog.KeyValue     = STRING(YEAR(idaInvDate),"9999") + 
                               STRING(MONTH(idaInvDate),"99") + 
                               STRING(DAY(idaInvDate),"99")
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionDec    = iiInvType
      ActionLog.UserCode     = katun
      ActionLog.ActionStatus = 0.

  FIND CURRENT ActionLog NO-LOCK.
END.

FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand   = gcBrand    AND
         Invoice.InvDate = idaInvDate AND
         Invoice.InvType = iiInvType  AND
         Invoice.DeliveryState = 0:

   IF idaInvDueDate NE ? AND Invoice.DueDate NE idaInvDueDate THEN NEXT.

   DO FOR bInv TRANS:
      FIND bInv WHERE RECID(bInv) = RECID(Invoice) EXCLUSIVE-LOCK.
      ASSIGN 
         bInv.DeliveryState = iiState
         oiMarked           = oiMarked + 1.
   END.

   IF iiUpdateInterval > 0 AND oiMarked MOD iiUpdateInterval = 0 THEN DO:
      IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiMarked) THEN
      RETURN "ERROR:Stopped".
   END. /* IF iiUpdateInterval > 0 AND oiHandled */

   IF NOT SESSION:BATCH AND oiMarked MOD 1000 = 0 THEN DO:
      PAUSE 0.
      DISP oiMarked LABEL "Marked" FORMAT ">>>>>>>>9" 
      WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " Invoices " FRAME fQty.
   END.

   IF oiMarked MOD 100000 EQ 0 THEN DO TRANS:
      FIND CURRENT ActionLog EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN 
         ActionLog.ActionChar   = "Delivery state " + STRING(iiState) +
                                  " marked to " + STRING(oiMarked) +
                                  " invoices."
         ActionLog.ActionTS     = fMakeTS().
   END.

END.

IF oiMarked > 0 THEN DO TRANS:
   FIND CURRENT ActionLog EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN
      ActionLog.ActionStatus = 3
      ActionLog.ActionChar   = "Delivery state " + STRING(iiState) +
                               " marked to " + STRING(oiMarked) + 
                               " invoices."
      ActionLog.ActionTS     = fMakeTS().
         
   RELEASE ActionLog.   
END.
 
IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.

