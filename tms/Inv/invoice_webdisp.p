/* ----------------------------------------------------------------------
  MODULE .......: invoice_webdisp.p
  TASK .........: Set web display permit to invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 10.12.09
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER idaInvDate AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType  AS INT  NO-UNDO.
DEF INPUT  PARAMETER icInvGroup AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiCustNum1 AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum2 AS INT  NO-UNDO.
DEF INPUT  PARAMETER icInvID1   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icInvID2   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilDisplay  AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiMarked   AS INT  NO-UNDO.

DEF BUFFER bInv FOR Invoice.

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "Invoice"  
      ActionLog.ActionID     = "WebDisp"
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
         Invoice.Brand    = gcBrand    AND
         Invoice.InvDate  = idaInvDate AND
         Invoice.InvType  = iiInvType  AND
         Invoice.CustNum >= iiCustNum1 AND
         Invoice.CustNum <= iiCustNum2 AND
         Invoice.ExtInvID >= icInvID1  AND
         Invoice.ExtInvID <= icInvID2  AND
         Invoice.WInvDisp NE ilDisplay:

   IF ilDisplay AND Invoice.PrintState = 0 THEN NEXT.
   
   IF icInvGroup > "" AND Invoice.InvGroup NE icInvGroup THEN NEXT.
     
   DO FOR bInv TRANS:
      FIND bInv WHERE RECID(bInv) = RECID(Invoice) EXCLUSIVE-LOCK.
      ASSIGN 
         bInv.WInvDisp = ilDisplay
         oiMarked      = oiMarked + 1.
   END.
   
   IF NOT SESSION:BATCH AND oiMarked MOD 1000 = 0 THEN DO:
      PAUSE 0.
      DISP oiMarked LABEL "Marked" FORMAT ">>>>>>>>9" 
      WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " Invoices " FRAME fQty.
   END.

   IF oiMarked MOD 100000 EQ 0 THEN DO TRANS:
      FIND CURRENT ActionLog EXCLUSIVE-LOCK NO-ERROR.

      ASSIGN 
         ActionLog.ActionChar   = 'Web display permit marked as "' +
                                  STRING(ilDisplay,"allowed/denied") + '" to ' +
                                  STRING(oiMarked) + " invoices."
         ActionLog.ActionTS     = fMakeTS().
   END.
END.

IF oiMarked > 0 THEN DO TRANS:
   FIND CURRENT ActionLog EXCLUSIVE-LOCK NO-ERROR.

   ASSIGN    
      ActionLog.ActionStatus = 3
      ActionLog.ActionChar   = 'Web display permit marked as "' + 
                               STRING(ilDisplay,"allowed/denied") + '" to ' +
                               STRING(oiMarked) + " invoices."
      ActionLog.ActionTS     = fMakeTS().
         
   RELEASE ActionLog.   
END.
 
IF NOT SESSION:BATCH THEN HIDE FRAME fQty NO-PAUSE.

