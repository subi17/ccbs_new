/* remove_old_events.p   03.04.12/aam

   Remove old unbilled events from billing
*/

{commali.i}
{cparam2.i}
{timestamp.i}
{eventval.i} 

DEF INPUT  PARAMETER iiInvCust    AS INT  NO-UNDO.
DEF INPUT  PARAMETER idaEventDate AS DATE NO-UNDO.
DEF INPUT  PARAMETER ilCDRs       AS LOG  NO-UNDO.
DEF INPUT  PARAMETER ilFees       AS LOG  NO-UNDO.
DEF INPUT  PARAMETER ilFATimes    AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiEvents     AS INT  NO-UNDO.

DEF VAR liCDRs      AS INT  NO-UNDO.
DEF VAR liFFees     AS INT  NO-UNDO.
DEF VAR liSFees     AS INT  NO-UNDO. 
DEF VAR liFATimes   AS INT  NO-UNDO.
DEF VAR liErrorCode AS INT  NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO.
DEF VAR lcSep       AS CHAR NO-UNDO INIT "|".

DEF STREAM sLog.

IF (ilFees OR ilFATimes) AND llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEF VAR lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).

   DEF VAR lhFFItem AS HANDLE NO-UNDO.
   lhFFItem = BUFFER FFITem:HANDLE.
   RUN StarEventInitialize(lhFFItem).

   DEF VAR lhFATime AS HANDLE NO-UNDO.
   lhFATime = BUFFER FATime:HANDLE.
   RUN StarEventInitialize(lhFATime).
END.


FORM
   liCDRs COLON 15 
      LABEL "CDRs"
      FORMAT ">>>>>>>>>>9" 
      SKIP
   liSFees COLON 15
      LABEL "Single Fees"
      FORMAT ">>>>>>>>>>9" 
      SKIP
   liFFees COLON 15
      LABEL "Fixed Fees"
      FORMAT ">>>>>>>>>>9" 
      SKIP
   liFatimes COLON 15
      LABEL "FATimes"
      FORMAT ">>>>>>>>>>9" 
      SKIP
   WITH OVERLAY SIDE-LABELS ROW 10 CENTERED TITLE " Events " FRAME fQty.   
      


/***** Main start *******/

IF ilCDRS THEN DO:
   liErrorCode = fCParamI("OldUnbilledCDRError").
   IF liErrorCode = ? OR liErrorCode = 0 THEN 
      RETURN "ERROR:Errorcode for CDRs has not been defined".
      
   RUN pRemoveCDRs.
END.

IF ilFees OR ilFATimes THEN DO:
   lcLogFile = fCParamC("OldUnbilledLogFile").
   IF lcLogFile = ? OR lcLogFile = "" THEN 
      RETURN "ERROR:Log file has not been defined".
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(idaEventDate),"9999") +
                                         STRING(MONTH(idaEventDate),"99") + 
                                         STRING(DAY(idaEventDate),"99")).
END.

IF ilFees THEN 
   RUN pRemoveFees.

IF ilFATimes THEN
   RUN pRemoveFATimes.
   
oiEvents = liCDRs + liSFees + liFFees + liFATimes.

RUN pLogAction.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
RETURN "".

FINALLY:
   fCleanEventObjects().
END.

/***** Main end ********/


PROCEDURE pRemoveCDRs:

   IF iiInvCust > 0 THEN 
   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.CustNum = iiInvCust AND
            InvSeq.ToDate <= idaEventDate AND
            InvSeq.Billed = FALSE:
      RUN pMoveToErrorCode(InvSeq.CustNum,
                           InvSeq.InvSeq).
   END.

   ELSE 
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.PayType = FALSE
   BREAK BY MsOwner.MsSeq 
         BY MsOwner.InvCust:
         
      IF FIRST-OF(MsOwner.InvCust) THEN 
      FOR EACH InvSeq NO-LOCK WHERE
               InvSeq.MsSeq = MsOwner.MsSeq AND
               InvSeq.CustNum = MsOwner.InvCust AND
               InvSeq.Billed = FALSE AND
               InvSeq.ToDate <= idaEventDate:
         RUN pMoveToErrorCode(InvSeq.CustNum,
                              InvSeq.InvSeq).
         IF NOT SESSION:BATCH THEN DO:
            PAUSE 0.
            DISP liCDRs WITH FRAME fQty.
         END.
      END.
   END.

END PROCEDURE.

PROCEDURE pMoveToErrorCode:

   DEF INPUT PARAMETER iiInvCust AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiInvSeq  AS INT  NO-UNDO.
   
   DEF VAR lcQueueTarget AS CHAR NO-UNDO.
   
   /* all billable CDRs are in the latest DB */
   FOR EACH MobCDR EXCLUSIVE-LOCK WHERE
            MobCDR.InvCust = iiInvCust AND
            MobCDR.InvSeq  = iiInvSeq:

      /* do not update fraud counters from old events */
      IF YEAR(MobCDR.DateSt) < YEAR(TODAY) OR
         MONTH(MobCDR.DateSt) < MONTH(TODAY) THEN 
         lcQueueTarget = "InvRow".
      ELSE lcQueueTarget = "".   
         
      CREATE TMQueue.
      BUFFER-COPY MobCDR TO TMQueue.
      ASSIGN
         TMQueue.Qty     = -1
         TMQueue.EventID = MobCDR.DtlSeq
         TMQueue.AgrCust = 0
         TmQueue.Source  = MobCDR.MSCID
         TmQueue.PayType = 1 + INT(MobCDR.PPFlag > 0)
         TMQueue.ReportingID = MobCDR.ServRid + "," + MobCDR.MPMRid
         TMQueue.ExtraAmount = MobCDR.MPMAmt
         TMQueue.AccumTarget = lcQueueTarget.
      RELEASE TMQueue.
      
      ASSIGN
         MobCDR.ErrorCode = liErrorCode
         MobCDR.InvSeq = 0
         liCDRs = liCDRs + 1.
   END.

END PROCEDURE.

PROCEDURE pRemoveFees:

   DEF VAR liBillPeriod AS INT  NO-UNDO.
   DEF VAR liCustNum1   AS INT  NO-UNDO.
   DEF VAR liCustNum2   AS INT  NO-UNDO.
   
   DEF BUFFER bSingleFee FOR SingleFee.
   DEF BUFFER bFFItem FOR FFItem.
   
   IF iiInvCust > 0 THEN ASSIGN
      liCustNum1 = iiInvCust
      liCustNum2 = iiInvCust.
   ELSE ASSIGN
      liCustNum2 = 99999999.
    
   liBillPeriod = YEAR(idaEventDate) * 100 + MONTH(idaEventDate).
   
   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.
    
   FOR EACH Customer NO-LOCK WHERE
       Customer.CustNum >= liCustNum1 AND
       Customer.CustNum <= liCustNum2:
   
      IF ilFees THEN DO:
      
         FOR EACH bSingleFee NO-LOCK WHERE
                  bSingleFee.CustNum = Customer.CustNum AND
                  bSingleFee.BillPeriod <= liBillPeriod AND
                  bSingleFee.Billed = FALSE AND
                  bSingleFee.Active = TRUE:
                  
            PUT STREAM sLog UNFORMATTED 
               "SingleFee" lcSep
               bSingleFee.CustNum lcSep     
               bSingleFee.BillCode lcSep
               bSingleFee.FeeModel lcSep
               bSingleFee.CalcObj  lcSep
               bSingleFee.HostTable lcSep
               bSingleFee.KeyValue  lcSep
               bSingleFee.BillPeriod lcSep
               bSingleFee.Amt SKIP.
                
            FIND FIRST SingleFee WHERE RECID(SingleFee) = RECID(bSingleFee)
               EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventMakeDeleteEventWithMemo(
                                       lhSingleFee,
                                       katun,
                                       "RemoveOldEvents").
            DELETE SingleFee.
            
            liSFees = liSFees + 1.

            IF NOT SESSION:BATCH AND liSFees MOD 10 = 0 THEN DO:
               PAUSE 0.
               DISP liSFees WITH FRAME fQty.
            END.
         END.

         FOR EACH FixedFee NO-LOCK WHERE
                  FixedFee.Brand = gcBrand AND
                  FixedFee.CustNum = Customer.CustNum AND
                  FixedFee.InUse = TRUE,
             EACH bFFItem OF FixedFee NO-LOCK WHERE
                  bFFItem.BillPeriod <= liBillPeriod AND
                  bFFItem.Billed = FALSE:
                  
            PUT STREAM sLog UNFORMATTED 
               "FixedFee" lcSep
               FixedFee.CustNum lcSep     
               FixedFee.BillCode lcSep
               FixedFee.FeeModel lcSep
               FixedFee.CalcObj  lcSep
               FixedFee.HostTable lcSep
               FixedFee.KeyValue  lcSep
               bFFItem.BillPeriod lcSep
               ROUND(bFFItem.Amt,2) SKIP.

            FIND FIRST FFItem WHERE RECID(FFItem) = RECID(bFFItem)
               EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFFItem).
            
            DELETE FFItem.
            
            liFFees = liFFees + 1.

            IF NOT SESSION:BATCH AND liFFees MOD 10 = 0 THEN DO:
               PAUSE 0.
               DISP liFFees WITH FRAME fQty.
            END.
         END.
      
      END.
   END.

   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      DISP liSFees liFFees WITH FRAME fQty.
   END.
   
   OUTPUT STREAM sLog CLOSE.
      
END PROCEDURE.

PROCEDURE pRemoveFATimes:

   DEF VAR liPeriod AS INT  NO-UNDO.
   
   DEF BUFFER bFATime FOR FATime.
   
    
   liPeriod = YEAR(idaEventDate) * 100 + MONTH(idaEventDate).
   
   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.
   
   FOR EACH bFATime NO-LOCK WHERE
            bFATime.InvNum = 0 AND
            bFATime.Period <= liPeriod:

      IF iiInvCust > 0 AND bFATime.CustNum NE iiInvCust THEN NEXT. 
      IF bFatime.TransPeriod > liPeriod THEN NEXT. 
      
      PUT STREAM sLog UNFORMATTED 
         "FATime" lcSep
         bFATime.CustNum lcSep     
         bFatime.TransPeriod lcSep
         bFATime.FTGrp lcSep
         ""  lcSep
         bFATime.CLI lcSep
         bFATime.MsSeq  lcSep
         bFATime.Period lcSep
         bFATime.Amt SKIP.

      FIND FIRST FATime WHERE RECID(FATime) = RECID(bFATime) EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFATime).
      DELETE FATime.

      liFATimes = liFATimes + 1.

      IF NOT SESSION:BATCH THEN DO:
         PAUSE 0.
         DISP liFATimes WITH FRAME fQty.
      END.
   END.
   
   OUTPUT STREAM sLog CLOSE.
      
END PROCEDURE.
 
PROCEDURE pLogAction:

   DEF VAR lcResult AS CHAR NO-UNDO. 

   IF oiEvents > 0 THEN DO TRANS:

      IF ilCDRS THEN lcResult = STRING(liCDRS) + " CDRs". 
      IF ilFees THEN lcResult = lcResult + 
                                (IF lcResult > "" THEN ", " ELSE "") +
                                STRING(liSFees) + " single fees, " +
                                STRING(liFFees) + " fixed fees".
      IF ilFATimes THEN lcResult = lcResult +                           
                                   (IF lcResult > "" THEN ", " ELSE "") +
                                   STRING(liFATimes) + " FATimes".
      lcResult = lcResult + " were removed from billing".
  
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "BillEvents"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99") + 
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "RemoveUnbilled"
         ActionLog.ActionPeriod = YEAR(idaEventDate) * 100 + 
                                  MONTH(idaEventDate)
         ActionLog.FromDate     = idaEventDate
         ActionLog.ToDate       = idaEventDate
         ActionLog.ActionDec    = 0
         ActionLog.ActionChar   = lcResult
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().
   END.
   
END PROCEDURE. 
   
        
