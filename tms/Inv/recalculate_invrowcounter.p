/* rerecalculate_invrowcounter.p   05.11.10/aam

   recalculate unbilled invoice row counters;
   - delete old counters
   - get cdrs of unbilled invseqs 
   - create entries to TMQueue from cdrs
*/

{Syst/commali.i}
{Func/callquery.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER iiInvCust   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiMsSeq     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiInvSeq    AS INT  NO-UNDO.
DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
DEF OUTPUT PARAMETER oiToQueue   AS INT  NO-UNDO.

DEF TEMP-TABLE ttInvSeq NO-UNDO 
   FIELD InvSeq AS INT
   FIELD InvCust AS INT.
   
DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable AS CHAR.

DEF VAR liDeleted AS INT  NO-UNDO.
 

/***** Main start *******/

RUN pGetInvSeqs.

RUN pCollectCDRs.
IF RETURN-VALUE BEGINS "ERROR" THEN
   RETURN RETURN-VALUE.

RUN pDeleteCurrentCounters.

RUN pEntriesToQueue.

RUN pLogAction.

RETURN "".

/***** Main end ********/


PROCEDURE pGetInvSeqs:

   EMPTY TEMP-TABLE ttInvSeq. 

   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.CustNum = iiInvCust AND
            InvSeq.ToDate   >= idaFromDate AND
            InvSeq.FromDate <= idaToDate AND
            InvSeq.Billed = FALSE AND
            (IF iiMsSeq > 0 
             THEN InvSeq.MsSeq = iiMsSeq
             ELSE TRUE)           AND
            (IF iiInvSeq > 0
             THEN InvSeq.InvSeq = iiInvSeq
             ELSE TRUE):
      CREATE ttInvSeq.
      ASSIGN 
         ttInvSeq.InvSeq = InvSeq.InvSeq
         ttInvSeq.InvCust = InvSeq.CustNum.
   END.

END PROCEDURE.

PROCEDURE pDeleteCurrentCounters:
      
   FOR EACH ttInvSeq,
       EACH InvRowCounter EXCLUSIVE-LOCK USE-INDEX InvCust WHERE
            InvRowCounter.InvCust = ttInvSeq.InvCust AND
            InvRowCounter.InvSeq  = ttInvSeq.InvSeq AND
            InvRowCounter.InvNum  = 0:
               
      liDeleted = liDeleted + 1.
      DELETE InvRowCounter.
   END.           

END PROCEDURE.

PROCEDURE pCollectCDRs:

   DEF VAR liErrorCodeOut AS INT  NO-UNDO.
   DEF VAR tthCDR AS HANDLE NO-UNDO.

   tthCDR = TEMP-TABLE ttCall:HANDLE.

   EMPTY TEMP-TABLE ttCall.
  
   FOR EACH ttInvSeq,
      FIRST InvSeq WHERE
            InvSeq.InvSeq = ttInvSeq.InvSeq:

      fMobCDRCollect(INPUT "post",
                     INPUT gcBrand,
                     INPUT katun,
                     INPUT InvSeq.FromDate,   
                     INPUT InvSeq.ToDate,
                     INPUT InvSeq.CustNum,
                     INPUT "inv",
                     INPUT "",
                     INPUT InvSeq.InvSeq,
                     INPUT 0,
                     INPUT "",
                     INPUT "",
                     INPUT "",
                     INPUT 0,
                     INPUT-OUTPUT liErrorCodeOut,
                     INPUT-OUTPUT tthCDR).
      IF liErrorCodeOut > 0 THEN
         RETURN "ERROR:EDR collection failed on error " + 
                STRING(liErrorCodeOut).
   END.
   
   RETURN "".
   
END PROCEDURE.

PROCEDURE pEntriesToQueue:

   FOR EACH ttCall:
   
      CREATE TMQueue.
      BUFFER-COPY ttCall TO TMQueue.
         
      ASSIGN
         TMQueue.AccumTarget = "InvRow"
         TMQueue.Qty         = 1
         TMQueue.EventID     = ttCall.DtlSeq
         TMQueue.Source      = ttCall.MSCID
         TMQueue.PayType     = 1 + INT(ttCall.PPFlag > 0)
         TMQueue.ReportingID = ttCall.ServRid + "," + ttCall.MPMRid
         TMQueue.ExtraAmount = ttCall.MPMAmt
         oiToQueue           = oiToQueue + 1.                         

      IF ttCall.PPFlag > 0 THEN 
         TMQueue.Amount = ttCall.Charge.
         
      RELEASE TMQueue.
   END.

END PROCEDURE.
             
PROCEDURE pLogAction:

   IF liDeleted > 0 OR oiToQueue > 0 THEN DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "InvRowCounter"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99") + 
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "Recalculate"
         ActionLog.ActionPeriod = YEAR(idaToDate) * 100 + MONTH(idaToDate)
         ActionLog.FromDate     = idaFromDate
         ActionLog.ToDate       = idaToDate
         ActionLog.ActionDec    = 0
         ActionLog.ActionChar   = STRING(liDeleted) + 
                                  " old counters were removed," + CHR(10) +
                                  STRING(oiToQueue) + 
                                  " TMQueue entries were added for" +
                                  " recalculation" + CHR(10) +
                                  "InvSeq: " + STRING(iiInvSeq) + ", " + 
                                  "InvCust: " + STRING(iiInvCust) + ", " + 
                                  "MsSeq: " + STRING(iiMsSeq)
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().
   END.
   
END PROCEDURE. 
   
        
