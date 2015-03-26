/* ----------------------------------------------------------------------------
  MODULE .......: invrowcounter_unbilled_split.p
  FUNCTION .....: Split subscriptions for unbilled invrowcounter check
  CREATED ......: 08.11.12/aam
  --------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{timestamp.i}
{funcrunprocess_update.i}
{date.i}
{funcrun_replica.i}

DEF INPUT  PARAMETER iiBatchQty      AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRExecID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUseReplica    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiSubsCnt       AS INT  NO-UNDO. 

DEF VAR liTotal       AS INT  NO-UNDO.
DEF VAR liCallQty     AS INT  NO-UNDO.
DEF VAR lcReplica     AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttSubs NO-UNDO
   FIELD MsSeq    AS INT
   FIELD InvCust  AS INT
   FIELD CallQty  AS INT
   INDEX CallQty CallQty DESC.


/***** Main start ********/

RUN pInitialize.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pCollectSubscriptions.
IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.

RUN pDivideIntoGroups.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/****** Main end *****/


PROCEDURE pInitialize:

   IF iiBatchQty = 0 THEN DO:
      RETURN "ERROR:Nothing to do".
   END.

   IF iiUseReplica > 0 THEN DO:
      lcReplica = fInitReplicaSettings(iiUseReplica,
                                       INPUT-OUTPUT iiBatchQty).
      IF lcReplica BEGINS "ERROR" THEN RETURN lcReplica.
   END.
      
   RETURN "".   
   
END PROCEDURE.

PROCEDURE pCollectSubscriptions:

   DEF VAR liPeriod AS INT  NO-UNDO.
   DEF VAR ldaDate  AS DATE NO-UNDO.
   
   ASSIGN 
      ldaDate = fLastDayOfMonth(TODAY)
      liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).

   MsOwnerSelect:
   FOR EACH MsOwner NO-LOCK USE-INDEX MsSeq WHERE 
            MsOwner.PayType = FALSE
   BREAK BY MsOwner.MsSeq
         BY MsOwner.InvCust:
         
      IF NOT FIRST-OF(MsOwner.InvCust) THEN NEXT.
 
      liCallQty = 0.
      FOR EACH SaldoCounter NO-LOCK WHERE
               SaldoCounter.MsSeq  = MsOwner.MsSeq AND
               SaldoCounter.Period = liPeriod AND
               SaldoCounter.Qty > 0:
         liCallQty = liCallQty + SaldoCounter.Qty.
      END.
      
      IF liCallQty = 0 THEN
      FOR FIRST InvSeq NO-LOCK WHERE
                InvSeq.MsSeq = MsOwner.MsSeq AND
                InvSeq.CustNum = MsOwner.InvCust AND
                InvSeq.Billed = FALSE AND
                InvSeq.ToDate = ldaDate:
         liCallQty = 1.       
      END.

      IF liCallQty = 0 THEN NEXT. 

      CREATE ttSubs.
      ASSIGN 
         ttSubs.MsSeq = MsOwner.MsSeq
         ttSubs.InvCust = MsOwner.InvCust
         ttSubs.CallQty = liCallQty
         liTotal = liTotal + liCallQty.

      oiSubsCnt = oiSubsCnt + 1.
      IF NOT SESSION:BATCH AND oiSubsCnt MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP oiSubsCnt COLUMN-LABEL "MsOwner Qty" 
            WITH 1 DOWN ROW 8 CENTERED TITLE " Collecting " 
            OVERLAY FRAME fQty.
      END.

      IF iiUpdInterval > 0 AND oiSubsCnt MOD iiUpdInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiSubsCnt) THEN
            RETURN "ERROR:Stopped".
      END.   
   END.

   RETURN "".
   
END PROCEDURE.
 
PROCEDURE pDivideIntoGroups: 

   DEF VAR liLimit       AS INT  NO-UNDO.
   DEF VAR liFeedOrder   AS INT  NO-UNDO.
   DEF VAR lcProcessHost AS CHAR NO-UNDO.
   DEF VAR liReplQty     AS INT  NO-UNDO.
   DEF VAR liReplLimit   AS INT  NO-UNDO.
   DEF VAR liMainLimit   AS INT  NO-UNDO.
   DEF VAR liBatchCnt    AS INT  NO-UNDO.
   
   ASSIGN
      liMainLimit = liTotal / iiBatchQty
      liBatchCnt  = 0.

   fCalculateReplicaQty(iiUseReplica,
                        iiBatchQty,
                        liTotal,        
                        INPUT-OUTPUT liMainLimit,
                        OUTPUT liReplQty,
                        OUTPUT liReplLimit).
  
   ASSIGN
      liCallQty   = -1
      liLimit = liMainLimit.

   FOR EACH ttSubs:
   
      IF liCallQty < 0 OR liCallQty >= liLimit THEN DO:

         ASSIGN 
            liCallQty   = 0
            liBatchCnt  = liBatchCnt + 1
            liFeedOrder = 0
            lcProcessHost = ""
            liLimit = liMainLimit.
            
         CASE iiUseReplica:
         /* partially to replica */
         WHEN 1 THEN DO:
            IF liBatchCnt <= liReplQty THEN ASSIGN
               lcProcessHost = lcReplica
               liLimit = liReplLimit.
         END.
         /* all to replica */
         WHEN 2 THEN lcProcessHost = lcReplica.
         END CASE. 
      END.
      
      liCallQty = liCallQty + ttSubs.CallQty.
            
      DO TRANS:
         CREATE FuncRunResult.
         ASSIGN 
            FuncRunResult.FRProcessID = iiFRProcessID
            FuncRunResult.FRExecID    = iiFRExecID
            FuncRunResult.FRResultSeq = liBatchCnt
            FuncRunResult.IntParam    = ttSubs.MsSeq
            FuncRunResult.DecParam    = ttSubs.InvCust
            liFeedOrder               = liFeedOrder + 1
            FuncRunResult.ResultOrder = liFeedOrder
            FuncRunResult.ProcessHost = lcProcessHost.
      END.
   END.

   RETURN "".

END PROCEDURE.   


