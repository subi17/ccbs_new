&GLOBAL-DEFINE PersistentRun YES
&GLOBAL-DEFINE CounterHandling TempTable

DEFINE VARIABLE objDynQueryMServiceLimit AS CLASS Syst.DynQuery NO-UNDO.
objDynQueryMServiceLimit = NEW Syst.DynQuery().
objDynQueryMServiceLimit:mAddBuffer(BUFFER mServiceLimit:HANDLE).

{Syst/commali.i}
{Rate/rerate_define.i}
{Rate/premiumnumber.i}
{Syst/funcrunprocess_update.i}
{Func/direct_dbconnect.i}

DEF INPUT  PARAMETER iiFRProcessID AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiDone        AS INT  NO-UNDO. 

DEF BUFFER bufTriggerItem  FOR TriggerItem.
DEF BUFFER bufTriggerEvent FOR TriggerEvent.
DEF BUFFER SearchItem      FOR TriggerItem.
DEF BUFFER bCustItem       FOR TriggerItem.

DEF VAR idtActDate  AS DATE NO-UNDO.
DEF VAR idtActDate2 AS DATE NO-UNDO.
DEF VAR ldtFrom     AS DATE NO-UNDO.
DEF VAR ldtTo       AS DATE NO-UNDO.
DEF VAR liPeriod    AS INT  NO-UNDO.
DEF VAR liInterval  AS INT  NO-UNDO INIT 1.
DEF VAR liWaitSeconds AS INT  NO-UNDO.
DEF VAR likpl       AS INT   NO-UNDO.
DEF VAR lrRecid     AS RECID NO-UNDO.
DEF VAR lcStarted   AS CHAR NO-UNDO.
DEF VAR lcCurrent   AS CHAR NO-UNDO.
DEF VAR LatestCreated LIKE TriggerEvent.Created NO-UNDO.
DEF VAR LatestConf  AS CHAR NO-UNDO.
DEF VAR latestEvent AS INT  NO-UNDO.
DEF VAR LatestItem  AS INT  NO-UNDO.
DEF VAR ldtDateFrom AS DATE NO-UNDO.
DEF VAR ldtDateTo   AS DATE NO-UNDO.
DEF VAR lcCustNum   AS CHAR NO-UNDO.
DEF VAR liCustNum   AS INT  NO-UNDO.
DEF VAR LatestCLI   AS CHAR NO-UNDO.
DEF VAR lcTriggersFrom AS CHAR NO-UNDO.
DEF VAR llEmptyLoop AS LOG  NO-UNDO.
DEF VAR prevRecID   AS RECID NO-UNDO.
DEF VAR ldtItemTime AS DATETIME NO-UNDO. 
DEF VAR liActiveDB    AS INT  NO-UNDO.
DEF VAR ldaActiveFrom AS DATE NO-UNDO.
DEF VAR ldaActiveTo   AS DATE NO-UNDO.

RUN pInitializeRerate.

PROCEDURE pInitializeRerate:

   fFillTT().
   
   lcRerateSource = "TRIGGER". 

END PROCEDURE.

PROCEDURE pInitializeRerateReport:

   DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idtFrom AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtTo AS DATE NO-UNDO.
   DEF INPUT PARAMETER icInvRunCode AS CHAR NO-UNDO.

   liRerateSeq = fRerateLogStart (
      icUserCode,
      idtFrom,
      idtTo,
      "", /* cli */
      0,  /* custnum from */
      0,  /* custnum to*/
      "", /* clitype */
      0,  /* ErrorCode */
      "", /* InvGroup */
      icInvRunCode).

END PROCEDURE.

PROCEDURE pFinalizeRerateReport:
   
   IF liRerateSeq > 0 THEN fRerateLogFinish(liRerateSeq).

END PROCEDURE.

DEFINE TEMP-TABLE ttReRate
   FIELD CustNum  AS INTEGER    FORMAT ">>>>>>>>9"
   FIELD FromDate AS DATE
   FIELD Cli      AS CHARACTER
   FIELD ToDate   AS DATE
   FIELD InvGroup AS CHARACTER
   FIELD Period   AS INTEGER.


FUNCTION fCreateTTRerate RETURN LOGICAL
(OUTPUT lrRecID AS RECID):

   DEF VAR ldaFrom AS DATE NO-UNDO.
   DEF VAR ldaTo   AS DATE NO-UNDO.
   
   IF NOT AVAIL TriggerItem OR LOCKED(TriggerItem) THEN RETURN FALSE.
                  
   FIND FIRST MsOwner WHERE 
              MsOwner.InvCust = TriggerItem.InvCust NO-LOCK NO-ERROR.

   IF NOT AVAIL msowner then RETURN FALSE.

   ASSIGN 
      ldaFrom = DATE(INT(SUBSTRING(STRING(TriggerItem.Period),5,2)), 
                     1,
                     INT(SUBSTRING(STRING(TriggerItem.Period),1,4)))
      ldaTo   = ldaFrom + 35
      ldaTo   = DATE(MONTH(ldaTo),1,YEAR(ldaTo)) - 1.
   
   /* is current db the correct one */
   IF ldaFrom > ldaActiveTo /* OR ldaTo < ldaActiveFrom */ THEN 
      RETURN FALSE.

   ASSIGN 
      TriggerItem.StatusCode = 1 
      TriggerItem.Activated  = DATETIME(Today,mtime)
      lrRecid                = RECID(TriggerItem).

   CREATE ttReRate.
   ASSIGN
      ttReRate.CustNum  = TriggerItem.InvCust
      ttRerate.FromDate = ldaFrom
      ttRerate.ToDate   = ldaTo
      ttRerate.Cli      = TriggerItem.Cli 
      ttReRate.Period   = TriggerItem.Period.

   RETURN TRUE.

END FUNCTION.

FUNCTION fHandleTTRerate RETURN LOGICAL
   (INPUT lrRecid AS RECID) .

   FOR EACH ttRerate NO-LOCK.

      IF ttRerate.cli = ""  THEN
         RUN pRunCustRerate(ttRerate.CustNum, ttRerate.FromDate, 
                            ttRerate.ToDate, TRUE). 
      ELSE  RUN pRunCliRerate(ttRerate.CustNum, ttRerate.Cli, 
                              ttRerate.FromDate, ttRerate.ToDate, TRUE).   
                      
      DO TRANS:
         FIND FIRST BufTriggerItem WHERE 
              RECID(BufTriggerItem) = lrRecID EXCLUSIVE-LOCK NO-ERROR NO-WAIT .
         
         IF AVAIL BufTriggerItem AND NOT LOCKED(BufTriggerItem) 
         THEN ASSIGN
            BufTriggerItem.Handled    = DATETIME(Today,mtime)
            BufTriggerItem.StatusCode = 4.
                 
         EMPTY TEMP-TABLE ttRerate.
         RELEASE BufTriggerItem.
      END.   
   END.

END FUNCTION.


EMPTY TEMP-TABLE ttRerate.

FORM
   lcStarted      COLON 20 FORMAT "X(20)"         LABEL "Started" 
   lcCurrent      COLON 20 FORMAT "X(20)"         LABEL "Latest Loop"   
   lcTriggersFrom COLON 20 FORMAT "x(20)"         LABEL "Triggers From" SKIP(1)
   LatestCreated  COLON 20                        LABEL "Latest TriggerItem"
   LatestConf     COLON 20 FORMAT "x(20)"         LABEL "Trigger Conf"
   LatestEvent    COLON 20 FORMAT ">>>>>>>>>"     LABEL "Trigger Event"
   LatestItem     COLON 20 FORMAT ">>>>>>>>>>>"   LABEL "InvCust"
   LAtestCli      COLON 20 FORMAT "X(20)"         LABEL "CLI"
   oiDone         COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Items Handled"

WITH CENTERED ROW 10 SIDE-LABELS 
     TITLE " TRIGGERITEM RATING " FRAME fQty.

ASSIGN 
   lcStarted = fTS2HMS(fMakeTS())
   liWaitSeconds = fCParamI("TriggerRerateDelay").
IF liWaitSeconds = ? THEN liWaitSeconds = 600.   

/* period for current active cdr db */ 
liActiveDB = fGetCurrentDB(gcBrand,
                           "MobCDR",
                           OUTPUT ldaActiveFrom,
                           OUTPUT ldaActiveTo).
IF ldaActiveFrom = ? THEN ASSIGN
   ldaActiveFrom = 1/1/2010
   ldaActiveTo   = 12/31/2049.
 
PAUSE 0.

lcCustNum = SOURCE-PROCEDURE:PRIVATE-DATA.

IF lcCustNum BEGINS "TriggerRate_Param:" THEN DO:
   ASSIGN liCustNum = INT(ENTRY(2,lcCustNum,":")).
END.

IF liCustNum > 0 THEN 
FOR EACH bCustItem NO-LOCK WHERE 
         bCustItem.InvCust    = liCustNum AND 
         bCustItem.StatusCode = 0:

   FIND FIRST TriggerItem WHERE RECID(TriggerItem) = RECID(bCustItem)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF NOT AVAIL TriggerItem OR LOCKED(TriggerItem) THEN LEAVE.
      
   IF NOT fCreateTTRerate(OUTPUT lrRecid) THEN NEXT.

   IF lrRecid = ? THEN NEXT.
   
   fHandlettRerate(lrRecid).
END.

IF liCustNum > 0 THEN RETURN.

IF iiFrProcessID = 0 THEN 
   DISP lcStarted WITH FRAME fQty.               

MAINLOOP:
DO WHILE TRUE:

   IF iiFrProcessID = 0 THEN 
      PUT SCREEN ROW 22 COL 2
         "F8 TO QUIT, other keys start handling immediatelly (Interval=" + 
        STRING(liInterval) + ")".

   ASSIGN 
   lrRecid                = ? 
   llEmptyLoop            = TRUE
   LatestConf             = "WAITING"
   .
           
   IF iiFrProcessID = 0 THEN DO:
      PAUSE 0.
      DISP LAtestConf WITH FRAME fQty .
   END.                          
   
   IF liInterval > 0 THEN DO:
      lcCurrent = fTS2HMS(fMakeTS()).
      latestcli = "". lrrecid = ?.

      IF iiFrProcessID = 0 THEN DO:
         DISP lcCurrent WITH FRAME fqty.

         READKEY PAUSE liInterval.
      END.
      
      RUN pInitializeRerate. 
      
      IF iiFrProcessID = 0 THEN PUT SCREEN ROW 8 
            STRING("Latest tariffs: " + STRING(Today,"99-99-9999")
                         + " " + STRING(Time,"hh:mm:ss")).
   END. 

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
      LEAVE MainLoop.
   END.

   LOOP:
   DO WHILE TRUE:

      TRIGGERCONF:
      FOR EACH TriggerConf NO-LOCK  
      BREAK BY TriggerConf.Prior.

         ASSIGN 
            LatestConf = TriggerConf.TriggerConfID
            ldtItemTime = ADD-INTERVAL(NOW,-1 * liWaitSeconds,"seconds")
            lcTriggersFrom = STRING(ldtItemTime,"99-99-9999 hh:mm:ss").
            
         IF FIRST(Triggerconf.prior) then llEmptyLoop = TRUE.
                 
         IF iiFrProcessID = 0 THEN DO:
            PAUSE 0.
            DISP LAtestConf lcTriggersFrom WITH FRAME fQty .
         END.
         
         lrRecid = ?.
         
         triggerevent:
         FOR  EACH TriggerEvent NO-LOCK WHERE 
                   TriggerEvent.TriggerConfID   = TriggerConf.TriggerConfID AND 
                   TriggerEvent.StatusCode      = 4
         BREAK BY TriggerEvent.TriggerConfID:          
   
            lrRecid = ?.
            
            FIND FIRST SearchItem WHERE  
                  SearchItem.TriggerConfID  = TriggerConf.TriggerConfID    AND
                  SearchItem.TriggerEventID = TriggerEvent.TriggerEventID  AND
                  SearchItem.StatusCode     < 2              NO-LOCK NO-ERROR.

            IF NOT AVAIL SearchItem THEN DO:
               
               FIND FIRST BufTriggerEvent WHERE
                    RECID(BufTriggerEvent) = RECID(TriggerEvent)
               EXCLUSIVE-LOCK NO-ERROR  NO-WAIT .
                                                               
               IF LOCKED(bufTriggerEvent) THEN DO:
                  NEXT.
               END.    

               ASSIGN bufTriggerEvent.StatusCode = 6.
                
               NEXT.
            END.

            SEARCHITEM:
            FOR EACH SearchItem WHERE
                  SearchItem.TriggerConfID  = TriggerConf.TriggerConfID    AND
                  SearchItem.TriggerEventID = TriggerEvent.TriggerEventID  AND
                  SearchItem.StatusCode    <= 1                            AND
                  SearchItem.Created       <= ldtItemTime NO-LOCK:
                                 
               FIND FIRST TriggerItem WHERE 
                          RECID(TriggerItem) = RECID(SearchItem) 
               EXCLUSIVE-LOCK NO-ERROR  NO-WAIT .

               IF NOT AVAIL TriggerItem OR LOCKED(TriggerItem) THEN 
                  NEXT SEARCHITEM.
               ELSE LEAVE SEARCHITEM. 
            END.

            IF NOT fCreateTTRerate(OUTPUT lrrecid) THEN DO:

               IF last(triggerconf.prior) AND 
                  LAST(TriggerEvent.TriggerConfID) THEN DO:
                  liinterval = 20.
                  LEAVE LOOP.
               END.
               ELSE NEXT TRIGGEREvent.
            END.

            IF lrRecid = ? THEN NEXT TriggerConf.
            
            ASSIGN
               LatestCreated       = TriggerItem.Created
               LatestConf          = TriggerItem.TriggerConfID
               LatestEvent         = TriggerItem.TriggerEventID
               LatestItem          = TriggerItem.InvCust
               llEmptyLoop         = FALSE
               LatestCli           = TriggerItem.Cli + " / " + 
                                     STRING(TriggerItem.Period).
                                     
            LEAVE.
         END.

         PAUSE 0.

         IF lrRecid = ? OR lrrecid = 0 THEN DO:
            IF  last(Triggerconf.prior) AND llEmptyLoop  THEN DO:
               liInterval = 30.
               LEAVE LOOP.
            END.
            ELSE liInterval = 0 .
             
            NEXT  Triggerconf.
         
         END.

         if lrrecid ne ? then DO:
            IF prevRecid = lrrecid THEN LEAVE TriggerConf.
            oiDone = oiDone + 1.
            PrevRecid = lrrecid.
         END.

         IF iiUpdInterval > 0 AND oiDone MOD iiUpdInterval = 0 THEN DO:
            IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiDone) THEN
               RETURN "ERROR:Stopped".
         END.   
          
         IF iiFrProcessID = 0 THEN DO:
            DISP
               lcStarted    
               lcCurrent    
               LatestCreated
               LatestConf   
               LatestEvent  
               LatestCli
               LatestItem 
               oiDone  
            WITH FRAME fQty.
            put screen row 1 string(lrrecid).
         END.
         
         fHandlettRerate(lrrecid).

         lrREcid = ? .

         RELEASE TriggerItem.
      END.
      
   END.  /* LOOP */

   IF iiFRProcessID > 0 AND llEmptyLoop THEN LEAVE.
   
END.

IF iiFRProcessID > 0 THEN RETURN.
ELSE QUIT.
         
PROCEDURE pRunCustRerate:

   DEF INPUT PARAMETER iiInvCust AS INT  NO-UNDO.
   DEF INPUT PARAMETER idtFrom   AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtTo     AS DATE NO-UNDO. 
   DEF INPUT PARAMETER ilSilent  AS LOG  NO-UNDO.

   DEF VAR ldeBegStamp AS DEC  NO-UNDO.
   
   DEF BUFFER Inv-Cust FOR Customer.

   ASSIGN 
   cdate1   = idtFrom
   cdate2   = idtTo.

   bbatch = ilSilent.

   /* convert DATE fields into CHAR */
   ASSIGN
   ldestamp = YEAR(cdate1)  * 10000 +
              MONTH(cdate1) * 100   +
              DAY(cdate1) 
   ldebegstamp = YEAR(cdate2)  * 10000 +
                 MONTH(cdate2) * 100   +
                 DAY(cdate2) .
                      
   fInitializeRerateLog(iiInvCust,
                        0,
                        "",
                        lcRerateSource,
                        idtFrom,
                        idtTo).
                        
   FOR FIRST Inv-Cust NO-LOCK WHERE 
             Inv-Cust.CustNum = iiInvCust:

      fEmptyRerateTempTables().

      /* counters to temp-table (with null amounts) */
      FOR EACH MsOwner NO-LOCK WHERE
               MsOwner.InvCust  = Inv-Cust.Custnum AND
               MsOwner.TSEnd   >= ldestamp         AND 
               Msowner.TSBegin <= ldebegstamp      AND
               MsOwner.PayType = FALSE
      BREAK BY MsOwner.MsSeq
            BY MsOwner.CustNum:

         IF NOT FIRST-OF(MsOwner.CustNum) THEN NEXT.
 
         fServiceLCounter2Temp(MsOwner.MsSeq,
                               MsOwner.CustNum,
                               cDate1,
                               cDate2).

         fDCCounter2Temp(MsOwner.MsSeq,
                         cDate1,
                         cDate2).

         fSaldoCounter2Temp(MsOwner.MsSeq,
                            cDate1,
                            cDate2).
      END.   

   END.

     
   ETIME(YES).
   Main:
   REPEAT:

      DO:
         MobCDR: 
         FOR EACH MobCDR NO-LOCK USE-INDEX InvCust WHERE   
                  MobCDR.InvCust  = iiInvCust AND 
                  MobCDR.datest  >= cdate1    AND
                  MobCDR.datest  <= cdate2    
         TRANSACTION WITH FRAME MobCDR: 
       
            {Rate/man_rate2.i}           


PROCEDURE pRunCliRerate:

   DEF INPUT PARAMETER iiInvCust AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCli     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idtFrom   AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtTo     AS DATE NO-UNDO. 
   DEF INPUT PARAMETER ilSilent  AS LOG  NO-UNDO.

   DEF VAR ldeBegStamp AS DEC  NO-UNDO.
   DEF VAR liMsSeq     AS INT  NO-UNDO.
   
   DEF BUFFER Inv-Cust FOR Customer.
   
   ASSIGN 
   cdate1   = idtFrom
   cdate2   = idtTo.

   bbatch = ilSilent.

   /* convert DATE fields into CHAR */
   ASSIGN
   ldestamp = YEAR(cdate1)  * 10000 +
              MONTH(cdate1) * 100   +
              DAY(cdate1) 
   ldebegstamp = YEAR(cdate2)  * 10000 +
                 MONTH(cdate2) * 100   +
                 DAY(cdate2) .
                      
                      
   FOR FIRST Inv-Cust NO-LOCK WHERE 
             Inv-Cust.CustNum = iiInvCust:
      
      fEmptyRerateTempTables().

      /* counters to temp-table (with null amounts) */
      FOR EACH MsOwner NO-LOCK WHERE
               MsOwner.InvCust  = Inv-Cust.Custnum AND
               MSOwner.cli      = icCli            AND 
               MsOwner.TSEnd   >= ldestamp         AND 
               Msowner.TSBegin <= ldebegstamp
      BREAK BY MsOwner.MsSeq
            BY MsOwner.CustNum:

         IF NOT FIRST-OF(MsOwner.CustNum) THEN NEXT.
         
         fServiceLCounter2Temp(MsOwner.MsSeq,
                               MsOwner.CustNum,
                               cDate1,
                               cDate2).

         fDCCounter2Temp(MsOwner.MsSeq,
                         cDate1,
                         cDate2).

         fSaldoCounter2Temp(MsOwner.MsSeq,
                            cDate1,
                            cDate2).

         IF liMsSeq > 0 AND MsOwner.MsSeq NE liMsSeq THEN liMsSeq = ?.
         ELSE liMsSeq = MsOwner.MsSeq.
      END.   

   END.

   IF liMsSeq = ? THEN liMsSeq = 0. 
   
   fInitializeRerateLog(iiInvCust,
                        liMsSeq,
                        MsOwner.CLI,
                        lcRerateSource,
                        idtFrom,
                        idtTo).
     
   ETIME(YES).
   Main:
   REPEAT:

      DO:
         MobCDR: 
         FOR EACH MobCDR NO-LOCK USE-INDEX CLI WHERE   
                  MobCDR.Cli      = icCli     AND 
                  MobCDR.datest  >= cdate1    AND
                  MobCDR.datest  <= cdate2    
         TRANSACTION WITH FRAME MobCDR: 
       
            {Rate/man_rate2.i}           


FINALLY:
   IF VALID-OBJECT(objDynQueryMServiceLimit)
   THEN DELETE OBJECT objDynQueryMServiceLimit.
END FINALLY.
  
      
