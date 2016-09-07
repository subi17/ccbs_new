/* mobcdr_double_check.p     10.01.11/aam separated from nndomco
*/

{commali.i}
{cparam2.i}
{funcrunprocess_update.i}
{error_codes.i}
{rate_roamzone.i}

DEF INPUT  PARAMETER icFindMode  AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
DEF INPUT  PARAMETER icCLI       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilMark      AS LOG  NO-UNDO.
DEF INPUT  PARAMETER ilDisplay   AS LOG  NO-UNDO.
DEF INPUT  PARAMETER icLogFile   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiQty       AS INT  NO-UNDO.

DEF VAR liTotal          AS INT  NO-UNDO.
DEF VAR liDouble         AS INT  NO-UNDO.
DEF VAR lcStartTime      AS CHAR NO-UNDO.
DEF VAR ldaPrevDate      AS DATE NO-UNDO.
DEF VAR lcColHeader      AS CHAR NO-UNDO.
DEF VAR lcCustName       AS CHAR NO-UNDO.
DEF VAR lcPrice          AS CHAR NO-UNDO.
DEF VAR lhDouble         AS HANDLE NO-UNDO.
DEF VAR lhMarkCDR        AS HANDLE NO-UNDO.
DEF VAR liCnt            AS INT  NO-UNDO.

DEF TEMP-TABLE ttMarkCDR NO-UNDO LIKE mcdr.MobCDR.

DEF BUFFER Double FOR mcdr.MobCDR.

DEF STREAM sLog.

FORM
   liDouble                           COLUMN-LABEL "Qty"
   Double.Datest                      COLUMN-LABEL "Date"
   lcStartTime       FORMAT "x(8)"    COLUMN-LABEL "StartTime"
   Double.CLI                         COLUMN-LABEL "A-subscriber"
   Double.CustNum                     COLUMN-LABEL "Cust.nr"
WITH OVERLAY 13 DOWN CENTERED ROW 2 TITLE " Doubles  " FRAME fLog.


FUNCTION  fTriggerEvent RETURN LOGICAL
(INPUT CallTimestamp AS DEC,
 INPUT iiMSSeq       AS INT,
 INPUT ideLatest     AS DEC):
  
   FIND FIRST TriggerConf WHERE
              TriggerConf.TriggerConfID = "MobCDR"        AND
              TriggerConf.EventRule     > 0               AND
              TriggerConf.ValidTo       >= Today          AND
              TriggerConf.ValidFrom     <= Today NO-LOCK NO-ERROR.
                                                             
   IF AVAIL TriggerConf THEN DO:
                                                             
      CREATE TriggerEvent.
      ASSIGN
      TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
      TriggerEvent.TriggerConfID  = "MobCDR"
      TriggerEvent.EventSource    = "MODIFY"
      TriggerEvent.Created        = DateTime(Today,mtime)
      TriggerEvent.TableID        = iiMSSEQ
      TriggerEvent.TableName      = "Mobcdr"
      TriggerEvent.Keyvalue       = STRING(CallTimeStamp)
      TriggerEvent.ChangedFields  = "DoubleCall"
      TriggerEvent.ChangedValues  = STRING(idelatest).
                                                
      RELEASE TriggerEvent.
   
   END.
                                                         
END FUNCTION.
                                                         
FUNCTION fHandleDouble RETURNS CHAR
   (ihDouble AS HANDLE,
    icDB     AS CHAR):

   DEF VAR llMarkOrig AS LOG NO-UNDO.
   DEF VAR liAgrCust AS INT NO-UNDO.
   DEF VAR lcCdrID AS CHAR NO-UNDO. 
   DEF VAR lcCallIdNum AS CHAR NO-UNDO.
   DEF VAR lcApn AS CHAR NO-UNDO. 
      
   IF ihDouble::MSCID EQ "CCGW" AND
      mcdr.MobCDR.MSCID EQ "CCGW" THEN DO:

      lcCdrId = fGetMcdrDtlValue(mcdr.MobCDR.Datest,
                                 mcdr.MobCDR.Dtlseq,
                                 "External running index").
      IF lcCdrId NE fGetMcdrDtlValue(ihDouble::Datest,
                                     ihDouble::Dtlseq,
                                     "External running index")
         THEN RETURN "".
   END.
   ELSE IF ihDouble::MSCID EQ "POST" AND
           ihDouble::EventType EQ "SMS" AND 
           mcdr.MobCDR.MSCID EQ  "POST" AND
           mcdr.MobCDR.EventType EQ "SMS" THEN DO:
      lcCallIdNum = fGetMcdrDtlValue(mcdr.MobCDR.Datest,
                                     mcdr.MobCDR.Dtlseq,
                                     "Call identification number").
      IF lcCallIdNum NE fGetMcdrDtlValue(ihDouble::Datest,
                                         ihDouble::Dtlseq,
                                         "Call identification number")
         THEN RETURN "".
   END.
   ELSE IF ihDouble::MSCID EQ "POSTD" AND
           mcdr.MobCDR.MSCID EQ  "POSTD" THEN DO:
      lcApn = fGetMcdrDtlValue(mcdr.MobCDR.Datest,
                               mcdr.MobCDR.Dtlseq,
                               "Access point name NI").
      IF lcApn NE fGetMcdrDtlValue(ihDouble::Datest,
                                   ihDouble::Dtlseq,
                                   "Access point name NI")
         THEN RETURN "".
   END.

   liDouble = liDouble + 1.
        
   IF ilDisplay THEN DO:
      DISPLAY
         liDouble
         ihDouble::DateSt @ Double.DateSt
         STRING(ihDouble::TimeStart,"hh:mm:ss") @ lcStartTime
         ihDouble::CLI @ Double.CLI
         ihDouble::CustNum @ Double.CustNum
      WITH FRAME fLog.
      DOWN WITH FRAME fLog.
   END.
   ELSE IF NOT SESSION:BATCH THEN 
      PUT SCREEN ROW 23 col 60 STRING(liDouble).

   FIND FIRST InvSeq WHERE
              InvSeq.InvSeq = ihDouble::InvSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE InvSeq THEN RETURN "".
       
   IF InvSeq.Billed = FALSE THEN DO:
      llMarkOrig = FALSE.
        
      lhMarkCDR:BUFFER-CREATE.
      lhMarkCDR:BUFFER-COPY(ihDouble).
   END.   
   ELSE DO:
      FIND FIRST InvSeq WHERE
                 InvSeq.InvSeq = mcdr.MobCDR.InvSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE InvSeq OR InvSeq.Billed THEN RETURN "".
            
      llMarkOrig = TRUE.
         
      CREATE ttMarkCDR.
      BUFFER-COPY mcdr.MobCDR TO ttMarkCDR.
   END.

   liAgrCust = InvSeq.AgrCust.
   
   IF icLogFile > "" THEN DO:

      FIND FIRST Customer where Customer.CustNum = ttMarkCDR.InvCust 
         NO-LOCK NO-ERROR.

      IF AVAIL Customer THEN 
         lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                       BUFFER Customer).
      ELSE lcCustName = "UNKNOWN".                                 

      lcPrice = REPLACE(STRING(ttMarkCDR.Amount,"->>>>9.9999"),".",",").

      PUT STREAM sLog UNFORMATTED
         STRING(ttMarkCDR.Datest,"99-99-99")  CHR(9)
         STRING(ttMarkCDR.TimeST,"hh:mm:ss")  CHR(9) 
         STRING(ttMarkCDR.BillDur,"hh:mm:ss") CHR(9)
         ttMarkCDR.CLI                        CHR(9)
         ttMarkCDR.GsmBnr                     CHR(9)
         ttMarkCDR.InvCust                    CHR(9)
         lcCustName                           CHR(9)
         lcPrice                              CHR(9)
         icDb                                 SKIP.
   END.

   IF ilMark THEN DO TRANS: 
          
      IF ttMarkCDR.ErrorCode = 0 AND
         ttMarkCDR.InvSeq > 0 THEN DO:
           
         FIND FIRST SaldoCounter WHERE
                    SaldoCounter.MsSeq  = ttMarkCDR.MsSeq AND
                    SaldoCounter.Period = YEAR(ttMarkCDR.DateSt) * 100 + 
                                          MONTH(ttMarkCDR.DateSt)
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.             

         IF AVAIL SaldoCounter AND NOT LOCKED(SaldoCounter) THEN DO:
            SaldoCounter.amt = SaldoCounter.Amt - ttMarkCDR.Amount.

            RELEASE SaldoCounter.
         END.    

         CREATE TMQueue.
         BUFFER-COPY ttMarkCDR TO TMQueue.
         ASSIGN
            TMQueue.Qty     = -1
            TMQueue.EventID = ttMarkCDR.DtlSeq
            TMQueue.AgrCust = liAgrCust
            TMQueue.Source  = ttMarkCDR.MSCID
            TMQueue.PayType = 1 + INT(ttMarkCDR.PPFlag > 0)
            TMQueue.ReportingID = ttMarkCDR.ServRid + "," + ttMarkCDR.MPMRid
            TMQueue.ExtraAmount = ttMarkCDR.MPMAmt.

            /* do not update fraud counters from old events */
            IF YEAR(ttMarkCDR.DateSt) < YEAR(TODAY) OR
               MONTH(ttMarkCDR.DateSt) < MONTH(TODAY) 
            THEN TMQueue.AccumTarget = "InvRow".
               
            RELEASE TMQueue.
      
      
         fTriggerEvent(DEC(YEAR(ttMarkCdr.Datest)  * 10000 + 
                          MONTH(ttMarkCDR.Datest) * 100   +
                            DAY(ttMarkCDR.DAtest) + 
                             (ttMarkCDR.Timest / 100000)),
                           ttMArkCDR.MSSeq,
                        DEC(YEAR(ttMarkCdr.Datest)  * 10000 +
                            MONTH(ttMarkCDR.Datest) * 100   +
                              DAY(ttMarkCDR.DAtest) +
                            (ttMarkCDR.Timest / 100000))).
      END.
         
      IF llMarkOrig THEN DO:
         FIND CURRENT mcdr.MobCDR EXCLUSIVE-LOCK.
         ASSIGN 
            mcdr.MobCDR.ErrorCode = IF ttMarkCDR.MSCID = "CCGW" THEN
                                       {&CDR_ERROR_DOUBLE_CCGW_CDR}
                                    ELSE IF ttMarkCDR.MSCID = "POSTD" THEN
                                       {&CDR_ERROR_DOUBLE_DATA_CDR}
                                    ELSE {&CDR_ERROR_DOUBLE_CALL}
            mcdr.MobCDR.InvSeq    = 0.
         RELEASE mcdr.MobCDR.   
      END.   
      ELSE DO:
         ihDouble:FIND-CURRENT(EXCLUSIVE-LOCK).
         ASSIGN 
            ihDouble::ErrorCode = IF ttMarkCDR.MSCID = "CCGW" THEN
                                     {&CDR_ERROR_DOUBLE_CCGW_CDR}
                                  ELSE IF ttMarkCDR.MSCID = "POSTD" THEN
                                     {&CDR_ERROR_DOUBLE_DATA_CDR}
                                  ELSE {&CDR_ERROR_DOUBLE_CALL}
            ihDouble::InvSeq    = 0.
         ihDouble:BUFFER-RELEASE().    
      END.   
   END.

   oiQty = oiQty + 1.
   IF iiUpdateInterval > 0 AND oiQty MOD iiUpdateInterval = 0 THEN DO:
      IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiQty) THEN
         RETURN "ERROR:Stopped".
   END.   
    
   RETURN "D".
   
END FUNCTION.

/****** Main start *********/

ASSIGN 
   lcColHeader      = "Date,Started,Duration,MSISDN,B-nbr,Customer,Name,Price,Db"
   lhDouble         = BUFFER Double:HANDLE
   lhMarkCDR        = BUFFER ttMarkCDR:HANDLE.


IF icLogFile > "" THEN DO:
   OUTPUT STREAM sLog TO value(icLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED 
      "Double CDRs from "
      STRING(idaFromDate) " - "  
      STRING(idaToDate)   CHR(9)
      STRING(ilMark,"Marked/Listed") SKIP.

   DO liCnt = 1 TO NUM-ENTRIES(lcColHeader).
      PUT STREAM sLog UNFORMATTED ENTRY(liCnt,lcColHeader) CHR(9).
   END.

   PUT STREAM sLog SKIP.
END.    

IF icFindMode = "ReadDate" THEN DO:

   FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE 
            MobCDR.ReadDate >= idaFromDate AND
            MobCDR.ReadDate <= idaToDate   AND
            MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
            MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
            MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR}:
      RUN pCheckDouble.
      IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.      
   END.
END.

ELSE DO:

   IF icCLI > "" THEN 
   FOR EACH mcdr.MobCDR USE-INDEX CLI NO-LOCK WHERE 
            mcdr.MobCDR.CLI = icCLI AND
            mcdr.MobCDR.DateSt >= idaFromDate AND
            mcdr.MobCDR.DateSt <= idaToDate AND
            mcdr.MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
            mcdr.MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
            mcdr.MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR}:
      RUN pCheckDouble.      
      IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.      
   END.
                 
   ELSE 
   FOR EACH mcdr.MobCDR USE-INDEX Date NO-LOCK WHERE 
            mcdr.MobCDR.DateSt >= idaFromDate AND
            mcdr.MobCDR.DateSt <= idaToDate AND
            mcdr.MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
            mcdr.MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
            mcdr.MobCDR.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR}:
      RUN pCheckDouble.      
      IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.      
   END.
END.

IF icLogFile > "" THEN 
   OUTPUT STREAM sLog CLOSE.

IF ilDisplay THEN HIDE FRAME fLog.

RETURN "". 

/******* Main end ********/


PROCEDURE pCheckDouble:

   DEF VAR lhOldCDR AS HANDLE NO-UNDO.
   DEF VAR lhFind   AS HANDLE NO-UNDO.
   DEF VAR lcFind   AS CHAR   NO-UNDO.
   DEF VAR lcOldCDR AS CHAR   NO-UNDO.
   DEF VAR lcResult AS CHAR   NO-UNDO.
      
   IF NOT SESSION:BATCH AND ldaPrevDate NE mcdr.MobCDR.Datest THEN 
      PUT SCREEN ROW 17 COL 3 STRING(mcdr.MobCDR.Datest).
   ldaPrevDate = mcdr.MobCDR.Datest.

   ASSIGN liTotal  = liTotal + 1.
     
   IF NOT SESSION:BATCH AND liTotal MOD 1000 = 0 THEN 
      PUT SCREEN ROW 1 COL 1 STRING(liTotal).

   FOR EACH Double USE-INDEX CLI NO-LOCK WHERE
            Double.CLI       = mcdr.MobCDR.CLI       AND
            Double.DateSt    = mcdr.MobCDR.DateSt    AND
            Double.TimeSt    = mcdr.MobCDR.TimeSt    AND
            Double.BillDur   = mcdr.MobCDR.BillDur   AND
            Double.GsmBnr    = mcdr.MobCDR.GsmBnr    AND
            Double.SpoCmt    = mcdr.MobCDR.SpoCmt    AND
            Double.CCharge   = mcdr.MobCDR.CCharge   AND
            RECID(Double)    NE RECID(mcdr.MobCDR)   AND
            Double.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL}     AND
            Double.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
            Double.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR}
   WITH FRAME LOG:

      lcResult = fHandleDouble(lhDouble,"current").
      IF lcResult BEGINS "ERROR:" THEN RETURN lcResult.
   END.

   IF CONNECTED("oldmcdr") THEN DO:

      lcOldCDR = "oldmcdr.MobCDR".
           
      CREATE BUFFER lhOldCDR FOR TABLE lcOldCDR.
    
      CREATE QUERY lhFind.
      lhFind:SET-BUFFERS(lhOldCDR).

      lcFind = 'FOR EACH ' + lcOldCDR + ' NO-LOCK USE-INDEX CLI WHERE ' +
                lcOldCDR + '.CLI         = ' + '"' + mcdr.MobCDR.CLI + '"'          + ' AND ' +
                lcOldCDR + '.DateSt      = ' + STRING(mcdr.MobCDR.DateSt)           + ' AND ' +
                lcOldCDR + '.TimeStart   = ' + STRING(mcdr.MobCDR.TimeStart)        + ' AND ' +
                lcOldCDR + '.BillDur     = ' + STRING(mcdr.MobCDR.BillDur)          + ' AND ' +
                lcOldCDR + '.GsmBnr      = ' + '"' + mcdr.MobCDR.GsmBnr + '"'       + ' AND ' +
                lcOldCDR + '.SpoCmt      = ' + STRING(mcdr.MobCDR.SpoCmt)           + ' AND ' +
                lcOldCDR + '.CCharge     = ' + STRING(mcdr.MobCDR.CCharge)          + ' AND ' +
                lcOldCDR + '.ErrorCode  NE ' + STRING({&CDR_ERROR_DOUBLE_CALL})     + ' AND ' +
                lcOldCDR + '.ErrorCode  NE ' + STRING({&CDR_ERROR_DOUBLE_CCGW_CDR}) + ' AND ' +
                lcOldCDR + '.ErrorCode  NE ' + STRING({&CDR_ERROR_DOUBLE_DATA_CDR}) + '     ' .
                /* in separate db so no need to check recid */

      lhFind:QUERY-PREPARE(lcFind).
      lhFind:QUERY-OPEN.

      REPEAT:
         lhFind:GET-NEXT.
         IF lhFind:QUERY-OFF-END THEN LEAVE.

         lcResult = fHandleDouble(lhOldCDR,
                                  "old").
         IF lcResult BEGINS "ERROR:" THEN RETURN lcResult.
      END.   
      
      lhFind:QUERY-CLOSE.
    
      DELETE OBJECT lhFind NO-ERROR.
      DELETE OBJECT lhOldCDR NO-ERROR.
   END.
     
END PROCEDURE.

