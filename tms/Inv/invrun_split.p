/* ----------------------------------------------------------------------
  MODULE .......: INVRUN_SPLIT
  TASK .........: Creates customer split files
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 21.01.08
  CHANGED ......: 29.01.08 kl use SaldoCounters
                  04.09.08/aam input parameters, cparam, run from ui            
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{files.i}
{timestamp.i}
{funcrunprocess_update.i}
{old_unbilled_events.i}

DEF INPUT  PARAMETER idaInvDate    AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaDueDate    AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaDateFrom   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaDateTo     AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiFeePeriod   AS INT  NO-UNDO.
DEF INPUT  PARAMETER ilNormalInv   AS LOG  NO-UNDO.
DEF INPUT  PARAMETER iiScreenQty   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiFRExecID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiCustomerQty AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCustPickMode AS INT  NO-UNDO.
DEF INPUT  PARAMETER icTestFile    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocFileList    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiCustCnt     AS INT  NO-UNDO. 

DEFINE VARIABLE liRunAmt    AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeCallPerc AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liRunLim    AS INTEGER   NO-UNDO.
DEFINE VARIABLE liCallGroup AS INTEGER   NO-UNDO.
DEFINE VARIABLE liAllGroup  AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldPeriodBeg AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldPeriodEnd AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcTemp      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liTemp      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcDumpPath  AS CHARACTER NO-UNDO.
DEFINE VARIABLE llBill      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcParams    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCustCnt   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liAdd       AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttInvCust NO-UNDO
   FIELD CustNum  AS INTEGER
   FIELD CallQty  AS INTEGER
   FIELD BatchNbr AS INTEGER
   INDEX CustNum  AS PRIMARY CustNum
   INDEX CallQty CallQty DESCENDING
   INDEX BatchNbr BatchNbr CallQty DESCENDING.

DEF TEMP-TABLE ttCustNum NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.

DEF TEMP-TABLE ttPeriod NO-UNDO
   FIELD Period AS INT
   INDEX Period Period DESC.
         
DEFINE STREAM sDump.

FORM  
   liCustCnt COLUMN-LABEL "Customer Qty" 
   WITH 1 DOWN ROW 8 CENTERED TITLE " Collecting " 
   OVERLAY FRAME fQty.


FUNCTION fOpenFile RETURNS CHARACTER():

   DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO.

   lcFileName = lcDumpPath + "/invrun_" + 
                STRING(idaInvDate,"999999") + "_" + 
                STRING(liTemp,"99") + ".txt".
   
   OUTPUT STREAM sDump CLOSE.
   OUTPUT STREAM sDump TO VALUE(lcFileName).
   
   PUT STREAM sDump UNFORMATTED lcParams CHR(10).
   
   ocFileList = ocFileList + (IF ocFileList > "" THEN "," ELSE "") +
                             lcFileName.
END.


/********** Main start **********/

ASSIGN
   liRunAmt    = 10
   ldPeriodBeg = fMake2Dt(idaDateFrom,0)
   ldPeriodEnd = fMake2Dt(idaDateTo,86399).
   
IF iiScreenQty > 0 THEN liRunAmt = iiScreenQty.

IF iiFRProcessID = 0 THEN DO:

   lcDumpPath  = fCParamC("SplitInvRunDir").
   IF lcDumpPath = ? OR lcDumpPath = "" THEN lcDumpPath = "/tmp".
   
   lcParams = STRING(idaInvDate,"99.99.99") + "," + 
              STRING(idaDateFrom,"99.99.99") + "," + 
              STRING(idaDateTo,"99.99.99") + "," + 
              STRING(iiFeePeriod) + "," + 
              STRING(liRunAmt) + "," + 
              STRING(NOT ilNormalInv) + "," + 
              (IF idaDueDate NE ?
               THEN STRING(idaDueDate,"99.99.99")
               ELSE "").

   /* check that there aren't any old files left */
   IF fHasDirectoryFiles(lcDumpPath,"invrun*") THEN 
      RETURN "ERROR:Directory " + lcDumpPath + " already contains files".
END.

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.

IF icTestFile > "" THEN DO:
   RUN pCustomersFromFile.
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
END.

ELSE DO:
   RUN pCustomersWithCounters.
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

   RUN pCustomersWithoutCounters.
   IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
END.

RUN pDivideIntoGroups.

IF NOT SESSION:BATCH THEN DO:
   HIDE FRAME fQty NO-PAUSE.
   
   DO liTemp = 20 TO 22:
      PUT SCREEN ROW liTemp COL 1 FILL(" ",78).
   END.   
END.   

RETURN "".

/********** Main end *******/


PROCEDURE pCustomersWithCounters:

   DEF VAR liCallQty AS INT  NO-UNDO.
   
   IF NOT SESSION:BATCH THEN 
      PUT SCREEN ROW 20 COL 1 
         STRING(TIME,"HH:MM:SS") + 
         ' 1: Checking saldocounters for period ' + 
         STRING(iiFeePeriod).

   /* first get pending STCs -> handle those last in billing run */
   IF icRunMode NE "test" THEN 
   FOR EACH MsRequest NO-LOCK USE-INDEX ReqType WHERE
            MsRequest.Brand    = gcBrand AND
            MsRequest.ReqType  = 0       AND
            MsRequest.ReqStat  = 8,
      FIRST MobSub NO-LOCK WHERE
            MobSub.MsSeq = MsRequest.MsSeq AND
            MobSub.PayType = FALSE:

      /* create ttInvCust without further checks for billable events; qty of 
         pending STCs per month is not large */
      FIND FIRST ttInvCust WHERE ttInvCust.CustNum = MobSub.InvCust NO-ERROR.
      IF NOT AVAILABLE ttInvCust THEN DO:
         CREATE ttInvCust.
         ASSIGN
            ttInvCust.CustNum = MobSub.InvCust
            ttInvCust.CallQty = -1
            liCustCnt         = liCustCnt + 1.
      END.
   END.

   GetCounterCustomers:
   FOR EACH InvGroup NO-LOCK WHERE
            InvGroup.Brand    = gcBrand AND
            InvGroup.BillPerm = TRUE,
       EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand AND
            Customer.InvGroup = InvGroup.InvGroup:
            
      FOR EACH MsOwner NO-LOCK USE-INDEX InvCust WHERE
               MsOwner.InvCust = Customer.CustNum AND   
               MsOwner.PayType = FALSE
      BREAK BY MsOwner.MsSeq:
               
         IF NOT FIRST-OF(MsOwner.MsSeq) THEN NEXT.         
            
         liCallQty = 0.
         FOR EACH SaldoCounter NO-LOCK WHERE
                  SaldoCounter.MsSeq  = MsOwner.MsSeq AND
                  SaldoCounter.Period = iiFeePeriod AND
                  SaldoCounter.Qty > 0:
            liCallQty = liCallQty + SaldoCounter.Qty.
         END.
      
         IF liCallQty = 0 THEN
         FOR FIRST InvSeq NO-LOCK WHERE
                   InvSeq.MsSeq = MsOwner.MsSeq AND
                   InvSeq.CustNum = MsOwner.InvCust AND
                   InvSeq.Billed = FALSE AND
                   InvSeq.FromDate < idaDateTo:
            liCallQty = 1.       
         END.
                
         IF liCallQty = 0 THEN NEXT. 
      
         FIND FIRST ttInvCust WHERE
                    ttInvCust.CustNum = Customer.CustNum
         EXCLUSIVE-LOCK NO-ERROR.
      
         IF NOT AVAIL ttInvCust THEN DO:
      
            CREATE ttInvCust.
            ttInvCust.CustNum  = Customer.CustNum.

            liCustCnt = liCustCnt + 1.
            IF NOT SESSION:BATCH AND liCustCnt MOD 1000 = 0 THEN DO:
               PAUSE 0.
               DISP liCustCnt WITH FRAME fQty.
            END.
    
            IF iiUpdInterval > 0 AND liCustCnt MOD iiUpdInterval = 0 
            THEN DO:
               IF NOT fUpdateFuncRunProgress(iiFRProcessID,liCustCnt) THEN 
                  RETURN "ERROR:Stopped".
            END.   
         END. 
   
         IF ttInvCust.CallQty >= 0 THEN 
             ttInvCust.CallQty = ttInvCust.CallQty + liCallQty.
         ELSE ttInvCust.CallQty = ttInvCust.CallQty - liCallQty.   
      
         IF icRunMode = "test" AND liCustCnt >= iiCustomerQty AND
           iiCustPickMode = 0 THEN LEAVE GetCounterCustomers.
      END.
   END.
   
   RETURN "".

END PROCEDURE.

PROCEDURE pCustomersWithoutCounters:

   DEF VAR ldaOldEventDate  AS DATE NO-UNDO.
   DEF VAR liOldEventPeriod AS INT  NO-UNDO.
   DEF VAR liPeriod         AS INT  NO-UNDO.
          
   IF NOT SESSION:BATCH THEN 
      PUT SCREEN ROW 21 COL 1 
         STRING(TIME,"HH:MM:SS") + 
         ' 2: Checking customers with no saldocounters during that period'.

   IF icRunMode = "test" AND liCustCnt >= iiCustomerQty AND
      iiCustPickMode = 0 THEN RETURN.

   ldaOldEventDate = fOldUnbilledEventLimit(INT(ilNormalInv = TRUE)).
   IF ldaOldEventDate = ? THEN ldaOldEventDate = 1/1/2006.
   liOldEventPeriod = YEAR(ldaOldEventDate) * 100 + MONTH(ldaOldEventDate).

   DO liPeriod = liOldEventPeriod TO iiFeePeriod:
      IF liPeriod MOD 100 > 12 THEN DO:
         liPeriod = (TRUNCATE(liPeriod / 100,0) + 1) * 100.
         NEXT.
      END.
                                
      CREATE ttPeriod.
      ttPeriod.Period = liPeriod.
   END.

   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand = gcBrand AND
            NOT CAN-FIND(FIRST ttInvCust WHERE
                               ttInvCust.CustNum = Customer.CustNum),
      FIRST InvGroup OF Customer NO-LOCK WHERE
            InvGroup.BillPerm = TRUE:

      FOR EACH ttPeriod:
         llBill = CAN-FIND(FIRST SingleFee WHERE
                              SingleFee.CustNum = Customer.CustNum AND
                              SingleFee.BillPeriod = ttPeriod.Period AND
                              SingleFee.Billed = FALSE).
         IF NOT llBill THEN 
            llBill = CAN-FIND(FIRST FFItem WHERE
                                 FFItem.CustNum = Customer.CustNum AND 
                                 FFItem.BillPeriod = ttPeriod.Period AND
                                 FFItem.Billed = FALSE).
         IF llBill THEN LEAVE.
      END.
      
      IF NOT llBill THEN 
         llBill = CAN-FIND(FIRST MsOwner WHERE
                              MsOwner.InvCust = Customer.CustNum AND
                              MsOwner.TsBeg  <= ldPeriodEnd AND
                              MsOwner.TsEnd  >= ldPeriodBeg AND
                              MsOwner.PayType = FALSE).

      IF llBill THEN DO:

         CREATE ttInvCust.
         ttInvCust.CustNum  = Customer.CustNum.

         liCustCnt = liCustCnt + 1.
         IF NOT SESSION:BATCH AND liCustCnt MOD 1000 = 0 THEN DO:
            PAUSE 0.
            DISP liCustCnt WITH FRAME fQty.
         END.

         IF iiUpdInterval > 0 AND liCustCnt MOD iiUpdInterval = 0 
         THEN DO:
            IF NOT fUpdateFuncRunProgress(iiFRProcessID,liCustCnt) THEN
               RETURN "ERROR:Stopped".
         END.   

         IF icRunMode = "test" AND liCustCnt >= iiCustomerQty AND
            iiCustPickMode = 0 THEN LEAVE.
         
      END.
   END.

   RETURN "". 

END PROCEDURE.

PROCEDURE pCustomersFromFile:

   DEF VAR lcCustNum   AS CHAR NO-UNDO.
   DEF VAR liCustNum   AS INT  NO-UNDO.
   
   FILE-INFO:FILE-NAME = icTestFile.
   IF NOT FILE-INFO:FILE-TYPE BEGINS "F" THEN
      RETURN "ERROR:File containing test customer numbers not found".
      
   INPUT STREAM sDump FROM VALUE(icTestFile).
   
   REPEAT:
   
      IMPORT STREAM sDump lcCustNum.
      IF lcCustNum = "" THEN NEXT.
      
      liCustNum = INT(lcCustNum).

      IF CAN-FIND(FIRST ttCustNum WHERE
                        ttCustNum.CustNum= liCustNum) THEN NEXT.
      
      CREATE ttCustNum.
             ttCustNum.CustNum = liCustNum.
   END.

   INPUT STREAM sDump CLOSE.
   
   FOR EACH ttCustNum,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = ttCustNum.CustNum,
      FIRST InvGroup NO-LOCK WHERE
            InvGroup.Brand    = gcBrand AND
            InvGroup.InvGroup = Customer.InvGroup AND
            InvGroup.BillPerm = TRUE:
            
      FIND FIRST SaldoCounter WHERE
            SaldoCounter.MsSeq  = MsOwner.MsSeq AND
            SaldoCounter.Period = iiFeePeriod NO-LOCK NO-ERROR.

      FIND FIRST ttInvCust WHERE
                 ttInvCust.CustNum = Customer.CustNum
      EXCLUSIVE-LOCK NO-ERROR.
      
      IF NOT AVAIL ttInvCust THEN DO:
      
         CREATE ttInvCust.
         ttInvCust.CustNum  = Customer.CustNum.

         liCustCnt = liCustCnt + 1.
         IF NOT SESSION:BATCH AND liCustCnt MOD 1000 = 0 THEN DO:
            PAUSE 0.
            DISP liCustCnt WITH FRAME fQty.
         END.
    
         IF iiUpdInterval > 0 AND liCustCnt MOD iiUpdInterval = 0 
         THEN DO:
            IF NOT fUpdateFuncRunProgress(iiFRProcessID,liCustCnt) THEN 
               RETURN "ERROR:Stopped".
         END.   
      END. 
   
      ASSIGN
         ttInvCust.CallQty   = ttInvCust.CallQty + 
                               (IF AVAILABLE SaldoCounter
                                THEN SaldoCounter.Qty
                                ELSE 0).
      
   END.
   
   RETURN "".

END PROCEDURE.

PROCEDURE pDivideIntoGroups:

   DEF VAR llCheckQty AS LOG  NO-UNDO.
    
   IF NOT SESSION:BATCH THEN 
      PUT SCREEN ROW 22 COL 1
         STRING(TIME,"HH:MM:SS") + ' 3: Creating invoice run files'.

   ASSIGN
      lcTemp = ""
      liTemp = 0
      liAdd  = 1
      liCustCnt = 0
      llCheckQty = (icRunMode = "test" AND icTestFile = "" AND 
                    iiCustPickMode = 1).

   /* divide evenly to groups using descending cdr qty order 
      if e.g. 4 groups then order sequence will be 12344321123443.. */
   FOR EACH ttInvCust NO-LOCK USE-INDEX CallQty:

      liCustCnt = liCustCnt + 1.
      IF llCheckQty AND liCustCnt > iiCustomerQty THEN DO:
         DELETE ttInvCust.
         NEXT.
      END.
      
      liTemp = liTemp + liAdd.
      IF liTemp > liRunAmt THEN ASSIGN
         liTemp = liRunAmt
         liAdd  = -1.
      ELSE IF liTemp = 0 THEN ASSIGN
         liTemp = 1
         liAdd  = 1.
   
      ttInvCust.BatchNbr = liTemp.
      
   END.

   IF iiFRProcessID > 0 THEN DO:
      /* write each batch to its own feed group */
      liTemp = 0.
      FOR EACH ttInvCust NO-LOCK USE-INDEX BatchNbr:

         IF liTemp NE ttInvCust.BatchNbr THEN ASSIGN
            liTemp    = ttInvCust.BatchNbr
            liCustCnt = 0.

         CREATE FuncRunResult.
         ASSIGN 
            FuncRunResult.FRProcessID = iiFRProcessID
            FuncRunResult.FRExecID    = iiFRExecID
            FuncRunResult.FRResultSeq = ttInvCust.BatchNbr
            FuncRunResult.IntParam    = ttInvCust.CustNum
            liCustCnt                 = liCustCnt + 1
            FuncRunResult.ResultOrde  = liCustCnt
            FuncRunResult.DecParam    = ttInvCust.CallQty
            oiCustCnt                 = oiCustCnt + 1.
      END.
   END.

   ELSE DO:
      /* write each batch to a separate file */
      liTemp = 0.
      FOR EACH ttInvCust NO-LOCK USE-INDEX BatchNbr:

         IF liTemp NE ttInvCust.BatchNbr THEN DO:
            ASSIGN
               liTemp    = ttInvCust.BatchNbr
               liRunLim  = 0
               liCustCnt = 0.
         
            fOpenFile().
         END.

         ASSIGN 
            liRunLim  = liRunLim + ttInvCust.CallQty
            liCustCnt = liCustCnt + 1
            oiCustCnt = oiCustCnt + 1.

         PUT STREAM sDump UNFORMATTED
            ttInvCust.CustNum CHR(9)
            ttInvCust.CallQty CHR(10).
      END.

      OUTPUT STREAM sDump CLOSE.
   END.
   
END PROCEDURE.


