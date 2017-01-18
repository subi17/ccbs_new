/*------------------------------------------------------------------------
  MODULE .......: bob_test_invoices.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Fri Aug 29 15:47:36 EEST 2014
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/

{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{date.i}
{billrund.i NEW}

DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep    AS CHARACTER NO-UNDO INITIAL ";".
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO INITIAL 0. 
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO INITIAL 0. 

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lhandle         AS HANDLE    NO-UNDO.
DEFINE VARIABLE ldtFromDate     AS DATE      NO-UNDO.
DEFINE VARIABLE ldtToDate       AS DATE      NO-UNDO.
DEFINE VARIABLE liFeePeriod     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcBillRun       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBillRunID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liInvCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcToday         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFuncRunQAllow AS CHARACTER NO-UNDO.
DEFINE VARIABLE i               AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldtInvDate      AS DATE      NO-UNDO.

/* field variables */
DEFINE VARIABLE liCustNum AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcAction  AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

EMPTY TEMP-TABLE ttInvCust.
      
ASSIGN
   lcIncDir        = fCParam("TestInvoices","IncDir") 
   lcProcDir       = fCParam("TestInvoices","IncProcDir")
   lcSpoolDir      = fCParam("TestInvoices","OutSpoolDir")
   lcOutDir        = fCParam("TestInvoices","OutDir")
   lcFuncRunQAllow = fCParam("TestInvoices","FuncRunConfigList")
   ldtFromDate     = DATE((MONTH(TODAY)), 1 , YEAR(TODAY))
   ldtToDate       = fLastDayOfMonth(TODAY)
   liFeePeriod     = INTEGER(STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99")).

RUN lamupers.p PERSISTENT SET lhandle.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   
   liNumErr = liNumErr + 1.
    
END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcInputFile) EQ FALSE THEN NEXT.
      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.
 
   ASSIGN 
      lcToday   = STRING(YEAR(TODAY),"9999") + 
                  STRING(MONTH(TODAY),"99")  +
                  STRING(DAY(TODAY),"99")  
      lcBillRun = "TEST-BOB" + lcToday + 
                   STRING(TIME,"99999")
      lcLogFile = "test_invoice_" + lcToday + 
                  "_" + STRING(TIME) + 
                  "_" + ".log"
      lcLogFile = lcSpoolDir + lcLogFile
      lcLine    = "".

   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP
              "BILLING RUN ID: " lcBillRun SKIP.
   
   ASSIGN
      liNumOk  = 0
      liNumErr = 0.
   
   IF NOT lcFileName BEGINS lcToday THEN DO:
      fError("Incorrect input filename format"). 
      RUN pTransOnError.
      NEXT.
   END.
          
   RUN pCheckFuncRunQueue.
   
   IF RETURN-VALUE <> "" THEN DO: 
      fError(RETURN-VALUE).
      RUN pTransOnError.
      NEXT.
   END. 

   fBatchLog("START", lcInputFile).    
   
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.

      /*[CustomerNumber];[Action]*/    /* Action = CREATE/DELETE */

      ASSIGN 
         liCustNum = INTEGER(ENTRY(1,lcLine,lcSep))
         lcAction  = ENTRY(2,lcLine,lcSep)   
         NO-ERROR.
        
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.
   
      IF LOOKUP(lcAction,"CREATE,DELETE") = 0 THEN DO:
         fError(SUBST("Incorrect action: &1", lcAction)).
         NEXT. 
      END.
     
      FIND FIRST Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE Customer THEN DO:
          fError("Invalid customer").
          NEXT.
      END.   
      
      FIND Invoice WHERE
           Invoice.Brand          = gcBrand      AND  
           Invoice.CustNum        = liCustNum    AND
           MONTH(Invoice.InvDate) = MONTH(TODAY) AND  
           Invoice.InvType        = 99           EXCLUSIVE-LOCK NO-ERROR. 

      IF AVAIL Invoice AND
         NOT Invoice.BillRun BEGINS "TEST-BOB" THEN DO:
         fError("Test invoice is not generated by BOB tool").
         NEXT.
      END.
          
      CASE lcAction:
          WHEN "DELETE" THEN DO:

              IF NOT AVAILABLE Invoice THEN DO:
                  fError("Test invoice unavailable").
                  NEXT.
              END.                       
              ELSE DO:
                  
                  RUN delete_test_invoice.p (Invoice.ExtInvId,
                                             Invoice.ExtInvId,
                                             ldtInvDate,
                                             0,
                                             0,
                                             "",
                                             OUTPUT liInvCount).
              
                  IF liInvCount <> 1 THEN DO:
                      fError("Error in deleting test invoice").
                      NEXT. 
                  END.    
               END.                                          
          END.
          WHEN "CREATE" THEN DO:

              IF AVAILABLE Invoice THEN DO:
                  RUN delete_test_invoice.p (Invoice.ExtInvId,
                                             Invoice.ExtInvId,
                                             ldtInvDate,
                                             0,
                                             0,
                                             "",
                                             OUTPUT liInvCount).
                  
                  IF liInvCount <> 1 THEN DO:
                      fError("Error in deleting test invoice").
                      NEXT.
                  END.    
              END.        
              
              lcBillRunID = "*" + lcBillRun.
                
              EMPTY TEMP-TABLE ttInvCust.
              
              FIND FIRST InvGroup WHERE 
                         InvGroup.InvGroup = Customer.InvGroup NO-LOCK NO-ERROR.
                          
              CREATE ttInvCust.
              ASSIGN ttInvCust.CustNr  = Customer.CustNum
                     ttInvCust.MinInv  = IF AVAILABLE InvGroup THEN InvGroup.MinInvAmt 
                                         ELSE 0 
                     ttInvCust.CallQty = 0
                     ttInvCust.LowVal  = FALSE.
              
              FIND ttInvCust NO-LOCK NO-ERROR.
              IF NOT AVAIL ttInvCust THEN DO:
                 fError("Customer initialization failed").
                 NEXT.
              END.
   
              RUN bundle_first_month_fee.p(ldtFromDate,
                                           ldtTodate,
                                           ttInvCust.CustNr,
                                           0,
                                           0,
                                           "",
                                           OUTPUT i).
             
              IF RETURN-VALUE BEGINS "ERROR" THEN DO:
                 fError("Calculation for bundle first month fee failed").
                 NEXT.
              END.

              /* If customer has DSS active then calculate Bundle fee */
              /* based on the DSS total consumption                   */
              RUN dss_bundle_first_month_fee.p(ldtFromDate,
                                               ldtToDate,
                                               ttInvCust.CustNr,
                                               0,
                                               0,
                                               "",
                                               OUTPUT i).
             
              IF RETURN-VALUE BEGINS "ERROR" THEN DO:
                 fError("Calculation for DSS bundle first month fee failed").
                 NEXT.
              END.

              RUN pCreateTestInv IN lhandle("",             /* msseq list */
                                            TODAY,
                                            TODAY,
                                            ldtFromDate,
                                            ldtToDate,
                                            liFeePeriod,
                                            0,
                                            FALSE,
                                            FALSE,
                                            1,                /* liCustQty */
                                            lcBillRunID).
                                            
              FIND Invoice WHERE 
                   Invoice.Brand          = gcBrand      AND 
                   Invoice.CustNum        = liCustNum    AND
                   MONTH(Invoice.InvDate) = MONTH(TODAY) AND  
                   Invoice.InvType        = 99           NO-LOCK NO-ERROR.
              
              IF NOT AVAILABLE Invoice THEN DO: 
                  fError("Error in creating test invoice").
                  NEXT. 
              END.                          
          END.                    
      
      END CASE.
      
      IF RETURN-VALUE BEGINS "ERROR" THEN 
         fError(ENTRY(2,RETURN-VALUE,":")).
      ELSE 
         liNumOK = liNumOK + 1 .
     
   END.
  
   RUN invoice_xml_testbill.p (TODAY,
                               lcBillRun) NO-ERROR.
   
   PUT STREAM sLog UNFORMATTED 
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK)          ", "
       "errors: " STRING(liNumErr) SKIP.
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).
    
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
END.

INPUT STREAM sFile CLOSE.

PROCEDURE pCheckFuncRunQueue:
    
   FOR EACH FuncRunQueue NO-LOCK WHERE
      LOOKUP(STRING(FuncRunQueue.FRQueueID),lcFuncRunQAllow) = 0,
      EACH FuncRunQSchedule NO-LOCK WHERE 
           FuncRunQSchedule.FRQueueID = FuncRunQueue.FRQueueID AND 
           LOOKUP(FuncRunQSchedule.RunState,"Scheduled,Initialized,Running") > 0:
      RETURN "Bill Run already scheduled".              
   END.           
  
   RETURN "".
   
END PROCEDURE.

PROCEDURE pTransOnError:
    
   PUT STREAM sLog UNFORMATTED 
      "input: " STRING(liNumOK + liNumErr) ", "
      "updated: " STRING(liNumOK)          ", "
      "errors: " STRING(liNumErr) SKIP.
   
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).
       
END PROCEDURE.    
