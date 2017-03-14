/* ----------------------------------------------------------------------
  MODULE .......: ifs_payment_status.p
  TASK .........: Read in payment status from ifs
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 24.06.09
  CHANGED ......: 16.09.14 iv Clear Xferred stamp with ClaimStatus change
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
END.

DEF INPUT  PARAMETER icFile    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilControl AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiRead    AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrors  AS INT  NO-UNDO.

DEF VAR liCustNum          AS INT   NO-UNDO.
DEF VAR liMsSeq            AS INT   NO-UNDO.
DEF VAR lcInvID            AS CHAR  NO-UNDO.
DEF VAR lcInvDate          AS CHAR  NO-UNDO.
DEF VAR lcDueDate          AS CHAR  NO-UNDO.
DEF VAR lcPaidAmount       AS CHAR  NO-UNDO.
DEF VAR lcUnpaidAmount     AS CHAR  NO-UNDO.
DEF VAR lcRejectionDesc    AS CHAR  NO-UNDO.
DEF VAR ldtInvDate         AS DATE  NO-UNDO.
DEF VAR ldtDueDate         AS DATE  NO-UNDO.
DEF VAR ldPaidAmount       AS DEC   NO-UNDO.
DEF VAR ldUnpaidAmount     AS DEC   NO-UNDO.
DEF VAR lcLogFile          AS CHAR  NO-UNDO.
DEF VAR lcPlainFile        AS CHAR  NO-UNDO.
DEF VAR lcReadLine         AS CHAR  NO-UNDO.
DEF VAR lcArcDir           AS CHAR  NO-UNDO.
DEF VAR lcTransDir         AS CHAR  NO-UNDO.
DEF VAR llFound            AS LOG   NO-UNDO.
DEF VAR lcClaimStatus      AS CHAR  NO-UNDO.
DEF VAR liBankAcc          AS INT   NO-UNDO.
DEF VAR lrActionID         AS RECID NO-UNDO.
DEF VAR lcCompanyID        AS CHAR  NO-UNDO.

DEF STREAM sRead.
DEF STREAM sLog.

FORM 
   oiRead   AT 2 FORMAT ">>>>>>>>9" LABEL "Rows Read" SKIP
   oiErrors AT 2 FORMAT ">>>>>>>>9" LABEL "Errors .." SKIP
   WITH OVERLAY CENTERED ROW 10 SIDE-LABELS TITLE " IMPORT " FRAME fQty.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcReadLine  ";"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
   oiErrors = oiErrors + 1.
END FUNCTION.


/******* Main start ********/

IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

RUN pInitialize.

/* check that there isn't already another run handling this file */
IF CAN-FIND(FIRST ActionLog USE-INDEX TableName WHERE
                  ActionLog.Brand        = gcBrand      AND    
                  ActionLog.TableName    = "Invoice"    AND
                  ActionLog.KeyValue     = lcPlainFile  AND
                  ActionLog.ActionID     = "IFSPAYSTAT" AND
                  ActionLog.ActionStatus = 0)
THEN RETURN.

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "Invoice"  
      ActionLog.KeyValue     = lcPlainFile
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "IFSPAYSTAT"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionStatus = 0.
      ActionLog.ActionTS     = fMakeTS().
      lrActionID             = RECID(ActionLog).
END.

INPUT STREAM sRead FROM VALUE(icFile).
OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   lcPlainFile  " "
   STRING(TODAY,"99.99.99") " "
   STRING(TIME,"hh:mm:ss") SKIP.

IF NOT SESSION:BATCH THEN DO:
   PAUSE 0.
   VIEW FRAME fQty.
END.

RUN pReadEvents.

fCleanEventObjects().

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

INPUT STREAM sRead CLOSE.
OUTPUT STREAM sLog CLOSE.

/* move to archive */
IF oiRead > 0 AND lcArcDir > "" THEN DO:   
   fTransDir(icFile,
             "",
             lcArcDir).
END.

IF lcTransDir > "" THEN DO:
   fTransDir(lcLogFile,
             "",
             lcTransDir).
END.
 
DO TRANS:
   FIND FIRST ActionLog WHERE RECID(ActionLog) = lrActionID EXCLUSIVE-LOCK.
   ASSIGN 
      ActionLog.ActionDec    = oiRead
      ActionLog.ActionChar   = lcPlainFile + CHR(10) + 
                               "Handled: " + STRING(oiRead) + CHR(10) + 
                               " Errors: " + STRING(oiErrors)  
      ActionLog.ActionStatus = 3.
END.


/******* Main end ********/


PROCEDURE pInitialize:
 
   DEF VAR liSeq   AS INT NO-UNDO.
   DEF VAR ldToday AS DEC NO-UNDO.
 
   IF llDoEvent THEN DO:
      RUN StarEventInitialize(lhInvoice).
      RUN StarEventInitialize(lhPayment).
   END.

   ASSIGN
      lcLogFile  = fCParamC("IFSPaymStatusLog")
      lcTransDir = fCParamC("IFSPaymStatusLogTrans")
      lcArcDir   = fCParamC("IFSPaymStatusArc")
      liBankAcc  = fCParamI("BankAcc")
      ldToday    = fMake2DT(TODAY,1).

   IF lcLogFile = ? OR lcLogFile = "" THEN 
      lcLogFile = "/tmp/IFS_paymstatus_#DATE.log".

   liSeq = 1.
   FOR EACH ActionLog NO-LOCK WHERE
            ActionLog.Brand    = gcBrand      AND
            ActionLog.ActionID = "IFSPAYSTAT" AND
            ActionLog.ActionTS >= ldToday:
      liSeq = liSeq + 1.
   END.
 
   ASSIGN
      lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                            STRING(MONTH(TODAY),"99") +
                                            STRING(DAY(TODAY),"99"))
      lcLogFile = REPLACE(lcLogFile,"#SEQ",STRING(liSeq,"999")). 
    
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(TODAY,"999999")).

   /* file without the dir */
   lcPlainFile = icFile.
   IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
 
END PROCEDURE. /* pInitialize */

PROCEDURE pReadEvents:

   REPEAT:

      lcReadLine = "".
    
      IMPORT STREAM sRead UNFORMATTED lcReadLine.
   
      oiRead = oiRead + 1.
   
      IF NOT SESSION:BATCH AND 
         (oiRead < 100 OR oiRead MOD 100 = 0) THEN DO:
         PAUSE 0.
         DISP oiRead oiErrors WITH FRAME fQty.
      END.
   
      IF lcReadLine = "" THEN DO:
         fError("Empty line").
         NEXT.
      END.

      ASSIGN 
         liCustNum      = INTEGER(SUBSTRING(lcReadLine,1,10))  
         liMsSeq        = INTEGER(SUBSTRING(lcReadLine,11,12))  
         lcInvID        = LEFT-TRIM(SUBSTRING(lcReadLine,23,16),"0")
         lcInvDate      = SUBSTRING(lcReadLine,39,8)             
         lcDueDate      = SUBSTRING(lcReadLine,47,8)             
         lcPaidAmount   = SUBSTRING(lcReadLine,55,16)            
         lcUnpaidAmount = SUBSTRING(lcReadLine,71,16)            
         lcClaimStatus  = TRIM(SUBSTRING(lcReadLine,87,6))
         lcCompanyID    = SUBSTRING(lcReadLine,93,4)
         NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Invalid format").
         NEXT.
      END.

      FIND FIRST Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN DO:
         fError("Unknown customer").
         NEXT.
      END.

      FIND FIRST Invoice WHERE 
                 Invoice.Brand    = gcBrand AND
                 Invoice.ExtInvID = lcInvID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice THEN DO:
         fError("Unknown invoice").
         NEXT.
      END.
    
      IF liMsSeq > 0 THEN DO:
         FIND FIRST MsOwner USE-INDEX InvCust WHERE 
                    MsOwner.InvCust = Customer.CustNum AND
                    MsOwner.MsSeq   = liMsSeq NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MsOwner THEN DO:
      
            llFound = FALSE.
            
            /* subscription for cash invoices may not have been created */
            FOR FIRST Order NO-LOCK USE-INDEX MsSeq WHERE
                      Order.MsSeq   = liMsSeq AND
                      Order.CustNum = liCustNum:
               llFound = TRUE.
            END.
         
            IF NOT llFound THEN DO:
               fError("Subscription not available").
               NEXT.
            END.   
         END.

         IF NOT CAN-FIND(FIRST SubInvoice OF Invoice WHERE 
                               SubInvoice.MsSeq = liMsSeq) THEN DO:
            fError("Subscription not included in the invoice").
            NEXT.
         END.
      END.
   
      ASSIGN
         ldtInvDate     = ?
         ldtDueDate     = ?
         ldPaidAmount   = 0
         ldUnpaidAmount = 0.
 
      ASSIGN 
         ldtInvDate = DATE(INTEGER(SUBSTRING(lcInvDate,5,2)),
                           INTEGER(SUBSTRING(lcInvDate,7,2)),
                           INTEGER(SUBSTRING(lcInvDate,1,4)))
         ldtDueDate = DATE(INTEGER(SUBSTRING(lcDueDate,5,2)),
                           INTEGER(SUBSTRING(lcDueDate,7,2)),
                           INTEGER(SUBSTRING(lcDueDate,1,4)))
         NO-ERROR.
                           
      IF ERROR-STATUS:ERROR OR ldtInvDate = ? OR ldtDueDate = ? THEN DO:
         fError("Invalid date").
         NEXT.
      END.

      IF Invoice.CustNum NE liCustNum OR
         Invoice.InvDate NE ldtInvDate OR
         Invoice.DueDate NE ldtDueDate 
      THEN DO:
         fError("Conflict in invoice data").
         NEXT.
      END.

      /* ifs uses dot as decimal point */
      IF SESSION:NUMERIC-FORMAT = "european" THEN ASSIGN
         lcPaidAmount   = REPLACE(lcPaidAmount,".",",")
         lcUnpaidAmount = REPLACE(lcUnpaidAmount,".",",").
   
      ASSIGN
         ldPaidAmount   = DECIMAL(lcPaidAmount) 
         ldUnpaidAmount = DECIMAL(lcUnpaidAmount)
         NO-ERROR.                        
      
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Invalid Amount").
         NEXT.
      END.

      IF ldPaidAmount + ldUnpaidAmount NE Invoice.InvAmt THEN DO:
         fError("Conflict in invoice amount").
         NEXT.
      END.

      /* YDR-1665 */
      IF lcClaimStatus BEGINS "1." THEN DO:

         lcRejectionDesc = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                            "Invoice",
                                            "ClaimStatus",
                                            lcClaimStatus).

         CREATE Memo.
         ASSIGN Memo.Brand     = gcBrand
                Memo.HostTable = "Invoice"
                Memo.KeyValue  = STRING(Invoice.InvNum)
                Memo.CustNum   = Invoice.CustNum
                Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
                Memo.CreUser   = katun
                Memo.MemoTitle = "Invoice rejection"
                Memo.MemoText  = STRING(Invoice.InvNum) +
                                 ": " + lcClaimStatus +
                                 "; " + lcRejectionDesc.
                Memo.CreStamp  = fMakeTS().
      END.

      RUN pUpdateStatus(Invoice.InvNum,
                        ldPaidAmount,
                        lcClaimStatus).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
      END.
      ELSE DO:
      END.
         
   END.

END PROCEDURE. /* pReadEvents */

PROCEDURE pUpdateStatus:

   DEF INPUT PARAMETER iiInvNum      AS INT  NO-UNDO.
   DEF INPUT PARAMETER idPaidAmt     AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icClaimStatus AS CHAR NO-UNDO.

   DEF VAR liVoucher AS INT  NO-UNDO.

   DO TRANS:
      FIND FIRST Invoice WHERE Invoice.InvNum = iiInvNum EXCLUSIVE-LOCK.

      IF Invoice.PaidAmt NE idPaidAmt THEN DO:

         RUN Ar/makepaym.p (BUFFER Invoice,
                       idPaidAmt - Invoice.PaidAmt,
                       TODAY,
                       liBankAcc,
                       "IFS",
                       1,
                       FALSE,
                       FALSE,
                       "",
                       "", 
                       OUTPUT liVoucher).

         IF liVoucher = 0 THEN RETURN "ERROR:Payment creation failed".
                       
      END.
      
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

      ASSIGN Invoice.ClaimStatus = icClaimStatus
             Invoice.ExpStamp = 0.
      
      IF icClaimStatus > "" AND
         NOT icClaimStatus BEGINS "0" THEN
         Invoice.DDState = 2.
       
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).

      RELEASE Invoice.
   END.

   RETURN "".

END PROCEDURE.  /* pUpdateStatus */

 
