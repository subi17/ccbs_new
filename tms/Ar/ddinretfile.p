/* -----------------------------------------------------------------------
  MODULE .......: ddinretfile.p
  FUNCTION .....: Read in returned (failed) payments from dd (CSB19)
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 08.03.07
  CHANGED . ....: 
  VERSION ......: Yoigo
 ---------------------------------------------------------------------------- */

{Syst/commali.i}
{Ar/paymfile.i}
{Func/cparam2.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER  icFile     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER  iiAccNum   AS INT  NO-UNDO.
DEF INPUT  PARAMETER  icLogPref  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER  TABLE FOR ttPayment.
DEF OUTPUT PARAMETER  oiDone     AS INT  NO-UNDO.

DEF STREAM sRead.

DEF VAR lcExtInvID    AS CHAR NO-UNDO.
DEF VAR liInvNum      AS INT  NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO. 
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcLine        AS CHAR NO-UNDO.
DEF VAR lcCompanyID   AS CHAR NO-UNDO.
DEF VAR ldInvAmt      AS DEC  NO-UNDO.
DEF VAR lcOrderID     AS CHAR NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO.
DEF VAR lcErrorFile   AS CHAR NO-UNDO.
DEF VAR llFirstError  AS LOG  NO-UNDO INIT TRUE.
DEF VAR liRead        AS INT  NO-UNDO.
DEF VAR lcErrorDescr  AS CHAR NO-UNDO.
DEF VAR lcFileCompany AS CHAR NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR ldTotalAmt    AS DEC  NO-UNDO.
DEF VAR ldCurrStamp   AS DEC  NO-UNDO.
DEF VAR ldtFileDate   AS DATE NO-UNDO.

DEF STREAM sError.


FUNCTION fErrorLog RETURNS DECIMAL
   (icMessage AS CHAR):

   DEF VAR lcErrCust AS CHAR NO-UNDO.

   OUTPUT STREAM sError TO VALUE(lcErrorFile) APPEND.
   
   IF llFirstError THEN DO:
   
      PUT STREAM sError UNFORMATTED
         "Started: "  STRING(TIME,"hh:mm:ss") SKIP
         "Invoice"      CHR(9)
         "Inv.Key"      CHR(9)
         "Customer"     CHR(9)
         "Name"         CHR(9)
         "Amount"       CHR(9)
         "DD Error"     CHR(9)
         "Read Error"   SKIP.
         
      llFirstError = FALSE.
   END.
   
   lcErrCust = "".
   IF liCustNum > 0 THEN DO:
      FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         lcErrCust = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                      BUFFER Customer).
   END.
   
   PUT STREAM sError UNFORMATTED
      lcExtInvID   CHR(9)
      liInvNum     CHR(9)
      liCustNum    CHR(9)
      lcErrCust    CHR(9)
      ldInvAmt     CHR(9)
      lcError      CHR(9)
      icMessage    SKIP.
     
   OUTPUT STREAM sError CLOSE.

   DO TRANS:
      /* save to db for reporting */
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = gcBrand
             ErrorLog.ActionID  = "DDRET"
             ErrorLog.TableName = "Invoice"
             ErrorLog.KeyValue  = STRING(liInvNum)
             ErrorLog.ActionTS  = ldCurrStamp
             ErrorLog.UserCode  = katun
             ErrorLog.ErrorMsg  = icMessage.
   END.
      
   ttPayment.ErrorTxt = icMessage.
   
END FUNCTION.


IF SEARCH(icFile) = ? THEN RETURN "ERROR:File not found".

/* posting date from file name */
ldtFileDate = ?.
DO liCount = YEAR(TODAY) - 1 TO YEAR(TODAY) + 1:
   liRead = INDEX(icFile,STRING(liCount,"9999")).
   IF liRead > 0 THEN DO:
      ldtFileDate = DATE(INTEGER(SUBSTRING(icFile,liRead + 4,2)),
                         INTEGER(SUBSTRING(icFile,liRead + 6,2)),
                         INTEGER(SUBSTRING(icFile,liRead,4))) NO-ERROR.
      LEAVE.
   END.
END.

IF ldtFileDate = ? THEN ldtFileDate = TODAY.


lcErrorFile = fCParamC("DDInErrorFile").

IF lcErrorFile = "" OR lcErrorFIle = ? THEN 
   lcErrorFile = "/tmp/ddin_error".
    
lcErrorFile = lcErrorFile + "_" + 
              STRING(YEAR(TODAY),"9999") + 
              STRING(MONTH(TODAY),"99")  +
              STRING(DAY(TODAY),"99") + 
              ".txt".

FIND FIRST Company NO-LOCK.
ASSIGN 
   lcCompanyID = REPLACE(Company.CompanyID,"-","")
   ldCurrStamp = fMakeTS()
   liRead      = 0
   ldTotalAmt  = 0.

INPUT STREAM sRead FROM VALUE(icFile).

REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.

   /* invoice lines */
   IF SUBSTRING(lcLine,1,4) = "5690" THEN DO:
       
      ASSIGN lcFileCompany = SUBSTRING(lcLine,5,9)
             liCustNum     = INTEGER(SUBSTRING(lcLine,17,12))
             lcCustName    = SUBSTRING(lcLine,29,40)
             ldInvAmt      = DECIMAL(SUBSTRING(lcLine,89,10)) / 100
             lcOrderID     = SUBSTRING(lcLine,99,6)
             liInvNum      = INTEGER(SUBSTRING(lcLine,105,10))
             lcExtInvID    = SUBSTRING(lcLine,115,40)
             lcError       = SUBSTRING(lcLine,155,1)
             NO-ERROR.
     
      IF lcExtInvID BEGINS "Fra." THEN 
         lcExtInvID = SUBSTRING(lcExtInvID,6,12).
      ELSE lcExtInvID = SUBSTRING(lcExtInvID,10,12).
      
      ASSIGN liRead       = liRead + 1
             lcErrorDescr = lcError + " " +
                             DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                              "Payment",
                                              "DDError",
                                              lcError).
 
      CREATE ttPayment.
      ASSIGN 
         ttPayment.AccDate   = ldtFileDate
         ttPayment.PaymDate  = ttPayment.AccDate
         ttPayment.Inv       = liInvNum      
         ttPayment.ExtInvID  = lcExtInvID
         ttPayment.CustNum   = liCustNum    
         ttPayment.AmtPaid   = -1 * ldInvAmt
         ttPayment.Origin    = "DDRET"
         ttPayment.FName     = icFile
         ttPayment.CustName  = lcCustName
         ttPayment.DDError   = lcErrorDescr
         ttPayment.ErrorCode = lcError
         oiDone              = oiDone + 1.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fErrorLog("Invalid data").
         NEXT.
      END.
      
      ldTotalAmt = ldTotalAmt + ldInvAmt.

      IF lcFileCompany NE lcCompanyID THEN DO:
         fErrorLog("Incorrect company ID").
         NEXT.
      END.
   
      FIND Invoice WHERE Invoice.InvNum = liInvNum NO-LOCK NO-ERROR.
   
      IF NOT AVAILABLE Invoice THEN DO:
         fErrorLog("Unknown invoice").
         NEXT.
      END.
   
      IF Invoice.ExtInvID NE lcExtInvID THEN DO:
         fErrorLog("Invalid invoice keys").
         NEXT.
      END.

      IF Invoice.DDState = 0 THEN DO:
         fErrorLog("Invoice has not been sent to DD").
         NEXT. 
      END.
      
      IF Invoice.ClaimStatus NE "" AND 
         Invoice.ClaimStatus NE "0.0" THEN DO:
         fErrorLog("Invoice is already in claiming process").
         NEXT.
      END.

      IF Invoice.DDState = 2 THEN DO:
         fErrorLog("DD has already been cancelled").
         NEXT. 
      END.
       
      IF Invoice.CustNum NE liCustNum THEN DO:
         fErrorLog("Invalid customer data").
         NEXT.
      END.
                              
   END.  /* 5690=invoice line */
   
   /* total */
   ELSE IF SUBSTRING(lcLine,1,4) = "5890" AND 
      lcCompanyID = SUBSTRING(lcLine,5,9)
   THEN DO:
   
      ASSIGN ldInvAmt      = DECIMAL(SUBSTRING(lcLine,89,10)) / 100
             liCount       = INTEGER(SUBSTRING(lcLine,105,10))
             NO-ERROR.
     
      IF ERROR-STATUS:ERROR THEN NEXT.

      IF ldInvAmt NE ldTotalAmt OR liCount NE liRead THEN DO:
         fErrorLog("File totals do not match: " +
                   STRING(ldInvAmt) + "/" + STRING(liCount) + " vrs " +
                   STRING(ldTotalAmt) + "/" + STRING(liRead)).
         
         FOR EACH ttPayment:
            DELETE ttPayment.
         END.

      END.

      ASSIGN liRead     = 0
             ldTotalAmt = 0.
   END.     
        
END.

INPUT STREAM sRead CLOSE.


