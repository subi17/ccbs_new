/* ----------------------------------------------------------------------
  MODULE .......: ddpayment.p
  TASK .........: Make payments to dd invoices 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.04.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE AllIncludes YES

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}
{Func/email.i}

{Ar/ddpaymentt.i}

/* invoices to be paid */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInvoice.
/* payment date */
DEFINE INPUT  PARAMETER idtPaymDate AS DATE NO-UNDO.
/* how many invoices are to be paid */
DEFINE INPUT  PARAMETER iiInvCount  AS INT  NO-UNDO. 
/* how many were paid */
DEFINE OUTPUT PARAMETER oiInvCount  AS INT  NO-UNDO. 

DEF STREAM sLog.

DEF VAR lcRefAmt    AS CHAR NO-UNDO. 
DEF VAR lcLastError AS CHAR NO-UNDO.
DEF VAR lcErrFile   AS CHAR NO-UNDO. 
DEF VAR lcConfDir   AS CHAR NO-UNDO. 
DEF VAR liBankAcc   AS INT  NO-UNDO.
DEF VAR liLineCnt   AS INT  NO-UNDO.
DEF VAR liVoucher   AS INT  NO-UNDO.
DEF VAR ldCurrStamp AS DEC  NO-UNDO.

DEF BUFFER bInv FOR Invoice.

DEF TEMP-TABLE ttError NO-UNDO
   FIELD Inv    AS CHAR
   FIELD Cust   AS INT
   FIELD ErrMsg AS CHAR.


FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE ttError.
    ASSIGN ttError.Inv    = Invoice.ExtInvID
           ttError.Cust   = Invoice.CustNum
           ttError.ErrMsg = iMessage
           lcLastError    = iMessage.

    /* delete the temp-table, so that "paymstate" doesn't get marked */
    DELETE ttInvoice. 

END FUNCTION.


/* nothing to do */
IF iiInvCount = 0 THEN RETURN "INFO: No invoices to be paid".

ASSIGN 
   lcRefAmt  = " / " + STRING(iiInvCount)
   liBankAcc = fCParamI("BankAcc").
   
IF liBankAcc = 0 OR liBankAcc = ? THEN 
   RETURN "ERROR:Payment account not defined".

EMPTY TEMP-TABLE ttError. 


PaymMainLoop:
FOR EACH ttInvoice,
   FIRST Invoice OF ttInvoice EXCLUSIVE-LOCK,
   FIRST Customer OF Invoice NO-LOCK
BY Invoice.ExtInvID:

   RUN Ar/makepaym (BUFFER Invoice,
                 Invoice.InvAmt,
                 idtPaymDate,
                 liBankAcc,
                 IF Invoice.ChargeType = 5 THEN "NDD" 
                 ELSE "DD",             /* payment source */
                 IF Invoice.ChargeType = 5 THEN 14
                 ELSE 8,                /* payment type   */
                 FALSE,
                 FALSE,
                 "",                    /* reference */
                 "",                    /* memo */
                 OUTPUT liVoucher).

   IF liVoucher = 0 THEN DO:
      fErrLine("Payment was not created").
      NEXT.
   END. 
      
   ASSIGN 
      ttInvoice.Paid = 1
      oiInvCount     = oiInvCount + 1.

   
   IF NOT SESSION:BATCH AND oiInvCount MOD 100 = 0 THEN DO:
       PAUSE 0.
       DISPLAY oiInvCount FORMAT ">>>>>>>9"
               lcRefAmt   FORMAT "x(12)"
       WITH NO-LABELS OVERLAY ROW 10 CENTERED
            TITLE " Posting " FRAME fPaym.
   END.

END.     

HIDE FRAME fPaym NO-PAUSE.


/* possible errors */
IF CAN-FIND(FIRST ttError) THEN DO:

    lcErrFile = fCParamC("DDPaymErrorFile").
    IF lcErrFile = "" OR lcErrFile = ? THEN lcErrFile = "/tmp/ddpaymerr".
    
    lcErrFile = lcErrFile + "_" + 
                           STRING(YEAR(TODAY),"9999") +
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99")    + 
                           "_" + STRING(TIME) + ".txt".                    

    ldCurrStamp = fMakeTS().
                           
    OUTPUT STREAM slog TO VALUE(lcErrFile).
    PUT STREAM slog UNFORMATTED
        "Invoice"   CHR(9)
        "Customer"  CHR(9)
        "Error"     SKIP.

    FOR EACH ttError TRANS:
        PUT STREAM slog UNFORMATTED
            ttError.Inv    CHR(9)
            ttError.Cust   CHR(9)
            ttError.ErrMsg SKIP.

       /* save to db for reporting */
       CREATE ErrorLog.
       ASSIGN ErrorLog.Brand     = gcBrand
              ErrorLog.ActionID  = "DDPAYM"
              ErrorLog.TableName = "Invoice"
              ErrorLog.KeyValue  = ttError.Inv
              ErrorLog.ActionTS  = ldCurrStamp
              ErrorLog.UserCode  = katun
              ErrorLog.ErrorMsg  = ttError.ErrMsg.
    END.

    OUTPUT STREAM slog CLOSE. 

    /* send the report AS email */
    lcConfDir = fCParamC("RepConfDir").
    
    /* mail recipients AND actual sending */
    GetRecipients(lcConfDir + "ddpaym_error.email").

    IF xMailAddr > "" THEN DO:
       ASSIGN xMailAttach = lcErrFile
              lcErrFile    = "/tmp/inv_ddpaym_errmsg.txt".
   
       OUTPUT STREAM slog TO VALUE(lcErrFile).
       PUT STREAM slog UNFORMATTED
          "Errors from making payments to direct debit invoices " + 
          STRING(TODAY,"99.99.9999") + "." + CHR(10) + CHR(10) +
          "Open the attachment file in Excel." + CHR(10).
       OUTPUT STREAM slog CLOSE.

       SendMail(lcErrFile,xMailAttach).
   END.
   
END.

/* return last error message, useable in case one invoice is paid */
RETURN lcLastError.

