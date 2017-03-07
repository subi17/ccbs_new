/* ----------------------------------------------------------------------
  MODULE .......: refundfile.p
  TASK .........: Print payments to a refund debit file csb34.1
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.09.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE AllIncludes YES

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}
{Func/email.i}
{Func/msreqfunc.i}
{Ar/refundfilett.i}

/* payments */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttRequest.
/* charge date */
DEFINE INPUT  PARAMETER idtAccDate  AS DATE NO-UNDO. 
/* how many Payments are to be printed */
DEFINE INPUT  PARAMETER iiPaymCount AS INT  NO-UNDO. 
/* printing file */
DEFINE INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
/* should an empty file be made */
DEFINE INPUT  PARAMETER ilEmptyFile AS LOG  NO-UNDO. 
/* how many were printed */
DEFINE OUTPUT PARAMETER oiPaymCount AS INT  NO-UNDO. 

DEF STREAM sLog.
DEF STREAM sFile.

DEF VAR lcRefLine     AS CHAR NO-UNDO. 
DEF VAR lcRefAmt      AS CHAR NO-UNDO. 
DEF VAR lcSubHeader   AS CHAR NO-UNDO.
DEF VAR ldDispAmt     AS DEC  NO-UNDO. 
DEF VAR lcErrTxt      AS CHAR NO-UNDO. 
DEF VAR lcTmpFile     AS CHAR NO-UNDO. 
DEF VAR lcLine        AS CHAR NO-UNDO. 
DEF VAR lcRepCode     AS CHAR NO-UNDO. 
DEF VAR lcLastError   AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.
DEF VAR lcFileExt     AS CHAR NO-UNDO.
DEF VAR liPCnt        AS INT  NO-UNDO.
DEF VAR lcMessage     AS CHAR NO-UNDO. 
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR lcConfDir     AS CHAR NO-UNDO. 
DEF VAR lcBankAcc     AS CHAR NO-UNDO.
DEF VAR lcCompanyID   AS CHAR NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR lcAddress     AS CHAR NO-UNDO.
DEF VAR lcPost        AS CHAR NO-UNDO. 
DEF VAR lcZipCode     AS CHAR NO-UNDO.
DEF VAR lcRegion      AS CHAR NO-UNDO. 
DEF VAR lcNewLine     AS CHAR NO-UNDO.
DEF VAR liLineCnt     AS INT  NO-UNDO.
DEF VAR liCustCnt     AS INT  NO-UNDO.
DEF VAR ldPaymTot     AS DEC  NO-UNDO.
DEF VAR lcPlainFile   AS CHAR NO-UNDO. 
DEF VAR ldCurrent     AS DEC  NO-UNDO. 
DEF VAR lcContAddress AS CHAR NO-UNDO.
DEF VAR lcDescription AS CHAR NO-UNDO.
DEF VAR lcSuffix      AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttError NO-UNDO
   FIELD MsRequest AS INT
   FIELD Cust      AS INT
   FIELD ErrMsg    AS CHAR.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.

FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE ttError.
    ASSIGN ttError.MsRequest = MsRequest.MsRequest
           ttError.Cust      = MsRequest.CustNum
           ttError.ErrMsg    = iMessage
           lcLastError       = iMessage.

    /* delete the temp-table, so that "expstamp" doesn't get marked */
    DELETE ttRequest. 

END FUNCTION.

FUNCTION fHeaderRowStart RETURNS LOGIC:
   
   PUT STREAM sFile UNFORMATTED
      "03"                                       /* record code */
      "62"                                       /* operation code */
      STRING(lcCompanyID,"X(9)")                 /* yoigo cif */
      lcSuffix                                   /* payment type (suffix) */
      FILL(" ",12).                              /* free    */

END FUNCTION.

FUNCTION fBenRowStart RETURNS LOGIC:

   PUT STREAM sFile UNFORMATTED
      "06"                                       /* record code */
      "56"                                       /* operation code */
      STRING(lcCompanyID,"X(9)")                 /* yoigo cif */
      lcSuffix                                   /* payment type (suffix) */
      STRING(Customer.OrgID,"X(12)").            /* customer id */

END FUNCTION.

FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN STRING(idAmt * 100,"999999999999").
   
END FUNCTION.

FUNCTION fDispInteger RETURNS CHARACTER
   (iiQty AS INT):
   
   RETURN STRING(iiQty,"9999999999").
   
END FUNCTION.


/* nothing to do */
IF iiPaymCount = 0 AND NOT ilEmptyFile THEN 
   RETURN "ERROR: No refunds were found".

FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF NOT AVAIL Company THEN RETURN "ERROR:Company data not available".

/* bank account that is dedicated for dd */
FOR FIRST BankAccount NO-LOCK WHERE
          BankAccount.Brand   = gcBrand AND
          LOOKUP("Refund",BankAccount.InvForm) > 0:
   lcBankAcc = BankAccount.BankAccount.
END.
IF LENGTH(lcBankAcc) < 24 THEN RETURN "ERROR:Invalid bank account".


ASSIGN lcRefAmt      = " / " + STRING(iiPaymCount)
       lcNewLine     = CHR(13) + CHR(10)
       /* yoigo cif */
       lcCompanyID   = REPLACE(Company.CompanyID,"-","")
       lcSuffix      = "001"    
       lcDescription = DYNAMIC-FUNCTION("fHdrText" IN ghFunc1,
                                        301,
                                        1).

/* transfer directory given */
IF INDEX(icFile,"*") > 0 THEN ASSIGN 
   lcTransDir  = ENTRY(1,icFile,"*")
   icFile      = ENTRY(2,icFile,"*").
ELSE 
   lcTransDir  = fCParamC("RefundTransDir").

/* get the extension from file name */
liPCnt = R-INDEX(icFile,".").
IF liPCnt > 0 THEN lcFileExt = SUBSTRING(icFile,liPCnt).

/* file without the path -> Payment */ 
ASSIGN 
   lcPlainFile = icFile
   liPCnt      = R-INDEX(lcPlainFile,"/").
IF liPCnt > 0 THEN lcPlainFile = SUBSTRING(lcPlainFile,liPCnt + 1).


EMPTY TEMP-TABLE ttError. 

/* check that file doesn't exist and form the complete name */
icFile = fUniqueFileName(icFile,
                         ".txt").
      
OUTPUT STREAM sFile TO VALUE(icFile).

IF iiPaymCount > 0 THEN DO:

   /* ordering client header */
   
   /* header 1 */
   fHeaderRowStart().
   PUT STREAM sFile UNFORMATTED
      "001"                                          /* data number */
      STRING(TODAY,"999999")                         /* date of the file */
      STRING(idtAccDate,"999999")                    /* issue date */
      STRING(lcBankAcc,"X(24)")                      /* bank account */
      "0"                                            /* charge detail */
      FILL(" ",8)                                    /* free      */
      lcNewLine.

   /* header 2 */
   fHeaderRowStart().
   PUT STREAM sFile UNFORMATTED
      "002"                                          /* data number */
      STRING(Company.CompName,"X(36)")               /* name      */
      FILL(" ",5)                                    /* free      */
      lcNewLine.

   /* header 3 */
   fHeaderRowStart().
   PUT STREAM sFile UNFORMATTED
      "003"                                          /* data number */
      STRING(Company.Address,"X(36)")                /* name      */
      FILL(" ",5)                                    /* free      */
      lcNewLine.

   /* header 4 */ 
   fHeaderRowStart().
   PUT STREAM sFile UNFORMATTED
      "004"                                          /* data number */
      STRING(Company.HomeLocation,"X(36)")           /* city      */
      FILL(" ",5)                                    /* free      */
      lcNewLine.

   /* national transfer records */
   
   /* header for rows */
   PUT STREAM sFile UNFORMATTED
      "04"                                       /* record code */
      "56"                                       /* operation code */
      STRING(lcCompanyID,"X(9)")                 /* yoigo cif */
      lcSuffix                                   /* payment type (suffix) */
      FILL(" ",12)                               /* free    */
      FILL(" ",3)                                /* free    */
      FILL(" ",41)                               /* free    */
      lcNewLine.

   /* national transfer lines */
   liLineCnt = 1.

   PrintMainLoop:
   FOR EACH ttRequest WHERE
            ttRequest.AccDate = idtAccDate,
      FIRST MsRequest NO-LOCK WHERE
            MsRequest.MsRequest = ttRequest.MsRequest,
      FIRST Customer OF MsRequest NO-LOCK
   BY Customer.BankAcc
   BY MsRequest.MsRequest:

      /* invalid bank data */
      IF LENGTH(Customer.BankAcc) < 24 THEN DO:
         fErrLine("Invalid bank data").
         NEXT PrintMainLoop.
      END.

      /* customer name and address data */
      ASSIGN 
         lcCustName = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                       BUFFER Customer)
         lcAddress  = Customer.Address
         lcZipCode  = Customer.ZipCode 
         lcPost     = Customer.PostOffice.

      IF lcAddress = "" AND lcZipCode = "" AND lcPost = "" THEN DO:
         fErrLine("Address data is missing").
         NEXT PrintMainLoop.
      END.
   
      /* all name and address data in capital letters */
      ASSIGN lcCustName    = CAPS(lcCustName)
             lcAddress     = CAPS(lcAddress)
             lcPost        = CAPS(lcPost)
             lcRegion      = CAPS(lcRegion)
             lcContAddress = "".

      /* should address be divided into two lines */
      IF LENGTH(lcAddress) > 36 THEN DO:
         REPEAT:
            liPCnt = R-INDEX(lcAddress," "). 
            IF liPCnt > 1 THEN ASSIGN 
               lcContAddress = SUBSTRING(lcAddress,liPCnt + 1) + 
                               (IF lcContAddress > "" THEN " " ELSE "") + 
                               lcContAddress
               lcAddress     = SUBSTRING(lcAddress,1,liPCnt - 1).
              
            IF LENGTH(lcAddress) <= 36 OR liPCnt = 0 THEN LEAVE.
         END.     
      END.
      
      /* 1. beneficiary record */
      fBenRowStart().
      PUT STREAM sFile UNFORMATTED
         "010"                                        /* data number */
         fDispDecimal(MsRequest.ReqDParam1)           /* refund amount */
         STRING(Customer.BankAcc,"X(24)")             /* bank code (4) +    */
                                                      /* bank office (4) +  */
                                                      /* control digits (2) + */
                                                      /* bank account (10)  */
         "1"                                          /* 'charged' code  */
         "9"                                          /* reason for order */
         STRING(lcDescription > "","1/0")             /* instructions */
         FILL(" ",6)                                  /* free      */
         lcNewLine.
         
      /* 2. beneficiary record */
      fBenRowStart().
      PUT STREAM sFile UNFORMATTED
         "011"                                        /* data number */
         STRING(lcCustName,"X(36)")                   /* name      */
         FILL(" ",5)                                  /* free      */
         lcNewLine.

      /* 3. beneficiary record */
      fBenRowStart().
      PUT STREAM sFile UNFORMATTED
         "012"                                        /* data number */
         STRING(lcAddress,"X(36)")                    /* address    */
         FILL(" ",5)                                  /* free       */
         lcNewLine.
           
      /* 4. beneficiary record */
      IF lcContAddress > "" THEN DO:
         fBenRowStart().
         PUT STREAM sFile UNFORMATTED
            "013"                                     /* data number */
            STRING(lcContAddress,"X(36)")             /* address    */
            FILL(" ",5)                               /* free      */
            lcNewLine.

         liLineCnt = liLineCnt + 1.   
      END.
          
      /* 5. beneficiary record */
      fBenRowStart().
      PUT STREAM sFile UNFORMATTED
         "014"                                        /* data number */
         STRING(lcZipCode + " " + lcPost,"X(36)")     /* address    */
         FILL(" ",5)                                  /* free       */
         lcNewLine.

      /* 6. beneficiary record */
      IF SUBSTRING(lcZipCode,3,3) NE "000" THEN DO:
         
         /* region is not shown if it is the capital (000) */
         FIND Region WHERE Region.Region = Customer.Region NO-LOCK NO-ERROR.
         IF AVAILABLE Region 
         THEN lcRegion = Region.RgName.
         ELSE lcRegion = Customer.Region.
         
         fBenRowStart().

         PUT STREAM sFile UNFORMATTED
            "015"                                     /* data number */
            STRING(lcRegion,"X(36)")                  /* region     */
            FILL(" ",5)                               /* free       */
            lcNewLine.

         liLineCnt = liLineCnt + 1.
      END.

      /* 7. beneficiary record */
      IF lcDescription > "" THEN DO:
         fBenRowStart().
         PUT STREAM sFile UNFORMATTED
            "016"                                     /* data number */
            STRING(lcDescription,"X(36)")             /* reason for transfer */
            FILL(" ",5)                               /* free       */
            lcNewLine.
            
         liLineCnt = liLineCnt + 1.   
      END.
            
      /* mark that Payment was handled */    
      ttRequest.Printed = 1.

      ASSIGN oiPaymCount = oiPaymCount + 1
             liLineCnt   = liLineCnt + 4
             ldPaymTot   = ldPaymTot + MsRequest.ReqDParam1.

      IF NOT CAN-FIND(FIRST ttCust WHERE ttCust.CustNum = MsRequest.CustNum)
      THEN DO:
         CREATE ttCust.
         ASSIGN ttCust.CustNum = MsRequest.CustNum
                liCustCnt      = liCustCnt + 1.
      END.
   
      IF NOT SESSION:BATCH AND oiPaymCount MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISPLAY oiPaymCount FORMAT ">>>>>>>9"
                 lcRefAmt   FORMAT "x(12)"
         WITH NO-LABELS OVERLAY ROW 10 CENTERED
              TITLE " Printing " FRAME fPrint.
      END.

   END.     /* Payments */

   /* totals (national transfers) */
   PUT STREAM sFile UNFORMATTED
      "08"                                         /* record code */
      "56"                                         /* operation code */
      STRING(lcCompanyID,"X(9)")                   /* yoigo cif */
      lcSuffix                                     /* payment type */
      FILL(" ",12)                                 /* free    */
      FILL(" ",3)                                  /* free    */
      fDispDecimal(ldPaymTot)                      /* total amount */
      SUBSTRING(fDispInteger(oiPaymCount),3)       /* nbr of 010 lines */
      fDispInteger(liLineCnt + 1)                  /* nbr of national lines */
      FILL(" ",6)                                  /* free    */
      FILL(" ",5)                                  /* free    */
      lcNewLine.

   /* totals (general) */
   PUT STREAM sFile UNFORMATTED
      "09"                                         /* record code */
      "62"                                         /* operation code */
      STRING(lcCompanyID,"X(9)")                   /* yoigo cif */
      lcSuffix                                     /* payment type */
      FILL(" ",12)                                 /* free    */
      FILL(" ",3)                                  /* free    */
      fDispDecimal(ldPaymTot)                      /* total amount */
      SUBSTRING(fDispInteger(oiPaymCount),3)       /* nbr of 010 lines */
      fDispInteger(liLineCnt + 6)                  /* nbr of all lines */
      FILL(" ",6)                                  /* free    */
      FILL(" ",5)                                  /* free    */
      lcNewLine.

   /* lilinecnt + 6 = 4 headers and 2 totals */

END.  /* PaymCount>0 */


OUTPUT STREAM sFile CLOSE.

HIDE FRAME fPrint NO-PAUSE.

ldCurrent = fMakeTS().

/* mark requests */
FOR EACH ttRequest WHERE
         ttRequest.Printed = 1,
FIRST MsRequest EXCLUSIVE-LOCK WHERE
      MsRequest.MsRequest = ttRequest.MsRequest:
       
   MsRequest.ReqCParam3 = lcPlainFile.
   fReqStatus(17,"").

   RELEASE MsRequest.    
END.

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = STRING(SESSION:BATCH,"Cron/UI")  
      ActionLog.KeyValue     = ""
      ActionLog.ActionID     = "RefundFile"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = oiPaymCount
      ActionLog.ActionChar   = lcPlainFile + 
                               IF NOT SESSION:BATCH
                               THEN ". Created by " + katun
                               ELSE "" 
      ActionLog.ActionStatus = 3.
      ActionLog.ActionTS     = fMakeTS().
END.

/* move the new file to the actual transfer directory */
fTransDir(icFile,
          lcFileExt,
          lcTransDir).

/* possible errors */
IF CAN-FIND(FIRST ttError) THEN DO:

    lcErrFile = fCParamC("RefundErrorFile").
    IF lcErrFile = "" OR lcErrFile = ? THEN lcErrFile = "/tmp/refunderr".
    
    lcErrFile = lcErrFile + "_" + 
                           STRING(YEAR(TODAY),"9999") +
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99")    + 
                           "_" + STRING(TIME) + ".txt".                    

    OUTPUT STREAM slog TO VALUE(lcErrFile).
    PUT STREAM slog UNFORMATTED
        "Request"   CHR(9)
        "Customer"  CHR(9)
        "Error"     lcNewLine.

    FOR EACH ttError:
             
        PUT STREAM slog UNFORMATTED
            ttError.MsRequest  CHR(9)
            ttError.Cust       CHR(9)
            ttError.ErrMsg     lcNewLine.

       /* save to db for reporting */
       CREATE ErrorLog.
       ASSIGN ErrorLog.Brand     = gcBrand
              ErrorLog.ActionID  = "RFNDFILE"
              ErrorLog.TableName = "MsRequest"
              ErrorLog.KeyValue  = STRING(ttError.MsRequest)
              ErrorLog.ActionTS  = ldCurrent
              ErrorLog.UserCode  = katun
              ErrorLog.ErrorMsg  = ttError.ErrMsg.
    END.

    OUTPUT STREAM slog CLOSE. 

    /* send the report AS email */
    lcConfDir = fCParamC("RepConfDir").
    
    /* mail recipients AND actual sending */
    GetRecipients(lcConfDir + "refund_error.email").

    IF xMailAddr > "" THEN DO:
       ASSIGN xMailAttach = lcErrFile
              lcErrFile    = "/tmp/inv_csb341_errmsg.txt".
   
       OUTPUT STREAM slog TO VALUE(lcErrFile).
       PUT STREAM slog UNFORMATTED
          "Errors from creating a payment CSB34.1-file for refunds " + 
          STRING(TODAY,"99.99.9999") + "." + lcNewLine.
       OUTPUT STREAM slog CLOSE.

       SendMail(lcErrFile,xMailAttach).
   END.
   
END.

/* return last error message, useable in case one Payment is printed */
RETURN lcLastError.


