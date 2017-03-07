/* ----------------------------------------------------------------------
  MODULE .......: ddoutfile.p
  TASK .........: Print invoices to a direct debit file csb19 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.01.07
  CHANGED ......: 18.04.07/aam ErrorLog to db
                  25.04.07/aam ilEmptyFile  
  Version ......: yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE AllIncludes YES

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}
{Func/email.i}
{Func/transname.i}
{Func/fhdrtext.i}
{Inv/ddoutfilett.i}
{Func/customer_address.i}
{Syst/funcrunprocess_update.i}

/* invoices TO be printed */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInvoice.
/* charge date */
DEFINE INPUT  PARAMETER idtDueDate    AS DATE NO-UNDO. 
/* bank code; not the actual customer bank but the one used for sorting */
DEFINE INPUT  PARAMETER icBankCode    AS CHAR NO-UNDO.
/* how many invoices are to be printed */
DEFINE INPUT  PARAMETER iiInvCount    AS INT  NO-UNDO. 
/* printing file */
DEFINE INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
/* should an empty file be made */
DEFINE INPUT  PARAMETER ilEmptyFile   AS LOG  NO-UNDO. 
DEFINE INPUT  PARAMETER iiFRProcessID AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiFileSeq     AS INT  NO-UNDO.
/* how many were printed */
DEFINE OUTPUT PARAMETER oiInvCount    AS INT  NO-UNDO. 

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
DEF VAR lcCompanyBank AS CHAR NO-UNDO.
DEF VAR lcCompanyID   AS CHAR NO-UNDO.
DEF VAR lcNewLine     AS CHAR NO-UNDO.
DEF VAR liLineCnt     AS INT  NO-UNDO.
DEF VAR liCustCnt     AS INT  NO-UNDO.
DEF VAR ldInvTot      AS DEC  NO-UNDO.
DEF VAR lcPlainFile   AS CHAR NO-UNDO. 
DEF VAR ldCurrStamp   AS DEC  NO-UNDO. 
DEF VAR lcDetails     AS CHAR NO-UNDO EXTENT 16.
DEF VAR lcHeader      AS CHAR NO-UNDO EXTENT 15.
DEF VAR lcBaseHeader  AS CHAR NO-UNDO EXTENT 15.
DEF VAR lcTaxZone     AS CHAR NO-UNDO.
DEF VAR ldTaxPerc     AS DEC  NO-UNDO.
DEF VAR lcBaseTaxZone AS CHAR NO-UNDO.
DEF VAR lcNumeric     AS CHAR NO-UNDO.
DEF VAR lcCLIList     AS CHAR NO-UNDO.

DEF BUFFER bInv FOR Invoice.

DEF TEMP-TABLE ttError NO-UNDO
   FIELD Inv    AS CHAR
   FIELD Cust   AS INT
   FIELD ErrMsg AS CHAR.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.

FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE ttError.
    ASSIGN ttError.Inv    = Invoice.ExtInvID
           ttError.Cust   = Invoice.CustNum
           ttError.ErrMsg = iMessage
           lcLastError    = iMessage.

    /* delete the temp-table, so that "ddstate" doesn't get marked */
    DELETE ttInvoice. 

END FUNCTION.

FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN STRING(idAmt * 100,"9999999999").
   
END FUNCTION.

FUNCTION fDispInteger RETURNS CHARACTER
   (iiQty AS INT):
   
   RETURN STRING(iiQty,"9999999999").
   
END FUNCTION.

FUNCTION fIndividualOptional RETURNS LOGIC
   (icDataCode AS CHAR,
    icField1   AS CHAR,
    icField2   AS CHAR,
    icField3   AS CHAR):

   PUT STREAM sFile UNFORMATTED    
      "56"                                            /* record code */
      STRING(icDataCode,"X(2)")                       /* data code */
      STRING(lcCompanyID,"X(9)")                      /* yoigo cif */
      "002"                                           /* suffix    */
      STRING(STRING(Customer.CustNum),"X(12)")        /* customer  */
      STRING(icField1,"X(40)")                        /* 1. detail field */
      STRING(icField2,"X(40)")                        /* 2. detail field */
      STRING(icField3,"X(40)")                        /* 3. detail field */
      FILL(" ",14)                                    /* free    */
      lcNewLine.
     
END FUNCTION.


/***** Main start *******/

RUN pInitialize.

IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.

/* create an empty file if no invoices */
OUTPUT STREAM sFile TO VALUE(icFile).

IF iiInvCount > 0 THEN DO:
   RUN pPrintHeader.
   
   RUN pPrintInvoices.
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
     RUN pLogErrors.
     RETURN RETURN-VALUE.
   END.  

   RUN pPrintTotals.
END.   

OUTPUT STREAM sFile CLOSE.

RUN pFinalize.

RETURN "".

/****** Main end *******/


PROCEDURE pInitialize:

   DEF VAR lcBankID AS CHAR NO-UNDO.
   DEF VAR liPos    AS INT  NO-UNDO.
   
   /* nothing to do */
   IF iiInvCount = 0 AND NOT ilEmptyFile THEN 
      RETURN "ERROR: No invoices were found".

   FIND FIRST Company WHERE
              Company.Brand = gcBrand NO-LOCK NO-ERROR.
   IF NOT AVAIL Company THEN RETURN "ERROR:Company data not available".

   /* bank account that is dedicated for dd */
   lcBankID = "DD" + icBankCode.
   
   FOR FIRST BankAccount NO-LOCK WHERE
             BankAccount.Brand   = gcBrand AND
             LOOKUP(lcBankID,BankAccount.InvForm) > 0:
      lcCompanyBank = BankAccount.BankAccount.
   END.
   IF LENGTH(lcCompanyBank) < 20 THEN RETURN "ERROR:Invalid bank account".

   ASSIGN 
      lcRefAmt    = " / " + STRING(iiInvCount)
      lcNewLine   = CHR(13) + CHR(10)
      lcNumeric   = SESSION:NUMERIC-FORMAT
      SESSION:NUMERIC-FORMAT = "european"
      /* yoigo cif */
      lcCompanyID = REPLACE(Company.CompanyID,"-","").

   /* transfer directory given */
   IF INDEX(icFile,"*") > 0 THEN ASSIGN 
      lcTransDir  = ENTRY(1,icFile,"*")
      icFile      = ENTRY(2,icFile,"*").
   ELSE 
      lcTransDir  = fCParamC("DDTransDir").

   /* get the extension from file name */
   liPCnt = R-INDEX(icFile,".").
   IF liPCnt > 0 THEN lcFileExt = SUBSTRING(icFile,liPCnt).

   /* check that file doesn't exist and form the complete name */
   icFile = fUniqueFileName(icFile,
                            ".txt").
 
   /* file without the path -> invoice */ 
   ASSIGN 
      lcPlainFile = icFile
      liPCnt      = R-INDEX(lcPlainFile,"/").
   IF liPCnt > 0 THEN lcPlainFile = SUBSTRING(lcPlainFile,liPCnt + 1).

   /* headers in base language */
   DO liPCnt = 1 TO EXTENT(lcBaseHeader):
      IF liPCnt >= 15 THEN liPos = 435 + liPcnt.
      ELSE liPos = 350 + liPCnt.
      lcBaseHeader[liPCnt] = fGetHdrText(liPos,1).
   END.

   lcBaseTaxZone = fGetItemName(gcBrand,
                                "TaxZone",
                                "1",
                                1,
                                idtDueDate).

   EMPTY TEMP-TABLE ttError. 

   RETURN "".
   
END PROCEDURE. /* pInitialize */
      
PROCEDURE pPrintHeader:

   /* presenting client header */
   PUT STREAM sFile UNFORMATTED
   "51"                                               /* record code */
   "80"                                               /* data code */
   STRING(lcCompanyID,"X(9)")                         /* yoigo cif */
   "002"                                              /* suffix    */
   STRING(TODAY,"999999")                             /* date of the file */
   FILL(" ",6)                                        /* free      */
   STRING(Company.CompName,"X(40)")                   /* name      */
   FILL(" ",20)                                       /* free      */
   SUBSTRING(lcCompanyBank,1,4)                       /* bank code */
   SUBSTRING(lcCompanyBank,5,4)                       /* bank office */
   FILL(" ",12)                                       /* free      */
   FILL(" ",40)                                       /* free      */
   FILL(" ",14)                                       /* free      */
   lcNewLine
   
   /* ordering client header */
   "53"                                               /* record code */
   "80"                                               /* data code */
   STRING(lcCompanyID,"X(9)")                         /* yoigo cif */
   "002"                                              /* suffix    */
   STRING(TODAY,"999999")                             /* date of the file */
   STRING(idtDueDate,"999999")                        /* charge date */
   STRING(Company.CompName,"X(40)")                   /* name      */
   STRING(lcCompanyBank,"X(20)")                      /* bank code (4) +    */
                                                      /* bank office (4) +  */
                                                      /* control digits (2) + */
                                                      /* bank account (10)  */
   FILL(" ",8)                                        /* free      */
   "01"                                               /* "first" procedure */
   FILL(" ",10)                                       /* free      */
   FILL(" ",40)                                       /* free      */
   FILL(" ",14)                                       /* free      */
   lcNewLine.

   liLineCnt = 2.

END PROCEDURE. /* pPrintHeader */

PROCEDURE pPrintInvoices:

   DEF VAR liPos AS INT  NO-UNDO.
   
   
   PrintMainLoop:
   FOR EACH ttInvoice WHERE
            ttInvoice.DueDate  = idtDueDate AND
            ttInvoice.BankCode = icBankCode,
      FIRST Invoice OF ttInvoice NO-LOCK,
      FIRST Customer OF Invoice NO-LOCK
   BY Customer.BankAcc
   BY Invoice.ExtInvID:

      /* invalid bank data */
      IF LENGTH(Customer.BankAcc) < 20 THEN DO:
         fErrLine("Invalid bank data").
         NEXT PrintMainLoop.
      END.

      fSetCustData().

      /* check that address etc. are valid */
      lcErrTxt = fCheckAddress(lcCustName,
                               lcZipCode,
                               lcCountry,
                               OUTPUT lcCountryName).

      IF lcErrTxt NE "" THEN DO:
         fErrLine(lcErrTxt).
         NEXT PrintMainLoop.
      END. 
      
      IF lcAddress = "" AND lcZipCode = "" AND lcPost = "" THEN DO:
         fErrLine("Address data is missing").
         NEXT PrintMainLoop.
      END.
   
      ASSIGN
         lcDetails = ""
         ldTaxPerc = 0.

      IF Invoice.TaxZone = "1" AND Customer.Language = 1 THEN
         lcTaxZone = lcBaseTaxZone.
      ELSE 
         lcTaxZone = fGetItemName(gcBrand,
                                  "TaxZone",
                                  Invoice.TaxZone,
                                  Customer.Language,
                                  idtDueDate).
   
      IF lcTaxZone = "" OR lcTaxZone = ? THEN lcTaxZone = Invoice.InvGroup.
 
      DO liPCnt = 1 TO 6:
         ldTaxPerc = MAX(ldTaxPerc,Invoice.VatPercent[liPCnt]).
      END.

      /* headers in customer's language */
      DO liPCnt = 1 TO EXTENT(lcBaseHeader):
         IF Customer.Language = 1 THEN 
            lcHeader[liPCnt] = lcBaseHeader[liPCnt].
         ELSE DO:
            IF liPCnt >= 15 THEN liPos = 435 + liPCnt.
            ELSE liPos = 350 + liPCnt.
            lcHeader[liPCnt] = fGetHdrText(liPos,Customer.Language).
         END.   
      END.

      /* list of subscriptions on the invoice */
      lcCLIList = "".
      FOR EACH SubInvoice OF Invoice NO-LOCK WHERE
               SubInvoice.CLI > "":
         IF NUM-ENTRIES(lcCLIList) >= 3 THEN DO:
            lcCLIList = lcCLIList + " " + lcHeader[15].
            LEAVE.
         END.
         lcCLIList = lcCLIList + (IF lcCLIList > "" THEN "," ELSE "") + 
                     SubInvoice.CLI.
      END.

      ASSIGN
         lcDetails[1]  = lcHeader[1] + " " + Invoice.ExtInvID + "  " +
                         lcHeader[2] + " " + 
                         STRING(Invoice.InvDate,"99/99/9999") + "  " +
                         lcHeader[3] + " " + 
                         STRING(Invoice.DueDate,"99/99/9999")
         lcDetails[3]  = lcHeader[4] + " " + 
                         lcHeader[5] + " " + lcCompanyID + ", " + 
                         lcHeader[6]
         lcDetails[5]  = lcHeader[7] + " " + STRING(lcCustName,"X(55)") + 
                         " " + Customer.CustIDType + ": " + Customer.OrgID
         lcDetails[7]  = lcAddress
         lcDetails[9]  = lcZipCode + " - " + lcRegion + " - " + lcPost
         lcDetails[11] = lcHeader[8] + " " + 
                         STRING(Invoice.FromDate,"99/99/9999") +
                         " " + lcHeader[9] + " " +
                         STRING(Invoice.ToDate,"99/99/9999") + " , " +
                         lcHeader[10] + " " + lcCLIList
         lcDetails[13] = lcHeader[11] + " " + 
                         STRING(Invoice.AmtExclVat,"->>,>>9.99") + " " +
                         Invoice.Currency + ", " + 
                         lcHeader[12] + "(" + lcTaxZone + " " +
                         STRING(ldTaxPerc,">9.9") + "%): " +
                         STRING(Invoice.VatAmt,"->>,>>9.99") + " " +
                         Invoice.Currency
         lcDetails[15] = lcHeader[13] + "  " +
                         STRING(Invoice.InvAmt,"->>,>>9.99") + " " +
                         Invoice.Currency + "  " +
                         lcHeader[14].
                         
      DO liPCnt = 1 TO 15 BY 2:
         ASSIGN 
            lcDetails[liPCnt + 1] = SUBSTRING(lcDetails[liPCnt],41)
            lcDetails[liPCnt]     = SUBSTRING(lcDetails[liPCnt],1,40).
      END.

                        
      /* compulsory individual */
      PUT STREAM sFile UNFORMATTED
      "56"                                            /* record code */
      "80"                                            /* data code */
      STRING(lcCompanyID,"X(9)")                      /* yoigo cif */
      "002"                                           /* suffix    */
      STRING(STRING(Customer.CustNum),"X(12)")        /* customer  */
      STRING(lcCustName,"X(40)")                      /* name      */
      STRING(Customer.BankAcc,"X(20)")                /* bank code (4) +    */
                                                      /* bank office (4) +  */
                                                      /* control digits (2) + */
                                                      /* bank account (10)  */
      fDispDecimal(Invoice.InvAmt)                    /* charged amount */
      FILL(" ",6)                                     /* returned payments */
      STRING(STRING(Invoice.InvNum),"X(10)")          /* internal reference */
      STRING(lcDetails[1],"X(40)")                    /* 1. detail field */
      FILL(" ",8)                                     /* free    */
      lcNewLine.

                        
      /* individual optional rows, 1-6 */
      fIndividualOptional("81",
                          lcDetails[2],
                          lcDetails[3],
                          lcDetails[4]).
                          
      fIndividualOptional("82",
                          lcDetails[5],
                          lcDetails[6],
                          lcDetails[7]).
                          
      fIndividualOptional("83",
                          lcDetails[8],
                          lcDetails[9],
                          lcDetails[10]).

      fIndividualOptional("84",
                          lcDetails[11],
                          lcDetails[12],
                          lcDetails[13]).
                          
      fIndividualOptional("85",
                          lcDetails[14],
                          lcDetails[15],
                          lcDetails[16]).
                          
      fIndividualOptional("86",
                          lcCustName,
                          lcAddress,
                          STRING(lcPost,"X(35)") + lcZipCode).
        
      /* mark that invoice was handled */    
      ttInvoice.Printed = 1.

      ASSIGN oiInvCount  = oiInvCount + 1
             liLineCnt   = liLineCnt + 7
             ldInvTot    = ldInvTot + Invoice.InvAmt.

      IF NOT CAN-FIND(FIRST ttCust WHERE ttCust.CustNum = Invoice.CustNum)
      THEN DO:
         CREATE ttCust.
         ASSIGN ttCust.CustNum = Invoice.CustNum
                liCustCnt      = liCustCnt + 1.
      END.
   
      IF NOT SESSION:BATCH AND oiInvCount MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISPLAY oiInvCount FORMAT ">>>>>>>9"
                 lcRefAmt   FORMAT "x(12)"
         WITH NO-LABELS OVERLAY ROW 10 CENTERED
              TITLE " Printing " FRAME fPrint.
      END.

      IF iiUpdInterval > 0 AND oiInvCount MOD iiUpdInterval = 0 
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiInvCount) THEN
            RETURN "ERROR:Stopped".
      END.   

   END.     /* PrintMainLoop */

   HIDE FRAME fPrint NO-PAUSE.
   
   RETURN "". 

END PROCEDURE. /* pPrintInvoices */

PROCEDURE pPrintTotals:

   /* total ordering client */
   PUT STREAM sFile UNFORMATTED
   "58"                                            /* record code */
   "80"                                            /* data code */
   STRING(lcCompanyID,"X(9)")                      /* yoigo cif */
   "002"                                           /* suffix    */
   FILL(" ",12)                                    /* free    */
   FILL(" ",40)                                    /* free    */
   FILL(" ",20)                                    /* free    */
   fDispDecimal(ldInvTot)                          /* total amount */
   FILL(" ",6)                                     /* free    */
   fDispInteger(oiInvCount)                        /* nbr of invoices */
   fDispInteger(liLineCnt)                         /* nbr of lines */
   FILL(" ",20)                                    /* free    */
   FILL(" ",18)                                    /* free    */
   lcNewLine

   /* total general */
   "59"                                            /* record code */
   "80"                                            /* data code */
   STRING(lcCompanyID,"X(9)")                      /* yoigo cif */
   "002"                                           /* suffix    */
   FILL(" ",12)                                    /* free    */
   FILL(" ",40)                                    /* free    */
   STRING(1,"9999")                                /* nbr of sending cust */
   FILL(" ",16)                                    /* free    */
   fDispDecimal(ldInvTot)                          /* total amount */
   FILL(" ",6)                                     /* free    */
   fDispInteger(oiInvCount)                        /* nbr of invoices */
   fDispInteger(liLineCnt + 2)                     /* nbr of all lines */
   FILL(" ",20)                                    /* free    */
   FILL(" ",18)                                    /* free    */
   lcNewLine.

END PROCEDURE. /* pPrintTotals */

PROCEDURE pFinalize:

   DEF VAR lcFinalFile AS CHAR NO-UNDO.
   
   SESSION:NUMERIC-FORMAT = lcNumeric.

   /* mark invoices AS printed 
   FOR EACH ttInvoice WHERE
            ttInvoice.DueDate  = idtDueDate AND
            ttInvoice.BankCode = icBankCode AND
            ttInvoice.Printed  = 1,
   FIRST Invoice OF ttInvoice exclusive-lock,
   FIRST Customer OF Invoice NO-LOCK:
       
      ASSIGN Invoice.DDState   = 1
             Invoice.DDFile    = lcPlainFile
             Invoice.DDBankAcc = Customer.BankAcc.

      RELEASE Invoice.    
   END.
   */

   /* move the new file to the actual transfer directory */
   lcFinalFile = "".
   lcTransDir = "".
   IF lcTransDir > "" THEN DO:
      lcFinalFile = fMove2TransDir(icFile,
                                   lcFileExt,
                                   lcTransDir).
   END.
   IF lcFinalFile = "" THEN lcFinalFile = icFile.
   
   IF iiFRProcessID > 0 THEN DO TRANS:
      CREATE FuncRunResult.
      ASSIGN 
         FuncRunResult.FRProcessID = iiFRProcessID
         FuncRunResult.FRResultSeq = iiFileSeq
         FuncRunResult.ResultOrder = 1
         FuncRunResult.CharParam   = lcFinalFile
         FuncRunResult.IntParam    = oiInvCount.
   END.

   RUN pLogErrors.
   
END PROCEDURE.

PROCEDURE pLogErrors:

   /* possible errors */
   IF CAN-FIND(FIRST ttError) THEN DO:

      lcErrFile = fCParamC("DDebitErrorFile").
      IF lcErrFile = "" OR lcErrFile = ? THEN lcErrFile = "/tmp/dderr".
    
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
          "Error"     lcNewLine.

      FOR EACH ttError TRANS:
         PUT STREAM slog UNFORMATTED
            ttError.Inv    CHR(9)
            ttError.Cust   CHR(9)
            ttError.ErrMsg lcNewLine.

         /* save to db for reporting */
         CREATE ErrorLog.
         ASSIGN ErrorLog.Brand     = gcBrand
                ErrorLog.ActionID  = "DDFILETEST"
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
      GetRecipients(lcConfDir + "ddout_error.email").

      IF xMailAddr > "" THEN DO:
         ASSIGN xMailAttach = lcErrFile
                lcErrFile    = "/tmp/inv_csb19_errmsg.txt".
   
         OUTPUT STREAM slog TO VALUE(lcErrFile).
         PUT STREAM slog UNFORMATTED
            "Errors from creating an invoice CSB19-file for direct debiting " + 
            STRING(TODAY,"99.99.9999") + "." + lcNewLine + lcNewLine +
            "Open the attachment file in Excel." + 
            lcNewLine + lcNewLine + "  ".
         OUTPUT STREAM slog CLOSE.

         SendMail(lcErrFile,xMailAttach).
      END.
   
   END.

END PROCEDURE. /* pFinalize */


