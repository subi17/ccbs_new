/* ----------------------------------------------------------------------
  MODULE .......: invoice_xml.p
  TASK .........: Print invoices to a xml file
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 08.12.08
  ---------------------------------------------------------------------- */

{Inv/printdoc1.i}
{Syst/funcrunprocess_update.i}
{Syst/host.i}
{Syst/tmsconst.i}
{Func/profunc.i}

/* invoices TO be printed */
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttInvoice.
DEFINE INPUT  PARAMETER idaInvDate    AS DATE NO-UNDO. 
/* how many invoices are to be printed */
DEFINE INPUT  PARAMETER iiInvCount    AS INT  NO-UNDO. 
/* each invoice to a separate file */
DEFINE INPUT  PARAMETER ilSeparate    AS LOG  NO-UNDO.
/* printing file */                  
DEFINE INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER icPrintHouse  AS CHAR NO-UNDO.
/* is this added to a tar file */
DEFINE INPUT  PARAMETER iiFileType    AS INT  NO-UNDO. 
DEFINE INPUT  PARAMETER ilDBWrite     AS LOG  NO-UNDO.
DEFINE INPUT  PARAMETER iiFRProcessID AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
/* how many were printed */         
DEFINE OUTPUT PARAMETER oiInvCount    AS INT  NO-UNDO. 

DEF VAR lcCodeVersion AS CHAR   NO-UNDO INIT "3.4".
DEF VAR lcRowText     AS CHAR   NO-UNDO.
DEF VAR lcTMSUser     AS CHAR   NO-UNDO.
DEF VAR lcLocalFile   AS CHAR   NO-UNDO.
DEF VAR lhXML         AS HANDLE NO-UNDO.
DEF VAR lhAttribute   AS HANDLE NO-UNDO. 
DEF VAR lcFinalDir    AS CHAR   NO-UNDO.
DEF VAR lcBillRun     AS CHAR   NO-UNDO.
DEF VAR lcXMLVersion  AS CHAR   NO-UNDO.
DEF VAR llFormatted   AS LOG    NO-UNDO.
DEF VAR lcTarFile     AS CHAR   NO-UNDO. 
DEF VAR lcTarFinalDir AS CHAR   NO-UNDO.
DEF VAR lcSpoolDir    AS CHAR   NO-UNDO. 
DEF VAR liCnt         AS INT    NO-UNDO.
DEF VAR lcTarError    AS CHAR   NO-UNDO. 
DEF VAR liTarCnt      AS INT    NO-UNDO. 
DEF VAR liTarFileLimit AS INT    NO-UNDO.
DEF VAR liTarBatch     AS INT    NO-UNDO.
DEF VAR lcTarBatchFile AS CHAR   NO-UNDO.
DEF VAR liFRExecID     AS INT    NO-UNDO.
DEF VAR llReplica      AS LOG    NO-UNDO.
DEF VAR liInitialOrderID AS INT  NO-UNDO.


DEF STREAM sRead.


FUNCTION fTargetDir RETURNS CHAR
   (INPUT icDir      AS CHAR,
    INPUT idaInvDate AS DATE,
    INPUT iiInvNum   AS INT):

   DEF VAR lcDirName AS CHAR NO-UNDO.
   DEF VAR lcInvNum  AS CHAR NO-UNDO.
   DEF VAR lcFirst   AS CHAR NO-UNDO.
   DEF VAR lcSecond  AS CHAR NO-UNDO.
   DEF VAR liDir     AS INT  NO-UNDO.
   DEF VAR liTry     AS INT  NO-UNDO.
   
   ASSIGN 
      lcInvNum  = STRING(iiInvNum)                      
      lcFirst   = SUBSTRING(lcInvNum,LENGTH(lcInvNum) - 1,2).
      
   IF LENGTH(lcInvNum) >= 4 THEN 
      lcSecond = SUBSTRING(lcInvNum,LENGTH(lcInvNum) - 3,2).
   ELSE lcSecond = "XX".
 
   ASSIGN 
      icDir = REPLACE(icDir,"#BILLRUN",STRING(YEAR(idaInvDate)) +
                                       STRING(MONTH(idaInvDate),"99") +
                                       STRING(DAY(idaInvDate),"99"))
      icDir = REPLACE(icDir,"#FF",lcFirst)
      icDir = REPLACE(icDir,"#SS",lcSecond).
    
   /* create directories in case they don't exist */
   lcDirName = "".
   DO liDir = 2 TO NUM-ENTRIES(icDir,"/"):
   
      lcDirName = lcDirName + "/" + ENTRY(liDir,icDir,"/").

      IF liDir >= NUM-ENTRIES(icDir,"/") - 2 THEN REPEAT liTry = 1 TO 5:
    
         FILE-INFO:FILE-NAME = lcDirName.
         IF FILE-INFO:FILE-TYPE BEGINS "D" THEN LEAVE.         
         
         UNIX SILENT VALUE("mkdir -p " + lcDirName).
      END.
   END.
   
   RETURN icDir.
   
END FUNCTION.

/******* MAIN start ********/

CREATE WIDGET-POOL "PrintHouse".

FIND FIRST Company WHERE
           Company.Brand = Syst.Var:gcBrand NO-LOCK NO-ERROR.
IF NOT AVAIL Company THEN RETURN.

objDBConn = NEW Syst.CDRConnect("MobCDR").

fPopulateBillItemAndGroup().

RUN pInitialize.

IF RETURN-VALUE BEGINS "ERROR" THEN ASSIGN
   llInterrupt = TRUE
   lcLastError = RETURN-VALUE.

ELSE DO: 
   IF NOT ilSeparate THEN RUN pInitXML.

   RUN pInvoice2XML.
   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      fErrLine(RETURN-VALUE).
      llInterrupt = TRUE.   
   END.

   IF NOT ilSeparate THEN RUN pFinalizeXML.
END.

DELETE WIDGET-POOL "PrintHouse".

/* move the tar file to its final directory */
IF iiFileType = 1 AND lcTarFinalDir > "" AND liTarCnt > 0 THEN 
   RUN pFinalizeTarFile. 

/* mark invoices AS printed except for online query (printhouse as Qvantel) */
IF NOT llInterrupt AND icPrintHouse <> "QVANTEL" THEN
   RUN pMarkPrinted("XML",
                    icPrintHouse).

/* move the new file to the actual transfer directory */
IF NOT llInterrupt AND NOT ilSeparate THEN DO:

   lcFinalDir = lcTransDir.
    
   FIND FIRST ttInvoice WHERE ttInvoice.Printed NE 2 NO-ERROR.
   IF AVAILABLE ttInvoice THEN DO:
      FIND FIRST Invoice WHERE Invoice.InvNum = ttInvoice.InvNum NO-LOCK.
      lcFinalDir = fTargetDir(lcFinalDir,
                              Invoice.InvDate,
                              Invoice.InvNum).
   END.
   ELSE lcFinalDir = REPLACE(lcFinalDir,"#","").
    
   fMove2TransDirOverwrite(icFile,
                           lcFinalDir).
END.

/* possible errors to a log file */
IF CAN-FIND(FIRST ttError) THEN RUN pErrorFile(ilDBWrite,"INVXML").

SESSION:NUMERIC-FORMAT = lcSesNum.

/* return last error message, useable in case one invoice is printed */
RETURN lcLastError.

/******  MAIN end *******/


PROCEDURE pInitialize:

   DEF VAR lcDate      AS CHAR NO-UNDO.
   DEF VAR llTestMode  AS LOG  NO-UNDO.
   DEF VAR liFRQueueID AS INT  NO-UNDO.

   /* transfer directory given */
   IF INDEX(icFile,"*") > 0 THEN ASSIGN 
      lcTransDir = ENTRY(1,icFile,"*")
      icFile     = ENTRY(2,icFile,"*").
   ELSE 
      lcTransDir = fCParamC("InvXMLTransDir").
   
   /* get the extension from file name */
   liPCnt = R-INDEX(icFile,".").
   IF liPCnt > 0 THEN lcFileExt = SUBSTRING(icFile,liPCnt).

   IF NOT ilSeparate THEN 
      /* check that file doesn't exist and form the complete name */
      icFile = fUniqueFileName(icFile,
                               ".xml").
   ELSE IF INDEX(icFile,"#INVNUM") = 0 THEN 
      RETURN "ERROR:File name does not contain tag for invoice number".

   llTestMode = FALSE.
   IF iiFRProcessID > 0 THEN 
   FOR FIRST FuncRunProcess NO-LOCK WHERE
             FuncRunProcess.FRProcessID = iiFRProcessID,
       FIRST FuncRunExec NO-LOCK WHERE
             FuncRunExec.FRExecID = FuncRunProcess.FRExecID,
       FIRST FuncRunQSchedule NO-LOCK WHERE
             FuncRunQSchedule.FRQScheduleID = FuncRunExec.FRQScheduleID:
      ASSIGN        
         llTestMode = (FuncRunQSchedule.RunMode = "Test")
         liFRQueueID = FuncRunQSchedule.FRQScheduleID.
   END. 
   
   ASSIGN 
      llReplica    = fIsThisReplica()
      lcXMLVersion = fCParamC("InvXMLVersion")  
      llFormatted  = (fCParamI("InvXMLFormatted") = 1)
      lcTarFinalDir = IF llTestMode 
                      THEN fCParamC("FRTestRunDir")
                      ELSE fCParamC("InvXMLTarTrans")
      liTarFileLimit = fCParamI("InvXMLTarLimit")
      lcCurrency   = fCParamC("DefCurrency")
      ldMinInvAmt  = fCParamDE("MinInvAmt")
      lcSesNum     = SESSION:NUMERIC-FORMAT
      lcNonCombinedData = fCParamC("NON_COMBINED_DATA_ROWS")
      lcRefAmt     = IF iiInvCount = 0
                     THEN ""
                     ELSE " / " + STRING(iiInvCount)
      llInterrupt  = FALSE
      SESSION:NUMERIC-FORMAT = "american".

   /* test files to their own dir */
   IF llTestMode AND lcTransDir = lcTarFinalDir AND liFrQueueID > 0 THEN DO:
      ASSIGN 
         lcTransDir = lcTransDir + "/frq_" + STRING(liFrQueueID)
         lcTarFinalDir = lcTarFinalDir + "/frq_" + STRING(liFrQueueID).
   END.
   
   lcTarFile = "".
   IF llReplica THEN DO:
      lcTarFile = fCParamC("InvXMLTarReplica").
      IF lcTarFile = ? THEN lcTarFile = "".
   END.
   IF lcTarFile = "" THEN lcTarFile = fCParamC("InvXMLTarFile").

   IF NUM-ENTRIES(icFile,"/") > 1 THEN 
   DO liCnt = 2 TO NUM-ENTRIES(icFile,"/") - 1:
      lcSpoolDir = lcSpoolDir + "/" + ENTRY(liCnt,icFile,"/").
   END.
   
   IF iiFileType = 1 THEN DO:
      ASSIGN 
         lcTarFile = REPLACE(lcTarFile,"#PHOUSE",icPrintHouse)
         lcTarFile = REPLACE(lcTarFile,"#PROC",
                             STRING(iiFRProcessID) + "#BATCH")
         lcDate    = Func.Common:mDateFmt(idaInvDate,
                                     "yyyymmdd")
         lcTarFile = REPLACE(lcTarFile,"#IDATE",lcDate)
         lcTarBatchFile = REPLACE(lcTarFile,"#BATCH",STRING(liTarBatch)).
         
      IF liTarFileLimit = ? OR liTarFileLimit = 0 THEN 
         liTarFileLimit = 20000.
   END.
      
   IF ldMinInvAmt > 0 AND lcMinInvAmt NE ? THEN 
      lcMinInvAmt = fDispXMLDecimal(ldMinInvAmt) + " " + lcCurrency.
      
   IF lcXMLVersion = ? OR lcXMLVersion = "" THEN lcXMLVersion = lcCodeVersion.

   /* code needs to be updated everytime that xml schema is changed, so 
      version is hardcoded here, cparam exists to give visibility and to make
      sure that right version is deployed */
   IF lcXMLVersion NE lcCodeVersion THEN RETURN "ERROR:Version conflict".
   
   FIND FIRST TMSUser WHERE TMSUser.UserCode = Syst.Var:katun NO-LOCK NO-ERROR.
   IF AVAILABLE TMSUser AND TMSUser.UserName > "" THEN 
      lcTMSUser = TMSUser.UserName.
   ELSE lcTMSUser = Syst.Var:katun.

   /* header texts to temp-table */
   FOR EACH HdrText NO-LOCK WHERE
            HdrText.Brand = Syst.Var:gcBrand:
      CREATE ttHead.
      ASSIGN ttHead.Lang = HdrText.te-kie
             ttHead.Nbr  = HdrText.te-nro
             ttHead.HTxt = HdrText.te-text.
   END.

   EMPTY TEMP-TABLE ttError. 

   IF iiFrProcessID > 0 THEN
   FOR FIRST FuncRunProcess NO-LOCK WHERE
             FuncRunProcess.FrProcessID = iiFrProcessID:
      liFRExecID = FuncRunProcess.FRExecID.
   END.
 
   RETURN "".  

END PROCEDURE.

PROCEDURE pInitXML:

   /* check that file doesn't exist and form the complete name */
   IF ilSeparate THEN DO: 
      lcLocalFile = REPLACE(icFile,"#INVNUM",
                            IF Invoice.InvType = 99
                            THEN Invoice.ExtInvID
                            ELSE STRING(Invoice.InvNum)).
   END.
   ELSE lcLocalFile = icFile.

   IF VALID-HANDLE(lhXML) THEN DELETE OBJECT lhXML.
   
   CREATE SAX-WRITER lhXML IN WIDGET-POOL "PrintHouse".
   lhXML:FORMATTED = llFormatted.
   lhXML:ENCODING = 'UTF-8'.
   lhXML:SET-OUTPUT-DESTINATION("FILE",lcLocalFile).                 

   lhXML:START-DOCUMENT().
   lhXML:WRITE-COMMENT("Created by Qvantel Oy").
   lhXML:START-ELEMENT("InvoiceDocument").
   lhXML:INSERT-ATTRIBUTE("Document","Invoice").
   lhXML:INSERT-ATTRIBUTE("Version",lcXMLVersion). 

   lhXML:START-ELEMENT("Batch").
   lhXML:WRITE-DATA-ELEMENT("Company",Syst.Var:ynimi).
   lhXML:WRITE-DATA-ELEMENT("Orderer",lcTMSUser).
   lhXML:WRITE-DATA-ELEMENT("Created",Func.Common:mISOTimeZone(TODAY,TIME)).
   lhXML:END-ELEMENT("Batch").

END PROCEDURE.

PROCEDURE pFinalizeXML:

   DEF VAR liMove AS INT  NO-UNDO.
   DEF VAR lcTarOption AS CHAR NO-UNDO.
   DEF VAR lcPlainLocalFile AS CHAR NO-UNDO.
   
   lhXML:END-ELEMENT("InvoiceDocument").
   lhXML:END-DOCUMENT().

   IF ilSeparate AND AVAILABLE Invoice THEN DO:
   
      /* add to tar file */
      IF iiFileType = 1 THEN DO:
         FILE-INFO:FILE-NAME = lcTarBatchFile.
         IF FILE-INFO:FILE-TYPE BEGINS "F" THEN lcTarOption = "r".
         ELSE lcTarOption = "c".
         
         lcPlainLocalFile = ENTRY(NUM-ENTRIES(lcLocalFile,"/"),
                                  lcLocalFile,"/").
         
         UNIX SILENT VALUE("tar -" + lcTarOption + 
                           "f " + lcTarBatchFile +
                           (IF lcSpoolDir > "" 
                            THEN " -C " + lcSpoolDir 
                            ELSE "") + " " + 
                           lcPlainLocalFile).

         liTarCnt = liTarCnt + 1.                  
         /* move the tar file to its final directory */
         IF liTarCnt >= liTarFileLimit AND lcTarFinalDir > "" THEN DO:
            RUN pFinalizeTarFile. 
            IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
            ASSIGN 
               liTarCnt = 0
               liTarBatch = liTarBatch + 1
               lcTarBatchFile = REPLACE(lcTarFile,"#BATCH",STRING(liTarBatch)).
         END.
      END.
      
      /* move the new file to the actual transfer directory 
         try several times, in case directory creation fails */
      DO liMove = 1 TO 8:

         lcFinalDir = fTargetDir(lcTransDir,
                                 Invoice.InvDate,
                                 Invoice.InvNum).

         IF fMove2TransDirOverwrite(lcLocalFile,
                                    lcFinalDir)
         THEN LEAVE.
      END.
   END.
   
   IF VALID-HANDLE(lhXML) THEN DELETE OBJECT lhXML.

   RETURN "". 
   
END PROCEDURE.

PROCEDURE pInvoice2XML:

   PrintMainLoop:
   FOR EACH ttInvoice USE-INDEX ZipCode,
      FIRST Invoice OF ttInvoice NO-LOCK,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = Invoice.CustNum
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      /* unexpected interrupt occurred */
      IF RETRY THEN DO:
   
         fErrLine("ERROR:Printing was abnormally interrupted").
         lhXML:WRITE-COMMENT("#" + lcLastError).
         llInterrupt = TRUE.   

         LEAVE.
      END.

      RUN pGetInvoiceHeaderData.
      IF RETURN-VALUE BEGINS "ERROR" THEN NEXT PrintMainLoop.
      
      /* XML files older than 2010 should not have
         bank and address information. YOT-1276 */
      IF Invoice.InvDate < 1/1/2010 THEN ASSIGN
         lcBankOffice = ""
         lcBankAcc = ""
         lcAddress = ""
         lcZipCode = ""
         lcPost = ""
         lcRegion = ""
         lcCountryName = "".

      IF ilSeparate THEN RUN pInitXML.
 
      oiInvCount = oiInvCount + 1.

      IF NOT SESSION:BATCH AND oiInvCount > 1 AND 
          (oiInvCount < 100 OR oiInvCount MOD 100 = 0) 
      THEN DO:
          PAUSE 0.
          DISPLAY oiInvCount FORMAT ">>>>>>>9"
                  lcRefAmt   FORMAT "x(12)"
          WITH NO-LABELS OVERLAY ROW 10 CENTERED
               TITLE " Printing " FRAME fPrint.
      END.
 
      IF iiUpdInterval > 0 AND oiInvCount MOD iiUpdInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiInvCount) THEN
            RETURN "ERROR:Stopped".
      END.   
 
      lhXML:START-ELEMENT("Invoice").
      lhXML:INSERT-ATTRIBUTE("Language",STRING(liLanguage,"99")).
      lhXML:INSERT-ATTRIBUTE("PrintHouseCode",icPrintHouse).
   
      /* header */
      lhXML:START-ELEMENT("Header").

      lhXML:WRITE-DATA-ELEMENT("InvoiceID",Invoice.ExtInvID).
      lhXML:START-ELEMENT("InvoiceDate").
      lhXML:INSERT-ATTRIBUTE("DateTime",Func.Common:mISOTimeZone(Invoice.InvDate, 0)).
      lhXML:WRITE-CHARACTERS(fDispDate(Invoice.InvDate, liLanguage)).
      lhXML:END-ELEMENT("InvoiceDate").      
      lhXML:WRITE-DATA-ELEMENT("DueDate",fDispDate(Invoice.DueDate,
                                                   liLanguage)).

      lhXML:START-ELEMENT("BillingPeriod").
      lhXML:WRITE-DATA-ELEMENT("BeginDate",fDispDate(Invoice.FromDate,
                                                     liLanguage)).
      lhXML:WRITE-DATA-ELEMENT("EndDate",fDispDate(Invoice.ToDate,
                                                   liLanguage)).
      lhXML:END-ELEMENT("BillingPeriod").

      lhXML:WRITE-DATA-ELEMENT("InvoiceType","Normal").
      
      lcRowText = Func.Common:mTMSCodeName("Invoice",
                                   "DelType",
                                   STRING(Invoice.DelType)).
      IF lcRowText = "" THEN lcRowText = STRING(Invoice.DelType).
      lhXML:WRITE-DATA-ELEMENT("DeliveryType",lcRowText).

      lcRowText = Func.Common:mTMSCodeName("Invoice",
                                   "ChargeType",
                                   STRING(Invoice.ChargeType)).
      IF lcRowText = "" THEN lcRowText = STRING(Invoice.ChargeType).
      lhXML:WRITE-DATA-ELEMENT("ChargeType",lcRowText).

      lhXML:START-ELEMENT("CustomHeader").

      lhXML:START-ELEMENT("CustomHeaderData").
      lhXML:WRITE-DATA-ELEMENT("CustomType","TotalAmount").
      lhXML:WRITE-DATA-ELEMENT("CustomValue",fDispXMLDecimal(Invoice.InvAmt)).
      lhXML:END-ELEMENT("CustomHeaderData").

      lhXML:END-ELEMENT("CustomHeader").
       
      lhXML:END-ELEMENT("Header").

      /* customer data */
      lhXML:START-ELEMENT("Customer").
   
      lhXML:WRITE-DATA-ELEMENT("CustomerType","Invoice").
      lhXML:WRITE-DATA-ELEMENT("CustomerNumber",STRING(Invoice.CustNum)).
      lhXML:WRITE-DATA-ELEMENT("CustomerID",Customer.OrgID).

      lhXML:START-ELEMENT("CustomerName").
      lhXML:WRITE-DATA-ELEMENT("FirstName",lcFirstName).
      lhXML:WRITE-DATA-ELEMENT("FullName",lcCustName).
      lhXML:END-ELEMENT("CustomerName").
   
      lhXML:START-ELEMENT("CustomerAddress").
      lhXML:START-ELEMENT("StreetAddress").
      lhXML:WRITE-DATA-ELEMENT("Address",lcAddress).
      lhXML:END-ELEMENT("StreetAddress").
      lhXML:WRITE-DATA-ELEMENT("ZipCode",lcZipCode).
      lhXML:WRITE-DATA-ELEMENT("City",lcPost).
      lhXML:WRITE-DATA-ELEMENT("Region",lcRegion).
      lhXML:WRITE-DATA-ELEMENT("Country",lcCountryName).
      lhXML:END-ELEMENT("CustomerAddress").
  
      lhXML:WRITE-DATA-ELEMENT("CustomerTaxZone",Invoice.TaxZone).
     
      FIND FIRST CustCat NO-LOCK WHERE
                 CustCat.Brand EQ Syst.Var:gcBrand AND
                 CustCat.Category EQ Customer.Category NO-ERROR.
         IF AVAILABLE CustCat THEN
            lhXML:WRITE-DATA-ELEMENT("Segment",CustCat.Segment).
         ELSE
            lhXML:WRITE-DATA-ELEMENT("Segment","").
             
      lhXML:END-ELEMENT("Customer").

      RUN pGetInvoiceRowData.
      
      /* main invoice amounts */
      lhXML:START-ELEMENT("InvoiceAmount").

      lhXML:START-ELEMENT("AmountDetail").
      lhXML:INSERT-ATTRIBUTE("Type","Main").
      lhXML:WRITE-DATA-ELEMENT("AmountExclTax",
                                fDispXMLDecimal(Invoice.AmtExclVat)).
      lhXML:WRITE-DATA-ELEMENT("TaxAmount",fDispXMLDecimal(Invoice.VatAmt)).
      
      RUN pGetInvoiceVatData.  

      FOR EACH ttVat NO-LOCK:
         lhXML:START-ELEMENT("TaxDetails").
         lhXML:WRITE-DATA-ELEMENT("TaxZone",lcTaxZone).
         lhXML:WRITE-DATA-ELEMENT("TaxPercent",fDispXMLDecimal(ttVat.VatPerc)).
         lhXML:WRITE-DATA-ELEMENT("AmountExclTax",
                                  fDispXMLDecimal(ttVat.VatBasis)).
         lhXML:WRITE-DATA-ELEMENT("TaxAmount",
                                  fDispXMLDecimal(ttVat.VatAmt)).
         lhXML:WRITE-DATA-ELEMENT("Amount",fDispXMLDecimal(ttVat.VatBasis +
                                                           ttVat.VatAmt)).
         lhXML:END-ELEMENT("TaxDetails").
      END.

      /* As per requirement, Discount Amt value has to be subtracted from Installment Amt. 
         But Discound Amt is negative value, so it is  added to InstallmentAmt value */
      lhXML:START-ELEMENT("AdditionalDetail").
      lhXML:START-ELEMENT("AdditionalAmount").
      lhXML:INSERT-ATTRIBUTE("Header","AmountInstallment").
      lhXML:WRITE-CHARACTERS(fDispXMLDecimal(ttInvoice.InstallmentAmt +
                                             ttInvoice.InstallmentDiscAmt)).
      lhXML:END-ELEMENT("AdditionalAmount").
      lhXML:END-ELEMENT("AdditionalDetail").

      /*Google Billing*/
      IF ttInvoice.GBValue GT 0 THEN DO: /* check if google payments */
         lhXML:START-ELEMENT("AdditionalDetail").
         lhXML:START-ELEMENT("AdditionalAmount").
         lhXML:INSERT-ATTRIBUTE("Header", "GOOGLEVAS").
         lhXML:WRITE-CHARACTERS(fDispXMLDecimal(ttInvoice.GBValue)).
         lhXML:END-ELEMENT("AdditionalAmount").
         lhXML:END-ELEMENT("AdditionalDetail"). 
      END.
      IF ttInvoice.GBDiscValue LT 0 THEN DO: /* Discount is negative value */
         lhXML:START-ELEMENT("AdditionalDetail").
         lhXML:START-ELEMENT("AdditionalAmount").
         lhXML:INSERT-ATTRIBUTE("Header", "GOOGLEVASFAT").
         lhXML:WRITE-CHARACTERS(fDispXMLDecimal(ttInvoice.GBDiscValue)).
         lhXML:END-ELEMENT("AdditionalAmount").
         lhXML:END-ELEMENT("AdditionalDetail").
      END.
      /* AmountExclTaxAndInstallment, TotalAmountExclInstallment  */
      /* As per requirement, Discount Amt value has to be added to Installment Amt.
         But Discound Amt is negative value, so it is subtracted to InstallmentAmt value */
      lhXML:START-ELEMENT("AdditionalDetail").
      lhXML:START-ELEMENT("AdditionalAmount").
      lhXML:INSERT-ATTRIBUTE("Header","AmountExclTaxAndInstallment").
      lhXML:WRITE-CHARACTERS(fDispXMLDecimal(
                             Invoice.AmtExclVat
                             - ttInvoice.InstallmentAmt
                             - ttInvoice.PenaltyAmt
                             - ttInvoice.InstallmentDiscAmt
                             - ttInvoice.GBValue
                             - ttInvoice.GBDiscValue)).  
      lhXML:END-ELEMENT("AdditionalAmount").
      lhXML:END-ELEMENT("AdditionalDetail").
      
      IF ttInvoice.InstallmentAmt > 0 OR
         ttInvoice.GBValue > 0 THEN DO:
         lhXML:START-ELEMENT("AdditionalDetail").
         lhXML:START-ELEMENT("AdditionalAmount").
         lhXML:INSERT-ATTRIBUTE("Header","TotalAmountExclInstallment").
         lhXML:WRITE-CHARACTERS(fDispXMLDecimal(
                                Invoice.InvAmt
                                - ttInvoice.InstallmentAmt
                                - ttInvoice.PenaltyAmt
                                - ttInvoice.InstallmentDiscAmt
                                - ttInvoice.GBValue
                                - ttInvoice.GBDiscValue)).
         lhXML:END-ELEMENT("AdditionalAmount").
         lhXML:END-ELEMENT("AdditionalDetail").
      END.
      
      IF ttInvoice.PenaltyAmt > 0 THEN DO:
         lhXML:START-ELEMENT("AdditionalDetail").
         lhXML:START-ELEMENT("AdditionalAmount").
         lhXML:INSERT-ATTRIBUTE("Header","PermanencyFailure").
         lhXML:WRITE-CHARACTERS(fDispXMLDecimal(ttInvoice.PenaltyAmt)).
         lhXML:END-ELEMENT("AdditionalAmount").
         lhXML:END-ELEMENT("AdditionalDetail").
      END.
      
      lhXML:WRITE-DATA-ELEMENT("TotalAmount",fDispXMLDecimal(Invoice.InvAmt)).
      lhXML:WRITE-DATA-ELEMENT("Currency",Invoice.Currency).
      
      lhXML:END-ELEMENT("AmountDetail").
      
      lhXML:END-ELEMENT("InvoiceAmount").
      
      lhXML:START-ELEMENT("PaymentInfo").
      lhXML:START-ELEMENT("CustomerBank").
      lhXML:START-ELEMENT("BankAccount").
      lhXML:INSERT-ATTRIBUTE("AccountType","National").
      lhXML:WRITE-CHARACTERS(lcBankAcc).
      lhXML:END-ELEMENT("BankAccount").
      lhXML:WRITE-DATA-ELEMENT("BankOffice",lcBankOffice).
      lhXML:END-ELEMENT("CustomerBank").
      lhXML:END-ELEMENT("PaymentInfo").

      /* diagram of usage */
      lhXML:START-ELEMENT("CustomRow").
      lhXML:WRITE-DATA-ELEMENT("CustomRowType","RowDiagram").

      lhXML:START-ELEMENT("CustomRowData").
      lhXML:WRITE-DATA-ELEMENT("CustomType","MaximumAmount").
      /* add a 10% margin */
      lhXML:WRITE-DATA-ELEMENT("CustomValue",fDispXMLDecimal(ldMaxRow * 1.1)).
      lhXML:END-ELEMENT("CustomRowData").

      lhXML:START-ELEMENT("CustomRowData").
      lhXML:WRITE-DATA-ELEMENT("CustomType","MinimumAmount").
      lhXML:WRITE-DATA-ELEMENT("CustomValue",fDispXMLDecimal(ldMinRow)).
      lhXML:END-ELEMENT("CustomRowData").

      liPCnt = 0.
      FOR EACH ttGraph USE-INDEX Order:

         IF ROUND(ttGraph.GraphAmt,3) = 0 THEN NEXT.
      
         lhXML:START-ELEMENT("CustomRowData").
         lhXML:WRITE-DATA-ELEMENT("CustomType","DiagramPoint").
         lhXML:WRITE-DATA-ELEMENT("CustomContent",ttGraph.GraphGroup).
         lhXML:WRITE-DATA-ELEMENT("CustomValue",
                                  fDispXMLDecimal(ttGraph.GraphAmt)).
         lhXML:END-ELEMENT("CustomRowData").

         liPCnt = liPCnt + 1.
      END. 
 
      lhXML:END-ELEMENT("CustomRow").

      RUN pGetSubInvoiceHeaderData.
      
      IF ttInvoice.PostPoned THEN DO:
         lhXML:START-ELEMENT("CustomRow").
         lhXML:WRITE-DATA-ELEMENT("CustomRowType","PostponedPayment").
         lhXML:END-ELEMENT("CustomRow").
      END.
   
      IF ttInvoice.q25Phase LT 99 THEN DO:
         lhXML:START-ELEMENT("CustomRow").
         lhXML:WRITE-DATA-ELEMENT("CustomRowType",ENTRY(ttInvoice.q25Phase + 1,
                                  "Q25Month24,Q25Month23,Q25Month22")).
         lhXML:END-ELEMENT("CustomRow").
      END.      

      /* subscription level */
      RUN pSubInvoice2XML. 

      IF ttInvoice.Printed NE 2
      THEN ttInvoice.Printed = 1.

      lhXML:END-ELEMENT("Invoice").

      IF ilSeparate THEN RUN pFinalizeXML.
      IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
  
   END.     /* END OF INVOICES             */

   HIDE FRAME fPrint NO-PAUSE.
   
   RETURN "".

END PROCEDURE.   /* pinvoice2xml */

      
PROCEDURE pSubInvoice2XML:

   DEF VAR llPremiumNumberText    AS LOGICAL INITIAL FALSE  NO-UNDO.
   DEF VAR lcBIGroupName          AS CHAR NO-UNDO. 
   DEF VAR liTFCount              AS INT  NO-UNDO.
   DEF VAR llGBText               AS LOGICAL INITIAL FALSE  NO-UNDO.
   DEF VAR lcFooterNotice         AS CHAR NO-UNDO.

    
   lhXML:START-ELEMENT("Contract").

   FOR EACH SubInvoice OF Invoice NO-LOCK,
      FIRST ttSub WHERE 
            ttSub.CLI   = SubInvoice.CLI  AND
            ttSub.MsSeq = SubInvoice.MsSeq:
      
      /* subscription */
      lhXML:START-ELEMENT("ContractDetail").
      IF SubInvoice.FixedNumber NE ? AND
         SubInvoice.FixedNumber > "" AND
         ttSub.CliEvent EQ "F" AND NOT /* Conv partially installed. No mobile */
         ttSub.PrintCLI THEN /* Mobile active during period. */
         lhXML:WRITE-DATA-ELEMENT("ContractID",SubInvoice.FixedNumber).
      ELSE 
         lhXML:WRITE-DATA-ELEMENT("ContractID",SubInvoice.CLI).
      lhXML:START-ELEMENT("ContractType").      
      IF fIsPro(Customer.Category) THEN
         lhXML:INSERT-ATTRIBUTE("Name",ttSub.CTName + " PRO"). /* YCO-27 */ 
      ELSE
         lhXML:INSERT-ATTRIBUTE("Name",ttSub.CTName). /* No PRO logic needed */               
      lhXML:WRITE-CHARACTERS(ttSub.CLIType).      
      lhXML:END-ELEMENT("ContractType").

      IF ttSub.OldCLIType > "" THEN DO:
         lhXML:START-ELEMENT("OldContractType").
         IF fIsPro(Customer.Category) THEN
            lhXML:WRITE-DATA-ELEMENT("TariffName",ttSub.OldCTName + " PRO").  /* YCO-27 */
         ELSE
            lhXML:WRITE-DATA-ELEMENT("TariffName",ttSub.OldCTName). /* No PRO logic needed */
         
         lhXML:WRITE-DATA-ELEMENT("TariffType",ttSub.OldCLIType).
         lhXML:WRITE-DATA-ELEMENT("TariffDate",ttSub.TariffActDate).
         lhXML:END-ELEMENT("OldContractType").         
      END.
      
      lhXML:START-ELEMENT("ContractName").
      lhXML:WRITE-DATA-ELEMENT("FullName",ttSub.UserName).
      lhXML:END-ELEMENT("ContractName").

      IF SubInvoice.FixedNumber > "" AND
         SubInvoice.FixedNumber <> SubInvoice.CLI AND
        (ttSub.CliEvent         <> "F" OR
         ttSub.PrintCLI) THEN DO:
         lhXML:START-ELEMENT("CustomContract").
         lhXML:WRITE-DATA-ELEMENT("CustomType","AdditionalContractID").
         lhXML:WRITE-DATA-ELEMENT("CustomContent",SubInvoice.FixedNumber).
         lhXML:WRITE-DATA-ELEMENT("CustomerIUA", ttSub.IUA).
         lhXML:END-ELEMENT("CustomContract").
      END.
      IF ttSub.MessageType > "" THEN DO:
         lhXML:START-ELEMENT("CustomContract").
         lhXML:WRITE-DATA-ELEMENT("CustomType","Message").
         lhXML:WRITE-DATA-ELEMENT("CustomContent",ttSub.MessageType).
         lhXML:END-ELEMENT("CustomContract").         
      END.

      /* invoice rows */
      lhXML:START-ELEMENT("InvoiceRow").

      FOR EACH ttRow WHERE
               ttRow.SubInvNum = SubInvoice.SubInvNum
      BY ttRow.GroupOrder 
      BY ttRow.RowGroup
      BY ttRow.RowOrder:

         /* row, billing item level */
         lhXML:START-ELEMENT("RowDetail").
         IF ttRow.RowCode BEGINS "44" OR
            ttRow.RowCode BEGINS "45" THEN 
            lhXML:INSERT-ATTRIBUTE("Type", ttRow.RowBillCode).
         
         IF ttRow.RowType > "" THEN DO:
            lhXML:INSERT-ATTRIBUTE("Type",ttRow.RowType).
            lhXML:INSERT-ATTRIBUTE("BillingItemGroupID",ttRow.RowGroup).
         END.
         IF ttRow.RowType > "" AND
            ttRow.RowGroup EQ "46" THEN /* Convergent uses CLI Type Name */
            lhXML:WRITE-DATA-ELEMENT("BillingItem",CAPS(ttSub.CTName)).
         ELSE
            lhXML:WRITE-DATA-ELEMENT("BillingItem",ttRow.RowName).
         lhXML:WRITE-DATA-ELEMENT("Quantity", STRING(ttRow.RowQty)).
 
         /* duration or data amount */
         IF ttRow.RowData > 0 THEN DO:
            /* for summary data is displayed in MB, 
               ttRow is calculated from InvRows which contains KBs */
            ttRow.RowData = ttRow.RowData / 1024.
            lhXML:WRITE-DATA-ELEMENT("DataAmount",
                                     fDispXMLDecimal(ttRow.RowData)).
         END.
         ELSE IF ttRow.RowDur NE 0 THEN DO:
            lcLine = TRIM(Func.Common:mSec2C(DECIMAL(ttRow.RowDur),10)).
            lhXML:WRITE-DATA-ELEMENT("Duration",lcLine).
         END.   
         /* Show Voice and Data month limit in FLATMFx row*/
         ELSE IF ttRow.RowGroup = "18" AND
                 ttRow.RowType = "" AND
                 INDEX(ttRow.RowBillCode,"FLAT") > 0 AND
                ((ttSub.CLIType BEGINS "CONTF" AND
                  NOT ttSub.CLIType BEGINS "CONTFH")
                  OR
                  (ttSub.OldCLIType BEGINS "CONTF" AND
                   NOT ttSub.OldCLIType BEGINS "CONTFH") 
                  ) THEN DO:
            lhXML:WRITE-DATA-ELEMENT("DataAmount",STRING(ttRow.DataLimit)).
            lhXML:WRITE-DATA-ELEMENT("Duration",STRING(ttRow.VoiceLimit)).
         END.

         IF ttSub.OldCLIType > "" AND ttRow.RowGroup = "3" AND
            ttRow.RowType = "" AND
            (INDEX(ttRow.RowBillCode,"MDUB") > 0 OR
             INDEX(ttRow.RowBillCode,"DATA") > 0) THEN
            lhXML:WRITE-DATA-ELEMENT("ToDate",STRING(ttRow.RowToDate)).
         
         lhXML:WRITE-DATA-ELEMENT("AmountExclTax",
                                  fDispXMLDecimal(ttRow.RowAmtExclVat)).
         lhXML:WRITE-DATA-ELEMENT("TaxAmount",
                                  fDispXMLDecimal(ttRow.RowVatAmt)).
         lhXML:WRITE-DATA-ELEMENT("Amount",fDispXMLDecimal(ttRow.RowAmt)).
         lhXML:END-ELEMENT("RowDetail").
      END.

      /* total of subinvoice */
      lhXML:START-ELEMENT("RowDetail").
      lhXML:INSERT-ATTRIBUTE("Type","Total").
      lhXML:WRITE-DATA-ELEMENT("AmountExclTax",
                               fDispXMLDecimal(SubInvoice.AmtExclVat)).
      lhXML:WRITE-DATA-ELEMENT("TaxAmount",
                               fDispXMLDecimal(SubInvoice.VatAmt)).
      lhXML:WRITE-DATA-ELEMENT("Amount",fDispXMLDecimal(SubInvoice.InvAmt)).
      lhXML:END-ELEMENT("RowDetail").

      /* TF phase2 - Footer Text */
      DO liTFCount = 1 TO EXTENT(ttSub.TFBankFooterText):
         IF ttSub.TFBankFooterText[liTFCount] > "" THEN DO:
            lhXML:START-ELEMENT("RowDetail").
            lhXML:INSERT-ATTRIBUTE("Type","FooterText").
            lhXML:WRITE-DATA-ELEMENT("Text",ttSub.TFBankFooterText[liTFCount]).
            lhXML:END-ELEMENT("RowDetail").
         END.
      END.

      lhXML:END-ELEMENT("InvoiceRow").

      /* subinvoice amounts */
      lhXML:START-ELEMENT("SubInvoiceAmount").

      lhXML:START-ELEMENT("AmountDetail").
      lhXML:INSERT-ATTRIBUTE("Type","Sub").
      lhXML:WRITE-DATA-ELEMENT("AmountExclTax",
                                fDispXMLDecimal(SubInvoice.AmtExclVat)).
      lhXML:WRITE-DATA-ELEMENT("TaxAmount",fDispXMLDecimal(SubInvoice.VatAmt)).
      
      lhXML:START-ELEMENT("AdditionalDetail").
      lhXML:START-ELEMENT("AdditionalAmount").
      lhXML:INSERT-ATTRIBUTE("Header","AmountExclTaxAndInstallment").
      lhXML:WRITE-CHARACTERS(fDispXMLDecimal(SubInvoice.AmtExclVat - 
                                             ttSub.InstallmentAmt  -
                                             ttSub.PenaltyAmt      -
                                             ttSub.InstallmentDiscAmt -
                                             ttSub.GBValue)).

      lhXML:END-ELEMENT("AdditionalAmount").
     
      DO liTFCount = 1 TO EXTENT(ttSub.TFBankBeforeAmt):
       
          IF ttSub.TFBankBeforeAmt[liTFCount] > 0 THEN DO:
             lhXML:START-ELEMENT("AdditionalAmount").
             lhXML:INSERT-ATTRIBUTE("Header","PendingInstallmentAmountBeforeInvoice").
             lhXML:WRITE-CHARACTERS(fDispXMLDecimal(ttSub.TFBankBeforeAmt[liTFCount])).
             lhXML:END-ELEMENT("AdditionalAmount").

             lhXML:START-ELEMENT("AdditionalAmount").
             lhXML:INSERT-ATTRIBUTE("Header","PendingInstallmentAmountAfterInvoice").
             lhXML:WRITE-CHARACTERS(fDispXMLDecimal(ttSub.TFBankAfterAmt[liTFCount])).
             lhXML:END-ELEMENT("AdditionalAmount").
          END.

      END.
        
      lhXML:END-ELEMENT("AdditionalDetail").

      lhXML:WRITE-DATA-ELEMENT("TotalAmount", fDispXMLDecimal(SubInvoice.InvAmt)).

      lhXML:END-ELEMENT("AmountDetail").
      
      lhXML:END-ELEMENT("SubInvoiceAmount").
 
      RUN pCollectCDR(SubInvoice.InvSeq,
                      INPUT-OUTPUT llPremiumNumberText,
                      INPUT-OUTPUT llGBText).
      
      FOR EACH ttCall NO-LOCK 
      BREAK BY ttCall.GroupOrder
            BY ttCall.BIGroup
            BY ttCall.DateSt
            BY ttCall.TimeSt:

         ASSIGN 
            lcCTName  = ""  
            ldEventTS = Func.Common:mMake2DT(ttCall.DateSt,ttCall.TimeSt).

         FIND FIRST ttCLIType WHERE
                    ttCLIType.CLI    = SubInvoice.CLI AND
                    ttCLIType.TSBeg <= ldEventTS AND
                    ttCLIType.TSEnd >= ldEventTS NO-ERROR.
         IF NOT AVAILABLE ttCLIType THEN 
            FIND LAST ttCLIType WHERE ttCLIType.CLI = SubInvoice.CLI NO-ERROR.
         IF AVAILABLE ttCLIType THEN 
            lcCTName = IF Invoice.InvDate >= 10/1/8 AND ttCLIType.RateName > "" 
                       THEN ttCLIType.RateName
                       ELSE ttCLIType.CTName.
 
         IF FIRST-OF(ttCall.BIGroup) THEN DO:
            lhXML:START-ELEMENT("EDR").
            /* is call itemization printed */
            lhXML:INSERT-ATTRIBUTE("PrintToInvoice",
                                   TRIM(STRING(ttSub.CallSpec > 0,"Yes/No"))).
            lhXML:INSERT-ATTRIBUTE("BillingItemGroupID", 
                                   STRING(ttCall.BIGroup)).
            lcBIGroupName = fLocalItemName("BItemGroup",
                                           ttCall.BIGroup,
                                           liLanguage,
                                           Invoice.ToDate).
            lhXML:INSERT-ATTRIBUTE("BillingItemGroup", lcBIGroupName).
            IF ttSub.FixedNumber > "" THEN 
               lhXML:INSERT-ATTRIBUTE("BillingItemGroupType",
                                   TRIM(STRING(ttCall.GroupType = 1,"fixed/mobile"))).
         END.

         IF FIRST-OF(ttCall.DateSt) THEN DO:
            EMPTY TEMP-TABLE ttData.
         END.

         /* some data rows are combined on daily level */
         IF ttCall.BIGroup EQ {&BITEM_GRP_INTERNET} AND
            LOOKUP(ttCall.BillCode,lcNonCombinedData) = 0 THEN DO:
            FIND FIRST ttData WHERE ttData.BIName = ttCall.BillItemName NO-ERROR.
            IF NOT AVAILABLE ttData THEN DO:
               CREATE ttData.
               ttData.BIName = ttCall.BillItemName.
            END.
            ASSIGN 
               ttData.DataAmt = ttData.DataAmt + ttCall.DataIn + ttCall.DataOut
               ttData.DataSum = ttData.DataSum + ttCall.Amount.
         END.

         /* other cdrs are each printed */   
         ELSE DO:
              
            lhXML:WRITE-EMPTY-ELEMENT("EDRDetail").
            lhXML:INSERT-ATTRIBUTE("Date",fDispDayMonth(ttCall.DateSt,
                                                             liLanguage)).
            lhXML:INSERT-ATTRIBUTE("Time",
                                   RIGHT-TRIM(STRING(ttCall.TimeSt,
                                                     "hh:mm"))).
            IF ttCall.GsmBnr EQ {&GB_B_NBR} THEN
               lhXML:INSERT-ATTRIBUTE("Destination","").
            ELSE
               lhXML:INSERT-ATTRIBUTE("Destination",ttCall.GsmBnr).
            lhXML:INSERT-ATTRIBUTE("BillingItem",ttCall.BillItemName).

            lcTipoName = fLocalCCName().
            lhXML:INSERT-ATTRIBUTE("CCN",lcTipoName).

            /* duration or data amount */
            IF ttCall.DataIn + ttCall.DataOut > 0 THEN DO:
               lcLine = fDispXMLDecimal3((ttCall.DataIn + ttCall.DataOut) / 
                                         ttSub.DataConv).
               lhXML:INSERT-ATTRIBUTE("DataAmount",lcLine).
            END.

            ELSE IF ttCall.BillDur NE 0 OR ttCall.EventType EQ "CALL" THEN DO:
               lcLine = TRIM(Func.Common:mSec2MinC(ttCall.BillDur,8)).
               lhXML:INSERT-ATTRIBUTE("Duration",lcLine).
            END.   

            ELSE DO:
               lhXML:INSERT-ATTRIBUTE("Quantity","1").
            END.
                
            lhXML:INSERT-ATTRIBUTE("Rating",lcCTName).
            lhXML:INSERT-ATTRIBUTE("Amount",
                                   fDispXMLDecimal3(ttCall.Amount)).
                                     
         END.
         
         /* combined data row as last row of the day */
         IF LAST-OF(ttCall.DateSt) THEN 
         FOR EACH ttData:
            
            lhXML:WRITE-EMPTY-ELEMENT("EDRDetail").
            lhXML:INSERT-ATTRIBUTE("Date",fDispDayMonth(ttCall.DateSt,
                                                          liLanguage)).
            lhXML:INSERT-ATTRIBUTE("BillingItem",ttData.BIName).

            lhXML:INSERT-ATTRIBUTE("DataAmount",
                                   fDispXMLDecimal3(ttData.DataAmt / 
                                                    ttSub.DataConv)).
            lhXML:INSERT-ATTRIBUTE("Rating",lcCTName).
            lhXML:INSERT-ATTRIBUTE("Amount",
                                   fDispXMLDecimal3(ttData.DataSum)).     
                                     
         END.
 
         IF LAST-OF (ttCall.BIGroup) THEN DO:
            lhXML:END-ELEMENT("EDR").
         END.
         
      END. 

      lhXML:END-ELEMENT("ContractDetail").

   END.     

   /*Google billing footer will be added if there are GB entreis*/
   lcFooterNotice = "".
   IF llPremiumNumberText THEN DO:
      IF lcFooterNotice NE "" THEN
         lcFooterNotice = lcFooterNotice + CHR(10) + CHR(10).
      lcFooterNotice = lcFooterNotice +  fHeadTxt(573,liLanguage).
   END.
   IF llGBText THEN DO:
       IF lcFooterNotice NE "" THEN
          lcFooterNotice = lcFooterNotice + CHR(10) + CHR(10).
      lcFooterNotice = lcFooterNotice + fHeadTxt(574,liLanguage).
   END.
   lhXML:WRITE-DATA-ELEMENT("FooterNotice", lcFooterNotice).

   lhXML:END-ELEMENT("Contract").

   RETURN "".

END PROCEDURE.   /* pSubInvoice2xml */

PROCEDURE pFinalizeTarFile:

   DEF VAR lcMD5Check AS CHAR NO-UNDO.
   DEF VAR ldTarSize  AS DEC  NO-UNDO.
 
   ASSIGN 
      FILE-INFO:FILE-NAME = lcTarBatchFile
      lcTarError = "".
   IF NOT FILE-INFO:FILE-TYPE BEGINS "F" THEN 
      lcTarError = "Tar file creation failed".
   ELSE DO:
      liCnt = 0.
      INPUT STREAM sRead THROUGH 
         VALUE("tar -tvf " + lcTarBatchFile + " | wc -l").
      IMPORT STREAM sRead liCnt NO-ERROR.
      INPUT STREAM sRead CLOSE.
      
      IF liCnt NE liTarCnt THEN 
         lcTarError = "Invoice qty in tar file " + lcTarBatchFile +
                      " vrs. printed is " +
                      STRING(liCnt) + "/" + STRING(liTarCnt).

      INPUT STREAM sRead THROUGH
         VALUE("digest -a md5 " + lcTarBatchFile).
      IMPORT STREAM sRead lcMD5Check.
      INPUT STREAM sRead CLOSE.
      
      ldTarSize = FILE-INFO:FILE-SIZE NO-ERROR.
      
      IF lcTarError = "" THEN DO:
         IF NOT fTransDir(lcTarBatchFile,".tar",lcTarFinalDir) THEN 
            lcTarError = "Tar file " + lcTarBatchFile + 
                        " could not be moved to final dir".
               
         ELSE DO TRANS:
            CREATE FuncRunResult.
            ASSIGN 
               FuncRunResult.FRProcessID = iiFRProcessID
               FuncRunResult.FRExecID    = liFRExecID
               FuncRunResult.FRResultSeq = 1
               FuncRunResult.ResultOrder = liTarBatch
               FuncRunResult.IntParam    = liTarCnt
               FuncRunResult.DecParam    = ldTarSize
               FuncRunResult.CharParam   = 
                  ENTRY(NUM-ENTRIES(lcTarBatchFile,"/"),lcTarBatchFile,"/") +
                  " MD5:" + lcMD5Check.
         END.      
      END.   

   END.                            
   
   IF lcTarError > "" THEN DO:
      CREATE ttError.
      ttError.ErrMsg = lcTarError.
      IF iiFrProcessID > 0 THEN DO:
         ASSIGN 
            ttError.TableName = "FuncRunProcess"
            ttError.KeyValue  = STRING(iiFRProcessID).
         /* also to execution level -> easier to view */
         FOR FIRST FuncRunProcess NO-LOCK WHERE
                   FuncRunProcess.FrProcessID = iiFRProcessID:
            CREATE ttError.
            ASSIGN 
               ttError.ErrMsg = lcTarError
               ttError.TableName = "FuncRunExec"
               ttError.KeyValue  = STRING(FuncRunProcess.FRExecID).
         END.
      END.
      ELSE ttError.Inv = "Tar file".
   END.
    
   RETURN "".
   
END PROCEDURE.

FINALLY:

   IF VALID-OBJECT(objDBConn)
   THEN DELETE OBJECT objDBConn.
   
   EMPTY TEMP-TABLE ttBillItemName.
   EMPTY TEMP-TABLE ttBillItemAndGroup.

END FINALLY.

 
