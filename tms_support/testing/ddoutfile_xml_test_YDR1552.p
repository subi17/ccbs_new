/* ----------------------------------------------------------------------
  MODULE .......: ddoutfile_xml.p
  TASK .........: Print invoices to a direct debit file csb19.14 XML
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.01.07
  CHANGED ......: 18.04.07/aam ErrorLog to db
                  25.04.07/aam ilEmptyFile  
                  31.10.13/Jto For XML format cause of SEPA
  Version ......: yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE AllIncludes YES

{commali.i}
{cparam2.i}
{timestamp.i}
{ftransdir.i}
{email.i}
{transname.i}
{fhdrtext.i}
{ddoutfilett.i}
{customer_address.i}
{funcrunprocess_update.i}
{fbankdata.i}
{func.i}
{fsepa.i}

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
                     
DEF VAR lhXML         AS HANDLE NO-UNDO.
DEF VAR llFormatted   AS LOG    NO-UNDO INIT True.
DEF VAR lcRefAmt      AS CHAR NO-UNDO. 
DEF VAR lcLastError   AS CHAR NO-UNDO.
DEF VAR lcTransDir    AS CHAR NO-UNDO.
DEF VAR lcFileExt     AS CHAR NO-UNDO.
DEF VAR liPCnt        AS INT  NO-UNDO.
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR lcConfDir     AS CHAR NO-UNDO. 
DEF VAR lcNewLine     AS CHAR NO-UNDO.
DEF VAR ldInvTot      AS DEC  NO-UNDO.  
DEF VAR lcPlainFile   AS CHAR NO-UNDO. 
DEF VAR ldCurrStamp   AS DEC  NO-UNDO. 
DEF VAR lcNumeric     AS CHAR NO-UNDO.
DEF VAR lcMsgId       AS CHAR NO-UNDO. 
DEF VAR liFrstCounter AS INT NO-UNDO.
DEF VAR liRcurCounter AS INT NO-UNDO. 
DEF VAR lcPmtInfId    AS CHAR NO-UNDO. 
DEF VAR lcHeaderTimeStamp AS CHAR NO-UNDO. 
DEF VAR ldaPmtDate AS DATE NO-UNDO. 
DEF VAR liPmtTime AS INT NO-UNDO. 
DEF VAR lcCompBIC AS CHAR NO-UNDO. 
DEF VAR llFrstCounter AS LOG NO-UNDO.
DEF VAR llRcurCounter AS LOG NO-UNDO. 
 

DEF TEMP-TABLE ttError NO-UNDO
   FIELD Inv    AS CHAR
   FIELD Cust   AS INT
   FIELD ErrMsg AS CHAR.

DEF TEMP-TABLE ttDDItem NO-UNDO
   FIELD SeqTp AS CHAR
   FIELD InstrId AS CHAR
   FIELD E2EId AS CHAR
   FIELD InvAmt AS DEC
   FIELD MandateId AS CHAR
   FIELD MandateDATE AS DATE
   FIELD MandateFlag AS LOG
   FIELD CustNm AS CHAR
   FIELD CustAdd AS CHAR
   FIELD CustZip AS CHAR
   FIELD CustIBAN AS CHAR
   FIELD CustBIC AS CHAR
   FIELD CustCountry AS CHAR
   FIELD Ustrd AS CHAR
   FIELD OldMandateId AS CHAR
   FIELD OldBankAcc AS CHAR
   FIELD BankStatus AS CHAR
   INDEX InstrId IS PRIMARY InstrId
   INDEX SeqTP SeqTp.

DEF TEMP-TABLE ttDDCreditor NO-UNDO
   FIELD PmtInfId AS CHAR
   FIELD SeqTp AS CHAR
   FIELD ReqdColltnDt AS DATE
   FIELD CompNm AS CHAR
   FIELD CompAdd AS CHAR
   FIELD CompIBAN AS CHAR
   FIELD CompBIC AS CHAR
   FIELD CreditorID AS CHAR.

DEF TEMP-TABLE ttBic NO-UNDO LIKE BankIdCode.

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

FUNCTION fDisplayDate RETURNS CHARACTER
   (idtDate    AS DATE):

   RETURN STRING(YEAR(idtDate),"9999") + "-" +
          STRING(MONTH(idtDate),"99") + "-" +
          STRING(DAY(idtDate),"99").
END FUNCTION.

FUNCTION fDispDecimal RETURNS CHAR
   (idAmount AS DEC):

   RETURN TRIM(REPLACE(STRING(idAmount,"->>>>>>>>>9.99"),",",".")).
END FUNCTION.

FUNCTION fCheckBankIdCode RETURNS CHARACTER
   (icBCode AS CHAR):
   FIND FIRST ttBic NO-LOCK WHERE
              ttBic.BankCode = icBCode NO-ERROR.
   IF NOT AVAIL ttBic THEN
      RETURN "NOTPROVIDED".
   ELSE RETURN ttBic.BIC.

END FUNCTION.

FUNCTION fCreatePmtInfId RETURNS CHARACTER
   (icPmtSet AS CHAR):

   DEF VAR lcStatus AS CHAR NO-UNDO.
   DEF VAR lcCurTime AS CHAR NO-UNDO. 

   IF icPmtSet = "001" THEN
     lcStatus = "FRST".
   ELSE
      lcStatus = "RCUR".

   lcCurTime = STRING(liPmtTime,"HH:MM:SS").
   lcPmtInfId = "COR1" + lcStatus + 
       STRING(YEAR(ldaPmtDate),"9999") +
       STRING(MONTH(ldaPmtDate),"99") +
       STRING(DAY(ldaPmtDate),"99") + "T" +
       ENTRY(1,lcCurTime,':') +
       ENTRY(2,lcCurTime,':') +
       ENTRY(3,lcCurTime,':') + icPmtSet.

   RETURN lcPmtInfId.

END FUNCTION.

/***** Main start *******/

CREATE WIDGET-POOL "PrintHouse".

RUN pInitialize.

IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.

IF iiInvCount > 0 THEN DO:

   RUN pInitXML.

   RUN pPrintInvoices. 
   
   RUN pCustHeader.  

   RUN pPrint2XML("FRST").

   RUN pPrint2XML("RCUR").

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
     RUN pLogErrors.
     RETURN RETURN-VALUE.
   END.  

END.   

RUN pFinalize.
IF RETURN-VALUE BEGINS "ERROR" THEN DO:
  RUN pLogErrors.
  RETURN RETURN-VALUE.
END.  

DELETE WIDGET-POOL "PrintHouse".


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
      IF LENGTH(BankAccount.BankAccount) < 24 THEN 
         RETURN "ERROR:Invalid bank account".
   END.

   IF icBankCode = '0049' THEN
      lcMsgId = 'PRE' + icBankCode.
   ELSE
      lcMsgId = icBankCode.
   
   ASSIGN 
      lcRefAmt    = " / " + STRING(iiInvCount)
      lcNewLine   = CHR(13) + CHR(10)
      lcNumeric   = SESSION:NUMERIC-FORMAT
      SESSION:NUMERIC-FORMAT = "american".
      /* yoigo cif */

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
                            ".xml").
 
   /* file without the path -> invoice */ 
   ASSIGN 
      lcPlainFile = icFile
      liPCnt      = R-INDEX(lcPlainFile,"/").
   IF liPCnt > 0 THEN lcPlainFile = SUBSTRING(lcPlainFile,liPCnt + 1).


   EMPTY TEMP-TABLE ttError. 

   FOR EACH BankIdCode NO-LOCK:
      CREATE ttBic.
      ASSIGN
         ttBic.BankCode = BankIdCode.BankCode
         ttBic.BankName = BankIdCode.BankName
         ttBic.BIC      = BankIdCode.BIC.
   END.

   RETURN "".
   
END PROCEDURE. /* pInitialize */


PROCEDURE pInitXML:

   /* check that file doesn't exist and form the complete name */
   
   IF VALID-HANDLE(lhXML) THEN DELETE OBJECT lhXML.

   /* Set Timestamps */
   ASSIGN
      ldaPmtDate = TODAY
      liPmtTime = TIME
      lcHeaderTimeStamp = fISOTimeZone(ldaPmtDate,liPmtTime).

   IF INT(SUBSTRING(BankAccount.BankAccount,5,4)) > 0 THEN
      lcCompBIC = BankAccount.BIC.
   ELSE
      lcCompBIC = "NOTPROVIDED".
   
   CREATE SAX-WRITER lhXML IN WIDGET-POOL "PrintHouse".
   lhXML:FORMATTED = llFormatted.
   lhXML:ENCODING = 'UTF-8'.
   /* lhXML:STANDALONE = 'no'. */
   lhXML:SET-OUTPUT-DESTINATION("FILE",icFile).                 

   lhXML:START-DOCUMENT().
   lhXML:START-ELEMENT("Document").
   lhXML:INSERT-ATTRIBUTE("xmlns","urn:iso:std:iso:20022:tech:xsd:pain.008.001.02").
   lhXML:INSERT-ATTRIBUTE("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance").
   lhXML:START-ELEMENT("CstmrDrctDbtInitn").

END PROCEDURE. /* pInitXML  */


PROCEDURE pCustHeader:

   /* Group Header */
    lhXML:START-ELEMENT("GrpHdr").
     lhXML:WRITE-DATA-ELEMENT("MsgId",lcMsgId). /* BankAccount.InvForm */ 
     lhXML:WRITE-DATA-ELEMENT("CreDtTm",lcHeaderTimeStamp).
     lhXML:WRITE-DATA-ELEMENT("NbOfTxs",STRING(oiInvCount)).
     lhXML:WRITE-DATA-ELEMENT("CtrlSum",fDispDecimal(ldInvTot)).
   
     /* Customer information  */
     lhXML:START-ELEMENT("InitgPty").
      lhXML:WRITE-DATA-ELEMENT("Nm",SUBSTRING(Company.CompName,1,70)).
      lhXML:START-ELEMENT("Id").
       lhXML:START-ELEMENT("OrgId").
        lhXML:START-ELEMENT("Othr").
         lhXML:WRITE-DATA-ELEMENT("Id",BankAccount.CreditorId).
        lhXML:END-ELEMENT("Othr").
       lhXML:END-ELEMENT("OrgId").
      lhXML:END-ELEMENT("Id").
     lhXML:END-ELEMENT("InitgPty").
    lhXML:END-ELEMENT("GrpHdr").

END PROCEDURE. /* pCustHeader */ 


PROCEDURE pPrintInvoices:

   DEF VAR liPos AS INT  NO-UNDO.

   PrintMainLoop:
   FOR EACH ttInvoice WHERE

            ttInvoice.DueDate  = idtDueDate AND
            ttInvoice.BankCode = icBankCode,
      FIRST Invoice OF ttInvoice NO-LOCK,
      FIRST Customer OF Invoice NO-LOCK:


      /* unexpected interrupt occurred */
      IF RETRY THEN DO:
   
         fErrLine("ERROR:Printing was abnormally interrupted").
         lhXML:WRITE-COMMENT("#" + lcLastError).

         LEAVE.
      END.
      /* invalid bank data */
      IF LENGTH(Invoice.DDBankAcc) < 24 THEN DO:
         fErrLine("Invalid bank data").
         NEXT PrintMainLoop.
      END.

      ASSIGN oiInvCount  = oiInvCount + 1
             ldInvTot    = ldInvTot + Invoice.InvAmt.

      IF NOT SESSION:BATCH AND oiInvCount MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISPLAY oiInvCount FORMAT ">>>>>>>>>9"
                 lcRefAmt   FORMAT "x(12)"
         WITH NO-LABELS OVERLAY ROW 10 CENTERED
              TITLE " Printing " FRAME fPrint.
      END.

      IF iiUpdInterval > 0 AND oiInvCount MOD iiUpdInterval = 0 
      THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiInvCount) THEN
            RETURN "ERROR:Stopped".
      END.   
      
      RUN pCollectData2XML.

      /* mark that invoice was handled */    
      ttInvoice.Printed = 1.
   
   END.     /* PrintMainLoop */
   

   HIDE FRAME fPrint NO-PAUSE.
   
   RETURN "". 

END PROCEDURE. /* pPrintInvoices */

PROCEDURE pCollectData2XML:

   DEF VAR lcUstrd AS CHAR NO-UNDO. 
   DEF VAR ldaMandateDate AS DATE NO-UNDO. 
   DEF VAR lcMandateDate AS CHAR NO-UNDO. 
   DEF VAR llMandateFlag AS LOG NO-UNDO. 
   DEF VAR lcCustBIC AS CHAR NO-UNDO.
   DEF VAR lcSeqTp AS CHAR NO-UNDO. 
   DEF VAR lcInstrId AS CHAR NO-UNDO. 
   DEF VAR liBankChngStatus AS INT NO-UNDO.

   DEF BUFFER bInvoice FOR Invoice.
   DEF BUFFER bInvoiceDD FOR Invoice.
   DEF BUFFER bInvoiceDD2 FOR Invoice.
   DEF BUFFER bInvoiceBA FOR Invoice.

   /* YOIGO - FACTURA 131D12345678 PERIODO: 01/01/2013 AL 01/01/2013 */
   ASSIGN 
      lcUstrd = "YOIGO - FACTURA " + Invoice.ExtInvId + 
                " PERIODO: " + STRING(Invoice.FromDate,"99/99/99") + 
                " AL " + STRING(Invoice.ToDate,"99/99/99").

   IF LENGTH(Invoice.MandateId) = 30 THEN DO:
      ASSIGN
         lcMandateDate = SUBSTRING(Invoice.MandateID,25,6)
         ldaMandateDate = DATE(INT(SUBSTRING(lcMandateDate,3,2)),
                               INT(SUBSTRING(lcMandateDate,5,2)),
                               INT(SUBSTRING(lcMandateDate,1,2)) + 2000) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN ldaMandateDate = 10/31/2009.
   END.
   ELSE DO:
      ASSIGN
         lcMandateDate = SUBSTRING(Invoice.MandateID,25,8)
         ldaMandateDate = DATE(INT(SUBSTRING(lcMandateDate,5,2)),
                               INT(SUBSTRING(lcMandateDate,7,2)),
                               INT(SUBSTRING(lcMandateDate,1,4))) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN ldaMandateDate = 10/31/2009.
   END.

   IF lcMandateDate EQ "000000" THEN 
      ldaMandateDate = 10/31/2009.
   
   /* Find last invoice to check is bank account same */
   FIND FIRST bInvoiceBA NO-LOCK WHERE
              bInvoiceBA.Brand = gcBrand AND
              bInvoiceBA.CustNum = Invoice.CustNum AND
              bInvoiceBA.InvDate >= 2/1/2014 AND
              bInvoiceBA.InvDate < Invoice.InvDate AND
              bInvoiceBA.DDState >= 1 AND
              bInvoiceBA.InvType = 1 AND
        ROWID(bInvoiceBA) NE ROWID(Invoice) USE-INDEX Custnum NO-ERROR.

   /* liBankChngStatus:
      0: No BankAcc change
      1: BankAcc change and Bank change
      2: BankAcc change */
   IF AVAIL bInvoiceBA AND
            bInvoiceBA.DDBankAcc NE Invoice.DDBankAcc THEN DO:
      IF SUBSTRING(bInvoiceBA.DDBankAcc,5,4) NE
         SUBSTRING(Invoice.DDBankAcc,5,4) THEN
         liBankChngStatus = 1.
      ELSE
         liBankChngStatus = 2.
   END.
   ELSE
      liBankChngStatus = 0.

   FIND FIRST bInvoice NO-LOCK WHERE
              bInvoice.Brand = gcBrand AND
              bInvoice.CustNum = Invoice.CustNum AND
              bInvoice.InvDate >= 2/1/2014 AND
              bInvoice.ITGroupID = Invoice.ITGroupID AND
              bInvoice.DDState >= 1 AND
              binvoice.InvType = 1 AND
        ROWID(bInvoice) NE ROWID(Invoice) USE-INDEX Custnum NO-ERROR.

   IF NOT AVAIL bInvoice OR
      bInvoice.MandateId EQ Invoice.MandateId 
   THEN llMandateFlag = FALSE.
   ELSE llMandateFlag = TRUE.


   lcCustBIC = fCheckBankIdCode(SUBSTRING(Customer.BankAcct,5,4)).

   /* bInvoiceDD and bInvoiceDD2 added inside same block */
   IF NOT AVAIL bInvoice OR bInvoice.MandateId NE Invoice.MandateId THEN DO:
      /* Check if previous invoice has same mandate -> RCUR */
      FIND FIRST bInvoiceDD NO-LOCK WHERE
                 bInvoiceDD.Brand = gcBrand AND
                 bInvoiceDD.CustNum = Invoice.CustNum AND
                 bInvoiceDD.InvDate >= 2/1/2014 AND
                 bInvoiceDD.InvDate < Invoice.InvDate AND
                 bInvoiceDD.DDState >= 1 AND
                 binvoiceDD.InvType = 1 AND
                 bInvoiceDD.MandateId EQ Invoice.MandateId AND
           ROWID(bInvoiceDD) NE ROWID(Invoice) USE-INDEX Custnum NO-ERROR.

      /* Check if customer has two invoices with same date -> RCUR */
      IF NOT AVAIL bInvoiceDD THEN 
         FIND FIRST bInvoiceDD2 NO-LOCK WHERE
                    bInvoiceDD2.Brand = gcBrand AND
                    bInvoiceDD2.CustNum = Invoice.CustNum AND
                    bInvoiceDD2.InvDate = Invoice.InvDate AND
                    bInvoiceDD2.MandateId EQ Invoice.MandateId AND
                    binvoiceDD2.InvType = 1 AND
                    bInvoiceDD2.InvAmt > 0 AND
              ROWID(bInvoiceDD2) NE ROWID(Invoice) USE-INDEX Custnum NO-ERROR.
   END. /* bInvoiceDD and bInvoiceDD2 added inside same block */
   
   /* IF Mandate differ with an "X" from previous, it's still RCUR
      This is releated into YDR-1553 */
   /* If Bank changed since last billing, Direct Debit go to FRST */
   IF ((NOT AVAIL bInvoice OR 
       bInvoice.MandateId NE Invoice.MandateId) AND
       NOT Invoice.MandateId BEGINS STRING(Invoice.Custnum) + "X" AND
       NOT AVAIL bInvoiceDD2 AND NOT AVAIL bInvoiceDD) OR
       liBankChngStatus = 1 THEN
      ASSIGN
         liFrstCounter = liFrstCounter + 1
         lcPmtInfId = fCreatePmtInfId("001")
         lcSeqTp = "FRST"
         lcInstrId = lcPmtInfId + STRING(liFrstCounter,"99999999").
   ELSE 
      ASSIGN
         liRcurCounter = liRcurCounter + 1
         lcPmtInfId = fCreatePmtInfId("002")
         lcSeqTp = "RCUR"
         lcInstrId = lcPmtInfId + STRING(liRcurCounter,"99999999").

   IF (liFrstCounter = 1 AND NOT llFrstCounter) OR 
      (liRcurCounter = 1 AND NOT llRcurCounter) THEN DO:
      CREATE ttDDCreditor.
      ASSIGN
         ttDDCreditor.PmtInfId = lcPmtInfId
         ttDDCreditor.SeqTp = lcSeqTp
         ttDDCreditor.ReqdColltnDt = Invoice.DueDate
         ttDDCreditor.CompNm = SUBSTRING("YOIGO",1,70)
         ttDDCreditor.CompAdd = SUBSTRING(Company.Address,1,70)
         ttDDCreditor.CompIBAN = BankAccount.BankAccount
         ttDDCreditor.CompBIC = lcCompBIC
         ttDDCreditor.CreditorID = BankAccount.CreditorID.
      IF liFrstCounter = 1 THEN llFrstCounter = TRUE.
      IF liRcurCounter = 1 THEN llRcurCounter = TRUE.
   END.

   CREATE ttDDItem.
   ASSIGN
      ttDDItem.SeqTp = lcSeqTp
      ttDDItem.InstrId = lcInstrId
      ttDDItem.E2EId = Invoice.ExtInvId
      ttDDItem.InvAmt = Invoice.InvAmt
      ttDDItem.MandateId = Invoice.MandateID
      ttDDItem.MandateDATE = ldaMandateDate
      ttDDItem.MandateFlag = llMandateFlag
      ttDDItem.CustNm = SUBSTRING(CAPS(fCheckSEPASpecialChar(DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1, BUFFER Customer))),1,70)
      ttDDItem.CustAdd = SUBSTRING(CAPS(fCheckSEPASpecialChar(Customer.Address)),1,70)
      ttDDItem.CustZip = SUBSTRING(Customer.ZipCode + " " + CAPS(fCheckSEPASpecialChar(Customer.PostOffice)),1,70)
      ttDDItem.CustIBAN = Invoice.DDBankAcc
      ttDDitem.CustBIC = lcCustBIC
      ttDDItem.CustCountry = CAPS(Customer.Country)
      ttDDItem.Ustrd = lcUstrd.
   IF llMandateFlag THEN
      ttDDItem.OldMandateId = bInvoice.MandateId.
   /* Check bank and/or bank account changes */
   IF liBankChngStatus = 1 THEN
      ttDDItem.BankStatus = "SMNDA".
   IF liBankChngStatus = 2 THEN
      ttDDItem.OldBankAcc = bInvoiceBA.DDBankAcc.
END PROCEDURE. /* pCollectData2XML */


PROCEDURE pPrint2XML:

   DEF INPUT PARAMETER icSeqTP AS CHAR NO-UNDO.

   FOR EACH ttDDItem NO-LOCK WHERE
            ttDDItem.SeqTP = icSeqTp BREAK BY ttDDItem.InstrId:

      IF FIRST(ttDDItem.InstrId) THEN DO:
         FIND FIRST ttDDCreditor NO-LOCK WHERE
                    ttDDCreditor.SeqTP = icSeqTp NO-ERROR.
         IF AVAIL ttDDCreditor THEN DO:
            lhXML:START-ELEMENT("PmtInf").
              lhXML:WRITE-DATA-ELEMENT("PmtInfId",ttDDCreditor.PmtInfId).
              lhXML:WRITE-DATA-ELEMENT("PmtMtd","DD").
              lhXML:START-ELEMENT("PmtTpInf").
                lhXML:START-ELEMENT("SvcLvl").
                  lhXML:WRITE-DATA-ELEMENT("Cd","SEPA").
                lhXML:END-ELEMENT("SvcLvl").
                lhXML:START-ELEMENT("LclInstrm").
                  lhXML:WRITE-DATA-ELEMENT("Cd","COR1").
                LHxML:END-ELEMENT("LclInstrm").
                lhXML:WRITE-DATA-ELEMENT("SeqTp",ttDDCreditor.SeqTp).
              lhXML:END-ELEMENT("PmtTpInf").
              lhXML:WRITE-DATA-ELEMENT("ReqdColltnDt",fDisplayDate(ttDDCreditor.ReqdColltnDt)).
              lhXML:START-ELEMENT("Cdtr").
                lhXML:WRITE-DATA-ELEMENT("Nm",ttDDCreditor.CompNm).
                lhXML:START-ELEMENT("PstlAdr").
                  lhXML:WRITE-DATA-ELEMENT("Ctry","ES").
                  lhXML:WRITE-DATA-ELEMENT("AdrLine",ttDDCreditor.CompAdd).
                lhXML:END-ELEMENT("PstlAdr").
              lhXML:END-ELEMENT("Cdtr").
              lhXML:START-ELEMENT("CdtrAcct").
                lhXML:START-ELEMENT("Id").
                  lhXML:WRITE-DATA-ELEMENT("IBAN",ttDDCreditor.CompIBAN).
                lhXML:END-ELEMENT("Id").
              lhXML:END-ELEMENT("CdtrAcct").
              lhXML:START-ELEMENT("CdtrAgt").
                lhXML:START-ELEMENT("FinInstnId").  
                  lhXML:WRITE-DATA-ELEMENT("BIC",ttDDCreditor.CompBIC).
                lhXML:END-ELEMENT("FinInstnId").
              lhXML:END-ELEMENT("CdtrAgt").
              lhXML:START-ELEMENT("CdtrSchmeId").
                lhXML:START-ELEMENT("Id").
                  lhXML:START-ELEMENT("PrvtId").
                    lhXML:START-ELEMENT("Othr").
                      lhXML:WRITE-DATA-ELEMENT("Id",ttDDCreditor.CreditorID).
                      lhXML:START-ELEMENT("SchmeNm").
                        lhXML:WRITE-DATA-ELEMENT("Prtry","SEPA").
                      lhXML:END-ELEMENT("SchmeNm").
                    lhXML:END-ELEMENT("Othr").
                  lhXML:END-ELEMENT("PrvtId").
                lhXML:END-ELEMENT("Id").
              lhXML:END-ELEMENT("CdtrSchmeId").
         END. /* IF AVAIL ttDDCreditor THEN DO: */
      END. /* IF FIRST(ttDDItem.InstrId) THEN DO: */

      /* Customer information */
        lhXML:START-ELEMENT("DrctDbtTxInf").
          lhXML:START-ELEMENT("PmtId").
            lhXML:WRITE-DATA-ELEMENT("InstrId",ttDDItem.InstrId).
            lhXML:WRITE-DATA-ELEMENT("EndToEndId",ttDDItem.E2EId).
          lhXML:END-ELEMENT("PmtId").
          lhXML:START-ELEMENT("InstdAmt").
          lhXML:INSERT-ATTRIBUTE("Ccy","EUR").
          lhXML:WRITE-CHARACTERS(fDispDecimal(ttDDItem.InvAmt)).
          lhXML:END-ELEMENT("InstdAmt").
          lhXML:START-ELEMENT("DrctDbtTx").
            lhXML:START-ELEMENT("MndtRltdInf").
              lhXML:WRITE-DATA-ELEMENT("MndtId",ttDDItem.MandateId).
              lhXML:WRITE-DATA-ELEMENT("DtOfSgntr",fDisplayDate(ttDDItem.MandateDate)).
              IF ttDDItem.MandateFlag EQ TRUE OR
                 ttDDItem.BankStatus > "" OR
                 ttDDItem.OldBankAcc > "" THEN DO:
                lhXML:WRITE-DATA-ELEMENT("AmdmntInd","true").
                lhXML:START-ELEMENT("AmdmntInfDtls").
                IF ttDDItem.MandateFlag EQ TRUE THEN
                  lhXML:WRITE-DATA-ELEMENT("OrgnlMndtId",ttDDItem.OldMandateId).
                IF ttDDItem.OldBankAcc > "" THEN DO:
                  lhXML:START-ELEMENT("OrgnlDbtrAcct").
                    lhXML:START-ELEMENT("Id").
                      lhXML:WRITE-DATA-ELEMENT("IBAN",ttDDItem.OldBankAcc).
                    lhXML:END-ELEMENT("Id").
                  lhXML:END-ELEMENT("OrgnlDbtrAcct").
                END.
                IF ttDDItem.BankStatus > "" THEN DO:
                  lhXML:START-ELEMENT("OrgnlDbtrAgt").
                     lhXML:START-ELEMENT("FinInstnId").
                        lhXML:START-ELEMENT("Othr").
                           lhXML:WRITE-DATA-ELEMENT("Id",ttDDItem.BankStatus).
                        lhXML:END-ELEMENT("Othr").
                     lhXML:END-ELEMENT("FinInstnId").
                  lhXML:END-ELEMENT("OrgnlDbtrAgt").
                END.
                lhXML:END-ELEMENT("AmdmntInfDtls").
              END.
              lhXML:END-ELEMENT("MndtRltdInf").
          lhXML:END-ELEMENT("DrctDbtTx").
          lhXML:START-ELEMENT("DbtrAgt").
            lhXML:START-ELEMENT("FinInstnId").
            lhXML:WRITE-DATA-ELEMENT("BIC",ttDDitem.CustBIC).
            lhXML:END-ELEMENT("FinInstnId").
          lhXML:END-ELEMENT("DbtrAgt").
          lhXML:START-ELEMENT("Dbtr").
            lhXML:WRITE-DATA-ELEMENT("Nm",ttDDItem.CustNm).
            lhXML:START-ELEMENT("PstlAdr").
              lhXML:WRITE-DATA-ELEMENT("Ctry",ttDDItem.CustCountry).
              lhXML:WRITE-DATA-ELEMENT("AdrLine",ttDDItem.CustAdd).
              lhXML:WRITE-DATA-ELEMENT("AdrLine",ttDDItem.CustZip).
            lhXML:END-ELEMENT("PstlAdr").
          lhXML:END-ELEMENT("Dbtr").
          lhXML:START-ELEMENT("DbtrAcct").
            lhXML:START-ELEMENT("Id").
              lhXML:WRITE-DATA-ELEMENT("IBAN",ttDDItem.CustIBAN).
            lhXML:END-ELEMENT("Id").
          lhXML:END-ELEMENT("DbtrAcct").
          lhXML:START-ELEMENT("RmtInf").
            lhXML:WRITE-DATA-ELEMENT("Ustrd",ttDDItem.Ustrd). 
          lhXML:END-ELEMENT("RmtInf").
        lhXML:END-ELEMENT("DrctDbtTxInf").

      IF LAST(ttDDItem.InstrId) THEN
         lhXML:END-ELEMENT("PmtInf").
   END. /*  FOR EACH ttDDItem NO-LOCK: */

END PROCEDURE. /* pPrint2XML */

PROCEDURE pFinalize:

   DEF VAR lcFinalFile AS CHAR NO-UNDO.
   
   lhXML:END-ELEMENT("CstmrDrctDbtInitn").
   lhXML:END-ELEMENT("Document").
   lhXML:END-DOCUMENT().
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

   EMPTY TEMP-TABLE ttBic NO-ERROR.
   
   IF VALID-HANDLE(lhXML) THEN DELETE OBJECT lhXML.
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
                ErrorLog.ActionID  = "DDFILETEST_XML"
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

