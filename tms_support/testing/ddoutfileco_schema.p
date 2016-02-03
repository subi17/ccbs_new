/* ----------------------------------------------------------------------
  MODULE .......: ddoutfileco.p
  TASK .........: Collect invoices to a direct debit file 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.01.07
  CHANGED ......: 19.04.07/aam icBillRun, check ChargeType
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}

DEFINE INPUT  PARAMETER icInvGrp       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum1     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum2     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icInvID1       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER icInvID2       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiInvDate      AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER iiInvType      AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icBillRun      AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiPrintState1  AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiPrintState2  AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icFile         AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ilEmptyFile    AS LOG  NO-UNDO. 
DEFINE INPUT  PARAMETER ilSplit        AS LOG  NO-UNDO.
DEFINE INPUT  PARAMETER iiFRProcessID  AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiUpdInterval  AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icRunMode      AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER icCSBFileForm  AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ilCSBValidate  AS LOG  NO-UNDO.
DEFINE OUTPUT PARAMETER oiInvCount     AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER oiFileCount    AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER ocError        AS CHAR NO-UNDO. 

DEF VAR ldtNameDate AS DATE NO-UNDO. 
DEF VAR lcDate      AS CHAR NO-UNDO.
DEF VAR lcFile      AS CHAR NO-UNDO. 
DEF VAR liInvCount  AS INT  NO-UNDO.
DEF VAR liPicked    AS INT  NO-UNDO. 
DEF VAR liBankQty   AS INT  NO-UNDO EXTENT 4.
DEF VAR ldBankAmt   AS DEC  NO-UNDO EXTENT 4.
DEF VAR liFileSeq   AS INT  NO-UNDO.
DEF VAR lcTestDir   AS CHAR NO-UNDO.
DEF VAR lcFileTxt   AS CHAR NO-UNDO. 
DEF VAR lcFileXml   AS CHAR NO-UNDO. 
DEF VAR liNBCCount  AS INT  NO-UNDO. 

{Inv/ddoutfilett.i}

DEF TEMP-TABLE ttDueDate NO-UNDO
   FIELD DueDate  AS DATE
   FIELD BankCode AS CHAR
   INDEX DueDate DueDate BankCode.

DEF BUFFER bttInvoice FOR ttInvoice.

FUNCTION fMakeTemp RETURNS LOGICAL.

    /* not numbered */
    IF Invoice.ExtInvID = "" THEN RETURN FALSE.
    
    /* already sent */
    IF Invoice.DDState > 0 THEN RETURN FALSE.
    
    /* charge type is not direct debit */
    IF Invoice.ChargeType NE 2 THEN RETURN FALSE. 
             
    /* no negative invoices */
    IF Invoice.InvAmt <= 0 THEN RETURN FALSE. 
    
    /* no partly paid invoices */
    IF Invoice.PaidAmt NE 0 THEN RETURN FALSE. 
    
    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN RETURN FALSE.
    
    IF iiInvType > 0 AND Invoice.InvType NE iiInvType 
    THEN RETURN FALSE.

    IF Invoice.PrintState < iiPrintState1 OR 
       Invoice.PrintState > iiPrintState2 THEN RETURN FALSE. 

    IF icBillRun > "" AND NOT Invoice.BillRun BEGINS icBillRun 
    THEN RETURN FALSE. 
    
    CREATE ttInvoice.
    ASSIGN ttInvoice.InvNum   = Invoice.InvNum
           ttInvoice.InvAmt   = Invoice.InvAmt 
           ttInvoice.DueDate  = Invoice.DueDate
           ttInvoice.Movable  = FALSE
           liPicked           = liPicked + 1. 

    IF ilSplit THEN DO:
       ttInvoice.BankCode = SUBSTRING(Customer.BankAcc,5,4).
   
       CASE ttInvoice.BankCode:
          WHEN "0049" OR
          WHEN "0030" THEN
             ttInvoice.BankCode = "0049". 
          WHEN "0182" OR
          WHEN "2040" OR
          WHEN "2074" OR
          WHEN "2059" OR
          WHEN "2107" THEN 
             ttInvoice.BankCode = "0182".
          WHEN "0081" THEN .
          /* Rest invoices will be divided based on the algorithm */
          OTHERWISE ASSIGN
             ttInvoice.BankCode = ""
             ttInvoice.Movable  = TRUE
             liNBCCount         = liNBCCount + 1.
       END CASE.
       
    END.
    ELSE ttInvoice.BankCode = "ALL".

    IF Customer.IDelName > ""
    THEN ttInvoice.ZipCode = Customer.IDelZipCode.
    ELSE ttInvoice.ZipCode = Customer.ZipCode.

    IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.

    RETURN TRUE.
    
END FUNCTION.

IF icFile = "" THEN DO:
   icFile = fCParamC("DDebitFileName").
   IF icFile = "" THEN RETURN "ERROR:File not defined".
END.

IF icInvID1 = icInvID2 THEN 
FOR FIRST Invoice NO-LOCK WHERE
          Invoice.Brand    = gcBrand AND
          Invoice.ExtInvID = icInvID1,
    FIRST Customer OF Invoice NO-LOCK:
   
   icInvGrp = Customer.InvGroup.
   
   fMakeTemp().

END.

ELSE IF iiInvDate NE ? THEN 
FOR EACH Invoice NO-LOCK WHERE    
         Invoice.Brand    = gcBrand    AND
         Invoice.InvDate  = iiInvDate  AND
         Invoice.ExtInvID >= icInvID1  AND      
         Invoice.ExtInvID <= icInvID2  AND   
         Invoice.CustNum >= iiCustNum1 AND
         Invoice.CustNum <= iiCustNum2,
   FIRST Customer OF Invoice NO-LOCK WHERE 
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().
        
   IF NOT SESSION:BATCH AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liPicked FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl.
   END.
   
END. 

ELSE 
FOR EACH Invoice NO-LOCK WHERE               
         Invoice.Brand  = gcBrand      AND
         Invoice.ExtInvID >= icInvID1  AND      
         Invoice.ExtInvID <= icInvID2  AND   
         Invoice.CustNum >= iiCustNum1 AND
         Invoice.CustNum <= iiCustNum2,
   FIRST Customer OF Invoice NO-LOCK WHERE
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().

   IF NOT SESSION:BATCH AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liPicked FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl2.
   END.
   
END. 

/* Call pSplitOtherInvoices to split other bank code invoices */
/* based on the algorithm specified in the YOT-1450           */
RUN pSplitOtherInvoices.

FOR EACH ttInvoice:
   IF NOT CAN-FIND(FIRST ttDueDate WHERE 
                         ttDueDate.DueDate  = ttInvoice.DueDate AND
                         ttDueDate.BankCode = ttInvoice.BankCode)
   THEN DO:
      CREATE ttDueDate.
      ASSIGN 
         ttDueDate.DueDate  = ttInvoice.DueDate
         ttDueDate.BankCode = ttInvoice.BankCode.
   END.
END.

/* should an empty file be made */
IF NOT CAN-FIND(FIRST ttDueDate) AND ilEmptyFile THEN DO:
   CREATE ttDueDate.
   ttDueDate.DueDate = TODAY.
END.

IF icRunMode = "test" THEN 
   lcTestDir = fCParamC("FRTestRunDir").
   
/* print, one file per due date / bankcode  */
FOR EACH ttDueDate:

   /* Define CSB file name for each process */
   IF icCSBFileForm EQ "TXT" OR
      icCSBFileForm EQ "BOTH" THEN
      ASSIGN
         lcFileTxt = fCParamC("DDebitFileNameOLD")
         lcFileXml = fCParamC("DDebitFileName"). 

   IF icCSBFileForm EQ "XML" THEN
         lcFileXml = fCParamC("DDebitFileName"). 

   /* invgroup to file name */
   IF icInvGrp > "" 
   THEN lcFile = REPLACE(icFile,"#IGRP",icInvGrp).
   ELSE lcFile = REPLACE(icFile,"#IGRP","ALL").
   
   /* due date to file name */   
   lcDate = DYNAMIC-FUNCTION("fDateFmt" IN ghFunc1,
                             ttDueDate.DueDate,
                             "yyyymmdd").
   ASSIGN 
      lcFileXml = REPLACE(lcFileXml,"#DDATE",lcDate)
      lcFileXml = REPLACE(lcFileXml,"#BANK",ttDueDate.BankCode)
      lcFileTxt = REPLACE(lcFileTxt,"#DDATE",lcDate)
      lcFileTxt = REPLACE(lcFileTxt,"#BANK",ttDueDate.BankCode)
      liFileSeq = liFileSeq + 1.
      
   /* use own directory for test run */
   IF icRunMode = "test" AND lcTestDir > "" THEN 
      lcFile = lcTestDir + "*" + lcFile.

   
   IF icCSBFileForm EQ "XML" OR
      icCSBFileForm EQ "BOTH" THEN 
      RUN ddoutfile_xml.p (INPUT-OUTPUT TABLE ttInvoice,  
                    ttDueDate.DueDate,
                    ttDueDate.BankCode,
                    liPicked,
                    lcFileXml,
                    ilEmptyFile,
                    iiFRProcessID,
                    iiUpdInterval,
                    liFileSeq,
                    ilCSBValidate,
                    OUTPUT liInvCount). 

   IF icCSBFileForm EQ "TXT" OR
      icCSBFileForm EQ "BOTH" THEN 
      RUN ddoutfile.p (INPUT-OUTPUT TABLE ttInvoice,  
                    ttDueDate.DueDate,
                    ttDueDate.BankCode,
                    liPicked,
                    lcFileTxt,
                    ilEmptyFile,
                    iiFRProcessID,
                    iiUpdInterval,
                    liFileSeq,
                    OUTPUT liInvCount). 

   ASSIGN 
      ocError     = ocError + (IF ocError > "" THEN ", " ELSE "") + 
                    RETURN-VALUE
      oiFileCount = oiFileCount + INTEGER(liInvCount > 0)
      oiInvCount  = oiInvCount + liInvCount.
END.

DO TRANS:

   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "Invoice"  
      ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                               STRING(MONTH(TODAY),"99") + 
                               STRING(DAY(TODAY),"99")
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "DDFILES"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionChar   = " Files: " + STRING(oiFileCount) + CHR(10) +
                               " Invoices: " + STRING(oiInvCount)
      ActionLog.ActionStatus = 3.
      ActionLog.ActionTS     = fMakeTS().
END.

RETURN ocError.

PROCEDURE pSplitOtherInvoices:

   DEFINE VARIABLE liCount    AS INTEGER NO-UNDO.
   DEFINE VARIABLE liModValue AS INTEGER NO-UNDO.
   DEFINE VARIABLE liTwPValue AS INTEGER NO-UNDO. /* Twenty Percentage Value */
   DEFINE VARIABLE liSBCValue AS INTEGER NO-UNDO. /* Sabadell bank Invoice Count Value */ 
   
   /* Two percentage value of the Invoices which does not have BankCode */
   ASSIGN liTwPValue = TRUNCATE(((20 / 100) * liNBCCount),0). 
 
   /* Fetch all other invoices by amount from higher to lower */
   /* and divide based on ticket  ydr-1601                    */
   FOR EACH ttInvoice EXCLUSIVE-LOCK  WHERE 
            ttInvoice.Movable  = TRUE AND
            ttInvoice.BankCode = ""
         BY ttInvoice.InvAmt DESC:
   
      IF liSBCValue < liTwPValue THEN 
         ASSIGN liCount    = liCount + 1
                liModValue = liCount MOD 3.
      ELSE ASSIGN liCount    = liCount + 1
                  liModValue = (liCount MOD 2) + 1.
   
      CASE liModValue:
         WHEN 1 THEN ASSIGN ttInvoice.BankCode = "0182".
         WHEN 2 THEN ASSIGN ttInvoice.BankCode = "0049".
         WHEN 0 THEN ASSIGN ttInvoice.BankCode = "0081"
                            liSBCValue         = liSBCValue + 1.
      END CASE. 
   
   END. /* FOR EACH ttInvoice WHERE */

END PROCEDURE. /* PROCEDURE pSplitOtherInvoices: */

