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
{Syst/tmsconst.i}
{Func/multitenantfunc.i}

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
DEFINE INPUT  PARAMETER ilCSBValidate  AS LOG  NO-UNDO.
DEFINE INPUT  PARAMETER icInputFileDir AS CHAR NO-UNDO.
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
DEF VAR lcFileXml   AS CHAR NO-UNDO. 
DEF VAR liNBCCount  AS INT  NO-UNDO. 
DEF VAR liSBICount  AS INT  NO-UNDO INITIAL 0.
DEF VAR liSAICount  AS INT  NO-UNDO INITIAL 0.
DEF VAR liBBICount  AS INT  NO-UNDO INITIAL 0.
DEF VAR liLAICount  AS INT  NO-UNDO INITIAL 0.
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcLine      AS CHAR NO-UNDO. 
DEF VAR ldaDueDate  AS DATE NO-UNDO. 
DEF VAR lcBankCode  AS CHAR NO-UNDO. 
DEF VAR liFiles     AS INT  NO-UNDO. 

DEFINE VARIABLE liSBIPerc  AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE liSAIPerc  AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE liBBIPerc  AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE liLAIPerc  AS DECIMAL NO-UNDO INITIAL 0.
DEFINE VARIABLE liSBIValue AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liSAIValue AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liBBIValue AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liLAIValue AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liSBIolval AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liSAIolval AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liBBIolval AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE liLAIolval AS INTEGER NO-UNDO INITIAL 0.

{Inv/ddoutfilett.i}

DEF TEMP-TABLE ttDueDate NO-UNDO
   FIELD DueDate  AS DATE
   FIELD BankCode AS CHAR
   INDEX DueDate DueDate BankCode.

DEF BUFFER bttInvoice FOR ttInvoice.

FUNCTION fParseDDFileName RETURNS LOGICAL (
   icFileName AS CHAR,
   OUTPUT ocBankCode AS CHAR,
   OUTPUT odaDueDate AS DATE):

   DEF VAR lcDueDate AS CHAR NO-UNDO. 
   DEF VAR ldeDueDate AS DEC NO-UNDO. 
   DEF VAR lcTempName AS CHAR NO-UNDO.

   lcTempname = REPLACE("#TENANT_SDD001_","#TENANT",
                        CAPS(fgetBrandNamebyTenantId(TENANT-ID("common")))).
   IF NUM-ENTRIES(icFileName, "_") < 4 THEN RETURN FALSE.
   IF NOT icFileName BEGINS lcTempname THEN RETURN FALSE.

   ASSIGN
      ocBankCode = ENTRY(3,icFileName,"_")
      ldeDueDate = INT(ENTRY(1,ENTRY(4,icFileName,"_"),".")) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN FALSE.

   IF NOT fTs2Date(ldeDueDate, OUTPUT odaDueDate) THEN RETURN FALSE. 

   RETURN TRUE.
END.

FUNCTION fMakeTemp RETURNS LOGICAL (
    icBankCode AS CHAR).

    /* not numbered */
    IF Invoice.ExtInvID = "" THEN RETURN FALSE.
    
    /* already sent */
    IF Invoice.DDState > 0 AND icInputFileDir EQ "" THEN RETURN FALSE.
    
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
           liPicked           = liPicked + 1. 

    IF icBankCode > "" THEN ttInvoice.BankCode = icBankCode.
    ELSE IF ilSplit THEN DO:
         ttInvoice.BankCode = SUBSTRING(Customer.BankAcc,5,4).
   
       /* Check bank code available in BankAccount data, AND assign
           parent bank code value to Invoice bank code field */
       FIND FIRST BankAccount NO-LOCK WHERE
                  BankAccount.Brand EQ gcBrand AND
           LOOKUP(ttInvoice.BankCode,BankAccount.BankCodes) > 0 NO-ERROR.

       IF AVAIL BankAccount THEN DO:
          ttInvoice.BankCode = LEFT-TRIM(BankAccount.InvForm,"DD").

          CASE ttInvoice.BankCode:
             WHEN {&TF_BANK_UNOE}     THEN liSAICount = liSAICount + 1.
             WHEN {&TF_BANK_BBVA}     THEN liBBICount = liBBICount + 1.
             WHEN {&TF_BANK_SABADELL} THEN liSBICount = liSBICount + 1.
             WHEN {&TF_BANK_LACAXIA}  THEN liLAICount = liLAICount + 1.
          END CASE.
       END.
       ELSE
          ASSIGN
            ttInvoice.BankCode = ""
            liNBCCount         = liNBCCount + 1.
 
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
   
IF icInputFileDir > "" THEN DO:

   FILE-INFO:FILE-NAME = icInputFileDir.
   IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN
      RETURN SUBST("ERROR:Directory not found: &1", icInputFileDir).

   DEF STREAM sFile.
   DEF STREAM sin.

   /* File reading and parsing */
   INPUT STREAM sFile THROUGH VALUE("ls -1tr " + icInputFileDir).
   REPEAT:
      IMPORT STREAM sFile UNFORMATTED lcFileName.

      liFiles = liFiles + 1.

      IF fParseDDFileName(lcFileName,
                          OUTPUT lcBankCode,
                          OUTPUT ldaDueDate) EQ FALSE THEN
         RETURN SUBST("ERROR:Incorrect input file name: &1", lcFileName).
 
      lcInputFile = icInputFileDir + "/" + lcFileName.
      
      IF SEARCH(lcInputFile) NE ? THEN 
         INPUT STREAM sin FROM VALUE(lcInputFile).
      ELSE RETURN SUBST("ERROR:File not found:&1", lcInputFile).
     
      FOR FIRST BankAccount NO-LOCK WHERE
                BankAccount.Brand   = gcBrand AND
                LOOKUP("DD" + lcBankCode,BankAccount.InvForm) > 0:
         IF LENGTH(BankAccount.BankAccount) < 24 THEN
            RETURN "ERROR:Invalid bank account".
      END.
      IF NOT AVAIL BankAccount THEN
         RETURN SUBST("ERROR:Invalid bank code1: &1", lcBankCode).
      
      IF CAN-FIND(FIRST ttDueDate WHERE 
                        ttDueDate.DueDate  = ldaDueDate AND
                        ttDueDate.BankCode = lcBankCode) THEN
         RETURN "ERROR:Two input files with the same bank code and due date".
     
      CREATE ttDueDate.
      ASSIGN 
         ttDueDate.DueDate  = ldaDueDate
         ttDueDate.BankCode = lcBankCode.
      
      REPEAT TRANS:
                     
         IMPORT STREAM sin UNFORMATTED lcLine.

         FOR FIRST Invoice EXCLUSIVE-LOCK WHERE
                   Invoice.Brand = gcBrand AND
                   Invoice.ExtInvId = TRIM(lcLine),
            FIRST Customer OF Invoice NO-LOCK:
            
            IF Invoice.DueDate NE ldaDueDate THEN RETURN
               SUBST("ERROR:Invoice &1 due date &2 does not match with input file &3",
                  Invoice.ExtInvId, Invoice.DueDate, lcInputFile).

            IF CAN-FIND(FIRST ttInvoice WHERE
                              ttInvoice.InvNum = Invoice.InvNum) THEN RETURN
               SUBST("ERROR:Invoice &1 is included in multiple input files", Invoice.ExtInvId).

            fMakeTemp(ttDueDate.BankCode).
         END.
      END.
      INPUT STREAM sin CLOSE.

  END.
  INPUT STREAM sFile CLOSE.

  IF liFiles EQ 0 THEN RETURN SUBST("ERROR:Input folder &1 is empty", lcInputFile).

  FOR EACH ttInvoice,
      FIRST Invoice EXCLUSIVE-LOCK WHERE
            Invoice.InvNum = ttInvoice.Invnum:

      ASSIGN
         Invoice.DDFile = ""
         Invoice.DDState = 0.

  END.
      
END.
ELSE IF icInvID1 = icInvID2 THEN 
FOR FIRST Invoice NO-LOCK WHERE
          Invoice.Brand    = gcBrand AND
          Invoice.ExtInvID = icInvID1,
    FIRST Customer OF Invoice NO-LOCK:
   
   icInvGrp = Customer.InvGroup.
   
   fMakeTemp("").

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

   fMakeTemp("").
        
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

   fMakeTemp("").

   IF NOT SESSION:BATCH AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liPicked FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl2.
   END.
   
END. 

/* Call pSplitOtherInvoices to split other bank code invoices */
/* based on the algorithm specified in the YOT-1450           */
IF icInputFileDir EQ "" THEN DO:
   
   /* Fetch allocation percentage for each individual banks */
   FOR EACH BankAccount NO-LOCK:
      CASE LEFT-TRIM(BankAccount.InvForm,"DD"):
             WHEN {&TF_BANK_UNOE}     THEN liSAIPerc = BankAccount.DDAllocation.
             WHEN {&TF_BANK_BBVA}     THEN liBBIPerc = BankAccount.DDAllocation.
             WHEN {&TF_BANK_SABADELL} THEN liSBIPerc = BankAccount.DDAllocation.
             WHEN {&TF_BANK_LACAXIA}  THEN liLAIPerc = BankAccount.DDAllocation.
      END CASE.
   END.
   
   IF liSAIPerc + liBBIPerc + 
      liSBIPerc + liLAIPerc NE 100 THEN 
   RETURN "ERROR: Allocation percentage is less/greater than 100%".      

   ASSIGN 
      liSAIValue = (liSAIPerc * liPicked) / 100
      liBBIValue = (liBBIPerc * liPicked) / 100
      liSBIValue = (liSBIPerc * liPicked) / 100
      liLAIValue = (liLAIPerc * liPicked) / 100.

   /* After taking percentage values, the invoice count 
      may vary due to rounding the values. So the remaining 
      invoices are allocated to BBVA Bank, as in real life 
      not more than 33.5% invoices are allocated to this bank */
   IF liPicked NE (liSAIValue + liBBIValue + 
                   liSBIValue + liLAIValue) THEN
      liBBICount = liBBICount + (liPicked - (liSAIValue + liBBIValue +
                                             liSBIValue + liLAIValue)).

   /* This code has to deleted after the Nov bill run */
   IF liLAICount > 10000 THEN DO:
      liLAIolval = liLAICount - 10000.
      RUN pSplitBankInvoices(liLAIolval,{&TF_BANK_LACAXIA}).
   END.    
   
   /* This code has to be uncommented after the Nov bill run */
 /*  IF liLAICount > liLAIValue THEN DO:
      liLAIolval = liLAICount - liLAIValue.
      RUN pSplitBankInvoices(liLAIolval,{&TF_BANK_LACAXIA}).
   END. */

   IF liSBICount > liSBIValue THEN DO:
      liSBIolval = liSBICount - liSBIValue.
      RUN pSplitBankInvoices(liSBIolval,{&TF_BANK_SABADELL}). 
   END.    

   IF liSAICount > liSAIValue THEN DO:
      liSAIolval = liSAICount - liSAIValue.
      RUN pSplitBankInvoices(liSAIolval,{&TF_BANK_UNOE}).  
   END.    
   
   IF liBBICount > liBBIValue THEN DO:
      liBBIolval = liBBICount - liBBIValue.
      RUN pSplitBankInvoices(liBBIolval,{&TF_BANK_BBVA}).
   END.

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
END.

/* should an empty file be made */
IF NOT CAN-FIND(FIRST ttDueDate) AND
   ilEmptyFile AND icInputFileDir EQ "" THEN DO:
   CREATE ttDueDate.
   ttDueDate.DueDate = TODAY.
END.

IF icRunMode = "test" THEN 
   lcTestDir = fCParamC("FRTestRunDir").
   
/* print, one file per due date / bankcode  */
FOR EACH ttDueDate:
         
   lcFileXml = fCParamC("DDebitFileName"). 

   IF icInputFileDir > "" THEN
      lcFileXml = SUBSTRING(lcFileXml, 1, R-INDEX(lcFileXml,"/")) +
                  "RGN_" + ENTRY(NUM-ENTRIES(lcFileXml,"/"),lcFileXml,"/").

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
      liFileSeq = liFileSeq + 1.

   /* use own directory for test run */
   IF icRunMode = "test" AND lcTestDir > "" THEN 
      lcFile = lcTestDir + "*" + lcFile.

   RUN Inv/ddoutfile_xml.p (INPUT-OUTPUT TABLE ttInvoice,  
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

   DEFINE VARIABLE liCount AS INTEGER NO-UNDO INITIAL 0.

   FOR EACH ttInvoice EXCLUSIVE-LOCK  WHERE
            ttInvoice.BankCode = ""
         BY ttInvoice.InvAmt DESC:
   
      /* Temporary code for Nov 2015 bill run YDR-1837 */
      /* Assigning 10,000 invoices to La Caixa bank code */ 
      IF liLAICount   < 10000 AND 
         YEAR(TODAY)  EQ 2015 THEN 
         ASSIGN 
            ttInvoice.BankCode = {&TF_BANK_LACAXIA}
            liLAICount         = liLAICount + 1.
      ELSE  /* Split based on ticket ydr-1837 */
         RUN pSplitInvoice.

      liCount = liCount + 1.
   END. /* FOR EACH ttInvoice WHERE */

END PROCEDURE. /* PROCEDURE pSplitOtherInvoices: */

PROCEDURE pSplitBankInvoices:
DEFINE INPUT PARAMETER liOLValue  AS INT  NO-UNDO. /* Over limit Value */
DEFINE INPUT PARAMETER lcBankCode AS CHAR NO-UNDO. /* Bank code Value */

DEF VAR liCount AS INT NO-UNDO INITIAL 0. 

   FOR EACH ttInvoice EXCLUSIVE-LOCK WHERE
            ttInvoice.BankCode = lcBankCode
         BY ttInvoice.InvAmt:
      
     RUN pSplitInvoice.

     liCount = liCount + 1.

     IF liOLValue EQ liCount THEN 
        LEAVE.
   END.      

END PROCEDURE. 

PROCEDURE pSplitInvoice:

   IF liSBICount <= liSBIValue THEN
      ASSIGN 
         ttInvoice.BankCode = {&TF_BANK_SABADELL}
         liSBICount         = liSBICount + 1.
   ELSE IF liSAICount <= liSAIValue THEN
      ASSIGN 
         ttInvoice.BankCode = {&TF_BANK_UNOE}
         liSAICount         = liSAICount + 1.
   ELSE IF liBBICount <= liBBIValue THEN
      ASSIGN 
         ttInvoice.BankCode = {&TF_BANK_BBVA}
         liBBICount         = liBBICount + 1.

/* This has to be uncommented after nov bill run */
/*  ELSE IF liLAICount <= liLAIValue THEN
   ttInvoice.BankCode = {&TF_BANK_LACAXIA}.  */

END PROCEDURE.
