/* ----------------------------------------------------------------------
  MODULE .......: ddoutfileco.p
  TASK .........: Collect invoices to a direct debit file 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.01.07
  CHANGED ......: 19.04.07/aam icBillRun, check ChargeType
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{timestamp.i}

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
DEF VAR lcFileTxt   AS CHAR NO-UNDO. 
DEF VAR lcFileXml   AS CHAR NO-UNDO. 
DEF VAR dte         AS DATE NO-UNDO.
DEF VAR tme         AS INT NO-UNDO.
DEF VAR lcoutstring   AS CHAR NO-UNDO.
DEF VAR lclogpath     AS CHARACTER NO-UNDO. 
DEF VAR lcfinfile     AS CHARACTER NO-UNDO. 
fSplitTS(fMakeTS(), OUTPUT dte, OUTPUT tme).
ASSIGN
   lclogpath = "/apps/yoigo/tms_support/testing/log/csb_duration_"
   lcoutstring = STRING(dte,"999999") + "T" + STRING(tme,"hh:mm:ss") + ".txt"
   lcfinfile = lclogpath + icCSBFileForm + "_" + lcoutstring.
DEF STREAM sout.
OUTPUT STREAM sout TO VALUE(lcfinfile).
{ddoutfilett.i}

PUT STREAM sout UNFORMATTED
   "CSB SESSION WITH " icCSBFileForm " started:" fTS2HMS(fMakeTS()) SKIP.
DEF TEMP-TABLE ttDueDate NO-UNDO
   FIELD DueDate  AS DATE
   FIELD BankCode AS CHAR
   INDEX DueDate DueDate BankCode.

DEF BUFFER bttInvoice FOR ttInvoice.

FUNCTION fMakeTemp RETURNS LOGICAL.

    /* not numbered */
    IF Invoice.ExtInvID = "" THEN RETURN FALSE.
    
    /* already sent 
    IF Invoice.DDState > 0 THEN RETURN FALSE.
    */
    
    /* charge type is not direct debit */
    IF Invoice.ChargeType NE 2 THEN RETURN FALSE. 
             
    /* no negative invoices */
    IF Invoice.InvAmt <= 0 THEN RETURN FALSE. 
    
    /* no partly paid invoices 
    IF Invoice.PaidAmt NE 0 THEN RETURN FALSE. 
    */
    
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
/*       ttInvoice.BankCode = SUBSTRING(Customer.BankAcc,5,4).
   
       CASE ttInvoice.BankCode:
       WHEN "0049" THEN ASSIGN 
          liBankQty[1] = liBankQty[1] + 1
          ldBankAmt[1] = ldBankAmt[1] + Invoice.InvAmt.
       WHEN "0030" THEN ASSIGN
          liBankQty[2] = liBankQty[2] + 1
          ldBankAmt[2] = ldBankAmt[2] + Invoice.InvAmt.
       WHEN "0182" OR
       WHEN "2040" OR
       WHEN "2074" OR
       WHEN "2059" OR
       WHEN "2107" THEN ASSIGN
          liBankQty[3] = liBankQty[3] + 1
          ldBankAmt[3] = ldBankAmt[3] + Invoice.InvAmt
          ttInvoice.BankCode = "0182".
       WHEN "0081" THEN ASSIGN
          liBankQty[4] = liBankQty[4] + 1
          ldBankAmt[4] = ldBankAmt[4] + Invoice.InvAmt.
       /* Rest invoices will be divided based on the algorithm */
       OTHERWISE ASSIGN
          ttInvoice.BankCode = ""
          ttInvoice.Movable  = TRUE.
       END CASE.
*/
       IF liBankQty[1] < 10000 THEN ASSIGN
          liBankQty[1] = liBankQty[1] + 1
          ttInvoice.BankCode = "0081".
       ELSE IF liBankQty[2] < 10000 THEN ASSIGN
          liBankQty[2] = liBankQty[2] + 1
          ttInvoice.BankCode = "0049".
       ELSE ttInvoice.BankCode = "0182".
    END.
    ELSE ttInvoice.BankCode = "ALL".
    
    IF Customer.IDelName > "" 
    THEN ttInvoice.ZipCode = Customer.IDelZipCode.
    ELSE ttInvoice.ZipCode = Customer.ZipCode.

    IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.

    RETURN TRUE.
    
END FUNCTION.

IF icFile = "" THEN DO:
   icFile = "/store/riftp/tmp/YOIGO_CSB19_#BANK_#DDATE.txt".
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

/* print, one file per due date / bankcode  */
FOR EACH ttDueDate:

   /* Define CSB file name for each process */
   IF icCSBFileForm EQ "TXT" OR
      icCSBFileForm EQ "BOTH" THEN
      ASSIGN
         lcFileTxt = icFile
         lcFileXml = REPLACE(icFile,"txt","xml")
         lcFileXml = REPLACE(lcFileXml,"CSB19","SDD001").

   IF icCSBFileForm EQ "XML" THEN
      ASSIGN
         lcFileXml = REPLACE(icFile,"txt","xml")
         lcFileXml = REPLACE(lcFileXml,"CSB19","SDD001").

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

   
   IF icCSBFileForm EQ "TXT" OR
      icCSBFileForm EQ "BOTH" THEN DO:
      PUT STREAM sout UNFORMATTED
      "CSB " ttDueDate.BankCode " " icCSBFileForm " TXT part started:" fTS2HMS(fMakeTS()) SKIP.
      RUN /apps/yoigo/tms_support/testing/ddoutfile_test.p (INPUT-OUTPUT TABLE ttInvoice,  
                    ttDueDate.DueDate,
                    ttDueDate.BankCode,
                    liPicked,
                    lcFileTxt,
                    ilEmptyFile,
                    iiFRProcessID,
                    iiUpdInterval,
                    liFileSeq,
                    OUTPUT liInvCount). 
      PUT STREAM sout UNFORMATTED
      "CSB " ttDueDate.BankCode " " icCSBFileForm " TXT part ended:" fTS2HMS(fMakeTS()) SKIP.
   END.
   IF icCSBFileForm EQ "XML" OR
      icCSBFileForm EQ "BOTH" THEN DO:
      PUT STREAM sout UNFORMATTED
      "CSB " ttDueDate.BankCode " " icCSBFileForm " XML part started:" fTS2HMS(fMakeTS()) SKIP.
      RUN /apps/yoigo/tms_support/testing/ddoutfile_xml_test_SCHEMA.p (INPUT-OUTPUT TABLE ttInvoice,  
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
      PUT STREAM sout UNFORMATTED
      "CSB " ttDueDate.BankCode " " icCSBFileForm " XML part ended:" fTS2HMS(fMakeTS()) SKIP.
   END.
   PUT STREAM sout UNFORMATTED
    "----------------------------------------------------" SKIP.
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
      ActionLog.ActionID     = "DDFILETEST"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionChar   = " Files: " + STRING(oiFileCount) + CHR(10) +
                               " Invoices: " + STRING(oiInvCount)
      ActionLog.ActionStatus = 3.
      ActionLog.ActionTS     = fMakeTS().
END.
PUT STREAM sout UNFORMATTED
   "CSB SESSION WITH " icCSBFileForm " ended:" fTS2HMS(fMakeTS()) SKIP.

RETURN ocError.

PROCEDURE pSplitOtherInvoices:

   DEF VAR liLimit1   AS INT NO-UNDO.
   DEF VAR liLimit2   AS INT NO-UNDO.
   DEF VAR liCounter  AS INT NO-UNDO.
   DEF VAR liIndex    AS INT NO-UNDO.

   liLimit1 = 100 * (((0.46 * liPicked) - liBankQty[3]) /
                     (liPicked - (liBankQty[1] + liBankQty[2] + liBankQty[3] + liBankQty[4]))).

   liLimit2 = 100 * (((0.46 * liPicked) - (liBankQty[1] + liBankQty[2])) /
                     (liPicked - (liBankQty[1] + liBankQty[2] + liBankQty[3] + liBankQty[4]))).

   
   /* Fetch all other invoices by amount from higher to lower */
   /* and divide based on the algorithm                       */
   FOR EACH ttInvoice WHERE 
            ttInvoice.Movable = TRUE AND
            ttInvoice.BankCode = ""
            BY ttInvoice.InvAmt DESC:

       liIndex = liCounter MOD 100.

       IF liIndex <= liLimit1 THEN
          ASSIGN liBankQty[3] = liBankQty[3] + 1
                 ldBankAmt[3] = ldBankAmt[3] + ttInvoice.InvAmt
                 ttInvoice.BankCode = "0182"
                 ttInvoice.Movable  = FALSE.
       ELSE IF liLimit1 < liIndex AND liIndex <= (liLimit1 + liLimit2) THEN
          ASSIGN liBankQty[2] = liBankQty[2] + 1
                 ldBankAmt[2] = ldBankAmt[2] + ttInvoice.InvAmt
                 ttInvoice.BankCode = "0030"
                 ttInvoice.Movable  = FALSE.
       ELSE IF liIndex > (liLimit1 + liLimit2) THEN
          ASSIGN liBankQty[2] = liBankQty[4] + 1
                 ldBankAmt[2] = ldBankAmt[4] + ttInvoice.InvAmt
                 ttInvoice.BankCode = "0081"
                 ttInvoice.Movable  = FALSE.

       liCounter = liCounter + 1.

   END. /* FOR EACH ttInvoice WHERE */

END PROCEDURE. /* PROCEDURE pSplitOtherInvoices: */

