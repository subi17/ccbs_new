/* ----------------------------------------------------------------------
  MODULE .......: billing_report.p
  TASK .........: Print a report from billing run
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 11.03.09
  Version ......: yoigo
---------------------------------------------------------------------- */


{commali.i}
{timestamp.i}
{cparam2.i}
{ftransdir.i}
{customer_address.i}
{funcrunprocess_update.i}
{tmsconst.i}

DEF INPUT  PARAMETER idaInvDate       AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiInvType        AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile           AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilDetails        AS LOG  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiInvCount       AS INT  NO-UNDO.

DEF VAR lcFile              AS CHAR NO-UNDO.
DEF VAR liCnt               AS INT  NO-UNDO.
DEF VAR lcTransDir          AS CHAR NO-UNDO.
DEF VAR lcValue             AS CHAR NO-UNDO.
DEF VAR liRowType           AS INT  NO-UNDO.
DEF VAR lcSections          AS CHAR NO-UNDO.
DEF VAR llAppend            AS LOG  NO-UNDO.
DEF VAR llTransDir          AS LOG  NO-UNDO.
DEF VAR lcCVSTransDir       AS CHAR NO-UNDO.
DEF VAR lcCVSFile           AS CHAR NO-UNDO.
DEF VAR ldaBillPeriodEnd    AS DATE NO-UNDO.
DEF VAR lcNotPrinted        AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttInvGroup NO-UNDO
   FIELD InvGroup   AS CHAR
   FIELD DueDate    AS DATE
   FIELD InvQty     AS INT
   FIELD SubInvQty  AS INT
   FIELD AmtExclVat AS DEC 
   FIELD InvAmt     AS DEC
   INDEX InvGroup InvGroup DueDate. 

DEF TEMP-TABLE ttInvDetail NO-UNDO
   FIELD Section    AS INT
   FIELD RowType    AS INT
   FIELD ErrorCode  AS INT
   FIELD InvNum     AS INT
   INDEX Section Section RowType
   INDEX InvNum InvNum. 
   
DEF TEMP-TABLE ttErrorCode NO-UNDO
   FIELD ErrorCode    AS INT
   FIELD ErrorMessage AS CHAR
   FIELD DispDetails  AS LOG
   INDEX ErrorCode ErrorCode.

DEF TEMP-TABLE ttSection NO-UNDO
   FIELD Section    AS INT
   FIELD RowType    AS INT
   FIELD RowInfo    AS CHAR
   FIELD Qty        AS INT
   FIELD AmtExclVat AS DEC 
   FIELD AmtInclVat AS DEC
   INDEX Section Section RowType RowInfo.

DEF TEMP-TABLE ttSectionName NO-UNDO
   FIELD Section  AS INT
   FIELD RowType  AS INT
   FIELD RowName  AS CHAR
   INDEX Section Section RowType.
   
DEF TEMP-TABLE ttDetails NO-UNDO
   FIELD Section AS INT
   FIELD RowType AS INT
   FIELD DispDetails AS LOGIC
   INDEX Section Section RowType.

/* CVS report */
DEF TEMP-TABLE ttCVSReport NO-UNDO
  FIELD BillPeriodEnd AS DATE
  FIELD InvoiceType AS CHAR
  FIELD ReportDate AS DATE
  FIELD Category AS CHAR
  FIELD SubCategory AS CHAR
  FIELD SubQty AS INT
  FIELD InvQty AS INT
  FIELD Quantity AS INT
  FIELD AmtExclVat AS DEC
  FIELD AmtInclVat AS DEC
  FIELD DueDate AS CHAR 
  INDEX Category Category SubCategory.


DEF STREAM sFile.
DEF STREAM sCVSFile.


FUNCTION fInvDetail RETURNS LOGIC
   (iiSection   AS INT,
    iiRowType   AS INT,
    iiErrorCode AS INT):
   
   /* is detailed listing allowed */
   IF iiErrorCode = 0 THEN DO:
      FIND FIRST ttDetails WHERE 
                 ttDetails.Section = iiSection AND
                 ttDetails.RowType = iiRowType NO-ERROR.
      IF NOT AVAILABLE ttDetails OR NOT ttDetails.DispDetails THEN 
         RETURN FALSE.
   END.
   
   ELSE DO: 
      FIND FIRST ttErrorCode WHERE 
                 ttErrorCode.ErrorCode = iiErrorCode NO-ERROR.
      IF NOT AVAILABLE ttErrorCode OR NOT ttErrorCode.DispDetails THEN
         RETURN FALSE. 
   END.
   
   IF CAN-FIND(FIRST ttInvDetail USE-INDEX InvNum WHERE          
                     ttInvDetail.InvNum = Invoice.InvNum AND
                     ttInvDetail.Section = iiSection AND
                     ttInvDetail.RowType = iiRowType) 
   THEN RETURN FALSE. 
   
   CREATE ttInvDetail.
   ASSIGN
      ttInvDetail.Section   = iiSection
      ttInvDetail.RowType   = iiRowType
      ttInvDetail.ErrorCode = iiErrorCode
      ttInvDetail.InvNum    = Invoice.InvNum.

END FUNCTION. 

FUNCTION fSectionAmt RETURNS LOGIC
   (iiSection AS INT,
    iiRowType AS INT,
    icRowInfo AS CHAR,
    iiQty     AS INT,
    idAmtExcl AS DEC,
    idAmtIncl AS DEC):

   FIND FIRST ttSection WHERE
              ttSection.Section = iiSection AND
              ttSection.RowType = iiRowType AND
              ttSection.RowInfo = icRowInfo NO-ERROR.
              
   IF NOT AVAILABLE ttSection THEN DO:
      CREATE ttSection.
      ASSIGN 
         ttSection.Section = iiSection
         ttSection.RowType = iiRowType
         ttSection.RowInfo = icRowInfo.
   END.
   
   ASSIGN 
      ttSection.Qty        = ttSection.Qty + iiQty
      ttSection.AmtExclVat = ttSection.AmtExclVat + idAmtExcl
      ttSection.AmtInclVat = ttSection.AmtInclVat + idAmtIncl.
    
END FUNCTION.
    
FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>>>>>>9.99")).
      
END FUNCTION.

FUNCTION fPrintInvDetails RETURNS LOGIC
   (iiSection AS INT,
    iiRowType AS INT):
 
   FOR EACH ttInvDetail WHERE
            ttInvDetail.Section = iiSection AND
            ttInvDetail.RowType = iiRowType,
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = ttInvDetail.InvNum
   BREAK BY Invoice.ExtInvID:

      IF FIRST(Invoice.ExtInvID) THEN 
         PUT STREAM sFile UNFORMATTED
            CHR(9) 
            "INVOICE ID"     CHR(9)
            "CUSTOMER"       CHR(9)
            "NOTICES"        SKIP.
          
      PUT STREAM sFile UNFORMATTED
                          CHR(9)
         Invoice.ExtInvID CHR(9)
         Invoice.CustNum  CHR(9).

      FIND FIRST ttErrorCode WHERE 
                 ttErrorCode.ErrorCode = ttInvDetail.ErrorCode NO-ERROR.
      IF AVAILABLE ttErrorCode THEN PUT STREAM sFile UNFORMATTED
         ttErrorCode.ErrorMessage.

      PUT STREAM sFile UNFORMATTED SKIP.
   END.      
    
END FUNCTION.


/******** Main start ******/      

RUN pInitialize.

RUN pCollectInvoices.
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

RUN pPrintReport.

RETURN "".

/******** Main end  ******/


PROCEDURE pInitialize:

   DEF VAR liBillPeriod AS INT  NO-UNDO.
   DEF VAR liPos AS INT  NO-UNDO.
   DEF VAR lcPlainFile AS CHAR NO-UNDO.

   lcTransDir = "".
   IF NUM-ENTRIES(icFile,"*") > 3 THEN ASSIGN
      llAppend   = (ENTRY(1,icFile,"*") = "append")
      llTransDir = (ENTRY(2,icFile,"*") = "trans")
      lcTransDir = ENTRY(3,icFile,"*")
      icFile     = ENTRY(4,icFile,"*").
   ELSE ASSIGN
      llAppend   = FALSE
      llTransDir = TRUE.

   IF llTransDir AND lcTransDir = "" THEN DO: 
      IF icRunMode = "test" THEN lcTransDir = fCParamC("FRTestRunDir").
      ELSE lcTransDir = fCParamC("BillRepTransDir").
      IF lcTransDir = ? THEN lcTransDir = "".
   END.

   FOR FIRST Invoice NO-LOCK USE-INDEX InvDate WHERE
                Invoice.Brand = gcBrand AND
                Invoice.InvDate = idaInvDate AND
                Invoice.InvType = iiInvType:
         liBillPeriod = YEAR(Invoice.ToDate) * 100 + MONTH(Invoice.ToDate).
         ldaBillPeriodEnd = Invoice.ToDate.
   END.

   lcFile = icFile.
   IF INDEX(lcFile,"#PERIOD") > 0 THEN
      lcFile = REPLACE(lcFile,"#PERIOD",STRING(liBillPeriod,"999999")).
  
   ASSIGN 
      lcFile       = REPLACE(lcFile,"#INVDATE",STRING(idaInvDate,"999999"))
      lcFile       = REPLACE(lcFile,"#MODE",
                             STRING(ilDetails,"detailed/summary"))
      lcSections   = "DELIVERY TYPE,DELIVERY STATUS,CHARGE TYPE," +
                     "DIRECT DEBIT STATUS,INVOICES,INVOICED CDRS," +
                     "ACTIVE SUBSCRIPTIONS".


   IF NUM-ENTRIES(lcFile,"/") > 1 THEN ASSIGN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcFile,"/"),lcFile,"/")
      liPos       = INDEX(lcFile,lcPlainFile)
      lcCVSFile   = SUBSTRING(lcFile,1,liPos - 1).
   ELSE ASSIGN
      lcPlainFile = lcFile
      lcCVSFile   = "".

   ASSIGN
      lcCVSFile = lcCVSFile + "cvs_" + STRING(liBillPeriod,"999999") + "_" +
                  lcPlainFile 
      lcCVSTransDir = IF icRunMode = "test"
                      THEN fCParamC("FRTestRunDir")
                      ELSE fCParamC("PentahoBillingReport")
      lcNotPrinted = STRING({&INV_DEL_TYPE_EMAIL}) + "," +
                     STRING({&INV_DEL_TYPE_SMS}) + "," +
                     STRING({&INV_DEL_TYPE_NO_DELIVERY}) + "," +
                     STRING({&INV_DEL_TYPE_EMAIL_PENDING}) + "," + 
                     STRING({&INV_DEL_TYPE_NO_TRAFFIC}) + "," + 
                     STRING({&INV_DEL_TYPE_FUSION_EMAIL}) + "," + 
                     STRING({&INV_DEL_TYPE_FUSION_EMAIL_PENDING}). 
      
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 2
      ttSectionName.RowType = 901
      ttSectionName.RowName = "Undelivered".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 2
      ttSectionName.RowType = 902
      ttSectionName.RowName = "Delivery denied".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 2
      ttSectionName.RowType = 903
      ttSectionName.RowName = "No paper invoice".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 4
      ttSectionName.RowType = 901
      ttSectionName.RowName = "Invoice amount zero".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 4
      ttSectionName.RowType = 902
      ttSectionName.RowName = "No Direct Debit".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 5
      ttSectionName.RowType = 901
      ttSectionName.RowName = "Included subscriptions active during the " +
                              "Billing Period".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 5
      ttSectionName.RowType = 902
      ttSectionName.RowName = "Included subscriptions terminated prior " +
                              "to the Billing Period".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 5
      ttSectionName.RowType = 903
      ttSectionName.RowName = "Invoice with events that have not been " +
                              "targeted to any subscription".
   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 6
      ttSectionName.RowType = 901
      ttSectionName.RowName = "Invoiced CDRs during the Billing Period".

   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 6
      ttSectionName.RowType = 902
      ttSectionName.RowName = "Invoiced CDRs prior to the Billing Period".

   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 7
      ttSectionName.RowType = 901
      ttSectionName.RowName = "Subscription active on 1. day of the " +
                              "Billing Period".

   CREATE ttSectionName.
   ASSIGN 
      ttSectionName.Section = 7
      ttSectionName.RowType = 902
      ttSectionName.RowName = "Subscription activated during the " +
                              "Billing Period".


   FOR FIRST ReportConf NO-LOCK WHERE
             ReportConf.Brand    = gcBrand AND
             ReportConf.ReportID = "BillingReport":

      FOR EACH ReportConfRow OF ReportConf NO-LOCK WHERE
               ReportConfRow.RowType = "ReportItem":
         CREATE ttDetails.
         ASSIGN 
            ttDetails.Section     = ReportConfRow.DecValue
            ttDetails.RowType     = ReportConfRow.IntValue
            ttDetails.DispDetails = ReportConfRow.LogicValue.
      END.
             
      FOR EACH ReportConfRow OF ReportConf NO-LOCK WHERE
               ReportConfRow.RowType = "ErrorCode":
             
         CREATE ttErrorCode.
         ASSIGN 
            ttErrorCode.ErrorCode    = ReportConfRow.IntValue
            ttErrorCode.ErrorMessage = ReportConfRow.CharValue
            ttErrorCode.DispDetails  = ReportConfRow.LogicValue.
      END.
   END.
         
END PROCEDURE.

PROCEDURE pCollectInvoices:

   DEF VAR liError      AS INT  NO-UNDO.
   DEF VAR ldPeriodBeg  AS DEC  NO-UNDO.
   DEF VAR ldPeriodEnd  AS DEC  NO-UNDO.
   DEF VAR liCDRQty     AS INT  NO-UNDO EXTENT 2.
   DEF VAR ldAmtExcl    AS DEC  NO-UNDO EXTENT 2.
   DEF VAR ldAmtIncl    AS DEC  NO-UNDO EXTENT 2.
   DEF VAR liPeriod     AS INT  NO-UNDO.
   DEF VAR liSubsActive AS INT  NO-UNDO EXTENT 2.
   DEF VAR lcPrintHouse AS CHAR NO-UNDO.
   DEF VAR ldPrintStamp AS DEC  NO-UNDO.
    
   ldPrintStamp = fMake2DT(idaInvDate + 2,86399).  

   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand    = gcBrand    AND
            Invoice.InvDate  = idaInvDate AND
            Invoice.InvType  = iiInvType,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = Invoice.CustNum:

      oiInvCount = oiInvCount + 1.
      IF NOT SESSION:BATCH AND oiInvCount MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP oiInvCount  
                 LABEL "Invoice Qty" 
                 FORMAT ">>>>>>>9"
         WITH SIDE-LABELS 1 DOWN ROW 10 CENTERED OVERLAY TITLE " Collecting "
              FRAME fQty.
      END.

      FIND FIRST ttInvGroup WHERE 
                 ttInvGroup.InvGroup = Invoice.InvGroup AND
                 ttInvGroup.DueDate  = Invoice.DueDate
         NO-ERROR.
      IF NOT AVAILABLE ttInvGroup THEN DO:
         CREATE ttInvGroup.
         ASSIGN
            ttInvGroup.InvGroup    = Invoice.InvGroup
            ttInvGroup.DueDate     = Invoice.DueDate.
      END.
      
      ASSIGN
         ttInvGroup.InvQty     = ttInvGroup.InvQty + 1
         ttInvGroup.AmtExclVat = ttInvGroup.AmtExclVat + Invoice.AmtExclVat
         ttInvGroup.InvAmt     = ttInvGroup.InvAmt + Invoice.InvAmt.
         
      FOR EACH SubInvoice OF Invoice NO-LOCK:
         ttInvGroup.SubInvQty = ttInvGroup.SubInvQty + 1.
      END.
         
      /* 1: delivery type */
      fSectionAmt(1,
                  Invoice.DelType,
                  "",
                  1,
                  Invoice.AmtExclVat,
                  Invoice.InvAmt).
      IF ilDetails THEN fInvDetail(1,Invoice.DelType,0).    
      
      /* 2: delivery status */
      ASSIGN
         liError = 0
         lcPrintHouse = "".
         
      IF Invoice.PrintState > 0 AND 
         LOOKUP(STRING(Invoice.DelType),lcNotPrinted) = 0
      THEN DO:
         liRowType = Invoice.PrintState.
         FOR FIRST ITSendLog NO-LOCK WHERE 
                   ITSendLog.InvNum  = Invoice.InvNum AND
                   ITSendLog.RepType = "Inv" AND
                   ITSendLog.SendStamp < ldPrintStamp:
            lcPrintHouse = ITSendLog.SendInfo.      
         END.
      END.

      ELSE DO:
         IF Invoice.InvCfg[1] = TRUE THEN ASSIGN
            liRowType = 902
            liError   = 205.

         ELSE DO:
            IF LOOKUP(STRING(Invoice.DelType),lcNotPrinted) > 0 THEN 
               liRowType = 903.
            ELSE liRowType = 901.
         
            IF ilDetails THEN DO:
               fSetCustData().
         
               IF LOOKUP(STRING(Invoice.DelType),lcNotPrinted) > 0 THEN
                  liError = 206. 
               ELSE IF Invoice.DelType > 1 THEN liError = 204. 
               
               ELSE IF fCheckAddress(lcCustName,
                                lcZipCode,
                                lcCountry,
                                OUTPUT lcValue) > ""
               THEN liError = 201.
      
               ELSE IF lcAddress = "" AND lcZipCode = "" AND lcPost = "" THEN 
                  liError = 201.
         
               ELSE IF Invoice.InvCfg[1] = TRUE THEN liError = 202.
  
               ELSE IF Invoice.CrInvNum > 0 OR
                  Invoice.InvAmt < 0 THEN liError = 203.
         
               ELSE liError = 210.
            END.   
         END.
            
      END.
   
      fSectionAmt(2,
                  liRowType,
                  lcPrintHouse,
                  1,
                  Invoice.AmtExclVat,
                  Invoice.InvAmt).
      IF ilDetails THEN fInvDetail(2,liRowType,liError).
      
      /* 3: charge type */
      fSectionAmt(3,
                  Invoice.ChargeType,
                  "",
                  1,
                  Invoice.AmtExclVat,
                  Invoice.InvAmt).
      IF ilDetails THEN fInvDetail(3,Invoice.ChargeType,0).

      /* 4: direct debit status */
      ASSIGN
         liRowType = Invoice.DDState
         liError   = 0.

      IF Invoice.DDState = 0 THEN DO:
         IF Invoice.InvAmt = 0 THEN ASSIGN
            liRowType = 901
            liError   = 401.
         
         ELSE IF ilDetails THEN DO:
            IF LENGTH(Customer.BankAcc) < 24 THEN liError = 402.
            ELSE IF Invoice.PaidAmt NE 0 THEN liError = 403.
            ELSE IF Invoice.PrintState = 0 THEN liError = 404.
            ELSE liError = 410.
         END.    
      END.   
      
      IF Invoice.ChargeType = 5 THEN liRowType = 902.

      fSectionAmt(4,
                  liRowType,
                  IF liRowType = 1 THEN Invoice.DDFile ELSE "",
                  1,
                  Invoice.AmtExclVat,
                  Invoice.InvAmt).
      IF ilDetails THEN fInvDetail(4,liRowType,liError).

      /* 5: invoices */
      ASSIGN 
         ldPeriodBeg  = fMake2Dt(Invoice.FromDate,0)
         ldPeriodEnd  = fMake2Dt(Invoice.ToDate,86399)
         liSubsActive = 0.

      FOR EACH SubInvoice OF Invoice NO-LOCK:
         IF SubInvoice.MsSeq = 0 THEN liRowType = 903.
         ELSE liRowType = 902.
            
         IF SubInvoice.MsSeq > 0 THEN 
         FOR EACH MsOwner NO-LOCK USE-INDEX MsSeq WHERE
                  MsOwner.MsSeq   = SubInvoice.MsSeq AND
                  MsOwner.TsEnd  >= ldPeriodBeg      AND
                  MsOwner.TsBeg  <  ldPeriodEnd      AND
                  MsOwner.InvCust = Invoice.CustNum  AND
                  MsOwner.PayType = FALSE
         BY MsOwner.TsBeg DESC:
            liRowType = 901.
            IF MsOwner.TsBeg <= ldPeriodBeg THEN 
               liSubsActive[1] = liSubsActive[1] + 1.
            ELSE liSubsActive[2] = liSubsActive[2] + 1.
            LEAVE.                                  
         END.

         fSectionAmt(5,
                     liRowType,
                     "",
                     1,
                     SubInvoice.AmtExclVat,
                     SubInvoice.InvAmt).                  
         IF ilDetails THEN fInvDetail(5,liRowType,0).
      END.
      
      /* 6: invoiced cdrs */
      ASSIGN 
         liCDRQty  = 0
         ldAmtExcl = 0
         ldAmtIncl = 0.
         
      FOR EACH InvRow OF Invoice NO-LOCK WHERE
               InvRow.RowType = 2:

         IF InvRow.FromDate NE ? AND InvRow.FromDate < Invoice.FromDate
         THEN liPeriod = 2.
         ELSE liPeriod = 1.
         
         liCDRQty[liPeriod] = liCDRQty[liPeriod] + InvRow.Qty.      
         IF Invoice.VatIncl THEN ASSIGN
            ldAmtIncl[liPeriod] = ldAmtIncl[liPeriod] + InvRow.Amt
            ldAmtExcl[liPeriod] = ldAmtExcl[liPeriod] + 
                                  (InvRow.Amt / (1 + InvRow.VatPerc / 100)).
         ELSE ASSIGN
            ldAmtExcl[liPeriod] = ldAmtExcl[liPeriod] + InvRow.Amt
            ldAmtIncl[liPeriod] = ldAmtIncl[liPeriod] + 
                                  (InvRow.Amt * (1 + InvRow.VatPerc / 100)).
      END.
      
      DO liPeriod = 1 TO 2:
         fSectionAmt(6,
                     900 + liPeriod,
                     "",
                     liCDRQty[liPeriod],
                     ldAmtExcl[liPeriod],
                     ldAmtIncl[liPeriod]).
      END.
      
      /* 7: active subscriptions */
      DO liCnt = 1 TO 2:
         IF liSubsActive[liCnt] > 0 THEN 
            fSectionAmt(7,
                        900 + liCnt,
                        "",
                        liSubsActive[liCnt],
                        0,
                        0).
      END.                  
      
      IF iiUpdateInterval > 0 AND oiInvCount MOD iiUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiInvCount) THEN
            RETURN "ERROR:Stopped".
      END.   
       
   END.

   IF NOT SESSION:BATCH THEN 
      HIDE FRAME fQty NO-PAUSE.
   
   RETURN "".
   
END PROCEDURE.  /* pCollectInvoices */

PROCEDURE pPrintReport:

   DEF VAR liCnt         AS INT  NO-UNDO.
   DEF VAR lcDescription AS CHAR NO-UNDO.
   DEF VAR liSection     AS INT  NO-UNDO.
   DEF VAR lcFieldName   AS CHAR NO-UNDO.
   DEF VAR lcFinalFile   AS CHAR NO-UNDO.
   DEF VAR lcInvoiceType AS CHAR NO-UNDO.

   IF llAppend THEN DO:
      OUTPUT STREAM sFile TO VALUE(lcFile) APPEND.
      PUT STREAM sFile SKIP(3).
   END.
   ELSE OUTPUT STREAM sFile TO VALUE(lcFile).

   lcInvoiceType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                   "Invoice",
                   "InvType",
                   STRING(iiInvType)) .

   PUT STREAM sFile UNFORMATTED
      "BILLING REPORT BY INVOICE DATE AND TYPE" SKIP
      "Invoice date"  CHR(9)
      STRING(idaInvDate,"99.99.9999")  SKIP
      "Invoice type"  CHR(9)
      lcInvoiceType SKIP
      "Reporting date" CHR(9)
      STRING(TODAY,"99.99.9999") SKIP(1).
   
   FOR EACH ttInvGroup
   BREAK BY ttInvGroup.InvGroup:
   
      FIND FIRST InvGroup WHERE
                 InvGroup.Brand = gcBrand AND
                 InvGroup.InvGroup = ttInvGroup.InvGroup NO-LOCK NO-ERROR.
                 
      IF FIRST(ttInvGroup.InvGroup) THEN 
      PUT STREAM sFile UNFORMATTED
         "INVOICE GROUP"    CHR(9)
         "NAME"             CHR(9)
         "DUE DATE"         CHR(9)
         "SUBSCRIPTION QTY" CHR(9)
         "INVOICE QTY"      CHR(9)
         "AMOUNT EXCL. TAX" CHR(9)
         "AMOUNT INCL. TAX" SKIP.
         

      PUT STREAM sFile UNFORMATTED
         ttInvGroup.InvGroup                      CHR(9)
         (IF AVAILABLE InvGroup THEN InvGroup.IGName ELSE "") CHR(9)
         STRING(ttInvGroup.DueDate,"99.99.9999")  CHR(9)
         ttInvGroup.SubInvQty                     CHR(9)
         ttInvGroup.InvQty                        CHR(9)
         fDispDecimal(ttInvGroup.AmtExclVat)      CHR(9)
         fDispDecimal(ttInvGroup.InvAmt)          SKIP.
         
      ACCUMULATE 
         ttInvGroup.SubInvQty (TOTAL)
         ttInvGroup.InvQty (TOTAL)
         ttInvGroup.AmtExclVat (TOTAL)
         ttInvGroup.InvAmt (TOTAL).
                 
      IF LAST(ttInvGroup.InvGroup) THEN 
      PUT STREAM sFile UNFORMATTED
         "TOTAL"                                         CHR(9)
         CHR(9) 
         CHR(9)
         (ACCUM TOTAL ttInvGroup.SubInvQty)              CHR(9)
         (ACCUM TOTAL ttInvGroup.InvQty)                 CHR(9)
         fDispDecimal(ACCUM TOTAL ttInvGroup.AmtExclVat) CHR(9)
         fDispDecimal(ACCUM TOTAL ttInvGroup.InvAmt)     SKIP.

      CREATE ttCVSReport. 
      ASSIGN ttCVSReport.BillPeriodEnd = ldaBillPeriodEnd
             ttCVSReport.InvoiceType = lcInvoiceType
             ttCVSReport.ReportDate = TODAY
             ttCVSReport.Category = "INVOICE GROUP"
             ttCVSReport.SubCategory = ttInvGroup.InvGroup
             ttCVSReport.SubQty =  ttInvGroup.SubInvQty
             ttCVSReport.InvQty =  ttInvGroup.InvQty
             ttCVSReport.AmtExclVat = ttInvGroup.AmtExclVat
             ttCVSReport.AmtInclVat = ttInvGroup.InvAmt
             ttCVSReport.DueDate = STRING(ttInvGroup.DueDate). 
     
      RELEASE ttCVSReport.
      
   END.
     
   DO liSection = 1 TO 7:
   
      PUT STREAM sFile UNFORMATTED
         SKIP(1)
         ENTRY(liSection,lcSections) SKIP.
      
      CASE liSection:
      WHEN 1 THEN lcFieldName = "DelType".
      WHEN 2 THEN lcFieldName = "PrintState".
      WHEN 3 THEN lcFieldName = "ChargeType".
      WHEN 4 THEN lcFieldName = "DDState".
      OTHERWISE lcFieldName = "".
      END CASE.
      
      FOR EACH ttSection WHERE
               ttSection.Section = liSection
      BREAK BY ttSection.RowType
            BY ttSection.RowInfo:
               
         IF ttSection.RowType < 900 THEN 
            lcDescription = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                             "Invoice",
                                             lcFieldName,
                                             STRING(ttSection.RowType)).
         ELSE DO:
            FIND FIRST ttSectionName WHERE 
                       ttSectionName.Section = ttSection.Section AND
                       ttSectionName.RowType = ttSection.RowType NO-ERROR.
            IF AVAILABLE ttSection THEN
               lcDescription = ttSectionName.RowName.
         END.
         
         IF lcDescription = "" THEN 
            lcDescription = "Type " + STRING(ttSection.RowType).
 
         PUT STREAM sFile UNFORMATTED
            lcDescription + 
               (IF ttSection.RowInfo > "" THEN " " + ttSection.RowInfo ELSE "")
               CHR(9)
            CHR(9)
            CHR(9)
            CHR(9)
            ttSection.Qty  CHR(9).
            
         IF liSection NE 7 THEN PUT STREAM sFile UNFORMATTED
            fDispDecimal(ttSection.AmtExclVat) CHR(9)
            fDispDecimal(ttSection.AmtInclVat).
             
         PUT STREAM sFile UNFORMATTED SKIP.
       
         IF ilDetails THEN DO:   
            fPrintInvDetails(ttSection.Section,
                             ttSection.RowType).
         END.   

         ACCUMULATE ttSection.Qty        (TOTAL BY ttSection.RowType)
                    ttSection.AmtExclVat (TOTAL BY ttSection.RowType)
                    ttSection.AmtInclVat (TOTAL BY ttSection.RowType)
                    ttSection.RowInfo    (COUNT BY ttSection.RowType).

         IF LAST-OF(ttSection.RowType) AND 
           (ACCUM COUNT BY ttSection.RowType ttSection.RowInfo) > 1 THEN DO:

            PUT STREAM sFile UNFORMATTED
               lcDescription " TOTAL" CHR(9)
               CHR(9)
               CHR(9)
               CHR(9)
               (ACCUM TOTAL BY ttSection.RowType ttSection.Qty) CHR(9).
               
            IF liSection NE 7 THEN PUT STREAM sFile UNFORMATTED
               fDispDecimal(ACCUM TOTAL BY ttSection.RowType
                               ttSection.AmtExclVat) CHR(9)
               fDispDecimal(ACCUM TOTAL BY ttSection.RowType
                               ttSection.AmtInclVat).
               
            PUT STREAM sFile UNFORMATTED SKIP.
         END.
                 
         IF LAST(ttSection.RowType) THEN DO:
            PUT STREAM sFile UNFORMATTED
               "TOTAL" CHR(9)
               CHR(9)
               CHR(9)
               CHR(9)
               (ACCUM TOTAL ttSection.Qty) CHR(9).
               
               
            IF liSection NE 7 THEN PUT STREAM sFile UNFORMATTED
               fDispDecimal(ACCUM TOTAL ttSection.AmtExclVat) CHR(9)
               fDispDecimal(ACCUM TOTAL ttSection.AmtInclVat).
               
            PUT STREAM sFile UNFORMATTED SKIP.
         END.

         /* update CVS Report */
         CREATE ttCVSReport. 
         ASSIGN ttCVSReport.BillPeriodEnd = ldaBillPeriodEnd
                ttCVSReport.InvoiceType = lcInvoiceType 
                ttCVSReport.ReportDate = TODAY
                ttCVSReport.Category   = ENTRY(liSection,lcSections)
                ttCVSReport.SubCategory   = lcDescription +
                                         (IF ttSection.RowInfo > "" 
                                          THEN " " + ttSection.RowInfo ELSE "") 
                ttCVSReport.Quantity =   ttSection.Qty
                ttCVSReport.AmtExclVat =  ttSection.AmtExclVat
                ttCVSReport.AmtInclVat =  ttSection.AmtInclVat.

      END.      

   END.

   OUTPUT STREAM sFile CLOSE.

   /* create CVS file */
   IF llAppend THEN OUTPUT STREAM sCVSFile TO VALUE(lcCVSFile) APPEND.
   ELSE DO:
     OUTPUT STREAM sCVSFile TO VALUE(lcCVSFile).
     PUT STREAM sCVSFile UNFORMATTED
          "BillPeriodEnd"     CHR(9)
          "InvoiceType"     CHR(9)
          "ReportDate"      CHR(9)
          "Category"        CHR(9)
          "SubCategory"     CHR(9)
          "Quantity"        CHR(9)
          "SubscriptionQty" CHR(9)
          "InvoiceQty"      CHR(9)
          "AmountExclTax"   CHR(9)
          "AmountInclTax"   CHR(9)
          "DueDate"         SKIP.
   END.
   FOR EACH ttCVSReport NO-LOCK :
       PUT STREAM sCVSFile UNFORMATTED
           ttCVSReport.BillPeriodEnd  CHR(9)
           ttCVSReport.InvoiceType    CHR(9)
           ttCVSReport.ReportDate     CHR(9)
           ttCVSReport.Category       CHR(9)
           ttCVSReport.SubCategory    CHR(9)
           ttCVSReport.Quantity       CHR(9)
           ttCVSReport.SubQty         CHR(9)
           ttCVSReport.InvQty         CHR(9)
           ttCVSReport.AmtExclVat     CHR(9)
           ttCVSReport.AmtInclVat     CHR(9)
           ttCVSReport.DueDate  SKIP.
   END.
   OUTPUT STREAM sCVSFile CLOSE.

   /* move the file to the transfer directory */
   lcFinalFile = "".
   IF lcTransDir > "" AND llTransDir THEN DO:
      lcFinalFile = fMove2TransDir(lcFile,
                                   ".txt",
                                   lcTransDir).
   END.
   IF lcFinalFile = "" THEN lcFinalFile = lcFile.
   
   IF iiFRProcessID > 0 THEN DO TRANS:
      CREATE FuncRunResult.
      ASSIGN 
         FuncRunResult.FRProcessID = iiFRProcessID
         FuncRunResult.FRResultSeq = 1
         FuncRunResult.ResultOrder = 1
         FuncRunResult.CharParam   = lcFinalFile.
   END.

   /* move CVS report */
   IF lcCVSTransDir > "" AND llTransDir THEN DO:
      lcFinalFile = fMove2TransDir(lcCVSFile,
                                   ".txt",
                                   lcCVSTransDir).
   END.

END PROCEDURE. /* pPrintReport */

