/* ----------------------------------------------------------------------
  MODULE .......: unbilled_subsqty.p
  TASK .........: Print a report of unbilled subscriptions 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 25.03.09
  Version ......: yoigo
---------------------------------------------------------------------- */

&GLOBAL-DEFINE TraceLog NO

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/coinv.i}
{Syst/funcrunprocess_update.i}

&IF "{&TraceLog}" = "YES" 
&THEN
{Func/log.i}
&ENDIF 

DEF INPUT  PARAMETER iiPeriod         AS INT  NO-UNDO.
DEF INPUT  PARAMETER idaInvDate1      AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaInvDate2      AS DATE NO-UNDO.
DEF INPUT  PARAMETER icFile           AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilDetails        AS LOG  NO-UNDO.
DEF INPUT  PARAMETER iiFRProcessID    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiUpdateInterval AS INT  NO-UNDO.
DEF INPUT  PARAMETER icRunMode        AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiSubsCount      AS INT  NO-UNDO.

DEF VAR lcFile              AS CHAR NO-UNDO.
DEF VAR liCnt               AS INT  NO-UNDO.
DEF VAR lcTransDir          AS CHAR NO-UNDO.
DEF VAR lcValue             AS CHAR NO-UNDO.
DEF VAR liFirstMonthNoUsage AS INT  NO-UNDO.
DEF VAR liTerminatedOnFirst AS INT  NO-UNDO.
DEF VAR liSTCOnFirst        AS INT  NO-UNDO.
DEF VAR liSuspended         AS INT  NO-UNDO.
DEF VAR liProhibitedActive  AS INT  NO-UNDO.
DEF VAR liProhibitedTerm    AS INT  NO-UNDO.
DEF VAR liOpenSubs          AS INT  NO-UNDO.
DEF VAR liBilledSubs        AS INT  NO-UNDO.
DEF VAR ldFromPeriod        AS DEC  NO-UNDO.
DEF VAR ldToPeriod          AS DEC  NO-UNDO.
DEF VAR ldaFromPeriod       AS DATE NO-UNDO.
DEF VAR ldaToPeriod         AS DATE NO-UNDO.
DEF VAR liUnbilledCust      AS INT  NO-UNDO.
DEF VAR llAppend            AS LOG  NO-UNDO.
DEF VAR llTransDir          AS LOG  NO-UNDO.
DEF VAR liRowType           AS INT  NO-UNDO.

DEF VAR lcCVSTransDir       AS CHAR NO-UNDO.
DEF VAR lcCVSFile           AS CHAR NO-UNDO.
DEF VAR ldaBillPeriodEnd    AS DATE NO-UNDO.

DEF TEMP-TABLE ttHandled NO-UNDO
   FIELD MsSeq   AS INT
   FIELD InvCust AS INT
   INDEX MsSeq MsSeq. 

DEF TEMP-TABLE ttBilled NO-UNDO
   FIELD MsSeq   AS INT
   FIELD InvCust AS INT
   INDEX MsSeq MsSeq. 

DEF TEMP-TABLE ttSubsDetail NO-UNDO
   FIELD RowType AS INT
   FIELD Owner   AS RECID
   INDEX RowType RowType.

DEF TEMP-TABLE ttCustomer NO-UNDO
   FIELD CustNum AS INT
   INDEX CustNum CustNum.

DEF TEMP-TABLE ttDetails NO-UNDO
   FIELD RowType     AS INT
   FIELD Description AS CHAR 
   FIELD DispDetails AS LOGIC
   INDEX RowType RowType.
 
DEF STREAM sFile.
DEF STREAM sCVSFile.


FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>>>>>>9.99")).
      
END FUNCTION.

FUNCTION fAddDetailRow RETURNS LOGIC
   (iiType AS INT):

   /* is detailed listing allowed */
   FIND FIRST ttDetails WHERE 
              ttDetails.RowType = iiType NO-ERROR.
   IF NOT AVAILABLE ttDetails OR NOT ttDetails.DispDetails THEN 
      RETURN FALSE.
    
   CREATE ttSubsDetail.
   ASSIGN 
      ttSubsDetail.RowType = iiType
      ttSubsDetail.Owner   = RECID(MsOwner).
      
END FUNCTION.

FUNCTION fAddCustomerRow RETURNS LOGIC
   (iiCustNum AS INT):

   FIND FIRST ttDetails WHERE 
              ttDetails.RowType = 401 NO-ERROR.
   IF NOT AVAILABLE ttDetails OR NOT ttDetails.DispDetails THEN 
      RETURN FALSE.
 
   IF CAN-FIND(FIRST ttCustomer WHERE 
      ttCustomer.CustNum = iiCustNum) THEN RETURN FALSE.
          
   CREATE ttCustomer.
   ttCustomer.CustNum = iiCustNum.
      
   liUnbilledCust = liUnbilledCust + 1.

   RETURN TRUE.
END FUNCTION.

FUNCTION fPrintDetailRow RETURNS LOGIC
   (iiType AS INT):
 
   IF NOT ilDetails THEN RETURN FALSE.
   
   IF iiType = 401 THEN 
   FOR EACH ttCustomer,
      FIRST Customer NO-LOCK WHERE
            Customer.CustNum = ttCustomer.Custnum
   BREAK BY Customer.CustNum:
   
      IF FIRST(Customer.CustNum) THEN       
      PUT STREAM sFile UNFORMATTED
         CHR(9) CHR(9)
         "Customer"  CHR(9)
         "Inv.Group" SKIP.

      PUT STREAM sFile UNFORMATTED
         CHR(9) CHR(9) 
         Customer.CustNum CHR(9)
         Customer.InvGroup SKIP.
   END.
   
   ELSE
   FOR EACH ttSubsDetail WHERE
            ttSubsDetail.RowType = iiType,
      FIRST MsOwner NO-LOCK WHERE
            RECID(MsOwner) = ttSubsDetail.Owner
   BREAK BY MsOwner.MsSeq:
            
      IF FIRST(MsOwner.MsSeq) THEN       
      PUT STREAM sFile UNFORMATTED
         CHR(9) CHR(9)
         "Subs.ID"  CHR(9)
         "MSISDN"   CHR(9)
         "Inv.Customer" SKIP.

      PUT STREAM sFile UNFORMATTED
         CHR(9) CHR(9) 
         MsOwner.MsSeq CHR(9)
         MsOwner.CLI   CHR(9)
         MsOwner.InvCust SKIP.
   END.
    
   
END FUNCTION.

FUNCTION fPrintCVSQuantity RETURNS LOGIC
      (icCategory AS CHAR,
       icSubCategory AS CHAR,
       icQuantity AS INT):
       PUT STREAM sCVSFile UNFORMATTED
           STRING(ldaToPeriod)  CHR(9)
           "Normal"  CHR(9)
           STRING(TODAY)   CHR(9)
           icCategory      CHR(9)
           icSubCategory   CHR(9)
           icQuantity      CHR(9)
           "0"             CHR(9)
           "0"             CHR(9)
           "0"             CHR(9)
           "0"             CHR(9) 
           "" SKIP.

END FUNCTION.


FUNCTION fPrintQuantity RETURNS LOGIC
   (iiType AS INT,
    iiQuantity AS INT,
    icCategory AS CHAR):
    
   DEF VAR lcDescription AS CHAR NO-UNDO.
   
   IF iiType = 999 THEN lcDescription = "TOTAL".
   ELSE DO:
      FIND FIRST ttDetails WHERE ttDetails.RowType = iiType NO-ERROR.
      IF AVAILABLE ttDetails THEN lcDescription = ttDetails.Description.
      ELSE lcDescription = "Row: " + STRING(iiType).
   END.   
   
   PUT STREAM sFile UNFORMATTED    
      lcDescription CHR(9)
      iiQuantity    SKIP.
      
   IF iiType NE 999 THEN DO: 
      fPrintDetailRow(iiType).
      fPrintCVSQuantity(icCategory,lcDescription,iiQuantity).
   END.


END FUNCTION.
 

/******** Main start ******/      

RUN pInitialize.

RUN pCollectSubscriptions.
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

RUN pCollectCustomers.

RUN pPrintReport.

RETURN "".

/******** Main end  ******/


PROCEDURE pInitialize:

   DEF VAR liPos AS INT  NO-UNDO.
   DEF VAR lcPlainFile AS CHAR NO-UNDO.
   
   lcTransDir = "".
   IF NUM-ENTRIES(icFile,"*") > 3 THEN DO:
      ASSIGN
         llAppend   = (ENTRY(1,icFile,"*") = "append")
         llTransDir = (ENTRY(2,icFile,"*") = "trans")
         lcTransDir = ENTRY(3,icFile,"*")
         icFile     = ENTRY(4,icFile,"*").
   END.
   ELSE ASSIGN
      llAppend   = FALSE
      llTransDir = TRUE.

   IF llTransDir AND lcTransDir = "" THEN DO: 
      IF icRunMode = "test" THEN lcTransDir = fCParamC("FRTestRunDir").
      ELSE lcTransDir = fCParamC("UnbilledSubsTransDir").
      IF lcTransDir = ? THEN lcTransDir = "".
   END.
      
   ASSIGN 
      lcFile         = REPLACE(icFile,"#PERIOD",STRING(iiPeriod,"999999"))
      lcFile         = REPLACE(lcFile,"#INVDATE",STRING(idaInvDate1,"999999"))
      lcFile         = REPLACE(lcFile,"#MODE",
                                      STRING(ilDetails,"detailed/summary"))
      ldaFromPeriod  = fInt2Date(iiPeriod,1)
      ldaToPeriod    = fInt2Date(iiPeriod,2)
      ldFromPeriod   = fMake2DT(ldaFromPeriod,0)
      ldToPeriod     = fMake2DT(ldaToPeriod,86399).

   IF NUM-ENTRIES(lcFile,"/") > 1 THEN ASSIGN
      lcPlainFile = ENTRY(NUM-ENTRIES(lcFile,"/"),lcFile,"/")
      liPos       = INDEX(lcFile,lcPlainFile)
      lcCVSFile   = SUBSTRING(lcFile,1,liPos - 1).
   ELSE ASSIGN
      lcPlainFile = lcFile
      lcCVSFile   = "".

   ASSIGN
      lcCVSFile = lcCVSFile + "cvs_" + STRING(iiPeriod,"999999") + "_" +
                  lcPlainFile 
      lcCVSTransDir = IF icRunMode = "test"
                      THEN fCParamC("FRTestRunDir")
                      ELSE fCParamC("PentahoBillingReport").
      ldaBillPeriodEnd =  ldaToPeriod.

   FOR FIRST ReportConf NO-LOCK WHERE
             ReportConf.Brand    = gcBrand AND
             ReportConf.ReportID = "UnbilledSubsQty":

      FOR EACH ReportConfRow OF ReportConf NO-LOCK WHERE
               ReportConfRow.RowType = "ReportItem":
         CREATE ttDetails.
         ASSIGN 
            ttDetails.RowType     = ReportConfRow.IntValue
            ttDetails.Description = ReportConfRow.CharValue
            ttDetails.DispDetails = ReportConfRow.LogicValue.
      END.
   END.
 
END PROCEDURE.

PROCEDURE pCollectSubscriptions:

   DEF VAR llTerminated  AS LOG  NO-UNDO.
   DEF VAR llNoUsage     AS LOG  NO-UNDO.
   DEF VAR ldFirstSubs   AS DEC  NO-UNDO.
   
   DEF BUFFER bOwner FOR MsOwner.
   
   /* first get billed ones */
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand = gcBrand        AND
            Invoice.InvDate >= idaInvDate1 AND
            Invoice.InvDate <= idaInvDate2 AND
            Invoice.InvType  = 1,
       EACH SubInvoice OF Invoice NO-LOCK:
      CREATE ttBilled.
      ASSIGN 
         ttBilled.MsSeq   = SubInvoice.MsSeq
         ttBilled.InvCust = Invoice.CustNum.
   END.
             
   /* postpaid subscriptions that were open in the beginning of 
      billing period */
   OpenSubscriptions:
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.Brand   = gcBrand AND
            MsOwner.PayType = FALSE   AND
            MsOwner.TsEnd  >= ldFromPeriod AND
            MsOwner.TsBeg  <= ldToPeriod:

      &IF "{&TraceLog}" = "YES" 
      &THEN
      fLogBasic("CLI:" + string(msowner.msseq)).
      &ENDIF
        
      IF CAN-FIND(FIRST ttHandled WHERE
                        ttHandled.MsSeq   = MsOwner.MsSeq AND
                        ttHandled.InvCust = MsOwner.InvCust)
      THEN NEXT.

      CREATE ttHandled.
      ASSIGN 
         ttHandled.MsSeq   = MsOwner.MsSeq
         ttHandled.InvCust = MsOwner.InvCust
         oiSubsCount       = oiSubsCount + 1.
 
      liOpenSubs = liOpenSubs + 1.

      IF NOT SESSION:BATCH AND liOpenSubs MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP liOpenSubs  
                 LABEL "Subscription Qty" 
                 FORMAT ">>>>>>>9"
         WITH SIDE-LABELS 1 DOWN ROW 10 CENTERED OVERLAY 
            TITLE " Open Subscriptions " FRAME fOpenSubs.
      END.

      IF CAN-FIND(FIRST ttBilled WHERE
                        ttBilled.MsSeq   = MsOwner.MsSeq AND
                        ttBilled.InvCust = MsOwner.InvCust) THEN DO:
         liBilledSubs = liBilledSubs + 1.
         NEXT OpenSubscriptions.
      END.

      /* billing denied */
      FOR FIRST Limit NO-LOCK USE-INDEX MsSeq WHERE
                Limit.MsSeq     = MsOwner.MsSeq   AND
                Limit.LimitType = 3               AND
                Limit.TMRuleSeq = 0               AND
                Limit.ToDate   >= idaInvDate1     AND
                Limit.FromDate <= idaInvDate2     AND
                Limit.LimitID   = 0               AND
                Limit.CustNum   = MsOwner.InvCust AND
                Limit.LimitAmt > 0:

         IF Limit.LimitAmt = 1 THEN DO:
            liSuspended  = liSuspended + 1.
            IF ilDetails THEN fAddDetailRow(201).                    
         END.      
         ELSE DO:
            IF MsOwner.TSEnd < ldToPeriod AND 
               NOT CAN-FIND(FIRST bOwner USE-INDEX InvCust WHERE
                                  bOwner.InvCust = MsOwner.InvCust AND
                                  bOwner.MsSeq   = MsOwner.MsSeq   AND
                                  bOwner.TsEnd   > ldToPeriod      AND
                                  bOwner.PayType = FALSE)
            THEN ASSIGN
               liProhibitedTerm   = liProhibitedTerm + 1
               liRowType          = 203.
            ELSE ASSIGN
               liProhibitedActive = liProhibitedActive + 1
               liRowType          = 202.

            IF ilDetails THEN fAddDetailRow(liRowType).                    
         END.
         
         NEXT OpenSubscriptions.
      END.   

      ASSIGN
         llTerminated = FALSE
         llNoUsage    = TRUE.
        
      /* terminated on 1. of billing period */
      IF TRUNCATE(MsOwner.TsEnd,0) = TRUNCATE(ldFromPeriod,0) THEN DO:  
      
         IF NOT CAN-FIND(FIRST bOwner USE-INDEX InvCust WHERE
                               bOwner.InvCust = MsOwner.InvCust AND
                               bOwner.MsSeq   = MsOwner.MsSeq   AND
                               bOwner.TsEnd   > MsOwner.TsEnd   AND
                               bOwner.TsBeg  <= ldToPeriod      AND
                               bOwner.PayType = FALSE)
         THEN llTerminated = TRUE.
      END.
      
      /* usage on billing period */
      FOR EACH InvSeq NO-LOCK WHERE
               InvSeq.MsSeq     = MsOwner.MsSeq AND
               InvSeq.FromDate <= ldaToPeriod   AND
               InvSeq.Billed    = FALSE,
         FIRST MobCDR NO-LOCK WHERE
               MobCDR.InvCust = InvSeq.CustNum AND
               MobCDR.InvSeq  = InvSeq.InvSeq  AND
               ROUND(MobCDR.Amount,2) > 0      AND 
               MobCDR.ReadInTS <= ldToPeriod:
         llNoUsage = FALSE.
         LEAVE. 
      END.

      IF llTerminated AND llNoUsage THEN DO:
      
         /* stc from postpaid to prepaid */
         IF CAN-FIND(FIRST bOwner USE-INDEX InvCust WHERE
                           bOwner.InvCust = MsOwner.InvCust AND
                           bOwner.MsSeq   = MsOwner.MsSeq   AND
                           bOwner.TsBeg  >= MsOwner.TsEnd   AND
                           bOwner.PayType = TRUE)
         THEN DO:
            liSTCOnFirst = liSTCOnFirst + 1.
            IF ilDetails THEN fAddDetailRow(303).            
         END.
         ELSE DO:
            liTerminatedOnFirst = liTerminatedOnFirst + 1.
            IF ilDetails THEN fAddDetailRow(302).                 
         END.
               
         NEXT OpenSubscriptions.
      END.
      
      ELSE IF llNoUsage THEN DO:
      
         ldFirstSubs = MsOwner.TsBeg.
         
         FOR EACH bOwner NO-LOCK WHERE  
                  bOwner.MsSeq   = MsOwner.MsSeq AND
                  bOwner.PayType = FALSE:
            ldFirstSubs = MIN(ldFirstSubs,bOwner.TsBeg).
         END.
         
         IF ldFirstSubs >= ldFromPeriod THEN DO:
            liFirstMonthNoUsage = liFirstMonthNoUsage + 1.         
            IF ilDetails THEN fAddDetailRow(301).   
            NEXT OpenSubscriptions.
         END.
      END.   
            
      /* IF ilDetails THEN */ fAddDetailRow(402).                    
      
      IF iiUpdateInterval > 0 AND oiSubsCount MOD iiUpdateInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(iiFRProcessID,oiSubsCount) THEN
            RETURN "ERROR:Stopped".
      END.   
       
   END.

   IF NOT SESSION:BATCH THEN HIDE FRAME fOpenSubs NO-PAUSE.
    
   RETURN "".
   
END PROCEDURE. /* pCollectUnbilled */

PROCEDURE pCollectCustomers:

   IF NOT SESSION:BATCH THEN DO:
      PAUSE 0.
      DISP liUnbilledCust  
         LABEL "Customer Qty" 
         FORMAT ">>>>>>>9"
      WITH SIDE-LABELS 1 DOWN ROW 10 CENTERED OVERLAY 
         TITLE " Unbilled Customers " FRAME fCustomer.
   END.

   FOR EACH SingleFee NO-LOCK USE-INDEX InvNum_s WHERE
            SingleFee.InvNum = 0     AND
            SingleFee.Billed = FALSE AND
            SingleFee.Active = TRUE  AND
            SingleFee.BillPeriod <= iiPeriod AND
            SingleFee.HostTable   = "Customer":

      IF NOT fAddCustomerRow(SingleFee.CustNum) THEN NEXT.

      IF NOT SESSION:BATCH THEN DO:
         PAUSE 0.
         DISP liUnbilledCust WITH FRAME fCustomer.
      END.
   END.
   
   FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = gcBrand AND
            FixedFee.HostTable = "Customer" AND
            FixedFee.InUse     = TRUE,
      FIRST FFItem OF FixedFee NO-LOCK WHERE
            FFItem.Billed   = FALSE AND 
            FFItem.BillPer <= iiPeriod:
            
      IF NOT fAddCustomerRow(FixedFee.CustNum) THEN NEXT.

      IF NOT SESSION:BATCH THEN DO:
         PAUSE 0.
         DISP liUnbilledCust WITH FRAME fCustomer.
      END.
   END.

   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.MsSeq   = 0 AND
            InvSeq.Billed  = FALSE AND
            InvSeq.ToDate <= ldaToPeriod,
      FIRST MobCDR NO-LOCK WHERE
            MobCDR.InvCust = InvSeq.CustNum AND
            MobCDR.InvSeq  = InvSeq.InvSeq  AND
            MobCDR.Amount  > 0:
      
      IF NOT fAddCustomerRow(InvSeq.CustNum) THEN NEXT. 

      IF NOT SESSION:BATCH THEN DO:
         PAUSE 0.
         DISP liUnbilledCust WITH FRAME fCustomer.
      END.
   END.         

   IF NOT SESSION:BATCH THEN HIDE FRAME fCustomer NO-PAUSE.
            
END PROCEDURE. /* pCollectCustomers */

PROCEDURE pPrintReport:

   DEF VAR liCnt          AS INT  NO-UNDO.
   DEF VAR lcDescription  AS CHAR NO-UNDO.
   DEF VAR liSection      AS INT  NO-UNDO.
   DEF VAR lcFieldName    AS CHAR NO-UNDO.
   DEF VAR liUnclearSubs  AS INT  NO-UNDO.
   DEF VAR liUnclearCust  AS INT  NO-UNDO.
   DEF VAR liUnbilledSubs AS INT  NO-UNDO.
   DEF VAR lcFinalFile    AS CHAR NO-UNDO.
   DEF VAR lcCategory     AS CHAR NO-UNDO. 

   IF llAppend THEN DO:
      OUTPUT STREAM sFile TO VALUE(lcFile) APPEND.
      PUT STREAM sFile SKIP(3).
      OUTPUT STREAM sCVSFile TO VALUE(lcCVSFile) APPEND.
   END.
   ELSE DO:
     OUTPUT STREAM sFile TO VALUE(lcFile).
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
          "DueDate"  SKIP.

   END.

   liUnbilledSubs = liOpenSubs - liBilledSubs.
   
   PUT STREAM sFile UNFORMATTED
      "UNBILLED CUSTOMERS AND SUBSCRIPTIONS" SKIP
      "Reporting date"           CHR(9)
         STRING(TODAY,"99.99.9999") SKIP
      "Billing Period"  CHR(9) 
         iiPeriod          SKIP
      "Invoice Dates"   CHR(9)
         STRING(idaInvDate1,"99.99.9999") "-" 
         STRING(idaInvDate2,"99.99.9999") SKIP(1)
      
      "UNBILLED TARGET"   SKIP
      "Customer"       CHR(9)
         liUnbilledCust   SKIP
      "Subscription"   CHR(9)
         liUnbilledSubs   SKIP
      "TOTAL"          CHR(9)
         liUnbilledCust + liUnbilledSubs SKIP(1).

   fPrintCVSQuantity ("UNBILLED TARGET","Customer",liUnbilledCust).
   fPrintCVSQuantity ("UNBILLED TARGET","Subscription",liUnbilledSubs).

   lcCategory = "SUBSCRIPTIONS' BILLING PERMISSION STATUS".
   PUT STREAM sFile UNFORMATTED lcCategory SKIP.
   fPrintQuantity(201,liSuspended,lcCategory).   
   fPrintQuantity(202,liProhibitedActive,lcCategory).
   fPrintQuantity(203,liProhibitedTerm,lcCategory).
   fPrintQuantity(999,liSuspended + liProhibitedActive + liProhibitedTerm,lcCategory).
   
   lcCategory = "PREVENTED MINIMUM CONSUMPTION CALCULATION".
   PUT STREAM sFile UNFORMATTED
      SKIP(1) lcCategory SKIP.
      
   fPrintQuantity(301,liFirstMonthNoUsage,lcCategory).      
   fPrintQuantity(302,liTerminatedOnFirst,lcCategory).
   fPrintQuantity(303,liSTCOnFirst,lcCategory).
   fPrintQuantity(999,liFirstMonthNoUsage + liTerminatedOnFirst + 
                      liSTCOnFirst,lcCategory).
   ASSIGN 
      liUnclearSubs = liUnbilledSubs - liFirstMonthNoUsage - 
                      liTerminatedOnFirst - liSTCOnFirst - 
                      liProhibitedActive - liProhibitedTerm - liSuspended 
      liUnclearCust = liUnbilledCust.
                      
   lcCategory = "UNCLEAR".
   PUT STREAM sFile UNFORMATTED
      SKIP(1) lcCategory SKIP.
      
   fPrintQuantity(401,liUnclearCust,lcCategory).   
   fPrintQuantity(402,liUnclearSubs,lcCategory).      
   fPrintQuantity(999,liUnclearCust + liUnclearSubs,lcCategory).      

   OUTPUT STREAM sFile CLOSE.
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
         FuncRunResult.ResultOrder = 2
         FuncRunResult.CharParam   = lcFinalFile.
   END.

   /* move CVS report */
   IF lcCVSTransDir > "" AND llTransDir THEN DO:
      lcFinalFile = fMove2TransDir(lcCVSFile,
                                   ".txt",
                                   lcCVSTransDir).
   END. 


END PROCEDURE. /* pPrintReport */

