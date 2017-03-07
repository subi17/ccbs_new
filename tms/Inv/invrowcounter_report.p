/* ----------------------------------------------------------------------
  MODULE .......: invrowcounter_report.p
  TASK .........: Print a report from invoice row counters
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 05.11.10
  Version ......: yoigo
---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}

DEF INPUT  PARAMETER idaFromDate      AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate        AS DATE NO-UNDO.
DEF INPUT  PARAMETER icExtInvID       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiInvCust        AS INT  NO-UNDO.
DEF INPUT  PARAMETER icCLI            AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icBillCode       AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiCCN            AS INT  NO-UNDO.
DEF INPUT  PARAMETER icBilled         AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icFile           AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiCount          AS INT  NO-UNDO.

DEF VAR lcFile       AS CHAR NO-UNDO.
DEF VAR lcTransDir   AS CHAR NO-UNDO.
DEF VAR ldaPeriodBeg AS DATE NO-UNDO.
DEF VAR ldaPeriodEnd AS DATE NO-UNDO.

DEF TEMP-TABLE ttCounter NO-UNDO
   FIELD ExtInvID AS CHAR 
   FIELD InvCust  AS INT
   FIELD CLI      AS CHAR
   FIELD BillCode AS CHAR
   FIELD CCN      AS INT
   FIELD Quantity AS INT
   FIELD Duration AS INT
   FIELD DataAmt  AS DEC
   FIELD Amount   AS DEC
   FIELD VatIncl  AS LOG
   INDEX ExtInvID ExtInvID InvCust CLI BillCode CCN.

DEF STREAM sFile.


FUNCTION fCollect2TempTable RETURNS LOGIC
   (icExtInvID AS CHAR):

   IF (icBilled = "billed" AND InvRowCounter.InvNum = 0) OR
      (icBilled = "unbilled" AND InvRowCounter.InvNum > 0) THEN
      RETURN FALSE.
      
   IF icBillCode > "" AND InvRowCounter.BillCode NE icBillCode THEN 
      RETURN FALSE.
      
   IF iiCCN > 0 AND InvRowCounter.CCN NE iiCCN THEN
      RETURN FALSE. 
   
   FIND FIRST ttCounter WHERE
              ttCounter.ExtInvID = icExtInvID AND
              ttCounter.InvCust  = InvRowCounter.InvCust AND
              ttCounter.CLI      = InvRowCounter.CLI AND
              ttCounter.BillCode = InvRowCounter.BillCode AND
              ttCounter.CCN      = InvRowCounter.CCN AND
              ttCounter.VatIncl  = InvRowCounter.VatIncl NO-ERROR.
   IF NOT AVAILABLE ttCounter THEN DO:
      CREATE ttCounter.
      ASSIGN 
         ttCounter.ExtInvID = icExtInvID 
         ttCounter.InvCust  = InvRowCounter.InvCust 
         ttCounter.CLI      = InvRowCounter.CLI 
         ttCounter.BillCode = InvRowCounter.BillCode 
         ttCounter.CCN      = InvRowCounter.CCN
         ttCounter.VatIncl  = InvRowCounter.VatIncl.
   END.
           
   ASSIGN 
      ttCounter.Quantity = ttCounter.Quantity + InvRowCounter.Quantity
      ttCounter.Duration = ttCounter.Duration + InvRowCounter.Duration
      ttCounter.DataAmt  = ttCounter.DataAmt + InvRowCounter.DataAmt
      ttCounter.Amount   = ttCounter.Amount + InvRowCounter.Amount
      oiCount            = oiCount + 1.

   IF ldaPeriodBeg = ? THEN ASSIGN
      ldaPeriodBeg = InvRowCounter.FromDate
      ldaPeriodEnd = InvRowCounter.ToDate.
   ELSE ASSIGN
      ldaPeriodBeg = MIN(ldaPeriodBeg,InvRowCounter.FromDate)
      ldaPeriodEnd = MAX(ldaPeriodEnd,InvRowCounter.ToDate).
   
   RETURN TRUE.
   
END FUNCTION.
   
FUNCTION fGetExtInvID RETURN CHAR
   (iiInvNum AS INT):
 
   IF iiInvNum > 0 THEN DO:
      FIND FIRST Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.
      IF AVAILABLE Invoice THEN RETURN Invoice.ExtInvID.
   END.
   
   RETURN "".
    
END FUNCTION.


/******** Main start ******/      

RUN pInitialize.
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

RUN pCollectCounters.
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.

RUN pPrintReport.

RUN pFinalize.

RETURN "".

/******** Main end  ******/


PROCEDURE pInitialize:

   DEF VAR liPeriod AS INT  NO-UNDO.

   lcTransDir = "".
   IF NUM-ENTRIES(icFile,"*") > 1 THEN ASSIGN 
      lcTransDir = ENTRY(1,icFile)
      icFile     = ENTRY(2,icFile).
   ELSE lcTransDir = fCParamC("IRCounterRepTransDir").

   IF lcTransDir = ? THEN lcTransDir = "".

   lcFile = icFile.
   IF INDEX(lcFile,"#PERIOD") > 0 THEN ASSIGN 
      liPeriod = YEAR(idaToDate) * 100 + MONTH(idaToDate)
      lcFile   = REPLACE(lcFile,"#PERIOD",STRING(liPeriod,"999999")).
  
   RETURN "".
   
END PROCEDURE.

PROCEDURE pCollectCounters:

   DEF VAR lcExtInvID AS CHAR NO-UNDO.
   
   IF icExtInvID > "" THEN DO:
   
      FIND FIRST Invoice WHERE 
                 Invoice.Brand = gcBrand AND
                 Invoice.ExtInvID = icExtInvID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice THEN 
         RETURN "ERROR:Unknown invoice".
         
      FOR EACH SubInvoice NO-LOCK WHERE
               SubInvoice.InvNum = Invoice.InvNum,
          EACH InvRowCounter NO-LOCK WHERE
               InvRowCounter.InvCust = Invoice.CustNum AND
               InvRowCounter.InvSeq  = SubInvoice.InvSeq AND
               InvRowCounter.ToDate   >= idaFromDate AND
               InvRowCounter.FromDate <= idaToDate:
               
         IF icCLI > "" AND InvRowCounter.CLI NE icCLI THEN NEXT.
         
         fCollect2TempTable(Invoice.ExtInvID).
      END.
   END.
   
   ELSE IF icCLI > "" THEN DO:
      FOR EACH MsOwner NO-LOCK WHERE
               MsOwner.CLI = icCLI
      BREAK BY MsOwner.MsSeq:
      
         IF FIRST-OF(MsOwner.MsSeq) THEN
         FOR EACH InvRowCounter NO-LOCK WHERE
                  InvRowCounter.MsSeq     = MsOwner.MsSeq AND
                  InvRowCounter.ToDate   >= idaFromDate AND
                  InvRowCounter.FromDate <= idaToDate:
                  
            IF iiInvCust > 0 AND InvRowCounter.InvCust NE iiInvCust THEN NEXT.

            lcExtInvID = fGetExtInvID(InvRowCounter.InvNum).
                       
            fCollect2TempTable(lcExtInvID).
         END.   
      END.
   END.
   
   ELSE IF iiInvCust > 0 THEN DO:
      FOR EACH InvRowCounter NO-LOCK WHERE
               InvRowCounter.InvCust   = iiInvCust AND
               InvRowCounter.ToDate   >= idaFromDate AND
               InvRowCounter.FromDate <= idaToDate:

         lcExtInvID = fGetExtInvID(InvRowCounter.InvNum).
         
         fCollect2TempTable(lcExtInvID).
      END.
   END.

   RETURN "".
    
END PROCEDURE.

PROCEDURE pPrintReport:

   OUTPUT STREAM sFile TO VALUE(lcFile).

   PUT STREAM sFile UNFORMATTED
      "Period"     CHR(9)
      ldaPeriodBeg CHR(9)
      "-"          CHR(9)
      ldaPeriodEnd SKIP
   "Invoice"      CHR(9)
   "Inv.Customer" CHR(9)
   "MSISDN"       CHR(9)
   "Billing Item" CHR(9)
   "CCN"          CHR(9)
   "Quantity"     CHR(9)
   "Duration"     CHR(9)
   "Data Mb"      CHR(9)
   "Amount"       CHR(9)
   "Tax"          SKIP.
   
   FOR EACH ttCounter:
   
      PUT STREAM sFile UNFORMATTED
         ttCounter.ExtInvID  CHR(9)
         ttCounter.InvCust   CHR(9)
         ttCounter.CLI       CHR(9)
         ttCounter.BillCode  CHR(9)
         ttCounter.CCN       CHR(9)
         ttCounter.Quantity  CHR(9)
         ttCounter.Duration  CHR(9)
         TRIM(STRING(ttCounter.DataAmt / (1024 * 1024),
              "->>>>>>>>>>9.99")) CHR(9)
         TRIM(STRING(ttCounter.Amount,"->>>>>>>>>9.99<")) CHR(9)
         STRING(ttCounter.VatIncl,"Included/Excluded") SKIP.
   END.
   
   OUTPUT STREAM sFile CLOSE.
   
END PROCEDURE.

PROCEDURE pFinalize:

   IF lcTransDir > "" THEN 
      fTransDir(lcFile,
                "",
                lcTransDir).

END PROCEDURE.


