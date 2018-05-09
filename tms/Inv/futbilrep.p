/* ------------------------------------------------------
  MODULE .......: FUTBILREP
  FUNCTION .....: List of future billing
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 12.02.03
  MODIFIED .....: 08.05.03/aam optionally leave out banned invgroups
                  22.09.03/aam brand
                  31.12.03/aam Customer.VatUsage, fVatFactor()
                  30.03.04 kl  fixes after index changes

  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/fcurrency.i}
{Func/fvatfact.i}

DEF INPUT  PARAMETER icInvGrp1    AS CHAR  NO-UNDO. 
DEF INPUT  PARAMETER icInvGrp2    AS CHAR  NO-UNDO. 
DEF INPUT  PARAMETER ilBanned     AS LOGIC NO-UNDO. 
DEF INPUT  PARAMETER ilContract   AS LOGIC NO-UNDO.
DEF INPUT  PARAMETER ilSingle     AS LOGIC NO-UNDO.
DEF INPUT  PARAMETER ilMobCalls   AS LOGIC NO-UNDO.
DEF INPUT  PARAMETER ilFixCalls   AS LOGIC NO-UNDO.
DEF INPUT  PARAMETER icFile       AS CHAR  NO-UNDO.
DEF OUTPUT PARAMETER oiCount      AS INT   NO-UNDO. 

DEF VAR lcMonths   AS CHAR NO-UNDO.
DEF VAR liCount    AS INT  NO-UNDO. 
DEF VAR liYear     AS INT  NO-UNDO. 
DEF VAR liMonth    AS INT  NO-UNDO. 
DEF VAR lcHeader   AS CHAR NO-UNDO EXTENT 12. 
DEF VAR lcNumMem   AS CHAR NO-UNDO.
DEF VAR liQty      AS INT  NO-UNDO. 
DEF VAR lcColl     AS CHAR NO-UNDO. 
DEF VAR liBegPer   AS INT  NO-UNDO.
DEF VAR liEndPer   AS INT  NO-UNDO. 
DEF VAR liPeriod   AS INT  NO-UNDO. 
DEF VAR liSecPer   AS INT  NO-UNDO.
DEF VAR liCustPer  AS INT  NO-UNDO. 
DEF VAR liAccount  AS INT  NO-UNDO. 
DEF VAR llVatIncl  AS LOG  NO-UNDO.
DEF VAR ldVatPerc  AS DEC  NO-UNDO. 
DEF VAR ldtPrevMth AS DATE NO-UNDO. 
DEF VAR llCustCnt  AS LOG  NO-UNDO. 
DEF VAR ldNet      AS DEC  NO-UNDO.
DEF VAR ldGross    AS DEC  NO-UNDO. 
DEF VAR lcTaxZone  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttAcc NO-UNDO
   FIELD Account  AS INT
   FIELD Prod     AS CHAR
   FIELD ProdName AS CHAR
   FIELD Interval AS CHAR
   FIELD Period   AS INT
   FIELD Qty      AS INT
   FIELD Amount   AS DEC 
   INDEX Account Account Prod Interval Period. 

DEF STREAM sLog.

FORM 
   liQty  AT 2 LABEL "Qty" FORMAT ">>>>>>>>9"
   WITH SIDE-LABELS OVERLAY ROW 10 CENTERED TITLE " Customers "
        FRAME fColl. 


FUNCTION fCollAmt RETURNS LOGICAL
   (icProd     AS CHAR,
    icInterval AS CHAR,
    iiPeriod   AS INT,
    idAmount   AS DEC,
    ilVatIncl  AS LOG,
    iiVatUsage AS INT,
    icTaxZone  AS CHAR).

   DEF VAR ldVatFactor AS DEC NO-UNDO.
   
   IF iiPeriod > liEndPer OR
      idAmount = 0 
   THEN RETURN FALSE. 

   FIND BillItem NO-LOCK WHERE 
        BillItem.Brand    = Syst.Var:gcBrand AND
        BillItem.BillCode = icProd NO-ERROR.
   IF AVAILABLE BillItem THEN 
       FIND FIRST CCRule NO-LOCK WHERE 
                  CCRule.Brand      = BillItem.Brand    AND 
                  CCRule.Category   = "*"               AND 
                  CCRule.BillCode   = BillItem.BillCode AND   
                  CCRule.CLIType    = ""                AND                 
                  CCRule.ValidTo    >= TODAY  NO-ERROR. 
                  
   liAccount = IF AVAILABLE CCRule THEN CCRule.AccNum ELSE 0. 

   FIND FIRST ttAcc WHERE 
              ttAcc.Account  = liAccount  AND
              ttAcc.Prod     = icProd     AND
              ttAcc.Interval = icInterval AND
              ttAcc.Period   = iiPeriod NO-ERROR.

   IF NOT AVAILABLE ttAcc THEN DO:
      CREATE ttAcc.
      ASSIGN ttAcc.Account  = liAccount 
             ttAcc.Prod     = icProd   
             ttAcc.ProdName = IF AVAILABLE BillItem
                              THEN BillItem.BIName
                              ELSE "Unknown"
             ttAcc.Interval = icInterval
             ttAcc.Period   = iiPeriod.
   END. 

   /* remove possible vat */
   IF ilVatIncl THEN DO:
      ldVatFactor = fVatFactor(iiVatUsage,
                               icTaxZone, 
                               icProd,
                               TODAY).
      idAmount = idAmount / ldVatFactor.
   END.
   
   ASSIGN ttAcc.Amount = ttAcc.Amount + idAmount
          ttAcc.Qty    = ttAcc.Qty + 1.

   IF llCustCnt THEN ASSIGN
      oiCount   = oiCount + 1
      llCustCnt = FALSE. 

   RETURN TRUE. 

END FUNCTION.


ASSIGN lcNumMem = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "european".

OUTPUT STREAM sLog TO VALUE(icFile).


ASSIGN lcMonths   = "January,February,March,April,May,June,July,August," +
                    "September,October,November,December"
       liYear     = YEAR(TODAY)
       liMonth    = MONTH(TODAY)
       /* end of previous month -> billing period of calls from this */ 
       ldtPrevMth = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1. 

/* next 12 months from today onwards (this month is first) */
DO liCount = 1 TO 12:

   CASE liCount:
   WHEN 1  THEN liBegPer  = liYear * 100 + liMonth.
   WHEN 2  THEN liSecPer  = liYear * 100 + liMonth.
   WHEN 12 THEN liEndPer  = liYear * 100 + liMonth. 
   END CASE. 

   ASSIGN lcHeader[liCount] = ENTRY(liMonth,lcMonths) + " " + STRING(liYear)
          liMonth           = liMonth + 1.

   IF liMonth > 12 THEN ASSIGN 
      liMonth = 1
      liYear  = liYear + 1.
END.     

PUT STREAM sLog UNFORMATTED
   "Account"        TAB
   "Product"        TAB
   "Name"           TAB
   "Interval"       TAB.

DO liCount = 1 TO 12:
   PUT STREAM sLog UNFORMATTED
      lcHeader[liCount] TAB.
END.

PUT STREAM sLog MY-NL. 


FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand     = Syst.Var:gcBrand   AND
         InvGroup.InvGroup >= icInvGrp1 AND
         InvGroup.InvGroup <= icInvGrp2 AND
         (IF NOT ilBanned 
          THEN InvGroup.Banned   = FALSE AND
               InvGroup.BillPerm = TRUE
          ELSE TRUE),
    EACH Customer NO-LOCK WHERE
         Customer.Brand    = InvGroup.Brand AND
         Customer.InvGroup = InvGroup.InvGroup:

   ASSIGN liQty     = liQty + 1
          llCustCnt = TRUE.

   IF liQty MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liQty WITH FRAME fColl.
   END. 

   liCustPer = liBegPer.
   /* has customer already been billed in this month */
   FOR FIRST Invoice OF Customer NO-LOCK WHERE
             Invoice.InvDate > ldtPrevMth AND
             Invoice.CrInvNum = 0:
      liCustPer = liSecPer.
   END. 

   lcTaxZone = fRegionTaxZone(Customer.Region).

   IF ilContract THEN 
   FOR EACH FixedFee NO-LOCK WHERE 
            FixedFee.CustNum = Customer.CustNum AND 
            FixedFee.InUse   = TRUE,
       EACH FFItem OF FixedFee NO-LOCK WHERE
            FFItem.Billed      = FALSE   AND
            FFItem.BillPeriod <= liEndPer:

      fCollAmt(FixedFee.BillCode,
               STRING(FixedFee.Interval),
               /* all older, unbilled events will be billed in the next run 
                  -> set customer's next billing period as billing period */
               MAX(liCustPer,FFItem.BillPeriod),
               FFItem.Amt,
               FixedFee.VatIncl,
               Customer.VatUsage,
               lcTaxZone). 

   END. /* contract */

   IF ilSingle THEN
   FOR EACH SingleFee NO-LOCK WHERE 
            SingleFee.CustNum     = Customer.CustNum AND
            SingleFee.Billed      = FALSE            AND
            SingleFee.BillPeriod <= liEndPer:

      fCollAmt(SingleFee.BillCode,
               "S",
               /* all older, unbilled events will be billed in the next run 
                  -> set customer's next billing period as billing period */
               MAX(liCustPer,SingleFee.BillPeriod),
               SingleFee.Amt,
               SingleFee.VatIncl,
               Customer.VatUsage,
               lcTaxZone). 

   END. /* single fees */

   IF ilMobCalls OR ilFixCalls THEN
   FOR EACH InvSeq NO-LOCK WHERE 
            InvSeq.CustNum = Customer.CustNum AND
            InvSeq.billed  = FALSE:

      /* all older, unbilled events will be billed in the next run 
         -> simple logic; all calls made this month will be billed 
         during next month, all others will be billed this month
         (unless customer has already been billed this month)
      */
      IF InvSeq.ToDate > ldtPrevMth 
      THEN liPeriod = liSecPer.
      ELSE liPeriod = liCustPer. 

      IF ilMobCalls THEN 
      FOR EACH MobCDR NO-LOCK WHERE
               MobCDR.CustNum = InvSeq.CustNum AND
               MobCDR.InvSeq  = InvSeq.InvSeq:

         /* is currency unit full or sub (assign ldnet & ldgross) */
         fCurrUnit(MobCDR.Amount,
                   MobCDR.Amount,
                   MobCDR.CurrUnit,
                   "",
                   MobCDR.TariffNum,
                   Syst.Var:gcBrand,
                   OUTPUT ldNet,
                   OUTPUT ldGross).

         fCollAmt(MobCDR.BillCode,
                  "",
                  liPeriod,
                  ldNet, 
                  MobCDR.VatIncl,
                  Customer.VatUsage,
                  lcTaxZone).
      END. 

      IF ilFixCalls THEN 
      FOR EACH FixCDR NO-LOCK WHERE
               FixCDR.InvSeq = InvSeq.InvSeq:

         /* is currency unit full or sub (assign ldnet & ldgross) */
         fCurrUnit(FixCDR.GrossPrice - FixCDR.DiscValue,
                   FixCDR.GrossPrice,
                   FixCDR.CurrUnit,
                   "",
                   FixCDR.TariffID,
                   Syst.Var:gcBrand,
                   OUTPUT ldNet,
                   OUTPUT ldGross).

         fCollAmt(FixCDR.BillCode,
                  "",
                  liPeriod,
                  ldNet,
                  FixCDR.VATIncl,
                  Customer.VatUsage,
                  lcTaxZone). 
                  
      END. 


   END. /* InvSeq */

END.  /* invgroup / customer */

HIDE FRAME fColl NO-PAUSE. 

/* print collected events */
FOR EACH ttAcc
BREAK BY ttAcc.Account
      BY ttAcc.Prod
      BY ttAcc.Interval 
      BY ttAcc.Period:

   IF FIRST-OF(ttAcc.Interval) THEN 
      PUT STREAM sLog UNFORMATTED
         ttAcc.Account  TAB
         ttAcc.Prod     TAB
         ttAcc.ProdName TAB
         ttAcc.Interval TAB.

   PUT STREAM sLog UNFORMATTED
      ROUND(ttAcc.Amount,2) TAB. 

   IF LAST-OF(ttAcc.Interval) THEN 
      PUT STREAM sLog MY-NL. 

END.      

OUTPUT STREAM sLog CLOSE.


