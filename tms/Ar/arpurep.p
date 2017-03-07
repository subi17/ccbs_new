/* -----------------------------------------------------------------------------
  MODULE .......: ARPUREP
  FUNCTION .....: List average revenue data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 30.08.02
  MODIFIED .....: 19.11.02 lp moved from JG
                  18.02.03/aam mob subscription type from mcdr.CLIType
                  20.03.03/aam VAT handling
                  12.09.03/aam brand
                  31.12.03/aam Customer.VatUsage, fVatFactor()
                  29.04.05/jp  lcperiod -> idtdate
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/utumaa.i}
{Func/fvatfact.i}

DEF INPUT PARAMETER idtDate1     AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtDate2     AS DATE  NO-UNDO.
DEF INPUT PARAMETER icInvGrp1    AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icInvGrp2    AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSubType1   AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSubType2   AS CHAR  NO-UNDO. 

DEF VAR viiva1 AS CHAR format "x(112)" NO-UNDO.
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR sl     AS INT  NO-UNDO.
DEF VAR rl     AS INT  NO-UNDO.
DEF VAR rlx    AS INT  NO-UNDO.
DEF VAR lev    AS INT  init 112  NO-UNDO.
DEF VAR otsi   AS CHAR EXTENT 39 NO-UNDO.   

DEF VAR xOk          AS LOG  NO-UNDO.
DEF VAR xEmpty       AS CHAR NO-UNDO INIT "<EMPTY>".
DEF VAR xSessionNum  AS CHAR NO-UNDO.
DEF VAR xDateHeader  AS CHAR NO-UNDO.
DEF VAR xQty         AS INT  NO-UNDO.

DEF VAR liPeriod     AS INT  NO-UNDO.
DEF VAR ldVatCalc    AS DEC  NO-UNDO. 
DEF VAR llFixInclVat AS LOG  NO-UNDO. 
DEF VAR llInclVat    AS LOG  NO-UNDO. 
DEF VAR ldAmount     AS DEC  NO-UNDO. 
DEF VAR lcSubType    AS CHAR NO-UNDO. 
DEF VAR liSubQty     AS INT  NO-UNDO.
DEF VAR liSubTotQty  AS INT  NO-UNDO. 

DEF VAR lcPeriod1    AS CHAR NO-UNDO.
DEF VAR lcPeriod2    AS CHAR NO-UNDO.
DEF VAR lcTaxZone    AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttBal NO-UNDO
    FIELD SubType  AS CHAR
    FIELD Prod     AS CHAR
    FIELD Asub     AS CHAR
    FIELD Amt      AS DEC
    FIELD VatAmt   AS DEC 
    FIELD Sec      AS INT
    FIELD Qty      AS INT
    FIELD SubQty   AS INT
    INDEX SubType SubType Prod Asub. 

DEF BUFFER bttBal FOR ttBal. 

ASSIGN 
    xSessionNum = SESSION:NUMERIC-FORMAT
    SESSION:NUMERIC-FORMAT = "European"
    viiva1   = fill("=",lev)
    viiva2   = fill("=",lev)
    viiva3   = fill("-",lev)
    viiva4   = fill("-",lev).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(30)" 
      "AVERAGE REVENUE PER USER" at 40
      "Page" at 102  
      sl format "ZZZZ9" skip
   xDateHeader AT 40 FORMAT "X(30)"
      pvm format "99.99.9999" at 103 skip
   viiva2 at 1 skip
   "Subscription type" AT 1
   "Product"           AT 31
   "Subs.qty"          TO 69
   "Amount VAT0"       TO 81
   "Amt with VAT"      TO 94
   "Minutes"           TO 103
   "Qty"               TO 112 SKIP
   viiva3
   with width 112 no-label no-box frame sivuotsi.

FUNCTION fUpdBal RETURNS LOGICAL
   (iSubType AS CHAR,
    iProd    AS CHAR,
    iAsub    AS CHAR).

    FIND FIRST ttBal WHERE
       ttBal.SubType = iSubType AND
       ttBal.Prod    = iProd    AND
       ttBal.Asub    = iAsub    NO-ERROR.
    IF NOT AVAILABLE ttBal THEN DO:
       CREATE ttBal.
       ASSIGN ttBal.SubType = iSubType 
              ttBal.Prod    = iProd
              ttBal.Asub    = iAsub.
    END.

    RETURN TRUE. 

END FUNCTION.


FUNCTION fPrintAmt RETURNS LOGICAL
   (iiSubQty AS INT,
    idAmt    AS DEC,
    idVatAmt AS DEC,
    iiSec    AS INT,
    iiQty    AS INT).

    IF iiSubQty = 0 THEN RETURN FALSE. 

    PUT STREAM tul 
       iiSubQty                 FORMAT ">>>>>>9"     TO 69
       idAmt / iiSubQty         FORMAT "->>>>>>9.99" TO 81
       idVatAmt / iiSubQty      FORMAT "->>>>>>9.99" TO 94
       (iiSec / 60) / iiSubQty  FORMAT "->>>>>9"     TO 103
       iiQty / iiSubQty         FORMAT "->>>>>9"     TO 112
       SKIP.

    RETURN TRUE. 
END FUNCTION.


FUNCTION CheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF rl >= skayt1 - iAddLine THEN DO:
        {Syst/uprfeed.i rl}
        ASSIGN rlx = 0
               sl = sl + 1.
        VIEW STREAM tul frame sivuotsi.  
        ASSIGN rl = 12.
    END.

    RETURN TRUE.
END.

ASSIGN sl          = 1
       rl          = 0
       liSubTotQty = 0
       xDateHeader = STRING(idtDate1,"99.99.9999") + " - " +
                     STRING(idtDate2,"99.99.9999").

FORM "Qty:" AT 2 xQty format ">>,>>>,>>9" 
WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting "
   FRAME fQty.
PAUSE 0.
VIEW FRAME fQty. 

ASSIGN lcPeriod1 = STRING(YEAR(idtDate1),"9999") + 
                   STRING(MONTH(idtDate1),"99")  +
                   STRING(DAY(idtDate1),"99")
       lcPeriod2 = STRING(YEAR(idtDate2),"9999") + 
                   STRING(MONTH(idtDate2),"99")  +
                   STRING(DAY(idtDate2),"99").

/* invoiced data from desired period, fixed calls and mobile calls */
FOR EACH Customer NO-LOCK WHERE
    Customer.Brand     = gcBrand    AND
    Customer.InvGroup >= icInvGrp1  AND
    Customer.InvGroup <= icInvGrp2,
EACH InvSeq NO-LOCK WHERE
    InvSeq.CustNum = Customer.CustNum AND
    InvSeq.FromDate <= idtDate2       AND
    InvSeq.ToDate   >= idtDate1       AND
    InvSeq.Billed = TRUE:

    FIND Invoice WHERE
         Invoice.InvNum = InvSeq.InvNum NO-LOCK.

    lcTaxZone = fRegionTaxZone(Customer.Region).
    
    IF icSubType1 <= "FIXED" AND
       icSubType2 >= "FIXED"
    THEN
    FOR EACH FixCDR NO-LOCK WHERE
       FixCDR.InvSeq  = InvSeq.InvSeq AND
       FixCDR.Date >= idtDate1      AND
       FixCDR.Date <= idtDate2:

       fUpdBal("FIXED",
               FixCDR.BillCode,
               FixCDR.CLI). 

       ldVatCalc = fVatFactor(Customer.VatUsage,
                              lcTaxZone,
                              FixCDR.BillCode,
                              Invoice.Invdate).
                                     
       ASSIGN ldAmount   = FixCDR.GrossPrice - FixCDR.DiscValue
              ttBal.Sec  = ttBal.Sec + FixCDR.Duration
              ttBal.Qty  = ttBal.Qty + 1. 

       IF FixCDR.VatIncl 
       THEN ASSIGN ttBal.VatAmt = ttBal.VatAmt + ldAmount
                   ttBal.Amt    = ttBal.Amt + ldAmount / ldVatCalc.
       ELSE ASSIGN ttBal.Amt    = ttBal.Amt + ldAmount
                   ttBal.VatAmt = ttBal.VatAmt + ldAmount * ldVatCalc.

       ASSIGN xQty = xQty + 1.

       IF xQty MOD 100 = 0 THEN DO:
          PAUSE 0.
          DISPLAY xQty WITH FRAME fQty.
       END.

    END.

    FOR EACH MobCDR NO-LOCK WHERE
       MobCDR.InvCust  = InvSeq.CustNum AND
       MobCDR.InvSeq   = InvSeq.InvSeq  AND
       MobCDR.DateST  >= idtdate1       AND
       MobCDR.Datest  <= idtdate2:
       
       lcSubType = MobCDR.CLIType.

       IF lcSubType < icSubType1 OR
          lcSubType > icSubType2 
       THEN NEXT. 

       fUpdBal(lcSubType,
               MobCDR.BillCode,
               MobCDR.CLI). 

       ASSIGN ttBal.Sec  = ttBal.Sec + MobCDR.billdur
              ttBal.Qty  = ttBal.Qty + 1. 

       ldVatCalc = fVatFactor(Customer.VatUsage,
                              lcTaxZone,
                              MobCDR.BillCode,
                              Invoice.Invdate).
        
       IF MobCDR.VatIncl 
       THEN ASSIGN ttBal.VatAmt = ttBal.VatAmt + MobCDR.Amount
                   ttBal.Amt    = ttBal.Amt + MobCDR.Amount / ldVatCalc.
       ELSE ASSIGN ttBal.Amt    = ttBal.Amt + MobCDR.Amount
                   ttBal.VatAmt = ttBal.VatAmt + MobCDR.Amount * ldVatCalc.

       ASSIGN xQty = xQty + 1.

       IF xQty MOD 100 = 0 THEN DO:
          PAUSE 0.
          DISPLAY xQty WITH FRAME fQty.
       END.

    END. 

END.

ASSIGN sl = 1. 
VIEW STREAM tul FRAME sivuotsi.
ASSIGN rl = 12.

/* amounts by subscription type by product  */
FOR EACH ttBal
BREAK BY ttBal.SubType
      BY ttBal.Prod
      BY ttBal.Asub:


   ACCUMULATE ttBal.Amt    (TOTAL BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.VatAmt (TOTAL BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.Sec    (TOTAL BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.Qty    (TOTAL BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.Asub   (COUNT BY ttBal.Prod).

   IF LAST-OF(ttBal.Prod) THEN DO:              
      CheckPage(1).

      ASSIGN lcSubType = "".
      IF LOOKUP(ttBal.SubType,"fixed,unknown") = 0 THEN DO:
         FIND CLIType WHERE 
              CLIType.Brand   = gcBrand AND
              CLIType.CliType = ttBal.SubType NO-LOCK NO-ERROR.
         IF AVAILABLE CLIType THEN lcSubType = CLIType.CliName.
      END. 

      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = ttBal.prod NO-LOCK NO-ERROR.

      PUT STREAM tul
         ttBal.SubType AT 1  FORMAT "X(8)"
         lcSubType     AT 10 FORMAT "X(20)"
         ttBal.prod    AT 31 FORMAT "X(10)"
         (IF AVAILABLE BillItem 
          THEN BillItem.BIName
          ELSE "")     AT 42 FORMAT "X(19)".

      fPrintAmt ((ACCUM COUNT BY ttBal.prod ttBal.Asub),
                 (ACCUM TOTAL BY ttBal.prod ttBal.Amt),
                 (ACCUM TOTAL BY ttBal.prod ttBal.VatAmt),
                 (ACCUM TOTAL BY ttBal.prod ttBal.Sec),
                 (ACCUM TOTAL BY ttBal.prod ttBal.Qty)). 

      ASSIGN rl = rl + 1.

   END.  /* last-of prod */

   IF LAST-OF(ttBal.SubType) THEN DO:              
      CheckPage(2).

      PUT STREAM tul
         FILL("-",112) AT 1 FORMAT "X(112)" SKIP
         ttBal.SubType AT 1
         " total:" .


      ASSIGN liSubQty = 0.
      /* calculate qty of subscriptions */
      FOR EACH bttBal WHERE
         bttBal.SubType = ttBal.SubType
      BREAK BY bttBal.Asub:
         IF LAST-OF(bttBal.Asub) THEN ASSIGN
            liSubQty = liSubQty + 1.
      END.

      /* total qty of subscriptions */
      ASSIGN liSubTotQty = liSubTotQty + liSubQty.

      fPrintAmt (liSubQty,
                 (ACCUM TOTAL BY ttBal.SubType ttBal.Amt),
                 (ACCUM TOTAL BY ttBal.SubType ttBal.VatAmt),
                 (ACCUM TOTAL BY ttBal.SubType ttBal.Sec),
                 (ACCUM TOTAL BY ttBal.SubType ttBal.Qty)). 

      PUT STREAM tul SKIP(1).
      ASSIGN rl = rl + 3.

   END.  /* last-of subtype */

   /* grand total */
   IF LAST(ttBal.SubType) THEN DO:
      CheckPage(3).

      PUT STREAM tul
         SKIP(1)
         FILL("=",112) AT 1 FORMAT "X(112)" SKIP
         "Grand total:" AT 1.

      fPrintAmt (liSubTotQty,
                 (ACCUM TOTAL ttBal.Amt),
                 (ACCUM TOTAL ttBal.VatAmt),
                 (ACCUM TOTAL ttBal.Sec),
                 (ACCUM TOTAL ttBal.Qty)). 

      ASSIGN rl = rl + 3.

   END.


END. 

{Syst/uprfeed.i rl}

HIDE FRAME fQty NO-PAUSE.
ASSIGN SESSION:NUMERIC-FORMAT = xSessionNum.

