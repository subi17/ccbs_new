/* ------------------------------------------------------
  MODULE .......: SUBSREP
  FUNCTION .....: List subscription revenue data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 21.08.03
  MODIFIED .....: 24.09.03/aam brand
                  31.12.03/aam asiakas.VatUsage, fVatFactor()
                  30.03.04 kl  fixes after index changes

  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}
{cparam2.i}
{utumaa.i}
{fcurrency.i}
{fvatfact.i}
{callquery.i}

DEF INPUT PARAMETER idtDate1     AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtDate2     AS DATE  NO-UNDO.
DEF INPUT PARAMETER icInvGrp1    AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icInvGrp2    AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSubType1   AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSubType2   AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icProd1      AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icProd2      AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER iiCCN1       AS INT   NO-UNDO. 
DEF INPUT PARAMETER iiCCN2       AS INT   NO-UNDO. 
DEF INPUT PARAMETER icToFile     AS CHAR  NO-UNDO. 

def var viiva1 as char NO-UNDO format "x(112)".
def var viiva2 like viiva1.
def var viiva3 like viiva1.
def var viiva4 like viiva1.
def var jar    as char NO-UNDO format "x(24)".
def var order  as int  NO-UNDO.
def var sl     as int  NO-UNDO.
def var rl     as int  NO-UNDO.
def var rlx    as int  NO-UNDO.
def var lev    as int  NO-UNDO init 112.

DEF VAR xOk         AS LOG  NO-UNDO.
DEF VAR xEmpty      AS CHAR NO-UNDO INIT "<EMPTY>".
DEF VAR xSessionNum AS CHAR NO-UNDO.
DEF VAR xDateHeader AS CHAR NO-UNDO.
DEF VAR xQty        AS INT  NO-UNDO.

DEF VAR liPeriod    AS INT  NO-UNDO.
DEF VAR liDefVat    AS INT  NO-UNDO. 
DEF VAR ldVatCalc   AS DEC  NO-UNDO. 
DEF VAR llFixInclVat AS LOG  NO-UNDO. 
DEF VAR llInclVat   AS LOG  NO-UNDO. 
DEF VAR ldAmount    AS DEC  NO-UNDO. 
DEF VAR lcSubType   AS CHAR NO-UNDO. 
DEF VAR liSubQty    AS INT  NO-UNDO.
DEF VAR liSubTotQty AS INT  NO-UNDO. 
DEF VAR lcAsub      AS CHAR NO-UNDO. 
DEF VAR ldNet       AS DEC  NO-UNDO.
DEF VAR ldGross     AS DEC  NO-UNDO. 
DEF VAR lcTaxZone   AS CHAR NO-UNDO.

DEF VAR tthCDR         AS HANDLE NO-UNDO.
DEF VAR liERrorCodeOut AS INT    NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable AS CHAR.

tthCDR = TEMP-TABLE ttCall:HANDLE.


DEF TEMP-TABLE ttBal NO-UNDO
    FIELD SubType  AS CHAR
    FIELD Prod     AS CHAR
    FIELD CCN      AS INT
    FIELD BDest    AS CHAR
    FIELD ASub     AS CHAR
    FIELD Amt      AS DEC
    FIELD VatAmt   AS DEC 
    FIELD CCost    AS DEC 
    FIELD Sec      AS INT
    FIELD Qty      AS INT
    FIELD Pulse    AS INT
    FIELD SubQty   AS INT
    INDEX SubType SubType Prod CCN BDest Asub. 

DEF TEMP-TABLE ttSubQty NO-UNDO
    FIELD SubType AS CHAR
    FIELD Qty     AS INT
    INDEX SubType SubType.

DEF BUFFER bttBal FOR ttBal. 
DEF BUFFER bInvSeq FOR InvSeq.

ASSIGN 
    xSessionNum = SESSION:NUMERIC-FORMAT
    SESSION:NUMERIC-FORMAT = "European"
    viiva1   = fill("=",lev)
    viiva2   = fill("=",lev)
    viiva3   = fill("-",lev)
    viiva4   = fill("-",lev)
    liDefVat = fCParamI("DefVatCode"). 


form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(30)" 
      "SUBSCRIPTION REVENUE" at 40
      "Page" at 102  
      sl format "ZZZZ9" skip
   xDateHeader AT 40 FORMAT "X(30)"
      pvm format "99.99.9999" at 103 skip
   viiva2 at 1 skip
   "BDestination"      AT 7
   "Qty"               TO 48
   "Pulses"            TO 56
   "Minutes"           TO 64
   "Amount VAT0"       TO 77
   "Amt with VAT"      TO 90
   "Cost"              TO 104
   "Margin%"           TO 112
   SKIP
   viiva3
   with width 112 no-label no-box frame sivuotsi.

FUNCTION fUpdBal RETURNS LOGICAL
   (iSubType AS CHAR,
    iProd    AS CHAR,
    iCCN     AS INT,
    iBDest   AS CHAR,
    iAsub    AS CHAR).

    FIND FIRST ttBal WHERE
       ttBal.SubType = iSubType AND
       ttBal.Prod    = iProd    AND
       ttBal.CCN     = iCCN     AND
       ttBal.BDest   = iBDest   AND
       ttBal.Asub    = iAsub    NO-ERROR.
    IF NOT AVAILABLE ttBal THEN DO:
       CREATE ttBal.
       ASSIGN ttBal.SubType = iSubType 
              ttBal.Prod    = iProd
              ttBal.CCN     = iCCN
              ttBal.BDest   = iBDest
              ttBal.Asub    = iAsub.
    END.

    RETURN TRUE. 

END FUNCTION.


FUNCTION CheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF icToFile > "" THEN RETURN FALSE.

    if rl + iAddLine >= skayt1 then do:
        {uprfeed.i rl}
        assign rlx = 0
               sl = sl + 1.
        view stream tul frame sivuotsi.  
        assign rl = 6.
    end.

    RETURN TRUE.
END.

assign rl          = 0
       liSubTotQty = 0
       xDateHeader = STRING(idtDate1,"99.99.9999") + " - " +
                     STRING(idtDate2,"99.99.9999").

FORM "Group: " AT 2 Customer.InvGroup SKIP
     "Qty: "   AT 2 xQty format ">>,>>>,>>9" 
WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting "
   FRAME fQty.
PAUSE 0.
VIEW FRAME fQty. 

/* invoiced data from desired period, fixed calls and mobile calls */
FOR EACH InvGroup NO-LOCK WHERE
         InvGroup.Brand     = gcBrand    AND
         InvGroup.InvGroup >= icInvGrp1  AND
         InvGroup.InvGroup <= icInvGrp2,
    EACH Customer OF InvGroup NO-LOCK,
    EACH bInvSeq NO-LOCK WHERE
         bInvSeq.CustNum   = Customer.CustNum AND
         bInvSeq.FromDate <= idtDate2         AND
         bInvSeq.ToDate   >= idtDate1         AND
         bInvSeq.billed = TRUE:

    lcTaxZone = fRegionTaxZone(Customer.Region).

    EMPTY TEMP-TABLE ttCall.
     
    fMobCDRCollect(INPUT "post",
                   INPUT gcBrand,
                   INPUT katun,
                   INPUT idtDate1,
                   INPUT idtDate2,
                   INPUT 0,
                   INPUT "",
                   INPUT "",
                   INPUT bInvSeq.InvSeq,
                   INPUT 0,
                   INPUT "",
                   INPUT "",
                   INPUT "",
                   INPUT 0,
                   INPUT-OUTPUT liErrorCodeOut,
                   INPUT-OUTPUT tthCDR).

    FOR EACH ttCall:

       ASSIGN lcAsub    = ttCall.CLI
              lcSubType = IF ttCall.CLIType = "" 
                          THEN "UNKNOWN"
                          ELSE ttCall.CLIType.

       IF lcSubType < icSubType1 OR
          lcSubType > icSubType2 
       THEN NEXT. 

       /* is currency unit full or sub (assign ldnet & ldgross) */
       fCurrUnit(ttCall.Amount,
                 ttCall.Amount,
                 ttCall.CurrUnit,
                 "",
                 ttCall.TariffNum,
                 gcBrand,
                 OUTPUT ldNet,
                 OUTPUT ldGross).

       FIND Invoice WHERE
            Invoice.InvNum = bInvSeq.Invnum NO-LOCK.

       ldVatCalc = fVatFactor(Customer.VatUsage,
                              lcTaxZone,
                              ttCall.BillCode,
                              Invoice.Invdate).
 
       fUpdBal(lcSubType,
               ttCall.BillCode,
               ttCall.CCN,
               ttCall.bdest,
               lcAsub). 

       ASSIGN ttBal.Sec   = ttBal.Sec   + ttCall.BillDur
              ttBal.Qty   = ttBal.Qty   + 1
              ttBal.Pulse = ttBal.Pulse + ttCall.Pulse
              /*
              ttBal.CCost = ttBal.CCost + ttCall.spo-hinta
              */.

       IF ttCall.VatIncl 
       THEN ASSIGN ttBal.VatAmt = ttBal.VatAmt + ttCall.Amount
                   ttBal.Amt    = ttBal.Amt    + ttCall.Amount / ldVatCalc.
       ELSE ASSIGN ttBal.Amt    = ttBal.Amt    + ttCall.Amount
                   ttBal.VatAmt = ttBal.VatAmt + ttCall.Amount * ldVatCalc.

       ASSIGN xQty = xQty + 1.

       IF xQty MOD 100 = 0 THEN DO:
          PAUSE 0.
          DISPLAY Customer.InvGroup xQty WITH FRAME fQty.
       END.

    END. 

END.

HIDE FRAME fQty NO-PAUSE. 

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.

/* headers to excel-file */
IF icToFile > "" THEN DO:
   OUTPUT STREAM tul TO VALUE(icToFile).
   PUT STREAM tul UNFORMATTED
      "SUBSCRIPTION REVENUE"  SKIP
      idtDate1 "-" idtDate2   SKIP
      ""
      (IF icInvGrp1 = ""
       THEN "EMPTY"
       ELSE icInvGrp1) "-" icInvGrp2 SKIP(1)
      "Subsc.Type"      CHR(9)
      "Product"         CHR(9)
      "Prod. Name"      CHR(9)
      "CCN"             CHR(9)
      "CCN Name"        CHR(9)
      "BDestin."        CHR(9)
      "BDest.Name"      CHR(9)
      "Qty"             CHR(9)
      "Pulses"          CHR(9)
      "Minutes"         CHR(9)
      "Amount VAT0"     CHR(9)
      "Amt with VAT"    CHR(9)
      "Cost VAT0"       CHR(9)
      "Margin%"         SKIP.
END.

/* headers to paper print */
ELSE DO:
   assign sl = 1. 
   VIEW STREAM tul FRAME sivuotsi.
   ASSIGN rl = 6.
END.

/* print amounts by subscription type by product by CCN by bdest */
FOR EACH ttBal
BREAK BY ttBal.SubType
      BY ttBal.Prod
      BY ttBal.CCN
      BY ttBal.BDest
      BY ttBal.Asub:


   ACCUMULATE ttBal.Amt    (TOTAL BY ttBal.BDest
                                  BY ttBal.CCN
                                  BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.VatAmt (TOTAL BY ttBal.BDest
                                  BY ttBal.CCN
                                  BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.Sec    (TOTAL BY ttBal.BDest
                                  BY ttBal.CCN
                                  BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.Qty    (TOTAL BY ttBal.BDest
                                  BY ttBal.CCN
                                  BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.Pulse  (TOTAL BY ttBal.BDest
                                  BY ttBal.CCN
                                  BY ttBal.Prod
                                  BY ttBal.SubType)
              ttBal.CCost  (TOTAL BY ttBal.BDest
                                  BY ttBal.CCN
                                  BY ttBal.Prod
                                  BY ttBal.SubType).

   /* subscription type header */                                  
   IF FIRST-OF(ttBal.SubType) AND icToFile = "" THEN DO:

      CheckPage(4).

      ASSIGN lcSubType = "".
      IF LOOKUP(ttBal.SubType,"fixed,unknown") = 0 THEN DO:
         FIND CLIType WHERE 
              CLIType.Brand   = gcBrand AND
              CLIType.CLIType = ttBal.SubType NO-LOCK NO-ERROR.
         IF AVAILABLE CLIType THEN lcSubType = CLIType.CLIName.
      END. 

      PUT STREAM tul UNFORMATTED
         ttBal.SubType AT 1 
         " " 
         lcSubType 
         SKIP(1).
      rl = rl + 2.   

   END.

   /* product header */
   IF FIRST-OF(ttBal.Prod) AND icToFile = "" THEN DO:              

      CheckPage(3).

      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = ttBal.prod NO-LOCK NO-ERROR.

      PUT STREAM tul UNFORMATTED
         ttBal.Prod AT 3 
         " " 
         (IF AVAILABLE BillItem THEN BillItem.BIName ELSE "")
         SKIP.
      rl = rl + 1.
   END.

   /* CCN header */
   IF FIRST-OF(ttBal.CCN) AND icToFile = "" THEN DO:              

      CheckPage(1).

      FIND CCN WHERE 
           CCN.Brand = gcBrand AND
           CCN.CCN   = ttBal.CCN NO-LOCK NO-ERROR.

      PUT STREAM tul UNFORMATTED
         ttBal.CCN AT 5 
         " " 
         (IF AVAILABLE CCN THEN CCN.CCNName ELSE "")
         SKIP.
      rl = rl + 1.
   END.

   /* actual data row */     
   IF LAST-OF(ttBal.BDest) THEN DO:              

      CheckPage(0).

      FIND FIRST BDest WHERE 
           BDest.Brand = gcBrand AND
           BDest.BDest = ttBal.BDest NO-LOCK NO-ERROR.

      /* to excel-file */
      IF icToFile > "" THEN DO:

          FIND BillItem WHERE 
               BillItem.Brand    = gcBrand AND
               BillItem.BillCode = ttBal.prod NO-LOCK NO-ERROR.
          FIND CCN WHERE 
               CCN.Brand = gcBrand AND
               CCN.CCN   = ttBal.CCN NO-LOCK NO-ERROR.

          PUT STREAM tul UNFORMATTED 
             ttBal.SubType                                        CHR(9)
             ttBal.Prod                                           CHR(9)
             (IF AVAILABLE BillItem THEN BillItem.BIName ELSE "")      CHR(9)
             ttBal.CCN                                            CHR(9)
             (IF AVAILABLE CCN THEN CCN.CCNName ELSE "")      CHR(9)
             ttBal.BDest                                          CHR(9)
             (IF AVAILABLE BDest THEN BDest.BDName ELSE "")    CHR(9)
             (ACCUM TOTAL BY ttBal.BDest ttBal.Qty)               CHR(9)
             (ACCUM TOTAL BY ttBal.BDest ttBal.Pulse)             CHR(9)
             INTEGER((ACCUM TOTAL BY ttBal.BDest ttBal.Sec) / 60) CHR(9)
             ROUND((ACCUM TOTAL BY ttBal.BDest ttBal.Amt),2)      CHR(9)
             ROUND((ACCUM TOTAL BY ttBal.BDest ttBal.VatAmt),2)   CHR(9)
             (ACCUM TOTAL BY ttBal.BDest ttBal.CCost)             CHR(9)
             ROUND((IF (ACCUM TOTAL BY ttBal.BDest ttBal.Amt) NE 0
                    THEN 100 * ((ACCUM TOTAL BY ttBal.BDest ttBal.Amt) -
                                (ACCUM TOTAL BY ttBal.BDest ttBal.CCost)) /
                         (ACCUM TOTAL BY ttBal.BDest ttBal.Amt)   
                    ELSE 0),2)                                    SKIP.
      END.

      /* paper print */
      ELSE DO:
         PUT STREAM tul
            ttBal.BDest + 
            (IF AVAILABLE BDest
             THEN " " + BDest.BDName
             ELSE "")   AT 7 FORMAT "X(34)".

         {subsrep.i "ACCUM TOTAL BY ttBal.BDest"}

         rl = rl + 1.
      END.

   END.

   /* subtotal by CCN */
   IF LAST-OF(ttBal.CCN) AND icToFile = "" THEN DO:              

      CheckPage(2).

      PUT STREAM tul UNFORMATTED 
         FILL("-",108) AT 5 SKIP
         STRING(ttBal.CCN) + " total"  AT 5.

      {subsrep.i "ACCUM TOTAL BY ttBal.CCN"}

      PUT STREAM tul SKIP(1).
      rl = rl + 3.
   END.

   /* subtotal by product */
   IF LAST-OF(ttBal.Prod) AND icToFile = "" THEN DO:              

      CheckPage(2).

      PUT STREAM tul UNFORMATTED 
         FILL("-",110) AT 3 SKIP
         ttBal.Prod + " total"  AT 3.

      {subsrep.i "ACCUM TOTAL BY ttBal.Prod"}

      PUT STREAM tul SKIP(1).
      rl = rl + 3.
   END.

   /* subtotal by subscription type */  
   IF LAST-OF(ttBal.SubType) THEN DO:              

      CheckPage(2).

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

      IF icToFile > "" THEN DO:
         CREATE ttSubQty.
         ASSIGN ttSubQty.SubType = ttBal.SubType
                ttSubQty.Qty     = liSubQty.
      END.

      ELSE DO:
         PUT STREAM tul UNFORMATTED
            FILL("-",112) AT 1 SKIP
            ttBal.SubType + " total (" +
            STRING(liSubQty) + " subscr.)"
            AT 1.

         {subsrep.i "ACCUM TOTAL BY ttBal.SubType"}

         PUT STREAM tul SKIP(1).
         rl = rl + 3.
      END.

   END.  /* last-of subtype */

   /* grand total */
   IF LAST(ttBal.SubType) AND icToFile = ""  THEN DO:
      CheckPage(2).

      PUT STREAM tul UNFORMATTED
         FILL("=",112) AT 1 SKIP
         "Grand total (" +
         STRING(liSubTotQty) + " subscr.)"
         AT 1.

      {subsrep.i "ACCUM TOTAL"}

      rl = rl + 2.

   END.

END. 

IF icToFile > "" THEN DO:

   PUT STREAM tul UNFORMATTED
      SKIP
      "Type"             CHR(9)
      "Subscription Qty" SKIP.

   FOR EACH ttSubQty:
      PUT STREAM tul UNFORMATTED
         ttSubQty.SubType   CHR(9)
         ttSubQty.Qty       SKIP.
   END.

   OUTPUT STREAM tul CLOSE.
END.

ELSE DO:
   {uprfeed.i rl}
END.

HIDE FRAME fQty NO-PAUSE.

ASSIGN SESSION:NUMERIC-FORMAT = xSessionNum.

