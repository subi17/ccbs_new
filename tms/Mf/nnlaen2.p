/* -------------------------------------------------------------------------
  MODULE .......: nnlaen2.p
  FUNCTION .....: Laskutusennuste kuvaruudulle 1 asiakkaasta
  APPLICATION ..: NN & TN
  CREATED ......: 24.08.1998 pt
  MODIFIED .....: 08.10.98 kl DECIMAL formats
                  01.11.99 kl USE-INDEX pu-as-l
                  22.11.2001/aam Asub AS an option
                  12.12.2001/aam use pl-vat AND vc-perc 
                  08.01.2002/aam old calls (from nn) are always vat 0,
                                 "kr" -references replace WITH currency,
                                 check PriceListhist FOR correct PriceList
                  31.01.2002 ht  fuction fVatCur corrected
                  12.11.2002 kl  fuction fVatCur corrected,
                                 show used pricelists
                  28.02.2003 aam fCallPriceList,
                                 defaults if FixCDR.TariffId = 0
                  03.03.03 tk  tokens               
                  11.12.03 aam VatUsage
  VERSION ......: M15
  ------------------------------------------------------------------------ */

{commali.i}
{fcustpl.i}
{fvatfact.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'fixcdr'}

DEF INPUT PARAMETER  liCustNum LIKE Customer.CustNum NO-UNDO.

DEF VAR alvpros   AS DE NO-UNDO.

DEF VAR lkm1      AS i  NO-UNDO.
DEF VAR lkm2      AS i  NO-UNDO.
def var summa1    as de no-undo format "-z,zzz,zzz.99".
def var summa2    as de no-undo format "-z,zzz,zzz.99".
def var summa1a   as de no-undo format "-z,zzz,zzz.99".
def var summa2a   as de no-undo format "-z,zzz,zzz.99".
def var pvm1      as da no-undo format "99.99.99".
def var pvm2      as da no-undo format "99.99.99".

def var xCLI     as ch no-undo format "x(15)".

def var xCur      as ch no-undo format "x(3)". 
DEF VAR xCur2     LIKE xCur.
DEF VAR xVatAmt        AS DEC NO-UNDO.
DEF VAR xWVat          AS DEC NO-UNDO.
DEF VAR xWOVat         AS DEC NO-UNDO.
DEF VAR xVatBilled     AS DEC NO-UNDO.
DEF VAR xNoVatBilled   AS DEC NO-UNDO.
DEF VAR xVatUnbilled   AS DEC NO-UNDO.
DEF VAR xNoVatUnbilled AS DEC NO-UNDO.
DEF VAR xPLCode        AS CH  NO-UNDO.
DEF VAR liBillTarget   AS INT NO-UNDO. 
DEF VAR ldPeriod1      AS DEC NO-UNDO.
DEF VAR ldPeriod2      AS DEC NO-UNDO. 

DEF VAR ldVatFactor AS DEC NO-UNDO.
DEF VAR ldAmt       AS DEC NO-UNDO.
DEF VAR lcTaxZone   AS CHAR NO-UNDO.
DEF VAR ldaTaxDate AS DATE NO-UNDO. 


FUNCTION fVatCur RETURNS LOGICAL
    (INPUT  iRid    AS INT, INPUT  iSum AS DEC,
     INPUT idaTaxDate AS DATE,
     OUTPUT iVatAmt AS DEC, OUTPUT iWVat AS DEC, OUTPUT iWOVat AS DEC):

    DEF VAR llVatIncl   AS LOGIC NO-UNDO.
    DEF VAR llCurrUnit  AS LOGIC NO-UNDO.
    DEF VAR lcPriceList AS CHAR  NO-UNDO. 

    ASSIGN llVatIncl   = FixCDR.VatIncl
           llCurrUnit  = FALSE
           lcPriceList = "".

    IF FixCDR.TariffId NE 0 THEN DO:

       FIND FIRST Tariff NO-LOCK WHERE
                  Tariff.TariffNum = FixCDR.TariffId
       NO-ERROR.

       IF AVAILABLE Tariff THEN DO:

          IF Tariff.PriceList NE "" THEN lcPriceList = Tariff.PriceList.
          ELSE lcPriceList = fCallPriceList(Customer.CustNum,
                                            liBillTarget,
                                            FixCDR.Prefix,
                                            "",
                                            /* FixCDR.DialType */ 1,
                                            FixCDR.Date). 

          FIND PriceList NO-LOCK WHERE
               PriceList.PriceList = lcPriceList NO-ERROR.

          IF AVAIL PriceList THEN DO:

             IF LOOKUP(PriceList.Currency,xCur) = 0 THEN DO:
                xCur = xCur + "," + PriceList.Currency.
                IF SUBSTR(xCur,1,1) = "," THEN xCur = SUBSTR(xCur,2).
             END.
             IF LOOKUP(PriceList.PriceList,xPLCode) = 0 THEN DO:
                xPLCode = xPLCode + "," + PriceList.PriceList.
                IF SUBSTR(xPLCode,1,1) = "," THEN xPLCode = SUBSTR(xPLCode,2).
             END.

             ASSIGN llCurrUnit = PriceList.CurrUnit.
          END.

       END.            

    END.

    ldVatFactor = fVatFactor(Customer.VatUsage,
                             lcTaxZone,   
                             FixCDR.BillCode,
                             idaTaxDate).
                             
    IF llVatIncl THEN ASSIGN
       iWOVat  = iSum / ldVatFactor
       iWVat   = iSum.

    ELSE ASSIGN
       iWOVat  = iSum
       iWVat   = iSum * ldVatFactor.

    iVatAmt = iWVat - iWOVat.
    
    IF NOT llCurrUnit THEN ASSIGN
       iVatAmt = iVatAmt / 100
       iWOVat  = iWOVat  / 100
       iWVat   = iWVat   / 100.


END FUNCTION.


FIND Customer where Customer.CustNum = liCustNum NO-LOCK.

form
   skip(1)
"  Note: This program shows the total value of telephone calls" skip
"        being made from this customer."                        skip(1)
"        A-number ...........:" xCLI 
    help "A-number for calls (EMPTY = all customer's A-numbers)"
    SKIP
"        Calls within .......:" pvm1
help "Earliest date of call" "-" pvm2 
help "Latest date of call" skip(1)

"    Not billed " lkm1 "calls," xCur summa1 "/" summa1a "w/VAT "  SKIP
"    Billed     " lkm2 "calls," xCur2 summa2 "/" summa2a "w/VAT " SKIP(1)
"    Pricelist(s) used:" xPLCode FORMAT "x(20)"                   SKIP(1)
WITH
   row 5 col 3 overlay no-labels title " " + ynimi +
   " Total value of calls, cust. " + string(Customer.CustNum) + " " +
   substr(Customer.CustName,1,16) + " " FRAME rajat.


pvm1 = date(month(TODAY),1,year(TODAY)).
pvm2 = pvm1 + 40.
pvm2 = date(month(pvm2),1,year(pvm2)) - 1.

lcTaxZone = fRegionTaxZone(Customer.Region).

rajat:
repeat WITH FRAME rajat:

   PAUSE 0.
   ehto = 9. RUN ufkey.

   UPDATE xCLI pvm1 pvm2
   validate (input pvm2 >= input pvm1,"Incorrect order !").

toimi:
   repeat WITH FRAME toimi:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN LEAVE toimi.
   END.

   ASSIGN lkm1 = 0 lkm2 = 0 summa1 = 0 summa2 = 0
          xVatBilled     = 0
          xNoVatBilled   = 0
          xVatUnbilled   = 0
          xNoVatUnbilled = 0
          xCur           = "".

   message "Calculating ...".                   

   /* if CLI was not chosen then we can use the first available BillTarget */

   ASSIGN liBillTarget = 0
          ldPeriod1    = YEAR(pvm1) * 10000 +
                         MONTH(pvm1) * 100  +
                         DAY(pvm1) 
          ldPeriod2    = YEAR(pvm2) * 10000 +
                         MONTH(pvm2) * 100  +
                         DAY(pvm2)          + 
                         0.86399. 

   IF xCLI NE "" THEN DO:

      FIND FIRST CLI NO-LOCK WHERE
                 CLI.CustNum   = Customer.CustNum AND
                 CLI.CLI       = xCLI             AND
                 CLI.CrStamp  <= ldPeriod1        AND
                 CLI.ClStamp  >= ldPeriod2 NO-ERROR.
      IF AVAILABLE CLI THEN ASSIGN liBillTarget = CLI.BillTarget.
   END.

   IF liBillTarget = 0 THEN DO:
      FIND FIRST BillTarget OF Customer NO-LOCK NO-ERROR.
      IF AVAILABLE BillTarget THEN liBillTarget = BillTarget.BillTarget.
   END.

   FOR EACH FixCDR NO-LOCK USE-INDEX InvCust where
            FixCDR.InvCust = liCustNum AND
            FixCDR.Date   >= pvm1  AND
            FixCDR.Date   <= pvm2,

      FIRST InvSeq NO-LOCK where
            InvSeq.InvSeq = FixCDR.InvSeq.

        if xCLI ne "" AND FixCDR.CLI NE xCLI THEN NEXT.

        ldaTaxDate = TODAY.
        IF InvSeq.Billed THEN DO:
           FIND Invoice WHERE
                Invoice.InvNum = InvSeq.InvNum NO-LOCK NO-ERROR.
           IF AVAIL Invoice THEN ldaTaxDate = Invoice.InvDate.
        END.

        fVatCur(INPUT  FixCDR.TariffId, 
                INPUT  FixCDR.GrossPrice - FixCDR.DiscValue,
                ldaTaxDate, 
                OUTPUT xVatAmt, OUTPUT xWVat, OUTPUT xWOVat).

        IF InvSeq.billed THEN DO:
            ASSIGN lkm2 = lkm2 + 1.
            ASSIGN 
                xVatBilled   = xVatBilled   + xWVat
                xNoVatBilled = xNoVatBilled + xWOVat.
        END.
        ELSE DO:
            ASSIGN lkm1 = lkm1 + 1. 
            ASSIGN 
                xVatUnbilled   = xVatUnbilled   + xWVat
                xNoVatUnbilled = xNoVatUnbilled + xWOVat.
        END.
   END.

   PAUSE 0.

   ASSIGN 
      summa1  = xNoVatUnbilled 
      summa1a = xVatUnbilled 
      summa2  = xNoVatBilled 
      summa2a = xVatBilled 
      xCur2   = xCur.

   DISP 
      lkm1 
      lkm2 
      xCur 
      xCur2 
      summa1 
      summa1a 
      summa2 
      summa2a
      xPLCode
   WITH FRAME rajat.

   PAUSE MESSAGE "Press ENTER to continue !".

   HIDE FRAME rajat NO-PAUSE.

   LEAVE rajat.

END.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

