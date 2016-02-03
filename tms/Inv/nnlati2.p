/* -----------------------------------------------------
  MODULE .......: nnlati2.p
  KUTSUVAMODULI : NN.P
  TEHTAVA ......: Laskutilasto mjä/asiakas/maa
  SOVELLUTUS ...: NN
  TEKIJA .......: PT
  CREATED ......: 02.12.96
  changePVM ....: 24.02.97 pt - laasno muutettu CustNum:ksi
                  04.05.97 pt - korjattu myyjArajaus FOR EACH:iin
                  22.01.98 kl - ohti into Salesman
                  29.01.98 kl - as-myyja => Salesman
                  21.01.99 pt - in English, CustGroup
                  12.09.02 jp - text error, Help text
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

/* print-linemuuttujat */
{Syst/utumaa.i NEW }

assign tuni1 = "nnlati"
       tuni2 = "".

def var date1    as Date format "99-99-99" NO-UNDO.
def var date2    as Date format "99-99-99" NO-UNDO.
def var sm-code1 as c    format "x(8)"     NO-UNDO init 1.
def var sm-code2 as c    format "x(8)"     NO-UNDO init 999.
DEF VAR CustGroup  LIKE CustGroup.CustGroup       NO-UNDO.
DEF VAR rl       AS i                      NO-UNDO.
DEF VAR sl       AS i                      NO-UNDO.

DEF VAR pdate1   AS Date                   NO-UNDO.
DEF VAR pdate2   AS Date                   NO-UNDO.
DEF VAR maalkm   AS i                      NO-UNDO.
DEF VAR netBill    AS DE                     NO-UNDO.
def var alep     as de  format "z9.9"      NO-UNDO.

DEF WORKFILE wcountry
   FIELD wcountry   AS i
   FIELD wnet   AS DE
   FIELD wdiscount   AS DE.

form
     skip(1)
     "  Note:  This program prints out a summary with Billed amounts  "
     "         by Salesman / customer /country(Service), from invoices Printed "
     "         during a given Period of time.  'TOP 5' countries(services) "
     "         of each customer are shown, sorted by Billed amount ." skip(1)
     "         REMEMBER to enter INVOICE dates (not CALL dates)."            
     skip(2)
     "Invoices written during:" at 15 date1 format "99-99-99" to 50 
     HELP "Invoice date from"
     "-"
     date2  NO-LABEL 
     HELP "Invoice date to"                                      SKIP
     "External CustGroup ....: " AT 15 CustGroup
     help "Code of an External Customer Group, (EMPTY = none)"
     "Salesman ..............:" at 15 sm-code1  to 50 
     HELP "Salesman from"
     "-"
     sm-code2 NO-LABEL 
     HELP "Salesman to" skip(5)

WITH
   width 80 COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " BILLING BY SALESMAN/CUSTOMER/COUNTRY(Service) " +
   string(pvm,"99-99-99") + " " NO-LABELS OVERLAY FRAME rajat.

form header

   fill("=",114) format "x(114)" SKIP
   ynimi at 2 format "x(25)" "BILLING STATISTICS BY" at 40 date1 "-" date2
   "Page" at 105 sl format "ZZZZ9" SKIP
   "SALESMAN / CUSTOMER / Country "  at 40 string(pvm,"99-99-99") AT 107 SKIP
   fill("=",114) format "x(114)" skip(1)
   "CustNo" at 4 "Customer's Name" at 11 "Billed (ex VATAmt)" TO 57
   space(1) "Disc% used" SKIP
   fill("-",114) format "x(114)" SKIP
   WITH width 114 NO-LABEL no-box FRAME sivuotsi.

/* Haetaan pvm-ehdotus ensin viim. laskuttamattom., sitten viim. laskutetusta */
ASSIGN date1  = pvm.
FIND FIRST Invoice no-lock no-error.
IF AVAIL Invoice THEN ASSIGN date1 = Invoice.InvDate.
date2 = date1.

cfc = "sel". RUN ufcolor.
LOOP:
repeat WITH FRAME rajat:
    ehto = 9. RUN ufkey.

    UPDATE

    date1
    date2   validate(input date2 >= input date1,"Invalid order of dates !")
    CustGroup validate(input CustGroup = "" OR can-find(CustGroup where CustGroup.CustGroup
                     = input CustGroup),"Unknown group !")
    sm-code1 
    sm-code2 validate(INPUT sm-code2 >= INPUT sm-code1,
    "Invalid order of salesmen !").

TOIMI:
   repeat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT LOOP.
      IF toimi = 8 THEN LEAVE LOOP.
      IF toimi = 5 THEN LEAVE TOIMI.
   END.


   /* Avataan striimi */
   ASSIGN tila = TRUE.
   {Syst/tmsreport.i "return"}

   message "Printing ...".                        

   FOR
      EACH  Invoice no-lock where
            Invoice.InvDate >= date1           AND
            Invoice.InvDate <= date2,
      FIRST Customer no-lock where
            Customer.CustNum = Invoice.CustNum  AND
            Customer.Salesman >= sm-code1      AND
            Customer.Salesman <= sm-code2      AND

           (if CustGroup ne "" THEN can-find(CGMember where 
            CGMember.CustNum = Customer.CustNum AND 
            CGMember.CustGroup = CustGroup) ELSE TRUE),

      FIRST Salesman    no-lock where
            Salesman.Salesman = string(Customer.Salesman)
   BREAK
   BY Customer.Salesman
   BY Customer.CustName + string(Customer.CustNum).

      accumulate
      Invoice.AmtExclVAT
         (sub-total BY Customer.Salesman BY 
                    Customer.CustName + string(Customer.CustNum)).

      IF first-of(Customer.Salesman) THEN DO:
         ASSIGN
         sl = sl + 1 rl = 7.
         view STREAM tul FRAME sivuotsi.

         IF sl = 1 THEN DO:
            /* show print criteria on 1st page */

            PUT STREAM tul UNFORMATTED
            "External CustGroup ...: " 
            (if CustGroup = "" then "NONE" ELSE CustGroup)
            SKIP
            "Salesmen .............: " sm-code1 " - " sm-code2
            skip(1).

            rl = rl + 3.
         END.


         PUT STREAM tul
         "Salesman"                           AT 1
         Customer.Salesman   format "x(5)"     AT 10
         SmName format "x(20)"              AT 16     
         skip(1).
         rl = rl + 2.
      END.

      IF first-of(Customer.CustName + string(Customer.CustNum)) THEN
      ASSIGN pdate1 = ? pdate2 = ?.

      /* asiakkaan varhaisim / myöhäisin puhelupvm */
      IF Invoice.FromDate > pdate1 OR pdate1 = ? THEN pdate1 = Invoice.FromDate.
      IF Invoice.ToDate < pdate2 OR pdate2 = ? THEN pdate2 = Invoice.ToDate.


      /* Tarvitaanko uusi sivu */
      IF rl >= skayt1 THEN DO:
         {Syst/uprfeed.i rl}
         ASSIGN rl = 7  sl = sl + 1.
         view STREAM tul FRAME sivuotsi.
      END.

      /* tulostetaan asiakkaan line */
      IF last-of (Customer.CustName + string(Customer.CustNum)) THEN DO:
         PUT STREAM tul
         Customer.CustNum   format "zzzzzz9" AT 3
         Customer.CustName  format "x(30)"   AT 11
         (accum sub-total 
            BY Customer.CustName + string(Customer.CustNum) Invoice.AmtExclVAT)
                  format "zz,zzz,zz9.99-" AT 45
         " TOTAL"
         SKIP.
         rl = rl + 1.

         /* kumuloidaan asiakkaan puhelut maittain ko. jaksolla */
MAA:     FOR EACH FixCDR  no-lock where
                  FixCDR.InvCust =  Invoice.CustNum AND
                  FixCDR.Date  >= pdate1         AND
                  FixCDR.Date  <= pdate2,

            FIRST InvSeq no-lock where
                  InvSeq.InvSeq = FixCDR.InvSeq    AND
                  InvSeq.Billed = TRUE

         BREAK BY CCN:
            netBill = GrossPrice - DiscValue.
            accumulate
               netBill    (sub-total BY CCN)
               DiscValue (sub-total BY CCN).
            .
            IF last-of(CCN) THEN DO:
               CREATE wcountry.
               ASSIGN
                 wcountry.wcountry  = FixCDR.CCN
                 wcountry.wnet      = (accum  sub-total BY CCN   netBill)
                 wcountry.wdiscount = (accum  sub-total BY CCN   DiscValue).
             END.
         END.
         /* nyt ovat puhelut koossa */
         maalkm = 0.
         FOR
            EACH  wcountry,
            FIRST CCN where
                  CCN.CCN = wcountry.wcountry 
         BY wcountry.wnet descending:

            maalkm = maalkm + 1.
            /* tulostetaan vain 5 suurinta maata */
            IF maalkm < 6 THEN DO:
               /* Tarvitaanko uusi sivu */
               IF rl >= skayt1 - 2 THEN DO:
                  {Syst/uprfeed.i rl}
                  ASSIGN
                  sl = sl + 1.
                  view STREAM tul FRAME sivuotsi.  rl = 7.
               END.

               alep = round(wcountry.wdiscount / (wcountry.wnet + wcountry.wdiscount) * 100,1).

               PUT STREAM tul
               CCN.CCNName format "x(24)" AT 13            space(1)
               wcountry.wnet   format "zz,zzz,zz9.99" AT 45    space(1)
               alep          format "z9.9" " %"
               SKIP.
               rl = rl + 1.
            END.
            DELETE wcountry.
         END. /* asiakkaan 5 parasta maata */
         IF rl < skayt1 THEN DO:
            PUT STREAM tul skip(1).
            rl = rl + 1.
         END.


      END.

      IF last-of (Customer.Salesman) THEN DO:
        /* Tarvitaanko uusi sivu */
         IF rl >= skayt1 - 2 THEN DO:
            {Syst/uprfeed.i rl}
            ASSIGN rl = 7  sl = sl + 1.
            view STREAM tul FRAME sivuotsi.
         END.

         /* myyjä yhteensä */
         PUT STREAM tul
         skip(1)
         "*"
         SmName  format "x(30)"  AT 11
         (accum sub-total BY Customer.Salesman Invoice.AmtExclVAT)
                  format "zz,zzz,zz9.99-" AT 45 SKIP.
         rl = rl + 2.
         {Syst/uprfeed.i rl}
      END.
   END.

   /* Tarvitaanko uusi sivu */
   IF rl >= skayt1 - 2 THEN DO:
      {Syst/uprfeed.i rl}
      ASSIGN
        sl = sl + 1.
      view STREAM tul FRAME sivuotsi.  rl = 7.
   END.

   PUT STREAM tul
       skip(1)
       "**"
       "TOTAL" AT 11
       (accum total Invoice.AmtExclVAT) format "zz,zzz,zz9.99-" AT 45 SKIP.
   rl = rl + 2.
   {Syst/uprfeed.i rl}.

   MESSAGE "Report completed !" VIEW-AS ALERT-BOX.

   LEAVE LOOP.
END. /* LOOP */


HIDE FRAME rajat no-pause.

