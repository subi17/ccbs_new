/* ------------------------------------------------------------------------
  MODULE .......: nnlati4.p  (substitutes nnlati3.p )
  FUNCTION .....: Invoice statistics salesman/resellers
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 30.12.98 pt
  MODIFIED .....: 21.01.99 pt CustGroup
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

/* print-linemuuttujat */
{Syst/utumaa.i NEW }

assign tuni1 = "nnlati"
       tuni2 = "".

def var date1    as Date format "99-99-99" NO-UNDO.
def var date2    as Date format "99-99-99" NO-UNDO.
def var sm-code1 as c    format "x(8)"     no-undo init "1".
def var sm-code2 as c    format "x(8)"     no-undo init "999".
def var Reseller  as c    format "x(8)"     NO-UNDO.
DEF VAR CustGroup  LIKE CustGroup.CustGroup       NO-UNDO.
DEF VAR rsname   AS c                      NO-UNDO.
DEF VAR Calls    AS DE                     NO-UNDO.
DEF VAR nocalls  AS DE                     NO-UNDO.
DEF VAR rl      AS i                       NO-UNDO.
DEF VAR sl      AS i                       NO-UNDO.

form
   skip(1)
   "  Note:  This program prints out a summary report of Billed totals " SKIP
   "         sorted by Salesman / agent (reseller), from invoices from"  SKIP
   "         time Period determined below." skip(1)
   "         Report shows Billed Calls and Billed CONTRACT FEES"         SKIP
   "         separately." skip(1)
   "         Observe that dates below are  INVOICE dates,"               SKIP
   "         not CALL dates"      
     skip(1)
     "Invoices from .........:"  AT 15 date1 format "99-99-99" to 50 "-"
     date2  NO-LABEL SKIP
     "External Customer Group:"  AT 15 CustGroup AT 43
help "Code of an External Customer Group (EMPTY = none)" SKIP
     "Salesmen ..............:"  AT 15 sm-code1  to 50 "-"
     sm-code2 NO-LABEL SKIP
     "Agent/reseller ........:"  AT 15 Reseller
        help "One for certain, empty for all" TO 50 skip(2)
WITH
   width 80 COLOR value(cfc)
   title color value(ctc) " " + ynimi + " INVOICE STATISTICS SALESMAN/AGENT "   
   + string(pvm,"99-99-99") + " " NO-LABELS OVERLAY FRAME rajat.

form header

   fill("=",114) format "x(114)" SKIP
   ynimi at 2 format "x(25)" "SALES STATISTICS" at 40 date1 "-" date2
   "Page" at 105 sl format "ZZZZ9" SKIP
   "SALESMAN / RESELLER"  at 40 string(pvm,"99-99-99") AT 107 SKIP
   fill("=",114) format "x(114)" skip(1)
   "SmCode"           AT 3
   "Salesman's name"  AT 11
   "Agent's name"     AT 42
   "Calls"            TO 76
   "Contr.Fees"       TO 87
   "Inv ex VAT"       TO 98 SKIP
   fill("-",114) format "x(114)" SKIP
   WITH width 114 NO-LABEL no-box FRAME sivuotsi.

/* Get Date proposal */
ASSIGN date1  = pvm.
FIND LAST Invoice  USE-INDEX InvDate no-lock no-error.
IF NOT AVAIL Invoice THEN
FIND LAST Invoice no-lock no-error.
IF AVAIL Invoice THEN ASSIGN date1 = Invoice.InvDate.
date2 = date1.

cfc = "sel". RUN Syst/ufcolor.
LOOP:
repeat WITH FRAME rajat:
    ehto = 9. RUN Syst/ufkey.

    UPDATE
    date1
    date2   validate(input date2 >= input date1,"Invalid order of dates !")
    CustGroup validate(input CustGroup = "" OR 
                       can-find(CustGroup where CustGroup.CustGroup = 
                       input CustGroup),"Unknown group !")
    sm-code1 
    sm-code2 validate(INPUT sm-code2 >= INPUT sm-code1,
    "Invalid order of salesmen !")
    Reseller validate(input Reseller = "" OR  can-find(Reseller where 
                                       Reseller.Reseller = input Reseller),
                                       "Unknown reseller/agent !").

TOIMI:
   repeat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT LOOP.
      IF toimi = 8 THEN LEAVE LOOP.
      IF toimi = 5 THEN LEAVE TOIMI.
   END.


   /* Avataan striimi */
   ASSIGN tila = TRUE.
   {Syst/tmsreport.i "leave LOOP"}

   message "Printing ...".

   FOR
      EACH  Invoice  no-lock where
            Invoice.InvDate   >= date1          AND
            Invoice.InvDate   <= date2,

      EACH  InvRow of Invoice no-lock,

      FIRST Customer no-lock where
            Customer.CustNum   = Invoice.CustNum AND
            Customer.Salesman >= sm-code1       AND
            Customer.Salesman <= sm-code2       AND
           (if Reseller ne "" THEN
            Customer.Reseller  = Reseller ELSE TRUE)  AND

           (if CustGroup <> "" THEN
            can-find(CGMember where
                     CGMember.CustNum  = Customer.CustNum AND
                     CGMember.CustGroup = CustGroup)
            ELSE TRUE
           ) 
            ,
      FIRST Salesman no-lock where
            Salesman.Salesman = string(Customer.Salesman)
   BREAK
      BY Customer.Salesman
      BY Customer.Reseller
      BY InvRow.InvNum.

      IF first-of(InvRow.InvNum) THEN
      accumulate
      Invoice.AmtExclVAT ( sub-total BY Customer.Salesman
                                  BY Customer.Reseller).

      /* EACH ROW contains either Calls OR a contract fee */
      IF InvRow.FFRow THEN ASSIGN nocalls = InvRow.Amt Calls   = 0.
                         ELSE ASSIGN Calls   = InvRow.Amt nocalls = 0.

      accumulate Calls  ( sub-total BY Customer.Salesman
                                   BY Customer.Reseller)
                 nocalls( sub-total BY Customer.Salesman
                                   BY Customer.Reseller).

      /* Tarvitaanko uusi sivu */
      IF first(Customer.Salesman) THEN DO:
         ASSIGN rl = 7  sl = sl + 1.
         view STREAM tul FRAME sivuotsi.

         IF sl = 1 THEN DO:
            /* show print criteria on 1st page */

            PUT STREAM tul UNFORMATTED
            "External CustGroup ...: " 
            (if CustGroup = "" then "NONE" ELSE CustGroup)
            SKIP
            "Salesmen .............: " sm-code1 " - " sm-code2
            SKIP
            "Reseller .............: " 
            (if Reseller = "" then "ALL" ELSE Reseller)
            skip(1).

            rl = rl + 4.
         END.

      END.

      /* sub-totals BY reseller */
      IF last-of(Customer.Reseller) THEN DO:
         if Customer.Reseller = "" then assign rsname = "Direct Customers".
         ELSE DO:
            FIND Reseller where Reseller.Reseller = Customer.Reseller
            no-lock no-error.
            IF AVAIL Reseller THEN ASSIGN rsname = Reseller.RsName.
            else                 assign rsname = "?? Reseller " +
                                                 Customer.Reseller.
         END.

         /* Tarvitaanko uusi sivu */
         IF rl >= skayt1 THEN DO:
            {Syst/uprfeed.i rl}
            ASSIGN rl = 7 sl = sl + 1.
            view STREAM tul FRAME sivuotsi.
         END.

         PUT STREAM tul
         Customer.Salesman                       AT  3
         Salesman.SmName format "x(30)"       AT 11
         rsname           format "x(24)"       AT 42
         (accum sub-total BY Customer.Reseller Calls)
                          format "->,>>>,>>9"  TO 76
         (accum sub-total BY Customer.Reseller nocalls)
                          format "->,>>>,>>9"  TO 87
         (accum sub-total BY Customer.Reseller Invoice.AmtExclVAT)
                          format "->,>>>,>>9"  TO 98 SKIP.

         ASSIGN rl = rl + 1.
      END.


      /* sub-total BY Salesman */
      IF last-of (Customer.Salesman) THEN DO:
        /* Tarvitaanko uusi sivu */
         IF rl >= skayt1 - 2 THEN DO:
            {Syst/uprfeed.i rl}
            ASSIGN rl = 7  sl = sl + 1.
            view STREAM tul FRAME sivuotsi.
         END.

         /* Salesman total */
         PUT STREAM tul
           "*"                      AT 3
           SmName  format "x(30)"  AT 11
           "total"                  AT 42
         (accum sub-total BY Customer.Salesman Calls)
                          format "->,>>>,>>9"  TO 76
         (accum sub-total BY Customer.Salesman nocalls)
                          format "->,>>>,>>9"  TO 87
         (accum sub-total BY Customer.Salesman Invoice.AmtExclVAT)
                          format "->,>>>,>>9" to 98 " *"
         skip(1).
         rl = rl + 2.
      END.
   END.

   /* Tarvitaanko uusi sivu */
   IF rl >= skayt1 - 2 THEN DO:
      {Syst/uprfeed.i rl}
      ASSIGN  sl = sl + 1.
      view STREAM tul FRAME sivuotsi.  rl = 7.
   END.

   PUT STREAM tul
      skip(1)
      "**"
      "ALL SALESMEN"   AT 11
      "TOTALLY"        AT 42
      (accum total Calls)           format "->,>>>,>>9" TO 76
      (accum total nocalls)         format "->,>>>,>>9" TO 87
      (accum total Invoice.AmtExclVAT) format "->,>>>,>>9" to 98 " **" SKIP.
   rl = rl + 2.

   /* TO print the LAST page */
   PUT STREAM tul skip(spit1 - rl).

   MESSAGE "Report complete !" VIEW-AS ALERT-BOX.

   LEAVE LOOP.

END. /* LOOP */

HIDE FRAME rajat no-pause.
HIDE MESSAGE no-pause.

