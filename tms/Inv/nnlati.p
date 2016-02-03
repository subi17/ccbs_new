/* -----------------------------------------------------
  MODULE .......: nnlati.p
  FUNCTION .....: Invoice statistics Salesman / reseller / customer
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 29.11.96
  MODIFIED .....: 24.02.97 pt - laasno muutettu CustNum:ksi
                  04.05.97 pt - korjattu myyjArajaus FOR EACH:iin
                  22.01.98 kl - ohti into Salesman
                  29.01.98 kl - as-sm-code => Salesman
                  01.06.98 kl - reseller statistics
                  03.08.98 kl - NEW page & grouping
                  21.01.99 pt - CustGroup
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

/* print-linemuuttujat */
{Syst/utumaa.i NEW }

assign tuni1 = "nnlati"
       tuni2 = "".

def var date1    as Date format "99-99-99" NO-UNDO.
def var date2    as Date format "99-99-99" NO-UNDO.
def var sm-code1 as c    format "x(8)"     no-undo init "1".
def var sm-code2 as c    format "x(8)"     no-undo init "999".
def var Reseller  as c    format "x(8)".
DEF VAR CustGroup  LIKE CustGroup.CustGroup NO-UNDO.

DEF VAR rlx AS i  NO-UNDO.
DEF VAR rl  AS i  NO-UNDO.
DEF VAR sl  AS i  NO-UNDO.
DEF VAR fst AS lo NO-UNDO.

form
     skip(1)
     "    Instruction:  This program prints out a report with statistics "
     "                  of invoicing during the time Period determined"
     "                  below.  Observe that dates below are INVOICE dates,"
     "                  not CALL dates."          skip(1)
     skip(2)
     "Invoices from ..:" at 19 date1 format "99-99-99" to 44 "-"
     date2  NO-LABEL SKIP
     "Ext.CustGroup ..:" AT 19 CustGroup 
     help "Code of an External Customer Group (EMPTY: none)"
     SKIP
     "Salesmen .......:" at 19 sm-code1  to 44 "-"
     sm-code2 NO-LABEL SKIP
     "Reseller/agent .:" AT 19 Reseller 
        help "One for certain, empty for all" TO 44 skip(5)
WITH
   width 80 COLOR value(cfc)
   title color value(ctc) " " + ynimi + " INVOICE STATISTICS SALESMAN/CUSTOMER "    + string(pvm,"99-99-99") + " " NO-LABELS OVERLAY FRAME rajat.

form header

   fill("=",114) format "x(114)" SKIP
   ynimi at 2 format "x(25)" "INVOICE STATISTICS" at 40 date1 "-" date2
   "Page" at 105 sl format "ZZZZ9" SKIP
   "SALESMAN / CUSTOMER"  at 40 string(pvm,"99-99-99") AT 107 SKIP
   fill("=",114) format "x(114)" skip(1)
   "CustNr" at 4 "Customer name" at 11 "Invoice without VAT" TO 57 SKIP
   fill("-",114) format "x(114)" SKIP
   WITH width 114 NO-LABEL no-box FRAME sivuotsi.

/* Get Date proposal */
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
    date2
    CustGroup    sm-code1 
    sm-code2     Reseller
WITH FRAME rajat EDITING:
   READKEY. 
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
         IF FRAME-FIELD = "date2" THEN DO:
            IF INPUT date2 < INPUT date1 THEN DO:
               MESSAGE "Invalid Order !".
               NEXT-PROMPT date1.
               NEXT.
            END.
         END.
         IF FRAME-FIELD = "CustGroup" AND INPUT Custgroup NE "" THEN DO:
            IF NOT CAN-FIND(CustGroup WHERE CustGroup.CustGroup =
                                      INPUT CustGroup)
            THEN DO:
               MESSAGE "Unknown group !".
               NEXT.
            END.
         END.
         IF FRAME-FIELD = "sm-code2" THEN DO:
            IF INPUT sm-code2 < INPUT sm-code1 THEN DO:
               MESSAGE "Invalid order !".
               NEXT.
            END.
         END.   
         IF FRAME-FIELD = "Reseller" AND INPUT Reseller NE "" THEN DO:
            IF NOT CAN-FIND(Reseller where Reseller.Reseller =
                                    INPUT Reseller)
            THEN DO:
               MESSAGE "Unknown reseller !".
               NEXT.
            END.
         END.
      END.
   APPLY LASTKEY.
END.

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

   message "Printing, break = ESC".

   fst = TRUE.

   FOR
      EACH  Invoice  no-lock where
            Invoice.InvDate   >= date1          AND
            Invoice.InvDate   <= date2,
      FIRST Customer no-lock where
            Customer.CustNum   = Invoice.CustNum AND
            Customer.Salesman >= sm-code1       AND
            Customer.Salesman <= sm-code2       AND
           (if Reseller ne "" THEN
            Customer.Reseller  = Reseller ELSE TRUE)  AND
           (if CustGroup ne "" THEN can-find(CGMember where CGMember.CustNum =
            Customer.CustNum AND CGMember.CustGroup = CustGroup) ELSE TRUE),
      FIRST Salesman no-lock where
            Salesman.Salesman = string(Customer.Salesman)
   BREAK
      BY Customer.Salesman
      BY Customer.Reseller
      BY Customer.CustName + string(Customer.CustNum).

      accumulate
         Invoice.AmtExclVAT (sub-total BY Customer.Salesman  
                                    BY Customer.CustName +
                                       string(Customer.CustNum)
                                    BY Customer.Reseller).

      IF first-of(Customer.Salesman) THEN DO:
         IF fst THEN fst = FALSE.
         ELSE PUT STREAM tul skip(spit1 - rl).
         ASSIGN sl = sl + 1 rl = 7 rlx = 0.
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

         PUT STREAM tul
            "Salesman:"                    AT 1
            Customer.Salesman format "x(8)"  AT 11
            SmName         format "x(20)" AT 20
            skip(1).
         rl = rl + 2.
      END.

      /* NEXT page ? */
      IF rl >= skayt1 THEN DO:
         PUT STREAM tul skip(spit1 - rl).
         ASSIGN rl = 7  sl = sl + 1.
         view STREAM tul FRAME sivuotsi.
      END.

      IF first-of(Customer.Reseller) THEN DO:
         if Customer.Reseller = "" THEN
            PUT STREAM tul UNFORMATTED
               "Salesman's direct customers" AT 6 skip(1).
         ELSE DO:
            FIND Reseller where Reseller.Reseller = Customer.Reseller 
            no-lock no-error.
            IF AVAIL Reseller THEN
               PUT STREAM tul UNFORMATTED
                  "Resel/ag:"                      AT 6
                  Reseller.Reseller   format "x(8)"   AT 16
                  Reseller.RsName   format "x(20)"  AT 26
                  skip(1).
            ELSE
               PUT STREAM tul UNFORMATTED
                 "!! UNKNOWN RESELLER/AGENT !!"
                  skip(1).
         END.
         ASSIGN rl = rl + 2 rlx = 0.
      END.

      /* tulostetaan asiakkaan line */
      IF last-of(Customer.CustName + string(Customer.CustNum)) THEN DO:
         IF rlx >= 5 THEN DO:
            PUT STREAM tul skip(1).
            ASSIGN rl = rl + 1 rlx = 0.
         END.
         /* NEXT page ? */
         IF rl >= skayt1 THEN DO:
            PUT STREAM tul skip(spit1 - rl).
            ASSIGN rl = 7  sl = sl + 1.
            view STREAM tul FRAME sivuotsi.
         END.
         PUT STREAM tul
            Customer.CustNum   format "zzzzzz9" AT 3
            CustName  format "x(30)"   AT 11
            (accum sub-total BY Customer.CustName + string(Customer.CustNum) 
                 Invoice.AmtExclVAT)
                     format "zz,zzz,zz9.99-" AT 45 SKIP.
         ASSIGN rl = rl + 1 rlx = rlx + 1.
      END.

      IF last-of(Customer.Reseller) THEN DO:
        /* NEXT page ? */
         IF rl >= skayt1 - 2 THEN DO:
            PUT STREAM tul skip(spit1 - rl).
            ASSIGN rl = 7 sl = sl + 1.
            view STREAM tul FRAME sivuotsi.
         END.

         FIND Reseller where Reseller.Reseller = Customer.Reseller 
         no-lock no-error.

         /* reseller total */
         put stream tul skip(1) "*".
         IF AVAIL Reseller THEN 
            put stream tul Reseller.RsName  format "x(30)" AT 11.
         ELSE
            put stream tul "Direct customers" format "x(30)" AT 11.
         PUT STREAM tul 
            (accum sub-total BY Customer.Reseller Invoice.AmtExclVAT)
             format "zz,zzz,zz9.99-" AT 45 skip(1).
         rl = rl + 2.
      END.

      IF last-of(Customer.Salesman) THEN DO:
        /* NEXT page ? */
         IF rl >= skayt1 - 2 THEN DO:
            PUT STREAM tul skip(spit1 - rl).
            ASSIGN rl = 7 sl = sl + 1.
            view STREAM tul FRAME sivuotsi.
         END.

         /* Salesman total */
         PUT STREAM tul
           skip(1)
           "**"
           SmName  format "x(30)"  AT 11
           (accum sub-total BY Customer.Salesman Invoice.AmtExclVAT)
            format "zz,zzz,zz9.99-" AT 45 SKIP.
         rl = rl + 3.
      END.

   END.

   /* NEXT page ? */
   IF rl >= skayt1 - 2 THEN DO:
      PUT STREAM tul skip(spit1 - rl).
      ASSIGN rl = 7 rlx = 0 sl = sl + 1.
      view STREAM tul FRAME sivuotsi.
   END.

   PUT STREAM tul
      skip(1)
      "***"
      "TOTALT" AT 11
      (accum total Invoice.AmtExclVAT) format "zz,zzz,zz9.99-" AT 45 SKIP.
   rl = rl + 2.

   /* TO print the LAST page */
   PUT STREAM tul skip(spit1 - rl).

   MESSAGE "Report completed !" VIEW-AS ALERT-BOX.

   LEAVE LOOP.

END. /* LOOP */
HIDE MESSAGE no-pause.

HIDE FRAME rajat no-pause.

