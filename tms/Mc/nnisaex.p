/* --------------------------------------------------------------------------
  MODULE .......: nnisaex.p
  FUNCTION .....: Invoice statistics salesman/resellers, excel
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 30.12.98 pt
  MODIFIED .....: 09.02.99 pt aq-comm (individual commission-% on EACH cust)
                              + commission letters (optional)
                  22.02.99 kl print also totinv, cfees & cbase on paper report
                  17.05.00 kl FIRST BillItem, fChkPath
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}                      
{Func/function.i}
{Syst/utumaa.i new} 

assign tuni1 = "nnisaex"
       tuni2 = "".


DEF NEW shared STREAM excel.

DEF WORKFILE w-total
  FIELD Salesman  LIKE Salesman.Salesman
  FIELD Reseller  LIKE Reseller.Reseller
  FIELD w-totinv AS DE
  FIELD w-cfees  AS DE
  FIELD w-cbase  AS DE
  FIELD w-commi  AS DE.

def var date1    as Date format "99-99-99" NO-UNDO.
def var date2    as Date format "99-99-99" NO-UNDO.
def var Salesman1 as c    format "x(8)"     no-undo init "1".
def var Salesman2 as c    format "x(8)"     no-undo init "999".
def var Reseller  as c    format "x(8)"     NO-UNDO.
def var InvGroup  as c    format "x(8)"     NO-UNDO.
DEF VAR rsname   AS c                      NO-UNDO.
DEF VAR totinv   AS DE                     NO-UNDO.
DEF VAR cbase    AS DE                     NO-UNDO.
DEF VAR cfees    AS DE                     NO-UNDO.
DEF VAR tab      AS c                      NO-UNDO.
DEF VAR exdir    AS c                      NO-UNDO.
def var exFile   as c    format "x(40)"    NO-UNDO.
DEF VAR cperc    AS DE                     NO-UNDO.
DEF VAR commi    AS DE                     NO-UNDO.
DEF VAR xperc    AS c                      NO-UNDO.
def var print    as lo   format "Yes/No"   NO-UNDO.

DEF VAR rl       AS i                      NO-UNDO.
DEF VAR sl       AS i                      NO-UNDO.
DEF VAR rlx      AS i                      NO-UNDO.

tab = chr(9).

form
     skip(1)
     " Note: This program prints out a TAB separated summary File "
     "       (and optional commission summary letters on paper)"
     "       with customers' Billed totals during a time Period."  
     "       File is sorted by Salesman, by"
     "       reseller (agent) and by customer no." skip(1)
     "       Remember to enter INVOICE dates, not CALL dates."
     skip(1)

     "Invoices from .........:" at 16 date1 format "99-99-99"
     help "Earliest Date of an invoice" "-"
     date2  no-label help "Latest Date of an invoice" SKIP

     "Invoicing Group .......:" AT 16 InvGroup
     help "Invoicing Group Code; EMPTY: ALL"   SKIP

     "Salesmen ..............:" AT 16 Salesman1
     help "First Salesman Code" "-"
     Salesman2 no-label help "Last Salesman Code "SKIP
     "Agent Code ............:" AT 16 Reseller
     help "Code of Agent; empty: ALL agents"  SKIP
     "Commission letters ....:" AT 16 print
     help "Are separate commission summary letters to be Printed also (Y/N)"
     skip(1)

     "Output File:" at 16 exFile help "Name of output file"
     skip(1)
WITH
   width 80 COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " INVOICE SUMMARY SALESMAN/AGENT/CUSTOMER "
   + string(pvm,"99-99-99") + " " NO-LABELS OVERLAY FRAME rajat.

DO FOR TMSUser.
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = fChkPath(TMSUser.RepDir).
END.
assign exFile   = exdir + "inv-smag.txt".

/* Make Date proposal */
ASSIGN
print = FALSE
date1 = date(month(pvm),1,year(pvm)).
date2 = date1 + 35.
date2 = date(month(date2),1,year(date2)) - 1.

cfc = "sel". RUN ufcolor.
LOOP:
repeat WITH FRAME rajat:
    ehto = 9. RUN ufkey.

    UPDATE
    date1   
    date2 validate (input date2 >= input date1,"Invalid order !")
    InvGroup validate(input InvGroup = "" OR can-find(InvGroup where
            InvGroup.InvGroup = input InvGroup),"Unknown group !")
    Salesman1 
    Salesman2 
    Reseller  validate(input Reseller = "" OR can-find(Reseller where
                       Reseller.Reseller = input Reseller),
                       "Unknown reseller/agent !")
    print
    exFile.

TOIMI:
   repeat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT LOOP.
      IF toimi = 8 THEN LEAVE LOOP.
      IF toimi = 5 THEN DO:
         IF print THEN DO:
            /* ask FOR printer IF letters are wanted */
            ASSIGN tila = TRUE.
            {Syst/tmsreport.i "next toimi"}
         END.
         LEAVE TOIMI.
      END.   
   END.

   OUTPUT STREAM excel TO value(exFile).

   PUT STREAM excel UNFORMATTED
      ynimi  tab "Invoice summary by Salesman / agent / customer".
      RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
      "All amounts are from invoice rows; amounts are rounded and ex VAT".
      RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
      "Invoicing Group" tab.
      if InvGroup = "" then put stream excel "ALL".
      ELSE PUT STREAM excel UNFORMATTED InvGroup.
      RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
      "Salesmen:" tab Salesman1 " - " Salesman2.
      RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
      "Invoices written:" tab date1 format "99.99.9999" " - "
                              date2 format "99.99.9999".
      RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
      "Reseller: " tab.
       if Reseller = "" then put stream excel "ALL".
       ELSE PUT STREAM excel UNFORMATTED Reseller.
      RUN uexskip(3).

   PUT STREAM excel UNFORMATTED
   "SmCode"     tab
   "Salesman"   tab
   "AgCode"     tab
   "Agent"      tab
   "CustNo"     tab
   "Customer"   tab
   "InvNo."     tab
   "InvDate"    tab
   "TotInv"     tab
   "ContFees"   tab
   "C.based on" tab
   "Comm%"      tab
   "Commission".
   RUN uexskip(2).

   message "Printing ...".

   FOR
      EACH  Invoice  no-lock where
            Invoice.InvDate   >= date1          AND
            Invoice.InvDate   <= date2,

      EACH  InvRow of Invoice no-lock,

      FIRST BillItem no-lock where
            BillItem.BillCode = InvRow.BillCode,

      FIRST Customer no-lock where
            Customer.CustNum   = Invoice.CustNum AND
            Customer.Salesman >= Salesman1       AND
            Customer.Salesman <= Salesman2       AND
           (if Reseller ne "" THEN
            Customer.Reseller  = Reseller ELSE TRUE) AND
           (if InvGroup ne "" THEN
            Customer.InvGroup  = InvGroup ELSE TRUE),
      FIRST Salesman no-lock where
            Salesman.Salesman = string(Customer.Salesman)
   BREAK
      BY Customer.Salesman
      BY Customer.Reseller
      BY Customer.CustNum
      BY InvRow.InvNum.

      if print and first-of(Customer.Reseller) and Customer.Reseller ne "" THEN DO:

         /*******************************
         * Print header on a NEW letter *
         *******************************/

         FIND Reseller where Reseller.Reseller = Customer.Reseller no-lock.

form header
      pvm format "99.99.9999" AT 62
      "Page"                  AT 83 
      sl  format "z9" 
      skip(5)
      Reseller.RsName          AT 62 SKIP
      Reseller.Address[1]    AT 62 SKIP  
      Reseller.Address[2]    AT 62 SKIP  
      Reseller.Address[3]    AT 62 skip(3)

      "SUMMARY OF COMMISSIONS FROM INVOICES WRITTEN DURING " AT 5
      date1 format "99.99.9999" " - " 
      date2 format "99.99.9999" 
      "  (AMOUNTS ARE EX VATAmt)"
      skip(1)

      "Cust."           TO 10
      "Customer's"      AT 12 
      "Invoice"         TO 50
      "Invoice"         AT 52
      "Invoiced"        TO 76
      "Contains"        TO 86
      "Commiss"         TO 96
      "Comm."           TO 102
      "Commission"      TO 113
      SKIP

      "Number"          TO 10
      "Name"            AT 12
      "Number"          TO 50
      "Date"            AT 52
      "Total Kr"        TO 76
      "C-Fees"          TO 86
      "based on"        TO 96
      "%%%"             TO 102
      "kr"              TO 113
      fill("-",109) at 5 format "x(109)"
      SKIP
      WITH NO-LABEL no-box width 130 FRAME let-hdr.

         ASSIGN rl = 19 sl = 1 rlx = 0.
         view STREAM tul FRAME let-hdr.
      END.

      /* accumulate total Billed amount */
      IF first-of(InvRow.InvNum) THEN DO:

         totinv = Invoice.AmtExclVAT.
         accumulate
         totinv ( sub-total BY Customer.Salesman
                            BY Customer.Reseller
                            BY InvRow.InvNum).
      END.

      /* is this ROW a contract fee ROW ? */
      IF InvRow.FFRow = TRUE THEN cfees = InvRow.Amt.
                                ELSE cfees = 0.

      accumulate cfees  ( sub-total BY Customer.Salesman
                                    BY Customer.Reseller
                                    BY InvRow.InvNum).


      cbase = InvRow.Amt.

      accumulate cbase  ( sub-total BY Customer.Salesman
                                    BY Customer.Reseller
                                    BY InvRow.InvNum).

      /* sub-totals BY invoice */
      IF last-of(InvRow.InvNum) THEN DO:
         if Customer.Reseller = "" then assign rsname = "Direct Customers".
         ELSE DO:
            FIND Reseller where Reseller.Reseller = Customer.Reseller
            no-lock no-error.
            IF AVAIL Reseller THEN ASSIGN rsname = Reseller.RsName.
            else                 assign rsname = "?? Reseller " +
                                                 Customer.Reseller.
         END.

         cbase   = round((accum sub-total BY InvRow.InvNum cbase),0).
         cfees   = round((accum sub-total BY InvRow.InvNum cfees),0).
         totinv  = round((accum sub-total BY InvRow.InvNum totinv),0).

         accumulate commi(sub-total BY Customer.Salesman
                                    BY Customer.Reseller).

         PUT STREAM excel UNFORMATTED
         Customer.Salesman                   tab
         Salesman.SmName                  tab
         Customer.Reseller                   tab
         rsname                            tab
         Customer.CustNum                    tab
         Customer.CustName                   tab
         InvRow.InvNum                     tab
         Invoice.InvDate format "99.99.9999" tab
         totinv  format "->>>>>>9"         tab
         cfees   format "->>>>>>9"         tab
         cbase   format "->>>>>>9"         tab
         xperc                             tab
         commi.
         RUN uexskip(1).

         if print and Customer.Reseller ne "" THEN DO:

            IF rl > skayt1 THEN DO:
               PUT STREAM tul UNFORMATTED chr(12).
               sl = sl + 1.
               view STREAM tul FRAME let-hdr.
               rl = 19. rlx = 0.
            END.   

            PUT STREAM tul
            Customer.CustNum   format "zzzzzz9"  TO 10
            Customer.CustName  format "x(30)"    AT 12
            Invoice.InvNum                      TO 50
            Invoice.InvDate    format "99.99.99" AT 52
            totinv           format "->>>,>>9" TO 76
            cfees            format "->>>,>>9" TO 86
            cbase            format "->>>,>>9" TO 96
            commi            format "->>>,>>9" TO 113
            SKIP.

            rl = rl + 1.    rlx = rlx + 1.
            IF rlx = 5 THEN DO:
               PUT STREAM tul skip(1).
               rl = rl + 1. rlx = 0.
            END.   
         END.

         accumulate commi(sub-total BY Customer.Reseller).
      END.

      IF last-of(Customer.Reseller) THEN DO:
         cbase   = round((accum sub-total BY Customer.Reseller cbase),0).
         cfees   = round((accum sub-total BY Customer.Reseller cfees),0).
         totinv  = round((accum sub-total BY Customer.Reseller totinv),0).
         commi   = round((accum sub-total BY Customer.Reseller commi),0).

         PUT STREAM excel UNFORMATTED
         tab tab "*" tab "TOTAL" tab tab tab tab tab
         totinv tab
         cfees  tab
         cbase  tab tab
         commi.
         RUN uexskip(2).

         /* make a summary record FOR further reporting */
         if Customer.Reseller ne "" THEN DO:
            CREATE w-total.
            ASSIGN
            w-total.Salesman  = Customer.Salesman
            w-total.Reseller  = Customer.Reseller 
            w-total.w-totinv = totinv
            w-total.w-cfees  = cfees
            w-total.w-cbase  = cbase
            w-total.w-commi  = commi.
         END.

         /* total & form feed on letter */
         if print and Customer.Reseller ne "" THEN  DO:
            IF rl > skayt1 - 2 THEN DO:
               PUT STREAM tul UNFORMATTED chr(12).
               sl = sl + 1.
               view STREAM tul FRAME let-hdr.
               rl = 19. rlx = 0.
            END.   
            PUT STREAM tul UNFORMATTED 
            fill("-",109) format "x(109)" AT 5 SKIP
            "****** T O T A L  ( EX VATAmt )" AT 5
            totinv format "->,>>>,>>9" TO 76
            cfees  format "->,>>>,>>9" TO 86
            cbase  format "->,>>>,>>9" TO 96
            commi  format "->,>>>,>>9" TO 113
            chr(12).
         END.

      END.

   END.

   /************************************
   * FINALLY we print out a summary of *
   * resellers                         *
   ************************************/

   RUN uexskip(5).

   put stream excel unformatted "TOTALS BY RESELLER:". RUN uexskip(2).

   PUT STREAM excel UNFORMATTED
   "SmanCode"    tab
   "SmanName"    tab
   "ResCode"     tab
   "ResName"     tab
   "InvTotal"    tab
   "C-Fees"      tab
   "CBased"      tab
   "CommKr"      .
   RUN uexskip(1).

   FOR EACH  w-total,
       FIRST Reseller   where Reseller.Reseller   = w-total.Reseller no-lock,
       FIRST Salesman where Salesman.Salesman = w-total.Salesman no-lock

   BREAK
   BY w-total.Salesman
   BY w-total.Reseller.

      IF first-of(w-total.Salesman) THEN RUN uexskip(1).

      PUT STREAM excel UNFORMATTED
      Salesman.Salesman    tab
      Salesman.SmName    tab
      Reseller.Reseller      tab
      Reseller.RsName      tab
      w-total.w-totinv    tab
      w-total.w-cfees     tab
      w-total.w-cbase     tab
      w-total.w-commi     .
      RUN uexskip(1).
   END.           

   OUTPUT STREAM excel CLOSE.

   IF print THEN DO:
      ASSIGN tila = FALSE.
      {Syst/tmsreport.i}
   END.   

   message "File" exfile "is ready - press ENTER !".
   PAUSE no-message.
   LEAVE LOOP.

END. /* LOOP */

HIDE FRAME rajat no-pause.
HIDE MESSAGE no-pause.

