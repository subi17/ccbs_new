/* --------------------------------------------------------------------------
  MODULE .......: nnparex.p
  FUNCTION .....: Invoice statistics salesman/resellers, excel
  APPLICATION ..: NN
  AUTHOR .......: HT
  CREATED ......: 07.05.00 ht
  MODIFIED .....:
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

{Func/function.i}

DEF NEW shared STREAM excel.

def var date1    as Date format "99-99-99" NO-UNDO.
def var date2    as Date format "99-99-99" NO-UNDO.
def var Salesman  as c    format "x(8)"     NO-UNDO.
DEF VAR SmName  LIKE Salesman.SmName     NO-UNDO.
def var Reseller  as c    format "x(8)"     NO-UNDO.
def var InvGroup  as c    format "x(8)"     NO-UNDO.
DEF VAR rsname   AS c                      NO-UNDO.
DEF VAR Calls    AS DE                     NO-UNDO.
DEF VAR totinv   AS DE                     NO-UNDO.
DEF VAR cfees    AS DE                     NO-UNDO.
DEF VAR tab      AS c                      NO-UNDO.
DEF VAR exdir    AS c                      NO-UNDO.
def var exfile   as c    format "x(40)"    NO-UNDO.
DEF VAR cperc    AS DE                     NO-UNDO.
DEF VAR xperc    AS c                      NO-UNDO.

DEF VAR rl       AS i                      NO-UNDO.
DEF VAR sl       AS i                      NO-UNDO.
DEF VAR rlx      AS i                      NO-UNDO.

tab = chr(9).

form
     skip(1)
     " Note: This program prints out a TAB separated summary File "
     "       with customers' Billed totals during a time Period."  
     "       File is sorted by Salesman and by reseller (agent)." skip(1)
     "       Remember to enter INVOICE dates, not CALL dates."
     skip(1)

     "Invoices from .........:" at 15 date1 format "99-99-99"
     help "Earliest Date of an invoice" "-"
     date2  no-label help "Latest Date of an invoice" SKIP

     "Invoicing Group .......:" AT 15 InvGroup
     help "Invoicing Group Code; EMPTY: ALL"   SKIP

     "Salesman ..............:" AT 15 Salesman
     help "Salesman Code"  SmName NO-LABEL     skip(4)

     "Output File:" at 15 exfile help "Name of output file"
     skip(1)
WITH
   width 80 COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " INVOICE SUMMARY SALESMAN/AGENT "
   + string(pvm,"99-99-99") + " " NO-LABELS OVERLAY FRAME rajat.

DO FOR TMSUser.
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = fChkPath(TMSUser.RepDir).
END.

assign exfile   = exdir + "partcomm.txt".

/* Make Date proposal */
ASSIGN
date1 = date(month(pvm),1,year(pvm)).
date2 = date1 + 35.
date2 = date(month(date2),1,year(date2)) - 1.

cfc = "sel". RUN Syst/ufcolor.p.
LOOP:
repeat WITH FRAME rajat:
    ehto = 9. RUN Syst/ufkey.p.

    UPDATE 
    date1    
    date2    validate(input date2 >= input date1, "Invalid order !")
    InvGroup validate(input InvGroup = "" OR can-find(InvGroup where
            InvGroup.InvGroup = input InvGroup),"Unknown group !")
    Salesman validate(can-find(Salesman where 
            Salesman.Salesman = input Salesman),"Unknown Salesman !")
    exfile EDITING:
       READKEY.
       IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME rajat:
          PAUSE 0.
          if frame-field = "sm-code" THEN DO:
             disp LC(exdir + "partcomm." + input Salesman + ".txt") 
             @ exfile.
             FIND FIRST Salesman where Salesman.Salesman = INPUT Salesman 
             no-lock no-error.
             SmName = Salesman.SmName.
             DISP SmName WITH FRAME rajat.
          END.
       END.
       APPLY LASTKEY.
    END.

TOIMI:
   repeat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT LOOP.
      IF toimi = 8 THEN LEAVE LOOP.
      IF toimi = 5 THEN DO:
         LEAVE TOIMI.
      END.   
   END.

   OUTPUT STREAM excel TO value(exfile).

   PUT STREAM excel UNFORMATTED
      ynimi  tab "Invoice summary by Salesman / agent".
      RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
      "All amounts are from invoice rows; amounts are rounded and ex VAT".
      RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
      "Invoicing Group" tab.
      if InvGroup = "" then put stream excel "ALL".
      ELSE PUT STREAM excel UNFORMATTED InvGroup.
      RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
      "Salesman:" tab Salesman tab SmName.
      RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
      "Invoices written:" tab date1 format "99.99.9999" " - "
                              date2 format "99.99.9999".
      RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
   tab   
   "Partner name" tab
   "Calls"        tab
   "Contr.Fees"   tab
   "Inv.ex.VAT"   tab.
   RUN Syst/uexskip.p(2).

   message "Printing ...".

   FOR
      EACH  Reseller no-lock where
            Reseller.Salesman = Salesman,

      EACH Customer no-lock where 
           Customer.Reseller = Reseller.Reseller AND
           (if InvGroup ne "" THEN
            Customer.InvGroup  = InvGroup ELSE TRUE),

      EACH  Invoice of Customer no-lock where
            Invoice.InvDate   >= date1        AND
            Invoice.InvDate   <= date2,

      EACH  InvRow of Invoice no-lock

   BREAK
      BY Customer.Reseller
      BY InvRow.InvNum.

      /* accumulate total Billed amounts */
      IF first-of(InvRow.InvNum) THEN DO:
         totinv  = Invoice.AmtExclVAT.
         accumulate totinv ( sub-total BY Customer.Reseller).
         accumulate totinv ( total). 
      END.

      /* is this ROW a call ROW ? */
      IF InvRow.FFRow = FALSE THEN Calls = InvRow.Amt.
                                 ELSE Calls = 0.

      accumulate Calls  ( sub-total BY Customer.Reseller).
      accumulate Calls  ( total).

      /* is this ROW a contract fee ROW ? */
      IF InvRow.FFRow = TRUE THEN cfees = InvRow.Amt.
                                ELSE cfees = 0.

      accumulate cfees  ( sub-total BY Customer.Reseller).
      accumulate cfees  ( total).

      IF last-of(Customer.Reseller) THEN DO:
         Calls   = round((accum sub-total BY Customer.Reseller Calls),0).
         cfees   = round((accum sub-total BY Customer.Reseller cfees),0).
         totinv  = round((accum sub-total BY Customer.Reseller totinv),0).

         PUT STREAM excel UNFORMATTED
                        tab
         Reseller.RsName tab
         Calls          tab
         cfees          tab
         totinv         tab.
         RUN Syst/uexskip.p(1).
      END.
   END.

   /************************************
   * FINALLY we print out a summary of *
   * Salesman                          *
   ************************************/

   Calls   = round((accum total Calls),0).
   cfees   = round((accum total cfees),0).
   totinv  = round((accum total totinv),0).

   RUN Syst/uexskip.p(1).

   PUT STREAM excel UNFORMATTED
   "All Salesman"    tab
   "Total"           tab
   Calls             tab
   cfees             tab
   totinv            tab.
   RUN Syst/uexskip.p(1).

   OUTPUT STREAM excel CLOSE.

   PAUSE 0.
   message "File" exfile "is ready - press ENTER !".
   PAUSE no-message.
   LEAVE LOOP.

END. /* LOOP */

HIDE FRAME rajat no-pause.
HIDE MESSAGE no-pause.

