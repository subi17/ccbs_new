/* -----------------------------------------------------------------
  MODULE .......: NNBSXOR.P
  TASK .........: creates an XOR File of payments
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 06.08.1998
  CHANGED ......: 12.08.1998 NEW layout on OUTPUT File:
                             acctNo/cost center/amount/invNo/CustNo/Date
                  02.02.2000 jp UPDATE of datelimit(TMSParam) added 
                  05.05.2000 ht continued AND MESSAGE added

  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i} /* katun = "eka". */

DEF WORKFILE sums
   field accno   as i format "zzz9"
   FIELD place   AS c 
   FIELD dte     AS DA       
   FIELD sum     AS DE
   FIELD cucode  AS c.

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
def var exdeci    as lo format "Period/Comma" NO-UNDO.
def var ok        as lo format "Yes/No"      NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR csum      AS c  NO-UNDO.
DEF VAR xtot      AS DE NO-UNDO.
DEF VAR tab       AS c  NO-UNDO.
def var InvGroup   like  InvGroup.InvGroup   init "T1E".
DEF VAR sum       AS DE NO-UNDO EXTENT 4.
DEF VAR acno      AS i  NO-UNDO EXTENT 4.
DEF VAR pr-code   AS c  NO-UNDO.
DEF VAR ks-code   AS c  NO-UNDO.
DEF VAR Salesman   AS c  NO-UNDO.
DEF VAR Currency   AS c  NO-UNDO.
DEF VAR kst       AS c  NO-UNDO.
DEF VAR TransFile     AS i  NO-UNDO.
DEF VAR DLimit    AS DA NO-UNDO.

DEF NEW shared STREAM excel.

{Func/tmsparam.i DateLimit return}. DLimit = TMSParam.DateVal.

MESSAGE
"NOTE !" skip(1)
"Payments till date" DLimit skip
"have already been transferred into XOR"
VIEW-AS ALERT-BOX information.

/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.

/* initial values */
tab      = chr(9).
exName   = exdir + "/" + "xor-pay.txt".


form
   skip(1)
"  Information:  This program writes a summary of all payments of bills, in"
"                customer AccNum receivables, for XOR, as a TAB-separated"
"                ascii-file."                    
skip(1)
"                File's Name is" exname format "x(35)" no-label skip(2)
"                Payments registered .:" exdate1 format "99-99-99"
help "Earliest registration day of payment"
"-" exdate2 format "99-99-99" help "Latest registration day of payment"  skip
"                Inv.group ...........:" InvGroup
help "Invoicing group's code, empty for all"  SKIP
"                Decimal separator ...:" exdeci help "Period/Comma" skip(6)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " XOR-SUMMARY OF PAYMENTS WITH ACCOUNTS " +
   string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

exdate2 = date(month(TODAY),1,year(TODAY)) - 1.
exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN ufcolor.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN ufkey.
   DISP exName.
   UPDATE
      exName
      exdate1  validate(exdate1 ne ?,"Give first Date !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid order !")
      InvGroup  validate(InvGroup = "" OR can-find(InvGroup where
                                 InvGroup.InvGroup = InvGroup),
                                 "Group does not exist !")
      exdeci
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

task:
   repeat WITH FRAME start:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   OUTPUT STREAM excel TO value(exName).

   /* get a consecutive number FOR xfer File */
   DO FOR FileExpLog:
      find first FileExpLog where TransType = "XOR-PAY" no-lock no-error.
      IF AVAIL FileExpLog THEN TransFile = FileExpLog.TransNum + 1.
      ELSE                 TransFile = 1.

      CREATE FileExpLog.
      ASSIGN
      FileExpLog.TransType = "XOR-PAY"
      FileExpLog.TransDate = pvm
      FileExpLog.TransNum   = TransFile.
   END.   

   PUT STREAM excel UNFORMATTED
   "%FILE ID " 
   string(TransFile,"99999999")
   " CREATED "
   string(pvm,"99.99.9999")
   " AT "
   string(time,"hh:mm:ss").

   RUN uexskip(2).

   PUT STREAM excel UNFORMATTED
      "Kontonr" tab "Kst" tab "Belopp" tab "Faktnr" tab "Kund" tab "Datum".
   RUN uexskip(1).


   message "Processing ...".                      
   xtot = 0.

   FOR
      EACH  Payment no-lock where
            Payment.PaymDate >= exdate1        AND
            Payment.PaymDate <= exdate2,
      FIRST Customer no-lock where
            Customer.CustNum = Payment.CustNum AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup
            ELSE TRUE),
      FIRST Salesman no-lock where
            Salesman.Salesman = Customer.Salesman,
      FIRST Salesoffice no-lock where
            Salesoffice.SalesOffice = Salesman.SalesOffice

   BREAK
      BY Payment.PaymDate.

      /******************************************
      * There are MAX 10 bookings on accounts   *
      * on EACH payment record                  *
      ******************************************/

      Currency = string(Customer.CustNum,"9999999").
      /* Size digit */
      if Customer.Size = "L" then Currency = Currency + "1".  ELSE
      if Customer.Size = "M" then Currency = Currency + "2".  ELSE
      if Customer.Size = "S" then Currency = Currency + "3".
      /* connection digit */
      Currency = Currency + string(Customer.ConnType,"0/5").

      DO i = 1 TO 10.

         IF Payment.AccNum[i] = 0 THEN NEXT.

         if Payment.AccNum[i] > 2999 then kst     = string(Salesoffice.CostCentre,"999").
         else                     kst     = "000".

         xtot = xtot + Payment.Posting[i].

         case Payment.AccNum[i]:

            when 1510 OR when 1511 THEN DO:

               IF Payment.AccNum[i] > 2999 THEN 
                  kst = string(Salesoffice.CostCentre,"999").
               ELSE
                  kst = "000".

               csum = string(Payment.Posting[i],"-9999999999.99").
               if not exdeci then substring(csum,12,1) = ",".

               PUT STREAM excel UNFORMATTED 

                  Payment.AccNum[i]       format "9999"       tab   /* acc. nr. */
                  kst             format "xxx"        tab   /* cost center */
                  csum            format "x(14)"      tab   /* amount      */
                  Payment.InvNum  format "99999999"   tab   /* inv. nr.    */
                  Currency         format "x(9)"       tab   /* cust. nr. + xx */
                  Payment.PaymDate format "99.99.9999" tab.  /* registr. Date  */

               RUN uexskip(1).

            END.

            otherwise DO:

               FIND FIRST sums where 
                          sums.dte   = Payment.PaymDate AND
                          sums.accno = Payment.AccNum[i]
               no-error.

               IF NOT AVAIL sums THEN DO:
                  CREATE sums.
                  ASSIGN
                     sums.accno  = Payment.AccNum[i]
                     sums.dte    = Payment.PaymDate
                     sums.place  = kst
                     sums.cucode = Currency.
               END.

               sums.sum = sums.sum + Payment.Posting[i].

            END.

         END.

      END. /* DO i = 1 TO 10 */  

      IF last-of(Payment.PaymDate) THEN DO:

         FOR EACH sums 
         BY sums.dte
         BY sums.accno:

            csum = string(sums.sum,"-9999999999.99").
            if not exdeci then substring(csum,12,1) = ",".

            PUT STREAM excel UNFORMATTED 

               sums.accno   format "9999"        tab   /* AccNum number */
               sums.place   format "xxx"         tab   /* cost center    */
               csum         format "x(14)"       tab   /* amount         */
               tab tab
               sums.dte     format "99.99.9999"  tab.  /* registration Date */.

            RUN uexskip(1).

            DELETE sums.

         END.   

      END.

   END.      

   /* total sum (should be 0.00) */
   csum = string(xtot,"-9999999999.99").
   if not exdeci then substr(csum,12,1) = ",".

   PUT STREAM excel UNFORMATTED
   "%KONTROLLBELOPP "  tab
   csum.

   RUN uexskip(1).

   OUTPUT STREAM excel CLOSE.

   /* UPDATE datelimit PARAMETER */
   FIND FIRST TMSParam WHERE TMSParam.ParamCode = "DateLimit" NO-ERROR.
      ASSIGN
      TMSParam.DateVal = exdate2.

   message "File is ready - press ENTER !".
   PAUSE no-message.

   LEAVE.
END.
HIDE FRAME start no-pause.

