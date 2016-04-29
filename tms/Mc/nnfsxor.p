/* -----------------------------------------------------------------
  MODULE .......: NNFSXOR.p
  TASK .........: creates 2 files FOR XOR of invoices
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 24.03.1998
  CHANGED ......: 20.05.1998 pt NEW FORMAT on some fields, SKIP 0-rows
                  20.08.1998 pt 2 files on one RUN (internal/external)
                  04.11.1998 pt NEW layout on external File
                  11.11.1998 pt further changes on layout
                  04.01.1999 kl layout ...
                  14.01.1999 kl previous continued
                  15.02.1999 kl Qty & Minutes summaries
                  16.03.1999 kl Qty & Minutes summaries FOR internal File
                  10.06.1999 kl country.i + danish exeptions
                  02.09.1999 kl AccNum FOR advance payments 
                  22.09.1999 kl use Currency rate FOR converting currencies
                                into national Currency
                  11.10.1999 kl use Invoice.InvCfg[3] TO mark invoice AS
                                Sent TO XOR File
                  18.11.1999 kl print invoice rows in SEK (NOT Currency)
                  04.01.2000 kl advance payment AS positive
                  04.01.2000 kl advance payment again
                  10.02.2000 jp cparams
                  18.02.2000 jp handling of Invoice.OverPaym
                  04.03.2000 kl use OverPayAcc AS default VALUE
                  15.05.2000 ht Pocket added
                  27.07.2000 kl partner & prev.inv added

  Version ......: M15
  ------------------------------------------------------------------ */

{Syst/commali.i}
{Syst/country.i}
{Mf/invsta2.frm}

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exname-i  AS c  NO-UNDO.
DEF VAR exname-e  AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
def var exdeci    as lo format "Period/Comma" NO-UNDO.
def var ok        as lo format "Yes/No"      NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR csum      AS c  NO-UNDO.
DEF VAR totinc    AS DE NO-UNDO.
DEF VAR totcalls  AS i  NO-UNDO.
DEF VAR totminu   AS i  NO-UNDO.
DEF VAR xtot      AS DE NO-UNDO.
DEF VAR xtot2     AS DE NO-UNDO.
DEF VAR tab       AS c  NO-UNDO.
def var InvGroup   like  InvGroup.InvGroup   init "T1E".
DEF VAR amt       AS DE NO-UNDO EXTENT 5.
DEF VAR acno      AS i  NO-UNDO EXTENT 5.
DEF VAR pr-code   AS c  NO-UNDO.
DEF VAR ks-code   AS c  NO-UNDO.
DEF VAR Salesman   AS c  NO-UNDO.
DEF VAR Currency   AS c  NO-UNDO.
DEF VAR cct       AS c  NO-UNDO.
DEF VAR TransFile     AS i  NO-UNDO.
DEF VAR dos-skip  AS c  NO-UNDO.
DEF VAR cperiod   AS c  NO-UNDO.
DEF VAR cnlist    AS c  NO-UNDO.
def var state1    as i  no-undo format "9".
def var state2    as i  no-undo format "9".
DEF VAR irowsek   AS DE NO-UNDO.

DEF VAR RoundAcc   AS i  NO-UNDO.
DEF VAR OTIntAcc   AS i  NO-UNDO.
DEF VAR VatAcc     AS i  NO-UNDO.
DEF VAR OverPayAcc AS i  NO-UNDO.
DEF VAR ReceivAcc  AS i  NO-UNDO.
DEF VAR rsname     AS c  NO-UNDO.
DEF VAR previnv    AS DA NO-UNDO.

DEF NEW shared STREAM xor-int.
DEF NEW shared STREAM xor-ext.

DEF BUFFER xxlasku FOR Invoice.

DEF TEMP-TABLE w-acct
   FIELD w-org      AS c
   FIELD w-acct     AS i
   FIELD w-cct      AS c
   FIELD w-amt      AS DE
   FIELD w-pcs      AS i
   FIELD w-min      AS i

   INDEX worg AS primary
      w-org
      w-acct.

/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCOde = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.

/* initial values */
tab        = chr(9).
dos-skip   = chr(13) + chr(10).
exname-i   = exdir + "/" + "xorinv-i.txt".
exname-e   = exdir + "/" + "xorinv-e.txt".

{Func/tmsparam.i RoundAcc   return}. RoundAcc   = TMSParam.IntVal.
{Func/tmsparam.i OTIntAcc   return}. OTIntAcc   = TMSParam.IntVal.
{Func/tmsparam.i VatAcc     return}. VatAcc     = TMSParam.IntVal.
{Func/tmsparam.i OverPayAcc return}. OverPayAcc = TMSParam.IntVal.
{Func/tmsparam.i ReceivAcc  return}. ReceivAcc  = TMSParam.IntVal.

form
   skip(19)
WITH no-box COLOR value(cfc) width 80 OVERLAY FRAME bground.

form
   skip(1)
"  Information:  This program writes 2 summary files of all invoices in"
"                customer AccNum receivables, for XOR."         skip(1)
"                Files' names are" skip
"                - internal :"  exname-i format "x(35)" no-label skip
"                - external :"  exname-e format "x(35)" no-label skip(1)
"                Invoice's Printed out:" exdate1 format "99-99-99"
help "Earliest invoicing date"
"-" exdate2 format "99-99-99" help "Latest invoicing date"   skip
"                Inv.group ...........:" InvGroup
help "Invoicing group's code, empty for all"  SKIP
"                Decimal separator ...:" exdeci help "Period/Comma" 
"                Invoice states ......:" state1 "-" state2 skip(1)
WITH
   width 73 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc) 
   " " + ynimi + " XOR-SUMMARY OF ACCOUNTS " +
   string(pvm,"99-99-99") + " " centered NO-LABELS FRAME start.

ASSIGN
   exdate2 = date(month(TODAY),1,year(TODAY)) - 1
   exdate1 = date(month(exdate2),1,year(exdate2)).

cfc = "sel". RUN Syst/ufcolor.

view FRAME bground.
PAUSE 0.
view FRAME statu.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN Syst/ufkey.
   UPDATE
      exname-i exname-e
      exdate1  validate(exdate1 ne ?,"Give first Date !")
      exdate2  validate(input exdate2 >= input exdate1,"Invalid order !")
      InvGroup  validate(InvGroup = "" OR can-find(InvGroup where
                                 InvGroup.InvGroup = InvGroup),
                                 "Group does not exist !")
      exdeci
      state1  validate(state1 < 2,"Value too high ! (0 OR 1)")
      state2  validate(state2 < 2,"Value too high ! (0 OR 1)")
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

   IF YEAR (exdate1) NE YEAR (exdate2)    OR
      month(exdate1) NE month(exdate2)    THEN DO:
      BELL.
      message "Both dates MUST be within same month !".
      NEXT-PROMPT exdate1.
      NEXT.
   END.

task:
   repeat WITH FRAME start:

      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   /* Period */
   cperiod = string(year (exdate1),"9999") +
             string(month(exdate1),"99").


   /**********************************************
   *                                             *
   * HERE WE START TO PRODUCE THE EXTERNAL       *
   * File THAT CONTAINS 1 ROW / INVOICE/ACCT     *
   *                                             *
   **********************************************/


   message "Preparing the EXTERNAL File ...".
   OUTPUT STREAM xor-ext TO value(exname-e).

   /* get a consecutive number FOR xfer File */
   DO FOR FileExpLog:
      find first FileExpLog where TransType = "XOR-INV" no-lock no-error.
      IF AVAIL FileExpLog THEN TransFile = FileExpLog.TransNum + 1.
      ELSE                 TransFile = 1.

      CREATE FileExpLog.
      ASSIGN
      FileExpLog.TransType = "XOR-INV"
      FileExpLog.TransDate = pvm
      FileExpLog.TransNum   = TransFile.
   END.

   /**************************
   * Header TO external File *
   **************************/

   PUT STREAM xor-ext UNFORMATTED
   "%FILE ID "
   string(TransFile,"99999999")
   " EXTERNAL, CREATED "
   string(pvm,"99.99.9999")
   " AT "
   string(time,"hh:mm:ss").

   PUT STREAM xor-ext  UNFORMATTED
   dos-skip
   dos-skip

   "Account"     tab
   "CostCnt"     tab
   "Project"     tab
   "Customer"    tab
   "Salesman"    tab
   "InvoiceNo"   tab
   "InvDate"     tab
   "Amount"      tab
   "NoOfCalls"   tab
   "NoOfMinutes" tab
   "Partner"     tab
   "Prev. inv."  tab
   dos-skip.

   message "Processing ...".

   ASSIGN
      xtot  = 0  
      xtot2 = 0.

   ASSIGN
      acno[1] = RoundAcc       /* rounding      */
      acno[2] = OTIntAcc       /* Interest      */
      acno[3] = VatAcc         /* VATAmt           */
      acno[5] = ReceivAcc.     /* receivables   */

   FOR EACH  Invoice no-lock where
             Invoice.InvDate       >= exdate1 AND
             Invoice.InvDate       <= exdate2 AND
         int(Invoice.InvCfg[3]) >= state1  AND
         int(Invoice.InvCfg[3]) <= state2,

       FIRST Customer no-lock where
             Customer.CustNum = Invoice.CustNum AND
             (if InvGroup ne "" THEN Customer.InvGroup = InvGroup
             ELSE TRUE),
       FIRST Salesman no-lock where
             Salesman.Salesman = Customer.Salesman,
       FIRST Salesoffice no-lock where
             Salesoffice.SalesOffice = Salesman.SalesOffice

   BREAK BY Invoice.InvNum:

      /******************************************
      * External File: 1 ROW / account/ invoice *
      ******************************************/

      /* collect solid amts from invoice header into a local table */
      ASSIGN
         amt[1]  = 0 - Invoice.Rounding        /* rounding                   */
         amt[2]  = 0 - Invoice.InterestAmt       /* Interest                   */
         amt[3]  = 0 - Invoice.VATAmt         /* VATAmt                        */
         amt[4]  = 0 - Invoice.OverPaym          /* advance payment OverPaym       */
         acno[4] = Invoice.OpAccNum            /* overpayments */    /* OverPaym */
         cct     = string(Salesoffice.CostCentre,"999").

      /* Default VALUE */
      IF acno[4] = 0 THEN acno[4] = OverPayAcc.

      /* total amount, 'receivable': IF a Currency rate is used */
      IF Invoice.ExchRate NE 0 THEN
         amt[5]  = Invoice.InvAmt * Invoice.ExchRate.
      ELSE
         amt[5]  = Invoice.InvAmt.

      /* project code on medium AND large customers */
      if {&country} = 4 then assign pr-code = string(Customer.Pocket,"999999").
      ELSE IF {&country} NE 3 AND
         Customer.Size = "S" then assign pr-code = "00000000".
      ELSE
         pr-code = string(Customer.CustNum,"9999999") + "0".

      /* Salesman code WITH leading zeros */
      Salesman = Salesman.Salesman.
      IF {&country} = 3 THEN
         Salesman = fill("0",4 - length(Salesman)) + Salesman.
      ELSE
         Salesman = substr("000",1,3 - length(Salesman)) + Salesman.

      /* customer code consists of cust.no PLUS 2 extra digits:

            8. digit = Size:  0 = EXTRA LARGE 1=LARGE  2=MEDIUM  3=SMALL
            9. digit = ConnType.  0 = direct 5 = indirect  */

      Currency = string(Customer.CustNum,"9999999").

      /* Size digit */
      if Customer.Size = "XL" then Currency = Currency + "0".  ELSE
      if Customer.Size = "L"  then Currency = Currency + "1".  ELSE
      if Customer.Size = "M"  then Currency = Currency + "2".  ELSE
      if Customer.Size = "S"  then Currency = Currency + "3".

      /* connection digit */
      Currency = Currency + string(Customer.ConnType,"0/5").

      /* possible partner Name */
      FIND FIRST Reseller where 
                 Reseller.Reseller = Customer.Reseller
      no-lock no-error.
      IF AVAIL Reseller THEN rsname = Reseller.RsName.
      else rsname = "".

      DO FOR xxlasku:
         FIND FIRST xxlasku of Customer where
                    xxlasku.InvDate < Invoice.InvDate AND
                    xxlasku.CrInvNum = 0
         no-lock no-error.
         IF AVAIL xxlasku THEN previnv = xxlasku.InvDate.
         ELSE previnv = ?.
      END.

      /***********************************************
      * 1 group of rows / invoice WITH               *
      *  - income rows (*)     CreditInvNum
      *  - rounding            CreditInvNum                *
      *  - Interest            CreditInvNum                *
      *  - VATAmt                 CreditInvNum                *
      *  - accounts receivable DEBIT                 *
      ***********************************************/

      /* collect ALL incomes sorted BY AccNum no. */
      FOR EACH InvRow of Invoice no-lock where
               InvRow.Amt NE 0
      BREAK
         BY InvRow.SlsAccNum.

         irowsek = InvRow.Amt.

         /* change Currency back TO SEK */
         IF Invoice.ExchRate NE 0 THEN 
            irowsek = irowsek * Invoice.ExchRate.

         accumulate
            irowsek       (sub-total BY InvRow.SlsAccNum)
            InvRow.Qty (sub-total BY InvRow.SlsAccNum)
            InvRow.Minutes (sub-total BY InvRow.SlsAccNum).

         IF last-of(InvRow.SlsAccNum) THEN DO:
            /* print a ROW WITH amount per AccNum */

            ASSIGN
            /* Billed income ex VATAmt */
            totinc  = 0 - (accum sub-total BY InvRow.SlsAccNum irowsek)
            csum    = string(totinc, "-9999999999.99").

            if not exdeci then substring(csum,12,1) = ",".

            ASSIGN
               /* Amount of Calls */
               totcalls = (accum sub-total BY InvRow.SlsAccNum InvRow.Qty)
               /* Amount of minutes */
               totminu = (accum sub-total BY InvRow.SlsAccNum InvRow.Minutes).

            PUT STREAM xor-ext UNFORMATTED
            InvRow.SlsAccNum                   tab  /* AccNum number       */
            cct           format "999"        tab  /* cost center          */
            pr-code                           tab  /* project code         */
            Currency                           tab  /* cust # + Size + ConnType */   
            Salesman                           tab  /* Salesman code        */
            Invoice.InvNum format "99999999"   tab  /* Invoice #            */
            Invoice.InvDate format "99.99.9999" tab  /* Invoice Date         */
            csum                              tab  /* amount               */
            totcalls                          tab  /* No. of Calls         */
            totminu                           tab  /* No. of minutes       */
            rsname                            tab  /* partner Name         */
            .

            IF previnv NE ? THEN PUT STREAM xor-ext UNFORMATTED
               previnv format "99.99.9999".        /* previously Billed   */

            PUT STREAM xor-ext UNFORMATTED dos-skip.

            xtot = xtot + totinc.

         END.
      END. /* FOR EACH InvRow */

      /* THEN one ROW per EACH amount on invoice header */

      DO i = 1 TO 5. /* OverPaym */

         IF amt[i] = 0 THEN NEXT.

         csum = string(amt[i],"-9999999999.99").
         if not exdeci then substring(csum,12,1) = ",".

         PUT STREAM xor-ext UNFORMATTED

         acno[i]                           tab  /* AccNum no.          */
         cct           format "999"        tab  /* cost center          */
         pr-code                           tab  /* project code         */
         Currency                           tab  /* cust # + Size + ConnType */
         Salesman                           tab  /* Salesman code        */
         Invoice.InvNum format "99999999"   tab  /* Invoice #            */
         Invoice.InvDate format "99.99.9999" tab  /* Invoice Date         */
         csum                              tab  /* amount               */
         dos-skip.

         xtot = xtot + amt[i].

      END.

      /* Grand total */
      IF Invoice.ExchRate NE 0 THEN
         xtot2 = xtot2 + Invoice.InvAmt * Invoice.ExchRate.
      ELSE
         xtot2 = xtot2 + Invoice.InvAmt.

      /* wait (loop) until updating is allowed */
      State:
      repeat:
         DO FOR xxlasku:
            FIND FIRST xxlasku where
                 recid(xxlasku) = recid(Invoice)
            exclusive-lock no-error no-wait.
            IF error-status:error THEN UNDO State, NEXT State.
            IF AVAIL xxlasku THEN
               ASSIGN 
                  Invoice.InvCfg[3] = TRUE
               no-error.
            IF error-status:error THEN UNDO State, NEXT State.
            ELSE LEAVE State.
         END.
      END.

   END.

   csum = string(xtot,"-9999999999.99").
   if not exdeci then substr(csum,12,1) = ",".
   PUT STREAM xor-ext UNFORMATTED     /* this should be 0 (debit + CreditInvNum) */
   "%CHECKSUM 1 " csum dos-skip.

   csum = string(xtot2,"-9999999999.99").
   if not exdeci then substr(csum,12,1) = ",".
   PUT STREAM xor-ext UNFORMATTED     /* this should be a grand total */
   "%CHECKSUM 2 " csum dos-skip.

   OUTPUT STREAM xor-ext CLOSE.
   HIDE MESSAGE no-pause.

  /********************************************
  *                                           *
  *  HERE WE START TO WRITE THE SECOND File   *
  *  THAT IS FOR INTERNAL USE.                *
  *  1 ROW / ORGANIZATION CODE                *
  *  WHICH MEANS THAT THERE CAN BE VARIOUS    *
  *  CUSTOMERS' INVOICE DATA ACCUMULATED      *
  *  ON ONE ROW                               *
  *                                           *
  ********************************************/

   message "Preparing the INTERNAL File ...".
   xtot  = 0.
   xtot2 = 0.

   OUTPUT STREAM xor-int TO value(exname-i).

   /**************************
   * Header TO INTERNAL File *
   **************************/

   PUT STREAM xor-int UNFORMATTED
   "%FILE ID "
   string(TransFile,"99999999")
   " INTERNAL, CREATED "
   string(pvm,"99.99.9999")
   " AT "
   string(time,"hh:mm:ss")
   dos-skip
   dos-skip
   "Account"       tab
   "CostCenter"    tab
   "Customer"      tab
   "Salesman"      tab
   "Project"       tab
   "Amount"        tab
   "Calls"         tab
   "Minutes"       tab
   "Period"        tab
   "Organization"  tab
   "Customers" 
   dos-skip.

   message "Processing ...".
   FOR
      EACH  Invoice no-lock where
            Invoice.InvDate >= exdate1        AND
            Invoice.InvDate <= exdate2,
      FIRST Customer no-lock where
            Customer.CustNum = Invoice.CustNum AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup
            ELSE TRUE),
      FIRST Salesman no-lock where
            Salesman.Salesman = Customer.Salesman,
      FIRST Salesoffice no-lock where
            Salesoffice.SalesOffice = Salesman.SalesOffice

   BREAK
   BY Customer.OrgId
   BY Invoice.CustNum:


      IF first-of(Customer.OrgId) THEN DO:
         cnlist = "".

         /* DEFINE some codes from FIRST customer record
            within a customer group WITH equal org. code */
         cct     = string(Salesoffice.CostCentre,"999").

         /* project code on medium AND large customers */
         if Customer.Size = "S" then assign pr-code = "00000000".
         ELSE
         pr-code = string(Customer.CustNum,"9999999") + "0".


         /* Salesman code WITH leading zeros */
         Salesman = Salesman.Salesman.
         Salesman = substr("000",1,3 - length(Salesman)) + Salesman.

         /* customer code consists of cust.no PLUS 2 extra digits:

            8. digit = Size:  0 = EXTRA LARGE 1 = LARGE  2 = MEDIUM  3 = SMALL
            9. digit = ConnType.  0 = direct 1 = indirect  */

         Currency = string(Customer.CustNum,"9999999").

      END.

      /* add customer's number in cust.no list IF NOT already */
      IF lookup(string(Invoice.CustNum),cnlist) = 0 THEN
      cnlist = cnlist + string(Invoice.CustNum) + ",".

      /**********************************
      * Amounts from invoice header rec *
      * are stored on WORKFILE, sorted  *
      * BY AccNum number               *
      **********************************/

      /* collect solid amts from invoice header into a local table */

      ASSIGN
         amt[1]  = 0 - Invoice.Rounding
         amt[2]  = 0 - Invoice.InterestAmt
         amt[3]  = 0 - Invoice.VATAmt.

      IF Invoice.ExchRate NE 0 THEN   
         amt[4] = Invoice.InvAmt * Invoice.ExchRate.
      ELSE
         amt[4] = Invoice.InvAmt.

      DO i = 1 TO 4.
         IF amt[i] = 0 THEN NEXT.

         FIND FIRST w-acct where
                    w-org  = Customer.OrgId   AND
                    w-acct = acno[i]
         no-error.
         IF NOT AVAIL w-acct THEN DO:
            CREATE w-acct.
            ASSIGN
            w-org  = Customer.OrgId
            w-acct = acno[i].

            if i = 3 or i = 4 then w-cct = cct. else w-cct = "000".
         END.
         w-amt  = w-amt + amt[i].
      END.

      /* accumulate revenues from invoice rows */
      FOR EACH InvRow no-lock where
               InvRow.InvNum   =  Invoice.InvNum  AND
               InvRow.Amt NE 0

      /* invoice rows WITH 020-calls are of VALUE 0.00 so we SKIP them */

      BY InvRow.SlsAccNum:

         FIND FIRST w-acct where
              w-org  = OrgId AND
              w-acct = SlsAccNum
         no-error.

         IF NOT AVAIL w-acct THEN DO:
            CREATE w-acct.
            ASSIGN
            w-org  = OrgId
            w-acct = SlsAccNum
            w-cct  = cct.
         END.

         irowsek = InvRow.Amt.

         /* change Currency back TO SEK */
         IF Invoice.ExchRate NE 0 THEN 
            irowsek = irowsek * Invoice.ExchRate.

         ASSIGN
            w-amt = w-amt - irowsek
            w-pcs = w-pcs + InvRow.Qty
            w-min = w-min + InvRow.Minutes.
      END.

      IF last-of(Customer.OrgId) THEN DO:

         /*******************************
         *  one line per AccNum number *
         *******************************/

         FOR EACH w-acct BY w-acct:

            csum = string(w-amt,"-9999999999.99").
            if not exdeci then substring(csum,12,1) = ",".

            PUT STREAM xor-int UNFORMATTED

            w-acct                            tab /* AccNum         */
            w-cct                             tab /* cost center     */
            Currency                           tab /* Customer code   */
            Salesman                           tab /* Salesman        */
            pr-code                           tab /* Project         */
            csum                              tab /* Amount          */
            w-pcs                             tab /* # Calls         */
            w-min                             tab /* # minutes       */
            cperiod                           tab /* YYYYmm          */
            w-org                             tab /* org. #          */
            cnlist                                /* Cust nr list    */
            dos-skip.

            xtot = xtot + w-amt.

            DELETE w-acct.
         END.
      END.
      IF Invoice.ExchRate NE 0 THEN 
         xtot2 = xtot2 + Invoice.InvAmt * Invoice.ExchRate.
      ELSE
         xtot2 = xtot2 + Invoice.InvAmt.
   END.

   /* Finally total amts,  */
   csum = string(xtot,"-9999999999.99").
   if not exdeci then substr(csum,12,1) = ",".

   PUT STREAM xor-int UNFORMATTED     /* this should be 0 (debit + CreditInvNum) */
   "%CHECKSUM 1 " csum dos-skip.

   csum = string(xtot2,"-9999999999.99").
   if not exdeci then substr(csum,12,1) = ",".

   PUT STREAM xor-int UNFORMATTED     /* this should be a grand total */
   "%CHECKSUM 2 " csum dos-skip.

   OUTPUT STREAM xor-int CLOSE.
   PAUSE 0.

   message "Both files are now ready - press ENTER !".
   PAUSE no-message.

   LEAVE.
END.
HIDE FRAME start no-pause.
HIDE MESSAGE.

