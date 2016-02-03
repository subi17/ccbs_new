/* ---------------------------------------------------------------------
  MODULE .......: NNRANK.P
  TASK .........: creates an CUSTOMER RANKING LIST AS an excel File
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 08.12.1998
  CHANGED ......: 01.01.1999 pt InvGroup, Reseller, NOT InvRow.FFRow
                  11.09.2002 jp Wrong display field
                  04.10.2002 jr Print by product and product group
                  08.10.2002 jr can print direct to printer
                  17.10.2002 aam use RowType,
                                 use InvRow.BillCode directly in foreach etc.,
                                 set tuni1 
                  12.09.2003 aam brand               
  Version ......: M15
----------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/utumaa.i "new"}

assign tuni1 = "nnrank"
       tuni2 = "".

DEF VAR exdir     AS c                     NO-UNDO.
DEF VAR exname    AS c                     NO-UNDO.
DEF VAR Salesman  LIKE Salesman.Salesman   NO-UNDO.
DEF VAR InvGroup  LIKE InvGroup.InvGroup   NO-UNDO.
DEF VAR CustGroup LIKE CustGroup.CustGroup NO-UNDO.
DEF VAR Reseller  LIKE Reseller.Reseller   NO-UNDO.
DEF VAR SmName    LIKE Salesman.SmName     NO-UNDO.
DEF VAR RsName    LIKE Reseller.RsName     NO-UNDO.
DEF VAR IGName    LIKE InvGroup.IGName     NO-UNDO.
DEF VAR CGName    LIKE InvGroup.IGName     NO-UNDO.
DEF VAR lcBillCode LIKE BillItem.BillCode   NO-UNDO.
DEF VAR BiName    LIKE BillItem.BIName     NO-UNDO.
DEF VAR BIGroup   LIKE BItemGroup.BIGroup  NO-UNDO.
DEF VAR BigName   LIKE BItemGroup.BIGName  NO-UNDO.
def var period1    as i  format "999999" NO-UNDO.
def var period2    as i  format "999999" NO-UNDO.
DEF VAR tab        AS c                  NO-UNDO.
DEF VAR camt       AS i                  NO-UNDO.
DEF VAR date1      AS DA                 NO-UNDO.
DEF VAR date2      AS DA                 NO-UNDO.
DEF VAR totbill    AS DE                 NO-UNDO.
DEF VAR totmin     AS i                  NO-UNDO.
DEF VAR totcalls   AS i                  NO-UNDO.
DEF VAR t-skey     AS i  EXTENT 1000     NO-UNDO.
DEF VAR t-bill     AS i  EXTENT 1000     NO-UNDO. /* note: INTEGER ! */
DEF VAR t-cust     AS i  EXTENT 1000     NO-UNDO.
DEF VAR t-min      AS i  EXTENT 1000     NO-UNDO.
DEF VAR t-calls    AS i  EXTENT 1000     NO-UNDO.
DEF VAR lt-bill    AS i                  NO-UNDO. /* note: INTEGER ! */
DEF VAR lt-cust  AS i FORMAT ">>>>>>>9"  NO-UNDO.
DEF VAR lt-min     AS i                  NO-UNDO.
DEF VAR lt-calls   AS i                  NO-UNDO.
DEF VAR lt-name    AS CHAR FORMAT "X(30)"  NO-UNDO.
DEF VAR i          AS i FORMAT ">>>9"      NO-UNDO.
DEF VAR j          AS i                  NO-UNDO.
DEF VAR yy         AS i                  NO-UNDO.
DEF VAR mm         AS i                  NO-UNDO.
DEF VAR t-max      AS i                  NO-UNDO.
DEF VAR p-max      AS i                  NO-UNDO.
DEF VAR periods    AS i  EXTENT 48       NO-UNDO.
DEF VAR idate1     AS DA                 NO-UNDO.
DEF VAR idate2     AS DA                 NO-UNDO.
DEF VAR sord       AS i                  NO-UNDO.
def var soname     as c format "x(24)"   NO-UNDO.
DEF VAR sonames    AS c                  NO-UNDO.
DEF VAR skey       AS i                  NO-UNDO.
DEF VAR Qty        AS i                  NO-UNDO.
DEF VAR llprint    AS LOG                NO-UNDO.

def var viiva1 as char format "x(114)".
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR sl AS INT.
DEF VAR rl AS INT.
DEF VAR lev AS INT init 114.

DEF NEW shared STREAM excel.

ASSIGN
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev).

/* get default directory name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.

/* initial values */
ASSIGN
tab      = chr(9)
exname   = exdir + "/" + "cusrank.txt"
camt     = 999
sord     = 1
sonames  = "VALUE OF Billed Calls,NUMBER OF ISValue,NUMBER OF MINUTES"
soname   = entry(1,sonames)
SMName  = "ALL SALESMEN"
RSName  = "ALL AGENTS"
IGName  = "ALL INVOICING GROUPS"
CGName  = "NONE"
BiName  = "ALL PRODUCTS"
BigName = "ALL PRODUCT GROUPS".

form
skip
" NOTE:  This program writes out an TAB separated ACII File " skip
"        with max" camt format "zz9"
"BEST customers matching with following parameters," skip
"        sorted by - according Your choice - either by:"
"         - 1: total VALUE  of Billed Calls, (contract fees are omitted)  OR"
"         - 2: total number of Calls                                      OR"
"         - 3: total number of MINUTES."
"        during that Period determined below."  skip(1)

"        Ext. CustGroup ...:" CustGroup
help "Code of a possible External Customer Group; EMPTY: none External Group"
                              CGName at 40 format "x(30)" SKIP

"        Invoicing Group . :" InvGroup
help "Code of invoicing Group; EMPTY: ALL groups"
                              IGName at 40 format "x(30)" SKIP

"        Salesman .........:" Salesman
help "Code of Salesman; EMPTY: ALL salesmen"
                              SmName at 40 format "x(30)" SKIP

"        Agent/reseller ...:" Reseller
help "Code of Agent/reseller; EMPTY: ALL agents"
                              RsName at 40 format "x(30)" SKIP
"        Sort Order (1/2/3):" sord format "9"
help "Choose sort order  1, 2 or 3 (see above)"
"BY" at 40 soname at 44 skip
"        Product ..........:" lcBillCode
help "Product code; EMPTY: ALL Products"
                              BiName at 47 format "x(25)" skip
"        Product group ....:" BiGroup
help "Product group; EMPTY: ALL Groups"
                              BigName at 47 format "x(25)" skip
"        Invoices during ..:" period1
         help "Earliest billing Period (month), format 'YYYYMM'" "-"
         period2 NO-LABEL
         help "Latest billing Period (month), format 'YYYYMM'"   SKIP
"        File's name is ...:" exname format "x(30)"
help "Name for output file" NO-LABEL    Qty TO 78


WITH
   overlay width 80 title " Customer ranking list " NO-LABELS FRAME rajat.

FORM HEADER
   viiva1       AT 2 SKIP
   ynimi        AT 2 
   "RANKING LIST OF CUSTOMERS"  AT 45 
   "page"       AT 106 sl FORMAT "ZZZZ9" SKIP
   "Billed values are ex vat amt and calls only, contract fees are omitted" AT 2
    STRING(pvm,"99-99-99") AT 108 SKIP 
   "According to invoices written during " AT 2
   string(period1) " - " string(period2) SKIP
   "External C-Group..:" AT 2 CGName  SKIP
   "Invoicing Group...:" AT 2 IGName  SKIP
   "Salesman..........:" AT 2 SmName  SKIP
   "Agent.............:" AT 2 RsName  SKIP
   "Product...........:" AT 2 BiName  SKIP
   "Product Group.....:" AT 2 BigName SKIP
   "SORTED BY " AT 2 soname SKIP
   viiva2       AT 2 skip
   "Rank"       AT 2
   "Customer"   AT 7
   "Name"       AT 16
   "Minutes"    TO 70
   "Calls"      TO 90
   "Amount"     TO 110 SKIP
   viiva3       AT 2 SKIP
WITH width 116 NO-LABEL no-box FRAME sivuots.

FORM
   i                   AT 2
   lt-cust             AT 7
   lt-name             AT 16   
   lt-bill             TO 70
   lt-min              TO 90
   lt-calls            TO 110  SKIP
WITH WIDTH 116 NO-LABELS NO-BOX FRAME linet.

ASSIGN
    period1 = YEAR(TODAY) * 100 + MONTH(TODAY)
    period2 = period1
    rl      = 15
    sl      = 1.

PAUSE 0.
DISP CGName IGName SmName RsName soname biname bigname WITH FRAME rajat.

rajat:
repeat WITH FRAME rajat.
   ehto = 9. RUN ufkey.

   UPDATE
   camt CustGroup InvGroup Salesman Reseller 
   sord lcBillcode BiGroup   period1 period2 exname
   WITH FRAME rajat EDITING:
      READKEY.

      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         HIDE MESSAGE.

         if frame-field = "CustGroup" THEN DO:
            if input CustGroup = "" then CGName = "NONE".
            ELSE DO:
               FIND CustGroup where
               CustGroup.Brand     = gcBrand AND
               CustGroup.CustGroup = INPUT CustGroup no-lock no-error.
               IF NOT AVAIL CustGroup THEN DO:
                  BELL.
                  message "Unknown External CustomerGroup !".
                  NEXT.
               END.
               CGName = CustGroup.CGName.
            END.
            DISP CGName.
         END.

         else if frame-field = "InvGroup" THEN DO:
            if input InvGroup = "" then IGName = "ALL INVOICING GROUPS".
            ELSE DO:
               FIND InvGroup where
               InvGroup.Brand    = gcBrand AND
               InvGroup.InvGroup = INPUT InvGroup no-lock no-error.
               IF NOT AVAIL InvGroup THEN DO:
                  BELL.
                  message "Unknown Invoicing Group !".
                  NEXT.
               END.
               IGName = InvGroup.IGName.
            END.
            DISP IGName.
         END.

         else if frame-field = "Salesman" THEN DO:
            if input Salesman = "" then SmName = "ALL SALESMEN".
            ELSE DO:
               FIND Salesman where
               SalesMan.Brand    = gcBrand AND
               Salesman.Salesman = INPUT Salesman no-lock no-error.
               IF NOT AVAIL Salesman THEN DO:
                  BELL.
                  message "Unknown Salesman !".
                  NEXT.
               END.
               SmName = Salesman.SmName.
            END.
            DISP SmName.
         END.

         else if frame-field = "Reseller" THEN DO:
            if input Reseller = "" then  RsName = "ALL AGENTS".
            ELSE DO:
               FIND Reseller where
               Reseller.Brand    = gcBrand AND
               Reseller.Reseller = INPUT Reseller no-lock no-error.
               IF NOT AVAIL Reseller THEN DO:
                  BELL.
                  message "Unknown agent !".
                  NEXT.
               END.
               RsName = Reseller.RsName.

               if input Salesman ne "" AND Reseller.Salesman NE INPUT Salesman
               THEN DO:
                  DISP RsName.
                  BELL.  MESSAGE
                  "Warning: this agent belongs to salesman" Reseller.Salesman "!".
                  message "Press ENTER !".
                  NEXT-PROMPT Salesman.
                  NEXT.
               END.
            END.
            DISP RsName.
         END.

         else if frame-field = "lcBillCode" THEN 
         DO:
            if input lcBillCode = "" then Biname = "ALL PRODUCTS".
            ELSE 
            DO:
               FIND BillItem WHERE
                    BillItem.Brand     = gcBrand AND
                    BillItem.BillCode  = INPUT lcBillCode No-LOCK NO-ERROR.
               IF NOT AVAIL BillItem THEN 
               DO:
                  BELL.
                  MESSAGE "Unknown Product !".
                  NEXT.
               END.
               Biname = BillItem.BIName.
            END.
            DISP Biname.
         END.

         else if frame-field = "BiGroup" THEN
         DO:
            if input BiGroup = "" then Bigname = "ALL PRODUCT GROUPS".
            ELSE
            DO:
               FIND BitemGroup WHERE
                    BItemGroup.Brand    = gcBrand AND
                    BItemGroup.BIGroup  = INPUT BiGroup No-LOCK NO-ERROR.
               IF NOT AVAIL BitemGroup THEN
               DO:
                  BELL.
                  MESSAGE "Unknown Product Group!".
                  NEXT.
               END.
               IF AVAIL BitemGroup AND INPUT lcBillCode NE "" THEN
               DO:
                  FIND BillItem NO-LOCK WHERE 
                       BillItem.Brand    = gcBrand AND
                       BillItem.BillCode = lcBillCode.
                  IF BItemGroup.BIGroup NE BillItem.BIGroup THEN
                  DO:
                    MESSAGE "Product Group and Product don't match !".
                    NEXT.
                  END.  
               END.
               Bigname = BItemGroup.BIGName.
            END.
            DISP Biname.
         END.

         else if frame-field  = "period1" THEN DO:
            RUN uperch(INPUT INPUT period1,output i).
            IF i > 0 THEN NEXT.
         END.

         else if frame-field  = "period2" THEN DO:
            RUN uperch(INPUT INPUT period2,output i).
            IF i > 0 THEN NEXT.
            IF INPUT period2 < INPUT period1 THEN DO:
               BELL.
               message "Invalid order !".
               NEXT.
            END.
         END.

         else if frame-field = "camt" THEN DO:
            IF INPUT camt = 0 THEN DO:
               BELL.
               message "Answer 1 ... 999 !".
               NEXT.
            END.
         END.

         else if frame-field = "sord" THEN DO:
            IF INPUT sord = 0 OR INPUT sord > 3 THEN DO:
               BELL. MESSAGE
               "Invalid choice - answer 1, 2 or 3 !".
               NEXT.
            END.
            soname = entry(INPUT sord,sonames).
            DISP soname.
         END.

      END.   /* IF LOOKUP */
      APPLY LASTKEY.
   END. /* EDITING */

   /* make list (a column INDEX FOR OUTPUT) of individual periods */
   periods = 0.
   p-max = 0.
   DO i = period1 TO period2:
      yy = truncate(i / 100,0).
      mm = i - yy * 100.

      IF mm = 13 THEN ASSIGN mm = 1 yy = yy + 1 i = yy * 100 + mm.
      p-max = p-max + 1.
      IF p-max > 48 THEN DO:
         BELL.
         message "This program can analyze max 48 months !".
         NEXT-PROMPT period2.
         NEXT rajat.
      END.
      periods[p-max] = i.
   END.

toimi:
   repeat WITH FRAME toimi:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 7 ufk[5] = 847 ufk[6] = 638 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 5 THEN LEAVE toimi.
      IF toimi = 6 THEN 
      DO:
         ASSIGN tila = TRUE.
         {Syst/tmsreport.i "return"}
         llprint = TRUE.
         VIEW STREAM tul FRAME sivuots.
         LEAVE toimi.
      END.   
      IF toimi = 8 THEN LEAVE rajat.
   END.
   ASSIGN
   date1 = Date (integer(substr(string(period1),5,2)),
                 1,
                 integer(substr(string(period1),1,4)))
   date2 = Date (integer(substr(string(period2),5,2)),
                 1,
                 integer(substr(string(period2),1,4))) + 35
   date2 = Date (month(date2),1,year(date2)) - 1
   t-max = 1.

   IF NOT llprint THEN
   DO:
       OUTPUT STREAM excel TO value(exname).

       put stream excel unformatted ynimi tab "RANKING LIST OF CUSTOMERS; Billed VALUES ARE EX VATAmt AND Calls ONLY, CONTRACT FEES ARE OMITTED,".
          RUN uexskip(1).

       PUT STREAM excel UNFORMATTED
          "According to invoices written during " +
          string(period1) " - " string(period2).
          RUN uexskip(2).

       put stream excel unformatted "This File contains " camt
                                    " BEST customers of:".
          RUN uexskip(1).

       PUT STREAM excel UNFORMATTED
          "External C-Group" tab CGName. RUN uexskip(1).
       PUT STREAM excel UNFORMATTED
          "invoicing Group" tab IGName.  RUN uexskip(1).
       PUT STREAM excel UNFORMATTED
          "salesman"        tab SmName.  RUN uexskip(1).
       PUT STREAM excel UNFORMATTED
          "agent"           tab RsName.  RUN uexskip(1).
       PUT STREAM excel UNFORMATTED
           "Product"        tab BiName.  RUN uexskip(1).
       PUT STREAM excel UNFORMATTED
           "Product Group"  tab BigName. RUN uexskip(1).
       put stream excel unformatted "SORTED BY " soname.
          RUN uexskip(2).


       /* column headings on 2 rows */
       PUT STREAM excel UNFORMATTED
       "Rank"           tab
       "Cust"           tab
       "Customer's"     tab
       "<---------"     tab
       "TOTAL"          tab
       "--------->"     tab.

       DO i = p-max TO 1 BY -1.
          IF periods[i] > 0 THEN PUT STREAM excel UNFORMATTED
          "<---------"       tab
          string(periods[i]) tab
          "-------->"        tab.
       END.
       RUN uexskip(1).

       PUT STREAM excel UNFORMATTED
       "no."            tab
       "Number"         tab
       "Name"           tab
       "Amount"         tab
       "# of Min"       tab
       "# of Calls"     tab.

       DO i = p-max TO 1 BY -1.
          IF periods[i] > 0 THEN PUT STREAM excel UNFORMATTED
          "Amount" tab "# of Min" tab "# of Calls" tab.
       END.
       RUN uexskip(1).
   END.

   message "Calculating total Billed amounts by customer, wait ...".

 pick:
   FOR
   EACH  Customer USE-INDEX CustNum no-lock where
         Customer.Brand = gcBrand AND

         (if CustGroup ne "" THEN can-find(CGMember where
                          CGMember.CustNum  = Customer.CustNum           AND
                          CGMember.CustGroup = CustGroup) ELSE TRUE)     AND

         (if InvGroup ne "" THEN Customer.InvGroup = InvGroup ELSE TRUE) AND
         (if Salesman ne "" THEN Customer.Salesman = Salesman ELSE TRUE) AND
         (if Reseller ne "" THEN Customer.Reseller = Reseller ELSE TRUE),

   EACH  Invoice no-lock where
         Invoice.Brand = gcBrand AND
         Invoice.Custnum = Customer.CustNum AND
         Invoice.InvDate >= date1   AND
         Invoice.InvDate <= date2

   BREAK
   BY Customer.CustNum:

      IF first-of(Customer.CustNum) THEN ASSIGN
         totbill  = 0
         totmin   = 0
         totcalls = 0.

      FOR EACH InvRow of Invoice no-lock where
               /* only Calls, no contract fees */
               InvRow.RowType >= 1 AND
               InvRow.RowType <= 2 AND
               (IF lcBillCode NE "" 
                THEN InvRow.BillCode = lcBillCode 
                ELSE TRUE):

          IF INPUT BiGroup NE "" THEN 
          DO:
             FIND BillItem NO-LOCK WHERE
                  BillItem.Brand = gcBrand AND
                  BillItem.BillCode = InvRow.BillCode NO-ERROR.
             IF AVAILABLE BillItem AND 
                BillItem.BIGroup NE INPUT BiGroup THEN NEXT.
          END.        

          ASSIGN
          totbill  = totbill  + InvRow.Amt
          totmin   = totmin   + InvRow.Minutes
          totcalls = totcalls + InvRow.Qty.
      END.

      IF last-of(Customer.CustNum) THEN DO:

         Qty = Qty + 1.
         DISP Qty WITH FRAME rajat.


         /* CHOOSE sort Key */
         IF sord = 1  THEN skey = integer(totbill). ELSE
         IF sord = 2  THEN skey = totcalls.         ELSE
         IF sord = 3  THEN skey = totmin.

         /* omit this customer IF t-bill is SMALLER than the LAST element in
            a FULL array */

         IF t-max < camt OR skey > t-skey[maximum(1,t-max)] THEN
         /* insert this customer into ranking table t-bill */
         DO i = 1 TO camt /* (MAX 999 */ :

            /* is this customer "bigger" than customer [i] ? */
            IF skey > t-skey[i] THEN DO:
               /* how many customers are already in the table  ? */
               t-max = minimum(t-max + 1, camt).
               IF t-cust[i] NE 0 THEN
               /* make space FOR this customer */
               DO j = t-max TO i + 1 BY -1:
                  ASSIGN
                  t-skey [j] = t-skey [j - 1]
                  t-bill [j] = t-bill [j - 1]
                  t-min  [j] = t-min  [j - 1]
                  t-calls[j] = t-calls[j - 1]
                  t-cust [j] = t-cust [j - 1].
               END.
               ASSIGN
               t-skey [i] = skey
               t-bill [i] = totbill
               t-min  [i] = totmin
               t-calls[i] = totcalls
               t-cust [i] = Customer.CustNum
               i          = camt.
            END.
         END.
      END. /* insert */
   END.

   /******************************************
   * Now the t-bill array includes "best"    *
   * customers sorted BY total billing,      *
   * biggest FIRST.  Consequently the array  *
   * t-cust contains customers' numbers.     *
   ******************************************/
   PAUSE 0.
   IF NOT llprint THEN MESSAGE "Printing the File ...".
   ELSE MESSAGE "Printing ...".

CUSTOMER:
   DO i = 1 TO camt:

      IF i > camt OR t-cust[i] = 0 THEN LEAVE CUSTOMER.

      FIND Customer where Customer.CustNum = t-cust[i] no-lock no-error.

      IF NOT llprint THEN
      PUT STREAM excel UNFORMATTED
      i                  tab            /* sequence                     */
      t-cust[i]          tab            /* customer no.                 */
      Customer.CustName  tab            /* customer's name              */
      string(t-bill[i])  tab            /* customer's total billing     */
      string(t-min [i])  tab            /* customer's total minutes     */
      string(t-calls[i]) tab            /* customer's total no. of calls*/ .

      ELSE 
      DO:    
         IF rl >= skayt1 - 3 THEN 
         DO:
             DO i = rl TO spit1:
                PUT STREAM tul SKIP(1).
             END.
             ASSIGN
                sl = sl + 1
                rl = 15.
             VIEW STREAM tul FRAME sivuots.
         END.
         ASSIGN 
            lt-cust  = t-cust[i] 
            lt-bill  = t-bill[i] 
            lt-min   = t-min [i] 
            lt-calls = t-calls[i]
            lt-name  = Customer.CustName.

         DISPLAY STREAM tul 
         i
         lt-cust 
         lt-name
         lt-bill  
         lt-min 
         lt-calls
         WITH FRAME linet.
         DOWN STREAM tul WITH FRAME linet.  
         rl = rl + 1.
      END.

      /* NEXT we show EACH separate invoice (amount, Minutes, Calls)  */

      DO j = p-max TO 1 BY -1:

         /* isolate MONTH AND YEAR from Period code */
         yy = truncate(periods[j] / 100,0).
         mm = periods[j] - (yy * 100).

         /* FIRST DAY in that Period */
         idate1 = date(mm,1,yy).
         idate2 = idate1 + 35.
         idate2 = date(month(idate2),1,year(idate2)) - 1.

         totbill = 0.
         totmin  = 0.
         totcalls = 0.

         /* search customer's ALL invoices from that Period */
         FOR EACH Invoice no-lock where
                  Invoice.Brand    = gcBrand AND
                  Invoice.CustNum  = Customer.CustNum AND
                  Invoice.InvDate  >= idate1         AND
                  Invoice.InvDate  <= idate2,
             EACH InvRow of Invoice NO-LOCK where
               InvRow.RowType >= 1 AND
               InvRow.RowType <= 2 AND
               (IF lcBillCode NE "" 
                THEN InvRow.BillCode = lcBillCode 
                ELSE TRUE):

             IF INPUT BiGroup NE "" THEN
             DO:
                FIND BillItem NO-LOCK WHERE
                     BillItem.Brand = gcBrand AND
                     BillItem.BillCode = InvRow.BillCode NO-ERROR.
                IF AVAILABLE BillItem AND
                   BillItem.BIGroup NE INPUT BiGroup THEN NEXT.
             END.

             ASSIGN
             totbill  = totbill  + InvRow.Amt
             totmin   = totmin   + InvRow.Minutes
             totcalls = totcalls + InvRow.Qty.
         END.

         totbill = round(totbill,0).
         IF NOT llprint THEN
         DO:
             IF totbill NE 0 THEN
                PUT STREAM excel UNFORMATTED string(totbill).
                PUT STREAM excel UNFORMATTED tab.
             IF totmin  NE 0 THEN
                PUT STREAM excel UNFORMATTED string(totmin ).
                PUT STREAM excel UNFORMATTED tab.
             IF totcalls NE 0 THEN
                PUT STREAM excel UNFORMATTED string(totcalls).
                PUT STREAM excel UNFORMATTED tab.
         END.
      END.
      IF NOT llprint THEN RUN uexskip(1).
   END.
   IF NOT llprint THEN
   OUTPUT STREAM excel CLOSE.
   IF llprint THEN 
   DO:
      /* VielA viimeinen sivu kohdalleen */
      DO i = rl TO spit1:
         PUT STREAM tul SKIP(1).
      END.

      ASSIGN tila = FALSE.
      {Syst/tmsreport.i}
   END.
   PAUSE 0.
   IF NOT llprint THEN 
      message "File " exname "is ready - press ENTER !" VIEW-AS ALERT-BOX.
   ELSE MESSAGE "Printing is ready - press ENTER !" VIEW-AS ALERT-BOX.
   LEAVE rajat.
END.
HIDE FRAME rajat no-pause.
HIDE MESSAGE.

