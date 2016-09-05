/* -----------------------------------------------------
  MODULE .......: NNCUIN.P
  FUNCTION .....: Customer list WITH Billed amounts
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 22.09.99
  MODIFIED .....: 04.03.03 tk tokens
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Func/excel.i}
{Func/function.i}
{Syst/commali.i}
{Func/date.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}

DEF TEMP-TABLE imth NO-UNDO
   FIELD imth AS i
   FIELD acct AS i
   FIELD Amt  AS DE

   INDEX imth AS primary
      imth.

DEF BUFFER xMonth FOR imth.

def var ok      as lo no-undo format "Yes/No".
DEF VAR i       AS i  NO-UNDO.
DEF VAR xMonth    AS i  NO-UNDO.
def var date1   as da no-undo format "99-99-99".
def var date2   as da no-undo format "99-99-99".
DEF VAR loop1   AS i  NO-UNDO.
DEF VAR loop2   AS i  NO-UNDO.
DEF VAR InvGroup AS c  NO-UNDO.
DEF VAR IGName  AS C NO-UNDO  format "x(30)".
def var fname   as c  no-undo format "x(30)".
DEF VAR bis     AS lo NO-UNDO.
DEF VAR Qty     AS i  NO-UNDO.
DEF VAR ptot    AS DE NO-UNDO.

form
   skip(1)
   "INSTRUCTION:  This module creates a tab separated ascii File of"
   "              all INVOICES / CUSTOMER summed up during the time"
   "              Period determined below (even if no invoices are"
   "              are found)."                                         skip(1)
   "              Sums are by CUSTOMER / AccNum / MONTH."             skip(1)
   "              Dates .......:" date1 "-" date2                      SKIP
   "              Invoice group:" InvGroup IGName                      skip(1)
   "              File name ...:" fname                                skip(5)
with centered width 80 no-label title " Customers bills " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname = TMSUser.RepDir + "/prodinv.txt".
END.

ASSIGN date2 = TODAY.

IF month(date2) NE 1 THEN   
   date1 = date(2,1,year(date2)).
ELSE
   date1 = date(2,1,year(date2) - 1).

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.

   ehto = 9. RUN Syst/ufkey.p.
   UPDATE 
      date1 date2
      InvGroup 
      fname 
   WITH FRAME frm EDITING:
      READKEY.
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
         IF FRAME-FIELD = "InvGroup" THEN DO:
            FIND FIRST InvGroup WHERE 
                       InvGroup.Brand    = gcBrand AND
                       InvGroup.InvGroup = input InvGroup
            NO-LOCK NO-ERROR.
            IF NOT AVAIL InvGroup THEN DO:
               BELL.
               MESSAGE "Unknown InvGroup !".
               NEXT.
            END.
            ELSE DISP InvGroup.IGName @ IGName.

         END.
      END.
      APPLY LASTKEY.
   END.


task:
   repeat WITH FRAME frm ON ENDKEY UNDO, RETURN:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   message "Browsing invoice data ...".

   ASSIGN
      loop1 = year(date1) * 100 + month(date1)
      loop2 = year(date2) * 100 + month(date2)
      Qty   = 0.

   DO i = loop1 TO loop2.
      CREATE imth.
      /* there are only 12 months in a YEAR */
      IF 100 * (i / 100 - int(i / 100)) > 12 THEN i = i + 88. 
      ASSIGN imth.imth = i.
   END.

   OUTPUT STREAM excel TO value(fname).

   FIND FIRST InvGroup where
              InvGroup.Brand    = gcBrand AND
              InvGroup.InvGroup = InvGroup
   no-lock no-error.

   /* printout header */
   PUT STREAM excel UNFORMATTED
      "Invoice data wo. VATAmt summed by AccNum numbers for invoice group: "
      InvGroup.InvGroup " - " InvGroup.IGName my-nl
      "Time Period: " string(date1,"99-99-99") " - " string(date2,"99-99-99")
      my-nl.

   /* column headers */
   PUT STREAM excel UNFORMATTED
      "Abbr."     tab
      "CustNr"    tab
      "CustName"  tab
      "Conn."     tab
      "SM-Code"   tab
      "Product"   tab.

   /* MONTH headers */
   FOR EACH imth.

      PUT STREAM excel UNFORMATTED imth.imth tab.

      DELETE imth.

   END.

   /* BillCode summary column */
   put stream excel unformatted "TOTAL" my-nl.

   FOR EACH Customer no-lock where
            Customer.Brand    = gcBrand AND
            Customer.CustNum  > 1000    AND
            Customer.InvGroup = InvGroup.

      FOR EACH Invoice of Customer no-lock where
               Invoice.InvDate >= date1 AND
               Invoice.InvDate <= date2,

          EACH InvRow of Invoice no-lock.

         ASSIGN
            bis  = TRUE
            xMonth = year(Invoice.InvDate) * 100 + month(Invoice.InvDate).

         /* this BillCode FIRST time ... */
         FIND FIRST imth where
                    imth.imth = xMonth            AND
                    imth.acct = InvRow.SlsAccNum
         no-error.

         /* ... IF so CREATE a column FOR ALL months */
         IF NOT AVAIL imth THEN DO i = loop1 TO loop2.

            CREATE imth.
            ASSIGN 
               imth.imth = i
               imth.acct = InvRow.SlsAccNum.

            /* this BillCode FIRST time NOW */
            FIND FIRST imth where
                       imth.imth = xMonth            AND
                       imth.acct = InvRow.SlsAccNum
            no-error.

         END.

         /* UPDATE the Billed amount */
         imth.amt = imth.amt + InvRow.amt.

      END.

      Qty = Qty + 1.

      /* print: products monthly */
      FOR EACH imth no-lock
      BREAK BY imth.acct.

         /* ALL monthly values FOR this BillCode */
         IF first-of(imth.acct) THEN DO:

            /* customer data */
            PUT STREAM excel UNFORMATTED
               Customer.SearchName              tab
               Customer.CustNum                 tab
               Customer.CustName                tab
               Customer.ConnType   format "DA/IA"  tab
               Customer.Salesman                tab
               imth.acct                      tab.

            ptot = 0.
            /* BillCode: MONTH BY MONTH */
            FOR EACH xMonth no-lock where
                     xMonth.acct = imth.acct
            BY xMonth.imth.

               ptot = ptot + xMonth.amt.
               PUT STREAM excel UNFORMATTED
                  fDEcToC(xMonth.amt,"->>>>>>>9.99") format "x(12)"  tab.

            END.

            /* BillCode: total */
            PUT STREAM excel UNFORMATTED 
               fDEcToC(ptot,"->>>>>>>9.99") format "x(12)" my-nl.

         END.

         DELETE imth.

      END.

   END.

   OUTPUT STREAM excel CLOSE.

   message " " + string(Qty) + " customers Printed !" 
   view-as alert-box title "= Billed CUSTOMERS =".

END.

