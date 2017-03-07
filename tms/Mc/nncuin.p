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
   FIELD yr   AS i
   FIELD imth AS i
   FIELD Amt  AS DE

   INDEX imth AS primary
      yr
      imth.

def var ok      as lo no-undo format "Yes/No".
DEF VAR i       AS i  NO-UNDO.
DEF VAR xMonth    AS i  NO-UNDO.
def var date1   as da no-undo format "99-99-99".
def var date2   as da no-undo format "99-99-99".
DEF VAR loop1   AS i  NO-UNDO.
DEF VAR loop2   AS i  NO-UNDO.
DEF VAR InvGroup AS c  NO-UNDO.
DEF VAR IGName  AS C  NO-UNDO format "x(30)".
def var fname   as c  no-undo format "x(30)".
def var zero    as lo no-undo format "All/Inv".
DEF VAR bis     AS lo NO-UNDO.
DEF VAR Qty     AS i  NO-UNDO.

form
   skip(1)
   "INSTRUCTION:  This module creates a tab separated ascii File of"
   "              all INVOICES / CUSTOMER summed up during the time"
   "              Period determined below (even if no invoices are"
   "              are found)."                                         skip(3)
   "              Dates .......:" date1 
HELP "From date"   
   "-" date2 
HELP "To date"                                                         SKIP
   "              Invoice group:" InvGroup 
HELP "Invoice group code"   IGName                                     SKIP
   "              Print all ...:" zero
help "Print all / only those who have invoices (A/I)"                  skip(1)
   "              File Name ...:" fname                                skip(3)
with centered width 80 no-label title " Customers bills " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname = TMSUser.RepDir + "/callamt.txt".
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
      zero
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

   PUT STREAM excel UNFORMATTED
      "CustNr"        tab
      "CustName"      tab
      "InvGroup"      tab
      "Cat."          tab
      "Size"          tab
      "Conn."         tab
      "SM-Code"       tab
      "Salesman"      tab
      "C"             tab
      "Closed"        tab.

   FOR EACH imth.
      PUT STREAM excel UNFORMATTED
         imth.imth tab.
   END.
   PUT STREAM excel UNFORMATTED my-nl.

   FOR EACH Customer no-lock where
            Customer.Brand    = gcBrand AND
            Customer.CustNum  > 1000    AND
            Customer.InvGroup = InvGroup,

      FIRST Salesman no-lock where
            Salesman.Brand    = gcBrand AND
            Salesman.Salesman = Customer.Salesman,

      FIRST Salesoffice no-lock where
            SalesOffice.Brand       = gcBrand AND
            Salesoffice.SalesOffice = Salesman.SalesOffice.

      DISP 
         Customer.CustNum Customer.CustName
      WITH FRAME frm.
      PAUSE 0.

      FOR EACH imth.
         imth.amt = 0.
      END.

      bis = FALSE.
      FOR EACH Invoice of Customer no-lock where
               Invoice.InvDate >= date1 AND
               Invoice.InvDate <= date2.

         ASSIGN
            bis  = TRUE
            xMonth = year(Invoice.InvDate) * 100 + month(Invoice.InvDate).

         FIND FIRST imth where
                    imth.imth = xMonth
         no-error.
         imth.amt = imth.amt + Invoice.InvAmt.

      END.

      IF NOT zero THEN IF NOT bis THEN NEXT.

      Qty = Qty + 1.

      PUT STREAM excel UNFORMATTED
         Customer.CustNum       tab
         Customer.CustName      tab
         Customer.InvGroup      tab
         Customer.Category       tab
         Customer.Size         tab
         Customer.ConnType         tab
         Customer.Salesman      tab
         Salesman.SmName     tab
         Salesoffice.CostCentre tab.

      IF Customer.ContrEnd = ? THEN
         PUT STREAM excel UNFORMATTED tab.
      ELSE  
         PUT STREAM excel UNFORMATTED 
         fDateFMT(Customer.ContrEnd,"dd-mm-yy") tab.

      FOR EACH imth.
         PUT STREAM excel UNFORMATTED
            fDEcToC(imth.amt,"->>>>>>>9.99") format "x(12)"  tab.
      END.
      PUT STREAM excel UNFORMATTED my-nl.

   END.

   OUTPUT STREAM excel CLOSE.

   message " " + string(Qty) + " customers Printed !" 
   view-as alert-box title "= Billed CUSTOMERS =".

END.

