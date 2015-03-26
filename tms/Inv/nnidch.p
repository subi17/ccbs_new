/* -----------------------------------------------
  MODULE .......: nnidch.p
  TASK .........: Change invoice dates
  SOLUTION .....: Ticket Master
  CREATED ......: 18.09.98 pt
  CHANGED ......: 09.10.98 kl inv/due DAY, InvGroup
                  16.12.98 kl ufk[5] = 795
                  04.11.02 jr Eventlog
                  05.03.03 tk tokens
                  15.09.03/aam brand
                  30.03.05/aam change only unprinted, unpaid invoices,
                               calculate qty before changing (F3),
                               check invgroup
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{eventval.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'invoice'}

DEF VAR lcInvGroup LIKE InvGroup.InvGroup  NO-UNDO.
def var IGName     like InvGroup.InvGroup  no-undo format "x(20)".
DEF VAR InvNum1    LIKE Invoice.InvNum     NO-UNDO.
DEF VAR InvNum2    LIKE Invoice.InvNum     NO-UNDO.
DEF VAR ldtInvDate LIKE Invoice.InvDate     NO-UNDO.
def var i-date    as lo format "Yes/No"  NO-UNDO.
def var e-date    as lo format "Yes/No"  NO-UNDO.
DEF VAR diff      AS INT                 NO-UNDO.
DEF VAR Qty       AS INT                 NO-UNDO.
def var ok        as lo format "Yes/No"  NO-UNDO.
DEF VAR hlp       AS DA                  NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhInvoice).
   END.
END.

form
   skip(1)
   "   CHECKING INVOICE NUMBERS   " SKIP
   "   Please wait a moment ...   " 
   skip(1)
with overlay centered row 9 title " NOTICE " FRAME msg.

form
  skip(1)
"  This program changes the invoice Date and/or the dueday"  skip
"  on unprinted, unpaid invoices that are within the criteria" skip
"  determined below." skip(1)

"               Invoice group ..:"  lcInvGroup 
help "Invoices from customers in this invoice group, empty for all" 
                                      IGName SKIP
"               Invoice Date ...:"  ldtInvDate 
help "Invoicing Date on invoices"           SKIP
"               Invoice nos. ...:"  InvNum1
help "Smallest invoice number" "-" InvNum2 help "Greatest invoice number"  skip(1)
"               Move dates with :" diff format "+>9"
    help "How many days (+/-) ?" "days"   skip(1)
"               Change"             skip
"                  Invoice Date :"  i-date 
help "Do You want to chage the INVOICE Date (Y/N) ?" SKIP
"                  Expiry  Date :"  e-date 
help "Do You want to chage the EXPIRY Date (Y/N) ?"  skip(1)
"               Invoices changed:" Qty no-label
skip(1)
WITH
   OVERLAY centered NO-LABELS
   width 80 title " Move invoice days " FRAME rajat.

ASSIGN
   diff   = 0 
   InvNum1 = 0 
   InvNum2 = 99999999.

PAUSE 0.

rajat:
repeat WITH FRAME rajat ON ENDKEY UNDO, RETURN:
   ehto = 9. RUN ufkey.

   UPDATE
      lcInvGroup
      ldtInvDate
      InvNum1 InvNum2 
      diff
      i-date e-date
   EDITING.

      READKEY. nap = keylabel(LASTKEY).

      if nap = "F4" THEN RETURN.

      IF lookup(nap,poisnap) > 0 THEN DO:

         if frame-field = "lcInvGroup" THEN DO:
            if input lcInvGroup = "" then assign IGName = "ALL".
            ELSE DO:
               FIND FIRST InvGroup no-lock where
                          InvGroup.Brand    = gcBrand AND
                          InvGroup.InvGroup = INPUT lcInvGroup
               NO-ERROR.
               IF NOT AVAIL InvGroup THEN DO:
                  BELL.
                  message "Invoice group does not exist !".
                  NEXT-PROMPT lcInvGroup.
                  NEXT.
               END.
               ELSE ASSIGN IGName = InvGroup.IGName.
            END.
            ASSIGN lcInvGroup.
            DISP IGName WITH FRAME rajat.
         END.

         if frame-field = "i-date" THEN DO:

            IF INPUT i-date THEN DO:
               e-date = TRUE.
               DISP e-date WITH FRAME rajat.
            END.

         END.

      END.

      APPLY LASTKEY.

   END.

   toimi:
   repeat WITH FRAME rajat:
      ASSIGN ufk = 0  
             ufk[1] = 7 
             ufk[3] = 1777
             ufk[5] = 795 
             ufk[8] = 8.
             
      IF InvNum2 = 0 OR diff = 0 THEN ufk[5] = 0.

      ehto = 0. RUN ufkey.
      IF toimi = 1 THEN NEXT rajat.

      ELSE IF toimi = 3 THEN DO:
         
         qty = 0.
         FOR EACH Invoice NO-LOCK USE-INDEX InvDate where
                  Invoice.Brand   = gcBrand    AND
                  Invoice.InvDate = ldtInvDate AND
                  Invoice.PrintState = 0       AND
                  Invoice.PaymState  = 0       AND
                  Invoice.DDState    = 0       AND 
                  Invoice.InvType    < 3       AND 
                  Invoice.InvNum >= InvNum1    AND
                  Invoice.InvNum <= InvNum2,
            FIRST Customer OF Invoice NO-LOCK:
      
            IF lcInvGroup > "" AND Customer.InvGroup NE lcInvGroup THEN NEXT.
           
            qty = qty + 1.
            IF qty MOD 100 = 0 THEN DO:
               PAUSE 0.
               DISP qty LABEL "Collecting" WITH SIDE-LABELS OVERLAY 
                    ROW 10 CENTERED FRAME fQty.
            END.
                   
         END.
         
         HIDE FRAME fqty NO-PAUSE.
         
         MESSAGE qty "invoices' dates will be changed with given criteria."
         VIEW-AS ALERT-BOX.
      END. 
      
      ELSE IF toimi = 8 THEN LEAVE rajat.
      ELSE IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Do you REALLY want to change dates with " + string(diff) +
                 " days (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE toimi.
      END.
   END.

   qty = 0.
   FOR EACH Invoice USE-INDEX InvDate where
            Invoice.Brand   = gcBrand    AND
            Invoice.InvDate = ldtInvDate AND
            Invoice.PrintState = 0       AND
            Invoice.PaymState  = 0       AND
            Invoice.DDState    = 0       AND 
            Invoice.InvType    < 3       AND 
            Invoice.InvNum >= InvNum1    AND
            Invoice.InvNum <= InvNum2,
      FIRST Customer OF Invoice NO-LOCK:
      
       IF lcInvGroup > "" AND Customer.InvGroup NE lcInvGroup THEN NEXT.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).

       IF i-date THEN Invoice.InvDate = Invoice.InvDate + diff.
       IF e-date THEN Invoice.DueDate = Invoice.DueDate + diff.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).

       Qty = Qty + 1.

       PAUSE 0.
       DISP Qty WITH FRAME rajat.
   END.

   message "Dates changed on" Qty "invoices - press ENTER !".
   PAUSE no-message.

   LEAVE.
END.
HIDE FRAME rajat no-pause.
HIDE MESSAGE.

