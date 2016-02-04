/* ----------------------------------------------------------------------------
  MODULE .......: NNPIEX.P
  FUNCTION .....: List of ALL payments + invoices
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 15.09.98
  MODIFIED .....: 21.01.99 pt ????????????
                  09.04.99 kl AccDate => PaymDate
                  13.04.99 kl CHOOSE unpaid / ALL
                  23.05.00 kl CHOOSE invoicegroup / ALL
                  11.02.02 lp changed header AND excel File 
                  20.03.03 jp External Customer group
                  15.09.03 aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}                              
{Func/excel.i}
{Mc/nnacyp.i}
{Func/tmsparam2.i}

DEF VAR iAR     AS i  NO-UNDO.
def var debt    as de no-undo format "-zzzzzz9.99".
def var CommPaid    as de no-undo format "-zzzzzz9.99".
def var pus     as da no-undo format "99-99-9999".
def var dte     as da no-undo format "99-99-9999".
def var fname   as c  no-undo format "x(30)".
def var exdeci  as lo no-undo format "Period/Comma".
def var exsum   as c  no-undo format "x(12)" EXTENT 3.
def var unpaid  as lo no-undo format "Unpaid/All" init TRUE. 
def var ok      as lo no-undo format "Yes/No".
DEF VAR loop    AS i  NO-UNDO.
DEF VAR InvQty  AS i  NO-UNDO.
DEF VAR InvGroup AS c  NO-UNDO.

DEF VAR ExtCustGrp   AS LOG   NO-UNDO init FALSE FORMAT "Y/N".
DEF VAR extname      AS CHAR  NO-UNDO.
DEF VAR dExtCustGrp  AS CHAR  NO-UNDO FORMAT "x(25)".
DEF VAR kpl          AS INT   NO-UNDO.
DEF VAR CgCustno1    AS INT no-undo.
DEF VAR cgCustno2    AS INT no-undo.


DEF TEMP-TABLE TCustGroup
FIELD CustGroup LIKE CustGroup.CustGroup
INDEX CustGroup CustGroup.

DEF TEMP-TABLE TCGMember
FIELD custno LIKE customer.custnum
INDEX custno custno.

form
   skip(1)
   "  Note: This program creates a summary of ALL PAYMENTS and" SKIP
   "        INVOICES until the determined day.                " skip(1)
   "        File will be in a tab separated form.             " skip(3)
   "        Date ...............:" dte                          SKIP
   "        Invoice group ......:" InvGroup                       
   help "Choose invoicegroup to check, empty for all invoicegroups"
       InvGroup.IgName  format "x(20)"                          SKIP
   "        External CustGroups :" extcustgrp
   HELP "Select external customer groups "
   extname format "x(20)" NO-LABEL                               SKIP
   "        Unpaid / All .......:" unpaid                        SKIP
   "        File Name ..........:" fname                         SKIP
   "        Decimal separator ..:" exdeci                        skip(3)
WITH 
   ROW 1 side-labels width 80 NO-LABELS
   title color value(ctc) " " + ynimi +
   " SUMMARY OF PAYMENTS AND INVOICES " +
   string(pvm,"99-99-99") + " " COLOR value(cfc) FRAME Limit.

ASSIGN
   iAR     = 1
   dte     = pvm.

PAUSE 0.
DISPLAY "ALL" @ InvGroup.IGName WITH FRAME limit.

DO FOR TMSUser:
   FIND TMSUser where
        TMSUser.UserCode = katun
   no-lock.
   fname = TMSUser.RepDir + "/invlist.txt".
END.

Limit:
repeat WITH FRAME Limit ON ENDKEY UNDO, LEAVE:

   ehto = 9. RUN Syst/ufkey.
   UPDATE 
      dte 
      InvGroup 
      extcustgrp
      unpaid 
      fname 
      exdeci 
   WITH FRAME Limit EDITING:
      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         if frame-field = "InvGroup" THEN DO:
            ASSIGN InvGroup.
            if InvGroup NE "" THEN DO:

               find InvGroup where
                    InvGroup.Brand    = gcBrand AND
                    InvGroup.InvGroup = InvGroup
               no-lock no-error.
               if not avail Invgroup THEN DO:
                  BELL.
                  MESSAGE
                  "Invoicegroup code '" + InvGroup + "' is not possible !"
                  VIEW-AS ALERT-BOX error.
               NEXT.
               END.
               ELSE DISP InvGroup.IGName.
            END.
            ELSE DISP "ALL" @ InvGroup.IGNAme.

         END.

         ELSE if frame-field = "extcustgrp" then do:

            ASSIGN
               cgcustno1 = 999999999
               cgcustno2 = 0.

            FOR EACH TCustGroup.
                DELETE TCustGroup.
            END.

            FOR EACH TCGMember.
               DELETE TCGMEMBER.
            END.

            assign frame limit extcustgrp.

            if extcustgrp NE YES   then do:
               disp "NOT SELECTED" @ extname with frame limit.
            end.
            else do:
               RUN Mc/gathecg(INPUT-OUTPUT table TCustGroup).
               /* DISPLAY Customer groups */
               EHTO = 9.
               RUN Syst/ufkey.
               FOR EACH TCustGroup.
                  dExtCustGrp = dExtCustGrp + TCustGroup.CustGroup +
                  ",".
               END.
               /* Remove last comma */

               IF dextcustgrp ne "" THEN
                  dextcustgrp = SUBSTRING(DextCustGrp,1,                                                LENGTH(dextCustGrp) - 1).
               disp ExtCustGrp dExtCustGrp @ extname with frame limit.

               IF  dExtCustGrp = "" THEN
               disp "NOT SELECTED" @ extname with frame limit.

               apply 13 /* ENTER*/ .
            end.
         END.
      END.
      APPLY LASTKEY.
   END.

   if dte = ? or fname = "" THEN LEAVE.

   do-it:
      repeat WITH FRAME Limit:
         ASSIGN ufk = 0 ehto = 0
            ufk[1] = 7 
            ufk[5] = 795
            ufk[8] = 8.
         RUN Syst/ufkey.

         IF toimi = 1 THEN NEXT Limit.
         IF toimi = 8 THEN LEAVE Limit.
         IF toimi = 5 THEN LEAVE do-it.
   END.

   message "Are You sure You want to start listing (Y/N) ?"
           UPDATE ok.
   IF NOT ok THEN LEAVE.

   OUTPUT STREAM excel TO value(fname).
   PUT STREAM excel UNFORMATTED
      "List of all payments + invoices which payday <= " dte.
   RUN Syst/uexskip(1).
   PUT STREAM excel UNFORMATTED
      "Invoice group = " InvGroup.
   RUN Syst/uexskip(1).    
   PUT STREAM excel UNFORMATTED
      "Unpaid / All  = " unpaid format "Unpaid/All".
   RUN Syst/uexskip(2).
   PUT STREAM excel UNFORMATTED
      "Cust.nr"     tab
      "Cust.name"   tab
      "Inv.gr"      tab
      "Inv.no"      tab
      "Inv.date"    tab
      "Due date"    tab
      "Value"       tab
      "Paid"        tab
      "Debt"        tab
      "Paid us"
      .
   RUN Syst/uexskip(1).

   message "Processing ...".

   FOR EACH TCustGroup.
      FOR EACH cgmember WHERE
               CGMember.Brand     = gcBrand AND
               cgmember.custgroup = Tcustgroup.custgroup
      NO-lock.
         FIND FIRST tcgmember WHERE
                    Tcgmember.custno = cgmember.custnum
         NO-LOCK NO-ERROR.
         IF NOT AVAIL tcgmember THEN DO:
            CREATE Tcgmember.
            ASSIGN
               Tcgmember.custno = cgmember.custnum
               kpl = kpl  + 1
               cgcustno1 = min(cgcustno1,cgmember.custnum)
               cgcustno2 = max(cgcustno2,cgmember.custnum).
         END.
      ENd.
   END. 

   if extcustgrp = FALSE THEN DO:
      ASSIGN
      cgcustno1 = 1
      cgcustno2 = 999999999.
   END.

   LOOP:
   FOR EACH Customer no-lock where
            Customer.Brand    = gcBrand          AND
            Customer.CustNum >= cgcustno1        AND
            Customer.CustNum <= cgcustno2        AND
           (if InvGroup = "" THEN TRUE 
            ELSE Customer.InvGroup = InvGroup)   AND
           (If NOT extcustgrp THEN TRUE
            ELSE CAN-FIND(FIRST tcgmember  where
            tcgmember.custno = Customer.CustNum)),

       EACH Invoice of Customer no-lock where
            Invoice.InvDate <= dte.

      ASSIGN pus = 1/1/1900 CommPaid = 0.

      FOR EACH Payment of Invoice NO-LOCK where
               Payment.PaymDate <= dte.

         DO loop = 1 TO 10:

            IF Payment.AccType[loop] = iAR THEN ASSIGN
               CommPaid = CommPaid - Payment.Posting[loop]
               pus  = maximum(pus,Payment.PaymDate).
         END.

      END.

      debt = Invoice.InvAmt - CommPaid.

      /* IF only unpaid AND debt is between -1 ... 1 crown THEN NEXT */
      IF unpaid AND (debt > -1 AND debt < 1) THEN NEXT LOOP.

      ASSIGN
         InvQty   = InvQty + 1
         exsum[1] = string(Invoice.InvAmt,"->>>>>>9.99")
         exsum[2] = string(CommPaid,"->>>>>>9.99")
         exsum[3] = string(debt,"->>>>>>9.99").

      IF NOT exdeci THEN DO loop = 1 TO 3:
         substr(exsum[loop],length(exsum[loop]) - 2) = ",".
      END.

      PUT STREAM excel UNFORMATTED
         Customer.CustNum    tab
         Customer.CustName   tab
         Customer.InvGroup   tab
         Invoice.InvNum     tab
         Invoice.InvDate     tab
         Invoice.DueDate    tab
         exsum[1]          tab
         exsum[2]          tab
         exsum[3].

      IF pus NE 1/1/1900 THEN PUT STREAM excel UNFORMATTED
         tab pus.

      RUN Syst/uexskip(1).

   END.  /* FOR EACH */

   OUTPUT STREAM excel CLOSE.

   MESSAGE
      InvQty "invoices were listed in file" fname
   VIEW-AS ALERT-BOX MESSAGE.

   HIDE MESSAGE no-pause.

   LEAVE.

END. /* Limit */


