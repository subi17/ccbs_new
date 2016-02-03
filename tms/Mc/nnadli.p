 /*---------------------------------------------------------------------------
  MODULE .......: NNADLI.P
  TASK .........: LIST OF ADVANCE PAYMENTS AND DEPOSITS
  APPLICATION ..: nn
  CREATED ......: 23.05.1999 pt
  MODIFIED .....: 18.09.02 jr   added help text
                  26.09.02/aam  customer balances in table CustBal,
                                new logic also otherwise
                  05.03.03/tk   tokens              
                  12.09.03/aam  brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}  

{Syst/utumaa.i "new"}
{Func/fcustbal.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'custbal'}

assign tuni1 = "nnadli"
       tuni2 = "".

DEF VAR lcInvGroup  LIKE  InvGroup.InvGroup NO-UNDO.        

DEF VAR tot      AS DE NO-UNDO EXTENT 3.
DEF VAR rl       AS i  NO-UNDO.
DEF VAR sl       AS i  NO-UNDO.

DEF VAR ldCustDP AS DEC NO-UNDO.
DEF VAR ldCustOP AS DEC NO-UNDO.
DEF VAR ldCustAP AS DEC NO-UNDO.

DEF TEMP-TABLE ttCust NO-UNDO   
   FIELD CustNum  AS INT
   FIELD CustName AS CHAR
   FIELD CustDP   AS DEC
   FIELD CustOP   AS DEC
   FIELD CustAP   AS DEC
   INDEX CustName CustName CustNum. 

form
   skip(1)
"  Note: This program prints out a summary of customers who have either" skip
"        advance payments and/or deposits."                             skip(2)
"        Invoicing Group" 
lcInvGroup HELP "Invoicing Group" InvGroup.IGName skip(11)
WITH
   width 80 overlay title 
   " " + ynimi + " Summary Of AdvPayments and Deposits " + 
   STRING(pvm,"99-99-99") + " "
   NO-LABELS FRAME rajat.

form header /* header FOR printout */                
   fill("=",78) format "x(78)" SKIP
   ynimi AT 1
      "SUMMARY OF DEPOSITS" AT 34
      "Page" to 70 sl format "zz9" TO 78

   "AND ADVANCE PAYMENTS"  AT 34
      string(pvm,"99.99.99") TO 78    SKIP
   fill("=",78) format "x(78)"
   skip(1)

   "Customer"   TO 10
   "Customer's" AT 12
   "Amount of"  TO 50
   "Amount of"  TO 64
   "Amount of"  TO 78

   SKIP

   "Number"      TO 10
   "Name"        AT 12
   "Deposit"     TO 50
   "Overpayment" TO 64
   "AdvPayment"  TO 78

   SKIP
   fill("-",78) at 1 format "x(78)"
   SKIP
WITH
   width 80 NO-LABEL no-box FRAME hdr-1.

FUNCTION fChkCust RETURNS LOGICAL.

   ASSIGN ldCustDP = fGetCustBal(Customer.CustNum,"TOTAL","DP")
          ldCustOP = fGetCustBal(Customer.CustNum,"TOTAL","OP")
          ldCustAP = fGetCustBal(Customer.CustNum,"TOTAL","AP").

   IF ldCustDP = 0 AND
      ldCustOP = 0 AND
      ldCustAP = 0 
   THEN RETURN FALSE.

   CREATE ttCust.
   ASSIGN ttCust.CustNum  = Customer.CustNum
          ttCust.CustName = Customer.CustName
          ttCust.CustDP   = ldCustDP
          ttCust.CustOP   = ldCustOP
          ttCust.CustAP   = ldCustAP.

   RETURN TRUE. 

END FUNCTION.

rajat:
repeat WITH FRAME rajat:
   ehto = 9.  RUN ufkey.

   UPDATE
   lcInvGroup
   WITH FRAME rajat
   EDITING:
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME rajat:
         HIDE MESSAGE.
         if frame-field = "lcInvGroup" THEN DO :
            if input lcInvGroup ne "" THEN DO:
               FIND InvGroup where 
                    InvGroup.Brand    = gcBrand AND
                    InvGroup.InvGroup = INPUT lcInvGroup 
                    no-lock no-error.
               IF NOT AVAIL InvGroup THEN DO:
                  BELL.
                  message "Unknown Invoicing Group !".
                  NEXT.
               END.
               DISP InvGroup.IGName.
            END.
            else disp "ALL" @ InvGroup.IGName.
         END.
      END.
      APPLY LASTKEY.
   END.               

Action:
   repeat WITH FRAME sel:
       ASSIGN
       ufk = 0 ehto = 0 ufk[1] = 91  ufk[5] = 63 ufk[8] = 8.
       RUN ufkey.


       IF toimi = 1 THEN NEXT rajat.
       IF toimi = 8 THEN LEAVE rajat.
       IF toimi = 5 THEN DO:

          tila = TRUE.
          {Syst/tmsreport.i "leave rajat"}

          LEAVE Action.
       END.
   END.

   message "Printing ...".

   ASSIGN sl = 1 rl = 8.
   view STREAM tul FRAME hdr-1.

   IF lcInvGroup NE "" THEN 
   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand    = gcBrand AND
            Customer.InvGroup = lcInvGroup:

      fChkCust().
   END.

   ELSE 
   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand  = gcBrand:
      fChkCust().
   END.

   FOR EACH ttCust 
   BREAK
   BY   ttCust.CustName
   BY   ttCust.CustNum.

      IF rl >= skayt1 THEN DO:
         {Syst/uprfeed.i rl}
         ASSIGN sl = sl + 1 rl = 8.
         view STREAM tul FRAME hdr-1.
      END.

      PUT STREAM tul

         ttCust.CustNum      TO 10 FORMAT ">>>>>>>9"
         ttCust.CustName     AT 12 format "x(25)"
         ttCust.CustDP       TO 50 format "->,>>>,>>9.99"
         ttCust.CustOP       TO 64 format "->,>>>,>>9.99"
         ttCust.CustAP       TO 78 format "->,>>>,>>9.99"
         SKIP.

      ASSIGN 
      tot[1] = tot[1] + ttCust.CustDP
      tot[2] = tot[2] + ttCust.CustOP
      tot[3] = tot[3] + ttCust.CustAP
      rl     = rl + 1.

      IF last(ttCust.CustNum) THEN DO:

         IF rl >= skayt1 - 1 THEN DO:
            {Syst/uprfeed.i rl}
            ASSIGN sl = sl + 1 rl = 8.
            view STREAM tul FRAME hdr-1.
         END.


         PUT STREAM tul
         "--------------------------------------" TO 78 SKIP
         "** TOTAL"                               AT 12
         tot[1] format "->,>>>,>>9.99"            TO 50
         tot[2] format "->,>>>,>>9.99"            TO 64
         tot[3] format "->,>>>,>>9.99"            TO 78
         SKIP.
         rl = rl + 2.

         {Syst/uprfeed.i rl}

         tila = FALSE.
         {Syst/tmsreport.i}
      END.

   END.

   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE rajat.
END. /* rajat */
HIDE FRAME rajat no-pause.
