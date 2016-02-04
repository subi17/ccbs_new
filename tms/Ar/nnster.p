 /*---------------------------------------------------------------------------
  MODULE .......: NNSTER.P
  TASK .........: SUORITUSTILIERITTELYN TULOSTUS 
  APPLICATION ..: nn
  CREATED ......: 13.04.1999 pt
  MODIFIED .....: 23.04.1999 pt  IF Deposit OR adv payment no InvNum is Printed
                  08.02.02   lp  more sign TO Customer.CustNum FORMAT -->8
                  05.03.2002 aam use AccDate instead of AccDate
                  06.08.02   aam longer format for page nbr
                  15.10.02   aam currency handling
                  15.09.03   aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}  
{Func/fcurrency.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'payment'}
{Syst/utumaa.i "new"}

assign tuni1 = "nnster"
       tuni2 = "".

def var pvm1     as da no-undo format "99-99-99".
def var pvm2     as da no-undo format "99-99-99".
def var acct     as i  no-undo format "zzzzz9".
def var tnro1    as i  no-undo format "zzzzzzz9".
def var tnro2    as i  no-undo format "zzzzzzz9".

DEF VAR xacct    AS i  NO-UNDO EXTENT 10.
DEF VAR xamt      AS DE NO-UNDO EXTENT 10.
DEF VAR yhtmk    AS DE NO-UNDO.
DEF VAR i        AS i  NO-UNDO.
DEF VAR rl       AS i  NO-UNDO.
DEF VAR sl       AS i  NO-UNDO.
DEF VAR po       AS i  NO-UNDO.
def var tot      as lo no-undo format "Total only/Detailed".
DEF VAR totamt   AS DE NO-UNDO.
DEF VAR ChInvNum   AS c  NO-UNDO.

DEF VAR lcCurrency    AS   CHAR            NO-UNDO. 
DEF VAR ldRate        AS   DEC             NO-UNDO. 
DEF VAR ldHomeAmt     AS   DEC             NO-UNDO. 


DEF TEMP-TABLE ttTotal NO-UNDO
   FIELD Currency AS CHAR
   FIELD Amt      AS DEC
   FIELD HomeAmt  AS DEC
   INDEX Currency Currency.

DEF TEMP-TABLE ttDate NO-UNDO
   LIKE ttTotal. 

FIND FIRST Account no-lock.

form
   skip(1)
"  Note: This program prints out a summary of payments where a certain" skip
"        Account number occurs."                                        skip(2)
"        Payments done during ..:"  pvm1
help "Earliest day of payment" "-" pvm2
help "latest day of payment" SKIP
"        Voucher nos. ..........: " tnro1 help "First Voucher no." "-"
tnro2 help "Last Voucher No."  SKIP
"        Account No. ...........:    "  acct format "zzzzz9"
help "Account number to be specified" Account.AccName NO-LABEL SKIP
"        Type of printout ......:" tot
help "Only (T)otal amount by Account or (D)etailed printout ?"
skip(8)
WITH
   width 80 overlay 
   title " " + ynimi + " Account Summary " + STRING(pvm,"99-99-99") + " "
   NO-LABELS FRAME rajat.

form header /* header FOR printout */                
   fill("=",78) format "x(78)" SKIP
   ynimi AT 1
      "ACCOUNT SUMMARY" AT 34
      "Page" to 70 sl format "zzz9" TO 78
   "Acct " at 1 acct format "zzzzz9" AccName format "x(18)"
      "DURING " at 34 pvm1 format "99.99.99"  "-" pvm2 format "99.99.99"
      string(pvm,"99.99.99") TO 78    SKIP
   fill("=",78) format "x(78)"
   skip(1)
   "Invoice"    TO  8
   "Payment"    TO 20
   "Voucher"    TO 28
   "Booked"     TO 42
   "Cur"        AT 44
   "Cust"       TO 55
   "Customer's" AT 57

   SKIP

   "number"     TO  8
   "date"       TO 20
   "number"     TO 28
   "amount"     TO 42
   "number"     TO 55
   "name"       AT 57
         SKIP
         fill("-",78) at 1 format "x(78)"
         SKIP
WITH
   width 80 NO-LABEL no-box FRAME sivuots.


FUNCTION fChgPage RETURNS LOGICAL
   (iAddLine AS INT).

   IF rl + iAddLine >= skayt1 THEN DO:
      ASSIGN sl = sl + 1 rl = 8.
      {Syst/uprfeed.i rl}
      view STREAM tul FRAME sivuots.
   END.

END FUNCTION.

ASSIGN
pvm2    = date(month(TODAY),1,year(TODAY))  - 1
pvm1    = date(month(pvm2),1,year(pvm2))
tnro1   = 0
tnro2   = 9999999.



rajat:
repeat WITH FRAME rajat:
   ehto = 9.  RUN Syst/ufkey.

   UPDATE
   pvm1  pvm2  validate (input pvm2  >= input pvm1,"Invalid order !")
   tnro1 tnro2 validate (input tnro2 >= input tnro1,"Invalid order 1")
   acct
   tot
   EDITING:
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME rajat:
         HIDE MESSAGE.
         if frame-field = "acct" THEN DO:
            IF INPUT acct NE 0 THEN DO:
               FIND Account where
                    Account.Brand  = gcBrand AND
                    Account.AccNum = INPUT acct no-lock no-error.
               IF NOT AVAIL Account THEN DO:
                  BELL.
                  message "Unknown Account !".
                  NEXT.
               END.
               DISP Account.AccName.
            END.
         END.
      END.
      APPLY LASTKEY.
   END.               

toimi:
   repeat WITH FRAME toimi:
       ASSIGN
       ufk = 0 ehto = 0 ufk[1] = 91  ufk[5] = 63 ufk[8] = 8.
       IF acct = 0 THEN ufk[5] = 0.
       RUN Syst/ufkey.


       IF toimi = 1 THEN NEXT rajat.
       IF toimi = 8 THEN LEAVE rajat.
       IF toimi = 5 THEN DO:

          tila = TRUE.
          {Syst/tmsreport.i "leave rajat"}

          LEAVE TOIMI.
       END.
   END.

   EMPTY TEMP-TABLE ttTotal.
   EMPTY TEMP-TABLE ttDate.

   message "Printing ...".

   ASSIGN sl = 1 rl = 8.
   view STREAM tul FRAME sivuots.

   FOR EACH Payment no-lock where
           Payment.Brand    = gcBrand   AND
           Payment.AccDate >= pvm1      AND
           Payment.AccDate <= pvm2      AND

           Payment.Voucher  >= tnro1    AND
           Payment.Voucher  <= tnro2    AND

       (   Payment.AccNum[1]   = acct  OR
           Payment.AccNum[2]   = acct  OR
           Payment.AccNum[3]   = acct  OR
           Payment.AccNum[4]   = acct  OR
           Payment.AccNum[5]   = acct  OR
           Payment.AccNum[6]   = acct  OR
           Payment.AccNum[7]   = acct  OR
           Payment.AccNum[8]   = acct  OR
           Payment.AccNum[9]   = acct  OR
           Payment.AccNum[10]  = acct),

           Customer no-lock where
           Customer.CustNum = Payment.CustNum
   BREAK

   BY   Payment.AccDate
   BY   Payment.Voucher:

       /* currency */
       IF Payment.InvNum > 0 THEN DO:
          FIND Invoice OF Payment NO-LOCK NO-ERROR.
          IF AVAILABLE Invoice THEN lcCurrency = Invoice.Currency.
          ELSE lcCurrency = lcHomeCurr. 
       END.
       ELSE ASSIGN lcCurrency = Customer.Currency.

       IF lcCurrency = "" THEN lcCurrency = lcHomeCurr. 

       /* rate; this is not the best way to do this, but so far the
          only way */
       ldRate = fCurrRate(lcCurrency,Payment.AccDate). 

       /* take rate from invoice if rates are not updated */
       IF ldRate = 0 AND Payment.InvNum > 0 AND AVAILABLE Invoice
       THEN ldRate = Invoice.ExchRate.

      /* collect ALL bookings WITH this Account from a single payment record */
      ASSIGN xacct = 0 xamt = 0 i = 0 totamt = 0.
      DO po = 1 TO 10:
         IF Payment.AccNum[po] = acct THEN
         ASSIGN
            i = i + 1
            xacct[i] = Payment.AccNum[po]
            xamt[i]  = Payment.Posting[po].
      END.

      FIND FIRST ttTotal WHERE
                 ttTotal.Currency = lcCurrency NO-ERROR.
      IF NOT AVAILABLE ttTotal THEN DO:
         CREATE ttTotal.
         ASSIGN ttTotal.Currency = lcCurrency.
      END.

      FIND FIRST ttDate WHERE
                 ttDate.Currency = lcCurrency NO-ERROR.
      IF NOT AVAILABLE ttDate THEN DO:
         CREATE ttDate.
         ASSIGN ttDate.Currency = lcCurrency.
      END. 

      kirjaus:
      /* Print out every booking WITH desired acct from a single payment rec */
      DO i = 1 TO 10:
         IF xacct[i] = 0 THEN NEXT KIRJAUS.

         fChgPage(0).   

         IF NOT tot THEN DO:

            IF Payment.InvNum NE 0 THEN 
               ChInvNum = string(Payment.InvNum,"zzzzzzz9").
            ELSE
               ChInvNum = " ADV/DEP".

            PUT STREAM tul
            ChInvNum           at  1 format "x(8)"          /* Invoice      */
            Payment.AccDate  to 20 format "99.99.99"         /* booking DAY  */
            Payment.Voucher  to 28 format "zzzzzz9"         /* Voucher No.  */
            xamt[i]          to 42 format "->,>>>,>>9.99"   /* Booked Qty   */
            lcCurrency       AT 44 FORMAT "X(3)"
            Customer.CustNum   to 55 format "zzzzzzz9"        /* CustNo.      */
            Customer.CustName  at 57 format "x(22)"           /* CustName     */
            SKIP.

            rl = rl + 1.
         END.

         ASSIGN ldHomeAmt       = fToHomeCurr(xAmt[i],ldRate)
                ttDate.Amt      = ttDate.Amt      + xAmt[i]
                ttDate.HomeAmt  = ttDate.HomeAmt  + ldHomeAmt
                ttTotal.Amt     = ttTotal.Amt     + xAmt[i]
                ttTotal.HomeAmt = ttTotal.HomeAmt + ldHomeAmt.

      END.

      IF last-of(Payment.AccDate) THEN DO:
         /*******************
         * Sub-total BY DAY *
         *******************/

         fChgPage(1).

         PUT STREAM tul
         " * TOTAL BY" 
         Payment.AccDate  to 20 format "99.99.99".  /* booking DAY    */

         FOR EACH ttDate:

            fChgPage(0).

            PUT STREAM tul ttDate.Amt      format "->>,>>>,>>9.99" TO 42
                           ttDate.Currency FORMAT "X(3)" AT 44
                           " *"
                           SKIP.
            rl = rl + 1. 

            ACCUMULATE ttDate.Currency (COUNT)
                       ttDate.HomeAmt  (TOTAL).

         END.

         IF (ACCUM COUNT ttDate.Currency) > 1 THEN DO:

            fChgPage(1).

            PUT STREAM tul UNFORMATTED
               FILL("-",18) TO 46 SKIP.
            PUT STREAM tul 
               " * " AT 1 
               Payment.AccDate FORMAT "99-99-9999" 
               " TOTAL "
               (ACCUM TOTAL ttDate.HomeAmt) format "->>,>>>,>>9.99" TO 42
               lcHomeCurr  FORMAT "X(3)" AT 44 
               SKIP.
            rl = rl + 2.
         END.

         EMPTY TEMP-TABLE ttDate.       

         IF NOT tot AND rl < skayt1 THEN DO:
            PUT STREAM tul skip(1).
            rl = rl + 1.
         END.   

      END.

      IF last(Payment.AccDate) THEN DO:

         fChgPage(1).

         PUT STREAM tul UNFORMATTED
         FILL("-",18) TO 46 SKIP
         "** TOTAL" AT 1.
         rl = rl + 1.

         FOR EACH ttTotal:

            fChgPage(0).

            PUT STREAM tul ttTotal.Amt      format "->>,>>>,>>9.99" TO 42
                           ttTotal.Currency FORMAT "X(3)" AT 44
                           " **"
                           SKIP.
            rl = rl + 1. 

            ACCUMULATE ttTotal.Currency (COUNT)
                       ttTotal.HomeAmt  (TOTAL).
         END.

         IF (ACCUM COUNT ttTotal.Currency) > 1 THEN DO:

            fChgPage(1).

            PUT STREAM tul UNFORMATTED
               FILL("-",18) TO 46 SKIP.
            PUT STREAM tul 
               "** GRAND TOTAL " AT 1
               (ACCUM TOTAL ttTotal.HomeAmt) format "->>,>>>,>>9.99" TO 42
               lcHomeCurr  FORMAT "X(3)" AT 44 
               " **" 
               SKIP.
            rl = rl + 2.
         END.

         tila = FALSE.

         {Syst/uprfeed.i rl}
         {Syst/tmsreport.i}
      END.

   END.

   MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

   LEAVE rajat.
END. /* rajat */
HIDE FRAME rajat no-pause.

