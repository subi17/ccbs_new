/* ------------------------------------------------------------------------
  MODULE .......: nnlasu.p
  FUNCTION .....: Myyntilaskun suoritusten BROWSE
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 23.02.97 pt
  changePVM ....: 28.07.1998 pt  PaymDate
                  05.08.1998 pt  F1: browse accounts
                  09.09.1998 pt  F3: print memo
                  13.10.1998 pt  F2: payment notes
                  06.09.1999 pt  Show beginning of memo[1] (BG/PG)
                  11.01.2000 jp  Interests added AND memo removed
                  05.04.2000 pt  FUNCTION Key labels repainted after F1
                  06.02.2002/aam longer format for "tosite",
                                 show PaymSrc,
                                 more lines (12) 
                  20.05.2002/tk  RUN memo
                  12.11.2002/jr "suoritus" => "payment" in memo
                  03.03.2003 tk tokens
                  15.09.2003/aam brand, 
                  06.02.2004 jp custnum for memo
                  04.05.04/aam account with 6 digits
                  04.11.04/aam disp ePayment always if mtilan=5
                  07.02.06/aam disp eventlog (f7)
  Version ......: M15
-------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'payment'}

DEF INPUT PARAMETER InvNum LIKE Invoice.InvNum. 

DEF VAR rtab      AS re NO-UNDO EXTENT 20 init ?.
DEF VAR i         AS i  NO-UNDO.
def var AccName   as c  format "x(30)" NO-UNDO.
DEF VAR ufkey     AS lo NO-UNDO init TRUE.
def var inte      as de no-undo format "->>>>9.99".
def var int-invno as i  no-undo  format ">>>>>>>>>".
DEF VAR TotPaid   AS DE NO-UNDO  format "->>>>>9.99".


cfc = "lis". RUN ufcolor.

FIND Invoice where Invoice.InvNum = InvNum no-lock.

IF Invoice.PaymState = 5 THEN DO:
   MESSAGE "There is a pending ePayment on invoice;" SKIP
           STRING(Invoice.ePaymDate,"99-99-9999") SKIP
           Invoice.ePaymAmt Invoice.Currency
   VIEW-AS ALERT-BOX
   TITLE " ePaid ".
END.

IF NOT can-find(FIRST Payment of Invoice) THEN DO:
   BELL.
   message "There are no payments on invoice no." InvNum
   " - press ENTER !".
   PAUSE no-message.
   RETURN.

END.


DO WITH FRAME PaidAmt.
FOR EACH Payment of Invoice no-lock 
BY Payment.PaymDate 
BY Payment.Voucher
WITH FRAME PaidAmt.

   ASSIGN
      inte      = 0
      int-invno = 0.


   FIND FIRST CustIntEvent WHERE 
              CustIntEvent.Voucher = Payment.Voucher 
   NO-LOCK NO-ERROR.
   IF AVAILABLE CustIntEvent THEN 
   ASSIGN
      inte      = CustIntEvent.Amt
      int-invno = CustIntEvent.BilledInvNum.

   DISPLAY
   Payment.PaymDate     column-label "DatePaid"
   Payment.Voucher     column-label "Voucher" format ">>>>>>>9"
   Payment.AccNum[1]           column-label "1.acct"
   Payment.PaymAmt              column-label "Money"
   Discount                column-label "Discnt"
   TotAmt              column-label "Total"
   inte                column-label "Int."
   PaymSrc             

 WITH
   OVERLAY ROW 3 12 DOWN COLOR value(cfc) TITLE COLOR value(ctc)
   " PAYMENTS ON INVOICE NO. " + string(InvNum) + " " centered FRAME PaidAmt.

   ASSIGN rtab[FRAME-LINE] = recid(Payment).

   IF i < 10 THEN DOWN.
END. 
up frame-line(PaidAmt) - 1 WITH FRAME PaidAmt.

toimi:
repeat WITH FRAME PaidAmt:
 
   IF ufkey THEN DO:
      ufkey = FALSE.
      ASSIGN ufk = 0 ufk[1] = 972 ufk[2] = 0 ufk[3] = 927 
             ufk[7] = 1752 ufk[8] = 8 ehto = 3.
      RUN ufkey.
   END.


   CHOOSE ROW Payment.Voucher 
   help "An unique voucher number for payment; F1: look all accounts"
   {uchoose.i} no-error.
   IF rtab[FRAME-LINE] = ? THEN NEXT.

   if lookup(keylabel(lastkey),"1,F1") > 0 THEN DO:
      IF rtab[FRAME-LINE] NE ? THEN DO:
         TotPaid = 0.

         FIND Payment where recid(Payment) = rtab[FRAME-LINE] no-lock.
         DO i = 1 TO 10 WITH OVERLAY FRAME acct:

            IF Payment.AccNum[i] NE 0 THEN DO:
               FIND FIRST Account where 
                          Account.Brand  = Payment.Brand AND
                          Account.AccNum = Payment.AccNum[i]
               no-lock no-error.
               IF AVAIL Account THEN AccName = Account.AccName.
               else AccName = "!! UNKNOWN AccNum !!".
            END.

            else AccName = "".

            IF Payment.AccType[i] = 1 THEN 
               TotPaid = TotPaid - Payment.Posting[i].

            disp Payment.AccNum[i]  column-label "Account"  format "zzzzzz"
                 AccName             column-label "NameOfAccount"
                 Payment.Posting[i]  column-label "Sum" 
                    when Payment.Posting[i] NE 0  
                 Payment.AccType[i] column-label "Type"
                    when Payment.Posting[i] NE 0
            WITH
              10 DOWN OVERLAY centered 
              title " Voucher " +  string(Payment.Voucher)  +
              " " ROW 2.
         END.     
         PAUSE 0.
         DISP 
            TotPaid label "Amount Booked as PAYMENT for A/R"
         WITH centered ROW 16 side-labels OVERLAY FRAME TotPaid.   


         ASSIGN ufk = 0 ufk[8] = 8 ufk[9] = 1 ehto = 0. RUN ufkey.
         ufkey = TRUE.
         HIDE FRAME acct no-pause.
         HIDE FRAME TotPaid no-pause.
         NEXT.
      END.   
   END.  /* F1 */

   ELSE IF LOOKUP(KEYLABEL(LASTKEY),"3,F3") > 0 THEN DO TRANS: /* memo */
         FIND Payment WHERE RECID(Payment) = rtab[FRAME-LINE] 
         NO-LOCK NO-ERROR.         
         RUN memo(INPUT 0,
                  INPUT "payment",
                  INPUT STRING(Payment.Voucher),
                  INPUT "Voucher number").
         ufkey = TRUE.
         NEXT.
   END. /* F3 */

   /* eventlog */
   ELSE IF LOOKUP(KEYLABEL(LASTKEY),"7,F7") > 0 THEN DO:
   
      ufkey = TRUE.
            
      FIND Payment WHERE RECID(Payment) = rtab[FRAME-LINE] 
         NO-LOCK NO-ERROR.         
      IF AVAILABLE Payment THEN 
      RUN eventsel ("Payment",
                    Payment.Brand + CHR(255) +
                    STRING(Payment.CustNum) + CHR(255) +
                    STRING(Payment.InvNum)  + CHR(255) +
                    STRING(Payment.Voucher)).
   END. 

   if lookup(keylabel(lastkey),"8,F8") > 0 THEN LEAVE.
END.

END. /* WITH FRAME PaidAmt */
HIDE FRAME PaidAmt no-pause.

