/* ===========================================================================
 MODULE ........: paymck.p
 APPLICATION ...:
 TASK ..........: This program calculates the real Balance of invoice
 CREATED .......: JP
 CHANGED .......:
 Version .......: M15
 ============================================================================*/

{commali.i}

DEF VAR i AS I NO-UNDO.

DEF INPUT  PARAMETER   Voucher   LIKE Payment.Voucher.
DEF OUTPUT PARAMETER   InvAmt LIKE Invoice.InvAmt.

DEF OUTPUT PARAMETER   PaidAmt     AS DE.
DEF OUTPUT PARAMETER   AccRec   AS DE. /* Outputs plus signed digit */
DEF OUTPUT PARAMETER   OthRec   AS DE. /* Outputs plus signed digit */

ASSIGN
   InvAmt  = 0
   PaidAmt = 0
   AccRec  = 0
   OthRec  = 0.

FIND Payment WHERE Payment.Voucher = Voucher NO-LOCK NO-ERROR.

IF AVAIL Payment THEN DO:
   FIND Invoice WHERE Invoice.InvNum = Payment.InvNum NO-LOCK NO-ERROR.

   FOR EACH Payment WHERE 
            Payment.InvNum   =  Invoice.InvNum AND 
            Payment.Voucher <= Voucher:
      /* Total Payments so far */
      PaidAmt = PaidAmt + Payment.PaymAmt. 
   END. /* FOR EACH Payment */

   FIND Payment WHERE Payment.Voucher = Voucher NO-LOCK NO-ERROR.   
   DO i = 1 TO 10:
      IF Payment.Posting[i] < 0 THEN DO:
         /* Payments which decreases AccNum receivables */
         IF Payment.AccType[i] =  1 /* AccNum receivable */
         THEN AccRec = AccRec + Payment.Posting[i].
         /* Other payments */
         IF Payment.AccType[i] NE 1 /* AccNum receivable */             
         THEN OthRec = OthRec + Payment.Posting[i].
      END. /* IF Posting < 0 */
   END.   

   /* Change minus TO plus */
     OthRec = OthRec - OthRec * 2.

   /* Total sum of invoice */
   InvAmt = Invoice.InvAmt.

END. /* IF AVAIL Invoice */


