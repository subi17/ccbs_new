/* finvbal.i 

   changed:    20.05.03/aam from invbal.p
               22.10.03/aam fCalcInvBal
               23.04.04/aam InvType 4
               16.06.04/aam use payment state 5 for epayment
               23.09.04/aam fCredLossPaid
               23.06.05/aam round total sum to 2 decimals

*/

{commali.i}
{cparam2.i}

&IF "{&InvBalDefined}" NE "YES" 
&THEN

&GLOBAL-DEFINE InvBalDefined YES


DEF VAR liBalCnt     AS INT NO-UNDO.
DEF VAR ldBalAmt     AS DEC NO-UNDO. 
DEF VAR liEPaymValid AS INT NO-UNDO. 
DEF VAR liType       AS INT NO-UNDO. 

DEF BUFFER bCalcPaym FOR Payment.

/* cparam2.i is needed in the calling program */
liEPaymValid = fCParamI("ePaymValid").
IF liEPaymValid = ? THEN liEPaymValid = 0. 

FUNCTION fCalcInvBal RETURNS DECIMAL
   (BUFFER ibBalInv FOR Invoice,
    idtDate  AS DATE,
    ilePaym  AS LOG).

   ldBalAmt = ibBalInv.InvAmt.
   
   /* invoice type determines which account is to be used */
   CASE ibBalInv.InvType:
   WHEN 3 THEN liType = 7.    /* deposit */
   WHEN 4 THEN liType = 19.   /* adv.payment */
   OTHERWISE   liType = 1.
   END CASE. 

   
   IF ibBalInv.InvNum NE 0 THEN
   FOR EACH bCalcPaym OF ibBalInv NO-LOCK WHERE
            bCalcPaym.AccDate <= idtDate:

      DO liBalCnt = 1 TO 10:

         IF bCalcPaym.AccType[liBalCnt] = liType
         THEN ldBalAmt = ldBalAmt + bCalcPaym.Posting[liBalCnt].

      END.

   END.

   /* check ePayment if actual payment has not yet arrived */
   IF ilePaym AND ibBalInv.PaymState = 5 THEN DO:

      /* is ePayment recent enough */
      IF ibBalInv.EPaymDate NE ?       AND 
         ibBalInv.EPaymDate <= idtDate AND
         idtDate - ibBalInv.EPaymDate <= liEPaymValid
      THEN ldBalAmt = ldBalAmt - ibBalInv.EPaymAmt.
      
   END.


   RETURN ldBalAmt.

END FUNCTION.

FUNCTION fInvBal RETURNS DECIMAL
   (BUFFER ibBalInv FOR Invoice,
    idtDate  AS DATE).

   RETURN fCalcInvBal(BUFFER ibBalInv,
                      idtDate,
                      TRUE).

END.

FUNCTION fCredLossPaid RETURNS DECIMAL
   (BUFFER ibBalInv FOR Invoice,
    idtDate          AS DATE,
    OUTPUT oiLossAcc AS INT).

   DEF VAR ldCLVatAmt AS DEC NO-UNDO.
   DEF VAR llCLFound  AS LOG NO-UNDO.
   
   ASSIGN ldBalAmt  = 0
          oiLossAcc = 0.

   /* has credit loss been posted */
   IF ibBalInv.InvNum NE 0 THEN
   FOR EACH bCalcPaym OF ibBalInv NO-LOCK WHERE
            bCalcPaym.AccDate <= idtDate:

      ASSIGN ldCLVatAmt = 0
             llCLFound  = FALSE.

      DO liBalCnt = 1 TO 10:
      
         IF bCalcPaym.AccType[liBalCnt] = 18 THEN ASSIGN 
            ldBalAmt = ldBalAmt + bCalcPaym.Posting[liBalCnt]
            /* assume that one account is used */
            oiLossAcc  = bCalcPaym.AccNum[liBalCnt]
            llCLFound = TRUE.

         ELSE IF bCalcPaym.AccType[liBalCnt] = 5 THEN 
            ldCLVatAmt = ldCLVatAmt + bCalcPaym.Posting[liBalCnt].
      END.

      /* add possible vat */
      IF llCLFound THEN ldBalAmt = ldBalAmt + ldCLVatAmt.

   END.
   
   RETURN ldBalAmt.

END FUNCTION.

FUNCTION fCreditedAmt RETURNS DECIMAL
   (BUFFER ibBalInv FOR Invoice,
    idtDate          AS DATE):

   ldBalAmt = 0.

   IF ibBalInv.InvNum NE 0 THEN
   FOR EACH bCalcPaym OF ibBalInv NO-LOCK WHERE
            bCalcPaym.AccDate <= idtDate AND
            bCalcPaym.PaymSrc  = "CR":

      DO liBalCnt = 1 TO 10:
         IF bCalcPaym.AccType[liBalCnt] = 1 THEN 
            ldBalAmt = ldBalAmt + bCalcPaym.Posting[liBalCnt].
      END.

   END.
   
   RETURN -1 * ldBalAmt.

END FUNCTION.


&ENDIF


