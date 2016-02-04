/* finvpayment.i      12.09.07/aam

   actions after payment to an invoice 
*/
{Syst/commali.i}
{Func/finvbal.i}
{Func/fpplan.i}
{Func/fpaymact.i}
{Func/fcustbal.i}

/* for calcint.p (interests) */
DEF NEW SHARED VAR intpro  AS DEC EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intdays AS INT EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intsumm AS DEC EXTENT 10 NO-UNDO.


FUNCTION fInvoicePaymentUpdate RETURNS LOGIC
   (BUFFER PaidInv FOR Invoice,
    idtPaymDate AS DATE,
    idPaymAmt   AS DEC,
    iiVoucher   AS INT,
    ilInterest  AS LOG,
    ilCustBal   AS LOG,
    ilBehaviour AS LOG):
   
   DEF VAR liIntMethod AS INT NO-UNDO.
   DEF VAR liCount    AS INT  NO-UNDO.
   DEF VAR ldBal      AS DEC  NO-UNDO. 
   DEF VAR ldIntMin   AS DEC  NO-UNDO.
   DEF VAR liIntMet   AS INT  NO-UNDO.
   DEF VAR lcIntDays  AS CHAR NO-UNDO. 
   DEF VAR lcIntPerc  AS CHAR NO-UNDO.
   DEF VAR ldIntAmt   AS DEC  NO-UNDO.

   FIND CURRENT PaidInv EXCLUSIVE-LOCK. 
   
   /* interest */
   IF ilInterest                    AND 
      PaidInv.InterestPerm          AND
      idtPaymDate - PaidInv.DueDate > 0 
   THEN DO:

      liIntMethod = fCParamI("IntCalcMet").
      IF liIntMethod = ? OR liIntMethod = 0 THEN liIntMethod = 1.

      RUN Ar/calcint (PaidInv.DueDate,
                   idtPaymDate,
                   liIntMethod,
                   idPaymAmt,
                   PaidInv.CustNum,
                   OUTPUT lcIntDays,
                   OUTPUT lcIntPerc,
                   OUTPUT ldIntAmt).

      IF ldIntAmt > fCParamDe("MinIntPay") THEN DO:

         CREATE CustIntEvent.
         ASSIGN CustIntEvent.Brand    = PaidInv.Brand
                CustIntEvent.Voucher  = iiVoucher
                CustIntEvent.InvNum   = PaidInv.InvNum
                CustIntEvent.CustNum  = PaidInv.CustNum
                CustIntEvent.InvAmt   = PaidInv.InvAmt
                CustIntEvent.InvDate  = PaidInv.InvDate
                CustIntEvent.DueDate  = PaidInv.DueDate
                CustIntEvent.PaymDate = idtPaymDate
                CustIntEvent.LateDays = idtPaymDate - PaidInv.DueDate
                CustIntEvent.Amt      = ldIntAmt
                CustIntEvent.PaidAmt  = idPaymAmt
                CustIntEvent.Percent  = DECIMAL(ENTRY(1,lcIntPerc)) / 100.

         /* Interest arrays */
         DO liCount = 1 TO 10:
            ASSIGN CustIntEvent.IntPerc[liCount] = intpro[liCount]
                   CustIntEvent.IntDays[liCount] = intdays[liCount]
                   CustIntEvent.IntAmt[liCount]  = intsumm[liCount].
         END.

         /* interest balance */
         IF ilCustBal THEN 
         fCustBal(PaidInv.CustNum,
                  PaidInv.CLI,
                  "INT",
                  CustIntEvent.Amt). 
      END.

   END.

   /* customer balance */
   IF ilCustBal THEN 
   fCustBal(PaidInv.CustNum,
            PaidInv.CLI,
            "ARBAL",
            -1 * idPaymAmt). 

   /* latest payment date and payment behaviour */
   fPaymBehaviour(PaidInv.CustNum,
                  PaidInv.CLI,
                  idtPaymDate,
                  IF ilBehaviour 
                  THEN PaidInv.DueDate
                  ELSE ?). 

   /* calculate new balance */
   ldBal = fInvBal(BUFFER PaidInv,
                   TODAY + 999).

   /* credit loss posted */
   IF fCredLossPaid(BUFFER PaidInv, 
                    TODAY + 999,
                    OUTPUT liCount) NE 0 
   THEN PaidInv.PaymState = 3.

   /* don't change status if involved in a payment plan */
   ELSE IF PaidInv.PaymState NE 4 THEN DO:
      IF ldBal = 0 THEN PaidInv.PaymState = 2.
      ELSE IF ldBal = PaidInv.InvAmt THEN PaidInv.PaymState = 0.
      ELSE PaidInv.PaymState = 1.
   END.   
                         
   ASSIGN                          
      PaidInv.PaymDate = idtPaymDate
      PaidInv.PaidAmt  = PaidInv.InvAmt - ldBal.

   /* update possible payment plan */
   fPaymPlanPaid(PaidInv.CustNum,
                 PaidInv.InvNum,
                 -1 * idPaymAmt).
 
   /* check if deposit invoice is created from order or from
      owner change request */
   IF PaidInv.InvType = 3 OR PaidInv.InvType = 4 THEN DO:
      fBalanceInvoicePaid(PaidInv.InvNum,
                          PaidInv.PaymState).
   END.                       

END FUNCTION.

