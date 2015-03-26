/* fcustbal.i       24.09.02/aam
   Update customer's various balances. These are all in one table, since these
   balances are most commonly updated in same application sections, one user
   at a time 

   changed:         10.10.02/jp  Function fUnbilledBalance
                    02.01.06/aam use SaldoCounter in fUnbilledBalance    
                    10.01.07/aam input CLI
*/

DEF VAR liLockCount AS INT NO-UNDO. 

/* update balances */
FUNCTION fCustBal RETURNS LOGICAL
   (iCustNum AS INT,
    icCLI    AS CHAR,
    iType    AS CHAR,
    iAmt     AS DEC). 

   IF iType = "" OR iAmt = 0
   THEN RETURN FALSE. 

   liLockCount = 0.

   /* do find each time this function is called -> extra finds, but should 
      remain in buffer when handling one invoice / payment */
   GetBal:
   DO WHILE TRUE:
      FIND CustBal EXCLUSIVE-LOCK WHERE
           CustBal.CustNum = iCustNum AND
           CustBal.CLI     = icCLI NO-ERROR NO-WAIT.

      IF LOCKED(CustBal) THEN DO:
         IF liLockCount > 500 THEN DO:
            /* to some errorlog ? */
            RETURN FALSE. 
         END.
         liLockCount = liLockCount + 1.
         PAUSE 3 NO-MESSAGE. 
      END.

      ELSE LEAVE GetBal.
   END.   

   IF NOT AVAILABLE CustBal THEN DO:
      CREATE CustBal.
      ASSIGN CustBal.CustNum = iCustNum 
             CustBal.CLI     = icCLI.
   END.

   CASE iType:
   WHEN "AP"    THEN CustBal.AdvPaym    = CustBal.AdvPaym  + iAmt. 
   WHEN "OP"    THEN CustBal.OverPaym   = CustBal.OverPaym + iAmt.
   WHEN "DP"    THEN CustBal.Deposit    = CustBal.Deposit  + iAmt.
   WHEN "REF"   THEN CustBal.Refund     = CustBal.Refund   + iAmt.
   WHEN "CL"    THEN CustBal.CreditLoss = CustBal.CreditLoss + iAmt.
   WHEN "ARBAL" THEN CustBal.Debt       = CustBal.Debt     + iAmt. 
   WHEN "INT"   THEN CustBal.Interest   = CustBal.Interest + iAmt.
   WHEN "INVP"  THEN CustBal.LatestInv  = INTEGER(iAmt). 
   END CASE. 

   RELEASE CustBal.

   RETURN TRUE. 

END FUNCTION.

FUNCTION fGetBalAmt RETURNS DECIMAL
   (icType AS CHAR):

   CASE icType:
   WHEN "AP"    THEN RETURN CustBal.AdvPaym. 
   WHEN "OP"    THEN RETURN CustBal.OverPaym.
   WHEN "DP"    THEN RETURN CustBal.Deposit.
   WHEN "REF"   THEN RETURN CustBal.Refund. 
   WHEN "CL"    THEN RETURN CustBal.CreditLoss.
   WHEN "ARBAL" THEN RETURN CustBal.Debt. 
   WHEN "INT"   THEN RETURN CustBal.Interest.
   WHEN "PBEH"  THEN RETURN DECIMAL(CustBal.PaymMethod).
   WHEN "PQTY"  THEN RETURN DECIMAL(CustBal.PaymQty). 
   WHEN "INVP"  THEN RETURN DECIMAL(CustBal.LatestInv). 
   OTHERWISE         RETURN 0.0. 
   END CASE. 
   
END FUNCTION.

/* get balances for display */
FUNCTION fGetCustBal RETURNS DECIMAL
   (iiCustNum AS INT,
    icCLI     AS CHAR,
    icType    AS CHAR). 

   DEF VAR ldCustBalAmt AS DEC NO-UNDO. 

   IF icType = ""  THEN RETURN 0.00. 

   ldCustBalAmt = 0.
   
   /* total per customer */
   IF icCLI = "total" THEN DO:
      FOR EACH CustBal NO-LOCK WHERE
               CustBal.CustNum = iiCustNum:
         ldCustBalAmt = ldCustBalAmt + fGetBalAmt(icType).
      END.         
   END.
   ELSE DO:
      IF NOT AVAILABLE CustBal OR
         CustBal.CustNum NE iiCustNum 
      THEN FIND CustBal NO-LOCK WHERE
                CustBal.CustNum = iiCustNum AND
                CustBal.CLI     = icCLI NO-ERROR.

      IF AVAILABLE CustBal THEN ldCustBalAmt = fGetBalAmt(icType).
   END.
   
   IF ldCustBalAmt = ? THEN ldCustBalAmt = 0. 

   RETURN ldCustBalAmt. 

END FUNCTION.

/* update latest invoice period */
FUNCTION fLatestInv RETURNS LOGICAL
   (iCustNum AS INT,
    icCLI    AS CHAR,
    iDate    AS DATE).

   IF iDate = ? THEN RETURN FALSE. 

   FIND CustBal EXCLUSIVE-LOCK WHERE
        CustBal.CustNum = iCustNum AND
        CustBal.CLI     = icCLI NO-ERROR.
   IF NOT AVAILABLE CustBal THEN DO:
      CREATE CustBal.
      ASSIGN CustBal.CustNum = iCustNum
             CustBal.CLI     = icCLI.
   END.

   CustBal.LatestInv = YEAR(iDate) * 100 + MONTH(iDate). 

   RELEASE CustBal.

   RETURN TRUE.                               

END.

/* update customer's payment behaviour */
FUNCTION fPaymBehaviour RETURNS LOGICAL
   (iCustNum  AS INT,
    icCLI     AS CHAR,
    iPaymDate AS DATE ,
    iDueDate  AS DATE). 

   IF iPaymDate = ? THEN RETURN FALSE. 

   FIND CustBal EXCLUSIVE-LOCK WHERE
        CustBal.CustNum = iCustNum AND
        CustBal.CLI     = icCLI NO-ERROR.

   IF NOT AVAILABLE CustBal THEN DO:
      CREATE CustBal.
      ASSIGN CustBal.CustNum = iCustNum
             CustBal.CLI     = icCLI.
   END.

   /* latest payment date */
   IF CustBal.LatestPaym = ? THEN CustBal.LatestPaym = iPaymDate.
   ELSE CustBal.LatestPaym = MAX(iPaymDate,CustBal.LatestPaym).

   /* update payment behaviour only if due date is given 
     (for old payments behaviour is not updated) */
   IF iDueDate NE ? THEN ASSIGN 
      CustBal.PaymMethod = (CustBal.PaymQty * CustBal.PaymMethod + 
                            (iPaymDate - iDueDate)) /
                           (CustBal.PaymQty + 1)
      CustBal.PaymQty    = CustBal.PaymQty + 1.

   RELEASE CustBal.

   RETURN TRUE. 

END FUNCTION.

FUNCTION fUnbilledBalance RETURNS DECIMAL
       (INPUT  iiMsSeq    AS INT,
        INPUT  iiPeriod   AS INT).

   DEF VAR ldebalance AS DEC NO-UNDO INIT 0.

   IF iiPeriod = 0 THEN iiPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).
   
   FOR EACH SaldoCounter NO-LOCK WHERE
            SaldoCounter.MsSeq   = iiMsSeq AND
            SaldoCounter.Period  = iiPeriod: 

      ldeBalance = ldeBalance + SaldoCounter.Amt.
   END.

   RETURN ldebalance.

END.        

