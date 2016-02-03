/*===========================================================================
 MODULE ........: intrumcr.p
 APPLICATION ...: TMS
 TASK ..........: read a File from Intrum Justitia WITH invoices which should
                  be posted as credit loss
 CREATED .......: 31.10.02/aam 
 CHANGED .......: 15.04.2003/aam makepaym returns voucher nbr
                  12.09.2003/aam brand
                  30.01.2004/aam CustNum to ClaimHist               
 VERSION .......: M15
 ============================================================================*/

{Syst/commali.i}          
{Func/cparam2.i}            
{Ar/intrumcr.i}

DEFINE OUTPUT PARAMETER TABLE FOR ttError.

DEF INPUT  PARAMETER icFile  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiRead  AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiFound AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiCLoss AS INT  NO-UNDO.

DEF VAR lcLine   AS CHAR NO-UNDO.
DEF VAR lcInvNum AS CHAR NO-UNDO.
DEF VAR liInvNum AS INT  NO-UNDO.
DEF VAR lcRef1   AS CHAR NO-UNDO.
DEF VAR lcOrg    AS CHAR NO-UNDO.
DEF VAR liCust   AS INT  NO-UNDO. 
DEF VAR lcName   AS CHAR NO-UNDO.
DEF VAR lcRef2   AS CHAR NO-UNDO.
DEF VAR liCancel AS INT  NO-UNDO.
DEF VAR ldAmount AS DEC  NO-UNDO.
DEF VAR liAccNum AS INT  NO-UNDO. 
DEF VAR liError  AS INT  NO-UNDO. 
DEF VAR ldBal    AS DEC  NO-UNDO.
DEF VAR liVoucher AS INT NO-UNDO. 

DEF STREAM sRead.

liAccNum = fCParamI("CreditLossAcc").

FIND Account WHERE 
     Account.Brand  = gcBrand AND
     Account.AccNum = liAccNum
NO-LOCK NO-ERROR.

IF NOT AVAILABLE Account OR 
   Account.AccType NE 18 
THEN DO:
   IF NOT SESSION:BATCH THEN 
      MESSAGE "Default account for credit loss is not properly defined."
      VIEW-AS ALERT-BOX
      ERROR.
   RETURN. 
END.

EMPTY TEMP-TABLE ttError.

INPUT STREAM sRead FROM VALUE(icFile) NO-ECHO.

REPEAT:

   IMPORT STREAM sRead UNFORMATTED lcLine.

   IF SUBSTRING(lcLine,1,2) NE "10" THEN NEXT.

   ASSIGN lcInvNum = SUBSTRING(lcLine,3,36)
          lcRef1   = TRIM(SUBSTRING(lcLine,39,16))
          lcOrg    = TRIM(SUBSTRING(lcLine,55,10))
          lcName   = TRIM(SUBSTRING(lcLine,65,37))
          lcRef2   = TRIM(SUBSTRING(lcLine,102,7))
          liCancel = INTEGER(SUBSTRING(lcLine,110,1))
          ldAmount = DECIMAL(SUBSTRING(lcLine,112,10)) / 100
          oiRead   = oiRead + 1
          liError  = 0. 

   IF lcInvNum BEGINS "LASKU " 
   THEN liInvNum = INTEGER(SUBSTRING(lcInvNum,7)) NO-ERROR.
   ELSE liInvNum = INTEGER(lcInvNum) NO-ERROR.


   IF liInvNum = 0 THEN DO:
      ASSIGN liCust = INTEGER(lcRef1) NO-ERROR.
      IF liCust NE 0 THEN DO:
         FIND Customer WHERE Customer.CustNum = liCust NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Customer OR 
             Customer.Brand NE gcBrand 
         THEN liCust = 0.
         ELSE IF NOT Customer.CustName BEGINS lcName THEN liCust = 0.
      END.

      IF liCust = 0 THEN 
      liCust = INTEGER(SUBSTRING(lcRef1,1,LENGTH(lcRef1) - 1)) NO-ERROR.

      IF liCust NE 0 THEN DO:
         FIND Customer WHERE Customer.CustNum = liCust NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Customer OR
            Customer.Brand NE gcBrand
         THEN liCust = 0.
         ELSE IF NOT Customer.CustName BEGINS lcName THEN liCust = 0.
      END.

      IF liCust > 0 AND AVAILABLE Customer THEN DO:

         FOR EACH Invoice OF Customer NO-LOCK WHERE
                  Invoice.InvAmt - Invoice.PaidAmt NE 0 AND
                  Invoice.ClaimBatch > 0:
            IF Invoice.InvAmt - Invoice.PaidAmt = ldAmount
            THEN liInvNum = Invoice.InvNum.
         END.
      END.
   END.

   IF liInvNum NE 0 THEN DO:
      FIND FIRST Invoice EXCLUSIVE-LOCK WHERE
                 Invoice.InvNum = liInvNum NO-ERROR.
      IF AVAILABLE Invoice             AND
         Invoice.Brand       = gcBrand AND
         Invoice.ClaimCancel = 0
      THEN DO:

         oiFound = oiFound + 1.

         RUN invbal(Invoice.InvNum, OUTPUT ldBal).

         IF ldBal <= 0 THEN DO:
            IF Invoice.CrInvNum > 0 
            THEN liError = 5.
            ELSE liError = 6.
         END. 
         ELSE IF ldBal < ldAmount THEN liError = 7.
         ELSE IF ldBal > ldAmount THEN liError = 8. 

         /* certain cancel codes are booked to credit loss */
         IF ldBal > 0 AND 
           ((liCancel >= 4 AND
             liCancel <= 7) OR
             liCancel = 9 
           )
         THEN DO:

            RUN makepaym (BUFFER Invoice,
                          MIN(ldAmount,ldBal),
                          TODAY,
                          liAccNum,
                          "IN",
                          1,
                          FALSE,
                          FALSE,
                          "",
                          "CreditLoss" +
                          " Intrum file (" + icFile +
                          "), Handler: " + katun,
                          OUTPUT liVoucher
                          ).

                   /* 3 = credit loss */
            ASSIGN Invoice.PaymState = 3
                   oiCLoss           = oiCLoss + 1.
         END.

         ELSE DO:
            IF liError = 0 THEN liError = 4. 
         END. 

         IF ldBal > 0 THEN DO:

            FIND FIRST ClaimHist WHERE
                       ClaimHist.InvNum = Invoice.InvNum AND
                       ClaimHist.Claim  = 9 EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE ClaimHist THEN DO:                    
               CREATE ClaimHist.
               ASSIGN ClaimHist.Brand     = Invoice.Brand
                      ClaimHist.InvNum    = Invoice.InvNum
                      ClaimHist.CustNum   = Invoice.CustNum
                      ClaimHist.Claim     = 9.
            END.

            ASSIGN ClaimHist.ClaimDate = TODAY
                   ClaimHist.Memo      = ClaimHist.Memo + 
                                         (IF ClaimHist.Memo NE ""
                                          THEN " "
                                          ELSE "") + 
                                         "Intrum file " + icFile
                   ClaimHist.Handler   = katun
                   ClaimHist.ClaimAmt  = ldAmount

                   Invoice.ClaimCancel = liCancel.

         END.

      END.

      ELSE DO:
         IF NOT AVAILABLE Invoice THEN liError = 2.
         ELSE IF Invoice.ClaimCancel > 0 THEN liError = 3. 
      END.

   END.

   ELSE liError = 1.

   IF liError > 0 THEN DO:
      CREATE ttError.
      ASSIGN ttError.ErrOrd   = oiRead
             ttError.ErrCode  = liError
             ttError.Inv      = lcInvNum
             ttError.Cust     = lcRef1
             ttError.Org      = lcOrg
             ttError.Name     = lcName
             ttError.IntrRef  = lcRef2
             ttError.ClCancel = liCancel
             ttError.Amount   = ldAmount.
   END.              

END.
