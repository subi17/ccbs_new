/* refupaym.p      30.03.04/aam
   make payments for refunding customer balances 
   
   changes:        04.09.07/aam refund balance REF,
                                icCLI,
                                use createpaym.p
*/   

{commali.i}
{tmsparam2.i}
{fcustbal.i}


DEF INPUT  PARAMETER iiCustNum AS INT  NO-UNDO.
DEF INPUT  PARAMETER icCLI     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiInvNum  AS INT  NO-UNDO.
DEF INPUT  PARAMETER icBal     AS CHAR NO-UNDO.  /* overpaym,deposit,adv.paym*/
DEF INPUT  PARAMETER idAmt     AS DEC  NO-UNDO. 
DEF INPUT  PARAMETER idtDate   AS DATE NO-UNDO. 
DEF INPUT  PARAMETER iiAccNum  AS INT  NO-UNDO.  /* bank account */
DEF INPUT  PARAMETER icMemo    AS CHAR NO-UNDO. 
DEF OUTPUT PARAMETER oiVoucher AS INT  NO-UNDO. 

DEF VAR liCount    AS INT  NO-UNDO.
DEF VAR lcTitle    AS CHAR NO-UNDO. 
DEF VAR liVoucher  AS INT  NO-UNDO.
DEF VAR ldPosting  AS DEC  NO-UNDO EXTENT 10.
DEF VAR liAccount  AS DEC  NO-UNDO EXTENT 10.


IF idtDate = ? THEN idtDate = TODAY.

FIND Customer WHERE 
     Customer.Brand   = gcBrand AND 
     Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   RETURN "ERROR:Unknown customer".
END.

IF idAmt = 0 THEN DO:
   RETURN "ERROR:Amount not defined".
END.

IF LOOKUP(icBal,"OP,DP,AP,REF") = 0 THEN DO:
   RETURN "ERROR:Unknown balance type".
END. 

/* make sure that balance is adequate */
IF fGetCustBal(Customer.CustNum,icCLI,icBal) < idAmt THEN DO:
  RETURN "ERROR:Customer's balance is less than refund amount".
END. 
        
/* debit account */
CASE icBal:
WHEN "OP"  THEN liAccount[1] = fCParamI("OverPayAcc").
WHEN "DP"  THEN liAccount[1] = fCParamI("ResDepositsAcc").
WHEN "AP"  THEN liAccount[1] = fCParamI("AdvPaymAcc").
WHEN "REF" THEN liAccount[1] = fCParamI("RefundBalAcc").
END CASE. 

ASSIGN
   liAccount[2] = iiAccNum
   ldPosting[1] = idAmt
   ldPosting[2] = -1 * idAmt.
   
RUN createpaym (iiCustNum,
                iiInvNum,
                icCLI,
                idtDate,
                idtDate,
                ldPosting,
                liAccount,
                "RP",
                6,
                "",
                icMemo,
                OUTPUT oiVoucher).

IF oiVoucher > 0 
THEN RETURN "".
ELSE RETURN "ERROR:Payment could not be created; " + RETURN-VALUE.








