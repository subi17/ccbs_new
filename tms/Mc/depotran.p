/* ---------------------------------------------------------------------------
  MODULE .......: DEPOTRAN
  FUNCTION .....: Transfer deposit from general customer to new subsc.customer
  APPLICATION ..: TMS
  CREATED ......: 26.01.04/aam 
  MODIFIED .....: 23.04.04/aam adv.payment
                  02.08.04/aam check if invoice has been credited
                  31.08.05/aam transfer also invoice and payment
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fcustbal.i}
{Func/finvbal.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER iiOrder AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER ocError AS CHAR NO-UNDO. 

DEF VAR lcDepoItem    AS CHAR NO-UNDO. 
DEF VAR lcAdvPaymItem AS CHAR NO-UNDO. 
DEF VAR liInvNum      AS INT  NO-UNDO. 
DEF VAR ldBal         AS DEC  NO-UNDO. 
DEF VAR ldAmount      AS DEC  NO-UNDO. 
DEF VAR liFee         AS INT  NO-UNDO. 
DEF VAR liCnt         AS INT  NO-UNDO. 
DEF VAR lcItem        AS CHAR NO-UNDO EXTENT 2. 
DEF VAR liInvType     AS INT  NO-UNDO. 
DEF VAR liAccType     AS INT  NO-UNDO. 
DEF VAR liOpFrom      AS INT  NO-UNDO.
DEF VAR liOpTo        AS INT  NO-UNDO. 
DEF VAR lcBalType     AS CHAR NO-UNDO. 
DEF VAR ldStamp       AS DEC  NO-UNDO.
DEF VAR liOldCust     AS INT  NO-UNDO.

DEF BUFFER bMemo FOR Memo.

FUNCTION fCreateOPLog RETURNS LOGICAL
   (iiCustNum AS INT,
    iiType    AS INT,
    idAmt     AS DEC).

    CREATE OPLog.
    ASSIGN
    OPLog.CustNum   = iiCustNum
    OPLog.EventDate = TODAY
    OPLog.UserCode  = katun
    OPLog.EventType = iiType      
    OPLog.InvNum    = Invoice.InvNum
    OPLog.Voucher   = 0
    OPLog.Amt       = idAmt
    OPLog.CreStamp  = ldStamp.

END FUNCTION.

FUNCTION fCopyMemos RETURNS LOGICAL
   (icHostTable AS CHAR,
    icKeyValue  AS CHAR).

   /* mark memos to subscriber customer */
   FOR EACH Memo EXCLUSIVE-LOCK WHERE
            Memo.Brand     = Invoice.Brand AND
            Memo.HostTable = icHostTable   AND
            Memo.KeyValue  = icKeyValue:
            
      Memo.CustNum = Invoice.CustNum.
   END.
    
END FUNCTION.


FIND Order NO-LOCK WHERE 
     Order.Brand   = gcBrand AND
     Order.OrderID = iiOrder NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   ocError = "Unknown order".
   RETURN.
END.

        
FIND Customer NO-LOCK WHERE 
     Customer.CustNum = Order.CustNum NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Unknown customer".
   RETURN.
END.

ASSIGN
/* billing item for fee */
lcDepoItem    = fCParamC("DepositItem")
lcAdvPaymItem = fCParamC("AdvPaymItem")
/* which invoice type is the default */
liInvType     = fCParamI("InvCreType").

liFee = ?.

IF liInvType = 4 THEN ASSIGN 
   lcItem[1] = lcAdvPaymItem
   lcItem[2] = lcDepoItem.
ELSE ASSIGN
   lcItem[1] = lcDepoItem
   lcItem[2] = lcAdvPaymItem.

DO liCnt = 1 TO 2:

   /* find deposit fee -> get invoice */
   FOR FIRST SingleFee NO-LOCK WHERE
             SingleFee.Brand     = gcBrand               AND
             SingleFee.HostTable = "Order"               AND
             SingleFee.KeyValue  = STRING(Order.OrderID) AND
             SingleFee.BillCode  = lcItem[liCnt]:

      IF SingleFee.MiscInt[5] = 1 THEN DO:
         ocError = "Already transferred".
         RETURN.
      END.

      ASSIGN liInvNum = SingleFee.InvNum
             liFee    = RECID(SingleFee)
             liCnt    = 2. 
   END.
    
END.

/* no fee done */
IF liFee = ? THEN RETURN "".

FIND Invoice WHERE 
     Invoice.InvNum = liInvNum NO-LOCK NO-ERROR.
IF liInvNum = 0 OR NOT AVAILABLE Invoice THEN DO:
   ocError = "Fee not billed".
   RETURN.
END.

/* invoice has been credited, not actually paid */
IF Invoice.CrInvNum > 0 THEN DO:
   ocError = "Invoice has been credited".
   RETURN.
END.

/* make sure that invoice is paid */
ldBal = fInvBal(BUFFER Invoice,
                TODAY).
                
IF ldBal NE 0 THEN DO:
   ocError = "Fee invoice not paid".
   RETURN.
END.

ldAmount = 0.

/* adv.payment */
IF Invoice.InvType = 4 
THEN ASSIGN liAccType = 19
            liOpFrom  = 20
            liOpTo    = 10
            lcBalType = "AP".
/* deposit */            
ELSE ASSIGN liAccType = 7
            liOpFrom  = 17
            liOpTo    = 13
            lcBalType = "DP".
   
DO TRANS:
           
   /* transfer to correct customer */                 
   FIND CURRENT Invoice EXCLUSIVE-LOCK.
   ASSIGN liOldCust          = Invoice.CustNum
          Invoice.CustNum    = Order.CustNum
          Invoice.CustName   = Customer.CustName
          Invoice.Address    = Customer.Address
          Invoice.PostOffice = Customer.ZipCode + " " + 
                               Customer.PostOffice.

   /* as deposit paid amount */
   FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:

      DO liCnt = 1 TO 10:
         IF Payment.AccType[liCnt] = liAccType 
         THEN ldAmount = ldAmount - Payment.Posting[liCnt].
      END.

      /* copy memos to subscriber customer */
      fCopyMemos("Payment",
                 STRING(Payment.Voucher)).

      Payment.CustNum = Invoice.CustNum.      
   
      RELEASE Payment.
   END.

   /* copy memos to subscriber customer */
   fCopyMemos("Invoice",
              STRING(Invoice.InvNum)).

   /* same stamp for both log events */
   ldStamp = fMakeTS().

   /* reduce deposit from general customer */
   fCustBal(liOldCust,
            Order.CLI,
            lcBalType,
           -1 * ldAmount). 

   fCreateOPLog(liOldCust,
                liOpFrom,
                -1 * ldAmount).
            
   /* add to subscriber customer */
   fCustBal(Order.CustNum,
            Order.CLI,
            lcBalType,
            ldAmount).
         
   fCreateOPLog(Order.CustNum,
                liOpTo,
                ldAmount).

   /* transfer fee to correct customer and mark it as transferred */
   FIND SingleFee WHERE RECID(SingleFee) = liFee EXCLUSIVE-LOCK.
   ASSIGN SingleFee.CustNum    = Order.CustNum 
          SingleFee.MiscInt[5] = 1.

   fCopyMemos("SingleFee",
              STRING(SingleFee.FMItemId)).

   RELEASE SingleFee.

   RELEASE Invoice.

END.

