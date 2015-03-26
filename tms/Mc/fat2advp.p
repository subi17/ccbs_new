/* ---------------------------------------------------------------------------
  MODULE .......: FAT2ADVP
  FUNCTION .....: transfer fat to advance payment
  APPLICATION ..: TMS
  CREATED ......: 02.01.06/aam 
  MODIFIED .....: 
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{commali.i}

DEF INPUT  PARAMETER iiFatNum    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiVoucher   AS INT  NO-UNDO.


FIND FATime WHERE 
     FATime.Brand  = gcBrand AND
     FATime.FATNum = iiFatNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE FATime THEN RETURN "FATime not found".

/* if account for billing item is adv.payment related
   -> make an adv.payment out of unused amount */
FOR FIRST FatGroup OF FATime NO-LOCK,
    FIRST BillItem NO-LOCK WHERE
          BillItem.Brand    = gcBrand AND
          BillItem.BillCode = FatGroup.BillCode,
    FIRST Account NO-LOCK WHERE
          Account.Brand   = gcBrand         AND
          Account.AccNum  = BillItem.AccNum AND
          Account.AccType = 19:
              
   /* adv.payment is posted to invoicing customer */
   FIND Customer OF FATime NO-LOCK.
   
   RUN advpaym (Customer.InvCust,
                Fatime.Amt - Fatime.Used - Fatime.TransQty,
                TODAY,
                Account.AccNum,
                22,
                "From FATime; " + 
                   STRING(FATime.CustNum) + "/" +
                   FATime.CLI + "/" +
                   FATime.FtGrp + "/" + 
                   STRING(FATime.Period) + "/" +
                   TRIM(STRING(Fatime.Amt - Fatime.Used - Fatime.TransQty,
                               "->>>>>>9.99")),
                OUTPUT oiVoucher).                     
END.

