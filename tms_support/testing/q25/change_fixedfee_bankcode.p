
{tmsconst.i}
{timestamp.i}

def var liFFNum as int no-undo.
def var lcTFBank as char no-undo.
def var ldeTotalAmount as dec no-undo.
def var ldaOrderDate as date no-undo.

assign liFFNum  = 70015691. 
        lcTFBank = "0081". /* "0049" - UNOE     */
                          /* "0081" - SABADELL */

FOR EACH FixedFee EXCLUSIVE-LOCK WHERE
         FixedFee.FFNum = liFFNum,
   FIRST OrderTimeStamp NO-LOCK WHERE
         OrderTimeStamp.Brand = "1" AND
         OrderTimeStamp.OrderId = FixedFee.OrderID AND
         OrderTimeStamp.RowType = {&ORDERTIMESTAMP_DELIVERY},
   FIRST Order NO-LOCK WHERE
         Order.Brand = "1" AND
         Order.OrderId = FixedFee.OrderID,
   FIRST OrderCustomer NO-LOCK WHERE
         OrderCustomer.Brand = "1" AND
         OrderCustomer.OrderId = Order.OrderId AND
         OrderCustomer.RowType = 1 BY OrderTimeStamp.TimeStamp:

   fTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).

   FIND FIRST FMItem NO-LOCK WHERE
              FMItem.Brand     = "1" AND
              FMItem.FeeModel  = FixedFee.FeeModel AND
              FMItem.ToDate   >= ldaOrderDate AND
              FMItem.FromDate <= ldaOrderDate NO-ERROR.

   if not available FMItem then do:
      message "Not Available FMItem" view-as alert-box.
      leave.
   end.

   ldeTotalAmount = ROUND(fmitem.FFItemQty * fmitem.Amount,2).

   ASSIGN FixedFee.TFBank         = lcTFBank
          FixedFee.FinancedResult = {&TF_STATUS_BANK}. /* "00" */

   FIND FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
              FixedFeeTF.FFNum = FixedFee.FFNum NO-ERROR.

   IF NOT AVAIL FixedFeeTF THEN DO:
      CREATE FixedFeeTF.
      ASSIGN
         FixedFeeTF.FFNum     = FixedFee.FFNum.
   END.

   ASSIGN
      FixedFeeTF.BankDate  = TODAY
      FixedFeeTF.TFBank    = lcTFBank
      FixedFeeTF.OrgId     = OrderCustomer.CustId
      FixedFeeTF.Amount    = ldeTotalAmount.

   FIND FIRST SingleFee EXCLUSIVE-LOCK WHERE
              SingleFee.Brand = "1" AND
              SingleFee.Custnum = FixedFee.Custnum AND
              SingleFee.HostTable = FixedFee.HostTable AND
              SingleFee.KeyValue = Fixedfee.KeyValue AND
              SingleFee.SourceKey = FixedFee.SourceKey AND
              SingleFee.SourceTable = FixedFee.SourceTable AND
              SingleFee.CalcObj = "RVTERM" NO-ERROR.

   if available SingleFee then do:
     
      if SingleFee.Amt > 0 then
         FixedFeeTF.ResidualAmount = SingleFee.Amt.

      if lcTFBank = "0049" then
         SingleFee.BillCode = "RVTERM1EF".
      else if lcTFBank = "0081" then
         SingleFee.BillCode = "RVTERMBSF".

   end.

   message "Done" view-as alert-box.
   leave.

end.

