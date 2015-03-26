/***************************************************************************
* TEMPORARY SOLUTION:  CREATE ONE BillTarg FOR CUSTOMER                    *
* IF NONE EXIST                                                            *
************************************************************************** 

changed:     17.09.02/aam RatePlan instead of PriceList on BillTarget
             10.10.02/jr  billtarg.address[] => changed
             04.03.03/jp  buffer billtarget
             17.03.03/jp  buffer billtarget removed, 
                          rateplan,discpaln from clitype
             08.05.03/tk  add billtarget 1 from tempcust

*/
define buffer tempbilltarg for billtarget.



FIND FIRST BillTarg where 
           BillTarg.CustNum    = customer.CustNum AND
           BillTarg.BillTarget = 1
no-error.

IF NOT AVAIL BillTarg THEN DO:

   FIND FIRST TempBillTarg where 
              TempBillTarg.CustNum    = CustTemp.CustNum AND
              TempBillTarg.BillTarget = 1
   no-error.
   IF AVAIL TempBillTarg THEN DO:
      CREATE BillTarg.
      ASSIGN
         BillTarg.CustNum    = Customer.CustNum
         BillTarg.BillTarget = TempBillTarg.BillTarget
         BillTarg.DiscPlan   = TempBillTarg.DiscPlan
         BillTarg.RatePlan   = TempBillTarg.RatePlan.
   END.
END.


FIND FIRST BillTarg where 
           BillTarg.CustNum    = Customer.CustNum AND
           BillTarg.BillTarget = clitype.billTarget 
no-error.

IF NOT AVAIL BillTarg THEN DO:
   CREATE BillTarg.
   ASSIGN
      BillTarg.CustNum    = Customer.CustNum
      BillTarg.BillTarget = CliType.BillTarget
      BillTarg.DiscPlan   = CliType.DiscPlan
      BillTarg.RatePlan   = CliType.PricePlan.

END.

