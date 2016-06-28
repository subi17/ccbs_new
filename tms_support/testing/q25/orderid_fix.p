{Func/date.i}

DEF VAR lcMSISDN AS CHAR NO-UNDO.
DEF VAR ldeFF AS DEC NO-UNDO.

lcMSISDN = "722509721".

find first MobSub no-lock where
           MobSub.CLI = lcMSISDN no-error.
if not available MobSub then return.

FOR EACH SingleFee exclusive-lock where
         SingleFee.Brand     = "1" AND
         SingleFee.Custnum   = MobSub.CustNum AND
         SingleFee.HostTable = "Mobsub" AND
         SingleFee.KeyValue  = STRING(MobSub.MsSeq) AND
         SingleFee.OrderId   = -1 AND
         SingleFee.CalcObj   BEGINS "RVTERM" 
         by SingleFee.billperiod desc:
   leave.
end.
   
if not avail SingleFee then do:
   message "No SingleFee" view-as alert-box.
   return.
end.

find first FixedFee exclusive-lock where
           FixedFee.Brand       = "1" AND
           FixedFee.Custnum     = SingleFee.Custnum AND
           FixedFee.HostTable   = SingleFee.hosttable AND
           FixedFee.KeyValue    = SingleFee.keyvalue AND
           FixedFee.OrderId     = -1 AND
           fixedfee.sourcetable = SingleFee.sourcetable and
           fixedfee.sourcekey   = SingleFee.sourcekey and
           FixedFee.BillCode    = "PAYTERM" NO-ERROR.
if not avail Fixedfee then do:
   message "No FixedFee" view-as alert-box.
   return.
end.

ldeFF = fmake2dt(fixedfee.begdate + 1,0).

for each Order no-lock where
         Order.MsSeq = MobSub.MsSeq and
         order.statuscode = "6" and
         order.crstamp < ldeFF by crstamp desc:
   leave.
end.

if not avail order then do:
   message "No Order" view-as alert-box.
   return.
end.

FIND FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
           FixedFeeTF.FFNum = FixedFee.FFNum NO-ERROR.

ASSIGN SingleFee.OrderId  = Order.OrderId
       FixedFee.OrderId   = Order.OrderId
       FixedFeeTF.OrderId = Order.OrderId WHEN AVAIL FixedFeeTF.

message "Single Fee: " SingleFee.OrderId  skip
        "Fixed Fee:  " FixedFee.OrderId   skip
        "Fixed Fee TF:  " FixedFeeTF.OrderId
        view-as alert-box.

release SingleFee.
release FixedFee.
