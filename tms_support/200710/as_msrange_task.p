DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DO i = 622440001 TO 622517000:
FIND FIRST msisdn where cli = string(i) EXCLUSIVE-LOCK no-error.
assign
   msisdn.statuscode = 1
   msisdn.POS = "PREACTIVATED"
   msisdn.custnum = 1001.
END.

create msrange.
assign
   msrange.CliFrom = "622440001"
   msrange.CliTo   = "622517000"
   msrange.custnum = 1001
   msrange.salesman = "PREACTIVATED"
   msrange.reservedate = TODAY
   msrange.expiredate  = 1/1/2052
   msrange.brand = "1".
