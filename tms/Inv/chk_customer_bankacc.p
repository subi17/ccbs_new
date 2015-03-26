{fbankdata.i}

define input parameter icOutputFile as char.
DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
   "RUNNING chk_customer_bankacc.p" skip(1).

put stream sout unformatted 
"DESCRIPTION: Postpaid customers with charge type = 'direct debit'" skip 
"and no valid bank account" skip(1). 

FOR EACH Customer WHERE
   Customer.ChargeType = 2 NO-LOCK:
      
      FIND FIRST Mobsub WHERE 
         Mobsub.CustNum = Customer.CustNum AND
         Mobsub.PayType = false NO-LOCK NO-ERROR.
      
      IF AVAIL Mobsub THEN DO: 
      IF LENGTH(Customer.BankAcc) < 24 or
        NOT fCheckBankAcc(Customer.BankAcc) THEN DO:
        if ok then do:
         ok = false.
         put stream sout 
            " Customer Bank Account" skip
            " -------- ------------" skip.
        end.
        put stream sout
             customer.custnum " "
             customer.bankacct format "x(24)" skip.
      END.
      END.

END.

if ok then put stream sout unformatted "OK" skip(1).
else put stream sout unformatted skip(1).
output stream sout close.

