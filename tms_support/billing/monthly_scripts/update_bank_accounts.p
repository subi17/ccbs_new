/* Monthly script to update the bank accounts for list of customers.
   Customer data must be in this order and delimited with ;
   CustNbr;CurrentBankAccount;BankAccountLastOrder
   Change input and output filenames according to ticket.
   Report error cases that were not changed after the run */

{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {/apps/tms/Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END.

def stream sread.
def stream slog.

input stream sread from "logs/yot_4252.csv".
output stream slog to "logs/yot_4252.log".
   
def var lcline as char no-undo.
def var licust as int no-undo.

def var lcnewbank as char no-undo.
def var i as int no-undo.
def var lcresult as char no-undo.
def var lcoldbank as char no-undo.
def var lccurrent as char no-undo.

put stream slog unformatted
"CustNbr;CurrentBankAccount;BankAccountLastOrder;Result" skip.
   
repeat trans:

   import stream sread unformatted lcline.

   if lcline = "" OR lcLine begins "CustNbr" then next.

   assign 
      licust = int(entry(1,lcline,";"))
      lcoldbank = entry(2,lcline,";").
      lcnewbank = entry(3,lcline,";") no-error.
      
   if error-status:error or licust = 0 then do:
      put stream slog unformatted
         lcLine ";Parsing error" skip.
      next.
   end.
   i = i + 1.
   disp i with frame a.
   pause 0.

   find first customer where customer.custnum = licust EXCLUSIVE-LOCK no-error.
   if not avail customer then do:
      put stream slog unformatted
      lcLine ";" "customer not found" skip.
      next.
   end.
   if lcnewbank = "" then lcresult = "New Bank Account is missing".
   else if lcnewbank = customer.bankacc then lcresult = "Already same".
   else if customer.bankacc ne lcoldbank then lcresult = "CurrentBankAccount does not match with real current account " + 
      customer.bankacc.
   else if length(lcnewbank) ne 24 then lcresult = "Incorrect Bank Account length".
   else do:

      lccurrent = customer.bankacc.
      
      RUN StarEventSetOldBuffer(lhCustomer).
      customer.bankacc = lcnewbank.
      RUN StarEventMakeModifyEvent(lhCustomer).

      /* Write memo */
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "customer",
                 STRING(Customer.CustNum),
                 Customer.CustNum,
                 "Cuenta EVO caducada",
                 "Cuenta EVO caducada. Actualización masiva comunicada por EVO: " + lcoldbank + " -> " + lcnewbank ).

      release customer.
      lcresult = "Updated".
   end.    
   
   put stream slog unformatted
      lcLine ";"
      lcresult          skip.
      
end.

input stream sread close.
OUTPUT STREAM slog CLOSE.

fcleaneventobjects().
