{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END.

def stream sread.
def stream slog.

input stream sread from 
   /home/ari/ddbank/check_bankaccount_change_custcare_110207.txt.

output stream slog to
   /apps/yoigo/tms_support/201102/aam_yts2703.log.
   
def var lcline as char no-undo.
def var licust as int no-undo.

def var lcnewbank as char no-undo.
def var ldabankdate as date no-undo.
def var ldaccdate as date no-undo.
def var i as int no-undo.
def var lcresult as char no-undo.
def var lccurrent as char no-undo.

put stream slog unformatted
   "Customer"   chr(9)
   "Current"    chr(9)
   "Changed To" chr(9)
   "Result"     skip.
   
repeat:

   import stream sread unformatted lcline.

   assign 
      licust = int(entry(1,lcline,chr(9)))
      ldabankdate = date(entry(4,lcline,chr(9)))
      lcnewbank = entry(5,lcline,chr(9))
      ldaccdate = date(entry(6,lcline,chr(9))) no-error.
      
   if error-status:error or licust = 0 then next.
   
   i = i + 1.

   find first customer where customer.custnum = licust no-lock.
   
   if lcnewbank = "" or ldabankdate > ldaccdate then lcresult = "Not updated".
   else if lcnewbank = customer.bankacc then lcresult = "Already same".
   else do:

      lccurrent = customer.bankacc.
      
      RUN StarEventSetOldBuffer(lhCustomer).
      find current customer exclusive-lock.
      customer.bankacc = lcnewbank.
      RUN StarEventMakeModifyEvent(lhCustomer).
      
      lcresult = "Updated".
   end.    
      
   /*
   disp i licust format ">>>>>>>>9"
        lcnewbank format "x(20)"
        ldabankdate format "99-99-99"
        ldaccdate format "99-99-99".
   */
   
   put stream slog unformatted
      customer.custnum  chr(9)
      lccurrent         chr(9)
      lcnewbank         chr(9)
      lcresult          skip.
      
end.

input stream sread close.

fcleaneventobjects().
