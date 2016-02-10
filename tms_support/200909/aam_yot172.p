{Syst/testpaa.i}
katun = "ari".

{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhLimit AS HANDLE NO-UNDO.
   lhLimit = BUFFER Limit:HANDLE.
   RUN StarEventInitialize(lhLimit).
END.


def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def var lcline as char no-undo.
def var licust as int  no-undo.
def var lidone as int no-undo.
def var lilimit as int no-undo.
def var llupd as log no-undo.

def temp-table ttdone no-undo
   field custnum as int
   index custnum custnum.

def temp-table ttcat no-undo
   field category as char
   field sublimit as int
   index category category.
   
   
def stream sread.
input stream sread from /apps/snet/200909/customer_mobsub_limit_yot172.txt.

def stream slog.
output stream slog to /apps/snet/200909/aam_yot172.log append.

for each custcat no-lock where 
         custcat.brand = "1":
         
   create ttcat.
   assign 
      ttcat.category = custcat.category
      ttcat.sublimit = custcat.mobsublimit.
end.         

put stream slog unformatted
   "Customer" ";"
   "Required Limit" ";"
   "Updated" skip.

repeat:
   import stream sread unformatted lcline.
   
   licust = integer(lcline) no-error.
     
   if error-status:error then next.
   

   if licust = 0 then next.
   
   i = i + 1. 
   
   find first customer where customer.custnum = licust
        no-lock no-error.
   if not available customer then do:
      put stream slog unformatted
         lcline ";"
         "Invalid customer nbr" 
         skip.
      next.   
   end.

   if can-find(first ttdone where ttdone.custnum = customer.custnum)
   then next.
   
   create ttdone.
   ttdone.custnum = customer.custnum.
   
   lilimit = 0.
   for each mobsub no-lock where
            mobsub.brand = "1" and
            mobsub.agrcust = customer.custnum:
      lilimit = lilimit + 1.      
   end.
   
   lilimit = max(5,lilimit + 1).

   llupd = false.
   
   find first limit no-lock where
              limit.custnum = customer.custnum and
              limit.limittype = 2 and
              limit.todate >= today and
              limit.fromdate <= today no-error.
   if available limit then do:
      
      if lilimit > limit.limitamt then do:

         find current limit exclusive-lock.
      
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).
         limit.limitamt = lilimit.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhLimit).

         llupd = true.
         j = j + 1.
      end.
   end.                                  

   else do:
      
      find first ttcat where ttcat.category = customer.category no-error.
      if not available ttcat or ttcat.sublimit < lilimit then do:

         CREATE Limit.
         ASSIGN
            Limit.CustNum   = licust
            Limit.LimitType = 2
            Limit.LimitId   = 0
            Limit.ValueType = 1
            Limit.FromDate  = TODAY
            Limit.ToDate    = 12/31/2049
            Limit.DefValue  = FALSE
            Limit.LimitAmt  = lilimit.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhLimit).

         llupd = true.
         k = k + 1.
      end.
   end.

   put stream slog unformatted
      customer.custnum ";"
      lilimit ";" 
      llupd skip.
      
   lidone = lidone + 1.

   if i mod 1000 = 0 then do:
      pause 0.
      disp i j k lidone with 1 down.
   end.
end.

input stream sread close.
output stream slog close.

fcleaneventobjects().

disp i j k lidone .


