{Func/timestamp.i}

def stream sread.
input stream sread from /apps/snet/200810/deny_billing_081028.txt.

def var lcline    as char no-undo.
def var lccli     as char no-undo.
def var limsseq   as int  no-undo.
def var liagrcust as int  no-undo.
def var i         as int  no-undo.
def var j         as int  no-undo.
def var ldcurrent as dec  no-undo.
def var licode    as int  no-undo.

ldcurrent = fmakets().

repeat:

   import stream sread unformatted lcline.

   limsseq = 0.
   
   assign 
      lccli     = entry(1,lcline,chr(9))
      limsseq   = integer(entry(9,lcline,chr(9)))
      liagrcust = integer(entry(6,lcline,chr(9)))
      licode    = 0
      no-error.

   if error-status:error then next. 

   if num-entries(lcline,chr(9)) > 9 then 
      licode = integer(entry(10,lcline,chr(9))) no-error.
      
   i = i + 1.
   
   find first msowner where
              msowner.msseq = limsseq and
              msowner.cli   = lccli   and
              msowner.agrcust = liagrcust no-lock no-error.
   if not available msowner then do:
      j = j + 1.
      next.
   end.
   
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = "1"   
      ActionLog.TableName    = "MobSub"  
      ActionLog.KeyValue     = string(limsseq)
      ActionLog.ActionChar   = lccli
      ActionLog.CustNum      = MsOwner.InvCust
      ActionLog.ActionID     = "DENYBILL"
      ActionLog.FromDate     = 11/1/8
      ActionLog.ToDate       = 12/31/2049
      ActionLog.ActionPeriod = 200811
      ActionLog.ActionDec    = licode 
      ActionLog.ActionTS     = ldCurrent
      ActionLog.ActionStatus = 0
      ActionLog.UserCode     = "ari".

   if i mod 100 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
end.

input stream sread close.

disp i j.

