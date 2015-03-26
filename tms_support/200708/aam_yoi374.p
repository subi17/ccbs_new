def var ldend as dec no-undo.
def var liseq as int no-undo.
def var i     as int no-undo.

for each actionlog where
         ActionLog.Brand        = "1"       AND
         ActionLog.TableName    = "MobSub"  AND
         ActionLog.ActionID     = "MINCONS" and
         actionlog.actionchar   = "":
         
   ldend = 0.
   liseq = 0.
   
   find first mobsub where 
         mobsub.cli = actionlog.keyvalue no-lock no-error.
   if not available mobsub then do:
      find first msowner no-lock where msowner.cli = actionlog.keyvalue
          no-error.
      if available msowner then assign
         ldend = msowner.tsend
         liseq = msowner.msseq.
   end.
   else assign 
      ldend = 99999999
      liseq = mobsub.msseq.

   if liseq = 0 then do:
      message "chk:" actionlog.keyvalue actionlog.actionperiod
      view-as alert-box.
      next.
   end.
   
   /*
   disp actionlog.actionperiod liseq ldend format "99999999.99999".
   */
   
   assign 
      actionlog.actionchar = actionlog.keyvalue
      actionlog.keyvalue   = string(liseq).
   
   i = i + 1.
   if i mod 100 = 0 then do:
      pause 0.
      disp i actionlog.actionperiod with 1 down.
   end.
end.

disp i.

