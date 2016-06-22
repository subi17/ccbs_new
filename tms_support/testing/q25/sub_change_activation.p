
{timestamp.i}

def var lcMSISDN         as char no-undo.
def var liCount          as int  no-undo.
def var ldActivatedTS    as dec no-undo.
def var ldtActivatedDate as date no-undo.

assign lcMSISDN      = "777777777"
       ldActivatedTS = 20140101
       liCount       = 0.

fTS2Date(ldActivatedTS,
         OUTPUT ldtActivatedDate).

find first MobSub exclusive-lock where
           MobSub.Brand = "1" and
           MobSub.CLI = lcMSISDN no-error.

if available MobSub then do:
   for each MsOwner no-lock where
            MsOwner.Brand = "1" and
            MsOwner.CLI = MobSub.CLI:
      liCount = liCount + 1.
   end.

   if liCount = 1 then 
      assign MobSub.ActivationTS   = ldActivatedTS
             MobSub.ActivationDate = ldtActivatedDate.
   else message "More than one MsOwner. The activation date will not be changed" view-as alert-box.

   release MobSub.

end.
else message "Unknown Subscription" view-as alert-box.

