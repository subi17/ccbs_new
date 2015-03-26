def var llold as log no-undo.
def var i     as int no-undo.

def buffer bowner for msowner.

def stream slog.
output stream slog to /tmp/moved_subs_oldcalls.txt.

def stream sact.
output stream sact to /tmp/moved_subs_bdeny.txt.

put stream slog unformatted
   "ID"   chr(9)
   "MSISDN" chr(9)
   "New customer"  chr(9)
   "Old customer"  chr(9)
   "Old Customer Active" chr(9)
   "Old calls" skip.
    
put stream sact unformatted
   "ID"   chr(9)
   "MSISDN" chr(9)
   "New customer"  chr(9)
   "Old customer"  chr(9)
   "Old Customer Active" chr(9)
   "Deny Customer" chr(9)
   "Old calls" skip.
    
for each msowner no-lock where
         msowner.tsend = 20090101.00000 and
         msowner.paytype = false,
   first bowner no-lock where
         bowner.msseq = msowner.msseq and
         bowner.tsbegin = 20090101.00001 and
         bowner.agrcust ne msowner.agrcust,
   first customer no-lock where
         customer.custnum = msowner.agrcust:
    
    i = i + 1.
    pause 0.
    disp i with 1 down.

    llold = false.     
    for first invseq no-lock use-index msseq where
              invseq.msseq = msowner.msseq and
              invseq.billed = false and
              invseq.fromdate < 1/1/9:
       llold = true.       
    end.

    put stream slog unformatted
       msowner.msseq  chr(9)
       msowner.cli    chr(9)
       bowner.agrcust chr(9)
       msowner.agrcust chr(9)
       customer.roles chr(9)
       llold          skip.
       
    FIND FIRST ActionLog WHERE
               ActionLog.Brand        = "1"      AND
               ActionLog.TableName    = "MobSub"      AND
               ActionLog.KeyValue     = STRING(MsOwner.MsSeq) AND
               ActionLog.ActionID     = "DENYBILL"    AND
               ActionLog.FromDate    <= today      AND
               ActionLog.ToDate      >= today       AND
               ActionLog.ActionStatus = 0 NO-LOCK NO-ERROR.
 
    if available actionlog then put stream sact unformatted
        msowner.msseq chr(9)
        msowner.cli   chr(9)
        bowner.agrcust chr(9)
        msowner.agrcust chr(9)
        customer.roles chr(9)
        actionlog.custnum chr(9)
        llold skip.
end.         
