{testpaa.i}
{fmakemsreq.i}

katun = "ari".

def var i       as int  no-undo.
def var j       as int  no-undo.
def var lcerror as char no-undo.

for each msowner no-lock where
         msowner.tsend >= 20070401 and
         msowner.tsend <= 20070516:
         
   if can-find(mobsub where mobsub.cli = msowner.cli) then next.
   
   for each dccli no-lock where
            dccli.msseq = msowner.msseq and
            dccli.termdate = ?:

      i = fPCActionRequest(DCCLI.MsSeq,  
                           DCCLI.DCEvent,
                           "term",
                           msowner.tsend,
                           (dccli.dcevent = "TERM18"),        /* create fees */
                           "",
                           "",
                          OUTPUT lcError).

      if lcerror > "" then do:
         message "created" i
              msowner.cli msowner.msseq msowner.custnum
              dccli.dcevent lcerror
         view-as alert-box.
      end.
      else j = j + 1.
      
      pause 0.
      disp j with 1 down.
         
      /*  
      CREATE memo.
      ASSIGN
         memo.Brand     = "1"
         memo.CreStamp  = ldCurrStamp
         memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.Custnum   = MobSub.CustNum
         memo.HostTable = "Customer"
         memo.KeyValue  = STRING(Mobsub.CustNum)
         memo.CreUser   = katun
         memo.MemoTitle = "Periodical Contract"
         Memo.memotext  = DCCLI.DCEvent + 
                          ": Terminated along with the subscription" +
                          (IF lcError > "" 
                           THEN ". Request failed: " + lcError
                           ELSE "").
      */      
   end.
   
end.   

