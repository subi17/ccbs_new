{Syst/testpaa.i}
{Func/fmakemsreq.i}

katun = "ari".

def var i       as int  no-undo.
def var j       as int  no-undo.
def var lcerror as char no-undo.
def var ldtdate as date no-undo.

def stream slog.
output stream slog to ../Work/yoi195_1.log append.

for each msowner no-lock where
         msowner.tsend >= 20070401 and
         msowner.tsend <= 20070516 and
         msowner.clitype begins "cont":
         
   if can-find(mobsub where mobsub.cli = msowner.cli) then next.
   
   for each msrequest where
            msrequest.msseq      = msowner.msseq and
            msrequest.reqtype    = 9             and
            msrequest.reqcparam3 = "term18"      and
            msrequest.reqstat    = 2             and
            msrequest.createfees = false:

      /*  
      disp msrequest.createfees 
           msrequest.actstamp
           msowner.custnum
           msowner.cli
           msowner.msseq.
      */
           

      fsplitts(msrequest.actstamp,
               output ldtdate,
               output i).
         
              
      RUN Mc/creasfee (MsOwner.CustNum,
                    MsRequest.MsSeq,
                    ldtdate ,
                    "MobSub",
                    "TERM_PERIOD",
                    1,
                    /* memo   */
                    msrequest.reqcparam3 + " terminated " + 
                       STRING(ldtDate,"99.99.9999"),
                    FALSE,          /* no messages to screen */
                    OUTPUT lcError).

      put stream slog unformatted 
         msrequest.msrequest chr(9) 
         lcerror skip.
               
      msrequest.createfees = true.
 
      j = j + 1.
      
      pause 0.
      disp j with 1 down.
   end.   
end.

