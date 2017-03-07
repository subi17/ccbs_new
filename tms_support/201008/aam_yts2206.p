{Syst/testpaa.i}
{Func/timestamp.i}


def var i as int no-undo.
def var j as int no-undo.

def buffer blimit for mservicelimit.

def stream slog.
output stream slog to 
   /apps/yoigo/tms_support/201008/aam_yts2206_august.log append.

for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstat = 2 and
         msrequest.actstamp >= 20100801 and
         msrequest.actstamp < 20100802 and
         msrequest.reqcparam1 begins "contrd" and
         msrequest.reqcparam2 begins "contrd":
         
   /*
   disp msrequest.reqcparam1 msrequest.reqcparam2
        fts2hms(msrequest.donestamp) format "x(19)".
   */

   i = i + 1.  
   
   for each mservicelimit no-lock where
            mservicelimit.msseq = msrequest.msseq and
            mservicelimit.fromts > 20100801,
      first servicelimit no-lock where
            servicelimit.slseq = mservicelimit.slseq and
            servicelimit.groupcode begins "contd":
      /*
      disp mservicelimit.fromts mservicelimit.endts servicelimit.groupcode.
      */
      j = j + 1. 
    
      put stream slog unformatted
         msrequest.msseq chr(9)
         msrequest.cli   chr(9)
         msrequest.reqcparam1 chr(9)
         msrequest.reqcparam2 chr(9)
         servicelimit.groupcode chr(9)
         mservicelimit.fromts skip.
         
      find first blimit where recid(blimit) = recid(mservicelimit)
         exclusive-lock.
      blimit.fromts = 20100801.   

      RUN Rate/cli_rate.p (msrequest.cli,
                      8/1/10,
                      8/31/10,
                      true).
   end.


   pause 0.
   disp i j with 1 down.
end.

output stream slog close.


