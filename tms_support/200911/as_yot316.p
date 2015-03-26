input from /apps/snet/200909/rd_yot_210.input.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200911/as_yot316.output.

looppi:
repeat:
   import unformatted lcline.
   FOR EACH msisdn EXCLUSIVE-LOCK where
      msisdn.cli = lcline:
      if msisdn.statuscode = 0 and 
         msisdn.validto = 99999999.99999 then do:
         export stream slog msisdn.
         delete msisdn. 
         next looppi.
      end.
   end.
end.
