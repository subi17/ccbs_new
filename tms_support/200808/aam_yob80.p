def stream slog.
output stream slog to /apps/snet/200808/aam_yob80.log append.

def var lldel as log no-undo.

for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = 0 and
         msrequest.reqstat = 2 and
         msrequest.actstamp > 20080100 and
         msrequest.reqcparam1 = "cont2",
    each fixedfee no-lock where
         fixedfee.brand = "1" and
         fixedfee.hosttable = "mobsub" and
         fixedfee.keyvalue = string(msrequest.msseq),
    each ffitem of fixedfee exclusive-lock where
         ffitem.billed = false:

   lldel = (ffitem.billper >= truncate(msrequest.donestamp / 100,0)).

   disp msrequest.cli
        msrequest.reqcparam2 format "x(15)"
        ffitem.billper 
        ffitem.amt
        msrequest.donestamp
        lldel.
        
   if lldel then do:
      export stream slog ffitem.
      delete ffitem.
   end.
end.

output stream slog close.
