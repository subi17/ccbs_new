{Syst/testpaa.i}
katun = "ari".
{Func/timestamp.i}

def var lireq as int no-undo.

def var i as int no-undo.

def stream slog.
output stream slog to /apps/snet/200708/aam_yoi383.log append.

for each order no-lock where
         order.brand = "1" and
         order.crstamp > 20070822 and
         order.crstamp < 20070823 and
         order.paytype = true,
   first mobsub no-lock where
         mobsub.msseq = order.msseq:

   disp i order.orderid
        fts2hms(order.crstamp) format "x(19)"
        order.statuscode.
        
   find first prepaidrequest no-lock use-index cli where
              prepaidrequest.brand  = "1" and
              prepaidrequest.cli    = order.cli and
              prepaidrequest.source = "WEB Order" no-error.
   if available prepaidrequest then
      disp prepaidrequest.pprequest format ">>>>>>>>9".
   else do:
                     
      RUN Mm/topupcamp(MobSub.MsSeq, 
                    OUTPUT lireq).
 
      put stream slog unformatted 
         order.orderid  chr(9)
         order.cli      chr(9)
         order.msseq    chr(9)
         lireq          skip.
 
      i = i + 1.    
      disp lireq format ">>>>>>>>9".
   end.

end.

