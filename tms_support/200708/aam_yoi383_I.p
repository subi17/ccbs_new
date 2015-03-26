def var i as int no-undo.

i = 1001103.

def stream slog.
output stream slog to /home/ari/ppid_move.log append.

def buffer brequest for prepaidrequest.

for each prepaidrequest exclusive-lock where
         brand = "1" and
         pprequest >= 1 /* 991100033 */ and
         pprequest <= 100115
         by pprequest:

         if tsrequest > 20070822 then do:
         

            find first payment where
                       payment.brand = "1" and
                       payment.refnum = string(pprequest) 
                       exclusive-lock no-error.

            disp pprequest format ">>>>>>>>9"  tsrequest ppstat
                 cli format "x(12)" request.
            
            repeat:    
               i = i + 1.
               if can-find(first brequest where
                                 brequest.brand = "1" and
                                 brequest.pprequest = i)
               then next.
               leave.
            end.

            put stream slog unformatted
               pprequest   chr(9)
               i           chr(9)
               cli         chr(9)
               recid(prepaidrequest) skip.

            prepaidrequest.pprequest = i.
            
            if available payment and payment.accdate = 8/22/7
            then payment.refnum = string(i).

        end.
 end.


