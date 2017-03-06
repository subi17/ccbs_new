{Func/timestamp.i}

def var i       as int  no-undo.
def var j       as int  no-undo.
def var k       as int  no-undo.
def var ldtdate as date no-undo.
def var litime  as int  no-undo.

def stream slog.
output stream slog to /apps/snet/200710/aam_yoi450_II.log append.

def buffer breq for prepaidrequest.

for each prepaidrequest no-lock where 
         prepaidrequest.brand  = "1"       and
         prepaidrequest.source = "mincons" and
         prepaidrequest.tsrequest > 20071001:

    i = i + 1.

    if prepaidrequest.vatamt / prepaidrequest.topupamt < 0 then do:

       fsplitts(prepaidrequest.tsrequest,
                output ldtdate,
                output litime).

       put stream slog unformatted
          prepaidrequest.pprequest chr(9)
          prepaidrequest.cli       chr(9).
 
       if round(prepaidrequest.vatamt / 100,2) ne 0 then do:
                
          find first payment use-index refnum where
                     payment.brand   = "1"               and
                     payment.refnum  = string(pprequest) and
                     payment.accdate = ldtdate
          exclusive-lock no-error.

          if not available payment or 
             payment.posting[2] ne -1 * prepaidrequest.topupamt / 100
          then next.
       
          assign payment.posting[3] = round(prepaidrequest.vatamt / 100,2)
                 payment.posting[1] = -1 * (payment.posting[2] + 
                                            payment.posting[3]).
       
          put stream slog unformatted
             payment.voucher.
             
          k = k + 1.   
       end.
          
       put stream slog skip.
          
       find breq where recid(breq) = recid(prepaidrequest) exclusive-lock.
       breq.vatamt = -1 * breq.vatamt.
       
       j = j + 1.
    end.

    pause 0.
    disp i j k with 1 down.
end.
    
    
