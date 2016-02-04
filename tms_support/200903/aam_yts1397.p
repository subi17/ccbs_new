{Syst/testpaa.i}
katun = "ari".


def stream sread.
input stream sread from /apps/snet/200903/mnp_onlysim_initial_topup.txt.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var liorder as int  no-undo.
def var lireq   as int  no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.

repeat:
   import stream sread unformatted lcline.

   assign lccli = entry(1,lcline,chr(9))
          liorder = integer(entry(2,lcline,chr(9)))
          no-error.
   if error-status:error then next.
                               
   find first order where 
         order.brand = "1" and
         order.orderid = liorder no-lock no-error.
   if not available order or order.cli ne lccli then do:
       message "check:" lccli liorder
       view-as alert-box.
       next.
   end.

   i = i + 1.

   if can-find(first prepaidrequest use-index msseq where
                     prepaidrequest.brand = "1" and
                     prepaidrequest.msseq = order.msseq and
                     prepaidrequest.source = "web order")
   then next.
   
   j = j + 1.
   disp i j.
   /*
   RUN Mm/topupcamp.p (order.msseq,
                    output lireq).
   */                                
end.

disp i j.

