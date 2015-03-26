def stream sread.
input stream sread from /apps/snet/200809/dextra_resend_080911_II.txt.

def var lcline  as char no-undo.
def var limsseq as int  no-undo.
def var lccontr as char no-undo.
def var lccli   as char no-undo.

def buffer bord for order.

repeat:

   import stream sread unformatted lcline.
   
   assign
      limsseq = integer(substring(lcline,1,8))
      lccontr = substring(lcline,20,5)
      lccli   = substring(lcline,90,12).
      
  find first order no-lock where 
             order.brand = "1" and
             order.orderid = limsseq.
             
  if lccontr ne order.contract or lccli ne order.cli then do:
      message "diff" order.orderid
      view-as alert-box.
      next.
  end.
      
  disp orderid 
       order.contract 
       order.cli format "x(12)"
       order.crstamp
       order.msseq 
       order.statuscode.

  /*
  find first sim where sim.icc = order.icc exclusive-lock.
  if sim.simstat = 21 then sim.simstat = 20.
  */
  
  find first sim where sim.icc = order.icc no-lock.
  disp sim.simstat.
end.
       
       
       
   
