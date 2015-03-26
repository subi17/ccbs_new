def var lccli      as char no-undo.
def var lcline     as char no-undo.
def var liorder    as int  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.


def stream sread.
input stream sread from /apps/snet/200801/aam_yts459_dextra.log.

def temp-table ttlog no-undo
   field orderid as int
   index orderid orderid.
   
repeat:

   import stream sread unformatted lcline.
   
   assign
      liorder = integer(entry(3,lcline,chr(9))) 
      lccli   = entry(1,lcline,chr(9))
      no-error.

   if error-status:error then next.
   
   find order where 
        order.brand   = "1" and
        order.orderid = liorder no-lock no-error.
   if not available order or order.cli ne lccli then next.
   
   find sim where sim.icc = order.icc no-lock no-error.
   if not available sim or sim.simstat ne 21 then next.
   
   i = i + 1.
   
   display i
           order.orderid
           order.cli
           order.invnum 
           order.statuscode.

   find first ttlog where ttlog.orderid = order.orderid no-error.
   if not available ttlog then do:
      create ttlog.
      ttlog.orderid = order.orderid.
      j = j + 1.
   end.
   
   /* 
   find current sim exclusive-lock.
   sim.simstat = 20.
   */
end.

disp i j .

