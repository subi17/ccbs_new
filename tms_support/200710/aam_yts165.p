def stream sread.
input stream sread from /apps/snet/200710/incorrect_sim_status.txt.

/*
def stream slog.
output stream slog to /apps/snet/200710/aam_yts165.log.
*/

def var lcline  as char no-undo.
def var liorder as int  no-undo.
def var lccli   as char no-undo.
def var i       as int  no-undo.
def var lcsub   as char no-undo.
def var linew   as int  no-undo.

/*
put stream slog unformatted
   "Order"         chr(9)
   "MSISDN"        chr(9)
   "Order status"  chr(9)
   "Subs. status"  chr(9)
   "Sim status"    chr(9)
   "MSISDN status" skip.
*/
 
repeat:

   import stream sread unformatted lcline.
   
   assign 
      liorder = integer(entry(1,lcline,chr(9)))
      lccli   = entry(4,lcline,chr(9))
      no-error.

   if error-status:error then next.

   find order where
        order.brand   = "1" and
        order.orderid = liorder no-lock no-error.
   if not available order or order.cli ne lccli then next.

   find mobsub where mobsub.msseq = order.msseq no-lock no-error.
   
   if not available mobsub then lcsub = "terminated".
   else lcsub = string(mobsub.msstat).
   
   /*
   put stream slog unformatted
      order.orderid     chr(9)
      order.cli         chr(9)
      order.statuscode  chr(9)
      lcsub             chr(9).
   */
   
   find sim where
        sim.icc = order.icc no-lock no-error.
   /*
   if not available sim then put stream slog unformatted
      "n/a" chr(9).
   else put stream slog unformatted
      sim.simstat chr(9).
   */
    
   find first msisdn where
              msisdn.brand = "1" and
              msisdn.cli = order.cli no-lock no-error.
   /*
   if not available msisdn then put stream slog unformatted
      "n/a" skip.
   else put stream slog unformatted
      msisdn.statuscode skip.
   */
      
   if lcsub = "terminated" then linew = 9.
   else if sim.simstat = 14 then linew = sim.simstat.
   else linew = 4. 

   i = i + 1.
   
   disp i
        order.statuscode
        order.cli 
        sim.simstat column-label "sim" when available sim
        msisdn.statuscode column-label "msisdn" when available msisdn
        lcsub
        linew.

   if linew ne sim.simstat then do:
      find current sim exclusive-lock.
      sim.simstat = linew.
   end.
end.

 