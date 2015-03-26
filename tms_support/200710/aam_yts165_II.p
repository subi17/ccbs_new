def stream sread.
input stream sread from /apps/snet/200710/incorrect_sim_status.txt.

def var lcline  as char no-undo.
def var liorder as int  no-undo.
def var lccli   as char no-undo.
def var i       as int  no-undo.
def var lcsub   as char no-undo.
def var listat  as int  no-undo.

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
   
   if not available mobsub then next.
   
   find sim where
        sim.icc = mobsub.icc no-lock no-error.
   if not available sim then next. 
    
   if sim.simstat ne 13 then next.
   
   find first msisdn where
              msisdn.brand = "1" and
              msisdn.cli = order.cli no-lock no-error.
    
   i = i + 1.

   listat = 0.
   for each msrequest no-lock where
            msrequest.msseq = mobsub.msseq and
            msrequest.reqtype = 15:
            
      if MSRequest.ReqCParam2 = mobsub.icc and 
         msrequest.reqstat = 2 
      then listat = 2.
   end.

   disp i
        order.statuscode
        order.cli 
        sim.simstat column-label "sim" when available sim
        msisdn.statuscode column-label "msisdn" when available msisdn
        lcsub
        listat.

   if listat = 2 then do:
      find current sim exclusive-lock.
      sim.simstat = 4.
   end.
   
end.

 