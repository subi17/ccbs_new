input from as_yts2321_2.input.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.
def stream slog.
output stream slog to as_yts2321_2.log append.

def buffer bmnp for mnpprocess.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

{Func/timestamp.i}
LOOPPI:
repeat:
   import unformatted lcLine.
   
   i = i + 1.
   if i <= 2 then next. 

   do trans:

   find mnpprocess where
      mnpprocess.formrequest eq lcLine NO-LOCK.


   find first mnpoperation where
      mnpoperation.mnpseq = mnpprocess.mnpseq and
      mnpoperation.messagetype = "CrearSolicitudIndividualAltaPortabilidadMovil" and
      mnpoperation.errorcode = "AREC CUPO1" NO-LOCK no-error.

   IF NOT AVAIL mnpoperation then do:
       put stream slog unformatted  mnpprocess.formrequest "|" mnpprocess.statuscode "|WRONG ERROR" skip.
       NEXT LOOPPI.
   end.

   find order where
      order.brand eq "1" and
      order.orderid eq mnpprocess.orderid NO-LOCK.

   if order.mnpstatus ne 1 then do:

      if order.mnpstatus ne 2 then do:
       put stream slog unformatted  mnpprocess.formrequest "|" order.orderid "|" order.cli "|" order.statuscode "|" order.mnpstatus "|" mnpprocess.statuscode "|SKIPPED" skip.
         next LOOPPI.
      end.


      if mnpprocess.statuscode ne 0 then do:
         put stream slog unformatted mnpprocess.formrequest "|" order.orderid "|" order.cli "|" order.statuscode "|" order.mnpstatus "|" mnpprocess.statuscode "|WRONG ORDER STATUS" skip.
         next LOOPPI.
      end.

      
      find current mnpprocess EXCLUSIVE-LOCK.

      put stream slog unformatted  mnpprocess.formrequest "|" order.orderid "|" order.cli "|" order.statuscode "|" order.mnpstatus "|" mnpprocess.statuscode "|error handled" skip.
      FOR EACH mnpoperation where
         mnpoperation.mnpseq = mnpprocess.mnpseq and
         mnpoperation.messagetype = "CrearSolicitudIndividualAltaPortabilidadMovil" and
         mnpoperation.errorcode = "AREC CUPO1" and
         mnpoperation.errorhandled eq 1 EXCLUSIVE-LOCK:
         mnpoperation.errorhandled = 2.
      END. 
       assign
          mnpprocess.updatets = fmakets()
          mnpprocess.statuscode = 4
          mnpprocess.statusreason = "AREC CUPO1".

       release mnpprocess.
       NEXT LOOPPI.
   end.
   else do:
      
      if can-find(first bmnp where
                        bmnp.orderid = order.orderid and
                        recid(bmnp) ne recid(mnpprocess) NO-LOCK) or
         order.statuscode ne "12" or
         order.mnpstatus ne 1  or
         mnpprocess.statuscode ne 0 THEN DO:
       put stream slog unformatted  mnpprocess.formrequest "|" order.orderid "|" order.cli "|" order.statuscode "|" order.mnpstatus "|" mnpprocess.statuscode "|SKIPPED" skip.
        NEXT LOOPPI.
       END.

       put stream slog unformatted  mnpprocess.formrequest "|" order.orderid "|" order.cli "|" order.statuscode "|" order.mnpstatus "|" mnpprocess.statuscode "|OK" skip.
      
      find current order EXCLUSIVE-LOCK.
      find current mnpprocess EXCLUSIVE-LOCK.
        
      FOR EACH mnpoperation where
         mnpoperation.mnpseq = mnpprocess.mnpseq and
         mnpoperation.messagetype = "CrearSolicitudIndividualAltaPortabilidadMovil" and
         mnpoperation.errorcode = "AREC CUPO1" and
         mnpoperation.errorhandled eq 1 EXCLUSIVE-LOCK:
         mnpoperation.errorhandled = 2.
      END. 
      
      assign
          mnpprocess.updatets = fmakets()
          mnpprocess.statuscode = 4
          mnpprocess.statusreason = "AREC CUPO1"
          order.mnpstatus = 5
          order.statuscode = "3".
      
      release mnpprocess.
      release order.

   end.

   end.

end.
