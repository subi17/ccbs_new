{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msisdn.i}

/* MSISDN STOCK */

def stream slog.
output stream slog to /apps/snet/200811/as_ycm1037_misdn.log.

FUNCTION fChgMSISNDStock RETURNS LOGICAL
(liBegin AS INTEGER,
 liEnd AS INTEGER):

   DEFINE VARIABLE i AS INTEGER NO-UNDO. 

   DO i = liBegin TO liEnd with frame a:
      
      FIND msisdnnumber where
         msisdnnumber.cli = string(i) NO-LOCK.
      
      FIND FIRST msisdn where
         msisdn.brand = "1" and
         msisdn.cli = msisdnnumber.cli NO-LOCK NO-ERROR.

      IF NOT AVAIL msisdn then do:
      
         FIND msisdn where
            msisdn.brand = "" and
            msisdn.cli = msisdnnumber.cli EXCLUSIVE-LOCK NO-ERROR.

         assign msisdn.brand = "1". 
         put stream slog unformatted msisdn.cli " Brand to 1" skip.
         find current msisdn no-lock.
      end.
      
      if msisdn.pos eq "" then do:
         
         put stream slog unformatted msisdn.cli " " msisdn.pos " " 
            msisdn.statuscode " OK" skip.
         
         fMakeMsidnHistory(recid(msisdn)). 
         assign
            msisdn.pos = "VIP". 
      end.
      else do:   
         put stream slog unformatted msisdn.cli " " msisdn.pos " " 
            msisdn.statuscode " SKIPPED" skip.
      end. 
   END.

END FUNCTION. 

fChgMSISNDStock(633000000, 633009999).
fChgMSISNDStock(633980000, 633999999).

output stream slog close.

/* ICC STOCK */

DEFINE VARIABLE lcICCStart AS int64 NO-UNDO. 
DEFINE VARIABLE lcICCEnd   AS int64 NO-UNDO. 

lcICCStart = 8934040908013810823.
lcICCEnd   = 8934040908013811813.

def stream slog2.
output stream slog2 to /apps/snet/200811/as_YCM1102_ICC.log.
DEFINE VARIABLE i as int64 no-undo.

do i = lcICCStart TO lcICCEnd:
   
   FIND sim where sim.icc = string(i) NO-LOCK NO-ERROR.
   
   IF not avail sim THEN DO:
      next.
   END.

   if sim.stock ne "retailer" or
      sim.simstat ne 1 then do:   
      put stream slog2 unformatted sim.icc " " sim.stock " " sim.simstat " SKIPPED" skip.
   
   END.
   ELSE DO:
      
      put stream slog2 unformatted sim.icc " " sim.stock " " sim.simstat " OK" skip.
      find current sim EXCLUSIVE-LOCK.
      assign sim.stock = "VIP".
     
   END.
END.  

output stream slog2 close.
