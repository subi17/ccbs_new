{Syst/commpaa.i}
katun = "harrim".
gcBrand = "1".
{Func/msisdn.i}

DEFINE VARIABLE iCLi as int64 no-undo.

DEFINE VARIABLE liHandled AS INTEGER NO-UNDO init 0. 
DEFINE VARIABLE lcPos AS CHARACTER NO-UNDO INIT "webshop". 

def stream slog.
def stream sskip.
def stream sgold.
output stream slog to  /apps/snet/200812/hm_ycm1181_handled.txt.
output stream sgold to /apps/snet/200812/hm_ycm1181_golden.txt.
output stream sskip to /apps/snet/200812/hm_ycm1181_exceptions.txt.

LOOPPI:

DO iCli = 633351978 TO 633499999:
  
  FIND FIRST msisdnnumber where
      msisdnnumber.cli = string(iCli) NO-LOCK NO-ERROR.
  
  IF NOT AVAIL msisdnnumber THEN DO:
     put stream sskip unformatted iCLi " IS NOT AVAILABLE" skip.
     NEXT LOOPPI.
  END.
  
  ELSE DO:
      
      IF msisdnnumber.rank = 2 THEN DO:
         put stream sskip unformatted iCLi " HAS RANK 2 (?)" skip.
         NEXT LOOPPI.
      END.
      
      IF msisdnnumber.rank = 1 THEN DO:
         put stream sgold unformatted msisdnnumber.cli " IS GOLD NUMBER" skip.
         NEXT LOOPPI.
      END.
      
      IF msisdnnumber.rank ne 0 THEN DO:
         put stream sgold unformatted msisdnnumber.cli " HAS RANK " msisdnnumber.rank skip.
         NEXT LOOPPI.
      END.

      FIND FIRST msisdn where msisdn.brand = "1" and 
         msisdn.cli = msisdnnumber.cli use-index cli NO-LOCK NO-ERROR.
     
      IF NOT AVAIL msisdn THEN DO:
         find msisdn where msisdn.brand = "" AND
         msisdn.cli = msisdnnumber.cli NO-LOCK NO-ERROR.
      END.

      IF NOT AVAIL msisdn THEN DO:
         put stream sskip unformatted iCLi " TIMESTAMP NOT FOUND" skip.
         NEXT LOOPPI.
      END.
      
      /* check that number is not really used */
      FIND FIRST mobsub where
         mobsub.cli = msisdn.cli NO-LOCK NO-ERROR.
      
      IF AVAIL mobsub then do:
         put stream sskip unformatted mobsub.cli " IS IN USE" skip.
         NEXT LOOPPI.
      END.

      IF msisdn.statuscode ne 0 THEN DO:
         put stream sskip unformatted msisdn.cli " HAS STATUS " msisdn.statuscode  skip.
         NEXT LOOPPI.
      END.
     
      liHandled = liHandled + 1.
      
      if liHandled > 50000 then do:
         LEAVE LOOPPI.
      end.
      put stream slog unformatted msisdn.cli " " lcPos skip.
      
      do trans: 
         
         IF msisdn.brand eq "1" THEN DO:
            put stream sskip unformatted msisdn.cli " ALLREADY Brand " msisdn.Brand skip.
         END.
         ELSE DO:
          find current msisdn EXCLUSIVE-LOCK.
            assign msisdn.brand = "1". 
            find current msisdn NO-LOCK.  
         END.

         fMakeMSIDNHistory(recid(msisdn)).
         assign
           msisdn.statuscode = 1
           msisdn.pos = lcPos.

         release msisdn. 
     end.

  END.

END.

output stream sskip close.
output stream sgold close.
output stream slog close.
      
