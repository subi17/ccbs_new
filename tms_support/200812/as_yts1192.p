DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200812/as_yts1192.log.


def buffer bufSubser for subser.

FOR EACH mobsub where
   brand = "1" /*and
   cli = "622593412"*/ NO-LOCK:

   FOR FIRST SubSer NO-LOCK WHERE
      SubSer.MsSeq = MobSub.MsSeq AND
      SubSer.ServCom = "VMS",
   FIRST ServCom NO-LOCK WHERE
      ServCom.Brand = "1" AND
      ServCom.ServCom = SubSer.ServCom:
      
      /* Easy On-Off services */
      IF subser.ssstat = 0 then do:
      
         find bufSubser where rowid(bufSubser) = rowid(subser) EXCLUSIVE-LOCK.
         
         put stream slog unformatted bufsubser.msseq "|" bufsubser.servcom "|"
            bufsubser.ssdate "|" bufsubser.ssstat skip.

         assign bufSubser.ssstat = 1.
         
         release bufSubser.

      END.

   END.
END.

output stream slog close.
