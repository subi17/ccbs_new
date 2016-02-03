{Func/date.i}
{Syst/commpaa.i}
{Func/cparam2.i}

ASSIGN gcBrand = "1"
       katun   = "batch".

DEF VAR lcSpoolPath      AS CHAR NO-UNDO.
       ASSIGN
       lcSpoolPath = fCParam("Nagios","NagiosTmpDir").
       

FUNCTION getOnlineUsersSelfcare RETURNS INTEGER ():

   DEF VAR SPcounter AS INT INITIAL 0 NO-UNDO.
   FOR EACH websession NO-LOCK WHERE
            websession.lastdate >= TODAY AND
            INTEGER(websession.lasttime) + 900 >= INTEGER(TIME):

      SPcounter = SPcounter + 1.
   END.

   RETURN SPcounter.
END.


DEF STREAM log.
OUTPUT STREAM log TO VALUE (lcSpoolPath + "selfcare-session-count.txt").

PUT STREAM log UNFORMATTED 
  getOnlineUsersSelfcare() SKIP.
   
OUTPUT STREAM log CLOSE.   
