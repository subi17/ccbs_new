{commpaa.i}
{cparam2.i}

ASSIGN gcBrand = "1"
       katun   = "batch".

DEFINE STREAM osDump.
DEFINE TEMP-TABLE ttMStat NO-UNDO
 FIELD iStat AS INT
 FIELD iAmt  AS INT.
 

DEFINE VARIABLE liAll       AS INTEGER   NO-UNDO.
DEFINE VARIABLE liAct       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcSpoolPath AS CHARACTER NO-UNDO.

ASSIGN
lcSpoolPath = fCParam("Nagios","NagiosTmpDir").

FOR EACH mobsub NO-LOCK:
   liAll = liAll + 1.
   IF  NOT(mobsub.msstatus = 4 OR
           mobsub.msstatus = 7 OR
           mobsub.msstatus = 8 OR
           mobsub.msstatus = 9 OR
           mobsub.msstatus > 30) AND
           mobsub.brand = "1" THEN NEXT.
   FIND FIRST ttMStat WHERE ttMStat.iStat = MobSub.MsStatus NO-ERROR.
   IF NOT AVAILABLE ttMStat THEN DO:
      liAct = liAct + 1.
      CREATE ttMStat.
      ASSIGN ttMStat.iStat = MobSub.MsStatus.
   END.
   
   ttMStat.iAmt = ttMStat.iAmt + 1.
   /*
   IF liAll MOD 1000 = 0 THEN DO:
      DISP liAll liAct WITH 1 DOWN.
      PAUSE 0.
   END.
   */
END.
/* Write values for munin */

OUTPUT STREAM osDump TO VALUE (lcSpoolPath + "msjobstat.txt").
   FOR EACH ttMStat NO-LOCK.

   PUT STREAM osDump UNFORMATTED 
              ttMStat.iStat " | " ttMStat.iAmt SKIP.
END.
PUT STREAM osDump UNFORMATTED 
           "99" " | " liAll SKIP.
           
OUTPUT STREAM osDump CLOSE.
