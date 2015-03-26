def input param icFile as char no-undo.
def input param icLog as char no-undo.
def input param iReqOldStat as int no-undo.
def input param icNewStat as int no-undo.
def input param ilSimulate as log no-undo.

DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttExpStatus
   FIELD iStatus AS INTEGER
   FIELD iCount  AS INTEGER
   INDEX idxStatus IS UNIQUE iStatus.

def stream sin.
input stream sin from value(icFile).

def stream slog.
output stream slog to value(icLog).

DEFINE TEMP-TABLE ttICCInUse
FIELD icc AS CHAR
FIELD msseq AS INT
INDEX icc IS PRIMARY UNIQUE icc. 

FOR EACH mobsub NO-LOCK:
   CREATE ttICCInUse.
   ASSIGN 
      ttICCInUse.icc = mobsub.icc
      ttICCInUse.msseq = mobsub.msseq.
END.
icc_loop:
repeat:

   import stream sin unformatted lcFile.

   lcFile = TRIM(lcFile).

   FIND sim where sim.icc = lcFile NO-LOCK NO-ERROR.
 
   IF LENGTH(lcFile) NE 19 THEN 
   DO:
      MESSAGE "Could not trim input line " lcFile 
        " to contain only 19 chars as ICC"
        VIEW-AS ALERT-BOX.
      RETURN.
   END.

   IF not avail sim THEN DO:
      put stream slog unformatted lcFile " NOT FOUND" SKIP.
   END.
   ELSE DO:
      find ttICCInUse where
         ttICCInUse.icc = sim.icc NO-LOCK NO-ERROR.
      IF AVAIL ttICCInUse then do:
         put stream slog unformatted "sim with icc " sim.icc " is in use with subscription " ttICCInUse.msseq skip.
         next icc_loop.
      end.
     
      IF sim.simstat eq iReqOldStat THEN
      do:
         put stream slog unformatted 
            sim.icc " " sim.stock " " sim.simstat "->" icNewStat skip.

         IF NOT ilSimulate THEN
         DO:
            find current sim EXCLUSIVE-LOCK.
            assign sim.simstat = icNewStat.
         END.
      end.
      else
      do:
         put stream slog unformatted "sim with icc " sim.icc " was not in status " 
             iReqOldStat " as expected, but in status " sim.simstat SKIP.
         if not can-find(ttexpstatus where ttexpstatus.istatus = sim.simstat)
         then
         do:
            create ttexpstatus.
            ttexpstatus.istatus = sim.simstat.
            ttexpstatus.iCount = 1.
         end.
         else
         do:
            find ttexpstatus where ttexpstatus.istatus = sim.simstat.
            ttexpstatus.iCount = iCount + 1.
         end.
      end.
   END.
END.

IF CAN-FIND(FIRST ttexpstatus) THEN
DO:
   put stream sLog unformatted SKIP(2) "exceptional SIM states:" SKIP.
   FOR EACH ttExpStatus:
       PUT STREAM sLog unformatted "Status: " ttexpstatus.iStatus "," 
          ttexpstatus.iCount " sims." SKIP.
   END.
END.
output stream slog close.
input stream sin close. 
