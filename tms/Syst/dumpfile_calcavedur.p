/* ----------------------------------------------------------------------
  MODULE .......: dumpfile_calcavedur.p
  TASK .........: Calculate average duration of dump creation
  APPLICATION ..: tms
  AUTHOR .......: Jannetou
  CREATED ......: 15.05.14
  Version ......: yoigo
---------------------------------------------------------------------- */


{timestamp.i}
DEF INPUT PARAMETER iiDumpId AS INT NO-UNDO. 

DEF VAR liavr AS INT NO-UNDO. 
DEF VAR liAveDur AS INT NO-UNDO. 
DEF VAR lDtStart AS DATETIME NO-UNDO.
DEF VAR lDtEnd AS DATETIME NO-UNDO.
DEF VAR liDumpDur AS INT NO-UNDO.
DEF VAR liResDur AS INT NO-UNDO. 
DEF VAR lind AS INT NO-UNDO. 
DEF VAR lcDumpMode AS CHAR NO-UNDO. 
DEF VAR lcModes AS CHAR NO-UNDO INIT "Full,Modified".
DEF VAR liDays AS INT NO-UNDO. 

DEF BUFFER bDumpFile FOR DumpFile.

FUNCTION fCheckDuration RETURN INTEGER
   (INPUT piavr AS INT,
    INPUT pcMode AS CHAR):
   DEF VAR liDbhrs AS INT NO-UNDO.
   DEF VAR liDbmin AS INT NO-UNDO.
   DEF VAR liDbsec AS INT NO-UNDO. 
   DEF VAR liDiff AS INT NO-UNDO. 
   DEF VAR liDuration AS INT NO-UNDO. 

   /* Read DB seconds from DumpFile */
   IF pcMode = "Full" THEN
      liDbsec = DumpFile.AveDurFull.
   ELSE
      liDbsec = DumpFile.AveDurMod.
   
   IF liDbSec > 0 THEN DO:
      /* Compare duration hours between database and recent value */
      IF piavr > liDbsec THEN
         lidiff = INT((1 - DEC(liDbsec / piavr)) * 100).
      ELSE 
         lidiff = INT((1 - DEC(piavr / liDbsec)) * 100).
      IF lidiff >= 5 THEN
         liDuration = piavr.
      ELSE
         liDuration = 0.
   END.
   ELSE
      liDuration = piavr.

   RETURN liDuration.

END FUNCTION. /* FUNCTION fCheckDuration RETURN CHARACTER */


DO lind = 1 TO NUM-ENTRIES(lcModes,","):
   lcDumpMode = ENTRY(lind,lcModes,",").

   IF lcDumpMode = "Full" THEN
      liDays = 90.
   ELSE
      liDays = 32.

   FOR EACH DumpFile NO-LOCK WHERE
            DumpFile.Active = True:
            
      IF iiDumpId > 0 AND DumpFile.DumpId NE iiDumpId THEN NEXT.
            
      FOR EACH DumpLog NO-LOCK  WHERE
               DumpLog.DumpId = DumpFile.DumpId AND
               DumpLog.DumpLogStatus = 3 AND
               DumpLog.DumpType = lcDumpMode AND
               DumpLog.CreateStart > fMake2DT(TODAY - liDays,0)
               BY DumpLog.CreateStart:
         IF DumpLog.CreateEnd = 0 OR
            DumpLog.CreateStart = 0 THEN NEXT.

         ASSIGN
            lDtStart = fTimeStamp2DateTime(DumpLog.CreateStart)
            lDtEnd   = fTimeStamp2DateTime(DumpLog.CreateEnd)
            liDumpDur = INT64(lDtEnd - lDtStart)
            liResDur  = liDumpDur / 1000.

         IF liResDur > 0 THEN
            ACCUMULATE (liResDur) (AVERAGE).
      END.
      liavr = (ACCUM AVERAGE liResDur).
      IF liavr > 0 THEN
         liAveDur = fCheckDuration(liavr,lcDumpMode).
         
      IF liAveDur > 0 THEN DO:
         FIND bDumpFile EXCLUSIVE-LOCK WHERE
              ROWID(bDumpFile) = ROWID(Dumpfile) NO-ERROR.
         IF lcDumpMode = "Full" THEN
            bDumpFile.AveDurFull = liAveDur.
         ELSE
            bDumpFile.AveDurMod = liAveDur.
         RELEASE bDumpFile.
      END.
   END.
END.
