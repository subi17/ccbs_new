/*  scheduled_dumps.i   11.02.2013/jannetou
*/
&IF "{&SCHEDULED_DUMPS_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE SCHEDULED_DUMPS_I YES
{Syst/dftimetable.i}

DEF TEMP-TABLE ttEvent NO-UNDO
   FIELD DumpID      AS INT
   FIELD DumpName    AS CHAR
   FIELD DumpMode    AS CHAR
   FIELD EventDate   AS DATE
   FIELD EventDay    AS INT
   FIELD EventTime   AS INT
   FIELD AveDuration AS CHAR
   INDEX EventDate EventDate EventDay EventTime.


FUNCTION fCreate_ttEvent RETURNS LOG:
   FOR EACH ttDays,
       EACH ttTimes:

      CREATE ttEvent.
       
      IF ttDays.MonthDay NE ? THEN ASSIGN
         ttEvent.EventDate = ttDays.MonthDay
         ttEvent.EventDay  = WEEKDAY(ttDays.MonthDay).

      ASSIGN 
         ttEvent.EventTime = ttTimes.DumpTime
         ttEvent.DumpID    = DumpFile.DumpID
         ttEvent.DumpName  = DumpFile.DumpName
         ttEvent.DumpMode  = DFTimeTable.DumpMode.
      IF DFTimeTable.DumpMode = "Full" THEN
         ttEvent.AveDuration = STRING(DumpFile.AveDurFull,"HH:MM:SS").
      ELSE
         ttEvent.AveDuration = STRING(DumpFile.AveDurMod,"HH:MM:SS").
      
  END.
END FUNCTION.


FUNCTION fDumpEventList RETURNS LOG
   (INPUT ldaEndDate AS DATE, 
    INPUT ldaFromDate AS DATE,
    INPUT dumpId AS INT).

   DEF VAR lcOmittedDumps AS CHAR NO-UNDO INIT "TMS,TRACK,Cassandra,HPD".
   /* Make ttEvent for only certain DumpID */
   IF dumpId = 0 THEN DO:
      FOR EACH DFTimeTable NO-LOCK,
         FIRST DumpFile OF DFTimeTable NO-LOCK WHERE
               DumpFile.Active = TRUE:

         IF LOOKUP(DumpFile.FileCategory,lcOmittedDumps,",") > 0 THEN NEXT.

         fAnalyseTimeTable(ldaFromDate,ldaEndDate).
         fCreate_ttEvent().
      END.
   END.
   /* Make ttEvent for all dumpIDs */
   IF dumpId > 0 THEN DO:
      FOR EACH DFTimeTable NO-LOCK WHERE
               DFTimeTable.DumpId = DumpId,
         FIRST DumpFile OF DFTimeTable NO-LOCK WHERE
               DumpFile.Active = TRUE:

         IF LOOKUP(DumpFile.FileCategory,lcOmittedDumps,",") > 0 THEN NEXT.

            fAnalyseTimeTable(ldaFromDate,ldaEndDate).
            fCreate_ttEvent().
      END.
   END.
END FUNCTION. 

&ENDIF
