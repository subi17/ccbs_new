/* ---------------------------------------------------------------------------
  MODULE .......: dumpfile_batch.P
  FUNCTION .....: batch process for creating dump files
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 29.10.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN 
   gcBrand = "1" 
   katun   = "Cron".
       
{Func/lib/eventlog.i}
{Func/timestamp.i}
{Syst/dftimetable.i}
{Func/cparam2.i}
{Syst/host.i}

DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR liDay       AS INT  NO-UNDO.
DEF VAR liDumped    AS INT  NO-UNDO. 
DEF VAR liCurrent   AS INT  NO-UNDO.
DEF VAR ldaLastRun  AS DATE NO-UNDO.
DEF VAR liLastRun   AS INT  NO-UNDO.
DEF VAR lcActionKey AS CHAR NO-UNDO.
DEF VAR llQuery     AS LOG  NO-UNDO.
DEF VAR lcQueryLog  AS CHAR NO-UNDO.
DEF VAR lcLastRun   AS CHAR NO-UNDO.
DEF VAR lcNow       AS CHAR NO-UNDO.
DEF VAR ldaDumpDate AS DATE NO-UNDO.
DEF VAR ldaOngoing  AS DATE NO-UNDO.
DEF VAR liOngoing   AS INT  NO-UNDO.

DEFINE VARIABLE  llReplica AS LOGICAL NO-UNDO.

DEF TEMP-TABLE ttDump NO-UNDO
   FIELD DumpID      AS INT
   FIELD DumpName    AS CHAR
   FIELD DumpMode    AS CHAR
   FIELD DumpTime    AS INT  /* dumps are executed in correct order */
   FIELD TTRecid     AS RECID
   FIELD FileNameTag AS CHAR
   FIELD Replication AS LOG
   INDEX DumpID DumpID DumpMode.
   
DEF BUFFER bTimeTable FOR DFTimeTable.
DEF BUFFER bttDump    FOR ttDump.

DEF STREAM sLog.


FUNCTION fMarkTimeTable RETURNS LOGIC
   (irTimeTable AS RECID,
    icAction    AS CHAR):
   
   /* mark an ongoing run to a separate field, so that it is easier to 
      detect possible problems */
   DO TRANS:
      FIND bTimeTable WHERE RECID(bTimeTable) = irTimeTable EXCLUSIVE-LOCK.
      CASE icAction:
      WHEN "done"   THEN ASSIGN
         bTimeTable.LastRun = fMakeTS()
         bTimeTable.Ongoing = 0.
      WHEN "pick" THEN bTimeTable.Ongoing = fMakeTS().
      WHEN "stop" THEN bTimeTable.Ongoing = 0.
      END CASE.
      RELEASE bTimeTable.
   END.
   
END FUNCTION.


ASSIGN
   liCurrent   = TIME
   ldaDumpDate = TODAY.

/* only do a query of what runs are due */
IF ENTRY(1,SESSION:PARAMETER) = "query" THEN DO:
   llQuery = TRUE.
   
   IF NUM-ENTRIES(SESSION:PARAMETER) > 1 THEN DO:
      liCnt = INTEGER(ENTRY(2,SESSION:PARAMETER)) NO-ERROR.
      ldaDumpDate = ldaDumpDate + liCnt.
   END.
   IF NUM-ENTRIES(SESSION:PARAMETER) > 2 THEN DO:
      liCnt = INTEGER(ENTRY(3,SESSION:PARAMETER)) NO-ERROR.
      liCurrent = liCurrent + liCnt * 3600.
   END.
END.   

ELSE IF SESSION:PARAMETER > "" THEN QUIT.

IF NOT llQuery THEN DO:
   fELog("DUMPFILE","started").
END.

llReplica = fIsThisReplica().

FOR EACH DumpFile NO-LOCK WHERE
         DumpFile.Brand  = gcBrand AND
         DumpFile.Active = TRUE:
   
   /* analyse time table definitions and check if any are due now */      
   FOR EACH DFTimeTable NO-LOCK WHERE
            DFTimeTable.Brand     = gcBrand         AND
            DFTimeTable.DumpID    = DumpFile.DumpID AND
            DFTimeTable.ToDate   >= ldaDumpDate     AND
            DFTimeTable.FromDate <= ldaDumpDate:
        
      IF (DFTimeTable.UseReplica AND NOT llReplica) OR  
         (NOT DFTimeTable.UseReplica AND llReplica) THEN NEXT.         
      
      fAnalyseTimeTable(TODAY - 30,TODAY).

      IF DFTimeTable.LastRun > 0 THEN DO:
         fSplitTS(DFTimeTable.LastRun,
                  OUTPUT ldaLastRun,
                  OUTPUT liLastRun).
      END.
      ELSE ldaLastRun = ?.
        
      IF DFTimeTable.Ongoing > 0 THEN DO:
         fSplitTS(DFTimeTable.Ongoing,
                  OUTPUT ldaOngoing,
                  OUTPUT liOngoing).
                         
         /* another run that started today is still active */
         IF ldaOngoing = ldaDumpDate THEN NEXT.
      END.
      
      /* month level */
      FIND FIRST ttDays WHERE ttDays.MonthDay = ldaDumpDate NO-ERROR.

      IF NOT AVAILABLE ttDays THEN NEXT.   
          

      FOR EACH ttTimes WHERE 
               ttTimes.DumpTime <= liCurrent
      BY ttTimes.DumpTime DESC:
               
         IF ldaLastRun = ldaDumpDate AND liLastRun >= ttTimes.DumpTime THEN
            NEXT.

         IF CAN-FIND(FIRST ttDump WHERE 
                           ttDump.DumpID   = DumpFile.DumpID AND
                           ttDump.DumpMode = DFTimeTable.DumpMode)
         THEN NEXT.
         
         CREATE ttDump.
         ASSIGN
            ttDump.DumpID      = DumpFile.DumpID 
            ttDump.DumpName    = DumpFile.DumpName
            ttDump.DumpMode    = DFTimeTable.DumpMode
            ttDump.TTRecid     = RECID(DFTimeTable)
            ttDump.DumpTime    = ttTimes.DumpTime
            ttDump.FileNameTag = DFTimeTable.FileNameTag
            ttDump.Replication = llReplica.
         
         /* mark as picked, in case next cron run starts before this
            is handled */
         IF NOT llQuery THEN DO:   
            fMarkTimeTable(ttDump.TTRecid,"pick").
         END.   
      END.
   END.         
   
   /* if there are more than one entries for the same dumpid and time, and 
      one of them is 'full', perform only that (mark others as done) */
   FOR FIRST ttDump WHERE 
             ttDump.DumpID   = DumpFile.DumpID AND
             ttDump.DumpMode = "full",
        EACH bttDump WHERE 
             bttDump.DumpID   = DumpFile.DumpID AND
             bttDump.DumpTime = ttDump.DumpTime AND
             bttDump.DumpMode NE "full":
      
      IF NOT llQuery THEN DO:
         fMarkTimeTable(bttDump.TTRecid,"done").      
      END.
      
      DELETE bttDump.
   END.
 
END.  /* dumpfile */


/* write a log file as a response to timetable query */
IF llQuery THEN DO:

   lcQueryLog = fCParamC("TimeTableQueryLog").
   IF lcQueryLog = ? THEN lcQueryLog = "/tmp/timetable_query.log".

   lcNow = fISOTimeZone(ldaDumpDate,liCurrent).
   
   OUTPUT STREAM sLog TO VALUE(lcQueryLog).
   
   FOR EACH ttDump,
      FIRST DumpFile NO-LOCK WHERE
            DumpFile.DumpID = ttDump.DumpID,
      FIRST DFTimeTable NO-LOCK WHERE
            RECID(DFTimeTable) = ttDump.TTRecid
   BY ttDump.DumpTime:
   
      IF DFTimeTable.LastRun > 0 THEN DO:
         fSplitTS(DFTimeTable.LastRun,
                  OUTPUT ldaLastRun,
                  OUTPUT liLastRun).
         lcLastRun = fISOTimeZone(ldaLastRun,liLastRun).
      END.
      ELSE lcLastRun = fISOTimeZone(DATE(1,1,2007),0).
               
      PUT STREAM sLog UNFORMATTED 
         DumpFile.DumpID       " " 
         DumpFile.DumpName     " "
         DumpFile.FileCategory " "
         DFTimeTable.DumpMode  " "
         lcNow                 " " 
         lcLastRun             SKIP.
         
   END.

   OUTPUT STREAM sLog CLOSE. 
   
   QUIT.
END.


IF NOT llQuery THEN 
FOR EACH ttDump
BY ttDump.DumpTime:
   
   RUN dumpfile_run (ttDump.DumpID,
                     ttDump.DumpMode,
                     ttDump.FileNameTag,
                     ttDump.Replication,
                     OUTPUT liCnt).
   
   /* nothing done or interrupted */
   IF RETURN-VALUE BEGINS "ERROR:" OR RETURN-VALUE BEGINS "INFORMATION:"
   THEN DO:
      fMarkTimeTable(ttDump.TTRecid,"stop").
      NEXT.
   END.   

   ASSIGN
      liFiles  = liFiles + 1
      liDumped = liDumped + liCnt.
   
   /* mark as done */
   fMarkTimeTable(ttDump.TTRecid,"done").
END.

fELog("DUMPFILE","stopped,Dumps:" + STRING(liFiles) + 
                ",Rows:" + STRING(liDumped)).

QUIT.

