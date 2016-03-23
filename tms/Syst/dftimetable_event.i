&IF "{&DFTIMETABLE_EVENT_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE DFTIMETABLE_EVENT_I YES

{Func/timestamp.i}
{Syst/dftimetable_generic.i}

DEF TEMP-TABLE ttEvent NO-UNDO
   FIELD DumpID      AS INT
   FIELD DumpName    AS CHAR
   FIELD DumpMode    AS CHAR
   FIELD EventDate   AS DATE
   FIELD EventDay    AS INT
   FIELD EventTime   AS INT
   FIELD AveDuration AS CHAR
   INDEX EventDate EventDate EventDay EventTime.
   
DEFINE TEMP-TABLE ttTime NO-UNDO
   FIELD IntTime AS INTEGER
   INDEX IntTime IS PRIMARY UNIQUE IntTime.


/*
   This is internal function and only function fDoEventList specified
   below is calling this function.
   
   Function will create ttEvent records for one DFTimeTable record.
   
   idaDate   =  the day on where the function will create ttEvents
                (checked before calling this function that it is ok to create ttEvents
                for the date)
   iiFromTime = it is possible to tell which is the minimum allowed time
                (this is at value -1 when there is no restriction)
   ideLastRun = DFTimeTable.LastRun (when idaDate is same as the lastrun date
                                     the fromtime is set to lastrun time)
   icValidTimes = DFTimeTable.DumpTime
   iiDumpID     = DumpFile.DumpID
   icDumpName   = DumpFile.DumpName
   icDumpMode   = DFTimeTable.DumpMode
   icAveDuration = DumpFile.AveDurFull OR DumpFile.AveDurMod
*/
FUNCTION fCreateTTEvent RETURNS LOGICAL
   (idaDate         AS DATE,
    iiFromTime      AS INTEGER, 
    ideLastRun      AS DECIMAL,
    icValidTimes    AS CHARACTER,
    iiDumpID        AS INTEGER,
    icDumpName      AS CHARACTER,
    icDumpMode      AS CHARACTER,
    icAveDuration   AS CHARACTER):

   DEFINE VARIABLE lcTimeList AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liTime     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lii        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldaLastRunDate AS DATE NO-UNDO.
   DEFINE VARIABLE liLastRunTime AS INTEGER NO-UNDO.
   DEFINE VARIABLE liMaxEventsForDFTimeTable AS INTEGER INITIAL 30 NO-UNDO.
   
   fSplitTS(ideLastRun, OUTPUT ldaLastRunDate, OUTPUT liLastRunTime).   
   
   IF ldaLastRunDate = idaDate AND liLastRunTime > iiFromTime
   THEN iiFromTime = liLastRunTime. 

   lcTimeList = fConvertListToTime(icValidTimes).
   
   DO lii = 1 TO NUM-ENTRIES(lcTimeList):
      
      liTime = INTEGER(ENTRY(lii,lcTimeList)).

      IF liTime = 86400
      THEN liMaxEventsForDFTimeTable = fCreateTTTimeFromSpecialTimeEntry(ENTRY(lii,icValidTimes), iiFromTime, liMaxEventsForDFTimeTable).
      
      ELSE IF liTime >= iiFromTime AND fDoTTTime(liTime)
      THEN liMaxEventsForDFTimeTable = liMaxEventsForDFTimeTable - 1.
      
      IF liMaxEventsForDFTimeTable <= 0
      THEN LEAVE.
   
   END.
   
   FOR EACH ttTime:
      
      CREATE ttEvent.
      ASSIGN 
         ttEvent.EventTime   = ttTime.IntTime
         ttEvent.EventDate   = idaDate
         ttEvent.EventDay    = WEEKDAY(idaDate)
         ttEvent.DumpID      = iiDumpID
         ttEvent.DumpName    = icDumpName
         ttEvent.DumpMode    = icDumpMode
         ttEvent.AveDuration = icAveDuration.   
      
   END.
   
   FINALLY:
      EMPTY TEMP-TABLE ttTime.
   END FINALLY.

END FUNCTION.

/*
   Function will call fCreateTTEvent function and will
   create ttEvent temp-table records.
   
   idaFromDate = The earliest date for ttEvent records (this is normally TODAY or more)
   iiFromTime  = The earliest time which is allowed in idaFromDate for ttEvent records (set this to 0 to allow everything)

   idaToDate   = The latest date for ttEvent records
   iiDumpID    = The dump id to which a ttEvent records are created
                 if this is set 0 (zero) then ttEvent records are created for every dumps
   icOmittedDumps = Comma separated list of dump file categories which will be omitted
   ilOnlyWorkDays = Is on value TRUE if only working days are allowed
*/
FUNCTION fDoEventList RETURNS LOGICAL
   (idaFromDate     AS DATE,
    iiFromTime      AS INTEGER,
    idaToDate       AS DATE,
    iiDumpID        AS INTEGER,
    icOmittedDumps  AS CHARACTER,
    ilOnlyWorkDays  AS LOGICAL).


   EMPTY TEMP-TABLE ttEvent.
   
   IF iiDumpID = ?
   THEN RETURN FALSE.
   
   IF idaFromDate = ? OR idaToDate = ? OR idaFromDate > idaToDate
   THEN RETURN FALSE.
   
   DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
   
   FOR
      EACH DumpFile NO-LOCK USE-INDEX DumpID WHERE
         DumpFile.DumpID >= iiDumpID AND
         DumpFile.Active  = TRUE:

      IF iiDumpID > 0 AND DumpFile.DumpID > iiDumpID
      THEN LEAVE.

      IF LOOKUP(DumpFile.FileCategory,icOmittedDumps,",") > 0 THEN NEXT.

      FOR
         EACH DFTimeTable NO-LOCK WHERE
            DFTimeTable.Brand     = DumpFile.Brand  AND
            DFTimeTable.DumpID    = DumpFile.DumpID AND
            DFTimeTable.FromDate <= idaFromDate     AND
            DFTimeTable.ToDate   >= idaToDate: 
         
         DO ldaDate = idaFromDate TO idaToDate:
            IF fDayAllowed(ldaDate, DFTimeTable.DumpDay, DFTimeTable.DumpWeekDay, ilOnlyWorkDays)
            THEN fCreateTTEvent(ldaDate,
                                (IF ldaDate = idaFromDate THEN iiFromTime ELSE -1),
                                DFTimeTable.LastRun, 
                                DFTimeTable.DumpTime,
                                DumpFile.DumpID,
                                DumpFile.DumpName,
                                DFTimeTable.DumpMode,
                                IF DFTimeTable.DumpMode = "Full"
                                THEN STRING(DumpFile.AveDurFull,"HH:MM:SS")
                                ELSE STRING(DumpFile.AveDurMod,"HH:MM:SS")).
         END.
      END.
   END.

   RETURN TRUE.

END FUNCTION. 


/*
   Function will call fCreateTTEvent function and will
   create ttEvent temp-table records for one DFTimeTable record.
   
   idaFromDate = The earliest date for ttEvent records (this is normally TODAY or more)
   iiFromTime  = The earliest time which is allowed in idaFromDate for ttEvent records (set this to 0 to allow everything)

   idaToDate   = The latest date for ttEvent records
   iiDumpID    = The dump id to which a ttEvent records are created
                 if this is set 0 (zero) then ttEvent records are created for every dumps
   icOmittedDumps = Comma separated list of dump file categories which will be omitted
   ilOnlyWorkDays = Is on value TRUE if only working days are allowed
*/
FUNCTION fDoEventListForOne RETURNS LOGICAL
   (idaFromDate     AS DATE,
    iiFromTime      AS INTEGER,
    idaToDate       AS DATE,
    iridDFTT        AS ROWID,
    ilOnlyWorkDays  AS LOGICAL).

   EMPTY TEMP-TABLE ttEvent.
   
   IF idaFromDate = ? OR idaToDate = ? OR idaFromDate > idaToDate
   THEN RETURN FALSE.
   
   FIND FIRST DFTimeTable NO-LOCK WHERE
      ROWID(DFTimeTable) = iridDFTT       AND
      DFTimeTable.FromDate <= idaFromDate AND
      DFTimeTable.ToDate   >= idaToDate
   NO-ERROR.
   
   IF NOT AVAILABLE DFTimeTable
   THEN RETURN FALSE.
   
   DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
   
   DO ldaDate = idaFromDate TO idaToDate:
      IF fDayAllowed(ldaDate, DFTimeTable.DumpDay, DFTimeTable.DumpWeekDay, ilOnlyWorkDays)
      THEN fCreateTTEvent(ldaDate,
                          (IF ldaDate = idaFromDate THEN iiFromTime ELSE -1),
                          DFTimeTable.LastRun, 
                          DFTimeTable.DumpTime,
                          0,
                          "",
                          DFTimeTable.DumpMode,
                          "").
   END.

   RETURN TRUE.

END FUNCTION. 

&ENDIF