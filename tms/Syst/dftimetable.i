/* dftimetable.i      29.10.08/aam

*/

DEF TEMP-TABLE ttDays NO-UNDO
   FIELD MonthDay AS DATE.
   
DEF TEMP-TABLE ttTimes NO-UNDO
   FIELD DumpTime AS INT.
 

FUNCTION fAnalyseTimeTable RETURNS LOGIC
   (INPUT pFromDate AS DATE,
    INPUT pToDate AS DATE).

   DEF VAR liCnt      AS INT  NO-UNDO.
   DEF VAR liDay      AS INT  NO-UNDO.
   DEF VAR liTime     AS INT  NO-UNDO.
   DEF VAR liConfDay  AS INT  NO-UNDO EXTENT 2.
   DEF VAR lcTime     AS CHAR NO-UNDO.
   DEF VAR liCalcTime AS INT  NO-UNDO.
   DEF VAR liDumpTime AS INT  NO-UNDO.
   DEF VAR lcConfDay  AS CHAR NO-UNDO.
   DEF VAR ldtCnt     AS DATE NO-UNDO.

   EMPTY TEMP-TABLE ttDays.
   EMPTY TEMP-TABLE ttTimes.
      
   /* month level */
   
   IF DFTimeTable.DumpDay = "*" THEN
   DO ldtCnt = pFromDate TO pToDate:
      CREATE ttDays.
      ASSIGN ttDays.MonthDay = ldtCnt.
   END.
   
   ELSE
   DO liCnt = 1 TO NUM-ENTRIES(DFTimeTable.DumpDay):
      lcConfDay = ENTRY(liCnt,DFTimeTable.DumpDay).
      
      liConfDay = 0.
      liConfDay = INTEGER(ENTRY(1,lcConfDay,"-")) NO-ERROR.
      IF ERROR-STATUS:ERROR OR liConfDay[1] = 0 THEN NEXT.
       
      IF NUM-ENTRIES(lcConfDay,"-") > 1 THEN DO:
         liConfDay[2] = INTEGER(ENTRY(2,lcConfDay,"-")) NO-ERROR.
         IF ERROR-STATUS:ERROR OR liConfDay[2] = 0 THEN NEXT.
      END.   
         
      IF liConfDay[2] < liConfDay[1] THEN NEXT.
      
      DO ldtCnt = pFromDate TO pToDate:
         DO liDay = liConfDay[1] TO liConfDay[2]:

            IF DAY(ldtCnt) = liDay OR
               (liDay > 28 AND liDay > DAY(ldtCnt) AND
                MONTH(ldtCnt) NE MONTH(ldtCnt + 1))
            THEN DO: 
              IF CAN-FIND(FIRST ttDays WHERE ttDays.MonthDay = ldtCnt)
              THEN NEXT.
         
              CREATE ttDays.
              ttDays.MonthDay = ldtCnt.
            END.
         END.
      END.

   END.
   
   /* weekday level */
   IF DFTimeTable.DumpWeekDay = "*" THEN
   DO ldtCnt = pFromDate TO pToDate:
      CREATE ttDays.
      ASSIGN ttDays.MonthDay = ldtCnt.
   END.

   IF DFTimeTable.DumpWeekDay > "" THEN
   DO liCnt = 1 TO NUM-ENTRIES(DFTimeTable.DumpWeekDay):
      lcConfDay = ENTRY(liCnt,DFTimeTable.DumpWeekDay).
      
      liConfDay = 0.
      liConfDay = INTEGER(ENTRY(1,lcConfDay,"-")) NO-ERROR.
      IF ERROR-STATUS:ERROR OR liConfDay[1] = 0 THEN NEXT.
       
      IF NUM-ENTRIES(lcConfDay,"-") > 1 THEN DO:
         liConfDay[2] = INTEGER(ENTRY(2,lcConfDay,"-")) NO-ERROR.
         IF ERROR-STATUS:ERROR OR liConfDay[2] = 0 THEN NEXT.
      END.   
         
      IF liConfDay[2] < liConfDay[1] THEN NEXT.
      
      DO ldtCnt = pFromDate TO pToDate:
         IF CAN-FIND(FIRST ttDays WHERE ttDays.MonthDay = ldtCnt) 
         THEN NEXT.
         IF liConfDay[2] = 0 THEN DO:
            IF WEEKDAY(ldtCnt) = liConfDay[1] THEN DO: 
               CREATE ttDays.
               ttDays.MonthDay = ldtCnt.
            END.
         END.
         ELSE DO liDay = liConfDay[1] TO liConfDay[2]:
            IF WEEKDAY(ldtCnt) = liDay THEN DO: 
               CREATE ttDays.
               ttDays.MonthDay = ldtCnt.
            END.
         END.
      END.
   END.

   /* Trigger level */ 
   IF DFTimeTable.DumpTrigger THEN
   DO ldtCnt = pFromDate TO pToDate:
      CREATE ttDays.
      ASSIGN ttDays.MonthDay = ldtCnt.
   END.

   IF DFTimeTable.DumpTime > "" THEN 
   DO liCnt = 1 TO NUM-ENTRIES(DFTimeTable.DumpTime):
              
      ASSIGN 
         lcTime     = ENTRY(liCnt,DFTimeTable.DumpTime)
         liDumpTime = 0.
         
      DO liTime = 1 TO NUM-ENTRIES(lcTime,":"):
         liCalcTime = 0.
         liCalcTime = INTEGER(ENTRY(liTime,lcTime,":")) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            liDumpTime = ?.
            LEAVE.
         END.
            
         CASE liTime:
         WHEN 1 THEN liCalcTime = liCalcTime * 3600.
         WHEN 2 THEN liCalcTime = liCalcTime * 60.
         END CASE.
            
         liDumpTime = liDumpTime + liCalcTime.
      END.
              
      IF liDumpTime NE ? THEN DO:
         CREATE ttTimes.
         ttTimes.DumpTime = liDumpTime.
      END.
   END.   
END FUNCTION.

FUNCTION fResetDumpTrigger RETURNS LOGICAL 
    (INPUT liDumpID   AS INTEGER,
     INPUT lcDumpMode AS CHARACTER):

   FIND FIRST DFTimeTable EXCLUSIVE-LOCK WHERE 
              DFTimeTable.Brand       = gcBrand    AND 
              DFTimeTable.DumpId      = liDumpID   AND 
              DFTimeTable.DumpMode    = lcDumpMode AND 
              DFTimeTable.DumpTrigger = YES        NO-ERROR. 

   IF AVAIL DFTimeTable THEN DO:
      DFTimeTable.DumpTrigger = NO.
      RETURN TRUE.
   END.   
   ELSE 
      RETURN FALSE.

END.    

