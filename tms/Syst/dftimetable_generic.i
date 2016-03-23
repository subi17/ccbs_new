&IF "{&DFTIMETABLE_GENERIC_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE DFTIMETABLE_GENERIC_I YES

FUNCTION fIsWorkingDay RETURNS LOGIC
   (idaDate AS DATE):

   IF WEEKDAY(idaDate) = 7 OR WEEKDAY(idaDate) = 1 OR
      CAN-FIND(FIRST NatHoliday WHERE NatHoliday.Holiday = idaDate)
   THEN RETURN FALSE.
   ELSE RETURN TRUE.
   
END FUNCTION.


/* 
   Function will expand range n-m
   For example:
      1-4       => 1,2,3,4
      1,5,2-5,2 => 1,5,2,3,4,5,2
*/
FUNCTION fExpandRange RETURNS CHARACTER
   (icText      AS CHARACTER,
    icDelimiter AS CHARACTER):

      IF INDEX(icText,"-") = 0 OR icText = ?
      THEN RETURN icText.

      IF NUM-ENTRIES(ENTRY(1,icText,icDelimiter), "-") > 2
      THEN RETURN ?.

      IF NUM-ENTRIES(ENTRY(1,icText,icDelimiter), "-") = 2
      THEN DO:
         IF INTEGER(ENTRY(1,ENTRY(1,icText,icDelimiter),"-")) >= INTEGER(ENTRY(2,ENTRY(1,icText,icDelimiter),"-"))
         THEN DO:
            IF INDEX(icText,icDelimiter) = 0
            THEN RETURN ENTRY(1,ENTRY(1,icText,icDelimiter),"-").
            ELSE RETURN ENTRY(1,ENTRY(1,icText,icDelimiter),"-") + icDelimiter +
                        fExpandRange(SUBSTRING(icText,INDEX(icText,icDelimiter) + 1),icDelimiter).
         END.
         ELSE DO:
            IF INDEX(icText,icDelimiter) = 0
            THEN RETURN ENTRY(1,ENTRY(1,icText,icDelimiter),"-") + icDelimiter +
                        fExpandRange(STRING(INTEGER(ENTRY(1,ENTRY(1,icText,icDelimiter),"-")) + 1) + "-" +
                                     STRING(INTEGER(ENTRY(2,ENTRY(1,icText,icDelimiter),"-"))),icDelimiter).
            ELSE RETURN ENTRY(1,ENTRY(1,icText),"-") + icDelimiter +
                        fExpandRange(STRING(INTEGER(ENTRY(1,ENTRY(1,icText,icDelimiter),"-")) + 1) + "-" +
                                     STRING(INTEGER(ENTRY(2,ENTRY(1,icText,icDelimiter),"-"))) +
                                     SUBSTRING(icText,INDEX(icText,icDelimiter)),icDelimiter).
        END.
      END.

      RETURN ENTRY(1,icText) + icDelimiter + fExpandRange(SUBSTRING(icText,INDEX(icText,icDelimiter) + 1),icDelimiter).

END FUNCTION.


/*
   Function goes through icDelimiter separated list of positive integer values
   given in a list icValueList
   
   Function returns the value from the list which closest to the maximum value
   iiMaxValue and not exeeding it. If there is the maximum value the function
   returns it.
   
   If icValueList is empty or there is not any value in the list matching the
   criteria the function returns ? 
*/
FUNCTION fGetValueAtMost RETURNS INTEGER
   (iiMaxValue  AS INTEGER,
    icValueList AS CHARACTER,
    icDelimiter AS CHARACTER):
   
   IF icValueList = ""
   THEN RETURN ?.
   
   DEFINE VARIABLE liReturnValue AS INTEGER INITIAL -1 NO-UNDO.
   DEFINE VARIABLE lii           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE liEntryValue  AS INTEGER            NO-UNDO.
      
   DO lii = 1 TO NUM-ENTRIES(icValueList, icDelimiter):
      
      liEntryValue = INTEGER(ENTRY(lii,icValueList,icDelimiter)) NO-ERROR.
      
      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
      
      IF liEntryValue = iiMaxValue
      THEN RETURN iiMaxValue.
      
      IF liEntryValue < iiMaxValue
      THEN DO:
         liReturnValue = MAXIMUM(liReturnValue,liEntryValue).   
      END.
   
   END.

   IF liReturnValue = -1
   THEN RETURN ?.
   
   RETURN liReturnValue.

END FUNCTION.


/*
   Function expects icTimeText parameter in following syntax:
      
      "H:M:S"
   
   H = hour
   M = minute
   S = second
   
   H can be * or number between 0-23 (also range is possible for example 4-13, note that * is same as range 0-23)    
   M can be * or number between 0-59 (also range is possible for example 4-53, note that * is same as range 0-59)    
   M can be * or number between 0-59 (also range is possible for example 4-53, note that * is same as range 0-59)    

   It is also possible to give multiple values using slash character. For example
   following line is representing a times for every hour and every 15 minutes when second is 0 
   *:0/15/30/45:0

   iiGoalTime is a value representing a time as the number of seconds since
   midnight.
   
   The function tries to return the time as close as the goal by
   not exeeding it. I.e. the optimal result for the function
   is the goal time itself.
   
   If the function cannot find any suitable time it returns 86400
   (a value which is one bigger than the highest possible time value).
*/
FUNCTION fGetClosestTime RETURNS INTEGER
   (icTimeText AS CHARACTER,
    iiGoalTime AS INTEGER):

   IF iiGoalTime < 0 OR iiGoalTime > 86399
   THEN RETURN 86400.

   DEFINE VARIABLE liTime         AS INTEGER   EXTENT 3 NO-UNDO.
   DEFINE VARIABLE liTimeMaxValue AS INTEGER   EXTENT 3 INITIAL [23,59,59] NO-UNDO.
   DEFINE VARIABLE lcTimeList     AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE lcEntry        AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE lii            AS INTEGER            NO-UNDO.
   DEFINE VARIABLE lij            AS INTEGER            NO-UNDO.
   DEFINE VARIABLE liTimeEntries  AS INTEGER            NO-UNDO.

   ASSIGN
      liTime[1]     = TRUNCATE(iiGoalTime / 3600, 0) /* hour */
      liTime[2]     = TRUNCATE((iiGoalTime MOD 3600) / 60, 0) /* minute */
      liTime[3]     = iiGoalTime MOD 60 /* second */
      liTimeEntries = NUM-ENTRIES(icTimeText,":")
      .

   /* First expand the time list */
   DO lii = 1 TO liTimeEntries:
      
      lcEntry = ENTRY(lii,icTimeText,":").
      
      IF lcEntry = "*"
      THEN lcTimeList[lii] = fExpandRange("0-" + STRING(liTimeMaxValue[lii]),"/").
      ELSE lcTimeList[lii] = fExpandRange(lcEntry,"/").

      IF lcTimeList[lii] = ""
      THEN RETURN 86400.
   END.

   lii = 1.   

   DO WHILE lii <= liTimeEntries:

      lij = fGetValueAtMost(liTime[lii], lcTimeList[lii], "/").
      
      /* No suitable value found on the list */
      IF lij = ? 
      THEN DO:
         /* If there is no hour which would give a time
            less or equal to optimal hour it is not
            possible to get a time value */
         IF lii = 1
         THEN RETURN 86400.

         /* If there is no minute which would give a time
            less or equal to optimal minute we need to
            get back to hour and mark minutes and seconds
            to accept maximum values. */
         IF lii = 2
         THEN ASSIGN
                liTime[2] = liTimeMaxValue[2]
                liTime[3] = liTimeMaxValue[3].
                
                  
         /* If there is no second which would give a time
            less or equal to optimal second we need to
            get back to minute and mark seconds
            to accept a maximum value. */
         IF lii >= 2
         THEN liTime[3] = liTimeMaxValue[3].

         /* Lets get back to hour or minute and use one
            value less than previous value */
         ASSIGN         
            lii = lii - 1
            liTime[lii] = liTime[lii] - 1.
            
      END.

      /* There was a value but it is less than the optimal value.
         Lets use the value less than optimal and set following
         time values to maximum and go to next value */
      ELSE IF lij < liTime[lii]
      THEN DO:
         liTime[lii] = lij.
         
         /* Minutes and seconds to maximum values */                  
         IF lii = 1
         THEN ASSIGN
                 liTime[2] = liTimeMaxValue[2]
                 liTime[3] = liTimeMaxValue[3].
         
         /* Seconds to maximum values */
         IF lii = 2 
         THEN liTime[3] = liTimeMaxValue[3].                

         lii = lii + 1.

      END.               
      
      /* There was an optimal value => just go to next value */
      ELSE lii = lii + 1.
      
   END.

   RETURN liTime[1] * 3600 + liTime[2] * 60 + liTime[3].
   
END.


/* 
   Function will convert time to seconds after midnight
   For example:
      16:21:59  => 58919
      1:01,2:10 => 3660,7800
      1,8,1:01  => 3600,28800,3660
*/
FUNCTION fConvertListToTime RETURNS CHARACTER
   (icText AS CHARACTER):

   IF INDEX(icText,",") = 0
   THEN DO:
      IF INDEX(icText,"*") > 0 OR INDEX(icText,"/") > 0
      THEN RETURN "86400".
      ELSE RETURN STRING(INTEGER(ENTRY(1,icText,":")) * 3600 + 
               ( IF NUM-ENTRIES(icText,":") > 1
                 THEN INTEGER(ENTRY(2,icText,":")) * 60
                 ELSE 0 ) + 
               ( IF NUM-ENTRIES(icText,":") > 2
                 THEN INTEGER(ENTRY(3,icText,":"))
                 ELSE 0 )). 
   END.

   RETURN fConvertListToTime(SUBSTRING(icText,1, INDEX(icText,",") - 1)) + "," + fConvertListToTime(SUBSTRING(icText,INDEX(icText,",") + 1)).

END FUNCTION.


/* 
   Function will convert time to seconds after midnight
   For example:
      16:21:59  => 58919
      1:01,2:10 => 3660,7800
      1,8,1:01  => 3600,28800,3660
      
   This version can also use fGetClosestTime if needed 
*/
FUNCTION fConvertListToTimeWithGoal RETURNS CHARACTER
   (icText     AS CHARACTER,
    iiGoalTime AS INTEGER):

   IF INDEX(icText,",") = 0
   THEN DO:
      IF INDEX(icText,"*") > 0 OR INDEX(icText,"/") > 0
      THEN RETURN STRING(fGetClosestTime(icText,iiGoalTime)).
      ELSE RETURN STRING(INTEGER(ENTRY(1,icText,":")) * 3600 + 
               ( IF NUM-ENTRIES(icText,":") > 1
                 THEN INTEGER(ENTRY(2,icText,":")) * 60
                 ELSE 0 ) + 
               ( IF NUM-ENTRIES(icText,":") > 2
                 THEN INTEGER(ENTRY(3,icText,":"))
                 ELSE 0 )). 
   END.
   
   RETURN fConvertListToTimeWithGoal(SUBSTRING(icText,1, INDEX(icText,",") - 1),iiGoalTime) + "," + fConvertListToTimeWithGoal(SUBSTRING(icText,INDEX(icText,",") + 1),iiGoalTime).

END FUNCTION.

/*
   Function tells is it allowed to do a dump for a date idaDay
   
   Function requires data from DFTimeTable
   
   icValidDays     = DFTimeTable.DumpDay
   icValidWeekDays = DFTimeTable.DumpWeekDay
   
   ilOnlyWorkDays parameter is at value yes if only working days is allowed 
*/
FUNCTION fDayAllowed RETURNS LOGICAL
   (idaDay AS DATE,
    icValidDays AS CHARACTER,
    icValidWeekDays AS CHARACTER,
    ilOnlyWorkDays  AS LOGICAL):
       
   IF ilOnlyWorkDays AND 
      NOT fIsWorkingDay(idaDay)
   THEN RETURN FALSE.
   
   ASSIGN
      icValidDays     = fExpandRange(icValidDays,",")
      icValidWeekDays = fExpandRange(icValidWeekDays,",")
      .
   
   IF icValidDays = "" AND icValidWeekDays = ""
   THEN RETURN FALSE.

   /* Is day in valid day list */
   IF icValidDays > ""   AND
      icValidDays NE "*" AND
      LOOKUP(STRING(DAY(idaDay)),icValidDays) = 0
   THEN RETURN FALSE.
   
   /* Is day in valid weekday list */
   IF icValidWeekDays > ""    AND
       icValidWeekDays NE "*" AND
       LOOKUP(STRING(WEEKDAY(idaDay)),icValidWeekDays) = 0
   THEN RETURN FALSE.

   RETURN TRUE.

END FUNCTION.


/*
   Function creates ttTime temp-table record
   and returns TRUE if a new record was created otherwise it returns FALSE
*/
FUNCTION fDoTTTime RETURNS LOGICAL
   (iiTime AS INTEGER):

   FIND ttTime WHERE ttTime.IntTime = iiTime NO-ERROR.
   
   IF AVAILABLE ttTime
   THEN RETURN FALSE.
   
   CREATE ttTime.
   ttTime.IntTime = iiTime.

   RETURN TRUE.

END FUNCTION.


/*
   This internal function gets time entry icEntry which contains "*" or "/" character
   for example "*:0/15/30/45:0"
   
   iiFromTime tells a minumum time which is allowed
   
   iiMaxEventCount tells how many ttTime records it is allowed to create
   
   Function returns an updated iiMaxEventCount value
*/
FUNCTION fCreateTTTimeFromSpecialTimeEntry RETURNS INTEGER
   (icEntry AS CHARACTER,
    iiFromTime AS INTEGER,
    iiMaxEventCount AS INTEGER):

   DEFINE VARIABLE liTimeMaxValue AS INTEGER   EXTENT 3 INITIAL [23,59,59] NO-UNDO.
   DEFINE VARIABLE lcTimeList     AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE lcEntry        AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE liTime         AS INTEGER            NO-UNDO.

   DEFINE VARIABLE lii AS INTEGER NO-UNDO.
   DEFINE VARIABLE lij AS INTEGER NO-UNDO.
   DEFINE VARIABLE lik AS INTEGER NO-UNDO.

   /* First expand the time list */
   DO lii = 1 TO  NUM-ENTRIES(icEntry,":"):
      
      lcEntry = ENTRY(lii,icEntry,":").
      
      IF lcEntry = "*"
      THEN lcTimeList[lii] = fExpandRange("0-" + STRING(liTimeMaxValue[lii]),"/").
      ELSE lcTimeList[lii] = fExpandRange(lcEntry,"/").

      IF lcTimeList[lii] = ""
      THEN RETURN iiMaxEventCount.
      
   END.         

   DO lii = 1 TO NUM-ENTRIES(lcTimeList[1],"/"):
      DO lij = 1 TO NUM-ENTRIES(lcTimeList[2],"/"):
         DO lik = 1 TO NUM-ENTRIES(lcTimeList[3],"/"):
            
            liTime = INTEGER(ENTRY(lii,lcTimeList[1],"/")) * 3600 +
                     INTEGER(ENTRY(lij,lcTimeList[2],"/")) * 60   +
                     INTEGER(ENTRY(lik,lcTimeList[3],"/")).
            
            IF liTime < iiFromTime
            THEN NEXT.
            
            IF fDoTTTime(liTime)
            THEN iiMaxEventCount = iiMaxEventCount - 1.
            
            IF iiMaxEventCount <= 0
            THEN RETURN iiMaxEventCount.
         
         END.
      END.
   END.

   RETURN iiMaxEventCount.

END FUNCTION.

/*
   Function tells which is the maximum allowed dump time for the idaGoalDate
   which is not past of the iiGoalTime. If there is none the function returns
   question mark.
   
   idaGoalDate = the date to where we are trying to find run time
   iiGoalTime  = the most suitable dump time (this is the limit and the dump time cannot exceed this)
   idaLastRunDay = the day when the dump was last in run
   iiLastTime    = the time when the dump was last in run
   icValidDays     = DFTimeTable.DumpDay
   icValidWeekDays = DFTimeTable.DumpWeekDay
   icValidTimes = DFTimeTable.DumpTime
   ilOnlyWorkDays = Is on value TRUE if only working days are allowed     
*/
FUNCTION fMaxSuitableRunTime RETURNS INTEGER
   (idaGoalDate     AS DATE,
    iiGoalTime      AS INTEGER,
    idaLastRunDay   AS DATE,
    iiLastTime      AS INTEGER, 
    icValidDays     AS CHARACTER,
    icValidWeekDays AS CHARACTER,
    icValidTimes    AS CHARACTER,
    ilOnlyWorkDays  AS LOGICAL):

   IF icValidTimes = "" OR icValidTimes = ?
   THEN RETURN ?. 

   IF NOT fDayAllowed(idaGoalDate, icValidDays, icValidWeekDays, ilOnlyWorkDays)
   THEN RETURN ?.

   DEFINE VARIABLE liMaxSuitable AS INTEGER INITIAL -1 NO-UNDO.
   DEFINE VARIABLE lii           AS INTEGER NO-UNDO.
   DEFINE VARIABLE liTime        AS INTEGER NO-UNDO.
   
   icValidTimes = fConvertListToTimeWithGoal(icValidTimes, iiGoalTime).
      
   DO lii = 1 TO NUM-ENTRIES(icValidTimes):
         
      liTime = INTEGER(ENTRY(lii,icValidTimes)).
      
      IF idaLastRunDay = idaGoalDate AND liTime <= iiLastTime
      THEN NEXT.
      
      IF liTime > iiGoalTime
      THEN NEXT.
      
      liMaxSuitable = MAXIMUM(liMaxSuitable, liTime).
      
   END.
   
   IF liMaxSuitable = -1
   THEN RETURN ?.
   
   RETURN liMaxSuitable.
      
END FUNCTION.

&ENDIF