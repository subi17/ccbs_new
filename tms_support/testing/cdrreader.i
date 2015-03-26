/* ----------------------------------------------------------------------
  MODULE .......: cdrreader.i 
  TASK .........: cdr functions for testing
  APPLICATION ..: TMS
  AUTHOR .......: kariaika & ilkkasav
  CREATED ......: 28.01.2015
  Version ......: xfera
----------------------------------------------------------------------- */

/*ilkkasav&kariaika: remember to change the directories before releasing*/
{tmsconst.i}

/*Function counts milliseconds to format HHMMSS*/
FUNCTION fMTimetoString RETURN CHARACTER
  (INPUT mTimeValue AS INTEGER):

   DEFINE VARIABLE litempTime AS INTEGER NO-UNDO.
   DEFINE VARIABLE liHours AS INTEGER NO-UNDO.
   DEFINE VARIABLE liMinutes AS INTEGER NO-UNDO.
   DEFINE VARIABLE liSeconds AS INTEGER NO-UNDO.
   
   litempTime = mTimeValue / 1000.
   liSeconds = litempTime MOD 60.
   litempTime = (litempTime - liSeconds) / 60.
   liMinutes = litempTime MOD 60.
   liHours = (litempTime - liMinutes) / 60.
   
   RETURN STRING(liHours, "99") + STRING(liMinutes, "99" ) + 
          STRING(liSeconds, "99").

END FUNCTION.


/*Function returns full hours of given minutes.*/
FUNCTION fMinutesToHours RETURN INTEGER
      ( piMinutes AS INTEGER): 
DEF VAR liHours AS INTEGER NO-UNDO.
  IF (piMinutes > 59) THEN DO:
    piMinutes = piMinutes - 30.
    liHours = piMinutes / 60.
    RETURN lihours.
  END.
  ELSE DO:
    RETURN 0.
  END.

END FUNCTION.


/*Function checks that time format is correct.*/
FUNCTION fCheckTimeFormat RETURN LOGICAL
      ( pcTime AS CHARACTER): 
DEF VAR liHours AS INTEGER NO-UNDO.
DEF VAR liMins AS INTEGER NO-UNDO.
   IF pcTime > "" THEN DO:
      IF NUM-ENTRIES(pcTime,":") NE 2 THEN DO:
         MESSAGE "Incorrect time format:" + pcTime VIEW-AS ALERT-BOX ERROR.         
         RETURN FALSE.
      END.
      ASSIGN
         liHours = INT(ENTRY(1,pcTime,":"))
         liMins = INT(ENTRY(2,pcTime,":")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         MESSAGE "Incorrect time format:" + pcTime VIEW-AS ALERT-BOX ERROR.         
         RETURN FALSE.
      END.
      IF (liHours < 0 OR liHours > 23) OR
         (liMins < 0 OR liMins > 59) THEN DO:
         MESSAGE "Incorrect time format:" + pcTime VIEW-AS ALERT-BOX ERROR.         
         RETURN FALSE.
      END.
    END.
    RETURN TRUE.
END FUNCTION.


/*Function adds piDuration to pcOldTime. Duration is minutes*/
FUNCTION fCreateTimeValue RETURN CHARACTER
      ( pcOldTime AS CHARACTER,
        piDuration AS INT): 
DEF VAR liHours AS INT NO-UNDO. 
DEF VAR liAddHours AS INT NO-UNDO.
DEF VAR liMins AS INT NO-UNDO. 
DEF VAR lcNewTime AS CHARACTER NO-UNDO.

    
    IF(NOT fCheckTimeFormat(pcOldTime)) THEN DO: 
        NEXT.
    END.
    /*Generate new timestamp*/
    ASSIGN
         liHours = INT(ENTRY(1,pcOldTime,":"))
         liMins = INT(ENTRY(2,pcOldTime,":"))
         liMins = liMins + piDuration
         liAddHours = fMinutesToHours(liMins)
         liHours = liHours + liAddHours
         liMins = liMins - (liAddHours * 60) NO-ERROR.
      
    /*Fill data to cNewTime, format hh:mm*/
    /*Hours*/
    IF(liHours < 10) THEN DO:
        lcNewTime = "0" + STRING(liHours).
    END. 
    ELSE DO:
        lcNewTime = STRING(liHours).
    END.
        lcNewTime = lcNewTime + ":".
    /**Minutes*/  
    IF(liMins < 10) THEN DO:
        lcNewTime = lcNewTime + "0" + STRING(liMins).
    END. 
    ELSE DO:
        lcNewTime = lcNewTime + STRING(liMins).
    END.
        
    /*Ensure that new modified value is correct*/
    IF(NOT fCheckTimeFormat(lcNewTime))  THEN DO: 
        NEXT.
    END.
  
    RETURN lcNewTime.  

END FUNCTION.

/*Function checks that is the program running in staging or development
environment.*/

FUNCTION fIsInInternalEnv RETURNS LOGICAL:

DEF VAR lcHostName AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF(LOOKUP(lcHostName,{&HOSTNAME_DEVEL})  = 0  AND 
   LOOKUP(lcHostName,{&HOSTNAME_STAGING}) = 0) THEN DO:
   RETURN FALSE.
END.

RETURN TRUE.


END FUNCTION.
