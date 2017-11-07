/* create_eventlog.i    10.06.08/aam (from lib/EventLog.i)

   lighter version just for adding an entry to EventLog from creation event,
   no need to clean dynamic objects from memory afterwards

   in caller needed:
   DEF VAR lhTable AS HANDLE NO-UNDO.
   lhTable = BUFFER TableCreated:HANDLE.
*/   

&IF "{&CreateEventLog}" NE "YES" 
&THEN

&GLOBAL-DEFINE CreateEventLog YES

FUNCTION fGetEventField RETURNS CHARACTER
   (ihField  AS HANDLE,
    iiExtent AS INTEGER):
    
    DEFINE VARIABLE lcField AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lda     AS DATE       NO-UNDO.

    IF ihField:BUFFER-VALUE[iiExtent] = ? THEN RETURN '<Null>'.

    CASE ihField:DATA-TYPE:
        
        WHEN 'LOGICAL' 
        THEN lcField = STRING(ihField:BUFFER-VALUE[iiExtent],'YES/NO'). 
        
        WHEN 'DATE'    
        THEN ASSIGN
            lda     = ihField:BUFFER-VALUE[iiExtent]
            lcField = STRING(YEAR(lda))       + '/'
                    + STRING(MONTH(lda),'99') + '/'
                    + STRING(DAY(lda),'99').
        WHEN 'DECIMAL'  /* European format times with dot to event log */
        THEN lcField = REPLACE(STRING(ihField:BUFFER-VALUE[iiExtent]),",",".").
        OTHERWISE      
        ASSIGN lcField = STRING(ihField:BUFFER-VALUE[iiExtent]).
        
    END CASE.

    RETURN lcField.

END FUNCTION.

FUNCTION fEventKeyFields RETURNS CHARACTER
   (ihBuffer AS HANDLE):

    DEFINE VARIABLE lcKey AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lii   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcIndexInformation AS CHARACTER  NO-UNDO.

    DEF BUFFER bEventLogConf FOR EventLogConf.
    
    /* first check if keys have been defined manually */
    FOR FIRST bEventLogConf NO-LOCK WHERE
              bEventLogConf.TableName  = ihBuffer:TABLE AND
              bEventLogConf.ConfigType = "KeyFields"   AND
              bEventLogConf.ToDate    >= TODAY         AND
              bEventLogConf.FromDate  <= TODAY:
       lcIndexInformation = bEventLogConf.ConfigValue.
    END.
    
    IF lcIndexInformation = "" THEN DO:

       /* find primary index */
       DO lii = 1 TO 100:
          lcc = ihBuffer:INDEX-INFORMATION[lii].
          IF lcc = ? THEN RETURN " ???".
          IF ENTRY(3,lcc) = '1' THEN LEAVE.    /* value "1" = primary */
       END.

       DO lii = 5 TO NUM-ENTRIES(lcc) BY 2:
          lcIndexInformation = lcIndexInformation + 
                               (IF lcIndexInformation > "" THEN "," ELSE "") +
                               ENTRY(lii,lcc).
       END.
    END.

    /* make key */
    DO lii = 1 TO NUM-ENTRIES(lcIndexInformation):
        lcc = ENTRY(lii,lcIndexInformation). /* name of the field */
        IF lcc = '' THEN NEXT. 
        lcKey = lcKey + CHR(255) + lcc.
    END.                                                    

    RETURN TRIM(lcKey,CHR(255)).

END FUNCTION.

FUNCTION fEventKeyValues RETURNS CHARACTER
   (ihBuffer AS HANDLE,
    icFields AS CHAR):

    DEFINE VARIABLE lcKey    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lhh      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lii      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc      AS CHARACTER  NO-UNDO.

    /* make key */
    DO lii = 1 TO NUM-ENTRIES(icFields,CHR(255)):
        lcc = ENTRY(lii,icFields,CHR(255)).
        IF lcc = '' THEN NEXT. 
        lhh = ihBuffer:BUFFER-FIELD(lcc). /* find buffer-field handle */
        lcKey = lcKey + CHR(255) + fGetEventField(lhh,0).
    END.                                                    

    RETURN TRIM(lcKey,CHR(255)).

END FUNCTION.


FUNCTION fGetEventKey RETURNS CHARACTER
   (ihBuffer AS HANDLE):

    DEFINE VARIABLE lcFields AS CHARACTER  NO-UNDO.

    lcFields = fEventKeyFields(ihBuffer).

    RETURN fEventKeyValues(ihBuffer,
                           lcFields).

END FUNCTION.


FUNCTION fMakeCreateEvent RETURNS LOGIC
   (INPUT ihBuffer AS HANDLE,
    INPUT icFields AS CHAR,
    INPUT icUserCode AS CHAR,
    INPUT icMemo AS CHAR):

   DEF VAR liField AS INT    NO-UNDO.
   DEF VAR lhField AS HANDLE NO-UNDO.

   CREATE EventLog.
   ASSIGN
      EventLog.EventDate      = TODAY
      EventLog.EventTime      = STRING(TIME,"HH:MM:SS")
      EventLog.UserCode       = icUserCode
      EventLog.Memo           = icMemo
      EventLog.Action         = 'Create'.
   ASSIGN
      EventLog.Key            = fGetEventKey(ihBuffer)
      EventLog.TableName      = ihBuffer:TABLE.
        
   IF icFields > "" THEN DO liField = 1 TO NUM-ENTRIES(icFields):
   
      lhField = ihBuffer:BUFFER-FIELD(ENTRY(liField,icFields)).
      
      EventLog.DataValues = EventLog.DataValues + 
                            (IF liField > 1 THEN CHR(255) ELSE "") + 
                            lhField:NAME + CHR(255) + 
                            ""           + CHR(255) + 
                            lhField:BUFFER-VALUE. 
   END.   
    
END FUNCTION.

&ENDIF
