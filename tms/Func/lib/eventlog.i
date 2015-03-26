&IF "{&EVENTLOG_I}" NE "YES"
&THEN
&GLOBAL-DEFINE EVENTLOG_I YES

&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : lib/eventlog.i
    Purpose     : Make event's to log each change of buffer

    Syntax      :

    Description :

    Author(s)   : TaH
    Created     : 12.4.2002
    Notes       : 30.12.2002/aam in Create event no field values to log,
                                 in Delete event all field values to log
                  08.08.2005/aam new function fCleanEventObjects 
                  18.10.2005/aam case sensitivity possibility, 
                                 set for Customer's address fields


How to use this library?

Make following steps

1. put this library to definition block

2. to your program...

2.1 user-code handling... preprosessor STAR_EVENT_USER
&GLOBAL-DEFINE STAR_EVENT_USER <shared variable>
&GLOBAL-DEFINE STAR_EVENT_USER DYNAMIC-FUNCTION('getStarEnviroment,?,'usercode')

2.2 Inialize
DEFINE VARIABLE lhAsiakas AS HANDLE     NO-UNDO.
lhAsiakas = BUFFER asiakas:HANDLE. /* Buffer handle of asiakas */
RUN StarEventInitialize ( lhAsiakas ). /* initialize asiakas event */
You can hava maximum 10 buffer per single program


2.3 How to "save" old buffer
RUN StarEventSetOldBuffer ( lhAsiakas ).
This just copy buffer to dynamic temp-table

2.4 How to make event per each action

2.4.1 Create
Last line of create
RUN StarEventMakeCreateEvent (lhAsiakas).

2.4.2 Modify
Last line of modify
RUN StarEventMakeModifyEvent ( lhAsiakas ).

2.4.3 Delete
Last line before delete clause
RUN StarEventMakeDeleteEvent (lhAsiakas).


26.4.2002 TaH datavalues and key go's without formatting. ? value is stored by value '<Null>'

*/

&IF DEFINED(STAR_EVENT_USER) = 0 
&THEN
    &MESSAGE "Preprosessor STAR_EVENT_USER not defined"
&ENDIF

&SCOPED-DEFINE NUM_BUFFERS 10

DEFINE VARIABLE ghEventSource       AS HANDLE EXTENT {&NUM_BUFFERS} NO-UNDO.
DEFINE VARIABLE ghBEventSource      AS HANDLE EXTENT {&NUM_BUFFERS} NO-UNDO.

DEFINE VARIABLE gcEventTableNames   AS CHARACTER  NO-UNDO.

/* tables and fields that should be handled as case sensitive */
DEFINE VARIABLE gcCaseTables AS CHAR NO-UNDO.
DEFINE VARIABLE gcCaseFields AS CHAR NO-UNDO.

ASSIGN gcCaseTables = "Customer¤{&EventCaseTable}"
       gcCaseFields = "CustName,Address,PostOffice¤{&EventCaseField}".

&GLOBAL-DEFINE STAR-DELIMITER CHR(255)

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStarEventField Include 
FUNCTION getStarEventField RETURNS CHARACTER
  ( ihField  AS HANDLE,
    iiExtent AS INTEGER 
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStarEventKey Include 
FUNCTION getStarEventKey RETURNS CHARACTER
  ( ihBuffer AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarEventInitialize Include 
PROCEDURE StarEventInitialize :
/*------------------------------------------------------------------------------
  Purpose:     Initialize dynamic temp-table to buffer
  Parameters:  1. Buffer handle
  Notes:       Routine can handle maximum 10 buffers per program.
               To enlarge number of buffer, just change value of NUM_BUFFERS in 
               definitions block
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE     NO-UNDO.

    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.

    lii = LOOKUP(ihBuffer:TABLE,gcEventTableNames).

    IF lii > 0 AND VALID-HANDLE(ghEventSource[lii]) THEN RETURN.

    IF lii = 0 THEN ASSIGN
        gcEventTableNames   = TRIM( gcEventTableNames 
                                    + ','
                                    + ihBuffer:TABLE
                                    ,','
                                  )
        lii                 = LOOKUP(ihBuffer:TABLE,gcEventTableNames).

    IF VALID-HANDLE(ghEventSource[lii]) 
    THEN DELETE OBJECT ghEventSource[lii].

    CREATE TEMP-TABLE ghEventSource[lii].

    ghEventSource[lii]:CREATE-LIKE(ihBuffer).
    ghEventSource[lii]:TEMP-TABLE-PREPARE('ttEventSouce').

    ghBEventSource[lii] = ghEventSource[lii]:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarEventMakeCreateEvent Include 
PROCEDURE StarEventMakeCreateEvent :
/*------------------------------------------------------------------------------
  Purpose:     Make create event
  Parameters:  1. buffer handle
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.

    RUN StarEventMakeCreateEventWithMemo(ihBuffer, {&STAR_EVENT_USER}, "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE StarEventMakeCreateEventWithMemo :
/*------------------------------------------------------------------------------
  Purpose:     Make create event
  Parameters:  1. buffer handle
               2. Usercode
               3. Memo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER icUserCode AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER icMemo AS CHAR NO-UNDO.

    CREATE eventlog.
    ASSIGN
        eventlog.eventdate      = TODAY
        eventlog.eventtime      = STRING(TIME,"HH:MM:SS")
        eventlog.usercode       = icUserCode 
        eventlog.Memo           = icMemo
        eventlog.action         = 'Create'.
    ASSIGN
        eventlog.KEY            = getStarEventKey(ihBuffer)
        eventlog.tablename      = ihBuffer:TABLE.
       
    FIND FIRST TriggerConf WHERE 
               TriggerConf.TriggerConfID = ihBuffer:TABLE AND 
               TriggerConf.EventRule    > 0               AND 
               TriggerConf.ValidTo      >= Today          AND 
               TriggerConf.ValidFrom    <= Today NO-LOCK NO-ERROR.
                                                                    
    IF AVAIL TriggerConf THEN DO:
                                                                                 
       CREATE TriggerEvent.
       ASSIGN 
           TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
           TriggerEvent.TriggerConfID  = ihBuffer:TABLE
           TriggerEvent.EventSource    = "CREATE"
           TriggerEvent.Created        = DateTime(Today,mtime)
           TriggerEvent.TableID        = ihBuffer:recid
           TriggerEvent.TableName      = Eventlog.Tablename 
           TriggerEvent.KeyValue       = EventLog.Key
           TriggerEvent.ChangedFields  = EventLog.ModifiedFields
           TriggerEvent.ChangedValues  = EventLog.DataValues.
    END.

END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarEventMakeDeleteEvent Include 
PROCEDURE StarEventMakeDeleteEvent :
/*------------------------------------------------------------------------------
  Purpose:     Make delete event
  Parameters:  1. buffer handle
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
    
    RUN StarEventMakeDeleteEventWithMemo(ihBuffer, {&STAR_EVENT_USER}, "").
                                                                              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE StarEventMakeDeleteEventWithMemo :
/*------------------------------------------------------------------------------
  Purpose:     Make delete event
  Parameters:  1. buffer handle
               2. Usercode
               3. Memo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER icUserCode AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER icMemo AS CHAR NO-UNDO.

    DEFINE VARIABLE liField     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE liExtent    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lii         AS INTEGER    NO-UNDO.

    DEFINE VARIABLE lcDeleteFields      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcFieldFormats      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcDataValues        AS CHARACTER  NO-UNDO.
 
    DEFINE VARIABLE lhDelBuffer AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhDelField  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lcDelValues AS CHARACTER  NO-UNDO.
 
    ASSIGN
        lii         = LOOKUP(ihBuffer:TABLE,gcEventTableNames)
        lhDelBuffer = ihBuffer
        .

    DO liField = 1 TO lhDelBuffer:NUM-FIELDS:

        ASSIGN lhDelField = lhDelBuffer:BUFFER-FIELD(liField).

        IF lhDelField:EXTENT = 0 
        THEN DO: 
            ASSIGN
                lcDeleteFields = lcDeleteFields
                                 + ','
                                 + lhDelField:NAME
                lcFieldFormats   = lcFieldFormats
                                 + ','
                                 + lhDelField:FORMAT
                lcDataValues     = lcDataValues
                                 + {&STAR-DELIMITER}
                                 + lhDelField:NAME 
                                 + {&STAR-DELIMITER}
                                 + getStarEventField(lhDelField,0)
                              /* empty value.. compatibl~y to modified event */
                                 + {&STAR-DELIMITER}. 
        END.
        ELSE 
        DO liExtent = 1 TO lhDelField:EXTENT:
            ASSIGN
                lcDeleteFields = lcDeleteFields
                                 + ','
                                 + lhDelField:NAME + "[" + STRING(liExtent) 
                                 + "~]"
                lcFieldFormats   = lcFieldFormats
                                 + ','
                                 + lhDelField:FORMAT.
                lcDataValues     = lcDataValues
                                 + {&STAR-DELIMITER}
                                 + lhDelField:NAME + "[" + STRING(liExtent) 
                                 + "~]" + {&STAR-DELIMITER}
                                 + {&STAR-DELIMITER} 
                           /* empty value.. compatibl~y to modified event */ 
                                 + getStarEventField(lhDelField,liExtent).
        END.                                        

    END.

    CREATE eventlog.
    ASSIGN
        eventlog.eventdate  = TODAY
        eventlog.eventtime  = STRING(TIME,"HH:MM:SS")
        eventlog.usercode   = icUserCode
        eventlog.memo       = icMemo
        eventlog.action     = 'Delete'.
    ASSIGN
        eventlog.KEY            = getStarEventKey(ihBuffer)
        eventlog.tablename      = ihBuffer:TABLE
        eventlog.ModifiedFields = TRIM(lcDeleteFields,',')
        eventlog.FieldFormats   = TRIM(lcFieldFormats,',')
        eventlog.DataValues     = LEFT-TRIM(lcDataValues  ,{&STAR-DELIMITER}).

    FIND FIRST TriggerConf WHERE 
               TriggerConf.TriggerConfID = ihBuffer:TABLE AND 
               TriggerConf.EventRule    > 0               AND
               TriggerConf.ValidTo      >= Today          AND 
               TriggerConf.ValidFrom    <= Today NO-LOCK NO-ERROR.
                                                     
    IF AVAIL TriggerConf THEN DO:
                                                                  
       CREATE TriggerEvent.
       ASSIGN 
          TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
          TriggerEvent.TriggerConfID  = ihBuffer:TABLE
          TriggerEvent.EventSource    = "DELETE"
          TriggerEvent.Created        = DateTime(Today,mtime)
          TriggerEvent.TableID        = ihBuffer:recid
          TriggerEvent.TableName      = Eventlog.Tablename 
          TriggerEvent.KeyValue       = EventLog.Key
          TriggerEvent.ChangedFields  = EventLog.ModifiedFields
          TriggerEvent.ChangedValues  = Eventlog.DataValues.
                                                                     
    END.
                                                                              
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarEventMakeModifyEvent Include 
PROCEDURE StarEventMakeModifyEvent :
/*------------------------------------------------------------------------------
  Purpose:     Make modify event
  Parameters:  1. Buffer handle
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
             
    RUN StarEventMakeModifyEventWithMemo(ihBuffer, {&STAR_EVENT_USER}, "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE StarEventMakeModifyEventWithMemo :
/*------------------------------------------------------------------------------
  Purpose:     Make modify event
  Parameters:  1. Buffer handle
               2. Usercode
               3. Memo
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
    DEFINE INPUT  PARAMETER icUserCode AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER icMemo AS CHAR NO-UNDO.

    DEFINE VARIABLE lhOldBuffer AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhNewBuffer AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhOldField  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhNewField  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lcOldValue  AS CHARACTER  NO-UNDO CASE-SENSITIVE.
    DEFINE VARIABLE lcNewValue  AS CHARACTER  NO-UNDO CASE-SENSITIVE.
                                
    DEFINE VARIABLE liField     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE liExtent    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lii         AS INTEGER    NO-UNDO.

    DEFINE VARIABLE lcModifiedFields    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcFieldFormats      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcDataValues        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE liCaseCheck         AS INT        NO-UNDO.
    DEFINE VARIABLE lcCaseFields        AS CHAR       NO-UNDO.
    
    ASSIGN
        lii         = LOOKUP(ihBuffer:TABLE,gcEventTableNames)
        lhOldBuffer = ghBEventSource[lii]
        lhNewBuffer = ihBuffer
        liCaseCheck = LOOKUP(ihBuffer:TABLE,gcCaseTables,"¤")
        .

    /* fields that should be handled as case sensitive */
    IF liCaseCheck > 0 AND NUM-ENTRIES(gcCaseFields,"¤") >= liCaseCheck
    THEN lcCaseFields = ENTRY(liCaseCheck,gcCaseFields,"¤").

    DO liField = 1 TO lhNewBuffer:NUM-FIELDS:

        ASSIGN
            lhOldField = lhOldBuffer:BUFFER-FIELD(liField)
            lhNewField = lhNewBuffer:BUFFER-FIELD(liField)
            .

        IF lhOldField:EXTENT = 0 
        THEN DO: 
            
            IF liCaseCheck > 0                    AND 
               lhNewField:DATA-TYPE = "CHARACTER" AND
               LOOKUP(lhNewField:NAME,lcCaseFields) > 0
            THEN DO:
               ASSIGN lcOldValue = lhOldField:BUFFER-VALUE
                      lcNewValue = lhNewField:BUFFER-VALUE.
               IF lcOldValue = lcNewValue THEN NEXT. 
            END.
                
            ELSE IF lhOldField:BUFFER-VALUE = lhNewField:BUFFER-VALUE 
            THEN NEXT.

            ASSIGN
                lcModifiedFields = lcModifiedFields
                                 + ','
                                 + lhNewField:NAME
                lcFieldFormats   = lcFieldFormats
                                 + ','
                                 + lhNewField:FORMAT
                lcDataValues     = lcDataValues
                                 + {&STAR-DELIMITER}
                                 + lhNewField:NAME 
                                 + {&STAR-DELIMITER} 
                                 + getStarEventField(lhOldField,0)
                                 + {&STAR-DELIMITER} 
                                 + getStarEventField(lhNewField,0).
        END.
        ELSE 
        DO liExtent = 1 TO lhOldField:EXTENT:
            IF lhOldField:BUFFER-VALUE[liExtent] = lhNewField:BUFFER-VALUE[liExtent] THEN NEXT.
            ASSIGN
                lcModifiedFields = lcModifiedFields
                                 + ','
                                 + lhNewField:NAME + "[" + STRING(liExtent) + "]"
                lcFieldFormats   = lcFieldFormats
                                 + ','
                                 + lhNewField:FORMAT
                lcDataValues     = lcDataValues
                                 + {&STAR-DELIMITER}
                                 + lhNewField:NAME + "[" + STRING(liExtent) + "]" 
                                 + {&STAR-DELIMITER} + getStarEventField(lhOldField,liExtent)
                                 + {&STAR-DELIMITER} + getStarEventField(lhNewField,liExtent).
        END.                                                                                         

    END.

    IF lcModifiedFields = '' THEN RETURN. /* no change */

    CREATE eventlog.
    ASSIGN
        eventlog.eventdate      = TODAY
        eventlog.eventtime      = STRING(TIME,"HH:MM:SS")
        eventlog.usercode       = icUserCode
        eventlog.Memo           = icMemo
        eventlog.action         = 'Modify'.
    ASSIGN
        eventlog.KEY            = getStarEventKey(ihBuffer)
        eventlog.tablename      = ihBuffer:TABLE
        eventlog.ModifiedFields = TRIM(lcModifiedFields,',')
        eventlog.FieldFormats   = TRIM(lcFieldFormats  ,',')
        eventlog.DataValues     = LEFT-TRIM(lcDataValues    ,{&STAR-DELIMITER}).

    FIND FIRST TriggerConf WHERE 
               TriggerConf.TriggerConfID = EventLog.TableName AND 
               TriggerConf.EventRule    > 0                   AND
               TriggerConf.ValidTo      >= Today              AND 
               TriggerConf.ValidFrom    <= Today NO-LOCK NO-ERROR.
                                                     
    IF AVAIL TriggerConf THEN DO:
                                                                  
       CREATE TriggerEvent.
       ASSIGN 
          TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
          TriggerEvent.TriggerConfID  = ihBuffer:TABLE
          TriggerEvent.EventSource    = "MODIFY"
          TriggerEvent.Created        = DateTime(Today,mtime)
          TriggerEvent.TableID        = ihBuffer:recid
          TriggerEvent.TableName      = eventlog.tablename 
          TriggerEvent.KeyValue       = EventLog.Key
          TriggerEvent.ChangedFields  = EventLog.ModifiedFields
          TriggerEvent.ChangedValues  = EventLog.DataValues.
    END.
              
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarEventSetOldBuffer Include 
PROCEDURE StarEventSetOldBuffer :
/*------------------------------------------------------------------------------
  Purpose:     Save old values to temp-table 
  Parameters:  1. buffer handle
  Notes:       Only one record per time can be process
               That why temp-table have to empty first
               These values are used in StarEventMakeModifyEvent procedure
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihBuffer AS HANDLE     NO-UNDO.
    
    DEFINE VARIABLE lhB AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.

    ASSIGN
        lii = LOOKUP(ihBuffer:TABLE,gcEventTableNames)
        lhB = ghEventSource[lii]:DEFAULT-BUFFER-HANDLE.
    lhB:EMPTY-TEMP-TABLE().
    lhB:BUFFER-CREATE().
    lhB:BUFFER-COPY(ihBuffer).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStarEventField Include 
FUNCTION getStarEventField RETURNS CHARACTER
  ( ihField  AS HANDLE,
    iiExtent AS INTEGER 
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lcField AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lda AS DATE       NO-UNDO.

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
                                    
        OTHERWISE      
        ASSIGN lcField = STRING(ihField:BUFFER-VALUE[iiExtent]).
        
    END CASE.

    RETURN lcField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStarEventKey Include 
FUNCTION getStarEventKey RETURNS CHARACTER
  ( ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Make event-key for that buffer
    Notes:  Rutine try to find primary index and use it
    
    29.5.2002 TaH PrimaryKey is entry 3 .. not 2
    
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lcKey AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lhh AS HANDLE     NO-UNDO.

    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.

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
          IF ENTRY(3,lcc) = '1' THEN LEAVE. 
          /* value "1" = primary */
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
        IF lcc = '' THEN NEXT. /* some problem in that function */
        lhh = ihBuffer:BUFFER-FIELD(lcc). /* find buffer-field handle */
        lcKey   = lcKey 
                + {&STAR-DELIMITER}
                + getStarEventField(lhh,0).
    END.                                                    

    RETURN TRIM(lcKey,{&STAR-DELIMITER}).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* dynamic temp-table needs to be cleaned out, otherwise session ultimately 
   crashes */
FUNCTION fCleanEventObjects RETURNS LOGIC:

   DEF VAR liCleanCnt AS INT NO-UNDO.
   
   DO liCleanCnt = 1 TO {&NUM_BUFFERS}:
   
      IF VALID-HANDLE(ghEventSource[liCleanCnt]) 
      THEN DELETE OBJECT ghEventSource[liCleanCnt].
   END.
   
END FUNCTION.

&ENDIF

   


