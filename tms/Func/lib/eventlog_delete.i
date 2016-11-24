&IF "{&EventLog_DELETE_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE EventLog_DELETE_I YES

/*------------------------------------------------------------------------
    File        : EventLog_delete.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Nov 24 19:27:36 EET 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FUNCTION fGetStarEventField RETURNS CHARACTER
   (ihField  AS HANDLE,
    iiExtent AS INTEGER):
   
   IF ihField:BUFFER-VALUE[iiExtent] = ? THEN RETURN '<Null>'.

   CASE ihField:DATA-TYPE:
      
      WHEN 'LOGICAL' 
      THEN RETURN STRING(ihField:BUFFER-VALUE[iiExtent],'YES/NO'). 
      WHEN 'DATE'    
      THEN RETURN STRING(YEAR(ihField:BUFFER-VALUE[iiExtent])) + '/' +
                  STRING(MONTH(ihField:BUFFER-VALUE[iiExtent]),'99') + '/' +
                  STRING(DAY(ihField:BUFFER-VALUE[iiExtent]),'99').
   END CASE.

   RETURN STRING(ihField:BUFFER-VALUE[iiExtent]).

END FUNCTION.

FUNCTION fGetStarEventKey RETURNS CHARACTER
   (ihBuffer AS HANDLE):

   DEFINE VARIABLE lcKey AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE lhh AS HANDLE     NO-UNDO.

   DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
   DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE lcIndexInformation AS CHARACTER  NO-UNDO.

   DEF BUFFER bEventLogConf FOR EventLogConf.
    
   /* first check if keys have been defined manually */
   FOR FIRST bEventLogConf NO-LOCK WHERE
             bEventLogConf.TableName  = ihBuffer:TABLE AND
             bEventLogConf.ConfigType = "KeyFields"    AND
             bEventLogConf.ToDate    >= TODAY          AND
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
      lcKey = lcKey + CHR(255) + fGetStarEventField(lhh,0).
    END.                                                    

    RETURN TRIM(lcKey,CHR(255)).

END FUNCTION.


FUNCTION fStarEventMakeDeleteEventWithMemo RETURNS LOGICAL
   (ihBuffer AS HANDLE,
    icMemo AS CHARACTER):
/*------------------------------------------------------------------------------
  Purpose:     Make delete event
  Parameters:  1. buffer handle
               2. Usercode
               3. Memo
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE lcUserCode AS CHARACTER NO-UNDO.
   
   &IF "{&CommVarDef}" EQ "YES" 
   &THEN
      lcUserCode = katun.
   &ELSE
      lcUserCode = "bg_process".
   &ENDIF

   DEFINE VARIABLE liField     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE liExtent    AS INTEGER    NO-UNDO.

   DEFINE VARIABLE lcDeleteFields      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcFieldFormats      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE lcDataValues        AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE lhDelBuffer AS HANDLE     NO-UNDO.
   DEFINE VARIABLE lhDelField  AS HANDLE     NO-UNDO.
   DEFINE VARIABLE lcDelValues AS CHARACTER  NO-UNDO.

   DO liField = 1 TO ihBuffer:NUM-FIELDS:
      lhDelField = ihBuffer:BUFFER-FIELD(liField).

      IF lhDelField:EXTENT = 0 
      THEN ASSIGN
              lcDeleteFields = lcDeleteFields + ',' + lhDelField:NAME
              lcFieldFormats   = lcFieldFormats + ',' + lhDelField:FORMAT
              lcDataValues     = lcDataValues + CHR(255) + lhDelField:NAME + 
                                 CHR(255) + fGetStarEventField(lhDelField,0) +
                                 /* empty value.. compatibly to modified event */
                                 CHR(255).
      ELSE DO liExtent = 1 TO lhDelField:EXTENT:
         ASSIGN
            lcDeleteFields = lcDeleteFields + ',' + lhDelField:NAME + "[" +
                             STRING(liExtent) + "~]"
            lcFieldFormats = lcFieldFormats + ',' + lhDelField:FORMAT
            lcDataValues   = lcDataValues + CHR(255) + lhDelField:NAME + "[" +
                             STRING(liExtent) + "~]" + CHR(255) + CHR(255) +
                             /* empty value.. compatibly to modified event */ 
                             fGetStarEventField(lhDelField,liExtent).
      END.
   END.

   CREATE EventLog.
   ASSIGN
      EventLog.eventdate      = TODAY
      EventLog.eventtime      = STRING(TIME,"HH:MM:SS")
      EventLog.usercode       = lcUserCode
      EventLog.memo           = icMemo
      EventLog.action         = 'Delete'
      EventLog.KEY            = fGetStarEventKey(ihBuffer)
      EventLog.tablename      = ihBuffer:TABLE
      EventLog.ModifiedFields = TRIM(lcDeleteFields,',')
      EventLog.FieldFormats   = TRIM(lcFieldFormats,',')
      EventLog.DataValues     = LEFT-TRIM(lcDataValues,CHR(255)).

   FIND FIRST TriggerConf WHERE 
              TriggerConf.TriggerConfID = ihBuffer:TABLE AND 
              TriggerConf.EventRule    > 0               AND
              TriggerConf.ValidTo      >= TODAY          AND 
              TriggerConf.ValidFrom    <= TODAY NO-LOCK NO-ERROR.
                                                     
   IF AVAILABLE TriggerConf THEN DO:
      CREATE TriggerEvent.
      ASSIGN 
         TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
         TriggerEvent.TriggerConfID  = ihBuffer:TABLE
         TriggerEvent.EventSource    = "DELETE"
         TriggerEvent.Created        = DATETIME(TODAY,MTIME)
         TriggerEvent.TableID        = ihBuffer:RECID
         TriggerEvent.TableName      = EventLog.Tablename 
         TriggerEvent.KeyValue       = EventLog.Key
         TriggerEvent.ChangedFields  = EventLog.ModifiedFields
         TriggerEvent.ChangedValues  = EventLog.DataValues.
   END.
   
   RETURN FALSE.
                                                                              
END FUNCTION.


&ENDIF
