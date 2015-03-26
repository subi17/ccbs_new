/* dumpfile_run.i    13.11.08/aam
*/

{create_eventlog.i}
{timestamp.i}
&GLOBAL-DEFINE DUMPLOG_ERROR_NOTIFICATION -12345

/* convert defined delimiter into character(s) */
FUNCTION fInitDelimiter RETURNS CHAR
   (icDelimiter AS CHAR):

   DEF VAR liDelimitChar AS INT NO-UNDO.
   DEF VAR liCharPoint   AS INT NO-UNDO.
   
   REPEAT:
      liCharPoint = INDEX(icDelimiter,"ASC(").
      /* character code must be given with 3 digits) */
      IF liCharPoint = 0 OR SUBSTRING(icDelimiter,liCharPoint + 7,1) NE ")"
      THEN LEAVE.
      
      liDelimitChar = INTEGER(SUBSTRING(icDelimiter,liCharPoint + 4,3))
                      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN LEAVE.
      
      icDelimiter = REPLACE(icDelimiter,"ASC(" + STRING(liDelimitChar,"999") +
                            ")",CHR(liDelimitChar)).
   END.
   
   RETURN icDelimiter.
      
END FUNCTION.

/* has record been modified since last dump */
FUNCTION fWasRecordModified RETURNS LOGIC
   (ihTable          AS HANDLE,
    icCheckedFrom    AS CHAR,
    icEventFields    AS CHAR,
    idModified       AS DEC,
    idaModified      AS DATE,
    idtModified      AS DATETIME,
    icModifiedFields AS CHAR):
         
   DEF VAR llModified    AS LOG    NO-UNDO.
   DEF VAR lcEventLogKey AS CHAR   NO-UNDO.
   DEF VAR ldEventTime   AS DEC    NO-UNDO.
   DEF VAR lhCheckField  AS HANDLE NO-UNDO.
   DEF VAR liCheck       AS INT    NO-UNDO.
   DEF VAR liField       AS INT    NO-UNDO.
   DEF VAR llHit         AS LOG    NO-UNDO.

   llModified = FALSE.

   DO liCheck = 1 TO NUM-ENTRIES(icCheckedFrom,"|"):

      /* how are new/modified records identified */
      CASE ENTRY(liCheck,icCheckedFrom,"|"):
   
      /* from a field value (timestamp or datetime) */
      WHEN "Field" THEN DO:
         lhCheckField = ihTable:BUFFER-FIELD(ENTRY(liCheck,icEventFields,"|")) 
            NO-ERROR.
         IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(lhCheckField) THEN DO:
            IF lhCheckField:DATA-TYPE = "decimal" AND 
               lhCheckField:BUFFER-VALUE >= idModified THEN 
               llModified = TRUE.
            ELSE IF lhCheckField:DATA-TYPE = "datetime" AND
               lhCheckField:BUFFER-VALUE >= idtModified THEN
               llModified = TRUE.
         END.
      END.
   
      /* from eventlog */
      OTHERWISE DO:   
         lcEventLogKey = fEventKeyValues(ihTable,
                                         ENTRY(liCheck,icEventFields,"|")).

         /* any action will do */
         FOR EACH EventLog NO-LOCK USE-INDEX TableName WHERE
                  EventLog.TableName  = ihTable:NAME   AND
                  EventLog.Key        = lcEventLogKey  AND
                  EventLog.EventDate >= idaModified:
                  
            /* changed on dump day */
            IF EventLog.EventDate = idaModified THEN DO:
               ldEventTime = fHMS2TS(EventLog.EventDate,
                                     EventLog.EventTime).
               IF ldEventTime < idModified THEN NEXT.
            END.
       
            /* field(s) that must have been modified in order to pick this */
            IF EventLog.Action = "modify" AND TRIM(icModifiedFields) > "" 
            THEN DO:
               llHit = FALSE.
               DO liField = 1 TO NUM-ENTRIES(EventLog.ModifiedFields):
                  IF LOOKUP(ENTRY(liField,EventLog.ModifiedFields),
                            icModifiedFields) > 0 THEN DO:
                     llHit = TRUE.
                     LEAVE.
                  END.
               END.
               IF NOT llHit THEN NEXT.
            END.
            
            llModified = TRUE.
            LEAVE.
         END.
      END.
      
      END CASE.
   
      IF llModified THEN LEAVE.
   END.
   
   IF VALID-HANDLE(lhCheckField) THEN DELETE OBJECT lhCheckField. 
     
   RETURN llModified.

END FUNCTION.


/* find new eventlog rows and get records according to them */
PROCEDURE pFindFromEventLog:

   DEF INPUT PARAMETER ihTable          AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER icEventKey       AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER icModifiedFields AS CHAR   NO-UNDO.
   DEF INPUT PARAMETER idModified       AS DEC    NO-UNDO.
   DEF INPUT PARAMETER icCollFunction   AS CHAR   NO-UNDO.
   
   DEF VAR liField      AS INT    NO-UNDO.
   DEF VAR lcDataType   AS CHAR   NO-UNDO.
   DEF VAR liEventTime  AS INT    NO-UNDO.
   DEF VAR lhEvent      AS HANDLE NO-UNDO.
   DEF VAR lhFind       AS HANDLE NO-UNDO.
   DEF VAR lhField      AS HANDLE NO-UNDO.
   DEF VAR liEventField AS HANDLE NO-UNDO.
   DEF VAR lcFind       AS CHAR   NO-UNDO.
   DEF VAR lcFieldName  AS CHAR   NO-UNDO.
   DEF VAR ldtEventDate AS DATE   NO-UNDO.
   DEF VAR ldEventTime  AS DEC    NO-UNDO.
   DEF VAR llHit        AS LOG    NO-UNDO.

   CREATE WIDGET-POOL "EventLogFind".

   lhEvent = BUFFER EventLog:HANDLE.
   
   fSplitTS(idModified,
            OUTPUT ldtEventDate,
            OUTPUT liEventTime).

   CREATE QUERY lhFind IN WIDGET-POOL "EventLogFind".         
   lhFind:SET-BUFFERS(lhEvent,ihTable).


   /* note; this will get create and modify events, but not delete */
   lcFind = 'FOR EACH EventLog NO-LOCK USE-INDEX EventDate WHERE ' +
            'EventLog.EventDate >= ' + STRING(ldtEventDate) + ' AND ' +
            'EventLog.TableName = "' + ihTable:NAME + '", ' +
            'EACH ' + ihTable:NAME + ' NO-LOCK WHERE '.
   
   /* analyse keyvalues in eventlog for finding the actual table */
   DO liField = 1 TO NUM-ENTRIES(icEventKey,CHR(255)):
   
      ASSIGN
         lcFieldName = ENTRY(liField,icEventKey,CHR(255))
         lcFind      = lcFind + (IF liField > 1 THEN " AND " ELSE "") +
                       ihTable:NAME + "." + lcFieldName + " = "
         lcDataType = ""
         lhField    = ihTable:BUFFER-FIELD(lcFieldName) NO-ERROR.
         
      IF NOT ERROR-STATUS:ERROR AND lhField:DATA-TYPE NE "character" 
      THEN ASSIGN
         lcDataType = lhField:DATA-TYPE
         lcFind     = lcFind + lcDataType + "(".
         
      lcFind = lcFind + 'ENTRY(' + STRING(liField) + 
                        ',EventLog.Key,CHR(255))'.
                        
      IF lcDataType > "" THEN lcFind = lcFind + ")".
      
   END.

   lhFind:QUERY-PREPARE(lcFind).
   lhFind:QUERY-OPEN.
   
   REPEAT ON QUIT UNDO, RETRY
          ON STOP UNDO, RETRY:
          
      IF RETRY THEN LEAVE.
                    
      lhFind:GET-NEXT.
                           
      IF lhFind:QUERY-OFF-END THEN LEAVE.

      IF lhEvent::EventDate = ldtEventDate THEN DO:
         ldEventTime = fHMS2TS(ldtEventDate,
                               lhEvent::EventTime).
         IF ldEventTime < idModified THEN NEXT.
      END.

      /* field(s) that must have been modified in order to pick this */
      IF lhEvent::Action = "modify" AND TRIM(icModifiedFields) > "" THEN DO:
         
         llHit = FALSE.

         DO liField = 1 TO NUM-ENTRIES(lhEvent::ModifiedFields):
            IF LOOKUP(ENTRY(liField,lhEvent::ModifiedFields),
                            icModifiedFields) > 0 
            THEN DO:
               llHit = TRUE.
               LEAVE.
            END.
         END.
         
         IF NOT llHit THEN NEXT.
      END.
       
      /* function for collecting data (or to handle somehow) */
      IF icCollFunction > "" THEN 
         DYNAMIC-FUNCTION(icCollFunction).
   END.
       
   lhFind:QUERY-CLOSE.

   DELETE WIDGET-POOL "EventLogFind".
                                  
END PROCEDURE.


