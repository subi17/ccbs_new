/* ----------------------------------------------------------------------------
  MODULE .......: requestrunner
  FUNCTION .....: process requests
  APPLICATION ..: TMS
  CREATED ......: 31.10.07/aam
  CHANGED ......: 
  Version ......: TMS
  --------------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "request".
   
{heartbeat.i}
{timestamp.i}
{log.i}
{requestrunner.i}

DEF VAR lcUser     AS CHAR NO-UNDO.
DEF VAR liErrors   AS INT  NO-UNDO.
DEF VAR liHandled  AS INT  NO-UNDO.
DEF VAR liAddErr   AS INT  NO-UNDO.
DEF VAR liAddHand  AS INT  NO-UNDO.
DEF VAR liLoop     AS INT  NO-UNDO.
DEF VAR lcTime     AS CHAR NO-UNDO.
DEF VAR lcStarted  AS CHAR NO-UNDO.
DEF VAR liQueue    AS INT  NO-UNDO.
DEF VAR lcTitle1   AS CHAR NO-UNDO.
DEF VAR lcTitle2   AS CHAR NO-UNDO.
DEF VAR llLog      AS LOG  NO-UNDO.
DEF VAR ldCurrent  AS DEC  NO-UNDO.
DEF VAR ldaSystemDay   AS DATE NO-UNDO.
DEF VAR liInterval     AS INT  NO-UNDO.
DEF VAR llHandled      AS LOG  NO-UNDO. 
DEF VAR liIntervalLoop AS INT  NO-UNDO. 

DEF TEMP-TABLE ttHandled NO-UNDO
   FIELD ReqType AS INT
   FIELD ReqStat AS INT
   FIELD Handled AS INT
   FIELD Errors  AS INT
   FIELD LogOn   AS LOG
   INDEX ReqType ReqType ReqStat.
   
FORM
   lcTime 
      FORMAT "X(15)" 
      LABEL "Time" 
   liLoop   
      FORMAT ">>>>>>>>9"
      LABEL "Loop"
   liHandled 
      FORMAT ">>>>>>>9"
      LABEL "Handled"
   liErrors
      FORMAT ">>>>>>>9"
      LABEL "Errors"
   llLog 
      FORMAT "On/Off"
      LABEL "Log"
WITH ROW 1 COL 1 OVERLAY 17 DOWN TITLE lcTitle1 FRAME fLog.

FORM
   ttHandled.ReqType 
      FORMAT ">>9" 
      LABEL "Type"
   ttHandled.ReqStat
      FORMAT ">9"
      LABEL "St"
   ttHandled.Handled
      FORMAT ">>>>>>>9"
      LABEL "Handled"
   ttHandled.Errors
      FORMAT ">>>>>>9"
      LABEL "Errors"
   ttHandled.LogOn
      FORMAT "On/Off"
      LABEL "Log"
WITH ROW 1 COL 50 OVERLAY 17 DOWN TITLE lcTitle2 FRAME fType.

  
ASSIGN 
   lcStarted = STRING(TODAY,"99.99.99") + " " + STRING(TIME,"hh:mm:ss")
   lcTitle2  = " St: " + lcStarted + " ".
   
IF NUM-ENTRIES(SESSION:PARAMETER) > 1 THEN 
   ldaSystemDay = DATE(ENTRY(2,SESSION:PARAMETER)) NO-ERROR.
liQueue = INTEGER(ENTRY(1,SESSION:PARAMETER)) NO-ERROR.
   
IF ldaSystemDay = ? OR ldaSystemDay < TODAY THEN 
   ldaSystemDay = TODAY.

IF liQueue = 0 THEN DO:
   
   ASSIGN
      gcHelpParam = "choose"
      si-recid    = 0.
      
   RUN requestqueue.
   
   IF si-recid > 0 THEN DO:
      FIND RequestQueue WHERE RECID(RequestQueue) = si-recid
         NO-LOCK NO-ERROR.
      IF AVAILABLE RequestQueue THEN liQueue = RequestQueue.Queue.
   END.
END.

IF liQueue = 0 THEN RETURN "ERROR:Queue has not been selected".

FIND RequestQueue WHERE 
     RequestQueue.Brand = gcBrand AND
     RequestQueue.Queue = liQueue NO-LOCK NO-ERROR.
IF NOT AVAILABLE RequestQueue THEN RETURN "ERROR:Queue not defined".
     
lcTitle1 = " QUEUE " + STRING(RequestQueue.Queue) + " " +
           RequestQueue.QName + " ".

PAUSE 0.
VIEW FRAME fLog.
PAUSE 0.
VIEW FRAME fType.

REQUESTER:
DO WHILE TRUE
   ON QUIT UNDO, LEAVE
   ON STOP UNDO, LEAVE:

   /* get the latest configuration */ 
   FIND RequestQueue WHERE 
        RequestQueue.Brand = gcBrand AND
        RequestQueue.Queue = liQueue NO-LOCK NO-ERROR.

   IF NOT RequestQueue.InUse THEN LEAVE REQUESTER.     
        
   /* logging wanted */
   IF RequestQueue.LogOn NE llLog THEN DO:

      IF RequestQueue.LogOn = FALSE THEN DO:
         fCloseLog().
         llLog = FALSE.
      END.

      ELSE IF RequestQueue.LogFile > "" AND RequestQueue.LogEntry > "" THEN DO:
         fSetLogFileName(RequestQueue.LogFile).
         fSetLogEntryTypes(RequestQueue.LogEntry).
         fSetLogTreshold(INTEGER(RequestQueue.LogThreshold)).      
         llLog = TRUE.    
      END.
   END.

   llHandled = FALSE.

   /* go through all types */ 
   FOR EACH RequestType NO-LOCK WHERE
            RequestType.Brand = gcBrand  AND
            RequestType.Queue = RequestQueue.Queue AND
            RequestType.InUse:

      /* current time for getting scheduled requests */
      IF TODAY > ldaSystemDay THEN ldaSystemDay = TODAY.
      ldCurrent = fMake2DT(ldaSystemDay,TIME).
            
      lcUser = RequestType.UserCode.
      IF lcUser = "" THEN lcUser = katun.
 
      /* logging on type level */
      IF RequestType.LogOn THEN DO:

         IF RequestType.LogFile > "" AND RequestType.LogEntry > "" THEN DO:
            fSetLogFileName(RequestType.LogFile).
            fSetLogEntryTypes(RequestType.LogEntry).
            fSetLogTreshold(INTEGER(RequestType.LogThreshold)).      
            
            IF RequestType.LogClear THEN DO:
               fClearLog().
            END.
         END.
      END.
        
      FOR EACH RequestStatus OF RequestType NO-LOCK WHERE
               RequestStatus.InUse:

         FIND FIRST ttHandled WHERE
                    ttHandled.ReqType = RequestStatus.ReqType AND
                    ttHandled.ReqStat = RequestStatus.ReqStat NO-ERROR.
         IF NOT AVAILABLE ttHandled THEN DO:
            CREATE ttHandled.
            ASSIGN 
               ttHandled.ReqType = RequestStatus.ReqType
               ttHandled.ReqStat = RequestStatus.ReqStat.
         END.
 
         /* logging on status level */
         IF RequestStatus.LogOn THEN DO:

            IF RequestStatus.LogFile > "" AND RequestStatus.LogEntry > "" 
            THEN DO:
               fSetLogFileName(RequestStatus.LogFile).
               fSetLogEntryTypes(RequestStatus.LogEntry).
               fSetLogTreshold(INTEGER(RequestStatus.LogThreshold)).      
            
               IF RequestStatus.LogClear THEN DO:
                  fClearLog().
               END.
            END.
         END.
    
         ttHandled.Log = RequestStatus.LogOn.
            
         IF FRAME-LINE(fType) = 1 THEN DO:
            PAUSE 0.
            CLEAR FRAME fType ALL.
         END.
               
         DISPLAY ttHandled.ReqType
                 ttHandled.ReqStat
         WITH FRAME fType.
         
         RUN pRunRequest(0,
                         RequestType.ReqType,
                         RequestStatus.ReqStat,
                         ldCurrent,
                         IF RequestStatus.Program > ""
                         THEN RequestStatus.Program
                         ELSE RequestType.Program,
                         lcUser,
                         OUTPUT liAddHand,
                         OUTPUT liAddErr).
   
         ASSIGN 
            liHandled = liHandled + liAddHand
            liErrors  = liErrors  + liAddErr 
            ttHandled.Handled = ttHandled.Handled + liAddHand
            ttHandled.Errors  = ttHandled.Errors  + liAddErr.

         IF liAddHand > 0 THEN llHandled = TRUE.   

         DISPLAY ttHandled.ReqType
                 ttHandled.ReqStat
                 ttHandled.Handled
                 ttHandled.Errors 
                 ttHandled.Log WITH FRAME fType.
         IF FRAME-LINE(fType) = FRAME-DOWN(fType) THEN 
            UP FRAME-LINE(fType) - 1 WITH FRAME fType.
         ELSE DOWN WITH FRAME fType.

         /* close status level log */  
         IF RequestStatus.LogOn THEN DO:
            fCloseLog().
         END.

         /* type level log back on */
         IF RequestType.LogOn THEN DO:
            fSetLogFileName(RequestType.LogFile).
         END.
      END. 

      /* close type level log */  
      IF RequestType.LogOn THEN DO:
         fCloseLog().
      END.

      /* queue level log back on */
      IF RequestQueue.LogOn THEN DO:
         fSetLogFileName(RequestQueue.LogFile).
      END.
   END.                

   liLoop = liLoop + 1.

   PAUSE 0.
   DISP  liLoop  
         SUBSTRING(STRING(ldaSystemDay,"99.99.99"),1,6) + " " + 
            STRING(TIME,"hh:mm:ss") @ lcTime
         llLog   
         liHandled
         liErrors
   WITH FRAME fLog.
   DOWN WITH FRAME fLog.

   PAUSE 0.
   UP FRAME-LINE(fType) - 1 WITH FRAME fType.

   /* monitoring */   
   IF RequestQueue.MonitorOn AND RequestQueue.Monitor > "" THEN DO:
     fKeepAlive(RequestQueue.Monitor).
  
     PUT SCREEN ROW 23 COL 2 
        "MONITOR " + RequestQueue.Monitor + " " + 
        STRING(TODAY,"99-99-9999") + " " + STRING(TIME,"hh:mm:ss").
   END.

   IF llHandled THEN ASSIGN
      liInterval = MIN(2,RequestQueue.Interval)
      liIntervalLoop = 0.
   ELSE DO:
      IF liIntervalLoop < 5 THEN liIntervalLoop = liIntervalLoop + 1.
      ELSE ASSIGN 
         liInterval = MIN(liInterval + 1,RequestQueue.Interval)
         liIntervalLoop = 0.
   END.

   PUT SCREEN ROW 22 COL 2
      "F8 TO QUIT, OTHER KEYS START HANDLING IMMEDIATELLY (INTERVAL=" +
      STRING(liInterval) + ")".
 
   READKEY PAUSE liInterval.
    
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
       LEAVE REQUESTER.
   END.

   PUT SCREEN ROW 22 COL 2 FILL(" ",70).
END.

IF RequestQueue.LogOn THEN DO:
   fCloseLog().
END.

QUIT.



