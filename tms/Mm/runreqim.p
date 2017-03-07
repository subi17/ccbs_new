/* runreqim.p     03.01.05/aam

   run one service msrequest immediately 
   
   changes:       15.12.05/aam input parameter ilReRate
                  13.11.07/aam RequestType,RequestStatus
*/

{Syst/requestrunner.i}
{Func/log.i}

DEF INPUT  PARAMETER iiRequest  AS INT  NO-UNDO.

DEF VAR liDone    AS INT  NO-UNDO.
DEF VAR liError   AS INT  NO-UNDO. 
DEF VAR liReqType AS INT  NO-UNDO.
DEF VAR liReqStat AS INT  NO-UNDO.
DEF VAR lcUser    AS CHAR NO-UNDO. 

FIND MsRequest WHERE 
     MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN RETURN "ERROR:Unknown request".

ASSIGN
   liReqType = MsRequest.ReqType
   liReqStat = MsRequest.ReqStatus.

FOR FIRST RequestType NO-LOCK WHERE
          RequestType.Brand   = gcBrand   AND
          RequestType.ReqType = liReqType AND 
          RequestType.InUse:

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
        
   FOR FIRST RequestStatus OF RequestType NO-LOCK WHERE
             RequestStatus.ReqStatus = liReqStat AND
             RequestStatus.InUse:

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
            
      RUN pRunRequest(iiRequest,
                      RequestType.ReqType,
                      RequestStatus.ReqStat,
                      0.0,
                      IF RequestStatus.Program > ""
                      THEN RequestStatus.Program
                      ELSE RequestType.Program,
                      lcUser,
                      OUTPUT liDone,
                      OUTPUT liError).

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
   
END.  

IF liError > 0 THEN RETURN "ERROR:Request was marked to error status".
ELSE IF liDone = 0 THEN RETURN "INFO:Request could not be handled".
ELSE RETURN "". 


