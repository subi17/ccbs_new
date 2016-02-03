/* ----------------------------------------------------------------------------
  MODULE .......: request_daemon.p
  FUNCTION .....: process funcrun batches 
  APPLICATION ..: TMS
  Author........: Subhash Sanjeevi
  CREATED ......: 13.01.16/aam 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "Cron".
   
{Func/timestamp.i}
{Func/log.i}
{Func/cparam2.i}

DEF VAR llHandled AS LOG NO-UNDO INIT FALSE. 
DEF VAR lcProgram AS CHAR NO-UNDO. 

/******** Main start *********/
FOR EACH RequestType NO-LOCK WHERE 
         RequestType.Brand = gcBrand AND 
         RequestType.Mode  = "Batch" AND
         RequestType.InUse:
   
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

      FOR EACH MsRequest NO-LOCK WHERE 
               MsRequest.Brand     EQ gcBrand               AND
               MsRequest.ReqType   EQ RequestType.ReqType   AND 
               MsRequest.ReqStatus EQ RequestStatus.ReqStat AND
               MsRequest.ActStamp <= fMakeTS()
            BY MsRequest.ActStamp 
            BY MsRequest.MsRequest:

         IF RequestStatus.Program > "" THEN lcProgram = RequestStatus.Program.
         ELSE lcProgram = RequestType.Program.
   
         IF SEARCH(lcProgram + ".r") = ? THEN DO:
            IF SEARCH(lcProgram + ".p") = ? THEN DO:
               fLogError(SUBST("ERROR:Module &1 not found", lcProgram)).
               LEAVE.
            END.
         END.

         RUN VALUE(lcProgram + ".p")(MsRequest.MsRequest).
         IF RETURN-VALUE BEGINS "ERROR" THEN NEXT.
         llHandled = TRUE.
         LEAVE.
      END.
         
      /* close status level log */  
      IF RequestStatus.LogOn THEN DO:
         fCloseLog().
      END.

      /* type level log back on */
      IF RequestType.LogOn THEN DO:
         fSetLogFileName(RequestType.LogFile).
      END.

      IF llHandled THEN LEAVE.
   END.

   /* close type level log */  
   IF RequestType.LogOn THEN DO:
      fCloseLog().
   END.
      
   IF llHandled THEN LEAVE.

END.         
 
/********** Main end **********/
