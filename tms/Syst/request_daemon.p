/* ----------------------------------------------------------------------------
  MODULE .......: request_daemon.p
  FUNCTION .....: process funcrun batches 
  APPLICATION ..: TMS
  Author........: Subhash Sanjeevi
  CREATED ......: 13.01.16/aam 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "".
   
{timestamp.i}
{log.i}
{cparam2.i}
{tmsconst.i}

/******** Main start *********/

FOR EACH RequestType WHERE 
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

      FIND MsRequest NO-LOCK WHERE 
           MsRequest.Brand     EQ gcBrand               AND
           MsRequest.ReqType   EQ RequestType.ReqType   AND 
           MsRequest.ReqStatus EQ RequestStatus.ReqStat NO-ERROR.                 
      IF AVAIL MsRequest THEN DO:

         IF MsRequest.ReqType EQ ({&REQTYPE_PUBLISH_INVOICE}) THEN 
            RUN publish_invoice.p (MsRequest.MsRequest).
         ELSE IF MsRequest.ReqType EQ ({&REQTYPE_PUBLISH_IFS}) THEN 
            RUN publish_ifs.p (MsRequest.MsRequest).
         
      END.

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
 
/********** Main end **********/
