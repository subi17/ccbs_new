/* requestrunner.i     01.11.07/aam
*/

{Syst/commali.i}
{Func/multitenantfunc.i}

/* process requests */
PROCEDURE pRunRequest:

   DEF INPUT  PARAMETER iiRequestID AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiReqType   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiReqStat   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idActTime   AS DEC  NO-UNDO.
   DEF INPUT  PARAMETER icProgram   AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icUser      AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER oiHandled   AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiErrors    AS INT  NO-UNDO. 
   
   DEF VAR lcTMSUser     AS CHAR NO-UNDO.
   DEF VAR liHandled     AS INT  NO-UNDO.
   DEF VAR liErrors      AS INT  NO-UNDO.
   DEF VAR liReqStat     AS INT  NO-UNDO.
   DEF VAR liResult      AS INT  NO-UNDO.

   IF SEARCH(icProgram) = ?
   THEN RETURN "ERROR:Module not found".
   
   /* user for eventlog */ 
   ASSIGN lcTMSUser  = katun
          katun      = icUser
          liHandled  = 0.

   /* go through all */                               
   IF iiRequestID = 0 THEN  
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand   AND
            MsRequest.ReqType   = iiReqType AND     
            MsRequest.ReqStatus = iiReqStat AND   
            MsRequest.ActStamp <= idActTime /* scheduled or immediate */
   TENANT-WHERE buffer-tenant-name(order) = {&TENANT_YOIGO} or 
                buffer-tenant-name(order) = {&TENANT_MASMOVIL}
   BY MsRequest.ActStamp
   BY MsRequest.MsRequest:

      /* there is a possibility that another process has just started
         handling this same request */
      IF MsRequest.ReqStatus NE iiReqStat THEN NEXT. 
      fsetEffectiveTenantForAllDB(BUFFER-TENANT-NAME(MsRequest)).      
      RUN VALUE(icProgram) (MsRequest.MsRequest).
      
      IF MsRequest.ReqType = 65 THEN DO:
         liResult = 0.
         liResult = INTEGER(RETURN-VALUE) NO-ERROR.
         ASSIGN
            oiHandled = oiHandled + liResult
            liHandled = liHandled + liResult.
      END.
      
      ELSE DO:

         IF RETURN-VALUE BEGINS "ERROR" THEN NEXT.

         IF MsRequest.ReqStatus > 1 THEN oiHandled = oiHandled + 1.
      
         IF MsRequest.ReqStatus = 3 THEN oiErrors = oiErrors + 1.

         liHandled = liHandled + 1.
      END.
      
      /* don't spend too much time to one type/status at one round */
      IF liHandled > 500 THEN LEAVE. 
   END.

   /* handle single event */
   ELSE 
   FOR FIRST MsRequest NO-LOCK WHERE
             MsRequest.MsRequest = iiRequestID 
   TENANT-WHERE buffer-tenant-name(order) = {&TENANT_YOIGO} or
                buffer-tenant-name(order) = {&TENANT_MASMOVIL}:
             
      IF MsRequest.ReqType NE iiReqType OR
         MsRequest.ReqStat NE iiReqStat
      THEN RETURN "ERROR: conflict in request data".
      fsetEffectiveTenantForAllDB(BUFFER-TENANT-NAME(MsRequest)).   
      RUN VALUE(icProgram) (MsRequest.MsRequest).
      
      IF MsRequest.ReqStatus > 1 THEN oiHandled = oiHandled + 1.
      
      IF MsRequest.ReqStatus = 3 THEN oiErrors = oiErrors + 1.
   END.
 
   katun = lcTMSUser.
   
END PROCEDURE.
