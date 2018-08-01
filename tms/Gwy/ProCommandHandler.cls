/*------------------------------------------------------------------------
    File        : ProCommandHandler
    Purpose     : 
    Syntax      : 
    Description : Go through ProCommand records and send them
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING PROGRESS.Json.ObjectModel.*.
USING Gwy.ParamRESTClient.
USING Gwy.RESTError.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.ProCommandHandler FINAL:

   DEFINE PRIVATE TEMP-TABLE ttProcess NO-UNDO
      FIELD ProCommandTarget AS CHARACTER
      FIELD ProCommandURIPrefix AS CHARACTER
      FIELD TenantName AS CHARACTER
      FIELD LastProcessed AS DATETIME-TZ
      INDEX TenantName IS PRIMARY TenantName LastProcessed ProCommandTarget.

   DEFINE PRIVATE TEMP-TABLE ttStatistic NO-UNDO
      FIELD ProCommandTarget AS CHARACTER
      FIELD ProCommandType   AS CHARACTER
      FIELD ProcessAmt       AS INT64
      FIELD FailedAmt        AS INT64
      FIELD LockedAmt        AS INT64
      FIELD LastUpdated AS DATETIME-TZ
      INDEX ProCommandTarget IS PRIMARY UNIQUE ProCommandTarget ProCommandType.

   DEFINE PRIVATE PROPERTY ValidRESTMethods AS CHARACTER NO-UNDO
      GET:
         RETURN "POST,GET,PATCH".
      END GET.

   DEFINE PRIVATE PROPERTY ValidTargets     AS CHARACTER NO-UNDO
      GET:
         RETURN "NB_AS,NB_CH,BPM".
      END GET.

   DEFINE PRIVATE VARIABLE aoRESTClient        AS CLASS ParamRESTClient NO-UNDO.

   DEFINE PRIVATE VARIABLE aiMaxTime       AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acTenants       AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE acCurrentTenant AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ahQuery         AS HANDLE    NO-UNDO.

   DEFINE PRIVATE VARIABLE aoOMParser      AS CLASS ObjectModelParser   NO-UNDO.


   CONSTRUCTOR PUBLIC ProCommandHandler
      (icProcommandTargets AS CHARACTER):

      mInitProcess(icProcommandTargets).

      DO ON ERROR UNDO, THROW:

         aiMaxTime = Syst.Parameters:geti("ReqHandler.MaxTime", "ReqHandler").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume max time is 5000 (milliseconds) */
            aiMaxTime = 5000.
         END CATCH.

      END.

      aoRESTClient = NEW ParamRESTClient("SAPC").

      CREATE QUERY ahQuery.

      ahQuery:SET-BUFFERS(BUFFER ttProcess:HANDLE).
      /* Preselect is needed as we are updating the field which is part
         of the index */
      ahQuery:QUERY-PREPARE('PRESELECT EACH ttProcess NO-LOCK BREAK BY ttProcess.TenantName').

      aoOMParser = NEW ObjectModelParser().

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC ProCommandHandler( ):

      IF VALID-OBJECT(aoOMParser)
      THEN DELETE OBJECT aoOMParser.

      IF VALID-HANDLE(ahQuery)
      THEN DO:
         ahQuery:QUERY-CLOSE().
         DELETE OBJECT ahQuery.
      END.

      EMPTY TEMP-TABLE ttProcess.
      EMPTY TEMP-TABLE ttStatistic.

      IF VALID-OBJECT(aoRESTClient)
      THEN DELETE OBJECT aoRESTClient.

   END DESTRUCTOR.


   METHOD PRIVATE VOID mInitProcess
      (icProCommandTargets AS CHARACTER):

      EMPTY TEMP-TABLE ttProcess.

      DEFINE VARIABLE lii           AS INTEGER NO-UNDO.
      DEFINE VARIABLE lij           AS INTEGER NO-UNDO.
      DEFINE VARIABLE liTenantCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE lcTenants     AS CHARACTER NO-UNDO.

      DO lii = 1 TO NUM-ENTRIES(multitenancy.TenantInformation:TenantList):
         IF ENTRY(lii,multitenancy.TenantInformation:TenantList) =
            multitenancy.TenantInformation:SuperTenant
         THEN NEXT.
         
         ASSIGN
            lcTenants = lcTenants + "," +
                        ENTRY(lii,multitenancy.TenantInformation:TenantList)
            liTenantCount = liTenantCount + 1.
                     
      END.

      lcTenants = LEFT-TRIM(lcTenants,",").
      
      IF liTenantCount = 0
      THEN RETURN ERROR NEW Progress.Lang.AppError
                  ("Cannot find any tenants", 1).

      DO lii = 1 TO NUM-ENTRIES(icProCommandTargets):
         DO lij = 1 TO liTenantCount:
            CREATE ttProcess.
            ASSIGN
               ttProcess.ProCommandTarget = ENTRY(lii, icProCommandTargets)
               ttProcess.TenantName       = ENTRY(lij, lcTenants)
               ttProcess.LastProcessed    = NOW.

            CASE ttProcess.ProCommandTarget:
               WHEN "NB_AS"
               THEN ttProcess.ProCommandURIPrefix = "/northbound-common/post-sales/ema". 
               WHEN "NB_CH"
               THEN ttProcess.ProCommandURIPrefix = "/northbound-tms/sync/orders".
               WHEN "BPM"
               THEN ttProcess.ProCommandURIPrefix = "/crm-integration-tms/Order".
            END CASE. 
         END.
      END.

   END METHOD.


   METHOD PRIVATE VOID mUpdateStatistic 
      (INPUT icTarget    AS CHARACTER,
       INPUT icType      AS CHARACTER,
       INPUT iiProcessed AS INT64,
       INPUT iiFailed    AS INT64):
   
      FIND ttStatistic WHERE
        ttStatistic.ProCommandTarget = icTarget AND
        ttStatistic.ProCommandType   = icType
      NO-ERROR.
      
      IF NOT AVAILABLE ttStatistic
      THEN DO:
         CREATE ttStatistic.
         ASSIGN
            ttStatistic.ProCommandTarget = icTarget
            ttStatistic.ProCommandType   = icType.
      END.
      
      ASSIGN
         ttStatistic.ProcessAmt  = ttStatistic.ProcessAmt + iiProcessed
         ttStatistic.FailedAmt   = ttStatistic.FailedAmt  + iiFailed
         ttStatistic.LockedAmt   = ttStatistic.LockedAmt  + ABSOLUTE(1 - iiProcessed)
         ttStatistic.LastUpdated = NOW.
         
   END METHOD.

   METHOD PRIVATE VOID mVerifyStatusOK
      ( ioJsonObject AS JsonObject ):

      IF NOT ioJsonObject:Has("result")
      THEN UNDO, THROW NEW RESTError("Response data doesn't have result information").

      IF ioJsonObject:GetType("result") NE JsonDataType:STRING
      THEN UNDO, THROW NEW RESTError("Response data result is not string").

      IF NOT ioJsonObject:GetCharacter("result") EQ "OK"
      THEN UNDO, THROW NEW RESTError(200, "OK", ioJsonObject).

   END METHOD.

   METHOD PRIVATE VOID mSendProCommand
      (iiProCommandId AS INT64,
       icURI AS CHARACTER):

      DEFINE VARIABLE lmpJsonData    AS MEMPTR              NO-UNDO.
      DEFINE VARIABLE loReqJsonData  AS CLASS JsonConstruct NO-UNDO.
      DEFINE VARIABLE lcReturnValue  AS LONGCHAR            NO-UNDO.
      DEFINE VARIABLE lii            AS INTEGER             NO-UNDO.
      DEFINE VARIABLE ldtSendTS      AS DATETIME-TZ         NO-UNDO.
      DEFINE VARIABLE ldtResponseTS  AS DATETIME-TZ         NO-UNDO.
      DEFINE VARIABLE lcTemp         AS LONGCHAR            NO-UNDO.
      DEFINE VARIABLE llRequestNotAvailable AS LOGICAL INITIAL FALSE NO-UNDO.

      DEFINE BUFFER ProCommand FOR ProCommand.
      DEFINE BUFFER MsRequest  FOR MsRequest.

      FIND ProCommand EXCLUSIVE-LOCK WHERE
           ProCommand.ProCommandId = iiProCommandId NO-ERROR NO-WAIT.

      IF NOT AVAILABLE ProCommand
      THEN RETURN.

      IF ProCommand.MsRequest > 0
      THEN DO:
         FIND MsRequest EXCLUSIVE-LOCK WHERE
              MsRequest.MsRequest = ProCommand.MsRequest NO-ERROR NO-WAIT.

         IF NOT AVAILABLE MsRequest
         THEN DO:
            IF LOCKED MsRequest
            THEN RETURN.
            ELSE llRequestNotAvailable = TRUE.
         END.
      END.

      DO ON ERROR UNDO, THROW:

         IF llRequestNotAvailable
         THEN UNDO, THROW NEW RESTError
               (SUBSTITUTE("MsRequest id &1 doesn't exists",
                           ProCommand.MsRequest)).

         IF LOOKUP(ProCommand.ProCommandVerb,ValidRESTMethods) EQ 0
         THEN UNDO, THROW NEW RESTError
               (SUBSTITUTE("Invalid rest method &1 defined to ProCommandVerb",
                           ProCommand.ProCommandVerb)).

         IF LOOKUP(ProCommand.ProCommandTarget,ValidTargets) EQ 0
         THEN UNDO, THROW NEW RESTError
               (SUBSTITUTE("Invalid ProCommandTarget &1",
                           ProCommand.ProCommandTarget)).

         IF ProCommand.CommandLine NE ?
         THEN DO:
            COPY-LOB FROM OBJECT ProCommand.CommandLine TO OBJECT lmpJsonData NO-CONVERT.
            loReqJsonData = aoOMParser:Parse(lmpJsonData).
         END.

         aoRESTClient:mSetURIPath(icURI).

         /* After this line it is sure that the ProCommand record itself is valid
            (otherwise error would have been thrown) */

         ldtSendTS = NOW.

         CASE ProCommand.ProCommandVerb:
            WHEN "POST"
            THEN aoRESTClient:mPOST(loReqJsonData).  /* loReqJsonData EQ ? is supported. */
            WHEN "PATCH"
            THEN aoRESTClient:mPATCH(loReqJsonData). /* loReqJsonData EQ ? is supported. */
            WHEN "GET"
            THEN aoRESTClient:mGET().
         END.

         ASSIGN
            ldtResponseTS         = NOW
            SET-SIZE(lmpJsonData) = 0.

         IF NOT VALID-OBJECT(aoRESTClient:aoResultObject) OR
            NOT TYPE-OF(aoRESTClient:aoResultObject, JsonObject)
         THEN UNDO, THROW NEW RESTError("Result body is empty or not Json object").

         DO ON ERROR UNDO, THROW:
            CAST(aoRESTClient:aoResultObject, JsonConstruct):Write(lmpJsonData, YES).
            COPY-LOB FROM OBJECT lmpJsonData TO OBJECT ProCommand.Response NO-CONVERT.
            CATCH loWriteError AS Progress.Lang.Error:
               UNDO, THROW NEW RESTError("Cannot store json response to database clob field").
            END CATCH.
         END.

         mVerifyStatusOK(CAST(aoRESTClient:aoResultObject, JsonObject)).

         /* After this line it is sure that no errors occurred
            (note SendTS and ResponseTS will be set in finally block) */

         ProCommand.ActivationTS = ldtResponseTS.

         CASE ProCommand.ProCommandTarget:
            WHEN "BPM"
            THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_ONGOING}.
            WHEN "NB_AO" OR WHEN "NB_CH"
            THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_DONE}.
         END CASE.

         CATCH loRESTError AS RESTError:

            IF loRESTError:ErrorMessage > ""
            THEN COPY-LOB FROM OBJECT loRESTError:ErrorMessage TO OBJECT ProCommand.Response NO-CONVERT.
            ELSE COPY-LOB FROM OBJECT loRESTError:mGetErrorData() TO OBJECT ProCommand.Response NO-CONVERT.

            CASE loRESTError:HTTPStatus:
               WHEN 400 /* 400 Bad Request */
               THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_400_ERROR}.
               WHEN 500 /* 500 Internal server error */
               THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_500_ERROR}.
               WHEN 200 /* 200 OK */
               THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_200_ERROR}.
               OTHERWISE DO:
                  IF loRESTError:HTTPStatus EQ 0
                  THEN DO:
                     IF ldtSendTS EQ ?
                     THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_INTERNAL_ERROR}. /* Internal error */
                     ELSE ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_200_ERROR}.
                  END.
                  ELSE ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_NOT_SUPPORTED_HTTP_STATUS_ERROR}.
               END.
            END CASE.

         END CATCH.

         CATCH loError AS Error:

            ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_INTERNAL_ERROR}. /* Internal error */

            IF TYPE-OF(loError, AppError) AND
               CAST(loError,AppError):ReturnValue > ""
            THEN lcTemp = CHR(10) + CAST(loError,AppError):ReturnValue.

            DO lii = 1 TO loError:NumMessages:
               lcTemp = lcTemp + CHR(10) + loError:GetMessage(lii).
            END.

            lcTemp = SUBSTRING(lcTemp,2).

            COPY-LOB FROM OBJECT lcTemp TO OBJECT ProCommand.Response NO-CONVERT.

         END CATCH.

         FINALLY:
            ASSIGN
               ProCommand.SendTS     = ldtSendTS
               ProCommand.ResponseTS = ldtResponseTS.

            IF AVAILABLE MsRequest
            THEN CASE ProCommand.ProCommandStatus:
                    WHEN {&PROCOMMANDSTATUS_NEW} OR WHEN {&PROCOMMANDSTATUS_ONGOING}
                    THEN .
                    WHEN {&PROCOMMANDSTATUS_DONE}
                    THEN MsRequest.ReqStatus = {&REQUEST_STATUS_HLR_DONE}.
                    OTHERWISE MsRequest.ReqStatus = {&REQUEST_STATUS_REJECTED}.
            END CASE.

         END FINALLY.
      END.

      FINALLY:
         SET-SIZE(lmpJsonData) = 0.
         IF VALID-OBJECT(loReqJsonData)
         THEN DELETE OBJECT loReqJsonData.
      END FINALLY.

   END METHOD.


   METHOD PUBLIC LOGICAL mProcess():

      DEFINE VARIABLE liStartTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liResult       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liMTIME        AS INTEGER   NO-UNDO.
      DEFINE VARIABLE llOK           AS LOGICAL   NO-UNDO.
      
      ASSIGN
         liStartTime = MTIME.

      ahQuery:QUERY-OPEN().

      DO WHILE TRUE ON ERROR UNDO, THROW:

         llOK = ahQuery:GET-NEXT(NO-LOCK).

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).

         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.

         IF ahQuery:FIRST-OF(1) AND ttProcess.TenantName NE acCurrentTenant
         THEN DO:
            multitenancy.TenantInformation:mSetEffectiveTenant(ttProcess.TenantName).
            acCurrentTenant = ttProcess.TenantName.
         END.

         ASSIGN
            ttProcess.LastProcessed = NOW.

         FOR
            EACH ProCommand NO-LOCK WHERE
                 ProCommand.ProCommandTarget = ttProcess.ProcommandTarget AND
                 ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_NEW}
            BY ProCommand.CreatedTS:

            /* there is a possibility that another process has just started
               handling this same request */
            IF ProCommand.ProCommandStatus NE 0 THEN NEXT.
            
            DO TRANSACTION:
               mSendProCommand(ProCommand.ProCommandId,
                               SUBSTITUTE("&1&2",
                                          ttProcess.ProCommandURIPrefix,
                                          ProCommand.ProCommandTargetURL)).
            END.

            mUpdateStatistic(ttProcess.ProcommandTarget,
                             ProCommand.ProCommandType,
                             INTEGER(ProCommand.ProCommandStatus > 0),
                             INTEGER(ProCommand.ProCommandStatus > 2)).

            liMTIME = MTIME.

            /* Maximum processing time per one round is aiMaxTime.
               No separate wait in the calling side is needed when
               the maximum time is reached.
               In order to handle situation when day is changed
               two possible scenarios must be handled */
            IF ( liStartTime > liMTIME AND
                 liStartTime < 86400000 + liMTIME - aiMaxTime ) OR
               ( liStartTime <= liMTIME AND
                 liStartTime < liMTIME - aiMaxTime )
            THEN RETURN FALSE.
         END.
      END.

      /* We will be there only when there are no ProCommand records to process
         or we got every ProCommand records handled */
      RETURN TRUE.

      FINALLY:
         ahQuery:QUERY-CLOSE().
      END FINALLY.
   
   END METHOD.


   METHOD PUBLIC VOID mLogStatistic
      (idttzLastCheck AS DATETIME-TZ):

      DEFINE VARIABLE lcc AS CHARACTER NO-UNDO.

      FOR EACH ttStatistic:

         IF ttStatistic.LastUpdated < idttzLastCheck
         THEN NEXT.

         lcc = lcc + ", " + SUBSTITUTE("&1/&2: P&3, F&4, L&5",
                                       ttStatistic.ProCommandTarget,
                                       ttStatistic.ProCommandType,
                                       ttStatistic.ProcessAmt,
                                       ttStatistic.FailedAmt,
                                       ttStatistic.LockedAmt).

         IF LENGTH(lcc) > 130
         THEN DO:
            LOG-MANAGER:WRITE-MESSAGE(SUBSTRING(lcc, 3), "STATUS").
            lcc = "".
         END.
      END.

      IF lcc > ""
      THEN LOG-MANAGER:WRITE-MESSAGE(SUBSTRING(lcc, 3), "STATUS").

   END METHOD.

END CLASS.