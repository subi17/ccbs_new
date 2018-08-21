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
USING Gwy.SAPC.TargetEnum.
USING Gwy.SAPC.VerbEnum.

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

   DEFINE PRIVATE VARIABLE aoRESTClient        AS CLASS ParamRESTClient NO-UNDO.

   DEFINE PRIVATE VARIABLE aiRoundTime     AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acURIPrefix     AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE acTenants       AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE acCurrentTenant AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ahQuery         AS HANDLE    NO-UNDO.
   DEFINE PRIVATE VARIABLE acProCommandTargetList  AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE acProCommandVerbList    AS CHARACTER NO-UNDO.

   DEFINE PRIVATE VARIABLE aoOMParser      AS CLASS ObjectModelParser   NO-UNDO.


   CONSTRUCTOR PUBLIC ProCommandHandler
      (icProcommandTargets AS CHARACTER):

      DO ON ERROR UNDO, THROW:

         aiRoundTime = Syst.Parameters:geti("RoundTime", "SAPC").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume round time is 5000 (milliseconds) */
            aiRoundTime = 5000.
         END CATCH.

      END.

      DO ON ERROR UNDO, THROW:

         acURIPrefix = Syst.Parameters:getc("URIPrefix", "SAPC").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then there is no URIPrefix */
            acURIPrefix = "".
         END CATCH.

      END.

      mInitProcess(icProcommandTargets).

      aoRESTClient = NEW ParamRESTClient("SAPC").

      CREATE QUERY ahQuery.

      ahQuery:SET-BUFFERS(BUFFER ttProcess:HANDLE).
      /* Preselect is needed as we are updating the field which is part
         of the index */
      ahQuery:QUERY-PREPARE('PRESELECT EACH ttProcess NO-LOCK BREAK BY ttProcess.TenantName').

      aoOMParser = NEW ObjectModelParser().

      ASSIGN
         acProCommandTargetList = GET-CLASS(TargetEnum):GetEnumNames()
         acProCommandVerbList   = GET-CLASS(VerbEnum):GetEnumNames().

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

            CASE TargetEnum:GetEnum(ttProcess.ProCommandTarget):
               WHEN TargetEnum:NB_AS
               THEN ttProcess.ProCommandURIPrefix = SUBSTITUTE("&1/api/northbound-common/post-sales/ema/1",
                                                               acURIPrefix).
               WHEN TargetEnum:NB_CH
               THEN ttProcess.ProCommandURIPrefix = SUBSTITUTE("&1/api/northbound-tms/1/sync/orders",
                                                               acURIPrefix).
               WHEN TargetEnum:BPM
               THEN ttProcess.ProCommandURIPrefix = SUBSTITUTE("&1/tms/Order",
                                                               acURIPrefix).
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

   METHOD PRIVATE LOGICAL mVerifyStatusOK
      ( ioJsonObject AS JsonObject ):

      IF NOT ioJsonObject:Has("result")
      THEN UNDO, THROW NEW AppError
         ("Response data doesn't have result property",
          {&PROCOMMANDSTATUS_INVALID_RESPONSE_BODY}).

      IF ioJsonObject:GetType("result") NE JsonDataType:STRING
      THEN UNDO, THROW NEW AppError
         ("Response data result property is not a string",
          {&PROCOMMANDSTATUS_INVALID_RESPONSE_BODY}).

      IF ioJsonObject:GetCharacter("result") EQ "OK"
      THEN RETURN TRUE.

      RETURN FALSE.

   END METHOD.

   /* Return value:
         TRUE  => No connection to the server, please wait.
         FALSE => no need to wait
   */
   METHOD PRIVATE LOGICAL mSendProCommand
      (iiProCommandId AS INT64,
       icURI AS CHARACTER):

      DEFINE VARIABLE lmpJsonData    AS MEMPTR              NO-UNDO.
      DEFINE VARIABLE loReqJsonData  AS CLASS JsonConstruct NO-UNDO.
      DEFINE VARIABLE loRestError    AS CLASS RESTError     NO-UNDO.

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
      THEN RETURN FALSE.

      IF ProCommand.MsRequest > 0
      THEN DO:
         FIND MsRequest EXCLUSIVE-LOCK WHERE
              MsRequest.MsRequest = ProCommand.MsRequest NO-ERROR NO-WAIT.

         IF NOT AVAILABLE MsRequest
         THEN DO:
            IF LOCKED MsRequest
            THEN RETURN FALSE.
            ELSE llRequestNotAvailable = TRUE.
         END.
      END.

      DO ON ERROR UNDO, THROW:

         IF llRequestNotAvailable
         THEN UNDO, THROW NEW AppError
                (SUBSTITUTE("MsRequest id &1 doesn't exists",
                            ProCommand.MsRequest),
                 {&PROCOMMANDSTATUS_FAULTY_RECORD}).

         IF LOOKUP(ProCommand.ProCommandVerb,acProCommandVerbList) EQ 0
         THEN UNDO, THROW NEW AppError
                (SUBSTITUTE("Invalid rest method &1 defined to ProCommandVerb",
                            ProCommand.ProCommandVerb),
                 {&PROCOMMANDSTATUS_FAULTY_RECORD}).

         IF LOOKUP(ProCommand.ProCommandTarget,acProCommandTargetList) EQ 0
         THEN UNDO, THROW NEW AppError
                (SUBSTITUTE("Invalid ProCommandTarget &1",
                            ProCommand.ProCommandTarget),
                 {&PROCOMMANDSTATUS_FAULTY_RECORD}).

         IF ProCommand.CommandLine NE ?
         THEN DO ON ERROR UNDO, THROW:
            COPY-LOB FROM OBJECT ProCommand.CommandLine TO OBJECT lmpJsonData NO-CONVERT.
            loReqJsonData = aoOMParser:Parse(lmpJsonData).

            CATCH loError AS Progress.Lang.Error :
               UNDO, THROW NEW AppError("ProCommand.CommandLine contains invalid json data",
                                        {&PROCOMMANDSTATUS_FAULTY_RECORD}).
            END CATCH.
         END.

         DO ON ERROR UNDO, THROW:
            aoRESTClient:mSetURIPath(icURI).
            CATCH loError AS Progress.Lang.Error :
               UNDO, THROW NEW AppError(SUBSTITUTE("Setting URI path '&1' failed",
                                                   icURI),
                                        {&PROCOMMANDSTATUS_INTERNAL_PROBLEM}).
            END.
         END.

         /* After this line it is sure that the ProCommand record itself is valid
            (otherwise error would have been thrown).
            => We can store the SendTS

            Note! ldtSendTS will be stored to ProCommand.SendTS in
            the finally block */
         ldtSendTS = NOW.

         CASE VerbEnum:GetEnum(ProCommand.ProCommandVerb):
            WHEN VerbEnum:POST
            THEN aoRESTClient:mPOST(loReqJsonData).   /* loReqJsonData EQ ? is supported. */
            WHEN VerbEnum:PATCH
            THEN aoRESTClient:mPATCH(loReqJsonData).  /* loReqJsonData EQ ? is supported. */
            WHEN VerbEnum:PUT
            THEN aoRESTClient:mPUT(loReqJsonData).    /* loReqJsonData EQ ? is supported. */
            WHEN VerbEnum:DELETE
            THEN aoRESTClient:mDELETE().
            WHEN VerbEnum:GET
            THEN aoRESTClient:mGET().
         END.

         /* After this line it is sure that the rest call was successful
            (otherwise error would have been thrown).
            => We can store the ResponseTS

            Note! ldtResponseTS will be stored to ProCommand.ResponseTS in
            the finally block */
         ASSIGN
            ldtResponseTS         = NOW
            SET-SIZE(lmpJsonData) = 0.

         IF NOT VALID-OBJECT(aoRESTClient:aoResultObject)
         THEN UNDO, THROW NEW AppError("Result body is empty",
                                       {&PROCOMMANDSTATUS_INVALID_RESPONSE_BODY}).

         IF NOT TYPE-OF(aoRESTClient:aoResultObject, JsonObject)
         THEN UNDO, THROW NEW AppError("Result body is not json object",
                                       {&PROCOMMANDSTATUS_INVALID_RESPONSE_BODY}).

         IF NOT CAST(aoRESTClient:aoResultObject, JsonConstruct):Write(lmpJsonData, YES)
         THEN UNDO, THROW NEW AppError("Cannot parse json from result body",
                                     {&PROCOMMANDSTATUS_INTERNAL_PROBLEM}).

         DO ON ERROR UNDO, THROW:
            COPY-LOB FROM OBJECT lmpJsonData TO OBJECT ProCommand.Response NO-CONVERT.
            CATCH loError AS Progress.Lang.Error:
               UNDO, THROW NEW AppError("Cannot store json response to database clob field",
                                        {&PROCOMMANDSTATUS_INTERNAL_PROBLEM}).
            END CATCH.
         END.

         /*
            mVerifyStatusOK will throw also error when the
            request body is invalid.

            Note! SendTS and ResponseTS will be set in finally block
         */
         IF mVerifyStatusOK(CAST(aoRESTClient:aoResultObject, JsonObject))
         THEN CASE TargetEnum:GetEnum(ProCommand.ProCommandTarget):
                 WHEN TargetEnum:BPM
                 THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_WAITING_ACCEPT_NOTIFY}.
                 WHEN TargetEnum:NB_AS OR WHEN TargetEnum:NB_CH
                 THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_DONE}.
              END CASE.
         ELSE ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_KO_RESPONSE}.

         CATCH loError AS Error:

            /* RESTClient class will always throw RESTError when in error */
            IF TYPE-OF(loError, RESTError)
            THEN DO:
               loRESTError = CAST(loError, RESTError).
               /* Connection failure for host <host_name> port <port> transport
                  <transport_name>. (9407)

                  => Lets not touch the promessage record but instead
                     lets wait until the connection is established */
               IF loRESTError:GetMessageNum(1) EQ 9407
               THEN RETURN TRUE.

               IF loRESTError:ErrorMessage > ""
               THEN COPY-LOB FROM OBJECT loRESTError:ErrorMessage TO OBJECT ProCommand.Response.
               ELSE COPY-LOB FROM OBJECT loRESTError:mGetErrorData() TO OBJECT ProCommand.Response.

               CASE loRESTError:HTTPStatus:
                  WHEN 0
                  THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_INTERNAL_PROBLEM}.
                  WHEN 400 /* 400 Bad Request */
                  THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_HTTP_400_RESPONSE}.
                  WHEN 500 /* 500 Internal server error */
                  THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_HTTP_500_RESPONSE}.
                  OTHERWISE
                       ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_HTTP_UNSUPPORTED_RESPONSE}.
               END CASE.
            END.

            ELSE IF TYPE-OF(loError, AppError)
            THEN DO:
               CASE loError:GetMessageNum(1):
                  WHEN {&PROCOMMANDSTATUS_FAULTY_RECORD} OR
                  WHEN {&PROCOMMANDSTATUS_INVALID_RESPONSE_BODY}
                  THEN ProCommand.ProCommandStatus = loError:GetMessageNum(1).
                  OTHERWISE
                       ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_INTERNAL_PROBLEM}.
               END CASE.

               IF CAST(loError,AppError):ReturnValue > ""
               THEN lcTemp = CHR(10) + CAST(loError,AppError):ReturnValue.

               DO lii = 1 TO loError:NumMessages:
                  lcTemp = lcTemp + CHR(10) + loError:GetMessage(lii).
               END.

               lcTemp = SUBSTRING(lcTemp,2).

               COPY-LOB FROM OBJECT lcTemp TO OBJECT ProCommand.Response.
            END.

            ELSE UNDO, THROW loError.

         END CATCH.

         FINALLY:
            ASSIGN
               ProCommand.SendTS     = ldtSendTS
               ProCommand.ResponseTS = ldtResponseTS.

            IF AVAILABLE MsRequest
            THEN
            CASE ProCommand.ProCommandStatus:
               WHEN {&PROCOMMANDSTATUS_NEW} OR
               WHEN {&PROCOMMANDSTATUS_WAITING_ACCEPT_NOTIFY}
               THEN .
               WHEN {&PROCOMMANDSTATUS_DONE}
               THEN DO:
                  IF Func.MsReqFunc:mAllProCommandOK(MsRequest.MsRequest,
                                                     ROWID(ProCommand))
                  THEN ASSIGN
                          MsRequest.ReqStatus   = {&REQUEST_STATUS_HLR_DONE}
                          MsRequest.UpdateStamp = Func.Common:mMakeTS().
               END.
               OTHERWISE DO:
                  ASSIGN
                     MsRequest.ReqStatus = {&REQUEST_STATUS_REJECTED}
                     MsRequest.DoneStamp = Func.Common:mMakeTS().

                  IF MsRequest.OrigRequest > 0 AND
                     MsRequest.Mandatory   > 0
                  THEN Func.MsReqFunc:mRejectOrigReq(MsRequest.OrigRequest,
                                                     MsRequest.ReqType).
               END.
            END CASE.

         END FINALLY.

      END.

      RETURN FALSE.

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
               IF mSendProCommand(ProCommand.ProCommandId,
                                  SUBSTITUTE("&1&2",
                                          ttProcess.ProCommandURIPrefix,
                                          ProCommand.ProCommandTargetURL))
               THEN DO:
                  LOG-MANAGER:WRITE-MESSAGE("Cannot connect to the host. Waiting...", "ERROR").
                  RETURN TRUE.
               END.
            END.

            mUpdateStatistic(ttProcess.ProcommandTarget,
                             ProCommand.ProCommandType,
                             INTEGER(ProCommand.ProCommandStatus > {&PROCOMMANDSTATUS_NEW}),
                             INTEGER(ProCommand.ProCommandStatus >= {&PROCOMMANDSTATUS_FAULTY_RECORD})).

            liMTIME = MTIME.

            /* Maximum processing time per one round is aiRoundTime.
               No separate wait in the calling side is needed when
               the maximum time is reached.
               In order to handle situation when day is changed
               two possible scenarios must be handled */
            IF ( liStartTime > liMTIME AND
                 liStartTime < 86400000 + liMTIME - aiRoundTime ) OR
               ( liStartTime <= liMTIME AND
                 liStartTime < liMTIME - aiRoundTime )
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