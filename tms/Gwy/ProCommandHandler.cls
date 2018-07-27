/*------------------------------------------------------------------------
    File        : ProCommandHandler
    Purpose     : 
    Syntax      : 
    Description : Go through ProCommand records and send them
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING PROGRESS.Json.ObjectModel.*.
USING Gwy.ParamRESTClient.

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
      FIELD LastUpdated AS DATETIME-TZ
      INDEX ProCommandTarget IS PRIMARY UNIQUE ProCommandTarget ProCommandType.

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
         ttStatistic.LastUpdated = NOW.
         
   END METHOD.


   METHOD PRIVATE VOID mSendProCommand
      (iiProCommandId AS INT64,
       icURI AS CHARACTER):

      DEFINE VARIABLE lmpJsonData AS MEMPTR NO-UNDO.

      DEFINE BUFFER ProCommand FOR ProCommand.

      FIND ProCommand EXCLUSIVE-LOCK WHERE
           ProCommand.ProCommandId = iiProCommandId NO-ERROR NO-WAIT.

      IF NOT AVAILABLE ProCommand
      THEN RETURN.

      aoRESTClient:mSetURIPath(icURI).

      COPY-LOB FROM OBJECT ProCommand.CommandLine TO OBJECT lmpJsonData NO-CONVERT. 

      ProCommand.SendTS = NOW.

      aoRESTClient:mPOST(aoOMParser:Parse(lmpJsonData)).

      SET-SIZE(lmpJsonData) = 0.

      /* Logic to get the status from aoRESTClient:aoResultObject is needed... */
      IF TYPE-OF(aoRESTClient:aoResultObject, Progress.Json.ObjectModel.JsonConstruct)
      THEN CAST(aoRESTClient:aoResultObject, Progress.Json.ObjectModel.JsonConstruct):Write(lmpJsonData, YES).

      COPY-LOB FROM OBJECT lmpJsonData TO OBJECT ProCommand.Response NO-CONVERT.

      CASE ProCommand.ProCommandTarget:
         WHEN "BPM"
         THEN ProCommand.ProCommandStatus = 1.
         WHEN "NB_AO" OR WHEN "NB_CH"
         THEN ProCommand.ProCommandStatus = 2.
         OTHERWISE ProCommand.ProCommandStatus = 3.
      END CASE.

      CATCH loJsonError AS Progress.Json.JsonError:
         ProCommand.ProCommandStatus = 3.
      END CATCH.

      /* Better error handling is needed */
      CATCH loRESTError AS Gwy.RESTError:
         ProCommand.ProCommandStatus = 3.
      END CATCH.

      FINALLY:
         SET-SIZE(lmpJsonData) = 0.
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
                 ProCommand.ProCommandStatus = 0
            BY ProCommand.CreStamp:

            /* there is a possibility that another process has just started
               handling this same request */
            IF ProCommand.ProCommandStatus NE 0 THEN NEXT.
            
            DO TRANSACTION:
               mSendProCommand(ProCommand.ProCommandId,
                               SUBSTITUTE(&1&2,
                                          ttProcess.ProCommandURIPrefix,
                                          ProCommand.ProCommandTargetURL)).
            END.

            mUpdateStatistic(ttProcess.ProcommandTarget,
                             ProCommand.ProCommandType,
                             INTEGER(ProCommand.ProCommandStatus > 0),
                             INTEGER(ProCommand.ProCommandStatus EQ 3)).

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

         lcc = lcc + ", " + SUBSTITUTE("&1/&2: P&3, F&4",
                                       ttStatistic.ProCommandTarget,
                                       ttStatistic.ProCommandType,
                                       ttStatistic.ProcessAmt,
                                       ttStatistic.FailedAmt).

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