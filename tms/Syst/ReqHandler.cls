 
 /*------------------------------------------------------------------------
    File        : ReqHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Tue Oct 24 16:52:13 EEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Syst.ReqHandler: 

   DEFINE PRIVATE TEMP-TABLE ttStatistic NO-UNDO
      FIELD ReqType     AS INTEGER
      FIELD ReqStatus   AS INTEGER
      FIELD ProcessAmt  AS INT64
      FIELD FailedAmt   AS INT64
      FIELD LastUpdated AS DATETIME-TZ
      INDEX ReqType IS PRIMARY UNIQUE ReqType ReqStatus.

   DEFINE PRIVATE TEMP-TABLE ttProcess NO-UNDO
      FIELD ReqType    AS INTEGER
      FIELD ReqStatus  AS INTEGER
      FIELD TenantName AS CHARACTER
      FIELD Program    AS CHARACTER
      FIELD LastProcessed AS DATETIME-TZ
      INDEX TenantName IS PRIMARY TenantName LastProcessed ReqType ReqStatus.

   DEFINE PRIVATE VARIABLE aiQueue         AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE aiMaxTime       AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acTenants       AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE aiTenantCount   AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acCurrentTenant AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ahQuery         AS HANDLE    NO-UNDO.

   CONSTRUCTOR PUBLIC ReqHandler
      (iiQueue AS INTEGER):

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      aiQueue = iiQueue.
      
      DO lii = 1 TO NUM-ENTRIES(multitenancy.TenantInformation:TenantList):
         IF ENTRY(lii,multitenancy.TenantInformation:TenantList) =
            multitenancy.TenantInformation:SuperTenant
         THEN NEXT.
         
         ASSIGN
            acTenants = acTenants + "," +
                        ENTRY(lii,multitenancy.TenantInformation:TenantList)
            aiTenantCount = aiTenantCount + 1.
                     
      END.

      acTenants = LEFT-TRIM(acTenants,",").
      
      IF aiTenantCount = 0
      THEN RETURN ERROR NEW Progress.Lang.AppError
                  ("Cannot find any tenants", 1).

      DO ON ERROR UNDO, THROW:

         aiMaxTime = Syst.Parameters:geti("ReqHandler.MaxTime", "ReqHandler").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume max time is 5000 (milliseconds) */
            aiMaxTime = 5000.
         END CATCH.

      END.

      CREATE QUERY ahQuery.

      ahQuery:SET-BUFFERS(BUFFER ttProcess:HANDLE).
      /* Preselect is needed as we are updating the field which is part
         of the index */
      ahQuery:QUERY-PREPARE('PRESELECT EACH ttProcess NO-LOCK BREAK BY ttProcess.TenantName').

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC ReqHandler( ):

      IF VALID-HANDLE(ahQuery)
      THEN DO:
         ahQuery:QUERY-CLOSE().
         DELETE OBJECT ahQuery.
      END.

      EMPTY TEMP-TABLE ttProcess.
      EMPTY TEMP-TABLE ttStatistic.
            
   END DESTRUCTOR.


   METHOD PRIVATE DECIMAL mMake2DT 
      (INPUT ldaTSDate AS DATE,
       INPUT liTSTime  AS INTEGER):
   
      DEFINE VARIABLE ldeRet AS DECIMAL NO-UNDO FORMAT "99999999.99999".

      ldeRet = YEAR(ldaTSDate) * 10000 +
               MONTH(ldaTSDate) * 100 +
               DAY(ldaTSDate) +
               (liTSTime / 100000).

      RETURN ldeRet.

   END METHOD.


   METHOD PRIVATE VOID mUpdateStatistic 
      (INPUT iiReqType   AS INTEGER,
       INPUT iiReqStatus AS INTEGER,
       INPUT iiProcessed AS INT64,
       INPUT iiFailed    AS INT64):
   
      FIND ttStatistic WHERE
        ttStatistic.ReqType   = iiReqType AND
        ttStatistic.ReqStatus = iiReqStatus
      NO-ERROR.
      
      IF NOT AVAILABLE ttStatistic
      THEN DO:
         CREATE ttStatistic.
         ASSIGN
            ttStatistic.ReqType   = iiReqType
            ttStatistic.ReqStatus = iiReqStatus.
      END.
      
      ASSIGN
         ttStatistic.ProcessAmt  = ttStatistic.ProcessAmt + iiProcessed
         ttStatistic.FailedAmt   = ttStatistic.FailedAmt + iiFailed
         ttStatistic.LastUpdated = NOW.
         
   END METHOD.

   METHOD PUBLIC VOID mUpdateProcess():

      EMPTY TEMP-TABLE ttProcess.

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      FOR
         EACH RequestType NO-LOCK WHERE
              RequestType.Brand = "1"      AND
              RequestType.Queue = aiQueue  AND
              RequestType.InUse = TRUE     AND
              RequestType.Mode NE "BATCH",
         EACH RequestStatus NO-LOCK WHERE
              RequestStatus.Brand   = "1"                 AND
              RequestStatus.ReqType = RequestType.ReqType AND
              RequestStatus.InUse   = TRUE:

         DO lii = 1 TO aiTenantCount:
            CREATE ttProcess.
            ASSIGN
               ttProcess.ReqType       = RequestType.ReqType
               ttProcess.ReqStatus     = RequestStatus.ReqStatus
               ttProcess.Tenant        = ENTRY(lii,acTenants)
               ttProcess.Program       = IF RequestStatus.Program > ""
                                         THEN RequestStatus.Program
                                         ELSE RequestType.Program
               ttProcess.LastProcessed = NOW.
         END.

         IF SEARCH(ttProcess.Program) = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError
                  (SUBSTITUTE("Module &1 not found", ttProcess.Program), 1).
      END.

   END METHOD.

   METHOD PUBLIC VOID mLogStatistic
      (idttzLastCheck AS DATETIME-TZ):

      DEFINE VARIABLE lcc AS CHARACTER NO-UNDO.

      FOR EACH ttStatistic:

         IF ttStatistic.LastUpdated < idttzLastCheck
         THEN NEXT.

         lcc = lcc + ", " + SUBSTITUTE("&1/&2: P&3, F&4",
                                       ttStatistic.ReqType,
                                       ttStatistic.ReqStatus,
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

   METHOD PUBLIC LOGICAL mProcess():

      DEFINE VARIABLE ldeActTime     AS DECIMAL   NO-UNDO.
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
            ldeActTime              = mMake2DT(TODAY,TIME)
            ttProcess.LastProcessed = NOW.

         FOR
            EACH MsRequest NO-LOCK WHERE
                 MsRequest.Brand     = "1"                 AND
                 MsRequest.ReqType   = ttProcess.ReqType   AND
                 MsRequest.ReqStatus = ttProcess.ReqStatus AND
                 MsRequest.ActStamp <= ldeActTime /* scheduled or immediate */
            BY MsRequest.ActStamp
            BY MsRequest.MsRequest:

            /* there is a possibility that another process has just started
               handling this same request */
            IF MsRequest.ReqStatus NE ttProcess.ReqStatus THEN NEXT.

            RUN VALUE(ttProcess.Program) (MsRequest.MsRequest).

            IF MsRequest.ReqType = {&REQTYPE_RERATE}
            THEN DO:
               liResult = INTEGER(RETURN-VALUE) NO-ERROR.
               IF NOT ERROR-STATUS:ERROR
               THEN mUpdateStatistic(ttProcess.ReqType,
                                     ttProcess.ReqStatus,
                                     liResult,
                                     0).
            END.
            ELSE DO:
               IF NOT RETURN-VALUE BEGINS "ERROR"
               THEN mUpdateStatistic(ttProcess.ReqType,
                                     ttProcess.ReqStatus,
                                     INTEGER(MsRequest.ReqStatus > 1),
                                     INTEGER(MsRequest.ReqStatus = 3)).
            END.

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

      /* We will be there only when there are no request or we got every
         request handled */
      RETURN TRUE.

      FINALLY:
         ahQuery:QUERY-CLOSE().
      END FINALLY.
   
   END METHOD.

END CLASS.