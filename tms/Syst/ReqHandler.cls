 
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
      FIELD ReqType    AS INTEGER
      FIELD ReqStatus  AS INTEGER
      FIELD ProcessAmt AS INT64
      FIELD FailedAmt  AS INT64
      INDEX ReqType IS PRIMARY UNIQUE ReqType ReqStatus.

   DEFINE PRIVATE VARIABLE aiQueue       AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE aiMaxAmount   AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE aiMaxTime     AS INTEGER   NO-UNDO.
   DEFINE PRIVATE VARIABLE acTenants     AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE aiTenantCount AS INTEGER   NO-UNDO.

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

         aiMaxAmount = Syst.Parameters:geti("ReqHandler.MaxAmount", "ReqHandler").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume max amount is 500 */
            aiMaxAmount = 500.
         END CATCH.

      END.

      DO ON ERROR UNDO, THROW:

         aiMaxTime = Syst.Parameters:geti("ReqHandler.MaxTime", "ReqHandler").

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then assume max time is 5000 (milliseconds) */
            aiMaxTime = 5000.
         END CATCH.

      END.

   END CONSTRUCTOR.


   DESTRUCTOR PUBLIC ReqHandler( ):

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
      THEN CREATE ttStatistic.
      
      ASSIGN
         ttStatistic.ProcessAmt = ttStatistic.ProcessAmt + iiProcessed
         ttStatistic.FailedAmt  = ttStatistic.FailedAmt + iiProcessed.
         
   END METHOD.


   METHOD PUBLIC CHARACTER mGetStatistic():

      DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.

      FOR EACH ttStatistic:
         lcReturnValue = lcReturnValue + CHR(10) +
                SUBSTITUTE("ReqType=&1, ReqStatus=&2: Processed=&3, Failed=&4",
                           ttStatistic.ReqType,
                           ttStatistic.ReqStatus,
                           ttStatistic.ProcessAmt,
                           ttStatistic.FailedAmt).
      END.
      
      RETURN LEFT-TRIM(lcReturnValue, CHR(10)).
      
   END METHOD. 


   METHOD PUBLIC LOGICAL mProcess():

      DEFINE VARIABLE ldeActTime     AS DECIMAL   NO-UNDO.
      DEFINE VARIABLE liStartTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liCount        AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liResult       AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcProgram      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lii            AS INTEGER   NO-UNDO.
      
      liStartTime = MTIME.
      
      FOR
         EACH RequestType NO-LOCK WHERE
              RequestType.Brand = "1"      AND
              RequestType.Queue = aiQueue  AND
              RequestType.InUse = TRUE     AND
              RequestType.Mode NE "BATCH",
         EACH RequestStatus NO-LOCK WHERE
              RequestStatus.Brand = "1" AND
              RequestStatus.ReqType = RequestType.ReqType
         BREAK BY RequestStatus.ReqType:
            
         IF FIRST-OF(RequestStatus.ReqType)
         THEN DO:
            ldeActTime = mMake2DT(TODAY,TIME).
         END.

         lcProgram = IF RequestStatus.Program > ""
                     THEN RequestStatus.Program
                     ELSE RequestType.Program.

         IF SEARCH(lcProgram) = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError
                  (SUBSTITUTE("Module &1 not found", lcProgram), 1).

         DO lii = 1 TO aiTenantCount:
            
            multitenancy.TenantInformation:mSetEffectiveTenant(ENTRY(lii,acTenants)).
            
            FOR
               EACH MsRequest NO-LOCK WHERE
                    MsRequest.Brand     = "1"                     AND
                    MsRequest.ReqType   = RequestStatus.ReqType   AND     
                    MsRequest.ReqStatus = RequestStatus.ReqStatus AND   
                    MsRequest.ActStamp <= ldeActTime /* scheduled or immediate */
               BY MsRequest.ActStamp
               BY MsRequest.MsRequest:
   
               /* there is a possibility that another process has just started
                  handling this same request */
               IF MsRequest.ReqStatus NE RequestStatus.ReqStatus THEN NEXT.
   
               liCount = liCount + 1.
   
               RUN VALUE(IF RequestStatus.Program > ""
                         THEN RequestStatus.Program
                         ELSE RequestType.Program) (MsRequest.MsRequest).
   
               IF MsRequest.ReqType = {&REQTYPE_RERATE}
               THEN DO:
                  liResult = INTEGER(RETURN-VALUE) NO-ERROR.
                  IF NOT ERROR-STATUS:ERROR
                  THEN mUpdateStatistic(RequestStatus.ReqType,
                                        RequestStatus.ReqStatus,
                                        liResult,
                                        0).
               END.
               ELSE DO:
                  IF NOT RETURN-VALUE BEGINS "ERROR"
                  THEN mUpdateStatistic(RequestStatus.ReqType,
                                        RequestStatus.ReqStatus,
                                        INTEGER(MsRequest.ReqStatus > 1),
                                        INTEGER(MsRequest.ReqStatus = 3)).
               END.
   
               IF liCount > aiMaxAmount OR liStartTime >= ABSOLUTE(MTIME - aiMaxTime)
               THEN RETURN FALSE.
            END.
         END.
      END.
      
      /* We will go there only when there are no request or we got every
         request handled */
      RETURN TRUE.
   
   END METHOD.

END CLASS.