/* ----------------------------------------------------------------------
  MODULE .......: limitrequest.p 
  TASK .........: Handles msrequests with reqtype 40
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 12.05.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/eventval.i}
{Func/msreqfunc.i}
{Func/flimitreq.i}


IF llDoEvent THEN DO:

   {lib/eventlog.i}
   
   DEFINE VARIABLE lhLimit     AS HANDLE    NO-UNDO.
   
   lhLimit = BUFFER Limit:HANDLE.
                  
   RUN StarEventInitialize(lhLimit).
                                    
END.

DEFINE INPUT PARAMETER iiMSrequest AS INT NO-UNDO.

/* msrequest parameters */
DEFINE VARIABLE icOperation AS CHARACTER NO-UNDO.
DEFINE VARIABLE ideNewValue AS DECIMAL NO-UNDO EXTENT 2.
DEFINE VARIABLE ideLimitPerc AS DECIMAL NO-UNDO.
DEFINE VARIABLE iiLimitType AS INTEGER NO-UNDO.
DEFINE VARIABLE iiTMRuleSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE ilDefValue  AS LOG     NO-UNDO.

DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

FIND FIRST MSRequest WHERE 
           MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 40 THEN DO:
   fCleanEventObjects().
   RETURN "ERROR".
END.

ASSIGN
   icOperation    = MsRequest.ReqCParam1 
   ideNewValue[1] = MsRequest.ReqDParam1
   ideNewValue[2] = MsRequest.ReqDParam2
   iiLimitType    = MsRequest.ReqIParam1
   iiTMRuleSeq    = MsRequest.ReqIParam2.

do-trans:
DO TRANSACTION ON ERROR UNDO:

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN DO:
      fCleanEventObjects().
      RETURN "ERROR".
   END.
 
   ilDefValue = LOGICAL(MsRequest.ReqIParam3) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fCleanEventObjects().
      fReqError("Not valid default value param"). 
      RETURN.
   END.

   FIND FIRST TMRLimit WHERE
      TMRLimit.TMRuleSeq = iiTMRuleSeq AND
      TMRLimit.LimitId >= 3 AND
      TMRLimit.ToDate >= TODAY NO-LOCK USE-INDEX LimitID NO-ERROR. 
   
   IF AVAIL TMRLimit THEN DO:
      fCleanEventObjects().
      fReqError("Maximum 2 limit values are supported"). 
      RETURN.
   END.

   FOR EACH TMRLimit WHERE
      TMRLimit.TMRuleSeq = iiTMRuleSeq AND
      TMRLimit.ToDate   >= TODAY NO-LOCK:

      IF icOperation EQ "Update" THEN DO:
      
         fGetLimit(MsRequest.Custnum, 0,
            {&LIMIT_TYPE_TMRLIMIT}, TMRLimit.LimitId, iiTMRuleSeq, TODAY).
         
         IF NOT AVAIL Limit THEN DO:
            lcError = "Limit ID " + STRING(TMRLimit.LimitId) + " no exists".
            UNDO do-trans.
         END.
  
         fCreateLimitHistory(
            MsRequest.Custnum,
            0,
            {&LIMIT_TYPE_TMRLIMIT},
            ideNewValue[TMRLimit.LimitID],
            TMRLimit.LimitId,
            iiTMRuleSeq,
            FALSE,
            TODAY,
            12/31/2049).
  
      END.
      ELSE IF icOperation EQ "Create" THEN DO:
       
         FIND FIRST Limit WHERE
            Limit.Custnum = MsRequest.Custnum AND
            Limit.LimitType = iiLimitType AND
            Limit.TMRuleSeq = iiTMRuleSeq AND
            Limit.LimitId   = TMRLimit.LimitID AND
            Limit.ToDate  >= TODAY EXCLUSIVE-LOCK NO-ERROR.

         IF AVAIL Limit THEN DO:
            lcError = "Limit ID " + STRING(TMRLimit.LimitId) + 
               " already exists".
            fCleanEventObjects().
            UNDO do-trans.
         END.

         CREATE Limit.
         ASSIGN
            Limit.Custnum   = MsRequest.Custnum
            Limit.MsSeq     = MsRequest.MsSeq
            Limit.DefValue  = ilDefValue
            Limit.FromDate  = TODAY
            Limit.LimitAmt  = ideNewValue[TMRLimit.LimitID]
            Limit.LimitPerc = TMRLimit.LimitPerc 
            Limit.ValueType = TMRLimit.ValueType 
            Limit.LimitType = iiLimitType
            Limit.LimitId   = TMRLimit.LimitId
            Limit.TMRuleSeq = iiTMRuleSeq
            Limit.ToDate    = TMRLimit.ToDate.
         
         IF llDoEvent THEN RUN StarEventMakeCreateEvent( lhLimit ).

      END.
   END.
   

END.
   
IF lcError NE "" THEN DO:
   fReqError(lcError).
END.
ELSE fReqStatus(2,""). /* request handled succesfully */

fCleanEventObjects().

