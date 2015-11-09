/* ----------------------------------------------------------------------------
  MODULE .......: requestaction_check.p
  FUNCTION .....: Execute request checks 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 18.09.08
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{barrfunc.i}
{requestaction_exec.i}

DEF INPUT  PARAMETER iiReqType    AS INT  NO-UNDO.
DEF INPUT  PARAMETER icCLIType    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiMsSeq      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError      AS CHAR NO-UNDO. 

DEF VAR lcBarrStatus  AS CHAR NO-UNDO. 
DEF VAR lcBarrComList AS CHAR NO-UNDO. 
DEF VAR lhRequest     AS HANDLE NO-UNDO.
DEF VAR liPayType     AS INT  NO-UNDO.
DEF VAR ldtReqDate    AS DATE NO-UNDO.

DEF BUFFER bfMobsub FOR Mobsub. 


FUNCTION fRequestCheck RETURNS LOGICAL:

   DEF BUFFER bCliType FOR CLIType.
   DEF VAR lcBundleBsdClitypes AS CHAR NO-UNDO. 
   lcBundleBsdClitypes = fCParamC("BUNDLE_BASED_CLITYPES").
   IF RequestAction.ActionType = "RequestsNotAllowed" 
      AND iiMsSeq > 0 THEN DO:
   
      FOR EACH MSrequest WHERE 
               MSRequest.MSSeq = iiMSSeq NO-LOCK:
         IF LOOKUP(STRING(MSRequest.ReqType),RequestAction.ActionKey) > 0
         AND LOOKUP(STRING(MSrequest.reqstatus),{&REQ_INACTIVE_STATUSES}) = 0
         THEN DO:
            /* Allow STC/BTC request even ongoing Termination request
               for additional lines, because of YTS-6053 */
            IF (LOOKUP(bfMobSub.CLIType,lcBundleBsdClitypes) = 0 AND
               CLIType.LineType = 2) OR
               (LOOKUP(bfMobSub.CLIType,lcBundleBsdClitypes) > 0 AND
               CAN-FIND( FIRST bCLIType NO-LOCK WHERE 
                               bCLIType.Brand = gcBrand AND
                               bCLIType.CLIType = bfMobSub.TariffBundle AND
                               bCLIType.LineType = 2)) THEN DO:
                  IF (MsRequest.ReqCParam3 EQ STRING({&SUBSCRIPTION_TERM_REASON_MULTISIM}) OR
                   MsRequest.ReqCParam3 EQ STRING({&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM}) ) AND
                  MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                 (iiReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                  iiReqType = {&REQTYPE_BUNDLE_CHANGE}) THEN NEXT.
            END.
            /* YDR-2036 
            Allow STC when there is an ongoing 
            MNP out request with ACON status
            */
             IF (iiReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                 iiReqType = {&REQTYPE_BUNDLE_CHANGE}) AND            
               MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
               MsRequest.ReqCParam3 EQ STRING({&SUBSCRIPTION_TERM_REASON_MNP}
               THEN NEXT.
                
            ocError = {&MSG_ONG_REQUEST}.
            RETURN FALSE.
         END.
         
      END. /* FOR EACH MSrequest */
   END.
   ELSE IF RequestAction.ActionType = "BarringsNotAllowed" 
      AND iiMsSeq > 0 THEN DO:
      IF bfMobsub.MsStatus = 8 THEN DO:
         IF fIsInList(fGetActiveBarrings(iiMsSeq),
                      RequestAction.ActionKey) EQ TRUE THEN DO:
            ocError = {&MSG_NOT_ALLOWED}.         
            RETURN FALSE.
         END.
      END.

   END.      

END FUNCTION. 


/******* Main start ******/

ldtReqDate = TODAY.

IF iiMsSeq = 0 THEN
   RETURN ({&MSG_NOT_ALLOWED}).

IF iiMsSeq > 0 THEN DO:
   FIND FIRST bfMobsub WHERE
      bfMobsub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL bfMobsub THEN DO:
      RETURN ({&MSG_NOT_ALLOWED}).
   END.
END.

FIND FIRST CLIType WHERE
           CLIType.Brand   = gcBrand AND
           CLIType.CLIType = icCLIType NO-LOCK NO-ERROR.
IF AVAILABLE CLIType THEN liPayType = CLIType.PayType.

RUN pCollectRequestActions(iiMsSeq,
                           lhRequest,
                           icCLIType,
                           liPayType,
                           iiReqType,
                           ldtReqDate,
                           "4").
RUN pRequestActions.

/******* Main end ******/


PROCEDURE pRequestActions:
   
   FOR EACH ttAction,
      FIRST RequestAction NO-LOCK WHERE
            RequestAction.RequestActionID = ttAction.ActionID:
 
      IF NOT fRequestCheck() THEN RETURN.
   END.

END PROCEDURE.


