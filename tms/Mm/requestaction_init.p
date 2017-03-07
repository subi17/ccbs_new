/* ----------------------------------------------------------------------------
  MODULE .......: requestaction_init.p
  FUNCTION .....: Execute request actions 
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 11.11.10
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/fmakemsreq.i}
{Func/service.i}
{Func/cparam2.i}
{Mm/requestaction_exec.i}
{Func/fsendsms.i}

DEF INPUT PARAMETER iiMsRequest  AS INT  NO-UNDO.

DEF VAR liPayType    AS INT  NO-UNDO.
DEF VAR liReqTime    AS INT  NO-UNDO.

DEF VAR lcPostpaidVoiceTariffs AS CHAR NO-UNDO.

DEF VAR lhRequest    AS HANDLE NO-UNDO.

DEF BUFFER bOrigRequest FOR MsRequest.


/****** Main start **********/

FIND FIRST bOrigRequest WHERE bOrigRequest.MsRequest = iiMsRequest
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE bOrigRequest THEN RETURN "ERROR:Unknown request".

lhRequest = BUFFER bOrigRequest:HANDLE.

FIND FIRST MsOwner WHERE 
           MsOwner.MsSeq = bOrigRequest.MsSeq AND  
           MsOwner.TSEnd >= bOrigRequest.CreStamp NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsOwner THEN 
   RETURN "ERROR:Subscription not available".

FIND FIRST CLIType WHERE
           CLIType.Brand   = gcBrand AND
           CLIType.CLIType = MsOwner.CLIType NO-LOCK NO-ERROR.
IF AVAILABLE CLIType THEN liPayType = CLIType.PayType.

lcPostpaidVoiceTariffs = fCParamC("POSTPAID_VOICE_TARIFFS").

RUN pCollectRequestActions(MsOwner.MsSeq,
                           lhRequest,
                           MsOwner.CLIType,
                           liPayType,
                           bOrigRequest.ReqType,
                           TODAY,
                           "12").

RUN pRequestActions(MsOwner.MsSeq,
                    lhRequest,
                    MsOwner.CLIType).

/****** Main end **********/


PROCEDURE pRequestActions:

   DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO.
   DEF INPUT PARAMETER ihRequest AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER icCLIType AS CHAR NO-UNDO.

   DEF VAR llAllowed AS LOG  NO-UNDO.
   
   DEF BUFFER bAction FOR ttAction.
   
   /* execute */
   FOR EACH ttAction,
      FIRST RequestAction NO-LOCK WHERE
            RequestAction.RequestActionID = ttAction.ActionID:
 
      /* additional rules defined */
      RUN pDoRulesAllow(iiMsSeq,
                        icCLIType,
                        ihRequest,
                        TODAY,
                        RequestAction.RequestActionID,
                        ttAction.ActionType,
                        ttAction.ActionKey,
                        OUTPUT llAllowed).


      IF NOT llAllowed THEN NEXT.
   
      CASE ttAction.ActionType:

      WHEN "MsRequest" THEN DO:
         RUN pMsRequest(iiMsSeq,
                        ttAction.ActionKey,
                        ihRequest::MsRequest).
      END.

      END CASE.
         
   END.  /* CLITypeAction */
      
END PROCEDURE.

PROCEDURE pMsRequest:

   DEF INPUT PARAMETER iiMsSeq       AS INT  NO-UNDO.
   DEF INPUT PARAMETER icActionKey   AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiOrigRequest AS INT  NO-UNDO.
    
   DEF VAR liCount       AS INT  NO-UNDO.
   DEF VAR liPendingType AS INT  NO-UNDO.

   DEF VAR lcAllowedBONOContracts   AS CHAR NO-UNDO.
   DEF VAR lcBONOContracts          AS CHAR NO-UNDO.

   DEF BUFFER bMemoRequest FOR MsRequest.

   IF iiMsSeq = 0 THEN RETURN.

   ASSIGN lcAllowedBONOContracts = fCParamC("ALLOWED_BONO_CONTRACTS")
          lcBONOContracts  = fCParamC("BONO_CONTRACTS").

   FIND FIRST bOrigRequest WHERE
              bOrigRequest.MsRequest = iiOrigRequest NO-LOCK NO-ERROR.

   DO liCount = 1 TO NUM-ENTRIES(icActionKey):
   
      liPendingType = INTEGER(ENTRY(liCount,icActionKey)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN NEXT. 
      
      FIND FIRST RequestType WHERE
                 RequestType.Brand   = gcBrand AND
                 RequestType.ReqType = liPendingType
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE RequestType THEN NEXT.

      CASE RequestAction.Action:
          
      /* termination (cancellation) */
      WHEN 12 THEN DO:
         FOR EACH MsRequest NO-LOCK WHERE
                  MsRequest.MsSeq   = iiMsSeq AND
                  MsRequest.ReqType = liPendingType AND
                  LOOKUP(STRING(MsRequest.ReqStatus),
                         {&REQ_INACTIVE_STATUSES}) = 0:
               
            /* Extra logic to prevent cancellation of BONO BTC request */
            /* if STC request is for Voice to Voice                    */
            IF AVAILABLE bOrigRequest AND
               bOrigRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
               LOOKUP(bOrigRequest.ReqCparam1,lcPostpaidVoiceTariffs) > 0 AND
               LOOKUP(bOrigRequest.ReqCparam2,lcPostpaidVoiceTariffs) > 0 AND
               LOOKUP(MsRequest.ReqCparam1,lcBONOContracts) > 0 THEN NEXT.
            
            /* Special check for BTC to MDUB9 + STC to IPL */
            IF AVAILABLE bOrigRequest AND
               bOrigRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
               LOOKUP(bOrigRequest.ReqCparam1,lcPostpaidVoiceTariffs) > 0 AND
               bOrigRequest.ReqCparam2 EQ "CONTD" AND
               LOOKUP(MsRequest.ReqCparam1,lcAllowedBonoContracts) > 0 THEN NEXT.

            IF NOT fReqStatus({&REQUEST_STATUS_CANCELLED},
                              "New request " + STRING(iiOrigRequest) + 
                              " overrides this one.")
            THEN DO:
               FIND FIRST bMemoRequest WHERE 
                  bMemoRequest.MsRequest = iiOrigRequest 
                  EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE bMemoRequest THEN DO:
                  bMemoRequest.Memo = bMemoRequest.Memo + 
                                      (IF bMemoRequest.Memo > "" 
                                       THEN " " ELSE "") + 
                                      "Cancellation of request " +
                                      STRING(MsRequest.MsRequest) + 
                                      " failed.".
                  RELEASE bMemoRequest.
               END.   
            END.                              
            /* Send a SMS for Voice Bundle if BTC request is being canceled */
            ELSE IF MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
               LOOKUP(MsRequest.ReqCParam1,lcBONOContracts) > 0 AND
               LOOKUP(MsRequest.ReqCParam2,lcBONOContracts) > 0 THEN
               RUN pSendSMS(INPUT MsRequest.MsSeq, INPUT MsRequest.MsRequest,
                            INPUT "BTCDeAct", INPUT 10,
                            INPUT {&UPSELL_SMS_SENDER}, INPUT "").
         END.
                  
      END.

      END CASE.
   END.
   
END PROCEDURE.


