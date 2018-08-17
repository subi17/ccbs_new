/* ----------------------------------------------------------------------------
  MODULE .......: requestaction_sms.p
  FUNCTION .....: Execute request actions to send the SMS
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 11.11.10
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Mm/requestaction_exec.i}
{Func/fsendsms.i}
{lib/smpp/smpp_defs.i}

DEF INPUT PARAMETER iiMsRequest  AS INT  NO-UNDO.
DEF INPUT PARAMETER icCLIType    AS CHAR NO-UNDO.
DEF INPUT PARAMETER icSource     AS CHAR NO-UNDO.  /* MsRequest.ReqSource */

DEF VAR liPayType                AS INT    NO-UNDO.
DEF VAR lhRequest                AS HANDLE NO-UNDO.

DEF BUFFER bOrigRequest FOR MsRequest.


/****** Main start **********/

FIND FIRST bOrigRequest WHERE bOrigRequest.MsRequest = iiMsRequest
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE bOrigRequest THEN RETURN "ERROR:Unknown request".

lhRequest = BUFFER bOrigRequest:HANDLE.

FIND FIRST CLIType WHERE
           CLIType.Brand   = Syst.Var:gcBrand AND
           CLIType.CLIType = icCLIType NO-LOCK NO-ERROR.
IF AVAILABLE CLIType THEN liPayType = CLIType.PayType.

RUN pCollectRequestActions(bOrigRequest.MsSeq,
                           lhRequest,
                           icCLIType,
                           liPayType,
                           bOrigRequest.ReqType,
                           TODAY,
                           "13").

RUN pRequestActions(bOrigRequest.MsSeq,
                    lhRequest,
                    icCLIType).

/****** Main end **********/


PROCEDURE pRequestActions:

   DEF INPUT PARAMETER iiMsSeq   AS INT    NO-UNDO.
   DEF INPUT PARAMETER ihRequest AS HANDLE NO-UNDO.
   DEF INPUT PARAMETER icCLIType AS CHAR   NO-UNDO.

   DEF VAR llAllowed             AS LOG    NO-UNDO.
   DEF VAR lcSMSName             AS CHAR   NO-UNDO.
   DEF VAR lcSender              AS CHAR   NO-UNDO.
   DEF VAR ldtFromDate           AS DATE   NO-UNDO.
   DEF VAR ldPeriodFromSTC       AS DEC    NO-UNDO.
   
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
   
      ASSIGN lcSMSName = ""
             lcSender  = "".

      CASE ttAction.ActionType:

      WHEN "SMS" THEN DO:
         IF ihRequest::ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND ihRequest::SendSMS = FALSE THEN
            NEXT. 

         IF NUM-ENTRIES(ttAction.ActionKey,"|") >= 1 THEN DO:
            ASSIGN lcSMSName = ENTRY(1,ttAction.ActionKey,"|")
                   lcSender  = ENTRY(2,ttAction.ActionKey,"|") no-error.

            IF lcSender EQ "" THEN lcSender = {&SMPP_DEFAULT_SOURCE_ADDRESS}.
                   
            /* YOT-5872 Separate STC_DONE and iSTC_DONE */
            ldtFromDate = DATE((MONTH(TODAY)), 1, YEAR(TODAY)).
            ldPeriodFromSTC = Func.Common:mMake2DT(ldtFromDate,0).
            IF ihRequest::ActStamp = ldPeriodFromSTC AND 
               lcSMSName = "STC_DONE" THEN 
               lcSMSName = "i" + lcSMSName.

            RUN pSendSMS(iiMsSeq,
                         ihRequest::MsRequest,
                         lcSMSName,
                         ?,
                         lcSender,
                         "").
         END. /* IF NUM-ENTRIES(ttAction.ActionKey,"|") >= 2 THEN DO: */
      END. /* WHEN "SMS" THEN DO: */

      END CASE.
         
   END.  /* CLITypeAction */
      
END PROCEDURE.
