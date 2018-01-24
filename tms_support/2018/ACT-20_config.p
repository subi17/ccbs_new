DEFINE VARIABLE giRequestActionID AS INTEGER NO-UNDO.

FUNCTION fGetNextRequestActionID RETURNS INTEGER ():

   DEFINE BUFFER RequestAction FOR RequestAction.

   FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.

   IF AVAILABLE RequestAction
   THEN RETURN RequestAction.RequestActionID + 1.

   RETURN 1.

END FUNCTION.

/* RequestAction */
FUNCTION fCreateRequestAction RETURNS LOGICAL
   ( icCLIType AS CHARACTER,
     iiReqType AS INTEGER,
     iiAction AS INT,
     icActionKey AS CHAR,
     iiPayType AS INT):

   FIND FIRST RequestAction EXCLUSIVE-LOCK WHERE
      RequestAction.Brand  = "1" AND
      RequestAction.CLIType = icCLIType AND
      RequestAction.ReqType = iiReqType AND
      RequestAction.ValidTo  = DATE(12,31,2049) AND
      RequestAction.ActionType = "SMS" AND
      RequestAction.ActionKey = icActionKey AND
      RequestAction.Action = iiAction AND
      RequestAction.PayType = iiPaytype
   NO-ERROR.
   
   IF NOT AVAILABLE RequestAction
   THEN DO:
      CREATE RequestAction.
      ASSIGN
         RequestAction.RequestActionID = fGetNextRequestActionID()
         RequestAction.Brand  = "1"
         RequestAction.CLIType    = icCLIType
         RequestAction.ReqType = iiReqType
         RequestAction.ValidFrom = DATE(6,5,2017)
         RequestAction.ValidTo  = DATE(12,31,2049)
         RequestAction.ActionType = "SMS"
         RequestAction.ActionKey = icActionKey
         RequestAction.Action = iiAction
         RequestAction.PayType = iiPaytype
         .
   END.
   
   ASSIGN
      giRequestActionID = RequestAction.RequestActionID
      .

END FUNCTION.

/* RequestActionRule */
FUNCTION fCreateRequestActionRule RETURNS LOGICAL
   ( iiRequestActionID AS INTEGER,
     icParamField AS CHARACTER,
     icParamValue AS CHARACTER ):

   FIND FIRST RequestActionRule EXCLUSIVE-LOCK WHERE
      RequestActionRule.RequestActionID  = iiRequestActionID AND
      RequestActionRule.ParamField = icParamField AND
      RequestActionRule.ToDate  = DATE(12,31,2049)
   NO-ERROR.

   IF NOT AVAILABLE RequestActionRule
   THEN CREATE RequestActionRule.

   ASSIGN
      RequestActionRule.RequestActionID = iiRequestActionID
      RequestActionRule.ParamField      = icParamField
      RequestActionRule.FromDate        = DATE(1,1,2018)
      RequestActionRule.ToDate          = DATE(12,31,2049)
      RequestActionRule.ParamValue      = icParamValue.
      .

END FUNCTION.
/* Case 0 */

/* This is already existing rule...
fCreateRequestAction("", 0, 13, "STC_DONE", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,2").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,|CONVERGENT").
*/

fCreateRequestAction("", 0, 13, "STC_Requested_Any_to_Cvg", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,0").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,|CONVERGENT").

/* Case 1 */
fCreateRequestAction("", 0, 13, "STC_DONE_28_to_29", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,2").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT28|CONT29").


fCreateRequestAction("", 0, 13, "STC_Requested_28_to_29", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,0").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT28|CONT29").


/* Case 2 */
fCreateRequestAction("", 0, 13, "STC_DONE_29_to_28", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,2").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT29|CONT28").

fCreateRequestAction("", 0, 13, "STC_Requested_29_to_28", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,0").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT29|CONT28").

/*
/* Case 3 */
fCreateRequestAction("", 0, 13, "STC_DONE_From_Convergent", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,2").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONVERGENT_MAINLINE|CONVERGENT_NO_MAINLINE,MOBILEONLY").

fCreateRequestAction("", 0, 13, "STC_Requested_From_Convergent", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,0").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONVERGENT_MAINLINE|CONVERGENT_NO_MAINLINE,MOBILEONLY").
*/

/* Case 3 */
fCreateRequestAction("", 0, 13, "STC_DONE_28_to_NonCvg", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,2").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT28|CONVERGENT_NO_MAINLINE,MOBILEONLY").

fCreateRequestAction("", 0, 13, "STC_Requested_28_to_NonCvg", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,0").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT28|CONVERGENT_NO_MAINLINE,MOBILEONLY").

/* Case 4 */
fCreateRequestAction("", 0, 13, "STC_DONE_29_to_NonCvg", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,2").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT29|CONVERGENT_NO_MAINLINE,MOBILEONLY").

fCreateRequestAction("", 0, 13, "STC_Requested_29_to_NonCvg", 1).
fCreateRequestActionRule(giRequestActionID, "ReqStatus", "+,0").
fCreateRequestActionRule(giRequestActionID, "#STCFROMTO", "+,CONT29|CONVERGENT_NO_MAINLINE,MOBILEONLY").
