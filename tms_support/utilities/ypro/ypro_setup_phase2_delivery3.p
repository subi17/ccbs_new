{Syst/tmsconst.i}
/* RequestAction */
DEF VAR liActionID AS INT NO-UNDO.

FUNCTION fcreateRequestAction RETURNS LOGICAL (INPUT iireqtype AS INT,
                                               INPUT icclitype AS CHAR,
                                               INPUT iiAction  AS INT,
                                               INPUT icActType AS CHAR,
                                               INPUT icKey     AS CHAR):
   FIND FIRST RequestAction WHERE
              RequestAction.brand      EQ "1" AND
              RequestAction.clitype    EQ icclitype AND
              RequestAction.reqtype    EQ iireqtype AND
              RequestAction.validto    GE TODAY AND
              RequestAction.action     EQ iiAction AND
              RequestAction.actionKey  EQ icKey NO-ERROR.
   IF NOT AVAIL Requestaction THEN DO:
      FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.

      CREATE Requestaction.
      ASSIGN
         RequestAction.brand = "1"
         RequestAction.RequestActionID = liActionID
         RequestAction.reqtype         = iireqtype
         RequestAction.validfrom       = TODAY
         RequestAction.validto         = 12/31/49
         RequestAction.action          = iiAction
         RequestAction.actiontype      = icActType
         RequestAction.clitype         = icclitype
         RequestAction.actionkey       = icKey.

   END.
END FUNCTION.

/* RequestActionRule */
FUNCTION fCreateRequestActionRule RETURNS LOGICAL
   (iiRequestActionID AS INT,
    icParamField      AS CHAR,
    icParamValue      AS CHAR):

   FIND FIRST RequestActionRule EXCLUSIVE-LOCK WHERE
              RequestActionRule.RequestActionID = iiRequestActionID AND
              RequestActionRule.ParamField      = icParamField AND
              RequestActionRule.ToDate          = DATE(12,31,2049) NO-ERROR.

   IF NOT AVAILABLE RequestActionRule THEN
      CREATE RequestActionRule.

   ASSIGN
      RequestActionRule.RequestActionID = iiRequestActionID
      RequestActionRule.ParamField      = icParamField
      RequestActionRule.FromDate        = TODAY
      RequestActionRule.ToDate          = DATE(12,31,2049)
      RequestActionRule.ParamValue      = icParamValue.

END FUNCTION.


fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONT15",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL39",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH39_50",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH49_300",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL48",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH48_50",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH58_300",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL39",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH39_50",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH49_300",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL48",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH48_50",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH58_300",2,"DayCampaign","VOICE100").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH76_1000",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTDSL39",2,"DayCampaign","VOICE200B").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH39_50",2,"DayCampaign","VOICE200B").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH49_300",2,"DayCampaign","VOICE200B").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").
fcreateRequestAction({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}, "CONTFH69_1000",2,"DayCampaign","VOICE200").
fCreateRequestActionRule(liActionID, "ReqIParam4", "1").

