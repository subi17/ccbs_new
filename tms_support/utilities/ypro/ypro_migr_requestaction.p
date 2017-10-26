{Syst/tmsconst.i}
/* RequestAction */
FUNCTION fcreateRequestAction RETURNS LOGICAL (INPUT iireqtype AS INT,
                                               INPUT icclitype AS CHAR,
                                               INPUT iiAction  AS INT,
                                               INPUT icActType AS CHAR,
                                               INPUT icKey     AS CHAR):
   DEF VAR liActionID AS INT NO-UNDO.
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

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONT15",2,"DayCampaign","VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",2,"DayCampaign","VOICE200").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_39_50",2,"DayCampaign","VOICE200").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_49_300",2,"DayCampaign","VOICE200").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",2,"DayCampaign","VOICE200").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_48_50",2,"DayCampaign","VOICE200").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_58_300",2,"DayCampaign","VOICE200").

fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",2,"DayCampaign","VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_39_50",2,"DayCampaign","VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_49_300",2,"DayCampaign","VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL48",2,"DayCampaign","VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_48_50",2,"DayCampaign","VOICE100").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_58_300",2,"DayCampaign","VOICE100").


fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_76_1000",2,"DayCampaign","VOICE200").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTDSL39",2,"DayCampaign","VOICE200B").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_39_50",2,"DayCampaign","VOICE200B").
fcreateRequestAction({&REQTYPE_PRO_MIGRATION}, "CONTFH_49_300",2,"DayCampaign","VOICE200B").
