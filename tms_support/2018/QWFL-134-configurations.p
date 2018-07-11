DEF VAR liActionID AS INT NO-UNDO. 

FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
IF AVAILABLE RequestAction THEN
  liActionID = RequestAction.RequestActionID + 1.
  ELSE liActionID = 1.


DO TRANS:

FOR EACH daycampaign NO-LOCK where
         daycampaign.dcevent begins "fterm":

   FIND FIRST requestaction NO-LOCK  where
              requestaction.brand = "1" and
              requestaction.reqtype = 0 and
              requestaction.actiontype = "DayCampaign" and
              requestaction.actionkey = DayCampaign.dcevent and
              requestaction.action = 7 and
              requestaction.validto >= today no-error.
   IF AVAIL requestaction then next.

   create requestaction.
   assign
      requestaction.Action       = 7
      requestaction.ActionKey    = DayCampaign.DCEvent
      requestaction.ActionType   = "DayCampaign"
      requestaction.Brand        = "1"
      requestaction.CLIType      = ""
      requestaction.PayType      = 1
      requestaction.ReqType      = 0
      requestaction.RequestActionID = liActionID
      requestaction.ValidFrom    = today
      requestaction.ValidTo      = 12/31/2049.

   create requestactionrule.
   assign
      requestactionrule.requestactionid = requestaction.requestactionid
      requestactionrule.fromdate = requestaction.validfrom
      requestactionrule.todate = requestaction.validto
      requestactionrule.paramfield = "ReqIParam5"
      requestactionrule.ParamValue = "+,3".

   disp requestactionrule.
   create requestactionrule.
   assign
      requestactionrule.requestactionid = requestaction.requestactionid
      requestactionrule.fromdate = requestaction.validfrom
      requestactionrule.todate = requestaction.validto
      requestactionrule.paramfield = "ReqCParam1"
      requestactionrule.ParamValue = "+,CONTDSL*,CONTFH*".

   create requestactionrule.
   assign
      requestactionrule.requestactionid = requestaction.requestactionid
      requestactionrule.fromdate = requestaction.validfrom
      requestactionrule.todate = requestaction.validto
      requestactionrule.paramfield = "ReqCParam2"
      requestactionrule.ParamValue = "+,CONTDSL*,CONTFH*".

   liActionID = liActionID + 1.

end.

FIND FIRST requestactionrule EXCLUSIVE-LOCK where
           requestactionrule.requestactionid = 519 and
           requestactionrule.paramfield = "reqiparam5" and   
           requestactionrule.paramvalue = "+,0" no-error.
IF AVAIL requestactionrule then requestactionrule.paramvalue = "+,0,3".


FIND FIRST tmscodes NO-LOCK where
     tmscodes.tablename = "requestaction" and
     tmscodes.fieldname = "action" and
     tmscodes.codevalue = "7" no-error.

IF NOT AVAIL tmscodes then do:
   create tmscodes.
   assign
      tmscodes.CodeGroup    = "Request"
      tmscodes.CodeValue    = "7"
      tmscodes.CodeName     = "Refresh"
      tmscodes.FieldName    = "Action"
      tmscodes.InUse        = 1
      tmscodes.TableName    = "RequestAction".
end.

END.

disp "done".
