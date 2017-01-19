DEF VAR liOFID AS INT NO-UNDO.
liOFID = 60.

FOR EACH orderfunction NO-LOCK:

   DISP orderfunction.
END.

FOR EACH ofitem NO-LOCK :
   DISP ofitem.
END.

MESSAGE "Adding starts" VIEW-AS ALERT-BOX.

FIND FIRST ofitem where
           ofitem.ofid EQ liOFID NO-ERROR.
IF NOT AVAIL ofitem THEN DO:           
   message "create ofitem" VIEW-AS ALERT-BOX.
   CREATE ofitem.
   ASSIGN ofitem.ofid = liOFID.
          ofitem.statuscode = "*".
END.
ELSE message "ofitem already exists" VIEW-AS ALERT-BOX.

FIND FIRST orderfunction where 
           orderfunction.ofid EQ liOFID NO-ERROR.

IF NOT AVAIL orderfunction THEN DO:           
    message "create orderfunction" VIEW-AS ALERT-BOX.
   CREATE OrderFunction.
   ASSIGN OrderFunction.ofID = liOFID
          OrderFunction.OFModule = "convview"
          OrderFunction.OfName = "Show Convergent data".
END.   
ELSE  message "ofunc already exists" VIEW-AS ALERT-BOX.
