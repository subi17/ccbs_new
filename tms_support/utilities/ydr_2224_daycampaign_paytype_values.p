DEF VAR lcPrepaid AS CHAR NO-UNDO. 
DEF VAR lcBoth    AS CHAR NO-UNDO. 


assign 
   lcPrepaid = "PMDUB,PMDUB_UPSELL,TARJ7,TARJ7_UPSELL,TARJ9,TARJ_UPSELL,PRERENOVE,PRETERM"
   lcBoth    = "HSPA_ROAM_EU".

FOR EACH DayCampaign EXCLUSIVE-LOCK:

   IF LOOKUP(DayCampaign.DCEvent,lcPrepaid) > 0 THEN 
      DayCampaign.PayType = 2.
   ELSE IF LOOKUP(DayCampaign.DCEvent,lcBoth) > 0 THEN
      DayCampaign.PayType = 0.
   ELSE DayCampaign.PayType = 1.   

END.
