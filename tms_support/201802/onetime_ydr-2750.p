



REATE TMSCodes.
ASSIGN
   TMSCodes.TableName = "DayCampaign"
   TMSCodes.FieldName = "BundleType"
   TMSCodes.CodeGroup = "MobSub"
   TMSCodes.CodeValue = "0"
   TMSCodes.CodeName  = "undefined"
   TMSCodes.InUse = 1
   .

  
CREATE TMSCodes.
ASSIGN
   TMSCodes.TableName = "DayCampaign"
   TMSCodes.FieldName = "BundleType"
   TMSCodes.CodeGroup = "MobSub"
   TMSCodes.CodeValue = "1"
   TMSCodes.CodeName  = "Tariff bundle"
   TMSCodes.InUse = 1
   .

  
CREATE TMSCodes.
ASSIGN
   TMSCodes.TableName = "DayCampaign"
   TMSCodes.FieldName = "BundleType"
   TMSCodes.CodeGroup = "MobSub"
   TMSCodes.CodeValue = "2"
   TMSCodes.CodeName  = "Additional Bundle(Voice or Data)"
   TMSCodes.InUse = 1
   .

 
 FOR EACH Daycampaign EXCLUSIVE-LOCK:
     
     Daycampign.BundleType = 0.
     
     IF (DayCampaign.DCEvent  BEGINS  "CON" OR DayCampaign.DCEvent BEGINS "DUB") AND
        (DayCampaign.DCType EQ  "1" OR 
         DayCampaign.DCType EQ  "4" OR 
         DayCampaign.DCType EQ  "7"   ) THEN 
         Daycampign.BundleType = 1.
     
 END.
 
