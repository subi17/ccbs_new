
 DEFINE BUFFER b_FMItem FOR FMItem.
 DEFINE VARIABLE ldToDate       AS DATE    NO-UNDO.
 DEFINE VARIABLE ldFromDate     AS DATE    NO-UNDO.
 DEFINE VARIABLE liFirstMonthBr AS INTEGER NO-UNDO.
 DEFINE VARIABLE liBrokenRental AS INTEGER NO-UNDO.


CREATE TMSCodes.
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
     Daycampaign.BundleType = 0. 
     IF (DayCampaign.DCEvent  BEGINS  "CON" OR DayCampaign.DCEvent BEGINS "DUB") AND
        (DayCampaign.DCType EQ  "1" OR 
         DayCampaign.DCType EQ  "4" OR 
         DayCampaign.DCType EQ  "7"   ) THEN 
         Daycampaign.BundleType = 1.
 END.
 

 FOR EACH DayCampaign NO-LOCK:
     IF NOT (DayCampaign.DCEvent  BEGINS  "CON" OR DayCampaign.DCEvent BEGINS "DUB") THEN NEXT.
     IF NOT (DayCampaign.DCType EQ  "1" OR  
             DayCampaign.DCType EQ  "4" OR 
             DayCampaign.DCType EQ  "7" )  THEN NEXT.
     FOR EACH FeeModel WHERE FeeModel.FeeModel = DayCampaign.FeeModel NO-LOCK :
         IF NOT ( CAN-FIND(FIRST FMItem OF FeeModel WHERE FMItem.BrokenRental EQ 1 ) OR 
                  CAN-FIND(FIRST FMItem OF FeeModel WHERE FMItem.FirstMonthBR EQ 1 ) ) THEN NEXT.       
         FOR EACH FMItem OF FeeModel EXCLUSIVE-LOCK :
            IF FMItem.ToDate < TODAY THEN NEXT. /* Already closed */
            IF FMItem.BrokenRental EQ 1 OR FMItem.FirstMonthBR EQ 1 THEN DO: /* LMF or FMF = 1 */
                ASSIGN 
                    liFirstMonthBr = FMItem.FirstMonthBR   /* FirstMonthBr ( could be 2 ) */
                    liBrokenRental = FMItem.BrokenRental   /* Last month       */
                    ldToDate       = FMItem.ToDate         /* Required TO date */
                    ldFromDate     = TODAY.                /* New one starts from today */
                IF FMItem.FromDate > TODAY THEN ldFromDate     = FMItem.FromDate.   /* If future start date */
                IF liFirstMonthBr = 1      THEN liFirstMonthBr = 0.                  /*  Only change when it is 1 */
                IF liBrokenRental = 1      THEN liBrokenRental = 0.                  /*  Only change when it is 1 */
                ASSIGN FMItem.ToDate = TODAY - 1.   /* Close FMITem */
                CREATE b_FMItem. 
                BUFFER-COPY FMItem EXCEPT FMItem.BrokenRental FMItem.FirstMonthBR FMItem.FromDate FMItem.ToDate TO b_FMItem 
                    ASSIGN 
                        b_FMItem.FirstMonthBR  = liFirstMonthBr
                        b_FMItem.BrokenRental  = liBrokenRental
                        b_FMItem.FromDate      = ldFromDate
                        b_FMItem.ToDate        = ldToDate.
            END.
         END.
     END.
 END.     


 