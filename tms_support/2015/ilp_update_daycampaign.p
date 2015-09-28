/* Intelligent landing page project. YPR-2173
Add ",DATA200_UPSELL" or ",DSS200_UPSELL" to after existing 
upsells that need adding.*/
/*Example:
   CONTD2: DATA6_UPSELL -> DATA6_UPSELL,DATA200_UPSELL

   */
DEF VAR lcAddUpsell AS CHAR NO-UNDO.
DEF VAR lcAddWith AS CHAR NO-UNDO.

lcAddUpsell = "DATA200_UPSELL".
lcAddWith   = "DATA6_UPSELL".

FOR EACH DayCampaign EXCLUSIVE-LOCK WHERE 
         (DayCampaign.StatusCode EQ 1 OR /* Active */
         DayCampaign.StatusCode EQ 2 ) /* Retired */        
         AND
         DayCampaign.DCEvent NE "BONO_VOIP" AND
         DayCampaign.DCType EQ "4" AND
         DayCampaign.BundleUpsell EQ lcAddWith AND
         INDEX(DayCampaign.BundleUpsell, lcAddUpsell,1) EQ 0 /*no double*/
         /*AND DayCampaign.DCEvent EQ "CONTD3" */
         :
   /*Assign new upsell with the existing one*/
   /*MESSAGE "Add to " + DayCampaign.BundleUpsell VIEW-AS ALERT-BOX.*/
   DayCampaign.BundleUpsell = DayCampaign.BundleUpsell +
                              "," + lcAddUpsell. 
END.

lcAddUpsell = "DSS200_UPSELL".
lcAddWith   = "DSS_UPSELL".

FOR EACH DayCampaign EXCLUSIVE-LOCK WHERE 
         (DayCampaign.StatusCode EQ 1 OR /* Active */
         DayCampaign.StatusCode EQ 2 ) /* Retired */        
         AND
         DayCampaign.DCEvent NE "BONO_VOIP" AND
         DayCampaign.DCType EQ "4" AND
         DayCampaign.BundleUpsell EQ lcAddWith AND
         INDEX(DayCampaign.BundleUpsell, lcAddUpsell,1) EQ 0
         :
   /*MESSAGE "Add to " + DayCampaign.BundleUpsell VIEW-AS ALERT-BOX.*/
   DayCampaign.BundleUpsell = DayCampaign.BundleUpsell +
                              "," + lcAddUpsell. 
END.

lcAddUpsell = "DSS200_UPSELL".
lcAddWith   = "DSS2_UPSELL".

FOR EACH DayCampaign EXCLUSIVE-LOCK WHERE 
         (DayCampaign.StatusCode EQ 1 OR /* Active */
         DayCampaign.StatusCode EQ 2 ) /* Retired */        
         AND
         DayCampaign.DCEvent NE "BONO_VOIP" AND
         DayCampaign.DCType EQ "4" AND
         DayCampaign.BundleUpsell EQ lcAddWith AND
         INDEX(DayCampaign.BundleUpsell, lcAddUpsell,1) EQ 0 /*no double*/
         :
   /*MESSAGE "Add to " + DayCampaign.BundleUpsell VIEW-AS ALERT-BOX.*/
   DayCampaign.BundleUpsell = DayCampaign.BundleUpsell +
                              "," + lcAddUpsell. 
END.


