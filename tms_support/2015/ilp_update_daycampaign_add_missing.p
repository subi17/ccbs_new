FOR EACH DayCampaign EXCLUSIVE-LOCK WHERE
         (DayCampaign.StatusCode EQ 1 OR /* Active */
         DayCampaign.StatusCode EQ 2 ) /* Retired */
         AND
         DayCampaign.DCEvent NE "BONO_VOIP" AND
         DayCampaign.DCType EQ "4" AND
         NOT DayCampaign.DCEvent BEGINS "Tarj" AND
         DayCampaign.BundleUpsell EQ ""
        :
  DayCampaign.BundleUpsell = "DATA200_UPSELL".
END.
