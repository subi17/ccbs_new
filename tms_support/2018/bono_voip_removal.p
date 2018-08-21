FOR EACH Daycampaign WHERE
         Daycampaign.brand EQ "1" AND
         Daycampaign.dcevent EQ "BONO_VOIP" AND
         Daycampaign.validto > TODAY:
   DISP DAYCampaign.
   /*DayCampaign.validto = TODAY - 1.*/
END.

/* Make these modifications by hand in CUI for storing eventlog */
FOR EACH RequestAction WHERE
         RequestAction.actionkey EQ "BONO_VOIP" AND
         RequestAction.ValidTo > TODAY:

   DISP RequestAction.      
   /* Make these reqiuestaction modifications by hand in CUI 
      for storing eventlog */
   /*RequestAction.validTo = TODAY - 1.*/
END.

FOR EACH RequestAction WHERE
         RequestAction.actionkey EQ "VOIPVIDEO" AND
         RequestAction.ValidTo > TODAY:

   DISP RequestAction.
   /* Make these requestaction modifications by hand in CUI 
      for storing eventlog */
   /*RequestAction.validTo = TODAY - 1.*/
END.
