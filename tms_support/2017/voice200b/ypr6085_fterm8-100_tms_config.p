DEF BUFFER bNewDayCampaign FOR daycampaign.
DEF TEMP-TABLE ttDC LIKE daycampaign.

/*New daycampaign will be base on this*/
find first daycampaign NO-LOCK WHERE
         daycampaign.dcevent eq "FTERM12-100".

if not avail daycampaign then message "fterm not found" VIEW-AS ALERT-BOX.

disp daycampaign.dcevent.
disp daycampaign.dcname.
disp daycampaign.durmonths.

DO TRANS:
   create ttDC.
   create bNewDayCampaign.
   buffer-copy daycampaign to ttDC.

   /*Change the fields that are different.*/
   ttDC.dcevent = "FTERM8-100".
   ttDC.dcname = "FTERM8 periodical contract".
   ttDC.durmonths = 8.
   ttDC.validfrom = today.

   /*copy the new one to daycampaign table*/
   buffer-copy ttDC to bNewDayCampaign.

   /*proudly show the results*/
   message "created " + bNewDayCampaign.dcevent VIEW-AS ALERT-BOX.
   disp bNewDaycampaign.dcevent.
   disp bNewDaycampaign.dcname.
   disp bNewDaycampaign.durmonths.

END.

