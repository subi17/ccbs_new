/*------------------------------------------------------------------------
    File        : daycampaign_bundletarget_update.p
    Purpose     : 

    Syntax      :

    Description : This Program will merge BundleType field to BundleTarget in DayCampaign table

    Author(s)   : Koundinya Maddali
    Created     : Sat May 05 13:12:22 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/*
/*DayCampaign.BundleTarget*/
&GLOBAL-DEFINE DC_BUNDLE_TARGET_MOBILE 0
&GLOBAL-DEFINE DC_BUNDLE_TARGET_FIXED  1
&GLOBAL-DEFINE TELEVISION_BUNDLE       2
&GLOBAL-DEFINE DC_BUNDLE_TARGET_SVA    3
&GLOBAL-DEFINE DB_BUNDLE_TARGET_TARIFF 4

/* DayCampaign.BundleType */
&GLOBAL-DEFINE DC_BUNDLE_TYPE_UNDEFINED     0
&GLOBAL-DEFINE DC_BUNDLE_TYPE_TARIFF        1
&GLOBAL-DEFINE DC_BUNDLE_TYPE_ADDITIONAL    2

*/

MESSAGE "This script will update the BundleTarget for the Periodical Contracts." SKIP 
        "Are you sure you want to proceed?"
VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

IF NOT lgChoice THEN RETURN.

DEFINE VARIABLE lcRefBundleTarget AS INTEGER    INIT 3                             NO-UNDO.
DEFINE VARIABLE lcBkpFile         AS CHARACTER  LABEL "Backup File" FORMAT "X(60)" NO-UNDO.

DEFINE BUFFER bfDayCampaign FOR DayCampaign.

ASSIGN lcBkpFile = "/apps/yoigo/tms_support/201805/" + "dc_bundletarget_" + STRING(MTIME) + ".txt".

OUTPUT TO VALUE(lcBkpFile).

EXPORT DELIMITER "," "DCEvent" "BundleType" "OldBundleTarget" "NewBundleTarget".

FOR EACH DayCampaign NO-LOCK:
    
    IF DayCampaign.BundleType    =   0 OR 
       DayCampaign.BundleTarget <>   0 THEN NEXT.
    
    FIND FIRST bfDayCampaign EXCLUSIVE-LOCK WHERE 
         ROWID(bfDayCampaign) = ROWID(DayCampaign) NO-WAIT NO-ERROR.
         
    IF LOCKED(bfDayCampaign)       OR 
       NOT AVAILABLE bfDayCampaign 
    THEN NEXT. 
    
    EXPORT DELIMITER "," DayCampaign.DCEvent DayCampaign.BundleType DayCampaign.BundleTarget (DayCampaign.BundleType + lcRefBundleTarget).
    
    ASSIGN bfDayCampaign.BundleTarget = (DayCampaign.BundleType + lcRefBundleTarget) NO-ERROR.
    
END.

OUTPUT CLOSE.

DISPLAY lcBkpFile.

MESSAGE "Script Completed."
VIEW-AS ALERT-BOX.















