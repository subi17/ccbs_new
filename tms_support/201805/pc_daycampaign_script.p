
/*------------------------------------------------------------------------
    File        : pc_daycampaign_script.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Koundinya Maddali 
    Created     : Tue May 15 15:08:10 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

{Syst/tmsconst.i}

DEFINE STREAM bkp.
DEFINE STREAM err.

RUN ipUpdateBundleTarget.
RUN ipCreateBundleUpsellValues.
RUN ipModifyMenuTree.
RUN ipCreateUpsellMenu.

PROCEDURE ipUpdateBundleTarget:
    
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

    DEFINE VARIABLE lcRefBundleTarget AS INTEGER   INIT 3 NO-UNDO.
    DEFINE VARIABLE lcBkpFile         AS CHARACTER LABEL "Backup File" FORMAT "X(55)" NO-UNDO.
    DEFINE VARIABLE lcErrFile         AS CHARACTER LABEL "Error File"  FORMAT "X(55)" NO-UNDO.
   
    ASSIGN 
        lcBkpFile = "/apps/yoigo/tms_support/201805/" + "dc_bundletarget_bkp_" + STRING(MTIME) + ".txt"
        lcErrFile = "/apps/yoigo/tms_support/201805/" + "dc_bundletarget_err_" + STRING(MTIME) + ".txt" .

    OUTPUT STREAM bkp TO VALUE(lcBkpFile).
    OUTPUT STREAM err TO VALUE(lcErrFile).

    EXPORT STREAM bkp DELIMITER "," "DCEvent" "BundleType" "OldBundleTarget" "NewBundleTarget".

    FOR EACH DayCampaign NO-LOCK:
    
        IF DayCampaign.BundleType    =   0 OR 
            DayCampaign.BundleTarget <>   0 THEN NEXT.
    
        FIND CURRENT DayCampaign EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         
        IF LOCKED(DayCampaign)  
        THEN DO:
            EXPORT STREAM err "Daycampaign record with Event:" Daycampaign.DCEvent " has been locked. Update failed.".
            NEXT.
        END. 
        ELSE IF NOT AVAILABLE DayCampaign 
        THEN DO:
            EXPORT STREAM err "Daycampaign record with Event:" Daycampaign.DCEvent " is not available. Update failed.".
            NEXT.
        END.
    
        EXPORT STREAM bkp DELIMITER "," DayCampaign.DCEvent DayCampaign.BundleType DayCampaign.BundleTarget (DayCampaign.BundleType + lcRefBundleTarget).
    
        ASSIGN 
            DayCampaign.BundleTarget = (DayCampaign.BundleType + lcRefBundleTarget) NO-ERROR.
    
    END.

    OUTPUT STREAM err CLOSE.
    OUTPUT STREAM bkp CLOSE.

    DISPLAY lcBkpFile lcErrFile WITH 1 COL.    
    
END PROCEDURE.

PROCEDURE ipCreateBundleUpsellValues:
    
    DEFINE VARIABLE liUpsellCount AS INTEGER NO-UNDO.
    
    MESSAGE "This script will create the TMSRelation records for Periodical Contracts." SKIP 
            "Do you want to continue?"
    VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.
    
    IF NOT lgChoice THEN RETURN.

    IF NOT CAN-FIND(FIRST TMSRelation WHERE TMSRelation.TableName   = "TMSRelation"
                                        AND TMSRelation.KeyType     = "KeyType"
                                        AND TMSRelation.ChildValue  = {&DCKEYTYPE}
                                        AND TMSRelation.ParentValue = {&DCTABLENAME}
                                        AND (TMSRelation.ToTime     >= NOW OR 
                                             TMSRelation.ToTime     =  ? ) USE-INDEX ChildValue )  
    THEN Syst.TMSRelation:mAddKeyType({&DCTABLENAME},{&DCKEYTYPE}).

    FOR EACH DayCampaign NO-LOCK WHERE 
        DayCampaign.Brand          =   Syst.Var:gcBrand AND 
        DayCampaign.DCEvent        >   ""               AND 
        DayCampaign.BundleUpsell   >   ""               :    
    
    
        DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell) :
    
            IF NOT CAN-FIND(FIRST TMSRelation WHERE TMSRelation.TableName = {&DCTABLENAME}
                                                AND TMSRelation.KeyType   = {&DCKEYTYPE}
                                                AND TMSRelation.ChildValue  = ENTRY(liUpsellCount,DayCampaign.BundleUpsell)
                                                AND TMSRelation.ParentValue = DayCampaign.DCEvent                                                
                                                AND (TMSRelation.ToTime     >= NOW OR 
                                                     TMSRelation.ToTime     =  ? ) USE-INDEX ChildValue )  
            THEN Syst.TMSRelation:mAddRelation({&DCTABLENAME},
                                               {&DCKEYTYPE},
                                               DayCampaign.DCEvent,
                                               ENTRY(liUpsellCount,DayCampaign.BundleUpsell),
                                               {&DCRELATIONTYPE},
                                               YES).
    
        END. /*  DO liUpsellCount */        
    END. /* FOR EACH DayCampaign */

    MESSAGE "TMS Relation records got created in TMS."
        VIEW-AS ALERT-BOX.   
    
END PROCEDURE.

PROCEDURE ipModifyMenuTree:
    
    MESSAGE "This script will change the Menu Program for TMSRelation." SKIP 
            "Do you want to continue?"
        VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.       
          
    IF NOT lgChoice THEN RETURN.   
    
    OUTPUT STREAM bkp TO VALUE("/apps/yoigo/tms_support/201805/menutree_bkp.d").
    OUTPUT STREAM err TO VALUE("/apps/yoigo/tms_support/201805/menutree_err.txt").
    
    FIND FIRST MenuTree EXCLUSIVE-LOCK 
         WHERE MenuTree.MenuNum  = 9856 NO-WAIT NO-ERROR.
         
    IF LOCKED(MenuTree)
    THEN DO:                
        EXPORT STREAM err "ERROR:MenuTree record is locked by other user. Please run the script again.".
        RETURN.        
    END.
    ELSE IF NOT AVAILABLE MenuTree 
    THEN DO:        
            EXPORT STREAM err "ERROR:MenuTree reccord is not available.".        
            RETURN.        
    END.
    
    IF TRIM(MenuTree.Module) <> "Mc/tmsrelation.p"
    THEN DO:        
        MESSAGE "ERROR:Retrieved record is not MenuTree for TMS Relation." SKIP 
            "Please modify the script and try to run again."
            VIEW-AS ALERT-BOX.
        RETURN.        
    END.
    
    EXPORT STREAM bkp MenuTree.
    
    ASSIGN 
        MenuTree.Module = "Mc/tmsrelation_run.p".
    
    OUTPUT STREAM bkp CLOSE.
    OUTPUT STREAM err CLOSE.
    
    MESSAGE "Menu Program got updated." 
        VIEW-AS ALERT-BOX.
    
END PROCEDURE.


PROCEDURE ipCreateUpsellMenu:
    
    MESSAGE "This script will Create the New Menu for Periodical Contract UPSELL values." SKIP 
            "Do you want to continue?"
        VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.       
          
    IF NOT lgChoice THEN RETURN.   

    IF CAN-FIND(FIRST MenuText WHERE MenuText.MenuNum = 9858) 
    THEN DO:
        
        MESSAGE "MenuText already exists with 9858." SKIP 
            "Please change the script and related programs."
            VIEW-AS ALERT-BOX.
        RETURN.
        
    END. 
    
    CREATE MenuText.
    ASSIGN 
        MenuText.MenuNum  = 9858
        MenuText.MenuText = "ALLOWED UPSELL'S".
           
    MESSAGE "New menu for UPSELL got created." SKIP 
            "Script Completed." VIEW-AS ALERT-BOX.
            
END PROCEDURE.
