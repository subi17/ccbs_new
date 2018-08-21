
/*------------------------------------------------------------------------
    File        : bundleupsells.i
    Purpose     : 

    Syntax      :

    Description : Functions Related to the Bundle Upsells retrieval/processing

    Author(s)   : Koundinya Maddali
    Created     : Mon May 07 16:47:04 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{Syst/tmsconst.i}

FUNCTION fGetDayCampaignUpsells RETURNS CHARACTER 
    (INPUT icDCEvent AS CHARACTER ):
    
    DEFINE VARIABLE lcBundleUpsells AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bfTMSRelation FOR TMSRelation.
    
    FOR EACH bfTMSRelation NO-LOCK WHERE 
        bfTMSRelation.TableName    =  {&DCTABLENAME}  AND 
        bfTMSRelation.KeyType      =  {&DCKEYTYPE}    AND 
        bfTMSRelation.ParentValue  =  icDCEvent       AND 
        bfTMSRelation.RelationType =  {&DCRELATIONTYPE}:
                 
        ASSIGN 
            lcBundleUpsells = lcBundleUpsells + "," + bfTMSRelation.ChildValue.
        
    END. 
          
    RETURN (TRIM(lcBundleUpsells,",")).       
        
END.        

FUNCTION fIsDayCampaignBundleUpsellExists RETURNS LOGICAL 
    (INPUT icDCEvent AS CHARACTER):
   
    /* TODO-PC : We need to confirm whether this UPSELL Hardcoding is required in future.*/   
        
    DEFINE VARIABLE lcNonMatchUpSells AS CHARACTER EXTENT 3 NO-UNDO.
    DEFINE VARIABLE lcBundleUpsell    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.
    
    ASSIGN 
        lcNonMatchUpSells[1] = "DATA200_UPSELL"
        lcNonMatchUpSells[2] = "SAN1GB_001,SAN5GB_002,DATA200_UPSELL"
        lcNonMatchUpSells[3] = "SAN1GB_001,SAN5GB_002"
        lcBundleUpsell       = fGetDayCampaignUpsells(icDCEvent) 
        .
    
    DO liCount = 1 TO EXTENT(lcNonMatchUpSells):
                
        IF lcBundleUpsell = lcNonMatchUpSells[liCount]
        THEN RETURN TRUE.
                
    END.    
    
    RETURN FALSE.  
        
END FUNCTION.  