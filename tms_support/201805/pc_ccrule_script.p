
/*------------------------------------------------------------------------
    File        : pc_ccrule_script.p
    Purpose     : 

    Syntax      :

    Description : This will create the CCRule menu and load the data into CCRULE records and updates billingitem table

    Author(s)   : Koundinya Maddali
    Created     : Wed May 09 18:53:58 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

DEFINE STREAM err.
DEFINE STREAM bkp.

RUN ipCCRuleMenuCreate.
RUN ipCCRuleDataCreation.
RUN ipUpdateBillingItem.

PROCEDURE ipCCRuleMenuCreate:
    
    IF NOT CAN-FIND(FIRST MenuText WHERE MenuText.MenuNum = 9857) 
    THEN DO:
        
        CREATE MenuText.
        ASSIGN 
            MenuText.MenuNum  = 9857
            MenuText.MenuText = "ACCOUNT.RULES"
            .
    END.  
            
END PROCEDURE.

PROCEDURE ipCCRuleDataCreation:    
    
    MESSAGE "This Script will create the Account Rules records in the database." SKIP 
            "Do you want to continue?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

    IF NOT lgChoice THEN RETURN.

    FOR EACH BillItem NO-LOCK:
    
        FIND FIRST CostCentre WHERE 
                   CostCentre.Brand      =   BillItem.Brand AND 
                   CostCentre.CostCentre =   BillItem.CostCentre NO-LOCK NO-ERROR.
    
        IF NOT AVAILABLE CostCentre 
        THEN DO:
        
            CREATE CostCentre.
            ASSIGN 
                CostCentre.Brand      = BillItem.Brand
                CostCentre.CostCentre = BillItem.CostCentre.
        END.  
        
        IF NOT CAN-FIND(FIRST CCRule WHERE CCRule.Brand     = BillItem.Brand
                                       AND CCRule.Category  = "*"
                                       AND CCRule.BillCode  = BillItem.BillCode
                                       AND CCRule.CLIType   = ""
                                       AND CCRule.ValidTo  >= TODAY) 
        THEN DO:
            
            CREATE CCRule.
            ASSIGN 
                CCRule.Brand       = BillItem.Brand
                CCRule.CCruleId    = NEXT-VALUE(CCRuleSeq)
                CCRule.Category    = "*"
                CCRule.BillCode    = BillItem.BillCode
                CCRule.ValidFrom   = DATE(01,01,2018)
                CCRule.ValidTo     = DATE(12,31,2049)
                CCRule.ReportingID = BillItem.SAPRid
                CCRule.CostCentre  = BillItem.CostCentre
                CCRule.AccNum      = BillItem.AccNum
                CCRule.EUAccNum    = BillItem.EUAccNum
                CCRule.EUConAccNum = BillItem.EUConAccNum
                CCRule.FSAccNum    = BillItem.FSAccNum
                .
        END.        
    END.

    MESSAGE "Account Rules records got created."    
        VIEW-AS ALERT-BOX.
    
END PROCEDURE.

PROCEDURE ipUpdateBillingItem:
    
    
    DEFINE VARIABLE lcBkpFile AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE lcErrFile AS CHARACTER NO-UNDO.

    ASSIGN 
        lcBkpFile = "/apps/yoigo/tms_support/201805/billingitem_bkp.d"
        lcErrFile = "/apps/yoigo/tms_support/201805/billingitem_err.txt".

    MESSAGE "This script will update the Item Type for the BillItem." SKIP 
            "Do you want to continue?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

    IF NOT lgChoice THEN RETURN.

    OUTPUT STREAM bkp TO VALUE(lcBkpFile).
    OUTPUT STREAM err TO VALUE(lcErrFile).

    EXPORT STREAM bkp DELIMITER "," "Brand" "BIGroup" "BillCode" "ItemType" "New ItemType".

    FOR EACH BillItem NO-LOCK :
     
        FIND FIRST BItemGroup NO-LOCK WHERE 
                   BItemGroup.Brand    =  BillItem.Brand AND 
                   BItemGroup.BIGroup  =  BillItem.BIGroup NO-ERROR.
   
        IF AVAILABLE BItemGroup 
        THEN DO:
        
            FIND CURRENT BillItem EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        
            IF LOCKED(BillItem) THEN 
            DO:
                EXPORT STREAM err "BillItem record with the BillCode:" BillItem.Billcode " is locked. Update failed.".  
                NEXT.
            END. 
            ELSE IF NOT AVAILABLE(BillItem) THEN 
                DO:
                    EXPORT STREAM err "BillItem record with the BillCode:" BillItem.Billcode " is not available. Update failed.".  
                    NEXT.
                END. 
               
            EXPORT STREAM bkp DELIMITER "," BillItem.Brand BillItem.BIGroup BillItem.BillCode BillItem.ItemType BItemGroup.GroupType.
        
            ASSIGN 
                BillItem.ItemType = BItemGroup.GroupType NO-ERROR.
       
        END.    
    END.

    OUTPUT STREAM bkp CLOSE.
    OUTPUT STREAM err CLOSE.

    DISPLAY lcBkpFile LABEL "Backup file" FORMAT "X(60)"
            lcErrFile LABEL "Error File"  FORMAT "X(60)" WITH 1 COL.

    MESSAGE "Script Execution Completed." VIEW-AS ALERT-BOX.
    
END PROCEDURE.