/*------------------------------------------------------------------------
    File        : BillItem_data_updation.p
    Purpose     : This program assigns the billitem.itemtype with bitemgroup.grouptype

    Syntax      :

    Description : 

    Author(s)   : Koundinya Maddali
    Created     : Fri Apr 20 11:57:28 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */


DEFINE VARIABLE lcBkpFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcErrFile AS CHARACTER NO-UNDO.

DEFINE STREAM err.
DEFINE STREAM bkp.

ASSIGN lcBkpFile = "/apps/yoigo/tms_support/201805/billingitem_bkp.d"
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

MESSAGE "Script Execution Completed."       
VIEW-AS ALERT-BOX.
