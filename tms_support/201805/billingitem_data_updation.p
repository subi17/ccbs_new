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

ASSIGN lcBkpFile = "/apps/yoigo/tms_support/201805/billing_item_bkp.d".

DEFINE STREAM bkp.

DEFINE BUFFER bfBillItem FOR BillItem.

MESSAGE "This script will update the Item Type for the BillItem." SKIP 
        "Do you want to continue?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

IF NOT lgChoice THEN RETURN.

OUTPUT TO VALUE(lcBkpFile).

EXPORT DELIMITER "," "Brand" "BIGroup" "BillCode" "ItemType" "New ItemType".

FOR EACH BillItem NO-LOCK :
     
    FIND FIRST BItemGroup NO-LOCK WHERE 
        BItemGroup.Brand    =  BillItem.Brand AND 
        BItemGroup.BIGroup  =  BillItem.BIGroup NO-ERROR.
   
    IF AVAILABLE BItemGroup 
    THEN DO:
        
        FIND FIRST bfBillItem EXCLUSIVE-LOCK WHERE 
            ROWID(bfBillItem)  = ROWID(BillItem) NO-WAIT NO-ERROR.
             
        IF NOT AVAILABLE bfBillItem OR LOCKED(bfBillItem) THEN NEXT.
        
        EXPORT DELIMITER "," BillItem.Brand BillItem.BIGroup BillItem.BillCode BillItem.ItemType BItemGroup.GroupType.
        
        ASSIGN 
            bfBillItem.ItemType = BItemGroup.GroupType NO-ERROR.
       
    END. 
   
END.

OUTPUT CLOSE.

DISPLAY lcBkpFile LABEL "Backup file" FORMAT "X(70)" WITH 1 COL.

MESSAGE "Script Execution Completed." SKIP "BillItem records successfully updated."
VIEW-AS ALERT-BOX.
