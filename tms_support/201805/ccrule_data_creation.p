/*------------------------------------------------------------------------
    File        : ccrule_data_creation.p
    Purpose     : This is a Migration program which create the CCRULE records.

    Syntax      :

    Description : 

    Author(s)   : Koundinya Maddali
    Created     : Fri Apr 20 11:56:43 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/


MESSAGE "This Script will create the CCRULE records in the database." SKIP 
        "Do you want to continue?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

IF NOT lgChoice THEN RETURN.

FOR EACH BillItem NO-LOCK:
    
    CREATE CCRule.
    ASSIGN CCRule.CCruleId     =    STRING(NEXT-VALUE(CCRuleSeq))
           CCRule.Category     =    "*"
           CCRule.ValidFrom    =    TODAY
           CCRule.ValidTo      =    12/31/49
           CCRule.ReportingID  =    BillItem.SAPRid
           .
           
    BUFFER-COPY BillItem TO CCRule NO-ERROR.
    
END.

MESSAGE "Script Execution Completed." SKIP
        "CCRULE records got created."    
VIEW-AS ALERT-BOX.