/*------------------------------------------------------------------------
    File        : ccrule_data_creation.p
    Purpose     : This is a Migration program which create the CCRULE records.

    Syntax      :

    Description : 

    Author(s)   : Koundinya Maddali
    Created     : Fri Apr 20 11:56:43 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/


MESSAGE "This Script will create the Accunt Rules records in the database." SKIP 
        "Do you want to continue?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lgChoice AS LOGICAL.

IF NOT lgChoice THEN RETURN.

FOR EACH BillItem NO-LOCK:
    
    FIND FIRST CostCentre WHERE CostCentre.Brand      =   BillItem.Brand 
                            AND CostCentre.CostCentre =   BillItem.CostCentre NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE CostCentre 
    THEN DO:
        CREATE CostCentre.
        ASSIGN 
            CostCentre.Brand      = BillItem.Brand
            CostCentre.CostCentre = BillItem.CostCentre.
    END.  
    
    CREATE CCRule.
    ASSIGN CCRule.Brand        =    BillItem.Brand
           CCRule.CCruleId     =    NEXT-VALUE(CCRuleSeq)
           CCRule.Category     =    "*"
           CCRule.BillCode     =    BillItem.BillCode
           CCRule.ValidFrom    =    TODAY
           CCRule.ValidTo      =    12/31/49
           CCRule.ReportingID  =    BillItem.SAPRid
           CCRule.CostCentre   =    BillItem.CostCentre
           CCRule.AccNum       =    BillItem.AccNum
           CCRule.EUAccNum     =    BillItem.EUAccNum
           CCRule.EUConAccNum  =    BillItem.EUConAccNum
           CCRule.FSAccNum     =    BillItem.FSAccNum
           .
           
END.

MESSAGE "Script Execution Completed." SKIP
        "Account Rules records got created."    
VIEW-AS ALERT-BOX.