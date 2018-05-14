
/*------------------------------------------------------------------------
    File        : upsell_tmsrelation_create.p
    Purpose     : 

    Syntax      :

    Description : This will create the 

    Author(s)   : 
    Created     : Sat May 05 14:50:07 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

{Syst/tmsconst.i}

DEFINE VARIABLE liUpsellCount AS INTEGER  NO-UNDO.

Syst.TMSRelation:mAddKeyType({&DCTABLENAME},{&DCKEYTYPE}).

FOR EACH DayCampaign NO-LOCK WHERE 
    DayCampaign.Brand          =   Syst.Var:gcBrand AND 
    DayCampaign.DCEvent        >   ""               AND 
    DayCampaign.BundleUpsell   >   ""               :    
    
    
    DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell) :
    
        Syst.TMSRelation:mAddRelation({&DCTABLENAME},
                                      {&DCKEYTYPE},
                                      DayCampaign.DCEvent,
                                      ENTRY(liUpsellCount,DayCampaign.BundleUpsell),
                                      {&DCRELATIONTYPE},
                                      YES).
    
    END. /*  DO liUpsellCount */        
END. /* FOR EACH DayCampaign */

MESSAGE "Script Completed." SKIP 
        "TMS Relation records got created in TMS."
VIEW-AS ALERT-BOX.