
/*------------------------------------------------------------------------
    File        : upsell_tmsrelation_create.p
    Purpose     : 

    Syntax      :

    Description : This will create the 

    Author(s)   : 
    Created     : Sat May 05 14:50:07 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE liUpsellCount AS INTEGER  NO-UNDO.

Syst.TMSRelation:mAddKeyType("DayCampaign","UPSELL").

FOR EACH DayCampaign NO-LOCK WHERE 
    DayCampaign.Brand          =   Syst.Var:gcBrand AND 
    DayCampaign.DCEvent        >   ""               AND 
    DayCampaign.BundleUpsell   >   ""               :    
    
    
    DO liUpsellCount = 1 TO NUM-ENTRIES(DayCampaign.BundleUpsell) :
    
        Syst.TMSRelation:mAddRelation("DayCampaign",
                                      "UPSELL",
                                      DayCampaign.DCEvent,
                                      ENTRY(liUpsellCount,DayCampaign.BundleUpsell),
                                      "Compatibility",
                                      YES).
    
    END. /*  DO liUpsellCount */        
END. /* FOR EACH DayCampaign */

