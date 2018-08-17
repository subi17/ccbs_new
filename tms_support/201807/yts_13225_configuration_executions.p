DEF VAR lcCLITypes      AS CHAR NO-UNDO.
DEF VAR lc3GUpsellList  AS CHAR NO-UNDO.
DEF VAR lc5GUpsellList  AS CHAR NO-UNDO. 
DEF VAR liCLICount      AS INT  NO-UNDO.
DEF VAR liUpsellCount   AS INT  NO-UNDO.
DEF VAR lc3GBaseCLIType AS CHAR NO-UNDO. 
DEF VAR lc5GBaseCLIType AS CHAR NO-UNDO. 

DEFINE BUFFER bSLGAnalyse FOR SLGAnalyse.
DEFINE BUFFER bMatrix     FOR Matrix.
DEFINE BUFFER bMXItem     FOR MXItem.

ASSIGN lcCLITypes      = "CONT10,CONT26,CONT4,CONT5,CONTDSL45,CONTSF10,CONTFF,CONTFH45_50,CONTSF14,CONTFHNB7G_300,CONTFH55_300,CONTFHNB3G_300,CONTFHNB2G_300,CONTFHNB58_300,CONTFH52_50,CONTFH62_300,CONTFH82_1000"
       lc3GUpsellList  = "FID3GB_R_UPSELL,FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL,FID3GB_12m_R_UPSELL"
       lc5GUpsellList  = "RET5GB_3m_R_UPSELL,RET5GB_6m_R_UPSELL,RET5GB_12m_R_UPSELL,RET5GB_3mP_R_UPSELL,RET5GB_6mP_R_UPSELL,RET5GB_12mP_R_UPSELL"
       lc3GBaseCLIType = "CONT28"
       lc5GBaseCLIType = "CONT34".

DO liUpsellCount = 1 TO NUM-ENTRIES(lc3GUpsellList):
   FOR EACH bSLGAnalyse NO-LOCK WHERE
            bSLGAnalyse.Brand             EQ Syst.Var:gcBrand                    AND
            bSLGAnalyse.ServiceLimitGroup EQ ENTRY(liUpsellCount,lc3GUpsellList) AND 
            bSLGAnalyse.CLIType           EQ lc3GBaseCLIType:

      DO liCLICount = 1 TO NUM-ENTRIES(lcCLITypes):
   
         CREATE SLGAnalyse.
         BUFFER-COPY bSLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
         ASSIGN SLGAnalyse.CliType   = ENTRY(liCLICount,lcCLITypes)
                SLGAnalyse.ValidFrom = TODAY
                SLGAnalyse.ValidTo   = DATE(12,31,2049).

      END.

   END.

END.

DO liUpsellCount = 1 TO NUM-ENTRIES(lc5GUpsellList):
   FOR EACH bSLGAnalyse NO-LOCK WHERE
            bSLGAnalyse.Brand             EQ Syst.Var:gcBrand                    AND
            bSLGAnalyse.ServiceLimitGroup EQ ENTRY(liUpsellCount,lc5GUpsellList) AND 
            bSLGAnalyse.CLIType           EQ lc5GBaseCLIType:

      DO liCLICount = 1 TO NUM-ENTRIES(lcCLITypes):
      
         CREATE SLGAnalyse.
         BUFFER-COPY bSLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
         ASSIGN SLGAnalyse.CliType   = ENTRY(liCLICount,lcCLITypes)
                SLGAnalyse.ValidFrom = TODAY
                SLGAnalyse.ValidTo   = DATE(12,31,2049).

      END.

   END.

END. 

DO liUpsellCount = 1 TO NUM-ENTRIES(lc3GUpsellList):

   FOR FIRST bMatrix NO-LOCK WHERE 
             bMatrix.Brand  EQ Syst.Var:gcBrand AND 
             bMatrix.MXKey  EQ "PERCONTR"       AND 
             bMatrix.MXName BEGINS ENTRY(liUpsellCount,lc3GUpsellList):

      DO liCLICount = 1 TO NUM-ENTRIES(lcCLITypes):
         CREATE MXItem.
         ASSIGN MXItem.MXSeq   = bMatrix.MXSeq            
                MXItem.MXName  = "SubsTypeTo"            
                MXItem.MXValue = ENTRY(liCLICount,lcCLITypes).
      END.

   END.

END.            


DO liUpsellCount = 1 TO NUM-ENTRIES(lc5GUpsellList):

   FOR FIRST bMatrix NO-LOCK WHERE 
             bMatrix.Brand  EQ Syst.Var:gcBrand AND 
             bMatrix.MXKey  EQ "PERCONTR"       AND 
             bMatrix.MXName BEGINS ENTRY(liUpsellCount,lc5GUpsellList):

      DO liCLICount = 1 TO NUM-ENTRIES(lcCLITypes):
         CREATE MXItem.
         ASSIGN MXItem.MXSeq   = bMatrix.MXSeq            
                MXItem.MXName  = "SubsTypeTo"            
                MXItem.MXValue = ENTRY(liCLICount,lcCLITypes).
      END.
             
   END.

END.             
