BLOCK-LEVEL ON ERROR UNDO, THROW.

PROCEDURE pRatePlan:
    DEFINE INPUT PARAMETER icRatePlan          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRPName            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icReferenceRatePlan AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRatePlanAction    AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf_RatePlanCopyFrom  FOR RatePlan.
    DEFINE BUFFER bf_PListConfCopyFrom FOR PListConf.
    DEFINE BUFFER bPListConf           FOR PListConf.
    DEFINE BUFFER bPriceList           FOR PriceList.
    DEFINE BUFFER bTariff              FOR Tariff.

    FIND FIRST bf_RatePlanCopyFrom WHERE bf_RatePlanCopyFrom.Brand = Syst.Var:gcBrand AND bf_RatePlanCopyFrom.RatePlan = icReferenceRatePlan NO-LOCK NO-ERROR.
    IF AVAIL bf_RatePlanCopyFrom THEN 
    DO:
        FIND FIRST RatePlan WHERE RatePlan.Brand = Syst.Var:gcBrand AND RatePlan.RatePlan = icRatePlan NO-LOCK NO-ERROR.
        IF NOT AVAIL RatePlan THEN 
        DO:
            CREATE RatePlan.
            BUFFER-COPY bf_RatePlanCopyFrom EXCEPT RatePlan RPName TO RatePlan
                ASSIGN 
                    RatePlan.RatePlan = icRatePlan
                    RatePlan.RPName   = icRPName.        
                
            FOR EACH bf_PListConfCopyFrom WHERE bf_PListConfCopyFrom.Brand    = Syst.Var:gcBrand                      AND 
                                                bf_PListConfCopyFrom.RatePlan = bf_RatePlanCopyFrom.RatePlan AND
                                                bf_PListConfCopyFrom.dFrom   <= TODAY                        AND
                                                bf_PListConfCopyFrom.dTo     >= TODAY                        NO-LOCK:

                IF icRatePlanAction = "New" AND (bf_PListConfCopyFrom.RatePlan EQ bf_PListConfCopyFrom.PriceList)  THEN 
                DO:
                    /* A copy of existing rateplan's pricelist and related tariffs is created */
                    FIND FIRST PriceList WHERE PriceList.Brand = Syst.Var:gcBrand AND PriceList.PriceList = bf_PListConfCopyFrom.PriceList NO-LOCK NO-ERROR.
                    IF AVAIL PriceList THEN 
                    DO:
                        CREATE bPriceList.
                        BUFFER-COPY PriceList EXCEPT PriceList PLName TO bPriceList
                            ASSIGN 
                                bPriceList.PriceList = RatePlan.RatePlan
                                bPriceList.PLName    = RatePlan.RPName.

                        FOR EACH Tariff WHERE Tariff.Brand = Syst.Var:gcBrand AND Tariff.PriceList = PriceList.PriceList NO-LOCK:                     
                            CREATE bTariff.
                            BUFFER-COPY Tariff EXCEPT TariffNum PriceList ValidFrom TO bTariff
                                ASSIGN 
                                    bTariff.TariffNum = NEXT-VALUE(Tariff)
                                    bTariff.PriceList = bPriceList.PriceList
                                    bTariff.ValidFrom = TODAY.                      
                        END.                

                        CREATE bPListConf.
                        BUFFER-COPY bf_PListConfCopyFrom EXCEPT PriceList RatePlan dFrom dTo TO bPListConf
                            ASSIGN 
                                bPListConf.PriceList = RatePlan.RatePlan
                                bPListConf.RatePlan  = RatePlan.RatePlan
                                bPListConf.dFrom     = TODAY 
                                bPListConf.dTo       = DATE(12,31,2049).
                    END.        
                END.            
                ELSE
                DO: 
                    /* Share existing rateplan's pricelist and related tariffs */                               
                    CREATE bPListConf.
                    BUFFER-COPY bf_PListConfCopyFrom EXCEPT RatePlan TO bPListConf
                        ASSIGN bPListConf.RatePlan = RatePlan.RatePlan.                                                  
                END.
            END.                                      
        END. /* IF NOT AVAIL RatePlan THEN */
    END.

    RETURN "".

END PROCEDURE.

DEFINE VARIABLE lcConvergentCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE lii AS INTEGER NO-UNDO.

lcConvergentCLI = "CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000".

ONE_TRANSACTION:
DO TRANSACTION:
   FOR EACH FMItem EXCLUSIVE-LOCK WHERE
      FMItem.Brand     = Syst.Var:gcBrand AND
      FMItem.FeeModel  = "CONT34MF"       AND
      FMItem.PriceList = "CONTRATOS"      AND
      FMItem.BillCode  = "CONT34MF":

      FMItem.PriceList = "CONTRATO8".
   END.

   DO lii = 1 TO NUM-ENTRIES(lcConvergentCLI):
      FIND CLIType EXCLUSIVE-LOCK WHERE CLIType.CLIType = ENTRY(lii,lcConvergentCLI) NO-ERROR.
      
      IF NOT AVAILABLE CLITYPE
      THEN DO:
         MESSAGE SUBSTITUTE("Did not found CLIType &1",ENTRY(lii,lcConvergentCLI)) VIEW-AS ALERT-BOX.
         UNDO ONE_TRANSACTION, LEAVE ONE_TRANSACTION.
      END.
      CLIType.PricePlan = "CONTRATOCONVF".
   END.

   FIND CLIType EXCLUSIVE-LOCK WHERE CLIType.CLIType = "CONT34" NO-ERROR.
   
   IF NOT AVAILABLE CLITYPE
   THEN DO:
      MESSAGE "Did not found CLIType" VIEW-AS ALERT-BOX.
      UNDO ONE_TRANSACTION, LEAVE ONE_TRANSACTION.
   END.
   
   CLIType.PricePlan = "CONTRATO34".
   
   RUN pRatePlan("CONTRATO34", CLIType.CliName, "CONTRATO8", "UseExisting").   
   
END. 