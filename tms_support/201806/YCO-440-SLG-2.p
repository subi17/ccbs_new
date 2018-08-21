DEF VAR lSuccess      AS LOGICAL NO-UNDO.
DEF VAR liUpsells     AS INTEGER NO-UNDO.
DEF VAR liTariff      AS INTEGER NO-UNDO.
DEF VAR iMaxPrior     AS INTEGER NO-UNDO. 
DEF VAR iNewPrior     AS INTEGER NO-UNDO. 
DEF VAR lSimulation   AS LOGICAL NO-UNDO INITIAL TRUE. 

/* List of valid tariffs for this upsell */
DEF VAR cValidList AS CHAR INITIAL
   "CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTFH39_50,CONTFH49_300,CONTFH69_1000,CONTFH48_50,CONTFH58_300,CONTFH76_1000,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONT34,CONT15,CONT33,CONT25,CONTFH35_50,CONTFH45_300,CONTFH65_1000".

/* List of upsells with description and feemodel id */
DEF VAR cUpsell_Id      AS CHARACTER NO-UNDO INITIAL 
   "RET5GB_3mP_R_UPSELL,RET5GB_6mP_R_UPSELL,RET5GB_12mP_R_UPSELL".

MESSAGE 
   "Execute in simulation mode?" SKIP(2) 
   "In simulation, system shows current SLG analyse records" SKIP 
   "and show the priority suggested by the program for the new" SKIP 
   "SLG Analyse records but it does NOT create the new SLG analyse records" SKIP(2) 
   "Please pay attention to new priority and change if needed!!!" SKIP(2)
   "Same priority is assigned to all FID3GB upsells created "  
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSimulation.

blk-upsell:
DO TRANSACTION ON ERROR UNDO blk-upsell, LEAVE blk-upsell
               ON STOP  UNDO blk-upsell, LEAVE blk-upsell:
                  
  lsuccess = FALSE.

  DO liTariff = 1 TO NUM-ENTRIES(cValidList):
     
     /* Calculating maximun priority for GPRS in the Tariff (upsells) */
     iMaxPrior = 0.
     FOR EACH SLGAnalyse NO-LOCK
         WHERE SLGAnalyse.Brand   = "1" 
           AND SLGAnalyse.CliType = ENTRY(liTariff,cValidList)
           AND SLGAnalyse.BDest   = "ROAMGPRS_EU"
           AND SLGAnalyse.CCN     = 90
           AND SLGAnalyse.ServiceLimitGroup MATCHES "*UPSELL*":
         IF SLGAnalyse.Prior > iMaxPrior THEN 
            iMaxPrior = SLGAnalyse.Prior.
     END.
     
     iNewPrior = iMaxPrior.
           
     DISP "Tarif:" ENTRY(liTariff,cValidList) FORMAT "X(20)" 
          "new prior:" iNewPrior WITH no-labels FRAME b.

     /* Showing GPRS data */
     FOR EACH SLGAnalyse NO-LOCK
         WHERE SLGAnalyse.Brand   = "1" 
           AND SLGAnalyse.CliType = ENTRY(liTariff,cValidList)
           AND SLGAnalyse.BDest   = "ROAMGPRS_EU"
           AND SLGAnalyse.CCN     = 90:
        DISP SLGAnalyse.CliType SLGAnalyse.ServiceLimitGroup SLGAnalyse.Prior
           WITH NO-LABELS.
     END. 

     UPDATE iNewPrior WITH FRAME b.

     IF lSimulation = FALSE THEN
     DO:     
        DO liUpsells = 1 TO NUM-ENTRIES(cUpsell_Id):
           CREATE SLGAnalyse.
           ASSIGN 
              SLGAnalyse.Brand             = "1" 
              SLGAnalyse.BelongTo          = TRUE  /* + */
              SLGAnalyse.ServiceLimitGroup = ENTRY(liUpsells,cUpsell_Id) 
              SLGAnalyse.CliType           = ENTRY(liTariff,cValidList)
              SLGAnalyse.Billcode          = "14104019"
              SLGAnalyse.CCN               = 90
              SLGAnalyse.BDest             = "ROAMGPRS_EU"
              SLGAnalyse.ValidFrom         = DATE(05,01,2018)
              SLGAnalyse.ValidTo           = DATE(12,31,2049)
              SLGAnalyse.Prior             = iNewPrior
              SLGAnalyse.SLGAType          = 6.
           RELEASE SLGAnalyse. 
        END.
     END.
  
  END.  /* DO liUpsells */
  
  /* Process OK */
  lSuccess = TRUE.
END.

IF lSuccess THEN
    MESSAGE "Upsells successfully created" VIEW-AS ALERT-BOX.
ELSE 
    MESSAGE "ERROR: Failed to create the upsells" VIEW-AS ALERT-BOX.



    