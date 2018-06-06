/* YCO-457-3Gb-Upsell-legacy-creation-2.p 
   Purpose: add upsells to SLGAnalyse for the legacy tariffs  
*/
DEF VAR lSuccess      AS LOGICAL NO-UNDO.
DEF VAR liUpsells     AS INTEGER NO-UNDO.
DEF VAR liTariff      AS INTEGER NO-UNDO. 
DEF VAR iMaxPrior     AS INTEGER NO-UNDO. 
DEF VAR iNewPrior     AS INTEGER NO-UNDO. 
DEF VAR lSimulation   AS LOGICAL NO-UNDO INITIAL TRUE. 

/* List of valid tariffs for this upsell */
DEF VAR cValidList AS CHAR INITIAL
   "CONTM,CONTM2,CONT6,CONT7,CONT8,CONT9,CONT23,CONT24,CONT27,CONT28,CONT31".

/* List of upsells where to add the tariffs */
DEF VAR cUpsell_Id      AS CHARACTER NO-UNDO INITIAL 
   "FID3GB_R_UPSELL,FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL,FID3GB_12m_R_UPSELL".

/* Checking scripts are executed in the right order */
MESSAGE "Have you executed YCO-457-3Gb-Upsell-legacy-creation?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue AS LOG.
IF lContinue = FALSE THEN 
DO:
   MESSAGE "Aborting. YCO-457-3Gb-Upsell-legacy-creation is mandatory."
      VIEW-AS ALERT-BOX.
   RETURN.
END. 

MESSAGE 
   "Execute in simulation mode?" SKIP(2) 
   "In simulation, system shows current SLG analyse records" SKIP 
   "and show the priority suggested by the program for the new" SKIP 
   "SLG Analyse records but it does NOT create the new SLG analyse records" SKIP(2) 
   "Please pay attention to new priority and change if needed!!!" SKIP(2)
   "Same priority is assigned to all FID3GB upsells created "  
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSimulation.

/* Testing the configuration is not there for any Tariff */
DO liTariff = 1 TO NUM-ENTRIES(cValidList):
   DO liUpsells = 1 TO NUM-ENTRIES(cUpsell_Id):
     
      FIND FIRST SLGAnalyse NO-LOCK
         WHERE SLGAnalyse.Brand             = "1" 
           AND SLGAnalyse.CliType           = ENTRY(liTariff,cValidList)
           AND SLGAnalyse.ServiceLimitGroup = ENTRY(liUpsells,cUpsell_Id)
           AND SLGAnalyse.BDest             = "GPRS"
           AND SLGAnalyse.CCN               = 93 NO-ERROR.
      IF AVAIL SLGAnalyse THEN 
      DO:
         MESSAGE 
            "SGLAnalyse is already there for: " SKIP 
            "CliType:" SLGAnalyse.CliType SKIP 
            "SLGAnalyse.ServiceLimitGroup:" SLGAnalyse.ServiceLimitGroup SKIP(2)
            "Aborting" 
            VIEW-AS ALERT-BOX. 
         RETURN.
      END.
   END.
END.

/* MAIN BLOCK */
blk-upsell:
DO TRANSACTION ON ERROR UNDO blk-upsell, LEAVE blk-upsell
               ON STOP  UNDO blk-upsell, LEAVE blk-upsell:
                  
  lsuccess = FALSE.

  /* Adding */
  DO liTariff = 1 TO NUM-ENTRIES(cValidList):
     
     /* Calculating maximun priority for GPRS in the Tariff (upsells) */
     iMaxPrior = 0.
     FOR EACH SLGAnalyse NO-LOCK
         WHERE SLGAnalyse.Brand   = "1"
           AND SLGAnalyse.CliType = ENTRY(liTariff,cValidList)
           AND SLGAnalyse.BDest   = "GPRS"
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
           AND SLGAnalyse.BDest   = "GPRS"
           AND SLGAnalyse.CCN     = 93:
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
              SLGAnalyse.Billcode          = "14100001"
              SLGAnalyse.CCN               = 93
              SLGAnalyse.BDest             = "GPRS"
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
