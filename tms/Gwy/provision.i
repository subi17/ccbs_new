&IF "{&PROVISION_I}" NE "YES"
&THEN


&GLOBAL-DEFINE PROVISION_I YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/multitenantfunc.i}

function fMakeCommLine returns CHAR
(INPUT iiSolog     AS INT,
 INPUT icValue     AS CHAR).

   DEF BUFFER provSolog     FOR Solog.
   DEF BUFFER provMobsub    FOR Mobsub.
   DEF BUFFER provMSREquest FOR MSRequest.
   DEF BUFFER provCliType   FOR CLIType.
   DEF BUFFER provTermMobsub FOR TermMobsub.

   DEF VAR lcAdkey    AS CHAR NO-UNDO.
   DEF VAR lcReturn   AS CHAR NO-UNDO.
   DEF VAR lcPayTypes AS CHAR NO-UNDO INIT "UNKNOWN,POSTPAID,PREPAID".
   DEF VAR lcProfile  AS CHAR NO-UNDO.
   DEF VAR lcPayType  AS CHAR NO-UNDO.

   DEF VAR lhMobSub   AS HANDLE NO-UNDO.

   FIND FIRST provSolog WHERE 
              provSolog.Solog = iiSolog NO-LOCK NO-ERROR.

   IF NOT AVAIL provSolog THEN LEAVE.

   IF icValue = "REACTIVATE" THEN DO:
      lhMobSub = BUFFER provTermMobsub:HANDLE.
      FIND FIRST provTermMobsub WHERE
                 provTermMobsub.MSSeq = ProvSolog.MSSeq  NO-LOCK NO-ERROR.  
   END. /* IF icValue = "REACTIVATE" THEN DO: */
   ELSE DO:
      lhMobSub = BUFFER provMobsub:HANDLE.
      FIND FIRST provMobsub NO-LOCK WHERE
                 provMobsub.MSSeq = ProvSolog.MSSeq AND
                 provMobsub.MsStatus NE {&MSSTATUS_MOBILE_PROV_ONG} NO-ERROR.
   END. /* ELSE DO: */

   IF lhMobSub:AVAILABLE THEN
   FIND FIRST provCliType WHERE
              provClitype.Brand   = lhMobSub::Brand AND 
              provClitype.CliType = lhMobSub::CliType NO-LOCK NO-ERROR.
   IF AVAILABLE provCliType THEN lcProfile = provCliType.ServicePack.
      
   FOR EACH Order NO-LOCK WHERE
            Order.MSSeq = ProvSolog.MSSeq AND
            LOOKUP(STRING(Order.OrderType),"0,1,3") > 0
      BY Order.CrStamp DESC:
      LEAVE.
   END.
  
   /* CREATE extra parameters */ 
   IF LOOKUP(icValue, "CREATE,REACTIVATE") > 0 THEN DO:

      /* Add the logic to support command line for Reactivation */
      IF icValue = "REACTIVATE" THEN DO:
         lcPayType = STRING(lhMobSub::PayType,"PREPAID/POSTPAID").

         FIND FIRST Sim WHERE
                    Sim.ICC = lhMobSub::ICC NO-LOCK NO-ERROR.
         FIND FIRST CliType WHERE 
                    CliType.Brand    = lhMobSub::Brand  AND 
                    CliType.CliType  = lhMobSub::CliType NO-LOCK NO-ERROR.
      END. /* IF icValue = "REACTIVATE" THEN DO: */

      ELSE IF Avail Order THEN DO:
         FIND FIRST Sim WHERE
                    Sim.ICC = order.ICC NO-LOCK NO-ERROR.
         
         FIND FIRST CliType WHERE 
                    CliType.Brand    = Order.Brand  AND 
                    CliType.CliType  = Order.CliType NO-LOCK NO-ERROR.
         ASSIGN lcPayType = (IF AVAILABLE CLIType AND 
                             CLIType.PayType + 1 <= NUM-ENTRIES(lcPayTypes)
                             THEN ENTRY(CLIType.PayType + 1,lcPayTypes)
                             ELSE STRING(Order.PayType,"PREPAID/POSTPAID"))
                lcProfile = (IF AVAILABLE CLIType 
                             THEN CLIType.ServicePack
                             ELSE IF BUFFER-TENANT-NAME(Order) = {&TENANT_MASMOVIL}
                             THEN STRING(Order.PayType,"52/51")
                             ELSE STRING(Order.PayType,"42/41")).

      END. /* ELSE IF Avail Order THEN DO: */

      FIND FIRST IMSI WHERE IMSI.ICC = SIM.ICC NO-LOCK NO-ERROR.
      
      ASSIGN lcADkey = "PAYTYPE=" + lcPayType +
                       ",PROFILE=" + lcProfile +
                       ",KI=" + IMSI.KI + ",".
      
      IF Avail Order AND Order.MnpStatus > 0 THEN DO:

         FIND FIRST MNPOperator WHERE
                    MNPOperator.Brand = Syst.Var:gcBrand AND
                    MNPOperator.OperName = STRING(order.curroper) AND
                    MNPOperator.Active = True
         NO-LOCK NO-ERROR.

         IF NOT AVAIL MNPOperator THEN
            FIND FIRST MNPOperator WHERE
                       MNPOperator.Brand = Syst.Var:gcBrand AND
                       MNPOperator.OperName = STRING(order.curroper) AND
                       MNPOperator.Active = False
            NO-LOCK NO-ERROR.

         IF AVAIL MNPOperator AND
                  MNPOperator.NRN > ""
         THEN lcAdKey = lcAdKey + "MNP=" + MNPOperator.NRN + ",".
         ELSE lcAdkey = lcAdKey + "MNP,".
      END.

                       
      IF Avail CliType THEN DO:
         IF CliType.ServiceClass ne "" THEN 
            lcAdkey = lcAdkey + "SERVICECLASS=" + CliType.ServiceClass + ",".
         IF CLIType.CLIType EQ "CONTM2" THEN
            lcAdkey = lcAdkey + "BARRING=0110000,".
      END.

      lcAdkey = lcAdKey + "SERVICE_OPERATOR=" + CAPS(fConvertTenantToBrand(BUFFER-TENANT-NAME(provSolog))) + ",".

   END.

   IF icValue = "DELETE" THEN DO:

      FIND FIRST ProvMsRequest WHERE
                 ProvMsRequest.MsSeq   = SoLog.MsSeq AND
                 ProvMsRequest.ReqType = 18
      NO-LOCK NO-ERROR.

      IF NOT AVAIL ProvMsRequest THEN 
      FIND FIRST ProvMsRequest WHERE
                 ProvMsRequest.MsSeq   = SoLog.MsSeq AND
                 ProvMsRequest.ReqType = 13          AND
                 ProvMSRequest.ReqCparam1 = "DELETE" 
      NO-LOCK NO-ERROR.
                                                   
      IF AVAIL provMSRequest AND 
               ProvMsRequest.ReqCParam2 NE "" THEN
         lcAdkey = lcAdkey + "MNP=" + STRING(ProvMsRequest.ReqCParam2) + ",".

      lcReturn = STRING(ProvSolog.Solog)  +  " " + "DELETE" + ","  + /* Action*/
                "MSISDN=34" + lhMobSub::Cli       + ","  +     /* MSISDN    */
                "IMSI="   + lhMobSub::Imsi       + ","  +     /* IMSI      */
                ",PAYTYPE=" + STRING(lhMobSub::PayType,"PREPAID/POSTPAID") +
                ",OPERATOR=YOIGO," + lcadkey    .
   END.                       
   
   ELSE IF Avail ProvMobsub  THEN DO:
     lcReturn = STRING(ProvSolog.Solog)      +  " " + 
                icValue                      + ","  +     /* Action  */
               "MSISDN=34" + lhMobSub::Cli   + ","  +     /* MSISDN       */
               "IMSI="   + lhMobSub::Imsi  + ","  +     /* IMSI         */
               "PAYTYPE=" + STRING(lhMobSub::PayType,"PREPAID/POSTPAID") +
               ",OPERATOR=YOIGO,"     .
   END.
   ELSE IF icValue = "REACTIVATE"  THEN
     lcReturn = STRING(ProvSolog.Solog)      + " "  + 
                "CREATE"                     + ","  +  /* Action-use the same command line for Reactivation */
               "MSISDN=34" + lhMobSub::Cli   + ","  +  /* MSISDN       */
               "IMSI="   + lhMobSub::Imsi  + ","  +    /* IMSI         */
               ",OPERATOR=YOIGO," + lcAdkey.
   ELSE IF NOT AVAIL ProvMObsub AND Avail Order  
   THEN DO:
        
      FIND FIRST Imsi WHERE 
                 Imsi.ICC = Order.ICC NO-LOCK NO-ERROR.
        
      lcReturn = STRING(ProvSolog.Solog)      +  " " +
                 icValue                      + ","  +     /* Action  */
                "MSISDN=34" + Order.Cli        + ","  +     /* MSISDN       */
                "IMSI="   + Imsi.Imsi        + ","  +     /* IMSI         */
                "OPERATOR=YOIGO," + 
                lcAdkey .
   END.
   ELSE DO:
     lcReturn = STRING(ProvSolog.Solog)      +  " " + 
                icValue     + ","  +     /* Action  */
               "MSISDN=34" + Order.Cli        + ","  +     /* MSISDN       */
               "OPERATOR=YOIGO," + 
               lcAdKey                      + "," .   
   END.

   RETURN lcReturn.

END.   



function fMakeCommLine2 returns CHAR
(INPUT iiSolog     AS INT,
 INPUT iiMSRequest AS INT,
 INPUT ilSTCResend AS LOG).

   DEF BUFFER provSolog     FOR Solog.
   DEF BUFFER provMobsub    FOR Mobsub.
   DEF BUFFER provMSREquest FOR MSRequest.
   DEF BUFFER provCliType   FOR CLIType.
   DEF BUFFER provTermMobsub FOR TermMobsub.
   DEF BUFFER provIMSI      FOR IMSI.
   DEF BUFFER provSIM       FOR SIM.

   DEF VAR lcAdkey    AS CHAR NO-UNDO.
   DEF VAR lcReturn   AS CHAR NO-UNDO.
   DEF VAR lcPayTypes AS CHAR NO-UNDO INIT "UNKNOWN,POSTPAID,PREPAID".
   DEF VAR lcNewtype  AS CHAR NO-UNDO.
   DEF VAR llNewType  AS LOG  NO-UNDO.
  
   DEF VAR lhMobSub   AS HANDLE NO-UNDO.

   FIND FIRST provSolog WHERE
              provSolog.Solog = iiSolog NO-LOCK NO-ERROR.
   
   FIND FIRST ProvMSRequest WHERE 
              ProvMSRequest.MSrequest = iiMSRequest NO-LOCK NO-ERROR.

   FIND FIRST provMobsub NO-LOCK WHERE
              provMobsub.MSSeq = ProvSolog.MSSeq AND
              provMobsub.MsStatus NE {&MSSTATUS_MOBILE_PROV_ONG} NO-ERROR.
   IF AVAILABLE provMobsub THEN
      lhMobSub = BUFFER provMobsub:HANDLE.
   ELSE DO:
      FIND FIRST provTermMobsub WHERE
                 provTermMobsub.MSSeq = ProvSolog.MSSeq NO-LOCK NO-ERROR.
      IF AVAILABLE provTermMobsub THEN
         lhMobSub = BUFFER provTermMobsub:HANDLE.
   END. /* ELSE DO: */

   IF lhMobSub:AVAILABLE THEN
   FIND FIRST provCliType WHERE
              provClitype.Brand   = lhMobSub::Brand AND
              provClitype.CliType = lhMobSub::CliType NO-LOCK NO-ERROR.
   
   FOR EACH Order NO-LOCK WHERE
            Order.MSSeq = ProvSolog.MSSeq AND
            LOOKUP(STRING(Order.OrderType),"0,1,3") > 0
      BY Order.CrStamp DESC:
      LEAVE.
   END.

   IF ProvMSrequest.ReqCParam1 = "CHANGEMSISDN" THEN DO:
   
      lcReturn = STRING(ProvSolog.Solog)      +  " " +
                "MODIFY" + ","  +                       /* Action  */
                "MSISDN=34" + lhMobSub::Cli   + "->34" +    /* MSISDN  */
                 ProvMSrequest.ReqCParam2     + ","  +
                "IMSI="   + lhMobSub::Imsi  + ","  +    /* IMSI    */
                "OPERATOR=YOIGO," + 
                "PAYTYPE=" + STRING(lhMobSub::PayType,"PREPAID/POSTPAID").

   END.
   ELSE IF ProvMSrequest.ReqCParam1 = "CHANGEICC" THEN DO:
      
      FIND FIRST ProvSim WHERE 
                 ProvSim.Icc = ProvMSrequest.ReqCParam2 NO-LOCK NO-ERROR.

      FIND FIRST ProvIMSI WHERE 
                 ProvImsi.Icc = ProvSim.Icc NO-LOCK NO-ERROR.
      
      lcReturn = STRING(ProvSolog.Solog)      +  " " +
                "MODIFY" + ","  +                           /* Action  */
                "MSISDN=34" + lhMobSub::Cli   +  ","  +    /* MSISDN  */
                "IMSI="     + lhMobSub::IMSI + "->" + ProvIMSI.IMSI  + "," +
                "KI=" +       ProvImsi.ki  +  "," +          
                "OPERATOR=YOIGO," +
                "PAYTYPE=" + STRING(lhMobSub::PayType,"PREPAID/POSTPAID").
      
   END.
   ELSE IF ProvMSrequest.ReqType = 0 THEN DO:
   
      FIND FIRST ProvCliType WHERE
                 ProvCliType.CliType = ProvMSrequest.ReqCParam2
      NO-LOCK NO-ERROR.
                
      IF NOT AVAILABLE ProvCLIType THEN RETURN "". 
                             
      IF ProvCliType.ServiceClass ne ""
      THEN lcAdkey = "SERVICECLASS=" + 
            (IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ6" THEN "0007"
             ELSE IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ7" THEN "0003"
             ELSE IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ9" THEN "0009"
             ELSE IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ10" THEN "0010"
             ELSE IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ11" THEN "0011"
             ELSE IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ12" THEN "0012"
             ELSE IF ProvMSrequest.ReqCParam1 BEGINS "TARJ" AND
                ProvCLIType.CLIType = "TARJ13" THEN "0020"
             ELSE ProvCliType.ServiceClass) + "," .

      IF ProvCLIType.PayType + 1 <= NUM-ENTRIES(lcPayTypes)
      THEN ASSIGN
         lcNewType = ENTRY(ProvCLIType.PayType + 1,lcPayTypes)
         llNewType = (ProvCLIType.PayType = 2).
      ELSE RETURN "".

      IF lhMobSub::PayType NE llNewType AND
         NOT ilSTCResend THEN  
      lcAdkey = lcAdkey +
      "PAYTYPE=" + TRIM(STRING(lhMobSub::PayType,"PREPAID/POSTPAID")) + "->" +
      TRIM(lcNewType) + "," .
      ELSE lcAdkey = lcAdkey + "PAYTYPE=" + TRIM(lcNewType) + ",".
      
      lcReturn = STRING(ProvSolog.Solog)     +  " " +
              "MODIFY"                    + ","  +     /* Action  */
              "MSISDN=34" + lhMobSub::Cli       + ","  +     /* MSISDN       */
              "IMSI="   +   lhMobSub::Imsi      + ","  +     /* IMSI         */
              "OPERATOR=YOIGO," + 
              lcAdkey .
   END.
  
   RETURN lcReturn.

END.   

FUNCTION fGetShaperConfCommLine RETURN CHAR
   (INPUT icShaperConfID AS CHAR):

   DEF VAR lcCommLine AS CHAR NO-UNDO. 

   FIND FIRST ShaperConf NO-LOCK WHERE
              ShaperConf.Brand = Syst.Var:gcBrand AND
              ShaperConf.ShaperConfID = icShaperConfID
   NO-ERROR.

   IF NOT AVAIL ShaperConf THEN RETURN "".
   
   ASSIGN
      lcCommLine = lcCommLine + 
        "TARIFF_TYPE=" + STRING(ShaperConf.TariffType) + "," WHEN
         ShaperConf.TariffType > ""
      lcCommLine = lcCommLine + 
         "TARIFF=" + STRING(ShaperConf.Tariff) + "," WHEN
         ShaperConf.Tariff > ""
      lcCommLine = lcCommLine + 
         "TEMPLATE=" + STRING(ShaperConf.Template) + "," WHEN
                              ShaperConf.Template > "" 
      lcCommLine = lcCommLine + 
         "LIMIT_UNSHAPED=" + STRING(ShaperConf.LimitUnShaped) + "," WHEN
                                    ShaperConf.LimitUnShaped NE ?
      lcCommLine = lcCommLine + 
         "LIMIT_SHAPED=" + STRING(ShaperConf.LimitShaped) + "," WHEN
                                  ShaperConf.LimitShaped NE ?.

   RETURN lcCommLine.
END.

&ENDIF
