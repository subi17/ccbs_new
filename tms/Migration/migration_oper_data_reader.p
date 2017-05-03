/* ----------------------------------------------------------------------
  MODULE .......: migration_oper_data_reader.p
  TASK .........: Program reads Migration Finalization Data Files.
                  It sets services, barrings, bundles, upsells and sets
                  given data amount to network.
                  Program procides information about success in JSON format 
                  to Migration Tool (WEB).
                  Format for input file:
|MM internal ID|MSISDN|BARRINGS|SERVICES|BONO|UPSELL|USED_DATA_AMOUNT|
|MM internal ID|MSISDN|barring_1;barring_2|service_1;service_2;service_3||upsell_1|34000| 
  APPLICATION ..: tms
  AUTHOR .......: ilsavola
  CREATED ......: 30.1.2017
  Version ......: yoigo
---------------------------------------------------------------------- */
{Syst/commpaa.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/barrfunc.i}
{Func/upsellbundle.i}
{Mc/dpmember.i}
{Migration/migrationfunc.i}
{Func/ftransdir.i}

gcBrand = "1".

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.
DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionID AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC NO-UNDO.
DEF VAR lcInDir AS CHAR NO-UNDO.
DEF VAR ldaReadDate AS DATETIME.
DEF VAR lcTimePart AS CHAR. /*For log file name*/
DEF VAR lcLogFile AS CHAR NO-UNDO.
DEF VAR lcRowStatus AS CHAR NO-UNDO.
DEF VAR lcErr AS CHAR NO-UNDO.
DEF VAR lcFileName AS CHAR NO-UNDO.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcProcDir AS CHAR NO-UNDO.


DEFINE TEMP-TABLE ttBarrings NO-UNDO SERIALIZE-NAME ""
   FIELD barring_id AS CHARACTER
   FIELD barring_value AS CHARACTER SERIALIZE-NAME "value".

DEFINE TEMP-TABLE ttSingleFees NO-UNDO SERIALIZE-NAME ""
   FIELD single_fee_id AS CHARACTER
   FIELD amount AS DECIMAL.

DEFINE TEMP-TABLE ttUpsells NO-UNDO SERIALIZE-NAME ""
   FIELD upsell_id AS CHARACTER
   FIELD amount AS INTEGER.

DEFINE TEMP-TABLE ttDiscounts NO-UNDO SERIALIZE-NAME ""
   FIELD discount_id AS CHARACTER
   FIELD begin_date AS DATE.

DEFINE TEMP-TABLE ttRootlevel NO-UNDO SERIALIZE-NAME ""
   FIELD msisdn           AS CHARACTER
   FIELD used_data_amount AS INTEGER
   FIELD saldo            AS INTEGER
   FIELD fat              AS INTEGER.



ASSIGN
   lcTableName = "MB_Migration"
   lcActionID = "migration_oper_data_reader"
   ldCurrentTimeTS = fMakeTS()
   lcLogDir = fCParam("MB_Migration", "MigrationLogDir")
   lcInDir = fCParam("MB_Migration", "MigrationInDir").

IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".
IF lcInDir EQ "" OR lcInDir EQ ? THEN DO:
   ASSIGN
      lcInDir = "/tmp/"
      lcLogDir = "/tmp/"
      lcProcDir = "/tmp/".
END.
ELSE DO:
   ASSIGN
      lcProcDir = lcInDir + "/processed/"
      lcLogDir = lcInDir + "/logs/"
      lcInDir = lcInDir + "/incoming/".
END.


/*Set output and log files*/
ldaReadDate = TODAY.
lcTimePart = STRING(YEAR(ldaReadDate)) +
             STRING(MONTH(ldaReadDate),"99") +
             STRING(DAY(ldaReadDate),"99") +
             REPLACE(STRING(TIME,"HH:MM:SS"),":","").
lcLogFile = lcLogDir + "MM_MIGRATION_FINAL_DATA__" + lcTimePart + ".log".

OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

PUT STREAM sLog UNFORMATTED
   "Migration filefinal data reading starts " + fTS2HMS(fMakeTS()) SKIP.

/*Ensure that multiple instances of the program are not running*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      PUT STREAM sLog UNFORMATTED
         "File processing alrady ongoing " + fTS2HMS(fMakeTS()) SKIP.
      OUTPUT STREAM sLog CLOSE.
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      QUIT. /*No reporting in first time.*/
   END.
END.
/*Execution part*/
lcErr = fInitMigrationMQ("oper_data_reader").
IF lcErr NE "" THEN DO:
   PUT STREAM sLog UNFORMATTED
   "MQ error. Operational data file will be skipped: " + lcErr +
      fTS2HMS(fMakeTS()) SKIP.

END.
ELSE DO:
   /*Execution part*/
   INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcindir + "/OPER_DATA*" ).
   REPEAT:
      IMPORT STREAM sFile UNFORMATTED lcFileName.
      lcInputFile =  lcFileName.
      IF SEARCH(lcInputFile) NE ? THEN INPUT STREAM sIn FROM VALUE(lcInputFile).
      ELSE NEXT.
      /*OPER_DATA_SET -> setting
       OPER_DATA_VALIDATE -> validation
      */ 

      IF lcInputFile MATCHES "*OPER_DATA_SET*" THEN /**/
         RUN pReadInputJSON(lcInputFile, FALSE).
      ELSE RUN pReadInputJSON(lcInputFile, TRUE).
      fMove2TransDir(lcInputFile,"",lcProcDir).
   END.


END.
/*Release ActionLog lock*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

PUT STREAM sLog UNFORMATTED
   "Migration finalization file handling done " + fTS2HMS(fMakeTS()) SKIP.
OUTPUT STREAM sLog CLOSE.

/*Precheck functions. Function returns text in case of error.*/
FUNCTION fBarringExists RETURNS CHAR
   (icBarringCode AS CHAR):
   DEF BUFFER BarringConf FOR BarringConf.
   FIND FIRST BarringConf NO-LOCK WHERE 
              BarringConf.BarringCode EQ icBarringCode 
              NO-ERROR.
   IF NOT AVAIL BarringConf THEN RETURN "Barring not found " + icBarringCode.

   RETURN "".
END.   

/*Precheck functions. Function returns text in case of error.*/
FUNCTION fUpsellExists RETURNS CHAR
   (icUPS AS CHAR):
   DEF BUFFER DayCampaign for DayCampaign.
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand EQ gcBrand AND
              DayCampaign.DCEvent EQ icUps NO-ERROR.
   IF NOT AVAIL DayCampaign THEN RETURN "Item not found " + icUPS.

   RETURN "".
END.

/*Precheck functions. Function returns text in case of error.*/
FUNCTION fBonoExists RETURNS CHAR
   (icBono AS CHAR):
   DEF VAR lcAllowedBonos AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
   /*lcAllowedBonos = "BONO_RELAX".*/
   lcAllowedBonos = fCParamC("BONO_CONTRACTS").
   IF NUM-ENTRIES(lcAllowedBonos) EQ 0 THEN RETURN "Incorrec bono list".
   DO i=1 TO NUM-ENTRIES(lcAllowedBonos):
      IF ENTRY(i, lcAllowedBonos) EQ icBono THEN RETURN "".

   END.
   RETURN "Bono not found " + icBono.

END.


/*Functions and procedures*/

/*Function validates barring request.
Codes:
   EMPTY                                 :  The command is OK
   Validation Error + description        :  Errorneous request
   Ongoing network command               :  Barring command was ongoing  
   ERROR: MobSub not found.              :  MobSub not found
   ERROR: Mobile line provisioning is not complete : Mobsub, wrong status
   */
FUNCTION fValidateMMBarringCommand RETURNS CHAR
   (iiMsSeq AS INT,
    icInput AS CHAR): /*In processed format b1=1,B2=1...*/
    
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcValStatus AS CHAR NO-UNDO.

   FIND MobSub NO-LOCK WHERE Mobsub.MsSeq = iiMsSeq NO-ERROR.

   IF NOT AVAIL MobSub THEN RETURN "ERROR: MobSub not found.".
   IF icInput EQ "" THEN RETURN "". /*It is notmandatory to update*/

   IF fIsReasonableSet(icInput, MobSub.MsSeq) EQ FALSE THEN 
      RETURN "Barring already in given status:" + icInput. 


   IF MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} /*16*/ THEN
      RETURN "ERROR: Mobile line provisioning is not complete".

   IF icInput EQ "" THEN RETURN "".
   lcValStatus = fValidateBarrRequest(iiMsSeq, icInput).
   IF lcValStatus EQ "ONC" THEN RETURN "Ongoing network command".
   ELSE IF lcValStatus NE "" THEN RETURN "Validation Error: " + lcValStatus.
   
   RETURN lcValStatus.


END.

/*Function checks if that is a setting allowed for specific clitype.*/
/*RETURNs emptyy if OK, text in error cases.*/
FUNCTION fIsPermittedModule RETURNS CHAR 
 ( icCliType AS CHAR,
   icModule AS CHAR):

   DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO.

   IF fMatrixAnalyse("1",
                     "DENIED",
                     "SubsTypeFrom;Module",
                     icCliType + ";" + icModule,
                     OUTPUT ocResult) = ? THEN DO:
      RETURN "".
   END.

   RETURN "Operation not allowed " + icCliType.

END FUNCTION.


/*Function sets given barring set to given MobSub.
 -Validate command
 -Execute command
 Error handling:
 "" - OK
 Other code -> Barring setting failed*/
FUNCTION fSetMigrationBarring RETURNS CHAR
   (iiMsSeq AS INT,
    icCommand AS CHAR,
    ilgSimulate AS LOGICAL):
   DEF VAR lcStat AS CHAR NO-UNDO.
   DEF VAR liReq AS INT NO-UNDO.
   
   lcStat = fValidateMMBarringCommand(iiMsSeq, icCommand).
   IF lcStat NE "" THEN RETURN lcStat.
   IF icCommand EQ "" THEN RETURN "".

   IF ilgSimulate EQ TRUE THEN RETURN "".
   RUN Mm/barrengine.p(iiMsSeq,
                       icCommand,
                       {&REQUEST_SOURCE_MIGRATION},
                       "",
                       fMakeTS(),
                       "",
                       OUTPUT lcStat).

   liReq = INT(lcStat) NO-ERROR.
   IF liReq EQ 0 THEN RETURN lcStat.
   RETURN "".
END.    

FUNCTION fSetMigrationFees RETURNS CHAR
   (iiMsSeq AS INT,
    icCommand AS CHAR):
   RETURN "".
END.
/*Function sets migration related upsells*/
/*Note: current version suppots only postpaid upsells*/
FUNCTION fSetMigrationUpsell RETURNS CHAR
   (iiMsSeq AS INT,
    icCommand AS CHAR,
    iiAmount AS INT,
    ilgSimulate AS LOGICAL):
    DEF VAR i AS INT NO-UNDO.
    DEF VAR lcError AS CHAR NO-UNDO.
    DEF VAR lcReturn AS CHAR NO-UNDO.
    DEF VAR liReq AS INT NO-UNDO.
    IF NOT icCommand MATCHES "*UPSELL*" THEN 
       RETURN "Not valid upsell " + icCommand.
       
    IF ilgSimulate EQ TRUE THEN RETURN "".

    FIND FIRST MobSub NO-LOCK where 
               MobSub.MsSeq EQ iiMsSeq NO-ERROR.
    IF NOT AVAIL MobSub THEN RETURN "Upsell setting failed, no mobsub " +
                             STRING(iiMsSeq).
    /* TODO mobsub cgheck */
               
    DO i = 1 TO iiAmount:
       fCreateUpsellBundle(iiMsSeq,
                           icCommand,
                           {&REQUEST_SOURCE_MIGRATION},
                           fMakeTS(),
                           OUTPUT liReq,
                           OUTPUT lcError).
       IF lcError NE "" THEN DO:
          lcReturn = lcReturn + "|" + lcError.
       END.
   END.

   RETURN "".
END.


/*Function sets migration related upsells*/
/*Note: current version suppots only postpaid upsells*/
FUNCTION fSetMigrationUpsells RETURNS CHAR
   (iiMsSeq AS INT,
    icCommand AS CHAR):
    DEF VAR i AS INT NO-UNDO.
    DEF VAR lcUpsell AS CHAR NO-UNDO.
    DEF VAR lcError AS CHAR NO-UNDO.
    DEF VAR lcReturn AS CHAR NO-UNDO.
    DEF VAR liReq AS INT NO-UNDO.

    DO i = 1 TO NUM-ENTRIES(icCommand,icCommand):
       lcUpsell = ENTRY(i, icCommand).
       IF lcUpsell MATCHES "*_UPSELL*" THEN DO:

          fCreateUpsellBundle(iiMsSeq,
                              lcUpsell,
                              {&REQUEST_SOURCE_MIGRATION},
                              fMakeTS(),
                              OUTPUT liReq,
                              OUTPUT lcError).
          IF lcError NE "" THEN DO:
             lcReturn = lcReturn + "|" + lcError.
          END.
       END.
       ELSE lcReturn = lcReturn + "|" + "Not valid upsell: " + lcUpsell.
   END.

   RETURN lcReturn.
END.

/*Functionality adds terminal, */
FUNCTION fSetMigrationTerminals RETURNS CHAR
   (iiMsSeq AS INT,
    iiOrderID AS INT,
    icTerminals AS CHAR):
   DEF VAR lcTerminal AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.

    
   DO i = 1 TO NUM-ENTRIES(icTerminals):
      lcTerminal =  STRING(ENTRY(i,icTerminals)).
      CREATE SubsTerminal.
      ASSIGN SubsTerminal.brand = gcBrand
             SubsTerminal.OrderID = iiOrderid
             SubsTerminal.imei = lcTerminal.

   END.

   RETURN "".
END.

FUNCTION fSetMigrationUsedData RETURNS CHAR
   (iiMsSeq AS INT,
    iiCommand AS INT):
   RETURN "".
END.

FUNCTION fSetMigrationDiscounts RETURNS CHAR
   (iiMsSeq AS INT,
   icCommand AS CHAR):
   DEF VAR lcStat AS CHAR NO-UNDO.
   DEF BUFFER mobsub for mobsub.

   IF icCommand EQ "" THEN RETURN "".
   FIND FIRST MobSub NO-LOCK where
              MobSub.MsSeq EQ iiMsSeq NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "Mobsub not found".           
   
   /*Checkint that is discount allowed*/
   lcStat = fIsPermittedModule(MobSub.CliType, "dpmember").
   IF lcStat NE "" THEN
      RETURN "Discount setting failed" + lcStat.


   CREATE DPMember.
   ASSIGN
      DPMember.DPMemberID = NEXT-VALUE(DPMemberID)
      DPMember.DPID       = 0 /*TODO define*/
      DPMember.HostTable  = "MobSub"
      DPMember.KeyValue   = STRING(iiMsSeq)
      DPMember.ValidFrom  = TODAY
      DPMember.ValidTo    = 12/31/2049.

   IF DPMember.DPID > 0 THEN DO:
      FOR FIRST DPRate NO-LOCK WHERE
                DPRate.DPId EQ DPMember.DPID AND
                DPRate.ValidTo >= TODAY AND
                DPRate.ValidFrom <= TODAY:
        DpMember.DiscValue = DPRate.DiscValue.
      END.

      FIND FIRST DiscountPlan WHERE DiscountPlan.DPID EQ DPMember.DPID NO-LOCK.
      IF DiscountPlan.ValidPeriods > 0 THEN
          DPMember.ValidTo = fCalcDPMemberValidTo(DPMember.ValidFrom,
                                                  DiscountPlan.ValidPeriods). 
   END.

   RETURN "".
END.

/*Function sets bono that is defined.*/
FUNCTION fSetMigrationBonos RETURNS CHAR
   (iiMsSeq AS INT,
    icCommand AS CHAR):
    DEF VAR i AS INT NO-UNDO.
    DEF VAR lc AS CHAR NO-UNDO.
    DEF VAR lcError AS CHAR NO-UNDO.
    DEF VAR liRequest AS INT NO-UNDO.
    DEF VAR lcReturn AS CHAR NO-UNDO.

    IF icCommand EQ "" THEN RETURN "".

    IF icCommand MATCHES "*BONO*" THEN DO:

       liRequest = fPCActionRequest(iiMsSeq,
                                icCommand,
                                "act",
                                fMakeTS(),
                                TRUE,    /* fees */
                                {&REQUEST_SOURCE_MIGRATION},
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT lcError).
       IF liRequest EQ 0 THEN DO:
          lcReturn = lcReturn + "|" + "Bono request creation failed " + lcError.
       END.
       ELSE lcReturn = lcReturn + "|" + "Not valid bono: " + icCommand.
   END.

   RETURN lcReturn.
END.

FUNCTION fSetMigrationFAT RETURNS CHAR
   (iiMsSeq AS INT,
    idCommand AS DECIMAL):
/*
     ocErrInfo =  fCreateFatRow(
                             "GOOGLEVASFAT",
                             bMobSub.CustNum,
                             bMobSub.MsSeq,
                             icMSISDN,
                             "", /*host table*/
                             "", /*key value*/
                             ideAmount,
                             0, /* percentage  */
                             ?, /* VAT included */
                             INT(lcPeriod), /*from period*/
                             999999, /*to period*/
                             lcMemoText).*/
   RETURN "".
END.



/*Function makes setting for each op data entry.
  If some of the settings fail already in setting phase
  notification to WEB is sent immediately.
  Program tries to set all settings even some setting fails.
  If all requests are created sccessfully the notification is sent
  after requests are done (by different program).
  NOTE IMPORTANT:
  Migration projectais paused(?) according to customer sttrategy decision.
  Implementation is ready only for barrings and upsells that were clear 
  in specifiaction phase Other data types have also functions for handling
  but they are not used..

*/
FUNCTION fSetOperData RETURNS CHAR
   (icMSISDN AS CHAR,
    iiMsSeq AS INT,
    icBarrings AS CHAR,     /*   Barr1=1,Barr2=1,...*/
    icSingleFees AS CHAR, /*     Fee1=30,Fee2=22,...*/
    icUpsells AS CHAR,   /*      Upsell1,Upsell2,...*/
    iiUsedData AS INT,  /*       used data amount*/
    icTerminals AS CHAR,     /*  imei1,imai2*/
    icDiscounts AS CHAR,    /*TODO define*/
    icBonos AS CHAR,       /*    bono1,bpono2*/
    icPrepaidSaldo AS DECIMAL, /*24,5*/
    idFat AS DECIMAL):            /* 33*/
   DEF VAR lcStat AS CHAR NO-UNDO.
   DEF VAR lcRet AS CHAR NO-UNDO.
   DEF VAR lcCommands AS CHAR NO-UNDO. /*For logging*/
   DEF VAR liOrderID AS INT NO-UNDO.
   DEF VAR lcMQMessage AS CHAR NO-UNDO. /*Message to WEB in error cases*/

   lcCommands = STRING(iiMsSeq) + "|" + icMSISDN + "|".

   lcStat =  fSetMigrationBarring(iiMsSeq, icBarrings, TRUE). 
   lcCommands = lcCommands + icBarrings + "|".
   IF lcStat NE "" THEN DO:
      lcRet = "Barring error: " + lcStat + "/".
      lcStat = "".
   END.
/*
   lcStat = fSetMigrationFees(iiMsSeq, icSinglefees).
   lcCommands = lcCommands + icSingleFees + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Fee setting error: " + lcStat + "/".
      lcStat = "".
   END.
*/
   lcStat = fSetMigrationUpsells(iiMsSeq, icUpsells).
   lcCommands = lcCommands + icUpsells + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Upsell setting error: " + lcStat + "/".
      lcStat = "".
   END.
/*
   lcStat = fSetMigrationUsedData(iiMsSeq,iiUsedData).
   lcCommands = lcCommands + STRING(iiUsedData) + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Data update error: " + lcStat + "/".
      lcStat = "".
   END.


   lcStat = fSetMigrationTerminals(iiMsSeq,
                                            liOrderID,
                                            icTerminals).
   lcCommands = lcCommands + icTerminals + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Terminal setting error: " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = fSetMigrationDiscounts(iiMsSeq,icDiscounts).
   lcCommands = lcCommands + icDiscounts + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Discount setting error " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = fSetMigrationBonos(iiMsSeq,icBonos).
   lcCommands = lcCommands + icBonos + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Bundle setting error " + lcStat + "/".
      lcStat = "".
   END.
 To be added when prepaid support is implemented
   lcCommands = lcCommands + xxx + "|".
   lcStat = lcStat + fSetPrepaidSaldo(iiMsSeq,).
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Prepaid setting error " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = fSetMigrationFat(iiMsSeq,idFat).
   lcCommands = lcCommands + STRING(idFat) + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "FAT setting error " + lcStat + "/".
      lcStat = "".
   END.
*/
   PUT STREAM sLog UNFORMATTED lcCommands + lcRet SKIP.
   
   /*In failure cases WEB must know status as soon as possible*/
   IF lcRet NE "" THEN DO:
      FIND FIRST Order NO-LOCK WHERE
                 Order.CLI EQ icMSISDN AND
                 Order.Orderchannel BEGINS "migration"
                 NO-ERROR.
      IF AVAIL Order THEN liOrderID = Order.Orderid.
      ELSE liOrderID = 0.

      lcMQMessage = fGenerateOrderInfo(liOrderID,
                                       icMSISDN,
                                       lcStat).
      IF lcMQMessage EQ "" THEN
         lcCommands = lcCommands + ";Message creation failed".
      ELSE DO:
         lcCommands = ";" + fWriteToMQ(lcMQMessage).
      END.
      PUT STREAM sLog UNFORMATTED lcCommands SKIP.
      PUT STREAM sLog UNFORMATTED lcMQMessage SKIP.

   END.

   RETURN "".

END.

/*Todo: Move rest functionality from fSetOperData here if the feature 
  is taken into use.*/
PROCEDURE pReadInputJSON:
   DEF INPUT PARAMETER icFile AS CHAR.
   DEF INPUT PARAMETER ilgSimulate AS LOGICAL.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR lcMMOrderID AS CHAR NO-UNDO. 
   DEF VAR lcBarringList AS CHAR NO-UNDO.
   DEF VAR lcServiceList AS CHAR NO-UNDO.
   DEF VAR lcBono AS CHAR NO-UNDO.
   DEF VAR lcUpsell AS CHAR NO-UNDO.
   DEF VAR liDataAmount AS INT NO-UNDO.
   DEF VAR objJsonToTT AS CLASS Class.JsonToTT.
   DEF VAR lcBCommand AS CHAR NO-UNDO.
   DEF VAR liMsSeq AS INT NO-UNDO.
   DEF VAR lcCommands AS CHAR NO-UNDO.
   DEF VAR lcStat AS CHAR NO-UNDO.
   DEF VAR lcRet AS CHAR NO-UNDO.
   DEF VAR liOrderID AS INT NO-UNDO.
   DEF VAR lcMQMessage AS CHAR NO-UNDO.
   DEF VAR lcMode AS CHAR NO-UNDO INIT "Set values".

   IF ilgSimulate EQ TRUE THEN lcMode = "Simulation".

   PUT STREAM sLog UNFORMATTED
      "List collection starts " + fTS2HMS(fMakeTS()) + 
      "Mode: " + lcMode SKIP.


   objJsonToTT = NEW Class.JsonToTT().

   objJsonToTT:mStoreTT(BUFFER ttRootLevel:HANDLE).

   objJsonToTT:mStoreTT("barrings", BUFFER ttBarrings:HANDLE).
   objJsonToTT:mStoreTT("single_fees", BUFFER ttSingleFees:HANDLE).
   objJsonToTT:mStoreTT("upsells", BUFFER ttUpsells:HANDLE).
   objJsonToTT:mStoreTT("discounts", BUFFER ttDiscounts:HANDLE).


   objJsonToTT:mParseJsonFile(icFile).
   DO WHILE objJsonToTT:mGetNext():
      lcRet = "".
      FOR EACH ttRootLevel: 
         lcMSISDN = ttRootLevel.msisdn. 
         limsseq = 0.
         FIND FIRST mobsub NO-LOCK WHERE
                    mobsub.brand = gcBrand AND
                    mobsub.cli = lcMSISDN NO-ERROR.
         IF AVAIL mobsub then do:
            liMsSeq = mobsub.msseq.
         END.
      END.
      lcCommands = STRING(liMsSeq) + "|" + lcMSISDN + "|".

      FOR EACH ttBarrings: 
        if lcBCommand NE "" THEN lcBCommand = lcBCommand + ",".
               lcBCommand = lcBCommand + barring_id + "=1".
      END.
      lcStat =  fSetMigrationBarring(liMsSeq, lcBCommand, ilgSimulate).
      lcCommands = lcCommands + lcBCommand + "|".
      IF lcStat NE "" THEN DO:
         lcRet = lcRet + "Barring error: " + lcStat + "/".
         lcStat = "".
      END.

      FOR EACH ttUpsells: 
         lcStat = fSetMigrationUpsell(liMsSeq, 
                                      ttUpsells.upsell_id, 
                                      ttUpsells.amount,
                                      ilgSimulate).

          lcCommands = lcCommands + " " + ttUpsells.upsell_id.                            
          IF lcStat NE "" THEN DO:
             lcRet = lcRet + "Upsell error: " + lcStat + "/".
             lcStat = "".
         END.
         lcCommands = lcCommands + "|".
      END.
      FOR EACH ttDiscounts: 
         /*DISP ttDiscounts. */
      END.
      FOR EACH ttSinglefees:
         /*DISP ttSinglefees.*/
      END.

      PUT STREAM sLog UNFORMATTED lcCommands + lcRet SKIP.

      /*In failure cases WEB must know status as soon as possible*/
      IF lcRet NE "" THEN DO:
         FIND FIRST Order NO-LOCK WHERE
                    Order.CLI EQ lcMSISDN AND
                    Order.Orderchannel BEGINS "migration"
                    NO-ERROR.
         IF AVAIL Order THEN liOrderID = Order.Orderid.
         ELSE liOrderID = 0.

         lcMQMessage = fGenerateOrderInfo(liOrderID,
                                       lcMSISDN,
                                       lcRet).
         IF lcMQMessage EQ "" THEN
            lcCommands = lcCommands + ";Message creation failed".
         ELSE DO:
            lcCommands = ";" + fWriteToMQ(lcMQMessage).
         END.
         PUT STREAM sLog UNFORMATTED lcCommands SKIP.
         PUT STREAM sLog UNFORMATTED lcMQMessage SKIP.

      END.

   END.

   PUT STREAM sLog UNFORMATTED
      "Collection done " + fTS2HMS(fMakeTS()) SKIP.
   RETURN "".   
END.


