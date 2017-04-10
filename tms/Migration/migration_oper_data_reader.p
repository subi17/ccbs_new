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
IF lcInDir EQ "" OR lcInDir EQ ? THEN lcInDir = "/tmp/".

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
lcErr = fInitMigrationMQ("response").
IF lcErr NE "" THEN DO:
   PUT STREAM sLog UNFORMATTED
   "MQ error. Operational data file will be skipped: " + lcErr +
      fTS2HMS(fMakeTS()) SKIP.

END.
ELSE DO:
   /*Execution part*/
   RUN pHandleInputJson. 
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
              DayCampaign.Brand EQ "1" AND
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

/*Function generates a barring command from separated list format:
  barring1,barring2,barring3 ->
  barring1=1,barring2=1,barring3=1
  The program clears list so that already active barrings are not set to
  list to be activated again. (Idea: reduce errors by incoorect input)
  List handling creates log for problem solving purposes.
  The program does not make validations for barring command compability etc.
  The checks are done when calling actual barring functions.  
  */
 /*TODO: changes this when incoming data format is clear*/
FUNCTION fMMInputToBarrCmd RETURNS CHAR
   (iiMsSeq AS INT,
    icInput AS CHAR,
    icInputSeparator AS CHAR):
   DEF VAR i AS INT NO-UNDO.
   DEF VAR lcCommand AS CHAR NO-UNDO.

   PUT STREAM sLog UNFORMATTED
      "Generating Barring Command, input: "
      STRING(iiMsSeq) + " " +
      icInput "." SKIP.

   IF icInput EQ "" THEN DO:
      PUT STREAM sLog UNFORMATTED
         "Empty barring list " SKIP.
      RETURN icInput.
   END.
   DO i = 1 TO NUM-ENTRIES(icInput,icInputSeparator):
      IF fGetBarringStatus(ENTRY(i,icInput,icInputSeparator), iiMsSeq) EQ
         {&BARR_STATUS_ACTIVE} THEN DO:
         PUT STREAM sLog UNFORMATTED
            "Already active, not added to command: "
            ENTRY(i,icInput,icInputSeparator) " " SKIP.
      END.
      ELSE DO:
         lcCommand = lcCommand + ENTRY(i,icInput,icInputSeparator) + "=1".
         IF i NE NUM-ENTRIES(icInput,icInputSeparator) THEN
            lcCommand = lcCommand + ",".
      END.
   END.
   PUT STREAM sLog UNFORMATTED
      "Generated Barring Command: "
       STRING(iiMsSeq) + " " +
       lcCommand "." SKIP.

   RETURN lcCommand.
END.

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
    icCommand AS CHAR):
   DEF VAR lcStat AS CHAR NO-UNDO.
   DEF VAR liReq AS INT NO-UNDO.
   
   lcStat = fValidateMMBarringCommand(iiMsSeq, icCommand).
   IF lcStat NE "" THEN RETURN lcStat.
   IF icCommand EQ "" THEN RETURN "".
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
    DEF VAR lcStat AS CHAR NO-UNDO.
   RETURN "".
END.

/*Function sets migration related upsells*/
/*Note: current version suppots only postpaid upsells*/
FUNCTION fSetMigrationUpsells RETURNS CHAR
   (iiMsSeq AS INT,
    icCommand AS CHAR):
    DEF VAR lcStat AS CHAR NO-UNDO.
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
   DEF VAR lcStat AS CHAR NO-UNDO.
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
    DEF VAR lcStat AS CHAR NO-UNDO.
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
    DEF VAR lcStat AS CHAR NO-UNDO.
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
    DEF VAR lcStat AS CHAR NO-UNDO.
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

   lcStat =  fSetMigrationBarring(iiMsSeq, icBarrings). 
   lcCommands = lcCommands + icBarrings + "|".
   IF lcStat NE "" THEN DO:
      lcRet = "Barring error: " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = lcStat + fSetMigrationFees(iiMsSeq, icSinglefees).
   lcCommands = lcCommands + icSingleFees + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Fee setting error: " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = lcStat + fSetMigrationUpsells(iiMsSeq, icUpsells).
   lcCommands = lcCommands + icUpsells + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Upsell setting error: " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = lcStat + fSetMigrationUsedData(iiMsSeq,iiUsedData).
   lcCommands = lcCommands + STRING(iiUsedData) + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Data update error: " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = lcStat + fSetMigrationTerminals(iiMsSeq,
                                            liOrderID,
                                            icTerminals).
   lcCommands = lcCommands + icTerminals + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Terminal setting error: " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = lcStat + fSetMigrationDiscounts(iiMsSeq,icDiscounts).
   lcCommands = lcCommands + icDiscounts + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Discount setting error " + lcStat + "/".
      lcStat = "".
   END.

   lcStat = lcStat + fSetMigrationBonos(iiMsSeq,icBonos).
   lcCommands = lcCommands + icBonos + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Bundle setting error " + lcStat + "/".
      lcStat = "".
   END.
/* To be added when prepaid support is implemented
   lcCommands = lcCommands + xxx + "|".
   lcStat = lcStat + fSetPrepaidSaldo(iiMsSeq,).
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "Prepaid setting error " + lcStat + "/".
      lcStat = "".
   END.
*/
   lcStat = lcStat + fSetMigrationFat(iiMsSeq,idFat).
   lcCommands = lcCommands + STRING(idFat) + "|".
   IF lcStat NE "" THEN DO:
      lcRet = lcRet + "FAT setting error " + lcStat + "/".
      lcStat = "".
   END.

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



/*File reading and data processing for each filed.  
  MSISDN (Single value)
  Barrings (list):
     List of barrings will be set in a command.
     Barring list handling filters out already active barrings and
     makes also other validations.
  Services (list)
     Services are set one by one.
  Bono (single value)     
  Upsell (Single value)
  Data (Single value)
  */
PROCEDURE pHandleInputFile:
   DEF VAR lcLine AS CHAR NO-UNDO.
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR liLineNumber AS INT NO-UNDO.
   DEF VAR lcMMOrderID AS CHAR NO-UNDO. 
   DEF VAR lcBarringList AS CHAR NO-UNDO.
   DEF VAR lcServiceList AS CHAR NO-UNDO.
   DEF VAR lcBono AS CHAR NO-UNDO.
   DEF VAR lcUpsell AS CHAR NO-UNDO.
   DEF VAR liDataAmount AS INT NO-UNDO.
   PUT STREAM sLog UNFORMATTED
      "List collection starts " + fTS2HMS(fMakeTS()) SKIP.
   FILE_LINE:
   REPEAT TRANS:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.
      liLineNumber = liLineNumber + 1.

      IF NOT SESSION:BATCH AND liLineNumber MOD 10 = 0 THEN DO:
         disp "Reading data: " lcFilename liLineNumber with frame a.
         pause 0.
      END.
      IF NUM-ENTRIES (lcLine) NE 3 THEN DO:
         PUT STREAM sLog UNFORMATTED
         lcLine + ";" + "ERROR:Incorrect input format"  SKIP.
         NEXT.

      END.
      assign
         lcMMOrderID = STRING(ENTRY(1,lcline,";"))
         lcMSISDN = STRING(ENTRY(2,lcline,";"))
         lcBarringList = STRING(ENTRY(3,lcline,";"))
         lcServiceList = STRING(ENTRY(4,lcline,";"))
         lcBono = STRING(ENTRY(5,lcline,";"))
         lcUpsell = STRING(ENTRY(6,lcline,";"))
         liDataAmount = INT(ENTRY(7,lcline,";")).

   END. /* Line handling END */

   PUT STREAM sLog UNFORMATTED
      "Read " + STRING(liLineNumber) + " lines. " 
      "Collection done " + fTS2HMS(fMakeTS()) SKIP.
END.

PROCEDURE pReadInputJSON:
   DEF VAR lcMSISDN AS CHAR NO-UNDO.
   DEF VAR lcFileName AS CHAR NO-UNDO.
   DEF VAR lcMMOrderID AS CHAR NO-UNDO. 
   DEF VAR lcBarringList AS CHAR NO-UNDO.
   DEF VAR lcServiceList AS CHAR NO-UNDO.
   DEF VAR lcBono AS CHAR NO-UNDO.
   DEF VAR lcUpsell AS CHAR NO-UNDO.
   DEF VAR liDataAmount AS INT NO-UNDO.
   DEF VAR objJsonToTT AS CLASS Class.JsonToTT.

   PUT STREAM sLog UNFORMATTED
      "List collection starts " + fTS2HMS(fMakeTS()) SKIP.


   objJsonToTT = NEW Class.JsonToTT().

   objJsonToTT:mStoreTT(BUFFER ttRootLevel:HANDLE).

   objJsonToTT:mStoreTT("barrings", BUFFER ttBarrings:HANDLE).
   objJsonToTT:mStoreTT("single_fees", BUFFER ttSingleFees:HANDLE).
   objJsonToTT:mStoreTT("upsells", BUFFER ttUpsells:HANDLE).
   objJsonToTT:mStoreTT("discounts", BUFFER ttDiscounts:HANDLE).

   objJsonToTT:mParseJsonFile("/tmp/ilkka.json").
   DO WHILE objJsonToTT:mGetNext():
      FOR EACH ttRootLevel: DISP ttRootLevel. END.
      FOR EACH ttBarrings: DISP ttBarrings. END.
      FOR EACH ttSinglefees: DISP ttSinglefees. END.
      FOR EACH ttUpsells: DISP ttUpsells. END.
      FOR EACH ttDiscounts: DISP ttDiscounts. END.

      MESSAGE objJsonToTT:mListJsonArrayNames() 
         VIEW-AS ALERT-BOX TITLE "Arrays".
      MESSAGE objJsonToTT:mListJsonObjectNames() 
         VIEW-AS ALERT-BOX TITLE "Objects".
   END.



   PUT STREAM sLog UNFORMATTED
      "Collection done " + fTS2HMS(fMakeTS()) SKIP.
END.


