/*--------------------------------------------------------------------
  MODULE .......: YCO-1_upsell_update_monthly.p
  TASK .........: Activating 5gb upsell for 12 months
  APPLICATION ..: TMS
  AUTHOR .......: Diego Pastrana
  CREATED ......: 14/02/2018
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/tmsconst.i}
{Syst/commpaa.i}
{Func/upsellbundle.i}

/*Logic:*
   1. Make a lock that prevents multiple instances of the program. 
   2. Collect all subscription with a 5gb upsell activated with the bobtool.      
   3. Activate upsell for the mentioned subscription & write log.
   4. Free lock taken in step 1.
*/

Syst.Var:gcBrand = "1".

DEF VAR llgSimulation       AS LOG  NO-UNDO.
DEF VAR lcActionId          AS CHAR NO-UNDO.
DEF VAR lcTableName         AS CHAR NO-UNDO.
DEF VAR ldCurrentTimeTS     AS DEC  NO-UNDO.
DEF VAR ldaReadDate         AS DATE NO-UNDO.
DEF VAR lcLogDir            AS CHAR NO-UNDO.
DEF VAR lcLogFile           AS CHAR NO-UNDO.
DEF VAR lcUpsell            AS CHAR NO-UNDO.
DEF VAR ldCollPeriodStartTS AS DEC  NO-UNDO.
DEF VAR ldCollPeriodEndTS   AS DEC  NO-UNDO. /* now - 1 minute */
DEF VAR lcResult            AS CHAR NO-UNDO.
DEF VAR lcOutRow            AS CHAR NO-UNDO.
DEF VAR ldCampaignStart     AS DEC  NO-UNDO.
DEF VAR ldCampaignEnd       AS DEC  NO-UNDO.

/* Temp table for orders of activated mobsubs during the collection period */
DEF TEMP-TABLE ttMobSubList NO-UNDO
   FIELD cli     AS CHAR FORMAT "X(14)"
   FIELD MsSeq   AS INT
   INDEX idxcli cli.

DEF STREAM sLogFile.

ASSIGN 
   llgSimulation   = FALSE                            /* TRUE -> only log writing, FALSE -> make real updates */
   lcActionId      = "UpsellFor5G"                    /* For execution lock   */
   lcTableName     = "YCO-1-Promo-5Gb"                /* For execution lock   */
   ldCurrentTimeTS = Func.Common:mMakeTS()
   lcUpsell        = "DATA5G_UPSELL"                  /* Upsells that will be aded in the promo */
   ldCampaignStart = fCParamDe("YCO-1-5G-FromDate")   /* Promotion start date */
   ldCampaignEnd   = fCParamDe("YCO-1-5G-ToDate").    /* Promotion start date */

ASSIGN 
   ldaReadDate  = TODAY
   lcLogDir     = fCParam("YCO-1-Promo","YCO-1-Dir").
IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".

ASSIGN 
   lcLogFile    = lcLogDir + "YCO-1_upsell_5Gb_update_" +
                             STRING(YEAR(ldaReadDate)) +
                             STRING(MONTH(ldaReadDate),"99") +
                             STRING(DAY(ldaReadDate),"99") +
                             REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".

/* *************************** FUNCTIONS ***************************** */
FUNCTION fIsValid RETURNS LOG
   (icCliType AS CHAR):
 
   IF icCliType EQ "CONT15" OR
      icCliType EQ "CONT25" OR
      icCliType EQ "CONT26" THEN
      RETURN TRUE.
 
   RETURN FALSE.
END.   

FUNCTION fCollect RETURNS CHAR
   (idStartTS AS DEC,
    idEndTS   AS DEC):
       
   DEF BUFFER Order          FOR Order.
   DEF BUFFER Mobsub         FOR Mobsub.
   DEF BUFFER ORdertimestamp FOR Ordertimestamp.
   
   DEF VAR lcErr AS CHAR NO-UNDO.
   
   /* Finding subscriptions where this upsell has been activated */
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand      EQ Syst.Var:gcBrand               AND  
            MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus  EQ {&REQUEST_STATUS_DONE}         AND
            MsRequest.ReqSource  EQ {&REQUEST_SOURCE_YOIGO_TOOL}   AND /* Created by BobTool */
            MsRequest.ReqCparam3 EQ lcUpsell                       AND
            MsRequest.crestamp > ldCampaignStart                   AND
            MsRequest.crestamp < ldCampaignEnd:
   
      lcErr = "".

      FIND FIRST Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq EQ MsRequest.MsSeq NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         lcErr = "No active mobsub " + STRING(MsRequest.MsSeq).
         PUT STREAM sLogFile UNFORMATTED lcErr SKIP.
         NEXT.
      END.

      IF fIsValid(Mobsub.CliType) NE TRUE THEN DO:
         lcErr = "No valid CLITYPE"      + "|" +
                 STRING(MobSub.cli)      + "|" +
                 STRING(MobSub.clitype)  + "|" +
                 STRING(MSRequest.MsSeq).
         PUT STREAM sLogFile UNFORMATTED lcErr SKIP.
         NEXT.
      END.
 
      FIND FIRST ttMobSubList WHERE
                 ttMobSubList.cli = mobsub.cli NO-LOCK NO-ERROR.
      IF AVAIL ttMobSubList THEN NEXT. /* Skip duplicates */

      /* After all validations: this subscription needs upsell */
      CREATE ttMobSubList.
      ASSIGN
         ttMobSubList.CLi   = MobSub.cli 
         ttMobSubList.MsSeq = MsRequest.MsSeq.
   END.
END.

/*Function returns start moment of given month, example
  20170322.15 -> 20170301*/
FUNCTION fMonthStart RETURNS DECIMAL
   (idIn AS DECIMAL):
      
   DEF VAR liOut AS INT NO-UNDO.
   liOut = idIn / 100 .
   liOut = liOut * 100 + 1.

   RETURN DECIMAL(liOut).
END.

/* Function counts successful activations */
FUNCTION fCountReq RETURNS INT
   (iiMsSeq AS INT):

   DEF VAR liC AS INT NO-UNDO.
   
   DEF BUFFER MsRequest FOR MsRequest.
   
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.MsSeq      EQ iiMsSeq                        AND
            MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus  EQ {&REQUEST_STATUS_DONE}         AND
            MsRequest.ReqSource  EQ {&REQUEST_SOURCE_YOIGO_TOOL}   AND /* Created by BobTool */
            MsRequest.ReqCparam3 EQ lcUpsell                       AND
            MsRequest.crestamp > ldCampaignStart
            USE-INDEX MsSeq:
      liC = liC + 1.
   END.
   
   RETURN liC.
END.

/* Function activates upsell for subscription that is related to given order */
FUNCTION fUpsellForYCO-1 RETURNS CHAR
   (iiMsSeq AS INT):
   
   DEF VAR lcError           AS CHAR NO-UNDO.
   DEF VAR liRequest         AS INT NO-UNDO.
   DEF VAR liDoneActivations AS INT NO-UNDO.
   
   DEF BUFFER MsRequest FOR MsRequest.

   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.MsSeq      EQ iiMsSeq                        AND
              MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
              MsRequest.ReqCparam3 EQ lcUpsell                       AND 
              MsRequest.crestamp > fMonthStart(Func.Common:mMakeTS()) /* do not care times done in eariler months */
              NO-ERROR.

   /* Do not allow more than 12 activations */
   liDoneActivations = fCountReq(iiMsSeq).
   IF liDoneActivations >= 12 THEN RETURN "Activation month count full".

   IF AVAIL MsRequest THEN RETURN "Upsell already activated".
   
   IF llgSimulation EQ FALSE THEN DO:
      fCreateUpsellBundle(iiMsSeq,
                          lcUpsell,
                          {&REQUEST_SOURCE_YOIGO_TOOL},
                          Func.Common:mMakeTS(),
                          OUTPUT liRequest,
                          OUTPUT lcError).
      IF lcError NE "" THEN RETURN lcError.                    
   END.
   
   RETURN "OK".
END.

/* **************************** MAIN BLOCK *************************** */
OUTPUT STREAM sLogFile TO VALUE(lcLogFile) APPEND.

PUT STREAM sLogFile UNFORMATTED "YCO-1 5Gb Activation starts " +
                                 Func.Common:mTS2HMS(ldCurrentTimeTS) SKIP.
IF llgSimulation EQ TRUE THEN
   PUT STREAM sLogFile UNFORMATTED "Simulation mode" SKIP.

/* Set execution lock on */
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand AND
              ActionLog.ActionID  EQ  lcActionID       AND
              ActionLog.TableName EQ  lcTableName      NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /* First execution stamp */
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCampaignStart.
      RELEASE ActionLog.
      RETURN. /* No reporting in first time */
   END.
   ELSE DO:
      ASSIGN
         /* Store previous starting time before setting new value to db */
         ldCollPeriodStartTS    = ActionLog.ActionTS

         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.

      RELEASE Actionlog.
   END.
END.

/* Actual execution */
ldCollPeriodEndTS = Func.Common:mSecOffSet(ldCampaignStart, - 60). /* Now - 1 minute */

/* Select orders that need activation, additional 30 days is done to be sure that also delayed activations are found */
fCollect (ldCampaignStart, (ldCampaignEnd + 30)). 

/* Activate upsells */
FOR EACH ttMobSubList:
   ASSIGN 
      lcOutRow = ""
      
      lcResult = fUpsellForYCO-1 (ttMobSubList.MsSeq)
      
      lcOutRow = STRING(ttMobSubList.cli) + "|" +
                 STRING(ttMobSubList.MsSeq)   + "|" +
                 lcResult.
                 
   PUT STREAM sLogFile UNFORMATTED lcoutRow SKIP.
END.

/*Release execution lock*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand        EQ  Syst.Var:gcBrand AND
              ActionLog.ActionID     EQ  lcActionID       AND
              ActionLog.TableName    EQ  lcTableName      AND
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN 
      ASSIGN 
         ActionLog.ActionTS     = ldCollPeriodEndTS    /*Start next collection from here*/
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.

   RELEASE ActionLog.
END.

PUT STREAM sLogFile UNFORMATTED "YCO-1 5Gb Activation done " +
                                 Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.
OUTPUT STREAM sLogFile CLOSE.

