/*--------------------------------------------------------------------
  MODULE .......: YCO-276_upsell_update_monthly_3gb_12m.p
  TASK .........: Activating 3gb upsell for 12 months - FID3GB_12m_R_Upsell
  APPLICATION ..: TMS
  AUTHOR .......: Diego Pastrana
  CREATED ......: 27/04/2018
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/tmsconst.i}
{Syst/commpaa.i}
{Func/upsellbundle.i}

/*Logic:*
   1. Make a lock that prevents multiple instances of the program. 
   2. Collect all subscription with a 1gb upsell activated with the bobtool.
   3. Activate upsell for the mentioned subscription & write log.
   4. Free lock taken in step 1.
*/


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
DEF VAR lcclitype           AS CHAR NO-UNDO. /* YCO-457 */
DEF VAR lcBundleCLITypes    AS CHAR NO-UNDO. /* YCO-457 */

/* List of valid tariffs for this upsell */
DEF VAR cValidList AS CHAR INITIAL
   "CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTFH39_50,CONTFH49_300,CONTFH69_1000,CONTFH48_50,CONTFH58_300,CONTFH76_1000,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONT34,CONT15,CONT33,CONT25,CONTFH35_50,CONTFH45_300,CONTFH65_1000,CONTDSL2G,CONTDSL3G,CONTDSL7G,CONTDSL39,CONTDSL48,CONTDSL59,CONTDSL99".

/* YCO-457 - Adding legacy tariffs to the list so they are also renewed */
cValidList = cValidList + ",CONT6,CONT7,CONT8,CONT9,CONTF11,CONTF20D,CONTF30,CONTF40,CONTF55,CONTF8,CONTM,CONTM2,CONT23,CONT24,CONTS12,CONTS15,CONTS16,CONTS20,CONTS21,CONTS25,CONTS26,CONTS30,CONTS32,CONT28,CONT27,CONT31,CONTRD1,CONTRD2,CONTRD3,CONTRD4,CONTRD9,CONT10,CONT26,CONT4,CONT5,CONTDSL45,CONTSF10,CONTFF,CONTFH45_50,CONTSF14,CONTFHNB7G_300,CONTFH55_300,CONTFHNB3G_300,CONTFHNB2G_300,CONTFHNB58_300,CONTFH52_50,CONTFH62_300,CONTFH82_1000".


/* Temp table for orders of activated mobsubs during the collection period */
DEF TEMP-TABLE ttMobSubList NO-UNDO
   FIELD cli     AS CHAR FORMAT "X(14)"
   FIELD MsSeq   AS INT
   INDEX idxcli cli.

DEF STREAM sLogFile.

ASSIGN 
   llgSimulation   = FALSE                                     /* TRUE -> only log writing, FALSE -> make real updates */
   lcActionId      = "FID3GB_12m_R_UPSELL"                     /* For execution lock                                   */
   lcTableName     = "FID3GB_12m-Promo"                        /* For execution lock                                   */
   ldCurrentTimeTS = Func.Common:mMakeTS()
   lcUpsell        = "FID3GB_12m_R_UPSELL"                     /* Upsells that will be added in the promo              */
   ldCampaignStart = fCParamDe("YCO-276-FID3GB_12m-FromDate")  /* Promotion start date                                 */
   ldCampaignEnd   = fCParamDe("YCO-276-FID3GB_12m-ToDate")    /* Promotion end date                                   */
   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").       /* YCO-457 */

ASSIGN 
   ldaReadDate  = TODAY
   lcLogDir     = fCParam("YCO-276-Dir","YCO-276-FID3GB_Xm-Logs").
IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".

ASSIGN 
   lcLogFile    = lcLogDir + "FID3GB_12m_upsell_3Gb_update_" +
                             STRING(YEAR(ldaReadDate)) +
                             STRING(MONTH(ldaReadDate),"99") +
                             STRING(DAY(ldaReadDate),"99") +
                             REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".

/* *************************** FUNCTIONS ***************************** */
FUNCTION fIsValid RETURNS LOG
   (icCliType AS CHAR):

   IF LOOKUP(icCliType,cValidList) > 0 THEN
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
            MsRequest.ReqSource  EQ {&REQUEST_SOURCE_NEWTON}       AND
            MsRequest.ReqCparam3 EQ lcUpsell                       AND
            MsRequest.actstamp > ldCampaignStart                   AND
            MsRequest.actstamp < ldCampaignEnd
            USE-INDEX reqtype:
   
      lcErr = "".

      FIND FIRST Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq EQ MsRequest.MsSeq NO-ERROR.
      IF NOT AVAIL Mobsub THEN DO:
         lcErr = "No active mobsub " + STRING(MsRequest.MsSeq).
         PUT STREAM sLogFile UNFORMATTED lcErr SKIP.
         NEXT.
      END.

      /* YCO-457
         - some old legacy tariffs store the tariff in mobsub.tariffbundle rather than mobsub.clitype
         - The compatibility matrix have the specific tariffs rather than the "families"
           because not all members of a "family" are compatible. So I have to pass the
           tariffbundle that contains the specific tariff to the function in charge of the validation */
      IF LOOKUP(Mobsub.CliType,lcBundleCLITypes) > 0 THEN
          lcclitype = Mobsub.tariffbundle.
      ELSE 
          lcclitype = Mobsub.CliType.      

      IF fIsValid(lcclitype) NE TRUE THEN DO:
         lcErr = "No valid CLITYPE"      + "|" +
                 STRING(MobSub.cli)      + "|" +
                 STRING(lcclitype)       + "|" +
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
            MsRequest.Brand      EQ Syst.Var:gcBrand               AND    
            MsRequest.MsSeq      EQ iiMsSeq                        AND
            MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus  EQ {&REQUEST_STATUS_DONE}         AND
            MsRequest.ReqSource  EQ {&REQUEST_SOURCE_NEWTON}       AND   
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
              MsRequest.Brand      EQ Syst.Var:gcBrand               AND 
              MsRequest.MsSeq      EQ iiMsSeq                        AND
              MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
              MsRequest.ReqCparam3 EQ lcUpsell                       AND 
              MsRequest.ReqSource  EQ {&REQUEST_SOURCE_NEWTON}       AND
              MsRequest.crestamp > fMonthStart(Func.Common:mMakeTS()) /* do not care times done in eariler months */
              USE-INDEX MsSeq
              NO-ERROR.

   /* Do not allow more than 12 activations */
   liDoneActivations = fCountReq(iiMsSeq).
   IF liDoneActivations >= 12 THEN RETURN "Activation month count full".

   IF AVAIL MsRequest THEN RETURN "Upsell already activated".
   
   IF llgSimulation EQ FALSE THEN DO:
      fCreateUpsellBundle(iiMsSeq,
                          lcUpsell,
                          {&REQUEST_SOURCE_NEWTON},
                          Func.Common:mMakeTS(),
                          OUTPUT liRequest,
                          OUTPUT lcError).
      IF lcError NE "" THEN RETURN lcError.                    
   END.
   
   RETURN "OK".
END.

/* **************************** MAIN BLOCK *************************** */
OUTPUT STREAM sLogFile TO VALUE(lcLogFile) APPEND.

PUT STREAM sLogFile UNFORMATTED "FID3GB_12m 3Gb Activation starts " +
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
                 
   IF llgSimulation = FALSE then              
       PUT STREAM sLogFile UNFORMATTED lcoutRow SKIP.
   ELSE 
       PUT STREAM sLogFile UNFORMATTED lcoutRow + " - Simulation" SKIP.
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

PUT STREAM sLogFile UNFORMATTED "FID3GB_12m 3Gb Activation done " +
                                 Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP.
OUTPUT STREAM sLogFile CLOSE.

