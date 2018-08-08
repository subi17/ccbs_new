/* ----------------------------------------------------------------------
  PROGRAM ..: setSAPC.P
  TASK .....: Construct ProCommand message
  AUTHOR ...: DP
  CREATED ..: 06-07-2018
  CHANGED ..:                    
  VERSION ..:
  NOTES.....:
   - Program is based on setms.p
   - Shapedlimit and unshapedlimit values are not needed anymore, values will 
     be configured to SAPC. Therefore, this program does not retrieve them.
   - At 18/07/18 program is ready to manage DATAPLAN and UPSELL request. These 
     have MsRequest.ReqCParam1 = "SHAPER".
     However, the code has commented some other parts in charge of some other 
     requests that will be adjusted when necessary
   - New dataplans are needed for Bonos, because with SPAC they will have their 
     own dataplan instead of using same with tariff data plan
  ---------------------------------------------------------------------- */
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

{Func/cparam2.i}
{Func/sharperconfid.i}
{Mm/active_bundle.i}
{Syst/tmsconst.i}
{Func/SAPC_change_API.i}

/*
{Mm/barrgrp.i}
{Gwy/provision.i}
{Mm/ongoing_bundle.i}
*/

DEF INPUT  PARAMETER iiMsRequest LIKE MSRequest.msrequest NO-UNDO.
DEF OUTPUT PARAMETER oiReqCnt    AS INTEGER               NO-UNDO.
DEF OUTPUT PARAMETER ocError     AS CHAR                  NO-UNDO.
 
/* Values for oiReqCnt: 
    -1: error occurred 
    0: no need to create PROCOMMAND record 
    1: solog created 
*/

/* Local variables */
DEF VAR liCurrentServiceClass AS INT   NO-UNDO.
DEF VAR lcError               AS CHAR  NO-UNDO. /* Error in invoked procedure */
DEF VAR lcPrepaidVoiceTariffs AS CHAR  NO-UNDO.
DEF VAR lcBBProfile1          AS CHAR  NO-UNDO.
DEF VAR lcBBProfile2          AS CHAR  NO-UNDO.
DEF VAR liCount               AS INT   NO-UNDO.  /* Used for looping */ 
DEF VAR lcProcommandType      AS CHAR  NO-UNDO.
DEF VAR ldCurrent             AS DEC   NO-UNDO.  /* Actual date and time */ 
DEF VAR lcParam               AS CHAR  NO-UNDO. 
DEF VAR lcBaseBundle          AS CHAR  NO-UNDO.  
DEF VAR cJsonMsg              AS LONGCHAR    NO-UNDO.
DEF VAR lJsonCreation         AS LOGICAL     NO-UNDO.
DEF VAR ldaActiveDate         AS DATE        NO-UNDO.
DEF VAR liActiveTime          AS INT         NO-UNDO.
DEF VAR llVoIPActive          AS LOG         NO-UNDO.
DEF VAR lcAction              AS CHAR        NO-UNDO.               
DEF VAR lcUpsellSize          AS CHAR        NO-UNDO.
DEF VAR lcEmaDataPlan         AS CHAR        NO-UNDO.
DEFINE VARIABLE loProCommand  AS CLASS Gwy.ProCommand NO-UNDO.
DEFINE VARIABLE loCommandLine  AS CLASS JsonObject NO-UNDO.


DEF VAR lcShaperConf_Tariff_Type AS CHAR NO-UNDO.
DEF VAR lcShaperConf_Tariff      AS CHAR NO-UNDO.
DEF VAR lcShaperConf_Template    AS CHAR NO-UNDO.
DEF VAR lcShaperConf_Reset_Day   AS CHAR NO-UNDO.
DEF VAR lcShaperConfId           AS CHAR NO-UNDO.
 
DEF BUFFER bMsRequest   FOR MSRequest.
DEF BUFFER bCLIType     FOR CLIType.

/*
DEF VAR ldActStamp    AS DEC                        NO-UNDO. 
DEF VAR ldOrder       AS DEC                        NO-UNDO. 
DEF VAR llSkip        AS LOG                        NO-UNDO. 
DEF VAR lcActiveBundles AS CHAR                     NO-UNDO.
DEF VAR lcBundle      AS CHAR                       NO-UNDO.
DEF VAR liNumEntries  AS INT                        NO-UNDO.
DEF VAR lcServiceClass AS CHAR                      NO-UNDO.
DEF VAR lcShaperProfile        AS CHAR              NO-UNDO.
DEF VAR llCheckSC AS LOG NO-UNDO INIT TRUE.
*/

/* Temp-tables: defined in Func/SAPC_Change_API.i */

/* Remains in case is needed for a DATASET DS_Upsell:WRITE-JSON statement
DEFINE DATASET DS_upsell FOR ttOrder, ttOutService, ttInService, ttCharacteristic, ttLine
   DATA-RELATION r1 FOR ttOrder, ttOutService NESTED FOREIGN-KEY-HIDDEN 
      RELATION-FIELDS (orderid, orderid) 
   DATA-RELATION r2 FOR ttOutService, ttInService NESTED FOREIGN-KEY-HIDDEN  
      RELATION-FIELDS (orderid, orderid, type, outtype, serviceId, outserviceId) 
   DATA-RELATION r3 FOR ttInService, ttCharacteristic NESTED FOREIGN-KEY-HIDDEN 
      RELATION-FIELDS (orderid, orderid, type, type, servicename, servicename)
   DATA-RELATION r4 FOR ttOutService, ttline NESTED FOREIGN-KEY-HIDDEN 
      RELATION-FIELDS (orderid, orderid, type, type, serviceId, serviceId).
*/   


/* **************************  Function Implementations ********************* */

FUNCTION fDateTime-string RETURNS CHARACTER 
   (INPUT id-datetime AS DEC):
/*------------------------------------------------------------------------------
 Purpose: Return a string made of the conversion of a decimal notation of a
          date and time into a string
 Notes:   Used to set the value into DATETIME-TZ fields
------------------------------------------------------------------------------*/ 
   DEF VAR lcDateTime AS CHAR NO-UNDO.
   DEF VAR lcOrigData AS CHAR FORMAT "99999999.99999" NO-UNDO.
   DEF VAR lcOrigDate AS DATE NO-UNDO.
   DEF VAR lcOrigTime AS INT  NO-UNDO.
   
   ASSIGN 
      lcOrigData = STRING(id-datetime,"99999999.99999")
      lcOrigDate = DATE(INT(SUBSTRING(lcOrigData,5,2)),  /* Month */
                        INT(SUBSTRING(lcOrigData,7,2)),  /* Day   */
                        INT(SUBSTRING(lcOrigData,1,4)))  /* Year  */
      lcOrigTime = INT(SUBSTRING(lcOrigData,11,5))
      lcDateTime = STRING(lcOrigDate,"99-99-9999")  
                   + " " + 
                   STRING(lcOrigTime,"HH:MM:SS").
                
   RETURN lcDateTime.

END FUNCTION.


/* ******************************** MAIN-BLOCK ****************************** */

/* Local variable initialization */
ASSIGN
   ldCurrent             = Func.Common:mMakeTS()
   lcPrepaidVoiceTariffs = fCParamC("PREPAID_VOICE_TARIFFS")
   lcBBProfile1          = fCParamC("BB_PROFILE_1")
   lcBBProfile2          = fCParamC("BB_PROFILE_2").

blk:
DO TRANSACTION ON ERROR UNDO blk, LEAVE blk
               ON STOP  UNDO blk, LEAVE blk:

   /* INITIALIZATION ******************************************************** */
   oiReqCnt = -1.

   /* CHECKS **************************************************************** */
   
   /* Request */    
   FIND FIRST MSrequest WHERE 
              MSRequest.MSrequest = iiMSrequest NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsRequest THEN
   DO:
      ocError = "ERROR: No valid request id " + STRING(iiMSrequest).
      RETURN ocError.
   END.          
    
   /* Subscription */
   FIND FIRST MobSub WHERE 
              MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MobSub THEN
   DO:
      ocError = "ERROR: No available subscription for msSeq " + STRING(MSrequest.MsSeq).
      RETURN ocError.
   END.          
             
   /* IMSI number Record (we get KI ) */
   FIND FIRST IMSI WHERE IMSI.ICC = MobSub.ICC NO-LOCK NO-ERROR.
   IF NOT AVAILABLE IMSI THEN
   DO:
      ocError = "ERROR: IMSI not found for ICC " + MobSub.ICC.
      RETURN ocError.
   END.
             
   /* SIM Card  (Here we get the Batch #) */
   FIND sim WHERE sim.ICC = imsi.ICC NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sim THEN
   DO:
      ocError = "ERROR: SIM not found for ICC " + imsi.ICC.
      RETURN ocError.
   END.

   /* Prepaid account information */
   IF MobSub.CLIType       = "TARJ5" AND 
      MsRequest.ReqCParam1 = "HSDPA" THEN
   DO:
      RUN Gwy/air_get_account_details.p(MobSub.CLI, 
                                        OUTPUT liCurrentServiceClass,
                                        OUTPUT lcError).
      IF lcError BEGINS "ERROR" THEN
         RETURN lcError.
   END.
   
   /* Service Component */   
   FIND ServCom WHERE
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = MsRequest.ReqCParam1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE servCom THEN
   DO:
      ocError = "ERROR: SIM not found for ICC " + imsi.ICC.
      RETURN ocError.
   END.

   /* Review when this happens !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   /* CF */
   IF MsRequest.ReqCparam1 = "CF" THEN
   DO:
      IF MsRequest.ReqCParam2 > "" THEN
      DO:
         IF LENGTH(MsRequest.ReqCParam2) NE 3 THEN
         DO:
            ocError = "ERROR: Incorrect number of CF parameters".
            RETURN ocError.
         END.

         DO liCount = 1 TO 3:
            IF     SUBSTRING(MsRequest.ReqCparam2,liCount,1) NE "0" 
               AND SUBSTRING(MsRequest.ReqCparam2,liCount,1) NE "1" 
               AND SUBSTRING(MsRequest.ReqCparam2,liCount,1) NE "2" THEN
            DO:
               ocError = "ERROR: Incorrect CF parameters".
               RETURN ocError.
            END.
         END.
      END.
      ELSE
      DO:
         IF MsRequest.ReqIParam1 NE 0 AND   /* OFF */
            MsRequest.ReqIParam1 NE 1 AND   /* CF  */
            MsRequest.ReqIParam1 NE 2 THEN  /* MCA */ 
         DO:
            ocError = "ERROR :Incorrect CF parameters".
            RETURN ocError.
         END.
      END.
   END. */
   IF MsRequest.ReqCparam1 = "CF" THEN
   DO:
      ocError = "ERROR: setSAPC program not ready yet to manage CF request ".
      RETURN ocError.
   END.
   ELSE IF MsRequest.ReqCparam1 = "NW" THEN
   DO:
      ocError = "ERROR: setSAPC program not ready yet to manage NW request ".
      RETURN ocError.
   END.
   ELSE
   IF MsRequest.ReqCparam1 <> "SHAPER" THEN
   DO:
      ocError = "ERROR: setSAPC program not ready yet to manage " + 
                 MsRequest.ReqCparam1 + " request ".
      RETURN ocError.
   END. 

                
   /* BUILDING PROCOMMAND *************************************************** */
  
   /* Checking if the request is for UPSELL or DATAPLAN */
   IF MsRequest.ReqCParam1 = "SHAPER" AND
      MsRequest.OrigRequest > 0 THEN 
   DO:
      FIND FIRST bMsRequest WHERE 
                 bMsRequest.MsRequest = MsRequest.OrigRequest
                 NO-LOCK NO-ERROR.
      IF AVAILABLE bMsRequest THEN 
      DO:
         IF bMsRequest.ReqCparam3 MATCHES "*_UPSELL" THEN
         DO:
            IF bMsRequest.ReqCparam3 BEGINS "DSS" THEN
            DO:
               ocError = "ERROR: setSAPC program not ready yet to manage " + 
                         MsRequest.ReqCparam1 + " request ".
              RETURN ocError.
               /* 
               ASSIGN 
                  lcProcommandType = "ADD_DSS_UPSELL".
               */
            END.
            ELSE 
               ASSIGN 
                  lcProcommandType = "ADD_UPSELL".
         END.
         /* Review when this happens !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            Probably it has to behave as an upsell !!!!!!!!!!!!!!!!!!!!!!!!  
         ELSE   
         IF (   bMsRequest.ReqType EQ 0 
             OR bMsRequest.ReqType EQ 81) AND
             INDEX(MsRequest.ReqCParam2,"HSPA") > 0 THEN
            ASSIGN 
               lcServName = "QUOTA". */
         
         ELSE /* Dataplan. Later, we decide the type: ADD/MOD/DEL */
            ASSIGN 
               lcProcommandType = "DATAPLAN".
      END.
      ELSE 
      DO:
         ocError = "ERROR: Unable to find original request " + 
                   STRING(MsRequest.OrigRequest).
         RETURN ocError.
      END.
   END.

   /* Review when this happens !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   IF MsRequest.ReqCparam1 = "CF" THEN
   DO:
      IF MsRequest.ReqCParam2 > "" THEN
      DO:
         DO liCount = 1 TO 3:
            lcServName = lcServName + ENTRY(liCount,"CFB,CFNRC,CFNRY").
            
            CASE SUBSTRING(MsRequest.ReqCparam2,liCount,1):
               WHEN "0" THEN lcServName = lcServName + "=0".
               WHEN "1" THEN lcServName = lcServName + "=34633633633".
               WHEN "2" THEN lcServName = lcServName + "=34633633556".
            END.
            
            IF liCount NE 3 THEN lcServName = lcServName + ",".
         END.
      END.
      ELSE
      DO:
         CASE MsRequest.ReqIParam1:
            WHEN 0 THEN /* OFF */
               lcServName = "CFB=0,CFNRC=0,CFNRY=0".
            WHEN 1 THEN /* CF */
               lcServName = "CFB=34633633633,CFNRC=34633633633,CFNRY=34633633633".
            WHEN 2 THEN /* MCA */
               lcServName = "CFB=34633633556,CFNRC=34633633556,CFNRY=0".
         END CASE.
      END.
   END.
   */
   
   /* No Procommand needed */
   IF ServCom.ActType <> 0 THEN
   DO:
      oiReqCnt = 0.
      RETURN.
   END.

   /* Generating command *********************************************** */
   CASE lcProcommandType:
      WHEN "ADD_UPSELL" OR WHEN "ADD_DSS_UPSELL" THEN 
         ASSIGN 
            lcAction      = "Add"
            lcUpsellSize  = ENTRY(1,MsRequest.reqcparam2)
            lcEmaDataPlan = bMsRequest.reqcparam3.
      WHEN "DATAPLAN" THEN 
      DO:
         IF AVAIL bMsRequest THEN
         DO:
            IF bMsRequest.ReqType = 8 THEN /* ADD - Per. contract activation */
               ASSIGN 
                  lcAction         = "Add"
                  lcProcommandType = "ADD_DATAPLAN"
                  lcEmaDataPlan    = bMsRequest.reqcparam3.
            ELSE 
            IF bMsRequest.ReqType = 9 THEN /* DELETE - Per. contract termination*/
               ASSIGN 
                  lcAction         = "Delete"
                  lcProcommandType = "DELETE_DATAPLAN"
                  lcEmaDataPlan    = bMsRequest.reqcparam3.
         END. /* Avail bMsRequest */

         IF lcProcommandType = "DATAPLAN" THEN 
         DO:
            ocError = "ERROR: setSAPC program not able to resolve ACTION ".
            RETURN ocError.
         END. 
         
         /* Code in setms.p, probably not all of it is necessary!!!!!!!!!!! */    
         FIND FIRST ShaperConf NO-LOCK WHERE
                    ShaperConf.Brand        = Syst.Var:gcBrand AND
                    ShaperConf.ShaperConfID = MsRequest.ReqCParam2
                    NO-ERROR.
         IF AVAIL ShaperConf THEN 
            ASSIGN 
               lcShaperConf_Tariff_Type    = ShaperConf.TariffType
               lcShaperConf_Tariff         = ShaperConf.Tariff
               lcShaperConf_Template       = ShaperConf.Template.
               
         IF MsRequest.ReqCParam2 = "VOIP_ADD"    OR
            MsRequest.ReqCParam2 = "VOIP_REMOVE" THEN
         DO:
            Func.Common:mSplitTS(MsRequest.ActStamp,
                                 OUTPUT ldaActiveDate,
                                 OUTPUT liActiveTime).
   
            IF MobSub.TariffBundle = "" THEN
            DO:
               FIND FIRST bCLIType WHERE
                          bCLIType.Brand   = Syst.Var:gcBrand AND
                          bCLIType.CLIType = MobSub.CLIType NO-LOCK NO-ERROR.
               IF AVAIL bCLIType THEN
               DO:
                  IF bCLIType.BaseBundle = "" THEN
                     lcBaseBundle = "".
                  ELSE
                     lcBaseBundle = bCLIType.BaseBundle.
               END.
            END.
            ELSE 
               lcBaseBundle = MobSub.TariffBundle.

            /* Find current Shaper Conf */
            lcShaperConfId = fGetShaperConfId(MsRequest.MSSeq,
                                              lcBaseBundle,
                                             (IF lcBaseBundle = "" THEN
                                              "#ADDBUNDLE" ELSE ""),
                                              ldaActiveDate,
                                              MobSub.CLIType).

            /* Send default shaper if there is no actual shaper id returned */
            IF lcShaperConfId = "" THEN lcShaperConfId = "DEFAULT".

            IF MsRequest.ReqCParam2 = "VOIP_ADD" THEN
               FIND FIRST ShaperConf NO-LOCK WHERE
                          ShaperConf.Brand        = Syst.Var:gcBrand AND
                          ShaperConf.ShaperConfID = (lcShaperConfId + "wVOIP")
                          NO-ERROR.
            ELSE
               FIND FIRST ShaperConf NO-LOCK WHERE
                          ShaperConf.Brand        = Syst.Var:gcBrand AND
                          ShaperConf.ShaperConfID = lcShaperConfId NO-ERROR.

            IF NOT AVAIL ShaperConf THEN
            DO:
               ocError = "ERROR: Shaper Configuration not found for VOIP request".
               RETURN ocError.
            END.

            ASSIGN 
               lcShaperConf_Tariff_Type    = ShaperConf.TariffType
               lcShaperConf_Tariff         = ShaperConf.Tariff
               lcShaperConf_Template       = ShaperConf.Template.

            IF lcShaperConf_Template = "HSPA_MONTHLY" THEN
               ASSIGN 
                  lcShaperConf_Template = "HSPA_MONTHLY_ADD".
            
         END. /* IF MsRequest.ReqCParam2 = "VOIP_ADD" */
         ELSE IF AVAILABLE bMsRequest THEN
         DO:
            Func.Common:mSplitTS(bMsRequest.ActStamp,
                                 OUTPUT ldaActiveDate,
                                 OUTPUT liActiveTime).
   
            IF LOOKUP(bMsRequest.ReqCparam3,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 AND 
                      bMsRequest.ReqType = 8 THEN
               ASSIGN 
                  lcShaperConf_Reset_Day = STRING(DAY(ldaActiveDate)).
            ELSE 
            IF MsRequest.ReqCParam2 <> "DEFAULT" THEN
            DO:
               IF fGetActiveSpecificBundle(bMsRequest.MsSeq,bMsRequest.ActStamp,
                                           "BONO_VOIP") > "" THEN
               DO:
                  FIND FIRST ShaperConf NO-LOCK WHERE
                             ShaperConf.Brand        = Syst.Var:gcBrand AND
                             ShaperConf.ShaperConfID = (MsRequest.ReqCParam2 + "wVOIP")
                             NO-ERROR.
                  IF NOT AVAIL ShaperConf THEN
                  DO:
                     ocError = "ERROR: Shaper Configuration not found for " + 
                               MsRequest.ReqCParam2 + "wVOIP".
                    RETURN ocError.
                  END.
                  
                  ASSIGN 
                     lcShaperConf_Tariff_Type    = ShaperConf.TariffType
                     lcShaperConf_Tariff         = ShaperConf.Tariff
                     lcShaperConf_Template       = ShaperConf.Template                         
                     llVoIPActive                = TRUE.
               END.
   
               IF (bMsRequest.ReqSource = {&REQUEST_SOURCE_STC}               OR
                   bMsRequest.ReqSource = {&REQUEST_SOURCE_BTC}               OR
                   bMsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                   bMsRequest.ReqType   = {&REQTYPE_BUNDLE_CHANGE}) THEN
               DO:
                  IF llVoIPActive THEN
                  DO:
                     IF lcShaperConf_Template = "HSPA_MONTHLY" THEN
                        ASSIGN 
                           lcShaperConf_Template = "HSPA_MONTHLY_ADD".
                  END.
                  ELSE
                  DO:
                     IF lcShaperConf_Template = "HSPA_MONTHLY_ADD" THEN
                        ASSIGN 
                           lcShaperConf_Template = "HSPA_MONTHLY".
                  END.
               END. /* IF (bMsRequest.ReqSource = {&REQUEST_SOURCE_STC} */
               ELSE
               IF bMsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} OR
                  bMsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}
               THEN
               DO:
                  IF lcShaperConf_Template = "HSPA_MONTHLY_ADD" THEN
                     ASSIGN 
                        lcShaperConf_Template = "HSPA_MONTHLY".
               END.
            END.
         END. /* IF AVAILABLE bMsRequest AND */
      END.   
      /* Review when this happens !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      OTHERWISE
      DO:
         IF LOOKUP(MsRequest.ReqCParam1,"CF") > 0 THEN
              ttSolog.CommLine = ttSolog.CommLine + lcServName + ",".
         ELSE DO:
         
            lcParam = STRING(MsRequest.ReqIParam1).
         
            IF MsRequest.ReqIParam1 > 0 AND
               MsRequest.ReqCparam2 NE "" THEN lcParam = MsRequest.ReqCParam2.
            ELSE IF lcServName EQ "NW" THEN DO:
               CASE MsRequest.ReqIParam1:
                  WHEN 1 THEN lcParam = "YOIGO".
                  WHEN 2 THEN lcParam = "YOIGO_ORANGE".
                  WHEN 3 THEN lcParam = "YOIGO_ORANGE_TELEFONICA".
               END CASE.
            END.
               
            ttSolog.CommLine = ttSolog.CommLine +
                               TRIM(lcServName)   + "="  +
                               lcParam + ",".
         END.
      END. */      
   END CASE.

   /* Preparing data for Json */
   FIND FIRST dayCampaign WHERE
              dayCampaign.Brand   = Syst.Var:gcBrand AND
              dayCampaign.DCEvent = lcEmaDataPlan    AND
              dayCampaign.ValidTo >= TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL DayCampaign THEN
   DO:
      ocError = "ERROR: Incorrect data contract " + lcEmaDataPlan.
      RETURN ocError.
   END.
   
   /* 1st command (and only one for not DSS upsells) */
   IF lcProcommandType = "ADD_DSS_UPSELL" THEN 
   DO:
      
   END.
   ELSE 
   DO:
      ASSIGN
         loProCommand                       = NEW Gwy.ProCommand()
         loProCommand:aiMsRequest           = iiMSrequest
         loProCommand:aiMsSeq               = MobSub.MsSeq
         loProCommand:acProCommandTarget    = "NB_CH"
         loProCommand:acProCommandType      = lcProcommandType
         loProCommand:acProCommandVerb      = "POST".
      
      loProCommand:mSetProCommandId(). 
   END. 
   
   /* Common Body to Upsell, Add/Modify/Delete dataplan */
   CREATE ttOrder.
   ASSIGN 
      ttOrder.orderId     = loProCommand:aiProCommandId
      ttOrder.orderType   = "Change"
      ttOrder.sellChannel = "TMSB2C" 
      ttOrder.sellDate    = IF AVAIL bmsrequest THEN 
                               DATETIME-TZ(fDateTime-String(bmsrequest.crestamp))
                            ELSE 
                               DATETIME-TZ(fDateTime-String(msrequest.crestamp))
      ttOrder.seller      = IF AVAIL bmsrequest THEN bmsrequest.salesman 
                                                ELSE msrequest.salesman
      ttOrder.createdBy   = IF AVAIL bmsrequest THEN bmsrequest.usercode 
                                                ELSE msrequest.usercode
      ttOrder.createdDate = IF AVAIL bmsrequest THEN 
                               DATETIME-TZ(fDateTime-String(bmsrequest.crestamp))
                            ELSE 
                               DATETIME-TZ(fDateTime-String(msrequest.crestamp)).   

   CREATE ttOutService.
   ASSIGN 
      ttOutService.orderId     = ttOrder.orderid
      ttOutService.type        = "DEFAULT_PRICE_PLAN"
      ttOutService.ServiceId   = mobsub.clitype
      ttOutService.quantity    = "1"
      ttOutService.action      = "None".  

   CREATE ttLine.
   ASSIGN 
      ttLine.orderId     = ttOutService.orderId
      ttLine.type        = ttOutService.type
      ttLine.serviceId   = ttOutService.ServiceId
      ttLine.phonenumber = mobsub.cli.
   
   /* Specific Services, Line and  object */
   CASE lcProcommandType.
      WHEN "ADD_UPSELL" THEN 
      DO:
         /* UPSELL uses "Change_API_rest" */
         CREATE ttInService.
         ASSIGN 
            ttInService.orderId      = ttOutService.orderId
            ttInService.OutType      = ttOutService.type
            ttInService.OutServiceId = ttOutService.ServiceId
            ttInService.type         = "UPSELL"
            ttInService.ServiceName  = dayCampaign.DCName   /* Upsell name */            
            ttInService.quantity     = "1"
            ttInService.action       = lcAction.  
   
         CREATE ttCharacteristic.
         ASSIGN 
            ttCharacteristic.orderId     = ttInService.orderId
            ttCharacteristic.type        = ttInService.Type
            ttCharacteristic.serviceName = ttInService.ServiceName
            ttCharacteristic.name        = "emaUnshapedLimit"
            ttCharacteristic.valueamt    = lcUpsellSize. 
   
      END.
      WHEN "ADD_DATAPLAN" OR WHEN "DELETE_DATAPLAN" THEN 
      DO:
         /* ADD_DATAPLAN and DELETE_DATAPLAN use "Change_API_rest" */
         CREATE ttInService.
         ASSIGN 
            ttInService.orderId      = ttOutService.orderId
            ttInService.OutType      = ttOutService.type
            ttInService.OutServiceId = ttOutService.ServiceId
            ttInService.type         = "PERIODIC_DATAPLAN"
            ttInService.ServiceName  = dayCampaign.DCName /* Dataplan name */
            ttInService.quantity     = "1"
            ttInService.action       = lcAction.  
   
         CREATE ttCharacteristic.
         ASSIGN 
            ttCharacteristic.orderId     = ttInService.orderId
            ttCharacteristic.type        = ttInService.Type
            ttCharacteristic.serviceName = ttInService.ServiceName
            ttCharacteristic.name        = "emaDataPlan"
            ttCharacteristic.valueamt    = lcEmaDataPlan. 
   
      END.
   END CASE.
   
   DO ON ERROR UNDO, THROW:
      /* Getting Json string */
      loCommandLine = fCreateJSON_for_API_Interface().
      loProCommand:mSetCommandLine(loCommandLine).
      loProCommand:mStoreProCommand().
      CATCH loAppError AS Progress.Lang.AppError :
         ocError = "ERROR: Json generation failed".
         UNDO blk, RETURN ocError.
      END CATCH.
   END.

   oiReqCnt               = 1.  /* Successful */
      
FINALLY:
   IF VALID-OBJECT(loProCommand)
   THEN DELETE OBJECT loProCommand.
   EMPTY TEMP-TABLE ttOrder.
   EMPTY TEMP-TABLE ttOutService.
   EMPTY TEMP-TABLE ttInService.
   EMPTY TEMP-TABLE ttCharacteristic.
   EMPTY TEMP-TABLE ttLine.
END FINALLY.
      
END. /* TRANSACTION */


/***************************************************************
/* Entries to db */
FOR EACH ttProCommand WHERE 
         ttProCommand.CommLine > ""
   BY ttProCommand.ActStamp:
   
   /* Special handling for Prepaid Bono8 HSDPA, SER-1345  */
   IF liCurrentServiceClass > 0 OR
      (MSrequest.ReqCParam1 = "HSDPA" AND
       LOOKUP(Mobsub.CLIType,lcPrepaidVoiceTariffs) > 0) THEN
   DO:

      FIND FIRST bCliType WHERE
                 bCliType.CliType = Mobsub.CliType
      NO-LOCK NO-ERROR.

      IF MsRequest.ReqCParam1 = "HSDPA" AND
         MsRequest.ReqIParam1 = 0 AND
         LOOKUP(Mobsub.CLIType,lcPrepaidVoiceTariffs) > 0 THEN DO:
         FIND FIRST bMsRequest NO-LOCK WHERE
                    bMsRequest.MsSeq = Mobsub.MsSeq AND
                    bMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                    LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") = 0 AND
                    bMsRequest.ActStamp = MsRequest.ActStamp AND
                    LOOKUP(bMsRequest.ReqCparam2,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0
              USE-INDEX MsSeq NO-ERROR.
         IF AVAILABLE bMsRequest THEN
            ASSIGN lcServiceClass = ""
                   llCheckSC      = FALSE.
      END. /* IF MsRequest.ReqCParam1 = "HSDPA" AND */

      /* Check Service Class */
      IF llCheckSC THEN
      CASE Mobsub.CLIType:
         WHEN "TARJ" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0081".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
         END. /* WHEN "TARJ" THEN DO: */
         WHEN "TARJ4" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0084".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
         END. /* WHEN "TARJ4" THEN DO: */
         WHEN "TARJ5" THEN DO:
         
            IF MsRequest.ReqIParam1 EQ 1 THEN DO:
               IF liCurrentServiceClass EQ {&SC_TARJ5_PROMOTIONAL_BONO}
               THEN lcServiceClass = "".
               ELSE lcServiceClass = ",SERVICECLASS=0086".
            END.
            ELSE DO:
               IF liCurrentServiceClass EQ {&SC_TARJ5_PROMOTIONAL}
               THEN lcServiceClass = "".
               ELSE lcServiceClass = (IF AVAIL bCliType THEN
                    ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
            END.
         END. /* WHEN "TARJ5" THEN DO: */
         WHEN "TARJ6" THEN DO:
            /* TODO: This is to prevent HSDPA=0 command (should be possible
               only from STC bundle termination) to override SERVICECLASS in
               STC MODIFY command */  
            IF MsRequest.ReqIParam1 EQ 0 THEN
               lcServiceClass = "".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass
                                 ELSE "").
         END. /* WHEN "TARJ6" THEN DO: */
         WHEN "TARJ7" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0003".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ7" THEN DO: */
         WHEN "TARJ8" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0082".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
         END. /* WHEN "TARJ8" THEN DO: */
         WHEN "TARJ9" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0009".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ9" THEN DO: */
         WHEN "TARJ10" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0010".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ10" THEN DO: */
         WHEN "TARJ11" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0011".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ11" THEN DO: */
         WHEN "TARJ12" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0012".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ12" THEN DO: */
         WHEN "TARJ13" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0020".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ13" THEN DO: */
         OTHERWISE
            lcServiceClass = (IF AVAIL bCliType AND
                                       bCliType.ServiceClass > "" THEN
                              ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
      END CASE.

      ttProCommand.CommLine = ttProCommand.CommLine + lcServiceClass.
   END. /* IF MSrequest.ReqCParam1 = "HSDPA" AND */

   /* Special handling for BB service  */
   IF MSrequest.ReqCParam1 = "BB" AND MsRequest.ReqIParam1 = 1 THEN DO:

      IF Mobsub.CLIType = "TARJ6" OR MobSub.CLIType = "CONTS" THEN
         ttProCommand.Commline = TRIM(ttProCommand.CommLine) + "|1".
      ELSE DO:
      lcActiveBundles = fGetActOngoingDataBundles(INPUT Mobsub.MsSeq,
                                                  INPUT ldActStamp).

      /* Main logic to find the correct profile */
      IF lcActiveBundles > "" THEN DO:
         ASSIGN ttProCommand.Commline = TRIM(ttProCommand.Commline, " ")
                liNumEntries = NUM-ENTRIES(lcActiveBundles).

         DO liCount = 1 TO liNumEntries:
            lcBundle = ENTRY(liCount,lcActiveBundles).
            IF LOOKUP(lcBundle,lcBBProfile1) > 0 THEN DO:
               ASSIGN ttProCommand.Commline = ttProCommand.CommLine + "|1"
                      llSkip = TRUE.
               LEAVE.
            END. /* IF LOOKUP(lcBundle,lcBBProfile1) > 0 THEN DO: */
         END. /* DO liCount = 1 TO liNumEntries: */

         IF NOT llSkip THEN DO:
            liCount = 0.
            DO liCount = 1 TO liNumEntries:
               lcBundle = ENTRY(liCount,lcActiveBundles).
               IF LOOKUP(lcBundle,lcBBProfile2) > 0 THEN DO:
                  ttProCommand.Commline = ttProCommand.CommLine + "|2".
                  LEAVE.
               END. /* IF LOOKUP(lcBundle,lcBBProfile2) > 0 THEN */
            END. /* DO liCount = 1 TO liNumEntries: */
         END. /* IF NOT llSkip THEN DO: */
      END. /* IF lcActiveBundles > "" THEN DO: */
      END.
   END. /* IF MSrequest.ReqCParam1 = "BB" THEN DO: */

   ASSIGN
      Solog.CommLine    = fMakeCommline(Solog.solog,"MODIFY") + ttProCommand.CommLine
      SoLog.CommLine    = fDoubleParam(SoLog.CommLine)
      SoLog.CommLine    = TRIM(REPLACE(SoLog.CommLine,",,",","),",")
      Solog.TimeSlotTMS = ldSchedule
      oiReqCnt                = 1.

END.
*/
