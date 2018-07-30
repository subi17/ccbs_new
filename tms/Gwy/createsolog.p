USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray .

{Syst/commali.i}
{Gwy/provision.i}
{Func/fmakemsreq.i}
{Func/dss_matrix.i}
{Func/msreqfunc.i}
{Func/SAPC.i}
{Func/SAPC_change_API.i}
{Func/cparam2.i}
                    
DEF INPUT PARAMETER iiRequest AS INTEGER NO-UNDO.

DEF VAR ldActStamp                 AS DEC  NO-UNDO.
DEF VAR liOffSet                   AS INT  NO-UNDO.
DEF VAR liReq                      AS INT  NO-UNDO. 
DEF VAR lcError                    AS CHAR NO-UNDO. 
DEF VAR ldeCurrMonthLimit          AS DEC  NO-UNDO. 
DEF VAR ldeConsumedData            AS DEC  NO-UNDO. 
DEF VAR ldeOtherMonthLimit         AS DEC  NO-UNDO. 
DEF VAR lcDSSResult                AS CHAR NO-UNDO. 
DEF VAR lcALLPostpaidBundles       AS CHAR NO-UNDO.
DEF VAR lcALLPostpaidUPSELLBundles AS CHAR NO-UNDO.
DEF VAR lcDependentErrMsg          AS CHAR NO-UNDO. 
DEF VAR lcDSS4PrimarySubTypes      AS CHAR NO-UNDO. 
DEF VAR lcDSS2PrimarySubTypes      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDSSId            AS CHARACTER NO-UNDO.
 
/* SAPC */
DEFINE VARIABLE lcmsisdns          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE dDataLimit         AS DECIMAL   NO-UNDO.  
DEFINE VARIABLE oJson_DSS          AS JsonObject NO-UNDO.
DEFINE VARIABLE oJson_DSS_msisdn   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJson_DataLimit    AS JsonObject NO-UNDO.

DEF BUFFER bbMsRequest FOR MSRequest.
DEF BUFFER bDSSMobSub  FOR MobSub.

/* SAPC - Create DSS group */
DEFINE TEMP-TABLE ttDSS
   FIELD type      AS CHAR  
   FIELD priority  AS CHAR 
   FIELD msisdns   AS CHAR.

/* SAPC - Update data limit */
DEFINE TEMP-TABLE ttDSSDataLimit
   FIELD offeringName      AS CHAR
   FIELD roamingLikeAtHome AS CHAR
   FIELD tariff            AS CHAR.

DEFINE TEMP-TABLE ttDSS_msisdn
   FIELD action AS CHAR 
   FIELD msisdn AS CHAR.


FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest THEN RETURN "ERROR".

lcDependentErrMsg = "ERROR:Another request that this depends on has not been " + 
                    "completed".
   
IF MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} THEN DO:

   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.MsSeq = MsRequest.MsSeq NO-ERROR.

   IF AVAIL MobSub THEN DO:
      IF NOT fCanTerminateConvergenceTariff(MobSub.MsSeq,
                                            INT(MsRequest.ReqCParam3),
                                            OUTPUT lcError) THEN DO:
         fReqError(SUBST("ERROR: &1", lcError)).
         RETURN.
      END.
   
      IF (MobSub.MsStatus NE {&MSSTATUS_MOBILE_PROV_ONG} OR
          MobSub.MsStatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE}) AND
         MobSub.IMSI EQ "" THEN DO:
         fReqStatus(6,"").
         RETURN.
      END.
   END.
END.
 
IF (MSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} OR
    MSRequest.ReqType = {&REQTYPE_ICC_CHANGE}) AND
    MSRequest.ReqIParam5 > 0 THEN 
DO:
   
  FIND FIRST bbMsRequest NO-LOCK WHERE
             bbMsRequest.MsRequest = MsRequest.ReqIParam5 NO-ERROR.
  IF AVAILABLE bbMsRequest AND
     LOOKUP(STRING(bbMsRequest.ReqStatus),
            {&REQ_INACTIVE_STATUSES} + ",3") = 0 THEN
     RETURN lcDependentErrMsg.
END.

/* Verify the criteria again and update ReqCParam2 */
IF MsRequest.ReqType = {&REQTYPE_DSS} AND
   MsRequest.ReqCParam1 = "CREATE" THEN DO:

   IF fOngoingDSSTerm(MsRequest.CustNum,
                      Func.Common:mSecOffSet(MsRequest.ActStamp,180)) THEN 
      RETURN lcDependentErrMsg.

   IF MsRequest.ReqIParam2 > 0 THEN DO:
      FIND FIRST bbMsRequest NO-LOCK WHERE 
                 bbMsRequest.MsRequest = MsRequest.ReqIParam2 NO-ERROR.
      IF AVAILABLE bbMsRequest AND 
         LOOKUP(STRING(bbMsRequest.ReqStatus),
                {&REQ_INACTIVE_STATUSES} + ",3") = 0 THEN 
         RETURN lcDependentErrMsg.
   END. /* IF MsRequest.ReqIParam2 > 0 THEN DO: */

   ASSIGN lcALLPostpaidBundles = fCParamC("ALL_POSTPAID_CONTRACTS")
          lcALLPostpaidUPSELLBundles = fCParamC("POSTPAID_DATA_UPSELLS").

   IF CAN-FIND(FIRST bbMsRequest NO-LOCK WHERE
                    bbMsRequest.Brand   = Syst.Var:gcBrand AND
                    bbMsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
                    bbMsRequest.Custnum = MsRequest.CustNum AND
           (LOOKUP(bbMsRequest.ReqCParam3,lcALLPostpaidBundles) > 0 OR
            LOOKUP(bbMsRequest.ReqCParam3,lcALLPostpaidUPSELLBundles) > 0) AND
           LOOKUP(STRING(bbMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0
           USE-INDEX CustNum) THEN
      RETURN lcDependentErrMsg.

   IF CAN-FIND(FIRST bbMsRequest NO-LOCK WHERE
                    bbMsRequest.Brand   = Syst.Var:gcBrand AND
                    bbMsRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
                    bbMsRequest.Custnum = MsRequest.CustNum AND
                    bbMsRequest.ReqCparam1 = "SHAPER"  AND
                    bbMsRequest.ReqCparam2 = "DEFAULT" AND
           LOOKUP(STRING(bbMsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES} + ",3") = 0
           USE-INDEX CustNum) THEN
      RETURN lcDependentErrMsg.

   IF NOT fIsDSSAllowed(INPUT  MsRequest.CustNum,
                        INPUT  MsRequest.MsSeq,
                        INPUT  (IF MsRequest.ActStamp > Func.Common:mMakeTS() THEN
                                MsRequest.ActStamp ELSE Func.Common:mMakeTS()),
                        INPUt  MsRequest.ReqCparam3,
                        INPUT  "",
                        OUTPUT ldeCurrMonthLimit,
                        OUTPUT ldeConsumedData,
                        OUTPUT ldeOtherMonthLimit,
                        OUTPUT lcDSSResult) THEN DO:
      /* YDR-392 - Criteria does not match then should go in separate queue */
      IF MsRequest.ReqCparam3 = {&DSS} AND
         (lcDSSResult = "dss_no_postpaid_subscription" OR
          lcDSSResult = "dss_no_data_bundle") THEN
         fReqStatus(19,lcDSSResult).
      ELSE
         fReqError(lcDSSResult).
      RETURN.
   END. /* IF NOT fIsDSSAllowed(INPUT  MsRequest.CustNum */
   ELSE DO TRANSACTION:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE MsRequest THEN
         MsRequest.ReqCParam2 = lcDSSResult.
   END.
END.

/* This is a hack this has to be removed after root cause fix */
DO TRANSACTION:
   IF MsRequest.ReqType    EQ {&REQTYPE_DSS} AND 
      MsRequest.ReqCParam3 EQ ""             THEN DO:
      
      FIND FIRST bDSSMobSub NO-LOCK WHERE 
                 bDSSMobSub.MsSeq EQ MsRequest.MsSeq NO-ERROR.

      IF NOT AVAIL bDSSMobSub THEN 
         RETURN "DSS subscription not available".

      ASSIGN lcDSS2PrimarySubTypes = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
             lcDSS4PrimarySubTypes = fCParamC("DSS4_PRIMARY_SUBS_TYPE").

      IF LOOKUP(bDSSMobSub.CLIType,lcDSS4PrimarySubTypes) > 0 THEN 
         lcDSSId = {&DSS4}.
      ELSE IF LOOKUP(bDSSMobSub.CLIType,lcDSS2PrimarySubTypes) > 0 THEN  
         lcDSSId = {&DSS2}.

      IF lcDSSId > "" THEN DO:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR.

         IF AVAIL MsRequest THEN 
            MsRequest.ReqCParam3 = lcDSSId.

      END.

   END.
END.

IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN DO:
   liOffSet = MsRequest.ReqIParam4.
   RUN pSolog.
END.
ELSE IF MsRequest.ReqType = {&REQTYPE_DSS} THEN RUN pSolog.

ELSE CASE MsRequest.ReqCParam1:
   WHEN "CREATE"        THEN RUN pSolog.
   WHEN "DELETE"        THEN RUN pSolog.
   WHEN "REACTIVATE"    THEN RUN pSolog.
   WHEN "CHANGEICC"     THEN RUN pSolog.
   WHEN "CHANGEMSISDN"  THEN RUN pSolog. 
   WHEN "CHANGEPAYTYPE" THEN RUN pSolog.
   OTHERWISE RETURN "ERROR".
END CASE.

RETURN RETURN-VALUE.

PROCEDURE pSolog:

   DEF BUFFER bufOrder  FOR Order.
   DEF BUFFER bufMobsub FOR Mobsub.
   DEF BUFFER bufTermMobsub FOR TermMobsub.
   DEF BUFFER bActionLog    FOR ActionLog.

   DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO.
   DEF VAR ldCurrBal AS DECIMAL NO-UNDO. 
   DEF VAR liError       AS INT NO-UNDO.
   DEF VAR lcResult      AS CHAR NO-UNDO.
   DEF VAR liOrderId     AS INT  NO-UNDO.

   /* SAPC */
   DEFINE VARIABLE cJsonMsg           AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE cJsonMsg_DataLimit AS LONGCHAR NO-UNDO.
   DEFINE VARIABLE lJsonCreation      AS LOGICAL  NO-UNDO.


   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   IF MsRequest.ReqType = {&REQTYPE_DSS} THEN lcCli = MsRequest.CLI.

   ELSE IF MsRequest.ReqCParam1 = "CREATE" THEN DO:
      
      FOR EACH BufOrder NO-LOCK WHERE
               BufOrder.MSSeq = MsreQuest.MSSeq AND
               LOOKUP(STRING(BufOrder.OrderType),"0,1,3") > 0
         BY BufOrder.CrStamp DESC:
         LEAVE.
      END.

      IF NOT AVAILABLE bufOrder THEN DO:
         fReqError("Order Subscription not found").
         RETURN.
      END.
      
      lcCli = BufOrder.Cli.
      
   END.
   ELSE IF MsRequest.ReqCParam1 = "REACTIVATE" THEN DO:
      
      FIND FIRST bufTermMobsub WHERE 
                 bufTermMobsub.MSSeq = MSRequest.MSSeq
      NO-LOCK NO-ERROR.

      IF NOT AVAILABLE bufTermMobsub THEN DO:
         fReqError("Terminated Mobile Subscription not found").
         RETURN.
      END.
   
      lcCli = bufTermMobsub.CLI.
      
   END. /* ELSE IF MsRequest.ReqCParam1 = "REACTIVATE" THEN DO: */
   ELSE DO:
      
      FIND FIRST bufMobsub WHERE 
                 bufMobsub.MSSeq = MSRequest.MSSeq
      NO-LOCK NO-ERROR.

      IF NOT AVAILABLE bufMobsub THEN DO:
         fReqError("Mobile Subscription not found").
         RETURN.
      END.
   
      lcCli = BufMobsub.CLI.
   
      IF (MSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} OR
          MSRequest.ReqType = {&REQTYPE_ICC_CHANGE}) THEN 
      DO:

         IF MSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION}  THEN
         DO:

            IF (fIsFixedOnly(bufMobsub.CLIType)                  AND
                MSRequest.ReqCParam6 EQ {&TERMINATION_TYPE_FULL} AND
                CAN-FIND(FIRST bActionLog NO-LOCK  WHERE
                               bActionLog.Brand     EQ Syst.Var:gcBrand                     AND
                               bActionLog.ActionID  EQ {&MERGE2P3P}                         AND
                               bActionLog.TableName EQ "MobSub"                             AND
                       ENTRY(1,bActionLog.ActionChar,CHR(255)) EQ STRING(MsRequest.MsSeq))) THEN .
            ELSE IF (fHasConvergenceTariff(MSRequest.MSSeq) AND
                     MSRequest.ReqCParam6 = {&TERMINATION_TYPE_FULL}) THEN DO:

               liOrderId = fFindFixedLineOrder(MSRequest.MSSeq).
               IF liOrderId EQ 0
                  THEN lcResult = "OrderID not found".
               /* This call makes synchronous termination request to MuleDB */
               ELSE lcResult = fSendFixedLineTermReqToMuleDB(liOrderId).
               
               IF lcResult > "" THEN DO:
                  Func.Common:mWriteMemo("MobSub",
                              STRING(BufMobsub.MsSeq),
                              BufMobsub.Custnum,
                              "La baja del sevicio fijo ha fallado: ", /* Fixed number termination failed" */
                              lcResult).
                  fReqError("La baja del sevicio fijo ha fallado: " + lcResult).
                  RETURN.
               END.       
            END.
         END.

         /* Cancel the active/suspended BB service before
            subscription termination or icc change provisioning */
         FOR FIRST SubSer WHERE SubSer.ServCom = "BB"            AND
                                SubSer.MsSeq   = MsRequest.MsSeq AND
                                SubSer.SsDate <= TODAY  NO-LOCK:
            IF SubSer.SSStat > 0 THEN DO:
               IF CAN-FIND(FIRST bbMSRequest WHERE
                           bbMSRequest.MsSeq   = MsRequest.MsSeq AND
                           bbMSRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
                           bbMSRequest.ReqCParam1 = "BB" AND
                           bbMSRequest.ReqIParam1 = 0 AND
                           LOOKUP(STRING(bbMSRequest.ReqStatus),
                                  {&REQ_INACTIVE_STATUSES}) = 0
                           USE-INDEX MsSeq) THEN LEAVE.

               liReq = fServiceRequest(INPUT MsRequest.MsSeq,
                                       INPUT SubSer.ServCom,
                                       INPUT 0,
                                       INPUT "",
                                       INPUT Func.Common:mMakeTS(),
                                       INPUT "",                /* SalesMan */
                                       INPUT TRUE,              /* Set fees */
                                       INPUT FALSE,             /* SMS      */
                                       INPUT "",
                                       INPUT IF MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} 
                                             THEN {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION}
                                             ELSE MsRequest.ReqSource,
                                       INPUT (IF SubSer.SSStat <> 1 THEN 0
                                              ELSE MsRequest.MsRequest),
                                       INPUT FALSE,   /* Mandatory Request */
                                       OUTPUT lcError).
               IF liReq > 0 THEN DO TRANSACTION:
                  FIND CURRENT MsRequest EXCLUSIVE-LOCK.
                  ASSIGN MsRequest.ReqIParam5 = liReq.
                  fReqStatus(0,"").
                  RELEASE MsRequest.
                  RETURN.
               END.
               ELSE
                  Func.Common:mWriteMemo("Customer",
                          STRING(MsRequest.CustNum),
                          MsRequest.Custnum,
                          "BB Service",
                          "Deactivating BB service failed; "
                           + lcError).
            END. /* IF SubSer.SSStat > 0 THEN DO: */
         END. /* FOR FIRST SubSer WHERE SubSer.ServCom = "BB" AND */
         
         /* Create a memo with prepaid balance if any */
         IF bufMobSub.PayType THEN DO:
            RUN Gwy/balancequery.p(bufMobSub.CLI).
            ldCurrBal = DEC(RETURN-VALUE) / 100 NO-ERROR.
            IF ldCurrBal > 0 THEN
               Func.Common:mWriteMemo("Mobsub",
                          STRING(bufMobSub.MsSeq),
                          bufMobSub.Custnum,
                          "Prepaid Balance",
                          "Prepaid balance " + STRING(ldCurrBal) + 
                          " euro on CLI " + bufMobSub.CLI).
            ELSE IF RETURN-VALUE BEGINS "ERROR" THEN
               Func.Common:mWriteMemo("Mobsub",
                          STRING(bufMobSub.MsSeq),
                          bufMobSub.Custnum,
                          "Prepaid Balance",
                          "Balance query failed:" + CHR(10) + 
                          RETURN-VALUE).
         END. /* IF MobSub.PayType THEN DO: */
      END.
   END.
   
   ldActStamp = MSRequest.ActStamp.
   IF liOffSet NE 0 THEN 
      ldActStamp = Func.Common:mSecOffSet(ldActStamp,liOffSet).
      
   DO TRANSACTION:
      
      /* SAPC-56 redirecting new SAPC customers to new logic */
      IF Customer.AccGrp = 2 AND 
         fIsFunctionAvailInSAPC(Msrequest.msrequest) THEN  /* SAPC */
      DO:
         /* Locating data */
         FIND FIRST Mobsub WHERE 
                    Mobsub.MSSeq = MSRequest.MSSeq
              NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Mobsub THEN
         DO:
            fReqError("Mobile Subscription not found for request " + 
                       STRING(MSRequest.MSRequest)).
            RETURN.
         END.
         
         IF MSrequest.ReqCparam1 = "CREATE" THEN /* Creating DSS group */
         DO:
            ASSIGN 
               lcmsisdns = SUBSTRING(MSRequest.ReqCParam2,INDEX(cc,"MSISDNS="))
               lcmsisdns = REPLACE(lcmsisdns,"MSISDNS=","")
               lcmsisdns = REPLACE(lcmsisdns,";","|").
                       
            /* 1st. Creating ProCommand for creating DSS group */   
            DEF VAR lcdummygrp AS CHAR NO-UNDO INITIAL "dummygrp-".
            lcdummygrp = lcdummygrp + STRING(TIME).
             
            CREATE ProCommand.
            ASSIGN
               ProCommand.MsRequest           = MsRequest.MsRequest
               ProCommand.ProcommandId        = NEXT-VALUE(Seq_ProCommand_ProCommandId)
               ProCommand.ProCommandType      = "CREATE_DSS_GROUP"
               ProCommand.CreatedTS           = NOW 
               ProCommand.Creator             = Syst.Var:katun    
               ProCommand.MsSeq               = MobSub.MsSeq   /* Mobile Subscription No. */
               ProCommand.ProCommandstatus    = 0              /* 0 - New                 */
               ProCommand.ProCommandtarget    = "NB_CH"
               ProCommand.ProCommandVerb      = "PUT"
               ProCommand.ProCommandtargetURL = "/groups/" + lcdummygrp.
            
            /* Json content */
            CREATE ttDSS.
            ASSIGN 
               ttDSS.type     = "POST"  /* Hard-coded. Type of user: POST */  
               ttDSS.priority = "1000"  /* Hard-coded. For Base tariff = 1000 */
               ttDSS.msisdns  = lcmsisdns.

            /* Getting Json string */
            oJson_DSS = NEW JsonObject().
            oJson_DSS:add("type", ttDSS.type).
            oJson_DSS:add("priority", ttDSS.priority).
            oJson_DSS:add("msisdns", ttDSS.msisdns).
            lJsonCreation = oJson_DSS:WRITE(cJsonMsg,TRUE).
                      
            /*
            lJsonCreation = TEMP-TABLE ttDSS:WRITE-JSON("LONGCHAR",
                                                        cJsonMsg, 
                                                        TRUE,    /* Formatted           */
                                                        "UTF-8", /* Encoding            */
                                                        FALSE,   /* Omit initial values */
                                                        TRUE,    /* Omit outer object   */
                                                        FALSE).  /* Write before image  */
            */
            IF lJsonCreation = FALSE then
            DO:
               fReqError("Json generation failed for request " + 
                         STRING(MSRequest.MSRequest)).
               UNDO, RETURN.
            END.
            ELSE
               /* Successful. Saving Json message command */ 
               ASSIGN
                  Procommand.CommandLine = cJsonMsg.
         END. 
         ELSE
         IF MSrequest.ReqCparam1 = "ADD"    OR   /* Adding msisdn to DSS */
            MSrequest.ReqCparam1 = "REMOVE" THEN /* Removing msisdn from DSS */    
         DO:  
            ASSIGN 
               lcmsisdns = SUBSTRING(MSRequest.ReqCParam2,INDEX(cc,"MSISDN="))
               lcmsisdns = REPLACE(lcmsisdns,"MSISDN=","")
               lcmsisdns = SUBSTRING(lcmsisdns,1,INDEX(lcmsisdns,",") - 1).
                       
            /* 1st. Creating ProCommand for Adding/removing msisdn to DSS group */   
            CREATE ProCommand.
            ASSIGN
               ProCommand.MsRequest           = MsRequest.MsRequest
               ProCommand.ProcommandId        = NEXT-VALUE(Seq_ProCommand_ProCommandId)
               ProCommand.ProCommandType      = (IF MSrequest.ReqCparam1 = "ADD" THEN 
                                                    "ADD_TO_DSS_GROUP"
                                                 ELSE 
                                                    "REMOVE_FROM_DSS_GROUP")
               ProCommand.CreatedTS           = NOW 
               ProCommand.Creator             = Syst.Var:katun    
               ProCommand.MsSeq               = MobSub.MsSeq  /* Mobile Subscription No. */
               ProCommand.ProCommandstatus    = 0             /* 0 - New                 */
               ProCommand.ProCommandtarget    = "NB_CH"
               ProCommand.ProCommandVerb      = "POST"
               ProCommand.ProCommandtargetURL = "/groups/" + lcdummygrp + 
                                                "/manage-subscription".
            
            /* Json content */
            CREATE ttDSS_msisdn.
            ASSIGN 
               ttDSS_msisdn.action = (IF MSrequest.ReqCparam1 = "ADD" THEN
                                         "add"  /* Hard-coded when adding to DSS */
                                      ELSE   
                                         "remove") /* Hard-coded when removing from DSS */
               ttDSS_msisdn.msisdn = lcmsisdns.
            
            /* Getting Json string */
            oJson_DSS_msisdn = NEW JsonObject().
            oJson_DSS_msisdn:add("action", ttDSS_msisdn.action).
            oJson_DSS_msisdn:add("msisdn", ttDSS_msisdn.msisdn).
            lJsonCreation = oJson_DSS_msisdn:WRITE(cJsonMsg,TRUE).
                      
            /* 
            lJsonCreation = TEMP-TABLE ttDSS_msisdn:WRITE-JSON("LONGCHAR",
                                                               cJsonMsg, 
                                                               TRUE,    /* Formatted           */
                                                               "UTF-8", /* Encoding            */
                                                               FALSE,   /* Omit initial values */
                                                               TRUE,    /* Omit outer object   */
                                                               FALSE).  /* Write before image  */
            */
            IF lJsonCreation = FALSE then
            DO:
               fReqError("Json generation failed for request " + 
                         STRING(MSRequest.MSRequest)).
               UNDO, RETURN.
            END.
            ELSE
               /* Successful. Saving Json message command */ 
               ASSIGN
                  Procommand.CommandLine = cJsonMsg.
            
         END.
         ELSE IF MSrequest.ReqCparam1 = "DELETE" THEN /* Delete DSS group */    
         DO:                         
            /* Creating ProCommand for Deleting DSS group */   
            CREATE ProCommand.
            ASSIGN
               ProCommand.MsRequest           = MsRequest.MsRequest
               ProCommand.ProcommandId        = NEXT-VALUE(Seq_ProCommand_ProCommandId)
               ProCommand.ProCommandType      = "TERMINATE_DSS_GROUP"
               ProCommand.CreatedTS           = NOW 
               ProCommand.Creator             = Syst.Var:katun    
               ProCommand.MsSeq               = MobSub.MsSeq  /* Mobile Subscription No. */
               ProCommand.ProCommandstatus    = 0             /* 0 - New                 */
               ProCommand.ProCommandtarget    = "NB_CH"
               ProCommand.ProCommandVerb      = "DELETE"
               ProCommand.ProCommandtargetURL = "/groups/" + lcdummygrp.
         END.
         
         IF MSrequest.ReqCparam1 NE "DELETE" THEN /* Adding/Removing msisdn to DSS */
         DO:
            /* 2nd. Creating ProCommand for updating Data limit for DSS group 
                    This command is needed when creating a DSS group or 
                    adding a msisdn to the DSS group */
            CREATE ProCommand.
            ASSIGN
               ProCommand.MsRequest           = MsRequest.MsRequest
               ProCommand.ProcommandId        = NEXT-VALUE(Seq_ProCommand_ProCommandId)
               ProCommand.ProCommandType      = "UPDATE_DSS_GROUP_DATALIMIT"
               ProCommand.CreatedTS           = NOW 
               ProCommand.Creator             = Syst.Var:katun    
               ProCommand.MsSeq               = MobSub.MsSeq  
               ProCommand.ProCommandstatus    = 0               /* 0 - New */
               ProCommand.ProCommandtarget    = "NB_CH"
               ProCommand.ProCommandVerb      = "POST"            
               ProCommand.ProCommandtargetURL = "/groups/" + lcdummygrp + 
                                                "/set-accumulated-volume".
     
            /* Json content */
            ASSIGN dDataLimit = fGetDSSDataLimit(MsRequest.MsRequest).
            IF dDataLimit = 0 THEN
            DO:
               fReqError("Json generation (datalimit not found) failed for request " + 
                         STRING(MSRequest.MSRequest)).
               UNDO, RETURN.
            END.
            
            CREATE ttDSSDataLimit.
            ASSIGN 
               ttDSSDataLimit.offeringName      = MsRequest.ReqCParam3 /* DSS/DSS2/DSS4 */  
               ttDSSDataLimit.roamingLikeAtHome = STRING(dDataLimit) 
               ttDSSDataLimit.tariff            = STRING(dDataLimit).
   
            /* Getting Json string */
            oJson_DataLimit = NEW JsonObject().
            oJson_DataLimit:add("offeringName", ttDSSDataLimit.offeringName).
            oJson_DataLimit:add("roamingLikeAtHome", ttDSSDataLimit.roamingLikeAtHome).
            oJson_DataLimit:add("tariff", ttDSSDataLimit.tariff).
            lJsonCreation = oJson_DataLimit:WRITE(cJsonMsg_DataLimit,TRUE).
                   
            /* 
            lJsonCreation = TEMP-TABLE ttDSSDataLimit:WRITE-JSON("LONGCHAR",
                                                                 cJsonMsg_DataLimit, 
                                                                 TRUE,    /* Formatted           */
                                                                 "UTF-8", /* Encoding            */
                                                                 FALSE,   /* Omit initial values */
                                                                 TRUE,    /* Omit outer object   */
                                                                 FALSE).  /* Write before image  */
            */
            IF lJsonCreation = FALSE then
            DO:
               fReqError("Json generation (datalimit) failed for request " + 
                         STRING(MSRequest.MSRequest)).
               UNDO, RETURN.
            END.
            ELSE
               /* Successful. Saving Json message command */ 
               ASSIGN
                  Procommand.CommandLine = cJsonMsg_DataLimit.
         END.            
      END.  /*end of SAPC customer */
      ELSE 
      DO: /* Existing logic for Packet Logic */
         CREATE Solog.
         ASSIGN
            Solog.Solog = NEXT-VALUE(Solog).
   
         ASSIGN
            Solog.CreatedTS    = Func.Common:mMakeTS()
            Solog.MsSeq        = MsreQuest.MsSeq    /* Mobile Subscription No.    */
            Solog.CLI          = lcCLI              /* MSISDN                     */
            Solog.Stat         = 0                  /* just created               */
            Solog.Brand        = MSRequest.Brand
            Solog.Users        = MSREquest.UserCode
            Solog.TimeSlotTMS  = ldActStamp
            Solog.ActivationTS = ldActStamp
            Solog.MSrequest    = MSRequest.MSRequest.
   
         IF MsRequest.ReqCParam1 = "CHANGEICC"      OR
            MSrequest.ReqCparam1 = "CHANGEMSISDN"   OR
            MSrequest.ReqType    = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN
            Solog.CommLine = fMakeCommLine2(Solog.Solog,MsRequest.MSrequest,False).
         ELSE IF MSrequest.ReqType = {&REQTYPE_DSS} THEN
            Solog.CommLine = fMakeDSSCommLine(Solog.Solog,MsRequest.MSrequest).
         ELSE
            Solog.CommLine = fMakeCommLine(Solog.Solog,MsRequest.ReqCParam1).
   
         ASSIGN
            SoLog.CommLine = TRIM(REPLACE(SoLog.CommLine,",,",","),",").
   
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   
         MsRequest.Solog = Solog.Solog.

      END.
      
      fReqStatus(5,"").
      
   END.

END PROCEDURE.


