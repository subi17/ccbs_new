{Syst/commali.i}
{Gwy/provision.i}
{Func/fmakemsreq.i}
{Func/dss_matrix.i}
{Func/msreqfunc.i}

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
DEFINE VARIABLE lcDSSId AS CHARACTER NO-UNDO.
DEF VAR lcResult                   AS CHAR NO-UNDO.
DEF VAR liOrderId                  AS INT  NO-UNDO.

DEF BUFFER bbMsRequest FOR MSRequest.
DEF BUFFER bDSSMobSub  FOR MobSub.
DEF BUFFER bActionLog  FOR ActionLog.

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
   
      IF MobSub.IMSI EQ "" THEN DO:

         IF (fIsFixedOnly(Mobsub.CLIType) AND
             CAN-FIND(FIRST bActionLog NO-LOCK  WHERE
                            bActionLog.Brand     EQ Syst.Var:gcBrand                     AND
                            bActionLog.ActionID  EQ {&MERGE2P3P}                         AND
                            bActionLog.TableName EQ "MobSub"                             AND
                    ENTRY(1,bActionLog.ActionChar,CHR(255)) EQ STRING(MsRequest.MsSeq))) THEN .
         ELSE DO:

            liOrderId = fFindFixedLineOrder(MSRequest.MSSeq).
            IF liOrderId EQ 0
               THEN lcResult = "OrderID not found".
            /* This call makes synchronous termination request to MuleDB */
            ELSE lcResult = fSendFixedLineTermReqToMuleDB(liOrderId).

            IF lcResult > "" THEN DO:
               Func.Common:mWriteMemo("MobSub",
                           STRING(Mobsub.MsSeq),
                           Mobsub.Custnum,
                           "La baja del sevicio fijo ha fallado: ", /* Fixed number termination failed" */
                           lcResult).
               fReqError("La baja del sevicio fijo ha fallado: " + lcResult).
               RETURN.
            END.
         END.
            
         fReqStatus(6,"").
         RETURN.
      END.
   END.
END.
 
IF (MSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} OR
    MSRequest.ReqType = {&REQTYPE_ICC_CHANGE}) AND
    MSRequest.ReqIParam5 > 0 THEN DO:
   
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
                        INPUT  MsRequest.ReqCparam3,
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

   DEF BUFFER bufOrder      FOR Order.
   DEF BUFFER bufMobsub     FOR Mobsub.
   DEF BUFFER bufTermMobsub FOR TermMobsub.
   DEF BUFFER bufActionLog  FOR ActionLog.

   DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO.
   DEF VAR ldCurrBal     AS DECIMAL NO-UNDO.
   DEF VAR liError       AS INT NO-UNDO.

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
          MSRequest.ReqType = {&REQTYPE_ICC_CHANGE}) THEN DO:

         IF MSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION}  THEN DO:
           
            IF (fIsFixedOnly(bufMobsub.CLIType)                  AND
                MSRequest.ReqCParam6 EQ {&TERMINATION_TYPE_FULL} AND
                CAN-FIND(FIRST bufActionLog NO-LOCK  WHERE
                               bufActionLog.Brand     EQ Syst.Var:gcBrand                     AND
                               bufActionLog.ActionID  EQ {&MERGE2P3P}                         AND
                               bufActionLog.TableName EQ "MobSub"                             AND
                       ENTRY(1,bufActionLog.ActionChar,CHR(255)) EQ STRING(MsRequest.MsSeq))) THEN .
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

      fReqStatus(5,"").
   END.

END PROCEDURE.


