/* ----------------------------------------------------------------------
  MODULE .......: dss_request.p  
  TASK .........: Handle DSS requests
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 10.09.11   
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/fsendsms.i}
{Func/fcpfat.i}
{Mm/active_bundle.i}
{Func/service.i}
{Mm/fbundle.i}
{Func/dss_matrix.i}
{Func/dss_request.i}
{Func/msreqfunc.i}

DEF INPUT PARAMETER iiMsRequest AS INT  NO-UNDO.

DEF VAR lcUseCLIType      AS CHAR NO-UNDO.

DEF TEMP-TABLE ttAdditionalSIM NO-UNDO
    FIELD MsSeq    AS INT
    FIELD CustNum  AS INT
    FIELD CLI      AS CHAR.

/******** Main start *********/

FIND FIRST MSRequest WHERE
           MSRequest.MSRequest = iiMsRequest  AND
           MSRequest.ReqType   = {&REQTYPE_DSS} NO-LOCK NO-ERROR.
IF NOT AVAIL MSRequest THEN
   RETURN "ERROR:Unknown MSRequest " + STRING(iiMSRequest).

FIND FIRST Mobsub WHERE
           Mobsub.MSSeq = MsRequest.MSSeq NO-LOCK NO-ERROR.
IF AVAILABLE Mobsub THEN
   lcUseCLIType = MobSub.CLIType.
ELSE DO:
   FIND FIRST TermMobsub WHERE
              TermMobsub.MSSeq = MsRequest.MSSeq NO-LOCK NO-ERROR.
   IF AVAILABLE TermMobsub THEN
      lcUseCLIType = TermMobsub.CLIType.
END. /* ELSE DO: */

CASE MSRequest.ReqStatus:
   WHEN {&REQUEST_STATUS_HLR_DONE} THEN DO:
      IF MsRequest.ReqCparam1 = "CREATE" OR
         MsRequest.ReqCparam1 = "DELETE" THEN
         RUN pDSSContract.
      ELSE
         RUN pFinalize.
   END. /* WHEN {&REQUEST_STATUS_HLR_DONE} THEN DO: */
   WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} THEN
      RUN pFinalize.
   OTHERWISE RETURN "ERROR:Current status is not handled".
END CASE. /* CASE MSRequest.ReqStatus: */

IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
   fReqError(RETURN-VALUE).
END.

RETURN RETURN-VALUE.

/******** Main end *********/


PROCEDURE pDSSContract:

   DEF VAR lcError                AS CHAR NO-UNDO.
   DEF VAR liRequest              AS INT  NO-UNDO.
   DEF VAR lcActParam             AS CHAR NO-UNDO. 
         
   DEF BUFFER bTermRequest    FOR MsRequest.
   DEF BUFFER bMsRequest      FOR MsRequest.
   
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = MsRequest.ReqCParam3 AND
              LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign OR
      LOOKUP(DayCampaign.DCEvent,{&DSS_BUNDLES}) = 0 THEN
      RETURN "ERROR:Invalid Bundle".

   IF MsRequest.ReqCParam1 = "CREATE" THEN DO:
         
      /* Check if DSS termination is still ongoing for the
         old customer after ACC */
      FIND FIRST bMsRequest NO-LOCK WHERE
                 bMsRequest.MsSeq = MsRequest.MsSeq AND
                 bMsRequest.ReqType = 83 AND
                 bMsRequest.Custnum NE MsRequest.Custnum AND
                 bMsRequest.ReqCParam3 = MsRequest.ReqCParam3 AND
                 bMsRequest.ReqCParam1 = "DELETE" AND
          LOOKUP(STRING(bMsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES} + ",3") = 0 NO-ERROR.

      /* DSS termination request could be also mapped to different subs. */
      IF NOT AVAIL bMsRequest THEN
         FOR FIRST ServiceLimit NO-LOCK WHERE
                   ServiceLimit.GroupCode = MsRequest.ReqCParam3,
            FIRST MServiceLimit NO-LOCK WHERE
                  MServiceLimit.MsSeq    = MsRequest.MsSeq AND
                  MServiceLimit.DialType = ServiceLimit.DialType AND
                  MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                  MServiceLimit.EndTS    = 99999999.99999 AND
                  MServiceLimit.Custnum NE MsRequest.Custnum:

            FIND FIRST bMsRequest NO-LOCK WHERE
                       bMsRequest.Brand = Syst.Var:gcBrand AND
                       bMsRequest.ReqType = 83 AND
                       bMsRequest.Custnum = MServiceLimit.Custnum AND
                       bMsRequest.ReqCParam3 = MsRequest.ReqCParam3 AND
                       bMsRequest.ReqCParam1 = "DELETE" AND
                LOOKUP(STRING(bMsRequest.ReqStatus),
                       {&REQ_INACTIVE_STATUSES} + ",3") = 0 NO-ERROR.
         END.

      IF AVAIL bMsRequest THEN
         lcActParam = "act:wait" + STRING(bMsRequest.MsRequest).
      ELSE lcActParam = "act".

      /* Activate DSS periodical contract */
      liRequest = fPCActionRequest(MsRequest.MsSeq,
                                   MsRequest.ReqCParam3, 
                                   lcActParam,
                                   MsRequest.ActStamp,
                                   TRUE,   /* create fee */
                                   {&REQUEST_SOURCE_DSS},
                                   "",
                                   MsRequest.MsRequest,
                                   TRUE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcError).
      IF liRequest = 0 THEN 
         RETURN "ERROR:Bundle activation request creation failed; " + lcError.
   END. /* IF MsRequest.ReqCParam1 = "CREATE" THEN DO: */
   ELSE DO:
      /* Terminate DSS periodical contract */
      liRequest = fPCActionRequest(MsRequest.MsSeq,
                                   MsRequest.ReqCParam3,
                                   "term",
                                   MsRequest.ActStamp,
                                   TRUE,   /* create fee */
                                   {&REQUEST_SOURCE_DSS},
                                   "",
                                   MsRequest.MsRequest,
                                   TRUE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcError).
      IF liRequest = 0 THEN 
         RETURN "ERROR:Bundle termination request creation failed; " + lcError.

      /* Keep MsRequest Custnum same as Main request custnum */
      DO TRANS:
         FIND FIRST bTermRequest WHERE
                    bTermRequest.MsRequest = liRequest
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAILABLE bTermRequest THEN
            bTermRequest.CustNum = MsRequest.CustNum.

         RELEASE bTermRequest.
      END. /* DO TRANS: */
   END. /* ELSE DO: */
   
   /* wait for subrequests */
   fReqStatus(7,"").

   RETURN "".
   
END PROCEDURE.

PROCEDURE pFinalize:

   DEF VAR lcMSISDNS        AS CHAR NO-UNDO.
   DEF VAR lcCLI            AS CHAR NO-UNDO.
   DEF VAR liNumEntries     AS INT  NO-UNDO.
   DEF VAR liCount          AS INT  NO-UNDO.
   DEF VAR liDSSMsSeq       AS INT  NO-UNDO. 
   DEF VAR ldeDSSLimit      AS DEC  NO-UNDO. 
   DEF VAR liPeriod         AS INT  NO-UNDO. 
   DEF VAR ldaDate          AS DATE NO-UNDO. 
   DEF VAR liTime           AS INT  NO-UNDO. 
   DEF VAR lcResult         AS CHAR NO-UNDO. 
   DEF VAR lcBundleId       AS CHAR NO-UNDO.
   DEF VAR llMultiSimActive AS LOG  NO-UNDO.
   DEF VAR liRequest        AS INT  NO-UNDO.
   DEF VAR liDSSPriMsSeq    AS INT  NO-UNDO.
   DEF VAR ldeCurrentTS     AS DEC  NO-UNDO.
   DEF VAR lcError          AS CHAR NO-UNDO.

   DEF BUFFER MobSub      FOR MobSub.
   DEF BUFFER lbMobSub    FOR MobSub.
   DEF BUFFER bMsRequest  FOR MsRequest.

   /* check that subrequests really are ok */
   IF fGetSubRequestState(MsRequest.MsRequest) NE 2 THEN DO:
      IF MsRequest.ReqStat NE 7 THEN DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.   
      RETURN "".
   END.

   fReqStatus(2,"").

   ldeCurrentTS = Func.Common:mMakeTS().

   /* Send the SMS using Request Action Rules for DSS */
   RUN Mm/requestaction_sms.p(INPUT MsRequest.MsRequest,
                           INPUT lcUseCLIType,
                           INPUT MsRequest.ReqSource).

   /* Send separate SMS to all other postpaid subs. */
   IF MsRequest.ReqCparam1 = "CREATE" AND
      MsRequest.ReqCparam3 = {&DSS} THEN DO:
      lcMSISDNS = fParseKVP("MSISDNS",MsRequest.ReqCparam2,",").
      liNumEntries = NUM-ENTRIES(lcMSISDNS,";").

      DO liCount = 1 to liNumEntries:
         lcCLI = SUBSTRING(ENTRY(liCount,lcMSISDNS,";"),3). /*remove 34 prefix*/

         FIND FIRST MobSub WHERE
                    MobSub.Brand = Syst.Var:gcBrand AND
                    MobSub.CLI   = lcCLI NO-LOCK NO-ERROR.
         /* Exclude subs. who requested DSS activation */
         IF NOT AVAILABLE MobSub OR MobSub.CLI = MsRequest.CLI THEN NEXT.

         RUN pSendSMS(INPUT MobSub.MsSeq, INPUT 0, INPUT "DSSActOtherSubs",
                      INPUT 9, INPUT "622", INPUT "").
      END. /* DO liCount = 1 to liNumEntries: */
   END. /* IF MsRequest.ReqCparam1 = "CREATE" THEN DO: */

   /* Activate DSS2 if new criteria match 
      YTS-8140: To extend DELETE request matching also for DSS2 */
   IF MsRequest.ReqCparam3 BEGINS {&DSS} AND
      MsRequest.ReqCparam1 = "DELETE" AND
      fIsDSSActivationAllowed(MsRequest.CustNum,
                              0,
                              Func.Common:mMakeTS(),
                              {&DSS2},
                              OUTPUT liDSSPriMsSeq,
                              OUTPUT lcResult) AND
      NOT fIsDSSActive(MsRequest.CustNum,Func.Common:mMakeTS()) AND
      NOT fOngoingDSSAct(MsRequest.CustNum) THEN DO:

      FIND FIRST lbMobSub WHERE
                 lbMobSub.MsSeq = liDSSPriMsSeq NO-LOCK NO-ERROR.
      IF AVAIL lbMobSub THEN DO:
         liRequest = fDSSRequest(lbMobSub.MsSeq,
                                 lbMobSub.CustNum,
                                 "CREATE",
                                 "",
                                 "DSS2",
                                 Func.Common:mMakeTS(),
                                 {&REQUEST_SOURCE_DSS},
                                 "",
                                 TRUE, /* create fees */
                                 0,
                                 FALSE,
                                 OUTPUT lcResult).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            Func.Common:mWriteMemo("MobSub",
                             STRING(MsRequest.MsSeq),
                             MsRequest.Custnum,
                             "DSS2 activation failed in DSS handling",
                             lcResult).
         ELSE DO:
            /* Link New activation with old DSS termination */
            FIND FIRST bMsRequest WHERE
                       bMsRequest.MsRequest = liRequest
                 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bMsRequest THEN
               bMsRequest.ReqIParam2 = MsRequest.MsRequest.
         END.
      END.
   END.

   /* Terminate possible BB from additional lines */
   IF MsRequest.ReqCparam3 = "DSS2" AND
      MsRequest.ReqCparam1 = "DELETE" THEN
      RUN pHandleOtherServices(INPUT iiMsRequest).

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "".

   /* Create DSS FAT if MultiSIM is active*/
   IF MsRequest.ReqCparam3 = {&DSS} AND
      (MsRequest.ReqCparam1 EQ "CREATE" OR
      (MobSub.MultiSIMID > 0 AND MsRequest.ReqCParam1 EQ "ADD")) THEN DO:
   
      /* Check if any  multisim subscription pair is active */
      llMultiSimActive = FALSE.
      FOR EACH MobSub NO-LOCK WHERE
               MobSub.Custnum = MsRequest.Custnum AND
               MobSub.MultiSimID > 0,
         FIRST lbMobSub NO-LOCK USE-INDEX MultiSimID WHERE
               lbMobSub.Brand = Syst.Var:gcBrand AND
               lbMobSub.MultiSImID = Mobsub.MultiSImID AND
               lbMobSub.MultiSimType NE Mobsub.MultiSIMType AND
               lbMobSub.Custnum = Mobsub.Custnum:
         llMultiSimActive = TRUE.
         LEAVE.
      END.
      IF NOT llMultiSimActive THEN RETURN "".

      IF fGetDSSMsSeqLimit(MsRequest.Custnum,
                           ldeCurrentTS,
                           OUTPUT liDSSMsSeq,
                           OUTPUT ldeDSSLimit,
                           OUTPUT lcBundleId) EQ FALSE THEN RETURN "".
      
      Func.Common:mSplitTS(ldeCurrentTS, OUTPUT ldaDate, OUTPUT liTime).
      liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).
      
      IF NOT fFatExists("DSSCPFREE",
                        liDSSMsSeq,
                        MsRequest.Custnum,
                        liPeriod) THEN DO:

         RUN Mc/creafat.p(MsRequest.CustNum,
                       liDSSMsSeq,
                       "DSSCPFREE",
                       ?, /* amount */
                       0, /* percent */
                       ?, /* vat incl. */
                       liPeriod,
                       999999,
                       OUTPUT lcResult). 
         IF lcResult > "" THEN 
            Func.Common:mWriteMemo("MobSub",
                          STRING(liDSSMsSeq),
                          MsRequest.Custnum,
                          "Multi SIM DSS FAT creation failed",
                          lcResult).
      END.
   END. 

   RETURN "".
   
END PROCEDURE.

PROCEDURE pHandleOtherServices:

   DEF INPUT PARAMETER iiMsRequest  AS INT  NO-UNDO.

   DEF VAR ldeStamp                 AS DEC  NO-UNDO.
   DEF VAR liRequest                AS INT  NO-UNDO.
   DEF VAR lcError                  AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType    AS CHAR NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType    AS CHAR NO-UNDO.

   DEF BUFFER lbMobSub              FOR Mobsub.
   DEF BUFFER MsRequest             FOR MsRequest.

   EMPTY TEMP-TABLE ttAdditionalSIM NO-ERROR.

   FIND FIRST MsRequest WHERE
              MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest THEN RETURN "".

   ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          ldeStamp = MsRequest.ActStamp.

   FOR EACH lbMobSub WHERE
            lbMobSub.Brand   = Syst.Var:gcBrand AND
            lbMobSub.InvCust = MsRequest.CustNum AND
            LOOKUP(lbMobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
            (LOOKUP(lbMobSub.TariffBundle,lcDSS2PrimarySubsType) = 0 AND
             LOOKUP(lbMobSub.CLIType,lcDSS2PrimarySubsType) = 0) NO-LOCK:

      IF fGetActiveSpecificBundle(lbMobSub.MsSeq,ldeStamp,"BONO") > "" OR
         lbMobSub.TariffBundle = "CONTS16" OR
         lbMobSub.TariffBundle = "CONTS12" 
      THEN NEXT.

      RUN pChangedBBStatus(INPUT 2,
                           INPUT ldeStamp,
                           INPUT {&REQUEST_SOURCE_DSS},
                           BUFFER MsRequest,
                           BUFFER lbMobSub).

      CREATE ttAdditionalSIM.
      ASSIGN ttAdditionalSIM.MsSeq   = lbMobSub.MsSeq
             ttAdditionalSIM.CustNum = lbMobSub.CustNum
             ttAdditionalSIM.CLI     = lbMobSub.CLI.

   END. /* FOR EACH lbMobSub WHERE */

   EMPTY TEMP-TABLE ttAdditionalSIM NO-ERROR.

END PROCEDURE.


