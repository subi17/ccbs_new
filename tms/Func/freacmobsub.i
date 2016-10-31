/* ----------------------------------------------------------------------
  Module .......: Func/freacmobsub.i
  Task .........: Pre-validations and create MsRequest for Reactivation
                  Subscription
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.10.11
  Version ......: Yoigo
---------------------------------------------------------------------- */
&IF "{&fcreamobsub}" NE "YES"
&THEN
&GLOBAL-DEFINE fcreamobsub YES
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/fcreatereq.i}
{Func/fixedlinefunc.i}

FUNCTION freacprecheck RETURNS CHARACTER
   (INPUT  iiMsSeq        AS INTEGER,    /* Subscription ID    */
    INPUT  icUserName     AS CHARACTER,  /* User Name          */
    INPUT  ilSkipCheck    AS LOGICAL):   /* Skip Validation    */

   DEFINE BUFFER bTermMobSub    FOR termMobSub.
   DEFINE BUFFER bMsowner       FOR Msowner.
   DEFINE BUFFER bMsReacReq     FOR MsRequest.
   DEFINE BUFFER bMsRequest     FOR MsRequest.
   DEFINE BUFFER bMobSub        FOR MobSub.
   DEFINE BUFFER CLIType        FOR CLiType.

   DEFINE VARIABLE ldTermDate   AS DATE     NO-UNDO.
   DEFINE VARIABLE liTermTime   AS INTEGER  NO-UNDO.
   DEFINE VARIABLE liReacDays   AS INTEGER  NO-UNDO.

   DEFINE VARIABLE llPrimaryActive         AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE liReactMsseq            AS INTEGER   NO-UNDO.

   IF icUserName = "" OR icUserName = ? THEN
      RETURN "Username is empty".

   liReacDays = fCParamI("ReacDays").
   IF liReacDays = ? OR liReacDays = 0 THEN
      liReacDays = 30.

   IF CAN-FIND(FIRST MobSub WHERE MobSub.MsSeq = iiMsSeq) THEN
      RETURN "Subscription is already active".
   ELSE DO:
      FIND FIRST bTermMobSub WHERE bTermMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bTermMobSub THEN
         RETURN "Invalid Subscription Id".
   END. /* ELSE DO: */
   
  /*YPR-4770*/ 
  /*reactivation is not allowed for convergent tariffs.*/
  IF fIsConvergenceTariff(bTermMobSub.CLIType) THEN 
     RETURN "Not allowed for fixed line tariffs".

   /* Check that no other reactivation requests is under work */
   FIND FIRST bMsReacReq WHERE
              bMsReacReq.MsSeq   = iiMsSeq      AND
              bMsReacReq.ReqType = {&REQTYPE_SUBSCRIPTION_REACTIVATION} AND
              LOOKUP(STRING(bMsReacReq.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
        NO-LOCK NO-ERROR.
   IF AVAIL bMsReacReq AND NOT ilSkipCheck THEN
      RETURN 
      "There is already a scheduled reactivation request"             + CHR(10) +
      "for Mobile Subscription " + bTermMobSub.CLI                    + CHR(10) +
      "Saved by user '" + bMsReacReq.UserCode + "'"                   + CHR(10) +
      "Proposed time of reactivation " + fTS2HMS(bMsReacReq.ActStamp) + CHR(10) +
      "Status " + STRING(bMsReacReq.ReqStatus).
   
   FIND FIRST bMsowner WHERE 
              bMsowner.MsSeq = bTermMobSub.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bMsowner THEN
      RETURN "MsOwner record not found".

   fSplitTS(INPUT bMsowner.tsend, OUTPUT ldTermDate, OUTPUT liTermTime).
   IF today > (ldTermDate + liReacDays) AND NOT ilSkipCheck THEN DO:
      /* YOT-4715, reactivation over 30 days was not possible */
      ASSIGN liReactMsseq = fCParamI("ReactMsseq").
      IF btermmobsub.msseq EQ liReactMsseq THEN DO: 
         /* Bypass this one MsSeq only once and remove value from Cparam */
         FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
                    TMSParam.Brand     = gcBrand AND
                    TMSParam.ParamCode = "ReactMsseq" NO-ERROR.
         IF AVAILABLE TMSParam THEN DO:
            ASSIGN TMSParam.IntVal = -1.
            RELEASE TMSParam.
         END.
      END.
      ELSE DO: /* In normal case, return error */
            RETURN "Subscription reactivation is not allowed after " +
                   STRING(liReacDays) + " days of termination".
      END.
   END.
   
   IF CAN-FIND (FIRST mobsub where mobsub.cli = bTermMobSub.cli) THEN
      RETURN "Subscription is already active with same MSISDN".

   IF NOT CAN-FIND (FIRST MSISDN where 
                          MSISDN.Brand = gcBrand AND
                          MSISDN.CLI   = bTermMobSub.CLI) THEN
      RETURN "MSISDN not found".

   FIND FIRST IMSI where IMSI.IMSI = bTermMobSub.IMSI NO-LOCK NO-ERROR.
   IF NOT AVAIL IMSI THEN
      RETURN "IMSI not found".

   FIND FIRST SIM where SIM.ICC = bTermMobSub.ICC NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SIM THEN RETURN "SIM not found".
   ELSE DO:
      FIND FIRST mobsub WHERE
                 mobsub.brand = gcBrand AND
                 mobsub.imsi = imsi.imsi NO-LOCK NO-ERROR.
      IF AVAIL mobsub THEN RETURN "SIM is already in use".
   END.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand = gcBrand AND
              CLIType.CLIType = (IF bTermMobsub.TariffBundle > ""
                                 THEN bTermMobsub.TariffBundle
                                 ELSE bTermMobsub.CLIType) NO-ERROR.
   /* Block Additional line reactivation if primary line is not active */
   IF AVAIL CLIType AND 
            CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL} THEN DO:

      FOR EACH bMobSub NO-LOCK WHERE
               bMobSub.Brand   = gcBrand AND
               bMobSub.InvCust = bTermMobSub.CustNum AND
               bMobSub.PayType = FALSE,
         FIRST CLIType NO-LOCK WHERE
               CLIType.Brand = gcBrand AND
               CLIType.CLIType = bMobSub.TariffBundle AND
               CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}:
         llPrimaryActive = TRUE.
         LEAVE.
      END. /* FOR EACH bMobSub WHERE */
      IF NOT llPrimaryActive THEN DO:
         ASSIGN liReactMsseq = fCParamI("ReactMsseq").
         IF btermmobsub.msseq EQ liReactMsseq THEN DO:
         /* Bypass this one MsSeq only once and remove value from Cparam */
            FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
                       TMSParam.Brand     = gcBrand AND
                       TMSParam.ParamCode = "ReactMsseq" NO-ERROR.
            IF AVAILABLE TMSParam THEN DO:
               ASSIGN TMSParam.IntVal = -1.
               RELEASE TMSParam.
            END.
         END.
         ELSE DO: /* In normal case return error */
            RETURN "Additional line reactivation is not allowed " +
                   "since main line is not active".
         END.
      END.
   END.

END FUNCTION. /* FUNCTION freacprecheck RETURNS CHARACTER */

FUNCTION fReactivationRequest RETURNS INTEGER
   (INPUT  iiMsSeq        AS INTEGER,    /* subscription id                */
    INPUT  iiOrderId      AS INTEGER,    /* Order Id                       */
    INPUT  idActStamp     AS DECIMAL,    /* when request should be handled */
    INPUT  icUserName     AS CHARACTER,  /* who made the request           */
    INPUT  icReqSource    AS CHARACTER,
    OUTPUT ocResult       AS CHARACTER):

   DEFINE VARIABLE liReqCreated     AS INTEGER NO-UNDO.
   DEFINE VARIABLE llCancelMNPBaja  AS LOGICAL NO-UNDO.

   DEFINE BUFFER   bTermMobSub  FOR TermMobSub.

   FIND FIRST bTermMobSub WHERE
              bTermMobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bTermMobSub THEN DO:
      ocResult = "Invalid Subscription Id".
      RETURN 0.
   END. /* IF NOT AVAILABLE bTermMobSub THEN DO: */

   IF fPendingRequest(INPUT iiMsSeq, INPUT {&REQTYPE_SUBSCRIPTION_REACTIVATION})
   THEN DO:
      ocResult = "Reactivation Subscription Request is already pending".
      RETURN 0.
   END. /* IF fPendingRequest(INPUT iiMsSeq, INPUT 82) THEN DO: */

   /* only check with reactivation */
   IF iiOrderId EQ 0 THEN
   FOR EACH MNPSub WHERE
            MNPSub.MsSeq = bTermMobSub.MsSeq NO-LOCK,
       EACH MNPProcess WHERE
            MNPProcess.MNPSeq  = MNPSub.MNPSeq AND
            MNPProcess.MNPType = {&MNP_TYPE_TERMINATION} NO-LOCK:

      IF MNPProcess.StatusCode = {&MNP_ST_NEW} THEN DO:
         ocResult = "MNP Baja process is in error".
         RETURN 0.
      END.
      ELSE IF MNPProcess.StatusCode = {&MNP_ST_BDET} THEN DO:
         ocResult = "MNP Baja process is on hold".
         RETURN 0.
      END.
      ELSE IF MNPProcess.StatusCode = {&MNP_ST_BDEF} THEN DO:
         ocResult = "MSISDN already returned to original operator".
         RETURN 0.
      END.
      ELSE IF MNPProcess.StatusCode = {&MNP_ST_BNOT} THEN DO:
         RUN mnp_operation.p(MNPProcess.MNPSeq,
                             "cancelarSolicitudBajaNumeracionMovil","").
         IF RETURN-VALUE NE "OK" THEN DO:
            IF RETURN-VALUE BEGINS "ERROR:" THEN
               ocResult = "MNP Baja cancellation request failed".
            ELSE ocResult = RETURN-VALUE.
            RETURN 0.
         END.
         ELSE llCancelMNPBaja = TRUE.
         LEAVE.
      END.
   END. /* FOR EACH MNPSub WHERE */

   fCreateRequest({&REQTYPE_SUBSCRIPTION_REACTIVATION},
                  idActStamp,
                  icUserName,
                  FALSE,    /* create fees */
                  FALSE).   /* sms */
  
   ASSIGN
      bCreaReq.MsSeq       = bTermMobSub.MsSeq
      bCreaReq.CLI         = bTermMobSub.CLI
      bCreaReq.CustNum     = bTermMobSub.CustNum
      bCreaReq.ReqCParam1  = "REACTIVATE"
      bCreaReq.ReqIParam1  = iiOrderId
      bCreaReq.OrigReq     = 0
      bCreaReq.ReqSource   = icReqSource
      liReqCreated         = bCreaReq.MsRequest.

   /* Waiting MNP Baja process to cancel */
   IF llCancelMNPBaja THEN
      bCreaReq.ReqStatus = {&REQUEST_STATUS_CONFIRMATION_PENDING}.

   RELEASE bCreaReq.

   RETURN liReqCreated.

END FUNCTION. /* FUNCTION fReactivationRequest RETURNS INTEGER */

&ENDIF
