/**
 * Cancel acc, icc, request type change or dupliacte invoice request
 *
 * @input   brand;string;mandatory
            seqval;int;mandatory;Msseq
            salesman;string;mandatory;login id
            reqtype;string;mandatory;For options see below
            bundle_id;string;optional;if type=bundle_terminationss
            confirm_icc;boolean;optional;confirm icc status change
 * @output requests;int;number of canceled requests
 *
 * @RequestType subscription_type;Subscription type change
                acc;Agreement customer change
                icc;ICC change
                duplicate_invoice;Duplicate invoice
                bundle_type_change;Bundle Type Change
                bundle_termination;Bundle/Contract Termination
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Func/fsendsms.i}
{Func/fsubstermreq.i}
{Func/add_lines_request.i}

/* Input parameters */
DEF VAR pcTenant    AS CHAR NO-UNDO.
DEF VAR piReference AS INT NO-UNDO.
DEF VAR pcMemo AS CHAR NO-UNDO.
DEF VAR pcReqType AS CHAR NO-UNDO.
DEF VAR pcBundleName AS CHAR NO-UNDO.
DEF VAR plConfirm AS LOG NO-UNDO.
DEF VAR top_array AS CHAR NO-UNDO. 
/* Local variables */
DEF VAR liReqType AS INT NO-UNDO.
DEF VAR lii AS INT NO-UNDO.
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR ldeActStamp AS DEC NO-UNDO.
DEF VAR liQuarTime         AS INT   NO-UNDO.
DEF VAR liSimStat          AS INT   NO-UNDO.
DEF VAR liMSISDNStat       AS INT   NO-UNDO.
DEF VAR liRequest          AS INT   NO-UNDO.
DEF VAR lcError            AS CHAR  NO-UNDO.
DEF VAR ldaSecSIMTermDate  AS DATE  NO-UNDO.
DEF VAR liSecSIMTermTime   AS INT   NO-UNDO.
DEF VAR ldeSecSIMTermStamp AS DEC   NO-UNDO.
DEF VAR lcBONOContracts    AS CHAR  NO-UNDO.
DEF VAR lcVoiceBundles     AS CHAR  NO-UNDO.
DEF VAR lcIPLContracts     AS CHAR  NO-UNDO.

DEF BUFFER lbMobSub        FOR Mobsub.

DEF TEMP-TABLE ttAdditionalSIM NO-UNDO
    FIELD MsSeq    AS INT
    FIELD CustNum  AS INT
    FIELD CLI      AS CHAR.

/* Output parameters */
DEF VAR liReqCount AS INT INITIAL 0 NO-UNDO.

top_array = validate_request(param_toplevel_id, "string!,int,string,string,[string],[boolean]").
IF top_array EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
piReference = get_int(param_toplevel_id, "1").
pcMemo = "Newton user " + get_string(param_toplevel_id, "2") + " canceled".
pcReqType = get_string(param_toplevel_id, "3").

IF pcReqType EQ "bundle_termination" THEN
    pcBundleName = get_string(param_toplevel_id, "4").

IF NUM-ENTRIES(top_array) >= 5 THEN
   plConfirm = get_bool(param_toplevel_id, "5").

Syst.Var:katun = "VISTA_" + get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

CASE pcReqType:
   WHEN  "subscription_type" THEN liReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}.
   WHEN  "icc" THEN liReqType = {&REQTYPE_ICC_CHANGE}.
   WHEN  "acc" THEN liReqType = {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE}.
   WHEN  "stc" THEN liReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}.
   WHEN  "duplicate_invoice" THEN liReqType = {&REQTYPE_DUPLICATE_INVOICE}.
   WHEN  "bundle_type_change" THEN liReqType = {&REQTYPE_BUNDLE_CHANGE}.
   WHEN  "bundle_termination" THEN liReqType = {&REQTYPE_CONTRACT_TERMINATION}.
   WHEN  "address" THEN liReqtype = {&REQTYPE_ADDRESS_CHANGE}.
   OTHERWISE RETURN appl_err("Unknown request type " + pcReqType).
END.


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMsRequest AS HANDLE NO-UNDO.
   lhMsRequest = BUFFER MsRequest:HANDLE.
   RUN StarEventInitialize(lhMsRequest).

END.

IF liReqType = {&REQTYPE_ICC_CHANGE} THEN DO:
    FOR EACH MsRequest NO-LOCK
    WHERE MsRequest.Brand     = Syst.Var:gcBrand 
      AND MsRequest.ReqType   = liReqType
      AND MsRequest.msseq     = piReference
      AND (MsRequest.ReqStatus = 0
        OR MsRequest.ReqStatus = 19
        OR MsRequest.ReqStatus = 20) USE-INDEX MsSeq:
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).
         fReqStatus(4, pcMemo).
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).
         liReqCount = liReqCount + 1.
         FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                    SIM.icc = Msrequest.ReqCParam2 NO-ERROR.
         IF AVAIL SIM THEN DO:
            FIND FIRST Order NO-LOCK USE-INDEX MsSeq WHERE
                       Order.MsSeq = MsRequest.MsSeq AND
                       Order.ICC = SIM.ICC AND
                       Order.OrderType = 2 AND
                       INDEX(Order.OrderChannel,"pos") = 0 NO-ERROR.
            IF AVAIL Order THEN sim.simstat = 7.
            ELSE SIM.simstat = (IF plConfirm THEN 1 ELSE 9).
            RELEASE SIM.
         END.
    END.
END.
ELSE FOR EACH MsRequest NO-LOCK WHERE 
         MsRequest.Brand = Syst.Var:gcBrand AND
         MsRequest.MsSeq = piReference AND
         MsRequest.ReqType = liReqType USE-INDEX MsSeq:
 
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsRequest).
   CASE pcReqType:
      WHEN "subscription_type" THEN
         IF fChkReqStatusChange(4) EQ TRUE AND 
            MsRequest.ActStamp > Func.Common:mMakeTS() THEN DO:
            /* cancel possible renewal pos stc order */
            FIND FIRST Order WHERE
               Order.MsSeq = MsRequest.MsSeq AND
               LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 AND
               LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES} + ",12") = 0
               NO-LOCK NO-ERROR.
            IF AVAIL Order THEN DO TRANS ON ERROR UNDO:
               RUN Mc/closeorder.p(Order.OrderId,TRUE).
               IF RETURN-VALUE NE "" THEN NEXT.
               IF fReqStatus(4, pcMemo) = FALSE THEN UNDO.
               ELSE liReqCount = liReqCount + 1.
            END.
            ELSE DO:
               fReqStatus(4, pcMemo).
               liReqCount = liReqCount + 1.

               /* if additional line to non-additional line pending STC is cancellled 
                  and it doesn't contain any main line then STC request has to be created
                  for additional line to CONT9*/
               fNonAddLineSTCCancellationToAddLineSTC(MsRequest.MsRequest). 
            END.

            FIND Customer WHERE
                 Customer.Custnum = MsRequest.Custnum NO-LOCK NO-ERROR.

            lcSMSText = fGetSMSTxt("STC_Cancelled",
                                   TODAY,
                                   (IF AVAIL Customer THEN Customer.Language
                                    ELSE 1),
                                   OUTPUT ldeActStamp).

            IF lcSMSText > "" THEN
               fMakeSchedSMS2(MsRequest.CustNum,
                             MsRequest.CLI,
                             {&SMSTYPE_STC},
                             lcSMSText,
                             ldeActStamp,
                             {&STC_SMS_SENDER},
                             "").

            /* YDR-819 - Create CONTM termination request - MultiSIM */
            FIND FIRST MobSub WHERE
                       MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
            IF AVAIL MobSub AND MobSub.MultiSIMId > 0 AND
               MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
               FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                          lbMobSub.Brand  = Syst.Var:gcBrand AND
                          lbMobSub.MultiSimID = MobSub.MultiSimID AND
                          lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                          lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
               IF NOT AVAIL lbMobSub THEN DO:
                  FIND FIRST TermMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                             TermMobSub.Brand  = Syst.Var:gcBrand AND
                             TermMobSub.MultiSimID = MobSub.MultiSimID AND
                             TermMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                             TermMobSub.Custnum = MobSub.Custnum NO-ERROR.
                  IF NOT AVAIL TermMobSub THEN
                     ldaSecSIMTermDate = TODAY.
                  ELSE DO:
                     FIND FIRST Msowner WHERE
                                Msowner.MsSeq = TermMobsub.MsSeq
                          NO-LOCK NO-ERROR.
                     IF AVAIL Msowner THEN
                        Func.Common:mSplitTS(Msowner.TSEnd,OUTPUT ldaSecSIMTermDate,
                                 OUTPUT liSecSIMTermTime).
                     ELSE ldaSecSIMTermDate = TODAY.
                  END. /* ELSE DO: */

                  fTermAdditionalSim(MobSub.MsSeq,
                                     MobSub.CLI,
                                     MobSub.CustNum,
                                     {&SUBSCRIPTION_TERM_REASON_MULTISIM},
                                     ldaSecSIMTermDate,
                                     {&REQUEST_SOURCE_MANUAL_TMS},
                                     0,
                                     lcError).

               END. /* IF NOT AVAIL lbMobSub THEN DO: */
            END. /* IF AVAIL MobSub THEN DO: */
            /* Additional SIM STC cancellation logic */
            ELSE IF AVAIL MobSub THEN DO:
               fAddLineSTCCancellation(MsRequest.MsRequest, 
                                       MsRequest.CustNum). 
            END. /* ELSE IF AVAIL MobSub THEN DO: */
         END.
      WHEN "acc" THEN
         IF MsRequest.ReqStatus = 0 OR 
            MsRequest.ReqStatus = 8 THEN DO:
            fReqStatus(4, pcMemo).
            fChangeOrderStatus(MsRequest.ReqIParam4, {&ORDER_STATUS_CLOSED}).
            RUN Mm/acc_sendsms.p(MsRequest.MsRequest,
                            MsRequest.CustNum,
                            "Cancelled",
                            "").
            liReqCount = liReqCount + 1.
         END.
      WHEN "duplicate_invoice" OR WHEN "address" THEN
         IF MsRequest.ReqStatus = 0 THEN DO:
            fReqStatus(4, pcMemo).
            liReqCount = liReqCount + 1.
         END.
      WHEN "bundle_type_change" THEN
         IF LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
         THEN DO:
            ASSIGN lcIPLContracts  = fCParamC("IPL_CONTRACTS")
                   lcBONOContracts = fCParamC("BONO_CONTRACTS")
                   lcVoiceBundles  = fCParamC("VOICE_BONO_CONTRACTS").

            /* BTC with Upgrade Upsell cancellation is not allowed */
            IF MsRequest.ReqCparam5 > "" THEN 
            DO:
               IF LOOKUP(MsRequest.ReqCParam1,lcBONOContracts) > 0 THEN
                  RETURN appl_err("BONO Upgrade BTC Cancellation is not allowed").
               ELSE IF LOOKUP(MsRequest.ReqCParam1,lcVoiceBundles) > 0 THEN   
                  RETURN appl_err("Voice Bundle Upgrade BTC Cancellation is not allowed").
               ELSE IF LOOKUP(MsRequest.ReqCParam1,lcIPLContracts) > 0 THEN
                  RETURN appl_err("IPL Upgrade BTC Cancellation is not allowed").
            END. /* IF MsRequest.ReqCparam5 > "" THEN DO: */

            fReqStatus(4,pcMemo).
            liReqCount = liReqCount + 1.
            /* Send a SMS only for Voice BTC Cancellation */
            IF (LOOKUP(MsRequest.ReqCParam1,lcBONOContracts) > 0 AND LOOKUP(MsRequest.ReqCParam2,lcBONOContracts) > 0) OR 
               (LOOKUP(MsRequest.ReqCParam1,lcVoiceBundles ) > 0 AND LOOKUP(MsRequest.ReqCParam2,lcVoiceBundles ) > 0) THEN
               RUN pSendSMS(INPUT MsRequest.MsSeq, 
                            INPUT MsRequest.MsRequest,
                            INPUT "BTCDeAct", 
                            INPUT 10,
                            INPUT {&UPSELL_SMS_SENDER}, 
                            INPUT "").

            /* if additional line to non-additional line pending BTC is cancelled 
               and it doesn't contain any main line then STC request has to be created
               for additional line to CONT9*/
            fNonAddLineSTCCancellationToAddLineSTC(MsRequest.MsRequest). 

            /* If cancelled BTC is doesn't contain any pending additional line to non-additional line,
               then we have to cancel additional lines for the mainline  */
            fAddLineSTCCancellation(MsRequest.MsRequest, 
                                    MsRequest.CustNum).

         END. /* IF LOOKUP(STRING(MsRequest.ReqStatus) */
      WHEN "bundle_termination" THEN
         IF MsRequest.ReqStatus = 0 AND
            MsRequest.ReqCparam3 = pcBundleName THEN DO:
            fReqStatus(4, pcMemo).
            liReqCount = liReqCount + 1.
         END. /* IF MsRequest.ReqStatus = 0 */
   END. 
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsRequest).
END.

add_int(response_toplevel_id, "", liReqCount).

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
   END.
