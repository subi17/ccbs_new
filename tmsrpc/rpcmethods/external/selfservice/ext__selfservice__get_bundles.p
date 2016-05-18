/**
 * Get data bundle status 
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;subscription msisdn number
           bundle_id;string;mandatory;bundle id (eg: BONO Contracts/DSS) 
 * @output    struct;mandatory;response struct
 * @response  transaction_id;string;transaction id
           bundle_status;int
           inactive;0
           active;1
           pending deactivation;2
           pending activation;3
 * @exceptions  1;Subscription not found
                2;Incorrect Bundle Id
                3;Bundle is not allowed for this subscription type
                4;Application Id does not match
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{Syst/commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/matrix.i}
{Func/fdss.i}
{Func/fexternalapi.i}

DEFINE VARIABLE pcCLI           AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcBundleId      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcTransId       AS CHARACTER NO-UNDO.
DEFINE VARIABLE top_struct      AS CHARACTER NO-UNDO.

DEFINE VARIABLE liStatus        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldEndStamp      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcBONOContracts AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string") = ? THEN RETURN.

ASSIGN pcTransId = get_string(param_toplevel_id, "0")
       pcCLI = get_string(param_toplevel_id,"1")
       pcBundleId = get_string(param_toplevel_id,"2")
       lcBONOContracts = fCParamC("BONO_CONTRACTS").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NOT fchkTMSCodeValues(gbAuthLog.UserName,substring(pcTransId,1,3)) THEN
   RETURN appl_err("Application Id does not match").

FIND FIRST MobSub  WHERE 
           MobSub.CLI = pcCLI NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN appl_err("Subscription not found").

IF LOOKUP(pcBundleId,lcBONOContracts) = 0 AND
   pcBundleId <> {&DSS} THEN
   RETURN appl_err("Incorrect Bundle Id").

/* Check if subscription type is not compatible with bundle */
IF fMatrixAnalyse(gcBrand,
                  "PERCONTR",
                  "PerContract;SubsTypeTo",
                  pcBundleId + ";" + MobSub.CLIType,
                  OUTPUT lcReqChar) NE 1 THEN DO:
   RETURN appl_err("Bundle is not allowed for this subscription type").
END.

ASSIGN ldEndStamp = fMake2Dt(fLastDayOfMonth(TODAY),86399)
       liStatus   = 0. /* deactivated */

/* if exist any MDUB valid to the future then service is activated */   
FOR EACH ServiceLimitGroup NO-LOCK WHERE 
         ServiceLimitGroup.Brand     = gcBrand AND
         ServiceLimitGroup.GroupCode = pcBundleId,
    EACH ServiceLimit NO-LOCK WHERE 
         ServiceLimit.GroupCode  = pcBundleId AND 
         ServiceLimit.ValidFrom <= TODAY AND 
         ServiceLimit.ValidTo   >= TODAY:

      /* Customer level contract */
      IF pcBundleId = {&DSS} THEN DO:
         IF CAN-FIND(FIRST MServiceLimit WHERE 
                     MServiceLimit.Custnum  = MobSub.Custnum        AND
                     MServiceLimit.DialType = ServiceLimit.DialType AND
                     MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                     MServiceLimit.EndTS    > ldEndStamp)
         THEN liStatus = 1. /* activated */
         ELSE IF CAN-FIND(FIRST MServiceLimit WHERE 
                          MServiceLimit.Custnum  = MobSub.Custnum        AND
                          MServiceLimit.DialType = ServiceLimit.DialType AND
                          MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                          MServiceLimit.EndTS    = ldEndStamp)
         THEN liStatus = 2. /* cancelled ongoing */
      END. /* IF pcBundleId = {&DSS} THEN DO: */

      ELSE DO:
         IF CAN-FIND(FIRST MServiceLimit WHERE 
                           MServiceLimit.MSSeq    = MobSub.MsSeq          AND
                           MServiceLimit.DialType = ServiceLimit.DialType AND
                           MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                           MServiceLimit.EndTS    > ldEndStamp)
         THEN liStatus = 1 . /* activated */
         ELSE IF CAN-FIND(FIRST MServiceLimit WHERE 
                          MServiceLimit.MSSeq    = MobSub.MsSeq          AND
                          MServiceLimit.DialType = ServiceLimit.DialType AND
                          MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                          MServiceLimit.EndTS    = ldEndStamp)
         THEN liStatus = 2. /* cancelled ongoing */
      END. /* ELSE DO: */
END.

IF pcBundleId = {&DSS} THEN DO:
   /* ongoing DSS cancellation */
   IF fOngoingDSSTerm(MobSub.Custnum,ldEndStamp) THEN liStatus = 2.

   /* ongoing DSS activation */
   ELSE IF fOngoingDSSAct(MobSub.Custnum) THEN liStatus = 3.
END. /* IF pcBundleId = {&DSS} THEN DO: */

/* pending request for MDUB termination */
ELSE IF CAN-FIND(
      FIRST MsRequest WHERE
            MsRequest.MsSeq = MobSub.MsSeq AND
            MsRequest.ReqType = ({&REQTYPE_CONTRACT_TERMINATION}) AND
            MsRequest.ReqCParam3 = pcBundleId AND
            LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) THEN 
      liStatus = 2. /* pending cancellation */

/* pending request for bundle activation */
ELSE IF CAN-FIND(FIRST MsRequest WHERE
                       MsRequest.MsSeq      = MobSub.MsSeq  AND
                       MsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
                       MsRequest.ReqCParam3 = pcBundleId AND
                       LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
                       USE-INDEX MsSeq) THEN
      liStatus = 3. /* activation ongoing */

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_int(top_struct, "bundle_status", liStatus).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
