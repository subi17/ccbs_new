/**
 * Get data bundle status 
 *
 * @input  int;mandatory;subscription id
           string;mandatory;bundle id (Bono Contracts and DSS) 
 * @output bundle status id;bundle status 
           0;inactive
           1;active
           2;pending deactivation
           3;pending activation
 * @Exceptions  1;MobSub not found
                2;Incorrect Bundle Id
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
Syst.Var:katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId. 
Syst.Var:gcBrand = "1".
{Mm/fbundle.i}
{Mm/active_bundle.i}

DEFINE VARIABLE piMsSeq     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE pcBundleId  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liStatus    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE ldEndStamp  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcReqChar   AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcBONOContracts AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int,string") = ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id,"0").
pcBundleId = get_string(param_toplevel_id,"1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

ASSIGN ldEndStamp = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(TODAY),86399)
       lcBONOContracts = fCParamC("BONO_CONTRACTS").

IF LOOKUP(pcBundleId,lcBONOContracts) = 0 AND
   pcBundleId <> {&DSS} THEN
   RETURN appl_err("Incorrect Bundle Id").

/* Check if subscription type is not compatible with bundle */
IF fMatrixAnalyse(Syst.Var:gcBrand,
                  "PERCONTR",
                  "PerContract;SubsTypeTo",
                  pcBundleId + ";" + MobSub.CLIType,
                  OUTPUT lcReqChar) NE 1 THEN DO:
   RETURN appl_err("Incorrect Bundle Id").
END.

liStatus = 0. /* deactivated */

/* if exist any MDUB valid to the future then service is activated */   
FOR EACH ServiceLimitGroup NO-LOCK WHERE 
         ServiceLimitGroup.Brand     = Syst.Var:gcBrand AND
         ServiceLimitGroup.GroupCode = pcBundleId,
    EACH ServiceLimit NO-LOCK WHERE 
         ServiceLimit.GroupCode  = pcBundleId AND 
         ServiceLimit.ValidFrom <= TODAY AND 
         ServiceLimit.ValidTo   >= TODAY:

   IF pcBundleId = {&DSS} THEN DO:
      IF CAN-FIND(FIRST MServiceLimit WHERE 
                  MServiceLimit.Custnum  = MobSub.Custnum        AND
                  MServiceLimit.DialType = ServiceLimit.DialType AND
                  MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                  MServiceLimit.EndTS    > ldEndStamp)
      THEN liStatus = 1.
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
      THEN liStatus = 1.
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

/* pending request for bundle termination */
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

add_int(response_toplevel_id, "", liStatus).

FINALLY:
   END.
