/**
 * Request a new subscription type.
 *
 * Parameters are in the toplevel array and the meaning for them are:
 * @input msisdn;string;mandatory;subscription msisdn number
          username;string;mandatory;person who requests the change
          subscription_type_id;string;mandatory;new subscription type
          activation_stamp;datetime;mandatory;request activation time
          charge;decimal;mandatory;0 if no charged
          charge_limit;decimal;mandatory;
          contract_id;string;optional;contract ID
          bank_account;string;optional;new bank account for postpaid
          data_bundle_id;string;(mandatory);bundle type (Voice->Data STC)
          renewal_stc;boolean;optional;not in use
          bypass;boolean;optional;Allow STC for retired subscription types
          extend_term_contract;boolean;optional;false=terminate,true=extent
          exclude_term_penalty (boolean, optional) * To accept the penalty exemption
          memo;struct;optional;
          merge_with:string;optional;
 * @memo  title;string;mandatory;memo title
          contents;string;mandatory;memo content
 * @output success;boolean
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "Newton".

{Func/fbankdata.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/fcharge_comp_loaded.i}
{Syst/tmsconst.i}
{Func/profunc.i}
{Func/extralinefunc.i}

/* Input parameters */
DEF VAR pcTenant             AS CHAR NO-UNDO.
DEF VAR pcMSISDN             AS CHAR NO-UNDO.
DEF VAR pcSalesman           AS CHAR NO-UNDO.
DEF VAR pcCliType            AS CHAR NO-UNDO.
DEF VAR pdActivation         AS DEC  NO-UNDO.
DEF VAR pdeCharge            AS DEC  NO-UNDO. 
DEF VAR pdeChargeLimit       AS DEC  NO-UNDO. 
DEF VAR pcBankAcc            AS CHAR NO-UNDO.
DEF VAR pcDataBundleId       AS CHAR NO-UNDO. 
DEF VAR plByPass             AS LOG  NO-UNDO. 
DEF VAR plExtendContract     AS LOG  NO-UNDO.
DEF VAR plExcludeTermPenalty AS LOG  NO-UNDO.
DEF VAR pcMemoStruct         AS CHAR NO-UNDO.
DEF VAR pcMemoTitle          AS CHAR NO-UNDO.
DEF VAR pcMemoContent        AS CHAR NO-UNDO.
DEF VAR pcContractID         AS CHAR NO-UNDO.
DEF VAR pcChannel            AS CHAR NO-UNDO.
DEF VAR lcProValidation      AS CHAR NO-UNDO.
DEF VAR pcMergeWith          AS CHAR NO-UNDO.

/* Local variables */
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR liCreditCheck    AS INT  NO-UNDO INIT 1.
DEF VAR llCreateFees     AS LOG  NO-UNDO INIT FALSE.
DEF VAR llSendSMS        AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcInfo           AS CHAR NO-UNDO.
DEF VAR ok               AS LOG  NO-UNDO.
DEF VAR lcPCDenyCT       AS CHAR NO-UNDO. 
DEF VAR lcTiePeriod      AS CHAR NO-UNDO. 
DEF VAR lcError          AS CHAR NO-UNDO.
DEF VAR liRequest        AS INT  NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR iiRequestFlags   AS INT  NO-UNDO.
DEF VAR liSTCMsSeq       AS INT  NO-UNDO.
DEF VAR liMergeMsSeq     AS INT  NO-UNDO.
DEF VAR llgMerge         AS LOG  NO-UNDO INIT FALSE.

DEF BUFFER NewCliType   FOR CliType.
DEF BUFFER bMergeMobSub FOR MobSub.
DEF BUFFER bChkMobSub   FOR MobSub.

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
/* web is passing renewal_stc but we don't actually need it */
lcstruct = validate_struct(pcStruct, 
   "brand!,msisdn!,username!,subscription_type_id!,activation_stamp!,charge!," +
   "charge_limit!,bank_account,data_bundle_id,renewal_stc,bypass," +
   "extend_term_contract,exclude_term_penalty,memo,contract_id,channel,merge_with").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN pcTenant       = get_string(pcStruct, "brand")
       pcMSISDN       = get_string(pcStruct, "msisdn")
       pcSalesman     = get_string(pcStruct, "username")
       pcCliType      = get_string(pcStruct, "subscription_type_id")
       pdActivation   = get_timestamp(pcStruct, "activation_stamp")
       pdeCharge      = get_double(pcStruct, "charge")
       pdeChargeLimit = get_double(pcStruct, "charge_limit").

{newton/src/settenant.i pcTenant}

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

ASSIGN pcBankAcc            = get_string(pcStruct, "bank_account") 
                                 WHEN LOOKUP("bank_account", lcstruct) > 0
       pcDataBundleId       = get_string(pcStruct, "data_bundle_id")
                                 WHEN LOOKUP(pcCliType,lcBundleCLITypes) > 0
       plByPass             = get_bool(pcStruct, "bypass") 
                                 WHEN LOOKUP("bypass", lcstruct) > 0
       plExtendContract     = get_bool(pcStruct,"extend_term_contract")
                                 WHEN LOOKUP("extend_term_contract", lcstruct) > 0
       pcContractID         = get_string(pcStruct,"contract_id")
                                 WHEN LOOKUP("contract_id", lcstruct) > 0
       pcChannel            = get_string(pcStruct,"channel")
                                 WHEN LOOKUP("channel", lcstruct) > 0
       plExcludeTermPenalty = get_bool(pcStruct,"exclude_term_penalty")
                                 WHEN LOOKUP("exclude_term_penalty", lcstruct) > 0
       pcMergeWith          = get_string(pcStruct,"merge_with")
                                 WHEN LOOKUP("merge_with", lcStruct) > 0.

IF LOOKUP("memo", lcstruct) > 0 THEN DO:
   pcMemoStruct = get_struct(pcStruct,"memo").
   ASSIGN
      pcMemoTitle   = get_string(pcMemoStruct,"title")
      pcMemoContent = get_string(pcMemoStruct,"content").
END. /* IF LOOKUP("memo", lcstruct) > 0 THEN DO: */

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF plExtendContract AND plExcludeTermPenalty THEN
   RETURN appl_err("Both 'Contract extension' and 'Penalty exemption' requested").
   
Syst.Var:katun = "VISTA_" + pcSalesman.

IF TRIM(Syst.Var:katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND mobsub NO-LOCK WHERE
     mobsub.cli = pcMSISDN NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", pcMSISDN)).

/* Set the Syst.Var:katun to check correct barring */
Syst.Var:katun = "NewtonAd".

IF fCLITypeIsExtraLine(pcCliType)                       AND
   NOT fValidateExtraLineSTC(MobSub.CustNum, pcCliType) THEN 
   RETURN appl_err("Mainline not available for the La Duo").

IF pcMergeWith GT "" AND
   pcMergeWith NE ?  THEN
   llgMerge = TRUE.

/* Set the Syst.Var:katun again with original username */
Syst.Var:katun = "VISTA_" + pcSalesman.

IF fValidateNewCliType(INPUT pcCliType, 
                       INPUT pcDataBundleId,
                       INPUT plByPass, 
                       OUTPUT lcError) NE 0 THEN 
   RETURN appl_err(lcError).

FIND FIRST NewCliType NO-LOCK WHERE
           NewCLIType.Brand   EQ Syst.Var:gcBrand AND
           NewCLIType.CLIType EQ pcCliType        NO-ERROR.
IF NOT AVAIL NewCLIType THEN
   RETURN appl_err(SUBST("Unknown CLIType &1", pcCliType)).
   
/* Check if credit check is needed */
IF fServAttrValue(MobSub.CLIType,
                  "TypeChg",
                  "CreditCheck",
                  OUTPUT ok) = "0"
   OR NewCLIType.PayType = 2 THEN liCreditcheck = 0.

IF pdeCharge > 0 THEN DO:
   lcError = fCheckChargeLimits(Mobsub.CLI,
                                Mobsub.PayType,
                                pdeCharge,
                                pdeChargeLimit).
END.

IF lcError > "" THEN RETURN appl_err(lcError).

/*empty contract_id if it is not from VFR*/
IF pcChannel NE {&DMS_VFR_REQUEST} THEN
   pcContractId = "".

/* YDR-2038 
   exempt penalty fee when doing an STC
   iiRequestFlags (0=no extend_term_contract, 
                   1=extend_term_contract
                   2=exclude_term_penalty)
   */
IF plExtendContract THEN
DO:
   iiRequestFlags = IF plExtendContract THEN 1 ELSE 0.
END.
ELSE
IF plExcludeTermPenalty THEN
   iiRequestFlags = 2.   

liSTCMsSeq = MobSub.MsSeq.

IF llgMerge THEN DO:

    IF MobSub.Fixednumber NE ? AND
       MobSub.Fixednumber > '' THEN DO:

       liMergeMsSeq = MobSub.MsSeq.

       FIND bChkMobSub NO-LOCK WHERE
            bChkMobSub.CLI EQ pcMergeWith NO-ERROR.

       IF AVAILABLE bChkMobSub THEN
          liSTCMsSeq = bChkMobSub.MsSeq.
    END.
    ELSE DO:
        FIND bChkMobSub NO-LOCK WHERE
             bChkMobSub.CLI EQ pcMergeWith NO-ERROR.

        IF AVAILABLE bChkMobSub THEN
           liMergeMsSeq = bChkMobSub.MsSeq.
    END.

    FIND FIRST bMergeMobSub NO-LOCK WHERE
               bMergeMobSub.MsSeq    EQ liMergeMsSeq                  AND
               bMergeMobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE} NO-ERROR.
 
    IF NOT AVAIL bMergeMobSub THEN
       RETURN appl_err(SUBST("Convergent subscription &1 is not 2P standalone", STRING(liMergeMsSeq))).

END.

/* Various validations */
IF fValidateMobTypeCh(liSTCMsSeq,
                      pcCliType,
                      pdActivation,
                      plExtendContract,
                      FALSE, /* bypass stc type check */
                      0, /* stc order id */
                      {&REQUEST_SOURCE_NEWTON},
                      llgMerge,
                      OUTPUT lcError) EQ FALSE THEN 
   RETURN appl_err(lcError).

liRequest = fCTChangeRequest(liSTCMsSeq,
                             pcCliType,
                             pcDataBundleId,
                             pcBankAcc,      /* validation is already done in newton */
                             pdActivation,
                             liCreditCheck,  /* 0 = Credit check ok */
                             iiRequestFlags,
                             "" /* pcSalesman */,
                             (pdeCharge > 0),
                             llSendSMS,
                             "",
                             pdeCharge,
                             {&REQUEST_SOURCE_NEWTON}, 
                             0, /* order id */
                             0,
                             pcContractId, /*dms: contract_id,channel ->ReqCParam6*/
                             OUTPUT lcInfo).

IF liRequest = 0 THEN DO:
   RETURN appl_err("Request creation failed: " +  lcInfo).
END.

IF llgMerge THEN
   fCreateMsRequestParam(liRequest,
                         {&MERGE2P3P},
                         {&INTVAL},
                         STRING(bMergeMobSub.MsSeq)).

IF pcMemoTitle > "" OR pcMemoContent > "" THEN DO:
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = Syst.Var:gcBrand
      Memo.HostTable = "MobSub"
      Memo.KeyValue  = STRING(liSTCMsSeq)
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = Syst.Var:katun
      Memo.MemoTitle = pcMemoTitle
      Memo.MemoText  = pcMemoContent
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "MobSub".
END. /* IF pcMemoTitle > "" OR pcMemoContent > "" THEN DO: */

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   END.

