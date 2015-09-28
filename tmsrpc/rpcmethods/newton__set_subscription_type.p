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
          memo;struct;optional;
 * @memo  title;string;mandatory;memo title
          contents;string;mandatory;memo content
 * @output success;boolean
 */
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
katun = "Newton".

{fbankdata.i}
{fctchange.i}
{fmakemsreq.i}
{fcharge_comp_loaded.i}
{tmsconst.i}

/* Input parameters */
DEF VAR pcMSISDN         AS CHAR NO-UNDO.
DEF VAR pcSalesman       AS CHAR NO-UNDO.
DEF VAR pcCliType        AS CHAR NO-UNDO.
DEF VAR pdActivation     AS DEC  NO-UNDO.
DEF VAR pdeCharge        AS DEC  NO-UNDO. 
DEF VAR pdeChargeLimit   AS DEC  NO-UNDO. 
DEF VAR pcBankAcc        AS CHAR NO-UNDO.
DEF VAR pcDataBundleId   AS CHAR NO-UNDO. 
DEF VAR plByPass         AS LOG  NO-UNDO. 
DEF VAR plExtendContract AS LOG  NO-UNDO.
DEF VAR pcMemoStruct     AS CHAR NO-UNDO.
DEF VAR pcMemoTitle      AS CHAR NO-UNDO.
DEF VAR pcMemoContent    AS CHAR NO-UNDO.
DEF VAR pcContractID     AS CHAR NO-UNDO.

/* Local variables */
DEF VAR lcc AS CHAR NO-UNDO.
DEF VAR liCreditCheck AS INT  NO-UNDO INIT 1.
DEF VAR llCreateFees  AS LOG  NO-UNDO INIT FALSE.
DEF VAR llSendSMS     AS LOG  NO-UNDO INIT TRUE.
DEF VAR lcInfo        AS CHAR NO-UNDO.
DEF VAR ok            AS LOG  NO-UNDO.
DEF VAR lcPCDenyCT    AS CHAR NO-UNDO. 
DEF VAR lcTiePeriod   AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO.
DEF VAR liRequest     AS INT  NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.

DEF BUFFER NewCliType   FOR CliType.

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
/* web is passing renewal_stc but we don't actually need it */
lcstruct = validate_struct(pcStruct, 
   "msisdn!,username!,subscription_type_id!,activation_stamp!,charge!,charge_limit!,bank_account,data_bundle_id,renewal_stc,bypass,extend_term_contract,memo,contract_id").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcMSISDN    = get_string(pcStruct, "msisdn").
pcSalesman  = get_string(pcStruct, "username").
pcCliType   = get_string(pcStruct, "subscription_type_id").
pdActivation = get_timestamp(pcStruct, "activation_stamp").
pdeCharge = get_double(pcStruct, "charge").
pdeChargeLimit = get_double(pcStruct, "charge_limit").

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

ASSIGN
   pcBankAcc = get_string(pcStruct, "bank_account") 
      WHEN LOOKUP("bank_account", lcstruct) > 0
   pcDataBundleId = get_string(pcStruct, "data_bundle_id")
      WHEN LOOKUP(pcCliType,lcBundleCLITypes) > 0
   plByPass = get_bool(pcStruct, "bypass") WHEN LOOKUP("bypass", lcstruct) > 0
   plExtendContract = get_bool(pcStruct,"extend_term_contract")
      WHEN LOOKUP("extend_term_contract", lcstruct) > 0
   pcContractID = get_string(pcStruct,"contract_id")
         WHEN LOOKUP("contract_id", lcstruct) > 0.

IF LOOKUP("memo", lcstruct) > 0 THEN DO:
   pcMemoStruct = get_struct(pcStruct,"memo").
   ASSIGN
      pcMemoTitle   = get_string(pcMemoStruct,"title")
      pcMemoContent = get_string(pcMemoStruct,"content").
END. /* IF LOOKUP("memo", lcstruct) > 0 THEN DO: */

IF gi_xmlrpc_error NE 0 THEN RETURN.

katun = "VISTA_" + pcSalesman.

IF TRIM(katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND mobsub NO-LOCK WHERE
     mobsub.cli = pcMSISDN NO-ERROR.
IF NOT AVAILABLE mobsub THEN
   RETURN appl_err(SUBST("MobSub entry &1 not found", pcMSISDN)).

/* Set the katun to check correct barring */
katun = "NewtonAd".

/* Various validations */
IF fValidateMobTypeCh(
   MobSub.Msseq,
   pcCliType,
   pdActivation,
   plExtendContract,
   FALSE, /* bypass stc type check */
   0, /* stc order id */
   OUTPUT lcError) EQ FALSE THEN RETURN appl_err(lcError).

/* Set the katun again with original username */
katun = "VISTA_" + pcSalesman.

IF fValidateNewCliType(INPUT pcCliType, INPUT pcDataBundleId,
                       INPUT plByPass, OUTPUT lcError) NE 0
THEN RETURN appl_err(lcError).

FIND FIRST NewCliType WHERE
           NewCLIType.Brand = gcBrand AND
           NewCLIType.CLIType = pcCliType NO-LOCK.
IF NOT AVAIL NewCLIType THEN
   RETURN appl_err(SUBST("Unknown CLIType &1", pcCliType)).
   
/* Check if credit check is needed */
IF fServAttrValue(MobSub.CLIType,
                  "TypeChg",
                  "CreditCheck",
                  OUTPUT ok) = "0"
   OR NewCLIType.PayType = 2 THEN liCreditcheck = 0.

IF pdeCharge > 0 THEN DO:
   lcError = fCheckChargeLimits (
      Mobsub.CLI,
      Mobsub.PayType,
      pdeCharge,
      pdeChargeLimit).
END.

IF lcError > "" THEN RETURN appl_err(lcError).

liRequest = fCTChangeRequest(MobSub.msseq,
                  pcCliType,
                  pcDataBundleId,
                  pcBankAcc,      /* validation is already done in newton */
                  pdActivation,
                  liCreditCheck,  /* 0 = Credit check ok */
                  plExtendContract,
                  "" /* pcSalesman */,
                  (pdeCharge > 0),
                  llSendSMS,
                  "",
                  pdeCharge,
                  {&REQUEST_SOURCE_NEWTON}, 
                  0, /* order id */
                  0,
                  pcContractID, /*dms, contract_id ->ReqCParam6*/
                  OUTPUT lcInfo).

IF liRequest = 0 THEN DO:
   RETURN appl_err("Request creation failed: " +  lcInfo).
END.

IF pcMemoTitle > "" OR pcMemoContent > "" THEN DO:
   CREATE Memo.
   ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = gcBrand
      Memo.HostTable = "MobSub"
      Memo.KeyValue  = STRING(MobSub.MsSeq)
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun
      Memo.MemoTitle = pcMemoTitle
      Memo.MemoText  = pcMemoContent
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "MobSub".
END. /* IF pcMemoTitle > "" OR pcMemoContent > "" THEN DO: */

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
