/**
 * Change subscription type.
 *
 * Parameters are in the toplevel struct and the meaning for them are:
 * @input transaction_id;string;mandatory;transaction id
          msisdn;string;mandatory;subscription msisdn number
          subscription_type_id;string;mandatory;new subscription type
          activation_stamp;datetime;mandatory;request activation time
          bank_account;string;optional;new bank account for postpaid
          subscription_bundle_id;string;mandatory;bundle type (for CONTF/CONTRD/CONTS)
 * @output    struct;mandatory;response struct
 * @response  transaction_id;string;transaction id
              result;boolean;True
 * @exceptions  1;Subscription not found
                2;Unknown CLIType specified
                3;Request creation failed
                4;Function not allowed due to business rules
                5;Ongoing MNP OUT request. Too late for STC
                6;Unknown or missing clitype!
                7;New clitype cannot be same as the old one!
                8;Bundle is not allowed for this subscription type
                9;Function not allowed due to existing requests
               10;Subscription based bundle is missing
               11;Application Id does not match
               12;STC is not allowed for multi SIM secondary subscription
               13;Activation date can't be other than 1st day of month
               14;Multiple immediate STC is not allowed in same month due to business rules!
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/fbankdata.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/fcharge_comp_loaded.i}
{Syst/tmsconst.i}
{Func/fexternalapi.i}
{Func/profunc.i}

/* Input parameters */
DEF VAR pcMSISDN         AS CHAR NO-UNDO.
DEF VAR pcCliType        AS CHAR NO-UNDO.
DEF VAR pdActivation     AS DEC  NO-UNDO.
DEF VAR pcBankAcc        AS CHAR NO-UNDO.
DEF VAR pcDataBundleId   AS CHAR NO-UNDO. 
DEF VAR pcTransId        AS CHAR NO-UNDO.
DEF VAR top_struct       AS CHAR NO-UNDO.

/* Local variables */
DEF VAR liCreditCheck    AS INT  NO-UNDO INIT 1.
DEF VAR lcInfo           AS CHAR NO-UNDO.
DEF VAR ok               AS LOG  NO-UNDO.
DEF VAR lcError          AS CHAR NO-UNDO.
DEF VAR liRequest        AS INT  NO-UNDO.
DEF VAR pcStruct         AS CHAR NO-UNDO. 
DEF VAR lcStruct         AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR ldaActDate       AS DATE NO-UNDO.
DEF VAR lcApplicationId  AS CHAR NO-UNDO.
DEF VAR lcProValidation  AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId   AS CHAR NO-UNDO.

DEF BUFFER NewCliType    FOR CliType.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
lcstruct = validate_struct(pcStruct, 
   "transaction_id!,msisdn!,subscription_type_id!,activation_stamp!,bank_account,data_bundle_id").

pcTransId      = get_string(pcStruct, "transaction_id").
pcMSISDN       = get_string(pcStruct, "msisdn").
pcCliType      = get_string(pcStruct, "subscription_type_id").
pdActivation   = get_timestamp(pcStruct, "activation_stamp").

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

ASSIGN
   pcBankAcc = get_string(pcStruct, "bank_account") 
      WHEN LOOKUP("bank_account", lcstruct) > 0
   pcDataBundleId = get_string(pcStruct, "data_bundle_id")
      WHEN LOOKUP(pcCliType,lcBundleCLITypes) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

{selfservice/src/findtenant.i NO ordercanal MobSub Cli pcMSISDN}

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = ghAuthLog::EndUserId.
       
IF NOT fchkTMSCodeValues(ghAuthLog::UserName,lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

FIND FIRST NewCliType WHERE
           NewCLIType.Brand = Syst.Var:gcBrand AND
           NewCLIType.CLIType = pcCliType NO-LOCK.
IF NOT AVAIL NewCLIType THEN
   RETURN appl_err("Unknown CLIType specified").

IF LOOKUP(pcCliType,lcBundleCLITypes) > 0 AND pcDataBundleId = "" THEN
   RETURN appl_err("Subscription based bundle is missing").

Func.Common:mSplitTS(pdActivation,OUTPUT ldaActDate,OUTPUT liActTime).

ASSIGN pdActivation = Func.Common:mMake2DT(ldaActDate, 0).

Syst.Var:katun = "NewtonAd". /* check correct barring */

IF fValidateMobTypeCh(
   MobSub.Msseq,
   pcCliType,
   pdActivation,
   FALSE, /* extend contract */
   FALSE, /* bypass stc type check */
   0, /* stc order id */
   {&REQUEST_SOURCE_EXTERNAL_API},
   FALSE,
   OUTPUT lcError) EQ FALSE THEN RETURN appl_err(lcError).

IF fValidateNewCliType(INPUT pcCliType, INPUT pcDataBundleId,
                       INPUT FALSE, OUTPUT lcError) NE 0 THEN DO:
   RETURN appl_err(lcError).
END.

/*YPRO*/
lcProValidation = fValidateProSTC(MobSub.Custnum,
                                  MobSub.CliType,
                                  pcCliType).
IF lcProValidation NE "" THEN
   RETURN appl_err("Pro customer validatuin error: " + lcProValidation).



/* Check if credit check is needed */
IF fServAttrValue(MobSub.CLIType,
                  "TypeChg",
                  "CreditCheck",
                  OUTPUT ok) = "0"
   OR NewCLIType.PayType = 2 THEN liCreditcheck = 0.

IF lcError > "" THEN DO:
   RETURN appl_err(lcError).
END.

Syst.Var:katun = fgetAppUserId(INPUT lcApplicationId, 
                      INPUT lcAppEndUserId).
                      
liRequest = fCTChangeRequest(MobSub.msseq,
                  pcCliType,
                  pcDataBundleId,
                  pcBankAcc,      /* validation is already done in newton */
                  pdActivation,
                  liCreditCheck,  /* 0 = Credit check ok */
                  0, /* 0 NO extend contract */
                  "",
                  FALSE,
                  TRUE,
                  "",
                  0,
                  {&REQUEST_SOURCE_EXTERNAL_API},
                  0, /* order id */
                  0,
                  "",
                  OUTPUT lcInfo).

IF liRequest = 0 THEN DO:
   RETURN appl_err("Request creation failed").
END.

CREATE Memo.
ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = Syst.Var:gcBrand 
      Memo.HostTable = "MobSub" 
      Memo.KeyValue  = STRING(MobSub.MsSeq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = Syst.Var:katun 
      Memo.MemoTitle = "Subscription Type Change"
      Memo.MemoText  = "External API subscription type change " + 
                       MobSub.CLIType + " --> " + pcCliType
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "MobSub".

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.

   END.
