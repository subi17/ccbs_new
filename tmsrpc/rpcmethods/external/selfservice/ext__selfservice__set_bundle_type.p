
/**
 * Creates Data Bundle Change (BTC) request for BONO->BONO,La Infinita->La Infinita
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;subscription msisdn number
           new_bundle;string;mandatory;new bundle id
           activation_stamp;datetime;mandatory;request activation time
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions  1;Subscription not found
                2;Incorrect Bundle Id
                3;Bundle not found
                4;Change request could not be created
                5;Ongoing BTC request
                6;Nothing to do
                7;Bundle is not allowed for this subscription type
                8;Subscription has a pending STC. Bundle change not allowed (STC from VOICE to OTHER)
                9;Application Id does not match
               10;Invalid activation date 
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
gcBrand = "1".
{Func/date.i}
{Mm/fbundle.i}
{Func/fbtc.i}
{Func/fexternalapi.i}

DEF VAR pcCLI              AS CHAR NO-UNDO.
DEF VAR pcTransId          AS CHAR NO-UNDO.
DEF VAR pcNewBundle        AS CHAR NO-UNDO.
DEF VAR top_struct         AS CHAR NO-UNDO.
DEF VAR pdActivation       AS DEC  NO-UNDO.

DEF VAR lcError            AS CHAR NO-UNDO.
DEF VAR ldaActDate         AS DATE NO-UNDO.
DEF VAR lcOldBundle        AS CHAR NO-UNDO.
DEF VAR liCreated          AS INT  NO-UNDO. 
DEF VAR ldActStamp         AS DEC  NO-UNDO.
DEF VAR lcBundleType       AS CHAR NO-UNDO.
DEF VAR lcApplicationId    AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId     AS CHAR NO-UNDO.

DEF VAR lcAllowedBONOContracts  AS CHAR NO-UNDO.
DEF VAR lcAllowedCONTSContracts AS CHAR NO-UNDO.
DEF VAR lcMemoType AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string,datetime") EQ ? THEN RETURN.

ASSIGN pcTransId    = get_string(param_toplevel_id, "0")
       pcCLI        = get_string(param_toplevel_id,"1")
       pcNewBundle  = get_string(param_toplevel_id,"2")
       pdActivation = get_timestamp(param_toplevel_id,"3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = ghAuthLog::EndUserId.

IF NOT fchkTMSCodeValues(ghAuthLog::UserName,lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

FIND FIRST MobSub  WHERE 
           MobSub.CLI = pcCLI NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN appl_err("Subscription not found").

fSplitTS(pdActivation,OUTPUT ldaActDate,OUTPUT liActTime).

ASSIGN pdActivation            = fMake2Dt(ldaActDate, 0)
       lcAllowedBONOContracts  = fCParamC("ALLOWED_BONO_CONTRACTS")
       lcAllowedCONTSContracts = fCParamC("ALLOWED_CONTS_CONTRACTS").

IF LOOKUP(pcNewBundle,lcAllowedBONOContracts) > 0 THEN
   lcBundleType = "BONO".
ELSE IF LOOKUP(pcNewBundle,lcAllowedCONTSContracts) > 0 THEN
   lcBundleType = "CONTS".
ELSE RETURN appl_err("Incorrect Bundle Id").

IF lcBundleType = "BONO" THEN
   lcOldBundle = fGetCurrentSpecificBundle(INPUT MobSub.MsSeq,
                                           INPUT lcBundleType).
ELSE
   lcOldBundle = MobSub.TariffBundle.

IF lcOldBundle = "" OR lcOldBundle = ? THEN
   RETURN appl_err("Bundle not found").

IF NOT fValidateBTC (MobSub.MsSeq,
                     lcOldBundle,
                     pcNewBundle,
                     ldaActDate,
                     MobSub.CliType,
                     FALSE, /* extend contract */
                     OUTPUT lcError)
THEN RETURN appl_err(lcError). 

liCreated = fBundleChangeRequest(MobSub.MsSeq,
                                 lcOldBundle,
                                 pcNewBundle,
                                 pdActivation,
                                 {&REQUEST_SOURCE_EXTERNAL_API},
                                 "",    /* creator */
                                 FALSE, /* create fees */
                                 0,     /* orig. request */
                                 FALSE, /* mandatory */
                                 FALSE, /* upgrade upsell */
                                 0,     /* 0 NO extend terminal contract*/
                                 "",
                                 OUTPUT lcError).

IF liCreated = 0 THEN
   RETURN appl_err("Change request could not be created").

IF lcBundleType = "BONO" THEN
   lcMemoType = "Service".
ELSE
   lcMemoType = "MobSub".

DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                 "MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 "Bono modificado",                    /* MemoTitle */
                 lcOldBundle + " --> " + pcNewBundle,  /* MemoText */
                 lcMemoType,                           /* MemoType */
                 fgetAppDetailedUserId(INPUT lcApplicationId,
                                       INPUT Mobsub.CLI)).

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
