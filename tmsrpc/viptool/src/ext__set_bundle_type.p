/**
 * Creates Data Bundle Change (BTC) request
 *
 * @input  msseq;int;mandatory;subscription id
           new_bundle;string;mandatory;new bundle
           upgrade_upsell;boolean;mandatory;True/False
 * @output status;empty struct
 * @Exceptions 1;Subscription not found
               2;Bundle not found
               3;Change request could not be created
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId. 
gcBrand = "1".
{Func/date.i}
{Mm/fbundle.i}
{Func/fbtc.i}
{Func/fmakemsreq.i}
{Func/fdss.i}

DEF VAR piMsSeq            AS INT  NO-UNDO. 
DEF VAR pcNewBundle        AS CHAR NO-UNDO.
DEF VAR lcError            AS CHAR NO-UNDO.
DEF VAR ldaActDate         AS DATE NO-UNDO.
DEF VAR lcOldBundle        AS CHAR NO-UNDO.
DEF VAR lcResultStruct     AS CHAR NO-UNDO.
DEF VAR liCreated          AS INT  NO-UNDO. 
DEF VAR ldActStamp         AS DEC  NO-UNDO.
DEF VAR lcBundleType       AS CHAR NO-UNDO.
DEF VAR llUpgradeUpsell    AS LOG  NO-UNDO INIT FALSE.
DEF VAR lcUpgradeUpsell    AS CHAR NO-UNDO.
DEF VAR liUpsellCreated    AS INT  NO-UNDO.

DEF VAR lcAllowedBONOContracts   AS CHAR NO-UNDO.
DEF VAR lcAllowedCONTSContracts  AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int,string,boolean") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id,"0").
pcNewBundle = get_string(param_toplevel_id,"1").
llUpgradeUpsell = get_bool(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

ASSIGN lcAllowedBONOContracts  = fCParamC("ALLOWED_BONO_CONTRACTS")
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

ldaActDate = (fLastDayOfMonth(TODAY) + 1).

IF NOT fValidateBTC (MobSub.MsSeq,
                     lcOldBundle,
                     pcNewBundle,
                     ldaActDate,
                     MobSub.CliType,
                     FALSE, /* extend contract */
                     OUTPUT lcError)
THEN RETURN appl_err(lcError). 

ldActStamp = fMake2Dt(ldaActDate,0).

liCreated = fBundleChangeRequest(MobSub.MsSeq,
                                 lcOldBundle, 
                                 pcNewBundle,
                                 ldActStamp,
                                 {&REQUEST_SOURCE_EXTERNAL_API},
                                 "",    /* creator */
                                 FALSE, /* create fees */
                                 0,     /* orig. request */
                                 FALSE, /* mandatory */
                                 llUpgradeUpsell, /* Upgrade Upsell */
                                 0,     /* 0 NO extend terminal contract*/
                                 "",
                                 OUTPUT lcError).

IF liCreated = 0 THEN
   RETURN appl_err("Change request could not be created").

/* Activate Upgrade Upsell */
IF llUpgradeUpsell THEN DO:
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "UpsellUpgrade" AND
              TMSCodes.FieldName = "BundleLimit"   AND
              TMSCodes.CodeValue = lcOldBundle + "TO" + pcNewBundle
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSCodes THEN
      /* Write memo */
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.CustNum,
                       "Upgrade Upsell Creation Failed",
                       "Upgrade Upsell Limit is not configured in TMS for " +
                       lcOldBundle + "TO" + pcNewBundle).
   ELSE DO:
      IF fIsDSSActive(INPUT MobSub.CustNum,INPUT fMakeTS()) THEN
         lcUpgradeUpsell = "DSS_UPSELL_UPGRADE".
      ELSE lcUpgradeUpsell = "UPGRADE_UPSELL".

      liUpsellCreated = fPCActionRequest(MobSub.MsSeq,
                                         lcUpgradeUpsell, 
                                         "act",
                                         fMakeTS(),
                                         TRUE,   /* create fee */
                                         {&REQUEST_SOURCE_BTC},
                                         "",
                                         liCreated,
                                         TRUE,
                                         "",
                                         0,
                                         0,
                                         "",
                                         OUTPUT lcError).
      IF liUpsellCreated = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Upgrade Upsell Creation Failed",
                          "Upgrade Upsell Creation Failed. " + lcError).
   END. /* ELSE DO: */
END. /* IF llUpgradeUpsell THEN DO: */

CREATE Memo.
ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = gcBrand 
      Memo.HostTable = "MobSub" 
      Memo.KeyValue  = STRING(MobSub.MsSeq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun 
      Memo.MemoTitle = "Bundle Type Change"
      Memo.MemoText  = "External API bundle type change " + 
                       lcOldBundle + " --> " + pcNewBundle
      Memo.CustNum   = MobSub.CustNum.

IF lcBundleType = "BONO" THEN
   Memo.MemoType = "Service".
ELSE
   Memo.MemoType = "MobSub".

lcResultStruct = add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
