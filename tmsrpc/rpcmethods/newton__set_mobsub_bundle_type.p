/**
 * Creates Data Bundle Change (BTC) request 
 *
 * @input  msseq;int;mandatory;subscription id
           old_bundle;string;mandatory;current active bundle
           new_bundle;string;mandatory;new bundle
           username;string;mandatory;user name
           date;datetime;mandatory;activation date
           memo;struct;optional;
           extend_term_contract;boolean;optional;false=terminate,true=extent
 * @memo   title;string;mandatory;memo title
           contents;string;mandatory;memo content
 * @output status;empty struct;
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
gcBrand = "1".
{tmsconst.i}
{date.i}
{fbundle.i}
{fbtc.i}
{fdss.i}
{fmakemsreq.i}

DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO.  
DEF VAR piMsSeq AS INTEGER NO-UNDO. 
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 
DEF VAR pdaActDate AS DATE NO-UNDO.
DEF VAR pcMemoTitle AS CHARACTER NO-UNDO. 
DEF VAR pcMemoContent AS CHARACTER NO-UNDO. 
DEF VAR pcOldBundle AS CHAR NO-UNDO. 
DEF VAR pcNewBundle AS CHAR NO-UNDO. 
DEF VAR plExtendContract AS LOG NO-UNDO. 

DEF VAR liCreated AS INT NO-UNDO. 
DEF VAR ldActStamp AS DEC NO-UNDO. 
DEF VAR llUpgradeUpsell AS LOG  NO-UNDO INIT FALSE.
DEF VAR lcUpgradeUpsell AS CHAR NO-UNDO.
DEF VAR liUpsellCreated AS INT  NO-UNDO.
DEF VAR lcBONOContracts AS CHAR NO-UNDO.
DEF VAR lcMemoType AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct,
           "msseq!,old_bundle!,new_bundle!,date!,username!,memo,upgrade_upsell,extend_term_contract").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   piMsSeq = get_int(pcStruct,"msseq")
   pcOldBundle = get_string(pcStruct,"old_bundle")
   pcNewBundle = get_string(pcStruct,"new_bundle")
   pdaActDate = get_date(pcStruct,"date")
   katun = "VISTA_" + get_string(pcStruct,"username")
   llUpgradeUpsell = get_bool(pcStruct,"upgrade_upsell")
      WHEN LOOKUP("upgrade_upsell", lcstruct) > 0
   plExtendContract = get_bool(pcStruct,"extend_term_contract")
      WHEN LOOKUP("extend_term_contract", lcstruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP("memo", lcstruct) > 0 THEN DO:
   pcMemoStruct = get_struct(pcStruct,"memo").
   ASSIGN
      pcMemoTitle = get_string(pcMemoStruct,"title")
      pcMemoContent = get_string(pcMemoStruct,"content").
END. /* IF LOOKUP("memo", lcstruct) > 0 THEN DO: */

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub WHERE 
           MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN appl_err("Subscription not found").

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = gcBrand AND
           DayCampaign.DCEvent = pcNewBundle NO-ERROR.
IF NOT AVAIL DayCampaign THEN RETURN appl_err("DayCampaign not defined").

IF TRIM(katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

lcBONOContracts = fCParamC("BONO_CONTRACTS").

IF NOT fValidateBTC
   (MobSub.MsSeq,
    pcOldBundle,
    pcNewBundle,
    pdaActDate,
    MobSub.CliType,
    plExtendContract, /* extend contract */
    OUTPUT lcError)
   THEN RETURN appl_err(lcError). 

ldActStamp = fMake2Dt(pdaActDate,
                      IF pdaActDate = TODAY
                      THEN TIME
                      ELSE 0).

liCreated = fBundleChangeRequest(MobSub.MsSeq,
                                 pcOldBundle, 
                                 pcNewBundle,
                                 ldActStamp,
                                 {&REQUEST_SOURCE_NEWTON},
                                 "",    /* creator */
                                 FALSE, /* create fees */
                                 0,     /* orig. request */
                                 FALSE, /* mandatory */
                                 llUpgradeUpsell, /* Upgrade Upsell */
                                 plExtendContract, /*extend terminal contract*/
                                 OUTPUT lcError).
IF liCreated = 0 THEN
   RETURN appl_err("Change request could not be created").

/* Activate Upgrade Upsell */
IF llUpgradeUpsell THEN DO:
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "UpsellUpgrade" AND
              TMSCodes.FieldName = "BundleLimit"   AND
              TMSCodes.CodeValue = pcOldBundle + "TO" + pcNewBundle
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSCodes THEN
      /* Write memo */
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.CustNum,
                       "Upgrade Upsell Creation Failed",
                       "Upgrade Upsell Limit is not configured in TMS for " +
                       pcOldBundle + "TO" + pcNewBundle).
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
                                         OUTPUT lcError).
      IF liUpsellCreated = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Upgrade Upsell Creation Failed",
                          "Upgrade Upsell Creation Failed. " + lcError).
   END. /* ELSE DO: */
END. /* IF ilUpgradeUpsell THEN DO: */

IF LOOKUP(pcNewBundle,lcBONOContracts) > 0 THEN
   lcMemoType  = "Service".
ELSE
   lcMemoType  = "MobSub".

DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                 "MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 "Bono modificado",                    /* MemoTitle */
                 pcMemoContent + " " + pcOldBundle + " --> " + pcNewBundle,  /* MemoText */
                 lcMemoType,                           /* MemoType */
                 katun).


lcResultStruct = add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
