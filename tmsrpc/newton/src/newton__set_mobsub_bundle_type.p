/**
 * Creates Data Bundle Change (BTC) request 
 *
 * @input  msseq;int;mandatory;subscription id
           old_bundle;string;mandatory;current active bundle
           new_bundle;string;mandatory;new bundle
           username;string;mandatory;user name
           date;datetime;mandatory;activation date
           memo;struct;optional;
           extend_term_contract;boolean;optional;false=terminate,true=extend
           exclude_term_penalty (boolean, optional) * To accept the penalty exemption
 * @memo   title;string;mandatory;memo title
           contents;string;mandatory;memo content
 * @output status;empty struct;
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Func/fbtc.i}
{Func/fdss.i}
{Func/fmakemsreq.i}

DEF VAR pcStruct             AS CHARACTER NO-UNDO. 
DEF VAR lcStruct             AS CHARACTER NO-UNDO.  
DEF VAR piMsSeq              AS INTEGER   NO-UNDO. 
DEF VAR lcResultStruct       AS CHARACTER NO-UNDO. 
DEF VAR lcError              AS CHAR      NO-UNDO. 
DEF VAR pcMemoStruct         AS CHARACTER NO-UNDO. 
DEF VAR pdaActDate           AS DATE      NO-UNDO.
DEF VAR pcMemoTitle          AS CHARACTER NO-UNDO. 
DEF VAR pcMemoContent        AS CHARACTER NO-UNDO. 
DEF VAR pcOldBundle          AS CHAR      NO-UNDO. 
DEF VAR pcNewBundle          AS CHAR      NO-UNDO. 
DEF VAR plExtendContract     AS LOG       NO-UNDO. 
DEF VAR plExcludeTermPenalty AS LOGICAL   NO-UNDO.
DEF VAR iiRequestFlags       AS INTEGER   NO-UNDO. 

DEF VAR liCreated       AS INT     NO-UNDO. 
DEF VAR ldActStamp      AS DEC     NO-UNDO. 
DEF VAR llUpgradeUpsell AS LOG     NO-UNDO INIT FALSE.
DEF VAR lcUpgradeUpsell AS CHAR    NO-UNDO.
DEF VAR liUpsellCreated AS INT     NO-UNDO.
DEF VAR lcBONOContracts AS CHAR    NO-UNDO.
DEF VAR lcVoiceBundles  AS CHAR    NO-UNDO.
DEF VAR lcMemoType      AS CHAR    NO-UNDO.

DEF VAR pcContractID     AS CHAR NO-UNDO.
DEF VAR pcChannel        AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct,
           "msseq!,old_bundle!,new_bundle!,date!,username!,memo,upgrade_upsell," +
           "extend_term_contract,contract_id,channel,exclude_term_penalty ").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   piMsSeq = get_int(pcStruct,"msseq")
   pcOldBundle = get_string(pcStruct,"old_bundle")
   pcNewBundle = get_string(pcStruct,"new_bundle")
   pdaActDate = get_date(pcStruct,"date")
   Syst.Var:katun = "VISTA_" + get_string(pcStruct,"username")
   llUpgradeUpsell = get_bool(pcStruct,"upgrade_upsell")
      WHEN LOOKUP("upgrade_upsell", lcstruct) > 0
   plExtendContract = get_bool(pcStruct,"extend_term_contract")
      WHEN LOOKUP("extend_term_contract", lcstruct) > 0
   pcContractID = get_string(pcStruct,"contract_id")
      WHEN LOOKUP("contract_id", lcstruct) > 0
   pcChannel = get_string(pcStruct,"channel")
      WHEN LOOKUP("channel", lcstruct) > 0
   plExcludeTermPenalty = get_bool(pcStruct,"exclude_term_penalty")
      WHEN LOOKUP("exclude_term_penalty", lcstruct) > 0.

ASSIGN
    pcOldBundle = ENTRY(1,pcOldBundle,"|")  
    pcNewBundle = ENTRY(1,pcNewBundle,"|").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF plExtendContract AND plExcludeTermPenalty THEN
   RETURN appl_err("Both 'Contract extension' and 'Penalty exemption' requested").

IF LOOKUP("memo", lcstruct) > 0 THEN DO:
   pcMemoStruct = get_struct(pcStruct,"memo").
   ASSIGN
      pcMemoTitle = get_string(pcMemoStruct,"title")
      pcMemoContent = get_string(pcMemoStruct,"content").
END. /* IF LOOKUP("memo", lcstruct) > 0 THEN DO: */

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO OrderCanal MobSub MsSeq piMsSeq}

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = Syst.Var:gcBrand AND
           DayCampaign.DCEvent = pcNewBundle NO-ERROR.
IF NOT AVAIL DayCampaign THEN RETURN appl_err("DayCampaign not defined").

IF TRIM(Syst.Var:katun) EQ "VISTA_" THEN RETURN appl_err("username is empty").

ASSIGN
    lcBONOContracts = fCParamC("BONO_CONTRACTS")
    lcVoiceBundles  = fCParamC("VOICE_BONO_CONTRACTS").

IF NOT fValidateBTC
   (MobSub.MsSeq,
    pcOldBundle,
    pcNewBundle,
    pdaActDate,
    MobSub.CliType,
    plExtendContract, /* extend contract */
    OUTPUT lcError)
   THEN RETURN appl_err(lcError). 

ldActStamp = Func.Common:mMake2DT(pdaActDate,
                      IF pdaActDate = TODAY
                      THEN TIME
                      ELSE 0).

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
                                 iiRequestFlags, /*extend terminal contract*/
                                 pcContractId,
                                 OUTPUT lcError).

IF liCreated = 0 THEN DO:
   RETURN appl_err("Change request could not be created" + lcError).
END.   

/* Activate Upgrade Upsell */
IF llUpgradeUpsell THEN DO:
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "UpsellUpgrade" AND
              TMSCodes.FieldName = "BundleLimit"   AND
              TMSCodes.CodeValue = pcOldBundle + "TO" + pcNewBundle
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE TMSCodes THEN
      /* Write memo */
      Func.Common:mWriteMemo("MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.CustNum,
                       "Upgrade Upsell Creation Failed",
                       "Upgrade Upsell Limit is not configured in TMS for " +
                       pcOldBundle + "TO" + pcNewBundle).
   ELSE DO:
      IF fIsDSSActive(INPUT MobSub.CustNum,INPUT Func.Common:mMakeTS()) THEN
         lcUpgradeUpsell = "DSS_UPSELL_UPGRADE".
      ELSE lcUpgradeUpsell = "UPGRADE_UPSELL".

      liUpsellCreated = fPCActionRequest(MobSub.MsSeq,
                                         lcUpgradeUpsell, 
                                         "act",
                                         Func.Common:mMakeTS(),
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
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Upgrade Upsell Creation Failed",
                          "Upgrade Upsell Creation Failed. " + lcError).
   END. /* ELSE DO: */
END. /* IF ilUpgradeUpsell THEN DO: */

IF LOOKUP(pcNewBundle,lcBONOContracts) > 0 OR LOOKUP(pcNewBundle,lcVoiceBundles) > 0 THEN
   lcMemoType  = "Service".
ELSE
   lcMemoType  = "MobSub".

Func.Common:mWriteMemoWithType("MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 "Bono modificado",                    /* MemoTitle */
                 pcMemoContent + " " + pcOldBundle + " --> " + pcNewBundle,  /* MemoText */
                 lcMemoType,                           /* MemoType */
                 Syst.Var:katun).


lcResultStruct = add_struct(response_toplevel_id, "").

FINALLY:
   END.
