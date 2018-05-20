/**
 * RPC to return DSS related information.
 *
 * @input: custnum;int;mandatory;customer number
 * @output array;containing DSS details
 * @struct cli;string;
           cli_type;string;
           data_bundle_limit;double;
           dss_limit;double
*/
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/extralinefunc.i}

/* Input parameters */
DEF VAR piCustNum           AS INT  NO-UNDO.
DEF VAR top_array           AS CHAR NO-UNDO.
DEF VAR dss_struct          AS CHAR NO-UNDO.
DEF VAR lcDataBundle        AS CHAR NO-UNDO.
DEF VAR ldeCurrentTS        AS DEC  NO-UNDO.
DEF VAR ldeDSSTotalLimit    AS DEC  NO-UNDO.
DEF VAR ldeDataBundleLimit  AS DEC  NO-UNDO.
DEF VAR ldPeriodFrom        AS DEC  NO-UNDO.
DEF VAR ldPeriodTo          AS DEC  NO-UNDO.
DEF VAR ldFromDate          AS DATE NO-UNDO.
DEF VAR ldToDate            AS DATE NO-UNDO.
DEF VAR lcIPLContracts      AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts    AS CHAR NO-UNDO.
DEF VAR lcFLATContracts     AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts    AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts   AS CHAR NO-UNDO.
DEF VAR lcBundleCLITypes    AS CHAR NO-UNDO.
DEF VAR lcExcludeBundles    AS CHAR NO-UNDO.
DEF VAR lcDSSBundleId       AS CHAR NO-UNDO.
DEF VAR lcBundleId          AS CHAR NO-UNDO.  

DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
DEF VAR lcAllowedDSS4SubsType AS CHAR NO-UNDO. 

DEF BUFFER bMServiceLimit   FOR MServiceLimit.
DEF BUFFER bMserviceLPool   FOR MserviceLPool.
DEF BUFFER bServiceLimit    FOR ServiceLimit.
DEF BUFFER bMsOwner         FOR MsOwner.
DEF BUFFER bDayCampaign     FOR DayCampaign.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piCustNum = get_int(param_toplevel_id, "0").

ASSIGN ldeCurrentTS = Func.Common:mMakeTS()
       ldFromDate   = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldToDate     = TODAY
       ldPeriodFrom = Func.Common:mMake2DT(ldFromDate,0)
       ldPeriodTo   = Func.Common:mMake2DT(ldToDate,86399)
       lcIPLContracts   = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
       lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
       lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
       lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
       lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES")
       lcExcludeBundles = fCParamC("EXCLUDE_BUNDLES").

{newton/src/findtenant.i NO Common Customer CustNum piCustNum}

/* Return DSS/UPSELL data volume */
FOR EACH ServiceLimit WHERE
         ServiceLimit.GroupCode BEGINS {&DSS} NO-LOCK,
    FIRST MServiceLimit WHERE
          MServiceLimit.Custnum = Customer.Custnum   AND
          MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
          MServiceLimit.FromTS <= ldeCurrentTS       AND
          MServiceLimit.EndTS  >= ldeCurrentTS NO-LOCK:

    IF LOOKUP(ServiceLimit.GroupCode,{&DSS_BUNDLES}) > 0 THEN DO:
       lcDSSBundleId = ServiceLimit.GroupCode.
       FIND FIRST MServiceLPool WHERE
                  MserviceLPool.Custnum = MServiceLimit.Custnum AND
                  MserviceLPool.SLSeq   = MServiceLimit.SLSeq   AND
                  MserviceLPool.FromTS <= ldPeriodTo            AND
                  MserviceLPool.EndTS  >= ldPeriodTo NO-LOCK NO-ERROR.
       IF AVAILABLE MserviceLPool THEN
          ldeDSSTotalLimit = ldeDSSTotalLimit + MserviceLPool.LimitAmt.
       ELSE
          ldeDSSTotalLimit = ldeDSSTotalLimit + MServiceLimit.InclAmt.
    END. /* IF ServiceLimit.GroupCode = {&DSS} THEN DO: */
    ELSE DO:
       FIND FIRST MServiceLPool WHERE
                  MserviceLPool.Custnum = MServiceLimit.Custnum AND
                  MserviceLPool.SLSeq   = MServiceLimit.SLSeq   AND
                  MserviceLPool.EndTS  >= ldPeriodFrom          AND
                  MserviceLPool.FromTS <= ldPeriodTo NO-LOCK NO-ERROR.
       IF AVAILABLE MserviceLPool THEN
          ldeDSSTotalLimit = ldeDSSTotalLimit + MserviceLPool.LimitAmt.
    END. /* ELSE DO: */
END. /* FOR EACH ServiceLimit WHERE */

top_array = add_array(response_toplevel_id, "").

IF lcDSSBundleId EQ {&DSS4} THEN 
   lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE").
ELSE IF lcDSSBundleId = {&DSS2} THEN
   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

FOR EACH bMsOwner NO-LOCK WHERE
         bMsOwner.InvCust  = Customer.CustNum AND
         bMsOwner.TsBegin <= ldPeriodTo       AND
        (bMsOwner.TsEnd   >= ldPeriodFrom AND
         bMsOwner.TsEnd   >= ldPeriodTo)      AND
         NOT bMsOwner.PayType
    BREAK BY bMsOwner.MsSeq
          BY bMsOwner.TsEnd DESC:

   IF NOT FIRST-OF(bMsOwner.MsSeq) THEN NEXT.

   IF lcDSSBundleId EQ {&DSS4} AND 
      LOOKUP(bMsOwner.CLIType,lcAllowedDSS4SubsType) = 0 THEN NEXT.
   ELSE IF lcDSSBundleId = {&DSS2} AND
      LOOKUP(bMsOwner.CLIType,lcAllowedDSS2SubsType) = 0 THEN NEXT.
   
   IF (LOOKUP(bMsOwner.CLIType,lcAllowedDSS4SubsType) > 0 OR
       LOOKUP(bMsOwner.CLIType,lcAllowedDSS2SubsType) > 0)   AND
      (fCLITypeIsMainLine(bMsOwner.CLIType)   OR
       fCLITypeIsExtraLine(bMsOwner.CLIType))                THEN
      IF NOT fCheckActiveExtraLinePair(bMsOwner.MsSeq,
                                       bMsOwner.CLIType,
                                       OUTPUT lcBundleId) THEN
         NEXT.
   
   dss_struct = add_struct(top_array, "").
   ASSIGN ldeDataBundleLimit = 0
          lcDataBundle       = "".

   add_string(dss_struct, "cli", bMsOwner.CLI).
   add_double(dss_struct, "dss_limit", ldeDSSTotalLimit).

   FOR EACH bMServiceLimit WHERE
            bMServiceLimit.MsSeq   = bMsOwner.MsSeq AND
            bMServiceLimit.FromTS <= ldeCurrentTS   AND
            bMServiceLimit.EndTS  >= ldeCurrentTS NO-LOCK,
       FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
             bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
       FIRST bDayCampaign WHERE
             bDayCampaign.Brand = Syst.Var:gcBrand AND
             bDayCampaign.DCEvent = bServiceLimit.GroupCode NO-LOCK:

         IF LOOKUP(STRING(bDayCampaign.DCType),
                   {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
            bDayCampaign.DCType NE {&DCTYPE_POOL_RATING} THEN NEXT.

         IF bDayCampaign.DCEvent BEGINS {&DSS} OR
            LOOKUP(bDayCampaign.DCEvent,lcExcludeBundles) > 0 THEN NEXT.

         /* Check DSS upgrade upsell limit */
         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.MsSeq = bMsOwner.MsSeq AND
                    MsRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
                    MsRequest.ReqCParam3 = "DSS_UPSELL_UPGRADE" AND
                    MsRequest.ActStamp >= ldPeriodFrom AND
                    MsRequest.ActStamp <= ldPeriodTo   AND
                    MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} NO-ERROR.
         IF AVAILABLE MsRequest THEN
            ldeDataBundleLimit = ldeDataBundleLimit + MsRequest.ReqDparam1.

         /* All bundle + UPSELL limits for first month DSS */
         IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:
            FIND FIRST bMServiceLPool WHERE
                       bMserviceLPool.MsSeq   = bMServiceLimit.MsSeq  AND
                       bMserviceLPool.SLSeq   = bMServiceLimit.SLSeq  AND
                       bMserviceLPool.EndTS  >= ldPeriodFrom          AND
                       bMserviceLPool.FromTS <= ldPeriodTo NO-LOCK NO-ERROR.
            IF AVAILABLE bMserviceLPool THEN
               ldeDataBundleLimit = ldeDataBundleLimit + bMserviceLPool.LimitAmt.
         END. /* IF bDayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO: */
         ELSE DO:
            IF bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN
               ldeDataBundleLimit = ldeDataBundleLimit + bMServiceLimit.InclAmt.

            IF LOOKUP(bServiceLimit.GroupCode,lcIPLContracts) > 0 OR
               LOOKUP(bServiceLimit.GroupCode,lcCONTDContracts) > 0 OR
               LOOKUP(bServiceLimit.GroupCode,lcCONTSContracts) > 0 OR
               LOOKUP(bServiceLimit.GroupCode,lcFLATContracts) > 0 OR
               LOOKUP(bServiceLimit.GroupCode,lcCONTSFContracts) > 0 THEN
               lcDataBundle = bServiceLimit.GroupCode.
         END. /* ELSE DO: */
   END. /* FOR EACH bMServiceLimit WHERE */

   IF LOOKUP(bMsOwner.CLIType,lcBundleCLITypes) > 0 THEN
      add_string(dss_struct, "cli_type", bMsOwner.CLIType + lcDataBundle).
   ELSE
      add_string(dss_struct, "cli_type", bMsOwner.CLIType).
   add_double(dss_struct, "data_bundle_limit", ldeDataBundleLimit).

END. /* FOR EACH bMsOwner WHERE */

FINALLY:
   END.
