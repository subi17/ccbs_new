/**
 * Get upsell bundle count
 *
 * @input int;mandatory;subscription id
 * @output_struct 
           bundle;string;optional;main bundle id (CONTD1, CONTD2, CONTD3, MDUB, MDUB2)
           upsell_bundle;string;optional;upsell bundle id (CONTD1_UPSELL, CONTD2_UPSELL, CONTD3_UPSELL, MDUB_UPSELL, MDUB2_UPSELL)
           upsell_bundle_count;int;optional;number of upsell bundles
           btc_new_bundle;optional;string;BTC New bundle id (CONTD1, CONTD2, MDUB2, MDUB3)
           btc_upgrade_upsell;optional;boolean;BTC Upgrade Upsell (True)
           customer_bundle;string;optional;main bundle id (DSS)
           customer_upsell_bundle;string;optional;upsell bundle id (DSS_UPSELL)
           customer_upsell_bundle_count;int;optional;number of upsell bundles
 * @Exceptions  1;Subscription not found
                2;Upsell limit exceeded
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
Syst.Var:katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId.
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/upsellbundle.i}

DEF VAR piMsSeq AS INTEGER NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcContract AS CHAR NO-UNDO.
DEF VAR lcUpsellContract AS CHAR NO-UNDO. 
DEF VAR liUpsellCount AS INT NO-UNDO. 
DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR lcCustomerContract AS CHAR NO-UNDO.
DEF VAR lcBTCNewBundle     AS CHAR NO-UNDO.
DEF VAR ldeCurrentTS AS DEC NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id,"0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO ordercanal MobSub MsSeq piMsSeq}

ldeCurrentTS = Func.Common:mMakeTS().

lcResultStruct = add_struct(response_toplevel_id, "").

lcContract = fGetUpSellBasicContract(MobSub.MsSeq,
                                     MobSub.CustNum,
                                     MobSub.PayType,
                                     "MobSub",
                                     "",
                                     {&REQUEST_SOURCE_EXTERNAL_API}).
IF lcContract NE "" THEN DO:

   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = lcContract AND
              DayCampaign.ValidTo >= TODAY NO-LOCK NO-ERROR.
   IF AVAIL DayCampaign THEN
      lcUpsellContract = fGetDayCampaignUpsells(DayCampaign.DCEvent).

   liUpsellCount = fGetUpsellCount(
                      lcUpsellContract,
                      MobSub.MsSeq,
                      MobSub.CustNum,
                      OUTPUT lcError).
   IF lcError > "" AND
      NOT lcError BEGINS "Upsell limit exceeded" THEN RETURN appl_err(lcError).
   
   IF lcContract EQ "CONTDATA" THEN lcContract = "CONTD1".
   add_string(lcResultStruct,"bundle",lcContract).

   add_string(lcResultStruct,"upsell_bundle",lcUpsellContract).
   
   add_int(lcResultStruct,"upsell_bundle_count",liUpsellCount).

   FOR FIRST MsRequest NO-LOCK WHERE
             MsRequest.MsSeq   = MobSub.MsSeq  AND
             MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
             LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
       USE-INDEX MsSeq:
      IF MsRequest.ReqCparam2 = "CONTDATA" THEN lcBTCNewBundle = "CONTD1".
      ELSE lcBTCNewBundle = MsRequest.ReqCparam2.

      add_string(lcResultStruct,"btc_new_bundle",lcBTCNewBundle).

      IF MsRequest.ReqCparam5 > "" THEN
         add_boolean(lcResultStruct,"btc_upgrade_upsell",True).
   END. /* FOR FIRST MsRequest NO-LOCK WHERE */
END.

IF Mobsub.CliType EQ "TARJ6" THEN DO:
   
   FOR FIRST ServiceLimit NO-LOCK WHERE
             ServiceLimit.GroupCode = {&TARJ_UPSELL} AND
             ServiceLimit.ValidFrom <= TODAY  AND
             ServiceLimit.ValidTo   >= TODAY.
      /* there can be overlapping bundles, only use the latest one */
      liUpsellCount = 0.
      FOR EACH MServiceLimit NO-LOCK WHERE
         MServiceLimit.MsSeq    = Mobsub.MsSeq AND
         MServiceLimit.DialType = ServiceLimit.DialType AND
         MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
         MServiceLimit.EndTS   >= ldeCurrentTs AND
         MServiceLimit.FromTs  <= ldeCurrentTs:
         liUpsellCount = liUpsellCount + 1.
      END.

      add_string(lcResultStruct, "bundle", "TARJ_UPSELL").
      add_string(lcResultStruct, "upsell_bundle", "TARJ_UPSELL").
      add_int(lcResultStruct, "upsell_bundle_count", liUpsellCount).

   END.
END.

/* Only check if postpaid subscription */
IF Mobsub.PayType = FALSE THEN
   lcCustomerContract = fGetUpSellBasicContract(MobSub.MsSeq,
                                                MobSub.CustNum,
                                                MobSub.PayType,
                                                "Customer",
                                                "",
                                                {&REQUEST_SOURCE_EXTERNAL_API}).
IF lcCustomerContract NE "" THEN DO:
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = lcCustomerContract AND
              DayCampaign.ValidTo >= TODAY NO-LOCK NO-ERROR.
   IF AVAIL DayCampaign THEN
      lcUpsellContract = fGetDayCampaignUpsells(DayCampaign.DCEvent).

   liUpsellCount = fGetUpsellCount(
                      lcUpsellContract,
                      MobSub.MsSeq,
                      MobSub.CustNum,
                      OUTPUT lcError).
   IF lcError > "" AND
      NOT lcError BEGINS "Upsell limit exceeded" THEN RETURN appl_err(lcError).

   add_string(lcResultStruct,"customer_bundle",lcCustomerContract).

   add_string(lcResultStruct,"customer_upsell_bundle",lcUpsellContract).

   add_int(lcResultStruct,"customer_upsell_bundle_count",liUpsellCount).
END.

FINALLY:
   END.
