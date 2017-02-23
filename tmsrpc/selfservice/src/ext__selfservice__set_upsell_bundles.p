/**
 * Creates upsell bundle. If upsell limit is reached sends SMS notification
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;subscription msisdn number
           upsell_bundle_id;string;mandatory;upsell bundle id(eg:CONTD1_UPSELL/MDUB_UPSELL/DSS_UPSELL,HSPA_ROAM_EU)
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions  1;Subscription not found
                2;Customer not found
                3;Upsell limit exceeded
                4;Data contract does not exist
                5;Incorrect upsell type
                6;Upsell is not allowed because DSS is active for this customer
                7;Not enough balance
                8;Application Id does not match
                9;Bundle request not created
               10;Bundle Upsell is not allowed for this subscription type
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId.
gcBrand = "1".
{Syst/tmsconst.i}
{Func/upsellbundle.i}
{Func/fgettxt.i}
{Func/fexternalapi.i}
{Func/fprepaidfee.i}

DEF VAR pcUpsellId          AS CHAR NO-UNDO.
DEF VAR pcCLI               AS CHAR NO-UNDO.
DEF VAR pcTransId           AS CHAR NO-UNDO.
DEF VAR top_struct          AS CHAR NO-UNDO.

DEF VAR lcError             AS CHAR NO-UNDO.
DEF VAR liRequest           AS INT  NO-UNDO.
DEF VAR ldeSMSStamp         AS DEC  NO-UNDO.
DEF VAR llResult            AS LOG  NO-UNDO.
DEF VAR ldeBundleFee        AS DEC  NO-UNDO.
DEF VAR lcApplicationId     AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId      AS CHAR NO-UNDO.
DEF VAR secondsFromPrevious AS INT  NO-UNDO.
DEF VAR lcMemoText          AS CHAR NO-UNDO.
DEF VAR lcMemoTitle         AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string") EQ ? THEN RETURN.

ASSIGN pcTransId  = get_string(param_toplevel_id, "0")
       pcCLI      = get_string(param_toplevel_id,"1")
       pcUpsellId = get_string(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub Cli pcCLI}

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = ghAuthLog::EndUserId.

IF NOT fchkTMSCodeValues(ghAuthLog::UserName,lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

/*YPR-4775*/
/*(De)Activation is not allowed if fixed line provisioning is pending*/
IF MobSub.MsStatus EQ {&MSSTATUS_FIXED_PROV_ONG} /*16*/ THEN
   RETURN appl_err("Mobile line provisioning is not complete").


/* YDR-1783 Check that previous request is not done during
   previous five minutes from external api */
IF CAN-FIND( FIRST MsRequest NO-LOCK WHERE
                   MsRequest.MsSeq = Mobsub.MsSeq AND
                   MsRequest.ActStamp > fSecOffSet(fMakeTS(),-300) AND
                   MsRequest.ReqType = 8 AND
                   MsRequest.ReqCParam3 = pcUpsellId AND
                   MsRequest.ReqSource = {&REQUEST_SOURCE_EXTERNAL_API}
                   USE-INDEX MsActStamp) THEN
   RETURN appl_err("The requested activation was not handled because " +
                   "there is a too recent activation.").

FIND FIRST Customer WHERE
           Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found"). 

IF pcUpsellId = "CONTD1_UPSELL" THEN pcUpsellId = "CONTDATA_UPSELL".

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = gcBrand AND
           DayCampaign.DCEvent = pcUpsellId NO-ERROR.
IF NOT AVAIL DayCampaign THEN RETURN appl_err("DayCampaign not defined").

/* DATA200_UPSELL and DSS200_UPSELL will only be allowed
   to activate from Landing page (application id - 505) */
IF (pcUpsellId = "DATA200_UPSELL" OR
    pcUpsellId = "DSS200_UPSELL") AND
    lcApplicationId <> "505" THEN
   RETURN appl_err(SUBST("&1 activation is allowed from LP only", pcUpsellId)).

IF pcUpsellId EQ {&HSPA_ROAM_EU} OR pcUpsellId EQ {&TARJ_UPSELL} THEN DO:

   /* Check if subscription type is not compatible with bundle */
   IF fMatrixAnalyse(gcBrand,
                     "PERCONTR",
                     "PerContract;SubsTypeTo",
                     pcUpsellId + ";" + MobSub.CLIType,
                     OUTPUT lcError) NE 1 THEN DO:
      RETURN appl_err("Bundle Upsell is not allowed for this subscription type").
   END.
      
   /* Validate Prepaid Balance before making TARJ UPSell activation request */
   IF pcUpsellId = {&TARJ_UPSELL} THEN DO:
      ldeBundleFee = fgetPrepaidFeeAmount(pcUpsellId, TODAY).
      RUN pEnoughBalance(INPUT MobSub.CLI,
                         INPUT ldeBundleFee,
                         OUTPUT llResult).
      IF NOT llResult THEN DO:
         RUN pSendSMS(INPUT MobSub.MsSeq,
                      INPUT 0,
                      INPUT "UpsellTARJ6NoBal",
                      INPUT 10,
                      INPUT "22622",
                      INPUT "").
         RETURN appl_err("Not enough balance").
      END.
   
   END.
   liRequest = fPCActionRequest(MobSub.MsSeq,
                                pcUpsellId,
                                "act",
                                fMakeTS(),
                                TRUE, /* fees */
                                {&REQUEST_SOURCE_EXTERNAL_API},
                                "",   /* creator */
                                0,    /* no father request */
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT lcError).
   IF liRequest = 0 THEN RETURN appl_err("Bundle request not created"). 
END.
ELSE IF NOT fCreateUpsellBundle(
   MobSub.MsSeq,
   pcUpsellId,
   {&REQUEST_SOURCE_EXTERNAL_API},
   fMakeTS(),
   OUTPUT liRequest,
   OUTPUT lcError) THEN DO:

   IF lcError EQ "Upsell limit exceeded" THEN DO:
      
      lcSMSText = fGetSMSTxt("UpsellLimit",
                             TODAY,
                             Customer.Language,
                             OUTPUT ldeSMSStamp).
      
      IF lcSMSText > "" THEN
         fMakeSchedSMS2 (
             Customer.CustNum,
             MobSub.CLI,
             1,
             lcSMSText,
             ldeSMSStamp,
             "622",
             "").

   END.
   RETURN appl_err(lcError).
END.

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = gcBrand AND
           DayCampaign.DCEvent = pcUpsellId NO-ERROR.

ASSIGN lcMemoText = IF INDEX(Daycampaign.DCName,"Ampliación")>0 THEN
                       DayCampaign.DCName + " - Activar"
                    ELSE "Ampliación " + DayCampaign.DCName + " - Activar"
       lcMemoTitle = DayCampaign.DCName.


IF pcUpsellId = "DATA200_UPSELL" THEN ASSIGN
   lcMemoTitle = "Ampliación 200 MB"
   lcMemoText = "Ampliación 200 MB".
ELSE IF pcUpsellId = "DSS200_UPSELL" THEN ASSIGN
   lcMemoTitle = "Ampliación 200 MB"
   lcMemoText  = "Internet compartido - Ampliación 200 MB".
          

DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                 "MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 lcMemoTitle,                          /* MemoTitle */
                 lcMemoText,                           /* MemoText */
                 "Service",                            /* MemoType */
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
