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

{xmlrpc/xmlrpc_access.i}

DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{tmsconst.i}
{upsellbundle.i}
{fgettxt.i}
{fexternalapi.i}

DEF VAR pcUpsellId         AS CHAR NO-UNDO.
DEF VAR pcCLI              AS CHAR NO-UNDO.
DEF VAR pcTransId          AS CHAR NO-UNDO.
DEF VAR top_struct         AS CHAR NO-UNDO.

DEF VAR lcError            AS CHAR NO-UNDO. 
DEF VAR liRequest          AS INT  NO-UNDO.
DEF VAR ldeSMSStamp        AS DEC  NO-UNDO. 
DEF VAR llResult           AS LOG  NO-UNDO. 
DEF VAR ldeBundleFee       AS DEC  NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string") EQ ? THEN RETURN.

ASSIGN pcTransId  = get_string(param_toplevel_id, "0")
       pcCLI      = get_string(param_toplevel_id,"1")
       pcUpsellId = get_string(param_toplevel_id,"2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NOT fchkTMSCodeValues(gbAuthLog.UserName,substring(pcTransId,1,3)) THEN
   RETURN appl_err("Application Id does not match").

FIND FIRST MobSub  WHERE 
           MobSub.CLI = pcCLI NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN appl_err("Subscription not found").
      
FIND FIRST Customer WHERE
           Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found"). 

IF pcUpsellId = "CONTD1_UPSELL" THEN pcUpsellId = "CONTDATA_UPSELL".

FIND FIRST DayCampaign NO-LOCK WHERE
           DayCampaign.Brand = gcBrand AND
           DayCampaign.DCEvent = pcUpsellId NO-ERROR.
IF NOT AVAIL DayCampaign THEN RETURN appl_err("DayCampaign not defined").

   
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
      ldeBundleFee = fCParamDe("TARJ_UPSELLFee").
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


CREATE Memo.
ASSIGN
      Memo.CreStamp  = {&nowTS}
      Memo.Brand     = gcBrand 
      Memo.HostTable = "MobSub" 
      Memo.KeyValue  = STRING(MobSub.MsSeq) 
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.CreUser   = katun 
      Memo.MemoTitle = DayCampaign.DCName
      Memo.MemoText  = "External API upsell"
      Memo.CustNum   = MobSub.CustNum
      Memo.MemoType  = "Service".

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
