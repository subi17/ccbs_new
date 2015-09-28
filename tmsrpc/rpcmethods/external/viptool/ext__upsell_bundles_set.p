/**
 * Creates upsell bundle. If upsell limit is reached sends SMS notification
 *
 * @input int;mandatory;subscription id
          string;mandatory;upsell bundle id (CONTD1_UPSELL, CONTD2_UPSELL, MDUB_UPSELL, MDUB2_UPSELL)
 *          
 * @output boolean;true
 * @Exceptions  1;Subscription not found
                2;Customer not found
*/

{xmlrpc/xmlrpc_access.i}

DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{tmsconst.i}
{upsellbundle.i}
{fgettxt.i}
{fprepaidfee.i}

DEF VAR pcUpsellId       AS CHAR NO-UNDO.
DEF VAR piMsSeq          AS INT  NO-UNDO. 
DEF VAR lcError          AS CHAR NO-UNDO. 
DEF VAR liRequest        AS INT  NO-UNDO.
DEF VAR ldeSMSStamp      AS DEC  NO-UNDO. 
DEF VAR llResult         AS LOG  NO-UNDO.
DEF VAR ldeBundleFee     AS DEC  NO-UNDO.

IF validate_request(param_toplevel_id, "int,string") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id,"0").
pcUpsellId = get_string(param_toplevel_id,"1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub  WHERE 
           MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN RETURN appl_err("Subscription not found").
      
FIND FIRST Customer WHERE
           Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN RETURN appl_err("Customer not found"). 

IF pcUpsellId = "CONTD1_UPSELL" THEN pcUpsellId = "CONTDATA_UPSELL".

IF pcUpsellId EQ {&TARJ_UPSELL} THEN DO:

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

add_boolean(response_toplevel_id,"",True).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
