/**
 * Send MNP In cancellation code SMS from Web Online Order Tracking.
 * User need to feed the correct code to be able to proceed
 * MNP in cancellation. This verifies that the user is the number owner.
 *
 * @input      transaction_id;string;mandatory;transaction id
 *             order_id;int;mandatory;order id
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions 1;Application Id does not match
               2;Order does not exist
               3;CLI is empty
               4;SMS template Does not exist
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{Syst/commpaa.i}
ASSIGN katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId
       gcBrand = "1".
{Func/fgettxt.i}
{Func/fexternalapi.i}
{Func/fmakesms.i}

DEF VAR pcTransId       AS CHAR NO-UNDO.    /* Transaction ID */
DEF VAR piOrderId       AS INT  NO-UNDO.    /* Order ID */
DEF VAR lcApplicationId AS CHAR NO-UNDO.    /* Application ID */
DEF VAR top_struct      AS CHAR NO-UNDO.    /* Returned response */
DEF VAR lcSMSText       AS CHAR NO-UNDO.    /* SMS text to be send */
DEF VAR ldReqStamp      AS DEC  NO-UNDO.    /* Time when SMS will be send */
DEF VAR liLang          AS INTEGER NO-UNDO. /* Which language wanted */
DEF VAR liNumber        AS INTEGER NO-UNDO. /* Random number for SMS */
DEF VAR lcCli           AS CHAR NO-UNDO.    /* Number where SMS send */
DEF VAR lcCheckNbr      AS CHAR NO-UNDO.    /* Given char number inserted */
DEF VAR liCheckNbr      AS INTEGER NO-UNDO. /* Given number to be checked */

IF validate_request(param_toplevel_id, "string,int") EQ ? THEN RETURN.

ASSIGN
   pcTransId = get_string(param_toplevel_id, "0")
   piOrderId = get_int(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + gbAuthLog.EndUserId.

FIND FIRST Order WHERE
           Order.Brand   = gcBrand   AND
           Order.OrderId = piOrderId NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN
   RETURN appl_err("Order does not exist").

IF Order.CLI EQ "" THEN
   RETURN appl_err("Mobile number does not exist").

liLang = 1.    /* Set language to Spanish */

/* Fetch SMS text from message management */
lcSMSText = fGetSMSTxt( "MNPInCancelOnline", /* KeyValue for text search */
                        TODAY,               /* Valid from/to */
                        liLang,              /* Language */
                        OUTPUT ldReqStamp).  /* Sending time */

IF lcSMSText = "" THEN
   RETURN appl_err("Missing SMS Template").

/* #code in SMS text need to be replaced with generated random number */
liNumber = RANDOM(1000,9999).
lcSMSText = REPLACE(lcSMSText,"#code",STRING(liNumber)).

/* Reguirement to delay SMS sending by Yoigo */
ldReqStamp = fSecOffSet(ldReqStamp, RANDOM(105,120)).

/* Send SMS to customer */
fMakeSchedSMS2(0,       /* Customer number not known */
               Order.CLI,
               {&SMSTYPE_MNP_CANCEL},  /* Credit Type for this SMS */
               lcSMSText,
               ldReqStamp,
               "622",            /* Sender number */
               "").              /* Time when SMS can be send */

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
