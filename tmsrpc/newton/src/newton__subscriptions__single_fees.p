/**
 * Get all SingleFee items of charge and compensation 
 * for an specific mobssub
 * 
 *
 * @input msseq;int;mandatory;id of subscription
          counts;int;mandatory;number of single fee to show
 * @output singlefee;array of struct;singlefee data
 * @singlefee billing_item_id;string;
              amount;double;
              period;int;
              billed_on_invoice;int;invoice number
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEFINE VARIABLE piMsSeq     AS INTEGER   NO-UNDO.
DEFINE VARIABLE piCount     AS INTEGER   NO-UNDO.

/* Output parameters */
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFeeStruct  AS CHARACTER NO-UNDO.

/* Local variables */
DEFINE VARIABLE lcBrand       AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE icount        AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,int") EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
piCount = get_int(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

/* Check that mobsub is available */
FIND MobSub WHERE
     MobSub.MsSeq = piMsSeq
NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN DO:
   RETURN appl_err("System Error ! Mobile Subscription not available").
END.



resp_array = add_array(response_toplevel_id, "").

FOR EACH SingleFee  NO-LOCK WHERE
         SingleFee.Brand = lcBrand AND   
         SingleFee.HostTable = "MobSub" AND
         SingleFee.KeyValue  = STRING(piMsSeq) AND
         SingleFee.BillType  = "CC" BY BillPeriod DESCENDING :

    lcFeeStruct = add_struct(resp_array,"").
    add_string(lcFeeStruct,"billing_item_id",SingleFee.BillCode).
    add_double(lcFeeStruct,"amount",SingleFee.Amt).
    add_int(lcFeeStruct,"period",SingleFee.BillPeriod).
    add_int(lcFeeStruct,"billed_on_invoice",SingleFee.InvNum).
   
   icount = icount + 1.
   IF icount EQ piCount THEN LEAVE.

END.









