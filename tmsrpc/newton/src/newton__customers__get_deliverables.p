/**
 * Get Customer deliverables 
 *
 * @input   int;integer;customer number
 * @output  data;struct;
 * @data    delivery_channel;string;mandatory;Delivery channel
            delivery_channel_fusion;string;optional;Fusion delivery channel
            invoice_target;string;mandatory;all_grouped/all_split/customized
            itemizations;array;mandatory;array of items
 * @item    msseq;int;mandatory;subscripton id   
            msisdn;string;mandatory;CLI
            callspec;string;mandatory;call itemization in miyoigo/paper/email invoice
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Mc/invoicetarget.i}

DEF VAR piCustNum       AS INT  NO-UNDO.   
DEF VAR lcTopStruct     AS CHAR NO-UNDO.
DEF VAR lcItemsArray    AS CHAR NO-UNDO.
DEF VAR lcItemStruct    AS CHAR NO-UNDO.
DEF VAR lcError         AS CHAR NO-UNDO.
DEF VAR lcDType         AS CHAR NO-UNDO.
DEF VAR lcInvoiceTarget AS CHAR NO-UNDO.
DEF VAR liGroupCount    AS INT  NO-UNDO.
DEF VAR liSubCount      AS INT  NO-UNDO.
DEF VAR lcFusionDelType AS CHAR NO-UNDO. 
DEF VAR lcFusionSubsType AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "int") = ? THEN RETURN.
piCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND Customer WHERE
     Customer.CustNum = piCustNum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN 
   RETURN appl_err("Not found Customer with custnum " + STRING(piCustNum)).

lcFusionSubsType = fCParamC("FUSION_SUBS_TYPE").

lcTopStruct = add_struct(response_toplevel_id,"").

lcDType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "Invoice",
                           "DelType",
                           STRING(Customer.DelType)).
add_string(lcTopStruct,"delivery_channel",lcDType).

lcInvoiceTarget = fGetCustomerCurrentGrouping(Customer.CustNum, 
                                              OUTPUT liGroupCount,
                                              OUTPUT liSubCount).
add_string(lcTopStruct,"invoice_target",lcInvoiceTarget).

lcItemsArray = add_array(lcTopStruct, "itemizations").

FOR EACH InvoiceTargetGroup NO-LOCK WHERE
         InvoiceTargetGroup.CustNum = piCustNum,
    EACH InvoiceTarget OF InvoiceTargetGroup NO-LOCK WHERE
         InvoiceTarget.ToDate > TODAY:

    FIND MobSub WHERE
         MobSub.MsSeq = InvoiceTarget.MsSeq NO-LOCK NO-ERROR. 
    IF NOT AVAIL MobSub THEN NEXT.

    IF MobSub.PayType THEN NEXT.
   
    IF lcFusionDelType EQ "" AND
       LOOKUP(MobSub.CLIType,lcFusionSubsType) > 0 THEN DO: 
      
      lcFusionDelType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "Invoice",
                                 "DelType",
                                 STRING(invoicetargetgroup.DelType)).
      IF lcFusionDelType EQ "" THEN
         lcFusionDelType = STRING(invoicetargetgroup.DelType).
      add_string(lcTopStruct,"delivery_channel_fusion",lcFusionDelType).
    END.
    
    lcItemStruct = add_struct(lcItemsArray,"").
    add_int(lcItemStruct,"msseq",MobSub.MsSeq).
    add_string(lcItemStruct,"msisdn",MobSub.CLI).

    FIND FIRST SubSer WHERE 
               SubSer.MsSeq = InvoiceTarget.MsSeq AND  
               SubSer.ServCom = "CALLSPEC" AND 
               SubSer.SSDate <= TODAY  USE-INDEX ServCom NO-LOCK NO-ERROR. 
    IF AVAIL SubSer AND SubSer.SSStat = 1 THEN 
       add_string(lcItemStruct,"callspec","on").
    ELSE
       add_string(lcItemStruct,"callspec","off").
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

