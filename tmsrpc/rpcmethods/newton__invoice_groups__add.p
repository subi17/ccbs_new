/*
 * Add Invoice Group
 *
 * @input  invoice_group;struct; Invoice Group data 
 * @invoice_group default;boolean; set as default Invoice Group
                  active;boolean; set it active 
                  subscriptions;array; array of Invoice Target to update
                  username;string;mandatory; user name
                  reason;string;mandatory;
 * @output empty
 */

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
gcBrand = "1".
{invoicetarget.i}

DEF VAR piCustNum AS INT NO-UNDO.
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR liITGroupId AS INT NO-UNDO. 
DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR liInvoiceTargetId AS INT NO-UNDO. 
DEF VAR pcArrayInvoiceTarget AS CHAR NO-UNDO.
DEF VAR plDefaultGroup AS LOG NO-UNDO INITIAL FALSE.
DEF VAR plActive AS LOG NO-UNDO INITIAL TRUE. 
DEF VAR pcReason AS CHAR NO-UNDO.
DEF VAR pcUserName AS CHAR NO-UNDO.
DEF VAR pcInvoiceTarget AS CHAR NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO.
DEF VAR lcListInputId AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR resp_struct AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
lcStruct = validate_request(pcStruct,"customer_id!,default,active,subscriptions,username!,reason").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = "VISTA_" + get_string(pcStruct,"username").
piCustNum = get_int(pcStruct,"customer_id").
katun = pcUserName. 
IF LOOKUP("reason",lcStruct) > 0 THEN 
pcReason = get_string(pcStruct,"reason").
IF LOOKUP("default",lcStruct) > 0 THEN 
plDefaultGroup = get_bool(pcStruct,"default").
IF LOOKUP("active",lcStruct) > 0 THEN
plActive =  get_bool(pcStruct,"active").
IF NOT plActive THEN RETURN appl_err("param: active,invalid value").

pcArrayInvoiceTarget = get_array(pcStruct,"subscriptions").
IF gi_xmlrpc_error NE 0 THEN RETURN.
DO liCount = 0 TO get_paramcount(pcArrayInvoiceTarget) - 1:
   piMsSeq= get_int(pcArrayInvoiceTarget,STRING(liCount)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   FIND InvoiceTarget WHERE
        InvoiceTarget.MsSeq = piMsSeq AND 
        InvoiceTarget.ToDate > TODAY NO-LOCK NO-ERROR. 
   IF NOT AVAIL InvoiceTarget THEN 
      RETURN appl_err("Not found Invoice Target with msseq " + STRING(piMsSeq)).

   FIND FIRST InvoiceTargetGroup NO-LOCK WHERE
              InvoiceTargetGroup.ITGroupId = InvoiceTarget.ITGroupId AND
              InvoiceTargetGroup.ToDate >= TODAY NO-ERROR.
   IF AVAIL InvoiceTargetGroup AND
      (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL} OR
       InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}) THEN
      RETURN appl_err("Fusion Invoice group cannot be modified").

   IF NOT fIsActiveInvoiceTarget(InvoiceTarget.InvoiceTargetId,
                                 "",
                                OUTPUT lcError) THEN 
      RETURN appl_err("Not found active invoice target with msseq " + STRING(piMsSeq)).

   IF lcListInputId NE "" THEN
      lcListInputId = lcListInputId + "," + STRING(InvoiceTarget.InvoiceTargetId).
   ELSE
      lcListInputId = STRING(InvoiceTarget.InvoiceTargetId).
END.

create_group:
DO TRANS:
  
   liITGroupID = fAddInvoiceTargetGroup(piCustNum,
                 ?,
                 OUTPUT lcError). 
   IF lcError NE "" THEN UNDO create_group, RETURN appl_err(lcError).

   IF NOT fMoveListOfInvoiceTarget(lcListInputId,
                                   liITGroupID,
                                   OUTPUT lcError)
   THEN UNDO create_group, RETURN appl_err(lcError).
   
   IF plDefaultGroup THEN 
      IF NOT fSetDefaultInvoiceTargetGroup(liITGroupID,
                                    OUTPUT lcError) 
      THEN UNDO create_group, RETURN appl_err(lcError).

   IF pcReason NE '' THEN DO:
      CREATE Memo.
      ASSIGN
          Memo.CreStamp  = {&nowTS}
          Memo.Brand     = gcBrand 
          Memo.HostTable = "Customer" 
          Memo.KeyValue  = STRING(piCustNum) 
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = "Invoice Target Group Add"
          Memo.MemoText  = pcReason
          Memo.CustNum   = piCustNum. 
   END.

END.

resp_struct = add_struct(response_toplevel_id, "").
add_string(resp_struct, "id", STRING(liITGroupID)).

FINALLY:
IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
