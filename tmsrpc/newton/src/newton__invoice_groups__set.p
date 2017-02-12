/*
 * Set Invoice Group
 *
 * @input  id;int; Invoice Group id
           invoice_group;struct; Invoice Group data 
 * @invoice_group default;boolean; set as default Invoice Group
                  active;boolean; set it active 
                  subscriptions;array; array of Invoice Target to update
                  username;string;mandatory; user name
                  reason;string;mandatory;
 * @output empty
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
gcBrand = "1".
{Mc/invoicetarget.i}

FUNCTION fGetExcludedList RETURNS CHAR 
         (icCheckList AS CHAR,
          icFromList AS CHAR):

   DEF VAR li AS INT NO-UNDO. 
   DEF VAR lcValue As CHAR NO-UNDO.
   DEF VAR lcReturnList AS CHAR NO-UNDO.

   DO li = 1 TO NUM-ENTRIES(icCheckList) :
     lcValue = ENTRY(li,icCheckList).
     IF LOOKUP(lcValue,icFromList) = 0 THEN DO:
        IF lcReturnList NE "" THEN 
           lcReturnList = lcReturnList + "," + lcValue .
        ELSE 
           lcReturnList = lcValue. 
     END.
   END.
   RETURN lcReturnList.
END FUNCTION.



DEF VAR piITGroupId AS INT NO-UNDO. 
DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR piMsSeq AS INT NO-UNDO. 
DEF VAR pcArrayInvoiceTarget AS CHAR NO-UNDO.
DEF VAR plDefaultGroup AS LOG NO-UNDO INITIAL FALSE.
DEF VAR plActive AS LOG NO-UNDO INITIAL TRUE. 
DEF VAR pcReason AS CHAR NO-UNDO.
DEF VAR pcUserName AS CHAR NO-UNDO.
DEF VAR pcInvoiceTarget AS CHAR NO-UNDO.
DEF VAR liCustNum AS INTEGER NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO.
DEF VAR lcListInputId AS CHAR NO-UNDO. 
DEF VAR lcListCurrId AS CHAR NO-UNDO.
DEF VAR lcListAddId AS CHAR NO-UNDO.
DEF VAR lcListRemoveId AS CHAR NO-UNDO. 
DEF VAR liDefITGroupId AS INT NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.
DEF VAR resp_struct AS CHAR NO-UNDO.
DEF VAR llSetDefaultGroup AS LOG NO-UNDO INITIAL FALSE.
DEF VAR liActAction AS INT NO-UNDO INITIAL 0. 

IF validate_request(param_toplevel_id, "int,struct") EQ ? THEN RETURN.
piITGroupId = get_int(param_toplevel_id,"0").
pcStruct = get_struct(param_toplevel_id, "1").
lcStruct = validate_request(pcStruct,"default,active,subscriptions,username!,reason").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUserName = "VISTA_" + get_string(pcStruct,"username").
katun = pcUserName. 
IF LOOKUP("reason",lcStruct) > 0 THEN 
pcReason = get_string(pcStruct,"reason").
IF LOOKUP("default",lcStruct) > 0 THEN 
plDefaultGroup = get_bool(pcStruct,"default").
IF LOOKUP("active",lcStruct) > 0 THEN
plActive =  get_bool(pcStruct,"active").

pcArrayInvoiceTarget = get_array(pcStruct,"subscriptions").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO Common InvoiceTargetGroup ITGroupID piITGroupId}

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

/* validate InvoiceTargetGroup */
FIND InvoiceTargetGroup WHERE
     InvoiceTargetGroup.ITGroupID = piITGroupId NO-LOCK NO-ERROR. 
IF NOT AVAIL InvoiceTargetGroup THEN RETURN appl_err("Invoice Target Group not found").

IF InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL} OR
   InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} THEN
   RETURN appl_err("Fusion Invoice Target Group can not be changed").

/* set up update values */
liCustNum = InvoiceTargetGroup.CustNum. 
IF plDefaultGroup NE InvoiceTargetGroup.DefaultGroup THEN 
   llSetDefaultGroup = plDefaultGroup. 
IF plActive NE ( IF InvoiceTargetGroup.ToDate > TODAY THEN TRUE ELSE FALSE) THEN DO:
   CASE plActive :
   WHEN TRUE THEN liActAction = 1. /*activate */
   WHEN FALSE THEN liActAction = 2. /* deactivate */
   END CASE.
END.

liDefITGroupId = fGetDefaultInvoiceTargetGroup(liCustNum). 
/* create list of currents InvoiceTargets in this group */
FOR EACH InvoiceTarget NO-LOCK WHERE
         InvoiceTarget.ITGroupID = piITGroupId AND
         InvoiceTarget.ToDate > TODAY :
         
         IF NOT fIsActiveInvoiceTarget(InvoiceTarget.InvoiceTargetId,
                                       "",
                                       OUTPUT lcError) THEN NEXT.
         
         IF lcListCurrId NE "" THEN 
            lcListCurrId = lcListCurrId + "," + STRING(InvoiceTarget.InvoiceTargetId) .
         ELSE 
            lcListCurrId = STRING(InvoiceTarget.InvoiceTargetId). 
END.

/* create the Add list */
lcListAddId = fGetExcludedList(lcListInputId,lcListCurrId).
/* create the Remove list */
lcListRemoveId = fGetExcludedList(lcListCurrId,lcListInputId).

update_group:
DO TRANS:
   
   /* move from this group to default */
   IF NOT fMoveListOfInvoiceTarget(lcListRemoveId,
                               liDefITGroupId,
                               OUTPUT lcError)
   THEN UNDO update_group, RETURN appl_err(lcError).

   /* move from other groups to this group */
   IF NOT fMoveListOfInvoiceTarget(lcListAddId,
                               piITGroupID,
                               OUTPUT lcError)
   THEN UNDO update_group, RETURN appl_err(lcError).

   IF liActAction = 1 THEN 
      IF NOT fActivateInvoiceTargetGroup(piITGroupID,
                                    OUTPUT lcError) 
      THEN UNDO update_group, RETURN appl_err(lcError).

   IF liActAction = 2 THEN 
      IF NOT fDeactivateInvoiceTargetGroup(piITGroupID,
                                    OUTPUT lcError) 
      THEN UNDO update_group, RETURN appl_err(lcError).

   IF llSetDefaultGroup THEN 
      IF NOT fSetDefaultInvoiceTargetGroup(piITGroupID,
                                           OUTPUT lcError) 
         THEN UNDO update_group, RETURN appl_err(lcError).

   IF pcReason NE '' THEN DO:
      CREATE Memo.
      ASSIGN
          Memo.CreStamp  = {&nowTS}
          Memo.Brand     = gcBrand 
          Memo.HostTable = "Customer" 
          Memo.KeyValue  = STRING(InvoiceTargetGroup.CustNum) 
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = "Invoice Target Group Updates"
          Memo.MemoText  = pcReason
          Memo.CustNum   = InvoiceTargetGroup.CustNum. 
   END.

END.

resp_struct = add_struct(response_toplevel_id, "").

FINALLY:
IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
