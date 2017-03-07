/**
 * Mnp process operation.
 *
 * @input   id;int;mnp id
            operation;struct;mandatory;operation data
 * @operation operation_id;string;mandatory;reject,confirm,confirm_cancel_proposal,reject_cancel_proposal,cancel or close
            reason_code;string;optional;mandatory for cancellation and rejection
            username;string;mandatory;newton username
            memo;struct;mandatory;
            pdf;base64;optional;pdf attachment (only with cancel operation)
 * @memo title;string;mandatory;
         content;string;mandatory
         source;string;mandatory
 * @output  result;struct;empty
 */

{xmlrpc/xmlrpc_access.i}
{Func/log.i}
{Syst/tmsconst.i}

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR pcId AS CHAR NO-UNDO.
DEF VAR piId AS INT NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR lcRespStruct AS CHAR NO-UNDO. 
DEF VAR ocError AS CHARACTER NO-UNDO. 
DEF VAR pcReason AS CHARACTER NO-UNDO. 
DEF VAR pcOperation AS CHARACTER NO-UNDO. 
DEF VAR pcMemoStruct AS CHARACTER NO-UNDO. 
DEF VAR pcMemoTitle AS CHARACTER NO-UNDO. 
DEF VAR pcMemoContent AS CHARACTER NO-UNDO. 
DEF VAR pcMemoSource  AS CHARACTER NO-UNDO.

FUNCTION store_base64 RETURN CHARACTER
      ( pparent AS CHAR,
        ppath AS CHAR):

    DEF VAR llcContents AS LONGCHAR NO-UNDO.
    DEF VAR lmpContents AS MEMPTR NO-UNDO.

    IF find_param(pparent, ppath) EQ ? THEN RETURN ERROR.
    IF tt_param.type NE "base64" THEN
        RETURN param_err(SUBST("Parameter `&1` at &2 must be a base64",
                                ppath, pparent)).
    COPY-LOB FROM FILE tt_param.cvalue TO llcContents.
    lmpContents = BASE64-DECODE(llcContents).
    COPY-LOB FROM lmpContents TO mnpcancelproposal.pdf.
    SET-SIZE(lmpContents) = 0.

    RETURN "".

END FUNCTION.

IF validate_request(param_toplevel_id, "int,struct") EQ ? THEN RETURN.

piId     = get_int(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").
lcstruct = validate_struct(pcStruct, "operation_id!,reason_code,username!,memo!,pdf").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").
pcMemoStruct = get_struct(pcStruct,"memo").
pcMemoTitle = get_string(pcMemoStruct,"title").
pcMemoContent = get_string(pcMemoStruct,"content").
pcMemoSource = get_string(pcMemoStruct,"source").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND MNPProcess NO-LOCK WHERE
     MNPProcess.MNPSeq = piId NO-ERROR.

IF NOT AVAIL MNPProcess THEN 
   RETURN appl_err("MNPProcess id not found: " + pcId).

ASSIGN
   pcOperation = get_string(pcStruct, "operation_id").

IF LOOKUP(pcOperation,"cancel,reject") > 0 THEN 
   pcReason = get_string(pcStruct, "reason_code"). 

IF pcOperation = "cancel" AND LOOKUP("pdf",lcstruct) > 0 THEN DO:

   FIND FIRST MNPCancelProposal WHERE
              MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq AND
              MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_RECIPIENT}
   NO-LOCK NO-ERROR.

   IF AVAIL MNPCancelProposal THEN 
      RETURN appl_err("MNP cancellation PDF already exists").
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

{Syst/commpaa.i}
katun = pcUsername.
gcBrand = "1".

IF pcOperation = "cancel" AND LOOKUP("pdf",lcstruct) > 0 THEN DO:
   
   CREATE MNPCancelProposal.
   ASSIGN
      MNPCancelProposal.AttachmentFile = MNPProcess.FormRequest + ".zip"
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_RECIPIENT}
      MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq
      MNPCancelProposal.CreatedTS = {&nowts}. 

   store_base64(pcStruct,"pdf").

   IF gi_xmlrpc_error NE 0 THEN DO:
      DELETE MNPCancelProposal.
      RETURN.
   END.
END.

RUN Mnp/mnp_operation.p(MNPProcess.MNPSeq,pcOperation,pcReason).

IF RETURN-VALUE NE "OK" THEN DO:
   IF AVAIL MNPCancelProposal THEN DELETE MNPCancelProposal.
   IF RETURN-VALUE BEGINS "ERROR:" THEN
        RETURN appl_err(ENTRY(2,RETURN-VALUE,":")). 
   ELSE RETURN appl_err(RETURN-VALUE). 
END.

CREATE Memo.
ASSIGN
    Memo.CreStamp  = {&nowTS}
    Memo.Brand     = gcBrand
    Memo.HostTable = "MNPProcess"
    Memo.KeyValue  = STRING(MNPProcess.MNPSeq)
    Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
    Memo.CreUser   = pcUsername
    Memo.MemoTitle = pcMemoTitle
    Memo.MemoText  = pcMemoContent
    Memo.Source    = pcMemoSource.

IF lookup(pcOperation,"cancel,close") > 0 THEN DO:
   
   FIND FIRST Order WHERE
              Order.Brand = gcBrand AND
              Order.OrderID = MNPProcess.OrderId NO-LOCK NO-ERROR.

   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "Order"
       Memo.KeyValue  = STRING(MNPProcess.OrderId)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = pcUsername
       Memo.MemoTitle = pcMemoTitle
       Memo.MemoText  = pcMemoContent
       Memo.CustNum   = Order.Custnum WHEN AVAIL Order
       Memo.Source    = pcMemoSource.

END.

lcRespStruct = add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
