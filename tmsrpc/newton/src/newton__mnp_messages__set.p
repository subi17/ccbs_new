/**
 * For setting error mnp xml message as handled
 *
 * @input id;string;mandatory;mnp message id
          mnpmessage;struct;mnp message data
 * @mnpmessage operation_id;mandatory;string;handle_error/resend
          memo;struct;mandatory;
          username;string;mandatory;
 * @memo title;string;mandatory;
         content;string;mandatory

 * @output struct;empty
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEFINE VARIABLE pcID AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMNPOperationID AS INTEGER NO-UNDO. 
DEFINE VARIABLE pcUsername AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcMemoStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcMemoTitle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcMemoContent AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcOperation AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.
pcId = get_string(param_toplevel_id, "0").
pcStruct = get_struct(param_toplevel_id, "1").

lcstruct = validate_struct(pcStruct, "operation_id!,memo!,username!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcOperation = get_string(pcStruct, "operation_id")
   pcMemoStruct = get_struct(pcStruct,"memo")
   pcUserName = "VISTA_" + get_string(pcStruct, "username")
   pcMemoTitle = get_string(pcMemoStruct,"title")
   pcMemoContent = get_string(pcMemoStruct,"content").

IF gi_xmlrpc_error NE 0 THEN RETURN.
   
liMNPOperationID = INT(pcId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN
   appl_err("Invalid key: " + pcID).

FIND MNPOperation EXCLUSIVE-LOCK WHERE
     MNPOperation.MNPOperationID = liMNPOperationID NO-ERROR.
IF NOT AVAIL MNPOperation THEN RETURN
   appl_err("MNP message not found: " + pcID).

IF MNPOperation.ErrorHandled NE ({&MNP_ERRORHANDLED_NO}) THEN
   RETURN appl_err("Operation is allowed only for unhandled error").

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = pcUserName.
{Syst/eventval.i}
{Syst/tmsconst.i}
{Mnp/mnpoperation.i}
{Func/log.i}

CASE pcOperation:
   WHEN "handle_error" THEN .
   WHEN "resend" THEN fResendMNPMessage(BUFFER MNPOperation, OUTPUT lcError).
   OTHERWISE lcError = "Incorrect operation_id value: " + pcOperation.
END.

IF lcError NE "" THEN DO:
   RETURN appl_err(lcError).
END.

IF MNPOperation.ErrorHandled EQ ({&MNP_ERRORHANDLED_NO}) THEN DO:

   IF llDoEvent THEN DO:
      &GLOBAL-DEFINE STAR_EVENT_USER katun 
      {Func/lib/eventlog.i}
      DEF VAR lhMNPOperation AS HANDLE NO-UNDO.
      lhMNPOperation = BUFFER MNPOperation:HANDLE.
      RUN StarEventInitialize(lhMNPOperation).
      RUN StarEventSetOldBuffer(lhMNPOperation).
   END.

   ASSIGN MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_YES}.

   IF llDoEvent THEN DO:
      RUN StarEventMakeModifyEvent(lhMNPOperation).
      fCleanEventObjects().
   END.
END.

CREATE Memo.
ASSIGN
    Memo.CreStamp  = {&nowTS}
    Memo.Brand     = gcBrand
    Memo.HostTable = "MNPProcess"
    Memo.KeyValue  = STRING(MNPOperation.MNPSeq)
    Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
    Memo.CreUser   = pcUsername
    Memo.MemoTitle = pcMemoTitle
    Memo.MemoText  = pcMemoContent.

RELEASE MNPOperation.
add_struct(response_toplevel_id, "").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
