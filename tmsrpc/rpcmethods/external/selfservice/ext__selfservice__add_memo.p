/**
 * Create memo for (a Q25 operation).
 *
 * @input       transaction_id;string;mandatory;transaction id
                msisdn;string;mandatory;subscription msisdn number
                memo_title;string;mandatory;memo title
                memo_content;string;mandatory;memo content
                memo_type;string;mandatory;memo type
 * @output      struct;mandatory;response struct
 * @response    transaction_id;string;transaction id
                result;boolean;True
 */
{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{commpaa.i}
gcBrand = "1".
{tmsconst.i}
{fexternalapi.i}

DEF VAR pcTransId       AS CHAR NO-UNDO.
DEF VAR pcMSISDN       AS CHAR NO-UNDO.
DEF VAR pcMemoTitle     AS CHAR NO-UNDO.
DEF VAR pcMemoContent   AS CHAR NO-UNDO.
DEF VAR pcMemoType     AS CHAR NO-UNDO.

DEF VAR lcApplicationId     AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId      AS CHAR NO-UNDO.

DEF VAR top_struct      AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string!,string!,string!,string!,string!") EQ ? THEN RETURN.

ASSIGN pcTransId     = get_string(param_toplevel_id, "0")
       pcMSISDN      = get_string(param_toplevel_id,"1")
       pcMemoTitle   = get_string(param_toplevel_id,"2")
       pcMemoContent = get_string(param_toplevel_id,"3")
       pcMemoType    = get_string(param_toplevel_id,"4").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = gbAuthLog.EndUserId.

IF NOT fchkTMSCodeValues(gbAuthLog.UserName,lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").


katun = fgetAppUserId(INPUT lcApplicationId,
                      INPUT lcAppEndUserId).

FIND FIRST mobsub NO-LOCK WHERE
           mobsub.cli EQ pcMSISDN NO-ERROR.
IF NOT AVAIL mobsub THEN RETURN appl_err("Subscription not found").           
DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                 "MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 pcMemoTitle,                          /* MemoTitle */
                 pcMemoContent,                        /* MemoText */
                 pcMemoType,                           /* MemoType */
                 fgetAppDetailedUserId(INPUT lcApplicationId,
                                      INPUT Mobsub.CLI)).

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).



FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.


