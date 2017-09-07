/**
 * Create memo for (a Q25 operation).
 *
 * @input       transaction_id;string;mandatory;transaction id
                salesman_id;mandatory;salesman (can be empty)
                msisdn;string;mandatory;subscription msisdn number
                memo_title;string;mandatory;memo title
                memo_content;string;mandatory;memo content
                memo_type;string;mandatory;memo type
 * @output      struct;mandatory;response struct
 * @response    transaction_id;string;transaction id
                result;boolean;True
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/fexternalapi.i}

DEF VAR pcTransId       AS CHAR NO-UNDO.
DEF VAR pcSalesman      AS CHAR NO-UNDO.
DEF VAR pcMSISDN        AS CHAR NO-UNDO.
DEF VAR pcMemoTitle     AS CHAR NO-UNDO.
DEF VAR pcMemoContent   AS CHAR NO-UNDO.
DEF VAR pcMemoType      AS CHAR NO-UNDO.

DEF VAR lcApplicationId AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId  AS CHAR NO-UNDO.
DEF VAR lcUser          AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string,string,string,string") EQ ? THEN RETURN.

ASSIGN pcTransId     = get_string(param_toplevel_id,"0")
       pcSalesman    = get_string(param_toplevel_id,"1")
       pcMSISDN      = get_string(param_toplevel_id,"2")
       pcMemoTitle   = get_string(param_toplevel_id,"3")
       pcMemoContent = get_string(param_toplevel_id,"4")
       pcMemoType    = get_string(param_toplevel_id,"5").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{selfservice/src/findtenant.i NO ordercanal MobSub Cli pcMSISDN}

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = ghAuthLog::EndUserId.

IF NOT fchkTMSCodeValues(ghAuthLog::UserName,lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = fgetAppUserId(INPUT lcApplicationId,
                      INPUT lcAppEndUserId).

IF pcSalesman EQ "" THEN 
   lcUser = fgetAppDetailedUserId(INPUT lcApplicationId,INPUT Mobsub.CLI).
ELSE 
   lcUser = pcSalesman.

DYNAMIC-FUNCTION("fWriteMemoWithType" IN ghFunc1,
                 "MobSub",                             /* HostTable */
                 STRING(Mobsub.MsSeq),                 /* KeyValue  */
                 MobSub.CustNum,                       /* CustNum */
                 pcMemoTitle,                          /* MemoTitle */
                 pcMemoContent,                        /* MemoText */
                 pcMemoType,                           /* MemoType */
                 lcUser).

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN 
      DELETE OBJECT ghFunc1 NO-ERROR.
END.


