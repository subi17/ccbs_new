/* ----------------------------------------------------------------------
  Module .......: tmsrpc/newton/src/newton__add_pro_migration_request.p
  Task .........: Subscription Pro Migration Request
  Application ..: RPCMETHOD
  Author .......: kaaikas
  Created ......: 22.08.17
  Version ......: Yoigo

  Input ........: 1) migrationdata;struct;mandatory;contains input data
                    a) String;mandatory;custid
                    b) String;mandatory;custidtype
                    c) String;mandatory;salesman
                  2) struct;mandatory;memo
                    a) title;string;mandatory
                    b) content;string;mandatory
  Output .......: success;boolean
---------------------------------------------------------------------- */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Func/cparam2.i}
{Func/fcreatereq.i}
{Mnp/mnp.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/freacmobsub.i}
{Func/profunc.i}

DEFINE VARIABLE liMsreq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE ldActStamp        AS DECIMAL     NO-UNDO.

DEFINE VARIABLE lcCustId          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcCustIdType      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcResult          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcMigrateStruct   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcMemoStruct      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcStruct          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcMemoTitle       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcMemoContent     AS CHARACTER   NO-UNDO.

DEFINE BUFFER lbMobSub            FOR MobSub.
DEFINE BUFFER bTermMobSub         FOR TermMobSub.

IF validate_request(param_toplevel_id, "struct,struct") EQ ? THEN RETURN.

pcMigrateStruct = get_struct(param_toplevel_id, "0").
pcMemoStruct = get_struct(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcMigrateStruct,"custid!,custidType!,salesman!").
IF lcStruct EQ ? THEN RETURN.

lcStruct = validate_request(pcMemoStruct,"title!,content!").
IF lcStruct EQ ? THEN RETURN.

/* Required Params */
lcCustId  = get_string(pcMigrateStruct, "custid").
lcCustIdType = get_string(pcMigrateStruct, "custidtype").
katun    = "VISTA_" + get_string(pcMigrateStruct, "salesman").

lcMemoTitle = get_string(pcMemoStruct, "title").
lcMemoContent = get_string(pcMemoStruct, "content").

IF TRIM(katun) EQ "VISTA_" THEN
   RETURN appl_err("username is empty").
FIND FIRST Customer WHERE
           Customer.brand EQ "1" AND
           Customer.orgid EQ lccustid NO-ERROR.
/* Create Reactivation Request */
liMsReq = fMigrationRequest(INPUT lcCustID,
                            INPUT lcCustIDType,
                            INPUT katun,
                            INPUT {&REQUEST_SOURCE_NEWTON},
                            OUTPUT lcResult).

IF liMsReq > 0 THEN
   add_boolean(response_toplevel_id, "", true).
ELSE
   RETURN appl_err(lcResult).

IF lcMemoTitle > "" AND liMsReq > 0 THEN DO:
   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "MobSub"
       Memo.KeyValue  = STRING(lcCustID)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.MemoTitle = lcMemoTitle
       Memo.MemoText  = lcMemoContent
       Memo.CustNum   = (IF AVAILABLE Customer THEN Customer.custnum ELSE 0).
END. /* IF lcMemoTitle > "" THEN DO: */

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.


