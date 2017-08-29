/* ----------------------------------------------------------------------
  Module .......: tmsrpc/newton/src/newton__add_pro_migration_request.p
  Task .........: Subscription Pro Migration Request
  Application ..: RPCMETHOD
  Author .......: kaaikas
  Created ......: 22.08.17
  Version ......: Yoigo

  Input ........:  1) migrationdata;struct;mandatory;contains input data
-                    a) Int;mandatory;msseq
-                    c) String;mandatory;salesman 
  Output .......:   success;boolean
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
{Func/fpromigrationreq.i}

DEFINE VARIABLE piMsseq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE pcSalesman        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ldActStamp        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE liMsreq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcResult          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcStruct          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcMigrStruct      AS CHARACTER   NO-UNDO.

DEFINE BUFFER lbMobSub            FOR MobSub.
DEFINE BUFFER bTermMobSub         FOR TermMobSub.

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcMigrStruct = get_struct(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcMigrStruct,"salesman!,msseq!").
IF lcStruct EQ ? THEN RETURN.

/* Required Params */
piMsSeq  = get_pos_int(pcMigrStruct, "msseq").
katun    = "VISTA_" + get_string(pcMigrStruct, "salesman").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Mobsub WHERE 
           Mobsub.msseq EQ piMsseq NO-LOCK NO-ERROR.
IF NOT AVAIL Mobsub THEN
   RETURN appl_err("Mobsub not found").

FIND FIRST Customer WHERE
           Customer.custnum EQ Mobsub.custnum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not found").

/* Create Reactivation Request */
liMsReq = fProMigrationRequest(INPUT Customer.custnum,
                               INPUT katun,
                               INPUT {&REQUEST_SOURCE_NEWTON},
                               OUTPUT lcResult).

IF liMsReq > 0 THEN
   add_boolean(response_toplevel_id, "", true).
ELSE
   RETURN appl_err(lcResult).


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.


