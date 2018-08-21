/* ----------------------------------------------------------------------
  Module .......: tmsrpc/newton/src/newton__add_pro_migration_request.p
  Task .........: Subscription Pro Migration Request
  Application ..: RPCMETHOD
  Author .......: kaaikas
  Created ......: 22.08.17
  Version ......: Yoigo

  Input ........:  1) migrationdata;struct;mandatory;contains input data
-                    a) Int;mandatory;msseq
-                    c) String;mandatory;salesman_id 
  Output .......:   success;boolean
---------------------------------------------------------------------- */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Func/profunc_request.i}

DEFINE VARIABLE piMsseq           AS INTEGER     NO-UNDO.
DEFINE VARIABLE liRequest         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lcResult          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcStruct          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcMigrStruct      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pcTenant          AS CHAR NO-UNDO INIT "Yoigo".

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcMigrStruct = get_struct(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcMigrStruct,"salesman_id!,msseq!").
IF lcStruct EQ ? THEN RETURN.

/* Required Params */
piMsSeq  = get_pos_int(pcMigrStruct, "msseq").
Syst.Var:katun = "VISTA_" + get_string(pcMigrStruct, "salesman_id").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

FIND FIRST Mobsub WHERE 
           Mobsub.msseq EQ piMsseq NO-LOCK NO-ERROR.
IF NOT AVAIL Mobsub THEN
   RETURN appl_err("Mobsub not found").

FIND FIRST Customer WHERE
           Customer.custnum EQ Mobsub.custnum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not found").

/* Create Reactivation Request */
liRequest = fProMigrationRequest(INPUT piMsseq,
                               INPUT Syst.Var:katun,
                               INPUT {&REQUEST_SOURCE_NEWTON},
                               0,
                               OUTPUT lcResult).
IF liRequest > 0 THEN
   lcResult = fProMigrateOtherSubs(Mobsub.AgrCust, 
                                   Mobsub.MsSeq, 
                                   liRequest, 
                                   Syst.Var:katun).

IF lcResult = "" THEN
   add_boolean(response_toplevel_id, "", true).
ELSE 
   RETURN appl_err(lcResult).  
    
FINALLY:
END.


