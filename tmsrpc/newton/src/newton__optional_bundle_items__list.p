/**
 * Get optional bundle items ids.
 *
 * @input conditions;struct;mandatory;subscription_type_id
 * @output struct;array of bundle items ids
*/

{newton/src/flistrpc.i}

DEF VAR lcCLIType    AS CHAR NO-UNDO.

lcStruct = validate_request(pcStruct, "subscription_type_id!").
IF lcStruct EQ ? THEN RETURN.

lcCLIType = get_string(pcStruct,"subscription_type_id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NUM-ENTRIES(pcId,"|") > 1 THEN
  ASSIGN
      pcTenant  = ENTRY(2,lcCLIType,"|")
      lcCLIType = ENTRY(1,lcCLIType,"|").
ELSE
  RETURN appl_err("Invalid tenant information").

{newton/src/settenant.i pcTenant}

FIND FIRST CLIType WHERE
           CLIType.Brand   = gcBrand AND
           CLIType.CLIType = lcCLIType NO-LOCK NO-ERROR.
IF NOT AVAILABLE CLIType AND lcCLIType NE "ALL_VOICE" THEN
   RETURN appl_err("CLIType not found").

fListBundleQuery(INPUT gcBrand,
                 INPUT "Bundle",
                 INPUT "SubsTypeFrom;PerContract",
                 INPUT lcCLIType).
