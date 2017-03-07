/**
 * A mobsub object
 *
 * @input       cli_type;string;optional;Current cli type
                bundle_id;string;optional;Current bundle id
 * @output      clitypes;array of structs;
 * @clitypes    cli_type;string;mandatory;
                tariff_bundle;string;mandatory;
                status_code;int;mandatory;(0=Inactive,1=active,2=retired)
 */
{xmlrpc/xmlrpc_access.i}

DEF VAR gcBrand    AS CHAR NO-UNDO.
DEF VAR katun      AS CHAR NO-UNDO.
DEF VAR pcCliType  AS CHAR NO-UNDO.
DEF VAR pcBundleId AS CHAR NO-UNDO.
DEF VAR pcInputStruct AS CHAR NO-UNDO.
DEF VAR lcInputFields AS CHAR NO-UNDO.

ASSIGN katun = "Newton"
       gcBrand = "1".

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fixedlinefunc.i}

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcInputStruct = get_struct(param_toplevel_id,"0").
IF gi_xmlrpc_error NE 0 THEN RETURN.
lcInputFields = validate_request(pcInputStruct,"cli_type,bundle_id").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcCliType     = get_string(pcInputStruct, "cli_type")
      WHEN LOOKUP("cli_type",lcInputFields) > 0
   pcBundleId    = get_string(pcInputStruct, "bundle_id")
      WHEN LOOKUP("bundle_id",lcInputFields) > 0.

/* Output parameters */
DEF VAR top_struct         AS CHAR NO-UNDO.
DEF VAR result_array       AS CHAR NO-UNDO.
DEF VAR sub_struct         AS CHAR NO-UNDO.
DEF VAR ldaCont15PromoEnd  AS DATE NO-UNDO. 
DEF VAR lcStatusCode       AS INT  NO-UNDO.

DEF BUFFER bCLIType        FOR CLIType.

ldaCont15PromoEnd  = fCParamDa("CONT15PromoEndDate").

FUNCTION fAddCLITypeStruct RETURNS LOGICAL (INPUT icCLIType      AS CHAR,
                                            INPUT icTariffBundle AS CHAR,
                                            INPUT iiStatusCode   AS INT):

   /* YPR-1720 */
   IF icCLIType EQ "CONT15" AND
      iiStatusCode EQ 2 AND
      ldaCont15PromoEnd NE ? AND
      TODAY <= ldaCont15PromoEnd THEN iiStatusCode = 1.

   sub_struct = add_struct(result_array,"").

   add_string(sub_struct,"cli_type",icCLIType).
   add_string(sub_struct,"tariff_bundle",icTariffBundle).
   add_int(sub_struct,"status_code",iiStatusCode).
   
END FUNCTION. 

top_struct = add_struct(response_toplevel_id, "").
result_array = add_array(top_struct, "clitypes").

FOR EACH CLIType NO-LOCK WHERE
         CLIType.Brand = gcBrand AND
         CLIType.WebStatusCode > 0:

   /* Combine bundle based clitypes with base tariff */
   IF CLIType.BundleType = TRUE THEN
      FOR EACH bCLIType WHERE
               bCLIType.Brand = gcBrand AND
               bCLIType.BillTarget EQ CLIType.BillTarget AND
               bCLIType.CLIType <> CLIType.CLIType AND
               bCLIType.BundleType = CLIType.BundleType NO-LOCK:
         lcStatusCode = bCLIType.StatusCode.
         fAddCLITypeStruct(CLIType.CLIType,bCLIType.CLIType,
                           lcStatusCode).

      END. /* FOR EACH bCLIType WHERE */
   ELSE DO:
      lcStatusCode = CLIType.StatusCode.
      /* Mobile subscrition should be allowed to do STC in convergent
         tariffs, but fixed part should remain same */
      IF fIsConvergenceTariff(pcClitype) AND
         fIsConvergenceTariff(CliType.Clitype) AND
         fCheckConvergentSTCCompability(pcClitype,Clitype.clitype) THEN
         lcStatusCode = 1.
      fAddCLITypeStruct(CLIType.CLIType,"",lcStatusCode).
   END.
END. /* FOR EACH CLIType WHERE */

