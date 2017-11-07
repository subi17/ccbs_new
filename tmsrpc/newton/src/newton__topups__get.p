/**
 * Get topups 
 *
 * @input  ids;array of string;mandatory;topups ids
 * @output topup;array of struct;topups data
 * @topup  msisdn;string;MSISDN or CLI for topup
           request;string;Request for topup (RCG,ANT .. )
           source;string; source
           datetime;string;timestamp for topup (string) in format "dd-mm-yyyy HH:MM:ss"
           response_code;string;"OK" if RespCode = 0, "NOK" otherwise
           topup_amount;string;Topup amount after vat reduction
           vat_amount;string; VAT amount
           reference;string; Fenix reference number
           entity;string;id of entity that created topup
           entity_name;string; entity name
           origin_entity;string; salesman id
           operation_number;string; salesman request id  (string)
           vat_percent;double;topup vat percent
  
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/tsformat.i}
{Func/xmlfunction.i}

DEF VAR lcResultStruct AS CHAR NO-UNDO. 
DEF VAR pcId AS CHAR NO-UNDO. 
DEF VAR pcIdArray AS CHAR NO-UNDO. 
DEF VAR liCounter AS INTEGER NO-UNDO. 

DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcTenant   AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "string,array") = ? THEN RETURN.

pcTenant  = get_string(param_toplevel_id, "0").
pcIDArray = get_array (param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

resp_array = add_array(response_toplevel_id, "").

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   FIND PrepaidRequest NO-LOCK WHERE
        PrepaidRequest.Brand = Syst.Var:gcBrand AND
        PrePaidRequest.PPRequest = INT(pcID) NO-ERROR. 

   IF NOT AVAIL PrepaidRequest THEN RETURN appl_err("Topup not found: "+ pcId).
      
   lcResultStruct = add_struct(resp_array, "").

   FIND TMSCodes WHERE
      TMSCodes.TableName = "PrepaidRequest" AND
      TMSCodes.FieldName = "Entidad" AND
      TMSCodes.CodeValue = PrepaidRequest.Entidad NO-LOCK NO-ERROR.

   add_string(lcResultStruct, "msisdn", PrepaidRequest.CLI).
   add_string(lcResultStruct, "request", PrepaidRequest.Request).
   add_string(lcResultStruct, "source", PrepaidRequest.Source).
   add_string(lcResultStruct, "datetime", fTSFormat("dd-mm-yyyy HH:MM:ss",PrepaidRequest.TSRequest)).
   add_string(lcResultStruct, "response_code", 
      (IF PrepaidRequest.RespCode = 0 THEN "OK" ELSE "NOK")).
   add_string(lcResultStruct, "topup_amount",
      TRIM(REPLACE(STRING(TRUNC(PrepaidRequest.TopUpAmt / 100, 2),"->9.99"), ",", "."))).
   add_string(lcResultStruct, "vat_amount",
      TRIM(REPLACE(STRING(TRUNC(PrepaidRequest.VatAmt / 100, 2),"->9.99"), ",", "."))).
   add_string(lcResultStruct, "reference", PrepaidRequest.Reference).

   /* reseller */
   add_string(lcResultStruct, "entity", PrepaidRequest.Entidad). 
   IF AVAIL TMSCodes THEN add_string(lcResultStruct,"entity_name",TMSCodes.CodeName).
   /* salesman */ 
   add_string(lcResultStruct,"origin_entity", fGetNodeValue(PrepaidRequest.CommLine,"entidadOrigen")). 
   /* creator reference number */
   add_string(lcResultStruct,"operation_number", fGetNodeValue(PrepaidRequest.CommLine,"numOper")).
  
   IF PrepaidRequest.TopUpAmt NE 0 THEN
      add_double(lcResultStruct, "vat_percent", INTEGER(100 * PrepaidRequest.VatAmt / PrepaidRequest.TopUpAmt)).

   add_string(lcResultStruct,"id",pcId).

END.
