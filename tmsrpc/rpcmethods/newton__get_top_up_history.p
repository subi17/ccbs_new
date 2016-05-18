/**
 * newton__get_top_up_history.p
 *
 * @input       msseq;int;mandatory;subscription sequence number
                count;int;mandatory;count of topups returned
 * @output      top_up_history;Contains last subscriptions (amount of count)
 * @top_up_history  top_up_rowarray;array of topup data rows
 * @top_up_rowarray array;array of strings 
 
  1 st element: MSISDN or CLI for topup (string)
  2 nd element: Request for topup (string)
  3 rd element: Source for topup (string)
  4 th element: Request timestamp for topup (string) in format "dd-mm-yyyy HH:MM:ss"
  5 th element: "OK" if RespCode = 0, "NOK" otherwise (string)
  6 th element: Topup amount (string)
  7 th element: VAT amount (string)
  8 th element: Operation number (string)
  9 th element: Entity name (string)
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/tsformat.i}
/* Input parameters */
DEFINE VARIABLE piMsSeq     AS INTEGER   NO-UNDO.
DEFINE VARIABLE piCount     AS INTEGER   NO-UNDO.

/* Output parameters */
DEFINE VARIABLE resp_array AS CHARACTER NO-UNDO.
DEFINE VARIABLE row_array  AS CHARACTER NO-UNDO.

/* Local variables */
DEFINE VARIABLE lcBrand       AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE i             AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "int,int") EQ ? THEN RETURN.
piMsSeq = get_int(param_toplevel_id, "0").
piCount = get_int(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "top_up_history").

FOR EACH PrepaidRequest NO-LOCK WHERE
   PrepaidRequest.Brand = lcBrand AND   
   PrepaidRequest.MsSeq = piMsSeq USE-INDEX MsSeq:
   
   IF PrepaidRequest.Response = "First4B" THEN NEXT.
  
   FIND TMSCodes WHERE
      TMSCodes.TableName = "PrepaidRequest" AND
      TMSCodes.FieldName = "Entidad" AND
      TMSCodes.CodeValue = PrepaidRequest.Entidad NO-LOCK NO-ERROR.

   row_array = add_array(resp_array, "").
   add_string(row_array, "", PrepaidRequest.CLI).
   add_string(row_array, "", PrepaidRequest.Request).
   add_string(row_array, "", PrepaidRequest.Source).
   add_string(row_array, "", fTSFormat("dd-mm-yyyy HH:MM:ss",PrepaidRequest.TSRequest)).
   add_string(row_array, "", 
      (IF PrepaidRequest.RespCode = 0 THEN "OK" ELSE "NOK")).
   add_string(row_array, "",
      TRIM(REPLACE(STRING(TRUNC(PrepaidRequest.TopUpAmt / 100, 2),"->>9.99"), ",", "."))).
   add_string(row_array, "",
      TRIM(REPLACE(STRING(TRUNC(PrepaidRequest.VatAmt / 100, 2),"->>9.99"), ",", "."))).
   add_string(row_array, "", PrepaidRequest.Reference).
   add_string(row_array, "", (IF AVAIL TMSCodes THEN TMSCodes.CodeName ELSE PrepaidRequest.Entidad)).
   i = i + 1.
   IF i EQ piCount THEN LEAVE.
END.

/* EOF newton__get_top_up_history.p */
