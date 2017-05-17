/**
 * Get service packages ids.
 *
 * @input conditions;struct;mandatory; begins'
 * @output struct;array of BillItem ids
*/

FUNCTION fCheckServPac RETURN LOGICAL 
         ( INPUT pcServPac AS CHAR ):

/* check that this servpac should contain
     the same status (defvalue) and same param definition
     in all CLIType where it has been created  */
  DEFINE VARIABLE liDefValue AS INTEGER NO-UNDO. 
  DEFINE VARIABLE lcDefParam AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE liCount AS INTEGER NO-UNDO INITIAL 0. 
  DEFINE VARIABLE llAllowed AS LOGICAL NO-UNDO INITIAL TRUE. 
  DEFINE VARIABLE lcBrand AS CHARACTER NO-UNDO INITIAL "1".

  FOR EACH CTServPac NO-LOCK WHERE
           CTServPac.Brand     = lcBrand   AND
           CTServPac.ServPac   = pcServPac     AND
           CTServPac.FromDate <= TODAY   AND
           CTServPac.ToDate   >= TODAY :

          FOR EACH CTServEl NO-LOCK WHERE
                   CTServEl.Brand     = lcBrand   AND
                   CTServEl.ServPac   = CTServPac.ServPac  AND
                   CTServEl.FromDate >= CTServPac.FromDate AND
                   CTServEl.FromDate <= CTServPac.ToDate   AND
                   CTServEl.FromDate <= TODAY :

                  IF liCount NE 0 AND 
                      ( CTServEl.DefValue NE liDefValue OR
                      CTServEl.DefParam NE lcDefParam ) THEN DO:
                      llAllowed = FALSE.
                      LEAVE. 
                  END.

                  ASSIGN 
                    liCount = liCount + 1
                    liDefValue = CTServEl.DefValue
                    lcDefParam = CTServEl.DefParam.

          END.
  END.
  IF liCount EQ 0 THEN llAllowed = FALSE.

  RETURN llAllowed .
END.


{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO. 
IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").

lcResultStruct = add_array(response_toplevel_id, "").

lcStruct = validate_struct(pcStruct, "id_begins").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FOR EACH ServPac NO-LOCK WHERE
         ServPac.Brand = "1" :

    IF LOOKUP("id_begins",lcStruct) > 0 THEN 
       IF NOT ServPac.ServPac BEGINS get_string(pcStruct,"id_begins") THEN NEXT. 

    IF fCheckServPac(ServPac.ServPac) THEN  
        add_string(lcResultStruct, "",ServPac.ServPac).
  
END.


