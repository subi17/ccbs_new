/**
 * Changes a request status.
 *
 * @input   msrequest;int;mandatory;msrequest id
            newstatus;int;mandatory;new status for msrequest
            usercode;string;optional;login id 
 * @output  success;boolean
 */

{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

/* Input parameters */
DEF VAR piMsRequest AS INT NO-UNDO.
DEF VAR piNewStatus AS INT NO-UNDO.
DEF VAR pcUserCode  AS CHAR NO-UNDO.
DEF VAR top_array AS CHAR NO-UNDO. 

/* Local variables */
DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.

/* Parse and validate request */
top_array = validate_request(param_toplevel_id, "int,int,[string]").
IF top_array EQ ? THEN RETURN.
piMsRequest = get_int(param_toplevel_id, "0").
piNewStatus = get_int(param_toplevel_id, "1").
IF NUM-ENTRIES(top_array) >= 3 THEN
   pcUserCode = get_string(param_toplevel_id,"2").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Msrequest NO-LOCK WHERE
   MsRequest.MsRequest = piMsRequest NO-ERROR.
IF NOT AVAIL MsRequest THEN RETURN 
   appl_err(SUBST("MsRequest &1 not found!",piMsRequest)).

{commpaa.i}
gcbrand = "1".
katun = "Newton".
{msreqfunc.i}

FIND MsReqStatFunc NO-LOCK WHERE
     MsReqStatFunc.ReqType   = MsRequest.ReqType AND
     MsReqStatFunc.ReqStatus = MsRequest.ReqStatus
NO-ERROR.
IF AVAIL MsReqStatFunc THEN DO:
   /* Get list of available functions for this type and status from FuncGroup */
   DO liLoop = 2 TO NUM-ENTRIES(MsReqStatFunc.FuncGroup,","):
      FIND MsReqFuncItem NO-LOCK WHERE
           MsReqFuncItem.ItemId = ENTRY(liLoop,MsReqStatFunc.FuncGroup,",")
      NO-ERROR.
      IF AVAIL MsReqFuncItem AND piNewStatus = MsReqFuncItem.IParam THEN DO:
         IF NOT fReqStatus(piNewStatus,"") THEN DO:
            RETURN appl_err("Error occured during update, request was locked!").
         END.   
         add_boolean(response_toplevel_id, "", TRUE).
         RETURN.
      END.
   END.
END.

RETURN appl_err(SUBST("Status change from &1 to &2 is not possible with msrequest type &3.", MsRequest.ReqStatus, piNewStatus, MsRequest.ReqType)).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
