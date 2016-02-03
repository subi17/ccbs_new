/**
 * Request immediate XML-invoice creation
 *
 * @input   invnum;string;mandatory;invoice number
            dirname;string;mandatory;target directory (on shared NFS)
 * @output  success;boolean
 *
 * @changes FabianKreutz;11/2009;created
 */

{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1} 

{Syst/commpaa.i}
gcBrand = "1".

/* Input parameters */
DEF VAR piInvnum AS INT NO-UNDO.
DEF VAR pcDirname AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "int,string") EQ ? THEN RETURN.
pcDirname = get_string(param_toplevel_id, "1").
piInvnum   = get_pos_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

RUN invoice_xml_printone(piInvnum, "/mnt/xmlstore/" + pcDirname, "").
IF RETURN-VALUE BEGINS "ERROR" THEN RETURN appl_err(RETURN-VALUE).
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
