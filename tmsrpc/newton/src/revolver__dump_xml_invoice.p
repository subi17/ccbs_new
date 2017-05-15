/**
 * Request immediate XML-invoice creation
 *
 * @input   invnum;string;mandatory;invoice number
            dirname;string;mandatory;target directory (on shared NFS)
 * @output  success;boolean
 *
 * @changes FabianKreutz;11/2009;created
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1} 

{Syst/commpaa.i}
{Syst/tmsconst.i}
gcBrand = "1".

/* Input parameters */
DEF VAR piInvnum  AS INT  NO-UNDO.
DEF VAR pcDirname AS CHAR NO-UNDO.

DEF VAR lcOutDir  AS CHAR NO-UNDO.
DEF VAR liCount   AS INTE NO-UNDO.

IF validate_request(param_toplevel_id, "int,string") EQ ? THEN RETURN.

piInvnum   = get_pos_int(param_toplevel_id, "0").
pcDirname = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO Common Invoice InvNum piInvnum}

IF vcTenant = {&TENANT_MASMOVIL} THEN 
	ASSIGN lcOutDir = "/mnt/masmovil/xmlstore/".
ELSE 
	ASSIGN lcOutDir = "/mnt/xmlstore/".	

RUN Inv/invoice_xml_printone.p(piInvnum, (lcOutDir + pcDirname), "").

IF RETURN-VALUE BEGINS "ERROR" THEN 
	RETURN appl_err(RETURN-VALUE).

CHECK_FILE_AVAIL:
REPEAT:
	PAUSE 1 NO-MESSAGE.

	FILE-INFO:FILE-NAME = (lcOutDir + pcDirname + (IF SUBSTRING(pcDirname,LENGTH(pcDirname),1) <> "/" THEN "/" ELSE "") + STRING(piInvnum) + ".xml").
	IF (FILE-INFO:FULL-PATHNAME > "" AND FILE-INFO:FILE-SIZE > 0) OR liCount >= 5 THEN 
		LEAVE CHECK_FILE_AVAIL.

	ASSIGN liCount = liCount + 1.	
END.

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
