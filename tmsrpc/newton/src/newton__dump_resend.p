/**
 * Request dump file to be resent
 *
 * @input dumpid;int;mandatory; dump id
          dumpfile;string;mandatory; dump file to be resent

 * @output success;boolean

*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
katun = "Newton".
gcBrand = "1".


/* Input parameters */
DEF VAR pcFileName AS CHAR NO-UNDO.
DEF VAR piID AS INT NO-UNDO.
/* Local parameters */
DEF VAR llResult AS LOG NO-UNDO.
DEF VAR lcline AS CHAR NO-UNDO.
DEF VAR lcfld AS CHAR NO-UNDO.
DEF VAR litem AS INT NO-UNDO.
DEF VAR liEntries AS INT NO-UNDO.
DEF VAR lcOutFolder AS CHAR NO-UNDO.
DEF VAR lcProFolder AS CHAR NO-UNDO.
DEF VAR lcSaveFolder AS CHAR NO-UNDO.
DEF VAR llFile AS LOG NO-UNDO.
DEF VAR pcTenant AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,int,string") = ? THEN RETURN.

pcTenant   = get_string(param_toplevel_id, "0").
piID       = get_int(param_toplevel_id, "1").
pcFileName = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

DEF STREAM sFile.

FIND FIRST DumpFile NO-LOCK WHERE DumpFile.DumpID = piID NO-ERROR.
IF NOT AVAIL DumpFile THEN 
   RETURN appl_err("DumpFile ID " + STRING(piID) + " cannot be found").

ASSIGN
   lcProFolder = ""
   lcfld = ""
   litem = 0
   lcOutFolder = DumpFile.Transdir
   liEntries = NUM-ENTRIES(lcOutFolder,"/") - 1
   lcSaveFolder = "/processed".

/* Get processed folder */
DO litem = 1 TO liEntries:
   lcfld = lcfld + "/" + ENTRY(litem,lcOutFolder,"/").
   lcProFolder = SUBSTRING(lcfld,2) + lcSaveFolder.
END.

/* Search file to be resend and resend if found */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcProFolder).
REPEAT:
   IMPORT STREAM sFile UNFORMATTED lcline.
   IF lcline BEGINS pcFilename THEN DO:
      llFile = True.
      LEAVE.
   END.
END.
INPUT STREAM sFile CLOSE. 

IF llFile = True THEN DO:
   UNIX SILENT VALUE("mv " + lcProFolder + "/" + lcline + 
   " " + lcOutFolder + "/" + pcFilename).
   llResult = True.
END.
ELSE
   RETURN appl_err("Dumpfile " + pcFilename + " was not found.").

add_boolean(response_toplevel_id, "", llResult).

FINALLY:
  IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

