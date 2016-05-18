/**
 * Get pdf messages.
 *
 * @input ids;array of string;mandatory;file names
 * @output data;array of base64;mnp pdf messages
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEF VAR lcPDFStruct AS CHARACTER NO-UNDO.
DEF VAR pcId AS CHAR NO-UNDO.
DEF VAR pcIdArray AS CHAR NO-UNDO.
DEF VAR liCounter AS INTEGER NO-UNDO.
DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAR lcTempFile AS CHARACTER NO-UNDO.
DEF VAR lcPortRequest AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "array") = ? THEN RETURN.
pcIDArray = get_array(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

resp_array = add_array(response_toplevel_id, "").

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:

   pcID = get_string(pcIDArray, STRING(liCounter)).
   lcPortRequest = entry(1,pcID,".").

   IF LENGTH(lcPortRequest) = 11 THEN DO:
      FIND FIRST mnpprocess where
                 mnpprocess.formrequest = lcPortRequest NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      lcPortRequest = entry(1,lcPortRequest,"_").

      FIND FIRST mnpprocess where
                 mnpprocess.portrequest = lcPortRequest NO-LOCK NO-ERROR.
   END.

   IF NOT AVAIL mnpprocess THEN
      RETURN appl_err("MNPProcess not found").

   FIND FIRST MNPCancelProposal WHERE
              MNPCancelProposal.MNPSeq = mnpprocess.MNPSeq AND
              MNPCancelProposal.AttachmentFile = pcID NO-LOCK NO-ERROR.

   IF NOT AVAIL MNPCancelProposal OR LENGTH(MNPCancelProposal.Pdf) = ? THEN
      RETURN appl_err("File not found:" + pcId).

   lcPDFStruct = add_struct(resp_array,"").
   lcTempFile = SESSION:TEMP-DIR + "/" + GUID(GENERATE-UUID).
   COPY-LOB FROM MNPCancelProposal.Pdf TO FILE lcTempFile.
   add_file(lcPDFStruct,"pdf", lcTempFile).

END.

