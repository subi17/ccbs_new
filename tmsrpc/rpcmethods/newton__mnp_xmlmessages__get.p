/**
 * Get mnp xml messages.
 *
 * @input ids;array of string;mandatory;mnp message ids
 * @output mnpxml;array of struct;mnp xml messages
 * @mnpxml xml_request;array of string;xml is slitted if size > 32000 chars
          xml_response;array of string;xml is slitted if size > 32000 chars
 */

{header_get.i}
{log.i}
      
DEFINE VARIABLE gcStructMnpMessage AS CHARACTER NO-UNDO. 

DEF BUFFER MNPBuzon FOR MNPOperation.

FUNCTION fFormatXML RETURNS LOGICAL
   (icMessageType AS CHAR):
      
   DEFINE VARIABLE liBase64Index AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liBase64IndexEnd AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcXMLMessage AS LONGCHAR NO-UNDO. 
   DEFINE VARIABLE lcXMLArray AS CHARACTER NO-UNDO. 
   
   IF icMessageType = "xml_response" THEN DO:
      /* buzon xml */
      IF MNPOperation.XMLSeq > 0 THEN DO:
         FIND MNPBuzon WHERE
            MNPBuzon.MnpOperationID = MNPOperation.XMLSeq NO-LOCK NO-ERROR.
         IF AVAIL MNPBuzon THEN DO:
            COPY-LOB MNPBuzon.XMLRequest TO lcXMLMessage.
         END.
         ELSE DO:
            fLogError("MNPXml not found").
            lcXmlMessage = "".
         END.
      END.
      ELSE COPY-LOB MNPOperation.XMLResponse TO lcXMLMessage.
   END.
   ELSE COPY-LOB MNPOperation.XMLRequest TO lcXMLMessage.
   
   IF lcXMLMessage = ? THEN lcXMLMessage = "".
   
   lcXMLArray =  add_array(gcStructMnpMessage, icMessageType).
   DEFINE VAR i AS INTEGER NO-UNDO. 

   LOOP:
   DO WHILE TRUE:
      IF LENGTH(lcXMLMessage) > 31000 THEN DO:
         add_string(lcXMLArray, "", from_utf8(STRING(SUBSTRING(lcXMLMessage,1,31000)))).
         lcXMLMessage = SUBSTRING(lcXMLMessage,31001).
      END.
      ELSE DO:
         add_string(lcXMLArray, "", from_utf8(STRING(lcXMLMessage))).
         LEAVE LOOP.
      END.
   END.

   RETURN TRUE.

END FUNCTION. 
      
DEFINE VARIABLE liID AS INT NO-UNDO.
DEFINE VARIABLE ldeStartTS AS DEC NO-UNDO. 
DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   liID = INT(pcId) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN
      appl_err("Invalid key: " + pcID).

   FIND MNPOperation NO-LOCK WHERE
        MNPOperation.MNPOperationID = liID NO-ERROR.
   IF NOT AVAIL MNPOperation THEN RETURN
      appl_err("MNPMessage not found: " + pcID).

   gcStructMnpMessage = add_struct(resp_array,"").
   fFormatXML("xml_request").
   fFormatXML("xml_response"). 

END.
