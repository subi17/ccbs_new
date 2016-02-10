/** 
 * RPC to set SMS.
 *
 * @input       username;string;user who changed the SMS
                keyvalue;string;identification of SMS
                langitems;array;array of language item structs
 * @langitems   language;int;translation of SMS
                smstext;string;content of SMS

 * @output      success;boolean;Result status of saving SMS
*/
{xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
{Syst/tmsconst.i}
gcBrand = "1".

{Syst/eventval.i}
DEFINE VARIABLE pcReqList AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcKeyValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcLangItems AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcSmsContent AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liArrayCount AS INTEGER NO-UNDO.
DEFINE VARIABLE piLanguage AS INTEGER NO-UNDO.   
DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhInvText AS HANDLE NO-UNDO.
DEFINE VARIABLE lhRepText AS HANDLE NO-UNDO.

pcReqList = validate_request(param_toplevel_id, "string,string,array").
IF pcReqList EQ ? THEN RETURN.

pcUserName = "VISTA_" + get_string(param_toplevel_id, "0").
pcKeyValue = get_string(param_toplevel_id, "1").
pcLangItems = get_array(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUserName) EQ "VISTA_" THEN RETURN appl_err("username is empty").

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername
   {Func/lib/eventlog.i}
   lhInvText = BUFFER InvText:HANDLE.
   lhRepText = BUFFER RepText:HANDLE.
   RUN StarEventInitialize(lhInvText).
   RUN StarEventInitialize(lhRepText).
END.

SMS_UPDATE:
DO TRANSACTION:
DO liArrayCount = 0 TO get_paramcount(pcLangItems) - 1:
   /* lisaa struct ja hae avaimiilla  */
   pcStruct = get_struct(pcLangItems,STRING(liArrayCount)). 
   piLanguage = get_int(pcStruct,"language").
   pcSmsContent = get_string(pcStruct,"smstext").
   IF gi_xmlrpc_error NE 0 THEN UNDO SMS_UPDATE, RETURN.
   pcSMSContent = TRIM(pcSMSContent). 

   FIND InvText NO-LOCK WHERE
        InvText.Brand     = gcBrand AND
        InvText.Target    = "SMS" AND
        InvText.KeyValue  = pcKeyValue AND
        InvText.Language  = 1 AND
        InvText.ToDate   >= TODAY AND
        InvText.FromDate <= TODAY NO-ERROR.
   IF NOT AVAIL InvText THEN UNDO SMS_UPDATE,
         RETURN appl_err("Not found requested SMS " + pcKeyValue).

   IF piLanguage = 1 THEN DO:
      IF pcSMSContent = "" THEN RETURN appl_err("spanish text cannot be empty"). 

      FIND CURRENT InvText EXCLUSIVE-LOCK. 
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvText).
      
      ASSIGN InvText.InvText = pcSmsContent.

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvText).
      RELEASE InvText.
   END. /* IF piLanguage = 1 THEN DO: */

   IF piLanguage > 1 THEN DO:
      FIND RepText EXCLUSIVE-LOCK WHERE
           RepText.Brand     = gcBrand AND
           RepText.TextType  = {&REPTEXT_SMS} AND
           RepText.LinkCode  = STRING(InvText.ITNum) AND
           RepText.Language  = piLanguage AND
           RepText.ToDate   >= TODAY AND
           RepText.FromDate <= TODAY NO-ERROR.

      IF NOT AVAIL RepText THEN DO:
         
         CREATE RepText.
         ASSIGN
            RepText.Brand    = gcBrand
            Reptext.Language = piLanguage
            RepText.TextType = {&REPTEXT_SMS}
            RepText.LinkCode = STRING(InvText.ITNum)
            RepText.RepText  = pcSmsContent
            RepText.FromDate = TODAY
            RepText.ToDate   = 12/31/2049.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRepText).
      END. /* IF NOT AVAIL RepText THEN DO: */

      ELSE DO:
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRepText).

         ASSIGN RepText.RepText = pcSmsContent.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRepText).
         RELEASE RepText.
      END. /* ELSE block */
   END. /* IF piLanguage > 1 THEN DO: */
END.
END.


add_boolean(response_toplevel_id,?,TRUE).


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
   
   IF llDoEvent THEN
      fCleanEventObjects().
END.

