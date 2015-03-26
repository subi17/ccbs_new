/**
 * Set mnp retention dispatch rules
 *
 * @input  string;mandatory;username
           array of struct;mandatory;mnp retention dispatch rules
 * @struct id;string;mandatory;retention platform id
           percentage;decimal;mandatory;Total percentage field sum must be 100
 * @output boolean;True
 */

{xmlrpc/xmlrpc_access.i}

DEF VAR resp_array AS CHARACTER NO-UNDO.
DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcArray AS CHAR NO-UNDO. 
DEF VAR liLoop AS INT NO-UNDO. 
DEF VAR pcID AS CHAR NO-UNDO. 
DEF VAR pdePercentage AS DEC NO-UNDO. 
DEF VAR ldePercentageTotal AS DEC NO-UNDO. 
DEF VAR pcUsername AS CHAR NO-UNDO. 
DEF VAR llEqual AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttMNPRetPlatform NO-UNDO LIKE mnpretplatform.

IF validate_request(param_toplevel_id, "string,array") = ? THEN RETURN.
pcUsername = "VISTA_" + get_string(param_toplevel_id, "0").
pcArray = get_array(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

{commpaa.i}
{eventval.i}
ASSIGN
   katun = pcUsername
   gcBrand = "1".

FOR EACH mnpretplatform NO-LOCK WHERE
         mnpretplatform.brand = gcBrand AND
         mnpretplatform.Todate >= TODAY AND
         mnpretplatform.FromDate <= TODAY:
   CREATE ttMNPRetPlatform.
   BUFFER-COPY mnpretplatform TO ttMNPRetPlatform.
END.

DO liLoop = 0 TO get_paramcount(pcArray) - 1:
   
   pcStruct = get_struct(pcArray, STRING(liLoop)).
   validate_struct(pcStruct,"id,percentage").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   pcID = get_string(pcStruct, "id").
   pdePercentage = get_double(pcStruct, "percentage").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   
   FIND FIRST ttMNPRetPlatform WHERE
              ttMNPRetPlatform.RetentionPlatform = pcID EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL ttMNPRetPlatform THEN RETURN 
      appl_err(SUBST("Unknown MNP retention platform: &1", pcID)).
   ASSIGN
     ttMNPRetPlatform.percentage = pdePercentage.
END.

FOR EACH ttMNPRetPlatform NO-LOCK:
   ldePercentageTotal = ldePercentageTotal + ttMNPRetPlatform.Percentage.
END.

IF ABS(ldePercentageTotal - 100) > 0.001 THEN RETURN 
   appl_err("percentage_error").
         
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {lib/eventlog.i}
   DEF VAR lhMNPRetPlatform AS HANDLE NO-UNDO.
   lhMNPRetPlatform = BUFFER MNPRetPlatform:HANDLE.
END.

DO TRANSACTION:
   
   FOR EACH ttMNPRetPlatform NO-LOCK:

      FIND MNPRetPlatform NO-LOCK WHERE
           MNPRetPlatform.RetentionPlatform = 
           ttMNPRetPlatform.RetentionPlatform NO-ERROR.
      IF NOT AVAIL MNPRetPlatform THEN UNDO, RETURN
         appl_err("Record not found").
      
      BUFFER-COMPARE ttMNPRetPlatform TO MNPRetPlatform SAVE llEqual.

      IF NOT llEqual THEN DO:

         FIND CURRENT MNPRetPlatform EXCLUSIVE-LOCK.

         IF llDoEvent THEN DO:
            RUN StarEventInitialize(lhMNPRetPlatform).
            RUN StarEventSetOldBuffer(lhMNPRetPlatform).
         END.
         
         ASSIGN
            MNPRetPlatform.Percentage = ttMNPRetPlatform.Percentage.

         IF llDoEvent THEN DO:
            RUN StarEventMakeModifyEvent(lhMNPRetPlatform).
         END.
      END.
      
      RELEASE MNPRetPlatform.
   END.
END.

add_boolean(response_toplevel_id, ?, TRUE).
 
FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
   IF llDoEvent THEN fCleanEventObjects().
   EMPTY TEMP-TABLE ttMNPRetPlatform.
END.
