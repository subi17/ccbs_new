/**
 * Set system configuration.
 *
 * @input  name;string;mandatory;setting name (provisioning)
           parameters;struct;mandatory;setting parameters
 * @parameters value;int;mandatory;new setting status (0=off, 1=on)
            user;struct;mandatory;user data who made change
 * @user username;int;mandatory;newton username
         name;string;mandatory;full name
         email;string;optional (if phone_number is given);contact email
         phone_number;string;optional (if email is given);contact phone
 * @output status;int;current setting status (0=off, 1=on)
 */

{xmlrpc/xmlrpc_access.i}
{tmsparam4.i}
{tmsconst.i}

DEF VAR custcat_struct AS CHAR NO-UNDO. 
DEF VAR top_array AS CHARACTER NO-UNDO. 
DEF VAR pcSetting AS CHAR NO-UNDO. 

DEF VAR pcParamStruct AS CHAR NO-UNDO. 
DEF VAR piValue AS INTEGER NO-UNDO. 
DEF VAR pcUserStruct AS CHAR NO-UNDO. 
DEF VAR pcUsername AS CHARACTER NO-UNDO. 
DEF VAR pcUser AS CHARACTER NO-UNDO. 
DEF VAR pcEmail AS CHARACTER NO-UNDO. 
DEF VAR pcPhone AS CHARACTER NO-UNDO. 
DEF VAR pcFlag AS CHARACTER NO-UNDO.

DEF VAR lcUserStruct AS CHAR NO-UNDO. 

DEF VAR lcEmail AS CHARACTER NO-UNDO. 
DEF VAR liMaintB AS INTEGER NO-UNDO. 
DEF VAR liValue  AS INTEGER NO-UNDO. 

/* validate 1st and 2nd parameter */
IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.
pcSetting = get_string(param_toplevel_id, "0").
pcParamStruct = get_struct(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(pcSetting,"setFlag") > 0 THEN DO:
   IF validate_struct(pcParamStruct,"flag!,value!") = ? THEN RETURN.
   pcFlag = get_string(pcParamStruct, "flag").
   piValue = get_int(pcParamStruct, "value").
   
   IF NOT(fCParam4SetI("1","PREPAIDMC",pcFlag, piValue)) THEN 
      RETURN appl_err(SUBST("Param not found &1", pcFlag)).

END.
ELSE IF LOOKUP(pcSetting,"DMS") > 0 THEN DO:
   IF validate_struct(pcParamStruct,"value!") = ? THEN RETURN.
   pcFlag = {&DMS_ON_OFF}.
   piValue = get_int(pcParamStruct, "value").
   
   IF NOT(fCParam4SetI("1","DMS",pcFlag, piValue)) THEN 
      RETURN appl_err(SUBST("Param not found &1", pcFlag)).

END.

ELSE DO:
   /* validate 2nd parameter struct */
   IF validate_struct(pcParamStruct,"value!,user!") = ? THEN RETURN.
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   piValue = get_int(pcParamStruct, "value").
   pcUserStruct = get_struct(pcParamStruct, "user").

   /* validate and handle user struct */
   lcUserStruct = validate_struct(pcUserStruct,
                  "username!,name!,email,phone_number").
   IF gi_xmlrpc_error NE 0 THEN RETURN.
   pcUsername = get_string(pcUserStruct, "username").
   pcUser = get_string(pcUserStruct, "name").
   IF LOOKUP("email", lcUserStruct) > 0 THEN
      pcEmail = get_string(pcUserStruct, "email").
   IF LOOKUP("phone_number", lcUserStruct) > 0 THEN
      pcPhone = get_string(pcUserStruct, "phone_number").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   IF pcEmail = "" AND pcPhone = "" THEN DO:
      RETURN appl_err("Must give either email or phone number").
   END.

   IF LOOKUP(pcSetting,"provisioning") = 0 THEN 
      RETURN appl_err(SUBST("Unknown service &1", pcSetting)). 

   IF piValue NE 0 AND piValue NE 1 THEN
      RETURN appl_err(SUBST("Unknown setting value &1", piValue)). 

   IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

   liValue = (IF piValue = 0 THEN 1 ELSE 0).

   liMaintB = INT(fCParamC4("1","ServiceBreak","Activation")).
   IF liMaintB = ? THEN RETURN appl_err("Missing system parameter"). 

   /* parameter validation ends*/
   {commpaa.i}
   katun = "Newton".
   gcBrand = "1".
   {provmaint.i}
   {timestamp.i}

   IF liValue NE liMaintB THEN DO:
     
      lcEmail  = "Notification of changed system setting:  " +
                  CHR(10)                                + 
                  "Provisioning handling has been " +
                  (IF piValue = 1 THEN "STARTED" ELSE "STOPPED") + 
                  " at " + STRING(TIME,"HH:MM:SS") +
                  CHR(10) + CHR(10)                             +
                  "User responsible of this action: " + pcUserName + 
                   CHR(10) + CHR(10) +
                   " Name: " + pcUser + CHR(10) +
                   "Phone: " + pcPhone + CHR(10) +
                   "Email: " +  pcEmail.

      fUpdateMaintBreak(fMakeTs(),fMakeTs(),liValue).
      RUN pMailMaintBreak(lcEmail).
   END.
END.

add_int(response_toplevel_id, "",  piValue).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
