/**
 * Get system configuration.
 *
 * @input names;array of string;mandatory;array of setting names 
 * (=provisioning, minconsflag)
 * @output statuses;array of int;current status of setting (0=off, 1=on)
 */

{header_get.i}
{Func/tmsparam4.i}
{Syst/tmsconst.i}

DEF VAR liMaintB AS INTEGER NO-UNDO.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF LOOKUP(pcId,"provisioning") > 0 THEN DO:
      liMaintB = INT(fCParamC4("1","ServiceBreak","Activation")).
      IF liMaintB = ? THEN RETURN appl_err("Missing system parameter"). 
   END.
   ELSE IF LOOKUP(pcId,"minconsflag") > 0 THEN DO:
      liMaintB = INT(fCParamI4("1","PREPAIDMC","MinConsRerunAllowed")).
      IF liMaintB = ? THEN RETURN appl_err("Missing system parameter").
   END.   
   ELSE IF LOOKUP(pcId,"DMS") > 0 THEN DO:
      liMaintB = INT(fCParamI4("1","DMS",{&DMS_ON_OFF})).
      IF liMaintB = ? THEN RETURN appl_err("Missing system parameter").
   END.   
   ELSE
      RETURN appl_err(SUBST("Unknown service &1", pcId)).

   IF LOOKUP(pcId,"DMS") > 0 THEN
      add_int(resp_array, "", liMaintB).
   ELSE
      add_int(resp_array, "", (IF liMaintB = 0 THEN 1 ELSE 0)).
      
END.
