/**
 * Get system configuration.
 *
 * @input names;array of string;mandatory;array of setting names (=provisioning)
 * @output statuses;array of int;current status of setting (0=off, 1=on)
 */

{header_get.i}
{tmsparam4.i}

DEF VAR liMaintB AS INTEGER NO-UNDO.

DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
   
   pcID = get_string(pcIDArray, STRING(liCounter)).

   IF LOOKUP(pcId,"provisioning") = 0 THEN 
      RETURN appl_err(SUBST("Unknown service &1", pcId)). 

   liMaintB = INT(fCParamC4("1","ServiceBreak","Activation")).
   IF liMaintB = ? THEN RETURN appl_err("Missing system parameter"). 

   add_int(resp_array, "", (IF liMaintB = 0 THEN 1 ELSE 0)).
END.
