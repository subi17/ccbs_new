/* ----------------------------------------------------------------------
  MODULE .......: tenant.i 
  TASK .........: Utility functions for identified tenant
  APPLICATION ..: TMS
  AUTHOR .......: ushak  
  CREATED ......: 19.01.17
  Version ......: xfera  
----------------------------------------------------------------------- */
DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

IF pcTenant = "" THEN 
    RETURN appl_err("Invalid tenant information").

DO liCount = 1 TO NUM-DBS:
    SET-EFFECTIVE-TENANT(pcTenant, LDBNAME(liCount)).
END.