/* ----------------------------------------------------------------------
  MODULE .......: settenant.i 
  TASK .........: Utility functions for setting access to specific tenant
  APPLICATION ..: TMS
  AUTHOR .......: ushak  
  CREATED ......: 19.01.17
  Version ......: xfera  
----------------------------------------------------------------------- */
&IF "{&settenant}" NE "YES" &THEN

&GLOBAL-DEFINE settenant YES

DEFINE VARIABLE liDBCount AS INTEGER NO-UNDO.

&ENDIF

IF {1} = "" THEN 
DO:
   lcError = "Invalid tenant, check donor/receptorcode for" + " " + ttInput.PortRequest.
   RETURN appl_err(lcError).
END.

DO liDBCount = 1 TO NUM-DBS
    ON ERROR UNDO, THROW:
    
    SET-EFFECTIVE-TENANT({1}, LDBNAME(liDBCount)).
    
    CATCH e AS Progress.Lang.Error:
        ASSIGN lcError = e:GetMessage(1).
        RETURN appl_err(lcError).
    END CATCH.
END.
