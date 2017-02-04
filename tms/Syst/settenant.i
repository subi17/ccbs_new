/* ----------------------------------------------------------------------
  MODULE .......: settenant.i 
  TASK .........: Utility functions for setting access to specific tenant
  APPLICATION ..: TMS
  AUTHOR .......: ushak  
  CREATED ......: 19.01.17
  Version ......: xfera  
----------------------------------------------------------------------- */
DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

IF pcTenant = "" THEN 
    RETURN appl_err("Invalid tenant information").

DO liCount = 1 TO NUM-DBS
	ON ERROR UNDO, THROW:
    
    SET-EFFECTIVE-TENANT(pcTenant, LDBNAME(liCount)).
    
    CATCH e AS Progress.Lang.Error:
		RETURN appl_err("Unable to set access to specific tenant.Abort!").    	
    END CATCH.
END.