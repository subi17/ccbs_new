/* ----------------------------------------------------------------------
  MODULE .......: settenant.i 
  TASK .........: Utility functions for setting access to specific tenant
  APPLICATION ..: TMS
  AUTHOR .......: ushak  
  CREATED ......: 19.01.17
  Version ......: xfera  
----------------------------------------------------------------------- */
DEFINE VARIABLE liDBCount AS INTEGER NO-UNDO.

{Func/multitenantfunc.i}

pcTenant = fConvertBrandToTenant(pcTenant).

IF pcTenant = "" THEN 
    RETURN appl_err("Invalid tenant information").

DO liDBCount = 1 TO NUM-DBS
    ON ERROR UNDO, THROW:
    
    SET-EFFECTIVE-TENANT(pcTenant, LDBNAME(liDBCount)).
    
    CATCH e AS Progress.Lang.Error:
		    RETURN appl_err("Unable to set access to specific tenant.Abort!").    	
    END CATCH.
END.
