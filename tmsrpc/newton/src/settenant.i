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

{Func/multitenantfunc.i}

pcTenant = fConvertBrandToTenant({1}).

IF pcTenant = "" THEN 
    RETURN appl_err("Invalid tenant information").

DO liDBCount = 1 TO NUM-DBS
    ON ERROR UNDO, THROW:
    
    SET-EFFECTIVE-TENANT(pcTenant, LDBNAME(liDBCount)).
    
    CATCH e AS Progress.Lang.Error:
		    RETURN appl_err("Unable to set access to specific tenant.Abort!").    	
    END CATCH.
END.
