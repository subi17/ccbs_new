/* ----------------------------------------------------------------------
  MODULE .......: findtenant.i 
  TASK .........: Utility functions for identified tenant
  APPLICATION ..: TMS
  AUTHOR .......: ushak  
  CREATED ......: 19.01.17
  Version ......: xfera 
  Parameters....: 1 = Database
                  2 = Check for Brand
                  3 = TableName
                  4 = FieldName 
                  5 = FieldValue
----------------------------------------------------------------------- */
DEFINE VARIABLE liCnt    AS INTEGER   NO-UNDO.
DEFINE VARIABLE liCount  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vcTenant AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, THROW:
    /* This is to validate availability of multiple records with unique id passed. Ideally, only 1 record is expected, since sequences are globally unique */
    FOR EACH {3} WHERE &IF {1} &THEN {3}.Brand = gcBrand AND &ENDIF {3}.{4} = {5} TENANT-WHERE TENANT-ID() > -1 NO-LOCK
        ON ERROR UNDO, THROW:

        ASSIGN liCnt = liCnt + 1.
            
        IF liCnt > 1 THEN
            UNDO, THROW NEW Progress.Lang.AppError(SUBST("Multiple {3} records identified for given &1. So, unable to set access to specific tenant!", {5}),1).                  
    END.
        
    /* This is to find the appropriate record w.r.t input parameters. */
    FOR FIRST {3} WHERE &IF {1} &THEN {3}.Brand = gcBrand AND &ENDIF {3}.{4} = {5} TENANT-WHERE TENANT-ID() > -1 NO-LOCK:
        ASSIGN vcTenant = BUFFER-TENANT-NAME({3}).                
    END.
    
    IF NOT AVAILABLE {3} THEN 
        UNDO, THROW NEW Progress.Lang.AppError(SUBST("{3} with {4} &1 doesn't exists!", {5}),1).                 
    
    IF vcTenant > "" THEN 
    DO liCount = 1 TO NUM-DBS
       ON ERROR UNDO, THROW:
        SET-EFFECTIVE-TENANT(vcTenant, LDBNAME(liCount)).
    END.

    CATCH e AS Progress.Lang.Error:
        RETURN appl_err(e:GetMessage(1)).
    END CATCH.    
END.

         

