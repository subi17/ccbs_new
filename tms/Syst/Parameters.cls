ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Syst.Parameters FINAL:

   DEFINE STATIC BUFFER lbTMSParam FOR TMSParam.
   DEFINE PUBLIC STATIC VARIABLE gcBrand AS CHARACTER NO-UNDO INITIAL "1".

   DEFINE PUBLIC STATIC PROPERTY Tenant AS CHARACTER NO-UNDO
   GET ():
      
      DEFINE VARIABLE lcTenant AS CHARACTER NO-UNDO. 

      FIND FIRST lbTMSParam NO-LOCK NO-ERROR.
      
      IF NOT AVAIL lbTMSParam THEN
         RETURN ERROR NEW Progress.Lang.AppError("Cannot find TMSParam"). 

      lcTenant = BUFFER-TENANT-NAME(lbTMSParam).

      CASE lcTenant:
         WHEN "Default" THEN
            RETURN "yoigo".
         WHEN "Tmasmovil" THEN 
            RETURN "masmovil".
         OTHERWISE RETURN "?".
      END CASE.        

   END GET.

   METHOD PUBLIC STATIC CHARACTER getc
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER ):
      
      RETURN getParam(icCode, icGroup, "C").
   
   END METHOD.


   METHOD PUBLIC STATIC INTEGER geti
      ( icCode AS CHARACTER ):
      
      RETURN geti(icCode, ?).
   
   END METHOD.


   METHOD PUBLIC STATIC INTEGER geti
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER ):
      
      RETURN INTEGER(getParam(icCode, icGroup, "I")).
   
   END METHOD.


   METHOD PUBLIC STATIC DECIMAL getde
      ( icCode AS CHARACTER ):
      
      RETURN getde(icCode, ?).
   
   END METHOD.


   METHOD PUBLIC STATIC DECIMAL getde
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER ):
      
      RETURN DECIMAL(getParam(icCode, icGroup, "DE")).
   
   END METHOD.


   METHOD PUBLIC STATIC DATE getda
      ( icCode AS CHARACTER ):
      
      RETURN getda(icCode, ?).
   
   END METHOD.


   METHOD PUBLIC STATIC DATE getda
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER ):
      
      RETURN DATE(getParam(icCode, icGroup, "DA")).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID setc
      ( icCode  AS CHARACTER,
        icValue AS CHARACTER ):
      
      setc(icCode, "", icValue).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID setc
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER,
        icValue AS CHARACTER ):
      
      setParam(icCode, icGroup, "C", icValue).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID seti
      ( icCode  AS CHARACTER,
        iiValue AS INTEGER ):
      
      seti(icCode, "", iiValue).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID seti
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER,
        iiValue AS INTEGER ):
      
      setParam(icCode, icGroup, "I", STRING(iiValue)).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID setde
      ( icCode  AS CHARACTER,
        ideValue AS DECIMAL ):
      
      setde(icCode, "", ideValue).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID setde
      ( icCode   AS CHARACTER,
        icGroup  AS CHARACTER,
        ideValue AS DECIMAL):
      
      setParam(icCode, icGroup, "DE", STRING(ideValue)).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID setda
      ( icCode AS CHARACTER,
        idaDate AS DATE ):
      
      setda(icCode, "", idaDate).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID setda
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER,
        idaDate AS DATE ):
      
      setParam(icCode, icGroup, "DA", STRING(idaDate)).
   
   END METHOD.


   METHOD PUBLIC STATIC VOID incr
      ( icCode AS CHARACTER ):
         
      incr(icCode, ?).

   END METHOD.


   METHOD PUBLIC STATIC VOID incr
      ( icCode AS CHARACTER,
        icGroup AS CHARACTER ):
      
      DEFINE VARIABLE liCurrentValue AS INTEGER NO-UNDO.
      
      liCurrentValue = geti(icCode, icGroup).
      
      seti(icCode, icGroup, liCurrentValue + 1).

   END METHOD.


   METHOD STATIC PRIVATE CHARACTER mTypeName
      (icType AS CHARACTER):
                  
      CASE icType:
         WHEN "C"
         THEN RETURN "a string".
         WHEN "I"
         THEN RETURN "an integer".
         WHEN "DE"
         THEN RETURN "a decimal".
         WHEN "DA"
         THEN RETURN "a date".
         OTHERWISE RETURN "".
      END CASE.
            
   END METHOD.


   METHOD PRIVATE STATIC CHARACTER getParam
      ( icCode  AS CHARACTER,
        icGroup AS CHARACTER,
        icType  AS CHARACTER ):
            
      FOR FIRST lbTMSParam NO-LOCK WHERE
         lbTMSParam.Brand      = gcBrand          AND
         lbTMSParam.ParamCode  = icCode           AND
         ( IF icGroup = ?
           THEN TRUE
           ELSE lbTMSParam.ParamGroup = icGroup ):
         
         IF lbTMSParam.ParamType <> icType
         THEN RETURN ERROR NEW Progress.Lang.AppError(SUBSTITUTE("Configuration parameter &1 is not &2.", IF icGroup = ? THEN icCode ELSE icGroup + ":" + icCode, mTypeName(icType)), 1).
         
         CASE icType:
            WHEN "C"
            THEN RETURN lbTMSParam.CharVal.
            WHEN "I"
            THEN RETURN STRING(lbTMSParam.IntVal).
            WHEN "DE"
            THEN RETURN STRING(lbTMSParam.DecVal).
            WHEN "DA"
            THEN RETURN STRING(lbTMSParam.DateVal).
         END CASE.
      END.   

      RETURN ERROR NEW Progress.Lang.AppError(SUBST("Configuration parameter &1 not found.", IF icGroup = ? THEN icCode ELSE icGroup + ":" + icCode), 2). 
      
   END METHOD.


   METHOD PRIVATE STATIC VOID setParam
      (icCode  AS CHARACTER,
       icGroup AS CHARACTER,
       icType  AS CHARACTER,
       icValue AS CHARACTER ):

      DEFINE BUFFER localTMSParam FOR TMSParam.
      DEFINE VARIABLE llAvailable AS LOGICAL INITIAL TRUE  NO-UNDO.
      DEFINE VARIABLE llTypeError AS LOGICAL INITIAL FALSE NO-UNDO.
      DEFINE VARIABLE lii         AS INTEGER               NO-UNDO.      

      IF getParam(icCode, icGroup, icType) = icValue
      THEN RETURN.
      
      CATCH apperrorobj AS Progress.Lang.AppError:         
         DO lii = 1 TO apperrorobj:NumMessages:    
            IF apperrorobj:GetMessageNum(lii) = 1
            THEN DO:
               llTypeError = TRUE.
               RETURN ERROR NEW Progress.Lang.AppError(apperrorObj:GetMessage(lii), 1).
            END.
            IF apperrorobj:GetMessageNum(lii) = 2
            THEN llAvailable = FALSE. 
         END.
      END.

      FINALLY:
         IF NOT llTypeError
         THEN DO TRANSACTION:
            IF llAvailable
            THEN FIND localTMSParam EXCLUSIVE-LOCK WHERE
                     localTMSParam.Brand      = gcBrand AND
                     localTMSParam.ParamCode  = icCode  AND
                     localTMSParam.ParamGroup = icGroup.
            ELSE DO:
                  CREATE localTMSParam.
                  ASSIGN
                     localTMSParam.Brand      = gcBrand
                     localTMSParam.ParamCode  = icCode
                     localTMSParam.ParamType  = icType
                     localTMSParam.ParamGroup = icGroup
                     .
            END.
               
            CASE icType:
               WHEN "C"
               THEN localTMSParam.CharVal = icValue.
               WHEN "I"
               THEN localTMSParam.IntVal  = INTEGER(icValue).
               WHEN "DE"
               THEN localTMSParam.DecVal  = DECIMAL(icValue).
               WHEN "DA"
               THEN localTMSParam.DateVal = DATE(icValue).
            END CASE.
         END.
      END FINALLY.   

   END METHOD.

END CLASS.
