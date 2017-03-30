/* ----------------------------------------------------------------------
  MODULE .......: multitenant.i
  TASK .........: Multitenant functions
  APPLICATION ..: TMS
  AUTHOR .......: kaaikas
  CREATED ......: 06.02.17
  CHANGED ......: 
  Version ......: xfera
----------------------------------------------------------------------- */

&IF "{&multitenantfunc_i}" NE "YES"
&THEN

&GLOBAL-DEFINE multitenantfunc_i YES
{Syst/tmsconst.i}

/* Function to set effective tenant based by tenant name 
   for all system databases */
FUNCTION fsetEffectiveTenantForAllDB RETURNS LOGICAL 
   (INPUT icTenant AS CHAR).
   DEF VAR liDBid   AS INT  NO-UNDO.
   DEF VAR lcDBname AS CHAR NO-UNDO.   

   IF icTenant EQ "" THEN 
       RETURN FALSE.

   DO liDBid = 1 TO NUM-DBS:
      lcDBname = LDBNAME(liDBid).
      set-effective-tenant(icTenant,lcDBname) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         RETURN FALSE.
   END.

   RETURN TRUE.
   
END FUNCTION.

/* Function to set effective tenant based by tenant id
   for all system databases */
FUNCTION fsetEffectiveTenantIdForAllDB RETURNS LOGICAL
   (INPUT iiTenant AS INT).
   DEF VAR liDBid AS INT NO-UNDO.
   DEF VAR lcDBname AS CHAR NO-UNDO.
   DO liDBid = 1 TO NUM-DBS:
      lcDBname = LDBNAME(liDBid).
      set-effective-tenant(iiTenant,lcDBname) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
         RETURN FALSE.      
      END.
   END.
   RETURN TRUE.
END FUNCTION.

FUNCTION fgetTenantNames RETURNS CHAR (). 
   DEF VAR lcNames AS CHAR NO-UNDO.
   FOR EACH _tenant:
      IF lcNames EQ "" THEN
         lcNames = _tenant._tenant-name.
      ELSE  
         lcNames = lcNames + "," + _tenant._tenant-name.
   END.
   RETURN lcNames.
END FUNCTION.



/* Function to find out maximum tenantid in system */
FUNCTION fgetMaxTenantId RETURNS INT ().
   DEF VAR liid AS INT NO-UNDO.
   FOR EACH _tenant:
      IF _tenant._tenantId > liid THEN
         liid = _tenant._tenantId.
   END.
   RETURN liid.
END FUNCTION.

/* Function to return available tenant ids. If caller not super tenant or 
   effective tenant id is set then return only current tenant id. */
FUNCTION fgetTenantIds RETURNS CHAR ().
   DEF VAR lcIds AS CHAR NO-UNDO.
   IF TENANT-ID(LDBNAME(1)) NE -1 THEN  /* Not super, only one id to return */
      RETURN STRING(TENANT-ID(LDBNAME(1))).
   FOR EACH _tenant:
      IF lcIds EQ "" THEN
         lcids = STRING(_tenant._tenantId).
      ELSE
         lcids = lcids + "," + STRING(_tenant._tenantId).
   END.
   RETURN lcids.
END FUNCTION.

/* Function to find tenant Id by name */
FUNCTION fgetTenantIdbyName RETURNS INT 
   (INPUT icname AS CHAR).
   FIND FIRST _tenant WHERE _tenant._tenant-name EQ icname.
   IF AVAIL _tenant THEN RETURN _tenant._tenantId.
END FUNCTION.

/* Function to find tenant Id by name */
FUNCTION fgetTenantNamebyId RETURNS CHAR
   (INPUT iiid AS INT).
   FIND FIRST _tenant WHERE _tenant._tenantId EQ iiid.
   IF AVAIL _tenant THEN RETURN _tenant._tenant-name.
END FUNCTION.

FUNCTION fgetBrandNamebyTenantId RETURNS CHAR
   (INPUT iiid AS INT).
   DEF VAR lcBrand AS CHAR NO-UNDO.
   lcBrand = fgetTenantNamebyId(iiid).
   IF lcBrand EQ "Default" THEN 
      lcBrand = "yoigo".
   ELSE
      lcBrand = SUBSTRING(lcBrand,2).
   RETURN lcBrand.
END FUNCTION.

FUNCTION fgetCompanyId RETURNS CHAR
   ().
   CASE TENANT-ID(LDBNAME(1)):
      WHEN 0 THEN RETURN "X001".
      WHEN 1 THEN RETURN "M001".
      OTHERWISE RETURN "".
   END CASE.
END FUNCTION.

FUNCTION fConvertBrandToTenant RETURNS CHARACTER
   (INPUT icBrand AS CHARACTER):

   DEF VAR lcTenant AS CHAR NO-UNDO.
   
   CASE icBrand:
      WHEN "Yoigo" THEN
         ASSIGN lcTenant = {&TENANT_YOIGO}.
      WHEN "Masmovil" THEN 
         ASSIGN lcTenant = {&TENANT_MASMOVIL}.
      OTHERWISE
         ASSIGN lcTenant = "".
   END CASE.        
   
   RETURN lcTenant.
       
END FUNCTION.    

FUNCTION fConvertTenantToBrand RETURNS CHARACTER
   (INPUT icTenant AS CHARACTER):

   DEF VAR lcBrand AS CHAR NO-UNDO.
   
   CASE icTenant:
      WHEN "Default" THEN
         ASSIGN lcBrand = "yoigo".
      WHEN "Tmasmovil" THEN 
         ASSIGN lcBrand = "masmovil".
      OTHERWISE
         ASSIGN lcBrand = "".
   END CASE.        
   
   RETURN lcBrand.
       
END FUNCTION.

FUNCTION fGetCurrentBrand RETURNS CHAR ():
   DEF BUFFER Company FOR Company.
   FIND FIRST Company NO-LOCK WHERE
              Company.Brand = "1" NO-ERROR.
   IF AVAIL Company THEN 
      RETURN fConvertTenantToBrand(BUFFER-TENANT-NAME(Company)).
   ELSE RETURN fConvertTenantToBrand(TENANT-NAME(LDBNAME("common"))).
END FUNCTION.

/*
   Function returns the brand name of a given table name.
   A caller must take care that the table name exists.
   If the table is shared then the function returns ""
*/
FUNCTION fGetTableBrand RETURNS CHARACTER
   ( icTableName AS CHARACTER ):

   DEFINE VARIABLE lhBuffer      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.

   CREATE BUFFER lhBuffer FOR TABLE icTableName.

   lcReturnValue = TENANT-NAME(lhBuffer:DBNAME).

   RETURN fConvertTenantToBrand(lcReturnValue).

   FINALLY:
      IF VALID-HANDLE(lhBuffer)
      THEN DELETE OBJECT lhBuffer.
   END FINALLY.

END FUNCTION.

&ENDIF
