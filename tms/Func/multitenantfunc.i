/* ----------------------------------------------------------------------
  MODULE .......: multitenant.i
  TASK .........: Multitenant functions
  APPLICATION ..: TMS
  AUTHOR .......: kaaikas
  CREATED ......: 06.02.17
  CHANGED ......: 
  Version ......: xfera
----------------------------------------------------------------------- */

&IF "{&multitenant}" NE "YES"
&THEN

&GLOBAL-DEFINE multitenant YES
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

&ENDIF
