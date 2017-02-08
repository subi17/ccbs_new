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

FUNCTION fsetEffectiveTenantForAllDB RETURNS LOGICAL 
   (INPUT icTenant AS CHAR).
   DEF VAR liDBid AS INT NO-UNDO.
   DEF VAR lcDBname AS CHAR NO-UNDO.   
   DO liDBid = 1 TO NUM-DBS with frame a.
      lcDBname = LDBNAME(liDBid).
      set-effective-tenant(icTenant,lcDBname).
   END.
END FUNCTION.

FUNCTION fConvertBrandToTenant RETURNS CHARACTER
   (INPUT icBrand AS CHARACTER):

   DEF VAR lcTenant AS CHAR NO-UNDO.
   
   CASE icBrand:
      WHEN "Yoigo" THEN
         ASSIGN lcTenant = "Default".
      WHEN "Masmovil" THEN 
         ASSIGN lcTenant = "Tmasmovil".
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
