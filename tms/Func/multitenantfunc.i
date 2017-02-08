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
&ENDIF
