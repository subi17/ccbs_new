/* ----------------------------------------------------------------------
  MODULE .......: msisdn_prefix.i
  TASK .........: 
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 18.06.12
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */

&IF "{&MSISDN_PREFIX_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MSISDN_PREFIX_I YES

{tmsconst.i}

FUNCTION fIsYoigoCLI RETURNS LOG            
(icCLI AS CHAR):

   DEF VAR i AS INT NO-UNDO. 
   
   DO i = 1 TO NUM-ENTRIES({&MSISDN_YOIGO_PREFIXES}):
      IF icCLI BEGINS ENTRY(i, {&MSISDN_YOIGO_PREFIXES}) THEN
         RETURN TRUE.
   END.
   RETURN FALSE.
END.

FUNCTION fIsMobileNumber RETURNS LOG            
(icCLI AS CHAR):

   RETURN (icCLI BEGINS "6" OR icCLI BEGINS "7").
END.

&ENDIF
