/* deletems2_1.i */

DEF VAR DefIMSI  AS C  NO-UNDO.
DEF VAR delcmd   AS C  NO-UNDO.


/* get default IMSI FOR FNSUB */
{tmsparam.i DefIMSI return}. DefIMSI = TMSParam.CharVal.


/* get default IMSI FOR FNSUB */
{tmsparam.i DefIMSI return}. DefIMSI = TMSParam.CharVal.

FUNCTION fSONG RETURNS LOG(mi-no AS CHARACTER).

   DEF VAR msprefix AS C NO-UNDO.

   msprefix = SUBSTR(mi-no,1,7).

   IF (msprefix >= "4670100" AND msprefix <= "4670109") OR
      (msprefix >= "4670180" AND msprefix <= "4670189")
   THEN RETURN TRUE.
   ELSE RETURN FALSE.   

END.


