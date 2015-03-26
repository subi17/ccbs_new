&IF "{&dataformat}" NE "YES"
&THEN

&GLOBAL-DEFINE dataformat YES

/* ----------------------------------------------------------------------
  MODULE .......: dataformat.i
  TASK .........: Data format functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 13.05.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

PROCEDURE fFormatUnit: 
   
   DEF INPUT PARAM iiUnitType AS INTEGER.
   DEF INPUT PARAM ideAmount AS DEC.
   DEF OUTPUT PARAM ocUnitName AS CHAR.
   DEF OUTPUT PARAM ocAmount AS CHAR.

   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "TMRule"   AND
              TMSCodes.FieldName    = "CounterAmount"      AND
              TMSCodes.CodeGroup    = "TMR"          AND
              TMSCodes.CodeValue    = STRING(iiUnitType)
   NO-LOCK NO-ERROR.
   
   IF AVAIL TMSCodes THEN ocUnitName = TMSCodes.CodeName.

   CASE iiUnitType:
      WHEN 1 THEN DO:
         ocAmount = STRING(INT(ideAmount * 60),"HH:MM:SS").
      END.
      WHEN 2 THEN DO:
         ocAmount = STRING(INT(ideAmount),"HH:MM:SS").
      END.
      WHEN 3 OR WHEN 4 THEN DO:
         ocAmount = TRIM(STRING(ideAmount,">>>>>>>>>>>>>9.999999")).
      END.
      WHEN 5 THEN DO:
         ocAmount = STRING(INT(ideAmount)).
      END.
      WHEN 6 THEN DO:
         ocAmount = STRING(ideAmount,"->>>>>>>>9.999").
      END.
      WHEN 7 THEN DO:
         ocAmount = STRING(INT(ideAmount)).
      END.
      OTHERWISE DO:
         ocAmount = STRING(ideAmount).
      END.
   END.

END PROCEDURE.

&ENDIF
