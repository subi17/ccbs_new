/*------------------------------------------------------
  Module .......: v-tmscodes.p
  Parent .......: 
  FUNCTION .....: Validate for TmsCodes
  APPLICATION ..: NN
  AUTHOR .......: jr
  CREATED ......: 16-09-02
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF INPUT  PARAMETER icTableName AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icFieldName AS CHAR NO-UNDO.      
DEF INPUT  PARAMETER icCodeGroup AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icValue     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER olfind      AS LOG  NO-UNDO.

FIND FIRST TMSCodes WHERE
           TMSCodes.TableName = icTableName AND
           TMSCodes.FieldName = icFieldName AND
           TMSCodes.CodeGroup = icCodeGroup AND 
           TMSCodes.CodeValue = icValue 
NO-LOCK  NO-ERROR.

IF AVAILABLE TMSCodes 
THEN olfind = TRUE.
ELSE 
DO:
   olfind = FALSE.
   MESSAGE "Uknown " + icFieldName + " !".
   PAUSE no-message.
   HIDE MESSAGE no-pause.
END.   

