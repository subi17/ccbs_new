/*------------------------------------------------------------
  MODULE .......: clisavef.p 
  FUNCTION .....: Save a copy in zip-format before DELETE.
  APPLICATION ..: 
  AUTHOR .......: PG Orbil
  CREATED ......: 19.10.1999 kl
  MODIFIED .....: 12.07.2000 BY kl: use rsoper.rs-dir

  Version ......: M15
--------------------------------------------------------------*/

{Func/function.i}

DEFINE INPUT PARAMETER  lFileName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER  pCustNr   AS INTEGER   NO-UNDO.

DEFINE VARIABLE lSaveFileName AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lFile         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lLen          AS INTEGER      NO-UNDO.
DEFINE VARIABLE lIndex        AS INTEGER      NO-UNDO.

ASSIGN
   lLen   = LENGTH(lFileName)
   lIndex = R-INDEX(lFileName,"/",lLen)
   lFile  = SUBSTRING(lFileName,lIndex + 1,lLen - lIndex). 

FIND FIRST rsoper WHERE
           rsoper.CustNum = pCustNr
NO-LOCK NO-ERROR.

IF AVAIL rsoper THEN
   lSaveFileName = fChkPath(rsoper.rs-dir) + "save/" + lFile + ".zip".
ELSE RETURN "Failed".

IF OPSYS = "UNIX" THEN DO:

   UNIX SILENT VALUE("ZipCode " + lSaveFileName + " " + lFileName + " > /dev/null").

   /* Ends the program WITH error MESSAGE */
   IF SEARCH(lSaveFileName) = ? THEN RETURN "Failed".
   ELSE UNIX SILENT VALUE("rm " + lFileName).    

END.
ELSE MESSAGE "Save a copy in zip-format before delete." VIEW-AS ALERT-BOX.

RETURN "".



