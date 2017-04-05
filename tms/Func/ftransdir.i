&IF "{&FTRANSDIR_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE FTRANSDIR_I YES
/* ftransdir       08.03.2002/aam 
   move a File TO a Transfer directory 

   changes:        30.09.05/aam "_" before additional number
                   07.11.05/aam suppress error in mv-command 
                   20.12.06/aam fUniqueFileName separated from fTransDir
*/

FUNCTION fCheckFileNameChars RETURNS LOGICAL
   (icFileName AS CHAR):
   IF INDEX(icFileName,'"') > 0 THEN RETURN FALSE.
   IF INDEX(icFileName,'&') > 0 THEN RETURN FALSE.
   RETURN TRUE.
END.

FUNCTION fGetFileName RETURNS CHARACTER
   (icFile AS CHAR):

    DEF VAR liDPos AS INT NO-UNDO.
    DEF VAR lcFileName AS CHAR NO-UNDO.

    IF icFile = "" THEN RETURN "".
    FILE-INFO:FILE-NAME = icFile.
    IF FILE-INFO:FILE-TYPE = ? OR
       NOT FILE-INFO:FILE-TYPE BEGINS "F" THEN RETURN "".

    /* File without the directory */
    IF INDEX(icFile,"/") GT 0 THEN ASSIGN 
       liDPos   = R-INDEX(icFile,"/")
       lcFileName = SUBSTRING(icFile,liDPos + 1).
    ELSE lcFileName = icFile.

    RETURN lcFileName. 
END FUNCTION.

FUNCTION fUniqueFileName RETURNS CHARACTER
   (icFile AS CHAR,   
    icExt  AS CHAR):   /* extension that should be retained */
    
    DEF VAR lcTrAdd   AS CHAR NO-UNDO. 
    DEF VAR liDPos    AS INT  NO-UNDO. 
    DEF VAR lcUniFile AS CHAR NO-UNDO.

    IF icFile = "" THEN RETURN "".

    /* is ok as it is */
    FILE-INFO:FILE-NAME = icFile.
    IF FILE-INFO:FILE-TYPE = ? THEN RETURN icFile.

    /* file without the extension */
    IF icExt NE "" THEN icFile = REPLACE(icFile,icExt,"").

    ASSIGN liDPos  = 0
           lcTrAdd = icExt.

    /* check that the File doesn't exist */        
    REPEAT:
        FILE-INFO:FILE-NAME = icFile + lcTrAdd.
        IF FILE-INFO:FILE-TYPE = ? THEN LEAVE.         

        ASSIGN liDPos  = liDPos + 1
               lcTrAdd = "_" + STRING(liDPos) + icExt.
    END.

    RETURN icFile + lcTrAdd. 
    
END FUNCTION.    

FUNCTION fMove2TransDir RETURNS CHARACTER
    (icFile  AS CHAR,    /* File TO be transferred */
     icExt   AS CHAR,    /* extension that should be retained */
     icToDir AS CHAR):   /* Target directory */

    DEF VAR lcTarget AS CHAR NO-UNDO.

    IF icFile = "" OR icToDir = "" THEN RETURN "".

    lcTarget = fGetFileName(icFile).
    IF lcTarget = "" THEN RETURN "".
    
    lcTarget = icToDir + "/" + lcTarget.

    /* check that the file doesn't exist on target */        
    lcTarget = fUniqueFileName(lcTarget,
                               icExt).

    /* move the file */
    UNIX SILENT VALUE("mv " + QUOTER(icFile) + " " + QUOTER(lcTarget) + " >/dev/null 2>&1").

    FILE-INFO:FILE-NAME = lcTarget.
    IF FILE-INFO:FILE-TYPE BEGINS "F" THEN RETURN lcTarget.
    ELSE RETURN "". 

END FUNCTION.


FUNCTION fCopy2TargetDir RETURNS CHARACTER
    (icFile  AS CHAR,    /* File TO be copied */
     icExt   AS CHAR,    /* extension that should be retained */
     icToDir AS CHAR):   /* Target directory */

    DEF VAR lcTarget AS CHAR NO-UNDO.

    IF icFile = "" OR icToDir = "" THEN RETURN "".

    lcTarget = fGetFileName(icFile).
    IF lcTarget = "" THEN RETURN "".
    
    lcTarget = icToDir + "/" + lcTarget.

    /* check that the file doesn't exist on target */        
    lcTarget = fUniqueFileName(lcTarget,
                               icExt).

    /* copy the file */
    UNIX SILENT VALUE("cp " + QUOTER(icFile) + " " + QUOTER(lcTarget) + " >/dev/null 2>&1").

    FILE-INFO:FILE-NAME = lcTarget.
    IF FILE-INFO:FILE-TYPE BEGINS "F" THEN RETURN lcTarget.
    ELSE RETURN "". 

END FUNCTION.


FUNCTION fTransDir RETURNS LOGICAL
    (icFile  AS CHAR,    /* File TO be transferred */
     icExt   AS CHAR,    /* extension that should be retained */
     icToDir AS CHAR).   /* Target directory */

    RETURN (fMove2TransDir(icFile,icExt,icToDir) > "").
                          
END FUNCTION.

FUNCTION fMove2TransDirOverwrite RETURNS LOGICAL
    (icFile  AS CHAR,    /* File TO be transferred */
     icToDir AS CHAR):   /* Target directory */

    DEF VAR lcTarget AS CHAR NO-UNDO.

    IF icFile = "" OR icToDir = "" THEN RETURN FALSE.

    lcTarget = fGetFileName(icFile).
    IF lcTarget = "" THEN RETURN FALSE.
 
    lcTarget = icToDir + "/" + lcTarget.

    /* move the file */
    UNIX SILENT VALUE("mv " + QUOTER(icFile) + " " + QUOTER(lcTarget) + " >/dev/null 2>&1").

    FILE-INFO:FILE-NAME = lcTarget.
    RETURN (FILE-INFO:FILE-TYPE BEGINS "F").

END FUNCTION.

FUNCTION fGetDir RETURNS CHAR (INPUT pfilename AS CHARACTER):
   DEFINE VARIABLE iPathEntries AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iEntry AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cDir AS CHARACTER NO-UNDO. 

   iPathEntries = NUM-ENTRIES(pfilename, "/").
   REPEAT iEntry = 1 TO iPathEntries - 1:
      cEntry = ENTRY(iEntry, pfilename, "/").
      IF iEntry = 1 THEN
         cDir = cEntry.
      ELSE
         cDir = cDir + "/" + cEntry.
   END.
   cDir = cDir + "/".

   RETURN cDir.
END.

FUNCTION fIsOutputFile RETURNS LOG
   (INPUT pFileName AS CHARACTER):

   /* Check if unexisting TMSParam */
   IF pFileName = ? OR pFileName = "" THEN 
   DO: 
      RETURN FALSE.
   END. /* IF pFileName = */

   /* Get the directory */
   DEFINE VARIABLE cDir AS CHARACTER NO-UNDO. 
   cDir = fGetDir(pFileName).
  
   /* Set FILE-INFO for directory checking */
   FILE-INFO:FILENAME = cDir.
   
   /* Check if the directory exists */
   IF FILE-INFO:FILE-TYPE = ? THEN
   DO:
      RETURN FALSE.
   END.

   /* Check if the directory is writeable */
   IF INDEX(FILE-INFO:FILE-TYPE, "W") = 0 THEN
   DO:
      RETURN FALSE.
   END.
   
   /* Check if the file exists: if does not, it can be written */
   IF SEARCH(pFileName) <> ? THEN
   DO:
      /* Check if the target file is unwriteable: file must have write rights */
      FILE-INFO:FILE-NAME = pFileName.
      IF INDEX(FILE-INFO:FILE-TYPE,"W") = 0 THEN
      DO:
         RETURN FALSE.
      END.
   END.
   RETURN TRUE.
END.

&ENDIF
