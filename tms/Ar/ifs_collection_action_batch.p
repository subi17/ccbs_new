/* ---------------------------------------------------------------------------
  MODULE .......: ifs_collection_action_batch.p
  FUNCTION .....: batch process for reading collection actions data from ifs
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 24.06.09
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Syst/eventlog.i}

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcIFSFile   AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcReadDir   AS CHAR NO-UNDO.
DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR liTotal     AS INT  NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD IFSFile AS CHAR
   INDEX IFSFile IFSFile.

DEF STREAM sRead.


FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

lcReadDir  = fCParamC("IFSCollActionFile").
   
IF lcReadDir = "" OR lcReadDir = ? THEN RETURN "ERROR:Definitions missing".
   

fELog("IFS_COLLECTION_ACTION","Started").

RUN pFindFiles (lcReadDir,
                OUTPUT lcIFSFile).
                      
IF lcIFSFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcIFSFile,"¤"):
 
   lcFile = ENTRY(liCnt,lcIFSFile,"¤").
   
   /* file not found */
   IF lcFile = "" OR SEARCH(lcFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.IFSFile = lcFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.IFSFile = lcFile.
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Ar/ifs_collection_action.p (ttFiles.IFSFile,
                              OUTPUT liRead,
                              OUTPUT liError).
                        
   liTotal = liTotal + liRead - liError.
END.

fELog("IFS_COLLECTION_ACTION","Stopped,Files:" + STRING(liFiles) +
                             ",Handled:" + STRING(liTotal)).


PROCEDURE pFindFiles:

   DEF INPUT  PARAMETER icFilter AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER ocFiles  AS CHAR NO-UNDO. 
       
   DEF VAR lcFileName AS CHAR NO-UNDO.
   
   INPUT STREAM sRead THROUGH VALUE("ls -1 " + icFilter).

   REPEAT:
      IMPORT STREAM sRead UNFORMATTED lcFileName.
      
      IF lcFileName = "" OR 
         lcFileName MATCHES ("*o such file or*") 
      THEN NEXT.
      
      ocFiles = ocFiles + 
                (IF ocFiles > "" THEN "¤" ELSE "") + 
                lcFileName.
   END.
                        
   INPUT STREAM sRead CLOSE.

END PROCEDURE.


