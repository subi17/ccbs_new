/* ---------------------------------------------------------------------------
  MODULE .......: dd_bankacc_change_batch.p
  FUNCTION .....: batch process for reading bank account changes (csb19)
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 30.09.09
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Func/lib/eventlog.i}

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcCSBFile   AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcReadDir   AS CHAR NO-UNDO.
DEF VAR lcFile      AS CHAR NO-UNDO.
DEF VAR liChanged   AS INT  NO-UNDO.
DEF VAR liTotal     AS INT  NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD CSBFile AS CHAR
   INDEX CSBFile CSBFile.

DEF STREAM sRead.

FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

lcReadDir = fCParamC("DDBankChgFile").
   
IF lcReadDir = "" OR lcReadDir = ? THEN RETURN "ERROR:Definitions missing".

fELog("DD_BANK_CHANGE","Started").

RUN pFindFiles (lcReadDir,
                OUTPUT lcCSBFile).
                      
IF lcCSBFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcCSBFile,"¤"):
 
   lcFile = ENTRY(liCnt,lcCSBFile,"¤").
   
   /* file not found */
   IF lcFile = "" OR SEARCH(lcFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.CSBFile = lcFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.CSBFile = lcFile.
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Inv/dd_bankacc_change (ttFiles.CSBFile,
                          OUTPUT liRead,
                          OUTPUT liChanged,
                          OUTPUT liError).
                        
   liTotal = liTotal + liChanged.
END.

fELog("DD_BANK_CHANGE","Stopped,Files:" + STRING(liFiles) + 
                       ",BankAcc_changes:" + STRING(liTotal)).


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


