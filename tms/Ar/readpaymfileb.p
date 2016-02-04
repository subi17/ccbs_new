/* ---------------------------------------------------------------------------
  MODULE .......: READPAYMFILEB.P
  KUTSUVAMODULI : 
  FUNCTION .....: batch process for reading payments from files            
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 22.11.07
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN gcBrand = "1" 
       katun   = "Cron".
       
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/lib/eventlog.i}
{Func/timestamp.i}

DEF VAR liCnt       AS INT  NO-UNDO.
DEF VAR lcPaymFile  AS CHAR NO-UNDO.
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO.
DEF VAR liFiles     AS INT  NO-UNDO.
DEF VAR lcPlainFile AS CHAR NO-UNDO.
DEF VAR lcTransDir  AS CHAR NO-UNDO.
DEF VAR lcReadDir   AS CHAR NO-UNDO.
DEF VAR lcLogFile   AS CHAR NO-UNDO. 
DEF VAR lcLogDir    AS CHAR NO-UNDO.
DEF VAR lcLogTrans  AS CHAR NO-UNDO.

DEF TEMP-TABLE ttFiles NO-UNDO
   FIELD PaymFile AS CHAR
   INDEX PaymFile PaymFile.

DEF STREAM sRead.

FUNCTION fCollTemp RETURNS LOGIC
   (icPaymFile AS CHAR).

   /* file not found */
   IF icPaymFile = "" OR SEARCH(icPaymFile) = ? THEN NEXT.
   
   IF CAN-FIND(FIRST ttFiles WHERE ttFiles.PaymFile = icPaymFile) THEN NEXT.
   
   CREATE ttFiles.
   ASSIGN ttFiles.PaymFile = icPaymFile.
   
END FUNCTION.


FIND FIRST Company WHERE
           Company.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE Company THEN ynimi = Company.CompName.

ASSIGN 
   lcReadDir  = fCParamC("PaymFiles")
   lcTransDir = fCParamC("PaymFileArc")
   lcLogFile  = fCParamC("PaymFileLog")
   lcLogDir   = fCParamC("PaymFileLogDir")
   lcLogTrans = fCParamC("PaymFileLogTrans").
   
   
IF lcReadDir = "" OR lcReadDir = ? THEN RETURN "ERROR:Definitions missing".
   
ASSIGN
   lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                         STRING(MONTH(TODAY),"99") + 
                                         STRING(DAY(TODAY),"99"))
   lcLogFile = REPLACE(lcLogFile,"#TIME",
                                 REPLACE(STRING(TIME,"hh:mm:ss"),":",""))
   lcLogFile = lcLogDir + "/" + lcLogFile.                              
                                         
                                      
fELog("READPAYMFILE","Started").

RUN pFindFiles (lcReadDir,
                OUTPUT lcPaymFile).
                      
IF lcPaymFile > "" THEN 
DO liCnt = 1 TO NUM-ENTRIES(lcPaymFile,"¤"):
   fCollTemp(ENTRY(liCnt,lcPaymFile,"¤")).
END.

      
FOR EACH ttFiles:

   liFiles = liFiles + 1.
   
   RUN Ar/readpaymfile (ttFiles.PaymFile,
                     lcLogFile,
                     OUTPUT liRead,
                     OUTPUT liError).
                  
   /* no events found from file -> move file anyway to processed */
   IF liRead = 0 THEN DO:
      RUN pTransferFile(ttFiles.PaymFile,
                        lcTransDir).
      NEXT. 
   END.

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "Cron"  
         ActionLog.KeyValue     = "" 
         ActionLog.ActionID     = "PaymFile"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = liRead
         ActionLog.ActionChar   = "Read: " + STRING(liRead) + 
                                  " Errors: " + STRING(liError) + 
                                  " Succesful: " + STRING(liRead - liError)
         ActionLog.ActionStatus = 3.
         ActionLog.ActionTS     = fMakeTS().

      /* file without the dir */
      lcPlainFile = ttFiles.PaymFile.
      IF NUM-ENTRIES(lcPlainFile,"/") > 1 THEN
         lcPlainFile = ENTRY(NUM-ENTRIES(lcPlainFile,"/"),lcPlainFile,"/").
         
      ActionLog.KeyValue = lcPlainFile.
   
   END.

   /* delete or move the payment file to archive */
   IF liRead > 0 THEN DO:
      RUN pTransferFile(ttFiles.PaymFile,
                        lcTransDir). 
      /* also log file */
      RUN pTransferFile(lcLogFile,
                        lcLogTrans).
   END.
END.

fELog("READPAYMFILE","Stopped,Files:" + STRING(liFiles)).


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

PROCEDURE pTransferFile:

   DEF INPUT PARAMETER icFile     AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icTransDir AS CHAR NO-UNDO.
   
   IF icTransDir = "" THEN RETURN.
   
   fTransDir(icFile,
             "",
             icTransDir).
END.



